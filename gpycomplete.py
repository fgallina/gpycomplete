# This file is part of gpycomplete.

# gpycomplete is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# gpycomplete is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with gpycomplete.  If not, see <http://www.gnu.org/licenses/>.

# gpycomplete is written from scratch by Fabian Ezequiel Gallina
# <fgallina at caffeinegroup dot com dot ar> but it is somehow based on
# the original pycomplete package from the http://python-mode.sf.net.

# gpycomplete allows inline completion and help for the python
# programing language within GNU/Emacs

import sys
import types
import inspect
import StringIO
import os
import re
import glob

BUILTIN_KEYS = ['ArithmeticError', 'AssertionError', 'AttributeError',
                'BaseException', 'DeprecationWarning', 'EOFError',
                'Ellipsis', 'EnvironmentError', 'Exception', 'False',
                'FloatingPointError', 'FutureWarning', 'GeneratorExit',
                'IOError', 'ImportError', 'ImportWarning',
                'IndentationError', 'IndexError', 'KeyError',
                'KeyboardInterrupt', 'LookupError', 'MemoryError',
                'NameError', 'None', 'NotImplemented',
                'NotImplementedError', 'OSError', 'OverflowError',
                'PendingDeprecationWarning', 'ReferenceError',
                'RuntimeError', 'RuntimeWarning', 'StandardError',
                'StopIteration', 'SyntaxError', 'SyntaxWarning',
                'SystemError', 'SystemExit', 'TabError', 'True',
                'TypeError', 'UnboundLocalError', 'UnicodeDecodeError',
                'UnicodeEncodeError', 'UnicodeError',
                'UnicodeTranslateError', 'UnicodeWarning', 'UserWarning',
                'ValueError', 'Warning', 'ZeroDivisionError', '__debug__',
                '__doc__', '__import__', '__name__', 'abs', 'all', 'any',
                'apply', 'basestring', 'bool', 'buffer', 'callable',
                'chr', 'classmethod', 'cmp', 'coerce', 'compile',
                'complex', 'copyright', 'credits', 'delattr', 'dict',
                'dir', 'divmod', 'enumerate', 'eval', 'execfile', 'exit',
                'file', 'filter', 'float', 'frozenset', 'getattr',
                'globals', 'hasattr', 'hash', 'help', 'hex', 'id',
                'input', 'int', 'intern', 'isinstance', 'issubclass',
                'iter', 'len', 'license', 'list', 'locals', 'long', 'map',
                'max', 'min', 'object', 'oct', 'open', 'ord', 'pow',
                'property', 'quit', 'range', 'raw_input', 'reduce',
                'reload', 'repr', 'reversed', 'round', 'set', 'setattr',
                'slice', 'sorted', 'staticmethod', 'str', 'sum', 'super',
                'tuple', 'type', 'unichr', 'unicode', 'vars', 'xrange',
                'zip']

PROTECTED_ITEMS = ["__builtins__", "__name__", "__doc__", "__file__",
                   # imports
                   "sys", "types", "inspect", "StringIO", "os","re",
                   "glob", "lisp",
                   # global variables and constants
                   "BUILTIN_KEYS", "PROTECTED_ITEMS",
                   "subprogram_globals_buffer", "subprogram_globals",
                   # functions
                   "_delete_globals", "__find_constructor",
                   "_get_completions", "_get_context", "_get_dir",
                   "_set_globals", "add_path", "complete", "get_help",
                   "get_signature","refresh_context",
                   "set_django_project",
                   # objects in the if __name__ == "__main__": part
                   "code"]

subprogram_globals_buffer = {}
subprogram_globals = {}

def complete(obj, code):
    """Returns the completions parsed as a string"""
    completions = _get_completions(obj, code)
    msg = ""
    maxwidth = 80
    currwidth = 0
    for completion in completions:
        if currwidth >= 80:
            msg += '\n'
            currwidth = 0
        msg += completion + ", "
        currwidth += len(completion) + 2
    if completions:
        return msg[:msg.rfind(',')]
    else:
        return "No completions"


def get_help(obj):
    """Returns the help of the given object.
    Inspired in the original pycomplete package
    """
    paren = obj.rfind("(")
    if paren != -1:
        obj = obj[:paren]
    if obj.endswith("(") or obj.endswith("."):
        obj = obj[:-1]
    if not obj in _get_context():
        code = "import " + obj
        try:
            exec code in globals()
        except:
            pass
    obj = eval(obj)
    stdout = sys.stdout
    out = StringIO.StringIO()
    try:
        sys.stdout = out
        help(obj)
    finally:
        sys.stdout = stdout
    return out.getvalue()


def add_path(path):
    """Adds a path to the pythonpath and returns the path"""
    sys.path.append(path)
    return path


def set_django_project(path, settings='settings'):
    """Adds the django proyect path to the pythonpath and sets the
    DJANGO_SETTINGS_MODULE environment variable to the value specified
    in the settings param
    """
    sys.path.append(path)
    os.environ['DJANGO_SETTINGS_MODULE'] = settings
    return "Path: " + path + " // Settings: " + settings


def refresh_context(code):
    """Executes the code passed as param in the global scope and if it
    executes sucessfully returns a sucess message else a failure
    message
    """
    if _exec_code(code):
        return "This file contains no errors"
    else:
        return "This file contains errors"

def get_signature(obj):
    """Returns the signature of the given object.
    Inspired in the original pycomplete package
    """
    if not re.match("^[A-Za-z_][A-Za-z_0-9]*([.][A-Za-z_][A-Za-z_0-9]*)*.*$", obj):
        return ""
    paren = obj.find("(")
    if paren != -1:
        obj = obj[:paren]
    if not obj in _get_context():
        dot = obj.rfind('.')
        if dot != -1:
            pobj = obj[:dot]
        else:
            pobj = obj
        try:
            code = "import " + pobj
            exec code in globals()
        except:
            pass
    sig = ""
    try:
        obj = eval(obj)
    except:
        return ""
    # This part is extracted from the pycomplete.py file
    if type(obj) in (types.ClassType, types.TypeType):
        obj = _find_constructor(obj)
    elif type(obj) == types.MethodType:
        obj = obj.im_func
    if type(obj) in [types.FunctionType, types.LambdaType]:
        (args, varargs, varkw, defaults) = inspect.getargspec(obj)
        sig = ('%s: %s' % (obj.__name__,
                           inspect.formatargspec(args, varargs, varkw,
                                                 defaults)))
    doc = getattr(obj, '__doc__', '')
    if doc and not sig:
        doc = doc.lstrip()
        pos = doc.find('\n')
        if pos < 0 or pos > 70:
            pos = 70
        sig = doc[:pos]
    return sig


def _find_constructor(class_ob):
    # This part is extracted from the pycomplete.py file
    # Given a class object, return a function object used for the
    # constructor (ie, __init__() ) or None if we can't find one.
    try:
        return class_ob.__init__.im_func
    except AttributeError:
        for base in class_ob.__bases__:
            rc = _find_constructor(base)
            if rc is not None: return rc
    return None


def _get_context():
    """returns a list with all the keywords which are in the global scope"""
    keys = []
    keys = dir(globals()['__builtins__']) + subprogram_globals.keys()
    keys.sort()
    return keys


def _get_completions(word, code):
    """gets the completions for word after evaluating code"""
    global subprogram_globals
    _exec_code(code)
    keys = _get_context()
    dots = word.split('.')
    # If it is a not completable python expression return an empty list
    pattern = "^[A-Za-z_][A-Za-z_0-9]*([.][A-Za-z_][A-Za-z_0-9]*)*\.?$"
    if not re.match(pattern, word):
        return []
    elif word.rfind('.') == -1:
        print word + " - sin puntos"
        # If the word is a simple statement not containing "." return
        # the global keys starting with the word
        return [i for i in keys if i.startswith(word)]
    else:
        # If word ends with a "." strip it and execute _get_dir
        if word.endswith('.'):
            print word + " - termina con punto"
            module = _eval_code(word[:-1])
            if module:
                return _get_dir(module)
            else:
                return []
        else:
            print word + " - tiene un punto en algun lado"
            # If word does not ends with "." but it contains a dot
            # then eval word up to ".", split the remaining part, get
            # the attributes of the module when the attribute starts
            # with the remaining
            dot_index = word.rfind('.')
            module = _eval_code(word[:dot_index])
            if module:
                mword = word[dot_index+1:]
                return [i for i in _get_dir(module) if i.startswith(mword)]
            else:
                return []


def _get_dir(obj):
    """Returns the attributes for a given object"""
    try:
        path = obj.__path__
    except:
        path = None
    attributes = dir(obj)
    if path:
        for path in obj.__path__:
            for subdir in os.listdir(path):
                fullname = os.path.join(path, subdir)
                if re.match(".*\.py$", subdir) and subdir != "__init__.py":
                    attributes.append(subdir[:-3])
                if os.path.isdir(fullname) and \
                   glob.glob(os.path.join(fullname, '__init__.py*')):
                    attributes.append(subdir)
    attributes.sort()
    return attributes

def _exec_code(code):
    """Executes code in a sure way in the subprogram_globals dict
    """
    global subprogram_globals_buffer
    global subprogram_globals
    pattern = "(if\s+__name__\s*==\s*(\"|')__main__(\"|')\s*:"
    pattern += "\s*\n+([ \t\r\f\v]+.+\n)+)"
    code = re.sub(pattern, "", code)
    subprogram_globals_buffer = {}
    success = False
    try:
        exec code in subprogram_globals_buffer
        success = True
    except:
        success = False
    if success:
        subprogram_globals = subprogram_globals_buffer
    return success

def _eval_code(code):
    """Evals code in the subprogram_globals dictionary"""
    global subprogram_globals
    module = None
    try:
        module = eval(code, subprogram_globals)
    except:
        module = None
    return module


if __name__ == "__main__":
    code = "import django\nimport django.contrib\na = django.contrib\nimport sys"
    complete("di", code)
    subprogram_globals_buffer
    complete("di", code)
    complete("djan", code)
    print complete("django.con", code)
    complete("sys", code)
    print complete("a.r", code)
#     print get_signature("sys.path")
#     print get_signature("dir")
#     print get_signature("glob.glob")
#     print get_signature("_get_completions(\"asdasd\",\"sdsada\"")
#     print get_help("dir")
#     print get_help("django.contrib")
#     print _get_context()
#     print get_help("sys.path.append")
