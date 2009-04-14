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
helper_globals = {}
helper_globals_buffer = {}
# FIXME - CONVERT SUBCONTEXT_GLOBALS IN A DICT
subcontext_globals = []


def get_completions(obj, code, subcontext="", cursor_indentation=""):
    """Returns the completions parsed as a string"""
    _calculate_subcontext(subcontext, cursor_indentation)
    return _get_completions(obj, code)


def get_help(obj):
    """Returns the help of the given object.
    Inspired in the original pycomplete package
    """
    paren = obj.rfind("(")
    if paren != -1:
        obj = obj[:paren]
    if obj.endswith("(") or obj.endswith("."):
        obj = obj[:-1]
    out = "no help string for " + obj
    found = False
    context = 'subprogram_globals'
    if not obj in _get_context():
        context = 'helper_globals'
        found = _import(obj, context)
    else:
        found = True
    if obj not in subcontext_globals and \
       found == True:
        obj = _eval_code(obj, context)
    else:
        return out
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
        return True
    else:
        return False


def get_signature(obj):
    """Returns the signature of the given object.
    Inspired in the original pycomplete package
    """
    # FIXME - make this function less ugly
    paren = obj.find("(")
    if paren != -1:
        obj = obj[:paren]
    context = 'subprogram_globals'
    if not obj in _get_context():
        context = 'helper_globals'
        if not _import(obj, context):
            return "no signature for" + obj
    try:
        obj = _eval_code(obj, context)
    except:
        return "no signature for" + obj
    sig = ""
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


def get_path(position, code):
    """Returns the path were the pointer is currently

    Arguments:
    - `position`: the position of the pointer
    - `code`: the code to check
    """
    return str(position)


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


def _calculate_subcontext(subcontext, cursor_indentation):
    # 0: indentation
    # 1: type (def or class)
    # 2: name
    # 3: point
    # FIXME - get complete subcontext for classes and methods
    global subcontext_globals
    code = ""
    if subcontext and subcontext[len(subcontext)-1][0] < cursor_indentation:
        for i in range(len(subcontext)):
            try:
                if subcontext[i][0] != subcontext[i+1][0]:
                    code += subcontext[i][2] + "."
            except IndexError:
                code += subcontext[i][2] + "."
        code = code[:-1]
        obj = _eval_code(code)
        context = []
        if type(obj) == types.MethodType:
            context = list(obj.im_func.func_code.co_varnames)
            for item in dir(obj):
                context.append(item)
        elif type(obj) == types.FunctionType:
            context = list(obj.func_code.co_varnames)
        subcontext_globals = context
    else:
        subcontext_globals = []
    return code


def _get_context():
    """returns a list with all the keywords which are in the global scope"""
    global BUILTIN_KEYS
    global helper_globals
    global subcontext_globals
    keys = []
    keys = dir(globals()['__builtins__']) + \
           subprogram_globals.keys() + \
           helper_globals.keys() + \
           BUILTIN_KEYS + \
           subcontext_globals
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
        # If the word is a simple statement not containing "." return
        # the global keys starting with the word
        return [i for i in keys if i.startswith(word)]
    else:
        # If word ends with a "." strip it and execute _get_dir
        if word.endswith('.'):
            module = _eval_code(word[:-1])
            if module:
                return _get_dir(module)
            else:
                return []
        else:
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


def _exec_code(code, context='subprogram_globals_buffer'):
    """Executes code in a sure way in the subprogram_globals dict
    """
    global subprogram_globals_buffer
    global subprogram_globals
    global helper_globals_buffer
    global helper_globals
    pattern = "(if\s+__name__\s*==\s*(\"|')__main__(\"|')\s*:"
    pattern += "\s*\n+([ \t\r\f\v]+.+\n)+)"
    code = re.sub(pattern, "", code)
    subprogram_globals_buffer = {}
    helper_globals_buffer = {}
    success = False
    try:
        if context.startswith('subprogram_globals'):
            exec code in subprogram_globals_buffer
        elif context.startswith('helper_globals'):
            exec code in helper_globals_buffer
        success = True
    except:
        success = False
    if success:
        if context == 'subprogram_globals_buffer':
            subprogram_globals = subprogram_globals_buffer
        else:
            helper_globals = helper_globals_buffer
    return success


def _eval_code(code, context='subprogram_globals'):
    """Evals code in the given context"""
    global subprogram_globals
    global subprogram_globals_buffer
    global helper_globals
    obj = None
    try:
        if context == 'subprogram_globals':
            obj = eval(code, subprogram_globals)
        elif context == 'subprogram_globals_buffer':
            obj = eval(code, subprogram_globals_buffer)
        elif context == 'helper_globals':
            obj = eval(code, helper_globals)
        elif context == 'helper_globals_buffer':
            obj = eval(code, helper_globals_buffer)
    except:
        obj = None
    return obj


def _import(obj, context='helper_globals'):
    dot = obj.rfind('.')
    if dot != -1:
       pobj = obj[:dot]
    else:
       pobj = obj
       dot = 1
    imported = False
    while not imported and dot != -1:
       code = "import " + pobj
       imported = _exec_code(code, context)
       dot = pobj.rfind('.')
       pobj = pobj[:dot]
    return imported


if __name__ == "__main__":
    code = "import django\nimport django.contrib\nimport sys\na = django\ndef test(a):\n\tbar = a\n\tfoo = 2"
    print get_completions("djan", code)
    print get_completions("tes", code)
    print get_completions("sys", code)
    print get_completions("a.", code)
    print get_completions("di", code, [["","def","test",2313]], "\t")
    print get_signature("sys.path")
    print get_signature("dir")
    print get_signature("glob.glob(")
    print get_signature("_get_completions(")
    print get_help("django.contrib")
    print get_help("foo")
    print get_help("sys.path")
    print get_completions("fo", code, [["","def","test",2313]], "\t")
    print get_completions("fo", code, [["","def","test",234]],"\t")

