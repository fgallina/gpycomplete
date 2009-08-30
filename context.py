import sys
import types
import re

BUILTIN_KEYS = dir(globals()['__builtins__'])

subprogram_globals_buffer = {}
subprogram_globals = {}
helper_globals = {}
helper_globals_buffer = {}
subcontext_globals = {}

def refresh_context(code):
    """Executes the code passed as param in the global scope and if it
    executes sucessfully returns True, otherwise returns False
    """
    if exec_code(code):
        return True
    else:
        return False

def calculate_subcontext(subcontext, cursor_indentation):
    """ Calculates the subcontext taking into account the cursor
    position
    """
    # 0: indentation
    # 1: type (def or class)
    # 2: name
    # 3: point
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
        obj = eval_code(code)
        context = {}
        if type(obj) == types.MethodType:
            variables = list(obj.im_func.func_code.co_varnames)
            #for item in dir(obj):
            for item in variables:
                # context.append(item)
                context[item] = types.ObjectType
        elif type(obj) == types.FunctionType:
            variables = list(obj.func_code.co_varnames)
            for item in variables:
                # context.append(item)
                context[item] = types.ObjectType
        # subcontext_globals = context
        subcontext_globals = context
    else:
        subcontext_globals = {}
    return code


def get_context():
    """returns a list with all the keywords which are in the global scope"""
    global BUILTIN_KEYS
    global helper_globals
    global subcontext_globals
    keys = []
    keys = dir(globals()['__builtins__']) + \
           subprogram_globals.keys() + \
           helper_globals.keys() + \
           BUILTIN_KEYS + \
           subcontext_globals.keys()
    keys.sort()
    return keys


def exec_code(code, context='subprogram_globals_buffer'):
    """Executes code in a safe way in the subprogram_globals /
    helper_globals dict
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


def eval_code(code, context='subprogram_globals'):
    """Evals code in the given context"""
    global subprogram_globals
    global helper_globals
    obj = None
    try:
        if context == 'subprogram_globals':
            obj = eval(code, subprogram_globals)
        elif context == 'helper_globals':
            obj = eval(code, helper_globals)
    except:
        obj = None
    return obj


def cimport(obj, context='helper_globals'):
    """Safe import of modules in the appropiate context"""
    dot = obj.rfind('.')
    if dot != -1:
       pobj = obj[:dot]
    else:
       pobj = obj
       dot = 1
    imported = False
    while not imported and dot != -1:
       code = "import " + pobj
       imported = exec_code(code, context)
       dot = pobj.rfind('.')
       pobj = pobj[:dot]
    return imported


def add_path(path):
    """Adds a path to the pythonpath and returns the path"""
    sys.path.append(path)
    return path
