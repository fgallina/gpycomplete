import glob
import os
import re
import context

def get_completions(word, code, subcontext, cursor_indentation):
    """gets the completions for word after evaluating code"""
    global subprogram_globals
    parsed_subcontext = context.calculate_subcontext(subcontext, cursor_indentation)
    context.exec_code(code)
    keys = context.get_context()
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
        if word.startswith('self'):
            dot_index = parsed_subcontext.rfind('.')
            parsed_subcontext = parsed_subcontext[:dot_index]
            word = word.replace('self', parsed_subcontext)
        # If word ends with a "." strip it and execute get_dir
        if word.endswith('.'):
            module = context.eval_code(word[:-1])
            if module:
                return get_dir(module)
            else:
                return []
        else:
            # If word does not ends with "." but it contains a dot
            # then eval word up to ".", split the remaining part, get
            # the attributes of the module when the attribute starts
            # with the remaining
            dot_index = word.rfind('.')
            module = context.eval_code(word[:dot_index])
            if module:
                mword = word[dot_index+1:]
                return [i for i in get_dir(module) if i.startswith(mword)]
            else:
                return []


def get_dir(obj):
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
