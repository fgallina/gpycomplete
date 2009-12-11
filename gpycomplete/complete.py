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
# <fgallina at gnu dot org dot ar> but it is somehow based on the
# original pycomplete package from the http://python-mode.sf.net.

# gpycomplete allows inline completion and help for the python
# programing language within GNU/Emacs
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
    # If it is a not completable python expression completions = an empty list
    pattern = "^[A-Za-z_][A-Za-z_0-9]*([.][A-Za-z_][A-Za-z_0-9]*)*\.?$"
    if not re.match(pattern, word):
        completions = []
    elif word.rfind('.') == -1:
        # If the word is a simple statement not containing "." completions =
        # the global keys starting with the word
        completions = [i for i in keys if i.startswith(word)]
    else:
        if word.startswith('self'):
            dot_index = parsed_subcontext.rfind('.')
            parsed_subcontext = parsed_subcontext[:dot_index]
            word = word.replace('self', parsed_subcontext)
        # If word ends with a "." strip it and execute get_dir
        if word.endswith('.'):
            module = context.eval_code(word[:-1])
            if module:
                completions = get_dir(module)
            else:
                completions = []
        else:
            # If word does not ends with "." but it contains a dot
            # then eval word up to ".", split the remaining part, get
            # the attributes of the module when the attribute starts
            # with the remaining
            dot_index = word.rfind('.')
            module = context.eval_code(word[:dot_index])
            if module:
                mword = word[dot_index+1:]
                completions = [i for i in get_dir(module) if i.startswith(mword)]
            else:
                completions = []
    return sorted(list(set(completions)))

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
    return sorted(list(set(attributes)))
