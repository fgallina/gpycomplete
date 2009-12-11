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
import pydoc
import types
import inspect
import context


def get_signature(obj):
    """Returns the signature of the given object.
    Inspired in the original pycomplete package
    """
    # FIXME - make this function less ugly
    paren = obj.find("(")
    if paren != -1:
        obj = obj[:paren]
    context_dict = 'subprogram_globals'
    if not obj in context.get_context():
        context_dict = 'helper_globals'
        if not context.cimport(obj, context_dict):
            return "no signature for " + obj
    try:
        obj = context.eval_code(obj, context_dict)
    except:
        return "no signature for " + obj
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


def get_help(obj):
    """Returns the help of the given object.
    Inspired in the original pycomplete package
    """
    paren = obj.rfind("(")
    if paren != -1:
        obj = obj[:paren]
    if obj.endswith("(") or obj.endswith("."):
        obj = obj[:-1]
    found = False
    pobj = None
    context_dict = 'subprogram_globals'
    if not obj in context.get_context():
        context_dict = 'helper_globals'
        found = context.cimport(obj, context_dict)
    else:
        pobj = context.eval_code(obj)
    if obj not in context.subcontext_globals and found:
        pobj = context.eval_code(obj, context_dict)
    if not pobj:
        return "no help string for " + obj
    obj = context.eval_code(obj)
    return pydoc.getdoc(obj)
