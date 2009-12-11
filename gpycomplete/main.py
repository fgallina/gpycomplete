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
import complete
import context
import helpers
import django

def refresh_context(code):
    return context.refresh_context(code)

def get_completions(obj, code, subcontext="", cursor_indentation=""):
    return complete.get_completions(obj, code, subcontext, cursor_indentation)

def get_help(obj):
    return helpers.get_help(obj)

def get_signature(obj):
    return helpers.get_signature(obj)

def refresh_context(code):
    return context.refresh_context(code)

def set_path(path):
    return context.set_path(path)

def set_django_project(path, settings='settings'):
    return django.set_project(path, settings_module)

if __name__ == "__main__":

    code = """
import sys\n
import os\n
import glob\n

def function(a, b=1):\n\t
  bar = a\n\t
  foo = 2\n

class Class(object):\n\t
  def __init__(self, arg1):\n\t
    self._arg1 = arg1\n

  def test(self, arg):\n\t
    a = self._arg1"""

    print 'completions:'
    print get_completions("sys.", code)[:4]
    print get_completions("os.", code)[:4]
    print get_completions("func", code)
    print get_completions("Cla", code)
    print get_completions("fo", code, [["","def","function"]], "\t")
    print get_completions("self._", code, [["    ","def","test"]], "\t")
    print ''
    print 'signatures'
    print context.get_context()
    print get_signature("function(")
    print get_signature("glob.glob")
    print ''
    print 'helpers'
    print get_help("help")
    print get_help("os.path")
