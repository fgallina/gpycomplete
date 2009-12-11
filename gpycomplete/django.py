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
import sys

def set_project(path, settings='settings'):
    """Adds the django proyect path to the pythonpath and sets the
    DJANGO_SETTINGS_MODULE environment variable to the value specified
    in the settings param
    """
    sys.path.append(path)
    os.environ['DJANGO_SETTINGS_MODULE'] = settings
    return "Path: " + path + " // Settings: " + settings
