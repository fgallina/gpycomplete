import sys

def set_project(path, settings='settings'):
    """Adds the django proyect path to the pythonpath and sets the
    DJANGO_SETTINGS_MODULE environment variable to the value specified
    in the settings param
    """
    sys.path.append(path)
    os.environ['DJANGO_SETTINGS_MODULE'] = settings
    return "Path: " + path + " // Settings: " + settings
