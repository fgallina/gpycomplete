import sys


class MyClass(object):
    """This is a test class
    """

    def __init__(self, arg1):
        """

        Arguments:
        - `arg1`:
        """
        self._arg1 = arg1

    def some_def(self):
        print "some_def"

    def another_def(self, arg1, arg2):
        print arg1 + arg2

    class c():

        def __init__(self):
            pass


def a_great_def(arg):
    """This is the best function of the world"""
    aasd = 4


c = MyClass("test")

c.another_def("arg1", "arg2")

c.some_def()

a_great_def("yes, this works fine")

sys.path.append("/usr/lib/mypythonpath")

obj = MyClass("asd")
