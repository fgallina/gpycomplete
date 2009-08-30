import gpycomplete
import unittest

code = """
import os\n
import sys\n

def function(a, b=1):\n\t
    bar = a\n\t
    foo = 2\n

class Class(object):\n\t

    def __init__(self, arg1, arg2=2):\n\t
        self.arg1 = arg1\n
        self.arg2 = arg2\n

    def test(self, append):\n\t
        print self.arg1 + append

c = Class(1, 2)
"""


class BaseTestCase(unittest.TestCase):

    def setUp(self):
        gpycomplete._exec_code(code)
