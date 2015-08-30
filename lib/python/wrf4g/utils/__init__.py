__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"


class Enumerate( frozenset ):

    def __new__(cls, *args):
        return frozenset.__new__(cls, args)

    def __getattr__(self, name):
        if name in self:
            return name
        else:
            raise AttributeError("No '%s' in enumeration '%s'"
                                   % (name, self.__class__.__name__))

