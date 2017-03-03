#
# Copyright 2016 Universidad de Cantabria
#
# Licensed under the EUPL, Version 1.1 only (the
# "Licence");
# You may not use this work except in compliance with the
# Licence.
# You may obtain a copy of the Licence at:
#
# http://ec.europa.eu/idabc/eupl
#
# Unless required by applicable law or agreed to in
# writing, software distributed under the Licence is
# distributed on an "AS IS" basis,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
# express or implied.
# See the Licence for the specific language governing
# permissions and limitations under the Licence.
#

def dict_compare(dict_one, dict_two ):
    """ 
    Compare two python dictionaries
    """
    dict_one_keys  = set( dict_one.keys() )
    dict_two_keys  = set( dict_two.keys() )
    intersect_keys = dict_one_keys.intersection( dict_two_keys  )
    added          = dict_one_keys - dict_two_keys
    removed        = dict_two_keys - dict_one_keys
    modified       = [ k for k in intersect_keys if dict_one[ k ] != dict_two[ k ] ]
    same           = set( k for k in intersect_keys if dict_one[ k ] == dict_two[ k ] )
    return ( added, removed, modified, same )

class dict2obj(dict):
    """
    Class to convert a dictionary to an object
    """
    def __init__(self, dictionary, default=None):
        self.__dictionary = dictionary
        self.__default = default
        super(self.__class__, self).__init__(dictionary)

    def __getattr__(self, key ):
        if key.startswith('__'):
            raise AttributeError
        if key in self.__dictionary :
            val = self.__dictionary[ key ]
            if isinstance(val, dict):
                val = self.__class__( val )
            setattr(self, key, val )
            return val
        return self.__default

    def __missing__(self, key):
        return False

class Enumerate( frozenset ):

    def __new__(cls, *args):
        return frozenset.__new__(cls, args)

    def __getattr__(self, name):
        if name in self:
            return name
        else:
            raise AttributeError("No '%s' in enumeration" % name )

