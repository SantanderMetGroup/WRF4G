import threading

__version__  = '2.4.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: list.py 2352 2015-02-24 10:23:57Z carlos $"

class List (object):
    """
    Dictionary self-protected.
    """	
    def __init__(self):
        self._map = { }
        self._lock = threading.Lock()

    def put(self, key, value):
        self._lock.acquire()
        try:
            self._map[key] = value
        finally:
            self._lock.release()

    def get(self, key):
        self._lock.acquire()
        try:
            return self._map.get(key, None)
        finally:
            self._lock.release()

    def delete(self, key):
        self._lock.acquire()
        try:
            try:
                del self._map[key]
            except KeyError:
                pass
        finally:
            self._lock.release()
            
    def has_key(self, key):
        self._lock.acquire()
        try:
            return key in self._map
        finally:
            self._lock.release()

    def values(self):
        self._lock.acquire()
        try:
            return list(self._map.values())
        finally:
            self._lock.release()

    def items(self):
        self._lock.acquire()
        try:
            return list(self._map.items())
        finally:
            self._lock.release()



            
