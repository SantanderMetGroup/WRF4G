__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id: url.py 1023 2011-07-07 15:46:10Z carlos $"

scheme   = None
netloc   = None
path     = None
params   = { }
query    = None
fragment = None

def urlparse (url):
    """
    Parse a URL and return a ParseResult object
    """
    global scheme, netloc, path, params, query, fragment
    if '://' in url:
        _scheme, url = url.split('://', 1)
        scheme = _scheme.lower()
    if '#' in url:
        url, fragment = url.split('#', 1)
    if '?' in url:
        url, query  = url.split('?', 1)
        params = dict(( elem.split('=')[0], elem.split('=')[1]) for elem in query.split(';') if '=' in elem)
    if '/' in url:
        if scheme == 'file': 
            path = url
        else:
            url, path  = url.split('/', 1)
    netloc = url        
    return ParseResult(scheme, netloc, path, params, query, fragment)

class ParseResult(object):
    """
    Methods for the parsed result objects
    """
    def __init__(self, scheme, netloc, path, params, query, fragment):

        self._scheme   = scheme
        self._netloc   = netloc
        self._path     = path
        self._params   = params
        self._query    = query
        self._fragment = fragment 
  
    def getscheme(self):
        return self._scheme

    def getnetloc(self):
        return self._netloc

    def getpath(self):
        return self._path

    def getparams(self):
        return self._params

    def getquery(self):
        return self._query
    
    def getfragment(self):
        return self._fragment

    def getusername(self):
        netloc = self._netloc
        if "@" in netloc:
            userinfo = netloc.split("@", 1)[0]
            if ":" in userinfo:
                userinfo = userinfo.split(":", 1)[0]
            return userinfo
        return None

    def getpassword(self):
        netloc = self._netloc
        if "@" in netloc:
            userinfo = netloc.split("@", 1)[0]
            if ":" in userinfo:
                return userinfo.split(":", 1)[1]
        return None

    def gethost(self):
        netloc = self._netloc
        if "@" in netloc:
            netloc = netloc.split("@", 1)[1]
        if ":" in netloc:
            netloc = netloc.split(":", 1)[0]
        return netloc or None

    def getport(self):
        netloc = self._netloc
        if "@" in netloc:
            netloc = netloc.split("@", 1)[1]
        if ":" in netloc:
            port = netloc.split(":", 1)[1]
            return int(port, 10)
        return None

    scheme   = property(getscheme)
    netloc   = property(getnetloc)
    path     = property(getpath)
    params   = property(getparams)
    query    = property(getquery)
    fragment = property(getfragment)
    username = property(getusername)
    password = property(getpassword)
    host     = property(gethost)
    port     = property(getport) 
