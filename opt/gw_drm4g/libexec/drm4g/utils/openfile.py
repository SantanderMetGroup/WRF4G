import re

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id: openfile.py 1023 2011-07-07 15:46:10Z carlos $"


comment      = re.compile(r'#.*$')
line_comment = re.compile(r'^#+')

def cleaner(filename):
    """
    cleaner reads a file and returns a string without comments.
    Syntax of comment:
       # Comment
    """
    try: 
        f = open(filename, 'r')
    except IOError:
        print 'Cannot open ', path
    else:
        lines = f.readlines()
        f.close()
        return ''.join([re.sub(comment, '', line) for line in lines if not line_comment.search(line)]).rstrip()
