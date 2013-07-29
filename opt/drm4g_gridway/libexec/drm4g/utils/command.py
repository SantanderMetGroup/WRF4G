import re

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id: command.py 1051 2011-07-20 08:33:33Z carlos $"

r = re.compile(r'[:,\s]') # match whitespac, coma or :

def parse(output):
    output = [r.split(line) for line in output.splitlines()]
    # now we have a list of lists, but it may contain empty strings
    for line in output:
        while '' in line:
            line.remove('')
    # turn into dict and return
    return dict([(line[0],line[1:]) for line in output])	
