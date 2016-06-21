import re

__version__  = '2.4.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: command.py 2352 2015-02-24 10:23:57Z carlos $"

r = re.compile(r'[:,\s]') # match whitespac, coma or :

def parse(output):
    output = [r.split(line) for line in output.splitlines()]
    # now we have a list of lists, but it may contain empty strings
    for line in output:
        while '' in line:
            line.remove('')
    # turn into dict and return
    return dict([(line[0],line[1:]) for line in output])	
