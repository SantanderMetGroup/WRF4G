import re

__version__  = '2.2.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: command.py 2250 2014-08-27 09:04:57Z carlos $"

r = re.compile(r'[:,\s]') # match whitespac, coma or :

def parse(output):
    output = [r.split(line) for line in output.splitlines()]
    # now we have a list of lists, but it may contain empty strings
    for line in output:
        while '' in line:
            line.remove('')
    # turn into dict and return
    return dict([(line[0],line[1:]) for line in output])	
