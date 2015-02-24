import logging

__version__  = '2.3.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: logger.py 2352 2015-02-24 10:23:57Z carlos $"


DEBUG = logging.DEBUG
INFO = logging.INFO
WARNING = logging.WARNING
ERROR = logging.ERROR
CRITICAL = logging.CRITICAL

def log_to_file(filename, level=DEBUG):
    l = logging.getLogger("drm4g")
    if len(l.handlers) > 0:
        return
    l.setLevel(level)
    f = open(filename, 'w')
    lh = logging.StreamHandler(f)
    lh.setFormatter(logging.Formatter('%(levelname)-.3s [%(asctime)s.%(msecs)03d] %(name)s: %(message)s',
                                      '%Y%m%d-%H:%M:%S'))
    l.addHandler(lh)

def get_logger(name):
    return logging.getLogger(name)
