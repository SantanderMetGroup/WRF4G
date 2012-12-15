import logging
import logging.config

__version__ = '0.1'
__author__  = 'Santander Meteorology Group'
__revision__ = "$Id$"


class Logger():
    """
    This class provides a simple interface for simple logging usage
    """
    def __init__(self, loggingFileConf):
        logging.config.fileConfig(loggingFileConf)
        
    def getLogger(self, loggerName):
        return logging.getLogger(loggerName)
    
    
"""
Example of logging file configuration:
  * The first handler lets a log file grow to a certain size, then open a new file and log to that.
  * The second handler does basic configuration for the logging system.   

[loggers]
keys=root

[handlers]
keys=hand01,hand02

[formatters]
keys=form01

[logger_root]
level=NOTSET
propagate=1
channel=
parent=
qualname=(root)
handlers=hand01,hand02

[handler_hand01]
class=handlers.RotatingFileHandler
level=NOTSET
formatter=form01
filename=logrecv.log
mode=w
maxBytes=1000000 
backupCount=30
args=('logrecv.log', 'w' ,1000000, 30)

[handler_hand02]
class=StreamHandler
level=NOTSET
formatter=form01
stream=sys.stderr
args=(sys.stderr,)

[formatter_form01]
format=%(asctime)s %(levelname)-9s %(name)-8s %(message)s
datefmt=

"""