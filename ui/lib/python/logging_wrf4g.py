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
        
    def get_logger(loggerName):
        return logging.getLogger(loggerName)
    
    
"""
Example of logging file conf

[loggers]
keys=root,simpleExample

[handlers]
keys=consoleHandler

[formatters]
keys=simpleFormatter

[logger_root]
level=DEBUG
handlers=consoleHandler

[logger_simpleExample]
level=DEBUG
handlers=consoleHandler
qualname=simpleExample
propagate=0

[handler_consoleHandler]
class=StreamHandler
level=DEBUG
formatter=simpleFormatter
args=(sys.stdout,)

[formatter_simpleFormatter]
format=%(asctime)s - %(name)s - %(levelname)s - %(message)s
datefmt=
"""