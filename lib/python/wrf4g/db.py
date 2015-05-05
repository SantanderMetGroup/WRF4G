from sqlalchemy                 import ( Column, INTEGER, 
                                         VARCHAR, SMALLINT, 
                                         DATETIME, ForeignKey,
                                         create_engine )
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm             import relationship, sessionmaker
from wrf4g                      import DB4G_CONF, logger
from wrf4g.utils                import VarEnv


__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"


DEFAULT_DB_CONF = """[DEFAULT]
URL = mysql+pymysql://wrf4guser:Meteo2011@%(hostname)s:%(port)s/WRF4GDB
"""

def get_session():
    """
    Create a sqlalchemy session to connect with WRF4G database
    """
    logger.debug( "Reading database configuration from '%s' file" % DB4G_CONF  )
    db4g_urls = VarEnv( DB4G_CONF ).get_variable( 'URL' )
    # an Engine, which the Session will use for connection
    engine = create_engine( db4g_urls )
    # create a configured "Session" class
    Session = sessionmaker(bind = engine)
    # create a Session
    return Session()

Base = declarative_base()