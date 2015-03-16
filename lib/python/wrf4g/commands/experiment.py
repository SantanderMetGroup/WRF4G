"""
Manage experiments. 
    
Usage: 
    wrf4g experiment list [ --dbg ] [--long ]
    wrf4g experiment <name> start   [ --dbg ] [ --template=<name> ] [ --dir=<directory> ] 
    wrf4g experiment <name> create  [ --dbg ] [ --reconfigure ] [ --dry-run ]  EXP_CONF_FILE
    wrf4g experiment <name> submit  [ --dbg ] [ --rerun ] [ --dry-run ] 
    wrf4g experiment <name> status  [ --dbg ] [ --long ]
    wrf4g experiment <name> stop    [ --dbg ] [ --dry-run ] 
    wrf4g experiment <name> delete  [ --dbg ] [ --dry-run ]
   
Options:
   --dbg                Debug mode.
   -n --dry-run         Dry run.
   -l --long            Show a detailed information.
   -t --template=<name> Experiment template, avaible templates are default, single, physics. 
   -d --dir=<directory> Directory to create the experiment [default: ./].
   -r --reconfigure     Change the features of a created experiment.
   --rerun              Force to run although this realization or experiment has finished.
   

"""
__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

import logging
import shutil
import wrf4g.core         import Experiment
from subprocess           import call
from sqlalchemy           import create_engine
from sqlalchemy.orm       import sessionmaker
from sqlalchemy.orm.exc   import NoResultFound
from wrf4g                import logger, DB4G_CONF
from wrf4g.utils          import VarEnv


def run( arg ) :
    if arg[ '--dbg' ] :
        logger.setLevel( logging.DEBUG )
        logging.getLogger( 'sqlalchemy.engine' ).setLevel( logging.DEBUG )
    if arg[ 'start' ] :
        Experiment.start( arg[ '<name>' ], arg[ '--template' ], arg[ '--dir' ] )
    elif arg[ 'create' ] :
        call(['bash', 'create_exp'] + arg )
    else :
        try :
            db4g_urls = VarEnv( DB4G_CONF ).get_variable( 'URL' )
            # an Engine, which the Session will use for connection
            engine = create_engine( db4g_url )
            # create a configured "Session" class
            Session = sessionmaker(bind = engine)
            # create a Session
            session = Session()
            # Options 
            if arg[ 'list' ] :
                exp = Experiment()
                exp.session = session
                exp.list( arg['--long'] )
            else :
                try :
                    q_exp = session.query( Experiment ).\
                            filter( Experiment.name == arg[ '<name>' ] ).one()
                except NoResultFound :
                    raise Exception( "'%s' experiment does not exist" % arg[ '<name>' ] )
                else :
                    exp = q_exp()
                    exp.session = session
                    if arg[ 'submit' ] :
                        exp.run( rerun = arg[ 'rerun' ], dryrun = arg[ '--dry-run' ] )
                    elif arg[ 'status' ] :
                        exp.status( )
                    elif arg[ 'stop' ] :
                        exp.stop( dryrun = arg[ '--dry-run' ] )
                    else :
                        exp.delete( )
            if arg[ '--dry-run' ] :
                session.rollback()
            else :
                session.commit()
        except Exception , err :
             session.rollback()
             logger.error( str( err ) )
        finally:
             session.close()
