"""
Manage experiments. 
    
Usage: 
    wrf4g experiment list
    wrf4g experiment <name> start   [ --dbg ] [ --template=<name> ] [ --dir=<directory> ] 
    wrf4g experiment <name> create  [ --dbg ] [ --reconfigure ] [ --dry-run ]  EXP_CONF_FILE
    wrf4g experiment <name> submit  [ --dbg ] [ --rerun ] [ --dry-run ] 
    wrf4g experiment <name> status  [ --dbg ] [ --long ]
    wrf4g experiment <name> stop    [ --dbg ] [ --dry-run ] 
    wrf4g experiment <name> delete  [ --dbg ] [ --dry-run ]
   
Options:
   --dbg                Debug mode.
   -n --dry-run         Dry run.
   -l --long            Show a detailed status.
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
from os.path              import expanduser, exists, expandvars
from wrf4g                import logger, WRF4G_DIR, DB4G_CONF
from wrf4g.utils          import validate_name, VarEnv


def start( exp_name, exp_template ):
    validate_name( exp_name )
    if not arg[ '--template' ] in [ 'default', 'single', 'physics' ] :
        raise Exception( "'%s' template does not exist" % exp_template )
    exp_dir = expandvars( expanduser( arg[ '--dir' ] ) )
    if not exists( exp_dir ):
        raise Exception("'%s' does not exist" % exp_dir )
    exp_dir_config = join( exp_dir, exp_name )
    if exists( exp_dir_config ):
        raise Exception("'%s' already exists" % exp_dir_config )
    logger.debug( "Creating '%s' directory" % exp_dir_config )
    shutil.copytree( join( WRF4G_DIR , 'etc' , 'templates' , 'experiments',  exp_template ),
                     exp_dir_config )
    for file in [ 'resources.wrf4g' , 'experiment.wrf4g' ] :
        dest_path = join( exp_dir_config , file )
        with open( dest_path , 'r') as f :
            data = ''.join( f.readlines( ) )
        data_updated = data % {
                               'WRF4G_EXPERIMENT_HOME' : exp_dir_config ,
                               'WRF4G_DIR_LOCATION'    : WRF4G_DIR_LOCATION ,
                               'exp_name'              : exp_name ,
                               }
        with open( dest_path , 'w') as f :
            f.writelines( data_updated )


def run( arg ) :
    if arg[ '--dbg' ] :
        logger.setLevel( logging.DEBUG )
        logging.getLogger( 'sqlalchemy.engine' ).setLevel( logging.DEBUG )
    if arg[ 'start' ] :
        start( arg[ '<name>' ], arg[ '--template' ] )
    else :
        try :
            db4g_urls = VarEnv( DB4G_CONF ).get_variable( 'URL' )
            # an Engine, which the Session will use for connection
            engine = create_engine( db4g_url )
            # create a configured "Session" class
            Session = sessionmaker(bind = engine)
            # create a Session
            session = Session()
            try :
                q_exp = session.query( Experiment ).\
                        filter( Experiment.name == arg[ '<name>' ] ).one()
            except NoResultFound :
                logger.error( "'%s' experiment does not exist" % arg[ '<name>' ] )
            exp = q_exp()
            exp.session = session
            if arg[ 'submit' ] :
                exp.run( rerun = arg[ 'rerun' ], dryrun = arg[ '--dry-run' ] )
            elif arg[ 'create' ] :              
                call(['bash', 'create_exp'] + arg )
            elif arg[ 'status' ] :
                exp.status( )
            elif arg[ 'stop' ] :
                exp.stop( dryrun = arg[ '--dry-run' ] )
            elif arg[ 'delete' ] :
                exp.delete( )
            else :
                for exp in session.query( Experiment ).all() :
                    logger.info( exp.name )
            if arg[ '--dry-run' ] :
                session.rollback()
            else :
                session.commit()
        except Exception , err :
             session.rollback()
             logger.error( str( err ) )
        finally:
             session.close()
