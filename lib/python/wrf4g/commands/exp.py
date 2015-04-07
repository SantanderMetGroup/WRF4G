"""
Manage WRF4G experiments. 
    
Usage: 
    wrf4g exp list           [ --dbg ] [ --long ] [ --pattern=<name> ]
    wrf4g exp <name> start   [ --dbg ] [ --template=<name> ] [ --dir=<directory> ] 
    wrf4g exp <name> create  [ --dbg ] [ --reconfigure ] [ --dry-run ] [ --dir=<directory> ]
    wrf4g exp <name> submit  [ --dbg ] [ --rerun ] [ --dry-run ] 
    wrf4g exp <name> status  [ --dbg ] [ --long ] [ --pattern=<name> ] 
    wrf4g exp <name> stop    [ --dbg ] [ --dry-run ] 
    wrf4g exp <name> delete  [ --dbg ] [ --dry-run ]
   
Options:
   --dbg                Debug mode.
   -n --dry-run         Dry run.
   -l --long            Show a detailed information.
   -p --pattern=<name>  Pattern to find experiments and realizations. 
   -t --template=<name> Experiment template, avaible templates are default, single, physics. 
   -d --dir=<directory> Directory to create or start an experiment [default: ./].
   -r --reconfigure     Change the features of a created experiment.
   --rerun              Force to run although this realization or experiment has finished.
  
Commands:
   list                 Show all the experiments available.
   start                Create the files needed to define a WRF4G experiment.
   create               Given experiment.wrf4g and resources.wrf4g files, prepare the 
                        experiment creating the realization and chunks needed to perform it.
   submit               Submit the experiment.        
   status               Check the status of the experiment realizations.
   stop                 Stop the active realizations by killing their jobs.
   delete               Remove the experiment from the database.
"""
__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

import logging
import shutil
from subprocess           import call
from sqlalchemy.orm.exc   import NoResultFound
from wrf4g                import logger
from wrf4g.db             import get_session
from wrf4g.core           import Experiment

def run( arg ) :
    if arg[ '--dbg' ] :
        logger.setLevel( logging.DEBUG )
    if arg[ 'start' ] :
        Experiment.create_files( arg[ '<name>' ], arg[ '--template' ], arg[ '--dir' ] )
    elif arg[ 'create' ] :
        call(['bash', 'create_exp'] + arg )
    else :
        # create a session to connect with the database
        session = get_session()
        try :
            # Options 
            if arg[ 'list' ] :
                exp = Experiment()
                exp.session = session
                exp.list( long_format = arg[ '--long' ] , pattern = arg[ '--pattern' ] )
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
                        exp.status( long_format = arg[ '--long' ], pattern = arg[ '--pattern' ]  )
                    elif arg[ 'stop' ] :
                        exp.stop( dryrun = arg[ '--dry-run' ] )
                    else :
                        exp.delete( dryrun = arg[ '--dry-run' ]  )
            if arg[ '--dry-run' ] :
                session.rollback()
            else :
                session.commit()
        except Exception , err :
            session.rollback()
            logger.error( str( err ) )
        finally:
            session.close()
