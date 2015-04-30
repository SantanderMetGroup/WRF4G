"""
Manage WRF4G experiments. 
    
Usage: 
    wrf4g exp list           [ --dbg ] [ --long ] [ --pattern=<name> ]
    wrf4g exp <name> start   [ --dbg ] [ --template=<name> ] [ --dir=<directory> ] 
    wrf4g exp <name> create  [ --dbg ] [ --dry-run ] [ --dir=<directory> ]
    wrf4g exp <name> update  [ --dbg ] [ --dry-run ] [ --dir=<directory> ]
    wrf4g exp <name> submit  [ --dbg ] [ --rerun ] [ --dry-run ] 
    wrf4g exp <name> status  [ --dbg ] [ --long ] [ --pattern=<name> ] 
    wrf4g exp <name> stop    [ --dbg ] [ --dry-run ] 
    wrf4g exp <name> delete  [ --dbg ] [ --dry-run ]
   
Options:
    --dbg                 Debug mode.
    -n --dry-run          Dry run.
    -l --long             Show a detailed information.
    -p --pattern=<name>   Pattern to find experiments and realizations. 
    -t --template=<name>  Experiment template, avaible templates are default, single, physics [default: default]. 
    -d --dir=<directory>  Directory to create or start an experiment [default: ./].
    --rerun               Force to run although this realization or experiment has finished.
  
Commands:
    list                  Show all the experiments available.
    start                 Create the files needed to define a WRF4G experiment.
    create                Given experiment.wrf4g file, prepare the 
                          experiment creating the realizations and chunks needed.
    update                Update the experiment configuration.
    submit                Submit the experiment.
    status                Check the status of the experiment realizations.
    stop                  Stop the active realizations by killing their jobs.
    delete                Remove the experiment from the database.
"""
__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

import logging
import shutil
from sqlalchemy.orm.exc   import NoResultFound
from wrf4g                import logger
from wrf4g.db             import get_session
from wrf4g.core           import Experiment

def run( arg ) :
    if arg[ '--dbg' ] :
        logger.setLevel( logging.DEBUG )
    if arg[ 'start' ] :
        Experiment.create_files( arg[ '<name>' ], arg[ '--template' ], arg[ '--dir' ] )
    else :
        # create a session to connect with the database
        session = get_session()
        try :
            if arg[ 'list' ] :
                q_exps = session.query( Experiment )
                if arg[ '--pattern' ] :
                    q_exps  = q_exps.filter( Experiment.name.like( arg[ '--pattern' ].replace('*','%') ) )
                if not arg[ '--long' ] and q_exps.all() :
                    # Header
                    logger.info( "Name" )
                    for e in q_exps :
                        logger.info( e.name )
                elif q_exps.all() :
                    # Header
                    logger.info( "%20s %20s %20s %12s %10s %-30s" % ( 
                           "Name", "Start Date" , "End Date", "Mult Paramts", "Mult Dates", "Mult Labels")  )
                    for e in q_exps :
                        logger.info("%20.20s %20.20s %20.20s %12.12s %10.10s %-30.30s" % ( 
                                 e.name, datetime2datewrf(e.sdate), datetime2datewrf(e.edate), 
                                 e.mult_parameters, e.mult_dates, e.mult_labels ) )
            else :
                try :
                    q_exp = session.query( Experiment ).\
                            filter( Experiment.name == arg[ '<name>' ] ).one()
                except NoResultFound :
                    if arg[ 'update' ] :
                        exp = Experiment()
                        exp.create( True, arg[ '--dry-run' ],  arg[ '--dir' ] )
                        session.add( exp )
                    else :
                        raise Exception( "'%s' experiment does not exist" % arg[ '<name>' ] )
                else :
                    exp = q_exp()
                    # Options 
                    if arg[ 'create' ] :
                        exp.create( False, arg[ '--dry-run' ],  arg[ '--dir' ] )
                    elif arg[ 'submit' ] :
                        exp.run( arg[ 'rerun' ], arg[ '--dry-run' ] )
                    elif arg[ 'status' ] :
                        exp.status( arg[ '--long' ], arg[ '--pattern' ]  )
                    elif arg[ 'stop' ] :
                        exp.stop( arg[ '--dry-run' ] )
                    else :
                        exp.delete( arg[ '--dry-run' ] )
                        session.delete( exp )
                        logger.info( "'%s' experiment has been deleted from the database" % exp.name )
            if arg[ '--dry-run' ] :
                session.rollback()
            else :
                session.commit()
        except Exception , err :
            session.rollback()
            logger.error( str( err ) )
        finally:
            session.close()
