"""
Manage WRF4G experiments. 
    
Usage: 
    wrf4g exp list          [ options ] [ --long ] [ --pattern=<name> ]
    wrf4g exp <name> define [ options ] [ --force ] [ --exp-template=<name> ] [ --dir=<directory> ] 
    wrf4g exp <name> create [ options ] [ --dir=<directory> ]
    wrf4g exp <name> update [ options ] [ --dir=<directory> ]
    wrf4g exp <name> submit [ options ] [ --rerun ] 
    wrf4g exp <name> status [ options ] [ --long ] [ --pattern=<name> ] 
    wrf4g exp <name> stop   [ options ]  
    wrf4g exp <name> delete [ options ]
   
Options:
    --dbg                    Debug mode.
    -n --dry-run             Dry run.
    -l --long                Show a detailed information.
    -f --force               Force to remove if it exists.
    -p --pattern=<name>      Pattern to find experiments and realizations. 
    -t --exp-template=<name> Experiment template, avaible templates are default, single, physics [default: default]. 
    -d --dir=<directory>     Directory to create or start an experiment [default: ./].
    --rerun                  Force to run although this realization or experiment has finished.
  
Commands:
    list                     Show all the experiments available.
    define                   Create the files needed to define a WRF4G experiment.
    create                   Given experiment.wrf4g file, prepare the experiment creating the realizations and chunks needed.
    update                   Update the experiment configuration.
    submit                   Submit the experiment.
    status                   Check the status of the experiment realizations.
    stop                     Stop the active realizations by killing their jobs.
    delete                   Remove the experiment from the database.
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
from wrf4g.utils          import datetime2datewrf

def run( arg ) :
    if arg[ '--dbg' ] :
        logger.setLevel( logging.DEBUG )
    if arg[ 'define' ] :
        Experiment.create_files( arg[ '<name>' ], arg[ '--template' ], arg[ '--force' ], arg[ '--dir' ] )
    else :
        # create a session to connect with the database
        session = get_session()
        try :
            if arg[ 'list' ] :
                l_exps = session.query( Experiment )
                if arg[ '--pattern' ] :
                    l_exps  = l_exps.filter( Experiment.name.like( arg[ '--pattern' ].replace('*','%') ) )
                if not arg[ '--long' ] and l_exps.all() :
                    # Header
                    logger.info( "Name" )
                    for e in l_exps :
                        logger.info( e.name )
                elif l_exps.all() :
                    # Header
                    logger.info( "%-20s %-20s %-20s %-12s %-10s" % ( 
                           "Name", "Start Date" , "End Date", "Mult Paramts", "Mult Dates" ) )
                    for e in l_exps :
                        logger.info("%-20.20s %-20.20s %-20.20s %-12.12s %-10.10s" % ( 
                                 e.name, datetime2datewrf(e.sdate), datetime2datewrf(e.edate), 
                                 e.mult_parameters, e.mult_dates ) )
            else :
                try :
                    exp = session.query( Experiment ).\
                            filter( Experiment.name == arg[ '<name>' ] ).one()
                except NoResultFound :
                    if arg[ 'create' ] :
                        exp2 = Experiment()
                        exp2.name = arg[ '<name>' ]
                        exp2.create( False, arg[ '--dry-run' ],  arg[ '--dir' ] )
                        session.add( exp2 )
                    else :
                        raise Exception( "'%s' experiment does not exist" % arg[ '<name>' ] )
                else :
                    # Options 
                    if arg[ 'update' ] :
                        exp.create( True, arg[ '--dry-run' ],  arg[ '--dir' ] )
                    elif arg[ 'submit' ] :
                        exp.run( arg[ '--rerun' ], arg[ '--dry-run' ] )
                    elif arg[ 'status' ] :
                        exp.get_status( arg[ '--long' ], arg[ '--pattern' ]  )
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
