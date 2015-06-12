"""
Manage WRF4G experiments. 
    
Usage: 
    wrf4g exp list          [ --pattern=<name> ]
    wrf4g exp <name> define [ --dbg ] [ --force ]   [ --from-template=<name> ] [ --dir=<directory> ] 
    wrf4g exp <name> edit   [ --dbg ] 
    wrf4g exp <name> create [ --dbg ] [ --dry-run ] [ --dir=<directory> ]
    wrf4g exp <name> update [ --dbg ] [ --dry-run ] [ --dir=<directory> ]
    wrf4g exp <name> submit [ --dbg ] [ --dry-run ] [ --rerun ] 
    wrf4g exp <name> status [ --dbg ] [ --pattern=<name> ] [ --rea-state=<state> ]
    wrf4g exp <name> stop   [ --dbg ] [ --dry-run ]   
    wrf4g exp <name> delete [ --dbg ] [ --dry-run ]
   
Options:
    --dbg                     Debug mode.
    -n --dry-run              Dry run.
    -f --force                Force to remove if it exists.
    -p --pattern=<name>       Pattern to find experiments and realizations.
    -s --rea-state=<state>    Monitor only realizations in the indicated state. Available states :
                              PREPARED, SUBMITTED, RUNNING, PENDING, FAILED and FINISHED 
    -t --from-template=<name> Experiment template, avaible templates are default, single, physics [default: default]. 
    -d --dir=<directory>      Directory to create or start an experiment [default: ./].
    --rerun                   Force to run although this realization or experiment has finished.
  
Commands:
    list                      Show all the experiments available.
    define                    Create the files needed to define a WRF4G experiment.
    edit                      Edit experiment.wrf4g file.
    create                    Given experiment.wrf4g file, prepare the experiment creating the realizations and chunks needed.
    update                    Update the experiment configuration.
    submit                    Submit the experiment.
    status                    Check the status of the experiment realizations.
    stop                      Stop the active realizations by killing their jobs.
    delete                    Remove the experiment from the database.
"""
__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

import sys
import logging
import shutil
from sqlalchemy.orm.exc   import NoResultFound
from sqlalchemy.exc       import OperationalError
from wrf4g.db             import get_session
from wrf4g.core           import Experiment
from wrf4g.utils.time     import datetime2datewrf

def run( arg ) :
    logging.basicConfig( format = '%(message)s', 
                         level  = logging.DEBUG if arg[ '--dbg' ] else logging.INFO,  
                         stream = sys.stdout )
    if arg[ 'define' ] :
        Experiment.create_files( arg[ '<name>' ], arg[ '--from-template' ], arg[ '--force' ], arg[ '--dir' ] )
    else :
        # create a session to connect with the database
        session = get_session()
        try :
            if arg[ 'list' ] :
                l_exps = session.query( Experiment )
                if arg[ '--pattern' ] :
                    l_exps  = l_exps.filter( Experiment.name.like( arg[ '--pattern' ].replace('*','%') ) )
                if not l_exps.all() :
                    logging.info( "There are not experiments" )
                else :
                    # Header
                    logging.info( "%-20.20s" % ( "Name" ) )
                    for e in l_exps :
                        logging.info("%-20.20s" % ( e.name ) ) 
            else :
                try :
                    exp = session.query( Experiment ).\
                            filter( Experiment.name == arg[ '<name>' ] ).one()
                    exp.dryrun = arg[ '--dry-run' ]
                except NoResultFound :
                    if arg[ 'create' ] :
                        exp2        = Experiment()
                        exp2.name   = arg[ '<name>' ]
                        exp2.dryrun = arg[ '--dry-run' ]
                        exp2.create( False, arg[ '--dir' ] )
                        session.add( exp2 )
                    else :
                        raise Exception( "'%s' experiment does not exist" % arg[ '<name>' ] )
                else :
                    # Options 
                    if arg[ 'edit' ] :
                        exp.edit( )
                    if arg[ 'update' ] :
                        exp.create( True, arg[ '--dir' ] )
                    elif arg[ 'submit' ] :
                        exp.run( arg[ '--rerun' ] )
                    elif arg[ 'status' ] :
                        exp.get_status( arg[ '--pattern' ], arg[ '--rea-state' ] )
                    elif arg[ 'stop' ] :
                        exp.stop( )
                    elif arg[ 'delete' ] :
                        exp.delete( )
                        session.delete( exp )
                        logging.info( "'%s' experiment has been deleted from the database" % exp.name )
                    elif arg[ 'create' ] :
                        logging.info( "'%s' experiment already exists" % exp.name )
                        
            if arg[ '--dry-run' ] :
                session.rollback()
            else :
                session.commit()
        except OperationalError, err :
            logging.error( err.message )
        except Exception , err :
            session.rollback()
            logging.error( str( err ) )
        finally:
            session.close()
