"""
Manage WRF4G experiments. 
    
Usage: 
    wrf4g exp list          [ --pattern=<name> ]
    wrf4g exp <name> define [ --dbg ] [ --force ]   [ --from-template=<name> ] [ --dir=<directory> ] 
    wrf4g exp <name> edit   [ --dbg ] 
    wrf4g exp <name> create [ --dbg ] [ --dry-run ] [ --dir=<directory> ]
    wrf4g exp <name> update [ --dbg ] [ --dry-run ] [ --dir=<directory> ]
    wrf4g exp <name> submit [ --dbg ] [ --dry-run ] [ --pattern=<name> ] [ --rea-state=<state> ] [ --rerun ] 
    wrf4g exp <name> status [ --dbg ] [ --pattern=<name> ] [ --rea-state=<state> ]
    wrf4g exp <name> cancel [ --dbg ] [ --dry-run ] [ --pattern=<name> ] [ --rea-state=<state> ]
    wrf4g exp <name> delete [ --dbg ] [ --dry-run ] 
   
Options:
    --dbg                     Debug mode.
    -n --dry-run              Dry run.
    -f --force                Force to remove if it exists.
    -p --pattern=<name>       Pattern to find experiments and realizations.
    -s --rea-state=<state>    Select only realizations in the indicated state. Available states :
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
    status                    Check the status of realizations and chunks showing computing resources, 
                              job identifier and exit codes (SEE EXIT CODES) 
    cancel                    Cancel the active realizations by killing their jobs.
    delete                    Remove the experiment from the database.

EXIT CODES
1  : Error creating directory to simulate
2  : Error creating log directory        
3  : Error copying apps            
4  : Error app type does not exist            
5  : Error executing source script       
6  : Job already executed  
7  : Error copying restart files        
8  : There is a mismatch in the restart date   
9  : Error copying namelist.wps    
10 : Error downloading WPS files    
11 : Error copying boundaries           
12 : Error modifying namelist
13 : Error executing PREPROCESSOR
14 : Error linking GRIB files     
15 : Error executing UNGRIB
16 : Error executing METGRID       
17 : Error executing REAL
18 : Error uploadinf WPS files      
19 : Error executing WRF
20 : Error executing POSTPROCESSOR 
21 : Error copying output file     

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
                    logging.info( "\033[1;4m%-20.20s\033[0m" % ( "EXPERIMENT" ) )
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
                        exp.run( arg[ '--rerun' ], arg[ '--pattern' ], arg[ '--rea-state' ] )
                    elif arg[ 'status' ] :
                        exp.get_status( arg[ '--pattern' ], arg[ '--rea-state' ] )
                    elif arg[ 'cancel' ] :
                        exp.cancel( arg[ '--pattern' ], arg[ '--rea-state' ] )
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
