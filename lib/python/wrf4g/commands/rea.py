"""
Manage WRF4G realizations. 
    
Usage: 
    wrf4g rea <name> submit [ --dbg ] [ --dry-run ] [ --rerun ] [ <first_ch> [ <last_ch> ] ]
    wrf4g rea <name> status [ --dbg ]
    wrf4g rea <name> log    [ --dbg ] <chunk_id> [ --dir=<directory> ]
    wrf4g rea <name> cancel [ --dbg ] [ --dry-run ]
   
Options:
   --dbg                 Debug mode.
   -n --dry-run          Dry run.
   --rerun               Force to run although the realization has finished.
   -d --dir=<directory>  Directory to unpack log files [default: ./].
  
Commands:
   submit                Submit the realization.       
   status                Check the status of a realization.
   log                   Get log files from a chunk. 
   cancel                Cancel the realization by killing its jobs.
"""
__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

import logging
import sys
from sqlalchemy.orm.exc   import NoResultFound
from sqlalchemy.exc       import OperationalError
from wrf4g.db             import get_session
from wrf4g.core           import Realization

def run( arg ) :
    logging.basicConfig( format = '%(message)s',
                         level  = logging.DEBUG if arg[ '--dbg' ] else logging.INFO,
                         stream = sys.stdout )
    # create a session to connect with the database
    session = get_session()
    try :
        try : 
            rea = session.query( Realization ).\
                    filter( Realization.name   == arg[ '<name>' ], 
                            Realization.exp_id != None ).one()
            rea.dryrun = arg[ '--dry-run' ]
        except NoResultFound :
            raise Exception( "'%s' realization does not exist" % arg[ '<name>' ] )
        else :
            if arg[ 'submit' ] :
                rea.run( first_chunk_run = arg[ '<first_ch>' ], 
                         last_chunk_run  = arg[ '<last_ch>' ] , 
                         rerun           = arg[ '--rerun' ] )
            elif arg[ 'status' ] :
                logging.info( '\033[1;4m%-60s %-10s %-10s %-16s %-10s %6s %-3s %6s\033[0m'% (
                        'REALIZATION','STATUS','CHUNKS','RESOURCE','RUN STATUS',
                        'JID', 'EXT','%' ) )
                rea.get_status( )
            elif arg[ 'log' ] :
                rea.get_log( arg[ '<chunk_id>' ], arg[ '--dir' ] )
            else :
                rea.cancel( )
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
