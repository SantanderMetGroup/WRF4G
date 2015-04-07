"""
Manage WRF4G realizations. 
    
Usage: 
    wrf4g rea <name> submit [ <fist_ch> [ <last_ch> ] ] [ --dbg ] [ --rerun ] [ --dry-run ]
    wrf4g rea <name> status [ --dbg ] [ --long ] 
    wrf4g rea <name> stop   [ --dbg ] [ --dry-run ] 
   
Options:
   --dbg         Debug mode.
   -n --dry-run  Dry run.
   -l --long     Show a detailed information.
   --rerun       Force to run although the realization has finished.
  
Commands:
   submit        Submit the realization.        
   status        Check the status of the realization.
   stop          Stop the realization by killing its jobs.
"""
__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

import logging
from sqlalchemy.orm.exc   import NoResultFound
from wrf4g                import logger
from wrf4g.db             import get_session
from wrf4g.core           import Realization

def run( arg ) :
    if arg[ '--dbg' ] :
        logger.setLevel( logging.DEBUG )
    # create a session to connect with the database
    session = get_session()
    try :
        try : 
            q_rea = session.query( Realization ).\
                    filter( Realization.name == arg[ '<name>' ] ).one()
        except NoResultFound :
            raise Exception( "'%s' realization does not exist" % arg[ '<name>' ] )
        else :
            rea = q_rea()
            rea.session = session
            if arg[ 'submit' ] :
                rea.run( first_chunk_run = arg[ '<first_ch>' ], 
                         last_chunk_run  = arg[ '<last_ch>' ] , 
                         rerun           = arg[ 'rerun' ], 
                         dryrun          = arg[ '--dry-run' ] )
            elif arg[ 'status' ] :
                rea.status( long_format = arg[ '--long' ] )
            else :
                rea.stop( dryrun = arg[ '--dry-run' ] )
            if arg[ '--dry-run' ] :
                session.rollback()
            else :
                session.commit()
    except Exception , err :
        session.rollback()
        logger.error( str( err ) )
    finally:
        session.close()
