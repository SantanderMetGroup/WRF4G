#
# Copyright 2016 Universidad de Cantabria
#
# Licensed under the EUPL, Version 1.1 only (the
# "Licence");
# You may not use this work except in compliance with the
# Licence.
# You may obtain a copy of the Licence at:
#
# http://ec.europa.eu/idabc/eupl
#
# Unless required by applicable law or agreed to in
# writing, software distributed under the Licence is
# distributed on an "AS IS" basis,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
# express or implied.
# See the Licence for the specific language governing
# permissions and limitations under the Licence.
#

import os
import socket
import signal
import time
import logging
from sqlalchemy                 import create_engine 
from sqlalchemy.pool            import NullPool
from sqlalchemy.orm             import relationship, sessionmaker
from os.path                    import join, exists
from wrf4g                      import DB4G_CONF, WRF4G_DIR
from wrf4g.utils.command        import exec_cmd_advance as exec_cmd
from wrf4g.utils.file           import VarEnv

DEFAULT_DB_CONF = """[DEFAULT]
URL = sqlite:////%(WRF4G_DIR)s/etc/wrf4g.db
"""

def process_is_runnig( pid ):
    """
    Check is a process is running given a file
    """
    try:
        with open( pid , 'r' ) as f :
            lines = f.readlines()
        os.kill( int( lines[0].strip() ) , 0 )
    except :
        return False
    else:
        return True

class MySQLDB( object ):
    """
    Class to manage MySQL database
    """

    def __init__( self, port=25000 ):
        self.mysql_port = port
        self.mysql_pid  = join( WRF4G_DIR, 'var', 'mysql.pid' )
        self.mysql_sock = join( WRF4G_DIR, 'var', 'mysql.sock' )
        self.mysql_log  = join( WRF4G_DIR, 'var', 'log', 'mysql.log' )

    def _port_is_free( self ):
        sock = socket.socket( socket.AF_INET, socket.SOCK_STREAM )
        if sock.connect_ex( ( '127.0.0.1', int( self.mysql_port ) ) ) == 0 :
            return False
        else :
            return True

    def status( self ):
        if not exists( self.mysql_pid ) :
            logging.info( "WRF4G_DB (MySQL) has not started" )
        elif process_is_runnig( self.mysql_pid ) :
            logging.info( "WRF4G_DB (MySQL) is running" )
        else :
            logging.info( "WRF4G_DB (MySQL) is stopped" )

    def start( self ):
        logging.info( "Starting WRF4G_DB (MySQL) ... " )
        if not self._port_is_free() and not process_is_runnig( self.mysql_pid ):
            raise Exception( "WARNING: Another process is listening on port %s.\n"
              "Change the port by executing 'wrf4g start --db-port=new_port'." % self.mysql_port
              )
        elif not exists( self.mysql_pid ) or ( exists( self.mysql_pid ) and not process_is_runnig( self.mysql_pid ) ) :
            mysql_options = "--no-defaults --port=%s --socket=%s --log-error=%s --pid-file=%s" % ( self.mysql_port ,
                                                                                                     self.mysql_sock ,
                                                                                                     self.mysql_log ,
                                                                                                     self.mysql_pid
                                                                                                     )
            cmd =  "cd %s ; nohup ./bin/mysqld_safe %s &>/dev/null &" % ( MYSQL_DIR , mysql_options )
            exec_cmd( cmd , nohup = True, stdin = False )
            time.sleep( 2.0 )
            if not exists( self.mysql_pid ) or self._port_is_free() :
                logging.error( " ERROR: MySQL did not start, check '%s' for more information " % self.mysql_log )
            else :
                logging.info( " OK" )
        else :
            logging.warn( " WARNING: MySQL is already running" )

    def stop( self ):
        if not exists( self.mysql_pid ) :
            logging.info( "WRF4G_DB (MySQL) has not started" )
        else :
            logging.info( "Stopping WRF4G_DB (MySQL) ..." )
            if not exists( self.mysql_pid ) and not process_is_runnig( self.mysql_pid ) :
                logging.warn( " WARNING: MySQL is already stopped" )
            elif exists( self.mysql_pid ) and process_is_runnig( self.mysql_pid ) :
                with open( self.mysql_pid , 'r') as f:
                    pid = f.readline().strip()
                mysql_ppid, err = exec_cmd( "ps h -p %s -o ppid" % pid )
                if err :
                    raise Exception( err )
                try :
                    os.kill( int( mysql_ppid ), signal.SIGKILL )
                    os.kill( int( pid ), signal.SIGKILL )
                    logging.info( " OK" )
                except Exception as err :
                    logging.error( " ERROR: stopping MySQL: %s" % err )
            else :
                logging.warn( " WARNING: MySQL is already stopped" )

def get_session():
    """
    Create a sqlalchemy session to connect with WRF4G database
    """
    logging.debug( "Reading database configuration from '%s' file" % DB4G_CONF  )
    db4g_urls = VarEnv( DB4G_CONF ).get_var( 'URL' )
    # an Engine, which the Session will use for connection
    engine = create_engine( db4g_urls )
    # create a configured "Session" class
    Session = sessionmaker(bind = engine)
    # create a Session
    return Session()

def init_db():
    """
    Create WRF4G database tables
    """
    logging.debug( "Reading database configuration from '%s' file" % DB4G_CONF  )
    engine = create_engine( VarEnv( DB4G_CONF ).get_var( 'URL' ) )
    Session = sessionmaker( bind = engine )
    session = Session()
    from wrf4g.orm import metadata
    logging.debug( "Creating WRF4G tables" )
    metadata.create_all( engine )
