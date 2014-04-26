from __future__   import with_statement
import os
import sys
from os.path      import dirname, join , abspath , exists

__version__  = '1.5.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

HOME                      = os.environ.get( 'HOME' )
WRF4G_LOCATION            = os.environ.get( 'WRF4G_LOCATION' , join ( HOME , '.wrf4g' ) )
WRF4G_DEPLOYMENT_LOCATION = dirname( dirname( dirname( dirname( abspath( __file__ ) ) ) ) )
WRF4G_SHELL               = os.environ.get( 'WRF4G_SHELL', None )
HOSTNAME                  = os.environ.get( 'HOSTNAME')
DB4G_CONF                 = os.environ.get( 'DB4G_CONF' ,  join( WRF4G_LOCATION , 'etc' , 'db.conf' ) )       
GW_LOCATION               = os.environ[ 'GW_LOCATION' ] = join( WRF4G_LOCATION , 'opt' , 'gw_drm4g' )
GW_BIN_LOCATION           = join( WRF4G_DEPLOYMENT_LOCATION , 'opt' , 'gw_drm4g' , 'bin' )
GW_LIB_LOCATION           = join( WRF4G_DEPLOYMENT_LOCATION , 'opt' , 'gw_drm4g' , 'libexec' )
MYSQL_LOCATION            = join( WRF4G_DEPLOYMENT_LOCATION , 'opt' , 'mysql-5.5' )

if exists( WRF4G_LOCATION ) is False and WRF4G_LOCATION != "NONE" :
    print "Creating a WRF4G local configuration in '%s'" % WRF4G_LOCATION
    directories  = [
                    'var/log' , 
                    'var/submission' , 
                    'var/mysql' ,
                    'opt/gw_drm4g/var' ,
                    ]
    for dir in directories :
        abs_dir = join ( WRF4G_LOCATION , dir )
        print "Creating '%s' directory" % abs_dir
        os.makedirs( abs_dir )
    from  shutil import copytree
    for dir_to_copy in [ 'etc' , 'opt/gw_drm4g/etc' ] :
        src  = join ( WRF4G_DEPLOYMENT_LOCATION  , dir_to_copy )
        dest = join ( WRF4G_LOCATION             , dir_to_copy )
        print "Coping from '%s' to '%s'" % ( src , dest )
        copytree( src , dest )
    with open( DB4G_CONF , 'r') as f :
        data = ''.join( f.readlines( ) )
    data_updated = data % { 'hostname' :  HOSTNAME }
    with open( DB4G_CONF , 'w') as f :
        f.writelines( data_updated )
try :
    import logging.config  
    logging.config.fileConfig( 
                              join( WRF4G_LOCATION , 'etc' , 'logger.conf') , 
                              {'WRF4G_LOCATION' : WRF4G_LOCATION } 
                               )
except : 
    pass

if not WRF4G_SHELL :
    import readline
    import atexit
    # history file
    histfile = join(  WRF4G_LOCATION , '.wrf4ghistory' )
    try:
        readline.read_history_file( histfile )
    except IOError:
        pass
    atexit.register(readline.write_history_file, histfile)
    del histfile, readline, atexit


