import os
import sys
from os.path  import dirname, join , abspath , exists

__version__  = '1.5.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

    
HOME                      = os.environ.get( 'HOME' )
WRF4G_LOCATION            = os.environ.get( 'WRF4G_LOCATION' , join ( HOME , '.wrf4g' ) )
WRF4G_DEPLOYMENT_LOCATION = dirname( dirname( dirname( dirname( abspath( __file__ ) ) ) ) )
DB4G_CONF                 = os.environ.get( 'DB4G_CONF' ,  join( WRF4G_LOCATION , 'etc' , 'db.conf' ) )       
GW_LOCATION               = os.environ[ 'GW_LOCATION' ] = join( WRF4G_LOCATION , 'opt' , 'gw_drm4g' )
GW_BIN_LOCATION           = join( WRF4G_DEPLOYMENT_LOCATION , 'opt' , 'gw_drm4g' , 'bin' )
GW_LIB_LOCATION           = join( WRF4G_DEPLOYMENT_LOCATION , 'opt' , 'gw_drm4g' , 'libexec' )
MYSQL_LOCATION            = join( WRF4G_DEPLOYMENT_LOCATION , 'opt' , 'mysql-5.5' )

if exists( WRF4G_LOCATION ) is False and WRF4G_LOCATION is not "NONE" :
    directories  = [
                    'var/log' , 
                    'var/submission' , 
                    'var/mysql' ,
                    'opt/gw_drm4g/var' ,
                    ]
    for dir in directories :
        os.makedirs( join ( WRF4G_LOCATION , dir ) )
    from  shutil import copytree
    for dir_to_copy in [ 'etc' , 'opt/gw_drm4g/etc' , 'repository' ] :
        copytree( 
                 join ( WRF4G_DEPLOYMENT_LOCATION  , dir_to_copy ) ,
                 join ( WRF4G_LOCATION             , dir_to_copy )
                 )    
try :
    import logging.config  
    logging.config.fileConfig( 
                              join( WRF4G_LOCATION , 'etc' , 'logger.conf') , 
                              {'WRF4G_LOCATION' : WRF4G_LOCATION } 
                               )
except : pass
