"""
Virtual copy a command to copy files using different protocols.
        
Usage:
    wrf4g vcp [ --dbg ] [ --overwrite  ] <source> <dest>

Arguments:

Options:
    --dbg           Debug mode.
    -o --overwrite  If the destination already exists, it will be overwritten.
    
Supported protocols:        
    LFN             lfn:///grid/VO/file file://home/user/file 
    GRIDFTP         gridftp://computer:2812/grid/VO/user/file
    RSYNC           rsync://user@computer:34/grid/VO/user/file
    SIMBOLIC LINK   ln:/home/user/file or ln:file
    FILE            file:/home/user/file file:/home/user/file2
    HTTPS           https://www.meteo.unican.es/work/WRF4G.tar.gz
    HTTP            http://www.meteo.unican.es/work/WRF4G.tar.gz
    FTP             ftp://www.meteo.unican.es/work/WRF4G.tar.gz
"""
__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

import logging
from os.path              import join, basename, dirname
from wrf4g.tools.vcplib   import VCPURL, copy
from wrf4g                import logger

def run( arg ):
    if arg[ '--dbg' ] :
        logger.setLevel(logging.DEBUG)
    try :
        if '*' in basename( arg[ '<source>' ] ) :
            for file_to_copy in VCPURL( dirname( arg[ '<source>' ] ).ls( basename( arg[ '<source>' ] ) ):
                copy( join(  dirname( arg[ '<source>' ] ), file_to_copy ), 
                      arg[ '<dest>' ], 
                      overwrite = arg[ '--overwrite' ], 
                      )
        else:
            copy( arg[ '<source>' ] , arg[ '<dest>' ], overwrite = arg[ '--overwrite' ] )        
    except Exception , err :
        logger.error( str( err ) )
