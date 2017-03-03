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
    SFTP            sftp://www.meteo.unican.es/work/WRF4G.tar.gz
"""

import logging, sys
from os.path              import join, basename, dirname
from wrf4g.utils.vcplib   import VCPURL, copy

def run( arg ):
    logging.basicConfig( format = '%(message)s',
                         level  = logging.DEBUG if arg[ '--dbg' ] else logging.INFO,
                         stream = sys.stdout )
    try :
        if '*' in basename( arg[ '<source>' ] ) and not 'sftp' in  arg[ '<source>' ] :
            for file_to_copy in VCPURL( dirname( arg[ '<source>' ] ) ).ls( basename( arg[ '<source>' ] ) ):
                copy( join(  dirname( arg[ '<source>' ] ), file_to_copy ), 
                      arg[ '<dest>' ], 
                      overwrite = arg[ '--overwrite' ] )
        else:
            copy( arg[ '<source>' ] , arg[ '<dest>' ], overwrite = arg[ '--overwrite' ] )       
    except KeyboardInterrupt :
        pass 
    except Exception as err :
        logging.error( str( err ) )
