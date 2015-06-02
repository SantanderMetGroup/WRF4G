#!/usr/bin/env python

__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"


import os
import sys
from os.path       import abspath, dirname, join
from wrf4g.config  import load_exp_pkl

if __name__ == "__main__":
    # Which resource is it ?
    resource_name   = os.environ.get('GW_HOSTNAME')
    exp_conf        = load_exp_pkl( dirname( dirname( abspath( sys.argv[0] ) ) ) )  
    # Find if there is a specific section for this resource
    if exp_conf.has_key( resource_name ) :
        resource_exp_conf = exp_conf[ resource_name ]
    else :
        resource_exp_conf = exp_conf[ 'default' ]
    # Create the source file
    source_file = open( 'easy_source.conf', 'w' )
    source_file.write( resource_exp_conf[ 'app_source_script' ] )
    source_file.close()
