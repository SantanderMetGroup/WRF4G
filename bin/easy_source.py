#!/usr/bin/env python

__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"


import os
import sys
from os.path     import abspath, dirname, join
from wrf4g.utils import VarEnv

if __name__ == "__main__":
    # Which resource is it ?
    resource_name   = os.environ.get('GW_HOSTNAME')
    experiment_file = join ( dirname( abspath( sys.argv[0] ) ), 'experiment.wrf4g' )
    experiment_conf = VarEnv( experiment_file )
    source_lines    = experiment_conf.get_variable( 'app_source_script' )
    # Find if there is a specific section for this resource
    for section in experiment_conf.sections() :
        if ':' in section and section.split( ':' , 1 )[ 1 ].strip() == resource_name :
            source_lines = experiment_conf.get_variable( 'app_source_script' , section )
    # Create the source file
    source_file = open( 'easy_source.conf', 'w' )
    source_file.write( source_lines )
    source_file.close()
