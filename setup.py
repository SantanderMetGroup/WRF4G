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

__version__  = '2.3.1'
__author__   = 'Valvanuz Fernández, Jesus Fernandez, Carlos Blanco and Antonio S. Cofiño'
__revision__ = "$Id$"

from setuptools import setup
from setuptools import find_packages
from setuptools.command.install import install
import glob
import sys
import os

#To ensure a script runs with a minimal version requirement of the Python interpreter
#assert sys.version_info >= (2,5)
if (sys.version_info[0]==2 and sys.version_info<=(2,5)) or (sys.version_info[0]==3 and sys.version_info<(3,3)):
    exit( 'The version number of Python has to be >= 2.6 or >= 3.3' )

# Python 2/3 compatibility
try: 
    input = raw_input
except NameError:
    pass

# Get every file under search_dir, including subdirectories. Needs to be improved
def get_conf_files(search_dir='data'):
    if os.path.exists('./wrf4g'):
        os.chdir('./wrf4g')
    directory_list = glob.glob(search_dir)
    search_dir += '/*'
    file_list = []
    if directory_list:
        for f in directory_list:
            if os.path.isfile(f):
                file_list.append(f)
        file_list += get_conf_files(search_dir)
    if not os.path.exists('./wrf4g'):
        os.chdir('..')
    return file_list

def get_long_description():
    readme_file = 'README'
    if not os.path.isfile(readme_file):
        return ''
    # Try to transform the README from Markdown to reStructuredText.
    try:
        import pandoc
        pandoc.core.PANDOC_PATH = 'pandoc'
        doc = pandoc.Document()
        doc.markdown = open(readme_file).read()
        description = doc.rst
    except Exception:
        description = open(readme_file).read()
    return description

bin_scripts= glob.glob(os.path.join('bin', '*'))

setup(
    name='wrf4g',
    packages=find_packages(),
    package_data={'wrf4g': get_conf_files()},
    version='__version__',
    author='Meteorology Group UC',
    author_email='meteo@unican.es',
    url='https://meteo.unican.es/trac/wiki/WRF4G2.0',
    license='European Union Public License 1.1',
    description='WRF for Grid (WRF4G) is a framework for the execution and monitoring of the WRF Modelling System.',
    long_description = get_long_description(),
    classifiers=[
        "Intended Audience :: Science/Research",
        "Programming Language :: Python",
        "Topic :: Scientific/Engineering",
        "Topic :: Office/Business :: Scheduling",
        "Programming Language :: Python :: 2.6",
        "Programming Language :: Python :: 2.7",
        "Programming Language :: Python :: 3.3",
        "Programming Language :: Python :: 3.4",
        "Programming Language :: Python :: 3.5",
        "Programming Language :: Python :: 3.6",
    ],
    install_requires=['drm4g', 'sqlalchemy', 'six', 'python-dateutil','requests'],
    scripts=bin_scripts,
)
