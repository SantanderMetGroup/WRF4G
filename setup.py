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

__version__ = '2.3.0'
__author__ = 'Jesus Fernandez and Carlos Blanco'
__revision__ = "$Id$"

from setuptools import setup
from setuptools import find_packages
from setuptools.command.install import install
from os import path
import sysconfig
import tarfile
import glob
import sys
import ast
import os


try:
    from urllib.request import urlopen  # Python 3 - not verified
except ImportError:
    from urllib2 import urlopen  # Python 2


class WRF4GSetup(object):
    def __init__(self):
        self.python_version = "{0}.{1}".format(*sys.version_info)
        self.sourcedir = path.abspath(path.dirname(__file__))
        self.moduledir = self.sourcedir + "/wrf4g"
        self.configdir = self.moduledir + "/etc"
        self.prefix_dir = None
        self.has_prefix = False
        self.lib_dir = None
        self.bin_dir = None

        arguments = str(sys.argv)
        # convert from string to list
        self.arguments = ast.literal_eval(arguments)

    def get_conf_files(self):
        file_list = []
        print("Adding configuration files")
        for dirpath, dirnames, filenames in os.walk(self.configdir):
            if filenames:
                filenames_abs = [os.path.join(dirpath, f) for f in filenames]
                print(filenames_abs)
                file_list.extend(filenames_abs)
        return file_list

    def run_preinstallation(self):
        self.prefix_option()
        self.extract_repository()

    def prefix_option(self):
        # Going through the whole list since the options can be defined in
        # different ways (--prefix=dir> or --prefix <dir>) Which is why I'm not
        # using self.arguments.index('--prefix') to find it, since it doesn't
        # check if it's a substring. Could also do it with a while and make it
        # stop if it finds '--prefix' or '--home'
        for i in range(len(self.arguments)):
            option = self.arguments[i]
            # folder name can't contain '--prefix' or '--home'
            if '--prefix' in option or '--home' in option:
                # I'm working under the impression that the path passed on to
                # prefix has to be an absolute path for the moment, if you use
                # a relative path, gridway's binary files will be copied to a
                # directory relative to where ./gridway-5.8 is
                self.has_prefix = True
                if '=' in option:
                    self.prefix_dir = option[option.find('=')+1:]
                elif "--user" in option:
                    self.prefix_dir = os.environ["HOME"] + ".local"
                else:
                    self.prefix_dir = self.arguments[i+1]

                if '--prefix' in option:
                    self.lib_dir = os.path.join(
                        self.prefix_dir,
                        'lib/python{}/site-packages'.format(self.python_version)
                    )
                elif '--home' in option:
                    self.lib_dir = os.path.join(self.prefix_dir, 'lib/python')


                try:
                    os.makedirs(self.lib_dir)
                except OSError:
                    print('\nDirectory {} already exists'.format(self.lib_dir))
        # Use sysconfig package to get the default install lib path for this
        # python installation
        if not self.has_prefix:
            self.lib_dir = sysconfig.get_path("platlib")
            self.prefix_dir = sysconfig.get_path("data")

        self.bin_dir = os.path.join(self.prefix_dir, 'bin')

    def download_repository(self):
        tar_file_local = "repository.tar.gz"
        if os.path.exists(tar_file_local):
            return None
        else:
            response = urlopen(
                'https://meteo.unican.es/work/repository.tar.gz')
            tar_file = response.read()
            # the disadvantage of this method is that the entire file is loaded
            # into ram before being saved to disk
            with open(tar_file_local, 'wb') as output:
                output.write(tar_file)

    def repository_files(self, members):
        for tarinfo in members:
            if "repository" in tarinfo.name:
                yield tarinfo

    def extract_repository(self):
        self.download_repository()
        with tarfile.open('repository.tar.gz', 'r') as tar:
            repository_path = self.prefix_dir
            tar.extractall(path=repository_path,
                           members=self.repository_files(tar))

    def print_final_message(self):
        print("WRF4G binary files installed in {}".format(self.bin_dir))
        print("WRF4G library files installed in {}".format(self.lib_dir))

        if self.prefix_dir is None:
            print(
                '''
                To finish with the installation, you have to add the 
                following paths to your $PYTHONPATH and $PATH:
                export PYTHONPATH={}:$PYTHONPATH
                export PATH={}:$PATH
                '''.format(self.lib_dir, self.bin_dir)
            )


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


def yes_no_choice(message,  default='y'):
    """
    Ask for Yes/No questions
    """
    choices = 'y/n' if default.lower() in ('y', 'yes') else 'y/N'
    values = ('y', 'yes', 'n', 'no')
    choice = ''
    while not choice.strip().lower() in values:
        choice = input("{} ({}) \n".format(message, choices))
    return choice.strip().lower()


wrf4g_setup = WRF4GSetup()


class BuildWrapper(install):
    def run(self):
        wrf4g_setup.run_preinstallation()
        install.run(self)


bin_scripts = glob.glob(os.path.join('bin', '*'))
bin_scripts.append('LICENSE')

setup(
    name='wrf4g',
    packages=find_packages(),
    package_data={'wrf4g': wrf4g_setup.get_conf_files()},
    version='2.3.0',
    author='Meteorology Group UC',
    author_email='josecarlos.blanco@unican.es',
    url='https://meteo.unican.es/trac/wiki/WRF4G2.0',
    license='European Union Public License 1.1',
    description='WRF for Grid (WRF4G) is a framework for the execution and '
                'monitoring of the WRF Modelling System.',
    long_description=get_long_description(),
    python_requires='>=2.7,!=3.0.*,!=3.1.*',
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
    install_requires=['sqlalchemy', 'six', 'requests', 'drm4g', 'docopt',
                      'python-dateutil', 'MySQL-python'],
    scripts=bin_scripts,
    cmdclass={
        'install': BuildWrapper,
    },
)

wrf4g_setup.print_final_message()
