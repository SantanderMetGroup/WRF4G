import logging
import mock
import os
import shutil
import sys
import tempfile
import unittest
from wrf4g.wrapper import PilotParams, WRF4GWrapper
from unittest import mock
from mocked_functions import mock_rmtree, mock_exec_cmd

log = logging.getLogger()
log.setLevel("DEBUG")

testargs = [
    "../../bin/wrf_wrapper.py",
    "test_experiment",
    "test_realization",
    "1",
    "2001-01-01_00:00:00",
    "2001-01-07_00:00:00",
    "0"
]
os.environ["GW_HOSTNAME"] = "bender"
os.environ["GW_JOB_ID"] = "0"
os.environ["GW_RESTARTED"] = "0"
os.environ["GW_NP"] = "4"


def write_phony_rsl(tempdir):
    rsldir = tempdir + "/WRFV3/run"
    os.makedirs(rsldir)
    rslfile = rsldir + "/rsl.out.0000"
    with open(rslfile, "w") as rsl:
        rsl.write("SUCCESS COMPLETE\n")


@mock.patch.object(sys, 'argv', testargs)
def get_wrf4g_wrapper(tempdir):
    params = PilotParams(
        json="test_experiment/realization.json",
        root_path=tempdir
    )
    return WRF4GWrapper(params)


def create_tempdir(tempdir):
    log.info("Running test in {}".format(tempdir))
    bindir = tempdir + "/bin"
    os.mkdir(bindir)
    shutil.copy2("./test_experiment/namelist.input", tempdir)
    shutil.copy2("./test_experiment/wrf4g_files/bin/preprocessor.default",
                 bindir)
    shutil.copy2("./test_experiment/wrf4g_files/bin/postprocessor.SFC", bindir)
    write_phony_rsl(tempdir)


class TestWRF4GWrapper:
    #@mock.patch('wrf4g.wrapper.os.makedirs', side_effect=mock_makedirs)
    #@mock.patch('wrf4g.wrapper.os.chmod', side_effect=mock_chmod)
    #@mock.patch('wrf4g.wrapper.os_stat', side_effect=mock_stat)
    #@mock.patch('wrf4g.wrapper.os.chdir', side_effect=mock_chdir)

    #@mock.patch('wrf4g.wrapper.shutil.copyfile',
    #            side_effect=mock_shutil_copyfile)
    #@mock.patch('wrf4g.wrapper.copy_file', side_effect=mock_copy_file)
    #@mock.patch('wrf4g.wrapper.extract', side_effect=mock_extract_file)
    @mock.patch('wrf4g.wrapper.shutil.rmtree', side_effect=mock_rmtree)
    @mock.patch('wrf4g.wrapper.exec_cmd', side_effect=mock_exec_cmd)
    @mock.patch('wrf4g.wrapper.fix_ptop')
    @mock.patch('wrf4g.utils.namelist.get_latlon_dx')
    def test_launch(self, *args):
        tempdir = tempfile.mkdtemp(prefix="wrf4g_test_", dir="/tmp")
        create_tempdir(tempdir)
        wrf4g_wrapper = get_wrf4g_wrapper(tempdir)

        try:
            wrf4g_wrapper.launch()
        except SystemExit as sysexit:
            # This will always happen
            # Clean test folder
            shutil.rmtree(tempdir)


if __name__ == "__main__":
    test_wrf4g_wrapper = TestWRF4GWrapper()
    test_wrf4g_wrapper.test_launch()
