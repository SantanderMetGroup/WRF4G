class MockStat(object):
    st_mode = 0


def mock_makedirs(idir):
    print("\nRunning os.makedirs({})".format(idir))


def mock_chmod(ifile, perm):
    print("\nRunning os.chmod({}, {})".format(ifile, perm))


def mock_stat(ifile):
    print("\nRunning os_stat({})".format(ifile))
    return MockStat()


def mock_chdir(idir):
    print("\nRunning os.chdir({})".format(idir))


def mock_rmtree(idir):
    print("\nRunning shutil.rmtree({})".format(idir))


def mock_copy_file(orig, dest):
    print("\nRunning copy_file with {} and {}".format(orig, dest))


def mock_shutil_copyfile(orig, dest):
    print("\nRunning shutil.copyfile with {} and {}".format(orig, dest))


def mock_extract_file(dest, to_path):
    print("\nExtracting file {} to {}".format(dest, to_path))


def mock_exec_cmd(app_cmd):
    print("\nExecuting command {}".format(app_cmd))
    if ">" in app_cmd:
        logfile = app_cmd.split(">")[1].strip()
        with open(logfile, "w") as lf:
            lf.write("Successful completion\n")
            lf.write("SUCCESS COMPLETE")
    code = 0
    output = "Successful completion"
    return code, output