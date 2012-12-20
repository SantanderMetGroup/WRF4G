from re import match, search
from sys import stderr
from commands import getstatusoutput
from os.path import abspath, isdir, isfile, basename, dirname
import datetime
import time
import logging_wrf4g
try:
    logging_wrf4g.Logger(os.path.join(os.environ['WRF4G_LOCATION'], 'var','vcp','vcp.ini'))
except Exception, e:
    print 'Caught exception: %s: %s' % (e.__class__, str(e))
logger = logging_wrf4g.getLogger('vcp')

def http_ftp_protocol(protocol):
    if protocol == 'http' or protocol == 'https' or protocol == 'ftp':
        return True
    else :
        return False

class VCPURL:
    """
    Examples of usage of the VCPURL class:
    
    GSIFTP:
    
    >>> c=VCPURL("gsiftp://ce01.macc.unican.es:2812/tmp/prueba")
    >>> print c
    gsiftp://ce01.macc.unican.es:2812/tmp/prueba
    >>> c.rename("prueba1")
    >>> a=VCPURL("gsiftp://ce01.macc.unican.es:2812/tmp")
    >>> a.ls("prue*")
    ['prueba1']
    
    FILES:
    >>> d=VCPURL("./nuevo_directorio")
    >>> d.mkdir()
    >>> d.ls("*")
    ['']
    >>> f=VCPURL(".")
    >>> f.ls("nuevo*")
    ['nuevo_directorio']
    
    GSIFTP:
    >>> a=VCPURL("rsync://valva@ce01.macc.unican.es/tmp/")
    >>> a.ls("pepe*")
    ['pepe1p']
    >>> b=VCPURL("rsync://valva@ce01.macc.unican.es/tmp/pepe1p")
    >>> b.rename("juan1p")
    >>> a.ls("juan*")
    ['juan1p']
    """
    
    def __init__(self, url=None):
        """
        From a url (rsync://valva@sipc18:80/etc) it returns an array with 
        the following field [protocol,user,computer,port,file]
        """
        self.protocol     = ""
        self.user         = ""
        self.computer     = ""
        self.port         = ""
        self.file         = ""
        self.command      = ""
        self.usercomputer = ""
        
        if not url:
            out = "Not an url"
            logger.warning(out)
            raise Exception(out)
        else:
            # Split the url if the begging is protocol:
            g0 = match("([a-z]+):(.+)", url)
            # There's no protocol in the url
            if not g0 :
                self.protocol = "file"
                self.file = abspath(url)
            else :
                # The url contains a protocol
                (self.protocol, self.file) = g0.groups()
                # This is from URL with the form: gsiftp://ce01.macc.unican.es
                position = self.file.find('//')
                if position == 0 :
                    g1 = match("//([\w.@-]*):?(\d*)(/\S*)", self.file)
                    if g1 :
                        (self.usercomputer, self.port, self.file) = g1.groups()
                        self.computer = self.usercomputer
                        if self.protocol in ("ln", "file") :
                            self.file = abspath (self.file)
                        if self.computer.find("@") != -1 :
                            (self.user, self.computer) = self.computer.split("@")
                    else :
                        out="Url is not well formed"
                        logger.warning(out)
                        raise Exception(out)
                    
            self.command = {'file'  : {'ls'    : "'ls -1 %s'    %self.file" ,
                                       'mkdir' : "'mkdir -p %s' %self.file" ,
                                       'rm'    : "'rm -rf %s'   %self.file" , 
                                       'rename': "'mv %s %s'    %(orig,dest)", 
                                       'name'  : "self.file"},
                            'http'  : {'ls'    : "" ,
                                       'mkdir' : "" ,
                                       'rm'    : "" , 
                                       'rename': "" , 
                                       'name'  : "self.file"},
                            'https'  : {'ls'   : "" ,
                                       'mkdir' : "" ,
                                       'rm'    : "" , 
                                       'rename': "" , 
                                       'name'  : "self.file"},
                            'ftp'   : {'ls'   : "" ,
                                       'mkdir' : "" ,
                                       'rm'    : "" , 
                                       'rename': "" , 
                                       'name'  : "self.file"},    
                            'rsync' : {'ls'    : "'ssh -q %s ls -1 %s'    %(self.usercomputer, self.file)", 
                                       'mkdir' : "'ssh -q %s mkdir -p %s' %(self.usercomputer,self.file)", 
                                       'rm'    : "'ssh -q %s rm -rf %s'   %(self.usercomputer, self.file)" , 
                                       'rename': "'ssh -q %s mv %s %s'    %(self.usercomputer,orig,dest)", 
                                       'name'  : "self.file"} ,xrandr
                            'gsiftp': {'ls'    : "'uberftp %s \"ls %s\" | awk \"NR>2\"' %(self.computer, self.file)" , 
                                       'mkdir' : "'uberftp %s \"mkdir %s\"'             %(self.computer, self.file)" , 
                                       'rm'    : "'uberftp %s \"rm -r %s\"'             %(self.computer, self.file)"  , 
                                       'rename': "'uberftp %s \"rename %s %s\"'         %(self.computer,orig,dest)" ,
                                       'name'  : "self.file"}
                            }
            
    def __str__(self):
        """
        Returns an url from a array with the following fields 
        [protocol,user,computer,port,file]
        """
        # Fill the variables: protocol, user, computer, port and file 
        # user
        if self.user != "" : 
            user = self.user + "@"
        else : 
            user = ""
        
        # computer 
        computer = self.computer
        
        # port 
        if self.port != "" or self.protocol == "rsync" : 
            port = ":" + self.port
        else : 
            port = ""
            
        # file 
        file = self.file
        
        # protocol
        if self.protocol == "rsync" :
            protocol = ""
        # Transform local path to file://. If the file is a directory we add / at the end because
        # globus-url-copy works this way.
        elif self.protocol == "file":
            protocol = self.protocol + "://"
            file = abspath(file)
        else : protocol = self.protocol + "://"
        
        url = protocol + user + computer + port + file 
        return url
    
    def ls(self, file="*", verbose=False):
        """ 
        List all the files under the directory. If no arguments are given it list the whole directory. 
        It has the argument file that is the pattern we want to search inside this directory. Examples
            * a.ls()
            * a.ls("file*")
        """
        if http_ftp_protocol(self.protocol):
            out="This method is not available for " + self.protocol + " protocol"
            logger.warning(out)
            raise Exception(out)
        
        command = eval(self.command[self.protocol]['ls'])
        if verbose:
            stderr.write(command + "\n")
        (err, out) = getstatusoutput(command)
        if err != 0 :
            out = "Error creating dir: " + str(err)
            logger.warning(out)
            raise Exception(out)
            
        out_list = out.split("\n")
        if self.protocol == "gsiftp":
            for i, elem in enumerate(out_list):
                out_list[i] = elem.split()[-1]
        pattern = file.replace(".", "\.")
        pattern = pattern.replace("*", ".*")
        pattern = pattern + "$"
        
        try :
            out_list.remove(".")
            out_list.remove("..")
        except ValueError:
            pass
        
        file_list = []
        for file_name in out_list :
            if match(pattern, file_name) :
                file_list.append(file_name)
        file_list.sort()
        return file_list
    
    def mkdir(self, verbose=False):
        """
        Create the directory pointed by self
        """
        if http_ftp_protocol(self.protocol):
            out="This method is not available for " + self.protocol + " protocol"
            logger.warning(out)
            raise Exception(out)
        
        command = eval(self.command[self.protocol]['mkdir'])
        if verbose:
            stderr.write(command + "\n")
        (err, out) = getstatusoutput(command)
        
        if err != 0 :
            out = "Error creating dir: " + str(err)
            logger.warning(out)
            raise Exception(out)
        return 0
    
    def rm(self, verbose=False):
        """
        Delete a file or folder
        """
        if http_ftp_protocol(self.protocol):
            raise Exception("This method is not available for " + self.protocol + " protocol")
        
        command = eval(self.command[self.protocol]['rm'])
        if verbose: 
            stderr.write(command + "\n")
        (err, out) = getstatusoutput(command)
        
        if err != 0 : 
            out="Error deleting file: " + str(err)
            logger.warning(out)
            raise Exception(out)
        return 0
    
    def rename(self, newname, verbose=False):
        """
        Rename self into newname
        """
        if http_ftp_protocol(self.protocol):
            out="This method is not available for " + self.protocol + " protocol"
            logger.warning(out)
            raise Exception(out)
        
        orig = eval(self.command[self.protocol]['name'])
        dest_folder = dirname(orig)
        dest = dest_folder + "/" + newname
        command = eval(self.command[self.protocol]['rename'])
        if verbose: 
            stderr.write(command + "\n")
        (err, out) = getstatusoutput(command)
        if err != 0 :
            out="Error listing file: " + str(err)
            logger.warning(out)
            raise Exception(out)
        return 0
    

class wrffile :
    """
    This class manage the restart and output files and the dates they represent.
    It recieves a file name with one of the following shapes: wrfrst_d01_1991-01-01_12:00:00 or
    wrfrst_d01_19910101T120000Z and it return the date of the file, the name,...
    """
    
    def __init__(self, url, edate=None):
        """
        Change the name of the file in the repository (Change date to the iso format
        and add .nc at the end of the name
        """ 
        # wrfrst_d01_1991-01-01_12:00:00
        if edate:
            self.edate = datewrf2datetime(edate)
        
        g = search("(.*)(\d{4}-\d{2}-\d{2}_\d{2}:\d{2}:\d{2})", url)
        if g:
            base_file, date_file = g.groups() 
            self.date = datewrf2datetime(date_file)
        else :
            # wrfrst_d01_19910101T120000Z.nc
            g = search("(.*)(\d{8}T\d{6}Z)", url)
            if not g:
                out="File name is not well formed"
                logger.warning(out)
                raise Exception(out)
            else :
                base_file, date_file = g.groups()
                self.date = dateiso2datetime(date_file)
        self.file_name = basename(base_file)
        self.dir_name = dirname(base_file)
        
    def date_wrf(self):
        return datetime2datewrf(self.date)
    
    def date_iso(self):
        return datetime2dateiso(self.date)
    
    def file_name_wrf(self):
        return self.file_name + datetime2datewrf(self.date)
    
    def file_name_iso(self):
        return "%s%s.nc" % (self.file_name,datetime2dateiso(self.date))
    
    def file_name_out_iso(self):
        return "%s%s_%s.nc" % (self.file_name, datetime2dateiso(self.date), datetime2dateiso(self.edate))

def copy_file(origin, destination, verbose=False, recursive=False, streams=False):
    """
    Copies origin in destination. Both are arrays containing the following field 
    [protocol,user,computer,port,file]
    """
    if verbose :
        out="Starting to copy ..."
        logger.debug(out)
        stderr.write(out + "\n")
    orig = VCPURL(origin)
    dest = VCPURL(destination)
    if http_ftp_protocol(dest.protocol):
        out="Unable to copy if the destination protocol is " + dest.protocol
        logger.warning(out)
        raise Exception(out)
    if http_ftp_protocol(orig.protocol) and dest.protocol != 'file':
        out="Unable to copy if the destination protocol is not file://"
        logger.warning(out)
        raise Exception(out)
    if dest.protocol == 'ln' and orig.protocol != 'file':
        dest.protocol = 'file'
    if verbose :
        out = "Copying from " + orig.__str__() + " to " + dest.__str__()
        logger.debug(out)
        stderr.write(out + "\n")

    matrix = {'file': {'file':   {'verbose'  : '-v', 
                                  'recursive': '-R', 
                                  'command'  : "'cp %(verbose)s %(recursive)s %(orig)s %(dest)s' %param", 
                                  'orig'     : "orig.file", 
                                  'dest'     : "dest.file"},
                       'rsync':  {'verbose'  : '-v', 
                                  'recursive': '', 
                                  'command'  : "'rsync -au %(verbose)s %(recursive)s %(orig)s %(dest)s' %param", 
                                  'orig'     : "orig.file", 
                                  'dest'     : "dest"},
                       'ln':     {'verbose'  : '-v', 
                                  'recursive': '',
                                  'command'  : "'ln -s %(verbose)s %(recursive)s %(orig)s %(dest)s' %param", 
                                  'orig'     : "orig.file",
                                  'dest'     : "dest.file"},
                       'gsiftp': {'verbose'  : '-v', 
                                  'recursive': '-r -cd', 
                                  'command'  : "'globus-url-copy %(verbose)s %(recursive)s %(orig)s %(dest)s' %param", 
                                  'orig'     : "str(orig)", 
                                  'dest'     : "str(dest)"},
                       },
              'gsiftp':{'file':  {'verbose'  : '-v', 
                                  'recursive': '-r -cd', 
                                  'command'  : "'globus-url-copy %(verbose)s %(recursive)s %(orig)s %(dest)s' %param", 
                                  'orig'     : "str(orig)", 
                                  'dest'     : "str(dest)"},
                        },
              'rsync': {'file' : {'verbose'  : '-v', 
                                  'recursive': '', 
                                  'command'  : "'rsync -au %(verbose)s %(recursive)s %(orig)s %(dest)s' %param", 
                                  'orig'     : "orig", 
                                  'dest'     : "dest.file"},
                        },
              'https': {'file' : {'verbose'  : '-v', 
                                  'recursive': '-r', 
                                  'command'  : "'wget %(verbose)s %(recursive)s %(orig)s %(dest)s' %param", 
                                  'orig'     : "orig", 
                                  'dest'     : "dest.file"},
                        },
              'http': {'file' : {'verbose'   : '-v', 
                                  'recursive': '-r', 
                                  'command'  : "'wget %(verbose)s %(recursive)s %(orig)s %(dest)s' %param", 
                                  'orig'     : "orig", 
                                  'dest'     : "dest.file"},
                       },
              'ftp' : {'file' : {'verbose'   : '-v', 
                                  'recursive': '-r', 
                                  'command'  : "'wget %(verbose)s %(recursive)s %(orig)s %(dest)s' %param", 
                                  'orig'     : "orig", 
                                  'dest'     : "dest.file"},
                       },
              }

    #If "orig" and "dest" are on the same machine, VCP will copy using cp command
    if (dest.protocol == "rsync" or orig.protocol == "rsync") :
        if (isfile(orig.file) and (isdir(dest.file) or isdir(dest.rsplit('/',1)[0]))) or \
           (isdir(orig.file)  and isdir(dest.file)) or \
           (orig.file[-1] == "*"  and isdir(orig.file[:-1]) and isdir(dest.file)):
            orig.protocol = "file"
            dest.protocol = "file"

    param = matrix[orig.protocol][dest.protocol]
    orig_file = eval(param['orig'])
    dest_file = eval(param['dest'])
    
    #In order to copy using cp command "orig" and "dest" have to be "orig.file" and "dest.file"
    if (dest.protocol == "file" or orig.protocol == "file") :
        param['orig'] = orig.file
        param['dest'] = dest.file

    # FILE --> GRIDFTP & GRIDFTP -->FILE
    # the globus-url-copy has a strange behavior when copy directories. To indicate that
    # a file is a directory it's necesary that the end of the name is a /.
    if (dest.protocol == "gsiftp" or orig.protocol == "gsiftp") :
        if recursive:
            orig_file = orig_file + "/"
            dest_file = dest_file + "/"
        else:
            if origin.find("*") != -1 :
                dest_file = dest_file + "/"
            elif dest.protocol == "file" and isdir(dest.file) :
                dest_file = dest_file + "/" + basename(orig_file)
    
    # Convert origin, destination and command to the appropiate one.
    param['orig'] = orig_file
    param['dest'] = dest_file
    if not recursive : 
        param['recursive'] = ""
    if not verbose: 
        param['verbose'] = ""
    if verbose:
        start = time.time()
    command = eval(param['command'])
    if verbose :
        out="Command to copy " + command
        logger.debug(out)
        stderr.write(out + "\n")
    (err, out) = getstatusoutput(command)
    if err != 0 :
        raise Exception("Error copying file: " + str(err))
    if verbose :
        elapsed = str((time.time()- start)/60)
        out = "The copy lasted " + elapsed + " minutes"
        logger.debug(out)
        stderr.write(out + "\n")
    return 0

#    FUNCTIONS FOR MANAGE DATES      #
def datewrf2datetime (datewrf):
    g = match("(\d{4})-(\d{2})-(\d{2})_(\d{2}):(\d{2}):(\d{2})", datewrf)
    if not g :
        raise Exception("Date is not well formed")
    date_tuple = g.groups()
    date_object = datetime.datetime(*tuple(map(int, date_tuple)))
    return date_object

def dateiso2datetime (dateiso):
    g = match("(\d{4})(\d{2})(\d{2})T(\d{2})(\d{2})(\d{2})Z", dateiso)
    if not g :
        raise Exception("Date is not well formed")
    date_tuple = g.groups() 
    date_object = datetime.datetime(*tuple(map(int, date_tuple)))
    return date_object

def datetime2datewrf (date_object):
    return date_object.strftime("%Y-%m-%d_%H:%M:%S")

def datetime2dateiso (date_object):
    return date_object.strftime("%Y%m%dT%H%M%SZ")

