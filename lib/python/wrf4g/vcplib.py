import os 
import datetime
import time
import logging
import commands

from re        import match, search
from sys       import stderr
from os.path   import abspath, isdir, isfile, basename, dirname, join

__version__  = '1.5.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id:$"

logger = logging.getLogger(__name__)

def http_ftp_protocol(protocol):
    if protocol == 'http' or protocol == 'https' or protocol == 'ftp':
        return True
    else :
        return False

class VCPURL(object):
    """
    Examples of usage of the VCPURL class:
    
    GSIFTP:
    >>> c=VCPURL("gsiftp://ce01.macc.unican.es:2812/tmp/example")
    >>> print c
    gsiftp://ce01.macc.unican.es:2812/tmp/example
    >>> c.rename("example1")
    >>> a=VCPURL("gsiftp://ce01.macc.unican.es:2812/tmp")
    >>> a.ls("exam*")
    ['example1']
    
    FILES:
    >>> d=VCPURL("./new_directory")
    >>> d.mkdir()
    >>> d.ls("*")
    ['']
    >>> f=VCPURL(".")
    >>> f.ls("new*")
    ['new_directory']
    
    RSYNC:
    >>> a=VCPURL("rsync://user@ce01.macc.unican.es/tmp/")
    >>> a.ls("expam*")
    ['example2']
    >>> a.rename("example2p")
    >>> a.ls("example*")
    ['example2p']
    """
    
    def __init__(self, url=None):
        """
        From a url (protocol://user@machine:port/file) it returns an array with 
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
            logger.error(out)
            raise Exception(out)
        else:
            # Split the url if the begging is protocol:
            g0 = match("([a-z]+):(.+)", url)
            # There's no protocol in the url
            if not g0 :
                self.protocol = "file"
                abs_url = abspath(url)
                if url == ".":
                    self.file = abs_url + "/"
                else:
                    self.file = abs_url
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
                        logger.error(out)
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
                            'ftp'   : {'ls'    : "" ,
                                       'mkdir' : "" ,
                                       'rm'    : "" , 
                                       'rename': "" , 
                                       'name'  : "self.file"},    
                            'rsync' : {'ls'    : "'ssh -q %s ls -1 %s'    %(self.usercomputer,self.file)",
                                       'mkdir' : "'ssh -q %s mkdir -p %s' %(self.usercomputer,self.file)", 
                                       'rm'    : "'ssh -q %s rm -rf %s'   %(self.usercomputer,self.file)", 
                                       'rename': "'ssh -q %s mv %s %s'    %(self.usercomputer,orig,dest)", 
                                       'name'  : "self.file"},
                            'gsiftp': {'ls'    : "'uberftp %s \"ls %s\" | awk \"NR>2\"'  %(self.computer,self.file)", 
                                       'mkdir' : "'uberftp %s \"mkdir %s\"'              %(self.computer,self.file)", 
                                       'rm'    : "'uberftp %s \"rm -r %s\"'              %(self.computer,self.file)", 
                                       'rename': "'uberftp %s \"rename %s %s\"'          %(self.computer,orig,dest)",
                                       'name'  : "self.file"},
                            'lfn'   : {'ls'    : "'lfc-ls %s'         %(self.file)",
                                       'mkdir' : "'lfc-mkdir -p %s'   %(self.file)",
                                       'rm'    : "'lcg-del -a lfn:%s' %(self.file)",
                                       'rename': "'lfc-rename %s %s'  %(orig,dest)",
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
            logger.error(out)
            raise Exception(out)
        
        command = eval(self.command[self.protocol]['ls'])
        if verbose:
            stderr.write(command + "\n")
        (err, out) = commands.getstatusoutput(command)
        if err != 0 :
            out = "Error reading dir: " + str(out)
            logger.error(out)
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
            logger.error(out)
            raise Exception(out)
        
        command = eval(self.command[self.protocol]['mkdir'])
        if verbose:
            stderr.write(command + "\n")
        (err, out) = commands.getstatusoutput(command)
        
        if err != 0 :
            out = "Error creating dir: " + str(out)
            logger.error(out)
            raise Exception(out)
    
    def rm(self, verbose=False):
        """
        Delete a file or folder
        """
        if http_ftp_protocol(self.protocol):
            raise Exception("This method is not available for " + self.protocol + " protocol")
        
        command = eval(self.command[self.protocol]['rm'])
        if verbose: 
            stderr.write(command + "\n")
        (err, out) = commands.getstatusoutput(command)
        
        if err != 0 : 
            out="Error deleting file: " + str(out)
            logger.error(out)
            raise Exception(out)
    
    def rename(self, newname, verbose=False):
        """
        Rename self.file into newname
        """
        if http_ftp_protocol(self.protocol):
            out="This method is not available for " + self.protocol + " protocol"
            logger.error(out)
            raise Exception(out)
        
        orig = eval(self.command[self.protocol]['name'])
        dest_folder = dirname(orig)
        dest = join(dest_folder,newname)
        command = eval(self.command[self.protocol]['rename'])
        if verbose: 
            stderr.write(command + "\n")
        (err, out) = commands.getstatusoutput(command)
        if err != 0 :
            out="Error listing file: " + str(out)
            logger.error(out)
            raise Exception(out)

    def isfile(self, verbose=False):
        """
        Return True if self.file is a file
        """
        if self.protocol == "ln" :
           return True
        else:
           out = self.ls(self.file)
           if len(out) != 0 and self.file == out[0]:
              return True
           else:
              return False
    
    def exists(self, verbose=False):
        """
        Return True if self.file exists
        """
        try:
            out = self.ls(self.file)
        except Exception :
            return False
        else:
            return True

def copy(origin, destination, overwrite=True, verbose=False):
    """
    Copy orig to dest.
    """
    if VCPURL(origin).isfile():
        copy_file(origin, destination, overwrite, verbose)
    else:
        copy_tree(origin, destination, overwrite, verbose)


def copy_tree(origin, destination, overwrite=True, verbose=False):
    """
    Recursively copy orig to dest.
    """
    orig=VCPURL(origin)
    dest=VCPURL(destination)
    if dest.exists():
        dest.mkdir()
    for name in orig.ls():
        if not name: break
        orig_name = join(orig.__str__(), name)
        dest_name = join(dest.__str__(), name)
        try:
            if VCPURL(orig_name).isfile():
                copy_file(orig_name, dest_name, overwrite, verbose)
            else:
                copy_tree(orig_name, dest_name, overwrite, verbose)
        except Exception, err:
            stderr.write(str(err) + "\n")
            logger.warning(str(err))

def copy_file(origin, destination, overwrite=True, verbose=False):
    """
    Copy orig to dest file.
    """
    vcp_matrix = {'file': {'file':   {'verbose'  : '-v', 
                                  'command'  : "'cp %(verbose)s %(orig)s %(dest)s' %param", 
                                  'orig'     : "orig.file", 
                                  'dest'     : "dest.file"},
                       'rsync':  {'verbose'  : '-v', 
                                  'command'  : "'rsync -au %(verbose)s %(orig)s %(dest)s' %param", 
                                  'orig'     : "orig.file", 
                                  'dest'     : "str(dest)"},
                       'ln':     {'verbose'  : '-v', 
                                  'command'  : "'ln -s %(verbose)s %(orig)s %(dest)s' %param", 
                                  'orig'     : "orig.file",
                                  'dest'     : "dest.file"},
                       'gsiftp': {'verbose'  : '-v', 
                                  'command'  : "'globus-url-copy %(verbose)s %(orig)s %(dest)s' %param", 
                                  'orig'     : "str(orig)", 
                                  'dest'     : "str(dest)"},
                       'lfn':    {'verbose'  : '-v',
                                  'command'  : "'lcg-cr %(verbose)s %(dest)s %(orig)s' %param",
                                  'orig'     : "str(orig)",
                                  'dest'     : "str(dest)"},
                       },
              'gsiftp':{'file':  {'verbose'  : '-v', 
                                  'command'  : "'globus-url-copy %(verbose)s %(orig)s %(dest)s' %param", 
                                  'orig'     : "str(orig)", 
                                  'dest'     : "str(dest)"},
                        },
              'rsync': {'file' : {'verbose'  : '-v', 
                                  'command'  : "'rsync -au %(verbose)s %(orig)s %(dest)s' %param", 
                                  'orig'     : "str(orig)", 
                                  'dest'     : "dest.file"},
                        },
              'https': {'file' : {'verbose'  : '-v', 
                                  'command'  : "'wget %(verbose)s %(orig)s %(dest)s' %param", 
                                  'orig'     : "str(orig)", 
                                  'dest'     : "dest.file"},
                        },
              'http': {'file' : {'verbose'   : '-v', 
                                  'command'  : "'wget %(verbose)s %(orig)s %(dest)s' %param", 
                                  'orig'     : "str(orig)", 
                                  'dest'     : "dest.file"},
                       },
              'ftp' : {'file' : {'verbose'   : '-v', 
                                  'command'  : "'wget %(verbose)s %(orig)s %(dest)s' %param", 
                                  'orig'     : "str(orig)", 
                                  'dest'     : "dest.file"},
                       },
              'lfn' : {'file' : {'verbose'   : '-v',
                                  'command'  : "'lcg-cp %(verbose)s lfn:%(orig)s %(dest)s' %param",
                                  'orig'     : "orig.file",
                                  'dest'     : "str(dest)"},
                       },
              }

    orig = VCPURL(origin)
    #For protocols like lfn the destination must be a file
    if destination[-1] == "/" :
        dest = VCPURL(join(destination, basename(orig.file)))
    elif  destination == ".":
        dest = VCPURL(basename(orig.file))
    else:
        dest = VCPURL(destination)
    out="Starting to copy ..."
    logger.debug(out)
    if verbose :
        stderr.write(out + "\n")
    if http_ftp_protocol(dest.protocol):
        out="Unable to copy if the destination protocol is " + dest.protocol
        logger.error(out)
        raise Exception(out)
    if http_ftp_protocol(orig.protocol) and dest.protocol != 'file':
        out="Unable to copy if the destination protocol is not file://"
        logger.error(out)
        raise Exception(out)
    if dest.protocol == 'ln' and orig.protocol != 'file':
        dest.protocol = 'file'

    #If "orig" and "dest" are on the same machine, VCP will copy using cp command
    if (dest.protocol == "rsync" or orig.protocol == "rsync") :
        if (isfile(orig.file) and (isdir(dest.file) or isdir(dest.file.rsplit('/',1)[0]))) or \
           (isdir(orig.file)  and isdir(dest.file)) or \
           ("*" in orig.file  and isdir(orig.file.rsplit('/',1)[0]) and isdir(dest.file)):
            orig.protocol = "file"
            dest.protocol = "file"
    out = "Copying from " + orig.__str__() + " to " + dest.__str__()
    logger.debug(out)   
    if verbose :
        stderr.write(out + "\n")        
    param = vcp_matrix[orig.protocol][dest.protocol]
    #orig customization 
    orig_file = eval(param['orig'])
    param['orig'] = orig_file
    #dest customization 
    if dest.protocol == "lfn" and dest.computer:
        dest_file = '-d ' + dest.computer + ' -l ' + dest.file
    else:
        dest_file = eval(param['dest'])
    param['dest'] = dest_file
    if not verbose: 
        param['verbose'] = ""
    #Overwrite the destination if necessary
    if overwrite:
        if dest.exists() and dest.isfile():
            out = "Overwriting " + dest.__str__()
            logger.debug(out)
            if verbose :
                stderr.write(out + "\n")
            dest.rm(verbose)
    start = time.time()
    command = eval(param['command'])
    out="Command to copy " + command
    logger.debug(out)
    if verbose :  
        stderr.write(out + "\n")
    (err, out) = commands.getstatusoutput(command)
    if err != 0 :
        raise Exception("Error copying file: " + out)
    elapsed = time.time()- start
    out = "The copy lasted %.2f seconds" %(elapsed)
    logger.debug(out)
    if verbose :    
        stderr.write(out + "\n")

