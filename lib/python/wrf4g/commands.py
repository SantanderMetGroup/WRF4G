import cmdln
import os
import getpass
import drm4g.core.configure
from drm4g import REMOTE_VOS_DIR , PROXY_THRESHOLD 

__version__  = '2.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id:$"

http://code.google.com/p/cmdln/wiki/GettingStarted

class ManagementUtility( cmdln.Cmdln ):
    """
    Encapsulates the logic of the drm4g.py utilities.
    """
    prompt = "> "

    def do_check_proxy(self, line):
        """
        Check if the Grid proxy certificate is valid. You have to indicate the resource.
        
        Example : check_proxy localhost
        """
        if not line :
            print "\tPlease, provide a resource"
        else :
            self.do_check_resources( line )
            if not self.config.resources.has_key( line ) :
                print "\t'%s' is not a resource. The resources available are: " % line
            else :
                resource  = self.config.resources[ line ]
                communicator = self.config.make_communicators()[ line ]
                communicator.connect()
                if resource.has_key( 'myproxy_server' ) :
                    cmd = "MYPROXY_SERVER=%s myproxy-info" % resource[ 'myproxy_server' ] 
                else :
                    cmd = "myproxy-info"
                print "\tExecuting command ... ", cmd 
                out, err = communicator.execCommand( cmd )
                print "\t", out , err
    
    def do_upload_proxy(self, line ):
        """
        It uploads the credential to a myproxy-server. You have to indicate the resource.
        
        Example : myproxy_upload localhost
        """
        if not line :
            print "\tPlease, provide a resource"
        else :
            self.do_check_resources( line )
            if not self.config.resources.has_key( line ) :
                print "\t'%s' is not a resource. The resources available are: " % line
            else :
                resource  = self.config.resources[ line ]    
                communicator = self.config.make_communicators()[ line ]
                communicator.connect()
                print "\tCreating '%s' directory to store the proxy ... " % REMOTE_VOS_DIR
                cmd = "mkdir -p %s" % REMOTE_VOS_DIR
                print "\tExecuting command ... ", cmd 
                communicator.execCommand( cmd )
                out, err = communicator.execCommand( cmd )
                if not err :
                    message      = '\tInsert your GRID pass: '
                    grid_passwd  = getpass.getpass(message)
        
                    message      = '\tInsert MyProxy password: '
                    proxy_passwd = getpass.getpass(message)
        
                    if resource.has_key( 'myproxy_server' ) :
                        cmd = "MYPROXY_SERVER=%s myproxy-init -S -t %s" % (
                                                                           resource[ 'myproxy_server' ] ,
                                                                           PROXY_THRESHOLD 
                                                                           )
                    else :
                        cmd = "myproxy-init -S -t %s" % PROXY_THRESHOLD
                    print "\tExecuting command ... ", cmd 
                    out , err = communicator.execCommand( cmd , input = '\n'.join( [ grid_passwd, proxy_passwd, proxy_passwd ] ) )
                    print "\t", out , err
                else :
                    print "\t", err

    def do_download_proxy(self, line ):
        """
        It  retrieves  a  proxy  credential  from  the myproxy-server. You have to indicate the resource.
        
        Example : myproxy_download localhost
        """
        if not line :
            print "\tPlease, provide a resource"
        else :
            self.do_check_resources( line )
            if not self.config.resources.has_key( line ) :
                print "\t'%s' is not a resource. The resources available are: " % line
                self.do_list_resources( line )
            else :
                resource  = self.config.resources[ line ]   
                communicator = self.config.make_communicators()[ line ]
                communicator.connect()
                
                message      = '\tInsert MyProxy password: '
                proxy_passwd = getpass.getpass(message)
         
                if resource.has_key( 'myproxy_server' ) :
                    cmd = "X509_USER_PROXY=%s/proxy MYPROXY_SERVER=%s myproxy-logon -S" % (
                                                                                           REMOTE_VOS_DIR ,
                                                                                           resource[ 'myproxy_server' ]
                                                                                           ) 
                else :
                    cmd = "X509_USER_PROXY=%s/proxy myproxy-logon -S" % ( REMOTE_VOS_DIR )
                print "\tExecuting command ... ", cmd 
                out, err = communicator.execCommand( cmd , input = proxy_passwd )
                print "\t", out , err
        
    def do_create_proxy(self, line ):
        """
        It creates a proxy for 7 days. You only have to indicate the resource.
        
        Example : create_proxy localhost
        """
        self.do_upload_proxy( line )
        self.do_download_proxy( line )
        
    def do_destroy_proxy(self, line ):
        """
        It destroys the proxy on the myproxy-server. You have to indicate the resource.
        
        Example : destroy_proxy localhost
        """
        if not line :
            print "\tPlease, provide a resource"
        else :
            self.do_check_resources( line )
            if not self.config.resources.has_key( line ) :
                print "\t'%s' is not a resource. The resources available are: " % line
            else :
                resource     = self.config.resources[ line ]
                communicator = self.config.make_communicators()[ line ]
                communicator.connect()
                if resource.has_key( 'myproxy_server' ) :
                    cmd = "MYPROXY_SERVER=%s myproxy-destroy" %  resource[ 'myproxy_server' ]                          
                else :
                    cmd = "myproxy-destroy" % REMOTE_VOS_DIR 
                print "\tExecuting command ... ", cmd
                out , err = communicator.execCommand( cmd )
                print "\t", out , err

    def do_check_frontends(self , line ):
        """
        Check if all frontends are reachable.
        """
        self.do_check_resources( line )
        communicators = self.config.make_communicators()
        for resname, communicator in sorted( communicators.iteritems() ) :
            print "\t--> Resource '%s' ... " % resname
            try :
                communicator.connect()
            except Exception , err :
                print "\t\tThe front-end %s is not reachable" % communicator.frontend 
                print "\t\t" , err
            print "\t\tThe front-end %s is reachable" % communicator.frontend

    def do_list_resources(self, line):
        """
        Check if the drm4g.conf file has been configured well and list the resources available.
        """
        self.do_check_resources( line )
        for resname, resdict in sorted( self.config.resources.iteritems() ) :
            print "\t--> Resource '%s' ... " % resname
            for key , val in sorted( resdict.iteritems() ) :
                print "\t\t--> '%s' : '%s' " % ( key , val )

    def do_check_resources(self, line):
        """
        Check if the drm4g.conf file has been configured well.
        """
        self.config.load()
        errors = self.config.check()
        if errors :
            print "\tPlease, review your configuration file"
            for error in errors :
                print "\t" , error
        else :
            print "\tThe resources configured passed the check with flying colours"
    
    def do_exit (self , line ):
        """
        Quits the console.
        """
        return True

    do_EOF = do_exit

def execute_from_command_line( argv ):
    """
    A method that runs a ManagementUtility.
    """
    if len( argv ) > 1:
        ManagementUtility().onecmd( ' '.join( argv[ 1: ] ) )
    else:
        ManagementUtility().cmdloop()
    

