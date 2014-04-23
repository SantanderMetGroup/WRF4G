from __future__     import with_statement
import sys
import os
import subprocess
import cmdln
import errno
import shutil

from os.path     import exists , expandvars , join , abspath , dirname , expanduser , basename , isdir 
from wrf4g.core  import Experiment , Realization , JobStatus , Environment , Chunk , Job , FrameWork , Proxy , Resource
from wrf4g.utils import VarEnv , validate_name , pairs2dict
from wrf4g       import WRF4G_LOCATION , WRF4G_DEPLOYMENT_LOCATION , DB4G_CONF , GW_LOCATION , GW_BIN_LOCATION , GW_LIB_LOCATION , MYSQL_LOCATION
from wrf4g       import vcplib , vdblib

try :
    sys.path.insert( 0 , GW_LIB_LOCATION  )
    from drm4g.core.configure import Configuration
    from drm4g                import REMOTE_VOS_DIR , PROXY_THRESHOLD
except :
    pass 

__version__  = '1.5.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"
            
class ManagementUtility( cmdln.Cmdln ) :
    """Usage:
        wrf4g SUBCOMMAND [ARGS...]
        wrf4g help SUBCOMMAND

    ${command_list}
    ${help_list}
    WRF4G is a framework for managing WRF experiments.
    For additional information, see http://www.meteo.unican.es/software/wrf4g
    """
    
    prompt = "> "
    name   = "wrf4g"
    try :
        config = Configuration()
    except :
        config = None
    
    #Configure WRF4G_DB
    try:
        db4g_vars = VarEnv( DB4G_CONF )
        database_section =  'Database'
        dbc = vdblib.vdb( 
                         host   = db4g_vars.get_variable( 'WRF4G_DB_HOST' , database_section ) ,
                         user   = db4g_vars.get_variable( 'WRF4G_DB_USER' , database_section) ,
                         db     = db4g_vars.get_variable( 'WRF4G_DB_DATABASE' , database_section ) ,
                         port   = int ( db4g_vars.get_variable( 'WRF4G_DB_PORT' , database_section ) ) ,
                         passwd = db4g_vars.get_variable( 'WRF4G_DB_PASSWD' , database_section )
                         )
    except Exception, err:
        sys.stderr.write( '%s\n' % str( err ) )
    

    def __init__(self , *args , **kwargs ):
        
        cmdln.Cmdln.__init__( self , *args , **kwargs )
        cmdln.Cmdln.do_help.aliases.append( "h" )

    @cmdln.option("-n", "--dry-run",action="store_true", default=False, dest="dryrun",
                  help="Perform a trial run with no changes made")
    @cmdln.option( "-e", "--exp",metavar="name",dest="exp_name",
                  help="Experiment name to sumbit")
    @cmdln.option("-r", "--rea",metavar="name",dest="rea_name", 
                  help="Realization name to sumbit")
    @cmdln.option("-F", "--frea",metavar="FILE",dest="rea_file", 
                  help="File containing the realizations to submit.")
    @cmdln.option("-a", "--rerun",action="store_true",default=False, dest="rerun",
                  help="Force to run although this realization or experiment has finished")
    @cmdln.option("-c", "--chunk",metavar="N",dest="repeatchunk",type="int",default=0,
                  help="Number of the chunk to rerun. This option is only valid if the --rerun option is specified.")
    @cmdln.option("-o", "--run-just-one",action="store_true",default=False, dest="runone", 
                  help="Run just the first chunk of the first realization. Only for testing purposes.")
    @cmdln.option("-C", "--nchunk",metavar="N",dest="nchunk",type="int",default=0, 
                  help="Run the next N chunks not finished of each realization")
    @cmdln.option("-R", "--nrea",metavar="N",dest="nrea", type="int", default=0,
                  help="Run the next N realizations not finished of the experiment")
    @cmdln.option("-p", "--priority",metavar="P",dest="priority", default=0,
                  help="P is the priority the experiment or realization is going to be launched with (P is a integer between 0 and 19. 20 is an urgent job)")
    @cmdln.option("-v", "--verbose",action="store_true",default=False, dest="verbose",
                  help="Verbose mode. Explain what is being done")
    @cmdln.option("-f", "--force",action="store_true",default=False, dest="force",
                  help="Don't ask the user if he wants to submit an experiment already submitted")
    @cmdln.option("-d", "--tdep", action="store", type="choice", dest="type_dep", choices=["afterok", "afternotok", "afterany"], default="afterok",
                  help="Specify dependencies between Chunks. afterok: The chunk may be scheduled for execution only after jobs jobid have terminated with no errors.  afternotok: The chunk may be scheduled for execution only after jobs jobid have terminated with errors. afterany: The chunk may be scheduled for execution after jobs jobid have terminated, with or without errors.")
    
    def do_submit(self, subcmd, opts, *args):
        """Submit command submits an experiment or realization.

        usage:
            wrf4g submit [options]
        
        ${cmd_option_list}
        """
        try:
            self.dbc.connect()
            if not (opts.exp_name or opts.rea_name or opts.rea_file):
                # No experiment or realization given
                if not os.path.isfile('experiment.wrf4g'):
                    raise Exception( "Please use '--help' option " )
            if opts.rerun and not opts.force:
                raise Exception("""
                    BE CAREFULL! 
                    --rerun: Force to run although this realization or experiment has finished.
     
                    If you really want to run this command use the --force option
                """)
            if  opts.repeatchunk and  not opts.rea_name:
                raise Exception("--chunk option is available specifying a realization")
            if opts.runone:
                nrea=1
                nchunk=1    
            if opts.exp_name :
                exp=Experiment(data={'name': opts.exp_name},
                                        verbose=opts.verbose,
                                        dryrun=opts.dryrun,
                                        dbc=self.dbc)
                id=exp.get_id_from_name()
                if int(id) < 0:
                    raise Exception( "Experiment '%s' does not exists " % opts.exp_name )
                exp.run(nrea=opts.nrea,
                        nchunk=opts.nchunk,
                        priority=opts.priority,
                        rerun=opts.rerun,
                        force=opts.force,
                        type_dep=opts.type_dep)
            elif opts.rea_name:
                rea=Realization(data={'name':'%s'%opts.rea_name},
                                         verbose=opts.verbose,
                                         dryrun=opts.dryrun,
                                         dbc = self.dbc)
                id=rea.get_id_from_name()   
                if int(id) < 0:
                    raise Exception( "Realization '%s' does not exists" % opts.rea_name )
                if opts.repeatchunk != 0 :
                    if opts.rerun is not None :
                        rea.run(priority=opts.priority,
                                rerun=opts.rerun,
                                force=opts.force,
                                repeatchunk=opts.repeatchunk,
                                type_dep=opts.type_dep)
                    else:
                        raise Exception( "To rerun a chunk the --chunk option has to be used together with --rerun" )
                else:
                    rea.run(nchunk=opts.nchunk,
                            priority=opts.priority,
                            rerun=opts.rerun,
                            force=opts.force,
                            type_dep=opts.type_dep)
            elif opts.rea_file:
                try:
                    f=open(opts.rea_file)
                    for realization in f.readlines():
                        realization=realization.rstrip()
                        rea=Realization(data={'name': realization},verbose=opts.verbose,dryrun=opts.dryrun,dbc = self.dbc)
                        id=rea.get_id_from_name()
                        if id < 0 :
                            raise Exception( "Realization with name '%s' does not exist" % realization )
                        else:
                            rea.run(nchunk=opts.nchunk,
                                    priority=opts.priority,
                                    rerun=opts.rerun,
                                    force=opts.force,
                                    type_dep=opts.type_dep)      
                finally:     
                    f.close()
            else:
                raise Exception( "Use '--help' option " )
        except Exception, err :
            sys.stderr.write( str( err ) + '\n' )      
        finally:
            if self.dbc.is_open() :
                if opts.dryrun:
                    self.dbc.rollback()
                else:
                    self.dbc.commit()
                self.dbc.close()
    
    @cmdln.option("-e", "--exp",metavar="name",dest="exp_name", 
                  help="Experiment Name")
    @cmdln.option("-r", "--rea",metavar="name",dest="rea_name",
                  help="Realization Name")
    @cmdln.option("-l", "--long",action="store_true",default=False,dest="long", 
                  help="Show a detailed status")
    @cmdln.option("-n", "--ncharacters",default=20,type="int",dest="number_of_characters",
                  help="Print n characters of the Experiment Name or realization (default value is 20 characters)")
    
    def do_status(self, subcmd, opts, *args):
        """Status command prints the experiment or realization status.
        
        usage:
            wrf4g status [options]
        
        ${cmd_option_list}
        """
        if sys.stdout.isatty():
            bold  = "\033[1m"
            reset = "\033[0;0m"
        else:
            bold  = ""
            reset = ""
        
        def _sum_header( num_char ) :
            string = bold + '%-'+ str( num_char ) + 's %-3s %-3s %-3s %-3s %-3s' + reset
            print string % ('Experiment' , 'P' , 'W' , 'R' , 'D' , 'F' )

        def _long_header( num_char ):
            string = bold + '%-' + str( num_char ) + 's %-3s %-4s %-15s %-10s %-10s %-13s %2s %3s' + reset
            print string % ('Realization','GW','Stat','Chunks','Comp.Res','WN','Run.Sta','ext','%')        
        
        try:
            self.dbc.connect()
            if opts.long is False and opts.rea_name is None :
                _sum_header( opts.number_of_characters )
            else :
                _long_header( opts.number_of_characters )
            if opts.exp_name:
                experiment_name=opts.exp_name
                exp=Experiment(data={'name': experiment_name}, dbc = self.dbc)
                exp.data['id']=exp.get_id_from_name()
                if exp.data['id']<0:
                    raise Exception( "Experiment '%s' does not exists\n" % experiment_name )
                elif opts.long:
                    exp.ps(opts.number_of_characters)
                else:
                    exp.summarized_status(opts.number_of_characters)
            elif opts.rea_name:
                for rea_name in opts.rea_name.split( ',' ) :
                    Realization( data={ 'name' : rea_name.strip() } , dbc = self.dbc ).ps(opts.number_of_characters)
            else:
                list_exp = Environment( self.dbc ).list_experiments()
                if opts.long:
                    for id_exp in list_exp :
                        Experiment( data = { 'id' : id_exp } , dbc = self.dbc ).ps( opts.number_of_characters )
                else:
                    for id_exp in list_exp :
                        Experiment( data = { 'id' : id_exp } , dbc = self.dbc ).summarized_status( opts.number_of_characters )
        except Exception, err :
            sys.stderr.write( str( err ) + '\n' )
        finally:
            if self.dbc.is_open() :
                self.dbc.close()

    @cmdln.option("-e", "--exp",metavar="name",dest="exp_name", 
                  help="Experiment Name")
    @cmdln.option("-r", "--rea",metavar="name",dest="rea_name", 
                  help="Realization Name")
    @cmdln.option("-F", "--frea",metavar="FILE",dest="rea_file",
                  help="File containing the Realization Names to change of the priority")
    @cmdln.option("-v", "--verbose",action="store_true",default=False, dest="verbose",
                  help="Verbose mode. Explain what is being done")
    @cmdln.option("-p", "--priority",type="int",dest="priority",
                  help="The priority must be in range [0,20].")

    def do_priority(self, subcmd, opts, *args):
        """Change the priority of an experiment or realization. The priority must be in range [0,20] 
            and default value is 0. When a chunk gets a priority of 20, it becomes an urgent. 
            This chunk is dispatched as soon as possible, bypassing all the scheduling policies. 
        
        usage:
            wrf4g priority [options]
        
        ${cmd_option_list}
        """
        try:
            self.dbc.connect()
            if not ( opts.priority or opts.exp_name or opts.rea_name or opts.rea_file ):
                raise Exception( "Use '--help' option " )
            if opts.exp_name:
                exp=Experiment(data={'name':'%s'%opts.exp_name}, verbose=opts.verbose , dbc = self.dbc)
                id=exp.get_id_from_name()
                if int(id) < 0:
                    raise Exception( "Experiment '%s' does not exists" % opts.exp_name )
                else:
                    reas=exp.get_run_reas_id()
                    reas.extend(exp.get_wait_reas_id())
                    for id_rea in reas:
                        Realization(data={'id':'%s'%id_rea},verbose=opts.verbose,dbc = self.dbc)
                        
            elif opts.rea_name:
                for rea_name in opts.rea_name.split( ',' ) :
                    rea=Realization(data={'name': rea_name },verbose=opts.verbose,dbc = self.dbc)
                    id=rea.get_id_from_name()   
                    if int(id) < 0:
                        raise Exception( "Realization '%s' does not exists" % rea_name )
                    else:
                        rea.change_priority(opts.priority)
            elif opts.rea_file:
                try:
                    f=open(opts.rea_file)
                    for realization in f.readlines():
                        realization=realization.rstrip()
                    rea=Realization(data={'name':'%s'%realization},verbose=opts.verbose,dbc = self.dbc)
                    id=rea.get_id_from_name()
                    if int(id) < 0:
                        raise Exception( "Realization with name '%s' does not exist" % realization )
                    else:
                        rea.change_priority(opts.priority)
                finally: 
                    f.close()
        except Exception, err :
            sys.stderr.write( str( err ) + '\n' )    
        finally:
            if self.dbc.is_open() :
                self.dbc.close()

    @cmdln.option("-e", "--exp",metavar="name",dest="exp_name",
                  help="Experiment name.")
    @cmdln.option("-r", "--rea",metavar="name",dest="rea_name",
                  help="Realization name.")

    def do_statistic(self, subcmd, opts, *args):
        """Prints the experiment or realization status.
        
        usage:
            wrf4g statistic [options]
                    
        ${cmd_option_list}
        """
        def _long_header():
            linea="realization;chunk_id;gwid;exit_code;resource;wn;"
            lestados=JobStatus( self.dbc )
            es=lestados.keys()
            es.sort()
            for st in es:
                linea+=lestados[st] + ";"
            print linea
        try:
            self.dbc.connect()
            if opts.exp_name:
                experiment_name=opts.exp_name
                exp=Experiment(data={'name':'%s'%experiment_name} , dbc = self.dbc )
                exp.data['id']=exp.get_id_from_name()
                if exp.data['id']<0:
                    raise Exception( "Experiment '%s' does not exists" % experiment_name )
                _long_header()
                exp.statistics()
            elif opts.rea_name:
                realization_name=opts.rea_name
                rea=Realization(data={'name':'%s'%realization_name} , dbc = self.dbc )
                _long_header()
                rea.statistics()        
            else:
                raise Exception( "Experiment or realization name have to be provided" )    
        except Exception, err :
            sys.stderr.write( str( err ) + '\n' )
        finally:
            if self.dbc.is_open() :
                self.dbc.close()

    @cmdln.option("-n", "--dry-run",action="store_true", default=False, dest="dryrun",
                  help="Perform a trial run with no changes made")
    @cmdln.option("-e", "--exp",metavar="name",dest="exp_name", 
                  help="Experiment name")
    @cmdln.option("-r", "--rea",metavar="name",dest="rea_name", 
                  help="Realization name")
    @cmdln.option("-F", "--frea",metavar="FILE",dest="rea_file", 
                  help="File containing the realizations")
    @cmdln.option("-v", "--verbose",action="store_true",default=False, dest="verbose",
                  help="Verbose mode. Explain what is being done")

    def do_kill(self, subcmd, opts, *args):
        """Kills the jobs that belong to an experiment or realization. Additionally, 
            the experiment and realization ckunks, which have not done, will go back to "Prepared" status.
        
        usage:
            wrf4g kill [options]
                    
        ${cmd_option_list}
        """
        try:
            self.dbc.connect()
            if not ( opts.exp_name or opts.rea_name or opts.rea_file ): 
                raise Exception( "Use '--help' option " )
            if opts.exp_name:
                exp=Experiment(data={'name':'%s'%opts.exp_name},verbose=opts.verbose,dryrun=opts.dryrun,dbc = self.dbc)
                id=exp.get_id_from_name()
                if int(id) < 0:
                    raise Exception( "Experiment %s does not exists" % opts.exp_name )
                else:
                    id=exp.get_id_from_name()
                    reas=exp.get_run_reas_id()
                    reas.extend(exp.get_wait_reas_id())
                    for id_rea in reas:
                        rea=Realization(data={'id':'%s'%id_rea},verbose=opts.verbose,dryrun=opts.dryrun,dbc = self.dbc)
                        rea.stop_running_chunks()
            elif opts.rea_name:
                for rea_name in opts.rea_name.split( ',' ) :
                    rea=Realization(data={'name': rea_name},verbose=opts.verbose,dryrun=opts.dryrun,dbc = self.dbc)
                    id=rea.get_id_from_name()   
                    if int(id) < 0:
                        raise Exception( "Realization '%s' does not exists" % rea_name )
                    else:
                        rea.stop_running_chunks()
            elif opts.rea_file:
                f=open(opts.rea_file)
                try:
                    for realization in f.readlines():
                        realization=realization.rstrip()
                        rea=Realization(data={'name':'%s'%realization},verbose=opts.verbose,dryrun=opts.dryrun,dbc = self.dbc)
                        id=rea.get_id_from_name()
                        if int(id) < 0 :
                            raise Exception( "Realization '%s' does not exist" % realization )
                        else:
                            rea.stop_running_chunks()
                finally:
                    f.close()
        except Exception, err :
            sys.stderr.write( str( err ) + '\n' )
        finally:
            if self.dbc.is_open() :
                if opts.dryrun :
                    self.dbc.rollback()
                else:
                    self.dbc.commit()
                self.dbc.close()    
                    
    @cmdln.option("-s", "--start",action="store_true",
                  help="Start DRM4G and WRF4G database")
    @cmdln.option("-S", "--stop",action="store_true",
                  help="Stop DRM4G and WRF4G database")
    @cmdln.option("-t", "--status",action="store_true",
                  help="DRM4G and WRF4G database status")
    @cmdln.option("-c", "--clear",action="store_true",
                  help="Clears previous DRM4G jobs")
    
    def do_framework(self, subcmd, opts, *args):
        """Manages WRF4G framework components both DRM4G and WRF4G_DB.
        It loads the framework configuration from db.conf and resources.conf files.
        
        usage:
            wrf4g framework [options]
                    
        ${cmd_option_list}
        """
        try:
            framework = FrameWork()   
            if opts.start :
                framework.start()
            elif opts.stop :
                framework.stop()
            elif opts.status :
                framework.status()
            elif opts.clear : 
                framework.clear_drm4g()
            else :
                raise Exception( "Please use '--help' option " )
        except Exception, err:
            sys.stderr.write( str( err ) + '\n' )        
            
    @cmdln.option("-c", "--check",action="store_true",
                  help="Check if the Grid proxy certificate is valid. You have to indicate the resource.")
    @cmdln.option("-d", "--download",action="store_true",
                  help="It  retrieves  a  proxy  credential  from  the myproxy-server. You have to indicate the resource.")
    @cmdln.option("-u", "--upload", action="store_true",
                  help="It uploads the credential to a myproxy-server. You have to indicate the resource")
    @cmdln.option("-C", "--create",action="store_true",
                  help=" It creates a proxy for 7 days. You only have to indicate the resource.")
    @cmdln.option("-D", "--destroy", action="store_true",
                  help="It destroys the proxy on the myproxy-server. You have to indicate the resource.")

    def do_proxy(self, subcmd, opts, *args):
        """Command for managing X.509 Public Key Infrastructure (PKI) security credentials
        
        usage:
            wrf4g proxy [options] resource
                    
        ${cmd_option_list}
        """
        try :
            if len( args ) is not 1 :
               raise Exception( "Please, provide a resource" )
            res_name  = args[ 0 ]
            resource  = self.config.resources[ res_name ]
            communicator = self.config.make_communicators()[ res_name ]
            communicator.connect()
            proxy = Proxy( resource , communicator )
            if opts.download:
                proxy.download( )
            elif opts.check :
                proxy.check( )
            elif opts.destroy :
                proxy.destroy( )
            elif opts.upload :
                proxy.upload( )
            elif opts.create:
                proxy.upload( )
                proxy.download( )
            else :
                raise Exception( "Please use '--help' option " )
        except Exception, err:
            sys.stderr.write( str( err ) + '\n' )    
            
    @cmdln.option("-v", "--verbose",action="store_true", dest="verbose", default=False,
                  help="Verbose mode. Explain what is being done")
    @cmdln.option("-r", "--reconfigure",action="store_true", dest="reconfigure", default=False,
                  help="Reconfigure element in WRF4G")
    @cmdln.option("-n", "--dry-run",action="store_true", dest="dryrun", default=False,
                  help="Perform a trial run with no changes made")

    def do_shell(self, subcmd, opts, *args):
        """Specify an interpreter interface to execute WRF4G.
        
        usage:
            wrf4g shell [options] 
                    
        ${cmd_option_list}
        """
        try :
            if len( args ) < 2 :
                raise Exception( "Incorrect number of arguments" )
            class_name , function = args[ 0:2 ]
            if len( args ) > 2 :
                data = pairs2dict( args[ 2 ] )
            else :
                data = ''
            inst = "%s(data=%s,verbose=opts.verbose,reconfigure=opts.reconfigure,dryrun=opts.dryrun,dbc=self.dbc)" % ( class_name , data )
            comp = eval( inst )
            try:
                self.dbc.connect()
                if len( args ) > 3 :     
                    # Call the Class method.
                    output = getattr( comp , function )( args[ 3: ] )
                else:                  
                    output = getattr( comp , function )()
                sys.stdout.write( str( output ) )
            finally:
                if self.dbc.is_open() :
                    if opts.dryrun:
                        self.dbc.rollback()
                    else:
                        self.dbc.commit()
                    self.dbc.close()
        except Exception , err :
            sys.stderr.write( str( err ) + '\n' )
            sys.exit(-1)    
            
    @cmdln.option("-v", "--verbose",action="store_true", default=False,
                  help="Verbose mode. Explain what is being done")
    @cmdln.option("-r", "--reconfigure",action="store_true", default=False,
                  help="Reconfigure experiment. With this option we can change the start and end date of the experiments and add new physics." 
                   "Values are taken from a modified experiment.wrf4g.")
    @cmdln.option("-n", "--dry-run",action="store_true", dest="dryrun", default=False,
                  help="Perform a trial run with no changes made")

    def do_prepare(self, subcmd, opts, *args):
        """Given the location of a experiment.wrf4g file, it prepares the experiment
        creating the realizations and chunks needed to perform it.
        
        usage:
            wrf4g prepare [options] [directory]
                    
        ${cmd_option_list}
        """
        try :
            if len( args ) > 1 :
                raise Exception( "Please use '--help' option " )
            options = ''
            if opts.verbose :
                options += ' --verbose'
            if opts.reconfigure :
                options += ' --reconfigure'
            if opts.dryrun :
                options += ' --dry-run'
            if not args :
                dir_experiment_wrf4g = os.getcwd( )
            else :
                if isdir( args[ 0 ] ) :
                    dir_experiment_wrf4g = args[ 0 ]
                else :
                    raise Exception( "'%s' is not a directory" %  dir_experiment_wrf4g )
            experiment_wrf4g = join( dir_experiment_wrf4g , 'experiment.wrf4g' )
            if not exists( experiment_wrf4g ) :
                raise Exception( "'%s' does not exist" %  experiment_wrf4g )
            os.environ['W4G_EXPERIMENT'] = experiment_wrf4g
            cmd_prepare = join( WRF4G_DEPLOYMENT_LOCATION , 'bin' , 'prepare' ) + options
            exec_cmd = subprocess.Popen( cmd_prepare, shell=True , stderr=subprocess.PIPE )
            while True:
                out = exec_cmd.stderr.read(1)
                if out == '' and exec_cmd.poll() != None:
                    break
                if out != '':
                    sys.stdout.write(out)
                    sys.stdout.flush()
        except Exception, err:
            sys.stderr.write( str( err ) + '\n' )  

    @cmdln.option("-t", "--test", action="store", type="choice", choices=["single", "physics"],
                  help="Test experiment to choose. You can choose either 'single' or 'physics' experiments")
    @cmdln.option("-d", "--directory",action="store_true",
                  help="Directory to create the WRF4G experiment directory structure")
    @cmdln.option("-v", "--verbose",action="store_true",default=False,
                  help="Verbose mode. Explain what is being done")

    def do_startexp(self, subcmd, opts, *args):
        """Creates a WRF4G experiment directory structure for the given experiment name 
        in the current directory or in the given directory.

        usage:
            wrf4g startexp [options] [experiment_name] 
                    
        ${cmd_option_list}
        """
        try :
            if opts.test is None :
                if len( args ) is not 1 :
                    raise Exception( "Please, provide a resource" )
                exp_name  = args[ 0 ]
                validate_name( exp_name )
            else :
                exp_name = opts.test
        
            def _create_directory( dir ) :
                try:
                    os.makedirs( dir )
                except OSError as err :
                    if err.errno == errno.EEXIST:
                        message = "'%s' already exists" % dir
                    else:
                        message = err
                    raise Exception( message )
        
            # if some directory is given, make sure it's nicely expanded
            if opts.directory is None:
                exp_dir = join( os.getcwd( ) , exp_name ) 
            else:
                top_dir = abspath( expanduser( opts.directory ) )
                if not exists( top_dir ):
                    raise Exception("Destination directory '%s' does not "
                                       "exist, please create it first." % top_dir )
                exp_dir = join( top_dir, exp_name )
           
            if opts.test is None :
                if opts.verbose : 
                    print "Creating '%s'" % exp_dir
                _create_directory( exp_dir )
                wrf4g_files = join( exp_dir , 'wrf4g_files' )
                if opts.verbose : 
                    print "Creating '%s'" % wrf4g_files
                _create_directory( wrf4g_files )
                experiment_wrf4g_template = """
[DEFAULT]
#   Experiment configuration
#
experiment_name      = "%s"

#   Simulation domain
max_dom              = 1
domain_name          = "domain"

#   Input data
extdata_vtable       = "GFS"    # Vtables must exist as Vtable.[input_extdata]
extdata_path         = "PATH"
extdata_interval     = SS       # Seconds between global analysis input times
extdata_preprocessor = "preprocessor_name"

#   Output data
postprocessor        = "postprocessor_name"
wrfout_name_end_date = 0

#   Experiment time-specification
rerun                = 0
start_date           = "Year-Month-Day_HH:MM:SS"
end_date             = "Year-Month-Day_HH:MM:SS"
chunk_size_h         = HH

multiple_dates       = 0
simulation_interval_h    = 0
simulation_length_h      = 0

multiple_parameters  = 0
multiparams_variables    = ""
multiparams_nitems       = ""
multiparams_combinations = ""
multiparams_labels       = ""

#   Debugging
clean_after_run      = 1
save_wps             = 0

#   Parallel configuration  
real_parallel        = 0
wrf_parallel         = 1

#   WRF-namelist parameters. Override namelist.input variables 

namelist_version       = "x.x.x" """ % exp_name
                exp_file = join( exp_dir , 'experiment.wrf4g' )
                if opts.verbose :
                    print "Creating '%s'" % exp_file
                with open( exp_file , 'wb' ) as file:
                    file.write( experiment_wrf4g_template )
                old_path = join( WRF4G_LOCATION , 'etc' , 'resources.wrf4g' )
                new_path = join( exp_dir , 'resources.wrf4g' )
                if opts.verbose : 
                    print "Creating '%s'" % new_path
                shutil.copy( old_path , new_path )
            else :
                old_path = join( WRF4G_LOCATION , 'etc' , 'test_experiments' , opts.test )
                new_path = exp_dir 
                if opts.verbose : 
                    print "Creating '%s' test experiment" % opts.test
                shutil.copytree( old_path , new_path )
        except Exception , err :
            sys.stderr.write( str( err ) + '\n' )
            
    @cmdln.option("-l", "--list",action="store_true",
                  help="List the resources available.")
    @cmdln.option("-c", "--check", action="store_true",
                  help="Check if the resources.conf file has been configured well.")
    @cmdln.option("-f", "--features", action="store_true",
                  help="List the features of a given resource.")
    @cmdln.option("-C", "--check_frontends",action="store_true",
                  help="Check if all frontends are reachable.")
    @cmdln.option("-L", "--list_host",action="store_true",
                  help="List the hosts and their features."
                  "In order to see all the information you have to specify the HID.")

    def do_resources(self, subcmd, opts, *args):
        """Prints information about the resources in WRF4G 
        
        usage:
            wrf4g resources [options] 
                    
        ${cmd_option_list}
        """
        try :
            res = Resource( self.config )
            if opts.list :
                res.list_resources()
            elif opts.check_frontends :
                res.check_frontends()
            elif opts.features :
                res.resource_features()
            elif opts.list_host :
                if len( args ) is 1 :
                    hid = args[ 0 ]
                else :
                    hid = None
                res.list_host( hid )
            elif opts.check :
                res.check_resources()
            else :
                raise Exception( "Please use '--help' option " )
        except Exception , err :
            sys.stderr.write( str( err ) + '\n' )

    @cmdln.option("-v", "--verbose", action="store_true", dest="verbose", default=False, 
                      help="Verbose mode. Explain what is being done")
    @cmdln.option("-o", "--overwrite", action="store_true", dest="overwrite", default=True, 
                      help="If the destination already exists but is not a directory, it will be overwritten")
    
    def do_vcp(self, subcmd, opts, *args):
        """Virtual copy.
        
        usage:
            wrf4g vcp [options] SOURCE DEST
            
        LFN              lfn:///grid/VO/file file://home/user/file 
        GRIDFTP          gridftp://computer:2812/grid/VO/user/file
        RSYNC            rsync://user@computer:34/grid/VO/user/file
        SIMBOLIC LINK    ln:/home/user/file or ln:file
        FILE             file:/home/user/file file:/home/user/file2
        HTTPS            https://www.meteo.unican.es/work/WRF4G.tar.gz
        HTTP             http://www.meteo.unican.es/work/WRF4G.tar.gz
        FTP              ftp://www.meteo.unican.es/work/WRF4G.tar.gz
                        
        ${cmd_option_list}
        """
        try:
            if len(args) is not 2 :
                raise Exception("Incorrect number of arguments")
            orig = args[0]
            dest = args[1]
            orig_file = basename(orig)
            if '*' in orig_file :
                orig_dir = dirname(orig)
                for file in vcplib.VCPURL(orig_dir).ls(orig_file):
                    vcplib.copy(join(orig_dir, file), dest, overwrite = opts.overwrite, verbose=opts.verbose)
            else:
                vcplib.copy(orig, dest, overwrite = opts.overwrite, verbose=opts.verbose)
        except Exception, err :
            sys.stderr.write( str( err ) + '\n' )
            sys.exit(-1)

    def do_exit( self, subcmd, opts, *args ):
        """
        Quits the console.
        """
        sys.exit( 0 )
    
    do_quit = do_exit
           
                
def execute_from_command_line( argv ):
    """
    A method that runs a ManagementUtility.
    """
    if len( argv ) > 1:
        ManagementUtility().main( argv )
    else:
        ManagementUtility().cmdloop()
