from __future__     import with_statement
import sys
import os
import subprocess
import cmdln
import errno
import shutil


from drm4g.commands  import exec_cmd, process_is_runnig
from os.path         import exists , expandvars , join , abspath , dirname , expanduser , basename , isdir 
from wrf4g.core      import Experiment , Realization , JobStatus , Environment , Chunk , Job
from wrf4g.utils     import VarEnv , validate_name , pairs2dict , edit_file

__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"
            
class DataBase( object ):

    def __init__( self ):
        self.port       = VarEnv( DB4G_CONF ).get_variable( 'WRF4G_DB_PORT'  , 'Database' )

        self.file_pid   = join( WRF4G_DIR , 'var' , 'mysql.pid' )
        self.mysql_sock = join( WRF4G_DIR , 'var' , 'mysql.sock' )
        self.mysql_log  = join( WRF4G_DIR , 'var' , 'log'   , 'mysql.log' )

    def _port_is_free( self , port ):
        import socket
        sock = socket.socket( socket.AF_INET, socket.SOCK_STREAM )
        if sock.connect_ex( ( '127.0.0.1', int( port ) ) ) is 0 :
            return False
        else :
            return True

    def status( self ):
        if not exists( self.mysql_pid ) :
            logger.info( "WRF4G_DB (MySQL) is stopped" )
        elif process_is_runnig( self.mysql_pid ) :
            logger.info( "WRF4G_DB (MySQL) is running" )
        else :
            logger.info( "WRF4G_DB (MySQL) is stopped" )

    def start( self ):
        logger.info( "Starting WRF4G_DB (MySQL) ... " )
        if not self._port_is_free( self.mysql_port ) and not process_is_runnig( self.mysql_pid ):
            raise Exception( "WARNING: Another process is listening on port %s."
              "Change WRF4G_DB_PORT in '%s' file to start MySQL on a different port." % ( self.mysql_port , DB4G_CONF )
              "By executing the command 'wrf4g conf database'."
              )
        elif not exists( self.mysql_pid ) or ( exists( self.mysql_pid ) and not process_is_runnig( self.mysql_pid ) ) :
            mysql_options = "--no-defaults --port=%s --socket=%s --log-error=%s --pid-file=%s" % ( self.mysql_port ,
                                                                                                     self.mysql_sock ,
                                                                                                     self.mysql_log ,
                                                                                                     self.mysql_pid
                                                                                                     )
            cmd =  "cd %s ; nohup ./bin/mysqld_safe %s &>/dev/null &" % ( MYSQL_DIR , mysql_options )
            exec_cmd( cmd )
            time.sleep( 1.0 )
            if not exists( self.mysql_pid ) or self._port_is_free( self.mysql_port ) :
                logger.error( "ERROR: MySQL did not start, check '%s' for more information " % self.mysql_log )
            else :
                logger.info( "OK" )
        else :
            logger.warn( "WARNING: MySQL is already running" )

    def stop( self ):
        if self.local_db == '1' :
            logger.info( "Stopping WRF4G_DB (MySQL) ..." )
            if not exists( self.mysql_pid ) or ( exists( self.mysql_pid ) and not process_is_runnig( self.mysql_pid ) ) :
                logger.warn( "WARNING: MySQL is already stopped." )
            elif exists( self.mysql_pid ) and process_is_runnig( self.mysql_pid ) :
                with open( self.mysql_pid , 'r') as f:
                    pid = f.readline().strip()
                mysql_ppid , err = exec_cmd( "ps h -p %s -o ppid" % pid )
                if err :
                    raise Exception( err )
                try :
                    os.kill( int( mysql_ppid ), signal.SIGKILL )
                    os.kill( int( pid ) , signal.SIGKILL )
                    logger.info( "OK" )
                except Exception , err :
                    logger.error( "ERROR: stopping MySQL: %s" % err )
            else :
                logger.warn( "WARNING: MySQL is already stopped." )
        else :
             logger.warn( "You are using a remote WRF4G_DB (MySQL)" )


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
            wrf4g submit options
        
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
                rea=Realization(data={'name': opts.rea_name },
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
                with open( opts.rea_file , 'r') as f :
                    for realization in f.readlines():
                        realization=realization.rstrip()
                        rea=Realization(data={'name': realization},
                                        verbose=opts.verbose,
                                        dryrun=opts.dryrun,
                                        dbc = self.dbc)
                        id=rea.get_id_from_name()
                        if id < 0 :
                            raise Exception( "Realization with name '%s' does not exist" % realization )
                        else:
                            rea.run(nchunk=opts.nchunk,
                                    priority=opts.priority,
                                    rerun=opts.rerun,
                                    force=opts.force,
                                    type_dep=opts.type_dep)      
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
            wrf4g status options
        
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
            print string % ('Realization','GW','Stat','Chunks','Host','WN','Run.Sta','ext','%')        
        
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
                  help="Experiment name.")
    @cmdln.option("-r", "--rea",metavar="name",dest="rea_name",
                  help="Realization name.")

    def do_statistic(self, subcmd, opts, *args):
        """Prints the experiment or realization status.
        
        usage:
            wrf4g statistic options
                    
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
            wrf4g kill options
                    
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
                with open( opts.rea_file , 'r') as f :
                    for realization in f.readlines():
                        realization=realization.rstrip()
                        rea=Realization(data={'name':'%s'%realization},verbose=opts.verbose,dryrun=opts.dryrun,dbc = self.dbc)
                        id=rea.get_id_from_name()
                        if int(id) < 0 :
                            raise Exception( "Realization '%s' does not exist" % realization )
                        else:
                            rea.stop_running_chunks()
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
            wrf4g framework options
                    
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
             
    @cmdln.option("-v", "--verbose",action="store_true", default=False,
                  help="Verbose mode. Explain what is being done")
    @cmdln.option("-r", "--reconfigure",action="store_true", default=False,
                  help="Reconfigure experiment. With this option we can change the start and end date of the experiments and add new physics." 
                   "Values are taken from a modified experiment.wrf4g.")
    @cmdln.option("-n", "--dry-run",action="store_true", dest="dryrun", default=False,
                  help="Perform a trial run with no changes made")

    def do_startexp(self, subcmd, opts, *args):
        """Given the location of a experiment.wrf4g file, it prepares the experiment
        creating the realizations and chunks needed to perform it.
        
        usage:
            wrf4g startexp [options] [directory]
                    
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

    @cmdln.option("-t", "--template", action="store", type="choice", choices=[ "default", "single", "physics"], default="default",
                  help="Experiment template to choose. You can choose either 'single' or 'physics' experiments")
    @cmdln.option("-d", "--directory",action="store_true",
                  help="Directory to create a WRF4G experiment directory structure")
    @cmdln.option("-v", "--verbose",action="store_true",default=False,
                  help="Verbose mode. Explain what is being done")

    def do_setupexp(self, subcmd, opts, *args):
        """Creates a WRF4G experiment directory structure for the given experiment name 
        in the current directory or in the given directory.

        usage:
            wrf4g setupexp [options] [experiment_name] 
                    
        ${cmd_option_list}
        """
        try :
            if opts.template is None and len( args ) is not 1 :
                raise Exception( "Please, provide an experiment name" )
            elif ( opts.template and len( args ) is 1 ) or ( len( args ) is 1 ):
                exp_name  = args[ 0 ]
                validate_name( exp_name )
            else :
                exp_name = opts.template
                
            # if some directory is given, make sure it's nicely expanded
            if opts.directory is None :
                exp_dir = join( os.getcwd( ) , exp_name ) 
            else:
                top_dir = abspath( expanduser( opts.directory ) )
                if not exists( top_dir ):
                    raise Exception("Destination directory '%s' does not "
                                       "exist, please create it first." % top_dir )
                exp_dir = join( top_dir, exp_name )
            if exists( exp_dir ):
                raise Exception("'%s' already exists" % exp_dir )
                
            if opts.verbose :
                print "Creating '%s' directory" % exp_dir                    
            shutil.copytree( join( WRF4G_LOCATION , 'etc' , 'templates' , 'experiments',  opts.template ) ,  exp_dir )
            for file in [ 'resources.wrf4g' , 'experiment.wrf4g' ] :  
                dest_path = join( exp_dir , file )
                with open( dest_path , 'r') as f :
                    data = ''.join( f.readlines( ) )
                data_updated = data % { 
                                       'WRF4G_EXPERIMENT_HOME'     : exp_dir , 
                                       'WRF4G_DEPLOYMENT_LOCATION' : WRF4G_DEPLOYMENT_LOCATION ,
                                       'exp_name'                  : exp_name ,
                                        }
                with open( dest_path , 'w') as f :
                    f.writelines( data_updated )
        except Exception , err :
            sys.stderr.write( str( err ) + '\n' )
        
            
    @cmdln.option("-v", "--verbose",action="store_true", dest="verbose", default=False,
                  help="Verbose mode. Explain what is being done")
    @cmdln.option("-r", "--reconfigure",action="store_true", dest="reconfigure", default=False,
                  help="Reconfigure element in WRF4G")
    @cmdln.option("-n", "--dry-run",action="store_true", dest="dryrun", default=False,
                  help="Perform a trial run with no changes made")

    def do_shell(self, subcmd, opts, *args):
        """Specify an interpreter interface to execute WRF4G classes.
        
        usage:
            wrf4g shell [options] CLASSES
                    
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
            
