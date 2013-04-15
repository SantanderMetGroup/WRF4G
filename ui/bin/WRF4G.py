#!/usr/bin/env python

from wrf4g_env import *
from WRF4Glib import *

if __name__ == "__main__":
    usage="""%prog [OPTIONS] exp_values function fvalues 
             Example: %prog 
    """
        
    parser = OptionParser(usage,version="%prog 1.0")
    parser.add_option("-v", "--verbose",action="store_true", dest="verbose", default=False,help="Verbose mode. Explain what is being done")
    parser.add_option("-r", "--reconfigure",action="store_true", dest="reconfigure", default=False,help="Reconfigure element in WRF4G")
    parser.add_option("-n", "--dry-run",action="store_true", dest="dryrun", default=False,help="Perform a trial run with no changes made")
    (options, args) = parser.parse_args()
    
    if len(args) < 2:
        parser.error("Incorrect number of arguments")
        sys.exit(1)
    class_name=args[0]
    function=args[1]    
    data=''
    if len(args) > 2:
        data=pairs2dict(args[2])         
    inst="%s(data=%s,verbose=options.verbose,reconfigure=options.reconfigure,dryrun=options.dryrun)"%(class_name,data)
    # Instantiate the Component Class:
    # comp=Chunk(data={'id': '23'},verbose=options.verbose,reconfigure=options.reconfigure)
    comp=eval(inst)
    try:
        if len(args) > 3:     
            fvalues=args[3]
            # Call the Class method.
            output=getattr(comp,function)(*args[3:])
        else:                  
            output=getattr(comp,function)()
        sys.stdout.write(str(output))
    except Exception, e:
        print 'Caught exception: %s: %s' % (e.__class__, str(e))
        sys.exit(-1)
    finally:
        if options.dryrun:
            dbc.rollback()
        else:
            dbc.commit()
        dbc.close()






   
