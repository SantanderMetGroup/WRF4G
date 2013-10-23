import paramiko
import socket
import os.path
public_key_path = os.path.expanduser( '~/.ssh/id_rsa' )
key  = paramiko.RSAKey.from_private_key_file( public_key_path )
sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
sock.connect( ( 'ui.macc.unican.es' , 22 ) )
trans = paramiko.Transport( sock )
trans.connect( username = 'carlos' , pkey = key )
channel = trans.open_session()
channel.exec_command( 'MYPROXY_SERVER=px.grid.sara.nl myproxy-logon' )
channel.makefile('wb', -1).write( 'wolnlalee\n')
channel.makefile('wb', -1).flush()
print channel.makefile_stderr('rb', -1).readlines()
print channel.makefile('rb', -1).readlines()

