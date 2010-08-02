#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#       gw_tm_mad_ssh.py Compatible with Python2.3 - 2.6
#       
#       Copyright 2010 Jose Carlos <carlos@ciclon>
#       
#       This program is free software; you can redistribute it and/or modify
#       it under the terms of the GNU General Public License as published by
#       the Free Software Foundation; either version 2 of the License, or
#       (at your option) any later version.
#       
#       This program is distributed in the hope that it will be useful,
#       but WITHOUT ANY WARRANTY; without even the implied warranty of
#       MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#       GNU General Public License for more details.
#       
#       You should have received a copy of the GNU General Public License
#       along with this program; if not, write to the Free Software
#       Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
#       MA 02110-1301, USA.

import paramiko
import sys
import os
import re
import logging
import logging.handlers
from optparse import OptionParser
from threading import Thread
from Queue import Queue

if os.environ['GW_LOCATION']:
	try:
		sys.path.append(os.environ['GW_LOCATION'] +'etc')
		import variable
	except Exception, e:
		print e
		sys.exit(-1)
else:
	print 'Please, set GW_LOCATION variable'
	sys.exit(-1)

logger=None
handler=None

class GwTmMad:
	""" Information manager MAD
	
	The File Transfer Driver interfaces with Grid Data Management Services and is responsible for file staging, 
	remote working directory set-up and remote host clean up.
	
	The format to send a request to the Transfer MAD, through its standard input, is: 
	    
	    OPERATION JID TID EXE_MODE SRC_URL DST_URL
	
	Where:
	
	-OPERATION: Can be one of the following:
		-INIT: Initializes the MAD, JID should be max number of jobs.
		-START: Init transfer associated with job JID
		-END: Finish transfer associated with job JID
		-MKDIR: Creates directory SRC_URL
		-RMDIR: Removes directory SRC_URL
		-CP: start a copy of SRC_URL  to DST_URL, with identification TID, 
			and associated with job JID.
		-FINALIZE: Finalizes the MAD.
	-JID: Is a job identifier, chosen by GridWay.
	-TID: Transfer identifier, only relevant for command CP.
	-EXE_MODE: If equal to 'X' file will be given execution permissions, 
		only relevant for command CP.
	
	The format to receive a response from the MAD, through its standard output, is: 
	
		OPERATION JID TID RESULT INFO
	
	Where:
	
	-OPERATION: Is the operation specified in the request that originated 
        the response or CALLBACK, in the case of an asynchronous notification of a state change.
    -JID: It is the job identifier, as provided in the START request.
    -TID: It is the transfer identifier, as provided in the CP request.
    -RESULT: It is the result of the operation. Could be SUCCESS or FAILURE.
    -INFO: If RESULT is FAILURE, it contains the cause of failure."""

	def __init__(self):
		logger_init(variable.logger_name_tm,variable.LOG_FILENAME_tm,variable.format_tm)
		self.queue_remote_command = Queue()
		self.queue_cp = Queue()
		num_threads_remote_command = 10
		num_threads_cp = 10
		for i in xrange(num_threads_remote_command):
			worker = Thread(target = remote_command, args = (self.queue_remote_command,))
			worker.setDaemon(True)
			worker.start()
		for i in xrange(num_threads_cp):
			worker = Thread(target = cp_command, args = (self.queue_cp,))
			worker.setDaemon(True)
			worker.start()

	def __INIT(self, args):
		"""INIT: Initializes the MAD, JID should be max number of jobs.(i.e. INIT JID - - - -)"""
		answer(args,'SUCCESS','-')

	def __START(self, args):
		"""START: Init transfer associated with job JID.(i.e. START JID - - - -)"""
		answer(args,'SUCCESS','-')
	
	def __END(self, args):
		"""END: Finish transfer associated with job JID .(i.e. END JID - - - -)"""
		answer(args,'SUCCESS','-')

	def __FINALIZE(self, args):
		"""Finalizes the MAD (i.e. FINALIZE - - - - -)"""
		answer(args,'SUCCESS','-')
		logger.removeHandler(handler)
		handler.close()
		sys.exit(0)
	
	def __MKDIR(self, args):
		"""MKDIR: Creates directory SRC_URL (i.e. MKDIR JID - - SRC_URL -) """
		self.queue_remote_command.put(args)

	def __RMDIR(self, args):
		"""RMDIR: Removes directory SRC_URL (i.e. RMDIR JID - - SRC_URL -) """
		self.queue_remote_command.put(args)

	def __CP(self, args):
		"""CP: start a copy of SRC_URL  to DST_URL, with identification TID, 
			and associated with job JID.(i.e. CP JID TID - SRC_URL DST_URL) """
		self.queue_cp.put(args)
	
	methods = {
        'INIT':			__INIT,
        'START':        __START,
        'END':          __END,
        'MKDIR':        __MKDIR,
        'RMDIR':        __RMDIR,
        'CP':           __CP,
        'FINALIZE':    	__FINALIZE
        }
        
	def processLine(self):
		"""Choose the OPERATION through the command line"""
		while True:
			input = sys.stdin.readline().split()
			logger.debug(" ".join(input))
			action=input[0].upper() #OPERATION
			if len(input) == 6 and GwTmMad.method.has_key(action):
				GwTmMad.method[action](self,input)
			else:
				out = 'Incorrect number of arguments'
				print_stdout(out)
				logger.debug(out)

action_command  = {
        'MKDIR':        'mkdir',
        'RMDIR':        'rm -rf'
        }
        
def remote_command (queue):
	while True:
		args = queue.get()
		OPERATION,JID,TID,EXE_MODE,SRC_URL,DST_URL = args
		to_command = action_command[OPERATION] 
		(to_host, to_dir) = parse_url(SRC_URL)
		client = paramiko.SSHClient()
		client.set_missing_host_key_policy(paramiko.AutoAddPolicy())
		try:
			privatekeyfile = os.path.expanduser(variable.id_rsa_tm)
			mykey = paramiko.RSAKey.from_private_key_file(privatekeyfile)
			client.connect(to_host,username = variable.username_tm,pkey = mykey)
		except Exception, e:
			answer(args,'FAILUE',e)
		else:
			command = to_command + ' ' + to_dir
			stdin, stdout, stderr = client.exec_command(command)
			stdin.close()
			client.close()
			answer(args,'SUCCESS','-')

def cp_command(queue):
	while True:
		args = queue.get()
		OPERATION,JID,TID,EXE_MODE,SRC_URL,DST_URL = args
		(from_host, from_dir)=parse_url(SRC_URL)
		(to_host, to_dir)=parse_url(DST_URL)

		if from_host:
			from_dir = set_home_dir(from_dir)
			to_host = from_host
		else:
			to_dir = set_home_dir(to_dir)

		try:
			transport = paramiko.Transport(to_host)
			privatekeyfile = os.path.expanduser(variable.id_rsa_tm)
			mykey = paramiko.RSAKey.from_private_key_file(privatekeyfile)
			transport.connect(username = variable.username_tm,pkey = mykey)
			sftp = paramiko.SFTPClient.from_transport(transport)
		except Exception, e:
			answer(args,'FAILUE','(' + SRC_URL + ' --> ' + DST_URL +') except:' + e)
		else:
			if not from_host:
				try:
					sftp.put(from_dir,to_dir)
				except Exception, e:
					answer(args,'FAILUE','Error using the command \'put\' between' + '(' + SRC_URL + ' --> ' + DST_URL +') except:' + e)
				if EXE_MODE == 'X':
					try:
						sftp.chmod(to_dir,0755) #execution permissions
					except Exception, e:
						answer(args,'FAILUE','Error using the command \'chmod\' except:' + e)
			else:
				try:
					sftp.get(from_dir,to_dir)
				except Exception, e:
					answer(args,'FAILUE','Error using the command \'get\' between' + '(' + SRC_URL + ' --> ' + DST_URL +') except:' + e)
			sftp.close()
			transport.close()
			answer(args,'SUCCESS','(' + SRC_URL + ' --> ' + DST_URL +')')


def logger_init(logger_name = '',LOG_FILENAME = '',format = ''):
	global logger, handler
	if os.path.exists(LOG_FILENAME):
		os.remove(LOG_FILENAME)
	# Set up a specific logger with our desired output level
	logger = logging.getLogger(logger_name)
	logger.setLevel(logging.DEBUG)
	# Add the log message handler to the logger
	handler = logging.handlers.RotatingFileHandler(LOG_FILENAME)
	formatter = logging.Formatter(format)
	handler.setFormatter(formatter)
	logger.addHandler(handler)

def parse_url(url):
	mo = re.match('^gsiftp\:\/\/([^\/]+)\/(.*)$',url)
	if mo:
		return mo.groups()
	else:
		mo = re.match('^file\:\/\/(.*)$',url)
		return None,mo.groups()[0]

def set_home_dir(str):
	if re.match('^~',str):
		return '.' + str[1:]
	elif re.match('^[^/]',str):
		return '/' + str[1:]
	else:
		return str

def answer(args,RESULT,INFO):
	OPERATION,JID,TID,EXE_MODE,SRC_URL,DST_URL = args
	out = OPERATION + ' ' + JID + ' ' + TID + ' ' + RESULT + ' ' + INFO
	print_stdout(out)
	logger.debug(out)

def print_stdout(text):
	sys.stdout.write(text + '\n')
	sys.stdout.flush()

def main():

	parser=OptionParser(description="Information manager MAD",version="1.0",usage="Usage: gw_im_mad_static.py")
	MAD_TM = GwTmMad()
	MAD_TM.processLine()
	return 0

if __name__ == '__main__':
	main()

