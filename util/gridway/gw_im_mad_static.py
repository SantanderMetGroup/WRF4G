#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#       gw_im_mad_static.py
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

import cmd
import sys
import os
import debugger
from optparse import OptionParser


class GwImMad(cmd.Cmd):
	""" Information manager MAD 
	The format to receive a response from the MAD, through its standard output, is:
	    
	    OPERATION HID RESULT INFO
	
	Where:
	-OPERATION: Is the operation specified in the request that originated the response.
	-HID: It is the host identifier, as provided in the submission request.
	-RESULT: It is the result of the operation. Could be SUCCESS or FAILURE.
	-INFO: If RESULT is FAILURE, it contains the cause of failure. Otherwise, if OPERATION 
	       is   DISCOVER, it contains a list of discovered host, or if OPERATION is MONITOR, 
	       it contains a list of host attributes.	"""
	
	prompt =''

	def __init__(self,hostlist):
		cmd.Cmd.__init__(self)
		self.hostlist = hostlist
		self.host = {} #hostname:hostfile
		
	def do_INIT(self, args):
		"""Initializes the MAD (i.e. INIT - - -)"""
		debugger.Debugger.SendMessage('INIT - SUCCESS -')
        
	def do_DISCOVER(self, args):
		"""Discovers hosts (i.e. DISCOVER - - -)"""
		if os.path.exists(self.hostlist):
			try:
				f = open(self.hostlist,'r')
			except IOError:
				debugger.SendMessage('DISCOVER - FAILURE Can\'t open ' + hostlist + ' file')
			else:
				for line in f.readlines():
					self.host[line.split()[0]] = line.split()[1]	
				f.close()
				host = " ".join([host for host in self.host.keys()])	
				debugger.SendMessage('DISCOVER - SUCCESS ' + host)
		else:
			debugger.SendMessage('DISCOVER - FAILURE Can\'t access ' + hostlist + ' file')	
	    
	def do_MONITOR(self, args):
		"""Monitors a host (i.e. MONITOR HID HOST -) """
		if args:
			HID = args.split()[0]
			HOST = args.split()[1]
			if  self.host.has_key(HOST):
				path_host = os.environ['GW_LOCATION'] + '/' + self.host[HOST]
				if os.path.exists(path_host):
					try:
						f = open(path_host,'r')
					except IOError:
						debugger.SendMessage('MONITOR ' + HID + ' FAILURE Can\'t open ' +  path_host + 'file')
					else:
						data = " ".join(" ".join(f.readlines()).split())
						f.close()
						debugger.SendMessage('MONITOR ' + HID + ' SUCCESS ' + data)
				else:
					debugger.SendMessage('MONITOR ' + HID + ' FAILURE Can\'t access ' + path_host + ' file')
			else:
				debugger.SendMessage('MONITOR ' + HID + ' FAILURE There ' + HOST)		
    
	def do_FINALIZE(self, args):
		"""Finalizes the MAD (i.e. FINALIZE - - -)"""
		debugger.SendMessage('FINALIZE - SUCCESS -')
		sys.exit(0)		
		

def main():
	
	if not os.environ['GW_LOCATION']:
		print "Please, set GW_LOCATION variable."
		sys.exit(-1)
	
	parser=OptionParser(description="Information manager MAD",version="1.0",usage="Usage: gw_im_mad_static.py: [-s SERVER] [-b BASE] [-f HOSTFILTER] [-q QUEUEFILTER] [-l HOSTLIST] [-d DEBUGGER]")
	
	parser.add_option('--SERVER','-s')
	parser.add_option('--BASE','-b')
	parser.add_option('--HOSTFILTER','-f')
	parser.add_option('--QUEUEFILTER','-q')
	parser.add_option('--HOSTLIST','-l')    
	parser.add_option('--DEBUGGER','-d',default=False,action="store_true")
	(options, args) = parser.parse_args()

	if len(args) != 0:
		parser.error("incorrect number of arguments")      
	elif options.HOSTLIST:
		options.HOSTLIST = os.environ['GW_LOCATION'] + '/' + options.HOSTLIST
	else:
		parser.error("option not implemented")		
	
	debugger.Debugger('/tmp/im_carlos.log',options.DEBUGGER)
	GwImMad(options.HOSTLIST).cmdloop()
	return 0

if __name__ == '__main__':
	main()

