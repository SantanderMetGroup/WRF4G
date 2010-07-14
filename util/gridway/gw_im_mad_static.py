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

import sys
import os
from optparse import OptionParser


class GwImMad:
	""" Information manager MAD 
	
	The format to send a request to the Information MAD, through its standard input, is: 
	
		OPERATION HID HOST ARGS
		
	Where:
	-OPERATION: Can be one of the following:
		-INIT: Initializes the MAD (i.e. INIT - - -).
		-DISCOVER: Discovers hosts (i.e. DISCOVER - - - ).
		-MONITOR: Monitors a host (i.e. MONITOR HID HOST -).
		-FINALIZE: Finalizes the MAD (i.e. FINALIZE - - -).
	-HID: if the operation is MONITOR, it is a host identifier, chosen by GridWay. Otherwise it is ignored.
	-HOST: If the operation is MONITOR it specifies the host to monitor. Otherwise it is ignored.
			
	The format to receive a response from the MAD, through its standard output, is:
	    
	    OPERATION HID RESULT INFO
	    
	Where:
	-OPERATION: Is the operation specified in the request that originated the response.
	-HID: It is the host identifier, as provided in the submission request.
	-RESULT: It is the result of the operation. Could be SUCCESS or FAILURE.
	-INFO: If RESULT is FAILURE, it contains the cause of failure. Otherwise, if OPERATION 
		is   DISCOVER, it contains a list of discovered host, or if OPERATION is MONITOR, 
		it contains a list of host attributes.	"""


   def __init__(self,hostlist):
		self.hostlist = hostlist
		self.host = {} #hostname:hostfile

	def Menu(self):
		while True:
			input = sys.stdin.readline().split()
			f= open("/tmp/im_carlos",'a')
			str = " ".join(input)
			f.write(str + '\n')
			f.close()
			if len(input) == 4:
				OPERATION,HID,HOST,ARGS = input
				if OPERATION == 'INIT':
					self.__INIT(input)
				elif OPERATION == 'DISCOVER':
					self.__DISCOVER(input)
				elif OPERATION == 'MONITOR':
					self.__MONITOR(input)
				elif OPERATION == 'FINALIZE':
					self.__FINALIZE(input)
				else:
					print_stdout('wrong command')
			else:
				print_stdout('incorrect number of arguments')

     def __INIT(self, args):
		"""Initializes the MAD (i.e. INIT - - -)"""
		OPERATION,HID,HOST,ARGS = args
		print_stdout(OPERATION + ' ' + HID + ' ' + 'SUCCESS' + ' ' + ARGS)

	def __DISCOVER(self, args):
		"""Discovers hosts (i.e. DISCOVER - - -)"""
		OPERATION,HID,HOST,ARGS = args
		if os.path.exists(self.hostlist):
			try:
				f = open(self.hostlist,'r')
			except IOError:
				print_stdout(OPERATION + ' ' + HID + ' ' + 'FAILURE' + ' ' + 'Can\'t open ' + self.hostlist + ' file')
			else:
				for line in f.readlines():
					self.host[line.split()[0]] = line.split()[1]
					f.close()
					host = " ".join([host for host in self.host.keys()])
					print_stdout(OPERATION + ' ' + HID + ' ' + 'SUCCESS' + ' ' + host)
		else:
			print_stdout(OPERATION + ' ' + HID + ' ' + 'FAILURE' + ' ' + 'Can\'t access ' + self.hostlist + ' file')

	def __MONITOR(self, args):
		"""Monitors a host (i.e. MONITOR HID HOST -) """
		OPERATION,HID,HOST,ARGS = args
		if  self.host.has_key(HOST):
			path_host = os.environ['GW_LOCATION'] + '/' + self.host[HOST]
			if os.path.exists(path_host):
				try:
					f = open(path_host,'r')
				except IOError:
					print_stdout(OPERATION + ' ' + HID + ' ' + 'FAILURE' + ' ' + 'Can\'t open ' + path_host + ' file')
				else:
					data = " ".join(" ".join(f.readlines()).split())
					f.close()
					print_stdout(OPERATION + ' ' + HID + ' ' + 'SUCCESS' + ' ' + data)
			else:
				print_stdout(OPERATION + ' ' + HID + ' ' + 'FAILURE' + ' ' + 'Can\'t access ' + path_host + ' file')
		else:
			print_stdout(OPERATION + ' ' + HID + ' ' + 'FAILURE' + ' ' + 'There ' + HOST)

	def __FINALIZE(self, args):
		"""Finalizes the MAD (i.e. FINALIZE - - -)"""
		OPERATION,HID,HOST,ARGS = args
		print_stdout(OPERATION + ' ' + HID + ' ' + 'SUCCESS' + ' ' + ARGS)
		sys.exit(0)

def print_stdout(text):
	sys.stdout.write(text + '\n')
	sys.stdout.flush()
	f = open("/tmp/im_carlos",'a')
	f.write(text + '\n')
	f.close()

def main():
	
	if not os.environ['GW_LOCATION']:
		print_stdout('Please, set GW_LOCATION variable')
		sys.exit(-1)

	parser=OptionParser(description="Information manager MAD",version="1.0",usage="Usage: gw_im_mad_static.py: [-s SERVER] [-b BASE] [-f HOSTFILTER] [-q QUEUEFILTER] [-l HOSTLIST] [-d DEBUGGER]")

	parser.add_option('--SERVER','-s')
	parser.add_option('--BASE','-b')
	parser.add_option('--HOSTFILTER','-f')
	parser.add_option('--QUEUEFILTER','-q')
	parser.add_option('--HOSTLIST','-l')
	(options, args) = parser.parse_args()

	if len(args) != 0:
		print_stdout('incorrect number of arguments')
		sys.exit(-1)
	elif options.HOSTLIST:
		options.HOSTLIST = os.environ['GW_LOCATION'] + '/' + options.HOSTLIST.strip()
	else:
		print_stdout('option not implemented')
		sys.exit(-1)

    MAD_IM = GwImMad(options.HOSTLIST)
    MAD_IM.Menu()
    return 0

if __name__ == '__main__':
	main()

