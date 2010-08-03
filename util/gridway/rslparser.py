#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#       rslparser.py
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

import re

class RSLParser:
	
	""" Parser of RSL2 files. It is used like this:
	
	rsl=RSLParser.new(string_containing_the_rsl)
	rsl.parse
	
	rsl object will then have this information:
	* executable
	* stdout
	* stderr
	* directory
	* count
	* jobType
	* environment(is a hash with env variable names as keys)
	
	This information can be accessed using methods with the same name:
	
	stdout=rsl.stdout """
	
	def __init__(self,str):
		self.str = str #str contains rsl to parse
		self.values = {}
	
	def parseValue(self,name):
		"""Gets data from <job><name></name></job> and adds it 
			to self.values"""
		m =re.search('<' + name + '>(.*)</' + name + '>',self.str)
		if m:
			self.values[name] =  m.group(1)
	
	def parseEnvironmment(self):
		"""	Gets all the data enclosed in <job><environment>...
			</environment></job> and adds it to self.values"""
		m1 = re.findall('<environment>(.*)</environment>',self.str)
		if len(m1) == 1:
			values = []		
			for parser_env in m1[0].split('</environment><environment>'):
				m2 =re.search('<name>(.*)</name> <value>(.*)</value>', parser_env)
				if m2:
					values.append(m2.groups())
			self.values['environment'] = values	
			
	def parser(self):
		[self.parseValue(name) for name in ['executable','stdout','stderr','directory','count','jobType']]
		self.parseEnvironmment()
	
	def stdout(self):
		return self.values
