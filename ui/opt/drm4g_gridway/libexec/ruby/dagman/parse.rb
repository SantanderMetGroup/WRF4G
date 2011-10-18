# --------------------------------------------------------------------------
# Copyright 2002-2011, GridWay Project Leads (GridWay.org)          
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may
# not use this file except in compliance with the License. You may obtain
# a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# --------------------------------------------------------------------------


require 'dagman/node'

module DAGMan
	
	# Parses a dagman file
	class Parse
		attr_reader :jobs, :dependencies, :scripts
		
		JOB_REG=/^\s*JOB\s+(\S+)\s+(\S+)(\s+(DONE))?/i
		SCRIPT_REG=/^\s*SCRIPT\s+(PRE|POST)\s+(\S+)\s+(\S+)\s+(.*)/i
		PARENT_REG=/^\s*PARENT\s+(.*?)CHILD\s+(.*)/i
		
		def initialize(filename)
			@jobs=Hash.new
			@dependencies=Hash.new
			@scripts=Hash.new
			
			f=File.open(filename, 'r')
			f.each {|line|
				parse(line)
			}
		end
		
		def parse(line)
			case line
			when JOB_REG
				parse_job(line)
			when SCRIPT_REG
				parse_script(line)
			when PARENT_REG
				parse_parent(line)
			else
				# Do nothing
			end
		end
		
		def parse_job(line)
			res=JOB_REG.match(line)
			job_name=res[1]
			job_template=res[2]
			
			job=Node.new(job_name, job_template)
			
			# Mark as already completed if DONE is specified
			job.done=true if res[3]
			
			# Make names all uppercase so they are case insensitive
			@jobs[job_name.upcase]=job
		end
		
		def parse_script(line)
			res=SCRIPT_REG.match(line)
			script_type=res[1]
			script_job=res[2].upcase
			script_name=res[3]
			script_params=res[4]
			@scripts[script_job]=Hash.new if !@scripts[script_job]
			@scripts[script_job][script_type]=[script_name, script_params]
		end
		
		def parse_parent(line)
			res=PARENT_REG.match(line)
			parents=res[1].split(/\s/).compact
			children=res[2].split(/\s/).compact
			
			# Make everything upcase
			parents.each{|parent| parent.upcase! }
			children.each{|child| child.upcase! }

			children.each{|child|
				if !@dependencies[child]
					@dependencies[child]=parents
				else
					@dependencies[child]+=parents
				end
				@dependencies[child].sort!.uniq!
			}
		end
		
		# After parsing the file this function adds the information about
		# dependencies and pre/post scripts to the nodes
		def merge_data
			@dependencies.each{|child, parents|
				parents.each{|parent|
					@jobs[child].add_parent(@jobs[parent])
				}
			}
			@scripts.each{|job, scripts|
				@jobs[job].pre=scripts['PRE']
				@jobs[job].post=scripts['POST']
			}
		end
		
	end # class
	
end # module
