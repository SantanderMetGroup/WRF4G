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


module DAGMan

	# DAGMan node definition
	class Node
		# Instance variables that can be accessed from the outside
		attr_accessor :name, :template, :parents, :children
		attr_accessor :pre, :post, :gridway, :done, :vars
		
		# Creates a new node specifying the name of the node and the
		# filename of the template
		def initialize(name=nil, template=nil)
			@name=name
			@template=template
			@parents=Array.new
			@children=Array.new
			@done=false
			@gridway=GridWay.new(@template)
			
			@vars={
				'$JOB' => @name,
				'$RETURN' => 0
			}
		end
		
		# Adds a new parent node if it does not exist
		def add_parent(node)
			if !@parents.include?(node)
				@parents<<node
				node.add_child(self)
			end
		end
		
		# Adds a new child node if it does not exist
		def add_child(node)
			if !@children.include?(node)
				@children<<node
				node.add_parent(self)
			end
		end
		
		# Returns true if the node did not run and all the parents
		# are already done
		def runable?
			if done?
				return false
			else
				if !@parents.find {|parent| !parent.done? }
					return true
				else
					return false
				end
			end
		end
		
		# Job id given by gridway
		def job_id
			@gridway.job_id
		end
		
		# Returns true if the job is already finished.
		# The instance variable @done can be set to true so you can skip
		# jobs already launched previously
		def done?
			if @done
				true
			else
				@gridway.done?
			end
		end
		
		# Expands variables set by the job
		def expand_vars(str)
			expanded=str.clone
			@vars.each{|key, val|
				expanded.gsub!(key, val.to_s)
			}
			return expanded
		end
	end # class

end # module
