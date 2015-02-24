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
	
	class GridWay
		attr_accessor :job_id, :template, :state, :info
		
		# Wait for jobs specified in the array
		def self.wait(job_ids)
			ids=job_ids.collect{|id| id.to_s.strip }.join(' ')
			`gwwait -k -a #{ids}`
		end
		
		def initialize(template)
			@template=template
			@state=nil
		end
		
		# Submit the job to gridway and keep the job_id. (It is also
		# returned)
		def submit
			submit_out=`gwsubmit -v #{@template}`
			exit_code=$?
			if !submit_out or exit_code!=0
				return false
			end
			
			@job_id=submit_out.split(':')[1].strip
		end
		
		# Actualizes information for the job. Also sets state value
		def get_info
			ps_out=`gwps -f #{@job_id}`
			exit_code=$?
			if !ps_out or exit_code!=0
				return false
			end
			
			@info=Hash.new
			ps_out.each{|line|
				(name, val)=line.split('=')
				name.strip! if name
				val.strip! if val
				@info[name]=val if name
			}
			@state=@info['JOB_STATE']
		end
		
		# Kills the job
		def kill
			`gwkill #{@job_id}`
		end
		
		# Returns true if the job is done
		def done?
			if @state
				@state=='done'
			else
				false
			end
		end
		
	end # class
	
end # module

