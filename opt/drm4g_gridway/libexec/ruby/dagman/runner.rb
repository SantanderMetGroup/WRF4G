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

	# Does the actual running step taking care about the dependencies and
	# pre/post scripts
	class Runner
		attr_accessor :jobs, :to_do, :running, :done
		
		def initialize(jobs)
			@jobs=jobs
			@running=Array.new
			@to_do=Array.new
			@done=Array.new
			
			@jobs.each{|name, job|
				if !job.done?
					@to_do<<job
				end
			}
		end
		
		# Checks for finished jobs and submits the ones that have every
		# requirement satisfied
		def iterate
			check_jobs
			
			done=@running.find_all{|job| job.done? }
			done.each{|job| finish(job) } if done
			
			runable=@to_do.find_all{|job| job.runable? }
			runable.each{|job| run(job) } if runable
		end
		
		# Submits the specified job and its PRE script
		def run(job)
			puts "#{Time.now} Executing #{job.name}"
			if job.pre
				script="#{job.pre[0]} #{job.expand_vars(job.pre[1])}"
				puts "  PRE -> "+script
				`#{script}`
				if $?!=0
					puts "Error executing script!"
					exit(-1)
				end
			end
			puts "  Template -> "+job.template
			
			jid=job.gridway.submit
			if !jid
				puts "Error submitting job #{job.name}"
				exit(-1)
			end
			
			@running<<job
			@to_do.delete(job)
		end
		
		# Run the POST script
		def finish(job)
			puts "#{Time.now} Finalizing #{job.name}"
			
			if job.gridway.state=='failed'
				puts "Job #{job.name} failed!"
				exit(-1)
			end
				
			if job.post
				script="#{job.post[0]} #{job.expand_vars(job.post[1])}"
				puts "  POST -> "+script
				`#{script}`
				if $?!=0
					puts "Error executing script!"
					exit(-1)
				end
			end

			@done<<job
			@running.delete(job)
			
			job.gridway.kill
		end
		
		# Waits until some DAGMAN job finishes and then actualizes its information
		def check_jobs
			if @running.length>0
				GridWay.wait(@running.collect{|job| job.job_id })
			end
			
			@running.each{|job|
				job.gridway.get_info
			}
		end
		
		# True if there are no more jobs to do
		def finished?
			if (@to_do.length + @running.length) > 0
				false
			else
				true
			end
		end
		
	end
	
end
