#!/usr/bin/perl

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

use Net::LDAP;

###########
# Discovery
###########

sub dynamic_discover
{
	# Arg1 = BDII
	# Arg2 = QUEUEFILTER
	# Arg3 = TMPFILE
	my($bdii, $QUEUEFILTER, $TMPFILE) = @_;

	open(TMPFILE,">$TMPFILE");

	$ldap = Net::LDAP->new( "$bdii:2170" ) or print TMPFILE "DISCOVER - FAILURE $@\n";
	$mesg = $ldap->bind ;    # an anonymous bind
	$mesg = $ldap->search( # perform a search
				base   => "mds-vo-name=local,o=grid",
				filter => "(&(objectclass=GlueCE)$QUEUEFILTER)"
			);
	my $max = $mesg->count;
	for($i = 0 ; $i < $max ; $i++) 
	{
		my $entry = $mesg->entry($i);
		foreach my $attr ($entry->attributes) 
		{
			my $value = $entry->get_value($attr);
			if ($attr eq "GlueCEUniqueID")
			{
				($host,$trash) = split (":",$value);
				push (@hosts,$host);
			}
		}
	}
	print TMPFILE "DISCOVER - SUCCESS @hosts\n";
	close(TMPFILE);
	$mesg = $ldap->unbind;
}

###########
# Monitor
###########

sub dynamic_monitor
{
	# Arg1 = BDII
	# Arg2 = MID
	# Arg3 = HOST
	# Arg4 = QUEUEFILTER
	# Arg5 = TMPFILE
	my ($bdii, $mid, $host, $QUEUEFILTER, $TMPFILE) = @_;
	
	open(TMPFILE,">$TMPFILE");

	$ldap = Net::LDAP->new( "$bdii:2170" ) or print TMPFILE "MONITOR $mid FAILURE $@\n";
	$mesg = $ldap->bind ;    # an anonymous bind
	# First search
	$mesg = $ldap->search( # perform a search
				base   => "mds-vo-name=local,o=grid",
				filter => "(&(objectclass=GlueCE)(GlueCEInfoHostName=$host)$QUEUEFILTER)"
			);
	my $max = $mesg->count;
	# Default values
	$info = "";
	$queue_nodecount = 0;
	$queue_freenodecount = 0;
	$queue_maxtime = 0;
	$queue_maxcputime = 0;
	$queue_maxcount = 0;
	$queue_maxrunningjobs = 0;
	$queue_maxjobsinqueue = 0;
	for($i = 0 ; $i < $max ; $i++) 
		{
		my $entry = $mesg->entry($i);
		foreach my $attr ($entry->attributes) 
		{
			my $value = $entry->get_value($attr);
			if ($attr eq "GlueCEInfoTotalCPUs")
			{
				$nodecount = $value;
				$queue_nodecount = $value;
			}
			elsif ($attr eq "GlueCEUniqueID")
			{
				($trash,$lrms_name) = split ("/",$value);
				($lrms_name1,$lrms_name2,$trash) = split ("-",$lrms_name);
				$lrms_name = $lrms_name1 . "-" . $lrms_name2;
			}
			elsif ($attr eq "GlueCEInfoLRMSType")
			{
				$lrms_type = $value;
			}
			elsif ($attr eq "GlueCEName")
			{
				$queue_name = $value;
			}
			elsif ($attr eq "GlueCEStateFreeCPUs")
			{
				$queue_freenodecount = $value;
			}
			elsif ($attr eq "GlueCEPolicyMaxWallClockTime")
			{
				$queue_maxtime = $value;
			}
			elsif ($attr eq "GlueCEPolicyMaxCPUTime")
			{
				$queue_maxcputime = $value;
			}
			elsif ($attr eq "GlueCEPolicyMaxTotalJobs")
			{
				$queue_maxjobsinqueue = $value;
			}
			elsif ($attr eq "GlueCEPolicyMaxRunningJobs")
			{
				$queue_maxrunningjobs = $value;
			}
			elsif ($attr eq "GlueCEStateStatus")
			{
				$queue_status = $value;
			}
			elsif ($attr eq "GlueCEPolicyPriority")
			{
				$queue_priority = $value;
			}
			elsif ($attr eq "GlueCEStateWaitingJobs")
			{
				$queue_jobwait = $value;
			}
			elsif ($attr eq "GlueCEAccessControlBaseRule")
			{
				my @values = $entry->get_value($attr);
				$queue_access = "";
				foreach $value(@values)
				{
					($trash,$value) = split (":",$value); 
					$queue_access = "$queue_access:$value:";
				}
			}
		}	
		$info = $info . "QUEUE_NAME[$i]=\"$queue_name\" QUEUE_NODECOUNT[$i]=$queue_nodecount QUEUE_FREENODECOUNT[$i]=$queue_freenodecount QUEUE_MAXTIME[$i]=$queue_maxtime QUEUE_MAXCPUTIME[$i]=$queue_maxcputime QUEUE_MAXJOBSINQUEUE[$i]=$queue_maxjobsinqueue QUEUE_MAXRUNNINGJOBS[$i]=$queue_maxrunningjobs QUEUE_STATUS[$i]=\"$queue_status\" QUEUE_DISPATCHTYPE[$i]=\"batch\" QUEUE_PRIORITY[$i]=\"$queue_priority\" QUEUE_JOBWAIT[$i]=\"$queue_jobwait\" QUEUE_ACCESS[$i]=\"$queue_access\" ";
	}
	
	# Second search
	$mesg = $ldap->search( # perform a search
				base   => "mds-vo-name=local,o=grid",
				filter => "(&(objectclass=GlueHostOperatingSystem)(GlueSubClusterName=$host))"
			);	
	$max = $mesg->count;
	for($i = 0 ; $i < $max ; $i++) 
		{
		my $entry = $mesg->entry($i);
		foreach my $attr ($entry->attributes) 
		{
			my $value = $entry->get_value($attr);
			if ($attr eq "GlueHostOperatingSystemName")
			{
				$os_name = $value;
			}
			elsif ($attr eq "GlueHostOperatingSystemVersion")
			{
				$os_version = $value;
			}
                        elsif ($attr eq "GlueHostArchitecturePlatformType")
                        {
                                $arch = $value;
                        }
			elsif ($attr eq "GlueHostProcessorModel")
			{
				$cpu_model = $value;
			}
			elsif ($attr eq "GlueHostProcessorClockSpeed")
			{
				$cpu_mhz = $value;
			}
			elsif ($attr eq "GlueHostArchitectureSMPSize")
			{
				$cpu_smp = $value;
			}
			elsif ($attr eq "GlueHostMainMemoryRAMSize")
			{
				$free_mem_mb = $value;
				$size_mem_mb = $value;
			}
		}	
	}
	
	$info = "HOSTNAME=\"$host\" ARCH=\"$arch\" NODECOUNT=$nodecount LRMS_NAME=\"$lrms_name\" LRMS_TYPE=\"$lrms_type\" OS_NAME=\"$os_name\" OS_VERSION=\"$os_version\" CPU_MODEL=\"$cpu_model\" CPU_MHZ=$cpu_mhz CPU_SMP=$cpu_smp FREE_MEM_MB=$free_mem_mb SIZE_MEM_MB=$size_mem_mb " . $info . "\n";
	
	$mesg = $ldap->unbind;

	print TMPFILE "MONITOR $mid SUCCESS $info";
	close (TMPFILE);
}

###########
# Main
###########

# Read Arguments
$action = $ARGV[0];
if ($action eq "DISCOVER")
{
	# Arg1 = BDII
	# Arg2 = QUEUEFILTER
	# Arg3 = TMPFILE
	&dynamic_discover($ARGV[1],$ARGV[2],$ARGV[3]);
}
elsif ($action eq "MONITOR")
{
	# Arg1 = BDII
	# Arg2 = MID
	# Arg3 = HOST
	# Arg4 = QUEUEFILTER
	# Arg5 = TMPFILE
	&dynamic_monitor($ARGV[1],$ARGV[2],$ARGV[3],$ARGV[4],$ARGV[5]);
}
else
{
	die "ERROR: MAD invoked incorrectly\n";
}

