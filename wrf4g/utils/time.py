#
# Copyright 2016 Universidad de Cantabria
#
# Licensed under the EUPL, Version 1.1 only (the
# "Licence");
# You may not use this work except in compliance with the
# Licence.
# You may obtain a copy of the Licence at:
#
# http://ec.europa.eu/idabc/eupl
#
# Unless required by applicable law or agreed to in
# writing, software distributed under the Licence is
# distributed on an "AS IS" basis,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
# express or implied.
# See the Licence for the specific language governing
# permissions and limitations under the Licence.
#

import re
import calendar
from datetime                import datetime, timedelta
from dateutil.relativedelta  import relativedelta

def datewrf2datetime( date_wrf ):
    mo = re.match("(\d{4})-(\d{2})-(\d{2})_(\d{2})[:_](\d{2})[:_](\d{2})", date_wrf )
    if not mo :
        raise Exception("Date is not well formed")
    date_tuple  = mo.groups()
    date_object = datetime( *tuple( map( int, date_tuple ) ) )
    return date_object

def dateiso2datetime( date_iso ):
    mo = re.match("(\d{4})(\d{2})(\d{2})T(\d{2})(\d{2})(\d{2})Z", date_iso )
    if not mo :
        raise Exception("Date is not well formed")
    date_tuple  = mo.groups()
    date_object = datetime( *tuple( map( int, date_tuple ) ) )
    return date_object

def datetime2datewrf( date_object ):
    return date_object.strftime("%Y-%m-%d_%H:%M:%S")

def datetime2dateiso( date_object ):
    return date_object.strftime("%Y%m%dT%H%M%SZ")

def str2timedelta( input_str ):
    keys = [ "years", "months", "days", "hours" ]
    match = False
    for key in keys :
        if key in input_str :
            match = True
    if not match :
        raise Exception( "'%s' does not contain an argument. Please add an argument may be years, months, days, or hours." % input_str ) 
    regex = "".join(["((?P<%s>\d+)\s*%s ?)?" % (k, k[0]) for k in keys])
    kwargs = {}
    for key, val in re.match( regex, input_str ).groupdict( default = "0" ).items():
        kwargs[ key ] = int( val )
    return relativedelta( **kwargs )

def timedelta_total_seconds( td ):
    try :
        return (td.microseconds + (td.seconds + td.hours * 3600 + td.days * 24 * 3600) * 10**6) / 10**6     
    except :
        return (td.microseconds + (td.seconds + td.days * 24 * 3600) * 10**6) / 10**6

class Calendar( object ):
    """
    Class to manage calendars like 'standard' and 'no_leap' 
    """
    available_types = ( 'standard', 'no_leap' )

    def __init__( self, type = 'standard' ):
        if not type in self.available_types :
            raise Exception( "Calendar type not available" )
        else :
            self.type = type

    def sub_dates(self, date1, date2 ):
        """
        Subtract two dates returning a timedelta object
        """
        if self.type == 'no_leap' :
            diff_days = self._no_leap_day( date1 ) - self._no_leap_day( date2 )
            return relativedelta( days=diff_days )
        else :
            return date1 - date2

    def add(self, date, trelativedelta):
        """
        Add time to a date returning a datetime object
        """
        total_time = date + trelativedelta
        if self.type == 'no_leap' :
            hours_to_add = 0
            for year in range( date.year, total_time.year + 1 ) :
                if calendar.isleap( year ) : 
                    if date < datetime( year, 2, 29 ) < total_time :
                        hours_to_add += 24
            total_time += relativedelta( hours = hours_to_add ) 
        return total_time

    def sub(self, date, trelativedelta):
        """
        Subtract time to a date returning a datetime object
        """
        total_time = date - trelativedelta
        if self.type == 'no_leap' :
            hours_to_sub = 0
            for year in range( date.year, total_time.year + 1 ) :
                if calendar.isleap( year ) :     
                    if date < datetime( year, 2, 29 ) < total_time :
                        hours_to_sub -= 24
            total_time -= relativedelta( hours = hours_to_sub )
        return total_time

    def _no_leap_day(self, date):
        year        = date.year
        month       = date.month
        day         = date.day
        hour        = date.hour
        minute      = date.minute
        second      = date.second
        microsecond = date.microsecond
        # Convert time to fractions of a day
        day = day + hour / 24.0 + minute / 1440.0 + (second + microsecond/1.e6) / 86400.0
        # Start Meeus algorithm 
        if month < 3 :
            month = month + 12
            year = year - 1
        day = int(365.0 * (year + 4716)) + int(30.6001 * (month + 1)) + day - 1524.5
        return day

