from datetime  import datetime, timedelta

import re
import calendar

__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

def datewrf2datetime( date_wrf ):
    mo = re.match("(\d{4})-(\d{2})-(\d{2})_(\d{2}):(\d{2}):(\d{2})", date_wrf )
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

class Calendar( object ):
    """
    Class to manage calendars like 'standard' and 'leap' 
    """
    def __init__( self, type = 'standard' ):
        if not type in [ 'standard', 'leap' ] :
            raise Exception( "Calendar type not available" )
        else :
            self.type = type

    def sub(self, date1, date2 ):
        """
        Subtract two dates returning a timedelta object
        """
        if self.type == 'leap' :
            diff_days = self._no_leap_day( date1 ) - self._no_leap_day( date2 )
            return timedelta( days=diff_days )
        else :
            return date1 - date2

    def add_hours(self, date, hours):
        """
        Add hours to a date returning a datetime object
        """
        if self.type == 'leap' and calendar.isleap( date.year ) :
            date_add = date + timedelta(hours=hours)
            day_29   = datetime( date.year, 2 , 29 )
            if ( date < day_29 ) and ( day_29 < date_add ) :
               hours_to_add = 24
            else :
               hours_to_add = 0
            return date + timedelta(hours=hours+hours_to_add)
        else :
            return date + timedelta(hours=hours)

    def sub_hours(self, date, hours):
        """
        Subtract hours to a date returning a datetime object
        """
        if self.type == 'leap' and calendar.isleap( date.year ) :
            date_sub = date - timedelta(hours=hours)
            day_29   = datetime( date.year, 2 , 29 )
            if ( date > day_29 ) and ( day_29 > date_sub ) :
               hours_to_add = 24
            else :
               hours_to_add = 0
            return date + timedelta(hours=hours-hours_to_add)
        else :
            return date + timedelta(hours=hours)

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

