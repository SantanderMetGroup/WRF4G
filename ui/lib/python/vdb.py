#!/usr/bin/env python

from sys import stderr, exit, path, version_info
import os

try:
    path.insert(0, os.path.dirname(__file__))
    import pymysql as MySQLdb
    MySQLdb.install_as_MySQLdb() 
except Exception, e:
    print 'Caught exception: %s: %s' % (e.__class__, str(e))
    print 'Please check that LD_LIBRARY_PATH includes the directory $WRF4G_LOCATION/lib/shared_libs'
    exit(-1)
        
# there is a bug with rlcompleter in Centos 6. When used, the $PS1 (in our case \033[?1034h$) is writen into stdout
from re import search
from optparse import OptionParser


def parse_one_field(dict):
    return  dict[0].values()[0]

def parse_one_list(result, interpreter='bash'):
      """From the result of a query in list format, get the value of the
      first column and format it for bash or python
      """
      if interpreter == 'bash':
        lista = ""
        for dicti in result:
            if len(lista) == 0 : lista = str(dicti.values()[0]) 
            else:               lista = str(dicti.values()[0]) + " " + lista
      else:
        lista = []
        for dicti in result:
            lista.append(dicti.values()[0])
      return lista  


class vdb:
   """ vdb CLASS
   """
       

   def __init__(self, host="ui01.macc.unican.es", user="gridway", db="cam", port=13306, passwd="ui01"):
     try:
       self.con = MySQLdb.connect(host=host, user=user, db=db, port=port, passwd=passwd)
     except MySQLdb.Error, e:
       print "Error %d: %s" % (e.args[0], e.args[1])
       exit(9)

   def close(self):
       self.con.close()
       
   def commit(self):
       self.con.commit()
       
   def rollback(self):
       self.con.rollback()

   def insert(self, table, data, verbose=False):
      """ INSERT INTO table (data(keys)) VALUES(data(values))
      Insert in "table" the values from the "data" dictionary
      c=vdb()
      c.insert("experiments",{'id_experiment': 'prueba1', 'n_realization_experiment':'2'} )
      INSERT INTO experiment (id_experiment,n_realization_experiment) VALUES('prueba1','2')
      """
      
      # create a cursor
      cursor = self.con.cursor (MySQLdb.cursors.DictCursor)
      
      # execute SQL INSERT statement
      try:
        v = str(data.values())[1:-1]
        query = "INSERT INTO %s (%s) VALUES (%s)" % (table, ','.join(data.keys()), v)
        if verbose:
           stderr.write(query + "\n")
        cursor.execute(query)
        cursor.execute("SELECT LAST_INSERT_ID()")
        result = cursor.fetchall()
 
      except MySQLdb.Error, e:
        print "Error %d: %s" % (e.args[0], e.args[1])
        exit(8)

      
      id = list_query().one_field(result)
      return id

   def select(self, table, data, condition='1=1', verbose=False):
      """ SELECT fields FROM table WHERE condition
      Returns a dictionary with the name of the fields and their contents. 
      
      Example:
      c=vdb()
      output = c.select("realization","n_realization_experiment,UI_experiment","id_experiment='prueba1'" )
            
      SELECT n_realization_experiment FROM experiment WHERE id_experiment='prueba1'     
 
      output => ({'n_realization_experiment': 30L},)
      """
      # create a cursor
      cursor = self.con.cursor (MySQLdb.cursors.DictCursor)
      
      # execute SQL INSERT statement
      try:
        query = "SELECT %s FROM %s WHERE %s" % (data, table, condition)
        if verbose:
           stderr.write(query + "\n")
        cursor.execute(query)
        result = cursor.fetchall()

      except MySQLdb.Error, e:
        print "Error %d: %s" % (e.args[0], e.args[1])
        exit(10)
     
      
   
      
      # return an array with a dictionary for each entry
      return result

   def update(self, table, data, condition='1=1', verbose=False):
      """ UPDATE table SET data WHERE condition
      UPDATE "table" with the values of the "data" dictionary
      Example:

      c=vdb()
      UPDATE realization SET job_active=4,fk_id_realizationstatus=10,date_current_realization=2010-02-10_22:04:36 WHERE id_realization='prueba1_0001'         
      UPDATE experiment SET UI_experiment="mon01.macc.unican.es",VO_experiment="gmeteo" WHERE id_experiment='prueba4'
      """
       # create a cursor
      cursor = self.con.cursor (MySQLdb.cursors.DictCursor)

      # Conversion of the dictionary into a string with the pairs separated by commas.
      val = ""
      for field in data.keys():
          if data[field] != None:
              val += "%s='%s'," % (field, data[field])
          else:
              val += "%s=%s," % (field, 'NULL')
          
         
  
      # "val" is a string with comma separated pairs: UI_experiment='sipc18',end_experiment='1998-08-01_00:00:00', 
      # Here, we remove the last comma character
 
      # execute SQL INSERT statement
      try:
        query = "UPDATE %s SET %s WHERE %s" % (table, val[:-1], condition)
        if verbose:
           stderr.write(query + "\n")
        cursor.execute(query)

      except MySQLdb.Error, e:
        print "Error %d: %s" % (e.args[0], e.args[1])
        exit(10)
     
                
      return 0
  
   def delete_row(self,table,condition,verbose=False):
      """ DELETE FROM table WHERE condition
          DELETE FROM WRF4GDB.Experiment WHERE `id`='16';   
      """
       # create a cursor
      cursor = self.con.cursor (MySQLdb.cursors.DictCursor)

      try:
        query = "DELETE FROM %s WHERE %s" % (table, condition)
        if verbose:
           stderr.write(query + "\n")
        cursor.execute(query)

      except MySQLdb.Error, e:
        print "Error %d: %s" % (e.args[0], e.args[1])
        exit(10)
     
                
      return 0

   def describe(self,table,verbose=False):
       cursor = self.con.cursor (MySQLdb.cursors.DictCursor)
       
      # execute SQL INSERT statement
       try:
        query = "DESCRIBE %s" % table
        if verbose:
           stderr.write(query + "\n")
        cursor.execute(query)
        result = cursor.fetchall()
 
       except MySQLdb.Error, e:
        print "Error %d: %s" % (e.args[0], e.args[1])
        exit(8)

       
       fields=[]
       for i in result:
           fields.append(i['Field'])
       return fields
      
   def remove_entry(self,table,entry,verbose=False):
       cursor = self.con.cursor (MySQLdb.cursors.DictCursor)
       
      # execute SQL INSERT statement
       try:
        query = "DESCRIBE %s" % table
        if verbose:
           stderr.write(query + "\n")
        cursor.execute(query)
        result = cursor.fetchall()
 
       except MySQLdb.Error, e:
        print "Error %d: %s" % (e.args[0], e.args[1])
        exit(8)
        
       fields=[]
       for i in result:
           fields.append(i['Field'])
       return fields


class list_query:
   """This class receive a list from a select query and format it. 
      Depending the format the user wants return one value or other
      Example: val=list().single(out)
   """

   def one_field(self, result, interpreter='bash'):
      """From the result of a query in list format, get the value of the
      first column and format it for bash or python
      """
      if interpreter == 'bash':
        lista = ""
        for dicti in result:
            if len(lista) == 0 : lista = str(dicti.values()[0]) 
            else:               lista = str(dicti.values()[0]) + " " + lista
      else:
        lista = []
        for dicti in result:
            lista.append(dicti.values()[0])
      return lista  

   def random(self, result, interpreter='bash', tries=3):
      """From the result of a query in list format, get the value of the
      first column and give it back in random order and with the number
      of elements indicated in tries
      """
      from random import choice
      lista = []
      for dicti in result:
          lista.append(dicti.values()[0])
      i = 0
      rlista = []

      # Find if there is a replica in the same site. It there is not 
      # download aleatory.

      for e in lista:
         ce = os.getenv("GW_HOSTNAME")
         if ce is not None and search("%s/" % ce[-4:], e):
	     lista.remove(e)
             lista.insert(0, e)
             rlista.append(e)
             tries = tries - 1

      while lista != [] and i < tries:
         elem = choice(lista)
         lista.remove(elem)
         i = i + 1
         rlista.append(elem)
      return rlista  




if __name__ == "__main__":
   usage = """%prog [OPTIONS] SOURCE DEST
   Example: %prog insert -v experiment id_experiment=nino97,UI_experiment=mon01.macc.unican.es
            %prog update -v experiment id_experiment=exp01 id>8
            %prog select -v experiment id_experiment,UI_experiment id_experiment=\'prueba4\'
            %prog describe -v experiment
         INSERT INTO experiment (UI_experiment,id_experiment) VALUES ('mon01.macc.unican.es', 'nino99')
   """
   
   parser = OptionParser(usage, version="%prog 1.0")
   parser.add_option("-v", "--verbose", action="store_true", dest="verbose", default=False, help="Verbose mode. Explain what is being done")
   
   (options, args) = parser.parse_args()
   

   if len(args) < 2:
    parser.error("Incorrect number of arguments")
    exit(1)
   
   dict={} 
   statement = args[0]
   dict['table'] = args[1]
   dict['verbose']=options.verbose
   
   if statement == "select":
     dict['data'] = args[2]       
   elif statement == "insert" or statement == "update":
     pairs = {}
     for pair in args[2].split(',') :
       if options.verbose:   stderr.write("FIELDS:" + pair + "\n") 
       [field, value] = pair.split('=')
       pairs[field] = value
     dict['data']=pairs
    
   if len(args) >= 4:
       dict['condition']=' '.join(args[3:])
     
   con = vdb()
   o = getattr(con, statement)(**dict)
   
   if statement == "select":
      o = list_query().one_field(o)
   print o



  
