#!/usr/bin/python -i

from sys import stderr,exit
import MySQLdb
import os
import rlcompleter
import readline
from re import search
readline.parse_and_bind("tab: complete")
#sys.path.append("/oceano/gmeteo/users/valva/python/MySQL-python/") 


class vdb:
   """ vdb CLASS
   """
       

   def __init__(self,host="ui01.macc.unican.es",user="gridway",db="cam",port=13306,passwd="ui01"):
   # connect
     try:
       self.con = MySQLdb.connect(host="ui01.macc.unican.es", user="root",db="WRF4GDB",port=13306,passwd="anjanas")
      # db = MySQLdb.connect('host'=host, 'user'=user,'db'=db,'port'=port,'passwd'=passwd)
     except MySQLdb.Error, e:
       print "Error %d: %s" % (e.args[0], e.args[1])
       exit(9)
     

   def insert(self,table,data,verbose=False):
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
        v=str(data.values())[1:-1]
        query="INSERT INTO %s (%s) VALUES (%s)" % (table, ','.join(data.keys()), v)
        if verbose:
           stderr.write(query + "\n")
        cursor.execute(query)
        cursor.execute("SELECT LAST_INSERT_ID()")
        result = cursor.fetchall()
 
      except MySQLdb.Error, e:
        print "Error %d: %s" % (e.args[0], e.args[1])
        exit(8)

      self.con.commit()
      self.con.close()  
      id=list_query().one_field(result)
      return id

   def select(self,fields,table,condition='1=1',verbose=False):
      """ SELECT fields FROM table WHERE condition
      Returns a dictionary with the name of the fields and their contents. 
      
      Example:
      c=vdb()
      output = c.select("n_realization_experiment,UI_experiment","realization","id_experiment='prueba1'" )
            
      SELECT n_realization_experiment FROM experiment WHERE id_experiment='prueba1'     
 
      output => ({'n_realization_experiment': 30L},)
      """
      # create a cursor
      cursor = self.con.cursor (MySQLdb.cursors.DictCursor)
      
      # execute SQL INSERT statement
      try:
        query="SELECT %s FROM %s WHERE %s" % (fields, table, condition)
        if verbose:
           stderr.write(query + "\n")
        cursor.execute(query)
        result = cursor.fetchall()

      except MySQLdb.Error, e:
        print "Error %d: %s" % (e.args[0], e.args[1])
        exit(10)
     
      self.con.commit()
      self.con.close()      
      
      # return an array with a dictionary for each entry
      return result

   def update(self,table,data,condition='1=1',verbose=False):
      """ UPDATE table SET data WHERE condition
      UPDATE "table" with the values of the "data" dictionary
      Returns a dictionary with the name of the fields and their contents. 
      Example:

      c=vdb()
      UPDATE realization SET job_active=4,fk_id_realizationstatus=10,date_current_realization=2010-02-10_22:04:36 WHERE id_realization='prueba1_0001'         
      UPDATE experiment SET UI_experiment="mon01.macc.unican.es",VO_experiment="gmeteo" WHERE id_experiment='prueba4'
      """
       # create a cursor
      cursor = self.con.cursor (MySQLdb.cursors.DictCursor)

      # Conversion of the dictionary into a string with the pairs separated by commas.
      val=""
      for field in data.keys():
         val+="%s='%s'," % (field,data[field])
  
      # "val" is a string with comma separated pairs: UI_experiment='sipc18',end_experiment='1998-08-01_00:00:00', 
      # Here, we remove the last comma character
 
      # execute SQL INSERT statement
      try:
        query="UPDATE %s SET %s WHERE %s" % (table, val[:-1], condition)
        if verbose:
           stderr.write(query + "\n")
        cursor.execute(query)

      except MySQLdb.Error, e:
        print "Error %d: %s" % (e.args[0], e.args[1])
        exit(10)
     
      self.con.commit()
      self.con.close()           

class list_query:
   """This class receive a list from a select query and format it. 
      Depending the format the user wants return one value or other
      Example: val=list().single(out)
   """

   def one_field(self,result,interpreter='bash'):
      """From the result of a query in list format, get the value of the
      first column and format it for bash or python
      """
      if interpreter == 'bash':
        lista=""
        for dicti in result:
            if len(lista)== 0 : lista=str(dicti.values()[0]) 
            else:               lista=str(dicti.values()[0]) + " " + lista
      else:
        lista=[]
        for dicti in result:
            lista.append(dicti.values()[0])
      return lista  

   def random(self,result,interpreter='bash',tries=3):
      """From the result of a query in list format, get the value of the
      first column and give it back in random order and with the number
      of elements indicated in tries
      """
      from random import choice
      lista=[]
      for dicti in result:
          lista.append(dicti.values()[0])
      i=0
      rlista=[]

      # Find if there is a replica in the same site. It there is not 
      # download aleatory.

      for e in lista:
         ce = os.getenv("GW_HOSTNAME")
         if ce is not None and search("%s/" %ce[-4:], e):
	     lista.remove(e)
             lista.insert(0,e)
             rlista.append(e)
             tries=tries-1

      while lista != [] and i<tries:
         elem = choice(lista)
         lista.remove(elem)
         i=i+1
         rlista.append(elem)
      return rlista  




if __name__ == "__main__":
   usage="""%prog [OPTIONS] SOURCE DEST
   
   Example: %prog -r -p 10 /tmp/prueba gsiftp://se01.macc.unican.es/tmp/valva
   
   URL examples:
   LFC              lfn://vo.prod.eela-eu.eu@computer:/grid/valva
   GRIDTP           gridftp://computer:2812/grid/valva
   RSYNC            rsync://valva@computer:34/grid/valva
   SIMBOLIC LINK    ln:///valva or ln:valva
   FILE             valva
   """
   

   
   a=vdb()

   data={'id_experiment': 'prueba4', 'n_realization_experiment':'2'} 
   output = a.select("url","files","vfn=\'dir_cam_out\'",verbose=False )
   print output
   print list_query().random(output,'p',tries=1)
