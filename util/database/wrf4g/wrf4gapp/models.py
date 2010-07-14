from django.db import models

# Models for  data base wrf4g

# Class Experiment_db

class Experiment_db(models.Model):
      name=models.CharField(max_length=100)
      end_date=models.DateTimeField() # format datetime.datetime
      start_date=models.DateTimeField() # format datetime.datime
      status=models.CharField(max_length=1)# Options=[D,R,Q,S] Done,Run,Queue,Schedule

# Class Realization_db

class Realization_db(models.Model):
      name=models.CharField(max_length=100)
      exp=models.ForeignKey(Experiment_db)# experiment_id
      end_date=models.DateTimeField() # format datetime.datetime
      start_date=models.DateTimeField() # format datetime.datime

# Class Chunk_db
      
class Chunk_db(models.Model):
      name=models.CharField(max_length=100)
      rea=models.ForeignKey(Realization_db) # realization_id
      end_date=models.DateTimeField() # format datetime.datetime
      start_date=models.DateTimeField() # format datetime.datime
      current_date=models.DateTimeField() # format datetime.datetime
      wps_file=models.BooleanField()# True or False Be Careful!! mysql True=1 False=0
      status=models.CharField(max_length=1)# Options=[D,R,Q,S] Done,Run,Queue,Schedule

# Class File_Type_db

class File_Type_db(models.Model):
      type_file=models.CharField(max_length=3)# Options=[out rst wps]
      freq_h=models.IntegerField()# Options=[3h 6h]
     


# Class File_db

class File_db(models.Model):
      type_file=models.ForeignKey(File_Type_db)
      path=models.CharField(max_length=100)
      rea=models.ForeignKey(Realization_db) # realization_id
      end_date=models.DateTimeField() # format datetime.datetime
      start_date=models.DateTimeField() # format datetime.datime


      
      
