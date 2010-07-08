# This is an auto-generated Django model module.
# You'll have to do the following manually to clean this up:
#     * Rearrange models' order
#     * Make sure each model has one field with primary_key=True
# Feel free to rename the models, but don't rename db_table values or field names.
#
# Also note: You'll have to insert the output of 'django-admin.py sqlcustom [appname]'
# into your database.

from django.db import models

class InfoCheckCe(models.Model):
    #id_check_ce = models.AutoField(primary_key=True)
    #id_check_ce = models.IntegerField(primary_key=True)
    id_ce = models.IntegerField()
    id_job = models.CharField(max_length=300, blank=True)
    time = models.DateTimeField()
    state = models.CharField(max_length=300, blank=True)
    class Meta:
        db_table = u'Info_Check_Ce'

class InfoCheckSe(models.Model):
    id_check_se = models.IntegerField(primary_key=True)
    id_ce = models.IntegerField()
    name_se = models.CharField(max_length=300, blank=True)
    id_job = models.CharField(max_length=300, blank=True)
    time = models.DateTimeField()
    tup = models.TextField(blank=True) # This field type is a guess.
    nup = models.IntegerField(null=True, blank=True)
    tdown = models.TextField(blank=True) # This field type is a guess.
    ndown = models.IntegerField(null=True, blank=True)
    state = models.CharField(max_length=300, blank=True)
    class Meta:
        db_table = u'Info_Check_Se'

class InfoDinamicCe(models.Model):
    id_dinamic_ce = models.IntegerField(primary_key=True)
    id_ce = models.IntegerField()
    id_job = models.CharField(max_length=300, blank=True)
    time = models.DateTimeField()
    state = models.CharField(max_length=300, blank=True)
    memory = models.IntegerField(null=True, blank=True)
    node_cpu = models.IntegerField(null=True, blank=True)
    mem_cpu = models.IntegerField(null=True, db_column='mem_CPU', blank=True) # Field name made lowercase.
    model = models.CharField(max_length=300, blank=True)
    class Meta:
        db_table = u'Info_Dinamic_Ce'

class Ce(models.Model):
    #id_ce = models.IntegerField(primary_key=True)
    name = models.CharField(max_length=300, blank=True)
    cpus = models.IntegerField(null=True, blank=True)
    lrms = models.CharField(max_length=150, blank=True)
    memlibre_memtotal = models.CharField(max_length=150, blank=True)
    closese = models.CharField(max_length=300, db_column='closeSE', blank=True) # Field name made lowercase.
    class Meta:
        db_table = u'ce'

