# This Python file uses the following encoding: utf-8
#!/usr/bin/python -i

#Irene Canales Fernández
#July 2010

from clase_ce import *
from ces.cesapp.models import Ce



name="ce8"
vo="esr"
ce1=Ce_database(name,vo)

Ce_db=Ce()
Ce_db.name=ce1.name
Ce_db.vo=ce1.vo
Ce_db.save()




