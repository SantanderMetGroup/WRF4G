#! /usr/bin/env python

import sys
from wrf4g_classes import *



class WRF4GDB:

    def __init__(self):

        pass

    def mimetodo(self, arg="ninguno"):

        print "Me has llamado: mimetodo %s" % arg


    #Experiment functions
 
    def get_Exp_Id(self,name):
        """ Get the experiment id from experiment's name """
        exp=Experiment(name)
        exp_id=exp.get_Id()
        return exp_id

    def get_Exp_Name(self,exp_id):
        """ Get the experiment name from experiment's id """
        exp=Experiment_db.objects.get(id=exp_id)
        exp_name=exp.name
        return exp_name


    def get_Exp_Start_Date(self,name):
        """ Get the experiment end date from experiment'w name"""
        exp=Experiment(name)
        exp_start_date=exp.get_Start_Date()
        return exp_start_date


    def get_Exp_End_Date(self,name):
        """ Get the experiment end date from experiment'w name"""
        exp=Experiment(name)
        exp_end_date=exp.get_End_Date()
        return exp_end_date





if __name__ == "__main__":

   w4gdb = WRF4GDB()

   getattr(w4gdb, sys.argv[1])(*sys.argv[2:])

