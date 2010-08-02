#! /usr/bin/env python

import sys
from wrf4g_classes import *



class WRF4GDB:

    def __init__(self):

        pass

    def mimetodo(self, arg="ninguno"):

        print "Me has llamado: mimetodo %s" % arg


    ######### Experiment functions #############

    #         functions:         
    #                get_Exp_Id(exp_id)
    #                get_Exp_Name(name)                  
    #                get_Start_Date(name)
    #                set_Start_Date(name,start_date)
    #                get_End_Date(name)
    #                set_End_Date(name,end_date)
    #                get_Status(name)
    #                set_Status(name,status)
    


 
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
        """ Get the experiment start date from experiment's name"""
        exp=Experiment(name)
        exp_start_date=exp.get_Start_Date()
        return exp_start_date

    def set_Exp_Start_Date(self,name,start_date):
        """ Set the experiment start date from experiment's name"""
        exp=Experiment(name)
        exp_start_date=exp.set_Start_Date(start_date)


    def get_Exp_End_Date(self,name):
        """ Get the experiment end date from experiment's name"""
        exp=Experiment(name)
        exp_end_date=exp.get_End_Date()
        return exp_end_date


    def set_Exp_End_Date(self,name,end_date):
        """ Set the experiment end date from experiment's name"""
        exp=Experiment(name)
        exp_start_date=exp.set_End_Date(end_date)


    def get_Exp_Status(self,name):
        """ Get the experiment status from experiment's name"""
        exp=Experiment(name)
        exp_status=exp.get_Status()
        return exp_status

    def set_Exp_Status(self,name,status):
        """ Set the experiment status from experiment's name"""
        exp=Experiment(name)
        exp_status=exp.set_Status(status)


    		
    # Realization function

    #functions:
                    
    #                get_Rea_Name(rea_id)
    #                get_Rea_Id(name)
    #                get_Start_Date(name)
    #                set_Start_Date(name,start_date)
    #                get_End_Date(name)
    #                set_End_Date(name,end_date)
    #                get_Status(name)
    #                set_Status(name,status)


    def get_Rea_Id(self,name):
        """ Get the realization id from realization's name """
        rea=Realization(name)
        rea_id=rea.get_Id()
        return rea_id

    def get_Rea_Name(self,rea_id):
        """ Get the realization's name from realization's id """
        rea=Realization_db.objects.get(id=rea_id)
        rea_name=rea.name
        return rea_name

     def get_Rea_Start_Date(self,name):
        """ Get the realization's start date from realization's name"""
        rea=Realization(name)
        rea_start_date=exp.get_Start_Date()
        return rea_start_date

    def set_Rea_Start_Date(self,name,start_date):
        """ Set the realization's start date from realization's name"""
        rea=Realization(name)
        rea_start_date=rea.set_Start_Date(start_date)


    def get_Exp_End_Date(self,name):
        """ Get the realization's end date from realization's name"""
        rea=Realization(name)
        rea_end_date=rea.get_End_Date()
        return rea_end_date


    def set_Rea_End_Date(self,name,end_date):
        """ Set the realization's end date from realization's name"""
        rea=Realization(name)
        rea_start_date=rea.set_End_Date()

    def get_Exp_Rea(self,name):
        """ Get the realization's experiment from realization's name"""
        rea=Realization(name)
        rea_exp=rea.get_Exp()
        return rea_exp

    def set_Exp_Rea(self,name,rea):
        """ set the realization's experiment from realization's name"""
        rea=Realization(name)
        rea_exp=rea.set_Exp(rea)
       

    ################ Chunk function ############# 
    #
    #   
    #                get_Ch_Start_Date(name)
    #                set_Ch_Start_Date(name,start_date)
    #                get_Ch_End_Date(name)
    #                set_Ch_End_Date(name,end_date)
    #                get_Ch_Current_Date(name)
    #                set_Ch_Current_Date(name,current_date)
    #                get_Ch_Wps_File(name)
    #                set_Ch_Wps_File(name,wps_file)
    #                get_Ch_Status(name)
    #                set_Ch_Status(name,status)
 

    def get_Ch_Id(self,name):
        """ Get the chunk's id from chunk's name """
        ch=Chunk(name)
        ch_id=ch.get_Id()
        return ch_id

    def get_Ch_Name(self,ch_id):
        """ Get the chunk's name from chunk's id """
        ch=Chunk_db.objects.get(id=ch_id)
        ch_name=ch.name
        return ch_name

     def get_Ch_Start_Date(self,name):
        """ Get the chunk's start date from chunk's name"""
        ch=Chunk(name)
        ch_start_date=ch.get_Start_Date()
        return ch_start_date

    def set_Ch_Start_Date(self,name,start_date):
        """ Set the chunk's start date from chunk's name"""
        ch=Chunk(name)
        ch_start_date=ch.set_Start_Date(start_date)



    def get_Ch_End_Date(self,name):
        """ Get the chunk's end date from chunk's name"""
        ch=Chunk(name)
        ch_end_date=ch.get_Start_Date()
        return ch_end_date

    def set_Ch_End_Date(self,name,end_date):
        """ Set the chunk's end date from chunk's name"""
        ch=Chunk(name)
        ch_end_date=ch.set_End_Date(end_date)


    def get_Ch_Current_Date(self,name):
        """ Get the chunk's current date from chunk's name"""
        ch=Chunk(name)
        ch_current_date=ch.get_Current_Date()
        return ch_current_date

    def set_Ch_Current_Date(self,name,current_date):
        """ Set the chunk's current date from chunk's name"""
        ch=Chunk(name)
        ch_current_date=ch.set_Current_Date(current_date)

    def get_Ch_Wps_File(self,name):
        """ Get the chunk's wps file from chunk's name"""
        ch=Chunk(name)
        ch_wps_file=ch.get_Wps_File()
        return ch_wps_file

    def set_Ch_Wps_File(self,name,wps_file):
        """ Set the chunk's wps file from chunk's name"""
        ch=Chunk(name)
        ch_wps_file=ch.set_Wps_File(wps_file)

    def get_Ch_Status(self,name):
        """ Get the chunk's status from chunk's name"""
        ch=Chunk(name)
        ch_status=ch.get_Status()
        return ch_status
    
    def set_Ch_Status(self,name,status):
        """ Set the chunk's status from chunk's name"""
        ch=Chunk(name)
        ch_wps_status=ch.set_Wps_File(status)



    #File Function

    #     functions:
    #                get_Fi_Id(name)
    #                get_Fi_Name(fi_id)
    #                get_Fi_Type(name)
    #                set_Fi_Type(name,type)
    #                get_Fi_Path(name)
    #                set_Fi_Path(name,path)
    #                get_Fi_Rea(name)
    #                set_Fi_Rea(name,rea)
    #                get_Fi_Start_Date(name)
    #                set_Fi_Start_Date(name,start_date)
    #                get_Fi_End_Date(name)
    #                set_Fi_End_Date(name,end_date)
                 


    def get_Fi_Id(self,name):
        """ Get the file's id from file's name """
        fi=File(name)
        fi_id=fi.get_Id()
        return fi_id

    def get_Fi_Name(self,fi_id):
        """ Get the file's name from file's id """
        fi=File_db.objects.get(id=fi_id)
        fi_name=fi.name
        return fi_name

     def get_Fi_Start_Date(self,name):
        """ Get the file's start date from file's name"""
        fi=File(name)
        fi_start_date=fi.get_Start_Date()
        return fi_start_date

    def set_Fi_Start_Date(self,name,start_date):
        """ Set the file's start date from file's name"""
        fi=File(name)
        fi_start_date=fi.set_Start_Date(start_date)



    def get_Fi_End_Date(self,name):
        """ Get the file's end date from file's name"""
        fi=File(name)
        fi_end_date=fi.get_Start_Date()
        return fi_end_date

    def set_Fi_End_Date(self,name,end_date):
        """ Set the file's end date from file's name"""
        fi=File(name)
        fi_end_date=fi.set_End_Date(end_date)


    def get_Fi_Type(self,name)
        """ Get the file's type from file's name"""
        fi=File(name)
        fi_type=fi.get_Type()
        return fi_type

    def set_Fi_Type(self,name,type_)
        """ Set the file's type from file's name"""
        fi=File(name)
        fi_type=fi.set_Type(type_)
        

    def get_Fi_Path(self,name)
        """ Get the file's path from file's path"""
        fi=File(name)
        fi_path=fi.get_Path()
        return fi_path

    def set_Fi_Path(self,name,path)
        """ Set the file's path from file's name"""
        fi=File(name)
        fi_type=fi.set_Type(path)

    def get_Fi_Rea(self,name)
        """ Get the file's realization from file's name"""
        fi=File(name)
        fi_rea=fi.get_Rea()
        return fi_rea

    def set_Fi_Rea(self,name,rea)
        """ Set the file's realization from file's name"""
        fi=File(name)
        fi_rea=fi.set_Rea(rea)

  







if __name__ == "__main__":

   w4gdb = WRF4GDB()

   getattr(w4gdb, sys.argv[1])(*sys.argv[2:])

