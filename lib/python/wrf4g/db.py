from sqlalchemy                 import ( Column, INTEGER, 
                                         VARCHAR, SMALLINT, 
                                         DATETIME, ForeignKey,
                                         create_engine )
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm             import relationship, sessionmaker
from wrf4g                      import DB4G_CONF
from wrf4g.utils                import VarEnv


__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"


DEFAULT_DB_CONF = """
[DEFAULT]
mysql+pymysql:/wrf4guser:Meteo2011@%(hostname)s:%(port)s/WRF4GDB
"""

def get_session():
    """
    Create a sqlalchemy session to connect with WRF4G database
    """
    db4g_urls = VarEnv( DB4G_CONF ).get_variable( 'URL' )
    # an Engine, which the Session will use for connection
    engine = create_engine( db4g_url )
    # create a configured "Session" class
    Session = sessionmaker(bind = engine)
    # create a Session
    return Session()

Base = declarative_base()
#  WRF4G database is composed of the following tables:
#
#  Experiment
#  Realization
#  Job
#  Jobstatus
#  Chunk
#  User
#  Events
#  FileType
#  File

#  WRF4G classes to manage the database
class UserModel( Base ):

    __tablename__   = 'User'

    id              = Column(u'id',INTEGER(display_width=11), primary_key=True, nullable=False)
    name            = Column(u'name',VARCHAR(length=30))
    CN              = Column(u'CN',VARCHAR(length=200))

class JobStatusModel( Base ):

    __tablename__   = 'Jobstatus'

    # Columns
    id              = Column(u'id',INTEGER(display_width=11), primary_key=True, nullable=False)
    description     = Column(u'description',VARCHAR(length=45))

class ExperimentModel( Base ):

    __tablename__   = 'Experiment'
    
    # Columns
    id              = Column(u'id',INTEGER(display_width=11), primary_key=True, nullable=False)
    name            = Column(u'name',VARCHAR(length=512), nullable=False)
    sdate           = Column(u'sdate',DATETIME())
    edate           = Column(u'edate',DATETIME())
    csize           = Column(u'csize',INTEGER(display_width=11))
    user_id         = Column(u'id_user',INTEGER(display_width=11), ForeignKey(u'User.id')) 
    mult_parameters = Column(u'multiple_parameters',INTEGER(display_width=1))
    mult_dates      = Column(u'multiple_dates',INTEGER(display_width=1)) 
    basepath        = Column(u'basepath',VARCHAR(length=300))
    smul_length_h   = Column(u'simulation_length_h',INTEGER(display_width=11))
    smul_interval_h = Column(u'simulation_interval_h',INTEGER(display_width=11))
    mult_labels     = Column(u'multiparams_labels',VARCHAR(length=1024)) 
    exp_description = Column(u'experiment_description',VARCHAR(length=1024)) 

    # Relationships
    user            = relationship(UserModel)

class RealizationModel( Base ):

    __tablename__   = 'Realization'
    
    # Columns
    id              = Column(u'id',INTEGER(display_width=11), primary_key=True, nullable=False)
    exp_id          = Column(u'id_exp',INTEGER(display_width=11), ForeignKey(u'Experiment.id')) 
    name            = Column(u'name',VARCHAR(length=1024),nullable=False)
    sdate           = Column(u'sdate',DATETIME())
    edate           = Column(u'edate',DATETIME())
    restart         = Column(u'restart',DATETIME()) 
    status          = Column(u'status',SMALLINT(display_width=5))
    cdate           = Column(u'cdate',DATETIME())
    ctime           = Column(u'ctime',DATETIME()) 
    mult_label      = Column(u'multiparams_label',VARCHAR(length=100)) 
   
    # Realtionships
    exp             = relationship(ExperimentModel)

class ChunkModel( Base ):
  
    __tablename__   = 'Chunk'

    # Columns
    id              = Column(u'id',INTEGER(display_width=11), primary_key=True, nullable=False)
    rea_id          = Column(u'id_rea',INTEGER(display_width=11), ForeignKey(u'Realization.id'))
    sdate           = Column(u'sdate',DATETIME())
    edate           = Column(u'edate',DATETIME())
    wps             = Column(u'wps',INTEGER(display_width=1)) 
    status          = Column(u'status',SMALLINT(display_width=5))
    chunk_id        = Column(u'id_chunk',INTEGER(display_width=11))

    # Relationships
    rea             = relationship(RealizationModel)

class JobModel( Base ):
 
    __tablename__   = 'Job'

    # Columns
    id              = Column(u'id',INTEGER(display_width=11), primary_key=True, nullable=False)
    gw_job          = Column(u'gw_job',INTEGER(display_width=11))
    gw_restarted    = Column(u'gw_restarted',INTEGER(display_width=11)) 
    gw_array        = Column(u'gw_array',INTEGER(display_width=11)) 
    chunk_id        = Column(u'id_chunck', INTEGER(display_width=11), ForeignKey(u'Chunk.id'))
    resource        = Column(u'resource',VARCHAR(length=45))
    jobst_status    = Column(u'status', INTEGER(display_width=11), ForeignKey(u'Jobstatus.id'))
    hash            = Column(u'hash',VARCHAR(length=33)) 
    wn              = Column(u'wn',VARCHAR(length=45)) 
    exitcode        = Column(u'exitcode',INTEGER(display_width=11))

    # Relationships
    chunk           = relationship(ChunkModel)
    jobst           = relationship(JobStatusModel)
    
class EventsModel( Base ):

    __tablename__   = 'Events'   
 
    # Columns
    id              = Column(u'id',INTEGER(display_width=11), primary_key=True, nullable=False)
    job_id          = Column(u'id_job',INTEGER(display_width=11), ForeignKey(u'Job.id'))
    jobst_status    = Column(u'status',INTEGER(display_width=11), ForeignKey(u'Jobstatus.id'))
    timestamp       = Column(u'timestamp',DATETIME())

    # Relationships
    job             = relationship(JobModel)
    jobst           = relationship(JobStatusModel)


class FileTypeModel( Base ):

    __tablename__   = 'Filetype' 
 
    # Columns
    id              = Column(u'id',INTEGER(display_width=11), primary_key=True, nullable=False)
    type            = Column(u'type',VARCHAR(length=20))
    descr           = Column(u'descr',VARCHAR(length=2000))
    format          = Column(u'format',VARCHAR(length=20))

class FileModel( Base ):

    __tablename__   = 'File'

    # Columns
    id              = Column(u'id',INTEGER(display_width=11), primary_key=True, nullable=False)
    rea_id          = Column(u'rea_id',INTEGER(display_width=11), ForeignKey(u'Realization.id'))
    name            = Column(u'name',VARCHAR(length=45))
    filet_type      = Column(u'type',INTEGER(display_width=11), ForeignKey(u'Filetype.id'))
    sdate           = Column(u'sdate',DATETIME())
    edate           = Column(u'edate',DATETIME())

    # Relationships
    rea             = relationship(RealizationModel)
    filet           = relationship(FileTypeModel)

