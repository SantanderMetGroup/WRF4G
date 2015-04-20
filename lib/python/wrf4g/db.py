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
mysql+pymysql://wrf4guser:Meteo2011@%(hostname)s:%(port)s/WRF4GDB
"""

def get_session():
    """
    Create a sqlalchemy session to connect with WRF4G database
    """
    db4g_urls = VarEnv( DB4G_CONF ).get_variable( 'URL' )
    # an Engine, which the Session will use for connection
    engine = create_engine( db4g_urls )
    # create a configured "Session" class
    Session = sessionmaker(bind = engine)
    # create a Session
    return Session()

Base = declarative_base()
#  WRF4G database is composed of the following tables:
#
#  Experiment
#  Realization
#  Chunk
#  Job
#  Events

#  WRF4G classes to manage the database
class ExperimentModel( Base ):

    __tablename__   = 'Experiment'
    
    # Columns
    id              = Column(u'id', INTEGER, primary_key=True, nullable=False)
    name            = Column(u'name', VARCHAR(length=512), nullable=False)
    sdate           = Column(u'sdate', DATETIME())
    edate           = Column(u'edate', DATETIME())
    csize           = Column(u'csize', INTEGER)
    mult_parameters = Column(u'multiple_parameters', INTEGER)
    mult_dates      = Column(u'multiple_dates', INTEGER) 
    home_dir        = Column(u'home_directory', VARCHAR(length=300))
    smul_length_h   = Column(u'simulation_length_h', INTEGER)
    smul_interval_h = Column(u'simulation_interval_h', INTEGER)
    mult_labels     = Column(u'multiparams_labels', VARCHAR(length=1024)) 
    exp_description = Column(u'experiment_description', VARCHAR(length=1024)) 

    # Realtionships
    realization     = relationship("RealizationModel", back_populates="experiment")

class RealizationModel( Base ):

    __tablename__   = 'Realization'
    
    # Columns
    id              = Column(u'id',INTEGER, primary_key=True, nullable=False)
    exp_id          = Column(u'exp_id',INTEGER, ForeignKey(u'Experiment.id')) 
    name            = Column(u'name',VARCHAR(length=1024),nullable=False)
    sdate           = Column(u'sdate',DATETIME())
    edate           = Column(u'edate',DATETIME())
    restart         = Column(u'restart',DATETIME()) 
    status          = Column(u'status',VARCHAR(length=20))
    cdate           = Column(u'cdate',DATETIME())
    ctime           = Column(u'ctime',DATETIME()) 
    mult_label      = Column(u'multiparams_label',VARCHAR(length=100)) 
   
    # Realtionships
    chunk           = relationship("ChunkModel", back_populates="realization")
    experiment      = relationship("ExperimentModel", back_populates="realization")

class ChunkModel( Base ):
  
    __tablename__   = 'Chunk'

    # Columns
    id              = Column(u'id',INTEGER, primary_key=True, nullable=False)
    rea_id          = Column(u'rea_id',INTEGER, ForeignKey(u'Realization.id'))
    sdate           = Column(u'sdate',DATETIME())
    edate           = Column(u'edate',DATETIME())
    wps             = Column(u'wps',INTEGER) 
    status          = Column(u'status',VARCHAR(length=20))
    chunk_id        = Column(u'chunk_id',INTEGER)

    # Relationships
    job             = relationship("JobModel", back_populates="chunk")
    realization     = relationship("RealizationModel", back_populates="chunk")
    

class JobModel( Base ):
 
    __tablename__   = 'Job'

    # Columns
    id              = Column(u'id',INTEGER, primary_key=True, nullable=False)
    gw_job          = Column(u'gw_job',INTEGER)
    gw_restarted    = Column(u'gw_restarted',INTEGER)  
    chunk_id        = Column(u'chunck_id', INTEGER, ForeignKey(u'Chunk.id'))
    resource        = Column(u'resource',VARCHAR(length=45))
    status          = Column(u'status', VARCHAR(length=20))
    exitcode        = Column(u'exitcode',INTEGER)

    # Relationships
    events          = relationship("EventsModel", back_populates="job")
    chunk           = relationship("ChunkModel", back_populates="job")
    
class EventsModel( Base ):

    __tablename__   = 'Events'   
 
    # Columns
    id              = Column(u'id',INTEGER, primary_key=True, nullable=False)
    job_id          = Column(u'job_id',INTEGER, ForeignKey(u'Job.id'))
    job_status      = Column(u'job_status',INTEGER)
    timestamp       = Column(u'timestamp',DATETIME())

    # Relationships
    job             = relationship("JobModel", back_populates="events")

