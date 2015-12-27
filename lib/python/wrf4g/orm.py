from sqlalchemy                 import ( Column, INTEGER,
                                         VARCHAR, SMALLINT,
                                         DATETIME, ForeignKey,
                                         PickleType )
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm             import relationship, sessionmaker

__version__  = '2.2.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

Base = declarative_base()

class Experiment( Base ):

    __tablename__    = 'experiment'

    # Columns
    id               = Column('id',             INTEGER, primary_key=True, nullable=False)
    name             = Column('name',           VARCHAR(length=512), nullable=False)
    home_dir         = Column('home_directory', VARCHAR(length=300))

    # Realtionships
    realization      = relationship("Realization", back_populates="experiment", lazy='dynamic')

class Realization( Base ):

    __tablename__    = 'realization'

    # Columns
    id               = Column('id',               INTEGER, primary_key=True, nullable=False)
    exp_id           = Column('exp_id',           INTEGER, ForeignKey('experiment.id'))
    name             = Column('name',             VARCHAR(length=1024), nullable=False)
    start_date       = Column('start_date',       DATETIME())
    end_date         = Column('end_date',         DATETIME())
    chunk_size_h     = Column('chunk_size_h',     INTEGER)
    restart          = Column('restart',          DATETIME())
    status           = Column('status',           VARCHAR(length=20))
    current_date     = Column('current_date',     DATETIME())
    current_chunk    = Column('current_chunk',    INTEGER)
    nchunks          = Column('nchunks',          INTEGER)
    calendar         = Column('calendar',         VARCHAR(length=300))
    max_dom          = Column('max_dom',          INTEGER)
    np               = Column('np',               INTEGER)
    requirements     = Column('requirements',     VARCHAR(length=1024))
    environment      = Column('environment',      VARCHAR(length=1024))
    parallel_real    = Column('parallel_real',    VARCHAR(length=3))
    parallel_wrf     = Column('parallel_wrf',     VARCHAR(length=3))
    parallel_env     = Column('parallel_env',     VARCHAR(length=20))
    domain_path      = Column('domain_path',      VARCHAR(length=1024))
    preprocessor     = Column('preprocessor',     VARCHAR(length=1024))
    extdata_path     = Column('extdata_path',     VARCHAR(length=1024))
    postprocessor    = Column('postprocessor',    VARCHAR(length=1024))
    app              = Column('app',              VARCHAR(length=2048))
    output_path      = Column('output_path',      VARCHAR(length=1024))
    extdata_member   = Column('extdata_member',   VARCHAR(length=1024))
    namelist_version = Column('namelist_version', VARCHAR(length=10))
    namelist_values  = Column('namelist_values',  PickleType)

    # Realtionships
    experiment       = relationship("Experiment", back_populates = "realization")
    chunk            = relationship("Chunk",      back_populates = "realization", lazy='dynamic')

class Chunk( Base ):
    __tablename__   = 'chunk'

    # Columns
    id               = Column('id',         INTEGER, primary_key = True, nullable = False)
    rea_id           = Column('rea_id',     INTEGER, ForeignKey('realization.id'))
    start_date       = Column('start_date', DATETIME())
    end_date         = Column('end_date',   DATETIME())
    wps              = Column('wps',        INTEGER)
    chunk_id         = Column('chunk_id',   INTEGER)
    status           = Column('status',     VARCHAR(length=20))

    # Relationships
    realization      = relationship("Realization", back_populates = "chunk")
    job              = relationship("Job", back_populates = "chunk", lazy = "dynamic")

class Job( Base ):
    """
    A class to manage WRF4G jobs
    """
    __tablename__    = 'job'

    # Columns
    id               = Column('id',           INTEGER, primary_key=True, nullable=False)
    gw_job           = Column('gw_job',       INTEGER)
    gw_restarted     = Column('gw_restarted', INTEGER)
    chunk_id         = Column('chunck_id',    INTEGER, ForeignKey('chunk.id'))
    resource         = Column('resource',     VARCHAR(length=45))
    status           = Column('status',       VARCHAR(length=20))
    exitcode         = Column('exitcode',     VARCHAR(length=20))

    # Relationship
    chunk            = relationship("Chunk", back_populates = "job")
    events           = relationship("Events", back_populates = "job", lazy = 'dynamic')

class Events( Base ):

    __tablename__    = 'events'

    # Columns
    id               = Column('id',         INTEGER, primary_key=True, nullable=False)
    job_id           = Column('job_id',     INTEGER, ForeignKey('job.id'))
    job_status       = Column('job_status', VARCHAR(length=20))
    timestamp        = Column('timestamp',  DATETIME())

    # Relationship
    job              = relationship("Job", back_populates = "events")


