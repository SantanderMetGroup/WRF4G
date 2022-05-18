#
# Copyright 2016 Universidad de Cantabria
#
# Licensed under the EUPL, Version 1.1 only (the
# "Licence");
# You may not use this work except in compliance with the
# Licence.
# You may obtain a copy of the Licence at:
#
# http://ec.europa.eu/idabc/eupl
#
# Unless required by applicable law or agreed to in
# writing, software distributed under the Licence is
# distributed on an "AS IS" basis,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
# express or implied.
# See the Licence for the specific language governing
# permissions and limitations under the Licence.
#

from sqlalchemy     import ( Column, INTEGER,
                             VARCHAR, SMALLINT,
                             DATETIME, ForeignKey,
                             Interval, PickleType, 
                             MetaData, Table )
from sqlalchemy.orm import relationship, mapper
from wrf4g.core     import ( Experiment, Realization,
                             Chunk, Job, Events )

#
#  WRF4G database is composed of the following tables:
#  experiment
#  realization
#  chunk
#  job
#  events
#

metadata = MetaData()

experiment = Table( 'experiment', metadata,
                    Column('id',             INTEGER, primary_key=True, nullable=False),
                    Column('name',           VARCHAR(length=512), nullable=False),
                    Column('home_directory', VARCHAR(length=300))
                  )                                           

realization = Table( 'realization', metadata,
                     Column('id',               INTEGER, primary_key=True, nullable=False),
                     Column('exp_id',           INTEGER, ForeignKey('experiment.id')),
                     Column('name',             VARCHAR(length=1024), nullable=False),
                     Column('start_date',       DATETIME()),
                     Column('end_date',         DATETIME()),
                     Column('chunk_size',       Interval),
                     Column('restart',          DATETIME()),
                     Column('status',           VARCHAR(length=20)),
                     Column('wps_only_status',  VARCHAR(length=20)),
                     Column('current_date',     DATETIME()),
                     Column('current_chunk',    INTEGER),
                     Column('nchunks',          INTEGER),
                     Column('cfg',              PickleType)
                   )

chunk = Table( 'chunk', metadata,
                Column('id',         INTEGER, primary_key = True, nullable = False),
                Column('rea_id',     INTEGER, ForeignKey('realization.id')),
                Column('start_date', DATETIME()),
                Column('end_date',   DATETIME()),
                Column('wps',        INTEGER),
                Column('chunk_id',   INTEGER),
                Column('status',     VARCHAR(length=20)),
              )

job = Table( 'job', metadata,
             Column('id',           INTEGER, primary_key=True, nullable=False),
             Column('gw_job',       INTEGER),
             Column('gw_restarted', INTEGER),
             Column('chunck_id',    INTEGER, ForeignKey('chunk.id')),
             Column('resource',     VARCHAR(length=45)),
             Column('status',       VARCHAR(length=20)),
             Column('exitcode',     VARCHAR(length=20))
           )

events = Table( 'events', metadata, 
                Column('id',         INTEGER, primary_key=True, nullable=False),
                Column('job_id',     INTEGER, ForeignKey('job.id')),
                Column('job_status', VARCHAR(length=20)),
                Column('timestamp',  DATETIME())
              )

mapper(Experiment, experiment, properties={
       "realization" : relationship(Realization, back_populates="experiment", lazy='dynamic')
})
mapper(Realization, realization, properties={
       "experiment" : relationship(Experiment, back_populates = "realization" ),
       "chunk"      : relationship(Chunk,      back_populates = "realization", lazy='dynamic')
})
mapper(Chunk, chunk, properties={
       "realization" : relationship(Realization, back_populates = "chunk"),
       "job"         : relationship(Job,         back_populates = "chunk", lazy='dynamic')
       
})
mapper(Job, job, properties={
       "chunk"  : relationship(Chunk,  back_populates = "job"),
       "events" : relationship(Events, back_populates = "job", lazy='dynamic')
})
mapper(Events, events, properties={
       "job" : relationship(Job, back_populates = "events")
})


