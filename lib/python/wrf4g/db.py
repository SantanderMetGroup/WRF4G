from sqlalchemy      import Table, MetaData, Column, Integer, String, ForeignKey

__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

metadata = MetaData() 
# WRF4G database tables
experiment = Table(
             u'Experiment', metadata, 
             Column(u'id', INTEGER(display_width=11), primary_key=True, nullable=False), 
             Column(u'name', VARCHAR(length=512), nullable=False), 
             Column(u'sdate', DATETIME()), 
             Column(u'edate', DATETIME()), 
             Column(u'multiple_parameters', TINYINT(display_width=1)), 
             Column(u'multiple_dates', TINYINT(display_width=1)), 
             Column(u'basepath', VARCHAR(length=300)), 
             Column(u'multiparams_labels', VARCHAR(length=1024)), 
             Column(u'experiment_description', VARCHAR(length=1024)), 
             Column(u'id_user', INTEGER(display_width=11), ForeignKey(u'User.id')) 
             )

realization = Table(
             u'Realization', metadata, 
             Column(u'id', INTEGER(display_width=11), primary_key=True, nullable=False), 
             Column(u'id_exp', INTEGER(display_width=11), ForeignKey(u'Experiment.id')), 
             Column(u'name', VARCHAR(length=1024),nullable=False), 
             Column(u'sdate', DATETIME()), 
             Column(u'edate', DATETIME()), 
             Column(u'restart', DATETIME()), 
             Column(u'status', SMALLINT(display_width=5)), 
             Column(u'cdate', DATETIME()), 
             Column(u'ctime', DATETIME()), 
             Column(u'multiparams_label', VARCHAR(length=100)) 
             )

chunk = Table(
             u'Chunk', metadata,
             Column(u'id', INTEGER(display_width=11), primary_key=True, nullable=False), 
             Column(u'id_rea', INTEGER(display_width=11), ForeignKey(u'Realization.id')), 
             Column(u'sdate', DATETIME()), 
             Column(u'edate', DATETIME()), 
             Column(u'wps', TINYINT(display_width=1)), 
             Column(u'status', SMALLINT(display_width=5)), 
             Column(u'id_chunk', INTEGER(display_width=11))
             )

job  = Table(
             u'Job', metadata, 
             Column(u'id', INTEGER(display_width=11), primary_key=True, nullable=False), 
             Column(u'gw_job', INTEGER(display_width=11)), 
             Column(u'gw_restarted', INTEGER(display_width=11)), 
             Column(u'gw_array', INTEGER(display_width=11)), 
             Column(u'id_chunk', INTEGER(display_width=11), ForeignKey(u'Chunk.id')), 
             Column(u'resource', VARCHAR(length=45)), 
             Column(u'status', INTEGER(display_width=11), ForeignKey(u'Jobstatus.id')), 
             Column(u'hash', VARCHAR(length=33)), 
             Column(u'wn', VARCHAR(length=45)), 
             Column(u'exitcode', INTEGER(display_width=11))
             )

job_status = Table( 
             u'Jobstatus', metadata, 
             Column(u'id', INTEGER(display_width=11), primary_key=True, nullable=False), 
             Column(u'description', VARCHAR(length=45))
             )

events     = Table(
             u'Events', metadata, 
             Column(u'id', INTEGER(display_width=11), primary_key=True, nullable=False), 
             Column(u'id_job', INTEGER(display_width=11), ForeignKey(u'Job.id')), 
             Column(u'status', INTEGER(display_width=11), ForeignKey(u'Jobstatus.id')), 
             Column(u'timestamp', DATETIME())
             )

file_type  = Table(
             u'Filetype', metadata,
             Column(u'id', INTEGER(display_width=11), primary_key=True, nullable=False), 
             Column(u'type', VARCHAR(length=20)), 
             Column(u'descr', VARCHAR(length=2000)), 
             Column(u'format', VARCHAR(length=20))
             )

file       = Table(
             u'File', metadata, 
             Column(u'id', INTEGER(display_width=11), primary_key=True, nullable=False), 
             Column(u'id_rea', INTEGER(display_width=11), ForeignKey(u'Realization.id')), 
             Column(u'name', VARCHAR(length=45)), 
             Column(u'type', INTEGER(display_width=11), ForeignKey(u'Filetype.id')), 
             Column(u'sdate', DATETIME()), 
             Column(u'edate', DATETIME())
             )

user       = Table(
             u'User', metadata, 
             Column(u'id', INTEGER(display_width=11), primary_key=True, nullable=False), 
             Column(u'name', VARCHAR(length=30)), 
             Column(u'CN', VARCHAR(length=200))
             )

