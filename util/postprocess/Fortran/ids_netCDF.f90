PROGRAM ids_netCDF
! Program to read all ids of a netCDF
!
!!
! GMS.UC: Februrary '10
!
!!!!!!!!!! COMPILATION
!
!! OCEANO: pgf90 ids_netCDF.f90 -L/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/lib -lnetcdf -lm -I/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/include -Mfree -o ids_netCDF 

! 34567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

! Arguments
  INTEGER                                                :: iarg, narg
  CHARACTER(LEN=500), ALLOCATABLE, DIMENSION(:)          :: arguments

  CHARACTER(LEN=300)                                     :: file
  INTEGER                                                :: ncid, idim, ivar
  INTEGER                                                :: Ndims, Nvars, Ngatt, idunl 
  INTEGER                                                :: idvar, iddim, Rdim
  CHARACTER(LEN=50)                                      :: varname
  INTEGER                                                :: rcode
  INTEGER, ALLOCATABLE, DIMENSION(:)                     :: idvars, dimnames
  INTEGER, DIMENSION(6)                                  :: varshape
  INTEGER                                                :: vartype, varndims, varnatts

!!!!!!!!!!! Variables
! file: netCDF file
! ncid: id of netCDF file
! Ndims: number of dimensions
! Nvars: number of variables
! Ngatt: number of global attributes
! idunl: dimension id of unlimit range
! idvar: variable id
! iddim: dimension id
! Rdim: range of dimension
! varname: variable name
! dimnames: vector with dimension name
! idvars: vector with variable ids
! vartype: variable type
! varndims: variable number of dimensions
! varshape: variable shape
! varnatts: variable number of attributes

! Getting arguments
!!
  narg=COMMAND_ARGUMENT_COUNT()
  IF (ALLOCATED(arguments)) DEALLOCATE(arguments)
  ALLOCATE(arguments(narg))

  DO iarg=1, narg
    CALL GETARG(iarg,arguments(iarg))
  END DO
  file=arguments(1)

  DEALLOCATE(arguments)
  
  rcode = nf_open(file, 0, ncid)
  IF (rcode /= 0) PRINT *, rcode, nf_strerror(rcode)

  rcode = nf_inq(ncid, Ndims, Nvars, Ngatt, idunl)
  PRINT *,"netCDF file'"//TRIM(file)//"' global information____________"
  PRINT *,'number of dimensions: ',ndims
  PRINT *,'number of variables: ',nvars
  PRINT *,'number of global attributes: ',Ngatt

  rcode = nf_inq_ndim(ncid, Ndims)

  PRINT *,'Dimensions of file______________'
  DO idim=1,Ndims
    rcode = nf_inq_dim(ncid, idim, dimnames(idim), Rdim)
    WRITE(*,10)'dimension id:',idim,' name: ',TRIM(dimnames(idim)),' range: ',Rdim
  END DO 

  IF (ALLOCATED(idvars)) DEALLOCATE(idvars)
  ALLOCATE(idvars(Nvars))
!  rcode = nf_inq_varids(ncid, Nvars, idvars)

  DO ivar=1, Nvars
    rcode = nf_inq_var(ncid, ivar, varname, vartype, varndims, varshape, varnatts)    
    PRINT *,'var id:',ivar,'name: ',TRIM(varname),' type:',vartype,' n. of dimensions: ',       &
      varndims, ' var shape: ',(varshape(idim),idim=1,varndims),' n. of attributes: ', varnatts
  END DO
  rcode = nf_close(ncid)

 10 format(13a, 3i, 7a, 40a, a8, i6) 

END PROGRAM ids_netCDF
