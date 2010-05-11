PROGRAM netCDFvariable
! Fortran code to obtain a variable from a netCDF file
  
! =================================Make Executable============================
!  Make executable:
!  GMS.UC:
!  gfortran netCDFvariable.f90 -L/home/lluis/bin/netcdf-4.0.1/lib -lnetcdf -lm -I/home/lluis/bin/netcdf-4.0.1/include -o netCDFvariable
!  GMS.UC 
!  pgf90 netCDFvariable.f90 -L/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/lib -lnetcdf -lm -I/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/include -Mfree -o netCDFvariable
!

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'
  
! namelist
  CHARACTER(LEN=1024)                                     :: file
  CHARACTER(LEN=50)                                       :: variable
  INTEGER                                                 :: timestep, level
  
! Local
  INTEGER                                                 :: idim
  INTEGER                                                 :: ios, rcode
  INTEGER                                                 :: funit, lenfile
  INTEGER                                                 :: filendims, filenvars, filengatts,   &
    nunlimdimid
  INTEGER                                                 :: ncid, varid, varndims
  INTEGER                                                 :: dimx, dimy, dimz, dimt
  INTEGER, DIMENSION(6)                                   :: vardimsid
  INTEGER, ALLOCATABLE, DIMENSION(:)                      :: dimslength
  CHARACTER(LEN=20), ALLOCATABLE, DIMENSION(:)            :: dimsname
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:,:)               :: ncvariable
  LOGICAL                                                 :: is_used
  
  NAMELIST /io/ file, variable, timestep, level
  
! Read parameters from Fortran namelist
  DO funit=10,100
    INQUIRE(unit=funit, opened=is_used)
    IF (.not. is_used) EXIT
  END DO
  OPEN(funit,file='namelist.netCDFvariable',status='old',form='formatted',iostat=ios)
  IF ( ios /= 0 ) STOP "ERROR opening namelist.netdCDFvariable"
  READ(funit, io)
  CLOSE(funit)
  
!  lenfile=LEN_TRIM(file)

!  DO funit=10,100
!    INQUIRE(unit=funit, opened=is_used)
!    IF (.not. is_used) EXIT
!  END DO
  
! File information
  PRINT *,"Reading file '"//TRIM(file)//"' information..."
  rcode=nf_open(TRIM(file),0,ncid)
  IF (rcode /= 0) PRINT *,"Error opening file '"//TRIM(file)//"' "//nf_strerror(rcode)

  rcode=nf_inq_dim(ncid, filendims, filenvars, filengatts, nunlimdimid)
  IF (rcode /= 0) PRINT *,"Error reading file dimensions "//nf_strerror(rcode)

  IF (ALLOCATED(dimsname)) DEALLOCATE(dimsname)
  ALLOCATE(dimsname(filendims))
  IF (ALLOCATED(dimslength)) DEALLOCATE(dimslength)
  ALLOCATE(dimslength(filendims))
  DO idim=1, filendims
    rcode=nf_inq_dim(ncid, idim, dimsname(idim), dimslength(idim))
    PRINT *,'dimension: ',idim,' name: ',LEN(dimsname(idim)),' length:',dimslength(idim)
  END DO
  
  rcode=nf_inq_varid(file, TRIM(variable), varid)
  rcode=nf_inq_varndims(ncid, varid, varndims)
  rcode=nf_inq_varndims(ncid, varid, vardimsid)

  SELECT CASE (varndims)
    CASE (4)
      dimx=dimslength(vardimsid(1))
      dimy=dimslength(vardimsid(2))
      dimz=dimslength(vardimsid(3))
      dimt=dimslength(vardimsid(4))
      
      IF (ALLOCATED(ncvariable)) DEALLOCATE(ncvariable)
      ALLOCATE(ncvariable(dimx, dimy, dimz, dimt,1,1))
      
      rcode = nf_get_var_real ( ncid, varid, ncvariable)
      
    CASE (3)
      dimx=dimslength(vardimsid(1))
      dimy=dimslength(vardimsid(2))
      dimz=dimslength(vardimsid(3))
      dimt=1
      
      IF (ALLOCATED(ncvariable)) DEALLOCATE(ncvariable)
      ALLOCATE(ncvariable(dimx, dimy, dimz, dimt,1,1))
      
      rcode = nf_get_var_real ( ncid, varid, ncvariable)
      
    
    CASE (2)
      dimx=dimslength(vardimsid(1))
      dimy=dimslength(vardimsid(2))
      dimz=1
      dimt=1
      
      IF (ALLOCATED(ncvariable)) DEALLOCATE(ncvariable)
      ALLOCATE(ncvariable(dimx, dimy, dimz, dimt,1,1))
      
      rcode = nf_get_var_real ( ncid, varid, ncvariable)
    
    CASE (1)
      dimx=dimslength(vardimsid(1))
      dimy=1
      dimz=1
      dimt=1
      
      IF (ALLOCATED(ncvariable)) DEALLOCATE(ncvariable)
      ALLOCATE(ncvariable(dimx, dimy, dimz, dimt,1,1))
      
      rcode = nf_get_var_real ( ncid, varid, ncvariable)

  END SELECT

END PROGRAM netCDFvariable
