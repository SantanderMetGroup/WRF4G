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
  CHARACTER(LEN=300)                                      :: ncfile, output_path
  CHARACTER(LEN=50)                                       :: variable
  INTEGER                                                 :: timestep, level, xpoint, ypoint
  
! Local
  INTEGER                                                 :: idi
  INTEGER                                                 :: ios, rcode
  INTEGER                                                 :: funit, lenfile, Loutput
  INTEGER                                                 :: filendims, filenvars, filengatts,  &
    nunlimdimid
  INTEGER                                                 :: ncid, varid, varndims
  INTEGER                                                 :: dimx, dimy, dimz, dimt
  INTEGER, DIMENSION(6)                                   :: vardimsid
  INTEGER, ALLOCATABLE, DIMENSION(:)                      :: dimslength
  INTEGER                                                 :: itimestep, etimestep, ilevel,      &
    elevel, ixpoint, expoint, iypoint, eypoint
  INTEGER                                                 :: it, iz, ix, iy
  CHARACTER(LEN=4)                                        :: I_S4
  CHARACTER(LEN=20), ALLOCATABLE, DIMENSION(:)            :: dimsname
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:,:)               :: ncvariable
  LOGICAL                                                 :: is_used
  
  NAMELIST /io/ ncfile, variable, timestep, level, xpoint, ypoint, output_path
  
! Read parameters from Fortran namelist
  DO funit=10,100
    INQUIRE(unit=funit, opened=is_used)
    IF (.not. is_used) EXIT
  END DO
  OPEN(funit, file='namelist.netCDFvariable', status='old', form='formatted', iostat=ios)
  IF ( ios /= 0 ) STOP "ERROR opening namelist.netdCDFvariable"
  READ(funit, io)
  CLOSE(funit)
  
! File information
  PRINT *,"Reading file '"//TRIM(ncfile)//"' information..."
  rcode = nf_open(TRIM(ncfile), 0, ncid)
  IF (rcode /= 0) PRINT *,"Error opening file '"//TRIM(ncfile)//"' "//nf_strerror(rcode)

  rcode=nf_inq(ncid, filendims, filenvars, filengatts, nunlimdimid)
  IF (rcode /= 0) PRINT *,"Error reading file dimensions "//nf_strerror(rcode)

  IF (ALLOCATED(dimsname)) DEALLOCATE(dimsname)
  ALLOCATE(dimsname(filendims))
  IF (ALLOCATED(dimslength)) DEALLOCATE(dimslength)
  ALLOCATE(dimslength(filendims))
  DO idi=1, filendims
    rcode=nf_inq_dim(ncid, idi, dimsname(idi), dimslength(idi))
    PRINT *,'dimension: ',idi,' name: ',TRIM(dimsname(idi)),' length:',dimslength(idi)
  END DO
  
  rcode=nf_inq_varid(ncid, TRIM(variable), varid)
  rcode=nf_inq_varndims(ncid, varid, varndims)
  rcode=nf_inq_vardimid(ncid, varid, vardimsid)
  PRINT *,"Variable '"//TRIM(variable)//"' information____________"
  PRINT *,'varid: ',varid
  PRINT *,'varndims: ',varndims
  PRINT *,'vardimsid: ',vardimsid

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
  PRINT *,'dimx:',dimx,' dimy:',dimy,' dimz:',dimz,' dimt:',dimt

! Output writting
  Loutput=LEN_TRIM(output_path)
  IF (output_path(Loutput:Loutput) /= '/') output_path(Loutput+1:Loutput+1)='/'  

  itimestep=1
  ilevel=1
  ixpoint=1
  iypoint=1
  IF (timestep > 0) THEN
    itimestep=timestep
    etimestep=timestep
  ELSE
    etimestep=dimt
  END IF
  IF (level > 0) THEN
    ilevel=level
    elevel=level
  ELSE
    elevel=dimz
  END IF
  IF (xpoint > 0) THEN
    ixpoint=xpoint
    expoint=xpoint
  ELSE
    expoint=dimx
  END IF
  IF (ypoint > 0) THEN
    iypoint=ypoint
    eypoint=ypoint
  ELSE
    eypoint=dimy
  END IF

  IF ((timestep > 0) .AND. (level > 0) .AND. (xpoint > 0) .AND. (ypoint > 0) ) THEN
    PRINT *,"Writting file: '"//TRIM(output_path)//TRIM(variable)//'_T'//I_S4(timestep)//       &
      '_L'//I_S4(level)//'_X'//I_S4(xpoint)//'_Y'//I_S4(ypoint)//".dat'" 
    OPEN(unit=12, file=TRIM(output_path)//TRIM(variable)//'_T'//I_S4(timestep)//'_L'//          &
      I_S4(level)//'_X'//I_S4(xpoint)//'_Y'//I_S4(ypoint)//'.dat', status='unknown')
    WRITE(12,8)'#'//TRIM(ncfile)
    WRITE(12,10)timestep,level,xpoint,ypoint,ncvariable(xpoint,ypoint,level,timestep,1,1)
    CLOSE(unit=12)
  END IF

  IF ((timestep < 0) .AND. (level < 0) .AND. (xpoint < 0) .AND. (ypoint < 0) ) THEN
    DO it=itimestep, etimestep
      DO iz=ilevel, elevel
        PRINT *,"Writting file: '"//TRIM(output_path)//TRIM(variable)//'_T'//                   &
          I_S4(it)//'_L'//I_S4(iz)//"_X99_Y_99.dat'"
        OPEN(unit=12, file=TRIM(output_path)//TRIM(variable)//'_T'//I_S4(it)//'_L'              &
          //I_S4(iz)//'_X99_Y_99.dat', status='unknown')
        WRITE(12,8)'#'//TRIM(ncfile)
        DO ix=ixpoint, expoint
          WRITE(12,15)(ncvariable(ix,iy,iz,it,1,1), iy=iypoint, eypoint)
        END DO
        CLOSE(unit=12)
      END DO
    END DO
  END IF

 8  format(500a)
 10 format(4(i10,1x),e20.10,1x)
 15 format(1000(e20.10,1x))

END PROGRAM netCDFvariable

CHARACTER(LEN=4) FUNCTION I_S4(integerNum)
! Function to transform an integer to a string of Nchar characters

  IMPLICIT NONE
  
  INTEGER, INTENT(IN)                                     :: integerNum
  INTEGER, PARAMETER                                      :: Nchar=4

! Local
  INTEGER                                                 :: partInt, ipot, pot
  
  partInt=integerNum
  DO ipot=1,Nchar
    pot=10**(Nchar-ipot)
    I_S4(ipot:ipot)=CHAR(INT(partInt/pot)+48)
    partInt=partInt-INT(partInt/pot)*pot
  END DO

END FUNCTION I_S4

