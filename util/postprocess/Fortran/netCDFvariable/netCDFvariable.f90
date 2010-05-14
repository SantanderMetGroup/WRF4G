PROGRAM netCDFvariable
! Fortran code to obtain a variable from a netCDF file
  
! =================================Make Executable============================
!  Make executable:
!  GMS.UC:
!  gfortran netCDFvariable.f90 -L/home/lluis/bin/netcdf-4.0.1/lib -lnetcdf -lm -I/home/lluis/bin/netcdf-4.0.1/include -o netCDFvariable
!  GMS.UC 
!  pgf90 netCDFvariable.f90 -L/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/lib -lnetcdf -lm -I/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/include -Mfree -o netCDFvariable
!  GRIDUI
!  gfortran netCDFvariable.f90 -L/gpfs/csic_projects/meteo/software/ScientificLinux/5.3/netcdf/gcc_gfortran_NOhdf5/lib -lnetcdf -lm -I/gpfs/csic_projects/meteo/software/ScientificLinux/5.3/netcdf/gcc_gfortran_NOhdf5/include -o netCDFvariable

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'
  
! namelist
  CHARACTER(LEN=300)                                      :: ncfile, output_path
  CHARACTER(LEN=50)                                       :: variable
  INTEGER                                                 :: timestep, level, xpoint, ypoint
  
! Local
  INTEGER                                                 :: i
  INTEGER                                                 :: idi
  INTEGER                                                 :: ios, rcode
  INTEGER                                                 :: funit, lenfile, Loutput
  INTEGER                                                 :: filendims, filenvars, filengatts,  &
    nunlimdimid
  INTEGER                                                 :: ncid, varid, varndims
  INTEGER                                                 :: dimx, dimy, dimz, dimt
  INTEGER, DIMENSION(6)                                   :: vardimsid
  INTEGER, ALLOCATABLE, DIMENSION(:)                      :: dimslength
  INTEGER                                                 :: i1, i2, i3, i4, numdimall
  INTEGER                                                 :: idimall, idimone
  INTEGER, DIMENSION(4)                                   :: dimall, dimone
  CHARACTER(LEN=1), DIMENSION(4)                          :: dimallname, dimonename
  CHARACTER(LEN=4)                                        :: I_S4
  CHARACTER(LEN=20), ALLOCATABLE, DIMENSION(:)            :: dimsname
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:,:)               :: ncvariable,output  
  LOGICAL                                                 :: is_used
  
  NAMELIST /io/ ncfile, variable, timestep, level, xpoint, ypoint, output_path
  
!!!!!!!!!!!! Variables
! ncfile: netCDF file from which variable will be extracted
! variable: name of variable to be extracted
! output_path: direction to which ASCII output will be written
! timestep, level, xpoint, ypoint: specific coordenates to select (if any <0 all length will be
!   taken)
! lenfile: length of ncfile
! Loutput: length of output_path
! filendims: number fo dimensions of file
! filenvars: number of variables
! filengatts: number og global attributes
! nunlimdimid: id dimension of the unlimited variable
! ncid: id of netCDF file
! varid: id of variable
! varndims: number of dimensions of variable
! dimx, dimy, dimz, dimt: 4D cartesian dimensions of variable
! vardimsid: ids of variable dimensions
! dimslength: shape of variable dimensions
! numdimall: number of dimensions that will be entirely outputed
! idimall: number of dimensions to be entirely outputed
! idimone: number of dimensions to be only one value outputed
! dimall: range of idimall dimensions
! dimone: value of idimone dimensions
! dimallname: name of idimall dimensions
! dimonename: name of idimone dimensions
! dimsname: name of dimensions
! ncvariable: matrix with specific variable
! output: matrix with values of variable to be outputed

!!!!!!!!!!!! Function
! I_S4: function to transform an integer to a string of 4 characters 

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
  vardimsid=0
  
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
      dimt=dimslength(vardimsid(3))
      dimz=1
      
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
  PRINT *,'Variable dimensions. dim1:',dimx,' dim2:',dimy,' dim3:',dimz,' dim4:',dimt
! Checking values
  IF (dimx < xpoint) THEN
    PRINT *,'Value on dimension 1: ',xpoint,' is larger than variable dimension dim1:',dimx
    STOP
  END IF
  IF (dimy < ypoint) THEN
    PRINT *,'Value on dimension 2: ',ypoint,' is larger than variable dimension dim2:',dimy
    STOP
  END IF
  IF (dimz < level) THEN
    PRINT *,'Value on dimension 3: ',level,' is larger than variable dimension dim3:',dimz
    STOP
  END IF
  IF (dimt < timestep) THEN
    PRINT *,'Value on dimension 4: ',timestep,' is larger than variable dimension dim4:',dimt
    STOP
  END IF
  
  
! Output writting
  Loutput=LEN_TRIM(output_path)
  IF (output_path(Loutput:Loutput) /= '/') output_path(Loutput+1:Loutput+1)='/'  

  numdimall=0
  dimall=0
  dimone=0
  dimallname=' '
  dimonename=' '
  idimall=0
  idimone=0
  IF (timestep > 0) THEN
    idimone=idimone+1
    dimonename(idimone)='T'
    dimone(idimone)=timestep
  ELSE
    numdimall=numdimall+1
    idimall=idimall+1
    dimall(idimall)=dimt
    dimallname(idimall)='T'
  END IF
  IF (level > 0) THEN
    idimone=idimone+1
    dimonename(idimone)='Z'
    dimone(idimone)=level
  ELSE
    numdimall=numdimall+1
    idimall=idimall+1
    dimall(idimall)=dimz
    dimallname(idimall)='Z'
  END IF
  IF (xpoint > 0) THEN
    idimone=idimone+1
    dimonename(idimone)='X'
    dimone(idimone)=xpoint
  ELSE
    numdimall=numdimall+1
    idimall=idimall+1
    dimall(idimall)=dimx
    dimallname(idimall)='X'
  END IF
  IF (ypoint > 0) THEN
    idimone=idimone+1
    dimonename(idimone)='Y'
    dimone(idimone)=ypoint
  ELSE
    numdimall=numdimall+1
    idimall=idimall+1
    dimall(idimall)=dimy
    dimallname(idimall)='Y'
  END IF

  PRINT *,'Values___________'
  PRINT *,'all dim:'
  DO i=1,idimall
    PRINT *,i,': '//dimallname(i),'=',dimall(i)
  END DO
  PRINT *,'specific dim:'
  DO i=1,idimone
    PRINT *,i,': '//dimonename(i),'=',dimone(i)
  END DO
  
! Filling output
!!

  SELECT CASE (numdimall)
    CASE(4)
      IF (ALLOCATED(output)) DEALLOCATE(output)
      ALLOCATE(output(dimall(1),dimall(2),dimall(3),dimall(4),1,1))
      output=ncvariable
    CASE(3)
      IF (ALLOCATED(output)) DEALLOCATE(output)
      ALLOCATE(output(dimall(1),dimall(2),dimall(3),1,1,1))
      IF (xpoint>0) output(:,:,:,1,1,1)=ncvariable(xpoint,:,:,:,1,1)  
      IF (ypoint>0) output(:,:,:,1,1,1)=ncvariable(:,ypoint,:,:,1,1)  
      IF (level>0) output(:,:,:,1,1,1)=ncvariable(:,:,level,:,1,1)  
      IF (timestep>0) output(:,:,:,1,1,1)=ncvariable(:,:,:,timestep,1,1)  
    CASE(2)
      IF (ALLOCATED(output)) DEALLOCATE(output)
      ALLOCATE(output(dimall(1),dimall(2),1,1,1,1))
      IF (xpoint>0) THEN
	IF (ypoint>0) output(:,:,1,1,1,1)=ncvariable(xpoint,ypoint,:,:,1,1)
	IF (level>0) output(:,:,1,1,1,1)=ncvariable(xpoint,:,level,:,1,1)
	IF (timestep>0) output(:,:,1,1,1,1)=ncvariable(xpoint,:,:,timestep,1,1)
      ELSE IF (ypoint>0) THEN
	IF (level>0) output(:,:,1,1,1,1)=ncvariable(:,ypoint,level,:,1,1)  
	IF (timestep>0) output(:,:,1,1,1,1)=ncvariable(:,ypoint,:,timestep,1,1)	
      ELSE 
        output(:,:,1,1,1,1)=ncvariable(:,:,level,timestep,1,1)
      END IF
    CASE DEFAULT
      IF (ALLOCATED(output)) DEALLOCATE(output)
      ALLOCATE(output(dimall(1),1,1,1,1,1))
      IF (xpoint<0) output(:,1,1,1,1,1)=ncvariable(:,ypoint,level,timestep,1,1)
      IF (ypoint<0) output(:,1,1,1,1,1)=ncvariable(xpoint,:,level,timestep,1,1)
      IF (level<0) output(:,1,1,1,1,1)=ncvariable(xpoint,ypoint,:,timestep,1,1)
      IF (timestep<0) output(:,1,1,1,1,1)=ncvariable(xpoint,ypoint,level,:,1,1)      
  END SELECT
  PRINT *,'Resultant output dimensions________ '
  DO i=1,6
    PRINT *,i,' :',LBOUND(output,i),char(44),UBOUND(output,i)
  END DO
  
  DEALLOCATE(ncvariable)

  PRINT *,'Writting output', numdimall
! Writting output
!!

  SELECT CASE (numdimall)
    CASE(4)
      DO i1=1, dimall(1)
        DO i2=1, dimall(2)
          PRINT *,"Writting file: '"//TRIM(output_path)//TRIM(variable)//'_'//dimallname(1)// &
            I_S4(i1)//'_'//dimallname(2)//I_S4(i2)//'_'//dimallname(3)//I_S4(-1)//'_'//       &
	    dimallname(4)//I_S4(-1)//".dat'"
          OPEN(unit=12, file=TRIM(output_path)//TRIM(variable)//'_'//dimallname(1)//I_S4(i1)  &
            //'_'//dimallname(2)//I_S4(i2)//'_'//dimallname(3)//I_S4(-1)//'_'//dimallname(4)  &
	    //I_S4(-1)//'.dat', status='unknown')
          WRITE(12,8)'#'//TRIM(ncfile)
          DO i3=1, dimall(3)
            WRITE(12,15)(output(i1,i2,i3,i4,1,1), i4=1, dimall(4))
          END DO
          CLOSE(unit=12)
        END DO
      END DO
    CASE(3)
      DO i1=1,dimall(1)
        PRINT *,"Writting file: '"//TRIM(output_path)//TRIM(variable)//'_'//dimallname(1)//   &
          I_S4(i1)//'_'//dimallname(2)//I_S4(-1)//'_'//dimallname(3)//I_S4(-1)//'_'//         &
	  dimonename(1)//I_S4(dimone(1))//".dat'"
        OPEN(unit=12, file=TRIM(output_path)//TRIM(variable)//'_'//dimallname(1)//I_S4(i1)//  &
	  '_'//dimallname(2)//I_S4(-1)//'_'//dimallname(3)//I_S4(-1)//'_'//dimonename(1)//    &
	  I_S4(dimone(1))//'.dat', status='unknown')
        WRITE(12,8)'#'//TRIM(ncfile)//' '//TRIM(variable)//' @ '//dimonename(1)//'= '//       &
          I_S4(dimone(1))

        DO i2=1,dimall(2)
           WRITE(12,15)(output(i1,i2,i3,1,1,1), i3=1, dimall(3))
	END DO
      CLOSE(unit=12)

      END DO
    CASE(2)
      PRINT *,"Writting file: '"//TRIM(output_path)//TRIM(variable)//'_'//dimallname(1)//     &
        I_S4(-1)//'_'//dimallname(2)//I_S4(-1)//'_'//dimonename(1)//I_S4(dimone(1))//'_'//    &
	dimonename(2)//I_S4(dimone(2))//".dat'"
      OPEN(unit=12, file=TRIM(output_path)//TRIM(variable)//'_'//dimallname(1)//I_S4(-1)//    &
	'_'//dimallname(2)//I_S4(-1)//'_'//dimonename(1)//I_S4(dimone(1))//'_'//dimonename(2) &
	//I_S4(dimone(2))//'.dat', status='unknown')
      WRITE(12,8)'#'//TRIM(ncfile)//' '//TRIM(variable)//' @ '//dimonename(1)//'= '//         &
        I_S4(dimone(1))//' & '//dimonename(2)//'= '//I_S4(dimone(2))

      DO i1=1,dimall(1)
         WRITE(12,15)(output(i1,i2,1,1,1,1), i2=1, dimall(2))
      END DO
      CLOSE(unit=12)

    CASE(1)
      PRINT *,"Writting file: '"//TRIM(output_path)//TRIM(variable)//'_'//dimallname(1)//     &
        I_S4(-1)//'_'//dimonename(1)//I_S4(dimone(1))//'_'//dimonename(2)//I_S4(dimone(2))//  &
	'_'//dimonename(3)//I_S4(dimone(3))//".dat'"
      OPEN(unit=12, file=TRIM(output_path)//TRIM(variable)//'_'//dimallname(1)//I_S4(-1)//    &
	'_'//dimonename(1)//I_S4(dimone(1))//'_'//dimonename(2)//I_S4(dimone(2))//'_'//       &
	dimonename(3)//I_S4(dimone(3))//'.dat', status='unknown')
      WRITE(12,8)'#'//TRIM(ncfile)//' '//TRIM(variable)//' @ '//dimonename(1)//'= '//         &
        I_S4(dimone(1))//' & '//dimonename(2)//'= '//I_S4(dimone(2))//' & '//dimonename(3)//  &
        '= '//I_S4(dimone(3))

      DO i1=1,dimall(1)
         WRITE(12,18)i1,output(i1,1,1,1,1,1)
      END DO
      CLOSE(unit=12)
      
    CASE DEFAULT
      PRINT *,"Writting file: '"//TRIM(output_path)//TRIM(variable)//'_'//dimonename(1)//     &
        I_S4(dimone(1))//'_'//dimonename(2)//I_S4(dimone(2))//'_'//dimonename(3)//            &
	I_S4(dimone(3))//'_'//dimonename(4)//I_S4(dimone(4))//".dat'"
      OPEN(unit=12, file=TRIM(output_path)//TRIM(variable)//'_'//dimonename(1)//              &
        I_S4(dimone(1))//'_'//dimonename(2)//I_S4(dimone(2))//'_'//dimonename(3)//            &
	I_S4(dimone(3))//'_'//dimonename(4)//I_S4(dimone(4))//'.dat', status='unknown')
      WRITE(12,8)'#'//TRIM(ncfile)//' '//TRIM(variable)//' @ '//dimonename(1)//'= '//         &
        I_S4(dimone(1))//' & '//dimonename(2)//'= '//I_S4(dimone(2))//' & '//dimonename(3)//  &
        '= '//I_S4(dimone(3))//' & '//dimonename(4)//'= '//I_S4(dimone(4))

      WRITE(12,10)dimone,ncvariable(1,1,1,1,1,1)
      CLOSE(unit=12)

  END SELECT

  PRINT *,'============================================='
  PRINT *,"SUCCESSFULL END of 'netCDFvariable'.........."
  PRINT *,'============================================='
  DEALLOCATE(output)
        
8  format(500a)
10 format(4(i10,1x),e20.10,1x)
15 format(500(e20.10,1x))
18 format(i10,1x,e20.10,1x)

END PROGRAM netCDFvariable

CHARACTER(LEN=4) FUNCTION I_S4(integerNum)
! Function to transform an integer to a string of Nchar characters

  IMPLICIT NONE
  
  INTEGER, INTENT(IN)                                     :: integerNum
  INTEGER, PARAMETER                                      :: Nchar=4

! Local
  INTEGER                                                 :: partInt, ipot, pot
  
  IF (integerNum < 0) THEN
    I_S4='0all'
  ELSE
    partInt=integerNum
    DO ipot=1,Nchar
      pot=10**(Nchar-ipot)
      I_S4(ipot:ipot)=CHAR(INT(partInt/pot)+48)
      partInt=partInt-INT(partInt/pot)*pot
    END DO
  ENDIF
  
END FUNCTION I_S4

