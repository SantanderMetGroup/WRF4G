PROGRAM wrfnc_push_soil_data
! Program to change some fields from a netCDF from another one
!
!!
! GMS.UC: Februrary '10
!
!!!!!!!!!! COMPILATION
!
!! OCEANO: pgf90 wrfnc_push_soil_data.f90 -L/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/lib -lnetcdf -lm -I/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/include -Mfree -o wrfnc_push_soil_data

! 34567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'
! Arguments 
  INTEGER                                                :: iarg, narg
  CHARACTER(LEN=500), ALLOCATABLE, DIMENSION(:)          :: arguments

! Namelist variables
  CHARACTER(LEN=3000)                                    :: variablesIN, variablesCH
  LOGICAL                                                :: debug

! Program variables
  INTEGER                                                :: ivar
  INTEGER                                                :: funit, ios, ierr
  LOGICAL                                                :: is_used
  INTEGER                                                :: Nvariables
  INTEGER                                                :: Nnc_dims, Nnc_vars, Nnc_gatts,      &
    nc_idunlimid
  CHARACTER(LEN=500)                                     :: originalfile, changingfile, newfile
  CHARACTER(LEN=50), ALLOCATABLE, DIMENSION(:)           :: changeINvariables, changeCHvariables
  INTEGER                                                :: dimx, dimy, dimz, dimt 
  INTEGER, ALLOCATABLE, DIMENSION(:)                     :: rangedims 
  CHARACTER(LEN=50), ALLOCATABLE, DIMENSION(:)           :: namedims
  CHARACTER                                              :: spaces

  NAMELIST /vars/ variablesIN, variablesCH, debug

!!!!!!!!!!!!!!!!! Variables
! Nvariables: number of variables to change
! changevariables: vector with variables names
! originalfile: file to change variables
! changingfile: file with variables will be changed
! newfile: new output file 
! change[IN/CH]variables: vector with variables names to change

!!!!!!!!!!!!!!!!! Subroutines
! change_var: change variables of a file from another one
! read_var3D: read a 3D variable from a file
! read_var4D: read a 4D variable from a file
! variable_inf: read variable information from netCDF file
! variable_inf_ascii: read variable information from 'variables.inf' external ASCII file
! variable_Ndims_ascii: read variable number of dimensions from 'variables.inf' external ASCII file
! var_range_dimensions:
! nc_dimensions: obtain range of dimensions of a netCDF file
! nc_Ndim: obtain number of dimensions of a netCDF file
! string_values: obtain values from a 'namelist' string separated by comas  
! number_values: obtain number of variables from a 'namelist' variable with coma separation 
! spaces: function to give a number of spaces

! Getting arguments
!!
  narg=COMMAND_ARGUMENT_COUNT()
  IF (ALLOCATED(arguments)) DEALLOCATE(arguments)
  ALLOCATE(arguments(narg))

  DO iarg=1, narg
    CALL GETARG(iarg,arguments(iarg))
  END DO
  originalfile=arguments(1)
  changingfile=arguments(2)
  newfile=arguments(3)

  DEALLOCATE(arguments)

! Reading parameters from namelist
!!
  DO funit=10,100
    INQUIRE(unit=funit, opened=is_used)
    IF (.not. is_used) EXIT
  END DO
  OPEN(funit, file='namelist.wrfnc_push_soil_data', status='old', form='formatted', iostat=ios)
  IF ( ios /= 0 ) STOP "ERROR opening 'namelist.wrfnc_push_soil_data'"
  READ(funit, vars)
  CLOSE(funit)

  IF (debug) THEN
    PRINT *,'Original file: ',TRIM(originalfile)
    PRINT *,'Source of new fields: ',TRIM(changingfile)
    PRINT *,'New output file: ',TRIM(newfile)
  END IF

! Constructing variables names
!!
  CALL number_values(variablesIN, Nvariables)
  IF (ALLOCATED(changeINvariables)) DEALLOCATE(changeINvariables)
  ALLOCATE(changeINvariables(Nvariables), STAT=ierr)
  IF (ierr /= 0 ) PRINT *,"Error allocating 'changeINvariables'"
  changeINvariables=' '
  CALL string_values(variablesIN, debug, Nvariables, 50, changeINvariables)

  IF (ALLOCATED(changeCHvariables)) DEALLOCATE(changeCHvariables)
  ALLOCATE(changeCHvariables(Nvariables), STAT=ierr)
  IF (ierr /= 0 ) PRINT *,"Error allocating 'changeCHvariables'"
  changeCHvariables=' '
  CALL string_values(variablesCH, debug, Nvariables, 50, changeCHvariables)

  IF (debug) THEN
    PRINT *,'Will be changed ',Nvariables,' variables by:'
    DO ivar=1, Nvariables
      PRINT *,TRIM(changeINvariables(ivar)),' by ',TRIM(changeCHvariables(ivar))
    END DO
  END IF

! netCDF characteristics
!!
  CALL nc_Ndim(originalfile, debug, Nnc_dims, Nnc_vars, Nnc_gatts, nc_idunlimid)
  IF (ALLOCATED(rangedims)) DEALLOCATE(rangedims)
  IF (ALLOCATED(namedims)) DEALLOCATE(namedims)
  ALLOCATE(rangedims(Nnc_dims), namedims(Nnc_dims))

! Reading variables
!!
  CALL change_var(debug, originalfile, changingfile, changeINvariables, changeCHvariables,      &
    Nvariables, newfile) 

END PROGRAM wrfnc_push_soil_data

SUBROUTINE change_var(dbg, origf, chanf, varsIN, varsCH, Nvars, ofile) 
! Subroutine to change variables of a file from another one

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  LOGICAL, INTENT(IN)                                    :: dbg
  CHARACTER(LEN=500), INTENT(IN)                         :: origf, chanf, ofile 
  INTEGER, INTENT(IN)                                    :: Nvars
  CHARACTER(LEN=50), DIMENSION(Nvars), INTENT(IN)        :: varsIN, varsCH
! Local vars
  INTEGER                                                :: i, ivar
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:)                  :: variable4Din, variable4Dch
  REAL, ALLOCATABLE, DIMENSION(:,:,:)                    :: variable3Din, variable3Dch
  REAL, ALLOCATABLE, DIMENSION(:,:)                      :: variable2Din, variable2Dch
  CHARACTER(LEN=50), ALLOCATABLE, DIMENSION(:)           :: dimension_names
  INTEGER                                                :: ierr, rcode
  INTEGER                                                :: Ndimsvar_in, Ndimsvar_ch, Ndimsvar
  INTEGER, ALLOCATABLE, DIMENSION(:)                     :: rangedims, start_dims, dims_out
  CHARACTER(LEN=50)                                      :: varnameIN, varnameCH, units
  CHARACTER(LEN=250)                                     :: longdesc
  INTEGER                                                :: origid, nc_var_id_in, chanid,       &
    nc_var_id_ch, nc_var_id
  INTEGER, DIMENSION(6)                                  :: shape_in, shape_ch, jshape
  INTEGER, DIMENSION(4)                                  :: var_dimsid
  CHARACTER(LEN=50)                                      :: name
  INTEGER                                                :: val

!!!!!!!!!!!!!!!!! Variables
! origf, chanf, ofile: original, changing and output files 
! vars[IN/CH]: vector with variables names [IN] to be changed by [CH]
! Nvars: number of variables
! variable4D[in/ch]: 4D variable as [input/change] 
! variable3D[in/ch]: 2D variable as [input/change] 
! variable2D[in/ch]: 3D variable as [input/change]
! Ndimsvar: number of dimensions of variable
! varname[IN/CH]: name of diagnostic variable
! shape: shape of variable
! dimension_names: names of variable's dimension
! units: units of variable
! longdesc: long description attribute of variable
! origid: id of original netCDF file
! chanid: id of changing netCDF file
! nc_var_id_[in/ch]: variable id in netCDF original[in]/changing[ch] file
! shape_[in/ch]: shapes of original[in]/changing[ch] variables
  IF (dbg) PRINT *,"Subroutine 'chang_var'..."
!!
! changing variables 
!!
!!!
  changeALL_vars: DO ivar=1, Nvars
    varnameIN=varsIN(ivar)
    varnameCH=varsCH(ivar)
    CALL variable_inf(origf, varnameIN, dbg, nc_var_id_in, Ndimsvar_in, shape_in)
    CALL variable_inf(chanf, varnameCH, dbg, nc_var_id_ch, Ndimsvar_ch, shape_ch)
    IF (Ndimsvar_in /= Ndimsvar_ch) PRINT *,'Variables with different number of dimensions! '// &
      'original varibale '//TRIM(varnameIN)//' ndims: ',Ndimsvar_in
    change_var: SELECT CASE(TRIM(varnameIN))
    IF (ALLOCATED(dims_out)) DEALLOCATE(dims_out)
    ALLOCATE(dims_out(Ndimsvar_in))
    DO i=1, Ndimsvar_in
      dims_out(i)=shape_in(i)
    END DO
    PRINT *,'dimsout: ',dims_out

! SH2O
!!
     CASE('SH2O')
       PRINT *,'Changing '//TRIM(varnameIN)//' by '//TRIM(varnameCH)//'...'
   
       IF (ALLOCATED(variable4Din)) DEALLOCATE(variable4Din)
       IF (ALLOCATED(variable4Dch)) DEALLOCATE(variable4Dch)
       ALLOCATE(variable4Din(dims_out(1), dims_out(2), dims_out(3), dims_out(4)), STAT=ierr)
       IF (ierr /= 0) PRINT *,"Error allocating 'diagnostic4Din'"
       ALLOCATE(variable4Dch(dims_out(1), dims_out(2), dims_out(3), dims_out(4)), STAT=ierr)
       IF (ierr /= 0) PRINT *,"Error allocating 'diagnostic4Dch'"

       CALL read_var4D(dbg, origf, varnameIN, dims_out(1), dims_out(2), dims_out(3),            &
         dims_out(4), variable4Din)
       CALL read_var4D(dbg, chanf, varnameCH, dims_out(1), dims_out(2), dims_out(3),            &
         dims_out(4), variable4Dch)

       IF (dbg) THEN
         PRINT *,'VAR: '//TRIM(varnameIN)//' nc_id:',nc_var_id_in
         PRINT *,'1/2 example value. IN:', variable4Din(dims_out(1)/2, dims_out(2)/2,           &
           dims_out(3)/2, dims_out(4)/2), 'CHANGE: ', variable4Dch(dims_out(1)/2, dims_out(2)/2,&
           dims_out(3)/2, dims_out(4)/2)
       ENDIF

       rcode = nf_open(origf, nf_write, origid)
       rcode = nf_put_var_real (origid, nc_var_id_in, variable4Dch)
       IF (rcode /= 0) PRINT *,'Error writting change...',nf_strerror(rcode)
       rcode = nf_close(origid)

       DEALLOCATE(variable4Din, variable4Dch)
! SMOIS
!!
     CASE('SMOIS')
       PRINT *,'Changing '//TRIM(varnameIN)//' by '//TRIM(varnameCH)//'...'

       IF (ALLOCATED(variable4Din)) DEALLOCATE(variable4Din)
       IF (ALLOCATED(variable4Dch)) DEALLOCATE(variable4Dch)
       ALLOCATE(variable4Din(dims_out(1), dims_out(2), dims_out(3), dims_out(4)), STAT=ierr)
       IF (ierr /= 0) PRINT *,"Error allocating 'diagnostic4Din'"
       ALLOCATE(variable4Dch(dims_out(1), dims_out(2), dims_out(3), dims_out(4)), STAT=ierr)
       IF (ierr /= 0) PRINT *,"Error allocating 'diagnostic4Dch'"

       CALL read_var4D(dbg, origf, varnameIN, dims_out(1), dims_out(2), dims_out(3),            &
         dims_out(4), variable4Din)
       CALL read_var4D(dbg, chanf, varnameCH, dims_out(1), dims_out(2), dims_out(3),            &
         dims_out(4), variable4Dch)

       IF (dbg) THEN
         PRINT *,'VAR: '//TRIM(varnameIN)//' nc_id:',nc_var_id_in
         PRINT *,'1/2 example value. IN:', variable4Din(dims_out(1)/2, dims_out(2)/2,           &
           dims_out(3)/2, dims_out(4)/2), 'CHANGE: ', variable4Dch(dims_out(1)/2, dims_out(2)/2,&
           dims_out(3)/2, dims_out(4)/2)
       ENDIF

       rcode = nf_open(origf, nf_write, origid)
       rcode = nf_put_var_real (origid, nc_var_id_in, variable4Dch)
       IF (rcode /= 0) PRINT *,'Error writting change...',nf_strerror(rcode)
       rcode = nf_close(origid)

       DEALLOCATE(variable4Din, variable4Dch)
!  TMN
!!
     CASE('TMN')
       PRINT *,'Changing '//TRIM(varnameIN)//' by '//TRIM(varnameCH)//'...'

       IF (ALLOCATED(variable3Din)) DEALLOCATE(variable3Din)
       IF (ALLOCATED(variable3Dch)) DEALLOCATE(variable3Dch)
       ALLOCATE(variable3Din(dims_out(1), dims_out(2), dims_out(3)), STAT=ierr)
       IF (ierr /= 0) PRINT *,"Error allocating 'diagnostic3Din'"
       ALLOCATE(variable3Dch(dims_out(1), dims_out(2), dims_out(3)), STAT=ierr)
       IF (ierr /= 0) PRINT *,"Error allocating 'diagnostic4Dch'"

       CALL read_var3D(dbg, origf, varnameIN, dims_out(1), dims_out(2), dims_out(3),            &
         variable3Din)
       CALL read_var3D(dbg, chanf, varnameCH, dims_out(1), dims_out(2), dims_out(3),            &
         variable3Dch)

       IF (dbg) THEN
         PRINT *,'VAR: '//TRIM(varnameIN)//' nc_id:',nc_var_id_in
         PRINT *,'1/2 example value. IN:', variable3Din(dims_out(1)/2, dims_out(2)/2,           &
           dims_out(3)/2), 'CHANGE: ', variable3Dch(dims_out(1)/2, dims_out(2)/2, dims_out(3)/2)
       ENDIF

       rcode = nf_open(origf, nf_write, origid)
       rcode = nf_put_var_real (origid, nc_var_id_in, variable3Dch)
       IF (rcode /= 0) PRINT *,'Error writting change...',nf_strerror(rcode)
       rcode = nf_close(origid)

       DEALLOCATE(variable3Din, variable3Dch)
!  TSLB
!!
     CASE('TSLB')
       PRINT *,'Changing '//TRIM(varnameIN)//' by '//TRIM(varnameCH)//'...'

       IF (ALLOCATED(variable3Din)) DEALLOCATE(variable3Din)
       IF (ALLOCATED(variable3Dch)) DEALLOCATE(variable3Dch)
       ALLOCATE(variable3Din(dims_out(1), dims_out(2), dims_out(3)), STAT=ierr)
       IF (ierr /= 0) PRINT *,"Error allocating 'diagnostic3Din'"
       ALLOCATE(variable3Dch(dims_out(1), dims_out(2), dims_out(3)), STAT=ierr)
       IF (ierr /= 0) PRINT *,"Error allocating 'diagnostic4Dch'"

       CALL read_var3D(dbg, origf, varnameIN, dims_out(1), dims_out(2), dims_out(3),            &
         variable3Din)
       CALL read_var3D(dbg, chanf, varnameCH, dims_out(1), dims_out(2), dims_out(3),            &
         variable3Dch)

       IF (dbg) THEN
         PRINT *,'VAR: '//TRIM(varnameIN)//' nc_id:',nc_var_id_in
         PRINT *,'1/2 example value. IN:', variable3Din(dims_out(1)/2, dims_out(2)/2,           &
           dims_out(3)/2), 'CHANGE: ', variable3Dch(dims_out(1)/2, dims_out(2)/2, dims_out(3)/2)
       ENDIF

       rcode = nf_open(origf, nf_write, origid)
       rcode = nf_put_var_real (origid, nc_var_id_in, variable3Dch)
       IF (rcode /= 0) PRINT *,'Error writting change...',nf_strerror(rcode)
       rcode = nf_close(origid)

       DEALLOCATE(variable3Din, variable3Dch)
    END SELECT change_var

    DEALLOCATE(dims_out)

  END DO changeALL_vars 

  RETURN
END SUBROUTINE change_var 

SUBROUTINE variable_inf(file, varname, debg, nc_var_id, Ndimsvar, shape)
! Subroutine to obtain variable information from netCDF file

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  CHARACTER(LEN=500), INTENT(IN)                               :: file
  CHARACTER(LEN=50), INTENT(IN)                                :: varname
  INTEGER, INTENT(OUT)                                         :: nc_var_id, Ndimsvar
  INTEGER, DIMENSION(6), INTENT(OUT)                           :: shape  
  LOGICAL, INTENT(IN)                                          :: debg

! Local
  INTEGER                                                      :: ncid
  INTEGER, DIMENSION(6)                                        :: dims  
  INTEGER                                                      :: rcode, idim

!!!!!!!!!!!!! Variables
! file: netCDF file
! varname: name of variable
! nc_var_id: id of var in netCDF file
! Ndimsvar: number of dimensions of 'varname'
! shape: shape of dimensions of 'varname'

  shape=1
  dims=1

  rcode = nf_open(file, 0, ncid)
  rcode = nf_inq_varid(ncid, varname, nc_var_id)
  rcode = nf_inq_varndims(ncid, nc_var_id, Ndimsvar)
  rcode = nf_inq_vardimid(ncid, nc_var_id, dims)

  DO idim=1, Ndimsvar
    rcode = nf_inq_dimlen(ncid, dims(idim), shape(idim))
  END DO

  IF (debg) THEN
    PRINT *,"'"//TRIM(varname)//"' information from '"//TRIM(file)//"'_____________"
    PRINT *,'variable id:',nc_var_id
    PRINT *,'number of dimensions:',Ndimsvar
    PRINT *,'dimensions:',dims
    PRINT *,'shape of variable:',shape
  END IF

  RETURN
END SUBROUTINE variable_inf

SUBROUTINE read_var3D(debg, file, var, d1, d2, d3, variable3D)
! Subroutine to read a 3D variable from a file

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  CHARACTER(LEN=50), INTENT(IN)                                :: var
  CHARACTER(LEN=500), INTENT(IN)                               :: file
  INTEGER, INTENT(IN)                                          :: d1, d2, d3
  REAL, DIMENSION(d1, d2, d3), INTENT(OUT)                     :: variable3D
  LOGICAL, INTENT(IN)                                          :: debg

! Local
  INTEGER                                                      :: ncid, rcode
  INTEGER                                                      :: idvar
  INTEGER                                                      :: varfound
  CHARACTER(LEN=50)                                            :: section

!!!!!!!!!!!!!!!! Variables
! file: file to obtain variable
! var: variable name
! d1, d2, d3: range of dimensions
! r_dims: range of dimensions
! variable3D: 3-dimensional variable

! Adquiring variable
!!
  section='read_var3d'
  PRINT *,'var name: ',var
  rcode = nf_open(file, 0, ncid)

  varfound=0

  IF (debg) PRINT *,"Reading in file: '"//TRIM(file)//"' ..."
  IF (rcode /= 0) PRINT *,"Error in '"//TRIM(section)//"' "//nf_strerror(rcode)
  rcode = nf_inq_varid(ncid, TRIM(var), idvar)

  IF (rcode /= 0) PRINT *,"Error in '"//TRIM(section)//"' "//nf_strerror(rcode)
  rcode = nf_get_var_real ( ncid, idvar, variable3D )

  IF (rcode == 0) THEN
    varfound=1
  ELSE
    PRINT *,"Error reading '"//TRIM(var)//"' "//nf_strerror(rcode)
  END IF

  IF (varfound /= 1) THEN
    PRINT *,"Necessary variable '"//TRIM(var)//"' not found!"
    STOP
  ENDIF
  rcode = nf_close(ncid)

  RETURN
END SUBROUTINE read_var3D

SUBROUTINE read_var4D(debg, file, var, d1, d2, d3, d4, variable4D)
! Subroutine to read a 4D variable from a file

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  CHARACTER(LEN=50), INTENT(IN)                                :: var
  CHARACTER(LEN=500), INTENT(IN)                               :: file
  INTEGER, INTENT(IN)                                          :: d1, d2, d3, d4 
  REAL, DIMENSION(d1, d2, d3, d4), INTENT(OUT)                 :: variable4D
  LOGICAL, INTENT(IN)                                          :: debg

! Local
  INTEGER                                                      :: ncid, rcode
  INTEGER                                                      :: idvar
  INTEGER                                                      :: varfound
  CHARACTER(LEN=50)                                            :: section                 

!!!!!!!!!!!!!!!! Variables
! file: file to obtain variable
! var: variable name
! d1, d2, d3, d4: range of dimensions
! r_dims: range of dimensions
! variable4D: 4-dimensional variable 

! Adquiring variable
!!
  section='read_var4d'
  PRINT *,'var name: ',var
  rcode = nf_open(file, 0, ncid) 

  varfound=0

  IF (debg) PRINT *,"Reading in file: '"//TRIM(file)//"' ..."
  IF (rcode /= 0) PRINT *,"Error in '"//TRIM(section)//"' "//nf_strerror(rcode)
  rcode = nf_inq_varid(ncid, TRIM(var), idvar)

  IF (rcode /= 0) PRINT *,"Error in '"//TRIM(section)//"' "//nf_strerror(rcode)
  rcode = nf_get_var_real ( ncid, idvar, variable4D ) 

  IF (rcode == 0) THEN
    varfound=1 
  ELSE
    PRINT *,"Error reading '"//TRIM(var)//"' "//nf_strerror(rcode)
  END IF

  IF (varfound /= 1) THEN
    PRINT *,"Necessary variable '"//TRIM(var)//"' not found!" 
    STOP 
  ENDIF
  rcode = nf_close(ncid)

  RETURN
END SUBROUTINE read_var4D

SUBROUTINE variable_inf_ascii(debg, var, Ndim, shape, dim_names, longdesc, units) 
! Subroutine to read variable information from 'variables.inf' external ASCII file. File format:
!*var*
! variable_dim          Ndim 
! variable_dim_shape    shape (coma separated) 
! variable_dim_names    dim_names (coma separated; inverse order of shape)
! varaible_longdesc     longdesc 
! variable_units        units 
!    (blank line)
!*var*
! (...)

  IMPLICIT NONE

  CHARACTER(LEN=50), INTENT(IN)                          :: var
  INTEGER, INTENT(OUT)                                   :: Ndim
  INTEGER, DIMENSION(6), INTENT(OUT)                     :: shape
  CHARACTER(LEN=50), DIMENSION(Ndim), INTENT(OUT)        :: dim_names
  CHARACTER(LEN=250), INTENT(OUT)                        :: longdesc
  CHARACTER(LEN=50), INTENT(OUT)                         :: units
  LOGICAL, INTENT(IN)                                    :: debg
! Local variables
  INTEGER                                                :: i, ilin
  INTEGER                                                :: iunit, ios
  INTEGER                                                :: Llabel, posvar
  CHARACTER(LEN=1)                                       :: car
  CHARACTER(LEN=50)                                      :: label 
  LOGICAL                                                :: is_used

!!!!!!!!!!!!!! Variables
!  var: variable name
!  Ndim: number of dimension of variable
!  shape: order of id dimensions of varible
!  dim_names: names of variable dimensions
!  longdesc: long description of variable
!  units: units of variable
!  posvar: position of variable inside information file

!  Read parameters from variables information file 'variables.inf' 
   DO iunit=10,100
     INQUIRE(unit=iunit, opened=is_used)
     IF (.not. is_used) EXIT
   END DO
   OPEN(iunit, file='variables.inf', status='old', form='formatted', iostat=ios)
   IF ( ios /= 0 ) STOP "ERROR opening 'variables.inf'"
   ilin=1
   posvar=0

   DO 
     READ(iunit,*,END=100)label
     Llabel=LEN_TRIM(label)
     IF (label(2:Llabel-1)==TRIM(var)) THEN
       posvar=ilin
       EXIT
     END IF
     ilin=ilin+1
   END DO

 100 CONTINUE

   IF (posvar == 0) THEN
     PRINT *,"Variable '"//TRIM(var)//"' not found in 'variables.inf'"
     STOP
   END IF

   REWIND (iunit)

   DO ilin=1, posvar-1
     READ(iunit,*)car
   END DO
   shape=1

   READ(iunit,*)car
   READ(iunit,*)label, Ndim
   READ(iunit,*)label, (shape(i), i=1, Ndim)
   READ(iunit,*)label, (dim_names(i), i=1, Ndim)
   READ(iunit,*)label, longdesc
   READ(iunit,*)label, units 

   CLOSE(iunit)

   IF (debg) THEN
     PRINT *,"Read information for '"//TRIM(var)//"' variable________"
     PRINT *,'  Number of dimensions:', Ndim
     PRINT *,'  Shape of dimensions:', (shape(i), i=1,Ndim)
     PRINT *,'  Dimensions names:', (TRIM(dim_names(i)//', '), i=1,Ndim-1), TRIM(dim_names(Ndim))
     PRINT *,'  Long description:', TRIM(longdesc)
     PRINT *,'  Units:', TRIM(units)
   END IF
  
END SUBROUTINE variable_inf_ascii 

SUBROUTINE variable_Ndims_ascii(debg, var, Ndim)
! Subroutine to read variable number of dumensions from 'variables.inf' external ASCII file. 
! File format:
!*var*
! variable_dim          Ndim
! variable_dim_shape    shape (coma separated)
! variable_dim_names    dim_names (coma separated; inverse order of shape)
! varaible_longdesc     longdesc
! variable_units        units
!    (blank line)
!*var*
! (...)

  IMPLICIT NONE

  CHARACTER(LEN=50), INTENT(IN)                          :: var
  INTEGER, INTENT(OUT)                                   :: Ndim
  LOGICAL, INTENT(IN)                                    :: debg
! Local variables
  INTEGER                                                :: i, ilin
  INTEGER                                                :: iunit, ios
  INTEGER                                                :: Llabel, posvar
  CHARACTER(LEN=1)                                       :: car
  CHARACTER(LEN=50)                                      :: label
  LOGICAL                                                :: is_used

!!!!!!!!!!!!!! Variables
!  var: variable name
!  Ndim: number of dimension of variable
!  posvar: position of variable inside information file

!  Read parameters from variables information file 'variables.inf'
   DO iunit=10,100
     INQUIRE(unit=iunit, opened=is_used)
     IF (.not. is_used) EXIT
   END DO
   OPEN(iunit, file='variables.inf', status='old', form='formatted', iostat=ios)
   IF ( ios /= 0 ) STOP "ERROR opening 'variables.inf'"
   ilin=1
   posvar=0
   DO
     READ(iunit,*,END=100)label
     Llabel=LEN_TRIM(label)
     IF (label(2:Llabel-1)==TRIM(var)) THEN
       posvar=ilin
       EXIT
     END IF
     ilin=ilin+1
   END DO

 100 CONTINUE

   IF (posvar == 0) THEN
     PRINT *,"Variable '"//TRIM(var)//"' not found in 'variables.inf'"
     STOP
   END IF

   REWIND (iunit)

   DO ilin=1,posvar-1
     READ(iunit,*)car
   END DO
   READ(iunit,*)label
   READ(iunit,*)label, Ndim

   CLOSE(iunit)

   IF (debg) THEN
     PRINT *,"Read information for '"//TRIM(var)//"' variable________"
     PRINT *,'  Number of dimensions:', Ndim
   END IF

END SUBROUTINE variable_Ndims_ascii

SUBROUTINE var_range_dimensions(file, dbg, var, ndims, dnames, dimsval)
! Subroutine to obtain range of dimensions of a netCDF file

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER                                                :: idim
  CHARACTER(LEN=500), INTENT(IN)                         :: file
  CHARACTER(LEN=50), INTENT(IN)                          :: var
  LOGICAL, INTENT(IN)                                    :: dbg
  INTEGER, INTENT(IN)                                    :: ndims
  CHARACTER(LEN=50), DIMENSION(ndims), INTENT(IN)        :: dnames
  INTEGER, DIMENSION(ndims), INTENT(OUT)                 :: dimsval
! Local
  INTEGER                                                :: inc_dim, nc_ndims, nc_nvars,        &
    nc_ngatts, nc_nunlimdimid
  INTEGER                                                :: rcode, ncid
  INTEGER                                                :: nc_dimvalue
  CHARACTER(LEN=50)                                      :: nc_dimname

!!!!!!!!!!!!!!!! Variables
! file: netCDF file
! ndims: number of dimensions
! dnames: names of dimensions of variable
! dimsval: vector with ranges of all dimensions
! nc_ndims: number of dimensions of netCDF file
! nc_nvars: number of variables of netCDF file
! nc_ngatts: number of global attributes of netCDF file
! nc_nunlimdimid: id of unlimit variable of netCDF file

  ncid=25
  rcode = nf_open(file, 0, ncid)
  rcode = nf_inq(ncid, nc_ndims, nc_nvars, nc_ngatts, nc_nunlimdimid)

  DO inc_dim=1, nc_ndims
    rcode = nf_inq_dim(ncid, inc_dim, nc_dimname, nc_dimvalue)
!    IF (dbg) PRINT *,'netCDF shape of '//TRIM(nc_dimname),': ',nc_dimvalue
    DO idim=1, Ndims
      IF (TRIM(nc_dimname) == TRIM(dnames(idim))) dimsval(idim)=nc_dimvalue
    END DO
  END DO

  IF (dbg) THEN
    PRINT *,'Dimensions of file______________'
    DO idim=1,ndims
      PRINT *,TRIM(dnames(idim)),': ',dimsval(idim)
    END DO
  END IF
  rcode = nf_close(ncid)
  RETURN

END SUBROUTINE var_range_dimensions

SUBROUTINE nc_dimensions(file, dbg, ndims, Xdimname, Ydimname, Zdimname, Tdimname, dimsval,     &
  dimsname, dx, dy, dz, dt) 
! Subroutine to obtain range of dimensions of a netCDF file

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER                                                :: idim
  CHARACTER(LEN=500), INTENT(IN)                         :: file
  LOGICAL, INTENT(IN)                                    :: dbg
  INTEGER, INTENT(IN)                                    :: ndims
  CHARACTER(LEN=50), INTENT(IN)                          :: Xdimname, Ydimname, Zdimname, Tdimname
  INTEGER, DIMENSION(ndims), INTENT(OUT)                 :: dimsval
  CHARACTER(LEN=50), DIMENSION(ndims), INTENT(OUT)       :: dimsname
  INTEGER, INTENT(OUT)                                   :: dx, dy, dz, dt
  INTEGER                                                :: rcode, ncid
  CHARACTER(LEN=50)                                      :: dimname

!!!!!!!!!!!!!!!! Variables
! file: netCDF file
! ndims: number of dimensions
! [X/Y/Z/T]dimname: name of [X/Y/Z/T] dimensions
! dimsval: vector with ranges of all dimensions
! dimsname: vector with names of all dimensions
! dx, dy, dz, dt: ranges of X, Y, Z, T dimension 
  
  ncid=1
  rcode = nf_open(file, 0, ncid)

  DO idim=1, ndims
    rcode = nf_inq_dim(ncid, idim, dimsname(idim), dimsval(idim))
    IF (TRIM(dimsname(idim)) == TRIM(Xdimname(1:48))) dx=dimsval(idim)
    IF (TRIM(dimsname(idim)) == TRIM(Ydimname(1:48))) dy=dimsval(idim)
    IF (TRIM(dimsname(idim)) == TRIM(Zdimname(1:48))) dz=dimsval(idim)
    IF (TRIM(dimsname(idim)) == TRIM(Tdimname(1:48))) dt=dimsval(idim)
  END DO

  IF (dbg) THEN
    PRINT *,'Dimensions of file______________'
    DO idim=1,ndims
      PRINT *,dimsname(idim),':',dimsval(idim)
    END DO
    PRINT *,'File dimensions. dimx:',dx,' dimy:',dy,' dimz:',dz,' dimt:',dt
  END IF
  rcode = nf_close(ncid)
  RETURN

END SUBROUTINE nc_dimensions

SUBROUTINE nc_Ndim(file, dbg, ndims, nvars, ngatts, nunlimdimid) 
! Subroutine to obtain number of dimensions of a netCDF file 

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'

  CHARACTER(LEN=500), INTENT(IN)                         :: file 
  LOGICAL, INTENT(IN)                                    :: dbg
  INTEGER, INTENT(OUT)                                   :: ndims, nvars, ngatts, nunlimdimid
  INTEGER                                                :: rcode, ncid

!!!!!!!!!!!!!!!!! Variables
! file: netCDF file with path
! ndims: number of dimensions
! nvars: number of variables
! ngatts: number of global attributes
! nunlimdimid: id dimension with unlimit range

  ncid=1
  rcode = nf_open(file, 0, ncid)
  rcode = nf_inq(ncid, ndims, nvars, ngatts, nunlimdimid)
  IF (rcode /= 0) PRINT *,rcode,nf_strerror(rcode)
  IF (dbg) THEN
    PRINT *,' INPUT file has = ',ndims, ' dimensions, '
    PRINT *,'                  ',nvars, ' variables, and '
    PRINT *,'                  ',ngatts,' global attributes '
    PRINT *,'  '
  ENDIF
  rcode = nf_close(file)

  RETURN
END SUBROUTINE nc_Ndim

SUBROUTINE string_values(string, dbg, Nvalues, Lvalues, values)
! Subroutine to obtain values from a 'namelist' string separated by comas

  IMPLICIT NONE 

  INTEGER                                                    :: i, jval
  INTEGER                                                    :: Lstring
  INTEGER                                                    :: ival, eval
  INTEGER, INTENT(IN)                                        :: Nvalues, Lvalues
  CHARACTER(LEN=3000), INTENT(IN)                            :: string
  CHARACTER(LEN=Lvalues), DIMENSION(Nvalues), INTENT(OUT)    :: values
  LOGICAL, INTENT(IN)                                        :: dbg

  Lstring = LEN_TRIM(string)

  ival=1
  jval=1
  DO i=1,Lstring
    IF (string(i:i) == ',') THEN
      eval=i-1
      values(jval)=ADJUSTL(string(ival:eval))
      ival=i+1
      jval=jval+1
    END IF
  
  END DO

  values(jval)=ADJUSTL(string(ival:Lstring))

  IF (dbg) THEN
    PRINT *,'***************'//TRIM(string)//'*************'
    PRINT *,'Found values_______________'
    DO jval=1,Nvalues
      PRINT *,'************'//TRIM(values(jval))//'********'
    END DO
  END IF
  RETURN

END SUBROUTINE string_values

SUBROUTINE number_values(string, Nvalues)
! Subroutine to obtain number of variables from a 'namelist' variable with coma separation

  IMPLICIT NONE

  INTEGER                                                :: i, Lstring
  CHARACTER(LEN=3000), INTENT(IN)                        :: string
  INTEGER, INTENT(OUT)                                   :: Nvalues

  Lstring = len_trim(string)

  Nvalues=1

  DO i=1,Lstring

    IF (string(i:i) == ',') THEN
      Nvalues=Nvalues+1
    END IF
  END DO

!  PRINT *,'There are ',Nvalues,' values'
  RETURN

END SUBROUTINE number_values

CHARACTER FUNCTION spaces(N)
! Function to give a number of spaces 

  IMPLICIT NONE

  INTEGER                                                :: icar
  INTEGER, INTENT(IN)                                    :: N

  spaces=''
  DO icar=1,N
    spaces(icar:icar)=' '
  END DO

  RETURN
END FUNCTION spaces


