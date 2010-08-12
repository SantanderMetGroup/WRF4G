PROGRAM diagnostics_computation
  USE module_gen_tools
  USE module_nc_tools
  USE module_com_diagnostics

! Program to compute diagnostics variables from netCDF fields in vertical p coordinates
! GMS. UC: December 2009. version v0.0
! Following previous work of many authors for vis5D as 'userfuncs'
!
!!!!!!!!!! COMPILATION
!
!! OCEANO: pgf90 diagnostics_coputation.f90 -L/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/lib -lnetcdf -lm -I/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/include -Mfree -o diagnostics_coputation

  IMPLICIT NONE

!  INCLUDE 'netcdf.inc'

!   567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567

! Namelist variables
  INTEGER                                                :: grid_filt, ntimes_filt, debug,      &
    global_att_file
  CHARACTER(LEN=4)                                       :: process, lev_process
  CHARACTER(LEN=3000)                                    :: diagnostics, input_names, p_levels
  CHARACTER(LEN=250)                                     :: path_to_input, path_to_output,      & 
    path_to_geofile, geofile, outfile
  CHARACTER(LEN=50)                                      :: WEdimname, SNdimname, BTdimname,    &
    Timedimname, X_grid_spacing, Y_grid_spacing, WE_dimvarname, SN_dimvarname, BT_dimvarname,   &
    Time_dimvarname
  LOGICAL                                                :: filt, cartesian_x, cartesian_y

! General variables
  INTEGER                                                :: i
  INTEGER                                                :: funit, ios, lent
  INTEGER                                                :: rcode, ncid, ndims, nvars, ngatts,  &
    nunlimdimid
  INTEGER                                                :: ierr
  INTEGER                                                :: Ninputs, Ndiagnostics, Nlevels,     &
    Nvardiag
  INTEGER                                                :: dimx, dimy, dimz, dimt
  CHARACTER(LEN=250), ALLOCATABLE, DIMENSION(:)          :: diags 
  CHARACTER(LEN=500)                                     :: ingeofile, outputfile
  CHARACTER(LEN=250), ALLOCATABLE, DIMENSION(:)          :: innamefiles
  CHARACTER(LEN=500), ALLOCATABLE, DIMENSION(:)          :: infiles
  REAL, ALLOCATABLE, DIMENSION(:)                        :: levs
  LOGICAL                                                :: is_used
  INTEGER, ALLOCATABLE, DIMENSION(:)                     :: TOTdims
  INTEGER                                                :: nTOTdims, nTOTvar, nTOTatt, IDunlimit
  CHARACTER(LEN=50), ALLOCATABLE, DIMENSION(:)           :: TOTdimsname
  CHARACTER(LEN=50), DIMENSION(4)                        :: dimsvarname
  REAL                                                   :: X_grid, Y_grid

  NAMELIST /io/ path_to_input, input_names, cartesian_x, cartesian_y, WEdimname, SNdimname,     &
    BTdimname, Timedimname,       &
    path_to_output, WE_dimvarname, SN_dimvarname, BT_dimvarname, Time_dimvarname, diagnostics,  &
    process, debug, filt, grid_filt, ntimes_filt, path_to_geofile, geofile, outfile,            &
    global_att_file
  NAMELIST /levels/ lev_process, p_levels, X_grid_spacing, Y_grid_spacing

!!!!!!!!! Variables
! Ninputs: number of input files
! Ndiagnostics: number of diagnostic variables
! Nlevels: number of vertical levels
! diags: vector with diagnostic names
! ingeofile: file with geographic information
! innamefiles: vector with all input files
! infiles: vector with all input path-files
! levs: vector with vertical levels to compute diagnostics
! Nvardiag: Number of input variables necessary for a diagnostic computation
! dimx, dimy, dimz, dimt: 4D ranges of netCDF files
! TOTdims: vector with ranges of all dimensions of netCDF file
! nTOTdims: number of dimensions of netCDF file
! nTOTvar: total number of variables of netCDF file
! nTOTatt: total number of attributes of netCDF file
! IDunlimit: id dimension with unlimit range
! TOTdimsname: vector with names of all dimensions of netCDF file
! [X/Y]_grid: grid spacing in X/Y grid direction [m]
! dimsvarname: vector with variable names of dimensions

!!!!!!!!!!! Subroutines
! com_diagnostics: Subroutine to compute all wanted diagnostics

! Reading parameters from namelist
  DO funit=10,100
    INQUIRE(unit=funit, opened=is_used)
    IF (.not. is_used) EXIT
  END DO
  OPEN(funit, file='namelist.diagnostics', status='old', form='formatted', iostat=ios)
  IF ( ios /= 0 ) STOP "ERROR opening 'namelist.diagnostics'"
  READ(funit,io)
  READ(funit,levels)
  CLOSE(funit)

  lent = len_trim(path_to_input)
  IF ( path_to_input(lent:lent) /= "/" ) THEN
    path_to_input = TRIM(path_to_input)//"/"
  ENDIF
  lent = len_trim(path_to_output)
  IF ( path_to_output(lent:lent) /= "/" ) THEN
    path_to_output = TRIM(path_to_output)//"/"
  ENDIF

! Geofile name file
  ingeofile=''
  lent = LEN_TRIM(geofile)
  IF (lent > 1) THEN
    lent = LEN_TRIM(path_to_geofile)
    IF ( path_to_geofile(lent:lent) /= "/" ) path_to_geofile = TRIM(path_to_geofile)//"/"
    ingeofile = TRIM(path_to_geofile)//TRIM(geofile)
  ELSE
    ingeofile = ''
  ENDIF

  ingeofile = TRIM(path_to_geofile)//TRIM(geofile)
  outputfile = TRIM(path_to_output)//TRIM(outfile)
  PRINT *,'Output will be written in: '//TRIM(outputfile)

  PRINT *,'Constructing files names'
! Constructing files names
  CALL number_values(input_names, debug, Ninputs)
  ALLOCATE(innamefiles(Ninputs))
  CALL string_values(input_names, debug, Ninputs, innamefiles)

  PRINT *,'There are',Ninputs,'input files'
  DO i=1,Ninputs
    PRINT *,i,TRIM(innamefiles(i))
  END DO
  
! Variables with dimensions names
  dimsvarname(1)=WE_dimvarname
  dimsvarname(2)=SN_dimvarname
  dimsvarname(3)=BT_dimvarname
  dimsvarname(4)=Time_dimvarname

! Constructing diagnostics names
  CALL number_values(diagnostics, debug, Ndiagnostics)
  PRINT *,'Are required',Ndiagnostics,' diagnostics'
  IF (ALLOCATED(diags)) DEALLOCATE(diags)
  ALLOCATE(diags(Ndiagnostics), STAT=ierr)
  IF (ierr /= 0 ) PRINT *,"Error allocating 'diags'"
  diags=' '
  CALL string_values(diagnostics, debug, Ndiagnostics, diags)

  DO i=1,Ndiagnostics
    PRINT *,i,TRIM(diags(i))
  END DO

! Constructing vertical levels
  CALL number_values(p_levels, debug, Nlevels)
  ALLOCATE(levs(Nlevels))
  CALL string_Realvalues(debug, p_levels, Nlevels, levs)

  PRINT *,'There are',Nlevels,'input levels'
  DO i=1,Nlevels
    PRINT *,i,levs(i)
  END DO

! Constructing all paths-files structures
  IF (ALLOCATED(infiles)) DEALLOCATE(infiles)
  IF (LEN_TRIM(ingeofile) > 3 ) THEN
    IF (debug >= 20) PRINT *,"A 'geofile' is given by 'namelist' "//TRIM(ingeofile)//'***'
    Ninputs=Ninputs+1
    ALLOCATE(infiles(Ninputs))
    DO i=1,Ninputs-1
      infiles(i)=TRIM(path_to_input)//TRIM(innamefiles(i))
    END DO
    infiles(Ninputs)=TRIM(ingeofile)
  ELSE
    ALLOCATE(infiles(Ninputs))
    DO i=1,Ninputs
      infiles(i)=TRIM(path_to_input)//TRIM(innamefiles(i))
    END DO
  END IF

  IF (debug >= 20) THEN
    PRINT *,'Input files________'
    DO i=1, Ninputs
      PRINT *,i,' :',TRIM(infiles(i))
    END DO
  END IF

  CALL nc_Ndim(infiles(1), debug, nTOTdims, nTOTvar, nTOTatt, IDunlimit)
  IF (ALLOCATED(TOTdims)) DEALLOCATE(TOTdims)
  ALLOCATE(TOTdims(nTOTdims), TOTdimsname(nTOTdims))
  
  IF (debug <= 100 ) CALL nc_gatts(infiles(1), debug)

  CALL nc_dimensions(infiles(1), debug, nTOTdims, WEdimname, SNdimname, BTdimname, Timedimname, &
    TOTdims, TOTdimsname, dimx, dimy, dimz, dimt)

  CALL gattribute_REALvalue(infiles(1), debug, X_grid_spacing, 1, X_grid)
  CALL gattribute_REALvalue(infiles(1), debug, Y_grid_spacing, 1, Y_grid)

  CALL com_diagnostics(debug, infiles, Ninputs, dimx, dimy, dimz, dimt, diags, Ndiagnostics,    &
    X_grid, Y_grid, dimsvarname, outputfile, global_att_file, cartesian_x, cartesian_y)

END PROGRAM diagnostics_computation
