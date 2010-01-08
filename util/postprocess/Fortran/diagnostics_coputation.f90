PROGRAM diagnostics_computation
! Program to compute diagnostics variables from netCDF fields in vertical p coordinates
! GMS. UC: December 2009. version v0.0
! Following previous work of many authors for vis5D as 'userfuncs'
!
!!!!!!!!!! COMPILATION
!
!! OCEANO: pgf90 diagnostics_coputation.f90 -L/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/lib -lnetcdf -lm -I/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/include -Mfree -o diagnostics_coputation

  USE module_constants
  USE module_diagnostic

  IMPLICIT NONE
  INCLUDE 'netcdf.inc'
!   567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567

! Namelist variables
  INTEGER                                                :: grid_filt, ntimes_filt
  CHARACTER(LEN=4)                                       :: process, lev_process
  CHARACTER(LEN=3000)                                    :: diagnostics, input_names, p_levels
  CHARACTER(LEN=250)                                     :: path_to_input, path_to_output,      & 
  path_to_geofile, geofile 
  CHARACTER(LEN=50)                                      :: WEattribute, SNattribute,           &
    BTattribute, Timeattribute
  LOGICAL                                                :: debug, filt

! General variables
  INTEGER                                                :: i
  INTEGER                                                :: funit, ios, lent
  INTEGER                                                :: rcode, ncid, ndims, nvars, ngatts,  &
    nunlimdimid
  INTEGER                                                :: Ninputs, Ndiagnostics, Nlevels
  INTEGER                                                :: dimx, dimy, dimz, dimt
  CHARACTER(LEN=50), ALLOCATABLE, DIMENSION(:)           :: diags 
  CHARACTER(LEN=500)                                     :: ingeofile
  CHARACTER(LEN=250), ALLOCATABLE, DIMENSION(:)          :: innamefiles
  CHARACTER(LEN=500), ALLOCATABLE, DIMENSION(:)          :: infiles
  REAL, ALLOCATABLE, DIMENSION(:)                        :: levs
  LOGICAL                                                :: is_used
  CHARACTER(LEN=11), ALLOCATABLE, DIMENSION(:)           :: DIAGvariables
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:)                  :: diagnostic4D
  REAL, ALLOCATABLE, DIMENSION(:,:,:)                    :: diagnostic3D
  REAL, ALLOCATABLE, DIMENSION(:,:)                      :: diagnostic2D

  NAMELIST /io/ path_to_input, input_names, WEattribute, SNattribute, BTattribute, Timeattribute, path_to_output, diagnostics, process, debug, filt, grid_filt, ntimes_filt, path_to_geofile, geofile 
  NAMELIST /levels/ lev_process, p_levels

!!!!!!!!! Variables
! Ninputs: number of input files
! Ndiagnostics: number of diagnostic variables
! Nlevels: number of vertical levels
! diags: vector with diagnostic names
! ingeofile: file with geographic information
! innamefiles: vector with all input files
! infiles: vector with all input path-files
! levs: vector with vertical levels to compute diagnostics
! DIAGvariables: vector with names of variables needed for a diagnostic field
! dimx, dimy, dimz, dimt: 4D ranges of netCDF files
! diagnostic4D: 4D computed diagnostic
! diagnostic4D: 3D computed diagnostic
! diagnostic4D: 2D computed diagnostic

! Reading paramters from namelist
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
  lent = len_trim(path_to_geofile)
  IF ( path_to_geofile(lent:lent) /= "/" ) THEN
    path_to_geofile = TRIM(path_to_geofile)//"/"
  ENDIF

  ingeofile = TRIM(path_to_geofile)//TRIM(geofile)

! Constructing files names
  CALL number_values(input_names, Ninputs)
  ALLOCATE(innamefiles(Ninputs))
  CALL string_values(input_names, Ninputs, innamefiles)

  PRINT *,'There are',Ninputs,'input files'
  DO i=1,Ninputs
    PRINT *,i,TRIM(innamefiles(i))
  END DO

! Constructing diagnostics names
  CALL number_values(diagnostics, Ndiagnostics)
  ALLOCATE(diags(Ndiagnostics))
  CALL string_values(diagnostics, Ndiagnostics, diags)

  PRINT *,'Are requarired',Ndiagnostics,'diagnostics'
  DO i=1,Ndiagnostics
    PRINT *,i,TRIM(diags(i))
  END DO

! Constructing vertical levels
  CALL number_values(p_levels, Nlevels)
  ALLOCATE(levs(Nlevels))
  CALL string_Realvalues(p_levels, Nlevels, levs)

  PRINT *,'There are',Nlevels,'input files'
  DO i=1,Nlevels
    PRINT *,i,levs(i)
  END DO

! Constructing all paths-files structures
  IF (ALLOCATED(infiles)) DEALLOCATE(infiles)
  IF (LEN_TRIM(geofile) > 2 ) THEN
    ALLOCATE(infiles(Ninputs+1))
    infiles(Ninputs+1)=ingeofile
  ELSE
    ALLOCATE(infiles(Ninputs))
  END IF
  DO i=1,Ninputs
    infiles(i)=TRIM(path_to_input)//TRIM(innamefiles(i))
  END DO

! GET BASIC INFORMTION ABOUT THE FILE
! most important
!   ndims:  number of dimensions
!   nvars:  number of variables
!   ngatts: number of global attributes
  PRINT *,"Openning file '"//TRIM(infiles(1))//"' to obtain main netCDF atributes"
  rcode = nf_open(infiles(1), 0, ncid)
  rcode = nf_inq(ncid, ndims, nvars, ngatts, nunlimdimid)
!  IF (rcode .NE. nf_noerr) CALL handle_err(rcode)
  IF (debug) THEN
    PRINT *,' INPUT file has = ',ndims, ' dimensions, '
    PRINT *,'                  ',nvars, ' variables, and '
    PRINT *,'                  ',ngatts,' global attributes '
    PRINT *,'=================================================='
  ENDIF
  rcode = nf_get_att_int (ncid, nf_global, TRIM(WEattribute), dimx)
  rcode = nf_get_att_int (ncid, nf_global, TRIM(SNattribute), dimy)
  rcode = nf_get_att_int (ncid, nf_global, TRIM(BTattribute), dimz)
  rcode = nf_get_att_int (ncid, nf_global, TRIM(Timeattribute), dimt)

  PRINT *,'4D ranges. dimx:',dimx,'dimy:',dimy,'dimz:',dimz,'dimt:',dimt

!!!
!!
! Computing diagnostics
!!
!!!
  IF (ALLOCATED(diagnostic4D)) DEALLOCATE(diagnostic4D)
  ALLOCATE(diagnostic4D(dimx, dimy, dimz, dimt))

  IF (ALLOCATED(DIAGvariables)) DEALLOCATE(DIAGvariables)
  ALLOCATE(DIAGvariables(6))
  DIAGvariables=RESHAPE((/'U','V','T','COR','MAPFAC','PLEV'/),(/6/))

!  CALL PV(infiles, Ninputs+1, DIAGvariables, 6, dimx, dimy, dimz, dimt, diagnostic4D)

END PROGRAM diagnostics_computation


SUBROUTINE string_real(string, value)
! Subroutine to transform a string to a real value

  IMPLICIT NONE

  INTEGER                                                :: i,j
  INTEGER                                                :: Lstring, point, Lpre, Laft
  CHARACTER(LEN=20), INTENT(IN)                          :: string
  REAL, INTENT(OUT)                                      :: value       

!!!!!!!!!!!!!!!! Variables
! string: string with real number
! value: real value 
! Lstring: length of string
! point: position of point
! Lpre: length of string previous to the point
! Laft: length of string after the point

  Lstring=LEN_TRIM(string)
  point=INDEX(string, '.')
  Lpre=point-1
  Laft=Lstring-(point+1)

  value=0.
  DO i=1,Lpre
    value=value+(IACHAR(string(i:i))-48)*10.**(Lpre-i)
  END DO 

  DO i=1,Laft
    value=value+(IACHAR(string(point+i:point+i))-48)*10.**(-i)
  END DO

!  PRINT *,'String:',string,'Lpre:',Lpre,'Laft:',Laft,'Real:',value
  RETURN
END SUBROUTINE string_real 

SUBROUTINE string_Realvalues(string, Nvalues, values)
! Subroutine to obtain real values from a 'namelist' string separated by comas

  IMPLICIT NONE

  INTEGER                                                :: i, jval
  INTEGER                                                :: Lstring
  INTEGER                                                :: ival, eval, point
  INTEGER, INTENT(IN)                                    :: Nvalues
  CHARACTER(LEN=3000), INTENT(IN)                        :: string
  CHARACTER(LEN=20)                                      :: Svalues
  REAL, DIMENSION(Nvalues), INTENT(OUT)                  :: values

  Lstring = len_trim(string)

  ival=1
  jval=1
  DO i=1,Lstring
    IF (string(i:i) == ',') THEN
      eval=i-1
      Svalues=ADJUSTL(string(ival:eval))
      CALL string_real(Svalues, values(jval))
      ival=i+1
      jval=jval+1
    END IF

  END DO

  Svalues=ADJUSTL(string(ival:Lstring))
  CALL string_real(Svalues, values(jval))

!  PRINT *,'Found values_______________'
!  DO jval=1,Nvalues
!    PRINT *,'************'//TRIM(values(jval))//'********'
!  END DO
  RETURN

END SUBROUTINE string_Realvalues

SUBROUTINE string_values(string, Nvalues, values)
! Subroutine to obtain values from a 'namelist' string separated by comas

  IMPLICIT NONE 

  INTEGER                                                :: i, jval
  INTEGER                                                :: Lstring
  INTEGER                                                :: ival, eval
  INTEGER, INTENT(IN)                                    :: Nvalues
  CHARACTER(LEN=3000), INTENT(IN)                        :: string
  CHARACTER(LEN=250), DIMENSION(Nvalues), INTENT(OUT)    :: values

  Lstring = len_trim(string)

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

!  PRINT *,'Found values_______________'
!  DO jval=1,Nvalues
!    PRINT *,'************'//TRIM(values(jval))//'********'
!  END DO
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

END SUBROUTINE  number_values

