MODULE module_tools
  CONTAINS
! Subroutines/functions to make some general processes 
! GMS. UC: January 2010. version v0.0
! Following previous work of many authors for vis5D as 'userfuncs'
!
!!!!!!!!!! COMPILATION
!
!! OCEANO: pgf90 module_tools.f90 -L/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/lib -lnetcdf -lm -I/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/include -Mfree -c module_tools
!
!!!!!!!!! Subroutines/Functions
! diagnostic_inf: Subroutine to read diagnostic variable information from 
!     'variables_diagnostics.inf' external ASCII file
! diagnostic_inf_Ninvar: Subroutine to give number of input variables to copute varDIAG diagnostic
! def_nc_var: Subroutine to define a variable inside a netCDF file 
! attribute_REALvalue: Subroutine to obtain a real value from an attribute from a netCDF file
! nc_dimensions: Subroutine to obtain range of dimensions of a netCDF file
! nc_Ndim: Subroutine to obtain number of dimensions of a netCDF file
! string_real: Subroutine to transform a string to a real value
! string_Realvalues: Subroutine to obtain real values from a 'namelist' string separated by comas
! string_values: Subroutine to obtain values from a 'namelist' string separated by comas
! number_values: Subroutine to obtain number of variables from a 'namelist' variable with coma    
!    separation

SUBROUTINE diagnostic_inf(debg, varDIAG, Ninvar, varinnames, NdimDIAG, shapeDIAG, longdescDIAG, &
  unitsDIAG)
! Subroutine to read diagnostic variable information from 'variables_diagnostics.inf' external 
! ASCII file. File format:
!*varDIAG*
! Num_input_variables:  Ninvar 
! Input_variables:      varinnames (coma separated) 
! diagnostic_dim:       NdimDIAG 
! diagnostic_dim_shape: shapeDIAG (coma separated) 
! diagnostic_longdesc:  longdescDIAG 
! diagnostic_units:     unitsDIAG 
!    (blank line)
!*varDIAG*
! (...)

  IMPLICIT NONE

  CHARACTER(LEN=50), INTENT(IN)                          :: varDIAG
  INTEGER, INTENT(IN)                                    :: Ninvar
  INTEGER, INTENT(OUT)                                   :: NdimDIAG
  INTEGER, DIMENSION(6), INTENT(OUT)                     :: shapeDIAG
  CHARACTER(LEN=50), DIMENSION(Ninvar), INTENT(OUT)      :: varinnames
  CHARACTER(LEN=250), INTENT(OUT)                        :: longdescDIAG
  CHARACTER(LEN=50), INTENT(OUT)                         :: unitsDIAG
  LOGICAL, INTENT(IN)                                    :: debg
! Local variables
  INTEGER                                                :: i, ilin
  INTEGER                                                :: iunit, ios
  INTEGER                                                :: Llabel, posvarDIAG
  CHARACTER(LEN=1)                                       :: car
  CHARACTER(LEN=50)                                      :: label 
  LOGICAL                                                :: is_used

!!!!!!!!!!!!!! Variables
!  varDIAG: variable diagnostic name
!  Ninvar: number of input variables necessary to compute diagnostic variable
!  varinnames: varable input names variables
!  NdimDIAG: number of dimension of diagnostic variable
!  shapeDIAG: order of id dimensions of diagnostic varible
!  longdescDIAG: long description of diagnostic variable
!  unitsDIAG: units of diagnostic variable
!  posvarDIAG: position of diagnostic variable inside information file

!  Read parameters from diagnostic variables information file 'variables_diagnostics.inf' 
   DO iunit=10,100
     INQUIRE(unit=iunit, opened=is_used)
     IF (.not. is_used) EXIT
   END DO
   OPEN(iunit, file='variables_diagnostics.inf', status='old', form='formatted', iostat=ios)
   IF ( ios /= 0 ) STOP "ERROR opening 'variables_diagnostics.inf'"
   ilin=1
   posvarDIAG=0
   DO 
     READ(iunit,*,END=100)label
     Llabel=LEN_TRIM(label)
     IF (label(2:Llabel-1)==TRIM(varDIAG)) THEN
       posvarDIAG=ilin
       EXIT
     END IF
     ilin=ilin+1
   END DO

 100 CONTINUE

   IF (posvarDIAG == 0) THEN
     PRINT *,"Diagnostic variable '"//TRIM(varDIAG)//"' not found in 'variables_diagnostics.inf'"
     STOP
   END IF

   REWIND (iunit)

   DO ilin=1,posvarDIAG
     READ(iunit,*)car
   END DO
   shapeDIAG=1

   READ(iunit,*)car, car
   READ(iunit,*)car, (varinnames(i),i=1,Ninvar)
   READ(iunit,*)car, NdimDIAG
   READ(iunit,*)car, (shapeDIAG(i), i=1,NdimDIAG)
   READ(iunit,*)car, longdescDIAG
   READ(iunit,*)car, unitsDIAG 

   CLOSE(iunit)

   IF (debg) THEN
     PRINT *,"Read information for '"//TRIM(varDIAG)//"' variable________"
     PRINT *,'  Number of necessary input variables:', Ninvar
     PRINT *,'  Name of input variables:', ('  '//TRIM(varinnames(i)//'  '),i=1,Ninvar)
     PRINT *,'  Number of dimensions:', NdimDIAG
     PRINT *,'  Shape of dimensions:', (shapeDIAG(i), i=1,NdimDIAG)
     PRINT *,'  Long description:', TRIM(longdescDIAG)
     PRINT *,'  Units:', TRIM(unitsDIAG)
   END IF
  
END SUBROUTINE diagnostic_inf

SUBROUTINE diagnostic_inf_Ninvar(varDIAG, debg, Ninvar)
! Subroutine to give number of input variables to copute varDIAG diagnostic from
!  'variables_diagnostics.inf' external ASCII file. File format:
!*varDIAG*
! Num_input_variables:  Ninvar
! Input_variables:      varinnames (coma separated)
! diagnostic_dim:       NdimDIAG
! diagnostic_dim_shape: shapeDIAG (coma separated)
! diagnostic_longdesc:  longdescDIAG
! diagnostic_units:     unitsDIAG
!    (blank line)
!*varDIAG*
! (...)

  IMPLICIT NONE

  CHARACTER(LEN=50), INTENT(IN)                          :: varDIAG
  LOGICAL, INTENT(IN)                                    :: debg
  INTEGER, INTENT(OUT)                                   :: Ninvar
! Local variables
  INTEGER                                                :: iunit, ios, ilin
  INTEGER                                                :: posvarDIAG, Llabel
  CHARACTER(LEN=1)                                       :: car
  CHARACTER(LEN=50)                                      :: label
  LOGICAL                                                :: is_used

!  Read parameters from diagnostic variables information file 'variables_diagnostics.inf'
   DO iunit=10,100
     INQUIRE(unit=iunit, opened=is_used)
     IF (.not. is_used) EXIT
   END DO
   OPEN(iunit, file='variables_diagnostics.inf', status='old', form='formatted', iostat=ios)
   IF ( ios /= 0 ) STOP "ERROR opening 'variables_diagnostics.inf'"
   ilin=1
   posvarDIAG=0
   DO
     READ(iunit,*,END=100)label
     Llabel=LEN_TRIM(label)
     IF (label(2:Llabel-1)==TRIM(varDIAG)) THEN
       posvarDIAG=ilin
       EXIT
     END IF
     ilin=ilin+1
   END DO

 100 CONTINUE

   IF (posvarDIAG == 0) THEN
     PRINT *,"Diagnostic variable '"//TRIM(varDIAG)//"' not found in 'variables_diagnostics.inf'"
     STOP
   END IF

  REWIND (iunit)

   DO ilin=1,posvarDIAG
     READ(iunit,*)car
   END DO

   READ(iunit,*)car,Ninvar

   IF (debg) THEN
     PRINT *,"Read information for '"//TRIM(varDIAG)//"' variable________"
     PRINT *,' Number of necessary input variables:', Ninvar
   END IF
   close(iunit)

END SUBROUTINE diagnostic_inf_Ninvar

SUBROUTINE def_nc_var (mcid, ivar, cval, itype, idm, jshape, order, desc, units, stag, coord, debg )
! Subroutine to define a variable inside a netCDF file

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER, INTENT(IN)                                    :: mcid, ivar
  CHARACTER(LEN=50)                                      :: cval
  INTEGER                                                :: itype, idm
  INTEGER, DIMENSION(6)                                  :: jshape
  CHARACTER(LEN=3)                                       :: order
  CHARACTER(LEN=250)                                     :: desc
  CHARACTER(LEN=50)                                      :: units
  CHARACTER(LEN=1)                                       :: stag
  CHARACTER(LEN=20)                                      :: coord
  INTEGER                                                :: rcode, ilen
  CHARACTER (LEN=60)                                     :: att_text
  CHARACTER (LEN=50)                                     :: section
  LOGICAL, INTENT(IN)                                    :: debg

!!!!!!!!!! Variables
! mcid: netCDF file id
! ivar: variable id
! cval: variable name
! itype: type of variable (1: ; 2: ; 3: ; 4: ; 5: real)
! idm: number of dimensions of variable
! jshape: shape of variable (which coordinates)
! order: memory order of coordinates attribute
! desc: description attribute of varible
! units: units attribute of variable
! stag: staggered attribute of variable
! coord: coordinates labels attribute of variable
! section: name of subroutine
 
  section='def_nc_var'

  IF ( itype == 5 ) THEN
!    IF (debg) PRINT *,'Fixing netCDF id...'
!    rcode = nf_redef(mcid)
!    IF (rcode /= 0) PRINT *,"Error in '"//TRIM(section)//"' "//nf_strerror(rcode)
    IF (debg) PRINT *,'Defining real variable'
    rcode = nf_def_var(mcid, trim(cval), NF_REAL, idm, jshape, ivar)
    IF (rcode /= 0) PRINT *,"Error in '"//TRIM(section)//"' "//nf_strerror(rcode)
    IF (debg) PRINT *,'Adding real attribute in field'
    rcode = nf_put_att_int(mcid, ivar, "FieldType", NF_INT, 1, 104)
    IF (rcode /= 0) PRINT *,"Error in '"//TRIM(section)//"' "//nf_strerror(rcode)
  ENDIF

  IF (debg) PRINT *,"Adding MemoryOrder attribute of '"//TRIM(cval)//"' in netCDF file"
  att_text = order
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, ivar, "MemoryOrder", ilen, att_text(1:ilen) )
  IF (rcode /= 0) PRINT *,"Error in '"//TRIM(section)//"' "//nf_strerror(rcode)
  att_text = ' '

  IF (debg) PRINT *,"Adding decription attribute of '"//TRIM(cval)//"' in netCDF file"
  att_text = desc
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, ivar, "description", ilen, att_text(1:ilen) )
  IF (rcode /= 0) PRINT *,"Error in '"//TRIM(section)//"' "//nf_strerror(rcode)
  att_text = ' '

  IF (debg) PRINT *,"Adding units attribute of '"//TRIM(cval)//"' in netCDF file"
  att_text = units
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, ivar, "units", ilen, att_text(1:ilen) )
  IF (rcode /= 0) PRINT *,"Error in '"//TRIM(section)//"' "//nf_strerror(rcode)
  att_text = ''

  IF (debg) PRINT *,"Adding stagger attribute of '"//TRIM(cval)//"' in netCDF file"
  att_text = stag
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, ivar, "stagger", ilen, att_text(1:ilen) )
  IF (rcode /= 0) PRINT *,"Error in '"//TRIM(section)//"' "//nf_strerror(rcode)
  att_text = ''

  IF (debg) PRINT *,"Adding coordinates attribute of '"//TRIM(cval)//"' in netCDF file"
  att_text = coord
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, ivar, "coordinates", ilen, att_text(1:ilen) )
  IF (rcode /= 0) PRINT *,"Error in '"//TRIM(section)//"' "//nf_strerror(rcode)
  att_text = ''

  IF (debg) PRINT *,"Adding missing attribute of '"//TRIM(cval)//"' in netCDF file"
  att_text = '-99999.' 
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, ivar, "missing_value", ilen, att_text(1:ilen) )
  IF (rcode /= 0) PRINT *,"Error in '"//TRIM(section)//"' "//nf_strerror(rcode)
  att_text = ''

  rcode = nf_enddef(mcid)
  IF (rcode /= 0) PRINT *,'Error in '//TRIM(section)//nf_strerror(rcode)

END SUBROUTINE def_nc_var

SUBROUTINE attribute_REALvalue(file, dbg, attributename, value)
! Subroutine to obtain a real value from an attribute from a netCDF file

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  CHARACTER(LEN=500), INTENT(IN)                         :: file
  CHARACTER(LEN=50), INTENT(IN)                          :: attributename
  REAL, INTENT(OUT)                                      :: value
  LOGICAL, INTENT(IN)                                    :: dbg
!!! Local vars
  INTEGER                                                :: ncid, attid
  INTEGER                                                :: rcode
  CHARACTER(LEN=50)                                      :: section

  section='attribute_REALvalue'

  rcode = nf_open(file, 0, ncid)

  PRINT *,'Attribute name: '//TRIM(attributename)
  rcode =  nf_get_att_real(ncid, attid, TRIM(attributename), value)
  IF (rcode /= 0) PRINT *,"Error in '"//TRIM(section)//"' "//nf_strerror(rcode)

  IF (dbg) THEN
    PRINT *,"Attribute: #",attid," '"//TRIM(attributename)//'" :',value
  END IF

  rcode = nf_close(ncid)

  RETURN
END SUBROUTINE attribute_REALvalue

SUBROUTINE nc_dimensions(file, dbg, ndims, Xdimname, Ydimname, Zdimname, Tdimname, dimsval,      &
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
  
  rcode = nf_open(file, 0, ncid)

  DO idim=1, ndims

    rcode = nf_inq_dim(ncid, idim, dimsname(idim), dimsval(idim))
    IF (TRIM(dimsname(idim)) == TRIM(Xdimname)) dx=dimsval(idim)
    IF (TRIM(dimsname(idim)) == TRIM(Ydimname)) dy=dimsval(idim)
    IF (TRIM(dimsname(idim)) == TRIM(Zdimname)) dz=dimsval(idim)
    IF (TRIM(dimsname(idim)) == TRIM(Tdimname)) dt=dimsval(idim)
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

  rcode = nf_open(file, 0, ncid)
  rcode = nf_inq(ncid, ndims, nvars, ngatts, nunlimdimid)
  IF (dbg) THEN
    PRINT *,' INPUT file has = ',ndims, ' dimensions, '
    PRINT *,'                  ',nvars, ' variables, and '
    PRINT *,'                  ',ngatts,' global attributes '
    PRINT *,'  '
  ENDIF
  rcode = nf_close(file)

  RETURN
END SUBROUTINE nc_Ndim

SUBROUTINE string_real( string, value)
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

SUBROUTINE string_values(string, dbg, Nvalues, values)
! Subroutine to obtain values from a 'namelist' string separated by comas

  IMPLICIT NONE 

  INTEGER                                                :: i, jval
  INTEGER                                                :: Lstring
  INTEGER                                                :: ival, eval
  INTEGER, INTENT(IN)                                    :: Nvalues
  CHARACTER(LEN=3000), INTENT(IN)                        :: string
  CHARACTER(LEN=250), DIMENSION(Nvalues), INTENT(OUT)    :: values
  LOGICAL, INTENT(IN)                                    :: dbg

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

END MODULE module_tools
