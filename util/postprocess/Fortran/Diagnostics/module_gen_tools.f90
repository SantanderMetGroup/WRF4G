MODULE module_gen_tools
  CONTAINS
! Subroutines/functions to make some general processes 
! GMS. UC: January 2010. version v0.0
! Following previous work of many authors for vis5D as 'userfuncs'
!
!!!!!!!!! Subroutines/Functions
! attribute_REALvalue: Subroutine to obtain a real value from an attribute from a netCDF file
! diag_fatal: Subroutine to give fatal error with a message
! diagnostic_inf: Subroutine to read diagnostic variable information from 
!     'variables_diagnostics.inf' external ASCII file
! diagnostic_inf_Ninvar: Subroutine to give number of input variables to copute varDIAG diagnostic
! diag_fatal: Subroutine to give fatal error with a message
! def_nc_var: Subroutine to define a variable inside a netCDF file 
! fill_inputs: Subroutine to fill a kind-shape of input real fields
! nc_dimensions: Subroutine to obtain range of dimensions of a netCDF file
! nc_Ndim: Subroutine to obtain number of dimensions of a netCDF file
! search_variables: Subroutine to search variables from a given netcCDF file
! string_real: Subroutine to transform a string to a real value
! string_Realvalues: Subroutine to obtain real values from a 'namelist' string separated by comas
! string_values: Subroutine to obtain values from a 'namelist' string separated by comas
! number_values: Subroutine to obtain number of variables from a 'namelist' variable with coma    
!    separation

SUBROUTINE attribute_REALvalue(file, dbg, attributename, value)
! Subroutine to obtain a real value from an attribute from a netCDF file

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  CHARACTER(LEN=500), INTENT(IN)                         :: file
  CHARACTER(LEN=50), INTENT(IN)                          :: attributename
  REAL, INTENT(OUT)                                      :: value
  INTEGER, INTENT(IN)                                    :: dbg
!!! Local vars
  INTEGER                                                :: ncid, attid
  INTEGER                                                :: rcode
  CHARACTER(LEN=50)                                      :: section

  section='attribute_REALvalue'

  rcode = nf_open(file, 0, ncid)

  PRINT *,'Attribute name: '//TRIM(attributename)
  rcode =  nf_get_att_real(ncid, attid, TRIM(attributename), value)
  IF (rcode /= 0) PRINT *,"Error in '"//TRIM(section)//"' "//nf_strerror(rcode)

  IF (dbg >= 100) THEN
    PRINT *,"Attribute: #",attid," '"//TRIM(attributename)//'" :',value
  END IF

  rcode = nf_close(ncid)

  RETURN
END SUBROUTINE attribute_REALvalue

SUBROUTINE diag_fatal(msg)
! Subroutine to give fatal error with a message

  IMPLICIT NONE
  
  CHARACTER(LEN=250), INTENT(IN)                          :: msg
  
  PRINT *,msg
  STOP

END SUBROUTINE diag_fatal

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
  INTEGER, INTENT(IN)                                    :: debg
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

   IF (debg >= 75) THEN
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
  INTEGER, INTENT(IN)                                    :: debg
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

   IF (debg >= 75) THEN
     PRINT *,"Read information for '"//TRIM(varDIAG)//"' variable________"
     PRINT *,' Number of necessary input variables:', Ninvar
   END IF
   close(iunit)

END SUBROUTINE diagnostic_inf_Ninvar

SUBROUTINE def_nc_var (mcid, ivar, cval, itype, idm, jshape, order, desc, units, stag, coord, debg )
! Subroutine to define a variable inside a netCDF file

  USE module_constants

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
  CHARACTER (LEN=50)                                     :: section, errmsg
  INTEGER, INTENT(IN)                                    :: debg

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
 
  section="'def_nc_var'"

  IF ( itype == 5 ) THEN
!    IF (debg >= 100) PRINT *,'Fixing netCDF id...'
!    rcode = nf_redef(mcid)
!    IF (rcode /= 0) PRINT *,errmsg//" in "//TRIM(section)//" "//nf_strerror(rcode)
    IF (debg >= 100) PRINT *,'Defining real variable'
    rcode = nf_def_var(mcid, trim(cval), NF_REAL, idm, jshape, ivar)
    IF (rcode /= 0) PRINT *,errmsg//" in "//TRIM(section)//" "//nf_strerror(rcode)
    IF (debg >= 100) PRINT *,'Adding real attribute in field'
    rcode = nf_put_att_int(mcid, ivar, "FieldType", NF_INT, 1, 104)
    IF (rcode /= 0) PRINT *,errmsg//" in "//TRIM(section)//" "//nf_strerror(rcode)
  ENDIF

  IF (debg >= 100) PRINT *,"Adding MemoryOrder attribute of '"//TRIM(cval)//"' in netCDF file"
  att_text = order
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, ivar, "MemoryOrder", ilen, att_text(1:ilen) )
  IF (rcode /= 0) PRINT *,errmsg//" in "//TRIM(section)//" "//nf_strerror(rcode)
  att_text = ' '

  IF (debg >= 100) PRINT *,"Adding decription attribute of '"//TRIM(cval)//"' in netCDF file"
  att_text = desc
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, ivar, "description", ilen, att_text(1:ilen) )
  IF (rcode /= 0) PRINT *,errmsg//" in "//TRIM(section)//" "//nf_strerror(rcode)
  att_text = ' '

  IF (debg >= 100) PRINT *,"Adding units attribute of '"//TRIM(cval)//"' in netCDF file"
  att_text = units
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, ivar, "units", ilen, att_text(1:ilen) )
  IF (rcode /= 0) PRINT *,errmsg//" in "//TRIM(section)//" "//nf_strerror(rcode)
  att_text = ''

  IF (debg >= 100) PRINT *,"Adding stagger attribute of '"//TRIM(cval)//"' in netCDF file"
  att_text = stag
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, ivar, "stagger", ilen, att_text(1:ilen) )
  IF (rcode /= 0) PRINT *,errmsg//" in "//TRIM(section)//" "//nf_strerror(rcode)
  att_text = ''

  IF (debg >= 100) PRINT *,"Adding coordinates attribute of '"//TRIM(cval)//"' in netCDF file"
  att_text = coord
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, ivar, "coordinates", ilen, att_text(1:ilen) )
  IF (rcode /= 0) PRINT *,errmsg//" in "//TRIM(section)//" "//nf_strerror(rcode)
  att_text = ''

  IF (debg >= 100) PRINT *,"Adding missing attribute of '"//TRIM(cval)//"' in netCDF file"
  att_text = '-99999.' 
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, ivar, "missing_value", ilen, att_text(1:ilen) )
  IF (rcode /= 0) PRINT *,errmsg//" in "//TRIM(section)//" "//nf_strerror(rcode)
  att_text = ''

  rcode = nf_enddef(mcid)
  IF (rcode /= 0) PRINT *,errmsg//' in '//TRIM(section)//nf_strerror(rcode)

END SUBROUTINE def_nc_var

SUBROUTINE fill_inputs(debg, ncs, Nncs, fvars, dimMin, matin)
! Subroutine to fill a kind-shape of input real fields

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'

  INTEGER, INTENT(IN)                                     :: debg, Nncs 
  INTEGER, DIMENSION(2), INTENT(IN)                       :: fvars
  INTEGER, DIMENSION(6), INTENT(IN)                       :: dimMin
  REAL, DIMENSION(dimMin(1), dimMin(2), dimMin(3),                                               &
    dimMin(4), dimMin(5), dimMin(6)), INTENT(OUT)         :: matin
!  REAL, DIMENSION(:,:,:,:,:,:), ALLOCATABLE               :: matin
  CHARACTER(LEN=500), DIMENSION(Nncs), INTENT(IN)         :: ncs
! Local
  INTEGER                                                 :: rcode, ncid
  CHARACTER(LEN=50)                                       :: section, errmsg

  section="'fill_inputs'"

!  IF (ALLOCATED(matin)) DEALLCOATE(matin)
!  ALLOCATE(matin(dimMin(1), dimMin(2), dimMin(3), dimMin(4), dimMin(5), dimMin(6))))
  
  rcode = nf_open(ncs(fvars(1)), 0, ncid)
  IF (rcode /= 0) PRINT *,errmsg//" in "//TRIM(section)//" "//nf_strerror(rcode)
  rcode = nf_get_var_real ( ncid, fvars(2), matin )
  IF (rcode /= 0) PRINT *,errmsg//" in "//TRIM(section)//" "//nf_strerror(rcode)
  rcode = nf_close(ncid)
  IF (rcode /= 0) PRINT *,errmsg//" in "//TRIM(section)//" "//nf_strerror(rcode)
  IF (debg >= 100) PRINT *,'Memory loaded input variable. Value (dimN/2)=', matin(dimMin(1)/2,  &
    dimMin(2)/2, dimMin(3)/2, dimMin(4)/2, dimMin(5)/2, dimMin(6)/2)

!  DEALLOCATE(matin)

  RETURN
END SUBROUTINE fill_inputs

SUBROUTINE nc_dimensions(file, dbg, ndims, Xdimname, Ydimname, Zdimname, Tdimname, dimsval,      &
  dimsname, dx, dy, dz, dt) 
! Subroutine to obtain range of dimensions of a netCDF file

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER                                                :: idim
  CHARACTER(LEN=500), INTENT(IN)                         :: file
  INTEGER, INTENT(IN)                                    :: dbg
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

  IF (dbg >= 20) THEN
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
  INTEGER, INTENT(IN)                                    :: dbg
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
  IF (dbg >= 20) THEN
    PRINT *,' INPUT file has = ',ndims, ' dimensions, '
    PRINT *,'                  ',nvars, ' variables, and '
    PRINT *,'                  ',ngatts,' global attributes '
    PRINT *,'  '
  ENDIF
  rcode = nf_close(file)

  RETURN
END SUBROUTINE nc_Ndim

SUBROUTINE search_variables(dbg, ncs, Nnc, svars, Nsvars, fvars, dimfvars)
! Subroutine to search a list of 'Nsvars' variables from a given set of 'Nnc' netcCDF files 

  USE module_constants
!  USE module_calc_tools

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER, INTENT(IN)                                     :: dbg, Nnc, Nsvars
  CHARACTER(LEN=500), DIMENSION(Nnc), INTENT(IN)          :: ncs
  CHARACTER(LEN=50), DIMENSION(Nsvars), INTENT(IN)        :: svars
  INTEGER, DIMENSION(Nsvars, 2), INTENT(OUT)              :: fvars
  INTEGER, DIMENSION(Nsvars, 6), INTENT(OUT)              :: dimfvars

! Local
  INTEGER                                                 :: ifile, ivar, idim
  INTEGER                                                 :: rcode, ncid, idvar
  INTEGER                                                 :: Ndimfvar, ncNdims
  CHARACTER(LEN=50)                                       :: section, errmsg
  INTEGER, DIMENSION(6)                                   :: ndimsfvar
  INTEGER, DIMENSION(:), ALLOCATABLE                      :: ncdims
  CHARACTER(LEN=250)                                      :: messg

!!!!!!!!!! Variables
! ncs: vector with the names of netCDF files
! Nnc: number of netCDF files
! svars: vector with name of wanted variables
! Nsvars: number of variables to search
! fvars: matrix of location of desired variables (line as variable number according to svars)
!    col1: number of files (according to ncs)    col2: id variable in from file of 'col1'
! dimfvars: shape of fvars
! Ndimfvar: number of dimensions of fvar
! ndimsfvar: id dimension of dimensions of fvar
! ncNdims: number of dimensions of nerCDF file
! ncdims: shape dimensions of netCDF file

  section="'module_tools'"  

  IF (dbg >= 75) PRINT *,"Searching variables in '"//section//"'..."
  fvars=0
  files_loop: DO ifile=1, Nnc
    rcode = nf_open(TRIM(ncs(ifile)), 0, ncid)
    IF (dbg >= 20) PRINT *,"Reading in file: '"//TRIM(ncs(ifile))//"' ..."
    IF (rcode /= 0) PRINT *, TRIM(errmsg)//" in '"//TRIM(section)//"' "//nf_strerror(rcode)
    rcode = nf_inq_ndims(ncid, ncNdims)
    
    IF (ALLOCATED(ncdims)) DEALLOCATE(ncdims)
    ALLOCATE(ncdims(ncNdims))
    
    DO idim=1,ncNdims
      rcode = nf_inq_dimlen(ncid, idim, ncdims(idim))
    END DO

    DO ivar=1, Nsvars
      dimfvars=1
      rcode = nf_inq_varid(ncid, TRIM(svars(ivar)), idvar)
      IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in '"//TRIM(section)//"' "//nf_strerror(rcode)
      IF (fvars(ivar,1) == 0 ) THEN
        fvars(ivar,1)=ifile
	fvars(ivar,2)=idvar
	rcode = nf_inq_varndims (ncid, idvar, Ndimfvar)
	rcode = nf_inq_vardimid (ncid, idvar, ndimsfvar)
	dimfvars(ivar,1:Ndimfvar) = ncdims(ndimsfvar(1:Ndimfvar))
	
        IF (dbg >= 75) PRINT *,"Variable: '"//TRIM(svars(ivar))//"' found in '"//TRIM(ncs(ifile))&
	  //' var id:',fvars(ivar,2),' of dimensions: ',dimfvars(ivar,1:Ndimfvar)
      END IF
    END DO
    rcode = nf_close(ncid)
    DEALLOCATE (ncdims)
  END DO files_loop

  DO ivar=1, Nsvars
    IF (fvars(ivar,1)==0) PRINT *,TRIM(errmsg)//" variable: '"//TRIM(svars(ivar))//"' NOT found!"
  END DO
  
  messg='Some variables have not been not found !!'
  IF (.NOT.(ALL(fvars(:,1) == 0))) CALL diag_fatal(messg)

  RETURN

END SUBROUTINE search_variables

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
  INTEGER, INTENT(IN)                                    :: dbg

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

  IF (dbg >= 100) THEN
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

END MODULE module_gen_tools
