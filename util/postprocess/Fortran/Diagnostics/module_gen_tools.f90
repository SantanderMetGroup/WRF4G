MODULE module_gen_tools
  USE module_constants
  
  CONTAINS
! Subroutines/functions to make some general processes 
! GMS. UC: January 2010. version v0.0
! Following previous work of many authors for vis5D as 'userfuncs'
!
!!!!!!!!! Subroutines/Functions
! attribute_REALvalue: Subroutine to obtain a real value from an attribute from a netCDF file
! copy_nc_att: Subroutine to copy all global attributes from a netCDF file to other open one
! def_nc_var: Subroutine to define a variable inside a netCDF file 
! def_nc_att_text: Subroutine to define a global text attribute inside a netCDF file 
! diag_fatal: Subroutine to give fatal error with a message
! diagnostic_inf: Subroutine to read diagnostic variable information from 
!     'variables_diagnostics.inf' external ASCII file
! diagnostic_inf_Ninvar: Subroutine to give number of input variables to copute varDIAG diagnostic
! diag_fatal: Subroutine to give fatal error with a message
! fill_inputs_real: Subroutine to fill a kind-shape of input real fields
! halfdim: Function to give the half value of a dimension
! nc_atts: Subroutine to print all global attributes of a netCDF file 
! nc_dimensions: Subroutine to obtain range of dimensions of a netCDF file
! nc_Ndim: Subroutine to obtain number of dimensions of a netCDF file
! search_variables: Subroutine to search variables from a given netcCDF file
! string_real: Subroutine to transform a string to a real value
! string_Realvalues: Subroutine to obtain real values from a 'namelist' string separated by comas
! string_values: Subroutine to obtain values from a 'namelist' string separated by comas
! number_values: Subroutine to obtain number of variables from a 'namelist' variable with coma    
!    separation

SUBROUTINE attribute_REALvalue(file, debg, attributename, attelement, value)
! Subroutine to obtain a real value from an attribute from a netCDF file

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  CHARACTER(LEN=500), INTENT(IN)                         :: file
  CHARACTER(LEN=50), INTENT(IN)                          :: attributename
  REAL, INTENT(OUT)                                      :: value
  INTEGER, INTENT(IN)                                    :: debg, attelement
!!! Local vars
  INTEGER                                                :: ncid, attid, attlen, atttype
  INTEGER                                                :: rcode
  CHARACTER(LEN=50)                                      :: section, attname
  INTEGER                                                :: ndims, nvars, ngatts, nunlimdimid
  REAL, DIMENSION(:), ALLOCATABLE                        :: attrealvalues
  CHARACTER(LEN=250)                                     :: message

!!!!!!!!!!!!!!! Variables
! file: netCDF file
! attributename: name of attribute
! attelement: element of values to given back
! value: values recoded in attribute (only attelement component)
! 

  section="'attribute_REALvalue'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'

  rcode = nf_open(file, 0, ncid)
  rcode = nf_inq(ncid, ndims, nvars, ngatts, nunlimdimid)

  IF (debg >= 100) PRINT *,"Reading in fle '"//TRIM(file)//"'"
  
  PRINT *,"Attribute name: '"//TRIM(attributename)//"' "
  rcode = nf_inq_atttype(ncid, NF_GLOBAL, attributename, atttype)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)

! If attribute is not real (TYPE =5) stops
!!
  message = 'In '//TRIM(section)//" real attribute (type = 5) '"//TRIM(attributename)//              &
    "' is desired but it is of type: "//CHAR(48+atttype)
  IF (atttype /= 5) CALL diag_fatal(message)

! Looking for attribute length
!!
  rcode = nf_inq_attlen(ncid, NF_GLOBAL, attributename, attlen)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)

! Allocating and getting value
!!
  IF (ALLOCATED(attrealvalues)) DEALLOCATE(attrealvalues)
  ALLOCATE (attrealvalues(attlen))
  rcode = nf_get_att_real(ncid, NF_GLOBAL, attributename, attrealvalues)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  IF (debg >= 75) PRINT *,"attribute: '"//TRIM(attributename)//' values: ',attrealvalues(1:attlen)

  rcode = nf_close(ncid)
  value=attrealvalues(attelement)
  IF (debg >= 75) PRINT *,'giving back value: ',value

  DEALLOCATE(attrealvalues)
  RETURN
END SUBROUTINE attribute_REALvalue

SUBROUTINE copy_nc_att(debg, mcid, file)
! Subroutine to copy all global attributes from a netCDF file to other open one

  USE module_constants
  
  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'
  
  INTEGER, INTENT(IN)                                     :: debg, mcid
  CHARACTER(LEN=500), INTENT(IN)                          :: file
  
! Local
  INTEGER                                                 :: iatt, rcode
  INTEGER                                                 :: ncid
  INTEGER                                                 :: ndims, nvars, ngatts, nunlimdimid
  CHARACTER(LEN=50)                                       :: section, attname

!!!!!!!!!! Variables
! mcid: netCDF id to which attributes will be copied
! file: netCDF file from which attributes will be copied

  section="'copy_nc_att'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'
  
  rcode = nf_open(TRIM(file), 0, ncid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)

  rcode = nf_inq(ncid, ndims, nvars, ngatts, nunlimdimid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  
  DO iatt=1, ngatts
    rcode = nf_inq_attname(ncid, NF_GLOBAL, iatt, attname)
    rcode = nf_copy_att (ncid, NF_GLOBAL, attname, mcid, NF_GLOBAL)
    IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  END DO
  
  rcode = nf_close(ncid)

END SUBROUTINE copy_nc_att

SUBROUTINE def_nc_att_text (debg, mcid, atdesc, atvalue)
! Subroutine to define a global text attribute inside a netCDF file

  USE module_constants

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'
  
  INTEGER, INTENT(IN)                                     :: mcid, debg
  CHARACTER(LEN=50), INTENT(IN)                           :: atdesc
  CHARACTER(LEN=100), INTENT(IN)                          :: atvalue
  
! Local
  INTEGER                                                 :: rcode, ilen
  CHARACTER(LEN=50)                                       :: section
  CHARACTER(LEN=100)                                      :: att_text

!!!!!!!!! Variables
! mcid: id of netCDF to include a text attribute
! atdesc: name of attribute
! atvalue: value of new attribute

  section="'def_nc_att_text'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'
  
  IF (debg >= 100) PRINT *,"Adding global attribute '"//TRIM(atdesc)//"' in netCDF file"
  att_text = atvalue
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, NF_GLOBAL, TRIM(atdesc), ilen, att_text(1:ilen) )
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  att_text = ' '

END SUBROUTINE def_nc_att_text


SUBROUTINE def_nc_var (mcid, ivar, cval, itype, idm, jshape, order, desc, stddesc, units, stag, &
  coord, debg )
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
  CHARACTER(LEN=50)                                      :: units, stddesc
  CHARACTER(LEN=1)                                       :: stag
  CHARACTER(LEN=20)                                      :: coord
  INTEGER                                                :: rcode, ilen
  CHARACTER (LEN=60)                                     :: att_text
  CHARACTER (LEN=50)                                     :: section
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
! stddesc: standard (CF convection, http://cf-pcmdi.llnl.gov/) name of variable
! units: units attribute of variable
! stag: staggered attribute of variable
! coord: coordinates labels attribute of variable
! section: name of subroutine
 
  section="'def_nc_var'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'

  IF ( itype == 5 ) THEN
!    IF (debg >= 100) PRINT *,'Fixing netCDF id...'
!    rcode = nf_redef(mcid)
!    IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
    IF (debg >= 100) PRINT *,'Defining real variable'
    rcode = nf_def_var(mcid, trim(cval), NF_REAL, idm, jshape, ivar)
    IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
    IF (debg >= 100) PRINT *,'Adding real attribute in field'
    rcode = nf_put_att_int(mcid, ivar, "FieldType", NF_INT, 1, 104)
    IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  ENDIF

  IF (debg >= 100) PRINT *,"Adding MemoryOrder attribute of '"//TRIM(cval)//"' in netCDF file"
  att_text = order
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, ivar, "MemoryOrder", ilen, att_text(1:ilen) )
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  att_text = ' '

  IF (debg >= 100) PRINT *,"Adding long_name attribute of '"//TRIM(cval)//"' in netCDF file"
  att_text = desc
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, ivar, "long_name", ilen, att_text(1:ilen) )
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  att_text = ' '

  IF (debg >= 100) PRINT *,"Adding standard_name attribute of '"//TRIM(cval)//"' in netCDF file"
  att_text = stddesc
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, ivar, "standard_name", ilen, att_text(1:ilen) )
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  att_text = ' '

  IF (debg >= 100) PRINT *,"Adding units attribute of '"//TRIM(cval)//"' in netCDF file"
  att_text = units
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, ivar, "units", ilen, att_text(1:ilen) )
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  att_text = ''

  IF (debg >= 100) PRINT *,"Adding stagger attribute of '"//TRIM(cval)//"' in netCDF file"
  att_text = stag
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, ivar, "stagger", ilen, att_text(1:ilen) )
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  att_text = ''

  IF (debg >= 100) PRINT *,"Adding coordinates attribute of '"//TRIM(cval)//"' in netCDF file"
  att_text = coord
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, ivar, "coordinates", ilen, att_text(1:ilen) )
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  att_text = ''

  IF (debg >= 100) PRINT *,"Adding missing attribute of '"//TRIM(cval)//"' in netCDF file"
  att_text = '-99999.' 
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, ivar, "missing_value", ilen, att_text(1:ilen) )
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  att_text = '-99999.'
  rcode = nf_put_att_text(mcid, ivar, "_Fillvalue", ilen, att_text(1:ilen) )
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  att_text = ''

  rcode = nf_enddef(mcid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//' in '//TRIM(section)//nf_strerror(rcode)

END SUBROUTINE def_nc_var

SUBROUTINE diag_fatal(msg)
! Subroutine to give fatal error with a message

  IMPLICIT NONE
  
  CHARACTER(LEN=250), INTENT(IN)                          :: msg
  
  PRINT *,msg
  STOP

END SUBROUTINE diag_fatal

SUBROUTINE diagnostic_inf(debg, varDIAG, Ninvar, varinnames, NdimDIAG, shapeDIAG, longdescDIAG, &
  stddescDIAG, unitsDIAG)
! Subroutine to read diagnostic variable information from 'variables_diagnostics.inf' external 
! ASCII file. File format:
!*varDIAG*
! Num_input_variables:  Ninvar 
! Input_variables:      varinnames (coma separated) 
! diagnostic_dim:       NdimDIAG 
! diagnostic_dim_shape: shapeDIAG (coma separated) 
! diagnostic_longdesc:  longdescDIAG 
! diagnostic_std_name:  stddescDIAG
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
  CHARACTER(LEN=50), INTENT(OUT)                         :: unitsDIAG, stddescDIAG
  INTEGER, INTENT(IN)                                    :: debg
! Local variables
  INTEGER                                                :: i, ilin
  INTEGER                                                :: iunit, ios
  INTEGER                                                :: Llabel, posvarDIAG
  CHARACTER(LEN=1)                                       :: car
  CHARACTER(LEN=50)                                      :: label, section
  LOGICAL                                                :: is_used

!!!!!!!!!!!!!! Variables
!  varDIAG: variable diagnostic name
!  Ninvar: number of input variables necessary to compute diagnostic variable
!  varinnames: varable input names variables
!  NdimDIAG: number of dimension of diagnostic variable
!  shapeDIAG: order of id dimensions of diagnostic varible
!  longdescDIAG: long description of diagnostic variable
!  stddescDIAG: standard CF convection name
!  unitsDIAG: units of diagnostic variable
!  posvarDIAG: position of diagnostic variable inside information file

  section="'diagnostic_inf'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'

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
   READ(iunit,*)car, stddescDIAG   
   READ(iunit,*)car, unitsDIAG 

   CLOSE(iunit)

   IF (debg >= 75) THEN
     PRINT *,"Read information for '"//TRIM(varDIAG)//"' variable________"
     PRINT *,'  Number of necessary input variables:', Ninvar
     PRINT *,'  Name of input variables:', ('  '//TRIM(varinnames(i)//'  '),i=1,Ninvar)
     PRINT *,'  Number of dimensions:', NdimDIAG
     PRINT *,'  Shape of dimensions:', (shapeDIAG(i), i=1,NdimDIAG)
     PRINT *,'  Long description:', TRIM(longdescDIAG)
     PRINT *,'  CF-standard name:', TRIM(stddescDIAG)
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
  CHARACTER(LEN=50)                                      :: label, section
  LOGICAL                                                :: is_used

  section="'diagnostic_inf_Ninvar'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'

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

SUBROUTINE fill_inputs_real(debg, ncs, Nncs, fvars, dimMin, matin)
! Subroutine to fill a kind-shape of input real fields

  USE module_constants

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'

  INTEGER, INTENT(IN)                                     :: debg, Nncs 
  INTEGER, DIMENSION(2), INTENT(IN)                       :: fvars
  INTEGER, DIMENSION(6), INTENT(IN)                       :: dimMin
  REAL, DIMENSION(dimMin(1), dimMin(2), dimMin(3),                                               &
    dimMin(4), dimMin(5), dimMin(6)), INTENT(OUT)         :: matin
  CHARACTER(LEN=500), DIMENSION(Nncs), INTENT(IN)         :: ncs

! Local
  INTEGER                                                 :: rcode, ncid
  CHARACTER(LEN=50)                                       :: section
!  INTEGER                                                 :: halfdim

!!!!!!!!!! Variables
! Nncs: number of input files
! ncs: netCDF input files
! fvars: file (col1) and position in it (col2) of variable
! dimMinn: dimensions of variable
! matin: martix with values of variable

!!!!!!!!!! Function
! halfdim: Function to give the half value of a dimension

  section="'fill_inputs'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'
  IF (debg >= 75) THEN
    PRINT *,'Filling matrices of dimension: ',UBOUND(matin)
    PRINT *,"Variable in file: '"//TRIM(ncs(fvars(1)))//"' with id:",fvars(2)
  END IF
  rcode = nf_open(TRIM(ncs(fvars(1))), 0, ncid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  rcode = nf_get_var_real ( ncid, fvars(2), matin )
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  rcode = nf_close(ncid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)

  IF (debg >= 100) PRINT *,'Memory loaded input variable. Value (dimN/2)=',                     &
    matin(halfdim(dimMin(1)), halfdim(dimMin(2)), halfdim(dimMin(3)), halfdim(dimMin(4)),       &
      halfdim(dimMin(5)), halfdim(dimMin(6)))
  RETURN
  
  CONTAINS

    INTEGER FUNCTION halfdim(dim)
!     Function to give the half value of a dimension

!      IMPLICIT NONE
  
      INTEGER, INTENT(IN)                                     :: dim
  
      halfdim = dim/2
      IF (dim < 2) halfdim = 1     ! Assuming non-zero dimension range
  
    END FUNCTION halfdim

END SUBROUTINE fill_inputs_real

INTEGER FUNCTION halfdim(dim)
! Function to give the half value of a dimension

  IMPLICIT NONE
  
  INTEGER, INTENT(IN)                                     :: dim
  
  halfdim = dim/2
  IF (dim < 2) halfdim = 1     ! Assuming non-zero dimension range
  
END FUNCTION halfdim

SUBROUTINE nc_atts(fileinf, debg) 
! Subroutine to print all global attributes of a netCDF file 

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'

  CHARACTER(LEN=500), INTENT(IN)                         :: fileinf
  INTEGER, INTENT(IN)                                    :: debg
! Local
  INTEGER                                                :: iatt, j
  INTEGER                                                :: rcode, ncid, ndims, nvars, ngatts,  &
    nunlimdimid, atttype, attlen
  CHARACTER(LEN=50)                                      :: section
  CHARACTER(LEN=100)                                     :: attname
  INTEGER, DIMENSION(:), ALLOCATABLE                     :: attintvalues
  INTEGER, PARAMETER                                     :: long = SELECTED_INT_KIND(9)
  INTEGER(long), DIMENSION(:), ALLOCATABLE               :: attlongintvalues
  CHARACTER(LEN=100)                                     :: attcharvalues
  REAL, DIMENSION(:), ALLOCATABLE                        :: attrealvalues
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE            :: attdoublevalues

!!!!!!!!!!!!!!!!! Variables
! fileinf: netCDF file with path
! ngatts: number of global attributes

  section="'nc_atts'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'

  rcode = nf_open(fileinf, 0, ncid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  rcode = nf_inq(ncid, ndims, nvars, ngatts, nunlimdimid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  IF (debg >= 100) THEN
    PRINT *,'File: '//TRIM(fileinf)
    DO iatt=1, ngatts
      rcode = nf_inq_attname(ncid, NF_GLOBAL, iatt, attname)
      rcode = nf_inq_atttype(ncid, NF_GLOBAL, attname, atttype)
      rcode = nf_inq_attlen(ncid, NF_GLOBAL, attname, attlen)     
      PRINT *,'     iatt:',iatt,'***'//TRIM(attname)//'*** type:',atttype,' length: ',attlen
      SELECT CASE(atttype)
      CASE(2)
        attcharvalues=''
        rcode = nf_get_att_text(ncid, NF_GLOBAL, attname, attcharvalues)
	IF ( rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
        PRINT *,"          * value: '"//TRIM(attcharvalues)//"'"
      CASE(3)
        IF (ALLOCATED(attintvalues)) DEALLOCATE(attintvalues)
        ALLOCATE (attintvalues(attlen))
        rcode = nf_get_att_int1(ncid, NF_GLOBAL, TRIM(attname), attintvalues)
	IF ( rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
        PRINT *,'          * values:',(attintvalues(j),' ,',j=1,attlen)
        DEALLOCATE(attintvalues)
      CASE(4)
        IF (ALLOCATED(attintvalues)) DEALLOCATE(attintvalues)
        ALLOCATE (attintvalues(attlen))
        rcode = nf_get_att_int(ncid, NF_GLOBAL, TRIM(attname), attintvalues)
	IF ( rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
        PRINT *,'          * values:',(attintvalues(j),' ,',j=1,attlen)
        DEALLOCATE(attintvalues)
!      CASE(8)
!        IF (ALLOCATED(attlongintvalues)) DEALLOCATE(attlongintvalues)
!        ALLOCATE (attlongintvalues(attlen))
!        rcode = nf_get_att_int2(ncid, NF_GLOBAL, TRIM(attname), attlongintvalues)
!	IF ( rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
!        PRINT *,'          * values:',(attlongintvalues(j),' ,',j=1,attlen)
!        DEALLOCATE(attlongintvalues)
      CASE(5)
        IF (ALLOCATED(attrealvalues)) DEALLOCATE(attrealvalues)
        ALLOCATE (attrealvalues(attlen))
        rcode = nf_get_att_real(ncid, NF_GLOBAL, TRIM(attname), attrealvalues)
	IF ( rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
        PRINT *,'          * values:',(attrealvalues(j),' ,',j=1,attlen)
        DEALLOCATE(attrealvalues)
      CASE(6)
        IF (ALLOCATED(attdoublevalues)) DEALLOCATE(attdoublevalues)
        ALLOCATE (attdoublevalues(attlen))
        rcode = nf_get_att_double(ncid, NF_GLOBAL, TRIM(attname), attdoublevalues)
	IF ( rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
        PRINT *,'          * values:',(attdoublevalues(j),' ,',j=1,attlen)
        DEALLOCATE(attdoublevalues)
      CASE DEFAULT
        PRINT *,'           ----- Not printable data type -----'
      END SELECT
    END DO
  ENDIF
  rcode = nf_close(ncid)

  RETURN
END SUBROUTINE nc_atts

SUBROUTINE nc_dimensions(file, dbg, ndims, Xdimname, Ydimname, Zdimname, Tdimname, dimsval,      &
  dimsname, dx, dy, dz, dt) 
! Subroutine to obtain range of dimensions of a netCDF file

  USE module_constants
  
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
  CHARACTER(LEN=50)                                      :: dimname, section

!!!!!!!!!!!!!!!! Variables
! file: netCDF file
! ndims: number of dimensions
! [X/Y/Z/T]dimname: name of [X/Y/Z/T] dimensions
! dimsval: vector with ranges of all dimensions
! dimsname: vector with names of all dimensions
! dx, dy, dz, dt: ranges of X, Y, Z, T dimension 

  section="'nc_dimensions'"
  IF (dbg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'
  
  rcode = nf_open(file, 0, ncid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)

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

SUBROUTINE nc_Ndim(fileinf, debg, ndims, nvars, ngatts, nunlimdimid) 
! Subroutine to obtain number of dimensions of a netCDF file 

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'

  CHARACTER(LEN=500), INTENT(IN)                         :: fileinf
  INTEGER, INTENT(IN)                                    :: debg
  INTEGER, INTENT(OUT)                                   :: ndims, nvars, ngatts, nunlimdimid
! Local
  INTEGER                                                :: rcode, ncid
  CHARACTER(LEN=50)                                      :: section

!!!!!!!!!!!!!!!!! Variables
! fileinf: netCDF file with path
! ndims: number of dimensions
! nvars: number of variables
! ngatts: number of global attributes
! nunlimdimid: id dimension with unlimit range

  section="'nc_Ndim'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'

  rcode = nf_open(fileinf, 0, ncid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  rcode = nf_inq(ncid, ndims, nvars, ngatts, nunlimdimid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  IF (debg >= 20) THEN
    PRINT *,'File: '//TRIM(fileinf)
    PRINT *,' INPUT file has = ',ndims, ' dimensions, '
    PRINT *,'                  ',nvars, ' variables, and '
    PRINT *,'                  ',ngatts,' global attributes '
    PRINT *,'  '
  ENDIF
  rcode = nf_close(ncid)

  RETURN
END SUBROUTINE nc_Ndim

!SUBROUTINE put_nc_coords (debg, mcid, coordid, file, coornames)
! Subroutine to put coordinates matrixs in netCDF file taking values from another one

!  USE module_constants

!  IMPLICIT NONE

!  INCLUDE 'netcdf.inc'

!  INTEGER, INTENT(IN)                                    :: debg, mcid
!  INTEGER, DIMENSION(4), INTENT(IN)                      :: coorid
!  CHARACTER(LEN=500), INTENT(IN)                         :: file
 ! CHARACTER(LEN=50), DIMENSION (4), INTENT(IN)           :: coornames

! Local
!  INTEGER                                                :: ncid, rcode
!  INTEGER, DIMENSION(6)                                  :: jshape
!  CHARACTER(LEN=3)                                       :: order
!  CHARACTER(LEN=250)                                     :: desc
!  CHARACTER(LEN=50)                                      :: units, stddesc
!  CHARACTER(LEN=1)                                       :: stag
!  CHARACTER(LEN=20)                                      :: coord
!  INTEGER                                                :: rcode, ilen
!  CHARACTER (LEN=60)                                     :: att_text
!  CHARACTER (LEN=50)                                     :: section
!  INTEGER, INTENT(IN)                                    :: debg

!!!!!!!!!! Variables
! mcid: netCDF file id to put coordinates
! ivar: variable id
! cval: variable name
! itype: type of variable (1: ; 2: ; 3: ; 4: ; 5: real)
! idm: number of dimensions of variable
! jshape: shape of variable (which coordinates)
! order: memory order of coordinates attribute
! desc: description attribute of varible
! stddesc: standard (CF convection, http://cf-pcmdi.llnl.gov/) name of variable
! units: units attribute of variable
! stag: staggered attribute of variable
! coord: coordinates labels attribute of variable
! section: name of subroutine
 
!  section="'put_nc_coords'"
!  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'

!  IF ( itype == 5 ) THEN
!    IF (debg >= 100) PRINT *,'Fixing netCDF id...'
!    rcode = nf_redef(mcid)
!    IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
!    IF (debg >= 100) PRINT *,'Defining real variable'
!    rcode = nf_def_var(mcid, trim(cval), NF_REAL, idm, jshape, ivar)
!    IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
!    IF (debg >= 100) PRINT *,'Adding real attribute in field'
!    rcode = nf_put_att_int(mcid, ivar, "FieldType", NF_INT, 1, 104)
!    IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
!  ENDIF

!  IF (debg >= 100) PRINT *,"Adding MemoryOrder attribute of '"//TRIM(cval)//"' in netCDF file"
!  att_text = order
!  ilen = len_trim(att_text)
!  rcode = nf_put_att_text(mcid, ivar, "MemoryOrder", ilen, att_text(1:ilen) )
!  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
!  att_text = ' '

!  IF (debg >= 100) PRINT *,"Adding long_name attribute of '"//TRIM(cval)//"' in netCDF file"
!  att_text = desc
!  ilen = len_trim(att_text)
!  rcode = nf_put_att_text(mcid, ivar, "long_name", ilen, att_text(1:ilen) )
!  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
!  att_text = ' '

!  IF (debg >= 100) PRINT *,"Adding standard_name attribute of '"//TRIM(cval)//"' in netCDF file"
!  att_text = stddesc
!  ilen = len_trim(att_text)
!  rcode = nf_put_att_text(mcid, ivar, "standard_name", ilen, att_text(1:ilen) )
!  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
!  att_text = ' '

!  IF (debg >= 100) PRINT *,"Adding units attribute of '"//TRIM(cval)//"' in netCDF file"
!  att_text = units
!  ilen = len_trim(att_text)
!  rcode = nf_put_att_text(mcid, ivar, "units", ilen, att_text(1:ilen) )
!  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
!  att_text = ''

!  IF (debg >= 100) PRINT *,"Adding stagger attribute of '"//TRIM(cval)//"' in netCDF file"
!  att_text = stag
!  ilen = len_trim(att_text)
!  rcode = nf_put_att_text(mcid, ivar, "stagger", ilen, att_text(1:ilen) )
!  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
!  att_text = ''

!  IF (debg >= 100) PRINT *,"Adding coordinates attribute of '"//TRIM(cval)//"' in netCDF file"
!  att_text = coord
!  ilen = len_trim(att_text)
 ! rcode = nf_put_att_text(mcid, ivar, "coordinates", ilen, att_text(1:ilen) )
!  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
!  att_text = ''

!  IF (debg >= 100) PRINT *,"Adding missing attribute of '"//TRIM(cval)//"' in netCDF file"
!  att_text = '-99999.' 
!  ilen = len_trim(att_text)
!  rcode = nf_put_att_text(mcid, ivar, "missing_value", ilen, att_text(1:ilen) )
!  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
!  att_text = '-99999.'
!  rcode = nf_put_att_text(mcid, ivar, "_Fillvalue", ilen, att_text(1:ilen) )
!  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
!  att_text = ''

!  rcode = nf_enddef(mcid)
!  IF (rcode /= 0) PRINT *,TRIM(errmsg)//' in '//TRIM(section)//nf_strerror(rcode)

!END SUBROUTINE put_nc_coords

SUBROUTINE search_variables(debg, ncs, Nnc, svars, Nsvars, fvars, dimfvars)
! Subroutine to search a list of 'Nsvars' variables from a given set of 'Nnc' netcCDF files 

  USE module_constants
!  USE module_calc_tools

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER, INTENT(IN)                                     :: debg, Nnc, Nsvars
  CHARACTER(LEN=500), DIMENSION(Nnc), INTENT(IN)          :: ncs
  CHARACTER(LEN=50), DIMENSION(Nsvars), INTENT(IN)        :: svars
  INTEGER, DIMENSION(Nsvars, 2), INTENT(OUT)              :: fvars
  INTEGER, DIMENSION(Nsvars, 6), INTENT(OUT)              :: dimfvars

! Local
  INTEGER                                                 :: ifile, ivar, idim
  INTEGER                                                 :: rcode, ncid, idvar
  INTEGER                                                 :: Ndimfvar, ncNdims
  CHARACTER(LEN=50)                                       :: section
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

  section="'search_variables'"  
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'

  IF (debg >= 75) PRINT *,"Searching variables in "//TRIM(section)//"..."
  fvars=0
  dimfvars=1
  files_loop: DO ifile=1, Nnc
    rcode = nf_open(TRIM(ncs(ifile)), 0, ncid)
    IF (debg >= 20) PRINT *,"Reading in file: '"//TRIM(ncs(ifile))//"' ..."
    IF (rcode /= 0) PRINT *, TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
    rcode = nf_inq_ndims(ncid, ncNdims)
    
    IF (ALLOCATED(ncdims)) DEALLOCATE(ncdims)
    ALLOCATE(ncdims(ncNdims))
    
    DO idim=1,ncNdims
      rcode = nf_inq_dimlen(ncid, idim, ncdims(idim))
    END DO

! Searching variables characteristics
!!
    DO ivar=1, Nsvars
      rcode = nf_inq_varid(ncid, TRIM(svars(ivar)), idvar)
      IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
      IF (fvars(ivar,1) == 0 ) THEN
        fvars(ivar,1)=ifile
	fvars(ivar,2)=idvar
	rcode = nf_inq_varndims (ncid, idvar, Ndimfvar)
	rcode = nf_inq_vardimid (ncid, idvar, ndimsfvar)
	dimfvars(ivar,1:Ndimfvar) = ncdims(ndimsfvar(1:Ndimfvar))
	
        IF (debg >= 75) PRINT *,'Variable # ',ivar,'/',Nsvars,": '"//TRIM(svars(ivar))//        &
	  "' found in '"//TRIM(ncs(ifile))//' var id:',fvars(ivar,2),' of dimensions: ',        &
          dimfvars(ivar,1:Ndimfvar)
      END IF
    END DO
    rcode = nf_close(ncid)
    DEALLOCATE (ncdims)
  END DO files_loop

  DO ivar=1, Nsvars
    IF (fvars(ivar,1)==0) PRINT *,TRIM(errmsg)//" variable: '"//TRIM(svars(ivar))//"' NOT found!"
  END DO
  
  messg='Some variables have not been not found !!'
  IF (.NOT.(ALL(fvars(:,1) /= 0))) CALL diag_fatal(messg)
  IF (debg >= 75) THEN
    PRINT *,'Charactersitics of located fields_______________'
    DO ivar=1, Nsvars
      PRINT *,"variable: '"//TRIM(svars(ivar))//' file #',fvars(ivar,1),' position in file',    &
        fvars(ivar,2)
      PRINT *,'    dimension of variable: ',(dimfvars(ivar,idim),' ,',idim=1,5), dimfvars(ivar,6)
    END DO
  END IF

  RETURN

END SUBROUTINE search_variables

SUBROUTINE string_real(debg, string, value)
! Subroutine to transform a string to a real value

  IMPLICIT NONE

  INTEGER, INTENT(IN)                                    :: debg
  CHARACTER(LEN=20), INTENT(IN)                          :: string
  REAL, INTENT(OUT)                                      :: value       
! Local
  INTEGER                                                :: i,j
  INTEGER                                                :: Lstring, point, Lpre, Laft
  CHARACTER(LEN=50)                                      :: section

!!!!!!!!!!!!!!!! Variables
! string: string with real number
! value: real value 
! Lstring: length of string
! point: position of point
! Lpre: length of string previous to the point
! Laft: length of string after the point

  section="'string_real'"  
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'

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

SUBROUTINE string_Realvalues(debg, string, Nvalues, values)
! Subroutine to obtain real values from a 'namelist' string separated by comas

  IMPLICIT NONE

  INTEGER, INTENT(IN)                                    :: Nvalues, debg
  CHARACTER(LEN=3000), INTENT(IN)                        :: string
  REAL, DIMENSION(Nvalues), INTENT(OUT)                  :: values
! Local
  INTEGER                                                :: i, jval
  INTEGER                                                :: Lstring
  INTEGER                                                :: ival, eval, point
  CHARACTER(LEN=50)                                      :: section
  CHARACTER(LEN=20)                                      :: Svalues

  section="'string_Realvalues'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'

  Lstring = len_trim(string)

  ival=1
  jval=1
  DO i=1,Lstring
    IF (string(i:i) == ',') THEN
      eval=i-1
      Svalues=ADJUSTL(string(ival:eval))
      CALL string_real(debg, Svalues, values(jval))
      ival=i+1
      jval=jval+1
    END IF

  END DO

  Svalues=ADJUSTL(string(ival:Lstring))
  CALL string_real(debg, Svalues, values(jval))

  IF (debg >= 100) THEN
    PRINT *,'Found values_______________'
    DO jval=1,Nvalues
      PRINT *,values(jval)
    END DO
  END IF
  RETURN

END SUBROUTINE string_Realvalues

SUBROUTINE string_values(string, debg, Nvalues, values)
! Subroutine to obtain values from a 'namelist' string separated by comas

  IMPLICIT NONE 

  INTEGER, INTENT(IN)                                    :: Nvalues
  CHARACTER(LEN=3000), INTENT(IN)                        :: string
  CHARACTER(LEN=250), DIMENSION(Nvalues), INTENT(OUT)    :: values
  INTEGER, INTENT(IN)                                    :: debg
! Locals
  INTEGER                                                :: i, jval
  INTEGER                                                :: Lstring
  INTEGER                                                :: ival, eval
  CHARACTER(LEN=50)                                      :: section

  section="'string_values'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'

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

  IF (debg >= 100) THEN
    PRINT *,'Found values_______________'
    DO jval=1,Nvalues
      PRINT *,'************'//TRIM(values(jval))//'********'
    END DO
  END IF
  RETURN

END SUBROUTINE string_values

SUBROUTINE number_values(string, debg, Nvalues)
! Subroutine to obtain number of variables from a 'namelist' variable with coma separation

  IMPLICIT NONE

  INTEGER, INTENT(IN)                                    :: debg
  CHARACTER(LEN=3000), INTENT(IN)                        :: string
  INTEGER, INTENT(OUT)                                   :: Nvalues
! local
  INTEGER                                                :: i, Lstring
  CHARACTER(LEN=50)                                      :: section
  
  section="'number_values'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'

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
