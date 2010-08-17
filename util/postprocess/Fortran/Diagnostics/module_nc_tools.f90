MODULE module_nc_tools
  USE module_constants
  
  CONTAINS
! Subroutines/functions to make some general processes on netCDF files
! GMS. UC: January 2010. version v0.0
! Following previous work of many authors for vis5D as 'userfuncs'
!
!!!!!!!!! Subroutines/Functions
! gattribute_REALvalue: Subroutine to obtain a real value from a global attribute from a netCDF
! copy_nc_att: Subroutine to copy all global attributes from a netCDF file to other open one
! create_output: Subroutine to create netCDF output
! def_dim: Subroutine to define a 1D dimension 
! def_dim_ver: Subroutine to define a vertical coordinate
! def_dim_time: Subroutine to define a time coordinate
! def_nc_var: Subroutine to define a variable inside a netCDF file 
! def_nc_gatt_text: Subroutine to define a global text attribute inside a netCDF file 
! error_nc: Subroutine to print error nc messages
! exists_dim: Function to determine if a dimension exists
! exists_var: Function to determine if a variable exists
! fill_inputs_50char: Subroutine to fill a kind-shape of input 50-character fields
! fill_inputs_real: Subroutine to fill a kind-shape of input real fields
! nc_gatts: Subroutine to print all global attributes of a netCDF file 
! nc_dimensions: Subroutine to obtain range of dimensions of a netCDF file
! nc_Ndim: Subroutine to obtain number of dimensions of a netCDF file
! search_variables: Subroutine to search variables from a given netcCDF file

SUBROUTINE gattribute_REALvalue(file, debg, attributename, attelement, value)
! Subroutine to obtain a real value from an attribute from a netCDF file

  USE module_gen_tools, ONLY: diag_fatal

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
  CALL error_nc(section, rcode)

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
END SUBROUTINE gattribute_REALvalue

SUBROUTINE copy_nc_gatt(debg, mcid, file)
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

  rcode = nf_redef(mcid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  
  DO iatt=1, ngatts
    rcode = nf_inq_attname(ncid, NF_GLOBAL, iatt, attname)
    rcode = nf_copy_att (ncid, NF_GLOBAL, attname, mcid, NF_GLOBAL)
    IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)
  END DO

  rcode = nf_enddef(mcid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//' in '//TRIM(section)//nf_strerror(rcode)
  
  rcode = nf_close(ncid)

END SUBROUTINE copy_nc_gatt

SUBROUTINE create_output(debg, outfile, dimx, dimy, dimz, dimt, dimsvarname, file_gatt, xcar,   &
  ycar)
! create_output: Subroutine to create netCDF output

  USE module_constants

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER, INTENT(IN)                                    :: dimx, dimy, dimz, dimt, debg
  CHARACTER(LEN=500), INTENT(IN)                         :: outfile, file_gatt
  CHARACTER(LEN=50), DIMENSION(4), INTENT(IN)            :: dimsvarname
  LOGICAL, INTENT(IN)                                    :: xcar, ycar

! Local
  INTEGER                                                :: i, icord
  INTEGER                                                :: rcode, oid, attlen, jv
  INTEGER, DIMENSION(4)                                  :: dimsid
  INTEGER, DIMENSION(4,2)                                :: foundCnames 
  INTEGER, DIMENSION(4,6)                                :: dimcoords
  CHARACTER(LEN=50)                                      :: section, att_text, attnamei,        &
    coordname, coordstd, coordunits, coordscoord, coordaxis, coordpositive
  CHARACTER(LEN=250)                                     :: coordlong
  CHARACTER(LEN=100)                                     :: attvalue, coordformula
  REAL, DIMENSION(:), ALLOCATABLE                        :: values_cord
  REAL, DIMENSION(:,:,:,:,:,:), ALLOCATABLE              :: coordinate
  CHARACTER(LEN=50), DIMENSION(:,:,:,:,:), ALLOCATABLE   :: charcoor
  LOGICAL                                                :: exists_out

!!!!!!!!!!!!!!!!! Variables
! outfile: output netCDF file
! dim[x/y/z/t]: spatio-temporal range of dimensions
! dimsvarname: vector with name of variables with coordinates values from 'file_gatt' 
! file_gatt: netCDF file from which all global attributes will be copyed and non-cartesian 
!    values of coordinates will be obtained
! [x/ycar]: whether x and y coordinates are cartesian (if not values from dimsvarname will be used)
! outid: netCDF output id file
! jv: variable id in outfile
! dimids: vector with dimension id in output file
! values_cord: cartesian values of a coordinate
! coordinate: 2D values of a coordinate
! foundCnames: position in 'file_gatt' of coordinates
! dimcoords: dimension of coordinates

  section="'create_output'"
  jv = 0

  CALL search_variables(debg, (/file_gatt/), 1, dimsvarname, 4, foundCnames, dimcoords)

  IF (debg >= 100) THEN
    PRINT *,"Creation of output file '"//TRIM(outfile)//"'..."
    PRINT *,'  with dimensNetCDF: Operation not allowed in define modeions: '
    PRINT *,'  dimx: ',dimx,' dimy:',dimy,' dimz:',dimz,' dimt:',dimt
    PRINT *,'  variables with dimension values: ',(TRIM(dimsvarname(i)), char(44), i=1,3),      &
      TRIM(dimsvarname(4))
  END IF
  INQUIRE( FILE=TRIM(outfile), EXIST=exists_out) 
  IF (exists_out) THEN
    PRINT *,"Acting on a pre-existing file! '"//TRIM(outfile)//"'"
    rcode = nf_open(TRIM(outfile), NF_WRITE, oid)
    CALL error_nc(section, rcode)
  ELSE
    PRINT *,"Opening the new file! '"//TRIM(outfile)//"'"  
    rcode = nf_create(TRIM(outfile), NF_CLOBBER, oid)
    CALL error_nc(section, rcode)
  END IF

! X dimension
!!
  IF (debg >= 100) PRINT *,'Creation of x dimension...'
  IF (xcar) THEN
! x coordinate is cartesian, thus it can be defined directly
    coordname = 'lon'
    IF (.NOT.exists_dim(oid, coordname)) THEN
      rcode = nf_def_dim(oid, coordname, dimx, dimsid(1))
      CALL error_nc(section, rcode)
    ELSE
      rcode = nf_inq_dimid(oid, TRIM(coordname), dimsid(1))
    ENDIF
    
  ELSE
! x coordinate is not cartesian, longitudes will be given by a 2D matrix driven by a base 
!   coordinate labelled 'xc', as it is stated by CF-1.4 conventions 
!      see http://cf-pcmdi.llnl.gov/documents/cf-conventions/1.4/ch05s02.html
    coordname = 'xc'

    IF (.NOT.exists_dim(oid, coordname)) THEN
      IF (debg >= 100) PRINT *,'X dimension is not cartesian...'
      jv = jv + 1

      coordaxis='X'
      coordlong='X-coordinate in Cartesian system'
      coordunits='m'

      IF (ALLOCATED(values_cord)) DEALLOCATE(values_cord)
      ALLOCATE(values_cord(dimx))
      DO icord=1, dimx
        values_cord(icord)=icord*1.
      END DO
      IF (debg >= 100) PRINT *,"values 'xc':", values_cord

      CALL def_dim(debg, oid, jv, coordname, coordaxis, coordlong, coordunits, dimx, dimsid(1),  &
        values_cord)
    
    ELSE
      rcode = nf_inq_dimid(oid, coordname, dimsid(1))
    END IF
  END IF

! Y dimension
!!
  IF (debg >= 100) PRINT *,'Creation of y dimension...'
  IF (ycar) THEN
! y coordinate is cartesian, thus it can be defined directly
    coordname = dimsvarname(2)
    IF (.NOT.exists_dim(oid, coordname)) THEN
      rcode = nf_def_dim(oid, coordname, dimy, dimsid(2))
      CALL error_nc(section, rcode)
    ELSE
      rcode = nf_inq_dimid(oid, coordname, dimsid(2))
    END IF

  ELSE
! y coordinate is not cartesian, latitudes will be given by a 2D matrix driven by a base 
!   coordinate labelled 'yc', as it is stated by CF-1.4 conventions 
!      see http://cf-pcmdi.llnl.gov/documents/cf-conventions/1.4/ch05s02.html
  coordname = 'yc'
    IF (.NOT.exists_dim(oid, coordname)) THEN
      IF (debg >= 100) PRINT *,'Y dimension is not cartesian...'
      jv = jv + 1

      coordaxis='Y'
      coordlong='Y-coordinate in Cartesian system'
      coordunits='m'

      IF (ALLOCATED(values_cord)) DEALLOCATE(values_cord)
      ALLOCATE(values_cord(dimy))
      DO icord=1, dimy
        values_cord(icord)=icord*1.
      END DO

      IF (debg >= 100) PRINT *,"values 'yc':", values_cord
      CALL def_dim(debg, oid, jv, coordname, coordaxis, coordlong, coordunits, dimy, dimsid(2),  &
        values_cord)

    ELSE
      rcode = nf_inq_dimid(oid, coordname, dimsid(2))
    END IF
  END IF
  
! Z-dimension
!!
  coordname = 'lev'
  IF (.NOT.exists_dim(oid,coordname)) THEN
    IF (debg >= 100) PRINT *,'Creation of z dimension...'

    jv = jv + 1

    coordstd='atmosphere_sigma_coordinate'
    coordlong='atmosphere level in sigma coordinate'
    coordunits='NO'
    coordpositive='down'
    coordformula='sigma: lev ps: PSFC ptop: P_TOP'

    IF (ALLOCATED(coordinate)) DEALLOCATE(coordinate)
    ALLOCATE(coordinate(dimcoords(3,1), dimcoords(3,2), dimcoords(3,3), dimcoords(3,4),          &
      dimcoords(3,5), dimcoords(3,6))) 
    CALL fill_inputs_real(debg, (/file_gatt/), 1, foundCnames(3,:), dimcoords(3,:), coordinate)

    IF (debg >= 100) PRINT *,"values 'zc':", coordinate(:,1,1,1,1,1)
    CALL def_dim_ver(debg, oid, jv, coordname, coordlong, coordunits, coordpositive, coordstd,   &
      coordformula, dimz, dimsid(3), coordinate(:,1,1,1,1,1))

  ELSE
    rcode = nf_inq_dimid(oid, coordname, dimsid(3))
  END IF

! T-dimension
!!
  coordname = 'time'
  IF (.NOT.exists_dim(oid,coordname)) THEN
    IF (debg >= 100) PRINT *,'Creation of t dimension...'

    jv = jv + 1

!    coordstd='atmosphere_sigma_coordinate'
    coordlong='time'
    coordunits='seconds since 1950-1-1 0:0:0'

    IF (ALLOCATED(coordinate)) DEALLOCATE(coordinate)
    ALLOCATE(coordinate(dimcoords(4,1), dimcoords(4,2), dimcoords(4,3), dimcoords(4,4),          &
      dimcoords(4,5), dimcoords(4,6))) 
    IF (ALLOCATED(charcoor)) DEALLOCATE(charcoor)
    ALLOCATE(charcoor(dimcoords(4,2), dimcoords(4,3), dimcoords(4,4), dimcoords(4,5),            &
      dimcoords(4,6)))

    CALL fill_inputs_50char(debg, (/file_gatt/), 1, foundCnames(4,:), dimcoords(4,:), charcoor)
    DO icord=1, dimt
      coordinate(icord,1,1,1,1,1)=icord*1.
    END DO
    IF (debg >= 100) PRINT *,"values 'tc':", coordinate(:,1,1,1,1,1)
    CALL def_dim_time(debg, oid, jv, coordname, coordlong, coordunits, dimt, dimsid(4),          &
      coordinate(:,1,1,1,1,1))

  ELSE
    rcode = nf_inq_dimid(oid, coordname, dimsid(4))
  END IF

!  rcode = nf_redef(oid)
!  CALL error_nc(section, rcode)

1  coordname = TRIM(dimsvarname(4))
!  IF (.NOT.exists_dim(oid,dimsvarname(4))) THEN
!    IF (debg >= 100) PRINT *,'Creation of t dimension...'
!    rcode = nf_def_dim(oid, coordname, dimt, dimsid(4))
!    CALL error_nc(section, rcode)
!  ELSE
!    rcode = nf_inq_dimid(oid, coordname, dimsid(4))
!  END IF

  IF (ALLOCATED(values_cord)) DEALLOCATE(values_cord)
!  rcode = nf_enddef(oid)
!  CALL error_nc(section, rcode)

  IF (debg >= 100) THEN
    PRINT *,'Output file dimensions id:'
    PRINT *,'dx:',dimsid(1),' dy:',dimsid(2),' dz:',dimsid(3),' dt:',dimsid(4)
  END IF

! Defining and writting real values of 2D longitudes & latitudes
  coordname='lon'
  IF ((.NOT.xcar) .AND. (.NOT.exists_var(oid, coordname))) THEN
    IF (debg >= 100) PRINT *,'Including real values of 2D longitudes...'
    jv = jv +1

    IF (ALLOCATED(coordinate)) DEALLOCATE(coordinate)
    ALLOCATE(coordinate(dimx,dimy,dimz,dimt,1,1))
    CALL fill_inputs_real(debg, (/file_gatt/), 1, foundCnames(1,:), dimcoords(1,:), coordinate)

    coordlong='longitude'
    coordstd='longitude'
    coordunits='degrees'
    coordscoord='lon lat'

    CALL def_nc_var(oid, jv, coordname, 5, 2, (/dimsid(1), dimsid(2), 1, 1, 1, 1/), "XY ",       &
      coordlong, coordstd, coordunits, "-", coordscoord, debg)
    rcode = nf_put_vara_real(oid, jv, (/1, 1/), (/dimx, dimy/), coordinate(:,:,1,1,1,1))
    CALL error_nc(section, rcode)

  END IF

  coordname='lat'
  PRINT *,' exists lat:',exists_var(oid, coordname)
  IF ((.NOT.ycar) .AND. (.NOT.exists_var(oid, coordname))) THEN 
    IF (debg >= 100) PRINT *,'Including real values of 2D latitudes...'
    jv = jv + 1
    IF (ALLOCATED(coordinate)) DEALLOCATE(coordinate)
    ALLOCATE(coordinate(dimx,dimy,dimz,dimt,1,1))

    CALL fill_inputs_real(debg, (/file_gatt/), 1, foundCnames(2,:), dimcoords(2,:), coordinate)
    coordlong='latitude'
    coordstd='latitude'
    coordunits='degrees'
    coordscoord='lon lat'

    CALL def_nc_var(oid, jv, coordname, 5, 2, (/dimsid(1), dimsid(2), 1, 1, 1, 1/), "XY ",       &
      coordlong, coordstd, coordunits, "-", coordscoord, debg)

    rcode = nf_put_vara_real(oid, jv, (/1, 1/), (/dimx, dimy/), coordinate(:,:,1,1,1,1))
    CALL error_nc(section, rcode)

  END IF
  IF (ALLOCATED(coordinate)) DEALLOCATE(coordinate)

! Head section of new netCDF
!!  
  att_text='Conventions'
  attvalue='CF-1.4'
  CALL def_nc_gatt_text(debg, oid, att_text, attvalue)
  CALL copy_nc_gatt(debg, oid, file_gatt)

!CALL put_nc_coords(dbg, oid, dimido, ifiles(g_att_file), dimsVname)
  rcode = nf_close(oid)

END SUBROUTINE create_output

SUBROUTINE def_dim(debg, ncid, ivar, dimname, dimax, dimlong, dimunit, dimrg, dimid, dimvalues)
! Subroutine to define a 1D dimension

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER, INTENT(IN)                                    :: debg, ncid, ivar, dimrg
  CHARACTER(LEN=50), INTENT(IN)                          :: dimname, dimax, dimlong, dimunit
  REAL, DIMENSION(dimrg), INTENT(IN)                     :: dimvalues
  INTEGER, INTENT(OUT)                                   :: dimid

! Local
  INTEGER                                                :: rcode, attlen
  CHARACTER(LEN=50)                                      :: section, atttext

!!!!!!!!!!!!! Variables
! ncid: netCDF id
! ivar: number of variable in netCDF
! Text values of dimension (according to CF-1.4 conventions):
!    dimname: dimension name
!    dimax: dimension axis
!    dimlong: dimension long_name
!    dimunits: dimension units
! dimrg: range of dimension
! dimid: id of dimension
! dimvalues: real vector with values of dimension

  section="'def_dim'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'

  rcode = nf_redef(ncid)
  CALL error_nc(section, rcode)

  rcode = nf_def_dim(ncid, TRIM(dimname), dimrg, dimid)
  CALL error_nc(section, rcode)

  rcode = nf_def_var(ncid, TRIM(dimname), NF_REAL, 1, dimid, ivar)
  CALL error_nc(section, rcode)

  atttext = TRIM(dimax) 
  attlen = len_trim(atttext)
  rcode = nf_put_att_text(ncid, ivar, "axis", attlen, atttext(1:attlen) )
  CALL error_nc(section, rcode)
  atttext = ' '

  atttext = TRIM(dimlong)
  attlen = len_trim(atttext)
  rcode = nf_put_att_text(ncid, ivar, "long_name", attlen, atttext(1:attlen) )
  CALL error_nc(section, rcode)
  atttext = ' '

  atttext = TRIM(dimunit)
  attlen = len_trim(atttext)
  rcode = nf_put_att_text(ncid, ivar, "units", attlen, atttext(1:attlen) )
  CALL error_nc(section, rcode)

  rcode = nf_enddef(ncid)
  CALL error_nc(section, rcode)

  rcode = nf_put_vara_real (ncid, ivar, (/1/), (/dimrg/), dimvalues)
  CALL error_nc(section, rcode)

  RETURN
END SUBROUTINE def_dim

SUBROUTINE def_dim_time(debg, ncid, ivar, dimname, dimlong, dimunit, dimrg, dimid, dimvalues)
! Subroutine to define a vertical coordinate

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER, INTENT(IN)                                    :: debg, ncid, ivar, dimrg
  CHARACTER(LEN=50), INTENT(IN)                          :: dimname, dimlong, dimunit
  REAL, DIMENSION(dimrg), INTENT(IN)                     :: dimvalues
  INTEGER, INTENT(OUT)                                   :: dimid

! Local
  INTEGER                                                :: rcode, attlen, lunits
  CHARACTER(LEN=50)                                      :: section, atttext

!!!!!!!!!!!!! Variables
! ncid: netCDF id
! ivar: number of variable in netCDF
! Text values of dimension (according to CF-1.4 conventions):
!    dimname: dimension name
!    dimlong: dimension long_name
!    dimunits: dimension units
! dimrg: range of dimension
! dimid: id of dimension
! dimvalues: real vector with values of dimension

  section="'def_dim_time'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'

  rcode = nf_redef(ncid)
  CALL error_nc(section, rcode)

  rcode = nf_def_dim(ncid, TRIM(dimname), dimrg, dimid)
  CALL error_nc(section, rcode)

  rcode = nf_def_var(ncid, TRIM(dimname), NF_REAL, 1, dimid, ivar)
  CALL error_nc(section, rcode)

  atttext = TRIM(dimlong)
  attlen = len_trim(atttext)
  rcode = nf_put_att_text(ncid, ivar, "long_name", attlen, atttext(1:attlen) )
  CALL error_nc(section, rcode)
  atttext = ' '

  atttext = TRIM(dimunit)
  attlen = len_trim(atttext)
  rcode = nf_put_att_text(ncid, ivar, "units", attlen, atttext(1:attlen))
  CALL error_nc(section, rcode)
  
  rcode = nf_enddef(ncid)
  CALL error_nc(section, rcode)

  rcode = nf_put_vara_real (ncid, ivar, (/1/), (/dimrg/), dimvalues)
  CALL error_nc(section, rcode)

  RETURN
  
END SUBROUTINE def_dim_time

SUBROUTINE def_dim_ver(debg, ncid, ivar, dimname, dimlong, dimunit, dimpos, dimstd, dimform,     &
  dimrg, dimid, dimvalues)
! Subroutine to define a vertical coordinate

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER, INTENT(IN)                                    :: debg, ncid, ivar, dimrg
  CHARACTER(LEN=50), INTENT(IN)                          :: dimname, dimlong, dimunit, dimpos,  &
    dimstd
  CHARACTER(LEN=100), INTENT(IN)                         :: dimform
  REAL, DIMENSION(dimrg), INTENT(IN)                     :: dimvalues
  INTEGER, INTENT(OUT)                                   :: dimid

! Local
  INTEGER                                                :: rcode, attlen, lunits
  CHARACTER(LEN=50)                                      :: section, atttext

!!!!!!!!!!!!! Variables
! ncid: netCDF id
! ivar: number of variable in netCDF
! Text values of dimension (according to CF-1.4 conventions):
!    dimname: dimension name
!    dimlong: dimension long_name
!    dimunits: dimension units
!    dimpos: dimension sign on level increase
!    dimform: dimension formula (whether is not a udunits variable)
! dimrg: range of dimension
! dimid: id of dimension
! dimvalues: real vector with values of dimension

  section="'def_dim_ver'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'

  rcode = nf_redef(ncid)
  CALL error_nc(section, rcode)

  rcode = nf_def_dim(ncid, TRIM(dimname), dimrg, dimid)
  CALL error_nc(section, rcode)

  rcode = nf_def_var(ncid, TRIM(dimname), NF_REAL, 1, dimid, ivar)
  CALL error_nc(section, rcode)

  atttext = TRIM(dimlong)
  attlen = len_trim(atttext)
  rcode = nf_put_att_text(ncid, ivar, "long_name", attlen, atttext(1:attlen) )
  CALL error_nc(section, rcode)
  atttext = ' '

  atttext = TRIM(dimpos)
  attlen = len_trim(atttext)
  rcode = nf_put_att_text(ncid, ivar, "positive", attlen, atttext(1:attlen) )
  CALL error_nc(section, rcode)
  atttext = ' '

  atttext = TRIM(dimstd)
  attlen = len_trim(atttext)
  rcode = nf_put_att_text(ncid, ivar, "standard_name", attlen, atttext(1:attlen) )
  CALL error_nc(section, rcode)
  atttext = ' '

  IF (TRIM(dimunit) == "NO") THEN

    atttext = ''
    attlen = len_trim(atttext)
    rcode = nf_put_att_text(ncid, ivar, "units", attlen, atttext(1:attlen) )
    CALL error_nc(section, rcode)
    atttext = ' '

    atttext = TRIM(dimform)
    attlen = len_trim(atttext)
    rcode = nf_put_att_text(ncid, ivar, "formula_terms", attlen, atttext(1:attlen) )
    CALL error_nc(section, rcode)
    atttext = ' '

  ELSE
    atttext = TRIM(dimunit)
    attlen = len_trim(atttext)
    rcode = nf_put_att_text(ncid, ivar, "units", attlen, atttext(1:attlen) )
    CALL error_nc(section, rcode)
  
  END IF
  
  rcode = nf_enddef(ncid)
  CALL error_nc(section, rcode)

  rcode = nf_put_vara_real (ncid, ivar, (/1/), (/dimrg/), dimvalues)
  CALL error_nc(section, rcode)

  RETURN
  
END SUBROUTINE def_dim_ver

SUBROUTINE def_nc_gatt_text (debg, mcid, atdesc, atvalue)
! Subroutine to define a global text attribute inside a netCDF file

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

  rcode = nf_redef(mcid)
  CALL error_nc(section, rcode)
 
  IF (debg >= 100) PRINT *,"Adding global attribute '"//TRIM(atdesc)//"' in netCDF file"
  att_text = atvalue
  ilen = len_trim(att_text)
  rcode = nf_put_att_text(mcid, NF_GLOBAL, TRIM(atdesc), ilen, att_text(1:ilen) )
  CALL error_nc(section, rcode)

  att_text = ' '

  rcode = nf_enddef(mcid)
  CALL error_nc(section, rcode)

END SUBROUTINE def_nc_gatt_text

SUBROUTINE def_nc_var (mcid, ivar, cval, itype, idm, jshape, order, desc, stddesc, units, stag, &
  coord, debg )
! Subroutine to define a variable inside a netCDF file

  USE module_constants

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER, INTENT(IN)                                    :: mcid, ivar, itype, idm
  CHARACTER(LEN=50), INTENT(IN)                          :: cval
  INTEGER, DIMENSION(6), INTENT(IN)                      :: jshape
  CHARACTER(LEN=3), INTENT(IN)                           :: order
  CHARACTER(LEN=250), INTENT(IN)                         :: desc
  CHARACTER(LEN=50), INTENT(IN)                          :: units, stddesc
  CHARACTER(LEN=1), INTENT(IN)                           :: stag
  CHARACTER(LEN=50), INTENT(IN)                          :: coord
  INTEGER, INTENT(IN)                                    :: debg

! Local
  INTEGER                                                :: rcode, ilen
  CHARACTER (LEN=60)                                     :: att_text
  CHARACTER (LEN=50)                                     :: section

!!!!!!!!!! Variables
! mcid: netCDF file id
! ivar: variable id
! cval: variable name
! itype: type of variable (1: ; 2: integer ; 3: ; 4: ; 5: real)
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

  rcode = nf_redef(mcid)
  IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)

  IF ( itype == 5 ) THEN
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

SUBROUTINE error_nc(sec, rc)
! Subroutine to print error nc messages

  USE module_constants
  
  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'
  
  INTEGER, INTENT(IN)                                     :: rc
  CHARACTER(LEN=50), INTENT(IN)                           :: sec

! Local
  
  IF (rc /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(sec)//" "//nf_strerror(rc)

END SUBROUTINE error_nc

LOGICAL FUNCTION exists_dim(ncid, dimname)
! Function to determine if a dimension exists

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'
  
  INTEGER, INTENT(IN)                                     :: ncid
  CHARACTER(LEN=50), INTENT(IN)                           :: dimname
  
! Local
  INTEGER                                                 :: rcode, dimid
  CHARACTER(LEN=50)                                       :: section

!!!!!!! Variables
! ncid: netCDF id
! dimname: name of dimension

  section="'exists_dim'"
  exists_dim = .FALSE.

  rcode = nf_inq_dimid (ncid, dimname, dimid)
  IF (dimid > 0) exists_dim = .TRUE.

END FUNCTION exists_dim

LOGICAL FUNCTION exists_var(ncid, varname)
! Function to determine if a variable exists

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'
  
  INTEGER, INTENT(IN)                                     :: ncid
  CHARACTER(LEN=50), INTENT(IN)                           :: varname
  
! Local
  INTEGER                                                 :: rcode, varid
  CHARACTER(LEN=50)                                       :: section

!!!!!!! Variables
! ncid: netCDF id
! varname: name of variable

  section="'exists_var'"
  exists_var = .FALSE.

  rcode = nf_inq_varid (ncid, varname, varid)
  IF (varid > 0) exists_var = .TRUE.

END FUNCTION exists_var

SUBROUTINE fill_inputs_50char(debg, ncs, Nncs, fvars, dimMin, matou)
! Subroutine to fill a kind-shape of input real fields

  USE module_constants
  USE module_gen_tools, ONLY: halfdim

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'

  INTEGER, INTENT(IN)                                     :: debg, Nncs 
  INTEGER, DIMENSION(2), INTENT(IN)                       :: fvars
  INTEGER, DIMENSION(6), INTENT(IN)                       :: dimMin
  CHARACTER(LEN=50), DIMENSION(dimMin(2), dimMin(3),                                             &
    dimMin(4), dimMin(5), dimMin(6)), INTENT(OUT)         :: matou
  CHARACTER(LEN=500), DIMENSION(Nncs), INTENT(IN)         :: ncs

! Local
  INTEGER                                                 :: i, rcode, ncid
  CHARACTER(LEN=50)                                       :: section
  CHARACTER(LEN=50), DIMENSION(dimMin(1), dimMin(2),                                             &
    dimMin(3), dimMin(4), dimMin(5), dimMin(6))           :: matin

!!!!!!!!!! Variables
! Nncs: number of input files
! ncs: netCDF input files
! fvars: file (col1) and position in it (col2) of variable
! dimMinn: dimensions of variable
! matin: matrix with values of variable

!!!!!!!!!! Function
! halfdim: Function to give the half value of a dimension

  section="'fill_inputs'"
  IF (debg >= 100) PRINT *,'Section '//TRIM(section)//'... .. .'
  IF (debg >= 75) THEN
    PRINT *,'Filling matrices of dimension: ',UBOUND(matin)
    PRINT *,"Variable in file: '"//TRIM(ncs(fvars(1)))//"' with id:",fvars(2)
  END IF
  rcode = nf_open(TRIM(ncs(fvars(1))), 0, ncid)
  call error_nc(section, rcode)

  rcode = nf_get_var_text ( ncid, fvars(2), matin )
  call error_nc(section, rcode)

  rcode = nf_close(ncid)
  call error_nc(section, rcode)

  matou=''
  DO i=1,dimMin(1)
    matou(:,:,:,:,:)=matou(:,:,:,:,:)//matin(i,:,:,:,:,:)
  END DO

  IF (debg >= 100) PRINT *,'Memory loaded input variable. Value (dimN/2)=',                     &
    TRIM(matou(halfdim(dimMin(2)), halfdim(dimMin(3)), halfdim(dimMin(4)), halfdim(dimMin(5)),  &
      halfdim(dimMin(6))))
  RETURN
  
END SUBROUTINE fill_inputs_50char

SUBROUTINE fill_inputs_real(debg, ncs, Nncs, fvars, dimMin, matin)
! Subroutine to fill a kind-shape of input real fields

  USE module_constants
  USE module_gen_tools, ONLY: halfdim

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
  
END SUBROUTINE fill_inputs_real

SUBROUTINE nc_gatts(fileinf, debg) 
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
END SUBROUTINE nc_gatts

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
! itype: type of variable (1: ; 2: text ; 3: ; 4: integer ; 5: real)
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
  USE module_gen_tools, ONLY: diag_fatal

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

END MODULE module_nc_tools
