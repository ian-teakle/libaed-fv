
INCLUDE 'tuflowfv_wq_api.f90'
    
MODULE tuflowfv_external_wq_aed

! MODULE USE STATEMENTS
USE tuflowfv_wq_api !  Version 1.0
USE fv_aed
USE fv_zones

! MODULE DECLARATIONS
IMPLICIT NONE

! MODULE ACCESS STATEMENTS
PRIVATE
PUBLIC :: fvwq_ctrl_external, fvwq_external

! MODULE TYPE DEFINITIONS
TYPE,EXTENDS(fvwq_ctrl_class) :: fvwq_ctrl_external
    CHARACTER(LEN=30),ALLOCATABLE,DIMENSION(:) :: var_names     ! WQ PELAGIC CONSTITUENT NAMES
    CHARACTER(LEN=30),ALLOCATABLE,DIMENSION(:) :: ben_names     ! WQ BENTHIC CONSTITUENT NAMES
    CHARACTER(LEN=30),ALLOCATABLE,DIMENSION(:) :: diag_names    ! WQ DIAGNOSTIC VARIABLE NAMES
CONTAINS
    PROCEDURE,NOPASS :: initialise => fvwq_ctrl_initialise_external
    PROCEDURE :: check => fvwq_ctrl_check_external
    PROCEDURE :: construct => fvwq_ctrl_construct_external
    PROCEDURE :: destruct => fvwq_ctrl_destruct_external
END TYPE

TYPE,EXTENDS(fvwq_class) :: fvwq_external
    LOGICAL :: init                                             ! INITIALSED STATUS
CONTAINS
    PROCEDURE :: construct => fvwq_construct_external
    PROCEDURE :: destruct => fvwq_destruct_external
    PROCEDURE :: initialise => fvwq_initialise_external
    PROCEDURE :: update => fvwq_update_external
END TYPE

! MODULE PARAMETERS
INTEGER,PARAMETER :: WQFileNum = 20
!DEC$ IF DEFINED(linux) ! Linux
  CHARACTER(LEN=1),PARAMETER :: slash = '/'
!DEC$ ELSE ! Windows
  CHARACTER(LEN=1),PARAMETER :: slash = '\'
!DEC$ END IF

! MODULE OBJECTS
TYPE(fvwq_ctrl_external),TARGET :: aed_ctrl

! MODULE VARIABLES

! MODULE PROCEDURES
CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE fvwq_ctrl_initialise_external(ctrl)
!DEC$ ATTRIBUTES DLLEXPORT :: fvwq_ctrl_initialise_external
!DEC$ ATTRIBUTES ALIAS : 'FVWQ_CTRL_INITIALISE_EXTERNAL' :: fvwq_ctrl_initialise_external
CLASS(fvwq_ctrl_class),POINTER,INTENT(INOUT) :: ctrl

! Associate parent contol object with this module (child) ctrl object
ctrl => aed_ctrl
! fvaed library does not require further initialisation

END SUBROUTINE fvwq_ctrl_initialise_external
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE fvwq_ctrl_check_external(ctrl,wqctrlfil,errstat,errmsg)
!DEC$ ATTRIBUTES DLLEXPORT :: fvwq_ctrl_check_external
!DEC$ ATTRIBUTES ALIAS : 'FVWQ_CTRL_CHECK_EXTERNAL' :: fvwq_ctrl_check_external
CLASS(fvwq_ctrl_external),INTENT(INOUT) :: ctrl
CHARACTER(LEN=*),INTENT(IN) :: wqctrlfil
INTEGER,INTENT(OUT) :: errstat
CHARACTER(LEN=*),INTENT(OUT) :: errmsg
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'fvaed_ctrl_check'
CHARACTER(LEN=200) :: wqDir
INTEGER :: i
LOGICAL :: openstat

errstat = 0; errmsg = ''
INQUIRE(UNIT=logunit,OPENED=openstat)

! BEGIN
! GET AED CONFIGURATION
WRITE(*,'(a)', advance="no") 'Configuring "AED" external module... '
IF (openstat) WRITE(logunit,'(a)', advance="no") 'Configuring "AED" external module... '

aed_ctrl%wqmod = 'AED'
i = LEN_TRIM(wqctrlfil)
IF (i>0) THEN
    IF (wqctrlfil(i:i)==slash) THEN
        wqDir = TRIM(wqctrlfil)
    ELSE
        wqDir = TRIM(wqctrlfil)//slash
    END IF
ELSE
    wqDir = ''
END IF

CALL init_aed_models(WQFileNum,wqDir,aed_ctrl%Nwat,aed_ctrl%Nben,aed_ctrl%Ndiag,aed_ctrl%var_names,aed_ctrl%ben_names,aed_ctrl%diag_names)

WRITE(*,'(a)') 'Successful.'
IF (openstat) WRITE(logunit,'(a)') 'Successful.'

END SUBROUTINE fvwq_ctrl_check_external
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE fvwq_ctrl_construct_external(ctrl,wqctrlfil,runlabel,tform,tzero,ptm_grp_names, &
                                errstat,errmsg)
!DEC$ ATTRIBUTES DLLEXPORT :: fvwq_ctrl_construct_external
!DEC$ ATTRIBUTES ALIAS : 'FVWQ_CTRL_CONSTRUCT_EXTERNAL' :: fvwq_ctrl_construct_external
CLASS(fvwq_ctrl_external),INTENT(INOUT) :: ctrl
CHARACTER(LEN=*),INTENT(IN) :: wqctrlfil
CHARACTER(LEN=*),INTENT(IN) :: runlabel
INTEGER,INTENT(IN) :: tform
REAL(KIND=wqdk),INTENT(IN) :: tzero
CHARACTER(LEN=*),ALLOCATABLE,DIMENSION(:),INTENT(IN) :: ptm_grp_names
INTEGER,INTENT(OUT) :: errstat
CHARACTER(LEN=*),INTENT(OUT) :: errmsg

! AED nmls have already been processed during fvwq_ctrl_check
! No further construction of aed_ctrl required
errstat = 0; errmsg = ''
ctrl%isActivated = .TRUE.

END SUBROUTINE fvwq_ctrl_construct_external
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE fvwq_ctrl_destruct_external(ctrl,errstat,errmsg)
!DEC$ ATTRIBUTES DLLEXPORT :: fvwq_ctrl_destruct_external
!DEC$ ATTRIBUTES ALIAS : 'FVWQ_CTRL_DESTRUCT_EXTERNAL' :: fvwq_ctrl_destruct_external
CLASS(fvwq_ctrl_external),INTENT(INOUT) :: ctrl
INTEGER,INTENT(OUT) :: errstat
CHARACTER(LEN=*),INTENT(OUT) :: errmsg
!
INTEGER :: tmpstat

errstat = 0; errmsg = ''
DEALLOCATE(ctrl%var_names,ctrl%ben_names,ctrl%diag_names, STAT=tmpstat)
ctrl%isActivated = .FALSE.

END SUBROUTINE fvwq_ctrl_destruct_external
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE fvwq_construct_external(wq,errstat,errmsg)
!DEC$ ATTRIBUTES DLLEXPORT :: fvwq_construct_external
!DEC$ ATTRIBUTES ALIAS : 'FVWQ_CONSTRUCT_EXTERNAL' :: fvwq_construct_external
CLASS(fvwq_external),INTENT(INOUT) :: wq
INTEGER,INTENT(OUT) :: errstat
CHARACTER(LEN=*),INTENT(OUT) :: errmsg
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'fvwq_construct'
integer i,j
LOGICAL :: openstat

errstat = 0; errmsg = ''
INQUIRE(UNIT=logunit,OPENED=openstat)

! BEGIN
! INITIALISE AED LINKAGE
WRITE(*,'(a)') 'Initialising "AED" external module:'
IF (openstat) WRITE(logunit,'(a)') 'Initialising "AED" external module:'

! THIS API ROUTINE ASSUMES THAT WQ OBJECT MEMORY HAS BEEN ALLOCATED BY MAIN EXE - PERFORM SOME PARTIAL CHECKS TO CONFIRM
if ( .not. associated(wq%cc) .or. .not. associated(wq%diag) ) then
    write(*, '(a)') 'Water quality variables not associated'
    IF (openstat) write(logunit, '(a)') 'Water quality variables not associated'
    return
endif

wq%names(:) = aed_ctrl%var_names(:)
wq%ben_names(:) = aed_ctrl%ben_names(:)
wq%diag_names(:) = aed_ctrl%diag_names(:)

! ALLOCATE LOCAL MEMORY BLOCK FOR AED WQ VARIABLES
CALL init_var_aed_models(wq%nc3,wq%cc,wq%diag,wq%Nwat,wq%Nben, &
                                wq%surf_map,wq%benth_map)
                                
CALL set_env_aed_models( wq%dt_update,       &
                        ! 3D env variables
                        wq%temp,            &
                        wq%sal,             &
                        wq%density,         &
                        wq%thick,           &
                        wq%tss,             &
                        wq%par,             &
                        wq%vvel,            &
                        wq%hvel,            &
                        ! 3D feedback arrays
                        wq%bioshade,        &
                        ! 2D env variables
                        wq%area,            &
                        wq%I_0,             &
                        wq%longwave,        &
                        wq%wind,            &
                        wq%precip,          &
                        wq%humidity,        &
                        wq%air_temp,        &
                        wq%ustar_bed,       &
                        wq%ustar_surf,      &
                        wq%wv_uorb,         &
                        wq%wv_t,            &
                        wq%depth,           &
                        wq%bathy,           &
                        wq%mat_id,          &
                        wq%active,          &
                        ! 2D feedback arrays
                        wq%biodrag,         &
                        wq%solarshade,      &
                        wq%rainloss)

WRITE(*,'(a)') 'Successful.'
IF (openstat) WRITE(logunit,'(a)') 'Successful.'
wq%init = .TRUE.

END SUBROUTINE fvwq_construct_external
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE fvwq_destruct_external(wq,errstat,errmsg)
!DEC$ ATTRIBUTES DLLEXPORT :: fvwq_destruct_external
!DEC$ ATTRIBUTES ALIAS : 'FVWQ_DESTRUCT_EXTERNAL' :: fvwq_destruct_external
CLASS(fvwq_external),INTENT(INOUT) :: wq
INTEGER,INTENT(OUT) :: errstat
CHARACTER(LEN=*),INTENT(OUT) :: errmsg
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'fvwq_destruct'
LOGICAL :: openstat

errstat = 0; errmsg = ''
INQUIRE(UNIT=logunit,OPENED=openstat)

! BEGIN
IF (.NOT. wq%init) RETURN
WRITE(*,'(a)') 'Cleaning "AED" external module:'
IF (openstat) WRITE(logunit,'(a)') 'Cleaning "AED" external module:'

CALL clean_aed_models()

WRITE(*,'(a)') 'Successful.'
IF (openstat) WRITE(logunit,'(a)') 'Successful.'
wq%init = .FALSE.

END SUBROUTINE fvwq_destruct_external
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE fvwq_initialise_external(wq,errstat,errmsg)
!DEC$ ATTRIBUTES DLLEXPORT :: fvwq_initialise_external
!DEC$ ATTRIBUTES ALIAS : 'FVWQ_INITIALISE_EXTERNAL' :: fvwq_initialise_external
CLASS(fvwq_external),INTENT(INOUT) :: wq
INTEGER,INTENT(OUT) :: errstat
CHARACTER(LEN=*),INTENT(OUT) :: errmsg

! fvaed library does not require initialisation of wq object
errstat = 0; errmsg = ''

END SUBROUTINE fvwq_initialise_external
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE fvwq_update_external(wq,errstat,errmsg)
!DEC$ ATTRIBUTES DLLEXPORT :: fvwq_update_external
!DEC$ ATTRIBUTES ALIAS : 'FVWQ_UPDATE_EXTERNAL' :: fvwq_update_external
CLASS(fvwq_external),INTENT(INOUT) :: wq
INTEGER,INTENT(OUT) :: errstat
CHARACTER(LEN=*),INTENT(OUT) :: errmsg
! LOCAL VARIABLES
CHARACTER(LEN=30) :: sub = 'fvwq_update'
LOGICAL :: openstat
!
!BEGIN

errstat = 0; errmsg = ''
INQUIRE(UNIT=logunit,OPENED=openstat)

IF (.NOT. wq%init) THEN
    WRITE(*,'(a)') 'ERROR "AED" external module not initialised.'
    IF (openstat) WRITE(logunit,'(a)') 'ERROR "AED" external module not initialised.'
    WRITE(*,'(a)') 'Stopping in '//TRIM(sub)//'.'
    IF (openstat) WRITE(logunit,'(a)') 'Stopping in '//TRIM(sub)//'.'
    STOP
ENDIF

!IF (do_particle_bgc) CALL set_env_particles(wq%NG,wq%parts)

CALL do_aed_models(wq%nc3,wq%nc2)

END SUBROUTINE fvwq_update_external
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE tuflowfv_external_wq_aed
