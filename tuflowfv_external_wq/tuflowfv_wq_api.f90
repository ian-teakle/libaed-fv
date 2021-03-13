! Copyright 2020 by BMT Commercial Australia Pty Ltd
!*******************************************************************************************
MODULE tuflowfv_wq_api
IMPLICIT NONE
! WQ API ACCESS
PRIVATE
PUBLIC :: fvwq_ctrl_class, fvwq_class, wqrk, wqdk, wq_ctrl, tuflowfv_wq_api_version, logunit

! WQ API PARAMETERS
INTEGER,PARAMETER :: logunit = 100
INTEGER,PARAMETER :: wqrk = 8
INTEGER,PARAMETER :: wqdk = 8

! WQ API VERSION
INTEGER,PARAMETER :: tuflowfv_wq_api_version = 1.0

! CHANGE LOG
! v1.0 01/03/2021
! First version of tuflowfv_wq_api.  Same basic functionality as superseded tuflowfv_external_wq.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! WQ CONTROL OBJECT ABSTRACT DEFINITION
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
TYPE,ABSTRACT :: fvwq_ctrl_class
    PRIVATE
    ! The following variables must be defined by the WQ model during fvwq_ctrl_check
    CHARACTER(LEN=30),PUBLIC :: wqmod = ''
    INTEGER,PUBLIC :: Nwat = 0
    INTEGER,PUBLIC :: Nben = 0
    INTEGER,PUBLIC :: Ndiag = 0
    LOGICAL,PUBLIC :: isActivated = .FALSE.
    ! The timestep may be defined by the WQ model
    ! If not specified will be defined by the driver model
    REAL(wqdk),PUBLIC :: dt_update = -999.
CONTAINS
    PROCEDURE(fvwq_ctrl_initialise),NOPASS,DEFERRED :: initialise
    PROCEDURE(fvwq_ctrl_check),DEFERRED :: check
    PROCEDURE(fvwq_ctrl_construct),DEFERRED :: construct
    PROCEDURE(fvwq_ctrl_destruct),DEFERRED :: destruct
END TYPE
ABSTRACT INTERFACE
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE fvwq_ctrl_initialise(ctrl)
    IMPORT fvwq_ctrl_class
    CLASS(fvwq_ctrl_class),POINTER,INTENT(INOUT) :: ctrl
    ! This routine should be used to initialise class extension ctrl object (this_ctrl), i.e.
    ! ctrl => this_ctrl
    END SUBROUTINE
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE fvwq_ctrl_check(ctrl,wqctrlfil,errstat,errmsg)
    IMPORT fvwq_ctrl_class
    CLASS(fvwq_ctrl_class),INTENT(INOUT) :: ctrl
    CHARACTER(LEN=*),INTENT(IN) :: wqctrlfil
    INTEGER,INTENT(OUT) :: errstat
    CHARACTER(LEN=*),INTENT(OUT) :: errmsg
    END SUBROUTINE
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE fvwq_ctrl_construct(ctrl,wqctrlfil,runlabel,tform,tzero,ptm_grp_names, &
                                    errstat,errmsg)
    IMPORT fvwq_ctrl_class, wqdk
    CLASS(fvwq_ctrl_class),INTENT(INOUT) :: ctrl
    CHARACTER(LEN=*),INTENT(IN) :: wqctrlfil
    CHARACTER(LEN=*),INTENT(IN) :: runlabel
    INTEGER,INTENT(IN) :: tform
    REAL(KIND=wqdk),INTENT(IN) :: tzero
    CHARACTER(LEN=*),ALLOCATABLE,DIMENSION(:),INTENT(IN) :: ptm_grp_names
    INTEGER,INTENT(OUT) :: errstat
    CHARACTER(LEN=*),INTENT(OUT) :: errmsg
    END SUBROUTINE
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE fvwq_ctrl_destruct(ctrl,errstat,errmsg)
    IMPORT fvwq_ctrl_class
    CLASS(fvwq_ctrl_class),INTENT(INOUT) :: ctrl
    INTEGER,INTENT(OUT) :: errstat
    CHARACTER(LEN=*),INTENT(OUT) :: errmsg
    END SUBROUTINE
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END INTERFACE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! WQ OBJECT ABSTRACT DEFINITION
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
TYPE,ABSTRACT :: fvwq_class
    PRIVATE
    CHARACTER(LEN=30),PUBLIC :: model                                  ! WQ MODEL DESCRIPTION
    ! Mesh size
    INTEGER,PUBLIC :: NC2                                              ! NUMBER OF 2D CELLS
    INTEGER,PUBLIC :: NC3                                              ! NUMBER OF 3D CELLS
    ! WQ timestep, next update time and update status
    REAL(wqdk),PUBLIC :: dt_update                                     ! UPDATE TIMESTEP
    REAL(wqdk),PUBLIC :: t_update                                      ! NEXT UPDATE TIME
    LOGICAL,PUBLIC :: updated                                          ! UPDATED STATUS
    ! WQ model number of constituents
    INTEGER,PUBLIC :: Nwat                                             ! NUMBER OF WQ CONSTITUENTS
    INTEGER,PUBLIC :: Nben                                             ! NUMBER OF BENTHIC WQ CONSTITUENTS
    INTEGER,PUBLIC :: Ndiag                                            ! NUMBER OF WQ DIAGNOSTIC VARIABLES
    CHARACTER(LEN=60),PUBLIC,ALLOCATABLE,DIMENSION(:) :: names         ! WQ PELAGIC CONSTITUENT NAMES
    CHARACTER(LEN=60),PUBLIC,ALLOCATABLE,DIMENSION(:) :: ben_names     ! WQ BENTHIC CONSTITUENT NAMES
    CHARACTER(LEN=60),PUBLIC,ALLOCATABLE,DIMENSION(:) :: diag_names    ! WQ DIAGNOSTIC VARIABLE NAMES
    ! Mesh indexing and property arrays
    INTEGER,PUBLIC,POINTER,DIMENSION(:) :: surf_map                    ! SURFACE CELL MAP (NC2)
    INTEGER,PUBLIC,POINTER,DIMENSION(:) :: benth_map                   ! BOTTOM/BENTHIC LAYER MAP (NC2)
    INTEGER,PUBLIC,POINTER,DIMENSION(:) :: NL                          ! NUMBER OF LAYERS (NC2)
    INTEGER,PUBLIC,POINTER,DIMENSION(:,:) :: mat_id                    ! MATERIAL ID (NMG,NC2)
    ! Geometry arrays
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:) :: area                     ! CELL AREA (NC2)
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:) :: bathy                    ! ELEVATION OF COLUMN BOTTOM (NC2)
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:) :: thick                    ! CELL THICKNESS (NC3)
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:) :: depth                    ! LOCAL MID-CELL DEPTH (NC3)
    ! Environment arrays
    LOGICAL,PUBLIC,POINTER,DIMENSION(:) :: active                      ! COLUMN ACTIVE STATUS (NC2)
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:) :: sal                      ! SALINITY POINTER (NC3)
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:) :: temp                     ! TEMPERATURE POINTER (NC3)
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:) :: tss                      ! TOTAL SUSPENDED SOLIDS POINTER (NC3)
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:) :: vvel                     ! VERTICAL VELOCITIES (NC3)
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:) :: hvel                     ! HORIZONTAL VELOCITIES (NC3)
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:) :: density                  ! ABSOLUTE DENSITY (NC3)
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:) :: ustar_bed                ! BED FRICTION VELOCITY (NC2)
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:) :: ustar_surf               ! SURFACE FRICTION VELOCITY (NC2)
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:) :: I_0                      ! NET SURFACE SHORTWAVE IRRADIANCE (NC2)
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:,:) :: par                    ! NET PHOTOSYNTHETICALLY ACTIVE RADIATION (NC3)
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:) :: longwave                 ! NET SURFACE LONGWAVE RADIATION (NC2)
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:) :: wind                     ! 10M WINDSPEED (NC2)
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:) :: precip                   ! RAIN (NC2)
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:) :: air_temp                 ! AIR TEMPERATURE (NC2)
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:) :: humidity                 ! HUMIDITY (NC2)
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:) :: wv_uorb                  ! WAVE ORBITAL VELOCITY (NC2)
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:) :: wv_t                     ! WAVE PERIOD (NC2)
    ! WQ constituent arrays
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:,:) :: cc                     ! WQ CONSTITUENT CONCENTRATIONS (NWQ,NC3)
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:,:) :: dcdt                   ! TEMPORAL DERIVATIVE OF WQ CONSTITUENTS (NWQ,NC3)
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:,:) :: diag                   ! DIAGNOSTIC WQ VARIABLES (NDIAG,NC3)
    ! Arrays that control feedbacks between the models
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:) :: bioshade                 ! BIOGEOCHEMICAL LIGHT EXTINCTION COEFFICIENT RETURNED FROM WQ (NC3)
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:) :: biodrag                  ! ADDITIONAL DRAG ON FLOW FROM BIOLOGY, RETURNED FROM WQ (NC3)
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:) :: solarshade               ! REDUCTION OF SOLAR RADIATION DUE TO SHADING RETURNED FROM WQ (NC2)
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:) :: rainloss                 ! LOSS OF RAINFALL INTO EXPOSED SEDIMENT RETURNED FROM WQ (NC2)
    ! Variables related to particle tracking
    INTEGER,PUBLIC :: NPart                                            ! NUMBER OF PARTICLES
    INTEGER,PUBLIC,POINTER,DIMENSION(:,:) :: part_istat                ! PARTICLE STATUS PROPERTIES (Nistat,Npart)
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:,:) :: part_tstat             ! PARTICLE AGE PROPERTIES (Ntstat,Npart)
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:,:) :: part_prop              ! PARTICLE ENVIRONMENTAL PROPERTIES (Nprop,Npart)
    REAL(wqrk),PUBLIC,POINTER,DIMENSION(:,:) :: part_mass              ! PARTICLE MASS (NWQ,Npart) i.e. ptm%part%U

CONTAINS
    PROCEDURE(fvwq_construct),DEFERRED :: construct
    PROCEDURE(fvwq_destruct),DEFERRED :: destruct
    PROCEDURE(fvwq_initialise),DEFERRED :: initialise
    PROCEDURE(fvwq_update),DEFERRED :: update
END TYPE
ABSTRACT INTERFACE
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE fvwq_construct(wq,errstat,errmsg)
    IMPORT fvwq_class
    CLASS(fvwq_class),INTENT(INOUT) :: wq
    INTEGER,INTENT(OUT) :: errstat
    CHARACTER(LEN=*),INTENT(OUT) :: errmsg
    END SUBROUTINE
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE fvwq_destruct(wq,errstat,errmsg)
    IMPORT fvwq_class
    CLASS(fvwq_class),INTENT(INOUT) :: wq
    INTEGER,INTENT(OUT) :: errstat
    CHARACTER(LEN=*),INTENT(OUT) :: errmsg
    END SUBROUTINE
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE fvwq_initialise(wq,errstat,errmsg)
    IMPORT fvwq_class
    CLASS(fvwq_class),INTENT(INOUT) :: wq
    INTEGER,INTENT(OUT) :: errstat
    CHARACTER(LEN=*),INTENT(OUT) :: errmsg
    END SUBROUTINE
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE fvwq_update(wq,errstat,errmsg)
    IMPORT fvwq_class
    CLASS(fvwq_class),INTENT(INOUT) :: wq
    INTEGER,INTENT(OUT) :: errstat
    CHARACTER(LEN=*),INTENT(OUT) :: errmsg
    END SUBROUTINE
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END INTERFACE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! WQ API OBJECTS
CLASS(fvwq_ctrl_class),POINTER :: wq_ctrl
!DEC$ ATTRIBUTES DLLEXPORT :: wq_ctrl
!DEC$ ATTRIBUTES ALIAS : 'WQ_CTRL' :: wq_ctrl
! This public module object is used to enable the driver to interact with the control object, i.e.
! TYPE,EXTENDS(fvwq_ctrl_class) :: fvwq_ctrl_extended
! END TYPE
! TYPE(fvwq_ctrl_extended),TARGET :: this_ctrl
! Use fvwq_ctrl_initialise_extended to associate wq_ctrl => thic_ctrl

END MODULE tuflowfv_wq_api
!*******************************************************************************************