module module_inline
!
!*** this module contains the subroutines for cdeps inline capability 
!
! revision history
!  22 Aug 2023: U. Turuncoglu  Initial development
!
  use ESMF
  use dshr_strdata_mod, only: shr_strdata_type
  use dshr_strdata_mod, only: shr_strdata_init_from_inline

  implicit none

  private
  public stream_init

  type(shr_strdata_type) :: sdat ! input data stream

  type config
     integer :: year_first
     integer :: year_last
     integer :: year_align
     integer :: offset
     real(kind=8) :: dtlimit
     character(len=ESMF_MAXSTR) :: mesh_filename
     character(len=ESMF_MAXSTR), allocatable :: data_filename(:)
     character(len=ESMF_MAXSTR), allocatable :: fld_list(:)
     character(len=ESMF_MAXSTR), allocatable :: fld_list_model(:)
     character(len=ESMF_MAXSTR) :: mapalgo
     character(len=ESMF_MAXSTR) :: taxmode
     character(len=ESMF_MAXSTR) :: tintalgo
     character(len=ESMF_MAXSTR) :: name
  end type config
  type(config) :: stream
!
  contains

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------

    subroutine stream_init(comp, rc)
      ! input/output variables
      type(ESMF_GridComp), intent(in)  :: comp
      integer            , intent(out) :: rc

      integer           :: localPet
      type(ESMF_Grid)   :: grid
      type(ESMF_Mesh)   :: mesh
      type(ESMF_Clock)  :: clock

      ! query compontn to retrieve required information
      call ESMF_GridCompGet(comp, grid=grid, clock=clock, localPet=localPet, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      !masterproc = .false.
      !if (localPet == 0) masterproc = .true.

      ! read configuration
      !call read_config(rc)
      !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      ! create mesh from grid
      mesh = ESMF_MeshCreate(grid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      ! initialize the cdeps data type
      call shr_strdata_init_from_inline(sdat, &
                                        my_task = localPet, &
                                        logunit = 6, &
                                        compname = 'ATM', &
                                        model_clock = clock, &
                                        model_mesh = mesh, &
                                        stream_meshfile = 'INPUT_DATA/ESMFmesh.nc', &
                                        stream_filenames = (/ 'INPUT_DATA/tsfc_fv3grid_202318612_sub.nc' /), &
                                        stream_yearFirst = 2023, &
                                        stream_yearLast     = 2023,               &
                                        stream_yearAlign    = 2023,              &
                                        stream_fldlistFile  = (/ 'twsfc' /),                 &
                                        stream_fldListModel = (/ 'twsfc' /),                 &
                                        stream_lev_dimname  = 'null',                              &
                                        stream_mapalgo      = 'bilinear',                          &
                                        stream_offset       = 0,                                   &
                                        stream_taxmode      = 'cycle',                             &
                                        stream_dtlimit      = 1.5d0,                           &
                                        stream_tintalgo     = 'linear',                            &
                                        stream_name         = 'fvcom great lakes',         &
                                        rc                  = rc) 

    end subroutine stream_init

!    subroutine read_config()
!      ! input/output variables
!      integer, intent(out) :: rc
!
!      ! local variables
!      type(ESMF_Config) :: cf
!      character(ESMF_MAXPATHLEN) :: fname
!
!      rc = ESMF_SUCCESS
!
!      ! create ESMF config object to read in namelist options
!      cf = ESMF_ConfigCreate(rc=rc)
!      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
!
!      fname = "atm_in"
!      call ESMF_ConfigLoadFile(cf, fname, rc=rc)
!      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return




!     integer :: year_first
!     integer :: year_last
!     integer :: year_align
!     integer :: offset
!     real(kind=8) :: dtlimit
!     character(len=ESMF_MAXSTR) :: mesh_filename
!     character(len=ESMF_MAXSTR), allocatable :: data_filename(:)
!     character(len=ESMF_MAXSTR), allocatable :: fld_list(:)
!     character(len=ESMF_MAXSTR), allocatable :: fld_list_model(:)
!     character(len=ESMF_MAXSTR) :: mapalgo
!     character(len=ESMF_MAXSTR) :: taxmode
!     character(len=ESMF_MAXSTR) :: tintalgo
!     character(len=ESMF_MAXSTR) :: name


!    end subroutine read_config

end module module_inline
