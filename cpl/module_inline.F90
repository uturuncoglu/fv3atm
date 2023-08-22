module module_inline
!
!*** this module contains the subroutines for cdeps inline capability 
!
! revision history
!  22 Aug 2023: U. Turuncoglu  Initial development
!
  use ESMF
  use dshr_strdata_mod , only: shr_strdata_init_from_inline

  implicit none

  private
  public stream_init
!
  contains

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------

    subroutine stream_init(comp, rc)
      ! input/output variables
      type(ESMF_GridComp), intent(in)  :: comp
      integer            , intent(out) :: rc

      integer          :: localPet
      type(ESMF_Grid)  :: grid
      type(ESMF_Mesh)  :: mesh
      type(ESMF_Clock) :: clock

      ! query compontn to retrieve required information
      call ESMF_GridCompGet(comp, grid=grid, clock=clock, localPet=localPet, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      ! Read ndep_stream namelist
      !if (masterproc) then


    end subroutine stream_init

end module module_inline
