module module_inline
!
!*** this module contains the subroutines for cdeps inline capability 
!
! revision history
!  22 Aug 2023: U. Turuncoglu  Initial development
!
  use ESMF
  use dshr_mod        , only: dshr_pio_init
  use dshr_strdata_mod, only: shr_strdata_type
  use dshr_strdata_mod, only: shr_strdata_init_from_inline
  use dshr_strdata_mod, only: shr_strdata_advance

  implicit none

  private
  public stream_init
  public stream_run

  type(ESMF_RouteHandle) :: rh_m2g ! routehandle to move data from mesh to grid

  type(shr_strdata_type) :: sdat   ! input data stream

  type stream_config
     character(len=ESMF_MAXSTR) :: stream_meshFile
     character(len=ESMF_MAXSTR), allocatable :: stream_fileNames(:)
     integer :: stream_yearFirst
     integer :: stream_yearLast
     integer :: stream_yearAlign
     character(len=ESMF_MAXSTR), allocatable :: stream_fldListFile(:)
     character(len=ESMF_MAXSTR), allocatable :: stream_fldListModel(:)
     character(len=ESMF_MAXSTR) :: stream_levDimName
     character(len=ESMF_MAXSTR) :: stream_mapAlgo
     integer :: stream_offset
     character(len=ESMF_MAXSTR) :: stream_taxMode
     real(kind=8) :: stream_dtLimit
     character(len=ESMF_MAXSTR) :: stream_tIntAlgo
     character(len=ESMF_MAXSTR) :: stream_name
     character(len=ESMF_MAXSTR) :: name
  end type stream_config
  type(stream_config) :: config

  integer :: dbug = 1
!
  contains

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------

    subroutine stream_init(comp, clock, rc)
      ! input/output variables
      type(ESMF_GridComp), intent(in)  :: comp
      type(ESMF_Clock)   , intent(in)  :: clock
      integer            , intent(out) :: rc

      integer         :: localPet
      type(ESMF_Grid) :: grid
      type(ESMF_Mesh) :: mesh
      type(ESMF_ArraySpec) :: arraySpec

      ! query compontn to retrieve required information
      call ESMF_GridCompGet(comp, grid=grid, localPet=localPet, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      ! read configuration
      call read_config(rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      ! create mesh from grid
      mesh = ESMF_MeshCreate(grid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      ! init pio
      call dshr_pio_init(comp, sdat, 6, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      ! initialize the cdeps data type
      call shr_strdata_init_from_inline(sdat, &
                                        my_task             = localPet, &
                                        logunit             = 6, &
                                        compname            = 'ATM', &
                                        model_clock         = clock, &
                                        model_mesh          = mesh, &
                                        stream_meshfile     = trim(config%stream_meshFile), &
                                        stream_filenames    = config%stream_fileNames, &
                                        stream_yearFirst    = config%stream_yearFirst, &
                                        stream_yearLast     = config%stream_yearLast, &
                                        stream_yearAlign    = config%stream_yearAlign, &
                                        stream_fldlistFile  = config%stream_fldListFile, &
                                        stream_fldListModel = config%stream_fldListModel, &
                                        stream_lev_dimname  = trim(config%stream_levDimName), &
                                        stream_mapalgo      = trim(config%stream_mapAlgo), &
                                        stream_offset       = config%stream_offset, &
                                        stream_taxmode      = trim(config%stream_taxMode), &
                                        stream_dtlimit      = config%stream_dtLimit, &
                                        stream_tintalgo     = trim(config%stream_tIntAlgo), &
                                        stream_name         = trim(config%stream_name), &
                                        rc                  = rc) 
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    end subroutine stream_init

  !-----------------------------------------------------------------------------

    subroutine stream_run(clock, rc)
      ! input/output variables
      type(ESMF_Clock)   , intent(in)  :: clock
      integer            , intent(out) :: rc

      integer :: item
      integer :: curr_ymd, sec
      integer :: year, month, day, hour, minute, second
      character(len=ESMF_MAXSTR) :: filename
      type(ESMF_Time)  :: currTime
      type(ESMF_Field) :: fgrid, fmesh

      ! query clock
      call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      ! get current time
      call ESMF_TimeGet(currTime, yy=year, mm=month, dd=day, h=hour, m=minute, s=second, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      curr_ymd = abs(year)*10000+month*100+day
      sec = hour*3600+minute*60+second

      ! run inline cdeps
      call shr_strdata_advance(sdat, ymd=curr_ymd, tod=sec, logunit=6, istr='atm', rc=rc)      

      ! loop over fields
      do item = 1, size(config%stream_fldListFile)
         ! get field on mesh
         call ESMF_FieldBundleGet(sdat%pstrm(1)%fldbun_model, fieldName=trim(config%stream_fldListFile(item)), field=fmesh, rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

         ! write field to check it
         if (dbug > 0) then
            write(filename, fmt='(a,i4,a1,i2.2,a1,i2.2,a1,i5.5)') trim(config%stream_fldListFile(item)), &
               year, '-', month, '-', day, '-', sec
            call ESMF_FieldWriteVTK(fmesh, trim(filename), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
         end if


      end do


    end subroutine stream_run

  !-----------------------------------------------------------------------------

    subroutine read_config(rc)
      ! input/output variables
      integer, intent(out) :: rc

      ! local variables
      type(ESMF_Config) :: cf
      character(ESMF_MAXPATHLEN) :: fname
      character(ESMF_MAXPATHLEN) :: MapAlgo(5)
      character(ESMF_MAXPATHLEN) :: TaxMode(3)
      character(ESMF_MAXPATHLEN) :: tIntAlgo(5)
      integer :: item, line, lineCount, columnCount
      logical :: found

      rc = ESMF_SUCCESS

      ! create ESMF config object to read in namelist options
      cf = ESMF_ConfigCreate(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      fname = "stream.cfg"
      call ESMF_ConfigLoadFile(cf, fname, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      ! stream_meshFile
      call ESMF_ConfigGetAttribute(cf, config%stream_meshFile, label='stream_meshFile:', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      ! stream_fileNames
      call ESMF_ConfigGetDim(cf, lineCount, columnCount, label='stream_fileNames::', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      if (.not. allocated(config%stream_fileNames)) then
         allocate(config%stream_filenames(lineCount))
      end if

      call ESMF_ConfigFindLabel(cf, 'stream_fileNames::', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      do line = 1, lineCount
         call ESMF_ConfigNextLine(cf, rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

         call ESMF_ConfigGetAttribute(cf, config%stream_fileNames(line), rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      end do

      ! stream_yearFirst
      call ESMF_ConfigGetAttribute(cf, config%stream_yearFirst, label='stream_yearFirst:', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      ! stream_yearLast
      call ESMF_ConfigGetAttribute(cf, config%stream_yearLast, label='stream_yearLast:', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return 

      ! stream_yearAlign
      call ESMF_ConfigGetAttribute(cf, config%stream_yearAlign, label='stream_yearAlign:', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      ! stream_fldListFile and stream_fldListModel (both same)
      call ESMF_ConfigGetDim(cf, lineCount, columnCount, label='stream_fldListFile::', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      if (.not. allocated(config%stream_fldListFile)) then
         allocate(config%stream_fldListFile(lineCount))
         allocate(config%stream_fldListModel(lineCount))
      end if

      call ESMF_ConfigFindLabel(cf, 'stream_fldListFile::', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      do line = 1, lineCount
         call ESMF_ConfigNextLine(cf, rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
 
         call ESMF_ConfigGetAttribute(cf, config%stream_fldListFile(line), rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

         config%stream_fldListModel(line) = config%stream_fldListFile(line)
      end do

      ! stream_levDimName
      call ESMF_ConfigGetAttribute(cf, config%stream_LevDimName, label='stream_levDimName:', default='null', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      ! stream_mapAlgo
      call ESMF_ConfigGetAttribute(cf, config%stream_mapAlgo, label='stream_mapAlgo:', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      mapAlgo = (/ 'redist  ', 'nn      ', 'bilinear', 'constd  ', 'constf  ' /)
      found = .false.
      do item = 1, size(mapAlgo)
         if (trim(config%stream_mapAlgo) == trim(mapAlgo(item))) found = .true.
      end do
      if (.not. found) then
          call ESMF_LogWrite('Wrong stream_mapAlgo - '//trim(config%stream_mapAlgo)//'. The valid values are '// &
             'redist, nn, bilinear, constd, constf.')
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      end if

      ! stream_Offset
      call ESMF_ConfigGetAttribute(cf, config%stream_offset, label='stream_offset:', default=0, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      ! stream_TaxMode
      call ESMF_ConfigGetAttribute(cf, config%stream_taxMode, label='stream_taxMode:', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      taxMode = (/ 'extend', 'cycle ', 'limit ' /)
      found = .false.
      do item = 1, size(taxMode)
         if (trim(config%stream_taxMode) == trim(taxMode(item))) found = .true.
      end do
      if (.not. found) then
          call ESMF_LogWrite('Wrong stream_taxMode - '//trim(config%stream_taxMode)//'. The valid values are '// &
             'extend, cycle, limit.')
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      end if

      ! stream_dtLimit
      call ESMF_ConfigGetAttribute(cf, config%stream_dtLimit, label='stream_dtLimit:', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      ! stream_tIntAlgo
      call ESMF_ConfigGetAttribute(cf, config%stream_tIntAlgo, label='stream_tIntAlgo:', default='linear', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      tIntAlgo = (/ 'lower  ', 'upper  ', 'nearest', 'linear ', 'coszen ' /)
      found = .false.
      do item = 1, size(tIntAlgo)
         if (trim(config%stream_tIntAlgo) == trim(tIntAlgo(item))) found = .true.
      end do
      if (.not. found) then
          call ESMF_LogWrite('Wrong stream_tIntAlgo - '//trim(config%stream_tIntAlgo)//'. The valid values are '// &
             'lower, upper, nearest, linear, coszen.')
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      end if

      ! stream_name
      call ESMF_ConfigGetAttribute(cf, config%stream_name, label='stream_name:', default='unknown', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    end subroutine read_config

end module module_inline
