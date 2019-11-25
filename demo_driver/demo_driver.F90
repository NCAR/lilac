program demo_lilac_driver

    !----------------------------------------------------------------------------
    !***  All the components are in the hierarchy seen here:
    !
    !           main driver* (WRF, demo_lilac_driver)
    !               |
    !               |
    !          lilac (not a gridded component!)
    !               |     |________________________.____________.......... gridded components
    !               |                              |                 |
    !         ESMF lilac_atmcap            ESMF land cap     ESMF river cap
    !                                              |                 |
    !                                             CTSM          Mizzouroute...  
    !----------------------------------------------------------------------------

    use ESMF
    use lilac_mod   , only : lilac_init, lilac_run, lilac_final
    use lilac_utils , only : lilac_atm2lnd, lilac_lnd2atm, this_clock, gindex_atm
    use mpi         , only : MPI_COMM_WORLD, MPI_COMM_NULL, MPI_Init, MPI_FINALIZE, MPI_SUCCESS

    implicit none

    type (this_clock)      :: this_time
    integer                :: comp_comm
    integer                :: ierr
    real    , allocatable  :: centerCoords(:,:)
    real    , allocatable  :: lon(:), lat(:)
    integer                :: mytask, ntasks
    integer                :: my_start, my_end
    integer                :: i_local, i_global
    integer                :: nlocal, nglobal
    integer                :: start_time               !-- start_time    start time
    integer                :: end_time                 !-- end_time      end time
    integer                :: curr_time                !-- cur_time      current time
    integer                :: itime_step               !-- itime_step    counter of time steps
    integer                :: g,i,k                    !-- indices
    character(len=128)     :: filename
    !------------------------------------------------------------------------

    start_time = 1
    end_time   = 48

    !-----------------------------------------------------------------------------
    ! Initiallize MPI
    !-----------------------------------------------------------------------------

    write(*, *) "MPI initialization starts ..."

    call MPI_init(ierr)
    if (ierr .ne. MPI_SUCCESS) then
       print *,'Error starting MPI program. Terminating.'
       call MPI_ABORT(MPI_COMM_WORLD, ierr)
    end if

    comp_comm = MPI_COMM_WORLD
    call MPI_COMM_RANK(comp_comm, mytask, ierr)
    call MPI_COMM_SIZE(comp_comm, ntasks, ierr)

    if (mytask == 0 ) then
       print *, "MPI initialization done ..., ntasks=", ntasks
    end if

    !-----------------------------------------------------------------------------
    ! Read mesh file to get number of points (n_points)
    !-----------------------------------------------------------------------------
    filename = '/glade/p/cesmdata/cseg/inputdata/share/meshes/fv4x5_050615_polemod_ESMFmesh.nc'
    call read_netcdf_mesh(filename, nglobal)
    print *, "number of global points is is:", nglobal

    !-----------------------------------------------------------------------------
    ! atmosphere domain decomposition
    !-----------------------------------------------------------------------------

    nlocal = nglobal / ntasks

    my_start = nlocal*mytask + min(mytask, mod(nglobal, ntasks)) + 1
    ! The first mod(nglobal,ntasks) of ntasks are the ones that have an extra point
    if (mytask < mod(nglobal, ntasks)) then
       nlocal = nlocal + 1
    end if
    my_end = my_start + nlocal - 1

    allocate(gindex_atm(nlocal))

    i_global = my_start
    do i_local = 1, nlocal
       gindex_atm(i_local) = i_global
       i_global = i_global + 1
    end do

    print *, "size gindex_atm for ", mytask,"is: ", size(gindex_atm)
    print *, "gindex_atm for      ", mytask,"is: ", gindex_atm

    !------------------------------------------------------------------------
    ! Initialize lilac
    !------------------------------------------------------------------------

    call lilac_init(nlocal)

    !------------------------------------------------------------------------
    ! Fill in atm2lnd type pointer data
    !------------------------------------------------------------------------

    ! first determine lats and lons
    allocate(lon(nlocal))
    allocate(lat(nlocal))
    do i_local = 1,nlocal
       i_global = gindex_atm(i_local)
       lon(i) = centerCoords(1,i_global)
       lon(i) = real(nint(lon(i))) ! rounding to nearest int
       lat(i) = centerCoords(2,i_global)
       lat(i) = real(nint(lat(i))) ! rounding to nearest int
    end do

    ! now fill in the dataptr values
    call atm_to_lilac (lon, lat)

    !------------------------------------------------------------------------
    ! Run lilac
    !------------------------------------------------------------------------

    itime_step = 1
    do curr_time = start_time, end_time
       call lilac_run( )
       itime_step = itime_step + 1
    end do

    !------------------------------------------------------------------------
    ! Finalize lilac
    !------------------------------------------------------------------------

    call lilac_final( )

    print *,  "======================================="
    print *,  " ............. DONE ..................."
    print *,  "======================================="

  !=====================
  contains
  !=====================

    subroutine read_netcdf_mesh(filename, nglobal)

      use netcdf
      implicit none

      !  input/output variables
      character(*) , intent(in)  :: filename
      integer      , intent(out) :: nglobal

      !  local Variables
      integer :: idfile
      integer :: ierror
      integer :: dimid_elem
      integer :: dimid_coordDim
      integer :: iddim_elem
      integer :: iddim_coordDim
      integer :: idvar_CenterCoords
      integer :: nelem
      integer :: coordDim
      character (len=100) :: string
      !-----------------------------------------------------------------------------

      ! Open mesh file and get the idfile
      ierror  = nf90_open ( filename, NF90_NOWRITE, idfile) ; call nc_check_err(ierror, "opening file", filename)

      ! Get the dimid of  dimensions
      ierror  = nf90_inq_dimid(idfile, 'elementCount'     , dimid_elem )
      call nc_check_err(ierror, "inq_dimid elementCount", filename)
      ierror  = nf90_inq_dimid(idfile, 'coordDim'         , dimid_coordDim  )
      call nc_check_err(ierror, "coordDim", filename)

      ! Inquire dimensions based on their dimeid(s)
      ierror = nf90_inquire_dimension(idfile, dimid_elem        , string, nelem     )
      call nc_check_err(ierror, "inq_dim elementCount", filename)
      ierror = nf90_inquire_dimension(idfile, dimid_coordDim    , string, coordDim  )
      call nc_check_err(ierror, "inq_dim coordDim", filename)

      print *,  "======================================="
      print *, "number of elements is : ", nelem
      print *, "coordDim is :", coordDim
      print *,  "======================================="

      allocate (centerCoords(coordDim, nelem))

      ! Get coordinate values
      ierror = nf90_inq_varid(idfile, 'centerCoords' , idvar_centerCoords    )
      call nc_check_err(ierror, "inq_varid centerCoords", filename)
      ierror = nf90_get_var(idfile, idvar_CenterCoords     , centerCoords     , start=(/ 1,1/)   , count=(/ coordDim, nelem /)     )
      call nc_check_err(ierror,"get_var CenterCoords", filename)

      nglobal = nelem

    end subroutine read_netcdf_mesh

    !========================================================================
    subroutine nc_check_err(ierror, description, filename)

      use netcdf
      implicit none

      ! input/output variables
      integer     , intent(in) :: ierror
      character(*), intent(in) :: description
      character(*), intent(in) :: filename

      if (ierror /= nf90_noerr) then
         print *,  "ERROR"
         write (*,'(6a)') 'ERROR ', trim(description), '. NetCDF file : "', trim(filename), '". Error message:', &
              nf90_strerror(ierror)
      endif
    end subroutine nc_check_err

    !========================================================================
    subroutine atm_to_lilac (lon, lat)

      ! input/output variables
      real, intent(in) :: lon(:)
      real, intent(in) :: lat(:)

      ! local variables
      integer           :: lsize
      real*8, pointer   :: dataptr(:)
      integer           :: i
      integer           :: i_local
      ! --------------------------------------------------------

      lsize = size(lon)
      allocate(dataptr(lsize))

      dataptr(:) = 30.0d0 + lat(:)*0.01d0 + lon(:)*0.01d0
      call lilac_atm2lnd('Sa_z', dataptr)

      dataptr(:) =  10.0d0 + lat(:)*0.01d0 + lon(:)*0.01d0
      call lilac_atm2lnd('Sa_topo', dataptr)

      dataptr(:) =  20.0d0 + lat(:)*0.01d0 + lon(:)*0.01d0
      call lilac_atm2lnd('       Sa_u', dataptr)

      dataptr(:) =  40.0d0 + lat(:)*0.01d0 + lon(:)*0.01d0
      call lilac_atm2lnd('Sa_v', dataptr)

      dataptr(:) =  280.0d0 + lat(:)*0.01d0 + lon(:)*0.01d0
      call lilac_atm2lnd('Sa_ptem', dataptr)

      dataptr(:) =  100100.0d0 + lat(:)*0.01d0 + lon(:)*0.01d0
      call lilac_atm2lnd('Sa_pbot', dataptr)

      dataptr(:) = 280.0d0 + lat(:)*0.01d0 + lon(:)*0.01d0
      call lilac_atm2lnd('Sa_tbot', dataptr)

      dataptr(:) =  0.0004d0   !+(lat(:)*0.01d0 + lon(:)*0.01d0)*1.0e-8
      call lilac_atm2lnd('Sa_shum', dataptr)

      dataptr(:) =  200.0d0 + lat(:)*0.01d0 + lon(:)*0.01d0
      call lilac_atm2lnd('Faxa_lwdn', dataptr)

      !dataptr(:) =  0.0d0 +  (lat*0.01d0 + lon(:)*0.01d0)*1.0e-8
      dataptr(:) = 0.0d0
      call lilac_atm2lnd('Faxa_rainc', dataptr)

      dataptr(:) =  3.0d-8 +  (lat(:)*0.01d0 + lon(:)*0.01d0)*1.0e-8
      call lilac_atm2lnd('Faxa_rainl', dataptr)

      dataptr(:) =  1.0d-8 +  (lat(:)*0.01d0 + lon(:)*0.01d0)*1.0e-8
      call lilac_atm2lnd('Faxa_snowc', dataptr)

      dataptr(:) =  2.0d-8 +  (lat(:)*0.01d0 + lon(:)*0.01d0)*1.0e-8
      call lilac_atm2lnd('Faxa_snowl', dataptr)

      dataptr(:) =  100.0d0 + lat(:)*0.01d0 + lon(:)*0.01d0
      call lilac_atm2lnd('Faxa_swndr', dataptr)

      dataptr(:) =  50.0d0 + lat(:)*0.01d0 + lon(:)*0.01d0
      call lilac_atm2lnd('Faxa_swvdr', dataptr)

      dataptr(:) =  20.0d0 + lat(:)*0.01d0 + lon(:)*0.01d0
      call lilac_atm2lnd('Faxa_swndf', dataptr)

      dataptr(:) =  40.0d0 + lat(:)*0.01d0 + lon(:)*0.01d0
      call lilac_atm2lnd('Faxa_swvdf', dataptr)

    end subroutine atm_to_lilac

    !========================================================================
    subroutine lilac_to_atm ()

      ! local variables
      integer :: lsize
      real*8, pointer  :: dataptr(:)

      lsize = size(gindex_atm)
      allocate(dataptr(lsize))

      call lilac_lnd2atm('Sl_lfrin' , dataptr)
      call lilac_lnd2atm('Sl_t'     , dataptr)
      call lilac_lnd2atm('Sl_tref'  , dataptr)
      call lilac_lnd2atm('Sl_qref'  , dataptr)
      call lilac_lnd2atm('Sl_avsdr' , dataptr)
      call lilac_lnd2atm('Sl_anidr' , dataptr)
      call lilac_lnd2atm('Sl_avsdf' , dataptr)
      call lilac_lnd2atm('Sl_anidf' , dataptr)
      call lilac_lnd2atm('Sl_snowh' , dataptr)
      call lilac_lnd2atm('Sl_u10'   , dataptr)
      call lilac_lnd2atm('Sl_fv'    , dataptr)
      call lilac_lnd2atm('Sl_ram1'  , dataptr)
    end subroutine lilac_to_atm

end program demo_lilac_driver
