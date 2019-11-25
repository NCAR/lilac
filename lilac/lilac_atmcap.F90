module lilac_atmcap

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! This is a dummy atmosphere cap for setting up lilac structure.
  !-----------------------------------------------------------------------

  ! !USES
  use ESMF
  use spmdMod     , only : masterproc
  use lilac_utils , only : gindex_atm
  use lilac_utils , only : lnd2atm, atm2lnd
  use clm_varctl  , only : iulog
  implicit none

  include 'mpif.h'

  public :: atmos_register

  character(*), parameter :: modname =  "atmos_cap"
  integer, parameter      :: debug = 1 ! internal debug level

!========================================================================
contains
!========================================================================

  subroutine atmos_register (comp, rc)

    type(ESMF_GridComp)          :: comp   ! must not be optional
    integer, intent(out)         :: rc
    character(len=*), parameter  :: subname=trim(modname)//':(atmos_register) '
    !-------------------------------------------------------------------------

    print *, "in user register routine"

    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Set the entry points for standard ESMF Component methods
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, userRoutine=atmos_init, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUN, userRoutine=atmos_run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, userRoutine=atmos_final, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

  end subroutine atmos_register

!========================================================================

  subroutine atmos_init (comp, lnd2atm_a_state, atm2lnd_a_state, clock, rc)

    ! input/output variables
    type (ESMF_GridComp) ::  comp
    type (ESMF_State)    ::  lnd2atm_a_state, atm2lnd_a_state
    type (ESMF_Clock)    ::  clock
    integer, intent(out) ::  rc

    ! local variables
    type (ESMF_FieldBundle)     :: c2a_fieldbundle , a2c_fieldbundle
    type(ESMF_Mesh)             :: atmos_mesh
    character(len=ESMF_MAXSTR)  :: atmos_mesh_filepath
    integer                     :: n, i, myid
    integer                     :: mpierror, numprocs
    integer                     :: petCount, localrc, urc
    character(*),parameter      :: F02 =   "('[atmos_cap]',a,i5,2x,d26.19)"
    character(len=*), parameter :: subname=trim(modname)//': [atmos_init] '
    !-------------------------------------------------------------------------

    ! Initialize return code
    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//"------------------------!", ESMF_LOGMSG_INFO)

    call ESMF_GridCompGet (comp, petcount=petcount, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)

    !-------------------------------------------------------------------------
    ! Read in the mesh
    !-------------------------------------------------------------------------

    ! TODO: use ESMF VM calls
    call MPI_Comm_size(MPI_COMM_WORLD, numprocs, mpierror)
    call MPI_Comm_rank(MPI_COMM_WORLD, myid, mpierror)

    atmos_mesh_filepath = '/glade/p/cesmdata/cseg/inputdata/share/meshes/fv4x5_050615_polemod_ESMFmesh.nc'

    atmos_distgrid = ESMF_DistGridCreate (arbSeqIndexList=gindex_atm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    atmos_mesh = ESMF_MeshCreate(filename=trim(atmos_mesh_filepath), fileformat=ESMF_FILEFORMAT_ESMFMESH, &
         elementDistGrid=atmos_distgrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    call ESMF_LogWrite(subname//"Mesh for atmosphere is created!", ESMF_LOGMSG_INFO)
    !print *, "!Mesh for atmosphere is created!"

    !-------------------------------------------------------------------------
    ! Atmosphere to Coupler (land) Fields --  atmos --> land
    ! - Create empty field bundle -- a2c_fieldbundle
    ! - Create  Fields and add them to field bundle
    ! - Add a2c_fieldbundle to state (atm2lnd_a_state)
    !-------------------------------------------------------------------------

    ! *** NOTE - THE HOST ATMOSPHERE IS RESPONSIBLE for allocating the
    ! memory for atm2lnd and lnd2atm pointers in lilac_utils ***
    ! *** NOTE - the HOST ATMOSPHERE is also responsible for filling in the gindex information in
    ! lilac_utils - this is used to create the distgrid for the mesh in lilac ***

    ! Create individual fields and add to field bundle -- a2c
    a2c_fieldbundle = ESMF_FieldBundleCreate(name="a2c_fieldbundle", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    ! create fields and add to field bundle
    do n = 1, size(atm2lnd)
       field = ESMF_FieldCreate(atmos_mesh, meshloc=ESMF_MESHLOC_ELEMENT, &
            name=trim(atm2lnd(n)%fldname), farrayPtr=atm2lnd(n)%dataptr, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
       call ESMF_FieldBundleAdd(c2a_fieldbundle, (/field/), rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    end do

    call ESMF_LogWrite(subname//"fieldbundleadd is finished .... !", ESMF_LOGMSG_INFO)
    print *, "!Fields to  Coupler (atmos to  land ) (a2c_fieldbundle) Field Bundle Created!"

    ! Add field bundle to state
    call ESMF_StateAdd(atm2lnd_a_state, (/a2c_fieldbundle/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    call ESMF_LogWrite(subname//"atm2lnd_a_state is filled with dummy_var field bundle!", ESMF_LOGMSG_INFO)
    print *, "!atm2lnd_a_state is filld with dummy_var field bundle!"

    !-------------------------------------------------------------------------
    ! Coupler (land) to Atmosphere Fields --  c2a
    ! - Create Field Bundle -- c2a_fieldbundle for because we are in atmos
    ! - Create  Fields and add them to field bundle
    ! - Add c2a_fieldbundle to state (lnd2atm_a_state)
    !-------------------------------------------------------------------------

    c2a_fieldbundle = ESMF_FieldBundleCreate (name="c2a_fieldbundle", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    ! create fields and add to field bundle
    do n = 1, size(lnd2atm)
       field = ESMF_FieldCreate(atmos_mesh, meshloc=ESMF_MESHLOC_ELEMENT, &
            name=trim(lnd2atm(n)%stdname), farrayPtr=lnd2atm(n)%dataptr, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
       call ESMF_FieldBundleAdd(c2a_fieldbundle, (/field/), rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

       if (if debug > 0) then
          call ESMF_FieldPrint(field,  rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
       end if
    end do
    call ESMF_LogWrite(subname//"c2a fieldbundleadd is finished .... !", ESMF_LOGMSG_INFO)

    ! Add field bundle to state
    call ESMF_StateAdd(lnd2atm_a_state, (/c2a_fieldbundle/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    ! Set Attributes needed by land
    call ESMF_AttributeSet(lnd2atm_a_state, name="nextsw_cday", value=11, rc=rc)  ! TODO: mv what in the world is this???

  end subroutine atmos_init

!========================================================================

  subroutine atmos_run(comp, importState, exportState, clock, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: comp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(len=*), parameter :: subname=trim(modname)//': [atmos_run] '

    ! Initialize return code
    rc = ESMF_SUCCESS

    call ESMF_LogWrite(subname//"Should atmos_run ", ESMF_LOGMSG_INFO)

  end subroutine atmos_run

!========================================================================

  subroutine atmos_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: comp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    character(len=*), parameter :: subname=trim(modname)//': [atmos_final] '
    type (ESMF_FieldBundle)     ::  import_fieldbundle, export_fieldbundle

    ! Initialize return code
    rc = ESMF_SUCCESS

    call ESMF_StateGet(importState, "c2a_fieldbundle", import_fieldbundle, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    call ESMF_StateGet(exportState, "a2c_fieldbundle", export_fieldbundle, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    call ESMF_FieldBundleDestroy(import_fieldbundle, rc=rc)
    call ESMF_FieldBundleDestroy(export_fieldbundle, rc=rc)

    call ESMF_LogWrite(subname//"?? Are there any other thing for destroying in atmos_final??", ESMF_LOGMSG_INFO)

  end subroutine atmos_final

!========================================================================

  subroutine lilac_atmcap_addfield(fldname, dataptr, mesh, field_bundle, rc)

    ! input/output variables
    character(len=*), intent(in)    :: fldname
    real*8                          :: dataptr(:)
    type(ESMF_mesh), intent(in)     :: mesh
    type(ESMF_Field), intent(inout) :: field_bundle
    integer, intent(out)            :: rc

    ! local variables
    type(ESMF_Field) :: newfield
    !-----------------------------------------------------

    if (allocated(dataptr)) then
       newfield = ESMF_FieldCreate(mesh, meshloc=ESMF_MESHLOC_ELEMENT, name=trim(fldname), farrayPtr=dataptr, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

       call ESMF_FieldBundleAdd(field_bundle, (/newfield/), rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    else
       !TODO: what should be done here? just put out a warning that this is not allocated and proceed?
    end if

  end subroutine lilac_atmcap_addfield

end module lilac_atmcap
