module FlowVariablesModule
  
  use LogModule
  use Global
  use FlowFieldModule
  
  implicit none
  private
  
  character(*), parameter :: MOD_NAME = 'FlowVariables'

  

  type, extends(FlowVariableInterface), public :: ScalarFlowVariableType
     private
   contains
     procedure :: init => init_Scalar
     procedure :: deinit => deinit_Scalar
     procedure :: getNumComponents => getNumComponents_Scalar
!!$     procedure :: computeMatrixRows => computeMatrixRows_Scalar
     procedure :: memberOf => memberOf_Scalar
  end type ScalarFlowVariableType
  
  
  type, extends(FlowVariableInterface), public :: VectorFlowVariableType
     private
     type(ScalarFlowVariableType), dimension(NDIM) :: components
   contains
     procedure :: init => init_Vector
     procedure :: deinit => deinit_Vector
     procedure :: getNumComponents => getNumComponents_Vector
!!$     procedure :: computeMatrixRows => computeMatrixRows_Vector
     procedure :: memberOf => memberOf_Vector
  end type VectorFlowVariableType

  
  !-------------------------------------------------------------------
  !- factories
  !-------------------------------------------------------------------

    
  type, public :: ScalarFlowVariableFactoryType
   contains
     procedure :: create => createScalarFlowVariable
     procedure, nopass :: destroy => destroyScalarFlowVariable
  end type ScalarFlowVariableFactoryType

    
  type, public :: VectorFlowVariableFactoryType
   contains
     procedure :: create => createVectorFlowVariable
     procedure, nopass :: destroy => destroyVectorFlowVariable
  end type VectorFlowVariableFactoryType
  
    
contains

  
  !-------------------------------------------------------------------
  !- ScalarFlowVariable methods
  !-------------------------------------------------------------------
  
  subroutine init_Scalar( obj, name, log )
    class(ScalarFlowVariableType), intent(out) :: obj
    character(*), intent(in) :: name
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_Scalar', log )

    call obj%initFlowVariable( name, log )

    call endSub( log )
  end subroutine init_Scalar
  

  subroutine deinit_Scalar( obj, log )
    class(ScalarFlowVariableType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_Scalar', log )

    call endSub( log )
  end subroutine deinit_Scalar


  pure function getNumComponents_Scalar( obj )
    class(ScalarFlowVariableType), intent(in) :: obj
    integer :: getNumComponents_Scalar
    getNumComponents_Scalar = 1
  end function getNumComponents_Scalar

  
  pure function memberOf_Scalar( obj, fv )
    class(ScalarFlowVariableType), intent(in) :: obj
    class(FlowVariableInterface), intent(in) :: fv
    logical :: memberOf_Scalar
    ! scalar variable is the most primitive of variables - no variable
    ! can be a component of it
    memberOf_Scalar = .false.
  end function memberOf_Scalar

  
!!$  subroutine computeMatrixRows_Scalar( obj, pointMomentGroup, log )
!!$    class(ScalarFlowVariableType), intent(in) :: obj
!!$    class(PointMomentGroupInterface), intent(inout) :: &
!!$         pointMomentGroup
!!$    class(LogType), intent(inout) :: log
!!$    call beginSub( MOD_NAME, 'computeMatrixRows_Scalar', log )
!!$
!!$    ! delegate, but note that this time the target rows are assumed to
!!$    ! be NCOEFS_SL long.  Compare with the vector FV variant.
!!$    call pointMomentGroup%computeSubRows( log )
!!$
!!$    call endSub( log )
!!$  end subroutine computeMatrixRows_Scalar

  
  !-------------------------------------------------------------------
  !- VectorFlowVariable methods
  !-------------------------------------------------------------------
  
  subroutine init_Vector( obj, name, log )
    class(VectorFlowVariableType), intent(out) :: obj
    character(*), intent(in) :: name
    class(LogType), intent(inout), optional :: log
    character(NDIM), parameter :: sufficies = 'XYZ'
    integer :: i
    call beginSub( MOD_NAME, 'init_Vector', log )

    call obj%initFlowVariable( name, log )
    
    ! loop over dimensions initialising each scalar sub-variable
    do i = 1, NDIM
       call obj%components(i)%init( name//sufficies(i:i), log )
    end do

    call endSub( log )
  end subroutine init_Vector
  

  subroutine deinit_Vector( obj, log )
    class(VectorFlowVariableType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_Vector', log )

    call endSub( log )
  end subroutine deinit_Vector


  pure function getNumComponents_Vector( obj )
    class(VectorFlowVariableType), intent(in) :: obj
    integer :: getNumComponents_Vector
    getNumComponents_Vector = NDIM
  end function getNumComponents_Vector

  
  pure function memberOf_Vector( obj, fv )
    class(VectorFlowVariableType), intent(in) :: obj
    class(FlowVariableInterface), intent(in) :: fv
    logical :: memberOf_Vector
    integer :: i
    memberOf_Vector = .false.
    do i = 1, NDIM
       if ( fv%sameAs(obj%components(i)) ) then
          memberOf_Vector = .true.
          exit
       end if
    end do
  end function memberOf_Vector
  
  
!!$  subroutine computeMatrixRows_Vector( obj, pointMomentGroup, log )
!!$    class(VectorFlowVariableType), intent(in) :: obj
!!$    class(PointMomentGroupInterface), intent(inout) :: &
!!$         pointMomentGroup
!!$    class(LogType), intent(inout) :: log
!!$    call beginSub( MOD_NAME, 'computeMatrixRows_Vector', log )
!!$
!!$    do i = 1, NDIM
!!$       ! delegate back to pmg, selecting a portion of the target rows
!!$       ! at a time
!!$       call pointMomentGroup%computeSubRows( i, log )
!!$    end do
!!$
!!$    call endSub( log )
!!$  end subroutine computeMatrixRows_Vector

  
  !-------------------------------------------------------------------
  !- ScalarFlowVariable factory methods
  !-------------------------------------------------------------------
  
  subroutine createScalarFlowVariable( obj, sfv, name, log )
    class(ScalarFlowVariableFactoryType), intent(in) :: obj
    class(FlowVariableInterface), allocatable, intent(inout) :: sfv
    character(*), intent(in) :: name
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'createScalarFlowVariable', log )
    
    ! allocate
    if ( allocated(sfv) ) then
       call addEvent( FATAL, 'sfv already allocated.', log )
    else
       allocate( ScalarFlowVariableType :: sfv, stat=allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &ScalarFlowVariableType :: sfv.  STAT='&
	    //int2str(allocStat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise
    select type ( sfv )
    class is (ScalarFlowVariableType)
       call sfv%init( name, log )
    end select
    call endSub( log )
  end subroutine createScalarFlowVariable
  
  
  subroutine destroyScalarFlowVariable( sfv, log )
    class(FlowVariableInterface), allocatable, intent(inout) :: sfv
    class(LogType), intent(inout), optional :: log
    integer :: deallocStat
    call beginSub( MOD_NAME, 'destroyScalarFlowVariable', log )
    
    if ( .not. allocated(sfv) ) then
       call addEvent( WARNING, 'sfv already deallocated.', log )
    else
       call sfv%deinit( log )
       deallocate( sfv, stat=deallocStat )
       call addEvent( deallocStat/=0, FATAL, 'Problem deallocating &
            sfv.  STAT='//int2str(deallocStat), log )
    end if
    
    call endSub( log )
  end subroutine destroyScalarFlowVariable


  
  !-------------------------------------------------------------------
  !- VectorFlowVariable factory methods
  !-------------------------------------------------------------------
  
  subroutine createVectorFlowVariable( obj, vfv, name, log )
    class(VectorFlowVariableFactoryType), intent(in) :: obj
    class(FlowVariableInterface), allocatable, intent(inout) :: vfv
    character(*), intent(in) :: name
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'createVectorFlowVariable', log )
    
    ! allocate
    if ( allocated(vfv) ) then
       call addEvent( FATAL, 'vfv already allocated.', log )
    else
       allocate( VectorFlowVariableType :: vfv, stat=allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &VectorFlowVariableType :: vfv.  STAT='&
	    //int2str(allocStat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise
    select type ( vfv )
    class is (VectorFlowVariableType)
       call vfv%init( name, log )
    end select
    call endSub( log )
  end subroutine createVectorFlowVariable
  
  
  subroutine destroyVectorFlowVariable( vfv, log )
    class(FlowVariableInterface), allocatable, intent(inout) :: vfv
    class(LogType), intent(inout), optional :: log
    integer :: deallocStat
    call beginSub( MOD_NAME, 'destroyVectorFlowVariable', log )
    
    if ( .not. allocated(vfv) ) then
       call addEvent( WARNING, 'vfv already deallocated.', log )
    else
       call vfv%deinit( log )
       deallocate( vfv, stat=deallocStat )
       call addEvent( deallocStat/=0, FATAL, 'Problem deallocating &
            vfv.  STAT='//int2str(deallocStat), log )
    end if
    
    call endSub( log )
  end subroutine destroyVectorFlowVariable


  

  
  
end module FlowVariablesModule
