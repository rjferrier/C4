!> Combines pFUnit and LogModule and introduces a hybrid type for
!> the purpose of unit testing.
!
!> The hybrid type combines the LogType with pFUnit assertions
!> so that fatal errors can be logged in both frameworks before being
!> flagged for return.  The original LogType type should be
!> retained as an argument to methods residing in the source tree.
!> This new type should be used to manage diagnostics at the top
!> level, because it standardises the pattern of LogType usage
!> during unit tests.
! 
! Development notes: the new type was originally implemented as an
! extension to LogType.  Because the new extension was to be
! transparent to existing procedures in the source tree, method
! signatures had to be changed so that type(LogType)- i.e. a
! static dummy argument - was now class(LogType) -
! i.e. polymorphic.  Unfortunately this generated segmentation faults
! that could not be backtraced.  Possibly it is a compiler bug.  As a
! result the extension has been made non-polymorphic and we must deal
! with two object interfaces in our tests.
! 
module TestUtilities

  use LogModule
  use Echo
  use pFUnit

  implicit none
  public

  type, extends(LogType), public :: TestLogType
   contains
     ! overridden methods
     procedure :: reinit
     procedure :: checkDestructively
     procedure :: checkDestructivelyBeforeReinit
     generic :: test => checkDestructively, &
          checkDestructivelyBeforeReinit
     procedure, private :: checkDestructively_common
     procedure, private :: killDuring
  end type TestLogType

!!$  interface assertEqual
!!$     module procedure assertEqual_LogicalVector
!!$     module procedure assertEqual_IntVector
!!$  end interface

contains

  
  !-------------------------------------------------------------------
  !- TestLogType methods
  !-------------------------------------------------------------------

  subroutine reinit( obj, moduleName, procedureName )
    class(TestLogType), intent(inout) :: obj
    character(*), intent(in) :: moduleName, procedureName
    ! could kill the test if fatal errors exist, but then we might
    ! want to actively test for fatal errors - in which case such a
    ! test should pass
!!$    logical :: dummy
!!$    dummy = obj%checkDestructively_common( '  generated during &
!!$         &routine', FATAL )
    call obj%report()
    call obj%deinit()
    call obj%init( moduleName, procedureName )
  end subroutine reinit

  
  function checkDestructively_common( obj, msg, severity )
    class(TestLogType), intent(inout) :: obj
    character(*), intent(in) :: msg
    integer, intent(in) :: severity
    logical :: checkDestructively_common
    integer :: i

    checkDestructively_common = obj%check( severity )

    ! flag the error in the unit test
    call assertFalse( checkDestructively_common, trim(WARNINGS(obj%&
         getSeverity()+1))//msg//'.  See log file.' )
  end function checkDestructively_common

  
  subroutine killDuring( obj, testResult )
    class(TestLogType), intent(inout) :: obj
    logical :: testResult

    ! the original plan was to preserve the error to the end of the
    ! test, so that teardown could be allowed to clean up.  However,
    ! it appears that pFUnit kills the whole test if setup fails.
    if ( testResult ) then
!!$       call obj%endSub()  ! really needed? Seg errors if no sub
       call obj%report()
       call obj%deinit()
    end if
  end subroutine killDuring

  
  !> Call this during setup or the main unit tests every time a
  !> potentially destabilising procedure is called.
  ! 
  !> The method should be invoked with the clause 'if (
  !> obj%check('...', FATAL) ) return' or 'if ( obj%check('...') )
  !> return'.  This will simultaneously flag the (fatal) error in
  !> pFUnit and escape from the procedure in question.  The ellipsis
  !> represents a description of what was going on at the time.
  function checkDestructively( obj, severity )
    class(TestLogType), intent(inout) :: obj
    integer, intent(in) :: severity
    logical :: checkDestructively
    checkDestructively = obj%checkDestructively_common( ' reported', &
         severity )
    call obj%killDuring( checkDestructively )
  end function checkDestructively


  ! this does not work in the generic interface
!!$  function checkDestructivelyWithoutMsg( obj, severity )
!!$    class(TestLogType), intent(inout) :: obj
!!$    integer, intent(in), optional :: severity
!!$    logical :: checkDestructivelyWithoutMsg
!!$    checkDestructivelyWithoutMsg = obj%checkDestructively_common( ' reported', severity )
!!$    call obj%killDuring( checkDestructivelyWithoutMsg )
!!$  end function checkDestructivelyWithoutMsg


  !> Call this at the start of unit tests, after setup has taken
  !> place, with the clause 'if ( obj%checkBeforeReinit(MOD_NAME,
  !> PROC_NAME) ) return'.
  !
  !> If something went wrong in setup, this will escape immediately
  !> from the test in question while preserving the error object for
  !> handling in teardown.  Otherwise, the error object is
  !> reinitialised.
  !
  !> If non-fatal errors in setup are to be tolerated for a given
  !> test, invoke the method with 'if ( obj%checkBeforeReinit(FATAL,
  !> MOD_NAME, PROC_NAME) ) return'.
  function checkDestructivelyBeforeReinit( obj, moduleName, procedureName, &
       severity )
    class(TestLogType), intent(inout) :: obj
    integer, intent(in) :: severity
    character(*), intent(in) :: moduleName, procedureName
    logical :: checkDestructivelyBeforeReinit
    integer :: i
    checkDestructivelyBeforeReinit = &
         obj%checkDestructively_common( '  exist prior to this &
         &routine', severity )
    call obj%killDuring( checkDestructivelyBeforeReinit )
    
    ! if the error object passes, it is fit for reinitialisation
    if ( .not. checkDestructivelyBeforeReinit ) &
         call obj%reinit(moduleName, procedureName)
  end function checkDestructivelyBeforeReinit


end module TestUtilities
    
