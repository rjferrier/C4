module DynamicGenericModule
  
  USE_MACROS({Container})
  use LogModule
  use Global
  
  
  implicit none
  private
  public :: createA, createB, destroy
  
  character(*), parameter :: MOD_NAME = 'DynamicGenericModule'


  interface destroy
     module procedure destroyGeneric
  end interface
  
  
  type, abstract, public :: GenericInterface
     private
   contains
     procedure :: deinit
     procedure(g_c), deferred :: clone
     procedure(g_p), deferred, nopass :: describe
  end type GenericInterface
  

  abstract interface
     subroutine g_c( obj, tgt, log )
       import GenericInterface, LogType
       class(GenericInterface), intent(in) :: obj
       class(GenericInterface), allocatable, intent(out) :: tgt
       class(LogType), intent(inout) :: log
     end subroutine g_c

     function g_p()
       character(1) :: g_p
     end function g_p
  end interface


  type, extends(GenericInterface) :: GenericAType
   contains
     procedure :: clone => clone_A
     procedure, nopass :: describe => describe_A
  end type GenericAType

  
  type, extends(GenericInterface) :: GenericBType
   contains
     procedure :: clone => clone_B
     procedure, nopass :: describe => describe_B
  end type GenericBType
  
  
  TYPEDEFS_CONTAINER(Generic, Interface, List)
  
  
contains

  subroutine deinit( obj, log )
    class(GenericInterface), intent(inout) :: obj
    class(LogType), intent(inout) :: log
    call beginSub( MOD_NAME, 'deinit', log )

    call endSub( log )
  end subroutine deinit

  
  subroutine clone_A( obj, tgt, log )
    class(GenericAType), intent(in) :: obj
    class(GenericInterface), allocatable, intent(out) :: tgt
    class(LogType), intent(inout) :: log
    integer :: stat
    call beginSub( MOD_NAME, 'clone', log )

    allocate( GenericAType :: tgt, stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &GenericAType :: tgt.  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    call endSub( log )
  end subroutine clone_A

  
  function describe_A( )
    character(1) :: describe_A
    describe_A = 'A'
  end function describe_A

  
  subroutine clone_B( obj, tgt, log )
    class(GenericBType), intent(in) :: obj
    class(GenericInterface), allocatable, intent(out) :: tgt
    class(LogType), intent(inout) :: log
    integer :: stat
    call beginSub( MOD_NAME, 'clone', log )

    allocate( GenericBType :: tgt, stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &GenericBType :: tgt.  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    call endSub( log )
  end subroutine clone_B

  
  function describe_B( )
    character(1) :: describe_B
    describe_B = 'B'
  end function describe_B


  METHODDEFS_CONTAINER(Generic, Interface, List)

  
  !-------------------------------------------------------------------
  !- helper procedures
  !-------------------------------------------------------------------
  
  
  subroutine createA( gen, log )
    class(GenericInterface), allocatable :: gen
    class(LogType), intent(inout) :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'createA', log )
    
    allocate( GenericAType :: gen, stat=allocStat )
    call addEvent( allocStat/=0, FATAL, 'Problem allocating &
         &GenericAType :: gen.  STAT='//int2str(allocStat), log )

    call endSub( log )
  end subroutine createA

  
  subroutine createB( gen, log )
    class(GenericInterface), allocatable :: gen
    class(LogType), intent(inout) :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'createB', log )
    
    allocate( GenericBType :: gen, stat=allocStat )
    call addEvent( allocStat/=0, FATAL, 'Problem allocating &
         &GenericBType :: gen.  STAT='//int2str(allocStat), log )

    call endSub( log )
  end subroutine createB

  
  subroutine destroyGeneric( gen, log )
    class(GenericInterface), allocatable :: gen
    class(LogType), intent(inout) :: log
    integer :: deallocStat
    call beginSub( MOD_NAME, 'destroyGeneric', log )

    deallocate( gen, stat=deallocStat )
    call addEvent( deallocStat/=0, WARNING, 'Problem deallocating &
         &gen.  STAT='//int2str(deallocStat), log )
    
    call endSub( log )
  end subroutine destroyGeneric

    
  
end module DynamicGenericModule
