module StaticGenericModule
  
  USE_MACROS({Container})
  use LogModule
  use Global
  
  implicit none
  private
  
  character(*), parameter :: MOD_NAME = 'StaticGenericModule'

  type, public :: GenericType
     private
     character(1) :: name
   contains
     procedure :: init => initGeneric
     procedure :: deinit => deinitGeneric
     procedure :: describeGeneric
     procedure :: describe => describeGeneric
  end type GenericType

  
  type, extends(GenericType), public :: UniqueGenericType
     private
     integer :: ID
   contains
     procedure :: init => initUniqueGeneric
     procedure :: deinit => deinitUniqueGeneric
     procedure :: describe => describeUniqueGeneric
     procedure :: sameAs => sameAs_UniqueGeneric
  end type UniqueGenericType


  type(IDManagerType) :: IDManager
  
  EXPAND({TYPEDEFS_CONTAINER(Generic, Type, List)})
  
  EXPAND({TYPEDEFS_CONTAINER(UniqueGeneric, Type, Set, {}, 
     {procedure :: diagnostics})})
  
  
contains

  
  !-------------------------------------------------------------------
  !- Generic methods
  !-------------------------------------------------------------------

  subroutine initGeneric( obj, name, log )
    class(GenericType), intent(out) :: obj
    character(*), intent(in) :: name
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'initGeneric', log )
    
    obj%name = name

    call endSub( log )
  end subroutine initGeneric

  
  subroutine deinitGeneric( obj, log )
    class(GenericType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinitGeneric', log )
    
    obj%name = ''

    call endSub( log )
  end subroutine deinitGeneric

  
  pure function describeGeneric( obj )
    class(GenericType), intent(in) :: obj
    character(60) :: describeGeneric
    describeGeneric = trim(obj%name)
  end function describeGeneric
  

  !-------------------------------------------------------------------
  !- UniqueGeneric
  !-------------------------------------------------------------------
  

  subroutine initUniqueGeneric( obj, name, log )
    class(UniqueGenericType), intent(out) :: obj
    character(*), intent(in) :: name
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'initUniqueGeneric', log )
    
    call IDManager%requestID( obj%ID, log )
    obj%name = name

    call endSub( log )
  end subroutine initUniqueGeneric

  
  subroutine deinitUniqueGeneric( obj, log )
    class(UniqueGenericType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinitUniqueGeneric', log )
    
    call IDManager%returnID( obj%ID )
    obj%name = ''

    call endSub( log )
  end subroutine deinitUniqueGeneric

  
  pure function describeUniqueGeneric( obj )
    class(UniqueGenericType), intent(in) :: obj
    character(60) :: describeUniqueGeneric
    describeUniqueGeneric = trim(obj%describeGeneric())//' (ID '//&
         str(obj%ID)//')'
  end function describeUniqueGeneric
  

  pure function sameAs_UniqueGeneric( obj, g )
    class(UniqueGenericType), intent(in) :: obj
    class(UniqueGenericType), intent(in) :: g
    logical :: sameAs_UniqueGeneric
    sameAs_UniqueGeneric = ( obj%ID == g%ID )
  end function sameAs_UniqueGeneric

  
  !-------------------------------------------------------------------
  !- GenericList methods
  !-------------------------------------------------------------------

  EXPAND({METHODDEFS_CONTAINER(Generic, Type, List)})
  
  !-------------------------------------------------------------------
  !- UniqueGenericSet methods
  !-------------------------------------------------------------------
  
  EXPAND({METHODDEFS_CONTAINER(UniqueGeneric, Type, Set)})
  
  subroutine diagnostics( obj, label, log )
    class(UniqueGenericSetType), intent(in) :: obj
    character(*), intent(in) :: label
    class(LogType), intent(inout) :: log
    type(UniqueGenericSetIteratorType) :: iterator
    type(UniqueGenericType), pointer :: gen
    call beginSub( MOD_NAME, 'diagnostics', log )

    call addEvent( ADVICE, 'Set '//trim(label)//' consists of:', log )
    call iterator%init( obj )
    call iterator%first( gen )
    do
       if ( iterator%isDone() ) exit
       call addEvent( ADVICE, '   '//trim(gen%describe()), log )
       call iterator%next( gen )
    end do
       
    call endSub( log )
  end subroutine diagnostics
  
  
end module StaticGenericModule
