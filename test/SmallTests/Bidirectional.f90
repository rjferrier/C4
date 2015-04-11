! This is a very simple module that shows we can have bidirectional
! links between polymorphic variables.  In other words, Class1 might be
! defined before Class2, but as long as both classes share a common
! abstract type, Class1 can hold a link to Class2 and have it perform
! operations through the abstract interface.
module Bidirectional

  implicit none


  type, abstract :: AbstractClass
     private
     class(AbstractClass), pointer :: assoc
   contains
     procedure :: link
     procedure :: unlink
!!$     procedure :: getAssoc 
     procedure(intFn_classIn), deferred :: signal
  end type AbstractClass

  
  abstract interface
     function intFn_classIn( obj )
       import AbstractClass
       class(AbstractClass), intent(in) :: obj
       integer :: intFn_classIn
     end function intFn_classIn
  end interface


  type, extends(AbstractClass), public :: ConcreteClass1
     private
   contains
     procedure :: signal => signal1
  end type ConcreteClass1

  
  type, extends(AbstractClass), public :: ConcreteClass2
     private
   contains
     procedure :: signal => signal2
  end type ConcreteClass2


contains

  subroutine link( hostObj, targetObj )
    class(AbstractClass), intent(inout) :: hostObj
    class(AbstractClass), intent(in), target :: targetObj
    hostObj%assoc => targetObj
  end subroutine link
  

  subroutine unlink( hostObj )
    class(AbstractClass), intent(inout) :: hostObj
    nullify(hostObj%assoc)
  end subroutine unlink

  
!!$  function getAssoc( obj )    
!!$    class(AbstractClass), intent(in) :: obj
!!$    class(AbstractClass) :: getAssoc
!!$    getAssoc = obj%assoc
!!$  end function getAssoc


  function signal1( obj )
    class(ConcreteClass1), intent(in) :: obj
    integer :: signal1
    signal1 = 1
  end function signal1

  
  function signal2( obj )
    class(ConcreteClass2), intent(in) :: obj
    integer :: signal2
    signal2 = 2
  end function signal2


end module Bidirectional
