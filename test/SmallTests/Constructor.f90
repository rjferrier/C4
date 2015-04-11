module Constructor

  implicit none
  private
  public :: someObj

  type, public :: SomeObjType
     private
     integer :: dummy
   contains
     procedure :: getComponent
  end type SomeObjType

  interface someObj
     module procedure someObj_scalar
     module procedure someObj_vector
  end interface
  

contains
  
  function someObj_scalar( scalar )
    integer :: scalar
    type(SomeObjType) :: someObj_scalar
    someObj_scalar%dummy = scalar
  end function someObj_scalar

  
  function someObj_vector( vector )
    integer, dimension(:) :: vector
    type(SomeObjType) :: someObj_vector
    someObj_vector%dummy = product( vector )
  end function someObj_vector

  
  function getComponent( obj )
    class(SomeObjType), intent(in) :: obj
    integer :: getComponent
    getComponent = obj%dummy
  end function getComponent
  

    
end module Constructor
  
  

    
