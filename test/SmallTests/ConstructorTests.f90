module ConstructorTests

  use pFUnit
  use Constructor
  
  
  implicit none
  

contains

  subroutine testConstructWithScalar
    type(SomeObjType) :: obj
    
    obj = someObj(5)
    call assertEqual( 5, obj%getComponent() )
  end subroutine testConstructWithScalar


  subroutine testConstructWithVector
    type(SomeObjType) :: obj
    
    obj = someObj((/2, 3/))
    call assertEqual( 6, obj%getComponent() )
  end subroutine testConstructWithVector

end module ConstructorTests
