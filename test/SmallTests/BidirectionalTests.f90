! See Bidirectional in the same folder
module BidirectionalTests

  use pFUnit
  use Bidirectional
  
  implicit none

  type(ConcreteClass1) :: obj1
  type(ConcreteClass2) :: obj2

  
contains

  subroutine setUp
    call obj1%link(obj2)
    call obj2%link(obj1)
  end subroutine setUp

  
  subroutine tearDown
    call obj1%unlink()
    call obj2%unlink()
  end subroutine tearDown
  

  subroutine testLink1
    call assertEqual( 1, obj1%signal() )
  end subroutine testLink1

  
  subroutine testLink2
    call assertEqual( 2, obj2%signal() )
  end subroutine testLink2
  
end module BidirectionalTests

