module RealVectorTests
  
  use pFUnit
  use Global
  
  implicit none
  
  character(*), parameter :: MOD_NAME = 'RealVectorTests'

  type(RealVectorType) :: a, b, c
  
contains
  

  subroutine testAdd
    a = vector( [1._FLOAT, 2._FLOAT] )
    b = vector( [4._FLOAT, 3._FLOAT] )
    c = vector( [5._FLOAT, 5._FLOAT] )

    call assertTrue( all(a+b == c), 'Vectors should be equal' )
  end subroutine testAdd
  

  subroutine testSubtract
    a = vector( [1._FLOAT, 2._FLOAT] )
    b = vector( [4._FLOAT, 3._FLOAT] )
    c = vector( [3._FLOAT, 1._FLOAT] )

    call assertTrue( all(b-a == c), 'Vectors should be equal' )
  end subroutine testSubtract

  
  subroutine testGoodEqualUnitVectors
    a = unitVector( [1._FLOAT, 2._FLOAT] )
    b = unitVector( [0.5_FLOAT, 1._FLOAT] )

    call assertTrue( all(a == b), 'Unit vectors should be equal' )
  end subroutine testGoodEqualUnitVectors

  
  subroutine testBadEqualUnitVectors
    a = unitVector( [1._FLOAT, 2._FLOAT] )
    b = unitVector( [-0.5_FLOAT, -1._FLOAT] )

    call assertFalse( all(a == b), 'Vectors should not be equal' )
  end subroutine testBadEqualUnitVectors

  
end module RealVectorTests


  
