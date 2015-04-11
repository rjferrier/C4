module IntVectorTests
  
  use pFUnit
  use Global
  
  implicit none
  
  character(*), parameter :: MOD_NAME = 'IntVectorTests'

  type(IntVectorType) :: a, b, c
  
contains
  

  subroutine testAdd
    a = vector( [1, 2] )
    b = vector( [4, 3] )
    c = vector( [5, 5] )

    call assertTrue( all(a+b == c), 'Failed test' )
  end subroutine testAdd

  
  subroutine testAdd_MaskA
    a = vector( [1, 2] )
    call a%setMask( vector([.true., .false.]) )
    b = vector( [4, 3] )
    c = vector( [5, 3] )

    call assertTrue( all(a+b == c), 'Failed test' )
  end subroutine testAdd_MaskA

  
  subroutine testAdd_MaskA_scalar
    a = vector( [1, 2] )
    call a%setMask( vector([.true., .false.]) )
    ! let 1 be added to a
    c = vector( [2, 2] )

    call assertTrue( all(a+1 == c), 'Failed test' )
  end subroutine testAdd_MaskA_scalar
  

  subroutine testSubtract
    a = vector( [1, 2] )
    b = vector( [4, 3] )
    c = vector( [3, 1] )

    call assertTrue( all(b-a == c), 'Failed test' )
  end subroutine testSubtract
  

  subroutine testSubtract_MaskB
    a = vector( [1, 2] )
    b = vector( [4, 3] )
    call b%setMask( vector([.false., .true.]) )
    c = vector( [-1, 1] )

    call assertTrue( all(b-a == c), 'Failed test' )
  end subroutine testSubtract_MaskB
  

  subroutine testSubtract_MaskA_scalar
    a = vector( [1, 2] )
    call a%setMask( vector([.false., .true.]) )
    ! let 1 be subtracted from a
    c = vector( [1, 1] )

    call assertTrue( all(a-1 == c), 'Failed test' )
  end subroutine testSubtract_MaskA_scalar

  
  subroutine testGoodEqualVectors_MaskA_MaskB
    a = vector( [1, 2] )
    call a%setMask( vector([.false., .true.]) )
    b = vector( [3, 2] )
    call b%setMask( vector([.false., .true.]) )

    call assertTrue( all(a == b), 'Failed test' )
  end subroutine testGoodEqualVectors_MaskA_MaskB
  
  
  subroutine testBadEqualVectors_MaskA_MaskB
    a = vector( [1, 2] )
    call a%setMask( vector([.false., .true.]) )
    b = vector( [1, 2] )
    call b%setMask( vector([.true., .true.]) )

    call assertFalse( all(a == b), 'Failed test' )
  end subroutine testBadEqualVectors_MaskA_MaskB

  
  subroutine testBadEqualVector_MaskA_scalar
    a = vector( [1, 2] )
    call a%setMask( vector([.false., .true.]) )

    call assertTrue( any(a == 2), 'Failed test' )
    call assertFalse( all(a == 2), 'Failed test' )
  end subroutine testBadEqualVector_MaskA_scalar
  
  
!!$
!!$  subroutine testComplicatedAddAndSubtract
!!$    a = vector( [1, 2] )
!!$    call a%setMask( vector([.true., .false.]) )
!!$    b = vector( [4, 3] )
!!$    call b%setMask( vector([.false., .true.]) )
!!$    a = a + b
!!$    ! expect a to be [1, 3] with mask reset to true.
!!$    b = b - a
!!$    ! expect b to be [-1, 0] with mask reset to true.
!!$    
!!$    c = vector( [0, 3] )
!!$    call assertTrue( all(a+b == c), 'Failed test' )
!!$  end subroutine testSubtract_MaskB

  
end module IntVectorTests


  
