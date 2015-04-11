module PointSimplicialComplexTests
  
  use TestUtilities
  use Global
  use FiniteVolumeModule

  ! dynamic module under test
  use PolytopeComplexesModule

  ! dynamic modules that should not really be included here - earmark
  ! for faking/mocking
  use PolytopeCollectionsModule

  implicit none

  character(*), parameter, private :: MOD_NAME = &
       'PointSimplicialComplexTests'

  class(PolytopeComplexInterface), allocatable :: sc
  type(TestLogType) :: log  
  class(PolytopeArrayInterface), allocatable :: pointArray
  
contains

  subroutine setUp
    call log%init( MOD_NAME, 'setUp' )
    
    ! some arbitrary points
    call createPointArray( pointArray, [&
         1.2_FLOAT, 0.0_FLOAT, &
         1.5_FLOAT, 0.1_FLOAT, &
         1.3_FLOAT, 0.1_FLOAT], log )
    if ( log%test(FATAL) ) return
    
  end subroutine setUp
  

  subroutine tearDown
    call log%reinit( MOD_NAME, 'tearDown' )
    
    call destroy( pointArray, log )
    call destroy( sc, log )
    
    call log%report()
    call log%deinit()
  end subroutine tearDown

  
  subroutine testCreateWithNoComplaints
    if ( log%test(MOD_NAME, 'testCreateWithNoComplaints', FATAL) ) return
    
    call createUnmixedSimplicialComplex( sc, pointArray, [1:3], log )
  end subroutine testCreateWithNoComplaints
  

  subroutine testCreateEmpty
    integer, dimension(0) :: absentIndices
    if ( log%test(MOD_NAME, 'testCreateEmpty', FATAL) ) return
    
    call createUnmixedSimplicialComplex( sc, pointArray, &
         absentIndices, log )
  end subroutine testCreateEmpty

  
  
end module PointSimplicialComplexTests
