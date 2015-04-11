! Here is a simple grid to be tested.  It contains a number of
! different types of PolytopeCollection.  We will not test decorated
! polytopes (i.e. Cell, Face, Boundary types) at this stage - just the
! undecorated concrete types that hold connections.
!
!  p13------x23------p23------x23------p23-------x23-------p14 --0.8
!   |                 |                 |                   |
!   |                 |                 |                   |
!   |                 |                 |                   |
!  y31       r       y41       r       y41        r        y32
!   |                 |                 |                   |
!   |                 |                 |                   |
!   |                 |                 |                   |
!  p31------x41------p41------x41------p41-------x41-------p32 --0.4
!   |                 |                 |\\                /|
!   |                 |                 | \ \      B     /  |
!   |                 |                 |  \  f         e   d
!  y31       r       y41       r       y41  g   \      /    |
!   |                 |                 |    \    \  /    A |
!   |                 |                 |     \ C  ii---c--iii --0.1
!   |                 |                 | D    \  b
!  p11------x21------p21------x21------p21--a---i/             --0.0
!
!   |                 |                 |       |   |       |
!  0.0               0.5               1.0     1.2 1.3     1.5
!
module SmallStepGrid

  use TestUtilities
  use Global
  use FiniteVolumeModule

  ! dynamic module under test
  use PolytopeCollectionsModule
  
  implicit none
  
  character(*), parameter, private :: MOD_NAME = 'SmallStepGrid'

  
  ! declare polytope collections and their creators
  class(PolytopeArrayInterface), allocatable :: xPointArray, &
       yPointArray, freePointArray, xLineArray, yLineArray, &
       freeLineArray, triangleArray
  class(PolytopeGridInterface), allocatable :: pointGrid, xLineGrid, &
       yLineGrid, rectangleGrid

  
contains

  !-------------------------------------------------------------------
  !- fixture setup/teardown
  !-------------------------------------------------------------------

  subroutine setUp_SmallStepGrid( log )
    class(LogType), intent(inout) :: log
    type(RealVectorType) :: minCoords, maxCoords
    type(IntVectorType) :: pointGridSize
    type(PolytopeGridPointerType) :: nullGridPtr
    call beginSub( MOD_NAME, 'setUp_SmallStepGrid', log )
    
    pointGridSize = vector([4, 3])
    minCoords = vector([0.0_FLOAT, 0.0_FLOAT])
    maxCoords = vector([1.5_FLOAT, 0.8_FLOAT])

    call createPointGrid( pointGrid, pointGridSize, minCoords, &
         maxCoords, log )
    
    call createOrthotopeGrid( xLineGrid, &
         [ptr(pointGrid), nullGridPtr], log )
    call createOrthotopeGrid( yLineGrid, &
         [nullGridPtr, ptr(pointGrid)], log )
    call createOrthotopeGrid( rectangleGrid, [ptr(yLineGrid), ptr(&
         xLineGrid)], log )
    
    ! 'i' through 'iii' respectively:
    call createPointArray( xPointArray, &
         (/1.2_FLOAT, 0.0_FLOAT/), log )
    call createPointArray( yPointArray, &
         (/1.5_FLOAT, 0.1_FLOAT/), log )
    call createPointArray( freePointArray, &
         (/1.3_FLOAT, 0.1_FLOAT/), log ) 
    
    call createSimplexArray( xLineArray, 1, (/ &    ! 'a'
         ptr(pointGrid, (/3, 1/)),  &
         ptr(xPointArray, 1) &
         /), xPointArray, log )
        
    call createSimplexArray( yLineArray, 1, (/ &    ! 'd'
         ptr(pointGrid, (/4, 2/)),  &
         ptr(yPointArray, 1) &
         /), yPointArray, log )
    
    call createSimplexArray( freeLineArray, 1, (/&  ! 'b,c,e,f,g'
         ptr(xPointArray, 1),  &
         ptr(freePointArray, 1), &
         
         ptr(freePointArray, 1), &
         ptr(yPointArray, 1), &
         
         ptr(freePointArray, 1), &
         ptr(pointGrid, (/4, 2/)), &
         
         ptr(freePointArray, 1),  &
         ptr(pointGrid, (/3, 2/)), &
         
         ptr(xPointArray, 1),  &
         ptr(pointGrid, (/3, 2/)) &
         /), freePointArray, log )

    call createSimplexArray( triangleArray, 2, (/&  ! 'A' through 'D'
         ptr(freeLineArray, 2), &
         ptr(freeLineArray, 3), &
         ptr(yLineArray, 1), &
         
         ptr(freeLineArray, 3), &
         ptr(freeLineArray, 4), &
         ptr(xLineGrid, (/3, 2/)), &
         
         ptr(freeLineArray, 4), &
         ptr(freeLineArray, 5), &
         ptr(freeLineArray, 1), &         
         
         ptr(freeLineArray, 5), &
         ptr(yLineGrid, (/3, 1/)), &
         ptr(xLineArray, 1) &
         /), freeLineArray, log )

    call endSub( log )
  end subroutine setUp_SmallStepGrid


  subroutine tearDown_SmallStepGrid( log )
    class(LogType), intent(inout) :: log
    call beginSub( MOD_NAME, 'tearDown_SmallStepGrid', log )
    
    call destroy( rectangleGrid, log )
    call destroy( xLineGrid, log )
    call destroy( yLineGrid, log )
    call destroy( pointGrid, log )
    
    call destroy( triangleArray, log )
    call destroy( xLineArray, log )
    call destroy( yLineArray, log )
    call destroy( freeLineArray, log )
    call destroy( xPointArray, log )
    call destroy( yPointArray, log )
    call destroy( freePointArray, log )

    call endSub( log )
  end subroutine tearDown_SmallStepGrid

  
end module SmallStepGrid
