!
!      |                       |                       |
!      +H(4m+3)-P(2m+2)-H(4m+4)+---------------------/-+------          
!      |          /  \- T(16m+4)  T(16m+8)       /--- /|       
!      | T(16m+3)/     \-      |             /---    / |          
!      |        /    F(12m+2) V(4m+4)   F(12m+6)    /  |        
!      |    F(12m+3)       \-  |     /---          /   |         
!      |      /              \-| /---             /    |      / 
!      |     /  T(16m+2)     /Q(2m+2)  T(16m+7)  /     |     /
!      |    /            /---  | \-             /      |    /
!      |   /       F(12m+1)    |  F(12m+4)  F(12m+5)   |   /           
!      |  /      /---        V(4m+3) \-       /        |  /     
!      | /   /---    T(16m+1)  |       \-    / T(16m+6)| /   /--
!      |//---                  | T(16m+5)\- /          |//---
!      +---------------------/-+H(4m+1)-P(2m+1)-H(4m+2)+---------
!      |                 /--- /|          / \-         |
!      |   T12       /---    / |  T15    /    \-   T16 |      
!      V2        F9--       /  |        /      F11     V6        
!      |     /---          /   |       /          \-   |      /--
!      | /---             /    |     F12   T14      \- |  /---
!      Q1-               F8    |     /               /-Q4-
!      | \-     T11     /      |    /            /---  |\-       
!      |   \-          /       |   /        F10--      |  \-
!      V1    F7       /   T10  |  /      /---          V5   \-
!      |  T9   \-    /         | /   /---      T13     |      \-     /
!      |         \- /          |//---                  |        \-  /
!      +----H3-----P2----H4----+---------------------/-+----H7----P3----
!      |          /  \-     T4 |                 /--- /|          / \
!      |         /     \-      |   T8        /---    / |         /
!      |  T3    /        F2    V4        F6--       /  |  T19   /
!      |       /           \-  |     /---          /   |       /
!      |      F3    T2       \-| /---             /    |     F15
!      |     /               /-Q2        T7     F5     |     /   T18
!      |    /            /---  | \-             /      |    /     
!      |   /         F1--      |   \-          /       |   /       F13-
!      |  /      /---          V3    F4       /   T6   |  /     /--
!      | /   /---       T1     |       \-    /         | /   /--
!      |//---                  | T5      \- /          |//---     T17
!      +-----------------------+----H1-----P1----H2----+----------------
!
!
! Key:
!  P = point on horizontal line, Q = point on vertical line;
!  H = horizontal line, V = vertical line, F = free line;
!  T = triangle.
!
! This module generates the above 2m x 2n grid where m and n are integers.
! The grid is composed entirely of 'cut' Cartesian cells in the sense that
! each cell is subdivided into a complex of triangles, thus making it
! suitable for testing complex profile fitting.  To make the pattern
! repeatable, the grid has 2m x 2n cells where m and n are integers.  The
! unstructured elements are ordered as shown above.  Putting elements that
! bound each 2x2 group of cells first in the ordering is essential,
! otherwise there would be a jump in the indexing at the grid boundaries
! where such elements are needed in the absence of others.
! 
module TestGrid1

  use TestUtilities
  use Global
  use FiniteVolumeModule

  ! dynamic module under test
  use PolytopeCollectionsModule

  implicit none

  character(*), parameter, private :: MOD_NAME = 'TestGrid1'

  class(PolytopeGridInterface), allocatable :: pointGrid, xLineGrid, &
       yLineGrid, rectangleGrid
  class(PolytopeArrayInterface), allocatable :: xPointArray, &
       yPointArray, xLineArray, yLineArray, &
       freeLineArray, triangleArray 

  type(TestLogType) :: log

contains

  subroutine setup_TestGrid1( coordsMin, coordsMax, resolution, log )
    type(RealVectorType), intent(in) :: coordsMin, coordsMax
    type(IntVectorType), intent(in) :: resolution
    class(LogType), intent(inout) :: log
    type(PolytopeGridPointerType) :: nullGridPtr
    integer :: nGlobalPoints
    call beginSub( MOD_NAME, 'setUp_TestGrid1', log )
    
    call addEvent( any( mod(resolution%getValues(), 2) /= 0 ), FATAL, &
         'Grid cells must be even numbers in each direction.', log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! define structured elements
    call createPointGrid( pointGrid, resolution + 1, &    ! collection 1
         coordsMin, coordsMax, log )
    call createOrthotopeGrid( xLineGrid, &
         [ptr(pointGrid), nullGridPtr], log )             ! collection 2
    call createOrthotopeGrid( yLineGrid, &
         [nullGridPtr, ptr(pointGrid)], log )             ! collection 3
    call createOrthotopeGrid( rectangleGrid, &
         [ptr(yLineGrid), ptr(xLineGrid)], log )          ! collection 4
    
    ! define unstructured elements
    call setup_TestGrid1_unstructuredPoints( coordsMin, coordsMax, &
         resolution, log )                           ! collections 5-6    
    call setup_TestGrid1_unstructuredLines( &
         resolution, log )                           ! collections 7-9
    call setup_TestGrid1_unstructuredTriangles( & 
         resolution, log )                           ! collection 10
    
    call endSub( log )
  end subroutine setup_TestGrid1


  subroutine tearDown_TestGrid1( log )
    class(LogType), intent(inout) :: log
    call beginSub( MOD_NAME, 'tearDown_TestGrid1', log )
    
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

    call endSub( log )
  end subroutine tearDown_TestGrid1


  subroutine setup_TestGrid1_unstructuredPoints( coordsMin, coordsMax, &
       resolution, log )
    type(RealVectorType), intent(in) :: coordsMin, coordsMax
    type(IntVectorType), intent(in) :: resolution
    class(LogType), intent(inout) :: log
    real(FLOAT), dimension(:), allocatable :: coordsArrayP, coordsArrayQ
    integer :: mq, nq, iq, jq, i, j, p, q, stat
    real(FLOAT) :: dxq, dyq, x, y
    call beginSub( MOD_NAME, 'setup_TestGrid1_unstructuredPoints', log )

    mq = resolution%getValue(1)/2
    nq = resolution%getValue(2)/2
    dxq = (coordsMax%getValue(1) - coordsMin%getValue(1))/mq
    dyq = (coordsMax%getValue(2) - coordsMin%getValue(2))/nq
  
    ! there are (2*nq+1)*mq coordinates of type-P points and (2*mq+1)*nq
    ! coordinates of type Q points.
    allocate( coordsArrayP(NDIM*(2*nq+1)*mq), coordsArrayQ(NDIM*(2*mq+1)*&
         nq), stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating coordsArrayP(&
         &NDIM*(2*nq+1)*mq), coordsArrayQ(NDIM*(2*mq+1)*nq).  STAT='//&
         int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! use the largest possible value as an unset flag
    coordsArrayP = huge(0._FLOAT)
    coordsArrayQ = huge(0._FLOAT)

    ! loop over cell quadruples setting up P- and Q-coordinates
    do jq = 1, nq
       do iq = 1, mq
          p = NDIM*getIndex( 2, iq, jq, mq, nq, nExtraNorth=1 )
          q = NDIM*getIndex( 2, iq, jq, mq, nq, nExtraEast=1 )
          
          ! lower left hand coordinates of this cell quadruple
          x = coordsMin%getValue(1) + (iq-1)*dxq
          y = coordsMin%getValue(2) + (jq-1)*dyq
          
          ! P1, P3, etc.
          coordsArrayP(p + 1) = x + 0.75_FLOAT*dxq
          coordsArrayP(p + 2) = y + 0.00_FLOAT*dyq
          ! P2, P4, etc.
          coordsArrayP(p + 3) = x + 0.25_FLOAT*dxq
          coordsArrayP(p + 4) = y + 0.50_FLOAT*dyq
          
          ! at the end of each column of cells is one more P point
          if ( jq == nq ) then
             coordsArrayP(p + 5) = x + 0.75_FLOAT*dxq
             coordsArrayP(p + 6) = y + 1.00_FLOAT*dyq
          end if

          ! Q1, Q3, etc.
          coordsArrayQ(q + 1) = x + 0.00_FLOAT*dxq
          coordsArrayQ(q + 2) = y + 0.75_FLOAT*dyq
          ! Q2, Q4, etc.
          coordsArrayQ(q + 3) = x + 0.50_FLOAT*dxq
          coordsArrayQ(q + 4) = y + 0.25_FLOAT*dyq
       end do

       ! at the end of each row of cells is one more Q point
       coordsArrayQ(q + 5) = x + 1.00_FLOAT*dxq
       coordsArrayQ(q + 6) = y + 0.75_FLOAT*dyq
    end do

    ! complete?
    do i = 1, size(coordsArrayP)
       call addEvent( coordsArrayP(i) > coordsMax%getValue(2-mod(i, 2)), &
            FATAL, 'Element '//str(i)//' of coordsArrayP is still &
            &empty.', log )
    end do
    do i = 1, size(coordsArrayQ)
       call addEvent( coordsArrayQ(i) > coordsMax%getValue(2-mod(i, 2)), &
            FATAL, 'Element '//str(i)//' of coordsArrayQ is still &
            &empty.', log )
    end do
    
    call createPointArray( xPointArray, coordsArrayP, log )
    call createPointArray( yPointArray, coordsArrayQ, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    deallocate( coordsArrayP, coordsArrayQ, stat=stat )
    call addEvent( stat/=0, WARNING, 'Problem deallocating coordsArrayP, &
         &coordsArrayQ.  STAT='//int2str(stat), log )

    call endSub( log )
  end subroutine setup_TestGrid1_unstructuredPoints

  
  subroutine setup_TestGrid1_unstructuredLines( resolution, log )
    type(IntVectorType), intent(in) :: resolution
    class(LogType), intent(inout) :: log
    type(PolytopePointerType), dimension(:), allocatable :: ptpArrayH, &
         ptpArrayV, ptpArrayF
    integer :: mq, nq, iq, jq, i, j, p, q, h, v, f, pn, qe, stat
    type(PolytopePointerType) :: nullPtpPtr
    call beginSub( MOD_NAME, 'setup_TestGrid1_unstructuredLines', log )

    mq = resolution%getValue(1)/2
    nq = resolution%getValue(2)/2

    ! verticies are to be defined in pairs
    allocate( ptpArrayH(2*(4*nq+2)*mq), ptpArrayV(2*(4*mq+2)*&
         nq), ptpArrayF(2*12*mq*nq), stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating ptpArrayH(2*(2*&
         &nq+2)*mq), ptpArrayV(2*(2*mq+2)*nq), ptpArrayF(2*12*mq*&
         &nq).  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! loop over cell quadruples defining end verticies
    do jq = 1, nq
       do iq = 1, mq
          h = 2*getIndex( 4, iq, jq, mq, nq, nExtraNorth=2 )
          v = 2*getIndex( 4, iq, jq, mq, nq, nExtraEast=2 )
          f = 2*getIndex( 12, iq, jq, mq, nq )

          ! lower left hand point address of this cell quadruple
          i = 2*(iq - 1) + 1
          j = 2*(jq - 1) + 1
          ! reference index for each type of point
          p = getIndex( 2, iq, jq, mq, nq, nExtraNorth=1 )
          q = getIndex( 2, iq, jq, mq, nq, nExtraEast=1 )
          pn = getIndex( 2, iq, jq+1, mq, nq, nExtraNorth=1 )
          qe = getIndex( 2, iq+1, jq, mq, nq, nExtraEast=1 )
          
          ! H1, H5, etc.
          ptpArrayH(h+1) = ptr( pointGrid, [i+1, j] )
          ptpArrayH(h+2) = ptr( xPointArray, p+1 )
          ! H2, H6, etc.
          ptpArrayH(h+3) = ptr( xPointArray, p+1 )
          ptpArrayH(h+4) = ptr( pointGrid, [i+2, j] )
          ! H3, H7, etc.
          ptpArrayH(h+5) = ptr( pointGrid, [i, j+1] )
          ptpArrayH(h+6) = ptr( xPointArray, p+2 )
          ! H4, H8, etc.
          ptpArrayH(h+7) = ptr( xPointArray, p+2 )
          ptpArrayH(h+8) = ptr( pointGrid, [i+1, j+1] )

          ! at the end of each column of cells are two more H lines
          if ( jq == nq ) then
             ptpArrayH(h+9) = ptr( pointGrid, [i+1, j+2] )
             ptpArrayH(h+10) = ptr( xPointArray, pn+1)
             ptpArrayH(h+11) = ptr( xPointArray, pn+1 )
             ptpArrayH(h+12) = ptr( pointGrid, [i+2, j+2] )
          end if

          ! V1, V5, etc.
          ptpArrayV(v+1) = ptr( pointGrid, [i, j+1] )
          ptpArrayV(v+2) = ptr( yPointArray, q+1 )
          ! V2, V6, etc.
          ptpArrayV(v+3) = ptr( yPointArray, q+1 )
          ptpArrayV(v+4) = ptr( pointGrid, [i, j+2] )
          ! V3, V7, etc.
          ptpArrayV(v+5) = ptr( pointGrid, [i+1, j] )
          ptpArrayV(v+6) = ptr( yPointArray, q+2 )
          ! V4, V8, etc.
          ptpArrayV(v+7) = ptr( yPointArray, q+2 )
          ptpArrayV(v+8) = ptr( pointGrid, [i+1, j+1] )

          ! 1st-3rd free lines
          ptpArrayF(f+1) = ptr( pointGrid, [i, j] )
          ptpArrayF(f+2) = ptr( yPointArray, q+2 )
          ptpArrayF(f+3) = ptr( yPointArray, q+2 )
          ptpArrayF(f+4) = ptr( xPointArray, p+2 )
          ptpArrayF(f+5) = ptr( pointGrid, [i, j] )
          ptpArrayF(f+6) = ptr( xPointArray, p+2 )
          ! 4th-6th free lines
          ptpArrayF(f+7) = ptr( xPointArray, p+1 )
          ptpArrayF(f+8) = ptr( yPointArray, q+2 )
          ptpArrayF(f+9) = ptr( xPointArray, p+1 )
          ptpArrayF(f+10) = ptr( pointGrid, [i+2, j+1] )
          ptpArrayF(f+11) = ptr( yPointArray, q+2 )
          ptpArrayF(f+12) = ptr( pointGrid, [i+2, j+1] ) 
          ! 7th-9th free lines
          ptpArrayF(f+13) = ptr( xPointArray, p+2 )
          ptpArrayF(f+14) = ptr( yPointArray, q+1 )
          ptpArrayF(f+15) = ptr( xPointArray, p+2 )
          ptpArrayF(f+16) = ptr( pointGrid, [i+1, j+2] )
          ptpArrayF(f+17) = ptr( yPointArray, q+1 )
          ptpArrayF(f+18) = ptr( pointGrid, [i+1, j+2] )
          ! 10th-12th free lines
          ptpArrayF(f+19) = ptr( pointGrid, [i+1, j+1] )
          ptpArrayF(f+20) = ptr( yPointArray, qe+1 )
          ptpArrayF(f+21) = ptr( yPointArray, qe+1 )
          ptpArrayF(f+22) = ptr( xPointArray, pn+1 )
          ptpArrayF(f+23) = ptr( pointGrid, [i+1, j+1] )
          ptpArrayF(f+24) = ptr( xPointArray, pn+1 )
       end do
       
       ! at the end of each row of cells are two more V lines
       ptpArrayV(v+9) = ptr( pointGrid, [i+2, j+1] )
       ptpArrayV(v+10) = ptr( yPointArray, qe+1 )
       ptpArrayV(v+11) = ptr( yPointArray, qe+1 )
       ptpArrayV(v+12) = ptr( pointGrid, [i+2, j+2] )
    end do

    ! complete?
    do i = 1, size(ptpArrayH)
       call addEvent( ptpArrayH(i)%sameAs(nullPtpPtr), FATAL, &
            'Element '//str(i)//' of ptpArrayH is still empty.', log )
    end do
    do i = 1, size(ptpArrayV)
       call addEvent( ptpArrayV(i)%sameAs(nullPtpPtr), FATAL, &
            'Element '//str(i)//' of ptpArrayV is still empty.', log )
    end do
    do i = 1, size(ptpArrayF)
       call addEvent( ptpArrayF(i)%sameAs(nullPtpPtr), FATAL, &
            'Element '//str(i)//' of ptpArrayF is still empty.', log )
    end do
       
    call createSimplexArray( xLineArray, 1, ptpArrayH, xPointArray, log )
    call createSimplexArray( yLineArray, 1, ptpArrayV, yPointArray, log )
    call createSimplexArray( freeLineArray, 1, ptpArrayF, log=log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    deallocate( ptpArrayH, ptpArrayV, ptpArrayF, stat=stat )
    call addEvent( stat/=0, WARNING, 'Problem deallocating ptpArrayH, &
         &ptpArrayV, ptpArrayF.  STAT='//int2str(stat), log )

    call endSub( log )
  end subroutine setup_TestGrid1_unstructuredLines


  subroutine setup_TestGrid1_unstructuredTriangles( resolution, log )
    type(IntVectorType), intent(in) :: resolution
    class(LogType), intent(inout) :: log
    type(PolytopePointerType), dimension(:), allocatable :: ptpArray
    integer :: mq, nq, iq, jq, i, j, p, q, h, v, f, t, hn, ve, stat
    type(PolytopePointerType) :: nullPtpPtr
    call beginSub( MOD_NAME, 'setup_TestGrid1_unstructuredTriangles', log )

    mq = resolution%getValue(1)/2
    nq = resolution%getValue(2)/2

    ! bounding lines are to be defined in triples
    allocate( ptpArray(3*16*mq*nq), stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating ptpArray(3*16*&
         &mq*nq).  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! loop over cell quadruples defining boundaries
    do jq = 1, nq
       do iq = 1, mq
          ! reference counter
          t = 3*getIndex( 16, iq, jq, mq, nq )

          ! lower left hand address of this cell quadruple
          i = 2*(iq - 1) + 1
          j = 2*(jq - 1) + 1
          ! reference index for each type of line
          h = getIndex( 4, iq, jq, mq, nq, nExtraNorth=2 )
          v = getIndex( 4, iq, jq, mq, nq, nExtraEast=2 )
          hn = getIndex( 4, iq, jq+1, mq, nq, nExtraNorth=2 )
          ve = getIndex( 4, iq+1, jq, mq, nq, nExtraEast=2 )
          f = getIndex( 12, iq, jq, mq, nq )

          ! 1st-4th triangles
          ptpArray(t+1) = ptr( xLineGrid, [i, j] )
          ptpArray(t+2) = ptr( yLineArray, v+3 )
          ptpArray(t+3) = ptr( freeLineArray, f+1 )
          ptpArray(t+4) = ptr( freeLineArray, f+1 )
          ptpArray(t+5) = ptr( freeLineArray, f+2 )
          ptpArray(t+6) = ptr( freeLineArray, f+3 )
          ptpArray(t+7) = ptr( freeLineArray, f+3 )
          ptpArray(t+8) = ptr( xLineArray, h+3 )
          ptpArray(t+9) = ptr( yLineGrid, [i, j] )
          ptpArray(t+10) = ptr( freeLineArray, f+2 )
          ptpArray(t+11) = ptr( yLineArray, v+4 )
          ptpArray(t+12) = ptr( xLineArray, h+4 )
          
          ! 5th-8th triangles
          ptpArray(t+13) = ptr( xLineArray, h+1 )
          ptpArray(t+14) = ptr( freeLineArray, f+4 )
          ptpArray(t+15) = ptr( yLineArray, v+3 )
          ptpArray(t+16) = ptr( xLineArray, h+2 )
          ptpArray(t+17) = ptr( yLineGrid, [i+2, j] )
          ptpArray(t+18) = ptr( freeLineArray, f+5 )
          ptpArray(t+19) = ptr( freeLineArray, f+4 )
          ptpArray(t+20) = ptr( freeLineArray, f+5 )
          ptpArray(t+21) = ptr( freeLineArray, f+6 )
          ptpArray(t+22) = ptr( freeLineArray, f+6 )
          ptpArray(t+23) = ptr( xLineGrid, [i+1, j+1] )
          ptpArray(t+24) = ptr( yLineArray, v+4 )
          
          ! 9th-12th triangles
          ptpArray(t+25) = ptr( xLineArray, h+3 )
          ptpArray(t+26) = ptr( freeLineArray, f+7 )
          ptpArray(t+27) = ptr( yLineArray, v+1 )
          ptpArray(t+28) = ptr( xLineArray, h+4 )
          ptpArray(t+29) = ptr( yLineGrid, [i+1, j+1] )
          ptpArray(t+30) = ptr( freeLineArray, f+8 )
          ptpArray(t+31) = ptr( freeLineArray, f+7 )
          ptpArray(t+32) = ptr( freeLineArray, f+8 )
          ptpArray(t+33) = ptr( freeLineArray, f+9 )
          ptpArray(t+34) = ptr( freeLineArray, f+9 )
          ptpArray(t+35) = ptr( xLineGrid, [i, j+2] )
          ptpArray(t+36) = ptr( yLineArray, v+2 )
          
          ! 13th-16th triangles
          ptpArray(t+37) = ptr( xLineGrid, [i+1, j+1] )
          ptpArray(t+38) = ptr( yLineArray, ve+1 )
          ptpArray(t+39) = ptr( freeLineArray, f+10 )
          ptpArray(t+40) = ptr( freeLineArray, f+10 )
          ptpArray(t+41) = ptr( freeLineArray, f+11 )
          ptpArray(t+42) = ptr( freeLineArray, f+12 )
          ptpArray(t+43) = ptr( freeLineArray, f+12 )
          ptpArray(t+44) = ptr( xLineArray, hn+1 )
          ptpArray(t+45) = ptr( yLineGrid, [i+1, j+1] )
          ptpArray(t+46) = ptr( freeLineArray, f+11 )
          ptpArray(t+47) = ptr( yLineArray, ve+2 )
          ptpArray(t+48) = ptr( xLineArray, hn+2 )
       end do
    end do
    
    ! complete?
    do i = 1, size(ptpArray)
       call addEvent( ptpArray(i)%sameAs(nullPtpPtr), FATAL, &
            'Element '//str(i)//' of ptpArray is still empty.', log )
    end do

    call createSimplexArray( triangleArray, 2, ptpArray, &
         freeLineArray, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    deallocate( ptpArray, stat=stat )
    call addEvent( stat/=0, WARNING, 'Problem deallocating &
         &ptpArray.  STAT='//int2str(stat), log )

    call endSub( log )
  end subroutine setup_TestGrid1_unstructuredTriangles

  
  recursive function getIndex( nPolytopes, iq, jq, mq, nq, &
       nExtraEast, nExtraNorth )
    ! return a reference index of some polytope in cell quadruple (iq1, jq1)
    ! when there are mq quadruples in the x-direction and nPolytopes of
    ! that type of polytope in each quadruple.
    integer, intent(in) :: nPolytopes, iq, jq, mq, nq
    integer, intent(in), optional :: nExtraEast, nExtraNorth
    integer :: getIndex

    ! if have passed the end of a column or a row, bump down the address,
    ! recall the function, and add on the number of polytopes in that quad
    if ( iq > mq ) then
       getIndex = getIndex( nPolytopes, iq-1, jq, mq, nq, nExtraEast, &
            nExtraNorth ) + nPolytopes
       return
    end if
    if ( jq > nq ) then
       getIndex = getIndex( nPolytopes, iq, jq-1, mq, nq, nExtraEast, &
            nExtraNorth ) + nPolytopes
       return
    end if
       
    getIndex = nPolytopes*( (jq - 1)*mq + iq - 1 )
    
    ! Extra polytopes may exist at the end of each row of cells
    ! (nExtraEast) or at the top of each column (nExtraNorth).
    if ( present(nExtraEast) ) getIndex = getIndex + nExtraEast*(jq - 1)
    if ( present(nExtraNorth) ) then
       if ( jq == nq ) getIndex = getIndex + nExtraNorth*(iq - 1)
    end if
  end function getIndex

  
  !-------------------------------------------------------------------
  !- self tests
  !-------------------------------------------------------------------


  subroutine setUp
    call log%init( MOD_NAME, 'setup' )
    
    call setup_TestGrid1( vector([0._FLOAT, 0._FLOAT]), vector([&
         18._FLOAT, 12._FLOAT]), vector([6, 4]), log )
  end subroutine setUp

  subroutine tearDown
    call log%reinit( MOD_NAME, 'teardown' )
    
    call tearDown_TestGrid1( log )
    
    call log%report()
    call log%deinit()
  end subroutine tearDown

  
  subroutine testTestGrid1
    type(RealVectorType) :: r
    if ( log%test(MOD_NAME, 'testTestGrid1', FATAL) ) return

    ! --- rectangle spot check ---
    
    call rectangleGrid%getCentroid( rectangleGrid%convertToIndex(vector([&
         4, 3])), r, log )
    call assertEqual( [10.5_FLOAT, 7.5_FLOAT], r%getValues(), 'Centroid &
         &coordinates for rectangle (4, 3) are wrong.' )

    ! --- triangle spot checks ---

    ! make sure all types of bounding line are represented
    
    call triangleArray%getCentroid( 4, r, log )
    if ( log%test(FATAL) ) return
    call assertEqual( [2.5_FLOAT, 2.5_FLOAT], r%getValues(), 'Centroid &
         &coordinates for triangle (4) are wrong.' )

    call triangleArray%getCentroid( 6, r, log )
    if ( log%test(FATAL) ) return
    call assertEqual( [5.5_FLOAT, 1._FLOAT], r%getValues(), 'Centroid &
         &coordinates for triangle (6) are wrong.' )

    call triangleArray%getCentroid( 15, r, log )
    if ( log%test(FATAL) ) return
    call assertEqual( [3.5_FLOAT, 5._FLOAT], r%getValues(), 'Centroid &
         &coordinates for triangle (15) are wrong.' )
    
    call triangleArray%getCentroid( 27, r, log )
    if ( log%test(FATAL) ) return
    call assertEqual( [7.5_FLOAT, 4.5_FLOAT], r%getValues(), 'Centroid &
         &coordinates for triangle (27) are wrong.' )

    call triangleArray%getCentroid( 49, r, log )
    if ( log%test(FATAL) ) return
    call assertEqual( [2._FLOAT, 6.5_FLOAT], r%getValues(), 'Centroid &
         &coordinates for triangle (49) are wrong.' )
    
    ! --- last element spot checks ---
    
    call xPointArray%getCentroid( 15, r, log )
    if ( log%test(FATAL) ) return
    call assertEqual( [16.5_FLOAT, 12._FLOAT], r%getValues(), 'Centroid &
         &coordinates for xPoint (15) are wrong.' )
    
    call yPointArray%getCentroid( 14, r, log )
    if ( log%test(FATAL) ) return
    call assertEqual( [18._FLOAT, 10.5_FLOAT], r%getValues(), 'Centroid &
         &coordinates for yPoint (14) are wrong.' )

    call xLineArray%getCentroid( 30, r, log )
    if ( log%test(FATAL) ) return
    call assertEqual( [17.25_FLOAT, 12._FLOAT], r%getValues(), 'Centroid &
         &coordinates for xLine (30) are wrong.' )

    call yLineArray%getCentroid( 28, r, log )
    if ( log%test(FATAL) ) return
    call assertEqual( [18._FLOAT, 11.25_FLOAT], r%getValues(), 'Centroid &
         &coordinates for yLine (28) are wrong.' )
    
    call freeLineArray%getCentroid( 72, r, log )
    if ( log%test(FATAL) ) return
    call assertEqual( [15.75_FLOAT, 10.5_FLOAT], r%getValues(), 'Centroid &
         &coordinates for freeLine (72) are wrong.' )

    call triangleArray%getCentroid( 96, r, log )
    if ( log%test(FATAL) ) return
    call assertEqual( [17.5_FLOAT, 11.5_FLOAT], r%getValues(), 'Centroid &
         &coordinates for triangle (96) are wrong.' )
    
  end subroutine testTestGrid1
  

end module TestGrid1
