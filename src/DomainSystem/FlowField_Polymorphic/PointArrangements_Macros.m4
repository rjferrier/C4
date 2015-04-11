

MACRO({TYPEDEF_SUBPOINTARRANGEMENT}, {{
    type, extends(PointArrangementInterface), public :: $1PointArrangementType
     private
     class(Polytope$1Interface), pointer :: polytope$1
     integer :: polytopeDim, degree
     $2     
   contains
     procedure :: init => init_$1
     procedure :: deinit => deinit_$1
     procedure :: createPositionsMatrix => createPositionsMatrix_$1
     procedure :: convertToPolytopeValues => convertToPolytopeValues_$1
     procedure :: getPositions => getPositions_$1
     procedure :: getNumPointsPerPolytope => getNumPointsPerPolytope_$1
     procedure :: createPointPattern => createPointPattern_$1
     procedure :: createGaussianWeights => createGaussianWeights_$1
     $3
  end type $1PointArrangementType
  }})


  MACRO({METHODDEFS_SUBPOINTARRANGEMENT}, {{
  subroutine create$1PointArrangement( pa, polytope$1, degree, log )
    class(PointArrangementInterface), allocatable, intent(out) :: pa
    class(Polytope$1Interface), target, intent(in) :: polytope$1
    integer, intent(in) :: degree
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'create$1PointArrangement', log )
    
    ! allocate
    if ( allocated(pa) ) then
       call addEvent( FATAL, 'pa already allocated.', log )
    else
       allocate( $1PointArrangementType :: pa, stat=allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &$1PointArrangementType :: pa.  STAT='&
	    //int2str(allocStat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise
    select type ( pa )
    class is ($1PointArrangementType)
       call pa%init( polytope$1, degree, log )
    end select
    call endSub( log )
  end subroutine create$1PointArrangement

  
  subroutine init_$1( obj, polytope$1, degree, log )
    class($1PointArrangementType), intent(out) :: obj
    class(Polytope$1Interface), target, intent(in) :: polytope$1
    integer, intent(in) :: degree
    integer :: i
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_$1', log )

    obj%polytope$1 => polytope$1
    obj%degree = degree
    obj%polytopeDim = polytope$1%getPolytopeDim()
    
    call obj%createPointPattern( obj%pointPattern, log )
    call obj%createGaussianWeights( obj%gaussianWeights, log )

    call endSub( log )
  end subroutine init_$1

  
  pure subroutine deinit_$1( obj, log )
    class($1PointArrangementType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'deinit_$1', log )

    nullify(obj%polytope$1)
    deallocate( obj%pointPattern, obj%gaussianWeights, stat=stat )
    call addEvent( stat/=0, WARNING, 'Problem deallocating obj%&
         &pointPattern, obj%gaussianWeights.  STAT='//str(stat), log )
    obj%polytopeDim = 0
    obj%degree = 0
    
    call endSub( log )
  end subroutine deinit_$1

  
  subroutine createPositionsMatrix_$1( obj, positions, log )
    class($1PointArrangementType), intent(in) :: obj
    type(RealVectorType), dimension(:, :), allocatable, intent(inout) :: &
         positions
    class(LogType), intent(inout), optional :: log
    integer :: m, n, i, stat
    call beginSub( MOD_NAME, 'createPositionsMatrix_$1', log )

    m = obj%getNumPointsPerPolytope()
    n = obj%polytope$1%getNumPolytopes()
    allocate( positions(m, n), stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating positions(m, n).  &
         &STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    do i = 1, n
       call obj%getPositions( positions(:, i), i, log=log )
       if ( checkSub(FATAL, log) ) then
          call addEvent( ADVICE, 'at polytope index = '//str(i), &
               log )
          call endSub( log )
          return
       end if
    end do
    
    call endSub( log )
  end subroutine createPositionsMatrix_$1
  }})

  
