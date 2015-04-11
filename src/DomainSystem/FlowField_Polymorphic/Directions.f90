module DirectionsModule
  
  use LogModule
  use Global
  use FlowFieldModule
  
  implicit none
  private
  public :: createCartesianDirection, createObliqueDirection
  
  character(*), parameter :: MOD_NAME = 'DirectionsModule'

  
  type, extends(DirectionInterface), public :: CartesianDirectionType
     private
     integer :: dimIndex
   contains
     procedure :: init => init_CartesianDirection
     procedure :: dotProduct => &
          dotProduct_CartesianDirection
     procedure :: deinit => deinit_CartesianDirection
     procedure :: clone_single => clone_single_CartesianDirection
     procedure :: clone_multi => clone_multi_CartesianDirection
     procedure :: convert => convert_CartesianDirection
     procedure :: sameAs => sameAs_CartesianDirection
  end type CartesianDirectionType

  
  type, extends(DirectionInterface), public :: ObliqueDirectionType
     private
     type(RealVectorType) :: dirVector
   contains
     procedure :: init => init_ObliqueDirection
     procedure :: dotProduct => &
          dotProduct_ObliqueDirection
     procedure :: deinit => deinit_ObliqueDirection
     procedure :: clone_single => clone_single_ObliqueDirection
     procedure :: clone_multi => clone_multi_ObliqueDirection
     procedure :: convert => convert_ObliqueDirection
     procedure :: sameAs => sameAs_ObliqueDirection
  end type ObliqueDirectionType
  
  
contains
  
  
  !-------------------------------------------------------------------
  !- CartesianDirection methods
  !-------------------------------------------------------------------

  subroutine createCartesianDirection( cd, dimIndex, log )
    class(DirectionInterface), allocatable, intent(inout) :: cd
    integer, intent(in) :: dimIndex
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'createCartesianDirection', log )
    
    ! allocate
    if ( allocated(cd) ) then
       call addEvent( FATAL, 'cd already allocated.', log )
    else
       allocate( CartesianDirectionType :: cd, stat=allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &CartesianDirectionType :: cd.  STAT='&
	    //int2str(allocStat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise
    select type ( cd )
    class is (CartesianDirectionType)
       call cd%init( dimIndex, log )
    end select
    call endSub( log )
  end subroutine createCartesianDirection
  

  subroutine init_CartesianDirection( obj, dimIndex, log )
    class(CartesianDirectionType), intent(out) :: obj
    integer, intent(in) :: dimIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_CartesianDirection', log )

    obj%dimIndex = dimIndex
    
    call endSub( log )
  end subroutine init_CartesianDirection
  
  
  pure function dotProduct_CartesianDirection( obj, &
       singlePointRowComponents )
    class(CartesianDirectionType), intent(in) :: obj
    real(FLOAT), dimension(:, :), intent(in) :: &
         singlePointRowComponents
    real(FLOAT), dimension(size(singlePointRowComponents, 2)) :: &
         dotProduct_CartesianDirection
    
    ! just select the dimIndex'th component
    dotProduct_CartesianDirection = &
         singlePointRowComponents(obj%dimIndex, :)
    
  end function dotProduct_CartesianDirection

  
  pure subroutine deinit_CartesianDirection( obj, log )
    class(CartesianDirectionType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_CartesianDirection', log )
    
    obj%dimIndex = 0

    call endSub( log )
  end subroutine deinit_CartesianDirection

  
  pure subroutine clone_single_CartesianDirection( obj, tgt, log )
    class(CartesianDirectionType), intent(in) :: obj
    class(DirectionInterface), allocatable, intent(inout) :: tgt
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'clone_single_CartesianDirection', log )

    allocate( CartesianDirectionType :: tgt, stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &CartesianDirectionType :: tgt.  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    select type ( tgt )
    type is ( CartesianDirectionType )
       tgt%dimIndex = obj%dimIndex
    end select

    call endSub( log )
  end subroutine clone_single_CartesianDirection

  
  pure subroutine clone_multi_CartesianDirection( obj, directions, &
       nElements, log )
    class(CartesianDirectionType), intent(in) :: obj
    class(DirectionInterface), dimension(:), allocatable, intent(&
         inout) :: directions
    integer, intent(in) :: nElements
    class(LogType), intent(inout), optional :: log
    integer :: allocStat, i
    call beginSub( MOD_NAME, 'clone_multi_CartesianDirection', log )

    allocate( CartesianDirectionType :: directions(nElements), stat=&
         allocStat )
    call addEvent( allocStat/=0, FATAL, 'Problem allocating &
         &CartesianDirectionType :: directions(nElements).  STAT='//&
         int2str(allocStat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    select type ( directions )
    type is ( CartesianDirectionType )
       forall ( i = 1:nElements )
          directions(i)%dimIndex = obj%dimIndex
       end forall
    end select

    call endSub( log )
  end subroutine clone_multi_CartesianDirection


  pure function convert_CartesianDirection( obj )
    class(CartesianDirectionType), intent(in) :: obj
    type(RealVectorType) :: convert_CartesianDirection
    convert_CartesianDirection = 0._FLOAT
    call convert_CartesianDirection%setElement(obj%dimIndex, &
         1._FLOAT)
  end function convert_CartesianDirection


  pure function sameAs_CartesianDirection( obj, direction )
    class(CartesianDirectionType), intent(in) :: obj
    class(DirectionInterface), intent(in) :: direction
    logical :: sameAs_CartesianDirection
    select type ( direction )
    type is (CartesianDirectionType)
       sameAs_CartesianDirection = (obj%dimIndex == direction%dimIndex)
    class default
       sameAs_CartesianDirection = .false. 
    end select
  end function sameAs_CartesianDirection

  
  !-------------------------------------------------------------------
  !- ObliqueDirection methods
  !-------------------------------------------------------------------
  
  subroutine createObliqueDirection( od, dirVector, log )
    class(DirectionInterface), allocatable, intent(inout) :: od
    type(RealVectorType), intent(in) :: dirVector
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'createObliqueDirection', log )
    
    ! allocate
    if ( allocated(od) ) then
       call addEvent( FATAL, 'od already allocated.', log )
    else
       allocate( ObliqueDirectionType :: od, stat=allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &ObliqueDirectionType :: od.  STAT='&
	    //int2str(allocStat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise
    select type ( od )
    class is (ObliqueDirectionType)
       call od%init( dirVector, log )
    end select
    call endSub( log )
  end subroutine createObliqueDirection


  subroutine init_ObliqueDirection( obj, dirVector, log )
    class(ObliqueDirectionType), intent(out) :: obj
    type(RealVectorType), intent(in) :: dirVector
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_ObliqueDirection', log )
    
    obj%dirVector = dirVector
    call obj%dirVector%normalise()

    call endSub( log )
  end subroutine init_ObliqueDirection

  
  pure function dotProduct_ObliqueDirection( obj, &
       singlePointRowComponents )
    class(ObliqueDirectionType), intent(in) :: obj
    real(FLOAT), dimension(:, :), intent(in) :: &
         singlePointRowComponents
    real(FLOAT), dimension(size(singlePointRowComponents, 2)) :: &
         dotProduct_ObliqueDirection
    integer :: i, n
    real(FLOAT), dimension(NDIM) :: a, b, c
    
    forall ( i = 1:size(dotProduct_ObliqueDirection) )
       dotProduct_ObliqueDirection(i) = dot_product( obj%dirVector%&
            getValues(), singlePointRowComponents(:, i) )
    end forall
  end function dotProduct_ObliqueDirection

  
  pure subroutine deinit_ObliqueDirection( obj, log )
    class(ObliqueDirectionType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_ObliqueDirection', log )
    
    obj%dirVector = 0._FLOAT

    call endSub( log )
  end subroutine deinit_ObliqueDirection

  
  pure subroutine clone_single_ObliqueDirection( obj, tgt, log )
    class(ObliqueDirectionType), intent(in) :: obj
    class(DirectionInterface), allocatable, intent(inout) :: tgt
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'clone_single_ObliqueDirection', log )

    allocate( ObliqueDirectionType :: tgt, stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &ObliqueDirectionType :: tgt.  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    select type ( tgt )
    type is ( ObliqueDirectionType )
       tgt%dirVector = obj%dirVector
    end select

    call endSub( log )
  end subroutine clone_single_ObliqueDirection

  
  pure subroutine clone_multi_ObliqueDirection( obj, directions, &
       nElements, log )
    class(ObliqueDirectionType), intent(in) :: obj
    class(DirectionInterface), dimension(:), allocatable, intent(&
         inout) :: directions
    integer, intent(in) :: nElements
    class(LogType), intent(inout), optional :: log
    integer :: allocStat, i
    call beginSub( MOD_NAME, 'clone_multi_ObliqueDirection', log )

    allocate( ObliqueDirectionType :: directions(nElements), stat=&
         allocStat )
    call addEvent( allocStat/=0, FATAL, 'Problem allocating &
         &ObliqueDirectionType :: directions(nElements).  STAT='//&
         int2str(allocStat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    select type ( directions )
    type is ( ObliqueDirectionType )
       forall ( i = 1:nElements )
          directions(i)%dirVector = obj%dirVector
       end forall
    end select

    call endSub( log )
  end subroutine clone_multi_ObliqueDirection


  pure function convert_ObliqueDirection( obj )
    class(ObliqueDirectionType), intent(in) :: obj
    type(RealVectorType) :: convert_ObliqueDirection
    convert_ObliqueDirection = obj%dirVector
  end function convert_ObliqueDirection  
  

  pure function sameAs_ObliqueDirection( obj, direction )
    class(ObliqueDirectionType), intent(in) :: obj
    class(DirectionInterface), intent(in) :: direction
    logical :: sameAs_ObliqueDirection
    select type ( direction )
    type is (ObliqueDirectionType)
       sameAs_ObliqueDirection = all(obj%dirVector == direction%dirVector)
    class default
       sameAs_ObliqueDirection = .false. 
    end select
  end function sameAs_ObliqueDirection

  
end module DirectionsModule
