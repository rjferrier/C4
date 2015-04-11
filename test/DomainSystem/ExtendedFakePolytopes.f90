module ExtendedFakePolytopes
  
  use LogModule
  use Global
  use FiniteVolumeModule
  use FiniteVolume_Stubs
  
  implicit none
  private
  public :: extendedTriangleArray
  
  character(*), parameter :: MOD_NAME = 'ExtendedFakePolytopes'
  
  type, extends(PolytopeArrayStub) :: FakeTriangleArrayDecorator
     class(PolytopeArrayInterface), pointer :: pa
     class(CellArrayInterface), allocatable :: cellExtension
   contains
     procedure :: init
     procedure :: deinit
     procedure :: collectSubPolytopes
     procedure :: getCentroid
     procedure :: setExtension_Cells
     procedure :: getArrayExtension_Cells => getArrayExtension_Cells
  end type FakeTriangleArrayDecorator

  type(FakeTriangleArrayDecorator) :: extendedTriangleArray

  
  
contains
  
  !-------------------------------------------------------------------
  !- FakeTriangleArrayDecorator methods
  !-------------------------------------------------------------------
  
  ! -- new methods --
  
  subroutine init( obj, polytopeArray, log )
    class(FakeTriangleArrayDecorator), intent(inout) :: obj
    class(PolytopeArrayInterface), target, intent(in) :: polytopeArray
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init', log )

    obj%pa => polytopeArray
    
    call obj%initPolytopeArray( polytopeArray%getNumPolytopes(), &
         polytopeArray%getPolytopeDim(), log )
    
    call endSub( log )
  end subroutine init

  
  subroutine deinit( obj, log )
    class(FakeTriangleArrayDecorator), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit', log )

    call obj%deinitPolytopeArray( log )
    
    nullify(obj%pa)
    if ( allocated(obj%cellExtension) ) then
       call destroy( obj%cellExtension, log )
    end if
    
    call endSub( log )
  end subroutine deinit


  subroutine setExtension_Cells( obj, cellArray, log )
    class(FakeTriangleArrayDecorator), intent(inout) :: obj
    class(CellArrayInterface), allocatable, intent(inout) :: cellArray
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'setExtension_Cells', log )

    call checkInjection( allocated(cellArray), &
         allocated(obj%cellExtension), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    call move_alloc( cellArray, obj%cellExtension )
        
    call endSub( log )    
  end subroutine setExtension_Cells


  pure subroutine getArrayExtension_Cells( obj, cellArray, log )
    class(FakeTriangleArrayDecorator), intent(inout), target :: obj
    class(CellArrayInterface), pointer, intent(inout) :: cellArray
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getArrayExtension_Cells', &
         log )
    
    if ( allocated(obj%cellExtension) ) then
       cellArray => obj%cellExtension
    end if
    
    call endSub( log )    
  end subroutine getArrayExtension_Cells

  
  ! -- undecorated (delegated) methods --
  
  pure recursive subroutine collectSubPolytopes( obj, &
       index, pps, log, depth )
    class(FakeTriangleArrayDecorator), target, intent(inout) :: obj
    integer, intent(in) :: index
    class(PolytopePointerSetType), intent(inout) :: pps
    class(LogType), intent(inout), optional :: log
    integer, intent(in), optional :: depth
    integer, dimension(2) :: indexPair
    type(PolytopePointerType), dimension(3) :: pp
    call beginSub( MOD_NAME, 'collectSubPolytopes', &
         log )

    call obj%pa%collectSubPolytopes( index, pps, log, depth )
        
    call endSub( log )
  end subroutine collectSubPolytopes
  
  
  pure subroutine getCentroid( obj, index, coords, log )
    class(FakeTriangleArrayDecorator), intent(in) :: obj
    integer, intent(in) :: index
    type(RealVectorType), intent(out)  :: coords
    class(LogType), intent(inout), optional :: log
    type(PolytopePointerSetType) :: pps
    call beginSub( MOD_NAME, 'getCentroid', log )

    call obj%pa%getCentroid( index, coords, log )
    
    call endSub( log )
  end subroutine getCentroid

  
end module ExtendedFakePolytopes
