module PolytopeCollections_DomainDim_2

  use Global
  use LogModule

  implicit none

  character(*), parameter, private  :: MOD_NAME = &
       'PolytopeCollections_DomainDim_2'

  
!!$  type :: GridSuperPolytopesManager_NDPartsType
!!$     type(PolytopePointerSetType), dimension(:, :), allocatable :: &
!!$          modifiedSuperPolytopes
!!$   contains
!!$  end type GridSuperPolytopesManager_NDPartsType
  
  
  type :: ModifiedGridInfoType
     integer, dimension(:, :), allocatable :: iCut
   contains
     procedure :: init => initModifiedGridInfo
     procedure :: deinit => deinitModifiedGridInfo
     procedure :: addIndex => addIndex_ModifiedGridInfo
     procedure :: getIndex => getIndex_ModifiedGridInfo
     
  end type ModifiedGridInfoType


contains

  
  !-------------------------------------------------------------------
  !- ModifiedGridInfo methods
  !-------------------------------------------------------------------

  subroutine initModifiedGridInfo( obj, gridShape, log )
    class(ModifiedGridInfoType), intent(out) :: obj
    class(SizeVectorType) :: gridShape
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'initModifiedGridInfo', log )
    
    call gridShape%allocGrid( obj%iCut, allocStat )
    call addEvent( allocStat/=0, FATAL, 'Problem allocating &
         &elements of obj%iCut.  STAT='//int2str(allocStat), log )
    if ( checkSub(FATAL, log) ) then
      call endSub( log )
      return
    end if

    obj%iCut = 0

    call endSub( log )
  end subroutine initModifiedGridInfo
    

  subroutine deinitModifiedGridInfo( obj, log )
    class(ModifiedGridInfoType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: deallocStat
    call beginSub( MOD_NAME, 'deinitModifiedGridInfo', &
         log )
    
    deallocate( obj%iCut, stat=deallocStat )
    call addEvent( deallocStat/=0, &
         WARNING, 'Problem deallocating elements of obj%iCut.  &
         &STAT='//int2str(deallocStat), log )
    
    call endSub( log )
  end subroutine deinitModifiedGridInfo


  subroutine addIndex_ModifiedGridInfo( obj, gridAddress, index )
    class(ModifiedGridInfoType), intent(inout) :: obj
    class(IntVectorType), intent(in) :: gridAddress
    integer, intent(in) :: index
    call gridAddress%setGridElement( obj%iCut, index )
    
  end subroutine addIndex_ModifiedGridInfo


  elemental function getIndex_ModifiedGridInfo( obj, &
       gridAddress ) result ( index )
    class(ModifiedGridInfoType), intent(in) :: obj
    class(IntVectorType), intent(in) :: gridAddress
    integer :: index
    index = gridAddress%getGridElement( obj%iCut )
  end function getIndex_ModifiedGridInfo
  

end module PolytopeCollections_DomainDim_2
