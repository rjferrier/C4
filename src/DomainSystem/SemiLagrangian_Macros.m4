module SemiLagrangian_Macros

  USE_MACROS({FlowField})
     
     
  !-------------------------------------------------------------------
  !- PointMomentSources
  !------------------------------------------------------------------
  
  MACRO({TYPEDEF_POINTMOMENTFIELD}, {{
  ! a set of MomentFields sharing the same FlowVariable but different
  ! spatial derivatives could in fact be lumped into a single FlowField
  ! object, making flow variable matching more efficient.  This
  ! refactoring is left for a future iteration.
  type, extends($1MomentFieldInterface), abstract, public :: Point$1MomentFieldInterface
     private
   contains
     EXPAND({SIGNATURELIST_MOMENTFIELD($1, {Point})})
     EXPAND({METHODLIST_SUBMOMENTSOURCE($1, {MomentField}, {Point})})
     generic :: setField => setField_uniform, setField_array
     procedure(p{}ABB($1)mf_c), deferred :: clone
     procedure(p{}ABB($1)mf_gv), deferred :: getValue
     procedure(p{}ABB($1)mf_gvs), deferred :: getValues
  end type Point$1MomentFieldInterface
  }})

  
  MACRO({TYPEDEF_POINTBOUNDARYCONDITION}, {{
  type, extends($1BoundaryConditionInterface), abstract, public :: Point$1BoundaryConditionInterface
     private
   contains
     EXPAND({SIGNATURELIST_BOUNDARYCONDITION($1, {Point})})
     EXPAND({METHODLIST_SUBMOMENTSOURCE($1, {BoundaryCondition}, {Point})})
     procedure(p{}ABB($1)bc_c), deferred :: clone
     procedure(p{}ABB($1)bc_gv), deferred :: getValue
     procedure(p{}ABB($1)bc_gvs), deferred :: getValues
  end type Point$1BoundaryConditionInterface
  }})
     
  
  
  !-------------------------------------------------------------------
  !- PointMomentGroup 
  !-------------------------------------------------------------------

  MACRO({TYPEDEF_SUBPOINTMOMENTGROUP},{{
  type, extends(PointMomentGroupInterface), abstract, public :: PointScalarMomentGroupInterface
     private
     integer :: nPoints
     logical :: exactlyConstrained
   contains
     procedure :: init$1PointMomentGroup
     procedure :: deinit$1PointMomentGroup
     procedure :: getNumPoints => getNumPoints_$1PointMomentGroup
     EXPAND({SIGNATURELIST_SUBPOINTMOMENTGROUP($1)})
  end type PointScalarMomentGroupInterface
  }})

  MACRO({TYPEDEFS_CONTAINER_SUBPOINTMOMENTGROUP}, {{
    EXPAND({TYPEDEFS_CONTAINER($1PointMomentGroup, Interface, List,
    {class($1FlowVariableInterface), pointer :: LWR($1)FlowVariable},
     {procedure :: setFlowVariable => setFlowVariable_$1PointMomentGroupList
     procedure :: getFlowVariable => getFlowVariable_$1PointMomentGroupList
     procedure :: countMoments => countMoments_$1PointMomentGroupList})})
  }})

  
  !-------------------------------------------------------------------
  !- PointMomentPointers
  !-------------------------------------------------------------------
  
  MACRO({TYPEDEF_POINTMOMENTPOINTER}, {{
  type, extends($1MomentPointerInterface), public :: $1PointMomentPointerType
     private
     class($1PointMomentGroupInterface), pointer :: &
          ABB($1PointMomentGroup)
     integer :: pointIndex
   contains
     procedure :: init => init_$1PointMomentPointer
     procedure :: deinit => deinit_$1PointMomentPointer
     procedure :: copy => copy_$1PointMomentPointer
     procedure :: getValue => getValue_$1PointMomentPointer
  end type $1PointMomentPointerType
  }})

  
  !-------------------------------------------------------------------
  !- abstract interfaces
  !-------------------------------------------------------------------
 
  MACRO({SIGNATUREDEFS_POINTMOMENTFIELD},{{
     EXPAND({SIGNATUREDEFS_MOMENTFIELD({$1}, {Point})})

     subroutine p{}ABB($1)mf_c( obj, tgt, log )
       import Point$1MomentFieldInterface, LogType
       class(Point$1MomentFieldInterface), intent(in) :: obj
       class(Point$1MomentFieldInterface), allocatable, &
            intent(out) :: tgt
       class(LogType), intent(inout), optional :: log
     end subroutine p{}ABB($1)mf_c

     ! In this next signature, we could have reduced the argument list to
     ! a PointPointer.  However, we want to be able to quickly deference
     ! the MomentField when building each cell profile's RHS, which means
     ! we don't want to mess around with trivial objects.
     elemental subroutine p{}ABB($1)mf_gv( obj, value, &
          polytopeIndex, pointIndex ARGLINE_TENSOR_COMPONENT($1) )
       import Point$1MomentFieldInterface, FLOAT
       class(Point$1MomentFieldInterface), intent(in) :: obj
       real(FLOAT), intent(out) :: value
       integer, intent(in) :: polytopeIndex, pointIndex
       ARGLIST_TENSOR_COMPONENT($1)
     end subroutine p{}ABB($1)mf_gv

     pure subroutine p{}ABB($1)mf_gvs( obj, values, &
          polytopeIndex ARGLINE_TENSOR_COMPONENT($1) )
       import Point$1MomentFieldInterface, FLOAT
       class(Point$1MomentFieldInterface), intent(in) :: obj
       real(FLOAT), dimension(:), intent(out) :: values
       integer, intent(in) :: polytopeIndex
       ARGLIST_TENSOR_COMPONENT($1)
     end subroutine p{}ABB($1)mf_gvs
     }})
     
  MACRO({SIGNATUREDEFS_POINTBOUNDARYCONDITION},{{
     EXPAND({SIGNATUREDEFS_BOUNDARYCONDITION({$1}, {Point})})

     subroutine p{}ABB($1)bc_c( obj, tgt, log )
       import Point$1BoundaryConditionInterface, LogType
       class(Point$1BoundaryConditionInterface), intent(in) :: obj
       class(Point$1BoundaryConditionInterface), allocatable, &
            intent(out) :: tgt
       class(LogType), intent(inout), optional :: log
     end subroutine p{}ABB($1)bc_c
       
     elemental subroutine p{}ABB($1)bc_gv( obj, value ARGLINE_TENSOR_COMPONENT($1) )
       import Point$1BoundaryConditionInterface, FLOAT
       class(Point$1BoundaryConditionInterface), intent(in) :: obj
       real(FLOAT), intent(out) :: value
       ARGLIST_TENSOR_COMPONENT($1)
     end subroutine p{}ABB($1)bc_gv
       
     pure subroutine p{}ABB($1)bc_gvs( obj, values ARGLINE_TENSOR_COMPONENT($1) )
       import Point$1BoundaryConditionInterface, FLOAT
       class(Point$1BoundaryConditionInterface), intent(in) :: obj
       real(FLOAT), dimension(:), intent(out) :: values
       ARGLIST_TENSOR_COMPONENT($1)
     end subroutine p{}ABB($1)bc_gvs
     }})

     
  MACRO({SIGNATUREDEF_DEPARTUREPOINTCOLLECTION_APPENDMOMENTS},{{
!!$     ! this is a generic signature for appending moments to a list based
!!$     ! on some target flowVariable.
!!$     subroutine dpc_a{}ABB($1)m( &
!!$          obj, LWR($1)MomentGroupList, polytopeIndex, log )
!!$       import DeparturePointCollectionInterface, &
!!$            $1MomentGroupListType, &
!!$            FlowVariableInterface, LogType
!!$       class(DeparturePointCollectionInterface), intent(inout) :: obj
!!$       type($1MomentGroupListType), intent(inout) :: &
!!$            LWR($1)MomentGroupList
!!$       integer, intent(in) :: polytopeIndex
!!$       class(LogType), intent(inout), optional :: log
!!$     end subroutine dpc_a{}ABB($1)m
     }})

  MACRO({SIGNATUREDEF_DEPARTUREPOINTCOLLECTION_INITFIELDS},{{
!!$     subroutine dpc_i{}ABB($1)f( obj, LWR($1)FieldSpecifications, log )
!!$       import DeparturePointCollectionInterface, &
!!$            $1FieldSpecificationListType, LogType
!!$       class(DeparturePointCollectionInterface), intent(in) :: obj
!!$       type($1FieldSpecificationListType), intent(in) :: &
!!$            LWR($1)FieldSpecifications
!!$       class(LogType), intent(inout), optional :: log
!!$     end subroutine dpc_i{}ABB($1)f
     }})


  
contains

  
  !-------------------------------------------------------------------
  !- PointMomentSource methods
  !-------------------------------------------------------------------
  

  ! ***EARMARKED FOR DELETION***  
  MACRO({METHODDEF_ALLOCMATRIXROWS_SCALARPOINTMOMENTSOURCE}, {{
  subroutine allocMatrixRows_PointScalar$1( obj, rows, &
       pointGroupPointer, log )
    class(PointScalar$1Interface), intent(in) :: obj
    real(FLOAT), dimension(:, :), allocatable, intent(out) :: rows
    class(PointGroupPointerInterface), intent(in) :: pointGroupPointer
    class(LogType), intent(inout), optional :: log
    integer :: nPoints, stat
    call beginSub( MOD_NAME, 'allocMatrixRows_PointScalar$1', log )

    ! The client could have delegated to pointGroupPointer to create
    ! the matrix rows.  But that object has the smaller concern of
    ! computing component sub-rows straightforwardly according to a
    ! SpatialDerivative.  The broader control belongs to
    ! Point$1s.
    nPoints = pointGroupPointer%getNumPoints()
    allocate( rows(nPoints, NCOEFS_ADV), stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating rows(&
         &nPoints, NCOEFS_ADV).  STAT='//int2str(stat), log )
    
    call endSub( log )
  end subroutine allocMatrixRows_PointScalar$1
  }}) ! ***EARMARKED FOR DELETION***  

  
  !-------------------------------------------------------------------
  !- PointVectorMomentSource methods
  !-------------------------------------------------------------------

  ! ***EARMARKED FOR DELETION***  
  MACRO({METHODDEF_ALLOCMATRIXROWS_VECTORPOINTMOMENTSOURCE}, {{
  subroutine allocMatrixRows_PointVector$1( obj, rows, &
       pointGroupPointer, log )
    class(PointVector$1Interface), intent(in) :: obj
    real(FLOAT), dimension(:, :, :), allocatable, intent(out) :: rows
    class(PointGroupPointerInterface), intent(in) :: pointGroupPointer
    class(LogType), intent(inout), optional :: log
    integer :: nRows, nColumns, stat
    call beginSub( MOD_NAME, 'allocMatrixRows_PointVector$1', log )
    
    ! The client could have delegated to pointGroupPointer to create
    ! the matrix rows.  But that object has the smaller concern of
    ! computing component sub-rows straightforwardly according to a
    ! SpatialDerivative.  The broader control belongs to
    ! Point$1s.
    nRows = pointGroupPointer%getNumPoints()
    nColumns = NDIM*NCOEFS_ADV   ! formerly obj%getMatrixRowWidth()
    allocate( rows(nRows, nColumns, NDIM), stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating rows(&
         &nRows, nColumns, NDIM).  STAT='//int2str(stat), log )
    
    call endSub( log )
  end subroutine allocMatrixRows_PointVector$1
  }}) ! ***EARMARKED FOR DELETION***  



  
  !-------------------------------------------------------------------
  !- PointMomentField methods
  !-------------------------------------------------------------------
  
  MACRO({METHODDEFS_POINTMOMENTFIELD},{{
  EXPAND({METHODDEFS_SUBMOMENTFIELD($1, Point)})
  }})

  
  !-------------------------------------------------------------------
  !- PointBoundaryCondition methods
  !-------------------------------------------------------------------

  MACRO({METHODDEFS_POINTBOUNDARYCONDITION},{{
  EXPAND({METHODDEFS_BOUNDARYCONDITION({$1}, {Point})})
  }})

  
  !-------------------------------------------------------------------
  !- PointMomentPointer methods  ! ***EARMARKED FOR DELETION***  
  !-------------------------------------------------------------------

  MACRO({METHODDEFS_POINTMOMENTPOINTER}, {{  
  EXPAND({PROCEDURE_DESTROY_ARRAY({$1PointMomentPointer}, {Type})})
  
  subroutine init_$1PointMomentPointer( obj, &
       LWR($1PointMomentGroup), pointIndex )
    class($1PointMomentPointerType), intent(out) :: obj
    class($1PointMomentGroupInterface), target, intent(in) :: &
         LWR($1PointMomentGroup)
    integer, intent(in) :: pointIndex
    obj%ABB($1PointMomentGroup) => LWR($1PointMomentGroup)
    obj%pointIndex = pointIndex
  end subroutine init_$1PointMomentPointer

  
  pure subroutine deinit_$1PointMomentPointer( obj, log )
    class($1PointMomentPointerType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_$1PointMomentPointer', log )
    nullify(obj%ABB($1PointMomentGroup))
    obj%pointIndex = 0
    call endSub( log )
  end subroutine deinit_$1PointMomentPointer

  
  elemental subroutine copy_$1PointMomentPointer( obj, src, log )
    class($1PointMomentPointerType), intent(inout) :: obj
    class($1MomentPointerInterface), intent(in) :: src
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'copy_PointMomentPointer', log )

    select type ( src )
    type is ($1PointMomentPointerType)
       obj%ABB($1PointMomentGroup) => &
            src%ABB($1PointMomentGroup)
       obj%pointIndex = src%pointIndex
    class default
       call addEvent( WARNING, 'Source object''s type did not match that &
            &of current object.', log )
    end select
    
    call endSub( log )
  end subroutine copy_$1PointMomentPointer

  
  elemental subroutine getValue_$1PointMomentPointer( &
       obj ARGLINE_TENSOR_COMPONENT($1) ) result ( value )
    class($1PointMomentPointerType), intent(in) :: obj
    real(FLOAT), intent(out) :: value
    ARGLIST_TENSOR_COMPONENT($1)
    call obj%ABB($1PointMomentGroup)%&
         getValue( value, obj%pointIndex{}ARGLINE_TENSOR_COMPONENT($1) )
  end subroutine getValue_$1PointMomentPointer
  }})

end module SemiLagrangian_Macros
