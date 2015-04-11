HEADER()
module SemiLagrangianModule
 
  USE_MACROS({SemiLagrangian})
  USE_MACROS({Container})
  use LogModule
  use Global
  use FlowFieldModule

  implicit none
  private
  public :: destroy

  character(*), parameter :: MOD_NAME = 'SemiLagrangianModule'

  interface destroy
     module procedure destroyPointScalarMomentField
     module procedure destroyPointVectorMomentField
     module procedure destroyPointScalarBoundaryCondition
     module procedure destroyPointVectorBoundaryCondition
     module procedure destroyDeparturePointCollection
  end interface
       
  
  !-------------------------------------------------------------------
  !- PointMomentFields
  !-------------------------------------------------------------------

  EXPAND({TYPEDEF_POINTMOMENTFIELD({Scalar})})

  EXPAND({TYPEDEF_POINTMOMENTFIELD({Vector})})
    
  EXPAND({TYPEDEFS_CONTAINER(PointScalarMomentField, Interface, List)})
  
  EXPAND({TYPEDEFS_CONTAINER(PointVectorMomentField, Interface, List)})
  
     
  !-------------------------------------------------------------------
  !- PointBoundaryConditions
  !-------------------------------------------------------------------

  EXPAND({TYPEDEF_POINTBOUNDARYCONDITION({Scalar})})

  EXPAND({TYPEDEF_POINTBOUNDARYCONDITION({Vector})})

  EXPAND({TYPEDEFS_CONTAINER(PointScalarBoundaryCondition, Interface,
  List)})

  EXPAND({TYPEDEFS_CONTAINER(PointVectorBoundaryCondition, Interface,
  List)})

  
  !-------------------------------------------------------------------
  !- DeparturePoints
  !-------------------------------------------------------------------

  type, abstract, public :: DeparturePointCollectionInterface
     private
   contains
     procedure(dpc_di), deferred :: deinit
     procedure(dpc_aum), deferred :: appendVelocityMoments
     procedure(dpc_asm), deferred :: appendScalarMoments
     procedure(dpc_isf), deferred :: initialiseScalarFields
     procedure(dpc_ivf), deferred :: initialiseVectorFields
     generic :: initialiseFields => &
          initialiseScalarFields, initialiseVectorFields
  end type DeparturePointCollectionInterface


  !-------------------------------------------------------------------
  !- abstract interfaces
  !-------------------------------------------------------------------
  
  ! PointMomentField and subclass signatures
  abstract interface
     EXPAND({SIGNATUREDEFS_POINTMOMENTFIELD({Scalar})})
     
     EXPAND({SIGNATUREDEFS_POINTMOMENTFIELD({Vector})})
  end interface

    
  ! PointBoundaryCondition and subclass signatures
  abstract interface
     EXPAND({SIGNATUREDEFS_POINTBOUNDARYCONDITION({Scalar})})
     
     EXPAND({SIGNATUREDEFS_POINTBOUNDARYCONDITION({Vector})})
  end interface


  ! DeparturePoint signatures
  abstract interface
     subroutine dpc_di( obj, log )
       import DeparturePointCollectionInterface, LogType
       class(DeparturePointCollectionInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine dpc_di

     EXPAND({SIGNATUREDEF_APPENDMOMENTS({Scalar},
     {DeparturePointCollection}, {Interface}, {polytopeIndex, }, {
       integer, intent(in) :: polytopeIndex})})

     EXPAND({SIGNATUREDEF_APPENDVELOCITYMOMENTS(
     {DeparturePointCollection}, {Interface}, {polytopeIndex, }, {
       integer, intent(in) :: polytopeIndex})})

!!$     ! This signature is designed specifically for velocity as opposed
!!$     ! to general vectors, so its signature contains "u" instead of "v"
!!$     subroutine dpc_aum( obj, velocityMomentGroupList, &
!!$          polytopeIndex, log )
!!$       import DeparturePointCollectionInterface, &
!!$            VectorMomentGroupListType, FlowVariableInterface, LogType
!!$       class(DeparturePointCollectionInterface), intent(inout) :: obj
!!$       type(VectorMomentGroupListType), intent(inout) :: &
!!$            velocityMomentGroupList
!!$       integer, intent(in) :: polytopeIndex
!!$       class(LogType), intent(inout), optional :: log
!!$     end subroutine dpc_aum
     
     EXPAND({SIGNATUREDEF_DEPARTUREPOINTCOLLECTION_APPENDMOMENTS(
     {Scalar})})

     EXPAND({SIGNATUREDEF_INITFIELDS({Scalar}, {DeparturePointCollection})})
     
     EXPAND({SIGNATUREDEF_INITFIELDS({Vector}, {DeparturePointCollection})})
  end interface


contains

  !-------------------------------------------------------------------
  !- PointMomentField and subclass methods
  !-------------------------------------------------------------------

  EXPAND({METHODDEFS_POINTMOMENTFIELD({Scalar})})

  EXPAND({METHODDEFS_POINTMOMENTFIELD({Vector})})

  EXPAND({METHODDEFS_CONTAINER({PointScalarMomentField}, {Interface},
  {List})})
  
  EXPAND({METHODDEFS_CONTAINER({PointVectorMomentField}, {Interface},
  {List})})
  
  
  !-------------------------------------------------------------------
  !- PointBoundaryCondition and subclass methods
  !-------------------------------------------------------------------

  EXPAND({METHODDEFS_POINTBOUNDARYCONDITION({Scalar})})

  EXPAND({METHODDEFS_POINTBOUNDARYCONDITION({Vector})})

  EXPAND({METHODDEFS_CONTAINER({PointScalarBoundaryCondition}, {Interface},
  {List})})
  
  EXPAND({METHODDEFS_CONTAINER({PointVectorBoundaryCondition}, {Interface},
  {List})})

  
  !-------------------------------------------------------------------
  !- DeparturePoint methods
  !-------------------------------------------------------------------

  EXPAND({PROCEDURE_DESTROY({DeparturePointCollection})})
  

end module SemiLagrangianModule
 
