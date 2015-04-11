HEADER()
module FlowFieldModule
  
  USE_MACROS({Container})
  USE_MACROS({FlowField})
  use LogModule
  use Global
  use FlowField_AdvectionProfile_Cubic

  implicit none
  private
  public :: destroy, dot_product, DEGREE_ADV, NCOEFS_ADV, POWERS_ADV, &
       
       createScalarMomGpBasedRHSGenerator, &
       allocScalarMomGpBasedRHSGeneratorArray, &
       initScalarMomGpBasedRHSGeneratorArrayElement, &
       
       createVectorMomGpBasedRHSGenerator, &
       allocVectorMomGpBasedRHSGeneratorArray, &
       initVectorMomGpBasedRHSGeneratorArrayElement, &
       
       createScalarLinOpBasedRHSGenerator, &
       allocScalarLinOpBasedRHSGeneratorArray, &
       initScalarLinOpBasedRHSGeneratorArrayElement, &
       
       createVectorLinOpBasedRHSGenerator, &
       allocVectorLinOpBasedRHSGeneratorArray, &
       initVectorLinOpBasedRHSGeneratorArrayElement

  
  character(*), parameter :: MOD_NAME = 'FlowFieldModule'
  
  interface destroy
     module procedure destroyScalarFlowVariable
     module procedure destroyVectorFlowVariable
     module procedure destroyDirectedSpatialDerivative
     module procedure destroyUndirectedSpatialDerivative
     module procedure destroyDirection
     module procedure destroyDirectionArray
     module procedure destroyPointArrangement
     module procedure destroyStaticPointCollection
     module procedure destroyInteriorStaticPointCollection
     module procedure destroyBoundaryStaticPointCollection
     module procedure destroyScalarMomentSource
     module procedure destroyVectorMomentSource
     module procedure destroyScalarMomentField
     module procedure destroyVectorMomentField
     module procedure destroyScalarBoundaryCondition
     module procedure destroyVectorBoundaryCondition
     module procedure destroyScalarRHSGenerator
     module procedure destroyScalarRHSGeneratorArray
     module procedure destroyVectorRHSGenerator
     module procedure destroyVectorRHSGeneratorArray
     module procedure destroyScalarMomentGroup
     module procedure destroyVectorMomentGroup
     module procedure destroyScalarMomentPointerArray
     module procedure destroyVectorMomentPointerArray
     module procedure destroyRHSElement
     module procedure destroyRHSElementArray
     module procedure destroyScalarMatrix
     module procedure destroyVectorMatrix
     module procedure destroyScalarRHSColumn
     module procedure destroyVectorRHSColumn
     module procedure destroyScalarLinearSystem
     module procedure destroyVectorLinearSystem
     module procedure destroyPureScalarLinearOperation
     module procedure destroyPureScalarLinearOperationArray
     module procedure destroyImpureScalarLinearOperation
     module procedure destroyPureVectorLinearOperation
     module procedure destroyPureVectorLinearOperationArray
     module procedure destroyImpureVectorLinearOperation
     module procedure destroyScalarFieldSpecification
     module procedure destroyVectorFieldSpecification
  end interface


  interface assignment(=)
     module procedure assign_ScalarMomentPointer
     module procedure assign_VectorMomentPointer
  end interface

  
  interface dot_product
     module procedure dot_product_Direction_single
     module procedure dot_product_Direction_multi
  end interface


  !-------------------------------------------------------------------
  !- SpatialDerivativeInterfaces and DirectionInterface
  !-------------------------------------------------------------------

  EXPAND({TYPEDEF_SPATIALDERIVATIVE({Undirected}, {
!!$     procedure(usd_cd), deferred :: createDirected
!!$     generic :: create => createDirected})})

  EXPAND({TYPEDEF_SPATIALDERIVATIVE({Directed})})
  
  type, abstract, public :: DirectionInterface
     private
   contains
     procedure(d_di), deferred :: deinit
     procedure(d_c_s), deferred :: clone_single
     procedure(d_c_m), deferred :: clone_multi
     generic :: clone => clone_multi, clone_single
     procedure(d_dp), deferred :: dotProduct
     procedure(d_c), deferred :: convert
     procedure(d_sa), deferred :: sameAs
  end type DirectionInterface


  !-------------------------------------------------------------------
  !- FlowVariable
  !-------------------------------------------------------------------

  ! expect to issue up to 10 FlowVariable IDs
  type(IDManagerType) :: IDManager

  type, abstract, public :: FlowVariableInterface
     integer :: ID
     character(20) :: name
   contains
     EXPAND({SIGNATURELIST_FLOWVARIABLE})
     procedure(fv_gnc), deferred :: getNumComponents
     procedure :: initFlowVariable
     procedure :: deinitFlowVariable
     procedure :: sameAs => sameAs_FlowVariable
     procedure :: describe => describe_FlowVariable
  end type FlowVariableInterface


  type, extends(FlowVariableInterface), abstract, public :: ScalarFlowVariableInterface
     private
   contains
     EXPAND({SIGNATURELIST_SUBFLOWVARIABLE(Scalar)})
     EXPAND({METHODLIST_SUBFLOWVARIABLE(Scalar)})
     ! memberOf tests whether a variable is a scalar component of the
     ! subject.  These arguments are not commutative.
     procedure(sfv_mo), deferred :: memberOf
  end type ScalarFlowVariableInterface


  type, extends(FlowVariableInterface), abstract, public :: VectorFlowVariableInterface
     private
   contains
     EXPAND({SIGNATURELIST_SUBFLOWVARIABLE(Vector)})
     EXPAND({METHODLIST_SUBFLOWVARIABLE(Vector)})
  end type VectorFlowVariableInterface


  EXPAND({TYPEDEFS_CONTAINER(ScalarFlowVariable, Interface, List)})

  EXPAND({TYPEDEFS_CONTAINER(VectorFlowVariable, Interface, List)})

  
  !-------------------------------------------------------------------
  !- StaticPoints
  !-------------------------------------------------------------------
 
  ! PointArrangement is a strategy for StaticPointCollection that is
  ! mainly responsible for computing point positions.  
  type, abstract, public :: PointArrangementInterface
   contains
     procedure(pa_di), deferred :: deinit
     procedure(pa_gp), deferred :: getPositions
     procedure(pa_cgw), deferred :: createGaussianWeights
     procedure(pa_gnpp), deferred :: getNumPointsPerPolytope
     procedure(pa_cpm), deferred :: createPositionsMatrix
     procedure(pa_cpv), deferred :: convertToPolytopeValues
  end type PointArrangementInterface

  
  type, abstract, public :: StaticPointCollectionInterface
     class(PointArrangementInterface), allocatable :: pointArrangement
   contains
     EXPAND({SIGNATURELIST_STATICPOINTCOLLECTION()})
     EXPAND({INITLIST_STATICPOINTCOLLECTION()})
     EXPAND({METHODLIST_STATICPOINTCOLLECTION()})
     procedure :: getPositions => getPositions_StaticPointCollection
     procedure :: getNumPointsPerPolytope => &
          getNumPointsPerPolytope_StaticPointCollection
     procedure :: createPositionsMatrix => &
          createPositionsMatrix_StaticPointCollection
     procedure :: convertToPolytopeValues => &
          convertToPolytopeValues_StaticPointCollection
  end type StaticPointCollectionInterface
  

  type, abstract, public :: StaticPointGroupPointerInterface
     private
   contains
     EXPAND({SIGNATURELIST_STATICPOINTGROUPPOINTER()}) 
  end type StaticPointGroupPointerInterface

  
  !-------------------------------------------------------------------
  !- InteriorStaticPoints
  !-------------------------------------------------------------------

  EXPAND({TYPEDEF_SUBSTATICPOINTCOLLECTION({Interior})})
     
  EXPAND({TYPEDEF_STATICPOINTGROUPPOINTER_CONCRETE({Interior})})
  
  
  !-------------------------------------------------------------------
  !- BoundaryStaticPoints
  !-------------------------------------------------------------------

  EXPAND({TYPEDEF_SUBSTATICPOINTCOLLECTION({Boundary}, {}, {}, {
     procedure(bspc_gfn), deferred :: getFaceNormals})})
  
  EXPAND({TYPEDEF_STATICPOINTGROUPPOINTER_CONCRETE({Boundary}, {}, {}, {
     procedure :: getFaceNormals => &
          getFaceNormals_BoundaryStaticPointGroupPointer})})

  !-------------------------------------------------------------------
  !- MomentSource
  !-------------------------------------------------------------------

  EXPAND({TYPEDEF_MOMENTSOURCE({Scalar})})
  
  EXPAND({TYPEDEF_MOMENTSOURCE({Vector})})


  !-------------------------------------------------------------------
  !- MomentField
  !-------------------------------------------------------------------

  EXPAND({TYPEDEF_MOMENTFIELD({Scalar})})
  
  EXPAND({TYPEDEF_MOMENTFIELD({Vector})}) 

  !-------------------------------------------------------------------
  !- BoundaryCondition
  !-------------------------------------------------------------------

  EXPAND({TYPEDEF_BOUNDARYCONDITION({Scalar})})
  
  EXPAND({TYPEDEF_BOUNDARYCONDITION({Vector})})
  
  !-------------------------------------------------------------------
  !- MomentGroups
  !-------------------------------------------------------------------

  EXPAND({TYPEDEF_MOMENTGROUP({Scalar})})
  
  EXPAND({TYPEDEF_MOMENTGROUP({Vector})})

  EXPAND({TYPEDEFS_CONTAINER_MOMENTGROUP(Scalar)})

  EXPAND({TYPEDEFS_CONTAINER_MOMENTGROUP(Vector)})


  !-------------------------------------------------------------------
  !- RHSElements, MomentPointers
  !-------------------------------------------------------------------

  type, abstract, public :: RHSElementInterface
   contains
     procedure(rhse_di), deferred :: deinit
     procedure(rhse_gv), deferred :: getValue
     procedure(rhse_cl), deferred :: clone
  end type RHSElementInterface

  EXPAND({TYPEDEF_MOMENTPOINTER({Scalar})})
  
  EXPAND({TYPEDEF_MOMENTPOINTER({Vector})})


  !-------------------------------------------------------------------
  !- Linear Operations 
  !-------------------------------------------------------------------
  
  EXPAND({TYPEDEF_LINEAROPERAND({Scalar})})
  
  EXPAND({TYPEDEF_LINEAROPERAND({Vector})})

  
  !-------------------------------------------------------------------
  !- Matricies, RHS, LinearSystems
  !-------------------------------------------------------------------

  EXPAND({TYPEDEF_MATRIX({Scalar})})

  EXPAND({TYPEDEF_MATRIX({Vector})})

  EXPAND({TYPEDEF_RHSCOLUMN({Scalar})})

  EXPAND({TYPEDEF_RHSCOLUMN({Vector})})
     
  EXPAND({TYPEDEF_LINEARSYSTEM({Scalar})})  

  EXPAND({TYPEDEF_LINEARSYSTEM({Vector})})

  
  !-------------------------------------------------------------------
  !- Linear Operations 
  !-------------------------------------------------------------------
  
  EXPAND({TYPEDEFS_LINEAROPERATION({Scalar})})
  
  EXPAND({TYPEDEFS_LINEAROPERATION({Vector})})

  
  !-------------------------------------------------------------------
  !- RHSGenerators
  !-------------------------------------------------------------------

  EXPAND({TYPEDEFS_RHSGENERATOR({Scalar})})

  EXPAND({TYPEDEFS_RHSGENERATOR({Vector})})

  
  !-------------------------------------------------------------------
  !- FieldSpecification
  !-------------------------------------------------------------------

  EXPAND({TYPEDEF_FIELDSPECIFICATION({Scalar})})
  
  EXPAND({TYPEDEF_FIELDSPECIFICATION({Vector})})

  EXPAND({TYPEDEFS_CONTAINER(ScalarFieldSpecification, Interface, List)})

  EXPAND({TYPEDEFS_CONTAINER(VectorFieldSpecification, Interface, List)})

  
  !-------------------------------------------------------------------
  !- abstract interfaces
  !-------------------------------------------------------------------
     
  ! SpatialDerivative signatures
  abstract interface
     EXPAND({SIGNATUREDEFS_SPATIALDERIVATIVE({Undirected})})
  
     pure function usd_csr( obj, rowComponents, directions ) &
          result ( rows )
       import UndirectedSpatialDerivativeInterface, &
            DirectionInterface, RealVectorType, FLOAT
       class(UndirectedSpatialDerivativeInterface), intent(in) :: obj
       ! this routine is similar to computeComponentRows (sd_ccr)
       ! except that the components are collapsed along the 1st
       ! dimension.  The remaining dimensions shift to the left so
       ! that the monomials are now correctly represented along the 2nd
       ! dimension, i.e. in row form.
       real(FLOAT), dimension(:, :, :), intent(in) :: rowComponents
       class(DirectionInterface), dimension(:), intent(in) :: &
            directions
       real(FLOAT), dimension(size(rowComponents, 2), size(&
            rowComponents, 3)) :: rows
     end function usd_csr

     EXPAND({SIGNATUREDEFS_SPATIALDERIVATIVE({Directed})})
       
     pure function dsd_csr( obj, rowComponents ) result ( rows )
       ! same as usd_csr, but the direction argument is omitted
       ! since it is already embedded in the object
       import DirectedSpatialDerivativeInterface, RealVectorType, &
            FLOAT
       class(DirectedSpatialDerivativeInterface), intent(in) :: obj
       real(FLOAT), dimension(:, :, :), intent(in) :: rowComponents
       real(FLOAT), dimension(size(rowComponents, 2), size(&
            rowComponents, 3)) :: rows
     end function dsd_csr
  end interface

  ! Direction signatures
  abstract interface
     pure subroutine d_di( obj, log )
       import DirectionInterface, LogType
       class(DirectionInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine d_di

     pure subroutine d_c_s( obj, tgt, log )
       import DirectionInterface, LogType
       class(DirectionInterface), intent(in) :: obj
       class(DirectionInterface), allocatable, intent(inout) :: tgt
       class(LogType), intent(inout), optional :: log
     end subroutine d_c_s

     pure subroutine d_c_m( obj, directions, nElements, log )
       import DirectionInterface, LogType
       class(DirectionInterface), intent(in) :: obj
       class(DirectionInterface), dimension(:), allocatable, intent(&
            inout) :: directions
       integer, intent(in) :: nElements
       class(LogType), intent(inout), optional :: log
     end subroutine d_c_m
     
     pure function d_dp( obj, singlePointRowComponents )
       import DirectionInterface, FLOAT, LogType
       class(DirectionInterface), intent(in) :: obj
       real(FLOAT), dimension(:, :), intent(in) :: &
            singlePointRowComponents
       real(FLOAT), dimension(size(singlePointRowComponents, 2)) :: &
            d_dp
     end function d_dp
     
     pure function d_c( obj )
       import DirectionInterface, RealVectorType
       class(DirectionInterface), intent(in) :: obj
       type(RealVectorType) :: d_c
     end function d_c
     
     pure function d_sa( obj, direction )
       import DirectionInterface
       class(DirectionInterface), intent(in) :: obj
       class(DirectionInterface), intent(in) :: direction
       logical :: d_sa
     end function d_sa
  end interface

  ! FlowVariable signatures
  abstract interface
     SIGNATUREDEFS_FLOWVARIABLE()
     
     pure function fv_gnc( obj )
       import FlowVariableInterface
       class(FlowVariableInterface), intent(in) :: obj
       integer :: fv_gnc
     end function fv_gnc

     SIGNATUREDEFS_SUBFLOWVARIABLE(Scalar)
     
     pure function sfv_mo( obj, vectorFlowVariable )
       import ScalarFlowVariableInterface, VectorFlowVariableInterface
       class(ScalarFlowVariableInterface), intent(in) :: obj
       class(VectorFlowVariableInterface), intent(in) :: &
            vectorFlowVariable
       logical :: sfv_mo
     end function sfv_mo

     SIGNATUREDEFS_SUBFLOWVARIABLE(Vector)
  end interface

  
  ! PointArrangementInterface signatures  
  abstract interface     
     pure subroutine pa_di( obj, log )
       import PointArrangementInterface, LogType
       class(PointArrangementInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine pa_di
     
     pure subroutine pa_gp( obj, pointCoords, polytopeIndex, &
          pointIndex, log )
       import PointArrangementInterface, RealVectorType, LogType
       class(PointArrangementInterface), intent(in) :: obj
       type(RealVectorType), dimension(:), intent(out) :: pointCoords
       integer, intent(in) :: polytopeIndex
       integer, intent(in), optional :: pointIndex
       class(LogType), intent(inout), optional :: log
     end subroutine pa_gp

     pure subroutine pa_cgw( obj, gaussianWeights, &
          log )
       import PointArrangementInterface, FLOAT, LogType
       class(PointArrangementInterface), intent(in) :: obj
       real(FLOAT), dimension(:), allocatable, intent(inout) :: &
            gaussianWeights
       class(LogType), intent(inout), optional :: log
     end subroutine pa_cgw
     
     pure function pa_gnpp( obj )
       import PointArrangementInterface
       class(PointArrangementInterface), intent(in) :: obj
       integer :: pa_gnpp
     end function pa_gnpp

     subroutine pa_cpm( obj, positions, log )
       import PointArrangementInterface, RealVectorType, LogType
       class(PointArrangementInterface), intent(in) :: obj
       type(RealVectorType), dimension(:, :), allocatable, intent(&
            inout) :: positions
       class(LogType), intent(inout), optional :: log
     end subroutine pa_cpm
          
     function pa_cpv( obj, valuesMatrix ) result ( valuesArray )
       import PointArrangementInterface, FLOAT
       class(PointArrangementInterface), intent(in) :: obj
       real(FLOAT), dimension(:, :), intent(in) :: valuesMatrix
       real(FLOAT), dimension(size(valuesMatrix, 2)) :: valuesArray
     end function pa_cpv
  end interface
     
  ! StaticPoints signatures  
  abstract interface
     EXPAND({SIGNATUREDEFS_STATICPOINTCOLLECTION()})
     
     EXPAND({SIGNATUREDEFS_STATICPOINTGROUPPOINTER()})
  end interface
  

  ! InteriorStaticPoints signatures  
  abstract interface
     EXPAND({SIGNATUREDEFS_SUBSTATICPOINTCOLLECTION({Interior},
     {Directed})})
  end interface
  

  ! BoundaryStaticPoints signatures  
  abstract interface
     EXPAND({SIGNATUREDEFS_SUBSTATICPOINTCOLLECTION({Boundary},
     {Undirected})})
     
     pure subroutine bspc_gfn( obj, directions, polytopeIndex, log )
       import BoundaryStaticPointCollectionInterface, &
            DirectionInterface, LogType
       class(BoundaryStaticPointCollectionInterface), intent(in) :: obj
       class(DirectionInterface), dimension(:), allocatable, intent(&
            out) :: directions
       integer, intent(in) :: polytopeIndex
       class(LogType), intent(inout), optional :: log
     end subroutine bspc_gfn
  end interface

  
  ! MomentSource signatures
  abstract interface
     EXPAND({SIGNATUREDEFS_MOMENTSOURCE({Scalar}, {MomentSource})})
     
     EXPAND({SIGNATUREDEFS_MOMENTSOURCE({Vector}, {MomentSource})})
  end interface

  
  ! MomentField signatures
  abstract interface
     EXPAND({SIGNATUREDEFS_MOMENTFIELD({Scalar})})
     
     EXPAND({SIGNATUREDEFS_MOMENTFIELD({Vector})})
  end interface

  
  ! BoundaryCondition signatures
  abstract interface
     EXPAND({SIGNATUREDEFS_BOUNDARYCONDITION({Scalar})})
     
     EXPAND({SIGNATUREDEFS_BOUNDARYCONDITION({Vector})})
  end interface


  ! MomentGroup signatures
  abstract interface
     EXPAND({SIGNATUREDEFS_MOMENTGROUP({Scalar})})
     
     EXPAND({SIGNATUREDEFS_MOMENTGROUP({Vector})})
  end interface


  ! RHSElement signatures
  abstract interface
     pure subroutine rhse_di( obj, log )
       import RHSElementInterface, LogType
       class(RHSElementInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine rhse_di

     elemental subroutine rhse_gv( obj, value )
       import RHSElementInterface, FLOAT
       class(RHSElementInterface), intent(in) :: obj
       real(FLOAT), intent(out) :: value
     end subroutine rhse_gv

     subroutine rhse_cl( obj, tgt, log )
       import RHSElementInterface, LogType
       class(RHSElementInterface), intent(in) :: obj
       class(RHSElementInterface), allocatable, intent(out) :: tgt
       class(LogType), intent(inout), optional :: log
     end subroutine rhse_cl
  end interface
  
  ! LinearOperand signatures
  abstract interface
     EXPAND({SIGNATUREDEFS_LINEAROPERAND({Scalar})})

     EXPAND({SIGNATUREDEFS_LINEAROPERAND({Vector})})
  end interface

  
  ! Matrix signatures
  abstract interface
     EXPAND({SIGNATUREDEFS_MATRIX({Scalar})})

     EXPAND({SIGNATUREDEFS_MATRIX({Vector})})
  end interface

  
  ! RHSColumn signatures
  abstract interface
     EXPAND({SIGNATUREDEFS_RHSCOLUMN({Scalar})})

!!$     pure subroutine srhsc_gv( obj, values )
!!$       import ScalarRHSColumnInterface, FLOAT
!!$       class(ScalarRHSColumnInterface), intent(inout) :: obj
!!$       real(FLOAT), dimension(:, :), intent(out) :: values
!!$     end subroutine srhsc_gv 

     EXPAND({SIGNATUREDEFS_RHSCOLUMN({Vector})})
     
!!$     pure subroutine vrhsc_gv( obj, values, componentIndex, coupled )
!!$       import VectorRHSColumnInterface, FLOAT
!!$       class(VectorRHSColumnInterface), intent(inout) :: obj
!!$       real(FLOAT), dimension(:, :), intent(out) :: values
!!$       integer, intent(in) :: componentIndex
!!$       logical, intent(in) :: coupled
!!$     end subroutine vrhsc_gv 
  end interface

  
  ! LinearSystem signatures
  abstract interface
     EXPAND({SIGNATUREDEFS_LINEARSYSTEM({Scalar})})

     EXPAND({SIGNATUREDEFS_LINEARSYSTEM({Vector})})
  end interface

  
  ! LinearOperation signatures
  abstract interface
     EXPAND({SIGNATUREDEFS_LINEAROPERATION({Scalar})})
     
     EXPAND({SIGNATUREDEFS_SUBLINEAROPERATIONS({Scalar})})

     EXPAND({SIGNATUREDEFS_LINEAROPERATION({Vector})})

     EXPAND({SIGNATUREDEFS_SUBLINEAROPERATIONS({Vector})})
  end interface


  ! FieldSpecification signatures
  abstract interface
     EXPAND({SIGNATUREDEFS_FIELDSPECIFICATION({Scalar})})
     
     EXPAND({SIGNATUREDEFS_FIELDSPECIFICATION({Vector})})
  end interface

contains

  
  !-------------------------------------------------------------------
  !- SpatialDerivative methods
  !-------------------------------------------------------------------
  
  EXPAND({PROCEDURE_DESTROY(UndirectedSpatialDerivative)})
  
  EXPAND({PROCEDURE_DESTROY(DirectedSpatialDerivative)})
  
  EXPAND({METHODDEF_COMPUTESUBROWCOMPONENTS(Undirected)})
  
  EXPAND({METHODDEF_COMPUTESUBROWCOMPONENTS(Directed)})

  
  !-------------------------------------------------------------------
  !- Direction methods
  !-------------------------------------------------------------------
  
  EXPAND({PROCEDURE_DESTROY(Direction, pure)})

  EXPAND({PROCEDURE_DESTROY_ARRAY(Direction, Interface, pure)})
  
  pure function dot_product_Direction_single( direction, rowComponents )
    class(DirectionInterface), intent(in) :: direction
    real(FLOAT), dimension(:, :, :), intent(in) :: rowComponents
    real(FLOAT), dimension(size(rowComponents, 2), size(&
         rowComponents, 3)) :: dot_product_Direction_single
    integer :: i
    
    forall ( i = 1:size(rowComponents, 2) )
       ! apply the polymorphic dot product to all points.  The forall
       ! block is just there to achieve shape matching.
       dot_product_Direction_single(i, :) = direction%&
            dotProduct( rowComponents(:, i, :) )
    end forall
  end function  dot_product_Direction_single

  
  pure function dot_product_Direction_multi( directions, &
       rowComponents )
    class(DirectionInterface), dimension(:), intent(in) :: directions
    real(FLOAT), dimension(:, :, :), intent(in) :: rowComponents
    real(FLOAT), dimension(size(rowComponents, 2), size(&
         rowComponents, 3)) :: dot_product_Direction_multi
    integer :: i

    forall ( i = 1:size(rowComponents, 2) )
       ! apply the polymorphic dot product on a point-wise basis,
       ! selecting the corresponding direction each time.
       dot_product_Direction_multi(i, :) = directions(i)%&
            dotProduct( rowComponents(:, i, :) )
    end forall
  end function  dot_product_Direction_multi


  !-------------------------------------------------------------------
  !- FlowVariable methods
  !-------------------------------------------------------------------

  EXPAND({PROCEDURE_DESTROY(FlowVariable)})

  subroutine initFlowVariable( obj, name, log )
    class(FlowVariableInterface), intent(inout) :: obj
    character(*), intent(in) :: name
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'initFlowVariable', log )

    call IDManager%requestID( obj%ID, log )
    obj%name = name

    call endSub( log )
  end subroutine initFlowVariable


  subroutine deinitFlowVariable( obj, log )
    class(FlowVariableInterface), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinitFlowVariable', log )

    call IDManager%returnID( obj%ID )
    obj%name = ''

    call endSub( log )
  end subroutine deinitFlowVariable


  pure function describe_FlowVariable( obj )
    class(FlowVariableInterface), intent(in) :: obj
    character(60) :: describe_FlowVariable
    describe_FlowVariable = trim(obj%name)//' (variable '//str(&
         obj%ID)//')'
  end function describe_FlowVariable


  pure function sameAs_FlowVariable( obj, fv )
    class(FlowVariableInterface), intent(in) :: obj
    class(FlowVariableInterface), intent(in) :: fv
    logical :: sameAs_FlowVariable
    sameAs_FlowVariable = ( obj%ID == fv%ID )
  end function sameAs_FlowVariable

  
  !-------------------------------------------------------------------
  !- ScalarFlowVariable methods
  !-------------------------------------------------------------------

  EXPAND({PROCEDURE_DESTROY(ScalarFlowVariable)})

  EXPAND({METHODDEFS_SUBFLOWVARIABLE(Scalar)})
  
  pure function getNumComponents_ScalarFlowVariable( obj )
    class(ScalarFlowVariableInterface), intent(in) :: obj
    integer :: getNumComponents_ScalarFlowVariable
    getNumComponents_ScalarFlowVariable = NDIM
  end function getNumComponents_ScalarFlowVariable

  EXPAND({METHODDEFS_CONTAINER({ScalarFlowVariable}, {Interface}, 
       {List}, {}, {}, {! using alternative injection method})})
  

  
  !-------------------------------------------------------------------
  !- VectorFlowVariable methods
  !-------------------------------------------------------------------

  EXPAND({PROCEDURE_DESTROY(VectorFlowVariable)})

  EXPAND({METHODDEFS_SUBFLOWVARIABLE(Vector)})
  
  pure function getNumComponents_VectorFlowVariable( obj )
    class(VectorFlowVariableInterface), intent(in) :: obj
    integer :: getNumComponents_VectorFlowVariable
    getNumComponents_VectorFlowVariable = NDIM
  end function getNumComponents_VectorFlowVariable

  EXPAND({METHODDEFS_CONTAINER({VectorFlowVariable}, {Interface}, 
       {List}, {}, {}, {! using alternative injection method})})
  

  
  !-------------------------------------------------------------------
  !- PointArrangement methods
  !-------------------------------------------------------------------

  EXPAND({PROCEDURE_DESTROY(PointArrangement)})


  !-------------------------------------------------------------------
  !- StaticPointCollection methods
  !-------------------------------------------------------------------

  EXPAND({PROCEDURE_DESTROY(StaticPointCollection)})

  subroutine initStaticPointCollection( obj, pointArrangement, log )
    class(StaticPointCollectionInterface), intent(inout) :: obj
    class(PointArrangementInterface), allocatable, intent(inout) :: &
         pointArrangement
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'initStaticPointCollection', log )

    EXPAND({INJECT({pointArrangement})})
    
    call endSub( log )
  end subroutine initStaticPointCollection

  
  subroutine deinitStaticPointCollection( obj, log )
    class(StaticPointCollectionInterface), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'deinitStaticPointCollection', log )

    call destroy( obj%pointArrangement, log )
    
    call endSub( log )
  end subroutine deinitStaticPointCollection

  
  pure subroutine getPositions_StaticPointCollection( obj, &
       pointCoords, polytopeIndex, log )
    class(StaticPointCollectionInterface), intent(in) :: obj
    type(RealVectorType), dimension(:), intent(out) :: pointCoords
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getPositions_StaticPointCollection', log )
    
    ! delegate 
    call obj%pointArrangement%getPositions( pointCoords, &
         polytopeIndex, log=log )
    
    call endSub( log )
  end subroutine getPositions_StaticPointCollection

  
  pure function getNumPointsPerPolytope_StaticPointCollection( obj )
    class(StaticPointCollectionInterface), intent(in) :: obj
    integer :: getNumPointsPerPolytope_StaticPointCollection
    getNumPointsPerPolytope_StaticPointCollection = &
         obj%pointArrangement%getNumPointsPerPolytope()
  end function getNumPointsPerPolytope_StaticPointCollection


  subroutine createPositionsMatrix_StaticPointCollection( obj, &
       positions, log )
    class(StaticPointCollectionInterface), intent(in) :: obj
    type(RealVectorType), dimension(:, :), allocatable, intent(inout) :: &
         positions
    class(LogType), intent(inout), optional :: log
    integer :: m, n
    call beginSub( MOD_NAME, &
         'createPositionsMatrix_StaticPointCollection', log )
    
    ! delegate
    call obj%pointArrangement%createPositionsMatrix( positions, log )
    
    call endSub( log )
  end subroutine createPositionsMatrix_StaticPointCollection


  function convertToPolytopeValues_StaticPointCollection( obj, &
       valuesMatrix ) result ( valuesArray )
    class(StaticPointCollectionInterface), intent(in) :: obj
    real(FLOAT), dimension(:, :), intent(in) :: valuesMatrix
    real(FLOAT), dimension(size(valuesMatrix, 2)) :: valuesArray
    
    ! delegate
    valuesArray = obj%pointArrangement%convertToPolytopeValues( &
         valuesMatrix )
  end function convertToPolytopeValues_StaticPointCollection

  
  !-------------------------------------------------------------------
  !- InteriorStaticPointCollection methods
  !-------------------------------------------------------------------

  EXPAND({PROCEDURE_DESTROY(InteriorStaticPointCollection)})
  
  EXPAND({METHODDEFS_SUBSTATICPOINTCOLLECTION({Interior})})

  
  !-------------------------------------------------------------------
  !- InteriorStaticPointGroupPointer methods
  !-------------------------------------------------------------------
  
  EXPAND({METHODDEFS_STATICPOINTGROUPPOINTER_CONCRETE({Interior},
  {Directed})})

  
  !-------------------------------------------------------------------
  !- BoundaryStaticPointCollection methods
  !-------------------------------------------------------------------

  EXPAND({PROCEDURE_DESTROY(BoundaryStaticPointCollection)})
  
  EXPAND({METHODDEFS_SUBSTATICPOINTCOLLECTION({Boundary})})

  
  !-------------------------------------------------------------------
  !- BoundaryStaticPointGroupPointer methods
  !-------------------------------------------------------------------
  
  EXPAND({METHODDEFS_STATICPOINTGROUPPOINTER_CONCRETE({Boundary},
  {Undirected})})

  subroutine getFaceNormals_BoundaryStaticPointGroupPointer( obj, &
       directions, log )
    class(BoundaryStaticPointGroupPointerType), intent(in) :: obj
    class(DirectionInterface), dimension(:), allocatable, intent(&
         out) :: directions
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getFaceNormals_&
         &BoundaryStaticPointGroupPointer', log )

    call obj%boundaryStaticPointCollection%getFaceNormals( &
         directions, obj%polytopeIndex, log )

    call endSub( log )
  end subroutine getFaceNormals_BoundaryStaticPointGroupPointer

  
  !-------------------------------------------------------------------
  !- MomentSource methods
  !-------------------------------------------------------------------

  EXPAND({METHODDEFS_MOMENTSOURCE({Scalar})})
  
  EXPAND({METHODDEFS_MOMENTSOURCE({Vector})})

  
  !-------------------------------------------------------------------
  !- MomentField methods
  !-------------------------------------------------------------------

  EXPAND({METHODDEFS_MOMENTFIELD({Scalar})})
  
  EXPAND({METHODDEFS_MOMENTFIELD({Vector})})
  
  
  !-------------------------------------------------------------------
  !- BoundaryCondition methods
  !-------------------------------------------------------------------

  EXPAND({METHODDEFS_BOUNDARYCONDITION({Scalar})})
  
  EXPAND({METHODDEFS_BOUNDARYCONDITION({Vector})})
  
  
  !-------------------------------------------------------------------
  !- MomentGroup methods
  !-------------------------------------------------------------------

  EXPAND({METHODDEFS_MOMENTGROUP({Scalar}, {MomentSource})})
  
  EXPAND({METHODDEFS_MOMENTGROUP({Vector}, {MomentSource})})

  EXPAND({METHODDEFS_CONTAINER_MOMENTGROUP({Scalar}, {MomentSource})})
  
  EXPAND({METHODDEFS_CONTAINER_MOMENTGROUP({Vector}, {MomentSource})})
  
  
  !-------------------------------------------------------------------
  !- RHSElement and MomentPointer methods
  !-------------------------------------------------------------------

  EXPAND({PROCEDURE_DESTROY(RHSElement)})
  
  EXPAND({PROCEDURE_DESTROY_ARRAY({RHSElement}, {Interface})})
  
  EXPAND({METHODDEFS_MOMENTPOINTER({Scalar})})
  
  EXPAND({METHODDEFS_MOMENTPOINTER({Vector})})

  !-------------------------------------------------------------------
  !- Matrix methods
  !-------------------------------------------------------------------
  
  EXPAND({METHODDEFS_MATRIX({Scalar})})
  
  EXPAND({METHODDEFS_MATRIX({Vector})})

  !-------------------------------------------------------------------
  !- RHSColumn methods
  !-------------------------------------------------------------------
  
  EXPAND({METHODDEFS_RHSCOLUMN({Scalar})})
  
  EXPAND({METHODDEFS_RHSCOLUMN({Vector})})

  !-------------------------------------------------------------------
  !- LinearSystem methods
  !-------------------------------------------------------------------
  
  EXPAND({METHODDEFS_LINEARSYSTEM({Scalar})})
  
  EXPAND({METHODDEFS_LINEARSYSTEM({Vector})})


  !-------------------------------------------------------------------
  !- LinearOperation methods
  !-------------------------------------------------------------------

  EXPAND({PROCEDURE_DESTROY(PureScalarLinearOperation)})

  EXPAND({PROCEDURE_DESTROY_ARRAY(PureScalarLinearOperation, {Interface})})
  
  EXPAND({PROCEDURE_DESTROY(ImpureScalarLinearOperation)})

  EXPAND({PROCEDURE_DESTROY(PureVectorLinearOperation)})

  EXPAND({PROCEDURE_DESTROY_ARRAY(PureVectorLinearOperation, {Interface})})
  
  EXPAND({PROCEDURE_DESTROY(ImpureVectorLinearOperation)})

  EXPAND({METHODDEFS_LINEAROPERATION({Scalar})})
  
  EXPAND({METHODDEFS_LINEAROPERATION({Vector})})

  
  !-------------------------------------------------------------------
  !- RHSGenerator methods
  !-------------------------------------------------------------------

  EXPAND({METHODDEFS_RHSGENERATOR_ABSTRACT({Scalar})})
  
  EXPAND({METHODDEFS_RHSGENERATOR_MOMGPBASED({Scalar})})
  
  EXPAND({METHODDEFS_RHSGENERATOR_LINOPBASED({Scalar})})

  EXPAND({METHODDEFS_RHSGENERATOR_ABSTRACT({Vector})})
  
  EXPAND({METHODDEFS_RHSGENERATOR_MOMGPBASED({Vector})})
  
  EXPAND({METHODDEFS_RHSGENERATOR_LINOPBASED({Vector})})
  
  !-------------------------------------------------------------------
  !- FieldSpecification methods
  !-------------------------------------------------------------------
  
  EXPAND({METHODDEFS_FIELDSPECIFICATION(Scalar)})
  
  EXPAND({METHODDEFS_FIELDSPECIFICATION(Vector)})
  
  EXPAND({METHODDEFS_CONTAINER(ScalarFieldSpecification, Interface, List)})

  EXPAND({METHODDEFS_CONTAINER(VectorFieldSpecification, Interface, List)})
  

end module FlowFieldModule
