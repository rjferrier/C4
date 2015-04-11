HEADER()
module FiniteVolumeModule
  
  USE_MACROS({Container})
  USE_MACROS({FiniteVolume})
  use LogModule
  use Global
  use FlowFieldModule
  use SemiLagrangianModule
  
  implicit none
  private
  
  public :: ptr, create, destroy
                                                                      
  character(*), parameter :: MOD_NAME = 'FiniteVolumeModule'

  
  interface create
     module procedure createFacePointerArray
     module procedure createCellPointerArray
  end interface

  
  interface destroy
     module procedure destroyIntAveScalarMomentField
     module procedure destroyIntAveVectorMomentField
     module procedure destroyIntAveScalarBoundaryCondition
     module procedure destroyIntAveVectorBoundaryCondition
     module procedure destroyPolytopeArray
     module procedure destroyPolytopeGrid
     module procedure destroyPolytopeComplex
     module procedure destroyPolytopeComplexArray
     module procedure destroyPolytopePointerArray
     module procedure destroyIntegrableElementPointer
     module procedure destroyFaceArray
     module procedure destroyFaceGrid
     module procedure destroyFaceComplex
     module procedure destroyFacePointerArray
     module procedure destroyCellArray
     module procedure destroyCellGrid
     module procedure destroyCellComplex
     module procedure destroyCellPointerArray
     module procedure destroyScalarProfile
     module procedure destroyScalarProfileArray
     module procedure destroyPureScalarProfileOperation
     module procedure destroyImpureScalarProfileOperation
     module procedure destroyPureVectorProfileOperation
     module procedure destroyImpureVectorProfileOperation
  end interface

  
  !> General Polytope/PolytopeCollection pointer function.
  !
  !> PolytopePointers require two arguments; one, a
  !> PolytopeCollection; two, an array index or grid address to the
  !> polytope of interest.  The resulting objects help initialise more
  !> complex polytope types.
  !
  !> PolytopeCollectionPointers are less functional than
  !> PolytopePointers and essentially allow the corresponding
  !> PolytopeCollections to be concatenated into arrays for input to
  !> other methods.
  interface ptr
     module procedure pointToPolytopeArrayElement
     module procedure pointToPolytopeGridElement
     module procedure pointToPolytopeArray
     module procedure pointToPolytopeGrid
     module procedure pointToCellCollectionElement
     module procedure pointToFaceCollectionElement
  end interface 
  
  interface assignment(=)
     module procedure assignPolytopePointer
     module procedure assignPolytopeArrayPointer
     module procedure assignPolytopeGridPointer
     module procedure assignFacePointer
     module procedure assignCellPointer
  end interface

  !-------------------------------------------------------------------
  !- IntAveMomentFields
  !-------------------------------------------------------------------

  EXPAND({TYPEDEF_INTAVEMOMENTFIELD({Scalar})})

  EXPAND({TYPEDEF_INTAVEMOMENTFIELD({Vector})})
    
  EXPAND({TYPEDEFS_CONTAINER(IntAveScalarMomentField, Interface, List)})
  
  EXPAND({TYPEDEFS_CONTAINER(IntAveVectorMomentField, Interface, List)})

  ! compare the above with PointMomentFields in SemiLagrangianModule.
  ! There are additionally PointBoundaryConditions.  By extension, there
  ! should be a group of classes headed up by IntAveBoundaryCondition.
  ! However, these are not needed yet.

  
  !-------------------------------------------------------------------
  !- IntAveBoundaryConditions
  !-------------------------------------------------------------------

  EXPAND({TYPEDEF_INTAVEBOUNDARYCONDITION({Scalar})})

  EXPAND({TYPEDEF_INTAVEBOUNDARYCONDITION({Vector})})

  EXPAND({TYPEDEFS_CONTAINER(IntAveScalarBoundaryCondition, Interface,
  List)})

  EXPAND({TYPEDEFS_CONTAINER(IntAveVectorBoundaryCondition, Interface,
  List)})
  
  
  !-------------------------------------------------------------------
  !- PolytopeCollectionInterface
  !-------------------------------------------------------------------
  
  type(IDManagerType) :: pcIDManager
  
  type, abstract, public :: PolytopeCollectionInterface
     private
     integer :: ID = 0
     integer :: polytopeDim = 0
   contains
     procedure(pc_di), deferred :: deinit
     procedure(pc_csrp), deferred :: collectSuperPolytopes
     procedure(pc_csp), deferred :: collectSubPolytopes
     procedure(pc_gc), deferred :: getCentroid
     procedure(pc_gs), deferred :: getSize
     procedure(pc_gnp), deferred :: getNumPolytopes
     procedure(pc_dp), deferred :: injectDeparturePoints
     procedure(pc_dp), deferred :: extractDeparturePoints
     procedure(pc_aum), deferred :: appendVelocityMoments
     procedure(pc_asm), deferred :: appendScalarMoments
     procedure(pc_wpl), deferred :: writePointLocations
     procedure(pc_wpc), deferred :: writePointConnectivity
     procedure(pc_ac), deferred :: appendConnection
     
     procedure :: initPolytopeCollection
     procedure :: deinitPolytopeCollection
     procedure :: sameAs => sameAs_PolytopeCollection
     procedure :: describe => describe_PolytopeCollection
     procedure :: getPolytopeDim => getPolytopeDim_PolytopeCollection
     
     EXPAND({SIGNATURELIST_POLYTOPECOLLECTION_EXTENSIONS({Collection})})
     generic  :: getCollectionExtension => &
          getCollectionExtension_Faces, getCollectionExtension_Cells
  end type PolytopeCollectionInterface

  
  !-------------------------------------------------------------------
  !- PolytopeArray branch
  !-------------------------------------------------------------------

  type, extends(PolytopeCollectionInterface), abstract, public :: PolytopeArrayInterface
     private
     integer :: arraySize = 0
   contains
     procedure(pa_csrp), deferred :: collectSuperPolytopes
     procedure(pa_csp), deferred :: collectSubPolytopes
     procedure(pa_gc), deferred :: getCentroid
     procedure(pa_gs), deferred :: getSize
     procedure(pa_ptspa), deferred :: pointToSubPolytopeArray
     procedure(pa_wpl), deferred :: writePointLocations
     procedure(pa_wpc), deferred :: writePointConnectivity
     procedure(pa_di), deferred :: deinit
     procedure :: initPolytopeArray
     procedure :: deinitPolytopeArray
     procedure :: getNumPolytopes => getNumPolytopes_PolytopeArray
     EXPAND({SIGNATURELIST_SUBPOLYTOPECOLLECTION_EXTENSIONS(
     {Array})})
     generic  :: setExtension => setExtension_Faces, &
          setExtension_Cells
     generic  :: getArrayExtension => getArrayExtension_Faces, &
          getArrayExtension_Cells
  end type PolytopeArrayInterface
  
    
  !> More like a data type than a class.  Its only purpose is to be
  !> able to hold references to the corresponding PolytopeCollection
  !> in an array.  Can also this type unassigned to represent null
  !> elements.  
  type, public :: PolytopeArrayPointerType
     private 
     ! the pointer 'get' method was disastrous.  The returned pointer
     ! became disassociated on return, leading to lots of segmentation
     ! faults.  To get round this, the embedded pointer has been
     ! exposed as a public property, and a boolean property is used to
     ! record at call time the true association status.  The bug might
     ! be due to the pointer component having the same name as its
     ! target.  Keep an eye on PolytopePointerType.  It is not yet
     ! safeguarded against such a bug.
     class(PolytopeArrayInterface), pointer, public :: pa => null()
     logical :: isAssociated = .false.
   contains
     procedure :: associated => associated_PolytopeArrayPointer
  end type PolytopeArrayPointerType

  
  !-------------------------------------------------------------------
  !- PolytopeGrid branch
  !-------------------------------------------------------------------
  
  type, extends(PolytopeCollectionInterface), abstract, public :: PolytopeGridInterface
     private
     type(GridParametersType) :: gridParameters
   contains
     procedure(pg_csrp), deferred :: collectSuperPolytopes
     procedure(pg_csp), deferred :: collectSubPolytopes
     procedure(pg_gc), deferred :: getCentroid
     procedure(pg_gs), deferred :: getSize
     procedure(pg_wpl), deferred :: writePointLocations
     procedure(pg_wpc), deferred :: writePointConnectivity
     procedure(pg_di), deferred :: deinit
     procedure :: initPolytopeGrid
     procedure :: deinitPolytopeGrid
     procedure :: getNumPolytopes => getNumPolytopes_PolytopeGrid
     procedure(pg_bu), deferred :: breakUp
     procedure(pg_ptx), deferred :: pointToComplex
     procedure(pg_gcc), deferred :: getCartesianCode
     procedure :: getGridParameters
     procedure :: convertToIndex => address2index_PolytopeGrid
     procedure :: convertToGridAddress => index2address_PolytopeGrid
     EXPAND({SIGNATURELIST_SUBPOLYTOPECOLLECTION_EXTENSIONS(
     {Grid})})
     generic  :: setExtension => setExtension_Faces, &
          setExtension_Cells
     generic  :: getGridExtension => getGridExtension_Faces, &
          getGridExtension_Cells
  end type PolytopeGridInterface

  
  !> See notes in PolytopeArrayPointerType
  type, public :: PolytopeGridPointerType
     private 
     class(PolytopeGridInterface), pointer, public :: pg => null()
     logical :: isAssociated = .false. 
   contains
     procedure :: associated => associated_PolytopeGridPointer
  end type PolytopeGridPointerType
  

  !-------------------------------------------------------------------
  !- PolytopePointer
  !-------------------------------------------------------------------

  type, public :: PolytopePointerType
     private
     class(PolytopeCollectionInterface), pointer :: pc => null()
     ! we chose not encapsulate the element address in a polymorphic type
     ! because we want to keep PolytopePointer lean.  As soon as
     ! polymorphic components are introduced, we need to deal with
     ! allocations and deallocations.  Then creating the pointer object
     ! becomes expensive and there is a risk of memory leakage.
     integer :: index = 0
     
   contains
     procedure :: initPolytopePointerFromPointer
     procedure :: initPolytopePointerFromArray
     procedure :: initPolytopePointerFromGrid
     procedure :: initPolytopePointerFromSet
     generic :: init => initPolytopePointerFromPointer,&
          initPolytopePointerFromArray, initPolytopePointerFromGrid, &
          initPolytopePointerFromSet
     procedure :: deinit => deinitPolytopePointer

     procedure :: sameAs => sameAs_PolytopePointer
     procedure :: describe => describePolytopePointer
     procedure :: collectSubPolytopes => &
          collectSubPolytopes_PolytopePointer
     procedure :: collectSuperPolytopes => &
          collectSuperPolytopes_PolytopePointer
     procedure :: getPolytopeDim => getPolytopeDim_PolytopePointer
     procedure :: match => match_Collection_PolytopePointer
     procedure :: getCentroid => getCentroid_PolytopePointer
     procedure :: getSize => getSize_PolytopePointer
     procedure :: appendVelocityMoments => &
          appendVelocityMoments_PolytopePointer
     procedure :: appendScalarMoments => &
          appendScalarMoments_PolytopePointer
     procedure :: appendConnection => appendConnection_PolytopePointer
     
     procedure :: getPointerExtension_Faces
     procedure :: getPointerExtension_Cells
     generic :: getPointerExtension => getPointerExtension_Faces, &
          getPointerExtension_Cells
  end type PolytopePointerType


  EXPAND({TYPEDEFS_CONTAINER(PolytopePointer, Type, Set, {}, {
     procedure :: init => initPolytopePointerSet
     procedure :: print => printPolytopePointerSet
     procedure :: addRecursively
     procedure :: query => query_Collection_PolytopePointerSet
     procedure :: filter_Dimension_PolytopePointerSet
     generic :: filter => filter_Dimension_PolytopePointerSet})})

  
  !-------------------------------------------------------------------
  !- PolytopeComplex
  !-------------------------------------------------------------------

  type, abstract, public :: PolytopeComplexInterface
     private
   contains
     procedure(px_gp_a), deferred :: getPolytopes_asArray
     procedure(px_gp_s), deferred :: getPolytopes_asSet
     generic :: getPolytopes => getPolytopes_asArray, getPolytopes_asSet
     procedure(px_di), deferred :: deinit
     
     EXPAND({SIGNATURELIST_POLYTOPEOTHER_EXTENSIONS({Complex})})
     generic  :: setExtension => setExtension_Faces, setExtension_Cells
     generic  :: getComplexExtension => getComplexExtension_Faces, &
          getComplexExtension_Cells
  end type PolytopeComplexInterface

  
  
  !-------------------------------------------------------------------
  !- IntegrableElement extensions
  !-------------------------------------------------------------------

  m4_divert(-1)
  m4_define({NAME_SUPER}, {IntegrableElement})
  m4_define({SIGNATURELIST_SUPERCOLLECTION}, {
     procedure($1_csr), deferred :: computeSubRow
     procedure($1_aum), deferred :: appendVelocityMoments
     procedure($1_asm), deferred :: appendScalarMoments})
  m4_define({SIGNATURELIST_SUPERPOINTER}, {
     procedure($1_c), deferred :: clone
     procedure($1_iae), deferred :: initAsExtension
     procedure($1_csr), deferred :: computeSubRow
     procedure($1_aum), deferred :: appendVelocityMoments
     procedure($1_asm), deferred :: appendScalarMoments})
  m4_define({SIGNATURELIST_SUPERCOMPLEX}, {
     procedure($1_aum), deferred :: appendVelocityMoments
     procedure($1_asm), deferred :: appendScalarMoments})
  m4_define({METHODLIST_SUPERPOINTER_SUBCLASS}, {
     procedure :: clone => clone_$1
     procedure :: initAsExtension => initAsExtension_$1
     procedure :: computeSubRow => computeSubRow_$1
     procedure :: appendVelocityMoments => &
          appendVelocityMoments_$1
     procedure :: appendScalarMoments => &
          appendScalarMoments_$1})
  m4_divert(0)
  EXPAND({TYPEDEF_POLYTOPEEXTENSION_SUPERCOLLECTION({IntegrableElement})})

  EXPAND({TYPEDEF_POLYTOPEEXTENSION_SUPERPOINTER({IntegrableElement})})

  EXPAND({TYPEDEF_POLYTOPEEXTENSION_SUPERCOMPLEX({IntegrableElement})})

  
  !-------------------------------------------------------------------
  !- Face extensions
  !-------------------------------------------------------------------

  m4_divert(-1)
  m4_define({SIGNATURELIST_COLLECTION}, {
     procedure(f$1_gfn), deferred :: getFaceNormal
     procedure(f$1_fs), deferred :: findSide})
  m4_define({METHODLIST_COLLECTION}, {})
  m4_define({SIGNATURELIST_ARRAY}, {})
  m4_define({SIGNATURELIST_GRID}, {})
  m4_define({METHODLIST_POINTER}, {
     procedure :: getFaceNormal => getFaceNormal_FacePointer
     procedure :: findSide => findSide_FacePointer})
  m4_define({SIGNATURELIST_COMPLEX}, {})
  m4_divert(0)
  
  EXPAND({TYPEDEFS_POLYTOPEEXTENSION({Face}, {IntegrableElement})})
     

  !-------------------------------------------------------------------
  !- Cell extensions
  !-------------------------------------------------------------------

  m4_divert(-1)
  m4_define({SIGNATURELIST_COLLECTION}, {
     procedure(c$1_isf), deferred :: initialiseScalarFields
     procedure(c$1_ivf), deferred :: initialiseVectorFields})
  m4_define({SIGNATURELIST_ARRAY}, {})
  m4_define({SIGNATURELIST_GRID}, {
     procedure(cg_c2a), deferred :: coords2address
     generic :: convert => coords2address
     procedure(cg_l), deferred :: locate
     procedure(cg_wcd), deferred :: writeCellData
!!$     procedure(cg_iap_s), deferred :: interpolateAtPoint_Scalar
!!$     procedure(cg_iap_v), deferred :: interpolateAtPoint_Vector})
!!$     generic :: interpolateAtPoint => &
!!$          interpolateAtPoint_Scalar, interpolateAtPoint_Vector})
  m4_define({SIGNATURELIST_COMPLEX}, {
     procedure(cx_l), deferred :: locate})
  m4_define({METHODLIST_POINTER})})
  m4_define({METHODLIST_COLLECTION}, {})
  m4_divert(0)
    
  EXPAND({TYPEDEFS_POLYTOPEEXTENSION({Cell}, {IntegrableElement})})
     
     
  !-------------------------------------------------------------------
  !- (end of extensions)
  !-------------------------------------------------------------------
  
  m4_divert(-1)
  ! reset sub-macros
  m4_define({NAME_SUPER}, {})
  m4_define({SIGNATURELIST_SUPERCOLLECTION}, {})
  m4_define({SIGNATURELIST_COLLECTION}, {})
  m4_define({SIGNATURELIST_ARRAY}, {})
  m4_define({SIGNATURELIST_GRID}, {})
  
  m4_define({METHODLIST_COLLECTION}, {})
  m4_define({SIGNATURELIST_SUPERPOINTER}, {})
  m4_define({METHODLIST_SUPERPOINTER_SUBCLASS}, {})
  m4_divert(0)

  !-------------------------------------------------------------------
  !- Profiles
  !-------------------------------------------------------------------

  EXPAND({TYPEDEFS_PROFILEOPERATION({Scalar})})
  
  EXPAND({TYPEDEFS_PROFILEOPERATION({Vector})})
  
  EXPAND({TYPEDEF_PROFILE({Scalar})})
  
  EXPAND({TYPEDEF_PROFILE({Vector})})

  
  !-------------------------------------------------------------------
  !- abstract interfaces
  !-------------------------------------------------------------------
  
  ! IntAveMomentField and subclass signatures
  abstract interface
     EXPAND({SIGNATUREDEFS_INTAVEMOMENTFIELD({Scalar})})
     
     EXPAND({SIGNATUREDEFS_INTAVEMOMENTFIELD({Vector})})
  end interface

    
  ! IntAveBoundaryCondition and subclass signatures
  abstract interface
     EXPAND({SIGNATUREDEFS_INTAVEBOUNDARYCONDITION({Scalar})})
     
     EXPAND({SIGNATUREDEFS_INTAVEBOUNDARYCONDITION({Vector})})
  end interface

  
  ! PolytopeCollection signatures
  abstract interface
     subroutine pc_di( obj, log )
       import PolytopeCollectionInterface, LogType
       class(PolytopeCollectionInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine pc_di

     pure recursive subroutine pc_csrp( obj, index, pps, log, height )
       import PolytopeCollectionInterface, PolytopePointerSetType, &
            LogType
       class(PolytopeCollectionInterface), intent(inout), target :: &
            obj
       integer, intent(in) :: index
       class(PolytopePointerSetType), intent(inout) :: pps
       class(LogType), intent(inout), optional :: log
       integer, intent(in), optional :: height
     end subroutine pc_csrp

     pure recursive subroutine pc_csp( obj, index, pps, log, depth )
       import PolytopeCollectionInterface, PolytopePointerSetType, &
            LogType
       class(PolytopeCollectionInterface), intent(inout), target :: &
            obj
       integer, intent(in) :: index
       class(PolytopePointerSetType), intent(inout) :: pps
       class(LogType), intent(inout), optional :: log
       integer, intent(in), optional :: depth
     end subroutine pc_csp

     pure subroutine pc_gc( obj, index, coords, log )
       import PolytopeCollectionInterface, RealVectorType, &
            LogType
       class(PolytopeCollectionInterface), intent(in) :: obj
       integer, intent(in) :: index
       type(RealVectorType), intent(out)  :: coords
       class(LogType), intent(inout), optional :: log
     end subroutine pc_gc
     
     pure subroutine pc_gs( obj, index, size, log )
       import PolytopeCollectionInterface, FLOAT, LogType
       class(PolytopeCollectionInterface), intent(inout) :: obj
       integer, intent(in) :: index
       real(FLOAT), intent(out)  :: size
       class(LogType), intent(inout), optional :: log
     end subroutine pc_gs

     pure function pc_gnp( obj )
       import PolytopeCollectionInterface
       class(PolytopeCollectionInterface), intent(in) :: obj
       integer :: pc_gnp
     end function pc_gnp
     
     subroutine pc_dp( obj, departurePoints, log )
       import PolytopeCollectionInterface, &
            DeparturePointCollectionInterface, LogType
       class(PolytopeCollectionInterface), intent(inout) :: obj
       class(DeparturePointCollectionInterface), allocatable, intent(&
            inout) :: departurePoints
       class(LogType), intent(inout), optional :: log
     end subroutine pc_dp
     
     subroutine pc_aum( obj, velocityMomentGroupList, polytopeIndex, &
          log )
       import PolytopeCollectionInterface, &
            VectorMomentGroupListType, FlowVariableInterface, LogType
       class(PolytopeCollectionInterface), intent(inout) :: obj
       type(VectorMomentGroupListType), intent(inout) :: &
            velocityMomentGroupList
       integer, intent(in) :: polytopeIndex
       class(LogType), intent(inout), optional :: log
     end subroutine pc_aum
     
     subroutine pc_asm( obj, scalarMomentGroupList, polytopeIndex, log )
       import PolytopeCollectionInterface, &
            ScalarMomentGroupListType, &
            FlowVariableInterface, LogType
       class(PolytopeCollectionInterface), intent(inout) :: obj
       type(ScalarMomentGroupListType), intent(inout) :: &
            scalarMomentGroupList
       integer, intent(in) :: polytopeIndex
       class(LogType), intent(inout), optional :: log
     end subroutine pc_asm
     
     subroutine pc_wpl( obj, stringList, nPointsRunning, precision, &
          indent, log )
       import PolytopeCollectionInterface, StringListType, LogType
       class(PolytopeCollectionInterface), intent(inout) :: obj
       type(StringListType), intent(inout) :: stringList
       integer, intent(inout) :: nPointsRunning
       integer, intent(in), optional :: precision, indent
       class(LogType), intent(inout), optional :: log
     end subroutine pc_wpl
     
     subroutine pc_wpc( obj, connectivityList, offsetList, &
          nPolytopesRunning, startFromZero, indent, log )
       import PolytopeCollectionInterface, StringListType, LogType
       class(PolytopeCollectionInterface), intent(inout) :: obj
       type(StringListType), intent(inout) :: connectivityList, offsetList
       integer, intent(inout) :: nPolytopesRunning
       logical, intent(in) :: startFromZero
       integer, intent(in), optional :: indent
       class(LogType), intent(inout), optional :: log
     end subroutine pc_wpc
     
     pure subroutine pc_ac( obj, string, polytopeIndex, startFromZero, &
          indent )
       import PolytopeCollectionInterface
       class(PolytopeCollectionInterface), intent(in) :: obj
       character(*), intent(inout) :: string
       integer, intent(in)  :: polytopeIndex
       logical, intent(in)  :: startFromZero
       integer, intent(in), optional :: indent
     end subroutine pc_ac
     
     EXPAND({SIGNATUREDEFS_POLYTOPECOLLECTION_EXTENSIONS({Collection})})
  end interface
     
  
  ! PolytopeArray signatures
  abstract interface
     pure recursive subroutine pa_csrp( obj, index, pps, log, height )
       import PolytopeArrayInterface, PolytopePointerSetType, &
            LogType
       class(PolytopeArrayInterface), intent(inout), target :: &
            obj
       integer, intent(in) :: index
       class(PolytopePointerSetType), intent(inout) :: pps
       class(LogType), intent(inout), optional :: log
       integer, intent(in), optional :: height
     end subroutine pa_csrp

     pure recursive subroutine pa_csp( obj, index, pps, log, depth )
       import PolytopeArrayInterface, PolytopePointerSetType, &
            LogType
       class(PolytopeArrayInterface), intent(inout), target :: &
            obj
       integer, intent(in) :: index
       class(PolytopePointerSetType), intent(inout) :: pps
       class(LogType), intent(inout), optional :: log
       integer, intent(in), optional :: depth
     end subroutine pa_csp
     
     pure subroutine pa_gc( obj, index, coords, log )
       import PolytopeArrayInterface, RealVectorType, LogType
       class(PolytopeArrayInterface), intent(in) :: obj
       integer, intent(in) :: index
       type(RealVectorType), intent(out)  :: coords
       class(LogType), intent(inout), optional :: log
     end subroutine pa_gc
     
     pure subroutine pa_gs( obj, index, size, log )
       import PolytopeArrayInterface, FLOAT, LogType
       class(PolytopeArrayInterface), intent(inout) :: obj
       integer, intent(in) :: index
       real(FLOAT), intent(out)  :: size
       class(LogType), intent(inout), optional :: log
     end subroutine pa_gs

     subroutine pa_ptspa( obj, ptr )
       import PolytopeArrayInterface
       class(PolytopeArrayInterface), intent(in) :: obj
       class(PolytopeArrayInterface), pointer, intent(out) :: ptr
     end subroutine pa_ptspa
     
     subroutine pa_wpl( obj, stringList, nPointsRunning, precision, &
          indent, log )
       import PolytopeArrayInterface, StringListType, LogType
       class(PolytopeArrayInterface), intent(inout) :: obj
       type(StringListType), intent(inout) :: stringList
       integer, intent(inout) :: nPointsRunning
       integer, intent(in), optional :: precision, indent
       class(LogType), intent(inout), optional :: log
     end subroutine pa_wpl
     
     subroutine pa_wpc( obj, connectivityList, offsetList, &
          nPolytopesRunning, startFromZero, indent, log )
       import PolytopeArrayInterface, StringListType, LogType
       class(PolytopeArrayInterface), intent(inout) :: obj
       type(StringListType), intent(inout) :: connectivityList, offsetList
       integer, intent(inout) :: nPolytopesRunning
       logical, intent(in) :: startFromZero
       integer, intent(in), optional :: indent
       class(LogType), intent(inout), optional :: log
     end subroutine pa_wpc

     pure subroutine pa_ac( obj, string, polytopeIndex, startFromZero, &
          indent )
       import PolytopeArrayInterface
       class(PolytopeArrayInterface), intent(in) :: obj
       character(*), intent(inout) :: string
       integer, intent(in)  :: polytopeIndex
       logical, intent(in)  :: startFromZero
       integer, intent(in), optional :: indent
     end subroutine pa_ac

     subroutine pa_di( obj, log )
       import PolytopeArrayInterface, LogType
       class(PolytopeArrayInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine pa_di

     EXPAND({SIGNATUREDEFS_SUBPOLYTOPECOLLECTION_EXTENSIONS({Array})})
  end interface


  ! PolytopeGrid signatures
  abstract interface
     pure recursive subroutine pg_csrp( obj, index, pps, log, height )
       import PolytopeGridInterface, PolytopePointerSetType, LogType
       class(PolytopeGridInterface), intent(inout), target :: obj
       integer, intent(in) :: index
       class(PolytopePointerSetType), intent(inout) :: pps
       class(LogType), intent(inout), optional :: log
       integer, intent(in), optional :: height
     end subroutine pg_csrp
     
     pure recursive subroutine pg_csp( obj, index, pps, log, depth )
       import PolytopeGridInterface, PolytopePointerSetType, LogType
       class(PolytopeGridInterface), intent(inout), target :: obj
       integer, intent(in) :: index
       class(PolytopePointerSetType), intent(inout) :: pps
       class(LogType), intent(inout), optional :: log
       integer, intent(in), optional :: depth
     end subroutine pg_csp
     
     pure subroutine pg_gc( obj, index, coords, log )
       import PolytopeGridInterface, RealVectorType, LogType
       class(PolytopeGridInterface), intent(in) :: obj
       integer, intent(in) :: index
       type(RealVectorType), intent(out)  :: coords
       class(LogType), intent(inout), optional :: log
     end subroutine pg_gc
     
     pure subroutine pg_gs( obj, index, size, log )
       import PolytopeGridInterface, FLOAT, LogType
       class(PolytopeGridInterface), intent(inout) :: obj
       integer, intent(in) :: index
       real(FLOAT), intent(out)  :: size
       class(LogType), intent(inout), optional :: log
     end subroutine pg_gs
     
     pure function pg_gcc( obj ) result ( cartesianCode )
       import PolytopeGridInterface, LogicalVectorType
       class(PolytopeGridInterface), intent(in) :: obj
       type(LogicalVectorType) :: cartesianCode
     end function pg_gcc
     
     subroutine pg_bu( obj, gridAddresses, polytopeComplexes, &
          log )
       import PolytopeGridInterface, PolytopeComplexInterface, &
            IntVectorType, LogType
       class(PolytopeGridInterface), intent(inout) :: obj
       type(IntVectorType), dimension(:), intent(in) :: gridAddresses
       class(PolytopeComplexInterface), dimension(:), target, &
            intent(in) :: polytopeComplexes
       class(LogType), intent(inout), optional :: log
     end subroutine pg_bu

     pure subroutine pg_ptx( obj, gridAddress, polytopeComplex, &
          log )
       import PolytopeGridInterface, IntVectorType, &
            PolytopeComplexInterface, LogType
       class(PolytopeGridInterface), intent(inout) :: obj
       type(IntVectorType), intent(in) :: gridAddress
       class(PolytopeComplexInterface), pointer, intent(out) :: &
            polytopeComplex
       class(LogType), intent(inout), optional :: log
     end subroutine pg_ptx
     
     subroutine pg_wpl( obj, stringList, nPointsRunning, precision, &
          indent, log )
       import PolytopeGridInterface, StringListType, LogType
       class(PolytopeGridInterface), intent(inout) :: obj
       type(StringListType), intent(inout) :: stringList
       integer, intent(inout) :: nPointsRunning
       integer, intent(in), optional :: precision, indent
       class(LogType), intent(inout), optional :: log
     end subroutine pg_wpl
     
     subroutine pg_wpc( obj, connectivityList, offsetList, &
          nPolytopesRunning, startFromZero, indent, log )
       import PolytopeGridInterface, StringListType, LogType
       class(PolytopeGridInterface), intent(inout) :: obj
       type(StringListType), intent(inout) :: connectivityList, offsetList
       integer, intent(inout) :: nPolytopesRunning
       logical, intent(in) :: startFromZero
       integer, intent(in), optional :: indent
       class(LogType), intent(inout), optional :: log
     end subroutine pg_wpc

     pure subroutine pg_ac( obj, string, polytopeIndex, startFromZero, &
          indent )
       import PolytopeGridInterface
       class(PolytopeGridInterface), intent(in) :: obj
       character(*), intent(inout) :: string
       integer, intent(in)  :: polytopeIndex
       logical, intent(in)  :: startFromZero
       integer, intent(in), optional :: indent
     end subroutine pg_ac
     
     subroutine pg_di( obj, log )
       import PolytopeGridInterface, LogType
       class(PolytopeGridInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine pg_di
     
     EXPAND({SIGNATUREDEFS_SUBPOLYTOPECOLLECTION_EXTENSIONS({Grid})})
  end interface


  ! PolytopeComplex signatures
  abstract interface
     subroutine px_gp_a( obj, depth, ppArray, log )
       import PolytopeComplexInterface, PolytopePointerType, &
            LogType
       class(PolytopeComplexInterface), intent(in), target :: obj
       integer, intent(in) :: depth
       type(PolytopePointerType), dimension(:), allocatable, intent(&
            out) :: ppArray
       class(LogType), intent(inout), optional :: log
     end subroutine px_gp_a
     
     subroutine px_gp_s( obj, depth, ppSet, log )
       import PolytopeComplexInterface, PolytopePointerSetType, &
            LogType
       class(PolytopeComplexInterface), intent(in), target :: obj
       integer, intent(in) :: depth
       type(PolytopePointerSetType), intent(out) :: ppSet
       class(LogType), intent(inout), optional :: log
     end subroutine px_gp_s

     subroutine px_di( obj, log )
       import PolytopeComplexInterface, LogType
       class(PolytopeComplexInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine px_di
     
     EXPAND({SIGNATUREDEFS_POLYTOPEOTHER_EXTENSIONS({Complex})})
  end interface


  ! IntegrableElementCollection signatures
  abstract interface
     m4_define({SIGNATUREDEFS_SUPERCOLLECTION}, {
     pure function ABB($1$2)_csr( obj, polytopeIndex, &
          directedSpatialDerivative ) result ( rows )
       import $1$2Interface, &
            DirectedSpatialDerivativeInterface, FLOAT, NCOEFS_ADV
       class($1$2Interface), intent(in) :: obj
       integer, intent(in) :: polytopeIndex
       class(DirectedSpatialDerivativeInterface), intent(in) :: &
            directedSpatialDerivative
       real(FLOAT), dimension(1, NCOEFS_ADV) :: rows
     end function ABB($1$2)_csr

     EXPAND({SIGNATUREDEF_APPENDMOMENTS({Scalar}, {$1$2}, {Interface},
     {polytopeIndex, }, {
       integer, intent(in) :: polytopeIndex})})
       
     EXPAND({SIGNATUREDEF_APPENDVELOCITYMOMENTS({$1$2}, {Interface},
     {polytopeIndex, }, {
       integer, intent(in) :: polytopeIndex})})
     }) m4_dnl
     
     EXPAND({SIGNATUREDEFS_POLYTOPEEXTENSION_SUPERCOLLECTION(
     {IntegrableElement}, {Collection})})
  end interface
  
  
  ! IntegrableElementPointer signatures
  abstract interface
     m4_define({SIGNATUREDEFS_SUPERPOINTER}, {
     subroutine ABB($1){}p_c( obj, LWR($1)Pointer, log )
       import $1Pointer$2, IntegrableElementPointerInterface, LogType
       class($1Pointer$2), intent(in) :: obj
       class(IntegrableElementPointerInterface), allocatable, &
            intent(out) :: LWR($1)Pointer
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1){}p_c
     
     pure subroutine ABB($1){}p_iae( obj, polytopePointer, log )
       import $1Pointer$2, PolytopePointerType, LogType
       class($1Pointer$2), intent(inout) :: obj
       class(PolytopePointerType), intent(in) :: polytopePointer
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1){}p_iae
     
     pure function ABB($1){}p_csr( obj, directedSpatialDerivative ) &
          result ( row )
       import $1Pointer$2, &
            DirectedSpatialDerivativeInterface, FLOAT, NCOEFS_ADV
       class($1Pointer$2), intent(in) :: obj
       class(DirectedSpatialDerivativeInterface), intent(in) :: &
            directedSpatialDerivative
       real(FLOAT), dimension(1, NCOEFS_ADV) :: row
     end function ABB($1){}p_csr
     
     EXPAND({SIGNATUREDEF_APPENDMOMENTS({Scalar}, {$1Pointer}, {$2})})
     
     EXPAND({SIGNATUREDEF_APPENDVELOCITYMOMENTS({$1Pointer}, {$2})})
     }) m4_dnl

     EXPAND({SIGNATUREDEFS_POLYTOPEEXTENSION_SUPERPOINTER(
     {IntegrableElement}, {Interface})})
  end interface

    
  ! IntegrableElementComplex signatures
  abstract interface
     m4_define({SIGNATUREDEFS_SUPERCOMPLEX}, {
     EXPAND({SIGNATUREDEF_APPENDMOMENTS({Scalar}, {$1Complex},
     {Interface})})
     
     EXPAND({SIGNATUREDEF_APPENDVELOCITYMOMENTS({$1Complex},
     {Interface})})
     })
     
     EXPAND({SIGNATUREDEFS_POLYTOPEEXTENSION_SUPERCOMPLEX(
     {IntegrableElement}, {Interface})})
  end interface

  
  ! FaceCollection/etc signatures 
  abstract interface
     m4_define({SIGNATUREDEFS_COLLECTION}, {
     pure subroutine f{}ABB($1)_gfn( obj, index, vector, log )
       import Face$1Interface, FLOAT, NDIM, RealVectorType, &
            LogType
       class(Face$1Interface), intent(in) :: obj
       integer, intent(in) :: index
       type(RealVectorType), intent(out) :: vector
       class(LogType), intent(inout), optional :: log
     end subroutine f{}ABB($1)_gfn
     
     elemental subroutine f{}ABB($1)_fs( obj, index, coords, &
          side, log )
       import Face$1Interface, FLOAT, NDIM, RealVectorType, &
            LogType
       class(Face$1Interface), intent(inout) :: obj
       integer, intent(in) :: index
       type(RealVectorType), intent(in) :: coords
       logical, intent(out) :: side
       class(LogType), intent(inout), optional :: log
     end subroutine f{}ABB($1)_fs
     })
     EXPAND({SIGNATUREDEFS_POLYTOPEEXTENSION_COLLECTION({Face},
     {Collection})})

     EXPAND({SIGNATUREDEFS_POLYTOPEEXTENSION_COLLECTION({Face},
     {Array})})

     EXPAND({SIGNATUREDEFS_POLYTOPEEXTENSION_COLLECTION({Face},
     {Grid})})
     
     m4_define({SIGNATUREDEFS_COMPLEX}, {
     pure subroutine fx_gfn( obj, vector, log )
       import FaceComplexInterface, RealVectorType, LogType
       class(FaceComplexInterface), intent(in) :: obj
       type(RealVectorType), intent(out) :: vector
       class(LogType), intent(inout), optional :: log
     end subroutine fx_gfn
     
     elemental subroutine fx_fs( obj, coords, side, log )
       import FaceComplexInterface, RealVectorType, LogType
       class(FaceComplexInterface), intent(inout) :: obj
       type(RealVectorType), intent(in) :: coords
       logical, intent(out) :: side
       class(LogType), intent(inout), optional :: log
     end subroutine fx_fs})
     EXPAND({SIGNATUREDEFS_POLYTOPEEXTENSION_COMPLEX({Face})})
  end interface

  
  ! CellCollection/etc signatures 
  abstract interface
     m4_define({SIGNATUREDEFS_COLLECTION}, {
     EXPAND({SIGNATUREDEF_INITFIELDS({Scalar}, {Cell$1})})
     
     EXPAND({SIGNATUREDEF_INITFIELDS({Vector}, {Cell$1})})
     })
     
     EXPAND({SIGNATUREDEFS_POLYTOPEEXTENSION_COLLECTION({Cell},
     {Collection})})

     EXPAND({SIGNATUREDEFS_POLYTOPEEXTENSION_COLLECTION({Cell},
     {Array})})

     EXPAND({SIGNATUREDEFS_POLYTOPEEXTENSION_COLLECTION({Cell},
     {Grid})})
     
     elemental subroutine cg_c2a( obj, coords, gridAddress, &
          relationToGrid )
       import CellGridInterface, RealVectorType, IntVectorType
       class(CellGridInterface), intent(inout) :: obj
       type(RealVectorType), intent(in) :: coords
       type(IntVectorType), intent(out) :: gridAddress
       type(IntVectorType), intent(out), optional :: relationToGrid
     end subroutine cg_c2a

     elemental subroutine cg_l( obj, coords, pp, log )
       import CellGridInterface, FLOAT, NDIM, &
            PolytopePointerType, RealVectorType, LogType
       class(CellGridInterface), intent(inout) :: obj
       type(RealVectorType), intent(in) :: coords
       type(PolytopePointerType), intent(out) :: pp
       class(LogType), intent(inout), optional :: log
     end subroutine cg_l

     subroutine cg_wcd( obj, stringList, log )
       import CellGridInterface, StringListType, LogType
       class(CellGridInterface), intent(inout) :: obj
       type(StringListType), intent(in) :: stringList 
       class(LogType), intent(inout), optional :: log
     end subroutine cg_wcd

     m4_define({SIGNATUREDEF_CELLGRID_INTERPOLATEATPOINT}, {
!!$     elemental function cg_iap_{}ABB($1)( obj, coords, &
!!$          LWR($1)FlowVariable ) result ( value )
!!$       import CellGridInterface, RealVectorType, &
!!$            IMPORT_TENSOR_PRIMITIVE($1), $1FlowVariableInterface
!!$       class(CellGridInterface), intent(in) :: obj
!!$       type(RealVectorType), intent(in) :: coords
!!$       class($1FlowVariableInterface), intent(in) :: LWR($1)FlowVariable
!!$       TYPE_TENSOR_PRIMITIVE($1) :: value
!!$     end function cg_iap_{}ABB($1)})

     SIGNATUREDEF_CELLGRID_INTERPOLATEATPOINT(Scalar)
     
     SIGNATUREDEF_CELLGRID_INTERPOLATEATPOINT(Vector)
     
     m4_define({SIGNATUREDEFS_COMPLEX}, {
     elemental subroutine cx_l( obj, coords, pp, log )
       import CellComplexInterface, FLOAT, NDIM, &
            PolytopePointerType, RealVectorType, LogType
       class(CellComplexInterface), intent(inout) :: obj
       type(RealVectorType), intent(in) :: coords
       type(PolytopePointerType), intent(out) :: pp
       class(LogType), intent(inout), optional :: log
     end subroutine cx_l})
     EXPAND({SIGNATUREDEFS_POLYTOPEEXTENSION_COMPLEX({Cell})})
  end interface

  m4_divert(-1)
  ! reset sub-macros
  m4_define({SIGNATUREDEFS_SUPERCOLLECTION}, {})
  m4_define({SIGNATUREDEFS_SUPERPOINTER}, {})
  m4_define({SIGNATUREDEFS_SUPERCOMPLEX}, {})
  m4_define({SIGNATUREDEFS_COLLECTION}, {})
  m4_define({SIGNATUREDEFS_POINTER}, {})
  m4_define({SIGNATUREDEFS_COMPLEX}, {})
  m4_divert(0)
  
  ! Profile signatures
  abstract interface
     EXPAND({SIGNATUREDEFS_PROFILE({Scalar})})
     
     EXPAND({SIGNATUREDEFS_PRIMITIVEPROFILE({Scalar})})
     
     EXPAND({SIGNATUREDEFS_COMPLEXPROFILE({Scalar})})
     
     EXPAND({SIGNATUREDEFS_PROFILE({Vector})})
  end interface
  
  
  ! ProfileOperation signatures
  abstract interface
     EXPAND({SIGNATUREDEFS_PROFILEOPERATION({Scalar})})
     
     EXPAND({SIGNATUREDEFS_SUBPROFILEOPERATIONS({Scalar})})

     EXPAND({SIGNATUREDEFS_PROFILEOPERATION({Vector})})

     EXPAND({SIGNATUREDEFS_SUBPROFILEOPERATIONS({Vector})})
  end interface

  
contains


  !-------------------------------------------------------------------
  !- unbound procedures
  !-------------------------------------------------------------------

  function pointToPolytopeArrayElement( pa, arrayIndex ) &
       result (pp)
    class(PolytopeArrayInterface), intent(in), target :: pa
    integer, intent(in) :: arrayIndex
    type(PolytopePointerType) :: pp
    pp%pc => pa
    pp%index = arrayIndex
  end function pointToPolytopeArrayElement


  function pointToPolytopeGridElement( pg, gridAddress ) &
       result (pp)
    class(PolytopeGridInterface), intent(in), target :: pg
    integer, dimension(NDIM), intent(in) :: gridAddress
    type(PolytopePointerType) :: pp
    pp%pc => pg
    pp%index = pg%convertToIndex( vector(gridAddress) )
  end function pointToPolytopeGridElement


  function pointToPolytopeArray( pa ) result (pap)
    class(PolytopeArrayInterface), intent(in), target :: pa
    type(PolytopeArrayPointerType) :: pap
    pap%pa => pa
    pap%isAssociated = associated(pap%pa)
  end function pointToPolytopeArray


  function pointToPolytopeGrid( pg ) result (pgp)
    class(PolytopeGridInterface), intent(in), target :: pg
    type(PolytopeGridPointerType) :: pgp
    pgp%pg => pg
    pgp%isAssociated = associated(pgp%pg)
  end function pointToPolytopeGrid


  subroutine assignPolytopePointer( ppTgt, ppSrc )
    type(PolytopePointerType), intent(out) :: ppTgt
    type(PolytopePointerType), intent(in) :: ppSrc
    ppTgt%pc => ppSrc%pc
    ppTgt%index = ppSrc%index
  end subroutine assignPolytopePointer


  subroutine assignPolytopeArrayPointer( papTgt, papSrc )
    type(PolytopeArrayPointerType), intent(out) :: papTgt
    type(PolytopeArrayPointerType), intent(in) :: papSrc
    papTgt%pa => papSrc%pa
    papTgt%isAssociated = associated(papTgt%pa)
  end subroutine assignPolytopeArrayPointer


  subroutine assignPolytopeGridPointer( pgpTgt, pgpSrc )
    type(PolytopeGridPointerType), intent(out) :: pgpTgt
    type(PolytopeGridPointerType), intent(in) :: pgpSrc
    pgpTgt%pg => pgpSrc%pg
    pgpTgt%isAssociated = associated(pgpTgt%pg)
  end subroutine assignPolytopeGridPointer


  !-------------------------------------------------------------------
  !- IntAveMomentField methods
  !-------------------------------------------------------------------

  EXPAND({METHODDEFS_INTAVEMOMENTFIELD({Scalar})})

  EXPAND({METHODDEFS_INTAVEMOMENTFIELD({Vector})})

  EXPAND({METHODDEFS_CONTAINER({IntAveScalarMomentField}, {Interface},
  {List})})
  
  EXPAND({METHODDEFS_CONTAINER({IntAveVectorMomentField}, {Interface},
  {List})})
  
  
  !-------------------------------------------------------------------
  !- IntAveBoundaryCondition methods
  !-------------------------------------------------------------------

  EXPAND({METHODDEFS_INTAVEBOUNDARYCONDITION({Scalar})})

  EXPAND({METHODDEFS_INTAVEBOUNDARYCONDITION({Vector})})

  EXPAND({METHODDEFS_CONTAINER({IntAveScalarBoundaryCondition}, {Interface},
  {List})})
  
  EXPAND({METHODDEFS_CONTAINER({IntAveVectorBoundaryCondition}, {Interface},
  {List})})


  !-------------------------------------------------------------------
  !- PolytopeCollectionInterface methods
  !-------------------------------------------------------------------

  ! to be invoked at the end of subclasses' init routines.
  subroutine initPolytopeCollection( obj, polytopeDim, log )
    class(PolytopeCollectionInterface), intent(inout) :: obj
    integer, intent(in) :: polytopeDim
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'initPolytopeCollection', log )

    call pcIDManager%requestID( obj%ID, log )
    obj%polytopeDim = polytopeDim

    call endSub( log )
  end subroutine initPolytopeCollection


  ! should be invoked at the start of subclasses' deinit routines
  subroutine deinitPolytopeCollection( obj, log )
    class(PolytopeCollectionInterface), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinitPolytopeCollection', log )

    obj%polytopeDim = 0
    call pcIDManager%returnID( obj%ID, log )
       
    call endSub( log )
  end subroutine deinitPolytopeCollection


  pure function describe_PolytopeCollection( obj )
    class(PolytopeCollectionInterface), intent(in) :: obj
    character(30) :: describe_PolytopeCollection
    describe_PolytopeCollection = 'collection '//str(obj%ID)
  end function describe_PolytopeCollection


  pure function sameAs_PolytopeCollection( obj, pc )
    class(PolytopeCollectionInterface), intent(in) :: obj
    class(PolytopeCollectionInterface), intent(in) :: pc
    logical :: sameAs_PolytopeCollection
    sameAs_PolytopeCollection = ( obj%ID == pc%ID )
  end function sameAs_PolytopeCollection


  pure function getPolytopeDim_PolytopeCollection( obj ) &
       result ( getPolytopeDim )
    class(PolytopeCollectionInterface), intent(in) :: obj
    integer :: getPolytopeDim
    getPolytopeDim = obj%polytopeDim
  end function getPolytopeDim_PolytopeCollection



  !-------------------------------------------------------------------
  !- PolytopeArrayInterface methods
  !-------------------------------------------------------------------

  EXPAND({PROCEDURE_DESTROY({PolytopeArray})})

  ! to be invoked at the end of subclasses' init routines.
  subroutine initPolytopeArray( obj, arraySize, polytopeDim, log )
    class(PolytopeArrayInterface), intent(inout) :: obj
    integer, intent(in) :: arraySize
    integer, intent(in) :: polytopeDim
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'initPolytopeArray', log )

    obj%arraySize = arraySize
    call obj%initPolytopeCollection( polytopeDim, log )

    call endSub( log )
  end subroutine initPolytopeArray


  ! to be invoked at the start of subclasses' deinit routines
  subroutine deinitPolytopeArray( obj, log )
    class(PolytopeArrayInterface), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinitPolytopeArray', log )

    obj%arraySize = 0
    call obj%deinitPolytopeCollection( log )

    call endSub( log )
  end subroutine deinitPolytopeArray

  
  pure function getNumPolytopes_PolytopeArray( obj )
    class(PolytopeArrayInterface), intent(in) :: obj
    integer :: getNumPolytopes_PolytopeArray
    getNumPolytopes_PolytopeArray = obj%arraySize
  end function getNumPolytopes_PolytopeArray


  !-------------------------------------------------------------------
  !- PolytopeGridInterface methods
  !-------------------------------------------------------------------

  EXPAND({PROCEDURE_DESTROY({PolytopeGrid})})

  ! to be invoked at the end of subclasses' init routines.
  subroutine initPolytopeGrid( obj, gridParameters, polytopeDim, &
       log )
    class(PolytopeGridInterface), intent(inout) :: obj
    type(GridParametersType), intent(in) :: gridParameters
    integer, intent(in) :: polytopeDim
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'initPolytopeGrid', log )

    obj%gridParameters = gridParameters
    call obj%initPolytopeCollection( polytopeDim, log )

    call endSub( log )
  end subroutine initPolytopeGrid


  ! to be invoked at the start of subclasses' deinit routines
  subroutine deinitPolytopeGrid( obj, log )
    class(PolytopeGridInterface), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinitPolytopeGrid', log )

    obj%gridParameters%size = 0
    call obj%deinitPolytopeCollection( log )

    call endSub( log )
  end subroutine deinitPolytopeGrid


  pure function getNumPolytopes_PolytopeGrid( obj )
    class(PolytopeGridInterface), intent(in) :: obj
    integer :: getNumPolytopes_PolytopeGrid
    getNumPolytopes_PolytopeGrid = product( obj%gridParameters%size )
  end function getNumPolytopes_PolytopeGrid


  pure function getGridParameters( obj )
    class(PolytopeGridInterface), intent(in) :: obj
    type(GridParametersType) :: getGridParameters
    getGridParameters = obj%gridParameters
  end function getGridParameters


  !> converts a multidimensional array address into the 1D array index
  !> implied by Fortran array element ordering.
  pure function address2index_PolytopeGrid( obj, gridAddress )
    class(PolytopeGridInterface), intent(in) :: obj
    type(IntVectorType), intent(in) :: gridAddress
    integer :: address2index_PolytopeGrid
    ! delegate
    address2index_PolytopeGrid = &
         obj%gridParameters%size%address2index( gridAddress )
  end function address2index_PolytopeGrid


  !> converts a 1D array index into the multidimensional array address
  !> implied by Fortran array element ordering.
  pure function index2address_PolytopeGrid( obj, index )
    class(PolytopeGridInterface), intent(in) :: obj
    integer, intent(in) :: index
    type(IntVectorType) :: index2address_PolytopeGrid
    ! delegate
    index2address_PolytopeGrid = &
         obj%gridParameters%size%index2address( index )
  end function index2address_PolytopeGrid



  !-------------------------------------------------------------------
  !- PolytopePointer procedures 
  !-------------------------------------------------------------------

  EXPAND({PROCEDURE_DESTROY_ARRAY({PolytopePointer}, {Type})})
  
  pure subroutine initPolytopePointerFromPointer( obj, pp )
    class(PolytopePointerType), intent(out) :: obj
    class(PolytopePointerType), intent(inout), target :: pp
    obj%pc => pp%pc
    obj%index = pp%index
  end subroutine initPolytopePointerFromPointer


  pure subroutine initPolytopePointerFromArray( obj, pa, index )
    class(PolytopePointerType), intent(out) :: obj
    class(PolytopeArrayInterface), intent(inout), target :: pa
    integer, intent(in) :: index
    obj%pc => pa
    obj%index = index
  end subroutine initPolytopePointerFromArray


  pure subroutine initPolytopePointerFromGrid( obj, pg, gridAddress )
    class(PolytopePointerType), intent(out) :: obj
    class(PolytopeGridInterface), intent(inout), target :: pg
    type(IntVectorType), intent(in) :: gridAddress
    obj%pc => pg
    obj%index = pg%convertToIndex( gridAddress )
  end subroutine initPolytopePointerFromGrid


  subroutine initPolytopePointerFromSet( obj, pps, index )
    class(PolytopePointerType), intent(out) :: obj
    class(PolytopePointerSetType), intent(inout), target :: pps
    integer, intent(in) :: index
    type(PolytopePointerType), pointer :: pp
    call pps%find( pp, index )
    call obj%init( pp )
  end subroutine initPolytopePointerFromSet


  elemental subroutine deinitPolytopePointer( obj, log )
    class(PolytopePointerType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    nullify( obj%pc )
    obj%index = 0
  end subroutine deinitPolytopePointer


  elemental function sameAs_PolytopePointer( obj, pp )
    class(PolytopePointerType), intent(in) :: obj, pp
    logical :: sameAs_PolytopePointer
    if ( associated(obj%pc) .and. associated(pp%pc) ) then
       sameAs_PolytopePointer = obj%pc%sameAs(pp%pc) .and. &
            (obj%index == pp%index)
    else if ( (.not. associated(obj%pc)) .and. &
         (.not. associated(pp%pc)) ) then
       sameAs_PolytopePointer = .true. 
    else
       sameAs_PolytopePointer = .false. 
    end if
  end function sameAs_PolytopePointer


  pure function describePolytopePointer( obj )
    class(PolytopePointerType), intent(in) :: obj
    character(60) :: describePolytopePointer
    if ( .not. associated(obj%pc) ) then
       describePolytopePointer = 'null'
    else
       describePolytopePointer = trim(obj%pc%describe())//', &
            &element '//str(obj%index)
    end if
  end function describePolytopePointer


  pure recursive subroutine collectSubPolytopes_PolytopePointer( &
       obj, pps, log, depth )
    class(PolytopePointerType), intent(inout) :: obj
    class(PolytopePointerSetType), intent(inout) :: pps
    class(LogType), intent(inout), optional :: log
    integer, intent(in), optional :: depth
    call beginSub( MOD_NAME, &
         'collectSubPolytopes_PolytopePointer', log )

    call addEvent( .not. associated(obj%pc), FATAL, 'obj%pc not &
         &associated.', log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! delegate to the real polytope collection
    call obj%pc%collectSubPolytopes( obj%index, pps, log, depth ) 

    call endSub( log )
  end subroutine collectSubPolytopes_PolytopePointer


  pure recursive subroutine collectSuperPolytopes_PolytopePointer( &
       obj, pps, log, height )
    class(PolytopePointerType), intent(inout) :: obj
    class(PolytopePointerSetType), intent(inout) :: pps
    class(LogType), intent(inout), optional :: log
    integer, intent(in), optional :: height
    call beginSub( MOD_NAME, &
         'collectSuperPolytopes_PolytopePointer', log )

    ! delegate to the real polytope collection
    call obj%pc%collectSuperPolytopes( obj%index, pps, log, height ) 

    call endSub( log )
  end subroutine collectSuperPolytopes_PolytopePointer


  pure function getPolytopeDim_PolytopePointer( obj ) &
       result ( getPolytopeDim )
    class(PolytopePointerType), intent(in) :: obj
    integer :: getPolytopeDim
    getPolytopeDim = obj%pc%getPolytopeDim()
  end function getPolytopeDim_PolytopePointer
    

  !> returns indices for all polytopes in the set matching a certain
  !> PolytopeCollection.
  function match_Collection_PolytopePointer( obj, pc ) result ( index )
    class(PolytopePointerType), intent(inout) :: obj
    class(PolytopeCollectionInterface), intent(in) :: pc
    integer :: index
    if ( obj%pc%sameAs(pc) ) then
       index = obj%index
    else
       index = 0
    end if
  end function match_Collection_PolytopePointer


  pure subroutine getCentroid_PolytopePointer( obj, coords )
    class(PolytopePointerType), intent(inout) :: obj
    type(RealVectorType), intent(out)  :: coords 
    call obj%pc%getCentroid( obj%index, coords )
  end subroutine getCentroid_PolytopePointer


  pure subroutine getSize_PolytopePointer( obj, size )
    class(PolytopePointerType), intent(inout) :: obj
    real(FLOAT), intent(out)  :: size
    call obj%pc%getSize( obj%index, size )
  end subroutine getSize_PolytopePointer


  subroutine appendVelocityMoments_PolytopePointer( obj, &
       velocityMomentGroupList, log )
    class(PolytopePointerType), intent(inout) :: obj
    type(VectorMomentGroupListType), intent(inout) :: &
         velocityMomentGroupList
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'appendVelocityMoments_PolytopePointer', log )
    
    call obj%pc%appendVelocityMoments( velocityMomentGroupList, obj%&
         index, log )
    
    call endSub( log )
  end subroutine appendVelocityMoments_PolytopePointer

  
  subroutine appendScalarMoments_PolytopePointer( obj, &
       scalarMomentGroupList, log )
    class(PolytopePointerType), intent(inout) :: obj
    type(ScalarMomentGroupListType), intent(inout) :: &
         scalarMomentGroupList
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, &
         'appendScalarMoments_PolytopePointer', log )
    
    call obj%pc%appendScalarMoments( scalarMomentGroupList, &
         obj%index, log )
    
    call endSub( log )
  end subroutine appendScalarMoments_PolytopePointer


  pure subroutine appendConnection_PolytopePointer( obj, string, &
       startFromZero, indent )
    class(PolytopePointerType), intent(in) :: obj
    character(*), intent(inout) :: string
    logical, intent(in)  :: startFromZero
    integer, intent(in), optional :: indent
    call obj%pc%appendConnection( string, obj%index, &
         startFromZero, indent )
  end subroutine appendConnection_PolytopePointer
  

  EXPAND({METHODDEF_POLYTOPEPOINTER_EXTENSION({Face})})
  
  EXPAND({METHODDEF_POLYTOPEPOINTER_EXTENSION({Cell})})
  

  !-------------------------------------------------------------------
  !- PolytopePointerSet procedures
  !-------------------------------------------------------------------

  EXPAND({METHODDEFS_CONTAINER(PolytopePointer, Type, Set, {}, {pure})})
  
  pure subroutine initPolytopePointerSet( obj, ppArray, log )
    class(PolytopePointerSetType), intent(out), target :: obj
    type(PolytopePointerType), dimension(:), intent(inout) :: ppArray
    class(LogType), intent(inout), optional :: log
    integer :: i
    logical, parameter :: DEBUG_MODE = .false. 
    call beginSub( MOD_NAME, 'initPolytopePointerSet', log )

    ! loop over pp array elements
    do i = 1, size(ppArray)
       ! add to set
       call obj%append( ppArray(i), log )
       if ( checkSub(FATAL, log) ) then
          call endSub( log )
          return
       end if
    end do

    call endSub( log )
  end subroutine initPolytopePointerSet


  subroutine printPolytopePointerSet( obj, ioUnit )
    class(PolytopePointerSetType), intent(inout), target :: obj
    integer, intent(in) :: ioUnit
    type(PolytopePointerSetIteratorType) :: iterator
    type(PolytopePointerType), pointer :: pp
    call iterator%init( obj )
    call iterator%first( pp )
    do
       if ( iterator%isDone() ) exit
       write(ioUnit, *) '  ', trim(pp%describe())
       call iterator%next( pp )
    end do
  end subroutine printPolytopePointerSet


  pure recursive subroutine addRecursively( obj, pp, log, height )
    class(PolytopePointerSetType), intent(inout) :: obj
    type(PolytopePointerType), intent(inout) :: pp
    class(LogType), intent(inout), optional :: log
    integer, intent(in), optional :: height
    integer :: i, newHeight, allocStat
    type(PolytopePointerSetIteratorType) :: iterator
    type(PolytopePointerType), pointer :: contents
    logical, parameter :: DEBUG_MODE = .false. 
    call beginSub( MOD_NAME, 'addRecursively', log )

    ! terminate if the desired height has been reached.  We would not
    ! expect the client to supply us with a zero height, but this block
    ! is still included for safety.
    if ( present(height) ) then
       if ( height == 0 ) then
          call endSub( log )
          return
       end if
    end if

    ! does the polytope already exist in the set?  Terminate if so
    i = obj%match(pp)
    if ( i /= 0 ) then
       call addEvent( DEBUG_MODE, ADVICE, 'Match already found &
            &for polytope '//trim(pp%describe()), log )
       call endSub( log )
       return
    end if

    ! can now add the nominal dimensional polytope
    call obj%append( pp, log )
    call addEvent( DEBUG_MODE, ADVICE, 'Added '//trim(&
         pp%describe()), log )

    if ( DEBUG_MODE ) then
       call iterator%init( obj )
       call iterator%last( contents )
       call addEvent( ADVICE, 'Contents of last node are '//trim(&
            contents%describe()), log )
       call addEvent( .not. contents%sameAs(pp), FATAL, 'Contents of &
            &last node do not match those just added.', log )
    end if

    ! add sub- or super- dimensional polytopes if appropriate.  If no
    ! height specified, default to collecting subs until finished
    if ( .not. present(height) ) then
       call pp%collectSubPolytopes( obj, log )
       call endSub( log )
       return
    end if

    ! the relative depth or height needed has decreased by one level.
    newHeight = height - sign(1, height)
    select case ( newHeight )
    case (1:)
       ! collect supers
       call pp%collectSuperPolytopes( obj, log, newHeight )
    case (0)
       ! terminate
       call endSub( log )
       return
    case (:-1)
       ! collect subs (n.b. change height argument into depth)
       call pp%collectSubPolytopes( obj, log, -newHeight )
    end select

    call endSub( log )
  end subroutine addRecursively


  !> returns indices for all polytopes in the set matching a certain
  !> PolytopeCollection.
  !
  !> The reason this is not added to the 'match' generic interface is
  !> that the allocatable result demands that this procedure is a
  !> subroutine, not a function
  pure subroutine query_Collection_PolytopePointerSet( obj, pc, &
       indices, log )
    class(PolytopePointerSetType), intent(inout), target :: obj
    class(PolytopeCollectionInterface), intent(in) :: pc
    integer, dimension(:), allocatable, intent(out) :: indices
    class(LogType), intent(inout), optional :: log
    integer :: i, n, allocStat
    type(PolytopePointerSetIteratorType) :: iterator
    type(PolytopePointerType), pointer :: pp
    logical, parameter :: DEBUG_MODE = .false.
    call beginSub( MOD_NAME, &
         'query_Collection_PolytopePointerSet', log )
    call iterator%init( obj )

    ! first pass: count the nodes that match
    call iterator%first( pp )
    n = 0
    do
       if ( iterator%isDone() ) exit
       if ( pp%match(pc) /= 0 ) n = n + 1
       call iterator%next( pp )
    end do

    ! allocate the result
    allocate( indices(n), stat=allocStat )
    call addEvent( allocStat/=0, FATAL, 'Problem allocating &
         &elements of indices.  STAT='//int2str(allocStat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! second pass: fill in the elements
    call iterator%first( pp )
    n = 0
    do i = 1, obj%size()
       if ( pp%match(pc) /= 0 ) then
          n = n + 1
          indices(n) = pp%match(pc)
       end if
       call iterator%next( pp )
    end do

    if ( n > 0 ) then
       call addEvent( DEBUG_MODE, ADVICE, 'result = '//str(indices), &
            log )
    end if

    call endSub( log )
  end subroutine query_Collection_PolytopePointerSet


  !> removes from obj those polytopes that do not have dimension iDim.
  pure subroutine filter_Dimension_PolytopePointerSet( obj, iDim, log )
    class(PolytopePointerSetType), intent(inout), target :: obj
    integer, intent(in) :: iDim
    class(LogType), intent(inout), optional :: log
    integer :: i, deallocStat
    type(PolytopePointerSetIteratorType) :: iterator
    type(PolytopePointerType), pointer :: pp
    logical, parameter :: DEBUG_MODE = .false. 
    call beginSub( MOD_NAME, &
         'filter_Dimension_PolytopePointerSet', log )

    call iterator%init( obj )
    call iterator%first( pp )
    do
       if ( .not. associated(iterator%ptr) ) exit
       if ( pp%getPolytopeDim() /= iDim ) then
          call iterator%extract( log )
          call iterator%current( pp )
       else
          call iterator%next( pp )
       end if
    end do

    call endSub( log )
  end subroutine filter_Dimension_PolytopePointerSet


!!$  !> removes from obj those polytopes that are not associated with the
!!$  !> specified region.
!!$  subroutine filter_Region_PolytopePointerSet( obj, region, log )
!!$    class(PolytopePointerSetType), intent(inout), target :: obj
!!$    class(RegionInterface), intent(in) :: region
!!$    class(LogType), intent(inout), optional :: log
!!$    integer :: i, deallocStat
!!$    type(PolytopePointerSetIteratorType) :: iterator
!!$    type(PolytopePointerType), pointer :: pp
!!$    logical, parameter :: DEBUG_MODE = .false.
!!$    call beginSub( MOD_NAME, 'filter_Region_PolytopePointerSet', &
!!$         log )
!!$    
!!$    call iterator%init( obj )
!!$    call iterator%first( pp )
!!$    do
!!$       if ( .not. associated(iterator%ptr) ) exit
!!$       if ( .not. pp%isAssociatedWith(region) ) then
!!$          call iterator%extract( log )
!!$          call iterator%current( pp )
!!$       else
!!$          call iterator%next( pp )
!!$       end if
!!$    end do
!!$    
!!$    call endSub( log )
!!$  end subroutine filter_Region_PolytopePointerSet


  !-------------------------------------------------------------------
  !- PolytopeCollectionPointer methods
  !-------------------------------------------------------------------

  elemental function associated_PolytopeArrayPointer( obj )
    class(PolytopeArrayPointerType), intent(in) :: obj
    logical :: associated_PolytopeArrayPointer
    associated_PolytopeArrayPointer = obj%isAssociated
  end function associated_PolytopeArrayPointer


  elemental function associated_PolytopeGridPointer( obj )
    class(PolytopeGridPointerType), intent(in) :: obj
    logical :: associated_PolytopeGridPointer
    associated_PolytopeGridPointer = obj%isAssociated
  end function associated_PolytopeGridPointer


  
  !-------------------------------------------------------------------
  !- PolytopeComplex methods
  !-------------------------------------------------------------------

  EXPAND({PROCEDURE_DESTROY({PolytopeComplex})})
  
  EXPAND({PROCEDURE_DESTROY_ARRAY({PolytopeComplex}, {Interface})})


  !-------------------------------------------------------------------
  !- IntegrableElementPointer methods
  !-------------------------------------------------------------------

  EXPAND({PROCEDURE_DESTROY({IntegrableElementPointer})})
  
  m4_define({METHODDEFS_SUPERPOINTER_SUBCLASS},  {
  subroutine clone_$1Pointer( obj, integrableElementPointer, log )
    class($1PointerType), intent(in) :: obj
    class(IntegrableElementPointerInterface), allocatable, &
         intent(out) :: integrableElementPointer
    class(LogType), intent(inout), optional :: log
    integer :: stat
    type(PolytopePointerType) :: pp
    call beginSub( MOD_NAME, 'clone_$1Pointer', log )
    
    allocate( $1PointerType :: integrableElementPointer, stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating $1PointerType :: &
         &integrableElementPointer.  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    select type ( integrableElementPointer )
    type is ($1PointerType)
       call integrableElementPointer%init( obj )
    end select

    call integrableElementPointer%getOwner( pp )
    call addEvent( ADVICE, 'Cloned a $1Pointer based on '//trim(pp%&
         describe()), log )
    
    call endSub( log )
  end subroutine clone_$1Pointer

  
  pure subroutine initAsExtension_$1Pointer( obj, polytopePointer, log )
    class($1PointerType), intent(inout) :: obj
    class(PolytopePointerType), intent(in) :: polytopePointer
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'initAsExtension_$1Pointer', log )
    
    call polytopePointer%getPointerExtension( obj, log )
    
    call endSub( log )
  end subroutine initAsExtension_$1Pointer

     
  pure function computeSubRow_$1Pointer( obj, &
       directedSpatialDerivative ) result ( row )
    class($1PointerType), intent(in) :: obj
    class(DirectedSpatialDerivativeInterface), intent(in) :: &
         directedSpatialDerivative
    real(FLOAT), dimension(1, NCOEFS_ADV) :: row

    ! delegate
    row = obj%ABB($1)c%computeSubRow( obj%polytopeIndex, &
         directedSpatialDerivative )
    
  end function computeSubRow_$1Pointer

    
  subroutine appendScalarMoments_$1Pointer( obj, &
       scalarMomentGroupList, log )
    class($1PointerType), intent(inout) :: obj
    type(ScalarMomentGroupListType), intent(inout) :: &
         scalarMomentGroupList 
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendScalarMoments_$1Pointer', log )
    
    call obj%ABB($1)c%appendScalarMoments( scalarMomentGroupList, obj%&
         polytopeIndex, log )
    
    call endSub( log )
  end subroutine appendScalarMoments_$1Pointer

  
  subroutine appendVelocityMoments_$1Pointer( obj, &
       velocityMomentGroupList, log )
    class($1PointerType), intent(inout) :: obj
    type(VectorMomentGroupListType), intent(inout) :: &
         velocityMomentGroupList 
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'appendVelocityMoments_$1Pointer', log )

    call obj%ABB($1)c%appendVelocityMoments( velocityMomentGroupList, &
         obj%polytopeIndex, log )
    
    call endSub( log )
  end subroutine appendVelocityMoments_$1Pointer
  })

  
  !-------------------------------------------------------------------
  !- FaceExtension methods
  !-------------------------------------------------------------------

  EXPAND({METHODDEFS_POLYTOPEEXTENSION({Face})})
  
  ! --- pointer methods ---

  EXPAND({PROCEDURES_POLYTOPEEXTENSION_POINTER({Face})})
  
  EXPAND({METHODDEFS_POLYTOPEEXTENSION_POINTER({Face})})
  
  pure subroutine getFaceNormal_FacePointer( obj, vector, log )
    class(FacePointerType), intent(in) :: obj
    type(RealVectorType), intent(out) :: vector
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getFaceNormal_FacePointer', log )

    call obj%fc%getFaceNormal( obj%polytopeIndex, vector, log )

    call endSub( log )
  end subroutine getFaceNormal_FacePointer

  
  elemental subroutine findSide_FacePointer( obj, coords, side, log )
    class(FacePointerType), intent(inout) :: obj
    type(RealVectorType), intent(in) :: coords
    logical, intent(out) :: side
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'findSide_FacePointer', log )

    call obj%fc%findSide( obj%polytopeIndex, coords, side, log )

    call endSub( log )
  end subroutine findSide_FacePointer

  
  !-------------------------------------------------------------------
  !- CellExtension methods
  !-------------------------------------------------------------------

  EXPAND({METHODDEFS_POLYTOPEEXTENSION({Cell})})
  
  ! --- pointer methods ---
  
  EXPAND({PROCEDURES_POLYTOPEEXTENSION_POINTER({Cell})})
  
  EXPAND({METHODDEFS_POLYTOPEEXTENSION_POINTER({Cell})})

!!$  subroutine computeIntAveSubRow_CellPointer( obj, row, log )
!!$    class(CellPointerType), intent(in) :: obj
!!$    real(FLOAT), dimension(:, :), intent(out) :: row
!!$    class(LogType), intent(inout), optional :: log
!!$    call beginSub( MOD_NAME, 'computeIntAveSubRow_CellPointer', log )
!!$
!!$    call obj%cc%computeIntAveSubRow( row, obj%polytopeIndex, log )
!!$    
!!$    call endSub( log )
!!$  end subroutine computeIntAveSubRow_CellPointer
  
  EXPAND({METHODDEF_CELLPOINTER_APPENDROW({Scalar})})
  
  EXPAND({METHODDEF_CELLPOINTER_APPENDROW({Vector})})

  m4_undefine(METHODDEFS_SUPERPOINTER_SUBCLASS)
  

  !-------------------------------------------------------------------
  !- ProfileOperation methods
  !-------------------------------------------------------------------
  
  EXPAND({PROCEDURE_DESTROY({PureScalarProfileOperation})})
  
  EXPAND({PROCEDURE_DESTROY({ImpureScalarProfileOperation})})
  
  EXPAND({METHODDEFS_PROFILEOPERATION({Scalar})})
  
  EXPAND({PROCEDURE_DESTROY({PureVectorProfileOperation})})
  
  EXPAND({PROCEDURE_DESTROY({ImpureVectorProfileOperation})})
  
  EXPAND({METHODDEFS_PROFILEOPERATION({Vector})})

  !-------------------------------------------------------------------
  !- Profile methods
  !-------------------------------------------------------------------

  EXPAND({PROCEDURE_DESTROY({ScalarProfile})})

  EXPAND({PROCEDURE_DESTROY_ARRAY({ScalarProfile}, {Interface})})

  EXPAND({PROCEDURE_DESTROY({VectorProfile})})

  EXPAND({PROCEDURE_DESTROY_ARRAY({VectorProfile}, {Interface})})

  EXPAND({METHODDEFS_PROFILE({Scalar})})
  
  EXPAND({METHODDEFS_PROFILE({Vector})})

  
end module FiniteVolumeModule
