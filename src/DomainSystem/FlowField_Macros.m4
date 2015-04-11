module FlowField_Macros

  m4_define({TYPE_TENSOR_PRIMITIVE}, {m4_ifelse({$1}, {Vector},
  {type(RealVectorType)}, {real(FLOAT)})})

  m4_define({IMPORT_TENSOR_PRIMITIVE}, {m4_ifelse({$1}, {Vector},
  {RealVectorType}, {FLOAT})})
  
  m4_define({ARGLINE_TENSOR_COMPONENT}, {m4_ifelse({$1}, {Vector},
  {, componentIndex}, {})})
  
  m4_define({ARGLIST_TENSOR_COMPONENT}, {m4_ifelse({$1}, {Vector},
  {integer, intent(in) :: componentIndex}, {m4_dnl})})
  
  m4_define({DATALIST_TENSOR_COMPONENT}, {m4_ifelse({$1}, {Vector},
  {integer :: componentIndex}, {m4_dnl})})
    
  m4_define({ARGLINE_TENSOR_COUPLED}, {m4_ifelse({$1}, {Vector},
  {, coupledComponents}, {})})
  
  m4_define({ARGLIST_TENSOR_COUPLED}, {m4_ifelse({$1}, {Vector},
  {logical, intent(in), optional :: coupledComponents}, {m4_dnl})})
  
  !-------------------------------------------------------------------
  !- SpatialDerivativeInterface and DirectionInterface
  !-------------------------------------------------------------------
  
  MACRO({SIGNATURELIST_SPATIALDERIVATIVE}, {{
     procedure(ABB($1)sd_di), deferred :: deinit
     procedure(ABB($1)sd_c), deferred :: clone
     procedure(ABB($1)sd_crcfrc), deferred :: &
          computeSubRowComponentsFromRawCoords
     procedure(ABB($1)sd_sa), deferred :: sameAs
     }})
  
  MACRO({TYPEDEF_SPATIALDERIVATIVE}, {{
  type, abstract, public :: $1SpatialDerivativeInterface
     private
   contains
     EXPAND({SIGNATURELIST_SPATIALDERIVATIVE({$1})})
     ! "Sub-rows" are row vectors that are as long as the number of
     ! coefficients in a scalar profile.  Sub-rows, plural, because the
     ! SpatialDerivative might be dealing with multiple arrival points
     ! resident on a polytope.  "Row components" are a set of NDIM rows or
     ! sub-rows that represent an intermediate step in the computation
     ! before e.g. gradient direction has an influence.
     procedure(ABB($1)sd_csr), deferred :: computeSubRows
     procedure :: computeSubRowComponents => &
          computeSubRowComponents_$1
     $2
  end type $1SpatialDerivativeInterface
  }})

  
  !-------------------------------------------------------------------
  !- FlowVariable
  !-------------------------------------------------------------------

  MACRO({SIGNATURELIST_FLOWVARIABLE}, {{
     procedure(ABB($1)fv_di), deferred :: deinit
     }})

  MACRO({METHODLIST_SUBFLOWVARIABLE}, {{
     procedure :: init$1FlowVariable
     procedure :: deinit$1FlowVariable
     procedure :: getNumComponents => getNumComponents_$1FlowVariable
     }})

  MACRO({SIGNATURELIST_SUBFLOWVARIABLE}, {{
     EXPAND({SIGNATURELIST_FLOWVARIABLE($1)})
     procedure(ABB($1)fv_c), deferred :: clone
     }})

  
  !-------------------------------------------------------------------
  !- StaticPoints
  !-------------------------------------------------------------------

  MACRO({INITLIST_STATICPOINTCOLLECTION}, {{
     procedure :: init$1StaticPointCollection
     procedure :: deinit$1StaticPointCollection
     }})
  
  MACRO({SIGNATURELIST_STATICPOINTCOLLECTION}, {{
     procedure(ABB({$1})spc_di), deferred :: deinit
     }})

  ! a StaticPointGroupPointer holds a reference to the points existing on a
  ! particular Polytope.
     
  MACRO({SIGNATURELIST_STATICPOINTGROUPPOINTER}, {{
     procedure(ABB($1)spgp_gi), deferred :: getNumPoints
     procedure(ABB($1)spgp_gi), deferred :: getPolytopeIndex
     procedure(ABB($1)spgp_gp), deferred :: getPositions
     }})

     
  !-------------------------------------------------------------------
  !- StaticPoints sub- and concrete classes
  !-------------------------------------------------------------------

  MACRO({TYPEDEF_SUBSTATICPOINTCOLLECTION}, {{
  type, extends($2StaticPointCollectionInterface), abstract, public :: $1StaticPointCollectionInterface $3
   contains
     EXPAND({INITLIST_STATICPOINTCOLLECTION($1)})
     EXPAND({SIGNATURELIST_STATICPOINTCOLLECTION($1)})
     ! the following signature differs between sibling classes (due to
     ! directed vs undirected spatial derivative argument) and therefore
     ! cannot be declared a polymorphic method at the top level.
     procedure(ABB($1)spc_csr), deferred :: computeSubRows $4
  end type $1StaticPointCollectionInterface
  }})
     

  ! FAULTY - TO BE REMOVED
  ! 
  ! see notes for parent class
  MACRO({TYPEDEF_STATICPOINTGROUPPOINTER_CONCRETE}, {{
  type, extends($2StaticPointGroupPointerInterface), public :: $1StaticPointGroupPointerType
     class($1StaticPointCollectionInterface), pointer :: &
          LWR($1StaticPointCollection) => null()
     integer :: polytopeIndex = 0 $3
   contains
     procedure :: init_$1StaticPointGroupPointer_args
     procedure :: init_$1StaticPointGroupPointer_copy
     generic :: init => init_$1StaticPointGroupPointer_args, &
          init_$1StaticPointGroupPointer_copy
     procedure :: deinit => deinit_$1StaticPointGroupPointer
     procedure :: getNumPoints => &
          getNumPoints_$1StaticPointGroupPointer
     procedure :: getPolytopeIndex => &
          getPolytopeIndex_$1StaticPointGroupPointer
     procedure :: getPositions => &
          getPositions_$1StaticPointGroupPointer
     procedure :: computeSubRows => &
          computeSubRows_$1StaticPointGroupPointer $4
  end type $1StaticPointGroupPointerType
  }})

  
  ! a PointPointer wraps a StaticPointGroupPointer and additionally stores
  ! an index to one of the points.  But its practical use is limited and
  ! we may actually phase it out.

  MACRO({TYPEDEF_POINTPOINTER_CONCRETE}, {{
  type, extends($2PointPointerInterface), public :: $1PointPointerType
     type($1StaticPointGroupPointerType) :: LWR($1StaticPointGroupPointer)
     integer :: pointIndex = 0  $3
   contains
     procedure :: init => init_$1PointPointer
     procedure :: deinit => deinit_$1PointPointer
     procedure :: getPolytopeIndex => getPolytopeIndex_$1PointPointer
     procedure :: getPointIndex => getPointIndex_$1PointPointer
     $4
  end type $1PointPointerType
  }})
  

  !-------------------------------------------------------------------
  !- MomentSources (including MomentFields and BoundaryConditions)
  !-------------------------------------------------------------------
  
  MACRO({SIGNATURELIST_MOMENTSOURCE}, {{
     procedure(ABB($3$1$2)_di), deferred :: deinit{}m4_ifelse($1, {Vector}, {
     procedure(ABB($3$1$2)_hcc), deferred :: hasCoupledComponents})
     }})
     
  MACRO({TYPEDEF_MOMENTSOURCE}, {{
  type, abstract, public :: $1MomentSourceInterface
     private
     class($1FlowVariableInterface), pointer :: LWR($1)FlowVariable $2
   contains
     EXPAND({SIGNATURELIST_MOMENTSOURCE($1, {MomentSource})})
     procedure :: init$1MomentSource_args
     procedure :: init$1MomentSource_copy
     generic :: init$1MomentSource => &
          init$1MomentSource_args, &
          init$1MomentSource_copy
     procedure :: deinit$1MomentSource
     procedure :: getFlowVariable => &
          getFlowVariable_$1MomentSource
     procedure :: isAssociatedWith_FlowVar => &
          isAssociatedWith_FlowVar_$1MomentSource
     generic :: isAssociatedWith => isAssociatedWith_FlowVar
  end type $1MomentSourceInterface
  }})

  MACRO({METHODLIST_SUBMOMENTSOURCE}, {{
     procedure :: init$3$1$2_args
     procedure :: init$3$1$2_copy
     generic :: init$3$1$2 => &
          init$3$1$2_args, &
          init$3$1$2_copy
     procedure :: deinit$3$1$2
     }})

  MACRO({SIGNATURELIST_SUBMOMENTSOURCE}, {{
     EXPAND({SIGNATURELIST_MOMENTSOURCE($1, $2, $3)})
     ! Signatures for the following "append" methods differ depending on
     ! whether the MomentSource in question is a MomentField (with a
     ! DirectedSpatialDerivative) or a BoundaryCondition (with an
     ! UndirectedSpatialDerivative).  So the methods cannot be polymorphic
     ! at the upper level.
     procedure(ABB($3$1$2)_ar_m), deferred :: appendRows_Matrix
     procedure(ABB($3$1$2)_ar_ls), deferred :: appendRows_LinearSystem
     }})

     
  MACRO({SIGNATURELIST_MOMENTFIELD}, {{
     EXPAND({SIGNATURELIST_SUBMOMENTSOURCE($1, {MomentField}, $2)})
     procedure(ABB($2$1)mf_sf_u), deferred :: setField_uniform
     procedure(ABB($2$1)mf_sf_a), deferred :: setField_array
!!$     procedure(ABB($2$1)mf_sf_fs), deferred :: setField_FieldSpec
     }})
     
  MACRO({TYPEDEF_MOMENTFIELD}, {{
  type, extends($1MomentSourceInterface), abstract, public :: $1MomentFieldInterface
     private
     class(DirectedSpatialDerivativeInterface), allocatable :: &
          directedSpatialDerivative
   contains
     EXPAND({SIGNATURELIST_MOMENTFIELD($1)})
     EXPAND({METHODLIST_SUBMOMENTSOURCE($1, MomentField)})
     procedure :: getSpatialDerivative => &
          getSpatialDerivative_$1MomentField
     procedure :: isAssociatedWith_SpatialDeriv => &
          isAssociatedWith_SpatialDeriv_$1MomentField
     generic :: isAssociatedWith => isAssociatedWith_SpatialDeriv $3
  end type $1MomentFieldInterface
  }})
     

  MACRO({SIGNATURELIST_BOUNDARYCONDITION}, {{
     EXPAND({SIGNATURELIST_SUBMOMENTSOURCE($1, {BoundaryCondition}, $2)})
     }})

  MACRO({TYPEDEF_BOUNDARYCONDITION}, {{
  type, extends($1MomentSourceInterface), abstract, public :: $1BoundaryConditionInterface
     private
   contains
     EXPAND({SIGNATURELIST_BOUNDARYCONDITION($1)})
     EXPAND({METHODLIST_SUBMOMENTSOURCE($1, BoundaryCondition)})
  end type $1BoundaryConditionInterface
  }})

     
  !-------------------------------------------------------------------
  !- MomentGroups
  !-------------------------------------------------------------------
  
  MACRO({SIGNATURELIST_MOMENTGROUP},{{
     procedure(ABB($1)mg_di), deferred :: deinit
     procedure(ABB($1)mg_c), deferred :: clone
     procedure(ABB($1)mg_ar), deferred :: appendRows
     procedure(ABB($1)mg_ael), deferred :: appendElements
     procedure(ABB($1)mg_aeq), deferred :: appendEquations
     ! getValue was originally a function, but a strange bug existed in
     ! concrete classes whereby the return variable could change into
     ! single precision regardless of the FLOAT parameter specified in the
     ! Global module.  Our workaround was to make all getValue functions
     ! into subroutines.
     procedure(ABB($1)mg_gv), deferred :: getValue
     procedure(ABB($1)mg_gvs), deferred :: getValues
     }})

  MACRO({TYPEDEF_MOMENTGROUP}, {{
  type, abstract, public :: $1MomentGroupInterface
     private
     integer :: nMoments
     logical :: exactlyConstrained
     m4_ifelse($1, {Vector}, {
     logical :: coupledComponents})
   contains
     SIGNATURELIST_MOMENTGROUP($1)
     procedure :: init$1MomentGroup
     procedure :: deinit$1MomentGroup
     procedure :: getNumMoments => getNumMoments_$1MomentGroup
  end type $1MomentGroupInterface
  }})

  MACRO({TYPEDEFS_CONTAINER_MOMENTGROUP}, {{
    EXPAND({TYPEDEFS_CONTAINER($1MomentGroup, Interface, List,
    {class($1FlowVariableInterface), pointer :: LWR($1)FlowVariable},
     {procedure :: setFlowVariable => setFlowVariable_$1MomentGroupList
     procedure :: getFlowVariable => getFlowVariable_$1MomentGroupList
     procedure :: countNodes => countNodes_$1MomentGroupList
     procedure :: countMoments => countMoments_$1MomentGroupList})})
  }})
  
  
  !-------------------------------------------------------------------
  !- RHSElements
  !-------------------------------------------------------------------

  MACRO({SIGNATURELIST_RHSELEMENT}, {{
     procedure(ABB($1)rhse_di), deferred :: deinit
     procedure(ABB($1)rhse_gv), deferred :: getValue
     procedure(ABB($1)rhse_cl), deferred :: clone
     }})
  
  MACRO({TYPEDEF_MOMENTPOINTER}, {{
  ! this is a public implementation of RHSElementInterface that simply
  ! targets a $1MomentGroup.
  type, extends(RHSElementInterface), public :: $1MomentPointerType
     private
     class($1MomentGroupInterface), pointer :: ABB($1MomentGroup)
     integer :: momentIndex
     DATALIST_TENSOR_COMPONENT($1)
   contains
     procedure :: init => init_$1MomentPointer
     procedure :: deinit => deinit_$1MomentPointer
     procedure :: getValue => getValue_$1MomentPointer
     procedure :: clone => clone_$1MomentPointer
  end type $1MomentPointerType
  }})

  
  !-------------------------------------------------------------------
  !- Linear Operations and Operands
  !-------------------------------------------------------------------

  MACRO({SIGNATURELIST_LINEAROPERAND}, {{
     ! the accept methods work with a $1LinearOperation
     procedure(ABB($1)_a), deferred :: acceptAsOperand1
     procedure(ABB($1)_a), deferred :: acceptAsOperand2
     procedure(ABB($1)_a), deferred :: acceptAsOperand3
     }})
  
  MACRO({TYPEDEF_LINEAROPERAND}, {{
  ! a $1LinearOperand is designed to work with linear operations.
  ! See the latter classes for more details about how they work.
  type, abstract, public :: $1LinearOperandInterface
     private
   contains
     EXPAND({SIGNATURELIST_LINEAROPERAND({$1LinearOperand})})
  end type $1LinearOperandInterface
  }})
  
  MACRO({METHODLIST_LINEAROPERATION_VISIT}, {{
     procedure :: visit$2ForOperand1 => ignore_$1
     procedure :: visit$2ForOperand2 => ignore_$1
     procedure :: visit$2ForOperand3 => ignore_$1
     }})
     
  MACRO({SIGNATURELIST_LINEAROPERATION}, {{
     procedure(ABB($1)lotn_di), deferred :: deinit
     procedure(ABB($1)lotn_ck), deferred :: check
     }})
  
  
  MACRO({TYPEDEFS_LINEAROPERATION}, {{
  ! An implementation of the Visitor pattern.  Each visit method involves
  ! a linear operand passing some data to the binary operation object.  If
  ! the call is pertinent to the operation, it will override the existing
  ! stub to store a reference to the data.  See subclasses for execution
  ! methods.
  type, abstract, public :: $1LinearOperationInterface
     private
   contains
     EXPAND({SIGNATURELIST_LINEAROPERATION($1)})
     ! visit method stubs
     EXPAND({METHODLIST_LINEAROPERATION_VISIT($1, {Matrix})})
     EXPAND({METHODLIST_LINEAROPERATION_VISIT($1, {NullSpace})})
     EXPAND({METHODLIST_LINEAROPERATION_VISIT($1, {RHSColumn})})
     EXPAND({METHODLIST_LINEAROPERATION_VISIT($1, {SolutionColumn})})
  end type $1LinearOperationInterface


  ! When "perform" is called, the object carries out a pure arithmetic
  ! operation.
  type, extends($1LinearOperationInterface), abstract, public :: Pure$1LinearOperationInterface
     private
     real(FLOAT) :: scaleFactor = 1._FLOAT
   contains
     EXPAND({SIGNATURELIST_LINEAROPERATION(Pure$1)})
     procedure(p{}ABB($1)lotn_gn), deferred :: getNumRows
     procedure(p{}ABB($1)lotn_gn), deferred :: getNumColumns
     procedure(p{}ABB($1)lotn_p), deferred :: perform
     procedure(p{}ABB($1)lotn_c), deferred :: clone
     procedure :: setScaleFactor => setScaleFactor_$1
     procedure :: getScaleFactor => getScaleFactor_$1
  end type Pure$1LinearOperationInterface
  

  ! alternative to $1PureLinearOperationInterface whereby side effects are
  ! permissable and a Log argument may exist
  type, extends($1LinearOperationInterface), abstract, public :: Impure$1LinearOperationInterface
     private
   contains
     EXPAND({SIGNATURELIST_LINEAROPERATION(Impure$1)})
     procedure(i{}ABB($1)lotn_gn), deferred :: getNumRows
     procedure(i{}ABB($1)lotn_gn), deferred :: getNumColumns
     procedure(i{}ABB($1)lotn_p), deferred :: perform
  end type Impure$1LinearOperationInterface
  }})
  

  !-------------------------------------------------------------------
  !- Matricies, RHS, LinearSystems
  !-------------------------------------------------------------------
  
  MACRO({TYPEDEF_MATRIX}, {{
  type, extends($1LinearOperandInterface), abstract, public :: $1MatrixInterface
     private
   contains
     SIGNATURELIST_LINEAROPERAND({$1Matrix})
     procedure(ABB($1)mx_di), deferred :: deinit
     procedure(ABB($1)mx_s_r), deferred :: setRows_real
     procedure(ABB($1)mx_s_lo), deferred :: setRows_LinOp
     generic :: setRows => setRows_real, setRows_LinOp
     procedure(ABB($1)mx_f), deferred :: factorise
!!$     ! factorisation may have involve pivoting, so we need to be able to
!!$     ! permutate a corresponding RHS accordingly
!!$     procedure(ABB($1)mx_p), deferred :: permutate
     ! the solve method takes a RHS and returns a solution.  This will be
     ! the optimal solution if the matrix is over- or under-determined.
     procedure(ABB($1)mx_s), deferred :: solve
     procedure(ABB($1)mx_p), deferred :: permutate
     procedure(ABB($1)mx_gn), deferred :: getNumRows
     procedure(ABB($1)mx_gn), deferred :: getNumColumns
     procedure(ABB($1)mx_ptr), deferred :: prepareToReset
     $2
  end type $1MatrixInterface
  }})
  

  MACRO({TYPEDEF_RHSCOLUMN}, {{
  type, extends($1LinearOperandInterface), abstract, public :: $1RHSColumnInterface
     private
   contains
     SIGNATURELIST_LINEAROPERAND({$1RHSColumn})
     procedure(ABB($1)rhsc_di), deferred :: deinit
     procedure(ABB($1)rhsc_s_rhsg), deferred :: setElements_RHSGen
     procedure(ABB($1)rhsc_s_r), deferred :: setElements_real
     generic :: setElements => setElements_RHSGen, setElements_real
     ! Marked for deletion - no longer needed 
!!$     procedure(ABB($1)rhsc_sp), deferred :: setPermutation
     
     ! the refresh method updates a real-valued vector within the object.
     ! We do not actually need to store such a component, but it does make
     ! linear operations easier.  We may refactor it out sometime in the
     ! future.
     procedure(ABB($1)rhsc_r), deferred :: refresh
     procedure(ABB($1)rhsc_gn), deferred :: getNumElements
     procedure(ABB($1)rhsc_ptr), deferred :: prepareToReset
     $2
  end type $1RHSColumnInterface
  }})
  
  
  MACRO({TYPEDEF_LINEARSYSTEM}, {{
  type, extends($1LinearOperandInterface), abstract, public :: $1LinearSystemInterface
     private
   contains
     SIGNATURELIST_LINEAROPERAND({$1LinearSystem})
     procedure(ABB($1)ls_di), deferred :: deinit
     procedure(ABB($1)ls_sr_r), deferred :: setMatrixRows_real
     procedure(ABB($1)ls_s_lo), deferred :: setMatrixRows_LinOp
     generic :: setMatrixRows => setMatrixRows_real, setMatrixRows_LinOp
     procedure(ABB($1)ls_s_rhsg), deferred :: setRHSElements_RHSGen
     procedure(ABB($1)ls_se_r), deferred :: setRHSElements_real
     procedure :: setRHSElements_LinOp_$1LinearSystem
     generic :: setRHSElements => setRHSElements_RHSGen, &
          setRHSElements_real, setRHSElements_LinOp_$1LinearSystem
     procedure(ABB($1)ls_se_r), deferred :: setSolutionElements_real
     procedure(ABB($1)ls_s_lo), deferred :: setSolutionElements_LinOp
     generic :: setSolutionElements => setSolutionElements_real, &
          setSolutionElements_LinOp
     procedure(ABB($1)ls_f), deferred :: factorise
     procedure(ABB($1)ls_p), deferred :: permutateSolution
     procedure(ABB($1)ls_s), deferred :: solve
     procedure(ABB($1)ls_gn), deferred :: getNumEquations
     procedure(ABB($1)ls_p), deferred :: prepareToReset
     procedure(ABB($1)ls_p), deferred :: prepareToBeVisited
     $2
  end type $1LinearSystemInterface
  }})


  !-------------------------------------------------------------------
  !- RHSGenerators
  !-------------------------------------------------------------------

  MACRO({TYPEDEF_RHSGENERATOR_CONCRETE}, {{
  type, extends($1RHSGeneratorInterface), public :: $1$2BasedRHSGeneratorType
     private
     class($3Interface), pointer :: LWR($3) => null()
   contains
     procedure :: init_args => init_args_$1$2BasedRHSGenerator
     procedure :: init_src_conc => init_src_conc_$1$2BasedRHSGenerator
     generic :: init => init_args, init_src_conc
  end type $1$2BasedRHSGeneratorType
  }})

  
  MACRO({TYPEDEFS_RHSGENERATOR}, {{
  ! these are designed to be allocatable as arrays, with each element
  ! wrapping one of the other interfaces in this module.  The type of
  ! interface must remain invariant across elements, but the subclasses of
  ! that interface can be arbitrarily polymorphic.  For instance, we might
  ! have an array of $1MomGpBasedRHSGenerators, populated by various
  ! Interior/Boundary Point/IntAve MomentGroups.  $1RHSGenerator concrete
  ! classes are confined to this module, so we might as well put their
  ! same-signature methods in the abstract class and exploit the select
  ! type construct.
  type, abstract, public :: $1RHSGeneratorInterface
   contains
     procedure :: init_src_abs => init_src_abs_$1RHSGenerator
     generic :: init => init_src_abs
     procedure :: deinit => deinit_$1RHSGenerator
     procedure :: getValues => getValues_$1RHSGenerator
     procedure :: getNumElements => getNumElements_$1RHSGenerator
     procedure :: isInitialised => isInitialised_$1RHSGenerator
     procedure :: allocArray => allocArray_$1RHSGenerator
  end type $1RHSGeneratorInterface


  ! This one points to a $1MomentGroup.
  type, extends($1RHSGeneratorInterface), public :: $1MomGpBasedRHSGeneratorType
     private
     class($1MomentGroupInterface), pointer :: &
          LWR($1)MomentGroup => null()
   contains
     procedure :: init_args => init_args_$1MomGpBasedRHSGenerator
     procedure :: init_src_conc => init_src_conc_$1MomGpBasedRHSGenerator
     generic :: init => init_args, init_src_conc
  end type $1MomGpBasedRHSGeneratorType


  ! This one clones and wraps a Pure$1LinearOperation.  The cloning is
  ! done because clients of this class may need to last longer than those
  ! supplying the input Pure$1LinearOperation.
  type, extends($1RHSGeneratorInterface), public :: $1LinOpBasedRHSGeneratorType
     private
     class(Pure$1LinearOperationInterface), allocatable :: &
          pure$1LinearOperation
   contains
     procedure :: init_args => init_args_$1LinOpBasedRHSGenerator
     procedure :: init_src_conc => init_src_conc_$1LinOpBasedRHSGenerator
     generic :: init => init_args, init_src_conc
  end type $1LinOpBasedRHSGeneratorType
  }})

  
  !-------------------------------------------------------------------
  !- FieldSpecifications
  !-------------------------------------------------------------------

  MACRO({TYPEDEF_FIELDSPECIFICATION}, {{
  type, abstract, public :: $1FieldSpecificationInterface
     private
     class($1FlowVariableInterface), pointer :: LWR($1)FlowVariable 
     class(DirectedSpatialDerivativeInterface), allocatable :: &
          directedSpatialDerivative
   contains
     procedure(ABB($1)fs_di), deferred :: deinit
     procedure(ABB($1)fs_c), deferred :: clone
     procedure(ABB($1)fs_gv), deferred :: getValue
     procedure :: init$1FieldSpecification
     procedure :: deinit$1FieldSpecification
     procedure :: getValues => getValues_$1FieldSpecification
     procedure :: isAssociatedWith => &
          isAssociatedWith_$1FieldSpecification
  end type $1FieldSpecificationInterface
  }})
  
  
  
  !-------------------------------------------------------------------
  !- abstract interfaces
  !-------------------------------------------------------------------
    
  MACRO({SIGNATUREDEF_APPENDMOMENTS},{{
     ! this is a generic signature for appending moments to a list based
     ! on some target flowVariable.
     subroutine ABB($2)_a{}ABB($1)m( &
          obj, LWR($1)MomentGroupList, $4log )
       import $2$3, &
            $1MomentGroupListType, &
            FlowVariableInterface, LogType
       class($2$3), intent(inout) :: obj
       type($1MomentGroupListType), intent(inout) :: &
            LWR($1)MomentGroupList $5
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($2)_a{}ABB($1)m
     }})
     
  MACRO({SIGNATUREDEF_APPENDVELOCITYMOMENTS},{{     
     ! This signature is designed specifically for velocity as opposed
     ! to general vectors, so its signature contains "u" instead of "v"
     subroutine ABB($1)_aum( obj, velocityMomentGroupList, &
          $3log )
       import $1$2, &
            VectorMomentGroupListType, FlowVariableInterface, LogType
       class($1$2), intent(inout) :: obj
       type(VectorMomentGroupListType), intent(inout) :: &
            velocityMomentGroupList $4
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)_aum
     }})

     
  MACRO({SIGNATUREDEF_INITFIELDS},{{
     ! This is a generic signature for initialising scalar or vector
     ! moment fields
     subroutine ABB($2)_i{}ABB($1)f( obj, LWR($1)FieldSpecifications, log )
       import $2Interface, &
            $1FieldSpecificationListType, LogType
       class($2Interface), intent(inout) :: obj
       type($1FieldSpecificationListType), intent(in) :: &
            LWR($1)FieldSpecifications
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($2)_i{}ABB($1)f
     }})


  MACRO({SIGNATUREDEFS_SPATIALDERIVATIVE},{{
     subroutine ABB($1)sd_di( obj, log )
       import $1SpatialDerivativeInterface, LogType
       class($1SpatialDerivativeInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)sd_di

     subroutine ABB($1)sd_c( obj, tgt, log )
       import $1SpatialDerivativeInterface, LogType
       class($1SpatialDerivativeInterface), intent(in) :: obj
       class($1SpatialDerivativeInterface), allocatable, intent(out) :: tgt
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)sd_c

     pure function ABB($1)sd_crcfrc( obj, coords, &
          undiffPowers ) result ( rowComponents )
       import $1SpatialDerivativeInterface, RealVectorType, NDIM, FLOAT
       class($1SpatialDerivativeInterface), intent(in) :: obj
       ! BEWARE: coords and undiff powers are different shapes.  Both
       ! express components x,y,z in the 1st array dimension, but in
       ! the 2nd dimension, coords expresses multiple points, whereas
       ! undiffPowers expresses monomials corresponding to profile
       ! coefficients.  These entities are represented respectively in
       ! the resulting 3D array.
       real(FLOAT), dimension(:, :), intent(in) :: coords
       integer, dimension(:, :), intent(in) :: undiffPowers
       real(FLOAT), dimension(NDIM, size(coords, 2), size(&
            undiffPowers, 2)) :: rowComponents
     end function ABB($1)sd_crcfrc
     
     pure function ABB($1)sd_sa( obj, LWR($1)SpatialDerivative )
       import $1SpatialDerivativeInterface
       class($1SpatialDerivativeInterface), intent(in) :: obj
       class($1SpatialDerivativeInterface), intent(in) :: &
            LWR($1)SpatialDerivative
       logical :: ABB($1)sd_sa
     end function ABB($1)sd_sa
     }})
  
  MACRO({SIGNATUREDEFS_FLOWVARIABLE},{{
     subroutine ABB($1)fv_di( obj, log )
       import $1FlowVariableInterface, LogType
       class($1FlowVariableInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)fv_di
     }})
          
  MACRO({SIGNATUREDEFS_SUBFLOWVARIABLE},{{
     EXPAND({SIGNATUREDEFS_FLOWVARIABLE($1)})
  
     subroutine ABB($1)fv_c( obj, tgt, log )
       import $1FlowVariableInterface, LogType
       class($1FlowVariableInterface), intent(in) :: obj
       class($1FlowVariableInterface), allocatable, intent(out) :: tgt
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)fv_c
     }})

     
  MACRO({SIGNATUREDEFS_STATICPOINTCOLLECTION},{{
     subroutine ABB($1)spc_di( obj, log )
       import $1StaticPointCollectionInterface, LogType
       class($1StaticPointCollectionInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)spc_di
     }})
     

  MACRO({SIGNATUREDEFS_SUBSTATICPOINTCOLLECTION},{{
     EXPAND({SIGNATUREDEFS_STATICPOINTCOLLECTION({$1})})
     
     ! a sub-row, unlike a full matrix row, is only as long as the
     ! number of coefficients in the profile.  A full matrix row may
     ! be NDIM times longer if representing a vector with coupled
     ! components.  This procedure is a pure function so we can call
     ! it from a forall block if needed.
     pure function ABB($1)spc_csr( obj, polytopeIndex, &
          LWR($2)SpatialDerivative, nPoints ) result ( rows )
       import $1StaticPointCollectionInterface, &
            $2SpatialDerivativeInterface, FLOAT, NCOEFS_ADV
       class($1StaticPointCollectionInterface), intent(in) :: obj
       integer, intent(in) :: polytopeIndex
       class($2SpatialDerivativeInterface), intent(in) :: &
            LWR($2)SpatialDerivative
       integer, intent(in) :: nPoints
       ! nPoints is needed explicitly because the compiler has trouble
       ! with the object's accessor method appearing in the automatic
       ! array declaration
       real(FLOAT), dimension(nPoints, NCOEFS_ADV) :: rows
     end function ABB($1)spc_csr
     }})

  MACRO({SIGNATUREDEFS_STATICPOINTGROUPPOINTER}, {{
     pure subroutine ABB($1)spgp_gp( obj, pointCoords, log )
       import $1StaticPointGroupPointerInterface, RealVectorType, LogType
       class($1StaticPointGroupPointerInterface), intent(in) :: obj
       type(RealVectorType), dimension(:), intent(out) :: pointCoords
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)spgp_gp

     pure function ABB($1)spgp_gi( obj )
       import $1StaticPointGroupPointerInterface
       class($1StaticPointGroupPointerInterface), intent(in) :: obj
       integer :: ABB($1)spgp_gi
     end function ABB($1)spgp_gi
     }})

     
  MACRO({SIGNATUREDEFS_STATICPOINTGROUPPOINTER_CONCRETE}, {{
  EXPAND({SIGNATUREDEFS_STATICPOINTGROUPPOINTER({$1})})

     ! a sub-row, unlike a full matrix row, is only as long as the
     ! number of coefficients in the profile.  A full matrix row may
     ! be NDIM times longer if representing a vector with coupled
     ! components.  This procedure is a pure function so we can call
     ! it from a forall block if needed.
     pure function ABB($1)spgp_csr( obj, LWR($2)SpatialDerivative, &
          nPoints ) result ( rows )
       import $1StaticPointGroupPointerInterface, &
            $2SpatialDerivativeInterface, FLOAT, NCOEFS_ADV
       class($1StaticPointGroupPointerInterface), intent(in) :: obj
       class($2SpatialDerivativeInterface), intent(in) :: &
            LWR($2)SpatialDerivative
       integer, intent(in) :: nPoints
       ! nPoints is needed explicitly because the compiler has trouble
       ! with the object's accessor method appearing in the automatic
       ! array declaration
       real(FLOAT), dimension(nPoints, NCOEFS_ADV) :: rows
     end function ABB($1)spgp_csr
     }})


  MACRO({SIGNATUREDEFS_MOMENTSOURCE},{{
     subroutine ABB($3$1$2)_di( obj, log )
       import $3$1$2Interface, LogType
       class($3$1$2Interface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($3$1$2)_di {}m4_ifelse($1, {Vector}, {
     
     pure function ABB($3$1$2)_hcc( obj )
       import $3$1$2Interface
       class($3$1$2Interface), intent(in) :: obj
       logical :: ABB($3$1$2)_hcc
     end function ABB($3$1$2)_hcc})
     }})


  MACRO({SIGNATUREDEF_SUBMOMENTSOURCE_APPENDROWS}, {{
     ! I tried to get this signature down to one argument, but it is
     ! difficult.  The object should indeed take responsibility for
     ! computing matrix rows, because it may involve spatial derivatives
     ! and, in the case of a vector profile, it will know whether the
     ! components are coupled or not.  But it also needs to know point
     ! coordinates via LWR($3StaticPointGroupPointer).  Perhaps we could
     ! make the object just responsible for computing matrix rows, not
     ! appending to the linear system, in which case we would need to give
     ! the caller (a PointMomentGroup) more power to differentiate between
     ! coupled and segregated components.  However, there are more
     ! critical issues to attend to.
     subroutine ABB($4$1$2)_ar_{}ABB($5)( obj, &
          LWR($1$5), LWR($3)StaticPointGroupPointer, log )
       import $4$1$2Interface, &
            $1$5Interface, &
            $3StaticPointGroupPointerType, LogType
       class($4$1$2Interface), intent(in) :: obj
       class($1$5Interface), intent(inout) :: &
            LWR($1$5)
       class($3StaticPointGroupPointerType), intent(in) :: &
            LWR($3)StaticPointGroupPointer
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($4$1$2)_ar_{}ABB($5)
     }})
     
     
  MACRO({SIGNATUREDEFS_SUBMOMENTSOURCE},{{
     EXPAND({SIGNATUREDEFS_MOMENTSOURCE($1, $2, $4)})

     EXPAND({SIGNATUREDEF_SUBMOMENTSOURCE_APPENDROWS($1, $2, $3, $4,
     {Matrix})})
     
     EXPAND({SIGNATUREDEF_SUBMOMENTSOURCE_APPENDROWS($1, $2, $3, $4, 
     {LinearSystem})})
     }})
     
     subroutine ABB($4$1$2)_aeq( obj, &
          LWR($1LinearSystem), LWR($3)StaticPointGroupPointer, &
          log )
       import $4$1$2Interface, &
            $1LinearSystemInterface, &
            $3StaticPointGroupPointerType, LogType
       class($4$1$2Interface), intent(in) :: obj
       class($1LinearSystemInterface), intent(inout) :: &
            LWR($1LinearSystem)
       type($3StaticPointGroupPointerType), intent(in) :: &
            LWR($3)StaticPointGroupPointer
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($4$1$2)_aeq
     }})

     
  MACRO({SIGNATUREDEFS_MOMENTFIELD},{{
     EXPAND({SIGNATUREDEFS_SUBMOMENTSOURCE({$1}, {MomentField},
     {Interior}, {$2})})
          
     pure subroutine ABB($2$1)mf_sf_u( obj, src )
       import $2$1MomentFieldInterface, IMPORT_TENSOR_PRIMITIVE($1)
       class($2$1MomentFieldInterface), intent(out) :: obj
       TYPE_TENSOR_PRIMITIVE($1), intent(in) :: src
     end subroutine ABB($2$1)mf_sf_u

     pure subroutine ABB($2$1)mf_sf_a( obj, src )
       import $2$1MomentFieldInterface, IMPORT_TENSOR_PRIMITIVE($1)
       class($2$1MomentFieldInterface), intent(out) :: obj
       TYPE_TENSOR_PRIMITIVE($1), dimension(:), intent(in) :: src
     end subroutine ABB($2$1)mf_sf_a
          
!!$     pure subroutine ABB($2$1)mf_sf_fs( obj, src )
!!$       import $2$1MomentFieldInterface, $1FieldSpecificationInterface
!!$       class($2$1MomentFieldInterface), intent(out) :: obj
!!$       class($1FieldSpecificationInterface), intent(in) :: src
!!$     end subroutine ABB($2$1)mf_sf_fs
     }})

     
  MACRO({SIGNATUREDEFS_BOUNDARYCONDITION},{{
     EXPAND({SIGNATUREDEFS_SUBMOMENTSOURCE({$1}, {BoundaryCondition},
     {Boundary}, {$2})})
     }})


!!$     MACRO({SIGNATUREDEFS_RHSGENERATOR_ABSTRACT},{{
!!$     pure subroutine ABB($1RHSGenerator)_di( obj, log )
!!$       import $1RHSGeneratorInterface, LogType
!!$       class($1RHSGeneratorInterface), intent(inout) :: obj
!!$       class(LogType), intent(inout), optional :: log
!!$     end subroutine ABB($1RHSGenerator)_di
!!$     
!!$     pure subroutine ABB($1RHSGenerator)_gvs( obj, values &
!!$          ARGLINE_TENSOR_COMPONENT($1) )
!!$       import $1RHSGeneratorInterface, FLOAT
!!$       class($1RHSGeneratorInterface), intent(in) :: obj
!!$       real(FLOAT), dimension(:), intent(out) :: values
!!$       ARGLIST_TENSOR_COMPONENT($1)
!!$     end subroutine ABB($1RHSGenerator)_gvs
!!$     
!!$     pure function ABB($1RHSGenerator)_gne( obj )
!!$       import $1RHSGeneratorInterface
!!$       class($1RHSGeneratorInterface), intent(in) :: obj
!!$       integer :: ABB($1RHSGenerator)_gne
!!$     end function ABB($1RHSGenerator)_gne
!!$     }})
     
     
  MACRO({SIGNATUREDEFS_MOMENTGROUP},{{
    pure subroutine ABB($1)mg_di( obj, log )
       import $1MomentGroupInterface, LogType
       class($1MomentGroupInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)mg_di
     
     pure subroutine ABB($1)mg_c( obj, tgt, log )
       import $1MomentGroupInterface, LogType
       class($1MomentGroupInterface), intent(inout) :: obj
       class($1MomentGroupInterface), allocatable, intent(out) :: tgt
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)mg_c
     
     subroutine ABB($1)mg_ar( obj, &
          LWR($1Matrix), log )
       import $1MomentGroupInterface, &
            $1MatrixInterface, LogType
       class($1MomentGroupInterface), intent(in) :: obj
       class($1MatrixInterface), intent(inout) :: &
            LWR($1Matrix)
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)mg_ar

     subroutine ABB($1)mg_ael( obj, &
          LWR($1RHSColumn), log )
       import $1MomentGroupInterface, &
            $1RHSColumnInterface, LogType
       class($1MomentGroupInterface), intent(inout) :: obj
       class($1RHSColumnInterface), intent(inout) :: &
            LWR($1RHSColumn)
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)mg_ael

     subroutine ABB($1)mg_aeq( obj, &
          LWR($1LinearSystem), log )
       import $1MomentGroupInterface, &
            $1LinearSystemInterface, LogType
       class($1MomentGroupInterface), intent(inout) :: obj
       class($1LinearSystemInterface), intent(inout) :: &
            LWR($1LinearSystem)
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)mg_aeq

     elemental subroutine ABB($1)mg_gv( obj, value, &
          momentIndex ARGLINE_TENSOR_COMPONENT($1) ) 
       import $1MomentGroupInterface, FLOAT
       class($1MomentGroupInterface), intent(in) :: obj
       real(FLOAT), intent(out) :: value
       integer, intent(in) :: momentIndex
       ARGLIST_TENSOR_COMPONENT($1)
     end subroutine ABB($1)mg_gv

     pure subroutine ABB($1)mg_gvs( obj, values &
          ARGLINE_TENSOR_COMPONENT($1) ) 
       import $1MomentGroupInterface, FLOAT
       class($1MomentGroupInterface), intent(in) :: obj
       real(FLOAT), dimension(:), intent(out) :: values
       ARGLIST_TENSOR_COMPONENT($1)
     end subroutine ABB($1)mg_gvs
     }})

  
  MACRO({SIGNATUREDEFS_LINEAROPERAND},{{
     subroutine ABB($1{}DEFAULT($2, {LinearOperand}))_a( obj, &
          LWR($1)LinearOperation )
       import $1{}DEFAULT($2, {LinearOperand})Interface, &
            $1LinearOperationInterface
       class($1{}DEFAULT($2, {LinearOperand})Interface), &
            intent(inout), target :: obj
       class($1LinearOperationInterface), intent(inout) :: &
            LWR($1)LinearOperation
     end subroutine ABB($1{}DEFAULT($2, {LinearOperand}))_a
     }})

  
  MACRO({SIGNATUREDEFS_LINEAROPERATION},{{
     pure subroutine ABB($1)lotn_di( obj, log )
       import $1LinearOperationInterface, LogType
       class($1LinearOperationInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)lotn_di

     pure subroutine ABB($1)lotn_ck( obj, vacancies, log )
       import $1LinearOperationInterface, LogType
       class($1LinearOperationInterface), intent(in) :: obj
       logical, dimension(:), intent(out), optional :: vacancies
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)lotn_ck
     }})
     
     
  MACRO({SIGNATUREDEFS_SUBLINEAROPERATIONS},{{
     EXPAND({SIGNATUREDEFS_LINEAROPERATION({Pure$1})})
  
     pure function p{}ABB($1)lotn_gn( obj )
       import Pure$1LinearOperationInterface
       class(Pure$1LinearOperationInterface), intent(in) :: obj
       integer :: p{}ABB($1)lotn_gn
     end function p{}ABB($1)lotn_gn
     
     pure subroutine p{}ABB($1)lotn_p( obj, values, transposed )
       import Pure$1LinearOperationInterface, FLOAT
       class(Pure$1LinearOperationInterface), intent(in) :: obj
       real(FLOAT), dimension(:, :), intent(inout) :: values
       logical, intent(in), optional :: transposed
     end subroutine p{}ABB($1)lotn_p
     
     pure subroutine p{}ABB($1)lotn_c( obj, tgt, log )
       import Pure$1LinearOperationInterface, LogType
       class(Pure$1LinearOperationInterface), intent(inout) :: obj
       class(Pure$1LinearOperationInterface), allocatable, &
            intent(inout) :: tgt
       class(LogType), intent(inout), optional :: log
     end subroutine p{}ABB($1)lotn_c
     
     EXPAND({SIGNATUREDEFS_LINEAROPERATION({Impure$1})})

     function i{}ABB($1)lotn_gn( obj )
       import Impure$1LinearOperationInterface
       class(Impure$1LinearOperationInterface), intent(in) :: obj
       integer :: i{}ABB($1)lotn_gn
     end function i{}ABB($1)lotn_gn
     
     subroutine i{}ABB($1)lotn_p( obj, log )
       import Impure$1LinearOperationInterface, FLOAT, LogType
       class(Impure$1LinearOperationInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine i{}ABB($1)lotn_p
     }})


  MACRO({SIGNATUREDEFS_SUBLINEAROPERAND_SET}, {{
     pure subroutine ABB($1)$3( obj, $4, m4_ifelse($1, {Vector}, {&
          coupled, componentIndex, })log )
       import $1$2Interface, &
            $5, LogType
       class($1$2Interface), intent(inout) :: obj
       $6 :: &
            $4
       m4_ifelse($1, {Vector},
       {! if coupled, the resulting matrix rows will be NDIM times as long
       ! as normal.
       logical, intent(in) :: coupled
       ! if componentIndex is omitted, the resulting matrix rows will
       ! apply to all components.
       integer, intent(in), optional :: componentIndex})
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)$3
     }})

     
  MACRO({SIGNATUREDEFS_MATRIX},{{
     subroutine ABB($1)mx_di( obj, log )
       import $1MatrixInterface, LogType
       class($1MatrixInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)mx_di
     
     EXPAND({SIGNATUREDEFS_SUBLINEAROPERAND_SET($1, {Matrix}, {mx_s_r}, 
       {rows}, {FLOAT}, 
       {real(FLOAT), dimension(:, :), intent(in)})})

     EXPAND({SIGNATUREDEFS_SUBLINEAROPERAND_SET($1, {Matrix}, {mx_s_lo},
       {LWR($1)LinearOperation}, {Pure$1LinearOperationInterface},
       {class(Pure$1LinearOperationInterface), intent(in)})})
     
     subroutine ABB($1)mx_f( obj, log )
       import $1MatrixInterface, LogType
       class($1MatrixInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)mx_f
     
     subroutine ABB($1)mx_p( obj, columnVector, log )
       import $1MatrixInterface, FLOAT, LogType
       class($1MatrixInterface), intent(in) :: obj
       real(FLOAT), dimension(:, :), intent(inout) :: columnVector
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)mx_p
     
     pure subroutine ABB($1)mx_s( obj, columnVector, log )
       import $1MatrixInterface, FLOAT, LogType
       class($1MatrixInterface), intent(in) :: obj
       real(FLOAT), dimension(:, :), intent(inout) :: columnVector
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)mx_s
     
     pure function ABB($1)mx_gn( obj )
       import $1MatrixInterface
       class($1MatrixInterface), intent(in) :: obj
       integer :: ABB($1)mx_gn
     end function ABB($1)mx_gn
     
     subroutine ABB($1)mx_ptr( obj, log )
       import $1MatrixInterface, LogType
       class($1MatrixInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)mx_ptr

     EXPAND({SIGNATUREDEFS_LINEAROPERAND($1, {Matrix})})
     }})

  MACRO({SIGNATUREDEFS_RHSCOLUMN},{{
     subroutine ABB($1)rhsc_di( obj, log )
       import $1RHSColumnInterface, LogType
       class($1RHSColumnInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)rhsc_di

     EXPAND({SIGNATUREDEFS_SUBLINEAROPERAND_SET($1, {RHSColumn},
       {rhsc_s_rhsg}, {LWR($1)RHSGenerator}, {$1RHSGeneratorInterface}, 
       {class($1RHSGeneratorInterface), intent(inout)})})
       
     EXPAND({SIGNATUREDEFS_SUBLINEAROPERAND_SET($1, {RHSColumn},
       {rhsc_s_r}, {realValues}, {FLOAT}, 
       {real(FLOAT), dimension(:), intent(in)})})

     subroutine ABB($1)rhsc_sp( obj, indicies, log )
       import $1RHSColumnInterface, RHSElementInterface, LogType
       class($1RHSColumnInterface), intent(inout) :: obj
       integer, dimension(:), intent(in) :: indicies
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)rhsc_sp

     pure function ABB($1)rhsc_gn( obj, generatorIndex )
       import $1RHSColumnInterface
       class($1RHSColumnInterface), intent(in) :: obj
       integer, intent(in), optional :: generatorIndex
       integer :: ABB($1)rhsc_gn
     end function ABB($1)rhsc_gn

     pure subroutine ABB($1)rhsc_r( obj, values )
       import $1RHSColumnInterface, FLOAT
       class($1RHSColumnInterface), intent(inout) :: obj
       real(FLOAT), dimension(:, :), intent(out), optional :: values
     end subroutine ABB($1)rhsc_r

     subroutine ABB($1)rhsc_ptr( obj, log )
       import $1RHSColumnInterface, LogType
       class($1RHSColumnInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)rhsc_ptr
     
     EXPAND({SIGNATUREDEFS_LINEAROPERAND($1, {RHSColumn})})
     }})

  
  MACRO({SIGNATUREDEFS_LINEARSYSTEM},{{
     subroutine ABB($1)ls_di( obj, log )
       import $1LinearSystemInterface, LogType
       class($1LinearSystemInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)ls_di

     EXPAND({SIGNATUREDEFS_SUBLINEAROPERAND_SET($1, {LinearSystem},
       {ls_sr_r}, {rows}, {FLOAT}, 
       {real(FLOAT), dimension(:, :), intent(in)})})

     EXPAND({SIGNATUREDEFS_SUBLINEAROPERAND_SET($1, {LinearSystem},
       {ls_se_r}, {realValues}, {FLOAT}, 
       {real(FLOAT), dimension(:), intent(in)})})
       
     EXPAND({SIGNATUREDEFS_SUBLINEAROPERAND_SET($1, {LinearSystem},
       {ls_s_rhsg}, {LWR($1)RHSGenerator}, {$1RHSGeneratorInterface}, 
       {class($1RHSGeneratorInterface), intent(inout)})})
       
     EXPAND({SIGNATUREDEFS_SUBLINEAROPERAND_SET($1, {LinearSystem},
       {ls_s_lo}, {LWR($1)LinearOperation},
       {Pure$1LinearOperationInterface},
       {class(Pure$1LinearOperationInterface), intent(in)})})
     
     subroutine ABB($1)ls_f( obj, log )
       import $1LinearSystemInterface, LogType
       class($1LinearSystemInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)ls_f
     
     pure subroutine ABB($1)ls_s( obj, log )
       import $1LinearSystemInterface, LogType
       class($1LinearSystemInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)ls_s

     pure function ABB($1)ls_gn( obj ) result ( nEquations )
       import $1LinearSystemInterface
       class($1LinearSystemInterface), intent(in) :: obj
       integer :: nEquations
     end function ABB($1)ls_gn
     
     pure subroutine ABB($1)ls_( obj )
       import $1LinearSystemInterface
       class($1LinearSystemInterface), intent(inout) :: obj
     end subroutine ABB($1)ls_

     subroutine ABB($1)ls_p( obj, log )
       import $1LinearSystemInterface, LogType
       class($1LinearSystemInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)ls_p

     EXPAND({SIGNATUREDEFS_LINEAROPERAND($1, {LinearSystem})})
     }})


     MACRO({SIGNATUREDEFS_FIELDSPECIFICATION}, {{
     subroutine ABB($1)fs_di( obj, log )
       import $1FieldSpecificationInterface, LogType
       class($1FieldSpecificationInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)fs_di

     subroutine ABB($1)fs_c( obj, tgt, log )
       import $1FieldSpecificationInterface, LogType
       class($1FieldSpecificationInterface), intent(in) :: obj
       class($1FieldSpecificationInterface), allocatable, &
            intent(out) :: tgt
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)fs_c

     elemental function ABB($1)fs_gv( obj, position )
       import $1FieldSpecificationInterface, RealVectorType, &
            IMPORT_TENSOR_PRIMITIVE($1)
       class($1FieldSpecificationInterface), intent(in) :: obj
       type(RealVectorType), intent(in) :: position
       TYPE_TENSOR_PRIMITIVE($1) :: ABB($1)fs_gv
     end function ABB($1)fs_gv
     }})

     
contains

  
  !-------------------------------------------------------------------
  !- generic methods
  !-------------------------------------------------------------------

  MACRO({METHODDEF_APPENDMOMENTS}, {{
  ! This is a generic, macro-generated method for appending moments to a
  ! list based on some target flowVariable.
  subroutine append$7Moments_$1( obj, LWR($7)MomentGroupList, &
       polytopeIndex, log )
    class($1$2Type), intent(inout) :: obj
    type($7MomentGroupListType), intent(inout) :: &
         LWR($7)MomentGroupList
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    type($5$7$4ListIteratorType) :: iterator
    class($5$7$4Interface), pointer :: ABB($5$7$4)
    class($7MomentGroupInterface), allocatable :: ABB($7)mg
    class($7FlowVariableInterface), pointer :: fv, fvTgt
    logical, parameter :: DEBUG_MODE = .false.
    call beginSub( MOD_NAME, &
         'append$7Moments_$1', log )

    call LWR($7)MomentGroupList%getFlowVariable( fvTgt )
    call addEvent( .not. associated(fvTgt), FATAL, 'LWR($7)&
         &MomentGroupList has not been set a $7FlowVariable.', log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    call addEvent( DEBUG_MODE, ADVICE, 'Target flowVariable is '//&
         fvTgt%describe(), log )
    call addEvent( DEBUG_MODE, ADVICE, 'obj%{}LWR($5)$7$4List &
         &is '//str(obj%{}LWR($5)$7$4List%size())//&
         ' nodes long.', log )
    
    ! iterate over the $5$4s owned by the cells.
    ! For each $5$4, we need to create new moment(s)
    ! if it is associated with the same flowVariable.
    call iterator%init( obj%{}LWR($5)$7$4List )
    call iterator%first( ABB($5$7$4) )
    do
       if ( iterator%isDone() ) exit
       
       if ( DEBUG_MODE ) then
          call ABB($5$7$4)%getFlowVariable( fv )
          call addEvent( ADVICE, 'Current ABB($5$7$4) => '//&
               fv%describe(), log )
       end if
       
       if ( ABB($5$7$4)%&
            isAssociatedWith(fvTgt) ) then
          call addEvent( DEBUG_MODE, ADVICE, 'Adding to the list.', log )
          
          call create$3$5$7MomentGroup( ABB($7)mg, &
               $6, polytopeIndex, ABB($5$7$4), DEFAULT($8, {obj%&
               exactlyConstrained}), log )
          
          ! append to the list
          call $7MomentGroupList%append( ABB($7)mg, log )
          if ( checkSub(FATAL, log) ) then
             call endSub( log )
             return
          end if
          
       end if
       
       call iterator%next( ABB($5$7$4) )
    end do
    
    call endSub( log )
  end subroutine append$7Moments_$1
  }})

  
  MACRO({METHODDEF_APPENDVELOCITYMOMENTS_SOURCELIST}, {{
  subroutine appendVelocityMoments_$1( obj, velocityMomentGroupList, &
       polytopeIndex, log )
    class($1$2Type), intent(inout) :: obj
    type(VectorMomentGroupListType), intent(inout) :: &
         velocityMomentGroupList
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    type($5Vector$4ListIteratorType) :: iterator
    class($5Vector$4Interface), pointer :: ABB($5)vmf
    class(VectorMomentGroupInterface), allocatable :: vmg
    logical, parameter :: DEBUG_MODE = .false.
    call beginSub( MOD_NAME, &
         'appendVelocityMoments_$1', log )

    ! iterate over the $5Vector$4s owned by the points.
    ! For each $5Vector$4, we need to create new
    ! moment(s).  No matching of flow variables will be needed because
    ! we are dealing with velocity, which is unique.
    call iterator%init( obj%{}LWR($5)Velocity$4List )
    call iterator%first( ABB($5)vmf )
    do
       if ( iterator%isDone() ) exit

       call create$3$5VectorMomentGroup( vmg, &
            $6, polytopeIndex, ABB($5)vmf, &
            DEFAULT($7, {obj%exactlyConstrained}), log )
       if ( checkSub(FATAL, log) ) then
          call endSub( log )
          return
       end if
       
       ! append to the list
       call velocityMomentGroupList%append( vmg, log )
       if ( checkSub(FATAL, log) ) then
          call endSub( log )
          return
       end if

       call iterator%next( ABB($5)vmf )
    end do

    call endSub( log )
  end subroutine appendVelocityMoments_$1
  }})

  
  MACRO({METHODDEF_APPENDVELOCITYMOMENTS_SINGLESOURCE}, {{
  subroutine appendVelocityMoments_$1( obj, &
       velocityMomentGroupList, polytopeIndex, log )
    class($1$2Type), intent(inout) :: obj
    type(VectorMomentGroupListType), intent(inout) :: &
         velocityMomentGroupList
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    class(VectorMomentGroupInterface), allocatable :: vmg
    logical, parameter :: DEBUG_MODE = .false.
    call beginSub( MOD_NAME, &
         'appendVelocityMoments_$1', log )

    ! there is just one velocity field owned by the cells, so no iteration
    ! over a list is necessary.  Nor is matching of flow variables
    ! required.  Only one VectorMomentGroup needs to be created.
    call create$3$5VectorMomentGroup( vmg, $6, &
         polytopeIndex, obj%LWR($5)Velocity$4, &
         DEFAULT($7, {obj%exactlyConstrained}), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
       
    ! append to the list
    call velocityMomentGroupList%append( vmg, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    call endSub( log )
  end subroutine appendVelocityMoments_$1
  }})
     

  !-------------------------------------------------------------------
  !- SpatialDerivative methods
  !-------------------------------------------------------------------
  
  MACRO({METHODDEF_COMPUTESUBROWCOMPONENTS}, {{
  pure function computeSubRowComponents_$1( obj, coords, undiffPowers ) &
       result (rowComponents)
    ! A wrapper to computeSubRowComponentsFromRawCoords that converts
    ! coords from RealVector form to primitive array form.
    class($1SpatialDerivativeInterface), intent(in) :: obj
    type(RealVectorType), dimension(:), intent(in) :: coords
    integer, dimension(:, :), intent(in) :: undiffPowers
    real(FLOAT), dimension(NDIM, size(coords)) :: c
    real(FLOAT), dimension(NDIM, size(coords), size(undiffPowers, &
         2)) :: rowComponents
    integer :: i

    forall (i = 1:size(coords)) c(:, i) = coords(i)%getValues()
    rowComponents = obj%computeSubRowComponentsFromRawCoords( c, &
         undiffPowers )
  end function computeSubRowComponents_$1
  }})

  
  !-------------------------------------------------------------------
  !- FlowVariable methods
  !-------------------------------------------------------------------
  
  MACRO({METHODDEFS_SUBFLOWVARIABLE}, {{
  subroutine init$1FlowVariable( obj, name, log )
    class($1FlowVariableInterface), intent(inout) :: obj
    character(*), intent(in) :: name
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init$1FlowVariable', log )

    call obj%initFlowVariable( name, log )

    call endSub( log )
  end subroutine init$1FlowVariable


  subroutine deinit$1FlowVariable( obj, log )
    class($1FlowVariableInterface), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit$1FlowVariable', log )

    call obj%deinitFlowVariable( log )

    call endSub( log )
  end subroutine deinit$1FlowVariable
  }})
  
  !-------------------------------------------------------------------
  !- StaticPointCollection methods
  !-------------------------------------------------------------------
  
  MACRO({METHODDEFS_SUBSTATICPOINTCOLLECTION}, {{
  subroutine init$1StaticPointCollection( obj, pointArrangement, log )
    class($1StaticPointCollectionInterface), intent(inout) :: obj
    class(PointArrangementInterface), allocatable, intent(inout) :: &
         pointArrangement
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init$1StaticPointCollection', log )

    call obj%init$2StaticPointCollection( pointArrangement, log )
    
    call endSub( log )
  end subroutine init$1StaticPointCollection

  
  subroutine deinit$1StaticPointCollection( obj, log )
    class($1StaticPointCollectionInterface), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: deallocStat
    call beginSub( MOD_NAME, 'deinit$1StaticPointCollection', log )

    call obj%deinit$2StaticPointCollection( log )
    
    call endSub( log )
  end subroutine deinit$1StaticPointCollection
  }})

  
  !-------------------------------------------------------------------
  !- PointGroupPointer methods
  !-------------------------------------------------------------------
  

  MACRO({METHODDEFS_STATICPOINTGROUPPOINTER_CONCRETE}, {{
  elemental subroutine init_$1StaticPointGroupPointer_args( obj, &
       LWR($1StaticPointCollection), polytopeIndex )
    class($1StaticPointGroupPointerType), intent(out) :: obj
    class($1StaticPointCollectionInterface), intent(inout), target :: &
         LWR($1StaticPointCollection)
    integer, intent(in) :: polytopeIndex

    obj%LWR($1StaticPointCollection) => LWR($1StaticPointCollection)
    obj%polytopeIndex = polytopeIndex
  end subroutine init_$1StaticPointGroupPointer_args

  
  elemental subroutine init_$1StaticPointGroupPointer_copy( obj, src )
    class($1StaticPointGroupPointerType), intent(out) :: obj
    class($1StaticPointGroupPointerType), target, intent(inout) :: src

    obj%LWR($1StaticPointCollection) => src%LWR($1StaticPointCollection)
    obj%polytopeIndex = src%polytopeIndex
  end subroutine init_$1StaticPointGroupPointer_copy

  
  elemental subroutine deinit_$1StaticPointGroupPointer( obj, log )
    class($1StaticPointGroupPointerType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: deallocStat
    call beginSub( MOD_NAME, 'deinit_$1StaticPointGroupPointer', log )

    nullify(obj%LWR($1StaticPointCollection))
    obj%polytopeIndex = 0

    call endSub( log )
  end subroutine deinit_$1StaticPointGroupPointer

  
  pure function getNumPoints_$1StaticPointGroupPointer( obj )
    class($1StaticPointGroupPointerType), intent(in) :: obj
    integer :: getNumPoints_$1StaticPointGroupPointer
    getNumPoints_$1StaticPointGroupPointer = obj%&
         LWR($1StaticPointCollection)%getNumPointsPerPolytope()
  end function getNumPoints_$1StaticPointGroupPointer

  
  pure function getPolytopeIndex_$1StaticPointGroupPointer( obj )
    class($1StaticPointGroupPointerType), intent(in) :: obj
    integer :: getPolytopeIndex_$1StaticPointGroupPointer
    getPolytopeIndex_$1StaticPointGroupPointer = obj%polytopeIndex
  end function getPolytopeIndex_$1StaticPointGroupPointer

    
  pure subroutine getPositions_$1StaticPointGroupPointer( obj, pointCoords, log )
    class($1StaticPointGroupPointerType), intent(in) :: obj
    type(RealVectorType), dimension(:), intent(out) :: &
         pointCoords
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getPositions_$1StaticPointCollection', &
         log )
    
    call obj%LWR($1StaticPointCollection)%getPositions( &
         pointCoords, obj%polytopeIndex, log )
    
    call endSub( log )
  end subroutine getPositions_$1StaticPointGroupPointer
  

  pure function computeSubRows_$1StaticPointGroupPointer( obj, &
       LWR($2)SpatialDerivative, nPoints ) result ( rows )
    class($1StaticPointGroupPointerType), intent(in) :: obj
    class($2SpatialDerivativeInterface), intent(in) :: &
         LWR($2)SpatialDerivative
    integer, intent(in) :: nPoints
    real(FLOAT), dimension(nPoints, NCOEFS_ADV) :: rows
    type(RealVectorType), dimension(nPoints) :: x
    real(FLOAT), dimension(NDIM, size(x), NCOEFS_ADV) :: c
    rows = obj%LWR($1)StaticPointCollection%computeSubRows( &
         obj%polytopeIndex, LWR($2)SpatialDerivative, nPoints )
  end function computeSubRows_$1StaticPointGroupPointer
  }})

  
  !-------------------------------------------------------------------
  !- PointPointer methods
  !-------------------------------------------------------------------
  
  MACRO({METHODDEFS_POINTPOINTER_CONCRETE}, {{
  subroutine init_$1PointPointer( obj, &
       LWR($1StaticPointGroupPointer), polytopeIndex )
    class($1PointPointerType), intent(out) :: obj
    class($1StaticPointGroupPointerType), intent(in) :: &
         LWR($1StaticPointGroupPointer)
    integer :: pointIndex

    obj%LWR($1StaticPointGroupPointer) = LWR($1StaticPointGroupPointer)
    obj%pointIndex = pointIndex

  end subroutine init_$1PointPointer

  
  elemental subroutine deinit_$1PointPointer( obj, log )
    class($1PointPointerType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_$1PointPointer', log )

    call obj%LWR($1StaticPointGroupPointer)%deinit()
    obj%pointIndex = 0

    call endSub( log )
  end subroutine deinit_$1PointPointer

  
  pure function getPolytopeIndex_$1PointPointer( obj )
    class($1PointPointerType), intent(in) :: obj
    integer :: getPolytopeIndex_$1PointPointer
    getPolytopeIndex_$1PointPointer = obj%&
         LWR($1StaticPointGroupPointer)%getPolytopeIndex()
  end function getPolytopeIndex_$1PointPointer

  
  pure function getPointIndex_$1PointPointer( obj )
    class($1PointPointerType), intent(in) :: obj
    integer :: getPointIndex_$1PointPointer
    getPointIndex_$1PointPointer = obj%pointIndex
  end function getPointIndex_$1PointPointer
  }})
  

  !-------------------------------------------------------------------
  !- MomentSource methods
  !-------------------------------------------------------------------
  
  MACRO({METHODDEFS_MOMENTSOURCE}, {{
  EXPAND({PROCEDURE_DESTROY({$1MomentSource})})
  
  subroutine init$1MomentSource_args( obj, LWR($1)FlowVariable, $2log )
    class($1MomentSourceInterface), intent(inout) :: obj
    class($1FlowVariableInterface), target, intent(in) :: &
         LWR($1)FlowVariable
    class(LogType), intent(inout), optional :: log $3
    call beginSub( MOD_NAME, 'init$1MomentSource_args', log )

    obj%LWR($1)FlowVariable => LWR($1)FlowVariable
    $4
    
    call endSub( log )
  end subroutine init$1MomentSource_args

  
  subroutine init$1MomentSource_copy( obj, src, $2log )
    class($1MomentSourceInterface), intent(inout) :: obj
    class($1MomentSourceInterface), target, intent(in) :: src
    class(LogType), intent(inout), optional :: log $3
    call beginSub( MOD_NAME, 'init$1MomentSource_copy', log )

    obj%LWR($1)FlowVariable => src%LWR($1)FlowVariable
    $4
    
    call endSub( log )
  end subroutine init$1MomentSource_copy


  subroutine deinit$1MomentSource( obj, log )
    class($1MomentSourceInterface), intent(out) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit$1MomentSource', log )

    nullify(obj%LWR($1)FlowVariable)
    $5
    
    call endSub( log )
  end subroutine deinit$1MomentSource


  subroutine getFlowVariable_$1MomentSource( obj, &
       LWR($1)FlowVariable )
    class($1MomentSourceInterface), target, intent(in) :: obj
    class($1FlowVariableInterface), pointer, intent(inout) :: &
         LWR($1)FlowVariable
    LWR($1)FlowVariable => obj%LWR($1)FlowVariable
  end subroutine getFlowVariable_$1MomentSource


  pure function isAssociatedWith_FlowVar_$1MomentSource( obj, &
       LWR($1)FlowVariable )
    class($1MomentSourceInterface), intent(in) :: obj
    class($1FlowVariableInterface), intent(in) :: &
         LWR($1)FlowVariable
    logical :: isAssociatedWith_FlowVar_$1MomentSource
    isAssociatedWith_FlowVar_$1MomentSource = obj%LWR($1)FlowVariable%&
         sameAs( LWR($1)FlowVariable )
  end function isAssociatedWith_FlowVar_$1MomentSource
  }})


  
  MACRO({METHODDEFS_MOMENTFIELD}, {{
  EXPAND({PROCEDURE_DESTROY({$2$1MomentField})})
  
  subroutine init$2$1MomentField_args( obj, LWR($1)FlowVariable, &
       directedSpatialDerivative, log )
    class($2$1MomentFieldInterface), intent(inout) :: obj
    class($1FlowVariableInterface), target, intent(in) :: &
         LWR($1)FlowVariable
    class(DirectedSpatialDerivativeInterface), allocatable, intent(&
         inout) :: directedSpatialDerivative
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init$2$1MomentField_args', log )

    EXPAND({INJECT({directedSpatialDerivative})})
    call obj%init$1MomentSource( LWR($1)FlowVariable, log )
    
    call endSub( log )
  end subroutine init$2$1MomentField_args

  
  subroutine init$2$1MomentField_copy( obj, src, log )
    class($2$1MomentFieldInterface), intent(inout) :: obj
    class($2$1MomentFieldInterface), target, intent(in) :: src
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init$2$1MomentField_copy', log )

    call src%directedSpatialDerivative%clone( &
         obj%directedSpatialDerivative, log )
    call obj%init$1MomentSource( src%LWR($1)FlowVariable, log )
    
    call endSub( log )
  end subroutine init$2$1MomentField_copy


  subroutine deinit$2$1MomentField( obj, log )
    class($2$1MomentFieldInterface), intent(out) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit$2$1MomentField', log )

    call obj%deinit$1MomentSource( log )
    call destroy( obj%directedSpatialDerivative, log )
    
    call endSub( log )
  end subroutine deinit$2$1MomentField

  
  subroutine getSpatialDerivative_$1MomentField( obj, &
       directedSpatialDerivative )
    class($2$1MomentFieldInterface), target, intent(in) :: obj
    class(DirectedSpatialDerivativeInterface), pointer, &
         intent(out) :: directedSpatialDerivative
    directedSpatialDerivative => obj%directedSpatialDerivative
  end subroutine getSpatialDerivative_$1MomentField

  
  pure function isAssociatedWith_SpatialDeriv_$1MomentField( obj, &
       directedSpatialDerivative )
    class($2$1MomentFieldInterface), intent(in) :: obj
    class(DirectedSpatialDerivativeInterface), intent(in) :: &
         directedSpatialDerivative
    logical :: isAssociatedWith_SpatialDeriv_$1MomentField
    isAssociatedWith_SpatialDeriv_$1MomentField = &
         obj%directedSpatialDerivative%sameAs( directedSpatialDerivative )
  end function isAssociatedWith_SpatialDeriv_$1MomentField
  }})

  
  MACRO({METHODDEFS_BOUNDARYCONDITION}, {{
  EXPAND({PROCEDURE_DESTROY({$2$1BoundaryCondition})})
  
  subroutine init$2$1BoundaryCondition_args( obj, LWR($1)FlowVariable, log )
    class($2$1BoundaryConditionInterface), intent(inout) :: obj
    class($1FlowVariableInterface), target, intent(in) :: &
         LWR($1)FlowVariable
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init$2$1BoundaryCondition_args', log )

    call obj%init$1MomentSource( LWR($1)FlowVariable, log )
    
    call endSub( log )
  end subroutine init$2$1BoundaryCondition_args

  
  subroutine init$2$1BoundaryCondition_copy( obj, src, log )
    class($2$1BoundaryConditionInterface), intent(inout) :: obj
    class($2$1BoundaryConditionInterface), target, intent(in) :: src
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init$2$1BoundaryCondition_copy', log )

    call obj%init$1MomentSource( src, log )
    
    call endSub( log )
  end subroutine init$2$1BoundaryCondition_copy


  subroutine deinit$2$1BoundaryCondition( obj, log )
    class($2$1BoundaryConditionInterface), intent(out) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit$2$1BoundaryCondition', log )

    call obj%deinit$1MomentSource( log )
    
    call endSub( log )
  end subroutine deinit$2$1BoundaryCondition
  }})
  

  !-------------------------------------------------------------------
  !- MomentSource subclass methods
  !-------------------------------------------------------------------

  MACRO({METHODDEFS_SUBMOMENTFIELD}, {{
  EXPAND({PROCEDURE_DESTROY({$2$1MomentField})})
  
  subroutine init$2$1MomentField_args( obj, LWR($1)FlowVariable, &
       directedSpatialDerivative, log )
    class($2$1MomentFieldInterface), intent(inout) :: obj
    class($1FlowVariableInterface), target, intent(in) :: &
         LWR($1)FlowVariable
    class(DirectedSpatialDerivativeInterface), allocatable, intent(&
         inout) :: directedSpatialDerivative
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init$2$1MomentField_args', log )

    call obj%init$1MomentField( LWR($1)FlowVariable, &
         directedSpatialDerivative, log )
    
    call endSub( log )
  end subroutine init$2$1MomentField_args

  
  subroutine init$2$1MomentField_copy( obj, src, log )
    class($2$1MomentFieldInterface), intent(inout) :: obj
    class($2$1MomentFieldInterface), target, intent(in) :: src
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init$2$1MomentField_copy', log )

    call obj%init$1MomentField( src, log )
    
    call endSub( log )
  end subroutine init$2$1MomentField_copy


  subroutine deinit$2$1MomentField( obj, log )
    class($2$1MomentFieldInterface), intent(out) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit$2$1MomentField', log )

    call obj%deinit$1MomentField( log )

    call endSub( log )
  end subroutine deinit$2$1MomentField
  }})


  !-------------------------------------------------------------------
  !- MomentGroup methods
  !-------------------------------------------------------------------
  
  MACRO({METHODDEFS_MOMENTGROUP}, {{
  EXPAND({PROCEDURE_DESTROY({$1MomentGroup})})
  
  pure subroutine init$1MomentGroup( obj, nMoments, exactlyConstrained )
    class($1MomentGroupInterface), intent(out) :: obj
    integer, intent(in) :: nMoments
    logical, intent(in) :: exactlyConstrained
    obj%nMoments = nMoments
    obj%exactlyConstrained = exactlyConstrained
  end subroutine init$1MomentGroup
  

  pure subroutine deinit$1MomentGroup( obj )
    class($1MomentGroupInterface), intent(inout) :: obj
    obj%nMoments = 0
    obj%exactlyConstrained = .true.
  end subroutine deinit$1MomentGroup
  
  
  ! if exactlyConstrained is present, and the exactlyConstrained
  ! status of the MG does not match it, 0 will be returned
  elemental function getNumMoments_$1MomentGroup( obj, &
       exactlyConstrained ARGLINE_TENSOR_COUPLED($1) )
    class($1MomentGroupInterface), intent(in) :: obj
    logical, intent(in), optional :: exactlyConstrained
    ARGLIST_TENSOR_COUPLED($1)
    integer :: getNumMoments_$1MomentGroup
    if ( present(exactlyConstrained) ) then
       if ( obj%exactlyConstrained /= exactlyConstrained ) then
          getNumMoments_$1MomentGroup = 0
          return
       end if
    end if
    m4_ifelse($1, {Vector}, {
    if ( present(coupledComponents) ) then
       if ( obj%coupledComponents /= coupledComponents ) then
          getNumMoments_$1MomentGroup = 0
          return
       end if
    end if})
    getNumMoments_$1MomentGroup = obj%nMoments
  end function getNumMoments_$1MomentGroup
  }})

  
  MACRO({METHODDEFS_CONTAINER_MOMENTGROUP}, {{
  ! This container inludes an embedded link to some $1FlowVariable 
  EXPAND({METHODDEFS_CONTAINER({$1MomentGroup},
  {Interface}, {List}, {nullify(obj%LWR($1)FlowVariable)})})
  
  subroutine setFlowVariable_$1MomentGroupList( obj, &
       LWR($1)FlowVariable )
    class($1MomentGroupListType), intent(inout) :: obj
    class($1FlowVariableInterface), target, intent(in) :: &
         LWR($1)FlowVariable
    obj%LWR($1)FlowVariable => LWR($1)FlowVariable
  end subroutine setFlowVariable_$1MomentGroupList
  
  subroutine getFlowVariable_$1MomentGroupList( obj, &
       LWR($1)FlowVariable )
    class($1MomentGroupListType), intent(in) :: obj
    class($1FlowVariableInterface), pointer, intent(out) :: &
         LWR($1)FlowVariable
    LWR($1)FlowVariable => obj%LWR($1)FlowVariable
  end subroutine getFlowVariable_$1MomentGroupList
  
  function countNodes_$1MomentGroupList( obj, &
       exactlyConstrained ARGLINE_TENSOR_COUPLED($1) )
    class($1MomentGroupListType), intent(inout) :: obj
    logical, intent(in), optional :: exactlyConstrained
    ARGLIST_TENSOR_COUPLED($1)
    integer :: countNodes_$1MomentGroupList
    type($1MomentGroupListIteratorType) :: iterator
    class($1MomentGroupInterface), pointer :: ABB($1)mg
    call iterator%init( obj )
    countNodes_$1MomentGroupList = 0

    ! this basically returns the size of the list unless the
    ! exactlyConstrained filter is used
    
    call iterator%first( ABB($1)mg )
    do
       if ( iterator%isDone() ) exit

       if ( ABB($1)mg%getNumMoments(exactlyConstrained &
            ARGLINE_TENSOR_COUPLED($1)) > 0 ) &
            countNodes_$1MomentGroupList = &
            countNodes_$1MomentGroupList + 1
       
       call iterator%next( ABB($1)mg )
    end do

  end function countNodes_$1MomentGroupList

  function countMoments_$1MomentGroupList( obj, &
       exactlyConstrained ARGLINE_TENSOR_COUPLED($1) )
    class($1MomentGroupListType), intent(inout) :: obj
    logical, intent(in), optional :: exactlyConstrained
    ARGLIST_TENSOR_COUPLED($1)
    integer :: countMoments_$1MomentGroupList
    type($1MomentGroupListIteratorType) :: iterator
    class($1MomentGroupInterface), pointer :: ABB($1)mg
    call iterator%init( obj )
    countMoments_$1MomentGroupList = 0
    
    call iterator%first( ABB($1)mg )
    do
       if ( iterator%isDone() ) exit
       countMoments_$1MomentGroupList = &
            countMoments_$1MomentGroupList + &
            ABB($1)mg%getNumMoments( exactlyConstrained &
            ARGLINE_TENSOR_COUPLED($1) )
       
       call iterator%next( ABB($1)mg )
    end do

  end function countMoments_$1MomentGroupList
  }})
    
  
  !-------------------------------------------------------------------
  !- MomentPointer methods
  !-------------------------------------------------------------------

  MACRO({METHODDEFS_MOMENTPOINTER}, {{  
  EXPAND({PROCEDURE_DESTROY_ARRAY({$1MomentPointer}, {Type})})

  ! unbound procedure to overload the assignment(=) operator
  subroutine assign_$1MomentPointer( tgt, src )
    type($1MomentPointerType), intent(out) :: tgt
    class($1MomentPointerType), intent(in) :: src
    tgt%ABB($1MomentGroup) => src%ABB($1MomentGroup)
    tgt%momentIndex = src%momentIndex
  end subroutine assign_$1MomentPointer

  
  subroutine init_$1MomentPointer( obj, LWR($1MomentGroup), &
       momentIndex )
    class($1MomentPointerType), intent(out) :: obj
    class($1MomentGroupInterface), target, intent(in) :: &
         LWR($1MomentGroup)
    integer, intent(in) :: momentIndex
    obj%ABB($1MomentGroup) => LWR($1MomentGroup)
    obj%momentIndex = momentIndex
  end subroutine init_$1MomentPointer

  
  pure subroutine deinit_$1MomentPointer( obj, log )
    class($1MomentPointerType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_$1MomentPointer', log )
    nullify(obj%ABB($1MomentGroup))
    obj%momentIndex = 0
    call endSub( log )
  end subroutine deinit_$1MomentPointer

  
  elemental subroutine getValue_$1MomentPointer( obj, value )
    class($1MomentPointerType), intent(in) :: obj
    real(FLOAT), intent(out) :: value
    call obj%ABB($1MomentGroup)%&
         getValue( value, obj%momentIndex m4_ifelse($1, {Vector},
    {, obj%componentIndex}, {}) )
  end subroutine getValue_$1MomentPointer

  
  subroutine clone_$1MomentPointer( obj, tgt, log )
    class($1MomentPointerType), intent(in) :: obj
    class(RHSElementInterface), allocatable, intent(out) :: tgt
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'clone_MomentPointer', log )
    
    allocate( $1MomentPointerType :: tgt, stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &$1MomentPointerType :: tgt.  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    select type ( tgt )
    type is ($1MomentPointerType)
       call tgt%init( obj%ABB($1MomentGroup), obj%momentIndex )
    end select

    call endSub( log )
  end subroutine clone_$1MomentPointer
  }})


  !-------------------------------------------------------------------
  !- Linear Operation methods
  !-------------------------------------------------------------------

  MACRO({METHODDEFS_LINEAROPERATION}, {{
  subroutine ignore_$1( obj, array, from )
    class($1LinearOperationInterface), intent(inout) :: obj
    real(FLOAT), dimension(:, :), target, intent(inout) :: array
    integer, dimension(2), intent(in), optional :: from
    ! this method is a stub to be overwritten.  Do nothing.
  end subroutine ignore_$1

  pure subroutine setScaleFactor_$1( obj, value )
    class(Pure$1LinearOperationInterface), intent(inout) :: obj
    real(FLOAT), intent(in) :: value
    obj%scaleFactor = value
  end subroutine setScaleFactor_$1

  pure function getScaleFactor_$1( obj )
    class(Pure$1LinearOperationInterface), intent(in) :: obj
    real(FLOAT) :: getScaleFactor_$1
    getScaleFactor_$1 = obj%scaleFactor
  end function getScaleFactor_$1
  }})

  
  !-------------------------------------------------------------------
  !- Matrix, RHS, LinearSystem methods
  !-------------------------------------------------------------------
  
  MACRO({METHODDEFS_MATRIX}, {{
  EXPAND({PROCEDURE_DESTROY({$1Matrix})})
  }})
  
  MACRO({METHODDEFS_RHSCOLUMN}, {{
  EXPAND({PROCEDURE_DESTROY({$1RHSColumn})})
  }})

  MACRO({METHODDEF_LINEARSYSTEM_SETELEMENTS_LINOP}, {{
!!$  subroutine setRHSElements_LinOp_$1LinearSystem( obj, &
!!$       LWR($1)LinearOperation, m4_ifelse($1, {Vector}, {&
!!$          coupled, componentIndex, })log )
!!$    class($1LinearSystemInterface), intent(inout) :: obj
!!$    class(Pure$1LinearOperationInterface), intent(in) :: &
!!$         LWR($1)LinearOperation
!!$    m4_ifelse($1, {Vector},
!!$    {! if coupled, the resulting matrix rows will be NDIM times as long
!!$    ! as normal.
!!$    logical, intent(in) :: coupled
!!$    ! if componentIndex is omitted, the resulting matrix rows will
!!$    ! apply to all components.
!!$    integer, intent(in), optional :: componentIndex})
!!$    class(LogType), intent(inout), optional :: log 
!!$    type($1LinOpBasedRHSGeneratorType) :: rhsg
!!$    call beginSub( MOD_NAME, 'setRHSElements_LinOp_$1LinearSystem', &
!!$         log )
!!$
!!$    ! this method simply involves converting the argument to another type
!!$    ! before passing to a more general method
!!$    call rhsg%init( LWR($1)LinearOperation, log )
!!$    call obj%setRHSElements( rhsg, m4_ifelse($1, {Vector}, {&
!!$          coupled, componentIndex, }) log )
!!$    call rhsg%deinit( log )
!!$    
!!$    call endSub( log )
!!$  end subroutine setRHSElements_LinOp_$1LinearSystem
  }})
  
  
  MACRO({METHODDEFS_LINEARSYSTEM}, {{
  EXPAND({PROCEDURE_DESTROY({$1LinearSystem})})
  
  subroutine setRHSElements_LinOp_$1LinearSystem( obj, &
       LWR($1)LinearOperation, m4_ifelse($1, {Vector}, {&
          coupled, componentIndex, })log )
    class($1LinearSystemInterface), intent(inout) :: obj
    class(Pure$1LinearOperationInterface), intent(inout) :: &
         LWR($1)LinearOperation
    m4_ifelse($1, {Vector},
    {! if coupled, the resulting matrix rows will be NDIM times as long
    ! as normal.
    logical, intent(in) :: coupled
    ! if componentIndex is omitted, the resulting matrix rows will
    ! apply to all components.
    integer, intent(in), optional :: componentIndex})
    class(LogType), intent(inout), optional :: log 
    type($1LinOpBasedRHSGeneratorType) :: rhsg
    call beginSub( MOD_NAME, &
         'setRHSElements_LinOp_$1LinearSystem', log ) 

    ! check operands.  Since perform() is not being called immediately,
    ! allow method to proceed without exiting early.  It is the
    ! responsibility of the client to check the log before calling
    ! refresh() on the RHS.
    call LWR($1)LinearOperation%check( log=log )
    
    ! this method simply involves converting the argument to another type
    ! before passing to a more general method
    call rhsg%init( LWR($1)LinearOperation, log )
    call obj%setRHSElements( rhsg, m4_ifelse($1, {Vector}, {&
          coupled, componentIndex, }) log )
    call rhsg%deinit( log )
    
    call endSub( log )
  end subroutine setRHSElements_LinOp_$1LinearSystem
  }})
    
  
  !-------------------------------------------------------------------
  !- RHSGenerator methods
  !-------------------------------------------------------------------
  
  MACRO({METHODDEFS_RHSGENERATOR_MOMGPBASED}, {{
  EXPAND({PROCEDURE_CREATE({$1MomGpBasedRHSGenerator}, {$1RHSGenerator}, {&
         LWR($1)MomentGroup, },
    {class($1MomentGroupInterface), target, intent(inout) :: &
         LWR($1)MomentGroup})})
  EXPAND({PROCEDURES_CREATE_ARRAY({$1MomGpBasedRHSGenerator},
    {$1RHSGenerator}, {&
         LWR($1)MomentGroup, },
    {class($1MomentGroupInterface), target, intent(inout) :: &
         LWR($1)MomentGroup})})
    
  pure subroutine init_args_$1MomGpBasedRHSGenerator( obj, &
       LWR($1)MomentGroup, log )
    class($1MomGpBasedRHSGeneratorType), intent(out) :: obj
    class($1MomentGroupInterface), target, intent(inout) :: &
         LWR($1)MomentGroup
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_args_$1MomGpBasedRHSGenerator', log )

    obj%LWR($1)MomentGroup => LWR($1)MomentGroup

    call endSub( log )
  end subroutine init_args_$1MomGpBasedRHSGenerator


  pure subroutine init_src_conc_$1MomGpBasedRHSGenerator( obj, src, log )
    class($1MomGpBasedRHSGeneratorType), intent(out) :: obj
    class($1RHSGeneratorInterface), intent(in) :: src
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_src_conc_$1MomGpBasedRHSGenerator', log )
    select type ( src )
       
    class is ($1MomGpBasedRHSGeneratorType)
       call obj%init( src%LWR($1)MomentGroup, log )
       
    class default
       call addEvent( WARNING, 'Classes do not match.', log )
    end select
    call endSub( log )
  end subroutine init_src_conc_$1MomGpBasedRHSGenerator
  }})

  
  MACRO({METHODDEFS_RHSGENERATOR_LINOPBASED}, {{
  EXPAND({PROCEDURE_CREATE({$1LinOpBasedRHSGenerator}, {$1RHSGenerator}, {&
         pure$1LinearOperation, },
    {class(Pure$1LinearOperationInterface), intent(inout) :: &
         pure$1LinearOperation})})
  EXPAND({PROCEDURES_CREATE_ARRAY({$1LinOpBasedRHSGenerator},
  {$1RHSGenerator}, {&
         pure$1LinearOperation, },
    {class(Pure$1LinearOperationInterface), intent(inout) :: &
         pure$1LinearOperation})})
    
  pure subroutine init_args_$1LinOpBasedRHSGenerator( obj, &
       pure$1LinearOperation_arg, log )
    class($1LinOpBasedRHSGeneratorType), intent(out) :: obj
    class(Pure$1LinearOperationInterface), intent(inout) :: &
         pure$1LinearOperation_arg
    class(LogType), intent(inout), optional :: log
    class(Pure$1LinearOperationInterface), allocatable :: &
         pure$1LinearOperation
    call beginSub( MOD_NAME, 'init_args_$1LinOpBasedRHSGenerator', log )

    ! do not inject the argument, because it may belong to something else.
    ! Instead, clone it and inject the clone.
    call pure$1LinearOperation_arg%clone( &
         pure$1LinearOperation, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    EXPAND({INJECT({pure$1LinearOperation})})
    
    call endSub( log )
  end subroutine init_args_$1LinOpBasedRHSGenerator


  pure subroutine init_src_conc_$1LinOpBasedRHSGenerator( obj, src, log )
    class($1LinOpBasedRHSGeneratorType), intent(out) :: obj
    class($1RHSGeneratorInterface), intent(inout) :: src
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_src_conc_$1LinOpBasedRHSGenerator', log )
    select type ( src )
       
    class is ($1LinOpBasedRHSGeneratorType)
       call obj%init( src%pure$1LinearOperation, log )
       
    class default
       call addEvent( WARNING, 'Classes do not match.', log )
    end select
    call endSub( log )
  end subroutine init_src_conc_$1LinOpBasedRHSGenerator
  }})


  
  MACRO({METHODDEFS_RHSGENERATOR_ABSTRACT}, {{
  EXPAND({PROCEDURE_DESTROY($1RHSGenerator)})
  
  EXPAND({PROCEDURE_DESTROY_ARRAY({$1RHSGenerator}, {Interface})})

  
  pure subroutine init_src_abs_$1RHSGenerator( obj, src, log )
    class($1RHSGeneratorInterface), intent(out) :: obj
    class($1RHSGeneratorInterface), intent(inout) :: src
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_src_abs_$1RHSGenerator', log )
    select type ( obj )
       
    class is ($1MomGpBasedRHSGeneratorType)
       call obj%init( src, log )
       
    class is ($1LinOpBasedRHSGeneratorType)
       call obj%init( src, log )
       
    end select
    call endSub( log )
  end subroutine init_src_abs_$1RHSGenerator
  
  
  pure subroutine deinit_$1RHSGenerator( obj, log )
    class($1RHSGeneratorInterface), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'deinit_$1RHSGenerator', log )
    select type ( obj )
       
    class is ($1MomGpBasedRHSGeneratorType)
       nullify(obj%LWR($1)MomentGroup)
       
    class is ($1LinOpBasedRHSGeneratorType)
       deallocate( obj%pure$1LinearOperation, stat=stat )
       call addEvent( stat/=0, WARNING, 'Problem deallocating obj%&
            &pure$1LinearOperation.  STAT='//int2str(stat), log )
       
    end select
    call endSub( log )
  end subroutine deinit_$1RHSGenerator
  
  
  pure subroutine getValues_$1RHSGenerator( obj, values &
       ARGLINE_TENSOR_COMPONENT($1) )
    class($1RHSGeneratorInterface), intent(in) :: obj
    real(FLOAT), dimension(:, :), intent(out) :: values
    ARGLIST_TENSOR_COMPONENT($1)
    select type ( obj )
       
    class is ($1MomGpBasedRHSGeneratorType)
       call obj%LWR($1)MomentGroup%getValues( values(:, 1) &
            ARGLINE_TENSOR_COMPONENT($1) )
       
    class is ($1LinOpBasedRHSGeneratorType)
       call obj%pure$1LinearOperation%perform( values )
       
    end select
  end subroutine getValues_$1RHSGenerator

  
  elemental function getNumElements_$1RHSGenerator( obj )
    class($1RHSGeneratorInterface), intent(in) :: obj
    integer :: getNumElements_$1RHSGenerator
    select type ( obj )
       
    class is ($1MomGpBasedRHSGeneratorType)
       getNumElements_$1RHSGenerator = &
            obj%LWR($1)MomentGroup%getNumMoments()
       
    class is ($1LinOpBasedRHSGeneratorType)
       getNumElements_$1RHSGenerator = &
            obj%pure$1LinearOperation%getNumRows()
       
    end select
  end function getNumElements_$1RHSGenerator

  
  elemental function isInitialised_$1RHSGenerator( obj )
    class($1RHSGeneratorInterface), intent(in) :: obj
    logical :: isInitialised_$1RHSGenerator
    select type ( obj )
       
    class is ($1MomGpBasedRHSGeneratorType)
       isInitialised_$1RHSGenerator = &
            associated(obj%LWR($1)MomentGroup)
       
    class is ($1LinOpBasedRHSGeneratorType)
       isInitialised_$1RHSGenerator = &
            allocated(obj%pure$1LinearOperation)
       
    end select
  end function isInitialised_$1RHSGenerator
    
  
  pure subroutine allocArray_$1RHSGenerator( obj, rhsGenerators, &
       nElements, log )
    class($1RHSGeneratorInterface), intent(in) :: obj
    class($1RHSGeneratorInterface), dimension(:), allocatable, intent(&
         inout) :: rhsGenerators
    integer, intent(in) :: nElements
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'allocArray_$1RHSGenerator', log )
    select type ( obj )
       
    class is ($1MomGpBasedRHSGeneratorType)
       allocate( $1MomGpBasedRHSGeneratorType :: rhsGenerators(&
            nElements), stat=stat )
       call addEvent( stat/=0, FATAL, 'Problem allocating &
            &$1MomGpBasedRHSGeneratorType :: rhsGenerators(nElements).  &
            &STAT='//int2str(stat), log )
       
    class is ($1LinOpBasedRHSGeneratorType)
       allocate( $1LinOpBasedRHSGeneratorType :: rhsGenerators(&
            nElements), stat=stat )
       call addEvent( stat/=0, FATAL, 'Problem allocating &
            &$1LinOpBasedRHSGeneratorType :: rhsGenerators(nElements).  &
            &STAT='//int2str(stat), log )
       
    end select
    call endSub( log )
  end subroutine allocArray_$1RHSGenerator
  }})

  !-------------------------------------------------------------------
  !- FieldSpecification methods
  !-------------------------------------------------------------------
  
  MACRO({METHODDEFS_FIELDSPECIFICATION}, {{
  EXPAND({PROCEDURE_DESTROY({$1FieldSpecification})})
  
  subroutine init$1FieldSpecification( obj, LWR($1)FlowVariable, &
       directedSpatialDerivative, log )
    class($1FieldSpecificationInterface), intent(inout) :: obj
    class($1FlowVariableInterface), target, intent(in) :: &
         LWR($1)FlowVariable
    class(DirectedSpatialDerivativeInterface), allocatable, intent(&
         inout) :: directedSpatialDerivative
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_$1FieldSpecification', log )

    obj%LWR($1)FlowVariable => LWR($1)FlowVariable 
    EXPAND({INJECT({directedSpatialDerivative})})

    call endSub( log )
  end subroutine init$1FieldSpecification


  subroutine deinit$1FieldSpecification( obj, log )
    class($1FieldSpecificationInterface), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_$1FieldSpecification', log )

    call destroy( obj%directedSpatialDerivative, log )
    nullify(obj%LWR($1)FlowVariable)
    
    call endSub( log )
  end subroutine deinit$1FieldSpecification

  
  subroutine getValues_$1FieldSpecification( obj, &
       values, positions )
    class($1FieldSpecificationInterface), intent(in) :: obj
    TYPE_TENSOR_PRIMITIVE($1), dimension(:), intent(out)  :: values
    type(RealVectorType), dimension(:), intent(in) :: positions
    integer :: i
    forall ( i = 1:size(positions) )
       values(i) = obj%getValue( positions(i) )
    end forall
  end subroutine getValues_$1FieldSpecification
  

  pure function isAssociatedWith_$1FieldSpecification( obj, &
       LWR($1)MomentField )
    class($1FieldSpecificationInterface), intent(in) :: obj
    class($1MomentFieldInterface), intent(in) :: &
         LWR($1)MomentField
    logical :: isAssociatedWith_$1FieldSpecification
    isAssociatedWith_$1FieldSpecification = &
         LWR($1)MomentField%isAssociatedWith( &
         obj%directedSpatialDerivative ) &
         .and. &
         LWR($1)MomentField%isAssociatedWith( &
         obj%LWR($1)FlowVariable )
  end function isAssociatedWith_$1FieldSpecification
  }})


end module FlowField_Macros
