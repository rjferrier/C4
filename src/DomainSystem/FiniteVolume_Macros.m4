module FiniteVolume_Macros

  USE_MACROS({FlowField})
  
  !-------------------------------------------------------------------
  !- PolytopeCollectionInterface
  !-------------------------------------------------------------------

  ! abbreviation of Complex conflicts with that of Collection.
  ! Redefine so that the former abbreviates to "x".
  m4_define({ABB}, {m4_patsubst(
  {m4_translit({DOWNCASE({$*})}, {a-z}, {})},
  {Complex}, {X})})
  

  MACRO({SIGNATURE_POLYTOPECOLLECTION_SETEXTENSION}, {{
     procedure(p{}ABB($1)_se_{}ABB($2)), deferred, private :: &
          setExtension_$2s
     }})
     
  MACRO({SIGNATURE_POLYTOPECOLLECTION_GETEXTENSION}, {{
     procedure(p{}ABB($1)_g{}ABB($3)e_{}ABB($2)), deferred, private :: &
          get{}$3Extension_$2s
     }})
     
  MACRO({SIGNATURELIST_POLYTOPECOLLECTION_EXTENSIONS}, {{
     EXPAND({SIGNATURE_POLYTOPECOLLECTION_GETEXTENSION($1, {Face},
     m4_ifelse($2, {}, $1, $2))})
     EXPAND({SIGNATURE_POLYTOPECOLLECTION_GETEXTENSION($1, {Cell},
     m4_ifelse($2, {}, $1, $2))})
     }})
     
  MACRO({SIGNATURELIST_SUBPOLYTOPECOLLECTION_EXTENSIONS}, {{
     EXPAND({SIGNATURE_POLYTOPECOLLECTION_SETEXTENSION($1, {Face})})
     EXPAND({SIGNATURE_POLYTOPECOLLECTION_SETEXTENSION($1, {Cell})})
     EXPAND({SIGNATURELIST_POLYTOPECOLLECTION_EXTENSIONS($1,
     {Collection})})
     ! note that when this sub-interface is visible to the client, the
     ! use of get$1Extension is preferred.  getCollectionExtension is
     ! really only provided for PolytopePointers and other such
     ! clients that cannot distinguish between Arrays and Grids.
     EXPAND({SIGNATURE_POLYTOPECOLLECTION_GETEXTENSION($1, {Face}, $1)})
     EXPAND({SIGNATURE_POLYTOPECOLLECTION_GETEXTENSION($1, {Cell}, $1)})
     }})
     
  MACRO({SIGNATURELIST_POLYTOPEOTHER_EXTENSIONS}, {{
     EXPAND({SIGNATURE_POLYTOPECOLLECTION_SETEXTENSION($1, {Face})})
     EXPAND({SIGNATURE_POLYTOPECOLLECTION_SETEXTENSION($1, {Cell})})
     EXPAND({SIGNATURE_POLYTOPECOLLECTION_GETEXTENSION($1, {Face}, $1)})
     EXPAND({SIGNATURE_POLYTOPECOLLECTION_GETEXTENSION($1, {Cell}, $1)})
     }})

     
  !-------------------------------------------------------------------
  !- Polytope super extensions
  !-------------------------------------------------------------------

  ! these sub-macros will be defined by the client just before the
  ! expansion of {TYPEDEF_POLYTOPEEXTENSION_SUPER}.  Initialise them to
  ! empty expansions in the meantime.  The sub-macros represent a
  ! flexible alternative to static arguments, although they make
  ! the code somewhat convoluted.  In fact, I hate this system.
  ! To be refactored sometime down the line.
  m4_define({NAME_SUPER}, {})
  m4_define({SIGNATURELIST_SUPERCOLLECTION}, {})
  m4_define({SIGNATURELIST_SUPERPOINTER}, {})
  m4_define({METHODLIST_SUPERPOINTER}, {})
  m4_define({SIGNATURELIST_SUPERCOMPLEX}, {})
  m4_define({METHODLIST_SUPERCOMPLEX}, {})
  m4_define({METHODLIST_SUPERPOINTER_SUBCLASS}, {})
  
     
  MACRO({TYPEDEF_POLYTOPEEXTENSION_SUPERCOLLECTION}, {{
  type, abstract, public :: $1CollectionInterface
     private 
   contains 
     procedure(ABB($1)c_di), deferred :: deinit
     procedure(ABB($1)c_gco), deferred :: getCollectionOwner SIGNATURELIST_SUPERCOLLECTION({ABB($1)c})
  end type $1CollectionInterface
  }})
  
  MACRO({TYPEDEF_POLYTOPEEXTENSION_SUPERPOINTER}, {{
  type, abstract, public :: $1PointerInterface
     private 
   contains 
     procedure(ABB($1)p_di), deferred :: deinit
     procedure(ABB($1)p_go), deferred :: getOwner SIGNATURELIST_SUPERPOINTER({ABB($1)p})
  end type $1PointerInterface
  }})
  
  MACRO({TYPEDEF_POLYTOPEEXTENSION_SUPERCOMPLEX}, {{
  type, abstract, public :: $1ComplexInterface
     private 
   contains 
     procedure(ABB($1)x_di), deferred :: deinit
     procedure(ABB($1)x_gxo), deferred :: getComplexOwner SIGNATURELIST_SUPERCOMPLEX({ABB($1)x})
  end type $1ComplexInterface
  }})
  
  !-------------------------------------------------------------------
  !- Polytope extensions
  !-------------------------------------------------------------------

  ! these sub-macros will be defined by the client just before the
  ! expansion of {TYPEDEF_POLYTOPEEXTENSION}.  Initialise them to
  ! empty expansions in the meantime.  
  m4_define({SIGNATURELIST}, {})
  m4_define({METHODLIST}, {})

  
  MACRO({TYPEDEFS_POLYTOPEEXTENSION}, {{
  type, m4_ifelse({$2}, {}, {}, {extends($2CollectionInterface), })abstract, public  :: $1CollectionInterface
     private
   contains
     procedure(ABB($1)c_gco), deferred :: getCollectionOwner m4_dnl
     m4_ifelse({$2}, {}, {}, 
     {SIGNATURELIST_SUPERCOLLECTION(ABB($1)c)}) m4_dnl
     SIGNATURELIST_COLLECTION(c) m4_dnl
     METHODLIST_COLLECTION()
  end type $1CollectionInterface

  EXPAND({TYPEDEF_POLYTOPEEXTENSION_COMMON($1, {Array}, 
     {extends($1CollectionInterface), }, {}, {
     procedure :: getCollectionOwner => getCollectionOwner_$1Array
     m4_ifelse({$2}, {}, {},
     {SIGNATURELIST_SUPERCOLLECTION(ABB($1)a)}) m4_dnl
     SIGNATURELIST_COLLECTION(a) m4_dnl
     SIGNATURELIST_ARRAY})})
    
  EXPAND({TYPEDEF_POLYTOPEEXTENSION_COMMON($1, {Grid},
     {extends($1CollectionInterface), }, {}, {
     procedure :: getCollectionOwner => getCollectionOwner_$1Grid
     m4_ifelse({$2}, {}, {},
     {SIGNATURELIST_SUPERCOLLECTION(ABB($1)g)}) m4_dnl
     SIGNATURELIST_COLLECTION(g) m4_dnl
     SIGNATURELIST_GRID})})
    
  EXPAND({TYPEDEF_POLYTOPEEXTENSION_POINTER({$1}, {$2})})

  EXPAND({TYPEDEF_POLYTOPEEXTENSION_COMMON($1, {Complex}, 
     m4_ifelse({$2}, {}, {}, {{extends($2ComplexInterface), }}), {}, {
     SIGNATURELIST_SUPERCOMPLEX(ABB({$1})x)
     SIGNATURELIST_COMPLEX})})
  }})

  MACRO({TYPEDEF_POLYTOPEEXTENSION_COMMON}, {{
  type, $3abstract, public  :: $1$2Interface
     private
     class(Polytope$2Interface), pointer :: owner $4
   contains
     procedure(ABB($1$2)_di), deferred :: deinit
     procedure :: initExtension => initExtension_$1$2
     procedure :: deinitExtension => deinitExtension_$1$2
     procedure :: get$2Owner => get$2Owner_$1$2 $5
  end type $1$2Interface
  }})

  MACRO({TYPEDEF_POLYTOPEEXTENSION_POINTER}, {{
  type, m4_ifelse({SIGNATURELIST_SUPERCOLLECTION}, {}, {}, {extends($2PointerInterface), })public :: $1PointerType
     private
     class($1CollectionInterface), pointer :: ABB($1)c => null()
     integer :: polytopeIndex = 0
   contains
     procedure :: init_$1PointerFromCollection
     procedure :: init_$1PointerFromPointer
     generic :: init => init_$1PointerFromCollection, &
          init_$1PointerFromPointer
     procedure :: init_pure_$1PointerFromCollection
     generic :: init_pure => init_pure_$1PointerFromCollection
     procedure :: deinit => deinit_$1Pointer
     procedure :: getOwner => getOwner_$1Pointer
     METHODLIST_POINTER($1Pointer)
     METHODLIST_SUPERPOINTER_SUBCLASS($1Pointer) $3
  end type $1PointerType
  }})

  
    
  !-------------------------------------------------------------------
  !- GaussianPoints
  !-------------------------------------------------------------------
     
  MACRO({SIGNATURELIST_GAUSSIANPOINTCOLLECTION},{{
     EXPAND({SIGNATURELIST_POINTCOLLECTION($1Gaussian)})
     }})

  MACRO({TYPEDEF_GAUSSIANPOINTGROUPPOINTER},{{
     EXPAND({TYPEDEF_POINTGROUPPOINTER_CONCRETE($1Gaussian, $2, $3)})
     }})


  !-------------------------------------------------------------------
  !- IntAveMomentSources
  !------------------------------------------------------------------
  
  MACRO({TYPEDEF_INTAVEMOMENTFIELD}, {{
  ! a set of MomentFields sharing the same FlowVariable but different
  ! spatial derivatives could in fact be lumped into a single FlowField
  ! object, making flow variable matching more efficient.  This
  ! refactoring is left for a future iteration.
  type, extends($1MomentFieldInterface), abstract, public :: IntAve$1MomentFieldInterface
     private
   contains
     EXPAND({SIGNATURELIST_MOMENTFIELD($1, {IntAve})})
     EXPAND({METHODLIST_SUBMOMENTSOURCE($1, {MomentField}, {IntAve})})
     procedure(ia{}ABB($1)mf_c), deferred :: clone
     procedure(ia{}ABB($1)mf_gv), deferred :: getValue
     procedure(ia{}ABB($1)mf_gvs), deferred :: getValues
     generic :: setField => setField_uniform, setField_array
  end type IntAve$1MomentFieldInterface
  }})

  
  MACRO({TYPEDEF_INTAVEBOUNDARYCONDITION}, {{
  type, extends($1BoundaryConditionInterface), abstract, public :: IntAve$1BoundaryConditionInterface
     private
   contains
     EXPAND({SIGNATURELIST_BOUNDARYCONDITION($1, {IntAve})})
     EXPAND({METHODLIST_SUBMOMENTSOURCE($1, {BoundaryCondition}, {IntAve})})
     procedure(ia{}ABB($1)bc_c), deferred :: clone
     procedure(ia{}ABB($1)bc_gv), deferred :: getValue
     procedure(ia{}ABB($1)bc_gvs), deferred :: getValues
  end type IntAve$1BoundaryConditionInterface
  }})

  
  !-------------------------------------------------------------------
  !- IntAveMoment 
  !-------------------------------------------------------------------

  MACRO({SIGNATURELIST_INTAVEMOMENT},{{
     procedure(ABB($1)iam_di), deferred :: deinit
     }})

  MACRO({SIGNATURELIST_SUBINTAVEMOMENT},{{
!!$     procedure(ABB($1)iam_gp), deferred :: getPositions
     procedure(ABB($1)iam_cmr), deferred :: computeMatrixRows
     procedure(ABB($1)iam_gv), deferred :: getValue
     procedure(ABB($1)iam_gvs), deferred :: getValues
     }})

  MACRO({ATTRIBUTELIST_SUBINTAVEMOMENT},{{
     class($1FlowVariableInterface), pointer :: LWR($1)FlowVariable
     }})
  
  MACRO({METHODLIST_SUBINTAVEMOMENT},{{
     procedure :: setFlowVariable => setFlowVariable_$1IntAveMoment
     procedure :: getFlowVariable => getFlowVariable_$1IntAveMoment
     }})
     
  
  !-------------------------------------------------------------------
  !- Profile
  !-------------------------------------------------------------------

  MACRO({SIGNATURELIST_PROFILE},{{
     procedure(ABB($1)p_), deferred :: deinit
     procedure(ABB($1)p_gps), deferred :: getPolytopeSize
     procedure(ABB($1)p_), deferred :: buildSystems
     procedure(ABB($1)p_), deferred :: factoriseSystems
     ! linear algebra-performing methods
     procedure(ABB($1)p_s), deferred :: solveIncomplete
     procedure(ABB($1)p_f), deferred :: finalise
     procedure(ABB($1)p_gne), deferred :: getNumEquations
     ! methods for testing 
     procedure(ABB($1)p_ao), deferred :: acceptAsOperand1
     procedure(ABB($1)p_ao), deferred :: acceptAsOperand2
     procedure(ABB($1)p_ao), deferred :: acceptAsOperand3
     procedure(ABB($1)p_v), deferred :: visit
     }})

  
  MACRO({TYPEDEF_PROFILE},{{
  type, abstract, public :: $1ProfileInterface
     private
   contains
     EXPAND({SIGNATURELIST_PROFILE($1)})
     ! this method can be used to execute the three acceptAsOperand
     ! methods in succession, i.e. when this profile, and only this
     ! profile, contains all the components to be operated upon.
     procedure :: accept => accept_$1Profile
  end type $1ProfileInterface
  }})

  
  MACRO({TYPEDEF_PRIMITIVEPROFILE},{{
  type, extends($1ProfileInterface), abstract, public :: $1PrimitiveProfileInterface
     private
   contains
     EXPAND({SIGNATURELIST_PROFILE($1Primitive)})
  end type $1PrimitiveProfileInterface
  }})

  
  MACRO({TYPEDEF_COMPLEXPROFILE},{{
  type, extends($1ProfileInterface), abstract, public :: $1ComplexProfileInterface
     private
   contains
     EXPAND({SIGNATURELIST_PROFILE($1Complex)})
  end type $1ComplexProfileInterface
  }})


  MACRO({METHODLIST_PROFILEOPERATION_VISIT}, {{
     procedure :: visit$2ForOperand1 => ignore_$1
     procedure :: visit$2ForOperand2 => ignore_$1
     procedure :: visit$2ForOperand3 => ignore_$1
     }})
     
  MACRO({SIGNATURELIST_PROFILEOPERATION}, {{
     procedure(ABB($1)po_di), deferred :: deinit
     }})

  MACRO({TYPEDEFS_PROFILEOPERATION}, {{
  ! An(other) implementation of the Visitor pattern.  Each visit method
  ! involves a profile passing a $1LinearSystem to the operation object.
  ! If the call is pertinent to the operation, it will override the
  ! existing stub.
  type, abstract, public :: $1ProfileOperationInterface
     private
   contains
     EXPAND({SIGNATURELIST_PROFILEOPERATION($1)})
     ! visit method stubs
     EXPAND({METHODLIST_PROFILEOPERATION_VISIT($1,
     {ExactlyConstr})})
     EXPAND({METHODLIST_PROFILEOPERATION_VISIT($1,
     {LeastSquares})})
     EXPAND({METHODLIST_PROFILEOPERATION_VISIT($1,
     {PolytpIntAve})})
     EXPAND({METHODLIST_PROFILEOPERATION_VISIT($1,
     {Final})})
  end type $1ProfileOperationInterface
  

  ! When "perform" is called, the object carries out a pure operation
  type, extends($1ProfileOperationInterface), abstract, public :: Pure$1ProfileOperationInterface
     private
   contains
     EXPAND({SIGNATURELIST_PROFILEOPERATION(Pure$1)})
     procedure(p{}ABB($1)po_p), deferred :: perform
     procedure(p{}ABB($1)po_c), deferred :: convert
     procedure(p{}ABB($1)po_ssf), deferred :: setScaleFactor
  end type Pure$1ProfileOperationInterface
  
  
  ! When "perform" is called, the object carries out an impure operation
  type, extends($1ProfileOperationInterface), abstract, public :: Impure$1ProfileOperationInterface
     private
   contains
     EXPAND({SIGNATURELIST_PROFILEOPERATION(Impure$1)})
     procedure(i{}ABB($1)po_p), deferred :: perform
  end type Impure$1ProfileOperationInterface
  }})
  
     
  !-------------------------------------------------------------------
  !- abstract interfaces
  !-------------------------------------------------------------------
  
  MACRO({SIGNATUREDEF_POLYTOPECOLLECTION_SETEXTENSION}, {{
     subroutine p{}ABB($1)_se_{}ABB($2)( obj, LWR($2$1), log )
       import Polytope$1Interface, &
            $2$1Interface, LogType
       class(Polytope$1Interface), intent(inout) :: obj
       class($2$1Interface), allocatable, intent(inout) :: &
            LWR($2$1)
       class(LogType), intent(inout), optional :: log
     end subroutine p{}ABB($1)_se_{}ABB($2)
     }})

  MACRO({SIGNATUREDEF_POLYTOPECOLLECTION_GETEXTENSION},{{
     pure subroutine p{}ABB($1)_g{}ABB($3)e_{}ABB($2)( obj, &
          LWR($2$3), log )
       import Polytope$1Interface, &
            $2$3Interface, LogType
       class(Polytope$1Interface), intent(inout), target :: obj
       class($2$3Interface), pointer, intent(inout) :: &
            LWR($2$3)
       class(LogType), intent(inout), optional :: log
     end subroutine p{}ABB($1)_g{}ABB($3)e_{}ABB($2)
     }})
     
  MACRO({SIGNATUREDEFS_POLYTOPECOLLECTION_EXTENSIONS}, {{
     EXPAND({SIGNATUREDEF_POLYTOPECOLLECTION_GETEXTENSION($1, {Face},
     m4_ifelse($2, {}, $1, $2))})
     EXPAND({SIGNATUREDEF_POLYTOPECOLLECTION_GETEXTENSION($1, {Cell},
     m4_ifelse($2, {}, $1, $2))})
     }})
     
  MACRO({SIGNATUREDEFS_SUBPOLYTOPECOLLECTION_EXTENSIONS}, {{
     EXPAND({SIGNATUREDEF_POLYTOPECOLLECTION_SETEXTENSION($1, {Face})})
     EXPAND({SIGNATUREDEF_POLYTOPECOLLECTION_SETEXTENSION($1, {Cell})})
     EXPAND({SIGNATUREDEFS_POLYTOPECOLLECTION_EXTENSIONS($1,
     {Collection})})
     EXPAND({SIGNATUREDEF_POLYTOPECOLLECTION_GETEXTENSION($1, {Face}, $1)})
     EXPAND({SIGNATUREDEF_POLYTOPECOLLECTION_GETEXTENSION($1, {Cell}, $1)})
     }})

  MACRO({SIGNATUREDEFS_POLYTOPEOTHER_EXTENSIONS}, {{
     EXPAND({SIGNATUREDEF_POLYTOPECOLLECTION_SETEXTENSION($1, {Face})})
     EXPAND({SIGNATUREDEF_POLYTOPECOLLECTION_SETEXTENSION($1, {Cell})})
     EXPAND({SIGNATUREDEF_POLYTOPECOLLECTION_GETEXTENSION($1, {Face}, $1)})
     EXPAND({SIGNATUREDEF_POLYTOPECOLLECTION_GETEXTENSION($1, {Cell}, $1)})
     }})


  MACRO({SIGNATUREDEFS_POLYTOPEEXTENSION_COMMON}, {{
     subroutine ABB($1){}ABB($2)_di( obj, log )
       import $1$2Interface, LogType
       class($1$2Interface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1){}ABB($2)_di
  
     pure subroutine ABB($1){}ABB($2)_g{}ABB($3)o( obj, polytope$3 )
       import $1$2Interface, Polytope$3Interface
       class($1$2Interface), intent(inout) :: obj
       class(Polytope$3Interface), pointer, intent(out) :: &
            polytope$3
     end subroutine ABB($1){}ABB($2)_g{}ABB($3)o
     }})

  ! -- Polytope super extension signatures --
     
  MACRO({SIGNATUREDEFS_POLYTOPEEXTENSION_SUPERCOLLECTION}, {{
     EXPAND({SIGNATUREDEFS_POLYTOPEEXTENSION_COMMON({$1}, {$2},
     {Collection})})

     SIGNATUREDEFS_SUPERCOLLECTION({$1}, {$2})
     }})

  MACRO({SIGNATUREDEFS_POLYTOPEEXTENSION_SUPERPOINTER}, {{
     pure subroutine ABB($1){}p_di( obj, log )
       import $1Pointer$2, LogType
       class($1Pointer$2), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1){}p_di
     
     pure subroutine ABB($1)p_go( obj, polytopePointer )
       import $1Pointer$2, PolytopePointerType
       class($1Pointer$2), intent(inout) :: obj
       type(PolytopePointerType), intent(out) :: polytopePointer
     end subroutine ABB($1)p_go
     
     SIGNATUREDEFS_SUPERPOINTER({$1}, {$2})
     }})

  MACRO({SIGNATUREDEFS_POLYTOPEEXTENSION_SUPERCOMPLEX}, {{
     EXPAND({SIGNATUREDEFS_POLYTOPEEXTENSION_COMMON({$1}, {Complex},
     {Complex})})
     
     SIGNATUREDEFS_SUPERCOMPLEX({$1}, {$2})
     }})

     
  ! -- Polytope extension signatures --
  MACRO({SIGNATUREDEFS_POLYTOPEEXTENSION_COLLECTION}, {{
     EXPAND({SIGNATUREDEFS_POLYTOPEEXTENSION_SUPERCOLLECTION({$1}, {$2})})

     SIGNATUREDEFS_COLLECTION({$2})
     }})

  MACRO({SIGNATUREDEFS_POLYTOPEEXTENSION_COMPLEX}, {{
     EXPAND({SIGNATUREDEFS_POLYTOPEEXTENSION_SUPERCOMPLEX({$1}, {Type})})
     
     SIGNATUREDEFS_COMPLEX()
     }})


  MACRO({SIGNATUREDEF_GAUSSIANPOINTCOLLECTION_CREATEINTAVEMOMENT},{{
!!$     subroutine ABB($1)gpc_c{}ABB($3)iam( &
!!$          obj, LWR(IntAve$3Moment), polytopeIndex, &
!!$          LWR($3IntAve$2), log )
!!$       import $1GaussianPointCollectionInterface, &
!!$            IntAve$3MomentInterface, $3IntAve$2Interface, &
!!$            LogType
!!$       class($1GaussianPointCollectionInterface), intent(in) :: obj
!!$       class(IntAve$3MomentInterface), allocatable, &
!!$            intent(inout) :: LWR(IntAve$3Moment)
!!$       integer, intent(in) :: polytopeIndex
!!$       class($3IntAve$2Interface), target, intent(in) :: &
!!$            LWR($3IntAve$2)
!!$       class(LogType), intent(inout), optional :: log
!!$     end subroutine ABB($1)gpc_c{}ABB($3)iam
     }})

     
  MACRO({SIGNATUREDEFS_GAUSSIANPOINTCOLLECTION},{{
     EXPAND({SIGNATUREDEFS_POINTCOLLECTION({Gaussian}, $1)})

     EXPAND({SIGNATUREDEF_GAUSSIANPOINTCOLLECTION_CREATEINTAVEMOMENT($1,
     {MomentField}, {Scalar})})
     
     EXPAND({SIGNATUREDEF_GAUSSIANPOINTCOLLECTION_CREATEINTAVEMOMENT($1,
     {MomentField}, {Vector})})
     }})

          
  MACRO({SIGNATUREDEFS_INTAVEMOMENTFIELD},{{
     EXPAND({SIGNATUREDEFS_MOMENTFIELD({$1}, {IntAve})})

     subroutine ia{}ABB($1)mf_c( obj, tgt, log )
       import IntAve$1MomentFieldInterface, LogType
       class(IntAve$1MomentFieldInterface), intent(in) :: obj
       class(IntAve$1MomentFieldInterface), allocatable, &
            intent(out) :: tgt
       class(LogType), intent(inout), optional :: log
     end subroutine ia{}ABB($1)mf_c

     elemental function ia{}ABB($1)mf_gv( obj, &
          polytopeIndex ARGLINE_TENSOR_COMPONENT($1) )
       import IntAve$1MomentFieldInterface, FLOAT
       class(IntAve$1MomentFieldInterface), intent(in) :: obj
       integer, intent(in) :: polytopeIndex
       ARGLIST_TENSOR_COMPONENT($1)
       real(FLOAT) :: ia{}ABB($1)mf_gv
     end function ia{}ABB($1)mf_gv

     pure subroutine ia{}ABB($1)mf_gvs( obj, values, &
          polytopeIndex ARGLINE_TENSOR_COMPONENT($1) )
       import IntAve$1MomentFieldInterface, FLOAT
       class(IntAve$1MomentFieldInterface), intent(in) :: obj
       real(FLOAT), dimension(:), intent(out) :: values
       integer, intent(in) :: polytopeIndex
       ARGLIST_TENSOR_COMPONENT($1)
     end subroutine ia{}ABB($1)mf_gvs
     }})
     
  MACRO({SIGNATUREDEFS_INTAVEBOUNDARYCONDITION},{{
     EXPAND({SIGNATUREDEFS_BOUNDARYCONDITION({$1}, {IntAve})})
       
     subroutine ia{}ABB($1)bc_c( obj, tgt, log )
       import IntAve$1BoundaryConditionInterface, LogType
       class(IntAve$1BoundaryConditionInterface), intent(in) :: obj
       class(IntAve$1BoundaryConditionInterface), allocatable, &
            intent(out) :: tgt
       class(LogType), intent(inout), optional :: log
     end subroutine ia{}ABB($1)bc_c

     elemental function ia{}ABB($1)bc_gv( obj ARGLINE_TENSOR_COMPONENT($1) )
       import IntAve$1BoundaryConditionInterface, IMPORT_TENSOR_PRIMITIVE($1)
       class(IntAve$1BoundaryConditionInterface), intent(in) :: obj
       ARGLIST_TENSOR_COMPONENT($1)
       TYPE_TENSOR_PRIMITIVE($1) :: ia{}ABB($1)bc_gv
     end function ia{}ABB($1)bc_gv
       
     pure subroutine ia{}ABB($1)bc_gvs( obj, values ARGLINE_TENSOR_COMPONENT($1) )
       import IntAve$1BoundaryConditionInterface, FLOAT, IMPORT_TENSOR_PRIMITIVE($1)
       class(IntAve$1BoundaryConditionInterface), intent(in) :: obj
       real(FLOAT), dimension(:), intent(out) :: values
       ARGLIST_TENSOR_COMPONENT($1)
     end subroutine ia{}ABB($1)bc_gvs
     }})

  MACRO({SIGNATUREDEFS_PROFILE}, {{
     subroutine ABB($1)p_( obj, log )
       import $1ProfileInterface, LogType
       class($1ProfileInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)p_
     
     pure subroutine ABB($1)p_gps( obj, polytopeSize )
       import $1ProfileInterface, FLOAT
       class($1ProfileInterface), intent(inout) :: obj
       real(FLOAT), intent(out) :: polytopeSize
     end subroutine ABB($1)p_gps
     
     pure subroutine ABB($1)p_s( obj, log )
       import $1ProfileInterface, LogType
       class($1ProfileInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)p_s
     
     subroutine ABB($1)p_f( obj, log )
       import $1ProfileInterface, LogType
       class($1ProfileInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)p_f
     
     pure function ABB($1)p_gne( obj, selector ) result ( nEquations )
       import $1ProfileInterface
       class($1ProfileInterface), intent(in) :: obj
       character(1), intent(in) :: selector
       integer :: nEquations
     end function ABB($1)p_gne
     
     subroutine ABB($1)p_ao( obj, LWR($1)ProfileOperation )
       import $1ProfileInterface, &
            $1ProfileOperationInterface, LogType
       class($1ProfileInterface), intent(inout) :: obj
       class($1ProfileOperationInterface), intent(inout) :: &
            LWR($1)ProfileOperation
     end subroutine ABB($1)p_ao
     
     subroutine ABB($1)p_v( obj, LWR($1)LinearOperand )
       import $1ProfileInterface, $1LinearOperandInterface
       class($1ProfileInterface), intent(inout) :: obj
       class($1LinearOperandInterface), intent(inout) :: &
            LWR($1)LinearOperand
     end subroutine ABB($1)p_v
     }})

  MACRO({SIGNATUREDEFS_PROFILEOPERATION},{{
     subroutine ABB($1)po_di( obj, log )
       import $1ProfileOperationInterface, LogType
       class($1ProfileOperationInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine ABB($1)po_di
     }})

  MACRO({SIGNATUREDEFS_SUBPROFILEOPERATIONS},{{
     EXPAND({SIGNATUREDEFS_PROFILEOPERATION({Pure$1})})
  
     pure subroutine p{}ABB($1)po_p( obj, log )
       import Pure$1ProfileOperationInterface, LogType
       class(Pure$1ProfileOperationInterface), intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine p{}ABB($1)po_p
   
     pure subroutine p{}ABB($1)po_c( obj, pure$1LinearOperation, &
          log )
       import Pure$1ProfileOperationInterface, &
            Pure$1LinearOperationInterface, LogType
       class(Pure$1ProfileOperationInterface), intent(inout) :: obj
       class(Pure$1LinearOperationInterface), allocatable, intent(&
            inout) :: pure$1LinearOperation
       class(LogType), intent(inout), optional :: log
     end subroutine p{}ABB($1)po_c
     
     pure subroutine p{}ABB($1)po_ssf( obj, value )
       import Pure$1ProfileOperationInterface, FLOAT
       class(Pure$1ProfileOperationInterface), intent(inout) :: obj
       real(FLOAT), intent(in) :: value
     end subroutine p{}ABB($1)po_ssf

     EXPAND({SIGNATUREDEFS_PROFILEOPERATION({Impure$1})})
     
     subroutine i{}ABB($1)po_p( obj, log )
       import Impure$1ProfileOperationInterface, &
            Impure$1LinearOperationInterface, LogType
       class(Impure$1ProfileOperationInterface), &
            intent(inout) :: obj
       class(LogType), intent(inout), optional :: log
     end subroutine i{}ABB($1)po_p
     }})


contains
  
  !-------------------------------------------------------------------
  !- Extension methods
  !-------------------------------------------------------------------

  MACRO({METHODDEFS_POLYTOPEEXTENSION_COMMON}, {{
  EXPAND({PROCEDURE_DESTROY({$1$2})})
  
  subroutine initExtension_$1$2( obj, polytope$2, $3log )
    class($1$2Interface), intent(inout) :: obj
    class(Polytope$2Interface), intent(in), target :: polytope$2
    class(LogType), intent(inout), optional :: log $4
    call beginSub( MOD_NAME, 'initExtension_$1$2', log )
    obj%owner => polytope$2 $5
    call endSub( log )
  end subroutine initExtension_$1$2
  
  pure subroutine deinitExtension_$1$2( obj, log )
    class($1$2Interface), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinitExtension_$1$2', log )
    nullify(obj%owner) $6
    call endSub( log )
  end subroutine deinitExtension_$1$2
  
  pure subroutine get$2Owner_$1$2( obj, polytope$2 )
    class($1$2Interface), intent(inout) :: obj
    class(Polytope$2Interface), pointer, intent(out) :: polytope$2
    polytope$2 => obj%owner
  end subroutine get$2Owner_$1$2
  }})

  METHODLIST_POLYTOPEEXTENSION_SUPERPOINTER_SUBCLASS

  MACRO({METHODDEFS_POLYTOPEEXTENSION}, {{
  EXPAND({METHODDEFS_POLYTOPEEXTENSION_COMMON({$1}, {Array})})
    
  pure subroutine getCollectionOwner_$1Array( obj, polytopeCollection )
    class($1ArrayInterface), intent(inout) :: obj
    class(PolytopeCollectionInterface), pointer, intent(out) :: &
         polytopeCollection
    polytopeCollection => obj%owner
  end subroutine getCollectionOwner_$1Array

  EXPAND({METHODDEFS_POLYTOPEEXTENSION_COMMON({$1}, {Grid})})

  pure subroutine getCollectionOwner_$1Grid( obj, polytopeCollection )
    class($1GridInterface), intent(inout) :: obj
    class(PolytopeCollectionInterface), pointer, intent(out) :: &
         polytopeCollection
    polytopeCollection => obj%owner
  end subroutine getCollectionOwner_$1Grid
  
  EXPAND({METHODDEFS_POLYTOPEEXTENSION_COMMON({$1}, {Complex})})
  }})
  

  !-------------------------------------------------------------------
  !- GaussianPointCollection methods
  !-------------------------------------------------------------------
  
  MACRO({METHODDEFS_SUBGAUSSIANPOINTCOLLECTION},{{

  
  EXPAND({METHODDEFS_SUBPOINTCOLLECTION($1Gaussian,
  m4_ifelse($1, {Gaussian}, {}))})
  }})

  MACRO({METHODDEFS_GAUSSIANPOINTGROUPPOINTER},{{
  EXPAND({METHODDEFS_POINTGROUPPOINTER($1Gaussian)})
  }})

  
  !-------------------------------------------------------------------
  !- IntAveMoment methods
  !-------------------------------------------------------------------

  MACRO({METHODDEFS_SUBINTAVEMOMENT},{{
  subroutine setFlowVariable_IntAve$1Moment( obj, &
       LWR($1)FlowVariable )
    class(IntAve$1MomentInterface), intent(inout) :: obj
    class($1FlowVariableInterface), target, intent(in) :: &
         LWR($1)FlowVariable
    obj%LWR($1)FlowVariable => LWR($1)FlowVariable
  end subroutine setFlowVariable_IntAve$1Moment
  

  subroutine getFlowVariable_IntAve$1Moment( obj, &
       LWR($1)FlowVariable )
    class(IntAve$1MomentInterface), intent(in) :: obj
    class($1FlowVariableInterface), pointer, intent(out) :: &
         LWR($1)FlowVariable
    LWR($1)FlowVariable => obj%LWR($1)FlowVariable
  end subroutine getFlowVariable_IntAve$1Moment
  }})

  
  !-------------------------------------------------------------------
  !- IntAveMomentField methods
  !-------------------------------------------------------------------

  MACRO({METHODDEFS_INTAVEMOMENTFIELD},{{
  EXPAND({METHODDEFS_SUBMOMENTFIELD($1, IntAve)})
  }})
  
  !-------------------------------------------------------------------
  !- IntAveBoundaryCondition methods
  !-------------------------------------------------------------------

  MACRO({METHODDEFS_INTAVEBOUNDARYCONDITION},{{
  EXPAND({METHODDEFS_BOUNDARYCONDITION({$1}, {IntAve})})
  }})

  
  !-------------------------------------------------------------------
  !- PolytopePointerSet methods
  !-------------------------------------------------------------------

  MACRO({METHODDEF_POLYTOPEPOINTERSET_INIT}, {{
  $1 subroutine initPolytopePointerSet{}m4_ifelse({$1}, {}, {}, {_$1})( obj, ppArray, log )
    class(PolytopePointerSetType), intent(out), target :: obj
    type(PolytopePointerType), dimension(:), intent(m4_ifelse({$1}, {pure}, {inout}, {in})) :: ppArray
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
  end subroutine initPolytopePointerSet{}m4_ifelse({$1}, {}, {}, {_$1})
  }})
  

  !-------------------------------------------------------------------
  !- PolytopePointer extension methods
  !-------------------------------------------------------------------
  
  MACRO({METHODDEF_POLYTOPEPOINTER_EXTENSION}, {{  
  elemental subroutine getPointerExtension_$1s( obj, &
       LWR($1)Pointer, log )
    class(PolytopePointerType), intent(in), target :: obj
    type($1PointerType), intent(inout) :: LWR($1)Pointer
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getPointerExtension_$1s', log )
    
    call obj%pc%getCollectionExtension( LWR($1)Pointer%ABB($1)c, log )
    if ( associated(LWR($1)Pointer%ABB($1)c) ) then
       LWR($1)Pointer%polytopeIndex = obj%index
    else
       call addEvent( WARNING, 'Object does not have a &
            &$1 extension.', log )
    end if
    
    call endSub( log )
  end subroutine getPointerExtension_$1s
  }})


  
  !-------------------------------------------------------------------
  !- ExtensionPointer methods
  !--------------------------------------------------------------------
  
  MACRO({PROCEDURES_POLYTOPEEXTENSION_POINTER}, {{
  subroutine create$1PointerArray( LWR($1)Pointers, &
       polytopePointers, log )
    type($1PointerType), dimension(:), allocatable, intent(out) :: &
         LWR($1)Pointers
    type(PolytopePointerType), dimension(:), intent(in) :: &
         polytopePointers
    class(LogType), intent(inout), optional :: log
    integer :: stat, i
    call beginSub( MOD_NAME, 'create$1PointerArray', log )

    call addEvent( allocated(LWR($1)Pointers), FATAL, &
         'LWR($1)Pointers already allocated.', log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! allocate
    allocate( LWR($1)Pointers( size(polytopePointers) ), stat=&
         stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating LWR($1)&
         &Pointers( size(polytopePointers) ).  STAT='//int2str(stat), &
         log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! get an extension for each element (success not guaranteed, but the
    ! array can hold uninitialised elements if necessary)
    do i = 1, size(LWR($1)Pointers)
       call polytopePointers(i)%getPointerExtension_$1s( &
            LWR($1)Pointers(i), log )
    end do
          
    call endSub( log )
  end subroutine create$1PointerArray

  
  subroutine destroy$1PointerArray( LWR($1)Pointers, log )
    type($1PointerType), dimension(:), allocatable, intent(out) :: &
         LWR($1)Pointers
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'destroy$1PointerArray', log )

    deallocate( LWR($1)Pointers, stat=stat )
    call addEvent( stat/=0, WARNING, 'Problem deallocating LWR($1)&
         &Pointers.  STAT='//int2str(stat), log )

    call endSub( log )
  end subroutine destroy$1PointerArray


  function pointTo$1CollectionElement( ABB($1)c, &
       polytopeIndex ) result (ABB($1)p)
    class($1CollectionInterface), intent(in), target :: ABB($1)c
    integer, intent(in) :: polytopeIndex
    type($1PointerType) :: ABB($1)p
    ABB($1)p%ABB($1)c => ABB($1)c
    ABB($1)p%polytopeIndex = polytopeIndex
  end function pointTo$1CollectionElement

  
  subroutine assign$1Pointer( tgt, src )
    type($1PointerType), intent(out) :: tgt
    class($1PointerType), intent(in) :: src
    tgt%ABB($1)c => src%ABB($1)c
    tgt%polytopeIndex = src%polytopeIndex
  end subroutine assign$1Pointer
  }})
  
  
  MACRO({METHODDEFS_POLYTOPEEXTENSION_POINTER}, {{
  subroutine init_$1PointerFromCollection( obj, &
       LWR($1)Collection, polytopeIndex )
    class($1PointerType), intent(inout) :: obj
    class($1CollectionInterface), target, intent(in) :: &
         LWR($1)Collection
    integer, intent(in) :: polytopeIndex

    obj%ABB($1)c => LWR($1)Collection
    obj%polytopeIndex = polytopeIndex
  end subroutine init_$1PointerFromCollection

  
  subroutine init_$1PointerFromPointer( obj, LWR($1)Pointer )
    class($1PointerType), intent(inout) :: obj
    type($1PointerType), intent(in) :: LWR($1)Pointer

    obj%ABB($1)c => LWR($1)Pointer%ABB($1)c
    obj%polytopeIndex = LWR($1)Pointer%polytopeIndex
  end subroutine init_$1PointerFromPointer

  
  pure subroutine init_pure_$1PointerFromCollection( obj, &
       LWR($1)Collection, polytopeIndex )
    class($1PointerType), intent(inout) :: obj
    class($1CollectionInterface), target, intent(inout) :: &
         LWR($1)Collection
    integer, intent(in) :: polytopeIndex

    obj%ABB($1)c => LWR($1)Collection
    obj%polytopeIndex = polytopeIndex
  end subroutine init_pure_$1PointerFromCollection

  
  pure subroutine deinit_$1Pointer( obj, log )
    class($1PointerType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_$1Pointer', log )

    nullify(obj%ABB($1)c)
    obj%polytopeIndex = 0

    call endSub( log )
  end subroutine deinit_$1Pointer
  
  
  pure subroutine getOwner_$1Pointer( obj, polytopePointer )
    class($1PointerType), intent(inout) :: obj
    type(PolytopePointerType), intent(out) :: polytopePointer

    call obj%ABB($1)c%getCollectionOwner( polytopePointer%pc )
    polytopePointer%index = obj%polytopeIndex

  end subroutine getOwner_$1Pointer

  
  EXPAND({METHODDEFS_SUPERPOINTER_SUBCLASS($1)})
  }})

  
  MACRO({METHODDEF_CELLPOINTER_APPENDROW}, {{
!!$  subroutine appendRow_$1_CellPointer( obj, LWR($1Matrix), log )
!!$    class(CellPointerType), intent(in) :: obj
!!$    class($1MatrixInterface), intent(inout) :: LWR($1Matrix)
!!$    class(LogType), intent(inout), optional :: log
!!$    call beginSub( MOD_NAME, 'appendRow_$1_CellPointer', log )
!!$
!!$    call obj%cc%appendRow( LWR($1Matrix), obj%polytopeIndex, log )
!!$        
!!$    call endSub( log )
!!$  end subroutine appendRow_$1_CellPointer
  }})
  
  !-------------------------------------------------------------------
  !- Concrete Extension methods
  !-------------------------------------------------------------------

  ! these are for use strictly by the modules of concrete classes
  ! implementing the extension interfaces.
  
  MACRO({METHODDEFS_POLYTOPEEXTENSION_CONCRETE}, {{
  EXPAND({PROCEDURE_ATTACH({$1$2}, {$1$2}, {Polytope$2}, {$3}, {$4})})
  
  subroutine init_$1$2( obj, LWR({Polytope$2}), $3log )
    class($1$2Type), intent(out) :: obj
    class(Polytope$2Interface), intent(in), target :: LWR({Polytope$2})
    $4
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_$1$2', log )

    ! init extension superclass
    call obj%initExtension( LWR({Polytope$2}), log )
    call endSub( log )
  end subroutine init_$1$2

  
  subroutine deinit_$1$2( obj, log )
    class($1$2Type), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_$1$2', log )

    ! deinit extension superclass
    call obj%deinitExtension( log )
    call endSub( log )
  end subroutine deinit_$1$2
  }})

  
  !-------------------------------------------------------------------
  !- Profile and ProfileOperation methods
  !-------------------------------------------------------------------

  MACRO({METHODDEFS_PROFILEOPERATION}, {{
  subroutine ignore_$1( obj, LWR($1)LinearOperand )
    class($1ProfileOperationInterface), intent(inout) :: obj
    class($1LinearOperandInterface), intent(inout) :: &
         LWR($1)LinearOperand
    ! do nothing
  end subroutine ignore_$1
  }})

  MACRO({METHODDEFS_PROFILE}, {{
  subroutine accept_$1Profile( obj, LWR($1ProfileOperation) )
    class($1ProfileInterface), intent(inout) :: obj
    class($1ProfileOperationInterface), intent(inout) :: &
         LWR($1ProfileOperation) $3
    call obj%acceptAsOperand1( LWR($1ProfileOperation) )
    call obj%acceptAsOperand2( LWR($1ProfileOperation) )
    call obj%acceptAsOperand3( LWR($1ProfileOperation) )
  end subroutine accept_$1Profile
  }})

  
end module FiniteVolume_Macros
