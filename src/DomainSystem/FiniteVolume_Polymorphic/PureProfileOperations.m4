HEADER()
module PureProfileOperationsModule
      
  USE_MACROS({FlowField})
  USE_MACROS({PureProfileOperations})
  use LogModule
  use Global
  use SemiLagrangianModule
  use FiniteVolumeModule
  use DomainModule
  use FlowFieldModule

  ! there is a strong enough dependence on the following module of
  ! concrete classes and various LinearOperations that we should permit
  ! the coupling here.
  use LinearSystemsModule
  use PureLinearOperationsModule
  
  implicit none
   
  private
  public :: &
       ! our ProfileOperations do not carry allocatable components.  Make
       ! them directly public to save faffing around with dynamic
       ! creation.  The client will still have access to the init method.
       AppendLeastSquaresMatrix_ScalarType, &
       AppendLeastSquaresRHS_ScalarType, &
       AppendPolytpIntAveMatrix_ScalarType, &
       AppendPolytpIntAveRHS_ScalarType, &
       AppendFinalRHS_ScalarType, &
       FinaliseSimplexProfile_ScalarType, &
       FinaliseComplexProfile_ScalarType
  
  character(*), parameter :: MOD_NAME = 'PureProfileOperationsModule'

  
  EXPAND({TYPEDEF_PROFILEOPERATION_ABSTRACT({Scalar})})

  EXPAND({TYPEDEFS_APPENDREDUCEDSYSTEM({LeastSquares},
  {Scalar})})
  
  EXPAND({TYPEDEFS_APPENDREDUCEDSYSTEM({PolytpIntAve},
  {Scalar})})
  
  EXPAND({TYPEDEF_APPENDFINALRHS({Scalar})})
  
  EXPAND({TYPEDEF_FINALISE_PROFILE({Scalar}, {Simplex},
  {ExactlyConstr}, {PolytpIntAve})})

  EXPAND({TYPEDEF_FINALISE_PROFILE({Scalar}, {Complex},
  {PolytpIntAve}, {Final})})
  
  
contains

  EXPAND({METHODDEFS_PROFILEOPERATION_ABSTRACT({Scalar})})

  EXPAND({METHODDEFS_APPENDREDUCEDSYSTEM_LEASTSQUARES({Scalar})})

  EXPAND({METHODDEFS_APPENDREDUCEDSYSTEM_POLYTPINTAVE({Scalar})})

  EXPAND({METHODDEFS_APPENDFINALRHS({Scalar})})

  EXPAND({METHODDEFS_FINALISE_PROFILE({Scalar}, {Simplex},
  {ExactlyConstr}, {PolytpIntAve})})
  
  EXPAND({METHODDEFS_FINALISE_PROFILE({Scalar}, {Complex},
  {PolytpIntAve}, {Final})})
  
  
end module PureProfileOperationsModule
