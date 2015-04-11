HEADER()
module PureLinearOperationsModule
      
  USE_MACROS({FlowField})
  USE_MACROS({PureLinearOperations})
  use LogModule
  use Global
  use FlowFieldModule

  implicit none
  
  private
  public ::  createLinearOperation_Scalar, &
       
       ! our LinearOperations do not need initialising.  Make them
       ! directly public to save faffing around with dynamic creation
       Copy_A_ScalarType, Copy_A_VectorType, &
       Copy_B_ScalarType, Copy_B_VectorType, &
       A_Times_Q2_ScalarType, A_Times_Q2_VectorType, &
       B_Minus_A_Times_X_ScalarType, B_Minus_A_Times_X_VectorType, &
       X_Plus_Q2_Times_X_ScalarType
  
    
  character(*), parameter :: MOD_NAME = 'PureLinearOperationsModule'

  
  EXPAND({TYPEDEF_COPY_A({Scalar})})
  
  EXPAND({TYPEDEF_COPY_A({Vector})})

  EXPAND({TYPEDEF_COPY_B({Scalar})})
  
  EXPAND({TYPEDEF_COPY_B({Vector})})
  
  EXPAND({TYPEDEF_K_TIMES_A({Scalar})})
  
  EXPAND({TYPEDEF_K_TIMES_A({Vector})})

  EXPAND({TYPEDEF_A_TIMES_Q2({Scalar})})
  
  EXPAND({TYPEDEF_A_TIMES_Q2({Vector})})

  EXPAND({TYPEDEF_B_MINUS_A_TIMES_X({Scalar})})
  
  EXPAND({TYPEDEF_B_MINUS_A_TIMES_X({Vector})})

  EXPAND({TYPEDEF_X_PLUS_Q2_TIMES_X({Scalar})})
  
  EXPAND({TYPEDEF_X_PLUS_Q2_TIMES_X({Vector})})

  
contains
  
  EXPAND({PROCEDURE_CREATE_LINEAROPERATION_CONCRETE({Scalar})})
  
  EXPAND({PROCEDURE_CREATE_LINEAROPERATION_CONCRETE({Vector})})
  
  EXPAND({METHODDEFS_COPY_A({Scalar})})

  EXPAND({METHODDEFS_COPY_A({Vector})})
  
  EXPAND({METHODDEFS_COPY_B({Scalar})})

  EXPAND({METHODDEFS_COPY_B({Vector})})
  
  EXPAND({METHODDEFS_K_TIMES_A({Scalar})})
  
  EXPAND({METHODDEFS_K_TIMES_A({Vector})})
  
  EXPAND({METHODDEFS_A_TIMES_Q2({Scalar})})

  EXPAND({METHODDEFS_A_TIMES_Q2({Vector})})

  EXPAND({METHODDEFS_B_MINUS_A_TIMES_X({Scalar})})

  EXPAND({METHODDEFS_B_MINUS_A_TIMES_X({Vector})})

  EXPAND({METHODDEFS_X_PLUS_Q2_TIMES_X({Scalar})})

  EXPAND({METHODDEFS_X_PLUS_Q2_TIMES_X({Vector})})
  


end module PureLinearOperationsModule
