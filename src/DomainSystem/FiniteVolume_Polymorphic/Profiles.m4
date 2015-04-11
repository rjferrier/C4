HEADER()
module ProfilesModule
      
  USE_MACROS({FlowField})
  USE_MACROS({Profiles})
  use LogModule
  use Global
  use SemiLagrangianModule
  use FiniteVolumeModule
  use DomainModule
  use FlowFieldModule

  ! there is a strong enough dependence on the following modules of
  ! concrete classes and various ProfileOperations that we should permit
  ! the coupling here.
  use LinearSystemsModule
  use PureProfileOperationsModule
  use PureLinearOperationsModule
  
  implicit none
   
  private
  public :: createScalarSimplexProfile, allocScalarSimplexProfileArray, &
       initScalarSimplexProfileArrayElement, createScalarComplexProfile
  
  character(*), parameter :: MOD_NAME = 'ProfilesModule'
  

  !-------------------------------------------------------------------
  !- Profiles
  !-------------------------------------------------------------------

  EXPAND({TYPEDEF_SIMPLEXPROFILE({Scalar})})

  EXPAND({TYPEDEF_COMPLEXPROFILE({Scalar})})


  
contains


  !-------------------------------------------------------------------
  !- ScalarSimplexProfile methods
  !-------------------------------------------------------------------
  
  EXPAND({COMBINATION_CREATEINIT_SIMPLEXPROFILE({Scalar})})
  
  EXPAND({METHODDEF_SIMPLEXPROFILE_DEINIT({Scalar})})
  
  EXPAND({METHODDEF_SIMPLEXPROFILE_GETPOLYTOPESIZE({Scalar})})
  
  EXPAND({METHODDEF_SIMPLEXPROFILE_BUILDSYSTEMS({Scalar})})
  
  EXPAND({METHODDEF_SIMPLEXPROFILE_FACTORISESYSTEMS({Scalar})})

  EXPAND({METHODDEF_SIMPLEXPROFILE_SOLVE({Scalar})})

  EXPAND({METHODDEF_SIMPLEXPROFILE_VISIT({Scalar})})

  EXPAND({METHODDEF_SIMPLEXPROFILE_GETNUMEQUATIONS({Scalar})})

  EXPAND({METHODDEF_SIMPLEXPROFILE_ACCEPT({Scalar}, {1})})
  EXPAND({METHODDEF_SIMPLEXPROFILE_ACCEPT({Scalar}, {2})})
  EXPAND({METHODDEF_SIMPLEXPROFILE_ACCEPT({Scalar}, {3})})


  !-------------------------------------------------------------------
  !- ScalarComplexProfile methods
  !-------------------------------------------------------------------
  
  EXPAND({COMBINATION_CREATEINIT_COMPLEXPROFILE({Scalar})})
  
  EXPAND({METHODDEF_COMPLEXPROFILE_DEINIT({Scalar})})

  EXPAND({METHODDEF_COMPLEXPROFILE_GETPOLYTOPESIZE({Scalar})})
  
  EXPAND({METHODDEF_COMPLEXPROFILE_BUILDSYSTEMS({Scalar})})
  
  EXPAND({METHODDEF_COMPLEXPROFILE_FACTORISESYSTEMS({Scalar})})

  EXPAND({METHODDEF_COMPLEXPROFILE_SOLVE({Scalar})})

  EXPAND({METHODDEF_COMPLEXPROFILE_VISIT({Scalar})})

  EXPAND({METHODDEF_COMPLEXPROFILE_GETNUMEQUATIONS({Scalar})})

  EXPAND({METHODDEF_COMPLEXPROFILE_ACCEPT({Scalar}, {1})})
  EXPAND({METHODDEF_COMPLEXPROFILE_ACCEPT({Scalar}, {2})})
  EXPAND({METHODDEF_COMPLEXPROFILE_ACCEPT({Scalar}, {3})})
  
  
end module ProfilesModule
