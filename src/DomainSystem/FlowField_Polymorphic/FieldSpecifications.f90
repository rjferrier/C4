module FieldSpecificationsModule
  
  use LogModule
  use Global
  use FlowFieldModule

  ! practically all functionality in the module is domain
  ! dimension-specific
  use FieldSpecifications_DomainDim
  
  implicit none
  
  character(*), parameter, private :: MOD_NAME = &
       'FieldSpecificationsModule'
  
  
end module FieldSpecificationsModule
