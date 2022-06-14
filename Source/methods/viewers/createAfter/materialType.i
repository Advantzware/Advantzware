/* materialType.i - mtyndall - 2.24.2022 */

ASSIGN
    materialType.materialTypeGroup:LIST-ITEMS IN FRAME {&FRAME-NAME} = cMaterialTypeGroup
    materialType.materialTypeGroup:SCREEN-VALUE                      = ENTRY(1,cMaterialTypeGroup)
    materialType.calculationType:LIST-ITEM-PAIRS                     = cCalculationTypeList
    materialType.calculationType:SCREEN-VALUE                        = ENTRY(2,cCalculationTypeList)
    materialType.consumedByDept:SCREEN-VALUE                         = "PR"
    .
RUN pGetDeptDesc.
