
/*------------------------------------------------------------------------
    File        : FGReorder.p
    Purpose     : 

    Syntax      :

    Description : Procedures for Reorder Report processing

    Author(s)   : 
    Created     : Sun Aug 30 20:38:58 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{fgrep/ttFGReorder.i}
{jc/ttMultiSelectItem.i}

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE AssessSelections:
/*------------------------------------------------------------------------------
 Purpose:  Given the selections on temp-table, return the assessed selection totals
    and selection detail
 Notes: RUN AssessSelections IN hdFGReorder (OUTPUT iCountSelected, OUTPUT dTotalSqin, OUTPUT dTotalCyclesRequired, OUTPUT TABLE ttFGReorderSelection).
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER TABLE FOR ttMultiSelectItem. 
    DEFINE OUTPUT PARAMETER opiCountSelected AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdTotalArea AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdTotalCyclesRequired AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttFGReorderSelection.
          
    EMPTY TEMP-TABLE ttFGReorderSelection.
    FOR EACH ttMultiSelectItem NO-LOCK 
        WHERE ttMultiSelectItem.isSelected :     
        CREATE ttFGReorderSelection.
        BUFFER-COPY ttMultiSelectItem TO ttFGReorderSelection.
        ASSIGN 
            opiCountSelected       = opiCountSelected + 1
            opdTotalArea           = opdTotalArea + ttMultiSelectItem.blankArea * (MAXIMUM(1,ttMultiSelectItem.multiplier))
            ttFGReorderSelection.quantityCyclesRequired  = ttMultiSelectItem.quantityToOrder / (MAXIMUM(1,ttMultiSelectItem.multiplier))
            opdTotalCyclesRequired = IF ttFGReorderSelection.quantityCyclesRequired GT opdTotalCyclesRequired THEN ttFGReorderSelection.quantityCyclesRequired ELSE opdTotalCyclesRequired
            .
    END.
    FOR EACH ttFGReorderSelection:
        ASSIGN 
            ttFGReorderSelection.quantityCyclesSurplus = opdTotalCyclesRequired - ttFGReorderSelection.quantityCyclesRequired
            ttFGReorderSelection.quantityToOrderSurplus = ttFGReorderSelection.quantityCyclesSurplus * (MAXIMUM(1,ttFGReorderSelection.multiplier))
            ttFGReorderSelection.KeyItem =  (ttFGReorderSelection.quantityToOrder / (MAXIMUM(1,ttFGReorderSelection.multiplier))) EQ  opdTotalCyclesRequired
            .      
            
    END.
    
END PROCEDURE.

PROCEDURE BuildReport:
    /*------------------------------------------------------------------------------
     Purpose: Builds the primary super-set of data for a given company
     Notes:  RUN BuildReport IN hdFGReorder (ipcCompany, OUTPUT TABLE ttFGReorder).
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttFGReorder.

    DEFINE VARIABLE dQuantityOnHand AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQuantityOnOrder AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQuantityAllocated AS DECIMAL NO-UNDO.


    FOR EACH itemfg NO-LOCK 
        WHERE itemfg.company EQ ipcCompany
        :
        ASSIGN 
            dQuantityOnHand = itemfg.q-onh
            dQuantityOnOrder = itemfg.q-ono
            dQuantityAllocated = itemfg.q-alloc
            .
        IF dQuantityOnHand NE 0 OR dQuantityOnOrder NE 0 OR dQuantityAllocated NE 0 THEN DO:
            CREATE ttFGReorder.
            ASSIGN 
                ttFGReorder.company = itemfg.company
                ttFGReorder.itemID = itemfg.i-no
                ttFGReorder.itemName = itemfg.i-name
                ttFGReorder.itemDesc1 = itemfg.part-dscr1
                ttFGReorder.itemDesc2 = itemfg.part-dscr2
                ttFGReorder.itemDesc3 = itemfg.part-dscr3
		        ttFGReorder.itemStyle = itemfg.style
               	ttFGReorder.itemWhse = itemfg.loc
                ttFGReorder.itemEstNo = itemfg.est-no
                ttFGReorder.itemCust = itemfg.cust-no
		        ttFGReorder.itemCustPart = itemfg.part-no
                ttFGReorder.blankArea = itemfg.t-sqin
                ttFGReorder.blankAreaUOM = "SI"
                ttFGReorder.blankDimUom = "IN"
                ttFGReorder.multiplier = 1
                ttFGReorder.blankLength = itemfg.t-len
                ttFGReorder.blankWid = itemfg.t-wid
                ttFGReorder.productCategoryID = itemfg.procat
                ttFGReorder.productCategoryDesc = itemfg.procat-desc
                ttFGReorder.quantityAllocated = dQuantityAllocated
                ttFGReorder.quantityOnHand = dQuantityOnHand
                ttFGReorder.quantityOnOrder = dQuantityOnOrder
                ttFGReorder.quantityMinOrder = itemfg.ord-min
                ttFGReorder.quantityMaxOrder = itemfg.ord-max
                ttFGReorder.quantityReorderLevel = itemfg.ord-level
                ttFGReorder.quantityAvailable = dQuantityOnHand + dQuantityOnOrder - dQuantityAllocated
                ttFGReorder.quantityToOrderSuggested = MAX(0,ttFGReorder.quantityReorderLevel - ttFGReorder.quantityAvailable)
                .
            IF ttFGReorder.quantityToOrderSuggested NE 0 AND ttFGReorder.quantityToOrderSuggested LT ttFGReorder.quantityMinOrder THEN 
                ttFGReorder.quantityToOrderSuggested = ttFGReorder.quantityMinOrder.
            IF ttFGReorder.quantityMaxOrder NE 0 AND ttFGReorder.quantityToOrderSuggested GT ttFGReorder.quantityMaxOrder THEN 
                ttFGReorder.quantityToOrderSuggested = ttFGReorder.quantityMaxOrder.
            ttFGReorder.quantityToOrder = ttFGReorder.quantityToOrderSuggested.
            IF ttFGReorder.quantityToOrder EQ 0 THEN DELETE ttFGReorder.
                
        END.
    END.

END PROCEDURE.

