
/*------------------------------------------------------------------------
    File        : ResetCostGroupsAndCategories.p
    Purpose     : 

    Syntax      :

    Description : Deletes all cost groups and categories and creates new ones.

    Author(s)   : BV
    Created     : Fri Apr 24 16:02:41 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
//DEFINE INPUT PARAMETER iplPurgeFirst AS LOGICAL NO-UNDO.

DEFINE VARIABLE iplPurgeFirst AS LOGICAL NO-UNDO INITIAL YES.
DEFINE VARIABLE giCounter AS INTEGER NO-UNDO.
DEFINE VARIABLE glTempOnly AS LOGICAL NO-UNDO INITIAL NO.
DEFINE VARIABLE ghSession AS HANDLE.

DEFINE TEMP-TABLE ttGroupLevel LIKE estCostGroupLevel.
DEFINE TEMP-TABLE ttGroup LIKE estCostGroup.
DEFINE TEMP-TABLE ttCategory LIKE estCostCategory.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetSampleRecKey RETURNS CHARACTER PRIVATE
	(  ) FORWARD.


/* ***************************  Main Block  *************************** */

IF iplPurgeFirst THEN 
    RUN pDeleteAll.
RUN pBuildGroupLevels.
IF glTempOnly THEN RUN pTestGroupLevels.
RUN pBuildGroups.
IF glTempOnly THEN RUN pTestGroups.
RUN pBuildCategories.
IF glTempOnly THEN RUN pTestCategories.


/* **********************  Internal Procedures  *********************** */

PROCEDURE pAddUpdateCategory PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Adds (or updates) a category.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcID LIKE estCostCategory.estCostCategoryID.
    DEFINE INPUT PARAMETER ipcCompany LIKE estCostCategory.company.
    DEFINE INPUT PARAMETER ipiGroupID LIKE estCostCategory.estCostGroupID.
    DEFINE INPUT PARAMETER ipcDesc LIKE estCostCategory.estCostCategoryDesc.
    DEFINE INPUT PARAMETER ipcLabel LIKE estCostCategory.costCategoryLabel.
    DEFINE INPUT PARAMETER iplBoard AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplMat AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplLabor AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplVariableOverhead AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFixedOverhead AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFactory AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplNonFactory AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplProfit AS LOGICAL NO-UNDO.
    
    IF glTempOnly THEN DO:
        FIND FIRST ttCategory EXCLUSIVE-LOCK
            WHERE ttCategory.estCostCategoryID EQ ipcID
            AND ttCategory.company EQ ipcCompany
            NO-ERROR.
        IF NOT AVAILABLE ttCategory THEN
        DO:
            CREATE ttCategory.
            ASSIGN
                ttCategory.rec_key = fGetSampleRecKey()
                ttCategory.estCostCategoryID = ipcID
                ttCategory.company        = ipcCompany
                .
        END.
        ASSIGN
            ttCategory.estCostGroupID = ipiGroupID
            ttCategory.estCostCategoryDesc = ipcDesc
            ttCategory.costCategoryLabel = ipcLabel
            ttCategory.includeInBoardCost = iplBoard
            ttCategory.includeInMaterialCost = iplMat
            ttCategory.includeInLaborCost = iplLabor
            ttCategory.includeInFactoryCost = iplFactory
            ttCategory.includeInNonFactoryCost = iplNonFactory
            ttCategory.includeInNetProfit = iplProfit
            ttCategory.includeInVariableOverheadCost = iplVariableOverhead
            ttCategory.includeInFixedOverheadCost = iplFixedOverhead
            .
    END.
    ELSE DO:
        FIND FIRST estCostCategory EXCLUSIVE-LOCK
            WHERE estCostCategory.estCostCategoryID EQ ipcID
            AND estCostCategory.company EQ ipcCompany
            NO-ERROR.
        IF NOT AVAILABLE estCostCategory THEN
        DO:
            CREATE estCostCategory.
            ASSIGN
                estCostCategory.estCostCategoryID = ipcID
                estCostCategory.company        = ipcCompany
                .
        END.
        ASSIGN
            estCostCategory.estCostGroupID = ipiGroupID
            estCostCategory.estCostCategoryDesc = ipcDesc
            estCostCategory.costCategoryLabel = ipcLabel
            estCostCategory.includeInBoardCost = iplBoard
            estCostCategory.includeInMaterialCost = iplMat
            estCostCategory.includeInLaborCost = iplLabor
            estCostCategory.includeInVariableOverhead = iplVariableOverhead
            estCostCategory.includeInFixedOverhead = iplFixedOverhead
            estCostCategory.includeInFactoryCost = iplFactory
            estCostCategory.includeInNonFactoryCost = iplNonFactory
            estCostCategory.includeInNetProfit = iplProfit
            .    
    END.
END PROCEDURE.


PROCEDURE pAddUpdateGroupLevel PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Adds (or updates) a group level.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiID LIKE estCostGroupLevel.estCostGroupLevelID.
    DEFINE INPUT PARAMETER ipcDesc LIKE estCostGroupLevel.estCostGroupLevelDesc.
    DEFINE INPUT PARAMETER ipcCompany LIKE estCostGroupLevel.company.
    
    IF glTempOnly THEN DO:
        FIND FIRST ttGroupLevel EXCLUSIVE-LOCK
            WHERE ttGroupLevel.estCostGroupLevelID EQ ipiID
            AND ttGroupLevel.company EQ ipcCompany
            NO-ERROR.
        IF NOT AVAILABLE ttGroupLevel THEN
        DO:
            CREATE ttGroupLevel.
            ASSIGN
                ttGroupLevel.rec_key = fGetSampleRecKey()
                ttGroupLevel.estCostGroupLevelID = ipiID
                ttGroupLevel.company           = ipcCompany
                .
        END.
        ASSIGN
            ttGroupLevel.estCostGroupLevelDesc = ipcDesc
            .
    END.
    ELSE DO:
        FIND FIRST estCostGroupLevel EXCLUSIVE-LOCK
            WHERE estCostGroupLevel.estCostGroupLevelID EQ ipiID
            AND estCostGroupLevel.company EQ ipcCompany
            NO-ERROR.
        IF NOT AVAILABLE estCostGroupLevel THEN
        DO:
            CREATE estCostGroupLevel.
            ASSIGN
                estCostGroupLevel.estCostGroupLevelID = ipiID
                estCostGroupLevel.company             = ipcCompany
                .
        END.
        ASSIGN
            estCostGroupLevel.estCostGroupLevelDesc = ipcDesc
            .
    END.
END PROCEDURE.

PROCEDURE pAddUpdateGroup PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Adds (or updates) a group.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcID LIKE estCostGroup.estCostGroupID.
    DEFINE INPUT PARAMETER ipcCompany LIKE estCostGroup.company.
    DEFINE INPUT PARAMETER ipiLevelID LIKE estCostGroup.estCostGroupLevelID.
    DEFINE INPUT PARAMETER ipcDesc LIKE estCostGroup.estCostGroupDesc.
    DEFINE INPUT PARAMETER ipcLabel LIKE estCostGroup.costGroupLabel.
    DEFINE INPUT PARAMETER ipiSequence LIKE estCostGroup.costGroupSequence.
    
    IF glTempOnly THEN DO:
        FIND FIRST ttGroup EXCLUSIVE-LOCK
            WHERE ttGroup.estCostGroupID EQ ipcID
            AND ttGroup.company EQ ipcCompany
            NO-ERROR.
        IF NOT AVAILABLE ttGroup THEN
        DO:
            CREATE ttGroup.
            ASSIGN
                ttGroup.rec_key = fGetSampleRecKey()
                ttGroup.estCostGroupID = ipcID
                ttGroup.company        = ipcCompany
                .
        END.
        ASSIGN
            ttGroup.estCostGroupLevelID = ipiLevelID
            ttGroup.estCostGroupDesc = ipcDesc
            ttGroup.costGroupLabel = ipcLabel
            ttGroup.costGroupSequence = ipiSequence
            .
    END.
    ELSE DO:
        FIND FIRST estCostGroup EXCLUSIVE-LOCK
            WHERE estCostGroup.estCostGroupID EQ ipcID
            AND estCostGroup.company EQ ipcCompany
            NO-ERROR.
        IF NOT AVAILABLE estCostGroup THEN
        DO:
            CREATE estCostGroup.
            ASSIGN
                estCostGroup.estCostGroupID = ipcID
                estCostGroup.company        = ipcCompany
                .
        END.
        ASSIGN
            estCostGroup.estCostGroupLevelID = ipiLevelID
            estCostGroup.estCostGroupDesc = ipcDesc
            estCostGroup.costGroupLabel = ipcLabel
            estCostGroup.costGroupSequence = ipiSequence
            .
    END.
END PROCEDURE.

PROCEDURE pBuildCategories PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Builds Categories
 Notes:
------------------------------------------------------------------------------*/
    RUN pAddUpdateCategory("boardMinDiff", "", "costGroup1", "Board Cost - Min Difference", "Board Cost - Min Difference", YES, YES, NO, NO, NO, YES, NO, NO).
    RUN pAddUpdateCategory("boardNoWaste", "", "costGroup1", "Board Cost - No Waste", "Board Cost - No Waste", YES, YES, NO, NO, NO, YES, NO, NO).
    RUN pAddUpdateCategory("boardRunWaste", "", "costGroup1", "Board Cost - Run Waste", "Board Cost - Run Waste", YES, YES, NO, NO, NO, YES, NO, NO).
    RUN pAddUpdateCategory("boardSetupVend", "", "costGroup1", "Board Cost - Vendor Setup", "Board Cost - Vendor Setup", YES, YES, NO, NO, NO, YES, NO, NO).
    RUN pAddUpdateCategory("boardSetupWaste", "", "costGroup1", "Board Cost - Setup Waste", "Board Cost - Setup Waste", YES, YES, NO, NO, NO, YES, NO, NO).
    
    RUN pAddUpdateCategory("commission", "", "costGroup20", "Commission", "Commission", NO, NO, NO, NO, NO, NO, YES, NO). 
    RUN pAddUpdateCategory("CostCategory67", "", "costGroup4", "Material Handling Charge", "Material Handling Charge", NO, NO, NO, NO, NO, NO, YES, NO). /**/
    RUN pAddUpdateCategory("CostCategory68", "", "costGroup4", "Finished Goods Handling Charge", "Finished Goods Handling Charge", NO, NO, NO, NO, NO, NO, YES, NO). /**/
    
    RUN pAddUpdateCategory("matMinDiff", "", "costGroup1", "Material Cost - Min Difference", "Material Cost - Min Difference", NO, YES, NO, NO, NO, YES, NO, NO).
    RUN pAddUpdateCategory("matNoWaste", "", "costGroup1", "Material Cost - No Waste", "Material Cost - No Waste", NO, YES, NO, NO, NO, YES, NO, NO).
    RUN pAddUpdateCategory("matRunWaste", "", "costGroup1", "Material Cost - Run Waste", "Material Cost - Run Waste", NO, YES, NO, NO, NO, YES, NO, NO).
    RUN pAddUpdateCategory("matSetupVend", "", "costGroup1", "Material Cost - Vendor Setup", "Material Cost - Vendor Setup", NO, YES, NO, NO, NO, YES, NO, NO).
    RUN pAddUpdateCategory("matSetupWaste", "", "costGroup1", "Material Cost - Setup Waste", "Material Cost - Setup Waste", NO, YES, NO, NO, NO, YES, NO, NO).
    
    RUN pAddUpdateCategory("mLabCost", "", "costGroup6", "Misc Labor - Cost - COGS", "Misc Labor - Cost - COGS", NO, NO, YES, NO, NO, YES, NO, NO).
    RUN pAddUpdateCategory("mLabCostSep", "", "costGroup27", "Misc Labor - Cost - Separate Bill", "Misc Labor - Cost - Separate Bill", NO, NO, NO, NO, NO, NO, NO, NO).
    RUN pAddUpdateCategory("mLabPrice", "", "costGroup24", "Misc Labor - Profit - Price", "Misc Labor - Profit - Price", NO, NO, NO, NO, NO, NO, NO, NO).
    RUN pAddUpdateCategory("mLabProfit", "", "costGroup6", "Misc Labor - Profit - COGS", "Misc Labor - Profit - COGS", NO, NO, YES, NO, NO, YES, NO, NO).
    RUN pAddUpdateCategory("mLabProfitSep", "", "costGroup27", "Misc Labor - Profit - Separate Bill", "Misc Labor - Profit - Separate Bill", NO, NO, NO, NO, NO, NO, NO, NO).

    RUN pAddUpdateCategory("mMatCost", "", "costGroup3", "Misc Material - Cost - COGS", "Misc Material - Cost - COGS", NO, YES, NO, NO, NO, YES, NO, NO).
    RUN pAddUpdateCategory("mMatCostSep", "", "costGroup29", "Misc Material - Cost - Separate Bill", "Misc Material - Cost - Separate Bill", NO, NO, NO, NO, NO, NO, NO, NO).
    RUN pAddUpdateCategory("mMatPrice", "", "costGroup22", "Misc Material - Profit - Price", "Misc Material - Profit - Price", NO, NO, NO, NO, NO, NO, NO, NO).
    RUN pAddUpdateCategory("mMatProfit", "", "costGroup3", "Misc Material - Profit - COGS", "Misc Material - Profit - COGS", NO, YES, NO, NO, NO, YES, NO, NO).
    RUN pAddUpdateCategory("mMatProfitSep", "", "costGroup29", "Misc Material - Profit - Separate Bill", "Misc Material - Profit - Separate Bill", NO, NO, NO, NO, NO, NO, NO, NO).
    
    RUN pAddUpdateCategory("nfFolding", "", "costGroup33", "Folding", "Folding", NO, NO, NO, NO, NO, NO, YES, NO).
    RUN pAddUpdateCategory("nfFreight", "", "costGroup19", "Freight Charge", "Freight Charge", NO, NO, NO, NO, NO, NO, YES, NO).
    RUN pAddUpdateCategory("nfFreightMinDiff", "", "costGroup19", "Freight Charge - Minimum Difference", "Freight Charge - Minimum Difference", NO, NO, NO, NO, NO, NO, YES, NO).
    RUN pAddUpdateCategory("nfGSABoard", "", "costGroup31", "GS&A Board", "GS&A Board", NO, NO, NO, NO, NO, NO, YES, NO).
    RUN pAddUpdateCategory("nfGSABoard", "", "costGroup31", "GS&A Board", "GS&A Board", NO, NO, NO, NO, NO, NO, YES, NO).
    RUN pAddUpdateCategory("nfGSALab", "", "costGroup18", "GS&A Labor", "GS&A Labor", NO, NO, NO, NO, NO, NO, YES, NO).
    RUN pAddUpdateCategory("nfGSAMat", "", "costGroup17", "GS&A Material", "GS&A Material", NO, NO, NO, NO, NO, NO, YES, NO).
    RUN pAddUpdateCategory("nfMatMarkup", "", "costGroup30", "Direct Material Markup", "Direct Material Markup", NO, NO, NO, NO, NO, NO, YES, NO).
    RUN pAddUpdateCategory("nfUserDef1", "", "costGroup13", "User Defined Expense 1", "User Defined Expense 1", NO, NO, NO, NO, NO, NO, YES, NO).
    RUN pAddUpdateCategory("nfUserDef2", "", "costGroup14", "User Defined Expense 2", "User Defined Expense 2", NO, NO, NO, NO, NO, NO, YES, NO).
    RUN pAddUpdateCategory("nfUserDef3", "", "costGroup15", "User Defined Expense 3", "User Defined Expense 3", NO, NO, NO, NO, NO, NO, YES, NO).
    RUN pAddUpdateCategory("nfWarehouse", "", "costGroup32", "Warehousing", "Warehousing", NO, NO, NO, NO, NO, NO, YES, NO).
    
    RUN pAddUpdateCategory("opRunDL", "", "costGroup4", "Operation Run - Direct Labor", "Operation Run - DL", NO, NO, YES, NO, NO, YES, NO, NO). 
    RUN pAddUpdateCategory("opRunFO", "", "costGroup8", "Operation Run - Fixed Overhead", "Operation Run - FOH", NO, NO, NO, NO, YES, YES, NO, NO).  
    RUN pAddUpdateCategory("opRunMinDiff", "", "costGroup4", "Operation Run - Min Difference", "Operation Run - Min Difference", NO, NO, YES, NO, NO, YES, NO, NO). 
    RUN pAddUpdateCategory("opRunVO", "", "costGroup7", "Operation Run - Variable Overhead", "Operation Run - VOH", NO, NO, NO, YES, NO, YES, NO, NO). 
    
    RUN pAddUpdateCategory("opSetupDL", "", "costGroup4", "Operation Setup - Direct Labor", "Operation Setup - DL", NO, NO, YES, NO, NO, YES, NO, NO). 
    RUN pAddUpdateCategory("opSetupFO", "", "costGroup8", "Operation Setup - Fixed Overhead", "Operation Setup - FOH", NO, NO, NO, NO, YES, YES, NO, NO). 
    RUN pAddUpdateCategory("opSetupMinDiff", "", "costGroup4", "Operation Setup - Min Difference", "Operation Setup - Min Difference", NO, NO, YES, NO, NO, YES, NO, NO).
    RUN pAddUpdateCategory("opSetupVO", "", "costGroup7", "Operation Setup - Variable Overhead", "Operation Setup - VOH", NO, NO, NO, YES, NO, YES, NO, NO).
    
    RUN pAddUpdateCategory("pLabCost", "", "costGroup5", "Tooling Labor - Cost - COGS", "Tooling Labor - Cost - COGS", NO, NO, YES, NO, NO, YES, NO, NO). 
    RUN pAddUpdateCategory("pLabCostSep", "", "costGroup26", "Tooling Labor - Cost - Separate Bill", "Tooling Labor - Cost - Separate Bill", NO, NO, NO, NO, NO, NO, NO, NO).
    RUN pAddUpdateCategory("pLabPrice", "", "costGroup23", "Tooling Labor - Profit - Price", "Tooling Labor - Profit - Price", NO, NO, NO, NO, NO, NO, NO, YES).
    RUN pAddUpdateCategory("pLabProfit", "", "costGroup5", "Tooling Labor - Profit - COGS", "Tooling Labor - Profit - COGS", NO, NO, YES, NO, NO, YES, NO, NO). 
    RUN pAddUpdateCategory("pLabProfitSep", "", "costGroup26", "Tooling Labor - Profit - Separate Bill", "Tooling Labor - Profit - Separate Bill", NO, NO, NO, NO, NO, NO, NO, NO).         
    
    RUN pAddUpdateCategory("pMatCost", "", "costGroup2", "Tooling Material - Cost - COGS", "Tooling Material - Cost - COGS", NO, YES, NO, NO, NO, YES, NO, NO). 
    RUN pAddUpdateCategory("pMatCostSep", "", "costGroup28", "Tooling Material - Cost - Separate Bill", "Tooling Material - Cost - Separate Bill", NO, NO, NO, NO, NO, NO, NO, NO).
    RUN pAddUpdateCategory("pMatPrice", "", "costGroup21", "Tooling Material - Profit - Price", "Tooling Material - Profit - Price", NO, NO, NO, NO, NO, NO, NO, YES). 
    RUN pAddUpdateCategory("pMatProfit", "", "costGroup2", "Tooling Material - Profit - COGS", "Tooling Material - Profit - COGS", NO, YES, NO, NO, NO, YES, NO, NO). 
    RUN pAddUpdateCategory("pMatProfitSep", "", "costGroup28", "Tooling Material - Profit - Separate Bill", "Tooling Material - Profit - Separate Bill", NO, NO, NO, NO, NO, NO, NO, NO).         

    RUN pAddUpdateCategory("pProfit", "", "costGroup25", "Profit", "Profit", NO, NO, NO, NO, NO, NO, NO, YES).
    
    RUN pAddUpdateCategory("deviation", "", "costGroup34", "Deviation", "Deviation", NO, NO, NO, NO, NO,NO, NO, NO).
    
END PROCEDURE.

PROCEDURE pBuildGroups PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Builds Groups
 Notes:
------------------------------------------------------------------------------*/
    RUN pAddUpdateGroup("costGroup1", "", 1, "Direct Materials Dscr", "Direct Material", 1).
    RUN pAddUpdateGroup("costGroup10", "", 3, "Misc Labor - Expenses Dscr", "Misc Labor - Expenses", 10).
    RUN pAddUpdateGroup("costGroup11", "", 3, "Prep Material - Expenses Dscr", "Prep Material - Expenses", 11).
    RUN pAddUpdateGroup("costGroup12", "", 3, "Misc Material - Expenses Dscr", "Misc Material - Expenses", 12).
    RUN pAddUpdateGroup("costGroup13", "", 3, "User Defined Expense 1 Dscr", "User Defined Expense 1", 13).
    RUN pAddUpdateGroup("costGroup14", "", 3, "User Defined Expense 2 Dscr", "User Defined Expense 2", 14).
    RUN pAddUpdateGroup("costGroup15", "", 3, "User Defined Expense 3 Dscr", "User Defined Expense 3", 15).
    RUN pAddUpdateGroup("costGroup16", "", 3, "Other Expenses Dscr", "Other Expenses", 16).
    RUN pAddUpdateGroup("costGroup17", "", 3, "GS&A Material Dscr", "GS&A Material", 19).
    RUN pAddUpdateGroup("costGroup18", "", 3, "GS&A Labor Dscr", "GS&A Labor", 20).
    RUN pAddUpdateGroup("costGroup19", "", 3, "Freight Dscr", "Freight", 23).
    RUN pAddUpdateGroup("costGroup2", "", 1, "Prep Material - Factory Dscr", "Prep Material - Factory", 2).
    RUN pAddUpdateGroup("costGroup20", "", 3, "Commission Dscr", "Commission", 24).
    RUN pAddUpdateGroup("costGroup21", "", 4, "Prep Material - Profit Dscr", "Prep Material - Profit", 25).
    RUN pAddUpdateGroup("costGroup22", "", 4, "Misc Material - Profit Dscr", "Misc Material - Profit", 26).
    RUN pAddUpdateGroup("costGroup23", "", 4, "Prep Labor - Profit Dscr", "Prep Labor - Profit", 27).
    RUN pAddUpdateGroup("costGroup24", "", 4, "Misc Labor - Profit Dscr", "Misc Labor - Profit", 28).
    RUN pAddUpdateGroup("costGroup25", "", 4, "Profit Dscr", "Profit", 29).
    RUN pAddUpdateGroup("costGroup26", "", 5, "Prep Labor - Separate Dscr", "Prep Labor - Separate", 30).
    RUN pAddUpdateGroup("costGroup27", "", 5, "Misc Labor - Separate Dscr", "Misc Labor - Separate", 31).
    RUN pAddUpdateGroup("costGroup28", "", 5, "Prep Material - Separate Dscr", "Prep Material - Separate", 32).
    RUN pAddUpdateGroup("costGroup29", "", 5, "Misc Material - Separate Dscr", "Misc Material - Separate", 33).    
    RUN pAddUpdateGroup("costGroup3", "", 1, "Misc Material - Factory Dscr", "Misc Material - Factory", 3).
    RUN pAddUpdateGroup("costGroup30", "", 3, "Direct Material Markup", "Direct Material Markup", 21).
    RUN pAddUpdateGroup("costGroup31", "", 3, "GS&A Board", "GS&A Board", 18).
    RUN pAddUpdateGroup("costGroup32", "", 3, "Warehousing", "Warehousing", 17).
    RUN pAddUpdateGroup("costGroup33", "", 3, "Folding", "Folding", 22).
    RUN pAddUpdateGroup("costGroup34", "", 5, "Cost Deviation", "Cost Deviation", 23).
    RUN pAddUpdateGroup("costGroup4", "", 1, "Direct Labor Dscr", "Direct Labor", 4).
    RUN pAddUpdateGroup("costGroup5", "", 1, "Prep Labor - Factory Dscr", "Prep Labor - Factory", 5).
    RUN pAddUpdateGroup("costGroup6", "", 1, "Misc Labor - Factory Dscr", "Misc Labor - Factory", 6).
    RUN pAddUpdateGroup("costGroup7", "", 1, "Variable Overhead Dscr", "Variable Overhead", 7).
    RUN pAddUpdateGroup("costGroup8", "", 2, "Fixed Overhead Dscr", "Fixed Overhead", 8).
    RUN pAddUpdateGroup("costGroup9", "", 3, "Prep Labor - Expenses Dscr", "Prep Labor - Expenses", 9).

END PROCEDURE.

PROCEDURE pBuildGroupLevels PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:  Builds estCostGroupLevels
 Notes:
------------------------------------------------------------------------------*/

    RUN pAddUpdateGroupLevel(1,"Direct Cost", "").
    RUN pAddUpdateGroupLevel(2,"Factory Cost", "").
    RUN pAddUpdateGroupLevel(3,"Full Cost", "").
    RUN pAddUpdateGroupLevel(4,"Selling Price", "").
    

END PROCEDURE.

PROCEDURE pDeleteAll PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Deletes all GroupLevels, Groups and Categories
     Notes:
    ------------------------------------------------------------------------------*/
    IF glTempOnly THEN DO:
        EMPTY TEMP-TABLE ttGroupLevel.
        EMPTY TEMP-TABLE ttGroup.
        EMPTY TEMP-TABLE ttCategory.
    END.
    ELSE DO:
        FOR EACH estCostCategory:
            DELETE estCostCategory.
        END.
        FOR EACH estCostGroup:
            DELETE estCostGroup.
        END.
        FOR EACH estCostGroupLevel:
            DELETE estCostGroupLevel.
        END.
    END.
    
END PROCEDURE.

PROCEDURE pTestCategories PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    FOR EACH estCostCategory NO-LOCK
        BY estCostCategory.estCostCategoryID:
        FIND FIRST ttCategory NO-LOCK 
            WHERE ttCategory.estCostCategoryID EQ estCostCategory.estCostCategoryID 
            AND ttCategory.estCostGroupID EQ estCostCategory.estCostGroupID 
            AND ttCategory.company EQ estCostCategory.company
            AND ttCategory.estCostCategoryDesc EQ estCostCategory.estCostCategoryDesc
            AND ttCategory.costCategoryLabel EQ estCostCategory.costCategoryLabel
            AND ttCategory.includeInBoardCost EQ estCostCategory.includeInBoardCost
            AND ttCategory.includeInMaterialCost EQ estCostCategory.includeInMaterialCost
            AND ttCategory.includeInLaborCost EQ estCostCategory.includeInLaborCost
            AND ttCategory.includeInFactoryCost EQ estCostCategory.includeInFactoryCost
            AND ttCategory.includeInNonFactoryCost EQ estCostCategory.includeInNonFactoryCost
            AND ttCategory.includeInNetProfit EQ estCostCategory.includeInNetProfit
            NO-ERROR.
        IF NOT AVAILABLE ttCategory THEN DO: 
            ASSIGN 
                lError = YES
                cMessage = "Category - No Match: " + STRING(estCostCategory.estCostCategoryID)
                .
            LEAVE.
        END.
    END.
    IF lError THEN
        MESSAGE cMessage
            VIEW-AS ALERT-BOX.
        
END PROCEDURE.

PROCEDURE pTestGroupLevels PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    FOR EACH estCostGroupLevel NO-LOCK:
        FIND FIRST ttGroupLevel NO-LOCK 
            WHERE ttGroupLevel.estCostGroupLevelID EQ estCostGroupLevel.estCostGroupLevelID
            AND ttGroupLevel.company EQ estCostGroupLevel.company
            AND ttGroupLevel.estCostGroupLevelDesc EQ estCostGroupLevel.estCostGroupLevelDesc
            NO-ERROR.
        IF NOT AVAILABLE ttGroupLevel THEN DO: 
            ASSIGN 
                lError = YES
                cMessage = "Group Level No Match: " + STRING(estCostGroupLevel.estCostGroupLevelID)
                .
            LEAVE.
        END.
    END.
    IF lError THEN
        MESSAGE cMessage
            VIEW-AS ALERT-BOX.
        
END PROCEDURE.

PROCEDURE pTestGroups PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    FOR EACH estCostGroup NO-LOCK
        BY estCostGroup.costGroupSequence:
        FIND FIRST ttGroup NO-LOCK 
            WHERE ttGroup.estCostGroupID EQ estCostGroup.estCostGroupID 
            AND ttGroup.estCostGroupLevelID EQ estCostGroup.estCostGroupLevelID
            AND ttGroup.company EQ estCostGroup.company
            AND ttGroup.estCostGroupDesc EQ estCostGroup.estCostGroupDesc
            AND ttGroup.costGroupLabel EQ estCostGroup.costGroupLabel
            AND ttGroup.costGroupSequence EQ estCostGroup.costGroupSequence
            NO-ERROR.
        IF NOT AVAILABLE ttGroup THEN DO: 
            ASSIGN 
                lError = YES
                cMessage = "Group - No Match: " + STRING(estCostGroup.estCostGroupID)
                .
            LEAVE.
        END.
    END.
    IF lError THEN
        MESSAGE cMessage
            VIEW-AS ALERT-BOX.
        
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fGetSampleRecKey RETURNS CHARACTER PRIVATE
	(  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	
    giCounter = giCounter + 1.
	RETURN STRING(giCounter). 
END FUNCTION.

