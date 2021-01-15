 
 /*------------------------------------------------------------------------
    File        : ItemFG
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : DEVA$!
    Created     : Wed Jan 06 05:44:46 EST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS fg.ItemFG: 
    DEFINE BUFFER bf-itemfg FOR itemfg.

    DEFINE PRIVATE VARIABLE lIsAvailable AS LOGICAL NO-UNDO.
        
    METHOD PUBLIC LOGICAL SetContext (INPUT ipcCompany AS CHARACTER, INPUT ipcItemID AS CHARACTER):
        lIsAvailable = FALSE.

        FIND FIRST bf-itemfg NO-LOCK
             WHERE bf-itemfg.company EQ ipcCompany
               AND bf-itemfg.i-no    EQ ipcItemID
             NO-ERROR.
        
        lIsAvailable = AVAILABLE bf-itemfg.    

        RETURN lIsAvailable.    
    END METHOD.
    
    METHOD PUBLIC CHARACTER GetValue(INPUT ipcType AS CHARACTER):       
        
        IF NOT AVAILABLE bf-itemfg THEN
           RETURN "".
                
        CASE ipcType:
            WHEN "Company" THEN
                RETURN bf-itemfg.company. 
            WHEN "ItemID" THEN 
                RETURN STRING(bf-itemfg.i-no).
            WHEN "ItemName" THEN
                RETURN STRING(bf-itemfg.i-name).
            WHEN "QuantityInSubUnit" THEN 
                RETURN STRING(bf-itemfg.case-count).
            WHEN "SubUnitsPerUnit" THEN
                RETURN STRING(bf-itemfg.case-pall).
            WHEN "PurchaseUOM" THEN
                RETURN bf-itemfg.pur-uom.
            OTHERWISE
                RETURN "".
        END.
    END METHOD.

    METHOD PUBLIC LOGICAL IsAvailable ():
        RETURN AVAILABLE bf-itemfg.     
    END METHOD.

END CLASS.