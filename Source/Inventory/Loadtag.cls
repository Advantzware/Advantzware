 
 /*------------------------------------------------------------------------
    File        : Loadtag
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : DEVA$!
    Created     : Thu Jan 14 04:58:31 EST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

CLASS Inventory.Loadtag: 
    DEFINE BUFFER bf-loadtag FOR loadtag.
    
    METHOD PUBLIC LOGICAL SetContext (ipcCompany AS CHARACTER, iplItemType AS LOGICAL, ipcTag AS CHARACTER):

        FIND FIRST bf-loadtag NO-LOCK
             WHERE bf-loadtag.company   EQ ipcCompany
               AND bf-loadtag.item-type EQ iplItemType
               AND bf-loadtag.tag-no    EQ ipcTag
             NO-ERROR.
        
        RETURN AVAILABLE bf-loadtag.
    END METHOD.
    
    METHOD PUBLIC CHARACTER GetValue(INPUT ipcType AS CHARACTER):       
        
        IF NOT AVAILABLE bf-loadtag THEN
           RETURN "".
                
        CASE ipcType:
            WHEN "Company" THEN
                RETURN bf-loadtag.company. 
            WHEN "Warehouse" THEN 
                RETURN STRING(bf-loadtag.loc).
            WHEN "Location" THEN
                RETURN STRING(bf-loadtag.loc-bin).
            WHEN "ItemID" THEN 
                RETURN STRING(bf-loadtag.i-no).         
            WHEN "Quantity" THEN 
                RETURN STRING(bf-loadtag.qty). 
            WHEN "PO" THEN
                RETURN STRING(bf-loadtag.po-no).
            WHEN "POLine" THEN
                RETURN STRING(bf-loadtag.line).   
            WHEN "JobID" THEN
                RETURN bf-loadtag.job-no.
            WHEN "JobID2" THEN
                RETURN STRING(bf-loadtag.job-no2). 
            WHEN "QuantityInSubUnit" THEN 
                RETURN STRING(bf-loadtag.qty-case).
            WHEN "SubUnitsPerUnit" THEN
                RETURN STRING(bf-loadtag.case-bundle).
            WHEN "Partial" THEN
                RETURN STRING(bf-loadtag.partial).       
            OTHERWISE
                RETURN "".
        END.
    END METHOD.

    METHOD PUBLIC LOGICAL IsAvailable ():
        RETURN AVAILABLE bf-loadtag.     
    END METHOD.        
END CLASS.