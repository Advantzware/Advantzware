 
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
    DEFINE VARIABLE gcSSVendorTags AS CHARACTER NO-UNDO INITIAL "No Vendor Tags".
    
    DEFINE BUFFER bf-loadtag FOR loadtag.
    
    METHOD PUBLIC LOGICAL SetContext (ipcCompany AS CHARACTER, iplItemType AS LOGICAL, ipcTag AS CHARACTER):

        RUN spGetSettingByName ("SSVendorTags", OUTPUT gcSSVendorTags).

        FIND FIRST bf-loadtag NO-LOCK
             WHERE bf-loadtag.company   EQ ipcCompany
               AND bf-loadtag.item-type EQ iplItemType
               AND bf-loadtag.tag-no    EQ ipcTag
             NO-ERROR.

        IF gcSSVendorTags NE "No Vendor Tags" THEN DO:
            IF NOT AVAILABLE bf-loadtag THEN
                FIND FIRST bf-loadtag NO-LOCK
                     WHERE bf-loadtag.company      EQ ipcCompany
                       AND bf-loadtag.item-type    EQ iplItemType
                       AND bf-loadtag.misc-char[1] EQ ipcTag
                     NO-ERROR.
        END.
        
        RETURN AVAILABLE bf-loadtag.
    END METHOD.

    METHOD PUBLIC LOGICAL SetContext (ipcCompany AS CHARACTER, ipcTag AS CHARACTER):

        RUN spGetSettingByName ("SSVendorTags", OUTPUT gcSSVendorTags).

        FIND FIRST bf-loadtag NO-LOCK
             WHERE bf-loadtag.company   EQ ipcCompany
               AND bf-loadtag.item-type EQ YES
               AND bf-loadtag.tag-no    EQ ipcTag
             NO-ERROR.

        IF NOT AVAILABLE bf-loadtag THEN
            FIND FIRST bf-loadtag NO-LOCK
                 WHERE bf-loadtag.company   EQ ipcCompany
                   AND bf-loadtag.item-type EQ NO
                   AND bf-loadtag.tag-no    EQ ipcTag
                 NO-ERROR.

        IF gcSSVendorTags NE "No Vendor Tags" THEN DO:
            IF NOT AVAILABLE bf-loadtag THEN
                FIND FIRST bf-loadtag NO-LOCK
                     WHERE bf-loadtag.company      EQ ipcCompany
                       AND bf-loadtag.item-type    EQ YES
                       AND bf-loadtag.misc-char[1] EQ ipcTag
                     NO-ERROR.
    
            IF NOT AVAILABLE bf-loadtag THEN
                FIND FIRST bf-loadtag NO-LOCK
                     WHERE bf-loadtag.company      EQ ipcCompany
                       AND bf-loadtag.item-type    EQ NO
                       AND bf-loadtag.misc-char[1] EQ ipcTag
                     NO-ERROR.
        END.
        
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
            WHEN "ItemType" THEN 
                RETURN STRING(bf-loadtag.item-type).   
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
            WHEN "Tag" THEN
                RETURN bf-loadtag.tag-no.
            WHEN "ItemName" THEN
                RETURN bf-loadtag.i-name.     
            OTHERWISE
                RETURN "".
        END.
    END METHOD.

    METHOD PUBLIC LOGICAL IsAvailable ():
        RETURN AVAILABLE bf-loadtag.     
    END METHOD.        
END CLASS.