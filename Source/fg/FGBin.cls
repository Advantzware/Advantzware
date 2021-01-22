 
 /*------------------------------------------------------------------------
    File        : fg-bin
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : DEVA$!
    Created     : Thu Jan 14 04:58:31 EST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

CLASS fg.FGBin: 
    DEFINE BUFFER bf-fg-bin FOR fg-bin.
    
    METHOD PUBLIC LOGICAL SetContext (ipcCompany AS CHARACTER, ipcItemID AS CHARACTER, ipcTag AS CHARACTER):
        
        FIND FIRST bf-fg-bin NO-LOCK
             WHERE bf-fg-bin.company EQ ipcCompany
               AND bf-fg-bin.i-no    EQ ipcItemID
               AND bf-fg-bin.tag     EQ ipcTag
               AND bf-fg-bin.qty     GT 0
             NO-ERROR.
        
        RETURN AVAILABLE bf-fg-bin.
    END METHOD.
    
    METHOD PUBLIC CHARACTER GetValue(INPUT ipcType AS CHARACTER):       
        
        IF NOT AVAILABLE bf-fg-bin THEN
           RETURN "".
                
        CASE ipcType:
            WHEN "Company" THEN
                RETURN bf-fg-bin.company. 
            WHEN "Warehouse" THEN 
                RETURN STRING(bf-fg-bin.loc).
            WHEN "Location" THEN
                RETURN STRING(bf-fg-bin.loc-bin).
            WHEN "ItemID" THEN 
                RETURN STRING(bf-fg-bin.i-no).         
            WHEN "Quantity" THEN 
                RETURN STRING(bf-fg-bin.qty). 
            WHEN "QuantityInSubUnit" THEN 
                RETURN STRING(bf-fg-bin.case-count).
            WHEN "SubUnitsPerUnit" THEN
                RETURN STRING(TRUNC((bf-fg-bin.qty - bf-fg-bin.partial-count) / bf-fg-bin.case-count,0)).
            WHEN "Partial" THEN
                RETURN STRING(bf-fg-bin.partial-count).                           
            OTHERWISE
                RETURN "".
        END.
    END METHOD.

    METHOD PUBLIC LOGICAL IsAvailable ():
        RETURN AVAILABLE bf-fg-bin.     
    END METHOD.        
END CLASS.