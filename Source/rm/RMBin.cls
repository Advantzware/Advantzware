 
 /*------------------------------------------------------------------------
    File        : RMBun.cls
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : DEVA$!
    Created     : Thu Feb 24 10:19:31 EST 2022
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

CLASS rm.RMBin: 
    DEFINE BUFFER bf-rm-bin FOR rm-bin.
    
    METHOD PUBLIC LOGICAL SetContext (ipcCompany AS CHARACTER, ipcItemID AS CHARACTER, ipcTag AS CHARACTER):
        
        FIND FIRST bf-rm-bin NO-LOCK
             WHERE bf-rm-bin.company EQ ipcCompany
               AND bf-rm-bin.i-no    EQ ipcItemID
               AND bf-rm-bin.tag     EQ ipcTag
               AND bf-rm-bin.qty     GT 0
             NO-ERROR.
        
        RETURN AVAILABLE bf-rm-bin.
    END METHOD.
    
    METHOD PUBLIC CHARACTER GetValue(INPUT ipcType AS CHARACTER):       
        
        IF NOT AVAILABLE bf-rm-bin THEN
           RETURN "".
                
        CASE ipcType:
            WHEN "Company" THEN
                RETURN bf-rm-bin.company. 
            WHEN "Warehouse" THEN 
                RETURN STRING(bf-rm-bin.loc).
            WHEN "Location" THEN
                RETURN STRING(bf-rm-bin.loc-bin).
            WHEN "ItemID" THEN 
                RETURN STRING(bf-rm-bin.i-no).         
            WHEN "Quantity" THEN 
                RETURN STRING(bf-rm-bin.qty). 
            WHEN "Tag" THEN
                RETURN bf-rm-bin.tag.    
            WHEN "POID" THEN
                RETURN STRING(bf-rm-bin.po-no).    
            WHEN "ROWID" THEN
                RETURN STRING(ROWID(bf-rm-bin)).
            OTHERWISE
                RETURN "".
        END.
    END METHOD.

    METHOD PUBLIC LOGICAL IsAvailable ():
        RETURN AVAILABLE bf-rm-bin.     
    END METHOD.        
END CLASS.