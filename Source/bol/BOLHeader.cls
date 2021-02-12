 
 /*------------------------------------------------------------------------
    File        : oe-bolh
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : DEVA$!
    Created     : Wed Jan 22 05:44:46 EST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS bol.BOLHeader: 
    DEFINE BUFFER bf-oe-bolh FOR oe-bolh.

    DEFINE PRIVATE VARIABLE lIsAvailable AS LOGICAL NO-UNDO.
        
    METHOD PUBLIC LOGICAL SetContext (ipcCompany AS CHARACTER, ipiBOLID AS INTEGER):
        lIsAvailable = FALSE.

        FIND FIRST bf-oe-bolh NO-LOCK
             WHERE bf-oe-bolh.company EQ ipcCompany
               AND bf-oe-bolh.bol-no  EQ ipiBOLID
             NO-ERROR.
        
        lIsAvailable = AVAILABLE bf-oe-bolh.    

        RETURN lIsAvailable.    
    END METHOD.
    
    METHOD PUBLIC CHARACTER GetValue(INPUT ipcType AS CHARACTER):       
        
        IF NOT AVAILABLE bf-oe-bolh THEN
           RETURN "".
                
        CASE ipcType:
            WHEN "Company" THEN
                RETURN bf-oe-bolh.company. 
            WHEN "BOLID" THEN 
                RETURN STRING(bf-oe-bolh.bol-no).
            OTHERWISE
                RETURN "".
        END.
    END METHOD.

    METHOD PUBLIC LOGICAL IsAvailable ():
        RETURN AVAILABLE bf-oe-bolh.     
    END METHOD.

END CLASS.