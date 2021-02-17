 
 /*------------------------------------------------------------------------
    File        : ReleaseHeader
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : DEVA$!
    Created     : Sun Feb 14 09:04:17 EST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS oe.ReleaseHeader: 
    DEFINE BUFFER bf-oe-relh FOR oe-relh.

    METHOD PUBLIC LOGICAL SetContext (INPUT ipcCompany AS CHARACTER, INPUT ipiReleaseID AS INTEGER):
        FIND FIRST bf-oe-relh NO-LOCK
             WHERE bf-oe-relh.company  EQ ipcCompany
               AND bf-oe-relh.release# EQ ipiReleaseID
             NO-ERROR.
        
        RETURN AVAILABLE bf-oe-relh.    
    END METHOD.
    
    METHOD PUBLIC CHARACTER GetValue(INPUT ipcType AS CHARACTER):       
        
        IF NOT AVAILABLE bf-oe-relh THEN
           RETURN "".
                
        CASE ipcType:
            WHEN "Company" THEN
                RETURN bf-oe-relh.company. 
            WHEN "ReleaseID" THEN 
                RETURN STRING(bf-oe-relh.release#).
            WHEN "Printed" THEN
                RETURN STRING(bf-oe-relh.printed).
            WHEN "Posted" THEN
                RETURN STRING(bf-oe-relh.posted).
            OTHERWISE
                RETURN "".
        END.
    END METHOD.

    METHOD PUBLIC LOGICAL IsAvailable ():
        RETURN AVAILABLE bf-oe-relh.     
    END METHOD.

END CLASS.