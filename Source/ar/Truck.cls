 
 /*------------------------------------------------------------------------
    File        : Truck
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : DEVA$!
    Created     : Wed Sep 15 23:36:27 IST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS ar.Truck: 
    DEFINE BUFFER bf-truck FOR truck.

    METHOD PUBLIC LOGICAL SetContext (INPUT ipcCompany AS CHARACTER, INPUT ipcTruckID AS CHARACTER):
        FIND FIRST bf-truck NO-LOCK
             WHERE bf-truck.company    EQ ipcCompany
               AND bf-truck.truck-code EQ ipcTruckID
             NO-ERROR.
        
        RETURN AVAILABLE bf-truck.    
    END METHOD.
    
    METHOD PUBLIC CHARACTER GetValue(INPUT ipcType AS CHARACTER):       
        
        IF NOT AVAILABLE bf-truck THEN
           RETURN "".
                
        CASE ipcType:
            WHEN "Company" THEN
                RETURN bf-truck.company. 
            WHEN "ReleaseID" THEN 
                RETURN STRING(bf-truck.truck-code).
            WHEN "Location" THEN
                RETURN STRING(bf-truck.loc).
            OTHERWISE
                RETURN "".
        END.
    END METHOD.

    METHOD PUBLIC LOGICAL IsAvailable ():
        RETURN AVAILABLE bf-truck.     
    END METHOD.
END CLASS.