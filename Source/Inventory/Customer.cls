 
 /*------------------------------------------------------------------------
    File        : Inventory.Customer
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : DEVA$!
    Created     : Thu Jan 19 04:58:31 EST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

CLASS Inventory.Customer: 
    DEFINE BUFFER bf-cust FOR cust.
    
    METHOD PUBLIC LOGICAL SetContext (ipcCompany AS CHARACTER, ipcCustID AS CHARACTER):

        FIND FIRST bf-cust NO-LOCK
             WHERE bf-cust.company EQ ipcCompany
               AND bf-cust.cust-no EQ ipcCustID
             NO-ERROR.
        
        RETURN AVAILABLE bf-cust.
    END METHOD.
    
    METHOD PUBLIC CHARACTER GetValue(INPUT ipcType AS CHARACTER):       
        
        IF NOT AVAILABLE bf-cust THEN
           RETURN "".
                
        CASE ipcType:
            WHEN "Company" THEN
                RETURN bf-cust.company. 
            WHEN "Warehouse" THEN 
                RETURN STRING(bf-cust.loc).
            WHEN "PrintCopies" THEN
                RETURN STRING(bf-cust.int-field[1]).
            OTHERWISE
                RETURN "".
        END.
    END METHOD.

    METHOD PUBLIC LOGICAL IsAvailable ():
        RETURN AVAILABLE bf-cust.     
    END METHOD.        
END CLASS.