
/*------------------------------------------------------------------------
    File        : system/ CustomerProcs.p
    Purpose     : Centralization of numerous common functions in the import and manual entry of orders

    Syntax      :

    Description : Holds procedures for entering, editing and processing customers

    Author(s)   : Rahul Rawat
    Created     : Wed Apr 29 06:19:23 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE Customer_GetDefaultShipTo:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  ipcCustomer AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opriShipTO  AS ROWID     NO-UNDO.
       
    FIND FIRST shipto NO-LOCK
         WHERE shipto.company   EQ ipcCompany 
           AND shipto.cust-no   EQ ipcCustomer
           AND shipto.isDefault EQ YES
         NO-ERROR.
    IF NOT AVAILABLE shipto THEN 
        FIND FIRST shipto NO-LOCK 
             WHERE shipto.company    EQ ipcCompany 
               AND shipto.cust-no    EQ ipcCustomer
               AND shipto.ship-id    EQ ipcCustomer
               AND shipto.statusCode NE "I"
             NO-ERROR.
    IF NOT AVAILABLE shipto THEN          
        FIND FIRST shipto NO-LOCK 
             WHERE shipto.company    EQ ipcCompany
               AND shipto.cust-no    EQ ipcCustomer
               AND shipto.statusCode NE "I"
             NO-ERROR.
    IF AVAILABLE shipto THEN 
        opriShipTo = ROWID(shipto).
END PROCEDURE.

PROCEDURE Customer_GetNextShipToNo:
/*------------------------------------------------------------------------------
 Purpose: Returns the next shipto id for the given company and customer
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustNo  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiShipNo  AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-shipto FOR shipto.
    
    opiShipNo = 1.
    
    FIND LAST bf-shipto NO-LOCK 
        WHERE bf-shipto.company EQ ipcCompany
          AND bf-shipto.cust-no EQ ipcCustNo 
        USE-INDEX ship-no
        NO-ERROR.
    IF AVAILABLE bf-shipto THEN 
        opiShipNo = bf-shipto.ship-no + 1.
END PROCEDURE.

PROCEDURE Customer_IsActiveShipToAvailable:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  ipcCustomer AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcShipID   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplFound    AS LOGICAL   NO-UNDO.
    
    oplFound = CAN-FIND(FIRST shipto 
                        WHERE shipto.company    EQ ipcCompany 
                          AND shipto.cust-no    EQ ipcCustomer
                          AND shipto.ship-id    NE ipcShipId
                          AND shipto.statusCode NE "I"
                          ).
                         
END PROCEDURE.

