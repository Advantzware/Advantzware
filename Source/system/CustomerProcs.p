
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

PROCEDURE Customer_CalculateOrderBalance:
/*------------------------------------------------------------------------------
 Purpose: Calculates the customer order balance based on given rowid
 Notes:   Orignal Source: ar/updcust1.p, Deprecates ar/updcust1.p
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriCustomer   AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER iplUpdateOrder AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdOrdBalance  AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage     AS CHARACTER NO-UNDO.
       
    DEFINE VARIABLE dOrderRevenue     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTaxAmount        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dOrderLinesTotal  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotalRevenue     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dLinesTotalAmt    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dMiscAmount       AS DECIMAL NO-UNDO.
    DEFINE VARIABLE hdOrderProcs      AS HANDLE  NO-UNDO.
    
    RUN oe/OrderProcs.p PERSISTENT SET hdOrderProcs.
    
    FIND FIRST cust NO-LOCK 
         WHERE ROWID(cust) EQ ipriCustomer
         NO-ERROR.
         
    IF NOT AVAILABLE cust THEN 
        ASSIGN 
            opcMessage = "Invalid customer rowid passed in "
            oplError   = YES
            .     
    ELSE DO:
        FOR EACH oe-ord NO-LOCK
            WHERE oe-ord.company EQ cust.company
              AND oe-ord.cust-no EQ cust.cust-no
              AND oe-ord.opened  EQ YES   
            USE-INDEX opened:
            IF iplUpdateOrder THEN DO:
                RUN Order_CalculateOrderTotal IN hdOrderProcs(  
                    INPUT ROWID(oe-ord),
                    INPUT NO
                    ). 
                /* To read the updated record */    
                FIND CURRENT oe-ord NO-LOCK NO-ERROR.             
            END.          
            dOrderRevenue = oe-ord.t-revenue + oe-ord.tax.   
            
            ASSIGN 
                dOrderLinesTotal = 0
                dMiscAmount      = 0
                dTaxAmount       = 0
                .
                
            RUN Order_GetMiscAmountAndTax IN hdOrderProcs(
                INPUT  oe-ord.company,       
                INPUT  oe-ord.ord-no,
                INPUT  oe-ord.tax-gr,
                OUTPUT dMiscAmount,
                OUTPUT dTaxAmount
                ).
                
            dOrderRevenue = dOrderRevenue - dMiscAmount - dTaxAmount.  
            
            RUN Order_GetLinesTotal IN hdOrderProcs(
                INPUT  oe-ord.company,
                INPUT  oe-ord.ord-no,
                INPUT  oe-ord.tax-gr,
                OUTPUT dOrderLinesTotal
                ).
                
            IF dOrderLinesTotal GT dOrderRevenue THEN 
                dOrderLinesTotal = dOrderRevenue.
             
            ASSIGN
                dLinesTotalAmt = dLinesTotalAmt + dOrderLinesTotal
                dTotalRevenue  = dTotalRevenue  + dOrderRevenue
                .                 
        END. 
        FOR EACH inv-head NO-LOCK
            WHERE inv-head.company EQ cust.company
              AND inv-head.cust-no EQ cust.cust-no
              AND inv-head.bol-no  EQ 0
              AND inv-head.terms   NE "CASH":
          ACCUMULATE inv-head.t-inv-rev (TOTAL).
        END.
        
        opdOrdBalance = dTotalRevenue - dLinesTotalAmt + (ACCUM TOTAL inv-head.t-inv-rev).
        
        IF opdOrdBalance LT 0 
            THEN opdOrdBalance = 0. 
        IF VALID-HANDLE(hdOrderProcs) THEN 
            DELETE PROCEDURE hdOrderProcs.                  
    END.                     
END PROCEDURE.

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

