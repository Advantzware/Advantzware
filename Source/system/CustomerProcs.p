
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

PROCEDURE Customer_getShiptoDetails:
    
    DEFINE INPUT  PARAMETER ip-cCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ip-cCustNo  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ip-cShipId  AS CHARACTER NO-UNDO.
    
    DEFINE OUTPUT PARAMETER op-cShipName  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER op-cShipAddr1 AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER op-cShipAddr2 AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER op-cCity      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER op-cState     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER op-cZip       AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER op-cCarrier   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER op-cdestCode  AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-Shipto FOR shipto.
    
    FIND FIRST bf-shipto NO-LOCK 
        WHERE bf-shipto.Company = ip-cCompany
          AND bf-shipto.Cust-no = ip-cCustNo
          AND bf-shipto.Ship-Id = ip-cShipId NO-ERROR.
    
    IF AVAILABLE bf-shipto THEN
        ASSIGN 
            op-cShipName  = bf-shipto.ship-name
            op-cShipAddr1 = bf-shipto.ship-addr[1]
            op-cShipAddr2 = bf-shipto.ship-addr[2]
            op-cCity      = bf-shipto.ship-city
            op-cState     = bf-shipto.ship-state
            op-cZip       = bf-shipto.ship-zip
            op-cCarrier   = bf-shipto.carrier
            op-cdestCode  = bf-shipto.dest-code.
            
END.

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

PROCEDURE Customer_GetDefaultCustomer:
/*------------------------------------------------------------------------------
 Purpose: Returns the next shipto id for the given company and customer
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCustID    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCustName  AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-cust FOR cust.
    
    RUN pGetDefaultCustomerBuffer(
        INPUT  ipcCompany,
        BUFFER bf-cust
        ).
    IF AVAILABLE bf-cust THEN 
    ASSIGN
          opcCustID = bf-cust.cust-no
          opcCustName = bf-cust.name.
          
END PROCEDURE.

PROCEDURE pGetDefaultCustomerBuffer PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-cust        FOR cust.

        
    FIND FIRST opbf-cust
        WHERE opbf-cust.company EQ ipcCompany
          AND opbf-cust.ACTIVE EQ "X"
        NO-LOCK NO-ERROR.
   

END PROCEDURE.

PROCEDURE Customer_InterCompanyTrans:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  ipcCustomer AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcShipID   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcSoldID   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError    AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.
    
    RUN pInterCompanyTrans(
                  INPUT ipcCompany,
                  INPUT ipcCustomer,
                  INPUT ipcShipID,
                  INPUT ipcSoldID,
                  OUTPUT oplError,
                  OUTPUT opcMessage
                  ).
END PROCEDURE.

PROCEDURE pInterCompanyTrans:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  ipcCustomer AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcShipID   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcSoldID   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError    AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cTransCompany AS CHARACTER INIT "002" NO-UNDO.
    DEFINE VARIABLE lCustExist    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iInterCompanyBilling AS INTEGER NO-UNDO.
    DEFINE VARIABLE cCustomerValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShiptoValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iAsc AS INTEGER NO-UNDO.
    DEFINE VARIABLE cChar AS CHARACTER NO-UNDO.     
    DEFINE VARIABLE lSoldUsed AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-cust     FOR cust.
    DEFINE BUFFER bf-shipto   FOR shipto. 
    DEFINE BUFFER bf-soldto   FOR soldto.
    DEFINE BUFFER bff-shipto  FOR shipto.
    DEFINE BUFFER bff-soldto  FOR soldto.
    DEFINE BUFFER bf-ori-cust FOR cust.
        
    FIND FIRST shipto NO-LOCK 
        WHERE shipto.company  EQ ipcCompany 
        AND shipto.cust-no    EQ ipcCustomer
        AND shipto.ship-id    EQ ipcShipID           
        NO-ERROR.
    FIND FIRST bf-ori-cust NO-LOCK
         WHERE bf-ori-cust.company EQ ipcCompany
         AND bf-ori-cust.cust-no   EQ ipcCustomer
         NO-ERROR.
    FIND FIRST soldto NO-LOCK
         WHERE soldto.company EQ ipcCompany
         AND soldto.cust-no EQ ipcCustomer
         AND soldto.sold-id EQ ipcSoldID NO-ERROR.     
       
    RUN pGetNk1Settings (
        ipcCompany,
        ipcCustomer,
        OUTPUT lCustExist,
        OUTPUT iInterCompanyBilling,
        INPUT-OUTPUT cTransCompany
        ).
        
    IF iInterCompanyBilling EQ 1 AND ipcSoldID NE "" THEN
    DO:
        lSoldUsed = NO.
        cChar =  substring(ipcShipID,1,1).
        iAsc = ASC(cChar).
        IF iAsc GE 65 AND iAsc LE 122 THEN
        lSoldUsed = YES.         
    END.
        
    cCustomerValue = IF lSoldUsed THEN ipcSoldID ELSE IF iInterCompanyBilling EQ 1 THEN ipcShipID ELSE ipcCustomer .   
    cShiptoValue =  ipcShipID .
       
    IF NOT lCustExist THEN RETURN .
    
    IF AVAIL shipto THEN
    DO:
        FIND FIRST cust NO-LOCK
            WHERE cust.company EQ cTransCompany
            AND cust.active = "X"
            NO-ERROR.  
        IF AVAIL cust THEN
        DO:
            FIND FIRST bf-cust EXCLUSIVE-LOCK 
                WHERE bf-cust.company EQ cust.company 
                AND bf-cust.cust-no EQ cCustomerValue NO-ERROR .
            IF NOT AVAIL bf-cust THEN
            DO:           
                CREATE bf-cust.
                BUFFER-COPY cust EXCEPT cust-no tax-gr TO bf-cust.
                ASSIGN
                    bf-cust.cust-no      = cCustomerValue
                    bf-cust.ACTIVE       = "A"
                    bf-cust.internal     = NO .                                                            
              
                IF lSoldUsed AND AVAIL soldto THEN
                ASSIGN
                    bf-cust.NAME         = soldto.sold-name
                    bf-cust.addr[1]      = soldto.sold-addr[1]
                    bf-cust.addr[2]      = soldto.sold-addr[2]
                    bf-cust.spare-char-3 = soldto.spare-char-3
                    bf-cust.city         = soldto.sold-city
                    bf-cust.state        = soldto.sold-state
                    bf-cust.zip          = soldto.sold-zip
                    bf-cust.fax-country  = shipto.country .
                
                ELSE IF iInterCompanyBilling EQ 1 THEN
                 ASSIGN
                    bf-cust.NAME         = shipto.ship-name
                    bf-cust.addr[1]      = shipto.ship-addr[1]
                    bf-cust.addr[2]      = shipto.ship-addr[2]
                    bf-cust.spare-char-3 = shipto.spare-char-3
                    bf-cust.city         = shipto.ship-city
                    bf-cust.state        = shipto.ship-state
                    bf-cust.zip          = shipto.ship-zip
                    bf-cust.fax-country  = shipto.country .
                ELSE IF AVAIL bf-ori-cust THEN
                  ASSIGN
                    bf-cust.NAME         = bf-ori-cust.name
                    bf-cust.addr[1]      = bf-ori-cust.addr[1]
                    bf-cust.addr[2]      = bf-ori-cust.addr[2]
                    bf-cust.spare-char-3 = bf-ori-cust.spare-char-3
                    bf-cust.city         = bf-ori-cust.city
                    bf-cust.state        = bf-ori-cust.state
                    bf-cust.zip          = bf-ori-cust.zip
                    bf-cust.fax-country  = bf-ori-cust.fax-country .
            END.
            FIND FIRST bf-shipto EXCLUSIVE-LOCK
                 WHERE bf-shipto.company EQ cTransCompany
                 AND bf-shipto.cust-no EQ cCustomerValue
                 AND bf-shipto.ship-id EQ cShiptoValue NO-ERROR.
                 
            IF NOT AVAIL bf-shipto THEN
            DO:     
                FIND LAST bff-shipto
                    WHERE bff-shipto.company EQ cTransCompany
                    AND bff-shipto.cust-no EQ cCustomerValue                      
                    USE-INDEX ship-no NO-LOCK NO-ERROR.
                     
                CREATE bf-shipto .       
                BUFFER-COPY shipto EXCEPT company cust-no ship-id TO bf-shipto.
                ASSIGN
                    bf-shipto.company   = cTransCompany
                    bf-shipto.cust-no   = cCustomerValue                     
                    bf-shipto.ship-id   = cShiptoValue
                    bf-shipto.isDefault = YES
                    bf-shipto.ship-no   = (IF AVAIL bff-shipto THEN bff-shipto.ship-no ELSE 0) + 1
                    bf-shipto.broker    = IF iInterCompanyBilling EQ 1 AND lSoldUsed THEN YES ELSE NO .
            END.       
            
            FIND FIRST bf-soldto NO-LOCK
                 WHERE bf-soldto.company EQ cTransCompany
                 AND bf-soldto.cust-no EQ cCustomerValue
                 AND bf-soldto.sold-id EQ ipcSoldID NO-ERROR.
            IF NOT AVAIL bf-soldto then
            DO:     
            FIND LAST bff-soldto
                 WHERE bff-soldto.company EQ cTransCompany
                 AND bff-soldto.cust-no EQ cCustomerValue
                 USE-INDEX sold-no NO-LOCK NO-ERROR.
             
                CREATE bf-soldto .                
                ASSIGN
                    bf-soldto.company = cTransCompany
                    bf-soldto.cust-no = cCustomerValue                     
                    bf-soldto.sold-id = ipcSoldID
                    bf-soldto.sold-no = (IF AVAIL bff-soldto THEN bff-soldto.sold-no ELSE 0) + 1.
            END.              
        END.         
    END.   
    RELEASE bf-cust.
    RELEASE bf-shipto.
    RELEASE bff-shipto.
    RELEASE bff-soldto.
END PROCEDURE.

PROCEDURE pGetNk1Settings PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  ipcCompany        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  ipcCustomer       AS CHARACTER NO-UNDO.     
    DEFINE OUTPUT PARAMETER oplReturnCustomer AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opiInterCompany   AS INTEGER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplReturnCompany  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.      
      
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "InterCompanyBilling", "I" /* Logical */, YES /* check by cust */, 
        INPUT YES /* use cust not vendor */, ipcCustomer /* cust */, "" /* ship-to*/,
        OUTPUT cReturn, OUTPUT lRecFound).
    opiInterCompany = INTEGER(cReturn) NO-ERROR.
    
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "InterCompanyBilling", "L" /* Logical */, YES /* check by cust */, 
        INPUT YES /* use cust not vendor */, ipcCustomer /* cust */, "" /* ship-to*/,
        OUTPUT cReturn, OUTPUT lRecFound).
    oplReturnCustomer = LOGICAL(cReturn) NO-ERROR.
    
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "InterCompanyBilling", "C" /* Logical */, YES /* check by cust */, 
        INPUT YES /* use cust not vendor */, ipcCustomer /* cust */, "" /* ship-to*/,
        OUTPUT cReturn, OUTPUT lRecFound).
    ioplReturnCompany = IF cReturn NE "" THEN cReturn ELSE ioplReturnCompany.
    
END PROCEDURE.
