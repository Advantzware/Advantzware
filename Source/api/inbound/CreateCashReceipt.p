
/*------------------------------------------------------------------------
    File        : CreateCashReceipt.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : DEVA$!
    Created     : Wed Dec 22 21:14:02 IST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

{ar/ttARCash.i}
{ar/ttARCashLine.i}
    
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttARCash.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttARCashLine.
DEFINE OUTPUT       PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
DEFINE OUTPUT       PARAMETER opcMessage AS CHARACTER NO-UNDO.

DEFINE VARIABLE cImportType            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cImportTypeImporter    AS CHARACTER NO-UNDO INITIAL "Importer".
DEFINE VARIABLE cImportTypeEDI         AS CHARACTER NO-UNDO INITIAL "EDI".

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/* Input pre-processing */
RUN pPreProcessInputs (
    OUTPUT oplSuccess,
    OUTPUT opcMessage
    ).

    /* Input validation */
IF oplSuccess THEN
    RUN pValidateInputs (
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.

/* Input processing */
IF oplSuccess THEN
    RUN pProcessInputs (
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.
        
/* **********************  Internal Procedures  *********************** */


PROCEDURE pPreProcessEDI PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttARCash FOR ttARCash.
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO INITIAL TRUE.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-cust FOR cust.
    DEFINE BUFFER bf-bank FOR bank.
    
    IF ipbf-ttARCash.payorID NE "" THEN DO:
        FIND FIRST bf-cust NO-LOCK
             WHERE bf-cust.cust-no EQ ipbf-ttARCash.payorID
             NO-ERROR.
        IF AVAILABLE bf-cust THEN
            ASSIGN
                ipbf-ttARCash.company    = bf-cust.company
                ipbf-ttARCash.customerID = bf-cust.cust-no
                . 
    END.

    /* Hardcoding bank code 1 for now */
    FIND FIRST bf-bank NO-LOCK
         WHERE bf-bank.company   EQ ipbf-ttARCash.company
           AND bf-bank.bank-code EQ "1"
         NO-ERROR. 
    IF NOT AVAILABLE bf-bank THEN
        FIND FIRST bf-bank NO-LOCK
             WHERE bf-bank.company EQ ipbf-ttARCash.company
             NO-ERROR. 
    
    IF AVAILABLE bf-bank THEN DO:
        ipbf-ttARCash.bankCode = bf-bank.bank-code.
        FOR EACH ttARCashLine
            WHERE ttARCashLine.sequenceID EQ ipbf-ttARCash.sequenceID:
            ttARCashLine.accountNumber = bf-bank.actnum.
        END.
    END.
END PROCEDURE.

PROCEDURE pValidateInputs PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Procedure to validate inputs
 Notes:
------------------------------------------------------------------------------*/     
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO INITIAL TRUE.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    FOR EACH ttARCash:
        IF ttARCash.company EQ "" OR
            NOT CAN-FIND(FIRST company NO-LOCK
                         WHERE company.company EQ ttARCash.company) THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Invalid company '" + ttARCash.company + "'"
                .

            RETURN.
        END.

        IF ttARCash.customerID EQ "" OR
            NOT CAN-FIND(FIRST cust NO-LOCK
                         WHERE cust.company EQ ttARCash.company
                           AND cust.cust-no EQ ttARCash.customerID) THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Invalid customer '" + ttARCash.customerID + "'"
                .

            RETURN.
        END.

        IF ttARCash.bankCode EQ "" OR
            NOT CAN-FIND(FIRST bank NO-LOCK
                         WHERE bank.company   EQ ttARCash.company
                           AND bank.bank-code EQ ttARCash.bankCode) THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Invalid bank '" + ttARCash.bankCode + "'"
                .

            RETURN.
        END.

        FOR EACH ttARCashLine
            WHERE ttARCashLine.sequenceID EQ ttARCash.sequenceID:
            IF ttARCashLine.invoiceID EQ "" OR
                NOT CAN-FIND(FIRST ar-inv NO-LOCK
                             WHERE ar-inv.company EQ ttARCash.company
                               AND ar-inv.inv-no  EQ INTEGER(ttARCashLine.invoiceID)) THEN DO:
                ASSIGN
                    oplSuccess = FALSE
                    opcMessage = "Invalid invoice '" + ttARCashLine.invoiceID + "'"
                    .

                RETURN.
            END.
            
            IF ttARCashLine.accountNumber EQ "" OR
                NOT CAN-FIND (FIRST account NO-LOCK  
                              WHERE account.company EQ ttARCash.company
                                AND account.actnum  EQ ttARCashLine.accountNumber
                                AND account.type    NE 'T') THEN DO:                
                ASSIGN
                    oplSuccess = FALSE
                    opcMessage = "Invalid account number '" + ttARCashLine.accountNumber + "'"
                    .

                RETURN.       
            END.
        END.
    END.
END PROCEDURE.

PROCEDURE pProcessInputs PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Process the inputs to get the result
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO INITIAL TRUE.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE riARCash  AS ROWID NO-UNDO.
    DEFINE VARIABLE riARCashl AS ROWID NO-UNDO.
    
    MAIN-LOOP:         
    DO TRANSACTION ON ERROR UNDO, LEAVE:
        FOR EACH ttARCash:
            RUN pCreateNewCashHeader (
                INPUT  ttARCash.company,
                INPUT  ttARCash.customerID,
                INPUT  ttARCash.checkNumber,
                INPUT  ttARCash.checkDate,
                INPUT  ttARCash.checkAmount,
                OUTPUT riARCash                
                ) NO-ERROR.
            IF riARCash NE ? THEN DO:
                FOR EACH ttARCashLine
                    WHERE ttARCashLine.sequenceID EQ ttARCash.sequenceID:
                    RUN pCreateNewCashLine (
                        INPUT  riARCash,
                        INPUT  ttARCashLine.checkGrossAmount,
                        INPUT  ttARCashLine.discountAmount,
                        INPUT  INTEGER(ttARCashLine.invoiceID),
                        OUTPUT riARCashl
                        ) NO-ERROR.
                    IF riARCashl EQ ? THEN DO:
                        ASSIGN
                            oplSuccess = FALSE
                            opcMessage = "Error creating cash receipt line"
                            .
                        
                        UNDO MAIN-LOOP, LEAVE MAIN-LOOP. 
                    END.
                END.
            END.
            ELSE DO:
                ASSIGN
                    oplSuccess = FALSE
                    opcMessage = "Error creating cash receipt"
                    .
                
                UNDO MAIN-LOOP, LEAVE MAIN-LOOP. 
            END.
        END.
    END.
END PROCEDURE.

PROCEDURE pPreProcessInputs PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Pre-processes the inputs
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO INITIAL TRUE.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    FOR EACH ttARCash:
        IF ttARCash.importType EQ cImportTypeEDI THEN DO:
            RUN pPreProcessEDI (
                BUFFER ttARCash,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ).
            IF NOT oplSuccess THEN
                RETURN.
        END.
    END.        
END PROCEDURE.

PROCEDURE pCreateNewCashHeader:
    /*------------------------------------------------------------------------------
     Purpose: Creates a new AR Cash Header, setting defaults based on key values
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustomer    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiCheck       AS INT64     NO-UNDO.
    DEFINE INPUT  PARAMETER ipdtCheckDate  AS DATE      NO-UNDO.
    DEFINE INPUT  PARAMETER ipdCheckAmount AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opriCash       AS ROWID     NO-UNDO.
    
    DEFINE VARIABLE iNextCNo AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-ar-cash FOR ar-cash.
    
    FIND FIRST ar-ctrl NO-LOCK  
         WHERE ar-ctrl.company EQ ipcCompany
         NO-ERROR.
    IF AVAILABLE ar-ctrl THEN 
        FIND FIRST bank NO-LOCK 
             WHERE bank.company EQ ipcCompany 
               AND bank.actnum EQ ar-ctrl.cash-act 
             NO-ERROR.
    FIND FIRST company NO-LOCK 
        WHERE company.company EQ ipcCompany
        NO-ERROR.
    IF AVAILABLE company THEN 
        FIND FIRST currency NO-LOCK 
             WHERE currency.company EQ ipcCompany
               AND currency.c-code EQ company.curr-code
             NO-ERROR.

    FIND LAST bf-ar-cash USE-INDEX c-no NO-LOCK NO-ERROR.
    iNextCNo = IF AVAIL bf-ar-cash THEN bf-ar-cash.c-no + 1 ELSE 1.  
    
    CREATE ar-cash.
    ASSIGN
        ar-cash.company      = ipcCompany
        ar-cash.c-no         = iNextCNo
        ar-cash.check-date   = IF ipdtCheckDate EQ ? THEN TODAY ELSE ipdtCheckDate
        ar-cash.curr-code[1] = IF AVAILABLE company THEN company.curr-code ELSE ""
        ar-cash.ex-rate      = IF AVAILABLE currency THEN currency.ex-rate ELSE 0
        ar-cash.bank-code    = IF AVAILABLE bank THEN bank.bank-code ELSE "" 
        ar-cash.cust-no      = ipcCustomer
        ar-cash.check-no     = ipiCheck
        ar-cash.check-amt    = ipdCheckAmount
        .
        
    opriCash = ROWID(ar-cash).
    RELEASE ar-cash.
END PROCEDURE.

PROCEDURE pCreateNewCashLine:
    /*------------------------------------------------------------------------------
        Purpose: Creates a new AP invoice line, setting defaults based on key values
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriCash          AS ROWID   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdAmountPaid     AS DECIMAL NO-UNDO.
    DEFINE INPUT  PARAMETER ipdAmountDiscount AS DECIMAL NO-UNDO.
    DEFINE INPUT  PARAMETER ipiInvoice        AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opriCashl         AS ROWID   NO-UNDO.
    
    DEFINE VARIABLE iNextLine AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cAccount  AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ar-cashl FOR ar-cashl.
    
    FIND ar-cash NO-LOCK 
        WHERE ROWID(ar-cash) EQ ipriCash
        NO-ERROR.
    IF NOT AVAILABLE ar-cash THEN RETURN.
    
    FIND FIRST ar-ctrl NO-LOCK
        WHERE ar-ctrl.company EQ ar-cash.company
        NO-ERROR.

    iNextLine = 1.
    FOR EACH bf-ar-cashl OF ar-cash NO-LOCK BY LINE DESCENDING:
        iNextLine = bf-ar-cashl.line + 1.
        LEAVE.
    END.
                    
    CREATE ar-cashl.
    ASSIGN
        ar-cashl.company  = ar-cash.company
        ar-cashl.c-no     = ar-cash.c-no
        ar-cashl.line     = iNextLine
        ar-cashl.cust-no  = ar-cash.cust-no
        ar-cashl.check-no = STRING(ar-cash.check-no,"999999999999")
        ar-cashl.inv-date = TODAY
        .
    
    /*Get Account Number*/    
    FIND FIRST bank NO-LOCK 
         WHERE bank.company   EQ ar-cash.company 
           AND bank.bank-code EQ ar-cash.bank-code
         NO-ERROR.
    IF AVAILABLE bank THEN DO:
        FIND FIRST account NO-LOCK 
             WHERE account.company EQ ar-cash.company 
               AND account.actnum  EQ bank.actnum 
             NO-ERROR.
        IF AVAILABLE account THEN 
            cAccount = bank.actnum.
    END.
    ELSE DO:
        IF ar-cash.check-no GE 90000000 AND ar-cash.check-no LE 99999999 THEN 
            FIND FIRST account NO-LOCK  
                 WHERE account.company EQ ar-cash.company 
                   AND account.actnum  EQ ar-ctrl.sales 
                 NO-ERROR.
        ELSE 
            FIND FIRST account NO-LOCK 
                 WHERE account.company EQ ar-cash.company 
                   AND account.actnum  EQ ar-ctrl.cash-act 
                 NO-ERROR.
        IF AVAILABLE account THEN 
            ASSIGN cAccount = account.actnum.
    END.
    ASSIGN 
        ar-cashl.actnum   = cAccount
        ar-cashl.amt-paid = ipdAmountPaid
        ar-cashl.amt-disc = ipdAmountDiscount
        .
    /*End Get Account Number*/

    /*Get Invoice Information*/
    FIND FIRST ar-inv NO-LOCK 
         WHERE ar-inv.company EQ ar-cash.company
           AND ar-inv.inv-no EQ ipiInvoice
           AND ar-inv.posted
         NO-ERROR.
    IF AVAILABLE ar-inv THEN 
        ASSIGN 
            ar-cashl.inv-no   = ar-inv.inv-no
            ar-cashl.amt-due  = ar-inv.due
            ar-cashl.inv-date = ar-inv.inv-date
            .
               
    opriCashl = ROWID(ar-cashl).
    RELEASE ar-cashl.
  
END PROCEDURE.
