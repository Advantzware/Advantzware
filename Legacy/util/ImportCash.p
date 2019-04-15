
/*------------------------------------------------------------------------
    File        : ImportCash.p
    Purpose     : 

    Syntax      :

    Description : Import Program (Persistent) for Configuring and Processing the Import for Cash Receipts	

    Author(s)   : BV
    Created     : Thu Dec 7 16:18:38 EST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}

DEFINE TEMP-TABLE ttImportCash
    FIELD Company         AS CHARACTER FORMAT "x(3)"
    FIELD Location        AS CHARACTER FORMAT "x(5)"
    FIELD CustomerID      AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Customer ID" HELP "Required - Must be valid - Size:10"
    FIELD CheckNo         AS INTEGER   FORMAT "9999999999" COLUMN-LABEL "Check #" HELP "Required - Integer"
    FIELD BankCode        AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Bank" HELP "Defaults based on AR Control File Account - Size:3"
    FIELD CheckDate       AS DATE      FORMAT "99/99/9999" COLUMN-LABEL "Check Date" HELP "Defaults to Today - Date"
    FIELD CheckAmount     AS DECIMAL   FORMAT ">>,>>>,>>9.99" COLUMN-LABEL "Check Amount" HELP "Required non 0 - Decimal"
    FIELD InvoiceNo       AS INTEGER   FORMAT "999999" COLUMN-LABEL "Invoice #" HELP "Optional - Blank/0 will be place 'On Account'"
    FIELD InvoiceDiscount AS DECIMAL   FORMAT ">>9.99" COLUMN-LABEL "Discount" HELP "Optional - Decimal"
    FIELD InvoiceApplied  AS DECIMAL   FORMAT ">>,>>>,>>9.99" COLUMN-LABEL "Applied Amount" HELP "Defaults to Check Amount + Discount - Must not be greater than Check Amount - Decimal"
    FIELD AccountNumber   AS CHARACTER FORMAT "x(30)" COLUMN-LABEL "GL Account" HELP "Defaults from Bank or AR Control File - Size:15"
    .

DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 1 if there is a Company field in temp-table since this will not be part of the import data*/


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
{util/ImportProcs.i &ImportTempTable = "ttImportCash"}

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportCash FOR ttImportCash.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdValidator AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportCash FOR ttImportCash.

    RUN util/Validate.p PERSISTENT SET hdValidator.
    
    oplValid = YES.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCash.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCash.CustomerID EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Customer ID".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportCash.CheckNo EQ 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Check#".
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST cust NO-LOCK 
            WHERE cust.company EQ ipbf-ttImportCash.Company
            AND cust.cust-no EQ ipbf-ttImportCash.CustomerID
            NO-ERROR. 
        IF NOT AVAILABLE cust THEN 
            ASSIGN 
                oplValid = NO 
                opcNote  = "Key Field Invalid: CustomerID"
                .
    END.
    IF oplValid AND ipbf-ttImportCash.InvoiceNo NE 0 THEN 
    DO:
        FIND FIRST ar-inv NO-LOCK 
            WHERE ar-inv.company EQ ipbf-ttImportCash.Company 
            AND ar-inv.posted
            AND ar-inv.cust-no EQ ipbf-ttImportCash.CustomerID
            AND ar-inv.inv-no EQ ipbf-ttImportCash.InvoiceNo
            NO-ERROR.
        IF NOT AVAILABLE ar-inv THEN 
            ASSIGN 
                oplValid = NO 
                opcNote  = "Key Field Invalid: Invoice"
                .
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST bf-ttImportCash NO-LOCK 
            WHERE bf-ttImportCash.Company EQ ipbf-ttImportCash.Company
            AND bf-ttImportCash.CustomerID EQ ipbf-ttImportCash.CustomerID
            AND bf-ttImportCash.CheckNo EQ ipbf-ttImportCash.CheckNo
            AND bf-ttImportCash.InvoiceNo EQ ipbf-ttImportCash.InvoiceNo 
            AND ROWID(bf-ttImportCash) NE ROWID(ipbf-ttImportCash)
            NO-ERROR.
        IF AVAILABLE bf-ttImportCash THEN 
            ASSIGN 
                oplValid = NO 
                opcNote  = "Duplicate Record in Import File"
                .
    END.
    IF oplValid THEN 
    DO:
        FIND FIRST ar-cash NO-LOCK 
            WHERE ar-cash.company EQ ipbf-ttImportCash.Company
            AND ar-cash.cust-no EQ ipbf-ttImportCash.CustomerID
            AND ar-cash.check-no EQ ipbf-ttImportCash.CheckNo
            NO-ERROR .
        IF AVAILABLE ar-cash THEN 
            FIND FIRST ar-cashl NO-LOCK 
                WHERE ar-cashl.company EQ ar-cash.company
                AND ar-cashl.inv-no EQ ipbf-ttImportCash.InvoiceNo
                NO-ERROR.
        IF AVAILABLE ar-cash AND AVAILABLE ar-cashl THEN 
        DO:
            IF NOT iplUpdateDuplicates THEN 
                ASSIGN 
                    oplValid = NO
                    opcNote  = "Duplicate Exists:  Will be skipped"
                    .
            ELSE
                ASSIGN 
                    opcNote = "Update record - All fields to be overwritten"
                    .        
        END.
        ELSE IF AVAILABLE ar-cash AND NOT AVAILABLE ar-cashl THEN 
                ASSIGN 
                    opcNote = "Add record - New Line to Existing Check"
                    .
            ELSE 
                ASSIGN 
                    opcNote = "Add record"
                    .
        
    END.
    IF oplValid AND iplFieldValidation THEN 
    DO:
        IF oplValid AND ipbf-ttImportCash.BankCode NE "" THEN 
        DO:
            FIND FIRST bank NO-LOCK  
                WHERE bank.company EQ ipbf-ttImportCash.Company
                AND bank.bank-code EQ ipbf-ttImportCash.BankCode 
                NO-ERROR.
            IF NOT AVAILABLE bank THEN 
                ASSIGN 
                    oplValid = NO 
                    opcNote  = "Invalid Bank"
                    .
                    
        END.
        IF oplValid AND ipbf-ttImportCash.AccountNumber NE "" THEN 
        DO:
            FIND FIRST account NO-LOCK  
                WHERE account.company EQ ipbf-ttImportCash.Company
                AND account.actnum EQ ipbf-ttImportCash.AccountNumber
                AND account.type NE 'T'
                NO-ERROR.
            IF NOT AVAILABLE account THEN 
                ASSIGN 
                    oplValid = NO 
                    opcNote  = "Invalid Account"
                    .
                    
        END.
        IF oplValid AND ipbf-ttImportCash.CheckAmount EQ 0 THEN 
        DO:
            ASSIGN 
                oplValid = NO 
                opcNote  = "Check Amount is 0"
                .
                
        END.
        IF oplValid AND ipbf-ttImportCash.InvoiceApplied GT ipbf-ttImportCash.CheckAmount THEN 
        DO:
            ASSIGN 
                oplValid = NO 
                opcNote  = "Applied > Check"
                .
                
        END.
    END.
    
END PROCEDURE.

PROCEDURE pCreateNewCashHeader:
    /*------------------------------------------------------------------------------
     Purpose: Creates a new AR Cash Header, setting defaults based on key values
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER.
    DEFINE INPUT PARAMETER ipcCustomer AS CHARACTER.
    DEFINE INPUT PARAMETER ipiCheck AS INTEGER.
    DEFINE OUTPUT PARAMETER opriCash AS ROWID.
    
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
        ar-cash.check-date   = TODAY
        ar-cash.curr-code[1] = IF AVAIL company THEN company.curr-code ELSE ""
        ar-cash.ex-rate      = IF AVAIL currency THEN currency.ex-rate ELSE 0
        ar-cash.bank-code    = IF AVAILABLE bank THEN bank.bank-code ELSE "" 
        ar-cash.cust-no      = ipcCustomer
        ar-cash.check-no     = ipiCheck
        .
        
    opriCash = ROWID(ar-cash).
    RELEASE ar-cash.


END PROCEDURE.

PROCEDURE pCreateNewCashLine:
    /*------------------------------------------------------------------------------
        Purpose: Creates a new AP invoice line, setting defaults based on key values
        Notes:
       ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriCash AS ROWID.
    DEFINE INPUT PARAMETER ipiInvoice AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opriCashl AS ROWID.
    
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
        ar-cashl.check-no = STRING(ar-cash.check-no,"9999999999")
        ar-cashl.inv-date = TODAY
        .
    
    /*Get Account Number*/    
    FIND FIRST bank NO-LOCK 
        WHERE bank.company EQ ar-cash.company 
        AND bank.bank-code EQ ar-cash.bank-code
        NO-ERROR.
    IF AVAILABLE bank THEN 
    DO:
        FIND FIRST account NO-LOCK 
            WHERE account.company EQ ar-cash.company 
            AND account.actnum  EQ bank.actnum 
            NO-ERROR.
        IF AVAILABLE account THEN 
            cAccount = bank.actnum.
    END.
    ELSE 
    DO:
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
        ar-cashl.actnum = cAccount.
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


PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportCash FOR ttImportCash.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.

    DEFINE VARIABLE riCash  AS ROWID. 
    DEFINE VARIABLE riCashl AS ROWID. 
    

                
    iopiAdded = iopiAdded + 1.
        
    /*if found, add another line to existing header - otherwise, create a new header*/
    FIND FIRST ar-cash NO-LOCK
        WHERE ar-cash.company EQ ipbf-ttImportCash.Company
        AND ar-cash.check-date EQ ipbf-ttImportCash.CheckDate
        AND ar-cash.check-no EQ ipbf-ttImportCash.CheckNo
        AND ar-cash.cust-no EQ ipbf-ttImportCash.CustomerID
        NO-ERROR.
    IF NOT AVAILABLE ar-cash THEN /*create a new one*/
    DO:
        RUN pCreateNewCashHeader (ipbf-ttImportCash.Company, ipbf-ttImportCash.CustomerID, ipbf-ttImportCash.CheckNo, OUTPUT riCash).
        FIND ar-cash EXCLUSIVE-LOCK
            WHERE ROWID(ar-cash) EQ riCash
            NO-ERROR.
        IF NOT AVAILABLE ar-cash THEN NEXT.    
                    
        /*Override defaults with imported values for header*/
        IF ipbf-ttImportCash.CheckDate NE ? THEN 
            ar-cash.check-date =  ipbf-ttImportCash.CheckDate.
        IF ipbf-ttImportCash.BankCode NE "" THEN 
            ar-cash.bank-code = ipbf-ttImportCash.BankCode. 
            
        ar-cash.check-amt = ipbf-ttImportCash.CheckAmount.
            
    END. /*not available ar-cash*/
    RUN pCreateNewCashLine (ROWID(ar-cash), ipbf-ttImportCash.InvoiceNo, OUTPUT riCashl).
    FIND ar-cashl EXCLUSIVE-LOCK 
        WHERE ROWID(ar-cashl) EQ riCashl
        NO-ERROR.
    IF NOT AVAILABLE ar-cashl THEN NEXT.
                
    /*Override defaults with imported values for line*/ 
    ASSIGN 
        ar-cashl.amt-disc = ipbf-ttImportCash.InvoiceDiscount
        .       
        
    IF ipbf-ttImportCash.InvoiceApplied EQ 0 THEN 
        ar-cashl.amt-paid = ipbf-ttImportCash.CheckAmount + ar-cashl.amt-disc.
    ELSE 
        ar-cashl.amt-paid = ipbf-ttImportCash.InvoiceApplied.
        

    IF ipbf-ttImportCash.AccountNumber NE "" THEN 
        ar-cashl.actnum = ipbf-ttImportCash.AccountNumber.
                                                            

END PROCEDURE.

