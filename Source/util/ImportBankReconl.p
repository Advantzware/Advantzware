/*------------------------------------------------------------------------
    File        : ImportBankReconl.p
    Purpose     : 
    Syntax      :
    Description : Import Program (Persistent) for Configuring and Processing the Import for Customer	
    Author(s)   : 
    Created     : Fri Jun 21:18:38 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}
/*Refactor - required for old external procedures*/ 

DEFINE TEMP-TABLE ttImportBankReconl
    FIELD Company       AS CHARACTER 
    FIELD Location      AS CHARACTER 
    FIELD CheckJrl      AS CHARACTER FORMAT "X(10)" COLUMN-LABEL "Check/Journal#" HELP "Required - Size:10" 
    FIELD TransDate     AS DATE FORMAT "99/99/9999" COLUMN-LABEL "Trans Date" HELP "Required - Size:Date" 
    FIELD Amount        AS DECIMAL FORMAT ">>>>>>>>9.99" COLUMN-LABEL "Amount" HELP "Required - Size:decimal"
    FIELD BankCode      AS CHARACTER FORMAT "X(10)" COLUMN-LABEL "Bank Code" HELP "Required - Size:10"
    FIELD Vendor        AS CHARACTER FORMAT "X(10)" COLUMN-LABEL "Vendor" HELP "Required - Size:10"
    FIELD VendorName    AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Vendor Name" HELP "Optional - Size:30" 
    FIELD Cleared       AS CHARACTER FORMAT "X(10)" COLUMN-LABEL "Cleared" HELP  "Required - Yes or N0"
    
    .

{ap/reconcil.i NEW}

DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2. /*Set to 1 if there is a Company field in temp-table since this will not be part of the mport data*/
DEFINE VARIABLE glInitialized AS LOGICAL.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
{util/ImportProcs.i &ImportTempTable = "ttImportBankReconl"}

PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportBankReconl FOR ttImportBankReconl.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-ttImportBankReconl FOR ttImportBankReconl.
    
    oplValid = YES.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportBankReconl.Company EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Company".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportBankReconl.CheckJrl EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Check/Journal#".
    END.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportBankReconl.TransDate EQ ? THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Trans Date".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportBankReconl.Amount EQ 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Amount".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportBankReconl.BankCode EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Bank Code.".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportBankReconl.Cleared EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Key Field Blank: Cleared.".
    END.
   
    IF oplValid THEN 
        ASSIGN 
        opcNote = "Update record - Cleared fields to be overwritten" .
    
END PROCEDURE.

PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportBankReconl FOR ttImportBankReconl.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
   
    IF NOT glInitialized THEN DO: 
        RUN ap/reconcil.p.
        glInitialized = YES.
    END.
   
    FIND FIRST reconcile WHERE 
        reconcile.tt-number = STRING(INTEGER(ipbf-ttImportBankReconl.CheckJrl),"999999") AND 
        reconcile.tt-amt = DEC(ipbf-ttImportBankReconl.Amount)  
        NO-ERROR.
    IF AVAIL reconcile THEN DO:
        IF ipbf-ttImportBankReconl.Cleared EQ "Yes" THEN reconcile.tt-cleared = YES.
        RUN reconcile-file( BUFFER reconcile).
    END.
   
    RELEASE ap-pay.
    RELEASE reconcile.
    
END PROCEDURE.

PROCEDURE reconcile-file:
    /*------------------------------------------------------------------------------
     Purpose: 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-reconcile FOR reconcile.
    DEF BUFFER bf-ap-pay FOR ap-pay.
    DEFINE VARIABLE lv-bank LIKE ar-cash.bank-code NO-UNDO.

   CASE ipbf-reconcile.tt-type:
       WHEN 1  THEN DO:
           FIND ap-pay WHERE ROWID(ap-pay) EQ ipbf-reconcile.tt-rowid NO-LOCK NO-ERROR.
           IF AVAIL ap-pay THEN DO:
               IF ap-pay.d-no NE 0 
               AND NOT CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no) THEN 
                   FIND FIRST bf-ap-pay EXCLUSIVE-LOCK
                       WHERE bf-ap-pay.company   EQ ap-pay.company
                       AND bf-ap-pay.check-act EQ ap-pay.check-act
                       AND bf-ap-pay.check-no  EQ ap-pay.d-no  NO-ERROR.
               ELSE 
                   FIND bf-ap-pay EXCLUSIVE-LOCK 
                       WHERE ROWID(bf-ap-pay) EQ ROWID(ap-pay) NO-ERROR.

               IF AVAIL bf-ap-pay THEN DO:
                   bf-ap-pay.cleared = ipbf-reconcile.tt-cleared.
                   FOR EACH ap-pay EXCLUSIVE-LOCK
                       WHERE ap-pay.company EQ bf-ap-pay.company
                       AND ap-pay.d-no    EQ bf-ap-pay.check-no
                       AND NOT CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no)             
                       USE-INDEX d-no:
                       ASSIGN ap-pay.cleared = bf-ap-pay.cleared.
                   END.
               END.
           END.

       END. /*tt-type eq 1*/     

       WHEN  2 THEN DO:
         FIND ar-cash WHERE ROWID(ar-cash) EQ ipbf-reconcile.tt-rowid NO-LOCK NO-ERROR.
         IF AVAILABLE ar-cash THEN 
             DO:
             lv-bank = ar-cash.bank-code.
             RELEASE ar-cash.
             FOR EACH tt-cash
                 WHERE tt-trnum EQ INT(SUBSTR(ipbf-reconcile.tt-number,4,10))
                 USE-INDEX tt-trnum,
                 FIRST ar-cash EXCLUSIVE-LOCK
                 WHERE ROWID(ar-cash)     EQ tt-cash.row-id
                 AND ar-cash.reconciled EQ NO
                 AND ar-cash.posted     EQ YES
                 AND ar-cash.memo       EQ NO
                 AND ar-cash.bank-code  EQ lv-bank,
                 FIRST bank NO-LOCK
                 WHERE bank.company   EQ ar-cash.company
                 AND bank.bank-code EQ ar-cash.bank-code
                 USE-INDEX bank
                 /*TRANSACTION*/ :
                 ar-cash.cleared = ipbf-reconcile.tt-cleared.
             END.
         END.
       END.
       WHEN 3 THEN  DO /*TRANSACTION*/ :
           FIND gl-jrn WHERE ROWID(gl-jrn) EQ ipbf-reconcile.tt-rowid EXCLUSIVE-LOCK NO-ERROR.
           IF AVAILABLE gl-jrn THEN gl-jrn.cleared = ipbf-reconcile.tt-cleared.
       END.
       WHEN 4 THEN DO:
           FOR EACH ar-mcash NO-LOCK WHERE ROWID(ar-mcash) EQ ipbf-reconcile.tt-rowid,
               FIRST ar-mcash-ref
               WHERE ar-mcash-ref.rec_key  EQ ar-mcash.rec_key
               AND ar-mcash-ref.reftable EQ "ar-mcash-ref"
               AND ar-mcash-ref.company  EQ "ar-mcash"
               USE-INDEX rec_key
               /*TRANSACTION*/ EXCLUSIVE-LOCK:
               ar-mcash-ref.val[2] = INT(ipbf-reconcile.tt-cleared).
           END.
       END.
   END CASE.

   RELEASE ar-cash .
   RELEASE ar-mcash-ref .

END PROCEDURE.
