
/*------------------------------------------------------------------------
    File        : PostInvoicesTester.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : BV
    Created     : Mon May 04 14:13:12 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{custom/globdefs.i &NEW=NEW}
{sys/inc/var.i NEW SHARED}
DEFINE VARIABLE hdSession AS HANDLE.
DEFINE VARIABLE hdPostInvoices AS HANDLE.
RUN system\session.p PERSISTENT SET hdSession.
SESSION:ADD-SUPER-PROCEDURE (hdSession).
RUN oe\PostInvoices.p PERSISTENT SET hdPostInvoices.
SESSION:ADD-SUPER-PROCEDURE (hdPostInvoices).

DEFINE TEMP-TABLE ttARInv LIKE ar-inv
    USE-INDEX rec_key AS PRIMARY.
DEFINE TEMP-TABLE ttARInvl LIKE ar-invl
    USE-INDEX rec_key AS PRIMARY.
DEFINE TEMP-TABLE ttGLTrans LIKE gltrans
    USE-INDEX rec_key AS PRIMARY.

DEFINE VARIABLE cCompany            AS CHARACTER NO-UNDO INITIAL '001'.
DEFINE VARIABLE iInvStart           AS INTEGER   NO-UNDO INITIAL 1000054.
DEFINE VARIABLE iInvEnd             AS INTEGER   NO-UNDO INITIAL 1000060.
DEFINE VARIABLE dtStart             AS DATE      NO-UNDO INITIAL 1/1/2018.
DEFINE VARIABLE dtEnd               AS DATE      NO-UNDO INITIAL 12/31/2020.
DEFINE VARIABLE cCustStart          AS CHARACTER NO-UNDO INITIAL ''.
DEFINE VARIABLE cCustEnd            AS CHARACTER NO-UNDO INITIAL 'ZZZZZZ'.
DEFINE VARIABLE dtPost              AS DATE      NO-UNDO INITIAL TODAY.
DEFINE VARIABLE lPost               AS LOGICAL   NO-UNDO INITIAL YES.
DEFINE VARIABLE lRunLegacyFilesOnly AS LOGICAL   NO-UNDO INITIAL NO.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RUN oe/PostInvoices.p PERSISTENT SET hdPostInvoices.

IF lRunLegacyFilesOnly THEN 
    RUN pBuildCompareFiles("Standard").    
ELSE 
    RUN pBuildAndDisplay.



/* **********************  Internal Procedures  *********************** */

PROCEDURE pBuildAndDisplay PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Just process the invoices and export all related temp-tables for review
     Notes:
    ------------------------------------------------------------------------------*/
    

    DEFINE VARIABLE lError     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iTimer     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCount     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iProcessed AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iValid     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iPosted    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cOptions   AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-inv-line FOR inv-line.
    DEFINE BUFFER bf-inv-misc FOR inv-misc.
    DEFINE BUFFER bf-cust     FOR cust.
    
    cOptions = "Export".
    IF lPost THEN cOptions = cOptions + ",Post,ExportExceptions".
    
    FOR EACH inv-head NO-LOCK
        WHERE inv-head.company EQ cCompany
        AND inv-head.inv-no GE iInvStart
        AND inv-head.inv-no LE iInvEnd
        AND inv-head.inv-date GE dtStart
        AND inv-head.inv-date LE dtEnd
        AND inv-head.cust-no GE cCustStart
        AND inv-head.cust-no LE cCustEnd
        AND inv-head.printed
        AND inv-head.inv-no   GT 0
        AND (CAN-FIND(FIRST bf-inv-line WHERE bf-inv-line.r-no EQ inv-head.r-no)
        OR CAN-FIND(FIRST bf-inv-misc WHERE bf-inv-misc.r-no = inv-head.r-no )
        OR inv-head.multi-invoice)
        AND inv-head.stat     NE "H"
        USE-INDEX prnt,
        FIRST bf-cust NO-LOCK
        WHERE bf-cust.company EQ inv-head.company
        AND bf-cust.cust-no EQ inv-head.cust-no
        AND ((bf-cust.inv-meth EQ ? AND inv-head.multi-invoice) OR (bf-cust.inv-meth NE ? AND NOT inv-head.multi-invoice))  /*Filter multi-invoices correctly based on customer*/
        :
        iCount = iCount + 1.
//        DISPLAY inv-head.inv-no FORMAT ">>>>>>>9" inv-head.t-inv-rev inv-head.t-inv-freight inv-head.t-inv-tax.
    END.
    iTimer = TIME.    
    RUN PostInvoices(cCompany,
        iInvStart, iInvEnd,
        dtStart, dtEnd,
        cCustStart, cCustEnd,
        dtPost,
        cOptions,
        OUTPUT iProcessed, OUTPUT iValid, OUTPUT iPosted,
        OUTPUT lError, OUTPUT cMessage).

    MESSAGE  "Should be processed = " iCount SKIP
        "Processed = " iProcessed SKIP 
        "Valid = " iValid SKIP
        "Posted = " iPosted SKIP(2)
        "Error" lError cMessage SKIP(2) 
        "Completed in " TIME - iTimer " seconds"
        VIEW-AS ALERT-BOX.
     
    IF lPost THEN RUN pBuildCompareFiles("New").
        
END PROCEDURE.

PROCEDURE pBuildCompareFiles PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcResults AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hdOutput    AS HANDLE.
    DEFINE VARIABLE cTempFolder AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFile       AS CHARACTER NO-UNDO.
    
    RUN system\OutputProcs.p PERSISTENT SET hdOutput.
    THIS-PROCEDURE:ADD-SUPER-PROCEDURE (hdOutput).
    
    RUN FileSys_GetTempDirectory (OUTPUT cTempFolder).
    
    FOR EACH ar-inv NO-LOCK
        WHERE ar-inv.company EQ cCompany
        AND ar-inv.inv-no GE iInvStart
        AND ar-inv.inv-no LE iInvEnd:
        CREATE ttArInv.
        BUFFER-COPY ar-inv TO ttArInv.
        
        FOR EACH ar-invl NO-LOCK 
            WHERE ar-invl.x-no EQ ar-inv.x-no:
            CREATE ttArInvl.
            BUFFER-COPY ar-invl TO ttArInvl.
        END.    
    END.
    FOR EACH gltrans NO-LOCK
        WHERE gltrans.company EQ '001'
        AND gltrans.trnum EQ 60193:
        CREATE ttGlTrans.
        BUFFER-COPY gltrans TO ttGlTrans.
    END.
    
    RUN Output_TempTableToCSV(TEMP-TABLE ttArInv:HANDLE, cTempFolder + "\" + ipcResults + "ARInv.csv", YES).
    RUN Output_TempTableToCSV(TEMP-TABLE ttArInvl:HANDLE, cTempFolder + "\" + ipcResults + "ARInvl.csv", YES).
    RUN Output_TempTableToCSV(TEMP-TABLE ttGlTrans:HANDLE, cTempFolder + "\" + ipcResults + "GlTrans.csv", YES).
          

END PROCEDURE.

