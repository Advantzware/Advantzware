
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
DEFINE VARIABLE hdSession AS HANDLE.
RUN system\session.p PERSISTENT SET hdSession.
SESSION:ADD-SUPER-PROCEDURE (hdSession).


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

RUN pBuildAndDisplay.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBuildAndDisplay PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Just process the invoices and export all related temp-tables for review
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cCompany   AS CHARACTER NO-UNDO INITIAL '001'.
    DEFINE VARIABLE iInvStart  AS INTEGER   NO-UNDO INITIAL 1.
    DEFINE VARIABLE iInvEnd    AS INTEGER   NO-UNDO INITIAL 9999999.
    DEFINE VARIABLE dtStart    AS DATE      NO-UNDO INITIAL 1/1/2018.
    DEFINE VARIABLE dtEnd      AS DATE      NO-UNDO INITIAL 12/31/2020.
    DEFINE VARIABLE cCustStart AS CHARACTER NO-UNDO INITIAL ''.
    DEFINE VARIABLE cCustEnd   AS CHARACTER NO-UNDO INITIAL 'ZZZZZZ'.
    DEFINE VARIABLE dtPost     AS DATE      NO-UNDO INITIAL TODAY.

    DEFINE VARIABLE lError     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iTimer     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCount     AS INTEGER   NO-UNDO.    
    
    DEFINE BUFFER bf-inv-line FOR inv-line.
    DEFINE BUFFER bf-inv-misc FOR inv-misc.
    DEFINE BUFFER bf-cust FOR cust.
    
/*    FIND FIRST cust NO-LOCK                                                                                                                                       */
/*        WHERE cust.company EQ cCompany                                                                                                                            */
/*        AND cust.inv-meth NE ?                                                                                                                                    */
/*        AND CAN-FIND(FIRST inv-head WHERE inv-head.company EQ cust.company AND inv-head.cust-no EQ cust.cust-no AND inv-head.inv-no NE 0 AND inv-head.stat NE 'H')*/
/*        NO-ERROR.                                                                                                                                                 */
/*    IF AVAILABLE cust THEN                                                                                                                                        */
/*        ASSIGN                                                                                                                                                    */
/*            cCustStart = cust.cust-no                                                                                                                             */
/*            cCustEnd   = cust.cust-no                                                                                                                             */
/*            .                                                                                                                                                     */
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
        DISPLAY inv-head.inv-no FORMAT ">>>>>>>9" inv-head.t-inv-rev inv-head.t-inv-freight inv-head.t-inv-tax.
    END.
    iTimer = TIME.    
    RUN oe/PostInvoices.p(cCompany,
        iInvStart, iInvEnd,
        dtStart, dtEnd,
        cCustStart, cCustEnd,
        dtPost,
        "Export",
        OUTPUT lError, OUTPUT cMessage).

    MESSAGE  "Processed " iCount " invoices" SKIP 
    "Completed in " TIME - iTimer " seconds"
    VIEW-AS ALERT-BOX.
END PROCEDURE.

