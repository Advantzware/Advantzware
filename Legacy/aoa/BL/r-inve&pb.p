/* r-inve&pb.p */

/* To Do ***/
/* 1) UI to verify period for date given */
/* 2) PUt statements for misc */
/* 3) UI to verify there is something to post */
/* 4)  GL Summary  report */ 
/* 5) Prompt for close of orders, auto follow nk1 */
/* Note: inv-list-post runs twice. Once for report and once to post. */

/* ***************************  Definitions  ***************************/

/* Invoice Post Update GL.rpa */
{aoa/tempTable/ttInvoicePostUpdateGL.i}

{sys/ref/CustList.i NEW}
    
/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttInvoicePostUpdateGL.
{aoa/includes/pInvoicePostUpdateGL.i}

/* Local Variable Definitions ---                                       */
{custom/gcompany.i}
{custom/gloc.i}
{sys/inc/var.i NEW SHARED}
{methods/defines/globdefs.i &NEW=NEW}
{methods/defines/hndldefs.i &NEW="NEW"}
/* Shared Vars needed for 810 invoices */
RUN rc/genrcvar.p.
DEFINE VARIABLE cListName   AS cha       NO-UNDO.
DEFINE VARIABLE cInitDir    AS CHA       NO-UNDO.
DEFINE VARIABLE cCompCurr   AS cha       NO-UNDO.
DEFINE VARIABLE oeprep-char AS CHARACTER NO-UNDO.
DEFINE VARIABLE dProfit     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE hNotesProcs AS HANDLE NO-UNDO.
RUN "sys/NotesProcs.p" PERSISTENT SET hNotesProcs.

ASSIGN
    gcompany = ipcCompany
    cocode   = gcompany
    g_company = ipcCompany
   
    .
    
FIND FIRST company NO-LOCK WHERE company.company EQ cocode  NO-ERROR.
IF AVAILABLE company THEN cCompCurr = company.curr-code.
    
DEFINE NEW SHARED BUFFER xoe-relh    FOR oe-relh.
DEFINE NEW SHARED BUFFER yoe-relh    FOR oe-relh.
DEFINE NEW SHARED BUFFER xoe-rell    FOR oe-rell.
DEFINE NEW SHARED BUFFER inv-line    FOR inv-line.

DEFINE            BUFFER xinv-line   FOR inv-line.
DEFINE            BUFFER tmp-oe-boll FOR oe-boll.
DEFINE            BUFFER xoe-ord     FOR oe-ord.
DEFINE            BUFFER b-oe-ordl   FOR oe-ordl.

DEFINE NEW SHARED VARIABLE v-ar-acct         LIKE ar-ctrl.receivables.
DEFINE NEW SHARED VARIABLE v-ar-freight      LIKE ar-ctrl.freight.
DEFINE NEW SHARED VARIABLE v-ar-stax         LIKE ar-ctrl.stax.
DEFINE NEW SHARED VARIABLE v-ar-sales        LIKE ar-ctrl.sales.
DEFINE NEW SHARED VARIABLE v-ar-disc         LIKE ar-ctrl.discount.
DEFINE NEW SHARED VARIABLE v-return          AS LOG       INIT NO.
DEFINE NEW SHARED VARIABLE v-start2-compress AS CHARACTER.
DEFINE NEW SHARED VARIABLE v-end2-compress   AS CHARACTER.
DEFINE NEW SHARED VARIABLE v-post            AS LOG       INIT NO.
/* from gl-ctrl record */
DEFINE NEW SHARED VARIABLE v-trnum           AS INTEGER.
DEFINE NEW SHARED VARIABLE v-back            LIKE itemfg.q-back.
DEFINE NEW SHARED VARIABLE v-balance         AS DECIMAL   FORMAT ">>>,>>>,>>9.99cr".
DEFINE NEW SHARED VARIABLE v-reduce-ord-bal  LIKE cust.ord-bal NO-UNDO.
DEFINE NEW SHARED VARIABLE v-invline         AS RECID.
DEFINE NEW SHARED VARIABLE v-invhead         AS RECID.

DEFINE NEW SHARED VARIABLE v-detail          AS LOG       FORMAT "Detail/Summary" INIT NO NO-UNDO.
DEFINE NEW SHARED VARIABLE v-gldetail        AS LOG       FORMAT "Detail/Summary" INIT NO NO-UNDO.

DEFINE            VARIABLE lIsFreightTax     AS LOG       INIT NO.
DEFINE            VARIABLE lPostable         AS LOG       INIT NO.

/* Used in include files which are used elsewhere */
DEFINE            VARIABLE v-xno        LIKE ar-inv.x-no. /* Unique Internial # for header */
DEFINE            VARIABLE v-xline          AS INTEGER.     /* Unique Internail # for lines */
DEFINE            VARIABLE v-ord-no           LIKE inv-line.ord-no.
DEFINE            VARIABLE v-ord-date         AS DATE.
DEFINE            VARIABLE v-cost            AS DECIMAL   EXTENT 4 NO-UNDO.


DEFINE            VARIABLE iInvoiceQty       LIKE oe-ordl.inv-qty.
DEFINE            VARIABLE dInvDisc          AS DECIMAL   FORMAT "->>,>>9.99".
DEFINE            VARIABLE dInvDisc-w        AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE dTempAmount       AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE dTaxRate          AS DECIMAL   EXTENT 4 NO-UNDO.

DEFINE            VARIABLE dUninvOrdlAmt     LIKE oe-ordl.t-price NO-UNDO INIT 0.
DEFINE            VARIABLE lUInv             LIKE oe-ctrl.u-inv INIT FALSE NO-UNDO.
DEFINE            VARIABLE dLineTot          LIKE inv-line.t-price NO-UNDO.
DEFINE            VARIABLE dMiscTotal        LIKE inv-misc.amt NO-UNDO.
DEFINE            VARIABLE dLineTot-w        AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE lPostZeroCGS      AS LOG       NO-UNDO.
DEFINE            VARIABLE cExportNk1        LIKE sys-ctrl.char-fld NO-UNDO.
DEFINE            VARIABLE iRecsWritten      AS INTEGER   NO-UNDO.
DEFINE            VARIABLE iTotRecsWritten   AS INTEGER   NO-UNDO.

DEFINE            VARIABLE iCaseCount        LIKE itemfg.case-count NO-UNDO.
DEFINE            VARIABLE iCloseQty         LIKE oe-ordl.qty NO-UNDO.
DEFINE            VARIABLE dDcrVal           LIKE oe-ordl.cost INIT 0 NO-UNDO.
DEFINE            VARIABLE iUomRate          AS INTEGER NO-UNDO.
DEFINE            VARIABLE dSumRelQty        AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE dTax              AS DECIMAL NO-UNDO.
DEFINE            VARIABLE lIsInvalid        AS LOG       NO-UNDO.
DEFINE            VARIABLE cCListName        LIKE cListName NO-UNDO.
DEFINE            VARIABLE lFtpDone          AS LOG       NO-UNDO.
DEFINE            VARIABLE cPrintFormat      AS cha       NO-UNDO.
DEFINE            VARIABLE lWarned           AS LOG       NO-UNDO.
DEFINE            VARIABLE dTotalTax         AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE dTotalRate        AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE lUseLogs          AS LOGICAL NO-UNDO.
DEFINE            VARIABLE cDebugLog        AS CHARACTER NO-UNDO.

DEFINE STREAM sDebug.

DEFINE TEMP-TABLE w-report NO-UNDO LIKE report.

  
DEFINE TEMP-TABLE tt-gl NO-UNDO 
    FIELD row-id AS ROWID.
    
DEFINE TEMP-TABLE tt-custbal NO-UNDO
    FIELD cust-no AS CHARACTER
    FIELD ord-bal AS DECIMAL
    INDEX i1 cust-no.

DEFINE BUFFER b-inv-head FOR inv-head.
DEFINE BUFFER save-line  FOR reftable.

{oe/invwork.i NEW}
{oe/closchk.i NEW}

RUN oe/getacct.p.

FIND FIRST ar-ctrl WHERE ar-ctrl.company EQ cocode NO-LOCK.
IF v-return THEN RETURN.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "INVPOST"
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN 
DO TRANSACTION:
    MESSAGE "Creating new System Control record (INVPOST).".
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company = cocode
        sys-ctrl.name    = "INVPOST"
        sys-ctrl.log-fld = NO
        sys-ctrl.descrip = "Post cost-of-goods sold when cost is zero?"
        .

END.
lPostZeroCGS = sys-ctrl.log-fld.
FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "INVPRINT" NO-LOCK NO-ERROR.
cPrintFormat = IF AVAILABLE sys-ctrl THEN sys-ctrl.char-fld ELSE "".

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "AREXP"
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN 
DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company  = cocode
        sys-ctrl.name     = "AREXP"
        sys-ctrl.descrip  = "A/R Export option"
        sys-ctrl.char-fld = "ASI".

END.
cExportNk1 = sys-ctrl.char-fld.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK.
ASSIGN
    lIsFreightTax = oe-ctrl.f-tax
    lUInv         = oe-ctrl.u-inv
    .

DEFINE VARIABLE is-xprint-form AS LOG       NO-UNDO.
DEFINE VARIABLE ls-fax-file    AS cha       NO-UNDO.
DEFINE VARIABLE lv-audit-dir   AS CHARACTER NO-UNDO.

DO TRANSACTION:
    {sys/inc/postdate.i}
    {sys/inc/oeprep.i}
    {sys/inc/oeclose.i}
    FIND FIRST sys-ctrl WHERE
        sys-ctrl.company EQ cocode AND
        sys-ctrl.name    EQ "AUDITDIR"
        NO-LOCK NO-ERROR.
   
    IF NOT AVAILABLE sys-ctrl THEN 
    DO:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company  = cocode
            sys-ctrl.name     = "AUDITDIR"
            sys-ctrl.descrip  = "Audit Trails directory"
            sys-ctrl.char-fld = ".\AUDIT TRAILS"
            .
    END.
  
    lv-audit-dir = sys-ctrl.char-fld.
  
    IF LOOKUP(SUBSTR(lv-audit-dir,LENGTH(lv-audit-dir),1),"/,\") > 0 THEN
        lv-audit-dir = SUBSTR(lv-audit-dir,1,LENGTH(lv-audit-dir) - 1).
  
    RELEASE sys-ctrl.
END.

&SCOPED-DEFINE use-factored

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* These are used in include files, so cannot be changed */
DEFINE VARIABLE tran-period   AS INTEGER   FORMAT ">>":U INITIAL 0 LABEL "Period" .
DEFINE VARIABLE inexport-log  AS LOG NO-UNDO.
DEFINE VARIABLE inexport-desc AS CHARACTER NO-UNDO.
DEFINE VARIABLE inexport-cha  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cStatus       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReason       AS CHARACTER NO-UNDO.

/* ************************  Function Prototypes ********************** */
FUNCTION fDebugMsg RETURNS CHARACTER 
	(INPUT ipcMessage AS CHARACTER   ) FORWARD.

/* Main Block */
FIND FIRST period                   
      WHERE period.company EQ cocode
        AND period.pst     LE dtPostDate
        AND period.pend    GE dtPostDate
      NO-LOCK NO-ERROR.
      
   IF AVAILABLE period THEN
      tran-period = period.pnum.
   ELSE 
      .
      
IF SEARCH("logs/r-invepb.txt") NE ? THEN 
  lUseLogs = TRUE.
cDebugLog = "logs/" + "r-invepb" + string(today, "99999999") + STRING(time) + STRING(RANDOM(1,10)) + ".txt".
IF lUseLogs THEN 
  OUTPUT STREAM sDebug TO VALUE(cDebugLog).
  
ASSIGN 
       v-post = lpost
       v-detail = lInvoiceReportDetail /* Required since not part of parameters */
       locode = gloc
       g_loc = gloc
       .
       
/* persist.p needed for write trigger of job */       
RUN nosweat/persist.p PERSISTENT SET Persistent-Handle.
RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.

/* Main procedure to print and post */
RUN pPrintPost.

DELETE OBJECT Persistent-Handle.
DELETE OBJECT ListLogic-Handle.
DELETE OBJECT hNotesProcs.
OUTPUT STREAM sDebug CLOSE.


/* End Main Block */

PROCEDURE calc-tax-gr :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipr-head-row AS ROWID.
    DEFINE INPUT PARAMETER ipi-inv-no LIKE inv-head.inv-no NO-UNDO.
    DEFINE BUFFER bf-currency FOR currency.
    DEFINE BUFFER bf-inv-head FOR inv-head.

    DEFINE VARIABLE k      AS INTEGER NO-UNDO.
    DEFINE VARIABLE dAccum AS DECIMAL NO-UNDO.

    FIND bf-inv-head NO-LOCK WHERE ROWID(bf-inv-head) = ipr-head-row  NO-ERROR.

    IF NOT AVAILABLE bf-inv-head THEN
        RETURN.

    FIND FIRST bf-currency NO-LOCK
        WHERE bf-currency.company     EQ bf-inv-head.company
        AND bf-currency.c-code      EQ bf-inv-head.curr-code[1]
        AND bf-currency.ar-ast-acct NE ""
        AND bf-currency.ex-rate     GT 0
        NO-ERROR.

    ASSIGN 
        dTotalTax  = 0
        dTotalRate = 0.
    FIND FIRST stax NO-LOCK
    {sys/ref/stax1W.i}
        AND {sys/ref/taxgroup.i stax} EQ bf-inv-head.tax-gr
        NO-ERROR.
    IF NOT AVAILABLE stax THEN
        FIND FIRST stax NO-LOCK
            WHERE stax.company = bf-inv-head.company AND
            stax.tax-group EQ bf-inv-head.tax-gr
            NO-ERROR.
    dAccum = 1.
    IF AVAILABLE stax THEN 
    DO:
        DO i = 1 TO EXTENT(stax.tax-rate1):
            IF stax.tax-rate1[i] = 0 THEN NEXT.
            dTaxRate[i] = stax.tax-rate1[i].
            IF stax.accum-tax THEN 
            DO: 
                /*        ##PN - must find effective rate since this is accumulated*/
                dAccum = dAccum  * (1 + dTaxRate[i] / 100).
                dTaxRate[i] = 100 * (dAccum - (dTotalRate / 100) - 1).
            END.
            IF stax.company EQ "yes" AND i GT 1 THEN
            DO k = 1 TO i - 1:
                dTaxRate[i] = dTaxRate[i] +
                    (dTaxRate[i] * (stax.tax-rate1[k] / 100)).
            END.
            dTotalRate = dTotalRate + dTaxRate[i].
        END.
      
        DO i = 1 TO EXTENT(stax.tax-rate1):
            IF stax.tax-rate1[i] = 0 THEN NEXT.
            ASSIGN 
                dTaxRate[i] = ROUND(dTaxRate[i] / dTotalRate *
                                     bf-inv-head.t-inv-tax,2)
                dTotalTax   = dTotalTax + dTaxRate[i]
                .
        END.
      
        IF bf-inv-head.t-inv-tax NE dTotalTax THEN
            dTaxRate[1] = dTaxRate[1] +
                (bf-inv-head.t-inv-tax - dTotalTax).
      
        DO i = 1 TO EXTENT(stax.tax-rate1):
            IF stax.tax-rate1[i] = 0 THEN NEXT.
            FIND FIRST account NO-LOCK
                WHERE account.company EQ cocode
                AND account.actnum  EQ stax.tax-acc1[i]
                NO-ERROR.
            
            IF AVAILABLE account AND dTaxRate[i] NE 0 THEN 
            DO:
                CREATE tt-report.
                ASSIGN
                    tt-report.term-id = ""
                    tt-report.key-01  = "work-tax"
                    tt-report.key-02  = account.actnum
                    tt-report.key-03  = STRING(ipi-inv-no,"999999")
                    tt-report.key-04  = bf-inv-head.tax-gr
                    tt-report.key-05  = STRING(dTaxRate[i] *
                                      (IF AVAILABLE bf-currency  THEN
                                         bf-currency.ex-rate ELSE 1))
                    tt-report.weight  = dLineTot-w *
                               (dTaxRate[i] / bf-inv-head.t-inv-tax)
                               .
            END. /* avail account */

        END. /* 1 to 3 */

    END. /* avail stax */
END PROCEDURE.

PROCEDURE calc-tons :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ip-i-no LIKE itemfg.i-no NO-UNDO.
    DEFINE INPUT  PARAMETER ip-qty AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER op-weight AS DECIMAL NO-UNDO.

    DEFINE BUFFER b-itemfg FOR itemfg.


    FIND FIRST b-itemfg NO-LOCK
        WHERE b-itemfg.company EQ cocode
        AND b-itemfg.i-no    EQ ip-i-no
        NO-ERROR.
    IF AVAILABLE b-itemfg AND b-itemfg.weight-100 NE 0 THEN
        op-weight = b-itemfg.weight-100 * ip-qty / 100.

END PROCEDURE.

PROCEDURE close-order :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    IF CAN-FIND(FIRST oe-ordl WHERE 
        oe-ordl.company = oe-ord.company AND
        oe-ordl.ord-no = oe-ord.ord-no AND 
        oe-ordl.stat NE "C") THEN RETURN.

    RUN oe\close.p(RECID(oe-ord), YES).
END PROCEDURE.

PROCEDURE close-order-lines :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /* mdp adds logic to close lines from clslinchk.p */
   
    DEFINE BUFFER lb-oe-ordl FOR oe-ordl.

    FOR EACH oe-ordl NO-LOCK
        WHERE oe-ordl.company EQ oe-ord.company
        AND oe-ordl.ord-no  EQ oe-ord.ord-no:

        FIND itemfg NO-LOCK OF oe-ordl  NO-ERROR.
    
        IF (oe-ordl.inv-qty  GE oe-ordl.qty * (1 - (oe-ordl.under-pct / 100)) OR
            oe-ordl.is-a-component)                                               AND
            (oe-ordl.ship-qty GE oe-ordl.qty * (1 - (oe-ordl.under-pct / 100)) OR
            CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl})  OR
            (AVAILABLE itemfg AND NOT itemfg.stocked)) THEN                               
        DO:
            FIND lb-oe-ordl WHERE ROWID(lb-oe-ordl) = ROWID(oe-ordl)
                EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            IF AVAILABLE lb-oe-ordl THEN lb-oe-ordl.stat = "C".
        END.
    END.
END PROCEDURE.

PROCEDURE copy-report-to-audit-dir :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE targetfile AS CHARACTER FORMAT "X(50)" NO-UNDO.
    DEFINE VARIABLE dirname1   AS CHARACTER FORMAT "X(20)" NO-UNDO.
    DEFINE VARIABLE dirname2   AS CHARACTER FORMAT "X(20)" NO-UNDO.
    DEFINE VARIABLE dirname3   AS CHARACTER FORMAT "X(20)" NO-UNDO.
  
    ASSIGN 
        targetfile = lv-audit-dir + "\OP\OB4\Run#"
                    + STRING(v-trnum) + ".txt"
        dirname1   = lv-audit-dir
        dirname2   = lv-audit-dir + "\OP"
        dirname3   = lv-audit-dir + "\OP\OB4"
        .

    OS-COPY VALUE(cListName) VALUE (targetfile).

    IF SEARCH(targetfile) EQ ? THEN 
    DO:
        OS-CREATE-DIR VALUE(dirname1).
        OS-CREATE-DIR VALUE(dirname2).
        OS-CREATE-DIR VALUE(dirname3).
        OS-COPY VALUE(cListName) VALUE (targetfile).
    END.
END PROCEDURE.

PROCEDURE create-save-line :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    
    DISABLE TRIGGERS FOR LOAD OF inv-line.
    DISABLE TRIGGERS FOR LOAD OF inv-misc.

    FOR EACH inv-line WHERE inv-line.r-no EQ b-inv-head.r-no:
        CREATE save-line.
        ASSIGN
            save-line.reftable = "save-line" + STRING(v-trnum,"9999999999")
            save-line.val[1]   = inv-line.r-no
            save-line.val[2]   = inv-head.r-no
            save-line.val[3]   = INT(RECID(inv-line))
            inv-line.r-no      = inv-head.r-no
            .
    END.

    FOR EACH inv-misc WHERE inv-misc.r-no EQ b-inv-head.r-no:
        CREATE save-line.
        ASSIGN
            save-line.reftable = "save-line" + STRING(v-trnum,"9999999999")
            save-line.val[1]   = inv-misc.r-no
            save-line.val[2]   = inv-head.r-no
            save-line.val[3]   = INT(RECID(inv-misc))
            inv-misc.r-no      = inv-head.r-no
            .
    END.

END PROCEDURE.

PROCEDURE get-lot-no :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       THIS IS TO ASSIGN THE LOT NUMBER FROM THE REFTABLE TO 
    ------------------------------------------------------------------------------*/
      IF AVAILABLE inv-line AND AVAILABLE ar-invl THEN 
        ASSIGN 
              ar-invl.lot-no = TRIM(inv-line.lot-no).

END PROCEDURE.

PROCEDURE get-tr-dscr :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ip-inv-no LIKE ar-inv.inv-no NO-UNDO.
    DEFINE OUTPUT PARAMETER op-dscr LIKE gltrans.tr-dscr NO-UNDO.


    RELEASE ar-inv.
    RELEASE cust.

    FIND FIRST ar-inv NO-LOCK
        WHERE ar-inv.company EQ cocode
        AND ar-inv.inv-no  EQ ip-inv-no
        NO-ERROR.
    IF AVAILABLE ar-inv THEN
        FIND FIRST cust NO-LOCK
            WHERE cust.company EQ ar-inv.company
            AND cust.cust-no EQ ar-inv.cust-no
            NO-ERROR.
    op-dscr = TRIM(IF AVAILABLE cust THEN cust.name ELSE "Cust not on file") +
        " Inv# " + STRING(ip-inv-no,"99999999").

END PROCEDURE.

PROCEDURE list-gl :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/


    DEFINE VARIABLE dGLSales    AS DECIMAL   FORMAT ">>,>>>,>>9.99cr" NO-UNDO.
    DEFINE VARIABLE cAccountDscr        LIKE account.dscr NO-UNDO.
    DEFINE VARIABLE v-disp-actnum LIKE account.actnum NO-UNDO.
    DEFINE VARIABLE v-disp-amt    AS DECIMAL   FORMAT ">>,>>>,>>9.99cr" NO-UNDO.
    DEFINE VARIABLE v-tmp-amt     AS DECIMAL   FORMAT ">>,>>>,>>9.99cr" NO-UNDO.
    DEFINE VARIABLE v-empty       AS DECIMAL   FORMAT ">>,>>>,>>9.99cr" NO-UNDO.
    DEFINE VARIABLE ld-t          AS DECIMAL   FORMAT "->>>>9.99" EXTENT 3 NO-UNDO.
    DEFINE VARIABLE ld-pton       AS DECIMAL   FORMAT "->>>9.999" NO-UNDO.
    DEFINE VARIABLE lv-label-ton  AS CHARACTER FORMAT "x(19)" EXTENT 2 NO-UNDO.
    DEFINE VARIABLE v-recid       AS RECID     INIT ?.
    DEFINE VARIABLE lv-rowid      AS ROWID     NO-UNDO.

    DEFINE BUFFER b-tt-report FOR tt-report.

    post-print: 
    DO WHILE TRUE.
        cListName = TRIM(cCListName) + ".001".

        /** LIST G/L FOR LINE ITEMS **/
        {aoa/BL/r-inve&2b.i work-line "ITEMS"}

        /** LIST G/L FOR MISC. **/
        {aoa/BL/r-inve&2b.i work-misc "MISC."}

        /** LIST G/L FOR SALES TAX **/
        {aoa/BL/r-inve&2b.i work-tax "SALES TAX"}

        /** LIST G/L FOR CURRENCY GAIN/LOSS **/
        {aoa/BL/r-inve&2b.i work-curr "CURRENCY GAIN/LOSS"}
    
        /** LIST G/L FOR FG/COGS **/
        IF lGLReportDetail THEN 
        DO:
            ASSIGN
                v-disp-amt = 0
                ld-t[2]    = 0.

            FOR EACH tmp-work-job BREAK BY tmp-work-job.actnum
                BY tmp-work-job.inv-no:
                FIND FIRST account NO-LOCK WHERE account.company = cocode AND
                    account.actnum  = tmp-work-job.actnum
                    NO-ERROR.
                IF AVAILABLE account THEN
                    ASSIGN cAccountDscr = account.dscr
                    .
                ELSE
                    ASSIGN cAccountDscr = "ACCOUNT NOT FOUND - " + tmp-work-job.actnum
                    .

                ACCUMULATE tmp-work-job.amt (TOTAL BY tmp-work-job.actnum).
                ld-t[1] = tmp-work-job.weight / 2000.

                IF tmp-work-job.fg THEN
                    ASSIGN v-tmp-amt  = - tmp-work-job.amt
                        v-disp-amt = v-disp-amt - tmp-work-job.amt
                        ld-t[1]    = - ld-t[1]
                        .
                ELSE
                    ASSIGN v-tmp-amt  = tmp-work-job.amt
                        v-disp-amt = v-disp-amt + tmp-work-job.amt
                        .

                ASSIGN
                    ld-t[2] = ld-t[2] + ld-t[1]
                    ld-pton = v-tmp-amt / ld-t[1].

                IF ld-pton EQ ? THEN ld-pton = 0.


                IF LAST-OF(tmp-work-job.actnum) THEN 
                DO:
                    
                    IF lPrintTon THEN 
                    DO:
                        ld-pton = v-disp-amt / ld-t[2].
                        IF ld-pton EQ ? THEN ld-pton = 0.
                    /*            PUT ld-pton TO 138 ld-t[2] TO 148 SKIP(1).*/
                    END.

                    ASSIGN
                        v-disp-amt = 0
                        ld-t[2]    = 0
                        .
                END.
            END.
        END.

        FOR EACH work-job NO-LOCK BREAK BY work-job.actnum:
            FIND FIRST account WHERE account.company = cocode AND
                account.actnum  = work-job.actnum
                NO-ERROR.
            IF AVAILABLE account THEN
                ASSIGN cAccountDscr = account.dscr
                .
            ELSE
                ASSIGN cAccountDscr = "ACCOUNT NOT FOUND - " + work-job.actnum
                .

            ASSIGN 
                v-disp-actnum = work-job.actnum
                ld-t[2]       = work-job.weight / 2000
                .

            IF work-job.fg THEN
                ASSIGN v-disp-amt = - work-job.amt
                    ld-t[2]    = - ld-t[2]
                    .
            ELSE
                ASSIGN v-disp-amt = work-job.amt
                .

            ld-pton = v-disp-amt / ld-t[2].

            IF ld-pton EQ ? THEN ld-pton = 0.

            ASSIGN
                v-balance = v-balance + v-disp-amt
                ld-t[3]   = ld-t[3] + ld-t[2]
                .
        END. /* each work-job */

        /** POST FREIGHT TO G/L **/
        FIND FIRST account NO-LOCK
            WHERE account.company EQ cocode
            AND account.actnum  EQ v-ar-freight
            NO-ERROR.
        ASSIGN
            cAccountDscr     = IF AVAILABLE account THEN account.dscr
                               ELSE "ACCOUNT NOT FOUND - FREIGHT"
            v-disp-amt = 0
            ld-t[2]    = 0
            .

        IF lGLReportDetail THEN 
        DO:
            FOR EACH tt-report NO-LOCK
                WHERE tt-report.term-id EQ ""
                AND tt-report.key-01  EQ "work-freight"
                
                BREAK BY tt-report.key-02:

                ASSIGN
                    ld-t[1]    = tt-report.weight / 2000
                    v-disp-amt = v-disp-amt + dec(tt-report.key-05)
                    ld-t[2]    = ld-t[2] + ld-t[1]
                    .

                IF dec(tt-report.key-05) NE 0 THEN 
                DO:
                    ld-pton = dec(tt-report.key-05) / ld-t[1].

                    IF ld-pton EQ ? THEN ld-pton = 0.

                END.
            END.

            IF v-disp-amt NE 0 THEN 
            DO:
                
                IF lPrintTon THEN 
                DO:
                    ld-pton = v-disp-amt / ld-t[2].
                    IF ld-pton EQ ? THEN ld-pton = 0.

                END.
                
                ASSIGN
                    v-disp-amt = 0
                    ld-t[2]    = 0
                    .
            END.
        END.

        ASSIGN
            v-disp-actnum = v-ar-freight
            v-disp-amt    = v-post-freight
            ld-t[2]       = v-post-freight-w / 2000.

        IF NOT lGLReportDetail THEN 
        DO:
            ld-pton = v-disp-amt / ld-t[2].

            IF ld-pton EQ ? THEN ld-pton = 0.

        END.

        v-balance = v-balance + v-post-freight.
        /** POST DISCOUNT TO G/L **/
        FIND FIRST account NO-LOCK
            WHERE account.company EQ cocode
            AND account.actnum  EQ v-ar-disc
            NO-ERROR.
        ASSIGN
            cAccountDscr     = IF AVAILABLE account THEN account.dscr
                   ELSE "ACCOUNT NOT FOUND - DISCOUNT"
            v-disp-amt = 0
            ld-t[2]    = 0.

        IF lGLReportDetail THEN 
        DO:
            FOR EACH tt-report NO-LOCK
                WHERE tt-report.term-id EQ ""
                AND tt-report.key-01  EQ "work-disc"
                
                BREAK BY tt-report.key-02:

                ASSIGN
                    ld-t[1]    = tt-report.weight / 2000
                    v-disp-amt = v-disp-amt + dec(tt-report.key-05)
                    ld-t[2]    = ld-t[2] + ld-t[1].

                IF dec(tt-report.key-05) NE 0 THEN 
                DO:
                    ld-pton = dec(tt-report.key-05) / ld-t[1].

                    IF ld-pton EQ ? THEN ld-pton = 0.

                END.
            END.

            IF v-disp-amt NE 0 THEN 
            DO:
                
                IF lPrintTon THEN 
                DO:
                    ld-pton = v-disp-amt / ld-t[2].
                    IF ld-pton EQ ? THEN ld-pton = 0.
                
                END.
                
                ASSIGN
                    v-disp-amt = 0
                    ld-t[2]    = 0.
            END.
        END.

        ASSIGN
            v-disp-actnum = v-ar-disc
            v-disp-amt    = v-post-disc
            ld-t[2]       = v-post-disc-w / 2000.

        IF NOT lGLReportDetail THEN 
        DO:
            ld-pton = v-disp-amt / ld-t[2].

            IF ld-pton EQ ? THEN ld-pton = 0.

        END.

        v-balance = v-balance + v-disp-amt.
        /** POST CASH TO G/L **/
        IF v-post-cash NE 0 THEN 
        DO:
            FIND FIRST account NO-LOCK
                WHERE account.company EQ cocode
                AND account.actnum  EQ ar-ctrl.cash-act
                NO-ERROR.
            cAccountDscr = IF AVAILABLE account THEN account.dscr
            ELSE "ACCOUNT NOT FOUND - CASH".

            IF lGLReportDetail THEN 
            DO:
                ASSIGN
                    v-disp-amt = 0
                    ld-t[2]    = 0.

                FOR EACH tt-report NO-LOCK
                    WHERE tt-report.term-id EQ ""
                    AND tt-report.key-01  EQ "work-cash"
                    BREAK BY tt-report.key-02:

                    ASSIGN
                        ld-t[1]    = tt-report.weight / 2000
                        v-disp-amt = v-disp-amt + dec(tt-report.key-05)
                        ld-t[2]    = ld-t[2] + ld-t[1].

                    IF dec(tt-report.key-05) NE 0 THEN 
                    DO:
                        ld-pton = dec(tt-report.key-05) / ld-t[1].

                        IF ld-pton EQ ? THEN ld-pton = 0.

                    END.
                END.

                IF v-disp-amt NE 0 THEN 
                DO:
                    
                    IF lPrintTon THEN 
                    DO:
                        ld-pton = v-disp-amt / ld-t[2].
                        IF ld-pton EQ ? THEN ld-pton = 0.

                    END.
                    
                    ASSIGN
                        v-disp-amt = 0
                        ld-t[2]    = 0.
                END.
            END.

            ASSIGN
                v-disp-actnum = ar-ctrl.cash-act
                v-disp-amt    = v-post-cash
                ld-t[2]       = v-post-cash-w / 2000.

            IF NOT lGLReportDetail THEN 
            DO:
                ld-pton = v-disp-amt / ld-t[2].

                IF ld-pton EQ ? THEN ld-pton = 0.

            END.

            v-balance = v-balance + v-disp-amt.
        END.  

        /** OFFSET ENTRY TO G/L **/
        FIND FIRST account NO-LOCK
            WHERE account.company = cocode
            AND account.actnum  = v-ar-acct
            NO-ERROR.

        ASSIGN
            cAccountDscr        = IF AVAILABLE account THEN account.dscr
                                  ELSE "ACCOUNT NOT FOUND - OFFSET"
            v-disp-actnum = v-ar-acct
            v-disp-amt    = v-post-total.

        IF lGLReportDetail THEN 
        DO:
            ASSIGN
                ld-t[1] = v-post-total-w / 2000
                ld-pton = v-disp-amt / ld-t[1].

            IF ld-pton EQ ? THEN ld-pton = 0.

  
        END.

        ELSE 
        DO:
            ASSIGN
                ld-t[2] = v-post-total-w / 2000
                ld-pton = v-disp-amt / ld-t[2].

            IF ld-pton EQ ? THEN ld-pton = 0.

        END.

        v-balance = v-balance + v-post-total.   
   


        LEAVE.
    END. /* post-print */

    OUTPUT CLOSE.


    SESSION:SET-WAIT-STATE ("general").

    FOR EACH tt-report WHERE RECID(tt-report) NE v-recid:
        DELETE tt-report.
    END.

    SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

PROCEDURE list-post-inv :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-list-post AS CHARACTER NO-UNDO.

    DEFINE BUFFER b-oe-boll FOR oe-boll.

    DEFINE VARIABLE ld-t            AS DECIMAL FORMAT "->>>>9.99" EXTENT 3 NO-UNDO.
    DEFINE VARIABLE ld-pton         AS DECIMAL FORMAT "->>>9.999" NO-UNDO.
    DEFINE VARIABLE v-close-line-ok AS LOGICAL INITIAL NO.
    DEFINE VARIABLE v-first         AS LOG     INIT YES.
    DEFINE VARIABLE v-tot-frt       AS DECIMAL NO-UNDO.
  
  
  
 
    SESSION:SET-WAIT-STATE ("general").

    RUN oe/invpostd.p ("").
      
    v-post = ip-list-post EQ "post" AND lPost.
    fDebugMsg("List Post - setting v-post " + STRING(v-post)) .
    
    DISABLE TRIGGERS FOR LOAD OF inv-head.
    DISABLE TRIGGERS FOR LOAD OF inv-line.
    DISABLE TRIGGERS FOR LOAD OF oe-ord.
    DISABLE TRIGGERS FOR LOAD OF oe-ordl.
    DISABLE TRIGGERS FOR LOAD OF itemfg.
    DISABLE TRIGGERS FOR LOAD OF oe-relh.
    DISABLE TRIGGERS FOR LOAD OF oe-rell.
    
    ordblock:
    FOR EACH w-report NO-LOCK WHERE w-report.term-id EQ "" ,
    
        FIRST inv-head WHERE RECID(inv-head) EQ w-report.rec-id
        
        TRANSACTION
      
        BY w-report.key-01:
            
        /* Create eddoc for invoice if required */
        RUN ed/asi/o810hook.p (recid(inv-head), no, no).     
        FIND FIRST edmast NO-LOCK
            WHERE edmast.cust EQ inv-head.cust-no
            NO-ERROR.
        IF AVAIL edmast THEN 
        DO: 
            FIND FIRST edcode NO-LOCK
                WHERE edcode.partner EQ edmast.partner
                NO-ERROR.
            IF NOT AVAIL edcode THEN 
                FIND FIRST edcode NO-LOCK
                    WHERE edcode.partner EQ edmast.partnerGrp
                    NO-ERROR.
        END.  
        
        IF AVAIL edcode AND edcode.sendFileOnPrint THEN    
            RUN ed/asi/write810.p (INPUT cocode, INPUT inv-head.inv-no). 

        /* {oe/r-inve&pb.i} */
        
        fDebugMsg("list-post-inv invoice # " + string(inv-head.inv-no)).
        IF v-post THEN 
        DO:
            v-xno = 1.
            FIND LAST ar-inv NO-LOCK USE-INDEX x-no  NO-ERROR.
            IF AVAILABLE ar-inv THEN
                ASSIGN
                    v-xno = ar-inv.x-no + 1
                    v-xline   = 0.

            CREATE ar-inv.
            {oe/invhpost.i}

            IF cExportNk1 EQ "Sonoco" THEN 
            DO:
                RUN oe/sonofile.p (1,RECID(ar-inv)).
                RUN ar/sonoinv.p ("inv-head", RECID(inv-head),
                    OUTPUT iRecsWritten).
                              
                iTotRecsWritten = iTotRecsWritten + iRecsWritten.
            END.
        END.

        RELEASE currency.
        IF inv-head.terms NE "CASH"              AND
            cCompCurr NE ""                    AND
            cCompCurr NE inv-head.curr-code[1] THEN
            FIND FIRST currency NO-LOCK
                WHERE currency.company     EQ inv-head.company
                AND currency.c-code      EQ inv-head.curr-code[1]
                AND currency.ar-ast-acct NE ""
                AND currency.ex-rate     GT 0
                NO-ERROR.

        ASSIGN
            lPostable        = YES
            v-reduce-ord-bal = 0
            iInvoiceQty      = 0
            dInvDisc         = 0
            dLineTot         = 0
            dMiscTotal       = 0
            dLineTot-w       = 0
            dInvDisc-w       = 0
            v-ord-no          = 0
            v-ord-date        = ?.

        FOR EACH w-inv-line:
            DELETE w-inv-line.
        END.
        
        /************ line ITEMS ************************************************/
        FOR EACH inv-line
            WHERE inv-line.r-no EQ inv-head.r-no
            USE-INDEX r-no BREAK BY inv-line.ord-no:

            FIND FIRST itemfg
            {sys/look/itemfgrlW.i}
                AND itemfg.i-no EQ inv-line.i-no
                NO-ERROR.

            FIND FIRST uom NO-LOCK
                WHERE uom.uom  EQ inv-line.pr-uom
                AND uom.mult NE 0
                NO-ERROR.

            FIND FIRST oe-ordl
                WHERE oe-ordl.company EQ cocode
                AND oe-ordl.ord-no  EQ inv-line.ord-no
                AND oe-ordl.line    EQ inv-line.line
                AND oe-ordl.i-no    EQ inv-line.i-no
                USE-INDEX ord-no NO-ERROR.

            RELEASE oe-ord.
            IF inv-line.ord-no NE 0 AND AVAILABLE oe-ordl THEN
                FIND FIRST oe-ord NO-LOCK
                    WHERE oe-ord.company EQ oe-ordl.company
                    AND oe-ord.ord-no  EQ oe-ordl.ord-no
                    NO-ERROR.

            IF AVAILABLE oe-ord AND v-post THEN 
            DO:
            {oe/closeaud.i oe-ord}
                CREATE w-ord.
                ASSIGN
                    w-ord.ord-no = oe-ord.ord-no
                    w-ord.rec-id = RECID(oe-ord)
                    .
            END.
          
            ASSIGN
                iInvoiceQty = iInvoiceQty + inv-line.inv-qty
                iCaseCount  = IF inv-line.cas-cnt NE 0 THEN
                         inv-line.cas-cnt
                       ELSE
                       IF AVAILABLE oe-ordl AND oe-ordl.cas-cnt NE 0 THEN
                         oe-ordl.cas-cnt
                       ELSE
                       IF AVAILABLE itemfg AND itemfg.case-count NE 0 THEN
                         itemfg.case-count
                       ELSE 1.

            IF FIRST(inv-line.ord-no) THEN
                ASSIGN
                    v-ord-no   = inv-line.ord-no
                    v-ord-date = inv-line.ord-date.


            /*if v-detail and not v-post then do:*/
            CREATE w-inv-line.
            ASSIGN
                w-inv-line.ord-no   = inv-line.ord-no
                w-inv-line.i-no     = inv-line.i-no
                w-inv-line.i-name   = inv-line.i-name
                w-inv-line.qty      = inv-line.qty
                w-inv-line.inv-qty  = inv-line.inv-qty
                w-inv-line.ship-qty = inv-line.ship-qty
                w-inv-line.price    = inv-line.price
                w-inv-line.uom      = inv-line.pr-uom
                w-inv-line.t-price  = inv-line.t-price
                w-inv-line.cost     = inv-line.cost
                .
            /*end.*/

            IF AVAILABLE itemfg THEN
                FIND FIRST fgcat NO-LOCK 
                    WHERE fgcat.company EQ cocode
                    AND fgcat.procat  EQ itemfg.procat
                    NO-ERROR.
            ELSE
                IF v-post THEN UNDO ordblock, NEXT ordblock.

            RUN oe/GetCostInvl.p (ROWID(inv-line),
                OUTPUT v-cost[1], OUTPUT v-cost[2],
                OUTPUT v-cost[3], OUTPUT v-cost[4],
                OUTPUT inv-line.cost, OUTPUT inv-line.spare-char-2, 
                OUTPUT inv-line.t-cost, OUTPUT inv-line.spare-char-1).
            w-inv-line.t-cost = inv-line.t-cost.
            IF inv-line.inv-qty NE 0 AND
                inv-line.t-cost EQ 0  AND 
                NOT lPostZeroCGS   THEN UNDO ordblock, NEXT ordblock.

            RUN oe/invposty.p (inv-head.inv-no, inv-line.i-no, inv-line.inv-qty,
                "M", v-cost[1], v-cost[2], v-cost[3], v-cost[4]).

            IF AVAILABLE itemfg AND v-post THEN 
            DO:
                ASSIGN
                    v-invline = RECID(inv-line)
                    v-invhead = RECID(inv-head).

                RUN oe/invpost3.p (dtPostDate, tran-period).
            END. /* avail itemfg & v-post */

            RUN calc-tons (w-inv-line.i-no, w-inv-line.inv-qty, OUTPUT w-inv-line.weight).

            IF inv-line.t-price NE 0 THEN 
            DO:
                dTempAmount = 0.
                IF inv-line.disc NE 0 THEN 
                DO:
                    dTempAmount = ROUND((IF inv-line.pr-uom BEGINS "L" AND
                        inv-line.pr-uom NE "LB"    THEN
                        IF inv-line.inv-qty LT 0 THEN -1 ELSE 1
                        ELSE
                        IF inv-line.pr-uom EQ "CS" THEN
                        inv-line.inv-qty / iCaseCount
                        ELSE
                        IF AVAILABLE uom THEN
                        inv-line.inv-qty / uom.mult
                        ELSE
                        inv-line.inv-qty / 1000) *
                        inv-line.price,2) -
                        inv-line.t-price.

                    IF AVAILABLE currency THEN
                        dTempAmount = dTempAmount * currency.ex-rate.

                    ASSIGN
                        dInvDisc   = dInvDisc + dTempAmount
                        dInvDisc-w = dInvDisc-w + w-inv-line.weight.
                END.

                CREATE tt-report.
                ASSIGN
                    tt-report.term-id = ""
                    tt-report.key-01  = "work-line"
                    tt-report.key-02  = IF AVAILABLE fgcat AND fgcat.glacc NE ""
                                 THEN fgcat.glacc ELSE v-ar-sales
                    tt-report.key-03  = STRING(inv-head.inv-no,"999999")
                    tt-report.key-04  = inv-line.i-no
                    tt-report.weight  = w-inv-line.weight
                    dTempAmount       = dTempAmount +
                                 (inv-line.t-price *
                                  (IF AVAILABLE currency THEN currency.ex-rate ELSE 1))
                    tt-report.key-05  = STRING(dTempAmount).
            END.

            IF v-post THEN 
            DO:
                /*** Calculate the amount of dollars to take out of the
                     customer's on order balance ***/
                FIND CURRENT oe-ordl NO-ERROR.
                IF AVAILABLE oe-ordl THEN 
                DO:
                  
                    RUN ar/calctax2.p (oe-ord.tax-gr,
                        NO,
                        oe-ordl.t-price,
                        oe-ordl.company, 
                        oe-ordl.i-no,
                        OUTPUT dTax).

                    dUninvOrdlAmt = oe-ordl.t-price +
                        (IF oe-ordl.tax THEN dTax ELSE 0).

                    FOR EACH ar-invl NO-LOCK
                        WHERE ar-invl.company EQ cocode
                        AND ar-invl.posted  EQ YES
                        AND ar-invl.cust-no EQ inv-head.cust-no
                        AND ar-invl.ord-no  EQ inv-line.ord-no
                        AND ar-invl.line    EQ inv-line.line
                        AND ar-invl.i-no    EQ inv-line.i-no
                        USE-INDEX inv-status :
                  
                        RUN ar/calctax2.p (ar-inv.tax-code, 
                            NO,
                            ar-invl.amt,
                            ar-invl.company,
                            ar-invl.i-no,
                            OUTPUT dTax).
                
                        dUninvOrdlAmt = dUninvOrdlAmt - ar-invl.amt -
                            (IF ar-invl.tax THEN dTax ELSE 0).
                    END.
                END.

                ELSE
                    dUninvOrdlAmt = 0.

                dTax = 0.
                IF inv-line.tax THEN
                    RUN ar/calctax2.p (inv-head.tax-gr, 
                        NO,
                        inv-line.t-price, 
                        inv-line.company,
                        inv-line.i-no,
                        OUTPUT dTax).
                                  
                IF inv-line.t-price + dTax LT dUninvOrdlAmt THEN
                    v-reduce-ord-bal = v-reduce-ord-bal + inv-line.t-price + dTax.
                ELSE
                    v-reduce-ord-bal = v-reduce-ord-bal + dUninvOrdlAmt.

                CREATE ar-invl.
            
                {oe/invlpost.i}.

                /* gdm - 09290908 */ RUN get-lot-no.

                IF cExportNk1 EQ "Sonoco" THEN RUN oe/sonofile.p (2,RECID(ar-invl)).
                ELSE
                    IF cExportNk1 EQ "Inland" THEN RUN ar/jdedward.p (RECID(ar-invl)).
                    ELSE 
                    DO:
                        IF cExportNk1 EQ "Excel" THEN 
                        DO: 
                            RUN ar/jdedwrdx.p (RECID(ar-invl), INPUT v-first).
                            v-first = NO.
                        END.
                    END.
            END. /* v-post */
          
            IF AVAILABLE tt-report THEN
                ASSIGN
                    dLineTot   = dLineTot   + inv-line.t-price
                    dLineTot-w = dLineTot-w + tt-report.weight
                    .

            IF v-post THEN 
            DO:
                IF inv-line.ord-no NE 0 THEN 
                DO:
                    /* Sum all release qty */
                    dSumRelQty = 0.

                    FOR EACH oe-boll NO-LOCK
                        WHERE oe-boll.company EQ inv-line.company
                        AND oe-boll.b-no    EQ inv-line.b-no
                        AND oe-boll.ord-no  EQ inv-line.ord-no
                        AND oe-boll.i-no    EQ inv-line.i-no
                        AND oe-boll.line    EQ inv-line.line
                        AND oe-boll.po-no   EQ inv-line.po-no
                        AND CAN-FIND(FIRST oe-bolh
                        WHERE oe-bolh.b-no   EQ oe-boll.b-no
                        AND oe-bolh.posted EQ YES)
                
                        BREAK BY oe-boll.r-no
                        BY oe-boll.rel-no
                        BY oe-boll.b-ord-no:

                        IF FIRST-OF(oe-boll.b-ord-no) THEN
                            FOR EACH oe-rell NO-LOCK
                                WHERE oe-rell.company  EQ oe-boll.company
                                AND oe-rell.ord-no   EQ oe-boll.ord-no
                                AND oe-rell.line     EQ oe-boll.line
                                AND oe-rell.i-no     EQ oe-boll.i-no
                                AND oe-rell.r-no     EQ oe-boll.r-no
                                AND oe-rell.rel-no   EQ oe-boll.rel-no
                                AND oe-rell.b-ord-no EQ oe-boll.b-ord-no
                                AND CAN-FIND(FIRST oe-relh
                                WHERE oe-relh.r-no   EQ oe-boll.r-no
                                AND oe-relh.posted EQ YES)
                                USE-INDEX ord-no :
                                dSumRelQty = dSumRelQty + oe-rell.qty.
                            END.
                    END.

                    IF AVAILABLE oe-ordl AND  dSumRelQty GE oe-ordl.qty AND 
                        (CAN-FIND(oe-boll WHERE oe-boll.company EQ inv-line.company
                        AND oe-boll.b-no   EQ inv-line.b-no
                        AND oe-boll.ord-no EQ inv-line.ord-no
                        AND oe-boll.i-no   EQ inv-line.i-no
                        AND oe-boll.line   EQ inv-line.line
                        AND oe-boll.po-no  EQ inv-line.po-no
                        AND oe-boll.p-c    EQ TRUE) OR
                        inv-line.p-c EQ TRUE) THEN
                        FOR EACH oe-ordl WHERE oe-ordl.company EQ cocode
                            AND oe-ordl.ord-no  EQ inv-line.ord-no
                            AND oe-ordl.i-no    EQ inv-line.i-no:
                            /* previous runs may have overstated the shipped qty.  re-acquire the "truth" */
                            RUN oe/ordlsqty.p (ROWID(oe-ordl), OUTPUT oe-ordl.inv-qty, 
                                OUTPUT oe-ordl.ship-qty).
                            ASSIGN 
                                oe-ordl.t-ship-qty = oe-ordl.ship-qty.
                            ASSIGN 
                                iCloseQty = oe-ordl.qty - oe-ordl.t-ship-qty.
                            IF iCloseQty LT 0 
                                /*10021404 - also do not reduce allocated for Invoice Only*/
                                OR (CAN-FIND(oe-boll WHERE oe-boll.company EQ inv-line.company
                                AND oe-boll.b-no   EQ inv-line.b-no
                                AND oe-boll.ord-no EQ inv-line.ord-no
                                AND oe-boll.i-no   EQ inv-line.i-no
                                AND oe-boll.line   EQ inv-line.line
                                AND oe-boll.po-no  EQ inv-line.po-no
                                AND oe-boll.s-code EQ "I"))
                                THEN iCloseQty = 0.

                            FIND FIRST xoe-ord NO-LOCK WHERE xoe-ord.company EQ cocode
                                AND xoe-ord.ord-no  EQ oe-ordl.ord-no
                                NO-ERROR.

                            IF AVAILABLE itemfg THEN 
                            DO:
                                IF xoe-ord.type NE "T" THEN
                                    itemfg.q-alloc = itemfg.q-alloc - iCloseQty.
                                IF itemfg.q-alloc LT 0 THEN itemfg.q-alloc = 0.

                                itemfg.q-avail = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc.
                                IF itemfg.q-avail LT 0 THEN itemfg.q-avail = 0.

                                RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT xoe-ord.loc).

                                FIND FIRST itemfg-loc 
                                    WHERE itemfg-loc.company EQ itemfg.company
                                    AND itemfg-loc.i-no    EQ itemfg.i-no
                                    AND itemfg-loc.loc     EQ xoe-ord.loc
                                    EXCLUSIVE-LOCK NO-ERROR.

                                ASSIGN
                                    itemfg.q-ptd     = itemfg.q-ptd     - iCloseQty
                                    itemfg.q-ord-ytd = itemfg.q-ord-ytd - iCloseQty.

                                IF AVAILABLE itemfg-loc THEN
                                    itemfg-loc.q-alloc = itemfg-loc.q-alloc - iCloseQty.
                          
                                IF AVAIL(itemfg-loc) THEN 
                                DO:
                                    IF itemfg-loc.q-alloc LT 0 THEN itemfg-loc.q-alloc = 0.
    
                                    itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.
                                    IF itemfg-loc.q-avail LT 0 THEN itemfg-loc.q-avail = 0.
    
                                    ASSIGN
                                        itemfg-loc.q-ptd     = itemfg-loc.q-ptd     - iCloseQty
                                        itemfg-loc.q-ord-ytd = itemfg-loc.q-ord-ytd - iCloseQty.
                                END.
                                FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
                            END.

                            IF iCloseQty GT 0 THEN 
                            DO:
                                ASSIGN
                                    iUomRate = IF oe-ordl.pr-uom EQ "M"  THEN 1000
                                ELSE
                                IF oe-ordl.pr-uom EQ "C"  THEN 100
                                ELSE
                                IF AVAILABLE itemfg           AND
                                   oe-ordl.pr-uom  EQ "CS"
                                                          THEN itemfg.case-count
                                ELSE 1

                                    dDcrVal  = (iCloseQty / iUomRate) * oe-ordl.price
                                    dDcrVal  = dDcrVal - (dDcrVal * oe-ordl.disc / 100).

                                IF oe-ordl.tax THEN 
                                DO:
                                    RUN ar/calctax2.p (oe-ord.tax-gr, 
                                        NO,
                                        dDcrVal,
                                        oe-ordl.company,
                                        oe-ordl.i-no,
                                        OUTPUT dTax).
                                      
                                    dDcrVal = dDcrVal + dTax.
                                END.
                  
                                IF AVAILABLE cust THEN cust.ord-bal = cust.ord-bal - dDcrVal.
                            END.

                        /*RUN oe/clslnchkinv.p (BUFFER oe-ordl,OUTPUT v-close-line-ok).
                        IF v-close-line-ok THEN
                           RUN oe/closelin.p (ROWID(oe-ordl),YES).*/
                
                        END. /* for each oe-ordl */
                END.
            END.
            STATUS DEFAULT "Posting for Invoic#: " + string(inv-head.inv-no) +
                ", Item: " + inv-line.i-no.
        END. /* each inv-line */

        /******************* MISCELLANEOUS ITEMS ***********************************/
        /* Be aware that job nos are not stroed in ar-invl records for misc charges*/

        FOR EACH inv-misc
            WHERE inv-misc.r-no EQ inv-head.r-no
            USE-INDEX r-no:

            IF v-detail AND NOT v-post AND inv-misc.bill EQ "Y" THEN 
            DO:
                CREATE w-ord-misc.
                ASSIGN
                    w-ord-misc.ord-no = inv-misc.ord-no
                    w-ord-misc.charge = inv-misc.charge
                    w-ord-misc.dscr   = inv-misc.dscr
                    w-ord-misc.amt    = inv-misc.amt
                    w-ord-misc.tax    = inv-misc.tax
                    w-ord-misc.bill   = inv-misc.bill.
            END. /* v-detail */

            IF inv-misc.bill EQ "Y" AND inv-misc.amt NE 0 THEN 
            DO:
                CREATE tt-report.
                ASSIGN
                    tt-report.term-id = ""
                    tt-report.key-01  = "work-misc"
                    tt-report.key-02  = IF inv-misc.actnum NE ""
                                 THEN inv-misc.actnum ELSE v-ar-sales
                    tt-report.key-03  = STRING(inv-head.inv-no,"999999")
                    tt-report.key-04  = inv-misc.charge
                    tt-report.key-05  = STRING(inv-misc.amt *
                                        (IF AVAILABLE currency  THEN
                                           currency.ex-rate ELSE 1)).

                dMiscTotal = dMiscTotal + inv-misc.amt.
                
                RELEASE oe-ord.
                IF inv-misc.ord-no NE 0 THEN
                    FIND FIRST oe-ord NO-LOCK
                        WHERE oe-ord.company EQ inv-misc.company
                        AND oe-ord.ord-no  EQ inv-misc.ord-no
                        NO-ERROR.

                IF AVAILABLE oe-ord AND v-post THEN 
                DO:
              {oe/closeaud.i oe-ord}
                    CREATE w-ord.
                    ASSIGN
                        w-ord.ord-no = oe-ord.ord-no
                        w-ord.rec-id = RECID(oe-ord).
                END.
            END.

            IF v-post THEN 
            DO:
                FIND FIRST oe-ordm
                    WHERE oe-ordm.company EQ inv-misc.company
                    AND oe-ordm.ord-no  EQ inv-misc.ord-no
                    AND oe-ordm.line    EQ inv-misc.line
                    AND oe-ordm.charge  EQ inv-misc.charge
                    NO-ERROR.
                IF AVAILABLE oe-ordm THEN 
                DO:
                    IF oe-ordm.bill EQ "P" THEN oe-ordm.bill = IF inv-misc.bill EQ "Y" THEN "I" ELSE "Y".

                    

                        IF oe-ordm.miscType EQ 1 THEN
                            FOR EACH est-prep
                                WHERE est-prep.company EQ oe-ordm.company
                                AND est-prep.est-no  EQ oe-ordm.est-no
                                AND est-prep.eqty    EQ oe-ordm.estPrepEqty
                                AND est-prep.line    EQ oe-ordm.estPrepLine
                                AND est-prep.code    EQ oe-ordm.charge
                                AND est-prep.simon   EQ "S"
                                AND est-prep.amtz    EQ 100:
                                IF oeprep-log THEN DELETE est-prep.
                                ELSE est-prep.simon = "N".
                            END.


                    
                END.

                IF inv-misc.bill EQ "Y" THEN 
                DO:
                    RUN ar/calctax2.p (inv-head.tax-gr, 
                        NO,
                        inv-misc.amt,
                        inv-misc.company,
                        inv-misc.inv-i-no,
                        OUTPUT dTax).
                
                    v-reduce-ord-bal = v-reduce-ord-bal + inv-misc.amt +
                        (IF inv-misc.tax THEN dTax ELSE 0).
                END.
              
                FIND FIRST ar-invl NO-LOCK
                    WHERE ar-invl.x-no EQ v-xno
                    AND ar-invl.line EQ v-xline + 1
                    NO-ERROR.
                IF NOT AVAILABLE ar-invl THEN CREATE ar-invl.
                {oe/invmpost.i}.

                /* gdm - 09290908 */ RUN get-lot-no.

                IF inv-misc.bill NE "Y" THEN ar-invl.bill = NO.
                IF cExportNk1 EQ "Sonoco" THEN RUN oe/sonofile.p (3,RECID(ar-invl)).
            END. /* v-post */
                    
        END. /* each inv-misc */

        /******************* DISCOUNT ITEMS ****************************************/
        ASSIGN
            v-post-disc   = v-post-disc   + dInvDisc
            v-post-disc-w = v-post-disc-w + dInvDisc-w.

        IF dInvDisc NE 0 THEN 
        DO:
            CREATE tt-report.
            ASSIGN
                tt-report.term-id = ""
                tt-report.key-01  = "work-disc"
                tt-report.key-02  = STRING(inv-head.inv-no,"999999")
                tt-report.key-05  = STRING(dInvDisc)
                tt-report.weight  = dInvDisc-w
                .
        END.

        /******************* TAX ITEMS *********************************************/
        IF inv-head.t-inv-Tax NE 0 THEN 
        DO:
            IF inv-head.tax-gr NE "" THEN 
            DO:
                IF inv-head.multi-invoice THEN 
                DO:
                    FOR EACH b-inv-head
                        WHERE b-inv-head.company       EQ inv-head.company
                        AND b-inv-head.cust-no       EQ inv-head.cust-no
                        AND b-inv-head.inv-no        EQ inv-head.inv-no
                        AND b-inv-head.multi-invoice EQ NO:
                        RUN calc-tax-gr (ROWID(b-inv-head), inv-head.inv-no).
                    END.

                END.
                ELSE 
                    RUN calc-tax-gr (ROWID(inv-head), inv-head.inv-no).

            END.

            ELSE 
            DO:
                FIND FIRST account NO-LOCK
                    WHERE account.company EQ cocode
                    AND account.actnum  EQ v-ar-stax
                    NO-ERROR.
                CREATE tt-report.
                ASSIGN
                    tt-report.term-id = ""
                    tt-report.key-01  = "work-tax"
                    tt-report.key-02  = account.actnum
                    tt-report.key-03  = STRING(inv-head.inv-no,"999999")
                    tt-report.key-05  = STRING(inv-head.t-inv-tax *
                                        (IF AVAILABLE currency  THEN
                                           currency.ex-rate ELSE 1))
                    tt-report.weight  = dLineTot-w
                    .
            END.
        END.

        IF NOT v-post THEN 
        DO:
            fDebugMsg("List Post, not v-post, create ttInvoicePostUpdateGL Recs") .
            ASSIGN
                ld-t[2] = dLineTot-w / 2000
                ld-t[3] = ld-t[3] + dLineTot-w
                ld-pton = inv-head.t-inv-rev / ld-t[2]
                .

            IF ld-pton EQ ? THEN ld-pton = 0.

            IF NOT v-detail THEN 
            DO:
         
                /* Create temp-table without detail info */
                FIND FIRST ttInvoicePostUpdateGL
                  WHERE ttInvoicePostUpdateGL.invNo             = inv-head.inv-no
                    AND ttInvoicePostUpdateGL.invDate           = inv-head.inv-date
                    AND ttInvoicePostUpdateGL.custNo            = inv-head.cust-no
                    AND ttInvoicePostUpdateGL.custName          = inv-head.cust-name
                    AND ttInvoicePostUpdateGL.orderNumber       = v-ord-no
                    AND  ttInvoicePostUpdateGL.invoiceQty        = iInvoiceQty
                    AND ttInvoicePostUpdateGL.totInvoicefreight = inv-head.t-inv-freight
                    AND ttInvoicePostUpdateGL.totInvoiceTax     = inv-head.t-inv-tax
                    AND ttInvoicePostUpdateGL.miscTot           = dMiscTotal
                    AND ttInvoicePostUpdateGL.lineTot           = dLineTot
                    AND ttInvoicePostUpdateGL.iInvRev           = inv-head.t-inv-rev
                    NO-ERROR .
                IF NOT AVAILABLE ttInvoicePostUpdateGL THEN DO:
                CREATE ttInvoicePostUpdateGL.
                ASSIGN
                    ttInvoicePostUpdateGL.invNo             = inv-head.inv-no
                    ttInvoicePostUpdateGL.invDate           = inv-head.inv-date
                    ttInvoicePostUpdateGL.custNo            = inv-head.cust-no
                    ttInvoicePostUpdateGL.custName          = inv-head.cust-name
                    ttInvoicePostUpdateGL.orderNumber       = v-ord-no
                    ttInvoicePostUpdateGL.invoiceQty        = iInvoiceQty
                    ttInvoicePostUpdateGL.totInvoicefreight = inv-head.t-inv-freight
                    ttInvoicePostUpdateGL.totInvoiceTax     = inv-head.t-inv-tax
                    ttInvoicePostUpdateGL.miscTot           = dMiscTotal
                    ttInvoicePostUpdateGL.lineTot           = dLineTot
                    ttInvoicePostUpdateGL.iInvRev           = inv-head.t-inv-rev
                    .
                IF lPrintTon THEN 
                    ASSIGN 
                        ttInvoicePostUpdateGL.weightPerTon = ld-t[2]
                        ttInvoicePostUpdateGL.pricePerTon  = ld-pton
                        .
                 END.
            END.
          
            ELSE 
            DO:
                /* If v-detail */
                FOR EACH w-inv-line BREAK BY w-inv-line.ord-no:
                    IF lPrintTon THEN 
                    DO WITH FRAME invlt:
                        ASSIGN
                            ld-t[1] = w-inv-line.weight / 2000
                            ld-pton = w-inv-line.t-price / ld-t[1].

                        IF ld-pton EQ ? THEN ld-pton = 0.
                        dProfit = (w-inv-line.t-price - w-inv-line.t-cost) / w-inv-line.t-price * 100.
       
                    END.
                    ELSE
                    DO WITH FRAME invl:
                        dProfit = (w-inv-line.t-price - w-inv-line.t-cost) / w-inv-line.t-price * 100.
                
                    END.
                    dprofit = dProfit / 100.
                    
                FIND FIRST ttInvoicePostUpdateGL
                  WHERE ttInvoicePostUpdateGL.invNo             = inv-head.inv-no
                    AND ttInvoicePostUpdateGL.invDate           = inv-head.inv-date
                    AND ttInvoicePostUpdateGL.custNo            = inv-head.cust-no
                    AND ttInvoicePostUpdateGL.custName          = inv-head.cust-name
                    AND ttInvoicePostUpdateGL.orderNumber       = v-ord-no
                    AND  ttInvoicePostUpdateGL.invoiceQty        = iInvoiceQty
                    AND ttInvoicePostUpdateGL.totInvoicefreight = inv-head.t-inv-freight
                    AND ttInvoicePostUpdateGL.totInvoiceTax     = inv-head.t-inv-tax
                    AND ttInvoicePostUpdateGL.miscTot           = dMiscTotal
                    AND ttInvoicePostUpdateGL.lineTot           = dLineTot
                    AND ttInvoicePostUpdateGL.iInvRev           = inv-head.t-inv-rev
                    AND ttInvoicePostUpdateGL.iNo               = w-inv-line.i-no
                    AND ttInvoicePostUpdateGL.iName             = w-inv-line.i-name
                    AND ttInvoicePostUpdateGL.qty               = w-inv-line.qty
                    AND ttInvoicePostUpdateGL.invQty            = w-inv-line.inv-qty
                    AND ttInvoicePostUpdateGL.shipQty           = w-inv-line.ship-qty
                    AND ttInvoicePostUpdateGL.price             = w-inv-line.price
                    AND ttInvoicePostUpdateGL.uom               = w-inv-line.uom
                    AND ttInvoicePostUpdateGL.TotPrice          = w-inv-line.t-price
                    AND ttInvoicePostUpdateGL.profit            = dProfit                    
                    NO-ERROR .
                IF NOT AVAILABLE ttInvoicePostUpdateGL THEN DO:                    
                    CREATE ttInvoicePostUpdateGL.
                    ASSIGN
                        ttInvoicePostUpdateGL.invNo             = inv-head.inv-no
                        ttInvoicePostUpdateGL.invDate           = inv-head.inv-date
                        ttInvoicePostUpdateGL.custNo            = inv-head.cust-no
                        ttInvoicePostUpdateGL.custName          = inv-head.cust-name
                        ttInvoicePostUpdateGL.orderNumber       = v-ord-no
                        ttInvoicePostUpdateGL.invoiceQty        = iInvoiceQty
                        ttInvoicePostUpdateGL.totInvoicefreight = inv-head.t-inv-freight
                        ttInvoicePostUpdateGL.totInvoiceTax     = inv-head.t-inv-tax
                        ttInvoicePostUpdateGL.miscTot           = dMiscTotal
                        ttInvoicePostUpdateGL.lineTot           = dLineTot
                        ttInvoicePostUpdateGL.iInvRev           = inv-head.t-inv-rev
                        /*                ttInvoicePostUpdateGL.weightPerTon      = inv-head.weightPerTon*/
                        /*                ttInvoicePostUpdateGL.pricePerTon       = inv-head.pricePerTon */
                        ttInvoicePostUpdateGL.iNo               = w-inv-line.i-no
                        ttInvoicePostUpdateGL.iName             = w-inv-line.i-name
                        ttInvoicePostUpdateGL.qty               = w-inv-line.qty
                        ttInvoicePostUpdateGL.invQty            = w-inv-line.inv-qty
                        ttInvoicePostUpdateGL.shipQty           = w-inv-line.ship-qty
                        ttInvoicePostUpdateGL.price             = w-inv-line.price
                        ttInvoicePostUpdateGL.uom               = w-inv-line.uom
                        ttInvoicePostUpdateGL.TotPrice          = w-inv-line.t-price
                        ttInvoicePostUpdateGL.profit            = dProfit
                        .


                    IF lPrintTon THEN 
                        ASSIGN 
                            ttInvoicePostUpdateGL.weightPerTon = ld-t[1] 
                            ttInvoicePostUpdateGL.pricePerTon  = ld-pton
                            .
                   END. /* if createing temp table record */
                    DELETE w-inv-line.

                END.

                FOR EACH w-ord-misc BREAK BY w-ord-misc.ord-no WITH FRAME invm:
       
                    DELETE w-ord-misc.
                END. /* each w-inv-line */
            END.

        END. /* not v-post */

        ASSIGN
            v-post-total   = v-post-total   + inv-head.t-inv-rev
            v-post-total-w = v-post-total-w + dLineTot-w.

        IF AVAILABLE currency THEN 
        DO:
            CREATE tt-report.
            ASSIGN
                tt-report.term-id = ""
                tt-report.key-01  = "work-curr"
                tt-report.key-02  = currency.ar-ast-acct
                tt-report.key-05  = STRING(((inv-head.t-inv-rev * currency.ex-rate) -
                                       inv-head.t-inv-rev) * -1).
        END.

        v-tot-frt = 0.
        IF inv-head.multi-invoice THEN
            FOR EACH b-inv-head
                WHERE b-inv-head.company       EQ inv-head.company
                AND b-inv-head.cust-no       EQ inv-head.cust-no
                AND b-inv-head.inv-no        EQ inv-head.inv-no
                AND b-inv-head.multi-invoice EQ NO:
  
                IF b-inv-head.f-bill AND b-inv-head.t-inv-freight NE 0 THEN 
                    v-tot-frt = v-tot-frt + b-inv-head.t-inv-freight *
                        (IF AVAILABLE currency THEN currency.ex-rate ELSE 1).
            END.
        ELSE
            IF inv-head.f-bill THEN
                v-tot-frt = inv-head.t-inv-freight *
                    (IF AVAILABLE currency THEN currency.ex-rate ELSE 1).
        /** if Freight Is Billable then Post to GL **/
        IF v-tot-frt NE 0 THEN 
        DO:
            dTempAmount = v-tot-frt.
            ASSIGN
                v-post-freight   = v-post-freight   - dTempAmount
                v-post-freight-w = v-post-freight-w - dLineTot-w
                v-reduce-ord-bal = v-reduce-ord-bal + v-tot-frt
                .

            CREATE tt-report.
            ASSIGN
                tt-report.term-id = ""
                tt-report.key-01  = "work-freight"
                tt-report.key-02  = STRING(inv-head.inv-no,"999999")
                tt-report.key-05  = STRING(- dTempAmount)
                tt-report.weight  = - dLineTot-w
                .
        END.

        IF inv-head.terms EQ "CASH" AND inv-head.t-inv-rev NE 0 THEN 
        DO:
            ASSIGN
                v-post-cash    = v-post-cash    + inv-head.t-inv-rev
                v-post-total   = v-post-total   - inv-head.t-inv-rev
                v-post-cash-w  = v-post-cash-w  + dLineTot-w
                v-post-total-w = v-post-total-w - dLineTot-w.

            CREATE tt-report.
            ASSIGN
                tt-report.term-id = ""
                tt-report.key-01  = "work-cash"
                tt-report.key-02  = STRING(inv-head.inv-no,"999999")
                tt-report.key-05  = STRING(inv-head.t-inv-rev)
                tt-report.weight  = dLineTot-w.
        END.

        IF v-post THEN 
        DO:
            RUN oe/invcust.p (RECID(inv-head), v-ord-no, dtPostDate, tran-period).

            IF AVAILABLE ar-inv THEN 
            DO:
                ASSIGN
                    ar-inv.ord-no   = v-ord-no
                    ar-inv.ord-date = v-ord-date.

                RUN oe/invcost.p (RECID(ar-inv)).

                RELEASE ar-inv.
            END.

            /* update loadtag status - Bill of lading task#: 10190414 */
            FOR EACH inv-line NO-LOCK OF inv-head ,
                EACH oe-boll NO-LOCK
                WHERE oe-boll.company EQ inv-line.company
                AND oe-boll.b-no    EQ inv-line.b-no
                AND oe-boll.ord-no  EQ inv-line.ord-no
                AND oe-boll.i-no    EQ inv-line.i-no
                AND oe-boll.line    EQ inv-line.line
                AND oe-boll.po-no   EQ inv-line.po-no
                AND CAN-FIND(FIRST oe-bolh
                WHERE oe-bolh.b-no   EQ inv-line.b-no
                AND oe-bolh.posted EQ YES)
                :

                IF oe-boll.tag GT "" THEN
                    FIND FIRST loadtag EXCLUSIVE-LOCK
                        WHERE loadtag.company EQ inv-head.company
                        AND loadtag.item-type EQ NO
                        AND loadtag.i-no    EQ inv-line.i-no
                        AND loadtag.job-no  EQ oe-boll.job-no
                        AND loadtag.job-no2 EQ oe-boll.job-no2
                        AND loadtag.tag-no  EQ oe-boll.tag
                        USE-INDEX tag NO-ERROR.
                ELSE IF oe-boll.job-no GT "" THEN
                        FIND FIRST loadtag EXCLUSIVE-LOCK
                            WHERE loadtag.company EQ inv-head.company
                            AND loadtag.item-type EQ NO
                            AND loadtag.i-no    EQ inv-line.i-no
                            AND loadtag.job-no  EQ oe-boll.job-no
                            AND loadtag.job-no2 EQ oe-boll.job-no2
                            AND loadtag.tag-no  EQ oe-boll.tag
                            USE-INDEX job-no NO-ERROR.
                    ELSE 
                        FIND FIRST loadtag EXCLUSIVE-LOCK
                            WHERE loadtag.company EQ inv-head.company
                            AND loadtag.item-type EQ NO
                            AND loadtag.i-no    EQ inv-line.i-no
                            AND loadtag.job-no  EQ oe-boll.job-no
                            AND loadtag.job-no2 EQ oe-boll.job-no2
                            AND loadtag.tag-no  EQ oe-boll.tag
                            USE-INDEX i-no NO-ERROR.

                IF AVAILABLE loadtag THEN
                DO:
                    loadtag.sts = "Completed".
                    FIND CURRENT loadtag NO-LOCK NO-ERROR.
                END.
            END.

            FOR EACH inv-line WHERE inv-line.r-no EQ inv-head.r-no USE-INDEX r-no
                BREAK BY inv-line.b-no:

                IF LAST-OF(inv-line.b-no) THEN
                    FOR EACH oe-boll NO-LOCK
                        WHERE oe-boll.company EQ inv-line.company
                        AND oe-boll.b-no    EQ inv-line.b-no
                        AND CAN-FIND(FIRST oe-bolh
                        WHERE oe-bolh.b-no   EQ oe-boll.b-no
                        AND oe-bolh.posted EQ YES)
                
                        BREAK BY oe-boll.r-no:

                        IF LAST-OF(oe-boll.r-no) THEN
                            FOR EACH oe-rell
                                WHERE oe-rell.company EQ oe-boll.company
                                AND oe-rell.r-no    EQ oe-boll.r-no
                                AND (oe-rell.posted EQ NO OR
                                NOT CAN-FIND(FIRST tmp-oe-boll
                                WHERE tmp-oe-boll.company  EQ oe-rell.company
                                AND tmp-oe-boll.r-no     EQ oe-rell.r-no
                                AND tmp-oe-boll.ord-no   EQ oe-rell.ord-no
                                AND tmp-oe-boll.i-no     EQ oe-rell.i-no
                                AND tmp-oe-boll.line     EQ oe-rell.line
                                AND tmp-oe-boll.rel-no   EQ oe-rell.rel-no
                                AND tmp-oe-boll.b-ord-no EQ oe-rell.b-ord-no
                                AND tmp-oe-boll.po-no    EQ oe-rell.po-no
                                USE-INDEX ord-no))
                                USE-INDEX r-no,

                                FIRST oe-relh NO-LOCK
                                WHERE oe-relh.r-no   EQ oe-rell.r-no
                                AND oe-relh.posted EQ YES
                                USE-INDEX r-no :

                                FOR EACH oe-rel
                                    WHERE oe-rel.company  EQ oe-rell.company
                                    AND oe-rel.link-no  EQ oe-rell.r-no
                                    AND oe-rel.ord-no   EQ oe-rell.ord-no
                                    AND oe-rel.i-no     EQ oe-rell.i-no
                                    AND oe-rel.line     EQ oe-rell.line
                                    AND oe-rel.rel-no   EQ oe-rell.rel-no
                                    AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
                                    AND oe-rel.po-no    EQ oe-rell.po-no
                                    USE-INDEX link:
                                    ASSIGN
                                        oe-rel.rel-no   = 0
                                        oe-rel.b-ord-no = 0
                                        oe-rel.link-no  = 0.
                                END.

                                DELETE oe-rell.
                            END.
                    END.

                DELETE inv-line.
            END.

            FOR EACH inv-misc WHERE inv-misc.r-no EQ inv-head.r-no USE-INDEX r-no:
                DELETE inv-misc.
            END.

            IF inv-head.multi-invoice THEN
                FOR EACH b-inv-head
                    WHERE b-inv-head.company       EQ inv-head.company
                    AND b-inv-head.cust-no       EQ inv-head.cust-no
                    AND b-inv-head.inv-no        EQ inv-head.inv-no
                    AND b-inv-head.multi-invoice EQ NO:

                    DELETE b-inv-head.
                END.

            ELSE DELETE inv-head.

            IF cPrintFormat NE "Fibre" THEN RUN post-gl.
        END. /* v-post */
        
    END.

    FIND CURRENT inv-head NO-LOCK NO-ERROR.
    FIND CURRENT inv-line NO-LOCK NO-ERROR.
    FIND CURRENT itemfg   NO-LOCK NO-ERROR.
    FIND CURRENT oe-ordl  NO-LOCK NO-ERROR.
    FIND CURRENT ar-invl  NO-LOCK NO-ERROR.
    FIND CURRENT oe-ordm  NO-LOCK NO-ERROR.
    FIND CURRENT cust     NO-LOCK NO-ERROR.

    SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

PROCEDURE post-gl :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lcAccountDscr LIKE gltrans.tr-dscr NO-UNDO. 
   
    fDebugMsg("Starting post-gl").
    /** POST TO GENERAL LEDGER ACCOUNTS TRANSACTION FILE **/
    DO TRANSACTION:
        FOR EACH tt-gl,
            FIRST gltrans WHERE ROWID(gltrans) EQ tt-gl.row-id:
            DELETE gltrans.
        END.

        EMPTY TEMP-TABLE tt-gl.
        /** POST LINE ITEMS TO G/L TRANS **/
        FOR EACH tt-report NO-LOCK
            WHERE tt-report.term-id EQ ""
            AND tt-report.key-01  EQ "work-line"
            
            BREAK BY tt-report.key-02
            BY tt-report.key-03:

            ACCUMULATE dec(tt-report.key-05) (TOTAL BY tt-report.key-03).

            IF LAST-OF(tt-report.key-03) THEN 
            DO:
                RUN get-tr-dscr (INT(tt-report.key-03), OUTPUT lcAccountDscr).
                fDebugMsg("Starting gl work-line entry " + lcAccountDscr).
                CREATE tt-gl.
                CREATE gltrans.
                ASSIGN
                    tt-gl.row-id    = ROWID(gltrans)
                    gltrans.company = cocode
                    gltrans.actnum  = tt-report.key-02
                    gltrans.jrnl    = "OEINV"
                    gltrans.tr-dscr = TRIM(lcAccountDscr) + " LINE"
                    gltrans.tr-date = dtPostDate
                    gltrans.tr-amt  = - (ACCUMULATE TOTAL BY tt-report.key-03 dec(tt-report.key-05))
                    gltrans.period  = tran-period
                    gltrans.trnum   = v-trnum
                    .
                RELEASE gltrans.
            END. /* last actnum */
        END. /* each work-line */

        /** POST MISC. TO G/L TRANS **/
        FOR EACH tt-report NO-LOCK
            WHERE tt-report.term-id EQ ""
            AND tt-report.key-01  EQ "work-misc"
            
            BREAK BY tt-report.key-02
            BY tt-report.key-03:

            ACCUMULATE dec(tt-report.key-05) (TOTAL BY tt-report.key-03).

            IF LAST-OF(tt-report.key-03) THEN 
            DO:
                RUN get-tr-dscr (INT(tt-report.key-03), OUTPUT lcAccountDscr).

                CREATE tt-gl.
                CREATE gltrans.
                ASSIGN
                    tt-gl.row-id    = ROWID(gltrans)
                    gltrans.company = cocode
                    gltrans.jrnl    = "OEINV"
                    gltrans.tr-dscr = TRIM(lcAccountDscr) + " MISC"
                    gltrans.tr-date = dtPostDate
                    gltrans.actnum  = tt-report.key-02
                    gltrans.tr-amt  = - (ACCUMULATE TOTAL BY tt-report.key-03 dec(tt-report.key-05))
                    gltrans.period  = tran-period
                    gltrans.trnum   = v-trnum
                    .
            END. /* last actnum */
        END. /* each work-misc */

        /** POST SALES TAX TO G/L TRANS **/
        FOR EACH tt-report NO-LOCK
            WHERE tt-report.term-id EQ ""
            AND tt-report.key-01  EQ "work-tax"
            
            BREAK BY tt-report.key-02
            BY tt-report.key-03:

            ACCUMULATE dec(tt-report.key-05) (TOTAL BY tt-report.key-03).

            IF LAST-OF(tt-report.key-03) THEN 
            DO:
                RUN get-tr-dscr (INT(tt-report.key-03), OUTPUT lcAccountDscr).

                CREATE tt-gl.
                CREATE gltrans.
                ASSIGN
                    tt-gl.row-id    = ROWID(gltrans)
                    gltrans.company = cocode
                    gltrans.actnum  = tt-report.key-02
                    gltrans.jrnl    = "OEINV"
                    gltrans.tr-dscr = TRIM(lcAccountDscr) + " TAX"
                    gltrans.tr-date = dtPostDate
                    gltrans.tr-amt  = - (ACCUMULATE TOTAL BY tt-report.key-03 dec(tt-report.key-05))
                    gltrans.period  = tran-period
                    gltrans.trnum   = v-trnum
                    .
                RELEASE gltrans.
            END. /* last actnum */
        END. /* each work-tax */

        /** POST CURRENCY TO G/L TRANS **/
        FOR EACH tt-report NO-LOCK
            WHERE tt-report.term-id EQ ""
            AND tt-report.key-01  EQ "work-curr"
            
            BREAK BY tt-report.key-02:

            ACCUMULATE dec(tt-report.key-05) (TOTAL BY tt-report.key-02).

            IF LAST-OF(tt-report.key-02) THEN 
            DO:
                CREATE tt-gl.
                CREATE gltrans.
                ASSIGN
                    tt-gl.row-id    = ROWID(gltrans)
                    gltrans.company = cocode
                    gltrans.actnum  = tt-report.key-02
                    gltrans.jrnl    = "OEINV"
                    gltrans.tr-dscr = "ORDER ENTRY INVOICE CURRENCY GAIN/LOSS"
                    gltrans.tr-date = dtPostDate
                    gltrans.tr-amt  = - (ACCUMULATE TOTAL BY tt-report.key-02 dec(tt-report.key-05))
                    gltrans.period  = tran-period
                    gltrans.trnum   = v-trnum
                    .

                RELEASE gltrans.
            END. /* last actnum */
        END. /* each work-tax */

        FOR EACH tmp-work-job
            BREAK BY tmp-work-job.fg
            BY tmp-work-job.actnum
            BY tmp-work-job.inv-no:

            ACCUMULATE tmp-work-job.amt (TOTAL BY tmp-work-job.inv-no).

            IF LAST-OF(tmp-work-job.inv-no) THEN 
            DO:
                RUN get-tr-dscr (tmp-work-job.inv-no, OUTPUT lcAccountDscr).

                CREATE tt-gl.
                CREATE gltrans.
                ASSIGN
                    tt-gl.row-id    = ROWID(gltrans)
                    gltrans.company = cocode
                    gltrans.actnum  = tmp-work-job.actnum
                    gltrans.jrnl    = "OEINV"
                    gltrans.tr-date = dtPostDate
                    gltrans.period  = tran-period
                    gltrans.trnum   = v-trnum
                    .

                IF tmp-work-job.fg THEN
                    ASSIGN
                        gltrans.tr-amt  = - (ACCUMULATE TOTAL BY tmp-work-job.inv-no tmp-work-job.amt)
                        gltrans.tr-dscr = TRIM(lcAccountDscr) + " FG".
                ELSE
                    ASSIGN
                        gltrans.tr-amt  = (ACCUMULATE TOTAL BY tmp-work-job.inv-no tmp-work-job.amt)
                        gltrans.tr-dscr = TRIM(lcAccountDscr) + " COGS".

                RELEASE gltrans.
            END.
        END. /* each work-job */

        /** POST FREIGHT TO G/L TRANS **/
        CREATE tt-gl.
        CREATE gltrans.
        ASSIGN
            tt-gl.row-id    = ROWID(gltrans)
            gltrans.company = cocode
            gltrans.actnum  = v-ar-freight
            gltrans.jrnl    = "OEINV"
            gltrans.tr-dscr = "ORDER ENTRY INVOICE FREIGHT"
            gltrans.tr-date = dtPostDate 
            gltrans.tr-amt  = v-post-freight
            gltrans.period  = tran-period
            gltrans.trnum   = v-trnum
            .
        RELEASE gltrans. 

        /** POST DISCOUNT TO G/L TRANS **/
        CREATE tt-gl.
        CREATE gltrans.
        ASSIGN
            tt-gl.row-id    = ROWID(gltrans) 
            gltrans.company = cocode
            gltrans.actnum  = v-ar-disc
            gltrans.jrnl    = "OEINV"
            gltrans.tr-dscr = "ORDER ENTRY INVOICE DISCOUNT"
            gltrans.tr-date = dtPostDate
            gltrans.tr-amt  = v-post-disc
            gltrans.period  = tran-period
            gltrans.trnum   = v-trnum
            .
        RELEASE gltrans.

        /** POST CASH TO G/L TRANS **/
        IF v-post-cash NE 0 THEN 
        DO:
            CREATE tt-gl.
            CREATE gltrans.
            ASSIGN
                tt-gl.row-id    = ROWID(gltrans)
                gltrans.company = cocode
                gltrans.actnum  = ar-ctrl.cash-act
                gltrans.jrnl    = "CASHR"
                gltrans.tr-dscr = "CASH RECEIPT - INVOICE"
                gltrans.tr-date = dtPostDate
                gltrans.tr-amt  = v-post-cash
                gltrans.period  = tran-period
                gltrans.trnum   = v-trnum
                v-post-cash     = - v-post-cash
                .
            RELEASE gltrans.
        END.

        /** OFFSET ENTRY TO G/L **/
        CREATE tt-gl.
        CREATE gltrans.
        ASSIGN
            tt-gl.row-id    = ROWID(gltrans)
            gltrans.company = cocode
            gltrans.actnum  = v-ar-acct
            gltrans.jrnl    = "OEINV"
            gltrans.tr-dscr = "ORDER ENTRY INVOICE"
            gltrans.tr-date = dtPostDate
            gltrans.tr-amt  = v-post-total
            gltrans.period  = tran-period
            gltrans.trnum   = v-trnum
            .
        RELEASE gltrans.
    END.
END PROCEDURE.

PROCEDURE pPrintPost:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-post      AS LOG       NO-UNDO.
    DEFINE VARIABLE v-close-line AS LOG       NO-UNDO.
    DEFINE VARIABLE cStatus      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReason      AS CHARACTER NO-UNDO.

    DO TRANSACTION:       /** GET next G/L TRANS. POSTING # **/
        REPEAT:
            FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
            IF AVAILABLE gl-ctrl THEN DO:
                ASSIGN 
                    v-trnum       = gl-ctrl.trnum + 1 
                    gl-ctrl.trnum = v-trnum.
                FIND CURRENT gl-ctrl NO-LOCK.
                LEAVE.
            END. /* IF AVAIL gl-ctrl */
        END. /* REPEAT */
    END.
    
    fDebugMsg("Start Run Report").
    RUN run-report.
    fDebugMsg("After Run Report - lPostable? " + STRING(lPostable)).
    
    IF lPostable THEN DO:
        IF v-balance = 0 THEN lv-post = YES.
        fDebugMsg("After Run Report - lv-post? " + STRING(lv-post) + " v-post " + STRING(v-post) + " lpost " + STRING(lPost)).
        IF lv-post /* AND v-post (here by mistake?) */ THEN DO:
            RUN list-post-inv ("post").
            RUN post-gl.
            RUN copy-report-to-audit-dir.
            FOR EACH tt-report:
                DELETE tt-report.
            END.
            EMPTY TEMP-TABLE tt-gl.
            
            /* Taken out for auto post */
            /*            IF cExportNk1 EQ "Sonoco" THEN                                        */
            /*                RUN ar/sonoinv.p ("total", iTotRecsWritten, OUTPUT iRecsWritten).*/
          
            FOR EACH w-ord BREAK BY w-ord.ord-no:
                IF NOT FIRST-OF(w-ord.ord-no) THEN DELETE w-ord.
            END.

            order-close1:
            FOR EACH w-ord,
                FIRST oe-ord NO-LOCK
                WHERE oe-ord.company EQ cocode
                AND oe-ord.ord-no  EQ w-ord.ord-no
                BREAK BY oe-ord.cust-no:

                RELEASE cust.
                RUN oe/calcordt.p (ROWID(oe-ord)).
                IF LAST-OF(oe-ord.cust-no) THEN DO:
                    FIND FIRST tt-custbal NO-LOCK WHERE tt-custbal.cust-no EQ oe-ord.cust-no
                        NO-ERROR.
                    IF NOT AVAILABLE tt-custbal THEN DO:
                        CREATE tt-custbal.
                        ASSIGN 
                            tt-custbal.cust-no = oe-ord.cust-no.
                    END.

                    FIND FIRST cust NO-LOCK /* EXCLUSIVE */
                         WHERE cust.company EQ oe-ord.company
                           AND cust.cust-no EQ oe-ord.cust-no
                         NO-ERROR.
                    IF AVAILABLE cust THEN DO:
                        RUN ar/updcust1.p (NO, BUFFER cust, OUTPUT tt-custbal.ord-bal).
                        FIND CURRENT cust NO-LOCK.
                    END.
                END.
            END. /* Each w-ord */
      
            cust-bal:
            FOR EACH tt-custbal,
                FIRST cust EXCLUSIVE-LOCK
                WHERE cust.company EQ cocode
                  AND cust.cust-no EQ tt-custbal.cust-no
                :
                cust.ord-bal = tt-custbal.ord-bal.
                IF cust.ord-bal LT 0 THEN cust.ord-bal = 0.
            END.

            order-close2:
            FOR EACH w-ord,
                FIRST oe-ord NO-LOCK
                WHERE oe-ord.company EQ cocode
                AND oe-ord.ord-no  EQ w-ord.ord-no
                BREAK BY oe-ord.cust-no:

                RELEASE cust.

                FOR EACH oe-ordl NO-LOCK WHERE
                    oe-ordl.company EQ oe-ord.company AND
                    oe-ordl.ord-no  EQ oe-ord.ord-no AND
                    oe-ordl.stat    NE "C"
                    :
                    /* No UI */
                    RUN oe/CloseOrder.p(INPUT ROWID(oe-ordl),
                        INPUT NO,
                        OUTPUT cStatus,
                        OUTPUT cReason).
                    /* No UI */
                    IF cStatus EQ 'C' THEN
                        RUN oe/closelin.p (INPUT ROWID(oe-ordl),YES).
                END.

                RUN close-order.
            END. /* Each w-ord */
            

/*            IF oeclose-log THEN DO:                           */
/*                RUN oe/closchkinv.p (0).                      */
/*            /* Contains UI, so taken out for batch mode  */   */
/*            /*                IF CAN-FIND (FIRST w-ord) THEN*/*/
/*            /*                    RUN oe/d-close.w.         */*/
/*            END.                                              */
        END.
    END.

    FOR EACH save-line WHERE save-line.reftable EQ "save-line" + STRING(v-trnum,"9999999999"):
        RUN undo-save-line.
    END.

    IF NOT lPostable OR NOT lv-post THEN 
    DO TRANSACTION: 
        REPEAT:
            FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
            IF AVAILABLE gl-ctrl THEN DO:
                IF gl-ctrl.trnum EQ v-trnum THEN gl-ctrl.trnum = v-trnum - 1.
                FIND CURRENT gl-ctrl NO-LOCK.
                LEAVE.
            END. /* IF AVAIL gl-ctrl */
        END. /* REPEAT */
    END.

    IF lFtpDone THEN MESSAGE "File Export/FTP is completed." VIEW-AS ALERT-BOX INFORMATION.

END PROCEDURE.
	
PROCEDURE run-report :
    /* ---------------------------------------------------- oe/invpost.p 10/94 gb */
    /* Invoicing  - Edit Register & Post Invoicing Transactions                   */
    /* -------------------------------------------------------------------------- */
    DEFINE BUFFER xinv-head FOR inv-head.
    DEFINE VARIABLE str-tit4                AS CHARACTER FORMAT "x(20)" NO-UNDO.
    DEFINE VARIABLE lv-label-ton            AS CHARACTER FORMAT "x(20)" EXTENT 2 NO-UNDO.
    DEFINE VARIABLE v-contsrvc-export-found AS LOG       NO-UNDO.
    DEFINE VARIABLE v-goodman-export-found  AS LOG       NO-UNDO.

 
    FIND FIRST period NO-LOCK                 
        WHERE period.company EQ gcompany
        AND period.pst     LE dtPostDate
        AND period.pend    GE dtPostDate
        NO-ERROR.
         
    lPostable= NO.

    EMPTY TEMP-TABLE w-report.

    fDebugMsg("Run Report - Begin For Each") .
    FOR EACH inv-head NO-LOCK
        WHERE inv-head.company  EQ cocode
          AND inv-head.printed  EQ YES
          AND inv-head.inv-no   GT 0
          AND inv-head.inv-no   GE iStartInvNo
          AND inv-head.inv-no   LE iEndInvNo
          AND inv-head.inv-date GE dtStartInvoiceDate
          AND inv-head.cust-no  GE cStartCustNo
          AND inv-head.cust-no  LE cEndCustNo
          AND inv-head.inv-date LE dtEndInvoiceDate
          AND inv-head.stat     NE "H"
        USE-INDEX prnt,
        FIRST cust NO-LOCK
        WHERE cust.company EQ cocode
          AND cust.cust-no EQ inv-head.cust-no
          AND ((cust.inv-meth EQ ?
          AND inv-head.multi-invoice)
           OR (cust.inv-meth NE ?
          AND NOT inv-head.multi-invoice))
        TRANSACTION:
        IF lCustList AND
           NOT CAN-FIND(FIRST ttCustList
                        WHERE ttCustList.cust-no EQ inv-head.cust-no
                          AND ttCustList.log-fld EQ TRUE) THEN
        NEXT.
        FIND FIRST xinv-head EXCLUSIVE-LOCK
             WHERE RECID(xinv-head) EQ recid(inv-head)
             NO-WAIT NO-ERROR.
        IF AVAILABLE xinv-head THEN DO:
            CREATE w-report.
            ASSIGN
                w-report.term-id = ""
                w-report.key-01  = STRING(xinv-head.inv-no,"9999999999")
                w-report.rec-id  = RECID(xinv-head).
            CREATE tt-report.
            ASSIGN
                tt-report.term-id = ""
                tt-report.key-01  = STRING(xinv-head.inv-no,"9999999999")
                tt-report.rec-id  = RECID(xinv-head).
           
            IF inv-head.multi-invoice THEN
                IF CAN-FIND(FIRST b-inv-head
                    WHERE b-inv-head.company     EQ inv-head.company
                        AND b-inv-head.cust-no       EQ inv-head.cust-no
                        AND b-inv-head.inv-no        EQ inv-head.inv-no
                        AND b-inv-head.multi-invoice EQ NO) THEN
                    FOR EACH b-inv-head NO-LOCK
                        WHERE b-inv-head.company     EQ inv-head.company
                            AND b-inv-head.cust-no       EQ inv-head.cust-no
                            AND b-inv-head.inv-no        EQ inv-head.inv-no
                            AND b-inv-head.multi-invoice EQ NO:
                        RUN create-save-line.
                    END.
                ELSE DO:
                    DELETE tt-report.
                    DELETE w-report.
                    DELETE xinv-head.
                    NEXT.
                END.
                                
              IF cust.factored THEN
                FOR EACH inv-line NO-LOCK WHERE inv-line.r-no = inv-head.r-no:
                    IF CAN-FIND(FIRST itemfg WHERE itemfg.company  EQ inv-head.company
                        AND itemfg.i-no     EQ inv-line.i-no
                        AND itemfg.factored = yes)
                     THEN DO:
           
                        tt-report.key-02 = "Factored".  /* for oe/rep/expfrank.p task#  09200521*/
                        LEAVE.
                    END.
                END.
        END.
    END.

    cCListName = cListName.
    fDebugMsg("Run Report - run list-post-inv - list") .
    RUN list-post-inv ("list").
  
    /* wfk - taking out for batch report - export invoices to factor */   
    lFtpDone = NO.

    IF FALSE AND  inexport-log THEN DO:    
        DEFINE VARIABLE v-exp-file AS cha NO-UNDO.
        v-exp-file = inexport-desc +  
            "INVOICE_" + 
            substr(STRING(YEAR(TODAY),"9999"),3,2) +
            string(MONTH(TODAY),"99") +
            string(DAY(TODAY),"99") +
            substr(STRING(TIME,"HH:MM:SS"),1,2) +
            substr(STRING(TIME,"HH:MM:SS"),4,2) +
            substr(STRING(TIME,"HH:MM:SS"),7,2) + ".dat".

        IF (cPrintFormat = "Frankstn" OR cPrintFormat = "MIRPKG" ) AND inexport-cha EQ "CIT" THEN DO:
            OUTPUT TO VALUE(v-exp-file).
            RUN oe/rep/expfrank.p .
            OUTPUT CLOSE.
            OUTPUT TO VALUE(".\oe\ftpcmd2.txt").     /* ftp text file */
            PUT UNFORMATTED 
                "open cs.ftp.citonline.com" SKIP  /* ftp server ip address */
                "ftpa1526" SKIP  /* userid*/
                "none" SKIP  /* password */
                "put " v-exp-file " " '"' "$$ ID=EP003F BID='DI1526' PASSWORD=NARF" '"' SKIP         /* file to transfer */
                "quit" .
            OUTPUT CLOSE.
            OS-COMMAND SILENT VALUE("ftp -v -i -s:.\oe\ftpcmd2.txt"). 
            lFtpDone = YES.
        END.
        ELSE
            IF inexport-cha EQ "ContSrvc" THEN DO:
                lFtpDone = YES.

                FOR EACH tt-report NO-LOCK 
                    WHERE tt-report.term-id EQ "",
                    FIRST inv-head NO-LOCK WHERE RECID(inv-head) EQ tt-report.rec-id ,
                        FIRST cust NO-LOCK 
                            WHERE cust.company = inv-head.company AND
                            cust.cust-no = inv-head.cust-no AND
                            cust.an-edi-cust AND
                            NOT CAN-FIND(FIRST sys-ctrl-shipto WHERE
                                sys-ctrl-shipto.company EQ cust.company AND
                                sys-ctrl-shipto.NAME EQ "INEXPORT" AND
                                sys-ctrl-shipto.cust-vend EQ YES AND
                                sys-ctrl-shipto.cust-vend-no EQ cust.cust-no AND
                                sys-ctrl-shipto.char-fld EQ "GOODMAN")
                    :

                    v-contsrvc-export-found = YES.
                    LEAVE.
                END.

                IF v-contsrvc-export-found THEN DO:
                    OUTPUT TO VALUE(v-exp-file).
                    RUN oe/rep/expconts.p .
                    OUTPUT CLOSE.
                END.

                FOR EACH tt-report NO-LOCK WHERE tt-report.term-id EQ "",
                    FIRST inv-head NO-LOCK WHERE RECID(inv-head) EQ tt-report.rec-id ,
                    FIRST cust NO-LOCK 
                          WHERE cust.company = inv-head.company AND
                                cust.cust-no = inv-head.cust-no AND
                                cust.an-edi-cust AND
                                CAN-FIND(FIRST sys-ctrl-shipto WHERE
                                sys-ctrl-shipto.company EQ cust.company AND
                                sys-ctrl-shipto.NAME EQ "INEXPORT" AND
                                sys-ctrl-shipto.cust-vend EQ YES AND
                                sys-ctrl-shipto.cust-vend-no EQ cust.cust-no AND
                                sys-ctrl-shipto.char-fld EQ "GOODMAN")
                    :
                    v-goodman-export-found = YES.
                    LEAVE.
                END.

                IF v-goodman-export-found THEN DO:
                    v-exp-file = inexport-desc +  
                        "INVOICEG_" + 
                        substr(STRING(YEAR(TODAY),"9999"),3,2) +
                        string(MONTH(TODAY),"99") +
                        string(DAY(TODAY),"99") +
                        substr(STRING(TIME,"HH:MM:SS"),1,2) +
                        substr(STRING(TIME,"HH:MM:SS"),4,2) +
                        substr(STRING(TIME,"HH:MM:SS"),7,2) + ".dat".
                    OUTPUT TO VALUE(v-exp-file).
                    RUN oe/rep/expgoodman.p .
                    OUTPUT CLOSE.
                END.
            END.
    END.
    /* end of export */
    fDebugMsg("Run Report - if lpostable run list-gl " + STRING(lPostable)) .
    IF lPostable THEN RUN list-gl.
    EMPTY TEMP-TABLE tt-report.
END PROCEDURE.

PROCEDURE undo-save-line :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DISABLE TRIGGERS FOR LOAD OF inv-line.
    DISABLE TRIGGERS FOR LOAD OF inv-misc.

    RELEASE inv-line.
    RELEASE inv-misc.

    FIND FIRST inv-line EXCLUSIVE-LOCK WHERE RECID(inv-line) EQ INT(save-line.val[3]) NO-ERROR.
    IF AVAILABLE inv-line THEN inv-line.r-no = save-line.val[1].
    ELSE
    FIND FIRST inv-misc EXCLUSIVE-LOCK WHERE RECID(inv-misc) EQ INT(save-line.val[3]) NO-ERROR.
    IF AVAILABLE inv-misc THEN inv-misc.r-no = save-line.val[1].
    DELETE save-line.
END PROCEDURE.

{aoa/BL/pBuildCustList.i}

/* ************************  Function Implementations ***************** */

FUNCTION fDebugMsg RETURNS CHARACTER 
	( INPUT ipcMessage AS CHARACTER  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	
        DEFINE VARIABLE result AS CHARACTER NO-UNDO.
        IF lUseLogs THEN DO:
            OUTPUT STREAM sDebug CLOSE. OUTPUT STREAM sDebug TO VALUE(cDebugLog) append.
            PUT STREAM sDebug UNFORMATTED ipcMessage SKIP.
        END.
        
        RETURN result.
END FUNCTION.

