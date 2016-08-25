/*------------------------------------------------------------------------

  File: r-bole&p.w

  Description: BOL Edit List & Posting

    1) handle lUserChoice to print exception report
    2) handle email capability
    3) create capability to handle PROCEDURE email-reorderitems
    4) Create capability for: BOL posting Exception Report 
    5) PROCEDURE email-reorderitems :
    6) oe/bol-pre-post.p  is missing
    7) is lPost an input parameter?
    8) oe-bolp3 depends on created report records, but where are they created?
    9) implment delete logic on oe-bolh marked as deleted

*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttPostBOLCreateInvoice NO-UNDO
    {aoaAppSrv/ttFields.i}
    FIELD bolDate AS DATE      LABEL "Date" FORMAT "99/99/9999"
    FIELD bolNo   AS INTEGER   LABEL "BOL.#" FORMAT ">>>>>>>>9"
    FIELD carrier AS CHARACTER LABEL "Carrier" FORMAT "x(5)"
    FIELD trailer AS CHARACTER LABEL "Trailer" FORMAT "x(20)"
    FIELD freight AS DECIMAL   LABEL "Freight" FORMAT ">>,>>9.99"
    FIELD cwt     AS DECIMAL   LABEL "Rate" FORMAT ">>9.99"
    FIELD totWgt  AS DECIMAL   LABEL "Tot WT" FORMAT ">>,>>9.99"
    FIELD custNo  AS CHARACTER LABEL "Cust#" FORMAT "x(5)"
    FIELD shipID  AS CHARACTER LABEL "Ship#" FORMAT "x(8)"
    FIELD deleted AS LOGICAL   LABEL "Deleted" FORMAT "YES/NO"
    FIELD iNo     AS CHARACTER LABEL "Item#" FORMAT "x(1)"
    FIELD iName   AS CHARACTER LABEL "Item Name" FORMAT "x(1)"
    FIELD poNo    AS CHARACTER LABEL "P.O. #" FORMAT "x(1)"
    FIELD ordNo   AS INTEGER   LABEL "Ord#" FORMAT ">>>>>>"
    FIELD relNo   AS INTEGER   LABEL "Rel.#" FORMAT ">>>>>9"
    FIELD bOrdNo  AS INTEGER   LABEL "B-Ord" FORMAT ">>>>>>>9"
    FIELD loc     AS CHARACTER LABEL "Whse" FORMAT "x(1)"
    FIELD locBin  AS CHARACTER LABEL "Bin Loc" FORMAT "x(8)"
    FIELD tag     AS CHARACTER LABEL "Tag" FORMAT "x(1)"
    FIELD cases   AS INTEGER   LABEL "Cases" FORMAT "->>>,>>9"
    FIELD qtyCase AS INTEGER   LABEL "Qty/Case" FORMAT "->>>,>>9"
    FIELD partial AS DECIMAL   LABEL "Partial" FORMAT ">>,>>9"
    FIELD weight  AS INTEGER   LABEL "Weight" FORMAT ">>>>9"
    .

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttPostBOLCreateInvoice.

DEFINE VARIABLE iplPost         AS LOG       INIT YES NO-UNDO.
DEFINE VARIABLE lPost           AS LOG       NO-UNDO.

DEFINE VARIABLE ipcUserLocation AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */

/* {methods/defines/hndldefs.i &new=new} */


{custom/gcompany.i}
{custom/gloc.i}

{sys/inc/VAR.i new shared}
    
ASSIGN
    cocode = ipcCompany
    locode = ipcUserLocation
    .
{oe/oe-bolp1.i NEW}


DEFINE VARIABLE cTagDisplay        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iTagNumber         AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTagNumber2        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iDisplayFullTag    AS LOG       NO-UNDO.

DEFINE VARIABLE iLineCount         AS INTEGER   INIT 0.
DEFINE VARIABLE lCheckQty          AS LOG       NO-UNDO.
DEFINE VARIABLE cReturnChar        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecordFound       AS LOG       NO-UNDO.
DEFINE VARIABLE cExternalProgram   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInvoiceStatusType AS CHARACTER NO-UNDO.
DEFINE VARIABLE lInvoiceStatusLog  AS LOG       NO-UNDO.
DEFINE VARIABLE lInvalidDate       AS LOG       NO-UNDO.
DEFINE VARIABLE lUserChoice        AS LOG       NO-UNDO.
DEFINE VARIABLE lPrintInvoice      AS LOG       NO-UNDO.
DEFINE VARIABLE iCountNotPosted    AS INTEGER   NO-UNDO.
DEFINE VARIABLE hExtProgramHandle  AS HANDLE    NO-UNDO.
{oe/closchk.i NEW}

DEFINE TEMP-TABLE tt-email NO-UNDO
    FIELD tt-recid AS RECID
    FIELD bol-no   LIKE oe-boll.bol-no
    FIELD ord-no   LIKE oe-boll.ord-no
    FIELD i-no     LIKE itemfg.i-no
    FIELD qty      AS INTEGER
    FIELD cust-no  AS cha
    INDEX tt-cust IS PRIMARY cust-no DESCENDING .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */


/* ***********************  Control Definitions  ********************** */
DEFINE VARIABLE iStartBOLNumber    AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 0 LABEL "Beginning BOL#" NO-UNDO.
DEFINE VARIABLE cStartingCustNo    AS CHARACTER FORMAT "X(8)":U LABEL "Beginning Customer#" NO-UNDO.
DEFINE VARIABLE dtStartBOLDate     AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 LABEL "Beginning BOL Date" NO-UNDO.
DEFINE VARIABLE iEndBOLNumber      AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 99999999 LABEL "Ending BOL#" NO-UNDO.
DEFINE VARIABLE cEndingCustNo      AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" LABEL "Ending Customer#" NO-UNDO.
DEFINE VARIABLE dtEndingBOLDate    AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 LABEL "Ending BOL Date" NO-UNDO.
DEFINE VARIABLE dtTranDate         AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 LABEL "Post Date" NO-UNDO.
DEFINE VARIABLE iTransactionPeriod AS INTEGER   FORMAT ">>":U INITIAL 99 LABEL "Period" NO-UNDO.
DEFINE VARIABLE cTransactionTime   AS CHARACTER FORMAT "x(20)":U LABEL "Time" NO-UNDO.

ASSIGN
    cocode             = '001'
    locode             = 'main'
    iStartBOLNumber    = 8415
    iEndBOLNumber      = 8415
    cStartingCustNo    = "TacoBell"
    cEndingCustNo      = "TacoBell"
    dtStartBOLDate     = 1/1/2015   
    dtEndingBOLDate    = 12/31/2016
    dtTranDate         = 2/1/2016
    iTransactionPeriod = 2
    /* cTransactionTime      = "" */
    .


/* ************************  Control Triggers  ************************ */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */    



/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    /* security check need {methods/prgsecur.i} in definition section */

    ASSIGN
        dtTranDate      = 2/1/2015
        dtStartBOLDate  = 1/1/2015
        dtEndingBOLDate = 12/31/2016
        .
  
    FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.
    lPrintInvoice = oe-ctrl.u-inv.

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "BolPostTime"
        NO-LOCK NO-ERROR.

    IF AVAILABLE sys-ctrl AND sys-ctrl.char-fld EQ "Fixed Time"  THEN
        ASSIGN  cTransactionTime = STRING(int(SUBSTRING(STRING(sys-ctrl.dec-fld),1,2)) * 60 * 60 + int(SUBSTRING(STRING(sys-ctrl.dec-fld),3,4)) * 60 , "hh:mm:ss").
    ELSE ASSIGN cTransactionTime = STRING(TIME,"hh:mm:ss") .
      
    RELEASE sys-ctrl.

    FIND FIRST sys-ctrl NO-LOCK 
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "BOLPOST"
        NO-ERROR.

  
    /* Invstatus to determine invoice status when created  */
    RUN sys/ref/nk1look.p (cocode, "INVSTATUS", "L", NO, NO, "", "", 
        OUTPUT cReturnChar, OUTPUT lRecordFound).
    lInvoiceStatusLog = LOGICAL(cReturnChar).
    
    /* Invstatus to determine invoice status when created  */
    RUN sys/ref/nk1look.p (cocode, "INVSTATUS", "C", NO, NO, "", "", 
        OUTPUT cInvoiceStatusType, OUTPUT lRecordFound).

    lCheckQty = sys-ctrl.char-fld EQ "Bin>Qty".
    DO TRANSACTION:
    /* No prompt for creation of nk1 */
        {sys/inc/fgreorder.i}
    END.


    /* ***************************  Main Block  *************************** */    


    /* Best default for GUI applications is...                              */
    PAUSE 0 BEFORE-HIDE.

    ASSIGN
        dtTranDate      = 2/1/2015
        dtStartBOLDate  = 1/1/2015
        dtEndingBOLDate = 12/31/2016
        .  
    FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.
    lPrintInvoice = oe-ctrl.u-inv.

 
    /* OK button processing */
    MESSAGE "test main block"
        VIEW-AS ALERT-BOX.

    RUN check-date.  
    IF lInvalidDate THEN RETURN NO-APPLY.

    ASSIGN   
        v-s-bol         = iStartBOLNumber
        v-e-bol         = iEndBOLNumber
        v-s-date        = dtStartBOLDate
        v-e-date        = dtEndingBOLDate
        v-s-cust        = cStartingCustNo
        v-e-cust        = cEndingCustNo
        iCountNotPosted = 0
        v-tot-post      = 0
        iplPost         = TRUE
        v-tried         = NO.

    FOR EACH w-ord.
        DELETE w-ord.
    END.

    EMPTY TEMP-TABLE tt-email.
    MESSAGE "test start run report"
        VIEW-AS ALERT-BOX.
    RUN run-report. 

    IF iplPost THEN 
    DO:
        IF CAN-FIND(FIRST w-bolh) THEN 
        DO:
        
            lPost = YES.
            IF lPost THEN 
            DO:
                RUN post-bols.
       
                /* close transfer order here, Non-UI procedure */
                RUN oe/closchk.p (0).

    
                /* WFk- 5/4/12- This is here to make sure it is the last thing in */
                /* the posting process.  Posting relies on a cleanup routine */
                /* for releases instead of fixing the real problem.          */
                FOR EACH w-bolh,
                    FIRST oe-bolh NO-LOCK WHERE RECID(oe-bolh) EQ w-bolh.w-recid :
    
                    FOR EACH oe-boll NO-LOCK WHERE oe-boll.b-no EQ oe-bolh.b-no:
                         
                        FIND FIRST oe-ordl NO-LOCK
                            WHERE oe-ordl.company EQ oe-boll.company
                            AND oe-ordl.ord-no EQ oe-boll.ord-no
                            AND oe-ordl.line EQ oe-boll.line NO-ERROR.
                        RUN oe/cleanrel.p (INPUT ROWID(oe-ordl)).    
                    END.
                END.

                FOR EACH w-ord:
                    /* Non-UI procedure */
                    RUN oe/close.p (w-ord.rec-id, YES).  
                END.

                FIND FIRST tt-email NO-LOCK NO-ERROR.
                IF AVAILABLE tt-email THEN RUN email-reorderitems.
        
   
            END. /* if lPost */
        END. /* If can-find w-bolh */
    END. /* if iplPost */
END. /* main block */


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-date C-Win 
PROCEDURE check-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Validate in front end */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-nopost C-Win 
PROCEDURE create-nopost :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER cNoPostReason  LIKE w-nopost.reason NO-UNDO.


    FIND FIRST itemfg
        WHERE itemfg.company EQ oe-boll.company
        AND itemfg.i-no    EQ oe-boll.i-no
        NO-LOCK NO-ERROR.

    CREATE w-nopost.
    ASSIGN
        w-nopost.ord-no   = oe-boll.ord-no
        w-nopost.i-no     = oe-boll.i-no
        w-nopost.i-name   = IF AVAILABLE itemfg THEN itemfg.i-name ELSE "Not on File"
        w-nopost.bol-date = oe-bolh.BOL-date
        w-nopost.bol-no   = oe-bolh.BOL-no
        w-nopost.rel-no   = oe-boll.REL-no
        w-nopost.b-ord-no = oe-boll.b-ord-no
        w-nopost.cust-no  = oe-bolh.cust-no
        w-nopost.po-no    = oe-boll.PO-NO
        w-nopost.reason   = cNoPostReason.

    DELETE w-bolh.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-reorder C-Win 
PROCEDURE create-reorder :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-oeboll-rowid AS ROWID NO-UNDO.
    DEFINE BUFFER bf-oeboll FOR oe-boll.

    DEFINE VARIABLE iQtyOnHand    AS INTEGER NO-UNDO.
    DEFINE VARIABLE iQtyAvailable AS INTEGER NO-UNDO.
    DEFINE VARIABLE iReordQty     AS INTEGER NO-UNDO.

    FIND bf-oeboll WHERE ROWID(bf-oeboll) = ip-oeboll-rowid NO-LOCK.
    FIND itemfg WHERE itemfg.company = cocode AND
        itemfg.i-no = bf-oeboll.i-no NO-LOCK.

    iQtyOnHand = 0.
    FOR EACH fg-bin NO-LOCK
        WHERE fg-bin.company EQ itemfg.company
        AND fg-bin.i-no    EQ itemfg.i-no
        :
        iQtyOnHand = iQtyOnHand + fg-bin.qty.
    END.

    ASSIGN
        iQtyAvailable = iQtyOnHand /*+ (if v-inconh then itemfg.q-ono else 0)*/
                    -  itemfg.q-alloc.
 
    IF itemfg.ord-level GT iQtyAvailable THEN 
    DO:
        iReordQty = itemfg.ord-level - iQtyAvailable.

        IF iReordQty LT itemfg.ord-min AND
            itemfg.ord-min NE 0 THEN 
            iReordQty = itemfg.ord-min.

        IF iReordQty GT itemfg.ord-max AND
            itemfg.ord-max NE 0 THEN 
            iReordQty = itemfg.ord-max.
    END.
    ELSE iReordQty = 0.


    IF iReordQty > 0 THEN 
    DO:
     
        CREATE tt-email.
        ASSIGN 
            tt-email.bol-no  = bf-oeboll.bol-no
            tt-email.ord-no  = bf-oeboll.ord-no
            tt-email.i-no    = bf-oeboll.i-no
            tt-email.qty     = iReordQty
            tt-email.cust-no = oe-bolh.cust-no
            .
                                    
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exception-rpt C-Win 
PROCEDURE exception-rpt :
    /* -------------------------------------------------- oe/oe-bolp7.p 11/01 JLF */
    /* BOL posting Exception Report                                               */
    /* -------------------------------------------------------------------------- */

    /* Handle in front end */
    /*      ASSIGN iDisplayFullTag = YES.                        */
    /*     MESSAGE " Do you wish to print full tag value?  " */
    /*         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO      */
    /*         UPDATE iDisplayFullTag .                          */


    FIND FIRST period                   
        WHERE period.company EQ gcompany
        AND period.pst     LE dtTranDate
        AND period.pend    GE dtTranDate
        NO-LOCK NO-ERROR.


 
    FOR EACH w-except,

        FIRST oe-bolh
        WHERE oe-bolh.company EQ cocode
        AND oe-bolh.bol-no  EQ w-except.bol-no
        NO-LOCK

        BREAK BY w-except.bol-no
        BY w-except.ord-no
        BY w-except.rel-no
        BY w-except.b-ord-no:
    
        /* IF FIRST-OF(w-except.bol-no) THEN */ 
        DO:

            CREATE ttPostBOLCreateInvoice.
            ASSIGN
                ttPostBOLCreateInvoice.bolDate = oe-bolh.bol-date
                ttPostBOLCreateInvoice.bolNo   = oe-bolh.bol-no
                ttPostBOLCreateInvoice.carrier = oe-bolh.carrier
                ttPostBOLCreateInvoice.trailer = oe-bolh.trailer
                ttPostBOLCreateInvoice.freight = oe-bolh.freight
                ttPostBOLCreateInvoice.cwt     = oe-bolh.cwt
                ttPostBOLCreateInvoice.totWgt  = oe-bolh.tot-wt
                ttPostBOLCreateInvoice.custNo  = oe-bolh.cust-no
                ttPostBOLCreateInvoice.shipId  = oe-bolh.ship-id
                ttPostBOLCreateInvoice.deleted = oe-bolh.deleted
                .
 
        END.

        FIND FIRST itemfg
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ w-except.i-no
            NO-LOCK NO-ERROR.
        ASSIGN 
            iTagNumber  = 0
            cTagNumber2 = 0
            iTagNumber  = LENGTH(w-except.tag)
            cTagNumber2 = iTagNumber - 5 .

        IF NOT iDisplayFullTag AND iTagNumber <> 0 THEN 
            ASSIGN  cTagDisplay = SUBSTR(w-except.tag,cTagNumber2,6) .
        ELSE 
            ASSIGN cTagDisplay = w-except.tag .

        ASSIGN 
            ttPostBOLCreateInvoice.iNo     = w-except.i-no
            ttPostBOLCreateInvoice.tag     = cTagDisplay
            ttPostBOLCreateInvoice.iName   = itemfg.i-name
            ttPostBOLCreateInvoice.poNo    = w-except.po-no
            ttPostBOLCreateInvoice.ordNo   = w-except.ord-no
            ttPostBOLCreateInvoice.relNo   = w-except.rel-no /* STRING(w-except.rel-no,">>9") + "-" + STRING(w-except.b-ord-no,"99") */ 
            ttPostBOLCreateInvoice.loc     = w-except.loc
            ttPostBOLCreateInvoice.locBin  = w-except.loc-bin
            ttPostBOLCreateInvoice.cases   = w-except.cases
            ttPostBOLCreateInvoice.qtyCase = w-except.qty-case
            ttPostBOLCreateInvoice.partial = w-except.partial
            ttPostBOLCreateInvoice.weight  = w-except.weight    
            .
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-bols C-Win 
PROCEDURE post-bols :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /* DEFINE VARIABLE lv-exception AS LOG     NO-UNDO. */
    DEFINE VARIABLE dActualQty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE hRelLib    AS HANDLE  NO-UNDO.

    /* Delete xreport of report */
    {sa/sa-sls01.i}
    MESSAGE "test in post bol"
        VIEW-AS ALERT-BOX.
    DISABLE TRIGGERS FOR LOAD OF itemfg.
    FOR EACH report NO-LOCK WHERE report.term-id EQ v-term,

        FIRST oe-boll NO-LOCK WHERE RECID(oe-boll) EQ report.rec-id:
        MESSAGE "Report records found" oe-boll.bol-no
            VIEW-AS ALERT-BOX.
    END.
    FOR EACH w-bolh,
        FIRST oe-bolh WHERE RECID(oe-bolh) EQ w-bolh.w-recid
        BREAK BY oe-bolh.b-no
        BY oe-bolh.bol-no:
        IF NOT FIRST-OF(oe-bolh.b-no) THEN DELETE w-bolh.
    END.

    FIND FIRST sys-ctrl NO-LOCK
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ 'EDIBOLPost' NO-ERROR.

    /**********************  POSTING BLOCK  ****************************/
    post-blok:
    DO TRANSACTION.
        bolh:

        FOR EACH w-bolh,
            FIRST oe-bolh WHERE RECID(oe-bolh) EQ w-bolh.w-recid,

            FIRST cust
            WHERE cust.company EQ cocode
            AND cust.cust-no EQ oe-bolh.cust-no
            NO-LOCK
      
            BREAK BY oe-bolh.bol-no
            BY oe-bolh.ord-no
            BY oe-bolh.rel-no.

            /* Create tt-fg-bin */
            IF FIRST-OF(oe-bolh.bol-no) AND lPrintInvoice AND lCheckQty THEN
                RUN oe/bolcheck.p (ROWID(oe-bolh)).

            FIND FIRST w-except WHERE w-except.bol-no EQ oe-bolh.bol-no NO-ERROR.
            IF AVAILABLE w-except THEN NEXT bolh.

            iLineCount= iLineCount + 1.

            IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN 
            DO:
                FIND FIRST sys-ctrl-shipto NO-LOCK
                    WHERE sys-ctrl-shipto.company EQ sys-ctrl.company
                    AND sys-ctrl-shipto.name EQ sys-ctrl.name
                    AND sys-ctrl-shipto.cust-vend EQ YES
                    AND sys-ctrl-shipto.cust-vend-no EQ w-bolh.cust-no
                    AND sys-ctrl-shipto.log-fld EQ YES NO-ERROR.
            END. /* avail sys-ctrl */

            cExternalProgram = "sbo/oerel-recalc-act.p".
            RUN VALUE(cExternalProgram) PERSISTENT SET hExtProgramHandle NO-ERROR.
            hRelLib = hExtProgramHandle.

            FOR EACH oe-boll NO-LOCK WHERE oe-boll.b-no EQ oe-bolh.b-no,
                EACH oe-ordl NO-LOCK
                WHERE oe-ordl.company EQ oe-boll.company
                AND oe-ordl.ord-no EQ oe-boll.ord-no
                AND oe-ordl.line EQ oe-boll.LINE:

                FOR EACH oe-rel 
                    WHERE oe-rel.company EQ oe-ordl.company
                    AND oe-rel.ord-no  EQ oe-ordl.ord-no
                    AND oe-rel.i-no    EQ oe-ordl.i-no
                    AND oe-rel.line    EQ oe-ordl.line
                    AND oe-rel.stat = "P"
                    AND oe-rel.link-no GT 0 
                    AND oe-rel.rel-no GT 0:

                    /* Set actual quantity */
                    IF AVAILABLE oe-rel AND VALID-HANDLE(hRelLib) THEN 
                        RUN recalc-act-qty IN hRelLib (INPUT ROWID(oe-rel), OUTPUT dActualQty).

                END.
            END.

            IF VALID-HANDLE(hRelLib) THEN
                DELETE OBJECT hRelLib.

            FOR EACH oe-boll NO-LOCK
                WHERE oe-boll.company EQ oe-bolh.company
                AND oe-boll.b-no    EQ oe-bolh.b-no:
                    
                RUN oe/bol-pre-post.p (ROWID(oe-boll), v-term).

                IF fgreorder-log AND cust.ACTIVE EQ "E" THEN
                    RUN create-reorder (ROWID(oe-boll)).
 
            END. /* each oe-boll */


        END. /* for each oe-bolh */


        FOR EACH tt-fg-bin:
            DELETE tt-fg-bin.
        END.
          
          
        /* Non-UI process,
        Requires where report.term-id EQ v-term, FIRST oe-boll NO-LOCK WHERE RECID(oe-boll) EQ report.rec-id
        Requires shared buffer xoe-ord
         */
        RUN oe/oe-bolp3.p (v-term).


    END. /* post-blok*/

    delete-blok:
    FOR EACH oe-bolh EXCLUSIVE-LOCK
        WHERE oe-bolh.company  EQ cocode
        AND oe-bolh.deleted  EQ YES
        AND oe-bolh.bol-no   GE v-s-bol
        AND oe-bolh.bol-no  LE v-e-bol
        AND oe-bolh.bol-date GE v-s-date
        AND oe-bolh.bol-date LE v-e-date
        AND oe-bolh.cust-no  GE v-s-cust
        AND oe-bolh.cust-no  LE v-e-cust
        AND oe-bolh.trailer  NE "HOLD"
        AND oe-bolh.stat     EQ "R"
        USE-INDEX deleted:

        FOR EACH oe-boll
            WHERE oe-boll.company EQ oe-bolh.company
            AND oe-boll.b-no    EQ oe-bolh.b-no:
            DELETE oe-boll.
        END. /* each oe-boll */
  
        DELETE oe-bolh.
    END. /* each oe-bolh */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* -------------------------------------------------- oe/oe-bolp2.p 07/97 FWK */
    /* BILL OF LADING POSTING REPORT MODULE 2 - O/E Module                        */
    /* -------------------------------------------------------------------------- */

    DEFINE BUFFER b-oe-boll FOR oe-boll.

    MESSAGE "test in run report"
        VIEW-AS ALERT-BOX.

    FIND FIRST period                   
        WHERE period.company EQ gcompany
        AND period.pst     LE dtTranDate
        AND period.pend    GE dtTranDate
        NO-LOCK NO-ERROR.

    FOR EACH w-bolh:
        DELETE w-bolh.
    END.

    FOR EACH w-nopost:
        DELETE w-nopost.
    END.
    MESSAGE "test start for each" SKIP
        "cocode" cocode SKIP
        "post" iplPost
        "bol" v-s-bol SKIP
        "bol" v-e-bol SKIP
        "sdate" v-s-date SKIP
        "edate" v-e-date SKIP
        "v-s-cust" v-s-cust SKIP
        "v-e-cust" v-e-cust SKIP
        VIEW-AS ALERT-BOX.

    FOR EACH oe-bolh
        WHERE oe-bolh.company  EQ cocode
        AND oe-bolh.posted   EQ NO
        AND (oe-bolh.printed EQ YES OR NOT iplPost)
        AND oe-bolh.bol-no   GE v-s-bol
        AND oe-bolh.bol-no   LE v-e-bol
        AND oe-bolh.bol-date GE v-s-date
        AND oe-bolh.bol-date LE v-e-date
        AND oe-bolh.cust-no  GE v-s-cust
        AND oe-bolh.cust-no  LE v-e-cust
        AND oe-bolh.trailer  NE "HOLD"
        AND oe-bolh.stat     EQ "R"
        USE-INDEX post NO-LOCK:
        MESSAGE "test creatign w-bolh"
            VIEW-AS ALERT-BOX.
        CREATE w-bolh.
        ASSIGN
            w-bolh.bol-no   = oe-bolh.bol-no
            w-bolh.ord-no   = oe-bolh.ord-no
            w-bolh.w-recid  = RECID(oe-bolh)
            w-bolh.rel-no   = oe-bolh.rel-no
            w-bolh.b-ord-no = oe-bolh.b-ord-no
            w-bolh.cust-no  = oe-bolh.cust-no.
    END.

    FOR EACH oe-bolh
        WHERE oe-bolh.company  EQ cocode
        AND oe-bolh.deleted  EQ YES
        AND oe-bolh.posted   EQ YES
        AND oe-bolh.bol-no   GE v-s-bol
        AND oe-bolh.bol-no   LE v-e-bol
        AND oe-bolh.bol-date GE v-s-date
        AND oe-bolh.bol-date LE v-e-date
        AND oe-bolh.trailer  NE "HOLD"
        AND oe-bolh.stat     EQ "R"
        USE-INDEX deleted NO-LOCK:

        CREATE w-bolh.
        ASSIGN
            w-bolh.bol-no   = oe-bolh.bol-no
            w-bolh.ord-no   = oe-bolh.ord-no
            w-bolh.w-recid  = RECID(oe-bolh)
            w-bolh.rel-no   = oe-bolh.rel-no
            w-bolh.b-ord-no = oe-bolh.b-ord-no
            w-bolh.cust-no  = oe-bolh.cust-no.
    END.

    FOR EACH w-bolh,
        FIRST oe-bolh WHERE RECID(oe-bolh) EQ w-bolh.w-recid
        BREAK BY oe-bolh.b-no
        BY oe-bolh.bol-no:
        IF NOT FIRST-OF(oe-bolh.b-no) THEN DELETE w-bolh.
    END.

    MAINBLOK:
    FOR EACH w-bolh BY w-bolh.bol-no BY w-bolh.ord-no
        BY w-bolh.rel-no BY w-bolh.b-ord-no:
        FIND oe-bolh NO-LOCK WHERE RECID(oe-bolh) EQ w-bolh.w-recid .

        v-tot-post = v-tot-post + 1.

        FOR EACH oe-boll NO-LOCK
            WHERE oe-boll.company EQ oe-bolh.company
            AND oe-boll.b-no    EQ oe-bolh.b-no
            
            BREAK BY oe-boll.company
            BY oe-boll.b-no
            BY oe-boll.ord-no
            BY oe-boll.rel-no
            BY oe-boll.b-ord-no:

            RELEASE oe-ord.
            RELEASE oe-ordl.

            IF NOT oe-bolh.deleted THEN 
            DO:
                
                
                FIND FIRST oe-ord NO-LOCK WHERE oe-ord.company = oe-bolh.company AND
                    oe-ord.ord-no = oe-boll.ord-no  NO-ERROR.
                IF NOT AVAILABLE oe-ord THEN 
                DO:
                    RUN create-nopost ("Order Was Not Found").
                    NEXT mainblok.
                END.
        
        
        
                /* 04301302 - If customer 'x' and shipto = shipfrom, don't post */
                FIND cust NO-LOCK
                    WHERE cust.company EQ oe-bolh.company
                    AND cust.cust-no EQ oe-bolh.cust-no 
                    NO-ERROR.
               
                IF AVAIL(cust) AND cust.ACTIVE EQ "X"
                    AND oe-bolh.ship-id = oe-boll.loc THEN 
                DO:
                    RUN create-nopost ("Cannot transfer to the same location").
                    NEXT mainblok.
                END.
                
                
                FIND FIRST oe-ordl  NO-LOCK WHERE oe-ordl.company = oe-boll.company  AND
                    oe-ordl.ord-no = oe-boll.ord-no  AND
                    oe-ordl.line   = oe-boll.line  NO-ERROR.
                IF NOT AVAILABLE oe-ordl THEN 
                DO:
                    RUN create-nopost ("Order Lines Were Not Found").
                    NEXT mainblok.
                END.



                FIND FIRST oe-rell NO-LOCK WHERE oe-rell.company = oe-boll.company AND
                    oe-rell.r-no = oe-boll.r-no AND
                    oe-rell.i-no = oe-boll.i-no AND
                    oe-rell.line = oe-boll.line
                    USE-INDEX r-no  NO-ERROR.
                IF NOT AVAILABLE oe-rell THEN 
                DO:
                    RUN create-nopost ("Release Lines Were Not Found").
                    NEXT mainblok.
                END.



                FIND FIRST itemfg NO-LOCK WHERE itemfg.company = oe-boll.company AND
                    itemfg.i-no = oe-boll.i-no  NO-ERROR.
                IF NOT AVAILABLE itemfg THEN 
                DO:
                    RUN create-nopost ("Finish Good Item Was Not Found").
                    NEXT mainblok.
                END.

            
                IF oe-boll.loc EQ "" OR oe-boll.loc-bin EQ "" THEN 
                DO:
                    RUN create-nopost ("Warehouse or Bin is Blank").
                    NEXT mainblok.
                END.


                IF NOT CAN-FIND(FIRST b-oe-boll
                    WHERE b-oe-boll.company EQ oe-bolh.company
                    AND b-oe-boll.b-no    EQ oe-bolh.b-no
                    AND b-oe-boll.qty     NE 0)
                    THEN 
                DO:
                    RUN create-nopost ("BOL Qty is Zero").
                    NEXT mainblok.
                END.
            END.
        
            /* IF FIRST-OF(oe-boll.b-no) THEN */ 
            DO:
                CREATE ttPostBOLCreateInvoice.
                ASSIGN
                    ttPostBOLCreateInvoice.bolDate = oe-bolh.BOL-date
                    ttPostBOLCreateInvoice.bolNo   = oe-bolh.BOL-no
                    ttPostBOLCreateInvoice.carrier = oe-bolh.CARRIER
                    ttPostBOLCreateInvoice.trailer = oe-bolh.TRAILER
                    ttPostBOLCreateInvoice.freight = oe-bolh.FREIGHT
                    ttPostBOLCreateInvoice.cwt     = oe-bolh.CWT
                    ttPostBOLCreateInvoice.totWgt  = oe-bolh.TOT-WT
                    ttPostBOLCreateInvoice.custNo  = oe-bolh.cust-no
                    ttPostBOLCreateInvoice.shipID  = oe-bolh.ship-id
                    ttPostBOLCreateInvoice.deleted = oe-bolh.deleted
                    .
            END.

            ASSIGN
                ttPostBOLCreateInvoice.iNo     = oe-boll.i-no
                ttPostBOLCreateInvoice.iName   = itemfg.i-name 
                WHEN AVAILABLE itemfg
                ttPostBOLCreateInvoice.poNo    = oe-boll.po-no
                ttPostBOLCreateInvoice.ordNo   = oe-boll.ord-no
                ttPostBOLCreateInvoice.relNo   = oe-boll.rel-no
                ttPostBOLCreateInvoice.bOrdNo  = oe-boll.b-ord-no
                ttPostBOLCreateInvoice.loc     = oe-boll.loc
                ttPostBOLCreateInvoice.locBin  = oe-boll.loc-bin
                ttPostBOLCreateInvoice.tag     = oe-boll.tag
                                       

                ttPostBOLCreateInvoice.cases   = oe-boll.CASES
                ttPostBOLCreateInvoice.qtyCase = oe-boll.qty-CASE
                ttPostBOLCreateInvoice.partial = oe-boll.PARTIAL
                ttPostBOLCreateInvoice.weight  = oe-boll.WEIGHT
                .
            IF SUBSTR(oe-boll.tag,1,15) EQ oe-boll.i-no THEN        
                ttPostBOLCreateInvoice.tag =   SUBSTR(oe-boll.tag,16,8). 
            IF AVAILABLE oe-ord                                 AND
                AVAILABLE oe-ordl                                AND
                oe-ordl.ship-qty + oe-boll.qty GT
                oe-ordl.qty * (1 + (oe-ordl.over-pct / 100)) THEN
                /*         PUT SPACE(10)                                                     */
                /*             "*** Qty Shipped will exceed Qty Ordered + Allowable Overrun" */
                /*             SKIP                                                          */
                .
        END. /* each oe-boll */

      
    END. /* each w-bolh */

    iCountNotPosted = 0.

    FOR EACH w-nopost BREAK BY w-nopost.bol-no:
        
        IF FIRST(w-nopost.bol-no) THEN
            .
        /*       put skip(1)                                            */
        /*           "** Bills Of Lading Unable To Be Posted. **" skip. */
            

        /*     Add these to temp-table */
        /*     DISPLAY w-nopost.bol-no     COLUMN-LABEL "BOL.#"                       */
        /*             w-nopost.bol-date   COLUMN-LABEL "Date"                        */
        /*             w-nopost.ord-no     COLUMN-LABEL "Order#"                      */
        /*             string(w-nopost.rel-no,">>>9") + "-" +                         */
        /*             string(w-nopost.b-ord-no,"99")                                 */
        /*                                 COLUMN-LABEL "Rel#-BO#"    FORMAT "x(7)"   */
        /*             w-nopost.cust-no    COLUMN-LABEL "Cust.#"                      */
        /*             w-nopost.po-no      COLUMN-LABEL "PO#"                         */
        /*             w-nopost.i-no       COLUMN-LABEL "Item"                        */
        /*             w-nopost.i-name     COLUMN-LABEL "Name"         format "x(20)" */
        /*             w-nopost.reason     COLUMN-LABEL "Reason"       skip           */
 

        iCountNotPosted = iCountNotPosted + 1.

        DELETE w-nopost.
    END. /* each w-nopost */
    FIND FIRST w-nopost NO-ERROR.
    FIND FIRST w-bolh NO-ERROR.
    MESSAGE "test exit run-report" SKIP
        avail(w-nopost)
        avail(w-bolh)
        VIEW-AS ALERT-BOX.
 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


