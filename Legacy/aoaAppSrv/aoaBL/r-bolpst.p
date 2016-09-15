/*------------------------------------------------------------------------
  File: r-bole&p.w
  Description: BOL Edit List & Posting

    1) handle lUserChoice to print exception report
    2) handle email capability
    3) create capability to handle PROCEDURE email-reorderitems
    4) Create capability for: BOL posting Exception Report 
    5) PROCEDURE email-reorderitems :
    6) oe/bol-pre-post.p is missing
    7) is lPost an input parameter?
    8) oe-bolp3 depends on created report records, but where are they created?
    9) implment delete logic on oe-bolh marked as deleted
*/

/* ***************************  Definitions  ***************************/
{aoaAppSrv/includes/ttPostBolCreateInvoice.i}

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttPostBOLCreateInvoice.
{aoaAppSrv/includes/pPostBOLCreateInvoice.i}

DEFINE VARIABLE ipcUserLocation  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTransactionTime AS CHARACTER NO-UNDO LABEL "Time" FORMAT "x(20)":U.

/* Local Variable Definitions ---                                       */
{sys/ref/CustList.i }

{methods/defines/hndldefs.i &new=NEW}
{methods/defines/globdefs.i &new=NEW}
{custom/gcompany.i}
{custom/gloc.i}
{sys/inc/var.i NEW SHARED}

ASSIGN
    cocode = ipcCompany
    locode = ipcUserLocation
    g_company = ipcCompany
    g_loc     = gloc
    .
{oe/oe-bolp1.i NEW}

DEFINE VARIABLE cTagDisplay        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iTagNumber         AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTagNumber2        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iDisplayFullTag    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iLineCount         AS INTEGER   NO-UNDO.
DEFINE VARIABLE lCheckQty          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cReturnChar        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecordFound       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cExternalProgram   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInvoiceStatusType AS CHARACTER NO-UNDO.
DEFINE VARIABLE lInvoiceStatusLog  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lInvalidDate       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lUserChoice        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lPrintInvoice      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iCountNotPosted    AS INTEGER   NO-UNDO.
DEFINE VARIABLE hExtProgramHandle  AS HANDLE    NO-UNDO.

{oe/closchk.i NEW}

DEFINE TEMP-TABLE tt-email NO-UNDO
    FIELD tt-recid AS RECID
    FIELD bol-no LIKE oe-boll.bol-no
    FIELD ord-no LIKE oe-boll.ord-no
    FIELD i-no   LIKE itemfg.i-no
    FIELD qty      AS INTEGER
    FIELD cust-no  AS CHARACTER
        INDEX tt-cust IS PRIMARY cust-no DESCENDING
        .

/* ***************************  Main Block  *************************** */    

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    /* security check need {methods/prgsecur.i} in definition section */
    FIND FIRST oe-ctrl NO-LOCK
         WHERE oe-ctrl.company EQ cocode
         NO-ERROR.
    lPrintInvoice = oe-ctrl.u-inv.

    FIND FIRST sys-ctrl NO-LOCK 
        WHERE sys-ctrl.company EQ cocode
          AND sys-ctrl.name    EQ "BolPostTime"
        NO-ERROR.

    IF AVAILABLE sys-ctrl AND sys-ctrl.char-fld EQ "Fixed Time" THEN
         ASSIGN cTransactionTime = STRING(int(SUBSTRING(STRING(sys-ctrl.dec-fld),1,2)) * 60 * 60 + INT(SUBSTRING(STRING(sys-ctrl.dec-fld),3,4)) * 60 , "hh:mm:ss").
    ELSE ASSIGN cTransactionTime = STRING(TIME,"hh:mm:ss").
      
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
    
    /* Best default for GUI applications is...                              */
    PAUSE 0 BEFORE-HIDE.

    FIND FIRST oe-ctrl NO-LOCK
         WHERE oe-ctrl.company EQ cocode
         NO-ERROR.
    lPrintInvoice = oe-ctrl.u-inv.
 
    RUN pCheckDate.  
    IF lInvalidDate THEN RETURN NO-APPLY.

    iCountNotPosted = 0.
        
    FOR EACH w-ord.
        DELETE w-ord.
    END.

    EMPTY TEMP-TABLE tt-email.
    
    RUN pRunReport. 
    
    IF lPost THEN DO:
        IF CAN-FIND(FIRST w-bolh) THEN DO:
            RUN pPostBols.
            /* close transfer order here, Non-UI procedure */
            
            RUN oe/closchk.p (0).
            /* WFk- 5/4/12- This is here to make sure it is the last thing in*/
            /* the posting process.  Posting relies on a cleanup routine     */
            /* for releases instead of fixing the real problem.              */
            FOR EACH w-bolh,
                FIRST oe-bolh NO-LOCK
                WHERE RECID(oe-bolh) EQ w-bolh.w-recid
                :
                FOR EACH oe-boll NO-LOCK
                    WHERE oe-boll.b-no EQ oe-bolh.b-no
                    :
                    FIND FIRST oe-ordl NO-LOCK
                         WHERE oe-ordl.company EQ oe-boll.company
                           AND oe-ordl.ord-no  EQ oe-boll.ord-no
                           AND oe-ordl.line    EQ oe-boll.line
                         NO-ERROR.
                    RUN oe/cleanrel.p (INPUT ROWID(oe-ordl)).    
                END. /* each oe-boll */
            END. /* each w-bolh */
            
            FOR EACH w-ord:
                /* Non-UI procedure*/
                RUN oe/close.p (w-ord.rec-id, YES).  
            END.

            FIND FIRST tt-email NO-LOCK NO-ERROR.
            IF AVAILABLE tt-email THEN RUN email-reorderitems.
        END. /* If can-find w-bolh */
    END. /* if lPost */

    RUN pExceptionRpt.

    FOR EACH ttPostBOLCreateInvoice:
      IF  ttPostBOLCreateInvoice.bolStatus EQ "" THEN DO:
      /*  FIND FIRST oe-bolh NO-LOCK WHERE oe-bolh.company EQ gcompany
          AND oe-bolh.bol-no EQ ttPostBOLCreateInvoice.bolNo
          NO-ERROR.
        IF AVAIL oe-bolh AND oe-bolh.posted = TRUE THEN */
          ttPostBOLCreateInvoice.bolStatus  = "Posted".
      END.
    END.
    
END. /* main block */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pCheckDate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Validate in front end */
END PROCEDURE.

PROCEDURE pCreateNoPostRec :
    DEFINE INPUT PARAMETER cNoPostReason  LIKE w-nopost.reason NO-UNDO.

    FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ oe-boll.company
          AND itemfg.i-no    EQ oe-boll.i-no
        NO-ERROR.
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
        w-nopost.reason   = cNoPostReason
        .
    DELETE w-bolh.
END PROCEDURE.

PROCEDURE pCreateReorder :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprOEBollRowID AS ROWID NO-UNDO.

    DEFINE BUFFER bOEBoll FOR oe-boll.

    DEFINE VARIABLE iQtyOnHand    AS INTEGER NO-UNDO.
    DEFINE VARIABLE iQtyAvailable AS INTEGER NO-UNDO.
    DEFINE VARIABLE iReordQty     AS INTEGER NO-UNDO.

    FIND FIRST bOEBoll NO-LOCK
         WHERE ROWID(bOEBoll) EQ iprOEBollRowID
         NO-ERROR.
    FIND FIRST itemfg NO-LOCK
         WHERE itemfg.company EQ cocode
           AND itemfg.i-no    EQ bOEBoll.i-no
         NO-ERROR.

    iQtyOnHand = 0.
    FOR EACH fg-bin NO-LOCK
        WHERE fg-bin.company EQ itemfg.company
          AND fg-bin.i-no    EQ itemfg.i-no
        :
        iQtyOnHand = iQtyOnHand + fg-bin.qty.
    END.

    iQtyAvailable = iQtyOnHand - itemfg.q-alloc.
 
    IF itemfg.ord-level GT iQtyAvailable THEN DO:
        iReordQty = itemfg.ord-level - iQtyAvailable.

        IF iReordQty LT itemfg.ord-min AND itemfg.ord-min NE 0 THEN 
        iReordQty = itemfg.ord-min.

        IF iReordQty GT itemfg.ord-max AND itemfg.ord-max NE 0 THEN 
        iReordQty = itemfg.ord-max.
    END.
    ELSE iReordQty = 0.

    IF iReordQty GT 0 THEN DO:
        CREATE tt-email.
        ASSIGN 
            tt-email.bol-no  = bOEBoll.bol-no
            tt-email.ord-no  = bOEBoll.ord-no
            tt-email.i-no    = bOEBoll.i-no
            tt-email.qty     = iReordQty
            tt-email.cust-no = oe-bolh.cust-no
            .
    END.
END PROCEDURE.

PROCEDURE pExceptionRpt :
    /* -------------------------------------------------- oe/oe-bolp7.p 11/01 JLF */
    /* BOL posting Exception Report                                               */
    /* -------------------------------------------------------------------------- */
    FIND FIRST period NO-LOCK
        WHERE period.company EQ gcompany
          AND period.pst     LE dtPostDate
          AND period.pend    GE dtPostDate
        NO-ERROR.
 
    FOR EACH w-except,
        FIRST oe-bolh
        WHERE oe-bolh.company EQ cocode
          AND oe-bolh.bol-no  EQ w-except.bol-no
        NO-LOCK
        BREAK BY w-except.bol-no
              BY w-except.ord-no
              BY w-except.rel-no
              BY w-except.b-ord-no
        :
        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ cocode
              AND itemfg.i-no    EQ w-except.i-no
            NO-ERROR.
        /*
        CREATE ttPostBOLCreateInvoice.
        ASSIGN
            iTagNumber  = 0
            cTagNumber2 = 0
            iTagNumber  = LENGTH(w-except.tag)
            cTagNumber2 = iTagNumber - 5
            cTagDisplay = IF NOT iDisplayFullTag AND iTagNumber NE 0 THEN SUBSTR(w-except.tag,cTagNumber2,6)
                          ELSE w-except.tag
            ttPostBOLCreateInvoice.bolDate   = oe-bolh.bol-date
            ttPostBOLCreateInvoice.bolNo     = oe-bolh.bol-no
            ttPostBOLCreateInvoice.carrier   = oe-bolh.carrier
            ttPostBOLCreateInvoice.trailer   = oe-bolh.trailer
            ttPostBOLCreateInvoice.freight   = oe-bolh.freight
            ttPostBOLCreateInvoice.cwt       = oe-bolh.cwt
            ttPostBOLCreateInvoice.totWgt    = oe-bolh.tot-wt
            ttPostBOLCreateInvoice.custNo    = oe-bolh.cust-no
            ttPostBOLCreateInvoice.shipId    = oe-bolh.ship-id
            ttPostBOLCreateInvoice.deleted   = oe-bolh.deleted
            ttPostBOLCreateInvoice.iNo       = w-except.i-no
            ttPostBOLCreateInvoice.tag       = cTagDisplay
            ttPostBOLCreateInvoice.iName     = IF AVAILABLE itemfg THEN itemfg.i-name ELSE ""
            ttPostBOLCreateInvoice.poNo      = w-except.po-no
            ttPostBOLCreateInvoice.ordNo     = w-except.ord-no
            ttPostBOLCreateInvoice.relNo     = w-except.rel-no /* STRING(w-except.rel-no,">>9") + "-" + STRING(w-except.b-ord-no,"99") */ 
            ttPostBolCreateInvoice.bOrdNo    = w-except.b-ord-no
            ttPostBOLCreateInvoice.loc       = w-except.loc
            ttPostBOLCreateInvoice.locBin    = w-except.loc-bin
            ttPostBOLCreateInvoice.cases     = w-except.cases
            ttPostBOLCreateInvoice.qtyCase   = w-except.qty-case
            ttPostBOLCreateInvoice.partial   = w-except.partial
            ttPostBOLCreateInvoice.weight    = w-except.weight  
            ttPostBOLCreateInvoice.bolStatus = "Not Posted"  
            ttPostBOLCreateInvoice.reason    = "Insufficient Quantity"
            .
          */
        FOR EACH ttPostBOLCreateInvoice WHERE ttPostBOLCreateInvoice.bolNo EQ oe-bolh.bol-no:
          ASSIGN ttPostBOLCreateInvoice.reason    = "Insuffient Quantity"
                 ttPostBOLCreateInvoice.bolStatus = "Not Posted" 
                 .
        END.
    END.
END PROCEDURE.

PROCEDURE pPostBols :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /* DEFINE VARIABLE lv-exception AS LOG     NO-UNDO. */
    DEFINE VARIABLE dActualQty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE hRelLib    AS HANDLE  NO-UNDO.

    /* Deletes xreport of report */
    {sa/sa-sls01.i}

    DISABLE TRIGGERS FOR LOAD OF itemfg.
    FOR EACH w-bolh,
        FIRST oe-bolh NO-LOCK
        WHERE RECID(oe-bolh) EQ w-bolh.w-recid
        BREAK BY oe-bolh.b-no
              BY oe-bolh.bol-no
        :
        IF NOT FIRST-OF(oe-bolh.b-no) THEN DELETE w-bolh.
    END.

    FIND FIRST sys-ctrl NO-LOCK
         WHERE sys-ctrl.company EQ cocode
           AND sys-ctrl.name    EQ 'EDIBOLPost'
         NO-ERROR.
    /**********************  POSTING BLOCK  ****************************/
    FIND FIRST w-bolh NO-ERROR.
    
    post-blok:
    DO TRANSACTION.
        bolh:
        FOR EACH w-bolh,
            FIRST oe-bolh NO-LOCK
            WHERE RECID(oe-bolh) EQ w-bolh.w-recid,
            FIRST cust NO-LOCK
            WHERE cust.company EQ cocode
              AND cust.cust-no EQ oe-bolh.cust-no                  
            BREAK BY oe-bolh.bol-no
                  BY oe-bolh.ord-no
                  BY oe-bolh.rel-no
            :
            /* Create tt-fg-bin */
            IF FIRST-OF(oe-bolh.bol-no) AND lPrintInvoice AND lCheckQty THEN
            RUN oe/bolcheck.p (ROWID(oe-bolh)).

            FIND FIRST w-except WHERE w-except.bol-no EQ oe-bolh.bol-no NO-ERROR.
            IF AVAILABLE w-except THEN NEXT bolh.

            iLineCount = iLineCount + 1.

            IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN DO:
                FIND FIRST sys-ctrl-shipto NO-LOCK
                     WHERE sys-ctrl-shipto.company      EQ sys-ctrl.company
                       AND sys-ctrl-shipto.name         EQ sys-ctrl.name
                       AND sys-ctrl-shipto.cust-vend    EQ YES
                       AND sys-ctrl-shipto.cust-vend-no EQ w-bolh.cust-no
                       AND sys-ctrl-shipto.log-fld      EQ YES
                     NO-ERROR.
            END. /* avail sys-ctrl */

            cExternalProgram = "aoaAppSrv\aoaBL\recalcActRel.p".
            RUN VALUE(cExternalProgram) PERSISTENT SET hExtProgramHandle NO-ERROR.
            hRelLib = hExtProgramHandle.

            FOR EACH oe-boll NO-LOCK WHERE oe-boll.b-no EQ oe-bolh.b-no,
                EACH oe-ordl NO-LOCK
                WHERE oe-ordl.company EQ oe-boll.company
                AND oe-ordl.ord-no EQ oe-boll.ord-no
                AND oe-ordl.line EQ oe-boll.LINE:

                FOR EACH oe-rel NO-LOCK
                    WHERE oe-rel.company EQ oe-ordl.company
                      AND oe-rel.ord-no  EQ oe-ordl.ord-no
                      AND oe-rel.i-no    EQ oe-ordl.i-no
                      AND oe-rel.line    EQ oe-ordl.line
                      AND oe-rel.stat    EQ "P"
                      AND oe-rel.link-no GT 0
                      AND oe-rel.rel-no  GT 0
                    :                    
                    /* Set actual quantity */
                    IF AVAILABLE oe-rel AND VALID-HANDLE(hRelLib) THEN
                    RUN recalc-act-qty IN hRelLib (INPUT ROWID(oe-rel), OUTPUT dActualQty).
                END.
            END.

            IF VALID-HANDLE(hRelLib) THEN
                DELETE OBJECT hRelLib.

            FOR EACH oe-boll NO-LOCK
                WHERE oe-boll.company EQ oe-bolh.company
                  AND oe-boll.b-no    EQ oe-bolh.b-no
                :

                RUN oe/bol-pre-post.p (ROWID(oe-boll), v-term).

                IF fgreorder-log AND cust.ACTIVE EQ "E" THEN
                RUN pCreateReorder (ROWID(oe-boll)).



            END. /* each oe-boll */
        END. /* for each oe-bolh */

        FOR EACH tt-fg-bin:
            DELETE tt-fg-bin.
        END.
        /* Non-UI process,                                                                                     */
        /* Requires where report.term-id EQ v-term, FIRST oe-boll NO-LOCK WHERE RECID(oe-boll) EQ report.rec-id*/
        /* Requires shared buffer xoe-ord                                                                      */
        FIND FIRST report WHERE report.term-id EQ v-term NO-ERROR.

        RUN oe/oe-bolp3.p (v-term).
    
    END. /* post-blok*/
    
    DELETE-BLOK:
    FOR EACH oe-bolh EXCLUSIVE-LOCK
        WHERE oe-bolh.company  EQ cocode
          AND oe-bolh.deleted  EQ YES
          AND oe-bolh.bol-no   GE iStartBOL
          AND oe-bolh.bol-no  LE iEndBOL
          AND oe-bolh.bol-date GE dtStartBOLDate
          AND oe-bolh.bol-date LE dtEndBOLDate
          AND oe-bolh.cust-no  GE cStartCustNo
          AND oe-bolh.cust-no  LE cEndCustNo
          AND oe-bolh.trailer  NE "HOLD"
          AND oe-bolh.stat     EQ "R"
        USE-INDEX deleted
        :
        FOR EACH oe-boll
            WHERE oe-boll.company EQ oe-bolh.company
              AND oe-boll.b-no    EQ oe-bolh.b-no
            :
            DELETE oe-boll.
        END. /* each oe-boll */
        DELETE oe-bolh.
    END. /* each oe-bolh */
END PROCEDURE.

PROCEDURE pRunReport :
    /* -------------------------------------------------- oe/oe-bolp2.p 07/97 FWK */
    /* BILL OF LADING POSTING REPORT MODULE 2 - O/E Module                        */
    /* -------------------------------------------------------------------------- */
    DEFINE BUFFER b-oe-boll FOR oe-boll.

    FIND FIRST period NO-LOCK                
        WHERE period.company EQ gcompany
          AND period.pst     LE dtPostDate
          AND period.pend    GE dtPostDate
        NO-ERROR.

    FOR EACH w-bolh:
        DELETE w-bolh.
    END.

    FOR EACH w-nopost:
        DELETE w-nopost.
    END.

    FOR EACH oe-bolh NO-LOCK
        WHERE oe-bolh.company  EQ cocode
          AND oe-bolh.posted   EQ NO
          AND (oe-bolh.printed EQ YES OR NOT lPost)
          AND oe-bolh.bol-no   GE iStartBOL
          AND oe-bolh.bol-no   LE iEndBOL
          AND oe-bolh.bol-date GE dtStartBOLDate
          AND oe-bolh.bol-date LE dtEndBOLDate
          AND oe-bolh.cust-no  GE cStartCustNo
          AND oe-bolh.cust-no  LE cEndCustNo
          AND oe-bolh.trailer  NE "HOLD"
          AND oe-bolh.stat     EQ "R"
        USE-INDEX post
        :
        CREATE w-bolh.
        ASSIGN
            w-bolh.bol-no   = oe-bolh.bol-no
            w-bolh.ord-no   = oe-bolh.ord-no
            w-bolh.w-recid  = RECID(oe-bolh)
            w-bolh.rel-no   = oe-bolh.rel-no
            w-bolh.b-ord-no = oe-bolh.b-ord-no
            w-bolh.cust-no  = oe-bolh.cust-no
            .
    END.

    FOR EACH oe-bolh NO-LOCK
        WHERE oe-bolh.company  EQ cocode
          AND oe-bolh.deleted  EQ YES
          AND oe-bolh.posted   EQ YES
          AND oe-bolh.bol-no   GE iStartBOL
          AND oe-bolh.bol-no   LE iEndBOL
          AND oe-bolh.bol-date GE dtStartBOLDate
          AND oe-bolh.bol-date LE dtEndBOLDate
          AND oe-bolh.trailer  NE "HOLD"
          AND oe-bolh.stat     EQ "R"
        USE-INDEX deleted
        :
        CREATE w-bolh.
        ASSIGN
            w-bolh.bol-no   = oe-bolh.bol-no
            w-bolh.ord-no   = oe-bolh.ord-no
            w-bolh.w-recid  = RECID(oe-bolh)
            w-bolh.rel-no   = oe-bolh.rel-no
            w-bolh.b-ord-no = oe-bolh.b-ord-no
            w-bolh.cust-no  = oe-bolh.cust-no
            .
    END.

    FOR EACH w-bolh,
        FIRST oe-bolh NO-LOCK
        WHERE RECID(oe-bolh) EQ w-bolh.w-recid
        BREAK BY oe-bolh.b-no
              BY oe-bolh.bol-no
        :
        IF NOT FIRST-OF(oe-bolh.b-no) THEN DELETE w-bolh.
    END.

    MAINBLOK:
    FOR EACH w-bolh
        BY w-bolh.bol-no 
        BY w-bolh.ord-no
        BY w-bolh.rel-no 
        BY w-bolh.b-ord-no
        :
        FIND oe-bolh NO-LOCK WHERE RECID(oe-bolh) EQ w-bolh.w-recid.
        v-tot-post = v-tot-post + 1.
        FOR EACH oe-boll NO-LOCK
            WHERE oe-boll.company EQ oe-bolh.company
              AND oe-boll.b-no    EQ oe-bolh.b-no
            BREAK BY oe-boll.company
                  BY oe-boll.b-no
                  BY oe-boll.ord-no
                  BY oe-boll.rel-no
                  BY oe-boll.b-ord-no
            :
            RELEASE oe-ord.
            RELEASE oe-ordl.
            IF NOT oe-bolh.deleted THEN DO:
                FIND FIRST oe-ord NO-LOCK
                     WHERE oe-ord.company EQ oe-bolh.company
                       AND oe-ord.ord-no = oe-boll.ord-no 
                     NO-ERROR.
                IF NOT AVAILABLE oe-ord THEN DO:
                    RUN pCreateNoPostRec ("Order Was Not Found").
                    NEXT mainblok.
                END.
                /* 04301302 - If customer 'x' and shipto = shipfrom, don't post */
                FIND cust NO-LOCK
                    WHERE cust.company EQ oe-bolh.company
                      AND cust.cust-no EQ oe-bolh.cust-no 
                    NO-ERROR.
                IF AVAIL(cust) AND cust.ACTIVE EQ "X" AND oe-bolh.ship-id = oe-boll.loc THEN DO:
                    RUN pCreateNoPostRec ("Cannot transfer to the same location").
                    NEXT mainblok.
                END.
                FIND FIRST oe-ordl NO-LOCK
                     WHERE oe-ordl.company = oe-boll.company
                       AND oe-ordl.ord-no = oe-boll.ord-no
                       AND oe-ordl.line   = oe-boll.line  NO-ERROR.
                IF NOT AVAILABLE oe-ordl THEN 
                DO:
                    RUN pCreateNoPostRec ("Order Lines Were Not Found").
                    NEXT mainblok.
                END.

                FIND FIRST oe-rell NO-LOCK
                     WHERE oe-rell.company = oe-boll.company
                       AND oe-rell.r-no = oe-boll.r-no
                       AND oe-rell.i-no = oe-boll.i-no
                       AND oe-rell.line = oe-boll.line
                     USE-INDEX r-no NO-ERROR.
                IF NOT AVAILABLE oe-rell THEN DO:
                    RUN pCreateNoPostRec ("Release Lines Were Not Found").
                    NEXT mainblok.
                END.

                FIND FIRST itemfg NO-LOCK
                     WHERE itemfg.company = oe-boll.company
                       AND itemfg.i-no = oe-boll.i-no
                     NO-ERROR.
                IF NOT AVAILABLE itemfg THEN DO:
                    RUN pCreateNoPostRec ("Finish Good Item Was Not Found").
                    NEXT mainblok.
                END.
            
                IF oe-boll.loc EQ "" OR oe-boll.loc-bin EQ "" THEN DO:
                    RUN pCreateNoPostRec ("Warehouse or Bin is Blank").
                    NEXT mainblok.
                END.

                IF NOT CAN-FIND(FIRST b-oe-boll
                                WHERE b-oe-boll.company EQ oe-bolh.company
                                  AND b-oe-boll.b-no    EQ oe-bolh.b-no
                                  AND b-oe-boll.qty     NE 0) THEN DO:
                    RUN pCreateNoPostRec ("BOL Qty is Zero").
                    NEXT mainblok.
                END.
            END.
        
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

            ASSIGN
                ttPostBOLCreateInvoice.iNo     = oe-boll.i-no
                ttPostBOLCreateInvoice.iName   = itemfg.i-name WHEN AVAILABLE itemfg
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
                ttPostBOLCreateInvoice.tag = SUBSTR(oe-boll.tag,16,8). 
            IF AVAILABLE oe-ord  AND
               AVAILABLE oe-ordl AND
               oe-ordl.ship-qty + oe-boll.qty GT
               oe-ordl.qty * (1 + (oe-ordl.over-pct / 100)) THEN
            ttPostBOLCreateInvoice.reason = "Qty Shipped will exceed Qty Ordered + Allowable Overrun".
        END. /* each oe-boll */
    END. /* each w-bolh */

    iCountNotPosted = 0.

    FOR EACH w-nopost
        BREAK BY w-nopost.bol-no
        :
        /* Create record just for the status of not posted */
        CREATE ttPostBolCreateInvoice.
        ASSIGN
            ttPostBOLCreateInvoice.bolDate   = w-nopost.bol-date
            ttPostBOLCreateInvoice.bolNo     = w-nopost.bol-no
            ttPostBOLCreateInvoice.custNo    = w-nopost.cust-no
            ttPostBolCreateInvoice.bolStatus = "Not Posted"
            ttPostBolCreateInvoice.reason    = w-nopost.reason
            ttPostBOLCreateInvoice.iNo       = w-nopost.i-no
            ttPostBOLCreateInvoice.iName     = w-nopost.i-name  
            ttPostBOLCreateInvoice.poNo      = w-nopost.po-no
            ttPostBOLCreateInvoice.ordNo     = w-nopost.ord-no
            ttPostBOLCreateInvoice.relNo     = w-nopost.rel-no
            ttPostBOLCreateInvoice.bOrdNo    = w-nopost.b-ord-no
            iCountNotPosted = iCountNotPosted + 1
            .
        DELETE w-nopost.
    END. /* each w-nopost */
END PROCEDURE.

PROCEDURE pBuildCustList :
    /*------------------------------------------------------------------------------
      Purpose:     Template.rpa
      Parameters:  Company, Use List?, Start Cust, End Cust, ID
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplList      AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcStartCust AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEndCust   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcID        AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bCust FOR cust.
    
    DEFINE VARIABLE lActive AS LOGICAL NO-UNDO.
    
    EMPTY TEMP-TABLE ttCustList.

    IF iplList THEN
        RUN sys/ref/CustList.p (ipcCompany, ipcID, YES, OUTPUT lActive).
    ELSE 
    DO:
        FOR EACH bCust NO-LOCK
            WHERE bCust.company EQ ipcCompany
            AND bCust.cust-no GE ipcStartCust
            AND bCust.cust-no LE ipcEndCust
            :
            CREATE ttCustList.
            ASSIGN 
                ttCustList.cust-no = bCust.cust-no
                ttCustList.log-fld = YES
                .
        END. /* each bcust */
    END. /* else */
END PROCEDURE.
