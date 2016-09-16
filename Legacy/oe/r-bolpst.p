/*------------------------------------------------------------------------

  File: r-bole&p.w

  Description: BOL Edit List & Posting
Checklist:
    
    1) handle choice to print exception report
    2) handle email capability
    3) create capability to handle PROCEDURE email-reorderitems
    4) Create capability for: BOL posting Exception Report 
    5) PROCEDURE email-reorderitems :
    6) oe/bol-pre-post.p  is missing
    7) is lv-post an input parameter?
    8) oe-bolp3 depends on created report records, but where are they created?
    9) implment delete logic on oe-bolh marked as deleted
    
*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE VARIABLE ip-post   AS LOG INIT YES NO-UNDO.

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i &new=new}


{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/VAR.i new shared}
    
ASSIGN
    cocode = gcompany
    locode = gloc.

/* Shared var and shared temp-table defs */
{oe/oe-bolp1.i NEW}


DEFINE VARIABLE olinecnt       AS INTEGER   INIT 0.
DEFINE VARIABLE v-check-qty    AS LOG       NO-UNDO.
DEFINE VARIABLE v-rtn-char     AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-rec-found    AS LOG       NO-UNDO.
DEFINE VARIABLE invstatus-char AS CHARACTER NO-UNDO.
DEFINE VARIABLE invstatus-log  AS LOG       NO-UNDO.
DEFINE VARIABLE v-invalid      AS LOG       NO-UNDO.
DEFINE VARIABLE choice         AS LOG       NO-UNDO.

DEFINE VARIABLE begin_bolnum   AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 0 LABEL "Beginning BOL#" NO-UNDO.
DEFINE VARIABLE begin_cust     AS CHARACTER FORMAT "X(8)":U LABEL "Beginning Customer#" NO-UNDO.
DEFINE VARIABLE begin_date     AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 LABEL "Beginning BOL Date" NO-UNDO.
DEFINE VARIABLE end_bolnum     AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 99999999 LABEL "Ending BOL#" NO-UNDO.
DEFINE VARIABLE end_cust       AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" LABEL "Ending Customer#" NO-UNDO.
DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 LABEL "Ending BOL Date" NO-UNDO.
DEFINE VARIABLE tran-date      AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 LABEL "Post Date" NO-UNDO.
DEFINE VARIABLE tran-period    AS INTEGER   FORMAT ">>":U INITIAL 99 LABEL "Period" NO-UNDO.
DEFINE VARIABLE tran-time      AS CHARACTER FORMAT "x(20)":U LABEL "Time" NO-UNDO.

ASSIGN
    begin_bolnum   = 0
    begin_cust     = "ZOV100"
    begin_date     = TODAY - 2
    end_bolnum     = 999999
    end_cust       = "ZOV100"
    end_date       = TODAY - 2
    tran-date      = TODAY - 2
    tran-period    = 2
    /* tran-time      = "" */.


/* Defines w-ord temp-table */
{oe/closchk.i NEW}

DEFINE TEMP-TABLE tt-email NO-UNDO
    FIELD tt-recid AS RECID
    FIELD bol-no   LIKE oe-boll.bol-no
    FIELD ord-no   LIKE oe-boll.ord-no
    FIELD i-no     LIKE itemfg.i-no
    FIELD qty      AS INTEGER
    FIELD cust-no  AS cha
    INDEX tt-cust IS PRIMARY cust-no DESCENDING .



/* ********************  NK1 Values  ******************** */
FIND FIRST sys-ctrl NO-LOCK 
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "BolPostTime"
    NO-ERROR.
    
/* Handle error */
IF AVAILABLE sys-ctrl AND sys-ctrl.char-fld EQ "Fixed Time"  THEN
    ASSIGN  tran-time = STRING(int(SUBSTRING(STRING(sys-ctrl.dec-fld),1,2)) * 60 * 60 + int(SUBSTRING(STRING(sys-ctrl.dec-fld),3,4)) * 60 , "hh:mm:ss").
ELSE ASSIGN tran-time = STRING(TIME,"hh:mm:ss") .
      
RELEASE sys-ctrl.

FIND FIRST sys-ctrl NO-LOCK 
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "BOLPOST"
    NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN 
DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company = cocode
        sys-ctrl.name    = "BOLPOST"
        sys-ctrl.descrip = "Post BOL if BOL Qty > Bin Qty"
        choice           = YES.
END.
  
/* Invstatus to determine invoice status when created  */
RUN sys/ref/nk1look.p (cocode, "INVSTATUS", "L", NO, NO, "", "", 
    OUTPUT v-rtn-char, OUTPUT v-rec-found).
invstatus-log = LOGICAL(v-rtn-char).

/* Invstatus to determine invoice status when created  */
RUN sys/ref/nk1look.p (cocode, "INVSTATUS", "C", NO, NO, "", "", 
    OUTPUT invstatus-char, OUTPUT v-rec-found).

v-check-qty = sys-ctrl.char-fld EQ "Bin>Qty".

DO TRANSACTION:
    /* No prompt for creation of nk1 */
    {sys/inc/fgreorder.i}
END.


/* ***************************  Main Block  *************************** */    


/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

ASSIGN
    tran-date  = TODAY
    begin_date = TODAY
    end_date   = TODAY
    .  
FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.
v-u-inv = oe-ctrl.u-inv.

 
RUN check-date.
 
/* OK button processing */

DEFINE VARIABLE lv-post AS LOG NO-UNDO.

RUN check-date.
IF v-invalid THEN RETURN NO-APPLY.

IF invstatus-char EQ "One Bol Only" THEN
    ASSIGN END_bolnum = begin_bolnum
        END_bolnum = begin_bolnum.  


ASSIGN    
    tran-period = tran-period
    v-s-bol     = begin_bolnum
    v-e-bol     = end_bolnum
    v-s-date    = begin_date
    v-e-date    = end_date
    v-s-cust    = begin_cust
    v-e-cust    = end_cust
    v-no-post   = 0
    v-tot-post  = 0
    ip-post = true
    v-tried     = NO.

FOR EACH w-ord.
    DELETE w-ord.
END.

EMPTY TEMP-TABLE tt-email.
RUN run-report. 

IF ip-post THEN 
DO:
    IF CAN-FIND(FIRST w-bolh) THEN 
    DO:
        
        lv-post = YES.
        IF lv-post THEN 
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
        
   
        END. /* if lv-post */
    END. /* If can-find w-bolh */
END. /* if ip-post */

 

/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-date C-Win 
PROCEDURE check-date:
/*------------------------------------------------------------------------------
 Purpose:
  Parameters:  <none>
 Notes:
------------------------------------------------------------------------------*/
/* Validate in front end */
/*   DO with frame {&frame-name}:                                                  */
/*     v-invalid = no.                                                             */
/*                                                                                 */
/*     find first period                                                           */
/*         where period.company eq cocode                                          */
/*           and period.pst     le tran-date                                       */
/*           and period.pend    ge tran-date                                       */
/*         no-lock no-error.                                                       */
/*     if avail period then tran-period:SCREEN-VALUE = string(period.pnum).        */
/*                                                                                 */
/*     else                                                                        */
/*     IF ip-post THEN DO:                                                         */
/*       message "No Defined Period Exists for" tran-date view-as alert-box error. */
/*       v-invalid = yes.                                                          */
/*     end.                                                                        */
/*   END.                                                                          */
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
    DEFINE INPUT PARAMETER ip-reason LIKE w-nopost.reason NO-UNDO.


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
        w-nopost.reason   = ip-reason.

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

    DEFINE VARIABLE v-qty-onh   AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-qty-avail AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-reord-qty AS INTEGER NO-UNDO.

    FIND bf-oeboll WHERE ROWID(bf-oeboll) = ip-oeboll-rowid NO-LOCK.
    FIND itemfg WHERE itemfg.company = cocode AND
        itemfg.i-no = bf-oeboll.i-no NO-LOCK.

    v-qty-onh = 0.
    FOR EACH fg-bin NO-LOCK
        WHERE fg-bin.company EQ itemfg.company
        AND fg-bin.i-no    EQ itemfg.i-no
        :
        v-qty-onh = v-qty-onh + fg-bin.qty.
    END.

    ASSIGN
        v-qty-avail = v-qty-onh /*+ (if v-inconh then itemfg.q-ono else 0)*/
                    -  itemfg.q-alloc.
 
    IF itemfg.ord-level GT v-qty-avail THEN 
    DO:
        v-reord-qty = itemfg.ord-level - v-qty-avail.

        IF v-reord-qty LT itemfg.ord-min AND
            itemfg.ord-min NE 0 THEN 
            v-reord-qty = itemfg.ord-min.

        IF v-reord-qty GT itemfg.ord-max AND
            itemfg.ord-max NE 0 THEN 
            v-reord-qty = itemfg.ord-max.
    END.
    ELSE v-reord-qty = 0.


    IF v-reord-qty > 0 THEN 
    DO:
     
        CREATE tt-email.
        ASSIGN 
            tt-email.bol-no  = bf-oeboll.bol-no
            tt-email.ord-no  = bf-oeboll.ord-no
            tt-email.i-no    = bf-oeboll.i-no
            tt-email.qty     = v-reord-qty
            tt-email.cust-no = oe-bolh.cust-no
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
    DEFINE VARIABLE lv-exception AS LOG     NO-UNDO.
    DEFINE VARIABLE d-out        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lr-rel-lib   AS HANDLE  NO-UNDO.
{util/tmsg.i """post-bols"""}
    /* Delete xreport of report */
    {sa/sa-sls01.i}

    DISABLE TRIGGERS FOR LOAD OF itemfg.

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
            {util/tmsg.i """oe-bolh#"""}
            /* Create tt-fg-bin */
            IF FIRST-OF(oe-bolh.bol-no) AND v-u-inv AND v-check-qty THEN
                RUN oe/bolcheck.p (ROWID(oe-bolh)).

            FIND FIRST w-except WHERE w-except.bol-no EQ oe-bolh.bol-no NO-ERROR.
            IF AVAILABLE w-except THEN NEXT bolh.

            olinecnt = olinecnt + 1.

            IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN 
            DO:
                FIND FIRST sys-ctrl-shipto NO-LOCK
                    WHERE sys-ctrl-shipto.company EQ sys-ctrl.company
                    AND sys-ctrl-shipto.name EQ sys-ctrl.name
                    AND sys-ctrl-shipto.cust-vend EQ YES
                    AND sys-ctrl-shipto.cust-vend-no EQ w-bolh.cust-no
                    AND sys-ctrl-shipto.log-fld EQ YES NO-ERROR.
            END. /* avail sys-ctrl */

            RUN-PROC = "sbo/oerel-recalc-act.p".
            RUN VALUE(run-proc) PERSISTENT SET phandle NO-ERROR.
            lr-rel-lib = phandle.

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
                    IF AVAILABLE oe-rel AND VALID-HANDLE(lr-rel-lib) THEN 
                        RUN recalc-act-qty IN lr-rel-lib (INPUT ROWID(oe-rel), OUTPUT d-out).

                END.
            END.

            IF VALID-HANDLE(lr-rel-lib) THEN
                DELETE OBJECT lr-rel-lib.

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

{util/tmsg.i """start_run_report""" cocode}
    FIND FIRST period                   
        WHERE period.company EQ gcompany
        AND period.pst     LE tran-date
        AND period.pend    GE tran-date
        NO-LOCK NO-ERROR.

  
    FOR EACH w-bolh:
        DELETE w-bolh.
    END.

    FOR EACH w-nopost:
        DELETE w-nopost.
    END.

    FOR EACH oe-bolh
        WHERE oe-bolh.company  EQ cocode
        AND oe-bolh.posted   EQ NO
        AND (oe-bolh.printed EQ YES OR NOT ip-post)
        AND oe-bolh.bol-no   GE v-s-bol
        AND oe-bolh.bol-no   LE v-e-bol
        AND oe-bolh.bol-date GE v-s-date
        AND oe-bolh.bol-date LE v-e-date
        AND oe-bolh.cust-no  GE v-s-cust
        AND oe-bolh.cust-no  LE v-e-cust
        AND oe-bolh.trailer  NE "HOLD"
        AND oe-bolh.stat     EQ "R"
        USE-INDEX post NO-LOCK:

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
        FIND oe-bolh WHERE RECID(oe-bolh) EQ w-bolh.w-recid NO-LOCK.

        v-tot-post = v-tot-post + 1.

        FOR EACH oe-boll
            WHERE oe-boll.company EQ oe-bolh.company
            AND oe-boll.b-no    EQ oe-bolh.b-no
            NO-LOCK
            BREAK BY oe-boll.company
            BY oe-boll.b-no
            BY oe-boll.ord-no
            BY oe-boll.rel-no
            BY oe-boll.b-ord-no:

            RELEASE oe-ord.
            RELEASE oe-ordl.

            IF NOT oe-bolh.deleted THEN 
            DO:
                FIND FIRST oe-ord WHERE oe-ord.company = oe-bolh.company AND
                    oe-ord.ord-no = oe-boll.ord-no NO-LOCK NO-ERROR.
                IF NOT AVAILABLE oe-ord THEN 
                DO:
                    RUN create-nopost ("Order Was Not Found").
                    NEXT mainblok.
                END.
        
                /* 04301302 - If customer 'x' and shipto = shipfrom, don't post */
                FIND cust 
                    WHERE cust.company EQ oe-bolh.company
                    AND cust.cust-no EQ oe-bolh.cust-no 
                    NO-LOCK NO-ERROR.
               
                IF AVAIL(cust) AND cust.ACTIVE EQ "X"
                    AND oe-bolh.ship-id = oe-boll.loc THEN 
                DO:
                    RUN create-nopost ("Cannot transfer to the same location").
                    NEXT mainblok.
                END.
                FIND FIRST oe-ordl WHERE oe-ordl.company = oe-boll.company  AND
                    oe-ordl.ord-no = oe-boll.ord-no  AND
                    oe-ordl.line   = oe-boll.line NO-LOCK NO-ERROR.
                IF NOT AVAILABLE oe-ordl THEN 
                DO:
                    RUN create-nopost ("Order Lines Were Not Found").
                    NEXT mainblok.
                END.

                FIND FIRST oe-rell WHERE oe-rell.company = oe-boll.company AND
                    oe-rell.r-no = oe-boll.r-no AND
                    oe-rell.i-no = oe-boll.i-no AND
                    oe-rell.line = oe-boll.line
                    USE-INDEX r-no NO-LOCK NO-ERROR.
                IF NOT AVAILABLE oe-rell THEN 
                DO:
                    RUN create-nopost ("Release Lines Were Not Found").
                    NEXT mainblok.
                END.

                FIND FIRST itemfg WHERE itemfg.company = oe-boll.company AND
                    itemfg.i-no = oe-boll.i-no NO-LOCK NO-ERROR.
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
            END. /* if not oe-bolh deleted */  

        END. /* each oe-boll */

    END. /* each oe-bolh */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

