
/*------------------------------------------------------------------------
    File        : fgrctd-post.p
    Purpose     : Replaces fg/fgpstall.w  
                           

    Syntax      :

    Description : Persistent Procedure for housing logic related to 
    returning the correct posting material 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{custom/gcompany.i}
{custom/gloc.i}
{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}

cocode = g_company.
locode = g_loc.
DO TRANSACTION:
    {pc/pcprdd4u.i NEW}
    {fg/invrecpt.i NEW}
    {jc/jcgl-sh.i  NEW}
    {fg/fullset.i  NEW}
    {fg/fg-post3.i NEW}
END.
DEFINE VARIABLE v-post-date AS DATE      INITIAL TODAY.
DEFINE VARIABLE fg-uom-list AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-fgpostgl  AS CHARACTER NO-UNDO.

DEFINE STREAM logFile.
DEFINE STREAM st-email.
DEFINE TEMP-TABLE w-fg-rctd NO-UNDO LIKE fg-rctd 
    FIELD row-id   AS ROWID
    FIELD has-rec  AS LOG   INIT NO
    FIELD invoiced AS LOG   INIT NO.
DEFINE TEMP-TABLE tt-email NO-UNDO 
    FIELD tt-recid AS RECID
    FIELD job-no   LIKE job-hdr.job-no
    FIELD job-no2  LIKE job-hdr.job-no2
    FIELD i-no     LIKE itemfg.i-no
    FIELD qty      AS INTEGER
    FIELD cust-no  AS cha
    INDEX tt-cust IS PRIMARY cust-no DESCENDING .
    
RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


/* ***************************  Main Block  *************************** */

DO TRANSACTION:
    {sys/inc/closejob.i FGPost}
    {sys/inc/fgpostgl.i}
    {sys/inc/adjustgl.i}
    {sys/inc/fgemails.i}
    {sys/inc/postdate.i}
    {sys/inc/fgpost.i}
END.


/* **********************  Internal Procedures  *********************** */

PROCEDURE post-finish-goods:
    /*------------------------------------------------------------------------------
         Purpose: 
         Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcrowid AS ROWID NO-UNDO.
    
    DEFINE BUFFER b-fg-rcpts FOR fg-rcpts.
    DEFINE BUFFER b-fg-rdtl  FOR fg-rdtl.
    DEFINE BUFFER b-fg-bin   FOR fg-bin.
    DEFINE BUFFER b-itemfg   FOR itemfg.
    DEFINE BUFFER b-itemfg1  FOR itemfg.
    DEFINE BUFFER ps-rctd    FOR fg-rctd .
    DEFINE BUFFER b-po-ordl  FOR po-ordl.
    DEFINE BUFFER b-oe-ordl  FOR oe-ordl.

    DEFINE VARIABLE v-one-item     AS LOG.
    DEFINE VARIABLE v-dec          AS DECIMAL   DECIMALS 10.
    DEFINE VARIABLE v-po-no        LIKE rm-rcpt.po-no NO-UNDO.
    DEFINE VARIABLE x              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE i              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-r-qty        LIKE fg-rctd.qty NO-UNDO.
    DEFINE VARIABLE v-i-qty        LIKE fg-rctd.qty NO-UNDO.
    DEFINE VARIABLE v-t-qty        LIKE fg-rctd.qty NO-UNDO.
    DEFINE VARIABLE v-overrun-qty  LIKE fg-rctd.qty NO-UNDO.
    DEFINE VARIABLE v-underrun-qty LIKE fg-rctd.qty NO-UNDO.
    DEFINE VARIABLE v-reduce-qty   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-est-no       AS cha       NO-UNDO.
    DEFINE VARIABLE v-recid        AS RECID     NO-UNDO.
    DEFINE VARIABLE v-cost         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-binqty       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-qty          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-tagcost      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE ld-cvt-qty     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE ld-cvt-cost    AS DECIMAL   DECIMALS 10 NO-UNDO.
    DEFINE VARIABLE v-autobin      AS cha       NO-UNDO.
    DEFINE VARIABLE v-newhdr       AS LOG       NO-UNDO. 
    DEFINE VARIABLE v-fin-qty      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE choice         AS LOG       NO-UNDO.
    DEFINE VARIABLE v-trnum        LIKE gl-ctrl.trnum NO-UNDO.
    DEFINE VARIABLE uperiod        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE sysdate        AS DATE      INIT TODAY NO-UNDO.    
    DEFINE VARIABLE v-date         LIKE sysdate NO-UNDO.
    DEFINE VARIABLE v-underrun     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-qty-received AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-got-fgemail  AS LOG       NO-UNDO.
    DEFINE VARIABLE v-fgemail-file AS cha       NO-UNDO.
    DEFINE VARIABLE li-tag-no      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ll-qty-changed AS LOG       NO-UNDO.
    DEFINE VARIABLE ll-whs-item    AS LOG       NO-UNDO.
    DEFINE VARIABLE char-hdl       AS CHARACTER NO-UNDO.

    DEFINE VARIABLE fgPostLog      AS LOGICAL   NO-UNDO.
    FIND FIRST fg-rctd NO-LOCK
        WHERE fg-rctd.company EQ ipcCompany
        AND rowid(fg-rctd) EQ ipcrowid NO-ERROR .
    IF  NOT AVAILABLE fg-rctd THEN 
    DO:
        RETURN ERROR.
    END.

    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ fg-rctd.i-no
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE itemfg THEN 
    DO:  
        RETURN ERROR.
    END.


    fgPostLog = SEARCH('logs/fgpstall.log') NE ?.
    IF fgPostLog THEN
        OUTPUT STREAM logFile TO VALUE('logs/fgpstall.' +
            STRING(TODAY,'99999999') + '.' + STRING(TIME) + '.log').

    SESSION:SET-WAIT-STATE ("general").
    IF fgPostLog THEN RUN fgPostLog ('Started').
    FIND FIRST period NO-LOCK
        WHERE period.company EQ cocode
        AND period.pst     LE v-post-date
        AND period.pend    GE v-post-date.

    FIND FIRST sys-ctrl  WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "AUTOPOST"
        NO-LOCK NO-ERROR.
    v-autobin = IF AVAILABLE sys-ctrl THEN sys-ctrl.char-fld ELSE "".

    DISABLE TRIGGERS FOR LOAD OF itemfg.
    DISABLE TRIGGERS FOR LOAD OF b-oe-ordl.

    FOR EACH w-fg-rctd:
        DELETE w-fg-rctd.
    END.

    /* Create a single workfile record for the finished good being posted */
    CREATE w-fg-rctd.
    BUFFER-COPY fg-rctd TO w-fg-rctd
        ASSIGN 
        w-fg-rctd.row-id  = ROWID(fg-rctd)
        w-fg-rctd.has-rec = YES.

    FOR EACH w-fg-rctd,

        FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-fg-rctd.i-no

        BY w-fg-rctd.tag
        BY w-fg-rctd.rct-date
        BY w-fg-rctd.r-no:

        IF fgPostLog THEN RUN fgPostLog ('Start fg/fg-post.i ' + TRIM(itemfg.i-no)).
        {fg/fg-post.i w-fg-rctd w-fg-rctd}

        FIND CURRENT po-ordl NO-LOCK NO-ERROR.
        FIND CURRENT fg-bin NO-LOCK NO-ERROR.
        IF NOT AVAILABLE fg-bin THEN
            FIND FIRST fg-bin WHERE fg-bin.company = fg-rctd.company
                AND fg-bin.tag  = fg-rctd.tag
                NO-LOCK NO-ERROR.
      
        IF fgPostLog THEN RUN fgPostLog ('End fg/fg-post.i - Start fg/fgemails.i').
        IF w-fg-rctd.rita-code = "R" THEN 
        DO:
            {fg/fgemails.i}
        END.

        IF fgPostLog THEN RUN fgPostLog ('End fg-bin - Start fg-rctd').

        FIND FIRST fg-rctd WHERE ROWID(fg-rctd) EQ w-fg-rctd.row-id NO-ERROR.

        IF AVAILABLE fg-rctd THEN 
        DO:
            ASSIGN
                fg-rctd.rita-code = "P"  /* posted */
                fg-rctd.post-date = v-post-date
                fg-rctd.tag2      = w-fg-rctd.tag2.

            FOR EACH fg-rcpts
                WHERE fg-rcpts.company EQ fg-rctd.company
                AND fg-rcpts.r-no    EQ fg-rctd.r-no:
                fg-rcpts.rita-code = fg-rctd.rita-code.
            END.
        END.

        IF fgPostLog THEN RUN fgPostLog ('End loop'). 
    END.  /* for each fg-rctd */

    FIND CURRENT itemfg NO-LOCK NO-ERROR.

    IF fgPostLog THEN RUN fgPostLog ('End fg/fgemails.i - Start loadtag').
    FOR EACH w-fg-rctd
        BREAK BY w-fg-rctd.i-no
        BY w-fg-rctd.job-no
        BY w-fg-rctd.job-no2
        BY w-fg-rctd.loc
        BY w-fg-rctd.loc-bin
        BY w-fg-rctd.tag:

        IF LAST-OF(w-fg-rctd.tag) THEN 
        DO:
            IF TRIM(w-fg-rctd.tag) NE "" THEN 
                /* Ensure Bin/Tags Qty is correct.  Task 01270602 */
        
                FOR EACH fg-bin NO-LOCK
                    WHERE fg-bin.company EQ g_company
                    AND fg-bin.i-no    EQ loadtag.i-no
                    AND fg-bin.tag     EQ loadtag.tag-no
                    USE-INDEX tag:
                    RUN fg/calcbinq.p (ROWID(fg-bin)).
                END.

            /* IF w-fg-rctd.tag <> "" then*/
            FIND FIRST loadtag
                WHERE loadtag.company   EQ g_company
                AND loadtag.item-type EQ NO
                AND loadtag.tag-no    EQ w-fg-rctd.tag
                AND loadtag.i-no      EQ w-fg-rctd.i-no
                AND loadtag.job-no    EQ w-fg-rctd.job-no
                USE-INDEX tag EXCLUSIVE-LOCK NO-ERROR.
            IF fgPostLog THEN RUN fgPostLog ('End loadtag - Start fg-bin').

            IF AVAILABLE loadtag THEN 
            DO:
                FIND FIRST fg-bin
                    WHERE fg-bin.company EQ g_company
                    AND fg-bin.i-no    EQ loadtag.i-no
                    AND fg-bin.tag     EQ loadtag.tag-no
                    /*AND fg-bin.job-no = loadtag.job-no
                      AND fg-bin.job-no2 = loadtag.job-no2*/
                    AND fg-bin.qty     GT 0
                    USE-INDEX tag NO-LOCK NO-ERROR.
                IF NOT AVAILABLE fg-bin THEN
                    FIND FIRST fg-bin
                        WHERE fg-bin.company EQ g_company
                        AND fg-bin.i-no    EQ loadtag.i-no
                        AND fg-bin.tag     EQ loadtag.tag-no
                        /*AND fg-bin.job-no = loadtag.job-no
                          AND fg-bin.job-no2 = loadtag.job-no2*/
                        /* AND fg-bin.qty     GT 0 */
                        USE-INDEX tag NO-LOCK NO-ERROR.
          
                IF w-fg-rctd.rita-code = "T" AND /*loadtag.tot-cases = w-fg-rctd.cases*/
                    TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0) = w-fg-rctd.cases THEN  /* full qty transfer*/ 
                    ASSIGN
                        loadtag.loc          = w-fg-rctd.loc2   
                        loadtag.loc-bin      = w-fg-rctd.loc-bin2
                        loadtag.qty          = fg-bin.qty
                        loadtag.pallet-count = fg-bin.qty
                        loadtag.partial      = fg-bin.partial-count
                        loadtag.tot-cases    = (loadtag.qty - loadtag.partial) / loadtag.qty-case.
                ELSE /*partial transfer */
                    ASSIGN
                        loadtag.loc     = w-fg-rctd.loc
                        loadtag.loc-bin = w-fg-rctd.loc-bin.

                FIND CURRENT loadtag NO-LOCK NO-ERROR.
            END.
        END.
    END.

    FOR EACH w-inv:
        DELETE w-inv.
    END.

    IF fgPostLog THEN RUN fgPostLog ('End First - Start Second For Each w-fg-rctd').
    FOR EACH w-fg-rctd WHERE w-fg-rctd.invoiced,
        FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-fg-rctd.i-no
        NO-LOCK:

        CREATE w-inv.
        w-inv.row-id = w-fg-rctd.row-id.
    END.
    IF fgPostLog THEN RUN fgPostLog ('End Second For Each w-fg-rctd').

    IF fgPostLog THEN RUN fgPostLog ('Begin Run fg/invrecpt.p').
    RUN fg/invrecpt.p (?, 2).
    IF fgPostLog THEN RUN fgPostLog ('End Run fg/invrecpt.p').

    IF fgPostLog THEN RUN fgPostLog ('End First - Start Third For Each w-fg-rctd').
    FOR EACH w-fg-rctd WHERE TRIM(w-fg-rctd.tag) EQ "",
        FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-fg-rctd.i-no
        NO-LOCK
        BREAK BY w-fg-rctd.i-no:

        IF LAST-OF(w-fg-rctd.i-no) THEN 
        DO:
            IF fgPostLog THEN RUN fgPostLog ('Begin Run fg/updfgcs1.p for ' + w-fg-rctd.i-no).
            RUN fg/updfgcs1.p (RECID(itemfg), NO).
            IF fgPostLog THEN RUN fgPostLog ('End Run fg/updfgcs1.p for ' + w-fg-rctd.i-no).

            FOR EACH oe-ordl
                WHERE oe-ordl.company EQ cocode
                AND oe-ordl.opened  EQ YES
                AND oe-ordl.i-no    EQ w-fg-rctd.i-no
                AND oe-ordl.job-no  EQ ""
                AND oe-ordl.cost    EQ 0
                USE-INDEX opened NO-LOCK
                BREAK BY oe-ordl.ord-no
                TRANSACTION :

                DO i = 1 TO 1000:
                    FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) EXCLUSIVE NO-ERROR NO-WAIT.
                    IF AVAILABLE b-oe-ordl THEN 
                    DO:
                        IF itemfg.prod-uom EQ "M" THEN
                            b-oe-ordl.cost = itemfg.total-std-cost.
                        ELSE
                            RUN sys/ref/convcuom.p((IF LOOKUP(itemfg.prod-uom,fg-uom-list) GT 0
                                THEN "EA" ELSE itemfg.prod-uom),
                                "M", 0, 0, 0, 0,
                                itemfg.total-std-cost, OUTPUT b-oe-ordl.cost).
                        LEAVE.
                    END.
                END.
            END.
        END.
    END.
    IF fgPostLog THEN RUN fgPostLog ('End Third For Each w-fg-rctd').

    IF v-fgpostgl NE "None" THEN 
    DO TRANSACTION:
        /* gdm - 11050906 */
        REPEAT:
            FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
            IF AVAILABLE gl-ctrl THEN 
            DO:
                ASSIGN 
                    v-trnum       = gl-ctrl.trnum + 1
                    gl-ctrl.trnum = v-trnum.

                FIND CURRENT gl-ctrl NO-LOCK.
         
                IF fgPostLog THEN RUN fgPostLog ('Begin Run gl-from-work 1').         
                RUN gl-from-work (1, v-trnum).
                IF fgPostLog THEN RUN fgPostLog ('End 1 - Begin Run gl-from-work 2').
                RUN gl-from-work (2, v-trnum).
                IF fgPostLog THEN RUN fgPostLog ('End Run gl-from-work 2').
         
                LEAVE.
            END. /* IF AVAIL gl-ctrl */
        END. /* REPEAT */
    /* gdm - 11050906 */
    END.
    FIND FIRST w-job NO-ERROR.
    IF AVAILABLE w-job THEN 
    DO:
        IF fgPostLog THEN RUN fgPostLog ('Start jc/d-jclose.p').
        RUN jc/d-jclose.w.
        IF fgPostLog THEN RUN fgPostLog ('End jc/d-jclose.p').
    END.

    IF v-adjustgl THEN 
    DO TRANSACTION:
        /** GET next G/L TRANS. POSTING # **/
        FIND FIRST gl-ctrl WHERE gl-ctrl.company EQ cocode EXCLUSIVE-LOCK.
        ASSIGN
            v-trnum       = gl-ctrl.trnum + 1
            gl-ctrl.trnum = v-trnum.
        FIND CURRENT gl-ctrl NO-LOCK.
        IF fgPostLog THEN RUN fgPostLog ('Start For Each work-job').
        FOR EACH work-job BREAK BY work-job.actnum:
            CREATE gltrans.
            ASSIGN
                gltrans.company = cocode
                gltrans.actnum  = work-job.actnum
                gltrans.jrnl    = "ADJUST"
                gltrans.tr-date = v-post-date
                gltrans.period  = period.pnum
                gltrans.trnum   = v-trnum.

            IF work-job.fg THEN
                ASSIGN
                    gltrans.tr-amt  = - work-job.amt
                    gltrans.tr-dscr = "ADJUSTMENT FG".
            ELSE
                ASSIGN
                    gltrans.tr-amt  = work-job.amt
                    gltrans.tr-dscr = "ADJUSTMENT COGS".

            RELEASE gltrans.
        END. /* each work-job */
        IF fgPostLog THEN RUN fgPostLog ('End For Each work-job').
    END.
    IF v-got-fgemail THEN 
    DO:
        IF fgPostLog THEN RUN fgPostLog ('Start Run send-fgemail').
        RUN send-fgemail (v-fgemail-file).
        IF fgPostLog THEN RUN fgPostLog ('End Run send-fgemail').
    END.
    IF fgPostLog THEN RUN fgPostLog ('End').
    IF fgPostLog THEN OUTPUT STREAM logFile CLOSE.
    SESSION:SET-WAIT-STATE ("").
/* testing RUN adm-delete-record . */
/*  testing RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ).  */
/* wfk - out temporarily         */ 

END PROCEDURE.


PROCEDURE gl-from-work :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-run AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ip-trnum AS INTEGER NO-UNDO.
  
    DEFINE VARIABLE credits AS DECIMAL INIT 0 NO-UNDO.
    DEFINE VARIABLE debits  AS DECIMAL INIT 0 NO-UNDO. 

  
    FIND FIRST period
        WHERE period.company EQ cocode
        AND period.pst     LE v-post-date
        AND period.pend    GE v-post-date
        NO-LOCK.

    FOR EACH work-gl 
        WHERE (ip-run EQ 1 AND work-gl.job-no NE "")
        OR (ip-run EQ 2 AND work-gl.job-no EQ "")
        BREAK BY work-gl.actnum:
      
        ASSIGN
            debits  = debits  + work-gl.debits
            credits = credits + work-gl.credits.

        IF LAST-OF(work-gl.actnum) THEN 
        DO:
            CREATE gltrans.
            ASSIGN
                gltrans.company = cocode
                gltrans.actnum  = work-gl.actnum
                gltrans.jrnl    = "FGPOST"
                gltrans.period  = period.pnum
                gltrans.tr-amt  = debits - credits
                gltrans.tr-date = v-post-date
                gltrans.tr-dscr = IF work-gl.job-no NE "" THEN "FG Receipt from Job"
                                                 ELSE "FG Receipt from PO"
                gltrans.trnum   = ip-trnum
                debits          = 0
                credits         = 0.

            RELEASE gltrans.
        END.
    END.


END PROCEDURE.
