/* jc/jc-calc.p
  09/14/04  YSK  TASK 09130412   */
/* CF Production Control Test Run */

DEFINE INPUT PARAMETER rec-id AS RECID NO-UNDO.
DEFINE INPUT PARAMETER ip-recalc-stds-ju1 AS LOG NO-UNDO.
DEFINE VARIABLE gvlQtyFromJob AS LOG NO-UNDO INIT ?.
DEFINE VARIABLE gvlNoPrompt   AS LOG NO-UNDO.
{sys/inc/var.i SHARED}
DEFINE VARIABLE ii AS INTEGER.

DEFINE NEW SHARED BUFFER xest      FOR est.
DEFINE NEW SHARED BUFFER xef       FOR ef.
DEFINE NEW SHARED BUFFER xeb       FOR eb.
DEFINE NEW SHARED BUFFER xeb2      FOR eb.

DEFINE            BUFFER x-job     FOR job.
DEFINE            BUFFER x-item    FOR item.
DEFINE            BUFFER b-oe-ordl FOR oe-ordl.

DEFINE NEW SHARED VARIABLE v-rebuild      AS LOG       FORMAT "R/E" NO-UNDO.

DEFINE            VARIABLE v-est-qty      AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE save_id        AS RECID     NO-UNDO.
DEFINE            VARIABLE v-est-job      LIKE job.est-no NO-UNDO.
DEFINE            VARIABLE v-up           LIKE job-mat.n-up NO-UNDO.
DEFINE            VARIABLE v-out          LIKE ef.n-out NO-UNDO.
DEFINE            VARIABLE v-job-hdr      AS LOG       NO-UNDO.
DEFINE            VARIABLE v-hold-qty     LIKE job-hdr.qty NO-UNDO.
DEFINE            VARIABLE choice         AS LOG       NO-UNDO.
DEFINE            VARIABLE v-yld-qty      AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE li             AS INTEGER   NO-UNDO.
DEFINE            VARIABLE ld             AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE lv-format-f    AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lv-date        AS DATE      NO-UNDO.
DEFINE            VARIABLE v-on-f         AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-blk-qty      AS INTEGER   NO-UNDO.
DEFINE            VARIABLE ll-hold-qty    AS LOG       NO-UNDO.
DEFINE            VARIABLE ll-all-warn    AS LOG       NO-UNDO.
DEFINE            VARIABLE ll-add-over    AS LOG       NO-UNDO.
DEFINE            VARIABLE ll-whs-item    AS LOG       NO-UNDO.
DEFINE            VARIABLE ll-qty-changed AS LOG       NO-UNDO.

DEFINE            VARIABLE K_FRAC         AS DECIMAL   INIT 6.25 NO-UNDO.

DEFINE SHARED     VARIABLE nufile         AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE qty            AS INTEGER   NO-UNDO.
DEFINE NEW SHARED VARIABLE v-shared-rel   AS INTEGER   NO-UNDO.

DO TRANSACTION:
    {ce/print4.i "new SHARED" "new SHARED"}
END.

{ce/print42.i "new SHARED"}

DEFINE TEMP-TABLE work-ord NO-UNDO
    FIELD cust-no LIKE job-hdr.cust-no
    FIELD ord-no  LIKE job-hdr.ord-no.

DEFINE BUFFER bf-blk FOR blk.

DEFINE VARIABLE type-chk           AS CHARACTER INIT "C,D,F,G,I,L,M,P,R,T,V,W,B,1,2,3,4,5,6,7,8,9,X,Y,@" NO-UNDO.
DEFINE VARIABLE type-mat           LIKE item.mat-type NO-UNDO.
DEFINE VARIABLE over-pct           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE ll-use-netsht      AS LOG       NO-UNDO.
DEFINE VARIABLE v-qty              AS INTEGER   NO-UNDO.

DEFINE VARIABLE v-set-hdr          LIKE oe-ordl.i-no.
DEFINE VARIABLE v-item-no          LIKE itemfg.i-no.
DEFINE VARIABLE v-part-qty         AS DECIMAL.

DEFINE VARIABLE ll-one-part        AS LOG       NO-UNDO.
DEFINE VARIABLE ll-new-job-hdr     AS LOG       NO-UNDO.
DEFINE VARIABLE ll-sep-prep        AS LOG       NO-UNDO.
DEFINE VARIABLE blankNumber        AS INTEGER   NO-UNDO.
DEFINE VARIABLE lv-sell-by-ce-ctrl AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-sell-by         AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-pct-2            LIKE eb.comm NO-UNDO.
DEFINE VARIABLE v-comm-2           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-tot-comm-2       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-basis            AS CHARACTER NO-UNDO.
DEFINE VARIABLE ll-use-margin      AS LOG       NO-UNDO.
DEFINE VARIABLE v-sell-price       AS DECIMAL   DECIMALS 2 NO-UNDO.
DEFINE VARIABLE v-probe-comm       AS DECIMAL   DECIMALS 5 NO-UNDO.
DEFINE VARIABLE v-mp               AS DECIMAL   DECIMALS 5 NO-UNDO.
DEFINE VARIABLE v-qty-2            AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-board-cst        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE ll-jqcust          AS LOG       NO-UNDO.
DEFINE VARIABLE blk-fact           LIKE blk.fact NO-UNDO.
DEFINE VARIABLE ll-combo-req-qty   AS LOG       NO-UNDO.
DEFINE VARIABLE ll-fgoecost        AS LOG       NO-UNDO.
DEFINE VARIABLE ll-recalc-cost     AS LOG       NO-UNDO.
DEFINE VARIABLE ll-oe-program      AS LOG       NO-UNDO.

DEFINE BUFFER x-eb            FOR eb.
DEFINE BUFFER x-job-hdr       FOR job-hdr.
DEFINE BUFFER b-ef2           FOR ef.
DEFINE BUFFER b-eb2           FOR eb.
DEFINE BUFFER b-blk           FOR blk.
DEFINE BUFFER b-print-job-hdr FOR job-hdr.

DEFINE TEMP-TABLE tt-job-mch NO-UNDO LIKE job-mch.
DEFINE TEMP-TABLE tt-job-mat NO-UNDO LIKE job-mat.
DEFINE TEMP-TABLE tt-ordlist NO-UNDO
    FIELD ord-no LIKE oe-ord.ord-no
    INDEX ord-no ord-no.

{oe/oe-sysct1.i NEW}
 
{ce/msfcalc.i}

{sys/inc/f16to32.i}
    
DO TRANSACTION:
    {sys/inc/graphic.i}
END.

DEFINE VARIABLE hProc AS HANDLE NO-UNDO.
hProc = SESSION:FIRST-PROCEDURE.
DO WHILE VALID-HANDLE(hProc):
    IF INDEX(hProc:FILE-NAME, "v-job") GT 0 
        OR
        index(hProc:FILE-NAME, "impord") GT 0 THEN
        LEAVE. /* found it. */
    hProc = hProc:NEXT-SIBLING.
END.


IF VALID-HANDLE(hProc) THEN 
DO:
    IF INDEX(hProc:FILE-NAME, "impord") GT 0 THEN 
        gvlNoPrompt = TRUE.
    RUN getUseJobQty IN hProc (OUTPUT gvlQtyFromJob).    

END.
 
FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "JOBCARDF"
    NO-LOCK NO-ERROR.
IF AVAILABLE sys-ctrl THEN lv-format-f = sys-ctrl.char-fld.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "JOBCREAT"
    NO-LOCK NO-ERROR.
IF AVAILABLE sys-ctrl THEN ll-sep-prep = sys-ctrl.dec-fld EQ 1.

ll-fgoecost = YES.
FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "FGOECOST"
    NO-LOCK NO-ERROR.
IF AVAILABLE sys-ctrl THEN ll-fgoecost = NOT sys-ctrl.log-fld.
ll-recalc-cost = ll-fgoecost.
ll-oe-program = NO.
DO ii = 1 TO 10:
    IF INDEX(PROGRAM-NAME(ii), "oe/vp-oeitm") > 0 THEN 
        ll-oe-program = YES.    
END.
IF NOT ll-oe-program THEN
    ll-recalc-cost = YES.

{sys/ref/fgoecost.i}
  
DO TRANSACTION:
    {sys/inc/cerun.i C}  
END.

DO TRANSACTION:
    {sys/inc/cecomm.i}  
END.

DO TRANSACTION:
    {sys/inc/overwriteJobPlan.i}  
END.

FIND FIRST ce-ctrl {sys/look/ce-ctrlW.i} NO-LOCK NO-ERROR.

RUN oe/oe-sysct.p.

FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name EQ "JOB QTY"
    NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN 
DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company = cocode
        sys-ctrl.name    = "JOB QTY"
        sys-ctrl.descrip = "Create Job Quantity with overrun % from OE?"
        sys-ctrl.log-fld = NO.
    MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.
END.
ASSIGN
    ll-add-over      = sys-ctrl.log-fld
    ll-use-netsht    = sys-ctrl.char-fld EQ "Net Shts"
    ll-combo-req-qty = sys-ctrl.int-fld EQ 0.

FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name EQ "JOBQTYCUST"
    NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN 
DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company = cocode
        sys-ctrl.module  = "JC"
        sys-ctrl.name    = "JOBQTYCUST"
        sys-ctrl.descrip = "Create Job Quantity with overrun % from customer if no order?"
        sys-ctrl.log-fld = NO.
END.
ll-jqcust = sys-ctrl.log-fld.

FIND FIRST jc-ctrl WHERE jc-ctrl.company EQ cocode NO-LOCK NO-ERROR.
 
IF nufile EQ ? THEN
    ASSIGN
        nufile      = YES
        ll-hold-qty = YES.

mainloop:
DO:
    FIND job WHERE RECID(job) EQ rec-id NO-ERROR.

    IF NOT AVAILABLE job THEN 
    DO:
        FIND job-hdr WHERE RECID(job-hdr) EQ rec-id NO-LOCK NO-ERROR.
        IF AVAILABLE job-hdr THEN
            FIND FIRST job
                WHERE job.company EQ job-hdr.company
                AND job.job     EQ job-hdr.job
                AND job.job-no  EQ job-hdr.job-no
                AND job.job-no2 EQ job-hdr.job-no2
                NO-ERROR.
        RELEASE job-hdr.
    END.

    IF NOT AVAILABLE job THEN RETURN.
  
    IF nufile THEN 
    DO:


        FOR EACH job-hdr
            WHERE job-hdr.company EQ cocode
            AND job-hdr.job     EQ job.job
            AND job-hdr.job-no  EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2
            AND job-hdr.qty     EQ 0 TRANSACTION:
            DELETE job-hdr.
        END.

        EMPTY TEMP-TABLE work-ord.

        FOR EACH job-hdr
            WHERE job-hdr.company EQ cocode
            AND job-hdr.job     EQ job.job
            AND job-hdr.job-no  EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2
            AND job-hdr.ord-no  NE 0
            NO-LOCK
            BREAK BY job-hdr.cust-no TRANSACTION:
    
            IF LAST-OF(job-hdr.cust-no) THEN 
            DO:
                CREATE work-ord.
                ASSIGN
                    work-ord.cust-no = job-hdr.cust-no
                    work-ord.ord-no  = job-hdr.ord-no.
            END.  
        END.
    END.
  
    FIND FIRST xest
        WHERE xest.company EQ cocode
        AND xest.est-no  EQ job.est-no
        NO-ERROR.
    IF NOT AVAILABLE xest THEN 
    DO:
        choice = NO.
        IF NOT gvlNoPrompt THEN
            MESSAGE "There is no estimate to build from for this job.  "
                "Would you LIKE to create one?  " 
                VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE choice .
        IF NOT choice THEN UNDO, LEAVE.
        IF choice THEN 
        DO WHILE TRUE:
            MESSAGE "Enter the Estimate Number you wish to build from:  "
                UPDATE v-est-job.             
            RUN util/rjust.p (INPUT-OUTPUT v-est-job, INPUT 5).
            FIND FIRST xest WHERE xest.company = cocode
                AND xest.loc = locode
                AND xest.est-no EQ v-est-job NO-ERROR.
            IF NOT AVAILABLE xest THEN
                MESSAGE "Estimate Does Not Exist. Please Re-enter." VIEW-AS ALERT-BOX ERROR.
            IF AVAILABLE xest THEN 
            DO:
                DO TRANSACTION:
                    ASSIGN
                        job.est-no  = xest.est-no
                        job.rec_key = IF job.rec_key = "" THEN xest.rec_key ELSE job.rec_key.  /* for notes */
                END.

                FOR EACH job-hdr WHERE job-hdr.company EQ cocode
                    AND job-hdr.job     EQ job.job
                    AND job-hdr.job-no  EQ job.job-no
                    AND job-hdr.job-no2 EQ job.job-no2,
                    EACH oe-ordl WHERE oe-ordl.company EQ job-hdr.company
                    AND oe-ordl.job-no  EQ job-hdr.job-no
                    AND oe-ordl.job-no2 EQ job-hdr.job-no2
                    AND oe-ordl.ord-no  EQ job-hdr.ord-no,
                    FIRST oe-ord OF oe-ordl TRANSACTION:
 
                    ASSIGN 
                        job-hdr.est-no = job.est-no
                        oe-ordl.est-no = job.est-no
                        oe-ord.est-no  = job.est-no.
                END.
                LEAVE.
            END. /* avail xest */     
        END.  /* choice */
    END. /* not avail xest */
  
    li = 0.

    FOR EACH xeb
        WHERE xeb.company   EQ xest.company 
        AND xeb.est-no    EQ xest.est-no
        AND xeb.form-no   EQ 0
        AND (xeb.est-type NE xest.est-type OR
        (xest.est-type NE 2 AND xest.est-type NE 6)) TRANSACTION:
        DELETE xeb.
    END.
  
    IF nufile THEN
        FOR EACH xeb WHERE xeb.company = xest.company 
            AND xeb.est-no = xest.est-no
            BREAK BY xeb.est-no
            BY xeb.form-no
            BY xeb.blank-no TRANSACTION:
            IF lv-format-f EQ "CentBox" OR lv-format-f EQ "Accord" THEN
                ASSIGN
                    li         = li + 1
                    xeb.spc-no = TRIM(job.job-no)         + "-" +
                      STRING(job.job-no2,"99") + "-" +
                      STRING(xeb.form-no,"99") + "-" +
                      STRING(li,"99").
      
            FIND FIRST oe-ordl
                WHERE oe-ordl.company    EQ cocode
                AND oe-ordl.job-no     EQ job.job-no
                AND oe-ordl.job-no2    EQ job.job-no2
                AND ((oe-ordl.form-no  EQ xeb.form-no AND
                oe-ordl.blank-no EQ xeb.blank-no) OR
                xeb.est-type EQ 2 OR xeb.est-type EQ 6)
                NO-LOCK NO-ERROR.

            IF xeb.stock-no EQ "" AND AVAILABLE oe-ordl THEN xeb.stock-no = oe-ordl.i-no.
      
            FIND FIRST itemfg WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ xeb.stock-no
                NO-LOCK NO-ERROR.

            /* choice = YES.
             if not avail itemfg OR xeb.stock-no EQ "" THEN DO WHILE TRUE  :
               IF NOT choice OR xeb.stock-no EQ '' THEN DO:
                 choice = NO.
                 IF NOT gvlNoPrompt THEN
                   RUN jc/fgPrompt.w (cocode,xeb.cust-no,xeb.part-no,recid(xeb),INPUT-OUTPUT xeb.stock-no).
                 IF xeb.stock-no EQ '' THEN NEXT.
               END.
       
               FIND FIRST itemfg
                     {sys/look/itemfgrlW.i}
                       and itemfg.i-no   EQ xeb.stock-no
                     NO-LOCK NO-ERROR.
                 if not avail itemfg THEN do on endkey undo, NEXT :
                   choice = yes.
                   IF gvlNoPrompt THEN
                       choice = NO.
                   IF NOT gvlNoPrompt THEN
                   message "Item: " + TRIM(xeb.stock-no) + " doesn't exist, would you LIKE to create it?"
                           view-as alert-box question button yes-no update choice.
                   if choice THEN do:
                     {jc/fgadd.i} 
                     leave.
                   END.
                 END.
                 ELSE IF itemfg.est-no EQ '' THEN LEAVE.
             END.*/

            choice = YES.
            IF NOT AVAILABLE itemfg OR xeb.stock-no EQ "" THEN 
            DO  :
       
                IF NOT gvlNoPrompt AND xeb.stock-no EQ "" THEN
                    RUN jc/fgPrompt.w (cocode,xeb.cust-no,xeb.part-no,RECID(xeb),INPUT-OUTPUT xeb.stock-no).
          
                IF xeb.stock-no NE "" THEN
                    FIND FIRST itemfg
                    {sys/look/itemfgrlW.i}
                AND itemfg.i-no   EQ xeb.stock-no
                        NO-LOCK NO-ERROR.
                IF NOT AVAILABLE itemfg THEN 
                DO ON ENDKEY UNDO, NEXT :
                    choice = YES.
                    IF gvlNoPrompt THEN
                        choice = NO.
                    IF NOT gvlNoPrompt THEN
                        MESSAGE "Item: " + TRIM(xeb.stock-no) + " doesn't exist, would you LIKE to create it?"
                            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice.
                    IF choice THEN 
                    DO:
                    {jc/fgadd.i} 
                    END.
                END.
         
            END.

            IF xeb.form-no EQ 0 THEN 
            DO:
                v-set-hdr = xeb.stock-no.      
                FIND FIRST eb WHERE eb.company = xeb.company
                    AND eb.est-no   EQ xeb.est-no
                    AND eb.cust-no NE ""
                    NO-LOCK NO-ERROR.
            END.
           
            IF xest.est-type EQ 3 OR
                xest.est-type EQ 4 OR
                xest.est-type EQ 8 THEN
                FIND FIRST job-hdr WHERE job-hdr.company EQ cocode
                    AND job-hdr.job     EQ job.job
                    AND job-hdr.job-no  EQ job.job-no
                    AND job-hdr.job-no2 EQ job.job-no2
                    AND job-hdr.frm     EQ xeb.form-no
                    AND job-hdr.i-no    EQ xeb.stock-no
                    NO-ERROR.          
            ELSE
                IF xest.est-type EQ 1 OR
                    xest.est-type EQ 5 OR
                    xeb.form-no EQ 0   THEN
                    FIND FIRST job-hdr WHERE job-hdr.company  EQ cocode
                        AND job-hdr.job      EQ job.job
                        AND job-hdr.job-no   EQ job.job-no
                        AND job-hdr.job-no2  EQ job.job-no2 NO-ERROR.          
                ELSE 
                DO:
                    IF LAST(xeb.est-no)                           AND
                        (xest.est-type EQ 2 OR xest.est-type EQ 6) THEN 
                    DO:
                        {fg/addset.i v-set-hdr}
                    END.   
                    NEXT.
                END.

            RUN jc/job4rel.p (BUFFER job, BUFFER oe-rel).

            ll-new-job-hdr = NOT AVAILABLE job-hdr.

            IF ll-new-job-hdr THEN 
            DO:
                CREATE job-hdr.
                ASSIGN 
                    job-hdr.company    = cocode
                    job-hdr.loc        = locode
                    job-hdr.e-num      = xest.e-num
                    job-hdr.est-no     = xest.est-no
                    job-hdr.i-no       = xeb.stock-no
                    job-hdr.job-no     = job.job-no
                    job-hdr.job-no2    = job.job-no2
                    job-hdr.cust-no    = IF xeb.form-no EQ 0 AND AVAILABLE eb THEN eb.cust-no
                                       ELSE xeb.cust-no
                    job-hdr.frm        = xeb.form-no
                    job-hdr.job        = job.job
                    job-hdr.start-date = job.start-date                           
                    job-hdr.due-date   = job.due-date.
       
                FIND FIRST work-ord WHERE work-ord.cust-no EQ job-hdr.cust-no NO-ERROR.
                IF AVAILABLE work-ord THEN 
                DO: 
                    job-hdr.ord-no = work-ord.ord-no.

                    FIND FIRST b-oe-ordl WHERE
                        b-oe-ordl.company   EQ cocode 
                        AND b-oe-ordl.opened    EQ YES 
                        AND b-oe-ordl.ord-no    EQ job-hdr.ord-no
                        AND b-oe-ordl.i-no      EQ job-hdr.i-no NO-LOCK NO-ERROR.
                    IF AVAILABLE b-oe-ordl THEN
                        job-hdr.po-no = b-oe-ordl.po-no.
                END.

                {util/mkjobkey.i}

            END.
            IF job-hdr.frm EQ 0 THEN job-hdr.frm = 1.

            job-hdr.blank-no = IF xeb.form-no EQ 0 THEN 1 ELSE xeb.blank-no.
            RUN util/upditmfg.p (ROWID(job-hdr), -1).
            RUN jc/qty-changed.p (BUFFER job, OUTPUT ll-qty-changed).      

            /* wfk - If user makes a choice, allow it to go to this block */
            IF NOT ll-qty-changed OR gvlQtyFromJob NE ? THEN 
            DO:
                ASSIGN
                    v-qty      = 0
                    v-hold-qty = job-hdr.qty.
         
                IF NOT gvlQtyFromJob OR gvlQtyFromjob = ? THEN 
                DO:              
                    IF xest.est-type EQ 4 OR
                        xest.est-type EQ 8 OR
                        xest.est-type EQ 3 THEN
                        FOR EACH eb FIELDS(form-no blank-no yld-qty bl-qty) WHERE
                            eb.company  EQ xest.company AND
                            eb.est-no   EQ xest.est-no AND
                            eb.stock-no EQ xeb.stock-no
                            NO-LOCK :
                
                            IF eb.form-no EQ xeb.form-no AND eb.blank-no EQ xeb.blank-no THEN
                                job-hdr.qty = IF ll-combo-req-qty EQ YES THEN eb.bl-qty ELSE eb.yld-qty.
               
                            v-qty = v-qty + (IF ll-combo-req-qty EQ YES THEN eb.bl-qty ELSE eb.yld-qty).
                        END.
                    ELSE 
                    DO:
                        ASSIGN
                            v-qty       = xest.est-qty[1]
                            job-hdr.qty = xest.est-qty[1].
                    END.
             
                END.
                ELSE 
                    ASSIGN v-qty = job-hdr.qty. 
         
                IF job-hdr.qty EQ 0 THEN job-hdr.qty = 1.
                IF v-qty       EQ 0 THEN v-qty       = 1.
                RUN jc/jobhordl.p (BUFFER job-hdr, BUFFER oe-rel, BUFFER oe-ordl).

                IF (ll-hold-qty OR
                    (ip-recalc-stds-ju1 EQ NO AND
                    job-hdr.ord-no EQ 0 AND NOT AVAILABLE oe-rel)) AND
                    v-hold-qty NE 0 THEN 
                DO: 
                    job-hdr.qty = v-hold-qty.

                END.
                ELSE
                    IF AVAILABLE oe-ordl THEN 
                    DO:
                        IF ll-combo-req-qty EQ YES THEN 
                        DO: 
                            job-hdr.qty = (IF AVAILABLE oe-rel THEN oe-rel.qty            
                            ELSE oe-ordl.qty) *
                                (job-hdr.qty / v-qty).
                        END.
                        ELSE
                        DO:
                            IF NOT (xest.est-type EQ 4 OR
                                xest.est-type EQ 8 OR
                                xest.est-type EQ 3) THEN 
                                job-hdr.qty = (IF AVAILABLE oe-rel THEN oe-rel.qty
                                ELSE oe-ordl.qty) *
                                    (job-hdr.qty / v-qty).
                            ELSE
                                job-hdr.qty = job-hdr.qty *
                                    (job-hdr.qty / v-qty).
                        END.
           
                        IF NOT AVAILABLE oe-rel THEN job-hdr.ord-no = oe-ordl.ord-no.
                        ll-whs-item = oe-ordl.managed.
                        IF NOT ll-whs-item AND ll-add-over THEN
                            RUN oe/overundr.p ("O", "+", oe-ordl.over-pct, job-hdr.qty,
                                OUTPUT job-hdr.qty).
                    END.

                IF ip-recalc-stds-ju1 AND
                    ll-jqcust EQ YES AND
                    NOT AVAILABLE oe-ordl AND
                    job-hdr.ord-no EQ 0 AND
                    NOT AVAILABLE oe-rel AND
                    NOT ll-new-job-hdr THEN
                DO:
                    FIND FIRST cust WHERE
                        cust.company EQ job-hdr.company AND
                        cust.cust-no EQ job-hdr.cust-no
                        NO-LOCK NO-ERROR.
            
                    IF AVAILABLE cust THEN
                    DO:
                        job-hdr.qty = job-hdr.qty + (job-hdr.qty * cust.over-pct * .01).
                        {sys/inc/roundup.i job-hdr.qty}
                    END.
                END.
            END.
      
            IF AVAILABLE oe-ordl THEN job-hdr.due-date = oe-ordl.req-date.
            
            IF job-hdr.qty EQ 0 OR
                (job-hdr.ord-no EQ 0 AND
                NOT AVAILABLE oe-rel    AND
                ll-new-job-hdr) THEN
            DO:
                IF NOT gvlNoPrompt THEN
                    RUN jc/jobHdrQty.w ("Please enter job qty for FG#/Form#/Blank#: " +
                        TRIM(job-hdr.i-no) + "/" +
                        STRING(job-hdr.frm,"99") + "/" +
                        STRING(job-hdr.blank-no,"99") + "...",
                        INPUT-OUTPUT job-hdr.qty).
           
                IF ll-jqcust EQ YES AND NOT AVAILABLE oe-ordl THEN
                DO:
                    FIND FIRST cust WHERE
                        cust.company EQ job-hdr.company AND
                        cust.cust-no EQ job-hdr.cust-no
                        NO-LOCK NO-ERROR.
           
                    IF AVAILABLE cust THEN
                    DO:
                        job-hdr.qty = job-hdr.qty + (job-hdr.qty * cust.over-pct * .01).
                  {sys/inc/roundup.i job-hdr.qty}
                    END.
                END.
            END.

            RUN util/upditmfg.p (ROWID(job-hdr), 1).

            FOR EACH x-job-hdr
                WHERE x-job-hdr.company EQ job-hdr.company
                AND x-job-hdr.job     EQ job-hdr.job
                AND x-job-hdr.job-no  EQ job-hdr.job-no
                AND x-job-hdr.job-no2 EQ job-hdr.job-no2
                AND x-job-hdr.frm     EQ job-hdr.frm
                AND x-job-hdr.i-no    EQ job-hdr.i-no
                AND ROWID(x-job-hdr)  NE ROWID(job-hdr):

                RUN util/upditmfg.p (ROWID(x-job-hdr), -1) .

                DELETE x-job-hdr.
            END.
     
        END.  /* FOR EACH xeb */

    FOR EACH job-hdr
        WHERE job-hdr.company EQ job.company
        AND job-hdr.job     EQ job.job
        AND job-hdr.job-no  EQ job.job-no
        AND job-hdr.job-no2 EQ job.job-no2
        AND NOT CAN-FIND(FIRST xeb
        WHERE xeb.company      EQ job-hdr.company
        AND xeb.est-no       EQ job-hdr.est-no
        AND xeb.stock-no     EQ job-hdr.i-no
        AND ((xeb.form-no    EQ 0 AND
        xeb.blank-no   EQ 0 AND
        (xest.est-type EQ 2 OR xest.est-type EQ 6)) OR
        (xeb.form-no    EQ job-hdr.frm AND
        xeb.blank-no   EQ job-hdr.blank-no AND
        xest.est-type  NE 2 AND xest.est-type NE 6)))
        TRANSACTION:
        RUN util/upditmfg.p (ROWID(job-hdr), -1).

        DELETE job-hdr.
    END.
    DO TRANSACTION:
        ASSIGN
            job.est-no      = xest.est-no
            job.create-date = TODAY
            job.rec_key     = IF job.rec_key = "" THEN xest.rec_key ELSE job.rec_key
            job.stat          = "R"
            job.create-time = TIME.
    END.  
  
  
    FIND CURRENT job NO-LOCK.

    /* calc-est.p calls print4.p or print42.p to create op temp-table */
    RUN jc/calc-est.p (RECID(job)).

    FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "SCHEDULE" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN 
    DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN 
            sys-ctrl.company  = cocode
            sys-ctrl.name     = "SCHEDULE"
            sys-ctrl.char-fld = "None"
            sys-ctrl.descrip  = "Update Due date and Promise date for Schedule?".
        IF NOT gvlNoPrompt THEN
            MESSAGE sys-ctrl.descrip
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE sys-ctrl.log-fld.                             
    END.

    IF nufile THEN RUN jc/startdat.p (ROWID(job)).
    DO TRANSACTION:
        FIND CURRENT job EXCLUSIVE-LOCK.


        IF nufile AND AVAILABLE sys-ctrl AND sys-ctrl.char-fld = "NoDate" AND sys-ctrl.log-fld
            THEN job.start-date = ?.

        ASSIGN
            job.std-fix-cost = 0
            job.std-lab-cost = 0
            job.std-mat-cost = 0
            job.std-var-cost = 0.
    END.
  
    IF job.job NE 0 THEN
        FOR EACH reftable
            WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job.job,"999999999")
            TRANSACTION:
            DELETE reftable.
        END.

    FOR EACH job-hdr EXCLUSIVE-LOCK
        WHERE job-hdr.company EQ job.company
        AND job-hdr.job     EQ job.job
        AND job-hdr.job-no  EQ job.job-no
        AND job-hdr.job-no2 EQ job.job-no2
        TRANSACTION:

        job-hdr.ftick-prnt = NO.

        IF job-hdr.sq-in EQ 0 THEN 
        DO:
            IF xest.est-type EQ 2 OR xest.est-type EQ 6 THEN job-hdr.sq-in = 100.00.
            ELSE 
            DO:
                FIND FIRST xjob
                    WHERE xjob.form-no  EQ job-hdr.frm
                    AND xjob.blank-no EQ job-hdr.blank-no
                    AND xjob.stock-no EQ job-hdr.i-no
                    NO-ERROR.
                IF AVAILABLE xjob THEN
                    ASSIGN
                        job-hdr.blank-no = xjob.blank-no
                        job-hdr.frm      = xjob.form-no
                        job-hdr.sq-in    = xjob.pct * 100.
            END.
        END.

        ASSIGN
            job-hdr.lock         = YES
            job-hdr.std-lab-cost = 0
            job-hdr.std-mat-cost = 0
            job-hdr.std-var-cost = 0
            job-hdr.std-fix-cost = 0.

        FOR EACH xjob
            WHERE (xjob.form-no  EQ job-hdr.frm AND
            xjob.blank-no EQ job-hdr.blank-no AND
            xjob.stock-no EQ job-hdr.i-no)
            OR xest.est-type EQ 2
            OR xest.est-type EQ 6,

            FIRST eb
            WHERE eb.company  EQ job.company
            AND eb.est-no   EQ job.est-no
            AND eb.form-no  EQ xjob.form-no
            AND eb.blank-no EQ xjob.blank-no
            NO-LOCK:

            ASSIGN
                job-hdr.std-lab-cost = job-hdr.std-lab-cost + xjob.lab
                job-hdr.std-mat-cost = job-hdr.std-mat-cost + xjob.mat
                job-hdr.std-var-cost = job-hdr.std-var-cost + xjob.voh
                job-hdr.std-fix-cost = job-hdr.std-fix-cost + xjob.foh.

            FIND FIRST reftable
                WHERE reftable.reftable EQ "jc/jc-calc.p"
                AND reftable.company  EQ job.company
                AND reftable.loc      EQ ""
                AND reftable.code     EQ STRING(job.job,"999999999")
                AND reftable.code2    EQ eb.stock-no
                AND reftable.val[12]  EQ eb.form-no
                AND reftable.val[13]  EQ eb.blank-no
                USE-INDEX reftable NO-ERROR.
            IF NOT AVAILABLE reftable THEN 
            DO:
                CREATE reftable.
                ASSIGN
                    reftable.reftable = "jc/jc-calc.p"
                    reftable.company  = job.company
                    reftable.loc      = ""
                    reftable.code     = STRING(job.job,"999999999")
                    reftable.code2    = eb.stock-no
                    reftable.val[12]  = eb.form-no
                    reftable.val[13]  = eb.blank-no
                    reftable.val[11]  = eb.num-up.
            END.

            ASSIGN
                v-yld-qty       = IF eb.est-type EQ 2 THEN eb.cust-% ELSE eb.quantityPerSet
                v-yld-qty       = IF v-yld-qty LT 0 THEN (-1 / v-yld-qty) ELSE v-yld-qty
                reftable.val[1] = reftable.val[1] + (xjob.lab / v-yld-qty)
                reftable.val[2] = reftable.val[2] + (xjob.mat / v-yld-qty)
                reftable.val[3] = reftable.val[3] + (xjob.voh / v-yld-qty)
                reftable.val[4] = reftable.val[4] + (xjob.foh / v-yld-qty)
                reftable.val[5] = reftable.val[1] + reftable.val[2] +
                         reftable.val[3] + reftable.val[4].

            FIND CURRENT reftable NO-LOCK.
            RELEASE reftable.

            FIND FIRST itemfg
                WHERE itemfg.company EQ reftable.company
                AND itemfg.i-no    EQ reftable.code2
                NO-ERROR.

            IF AVAILABLE itemfg THEN RUN fg/calcqono.p (ROWID(itemfg), OUTPUT itemfg.q-ono).
        END.




        ASSIGN
            job-hdr.std-tot-cost = job-hdr.std-mat-cost + job-hdr.std-lab-cost +
                            job-hdr.std-fix-cost + job-hdr.std-var-cost
            job.std-fix-cost     = job.std-fix-cost     + job-hdr.std-fix-cost
            job.std-lab-cost     = job.std-lab-cost     + job-hdr.std-lab-cost
            job.std-mat-cost     = job.std-mat-cost     + job-hdr.std-mat-cost
            job.std-var-cost     = job.std-var-cost     + job-hdr.std-var-cost
            job.std-tot-cost     = job.std-fix-cost     + job.std-lab-cost +
                            job.std-mat-cost     + job.std-var-cost.

    


        FOR EACH eb
            WHERE eb.company  EQ job-hdr.company
            AND eb.est-no   EQ job-hdr.est-no
            AND eb.form-no  EQ job-hdr.frm
            AND eb.blank-no NE 0
            NO-LOCK
            BY eb.blank-no DESCENDING:
        
            job-hdr.n-on = eb.num-up.
            IF eb.blank-no EQ job-hdr.blank-no THEN LEAVE.
        END.
    END.
  
    EMPTY TEMP-TABLE tt-ordlist.
    FOR EACH job-hdr EXCLUSIVE-LOCK
        WHERE job-hdr.company EQ job.company
        AND job-hdr.job     EQ job.job
        AND job-hdr.job-no  EQ job.job-no
        AND job-hdr.job-no2 EQ job.job-no2
        AND job-hdr.ord-no  NE 0 
        BREAK BY job-hdr.i-no TRANSACTION:

        /* Save list of orders for calcordt.p after this loop */
        FIND FIRST tt-ordlist WHERE tt-ordlist.ord-no EQ job-hdr.ord-no
            NO-ERROR.
        IF NOT AVAILABLE tt-ordlist THEN 
        DO:
            CREATE tt-ordlist.
            ASSIGN 
                tt-ordlist.ord-no = job-hdr.ord-no.
        END.

        ACCUM job-hdr.std-mat-cost * job-hdr.qty (TOTAL BY job-hdr.i-no).
        ACCUM job-hdr.std-lab-cost * job-hdr.qty (TOTAL BY job-hdr.i-no).
        ACCUM job-hdr.std-fix-cost * job-hdr.qty (TOTAL BY job-hdr.i-no).
        ACCUM job-hdr.std-var-cost * job-hdr.qty (TOTAL BY job-hdr.i-no).
        ACCUM job-hdr.std-tot-cost * job-hdr.qty (TOTAL BY job-hdr.i-no).
        ACCUM job-hdr.qty                        (TOTAL BY job-hdr.i-no).

        IF LAST-OF(job-hdr.i-no) THEN 
        DO:
            ASSIGN
                job-hdr.std-mat-cost = (ACCUM TOTAL BY job-hdr.i-no job-hdr.std-mat-cost * job-hdr.qty) /
                              (ACCUM TOTAL BY job-hdr.i-no job-hdr.qty)
                job-hdr.std-lab-cost = (ACCUM TOTAL BY job-hdr.i-no job-hdr.std-lab-cost * job-hdr.qty) /
                              (ACCUM TOTAL BY job-hdr.i-no job-hdr.qty)
                job-hdr.std-fix-cost = (ACCUM TOTAL BY job-hdr.i-no job-hdr.std-fix-cost * job-hdr.qty) /
                              (ACCUM TOTAL BY job-hdr.i-no job-hdr.qty)
                job-hdr.std-var-cost = (ACCUM TOTAL BY job-hdr.i-no job-hdr.std-var-cost * job-hdr.qty) /
                              (ACCUM TOTAL BY job-hdr.i-no job-hdr.qty)
                job-hdr.std-tot-cost = (ACCUM TOTAL BY job-hdr.i-no job-hdr.std-tot-cost * job-hdr.qty) /
                              (ACCUM TOTAL BY job-hdr.i-no job-hdr.qty).

            FOR EACH x-job-hdr EXCLUSIVE
          WHERE x-job-hdr.company EQ job-hdr.company
            AND x-job-hdr.job     EQ job-hdr.job
            AND x-job-hdr.job-no  EQ job-hdr.job-no
            AND x-job-hdr.job-no2 EQ job-hdr.job-no2
            AND x-job-hdr.i-no    EQ job-hdr.i-no
            AND ROWID(x-job-hdr)  NE ROWID(job-hdr):
                ASSIGN
                    x-job-hdr.std-mat-cost = job-hdr.std-mat-cost
                    x-job-hdr.std-lab-cost = job-hdr.std-lab-cost
                    x-job-hdr.std-fix-cost = job-hdr.std-fix-cost
                    x-job-hdr.std-var-cost = job-hdr.std-var-cost
                    x-job-hdr.std-tot-cost = job-hdr.std-tot-cost.
            END.

            FOR EACH oe-ordl
                WHERE oe-ordl.company   EQ job-hdr.company
                AND oe-ordl.ord-no    EQ job-hdr.ord-no
                AND oe-ordl.i-no      EQ job-hdr.i-no
                AND oe-ordl.job-no    EQ job-hdr.job-no
                AND oe-ordl.job-no2   EQ job-hdr.job-no2,
                FIRST oe-ord NO-LOCK
                WHERE oe-ord.company EQ oe-ordl.company
                AND oe-ord.ord-no  EQ oe-ordl.ord-no:

                IF v-full-cost THEN 
                DO:
                    RELEASE blk.
                    IF xest.est-type EQ 3 OR
                        xest.est-type EQ 4 OR
                        xest.est-type EQ 8 THEN
                        FIND FIRST blk WHERE blk.id EQ oe-ordl.part-no NO-LOCK NO-ERROR.  
                    IF AVAILABLE blk THEN 
                    DO:
                        IF xest.est-type EQ 4 OR
                            xest.est-type EQ 8 THEN 
                        DO:
                            ASSIGN
                                v-blk-qty    = 0               
                                v-tot-comm-2 = 0.
                            IF oe-ordl.cost = 0 THEN
                                ll-recalc-cost = YES.
                            IF ll-recalc-cost THEN
                                oe-ordl.cost = 0.
                            FOR EACH blk WHERE blk.id EQ oe-ordl.part-no NO-LOCK,
                                FIRST xjob
                                WHERE xjob.form-no  EQ blk.snum
                                AND xjob.blank-no EQ blk.bnum:
              
                                IF xest.est-type EQ 8 THEN
                                DO:
                                    IF cerunc EQ "Fibre" THEN
                                        RUN est/usemargin.p (ROWID(xest), OUTPUT ll-use-margin).
                
                                    ASSIGN
                                        lv-sell-by         = ce-ctrl.sell-by
                                        lv-sell-by-ce-ctrl = ce-ctrl.sell-by
                                        v-pct-2            = ce-ctrl.prof-mrkup
                                        v-probe-comm       = eb.comm.
                
                                    FIND FIRST cust WHERE
                                        cust.company EQ job-hdr.company AND
                                        cust.cust-no EQ job-hdr.cust-no
                                        NO-LOCK.

                                    RUN custom/combasis.p (cocode,
                                        eb.sman,
                                        cust.type,
                                        eb.procat,
                                        0,
                                        cust.cust-no,
                                        OUTPUT v-basis).
                
                                    IF cust.markup NE 0 THEN
                                        v-pct-2 = cust.markup.
                
                                    IF NOT cecomm-log THEN
                                        v-probe-comm = 0.
                                    IF ll-use-margin THEN                 /* Get Margin% */
                                        RUN est/getsmanmtrx.p (ROWID(xest), "M",
                                            INPUT-OUTPUT v-probe-comm,
                                            INPUT-OUTPUT v-mp).
                                    ASSIGN
                                        v-board-cst = 0
                                        t-blkqty    = 0.

                                    FOR EACH b-eb2 FIELDS(form-no yrprice yld-qty bl-qty) NO-LOCK WHERE
                                        b-eb2.company EQ xest.company AND
                                        b-eb2.est-no  EQ xest.est-no AND
                                        b-eb2.form-no EQ job-hdr.frm:
                                        /* set total # of blanks on all forms */
                
                                        t-blkqty[b-eb2.form-no] = t-blkqty[b-eb2.form-no] +
                                            IF b-eb2.yrprice THEN b-eb2.yld-qty ELSE b-eb2.bl-qty.
                                    END.
                
                                    FOR EACH b-blk WHERE b-blk.id EQ xeb.part-no,
                                        FIRST b-ef2 NO-LOCK
                                        WHERE b-ef2.company EQ xest.company
                                        AND b-ef2.est-no  EQ xest.est-no
                                        AND b-ef2.form-no EQ b-blk.snum,
                                        EACH brd WHERE brd.form-no EQ b-ef2.form-no:
                     
                                        v-board-cst = v-board-cst + (brd.cost-m * b-blk.pct * (t-blkqty[b-ef2.form-no] / 1000)).
                                    END.
                     
                                    v-board-cst = v-board-cst / (v-qty-2 / 1000).
                                    /*REFACTORING REQUIRED - WHY IS JOB BUILD RECALCULATING PRICE BASED ON TARGET MARGIN!?!?*/
                                    RUN custom/markup.p (ROWID(xeb),
                                        v-board-cst,
                                        v-board-cst,
                                        v-board-cst,
                                        0,
                                        INPUT-OUTPUT lv-sell-by,
                                        INPUT-OUTPUT v-pct-2).
                
                                    IF ll-use-margin THEN
                                        v-pct-2 = v-mp.
                
                                    v-qty-2 = IF eb.yrprice THEN blk.qyld ELSE blk.qreq.
                
                                    /*                   IF lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B" THEN DO:                                    */
                                    /*                      ASSIGN                                                                                      */
                                    /*                         v-board-cst = 0                                                                          */
                                    /*                         t-blkqty = 0.                                                                            */
                                    /*                                                                                                                  */
                                    /*                      FOR EACH b-eb2 fields(form-no yrprice yld-qty bl-qty) NO-LOCK WHERE                         */
                                    /*                          b-eb2.company EQ xest.company AND                                                       */
                                    /*                          b-eb2.est-no  EQ xest.est-no AND                                                        */
                                    /*                          b-eb2.form-no EQ job-hdr.frm:                                                           */
                                    /*                          /* set total # of blanks on all forms */                                                */
                                    /*                                                                                                                  */
                                    /*                          t-blkqty[b-eb2.form-no] = t-blkqty[b-eb2.form-no] +                                     */
                                    /*                                                    if b-eb2.yrprice THEN b-eb2.yld-qty ELSE b-eb2.bl-qty.        */
                                    /*                      END.                                                                                        */
                                    /*                                                                                                                  */
                                    /*                      FOR EACH b-blk WHERE b-blk.id EQ xeb.part-no,                                               */
                                    /*                          FIRST b-ef2 NO-LOCK                                                                     */
                                    /*                          WHERE b-ef2.company EQ xest.company                                                     */
                                    /*                            AND b-ef2.est-no  EQ xest.est-no                                                      */
                                    /*                            AND b-ef2.form-no EQ b-blk.snum,                                                      */
                                    /*                          EACH brd WHERE brd.form-no EQ b-ef2.form-no:                                            */
                                    /*                                                                                                                  */
                                    /*                          v-board-cst = v-board-cst + (brd.cost-m * b-blk.pct * (t-blkqty[b-ef2.form-no] / 1000)).*/
                                    /*                      END.                                                                                        */
                                    /*                                                                                                                  */
                                    /*                      v-board-cst = v-board-cst / (v-qty-2 / 1000).                                               */
                                    /*                   END.                                                                                           */
                                    RUN custom/sellpric.p (lv-sell-by-ce-ctrl,
                                        lv-sell-by,
                                        v-basis,
                                        (IF lv-sell-by-ce-ctrl NE "B" AND
                                        lv-sell-by EQ "B" THEN v-board-cst
                                        ELSE blk.fact / (v-qty-2 / 1000)),
                                        (IF lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B" THEN 0
                                        ELSE (blk.cost / (v-qty-2 / 1000)) - (blk.fact / (v-qty-2 / 1000))),
                                        (IF ll-use-margin OR
                                        (lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B") THEN 0
                                        ELSE v-comm-2),
                                        v-pct-2,
                                        OUTPUT v-sell-price,
                                        OUTPUT v-comm-2).
                                    IF ll-use-margin OR
                                        (lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B") THEN
                                        v-comm-2 = v-sell-price * v-probe-comm / 100.
                                END.
                                ELSE
                                    v-comm-2 = 0.

                                ASSIGN
                                    v-tot-comm-2 = v-tot-comm-2 + v-comm-2.
                                IF ll-recalc-cost THEN
                                    oe-ordl.cost = oe-ordl.cost + blk.cost.

                                IF eb.yrprice THEN
                                    v-blk-qty = v-blk-qty + blk.qyld.
                                ELSE
                                    v-blk-qty = v-blk-qty + blk.qreq.

                            END. /*end blk*/
                            IF ll-recalc-cost THEN
                                oe-ordl.cost = (oe-ordl.cost / (v-blk-qty / 1000)) + v-tot-comm-2.
                        END.

                        ELSE IF ll-recalc-cost THEN
                                oe-ordl.cost = blk.cost -
                                    (((blk.fg-wt / 100) * blk.fg-wt$)
                                    * (blk.qyld / xest.est-qty[1])).
                    END.

                    ELSE
                        IF ll-recalc-cost THEN
                            oe-ordl.cost = tt-tot / (job-hdr.qty / 1000).
                END. /* If v-full-cost */

                ELSE
                DO:
                    IF xest.est-type EQ 4 THEN
                    DO:
                        FIND FIRST blk WHERE
                            blk.id EQ oe-ordl.part-no AND
                            blk.snum EQ job-hdr.frm AND
                            blk.bnum EQ job-hdr.blank-no.

                        ASSIGN
                            blk-fact = 0
                            v-qty-2  = IF eb.yrprice THEN blk.qyld ELSE blk.qreq.

                        FOR EACH bf-blk:
                            blk-fact = blk-fact + bf-blk.fact.
                        END. 
                        IF ll-recalc-cost THEN
                            oe-ordl.cost = blk.fact * (fac-tot / blk-fact) / (v-qty-2 / 1000).

                        RELEASE blk.
                    END.
                    ELSE IF ll-recalc-cost THEN
                            oe-ordl.cost = job-hdr.std-tot-cost.
                END. /* NOT v-full-cost */
            /* Moved to below, task 11241407 */
            /* RUN oe/calcordt.p (ROWID(oe-ord)). */
            END. /* each oe-ordl */
        END. /* last of job-hdr.i-no */

        IF job-hdr.po-no EQ "" THEN 
        DO:
            FIND FIRST b-oe-ordl NO-LOCK
                WHERE b-oe-ordl.company EQ cocode 
                AND b-oe-ordl.opened  EQ YES 
                AND b-oe-ordl.ord-no  EQ job-hdr.ord-no
                AND b-oe-ordl.i-no    EQ job-hdr.i-no NO-ERROR.
            IF AVAILABLE b-oe-ordl 
                THEN ASSIGN job-hdr.po-no = b-oe-ordl.po-no.
        END. /* job-hdr.po-no EQ "" */

    
    END. /* Each job-hdr */

    /* This allows calcordt to run once per order for speed */
    FOR EACH tt-ordlist,
        FIRST oe-ord WHERE oe-ord.company EQ cocode
        AND oe-ord.ord-no EQ tt-ordlist.ord-no
        NO-LOCK:    
        RUN oe/calcordt.p (ROWID(oe-ord)).
    END.

    RUN util/jobsqin2.p (RECID(job)).

    IF nufile THEN 
    DO TRANSACTION:
        EMPTY TEMP-TABLE tt-job-mat.
        EMPTY TEMP-TABLE tt-job-mch.

        FOR EACH job-mat NO-LOCK
            WHERE job-mat.company EQ cocode
            AND job-mat.job     EQ job.job
            AND job-mat.job-no  EQ job.job-no
            AND job-mat.job-no2 EQ job.job-no2:
            CREATE tt-job-mat.
            BUFFER-COPY job-mat EXCEPT rec_key TO tt-job-mat.
            RELEASE tt-job-mat.
        END.

        FOR EACH job-mch NO-LOCK
            WHERE job-mch.company EQ job.company
            AND job-mch.job     EQ job.job
            AND job-mch.job-no  EQ job.job-no
            AND job-mch.job-no2 EQ job.job-no2:
            CREATE tt-job-mch.
            BUFFER-COPY job-mch EXCEPT rec_key TO tt-job-mch.
            RELEASE tt-job-mch.
        END.

        RUN jc/delkids.p (ROWID(job), NO).

        x = 0.

        FOR EACH xprep
            WHERE xprep.ml EQ YES
            ,
            FIRST prep NO-LOCK
            WHERE prep.company EQ cocode
            AND prep.CODE EQ xprep.CODE
            , 
            FIRST item NO-LOCK
            WHERE item.company EQ cocode
            AND item.i-no    EQ prep.i-no
            AND CAN-DO("7,8,M,X,Y",item.mat-type)
            :

            CREATE brd.
            ASSIGN
                brd.form-no  = xprep.frm
                brd.blank-no = xprep.blank-no
                brd.i-no     = prep.i-no
                brd.cost     = xprep.std-cost
                brd.cost-m   = xprep.cost-m
                brd.qty-uom  = "EA"
                brd.sc-uom   = "EA"
                brd.qty      = xprep.qty
                brd.qty-uom  = prep.uom
                brd.sc-uom   = prep.uom
                /*        brd.i-no     = xprep.code */.

            DELETE xprep.
        END. /* each xprep */

        FOR EACH brd WHERE brd.i-no NE "":
            IF brd.cost EQ ? THEN brd.cost = 0.
            IF brd.cost-m EQ ? THEN brd.cost-m = 0.
            IF brd.qty EQ ? THEN brd.qty = 0.

            FIND FIRST item NO-LOCK
                WHERE item.company EQ cocode
                AND item.i-no    EQ brd.i-no.

            v-up = 1.
            IF CAN-DO("1,2,3,4,B,P,R",item.mat-type) THEN
                RUN sys/inc/numup.p (job.company,job.est-no, brd.form-no, OUTPUT v-up).
            v-out = 1.

            FIND FIRST ef NO-LOCK
                WHERE ef.company EQ job.company
                AND ef.est-no  EQ job.est-no
                AND ef.form-no EQ brd.form-no
                NO-ERROR.

            IF AVAILABLE ef THEN RUN est/ef-#out.p (ROWID(ef), OUTPUT v-out).

            type-mat = item.mat-type.

            IF type-mat EQ "B" AND item.i-code NE "R" THEN type-mat = "".

            z = LOOKUP(type-mat,type-chk).

            IF CAN-DO("5,6",type-mat) AND z = 0 THEN
                z = LOOKUP("C",type-chk).

            IF CAN-DO("1,2,3,4,B,P,R",item.mat-type) THEN
                FOR EACH job-mat NO-LOCK
                    WHERE job-mat.company  EQ job.company
                    AND job-mat.job      EQ job.job
                    AND job-mat.job-no   EQ job.job-no
                    AND job-mat.job-no2  EQ job.job-no2
                    AND job-mat.frm      EQ brd.form-no
                    AND job-mat.blank-no EQ brd.blank-no
                    AND job-mat.i-no     NE brd.i-no
                    AND job-mat.qty-all  GT 0
                    AND job-mat.all-flg  EQ YES
                    AND CAN-FIND(FIRST x-item
                    WHERE x-item.company  EQ job-mat.company
                    AND x-item.i-no     EQ job-mat.i-no
                    AND x-item.mat-type EQ item.mat-type)
                    USE-INDEX job:

                    ASSIGN
                        brd.qty = brd.qty * v-up * v-out
                        brd.qty = brd.qty - (job-mat.qty * job-mat.n-up)
                        brd.qty = brd.qty / (v-up * v-out).
      
                END.

            IF brd.qty GT 0 THEN 
            DO:
                FIND FIRST job-mat
                    WHERE job-mat.company  EQ job.company
                    AND job-mat.job      EQ job.job
                    AND job-mat.job-no   EQ job.job-no
                    AND job-mat.job-no2  EQ job.job-no2
                    AND job-mat.frm      EQ brd.form-no
                    AND job-mat.blank-no EQ brd.blank-no
                    AND job-mat.i-no     EQ brd.i-no
                    AND job-mat.rm-i-no  EQ brd.i-no
                    USE-INDEX i-no NO-ERROR.
        
                IF NOT AVAILABLE job-mat THEN 
                DO:
                    x = 0.
                    FOR EACH job-mat NO-LOCK
                        WHERE job-mat.company EQ job.company
                        AND job-mat.job     EQ job.job
                        AND job-mat.job-no  EQ job.job-no
                        AND job-mat.job-no2 EQ job.job-no2
                        USE-INDEX job
                        BY job-mat.line DESCENDING:
           
                        x = job-mat.line.
                        LEAVE.
                    END.
           
                    CREATE job-mat.
           
                    ASSIGN 
                        job-mat.company  = cocode
                        job-mat.job      = job.job
                        job-mat.job-no   = job.job-no
                        job-mat.job-no2  = job.job-no2
                        job-mat.line     = x + 1
                        job-mat.blank-no = brd.blank-no
                        job-mat.frm      = brd.form-no
                        job-mat.i-no     = brd.i-no
                        job-mat.rm-i-no  = brd.i-no.
                END.
       
                IF job-mat.qty-all LT 0 THEN job-mat.qty-all = 0.
         
                ASSIGN
                    job-mat.len      = brd.len
                    job-mat.wid      = brd.wid
                    job-mat.dep      = brd.dep
                    job-mat.cost-m   = brd.cost-m
                    job-mat.basis-w  = brd.basis-w
                    job-mat.n-up     = v-up * v-out
                    job-mat.qty-mr   = brd.qty-mr
                    job-mat.qty-wst  = brd.qty-wst
                    job-mat.qty-uom  = brd.qty-uom
                    job-mat.sc-uom   = brd.sc-uom
                    job-mat.std-cost = brd.cost
                    job-mat.qty      = brd.qty.
        
                IF job-mat.qty-all EQ 0 OR
                    NOT job-mat.all-flg  THEN
                    job-mat.qty-all = brd.qty - job-mat.qty-iss.
        
                ELSE
                    IF job-mat.qty-all NE brd.qty AND NOT ll-all-warn THEN 
                    DO:
                        MESSAGE "Allocated material must be manually updated by Planner..."
                            VIEW-AS ALERT-BOX WARNING.
                        ll-all-warn = YES.
                    END.
        
                job-mat.post = (z GT 0 AND jc-ctrl.post[z]) OR CAN-DO("J",type-mat).
        
                IF ll-use-netsht AND CAN-DO("1,2,3,4,B,P,R",item.mat-type) THEN 
                DO:
                    v-blk-qty = 0.
                    FOR EACH job-hdr
                        WHERE job-hdr.company  EQ job-mat.company
                        AND job-hdr.job      EQ job-mat.job
                        AND job-hdr.job-no   EQ job-mat.job-no
                        AND job-hdr.job-no2  EQ job-mat.job-no2
                        AND (job-hdr.frm     EQ job-mat.frm OR
                        xest.est-type   EQ 2           OR
                        xest.est-type   EQ 6)
                        USE-INDEX job NO-LOCK:
        
                        IF xest.est-type EQ 2 OR xest.est-type EQ 6 THEN
                            FOR EACH eb
                                WHERE eb.company EQ job.company
                                AND eb.est-no  EQ job.est-no
                                AND eb.form-no EQ job-mat.frm
                                USE-INDEX est-no NO-LOCK
                                BY job-hdr.qty *
                                (IF eb.est-type EQ 2 THEN
                                IF eb.cust-% LT 0 THEN (-1 / eb.cust-%) ELSE eb.cust-%
                                ELSE
                                IF eb.quantityPerSet LT 0 THEN (-1 / eb.quantityPerSet) ELSE eb.quantityPerSet) /
                                eb.num-up DESCENDING:
                                v-blk-qty = (job-hdr.qty *
                                    (IF eb.est-type EQ 2 THEN
                                    IF eb.cust-% LT 0 THEN (-1 / eb.cust-%) ELSE eb.cust-%
                                    ELSE
                                    IF eb.quantityPerSet LT 0 THEN (-1 / eb.quantityPerSet) ELSE eb.quantityPerSet) /
                                    (eb.num-up * v-out)).
                                LEAVE.
                            END.
                        ELSE v-blk-qty = v-blk-qty + (job-hdr.qty / job-mat.n-up).
                    END.
                    IF v-blk-qty NE 0 THEN job-mat.qty = v-blk-qty.
                END.
        
                IF job-mat.qty-uom EQ "EA" THEN 
                DO:
                {sys/inc/roundup.i job-mat.qty}
                END.
/*Ticket 25418 - Mismatch of cost/m on Materials tab vs. Job Hdr Cost/M - do not recalculate the cost/m */
/*                IF NOT AVAILABLE job-hdr THEN                                                                            */
/*                    FIND FIRST job-hdr WHERE job-hdr.company = cocode                                                    */
/*                        AND job-hdr.job-no  = job.job-no                                                                 */
/*                        AND job-hdr.job-no2 = job.job-no2                                                                */
/*                        AND job-hdr.job     = job.job                                                                    */
/*                        EXCLUSIVE-LOCK NO-ERROR.                                                                         */
/*                IF AVAIL(job-hdr) AND job-mat.len GT 0 AND job-mat.wid GT 0 AND job-mat.qty GT 0                         */
/*                    AND job-mat.std-cost GT 0 AND job-hdr.qty GT 0 AND job-mat.sc-uom = "MSF" THEN                       */
/*                    ASSIGN job-mat.cost-m = ((job-mat.LEN * job-mat.wid / 144) * (job-mat.qty / 1000) * job-mat.std-cost)*/
/*                 / job-hdr.qty * 1000.                                                                                   */
/*                IF job-mat.cost-m = ? THEN                                                                               */
/*                    job-mat.cost-m = 0.                                                                                  */

                IF job-mat.qty-all EQ 0 OR
                    NOT job-mat.all-flg  THEN
                    job-mat.qty-all = job-mat.qty - job-mat.qty-iss.
        
                ELSE
                    IF job-mat.qty-all NE brd.qty AND NOT ll-all-warn THEN 
                    DO:
                        MESSAGE "Allocated material must be manually updated by Planner..."
                            VIEW-AS ALERT-BOX WARNING.
                        ll-all-warn = YES.
                    END.
            END.
        END. /* each brd */

        EMPTY TEMP-TABLE tt-job-mat.

        FOR EACH op WHERE op.m-code NE "":
            IF CAN-FIND(FIRST job-mch
                WHERE job-mch.company   EQ job.company
                AND job-mch.m-code    EQ op.m-code
                AND job-mch.job       EQ job.job
                AND job-mch.job-no    EQ job.job-no
                AND job-mch.job-no2   EQ job.job-no2
                AND job-mch.frm       EQ op.form-no
                AND (job-mch.blank-no EQ op.blank-no OR
                job-mch.blank-no EQ 0)
                AND job-mch.pass      EQ op.pass
                AND job-mch.dept      EQ op.dept)
                OR CAN-FIND(FIRST job-mch
                WHERE job-mch.company   EQ job.company
                AND job-mch.job       EQ job.job
                AND job-mch.job-no    EQ job.job-no
                AND job-mch.job-no2   EQ job.job-no2
                AND job-mch.frm       EQ op.form-no
                AND (job-mch.blank-no EQ op.blank-no OR
                job-mch.blank-no EQ 0)
                AND job-mch.pass      EQ op.pass
                AND job-mch.dept      EQ op.dept
                AND job-mch.spare-char-1 EQ op.m-code) THEN
                NEXT.
            RUN  sys/inc/numup.p (job.company, job.est-no, op.form-no, OUTPUT v-up).
            v-out = 1.
            FIND FIRST ef WHERE ef.company EQ job.company
                AND ef.est-no  EQ job.est-no
                AND ef.form-no EQ op.form-no
                NO-LOCK NO-ERROR.
    
            IF AVAILABLE ef THEN RUN est/ef-#out.p (ROWID(ef), OUTPUT v-out).

            FIND job-hdr NO-LOCK
                WHERE job-hdr.company   EQ job.company
                AND job-hdr.job       EQ job.job
                AND job-hdr.job-no    EQ job.job-no
                AND job-hdr.job-no2   EQ job.job-no2
                AND job-hdr.frm       EQ op.form-no
                AND (job-hdr.blank-no EQ op.blank-no OR op.blank-no EQ 0)
                NO-ERROR.

            RELEASE itemfg.
            IF AVAILABLE job-hdr THEN
                FIND FIRST itemfg NO-LOCK
                    WHERE itemfg.company EQ job-hdr.company
                    AND itemfg.i-no    EQ job-hdr.i-no
                    NO-ERROR.

            FIND FIRST mach NO-LOCK 
            {sys/ref/machW.i}
            AND mach.m-code EQ op.m-code.

            v-on-f = 1.

            FIND FIRST est-op NO-LOCK
                WHERE est-op.company EQ cocode
                AND est-op.est-no  EQ job.est-no
                AND est-op.line    EQ op.line
                NO-ERROR.

            IF NOT AVAILABLE est-op THEN
                FIND FIRST est-op NO-LOCK
                    WHERE est-op.company EQ cocode
                    AND est-op.est-no  EQ job.est-no
                    AND est-op.line    EQ op.line + 500
                    NO-ERROR.

            IF AVAILABLE est-op THEN
                RUN sys/inc/numout.p (RECID(est-op), OUTPUT v-on-f).

            CREATE job-mch.

            ASSIGN
                job-mch.company  = cocode
                job-mch.blank-no = IF (mach.p-type EQ "B" OR
                                      (xest.est-type EQ 3 AND op.dept EQ "PR"))
                                   THEN op.blank-no ELSE 0
                job-mch.job      = job.job
                job-mch.job-no   = job.job-no
                job-mch.job-no2  = job.job-no2
                job-mch.dept     = op.dept
                job-mch.frm      = op.form-no
                job-mch.i-no     = /*IF AVAIL itemfg THEN itemfg.i-no ELSE - bpv TASK 12181205*/ op.i-no
                job-mch.i-name   = /*IF AVAIL itemfg THEN itemfg.i-name ELSE */ op.i-name
                job-mch.line     = op.line
                job-mch.m-code   = op.m-code
                job-mch.mr-fixoh = op.mr-fixoh
                job-mch.mr-hr    = op.mr-hr
                job-mch.mr-rate  = op.mr-rate
                job-mch.mr-varoh = op.mr-varoh
                job-mch.mr-waste = op.mr-waste
                job-mch.pass     = op.pass
                job-mch.run-hr   = op.run-hr
                job-mch.run-fixoh = op.run-fixoh
                job-mch.run-rate  = op.run-rate
                job-mch.run-varoh = op.run-varoh
                job-mch.speed    = op.speed
                job-mch.wst-prct = op.wst-prct
                job-mch.lag-time = mach.daily-prod-hours
             /* job-mch.start-date = job.start-date */
                job-mch.run-qty  = op.run-qty
                job-mch.n-out    = IF AVAIL est-op AND est-op.n-out NE 0 THEN est-op.n-out ELSE 1
                job-mch.n-on     = IF mach.p-type EQ "B" THEN 1 ELSE
                                     (v-up * v-out / v-on-f)
                job-mch.est-op_rec_key = op.rec_key
                lOverwriteJobPlan-Log  = cOverwriteJobPlan-Char NE "Yes"
                .
      
            FIND FIRST tt-job-mch
                 WHERE tt-job-mch.est-op_rec_key EQ job-mch.est-op_rec_key
                 NO-ERROR.
            IF NOT AVAILABLE tt-job-mch THEN 
            FIND FIRST tt-job-mch
                 WHERE tt-job-mch.company  EQ job-mch.company
                   AND tt-job-mch.m-code   EQ job-mch.m-code
                   AND tt-job-mch.job      EQ job-mch.job
                   AND tt-job-mch.job-no   EQ job-mch.job-no
                   AND tt-job-mch.job-no2  EQ job-mch.job-no2
                   AND tt-job-mch.frm      EQ job-mch.frm
                   AND tt-job-mch.blank-no EQ job-mch.blank-no
                   AND tt-job-mch.pass     EQ job-mch.pass
                   AND tt-job-mch.dept     EQ job-mch.dept
                 NO-ERROR.
            IF AVAILABLE tt-job-mch THEN DO:
                IF job-mch.m-code NE tt-job-mch.m-code THEN DO:
                    IF cOverwriteJobPlan-Char EQ "Ask" THEN DO:
                        MESSAGE
                            "Job:" job-mch.job-no + "-" + STRING(job-mch.job-no2)
                            "Form:" job-mch.frm SKIP(1)
                            "Routing has changed from ~"" + job-mch.m-code +
                            "~" to ~"" + tt-job-mch.m-code + "~"." SKIP(1)
                            "Allow this Routing Change?"
                        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                        UPDATE lOverwriteJobPlan-Log.
                    END. /* if ask */
                END. /* differing machince codes */ 
                IF lOverwriteJobPlan-Log EQ YES THEN
                ASSIGN
                    job-mch.m-code        = tt-job-mch.m-code
                    job-mch.start-date    = tt-job-mch.start-date
                    job-mch.start-date-su = tt-job-mch.start-date-su
                    job-mch.start-time    = tt-job-mch.start-time
                    job-mch.start-time-su = tt-job-mch.start-time-su
                    job-mch.end-date      = tt-job-mch.end-date
                    job-mch.end-date-su   = tt-job-mch.end-date-su
                    job-mch.end-time      = tt-job-mch.end-time
                    job-mch.end-time-su   = tt-job-mch.end-time-su
                    job-mch.mr-complete   = tt-job-mch.mr-complete
                    job-mch.run-complete  = tt-job-mch.run-complete
                    job-mch.sbLiveUpdate  = tt-job-mch.sbLiveUpdate
                    job-mch.anchored      = tt-job-mch.anchored
                    .
            END. /* avail tt-job-mch */
        END. /* each op */

        EMPTY TEMP-TABLE tt-job-mch.

        z = 0.
        FOR EACH job-mch WHERE job-mch.company EQ job.company
            AND job-mch.job     EQ job.job
            AND job-mch.job-no  EQ job.job-no
            AND job-mch.job-no2 EQ job.job-no2 EXCLUSIVE
                       BY job-mch.frm BY job-mch.line:
            ASSIGN
                z            = z + 1
                job-mch.line = z.
        END.

        FOR EACH xprep:        
   
            CREATE job-prep.
            ASSIGN
                job-prep.company  = cocode
                job-prep.job      = job.job
                job-prep.job-no   = job.job-no
                job-prep.job-no2  = job.job-no2
                job-prep.frm      = xprep.frm
                job-prep.blank-no = xprep.blank-no
                job-prep.code     = xprep.code
                job-prep.ml       = xprep.ml
                job-prep.std-cost = xprep.std-cost
                job-prep.cost-m   = xprep.cost-m
                job-prep.opn      = YES
                job-prep.qty      = xprep.qty
                job-prep.simon    = xprep.simon.
  
            FIND FIRST prep NO-LOCK
                WHERE prep.company EQ cocode
                AND prep.code    EQ xprep.code
                NO-ERROR.
            job-prep.sc-uom = IF AVAILABLE prep THEN prep.uom ELSE "EA".
        END. /* each xprep */
    END. /* if nufile */

    RUN jc/chkalloc.p (ROWID(job)).
    RUN jc/addJobFarm.p (INPUT job.job).
END. /* trans */

DEFINE VARIABLE v-jobcard AS cha NO-UNDO.
DEFINE VARIABLE v-reprint AS LOG NO-UNDO.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.

RELEASE oe-ord.


IF nufile THEN 
DO:
    IF NOT oe-ctrl.p-fact THEN
        FOR EACH job-hdr
            WHERE job-hdr.company EQ job.company
            AND job-hdr.job     EQ job.job
            AND job-hdr.job-no  EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2
            AND job-hdr.ord-no  NE 0
            NO-LOCK,
            FIRST oe-ord
            WHERE oe-ord.company EQ job-hdr.company
            AND oe-ord.ord-no  EQ job-hdr.ord-no
            AND (oe-ord.stat    EQ "H" OR oe-ord.priceHold)
            NO-LOCK:
            LEAVE.
        END.

    IF NOT AVAILABLE oe-ord THEN 
    DO:
        FIND FIRST xest
            WHERE xest.company     EQ cocode
            AND xest.est-no      EQ job.est-no
            AND TRIM(job.est-no) NE ""
            NO-LOCK NO-ERROR.
        v-jobcard = IF NOT AVAILABLE xest THEN "" ELSE
            IF xest.est-type LE 4 THEN "JOBCARDF"
            ELSE "JOBCARDC".

        IF v-jobcard NE "" THEN 
        DO:
            FIND FIRST sys-ctrl
                WHERE sys-ctrl.company EQ cocode
                AND sys-ctrl.name    EQ v-jobcard
                NO-LOCK NO-ERROR.  

            IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN 
            DO:
                FIND FIRST b-print-job-hdr NO-LOCK
                    WHERE b-print-job-hdr.company EQ job.company
                    AND b-print-job-hdr.job     EQ job.job
                    AND b-print-job-hdr.job-no  EQ job.job-no
                    AND b-print-job-hdr.job-no2 EQ job.job-no2 NO-ERROR.

                IF AVAILABLE b-print-job-hdr THEN
                DO:
                    v-reprint = b-print-job-hdr.ftick-prnt.
                    RELEASE b-print-job-hdr.
                END.

                RUN custom/setUserPrint.p (job.company,'r-ticket.',
                    'begin_job1,begin_job2,end_job1,end_job2,tb_reprint,fl-jobord',
                    job.job-no + ',' + STRING(job.job-no2) + ',' +
                    job.job-no + ',' + STRING(job.job-no2) + ',' +
                    STRING(v-reprint)+ ',' +  "0" ). /* gdm - 07130906 */

                RUN jcrep/r-ticket.w.

            END.
        END.
    END. 
END.

