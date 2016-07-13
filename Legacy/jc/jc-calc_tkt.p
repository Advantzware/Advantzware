/* jc/jc-calc.p
  09/14/04  YSK  TASK 09130412   */
/* CF Production Control Test Run */

DEFINE INPUT PARAMETER rec-id AS RECID NO-UNDO.
{sys/inc/var.i shared}
DEFINE NEW SHARED BUFFER xest  FOR est.
DEFINE NEW SHARED BUFFER xef   FOR ef.
DEFINE NEW SHARED BUFFER xeb   FOR eb.
DEFINE NEW SHARED BUFFER xeb2  FOR eb.
DEFINE            BUFFER x-job FOR job.
DEFINE            VARIABLE v-est-qty      AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE save_id        AS RECID     NO-UNDO.
DEFINE NEW SHARED VARIABLE v-rebuild      AS LOG       FORMAT "R/E" NO-UNDO.
DEFINE            VARIABLE v-est-job      LIKE job.est-no NO-UNDO.
DEFINE            VARIABLE v-up           LIKE job-mat.n-up NO-UNDO.
DEFINE            VARIABLE v-out          LIKE ef.n-out NO-UNDO.
DEFINE            VARIABLE v-job-hdr      AS LOG       NO-UNDO.
DEFINE            VARIABLE v-hold-qty     LIKE job-hdr.qty NO-UNDO.
DEFINE            VARIABLE choice         AS LOG       NO-UNDO.
DEFINE            VARIABLE v-yld-qty      AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE li             AS INTEGER   NO-UNDO.
DEFINE            VARIABLE lv-format-f    AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lv-date        AS DATE      NO-UNDO.
DEFINE            VARIABLE v-on-f         AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-print-job    AS LOG       NO-UNDO. /* flag for auto job ticket print Task#09180401*/
DEFINE            VARIABLE v-copies       AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-printer-name AS cha       NO-UNDO.

DEFINE            VARIABLE K_FRAC         AS DECIMAL   INIT 6.25 NO-UNDO.

DEFINE SHARED     VARIABLE nufile         AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE qty            AS INTEGER   NO-UNDO.

{ce/print4.i "new shared" "new shared"}
{ce/print42.i "new shared"}
   
DEFINE WORKFILE work-ord
    FIELD cust-no LIKE job-hdr.cust-no
    FIELD ord-no  LIKE job-hdr.ord-no.

DEFINE VARIABLE type-chk     AS CHARACTER INIT "C,D,F,G,I,L,M,P,R,T,V,W" NO-UNDO.
DEFINE VARIABLE over-pct     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-add-overrn AS LOG       NO-UNDO.
DEFINE VARIABLE v-qty        AS INTEGER   NO-UNDO.

DEFINE VARIABLE v-set-hdr    LIKE oe-ordl.i-no.
DEFINE VARIABLE v-item-no    LIKE itemfg.i-no.
DEFINE VARIABLE v-part-qty   AS DECIMAL.

DEFINE VARIABLE ll-one-part  AS LOG       NO-UNDO.

DEFINE BUFFER x-eb      FOR eb.
DEFINE BUFFER x-job-hdr FOR job-hdr.

{oe/oe-sysct1.i NEW}
 
{ce/msfcalc.i}
    
{sys/inc/f16to32.i}

DO TRANSACTION:
    {sys/inc/graphic.i}
END.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "JOBCARDF"
    NO-LOCK NO-ERROR.
IF AVAILABLE sys-ctrl THEN lv-format-f = sys-ctrl.char-fld.

{sys/ref/fgoecost.i}
 
RUN oe/oe-sysct.p.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name EQ "JOB QTY"
    NO-LOCK NO-ERROR.
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
v-add-overrn = sys-ctrl.log-fld.

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name EQ "JOBPRINT" NO-LOCK NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN 
DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company  = cocode
        sys-ctrl.name     = "JOBPRINT"
        sys-ctrl.descrip  = "Print Job Ticket From Order Automatically?"
        sys-ctrl.int-fld  = 1
        sys-ctrl.char-fld = SESSION:PRINTER-NAME
        sys-ctrl.log-fld  = NO.
    MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.
END.
ASSIGN 
    v-print-job    = sys-ctrl.log-fld
    v-copies       = sys-ctrl.int-fld
    v-printer-name = sys-ctrl.char-fld.

FIND FIRST jc-ctrl WHERE jc-ctrl.company EQ cocode NO-LOCK NO-ERROR.
 
mainloop:
DO TRANSACTION:
    FIND job WHERE RECID(job) EQ rec-id.

    IF nufile THEN 
    DO:
        IF NOT PROGRAM-NAME(2) BEGINS "util/updstand" THEN
            FIND FIRST job-mat
                WHERE job-mat.company EQ cocode
                AND job-mat.all-flg EQ YES
                AND job-mat.job     EQ job.job
                AND job-mat.job-no  EQ job.job-no
                AND job-mat.job-no2 EQ job.job-no2
                AND job-mat.qty-all NE 0

                NO-LOCK NO-ERROR.
      
        choice = NOT AVAILABLE job-mat.      
        IF NOT choice THEN 
        DO ON ENDKEY UNDO, LEAVE:
            MESSAGE "WARNING: Raw materials will be decommitted, continue?"
                VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE choice.
        END.
    
        IF choice THEN RUN jc/jc-dall.p (RECID(job)).
  
        ELSE LEAVE.

        FOR EACH job-hdr
            WHERE job-hdr.company EQ cocode
            AND job-hdr.job     EQ job.job
            AND job-hdr.job-no  EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2
            AND job-hdr.qty     EQ 0:
            DELETE job-hdr.
        END.

        FOR EACH work-ord:
            DELETE work-ord.
        END.

        FOR EACH job-hdr
            WHERE job-hdr.company EQ cocode
            AND job-hdr.job     EQ job.job
            AND job-hdr.job-no  EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2
            AND job-hdr.ord-no  NE 0
            NO-LOCK
            BREAK BY job-hdr.cust-no:
    
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
        AND xest.loc     EQ locode
        AND xest.est-no  EQ job.est-no
        USE-INDEX est-no NO-ERROR.
    IF NOT AVAILABLE xest THEN 
    DO:
        choice = NO.
        MESSAGE "There is no estimate to build from for this job.  "
            "Would you like to create one?  " 
            VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE choice .
        IF NOT choice THEN UNDO, LEAVE.
        IF choice THEN 
        REPEAT:
            MESSAGE "Enter the Estimate Number you wish to build from:  "
                UPDATE v-est-job.             
            RUN util/rjust.p (INPUT-OUTPUT v-est-job, INPUT 8).
            FIND FIRST xest WHERE xest.company = cocode
                AND xest.loc = locode
                AND xest.est-no EQ v-est-job NO-ERROR.
            IF NOT AVAILABLE xest THEN
                /*run sys/msg/wemsg.p("Estimate Does Not Exist.  Please Re-enter",
                                    "E R R O R!", 4).
                 */
                MESSAGE "Estimate Does Not Exist. Please Re-enter." VIEW-AS ALERT-BOX ERROR.                   
            IF AVAILABLE xest THEN 
            DO:
                job.est-no = xest.est-no.
                job.rec_key = xest.rec_key.  /* for notes */
  

                FOR EACH job-hdr WHERE job-hdr.company EQ cocode
                    AND job-hdr.job     EQ job.job
                    AND job-hdr.job-no  EQ job.job-no
                    AND job-hdr.job-no2 EQ job.job-no2,
                    EACH oe-ordl WHERE oe-ordl.company EQ job-hdr.company
                    AND oe-ordl.job-no  EQ job-hdr.job-no
                    AND oe-ordl.job-no2 EQ job-hdr.job-no2
                    AND oe-ordl.ord-no  EQ job-hdr.ord-no,
                    EACH oe-ord OF oe-ordl:
 
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

    IF nufile THEN
        FOR EACH xeb WHERE xeb.company = xest.company AND
            xeb.est-no = xest.est-no
            BREAK BY xeb.est-no
            BY xeb.form-no
            BY xeb.blank-no:

            IF lv-format-f EQ "CentBox" THEN
                ASSIGN
                    li         = li + 1
                    xeb.spc-no = TRIM(job.job-no)         + "-" +
                      STRING(job.job-no2,"99") + "-" +
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

            IF NOT AVAILABLE itemfg OR xeb.stock-no EQ "" THEN 
            REPEAT:
                IF xeb.stock-no EQ "" THEN 
                DO:
                    MESSAGE " FG Item Not defined, Please Enter "
                        UPDATE xeb.stock-no.
                    IF xeb.stock-no EQ "" THEN NEXT.
                END.
                FIND FIRST itemfg
                {sys/look/itemfgrlW.i}
                AND itemfg.i-no   EQ xeb.stock-no
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE itemfg THEN 
                DO ON ENDKEY UNDO, NEXT:
                    choice = YES.
                    MESSAGE "Item: " + TRIM(xeb.stock-no) + " doesn't exist, would you like to create it?"
                        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice.
                    IF choice THEN 
                    DO:
                    {jc/fgadd.i}
                        LEAVE.
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
           
            IF xest.est-type GE 3 AND xest.est-type LE 4 THEN
                FIND FIRST job-hdr WHERE job-hdr.company EQ cocode
                    AND job-hdr.job     EQ job.job
                    AND job-hdr.job-no  EQ job.job-no
                    AND job-hdr.job-no2 EQ job.job-no2
                    AND job-hdr.frm     EQ xeb.form-no
                    AND job-hdr.i-no    EQ xeb.stock-no
                    NO-ERROR.          
            ELSE IF xest.est-type EQ 1 OR xest.est-type EQ 5 OR xeb.form-no EQ 0 THEN
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

            IF NOT AVAILABLE job-hdr THEN 
            DO:
                x = 0.
                FIND LAST x-job-hdr USE-INDEX j-no NO-LOCK NO-ERROR.
                IF AVAILABLE x-job-hdr THEN x = x-job-hdr.j-no.

                CREATE job-hdr.
                ASSIGN 
                    job-hdr.company    = cocode
                    job-hdr.loc        = locode
                    job-hdr.j-no       = x + 1
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
                IF AVAILABLE work-ord THEN job-hdr.ord-no = work-ord.ord-no.
      
                    {util/mkjobkey.i}
            END.

            IF job-hdr.frm EQ 0 THEN job-hdr.frm = 1.

            job-hdr.blank-no = IF xeb.form-no EQ 0 THEN 1 ELSE xeb.blank-no.
            RUN update-itemfg (ROWID(job-hdr), -1).

            ASSIGN
                v-qty      = 0
                v-hold-qty = job-hdr.qty.

            IF xest.est-type LT 3 OR xest.est-type GT 4 THEN
                ASSIGN  v-qty       = xest.est-qty[1]
                    job-hdr.qty = xest.est-qty[1].
            ELSE 
                FOR EACH eb WHERE eb.company = xest.company 
                    AND eb.est-no EQ xest.est-no
                    AND eb.stock-no EQ xeb.stock-no NO-LOCK:
        
                    IF eb.form-no  EQ xeb.form-no AND eb.blank-no EQ xeb.blank-no THEN job-hdr.qty = eb.bl-qty.         
                    v-qty = v-qty + eb.bl-qty.
                END.
    
            IF job-hdr.qty EQ 0 THEN job-hdr.qty = 1.
            IF v-qty       EQ 0 THEN v-qty       = 1.

            over-pct = 0.

            FIND FIRST oe-ordl WHERE oe-ordl.company  EQ cocode
                AND ((oe-ordl.ord-no EQ job-hdr.ord-no AND job-hdr.ord-no NE 0) OR
                job-hdr.ord-no  EQ 0)
                AND oe-ordl.job-no   EQ job-hdr.job-no
                AND oe-ordl.job-no2  EQ job-hdr.job-no2
                AND oe-ordl.i-no     EQ job-hdr.i-no
                AND oe-ordl.est-no   EQ job-hdr.est-no  NO-ERROR.
            IF AVAILABLE oe-ordl THEN
                ASSIGN job-hdr.qty      = oe-ordl.qty * (job-hdr.qty / v-qty)
                    job-hdr.ord-no   = oe-ordl.ord-no
                    job-hdr.due-date = oe-ordl.req-date
                    over-pct         = oe-ordl.over-pct.
            ELSE
                IF v-hold-qty NE 0 THEN job-hdr.qty = v-hold-qty.

            /*over-pct = 0.
            if job-hdr.ord-no ne 0 then do:
               find first oe-ord  where oe-ord.company eq cocode
                                    and oe-ord.ord-no  eq job-hdr.ord-no
                                    no-lock no-error.
               if avail oe-ord then over-pct = oe-ord.over-pct.
            end.*/
      
            IF v-add-overrn THEN job-hdr.qty = job-hdr.qty + (job-hdr.qty * (over-pct * .01)).

            RUN update-itemfg (ROWID(job-hdr), 1).

            FOR EACH x-job-hdr
                WHERE x-job-hdr.company EQ job-hdr.company
                AND x-job-hdr.job     EQ job-hdr.job
                AND x-job-hdr.job-no  EQ job-hdr.job-no
                AND x-job-hdr.job-no2 EQ job-hdr.job-no2
                AND x-job-hdr.frm     EQ job-hdr.frm
                AND x-job-hdr.i-no    EQ job-hdr.i-no
                AND ROWID(x-job-hdr)  NE ROWID(job-hdr):

                RUN update-itemfg (ROWID(x-job-hdr), -1).

                DELETE x-job-hdr.
            END.
        END.  /* for each xeb */

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
        xest.est-type  NE 2 AND xest.est-type NE 6))):

        RUN update-itemfg (ROWID(job-hdr), -1).

        DELETE job-hdr.
    END.

    ASSIGN
        job.est-no  = xest.est-no
        job.rec_key = xest.rec_key.
    job.stat    = "R".

    FIND job WHERE RECID(job) EQ rec-id NO-LOCK.
  
    RUN jc/calc-est.p (RECID(job)).
 
    FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "SCHEDULE" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN 
    DO:
        CREATE sys-ctrl.
        ASSIGN 
            sys-ctrl.company  = cocode
            sys-ctrl.name     = "SCHEDULE"
            sys-ctrl.char-fld = "None"
            sys-ctrl.descrip  = "Update Due date and Promise date for Schedule?".
        MESSAGE sys-ctrl.descrip
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE sys-ctrl.log-fld.                             
    END.
    IF nufile THEN RUN jc/startdat.p (ROWID(job)).
   
    FIND job WHERE RECID(job) EQ rec-id EXCLUSIVE-LOCK.
    /* task 09130412 YSK 09/14/04  */
    IF nufile AND AVAILABLE sys-ctrl AND sys-ctrl.char-fld = "NoDate" AND sys-ctrl.log-fld
        THEN job.start-date = ?.

    ASSIGN
        job.std-fix-cost = 0
        job.std-lab-cost = 0
        job.std-mat-cost = 0
        job.std-var-cost = 0.

    FOR EACH job-hdr
        WHERE job-hdr.company EQ cocode
        AND job-hdr.job     EQ job.job
        AND job-hdr.job-no  EQ job.job-no
        AND job-hdr.job-no2 EQ job.job-no2
        EXCLUSIVE-LOCK
        BREAK BY job-hdr.i-no:

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

        FOR EACH reftable
            WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job-hdr.job,"999999999"):
            DELETE reftable.
        END.

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
                AND reftable.code     EQ STRING(job-hdr.job,"999999999")
                AND reftable.code2    EQ eb.stock-no
                AND reftable.val[12]  EQ eb.form-no
                AND reftable.val[13]  EQ eb.blank-no
                NO-ERROR.
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
                    reftable.val[13]  = eb.blank-no.
            END.

            ASSIGN
                v-yld-qty       = IF eb.yld-qty LT 0 THEN (-1 / eb.yld-qty) ELSE eb.yld-qty
                reftable.val[1] = reftable.val[1] + (xjob.lab / v-yld-qty)
                reftable.val[2] = reftable.val[2] + (xjob.mat / v-yld-qty)
                reftable.val[3] = reftable.val[3] + (xjob.voh / v-yld-qty)
                reftable.val[4] = reftable.val[4] + (xjob.foh / v-yld-qty)
                reftable.val[5] = reftable.val[1] + reftable.val[2] +
                         reftable.val[3] + reftable.val[4].

            FIND CURRENT reftable NO-LOCK.
        END.

        ASSIGN
            job-hdr.std-tot-cost = job-hdr.std-fix-cost + job-hdr.std-lab-cost +
                            job-hdr.std-mat-cost + job-hdr.std-var-cost
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
  
    RUN util/jobsqin2.p (RECID(job)).

    IF nufile THEN 
    DO:
        RUN jc/delkids.p (ROWID(job), NO).

        x = 0.

        FOR EACH brd:
            IF brd.cost EQ ? THEN brd.cost = 0.
            IF brd.cost-m EQ ? THEN brd.cost-m = 0.
            IF brd.qty EQ ? THEN brd.qty = 0.
            FIND FIRST item WHERE item.company EQ cocode
                AND item.i-no    EQ brd.i-no NO-LOCK.
            v-up = 0.
            IF INDEX("1234BPR",item.mat-type) GT 0 THEN
                RUN sys/inc/numup.p (job.company,job.est-no, brd.form-no, OUTPUT v-up).
            v-out = 1.
            FIND FIRST ef WHERE ef.company = job.company
                AND ef.est-no   EQ job.est-no
                AND ef.form-no EQ brd.form-no
                NO-LOCK NO-ERROR.
            IF AVAILABLE ef THEN v-out = (IF ef.n-out   EQ 0 THEN 1 ELSE ef.n-out  ) *
                    (IF ef.n-out-l EQ 0 THEN 1 ELSE ef.n-out-l) *
                    (IF ef.n-out-d EQ 0 THEN 1 ELSE ef.n-out-d).
            z = LOOKUP(item.mat-type, type-chk).
            x = x + 1.

            CREATE job-mat.

            ASSIGN 
                job-mat.company  = cocode
                job-mat.job      = job.job
                job-mat.job-no   = job.job-no
                job-mat.job-no2  = job.job-no2
                job-mat.line     = x
                job-mat.cost-m   = brd.cost-m
                job-mat.basis-w  = brd.basis-w
                job-mat.blank-no = brd.blank-no
                job-mat.frm      = brd.form-no
                job-mat.i-no     = brd.i-no
                job-mat.rm-i-no  = brd.i-no
                job-mat.len      = brd.len
                job-mat.wid      = brd.wid
                job-mat.n-up     = v-up * v-out
                job-mat.qty      = brd.qty
                job-mat.qty-mr   = brd.qty-mr
                job-mat.qty-wst  = brd.qty-wst
                job-mat.qty-uom  = brd.qty-uom
                job-mat.qty-all  = brd.qty
                job-mat.sc-uom   = brd.sc-uom
                job-mat.std-cost = brd.cost.
            IF z GT 0 THEN job-mat.post = jc-ctrl.post[z].      

        END.

        FOR EACH op:
            RUN sys/inc/numup.p (job.company,job.est-no,op.form-no,OUTPUT v-up).
            v-out = 1.
            FIND FIRST ef WHERE ef.company = job.company
                AND ef.est-no   EQ job.est-no
                AND ef.form-no EQ op.form-no
                NO-LOCK NO-ERROR.
            IF AVAILABLE ef THEN v-out = (IF ef.n-out   EQ 0 THEN 1 ELSE ef.n-out  ) *
                    (IF ef.n-out-l EQ 0 THEN 1 ELSE ef.n-out-l) *
                    (IF ef.n-out-d EQ 0 THEN 1 ELSE ef.n-out-d).

            FIND FIRST job-hdr WHERE job-hdr.company  EQ cocode
                AND job-hdr.job      EQ job.job
                AND job-hdr.job-no   EQ job.job-no
                AND job-hdr.job-no2  EQ job.job-no2
                AND job-hdr.frm      EQ op.form-no
                AND job-hdr.blank-no EQ op.blank-no NO-LOCK NO-ERROR.

            FIND FIRST mach {sys/ref/machW.i}
                      AND mach.m-code EQ op.m-code NO-LOCK.

            v-on-f = 1.

            FIND FIRST est-op
                WHERE est-op.company EQ cocode
                AND est-op.est-no  EQ job.est-no
                AND est-op.line    EQ op.line
                NO-LOCK NO-ERROR.

            IF NOT AVAILABLE est-op THEN
                FIND FIRST est-op
                    WHERE est-op.company EQ cocode
                    AND est-op.est-no  EQ job.est-no
                    AND est-op.line    EQ op.line + 500
                    NO-LOCK NO-ERROR.

            IF AVAILABLE est-op THEN
                RUN sys/inc/numout.p (RECID(est-op), OUTPUT v-on-f).

            CREATE job-mch.

            ASSIGN 
                job-mch.company  = cocode
                job-mch.blank-no = IF (mach.p-type EQ "B" OR
                                   (xest.est-type EQ 3 AND op.dept EQ "PR"))
                                then op.blank-no else 0
                job-mch.job      = job.job
                job-mch.job-no   = job.job-no
                job-mch.job-no2  = job.job-no2
                job-mch.dept     = op.dept
                job-mch.frm      = op.form-no
                job-mch.i-no     = if avail job-hdr then job-hdr.i-no else op.i-no
                job-mch.i-name   = op.i-name
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
                job-mch.start-date = job.start-date
                job-mch.run-qty  = op.run-qty
                job-mch.n-out    = IF AVAIL est-op THEN est-op.n-out ELSE 1
                job-mch.n-on     = IF INDEX("AB",mach.p-type) GT 0 THEN 1 ELSE
                                (v-up * v-out / v-on-f).

            /*task 09310402  */
            /*IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "NoDate" AND sys-ctrl.log-fld
            THEN */ 
            ASSIGN 
                job-mch.start-date-su = job.start-date
                job-mch.end-date      = job.start-date
                job-mch.end-date-su   = job.start-date.
        END.
  
        z = 0.
        FOR EACH job-mch WHERE job-mch.company EQ job.company
            AND job-mch.job     EQ job.job
            AND job-mch.job-no  EQ job.job-no
            AND job-mch.job-no2 EQ job.job-no2 exclusive
                       by job-mch.frm  by job-mch.line:
            z = z + 1.  
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
  
            FIND FIRST prep WHERE prep.company EQ cocode
                AND prep.loc     EQ locode
                AND prep.code    EQ xprep.code
                NO-LOCK NO-ERROR.
            job-prep.sc-uom = IF AVAILABLE prep THEN prep.uom ELSE "EA".
        END.
    END.
/*run jc/kiwiexp2.p (recid(job)).*/  

END. /* trans */

/* print job ticket auto if JOBPRINT = yes task# 09180401 YSK */
IF v-print-job THEN RUN print-ticket.

RETURN.

PROCEDURE update-itemfg:
    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ip-factor AS DECIMAL NO-UNDO.

    DEFINE BUFFER b-job-hdr FOR job-hdr.


    FIND b-job-hdr WHERE ROWID(b-job-hdr) EQ ip-rowid NO-LOCK NO-ERROR.

    IF AVAILABLE b-job-hdr THEN 
    DO:
        FIND FIRST itemfg
        {sys/look/itemfgrlW.i}
          AND itemfg.i-no EQ b-job-hdr.i-no
            NO-ERROR.

        itemfg.q-ono = itemfg.q-ono + (b-job-hdr.qty * ip-factor).
         
        RUN fg/comp-upd.p (RECID(itemfg), b-job-hdr.qty * ip-factor,
            "q-ono", b-job-hdr.est-no).
    END.
END PROCEDURE.

PROCEDURE print-ticket:
    DEFINE VARIABLE lv-format      AS cha     NO-UNDO.
    DEFINE VARIABLE lines-per-page AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-industry    AS cha     NO-UNDO.
    DEFINE VARIABLE init-dir       AS cha     NO-UNDO.
    DEFINE VARIABLE tmp-dir        AS cha     NO-UNDO.
    DEFINE VARIABLE lv-prt-name    AS cha     NO-UNDO.
    DEFINE VARIABLE list-name      AS cha     NO-UNDO.
    DEFINE VARIABLE lv-font-no     AS INTEGER INIT 11 NO-UNDO.
    DEFINE VARIABLE lv-ornt        AS cha     INIT "L" NO-UNDO.
    DEFINE VARIABLE lv-copy        AS INTEGER NO-UNDO.

    FIND FIRST xest WHERE xest.company EQ cocode
        AND xest.loc     EQ locode
        AND xest.est-no  EQ job.est-no USE-INDEX est-no NO-ERROR.
    IF AVAILABLE xest AND xest.est-type < 5 THEN 
    DO:
    {sys/inc/jobcard.i "F"}
        lv-format = sys-ctrl.char-fld.
        lv-industry = "Fold".
        IF LOOKUP(lv-format-f,"Interpac,FibreFC,Dayton,Livngstn,CentBox") > 0 THEN lines-per-page = 55.
    END.  
    ELSE IF AVAILABLE xest AND xest.est-type <= 6  THEN 
        DO:
        {sys/inc/jobcard.i "C"}
            lv-industry = "Corr".
            lv-format = sys-ctrl.char-fld.
        END.
    /*
    FIND FIRST user-print WHERE user-print.company    EQ cocode  
                            AND user-print.program-id EQ "JOBPRINT"
                            AND user-print.batch  = "" NO-LOCK NO-ERROR.
    IF NOT AVAIL user-print THEN DO TRANSACTION:
       RUN custom/d-printB.w (OUTPUT lv-prt-name,OUTPUT lv-copy).
       CREATE user-print.
       ASSIGN user-print.company = cocode
              user-print.program-id = "JOBPRINT"
              user-print.BATCH = ""
              user-print.PRINTER-NAME = lv-prt-name
              .
    END.
    IF AVAIL user-print THEN lv-prt-name = user-print.PRINTER-NAME.
    */
    lv-prt-name = v-printer-name.

    {sys/inc/print1.i}
    {sys/inc/outprint.i value(lines-per-page) }

    IF lv-industry EQ "Corr" OR 
        LOOKUP(lv-format,"Interpac,FibreFC,Dayton,Livngstn,CentBox,Keystone") GT 0 THEN 
    DO:
        PUT "<PRINTER" lv-prt-name "><COPIES>" v-copies.
    END.
    /* generate ticket */   
    IF lv-format = "FibreFC" THEN 
    DO:  
        PUT UNFORMATTED 
            "<FORMAT=11X17><OLANDSCAPE><P10>" .
        RUN cerep/jobfibre.p (lv-format,0). /* gdm - 07130906 */
    END.
    ELSE IF lv-format = "ARTIOS" THEN 
        DO:   /* For Fibre */    
            PUT UNFORMATTED 
                "<OLANDSCAPE><P10>" .
            RUN cecrep/jobfibre.p (lv-format). 
        END.

    /* print ticket*/
    IF lv-industry EQ "Corr" THEN 
    DO:
        FILE-INFO:FILE-NAME = list-name.
        RUN printfile (FILE-INFO:FILE-NAME).
    END.
    ELSE 
    DO:
        IF LOOKUP(lv-format-f,"Interpac,FibreFC,Dayton,Livngstn,CentBox,Keystone") > 0
            THEN 
        DO:
            FILE-INFO:FILE-NAME = list-name.
            RUN printfile (FILE-INFO:FILE-NAME).   
        END.
        ELSE RUN custom/prntproc.p (list-name, lv-font-no, lv-ornt).
    END.

END.
