/* cerep/jobruff.p   factory ticket  for folding , Ruffino   Copy of keystone  */
/* -------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER v-format LIKE sys-ctrl.char-fld.

{sys/inc/var.i shared}
{sys/form/s-top.f}

{jcrep/r-ticket.i "shared"}

DEFINE NEW SHARED VARIABLE save_id          AS RECID.
DEFINE NEW SHARED VARIABLE v-today          AS DATE      INIT TODAY.
DEFINE NEW SHARED VARIABLE v-job            AS CHARACTER FORMAT "x(6)" EXTENT 2 INIT [" ","zzzzzz"].
DEFINE NEW SHARED VARIABLE v-job2           AS INTEGER   FORMAT "99" EXTENT 2 INIT [00,99].
DEFINE NEW SHARED VARIABLE v-stypart        LIKE style.dscr.
DEFINE            VARIABLE v-dsc            LIKE oe-ordl.part-dscr1 EXTENT 3.
DEFINE NEW SHARED VARIABLE v-size           AS CHARACTER FORMAT "x(26)" EXTENT 2.
DEFINE NEW SHARED VARIABLE v-bld-job        LIKE oe-ord.job-no.
DEFINE NEW SHARED VARIABLE v-bld-job2       LIKE oe-ord.job-no2.
DEFINE NEW SHARED VARIABLE v-fill           AS CHARACTER FORMAT "x(90)".
DEFINE            VARIABLE v-fill78         AS CHARACTER FORMAT "x(78)".
DEFINE NEW SHARED VARIABLE v-frst           AS LOG.
DEFINE NEW SHARED VARIABLE v-ok             AS LOG.
DEFINE NEW SHARED VARIABLE v-est-qty        AS INTEGER   FORMAT "->>,>>>,>>9".
DEFINE NEW SHARED VARIABLE v-job-qty        AS INTEGER   FORMAT "->>,>>>,>>9".
DEFINE NEW SHARED VARIABLE v-fac            AS DECIMAL .
DEFINE NEW SHARED VARIABLE v-job-no         LIKE oe-ordl.job-no.
DEFINE NEW SHARED VARIABLE v-job-no2        LIKE oe-ordl.job-no2.
DEFINE NEW SHARED VARIABLE v-due-date       LIKE oe-ord.due-date.
DEFINE NEW SHARED VARIABLE v-reprint        AS LOG.
DEFINE NEW SHARED VARIABLE v-up             LIKE eb.num-up.
DEFINE NEW SHARED VARIABLE v-tandem         AS LOG.
DEFINE NEW SHARED VARIABLE v-form-no        LIKE eb.form-no.
DEFINE NEW SHARED VARIABLE v-fup            AS CHARACTER.
DEFINE NEW SHARED VARIABLE v-layout         AS CHARACTER FORMAT "x(30)".

DEFINE            VARIABLE v-line           AS INTEGER   INIT 1 NO-UNDO.
DEFINE            VARIABLE v-gsh-qty        AS INTEGER   NO-UNDO.
DEFINE            VARIABLE cnt              AS INTEGER   INIT 1 NO-UNDO.
DEFINE            VARIABLE v-frm-blk        AS CHARACTER FORMAT "x(6)" NO-UNDO.
DEFINE            VARIABLE v-dec            AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-ovund          AS CHARACTER FORMAT "x(34)" NO-UNDO.
DEFINE            VARIABLE v-mrhr           AS CHARACTER FORMAT "x(5)".
DEFINE            VARIABLE v-cas-dscr       LIKE item.est-dscr.
DEFINE            VARIABLE v-first          AS LOG       NO-UNDO.
DEFINE            VARIABLE v-spec-list      AS CHARACTER FORMAT "x(20)"INIT "QA" NO-UNDO.
DEFINE            VARIABLE lv-form-note     AS cha       NO-UNDO.
DEFINE            VARIABLE v-itm-printed    AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-alloc          AS cha       NO-UNDO.
DEFINE            VARIABLE v-prep           AS cha       EXTENT 8 NO-UNDO.
DEFINE            VARIABLE v-misc           AS cha       EXTENT 6 NO-UNDO.
DEFINE            VARIABLE v-spec-no        AS cha       EXTENT 8 NO-UNDO.
DEFINE            VARIABLE v-skip           AS LOG       NO-UNDO.
DEFINE            VARIABLE v-fill2          AS cha       INIT "-" FORM "x(125)" NO-UNDO.
DEFINE            VARIABLE li               AS INTEGER   NO-UNDO.
DEFINE            VARIABLE tb_app-unprinted AS LOG       NO-UNDO.
DEFINE            VARIABLE cDieNo           AS CHARACTER   FORMAT "x(15)" NO-UNDO.    

DEFINE TEMP-TABLE w-lo NO-UNDO
    FIELD layout LIKE v-layout.

DEFINE NEW SHARED BUFFER xjob-hdr FOR job-hdr.

DEFINE            BUFFER b-eb     FOR eb.
DEFINE            BUFFER bf-job-hdr FOR job-hdr.
DEFINE            BUFFER bf-oe-ordl FOR oe-ordl.

DEFINE NEW SHARED WORKFILE wrk-op
    FIELD m-dscr LIKE est-op.m-dscr
    FIELD m-code LIKE est-op.m-code
    FIELD d-seq LIKE est-op.d-seq
    FIELD dept LIKE est-op.dept
    FIELD b-num LIKE est-op.b-num
    FIELD s-num LIKE est-op.s-num
    FIELD pass LIKE est-op.op-pass
    FIELD mr LIKE est-op.op-mr EXTENT 100
    FIELD speed LIKE est-op.op-speed EXTENT 100
    FIELD run-hr LIKE job-mch.run-hr EXTENT 100
    FIELD num-sh LIKE est-op.num-sh EXTENT 100
    FIELD spoil LIKE job-mch.wst-prct EXTENT 100
    FIELD mr-waste LIKE job-mch.mr-waste EXTENT 100    .

DEFINE NEW SHARED WORKFILE wrk-die
    FIELD die-no LIKE eb.die-no
    FIELD form-no LIKE eb.form-no
    FIELD die-size AS CHARACTER FORMAT "x(17)".

DEFINE NEW SHARED WORKFILE wrk-sheet
    FIELD gsh-qty LIKE ef.gsh-qty
    FIELD cal LIKE ef.cal
    FIELD i-no LIKE ef.board
    FIELD brd-dscr LIKE ef.brd-dscr
    FIELD form-no LIKE ef.form-no
    FIELD sh-wid LIKE ef.nsh-len
    FIELD sh-len LIKE ef.nsh-wid.

DEFINE NEW SHARED WORKFILE wrk-film
    FIELD form-no LIKE ef.form-no
    FIELD snum AS INTEGER FORMAT "99"
    FIELD bnum AS INTEGER FORMAT "99"
    FIELD leaf AS CHARACTER FORMAT "x(10)"
    FIELD leaf-l AS DECIMAL FORMAT ">9.9999"
    FIELD leaf-w AS DECIMAL FORMAT ">9.9999".

DEFINE NEW SHARED WORKFILE wrk-ink
    FIELD i-code AS CHARACTER FORMAT "x(10)"
    FIELD form-no LIKE eb.form-no
    FIELD blank-no LIKE eb.blank-no
    FIELD i-dscr AS CHARACTER FORMAT "x(20)"
    FIELD i-qty AS DECIMAL FORMAT ">,>>9.9<"
    FIELD i-pass AS DECIMAL
    FIELD i-unit AS DECIMAL
    FIELD i-seq AS INTEGER
    FIELD i-per AS INTEGER
    FIELD side AS CHARACTER
    FIELD Iform AS INTEGER .

DEFINE NEW SHARED WORKFILE wrk-prep
    FIELD code LIKE est-prep.code
    FIELD dscr LIKE est-prep.dscr
    FIELD s-num AS INTEGER FORMAT "99"
    FIELD b-num AS INTEGER FORMAT "99"
    FIELD ml LIKE est-prep.ml.

DEFINE NEW SHARED WORKFILE wrk-spec
    FIELD form-no LIKE ef.form-no
    FIELD spec-no AS CHARACTER FORMAT "x(10)"
    FIELD dscr AS CHARACTER FORMAT "x(20)"
    FIELD qty AS DECIMAL FORMAT ">>>9.9<<<"
    FIELD uom AS CHARACTER FORMAT "x(3)".

DEFINE NEW SHARED WORKFILE wrk-inst
    FIELD d-seq LIKE dept.fc
    FIELD dscr LIKE est-inst.dscr
    FIELD line LIKE est-inst.line-no
    FIELD rec-id AS RECID.

DEFINE NEW SHARED WORKFILE wrk-misc
    FIELD form-no LIKE ef.form-no
    FIELD snum AS INTEGER FORMAT "99"
    FIELD bnum AS INTEGER FORMAT "99"
    FIELD cost AS CHARACTER FORMAT "x(20)".
  
FORM HEADER
    SKIP(1)
    "07/22/02 Job Ticket QF-130"   TO 132
    WITH NO-BOX NO-ATTR-SPACE FRAME bott PAGE-BOTTOM STREAM-IO WIDTH 132.
     
{custom/notesdef.i}
DEFINE VARIABLE v-inst2          AS cha     EXTENT 40 NO-UNDO.    
DEFINE VARIABLE v-dept-inst      AS cha     FORM "x(80)" EXTENT 40 NO-UNDO.
DEFINE VARIABLE v-spec-inst      AS cha     FORM "x(80)" EXTENT 10 NO-UNDO.
DEFINE VARIABLE v-note-length    AS INTEGER INIT 80 NO-UNDO.

DEFINE VARIABLE v-start-date     AS DATE    NO-UNDO.
DEFINE VARIABLE v-req-date       AS DATE    NO-UNDO.
DEFINE VARIABLE v-shipto         AS cha     FORMAT "x(30)" EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-case-size      AS cha     NO-UNDO.
DEFINE VARIABLE v-vend           LIKE po-ord.vend-no NO-UNDO.
DEFINE VARIABLE v-item           AS cha     EXTENT 50 NO-UNDO.
DEFINE VARIABLE v-ink1           AS cha     EXTENT 50 NO-UNDO.
DEFINE VARIABLE v-ink2           AS cha     EXTENT 50 NO-UNDO.
DEFINE VARIABLE v-po-no          LIKE oe-ordl.po-no NO-UNDO.
DEFINE VARIABLE lv-mat-dept-list AS cha     INIT "FB,FS,WN,WS,GL" NO-UNDO.
DEFINE VARIABLE v-mat-for-mach   AS cha     NO-UNDO.
DEFINE BUFFER xjob-mat FOR job-mat.
DEFINE VARIABLE v-fgitm        AS cha       FORM "x(15)" EXTENT 10 NO-UNDO.
DEFINE VARIABLE v-fgdsc        LIKE eb.part-dscr1 EXTENT 10 NO-UNDO.
DEFINE VARIABLE v-fgqty        LIKE job-hdr.qty EXTENT 10 FORM ">>,>>>,>>>" NO-UNDO.
DEFINE VARIABLE v-pono         LIKE oe-ordl.po-no EXTENT 10 NO-UNDO.
DEFINE VARIABLE v-num-of-fgitm AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-last-order   AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-est-no       LIKE est.est-no NO-UNDO.
DEFINE VARIABLE v-del-date     AS DATE      NO-UNDO.
DEFINE VARIABLE v-over-pct     LIKE oe-ord.over-pct NO-UNDO.
DEFINE VARIABLE dUnderPct     LIKE oe-ord.over-pct NO-UNDO.
DEFINE VARIABLE v-sht-qty      AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-prt-up       LIKE v-up NO-UNDO.
DEFINE VARIABLE v-prt-sht      LIKE v-sht-qty NO-UNDO.
DEFINE TEMP-TABLE tt-fgitm NO-UNDO
    FIELD i-no   AS cha     FORM "x(15)"
    FIELD seq    AS INTEGER
    FIELD qty    AS INTEGER 
    FIELD i-dscr AS cha
    FIELD po-no  AS cha.
DEFINE VARIABLE v-board-po      LIKE oe-ordl.po-no-po NO-UNDO.
DEFINE VARIABLE v-plate-printed AS LOG NO-UNDO.
DEFINE BUFFER xoe-ordl FOR oe-ordl.
DEFINE VARIABLE v-cust-name  LIKE oe-ord.cust-name NO-UNDO.
DEFINE VARIABLE v-cust-name2 LIKE oe-ord.cust-name NO-UNDO.
DEFINE VARIABLE v-cust-name3 LIKE oe-ord.cust-name NO-UNDO.
DEFINE VARIABLE v-cus        AS cha     EXTENT 4 FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-last-j     AS INTEGER NO-UNDO.
DEFINE VARIABLE v-po-no2     LIKE v-po-no NO-UNDO.
DEFINE VARIABLE v-po-no3     LIKE v-po-no NO-UNDO.
DEFINE VARIABLE v-spc-no     AS cha     FORM "x(15)" NO-UNDO.
DEFINE VARIABLE v-ord-qty    AS INTEGER NO-UNDO.
DEFINE VARIABLE v-job-qty2   AS INTEGER NO-UNDO.
DEFINE VARIABLE v-yld-qty    AS INTEGER NO-UNDO.
DEFINE BUFFER bf-eb FOR eb.
DEFINE VARIABLE v-pg-num AS INTEGER NO-UNDO.
DEFINE VARIABLE v-tot-up AS INTEGER NO-UNDO.
ASSIGN
    v-fill   = "<||6><C1><FROM><C82><LINE><||6>"
    v-fill78 = "<||6><C1><FROM><C78><LINE><||6>"
    v-fill2  = FILL("-",80).

DEFINE NEW SHARED FRAME head.

DEFINE SHARED VARIABLE s-prt-mstandard     AS LOG     NO-UNDO.
DEFINE SHARED VARIABLE s-prt-shipto        AS LOG     NO-UNDO.
DEFINE SHARED VARIABLE s-prt-sellprc       AS LOG     NO-UNDO.
DEFINE        VARIABLE v-po-duedate        LIKE po-ordl.due-date NO-UNDO.
DEFINE        VARIABLE v-upc-lbl           AS cha     FORM "x(10)" NO-UNDO.
DEFINE        VARIABLE v-shipto1           AS cha     FORM "x(30)" EXTENT 4 NO-UNDO.
DEFINE        VARIABLE v-shipto2           AS cha     FORM "x(30)" EXTENT 4 NO-UNDO.
DEFINE SHARED VARIABLE s-run-speed         AS LOG     NO-UNDO.
DEFINE        VARIABLE v-ink-seq           AS INTEGER NO-UNDO.
DEFINE        VARIABLE v-ink-list          AS cha     NO-UNDO.
DEFINE        VARIABLE v-ink-use-per-blank AS INTEGER NO-UNDO.  
DEFINE        VARIABLE vs-len              AS cha     NO-UNDO.
DEFINE        VARIABLE vs-wid              AS cha     NO-UNDO.
DEFINE        VARIABLE vs-dep              AS cha     NO-UNDO.
DEFINE        VARIABLE v-i-qty             AS INTEGER NO-UNDO.
DEFINE BUFFER bf-ink FOR wrk-ink.
DEFINE VARIABLE v-ord-no  LIKE job-hdr.ord-no NO-UNDO.
DEFINE VARIABLE ld-metric AS DECIMAL NO-UNDO.
DEFINE VARIABLE ld-wid    AS DECIMAL NO-UNDO.
DEFINE VARIABLE ld-len    AS DECIMAL NO-UNDO.
DEFINE VARIABLE ld-dep    AS DECIMAL NO-UNDO.
DEF VAR v-spoil LIKE job-mch.wst-prct NO-UNDO.
DEF VAR v-output AS INT FORM ">,>>>,>>9" NO-UNDO.
DEFINE VARIABLE cRelStat AS CHARACTER NO-UNDO .
DEFINE BUFFER bf-item FOR ITEM .
DEFINE VARIABLE cBarCodeVal AS CHARACTER NO-UNDO .
DEFINE VARIABLE ls-full-img1 AS CHAR FORMAT "x(200)" NO-UNDO.
DEFINE VARIABLE ls-image1 AS CHAR FORMAT "x(200)" NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER NO-UNDO .
DEF VAR v-spec-cnt AS INT NO-UNDO.
DEFINE VARIABLE iOrderNo AS INTEGER NO-UNDO . 
DEFINE VARIABLE cCustNo AS CHARACTER NO-UNDO .
ASSIGN 
    ls-image1 = "images\ruffino.png"
    FILE-INFO:FILE-NAME = ls-image1
    ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">"  .


/* get values to print */
{cerep/jc-keyst.i}
{cerep/jc-keys2.i}

DEFINE TEMP-TABLE tt-ink NO-UNDO
    FIELD i-code LIKE wrk-ink.i-code
    FIELD i-seq  LIKE wrk-ink.i-seq.

DEFINE TEMP-TABLE tt-reftable NO-UNDO LIKE reftable
    FIELD est-type LIKE est.est-type.


FORMAT "  Customer:" oe-ord.cust-name "Sold To:" oe-ord.sold-id
    "Salesman:" AT 90 oe-ord.sname[1] "Order#:" AT 138 oe-ord.ord-no
    WITH NO-BOX FRAME line-head NO-LABELS STREAM-IO WIDTH 162.
    
{sys/inc/notes.i}
DEFINE VARIABLE lv-jobcard-int AS INTEGER NO-UNDO.
FIND FIRST sys-ctrl WHERE sys-ctrl.company = cocode
    AND sys-ctrl.NAME = "JOBCARDF"
    NO-LOCK NO-ERROR.
lv-jobcard-int = IF AVAILABLE sys-ctrl THEN sys-ctrl.int-fld ELSE 0.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.

ASSIGN
    v-job[1]    = fjob-no
    v-job[2]    = tjob-no
    v-job2[1]   = fjob-no2
    v-job2[2]   = tjob-no2
    v-reprint   = reprint
    v-spec-list = spec-list.

{cerep/jobkeyst.i no-LOCK}
        break by job-hdr.job
              by job-hdr.job-no
              by job-hdr.job-no2
              BY job-hdr.frm
              BY job-hdr.blank-no:

IF est.est-type = 2 THEN
    FOR EACH reftable NO-LOCK WHERE reftable.reftable EQ "jc/jc-calc.p"
        AND reftable.company  EQ job-hdr.company
        AND reftable.loc      EQ ""
        AND reftable.code     EQ STRING(job-hdr.job,"999999999"):
        CREATE tt-reftable.
        BUFFER-COPY reftable TO tt-reftable.
        tt-reftable.est-type = est.est-type.
    END.
FIND FIRST tt-reftable WHERE tt-reftable.reftable EQ "jc/jc-calc.p"
    AND tt-reftable.company  EQ job-hdr.company
    AND tt-reftable.loc      EQ ""
    AND tt-reftable.code     EQ STRING(job-hdr.job,"999999999")
    AND tt-reftable.val[12] = job-hdr.frm   
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE tt-reftable THEN 
DO:
    CREATE tt-reftable.
    ASSIGN 
        tt-reftable.reftable = "jc/jc-calc.p"
        tt-reftable.company  = job-hdr.company
        tt-reftable.loc      = ""
        tt-reftable.CODE     = STRING(job-hdr.job,"999999999")
        tt-reftable.code2    = job-hdr.i-no
        tt-reftable.val[12]  = job-hdr.frm
        tt-reftable.val[13]  = job-hdr.blank-no
        tt-reftable.est-type = est.est-type.

END.
END.
/* end of tt-reftable build*/

{cerep/jobkeyst.i NO-LOCK}
,
EACH tt-reftable WHERE tt-reftable.val[12] EQ job-hdr.frm NO-LOCK
BREAK BY job-hdr.job
BY job-hdr.job-no
BY job-hdr.job-no2
BY tt-reftable.val[12]
BY tt-reftable.val[13]:

FIND FIRST job
    WHERE job.company EQ cocode
    AND job.job     EQ job-hdr.job
    AND job.job-no  EQ job-hdr.job-no
    AND job.job-no2 EQ job-hdr.job-no2
    NO-LOCK NO-ERROR.

IF production AND
    job.cs-trans-date NE ? THEN 
DO:
    li = 0.
    DO WHILE li LT 1000:
        li = li + 1.
        FIND CURRENT job EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF AVAILABLE job THEN
            ASSIGN
                job.pr-printed    = YES
                job.pr-user-id-p  = USERID("nosweat")
                job.pr-print-date = TODAY
                job.pr-print-time = TIME
                li                = 1000.
    END.
END.
     
ELSE 
DO:
    li = 0.
    IF NOT job-hdr.ftick-prnt THEN 
    DO WHILE li LT 1000:
        li = li + 1.
        FIND xjob-hdr EXCLUSIVE-LOCK
            WHERE ROWID(xjob-hdr) EQ ROWID(job-hdr)
            NO-ERROR NO-WAIT.
        IF AVAILABLE xjob-hdr THEN
            ASSIGN
                xjob-hdr.ftick-prnt = YES
                li                  = 1000.
    END.
     
    li = 0.
    DO WHILE li LT 1000:
        li = li + 1.
        FIND CURRENT job EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF AVAILABLE job THEN 
        DO:
            li = 1000.
     
            IF NOT job.cs-printed THEN
                ASSIGN
                    job.cs-printed    = YES
                    job.cs-user-id-p  = USERID("nosweat")
                    job.cs-print-date = TODAY
                    job.cs-print-time = TIME.
     
            IF approve THEN
                ASSIGN
                    job.cs-to-pr      = YES
                    job.cs-user-id-t  = USERID("nosweat")
                    job.cs-trans-date = TODAY
                    job.cs-trans-time = TIME.
        END.
    END.
END.

FIND CURRENT job NO-LOCK NO-ERROR.

ASSIGN
    v-est-qty = IF AVAILABLE est THEN est.est-qty[1] ELSE 0
    ld-metric = IF AVAILABLE est AND est.metric THEN 25.4 ELSE 1. 

IF AVAILABLE job-hdr THEN
    FIND FIRST oe-ord WHERE oe-ord.company EQ job-hdr.company
    AND oe-ord.ord-no  EQ job-hdr.ord-no NO-LOCK NO-ERROR.
      
IF FIRST-OF(tt-reftable.val[12]) THEN v-first = YES.
/** PRINT JOB HEADER **/
IF v-first THEN 
DO:
    ASSIGN
        v-job-no     = job-hdr.job-no
        v-job-no2    = job-hdr.job-no2
        v-est-no     = job-hdr.est-no   
        v-po-no      = IF AVAILABLE oe-ord THEN oe-ord.po-no ELSE ""
        v-ord-no     = job-hdr.ord-no
        v-last-order = "     0".        

    IF AVAILABLE oe-ord THEN
    DO:
        v-last-order = IF oe-ord.po-no2 NE "" THEN oe-ord.po-no2
        ELSE STRING(oe-ord.pord-no).

        IF TRIM(v-last-order) EQ "0" THEN
            v-last-order = "     0".

        IF NOT oe-ctrl.p-fact AND (oe-ord.stat EQ "H" OR oe-ord.priceHold) THEN NEXT.
    END.

    ASSIGN
        v-due-date   = IF AVAILABLE oe-ord THEN oe-ord.due-date ELSE ?
        v-start-date = job-hdr.start-date.

    /* get the earlist release date for the order */
    IF AVAILABLE oe-ord THEN
        FOR EACH oe-rel
            WHERE oe-rel.company EQ oe-ord.company
            AND oe-rel.ord-no  EQ oe-ord.ord-no
            NO-LOCK BY oe-rel.rel-date:            
            FIND FIRST oe-rell WHERE oe-rell.company  EQ oe-rel.company
                AND oe-rell.r-no  EQ oe-rel.link-no
                AND oe-rell.ord-no   EQ oe-rel.ord-no
                AND oe-rell.i-no     EQ oe-rel.i-no
                AND oe-rell.line     EQ oe-rel.line
                AND oe-rell.rel-no   EQ oe-rel.rel-no
                AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
                AND oe-rell.po-no    EQ oe-rel.po-no NO-LOCK NO-ERROR.
            IF NOT AVAILABLE oe-rell THEN
                FIND FIRST oe-rell WHERE oe-rell.company  EQ oe-rel.company
                    AND oe-rell.ord-no   EQ oe-rel.ord-no
                    AND oe-rell.i-no     EQ oe-rel.i-no
                    AND oe-rell.line     EQ oe-rel.line
                    AND oe-rell.rel-no   EQ oe-rel.rel-no
                    AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
                    AND oe-rell.po-no    EQ oe-rel.po-no NO-LOCK NO-ERROR.
            IF AVAILABLE oe-rell THEN 
                FIND oe-relh WHERE oe-relh.company = oe-rel.company
                    AND oe-relh.r-no = oe-rell.r-no NO-LOCK NO-ERROR.
            v-del-date = IF AVAILABLE oe-relh THEN oe-relh.rel-date ELSE oe-rel.rel-date.
            LEAVE.
        END.

    IF NOT FIRST(job-hdr.job-no) THEN 
    DO:
        PAGE.
    END.
    
    IF FIRST-OF(job-hdr.job-no2) THEN 
        v-pg-num = IF PAGE-NUMBER >= 2 THEN PAGE-NUMBER - 1 ELSE 0.
    cBarCodeVal = job-hdr.job-no + "-" + STRING(job-hdr.job-no2,"99") .
    /*v-pg-num = IF PAGE-NUMBER >= 2 THEN PAGE-NUMBER - 1 ELSE 0.*/
     /*VIEW FRAME head.*/       

    /** SUM UP NUMBER OF SHEETS **/
    FIND FIRST job
        WHERE job.company EQ cocode
        AND job.job     EQ job-hdr.job
        AND job.job-no  EQ v-job-no
        AND job.job-no2 EQ v-job-no2
        NO-LOCK NO-ERROR.
            
    IF AVAILABLE job THEN
        FOR EACH job-mch
            WHERE job-mch.company EQ cocode
            AND job-mch.job     EQ job.job
            AND job-mch.job-no  EQ job.job-no
            AND job-mch.job-no2 EQ job.job-no2
            AND job-mch.frm = int(tt-reftable.val[12])
            NO-LOCK,

            FIRST mach
            {sys/ref/machW.i}
              and mach.m-code eq job-mch.m-code
            no-lock

            by mach.d-seq
            by job-mch.frm
            by job-mch.blank-no
            by job-mch.pass
            by job-mch.run-qty desc:

    FIND FIRST wrk-op
        WHERE wrk-op.m-code EQ job-mch.m-code
        AND wrk-op.s-num  EQ job-mch.frm
        AND wrk-op.b-num  EQ job-mch.blank-no
        AND wrk-op.pass   EQ job-mch.pass 
        NO-ERROR.
    IF NOT AVAILABLE wrk-op THEN 
    DO:
        CREATE wrk-op.
        ASSIGN
            wrk-op.m-code = job-mch.m-code
            wrk-op.m-dscr = mach.m-dscr
            wrk-op.d-seq  = mach.d-seq
            wrk-op.dept   = job-mch.dept
            wrk-op.s-num  = job-mch.frm
            wrk-op.b-num  = job-mch.blank-no
            wrk-op.pass   = job-mch.pass.
    END.
    ASSIGN
        wrk-op.mr[job-mch.frm]       = job-mch.mr-hr
        wrk-op.speed[job-mch.frm]    = job-mch.speed
        wrk-op.num-sh[job-mch.frm]   = job-mch.run-qty
        wrk-op.spoil[job-mch.frm]    = job-mch.wst-prct   
        wrk-op.mr-waste[job-mch.frm] = job-mch.mr-waste   
        wrk-op.run-hr[job-mch.frm]   = job-mch.run-hr   .
END.

/** BUILD PREP WORK FILE **/
FOR EACH job-prep
    WHERE job-prep.company EQ cocode
    AND job-prep.job     EQ job-hdr.job
    AND job-prep.job-no  EQ job-hdr.job-no
    AND job-prep.job-no2 EQ job-hdr.job-no2
    NO-LOCK:
    FIND FIRST prep
        WHERE prep.company EQ cocode
        AND prep.code    EQ job-prep.code
        NO-LOCK NO-ERROR.
    CREATE wrk-prep.
    ASSIGN
        wrk-prep.code  = job-prep.code
        wrk-prep.dscr  = IF AVAILABLE prep THEN prep.dscr ELSE ""
        wrk-prep.s-num = job-prep.frm
        wrk-prep.b-num = job-prep.blank-no
        wrk-prep.ml    = job-prep.ml.
END. /* each job-prep */

IF AVAILABLE est THEN
    FOR EACH est-prep
        WHERE est-prep.company EQ est.company
        AND est-prep.est-no  EQ est.est-no
        AND index("SON",est-prep.simon) GT 0
        NO-LOCK:
        FIND FIRST prep
            WHERE prep.company EQ cocode
            AND prep.code    EQ est-prep.code
            NO-LOCK NO-ERROR.
        CREATE wrk-prep.
        ASSIGN
            wrk-prep.code  = est-prep.code
            wrk-prep.dscr  = IF AVAILABLE prep THEN prep.dscr ELSE ""
            wrk-prep.s-num = est-prep.s-num
            wrk-prep.b-num = est-prep.b-num
            wrk-prep.ml    = est-prep.ml.
    END.

IF AVAILABLE oe-ord THEN
    FOR EACH oe-ordm 
        WHERE oe-ordm.company EQ cocode
        AND oe-ordm.ord-no  EQ oe-ord.ord-no
        NO-LOCK:
        FIND FIRST wrk-prep WHERE wrk-prep.code EQ oe-ordm.charge NO-ERROR.
        IF NOT AVAILABLE wrk-prep THEN 
        DO:
            FIND FIRST prep
                WHERE prep.company EQ cocode
                AND prep.code    EQ oe-ordm.charge
                NO-LOCK NO-ERROR.
            CREATE wrk-prep.
            ASSIGN
                wrk-prep.code  = oe-ordm.charge
                wrk-prep.dscr  = IF AVAILABLE prep THEN prep.dscr ELSE ""
                wrk-prep.s-num = 9
                wrk-prep.b-num = 99
                wrk-prep.ml    = IF AVAILABLE prep THEN prep.ml ELSE ?.
        END.
    END.
      

FOR EACH ef
    WHERE ef.company EQ job-hdr.company
    AND ef.est-no  EQ job-hdr.est-no
    AND ef.form-no = tt-reftable.val[12]
    BREAK BY ef.est-no BY ef.form-no:

    v-job-qty = 0.
    FOR EACH xjob-hdr FIELDS(qty)
        WHERE xjob-hdr.company EQ cocode
        AND xjob-hdr.job     EQ job-hdr.job
        AND xjob-hdr.job-no  EQ job-hdr.job-no
        AND xjob-hdr.job-no2 EQ job-hdr.job-no2
        AND xjob-hdr.i-no    EQ job-hdr.i-no
        NO-LOCK:
        v-job-qty = v-job-qty + xjob-hdr.qty.
    END.
          
    ASSIGN
        v-est-qty = 0
        v-yld-qty = 0.
    IF est.est-type EQ 4 THEN
        FOR EACH eb FIELDS(yld-qty)
            WHERE eb.company  EQ ef.company
            AND eb.est-no   EQ ef.est-no
            AND eb.stock-no EQ job-hdr.i-no
            NO-LOCK:
            v-est-qty = v-est-qty + eb.yld-qty.          
        END.
    ELSE v-fac = 1.

    IF est.est-type EQ 4 THEN
        FOR EACH eb FIELDS(yld-qty)
            WHERE eb.company  EQ ef.company
            AND eb.est-no   EQ ef.est-no
            AND eb.form-no  EQ ef.form-no NO-LOCK:
            v-yld-qty = v-yld-qty + eb.yld-qty.
        END.

    v-itm-printed = 0.

    IF ef.form-no EQ tt-reftable.val[12] THEN 
        ebloop:
        FOR EACH eb
            WHERE eb.company     EQ ef.company
            AND eb.est-no      EQ ef.est-no
            AND eb.form-no     EQ ef.form-no
            NO-LOCK
            BREAK BY eb.form-no BY eb.blank-no.

            CREATE w-lo.
            FOR EACH b-eb
                WHERE b-eb.company EQ eb.company
                AND b-eb.est-no  EQ eb.est-no
                AND b-eb.part-no EQ eb.part-no
                NO-LOCK BREAK BY b-eb.est-no:
                v-fup = "F" + trim(STRING(b-eb.form-no,">>9")) + "-" +
                    trim(STRING(b-eb.blank-no,"99")) + "/" +
                    trim(STRING(b-eb.num-up,">>9")) + "up".
                IF LENGTH(TRIM(v-fup)) + length(TRIM(w-lo.layout)) GT 30 THEN 
                DO:
                    substr(w-lo.layout,LENGTH(TRIM(w-lo.layout)),1) = "".
                    CREATE w-lo.
                END.
                w-lo.layout = TRIM(w-lo.layout + " " + trim(v-fup) + ",").
                IF LAST(b-eb.est-no) THEN
                    substr(w-lo.layout,LENGTH(TRIM(w-lo.layout)),1) = "".
            END.
          
            FIND FIRST wrk-die WHERE wrk-die.die-no EQ eb.die-no NO-ERROR.
            IF NOT AVAILABLE wrk-die AND eb.die-no GT "" THEN 
            DO:
                CREATE wrk-die.
                ASSIGN 
                    wrk-die.die-no   = eb.die-no
                    wrk-die.form-no  = eb.form-no
                    wrk-die.die-size = STRING(ef.trim-w) + "x" +
              string(ef.trim-l).
            END.
            
            

           /** BUILD INK WORK FILE **/
           FOR EACH b-eb WHERE b-eb.company EQ cocode
                  AND b-eb.est-no EQ eb.est-no 
                  AND b-eb.form-no EQ eb.form-no .
            FOR EACH job-mat
                WHERE job-mat.company EQ cocode
                AND job-mat.job     EQ job-hdr.job
                AND job-mat.frm     EQ b-eb.form-no
                AND job-mat.blank-no = b-eb.blank-no
                NO-LOCK,
                FIRST item
                {sys/look/itemivW.i}
                and item.i-no eq job-mat.i-no
              no-lock:
              
               DO i = 1 TO 12:
                IF b-eb.i-code2[i] EQ job-mat.i-no THEN 
                DO:
              
                  CREATE wrk-ink .
                   ASSIGN                 
                    wrk-ink.i-code   = b-eb.i-code2[i]
                    wrk-ink.form-no  = b-eb.form-no
                    wrk-ink.blank-no = b-eb.blank-no
                    wrk-ink.i-dscr   = b-eb.i-dscr2[i]
                    wrk-ink.i-pass   = b-eb.i-ps2[i]
                    wrk-ink.i-unit   = b-eb.unitNo[i] 
                    wrk-ink.i-per    = b-eb.i-%2[i]
                    wrk-ink.side     = b-eb.side[i]
                    wrk-ink.Iform    = 1 .
                  END.
                END.                
              END.
             END.
            FOR EACH job-mat
                WHERE job-mat.company EQ cocode
                AND job-mat.job     EQ job-hdr.job
                AND job-mat.frm     EQ eb.form-no
                AND job-mat.blank-no = eb.blank-no
                NO-LOCK,
                FIRST item
                {sys/look/itemivW.i}
                and item.i-no eq job-mat.i-no
              no-lock:

            DO i = 1 TO 12:
                IF eb.i-code2[i] EQ job-mat.i-no THEN 
                DO:
           
                    CREATE wrk-ink.
                    ASSIGN
                        wrk-ink.i-code   = eb.i-code2[i]
                        wrk-ink.form-no  = eb.form-no
                        wrk-ink.blank-no = eb.blank-no
                        wrk-ink.i-dscr   = eb.i-dscr2[i]
                        wrk-ink.i-pass   = eb.i-ps2[i]
                        wrk-ink.i-unit   = eb.unitNo[i] 
                        wrk-ink.i-per    = eb.i-%2[i]
                        wrk-ink.side     = eb.side[i]
                        wrk-ink.i-seq    = i .
                /*  end. */
                END.
            END. /* loop i */

            FIND FIRST wrk-ink
                WHERE wrk-ink.i-code    EQ job-mat.i-no
                AND wrk-ink.form-no   EQ job-mat.frm
                AND (wrk-ink.blank-no EQ job-mat.blank-no OR
                est.est-type     EQ 4)
                NO-ERROR.
                
            IF NOT AVAILABLE wrk-ink                              AND
                (job-mat.blank-no  EQ eb.blank-no OR
                (job-mat.blank-no EQ 0 AND eb.blank-no EQ 1)) THEN 
            DO:
                CREATE wrk-ink.
                ASSIGN
                    wrk-ink.i-code   = job-mat.i-no
                    wrk-ink.form-no  = eb.form-no
                    wrk-ink.blank-no = eb.blank-no
                    wrk-ink.i-dscr   = item.est-dscr
                    wrk-ink.i-pass   = 1
                    wrk-ink.i-unit   = 1.
            END.

            IF AVAILABLE wrk-ink THEN wrk-ink.i-qty = wrk-ink.i-qty + job-mat.qty.
        END. /* JOB-MAT */

    IF eb.est-type EQ 4 THEN v-fac = eb.yld-qty / v-est-qty.
  
    FIND FIRST style
        WHERE style.company EQ eb.company
        AND style.style   EQ eb.style
        NO-LOCK NO-ERROR.
    IF AVAILABLE style THEN v-stypart = style.dscr.
    ASSIGN
        v-dsc[1]  = eb.part-dscr1
        v-dsc[2]  = eb.part-dscr2
        v-size[1] = STRING(eb.len) + "x" + string(eb.wid) + "x" + string(eb.dep)
        v-size[2] = eb.i-coldscr.

    IF eb.blank-no > 0 AND eb.blank-no < 11 THEN 
        ASSIGN v-fgdsc[eb.blank-no] = eb.part-dscr1.

    v-job-qty = 0.
    FOR EACH xjob-hdr FIELDS(qty) WHERE xjob-hdr.company EQ cocode
        AND xjob-hdr.job     EQ job-hdr.job
        AND xjob-hdr.job-no  EQ job-hdr.job-no
        AND xjob-hdr.job-no2 EQ job-hdr.job-no2
        AND xjob-hdr.i-no    EQ eb.stock-no NO-LOCK:
        v-job-qty = v-job-qty + xjob-hdr.qty.
    END.
    v-ord-qty = 0.
    IF job-hdr.ord-no EQ 0 THEN v-ord-qty = v-job-qty.
    ELSE
        FOR EACH oe-ordl FIELDS(qty)
            WHERE oe-ordl.company EQ job-hdr.company
            AND oe-ordl.ord-no  EQ job-hdr.ord-no
            AND oe-ordl.job-no  EQ job-hdr.job-no
            AND oe-ordl.job-no2 EQ job-hdr.job-no2
            AND oe-ordl.i-no EQ eb.stock-no
            AND oe-ordl.form-no = int(tt-reftable.val[12])
            NO-LOCK .
            v-ord-qty = v-ord-qty + oe-ordl.qty .
        END.
      
    v-job-qty2 = 0.
    FOR EACH xjob-hdr FIELDS(qty) WHERE
        xjob-hdr.company EQ cocode
        AND xjob-hdr.job     EQ job-hdr.job
        AND xjob-hdr.job-no  EQ job-hdr.job-no
        AND xjob-hdr.job-no2 EQ job-hdr.job-no2
        AND xjob-hdr.frm    EQ int(tt-reftable.val[12]) NO-LOCK:
        v-job-qty2 = v-job-qty2 + xjob-hdr.qty.
    END.
    /** PRINT ITEM **/
    
    FIND FIRST xjob-hdr WHERE xjob-hdr.company EQ cocode
                AND xjob-hdr.job     EQ job-hdr.job
                AND xjob-hdr.job-no  EQ job-hdr.job-no
                AND xjob-hdr.job-no2 EQ job-hdr.job-no2
                AND xjob-hdr.frm EQ eb.form-no
                AND xjob-hdr.blank-no EQ eb.blank-no
                AND xjob-hdr.i-no EQ eb.stock-no NO-LOCK NO-ERROR.    
               
    iOrderNo =  IF AVAIL xjob-hdr THEN xjob-hdr.ord-no ELSE job-hdr.ord-no .
    cCustNo =  IF AVAIL xjob-hdr THEN xjob-hdr.cust-no ELSE job-hdr.cust-no .
    v-shipto = "".
    FIND FIRST oe-ordl
        WHERE oe-ordl.company EQ job-hdr.company
        AND oe-ordl.ord-no  EQ iOrderNo
        AND oe-ordl.job-no  EQ job-hdr.job-no
        AND oe-ordl.job-no2 EQ job-hdr.job-no2
        AND oe-ordl.i-no    EQ eb.stock-no /*job-hdr.i-no*/
        NO-LOCK NO-ERROR.

    FIND FIRST oe-ord WHERE oe-ord.company EQ job-hdr.company
        AND oe-ord.ord-no  EQ iOrderNo NO-LOCK NO-ERROR.
    
    IF AVAILABLE oe-ord THEN 
    DO:
        FIND FIRST shipto
            WHERE shipto.company EQ cocode
            AND shipto.cust-no EQ oe-ord.cust-no
            AND shipto.ship-id EQ oe-ord.ship-id
            NO-LOCK NO-ERROR.  
        IF AVAILABLE shipto THEN
            ASSIGN v-shipto[1] = shipto.ship-name 
                v-shipto[2] = shipto.ship-addr[1]
                v-shipto[3] = shipto.ship-addr[2]
                v-shipto[4] = TRIM(shipto.ship-city) + ", " +
                               shipto.ship-state + "  " + shipto.ship-zip.          
    END.
   

    v-req-date = IF AVAILABLE oe-ordl THEN oe-ordl.req-date ELSE ?.
    FIND FIRST cust WHERE cust.company = job-hdr.company AND
        cust.cust-no = cCustNo NO-LOCK NO-ERROR.
    v-cust-name = IF AVAILABLE oe-ord THEN oe-ord.cust-name 
    ELSE IF AVAILABLE cust THEN cust.name
    ELSE job-hdr.cust-no.       
    ASSIGN          
        v-cus[2] = cust.addr[1]
        v-cus[3] = cust.addr[2]
        v-cus[4] = TRIM(cust.city) + ", " + cust.state + "  " + cust.zip
        v-line   = IF AVAILABLE est                            AND
                    est.est-type GT 2 AND est.est-type LT 5 THEN 500 ELSE 50.     

    IF AVAILABLE oe-ordl THEN 
    DO:
        v-est-qty = oe-ordl.qty.
        FIND FIRST oe-ord OF oe-ordl NO-LOCK.
        v-ovund = STRING("Overrun/Underrun %:  " +
            trim(STRING(oe-ordl.over-pct,">>9.99")) + "/" +
            trim(STRING(oe-ordl.under-pct,">>9.99"))).
    END.
    ELSE v-est-qty = v-job-qty.
            
    RELEASE w-lo.
    FIND FIRST w-lo NO-ERROR.
    v-case-size = STRING(eb.cas-len) + "x" + string(eb.cas-wid) + "x" +
        string(eb.cas-dep).

    ASSIGN 
        vs-len = ""
        vs-wid = ""
        vs-dep = "".
            
    IF eb.cas-len <> 0 THEN RUN sys/inc/dec-frac.p (eb.cas-len,64,OUTPUT vs-len).
    IF eb.cas-len <> 0 THEN RUN sys/inc/dec-frac.p (eb.cas-wid,64,OUTPUT vs-wid).
    IF eb.cas-len <> 0 THEN RUN sys/inc/dec-frac.p (eb.cas-dep,64,OUTPUT vs-dep).
            
    ASSIGN
        v-case-size = (IF vs-len <> "" THEN TRIM(vs-len) + "x" ELSE "") +
                             (IF vs-wid <> "" THEN TRIM(vs-wid) + "x" ELSE "") +
                              trim(vs-dep)
        v-tot-up    = 0.
            
    FOR EACH bf-eb FIELDS(num-up) WHERE
        bf-eb.company = eb.company AND
        bf-eb.est-no = eb.est-no AND
        bf-eb.form-no = eb.form-no NO-LOCK:
        v-tot-up = v-tot-up + bf-eb.num-up.                
    END.

    ASSIGN
        v-up      = eb.num-up
        v-po-no   = IF AVAILABLE oe-ordl THEN oe-ordl.po-no ELSE ""
        v-spc-no  = eb.spc-no
        v-job-qty = IF AVAILABLE oe-ordl THEN oe-ordl.qty ELSE job-hdr.qty.            
    FIND FIRST item
        WHERE item.company EQ cocode
        AND item.i-no    EQ eb.cas-no
        NO-LOCK NO-ERROR.
    v-cas-dscr = IF AVAILABLE item THEN item.i-name ELSE "".
    FIND FIRST tt-keyst
            WHERE tt-keyst.tt-job-no  EQ job-hdr.job-no
            AND tt-keyst.tt-job-no2 EQ job-hdr.job-no2
            AND tt-keyst.tt-frm     EQ int(tt-reftable.val[12])
            NO-ERROR.
        IF NOT AVAILABLE tt-keyst THEN CREATE tt-keyst.

        /* Number of sheets */
        RUN oe/rep/ticket1.p (RECID(ef), RECID(job-hdr)).
        FIND FIRST wrk-sheet WHERE RECID(wrk-sheet) EQ save_id.
        IF AVAILABLE oe-ordl THEN 
            FIND FIRST po-ord WHERE po-ord.company = oe-ordl.company
                AND po-ord.po-no = int(oe-ordl.po-no-po) NO-LOCK NO-ERROR.
        ASSIGN
            v-vend     = IF AVAILABLE oe-ordl THEN oe-ordl.vend-no ELSE ""
            v-board-po = IF AVAILABLE oe-ordl THEN oe-ordl.po-no-po ELSE 0.
        IF AVAILABLE oe-ordl THEN FIND oe-ord OF oe-ordl NO-LOCK NO-ERROR.
        ASSIGN
            v-over-pct = IF AVAILABLE oe-ordl THEN oe-ordl.over-pct ELSE
                          IF AVAILABLE oe-ord  THEN oe-ord.over-pct  ELSE 1
            dUnderPct  = IF AVAILABLE oe-ordl THEN oe-ordl.under-pct ELSE
                          IF AVAILABLE oe-ord  THEN oe-ord.under-pct  ELSE 1
            v-prt-up   = v-tot-up * ef.n-out-l.
        IF AVAILABLE po-ord THEN
            FIND FIRST po-ordl WHERE
                po-ordl.company EQ po-ord.company AND
                po-ordl.po-no   EQ po-ord.po-no AND
                po-ordl.i-no = wrk-sheet.i-no
                NO-LOCK NO-ERROR. 

        v-po-duedate = IF AVAILABLE po-ordl THEN po-ordl.due-date ELSE ?.


    IF FIRST-OF(eb.form-no) THEN do:
         ASSIGN cDieNo = eb.die-no.
         RUN pPrintHeader(1) .
        
          PUT "<R5><C1><FGCOLOR=GREEN>CUSTOMER                          SHIP TO"            "<P12><C60>Job#: <FGCOLOR=BLACK>"  string(job-hdr.job-no + "-" + string(job-hdr.job-no2,"99")) FORM "x(10)" "<P10>" SKIP
            v-cust-name            v-shipto[1] AT 35                    "<C60><FGCOLOR=GREEN> CUST PO: <FGCOLOR=BLACK>" (IF job-hdr.po-no NE "" THEN job-hdr.po-no ELSE if avail oe-ord THEN oe-ord.po-no ELSE "")  FORMAT "x(15)"   SKIP          
            v-cus[2]  v-shipto[2] AT 35                                 "<C60><FGCOLOR=GREEN>ORD DATE: <FGCOLOR=BLACK>"  (if avail oe-ord THEN string(oe-ord.ord-date) ELSE "")  FORMAT "x(10)"        SKIP
            v-cus[3]  v-shipto[3] AT 35                                 "<C60><FGCOLOR=GREEN>DUE DATE: <FGCOLOR=BLACK>"  (if avail oe-ord THEN string(oe-ord.due-date) ELSE "")  FORMAT "x(10)" SKIP
            v-cus[4]  v-shipto[4] AT 35                                 "<C60><FGCOLOR=GREEN>LAST JOB: <FGCOLOR=BLACK>"   v-last-order FORM "X(6)"        SKIP
            "<C60><FGCOLOR=GREEN>Estimate#: <FGCOLOR=BLACK>"   trim(job-hdr.est-no) FORM "X(6)"        SKIP
            v-fill SKIP
            "<R-0.5><C1><FGCOLOR=GREEN>Item On Job:" SKIP
            "<C4><P9>ITEM#  <C16>CUST PART# <C28>DESCRIPTION  <C50>FORM <C54>BLANK <C59>#UP <C64>ORDER QTY <C72>OVERS <C77>UNDERS <FGCOLOR=BLACK>" SKIP .
            iCount = 1 .
            FOR EACH bf-eb NO-LOCK
                WHERE bf-eb.company EQ eb.company
                  AND bf-eb.est-no EQ eb.est-no 
                  AND bf-eb.form-no EQ eb.form-no BREAK BY bf-eb.blank-no :

                FIND FIRST bf-oe-ordl NO-LOCK
                    WHERE bf-oe-ordl.company EQ job-hdr.company
                    AND bf-oe-ordl.ord-no  EQ job-hdr.ord-no
                    AND bf-oe-ordl.job-no  EQ job-hdr.job-no
                    AND bf-oe-ordl.job-no2 EQ job-hdr.job-no2
                    AND bf-oe-ordl.i-no EQ bf-eb.stock-no
                    AND bf-oe-ordl.form-no = int(tt-reftable.val[12])
                    NO-ERROR .
                FIND FIRST bf-job-hdr  NO-LOCK
                    WHERE bf-job-hdr.company EQ job-hdr.company
                    AND bf-job-hdr.ord-no  EQ job-hdr.ord-no
                    AND bf-job-hdr.job-no  EQ job-hdr.job-no
                    AND bf-job-hdr.job-no2 EQ job-hdr.job-no2
                    AND bf-job-hdr.i-no EQ bf-eb.stock-no
                    AND bf-job-hdr.frm = int(tt-reftable.val[12])
                    NO-ERROR .

               
                PUT "<C1.5>" STRING(string(iCount) + "." ) FORMAT "x(2)" 
                    "<C4>"  bf-eb.stock-no FORMAT "x(15)"
                    "<C16>" bf-eb.part-no FORMAT "x(15)"
                    "<C28>" bf-eb.part-dscr1 FORMAT "x(30)"
                    "<C51>" bf-eb.form-no FORMAT ">>"
                    "<C55>" bf-eb.blank-no FORMAT ">>"
                    "<C59>" bf-eb.num-up FORMAT ">>>"
                    "<C64>" (IF AVAIL bf-oe-ordl THEN bf-oe-ordl.qty ELSE IF AVAIL bf-job-hdr THEN bf-job-hdr.qty ELSE 0) FORMAT ">>>>>>>>"
                    "<C72>" (IF AVAIL bf-oe-ordl THEN bf-oe-ordl.over-pct ELSE v-over-pct) FORMAT ">>>>%"
                    "<C77>" (IF AVAIL bf-oe-ordl THEN bf-oe-ordl.under-pct ELSE dUnderPct) FORMAT ">>>>%" SKIP.
                iCount = iCount + 1 .
            END.
            PUT "<P10>" v-fill SKIP.
            
             /** PRINT SHEET **/
        x = 2.
        FOR EACH wrk-sheet BREAK BY wrk-sheet.form-no:
            v-size[1] = "".
            v-size[2] = "".
            RUN sys/inc/dec-frac.p (wrk-sheet.sh-len,64,OUTPUT vs-len).
            RUN sys/inc/dec-frac.p (wrk-sheet.sh-wid,64,OUTPUT vs-wid).
            IF SUBSTRING(vs-len,1,1) = "-" THEN vs-len = SUBSTRING(vs-len,2).
            IF SUBSTRING(vs-wid,1,1) = "-" THEN vs-len = SUBSTRING(vs-wid,2).
            IF SUBSTRING(vs-dep,1,1) = "-" THEN vs-len = SUBSTRING(vs-dep,2).
            ASSIGN 
                v-size[1] = TRIM(vs-wid) + " x " + trim(vs-len).

            RUN sys/inc/dec-frac.p (ef.nsh-len,64,OUTPUT vs-len).
            RUN sys/inc/dec-frac.p (ef.nsh-wid,64,OUTPUT vs-wid).
            IF SUBSTRING(vs-len,1,1) = "-" THEN vs-len = SUBSTRING(vs-len,2).
            IF SUBSTRING(vs-wid,1,1) = "-" THEN vs-len = SUBSTRING(vs-wid,2).
            /*task# 09260501*/
            ASSIGN 
                v-size[2] = IF lv-jobcard-int = 1 THEN TRIM(vs-len) + " x " + TRIM(vs-wid)
                                    ELSE TRIM(vs-wid) + " x " + TRIM(vs-len).

            IF est.est-type <> 4 THEN v-sht-qty = (v-ord-qty / v-tot-up) + (v-ord-qty / v-tot-up * v-over-pct / 100).
            ELSE v-sht-qty = (v-yld-qty / v-tot-up) + (v-yld-qty / v-tot-up * v-over-pct / 100).

            v-prt-sht = v-sht-qty / ef.n-out-l.

            FIND FIRST bf-item  NO-LOCK 
                where bf-item.company eq cocode
                and bf-item.i-no    eq wrk-sheet.i-no NO-ERROR .
                
                                               
            PUT "<FGCOLOR=GREEN>NAME                <C18>QTY SHEETS  <C28>SHEET SIZE        <C45>MIN SHEET SIZE       <C62.3>PO#  <C66>STOCK DUE <C75>VENDOR  <FGCOLOR=BLACK>"  SKIP                
                "<C1>" ( IF AVAIL bf-item THEN string(bf-item.i-NAME) ELSE "" ) FORM "x(20)" 
                "<C18>" v-sht-qty FORM "->,>>>,>>9" SPACE(2)
                "<C28>" v-size[1] FORM "X(18)" 
                "<C45>" v-size[2] FORM "X(16)" 
                "<C60>" v-board-po FORM ">>>>>9"
                "<C66>" v-po-duedate FORM "99/99/9999" 
                "<C75>" v-vend FORMAT "x(10)" SKIP(1).

            x = 1.
        END. /* each wrk-sheet */            
        PUT  v-fill SKIP.
                
        ASSIGN
            x      = 2
            i      = 1
            v-ink1 = ""
            v-ink2 = "".

        FOR EACH wrk-ink WHERE wrk-ink.form-no = eb.form-no
             AND wrk-ink.Iform EQ 1 
            BREAK BY wrk-ink.i-code
            BY wrk-ink.i-pass
            BY wrk-ink.i-unit
            :
             IF NOT FIRST-OF(wrk-ink.i-code) THEN 
                 DELETE wrk-ink.
             ELSE IF wrk-ink.i-unit = 0 THEN wrk-ink.i-unit = 999 .
        END. /* each wrk-ink */
             
        FOR EACH wrk-ink WHERE wrk-ink.form-no = eb.form-no
             AND wrk-ink.Iform EQ 1
            BREAK BY wrk-ink.i-pass
            BY wrk-ink.i-unit
            :
              IF wrk-ink.i-unit = 999 THEN wrk-ink.i-unit = 0 .
              ASSIGN v-ink1[i] = STRING(wrk-ink.form-no,">9") + "  " + /*"1  " + */
                  STRING(wrk-ink.i-code,"X(11)") + " " + 
                  string(wrk-ink.i-dscr,"x(30)") + " " + trim(string(wrk-ink.i-per,">>>>%")) + " " + STRING(wrk-ink.i-unit,">>>") + " " + STRING(wrk-ink.side)
                  /*v-item[i]*/
                  /*+ (IF i = 1 THEN "  " + eb.plate-no ELSE "") */
                  i         = i + 1         . 
          DELETE wrk-ink. 
        END. /* each wrk-ink */
        ASSIGN
            v-skip          = NO
            v-plate-printed = NO.
        iCount = 1 .
        PUT "<R-1><FGCOLOR=GREEN>INKS: <FGCOLOR=BLACK>" string(i) FORMAT "x(3)" "<FGCOLOR=GREEN>PASSES: <FGCOLOR=BLACK>" string(eb.i-pass) FORMAT "x(3)" "<FGCOLOR=GREEN>COATS: <FGCOLOR=BLACK>"  string(eb.i-coat) FORMAT "x(3)"
             "<FGCOLOR=GREEN>PASSES: <FGCOLOR=BLACK>"  string(eb.i-coat-p) FORMAT "x(3)"   SKIP
            "<FGCOLOR=GREEN>INK DESCRIPTION:<FGCOLOR=BLACK> "   eb.i-coldscr FORMAT "x(35)" SKIP.
        
        PUT "<P9><#5>" "<FGCOLOR=GREEN> F  COLORS      DESCRIPTION <C36.2>Per<C40.7>U <C42.5>S <FGCOLOR=BLACK>" SKIP. 
        DO j = 1 TO 12:
            IF TRIM(v-ink1[j]) = "-" THEN v-ink1[j] = "".               
            IF v-ink1[j] <> "" THEN do:
                PUT v-ink1[j] FORM "x(59)" SKIP .
                iCount = iCount + 1.
            END.
        END.
       
        PUT SKIP(12 - iCount ) "<R-14>" .
        PUT "<P10><C44><From><R+14><C44><Line><||6><R-14>" .
        v-skip = NO.
                
             
        FIND FIRST prep NO-LOCK
            WHERE prep.company EQ cocode
              AND prep.CODE EQ cDieNo AND cDieNo NE "" NO-ERROR .
        PUT  SKIP(1)
            "<FGCOLOR=GREEN><C44> CAD#:<FGCOLOR=BLACK> " eb.cad-no FORM "x(25)"  SKIP
            "<FGCOLOR=GREEN><C44> PLATES:<FGCOLOR=BLACK> " eb.plate-no FORM "x(25)" /*eb.plate-no*/ SKIP
            "<FGCOLOR=GREEN><C44> NOTES/COMMENTS:<FGCOLOR=BLACK> "  SKIP(2)                 
            "<FGCOLOR=GREEN><C44> DIE<FGCOLOR=BLACK> " cDieNo FORM "x(25)" /*eb.die-no*/ SKIP
            "<FGCOLOR=GREEN><C44> DIE DESCR:<FGCOLOR=BLACK> " (IF AVAIL prep THEN prep.dscr ELSE "")  FORMAT "x(35)"   SKIP
            "<FGCOLOR=GREEN><C44> DIE SIZE:<FGCOLOR=BLACK> " string(ef.trim-w) + "x" + string(ef.trim-l) FORMAT "x(25)" SKIP
            "<FGCOLOR=GREEN><C44> NOTES/COMMENTS:<FGCOLOR=BLACK> "  SKIP(4)                
            "<R-1>" v-fill  SKIP.
         
            lv-line-chars = 80.
            FIND FIRST job OF job-hdr NO-LOCK NO-ERROR.       
            {custom/notespr5.i job v-inst2 40 "notes.rec_key = job.rec_key and notes.note_code <> '' AND (notes.note_form_no EQ tt-reftable.val[12] OR notes.note_form_no EQ 0) AND LOOKUP(notes.note_code,v-exc-depts) EQ 0"}
            DO i = 1 TO 40:
                v-dept-inst[i] = v-inst2[i].
            END.
       
             
        PUT "<FGCOLOR=GREEN><C1>NOTES: <FGCOLOR=BLACK>" SKIP
            "  " v-dept-inst[1] FORM "x(80)"  SKIP
            "  " v-dept-inst[2] FORM "x(80)"  SKIP
            "  " v-dept-inst[3] FORM "x(80)"  SKIP
            "  " v-dept-inst[4] FORM "x(80)"  SKIP.
           DO i = 5 TO 40:
                IF v-dept-inst[i] NE "" THEN 
                    PUT "  "  v-dept-inst[i] FORM "x(80)"  SKIP.
                IF PAGE-SIZE - LINE-COUNTER LE 5 THEN do:
                     PAGE.
                     RUN pPrintHeader(1) .
                END.
           END.

          PUT SKIP(1) v-fill  SKIP.  

        PUT "<R+2><C1><FROM><C82><LINE><||6><R-4>" SKIP 
            "<C53><From><R+3><C53><Line><||6><R-4>" SKIP
            "<C68><From><R+3><C68><Line><||6><R-2>" .

        PUT "<C1><FGCOLOR=GREEN>MACHINE           MR WASTE  MR HRS   RUN SPEED  SPOLL    INPUT  GOOD SHEETS/PCS   OPER INIT/DATE  <FGCOLOR=BLACK>" SKIP(1) .
        j = 0 .
        MAIN:
        FOR EACH wrk-op WHERE wrk-op.s-num = job-hdr.frm BREAK by wrk-op.d-seq by wrk-op.b-num:
             v-mat-for-mach = "".
             IF lookup(wrk-op.dept,lv-mat-dept-list) > 0 THEN DO:
                 
                FOR EACH xjob-mat WHERE xjob-mat.company eq cocode
                                       and xjob-mat.job     eq job-hdr.job
                                       and xjob-mat.job-no  eq job-hdr.job-no
                                       and xjob-mat.job-no2 eq job-hdr.job-no2
                                       AND xjob-mat.frm = job-hdr.frm
                                       /*AND (xjob-mat.blank-no = job-hdr.blank-no
                                            OR xjob-mat.blank-no = 0)*/  NO-LOCK,
                     first ITEM WHERE ITEM.company = cocode AND
                                      ITEM.i-no = xjob-mat.rm-i-no AND
                                      ITEM.mat-type = SUBSTRING(wrk-op.dept,1,1) NO-LOCK :
                     v-mat-for-mach = ITEM.i-name + fill(" ", 33 - LENGTH(ITEM.i-name))  /*"       " */ +
                                      string(xjob-mat.wid) + "x" + STRING(xjob-mat.len) +
                                      "      " + string(xjob-mat.qty).                   
                     LEAVE.                 
                END.                            
             END.     

             IF LAST(wrk-op.d-seq) THEN DO: /* pallet code*/
                FOR EACH xjob-mat WHERE xjob-mat.company eq cocode
                                       and xjob-mat.job     eq job-hdr.job
                                       and xjob-mat.job-no  eq job-hdr.job-no
                                       and xjob-mat.job-no2 eq job-hdr.job-no2
                                       AND xjob-mat.frm = job-hdr.frm
                                       AND (xjob-mat.blank-no = job-hdr.blank-no
                                            OR xjob-mat.blank-no = 0) NO-LOCK,
                     first ITEM WHERE ITEM.company = cocode AND
                                      ITEM.i-no = xjob-mat.rm-i-no AND 
                                      ITEM.mat-type = "D" NO-LOCK :
                     v-mat-for-mach = v-mat-for-mach + "      " + ITEM.i-name.
                 END.
             END.          
             v-spoil = ROUND( ((wrk-op.num-sh[job-hdr.frm] - wrk-op.mr-waste[job-hdr.frm])
                       * wrk-op.spoil[job-hdr.frm] / 100),0).
             v-output = wrk-op.num-sh[job-hdr.frm] - wrk-op.mr-waste[job-hdr.frm] - v-spoil.

              PUT "<R+1><C1><FROM><C82><LINE><||6><R-1>"  
                  "<C53><From><R+1><C53><Line><||6><R-1>" 
                  "<C68><From><R+1><C68><Line><||6><R-1><C1>" .

             IF s-prt-mstandard THEN DO:                
                /*IF s-run-speed THEN*/
                   PUT wrk-op.m-dscr   SPACE(2)
                       wrk-op.mr-waste[job-hdr.frm]   SPACE(2)
                       wrk-op.mr[job-hdr.frm]         SPACE(4)
                       wrk-op.speed[job-hdr.frm] FORMAT ">>>>>9"     SPACE(1)
                      v-spoil FORM ">>,>>9"     SPACE(2)
                       wrk-op.num-sh[job-hdr.frm] SPACE(3)
                       
                     SKIP. 
             END.
             ELSE PUT wrk-op.m-dscr   SPACE(5)
                      SKIP.
             IF PAGE-SIZE - LINE-COUNTER LE 5 THEN do:
                     PAGE.
                     RUN pPrintHeader(1) .
             END.
        end. /* each wrk-op*/
         
    END.  /* first-of eb */
          
    /*IF LAST-OF(eb.form-no) THEN 
    DO:*/
     PAGE.
     RUN pPrintHeader(0) .

     v-inst2 = "".
     v-spec-cnt = 0 .
     v-spec-inst = "" .
     FIND FIRST itemfg WHERE itemfg.company = job-hdr.company
         AND itemfg.i-no = eb.stock-no NO-LOCK NO-ERROR.
     {custom/notespr2.i itemfg v-inst2 4
          "notes.rec_key = itemfg.rec_key and lookup(notes.note_code,spec-list) > 0 " }
         DO i = 1 TO 4:
             IF v-inst2[i] <> "" THEN do:
                 v-spec-cnt = v-spec-cnt  + 1.
                 v-spec-inst[v-spec-cnt] = v-inst2[i].
             END.
         END.

        PUT "<FGCOLOR=GREEN><C1>CUSTOMER                          SHIP TO"            "<P12><C60>Job#: <FGCOLOR=BLACK>"  string(job-hdr.job-no + "-" + string(job-hdr.job-no2,"99") + "-" + STRING(eb.form-no,"99") + "-" + STRING(eb.blank-no,"99") ) FORM "x(16)" "<P10>" SKIP
            v-cust-name            v-shipto[1] AT 35                    "<C60><FGCOLOR=GREEN> Cust Po: <FGCOLOR=BLACK>"  (if avail oe-ordl THEN string(oe-ordl.po-no) ELSE "")  FORMAT "x(15)"         SKIP          
            v-cus[2]  v-shipto[2] AT 35                                 "<C60><FGCOLOR=GREEN>Ord Date: <FGCOLOR=BLACK>"  (if avail oe-ord THEN string(oe-ord.ord-date) ELSE "")  FORMAT "x(10)"         SKIP
            v-cus[3]  v-shipto[3] AT 35                                 "<C60><FGCOLOR=GREEN>Due Date: <FGCOLOR=BLACK>"   (if avail oe-ord THEN string(oe-ord.due-date) ELSE "")  FORMAT "x(10)"   SKIP
            v-cus[4]  v-shipto[4] AT 35                                 "<C60><FGCOLOR=GREEN>Last Job: <FGCOLOR=BLACK>"    v-last-order FORM "X(6)"       SKIP
            "<C60><FGCOLOR=GREEN>Estimate#: <FGCOLOR=BLACK>"   trim(job-hdr.est-no) FORM "X(6)"        SKIP
            v-fill SKIP
            "<FGCOLOR=GREEN>ORDER QUANTITY:<FGCOLOR=BLACK>" v-ord-qty   "<C25><FGCOLOR=GREEN>OVER:<FGCOLOR=BLACK>" v-over-pct FORMAT ">>>>%" "<C36><FGCOLOR=GREEN>UNDER:<FGCOLOR=BLACK>" dUnderPct FORMAT ">>>>%" 
             "<C54><FGCOLOR=GREEN>PRINT #UP:<FGCOLOR=BLACK>"   v-prt-up FORM ">>9" "    <FGCOLOR=GREEN>DIE CUT #UP:<FGCOLOR=BLACK>" v-tot-up  FORM ">>9"  /*"<C51><FGCOLOR=GREEN>TOTAL COLORS<FGCOLOR=BLACK> "   eb.i-coldscr*/ SKIP
            "<C1><FGCOLOR=GREEN>   FG ITEM: <FGCOLOR=BLACK>" eb.stock-no FORMAT "x(15)"      "<C25><FGCOLOR=GREEN>DESC:<FGCOLOR=BLACK>" eb.part-dscr1 FORMAT "x(30)"        "<C54><FGCOLOR=GREEN>STYLE: <FGCOLOR=BLACK>" v-stypart FORMAT "x(30)" SKIP
            "<C1><FGCOLOR=GREEN> CUST PART: <FGCOLOR=BLACK>" eb.part-no FORMAT "x(15)"     "<C29>"    eb.part-dscr2 FORMAT "x(30)"          "<C54><FGCOLOR=GREEN> SIZE: <FGCOLOR=BLACK>" string(STRING(eb.len) + "x" + string(eb.wid) + "x" + string(eb.dep)) FORMAT "x(40)" SKIP
            "<C1><FGCOLOR=GREEN>SPEC NOTES: <FGCOLOR=BLACK>" v-spec-inst[1] FORM "x(128)" SKIP
            "<C11>" v-spec-inst[2] FORM "x(128)" SKIP
            "<C11>" v-spec-inst[3] FORM "x(128)" SKIP
            v-fill SKIP    .

        /** PRINT SHEET **/
        x = 2.
        FOR EACH wrk-sheet BREAK BY wrk-sheet.form-no:
            v-size[1] = "".
            v-size[2] = "".
            RUN sys/inc/dec-frac.p (wrk-sheet.sh-len,64,OUTPUT vs-len).
            RUN sys/inc/dec-frac.p (wrk-sheet.sh-wid,64,OUTPUT vs-wid).
            IF SUBSTRING(vs-len,1,1) = "-" THEN vs-len = SUBSTRING(vs-len,2).
            IF SUBSTRING(vs-wid,1,1) = "-" THEN vs-len = SUBSTRING(vs-wid,2).
            IF SUBSTRING(vs-dep,1,1) = "-" THEN vs-len = SUBSTRING(vs-dep,2).
            ASSIGN 
                v-size[1] = TRIM(vs-wid) + " x " + trim(vs-len).

            RUN sys/inc/dec-frac.p (ef.nsh-len,64,OUTPUT vs-len).
            RUN sys/inc/dec-frac.p (ef.nsh-wid,64,OUTPUT vs-wid).
            IF SUBSTRING(vs-len,1,1) = "-" THEN vs-len = SUBSTRING(vs-len,2).
            IF SUBSTRING(vs-wid,1,1) = "-" THEN vs-len = SUBSTRING(vs-wid,2).
            /*task# 09260501*/
            ASSIGN 
                v-size[2] = IF lv-jobcard-int = 1 THEN TRIM(vs-len) + " x " + TRIM(vs-wid)
                                    ELSE TRIM(vs-wid) + " x " + TRIM(vs-len).

            IF est.est-type <> 4 THEN v-sht-qty = (v-ord-qty / v-tot-up) + (v-ord-qty / v-tot-up * v-over-pct / 100).
            ELSE v-sht-qty = (v-yld-qty / v-tot-up) + (v-yld-qty / v-tot-up * v-over-pct / 100).

            v-prt-sht = v-sht-qty / ef.n-out-l.

            FIND FIRST bf-item  NO-LOCK 
                where bf-item.company eq cocode
                and bf-item.i-no    eq wrk-sheet.i-no NO-ERROR .
                
                                               
            PUT "<FGCOLOR=GREEN>NAME                <C18>QTY SHEETS  <C28>SHEET SIZE        <C45>MIN SHEET SIZE       <C62.3>PO#  <C66>STOCK DUE <C75>VENDOR  <FGCOLOR=BLACK>"  SKIP                
                "<C1>" ( IF AVAIL bf-item THEN string(bf-item.i-NAME) ELSE "" ) FORM "x(20)" 
                "<C18>" v-sht-qty FORM "->,>>>,>>9" SPACE(2)
                "<C28>" v-size[1] FORM "X(18)" 
                "<C45>" v-size[2] FORM "X(16)" 
                "<C60>" v-board-po FORM ">>>>>9"
                "<C66>" v-po-duedate FORM "99/99/9999" 
                "<C75>" v-vend FORMAT "x(10)" SKIP(1).

            x = 1.
            DELETE wrk-sheet .
        END. /* each wrk-sheet */            
        PUT  v-fill SKIP.
                
        ASSIGN
            x      = 2
            i      = 1
            v-ink1 = ""
            v-ink2 = "".

        FOR EACH wrk-ink WHERE wrk-ink.form-no = eb.form-no
            AND wrk-ink.Iform EQ 0
            BREAK BY wrk-ink.i-code
            BY wrk-ink.i-pass
            BY wrk-ink.i-unit
            :
             IF NOT FIRST-OF(wrk-ink.i-code) THEN 
                 DELETE wrk-ink.
             ELSE IF wrk-ink.i-unit = 0 THEN wrk-ink.i-unit = 999 .
        END. /* each wrk-ink */
             
        FOR EACH wrk-ink WHERE wrk-ink.form-no = eb.form-no
            AND wrk-ink.Iform EQ 0
            BREAK BY wrk-ink.i-pass
            BY wrk-ink.i-unit
            :
              IF wrk-ink.i-unit = 999 THEN wrk-ink.i-unit = 0 .
              ASSIGN v-ink1[i] = STRING(wrk-ink.form-no,">9") + "  " + /*"1  " + */
                  STRING(wrk-ink.i-code,"X(11)") + " " + 
                  string(wrk-ink.i-dscr,"x(30)") + " " + trim(string(wrk-ink.i-per,">>>>%")) + " " + STRING(wrk-ink.i-unit,">>>") + " " + STRING(wrk-ink.side)
                  /*v-item[i]*/
                  /*+ (IF i = 1 THEN "  " + eb.plate-no ELSE "") */
                  i         = i + 1         . 
            DELETE wrk-ink.
        END. /* each wrk-ink */
        ASSIGN
            v-skip          = NO
            v-plate-printed = NO.
        iCount = 1 .    
        PUT "<R-1><FGCOLOR=GREEN>INKS: <FGCOLOR=BLACK>" string(eb.i-col) FORMAT "x(3)" "<FGCOLOR=GREEN>PASSES: <FGCOLOR=BLACK>" string(eb.i-pass) FORMAT "x(3)" "<FGCOLOR=GREEN>COATS: <FGCOLOR=BLACK>"  string(eb.i-coat) FORMAT "x(3)"
             "<FGCOLOR=GREEN>PASSES: <FGCOLOR=BLACK>"  string(eb.i-coat-p) FORMAT "x(3)"   SKIP
            "<FGCOLOR=GREEN>INK DESCRIPTION:<FGCOLOR=BLACK> "   eb.i-coldscr FORMAT "x(35)" SKIP.
        
        PUT "<P9><#5>" "<FGCOLOR=GREEN> F  COLORS      DESCRIPTION <C36.2>Per<C40.7>U <C42.5>S <FGCOLOR=BLACK>" SKIP. 
        DO j = 1 TO 12:
            IF TRIM(v-ink1[j]) = "-" THEN v-ink1[j] = "".               
            IF v-ink1[j] <> "" THEN do:
                PUT v-ink1[j] FORM "x(59)" SKIP .
                iCount = iCount + 1.
            END.
        END.
       
        PUT SKIP(12 - iCount ) "<R-14>" .
        PUT "<P10><C44><From><R+14><C44><Line><||6><R-14>" .
        v-skip = NO.
             
        FIND FIRST prep NO-LOCK
            WHERE prep.company EQ cocode
              AND prep.CODE EQ eb.die-no AND eb.die-no NE "" NO-ERROR .
        PUT  SKIP(1)
            "<FGCOLOR=GREEN><C44> CAD#:<FGCOLOR=BLACK> " eb.cad-no FORM "x(25)"  SKIP
            "<FGCOLOR=GREEN><C44> PLATES:<FGCOLOR=BLACK> " eb.plate-no FORM "x(25)" /*eb.plate-no*/ SKIP
            "<FGCOLOR=GREEN><C44> NOTES/COMMENTS:<FGCOLOR=BLACK> "  SKIP(2)                 
            "<FGCOLOR=GREEN><C44> DIE<FGCOLOR=BLACK> " eb.die-no FORM "x(25)"  SKIP
            "<FGCOLOR=GREEN><C44> DIE DESCR:<FGCOLOR=BLACK> " (IF AVAIL prep THEN prep.dscr ELSE "")  FORMAT "x(35)"   SKIP
            "<FGCOLOR=GREEN><C44> DIE SIZE:<FGCOLOR=BLACK> " string(ef.trim-w) + "x" + string(ef.trim-l) FORMAT "x(25)" SKIP
            "<FGCOLOR=GREEN><C44> NOTES/COMMENTS:<FGCOLOR=BLACK> "  SKIP(4)                
            "<R-1>" v-fill  SKIP.
        PUT "<C2>" v-cust-name FORMAT "x(30)" SKIP .     
        PUT "<C2><FGCOLOR=GREEN>---------------------------------------- LABEL INFO ----------------------------------------<FGCOLOR=BLACK>"  SKIP.
        /* V-FILL78  SKIP. */
           
        FOR EACH bf-eb WHERE bf-eb.company = eb.company
            AND bf-eb.est-no = eb.est-no
            AND bf-eb.form-no = eb.form-no 
            AND bf-eb.blank-no = eb.blank-no 
            NO-LOCK BY bf-eb.blank-no:

            FIND FIRST style WHERE style.company EQ eb.company
                AND style.style   EQ bf-eb.style NO-LOCK NO-ERROR.
            IF AVAILABLE style THEN v-stypart = style.dscr.
            
            FIND FIRST xjob-hdr WHERE xjob-hdr.company EQ cocode
                AND xjob-hdr.job     EQ job-hdr.job
                AND xjob-hdr.job-no  EQ job-hdr.job-no
                AND xjob-hdr.job-no2 EQ job-hdr.job-no2
                AND xjob-hdr.frm EQ bf-eb.form-no
                AND xjob-hdr.blank-no EQ bf-eb.blank-no
                AND xjob-hdr.i-no EQ bf-eb.stock-no NO-LOCK NO-ERROR.                
               
            iOrderNo =  IF AVAIL xjob-hdr THEN xjob-hdr.ord-no ELSE job-hdr.ord-no .
            cCustNo =  IF AVAIL xjob-hdr THEN xjob-hdr.cust-no ELSE job-hdr.cust-no .

            FIND FIRST oe-ordl WHERE oe-ordl.company EQ job-hdr.company
                AND oe-ordl.ord-no  EQ iOrderNo
                AND oe-ordl.job-no  EQ job-hdr.job-no
                AND oe-ordl.job-no2 EQ job-hdr.job-no2
                AND oe-ordl.i-no    EQ bf-eb.stock-no /*job-hdr.i-no*/
                NO-LOCK NO-ERROR.
            v-po-no = IF AVAILABLE oe-ordl THEN oe-ordl.po-no ELSE v-po-no.
            IF AVAILABLE oe-ordl THEN
                FOR EACH oe-rel
                    WHERE oe-rel.company EQ oe-ordl.company
                    AND oe-rel.ord-no  EQ oe-ordl.ord-no
                    AND oe-rel.i-no    EQ oe-ordl.i-no
                    AND oe-rel.line    EQ oe-ordl.line
                    NO-LOCK BY oe-rel.rel-date:
                    FIND FIRST oe-rell WHERE oe-rell.company  EQ oe-rel.company
                        AND oe-rell.r-no  EQ oe-rel.link-no
                        AND oe-rell.ord-no   EQ oe-rel.ord-no
                        AND oe-rell.i-no     EQ oe-rel.i-no
                        AND oe-rell.line     EQ oe-rel.line
                        AND oe-rell.rel-no   EQ oe-rel.rel-no
                        AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
                        AND oe-rell.po-no    EQ oe-rel.po-no NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE oe-rell THEN
                        FIND FIRST oe-rell WHERE oe-rell.company  EQ oe-rel.company
                            AND oe-rell.ord-no   EQ oe-rel.ord-no
                            AND oe-rell.i-no     EQ oe-rel.i-no
                            AND oe-rell.line     EQ oe-rel.line
                            AND oe-rell.rel-no   EQ oe-rel.rel-no
                            AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
                            AND oe-rell.po-no    EQ oe-rel.po-no NO-LOCK NO-ERROR.
                    IF AVAILABLE oe-rell THEN 
                        FIND oe-relh WHERE oe-relh.company = oe-rel.company
                            AND oe-relh.r-no = oe-rell.r-no NO-LOCK NO-ERROR.
                    v-del-date = IF AVAILABLE oe-relh THEN oe-relh.rel-date ELSE oe-rel.rel-date.
                    LEAVE.
                END.

            v-shipto = "".
            IF AVAILABLE oe-ordl THEN
                FIND FIRST oe-rel WHERE oe-rel.company EQ cocode
                    AND oe-rel.ord-no  EQ oe-ordl.ord-no
                    AND oe-rel.i-no    EQ oe-ordl.i-no
                    AND oe-rel.line    EQ oe-ordl.line
                    NO-LOCK NO-ERROR.
            IF AVAILABLE oe-rel THEN 
            DO:
                FIND FIRST shipto WHERE shipto.company EQ cocode
                    AND shipto.cust-no EQ oe-rel.cust-no
                    AND shipto.ship-id EQ oe-rel.ship-id
                    NO-LOCK NO-ERROR.  
                
                IF AVAILABLE shipto THEN
                    ASSIGN v-shipto[1] = shipto.ship-name
                        v-shipto[2] = shipto.ship-addr[1]
                        v-shipto[3] = shipto.ship-addr[2]
                        v-shipto[4] = TRIM(oe-rel.ship-city) + ", " +
                                  oe-rel.ship-state + "  " + oe-rel.ship-zip.          
            END.
            IF AVAILABLE oe-ordl THEN
            FIND FIRST oe-ord WHERE oe-ord.company EQ cocode
                    AND oe-ord.ord-no  EQ oe-ordl.ord-no
                     NO-LOCK NO-ERROR.            

            ASSIGN
                v-dsc[1]  = IF AVAILABLE oe-ordl THEN oe-ordl.i-name ELSE bf-eb.part-dscr1
                v-dsc[2]  = IF AVAILABLE oe-ordl THEN oe-ordl.part-dscr1 ELSE bf-eb.part-dscr2
                v-dsc[3]  = IF AVAILABLE oe-ordl THEN oe-ordl.part-dscr2 ELSE ""
                v-size[2] = bf-eb.i-coldscr
                v-up      = bf-eb.num-up
                v-job-qty = IF AVAILABLE xjob-hdr THEN xjob-hdr.qty ELSE bf-eb.bl-qty
                ld-len    = bf-eb.len
                ld-wid    = bf-eb.wid
                ld-dep    = bf-eb.dep.

            IF ld-metric NE 1 THEN 
            DO:
                ASSIGN
                    ld-len = ld-len * ld-metric
                    ld-wid = ld-wid * ld-metric
                    ld-dep = ld-dep * ld-metric.

                {sys/inc/roundup.i ld-len}
                {sys/inc/roundup.i ld-wid}
                {sys/inc/roundup.i ld-dep}

                ASSIGN
                    vs-len = STRING(ld-len,"->>,>>>mm")
                    vs-wid = STRING(ld-wid,"->>,>>>mm")
                    vs-dep = STRING(ld-dep,"->>,>>>mm").
            END.

            ELSE 
            DO:
                RUN sys/inc/dec-frac.p (ld-len, 64, OUTPUT vs-len).
                RUN sys/inc/dec-frac.p (ld-wid, 64, OUTPUT vs-wid).
                RUN sys/inc/dec-frac.p (ld-dep, 64, OUTPUT vs-dep).

                IF SUBSTR(vs-len,1,1) = "-" THEN vs-len = SUBSTR(vs-len,2).
                IF SUBSTR(vs-wid,1,1) = "-" THEN vs-wid = SUBSTR(vs-wid,2).
                IF SUBSTR(vs-dep,1,1) = "-" THEN vs-dep = SUBSTR(vs-dep,2).
            END.

            v-size[1] = TRIM(vs-len) + " x " + TRIM(vs-wid) + " x " + TRIM(vs-dep).                

            FIND FIRST tt-key2
                WHERE tt-key2.tt-job-no  EQ job-hdr.job-no
                AND tt-key2.tt-job-no2 EQ job-hdr.job-no2
                AND tt-key2.tt-frm     EQ int(tt-reftable.val[12])
                AND tt-key2.tt-i-no    EQ bf-eb.stock-no
                AND tt-key2.tt-blank   EQ bf-eb.blank-no
                NO-LOCK NO-ERROR.                 
            IF NOT AVAILABLE tt-key2 THEN CREATE tt-key2.                                       
            V-PRT-UP = v-up * ef.n-out-l.
            
            IF PAGE-SIZE - LINE-COUNTER LE 12 THEN PAGE. 
            PUT 
              "<C3><FGCOLOR=GREEN>SHIP TO: "  "<C40><FGCOLOR=GREEN>CUST PO: <FGCOLOR=BLACK>" (IF AVAIL oe-ordl THEN oe-ordl.po-no ELSE "") FORMAT "x(15)" "<C59><FGCOLOR=GREEN>  CASE WEIGHT: <FGCOLOR=BLACK>" string(bf-eb.cas-wt) FORMAT "x(10)"   SKIP
              "<C7>" v-shipto[1] "<C43><FGCOLOR=GREEN> QTY: <FGCOLOR=BLACK>" IF AVAIL xjob-hdr THEN string(xjob-hdr.qty) ELSE "" SKIP
              "<C7>" v-shipto[2] "<C42><FGCOLOR=GREEN> PACK: <FGCOLOR=BLACK>" string(bf-eb.cas-cnt)   "<C61><FGCOLOR=GREEN>  PER CASE: <FGCOLOR=BLACK>" bf-eb.cas-no FORMAT "x(15)" SKIP
              "<C7>" v-shipto[3] "<C42><FGCOLOR=GREEN> PACK: <FGCOLOR=BLACK>" string(bf-eb.cas-pal)   "<C60><FGCOLOR=GREEN> PER PALLET: <FGCOLOR=BLACK>" bf-eb.tr-no FORMAT "x(15)" SKIP
              "<C7>" v-shipto[4]  SKIP
              "<C3><FGCOLOR=GREEN>PART#: <FGCOLOR=BLACK>" bf-eb.part-no FORMAT "x(15)" SKIP
              "<C4><FGCOLOR=GREEN>DESC: <FGCOLOR=BLACK>" bf-eb.part-dscr1 FORMAT "x(30)" SKIP
              "<C8> " bf-eb.part-dscr2 FORMAT "x(30)" SKIP .

            
            PUT "<FGCOLOR=GREEN> " v-fill2 SKIP.
                
            IF AVAILABLE tt-key2 THEN DELETE tt-key2.
        END.    /* for each bf-eb*/
               
        IF AVAILABLE tt-keyst THEN DELETE tt-keyst.
   /* END. /* last-of(eb.form-no) */*/
          
END. /* each eb */
END. /* each ef */
END. /* first job-no */     

/** PRINT MULT COPIES OF TICKETS **/
save_id = RECID(job-hdr).
IF LAST-OF(job-hdr.job-no2) THEN 
DO:
    FOR EACH wrk-op:
        DELETE wrk-op.
    END.
    FOR EACH wrk-prep:
        DELETE wrk-prep.
    END.
END.

FOR EACH wrk-spec:
    DELETE wrk-spec.
END.
FOR EACH wrk-film:
    DELETE wrk-film.
END.
FOR EACH wrk-die:
    DELETE wrk-die.
END.
FOR EACH wrk-sheet:
    DELETE wrk-sheet.
END.
FOR EACH wrk-misc:
    DELETE wrk-misc.
END.
FOR EACH wrk-inst:
    DELETE wrk-inst.
END.
      
v-first = NO.
END. /* each job-hdr */



PROCEDURE pPrintHeader :
    /*------------------------------------------------------------------------------
      Purpose:     Print header
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiType AS INTEGER NO-UNDO .
    IF ipiType EQ 0 THEN
         cBarCodeVal = job-hdr.job-no + "-" + STRING(job-hdr.job-no2,"99") + "-" + STRING(eb.form-no,"99") + "-" + STRING(eb.blank-no,"99") .
    IF ipiType EQ 1 THEN
         cBarCodeVal = job-hdr.job-no + "-" + STRING(job-hdr.job-no2,"99")  .
         PUT 
            "<FGCOLOR=GREEN><LINECOLOR=GREEN><R5><C58.5><From><R11><C58.5><Line><||6>"
            "<=1>" .

    PUT
    "<C1><R1><#1><R+3.5><C+25><IMAGE#1=" ls-full-img1  
    "<R1.5><C27><P18><FGCOLOR=GREEN>Page <FGCOLOR=BLACK><C33>" string(PAGE-NUMBER - v-pg-num,">>") "<C35>  of <#PAGES> <P10>"      
    "<C+4><R1.5><FROM><C80><R3.9><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE=" + cBarCodeVal + ">" FORMAT "x(150)" SKIP
    v-fill .

END PROCEDURE.

/* end ---------------------------------- copr. 1994  advanced software, inc. */
