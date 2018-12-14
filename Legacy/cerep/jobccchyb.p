/* cerep/jobcarded.p   Xprint FC Factory  Ticket for CCC-Hybrid */
/* -------------------------------------------------------------------------- */
DEFINE INPUT PARAMETER ip-job-no  LIKE job-hdr.job-no  NO-UNDO.
DEFINE INPUT PARAMETER ip-job-no2 LIKE job-hdr.job-no2 NO-UNDO.

DEFINE VARIABLE cSide AS CHARACTER NO-UNDO.
{sys/inc/var.i shared}
{sys/form/s-top.f}

{jcrep/r-ticket.i "shared"}
{cerep\tt-samp-ctn.i}

DEFINE NEW SHARED VARIABLE save_id                  AS RECID.
DEFINE NEW SHARED VARIABLE v-today                  AS DATE      INITIAL TODAY.
DEFINE NEW SHARED VARIABLE v-job                    AS CHARACTER FORMAT "x(6)" EXTENT 2 INITIAL [" ","zzzzzz"].
DEFINE NEW SHARED VARIABLE v-job2                   AS INTEGER   FORMAT "99" EXTENT 2 INITIAL [00,99].
DEFINE NEW SHARED VARIABLE v-stypart                LIKE style.dscr.
DEFINE NEW SHARED VARIABLE v-dsc                    LIKE oe-ordl.part-dscr1 EXTENT 2.
DEFINE NEW SHARED VARIABLE v-size                   AS CHARACTER FORMAT "x(26)" EXTENT 2.
DEFINE NEW SHARED VARIABLE v-bld-job                LIKE oe-ord.job-no.
DEFINE NEW SHARED VARIABLE v-bld-job2               LIKE oe-ord.job-no2.
DEFINE NEW SHARED VARIABLE v-fill                   AS CHARACTER FORMAT "x(128)".
DEFINE NEW SHARED VARIABLE v-frst                   AS LOGICAL.
DEFINE NEW SHARED VARIABLE v-ok                     AS LOGICAL.
DEFINE NEW SHARED VARIABLE v-est-qty                AS INTEGER   FORMAT "->>,>>>,>>9".
DEFINE NEW SHARED VARIABLE v-job-qty                AS INTEGER   FORMAT "->>,>>>,>>9".
DEFINE NEW SHARED VARIABLE v-fac                    AS DECIMAL .
DEFINE NEW SHARED VARIABLE v-job-no                 LIKE oe-ordl.job-no.
DEFINE NEW SHARED VARIABLE v-job-no2                LIKE oe-ordl.job-no2.
DEFINE NEW SHARED VARIABLE v-due-date               LIKE oe-ord.due-date.
DEFINE NEW SHARED VARIABLE v-reprint                AS LOGICAL.
DEFINE NEW SHARED VARIABLE v-up                     LIKE eb.num-up.
DEFINE NEW SHARED VARIABLE v-tandem                 AS LOGICAL.
DEFINE NEW SHARED VARIABLE v-form-no                LIKE eb.form-no.
DEFINE NEW SHARED VARIABLE v-fup                    AS CHARACTER.
DEFINE NEW SHARED VARIABLE v-layout                 AS CHARACTER FORMAT "x(30)".
DEFINE            VARIABLE v-case-count             LIKE eb.cas-cnt NO-UNDO.
DEFINE            VARIABLE v-case-qty               AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-spc-no                 LIKE eb.spc-no NO-UNDO.
DEFINE            VARIABLE v-gsh-qty                AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-frm-blk                AS CHARACTER FORMAT "x(6)" NO-UNDO.
DEFINE            VARIABLE v-dec                    AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-ovund                  AS CHARACTER FORMAT "x(34)" NO-UNDO.
DEFINE            VARIABLE v-mrhr                   AS CHARACTER FORMAT "x(5)".
DEFINE            VARIABLE v-cas-dscr               LIKE item.est-dscr.
DEFINE            VARIABLE v-first                  AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE v-spec-list              AS CHARACTER FORMAT "x(20)"INITIAL "QA" NO-UNDO.
DEFINE            VARIABLE lv-form-note             AS CHARACTER NO-UNDO.
DEFINE            VARIABLE v-itm-printed            AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-prep                   AS CHARACTER EXTENT 8 NO-UNDO.
DEFINE            VARIABLE v-misc                   AS CHARACTER EXTENT 6 NO-UNDO.
DEFINE            VARIABLE v-spec-no                AS CHARACTER EXTENT 8 NO-UNDO.
DEFINE            VARIABLE v-skip                   AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE v-fill2                  AS CHARACTER INITIAL "-" FORM "x(125)" NO-UNDO.
DEFINE            VARIABLE lv-text                  AS CHARACTER NO-UNDO.
DEFINE            VARIABLE li                       AS INTEGER   NO-UNDO.
DEFINE            VARIABLE lv-under-run             AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lv-over-run              AS CHARACTER NO-UNDO.
DEFINE            VARIABLE v-cust-name-extent       AS CHARACTER EXTENT 10 NO-UNDO.
DEFINE            VARIABLE v-ship1-extent           AS CHARACTER EXTENT 10 NO-UNDO.
DEFINE            VARIABLE v-ship2-extent           AS CHARACTER EXTENT 10 NO-UNDO.
DEFINE            VARIABLE v-ship4-extent           AS CHARACTER EXTENT 10 NO-UNDO.
DEFINE            VARIABLE v-po-no                  LIKE oe-ordl.po-no EXTENT 4 NO-UNDO.
DEFINE            VARIABLE v-unit-per-int           LIKE eb.cas-cnt NO-UNDO.
DEFINE            VARIABLE v-unit-per-dec           AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-job-qty-unit-per-int   AS INTEGER   FORMAT ">>>>>9" NO-UNDO.
DEFINE            VARIABLE v-job-qty-unit-per-dec   AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-dc-gl-speed            AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-job-qty-boxes-code-int AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-job-qty-boxes-code-dec AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-dc-out                 LIKE est-op.n-out NO-UNDO.
DEFINE            VARIABLE v-dc-only-out            LIKE est-op.n-out NO-UNDO.
DEFINE            VARIABLE v-shink-wrap             AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE v-sample-on-cnt          AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE v-shrink-wrap            AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE v-cas-wt                 AS DECIMAL   FORMAT ">>>>9.99" NO-UNDO.
DEFINE            VARIABLE v-cust-lot#              AS CHARACTER EXTENT 4 NO-UNDO.
DEFINE            VARIABLE v-per-ord                AS CHARACTER NO-UNDO.
DEFINE            VARIABLE v-upc-no                 LIKE eb.upc-no NO-UNDO.
DEFINE            VARIABLE v-pricnt-id              AS CHARACTER NO-UNDO .

DEFINE            VARIABLE ddivider                 AS DECIMAL   NO-UNDO .
DEFINE            VARIABLE lv-rowid                 AS ROWID     NO-UNDO.

DEFINE BUFFER b-est     FOR est.
DEFINE BUFFER b-oe-ordl FOR oe-ordl.
DEFINE BUFFER b-oe-rel  FOR oe-rel.
DEFINE BUFFER b-shipto  FOR shipto.
DEFINE BUFFER b-cust    FOR cust.
DEFINE BUFFER b-rt      FOR reftable.
DEFINE BUFFER ref-side  FOR reftable.

DEFINE TEMP-TABLE w-lo NO-UNDO
    FIELD layout LIKE v-layout.

DEFINE NEW SHARED BUFFER xjob-hdr FOR job-hdr.

DEFINE            BUFFER b-eb     FOR eb.

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
    FIELD spoil LIKE job-mch.wst-prct EXTENT 20
    FIELD mr-waste LIKE job-mch.mr-waste EXTENT 20    .

DEFINE NEW SHARED WORKFILE wrk-die
    FIELD die-no LIKE eb.die-no
    FIELD cad-no LIKE eb.cad-no
    FIELD form-no LIKE eb.form-no
    FIELD die-size AS CHARACTER FORMAT "x(17)".

DEFINE NEW SHARED WORKFILE wrk-sheet
    FIELD gsh-qty AS INTEGER  FORMAT "->>>,>>>,>>9" /* gdm - 12180809*/
    FIELD cal LIKE ef.cal
    FIELD i-no LIKE ITEM.i-no
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
    FIELD i-unit AS INTEGER
    FIELD i-side AS CHARACTER.

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
  
{custom/formtext.i NEW}     
{custom/notesdef.i}
DEFINE VARIABLE v-inst2          AS CHARACTER EXTENT 25 NO-UNDO.    
DEFINE VARIABLE v-dept-inst      AS CHARACTER FORMAT "x(80)" EXTENT 20 NO-UNDO.
DEFINE VARIABLE v-note-length    AS INTEGER   INITIAL 80 NO-UNDO.

DEFINE VARIABLE v-start-date     AS DATE      NO-UNDO.
DEFINE VARIABLE v-req-date       AS DATE      NO-UNDO.
DEFINE VARIABLE v-shipto         AS CHARACTER FORMAT "x(30)" EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-case-size      AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-vend           LIKE po-ord.vend-no NO-UNDO.
DEFINE VARIABLE v-item           AS CHARACTER EXTENT 100 NO-UNDO.
DEFINE VARIABLE v-i-qty          AS DECIMAL   EXTENT 100 NO-UNDO.
DEFINE VARIABLE v-ink1           AS CHARACTER EXTENT 100 NO-UNDO.
DEFINE VARIABLE v-ink2           AS CHARACTER EXTENT 100 NO-UNDO.
DEFINE VARIABLE lv-mat-dept-list AS CHARACTER INITIAL "FB,FS,WN,WS,GL" NO-UNDO.
DEFINE VARIABLE v-mat-for-mach   AS CHARACTER NO-UNDO.
DEFINE BUFFER xjob-mat   FOR job-mat. 
DEFINE BUFFER bf-job-mat FOR job-mat. 
DEFINE VARIABLE v-layer-qty    AS DECIMAL   NO-UNDO .
DEFINE VARIABLE v-cases-qty    AS DECIMAL   NO-UNDO .
DEFINE VARIABLE v-fgitm        AS CHARACTER FORMAT "x(15)" EXTENT 10 NO-UNDO.
DEFINE VARIABLE v-fgdsc        LIKE eb.part-dscr1 EXTENT 10 NO-UNDO.
DEFINE VARIABLE v-fgqty        LIKE job-hdr.qty EXTENT 10 FORM ">>,>>>,>>>" NO-UNDO.
DEFINE VARIABLE v-pono         LIKE oe-ordl.po-no EXTENT 10 NO-UNDO.
DEFINE VARIABLE v-num-of-fgitm AS INTEGER   NO-UNDO.
DEFINE TEMP-TABLE tt-fgitm 
    FIELD i-no      AS CHARACTER FORMAT "x(15)"
    FIELD seq       AS INTEGER
    FIELD qty       AS INTEGER 
    FIELD i-dscr    AS CHARACTER
    FIELD po-no     AS CHARACTER
    FIELD cust-name AS CHARACTER
    FIELD shipto1   AS CHARACTER
    FIELD shipto2   AS CHARACTER
    FIELD shipto4   AS CHARACTER.
DEFINE VARIABLE v-board-po      LIKE oe-ordl.po-no-po NO-UNDO.
DEFINE VARIABLE v-plate-printed AS LOGICAL NO-UNDO.
DEFINE BUFFER xoe-ordl FOR oe-ordl.
DEFINE VARIABLE v-cust-name         LIKE oe-ord.cust-name NO-UNDO.
DEFINE VARIABLE v-cust-name2        LIKE oe-ord.cust-name NO-UNDO.
DEFINE VARIABLE v-cust-name3        LIKE oe-ord.cust-name NO-UNDO.
DEFINE VARIABLE v-last-j            AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-po-no2            LIKE v-po-no NO-UNDO.
DEFINE VARIABLE v-po-no3            LIKE v-po-no NO-UNDO.
DEFINE VARIABLE v-lbs               AS DECIMAL   FORM ">>,>>>,>>9" NO-UNDO.
DEFINE VARIABLE v-dept-title        AS cha       NO-UNDO.
DEFINE VARIABLE v-dept-note-printed AS LOGICAL.
/* aj */
DEFINE VARIABLE v-ship-date         AS DATE      EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-due-qty           LIKE oe-rel.tot-qty EXTENT 4 NO-UNDO.
DEFINE VARIABLE icount              AS INTEGER   INITIAL 0 NO-UNDO.
DEFINE VARIABLE v-max-qty           AS INTEGER   NO-UNDO .
DEFINE VARIABLE v-min-qty           AS INTEGER   NO-UNDO .
DEFINE VARIABLE v-reprun            AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-brd-code          AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-item-desc         AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-weight            LIKE job-mat.rm-i-no NO-UNDO.
DEFINE VARIABLE v-width             LIKE job-mat.basis-w NO-UNDO.
DEFINE VARIABLE v-lenght            AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-print-qty         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-print-feet        AS DECIMAL   FORMAT "->>>,>>>,>>9.99" NO-UNDO.

v-fill = "<||3><C2><FROM><C108><LINE><||3>".

DEFINE NEW SHARED FRAME head.

DEFINE SHARED VARIABLE s-prt-mstandard AS LOGICAL   NO-UNDO.
DEFINE SHARED VARIABLE s-prt-shipto    AS LOGICAL   NO-UNDO.
DEFINE SHARED VARIABLE s-prt-sellprc   AS LOGICAL   NO-UNDO.
DEFINE        VARIABLE v-po-duedate    LIKE po-ordl.due-date NO-UNDO.
DEFINE        VARIABLE v-upc-lbl       AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE        VARIABLE v-shipto1       AS CHARACTER FORMAT "x(30)" EXTENT 4 NO-UNDO.
DEFINE        VARIABLE v-shipto2       AS CHARACTER FORMAT "x(30)" EXTENT 4 NO-UNDO.
DEFINE SHARED VARIABLE s-run-speed     AS LOGICAL   NO-UNDO.
DEFINE        VARIABLE v-pass-count    AS INTEGER   NO-UNDO.
DEFINE SHARED VARIABLE s-prt-label     AS LOGICAL   NO-UNDO.
DEFINE        VARIABLE v-boardcode     LIKE job-mat.rm-i-no NO-UNDO.  
DEFINE        VARIABLE v-length        LIKE job-mat.len NO-UNDO.
DEFINE        VARIABLE v-upnew         AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE v-lp-dep        AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE v-lp-qty        AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-mr-hours      AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE v-pr-speed      AS INTEGER   NO-UNDO.

DEFINE        VARIABLE cDraftImage     AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cDraftImageFull AS CHARACTER FORMAT "x(50)" NO-UNDO.
DEFINE        VARIABLE cJobNo          AS CHARACTER NO-UNDO.

ASSIGN 
    cDraftImage         = "images\draft.jpg"

    FILE-INFO:FILE-NAME = cDraftImage.
cDraftImageFull = IF lDraft 
    THEN  "<C25><#1><R+80><C+50><IMAGE#1=" + FILE-INFO:FULL-PATHNAME + ">" 
    ELSE "".

FORMAT HEADER 
    cDraftImageFull FORMAT "x(100)" SKIP
    "<R1><C68><FROM><AT=+.3,+1.7><BARCODE,TYPE=39,CHECKSUM=NONE,BarHeightPixels=2,VALUE=" cJobNo FORMAT "x(9)" /*v-job-no space(0) "-" space(0) v-job-no2 format "99"*/ ">"
    "<P12><C2><R2>JOB NUMBER:<B>" v-job-no SPACE(0) "-" SPACE(0) v-job-no2 FORMAT "99" "</B>"      SPACE(1) /* v-reprun   */
    "CSR:" v-pricnt-id
    "<C40><R2><B><P12>F A C T O R Y   T I C K E T</B><P10>" AT 52  
    "START DATE:" AT 128 v-start-date SKIP
    v-fill SKIP
    v-fill
    WITH NO-BOX FRAME head NO-LABELS STREAM-IO WIDTH 155.
    
{sys/inc/notes.i}

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.

ASSIGN
 v-job[1]    = fjob-no
 v-job[2]    = tjob-no
 v-job2[1]   = fjob-no2
 v-job2[2]   = tjob-no2
 v-reprint   = reprint
 v-spec-list = spec-list.

FOR EACH job-hdr NO-LOCK
        WHERE job-hdr.company       EQ cocode
          AND job-hdr.job-no        EQ ip-job-no
          AND job-hdr.job-no2       EQ ip-job-no2
          AND (production OR
               job-hdr.ftick-prnt           EQ v-reprint OR
               PROGRAM-NAME(2) MATCHES "*r-tickt2*")
          AND CAN-FIND(FIRST job WHERE job.company EQ cocode
                                   AND job.job     EQ job-hdr.job
                                   AND job.job-no  EQ job-hdr.job-no
                                   AND job.job-no2 EQ job-hdr.job-no2
                                   /*and job.stat    ne "H"*/
                                   AND (job.pr-printed EQ reprint OR
                                        NOT production))
        USE-INDEX job-no,

        FIRST est NO-LOCK
        WHERE est.company  EQ job-hdr.company
          AND est.est-no   EQ job-hdr.est-no

        BREAK BY job-hdr.job
              BY job-hdr.job-no
              BY job-hdr.job-no2
              BY job-hdr.frm:
      
    FIND FIRST job
        WHERE job.company EQ cocode
        AND job.job     EQ job-hdr.job
        AND job.job-no  EQ job-hdr.job-no
        AND job.job-no2 EQ job-hdr.job-no2
        NO-LOCK NO-ERROR.
    
    IF production THEN 
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
      
    v-est-qty = IF AVAILABLE est THEN est.est-qty[1] ELSE 0.
    FIND FIRST oe-ord NO-LOCK WHERE oe-ord.company EQ job-hdr.company
        AND oe-ord.ord-no  EQ job-hdr.ord-no NO-ERROR.

    IF FIRST-OF(job-hdr.frm) THEN v-first = YES.

    /** PRINT JOB HEADER **/
    IF v-first THEN 
    DO:
        ASSIGN
            v-job-no  = job-hdr.job-no
            v-job-no2 = job-hdr.job-no2
            cJobNo    = v-job-no + "-" + STRING(v-job-no2,"99").

        IF AVAILABLE oe-ord THEN
            IF NOT oe-ctrl.p-fact AND (oe-ord.stat EQ "H" OR oe-ord.priceHold) THEN NEXT.

        FIND FIRST cust NO-LOCK WHERE cust.company EQ job-hdr.company AND
            cust.cust-no EQ job-hdr.cust-no NO-ERROR.

        IF AVAILABLE cust THEN 
        DO:
            ASSIGN 
                v-pricnt-id = "" . 
            FOR EACH empalert NO-LOCK WHERE empalert.table_rec_key = cust.rec_key,
                FIRST users NO-LOCK WHERE users.user_id = empalert.USER-ID:

                IF empalert.spare-char-1 EQ "YES" THEN 
                DO:
                    ASSIGN 
                        v-pricnt-id = users.USER_id .
                    LEAVE.
                END.
            END.
        END.   

        ASSIGN
            v-start-date = job-hdr.start-date.

        IF NOT FIRST(job-hdr.job-no) THEN PAGE.
        
        v-shipto = "".
        FIND FIRST oe-ordl NO-LOCK
            WHERE oe-ordl.company EQ job-hdr.company
            AND oe-ordl.ord-no  EQ job-hdr.ord-no
            AND oe-ordl.job-no  EQ job-hdr.job-no
            AND oe-ordl.job-no2 EQ job-hdr.job-no2
            AND oe-ordl.i-no    EQ job-hdr.i-no
            NO-ERROR.
        IF AVAILABLE oe-ordl THEN 
            FIND FIRST oe-rel NO-LOCK
                WHERE oe-rel.company EQ cocode
                AND oe-rel.ord-no  EQ oe-ordl.ord-no
                AND oe-rel.i-no    EQ oe-ordl.i-no
                AND oe-rel.line    EQ oe-ordl.line
                NO-ERROR.
        ASSIGN 
            v-reprun = IF AVAILABLE oe-ordl AND oe-ordl.type-code = "R" THEN "RETURN" 
                          ELSE "NEW" .
        
        VIEW FRAME head.
        IF AVAILABLE oe-rel THEN 
        DO:
            v-po-no = oe-rel.po-no .

            ASSIGN 
                v-cust-lot# = oe-rel.lot-no.

            FIND FIRST shipto NO-LOCK
                WHERE shipto.company EQ cocode
                AND shipto.cust-no EQ oe-rel.cust-no
                AND shipto.ship-id EQ oe-rel.ship-id
                NO-ERROR.  
            IF AVAILABLE shipto THEN
                ASSIGN v-shipto[1] = shipto.ship-name
                    v-shipto[2] = shipto.ship-addr[1]
                    v-shipto[3] = shipto.ship-addr[2]
                    v-shipto[4] = TRIM(oe-rel.ship-city) + ", " +
                                   oe-rel.ship-state + "  " + oe-rel.ship-zip.  

        END.
        icount = 0.
        FOR EACH b-oe-rel NO-LOCK WHERE  b-oe-rel.company EQ cocode
            AND b-oe-rel.ord-no  EQ oe-rel.ord-no
            AND b-oe-rel.i-no    EQ oe-rel.i-no
            AND b-oe-rel.line    EQ oe-rel.LINE :
           
            icount =  icount + 1 .
            IF icount = 1 THEN  
                ASSIGN 
                    v-ship-date[1] = IF b-oe-rel.rel-date NE ?  THEN  b-oe-rel.rel-date ELSE ? 
                    v-due-qty[1]   = IF b-oe-rel.tot-qty NE 0 THEN b-oe-rel.tot-qty ELSE 0 
                    v-po-no[1]     = IF b-oe-rel.po-no NE "" THEN  b-oe-rel.po-no ELSE ""
                    v-cust-lot#[1] = IF b-oe-rel.lot-no <> "" THEN b-oe-rel.lot-no ELSE "" .
                      
            IF icount = 2 THEN  
                ASSIGN 
                    v-ship-date[2] = IF b-oe-rel.rel-date NE ?  THEN  b-oe-rel.rel-date ELSE ? 
                    v-due-qty[2]   = IF b-oe-rel.tot-qty NE 0 THEN b-oe-rel.tot-qty ELSE 0 
                    v-po-no[2]     = IF b-oe-rel.po-no NE "" THEN  b-oe-rel.po-no ELSE "" 
                    v-cust-lot#[2] = IF b-oe-rel.lot-no <> "" THEN b-oe-rel.lot-no ELSE "" .

            IF icount = 3 THEN  
                ASSIGN 
                    v-ship-date[3] = IF b-oe-rel.rel-date NE ?  THEN  b-oe-rel.rel-date ELSE ? 
                    v-due-qty[3]   = IF b-oe-rel.tot-qty NE 0 THEN b-oe-rel.tot-qty ELSE 0 
                    v-po-no[3]     = IF b-oe-rel.po-no NE "" THEN  b-oe-rel.po-no ELSE ""   
                    v-cust-lot#[3] = IF b-oe-rel.lot-no <> "" THEN b-oe-rel.lot-no ELSE "" .

            IF icount = 4 THEN  
                ASSIGN 
                    v-ship-date[4] = IF b-oe-rel.rel-date NE ?  THEN  b-oe-rel.rel-date ELSE ?  
                    v-due-qty[4]   = IF b-oe-rel.tot-qty NE 0 THEN b-oe-rel.tot-qty ELSE 0 
                    v-po-no[4]     = IF b-oe-rel.po-no NE "" THEN  b-oe-rel.po-no ELSE ""   
                    v-cust-lot#[4] = IF b-oe-rel.lot-no <> "" THEN b-oe-rel.lot-no ELSE "" . 
            
        END. /* FOR EACH */
        
        FIND FIRST cust NO-LOCK WHERE cust.company EQ job-hdr.company AND
            cust.cust-no EQ job-hdr.cust-no  NO-ERROR.

        ASSIGN
            v-req-date   = IF AVAILABLE oe-ordl THEN oe-ordl.req-date ELSE ?
            v-cust-name  = IF AVAILABLE oe-ord THEN oe-ord.cust-name 
                         ELSE IF AVAILABLE cust THEN cust.name
                         ELSE job-hdr.cust-no
            lv-over-run  = IF AVAILABLE oe-ordl THEN TRIM(STRING(oe-ordl.over-pct,">>9.99")) ELSE
                         IF AVAILABLE oe-ord  THEN TRIM(STRING(oe-ord.over-pct,">>9.99"))  ELSE ""
            lv-under-run = IF AVAILABLE oe-ordl THEN TRIM(STRING(oe-ordl.under-pct,">>9.99")) ELSE
                          IF AVAILABLE oe-ord  THEN TRIM(STRING(oe-ord.under-pct,">>9.99"))  ELSE ""
            v-due-date   = IF AVAILABLE oe-ordl THEN oe-ordl.prom-date ELSE ? .
        IF AVAILABLE oe-ord THEN
            v-per-ord   = IF oe-ord.po-no2 NE "" THEN oe-ord.po-no2 ELSE STRING(oe-ord.pord-no) .
        IF AVAILABLE oe-ord AND oe-ord.TYPE EQ "T" AND oe-ord.pord-no GT 0 THEN
            v-per-ord = STRING(oe-ord.pord-no).
      
        FIND FIRST eb NO-LOCK WHERE eb.company     EQ job-hdr.company
            AND eb.est-no      EQ job-hdr.est-no
            AND eb.form-no     EQ job-hdr.frm
            AND eb.stock-no = job-hdr.i-no  NO-ERROR.
        IF NOT AVAILABLE eb THEN FIND FIRST eb NO-LOCK WHERE eb.company     EQ job-hdr.company
                AND eb.est-no      EQ job-hdr.est-no
                AND eb.form-no     EQ job-hdr.frm
                AND eb.blank-no    GT 0 NO-ERROR.
        v-spc-no = IF AVAILABLE eb THEN eb.spc-no ELSE "".
        v-upc-no = IF AVAILABLE eb THEN eb.upc-no ELSE "".

        PUT "<B> Customer Name:</B>" v-cust-name FORM "x(25)" "<B>Acct Code:</B> " job-hdr.cust-no 
            "<B> REL. DATE:    QTY DUE:  PO#:         Customer Lot#:    Print Date:" SKIP
            " Shipto:</B>" v-shipto[1] SPACE(2) "Prev.Ord#:" v-per-ord v-ship-date[1] AT 65 v-due-qty[1] AT 75  v-po-no[1] FORMAT "x(12)" AT 89 v-cust-lot#[1] AT 102 FORM "x(15)" TODAY FORMAT "99/99/9999" AT 120 SKIP  
            v-shipto[2] AT 9 SPACE(2) "MFG DATE:" v-due-date v-ship-date[2] AT 61 v-due-qty[2] AT 71 v-po-no[2] FORMAT "x(12)" AT 85 v-cust-lot#[2] AT 98 FORM "x(15)"  STRING(TIME,"HH:MM am/pm") AT 115 " by " USERID("nosweat")   SKIP  
            v-shipto[3] AT 9 "<B>QC/SPC#</B>:" AT 41 v-spc-no  FORMAT "x(10)" SPACE(2) v-ship-date[3] SPACE(2) 
            v-due-qty[3] SPACE(3) v-po-no[3] FORMAT "x(12)" SPACE(1) v-cust-lot#[3] FORMAT "x(15)" SPACE(3) "<B>Estimate:</B>" /*AT 116*/  SKIP 
            v-shipto[4] AT 9 "Pharma Code:" AT 41 v-upc-no  TRIM(job-hdr.est-no) AT 116 SKIP 
            v-fill SKIP.

        /** SUM UP NUMBER OF SHEETS **/
        FIND FIRST job NO-LOCK
            WHERE job.company EQ cocode
            AND job.job     EQ job-hdr.job
            AND job.job-no  EQ v-job-no
            AND job.job-no2 EQ v-job-no2
            NO-ERROR.
            
        IF AVAILABLE job THEN
            FOR EACH job-mch NO-LOCK
                WHERE job-mch.company EQ cocode
                AND job-mch.job     EQ job.job
                AND job-mch.job-no  EQ job.job-no
                AND job-mch.job-no2 EQ job.job-no2
                AND job-mch.frm     EQ job-hdr.frm ,

                FIRST mach
                {sys/ref/machW.i}
              AND mach.m-code EQ job-mch.m-code
            NO-LOCK

            BY mach.d-seq
            BY job-mch.frm
            BY job-mch.blank-no
            BY job-mch.pass
            BY job-mch.run-qty DESC:

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
            wrk-op.run-hr[job-mch.frm]   = job-mch.run-hr    .
    END.

    /** BUILD PREP WORK FILE **/
    FOR EACH job-prep NO-LOCK
        WHERE job-prep.company EQ cocode
        AND job-prep.job     EQ job-hdr.job
        AND job-prep.job-no  EQ job-hdr.job-no
        AND job-prep.job-no2 EQ job-hdr.job-no2:
        FIND FIRST prep NO-LOCK
            WHERE prep.company EQ cocode
            AND prep.code    EQ job-prep.code
            NO-ERROR.
        CREATE wrk-prep.
        ASSIGN
            wrk-prep.code  = job-prep.code
            wrk-prep.dscr  = IF AVAILABLE prep THEN prep.dscr ELSE ""
            wrk-prep.s-num = job-prep.frm
            wrk-prep.b-num = job-prep.blank-no
            wrk-prep.ml    = job-prep.ml.
    END. /* each job-prep */

    IF AVAILABLE est THEN
        FOR EACH est-prep NO-LOCK
            WHERE est-prep.company EQ est.company
            AND est-prep.est-no  EQ est.est-no
            AND index("SON",est-prep.simon) GT 0 :
            FIND FIRST prep NO-LOCK
                WHERE prep.company EQ cocode
                AND prep.code    EQ est-prep.code
                NO-ERROR.
            CREATE wrk-prep.
            ASSIGN
                wrk-prep.code  = est-prep.code
                wrk-prep.dscr  = IF AVAILABLE prep THEN prep.dscr ELSE ""
                wrk-prep.s-num = est-prep.s-num
                wrk-prep.b-num = est-prep.b-num
                wrk-prep.ml    = est-prep.ml.
        END.

    IF AVAILABLE oe-ord THEN
        FOR EACH oe-ordm NO-LOCK 
            WHERE oe-ordm.company EQ cocode
            AND oe-ordm.ord-no  EQ oe-ord.ord-no :
            FIND FIRST wrk-prep WHERE wrk-prep.code EQ oe-ordm.charge NO-ERROR.
            IF NOT AVAILABLE wrk-prep THEN 
            DO:
                FIND FIRST prep NO-LOCK
                    WHERE prep.company EQ cocode
                    AND prep.code    EQ oe-ordm.charge
                    NO-ERROR.
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
        AND ef.form-no EQ job-hdr.frm
        BREAK BY ef.est-no BY ef.form-no:
        v-job-qty = 0.
        FOR EACH xjob-hdr FIELDS(qty) NO-LOCK
            WHERE xjob-hdr.company EQ cocode
            AND xjob-hdr.job     EQ job-hdr.job
            AND xjob-hdr.job-no  EQ job-hdr.job-no
            AND xjob-hdr.job-no2 EQ job-hdr.job-no2
            AND xjob-hdr.i-no    EQ job-hdr.i-no :
            v-job-qty = v-job-qty + xjob-hdr.qty.
        END.
          
        v-est-qty = 0.
        IF est.est-type EQ 4 THEN
            FOR EACH eb NO-LOCK
                WHERE eb.company  EQ ef.company
                AND eb.est-no   EQ ef.est-no
                AND eb.stock-no EQ job-hdr.i-no:
                v-est-qty = v-est-qty + eb.yld-qty.
            END.

        ELSE v-fac = 1.
        v-itm-printed = 0.

        IF ef.form-no EQ job-hdr.frm THEN 
            ebloop:
            FOR EACH eb NO-LOCK
                WHERE eb.company     EQ ef.company
                AND eb.est-no      EQ ef.est-no
                AND eb.form-no     EQ ef.form-no
                
                BREAK BY eb.form-no BY eb.blank-no.

                CREATE w-lo.
                FOR EACH b-eb NO-LOCK
                    WHERE b-eb.company EQ eb.company
                    AND b-eb.est-no  EQ eb.est-no
                    AND b-eb.part-no EQ eb.part-no
                    BREAK BY b-eb.est-no:
                    v-fup = "F" + TRIM(STRING(b-eb.form-no,">>9")) + "-" +
                        TRIM(STRING(b-eb.blank-no,"99")) + "/" +
                        TRIM(STRING(b-eb.num-up,">>9")) + "up".
                    IF LENGTH(TRIM(v-fup)) + LENGTH(TRIM(w-lo.layout)) GT 30 THEN 
                    DO:
                        SUBSTRING(w-lo.layout,LENGTH(TRIM(w-lo.layout)),1) = "".
                        CREATE w-lo.
                    END.
                    w-lo.layout = TRIM(w-lo.layout + " " + trim(v-fup) + ",").
                    IF LAST(b-eb.est-no) THEN
                        SUBSTRING(w-lo.layout,LENGTH(TRIM(w-lo.layout)),1) = "".
                END.
          
                FIND FIRST wrk-die WHERE wrk-die.die-no EQ eb.die-no NO-ERROR.
                IF NOT AVAILABLE wrk-die AND eb.die-no GT "" THEN 
                DO:
                    CREATE wrk-die.
                    ASSIGN 
                        wrk-die.die-no   = eb.die-no
                        wrk-die.cad-no   = eb.cad-no
                        wrk-die.form-no  = eb.form-no
                        wrk-die.die-size = STRING(ef.trim-w) + "x" +
              STRING(ef.trim-l).
                END.

                /** BUILD INK WORK FILE **/
                FIND FIRST reftable NO-LOCK WHERE 
                    reftable.reftable EQ "ce/v-est3.w Unit#" AND
                    reftable.company EQ eb.company AND
                    reftable.loc     EQ eb.est-no AND
                    reftable.code    EQ STRING(eb.form-no,"9999999999") AND
                    reftable.code2   EQ STRING(eb.blank-no,"9999999999")
                    NO-ERROR.

                FIND FIRST b-rt NO-LOCK WHERE
                    b-rt.reftable EQ "ce/v-est3.w Unit#1" AND
                    b-rt.company  EQ b-eb.company AND
                    b-rt.loc      EQ eb.est-no AND
                    b-rt.code     EQ STRING(eb.form-no,"9999999999") AND
                    b-rt.code2    EQ STRING(eb.blank-no,"9999999999")
                    NO-ERROR.

                FOR EACH job-mat NO-LOCK
                    WHERE job-mat.company EQ cocode
                    AND job-mat.job     EQ job-hdr.job
                    AND job-mat.frm     EQ eb.form-no,
                    FIRST item
                    {sys/look/itemivW.i}
                      AND item.i-no EQ job-mat.i-no
                      NO-LOCK:

                DO i = 1 TO 20:
                    IF eb.i-code2[i] EQ job-mat.i-no THEN 
                    DO:

                        cSide = "".
                        IF AVAIL(reftable) THEN
                            cSide = FILL(" ",5) + SUBSTRING(reftable.dscr,i,1).
                        FIND FIRST wrk-ink WHERE wrk-ink.i-code EQ eb.i-code2[i]
                            AND wrk-ink.form-no  EQ eb.form-no
                            AND wrk-ink.blank-no EQ eb.blank-no
                            AND (wrk-ink.i-side EQ cSide OR cSide EQ "")
                            NO-ERROR.
                        IF NOT AVAILABLE wrk-ink THEN 
                        DO:
                  
                            CREATE wrk-ink.
                            ASSIGN
                                wrk-ink.i-code   = eb.i-code2[i]
                                wrk-ink.form-no  = eb.form-no
                                wrk-ink.blank-no = eb.blank-no
                                wrk-ink.i-dscr   = eb.i-dscr2[i]
                                wrk-ink.i-pass   = eb.i-ps2[i]
                                wrk-ink.i-unit   = IF i LE 12 AND AVAILABLE reftable THEN reftable.val[i]
                                        ELSE IF i > 12 AND AVAILABLE b-rt THEN b-rt.val[i - 12]
                                        ELSE 1.
                            
                            IF i LE 12 THEN 
                            DO:
                                FIND FIRST ref-side NO-LOCK WHERE
                                    ref-side.reftable EQ "ce/v-est3.w Unit#"  AND
                                    ref-side.company  EQ eb.company AND
                                    ref-side.loc      EQ eb.est-no AND
                                    ref-side.code     EQ STRING(eb.form-no,"9999999999") AND
                                    ref-side.code2    EQ STRING(eb.blank-no,"9999999999")
                                    NO-ERROR.
                                IF AVAILABLE ref-side THEN
                                    wrk-ink.i-side = FILL(" ",5) + SUBSTRING(ref-side.dscr,i,1).
                            END.
                            ELSE 
                            DO:
                                FIND FIRST ref-side WHERE
                                    ref-side.reftable EQ "ce/v-est3.w Unit#1"  AND
                                    ref-side.company  EQ eb.company AND
                                    ref-side.loc      EQ eb.est-no AND
                                    ref-side.code     EQ STRING(eb.form-no,"9999999999") AND
                                    ref-side.code2    EQ STRING(eb.blank-no,"9999999999")
                                    NO-ERROR.
                                IF AVAILABLE ref-side THEN
                                    wrk-ink.i-side = FILL(" ",5) + SUBSTRING(ref-side.dscr,i - 12,1).
                            END.          
                        
                            IF wrk-ink.i-unit EQ 0 THEN
                                wrk-ink.i-unit = 1.
                        END.
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
                IF AVAILABLE wrk-ink AND
                    ((est.est-type EQ 4 AND eb.form-no = job-mat.frm AND eb.blank-no EQ job-mat.blank-no) OR
                    est.est-type NE 4 ) 
                    THEN wrk-ink.i-qty = wrk-ink.i-qty + job-mat.qty.            
            
            END. /* JOB-MAT */

        IF eb.est-type EQ 4 THEN v-fac = eb.yld-qty / v-est-qty.
          
        FIND FIRST style NO-LOCK
            WHERE style.company EQ eb.company
            AND style.style   EQ eb.style
            NO-ERROR.
        IF AVAILABLE style THEN v-stypart = style.dscr.
        ASSIGN
            v-dsc[1]  = eb.part-dscr1
            v-dsc[2]  = eb.part-dscr2
            v-size[1] = STRING(eb.len) + "x" + STRING(eb.wid) + "x" +
                         STRING(eb.dep)
            v-size[2] = eb.i-coldscr.

        IF eb.blank-no GT 0 AND eb.blank-no LE 11 THEN 
            ASSIGN v-fgdsc[eb.blank-no] = eb.part-dscr1.
                                                             
        /*if v-first then*/
        v-upc-lbl = "   CAD#".
        IF FIRST-OF(eb.form-no) THEN
            PUT "<P12><B> P R E S S <P9>" SKIP 
                " F/B   FG Item #       Cust Part #     Artwork #       Description       Order Qty       MAX QTY      MIN QTY      JOB QTY </B>" SKIP.
              
        v-job-qty = 0.
        FOR EACH xjob-hdr FIELDS(qty) NO-LOCK 
            WHERE xjob-hdr.company EQ cocode
            AND xjob-hdr.job     EQ job-hdr.job
            AND xjob-hdr.job-no  EQ job-hdr.job-no
            AND xjob-hdr.job-no2 EQ job-hdr.job-no2
            AND xjob-hdr.i-no    EQ eb.stock :
            v-job-qty = v-job-qty + xjob-hdr.qty.
        END.

        /** PRINT ITEM **/
        FIND FIRST oe-ordl NO-LOCK
            WHERE oe-ordl.company EQ job-hdr.company
            AND oe-ordl.ord-no  EQ job-hdr.ord-no
            AND oe-ordl.job-no  EQ job-hdr.job-no
            AND oe-ordl.job-no2 EQ job-hdr.job-no2
            AND oe-ordl.i-no    EQ eb.stock-no /*job-hdr.i-no*/
            NO-ERROR.
 
        IF AVAILABLE oe-ordl THEN 
        DO:
            v-est-qty = oe-ordl.qty.
            FIND FIRST oe-ord OF oe-ordl NO-LOCK.
            v-ovund = STRING("Overrun/Underrun %:  " +
                TRIM(STRING(oe-ordl.over-pct,">>9.99")) + "/" +
                TRIM(STRING(oe-ordl.under-pct,">>9.99"))).
            ASSIGN
                v-max-qty = INTEGER ( oe-ordl.qty + oe-ordl.qty * (DECIMAL(lv-over-run) / 100) )
                v-min-qty = INTEGER ( oe-ordl.qty - oe-ordl.qty * (DECIMAL(lv-under-run) / 100)).
        END.
        ELSE v-est-qty = v-job-qty.
           
        RELEASE w-lo.
        FIND FIRST w-lo NO-ERROR.
        ASSIGN
            v-case-size   = STRING(eb.cas-len) + "x" + STRING(eb.cas-wid) + "x" +
                         STRING(eb.cas-dep)
            v-up          = eb.num-up
            v-case-count  = IF AVAILABLE oe-ordl AND oe-ordl.cas-cnt NE 0 THEN oe-ordl.cas-cnt
                           ELSE eb.cas-cnt
            v-case-qty    = ROUND(v-job-qty / v-case-count,0)
            v-itm-printed = v-itm-printed + 1.

        FIND FIRST itemfg NO-LOCK WHERE itemfg.company EQ job-hdr.company
            AND itemfg.i-no    EQ job-hdr.i-no NO-ERROR.

        display SPACE(1) trim(string(eb.form-no,">>9")) + "-" +
                    trim(string(eb.blank-no,">>9")) FORM "x(5)" 
                    SPACE(1) eb.stock-no @ job-hdr.i-no 
                    (IF AVAIL oe-ordl  THEN oe-ordl.part-no ELSE IF AVAIL itemfg THEN itemfg.part-no ELSE "") FORM "x(15)"   SPACE(1)
                    (IF eb.plate-no <> "" THEN eb.plate-no  ELSE IF AVAIL itemfg THEN itemfg.plate-no ELSE "" ) FORM "x(15)"
                    SPACE(1) v-dsc[1] FORM "x(16)"
                    oe-ordl.qty WHEN AVAIL oe-ordl format "->,>>>,>>9"  /* Task #01240503*/   SPACE(4)
                    v-max-qty     SPACE(3)
                    v-min-qty     SPACE(3)
                    job-hdr.qty 
                with stream-io width 175 no-labels no-box frame line-det1.

        FIND FIRST ITEM NO-LOCK
            WHERE item.company EQ cocode
            AND item.i-no    EQ eb.cas-no
            NO-ERROR.
        /* v-cas-dscr = if avail item then item.i-name else "". */
        v-item-desc = IF AVAILABLE ITEM THEN ITEM.i-name ELSE  "" .  
        /* end. /* last-of(eb.form-no) */      */
        IF LAST-OF(eb.form-no) THEN 
        DO:
            IF v-itm-printed = 1 THEN PUT v-fill SKIP. 
            ELSE PUT SKIP(4 - v-itm-printed).
             
            /* Number of sheets ticket1.p - single board, ticket2.p - multi board */
            RUN oe/rep/ticket2.p (RECID(ef), RECID(job-hdr)).
            /*find first wrk-sheet where recid(wrk-sheet) eq save_id.*/
            IF AVAILABLE oe-ordl THEN
                FIND FIRST po-ord NO-LOCK WHERE po-ord.company EQ oe-ordl.company
                    AND po-ord.po-no EQ INTEGER(oe-ordl.po-no-po) NO-ERROR.
            ASSIGN
                v-vend     = IF AVAILABLE oe-ordl THEN oe-ordl.vend-no ELSE ""
                v-board-po = IF AVAILABLE oe-ordl THEN oe-ordl.po-no-po ELSE 0.

            IF AVAILABLE po-ord THEN
                FIND FIRST po-ordl NO-LOCK WHERE
                    po-ordl.company EQ po-ord.company AND
                    po-ordl.po-no   EQ po-ord.po-no AND
                    po-ordl.i-no EQ ef.board
                    NO-ERROR.

            v-po-duedate = IF AVAILABLE po-ordl THEN po-ordl.due-date ELSE ?.

            PUT "<P10>" 
                " <B>BOARD CODE                    STOCK CODE GRAIN     SHEETS      LF    SHEET SIZE    NET SHEET     DIE SIZE          DIE#              </B>" 
                SKIP.

            /** PRINT SHEET **/
            x = 2.
             
            FOR EACH wrk-sheet WHERE wrk-sheet.form-no = ef.form-no
            /*break by wrk-sheet.form-no*/:
                FIND FIRST ITEM NO-LOCK WHERE item.company EQ cocode
                    AND item.i-no    EQ wrk-sheet.i-no NO-ERROR.
                /*06271307 - change LBs to LF*/
                /*                v-lbs = wrk-sheet.gsh-qty * (wrk-sheet.sh-wid * wrk-sheet.sh-len / 144) / 1000 * ITEM.basis-w. */
                v-lbs = wrk-sheet.gsh-qty * wrk-sheet.sh-len / 12.
                FIND FIRST notes NO-LOCK WHERE notes.rec_key      EQ job.rec_key AND
                    notes.note_code    EQ "BS" AND
                    notes.note_form_no EQ wrk-sheet.form-no NO-ERROR.
                v-dept-title = IF AVAILABLE notes THEN notes.note_title ELSE "".

                DISPLAY SPACE(1) ITEM.i-no FORMAT "x(10)" SPACE(20)
                    TRIM(wrk-sheet.i-no) FORMAT "X(10)" SPACE(2)
                    ef.xgrain FORMAT "x(2)"
                    wrk-sheet.gsh-qty 
                    v-lbs
                    STRING(wrk-sheet.sh-wid) + "x" + STRING(wrk-sheet.sh-len)
                    FORMAT "x(13)"
                    STRING(ef.nsh-wid) + "x" + STRING(ef.nsh-len) FORMAT "x(13)"
                    STRING(ef.trim-w) + "x" + STRING(ef.trim-l) FORMAT "x(17)"
                    eb.die-no 
                    WITH STREAM-IO WIDTH 170 NO-LABELS NO-BOX FRAME sheet.
                /*PUT ITEM.i-name FORMAT "x(28)" SKIP.*/
                x = 1.
            END. /* each wrk-sheet */
            IF x NE 2 THEN PUT v-fill AT 1 SKIP.
             
            PUT "<B> PASS  SIDE  LBS    INK NAME               UNIT#   PASS   SIDE    LBS   INK NAME               UNIT#       </B>"
                SKIP.

            ASSIGN
                x            = 2
                i            = 1
                v-ink1       = ""
                v-ink2       = ""
                v-pass-count = 0.

            FOR EACH wrk-ink WHERE wrk-ink.form-no EQ eb.form-no
                BREAK BY wrk-ink.i-pass:
                IF FIRST-OF(wrk-ink.i-pass) THEN v-pass-count = v-pass-count + 1.
            END.
            FOR EACH wrk-ink WHERE wrk-ink.form-no EQ eb.form-no
                BREAK BY wrk-ink.i-pass
                BY wrk-ink.i-code
                BY wrk-ink.blank-no:

                IF wrk-ink.i-pass LE 2 THEN
                    IF FIRST-OF(wrk-ink.i-pass) THEN i = 1.

                IF FIRST-OF(wrk-ink.i-code) THEN ASSIGN v-item[i]  = ""
                        v-i-qty[i] = 0.
                ASSIGN
                    v-item[i]  = IF LOOKUP(STRING(wrk-ink.blank-no),v-item[i]) GT 0 THEN v-item[i] ELSE v-item[i] + string(wrk-ink.blank-no) + ","
                    v-i-qty[i] = v-i-qty[i] + wrk-ink.i-qty.

                /*IF LAST-OF(wrk-ink.i-code) THEN DO:*/
                IF SUBSTRING(v-item[i],LENGTH(v-item[i]),1) EQ "," THEN v-item[i] = SUBSTRING(v-item[i],1,LENGTH(v-item[i]) - 1).                    
                
                IF wrk-ink.i-side NE "" THEN
                DO: 
                    IF wrk-ink.i-pass EQ 1 THEN
                        ASSIGN v-ink1[i] = STRING(wrk-ink.i-pass) + wrk-ink.i-side +
                                             STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                             STRING(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                            i         = i + 1. 
                    ELSE IF wrk-ink.i-pass EQ 2 THEN
                            ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + wrk-ink.i-side +
                                             STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                             STRING(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                i         = i + 1.
                        ELSE IF wrk-ink.i-pass EQ 3 THEN
                                ASSIGN v-ink1[i] = STRING(wrk-ink.i-pass) + wrk-ink.i-side +
                                             STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                             STRING(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                    i         = i + 1.
                            ELSE IF wrk-ink.i-pass EQ 4 THEN
                                    ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + wrk-ink.i-side +
                                             STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                             STRING(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                        i         = i + 1.
                                ELSE IF wrk-ink.i-pass EQ 5 THEN
                                        ASSIGN v-ink1[i] = STRING(wrk-ink.i-pass) + wrk-ink.i-side +
                                             STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                             STRING(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                            i         = i + 1.
                                    ELSE IF wrk-ink.i-pass EQ 6 THEN
                                            ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + wrk-ink.i-side +
                                             STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                             STRING(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                                i         = i + 1.
                                        ELSE IF wrk-ink.i-pass EQ 7 THEN
                                                ASSIGN v-ink1[i] = STRING(wrk-ink.i-pass) + wrk-ink.i-side +
                                             STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                             STRING(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                                    i         = i + 1.
                                            ELSE IF wrk-ink.i-pass EQ 8 THEN
                                                    ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + wrk-ink.i-side +
                                             STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                             STRING(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                                        i         = i + 1.
                END.
                ELSE
                DO:
                    IF wrk-ink.i-pass EQ 1 THEN
                        ASSIGN v-ink1[i] = (IF v-pass-count EQ 1 THEN "F      " ELSE "B      ") +
                                              STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                             STRING(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                            i         = i + 1. 
                    ELSE IF wrk-ink.i-pass EQ 2 THEN
                            ASSIGN v-ink2[i] = "F      " + STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                      STRING(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                i         = i + 1.
                        ELSE IF wrk-ink.i-pass = 3 THEN
                                ASSIGN v-ink1[i] = "F      " + STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                      string(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                    i         = i + 1.
                            ELSE IF wrk-ink.i-pass EQ 4 THEN
                                    ASSIGN v-ink2[i] = "F      " + STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                      STRING(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                        i         = i + 1.
                                ELSE IF wrk-ink.i-pass EQ 5 THEN
                                        ASSIGN v-ink1[i] = "F      " + STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                      STRING(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                            i         = i + 1.
                                    ELSE IF wrk-ink.i-pass EQ 6 THEN
                                            ASSIGN v-ink2[i] = "F      " + STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                      STRING(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                                i         = i + 1.
                                        ELSE IF wrk-ink.i-pass EQ 7 THEN
                                                ASSIGN v-ink1[i] = "F      " + STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                      STRING(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                                    i         = i + 1.
                                            ELSE IF wrk-ink.i-pass = 8 THEN
                                                    ASSIGN v-ink2[i] = "F      " + STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                      STRING(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                                        i         = i + 1.
                END.

                /*END.*/
              
                DELETE wrk-ink.
            END. /* each wrk-ink */

            ASSIGN
                v-skip          = NO
                v-plate-printed = NO.
             
            DO j = 1 TO EXTENT(v-ink1):
                IF TRIM(v-ink1[j]) EQ "-" THEN v-ink1[j] = "".               
                IF v-ink1[j] NE "" THEN 
                DO:
                    IF v-skip THEN 
                    DO:
                        PUT v-ink1[j] FORMAT "x(52)" .
                        IF j EQ 2 THEN 
                        DO:
                            v-plate-printed = YES.
                        END.
                        PUT SKIP.
                    END.
                    ELSE PUT SPACE(1) v-ink1[j] FORMAT "x(52)".                                                             
                    v-skip = NOT v-skip.             
                END.
            END.

            IF NOT v-plate-printed THEN PUT SKIP(1).

            DO j = 1 TO EXTENT(v-ink2):
                IF TRIM(v-ink2[j]) EQ "-" THEN v-ink2[j] = "".                 
                IF v-ink2[j] NE "" THEN 
                DO:
                    IF v-skip THEN PUT SPACE(1) v-ink2[j] FORMAT "x(52)" SKIP.
                    ELSE PUT SPACE(1) v-ink2[j] FORMAT "x(52)".
                    v-skip = NOT v-skip.
                END.                
            END.
            PUT SKIP .

            ASSIGN
                v-upnew         = eb.num-wid * eb.num-len
                v-pr-speed      = 0
                v-mr-hours      = 0
                v-dc-gl-speed   = 0
                v-dc-out        = 0
                v-dc-only-out   = 0
                v-cas-wt        = 0
                v-sample-on-cnt = NO
                v-shrink-wrap   = CAN-FIND(FIRST est-op WHERE
                                est-op.company EQ job-hdr.company AND
                                est-op.est-no EQ est.est-no AND
                                est-op.dept = "SW").

            PUT "<B> UNIT SIZE:  </B>" STRING(eb.t-len) + " x " + STRING(eb.t-wid) FORMAT "X(38)"
                "  <B>#UP</B> :"   STRING(v-upnew)     "<B>Speed(FPM)        MR</B>" SKIP.
                
            FOR FIRST wrk-op WHERE
                wrk-op.s-num EQ job-hdr.frm AND
                wrk-op.dept EQ "PR"
                BREAK BY wrk-op.d-seq BY wrk-op.b-num:

                ASSIGN
                    v-pr-speed = wrk-op.speed[job-hdr.frm]
                    v-mr-hours = wrk-op.mr[job-hdr.frm].
            END.

            FOR FIRST wrk-op WHERE
                wrk-op.s-num EQ job-hdr.frm AND
                      INDEX("DC,GL",wrk-op.dept) > 0
                BREAK BY wrk-op.d-seq BY wrk-op.b-num:

                v-dc-gl-speed = wrk-op.speed[job-hdr.frm].
            END.

            FOR FIRST wrk-op WHERE
                wrk-op.s-num EQ job-hdr.frm AND
                wrk-op.dept EQ "DC"
                BREAK BY wrk-op.d-seq BY wrk-op.b-num:
                     
                v-dc-gl-speed = wrk-op.speed[job-hdr.frm].
            END.

            FOR EACH est-op FIELDS(n-out m-code d-seq b-num) NO-LOCK WHERE
                est-op.company EQ job-hdr.company AND
                est-op.est-no EQ est.est-no AND
                est-op.line LT 500,
                FIRST mach FIELDS(dept) NO-LOCK
                {sys/ref/machW.i}
                     AND mach.m-code EQ est-op.m-code
                     BY est-op.d-seq BY est-op.b-num:

            IF mach.dept[1] EQ "DC" OR
                mach.dept[2] EQ "DC" THEN
            DO:
                v-dc-only-out = est-op.n-out.
                LEAVE.
            END.
                
        END.

        FOR EACH est-op FIELDS(n-out m-code d-seq b-num) NO-LOCK WHERE
            est-op.company EQ job-hdr.company AND
            est-op.est-no EQ est.est-no AND
            est-op.line LT 500 ,
            FIRST mach NO-LOCK
            {sys/ref/machW.i}
                  AND mach.m-code EQ est-op.m-code
                  BY est-op.d-seq BY est-op.b-num:
                    
        IF INDEX("GL,DC",mach.dept[1]) GT 0 OR
                   INDEX("GL,DC",mach.dept[2]) GT 0 THEN
        DO:
            v-dc-out = est-op.n-out.
            LEAVE.
        END.
    END.

    IF ef.xgrain NE "B" THEN
        PUT " <B>#AC:</B>"    STRING(eb.num-wid)       STRING(eb.t-wid)  AT 21 v-pr-speed AT 69 v-mr-hours AT 86 SKIP  
            " <B>#AR:</B>"    STRING(eb.num-len)       STRING(eb.t-len)  AT 21.
    ELSE
        PUT " <B>#AC:</B>"    STRING(eb.num-wid)       STRING(eb.t-len)  AT 21 v-pr-speed AT 69 v-mr-hours AT 86 SKIP  
            " <B>#AR:</B>"    STRING(eb.num-len)       STRING(eb.t-wid)  AT 21.

    PUT v-fill AT 1 SKIP 
        "<B><P12> F I N I S H I N G</B><P9>" SKIP.

    FIND FIRST bf-job-mat NO-LOCK 
        WHERE bf-job-mat.company EQ job.company
        AND bf-job-mat.job     EQ job.job
        AND bf-job-mat.job-no  EQ job.job-no
        AND bf-job-mat.job-no2 EQ job.job-no2
        AND bf-job-mat.frm     EQ eb.form-no
        AND bf-job-mat.rm-i-no EQ eb.layer-pad 
        NO-ERROR .
                                         
    ASSIGN
        v-layer-qty = IF AVAILABLE bf-job-mat THEN bf-job-mat.qty ELSE 0 .
           
    FIND item WHERE
        item.company EQ eb.company AND
        item.i-no EQ eb.layer-pad  AND
        item.mat-type EQ "5" 
        NO-LOCK NO-ERROR.

    ASSIGN 
        v-lp-dep = IF AVAILABLE item THEN ITEM.case-d ELSE 0 
        v-lp-qty = IF AVAILABLE item THEN ITEM.box-case ELSE 0 .

    IF eb.lp-up NE 0 THEN
    DO:
        v-unit-per-dec = eb.cas-cnt / eb.lp-up.
        {sys/inc/roundup.i v-unit-per-dec}
        v-unit-per-int = INTEGER(v-unit-per-dec).
    END.
    ELSE
        v-unit-per-int = 0.

    IF v-unit-per-int NE 0 THEN
    DO:
        v-job-qty-unit-per-dec = v-job-qty / v-unit-per-int.
        {sys/inc/roundup.i v-job-qty-unit-per-dec}
        v-job-qty-unit-per-int = v-job-qty-unit-per-dec.
    END.
    ELSE
        v-job-qty-unit-per-int = 0.

    IF eb.cas-cnt NE 0 THEN
    DO:
        v-job-qty-boxes-code-dec = v-job-qty / eb.cas-cnt.
        {sys/inc/roundup.i v-job-qty-boxes-code-dec}
        v-job-qty-boxes-code-int = v-job-qty-boxes-code-dec.
    END.
    ELSE
        v-job-qty-boxes-code-int = 0.

    FIND FIRST itemfg NO-LOCK WHERE
        itemfg.company EQ eb.company AND
        itemfg.i-no EQ eb.stock-no
        NO-ERROR.
             
    IF AVAILABLE itemfg THEN
    DO:
        v-cas-wt = (itemfg.weight-100 / 100) * eb.cas-cnt.             
    END.

    FIND FIRST tt-sample-ctn WHERE
        tt-sample-ctn.tt-job-no EQ job-hdr.job-no AND
        tt-sample-ctn.tt-job-no2 EQ job-hdr.job-no2 AND
        tt-sample-ctn.tt-frm EQ eb.form-no
        NO-ERROR.

    IF AVAILABLE tt-sample-ctn THEN
        v-sample-on-cnt = tt-sample-ctn.tt-samp-on-cnt.

    IF v-dc-only-out EQ 0 THEN
        v-dc-only-out = 1.

    IF v-dc-out EQ 0 THEN
        v-dc-out = 1.

    FIND FIRST ITEM NO-LOCK
        WHERE item.company EQ cocode
        AND item.i-no    EQ eb.cas-no
        NO-ERROR.

    FIND FIRST bf-job-mat NO-LOCK
        WHERE bf-job-mat.company EQ job.company 
        AND bf-job-mat.job EQ job.job 
        AND bf-job-mat.job-no EQ job.job-no 
        AND bf-job-mat.job-no2 EQ job.job-no2
        AND bf-job-mat.frm EQ eb.form-no
        AND bf-job-mat.rm-i-no EQ eb.cas-no 
        NO-ERROR .
    ASSIGN
        v-cases-qty = IF AVAILABLE bf-job-mat THEN bf-job-mat.qty ELSE 0 .
    ASSIGN 
        ddivider = 0.
    IF AVAILABLE eb THEN 
    DO:
        RUN find-depth-reftable(ROWID(eb), OUTPUT lv-rowid).
        FIND reftable WHERE ROWID(reftable) EQ lv-rowid NO-ERROR.
        IF AVAILABLE reftable THEN
            ASSIGN
                ddivider = reftable.val[2].
    END.

    PUT "<P9><B> UNIT SIZE   Flat:</B>"  STRING(eb.t-len) + " x " + STRING(eb.t-wid * v-dc-only-out)  FORMAT "x(19)"
        "<B>Finished:</B> "  STRING(eb.len) + " x " + STRING(eb.wid) FORMAT "X(19)"
        "<B>UP#:</B>" v-dc-out SPACE(4)
        "<B>Speed:</B>" v-dc-gl-speed FORMAT "->,>>>,>>9" SPACE(4)
        "<B>Style:</B> " eb.style  SKIP

        "<B> Packaging: " SKIP
        " Tray #: </B>" eb.layer-pad FORMAT "x(15)"
        "<C20><B>Size: </B>"  STRING(eb.lp-len) + "x" + STRING(eb.lp-wid) + "x" + STRING(v-lp-dep)  FORMAT "x(27)"
        "<C38><B>  Qty Per Tray:</B>" /*v-lp-qty*/ ( IF v-lp-qty GT 0 THEN eb.cas-cnt / v-lp-qty ELSE 0)  FORMAT "->>>>>9.9<"
        "<C60><B># of trays:</B>"   v-layer-qty FORMAT "->>>>>>9.9<<"  SKIP 

        "<B> Case #: </B>"   eb.cas-no
        "<C20><B>Size: </B>"    STRING(eb.cas-len) + "x" + STRING(eb.cas-wid) + "x" + STRING(eb.cas-dep) FORMAT "x(27)"
        "<C39><B> Qty per case:</B>"   eb.cas-cnt FORMAT "->>>>>>>9"
        "<C60><B># of Cases:</B>" STRING(v-cases-qty,"->,>>>,>>9")  /*v-job-qty-boxes-code-int*/  FORMAT "x(10)"  
        "<C79><B>Case Wt:</B>" STRING(v-cas-wt,">>>>9.99") SKIP

        "<B> Divider: </B>"   eb.divider FORMAT "x(10)"
        "<C20><B>Size: </B>"    STRING(eb.div-len) + "x" + STRING(eb.div-wid) + "x" + STRING(ddivider) FORMAT "x(27)"
        "<C39><B> Qty per case:</B>"   eb.div-up FORMAT "->>>>>>>9" SKIP
        

        "<B> Pallet:</B> " eb.tr-no
        "<C20><B>Shrink Wrap: </B>" STRING(v-shrink-wrap,"Y/N")  
        "<C39><B>Packing Specs: </B>" (IF AVAILABLE itemfg THEN  itemfg.prod-no ELSE "") FORMAT "x(20)" SKIP .

    PUT v-fill AT 1 SKIP.
             
END. /* last-of(eb.form-no) */
          
END. /* each eb */
END. /* each ef */
END. /* first job-no */

IF LAST-OF(job-hdr.frm) THEN 
DO:

    IF s-run-speed THEN
        PUT "<B> MACHINE                 MR WASTE   MR HRS  RUN SPEED   SPOIL%    SHEETS PER MACHINE   </B>" /* SIZE  TOTAL REQUIRED  PALLET*/
            SKIP.
    ELSE
        PUT "<B> MACHINE                 MR WASTE   MR HRS   RUN HOUR    SPOIL%    SHEETS PER MACHINE   </B>"  /* SIZE  TOTAL REQUIRED  PALLET*/
            SKIP.

    FOR EACH wrk-op WHERE wrk-op.s-num = job-hdr.frm BREAK BY wrk-op.d-seq BY wrk-op.b-num:
        v-mat-for-mach = "".
        IF LOOKUP(wrk-op.dept,lv-mat-dept-list) GT 0 THEN 
        DO:
                 
            FOR EACH xjob-mat NO-LOCK WHERE xjob-mat.company EQ cocode
                AND xjob-mat.job     EQ job-hdr.job
                AND xjob-mat.job-no  EQ job-hdr.job-no
                AND xjob-mat.job-no2 EQ job-hdr.job-no2
                AND xjob-mat.frm = job-hdr.frm
                /*AND (xjob-mat.blank-no = job-hdr.blank-no
                     OR xjob-mat.blank-no = 0)*/ ,
                FIRST ITEM NO-LOCK WHERE ITEM.company EQ cocode AND
                ITEM.i-no EQ xjob-mat.rm-i-no AND
                ITEM.mat-type EQ SUBSTRING(wrk-op.dept,1,1) :
                v-mat-for-mach = /*ITEM.i-name + fill(" ", 33 - LENGTH(ITEM.i-name))  /*"       " */ + */
                    STRING(xjob-mat.wid) + "x" + STRING(xjob-mat.len) +
                    "      " + string(xjob-mat.qty).                   
                LEAVE.                 
            END.                            
        END.
     

        IF LAST(wrk-op.d-seq) THEN 
        DO: /* pallet code*/
            FOR EACH xjob-mat NO-LOCK WHERE xjob-mat.company EQ cocode
                AND xjob-mat.job     EQ job-hdr.job
                AND xjob-mat.job-no  EQ job-hdr.job-no
                AND xjob-mat.job-no2 EQ job-hdr.job-no2
                AND xjob-mat.frm EQ job-hdr.frm
                AND (xjob-mat.blank-no = job-hdr.blank-no
                OR xjob-mat.blank-no = 0),
                FIRST ITEM NO-LOCK WHERE ITEM.company EQ cocode AND
                ITEM.i-no EQ xjob-mat.rm-i-no AND 
                ITEM.mat-type = "D" :
                v-mat-for-mach = v-mat-for-mach +
                    (IF LENGTH(v-mat-for-mach) LE 20 THEN FILL(" ", 20 - LENGTH(v-mat-for-mach))
                    ELSE "  " ) +
                    ITEM.i-name.
            END.
        END.          
        IF s-prt-mstandard THEN 
        DO:
            IF s-run-speed THEN
                PUT SPACE(1) wrk-op.m-dscr   SPACE(4)
                    wrk-op.mr-waste[job-hdr.frm]   SPACE(6)
                    wrk-op.mr[job-hdr.frm]         SPACE(5)
                    wrk-op.speed[job-hdr.frm]      SPACE(5)
                    wrk-op.spoil[job-hdr.frm]      SPACE(5)
                    wrk-op.num-sh[job-hdr.frm]     SPACE(11)  /*9+9*/
                    SKIP.
            ELSE
                PUT SPACE(1) wrk-op.m-dscr   SPACE(5)
                    wrk-op.mr-waste[job-hdr.frm]   SPACE(5)
                    wrk-op.mr[job-hdr.frm]         SPACE(5)
                    wrk-op.run-hr[job-hdr.frm]     SPACE(5)
                    wrk-op.spoil[job-hdr.frm]      SPACE(5)
                    wrk-op.num-sh[job-hdr.frm]     SPACE(11)  /* 9 + 9 */
                    SKIP.
        END.
        ELSE PUT SPACE(1) wrk-op.m-dscr   SPACE(4)
                SPACE(10)
                SPACE(11)
                SPACE(10)
                SPACE(13)
                wrk-op.num-sh[job-hdr.frm]     SPACE(11)  /* 9 + 9 */
                SKIP.
    END. /* each wrk-op*/        

    PUT v-fill AT 1 SKIP.        
    /** PRINT JOB INSTRUCTIONS **/

        
    /* dept notes*/
    lv-line-chars = 128.
    FIND FIRST job OF job-hdr NO-LOCK NO-ERROR.       
        
    /*=change */
    FOR EACH tt-formtext:
        DELETE tt-formtext.
    END.
    ASSIGN
        lv-text             = ""
        v-dept-inst         = ""
        v-exc-depts         = v-exc-depts + (IF v-exc-depts NE "" THEN ",BS" ELSE "BS")
        v-dept-note-printed = NO.

    FOR EACH notes NO-LOCK WHERE notes.rec_key EQ job.rec_key
        AND (notes.note_form_no EQ job-hdr.frm OR notes.note_form_no = 0)
        AND (( notes.note_type EQ "O" AND notes.note_group EQ string(job.job) ) OR  notes.note_type NE "O" )
        AND LOOKUP(notes.note_code,v-exc-depts) EQ 0 
        /*AND notes.note_type NE 'O'*/   /* ticket 14661 */
        BREAK BY notes.note_code:
             
        IF FIRST-OF(notes.note_code) THEN 
        DO:
            lv-text = "".
            FOR EACH tt-formtext:
                DELETE tt-formtext.
            END.
            FIND FIRST dept NO-LOCK WHERE dept.CODE EQ notes.note_code NO-ERROR.
            lv-text = (IF AVAILABLE dept THEN dept.dscr ELSE notes.note_code) + "     " + 
                notes.note_title + " " + notes.note_text + CHR(10).
            DO li = 1 TO 4:
                CREATE tt-formtext.
                ASSIGN 
                    tt-line-no = li
                    tt-length  = 128.
            END.
            RUN custom/formtext.p (lv-text).
            i = 0.
            v-dept-inst = "".
            FOR EACH tt-formtext:
                i = i + 1.
                IF  i LE 4 THEN v-dept-inst[i] = tt-formtext.tt-text.      
            END.
            IF PAGE-SIZE - LINE-COUNTER LT 11 THEN PAGE.
            IF FIRST(notes.note_code) THEN PUT "<B> DEPARTMENT   INSTRUCTION NOTES</B>" SKIP.

            IF v-dept-inst[1] NE "" THEN
                PUT " " v-dept-inst[1] FORMAT "x(128)" SKIP.
            IF v-dept-inst[2] NE "" THEN
                PUT " " v-dept-inst[2] FORMAT "x(128)" SKIP.
            IF v-dept-inst[3] NE "" THEN
                PUT " " v-dept-inst[3] FORMAT "x(128)" SKIP.
            IF v-dept-inst[4] NE "" THEN
                PUT " " v-dept-inst[4] FORMAT "x(128)" SKIP.
               
        END. /* first-of(notes.note_code) */
        v-dept-note-printed = YES.
    END. /* for each notes */
    
    /*==== note ======*/


    IF NOT v-dept-note-printed THEN 
    DO:
        IF PAGE-SIZE - LINE-COUNTER LT 11 THEN PAGE.
        PUT "<B> DEPARTMENT   INSTRUCTION NOTES</B>" SKIP.
        IF v-dept-inst[1] NE "" THEN
            PUT " " v-dept-inst[1] FORMAT "x(128)" SKIP.
        IF v-dept-inst[2] NE "" THEN
            PUT " " v-dept-inst[2] FORMAT "x(128)" SKIP.
        IF v-dept-inst[3] NE "" THEN
            PUT " " v-dept-inst[3] FORMAT "x(128)" SKIP.
        IF v-dept-inst[4] NE "" THEN
            PUT " " v-dept-inst[4] FORMAT "x(128)" SKIP.
    END.

    /* spec note */
    ASSIGN
        lv-line-chars = 95
        v-inst2       = "".

    FIND FIRST itemfg NO-LOCK WHERE itemfg.company EQ job-hdr.company
        AND itemfg.i-no EQ job-hdr.i-no NO-ERROR.

    {custom/notespr8.i itemfg v-inst2 25 "notes.rec_key EQ itemfg.rec_key AND
                           notes.note_type EQ 'S' AND CAN-DO(v-spec-list,notes.note_code) "}
        

    PUT "<B> SPEC CODE                 SPEC NOTES</B>" SKIP.
        
    DO i = 1 TO 25:
        IF v-inst2[i] NE "" THEN 
        DO:        

            PUT " " v-inst2[i] FORMAT "X(128)" SKIP.

            IF LINE-COUNTER GE 47 THEN 
            DO:
                PAGE.
                PUT v-fill SKIP
                    "<B> SPEC CODE                 SPEC NOTES</B>" SKIP.
              
            END. /* IF v-lincnt GE 12 */
        END. /* IF v-inst2[i] NE ""*/              
    END. /* DO */
    ASSIGN
        i       = 1
        v-fgitm = "".

    FOR EACH tt-fgitm.
        DELETE tt-fgitm.
    END.

    FOR EACH xjob-hdr NO-LOCK WHERE xjob-hdr.company EQ cocode
        AND xjob-hdr.job     EQ job-hdr.job
        AND xjob-hdr.job-no  EQ job-hdr.job-no
        AND xjob-hdr.job-no2 EQ job-hdr.job-no2
        AND xjob-hdr.frm     EQ job-hdr.frm BY xjob-hdr.blank-no:
           
        FIND FIRST xoe-ordl NO-LOCK
            WHERE xoe-ordl.company EQ xjob-hdr.company
            AND xoe-ordl.ord-no  EQ xjob-hdr.ord-no
            AND xoe-ordl.job-no  EQ xjob-hdr.job-no
            AND xoe-ordl.job-no2 EQ xjob-hdr.job-no2
            AND xoe-ordl.i-no    EQ xjob-hdr.i-no
            NO-ERROR.
        IF AVAILABLE xoe-ordl THEN v-fgqty[i] = xoe-ordl.cas-cnt.
           
        FIND FIRST b-eb NO-LOCK WHERE b-eb.company EQ xjob-hdr.company
            AND b-eb.est-no  EQ xjob-hdr.est-no
            AND b-eb.form-no EQ xjob-hdr.frm
            AND b-eb.blank-no EQ xjob-hdr.blank-no
            AND b-eb.stock-no EQ xjob-hdr.i-no
            NO-ERROR.
        IF NOT AVAILABLE b-eb THEN 
            FIND FIRST b-eb NO-LOCK WHERE b-eb.company EQ xjob-hdr.company
                AND b-eb.est-no  EQ xjob-hdr.est-no
                AND b-eb.form-no EQ xjob-hdr.frm
                AND b-eb.stock-no EQ xjob-hdr.i-no
                NO-ERROR.
        CREATE tt-fgitm.
        ASSIGN 
            tt-fgitm.i-no   = xjob-hdr.i-no
            tt-fgitm.qty    = IF AVAILABLE xoe-ordl AND xoe-ordl.cas-cnt NE 0 THEN xoe-ordl.cas-cnt 
                                  ELSE IF AVAILABLE b-eb THEN b-eb.cas-cnt ELSE xjob-hdr.qty
            tt-fgitm.i-dscr = IF AVAILABLE b-eb THEN b-eb.part-dscr1 ELSE xjob-hdr.i-no
            tt-fgitm.po-no  = IF AVAILABLE xoe-ordl THEN xoe-ordl.po-no ELSE ""
            tt-fgitm.seq    = i
            i               = i + 1.

        FIND FIRST b-est NO-LOCK WHERE
            b-est.company  EQ xjob-hdr.company AND
            b-est.est-no   EQ xjob-hdr.est-no
            NO-ERROR.

        IF AVAILABLE b-est AND b-est.est-type EQ 4 THEN /*combo*/
        DO:
            FIND FIRST b-cust NO-LOCK WHERE
                b-cust.company EQ xjob-hdr.company AND
                b-cust.cust-no EQ xjob-hdr.cust-no
                NO-ERROR.

            IF AVAILABLE b-cust THEN
            DO:
                tt-fgitm.cust-name = b-cust.NAME.

                FIND FIRST b-oe-ordl NO-LOCK WHERE
                    b-oe-ordl.company EQ xjob-hdr.company AND
                    b-oe-ordl.ord-no  EQ xjob-hdr.ord-no AND
                    b-oe-ordl.job-no  EQ xjob-hdr.job-no AND
                    b-oe-ordl.job-no2 EQ xjob-hdr.job-no2 AND
                    b-oe-ordl.i-no    EQ xjob-hdr.i-no
                    NO-ERROR.

                IF AVAILABLE b-oe-ordl THEN 
                    FIND FIRST b-oe-rel NO-LOCK WHERE
                        b-oe-rel.company EQ cocode AND
                        b-oe-rel.ord-no  EQ b-oe-ordl.ord-no AND
                        b-oe-rel.i-no    EQ b-oe-ordl.i-no AND
                        b-oe-rel.line    EQ b-oe-ordl.LINE
                        NO-ERROR.

                IF AVAILABLE b-oe-rel THEN 
                DO:
                    FIND FIRST b-shipto NO-LOCK WHERE
                        b-shipto.company EQ cocode AND
                        b-shipto.cust-no EQ b-oe-rel.cust-no AND
                        b-shipto.ship-id EQ b-oe-rel.ship-id
                        NO-ERROR.  
                 
                    IF AVAILABLE b-shipto THEN
                        ASSIGN
                            tt-fgitm.shipto1 = b-shipto.ship-name
                            tt-fgitm.shipto2 = b-shipto.ship-addr[1]
                            tt-fgitm.shipto4 = TRIM(b-oe-rel.ship-city) + ", " +
                                             b-oe-rel.ship-state + "  " + b-oe-rel.ship-zip.
                 
                    RELEASE b-cust.
                    RELEASE b-oe-ordl.
                    RELEASE b-oe-rel.
                    RELEASE b-shipto.
                END.
            END.
        END.

    /* IF i > 10 THEN LEAVE.*/

    END.
        
    IF s-prt-shipto THEN 
    DO i = 1 TO 4:
        ASSIGN 
            v-shipto1[i] = v-shipto[i]
            v-shipto2[i] = v-shipto[i].
    END.
    ASSIGN 
        v-cust-name2 = v-cust-name 
        v-cust-name3 = v-cust-name
        .
    /* label prints per item */

    IF NOT s-prt-label THEN PUT SKIP v-fill SKIP.
    ELSE  
    DO:

        i = 0.
        j = 0.
        
        FOR EACH tt-fgitm BY tt-fgitm.seq.
            
            IF PAGE-SIZE - LINE-COUNTER LT 15 THEN PAGE.

            ASSIGN 
                i                     = i + 1
                v-fgitm[i]            = tt-fgitm.i-no
                v-fgdsc[i]            = tt-fgitm.i-dscr
                v-fgqty[i]            = tt-fgitm.qty
                v-pono[i]             = tt-fgitm.po-no
                v-cust-name-extent[i] = tt-fgitm.cust-name
                v-ship1-extent[i]     = tt-fgitm.shipto1
                v-ship2-extent[i]     = tt-fgitm.shipto2
                v-ship4-extent[i]     = tt-fgitm.shipto4
                j                     = j + 1.
            IF i GE 3 THEN 
            DO:
                IF v-cust-name-extent[2] NE "" THEN
                    ASSIGN
                        v-cust-name2 = v-cust-name-extent[2]
                        v-cust-name3 = v-cust-name-extent[3]
                        v-shipto1[1] = v-ship1-extent[2]
                        v-shipto2[1] = v-ship1-extent[3]
                        v-shipto1[2] = v-ship2-extent[2]
                        v-shipto2[2] = v-ship2-extent[3]
                        v-shipto1[4] = v-ship4-extent[2]
                        v-shipto2[4] = v-ship4-extent[3].

                DISPLAY v-fill SKIP
                    "<B><U>LABEL ITEM" + TRIM(STRING(j - 2)) + "</U>"  FORMAT "x(22)"
                    "<U>LABEL ITEM" + TRIM(STRING(j - 1)) + "</U>" FORMAT "x(20)" 
                    WHEN v-fgitm[2] <> "" AT 55
                    "<U>LABEL ITEM" + TRIM(STRING(j)) + "</U></B>" FORMAT "x(23)" 
                    WHEN v-fgitm[3] <> "" AT 107
                    SKIP
                    "Job#:" v-job-no + "-" + STRING(v-job-no2)
                    "Job#:" 
                    WHEN v-fgitm[2] <> ""  AT 45
                    v-job-no + "-" + STRING(v-job-no2)   
                    WHEN v-fgitm[2] <> "" 
                    "Job#:" 
                    WHEN v-fgitm[3] <> "" AT 90  
                    v-job-no + "-" + STRING(v-job-no2) 
                    WHEN v-fgitm[3] <> "" 
                    SKIP
                    "Customer:" v-cust-name 
                    "Customer:"  
                    WHEN v-fgitm[2] <> ""  AT 45 v-cust-name2  
                    WHEN v-fgitm[2] <> "" 
                    "Customer:" 
                    WHEN v-fgitm[3] <> "" AT 90  v-cust-name3 
                    WHEN v-fgitm[3] <> "" 
                    SKIP
                    "Purchase Order#:" v-pono[1]
                    "Purchase Order#:"  
                    WHEN v-fgitm[2] <> "" AT 45 v-pono[2]  
                    WHEN v-fgitm[2] <> "" 
                    "Purchase Order#:" 
                    WHEN v-fgitm[3] <> "" AT 90  v-pono[3] 
                    WHEN v-fgitm[3] <> "" 
                    SKIP
                    "<B>FG Item #:</B>" v-fgitm[1]
                    "<B>FG Item #:</B>"  
                    WHEN v-fgitm[2] <> "" AT 52 v-fgitm[2] 
                    WHEN v-fgitm[2] <> "" 
                    "<B>FG Item #:</B>" 
                    WHEN v-fgitm[3] <> "" AT 104 v-fgitm[3] 
                    WHEN v-fgitm[3] <> "" 
                    SKIP
                    "Description:" v-fgdsc[1]
                    "Description:"  
                    WHEN v-fgitm[2] <> "" AT 45 v-fgdsc[2]  
                    WHEN v-fgitm[2] <> "" 
                    "Description:" 
                    WHEN v-fgitm[3] <> "" AT 90  v-fgdsc[3] 
                    WHEN v-fgitm[3] <> "" 
                    SKIP
                    "Count:   " v-fgqty[1]
                    "Count:   "  
                    WHEN v-fgitm[2] <> "" AT 45 v-fgqty[2]  
                    WHEN v-fgitm[2] <> "" 
                    "Count:   "  
                    WHEN v-fgitm[3] <> "" AT 90  v-fgqty[3] 
                    WHEN v-fgitm[3] <> ""                               
                    SKIP
                    "Shipto:" 
                    WHEN s-prt-shipto v-shipto[1] 
                    WHEN s-prt-shipto
                    "Shipto:" 
                    WHEN s-prt-shipto AND v-fgitm[2] <> "" AT 45 v-shipto1[1] 
                    WHEN s-prt-shipto AND v-fgitm[2] <> ""
                    "Shipto:" 
                    WHEN s-prt-shipto AND v-fgitm[3] <> "" AT 90 v-shipto2[1] 
                    WHEN s-prt-shipto AND v-fgitm[3] <> ""
                    SKIP
                    v-shipto[2] AT 8  
                    WHEN s-prt-shipto
                    v-shipto1[2] 
                    WHEN s-prt-shipto AND v-fgitm[2] <> "" AT 53
                    v-shipto2[2] 
                    WHEN s-prt-shipto AND v-fgitm[3] <> "" AT 98
                    SKIP
                    v-shipto[4] AT 8  
                    WHEN s-prt-shipto
                    v-shipto1[4] 
                    WHEN s-prt-shipto AND v-fgitm[2] <> "" AT 53
                    v-shipto2[4] 
                    WHEN s-prt-shipto AND v-fgitm[3] <> "" AT 98
                    SKIP
                    WITH FRAME itmlbl NO-BOX NO-LABELS STREAM-IO WIDTH 180.
                DOWN WITH FRAME itmlbl.   
            
                ASSIGN 
                    i                     = 0
                    v-fgitm[1]            = ""
                    v-fgdsc[1]            = ""
                    v-fgqty[1]            = 0
                    v-fgitm[2]            = ""
                    v-fgdsc[2]            = ""
                    v-fgqty[2]            = 0
                    v-fgitm[3]            = ""
                    v-fgdsc[3]            = ""
                    v-fgqty[3]            = 0
                    v-pono[1]             = ""
                    v-pono[2]             = ""
                    v-pono[3]             = ""
                    v-cust-name-extent[1] = ""
                    v-cust-name-extent[2] = ""
                    v-cust-name-extent[3] = ""
                    v-ship1-extent[1]     = ""
                    v-ship1-extent[2]     = ""
                    v-ship1-extent[3]     = ""
                    v-ship2-extent[1]     = ""
                    v-ship2-extent[2]     = ""
                    v-ship2-extent[3]     = ""
                    v-ship4-extent[1]     = ""
                    v-ship4-extent[2]     = ""
                    v-ship4-extent[3]     = ""
                    v-last-j              = j.
            END. /* i = 3 */
        END.
        IF i GT 0 THEN 
        DO:
            IF PAGE-SIZE - LINE-COUNTER LT 15 THEN PAGE.

            IF v-cust-name-extent[2] NE "" THEN
                ASSIGN
                    v-cust-name2 = v-cust-name-extent[2]
                    v-cust-name3 = v-cust-name-extent[3]
                    v-shipto1[1] = v-ship1-extent[2]
                    v-shipto2[1] = v-ship1-extent[3]
                    v-shipto1[2] = v-ship2-extent[2]
                    v-shipto2[2] = v-ship2-extent[3]
                    v-shipto1[4] = v-ship4-extent[2]
                    v-shipto2[4] = v-ship4-extent[3].

            DISPLAY v-fill SKIP
                "<B><U>LABEL ITEM" + TRIM(STRING(v-last-j + 1)) + "</U>"  FORMAT "x(22)"
                "<U>LABEL ITEM" + TRIM(STRING(v-last-j + 2)) + "</U>" FORMAT "x(20)" 
                WHEN v-fgitm[2] <> "" AT 55
                "<U>LABEL ITEM" + TRIM(STRING(v-last-j + 3)) + "</U></B>" FORMAT "x(23)" 
                WHEN v-fgitm[3] <> "" AT 107
                SKIP
                "Job#:" v-job-no + "-" + STRING(v-job-no2)
                "Job#:" 
                WHEN v-fgitm[2] <> ""  AT 45
                v-job-no + "-" + STRING(v-job-no2) 
                WHEN v-fgitm[2] <> "" 
                "Job#:" 
                WHEN v-fgitm[3] <> "" AT 90  
                v-job-no + "-" + STRING(v-job-no2) 
                WHEN v-fgitm[3] <> "" 
                SKIP
                "Customer:" v-cust-name 
                "Customer:"  
                WHEN v-fgitm[2] <> ""  AT 45 v-cust-name2  
                WHEN v-fgitm[2] <> "" 
                "Customer:" 
                WHEN v-fgitm[3] <> "" AT 90  v-cust-name3 
                WHEN v-fgitm[3] <> "" 
                SKIP
                "Purchase Order#:" v-pono[1]
                "Purchase Order#:"  
                WHEN v-fgitm[2] <> "" AT 45 v-pono[2]  
                WHEN v-fgitm[2] <> "" 
                "Purchase Order#:" 
                WHEN v-fgitm[3] <> "" AT 90  v-pono[3] 
                WHEN v-fgitm[3] <> "" 
                SKIP
                "<B>FG Item #:</B>" v-fgitm[1]
                "<B>FG Item #:</B>"  
                WHEN v-fgitm[2] <> "" AT 52 v-fgitm[2] 
                WHEN v-fgitm[2] <> "" 
                "<B>FG Item #:</B>" 
                WHEN v-fgitm[3] <> "" AT 104 v-fgitm[3] 
                WHEN v-fgitm[3] <> "" 
                SKIP
                "Description:" v-fgdsc[1]
                "Description:"  
                WHEN v-fgitm[2] <> "" AT 45 v-fgdsc[2]  
                WHEN v-fgitm[2] <> "" 
                "Description:" 
                WHEN v-fgitm[3] <> "" AT 90  v-fgdsc[3] 
                WHEN v-fgitm[3] <> "" 
                SKIP
                "Count:   " v-fgqty[1]
                "Count:   "  
                WHEN v-fgitm[2] <> "" AT 45 v-fgqty[2]  
                WHEN v-fgitm[2] <> "" 
                "Count:   "  
                WHEN v-fgitm[3] <> "" AT 90  v-fgqty[3] 
                WHEN v-fgitm[3] <> ""      
                SKIP
                "Shipto:" 
                WHEN s-prt-shipto v-shipto[1] 
                WHEN s-prt-shipto
                "Shipto:" 
                WHEN s-prt-shipto AND v-fgitm[2] <> "" AT 45 v-shipto1[1] 
                WHEN s-prt-shipto AND v-fgitm[2] <> ""
                "Shipto:" 
                WHEN s-prt-shipto AND v-fgitm[3] <> "" AT 90 v-shipto2[1] 
                WHEN s-prt-shipto AND v-fgitm[3] <> ""
                SKIP
                v-shipto[2] AT 8  
                WHEN s-prt-shipto
                v-shipto1[2] 
                WHEN s-prt-shipto AND v-fgitm[2] <> "" AT 53
                v-shipto2[2] 
                WHEN s-prt-shipto AND v-fgitm[3] <> "" AT 98
                SKIP
                v-shipto[4] AT 8  
                WHEN s-prt-shipto
                v-shipto1[4] 
                WHEN s-prt-shipto AND v-fgitm[2] <> "" AT 53
                v-shipto2[4] 
                WHEN s-prt-shipto AND v-fgitm[3] <> "" AT 98
                SKIP
                WITH FRAME itmlbl2 NO-BOX NO-LABELS STREAM-IO WIDTH 180.
            i = 0.
        END. /* i <= 3 */
    END. /* s-prt-label*/           
    PAGE.
END. /* last-of job-hdr.frm */

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

END. /* for first job-hdr */ 

PROCEDURE find-depth-reftable :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER op-rowid AS ROWID NO-UNDO.

    DEFINE BUFFER b-eb   FOR eb.
    DEFINE BUFFER b-rt   FOR reftable.
    DEFINE BUFFER b-item FOR ITEM.

    FIND b-eb WHERE ROWID(b-eb) EQ ip-rowid NO-LOCK NO-ERROR.

    IF AVAILABLE b-eb THEN 
    DO TRANSACTION:
        FIND FIRST b-rt
            WHERE b-rt.reftable EQ "cedepth"
            AND b-rt.company  EQ b-eb.company
            AND b-rt.loc      EQ b-eb.est-no
            AND b-rt.code     EQ STRING(b-eb.form-no,"9999999999")
            AND b-rt.code2    EQ STRING(b-eb.blank-no,"9999999999")
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE b-rt THEN 
        DO:
            CREATE b-rt.
            ASSIGN
                b-rt.reftable = "cedepth"
                b-rt.company  = b-eb.company
                b-rt.loc      = b-eb.est-no
                b-rt.code     = STRING(b-eb.form-no,"9999999999")
                b-rt.code2    = STRING(b-eb.blank-no,"9999999999").

            IF eb.layer-pad NE "" THEN
            DO:
                FIND FIRST b-item WHERE
                    b-item.company = b-eb.company AND
                    b-item.i-no = b-eb.layer-pad AND
                    b-item.mat-type = "5"
                    NO-LOCK NO-ERROR.

                IF AVAILABLE b-item THEN
                DO:
                    ASSIGN
                        b-rt.val[1] = b-item.case-d
                        b-rt.val[2] = b-item.case-d.
                    RELEASE b-item.
                END.
            END.
        END.

        op-rowid = ROWID(b-rt).
    END.
END PROCEDURE.
    
/*     if v-format eq "Fibre" then page. */

/* end ---------------------------------- copr. 1994  advanced software, inc. */
