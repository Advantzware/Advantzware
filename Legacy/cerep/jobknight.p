/* ------------------------------------------------- cerep/jobintpk.p  */
/*  factory ticket  for folding , Interpack                                   */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAMETER v-format LIKE sys-ctrl.char-fld.

{sys/inc/var.i SHARED}
{sys/form/s-top.f}

{jcrep/r-ticket.i "shared"}

DEF NEW SHARED VAR save_id    AS RECID.
DEF NEW SHARED VAR v-today    AS DATE INIT TODAY.
DEF NEW SHARED VAR v-job      AS CHAR FORMAT "x(6)" EXTENT 2 INIT [" ","zzzzzz"].
DEF NEW SHARED VAR v-job2     AS INT FORMAT "99" EXTENT 2 INIT [00,99].
DEF NEW SHARED VAR v-stypart  LIKE style.dscr.
DEF NEW SHARED VAR v-dsc      LIKE oe-ordl.part-dscr1 EXTENT 2.
DEF NEW SHARED VAR v-size     AS CHAR FORMAT "x(26)" EXTENT 2.
DEF NEW SHARED VAR v-bld-job  LIKE oe-ord.job-no.
DEF NEW SHARED VAR v-bld-job2 LIKE oe-ord.job-no2.
DEF NEW SHARED VAR v-fill     AS CHAR FORMAT "x(128)".
DEF NEW SHARED VAR v-frst     AS LOG.
DEF NEW SHARED VAR v-ok       AS LOG.
DEF NEW SHARED VAR v-est-qty  AS INT FORMAT "->>,>>>,>>9".
DEF NEW SHARED VAR v-job-qty  AS INT FORMAT "->>,>>>,>>9".
DEF NEW SHARED VAR v-fac      AS DEC .
DEF NEW SHARED VAR v-job-no   LIKE oe-ordl.job-no.
DEF NEW SHARED VAR v-job-no2  LIKE oe-ordl.job-no2.
DEF NEW SHARED VAR v-due-date LIKE oe-ord.due-date.
DEF NEW SHARED VAR v-reprint  AS LOG.
DEF NEW SHARED VAR v-up       LIKE eb.num-up.
DEF NEW SHARED VAR v-tandem   AS LOG.
DEF NEW SHARED VAR v-form-no  LIKE eb.form-no FORMAT "99".
DEF NEW SHARED VAR v-fup      AS CHAR.
DEF NEW SHARED VAR v-layout   AS CHAR FORMAT "x(30)".

DEF NEW SHARED BUFFER xjob-hdr FOR job-hdr.

DEF NEW SHARED FRAME head.

DEF SHARED VAR s-prt-mstandard   AS LOG NO-UNDO.
DEF SHARED VAR s-prt-shipto      AS LOG NO-UNDO.
DEF SHARED VAR s-prt-sellprc     AS LOG NO-UNDO.
DEF SHARED VAR s-run-speed       AS LOG NO-UNDO.

DEF NEW SHARED WORKFILE wrk-op
   FIELD m-dscr   LIKE est-op.m-dscr
   FIELD m-code   LIKE est-op.m-code
   FIELD d-seq    LIKE est-op.d-seq
   FIELD dept     LIKE est-op.dept
   FIELD b-num    LIKE est-op.b-num
   FIELD s-num    LIKE est-op.s-num
   FIELD pass     LIKE est-op.op-pass
   FIELD mr       LIKE est-op.op-mr EXTENT 100
   FIELD speed    LIKE est-op.op-speed EXTENT 100
   FIELD run-hr   LIKE job-mch.run-hr EXTENT 100
   FIELD num-sh   LIKE est-op.num-sh EXTENT 100
   FIELD spoil    LIKE job-mch.wst-prct EXTENT 20
   FIELD mr-waste LIKE job-mch.mr-waste EXTENT 20    .

DEF NEW SHARED WORKFILE wrk-die
   FIELD die-no   LIKE eb.die-no
   FIELD cad-no   LIKE eb.cad-no
   FIELD form-no  LIKE eb.form-no
   FIELD die-size AS CHAR FORMAT "x(17)".

DEF NEW SHARED WORKFILE wrk-sheet
   FIELD gsh-qty  LIKE ef.gsh-qty
   FIELD cal      LIKE ef.cal
   FIELD i-no     LIKE ef.board
   FIELD brd-dscr LIKE ef.brd-dscr
   FIELD form-no  LIKE ef.form-no
   FIELD sh-wid   LIKE ef.nsh-len
   FIELD sh-len   LIKE ef.nsh-wid.

DEF NEW SHARED WORKFILE wrk-film
   FIELD form-no  LIKE ef.form-no
   FIELD snum     AS INT FORMAT "99"
   FIELD bnum     AS INT FORMAT "99"
   FIELD leaf     AS CHAR FORMAT "x(10)"
   FIELD leaf-l   AS DEC FORMAT ">9.9999"
   FIELD leaf-w   AS DEC FORMAT ">9.9999".

DEF NEW SHARED WORKFILE wrk-prep
   FIELD code  LIKE est-prep.code
   FIELD dscr  LIKE est-prep.dscr
   FIELD s-num AS INT FORMAT "99"
   FIELD b-num AS INT FORMAT "99"
   FIELD ml    LIKE est-prep.ml.

DEF NEW SHARED WORKFILE wrk-spec
   FIELD form-no  LIKE ef.form-no
   FIELD spec-no  AS CHAR FORMAT "x(10)"
   FIELD dscr     AS CHAR FORMAT "x(20)"
   FIELD qty      AS DEC FORMAT ">>>9.9<<<"
   FIELD uom      AS CHAR FORMAT "x(3)".

DEF NEW SHARED WORKFILE wrk-inst
   FIELD d-seq    LIKE dept.fc
   FIELD dscr     LIKE est-inst.dscr
   FIELD line     LIKE est-inst.line-no
   FIELD rec-id   AS RECID.

DEF NEW SHARED WORKFILE wrk-misc
   FIELD form-no  LIKE ef.form-no
   FIELD snum     AS INT FORMAT "99"
   FIELD bnum     AS INT FORMAT "99"
   FIELD cost     AS CHAR FORMAT "x(20)".

DEF TEMP-TABLE tt-fgitm NO-UNDO
   FIELD i-no        AS CHAR FORM "x(15)"
   FIELD seq         AS INT
   FIELD qty         AS INT 
   FIELD i-dscr      AS CHAR
   FIELD po-no       AS CHAR
   FIELD cust-name   AS CHAR
   FIELD shipto1     AS CHAR
   FIELD shipto2     AS CHAR
   FIELD shipto4     AS CHAR
   FIELD cas-pal     AS INT.  

DEF temp-table w-lo NO-UNDO
   FIELD layout LIKE v-layout.

DEF BUFFER b-eb      FOR eb.
DEF BUFFER b-est     FOR est.
DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER b-oe-rel  FOR oe-rel.
DEF BUFFER b-shipto  FOR shipto.
DEF BUFFER xjob-mat  FOR job-mat.
DEF BUFFER xoe-ordl  FOR oe-ordl.
DEF BUFFER b-cust    FOR cust.
DEF BUFFER b-job-hdr FOR job-hdr. /* rtc */
DEF BUFFER b-item    FOR ITEM.

DEF VAR v-line             AS INT INIT 1 NO-UNDO.
DEF VAR v-gsh-qty          AS INT NO-UNDO.
DEF VAR cnt                AS INT INIT 1 NO-UNDO.
DEF VAR v-frm-blk          AS CHAR FORMAT "x(6)" NO-UNDO.
DEF VAR v-dec              AS DEC NO-UNDO.
DEF VAR v-ovund            AS CHAR FORMAT "x(34)" NO-UNDO.
DEF VAR v-mrhr             AS CHAR FORMAT "x(5)" NO-UNDO.
DEF VAR v-cas-dscr         LIKE item.est-dscr NO-UNDO.
DEF VAR v-first            AS LOG NO-UNDO.
DEF VAR v-spec-list        AS CHAR FORMAT "x(20)"INIT "QA" NO-UNDO.
DEF VAR lv-form-note       AS CHAR NO-UNDO.
DEF VAR v-itm-printed      AS INT NO-UNDO.
DEF VAR v-alloc            AS CHAR NO-UNDO.
DEF VAR v-prep             AS CHAR EXTENT 8 NO-UNDO.
DEF VAR v-misc             AS CHAR EXTENT 6 NO-UNDO.
DEF VAR v-spec-no          AS CHAR EXTENT 8 NO-UNDO.
DEF VAR v-skip             AS LOG NO-UNDO.
DEF VAR v-fill2            AS CHAR INIT "-" FORM "x(125)" NO-UNDO.
DEF VAR li                 AS INT NO-UNDO.
DEF VAR v-cust-name-extent AS CHAR EXTENT 10 NO-UNDO.
DEF VAR v-ship1-extent     AS CHAR EXTENT 10 NO-UNDO.
DEF VAR v-ship2-extent     AS CHAR EXTENT 10 NO-UNDO.
DEF VAR v-ship4-extent     AS CHAR EXTENT 10 NO-UNDO.
DEF VAR v-inst2            AS CHAR EXTENT 20 NO-UNDO.    
DEF VAR v-dept-inst        AS CHAR FORM "x(80)" EXTENT 20 NO-UNDO.
DEF VAR v-spec-inst        AS CHAR FORM "x(80)" EXTENT 10 NO-UNDO.
DEF VAR v-note-length      AS INT INIT 80 NO-UNDO.
DEF VAR v-start-date       AS DATE NO-UNDO.
DEF VAR v-req-date         AS DATE NO-UNDO.
DEF VAR v-shipto           AS CHAR FORMAT "x(30)" EXTENT 4 NO-UNDO.
DEF VAR v-case-size        AS CHAR NO-UNDO.
DEF VAR v-vend             LIKE po-ord.vend-no NO-UNDO.
DEF VAR v-item             AS CHAR EXTENT 20 NO-UNDO.
DEF VAR v-ink1             AS CHAR EXTENT 20 NO-UNDO.
DEF VAR v-ink2             AS CHAR EXTENT 20 NO-UNDO.
DEF VAR v-po-no            LIKE oe-ordl.po-no NO-UNDO.
DEF VAR lv-mat-dept-list   AS CHAR INIT "FB,FS,WN,WS,GL" NO-UNDO.
DEF VAR v-mat-for-mach     AS CHAR NO-UNDO.
DEF VAR v-fgitm            AS CHAR FORM "x(15)" EXTENT 10 NO-UNDO.
DEF VAR v-fgdsc            LIKE eb.part-dscr1 EXTENT 10 NO-UNDO.
DEF VAR v-fgqty            LIKE job-hdr.qty EXTENT 10 FORM ">>,>>>,>>>" NO-UNDO.
DEF VAR v-pono             LIKE oe-ordl.po-no EXTENT 10 NO-UNDO.
DEF VAR v-cas-pal          LIKE eb.cas-pal EXTENT 10 NO-UNDO.
DEF VAR v-num-of-fgitm     AS INT NO-UNDO.
DEF VAR v-board-po         LIKE oe-ordl.po-no-po NO-UNDO.
DEF VAR v-plate-printed    AS LOG NO-UNDO.
DEF VAR v-cust-name        LIKE oe-ord.cust-name NO-UNDO.
DEF VAR v-cust-name2       LIKE oe-ord.cust-name NO-UNDO.
DEF VAR v-cust-name3       LIKE oe-ord.cust-name NO-UNDO.
DEF VAR v-last-j           AS INT NO-UNDO.
DEF VAR v-po-no2           LIKE v-po-no NO-UNDO.
DEF VAR v-po-no3           LIKE v-po-no NO-UNDO.
DEF VAR v-po-duedate       LIKE po-ordl.due-date NO-UNDO.
DEF VAR v-upc-lbl          AS CHAR FORM "x(10)" NO-UNDO.
DEF VAR v-shipto1          AS CHAR FORM "x(30)" EXTENT 4 NO-UNDO.
DEF VAR v-shipto2          AS CHAR FORM "x(30)" EXTENT 4 NO-UNDO.
DEF VAR v-pass-count       AS INT NO-UNDO.
DEF VAR v-last-job         AS CHAR FORMAT "X(6)" NO-UNDO.
DEF VAR v-number-cases     AS INT NO-UNDO.
DEF VAR v-nsh-wid          AS CHAR NO-UNDO.
DEF VAR v-nsh-len          AS CHAR NO-UNDO.
DEF VAR v-trim-w           AS CHAR NO-UNDO.
DEF VAR v-trim-l           AS CHAR NO-UNDO.
DEF VAR v-net-sheet        AS CHAR NO-UNDO.
DEF VAR v-die-size         AS CHAR NO-UNDO.
DEF VAR v-itemfg-i-name    LIKE itemfg.i-name NO-UNDO.
DEF VAR v-itemfg-part-dscr1 LIKE itemfg.part-dscr1 NO-UNDO.
DEF VAR v-up-cnt           AS INT FORMAT ">,>>9" NO-UNDO.
DEF VAR v-case-cnt         AS INT NO-UNDO.
DEF VAR v-ink-lbs          AS DEC FORMAT ">,>>9.9<".
DEF VAR v-job-qty-log      AS LOG NO-UNDO.

{custom/notesdef.i}
{cerep/tt-wrk-ink.i "SHARED" }

FORM HEADER
   SKIP(1)
   "07/22/02 Job Ticket QF-130"   TO 132
   WITH NO-BOX NO-ATTR-SPACE FRAME bott PAGE-BOTTOM STREAM-IO WIDTH 132.

FORMAT HEADER
   "JOB NUMBER:<B>" v-job-no SPACE(0) "-" SPACE(0) v-job-no2 FORMAT "99" SPACE(0) "-" SPACE(0) v-form-no FORMAT "99" "</B>"
   "<B><P12>F A C T O R Y   T I C K E T</B><P10>" AT 52  "JOB START DATE:" AT 123 v-start-date SKIP
    v-fill
    WITH NO-BOX FRAME head NO-LABELS STREAM-IO WIDTH 155.

FORMAT 
   "Customer:" oe-ord.cust-name "Sold To:" oe-ord.sold-id
   "Salesman:" AT 68 oe-ord.sname[1] "Order#:" AT 113 oe-ord.ord-no
   WITH NO-BOX FRAME line-head NO-LABELS STREAM-IO WIDTH 132.
    
{sys/inc/notes.i}

v-fill = "<||3><C1><FROM><C108><LINE><||3>".

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.

FIND FIRST sys-ctrl WHERE
     sys-ctrl.company EQ cocode AND
     sys-ctrl.name EQ "JOB QTY"
     NO-LOCK NO-ERROR.

ASSIGN
   v-job-qty-log = IF AVAIL sys-ctrl THEN sys-ctrl.log-fld ELSE NO
   v-job[1]    = fjob-no
   v-job[2]    = tjob-no
   v-job2[1]   = fjob-no2
   v-job2[2]   = tjob-no2
   v-reprint   = reprint
   v-spec-list = spec-list.

FOR EACH job-hdr NO-LOCK WHERE job-hdr.company               EQ cocode
                   AND job-hdr.job-no                GE substr(fjob-no,1,6)
                   AND job-hdr.job-no                LE substr(tjob-no,1,6)
                   AND FILL(" ",6 - LENGTH(TRIM(job-hdr.job-no))) 
                     + TRIM(job-hdr.job-no) 
                     + STRING(job-hdr.job-no2,"99")  GE fjob-no
                   AND FILL(" ",6 - LENGTH(TRIM(job-hdr.job-no))) 
                     + TRIM(job-hdr.job-no) 
                     + STRING(job-hdr.job-no2,"99")  LE tjob-no
                   and (production OR
               job-hdr.ftick-prnt           eq v-reprint OR
               PROGRAM-NAME(2) MATCHES "*r-tickt2*")
          and can-find(first job where job.company eq cocode
                                   and job.job     eq job-hdr.job
                                   and job.job-no  eq job-hdr.job-no
                                   and job.job-no2 eq job-hdr.job-no2
                                   and job.stat    ne "H"
                                   AND (job.pr-printed EQ reprint OR
                                        NOT production)) USE-INDEX job-no,
   FIRST est WHERE est.company  EQ job-hdr.company
               AND est.est-no   EQ job-hdr.est-no
               AND est.est-type LE 4 NO-LOCK
          BREAK BY job-hdr.job
                BY job-hdr.job-no
                BY job-hdr.job-no2
                BY job-hdr.frm:
   ASSIGN
      v-last-job           = ""
      v-itemfg-i-name      = ""
      v-itemfg-part-dscr1  = "".

   find first job
        where job.company eq cocode
          and job.job     eq job-hdr.job
          and job.job-no  eq job-hdr.job-no
          and job.job-no2 eq job-hdr.job-no2
        no-lock no-error.

    IF production AND
      job.cs-trans-date NE ? THEN DO:
      li = 0.
      DO WHILE li LT 1000:
        li = li + 1.
        FIND CURRENT job EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF AVAIL job THEN
          ASSIGN
           job.pr-printed    = YES
           job.pr-user-id-p  = USERID("nosweat")
           job.pr-print-date = TODAY
           job.pr-print-time = TIME
           li                = 1000.
      END.
    END.

    ELSE DO:
      li = 0.
      IF NOT job-hdr.ftick-prnt THEN DO WHILE li LT 1000:
        li = li + 1.
        FIND xjob-hdr EXCLUSIVE-LOCK
            WHERE ROWID(xjob-hdr) EQ ROWID(job-hdr)
            NO-ERROR NO-WAIT.
        IF AVAIL xjob-hdr THEN
          ASSIGN
           xjob-hdr.ftick-prnt = YES
           li                  = 1000.
      END.

      li = 0.
      DO WHILE li LT 1000:
        li = li + 1.
        FIND CURRENT job EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF AVAIL job THEN DO:
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

   v-est-qty = If AVAIL est THEN est.est-qty[1] ELSE 0.

   FIND FIRST oe-ord WHERE oe-ord.company EQ job-hdr.company
                       AND oe-ord.ord-no  EQ job-hdr.ord-no NO-LOCK NO-ERROR.
   
   IF AVAIL oe-ord THEN
      v-last-job = IF oe-ord.po-no2 NE "" THEN oe-ord.po-no2
                   ELSE STRING(oe-ord.pord-no).

   IF FIRST-OF(job-hdr.frm) THEN 
      v-first = YES.

   /** PRINT JOB HEADER **/
   IF v-first THEN DO: 
      ASSIGN
         v-job-no  = job-hdr.job-no
         v-job-no2 = job-hdr.job-no2.

      IF AVAIL oe-ord THEN
         IF NOT oe-ctrl.p-fact AND (oe-ord.stat EQ "H" OR oe-ord.priceHold) THEN 
            NEXT.

      ASSIGN
         v-due-date = If AVAIL oe-ord THEN oe-ord.due-date ELSE ?
         v-start-date = job-hdr.start-date.

      IF NOT FIRST(job-hdr.job-no) THEN 
         PAGE.

      v-form-no = job-hdr.frm.

      VIEW FRAME head.
    
      v-shipto = "".
     
      FIND FIRST oe-ordl WHERE oe-ordl.company EQ job-hdr.company
                           AND oe-ordl.ord-no  EQ job-hdr.ord-no
                           AND oe-ordl.job-no  EQ job-hdr.job-no
                           AND oe-ordl.job-no2 EQ job-hdr.job-no2
                           AND oe-ordl.i-no    EQ job-hdr.i-no NO-LOCK NO-ERROR.
      IF AVAIL oe-ordl THEN 
         FIND FIRST oe-rel WHERE oe-rel.company EQ cocode
                             AND oe-rel.ord-no  EQ oe-ordl.ord-no
                             AND oe-rel.i-no    EQ oe-ordl.i-no
                             AND oe-rel.line    EQ oe-ordl.line NO-LOCK NO-ERROR. 
      IF AVAIL oe-rel THEN DO:
         FIND FIRST shipto WHERE shipto.company EQ cocode
                             AND shipto.cust-no EQ oe-rel.cust-no
                             AND shipto.ship-id EQ oe-rel.ship-id NO-LOCK NO-ERROR.  
         IF AVAIL shipto THEN
            ASSIGN 
               v-shipto[1] = shipto.ship-name
               v-shipto[2] = shipto.ship-addr[1]
               v-shipto[3] = shipto.ship-addr[2]
               v-shipto[4] = TRIM(oe-rel.ship-city) + ", " + oe-rel.ship-state + "  " + oe-rel.ship-zip.          
      END.
     
      FIND FIRST cust WHERE cust.company = job-hdr.company AND
                            cust.cust-no = job-hdr.cust-no NO-LOCK NO-ERROR.
      ASSIGN
         v-req-date  = IF AVAIL oe-ordl THEN oe-ordl.req-date ELSE ?
         v-cust-name = IF AVAIL oe-ord THEN oe-ord.cust-name ELSE 
                       IF AVAIL cust THEN cust.name ELSE job-hdr.cust-no.

      PUT 
         "<B>Customer Name:</B>" v-cust-name 
         "<B>REQ DATE:   DUE DATE:   Estimate:" SKIP
         "Shipto:</B>" v-shipto[1] v-req-date AT 49 v-due-date AT 61 TRIM(job-hdr.est-no) FORMAT "x(8)" AT 74
         SKIP
         v-shipto[2] AT 7 SKIP
         v-shipto[4] AT 7 SKIP
         v-fill SKIP.     

      v-line = If AVAIL est                            AND
              est.est-type gt 2 AND est.est-type lt 5 THEN 500 else 50.

      /** SUM UP NUMBER OF SHEETS **/
      FIND FIRST job WHERE job.company EQ cocode
                       AND job.job     EQ job-hdr.job
                       AND job.job-no  EQ v-job-no
                       AND job.job-no2 EQ v-job-no2 NO-LOCK NO-ERROR.
      IF AVAIL job THEN
         FOR EACH job-mch WHERE job-mch.company EQ cocode
                            AND job-mch.job     EQ job.job
                            AND job-mch.job-no  EQ job.job-no
                            AND job-mch.job-no2 EQ job.job-no2
                            AND job-mch.frm = job-hdr.frm NO-LOCK,
            FIRST mach {sys/ref/machW.i}
              AND mach.m-code EQ job-mch.m-code NO-LOCK
               BY mach.d-seq
               BY job-mch.frm
               BY job-mch.blank-no
               BY job-mch.pass
               BY job-mch.run-qty DESC:

            FIND FIRST wrk-op WHERE wrk-op.m-code EQ job-mch.m-code
                                AND wrk-op.s-num  EQ job-mch.frm
                                AND wrk-op.b-num  EQ job-mch.blank-no
                                AND wrk-op.pass   EQ job-mch.pass NO-ERROR.
            
            IF NOT AVAIL wrk-op THEN DO:
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
               wrk-op.mr[job-mch.frm]     = job-mch.mr-hr
               wrk-op.speed[job-mch.frm]  = job-mch.speed
               wrk-op.num-sh[job-mch.frm] = job-mch.run-qty
               wrk-op.spoil[job-mch.frm] = job-mch.wst-prct   
               wrk-op.mr-waste[job-mch.frm] = job-mch.mr-waste  
               wrk-op.run-hr[job-mch.frm] = job-mch.run-hr    .
      END. /* FOR EACH job-mch */

      /** BUILD PREP WORK FILE **/
      FOR EACH job-prep WHERE job-prep.company EQ cocode
                          AND job-prep.job     EQ job-hdr.job
                          AND job-prep.job-no  EQ job-hdr.job-no
                          AND job-prep.job-no2 EQ job-hdr.job-no2 NO-LOCK:
         FIND FIRST prep WHERE prep.company EQ cocode
                           AND prep.code    EQ job-prep.CODE NO-LOCK NO-ERROR.
         CREATE wrk-prep.
         ASSIGN
            wrk-prep.code = job-prep.code
            wrk-prep.dscr = If AVAIL prep THEN prep.dscr ELSE ""
            wrk-prep.s-num = job-prep.frm
            wrk-prep.b-num = job-prep.blank-no
            wrk-prep.ml = job-prep.ml.
      END. /* each job-prep */

      If AVAIL est THEN
         FOR EACH est-prep WHERE est-prep.company EQ est.company
                             AND est-prep.est-no  EQ est.est-no
                             AND INDEX("SON",est-prep.simon) GT 0 NO-LOCK:
            FIND FIRST prep WHERE prep.company EQ cocode
                              AND prep.code    EQ est-prep.CODE NO-LOCK NO-ERROR.
            CREATE wrk-prep.
            ASSIGN
               wrk-prep.code  = est-prep.code
               wrk-prep.dscr  = IF AVAIL prep THEN prep.dscr ELSE ""
               wrk-prep.s-num = est-prep.s-num
               wrk-prep.b-num = est-prep.b-num
               wrk-prep.ml    = est-prep.ml.
         END.

      IF AVAIL oe-ord THEN
         FOR EACH oe-ordm WHERE oe-ordm.company EQ cocode
                            AND oe-ordm.ord-no  EQ oe-ord.ord-no NO-LOCK:
            FIND FIRST wrk-prep WHERE wrk-prep.code EQ oe-ordm.charge NO-ERROR.

            IF NOT AVAIL wrk-prep THEN DO:
               FIND FIRST prep WHERE prep.company EQ cocode
                                 AND prep.code    EQ oe-ordm.charge NO-LOCK NO-ERROR.
               CREATE wrk-prep.
               ASSIGN
                  wrk-prep.code  = oe-ordm.charge
                  wrk-prep.dscr  = If AVAIL prep THEN prep.dscr ELSE ""
                  wrk-prep.s-num = 9
                  wrk-prep.b-num = 99
                  wrk-prep.ml    = If AVAIL prep THEN prep.ml ELSE ?.
            END.
         END.
   
      FOR EACH ef WHERE ef.company EQ job-hdr.company
                    AND ef.est-no  EQ job-hdr.est-no
                    AND ef.form-no = job-hdr.frm
               BREAK BY ef.est-no 
                     BY ef.form-no:
         ASSIGN
            v-job-qty = 0
            v-up-cnt  = 0.

         FOR EACH xjob-hdr FIELDS(qty) WHERE
             xjob-hdr.company EQ cocode AND
             xjob-hdr.job     EQ job-hdr.job AND
             xjob-hdr.job-no  EQ job-hdr.job-no AND
             xjob-hdr.job-no2 EQ job-hdr.job-no2 AND
             xjob-hdr.i-no    EQ job-hdr.i-no NO-LOCK:
             v-job-qty = v-job-qty + xjob-hdr.qty.
         END.
       
         v-est-qty = 0.
         
         IF est.est-type EQ 4 THEN
            FOR EACH eb fields(yld-qty) WHERE
                eb.company  EQ ef.company AND
                eb.est-no   EQ ef.est-no AND
                eb.stock-no EQ job-hdr.i-no NO-LOCK:
               v-est-qty = v-est-qty + eb.yld-qty.
            END.
         ELSE 
            v-fac = 1.
     
         v-itm-printed = 0.

         IF ef.form-no EQ job-hdr.frm THEN 
            ebloop:
         
         FOR EACH eb WHERE eb.company     EQ ef.company
                       AND eb.est-no      EQ ef.est-no
                       AND eb.form-no     EQ ef.form-no NO-LOCK
                  BREAK BY eb.form-no 
                        BY eb.blank-no:
            CREATE w-lo.
       
            FOR EACH b-eb WHERE b-eb.company EQ eb.company
                            AND b-eb.est-no  EQ eb.est-no
                            AND b-eb.part-no EQ eb.part-no NO-LOCK 
                       BREAK BY b-eb.est-no:
         
               v-fup = "F" + TRIM(STRING(b-eb.form-no,">>9")) + "-" +
                             TRIM(STRING(b-eb.blank-no,"99")) + "/" +
                             TRIM(STRING(b-eb.num-up,">>9")) + "up".
               IF LENGTH(TRIM(v-fup)) + LENGTH(TRIM(w-lo.layout)) GT 30 THEN DO:
                  SUBSTR(w-lo.layout,LENGTH(TRIM(w-lo.layout)),1) = "".
                  CREATE w-lo.
               END.
               
               w-lo.layout = TRIM(w-lo.layout + " " + TRIM(v-fup) + ",").
               IF LAST(b-eb.est-no) THEN
                  SUBSTR(w-lo.layout,LENGTH(TRIM(w-lo.layout)),1) = "".
            END.
       
            FIND FIRST wrk-die WHERE wrk-die.die-no EQ eb.die-no NO-ERROR.
            IF NOT AVAIL wrk-die AND eb.die-no gt "" THEN DO:
               CREATE wrk-die.
               ASSIGN
                  wrk-die.die-no = eb.die-no
                  wrk-die.cad-no = eb.cad-no
                  wrk-die.form-no = eb.form-no
                  wrk-die.die-size = STRING(ef.trim-w) + "x" + STRING(ef.trim-l).
            END.

/*             /** BUILD INK WORK FILE **/                                                        */
/*             FOR EACH job-mat WHERE job-mat.company EQ cocode                                   */
/*                                AND job-mat.job     EQ job-hdr.job                              */
/*                                AND job-mat.frm     EQ eb.form-no NO-LOCK,                      */
/*                FIRST ITEM {sys/look/itemivW.i}                                                  */
/*                  AND item.i-no EQ job-mat.i-no NO-LOCK:                                        */
/*                                                                                                */
/*                DO i = 1 TO 12:                                                                 */
/*                   IF eb.i-code2[i] EQ job-mat.i-no THEN DO:                                    */
/*                      FIND FIRST wrk-ink WHERE wrk-ink.i-code   EQ eb.i-code2[i]                */
/*                                           AND wrk-ink.form-no  EQ eb.form-no                   */
/*                                           AND wrk-ink.blank-no EQ eb.blank-no                  */
/*                                           AND wrk-ink.i-pass = eb.i-ps2[i] NO-ERROR.           */
/*                      IF NOT AVAIL wrk-ink THEN DO:                                             */
/*                         CREATE wrk-ink.                                                        */
/*                         ASSIGN                                                                 */
/*                            wrk-ink.i-code   = eb.i-code2[i]                                    */
/*                            wrk-ink.form-no  = eb.form-no                                       */
/*                            wrk-ink.blank-no = eb.blank-no                                      */
/*                            wrk-ink.i-dscr   = eb.i-dscr2[i]                                    */
/*                            wrk-ink.i-pass   = eb.i-ps2[i]                                      */
/*                            wrk-ink.ink-lbs  = job-mat.qty.  /* .  rtc*/                        */
/*                      END.                                                                      */
/*                   END.                                                                         */
/*                END. /* loop i */                                                               */
/*                                                                                                */
/*                FIND FIRST wrk-ink WHERE wrk-ink.i-code    EQ job-mat.i-no                      */
/*                                     AND wrk-ink.form-no   EQ job-mat.frm                       */
/*                                     AND (wrk-ink.blank-no EQ job-mat.blank-no                  */
/*                                      OR est.est-type     EQ 4) NO-ERROR.                       */
/*                IF NOT AVAIL wrk-ink AND (job-mat.blank-no  EQ eb.blank-no                      */
/*                                      OR (job-mat.blank-no EQ 0 AND eb.blank-no EQ 1)) THEN DO: */
/*                   CREATE wrk-ink.                                                              */
/*                   ASSIGN                                                                       */
/*                      wrk-ink.i-code   = job-mat.i-no                                           */
/*                      wrk-ink.form-no  = eb.form-no                                             */
/*                      wrk-ink.blank-no = eb.blank-no                                            */
/*                      wrk-ink.i-dscr   = item.est-dscr                                          */
/*                      wrk-ink.i-pass   = 1.                                                     */
/*                END.                                                                            */


            IF eb.est-type EQ 4 THEN 
               v-fac = eb.yld-qty / v-est-qty.
       
            /*  if last-of(eb.form-no) THEN DO: */
            FIND FIRST style WHERE style.company EQ eb.company
                               AND style.style   EQ eb.style NO-LOCK NO-ERROR.
            IF AVAIL style THEN 
               v-stypart = style.dscr.
            
            ASSIGN
               v-dsc[1] = eb.part-dscr1
               v-dsc[2] = eb.part-dscr2
               v-size[1] = STRING(eb.len) + "x" + STRING(eb.wid) + "x" +
                           STRING(eb.dep)
               v-size[2] = eb.i-coldscr.
            
            IF eb.blank-no > 0 AND eb.blank-no < 11 THEN 
               ASSIGN v-fgdsc[eb.blank-no] = eb.part-dscr1.
                                                          
            /*if v-first THEN*/
            v-upc-lbl = IF s-prt-sellprc THEN "Sell Price" ELSE "   UPC#".
            
            IF FIRST-OF(eb.form-no) THEN
               PUT 
                  "<P9><B>F/B   FG Item #           Job Qty  Part#          Description           #UP  Last Job#     Case#        Count   Case/Pal    #Cases " "</B>" 
               SKIP.
         
            ASSIGN
               v-job-qty = 0
               v-number-cases = 0.
            
            FOR EACH xjob-hdr fields(qty) WHERE
                xjob-hdr.company EQ cocode AND
                xjob-hdr.job     EQ job-hdr.job AND
                xjob-hdr.job-no  EQ job-hdr.job-no AND
                xjob-hdr.job-no2 EQ job-hdr.job-no2 AND
                xjob-hdr.i-no    EQ eb.stock NO-LOCK:
               v-job-qty = v-job-qty + xjob-hdr.qty.
            END.

            /** PRINT ITEM **/
            FIND FIRST oe-ordl WHERE oe-ordl.company EQ job-hdr.company
                                 AND oe-ordl.ord-no  EQ job-hdr.ord-no
                                 AND oe-ordl.job-no  EQ job-hdr.job-no
                                 AND oe-ordl.job-no2 EQ job-hdr.job-no2
                                 AND oe-ordl.i-no    EQ eb.stock-no /*job-hdr.i-no*/ NO-LOCK NO-ERROR.
            IF AVAIL oe-ordl THEN DO:
               v-est-qty = oe-ordl.qty.

               FIND FIRST oe-ord of oe-ordl NO-LOCK.

               v-ovund = STRING("Overrun/Underrun %:  " +
                         TRIM(STRING(oe-ordl.over-pct,">>9.99")) + "/" +
                         TRIM(STRING(oe-ordl.under-pct,">>9.99"))).
            END.
            ELSE 
               v-est-qty = v-job-qty.
         
            RELEASE w-lo.
            
            FIND FIRST w-lo NO-ERROR.
            
            ASSIGN
               v-case-size = STRING(eb.cas-len) + "x" + STRING(eb.cas-wid) + "x" +
                             STRING(eb.cas-dep)
               v-up = eb.num-up
               v-up-cnt = v-up-cnt + eb.num-up /* rtc */
               v-po-no = IF AVAIL oe-ordl THEN oe-ordl.po-no ELSE "".

            IF AVAIL oe-ordl AND oe-ordl.cas-cnt > 0  THEN
               v-case-cnt = oe-ordl.cas-cnt.
            ELSE 
               v-case-cnt = eb.cas-cnt.
         
            IF v-job-qty-log THEN
               v-number-cases = job-hdr.qty  / v-case-cnt.
            ELSE IF AVAIL oe-ordl THEN
               v-number-cases = ((job-hdr.qty * (1 + oe-ordl.over-pct)) / v-case-cnt) / 10.

            FIND FIRST itemfg WHERE itemfg.company = eb.company
                                AND itemfg.i-no = eb.stock-no NO-LOCK NO-ERROR.
            IF AVAIL itemfg THEN
               ASSIGN
               v-itemfg-i-name = itemfg.i-name
               v-itemfg-part-dscr1 = itemfg.part-dscr1.

            DISPLAY 
               TRIM(STRING(eb.form-no,">>9")) + "-" + 
               TRIM(STRING(eb.blank-no,">>9")) FORM "x(5)" 
               eb.stock-no @ job-hdr.i-no
               v-job-qty /** v-fac*/ FORMAT "->>,>>>,>>9"
               eb.part-no
               v-itemfg-i-name FORM "x(21)"
               v-up
               v-last-job AT 81
               eb.cas-no  AT 93
               v-case-cnt
               eb.cas-pal AT 117
               v-number-cases
               SKIP
               WITH STREAM-IO WIDTH 190 NO-LABELS NO-BOX FRAME line-det1.
            
            IF v-itemfg-part-dscr1 <> "" THEN
               DISPLAY
                  v-itemfg-part-dscr1 AT 51
                  SKIP
                  WITH STREAM-IO WIDTH 190 NO-LABELS NO-BOX FRAME line-det2.

            v-itm-printed = v-itm-printed + 1.    

            FIND FIRST item WHERE item.company EQ cocode
                              AND item.i-no    EQ eb.cas-no NO-LOCK NO-ERROR.

            v-cas-dscr = IF AVAIL item THEN item.i-name ELSE "".
       
            /* end. /* last-of(eb.form-no) */      */
            IF LAST-OF(eb.form-no) THEN DO:
               IF v-itm-printed < 4 THEN PUT SKIP(4 - v-itm-printed).

               /* Number of sheets */
               RUN oe/rep/ticket3.p (RECID(ef), RECID(job-hdr)).
            
               FIND FIRST wrk-sheet WHERE RECID(wrk-sheet) EQ save_id.
               IF AVAIL oe-ordl THEN
                  FIND FIRST po-ord WHERE po-ord.company = oe-ordl.company
                                      AND po-ord.po-no = int(oe-ordl.po-no-po) NO-LOCK NO-ERROR.
               ASSIGN
                  v-vend = /*IF AVAIL po-ord THEN po-ord.vend-no ELSE "".*/
                        IF AVAIL oe-ordl THEN oe-ordl.vend-no ELSE ""
                  v-board-po = IF AVAIL oe-ordl THEN oe-ordl.po-no-po ELSE 0.
          
               IF AVAIL po-ord THEN
                  FIND FIRST po-ordl WHERE
                       po-ordl.company EQ po-ord.company AND
                       po-ordl.po-no   EQ po-ord.po-no AND
                       po-ordl.i-no = ef.board
                       NO-LOCK NO-ERROR.
          
               v-po-duedate = IF AVAIL po-ordl THEN po-ordl.due-date ELSE ?.

               PUT "<P10>" v-fill SKIP                       /*REQ'D*/
                  "<B>GRADE     CAL     SHEETS    NET SHEET             DIE SIZE             DIE#         TOTAL UP     CAD#</B>"
               SKIP.
               
               /** PRINT SHEET **/
               x = 2.
               
               FOR EACH wrk-sheet BREAK BY wrk-sheet.form-no:
                  IF AVAIL po-ord THEN
                     FIND FIRST po-ordl WHERE
                          po-ordl.company EQ po-ord.company AND
                          po-ordl.po-no   EQ po-ord.po-no AND
                          po-ordl.i-no = wrk-sheet.i-no
                          NO-LOCK NO-ERROR.

                     FIND FIRST b-item WHERE b-item.company EQ cocode
                                         AND b-item.i-no    EQ wrk-sheet.i-no 
                                         AND INDEX("BPR",b-item.mat-type) GT 0 NO-LOCK NO-ERROR.
                     
                  v-po-duedate = IF AVAIL po-ordl THEN po-ordl.due-date ELSE ?.

                  ASSIGN
                     v-net-sheet = ""
                     v-die-size  = "".
                  { sys/inc/fraction.i ef.nsh-wid v-net-sheet }
                  { sys/inc/fraction.i ef.nsh-len v-net-sheet }
                  { sys/inc/fraction.i ef.trim-w v-die-size }
                  { sys/inc/fraction.i ef.trim-l v-die-size }

                  DISPLAY
                     b-item.procat
                     b-ITEM.cal
                     wrk-sheet.gsh-qty AT 18
                     v-net-sheet AT 29 FORM "x(20)"
                     v-die-size  AT 51 FORM "x(20)"
                     eb.die-no
                     v-up-cnt
                     eb.cad-no
                     WITH STREAM-IO WIDTH 170 NO-LABELS NO-BOX FRAME sheet.
                  x = 1.
               END. /* each wrk-sheet */
          
               IF x NE 2 THEN 
                  PUT v-fill AT 1 SKIP.
                  
               /** PRINT INK **/
               PUT 
                  "<B>PASS INK             INK NAME              ITEMS  INK LBS        PASS INK             INK NAME             ITEMS  INK LBS        </B>" 
               SKIP.

               ASSIGN
                  x = 2
                  i = 1
                  v-ink1 = ""
                  v-ink2 = ""
                  v-pass-count = 0.
            
               FOR EACH wrk-ink WHERE wrk-ink.form-no = eb.form-no
                             BREAK BY wrk-ink.i-pass:
                  IF FIRST-OF(wrk-ink.i-pass) THEN
                     v-pass-count = v-pass-count + 1.
               END.
          
               FOR EACH wrk-ink WHERE wrk-ink.form-no = eb.form-no
                             BREAK BY wrk-ink.i-pass
                                   BY wrk-ink.i-code
                                   BY wrk-ink.blank-no:

                  IF FIRST-OF(wrk-ink.i-pass) THEN 
                     i = 1.
                  IF FIRST-OF(wrk-ink.i-code) THEN 
                     ASSIGN
                        v-ink-lbs = 0
                        v-item[i] = "".

                  ASSIGN
                  v-ink-lbs = v-ink-lbs + wrk-ink.i-qty
                  v-item[i] = IF LOOKUP(STRING(wrk-ink.blank-no),v-item[i]) > 0 THEN v-item[i] ELSE v-item[i] + STRING(wrk-ink.blank-no) + ",".

                  IF LAST-OF(wrk-ink.i-code) THEN DO:
                     IF SUBSTRING(v-item[i],LENGTH(v-item[i]),1) = "," THEN 
                        v-item[i] = SUBSTRING(v-item[i],1,LENGTH(v-item[i]) - 1).                    
                  
                     v-alloc = v-item[i].
                  
                     IF NUM-ENTRIES(v-item[i]) GT 1 THEN DO:
                        v-alloc = "".
                        DO j = 1 TO NUM-ENTRIES(v-item[i]):
                           if j eq 1 THEN v-alloc = v-alloc + entry(j,v-item[i]) + ",".
                           ELSE IF j eq num-entries(v-item[i]) THEN
                           DO:
                             if substr(v-alloc,length(trim(v-alloc)),1) eq "-" AND
                                int(entry(j,v-item[i])) - int(entry(j - 1,v-item[i])) GT 1 THEN
                                v-alloc = v-alloc + entry(j - 1,v-item[i]) + ",".

                             v-alloc = v-alloc + entry(j,v-item[i]) + ",".
                           END.
                           ELSE DO:
                              IF INT(ENTRY(j,v-item[i])) - INT(ENTRY(j - 1,v-item[i])) LE 1 THEN
                                 SUBSTR(v-alloc,LENGTH(TRIM(v-alloc)),1) = "-".
                              ELSE DO:
                                 IF SUBSTR(v-alloc,LENGTH(TRIM(v-alloc)),1) EQ "-" THEN
                                    v-alloc = v-alloc + entry(j - 1,v-item[i]) + ",".
                                 v-alloc = v-alloc + entry(j,v-item[i]) + ",".
                              END.
                           END.
                        END.
                    
                        IF v-alloc NE "" THEN 
                           SUBSTR(v-alloc,LENGTH(TRIM(v-alloc)),1) = "".
                     END.                     

                     IF wrk-ink.i-pass = 1 THEN
                        ASSIGN 
                           v-ink1[i] = (IF v-pass-count = 1 THEN "B    " ELSE "F    ") +
                                        STRING(wrk-ink.i-code,"X(15)") + " " + 
                                        STRING(wrk-ink.i-dscr,"x(20)") + "  " + STRING(TRIM(v-alloc), "X(6)") + " " + STRING(round(v-ink-lbs,2), ">>>9.99") /*v-item[i]*/
                           i = i + 1. 
                     
                     ELSE 
                        IF wrk-ink.i-pass = 2 THEN
                           ASSIGN 
                              v-ink2[i] = "B    " + STRING(wrk-ink.i-code,"X(15)") + " " + 
                                                    STRING(wrk-ink.i-dscr,"x(20)") + "  " + STRING(TRIM(v-alloc), "X(6)") + " " + STRING(round(v-ink-lbs,2), ">>>9.99") /*v-item[i]*/
                              i = i + 1.  

                     
                  END. /* IF LAST-OF(wrk-ink.i-code) */
                  /*delete wrk-ink.*/
               END. /* each wrk-ink */
          
               ASSIGN 
                  v-skip = NO.
               
               DO j = 1 TO 12:
                  IF TRIM(v-ink1[j]) = "-" THEN 
                     v-ink1[j] = "".               
                  
                  IF v-ink1[j] <> "" THEN DO:
                     IF v-skip THEN DO:
                        PUT  v-ink1[j] FORM "x(64)" .

                        PUT SKIP.
                     END.
                     ELSE
                        PUT v-ink1[j] FORM "x(64)". 
                     
                     v-skip = NOT v-skip.             
                  END.
               END.

               v-skip = YES.
          
               DO j = 1 TO 12:
                  IF TRIM(v-ink2[j]) = "-" THEN 
                     v-ink2[j] = "".                 
                  IF v-ink2[j] <> "" THEN DO:
                     IF v-skip THEN DO:
                        PUT v-ink2[j] FORM "x(64)".
                        PUT SKIP.
                     END.
                     ELSE
                        PUT v-ink2[j] FORM "x(64)".
                     
                     v-skip = NOT v-skip.
                  END.
               END.          
               PUT v-fill AT 1 SKIP.  
            END. /* last-of(eb.form-no) */
         END. /* each eb */
      END. /* each ef */
   END. /* first job-no */

   IF LAST-OF(job-hdr.frm) THEN DO:
      IF s-run-speed THEN
         PUT 
            "<B>MACHINE                 MR WASTE   MR HRS  RUN SPEED   SPOIL%    MATERIAL FOR MACHINE             SIZE  TOTAL REQUIRED  </B>"
            SKIP.
      ELSE
         PUT 
            "<B>MACHINE                 MR WASTE   MR HRS   RUN HOUR    SPOIL%    MATERIAL FOR MACHINE             SIZE  TOTAL REQUIRED </B>"
            SKIP.

      FOR EACH wrk-op WHERE wrk-op.s-num = job-hdr.frm 
                   BREAK BY wrk-op.d-seq 
                         BY wrk-op.b-num:
         v-mat-for-mach = "".
          
         IF LOOKUP(wrk-op.dept,lv-mat-dept-list) > 0 THEN DO:
            FOR EACH xjob-mat WHERE xjob-mat.company EQ cocode
                                AND xjob-mat.job     EQ job-hdr.job
                                AND xjob-mat.job-no  EQ job-hdr.job-no
                                AND xjob-mat.job-no2 EQ job-hdr.job-no2
                 AND xjob-mat.frm = job-hdr.frm NO-LOCK,
               FIRST ITEM WHERE ITEM.company = cocode 
                            AND ITEM.i-no = xjob-mat.rm-i-no
                            AND ITEM.mat-type = SUBSTRING(wrk-op.dept,1,1) NO-LOCK :
               
               v-mat-for-mach = ITEM.i-name + FILL(" ", 33 - LENGTH(ITEM.i-name))  /*"       " */ +
                                STRING(xjob-mat.wid) + "x" + STRING(xjob-mat.len) +
                                "      " + STRING(xjob-mat.qty).                   
               LEAVE.                 
            END.
         END.
  
/*          IF LAST(wrk-op.d-seq) THEN DO: /* pallet code*/                  */
/*             FOR EACH xjob-mat WHERE xjob-mat.company EQ cocode            */
/*                                 AND xjob-mat.job     EQ job-hdr.job       */
/*                                 AND xjob-mat.job-no  EQ job-hdr.job-no    */
/*                                 AND xjob-mat.job-no2 EQ job-hdr.job-no2   */
/*                                 AND xjob-mat.frm = job-hdr.frm            */
/*                                 AND (xjob-mat.blank-no = job-hdr.blank-no */
/*                                  OR xjob-mat.blank-no = 0) NO-LOCK,       */
/*                FIRST ITEM WHERE ITEM.company = cocode                     */
/*                             AND ITEM.i-no = xjob-mat.rm-i-no              */
/*                             AND ITEM.mat-type = "D" NO-LOCK :             */
/*                v-mat-for-mach = v-mat-for-mach + "      ".                */
/*             END.                                                          */
/*          END.                                                             */
          
         IF s-prt-mstandard THEN DO:
            IF s-run-speed THEN
               PUT 
                  wrk-op.m-dscr   SPACE(5)
                  wrk-op.mr-waste[job-hdr.frm]   SPACE(5)
                  wrk-op.mr[job-hdr.frm]         SPACE(5)
                  wrk-op.speed[job-hdr.frm]      SPACE(5)
                  wrk-op.spoil[job-hdr.frm]      SPACE(5)
                  v-mat-for-mach FORM "x(60)"
                  SKIP.
            ELSE
               PUT 
                  wrk-op.m-dscr   SPACE(5)
                  wrk-op.mr-waste[job-hdr.frm]   SPACE(5)
                  wrk-op.mr[job-hdr.frm]         SPACE(5)
                  wrk-op.run-hr[job-hdr.frm]     SPACE(5)
                  wrk-op.spoil[job-hdr.frm]      SPACE(5)
                  v-mat-for-mach FORM "x(60)"
                  SKIP.
         END.
         ELSE 
            PUT 
               wrk-op.m-dscr   SPACE(5)
               v-mat-for-mach FORM "x(60)"
                   SKIP.
      END. /* each wrk-op*/

      PUT v-fill AT 1 SKIP.
      
      /** PRINT JOB INSTRUCTIONS **/

      /* dept notes*/
      lv-line-chars = 128.
      FIND FIRST job OF job-hdr NO-LOCK NO-ERROR. 
      
      {custom/notespr4.i job v-inst2 20 "notes.rec_key = job.rec_key AND notes.note_code <> '' and LOOKUP(notes.note_code,v-exc-depts) EQ 0 and notes.note_form_no = job-hdr.frm"}
       /*AND notes.note_form_no = job-hdr.frm*/
     
      DO i = 1 TO 20:
        v-dept-inst[i] = v-inst2[i].
      END.
     
 /*  IF v-ship <> "" THEN v-dept-inst[6] = v-ship.  /* shipto notes */ */
      PUT 
         "<B>DEPARTMENT   INSTRUCTION NOTES</B>" SKIP
         v-dept-inst[1] FORM "x(128)" SKIP
         v-dept-inst[2] FORM "x(128)" SKIP
         v-dept-inst[3] FORM "x(128)" SKIP
         v-dept-inst[4] FORM "x(128)" SKIP.

       assign v-spec-no = ""
              v-misc = ""
              v-prep = "".
             

      FIND FIRST ef WHERE ef.company EQ job-hdr.company
                      AND ef.est-no  EQ job-hdr.est-no
                      AND ef.form-no = job-hdr.frm NO-LOCK NO-ERROR.
      IF AVAIL ef THEN DO:
         PUT
            v-fill 
            SKIP
            "<B>Special Materials             Miscellaneous - SubContract                                  Prep</B>"
            SKIP.
         
         j = 1.
         
         DO i = 1 TO 8:
            IF ef.spec-no[i] <> "" THEN ASSIGN  v-spec-no[j] = ef.spec-no[i]
                                               j = j + 1.              
         END.
        
         j = 1.
         
         DO i = 1 TO 6:
            IF ef.mis-cost[i] <> "" THEN
               ASSIGN  
                  v-misc[j] = STRING(ef.mis-snum[i]) + "-" + 
                              STRING(ef.mis-bnum[i]) + ef.mis-cost[i]
                  j = j + 1.                          
         END.
        
         ASSIGN
            j = 1
            v-prep = "".
        
         FOR EACH wrk-prep NO-LOCK WHERE wrk-prep.s-num = ef.form-no 
            BREAK BY wrk-prep.CODE:   
            
            IF FIRST-OF(wrk-prep.code) AND  wrk-prep.CODE <> "" AND j < 9 THEN 
               ASSIGN 
                  v-prep[j] = wrk-prep.CODE + " " + wrk-prep.dscr
                  j = j + 1.
         END.
      END. /* avail ef */
     
      DO i = 1 TO 6:
         IF v-spec-no[i] <> "" THEN 
            PUT v-spec-no[i].
         IF v-misc[i] <> "" THEN 
            PUT v-misc[i] AT 32 FORM "x(30)" .
         IF v-prep[i] <> "" THEN 
            PUT v-prep[i] AT 90.
         IF v-spec-no[i] <> "" OR v-misc[i] <> "" OR v-prep[i] <> "" 
            THEN PUT SKIP.
      END.
     
      DO i = 7 TO 8:
         IF v-spec-no[i] <> "" THEN 
            PUT v-spec-no[i].
         IF v-prep[i] <> "" THEN 
            PUT v-prep[i] AT 90.
         IF v-spec-no[i] <> "" OR v-prep[i] <> "" THEN 
            PUT SKIP.
      END.

      ASSIGN
         i = 1
         v-fgitm = "".
     
      EMPTY TEMP-TABLE tt-fgitm.

      FOR EACH xjob-hdr NO-LOCK WHERE xjob-hdr.company EQ cocode
                                  AND xjob-hdr.job     EQ job-hdr.job
                                  AND xjob-hdr.job-no  EQ job-hdr.job-no
                                  AND xjob-hdr.job-no2 EQ job-hdr.job-no2
                                  AND xjob-hdr.frm     EQ job-hdr.frm 
                                   BY xjob-hdr.blank-no:
        
            FIND FIRST xoe-ordl WHERE xoe-ordl.company EQ xjob-hdr.company
                                  AND xoe-ordl.ord-no  EQ xjob-hdr.ord-no
                                  AND xoe-ordl.job-no  EQ xjob-hdr.job-no
                                  AND xoe-ordl.job-no2 EQ xjob-hdr.job-no2
                                  AND xoe-ordl.i-no    EQ xjob-hdr.i-no NO-LOCK NO-ERROR.
        
            IF AVAIL xoe-ordl THEN v-fgqty[i] = xoe-ordl.cas-cnt.
        
            FIND FIRST b-eb WHERE b-eb.company  = xjob-hdr.company
                              AND b-eb.est-no   = xjob-hdr.est-no
                              AND b-eb.form-no  = xjob-hdr.frm
                              AND b-eb.blank-no = xjob-hdr.blank-no
                              AND b-eb.stock-no = xjob-hdr.i-no NO-LOCK NO-ERROR.
        
            IF NOT AVAIL b-eb THEN 
               FIND FIRST b-eb WHERE b-eb.company EQ xjob-hdr.company
                                 AND b-eb.est-no  EQ xjob-hdr.est-no
                                 AND b-eb.form-no = xjob-hdr.frm
                               /*AND b-eb.blank-no = xjob-hdr.blank-no*/
                                 AND b-eb.stock-no = xjob-hdr.i-no NO-LOCK NO-ERROR.

               FIND FIRST itemfg WHERE itemfg.company = xjob-hdr.company
                                   AND itemfg.i-no = xjob-hdr.i-no NO-LOCK NO-ERROR.

            CREATE tt-fgitm.
            ASSIGN 
               tt-fgitm.i-no = itemfg.part-no
               tt-fgitm.qty = /*xjob-hdr.qty*/
                               IF AVAIL xoe-ordl AND xoe-ordl.cas-cnt <> 0 THEN xoe-ordl.cas-cnt 
                               ELSE eb.cas-cnt 
               tt-fgitm.i-dscr = IF AVAIL itemfg THEN itemfg.i-name ELSE xjob-hdr.i-no
               tt-fgitm.po-no = IF AVAIL xoe-ordl THEN xoe-ordl.po-no ELSE ""
               tt-fgitm.seq = i
               tt-fgitm.cas-pal = b-eb.cas-pal.


            FIND FIRST b-est WHERE b-est.company  EQ xjob-hdr.company 
                               AND b-est.est-no   EQ xjob-hdr.est-no NO-LOCK NO-ERROR.

            IF AVAIL b-est AND b-est.est-type EQ 4 THEN DO: /*combo*/
               FIND FIRST b-cust WHERE b-cust.company EQ xjob-hdr.company
                                   AND b-cust.cust-no EQ xjob-hdr.cust-no NO-LOCK NO-ERROR.

               IF AVAIL b-cust THEN DO:
                  tt-fgitm.cust-name = b-cust.NAME.

                  FIND FIRST b-oe-ordl WHERE b-oe-ordl.company EQ xjob-hdr.company 
                                         AND b-oe-ordl.ord-no  EQ xjob-hdr.ord-no
                                         AND b-oe-ordl.job-no  EQ xjob-hdr.job-no 
                                         AND b-oe-ordl.job-no2 EQ xjob-hdr.job-no2
                                         AND b-oe-ordl.i-no    EQ xjob-hdr.i-no NO-LOCK NO-ERROR.

                  IF AVAIL b-oe-ordl THEN 
                     FIND FIRST b-oe-rel WHERE b-oe-rel.company EQ cocode
                                           AND b-oe-rel.ord-no  EQ b-oe-ordl.ord-no
                                           AND b-oe-rel.i-no    EQ b-oe-ordl.i-no 
                                           AND b-oe-rel.line    EQ b-oe-ordl.LINE NO-LOCK NO-ERROR.

                  IF AVAIL b-oe-rel THEN DO:
                     FIND FIRST b-shipto WHERE b-shipto.company EQ cocode 
                                           AND b-shipto.cust-no EQ b-oe-rel.cust-no 
                                           AND b-shipto.ship-id EQ b-oe-rel.ship-id NO-LOCK NO-ERROR.  
              
                     IF AVAIL b-shipto THEN
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
            i = i + 1.          
            /* IF i > 10 THEN LEAVE.*/
      END.

      IF s-prt-shipto THEN DO i = 1 TO 4:
         ASSIGN 
            v-shipto1[i] = v-shipto[i]
            v-shipto2[i] = v-shipto[i].
      END.
      ASSIGN 
         v-cust-name2 = v-cust-name 
         v-cust-name3 = v-cust-name 
         /* label prints per item */
         i = 0
         j = 0.
     
      FOR EACH tt-fgitm 
         BY tt-fgitm.seq.
         
         IF PAGE-SIZE - LINE-COUNTER < 7 THEN PAGE.

         ASSIGN
            i = i + 1
            v-fgitm[i]     = tt-fgitm.i-no
            v-fgdsc[i]     = tt-fgitm.i-dscr
            v-fgqty[i]     = tt-fgitm.qty
            v-pono[i]      = tt-fgitm.po-no
            v-cas-pal[i]   = tt-fgitm.cas-pal
            v-cust-name-extent[i] = tt-fgitm.cust-name
            v-ship1-extent[i] = tt-fgitm.shipto1
            v-ship2-extent[i] = tt-fgitm.shipto2
            v-ship4-extent[i] = tt-fgitm.shipto4
            j = j + 1.
       
         IF i >= 3 THEN DO:
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
            DISPLAY 
               v-fill 
               SKIP
               "<B><U>LABEL ITEM" + TRIM(STRING(j - 2)) + "</U>"  FORM "x(22)"
               "<U>LABEL ITEM" + TRIM(STRING(j - 1)) + "</U>" FORM "x(20)" WHEN v-fgitm[2] <> "" AT 55
               "<U>LABEL ITEM" + TRIM(STRING(j)) + "</U></B>" FORM "x(23)" WHEN v-fgitm[3] <> "" AT 107
               SKIP
               "Job#:" v-job-no + "-" + STRING(v-job-no2)
               "Job#:" WHEN v-fgitm[2] <> ""  AT 45
               v-job-no + "-" + STRING(v-job-no2)   WHEN v-fgitm[2] <> "" 
               "Job#:" WHEN v-fgitm[3] <> "" AT 90  
               v-job-no + "-" + STRING(v-job-no2) WHEN v-fgitm[3] <> "" 
               SKIP
               "Customer:" v-cust-name 
               "Customer:" WHEN v-fgitm[2] <> "" AT 45 v-cust-name2  WHEN v-fgitm[2] <> "" 
               "Customer:" WHEN v-fgitm[3] <> "" AT 90 v-cust-name3 WHEN v-fgitm[3] <> "" 
               SKIP
               "Purchase Order#:" v-pono[1]
               "Purchase Order#:" WHEN v-fgitm[2] <> "" AT 45 v-pono[2]  WHEN v-fgitm[2] <> "" 
               "Purchase Order#:" WHEN v-fgitm[3] <> "" AT 90 v-pono[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "<B>Part #:</B>" v-fgitm[1]
               "<B>Part #:</B>" WHEN v-fgitm[2] <> "" AT 52  v-fgitm[2] WHEN v-fgitm[2] <> "" 
               "<B>Part #:</B>" WHEN v-fgitm[3] <> "" AT 104 v-fgitm[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "Description:" v-fgdsc[1]
               "Description:" WHEN v-fgitm[2] <> "" AT 45 v-fgdsc[2]  WHEN v-fgitm[2] <> "" 
               "Description:" WHEN v-fgitm[3] <> "" AT 90 v-fgdsc[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "Case Count:   " v-fgqty[1]
               "Case Count:   "  WHEN v-fgitm[2] <> "" AT 45 v-fgqty[2]  WHEN v-fgitm[2] <> "" 
               "Case Count:   "  WHEN v-fgitm[3] <> "" AT 90 v-fgqty[3] WHEN v-fgitm[3] <> ""                               
               SKIP
               "Case/Pal:   " v-cas-pal[1]
               "Case/Pal:   "  WHEN v-fgitm[2] <> "" AT 45 v-cas-pal[2]  WHEN v-fgitm[2] <> "" 
               "Case/Pal:   "  WHEN v-fgitm[3] <> "" AT 90 v-cas-pal[3] WHEN v-fgitm[3] <> ""                               
               SKIP
               "Shipto:" WHEN s-prt-shipto v-shipto[1] WHEN s-prt-shipto
               "Shipto:" WHEN s-prt-shipto AND v-fgitm[2] <> "" AT 45 v-shipto1[1] WHEN s-prt-shipto AND v-fgitm[2] <> ""
               "Shipto:" WHEN s-prt-shipto AND v-fgitm[3] <> "" AT 90 v-shipto2[1] WHEN s-prt-shipto AND v-fgitm[3] <> ""
               SKIP
               v-shipto[2] AT 8  WHEN s-prt-shipto
               v-shipto1[2] WHEN s-prt-shipto AND v-fgitm[2] <> "" AT 53
               v-shipto2[2] WHEN s-prt-shipto AND v-fgitm[3] <> "" AT 98
               SKIP
               v-shipto[4] AT 8  WHEN s-prt-shipto
               v-shipto1[4] WHEN s-prt-shipto AND v-fgitm[2] <> "" AT 53
               v-shipto2[4] WHEN s-prt-shipto AND v-fgitm[3] <> "" AT 98
               SKIP
               WITH FRAME itmlbl NO-BOX NO-LABELS STREAM-IO WIDTH 180.
         
            DOWN WITH FRAME itmlbl.   
            ASSIGN
               i = 0
               v-fgitm[1] = ""
               v-fgdsc[1] = ""
               v-fgqty[1] = 0
               v-fgitm[2] = ""
               v-fgdsc[2] = ""
               v-fgqty[2] = 0
               v-fgitm[3] = ""
               v-fgdsc[3] = ""
               v-fgqty[3] = 0
               v-pono[1]  = ""
               v-pono[2]  = ""
               v-pono[3]  = ""
               v-cas-pal[1] = 0
               v-cas-pal[2] = 0
               v-cas-pal[3] = 0
               v-cust-name-extent[1] = ""
               v-cust-name-extent[2] = ""
               v-cust-name-extent[3] = ""
               v-ship1-extent[1] = ""
               v-ship1-extent[2] = ""
               v-ship1-extent[3] = ""
               v-ship2-extent[1] = ""
               v-ship2-extent[2] = ""
               v-ship2-extent[3] = ""
               v-ship4-extent[1] = ""
               v-ship4-extent[2] = ""
               v-ship4-extent[3] = ""
               v-last-j = j.
         END. /* i = 3 */
      END.
      
      IF i > 0 THEN DO:
         IF PAGE-SIZE - LINE-COUNTER < 9 THEN 
            PAGE.

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

         DISPLAY 
            v-fill 
            SKIP
            "<B><U>LABEL ITEM" + TRIM(STRING(v-last-j + 1)) + "</U>"  FORM "x(22)"
            "<U>LABEL ITEM" + TRIM(STRING(v-last-j + 2)) + "</U>" FORM "x(20)" WHEN v-fgitm[2] <> "" AT 55
            "<U>LABEL ITEM" + TRIM(STRING(v-last-j + 3)) + "</U></B>" FORM "x(23)" WHEN v-fgitm[3] <> "" AT 107
            SKIP
            "Job#:" v-job-no + "-" + STRING(v-job-no2)
            "Job#:" WHEN v-fgitm[2] <> ""  AT 45
             v-job-no + "-" + STRING(v-job-no2) WHEN v-fgitm[2] <> "" 
            "Job#:" WHEN v-fgitm[3] <> "" AT 90  
            v-job-no + "-" + STRING(v-job-no2) WHEN v-fgitm[3] <> "" 
            SKIP
            "Customer:" v-cust-name 
            "Customer:" WHEN v-fgitm[2] <> ""  AT 45 v-cust-name2  WHEN v-fgitm[2] <> "" 
            "Customer:" WHEN v-fgitm[3] <> "" AT 90  v-cust-name3 WHEN v-fgitm[3] <> "" 
            SKIP
            "Purchase Order#:" v-pono[1]
            "Purchase Order#:" WHEN v-fgitm[2] <> "" AT 45 v-pono[2]  WHEN v-fgitm[2] <> "" 
            "Purchase Order#:" WHEN v-fgitm[3] <> "" AT 90  v-pono[3] WHEN v-fgitm[3] <> "" 
            SKIP
            "<B>Part #:</B>" v-fgitm[1]
            "<B>Part #:</B>" WHEN v-fgitm[2] <> "" AT 52 v-fgitm[2] WHEN v-fgitm[2] <> "" 
            "<B>Part #:</B>" WHEN v-fgitm[3] <> "" AT 104 v-fgitm[3] WHEN v-fgitm[3] <> "" 
            SKIP
            "Description:" v-fgdsc[1]
            "Description:" WHEN v-fgitm[2] <> "" AT 45 v-fgdsc[2]  WHEN v-fgitm[2] <> "" 
            "Description:" WHEN v-fgitm[3] <> "" AT 90  v-fgdsc[3] WHEN v-fgitm[3] <> "" 
            SKIP
            "Case Count:   " v-fgqty[1]
            "Case Count:   "  WHEN v-fgitm[2] <> "" AT 45 v-fgqty[2]  WHEN v-fgitm[2] <> "" 
            "Case Count:   "  WHEN v-fgitm[3] <> "" AT 90  v-fgqty[3] WHEN v-fgitm[3] <> ""      
            SKIP
            "Case/Pal:   " v-cas-pal[1]
            "Case/Pal:   "  WHEN v-fgitm[2] <> "" AT 45 v-cas-pal[2] WHEN v-fgitm[2] <> "" 
            "Case/Pal:   "  WHEN v-fgitm[3] <> "" AT 90 v-cas-pal[3] WHEN v-fgitm[3] <> ""   
            SKIP
            "Shipto:" WHEN s-prt-shipto v-shipto[1] WHEN s-prt-shipto
            "Shipto:" WHEN s-prt-shipto AND v-fgitm[2] <> "" AT 45 v-shipto1[1] WHEN s-prt-shipto AND v-fgitm[2] <> ""
            "Shipto:" WHEN s-prt-shipto AND v-fgitm[3] <> "" AT 90 v-shipto2[1] WHEN s-prt-shipto AND v-fgitm[3] <> ""
            SKIP
            v-shipto[2] AT 8  WHEN s-prt-shipto
            v-shipto1[2] WHEN s-prt-shipto AND v-fgitm[2] <> "" AT 53
            v-shipto2[2] WHEN s-prt-shipto AND v-fgitm[3] <> "" AT 98
            SKIP
            v-shipto[4] AT 8  WHEN s-prt-shipto
            v-shipto1[4] WHEN s-prt-shipto AND v-fgitm[2] <> "" AT 53
            v-shipto2[4] WHEN s-prt-shipto AND v-fgitm[3] <> "" AT 98
            SKIP
            WITH FRAME itmlbl2 NO-BOX NO-LABELS STREAM-IO WIDTH 180.
         i = 0.
      END. /* i <= 3 */
   END. /* last-of job-hdr.frm */

   /** PRINT MULT COPIES OF TICKETS **/
   save_id = RECID(job-hdr).
   IF LAST-OF(job-hdr.job-no2) THEN DO:
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
   v-first = no.
END. /* each job-hdr */
 
IF v-format EQ "fibre" THEN 
   PAGE.

/* end ---------------------------------- copr. 1994  advanced software, inc. */
