/* ----------------------------------------------- cerep/jobcolnlp2.p   */
/*  factory ticket  for  corrugated ColonialPL                                 */
/* -------------------------------------------------------------------------- */
DEF INPUT PARAMETER ip-job-no LIKE job-hdr.job-no NO-UNDO.
DEF INPUT PARAMETER ip-job-no2 LIKE job-hdr.job-no2 NO-UNDO.
DEF VAR cSide AS CHAR NO-UNDO.
{sys/inc/var.i shared}
{sys/form/s-top.f}

{jcrep/r-ticket.i "shared"}
{cerep\tt-samp-ctn.i}

DEF NEW SHARED VAR save_id AS RECID.
DEF NEW SHARED VAR v-today AS DATE INIT TODAY.
DEF NEW SHARED VAR v-job AS CHAR FORMAT "x(6)" EXTENT 2 INIT [" ","zzzzzz"].
DEF NEW SHARED VAR v-job2 AS INT FORMAT "99" EXTENT 2 INIT [00,99].
DEF NEW SHARED VAR v-stypart LIKE style.dscr.
DEF NEW SHARED VAR v-dsc LIKE oe-ordl.part-dscr1 EXTENT 2.
DEF NEW SHARED VAR v-size AS CHAR FORMAT "x(26)" EXTENT 2.
DEF NEW SHARED VAR v-bld-job LIKE oe-ord.job-no.
DEF NEW SHARED VAR v-bld-job2 LIKE oe-ord.job-no2.
DEF NEW SHARED VAR v-fill AS CHAR FORMAT "x(128)".
DEF NEW SHARED VAR v-frst AS LOG.
DEF NEW SHARED VAR v-ok AS LOG.
DEF NEW SHARED VAR v-est-qty AS INT FORMAT "->>,>>>,>>9".
DEF NEW SHARED VAR v-job-qty AS INT FORMAT "->>,>>>,>>9".
DEF NEW SHARED VAR v-fac AS DEC .
DEF NEW SHARED VAR v-job-no LIKE oe-ordl.job-no.
DEF NEW SHARED VAR v-job-no2 LIKE oe-ordl.job-no2.
DEF NEW SHARED VAR v-due-date LIKE oe-ord.due-date.
DEF NEW SHARED VAR v-reprint AS LOG.
DEF NEW SHARED VAR v-up LIKE eb.num-up.
DEF NEW SHARED VAR v-tandem AS LOG.
DEF NEW SHARED VAR v-form-no LIKE eb.form-no.
DEF NEW SHARED VAR v-fup AS CHAR.
DEF NEW SHARED VAR v-layout AS CHAR FORMAT "x(30)".
DEF VAR v-case-count LIKE eb.cas-cnt NO-UNDO.
DEF VAR v-case-qty AS INT NO-UNDO.
DEF VAR v-spc-no LIKE eb.spc-no NO-UNDO.
DEF VAR v-gsh-qty AS INT NO-UNDO.
DEF VAR v-frm-blk AS CHAR FORMAT "x(6)" NO-UNDO.
DEF VAR v-dec AS DEC NO-UNDO.
DEF VAR v-ovund AS CHAR FORMAT "x(34)" NO-UNDO.
DEF VAR v-mrhr AS CHAR FORMAT "x(5)".
DEF VAR v-cas-dscr LIKE item.est-dscr.
DEF VAR v-first AS LOG NO-UNDO.
DEF VAR v-spec-list AS CHAR FORMAT "x(20)"INIT "QA" NO-UNDO.
DEF VAR lv-form-note AS cha NO-UNDO.
DEF VAR v-itm-printed AS INT NO-UNDO.
DEF VAR v-prep AS cha EXTENT 8 NO-UNDO.
DEF VAR v-misc AS cha EXTENT 6 NO-UNDO.
DEF VAR v-spec-no AS cha EXTENT 8 NO-UNDO.
DEF VAR v-skip AS LOG NO-UNDO.
DEF VAR v-fill2 AS cha INIT "-" FORM "x(125)" NO-UNDO.
DEF VAR lv-text AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-under-run AS CHAR NO-UNDO.
DEF VAR lv-over-run AS CHAR NO-UNDO.
DEF VAR v-cust-name-extent AS CHAR EXTENT 10 NO-UNDO.
DEF VAR v-ship1-extent AS CHAR EXTENT 10 NO-UNDO.
DEF VAR v-ship2-extent AS CHAR EXTENT 10 NO-UNDO.
DEF VAR v-ship4-extent AS CHAR EXTENT 10 NO-UNDO.
DEF VAR v-po-no LIKE oe-ordl.po-no EXTENT 4 NO-UNDO.
DEF VAR v-unit-per-int LIKE eb.cas-cnt NO-UNDO.
DEF VAR v-unit-per-dec AS DEC NO-UNDO.
DEF VAR v-job-qty-unit-per-int AS INT FORMAT ">>>>>9" NO-UNDO.
DEF VAR v-job-qty-unit-per-dec AS DEC NO-UNDO.
DEF VAR v-dc-gl-speed AS INT NO-UNDO.
DEF VAR v-job-qty-boxes-code-int AS INT NO-UNDO.
DEF VAR v-job-qty-boxes-code-dec AS DEC NO-UNDO.
DEF VAR v-dc-out LIKE est-op.n-out NO-UNDO.
DEF VAR v-dc-only-out LIKE est-op.n-out NO-UNDO.
DEF VAR v-shink-wrap AS LOG NO-UNDO.
DEF VAR v-sample-on-cnt AS LOG NO-UNDO.
DEF VAR v-shrink-wrap AS LOG NO-UNDO.
DEF VAR v-cas-wt AS DEC FORMAT ">>>>9.99" NO-UNDO.
DEF VAR v-cust-lot# AS CHAR EXTENT 4 NO-UNDO.
DEF VAR v-per-ord AS CHAR NO-UNDO.
DEF VAR v-upc-no LIKE eb.upc-no NO-UNDO.
DEF VAR v-pricnt-id AS CHAR NO-UNDO .

DEF BUFFER b-est FOR est.
DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER b-oe-rel FOR oe-rel.
DEF BUFFER b-shipto FOR shipto.
DEF BUFFER b-cust FOR cust.
DEF BUFFER b-rt FOR reftable.
DEF BUFFER ref-side FOR reftable.

DEF TEMP-TABLE w-lo NO-UNDO
  FIELD layout LIKE v-layout.

DEF NEW SHARED BUFFER xjob-hdr FOR job-hdr.

DEF BUFFER b-eb FOR eb.

DEF NEW SHARED WORKFILE wrk-op
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

DEF NEW SHARED WORKFILE wrk-die
  FIELD die-no LIKE eb.die-no
  FIELD cad-no LIKE eb.cad-no
  FIELD form-no LIKE eb.form-no
  FIELD die-size AS CHAR FORMAT "x(17)".

DEF NEW SHARED WORKFILE wrk-sheet
  /*field gsh-qty like ef.gsh-qty*/
  FIELD gsh-qty AS INT  FORMAT "->>>,>>>,>>9" /* gdm - 12180809*/
  FIELD cal LIKE ef.cal
  FIELD i-no LIKE ITEM.i-no
  FIELD brd-dscr LIKE ef.brd-dscr
  FIELD form-no LIKE ef.form-no
  FIELD sh-wid LIKE ef.nsh-len
  FIELD sh-len LIKE ef.nsh-wid.

DEF NEW SHARED WORKFILE wrk-film
  FIELD form-no LIKE ef.form-no
  FIELD snum AS INT FORMAT "99"
  FIELD bnum AS INT FORMAT "99"
  FIELD leaf AS CHAR FORMAT "x(10)"
  FIELD leaf-l AS DEC FORMAT ">9.9999"
  FIELD leaf-w AS DEC FORMAT ">9.9999".

DEF NEW SHARED WORKFILE wrk-ink
  FIELD i-code AS CHAR FORMAT "x(10)"
  FIELD form-no LIKE eb.form-no
  FIELD blank-no LIKE eb.blank-no
  FIELD i-dscr AS CHAR FORMAT "x(20)"
  FIELD i-qty AS DEC FORMAT ">,>>9.9<"
  FIELD i-pass AS DEC
  FIELD i-unit AS INT
  FIELD i-side AS CHAR.

DEF NEW SHARED WORKFILE wrk-prep
  FIELD code LIKE est-prep.code
  FIELD dscr LIKE est-prep.dscr
  FIELD s-num AS INT FORMAT "99"
  FIELD b-num AS INT FORMAT "99"
  FIELD ml LIKE est-prep.ml.

DEF NEW SHARED WORKFILE wrk-spec
  FIELD form-no LIKE ef.form-no
  FIELD spec-no AS CHAR FORMAT "x(10)"
  FIELD dscr AS CHAR FORMAT "x(20)"
  FIELD qty AS DEC FORMAT ">>>9.9<<<"
  FIELD uom AS CHAR FORMAT "x(3)".

DEF NEW SHARED WORKFILE wrk-inst
  FIELD d-seq LIKE dept.fc
  FIELD dscr LIKE est-inst.dscr
  FIELD line LIKE est-inst.line-no
  FIELD rec-id AS RECID.

DEF NEW SHARED WORKFILE wrk-misc
  FIELD form-no LIKE ef.form-no
  FIELD snum AS INT FORMAT "99"
  FIELD bnum AS INT FORMAT "99"
  FIELD cost AS CHAR FORMAT "x(20)".
  
{custom/formtext.i NEW}     
{custom/notesdef.i}
DEF VAR v-inst2 AS cha EXTENT 25 NO-UNDO.    
DEF VAR v-dept-inst AS cha FORM "x(80)" EXTENT 20 NO-UNDO.
DEF VAR v-note-length AS INT INIT 80 NO-UNDO.

DEF VAR v-start-date AS DATE NO-UNDO.
DEF VAR v-req-date AS DATE NO-UNDO.
DEF VAR v-shipto AS cha FORMAT "x(30)" EXTENT 4 NO-UNDO.
DEF VAR v-case-size AS cha NO-UNDO.
DEF VAR v-vend LIKE po-ord.vend-no NO-UNDO.
DEF VAR v-item AS cha EXTENT 100 NO-UNDO.
DEF VAR v-i-qty AS DEC EXTENT 100 NO-UNDO.
DEF VAR v-ink1 AS cha EXTENT 100 NO-UNDO.
DEF VAR v-ink2 AS cha EXTENT 100 NO-UNDO.
DEF VAR lv-mat-dept-list AS cha INIT "FB,FS,WN,WS,GL" NO-UNDO.
DEF VAR v-mat-for-mach AS cha NO-UNDO.
DEF BUFFER xjob-mat FOR job-mat.
DEF VAR v-fgitm AS cha FORM "x(15)" EXTENT 10 NO-UNDO.
DEF VAR v-fgdsc LIKE eb.part-dscr1 EXTENT 10 NO-UNDO.
DEF VAR v-fgqty LIKE job-hdr.qty EXTENT 10 FORM ">>,>>>,>>>" NO-UNDO.
DEF VAR v-pono LIKE oe-ordl.po-no EXTENT 10 NO-UNDO.
DEF VAR v-num-of-fgitm AS INT NO-UNDO.
DEF TEMP-TABLE tt-fgitm FIELD i-no AS cha FORM "x(15)"
                        FIELD seq AS INT
                        FIELD qty AS INT 
                        FIELD i-dscr AS cha
                        FIELD po-no AS cha
                        FIELD cust-name AS cha
                        FIELD shipto1 AS CHAR
                        FIELD shipto2 AS CHAR
                        FIELD shipto4 AS CHAR.
DEF VAR v-board-po LIKE oe-ordl.po-no-po NO-UNDO.
DEF VAR v-plate-printed AS LOG NO-UNDO.
DEF BUFFER xoe-ordl FOR oe-ordl.
DEF VAR v-cust-name LIKE oe-ord.cust-name NO-UNDO.
DEF VAR v-cust-name2 LIKE oe-ord.cust-name NO-UNDO.
DEF VAR v-cust-name3 LIKE oe-ord.cust-name NO-UNDO.
DEF VAR v-last-j AS INT NO-UNDO.
DEF VAR v-po-no2 LIKE v-po-no NO-UNDO.
DEF VAR v-po-no3 LIKE v-po-no NO-UNDO.
DEF VAR v-lbs AS DEC FORM ">>,>>>,>>9" NO-UNDO.
DEF VAR v-dept-title AS cha NO-UNDO.
DEF VAR v-dept-note-printed AS LOG.
/* aj */
DEF VAR v-ship-date AS DATE EXTENT 4 NO-UNDO.
DEF VAR v-due-qty  LIKE oe-rel.tot-qty  EXTENT 4  NO-UNDO.
DEF VAR icount AS INT INIT 0         NO-UNDO.
DEF VAR v-max-qty AS INT NO-UNDO .
DEF VAR v-min-qty AS INT   NO-UNDO .
DEFINE VARIABLE v-reprun AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-brd-code AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-item-desc AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-weight LIKE job-mat.rm-i-no   NO-UNDO.
DEFINE VARIABLE v-width LIKE job-mat.basis-w   NO-UNDO.
DEFINE VARIABLE v-lenght AS DECIMAL    NO-UNDO.
DEFINE VARIABLE v-print-qty AS DECIMAL    NO-UNDO.
DEFINE VARIABLE v-print-feet AS DECIMAL FORMAT "->>>,>>>,>>9.99"   NO-UNDO.

v-fill = "<||3><C2><FROM><C108><LINE><||3>".

DEF NEW SHARED FRAME head.

DEF SHARED VAR s-prt-mstandard AS LOG NO-UNDO.
DEF SHARED VAR s-prt-shipto AS LOG NO-UNDO.
DEF SHARED VAR s-prt-sellprc AS LOG NO-UNDO.
DEF VAR v-po-duedate LIKE po-ordl.due-date NO-UNDO.
DEF VAR v-upc-lbl AS cha FORM "x(10)" NO-UNDO.
DEF VAR v-shipto1 AS cha FORM "x(30)" EXTENT 4 NO-UNDO.
DEF VAR v-shipto2 AS cha FORM "x(30)" EXTENT 4 NO-UNDO.
DEF SHARED VAR s-run-speed AS LOG NO-UNDO.
DEF VAR v-pass-count AS INT NO-UNDO.
DEF SHARED VAR s-prt-label AS LOG NO-UNDO.
DEF VAR v-boardcode LIKE job-mat.rm-i-no NO-UNDO.  
DEF VAR v-length    LIKE job-mat.len NO-UNDO.
DEFINE VARIABLE v-upnew AS DECIMAL    NO-UNDO.
DEFINE VARIABLE v-lp-dep AS DECIMAL    NO-UNDO.
DEFINE VARIABLE v-lp-qty AS INTEGER    NO-UNDO.
DEF VAR v-mr-hours AS DEC NO-UNDO.
DEF VAR v-pr-speed AS INT NO-UNDO.

DEF VAR cDraftImage AS cha NO-UNDO.
DEF VAR cDraftImageFull AS cha FORM "x(50)" NO-UNDO.
DEF VAR cJobNo AS CHAR NO-UNDO.

ASSIGN cDraftImage = "images\draft.jpg"

FILE-INFO:FILE-NAME = cDraftImage.
cDraftImageFull = IF lDraft 
                    THEN  "<C25><#1><R+80><C+50><IMAGE#1=" + FILE-INFO:FULL-PATHNAME + ">" 
                    ELSE "".

FORMAT HEADER 
       cDraftImageFull FORMAT "x(100)" SKIP
       "<R1><C68><FROM><AT=+.3,+1.7><BARCODE,TYPE=39,CHECKSUM=NONE,BarHeightPixels=2,VALUE=" cJobNo FORMAT "x(9)" /*v-job-no space(0) "-" space(0) v-job-no2 format "99"*/ ">"
/*        "<AT=-.5,6.3><FROM><AT=+.3,+1.7><BARCODE,TYPE=39,CHECKSUM=NONE,BarHeightPixels=2,VALUE=" cJobNo FORMAT "x(9)" /*v-job-no space(0) "-" space(0) v-job-no2 format "99"*/ ">" */
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
 v-reprint   = reprint
 v-spec-list = spec-list.

FOR  EACH job-hdr NO-LOCK
        WHERE job-hdr.company               EQ cocode
          AND job-hdr.job-no                EQ ip-job-no
          AND job-hdr.job-no2               EQ ip-job-no2
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

        FIRST est
        WHERE est.company  EQ job-hdr.company
          AND est.est-no   EQ job-hdr.est-no
          AND est.est-type LE 4  
        NO-LOCK

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
         IF AVAIL job AND job.stat EQ "H" THEN DO:
            ASSIGN cDraftImage = "images\on-hold.jpg"
                FILE-INFO:FILE-NAME = cDraftImage.
            cDraftImageFull = "<C25><#1><R+80><C+50><IMAGE#1=" + FILE-INFO:FULL-PATHNAME + ">"  .
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
      
      v-est-qty = IF AVAIL est THEN est.est-qty[1] ELSE 0.
      FIND FIRST oe-ord WHERE oe-ord.company EQ job-hdr.company
                          AND oe-ord.ord-no  EQ job-hdr.ord-no NO-LOCK NO-ERROR.

      IF FIRST-OF(job-hdr.frm) THEN v-first = YES.

      /** PRINT JOB HEADER **/
      IF v-first THEN DO:
        ASSIGN
         v-job-no  = job-hdr.job-no
         v-job-no2 = job-hdr.job-no2
         cJobNo = v-job-no + "-" + string(v-job-no2,"99").

        IF AVAIL oe-ord THEN
          IF NOT oe-ctrl.p-fact AND oe-ord.stat EQ "H" THEN NEXT.

          FIND FIRST cust WHERE cust.company = job-hdr.company AND
                  cust.cust-no = job-hdr.cust-no NO-LOCK NO-ERROR.

          IF AVAIL cust THEN DO:
              ASSIGN v-pricnt-id = "" . 
              FOR EACH empalert WHERE empalert.table_rec_key = cust.rec_key NO-LOCK,
                  FIRST users WHERE users.user_id = empalert.user-id NO-LOCK:

                  IF empalert.spare-char-1 EQ "YES" THEN DO:
                      ASSIGN v-pricnt-id = users.USER_id .
                      LEAVE.
                  END.
              END.
          END.   

        ASSIGN
           /*v-due-date = if avail oe-ord then oe-ord.due-date else ?*/
           v-start-date = job-hdr.start-date.

        IF NOT FIRST(job-hdr.job-no) THEN PAGE.
        
        v-shipto = "".
        FIND FIRST oe-ordl
                WHERE oe-ordl.company EQ job-hdr.company
                  AND oe-ordl.ord-no  EQ job-hdr.ord-no
                  AND oe-ordl.job-no  EQ job-hdr.job-no
                  AND oe-ordl.job-no2 EQ job-hdr.job-no2
                  AND oe-ordl.i-no    EQ job-hdr.i-no
                NO-LOCK NO-ERROR.
        IF AVAIL oe-ordl THEN 
            FIND FIRST oe-rel
            WHERE oe-rel.company EQ cocode
              AND oe-rel.ord-no  EQ oe-ordl.ord-no
              AND oe-rel.i-no    EQ oe-ordl.i-no
              AND oe-rel.line    EQ oe-ordl.line
            NO-LOCK NO-ERROR.
        ASSIGN 
              v-reprun =  IF AVAIL oe-ordl AND oe-ordl.type-code = "R" THEN "RETURN" 
                          ELSE "NEW" .
        
        VIEW FRAME head.
        IF AVAIL oe-rel THEN DO:
           v-po-no = oe-rel.po-no .

           FIND FIRST reftable WHERE
                      reftable.reftable EQ "oe-rel.lot-no" AND
                      reftable.company  EQ STRING(oe-rel.r-no,"9999999999")
                      NO-LOCK NO-ERROR.
           IF AVAIL reftable THEN 
              ASSIGN v-cust-lot# = reftable.CODE.
           ELSE
             ASSIGN v-cust-lot# = "".

           FIND FIRST shipto
              WHERE shipto.company EQ cocode
                AND shipto.cust-no EQ oe-rel.cust-no
                AND shipto.ship-id EQ oe-rel.ship-id
              NO-LOCK NO-ERROR.  
          IF AVAIL shipto THEN
              ASSIGN v-shipto[1] = shipto.ship-name
                     v-shipto[2] = shipto.ship-addr[1]
                     v-shipto[3] = shipto.ship-addr[2]
                     v-shipto[4] = TRIM(oe-rel.ship-city) + ", " +
                                   oe-rel.ship-state + "  " + oe-rel.ship-zip.  

          END.
          /* aj */
          icount = 0.
          FOR EACH b-oe-rel WHERE  b-oe-rel.company EQ cocode
                                AND b-oe-rel.ord-no  EQ oe-rel.ord-no
                                AND b-oe-rel.i-no    EQ oe-rel.i-no
                                AND b-oe-rel.line    EQ oe-rel.line  
                                NO-LOCK :

             FIND FIRST reftable WHERE
                reftable.reftable EQ "oe-rel.lot-no" AND
                reftable.company  EQ STRING(b-oe-rel.r-no,"9999999999")
                        NO-LOCK NO-ERROR.
           
              icount =  icount + 1 .
              IF icount = 1 THEN  
                 ASSIGN 
                    v-ship-date[1] = IF b-oe-rel.rel-date <> ?  THEN  b-oe-rel.rel-date ELSE ? 
                    v-due-qty[1]   = IF b-oe-rel.tot-qty <> 0 THEN b-oe-rel.tot-qty ELSE 0 
                    v-po-no[1]     = IF b-oe-rel.po-no <> "" THEN  b-oe-rel.po-no ELSE ""
                    v-cust-lot#[1] = IF AVAIL reftable THEN reftable.CODE ELSE "" .
                      
             IF icount = 2 THEN  
                ASSIGN 
                  v-ship-date[2] = IF b-oe-rel.rel-date <> ?  THEN  b-oe-rel.rel-date ELSE ? 
                  v-due-qty[2]  =  IF b-oe-rel.tot-qty <> 0 THEN b-oe-rel.tot-qty ELSE 0 
                  v-po-no[2]   = IF b-oe-rel.po-no <> "" THEN  b-oe-rel.po-no ELSE "" 
                  v-cust-lot#[2] = IF AVAIL reftable THEN reftable.CODE ELSE ""   .

             IF icount = 3 THEN  
                ASSIGN 
                 v-ship-date[3] = IF b-oe-rel.rel-date <> ?  THEN  b-oe-rel.rel-date ELSE ? 
                 v-due-qty[3]  =  IF b-oe-rel.tot-qty <> 0 THEN b-oe-rel.tot-qty ELSE 0 
                 v-po-no[3]   = IF b-oe-rel.po-no <> "" THEN  b-oe-rel.po-no ELSE ""   
                  v-cust-lot#[3] = IF AVAIL reftable THEN reftable.CODE ELSE "" .

             IF icount = 4 THEN  
                ASSIGN 
                   v-ship-date[4] = IF b-oe-rel.rel-date <> ?  THEN  b-oe-rel.rel-date ELSE ?  
                   v-due-qty[4]  =  IF b-oe-rel.tot-qty <> 0 THEN b-oe-rel.tot-qty ELSE 0 
                   v-po-no[4]    =  IF b-oe-rel.po-no <> "" THEN  b-oe-rel.po-no ELSE ""   
                    v-cust-lot#[4] = IF AVAIL reftable THEN reftable.CODE ELSE "" . 
            
          END. /* FOR EACH */
        
        FIND FIRST cust WHERE cust.company = job-hdr.company AND
                              cust.cust-no = job-hdr.cust-no NO-LOCK NO-ERROR.

        ASSIGN
           v-req-date = IF AVAIL oe-ordl THEN oe-ordl.req-date ELSE ?
           v-cust-name = IF AVAIL oe-ord THEN oe-ord.cust-name 
                         ELSE IF AVAIL cust THEN cust.name
                         ELSE job-hdr.cust-no
           lv-over-run = IF AVAIL oe-ordl THEN TRIM(STRING(oe-ordl.over-pct,">>9.99")) ELSE
                         IF AVAIL oe-ord  THEN TRIM(STRING(oe-ord.over-pct,">>9.99"))  ELSE ""
           lv-under-run = IF AVAIL oe-ordl THEN TRIM(STRING(oe-ordl.under-pct,">>9.99")) ELSE
                          IF AVAIL oe-ord  THEN TRIM(STRING(oe-ord.under-pct,">>9.99"))  ELSE ""
           v-due-date = IF AVAIL oe-ordl THEN oe-ordl.prom-date ELSE ? .
            IF AVAIL oe-ord THEN
            v-per-ord   = IF oe-ord.po-no2 NE "" THEN oe-ord.po-no2 ELSE STRING(oe-ord.pord-no) .
            IF AVAIL oe-ord AND oe-ord.TYPE EQ "T" AND oe-ord.pord-no GT 0 THEN
               v-per-ord = STRING(oe-ord.pord-no).
      
       FIND FIRST eb WHERE eb.company     EQ job-hdr.company
                        AND eb.est-no      EQ job-hdr.est-no
                        AND eb.form-no     EQ job-hdr.frm
                        AND eb.stock-no = job-hdr.i-no NO-LOCK NO-ERROR.
        IF NOT AVAIL eb THEN FIND FIRST eb WHERE eb.company     EQ job-hdr.company
                        AND eb.est-no      EQ job-hdr.est-no
                        AND eb.form-no     EQ job-hdr.frm
                        AND eb.blank-no > 0 NO-LOCK NO-ERROR.
        v-spc-no = IF AVAIL eb THEN eb.spc-no ELSE "".
        v-upc-no = IF AVAIL eb THEN eb.upc-no ELSE "".

        PUT "<B> Customer Name:</B>" v-cust-name FORM "x(25)" "<B>Acct Code:</B> " job-hdr.cust-no 
            "<B> REL. DATE:    QTY DUE:  PO#:         Customer Lot#:    Print Date:" SKIP
            " Shipto:</B>" v-shipto[1] SPACE(2) "Prev.Ord#:" v-per-ord v-ship-date[1] AT 65 v-due-qty[1] AT 75  v-po-no[1] FORMAT "x(12)" AT 89 v-cust-lot#[1] AT 102 FORM "x(15)" TODAY FORMAT "99/99/9999" AT 120 SKIP  
            v-shipto[2] AT 9 SPACE(2) "MFG DATE:" v-due-date v-ship-date[2] AT 61 v-due-qty[2] AT 71 v-po-no[2] FORM "x(12)" AT 85 v-cust-lot#[2] AT 98 FORM "x(15)"  STRING(TIME,"HH:MM am/pm") AT 115 " by " USERID("nosweat")   SKIP  
            v-shipto[3] AT 9 "<B>QC/SPC#</B>:" AT 41 v-spc-no  FORM "x(10)" SPACE(2) v-ship-date[3] SPACE(2) 
            v-due-qty[3] SPACE(3) v-po-no[3] FORM "x(12)" SPACE(1) v-cust-lot#[3] FORM "x(15)" SPACE(3) "<B>Estimate:</B>" /*AT 116*/  SKIP 
            v-shipto[4] AT 9 "Pharma Code:" AT 41 v-upc-no  /*v-ship-date[4] AT 61 v-due-qty[4] AT 71 v-po-no[4] FORM "x(12)" AT 85 v-cust-lot#[4]  AT 98 FORM "x(15)"*/ TRIM(job-hdr.est-no) AT 116 SKIP 
            v-fill SKIP.

        /** SUM UP NUMBER OF SHEETS **/
        FIND FIRST job
            WHERE job.company EQ cocode
              AND job.job     EQ job-hdr.job
              AND job.job-no  EQ v-job-no
              AND job.job-no2 EQ v-job-no2
            NO-LOCK NO-ERROR.
            
        IF AVAIL job THEN
        FOR EACH job-mch
            WHERE job-mch.company EQ cocode
              AND job-mch.job     EQ job.job
              AND job-mch.job-no  EQ job.job-no
              AND job-mch.job-no2 EQ job.job-no2
              AND job-mch.frm = job-hdr.frm
            NO-LOCK,

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
           wrk-prep.code = job-prep.code
           wrk-prep.dscr = IF AVAIL prep THEN prep.dscr ELSE ""
           wrk-prep.s-num = job-prep.frm
           wrk-prep.b-num = job-prep.blank-no
           wrk-prep.ml = job-prep.ml.
        END. /* each job-prep */

        IF AVAIL est THEN
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
           wrk-prep.dscr  = IF AVAIL prep THEN prep.dscr ELSE ""
           wrk-prep.s-num = est-prep.s-num
           wrk-prep.b-num = est-prep.b-num
           wrk-prep.ml    = est-prep.ml.
        END.

        IF AVAIL oe-ord THEN
        FOR EACH oe-ordm 
            WHERE oe-ordm.company EQ cocode
              AND oe-ordm.ord-no  EQ oe-ord.ord-no
            NO-LOCK:
          FIND FIRST wrk-prep WHERE wrk-prep.code EQ oe-ordm.charge NO-ERROR.
          IF NOT AVAIL wrk-prep THEN DO:
            FIND FIRST prep
                WHERE prep.company EQ cocode
                  AND prep.code    EQ oe-ordm.charge
                NO-LOCK NO-ERROR.
            CREATE wrk-prep.
            ASSIGN
             wrk-prep.code  = oe-ordm.charge
             wrk-prep.dscr  = IF AVAIL prep THEN prep.dscr ELSE ""
             wrk-prep.s-num = 9
             wrk-prep.b-num = 99
             wrk-prep.ml    = IF AVAIL prep THEN prep.ml ELSE ?.
          END.
        END.
      

      FOR EACH ef
          WHERE ef.company EQ job-hdr.company
            AND ef.est-no  EQ job-hdr.est-no
            AND ef.form-no = job-hdr.frm
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
          
        v-est-qty = 0.
        IF est.est-type EQ 4 THEN
        FOR EACH eb
            WHERE eb.company  EQ ef.company
              AND eb.est-no   EQ ef.est-no
              AND eb.stock-no EQ job-hdr.i-no
            NO-LOCK:
          v-est-qty = v-est-qty + eb.yld-qty.
        END.

        ELSE v-fac = 1.
        v-itm-printed = 0.

        IF ef.form-no EQ job-hdr.frm THEN ebloop:
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
            IF LENGTH(TRIM(v-fup)) + length(TRIM(w-lo.layout)) GT 30 THEN DO:
              substr(w-lo.layout,LENGTH(TRIM(w-lo.layout)),1) = "".
              CREATE w-lo.
            END.
            w-lo.layout = TRIM(w-lo.layout + " " + trim(v-fup) + ",").
            IF LAST(b-eb.est-no) THEN
              substr(w-lo.layout,LENGTH(TRIM(w-lo.layout)),1) = "".
          END.
          
          FIND FIRST wrk-die WHERE wrk-die.die-no EQ eb.die-no NO-ERROR.
          IF NOT AVAIL wrk-die AND eb.die-no GT "" THEN DO:
            CREATE wrk-die.
            ASSIGN wrk-die.die-no = eb.die-no
                   wrk-die.cad-no = eb.cad-no
              wrk-die.form-no = eb.form-no
              wrk-die.die-size = STRING(ef.trim-w) + "x" +
              string(ef.trim-l).
          END.

          /** BUILD INK WORK FILE **/
          FIND FIRST reftable WHERE
               reftable.reftable EQ "ce/v-est3.w Unit#" AND
               reftable.company EQ eb.company AND
               reftable.loc     EQ eb.est-no AND
               reftable.code    EQ STRING(eb.form-no,"9999999999") AND
               reftable.code2   EQ STRING(eb.blank-no,"9999999999")
               NO-LOCK NO-ERROR.

          FIND FIRST b-rt WHERE
               b-rt.reftable EQ "ce/v-est3.w Unit#1" AND
               b-rt.company  EQ b-eb.company AND
               b-rt.loc      EQ eb.est-no AND
               b-rt.code     EQ STRING(eb.form-no,"9999999999") AND
               b-rt.code2    EQ STRING(eb.blank-no,"9999999999")
               NO-LOCK NO-ERROR.

          FOR EACH job-mat
              WHERE job-mat.company EQ cocode
                AND job-mat.job     EQ job-hdr.job
                AND job-mat.frm     EQ eb.form-no
              NO-LOCK,
              FIRST item
              {sys/look/itemivW.i}
                AND item.i-no EQ job-mat.i-no
              NO-LOCK:

            DO i = 1 TO 20:
               IF eb.i-code2[i] EQ job-mat.i-no THEN DO:

                   cSide = "".
                   IF AVAIL(reftable) THEN
                     cSide = FILL(" ",5) + SUBSTRING(reftable.dscr,i,1).
                   FIND FIRST wrk-ink WHERE wrk-ink.i-code = eb.i-code2[i]
                     AND wrk-ink.form-no  = eb.form-no
                     AND wrk-ink.blank-no = eb.blank-no
                     AND (wrk-ink.i-side EQ cSide OR cSide EQ "")
                   NO-ERROR.
                  IF NOT AVAIL wrk-ink THEN DO:
                  
                    CREATE wrk-ink.
                    ASSIGN
                     wrk-ink.i-code   = eb.i-code2[i]
                     wrk-ink.form-no  = eb.form-no
                     wrk-ink.blank-no = eb.blank-no
                     wrk-ink.i-dscr   = eb.i-dscr2[i]
                     wrk-ink.i-pass   = eb.i-ps2[i]
                     wrk-ink.i-unit   = IF i LE 12 AND AVAIL reftable THEN reftable.val[i]
                                        ELSE IF i > 12 AND AVAIL b-rt THEN b-rt.val[i - 12]
                                        ELSE 1.
  
  
                   /* IF AVAIL reftable THEN
                       wrk-ink.i-side = FILL(" ",5) + SUBSTRING(reftable.dscr,i,1).*/
                    IF i <= 12 THEN DO:
                    FIND FIRST ref-side WHERE
                      ref-side.reftable EQ "ce/v-est3.w Unit#"  AND
                      ref-side.company  EQ eb.company AND
                      ref-side.loc      EQ eb.est-no AND
                      ref-side.code     EQ STRING(eb.form-no,"9999999999") AND
                      ref-side.code2    EQ STRING(eb.blank-no,"9999999999")
                      NO-LOCK NO-ERROR.
                    IF AVAIL ref-side THEN
                        wrk-ink.i-side = FILL(" ",5) + SUBSTRING(ref-side.dscr,i,1).
                  END.
                  ELSE DO:
                      FIND FIRST ref-side WHERE
                          ref-side.reftable EQ "ce/v-est3.w Unit#1"  AND
                          ref-side.company  EQ eb.company AND
                          ref-side.loc      EQ eb.est-no AND
                          ref-side.code     EQ STRING(eb.form-no,"9999999999") AND
                          ref-side.code2    EQ STRING(eb.blank-no,"9999999999")
                          NO-LOCK NO-ERROR.
                       IF AVAIL ref-side THEN
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
                
            IF NOT AVAIL wrk-ink                              AND
               (job-mat.blank-no  EQ eb.blank-no OR
                (job-mat.blank-no EQ 0 AND eb.blank-no EQ 1)) THEN DO:
              CREATE wrk-ink.
              ASSIGN
               wrk-ink.i-code   = job-mat.i-no
               wrk-ink.form-no  = eb.form-no
               wrk-ink.blank-no = eb.blank-no
               wrk-ink.i-dscr   = item.est-dscr
               wrk-ink.i-pass   = 1
               wrk-ink.i-unit   = 1.
            END.
            IF AVAIL wrk-ink AND
               ((est.est-type EQ 4 AND eb.form-no = job-mat.frm AND eb.blank-no = job-mat.blank-no) OR
                 est.est-type <> 4 ) 
                 THEN wrk-ink.i-qty = wrk-ink.i-qty + job-mat.qty.            
            
          END. /* JOB-MAT */

          IF eb.est-type EQ 4 THEN v-fac = eb.yld-qty / v-est-qty.
          
            FIND FIRST style
                WHERE style.company EQ eb.company
                  AND style.style   EQ eb.style
                NO-LOCK NO-ERROR.
            IF AVAIL style THEN v-stypart = style.dscr.
            ASSIGN
             v-dsc[1] = eb.part-dscr1
             v-dsc[2] = eb.part-dscr2
             v-size[1] = STRING(eb.len) + "x" + string(eb.wid) + "x" +
                         string(eb.dep)
             v-size[2] = eb.i-coldscr.

             IF eb.blank-no > 0 AND eb.blank-no < 11 THEN 
                 ASSIGN v-fgdsc[eb.blank-no] = eb.part-dscr1.
                                                             
            /*if v-first then*/
            v-upc-lbl = "   CAD#".
            IF FIRST-OF(eb.form-no) THEN
               PUT "<P12><B> P R E S S <P9>" SKIP 
                   " F/B         FG Item #       Cust Part #      Artwork #      Description                       Order Qty    MAX QTY    MIN QTY </B>" SKIP.
              
           v-job-qty = 0.
           FOR EACH xjob-hdr FIELDS(qty) WHERE xjob-hdr.company EQ cocode
               AND xjob-hdr.job     EQ job-hdr.job
               AND xjob-hdr.job-no  EQ job-hdr.job-no
               AND xjob-hdr.job-no2 EQ job-hdr.job-no2
               AND xjob-hdr.i-no    EQ eb.stock NO-LOCK:
               v-job-qty = v-job-qty + xjob-hdr.qty.
           END.

           /** PRINT ITEM **/
           FIND FIRST oe-ordl
                WHERE oe-ordl.company EQ job-hdr.company
                  AND oe-ordl.ord-no  EQ job-hdr.ord-no
                  AND oe-ordl.job-no  EQ job-hdr.job-no
                  AND oe-ordl.job-no2 EQ job-hdr.job-no2
                  AND oe-ordl.i-no    EQ eb.stock-no /*job-hdr.i-no*/
                NO-LOCK NO-ERROR.
 
            IF AVAIL oe-ordl THEN DO:
              v-est-qty = oe-ordl.qty.
              FIND FIRST oe-ord OF oe-ordl NO-LOCK.
              v-ovund = STRING("Overrun/Underrun %:  " +
                               trim(STRING(oe-ordl.over-pct,">>9.99")) + "/" +
                               trim(STRING(oe-ordl.under-pct,">>9.99"))).
              ASSIGN
                  v-max-qty  = INT ( oe-ordl.qty + oe-ordl.qty * (dec(lv-over-run) / 100) )
                  v-min-qty =  INT ( oe-ordl.qty - oe-ordl.qty * (dec(lv-under-run) / 100)).
            END.
            ELSE v-est-qty = v-job-qty.
           
            RELEASE w-lo.
            FIND FIRST w-lo NO-ERROR.
            ASSIGN
            v-case-size = STRING(eb.cas-len) + "x" + string(eb.cas-wid) + "x" +
                         string(eb.cas-dep)
            v-up = eb.num-up
            v-case-count = IF AVAIL oe-ordl AND oe-ordl.cas-cnt <> 0 THEN oe-ordl.cas-cnt
                           ELSE eb.cas-cnt
            v-case-qty = ROUND(v-job-qty / v-case-count,0)
            v-itm-printed = v-itm-printed + 1.

             FIND FIRST itemfg WHERE itemfg.company = job-hdr.company
                            AND itemfg.i-no = job-hdr.i-no NO-LOCK NO-ERROR.

            DISPLAY SPACE(1) v-job-no + "-" + trim(STRING(eb.form-no,">>9")) +
                    trim(STRING(eb.blank-no,">>9")) FORM "x(11)" 
                    SPACE(1) eb.stock-no @ job-hdr.i-no 
                    (IF AVAIL oe-ordl  THEN oe-ordl.part-no ELSE IF AVAIL itemfg THEN itemfg.part-no ELSE "") FORM "x(15)"   SPACE(1)
                    (IF eb.plate-no <> "" THEN eb.plate-no  ELSE IF AVAIL itemfg THEN itemfg.plate-no ELSE "" ) FORM "x(15)"
                    SPACE(1) v-dsc[1] FORM "x(32)"
                    oe-ordl.qty WHEN AVAIL oe-ordl format "->,>>>,>>9"  /* Task #01240503*/   SPACE(1)
                    v-max-qty  format "->,>>>,>>9"   SPACE(1)
                    v-min-qty  format "->,>>>,>>9"
                with stream-io width 175 no-labels no-box frame line-det1.

            FIND FIRST item
                WHERE item.company EQ cocode
                  AND item.i-no    EQ eb.cas-no
                NO-LOCK NO-ERROR.
            /* v-cas-dscr = if avail item then item.i-name else "". */
           v-item-desc = IF AVAIL ITEM THEN ITEM.i-name ELSE  "" .  
           /* end. /* last-of(eb.form-no) */      */
          IF LAST-OF(eb.form-no) THEN DO:
             IF v-itm-printed = 1 THEN PUT SKIP(1). 
             ELSE PUT SKIP(4 - v-itm-printed).
             
             /* Number of sheets ticket1.p - single board, ticket2.p - multi board */
             RUN oe/rep/ticket2.p (RECID(ef), RECID(job-hdr)).
             /*find first wrk-sheet where recid(wrk-sheet) eq save_id.*/
             IF AVAIL oe-ordl THEN
               FIND FIRST po-ord WHERE po-ord.company = oe-ordl.company
                            AND po-ord.po-no = int(oe-ordl.po-no-po) NO-LOCK NO-ERROR.
             ASSIGN
                v-vend = IF AVAIL oe-ordl THEN oe-ordl.vend-no ELSE ""
                v-board-po = IF AVAIL oe-ordl THEN oe-ordl.po-no-po ELSE 0.

             IF AVAIL po-ord THEN
                FIND FIRST po-ordl WHERE
                     po-ordl.company EQ po-ord.company AND
                     po-ordl.po-no   EQ po-ord.po-no AND
                     po-ordl.i-no = ef.board
                     NO-LOCK NO-ERROR.

             v-po-duedate = IF AVAIL po-ordl THEN po-ordl.due-date ELSE ?.

             PUT "<P10>" 
                "<B> STOCK CODE  DESCRIPTION            GRAIN  WIDTH CYLINDER   DIE SIZE             DIE#              PRINT QTY      PRINT FEET</B>" 
                SKIP.

            /** PRINT SHEET **/
             x = 2.
             
             FOR EACH wrk-sheet WHERE wrk-sheet.form-no = ef.form-no  
                 /*break by wrk-sheet.form-no*/ :  
               FIND FIRST ITEM WHERE item.company EQ cocode
                                 AND item.i-no    EQ wrk-sheet.i-no NO-LOCK NO-ERROR.
               FIND FIRST job-mat 
                   WHERE job-mat.company EQ cocode 
                     AND job-mat.job-no EQ v-job-no
                     AND job-mat.job-no2 EQ v-job-no2
                     AND job-mat.frm EQ wrk-sheet.form-no
                     AND job-mat.i-no EQ wrk-sheet.i-no
                   NO-LOCK NO-ERROR.

               FIND FIRST notes WHERE notes.rec_key = job.rec_key AND
                                      notes.note_code = "BS" AND
                                      notes.note_form_no = wrk-sheet.form-no NO-LOCK NO-ERROR.

               ASSIGN
                  v-lbs = wrk-sheet.gsh-qty * (wrk-sheet.sh-wid * wrk-sheet.sh-len / 144) / 1000 * ITEM.basis-w
                  v-dept-title = IF AVAIL notes THEN notes.note_title ELSE ""
                  v-print-feet = (wrk-sheet.gsh-qty * ef.gsh-len) / 12 .
               
               DISPLAY
                 SPACE(1) TRIM(wrk-sheet.i-no) FORMAT "X(10)"  SPACE(2)
                   ITEM.i-name  FORM "x(23)"  
                   ef.xgrain FORM "x(2)" SPACE(1)
/*                     v-lbs SPACE(2) */
                  FILL(" ",8 - LENGTH(STRING(wrk-sheet.sh-wid))) + STRING(wrk-sheet.sh-wid)
                  SPACE (1)
                  (IF AVAIL job-mat THEN job-mat.len  ELSE ef.gsh-len) FORMAT ">>9.9999"
                   SPACE(2)
                  STRING(ef.trim-w) + "x" + string(ef.trim-l) FORMAT "x(21)" SPACE(1)
                  eb.die-no FORMAT "X(15)"  SPACE(1)
                 /* gdm - 12040803 */
                  wrk-sheet.gsh-qty FORMAT "->>,>>>,>>9"  SPACE (1)
                  v-print-feet 
                  WITH STREAM-IO WIDTH 170 NO-LABELS NO-BOX FRAME sheet.

               x = 1.
             END. /* each wrk-sheet */   

              /** Print Leaf/Film ticket 15459 **/

             FOR EACH wrk-film WHERE wrk-film.form-no = ef.form-no
                 /*break by wrk-sheet.form-no*/ NO-LOCK BREAK BY wrk-film.leaf :
               FIND FIRST ITEM WHERE item.company EQ cocode
                                 AND item.i-no    EQ wrk-film.leaf NO-LOCK NO-ERROR.
                FIND FIRST job-mch WHERE job-mch.company = cocode 
                                      AND job-mch.job = job.job
                   AND job-mch.job-no = job.job-no 
                   AND job-mch.job-no2 = job.job-no2 
                   AND job-mch.m-code  = ef.m-code NO-LOCK NO-ERROR .

               IF FIRST(wrk-film.leaf) THEN
                    PUT "<P10>" SKIP
                 "<B> LEAF/FILM CODE  DESCRIPTION                 FORM  BLANK  APERTURE SIZE (W x L)   TOTAL LINEAL FEET   POUNDS     </B>" 
                 SKIP.
                DISPLAY 
                    SPACE(1) wrk-film.leaf FORM "x(15)"
                    (IF AVAIL ITEM THEN ITEM.i-name ELSE "") FORM "x(28)" SPACE(2)
                    wrk-film.snum SPACE(4)
                    wrk-film.bnum SPACE(3)
                    string(wrk-film.leaf-l) + "x" + string(wrk-film.leaf-w)
                    format "x(23)" SPACE(10)
                    STRING( ( wrk-film.leaf-l + 1) * (IF AVAIL job-mch THEN job-mch.run-qty ELSE 0) / 12)
                    STRING((IF AVAIL job-mch THEN job-mch.run-qty ELSE 0) * (wrk-film.leaf-l + 1) * ( wrk-film.leaf-w + 1) /
                    (IF AVAIL ITEM THEN ITEM.sqin-lb ELSE 0) )
                    with stream-io width 170 no-labels no-box frame film.

                /*IF LAST(wrk-film.leaf) THEN
                     PUT "<P10>" SKIP(1) .*/
             END.
              /** Print Leaf/Film ticket 15459 **/
             
             PUT "<B> PASS  SIDE  LBS    INK NAME               UNIT#   PASS   SIDE    LBS   INK NAME               UNIT#       </B>"
                 SKIP.

             ASSIGN
                x = 2
                i = 1
                v-ink1 = ""
                v-ink2 = ""
                v-pass-count = 0.

             FOR EACH wrk-ink WHERE wrk-ink.form-no = eb.form-no
                 BREAK BY wrk-ink.i-pass:
                 IF FIRST-OF(wrk-ink.i-pass) THEN v-pass-count = v-pass-count + 1.
             END.
             FOR EACH wrk-ink WHERE wrk-ink.form-no = eb.form-no
                BREAK BY wrk-ink.i-pass
                      BY wrk-ink.i-code
                      BY wrk-ink.blank-no:

                IF wrk-ink.i-pass <= 2 THEN
                IF FIRST-OF(wrk-ink.i-pass) THEN i = 1.

                IF FIRST-OF(wrk-ink.i-code) THEN ASSIGN v-item[i] = ""
                                                        v-i-qty[i] = 0.
                ASSIGN
                   v-item[i] = IF LOOKUP(STRING(wrk-ink.blank-no),v-item[i]) > 0 THEN v-item[i] ELSE v-item[i] + string(wrk-ink.blank-no) + ","
                   v-i-qty[i] = v-i-qty[i] + wrk-ink.i-qty.

                /*IF LAST-OF(wrk-ink.i-code) THEN DO:*/
                    IF SUBSTRING(v-item[i],LENGTH(v-item[i]),1) = "," THEN v-item[i] = SUBSTRING(v-item[i],1,LENGTH(v-item[i]) - 1).                    
                    /*v-alloc = v-item[i].
                    if num-entries(v-item[i]) gt 1 then do:
                       v-alloc = "".
                       do j = 1 to num-entries(v-item[i]):
                          if j eq 1 or j eq num-entries(v-item[i]) THEN v-alloc = v-alloc + entry(j,v-item[i]) + ",".
                          else do:
                             if int(entry(j,v-item[i])) - int(entry(j - 1,v-item[i])) le 1 then
                                substr(v-alloc,length(trim(v-alloc)),1) = "-".
                             else do:
                                if substr(v-alloc,length(trim(v-alloc)),1) eq "-" then
                                   v-alloc = v-alloc + entry(j - 1,v-item[i]) + ",".

                                v-alloc = v-alloc + entry(j,v-item[i]) + ",".
                             END.
                          end.
                       end.                    
                       if v-alloc ne "" then substr(v-alloc,length(trim(v-alloc)),1) = "".
                       
                    end.*/
                 
                    IF wrk-ink.i-side NE "" THEN
                    DO: 
                       IF wrk-ink.i-pass = 1 THEN
                          ASSIGN v-ink1[i] = STRING(wrk-ink.i-pass) + wrk-ink.i-side +
                                             STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                             string(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                 i = i + 1. 
                       ELSE IF wrk-ink.i-pass = 2 THEN
                          ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + wrk-ink.i-side +
                                             STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                             string(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                 i = i + 1.
                      ELSE IF wrk-ink.i-pass = 3 THEN
                          ASSIGN v-ink1[i] = STRING(wrk-ink.i-pass) + wrk-ink.i-side +
                                             STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                             string(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                 i = i + 1.
                      ELSE IF wrk-ink.i-pass = 4 THEN
                          ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + wrk-ink.i-side +
                                             STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                             string(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                 i = i + 1.
                      ELSE IF wrk-ink.i-pass = 5 THEN
                          ASSIGN v-ink1[i] = STRING(wrk-ink.i-pass) + wrk-ink.i-side +
                                             STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                             string(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                 i = i + 1.
                      ELSE IF wrk-ink.i-pass = 6 THEN
                          ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + wrk-ink.i-side +
                                             STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                             string(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                 i = i + 1.
                     ELSE IF wrk-ink.i-pass = 7 THEN
                          ASSIGN v-ink1[i] = STRING(wrk-ink.i-pass) + wrk-ink.i-side +
                                             STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                             string(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                 i = i + 1.
                      ELSE IF wrk-ink.i-pass = 8 THEN
                          ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + wrk-ink.i-side +
                                             STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                             string(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                 i = i + 1.
                    END.
                    ELSE
                    DO:
                       IF wrk-ink.i-pass = 1 THEN
                          ASSIGN v-ink1[i] = (IF v-pass-count = 1 THEN "F      " ELSE "B      ") +
                                              STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                             string(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                 i = i + 1. 
                       ELSE IF wrk-ink.i-pass = 2 THEN
                          ASSIGN v-ink2[i] = "F      " + STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                      string(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                 i = i + 1.
                       ELSE IF wrk-ink.i-pass = 3 THEN
                          ASSIGN v-ink1[i] = "F      " + STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                      string(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                 i = i + 1.
                       ELSE IF wrk-ink.i-pass = 4 THEN
                          ASSIGN v-ink2[i] = "F      " + STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                      string(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                 i = i + 1.
                       ELSE IF wrk-ink.i-pass = 5 THEN
                          ASSIGN v-ink1[i] = "F      " + STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                      string(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                 i = i + 1.
                       ELSE IF wrk-ink.i-pass = 6 THEN
                          ASSIGN v-ink2[i] = "F      " + STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                      string(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                 i = i + 1.
                       ELSE IF wrk-ink.i-pass = 7 THEN
                          ASSIGN v-ink1[i] = "F      " + STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                      string(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                 i = i + 1.
                       ELSE IF wrk-ink.i-pass = 8 THEN
                          ASSIGN v-ink2[i] = "F      " + STRING(v-i-qty[i],"->,>>9.99") + "   " + 
                                      string(wrk-ink.i-dscr,"x(25)") + " " + STRING(wrk-ink.i-unit)
                                 i = i + 1.
                    END.

                /*END.*/
              
                DELETE wrk-ink.
             END. /* each wrk-ink */

             ASSIGN
                v-skip = NO
                v-plate-printed = NO.
             
             DO j = 1 TO EXTENT(v-ink1):
                IF TRIM(v-ink1[j]) = "-" THEN v-ink1[j] = "".               
                IF v-ink1[j] <> "" THEN DO:
                /* ASSIGN v-ink1[j] = SUBSTRING(v-ink1[j],1,5) + FILL(" ",7) + TRIM(SUBSTRING(v-ink1[j],6)). */
                   IF v-skip THEN DO:
                       PUT v-ink1[j] FORM "x(52)" .
                       IF j = 2 THEN DO:
                           /*PUT eb.plate-no AT 107.*/
                           v-plate-printed = YES.
                       END.
                       PUT SKIP.
                   END.
                   ELSE PUT SPACE(1) v-ink1[j] FORM "x(52)".                                                             
                   v-skip = NOT v-skip.             
                END.
             END.

             IF NOT v-plate-printed THEN PUT /*eb.plate-no AT 107*/ SKIP(1).

             DO j = 1 TO EXTENT(v-ink2):
                IF TRIM(v-ink2[j]) = "-" THEN v-ink2[j] = "".                 
                IF v-ink2[j] <> "" THEN DO:
                   IF v-skip THEN PUT SPACE(1) v-ink2[j] FORM "x(52)" SKIP.
                   ELSE PUT SPACE(1) v-ink2[j] FORM "x(52)".
                   v-skip = NOT v-skip.
                END.                
             END.
             PUT SKIP .

             ASSIGN
                v-upnew =  eb.num-wid * eb.num-len
                v-pr-speed = 0
                v-mr-hours = 0
                v-dc-gl-speed = 0
                v-dc-out = 0
                v-dc-only-out = 0
                v-cas-wt = 0
                v-sample-on-cnt = NO
                v-shrink-wrap = CAN-FIND(FIRST est-op WHERE
                                est-op.company EQ job-hdr.company AND
                                est-op.est-no EQ est.est-no AND
                                est-op.dept = "SW").

            PUT "<B> UNIT SIZE:  </B>" STRING(eb.t-len) + " x " + string(eb.t-wid) FORMAT "X(38)"
                "  <B>#UP</B> :"   STRING(v-upnew)     "<B>Speed(FPM)        MR</B>" SKIP.
                
            FOR FIRST wrk-op WHERE
                wrk-op.s-num = job-hdr.frm AND
                wrk-op.dept = "PR"
                BREAK BY wrk-op.d-seq BY wrk-op.b-num:

                ASSIGN
                   v-pr-speed = wrk-op.speed[job-hdr.frm]
                   v-mr-hours = wrk-op.mr[job-hdr.frm].
            END.

            FOR FIRST wrk-op WHERE
                wrk-op.s-num = job-hdr.frm AND
                INDEX("DC,GL",wrk-op.dept) > 0
                BREAK BY wrk-op.d-seq BY wrk-op.b-num:

                v-dc-gl-speed = wrk-op.speed[job-hdr.frm].
            END.

            FOR FIRST wrk-op WHERE
                wrk-op.s-num = job-hdr.frm AND
                wrk-op.dept = "DC"
                BREAK BY wrk-op.d-seq BY wrk-op.b-num:

                v-dc-gl-speed = wrk-op.speed[job-hdr.frm].
            END.

            FOR EACH est-op FIELDS(n-out m-code d-seq b-num) WHERE
                est-op.company EQ job-hdr.company AND
                est-op.est-no EQ est.est-no AND
                est-op.line < 500
                NO-LOCK,
                FIRST mach FIELDS(dept)
                {sys/ref/machW.i}
                AND mach.m-code EQ est-op.m-code
                NO-LOCK
                BY est-op.d-seq BY est-op.b-num:

                IF mach.dept[1] EQ "DC" OR
                   mach.dept[2] EQ "DC" THEN
                   DO:
                      v-dc-only-out = est-op.n-out.
                      LEAVE.
                   END.
                
            END.

            FOR EACH est-op FIELDS(n-out m-code d-seq b-num) WHERE
                est-op.company EQ job-hdr.company AND
                est-op.est-no EQ est.est-no AND
                est-op.line < 500
                NO-LOCK,
                FIRST mach
                {sys/ref/machW.i}
                AND mach.m-code EQ est-op.m-code
                NO-LOCK
                BY est-op.d-seq BY est-op.b-num:
                    
                IF INDEX("GL,DC",mach.dept[1]) > 0 OR
                   INDEX("GL,DC",mach.dept[2]) > 0 THEN
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

            PUT v-fill AT 1 SKIP "<B><P12> F I N I S H I N G</B><P9>" SKIP.
           
           FIND item WHERE
                item.company = eb.company AND
                item.i-no = eb.layer-pad  AND
                item.mat-type = "5" 
                NO-LOCK NO-ERROR.

           ASSIGN 
              v-lp-dep = IF AVAIL item THEN ITEM.case-d ELSE 0 
              v-lp-qty = IF AVAIL item THEN ITEM.box-case ELSE 0.

          /*IF FIRST-OF(eb.form-no) THEN
             PUT 
                "<P9><B> UNIT SIZE                      Packaging       Size                   Units Per        QTY Trays Per case     Speed(UPH)   Case wt     Style</B>" SKIP.*/

          IF eb.lp-up NE 0 THEN
          DO:
             v-unit-per-dec = eb.cas-cnt / eb.lp-up.
             {sys/inc/roundup.i v-unit-per-dec}
             v-unit-per-int = INT(v-unit-per-dec).
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

          FIND FIRST itemfg WHERE
               itemfg.company = eb.company AND
               itemfg.i-no = eb.stock-no
               NO-LOCK NO-ERROR.

          IF AVAIL itemfg THEN
          DO:
             v-cas-wt = (itemfg.weight-100 / 100) * eb.cas-cnt.             
          END.

          FIND FIRST tt-sample-ctn WHERE
               tt-sample-ctn.tt-job-no EQ job-hdr.job-no AND
               tt-sample-ctn.tt-job-no2 EQ job-hdr.job-no2 AND
               tt-sample-ctn.tt-frm EQ eb.form-no
               NO-ERROR.

          IF AVAIL tt-sample-ctn THEN
             v-sample-on-cnt = tt-sample-ctn.tt-samp-on-cnt.

          IF v-dc-only-out EQ 0 THEN
             v-dc-only-out = 1.

          IF v-dc-out EQ 0 THEN
             v-dc-out = 1.

          PUT "<P9><B> UNIT SIZE   Flat:</B>"  STRING(eb.t-len) + " x " + string(eb.t-wid * v-dc-only-out)  FORMAT "x(19)"
              "<B>Finished:</B> "  STRING(eb.len) + " x " + string(eb.wid) FORMAT "X(19)"
              "<B>UP#:</B>" v-dc-out SPACE(4)
              "<B>Speed:</B>" v-dc-gl-speed FORMAT "->,>>>,>>9" SPACE(4)
              "<B>Style:</B> " eb.style  SKIP

              "<B> Packaging: " SKIP
              " Tray # </B>" eb.layer-pad FORMAT "x(15)"
              "<C20><B>Size: </B>"  STRING(eb.lp-len) + "x" + string(eb.lp-wid) + "x" + string(v-lp-dep)  FORMAT "x(27)"
              "<C40><B>Qty per tray:</B>"   v-unit-per-int FORMAT "->>>>>9"
              "<C60><B># of trays:</B>"  v-lp-qty  FORMAT "->>>>>>9"  SKIP 

              "<B> Case #: </B>"   eb.cas-no
              "<C20><B>Size: </B>"    STRING(eb.cas-len) + "x" + STRING(eb.cas-wid) + "x" + STRING(eb.cas-dep) FORMAT "x(27)"
              "<C40><B>Qty per case:</B>"   eb.cas-cnt FORMAT "->>>>>9"
              "<C60><B># of trays per case:</B>" v-job-qty-boxes-code-int  FORMAT "->>>>>>9"  SKIP

              "<B> Pallet:</B> " eb.tr-no
              "<C20><B>Shrink Wrap: </B>" STRING(v-shrink-wrap,"Y/N")  
              "<C40><B>Packing Specs: </B>" (IF AVAIL itemfg THEN  itemfg.prod-no ELSE "") SKIP .


          /*PUT " Flat" "Finished"  AT 22 "Tray#" AT 33 eb.layer-pad FORMAT "x(10)"
               string(eb.lp-len) + "x" + string(eb.lp-wid) + "x" + string(v-lp-dep)  FORMAT "x(27)" AT 49
               v-unit-per-int   AT 76
               v-job-qty-unit-per-int AT 85
               STRING(v-lp-qty) AT 93
               v-dc-gl-speed    AT 111
               v-cas-wt        AT 122
               eb.style AT  137  SKIP
               SPACE(1) string(eb.t-len) + " x " + string(eb.t-wid * v-dc-only-out)  FORMAT "x(19)"
               space(1) STRING(eb.len) + " x " + string(eb.wid) FORMAT "X(19)"
               "case#" AT 33 eb.cas-no FORMAT "X(10)"
               STRING(eb.cas-len) + "x" + STRING(eb.cas-wid) + "x" + STRING(eb.cas-dep) FORMAT "x(27)" AT 49     
               eb.cas-cnt    AT 76 
               v-job-qty-boxes-code-int AT 85
               "Act"  AT 100    SPACE (1) "A"   
               "Sample On ctn"  AT 109 SPACE (1) STRING(v-sample-on-cnt,"Y/N") SKIP 
               "<B> Units/Shts UPS: </B>" v-dc-out
               "Pallet"  AT 40 space(1) eb.tr-no
               "Shrink wrap"  AT 116 SPACE (1) v-shrink-wrap.*/
           PUT v-fill AT 1 SKIP.
             
          END. /* last-of(eb.form-no) */
          
        END. /* each eb */
      END. /* each ef */
      END. /* first job-no */

      IF LAST-OF(job-hdr.frm) THEN DO:

         IF s-run-speed THEN
            PUT "<B> MACHINE                 MR WASTE   MR HRS  RUN SPEED   SPOIL%    SHEETS PER MACHINE   </B>" /* SIZE  TOTAL REQUIRED  PALLET*/
                SKIP.
         ELSE
            PUT "<B> MACHINE                 MR WASTE   MR HRS   RUN HOUR    SPOIL%    SHEETS PER MACHINE   </B>"  /* SIZE  TOTAL REQUIRED  PALLET*/
                SKIP.

         FOR EACH wrk-op WHERE wrk-op.s-num = job-hdr.frm BREAK BY wrk-op.d-seq BY wrk-op.b-num:
             v-mat-for-mach = "".
             IF LOOKUP(wrk-op.dept,lv-mat-dept-list) > 0 THEN DO:
                 
                FOR EACH xjob-mat WHERE xjob-mat.company EQ cocode
                                       AND xjob-mat.job     EQ job-hdr.job
                                       AND xjob-mat.job-no  EQ job-hdr.job-no
                                       AND xjob-mat.job-no2 EQ job-hdr.job-no2
                                       AND xjob-mat.frm = job-hdr.frm
                                       /*AND (xjob-mat.blank-no = job-hdr.blank-no
                                            OR xjob-mat.blank-no = 0)*/  NO-LOCK,
                     FIRST ITEM WHERE ITEM.company = cocode AND
                                      ITEM.i-no = xjob-mat.rm-i-no AND
                                      ITEM.mat-type = SUBSTRING(wrk-op.dept,1,1) NO-LOCK :
                     v-mat-for-mach = /*ITEM.i-name + fill(" ", 33 - LENGTH(ITEM.i-name))  /*"       " */ + */
                                      STRING(xjob-mat.wid) + "x" + STRING(xjob-mat.len) +
                                      "      " + string(xjob-mat.qty).                   
                     LEAVE.                 
                END.                            
             END.
     

             IF LAST(wrk-op.d-seq) THEN DO: /* pallet code*/
                FOR EACH xjob-mat WHERE xjob-mat.company EQ cocode
                                       AND xjob-mat.job     EQ job-hdr.job
                                       AND xjob-mat.job-no  EQ job-hdr.job-no
                                       AND xjob-mat.job-no2 EQ job-hdr.job-no2
                                       AND xjob-mat.frm = job-hdr.frm
                                       AND (xjob-mat.blank-no = job-hdr.blank-no
                                            OR xjob-mat.blank-no = 0) NO-LOCK,
                     FIRST ITEM WHERE ITEM.company = cocode AND
                                      ITEM.i-no = xjob-mat.rm-i-no AND 
                                      ITEM.mat-type = "D" NO-LOCK :
                     v-mat-for-mach = v-mat-for-mach +
                                      (IF LENGTH(v-mat-for-mach) <= 20 THEN FILL(" ", 20 - LENGTH(v-mat-for-mach))
                                      ELSE "  " ) +
                                      ITEM.i-name.
                 END.
             END.          
             IF s-prt-mstandard THEN DO:
                IF s-run-speed THEN
                   PUT SPACE(1) wrk-op.m-dscr   SPACE(4)
                       wrk-op.mr-waste[job-hdr.frm]   SPACE(6)
                       wrk-op.mr[job-hdr.frm]         SPACE(5)
                       wrk-op.speed[job-hdr.frm]      SPACE(5)
                       wrk-op.spoil[job-hdr.frm]      SPACE(5)
                       wrk-op.num-sh[job-hdr.frm]     SPACE(11)  /*9+9*/
                       /*v-mat-for-mach FORM "x(40)"    /*60*/*/
                       SKIP.
               ELSE
                   PUT SPACE(1) wrk-op.m-dscr   SPACE(5)
                       wrk-op.mr-waste[job-hdr.frm]   SPACE(5)
                       wrk-op.mr[job-hdr.frm]         SPACE(5)
                       wrk-op.run-hr[job-hdr.frm]     SPACE(5)
                       wrk-op.spoil[job-hdr.frm]      SPACE(5)
                       wrk-op.num-sh[job-hdr.frm]     SPACE(11)  /* 9 + 9 */
                       /*v-mat-for-mach FORM "x(40)"    /*60*/*/
                       SKIP.
             END.
             ELSE PUT SPACE(1) wrk-op.m-dscr   SPACE(4)
                     SPACE(10)
                     SPACE(11)
                     SPACE(10)
                     SPACE(13)
                     wrk-op.num-sh[job-hdr.frm]     SPACE(11)  /* 9 + 9 */
                     /*v-mat-for-mach FORM "x(40)" /*60*/*/
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
           lv-text = ""
           v-dept-inst = ""
           v-exc-depts = v-exc-depts + (IF v-exc-depts <> "" THEN ",BS" ELSE "BS")
           v-dept-note-printed = NO.

        FOR EACH notes WHERE notes.rec_key = job.rec_key
                    AND (notes.note_form_no = job-hdr.frm OR notes.note_form_no = 0)
                    AND (( notes.note_type EQ "O" AND notes.note_group EQ string(job.job) ) OR  notes.note_type NE "O" )
                    AND LOOKUP(notes.note_code,v-exc-depts) EQ 0 
                    /*AND notes.note_type NE 'O'*/  NO-LOCK  /* ticket 14661 */
                    BREAK BY notes.note_code:
             
            IF FIRST-OF(notes.note_code) THEN DO:
               lv-text = "".
               FOR EACH tt-formtext:
                   DELETE tt-formtext.
               END.
               FIND FIRST dept WHERE dept.CODE = notes.note_code NO-LOCK NO-ERROR.
               lv-text = (IF AVAIL dept THEN dept.dscr ELSE note.note_code) + "     " + 
                         notes.note_title + " " + notes.note_text + CHR(10).
               DO li = 1 TO 4:
                  CREATE tt-formtext.
                  ASSIGN tt-line-no = li
                         tt-length  = 128.
               END.
               RUN custom/formtext.p (lv-text).
               i = 0.
               v-dept-inst = "".
               FOR EACH tt-formtext:
                   i = i + 1.
                   IF  i <= 4 THEN v-dept-inst[i] = tt-formtext.tt-text.      
               END.
               IF PAGE-SIZE - LINE-COUNTER < 11 THEN PAGE.
               IF FIRST(notes.note_code) THEN PUT "<B> DEPARTMENT   INSTRUCTION NOTES</B>" SKIP.

               IF v-dept-inst[1] NE "" THEN
                  PUT " " v-dept-inst[1] FORM "x(128)" SKIP.
               IF v-dept-inst[2] NE "" THEN
                  PUT " " v-dept-inst[2] FORM "x(128)" SKIP.
               IF v-dept-inst[3] NE "" THEN
                  PUT " " v-dept-inst[3] FORM "x(128)" SKIP.
               IF v-dept-inst[4] NE "" THEN
                  PUT " " v-dept-inst[4] FORM "x(128)" SKIP.
               
            END. /* first-of(notes.note_code) */
            v-dept-note-printed = YES.
        END. /* for each notes */
    
       /*==== note ======*/


        IF NOT v-dept-note-printed THEN DO:
           IF PAGE-SIZE - LINE-COUNTER < 11 THEN PAGE.
           PUT "<B> DEPARTMENT   INSTRUCTION NOTES</B>" SKIP.
           IF v-dept-inst[1] NE "" THEN
              PUT " " v-dept-inst[1] FORM "x(128)" SKIP.
           IF v-dept-inst[2] NE "" THEN
              PUT " " v-dept-inst[2] FORM "x(128)" SKIP.
           IF v-dept-inst[3] NE "" THEN
              PUT " " v-dept-inst[3] FORM "x(128)" SKIP.
           IF v-dept-inst[4] NE "" THEN
              PUT " " v-dept-inst[4] FORM "x(128)" SKIP.
        END.

        /* spec note */
        ASSIGN
           lv-line-chars = 95
           v-inst2 = "".

        FIND FIRST itemfg WHERE itemfg.company = job-hdr.company
                            AND itemfg.i-no = job-hdr.i-no NO-LOCK NO-ERROR.

        {custom/notespr8.i itemfg v-inst2 25 "notes.rec_key EQ itemfg.rec_key AND
                           notes.note_type EQ 'S' AND CAN-DO(v-spec-list,notes.note_code) "}
        

        PUT "<B>SPEC CODE                 SPEC NOTES</B>" SKIP.
        
        DO i = 1 TO 25:
          IF v-inst2[i] NE "" THEN DO:        

            PUT " " v-inst2[i] FORMAT "X(128)" SKIP.

            IF LINE-COUNTER GE 47 THEN DO:
              PAGE.
              PUT v-fill SKIP
                  "<B>SPEC CODE                 SPEC NOTES</B>" SKIP.
              
            END. /* IF v-lincnt GE 12 */
          END. /* IF v-inst2[i] NE ""*/              
        END. /* DO */
/*********************************************
        PAGE.
  
        FIND first ef WHERE ef.company EQ job-hdr.company
                        AND ef.est-no  EQ job-hdr.est-no
                        AND ef.form-no = job-hdr.frm NO-LOCK NO-ERROR.
        IF AVAIL ef THEN DO:
           IF PAGE-SIZE - LINE-COUNTER < 11 THEN PAGE.
           PUT v-fill skip
               "<B> Special Materials             Miscellaneous - SubContract                               Prep</B>"
               SKIP.
           j = 1.
           DO i = 1 TO 8:
              IF ef.spec-no[i] <> "" THEN ASSIGN  v-spec-no[j] = ef.spec-no[i]
                                                  j = j + 1.              
           END.
           j = 1.
           DO i = 1 TO 6:
              IF ef.mis-cost[i] <> "" THEN
                 ASSIGN  v-misc[j] = string(ef.mis-snum[i]) + "-" + STRING(ef.mis-bnum[i]) +
                                     ef.mis-cost[i]
                         j = j + 1.                          
           END.
           j = 1.
           v-prep = "".
           FOR EACH wrk-prep WHERE wrk-prep.s-num = ef.form-no NO-LOCK BREAK BY wrk-prep.CODE:
               
               IF first-of(wrk-prep.code) AND  wrk-prep.CODE <> "" AND j < 9 THEN 
                     ASSIGN v-prep[j] = wrk-prep.CODE + " " + wrk-prep.dscr
                            j = j + 1.
           END.
        END.  /* avail ef */ 
        DO i = 1 TO 6:
            IF v-spec-no[i] <> "" THEN PUT SPACE(1) v-spec-no[i].
            IF v-misc[i] <> "" THEN PUT v-misc[i] AT 32 FORM "x(30)" .
            IF v-prep[i] <> "" THEN PUT v-prep[i] AT 90.
            IF v-spec-no[i] <> "" OR v-misc[i] <> "" OR v-prep[i] <> "" THEN PUT SKIP.
        END.
        DO i = 7 TO 8:
            IF v-spec-no[i] <> "" THEN PUT SPACE(1) v-spec-no[i].
            IF v-prep[i] <> "" THEN PUT v-prep[i] AT 90.
            IF v-spec-no[i] <> "" OR v-prep[i] <> "" THEN PUT SKIP.
        END.
********************************************/
        ASSIGN
           i = 1
           v-fgitm = "".

        FOR EACH tt-fgitm.
            DELETE tt-fgitm.
        END.

        FOR EACH xjob-hdr WHERE xjob-hdr.company EQ cocode
                            AND xjob-hdr.job     EQ job-hdr.job
                            AND xjob-hdr.job-no  EQ job-hdr.job-no
                            AND xjob-hdr.job-no2 EQ job-hdr.job-no2
                            AND xjob-hdr.frm     EQ job-hdr.frm NO-LOCK BY xjob-hdr.blank-no:
           
           FIND FIRST xoe-ordl
                WHERE xoe-ordl.company EQ xjob-hdr.company
                  AND xoe-ordl.ord-no  EQ xjob-hdr.ord-no
                  AND xoe-ordl.job-no  EQ xjob-hdr.job-no
                  AND xoe-ordl.job-no2 EQ xjob-hdr.job-no2
                  AND xoe-ordl.i-no    EQ xjob-hdr.i-no
                  NO-LOCK NO-ERROR.
           IF AVAIL xoe-ordl THEN v-fgqty[i] = xoe-ordl.cas-cnt.
           
           FIND FIRST b-eb WHERE b-eb.company EQ xjob-hdr.company
                             AND b-eb.est-no  EQ xjob-hdr.est-no
                             AND b-eb.form-no = xjob-hdr.frm
                             AND b-eb.blank-no = xjob-hdr.blank-no
                             AND b-eb.stock-no = xjob-hdr.i-no
                             NO-LOCK NO-ERROR.
           IF NOT AVAIL b-eb THEN 
               FIND FIRST b-eb WHERE b-eb.company EQ xjob-hdr.company
                             AND b-eb.est-no  EQ xjob-hdr.est-no
                             AND b-eb.form-no = xjob-hdr.frm
                             AND b-eb.stock-no = xjob-hdr.i-no
                             NO-LOCK NO-ERROR.
           CREATE tt-fgitm.
           ASSIGN tt-fgitm.i-no = xjob-hdr.i-no
                  tt-fgitm.qty =  IF AVAIL xoe-ordl AND xoe-ordl.cas-cnt <> 0 THEN xoe-ordl.cas-cnt 
                                  ELSE IF AVAIL b-eb THEN b-eb.cas-cnt ELSE xjob-hdr.qty
                  tt-fgitm.i-dscr = IF AVAIL b-eb THEN b-eb.part-dscr1 ELSE xjob-hdr.i-no
                  tt-fgitm.po-no = IF AVAIL xoe-ordl THEN xoe-ordl.po-no ELSE ""
                  tt-fgitm.seq = i
                  i = i + 1.

           FIND FIRST b-est WHERE
                b-est.company  EQ xjob-hdr.company AND
                b-est.est-no   EQ xjob-hdr.est-no
                NO-LOCK NO-ERROR.

           IF AVAIL b-est AND b-est.est-type EQ 4 THEN /*combo*/
           DO:
              FIND FIRST b-cust WHERE
                   b-cust.company EQ xjob-hdr.company AND
                   b-cust.cust-no EQ xjob-hdr.cust-no
                   NO-LOCK NO-ERROR.

              IF AVAIL b-cust THEN
              DO:
                 tt-fgitm.cust-name = b-cust.NAME.

                 FIND FIRST b-oe-ordl WHERE
                      b-oe-ordl.company EQ xjob-hdr.company AND
                      b-oe-ordl.ord-no  EQ xjob-hdr.ord-no AND
                      b-oe-ordl.job-no  EQ xjob-hdr.job-no AND
                      b-oe-ordl.job-no2 EQ xjob-hdr.job-no2 AND
                      b-oe-ordl.i-no    EQ xjob-hdr.i-no
                      NO-LOCK NO-ERROR.

                 IF AVAIL b-oe-ordl THEN 
                    FIND FIRST b-oe-rel WHERE
                         b-oe-rel.company EQ cocode AND
                         b-oe-rel.ord-no  EQ b-oe-ordl.ord-no AND
                         b-oe-rel.i-no    EQ b-oe-ordl.i-no AND
                         b-oe-rel.line    EQ b-oe-ordl.LINE
                         NO-LOCK NO-ERROR.

                  IF AVAIL b-oe-rel THEN DO:
                     FIND FIRST b-shipto WHERE
                          b-shipto.company EQ cocode AND
                          b-shipto.cust-no EQ b-oe-rel.cust-no AND
                          b-shipto.ship-id EQ b-oe-rel.ship-id
                          NO-LOCK NO-ERROR.  
                 
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

           /* IF i > 10 THEN LEAVE.*/

        END.
        
        IF s-prt-shipto THEN DO i = 1 TO 4:
            ASSIGN v-shipto1[i] = v-shipto[i]
                   v-shipto2[i] = v-shipto[i].
        END.
        ASSIGN v-cust-name2 = v-cust-name 
               v-cust-name3 = v-cust-name
               .
        /* label prints per item */

IF NOT s-prt-label THEN PUT SKIP v-fill SKIP.
ELSE  DO:

        i = 0.
        j = 0.
        
        FOR EACH tt-fgitm BY tt-fgitm.seq.
            
          IF PAGE-SIZE - LINE-COUNTER < 15 THEN PAGE.

          ASSIGN i = i + 1
                 v-fgitm[i] = tt-fgitm.i-no
                 v-fgdsc[i] = tt-fgitm.i-dscr
                 v-fgqty[i] = tt-fgitm.qty
                 v-pono[i] = tt-fgitm.po-no
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

            DISPLAY v-fill SKIP
               "<B><U>LABEL ITEM" + trim(STRING(j - 2)) + "</U>"  FORM "x(22)"
               "<U>LABEL ITEM" + trim(STRING(j - 1)) + "</U>" FORM "x(20)" WHEN v-fgitm[2] <> "" AT 55
               "<U>LABEL ITEM" + TRIM(STRING(j)) + "</U></B>" FORM "x(23)" WHEN v-fgitm[3] <> "" AT 107
               SKIP
               "Job#:" v-job-no + "-" + string(v-job-no2)
               "Job#:" WHEN v-fgitm[2] <> ""  AT 45
                v-job-no + "-" + string(v-job-no2)   WHEN v-fgitm[2] <> "" 
               "Job#:" WHEN v-fgitm[3] <> "" AT 90  
               v-job-no + "-" + string(v-job-no2) WHEN v-fgitm[3] <> "" 
               SKIP
               "Customer:" v-cust-name 
               "Customer:"  WHEN v-fgitm[2] <> ""  AT 45 v-cust-name2  WHEN v-fgitm[2] <> "" 
               "Customer:" WHEN v-fgitm[3] <> "" AT 90  v-cust-name3 WHEN v-fgitm[3] <> "" 
               SKIP
               "Purchase Order#:" v-pono[1]
               "Purchase Order#:"  WHEN v-fgitm[2] <> "" AT 45 v-pono[2]  WHEN v-fgitm[2] <> "" 
               "Purchase Order#:" WHEN v-fgitm[3] <> "" AT 90  v-pono[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "<B>FG Item #:</B>" v-fgitm[1]
               "<B>FG Item #:</B>"  WHEN v-fgitm[2] <> "" AT 52 v-fgitm[2] WHEN v-fgitm[2] <> "" 
               "<B>FG Item #:</B>" WHEN v-fgitm[3] <> "" AT 104 v-fgitm[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "Description:" v-fgdsc[1]
               "Description:"  WHEN v-fgitm[2] <> "" AT 45 v-fgdsc[2]  WHEN v-fgitm[2] <> "" 
               "Description:" WHEN v-fgitm[3] <> "" AT 90  v-fgdsc[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "Count:   " v-fgqty[1]
               "Count:   "  WHEN v-fgitm[2] <> "" AT 45 v-fgqty[2]  WHEN v-fgitm[2] <> "" 
               "Count:   "  WHEN v-fgitm[3] <> "" AT 90  v-fgqty[3] WHEN v-fgitm[3] <> ""                               
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
            
            ASSIGN i = 0
                   v-fgitm[1] = ""
                   v-fgdsc[1] = ""
                   v-fgqty[1] = 0
                   v-fgitm[2] = ""
                   v-fgdsc[2] = ""
                   v-fgqty[2] = 0
                   v-fgitm[3] = ""
                   v-fgdsc[3] = ""
                   v-fgqty[3] = 0
                   v-pono[1] = ""
                   v-pono[2] = ""
                   v-pono[3] = ""
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
            IF PAGE-SIZE - LINE-COUNTER < 15 THEN PAGE.

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
               "<B><U>LABEL ITEM" + trim(STRING(v-last-j + 1)) + "</U>"  FORM "x(22)"
               "<U>LABEL ITEM" + trim(STRING(v-last-j + 2)) + "</U>" FORM "x(20)" WHEN v-fgitm[2] <> "" AT 55
               "<U>LABEL ITEM" + TRIM(STRING(v-last-j + 3)) + "</U></B>" FORM "x(23)" WHEN v-fgitm[3] <> "" AT 107
               SKIP
               "Job#:" v-job-no + "-" + string(v-job-no2)
               "Job#:" WHEN v-fgitm[2] <> ""  AT 45
                v-job-no + "-" + string(v-job-no2) WHEN v-fgitm[2] <> "" 
               "Job#:" WHEN v-fgitm[3] <> "" AT 90  
               v-job-no + "-" + string(v-job-no2) WHEN v-fgitm[3] <> "" 
               SKIP
               "Customer:" v-cust-name 
               "Customer:"  WHEN v-fgitm[2] <> ""  AT 45 v-cust-name2  WHEN v-fgitm[2] <> "" 
               "Customer:" WHEN v-fgitm[3] <> "" AT 90  v-cust-name3 WHEN v-fgitm[3] <> "" 
               SKIP
               "Purchase Order#:" v-pono[1]
               "Purchase Order#:"  WHEN v-fgitm[2] <> "" AT 45 v-pono[2]  WHEN v-fgitm[2] <> "" 
               "Purchase Order#:" WHEN v-fgitm[3] <> "" AT 90  v-pono[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "<B>FG Item #:</B>" v-fgitm[1]
               "<B>FG Item #:</B>"  WHEN v-fgitm[2] <> "" AT 52 v-fgitm[2] WHEN v-fgitm[2] <> "" 
               "<B>FG Item #:</B>" WHEN v-fgitm[3] <> "" AT 104 v-fgitm[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "Description:" v-fgdsc[1]
               "Description:"  WHEN v-fgitm[2] <> "" AT 45 v-fgdsc[2]  WHEN v-fgitm[2] <> "" 
               "Description:" WHEN v-fgitm[3] <> "" AT 90  v-fgdsc[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "Count:   " v-fgqty[1]
               "Count:   "  WHEN v-fgitm[2] <> "" AT 45 v-fgqty[2]  WHEN v-fgitm[2] <> "" 
               "Count:   "  WHEN v-fgitm[3] <> "" AT 90  v-fgqty[3] WHEN v-fgitm[3] <> ""      
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
END. /* s-prt-label*/           
        PAGE.
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
      
      v-first = NO.

END. /* for first job-hdr */  
    
/*     if v-format eq "Fibre" then page. */

/* end ---------------------------------- copr. 1994  advanced software, inc. */
