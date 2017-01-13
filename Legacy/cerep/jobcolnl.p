/* ----------------------------------------------- cerep/jobcolnl.p  03/05YSK */
/*  factory ticket  for folding Colonial                                      */
/* -------------------------------------------------------------------------- */
DEFINE INPUT PARAMETER v-format LIKE sys-ctrl.char-fld.

{sys/inc/var.i shared}
{sys/form/s-top.f}

{jcrep/r-ticket.i "shared"}

{cerep\tt-samp-ctn.i}

DEFINE NEW SHARED VARIABLE save_id AS RECID.
DEFINE NEW SHARED VARIABLE v-today AS DATE INITIAL TODAY.
DEFINE NEW SHARED VARIABLE v-job AS CHARACTER FORMAT "x(6)" EXTENT 2 INITIAL [" ","zzzzzz"].
DEFINE NEW SHARED VARIABLE v-job2 AS INTEGER FORMAT "99" EXTENT 2 INITIAL [00,99].
DEFINE NEW SHARED VARIABLE v-stypart LIKE style.dscr.
DEFINE NEW SHARED VARIABLE v-dsc LIKE oe-ordl.part-dscr1 EXTENT 2.
DEFINE NEW SHARED VARIABLE v-size AS CHARACTER FORMAT "x(26)" EXTENT 2.
DEFINE NEW SHARED VARIABLE v-bld-job LIKE oe-ord.job-no.
DEFINE NEW SHARED VARIABLE v-bld-job2 LIKE oe-ord.job-no2.
DEFINE NEW SHARED VARIABLE v-fill AS CHARACTER FORMAT "x(128)".
DEFINE NEW SHARED VARIABLE v-frst AS LOGICAL.
DEFINE NEW SHARED VARIABLE v-ok AS LOGICAL.
DEFINE NEW SHARED VARIABLE v-est-qty AS INTEGER FORMAT "->>,>>>,>>9".
DEFINE NEW SHARED VARIABLE v-job-qty AS INTEGER FORMAT "->>,>>>,>>9".
DEFINE NEW SHARED VARIABLE v-fac AS DECIMAL .
DEFINE NEW SHARED VARIABLE v-job-no LIKE oe-ordl.job-no.
DEFINE NEW SHARED VARIABLE v-job-no2 LIKE oe-ordl.job-no2.
DEFINE VARIABLE v-pricnt-id AS CHARACTER NO-UNDO .
DEFINE NEW SHARED VARIABLE v-due-date LIKE oe-ord.due-date.
DEFINE NEW SHARED VARIABLE v-reprint AS LOGICAL.
DEFINE NEW SHARED VARIABLE v-up LIKE eb.num-up.
DEFINE NEW SHARED VARIABLE v-tandem AS LOGICAL.
DEFINE NEW SHARED VARIABLE v-form-no LIKE eb.form-no.
DEFINE NEW SHARED VARIABLE v-fup AS CHARACTER.
DEFINE NEW SHARED VARIABLE v-layout AS CHARACTER FORMAT "x(30)".
DEFINE VARIABLE v-case-count LIKE eb.cas-cnt NO-UNDO.
DEFINE VARIABLE v-case-qty AS INTEGER NO-UNDO.
DEFINE VARIABLE v-spc-no LIKE eb.spc-no NO-UNDO.
DEFINE VARIABLE v-upc-no LIKE eb.upc-no NO-UNDO.
DEFINE VARIABLE v-line AS INTEGER INITIAL 1 NO-UNDO.
DEFINE VARIABLE v-gsh-qty AS INTEGER NO-UNDO.
DEFINE VARIABLE cnt AS INTEGER INITIAL 1 NO-UNDO.
DEFINE VARIABLE v-frm-blk AS CHARACTER FORMAT "x(6)" NO-UNDO.
DEFINE VARIABLE v-dec AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-ovund AS CHARACTER FORMAT "x(34)" NO-UNDO.
DEFINE VARIABLE v-mrhr AS CHARACTER FORMAT "x(5)".
DEFINE VARIABLE v-cas-dscr LIKE item.est-dscr.
DEFINE VARIABLE v-first AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-spec-list AS CHARACTER FORMAT "x(20)"INITIAL "QA" NO-UNDO.
DEFINE VARIABLE lv-form-note AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-itm-printed AS INTEGER NO-UNDO.
DEFINE VARIABLE v-alloc AS cha NO-UNDO.
DEFINE VARIABLE v-per-ord AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-prep AS CHARACTER EXTENT 8 NO-UNDO.
DEFINE VARIABLE v-misc AS CHARACTER EXTENT 6 NO-UNDO.
DEFINE VARIABLE v-spec-no AS CHARACTER EXTENT 8 NO-UNDO.
DEFINE VARIABLE v-skip AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-fill2 AS CHARACTER INITIAL "-" FORM "x(125)" NO-UNDO.
DEFINE VARIABLE lv-text AS CHARACTER NO-UNDO.
DEFINE VARIABLE li AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-under-run AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-over-run AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-cust-name-extent AS CHARACTER EXTENT 10 NO-UNDO.
DEFINE VARIABLE v-ship1-extent AS CHARACTER EXTENT 10 NO-UNDO.
DEFINE VARIABLE v-ship2-extent AS CHARACTER EXTENT 10 NO-UNDO.
DEFINE VARIABLE v-ship4-extent AS CHARACTER EXTENT 10 NO-UNDO.
DEFINE VARIABLE cBarCode AS CHARACTER FORMAT "X(11)"  NO-UNDO.
DEFINE VARIABLE v-unit-per-dec AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-unit-per-int LIKE eb.cas-cnt NO-UNDO.
DEFINE VARIABLE v-job-qty-unit-per-int AS INTEGER FORMAT ">>>>>9" NO-UNDO.
DEFINE VARIABLE v-job-qty-unit-per-dec AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-job-qty-boxes-code-int AS INTEGER NO-UNDO.
DEFINE VARIABLE v-job-qty-boxes-code-dec AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-dc-gl-speed AS INTEGER NO-UNDO.
DEFINE VARIABLE v-dc-out LIKE est-op.n-out NO-UNDO.
DEFINE VARIABLE v-dc-only-out LIKE est-op.n-out NO-UNDO.
DEFINE VARIABLE v-shink-wrap AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-shrink-wrap AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-sample-on-cnt AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-cas-wt AS DECIMAL FORMAT ">>>>9.99" NO-UNDO.
DEFINE BUFFER b-est FOR est.
DEFINE BUFFER b-oe-ordl FOR oe-ordl.
DEFINE BUFFER b-oe-rel FOR oe-rel.
DEFINE BUFFER b-shipto FOR shipto.
DEFINE BUFFER b-cust FOR cust.
DEFINE BUFFER ref-side FOR reftable.

DEFINE TEMP-TABLE w-lo NO-UNDO
  FIELD layout LIKE v-layout.

DEFINE NEW SHARED BUFFER xjob-hdr FOR job-hdr.

DEFINE BUFFER b-eb FOR eb.
DEFINE BUFFER bff-eb FOR eb.

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
  /*field gsh-qty like ef.gsh-qty*/
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
  FIELD i-side AS CHARACTER FORMAT "X(4)".

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
DEFINE VARIABLE v-inst2 AS CHARACTER EXTENT 25 NO-UNDO.    
DEFINE VARIABLE v-dept-inst AS CHARACTER FORM "x(80)" EXTENT 20 NO-UNDO.
DEFINE VARIABLE v-note-length AS INTEGER INITIAL 80 NO-UNDO.

DEFINE VARIABLE v-start-date AS DATE NO-UNDO.
DEFINE VARIABLE v-req-date AS DATE NO-UNDO.
DEFINE VARIABLE v-shipto AS CHARACTER FORMAT "x(30)" EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-case-size AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-vend LIKE po-ord.vend-no NO-UNDO.
DEFINE VARIABLE v-item AS CHARACTER EXTENT 100 NO-UNDO.
DEFINE VARIABLE v-i-qty AS DECIMAL EXTENT 100 NO-UNDO.
DEFINE VARIABLE v-ink1 AS CHARACTER EXTENT 100 NO-UNDO.
DEFINE VARIABLE v-ink2 AS CHARACTER EXTENT 100 NO-UNDO.
DEFINE VARIABLE v-po-no LIKE oe-ordl.po-no NO-UNDO.
DEFINE VARIABLE lv-mat-dept-list AS CHARACTER INITIAL "FB,FS,WN,WS,GL" NO-UNDO.
DEFINE VARIABLE v-mat-for-mach AS CHARACTER NO-UNDO.
DEFINE BUFFER xjob-mat FOR job-mat.
DEFINE BUFFER bf-job-mat FOR job-mat.
DEFINE VARIABLE v-fgitm AS CHARACTER FORM "x(15)" EXTENT 10 NO-UNDO.
DEFINE VARIABLE v-fgdsc LIKE eb.part-dscr1 EXTENT 10 NO-UNDO.
DEFINE VARIABLE v-fgqty LIKE job-hdr.qty EXTENT 10 FORM ">>,>>>,>>>" NO-UNDO.
DEFINE VARIABLE v-pono LIKE oe-ordl.po-no EXTENT 10 NO-UNDO.
DEFINE VARIABLE v-num-of-fgitm AS INTEGER NO-UNDO.
DEFINE TEMP-TABLE tt-fgitm NO-UNDO
     FIELD i-no AS CHARACTER FORMAT "x(15)"
     FIELD seq AS INTEGER
     FIELD qty AS INTEGER 
     FIELD i-dscr AS CHARACTER
     FIELD po-no AS CHARACTER
     FIELD cust-name AS CHARACTER
     FIELD shipto1 AS CHARACTER
     FIELD shipto2 AS CHARACTER
     FIELD shipto4 AS CHARACTER.
DEFINE VARIABLE v-board-po LIKE oe-ordl.po-no-po NO-UNDO.
DEFINE VARIABLE v-plate-printed AS LOGICAL NO-UNDO.
DEFINE BUFFER xoe-ordl FOR oe-ordl.
DEFINE VARIABLE v-cust-name LIKE oe-ord.cust-name NO-UNDO.
DEFINE VARIABLE v-cust-name2 LIKE oe-ord.cust-name NO-UNDO.
DEFINE VARIABLE v-cust-name3 LIKE oe-ord.cust-name NO-UNDO.
DEFINE VARIABLE v-last-j AS INTEGER NO-UNDO.
DEFINE VARIABLE v-po-no2 LIKE v-po-no NO-UNDO.
DEFINE VARIABLE v-po-no3 LIKE v-po-no NO-UNDO.
DEFINE VARIABLE v-lbs AS DECIMAL FORMAT ">>,>>>,>>9" NO-UNDO.
DEFINE VARIABLE v-dept-title AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-dept-note-printed AS LOGICAL.
DEFINE VARIABLE v-cust-lot# AS CHARACTER NO-UNDO.

DEFINE VARIABLE v-case-size-ext AS CHARACTER EXTENT 10 NO-UNDO.
DEFINE VARIABLE v-case-qty-ext AS DECIMAL EXTENT 10 NO-UNDO.
DEFINE VARIABLE v-up-ext AS INTEGER EXTENT 10 NO-UNDO.
DEFINE VARIABLE vext AS INTEGER NO-UNDO.
DEFINE VARIABLE ij AS INTEGER NO-UNDO.

v-fill = "<||3><C1><FROM><C109><LINE><||3>".

DEFINE NEW SHARED FRAME head.
DEFINE NEW SHARED FRAME head2.

DEFINE SHARED VARIABLE s-prt-mstandard AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE s-prt-shipto AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE s-prt-sellprc AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-po-duedate LIKE po-ordl.due-date NO-UNDO.
DEFINE VARIABLE v-upc-lbl AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE v-shipto1 AS CHARACTER FORMAT "x(30)" EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-shipto2 AS CHARACTER FORMAT "x(30)" EXTENT 4 NO-UNDO.
DEFINE SHARED VARIABLE s-run-speed AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-pass-count AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE s-prt-label AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-lp-dep AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-lp-qty AS INTEGER NO-UNDO.
DEFINE VARIABLE ord-qty AS INTEGER NO-UNDO .

DEFINE VARIABLE cDraftImage AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDraftImageFull AS CHARACTER FORMAT "x(50)" NO-UNDO.
ASSIGN cDraftImage = "images\draft.jpg"

FILE-INFO:FILE-NAME = cDraftImage.
cDraftImageFull = IF lDraft 
                    THEN  "<C25><#1><R+80><C+50><IMAGE#1=" + FILE-INFO:FULL-PATHNAME + ">" 
                    ELSE "".

FORMAT HEADER
        cDraftImageFull FORMAT "x(100)" SKIP 
       "<C1><R2>JOB NUMBER:<B>" v-job-no SPACE(0) "-" SPACE(0) v-job-no2 FORMAT "99" "</B>" SPACE(1)
       "CSR:" v-pricnt-id
       "<B><P12>F A C T O R Y  T I C K E T</B><P10>" AT 54  "JOB START DATE:" AT 124 v-start-date SKIP
       v-fill
    WITH NO-BOX FRAME head NO-LABELS STREAM-IO WIDTH 155.

FORMAT "Customer:" oe-ord.cust-name "Sold To:" oe-ord.sold-id
       "Salesman:" AT 68 oe-ord.sname[1] "Order#:" AT 113 oe-ord.ord-no
    WITH NO-BOX FRAME line-head NO-LABELS STREAM-IO WIDTH 132.
    
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
        WHERE job-hdr.company               EQ cocode
          AND job-hdr.job-no                GE SUBSTRING(fjob-no,1,6)
          AND job-hdr.job-no                LE SUBSTRING(tjob-no,1,6)
          AND FILL(" ",6 - LENGTH(TRIM(job-hdr.job-no))) +
              TRIM(job-hdr.job-no) +
              STRING(job-hdr.job-no2,"99")  GE fjob-no
          AND FILL(" ",6 - LENGTH(TRIM(job-hdr.job-no))) +
              TRIM(job-hdr.job-no) +
              STRING(job-hdr.job-no2,"99")  LE tjob-no
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
          AND est.est-type LE 4  

        BREAK BY job-hdr.job
              BY job-hdr.job-no
              BY job-hdr.job-no2
              BY job-hdr.frm:

      /*check product line from job no   */
      FIND FIRST eb NO-LOCK WHERE eb.company  EQ est.company  
                              AND  eb.loc     EQ  est.loc 
                              AND  eb.est-no  EQ est.est-no NO-ERROR.

       IF AVAILABLE eb THEN
       DO:
          IF CAN-FIND(FIRST prodl WHERE prodl.company EQ cocode AND
                            (prodl.prolin EQ 'Printed' 
                             OR prodl.prolin EQ 'Labels') AND
                            prodl.procat EQ eb.procat) THEN
          DO:
             IF est.est-type NE 4 OR (est.est-type EQ 4 AND FIRST-OF(job-hdr.job)) THEN 
             RUN cerep/jobcolnlp2.p (job-hdr.job-no, job-hdr.job-no2).
             NEXT.
          END.        

          RELEASE eb.
       END.
      
      FIND FIRST job NO-LOCK
        WHERE job.company EQ cocode
          AND job.job     EQ job-hdr.job
          AND job.job-no  EQ job-hdr.job-no
          AND job.job-no2 EQ job-hdr.job-no2
        NO-ERROR.

      IF production AND
         job.cs-trans-date NE ? THEN DO:
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
      
      ELSE DO:
        li = 0.
        IF NOT job-hdr.ftick-prnt THEN DO WHILE li LT 1000:
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
          IF AVAILABLE job THEN DO:
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
      IF v-first THEN DO:
        

        IF AVAILABLE oe-ord THEN
          IF NOT oe-ctrl.p-fact AND oe-ord.stat EQ "H" THEN NEXT.

          FIND FIRST cust NO-LOCK WHERE cust.company EQ job-hdr.company AND
                                        cust.cust-no EQ job-hdr.cust-no NO-ERROR.

          IF AVAILABLE cust THEN DO:
              ASSIGN v-pricnt-id = "" . 
              FOR EACH empalert NO-LOCK WHERE empalert.table_rec_key EQ cust.rec_key,
                  FIRST users NO-LOCK WHERE users.user_id EQ empalert.USER-ID :

                  IF empalert.spare-char-1 EQ "YES" THEN DO:
                      ASSIGN v-pricnt-id = users.USER_id .
                      LEAVE.
                  END.
              END.
          END.    

        ASSIGN
           v-job-no  = job-hdr.job-no
           v-job-no2 = job-hdr.job-no2
           /*v-due-date = if avail oe-ord then oe-ord.due-date else ?*/
           v-start-date = job-hdr.start-date
           v-shipto = "" .
            

        IF AVAILABLE job AND job.stat EQ "H" THEN DO:
            ASSIGN cDraftImage = "images\on-hold.jpg"
                FILE-INFO:FILE-NAME = cDraftImage.
            cDraftImageFull = "<C25><#1><R+80><C+50><IMAGE#1=" + FILE-INFO:FULL-PATHNAME + ">"  .
        END.

        IF NOT FIRST(job-hdr.job-no) THEN PAGE.
        VIEW FRAME head.
       
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
        IF AVAILABLE oe-rel THEN DO:
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
        FIND FIRST cust NO-LOCK WHERE cust.company EQ job-hdr.company AND
                                      cust.cust-no EQ job-hdr.cust-no NO-ERROR.

        ASSIGN
           v-req-date = IF AVAILABLE oe-ordl THEN oe-ordl.req-date ELSE ?
           v-cust-name = IF AVAILABLE oe-ord THEN oe-ord.cust-name 
                         ELSE IF AVAILABLE cust THEN cust.name
                         ELSE job-hdr.cust-no
           
           lv-over-run = IF AVAILABLE oe-ordl THEN TRIM(STRING(oe-ordl.over-pct,">>9.99%")) ELSE
                         IF AVAILABLE oe-ord  THEN TRIM(STRING(oe-ord.over-pct,">>9.99%"))  ELSE ""
           lv-under-run = IF AVAILABLE oe-ordl THEN TRIM(STRING(oe-ordl.under-pct,">>9.99%")) ELSE
                          IF AVAILABLE oe-ord  THEN TRIM(STRING(oe-ord.under-pct,">>9.99%"))  ELSE ""
           v-due-date = IF AVAILABLE oe-ordl THEN oe-ordl.prom-date ELSE ?
           ord-qty     = IF AVAILABLE oe-ordl THEN oe-ordl.qty ELSE 0 .
           IF AVAILABLE oe-ord THEN
            v-per-ord   = IF oe-ord.po-no2 NE "" THEN oe-ord.po-no2 ELSE STRING(oe-ord.pord-no) .
           IF AVAILABLE oe-ord AND oe-ord.TYPE EQ "T" AND oe-ord.pord-no GT 0 THEN
               v-per-ord = STRING(oe-ord.pord-no).


        FIND FIRST eb NO-LOCK WHERE eb.company     EQ job-hdr.company
                                AND eb.est-no      EQ job-hdr.est-no
                                AND eb.form-no     EQ job-hdr.frm
                                AND eb.stock-no    EQ job-hdr.i-no NO-ERROR.
        IF NOT AVAILABLE eb THEN FIND FIRST eb NO-LOCK WHERE eb.company     EQ job-hdr.company
                                                         AND eb.est-no      EQ job-hdr.est-no
                                                         AND eb.form-no     EQ job-hdr.frm
                                                         AND eb.blank-no    GT 0 NO-ERROR.
        v-spc-no = IF AVAILABLE eb THEN eb.spc-no ELSE "".
        v-upc-no = IF AVAILABLE eb THEN eb.upc-no ELSE "".

        v-job-qty = 0.
          IF AVAILABLE eb THEN
           FOR EACH xjob-hdr FIELDS(qty) NO-LOCK WHERE
               xjob-hdr.company EQ cocode
               AND xjob-hdr.job     EQ job-hdr.job
               AND xjob-hdr.job-no  EQ job-hdr.job-no
               AND xjob-hdr.job-no2 EQ job-hdr.job-no2
               AND xjob-hdr.i-no    EQ eb.stock:
               v-job-qty = v-job-qty + xjob-hdr.qty.
           END.

        cBarCode = TRIM(v-job-no) + "-" + STRING(v-job-no2,"99").
        PUT "<AT=-.5,6.3><FROM><AT=+.3,+1.7><BARCODE,TYPE=39,CHECKSUM=NONE,BarHeightPixels=2,VALUE=" cBarCode FORMAT "X(11)" ">"
            "<C1><R4><B>Customer Name:</B>" v-cust-name  "Code: " job-hdr.cust-no 
            "   <B>REQ DATE:   MFG DATE:   Estimate:            Print Date:" SKIP
            "Shipto:</B>" v-shipto[1] SPACE(6) "Prev.Ord#:" v-per-ord v-req-date AT 67 v-due-date AT 79 TRIM(job-hdr.est-no) FORMAT "x(8)" AT 92
            TODAY FORMAT "99/99/9999" AT 112
            SKIP
            v-shipto[2] AT 7 /*"Order Qty:" AT 62   "Job Qty:" AT 87*/  STRING(TIME,"HH:MM am/pm") AT 107 " by " USERID("nosweat")
            v-shipto[4] AT 7  /*"QC/SPC#:" AT 38 v-spc-no*/
            /*ord-qty format "->,>>>,>>9" AT 62 v-job-qty format "->,>>>,>>9" AT 85*/  SKIP
            /*"Pharma Code:" AT 38 v-upc-no*/ "Overrun: " AT 65 lv-over-run "Underrun: " AT 86 lv-under-run SKIP
            v-fill SKIP.     

        v-line = IF AVAILABLE est                            AND
                 est.est-type GT 2 AND est.est-type LT 5 THEN 500 ELSE 50.

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
              AND job-mch.frm = job-hdr.frm ,

            FIRST mach
            {sys/ref/machW.i}
              AND mach.m-code EQ job-mch.m-code
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
          IF NOT AVAILABLE wrk-op THEN DO:
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
              AND job-prep.job-no2 EQ job-hdr.job-no2 :
          FIND FIRST prep NO-LOCK
              WHERE prep.company EQ cocode
                AND prep.code    EQ job-prep.code
              NO-ERROR.
          CREATE wrk-prep.
          ASSIGN
           wrk-prep.code = job-prep.code
           wrk-prep.dscr = IF AVAILABLE prep THEN prep.dscr ELSE ""
           wrk-prep.s-num = job-prep.frm
           wrk-prep.b-num = job-prep.blank-no
           wrk-prep.ml = job-prep.ml.
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
        FOR EACH oe-ordm 
            WHERE oe-ordm.company EQ cocode
              AND oe-ordm.ord-no  EQ oe-ord.ord-no
            NO-LOCK:
          FIND FIRST wrk-prep WHERE wrk-prep.code EQ oe-ordm.charge NO-ERROR.
          IF NOT AVAILABLE wrk-prep THEN DO:
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
        FOR EACH eb FIELDS(yld-qty) NO-LOCK
            WHERE eb.company  EQ ef.company
              AND eb.est-no   EQ ef.est-no
              AND eb.stock-no EQ job-hdr.i-no:
          v-est-qty = v-est-qty + eb.yld-qty.
        END.

        ELSE v-fac = 1.
        v-itm-printed = 0.

        IF ef.form-no EQ job-hdr.frm THEN ebloop:
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
            IF LENGTH(TRIM(v-fup)) + LENGTH(TRIM(w-lo.layout)) GT 30 THEN DO:
              SUBSTRING(w-lo.layout,LENGTH(TRIM(w-lo.layout)),1) = "".
              CREATE w-lo.
            END.
            w-lo.layout = TRIM(w-lo.layout + " " + trim(v-fup) + ",").
            IF LAST(b-eb.est-no) THEN
              SUBSTRING(w-lo.layout,LENGTH(TRIM(w-lo.layout)),1) = "".
          END.
          
          FIND FIRST wrk-die WHERE wrk-die.die-no EQ eb.die-no NO-ERROR.
          IF NOT AVAILABLE wrk-die AND eb.die-no GT "" THEN DO:
            CREATE wrk-die.
            ASSIGN wrk-die.die-no = eb.die-no
                   wrk-die.cad-no = eb.cad-no
              wrk-die.form-no     = eb.form-no
              wrk-die.die-size    = STRING(ef.trim-w) + "x" +
              STRING(ef.trim-l).
          END.

          FIND FIRST ref-side NO-LOCK WHERE
               ref-side.reftable EQ "ce/v-est3.w Unit#"  AND
               ref-side.company  EQ eb.company AND
               ref-side.loc      EQ eb.est-no AND
               ref-side.code     EQ STRING(eb.form-no,"9999999999") AND
               ref-side.code2    EQ STRING(eb.blank-no,"9999999999")
               NO-ERROR.

          /** BUILD INK WORK FILE **/
          FOR EACH job-mat NO-LOCK
              WHERE job-mat.company EQ cocode
                AND job-mat.job     EQ job-hdr.job
                AND job-mat.frm     EQ eb.form-no,
              FIRST ITEM no-lock
              {sys/look/itemivW.i}
                AND item.i-no EQ job-mat.i-no:

            DO i = 1 TO 20:
              IF eb.i-code2[i] EQ job-mat.i-no THEN DO:

                FIND FIRST wrk-ink
                    WHERE wrk-ink.i-code   EQ eb.i-code2[i]
                      AND wrk-ink.form-no  EQ eb.form-no
                      AND wrk-ink.blank-no EQ eb.blank-no
                      AND wrk-ink.i-pass   EQ eb.i-ps2[i]
                    NO-ERROR.
                 
                IF NOT AVAILABLE wrk-ink THEN DO:
                  CREATE wrk-ink.
                  ASSIGN
                   wrk-ink.i-code   = eb.i-code2[i]
                   wrk-ink.form-no  = eb.form-no
                   wrk-ink.blank-no = eb.blank-no
                   wrk-ink.i-dscr   = eb.i-dscr2[i]
                   wrk-ink.i-pass   = eb.i-ps2[i].

                  IF i LE 12 THEN DO:
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
                  ELSE DO:
                      FIND FIRST ref-side NO-LOCK WHERE
                          ref-side.reftable EQ "ce/v-est3.w Unit#1"  AND
                          ref-side.company  EQ eb.company AND
                          ref-side.loc      EQ eb.est-no AND
                          ref-side.code     EQ STRING(eb.form-no,"9999999999") AND
                          ref-side.code2    EQ STRING(eb.blank-no,"9999999999")
                          NO-ERROR.
                       IF AVAILABLE ref-side THEN
                     wrk-ink.i-side = FILL(" ",5) + SUBSTRING(ref-side.dscr,i - 12,1).
                  END.                                                                
                 
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
                (job-mat.blank-no EQ 0 AND eb.blank-no EQ 1)) THEN DO:
              CREATE wrk-ink.
              ASSIGN
               wrk-ink.i-code   = job-mat.i-no
               wrk-ink.form-no  = eb.form-no
               wrk-ink.blank-no = eb.blank-no
               wrk-ink.i-dscr   = item.est-dscr
               wrk-ink.i-pass   = 1.
            END.
            IF AVAILABLE wrk-ink AND
               ((est.est-type EQ 4 AND eb.form-no EQ job-mat.frm AND eb.blank-no EQ job-mat.blank-no) OR
                 est.est-type NE 4 ) 
                 THEN wrk-ink.i-qty = wrk-ink.i-qty + job-mat.qty.            
            
          END. /* JOB-MAT */

          IF eb.est-type EQ 4 THEN v-fac = eb.yld-qty / v-est-qty.
          
        /*  if last-of(eb.form-no) then do: */
            FIND FIRST style NO-LOCK
                WHERE style.company EQ eb.company
                  AND style.style   EQ eb.style
                NO-ERROR.
            IF AVAILABLE style THEN v-stypart = style.dscr.
            ASSIGN
             v-dsc[1] = eb.part-dscr1
             v-dsc[2] = eb.part-dscr2
             v-size[1] = STRING(eb.len) + "x" + STRING(eb.wid) + "x" +
                         STRING(eb.dep)
             v-size[2] = eb.i-coldscr.

             IF eb.blank-no GT 0 AND eb.blank-no LT 11 THEN 
                 ASSIGN v-fgdsc[eb.blank-no] = eb.part-dscr1.
                                                             
            
            v-upc-lbl = "   CAD#".
           /* IF FIRST-OF(eb.form-no) THEN DO:*/
              PUT
                 "<P9><B>F/B         FG Item #       Cust Part #     Art #            PO#            Customer Lot#     Description                 Cad            # Up   " "</B>" SKIP.  /* Style  Carton Size*/

              ASSIGN v-case-size-ext = ""
                     v-case-qty-ext = 0
                     v-up-ext = 0
                     vext = 0.
           /* END.*/

           v-job-qty = 0.
           FOR EACH xjob-hdr FIELDS(qty) NO-LOCK WHERE
                   xjob-hdr.company EQ cocode
               AND xjob-hdr.job     EQ job-hdr.job
               AND xjob-hdr.job-no  EQ job-hdr.job-no
               AND xjob-hdr.job-no2 EQ job-hdr.job-no2
               AND xjob-hdr.i-no    EQ eb.stock:
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

           IF NOT AVAILABLE oe-ordl THEN
              FIND FIRST oe-ordl NO-LOCK
                WHERE oe-ordl.company EQ job-hdr.company
                  AND oe-ordl.ord-no  EQ job-hdr.ord-no
                  AND oe-ordl.i-no    EQ eb.stock-no
                NO-ERROR.

            IF AVAILABLE oe-ordl THEN DO:
              v-est-qty = oe-ordl.qty.
              FIND FIRST oe-ord OF oe-ordl NO-LOCK.
              v-ovund = STRING("Overrun/Underrun %:  " +
                               TRIM(STRING(oe-ordl.over-pct,">>9.99")) + "/" +
                               TRIM(STRING(oe-ordl.under-pct,">>9.99"))).
              FIND FIRST oe-rel NO-LOCK
                   WHERE oe-rel.company EQ oe-ordl.company
                     AND oe-rel.ord-no  EQ oe-ordl.ord-no
                     AND oe-rel.i-no    EQ oe-ordl.i-no
                     AND oe-rel.line    EQ oe-ordl.line
                   NO-ERROR.
            END.
            ELSE v-est-qty = v-job-qty.
            
            RELEASE w-lo.
            FIND FIRST w-lo NO-ERROR.             

            ASSIGN
                v-case-size = STRING(eb.cas-len) + "x" + STRING(eb.cas-wid) + "x" +
                              STRING(eb.cas-dep)
                v-up = eb.num-up
                v-po-no = IF AVAILABLE oe-ordl THEN oe-ordl.po-no ELSE ""
                v-case-count = IF AVAILABLE eb THEN eb.cas-cnt ELSE
                               IF AVAILABLE oe-ordl AND oe-ordl.cas-cnt NE 0 THEN oe-ordl.cas-cnt
                               ELSE 0
                v-case-qty = ROUND(v-job-qty / v-case-count,0)
                vext = vext + 1
                v-case-size-ext[vext] = v-case-size
                v-case-qty-ext[vext]  = v-case-qty
                v-up-ext[vext]        = v-up.

            IF AVAILABLE oe-rel THEN
               FIND FIRST reftable NO-LOCK WHERE
                          reftable.reftable EQ "oe-rel.lot-no" AND
                          reftable.company  EQ STRING(oe-rel.r-no,"9999999999")
                          NO-ERROR.
            IF AVAILABLE reftable THEN 
               ASSIGN v-cust-lot# = reftable.CODE.
            ELSE
               ASSIGN v-cust-lot# = "".

            FIND FIRST itemfg NO-LOCK WHERE itemfg.company EQ job-hdr.company
                                        AND itemfg.i-no    EQ job-hdr.i-no NO-ERROR.

          IF AVAILABLE itemfg THEN
              DO:
              v-cas-wt = (itemfg.weight-100) * eb.cas-cnt / 100.
          END.
          ELSE v-cas-wt = 0.

            
           DISPLAY v-job-no + "-" + TRIM(STRING(eb.form-no,">>9")) + 
                    trim(STRING(eb.blank-no,">>9")) FORMAT "x(11)" 
                    eb.stock-no @ job-hdr.i-no 
                    (IF AVAILABLE oe-ordl  THEN oe-ordl.part-no ELSE IF AVAILABLE itemfg THEN itemfg.part-no ELSE "") FORMAT "x(15)"   
                    (IF eb.plate-no NE "" THEN eb.plate-no  ELSE IF AVAILABLE itemfg THEN itemfg.plate-no ELSE "" ) FORMAT "x(15)"
                    (IF AVAIL oe-ordl  THEN oe-ordl.po-no ELSE "") FORMAT "x(15)"
                    v-cust-lot#  FORMAT "x(17)"
                    v-dsc[1] FORMAT "x(27)" SPACE(1)
                    eb.cad-no FORMAT "x(15)" /*v-stypart */
                    v-up-ext[vext]        @ v-up FORMAT ">>9"  /*v-size[1] FORM "x(30)"*/
                    SKIP
                WITH STREAM-IO WIDTH 175 NO-LABELS NO-BOX FRAME line-det1.

            v-itm-printed = v-itm-printed + 1.   
            v-spc-no = IF AVAILABLE eb THEN eb.spc-no ELSE "".
            v-upc-no = IF AVAILABLE eb THEN eb.upc-no ELSE "".

              PUT
                 "<P9><B>            Ord Qty         Job Qty         QC/SPC #         Pharma Code     Style            Carton Size                    Case Wt." "</B>" SKIP.  /* Style  Carton Size*/
              PUT (IF AVAILABLE oe-ordl THEN oe-ordl.qty ELSE 0) FORMAT "->,>>>,>>9" AT 10 
                  job-hdr.qty FORMAT "->,>>>,>>9" AT 26
                  v-spc-no  AT 45 
                  v-upc-no  AT 62
                  eb.style FORMAT "x(8)" AT 78 SPACE(10)
                  v-size[1] FORMAT "x(30)" 
                  v-cas-wt  SKIP .

            v-itm-printed = v-itm-printed + 1. 
            /*ord-qty format "->,>>>,>>9" AT 62 v-job-qty format "->,>>>,>>9" AT 85*/  
            /*"Pharma Code:" AT 38 v-upc-no*/

            FIND FIRST ITEM NO-LOCK
                WHERE item.company EQ cocode
                  AND item.i-no    EQ eb.cas-no
                NO-ERROR.
            v-cas-dscr = IF AVAILABLE item THEN item.i-name ELSE "".
          /* end. /* last-of(eb.form-no) */      */
          IF LAST-OF(eb.form-no) THEN DO:
             IF v-itm-printed < 4 THEN PUT SKIP(4 - v-itm-printed).

             /* Number of sheets ticket1.p - single board, ticket2.p - multi board */
             RUN oe/rep/ticket2.p (RECID(ef), RECID(job-hdr)).
             /*find first wrk-sheet where recid(wrk-sheet) eq save_id.*/
             IF AVAILABLE oe-ordl THEN
               FIND FIRST po-ord WHERE po-ord.company = oe-ordl.company
                            AND po-ord.po-no = int(oe-ordl.po-no-po) NO-LOCK NO-ERROR.
             ASSIGN
             v-vend = IF AVAILABLE oe-ordl THEN oe-ordl.vend-no ELSE ""
             v-board-po = IF AVAILABLE oe-ordl THEN oe-ordl.po-no-po ELSE 0.
             IF AVAILABLE po-ord THEN
                FIND FIRST po-ordl NO-LOCK WHERE
                     po-ordl.company EQ po-ord.company AND
                     po-ordl.po-no   EQ po-ord.po-no AND
                     po-ordl.i-no    EQ ef.board
                     NO-ERROR. 

             v-po-duedate = IF AVAILABLE po-ordl THEN po-ordl.due-date ELSE ?.

             PUT "<P10>" v-fill SKIP
                 "<B>BOARD CODE                   GRAIN     SHEETS       LF   SHEET SIZE    NET SHEET     DIE SIZE         DIE#              </B>" 
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

               DISPLAY ITEM.i-name FORMAT "x(28)" SPACE(2)
                    ef.xgrain FORMAT "x(2)"
                    wrk-sheet.gsh-qty 
                    v-lbs
                    STRING(wrk-sheet.sh-wid) + "x" + STRING(wrk-sheet.sh-len)
                    FORMAT "x(13)"
                    STRING(ef.nsh-wid) + "x" + STRING(ef.nsh-len) FORMAT "x(13)"
                    STRING(ef.trim-w) + "x" + STRING(ef.trim-l) FORMAT "x(17)"
                    eb.die-no 
                    /*v-case-size-ext[1] @ v-case-size FORM "x(15)"*/
                  /*  v-case-qty-ext[1]  @ v-case-qty  FORM ">>>>9"    /* #01240503*/
                    v-up-ext[1]        @ v-up*/
                    WITH STREAM-IO WIDTH 170 NO-LABELS NO-BOX FRAME sheet.
               x = 1.
             END. /* each wrk-sheet */
             DO ij = 2 TO 10:
                IF v-case-size-ext[ij] <> "" THEN DO:
                    DISPLAY /*v-case-size-ext[ij]  FORM "x(15)" AT 103*/
                         v-case-qty-ext[ij]   FORMAT ">>>>9" AT 118    /* #01240503*/
                         v-up-ext[ij]         FORMAT ">>9"
                    WITH STREAM-IO WIDTH 170 NO-LABELS NO-BOX FRAME sheet2.
                    DOWN WITH FRAME sheet2.
                END.                                                      
             END.
             IF x NE 2 THEN PUT v-fill AT 1 SKIP.

             /** Print Leaf/Film Ticket 15459  **/
  
             FOR EACH wrk-film NO-LOCK WHERE wrk-film.form-no EQ ef.form-no
                 /*break by wrk-sheet.form-no*/ BREAK BY wrk-film.leaf :
               FIND FIRST ITEM NO-LOCK WHERE item.company EQ cocode
                                         AND item.i-no    EQ wrk-film.leaf NO-ERROR.
               FIND FIRST job-mch NO-LOCK WHERE job-mch.company EQ cocode 
                                            AND job-mch.job     EQ job.job
                                            AND job-mch.job-no  EQ job.job-no 
                                            AND job-mch.job-no2 EQ job.job-no2 
                                            AND job-mch.m-code  EQ ef.m-code NO-ERROR .
                                         
               IF FIRST(wrk-film.leaf) THEN
                    PUT "<P10>" 
                 "<B>LEAF/FILM CODE  DESCRIPTION                 FORM  BLANK  APERTURE SIZE (W x L)   TOTAL LINEAL FEET   POUNDS     </B>" 
                 SKIP.
                DISPLAY 
                    wrk-film.leaf FORMAT "x(15)"
                    (IF AVAILABLE ITEM THEN ITEM.i-name ELSE "") FORMAT "x(28)" SPACE(2)
                    wrk-film.snum SPACE(4)
                    wrk-film.bnum SPACE(3)
                    STRING(wrk-film.leaf-l) + "x" + STRING(wrk-film.leaf-w)
                    FORMAT "x(23)" SPACE(10)
                   STRING( ( wrk-film.leaf-l + 1) * (IF AVAILABLE job-mch THEN job-mch.run-qty ELSE 0) / 12) SPACE(3)
                   STRING((IF AVAIL job-mch THEN job-mch.run-qty ELSE 0) * (wrk-film.leaf-l + 1) * ( wrk-film.leaf-w + 1) /
                    (IF AVAIL ITEM THEN ITEM.sqin-lb ELSE 0) )
                    WITH STREAM-IO width 170 NO-LABELS NO-BOX FRAME film.

                IF LAST(wrk-film.leaf) THEN
                     PUT "<P10>" v-fill SKIP .
             END.
              /** Print Leaf/Film ticket 15459 **/
                     
             /** PRINT INK **/
             PUT "<B>PASS  SIDE         LBS   INK NAME          ITEMS    PASS  SIDE         LBS   INK NAME            ITEMS    </B>"
                 SKIP.
             ASSIGN
                x = 2
                i = 1
                v-ink1 = ""
                v-ink2 = ""
                v-pass-count = 0.

             FOR EACH wrk-ink WHERE wrk-ink.form-no EQ eb.form-no
                 BREAK BY wrk-ink.i-pass:
                 IF FIRST-OF(wrk-ink.i-pass) THEN v-pass-count = v-pass-count + 1.
             END.
             FOR EACH wrk-ink WHERE wrk-ink.form-no EQ eb.form-no
                BREAK BY wrk-ink.i-pass
                      BY wrk-ink.i-code
                      BY wrk-ink.blank-no
                       :
                IF FIRST(wrk-ink.i-pass) THEN i = 1.
                IF FIRST-OF(wrk-ink.i-code) THEN ASSIGN v-item[i] = ""
                                                        v-i-qty[i] = 0.
                ASSIGN
                   v-item[i] = IF LOOKUP(STRING(wrk-ink.blank-no),v-item[i]) GT 0 THEN v-item[i] ELSE v-item[i] + STRING(wrk-ink.blank-no) + ","
                   v-i-qty[i] = v-i-qty[i] + wrk-ink.i-qty.

                IF LAST-OF(wrk-ink.i-code) THEN DO:
                    IF SUBSTRING(v-item[i],LENGTH(v-item[i]),1) EQ "," THEN v-item[i] = SUBSTRING(v-item[i],1,LENGTH(v-item[i]) - 1).                    
                    v-alloc = v-item[i].
                    IF NUM-ENTRIES(v-item[i]) GT 1 THEN DO:
                       v-alloc = "".
                       DO j = 1 TO NUM-ENTRIES(v-item[i]):
                          IF j EQ 1 THEN v-alloc = v-alloc + ENTRY(j,v-item[i]) + ",".
                          ELSE IF j EQ NUM-ENTRIES(v-item[i]) THEN
                          DO:
                            IF SUBSTRING(v-alloc,LENGTH(TRIM(v-alloc)),1) EQ "-" AND
                               INTEGER(ENTRY(j,v-item[i])) - INTEGER(ENTRY(j - 1,v-item[i])) GT 1 THEN
                               v-alloc = v-alloc + ENTRY(j - 1,v-item[i]) + ",".

                            v-alloc = v-alloc + ENTRY(j,v-item[i]) + ",".
                          END.
                          ELSE DO:
                             IF INTEGER(ENTRY(j,v-item[i])) - INTEGER(ENTRY(j - 1,v-item[i])) LE 1 THEN
                                SUBSTRING(v-alloc,LENGTH(TRIM(v-alloc)),1) = "-".
                             ELSE DO:
                                IF SUBSTRING(v-alloc,LENGTH(TRIM(v-alloc)),1) EQ "-" THEN
                                   v-alloc = v-alloc + ENTRY(j - 1,v-item[i]) + ",".

                                v-alloc = v-alloc + ENTRY(j,v-item[i]) + ",".
                             END.
                          END.
                       END.                    
                       IF v-alloc NE "" THEN SUBSTRING(v-alloc,LENGTH(TRIM(v-alloc)),1) = "".
                       
                    END.
                        
                    IF wrk-ink.i-side NE "" THEN
                    DO:
                       IF wrk-ink.i-pass = 1 THEN
                          ASSIGN v-ink1[i] = STRING(wrk-ink.i-pass) + wrk-ink.i-side +
                                             STRING(v-i-qty[i],"->>>,>>>,>>9.99") + "   " + 
                                             STRING(wrk-ink.i-dscr,"x(20)") + " " + TRIM(v-alloc)
                                 i = i + 1. 
                       ELSE IF wrk-ink.i-pass = 2 THEN
                          ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + wrk-ink.i-side + STRING(v-i-qty[i],"->>>,>>>,>>9.99") + "   " + 
                                             STRING(wrk-ink.i-dscr,"x(20)") + " " + TRIM(v-alloc)
                                 i = i + 1.
                       ELSE IF wrk-ink.i-pass = 3 THEN
                          ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + wrk-ink.i-side + STRING(v-i-qty[i],"->>>,>>>,>>9.99") + "   " + 
                                             STRING(wrk-ink.i-dscr,"x(20)") + " " + TRIM(v-alloc)
                                 i = i + 1.
                       ELSE IF wrk-ink.i-pass = 4 THEN
                          ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + wrk-ink.i-side + STRING(v-i-qty[i],"->>>,>>>,>>9.99") + "   " + 
                                             STRING(wrk-ink.i-dscr,"x(20)") + " " + TRIM(v-alloc)
                                 i = i + 1.
                       ELSE IF wrk-ink.i-pass = 5 THEN
                          ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + wrk-ink.i-side + STRING(v-i-qty[i],"->>>,>>>,>>9.99") + "   " + 
                                             STRING(wrk-ink.i-dscr,"x(20)") + " " + TRIM(v-alloc)
                                 i = i + 1.
                       ELSE IF wrk-ink.i-pass = 6 THEN
                          ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + wrk-ink.i-side + STRING(v-i-qty[i],"->>>,>>>,>>9.99") + "   " + 
                                             STRING(wrk-ink.i-dscr,"x(20)") + " " + TRIM(v-alloc)
                                 i = i + 1.
                       ELSE IF wrk-ink.i-pass = 7 THEN
                          ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + wrk-ink.i-side + STRING(v-i-qty[i],"->>>,>>>,>>9.99") + "   " + 
                                             STRING(wrk-ink.i-dscr,"x(20)") + " " + TRIM(v-alloc)
                                 i = i + 1.
                       ELSE IF wrk-ink.i-pass = 8 THEN
                          ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + wrk-ink.i-side + STRING(v-i-qty[i],"->>>,>>>,>>9.99") + "   " + 
                                             string(wrk-ink.i-dscr,"x(20)") + " " + trim(v-alloc)
                                 i = i + 1.
                       ELSE IF wrk-ink.i-pass = 9 THEN
                          ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + wrk-ink.i-side + STRING(v-i-qty[i],"->>>,>>>,>>9.99") + "   " + 
                                             STRING(wrk-ink.i-dscr,"x(20)") + " " + TRIM(v-alloc)
                                 i = i + 1.
                       ELSE IF wrk-ink.i-pass = 10 THEN
                          ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + wrk-ink.i-side + STRING(v-i-qty[i],"->>>,>>>,>>9.99") + "   " + 
                                             STRING(wrk-ink.i-dscr,"x(20)") + " " + TRIM(v-alloc)
                                 i = i + 1.
                       ELSE IF wrk-ink.i-pass = 11 THEN
                          ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + wrk-ink.i-side + STRING(v-i-qty[i],"->>>,>>>,>>9.99") + "   " + 
                                             STRING(wrk-ink.i-dscr,"x(20)") + " " + TRIM(v-alloc)
                                 i = i + 1.
                       ELSE IF wrk-ink.i-pass = 12 THEN
                          ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + wrk-ink.i-side + STRING(v-i-qty[i],"->>>,>>>,>>9.99") + "   " + 
                                             STRING(wrk-ink.i-dscr,"x(20)") + " " + TRIM(v-alloc)
                                 i = i + 1.
                    END.
                    ELSE
                    DO:
                       IF wrk-ink.i-pass = 1 THEN
                          ASSIGN v-ink1[i] = STRING(wrk-ink.i-pass) + (IF v-pass-count = 1 THEN "     F" ELSE "     B") +
                                              STRING(v-i-qty[i],"->>>,>>>,>>9.99") + "   " + 
                                             STRING(wrk-ink.i-dscr,"x(20)") + " " + TRIM(v-alloc)
                                 i = i + 1.
                       ELSE IF wrk-ink.i-pass = 2 THEN
                          ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + "     F" + STRING(v-i-qty[i],"->>>,>>>,>>9.99") + "   " + 
                                      STRING(wrk-ink.i-dscr,"x(20)") + " " + TRIM(v-alloc)
                                 i = i + 1.
                       ELSE IF wrk-ink.i-pass = 3 THEN
                          ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + "     F" + STRING(v-i-qty[i],"->>>,>>>,>>9.99") + "   " + 
                                      STRING(wrk-ink.i-dscr,"x(20)") + " " + TRIM(v-alloc)
                                 i = i + 1.
                       ELSE IF wrk-ink.i-pass = 4 THEN
                          ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + "     F" + STRING(v-i-qty[i],"->>>,>>>,>>9.99") + "   " + 
                                      STRING(wrk-ink.i-dscr,"x(20)") + " " + TRIM(v-alloc)
                                 i = i + 1.
                       ELSE IF wrk-ink.i-pass = 5 THEN
                          ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + "     F" + STRING(v-i-qty[i],"->>>,>>>,>>9.99") + "   " + 
                                      STRING(wrk-ink.i-dscr,"x(20)") + " " + TRIM(v-alloc)
                                 i = i + 1.
                        ELSE IF wrk-ink.i-pass = 6 THEN
                          ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + "     F" + STRING(v-i-qty[i],"->>>,>>>,>>9.99") + "   " + 
                                      STRING(wrk-ink.i-dscr,"x(20)") + " " + TRIM(v-alloc)
                                 i = i + 1.
                       ELSE IF wrk-ink.i-pass = 7 THEN
                          ASSIGN v-ink2[i] = string(wrk-ink.i-pass) + "     F" + STRING(v-i-qty[i],"->>>,>>>,>>9.99") + "   " + 
                                      string(wrk-ink.i-dscr,"x(20)") + " " + TRIM(v-alloc)
                                 i = i + 1.
                       ELSE IF wrk-ink.i-pass = 8 THEN
                          ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + "     F" + STRING(v-i-qty[i],"->>>,>>>,>>9.99") + "   " + 
                                      STRING(wrk-ink.i-dscr,"x(20)") + " " + TRIM(v-alloc)
                                 i = i + 1.
                       ELSE IF wrk-ink.i-pass = 9 THEN
                          ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + "     F" + STRING(v-i-qty[i],"->>>,>>>,>>9.99") + "   " + 
                                      STRING(wrk-ink.i-dscr,"x(20)") + " " + TRIM(v-alloc)
                                 i = i + 1.
                        ELSE IF wrk-ink.i-pass = 10 THEN
                          ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + "     F" + STRING(v-i-qty[i],"->>>,>>>,>>9.99") + "   " + 
                                      string(wrk-ink.i-dscr,"x(20)") + " " + TRIM(v-alloc)
                                 i = i + 1.
                       ELSE IF wrk-ink.i-pass = 11 THEN
                          ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + "     F" + STRING(v-i-qty[i],"->>>,>>>,>>9.99") + "   " + 
                                      STRING(wrk-ink.i-dscr,"x(20)") + " " + TRIM(v-alloc)
                                 i = i + 1.
                       ELSE IF wrk-ink.i-pass = 12 THEN
                          ASSIGN v-ink2[i] = STRING(wrk-ink.i-pass) + "     F" + STRING(v-i-qty[i],"->>>,>>>,>>9.99") + "   " + 
                                      STRING(wrk-ink.i-dscr,"x(20)") + " " + TRIM(v-alloc)
                                 i = i + 1.
                      
                    END.

                END.
               
                DELETE wrk-ink.
             END. /* each wrk-ink */
             ASSIGN
                v-skip = NO
                v-plate-printed = NO.
            
             DO j = 1 TO EXTENT(v-ink1) :
                IF TRIM(v-ink1[j]) EQ "-" THEN v-ink1[j] = "".               
                IF v-ink1[j] NE "" THEN DO:
                   IF v-skip THEN DO:
                       PUT  v-ink1[j] FORMAT "x(52)" .
                       IF j = 2 THEN DO:
                          /* PUT eb.style FORMAT "x(8)" AT 106 SPACE(1) v-size[1] FORM "x(30)" .*/
                           v-plate-printed = YES.
                       END.
                       PUT SKIP.
                   END.
                   ELSE PUT v-ink1[j] FORMAT "x(52)".                                                             
                   v-skip = NOT v-skip.             
                END.
             END.
             IF NOT v-plate-printed THEN PUT /*eb.style FORMAT "x(8)" AT 106 SPACE(1) v-size[1] FORM "x(30)"*/ SKIP.

             DO j = 1 TO EXTENT(v-ink2):
                IF TRIM(v-ink2[j]) EQ "-" THEN v-ink2[j] = "".                 
                IF v-ink2[j] NE "" THEN DO:
                   IF v-skip THEN PUT v-ink2[j] FORMAT "x(52)" SKIP.
                   ELSE PUT v-ink2[j] FORMAT "x(52)".
                   v-skip = NOT v-skip.
                END.                
             END.

             PUT v-fill AT 1 SKIP "<B><P12> F I N I S H I N G</B><P9>" SKIP.
              FIND item NO-LOCK WHERE
                item.company  EQ eb.company AND
                item.i-no     EQ eb.layer-pad  AND
                item.mat-type EQ "5" 
                NO-ERROR.

           ASSIGN 
              v-lp-dep = IF AVAILABLE item THEN ITEM.case-d ELSE 0 
              v-lp-qty = IF AVAILABLE item THEN ITEM.box-case ELSE 0.

          FOR EACH bff-eb NO-LOCK WHERE bff-eb.company EQ eb.company
                                    AND bff-eb.est-no  EQ eb.est-no
                                    AND bff-eb.form-no EQ eb.form-no BREAK BY bff-eb.form-no :

               FIND FIRST bf-job-mat NO-LOCK WHERE bf-job-mat.company EQ job.company 
                   AND bf-job-mat.job EQ job.job 
                   AND bf-job-mat.job-no EQ job.job-no 
                   AND bf-job-mat.job-no2 EQ job.job-no2
                   AND bf-job-mat.frm EQ bff-eb.form-no
                   AND bf-job-mat.blank-no EQ bff-eb.blank-no
                   AND bf-job-mat.rm-i-no EQ  bff-eb.cas-no  NO-ERROR .
   

          IF FIRST-OF(bff-eb.form-no) THEN
             PUT 
                "<P9><B> F/B             Tray #          Qty per tray        Case #                Qty   Qty per case        Pallet          Packing Specs </B>" SKIP.

          IF bff-eb.lp-up NE 0 THEN
          DO:
             v-unit-per-dec = bff-eb.cas-cnt / bff-eb.lp-up.
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

          IF bff-eb.cas-cnt NE 0 THEN
          DO:
             v-job-qty-boxes-code-dec = v-job-qty / eb.cas-cnt.
             {sys/inc/roundup.i v-job-qty-boxes-code-dec}
             v-job-qty-boxes-code-int = v-job-qty-boxes-code-dec.
          END.
          ELSE
             v-job-qty-boxes-code-int = 0.

          FIND FIRST itemfg WHERE
               itemfg.company = bff-eb.company AND
               itemfg.i-no = bff-eb.stock-no
               NO-LOCK NO-ERROR.

          IF AVAILABLE itemfg THEN
          DO:
             v-cas-wt = (itemfg.weight-100 / 100) * bff-eb.cas-cnt.
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
          
          ASSIGN
              v-shrink-wrap = CAN-FIND(FIRST est-op WHERE
                               est-op.company EQ job-hdr.company AND
                               est-op.est-no EQ est.est-no AND
                               est-op.dept = "SW").
          PUT v-job-no + "-" + trim(STRING(bff-eb.form-no,">>9")) + 
                    trim(STRING(bff-eb.blank-no,">>9")) FORM "x(11)"

              bff-eb.layer-pad  AT 18 /* tray */
              ( IF v-lp-qty GT 0 THEN eb.cas-cnt / v-lp-qty ELSE 0) FORMAT "->>>>>9.99"  AT 36
              bff-eb.cas-no FORMAT "X(15)" AT 54 /* cases# */
              (IF AVAILABLE bf-job-mat THEN STRING(bf-job-mat.qty,"->>>>>>9.9<") ELSE "") FORMAT "x(11)"
              bff-eb.cas-cnt FORMAT "->>>>>9" AT 87  /* qty per case */
              bff-eb.tr-no   AT 102 /* Pallet */ 
              (IF AVAILABLE itemfg THEN itemfg.prod-not ELSE "") FORMAT "X(20)" AT 118 /* packing spacs*/ SKIP .
               
              RELEASE itemfg.
         /* PUT " Flat" "Finished"  AT 22 "Tray#" AT 33 eb.layer-pad FORMAT "x(10)"
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
           
          END.  /* each bff-eb */

          PUT v-fill AT 1 SKIP.

         END. /* last-of(eb.form-no) */
          
        END. /* each eb */
      END. /* each ef */
     END. /* first job-no */

      IF LAST-OF(job-hdr.frm) THEN DO:
         IF s-run-speed THEN
            PUT "<B>MACHINE                 MR WASTE   MR HRS  RUN SPEED   SPOIL%    SHEETS PER MACHINE   </B>" /* SIZE  TOTAL REQUIRED  PALLET*/
                SKIP.
         ELSE
            PUT "<B>MACHINE                 MR WASTE   MR HRS   RUN HOUR    SPOIL%    SHEETS PER MACHINE   </B>"  /* SIZE  TOTAL REQUIRED  PALLET*/
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
                FOR EACH xjob-mat NO-LOCK WHERE xjob-mat.company EQ cocode
                                            AND xjob-mat.job     EQ job-hdr.job
                                            AND xjob-mat.job-no  EQ job-hdr.job-no
                                            AND xjob-mat.job-no2 EQ job-hdr.job-no2
                                            AND xjob-mat.frm = job-hdr.frm
                                            AND (xjob-mat.blank-no = job-hdr.blank-no
                                            OR xjob-mat.blank-no = 0),
                     FIRST ITEM NO-LOCK WHERE ITEM.company  EQ cocode AND
                                              ITEM.i-no     EQ xjob-mat.rm-i-no AND 
                                              ITEM.mat-type EQ "D" :
                     v-mat-for-mach = v-mat-for-mach +
                                      (IF LENGTH(v-mat-for-mach) <= 20 THEN FILL(" ", 20 - LENGTH(v-mat-for-mach))
                                      ELSE "  " ) +
                                      ITEM.i-name.
                 END.
             END.          
             IF s-prt-mstandard THEN DO:
                IF s-run-speed THEN
                   PUT wrk-op.m-dscr   SPACE(5)
                       wrk-op.mr-waste[job-hdr.frm]   SPACE(5)
                       wrk-op.mr[job-hdr.frm]         SPACE(5)
                       wrk-op.speed[job-hdr.frm]      SPACE(5)
                       wrk-op.spoil[job-hdr.frm]      SPACE(5)
                       wrk-op.num-sh[job-hdr.frm]     SPACE(11)  /*9+9*/
                      /* v-mat-for-mach FORM "x(40)" */   /*60*/
                       SKIP.
               ELSE
                   PUT wrk-op.m-dscr   SPACE(5)
                       wrk-op.mr-waste[job-hdr.frm]   SPACE(5)
                       wrk-op.mr[job-hdr.frm]         SPACE(5)
                       wrk-op.run-hr[job-hdr.frm]     SPACE(5)
                       wrk-op.spoil[job-hdr.frm]      SPACE(5)
                       wrk-op.num-sh[job-hdr.frm]     SPACE(11)  /* 9 + 9 */
                       /*v-mat-for-mach FORM "x(40)" */   /*60*/
                       SKIP.
             END.
             ELSE PUT wrk-op.m-dscr   SPACE(5)
                     /* wrk-op.mr-waste[job-hdr.frm]   */ SPACE(10)
                      /*wrk-op.mr[job-hdr.frm] >>9.99 */   SPACE(11)
                      /*wrk-op.speed[job-hdr.frm] >>>>9*/      SPACE(10)
                      /*wrk-op.spoil[job-hdr.frm]   >>9.99*/   SPACE(12)
                      wrk-op.num-sh[job-hdr.frm]     SPACE(11)  /* 9 + 9 */
                      /*v-mat-for-mach FORM "x(40)"*/ /*60*/
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

        /* gdm - 01060907 */
        FOR EACH notes NO-LOCK WHERE notes.rec_key EQ job.rec_key
                    AND (notes.note_form_no EQ job-hdr.frm OR notes.note_form_no = 0)
                    AND LOOKUP(notes.note_code,v-exc-depts) EQ 0 
                    AND (( notes.note_type EQ "O" AND notes.note_group EQ STRING(job.job) ) OR  notes.note_type NE "O" )
                    BREAK BY notes.note_code
                    BY notes.note_form_no:
          
            IF FIRST(notes.note_code) THEN 
              PUT "<B>DEPARTMENT                 INSTRUCTION NOTES</B>" 
                SKIP.

            IF FIRST-OF(notes.note_code) THEN DO:
               lv-text = "".
               FOR EACH tt-formtext:
                   DELETE tt-formtext.
               END.
            END. /* IF FIRST-OF(notes.note_code) */

            FIND FIRST dept NO-LOCK 
                 WHERE dept.CODE EQ notes.note_code NO-ERROR.

            lv-text = lv-text + " " + 
                      TRIM(notes.note_text) + CHR(10).

            IF LAST-OF(notes.note_code) THEN DO:           
                
               DO li = 1 TO 4:
                  CREATE tt-formtext.
                  ASSIGN tt-line-no = li
                         tt-length  = 90.
               END.

               RUN custom/formtext.p (lv-text).

               i = 0.

               v-dept-inst = "".

               FOR EACH tt-formtext:
                   i = i + 1.
                   IF  i <= 4 THEN v-dept-inst[i] = tt-formtext.tt-text.      
               END.

               IF PAGE-SIZE - LINE-COUNTER < 11 THEN PAGE.
               
               IF v-dept-inst[1] NE "" THEN 
                 PUT 
                   (IF AVAILABLE dept THEN (dept.CODE + " " + dept.dscr) 
                                  ELSE (note.note_code + " " + note_title)) 
                                  FORMAT "x(28)" AT 1
                   v-dept-inst[1] FORMAT "x(90)" AT 29 SKIP.

               IF v-dept-inst[2] NE "" THEN 
                 PUT 
                   (IF AVAILABLE dept THEN (dept.CODE + " " + dept.dscr) 
                                  ELSE (note.note_code + " " + note_title)) 
                                  FORMAT "x(28)" AT 1
                   v-dept-inst[2] FORMAT "x(90)" AT 29 SKIP.

               IF v-dept-inst[3] NE "" THEN 
                 PUT 
                   (IF AVAILABLE dept THEN (dept.CODE + " " + dept.dscr) 
                                  ELSE (note.note_code + " " + note_title))
                                  FORMAT "x(28)" AT 1
                   v-dept-inst[3] FORMAT "x(90)" AT 29 SKIP.

               IF v-dept-inst[4] NE "" THEN 
                 PUT 
                   (IF AVAILABLE dept THEN (dept.CODE + " " + dept.dscr) 
                                  ELSE (note.note_code + " " + note_title)) 
                                  FORMAT "x(28)" AT 1
                   v-dept-inst[4] FORMAT "x(90)" AT 29 SKIP.
               
            END.  /*IF LAST-OF(notes.note_code) */ 

            v-dept-note-printed = YES.

        END. /* for each notes */
    
       /*==== note ======*/


    /*  IF v-ship <> "" THEN v-dept-inst[6] = v-ship.  /* shipto notes */ */
        IF NOT v-dept-note-printed THEN DO:
           IF PAGE-SIZE - LINE-COUNTER < 11 THEN PAGE.
           PUT "<B>DEPARTMENT   INSTRUCTION NOTES</B>" SKIP
              v-dept-inst[1] FORMAT "x(128)" SKIP
              v-dept-inst[2] FORMAT "x(128)" SKIP
              v-dept-inst[3] FORMAT "x(128)" SKIP
              v-dept-inst[4] FORMAT "x(128)" SKIP.
        END.
        
        /* spec note */
        ASSIGN
           lv-line-chars = 95
           v-inst2 = "".

        FIND FIRST itemfg NO-LOCK WHERE itemfg.company EQ job-hdr.company
                                    AND itemfg.i-no EQ job-hdr.i-no NO-ERROR.

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
            IF v-spec-no[i] <> "" THEN PUT v-spec-no[i].
            IF v-misc[i] <> "" THEN PUT v-misc[i] AT 32 FORM "x(30)" .
            IF v-prep[i] <> "" THEN PUT v-prep[i] AT 90.
            IF v-spec-no[i] <> "" OR v-misc[i] <> "" OR v-prep[i] <> "" THEN PUT SKIP.
        END.
        DO i = 7 TO 8:
            IF v-spec-no[i] <> "" THEN PUT v-spec-no[i].
            IF v-prep[i] <> "" THEN PUT v-prep[i] AT 90.
            IF v-spec-no[i] <> "" OR v-prep[i] <> "" THEN PUT SKIP.
        END.
********************************************/        
        i = 1.
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
           
           FIND FIRST b-eb NO-LOCK WHERE b-eb.company  EQ xjob-hdr.company
                                     AND b-eb.est-no   EQ xjob-hdr.est-no
                                     AND b-eb.form-no  EQ xjob-hdr.frm
                                     AND b-eb.blank-no EQ xjob-hdr.blank-no
                                     AND b-eb.stock-no EQ xjob-hdr.i-no
                                     NO-ERROR.
           IF NOT AVAILABLE b-eb THEN 
               FIND FIRST b-eb NO-LOCK WHERE b-eb.company EQ xjob-hdr.company
                                         AND b-eb.est-no  EQ xjob-hdr.est-no
                                         AND b-eb.form-no EQ xjob-hdr.frm
                                         /*AND b-eb.blank-no = xjob-hdr.blank-no*/
                                         AND b-eb.stock-no EQ xjob-hdr.i-no
                                         NO-ERROR.
           CREATE tt-fgitm.
           ASSIGN tt-fgitm.i-no = xjob-hdr.i-no
                  tt-fgitm.qty =  IF AVAILABLE xoe-ordl AND xoe-ordl.cas-cnt NE 0 THEN xoe-ordl.cas-cnt 
                                  ELSE IF AVAILABLE b-eb THEN b-eb.cas-cnt ELSE xjob-hdr.qty
                  tt-fgitm.i-dscr = IF AVAILABLE b-eb THEN b-eb.part-dscr1 ELSE xjob-hdr.i-no
                  tt-fgitm.po-no = IF AVAILABLE xoe-ordl THEN xoe-ordl.po-no ELSE ""
                  tt-fgitm.seq = i
                  i = i + 1.

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

                  IF AVAILABLE b-oe-rel THEN DO:
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
        
        IF s-prt-shipto THEN DO i = 1 TO 4:
            ASSIGN v-shipto1[i] = v-shipto[i]
                   v-shipto2[i] = v-shipto[i].
        END.
        ASSIGN v-cust-name2 = v-cust-name 
               v-cust-name3 = v-cust-name.
        /* label prints per item */

IF NOT s-prt-label THEN PUT SKIP v-fill SKIP.
ELSE  DO:
        ASSIGN
           i = 0
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
               "<B><U>LABEL ITEM" + TRIM(STRING(j - 2)) + "</U>"  FORMAT "x(22)"
               "<U>LABEL ITEM" + TRIM(STRING(j - 1)) + "</U>" FORMAT "x(20)" WHEN v-fgitm[2] <> "" AT 55
               "<U>LABEL ITEM" + TRIM(STRING(j)) + "</U></B>" FORMAT "x(23)" WHEN v-fgitm[3] <> "" AT 107
               SKIP
               "Job#:" v-job-no + "-" + STRING(v-job-no2)
               "Job#:" WHEN v-fgitm[2] <> ""  AT 45
                v-job-no + "-" + STRING(v-job-no2)   WHEN v-fgitm[2] <> "" 
               "Job#:" WHEN v-fgitm[3] <> "" AT 90  
               v-job-no + "-" + STRING(v-job-no2) WHEN v-fgitm[3] <> "" 
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
        IF i GT 0 THEN DO:
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
               "<U>LABEL ITEM" + TRIM(STRING(v-last-j + 2)) + "</U>" FORMAT "x(20)" WHEN v-fgitm[2] <> "" AT 55
               "<U>LABEL ITEM" + TRIM(STRING(v-last-j + 3)) + "</U></B>" FORMAT "x(23)" WHEN v-fgitm[3] <> "" AT 107
               SKIP
               "Job#:" v-job-no + "-" + STRING(v-job-no2)
               "Job#:" WHEN v-fgitm[2] <> ""  AT 45
                v-job-no + "-" + STRING(v-job-no2) WHEN v-fgitm[2] <> "" 
               "Job#:" WHEN v-fgitm[3] <> "" AT 90  
               v-job-no + "-" + STRING(v-job-no2) WHEN v-fgitm[3] <> "" 
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
  

    END. /* each job-hdr */
    
    IF v-format EQ "Fibre" THEN PAGE.

/* end ---------------------------------- copr. 1994  advanced software, inc. */
