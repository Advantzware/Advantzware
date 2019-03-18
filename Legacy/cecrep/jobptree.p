/* ---------------------------------------------- */
/*  cecrep/jobptree.p  factory ticket  for PEACHTREE landscape */
/* -------------------------------------------------------------------------- */

&scoped-define PR-PORT FILE,TERMINAL,FAX_MODEM,VIPERJOBTICKET

DEFINE INPUT PARAMETER v-format AS CHARACTER.
DEFINE INPUT PARAMETER ip-loop-num AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ip-copies AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ip-unprinted-approved AS LOG NO-UNDO.
DEFINE INPUT PARAMETER ip-make-hold AS LOG NO-UNDO.

DEFINE SHARED VARIABLE s-prt-fgimage AS LOG NO-UNDO.
DEFINE VARIABLE v-start-compress AS cha NO-UNDO.
DEFINE VARIABLE v-end-compress AS cha NO-UNDO.
DEFINE VARIABLE k_frac AS DECIMAL INIT 6.25 NO-UNDO.
DEFINE VARIABLE v-ink-1 AS cha FORM "X(30)" NO-UNDO.
DEFINE VARIABLE v-ink-2 AS cha FORM "X(30)" NO-UNDO.
DEFINE VARIABLE v-ink-3 AS cha FORM "X(30)" NO-UNDO.
DEFINE VARIABLE v-ink-4 AS cha FORM "X(30)" NO-UNDO.
DEFINE VARIABLE v-ink-5 AS cha FORM "X(30)" NO-UNDO.
DEFINE VARIABLE v-ink-6 AS cha FORM "X(30)" NO-UNDO.
DEFINE VARIABLE v-ink-7 AS cha FORM "X(30)" NO-UNDO.
DEFINE VARIABLE v-ink-8 AS cha FORM "X(30)" NO-UNDO.
DEFINE VARIABLE v-dept-note AS cha FORM "x(48)" EXTENT 50 NO-UNDO.
DEFINE VARIABLE v-spec-note AS cha FORM "x(124)" EXTENT 10 NO-UNDO.
DEFINE VARIABLE v-deptnote AS cha NO-UNDO.
DEFINE VARIABLE v-dept-length AS DECIMAL NO-UNDO.
DEFINE VARIABLE lv-under-run AS cha NO-UNDO.
DEFINE VARIABLE lv-over-run AS cha NO-UNDO.
DEFINE VARIABLE lv-part-name AS cha FORM "x(30)" NO-UNDO.
DEFINE VARIABLE lv-fg-name AS cha NO-UNDO.
DEFINE VARIABLE lv-status AS cha FORM "x(20)" NO-UNDO.
DEFINE VARIABLE lv-sts-code AS cha INIT "O,R,C,T,N,X,Q" NO-UNDO.
DEFINE VARIABLE lv-sts-desc AS cha INIT "O-Original,R-Repeat,C-Change,T-Transfer,N-New Customers,X-Complete Re-run,Q-Quality/Re-work" NO-UNDO.

DEFINE VARIABLE v-sman AS cha FORM "x(25)" NO-UNDO.
DEFINE VARIABLE v-blk-per-frm AS cha FORM "x(15)" NO-UNDO.
DEFINE VARIABLE ls-fgitem-img AS cha FORM "x(50)" NO-UNDO.
DEFINE VARIABLE lv-ord-po LIKE oe-ord.po-no NO-UNDO.
DEFINE VARIABLE lv-part-no AS cha FORM "x(15)" NO-UNDO.
DEFINE VARIABLE lv-cust-set AS cha FORM "x(15)" NO-UNDO.
DEFINE VARIABLE lv-cas-cnt AS INTEGER FORM ">>>>>>" NO-UNDO.
DEFINE VARIABLE v-height AS CHARACTER FORMAT "X(14)" NO-UNDO.
DEFINE VARIABLE v-spec-list AS CHARACTER FORMAT "x(20)"INIT "QA" NO-UNDO.
DEFINE VARIABLE v-count AS INTEGER NO-UNDO.

DEFINE VARIABLE v-set-qty AS INTEGER NO-UNDO.
DEFINE VARIABLE v-ord-qty AS INTEGER NO-UNDO.
DEFINE VARIABLE v-over-run AS cha NO-UNDO.
DEFINE VARIABLE v-under-run AS cha NO-UNDO.
DEFINE VARIABLE v-fg-set AS cha FORM "x(15)" NO-UNDO.
DEFINE VARIABLE v-see-1st-blank AS LOG NO-UNDO.
DEFINE VARIABLE v-shipto AS cha NO-UNDO.
DEFINE VARIABLE v-make-hold AS CHARACTER NO-UNDO FORMAT "x(12)".
DEFINE VARIABLE vll-is-a-set AS LOGICAL FORMAT "TRUE/FALSE" INITIAL FALSE NO-UNDO.

DEFINE NEW SHARED VARIABLE v-adder-1 AS CHARACTER FORMAT "X(10)" NO-UNDO.
DEFINE NEW SHARED VARIABLE v-adder-2 AS CHARACTER FORMAT "X(10)" NO-UNDO.
DEFINE NEW SHARED VARIABLE v-adder-3 AS CHARACTER FORMAT "X(10)" NO-UNDO.
DEFINE NEW SHARED VARIABLE v-adder-4 AS CHARACTER FORMAT "X(10)" NO-UNDO.
DEFINE NEW SHARED VARIABLE v-adder-5 AS CHARACTER FORMAT "X(10)" NO-UNDO.
DEFINE NEW SHARED VARIABLE v-adder-6 AS CHARACTER FORMAT "X(10)" NO-UNDO.
DEFINE VARIABLE ls-image1 AS cha NO-UNDO.
DEFINE VARIABLE ls-full-img1 AS cha FORM "x(200)" NO-UNDO.

DEFINE VARIABLE cStdHours AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRunHours AS CHARACTER NO-UNDO.


ASSIGN ls-image1 = "images\Quality-Check-Initials-Ptree.png".

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".


DEFINE BUFFER b-itemfg FOR itemfg.
DEF BUFFER b-job-hdr FOR job-hdr.

RUN sys/ref/ordtypes.p (OUTPUT lv-sts-code, OUTPUT lv-sts-desc).

{jcrep/r-ticket.i "shared"}

{cecrep/jobptree.i "new shared"}
{cecrep/jc-fibre.i }

DEFINE WORK-TABLE w-i2 LIKE w-i
    FIELD i-ext AS INTEGER.
DEFINE BUFFER b-w-i2 FOR w-i2.
DEFINE VARIABLE ld-i-qty LIKE w-i2.i-qty NO-UNDO.

{sys/inc/VAR.i SHARED}
{cec/msfcalc.i}

DEFINE NEW SHARED VARIABLE v-out1-id       AS   RECID    NO-UNDO.  /* YSK 06/08/01  was~ local var */
DEFINE NEW SHARED VARIABLE v-out2-id       AS   RECID    NO-UNDO.  /* YSK 06/08/01  was~ local var */
 
DEFINE VARIABLE v-vend-no       LIKE oe-ordl.vend-no                            NO-UNDO.
DEFINE VARIABLE v-po-no         LIKE oe-ordl.po-no-po                           NO-UNDO.
DEFINE VARIABLE v-qty-or-sup    AS   CHARACTER               FORMAT "x(38)"          NO-UNDO.
DEFINE VARIABLE v-i-line        AS   CHARACTER   EXTENT 4    FORMAT "x(38)"          NO-UNDO.
DEFINE VARIABLE v-print-score   AS   LOG    INIT YES                            NO-UNDO.
DEFINE VARIABLE v-pqty          AS   DECIMAL                                        NO-UNDO.

DEFINE VARIABLE lv-rt-num AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-add-entry AS INTEGER NO-UNDO.
DEFINE VARIABLE v-loop-cnt AS INTEGER NO-UNDO.
DEFINE VARIABLE v-note-cnt AS INTEGER NO-UNDO.
DEFINE VARIABLE v-note-length AS INTEGER NO-UNDO.
DEFINE VARIABLE v-die-loc AS cha FORM "x(15)" NO-UNDO.

DEFINE VARIABLE v-prev-ext-gap AS INTEGER NO-UNDO.
DEFINE VARIABLE v-coldscr LIKE eb.i-coldscr NO-UNDO.

DEFINE VARIABLE v-oecount AS LOG NO-UNDO.
DEFINE VARIABLE v-cont-string AS cha NO-UNDO.  
DEFINE VARIABLE v-prev-k AS INTEGER NO-UNDO.
DEFINE VARIABLE v-tmp-note-length AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-text AS CHARACTER NO-UNDO.
DEFINE VARIABLE li AS INTEGER NO-UNDO.
DEFINE VARIABLE v-cas-desc AS cha NO-UNDO.
DEFINE VARIABLE v-n-out AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE s-prt-ship-split AS LOG NO-UNDO.
DEFINE BUFFER b-eb FOR eb.
DEFINE VARIABLE lv-spattern-img AS cha FORM "x(50)" NO-UNDO.
DEFINE VARIABLE lv-split AS cha FORM "x(60)" EXTENT 4 NO-UNDO.
DEFINE VARIABLE lv-au AS cha FORM "x(20)" NO-UNDO.
DEFINE VARIABLE lv-est-type AS cha FORM "x(35)" NO-UNDO.
DEFINE VARIABLE v-qa-text AS cha FORM "x(30)" INIT "6/05 Job Ticket QF-119 Rev.A" NO-UNDO.
DEFINE VARIABLE v-start-note AS INTEGER NO-UNDO.
DEFINE VARIABLE v-end-note AS INTEGER NO-UNDO.
DEFINE VARIABLE v-i AS INTEGER NO-UNDO.
DEFINE VARIABLE dSheetPerHours AS DECIMAL NO-UNDO .
DEFINE VARIABLE iRunHours AS INTEGER NO-UNDO .
DEFINE VARIABLE dMinute AS DECIMAL NO-UNDO .

{custom/formtext.i NEW}
{sys/inc/notes.i}
DO TRANSACTION:
   {sys/inc/tspostfg.i}
END.
{cecrep/jc-prem.i}
{custom/notesdef.i}
DEFINE VARIABLE v-dept-inst AS cha FORM "x(80)" EXTENT 15 NO-UNDO.
DEFINE VARIABLE v-inst2 AS cha EXTENT 6 NO-UNDO.    
DEFINE VARIABLE v-tmp-line AS INTEGER NO-UNDO.
DEFINE BUFFER bf-eb FOR eb.
DEFINE BUFFER b-ef FOR ef.
DEFINE WORKFILE tt-wm LIKE w-m.
DEFINE VARIABLE lv-spec-qty LIKE ef.spec-qty FORMAT ">>>,>>9.9<<<<" NO-UNDO.
DEFINE SHARED VARIABLE s-prt-set-header AS LOG NO-UNDO.
DEFINE VARIABLE v-managed-order AS cha FORM "x(30)" NO-UNDO.
DEFINE VARIABLE ll-tandem AS LOG NO-UNDO.
DEFINE VARIABLE ll-jqcust AS LOG NO-UNDO.
DEFINE VARIABLE cCustpo-name AS CHARACTER NO-UNDO .

FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name EQ "JOBQTYCUST"
    NO-ERROR.
IF AVAILABLE sys-ctrl THEN
   ll-jqcust = sys-ctrl.log-fld.

IF ip-make-hold THEN
    v-make-hold = "Make & Hold".
ELSE
    v-make-hold = "".
/* gdm - */

ASSIGN
 v-line[1] = CHR(95) + fill(CHR(95),40) + chr(95) + "  " +
             chr(95) + fill(CHR(95),40) + chr(95) + "  " +
             chr(95) + fill(CHR(95),40) + chr(95)  
 v-line[2] = v-line[1]
 v-line[3] = CHR(95) + fill(CHR(95),128) + chr(95)
 v-line[4] = v-line[3]
 v-line[5] = CHR(95) + fill(CHR(95),84) + chr(95) + "  " +
                chr(95) + fill(CHR(95),40) + chr(95)
 v-line[6] = v-line[5]
 v-line[7] = CHR(95) + fill(CHR(95),25) + chr(95) + "  " +
             chr(95) + fill(CHR(95),99) + chr(95)
 v-line[8] = v-line[7]
 v-qty-or-sup = IF LOOKUP(v-format,"TriState,RFC,Boxtech,Brick,Corrugat") GT 0
                THEN ("Supplier:"     + fill("_",28))
                ELSE ("Qty Received: " + fill("_",24))
 v-spec-list = spec-list.

/*do v-local-loop = 1 to 1:*/
   FOR EACH job-hdr NO-LOCK
        WHERE job-hdr.company               EQ cocode
          AND (production OR
               job-hdr.ftick-prnt           EQ reprint OR
               PROGRAM-NAME(2) MATCHES "*r-tickt2*")
          AND job-hdr.job-no                GE substr(fjob-no,1,6)
          AND job-hdr.job-no                LE substr(tjob-no,1,6)

          AND fill(" ",6 - length(TRIM(job-hdr.job-no))) +
              trim(job-hdr.job-no) +
              string(job-hdr.job-no2,"99")  GE fjob-no

          AND fill(" ",6 - length(TRIM(job-hdr.job-no))) +
              trim(job-hdr.job-no) +
              string(job-hdr.job-no2,"99")  LE tjob-no
        USE-INDEX job-no,

        FIRST job
        WHERE job.company                   EQ cocode
          AND job.job                       EQ job-hdr.job
          AND job.job-no                    EQ job-hdr.job-no
          AND job.job-no2                   EQ job-hdr.job-no2
          AND job.stat                      NE "H"
          AND (job.pr-printed EQ reprint OR NOT production)
          AND (ip-unprinted-approved EQ NO OR
               (ip-unprinted-approved AND job.pr-printed = NO AND
                job.opened = YES AND job.cs-to-pr = YES)) 
        USE-INDEX job NO-LOCK,
        
        FIRST est
        WHERE est.company = job.company
          AND est.est-no                    EQ job.est-no
          AND est.est-type                  GT 4
        USE-INDEX est-no2 NO-LOCK,

        FIRST cust
        WHERE cust.company                  EQ cocode
          AND cust.cust-no                  EQ job-hdr.cust-no
        NO-LOCK,

        FIRST itemfg
        WHERE itemfg.company                EQ cocode
          AND itemfg.i-no                   EQ job-hdr.i-no
        NO-LOCK

        BREAK BY job.job-no
              BY job.job-no2:
      
       IF s-prt-ship-split THEN
       DO:
          FIND FIRST b-job-hdr where ROWID(b-job-hdr) = ROWID(job-hdr) EXCLUSIVE-LOCK NO-ERROR.
             ASSIGN b-job-hdr.splitShip = YES.
          Release b-job-hdr. 
       END.

       /* get whether warehous item or not */
       FIND FIRST oe-ordl WHERE oe-ordl.company EQ job-hdr.company
           AND oe-ordl.ord-no  EQ job-hdr.ord-no
           AND oe-ordl.job-no  EQ job-hdr.job-no
           AND oe-ordl.job-no2 EQ job-hdr.job-no2
           AND oe-ordl.i-no    EQ job-hdr.i-no NO-LOCK NO-ERROR.
       IF job-hdr.ord-no NE 0 AND NOT AVAILABLE oe-ordl THEN
          FIND FIRST oe-ordl WHERE oe-ordl.company EQ job-hdr.company
               AND oe-ordl.ord-no  EQ job-hdr.ord-no
               AND oe-ordl.i-no    EQ job-hdr.i-no NO-LOCK NO-ERROR.
       IF AVAILABLE oe-ordl THEN         
           v-managed-order = IF oe-ordl.managed = true THEN "MANAGED   WAREHOUSE   ORDER"
                         ELSE "".
                         
       v-break = FIRST-OF(job.job-no2).

      RELEASE xest.
      RELEASE xef.
      RELEASE xeb.
      RELEASE xoe-ord.
      RELEASE xoe-ordl.
      RELEASE xoe-rel.

      RUN cecrep/jobtick1.p (RECID(job-hdr), v-format,
                              ip-loop-num, ip-copies).

      FOR EACH w-ef WHERE (w-ef.frm = job-hdr.frm OR est.est-type <> 8),
          EACH b-eb NO-LOCK WHERE b-eb.company = job-hdr.company
                              AND b-eb.est-no = job-hdr.est-no 
                              AND b-eb.form-no = w-ef.frm
                              AND (b-eb.blank-no = job-hdr.blank-no OR est.est-type NE 8)
          BREAK BY w-ef.frm BY b-eb.blank-no:
        RELEASE xef.
        RELEASE xeb.
        RELEASE xstyle.
        RELEASE xxprep.

        RUN cecrep/jobtick6.p (RECID(w-ef), RECID(job-hdr),RECID(b-eb)). /* jobtick2.p*/

        ASSIGN
           v-pqty = 1
           v-cp = ""
           v-sman = "".

        IF AVAILABLE xeb THEN DO:
          RUN est/ef-#out.p (ROWID(xef), OUTPUT v-n-out).

          IF xeb.stock-no NE "" THEN v-fg = xeb.stock-no.
          ASSIGN
          v-cp = xeb.part-no
          lv-fg-name = IF AVAILABLE xoe-ordl AND xoe-ordl.i-no EQ job-hdr.i-no THEN xoe-ordl.i-name ELSE itemfg.i-name.
          {cec/rollfac.i}
          v-pqty = IF v-rollfac OR xeb.est-type EQ 8 THEN 1 ELSE
                   IF xeb.quantityPerSet LT 0 THEN (-1 / xeb.quantityPerSet)
                                       ELSE xeb.quantityPerSet.
          FIND FIRST sman WHERE sman.company = xeb.company AND
                                sman.sman = xeb.sman NO-LOCK NO-ERROR.
          v-sman = IF AVAILABLE sman THEN sman.sname ELSE xeb.sman.
          
          /* Fibre logic to show each ink on estimate - start */
          FOR EACH w-i2:
            DELETE w-i2.
          END.

          DO i = 1 TO EXTENT(xeb.i-code):
            IF xeb.i-code[i] NE "" THEN DO:
              CREATE w-i2.
              ASSIGN
               w-i2.i-code = xeb.i-code[i]
               w-i2.i-dscr = xeb.i-dscr[i]
               w-i2.i-ext  = i.
            END.
          END.

          FOR EACH w-i2,
              FIRST item
              WHERE item.company EQ cocode
                AND item.i-no    EQ w-i2.i-code
              NO-LOCK:
          
            DO i = 1 TO EXTENT(xeb.i-code):
              j = 0.
              IF xeb.i-code[i] EQ w-i2.i-code AND i EQ w-i2.i-ext THEN
              FOR EACH job-mch
                  WHERE job-mch.company EQ job.company
                    AND job-mch.job     EQ job.job
                    AND job-mch.job-no  EQ job.job-no
                    AND job-mch.job-no2 EQ job.job-no2
                    AND job-mch.frm     EQ xef.form-no
                    AND (job-mch.dept   EQ "PR" OR
                         (job-mch.dept  EQ "CT" AND item.mat-type EQ "V"))
                  NO-LOCK,
                  FIRST mach
                  {sys/ref/machW.i}
                    AND mach.m-code EQ job-mch.m-code
                  NO-LOCK
                  BY job-mch.line:
                j = j + 1.
                IF xeb.i-ps[i] EQ j THEN
                  w-i2.i-qty = w-i2.i-qty + mach.col-wastelb +
                               (((xeb.t-sqin - xeb.t-win) *
                                 (job-mch.run-qty * xeb.num-up * v-n-out) *
                                 (xeb.i-%[i] / 100)) / item.yield).
              END.
            END.
          END.

          j = 0.
          wst-ink:
          FOR EACH job-mch
              WHERE job-mch.company EQ job.company
                AND job-mch.job     EQ job.job
                AND job-mch.job-no  EQ job.job-no
                AND job-mch.job-no2 EQ job.job-no2
                AND job-mch.frm     EQ xef.form-no
                AND job-mch.dept    EQ "PR"
              NO-LOCK BY job-mch.line.
            j = j + 1.
            DO i = 1 TO 10:
              IF xeb.i-ps[i] EQ j THEN DO:
                FIND FIRST w-i2 WHERE w-i2.i-code EQ xeb.i-code[i] NO-ERROR.
                IF AVAILABLE w-i2 THEN DO:
                  FIND FIRST mach
                      {sys/ref/machW.i}
                        AND mach.m-code EQ job-mch.m-code
                      NO-LOCK NO-ERROR.
                  IF AVAILABLE mach THEN w-i2.i-qty = w-i2.i-qty + mach.ink-waste.
                  NEXT wst-ink.
                END.
              END.
            END.
          END.

          FOR EACH w-i2 BREAK BY w-i2.i-code:
            IF FIRST-OF(w-i2.i-code) THEN ld-i-qty = 0.

            ld-i-qty = ld-i-qty + w-i2.i-qty.

            IF LAST-OF(w-i2.i-code) THEN DO:
              FIND FIRST w-i WHERE w-i.i-code EQ w-i2.i-code NO-ERROR.

              IF AVAILABLE w-i THEN
              FOR EACH b-w-i2 WHERE b-w-i2.i-code EQ w-i2.i-code:
                b-w-i2.i-qty = w-i.i-qty * (b-w-i2.i-qty / ld-i-qty).
              END.
            END.
          END.
        END.  /* avail xeb*/

        ASSIGN
           v-loc     = ""
           v-loc-bin = ""
           lv-over-run =  IF AVAILABLE xoe-ordl THEN TRIM(STRING(xoe-ordl.over-pct,">>9.99%"))
                          ELSE IF AVAILABLE xoe-ord THEN TRIM(STRING(xoe-ord.over-pct,">>9.99%")) ELSE TRIM(STRING(cust.over-pct,">>9.99%"))
           lv-under-run = IF AVAILABLE xoe-ordl THEN TRIM(STRING(xoe-ordl.under-pct,">>9.99%"))
                          ELSE IF AVAILABLE xoe-ord THEN TRIM(STRING(xoe-ord.under-pct,">>9.99%")) ELSE TRIM(STRING(cust.over-pct,">>9.99%"))
            
           v-blk-per-frm = "  Part " + string(xeb.form-no,"99") + " of " + string(xest.form-qty,"99")     
           lv-ord-po = IF AVAILABLE xoe-ord THEN xoe-ord.po-no ELSE ""
           lv-status = IF AVAILABLE xoe-ordl THEN ENTRY(LOOKUP(xoe-ordl.type-code,lv-sts-code),lv-sts-desc) ELSE ""
           lv-part-no = IF AVAILABLE xoe-ordl AND xoe-ordl.i-no EQ job-hdr.i-no THEN xoe-ordl.part-no 
                        ELSE itemfg.part-no
           lv-cust-set = lv-part-no
           lv-au = IF itemfg.alloc THEN "U" ELSE "A"
           vll-is-a-set = itemfg.isaset
           v-dept-note[31] = "CSR: " + IF AVAILABLE xoe-ord THEN xoe-ord.user-id ELSE job.user-id.

       IF est.est-type = 6 THEN
          ASSIGN
            lv-part-name = xeb.part-dscr2
            lv-au = lv-au + "    " + job-hdr.i-no.
       ELSE DO:
         ASSIGN
            lv-part-name = IF AVAILABLE xoe-ordl THEN xoe-ordl.part-dscr1 ELSE ""  
            lv-au = "".
         RUN ce/com/istandem.p (ROWID(est), OUTPUT ll-tandem).
       END.
       ASSIGN
       lv-est-type = IF est.est-type = 5 THEN "SINGLE"
                     ELSE IF est.est-type = 6 THEN "SET"
                     ELSE IF ll-tandem THEN "TANDEM"
                     ELSE "COMBO"
       lv-est-type = lv-est-type + "   FORM " + string(b-eb.form-no) + " OF " + string(xest.form-qty) 
                    + "  BLANK " + STRING(b-eb.blank-no) + " OF " + STRING(xef.blank-qty).
       
       cJobNumber = v-job-prt + "-" + string(b-eb.form-no,"99") + "-" + STRING(b-eb.blank-no,"99") .

       FIND FIRST stackPattern NO-LOCK WHERE stackPattern.stackCode EQ b-eb.stack-code NO-ERROR.
       IF AVAILABLE stackPattern AND SEARCH(stackPattern.stackImage) NE ? THEN lv-spattern-img =  stackPattern.stackImage.
       
       PUT "<P10></PROGRESS>" SKIP(0.5) "<FCourier New><C2><B>" lv-au "<C33>" lv-est-type "</B>".
       PUT "<P12><B><C95>JOB TICKET" SKIP. /*AT 140*/  /*caps(SUBSTRING(v-fg,1,1)) FORM "x" AT 40*/       
      
       PUT UNFORMATTED "<r-2.7><UNITS=INCHES><C68><FROM><c95.8><r+2.7><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE="
              cJobNumber ">" SKIP "<r-1>".
       PUT
       "<#1><C1><FROM><C106><R+45><RECT><||3><C80><P10></B>" v-qa-text "<B>"
       "<=1><R-2><C33><B><P12>" v-managed-order "</B>"
       "<=1><C32><FROM><R+45><C32><LINE><||3>"
       "<=1><C55><FROM><R+8><C55><LINE><||3>"
       "<=1><R+20><C66><FROM><R+25><C66><LINE><||3>"
       "<=1><C74><FROM><R+20><C74><LINE><||3>"       
       "<=1><R+5><C74><FROM><C106><LINE><||3>"     /*Order info*/
       "<=1><R+8><C1><FROM><C74><LINE><||3>"     /*sold to*/
       "<=1><R+13><C74><FROM><C106><LINE><||3>"  /* board*/
       "<=1><R+16><C1><FROM><C32><LINE><||3>"   /*Production id*/
       "<=1><R+21><C1><FROM><C32><LINE><||3>"    /*item desc*/
       "<=1><R+20><C66><FROM><C106><LINE><||3>"    /*item desc*/
       "<=1><R+8><C33><P8>                 <U>Dept Notes:</U> "
       "<=1><R+27><C1><FROM><C32><LINE><||3>"   /*packing*/
       {cecrep/p-wastebox.i} 
       "<=1><R+28><C66><FROM><C106><LINE><||3>"   /*Printint*/
       "<=1><R+28><C96><FROM><R+11><LINE><||3>"
       "<=1><R+35><C1><FROM><C32><LINE><||3>"   /*machine routing*/
      /* "<=1><R+35><C20.8><FROM><R+10><LINE><||3>" */
       "<=1><R+40><C66><FROM><C106><LINE><||3>"   /*sheets ordered*/
       "<=1><R+40><C71><P8><U>Sheets Ordered:</U>          <U>Due Date:</U>      <U>Supplier:</U>"
       "<=1><R+40><C86><FROM><R+5><LINE><||3>"   
       "<=1><R+40><C96><FROM><R+5><LINE><||3>"
       "<=1><R+0.2><C56><#22><R+7.5><C+17.6><IMAGE#22=" ls-full-img1 /*+ ">"*/ FORM "x(200)"
       "<=1><R+27.2><C23><#22><R+5><C+9.7><IMAGE#22=" lv-spattern-img + ">" FORM "x(70)".  /*Stack pattern image*/

       VIEW FRAME head.  /* factory header display  */  
       
       ASSIGN
           v-i-line[1] = "ITEM DESCRIPTION"
           v-i-line[2] = "Style: " + IF AVAILABLE xstyle THEN xstyle.dscr ELSE ""
           v-i-line[3] = "Size: "  + IF AVAILABLE xeb    THEN
                     TRIM(STRING({sys/inc/k16v.i xeb.len},">,>>9.99")) + " x " +
                     trim(STRING({sys/inc/k16v.i xeb.wid},">,>>9.99")) + " x " +
                     trim(STRING({sys/inc/k16v.i xeb.dep},">,>>9.99")) ELSE ""
           v-i-line[4] = "Joint: " + IF AVAILABLE xeb THEN v-joint-dscr ELSE "".
        
       /*===== for xprint */
       /* RUN sys/ref/getpo#.p (IF AVAILABLE xoe-ordl THEN ROWID(xoe-ordl) ELSE ROWID(job),
                              w-ef.frm, OUTPUT v-po-no).*/
        RUN pGetpoNum(IF AVAILABLE xoe-ordl THEN ROWID(xoe-ordl) ELSE ROWID(job),
                      w-ef.frm, OUTPUT v-po-no).
        RELEASE po-ord.
        IF v-po-no NE 0 THEN
        FIND FIRST po-ord
            WHERE po-ord.company EQ cocode
              AND po-ord.po-no   EQ v-po-no
            NO-LOCK NO-ERROR.
        ASSIGN
         v-vend-no    = IF AVAILABLE po-ord THEN po-ord.vend-no ELSE ""
         v-qty-or-sup = "Vend:".

        IF v-vend-no NE "" THEN DO:
           v-qty-or-sup = v-qty-or-sup + trim(v-vend-no).
           IF v-po-no NE 0 THEN v-qty-or-sup = v-qty-or-sup + " PO#:" +
                                               trim(STRING(v-po-no,">>>>>9")) + "-" + (IF AVAIL po-ord AND  po-ord.stat NE "" THEN po-ord.stat ELSE "") .
        END.
        
        i = 0.
        FOR EACH w-i:
          i = i + 1.
        END.
        IF i LE 8 THEN DO i = i + 1 TO 8:
          CREATE w-i.
        END.

        v-form-sqft = ROUND(IF v-corr THEN (v-form-len * v-form-wid * .007)
                                       ELSE (v-form-len * v-form-wid / 144),3).
        FIND FIRST xxprep WHERE xxprep.company EQ cocode
                            AND xxprep.code EQ xeb.die-no
                            NO-LOCK NO-ERROR.

        ASSIGN
        v-die-loc = ""
        v-coldscr = IF AVAILABLE xeb AND xeb.i-coldscr <> "" THEN xeb.i-coldscr ELSE "Plain"
        lv-ord-po = IF AVAILABLE xoe-ord THEN xoe-ord.po-no ELSE "".

        FIND FIRST b-itemfg WHERE
             b-itemfg.company = itemfg.company AND
             b-itemfg.i-no = xeb.stock-no
             NO-LOCK NO-ERROR.

        RELEASE notes.

        IF AVAILABLE b-itemfg THEN 
          FIND FIRST notes WHERE notes.rec_key EQ b-itemfg.rec_key 
                             AND notes.note_code = "LOC" NO-LOCK NO-ERROR.

        IF AVAILABLE notes THEN v-die-loc = REPLACE(SUBSTRING(notes.note_text,1,10),CHR(10),"").

        IF AVAILABLE b-itemfg THEN ASSIGN lv-fg-name = b-itemfg.i-name.

        v-ord-qty = (IF AVAILABLE xoe-ordl AND xoe-ordl.i-no EQ job-hdr.i-no THEN xoe-ordl.qty ELSE job-hdr.qty) *
                    (IF est.form-qty LE 1 THEN 1 ELSE v-pqty).
        IF ll-jqcust AND NOT AVAILABLE xoe-ordl AND AVAILABLE job-hdr THEN
           ASSIGN v-ord-qty = v-ord-qty * ( 1 - (cust.over-pct / 100)).

        v-see-1st-blank = IF NOT FIRST-OF(w-ef.frm) OR
                             CAN-FIND(FIRST bf-eb WHERE bf-eb.company = b-eb.company
                                                    AND bf-eb.est-no = b-eb.est-no
                                                    AND bf-eb.form-no = b-eb.form-no
                                                    AND bf-eb.blank-no < b-eb.blank-no)
                          THEN YES ELSE NO.
                                                
        /**** Start code from Hughes for printing ink - btr ****/
        i = 0.
        FOR EACH w-i:
          i = i + 1.
        END.
        
        IF i LE 4 THEN DO i = i + 1 TO 5:
           CREATE w-i.
        END.

        ASSIGN
           v-ink-1 = ""
           v-ink-2 = ""
           v-ink-3 = ""
           v-ink-4 = ""
           v-ink-5 = ""
           v-ink-6 = ""
           v-ink-7 = ""
           v-ink-8 = "".

        DO v-i = 1 TO 8:
           IF b-eb.i-code[v-i] <> "" THEN DO:
               FIND FIRST w-i WHERE w-i.i-code = b-eb.i-code[v-i] NO-ERROR.
               IF AVAILABLE w-i THEN DO:
                  IF v-ink-1 = "" THEN
                     ASSIGN v-ink-1 =  w-i.i-dscr +
                                          (IF w-i.i-qty <> 0 THEN STRING(w-i.i-qty,">>>,>>9") ELSE "" ) +
                                           IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
                  ELSE IF v-ink-2 = "" THEN
                     ASSIGN v-ink-2 =  w-i.i-dscr +
                                          (IF w-i.i-qty <> 0 THEN STRING(w-i.i-qty,">>>,>>9") ELSE "" ) +
                                           IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
                  ELSE IF v-ink-3 = "" THEN
                     ASSIGN v-ink-3 =  w-i.i-dscr +
                                          (IF w-i.i-qty <> 0 THEN STRING(w-i.i-qty,">>>,>>9") ELSE "" ) +
                                           IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
                  ELSE IF v-ink-4 = "" THEN
                     ASSIGN v-ink-4 =  w-i.i-dscr +
                                         (IF w-i.i-qty <> 0 THEN STRING(w-i.i-qty,">>>,>>9") ELSE "" ) +
                                          IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
                  ELSE IF v-ink-5 = "" THEN
                     ASSIGN v-ink-5 =  w-i.i-dscr +
                                         (IF w-i.i-qty <> 0 THEN STRING(w-i.i-qty,">>>,>>9") ELSE "" ) +
                                          IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
                  ELSE IF v-ink-6 = "" THEN
                     ASSIGN v-ink-6 =  w-i.i-dscr +
                                         (IF w-i.i-qty <> 0 THEN STRING(w-i.i-qty,">>>,>>9") ELSE "" ) +
                                          IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
                  ELSE IF v-ink-7 = "" THEN
                     ASSIGN v-ink-7 =  w-i.i-dscr +
                                         (IF w-i.i-qty <> 0 THEN STRING(w-i.i-qty,">>>,>>9") ELSE "" ) +
                                          IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
                  ELSE IF v-ink-8 = "" THEN
                     ASSIGN v-ink-8 =  w-i.i-dscr +
                                         (IF w-i.i-qty <> 0 THEN STRING(w-i.i-qty,">>>,>>9") ELSE "" ) +
                                          IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
               END. /* IF AVAIL w-i THEN DO: */
           END. /* IF b-eb.i-code[v-i] <> "" THEN DO: */
        END. /*  DO v-i = 1 TO 5: */
        /**** END code from Hughes for printing ink ****/
        ASSIGN cCustpo-name = "".
        /* cust po from rel */ 
        RELEASE reftable .
          IF est.est-type = 6 THEN 
          DO:  /*Added to test for Unassembled vs. Assembled Set - ticket 18161*/ 
              FIND FIRST fg-set NO-LOCK
                  WHERE fg-set.company EQ est.company
                  AND fg-set.part-no EQ v-fg
                  NO-ERROR.
              IF AVAIL fg-set 
                  AND CAN-FIND(FIRST itemfg 
                  WHERE itemfg.company EQ fg-set.company
                  AND itemfg.i-no EQ fg-set.set-no
                  AND itemfg.alloc)
                  THEN
                  FIND FIRST xoe-rel NO-LOCK
                      WHERE xoe-rel.company EQ xoe-ordl.company
                      AND xoe-rel.ord-no  EQ xoe-ordl.ord-no
                      AND xoe-rel.i-no    EQ v-fg
                      NO-ERROR.
          END.
           IF AVAIL xoe-rel THEN
           ASSIGN cCustpo-name = xoe-rel.lot-no.
        
         IF AVAILABLE xoe-rel AND cCustpo-name = "" THEN
             ASSIGN cCustpo-name = (IF xoe-rel.po-no NE "" AND AVAIL xoe-rel THEN xoe-rel.po-no ELSE IF AVAILABLE xoe-ordl THEN xoe-ordl.po-no ELSE "") .  .
        
        DISPLAY  
           "<B>" WHEN  NOT vll-is-a-set
           "<C75>Order Qty: " + trim(STRING(v-ord-qty,">,>>>,>>9")) FORM "x(30)" 
           "</B>" WHEN NOT vll-is-a-set
           SKIP
           "<P8><U>Shipping Info: <P10></U>" AT 12 
           "<B>" WHEN vll-is-a-set
           "<P12><C75>Job Qty:" TRIM(STRING(job-hdr.qty * v-pqty,">,>>>,>>9")) FORMAT "x(30)"
           "</B>" WHEN vll-is-a-set
           "<P10>"
           SKIP
           "Ship To #:" AT 2
           xoe-ord.sold-id WHEN AVAILABLE xoe-ord
           xeb.ship-id WHEN AVAILABLE xeb @ xoe-ord.sold-id
           xoe-rel.ship-id WHEN AVAILABLE xoe-rel @ xoe-ord.sold-id 
           "<C75>Set Qty:" 
            TRIM(STRING(IF AVAILABLE xoe-ordl AND xoe-ordl.i-no EQ job-hdr.i-no THEN xoe-ordl.qty
                                       ELSE job-hdr.qty,">,>>>,>>9"))
                     WHEN AVAILABLE xeb AND xeb.est-type EQ 6    FORMAT "x(9)" 
           SKIP
           v-shp[1] AT 2 
           "Item Name:" AT 90 lv-fg-name FORM "x(26)"
           SKIP
           v-shp[2] AT 2  "Description 1:" AT 90 SUBSTRING(lv-part-name,1,22) FORM "x(25)" SKIP
           v-shp[3] AT 2  SKIP
           v-shp[4] AT 2  "<U>Board:</U>" AT 106 "<U>Adders:</U>" AT 125  WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank
           SKIP
           "Cust. PO #:" AT 2 cCustpo-name FORMAT "x(15)" /*xoe-ordl.po-no when avail xoe-ordl*/
           "Shts Req'd:" AT 90 WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank
           TRIM(STRING(v-sht-qty)) FORMAT "x(9)" WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank
           "<B><C97>" v-adder-1 "</B>" WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank
           SKIP
           "W:" + trim(STRING({sys/inc/k16v.i v-form-wid},">,>>9.99")) +
           " " +
           "L:" + trim(STRING({sys/inc/k16v.i v-form-len},">,>>9.99"))  FORMAT "x(22)" AT 90 WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank
           "<B><C97>" v-adder-2 "</B>" WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank
           SKIP
           "<P8><U>Production ID:</U><B><P10>" AT 10
           "<C75>Board:" WHEN FIRST-OF(w-ef.frm)
           "<C80>" v-form-code FORM "x(15)" WHEN FIRST-OF(w-ef.frm)
           "SEE 1st BLANK" WHEN v-see-1st-blank @ v-form-code
           "<B><C97>" v-adder-3 "</B>" WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank
           SKIP
           "<P10>Our Order#:"  AT 2 v-ord-no   "CAD#:" xeb.cad-no WHEN AVAILABLE xeb
           "<C74><B>" v-form-dscr WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank FORM "x(30)" 
           "<C97><P10>" v-adder-4 "</B>" WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank
           SKIP
           "<P10>FG#:" AT 2 v-fg FORM "x(15)"  v-blk-per-frm
           "<P10></B>"
           " Sq Ft:" WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank AT 103
           TRIM(STRING(v-form-sqft)) FORMAT "x(7)" WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank
           "MSF:"  + trim(STRING(v-sht-qty * v-form-sqft / 1000,">>>9.9<")) FORMAT "x(11)" WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank
           "<B><C97>" v-adder-5 "</B>" WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank
           SKIP
           "<P12><B>CustPart#:" AT 2 v-cp FORM "x(15)"  "</B>"
           "<P10>" v-qty-or-sup AT 101 FORM "x(28)" WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank            
           "<B><C97>" v-adder-6 "</B>" WHEN FIRST-OF(w-ef.frm) AND NOT v-see-1st-blank
           SKIP  
           "<P10>Item Name: " + lv-fg-name  AT 2  FORM "x(41)" 
           "<P8>" "<U>Die Cutting, Slit, & Saw</U><P10>" AT 125
           SKIP
           "<P8><U>Item Description:</U><P10>" AT 10               
           "Die #" AT 100 xeb.die-no WHEN AVAILABLE xeb " Loc:" v-die-loc 
           SKIP
           "Style:" AT 2 xstyle.dscr WHEN AVAILABLE xstyle              
           "Gross Size:" AT 80  
           "W:" + trim(STRING({sys/inc/k16v.i xef.gsh-wid},">>>9.99")) +
           " " +
           "L:" + trim(STRING({sys/inc/k16v.i xef.gsh-len},">>>9.99")) FORMAT "x(19)"
           "Slit: W:" + string(v-outw) + " L:" + string(v-outl) FORM "x(15)"   
           SKIP
           "Size:" AT 2 
           (trim(string({sys/inc/k16v.i xeb.len},">,>>9.99")) + " x " +
           trim(string({sys/inc/k16v.i xeb.wid},">,>>9.99")) + " x " +
           trim(string({sys/inc/k16v.i xeb.dep},">,>>9.99"))) FORM "x(30)" WHEN AVAIL xeb   
           "Net   Size:"  AT 80
           "W:" + trim(string({sys/inc/k16v.i xef.nsh-wid},">>>9.99")) +
           " " +
           "L:" + trim(string({sys/inc/k16v.i xef.nsh-len},">>>9.99")) format "x(22)"
           SKIP
           "Joint:" AT 2 v-joint-dscr             
                         "Die   Size:" AT 80
           "W:" + trim(string({sys/inc/k16v.i xef.trim-w},">>>9.99")) +
           " " +
           "L:" + trim(string({sys/inc/k16v.i xef.trim-l},">>>9.99")) format "x(21)"
           "Up:" "W:" + string(v-upl) + " L:" + string(v-upw) FORM "x(9)"
           SKIP            
           "Blank Size:" AT 80 
           "W:" + trim(string({sys/inc/k16v.i xeb.t-wid},">>>9.99")) +
           " " +
           "L:" + trim(string({sys/inc/k16v.i xeb.t-len},">>>9.99")) format "x(22)"
           SKIP
             "Impressions:" AT 80 trim(string(v-dc-qty))    format "x(7)"
           SKIP
           "D/C Style:" AT 80
           SKIP              
      WITH WIDTH 200 NO-BOX NO-LABEL FRAME shp-info STREAM-IO. 

      FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                          AND sys-ctrl.name    EQ "OECOUNT" NO-LOCK NO-ERROR.
      v-oecount = AVAILABLE sys-ctrl AND sys-ctrl.log-fld.

      IF AVAILABLE xeb THEN
         FIND FIRST item
              WHERE item.company EQ cocode
                AND item.i-no    EQ xeb.cas-no NO-LOCK NO-ERROR.

      ASSIGN
      v-cas-desc = IF AVAILABLE ITEM THEN ITEM.i-name 
                   ELSE IF AVAILABLE xeb THEN xeb.cas-no
                   ELSE ""
      lv-cas-cnt = 0.

      IF AVAILABLE xeb THEN FIND FIRST item
              WHERE item.company EQ cocode
                AND item.i-no    EQ xeb.tr-no NO-LOCK NO-ERROR.
          
      v-height = "HEIGHT: " + TRIM(STRING(b-eb.tr-dep,"ZZ9.99")).


      DISPLAY "<P8><U>Packing:</U>" AT 13
              "<U>Printing:</U>                   <U>Sheets:</U>" AT 125
              "<P10>"SKIP
              v-cas-desc AT 3 FORM "x(25)"
              "<B>PRINTING PLATE #:" AT 80 xeb.plate-no FORMAT "x(15)" WHEN AVAILABLE xeb "</B>"
              "<P8><U>Received:</U></P8>" AT 123
              SKIP
              "# Per Bndl:"        AT 3
              xeb.cas-cnt WHEN AVAILABLE xeb
              "<P10># Per Unit:" AT 3 xeb.tr-cnt WHEN AVAILABLE xeb
              "<P10>" v-stackcode AT 3   FORMAT "x(28)"
              SKIP
              "Pallet:" AT 3
              TRIM(STRING({sys/inc/k16v.i xeb.tr-len},">,>>9.99")) + " x " +
              trim(STRING({sys/inc/k16v.i xeb.tr-wid},">,>>9.99")) WHEN AVAILABLE xeb FORMAT "x(15)"
              SKIP
              item.i-name AT 3 FORMAT "X(20)" WHEN AVAILABLE ITEM
              v-height AT 24
/*               "" AT 80 v-ink-5 SKIP  */
/*               "" AT 80 v-ink-6 SKIP  */
/*               "" AT 80 v-ink-7 SKIP  */
/*               "" AT 80 v-ink-8 SKIP  */
/*               "" AT 80               */
              WITH NO-BOX NO-LABELS FRAME m6 WIDTH 200 NO-ATTR-SPACE STREAM-IO.

      RUN cecrep/jobfibr2.p (RECID(job-hdr),v-format,cust.terms).
      
      i = 0.
      FOR EACH w-m:
        i = i + 1.
      END.
      IF i LT 3 THEN DO i = i + 1 TO 3:
        CREATE w-m.
        w-m.dseq = 999999999.
      END.

      /* box for route */
      ASSIGN
         lv-rt-num = i + 3
         i = 0.

      PUT SKIP(3)
          "<R-3><P8>  <U>Machine Routing:</U>    <U>MR STD</U>  <U>RUN STD</U>  <U> #/Hr</U>"
          "<P8>"  SKIP.
      
      FOR EACH w-m BY w-m.dseq:
       RUN pConvertToHours(INPUT string(w-m.s-hr,"999.99"), OUTPUT cStdHours).
       RUN pConvertToHours(INPUT string(w-m.r-hr,"999.99"), OUTPUT cRunHours).
       RUN pConvertToHMin(INPUT string(w-m.r-hr,"999.99"), OUTPUT iRunHours, OUTPUT dMinute ).
        ASSIGN
           i = i + 1
           v-letter = substr("UTE",i,1)
           v-lines = v-lines + 1.
           dSheetPerHours = v-sht-qty / ( iRunHours + (dMinute / 60)) .
        
        IF w-m.dscr <> "" THEN
        DISPLAY "<C2><P8>" w-m.dscr FORMAT "x(20)"  cStdHours FORMAT "x(6)"  cRunHours FORMAT "x(6)"  dSheetPerHours FORMAT ">>>99.9" "<P10>"  /*"<P8><U>Received:</U><P10>" WHEN i = 1 AT 29*/
            WITH NO-BOX NO-LABELS FRAME oo1 WIDTH 150 NO-ATTR-SPACE DOWN STREAM-IO.
        
      END.
      FIND LAST b-ef USE-INDEX est-qty WHERE b-ef.company = est.company
                                         AND b-ef.est-no = est.est-no NO-LOCK NO-ERROR.
      
      IF AVAILABLE b-ef AND b-ef.form-no = w-ef.frm THEN 
         FOR EACH w-m:
             CREATE tt-wm.
             BUFFER-COPY w-m TO tt-wm.
      END.

    EMPTY TEMP-TABLE tt-formtext.

    lv-text = "".
    FOR EACH notes WHERE
        notes.rec_key = job.rec_key AND
        (notes.note_form_no = w-ef.frm OR notes.note_form_no = 0) AND
        LOOKUP(notes.note_code,v-exc-depts) EQ 0
        AND notes.note_type NE 'O'
        NO-LOCK,
        FIRST dept NO-LOCK WHERE dept.code = notes.note_code
           BY notes.note_form_no BY dept.fc:

       lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
    END.    

    DO li = 1 TO 11:
      CREATE tt-formtext.
      ASSIGN
       tt-line-no = li
       tt-length  = 48.
    END.

    DO li = 12 TO 30:
      CREATE tt-formtext.
      ASSIGN
       tt-line-no = li
       tt-length  = 39.
    END.

    RUN custom/formtext.p (lv-text).
    ASSIGN
       i = 0
       v-dept-note = "".

    FOR EACH tt-formtext:
        i = i + 1.
        IF i <= 30 THEN v-dept-note[i] = tt-formtext.tt-text.      
    END.

    /*** this is for the spec notes specific to itemfg replacing dept-notes extent 21-25 
         to insert these SPEC NOTES (AH) 05-20-10 */
    lv-text = "".
    EMPTY TEMP-TABLE tt-formtext.
    IF AVAILABLE itemfg THEN DO:
       FOR EACH notes NO-LOCK WHERE notes.rec_key   EQ itemfg.rec_key
                                AND notes.note_type EQ "S"
                                AND CAN-DO(v-spec-list,notes.note_code),
           FIRST ITEM-spec NO-LOCK WHERE item-spec.company = itemfg.company
                                     AND item-spec.i-no = ""
                                     AND item-spec.code = notes.note_code
           BY item-spec.code BY notes.note_date BY notes.note_time:
           
           lv-text = lv-text + 
                     STRING(job-hdr.frm,">9") + "     " +
                     "<B>" + CAPS(notes.note_code) + ":</B>" + "   " + 
                     notes.note_text + CHR(10) .
       END.
    END.

    ASSIGN
       v-start-note = 30
       v-end-note = 0.

    DO li = 30 TO 1 BY -1:
       IF v-dept-note[li] NE "" THEN
       DO:
          v-start-note = li.
          LEAVE.
       END.
    END.

    ASSIGN
       v-start-note = v-start-note + 1
       v-end-note = v-start-note + 4.

    IF v-start-note LT 31 THEN
    DO:
       DO li = v-start-note TO v-end-note:
          CREATE tt-formtext.
          ASSIGN
             tt-line-no = li
             tt-length  = 39.
       END.
       
       RUN custom/formtext.p (lv-text).
      
       DO li = v-start-note TO v-end-note:
          IF li LE 30 THEN
             v-dept-note[li] = "".
       END.
    END.

    ASSIGN
       i = 0
       v-count = v-start-note.

    FOR EACH tt-formtext:
        IF v-count GT 30 THEN
           LEAVE.
        i = i + 1.
        IF i <= 5 THEN
           v-dept-note[v-count] = tt-formtext.tt-text.
        v-count = v-count + 1.
    END.                       
    /***************************/
        ASSIGN
        v-inst = ""
        /*v-dept-note[31] = "CSR: " + IF AVAILABLE xoe-ord THEN xoe-ord.user-id ELSE job.user-id */ .
        IF s-prt-ship-split THEN
           FIND FIRST tt-fibre WHERE tt-fibre.tt-job-no = job-hdr.job-no
                         AND tt-fibre.tt-job-no2 = job-hdr.job-no2
                         AND tt-fibre.tt-frm = w-ef.frm
                         AND tt-fibre.tt-blank = b-eb.blank-no NO-LOCK NO-ERROR.
        IF AVAILABLE tt-fibre THEN
           ASSIGN lv-split[1] = "LOC1 <U>" + string(tt-fibre.tt-sqty1,">>>,>>9") + "</U>     |" + "   ORDER <U>" + string(tt-fibre.tt-order1,">>>>>9") + "</U>   QTY <U>" + string(tt-fibre.tt-oqty1,">>>,>>9") + "</U>"
                  lv-split[2] = "LOC2 <U>" + string(tt-fibre.tt-sqty2,">>>,>>9") + "</U>     |" + "   ORDER <U>" + string(tt-fibre.tt-order2,">>>>>9") + "</U>   QTY <U>" + string(tt-fibre.tt-oqty2,">>>,>>9") + "</U>"
                  lv-split[3] = "LOC3 <U>" + string(tt-fibre.tt-sqty3,">>>,>>9") + "</U>     |" + "   ORDER <U>" + string(tt-fibre.tt-order3,">>>>>9") + "</U>   QTY <U>" + string(tt-fibre.tt-oqty3,">>>,>>9") + "</U>"
                  lv-split[4] = "LOC4 <U>" + string(tt-fibre.tt-sqty4,">>>,>>9") + "</U>     |" + "   ORDER <U>" + string(tt-fibre.tt-order4,">>>>>9") + "</U>   QTY <U>" + string(tt-fibre.tt-oqty4,">>>,>>9") + "</U>". .

        
        PUT "<=1><R+9><C33>" v-dept-note[1] /*"  " v-dept-note[26]*/
            "<=1><R+10><C33>" v-dept-note[2] /* "  " v-dept-note[27] */
            "<=1><R+11><C33>" v-dept-note[3] /*"  " v-dept-note[28]    */
            "<=1><R+12><C33>" v-dept-note[4] /*"  " v-dept-note[29]    */
            "<=1><R+13><C33>" v-dept-note[5] /*"  " v-dept-note[30]    */
            "<=1><R+14><C33>" v-dept-note[6] /*"  " v-dept-note[31]    */
            "<=1><R+15><C33>" v-dept-note[7] /*"  " v-dept-note[32]    */
            "<=1><R+16><C33>" v-dept-note[8] /*"  " v-dept-note[33]    */
            "<=1><R+17><C33>" v-dept-note[9] /*"  " v-dept-note[34]    */
            "<=1><R+18><C33>" v-dept-note[10] /*"  " v-dept-note[35]   */
            "<=1><R+19><C33>" v-dept-note[11] /*"  " v-dept-note[36]   */
            "<=1><R+20><C33>" v-dept-note[12] FORM "x(39)" /*"  " v-dept-note[37]   */
            "<=1><R+21><C33>" v-dept-note[13] FORM "x(39)" /*"  " v-dept-note[38]   */
            "<=1><R+22><C33>" v-dept-note[14] FORM "x(39)" /*"  " v-dept-note[39]   */
            "<=1><R+23><C33>" v-dept-note[15] FORM "x(39)" /*"  " v-dept-note[40]   */
            "<=1><R+24><C33>" v-dept-note[16] FORM "x(39)" /*"  " v-dept-note[41]   */
            "<=1><R+25><C33>" v-dept-note[17] FORM "x(39)" /*"  " v-dept-note[42]   */
            "<=1><R+26><C33>" v-dept-note[18] FORM "x(39)" /*"  " /* v-dept-note[43]*/ */
            "<=1><R+27><C33>" v-dept-note[19] FORM "x(39)" /*"  " /*v-dept-note[44]*/   */
            "<=1><R+28><C33>" v-dept-note[20] FORM "x(39)" /*"  " /* v-dept-note[45] */  */
            /*"<=1><R+29><C33>" v-dept-note[21] FORM "x(39)" 
            "<=1><R+30><C33>" v-dept-note[22] FORM "x(39)" 
            "<=1><R+31><C33>" v-dept-note[23] FORM "x(39)" 
            "<=1><R+32><C33>" v-dept-note[24] FORM "x(39)" 
            "<=1><R+33><C33>" v-dept-note[25] FORM "x(39)"
            "<=1><R+34><C33>" v-dept-note[26] FORM "x(39)"
            "<=1><R+35><C33>" v-dept-note[27] FORM "x(39)"
            "<=1><R+36><C33>" v-dept-note[28] FORM "x(39)"
            "<=1><R+37><C33>" v-dept-note[29] FORM "x(39)"
            "<=1><R+38><C33>" v-dept-note[30] FORM "x(39)"
            "<=1><R+4><C75>" v-dept-note[31] FORM "x(39)" */
            "<=1><R+40.8><C33><FROM><R+4.2><C65><RECT><||3>"
            "<=1><R+41><C33><P8>  Bundles: ________ @ _______          TOTAL"
            "<=1><R+42><C40> ________ @ _______"
            "<=1><R+43><C33>  Pallets: ________ @ _______"
            "<=1><R+44><C40> ________ @ _______       __________"
            "<=1><R+42><C57><FROM><C64><LINE><||3>"
            "<=1><R+40.8><C55><FROM><R+4.2><C55><LINE><||3>"
            .
        IF s-prt-ship-split THEN
            PUT 
            "<=1><R+34><C32><FROM><C65><LINE><||3>"
            "<=1><R+34><C33>" "SPLIT SHIP/QTY   |         SPLIT ORDER" FORM "x(39)" 
            "<=1><R+35><C33>" lv-split[1] FORM "x(70)"
            "<=1><R+36><C33>" lv-split[2] FORM "x(70)"
            "<=1><R+37><C33>" lv-split[3] FORM "x(70)"
            "<=1><R+38><C33>" lv-split[4] FORM "x(70)"
            .

        PUT 
            /* btr */
            "<=1><R+30><C66.8><P10>Ink 1: " v-ink-1
            "<=1><R+31><C66.8><P10>Ink 2: " v-ink-2
            "<=1><R+32><C66.8><P10>Ink 3: " v-ink-3
            "<=1><R+33><C66.8><P10>Ink 4: " v-ink-4
            "<=1><R+34><C66.8><P10>Ink 5: " v-ink-5
            "<=1><R+35><C66.8><P10>Ink 6: " v-ink-6
            "<=1><R+36><C66.8><P10>Ink 7: " v-ink-7
            "<=1><R+37><C66.8><P10>Ink 8: " v-ink-8
            /* was +30 */
            "<=1><R+38><C66.8><P10>Color: " v-coldscr FORMAT "x(35)" "<FCourier New>"
            "<=1><C66><R+39><FROM><C106><LINE><||3>".

        /*PUT "<=1><R+30><C67><P14>" lv-checkbox-2
            "<=1><R+31.5><C67>" lv-checkbox-3 "<P10><FCourier New>".*/

        PAGE.
        PUT "<#11><C1><FROM><C106><R+47><RECT><||3><C80><P10>" v-qa-text  
            "<=11><C30><FROM><R+3><C30><LINE><||3>"
            "<=11><C60><FROM><R+3><C60><LINE><||3>"
            "<=11><R+3><C1><FROM><C106><LINE><||3>"
            "<=11>Job # <C30> Estimate # <C60> Cust Part #"  SKIP
            "<P12><C12>" cJobNumber FORMAT "x(16)"
            "<C40>" v-est-no
            "<C70>" lv-part-no SKIP /*(2)*/.


        IF print-box AND AVAILABLE xest THEN DO:
            /*PAGE. */
            IF xest.metric THEN do:
             v-out1-id = RECID(xeb).
               run cec/desprnptree.p (RECID(xef),
                                INPUT-OUTPUT v-lines,
                                RECID(xest)).         
            PAGE.
            END.
            ELSE DO:
                v-out1-id = RECID(xeb).
             RUN cec/desprnL2.p (RECID(xef),
                                INPUT-OUTPUT v-lines,
                                RECID(xest)).            
             PAGE.

            END.
        END.
        ELSE PAGE.
        /* print fgitem's image */

        IF s-prt-fgimage THEN
        DO:
           IF xest.est-type EQ 6 THEN
           DO:
              IF AVAILABLE b-itemfg AND b-itemfg.box-image NE "" THEN DO:        
                
                 ls-fgitem-img = b-itemfg.box-image .
              
                 PUT UNFORMATTED "<#12><C1><FROM><C106><R+47><RECT><||3><C80>" v-qa-text SKIP
                     "<=12><R+1><C5>FG Item: " b-itemfg.i-no " " b-itemfg.i-name
                     "<=12><R+3><C1><FROM><C106><LINE><||3>"
                     "<=12><R+5><C5><#21><R+45><C+90><IMAGE#21=" ls-fgitem-img ">" SKIP. 
                 PAGE.
              END. 
             
              IF LAST(w-ef.frm) AND itemfg.box-image NE "" THEN DO:        
                
                 ls-fgitem-img = itemfg.box-image .
              
                 PUT UNFORMATTED "<#12><C1><FROM><C106><R+47><RECT><||3><C80>" v-qa-text SKIP
                     "<=12><R+1><C5>FG Item: " itemfg.i-no " " itemfg.i-name
                     "<=12><R+3><C1><FROM><C106><LINE><||3>"
                     "<=12><R+5><C5><#21><R+45><C+90><IMAGE#21=" ls-fgitem-img ">" SKIP. 
                 PAGE.
              END.
           END.
           ELSE
           DO:
              ls-fgitem-img = itemfg.box-image .
           
              PUT UNFORMATTED "<#12><C1><FROM><C106><R+47><RECT><||3><C80>" v-qa-text SKIP
                  "<=12><R+1><C5>FG Item: " itemfg.i-no " " itemfg.i-name
                  "<=12><R+3><C1><FROM><C106><LINE><||3>"
                  "<=12><R+5><C5><#21><R+45><C+90><IMAGE#21=" ls-fgitem-img ">" SKIP. 
              PAGE.
           END.
        END.
       END.  /* for each w-ef */

       IF s-prt-set-header AND last-of(job.job-no2) AND est.est-type = 6 THEN DO: /* print set header */
          i = 0.
          FOR EACH bf-eb WHERE bf-eb.company = est.company
                            AND bf-eb.est-no = est.est-no
                            AND bf-eb.form-no > 0 NO-LOCK:
               i = i + 1.
          END.   
          IF i > 1 THEN DO:
             ASSIGN
             v-fg-set = job-hdr.i-no
             v-set-qty = IF AVAILABLE xeb AND xeb.est-type EQ 6 THEN
                           IF AVAILABLE xoe-ordl THEN xoe-ordl.qty ELSE job-hdr.qty
                         ELSE 0
             v-ord-qty = (IF AVAILABLE xoe-ordl THEN xoe-ordl.qty ELSE job-hdr.qty) *
                         (IF est.form-qty LE 1 THEN 1 ELSE v-pqty).
                                                            
             IF ll-jqcust AND NOT AVAILABLE xoe-ordl AND AVAILABLE job-hdr THEN 
                ASSIGN v-ord-qty = v-ord-qty * ( 1 - (cust.over-pct / 100)).

             ASSIGN
             v-over-run = IF AVAILABLE xoe-ordl THEN TRIM(STRING(xoe-ordl.over-pct,">>9.99%"))
                          ELSE IF AVAILABLE xoe-ord THEN TRIM(STRING(xoe-ord.over-pct,">>9.99%")) ELSE ""
             v-under-run = IF AVAILABLE xoe-ordl THEN TRIM(STRING(xoe-ordl.under-pct,">>9.99%"))
                           ELSE IF AVAILABLE xoe-ord THEN TRIM(STRING(xoe-ord.under-pct,">>9.99%")) ELSE "".

             PUT "<R3><C1><#15><C30><P16><B> SET HEADER<P7></B>" SKIP(2)
                 "Job #: " AT 3 v-job-prt "<C25>Our Order #: " v-ord-no 
                 "<C60>Our Date: " v-ord-date SKIP
                 "Est #: " AT 3 v-est-no "<C25>FG #: " v-fg-set "<C60>Due Date: " v-due-date SKIP
                 "<=1><R+6><C2><From><R+5><C78><RECT><||3>" SKIP
                 "<=1><R+6><C2>CUSTOMER INFORMATION <C25> ORDER INFORMATION <C53>ITEM DESCRIPTION" SKIP
                 v-cus[1] AT 3 " PO#: " v-po-no " Set Qty: "  v-set-qty
                 v-i-line[2] AT 90
                 SKIP
                 v-cus[2] AT 3 " Job Qty:" TRIM(STRING(job-hdr.qty * v-pqty,">>>,>>9"))    FORMAT "x(7)"
                 " Order Qty:" STRING(v-ord-qty) FORMAT "x(7)"
                 v-i-line[3] AT 90 SKIP
                 v-cus[3] AT 3  " Cust Part #:" lv-part-no 
                 v-i-line[4] AT 90 SKIP
                 v-cus[4]  AT 3 " Overrun:"  FORMAT "x(7)" 
                 " Underrun:" FORMAT "x(7)"  
                 "Adders:" v-adders FORM "x(33)" AT 90 SKIP
                 "<=1><R+11><C30><P8><B>Set Components<P7></B> <C50>Set item: " v-fg-set SKIP
                 "<C2>FINISHED GOOD #                 DESCRIPTION                       RATIO PER SET" SKIP.
             /* each components */
        
             v-tmp-line = 0.
             FOR EACH xeb FIELDS(stock-no part-dscr1 quantityPerSet) WHERE
                 xeb.company = est.company AND
                 xeb.est-no = est.est-no AND
                 xeb.form-no > 0 NO-LOCK:
                 PUT xeb.stock-no AT 3 SPACE(14) xeb.part-dscr1 SPACE(5) xeb.quantityPerSet FORMAT "->>>>>9.9<<<" SKIP.
                 v-tmp-line = v-tmp-line + 1.
             END.
             v-tmp-line = v-tmp-line + 1.
             /* print raw materials from misc/fram of Est */ 
             FIND LAST b-ef USE-INDEX est-qty WHERE b-ef.company = est.company
                                                AND b-ef.est-no = est.est-no NO-LOCK NO-ERROR.
             DO i = 1 TO 8:
                IF b-ef.spec-no[i] <> "" THEN DO:
                   RUN custom/extradec.p (.0001, b-ef.spec-qty[i],
                                          OUTPUT lv-spec-qty[i]).
                   PUT b-ef.spec-dscr[i] AT 32 SPACE(16) lv-spec-qty[i] SKIP.
                   v-tmp-line = v-tmp-line + 1.
                END.
             END.
             PUT "<=1><R+12><C2><FROM><R+" + string(v-tmp-line) + "><C78><RECT><||3>" FORM "x(150)" SKIP.
             v-tmp-line = v-tmp-line + 12 .
        
             i = 0.
             FOR EACH tt-wm WHERE LOOKUP(tt-wm.m-code,tspostfg-char) > 0:
                  i = i + 1.
             END.
             i = i + 2.
             PUT "  Machine Routing        SU:    Start    Stop    Total   RUN:  Start   Stop    Total   QTY:    In     Out     Waste     Date" SKIP
                 "<=1><R+" + string(v-tmp-line + 1) + "><C2><FROM><R+" + string(i) + "><C78><RECT><||3>" FORM "x(150)" SKIP
                 "<=1><R+" + string(v-tmp-line + 1) + ">" FORM "x(20)".
                 .
        
             i = 0.
             FOR EACH tt-wm WHERE LOOKUP(tt-wm.m-code,tspostfg-char) > 0  BY tt-wm.dseq:
               i = i + 1.
               DISPLAY tt-wm.dscr AT 3
                    tt-wm.s-hr WHEN tt-wm.s-hr NE 0
                    FILL("_",7)  FORMAT "x(7)"    TO 38   WHEN tt-wm.dscr NE ""
                    FILL("_",7)  FORMAT "x(7)"    TO 46   WHEN tt-wm.dscr NE ""
                    FILL("_",7)  FORMAT "x(7)"    TO 54   WHEN tt-wm.dscr NE ""
                    SPACE(2)
                    tt-wm.r-sp WHEN tt-wm.r-sp NE 0
                    FILL("_",7)  FORMAT "x(7)"    TO 69   WHEN tt-wm.dscr NE ""
                    FILL("_",7)  FORMAT "x(7)"    TO 77   WHEN tt-wm.dscr NE ""
                    FILL("_",7)  FORMAT "x(7)"    TO 85   WHEN tt-wm.dscr NE ""
                    FILL("_",8)  FORMAT "x(8)"    TO 99   WHEN tt-wm.dscr NE ""
                    FILL("_",8)  FORMAT "x(8)"    TO 108  WHEN tt-wm.dscr NE ""
                    FILL("_",8)  FORMAT "x(8)"    TO 117  WHEN tt-wm.dscr NE ""
                    FILL("_",8)  FORMAT "x(8)"    TO 129  WHEN tt-wm.dscr NE ""
                    WITH NO-BOX NO-LABELS FRAME o21 WIDTH 132 NO-ATTR-SPACE DOWN STREAM-IO.
             END.
             FOR EACH tt-wm:
                 DELETE tt-wm.
             END.
             v-tmp-line = v-tmp-line + 3 + i /* 4 and add machine routing lines */.
        
             v-shipto = IF AVAILABLE xoe-rel THEN xoe-rel.ship-id 
                        ELSE IF AVAILABLE xeb THEN xeb.ship-id
                        ELSE IF AVAILABLE xoe-ord THEN xoe-ord.sold-id 
                        ELSE "".
             FIND FIRST tt-prem WHERE tt-prem.tt-job-no  EQ job-hdr.job-no
                                   AND tt-prem.tt-job-no2  EQ job-hdr.job-no2 NO-LOCK NO-ERROR.
             IF NOT AVAILABLE tt-prem THEN DO:
                FIND FIRST bf-eb WHERE bf-eb.company = est.company
                                   AND bf-eb.est-no = est.est-no
                                   AND bf-eb.form-no = 0
                                   AND bf-eb.blank-no = 0 NO-LOCK NO-ERROR.
                CREATE tt-prem.
                IF AVAILABLE bf-eb THEN 
                   ASSIGN tt-prem.tt-#-bundle = STRING(bf-eb.cas-cnt)
                          tt-prem.tt-#-unit = STRING(bf-eb.cas-pal)
                          tt-prem.tt-pallet = bf-eb.tr-no
                          tt-prem.tt-count  = bf-eb.tr-cnt.
             END.

             FOR EACH tt-formtext:
                DELETE tt-formtext.
             END.    
             lv-text = "".
             FOR EACH notes FIELDS(note_text) NO-LOCK WHERE
                 notes.rec_key = job.rec_key AND
                 lookup(notes.note_code,"SH,UN") GT 0:
                 lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
             END.
             FOR EACH notes FIELDS(note_text) NO-LOCK WHERE
                 notes.rec_key = itemfg.rec_key AND
                 lookup(notes.note_code,"SH,UN") GT 0:
                 lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
             END.
             DO li = 1 TO 15:
                CREATE tt-formtext.
                ASSIGN
                   tt-line-no = li
                   tt-length  = 72.
             END.
             RUN custom/formtext.p (lv-text).
             i = 0.
             v-dept-inst = "".
             FOR EACH tt-formtext:
                i = i + 1.
                IF  i <= 15 THEN v-dept-inst[i] = tt-formtext.tt-text.      
             END.
             IF v-ship <> "" THEN v-dept-inst[15] = v-ship.  /* shipto notes */
             PUT "<=1><R+" + string(v-tmp-line) + ">" FORM "X(20)".
             v-tmp-line = v-tmp-line + 1.

             PUT "Unitizing Bale <C24>Date <C44>Units <C62>Complete" AT 3 SKIP
                 "# Per Bndl: " AT 3 tt-prem.tt-#-bundle "<C20>_____________________ <C40>____________________  <C60>________________" SKIP
                 "# Per Unit: " AT 3 tt-prem.tt-#-unit "<C20>_____________________ <C40>____________________  <C62>Partial" SKIP
                 "Pallet: " AT 3 tt-prem.tt-pallet FORM "x(10)"  "<C20>_____________________ <C40>____________________ " SKIP
                 "Count:  " AT 3 tt-prem.tt-count FORM ">>>>9" SKIP
                 "<=1><R+" + string(v-tmp-line) + "><C2><FROM><R+6><C78><RECT><||3>" FORM "x(150)" SKIP
                 "<=1><R+" + string(v-tmp-line + 7) + "><C2><FROM><R+16><C78><RECT><||3>" FORM "x(150)" SKIP
        
                 "<=1><R+" + string(v-tmp-line + 7) + "><C2>Special instructions  <C51>SHIPPING INFO       Ship to: " + v-shipto FORM "x(250)" SKIP
                 v-dept-inst[1] AT 3 FORM "x(82)" CHR(124) FORMAT "xx" v-shp[1] SKIP
                 v-dept-inst[2] AT 3 FORM "x(82)" CHR(124) FORMAT "xx" v-shp[2] SKIP
                 v-dept-inst[3] AT 3 FORM "x(82)" CHR(124) FORMAT "xx" v-shp[3] SKIP
                 v-dept-inst[4] AT 3 FORM "x(82)" CHR(124) FORMAT "xx" v-shp[4] SKIP
                 v-dept-inst[5] AT 3 FORM "x(82)" CHR(124) FORMAT "xx" "Item PO #:" v-po-no SKIP
                 v-dept-inst[6] AT 3 SKIP
                 v-dept-inst[7] AT 3 SKIP
                 v-dept-inst[8] AT 3 SKIP
                 v-dept-inst[9] AT 3 SKIP
                 v-dept-inst[10] AT 3 SKIP
                 v-dept-inst[11] AT 3 SKIP
                 v-dept-inst[12] AT 3 SKIP
                 v-dept-inst[13] AT 3 SKIP
                 v-dept-inst[14] AT 3 SKIP
                 v-dept-inst[15] AT 3 
                 .
             PAGE.
          END. /* i > 1*/
       END. /* set header printing  est.est-type = 6 */
    END.  /* each job */
/*    end.  /* end v-local-loop  */*/

PROCEDURE pConvertToHours:
    DEFINE INPUT PARAMETER ipdHours AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ipcOoutputHour AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cHours AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iHours AS INTEGER.
    DEFINE VARIABLE dResult AS DECIMAL.
    DEFINE VARIABLE cRoundHour AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCheckdec AS INTEGER.

    ASSIGN cHours = STRING(ipdHours) .
   
    ASSIGN iHours =  INT( SUBSTRING(cHours,1,3)).
     
    cRoundHour = SUBSTRING(cHours,INDEX(cHours,".") + 1).

    ASSIGN dResult = (int(cRoundHour) * 60) / 100.
    
    IF INDEX(string(dResult),".") > 0 THEN
        iCheckdec = int(SUBSTRING(string(dResult),INDEX(string(dResult),".") + 1)).
    
    IF INDEX(string(dResult),".") > 0 THEN
    dResult = INT( SUBSTRING(STRING(dResult),1,LENGTH(string(dResult)) - 1) ) .

    IF iCheckdec GT 0 THEN
        dResult = dResult + 1 .
    ipcOoutputHour =   string(iHours) + ":" +  string(int(dResult),"99") .

END PROCEDURE.

PROCEDURE pConvertToHMin:
    DEFINE INPUT PARAMETER ipdHours AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ipiOoutputHour AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER ipdOoutputMinute AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE cHours AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iHours AS INTEGER.
    DEFINE VARIABLE dResult AS DECIMAL.
    DEFINE VARIABLE cRoundHour AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCheckdec AS INTEGER.

    ASSIGN cHours = STRING(ipdHours) .
   
    ASSIGN iHours =  INT( SUBSTRING(cHours,1,3)).
     
    cRoundHour = SUBSTRING(cHours,INDEX(cHours,".") + 1).

    ASSIGN dResult = (int(cRoundHour) * 60) / 100.
    
    IF INDEX(string(dResult),".") > 0 THEN
        iCheckdec = int(SUBSTRING(string(dResult),INDEX(string(dResult),".") + 1)).
    
    IF INDEX(string(dResult),".") > 0 THEN
    dResult = INT( SUBSTRING(STRING(dResult),1,LENGTH(string(dResult)) - 1) ) .

    IF iCheckdec GT 0 THEN
        dResult = dResult + 1 .

    ipiOoutputHour =   iHours .
    ipdOoutputMinute = dResult .

END PROCEDURE.

PROCEDURE pGetpoNum:
DEFINE INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEFINE INPUT PARAM ip-form-no LIKE po-ordl.s-num NO-UNDO.
DEFINE OUTPUT PARAM op-po-no LIKE po-ordl.po-no NO-UNDO.
DEFINE  BUFFER bf-job FOR job.
DEFINE BUFFER bf-job-hdr FOR job-hdr.

DEF VAR v-ord-no LIKE oe-ordl.ord-no NO-UNDO.


ASSIGN
 op-po-no = 0
 v-ord-no = 0.

FIND oe-ordl WHERE ROWID(oe-ordl) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL oe-ordl THEN DO:
  ASSIGN
   v-ord-no = oe-ordl.ord-no
   op-po-no = oe-ordl.po-no-po.

  FIND FIRST bf-job
      WHERE bf-job.company EQ oe-ordl.company
        AND bf-job.job-no  EQ oe-ordl.job-no
        AND bf-job.job-no2 EQ oe-ordl.job-no2
      NO-LOCK NO-ERROR.  
END.

ELSE FIND bf-job WHERE ROWID(bf-job) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL bf-job THEN
FOR EACH job-mat
    WHERE job-mat.company EQ bf-job.company
      AND job-mat.job     EQ bf-job.job
      AND job-mat.job-no  EQ bf-job.job-no
      AND job-mat.job-no2 EQ bf-job.job-no2
      AND job-mat.frm     EQ ip-form-no
    NO-LOCK,

    FIRST item
    WHERE item.company  EQ bf-job.company
      AND item.i-no     EQ job-mat.rm-i-no
      AND INDEX("1234BPR",item.mat-type) GT 0
    NO-LOCK,

    FIRST bf-job-hdr OF bf-job NO-LOCK,

    FIRST reftable
    WHERE reftable.reftable EQ "ORDERPO"
      AND reftable.company  EQ bf-job.company
      AND reftable.loc      EQ STRING(v-ord-no,"9999999999")
      AND reftable.code     EQ STRING(job-mat.job,"9999999999") +
                               STRING(job-mat.frm,"9999999999")
      AND reftable.code2    EQ job-mat.rm-i-no
    NO-LOCK
    
    BY job-mat.blank-no
    BY job-mat.rm-i-no:
  op-po-no = reftable.val[1].
  LEAVE.
END.

IF op-po-no EQ 0 THEN
FOR EACH po-ordl
    WHERE po-ordl.company   EQ bf-job.company
      AND po-ordl.job-no    EQ bf-job.job-no
      AND po-ordl.job-no2   EQ bf-job.job-no2
      AND (po-ordl.s-num    EQ ip-form-no OR po-ordl.s-num EQ ?)
      AND po-ordl.item-type EQ YES
    USE-INDEX job-no NO-LOCK,

    FIRST po-ord WHERE
    po-ord.company EQ po-ordl.company AND
    po-ord.po-no   EQ po-ordl.po-no 
    NO-LOCK,

    FIRST item
    WHERE item.company  EQ po-ordl.company
      AND item.i-no     EQ po-ordl.i-no
      AND INDEX("1234BPR",item.mat-type) GT 0
    NO-LOCK:

  op-po-no = po-ordl.po-no.
  LEAVE.
END.
END PROCEDURE.
 
        HIDE ALL NO-PAUSE.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
