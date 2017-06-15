/* ----------------------------------------------  */
/*  cecrep/jobmcpt.p  factory ticket  for Multicell <= Xprint landscape */
/* -------------------------------------------------------------------------- */

&scoped-define PR-PORT FILE,TERMINAL,FAX_MODEM,VIPERJOBTICKET

DEFINE INPUT PARAMETER v-format AS CHARACTER.
DEFINE SHARED VARIABLE v-dept-log AS LOG NO-UNDO.
DEFINE SHARED VARIABLE v-dept-codes AS CHARACTER NO-UNDO.
DEFINE VARIABLE prt-copies       AS INTEGER          NO-UNDO.
DEFINE VARIABLE v-start-compress AS cha              NO-UNDO.
DEFINE VARIABLE v-end-compress   AS cha              NO-UNDO.
DEFINE VARIABLE k_frac           AS DECIMAL          INIT 6.25 NO-UNDO.
DEFINE VARIABLE v-ink-1          AS cha              FORM "X(30)" NO-UNDO.
DEFINE VARIABLE v-ink-2          AS cha              FORM "X(30)" NO-UNDO.
DEFINE VARIABLE v-ink-3          AS cha              FORM "X(30)" NO-UNDO.
DEFINE VARIABLE v-ink-4          AS cha              FORM "X(30)" NO-UNDO.
DEFINE VARIABLE v-dept-note      AS cha              FORM "x(55)" EXTENT 20 NO-UNDO.
DEFINE VARIABLE v-sp-note        AS cha              FORM "x(55)" EXTENT 15 NO-UNDO.
DEFINE VARIABLE v-dept-note2     AS cha              FORM "x(38)" EXTENT 10 NO-UNDO.
DEFINE VARIABLE v-spec-note      AS cha              FORM "x(124)" EXTENT 10 NO-UNDO.
DEFINE VARIABLE v-deptnote       AS cha              NO-UNDO.
DEFINE VARIABLE v-dept-length    AS DECIMAL          NO-UNDO.
DEFINE VARIABLE lv-under-run     AS cha              NO-UNDO.
DEFINE VARIABLE li-under-run     AS DECIMAL          NO-UNDO.
DEFINE VARIABLE lv-over-run      AS cha              NO-UNDO.
DEFINE VARIABLE li-over-run      AS DECIMAL          NO-UNDO.
DEFINE VARIABLE lv-part-name     AS cha              FORM "x(20)" NO-UNDO.
DEFINE VARIABLE lv-fg-name       AS cha              NO-UNDO.
DEFINE VARIABLE v-cust-po        LIKE oe-ordl.po-no    NO-UNDO.
DEFINE VARIABLE v-req-code1      LIKE oe-ordl.req-code NO-UNDO.
DEFINE VARIABLE v-req-code2 LIKE oe-ordl.req-code NO-UNDO.

DEFINE VARIABLE v-contact   AS cha     NO-UNDO.
DEFINE VARIABLE v-perf      AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-reversed  AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-set-qty   AS INTEGER NO-UNDO.
DEFINE VARIABLE v-tel       AS cha     NO-UNDO.
DEFINE VARIABLE v-fax       AS cha     NO-UNDO.
DEFINE VARIABLE v-email     AS cha     NO-UNDO.
DEFINE VARIABLE v-cas-no    AS cha     NO-UNDO.
DEFINE VARIABLE v-qty-1     AS INTEGER NO-UNDO.
DEFINE VARIABLE v-qty-2     AS INTEGER NO-UNDO.
DEFINE VARIABLE v-num-tags  AS INTEGER NO-UNDO.
DEFINE VARIABLE v-due-date1 AS DATE    NO-UNDO.
DEFINE VARIABLE v-due-date2 AS DATE    NO-UNDO.
DEFINE VARIABLE v-due-date3 AS DATE    NO-UNDO.
DEFINE VARIABLE v-dock-note AS cha     NO-UNDO.

DEFINE VARIABLE v-board-type-1   AS cha     NO-UNDO FORMAT "x(12)".
DEFINE VARIABLE v-board-type-2   AS cha     NO-UNDO FORMAT "x(12)".
DEFINE VARIABLE v-board-type-3   AS cha     NO-UNDO FORMAT "x(12)".
DEFINE VARIABLE v-board-type-1-1 AS cha     NO-UNDO.
DEFINE VARIABLE v-board-type-2-1 AS cha     NO-UNDO.
DEFINE VARIABLE v-board-type-3-1 AS cha     NO-UNDO.

DEFINE VARIABLE v-board-size-1   AS cha     NO-UNDO.
DEFINE VARIABLE v-board-size-2   AS cha     NO-UNDO.
DEFINE VARIABLE v-board-size-3   AS cha     NO-UNDO.
DEFINE VARIABLE v-board-qty-1    AS INTEGER FORMAT ">>>,>>>" NO-UNDO.
DEFINE VARIABLE v-board-qty-2    AS INTEGER FORMAT ">>>,>>>" NO-UNDO.
DEFINE VARIABLE v-board-qty-3      AS INTEGER    FORMAT ">>>,>>>" NO-UNDO.
DEFINE VARIABLE v-slot-height1     AS DECIMAL    FORMAT ">9.99999" NO-UNDO.
DEFINE VARIABLE v-slot-height2     AS DECIMAL    FORMAT ">9.99999" NO-UNDO.
DEFINE VARIABLE v-slot-height3     AS DECIMAL    FORMAT ">9.99999" NO-UNDO.
DEFINE VARIABLE v-dc-msetup        AS cha        EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-dc-setup         AS cha        EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-dies             AS cha        EXTENT 6 NO-UNDO.
DEFINE VARIABLE tb_app-unprinted   AS LOGICAL    NO-UNDO.
DEFINE VARIABLE fi_per-set         AS DECIMAL    NO-UNDO.
DEFINE VARIABLE fi_msf             AS DECIMAL    FORMAT "99.9999" NO-UNDO.
DEFINE VARIABLE v-len-array1       AS CHARACTER  FORMAT "x(9)" EXTENT 28 NO-UNDO.
DEFINE VARIABLE v-len-array2       AS CHARACTER  FORMAT "x(9)" EXTENT 28 NO-UNDO.
DEFINE VARIABLE v-len-array3       AS CHARACTER  FORMAT "x(9)" EXTENT 28 NO-UNDO.
/* DEF VAR v-cas-no LIKE eb.cas-no NO-UNDO. */
DEFINE VARIABLE v-cas-cnt          LIKE eb.cas-cnt NO-UNDO.
DEFINE VARIABLE v-skid             LIKE eb.tr-no   NO-UNDO.
DEFINE VARIABLE v-case-pallet      AS CHARACTER  NO-UNDO FORMAT "x(17)".
DEFINE VARIABLE v-casetag-param    AS CHARACTER  NO-UNDO FORMAT "x(20)" INIT "".

DEFINE VARIABLE v-len              AS DECIMAL    FORMAT ">>>>>>9.99<<<<" NO-UNDO.
DEFINE VARIABLE v-hight1           AS DECIMAL    FORMAT ">>>>>>9.99<<<<" NO-UNDO.
DEFINE VARIABLE v-qty-set          AS DECIMAL    FORMAT ">>>9" NO-UNDO.
DEFINE VARIABLE v-slt1             AS DECIMAL    FORMAT ">>9" NO-UNDO.
DEFINE VARIABLE scr-in-cell-length AS DECIMAL    FORMAT ">9.999999" NO-UNDO.
DEFINE VARIABLE scr-end-cell-l2    AS DECIMAL    FORMAT ">9.999999" NO-UNDO.

DEFINE VARIABLE v-wid              AS DECIMAL    FORMAT ">>>>>>9.99<<<<" NO-UNDO.
DEFINE VARIABLE v-hight2           AS DECIMAL    FORMAT ">>>>>>9.99<<<<" NO-UNDO.
DEFINE VARIABLE v-qty-set2         AS DECIMAL    FORMAT ">>>9" NO-UNDO.
DEFINE VARIABLE v-slt2             AS DECIMAL    FORMAT ">>9" NO-UNDO.
DEFINE VARIABLE scr-in-cell-width  AS DECIMAL    FORMAT ">9.999999" NO-UNDO.
DEFINE VARIABLE scr-end-cell-w2    AS DECIMAL    FORMAT ">9.999999" NO-UNDO.

DEFINE VARIABLE v-wid3             AS DECIMAL    FORMAT ">>>>>>9.99<<<<" NO-UNDO.
DEFINE VARIABLE v-hight3           AS DECIMAL    FORMAT ">>>>>>9.99<<<<" NO-UNDO.
DEFINE VARIABLE v-qty-set3         AS DECIMAL    FORMAT ">>>9" NO-UNDO.
DEFINE VARIABLE v-slt3             AS DECIMAL    FORMAT ">>9" NO-UNDO.
DEFINE VARIABLE scr-in-cell-width3 AS DECIMAL    FORMAT ">9.999999" NO-UNDO.
DEFINE VARIABLE scr-end-cell-w3    AS DECIMAL    FORMAT ">9.999999" NO-UNDO.


{jcrep/r-ticket.i "shared"}


{sys/inc/VAR.i SHARED}
{cec/msfcalc.i}
{sys/inc/f16to32.i}
DEFINE NEW SHARED VARIABLE v-out1-id       AS   RECID    NO-UNDO.  /* YSK 06/08/01  was~ local var */
DEFINE NEW SHARED VARIABLE v-out2-id       AS   RECID    NO-UNDO.  /* YSK 06/08/01  was~ local var */

DEFINE VARIABLE laser            AS LOGICAL          INIT NO FORMAT "Y/N" NO-UNDO.
DEFINE VARIABLE v-vend-no        LIKE oe-ordl.vend-no  NO-UNDO.
DEFINE VARIABLE v-po-no          LIKE oe-ordl.po-no-po NO-UNDO.
DEFINE VARIABLE v-qty-or-sup     AS CHARACTER        FORMAT "x(38)" NO-UNDO.
DEFINE VARIABLE v-i-line         AS CHARACTER        EXTENT 4 FORMAT "x(38)" NO-UNDO.
DEFINE VARIABLE v-flag           AS LOGICAL          INIT NO NO-UNDO.
DEFINE VARIABLE v-local-copies   AS INTEGER          NO-UNDO.
DEFINE VARIABLE v-local-loop     AS INTEGER          INIT 1 NO-UNDO.
DEFINE VARIABLE v-print-score    AS LOGICAL          INIT YES NO-UNDO.
DEFINE VARIABLE v-pqty           AS DECIMAL          NO-UNDO.
DEFINE VARIABLE lv-part-no       AS cha              FORM "x(15)" NO-UNDO.
DEFINE VARIABLE lv-rt-num        AS INTEGER          NO-UNDO.
DEFINE STREAM ctl.
DEFINE VARIABLE lv-add-entry     AS INTEGER          NO-UNDO.
DEFINE VARIABLE v-loop-cnt       AS INTEGER          NO-UNDO.
DEFINE VARIABLE v-note-cnt       AS INTEGER          NO-UNDO.
DEFINE VARIABLE v-note-length    AS INTEGER          NO-UNDO.
DEFINE VARIABLE v-die-loc        AS cha              FORM "x(15)" NO-UNDO.

DEFINE VARIABLE v-set-qty2       AS INTEGER          NO-UNDO.
DEFINE VARIABLE v-ord-qty        AS INTEGER          NO-UNDO.
DEFINE VARIABLE v-over-run       AS cha              NO-UNDO.
DEFINE VARIABLE v-under-run      AS cha              NO-UNDO.
DEFINE VARIABLE v-fg-set         AS cha              FORM "x(15)" NO-UNDO.
{custom/notesdef.i}
{cecrep/jc-prem.i}
DEFINE BUFFER b-ef FOR ef.
DEFINE VARIABLE v-xg-flag        AS LOGICAL          NO-UNDO.
DEFINE VARIABLE v-tmp-stype      AS cha              NO-UNDO.
DEFINE VARIABLE v-len-score2     AS cha              EXTENT 13 NO-UNDO.
DEFINE VARIABLE v-tmp-score      AS cha              NO-UNDO.
DEFINE VARIABLE m AS INTEGER.
DEFINE VARIABLE v-ship-notes AS CHARACTER FORMAT "x(38)" EXTENT 12.
DEFINE VARIABLE v-add-space AS LOG.

DEFINE BUFFER bf-eb FOR eb.
DEFINE VARIABLE lv-spec-qty      LIKE ef.spec-qty      FORMAT ">>>,>>9.9<<<<" NO-UNDO.
DEFINE SHARED VARIABLE s-prt-set-header AS LOGICAL          NO-UNDO.
DEFINE VARIABLE v-dept-inst      AS cha              FORM "x(80)" EXTENT 6 NO-UNDO.
DEFINE VARIABLE v-inst2          AS cha              EXTENT 6 NO-UNDO.
DEFINE BUFFER bf-item FOR ITEM.
DEFINE VARIABLE v-type-desc      LIKE ITEM.i-name      NO-UNDO.
DO TRANSACTION:
  {sys/inc/tspostfg.i}
END.

DEFINE VARIABLE d2-text AS CHARACTER FORMAT "X(45)" NO-UNDO.
DEFINE VARIABLE d3-text AS CHARACTER FORMAT "X(45)" NO-UNDO.
DEFINE VARIABLE d4-text AS CHARACTER FORMAT "X(45)" NO-UNDO.
DEFINE VARIABLE d5-text AS CHARACTER FORMAT "X(45)" NO-UNDO.
DEFINE VARIABLE pr-text AS CHARACTER FORMAT "X(45)" NO-UNDO.
DEFINE VARIABLE score-count AS INTEGER NO-UNDO.
DEFINE VARIABLE v-size AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE lv-note-text AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-count AS INTEGER NO-UNDO.
DEFINE VARIABLE v-count-2 AS INTEGER NO-UNDO.
DEFINE VARIABLE v-qa-text AS cha FORM "x(30)" INIT "6/05 Job Ticket QF-119 Rev.A" NO-UNDO.
DEFINE VARIABLE lv-char AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-mod-48-count AS INTEGER NO-UNDO.
DEFINE VARIABLE v-mod-39-count AS INTEGER NO-UNDO.
DEFINE VARIABLE v-cont-string AS cha NO-UNDO.
DEFINE VARIABLE v-prev-k AS INTEGER NO-UNDO.
DEFINE VARIABLE v-tmp-note-length AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-text AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-return-note AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-tmp-line AS INTEGER NO-UNDO.
DEFINE VARIABLE v-shipto AS cha NO-UNDO.

DEFINE BUFFER bf-xeb FOR eb .
DEF BUFFER b-eb1 FOR eb.
DEF BUFFER b-eb2 FOR eb.
DEF BUFFER b-eb3 FOR eb.

{custom/formtext.i NEW}
{sys/inc/notes.i}
DEFINE BUFFER xshipto FOR shipto.

/* {custom/notesdef.i} */
FUNCTION display-cw-dim RETURNS DECIMAL
  ( INPUT ip-dim AS DECIMAL)  FORWARD.
  
  {cecrep/jobmcpt.i "new shared"}
  DEFINE WORKFILE tt-wm LIKE w-m.
  ASSIGN
  v-line[1] = CHR(95) + FILL(CHR(95),40) + CHR(95) + "  " +
  CHR(95) + FILL(CHR(95),40) + CHR(95) + "  " +
  CHR(95) + FILL(CHR(95),40) + CHR(95)
  v-line[2] = v-line[1]
  v-line[3] = CHR(95) + FILL(CHR(95),128) + CHR(95)
  v-line[4] = v-line[3]
  v-line[5] = CHR(95) + FILL(CHR(95),84) + CHR(95) + "  " +
  CHR(95) + FILL(CHR(95),40) + CHR(95)
  v-line[6] = v-line[5]
  v-line[7] = CHR(95) + FILL(CHR(95),25) + CHR(95) + "  " +
  CHR(95) + FILL(CHR(95),99) + CHR(95)
  v-line[8] = v-line[7]
  v-qty-or-sup = IF LOOKUP(v-format,"TriState,RFC,Boxtech,Brick,Corrugat") GT 0
  THEN ("Supplier:"     + FILL("_",28))
  ELSE ("Qty Received: " + FILL("_",24)).
  
  
  
  ASSIGN v-local-copies = 1
  prt-copies = 1.
  
  DO v-local-loop = 1 TO v-local-copies:
    {cecrep/jobprem.i}
    BREAK BY job.job-no BY job.job-no2:
    
    
    v-break = FIRST-OF(job.job-no2).
    
    RELEASE xest.
    RELEASE xef.
    RELEASE xeb.
    RELEASE xoe-ord.
    RELEASE xoe-ordl.
    RELEASE xoe-rel.
    
    /* Create w-ef, v-shp[i], job-related fields */
    RUN cecrep/jobtick1.p (RECID(job-hdr), v-format,
    v-local-loop, v-local-copies).
    FOR EACH w-ef BREAK BY w-ef.frm:
      RELEASE xef.
      RELEASE xeb.
      RELEASE xstyle.
      RELEASE xxprep.
      /* create w-m, calculate form quantities */
      RUN cecrep/jobtick2.p (RECID(w-ef), RECID(job-hdr)).
      
      FIND FIRST est-prep WHERE est-prep.company = est.company
      AND est-prep.est-no = est.est-no NO-LOCK NO-ERROR.
      
      FIND FIRST est-qty WHERE est-qty.company = est.company
      AND est-qty.est-no = est.est-no NO-LOCK NO-ERROR.
      
      FIND FIRST est-op WHERE est-op.company = est-qty.company
      AND est-op.est-no = est-qty.est-no
      AND est-op.qty EQ est-qty.eqty
      AND est-op.LINE LT 500
      AND est-op.s-num EQ xeb.form-no
      AND est.est-type NE 8 NO-LOCK NO-ERROR.
     
      v-pqty = 1.
      v-cp = "".
      IF AVAILABLE xeb THEN DO:

          FIND FIRST b-eb1 NO-LOCK WHERE 
              b-eb1.company EQ xeb.company AND
              b-eb1.est-no  EQ xeb.est-no AND
              b-eb1.form-no NE 0 AND
              b-eb1.blank-no NE 0
              USE-INDEX est-qty
              NO-ERROR.

          IF AVAIL b-eb1 THEN
              FIND FIRST b-eb2 NO-LOCK WHERE 
              b-eb2.company EQ xeb.company AND
              b-eb2.est-no  EQ xeb.est-no AND
              b-eb2.form-no NE 0 AND
              b-eb2.blank-no NE 0 AND
              ROWID(b-eb2) NE ROWID(b-eb1)
              USE-INDEX est-qty
               NO-ERROR.

          IF AVAIL b-eb2 THEN
              FIND FIRST b-eb3 NO-LOCK WHERE 
              b-eb3.company EQ xeb.company AND
              b-eb3.est-no  EQ xeb.est-no AND
              b-eb3.form-no NE 0 AND
              b-eb3.blank-no NE 0 AND
              ROWID(b-eb3) NE ROWID(b-eb1) AND
              ROWID(b-eb3) NE ROWID(b-eb2)
              USE-INDEX est-qty
              NO-ERROR.
              
              DEFINE VARIABLE a-i AS INTEGER.
            
                IF AVAIL b-eb1 THEN DO:
                   v-len-array1 = "".
                   DO a-i = 1 TO 28:
                     IF b-eb1.k-len-array2[a-i] GT 0 THEN
                     v-len-array1[a-i] = STRING({sys/inc/k16.i b-eb1.k-len-array2[a-i]}) .
                     IF b-eb1.k-len-array2[a-i] GT 0 AND INDEX(v-len-array1[a-i], ".") EQ 0 THEN
                     v-len-array1[a-i] = v-len-array1[a-i] + ".0".
                   END.
                END.

                IF AVAIL b-eb2 THEN DO:
                   v-len-array2 = "".
                   DO a-i = 1 TO 28:
                     IF b-eb2.k-len-array2[a-i] GT 0 THEN
                     v-len-array2[a-i] = STRING({sys/inc/k16.i b-eb2.k-len-array2[a-i]}).
                     IF b-eb2.k-len-array2[a-i] GT 0 AND INDEX(v-len-array2[a-i], ".") EQ 0 THEN
                     v-len-array2[a-i] = v-len-array2[a-i] + ".0".
                     
                   END.
                END.
              
                IF AVAIL b-eb3 THEN DO:
                    v-len-array3 = "".
                    DO a-i = 1 TO 28:
                      IF b-eb3.k-len-array2[a-i] GT 0 THEN
                      v-len-array3[a-i] = STRING({sys/inc/k16.i b-eb3.k-len-array2[a-i]}) .
                      IF b-eb3.k-len-array2[a-i] GT 0 AND INDEX(v-len-array3[a-i], ".") EQ 0 THEN
                      v-len-array3[a-i] = v-len-array3[a-i] + ".0".
                      
                    END.
                END.

         
        IF xeb.stock-no NE "" THEN v-fg = xeb.stock-no.
        v-cp = xeb.part-no.
        
        ASSIGN lv-fg-name = itemfg.i-name.
        
        {cec/rollfac.i}
        v-pqty = IF v-rollfac OR xeb.est-type EQ 8 THEN 1 ELSE
        IF xeb.yld-qty LT 0 THEN (-1 / xeb.yld-qty)
        ELSE xeb.yld-qty.
      END.
      
      ASSIGN
      v-loc     = ""
      v-loc-bin = "".
      
      ASSIGN lv-over-run = IF AVAILABLE xoe-ordl THEN TRIM(STRING(xoe-ordl.over-pct,">>9.99%")) ELSE
      IF AVAILABLE xoe-ord  THEN TRIM(STRING(xoe-ord.over-pct,">>9.99%"))  ELSE ""
      li-over-run  = IF AVAILABLE xoe-ordl THEN DECIMAL(xoe-ordl.over-pct) ELSE
      IF AVAILABLE xoe-ord  THEN DECIMAL(xoe-ord.over-pct)  ELSE 0
      lv-under-run = IF AVAILABLE xoe-ordl THEN TRIM(STRING(xoe-ordl.under-pct,">>9.99%")) ELSE
      IF AVAILABLE xoe-ord  THEN TRIM(STRING(xoe-ord.under-pct,">>9.99%"))  ELSE ""
      li-under-run = IF AVAILABLE xoe-ordl THEN DECIMAL(xoe-ordl.under-pct) ELSE
      IF AVAILABLE xoe-ord  THEN DECIMAL(xoe-ord.under-pct)  ELSE 0
      lv-part-name = /* IF AVAIL xeb THEN xeb.part-dscr1 ELSE " " */ itemfg.i-name
      lv-part-no = IF AVAILABLE xoe-ordl THEN xoe-ordl.part-no ELSE itemfg.part-no
      v-cust-po = IF AVAILABLE xoe-ord THEN xoe-ord.po-no ELSE ""
      v-req-code1 = IF AVAILABLE xoe-ordl THEN xoe-ordl.req-code ELSE ""
      v-req-code2 = IF AVAILABLE xoe-ordl THEN xoe-ordl.req-code ELSE "".
      
      
      ASSIGN v-perf = YES
      v-tel = cust.area-code + "-" + cust.phone
      v-cas-no = ""
      v-fax = SUBSTRING(cust.fax,1,3) + '-' + SUBSTRING(cust.fax,4)
      v-email   = cust.email
      v-contact = cust.contact
      v-num-tags = 1
      .
      IF AVAILABLE xef THEN
      ASSIGN v-reversed = (xef.xgrain = "B" OR xef.xgrain = "S").
      
      ASSIGN v-set-qty = IF AVAILABLE xeb /*and xeb.est-type eq 6 */ THEN
      IF AVAILABLE xoe-ordl THEN xoe-ordl.qty
      ELSE job-hdr.qty
      ELSE 0.
      
      IF AVAILABLE est-op THEN DO:
        IF w-ef.frm = 1 THEN DO:
          
          ASSIGN
          v-dc-msetup[1] = est-op.att-type[1]
          v-dc-msetup[2] = est-op.att-type[2]
          v-dc-msetup[3] = est-op.att-type[3].
          IF v-dc-msetup[2] = "" THEN
          v-dc-msetup[2] = est-op.att-type[1].
          IF AVAILABLE est-prep THEN
          v-dc-msetup[4] = est-prep.CODE.
        END.
      END.
      
      IF AVAILABLE est-op THEN DO:
        IF w-ef.frm = 2 THEN DO:
          
          ASSIGN
          v-dc-setup[1] = est-op.att-type[1]
          v-dc-setup[2] = est-op.att-type[2]
          v-dc-setup[3] = est-op.att-type[3].
          IF AVAILABLE est-prep THEN
          v-dc-setup[4] = est-prep.CODE.
        END.
      END.
      IF w-ef.frm = 1 THEN DO:
        /* msf is based on max quantity (includes overrun qty) */
        ASSIGN
        fi_per-set = IF xeb.yld-qty LT 0 THEN -1 / xeb.yld-qty ELSE xeb.yld-qty
        fi_msf  = (v-set-qty *
        (fi_per-set + (fi_per-set * li-over-run) / 100)) *
        (IF v-corr THEN (xeb.t-sqin * .007)
        ELSE (xeb.t-sqin / 144)) / 1000.
        FIND bf-item WHERE bf-item.company = cocode
        AND bf-item.i-no    = v-form-code
        NO-LOCK NO-ERROR.
        IF AVAILABLE bf-item THEN
        v-type-desc = bf-item.i-name.
        ASSIGN v-due-date1 = DATE(v-due-date)
        v-board-type-1 = SUBSTRING(v-type-desc, 1, 12)
        v-board-type-1-1 = SUBSTRING(v-type-desc, 13)
        v-board-size-1 = STRING(fi_msf)
        v-dies[1] =      ""
        v-dies[2] =      ""
        v-die1    =      xeb.die-no
        v-slot-height1 =  {sys/inc/k16.i xeb.dep }
        v-qty-1 = v-set-qty - (v-set-qty * li-under-run  / 100 )
        v-board-qty-1 = ( (v-set-qty * li-over-run  / 100 ) + v-set-qty) * xeb.yld-qty
        .
        IF v-set-qty EQ 0 THEN
        v-board-qty-1 = xeb.yld-qty.
      END.
      
      ELSE IF w-ef.frm = 2 THEN DO:
        ASSIGN
        fi_per-set = IF xeb.yld-qty LT 0 THEN -1 / xeb.yld-qty ELSE xeb.yld-qty
        fi_msf  = (v-set-qty
        * (fi_per-set + (fi_per-set * li-over-run) / 100)) *
        (IF v-corr THEN (xeb.t-sqin * .007)
        ELSE (xeb.t-sqin / 144)) / 1000.
        FIND bf-item WHERE bf-item.company = cocode
        AND bf-item.i-no    = v-form-code
        NO-LOCK NO-ERROR.
        IF AVAILABLE bf-item THEN
        v-type-desc = bf-item.i-name.
        
        ASSIGN v-due-date2 = DATE(v-due-date)
        v-board-type-2 = SUBSTRING(v-type-desc, 1, 12)
        v-board-type-2-1 = SUBSTRING(v-type-desc, 13)
        v-board-size-2 = STRING(fi_msf)
        v-dies[3] =     ""
        v-dies[4] =     ""
        v-die2    =      xeb.die-no
        v-board-qty-2  = ((v-set-qty * li-over-run  / 100) + v-set-qty) * xeb.yld-qty
        v-slot-height2 = {sys/inc/k16.i xeb.dep }
        .
      END.
      ELSE IF w-ef.frm = 3 THEN DO:
        ASSIGN
        fi_per-set = IF xeb.yld-qty LT 0 THEN -1 / xeb.yld-qty ELSE xeb.yld-qty
        fi_msf  = (v-set-qty
        * (fi_per-set + (fi_per-set * li-over-run) / 100)) *
        (IF v-corr THEN (xeb.t-sqin * .007)
        ELSE (xeb.t-sqin / 144)) / 1000.
        FIND bf-item WHERE bf-item.company = cocode
        AND bf-item.i-no    = v-form-code
        NO-LOCK NO-ERROR.
        IF AVAILABLE bf-item THEN
        v-type-desc = bf-item.i-name.
        
        ASSIGN v-due-date3 = DATE(v-due-date)
        v-board-type-3 = SUBSTRING(v-type-desc, 1, 12)
        v-board-type-3-1 = SUBSTRING(v-type-desc, 13)
        v-board-size-3 = STRING(fi_msf)
        v-dies[5] =     ""
        v-dies[6] =     ""
        v-board-qty-3  = ((v-set-qty * li-over-run  / 100) + v-set-qty) * xeb.yld-qty
        v-slot-height3 = {sys/inc/k16.i xeb.dep }
        .
      END.
      
      ASSIGN
      v-qty-2  = v-set-qty + (v-set-qty * li-over-run  / 100)
      v-note-length = 40.
      
      ASSIGN v-tmp-lines = 0
      j = 0
      K = 0
      lv-got-return = 0
      v-dept-note2 = ""
      v-prev-note-rec = ?.
      
      FOR EACH notes WHERE notes.rec_key = cust.rec_key AND
        /* (notes.note_form_no = w-ef.frm OR notes.note_form_no = 0) */
        LOOKUP(notes.note_code,",D1,EN,RN,SN") GT 0  NO-LOCK:
        IF v-prev-note-rec <> ? AND
        v-prev-note-rec <> RECID(notes) THEN v-prev-extent = v-prev-extent + k.
        
        DO i = 1 TO LENGTH(notes.note_text):
          IF i - j >= v-note-length THEN ASSIGN j = i
          lv-got-return = lv-got-return + 1.
          
          v-tmp-lines = ( i - j ) / v-note-length.
          {SYS/INC/ROUNDUP.I v-tmp-lines}
          
          k = v-tmp-lines + lv-got-return +
          IF (v-prev-note-rec <> RECID(notes) AND v-prev-note-rec <> ?) THEN v-prev-extent ELSE 0.
          IF k < 5 THEN v-dept-note2[k] = v-dept-note2[k] + IF SUBSTRING(notes.note_text,i,1) <> CHR(10) THEN SUBSTRING(notes.note_text,i,1)
          ELSE "" .
          
          IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13)
          THEN DO:
            lv-got-return = lv-got-return + 1.
            j = i.
          END.
        END.
        
        ASSIGN v-prev-note-rec = RECID(notes)
        j = 0
        lv-got-return = 0.
      END. /* notes */
      
      IF LAST(w-ef.frm) THEN DO:
          
        PUT    "</PROGRESS>"
        "<P12><C2><B>CUSTOMER NAME:</B> " SUBSTRING(v-cus[1], 1, 18) FORMAT "x(18)"
        "<B><C37>DESCRIPTION/PART #:</B><B><C75>ORDER #:</B> " xoe-ord.ord-no SKIP
        "<C2><B>CUSTOMER PO#:</B> "  v-cust-po "<C37>"  lv-part-name "<C75><B>DUE DATE:</B><P10> " DATE(v-due-date) SPACE xoe-ord.due-code FORMAT "X(5)" SKIP
        "<#1><C1><FROM><C105><R+45><RECT><|3>"
        "<=1><C36><FROM><R+22><C36><LINE><|3>"
        "<=1><C72><FROM><R+22><C72><LINE><|3>"
        
        "<=1><R9.5><C1><FROM><C105><LINE><|3>"
        "<=1><R10.5><C1><FROM><C105><LINE><|3>"
        "<=#1><R17.5><C1><FROM><C105><LINE><|3>"
        "<=#1><R18.5><C1><FROM><C105><LINE><|3>"
        SKIP.
            
        RUN cecrep/jobmcpt2.p
        (RECID(job-hdr),
        v-format,
        cust.terms,
        itemfg.prod-notes,
        OUTPUT v-cas-no ,
        OUTPUT v-cas-cnt,
        OUTPUT v-skid ,
        OUTPUT v-case-pallet,
        OUTPUT v-casetag-param,
        OUTPUT v-len           ,
        OUTPUT v-hight1        ,
        OUTPUT v-qty-set       ,
        OUTPUT v-slt1          ,
        OUTPUT scr-in-cell-length,
        OUTPUT scr-end-cell-l2 ,
        OUTPUT v-wid           ,
        OUTPUT v-hight2        ,
        OUTPUT v-qty-set2      ,
        OUTPUT v-slt2          ,
        OUTPUT scr-in-cell-width,
        OUTPUT scr-end-cell-w2 ,
        OUTPUT v-wid3          ,
        OUTPUT v-hight3        ,
        OUTPUT v-qty-set3      ,
        OUTPUT v-slt3          ,
        OUTPUT scr-in-cell-width3,
        OUTPUT scr-end-cell-w3,
        OUTPUT v-adders1,
        OUTPUT v-adders2,
        OUTPUT v-adders3).
        VIEW FRAME head.   /* factory header display  * */
      END.
    END. /* w-ef*/
    
    FIND FIRST w-ef.
    
    IF AVAILABLE w-ef THEN DO:
      
      i = 0.
      FOR EACH w-m:
        i = i + 1.
      END.
      IF i LT 3 THEN DO i = i + 1 TO 3:
        CREATE w-m.
        w-m.dseq = 999999999.
      END.
      
      lv-rt-num = i + 3.
              
      
      PUT "<C1><R26><FROM><C105><LINE><|3>"       SKIP
      "<R-1><B><P10>Machine Routing          SU:    Start    Stop    Total      RUN:  Hours   Start    Stop   Total   QTY: In     Out     Waste<P10></B>" SKIP.
      i = 0.
      FOR EACH w-m BY w-m.dseq:
        i = i + 1.
        v-letter = substr("UTE",i,1).
        DISPLAY w-m.dscr AT 3
        w-m.s-hr                              WHEN w-m.s-hr NE 0
        FILL("_",7)  FORMAT "x(7)"    TO 38   WHEN w-m.dscr NE ""
        FILL("_",7)  FORMAT "x(7)"    TO 46   WHEN w-m.dscr NE ""
        FILL("_",7)  FORMAT "x(7)"    TO 54   WHEN w-m.dscr NE ""
        SPACE(4)
        w-m.r-sp                              WHEN w-m.r-sp NE 0
        w-m.r-hr                              WHEN w-m.r-hr NE 0
        FILL("_",7)  FORMAT "x(7)"    TO 80   WHEN w-m.dscr NE ""
        FILL("_",7)  FORMAT "x(7)"    TO 88   WHEN w-m.dscr NE ""
        FILL("_",7)  FORMAT "x(7)"    TO 96    WHEN w-m.dscr NE ""
        FILL("_",8)  FORMAT "x(8)"    TO 105  WHEN w-m.dscr NE ""
        FILL("_",8)  FORMAT "x(8)"    TO 114  WHEN w-m.dscr NE ""
        FILL("_",8)  FORMAT "x(8)"    TO 123  WHEN w-m.dscr NE ""
        WITH NO-BOX NO-LABELS FRAME oo1 WIDTH 150 NO-ATTR-SPACE DOWN STREAM-IO.
        v-lines = v-lines + 1.
        /* Per customer requirements, 1st 3 machines only */
        IF i GE 3 THEN
        LEAVE.
      END.
      
      IF i < 7 THEN PUT SKIP(7 - i).
      
      FIND LAST b-ef USE-INDEX est-qty WHERE b-ef.company = est.company
      AND b-ef.est-no = est.est-no NO-LOCK NO-ERROR.
      
      IF AVAILABLE b-ef AND b-ef.form-no = w-ef.frm THEN
      FOR EACH w-m:
        CREATE tt-wm.
        BUFFER-COPY w-m TO tt-wm.
      END.

      v-shipto = IF AVAILABLE xoe-rel THEN xoe-rel.ship-id
      ELSE IF AVAILABLE xeb THEN xeb.ship-id
      ELSE IF AVAILABLE xoe-ord THEN xoe-ord.sold-id
      ELSE "".

      FIND FIRST xshipto NO-LOCK WHERE xshipto.company EQ cust.company AND
                                          xshipto.cust-no EQ cust.cust-no AND
                                          xshipto.ship-id EQ v-shipto
                                         NO-ERROR.
      ASSIGN v-ship-notes = ""
            v-tmp-lines = 0
            j = 0
            K = 1
            lv-got-return = 0
            v-note-length = 25
            v-add-space = NO.
      /*
      DO m = 1 TO 4:
        j = 0.
        IF m GT 1 THEN v-ship-notes[k] = v-ship-notes[k] + " ".
        DO i = 1 TO LENGTH(xshipto.notes[m]) :
          
          IF i - j >= v-note-length AND SUBSTRING(xshipto.notes[m],i,1) EQ " "  THEN 
            ASSIGN j = i
                   lv-got-return = lv-got-return + 1.
          
          v-tmp-lines = ( i - j ) / v-note-length.
          {SYS/INC/ROUNDUP.I v-tmp-lines}
          
          k = v-tmp-lines + lv-got-return.
          
          IF k < 9 THEN v-ship-notes[k] = v-ship-notes[k] + IF SUBSTRING(xshipto.notes[m],i,1) <> CHR(10) THEN SUBSTRING(xshipto.notes[m],i,1)
          ELSE "" .
          
          IF SUBSTRING(xshipto.notes[m],i,1) = CHR(10) OR SUBSTRING(xshipto.notes[m],i,1) = CHR(13)
          THEN DO:
            lv-got-return = lv-got-return + 1.
            j = i.
          END.

        END.
      END.
      */
      RUN getDNotes (INPUT "SP", INPUT "Dept", OUTPUT v-return-note).
      RUN getNotesNew (INPUT "SP", INPUT "Pen", INPUT "Ship", OUTPUT v-return-note).
      RUN getNotesNew (INPUT "SP", INPUT "Book", INPUT "Ship", OUTPUT v-return-note).
/*       RUN getDNotes (INPUT "SP", INPUT "Dept", OUTPUT v-return-note).    */
/*                                                                          */
/*       RUN getDeptNotes (INPUT "SP", INPUT "Dept", OUTPUT v-return-note). */

      PUT "<R30><#7><C1><FROM><C105><LINE><|3>"
      "<=7><C30><FROM><R+19><C30><LINE><|3>"
      "<=7><C72><FROM><R+19><C72><LINE><|3>"
      "<P7>" SKIP.
      DISPLAY "<C2><R30><P10><B>Prep & Packing:" "<C77> Shipping Info:</B>" AT 92 SKIP
      "<C2>Case/Count:" v-cas-no v-cas-cnt "Ship To #:" AT 91 xoe-ord.sold-id WHEN AVAILABLE xoe-ord SKIP
      "<C2>Skid/Cases:" v-case-pallet    "<C74>" v-shp[1] SKIP
      "<C2>Label:" v-casetag-param       "<C74>" v-shp[2] SKIP
      "<C74>" v-shp[3] SKIP
      "<C2>Pack Note:" itemfg.prod-notes "<C74>" v-shp[4] SKIP(1)
      "<C72>" v-ship-notes[1] SKIP
      "<C2>________@____________________<C72>" v-ship-notes[2] SKIP
      "<C72>" v-ship-notes[3] SKIP
      "<C2>________@____________________<C72>" v-ship-notes[4] SKIP
      "<C72>" v-ship-notes[5] SKIP
      "<C2>Total Qty: __________________<C72>" v-ship-notes[6] SKIP
      "<C2>Total Weight: _______________<C72>" v-ship-notes[7] SKIP
      "<C2>Total Pallets: ______________<C72>" v-ship-notes[8] SKIP
      "<C72>" v-ship-notes[9] SKIP                                 
      "<C2>_____________________________<C72>" v-ship-notes[10] SKIP
      "<C2>INITIALS          DATE     <C72.5>End User PO#:" xoe-ordl.part-dscr1 FORMAT "X(25)"
          WITH NO-BOX NO-LABELS FRAME m8 WIDTH 170 NO-ATTR-SPACE STREAM-IO.
  
      v-note-length = 124.
      ASSIGN v-tmp-lines = 0
      j = 0
      K = 0
      lv-got-return = 0
      v-dept-note = ""
      v-prev-note-rec = ?.
      RUN getNotesNew (INPUT "MN", INPUT "Pen", INPUT "Manu", OUTPUT v-return-note).
      RUN getNotesNew (INPUT "MN", INPUT "Spec", INPUT "Manu", OUTPUT v-return-note).
      RUN getNotesNew (INPUT "MN", INPUT "Cust", INPUT "Manu", OUTPUT v-return-note).
/*       RUN getDNotes (INPUT "MN", INPUT "Other", OUTPUT v-return-note). */
/*       RUN getDeptNotes (INPUT "MN", INPUT "Other", OUTPUT v-return-note).  */
            /* was 7 */
      PUT "<=7><c30><B> Manufacturing Notes</B>" SKIP
      v-dept-note[1] AT 38 SKIP /* was 90 */
      v-dept-note[2] AT 38 SKIP
      v-dept-note[3] AT 38 SKIP
      v-dept-note[4] AT 38 SKIP
      v-dept-note[5] AT 38 SKIP
      v-dept-note[6] AT 38 SKIP
      v-dept-note[7] AT 38 SKIP 
      v-dept-note[8] AT 38 SKIP 
      v-dept-note[9] AT 38 SKIP 
      v-dept-note[10] AT 38 SKIP 
      v-dept-note[11] AT 38 SKIP 
      v-dept-note[12] AT 38 SKIP 
      v-dept-note[13] AT 38 SKIP 
      v-dept-note[14] AT 38 SKIP 
      v-dept-note[15] AT 38 SKIP 
      v-dept-note[16] AT 38 SKIP 
      v-dept-note[17] AT 38 SKIP 
      v-dept-note[18] AT 38 . 
      PAGE.
      v-inst = "".
      
      ASSIGN v-spec-note = ""
      v-tmp-lines = 0
      j = 0
      K = 0
      lv-got-return = 0.
      
      FOR EACH notes WHERE notes.rec_key = itemfg.rec_key NO-LOCK.
        DO i = 1 TO LENGTH(notes.note_text) :
          IF i - j >= v-note-length THEN ASSIGN j = i
          lv-got-return = lv-got-return + 1.
          
          v-tmp-lines = ( i - j ) / v-note-length.
          {SYS/INC/ROUNDUP.I v-tmp-lines}
          
          k = v-tmp-lines + lv-got-return.
          IF k < 9 THEN v-spec-note[k] = v-spec-note[k] + IF SUBSTRING(notes.note_text,i,1) <> CHR(10) THEN SUBSTRING(notes.note_text,i,1)
          ELSE "" .
          
          IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13)
          THEN DO:
            lv-got-return = lv-got-return + 1.
            j = i.
          END.
        END.
      END.
            
      IF print-box AND AVAILABLE xest THEN DO:
        RUN cec/desprntU.p (RECID(xef),
        INPUT-OUTPUT v-lines,
        RECID(xest)).
        PAGE.
      END.
      ELSE PAGE.
        
      RELEASE w-ef.
    END.  /*w-ef */
    
    s-prt-set-header = NO.
    
    IF s-prt-set-header AND LAST-OF(job.job-no2) AND est.est-type = 6 THEN DO: /* print set header */
      i = 0.
      FOR EACH bf-eb WHERE bf-eb.company = est.company
        AND bf-eb.est-no = est.est-no
        AND bf-eb.form-no > 0 NO-LOCK:
        i = i + 1.
      END.
      IF i > 1 THEN DO:
        
        v-fg-set = job-hdr.i-no.
        v-set-qty2 = IF AVAILABLE xeb AND xeb.est-type EQ 6 THEN
        IF AVAILABLE xoe-ordl THEN xoe-ordl.qty ELSE job-hdr.qty
        ELSE 0.
        v-ord-qty = (IF AVAILABLE xoe-ordl THEN xoe-ordl.qty ELSE job-hdr.qty) *
        IF est.form-qty LE 1 THEN 1 ELSE v-pqty.
        v-over-run = IF AVAILABLE xoe-ordl THEN TRIM(STRING(xoe-ordl.over-pct,">>9.99%")) ELSE
        IF AVAILABLE xoe-ord  THEN TRIM(STRING(xoe-ord.over-pct,">>9.99%"))  ELSE "".
        v-under-run = IF AVAILABLE xoe-ordl THEN TRIM(STRING(xoe-ordl.under-pct,">>9.99%")) ELSE
        IF AVAILABLE xoe-ord  THEN TRIM(STRING(xoe-ord.under-pct,">>9.99%"))  ELSE "".
        PUT "<R3><C1><#15><C30><P16><B> SET HEADER<P7></B>" SKIP(2)
        "Job #: " AT 3 v-job-prt "<C25>Our Order #: " v-ord-no
        "<C60>Our Date: " v-ord-date SKIP
        "Est #: " AT 3 v-est-no "<C25>FG #: " v-fg-set "<C60>Due Date: " v-due-date SKIP
        "<=1><R+6><C2><From><R+5><C78><RECT><||3>" SKIP
        "<=1><R+6><C2>CUSTOMER INFORMATION <C25> ORDER INFORMATION <C53>ITEM DESCRIPTION" SKIP
        v-cus[1] AT 3 " PO#: " v-po-no " Set Qty: "  v-set-qty2
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
        FOR EACH xeb WHERE xeb.company = est.company
          AND xeb.est-no = est.est-no
          AND xeb.form-no > 0 NO-LOCK:
          PUT xeb.stock-no AT 3 SPACE(14) xeb.part-dscr1 SPACE(5) xeb.yld-qty SKIP.
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
        PUT "<=1><R+12><C2><FROM><R+" + STRING(v-tmp-line) + "><C78><RECT><||3>" FORM "x(150)" SKIP.
        v-tmp-line = v-tmp-line + 12 .
        
        i = 0.
        FOR EACH tt-wm WHERE LOOKUP(tt-wm.m-code,tspostfg-char) > 0:
          i = i + 1.
        END.
        i = i + 2.
        
        PUT "  Machine Routing        SU:    Start    Stop    Total   RUN:  Start   Stop    Total   QTY:    In     Out     Waste     Date" SKIP
        "<=1><R+" + STRING(v-tmp-line + 1) + "><C2><FROM><R+" + STRING(i) + "><C78><RECT><||3>" FORM "x(150)" SKIP
        "<=1><R+" + STRING(v-tmp-line + 1) + ">" FORM "x(20)".
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
        v-tmp-line = v-tmp-line + 3 + i .
        
        v-shipto = IF AVAILABLE xoe-rel THEN xoe-rel.ship-id
        ELSE IF AVAILABLE xeb THEN xeb.ship-id
        ELSE IF AVAILABLE xoe-ord THEN xoe-ord.sold-id
        ELSE "".
                
        FIND FIRST tt-prem WHERE tt-prem.tt-job-no  EQ job-hdr.job-no
        AND tt-prem.tt-job-no2  EQ job-hdr.job-no2 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE tt-prem THEN CREATE tt-prem.
        
        ASSIGN v-tmp-lines = 0
               j = 0
               K = 0
               lv-got-return = 0
               v-dept-inst = "".
        
        {custom/notespr2.i job v-inst2 6 "notes.rec_key = job.rec_key and
        notes.note_form_no = 0" }
        
        DO i = 1 TO 6:
          v-dept-inst[i] = v-inst2[i].
        END.
        
        IF v-ship <> "" THEN v-dept-inst[6] = v-ship.  /* shipto notes */
        PUT "<=1><R+" + STRING(v-tmp-line) + ">" FORM "X(20)".
        v-tmp-line = v-tmp-line + 1.
        PUT "Unitizing Bale <C24>Date <C44>Units <C62>Complete" AT 3 SKIP
        "# Per Bndl: " AT 3 tt-prem.tt-#-bundle "<C20>_____________________ <C40>____________________  <C60>________________" SKIP
        "# Per Unit: " AT 3 tt-prem.tt-#-unit "<C20>_____________________ <C40>____________________  <C62>Partial" SKIP
        "Pattern: " AT 3 tt-prem.tt-pattern "<C20>_____________________ <C40>____________________  <C60>________________" SKIP
        "Pallet: " AT 3 tt-prem.tt-pallet "<C20>_____________________ <C40>____________________ " SKIP
        "<=1><R+" + STRING(v-tmp-line) + "><C2><FROM><R+6><C78><RECT><||3>" FORM "x(150)" SKIP
        "<=1><R+" + STRING(v-tmp-line + 7) + "><C2><FROM><R+7><C78><RECT><||3>" FORM "x(150)" SKIP
        
        "<=1><R+" + STRING(v-tmp-line + 7) + "><C2>Special instructions  <C51>SHIPPING INFO       Ship to: " + v-shipto FORM "x(250)" SKIP
        v-dept-inst[1] AT 3 FORM "x(82)" CHR(124) FORMAT "xx" v-shp[1] SKIP
        v-dept-inst[2] AT 3 FORM "x(82)" CHR(124) FORMAT "xx" v-shp[2] SKIP
        v-dept-inst[3] AT 3 FORM "x(82)" CHR(124) FORMAT "xx" v-shp[3] SKIP
        v-dept-inst[4] AT 3 FORM "x(82)" CHR(124) FORMAT "xx" v-shp[4] SKIP
        v-dept-inst[5] AT 3 FORM "x(82)" CHR(124) FORMAT "xx" "Item PO #:" v-po-no SKIP
        v-dept-inst[6]
        .
        
        PAGE.
      END. /* set header printing */
    END. /* est.est-type = 6 */
    /* end of set header printing */
    
  END.  /* each job */
END.  /* end v-local-loop  */

HIDE ALL NO-PAUSE.


PROCEDURE stackImage:
  DEFINE BUFFER pattern FOR reftable.
  
  IF v-stackcode EQ '' THEN RETURN.
  FIND FIRST pattern NO-LOCK
  WHERE pattern.reftable EQ 'STACKPAT'
  AND pattern.company EQ ''
  AND pattern.loc EQ ''
  AND pattern.CODE EQ SUBSTR(v-stackcode,9,1) NO-ERROR.
  IF AVAILABLE pattern AND SEARCH(pattern.dscr) NE ? THEN
  PUT UNFORMATTED
  "<#71><C27><R+1><FROM><C2><R+12>"
  "<IMAGE#71=" pattern.dscr ">"
  "<R-13>".
END PROCEDURE.

PROCEDURE getNotesNew:
    DEFINE INPUT PARAMETER ipcTypeList AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcNoteType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcPanel AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER v-return-note AS CHARACTER NO-UNDO.
  
    DEFINE VARIABLE li AS INTEGER.
    DEFINE VARIABLE l-width AS INTEGER.
    DEFINE VARIABLE l-rec LIKE note.rec_key.

    IF ipcNoteType = "Pen" THEN l-rec = job.rec_key.
    ELSE IF ipcNoteType = "Spec" THEN l-rec = itemfg.rec_key.
    ELSE l-rec = cust.rec_key.

    IF ipcPanel = "Ship" THEN l-width = 38.
    ELSE l-width = 48.

    ASSIGN
    lv-text = "".

  FOR EACH notes
    WHERE notes.rec_key = l-rec
    AND (notes.note_form_no = w-ef.frm OR notes.note_form_no = 0)
    AND LOOKUP(notes.note_code,ipcTypeList) GT 0
    NO-LOCK:
    lv-text = lv-text /*+ " <B>" + TRIM(notes.note_title) + "</B>" + CHR(10)*/
    + " " + note.note_text + CHR(10).
  END.
  
  IF lv-text NE "" THEN DO:

      DO li = 1 TO 20:
        CREATE tt-formtext.
        ASSIGN
        tt-line-no = li
        tt-length  = l-width.
      END.
      RUN custom/formtext.p (lv-text).
      i = 0.
      FOR EACH tt-formtext:
        i = i + 1.
        IF i <= 20 AND tt-formtext.tt-text NE "" THEN
           IF ipcPanel = "Ship" THEN
               RUN add-ship-note (tt-formtext.tt-text).
           ELSE
               RUN add-dept-note (tt-formtext.tt-text).
        DELETE tt-formtext.
       END.
  END.
  
END PROCEDURE.

/* PROCEDURE getNotes:                                                            */
/*   DEF VAR li AS INT.                                                           */
/*   ASSIGN                                                                       */
/*   lv-text = ""                                                                 */
/*   v-exc-depts = v-exc-depts + "D2,D3,D4,D5".                                   */
/*                                                                                */
/*   FOR EACH notes                                                               */
/*     WHERE notes.rec_key = job.rec_key                                          */
/*     AND (notes.note_form_no = w-ef.frm OR notes.note_form_no = 0)              */
/*     AND note.note_code = "MN"/*LOOKUP(notes.note_code,v-exc-depts) EQ 0*/      */
/*     NO-LOCK:                                                                   */
/*                                                                                */
/*     ASSIGN                                                                     */
/*     lv-note-text = ""                                                          */
/*     v-count = 0                                                                */
/*     v-count-2 = 0                                                              */
/*     v-mod-48-count = 1                                                         */
/*     v-mod-39-count = 1.                                                        */
/*                                                                                */
/*     DO li = 1 TO LENGTH(TRIM(notes.note_text)):                                */
/*       ASSIGN                                                                   */
/*       lv-char = SUBSTR(TRIM(notes.note_text),li,1)                             */
/*       lv-note-text = lv-note-text + lv-char                                    */
/*       v-count = v-count + 1.                                                   */
/* /*                                                               */            */
/* /*       IF lv-char EQ CHR(10) OR                                */            */
/* /*       lv-char EQ CHR(13) THEN                                 */            */
/* /*       DO:                                                     */            */
/* /*         IF v-count <= 480 THEN                                */            */
/* /*         v-count = v-count + ((48 * v-mod-48-count) - li).     */            */
/* /*         ELSE IF v-count > 480 THEN                            */            */
/* /*         ASSIGN                                                */            */
/* /*         v-count = v-count + ((39 * v-mod-39-count) - li)      */            */
/* /*         v-count-2 = v-count-2 + ((39 * v-mod-39-count) - li). */            */
/* /*       END.                                                    */            */
/* /*                                                               */            */
/* /*       IF v-count EQ 480 THEN                                  */            */
/* /*       v-count-2 = 0.                                          */            */
/* /*                                                               */            */
/* /*       ELSE                                                    */            */
/* /*       IF v-count GT 480 THEN                                  */            */
/* /*       v-count-2 = v-count-2 + 1.                              */            */
/* /*                                                               */            */
/* /*       IF v-count <= 480 AND v-count MOD 48 EQ 0 THEN          */            */
/* /*       DO:                                                     */            */
/* /*         ASSIGN                                                */            */
/* /*         lv-note-text = lv-note-text + CHR(10)                 */            */
/* /*         v-mod-48-count = v-mod-48-count + 1.                  */            */
/* /*       END.                                                    */            */
/* /*       ELSE                                                    */            */
/* /*       IF v-count > 480 AND v-count-2 MOD 39 EQ 0 THEN         */            */
/* /*       ASSIGN                                                  */            */
/* /*       lv-note-text = lv-note-text + CHR(10).                  */            */
/* /*       v-mod-39-count = v-mod-39-count + 1.                    */            */
/*     END.                                                                       */
/*                                                                                */
/*     lv-text = lv-text /*+ " <B>" + TRIM(notes.note_title) + "</B>" + CHR(10)*/ */
/*     + " " + lv-note-text + CHR(10).                                            */
/*   END.                                                                         */
/*                                                                                */
/*   DO li = 1 TO 20:                                                             */
/*     CREATE tt-formtext.                                                        */
/*     ASSIGN                                                                     */
/*     tt-line-no = li                                                            */
/*     tt-length  = 48.                                                           */
/*   END.                                                                         */
/*                                                                                */
/* /*   DO li = 12 TO 25:     */                                                  */
/* /*     CREATE tt-formtext. */                                                  */
/* /*     ASSIGN              */                                                  */
/* /*     tt-line-no = li     */                                                  */
/* /*     tt-length  = 39.    */                                                  */
/* /*   END.                  */                                                  */
/*                                                                                */
/*   RUN custom/formtext.p (lv-text).                                             */
/*   ASSIGN                                                                       */
/*   i = 0.                                                                       */
/* /*   v-dept-note = "". */                                                      */
/*                                                                                */
/*   FOR EACH tt-formtext:                                                        */
/*     i = i + 1.                                                                 */
/*     IF i <= 20 THEN                                                            */
/*     v-dept-note[i] = tt-formtext.tt-text.                                      */
/*   END.                                                                         */
/*                                                                                */
/* END PROCEDURE.                                                                 */

PROCEDURE getDNotes:
    DEFINE INPUT PARAMETER ipcTypeList AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcNoteType AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER v-return-note AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-end-note AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-note-length AS INTEGER.
    DEFINE VARIABLE v-prev-note-rec AS INTEGER.
    DEFINE VARIABLE m AS INTEGER.
    DEFINE VARIABLE v-start-note AS CHARACTER NO-UNDO.

    v-start-note = "".
    DO m = 1 TO 4:

      v-start-note = v-start-note + TRIM(xshipto.notes[m]) + " ".

    END.

    RUN sys/convert/txt-to-ln.p ( INPUT v-start-note, 38, OUTPUT v-end-note).
    DO m = 1 TO NUM-ENTRIES(v-end-note,"|"):
        v-return-note = ENTRY(m, v-end-note, "|").
        IF v-return-note GT "" THEN DO:
          IF ipcNoteType = "Dept" THEN
            RUN add-ship-note (INPUT v-return-note).
          ELSE
            RUN add-dept-note(INPUT v-return-note).
        END.

    END.


    /*
    v-note-length = 10.
    DO m = 1 TO 4:
      IF v-prev-note-rec <> 0 AND
      v-prev-note-rec <> m THEN v-prev-extent = v-prev-extent + k.
       j = 0.
      v-return-note = "".
      DO i = 1 TO LENGTH(xshipto.notes[m]):
        
        IF i - j >= v-note-length THEN ASSIGN j = i
        lv-got-return = lv-got-return + 1.
        
        v-tmp-lines = ( i - j ) / v-note-length.
        {SYS/INC/ROUNDUP.I v-tmp-lines}
        
        k = v-tmp-lines + lv-got-return +
        IF v-prev-note-rec <> m AND m NE 0 THEN v-prev-extent ELSE 0.
        
        IF k < 12 THEN 
            ASSIGN v-return-note = v-return-note + IF SUBSTRING(xshipto.notes[m],i,1) <> CHR(10) THEN SUBSTRING(xshipto.notes[m],i,1)
        ELSE "" 
              .
        
        IF SUBSTRING(xshipto.notes[m],i,1) = CHR(10) OR SUBSTRING(xshipto.notes[m],i,1) = CHR(13)
        THEN DO:
          lv-got-return = lv-got-return + 1.
          j = i.
        END.
      END.
      IF v-return-note GT "" THEN DO:
        IF ipcNoteType = "Dept" THEN
          RUN add-ship-note (INPUT v-return-note).
        ELSE
          RUN add-dept-note(INPUT v-return-note).
      END.
        
      ASSIGN v-prev-note-rec = m
      j = 0
      lv-got-return = 0.
        
    END. /* notes */
    */
END PROCEDURE.


/* PROCEDURE getDeptNotes:                                                                                                                 */
/*     DEF INPUT PARAMETER ipcTypeList AS CHAR NO-UNDO.                                                                                    */
/*     DEF INPUT PARAMETER ipcNoteType AS CHAR NO-UNDO.                                                                                    */
/*     DEF output PARAMETER v-return-note AS CHAR NO-UNDO.                                                                                 */
/*     DEF VAR v-note-length AS INT.                                                                                                       */
/*     v-note-length = 40.                                                                                                                 */
/*                                                                                                                                         */
/*     FOR EACH notes WHERE notes.rec_key = job.rec_key /*cust.rec_key*/ AND                                                               */
/*       (notes.note_form_no = w-ef.frm OR notes.note_form_no = 0)                                                                         */
/*       AND LOOKUP(notes.note_code, ipcTypeList) GT 0                                                                                     */
/*       NO-LOCK:                                                                                                                          */
/*                                                                                                                                         */
/*       IF v-prev-note-rec <> ? AND                                                                                                       */
/*       v-prev-note-rec <> RECID(notes) THEN v-prev-extent = v-prev-extent + k.                                                           */
/*                                                                                                                                         */
/*       v-return-note = "".                                                                                                               */
/*       DO i = 1 TO LENGTH(notes.note_text):                                                                                              */
/*         IF i - j >= v-note-length THEN ASSIGN j = i                                                                                     */
/*         lv-got-return = lv-got-return + 1.                                                                                              */
/*                                                                                                                                         */
/*         v-tmp-lines = ( i - j ) / v-note-length.                                                                                        */
/*         {SYS/INC/ROUNDUP.I v-tmp-lines}                                                                                                 */
/*                                                                                                                                         */
/*         k = v-tmp-lines + lv-got-return +                                                                                               */
/*         IF (v-prev-note-rec <> RECID(notes) AND v-prev-note-rec <> ?) THEN v-prev-extent ELSE 0.                                        */
/*                                                                                                                                         */
/*         IF k < 12 THEN v-return-note = v-return-note + IF SUBSTRING(notes.note_text,i,1) <> CHR(10) THEN SUBSTRING(notes.note_text,i,1) */
/*         ELSE "" .                                                                                                                       */
/*                                                                                                                                         */
/*         IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13)                                                     */
/*         THEN DO:                                                                                                                        */
/*           lv-got-return = lv-got-return + 1.                                                                                            */
/*           j = i.                                                                                                                        */
/*         END.                                                                                                                            */
/*       END.                                                                                                                              */
/*       IF v-return-note GT "" THEN DO:                                                                                                   */
/*         IF ipcNoteType = "Dept" THEN                                                                                                    */
/*           RUN add-ship-note (INPUT v-return-note).                                                                                      */
/*         ELSE                                                                                                                            */
/*           RUN add-dept-note(INPUT v-return-note).                                                                                       */
/*       END.                                                                                                                              */
/*                                                                                                                                         */
/*       ASSIGN v-prev-note-rec = RECID(notes)                                                                                             */
/*       j = 0                                                                                                                             */
/*       lv-got-return = 0.                                                                                                                */
/*                                                                                                                                         */
/*     END. /* notes */                                                                                                                    */
/* END PROCEDURE.                                                                                                                          */

PROCEDURE add-dept-note:
  DEFINE INPUT PARAMETER ipNote AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER.
  DO i = 1 TO 19:
    IF v-dept-note[i] = "" THEN DO:
        v-dept-note[i] = ipNote.
        LEAVE.
    END.

  END.
END PROCEDURE.

PROCEDURE add-ship-note:
  DEFINE INPUT PARAMETER ipNote AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DO i = 1 TO 12:
    IF v-ship-notes[i] = "" THEN DO:
        v-ship-notes[i] = ipNote.
        LEAVE.
    END.    
  END.
END PROCEDURE.

FUNCTION display-cw-dim RETURNS DECIMAL
  ( INPUT ip-dim AS DECIMAL ) :
  /*------------------------------------------------------------------------------
  Purpose:
  Notes:
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE out-dim AS DECIMAL DECIMALS 6 NO-UNDO.
  
  out-dim = ROUND(trunc(ip-dim,0) + ((ip-dim - trunc(ip-dim,0)) / K_FRAC), IF v-cecscrn-char NE "Decimal" THEN 2 ELSE 6).   /* @ ip-dim */
  RETURN out-dim.   /* Function return value. */
  
END FUNCTION.
/* end ---------------------------------- copr. 1997  advanced software, inc. */
