/* oe/getqpric.p   get price from quote */

DEF INPUT        PARAM v-recid      AS   RECID.
DEF INPUT        PARAM v-part-no    LIKE quoteit.part-no.
DEF INPUT        PARAM v-part-no2   LIKE quoteit.part-no.
DEF INPUT-OUTPUT PARAM v-price      LIKE oe-ordl.price.
DEF INPUT-OUTPUT PARAM v-uom        LIKE oe-ordl.pr-uom.
DEF OUTPUT       PARAM v-q-no       LIKE quotehd.q-no.
DEF INPUT-OUTPUT PARAM iop-qty       AS INT NO-UNDO.

{sys/inc/var.i NEW SHARED}

DEF VAR s-est-no         LIKE est.est-no NO-UNDO.
DEF VAR fil_id AS RECID NO-UNDO.
DEF VAR save_id AS RECID NO-UNDO.
DEF VAR v-chose-price AS LOG NO-UNDO.

DEF NEW SHARED TEMP-TABLE tt-report NO-UNDO LIKE report.

DEF TEMP-TABLE w-est-no NO-UNDO
    FIELD w-est-no LIKE itemfg.est-no
    FIELD w-run AS LOG
    INDEX w-est-no w-est-no.

FIND est WHERE RECID(est) EQ v-recid NO-LOCK NO-ERROR.
IF NOT AVAIL est THEN LEAVE.

ASSIGN
 cocode = est.company
 locode = est.loc.

EMPTY TEMP-TABLE w-est-no.

CREATE w-est-no.
w-est-no = est.est-no.

DO WHILE AVAIL w-est-no:
  ASSIGN
   w-run    = YES
   s-est-no = w-est-no.

  FOR EACH eb FIELDS(master-est-no)
      WHERE eb.company             EQ cocode
        AND eb.est-no              EQ s-est-no
        AND TRIM(eb.master-est-no) NE ""
        AND NOT CAN-FIND(FIRST w-est-no WHERE w-est-no EQ eb.master-est-no)
      NO-LOCK:
         
    CREATE w-est-no.
    w-est-no = eb.master-est-no.
  END.

  FIND FIRST w-est-no WHERE w-run EQ NO NO-ERROR.
END.

FOR EACH w-est-no BREAK BY w-est-no:
  IF NOT FIRST-OF(w-est-no) THEN DELETE w-est-no.
END.

ASSIGN
 save_id    = fil_id
 i          = 0
 s-est-no   = est.est-no.

IF v-part-no2 EQ "0" THEN v-part-no2 = "".

DO j = 1 TO 2:
  FOR EACH w-est-no,

      EACH quotehd
      WHERE quotehd.company EQ cocode
        AND quotehd.loc     EQ locode
        AND quotehd.est-no  EQ w-est-no
      USE-INDEX quote NO-LOCK,

      EACH quoteitm OF quotehd
      WHERE quoteitm.part-no  EQ v-part-no OR
            (quoteitm.part-no EQ v-part-no2 AND v-part-no2 NE "")
      USE-INDEX q-line NO-LOCK,
      EACH quoteqty OF quoteitm
      USE-INDEX qt-qty NO-LOCK

      BY quotehd.q-no DESC
      BY quoteqty.qty DESC:

    CREATE tt-report.
    ASSIGN
     tt-report.key-01 = STRING(i,"9999999999")
     tt-report.key-02 = STRING(quotehd.q-no,">>>>>>")
     tt-report.rec-id = RECID(quoteqty)
     i                = i + 1
     fil_id           = IF i EQ 1 AND j EQ 1 THEN RECID(tt-report) ELSE ?
     tt-report.key-03 = STRING(quoteqty.rels,">>")
     .
  END.
  
  IF j EQ 1 AND i GT 0 OR j EQ 2 THEN LEAVE.
END.

IF i GT 1 OR (j EQ 2 AND i GT 0) THEN
DO:
   RUN oe/g-qtpric.w (v-part-no, OUTPUT fil_id).
   IF fil_id NE ? THEN
      v-chose-price = YES.
END.

FIND tt-report WHERE RECID(tt-report) EQ fil_id NO-LOCK NO-ERROR.

IF AVAIL tt-report THEN DO:
   FIND quoteqty WHERE RECID(quoteqty) EQ tt-report.rec-id NO-LOCK NO-ERROR.
   IF AVAIL quoteqty THEN
   DO:
      ASSIGN
         v-price = quoteqty.price
         v-uom   = quoteqty.uom
         v-q-no  = quoteqty.q-no.

      IF v-chose-price THEN
         iop-qty  = quoteqty.qty.
   END.
END.  

fil_id = save_id.
