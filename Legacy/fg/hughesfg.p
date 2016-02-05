/* -------------------------------------------------- fg/hughesfg.p 12/02 JLF */
/* Finished Goods - Hughes FG# system                                         */
/* -------------------------------------------------------------------------- */

DEF INPUT  PARAM ip-rowid AS   ROWID.
DEF OUTPUT PARAM op-i-no  LIKE itemfg.i-no NO-UNDO INIT "".

{sys/inc/var.i SHARED}

DEF BUFFER b-eb FOR eb.
DEF BUFFER bb   FOR eb.

DEF VAR li AS INT NO-UNDO.
DEF VAR li1 AS INT NO-UNDO.
DEF VAR v-len AS INT NO-UNDO.

FOR FIRST b-eb FIELDS(company est-no form-no) WHERE
    ROWID(b-eb) EQ ip-rowid NO-LOCK,
    FIRST eb FIELDS(company procat cust-no)
    WHERE eb.company EQ b-eb.company
      AND eb.est-no  EQ b-eb.est-no
      AND eb.form-no NE 0
    NO-LOCK,
    
    FIRST est FIELDS(est-type)
    WHERE est.company EQ b-eb.company
      AND est.est-no  EQ b-eb.est-no
    NO-LOCK:

  ASSIGN
     op-i-no = SUBSTR(eb.procat,1,1) + SUBSTR(eb.cust-no,1,6)
     v-len   = LENGTH(op-i-no) + 1.

  FOR EACH itemfg
      WHERE itemfg.company          EQ eb.company
        AND itemfg.i-no             BEGINS op-i-no
        AND SUBSTR(itemfg.i-no,v-len,4) GE "0000"
        AND SUBSTR(itemfg.i-no,v-len,4) LE "9999"
      NO-LOCK
      BY SUBSTR(itemfg.i-no,1,11) DESC:
    li = INT(SUBSTR(itemfg.i-no,v-len,4)) NO-ERROR. 
    IF ERROR-STATUS:ERROR THEN li = 0.
    ELSE LEAVE.
  END.

  FOR EACH oe-ordl
      WHERE oe-ordl.company          EQ eb.company
        AND oe-ordl.i-no             BEGINS TRIM(op-i-no)
        AND SUBSTR(oe-ordl.i-no,v-len,4) GT STRING(li,"9999")
        AND SUBSTR(oe-ordl.i-no,v-len,4) LE "9999"
      NO-LOCK
      BY SUBSTR(oe-ordl.i-no,1,v-len + 3) DESC:
    li1 = INT(SUBSTR(oe-ordl.i-no,v-len,4)) NO-ERROR. 
    IF ERROR-STATUS:ERROR THEN li1 = 0.
    ELSE LEAVE.
  END.

  ASSIGN
     li = MAX(li,li1)
     li1 = 0.

  IF b-eb.form-no NE 0 THEN
  FOR EACH bb OF est
      WHERE bb.form-no  NE 0
        AND bb.blank-no NE 0
      NO-LOCK
      BY bb.form-no BY bb.blank-no:
    li1 = li1 + 1.
    IF ROWID(bb) EQ ROWID(b-eb) THEN LEAVE.
  END.

  op-i-no = op-i-no + STRING(li + 1,"9999") + "A" +
            (IF est.est-type EQ 2 OR est.est-type EQ 6 THEN
               STRING(li1,"99")
             ELSE "99").
END.
