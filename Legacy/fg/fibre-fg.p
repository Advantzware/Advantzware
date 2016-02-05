/* -------------------------------------------------- fg/fibre-fg.p 11/03 JLF */
/* Finished Goods - Fibre FG# system                                          */
/* -------------------------------------------------------------------------- */

DEF INPUT  PARAM ip-rowid AS   ROWID.
DEF OUTPUT PARAM op-i-no  LIKE itemfg.i-no NO-UNDO INIT "".

{sys/inc/var.i SHARED}

DEF BUFFER b-eb FOR eb.
DEF BUFFER bb   FOR eb.

DEF VAR li AS INT NO-UNDO.
DEF VAR li1 AS INT NO-UNDO.
DEF VAR li2 AS INT NO-UNDO.

FOR FIRST b-eb WHERE ROWID(b-eb) EQ ip-rowid NO-LOCK,

    FIRST eb
    WHERE eb.company EQ b-eb.company
      AND eb.est-no  EQ b-eb.est-no
      AND eb.form-no NE 0
    NO-LOCK,
    
    FIRST est
    WHERE est.company EQ b-eb.company
      AND est.est-no  EQ b-eb.est-no
    NO-LOCK:

  ASSIGN
   li      = 0
   op-i-no = (IF b-eb.procat BEGINS "BUY"  THEN "B"
              ELSE
              IF b-eb.procat EQ "FLBEL"    THEN "L"
              /*ELSE
              IF b-eb.procat BEGINS "DSPL" THEN "D"*/
              ELSE STRING(eb.est-type LE 4,"F/C")) +
             TRIM(SUBSTR(b-eb.cust-no,1,8))
   li1     = LENGTH(TRIM(op-i-no)) + 1
   li2     = 0.

  FOR EACH itemfg
      WHERE itemfg.company            EQ eb.company
        AND itemfg.i-no               BEGINS TRIM(op-i-no)
        AND SUBSTR(itemfg.i-no,li1,4) GE "0000"
        AND SUBSTR(itemfg.i-no,li1,4) LE "9999"
      NO-LOCK
      BY SUBSTR(itemfg.i-no,1,li1 + 3) DESC:
    li = INT(SUBSTR(itemfg.i-no,li1,4)) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN li = 0.
    ELSE LEAVE.
  END.

  FOR EACH oe-ordl
      WHERE oe-ordl.company            EQ eb.company
        AND oe-ordl.i-no               BEGINS TRIM(op-i-no)
        AND SUBSTR(oe-ordl.i-no,li1,4) GT STRING(li,"9999")
        AND SUBSTR(oe-ordl.i-no,li1,4) LE "9999"
      NO-LOCK
      BY SUBSTR(oe-ordl.i-no,1,li1 + 3) DESC:
    li2 = INT(SUBSTR(oe-ordl.i-no,li1,4)) NO-ERROR. 
    IF ERROR-STATUS:ERROR THEN li2 = 0.
    ELSE LEAVE.
  END.

  ASSIGN
     li = MAX(li,li2)
     li2 = 0.

  IF b-eb.form-no NE 0 THEN
  FOR EACH bb OF est
      WHERE bb.form-no  NE 0
        AND bb.blank-no NE 0
      NO-LOCK
      BY bb.form-no BY bb.blank-no:
    li2 = li2 + 1.
    IF ROWID(bb) EQ ROWID(b-eb) THEN LEAVE.
  END.

  op-i-no = op-i-no + STRING(li + 1,"9999") + "A" +
            (IF est.est-type EQ 2 OR est.est-type EQ 6 THEN
               STRING(li2,"9")
             ELSE "").
END.
