
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT-OUTPUT PARAM io-sell-by AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM io-pct AS DEC NO-UNDO.

{sys/inc/var.i SHARED}

DEF SHARED VAR qty AS INT NO-UNDO.

DEF VAR ld-yld AS DEC NO-UNDO.
DEF VAR ld-sqf AS DEC NO-UNDO.
DEF VAR li AS INT NO-UNDO.


{cec/msfcalc.i}

FIND eb WHERE ROWID(eb) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL eb THEN
FOR EACH cust-markup NO-LOCK
    WHERE cust-markup.company EQ eb.company
      AND cust-markup.cust-no EQ eb.cust-no
      AND (cust-markup.style  EQ eb.style OR
           cust-markup.style  EQ "")
      AND (cust-markup.procat EQ eb.procat OR
           cust-markup.procat EQ "")
    BREAK BY cust-markup.procat DESC
          BY cust-markup.style  DESC:

  ASSIGN
   ld-yld = IF eb.est-type LE 4 THEN
              IF eb.cust-% LT 0 THEN -1 / eb.cust-% ELSE
              IF eb.cust-% EQ 0 THEN 1              ELSE eb.cust-%
            ELSE                  
              IF eb.yld-qty LT 0 THEN -1 / eb.yld-qty ELSE
              IF eb.yld-qty EQ 0 THEN 1               ELSE eb.yld-qty
   ld-sqf = eb.t-sqin * qty * ld-yld
   ld-sqf = IF v-corr THEN (ld-sqf * .007) ELSE (ld-sqf / 144).

  DO li = 1 TO EXTENT(cust-markup.run-qty):
    IF cust-markup.run-qty[li] GE ld-sqf THEN DO:
      ASSIGN
       io-sell-by = cust-markup.markup-on[li]
       io-pct     = cust-markup.markup[li].
      LEAVE.
    END.
  END.

  LEAVE.
END.

IF io-sell-by EQ "" THEN io-sell-by = "N".

