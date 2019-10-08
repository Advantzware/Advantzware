
DEF INPUT  PARAM ip-company LIKE eb.company NO-UNDO.
DEF INPUT  PARAM ip-rowid   AS   ROWID      NO-UNDO.
DEF OUTPUT PARAM op-pct     AS   DEC        NO-UNDO.

DEF VAR cocode AS CHAR NO-UNDO.


cocode = ip-company.

{sys/inc/ceprice.i}

FIND eb WHERE ROWID(eb) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL eb THEN
FIND FIRST ce-ctrl
    WHERE ce-ctrl.company EQ eb.company
      AND ce-ctrl.loc     EQ eb.loc
    NO-LOCK NO-ERROR.

IF NOT AVAIL ce-ctrl THEN
FIND FIRST ce-ctrl
    WHERE ce-ctrl.company    EQ cocode
      AND ce-ctrl.prof-mrkup NE 0
    NO-LOCK NO-ERROR.
  
IF AVAIL ce-ctrl THEN op-pct = ce-ctrl.prof-mrkup.

IF AVAIL eb THEN
FIND FIRST cust NO-LOCK
    WHERE cust.company EQ eb.company
      AND cust.cust-no EQ eb.cust-no
      NO-ERROR.
IF AVAIL cust AND cust.markup NE 0 THEN op-pct = cust.markup.

IF AVAIL eb AND eb.est-type LE 4 THEN op-pct = op-pct + v-markup.

