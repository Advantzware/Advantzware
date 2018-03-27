
DEF INPUT PARAM ip-company LIKE oe-bolh.company NO-UNDO.
DEF OUTPUT PARAM op-bol-no LIKE oe-bolh.bol-no NO-UNDO.

DEFINE VAR vfrt-pay AS CHAR NO-UNDO.
DEFINE VAR vfob-code AS CHAR NO-UNDO.
DEFINE VAR vfrt-list AS CHAR NO-UNDO.
DEFINE VAR vfob-list AS CHAR NO-UNDO.
DEFINE VAR rell-ctr AS INTE NO-UNDO.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ ip-company EXCLUSIVE.

DO WHILE TRUE:
  ASSIGN
   op-bol-no     = oe-ctrl.n-bol
   oe-ctrl.n-bol = op-bol-no + 1.
   
  IF oe-ctrl.n-bol GT 999999 THEN oe-ctrl.n-bol = 1.
   
  FIND FIRST oe-bolh
      WHERE {1}oe-bolh.company EQ ip-company
        AND {1}oe-bolh.bol-no  EQ op-bol-no
      USE-INDEX bol-no NO-LOCK NO-ERROR.
  IF NOT AVAIL {1}oe-bolh THEN LEAVE.
END.
