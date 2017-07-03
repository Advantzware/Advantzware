DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

{sys/inc/var.i NEW SHARED}

DEF NEW SHARED VAR factor# AS DEC NO-UNDO.
DEF NEW SHARED VAR v-default-gl-log AS LOG NO-UNDO.
DEF NEW SHARED VAR v-default-gl-cha AS cha NO-UNDO.
DEF NEW SHARED VAR v-po-qty AS LOG INIT TRUE NO-UNDO.
DEF NEW SHARED VAR v-po-msf LIKE sys-ctrl.int-fld NO-UNDO.

DEF VAR v-basis-w AS DEC NO-UNDO. /* for po/po-adder2.p */
DEF VAR v-len LIKE po-ordl.s-len NO-UNDO.
DEF VAR v-wid LIKE po-ordl.s-wid NO-UNDO.
DEF VAR v-dep LIKE po-ordl.s-len NO-UNDO.
DEF VAR fg-uom-list AS CHAR NO-UNDO.
DEF VAR v-ord-qty AS DEC NO-UNDO.
DEF VAR v-tot-msf AS DEC NO-UNDO.
/* ip-type needed for include file podisdet2 */
DEFINE VARIABLE ip-type AS CHARACTER NO-UNDO.

RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

FIND po-ordl WHERE ROWID(po-ordl) EQ ip-rowid NO-ERROR.

IF AVAIL po-ordl THEN
FIND FIRST po-ord
    WHERE po-ord.company EQ po-ordl.company
      AND po-ord.po-no   EQ po-ordl.po-no
    NO-ERROR.

IF AVAIL po-ord THEN DO:
  ASSIGN
   cocode = po-ord.company
   locode = po-ord.loc.
      
  RUN po/po-sysct.p.

  {po/podisdet2.i "DEC"}
END.

