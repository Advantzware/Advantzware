/***************************************************************************\
*****************************************************************************
**  Program: e:\asi\patch\edi\ed\asi\reg
**       By: Chris Heins
** Descript: Regenerate 856 from oe-bolh.
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
DEF var ws_cust-no       LIKE oe-bolh.cust-no NO-UNDO.
DEF var lo_bol-date      LIKE oe-bolh.bol-date NO-UNDO.
DEF var hi_bol-date      LIKE oe-bolh.bol-date NO-UNDO.
DEF var ws_master-bol-no LIKE oe-bolh.master-bol-no NO-UNDO.
DEF NEW SHARED var v-emp-id LIKE soldto.sold-id NO-UNDO.
DEF NEW SHARED var v-emp-recid AS RECID NO-UNDO.
DEF NEW SHARED var v-mast-bol-no LIKE oe-bolh.bol-no FORMAT ">>>>>9" NO-UNDO.
DEF var answer AS logical NO-UNDO.
ASSIGN
  ws_cust-no = ?
  lo_bol-date = TODAY
  hi_bol-date = TODAY
  ws_master-bol-no = ?.
{rc/statline.i}
{rc/fcurrent.i}
{rc/stats.i}
{rc/viewline.i &displayf=
  "ws_cust-no    help 'enter customer number or ? for any'
 lo_bol-date   help 'enter starting bill of lading date' label 'From'
 hi_bol-date   help 'enter ending bill of lading date'   label 'Thru'
 ws_master-bol-no help 'enter master bol# or ? for any'
 "}
REPEAT:
  UPDATE ws_cust-no lo_bol-date hi_bol-date ws_master-bol-no
    WITH FRAME f-view.
  start = TIME.
  VIEW FRAME f-current.
  VIEW FRAME f-stats.
  STATUS DEFAULT "Wait, creating 856 ASN's ...".
  FOR EACH oe-bolh NO-LOCK
      WHERE (IF ws_cust-no = ? THEN TRUE
      ELSE oe-bolh.cust-no = ws_cust-no)
      AND oe-bolh.bol-date >= lo_bol-date
      AND oe-bolh.bol-date <= hi_bol-date
      AND (IF ws_master-bol-no = ? THEN oe-bolh.master-bol-no > 0
      ELSE oe-bolh.master-bol-no = ws_master-bol-no):
    v-emp-id = "87" + substring(trailer,1,3).
    v-mast-bol-no = oe-bolh.master-bol-no.
    DISPLAY
      oe-bolh.cust-no
      oe-bolh.bol-date
      oe-bolh.master-bol-no
      v-emp-id
      v-mast-bol-no
      WITH FRAME f-current.
    MESSAGE "Print?" UPDATE answer.
    IF NOT answer THEN
    NEXT.
    {rc/incr.i ws_recs_read}.
    {rc/incr.i ws_recs_selected}.
    {ed/asi/o856hook.i}
    {rc/statsdis.i}
  END.
  STATUS DEFAULT.
END.
STATUS DEFAULT.
