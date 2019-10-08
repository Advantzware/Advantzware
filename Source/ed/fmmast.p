/***************************************************************************\
*****************************************************************************
**  Program: D:\RPRODEV\ED\FMMAST.P
**       By: Chris Heins
** Descript: Trading Partner Setup
**
05.28.97 by CAH on ricky@812<rprodev> Log#0000:
1.  Added Print option on menu strip, calls ed/rpmast.p.
05.08.97 by CAH on ricky@812<rprodev> Log#0000:
1.  Added re-code.
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
{rc/statline.i}
{rc/viewline.i &displayf="ws_partner"}
{rc/stripvar.i new}
{rc/callvar.i}
DEF VAR save-list LIKE strip-list NO-UNDO.
def var hi_partner like ws_partner no-undo.
save-list = "Sets,Locations,Items,Copy partner,Print,Exit".
VIEW FRAME f-view.
FORM
  WITH FRAME f-det ROW 3 column 2.
FORM
  edmast.sf-code colon 15 edmast.del-days  colon 53
  edmast.order-no-mask colon 15  edmast.ship-to-mask colon 53
  edmast.asn-on-ds colon 15
  edmast.id-trim COLON 15 edmast.id-len edmast.id-out
  edmast.item-length colon 15 edmast.item-prefix edmast.item-suffix
  edmast.path-in COLON 10
  edmast.path-out COLON 10
  WITH FRAME f-details CENTER ROW 12 SIDE-LABELS.
{rc/scrvars.i}
{rc/scrfm3.i
  &FUNCTIONS  = "YYYY"
  &ROWS       = 5
  &TITLE      = " Trading Partner Setup "
  &FILE       = "EDMast"
  &INDEX      = " "
  &CONDITION  = "WHERE TRUE"
  &POSIT      = " "
  &DETFUNCT   = " "
  &CHOOSE     = "Partner"
  &KEYEDIT    = "assign ws_partner = edmast.partner.
    display ws_partner with frame f-view.
    "
  &DISPLAYF   =
  "edmast.cust
   edmast.we-vend-no
   edmast.re-code format 'x(10)'
   edmast.vendor
   edmast.we-cust
   edmast.seq
    "
  &DATAEDIT   = " "
  &TERMKEY    = " "
  &UPFLDS     =
  "edmast.sf-code edmast.del-days
   edmast.order-no-mask edmast.ship-to-mask
   edmast.asn-on-ds
   edmast.id-trim edmast.id-len edmast.id-out
   edmast.item-length edmast.item-prefix edmast.item-suffix
   edmast.path-in
   edmast.path-out"
  &HELPKEY    = " "
  &DETEDIT    = " "
  &ADDCODE    = " "
  &ADDPOST    =
  "
_menu: do while true:
strip-list = save-list.
run rc/strip.p.
if strip-f4 or strip-sel[1] = 'exit' then leave.
else do:
hide frame f-details no-pause.
hide frame f-det no-pause.
called = true.
caller = program-name(1).
if strip-sel[1] begins 'set'    then run ed/fmcode.p.   ELSE
IF strip-sel[1] begins 'loc'    then run ed/fmxship.p.  else
if strip-sel[1] begins 'item'   then run ed/fmxitem.p.  else
if strip-sel[1] begins 'copy'   then do:
    message 'Copy to partner-id' update hi_partner.
    hide message no-pause.
    status default 'Wait, copying ...'.
    run ed/fmmastcp.p (ws_partner, hi_partner).
    status default.
end. else
if strip-sel[1] begins 'print'  then run ed/rpmast.p.
called = false.
caller = ''.
view frame f-det.
view frame f-details.
end.
end.
"
&delcode =
"for each edshipto of edmast: delete edshipto.  end.
 for each edcode   of edmast: delete edcode.    end.
 for each edicxref of edmast: delete edicxref.  end.
 for each eddoc    where eddoc.partner =  edmast.partner:
    next_program = 'ed/fm' + eddoc.setid + 'del.p'.
    run value(next_program) (recid(eddoc)).
    delete eddoc.
 end.
"
  }
/*
&DATAGO     = " "
&DETGO      = " "
&HASHDISP   = " "
&HASHPLUS   = " "
&HASHMINUS  = " "
*/
HIDE FRAME f-det NO-PAUSE.
HIDE FRAME f-details NO-PAUSE.
