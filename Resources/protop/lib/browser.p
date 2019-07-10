/* lib/browser.p
 *
 * generic data browser setup
 *
 * protop.p runs this to initialize UI
 *
 */

{lib/dynscreen.i}

define input parameter dispOrder as integer   no-undo.
define input parameter evtName   as character no-undo.
define input parameter scrTitle  as character no-undo.
define input parameter dcName    as character no-undo.
define input parameter ttName    as character no-undo.
define input parameter minRows   as integer   no-undo.
define input parameter maxRows   as integer   no-undo.

define variable f as handle no-undo.

define variable showMe as logical no-undo initial no.

procedure showMe:

  publish "protop_display" ( f:name, showMe ).

end.

/* setup
 *
 */

/* if changes are made to formatting in dc/latch.p, dc/resrc.p, or dc/txe.p the column offsets below may need to change */

create frame f.
assign
  f:row          = 3					/* the actual rendering will (probably) modify this			*/
  f:column       = ( if evtName = "txe" then 104 else ( if evtName = "resources" then /* 47 */ 57 else 1 )) 
  f:height-chars = 1	/* 100 */
  f:box          = no
  f:top-only     = false
  f:overlay      = true
  f:name         = ttName
  f:private-data = scrTitle
no-error.

if "{&window-system}" <> "tty" then
  assign
    f:height-chars = 1
    f:scrollable   = false
  .

on row-display of f anywhere do:
  rowDisplay( self ).
  return.
end.

/* there are 4 lines of "decoration" -- so add 4 to the number of data rows     			*/
/* but apparently you need yet another if there is any hope of the title also being displayed...	*/

run dynBrowserInit( f, dcName, ttName, evtName, dispOrder, minRows, ( if maxRows <= 0 then maxRows else maxRows + 5 )).

/* initialize
 *
 */

publish "protop_register" ( dcName ).

subscribe to "protop_" + evtName anywhere run-procedure "showMe".

return.
