/* lib/viewer.p
 *
 * generic data viewer setup
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
/*
  showMe = not( f:visible ).
  assign
    f:visible = showMe
    f:hidden  = not( showMe )
  .
 */
end.

/* setup
 *
 */

create frame f.
assign
  f:row           = 4				/* 2 lines for the header, plus a blank				*/
  f:column        = 1 
  f:width-chars   = ( if "{&window-system}" <> "tty" then 160 else 255 )
  f:box           = no
  f:top-only      = false
  f:overlay       = true
  f:name          = ttName
  f:private-data  = scrTitle
no-error.

if "{&window-system}" <> "tty" then
  assign
    f:width-chars = 160
    f:scrollable  = false
  .

run dynViewerInit( f, dcName, ttName, evtName, dispOrder, minRows, maxRows ).

/* initialize
 *
 */

publish "protop_register" ( dcName ).

subscribe to "protop_" + evtName anywhere run-procedure "showMe".

return.
