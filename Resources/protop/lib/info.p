/* lib/info.p
 *
 * generic meta info setup
 *
 * protop.p runs this to initialize UI
 *
 */

{lib/tt_screenlist.i}

define input parameter dispOrder as integer   no-undo.
define input parameter evtName   as character no-undo.
define input parameter scrTitle  as character no-undo.
define input parameter dcName    as character no-undo.
define input parameter ttName    as character no-undo.
define input parameter minRows   as integer   no-undo.
define input parameter maxRows   as integer   no-undo.

procedure showMe:
  return.
end.

/* setup
 *
 */

define variable dCLOB    as longchar  no-undo.
define variable bx       as handle  no-undo.

/*** +++
run getConfig( substitute( "&1.xsd", ttName ), output dCLOB ).

create temp-table bx.
bx:read-xmlschema( "longchar", dCLOB, ?, ?, ?  ).
 ***/

find tt_screenList where tt_screenList.ttName = ttName no-error.
if not available( tt_screenList ) then
  do:
    create tt_screenList.
    assign
      tt_screenList.displayOrder  = dispOrder
      tt_screenList.screenName    = scrTitle
      tt_screenList.ttName        = ttName
      tt_screenList.dcName        = dcName
      tt_screenList.evtName       = evtName
      tt_screenList.screenType    = "info"
      tt_screenList.screenVisible = no
/*    tt_screenList.bufferHandle  = bx:default-buffer-handle +++ */
    .
  end.

/* initialize
 *
 */

publish "protop_register" ( dcName ).

return.
