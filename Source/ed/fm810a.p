{ed/sharedv.i}
{rc/statline.i}
form
with frame f-det down row 3 column 2.
form
with frame f-details side-labels.
form
with frame f-notes center title "Addon Line Notes".
find edivtran
where recid(edivtran) = ws_edivtran_rec exclusive-lock no-error.
if not avail edivtran
then do:
    bell.
    message color value(c_err) "Could not find edivtran".
    pause 2.
    return.
end.
{rc/scrvars.i}
{rc/scrfm3.i
&FUNCTIONS  = "NYYY"
&init = "f-details-title = 'Press f5 for Notes'."
&ROWS       = 5
&TITLE      = "INVOICE ADDON EDITOR"
&FILE       = "edivaddon"
&INDEX      = " "
&CONDITION  = "of edivtran"
&POSIT      = "where true "
&DETFUNCT   = " "
&CHOOSE     = "line"
&KEYEDIT    = "ws_edivaddon_rec = recid(edivaddon)."
&DISPLAYF   =
"
  EDIVAddon.Addon-line column-label 'Seq#'
  EDIVAddon.Allow-charge column-label 'A!C'
  EDIVAddon.Hand-meth    column-label 'Hand!Meth'
  EDIVAddon.Special-svc-code   column-label 'Addon!Code'
  EDIVAddon.Qty
  EDIVAddon.Uom-code     column-label 'UOM'
  EDIVAddon.Rate
  EDIVAddon.Percent
  EDIVAddon.Amount
"
&DATAEDIT   = " "
&TERMKEY    = " "
&UPFLDS     =
"
  EDIVAddon.Description[1]
  EDIVAddon.Description[2]
  EDIVAddon.Agency-qual
  EDIVAddon.Agency-code
  EDIVAddon.Option-code
  EDIVAddon.Ref-Num
  EDIVAddon.Basis-qual
"
&HELPKEY    = " "
&DETEDIT    =
"
  if keylabel(lastkey) = 'f5' then do:
  update text(edivAddon.Note) with frame f-notes center overlay no-labels.
  end.
"
&ADDCODE    = " "
&ADDPOST    = " "
}
/*
&DETGO      = " "
&DATAGO     = " "
&HASHDISP   = " "
&HASHPLUS   = " "
&HASHMINUS  = " "
*/
