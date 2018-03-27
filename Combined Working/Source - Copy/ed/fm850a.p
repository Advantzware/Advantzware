{ed/sharedv.i}
{rc/statline.i}
form
with frame f-det down row 3 column 2.
form
with frame f-details side-labels.
form
with frame f-notes center title "Addon Line Notes".
find edpotran
where recid(edpotran) = ws_edpotran_rec exclusive-lock no-error.
if not avail edpotran
then do:
    bell.
    message color value(c_err) "Could not find edpotran".
    pause 2.
    return.
end.
{rc/scrvars.i}
{rc/scrfm3.i
&FUNCTIONS  = "NYYY"
&init = "f-details-title = 'Press f5 for Notes'."
&ROWS       = 5
&TITLE      = "PURCHASE ORDER ADDON EDITOR"
&FILE       = "edpoaddon"
&INDEX      = " "
&CONDITION  = "of edpotran"
&POSIT      = "where true "
&DETFUNCT   = " "
&CHOOSE     = "line"
&KEYEDIT    = "ws_edpoaddon_rec = recid(edpoaddon)."
&DISPLAYF   =
"
  EDPOAddon.Uom-code
  EDPOAddon.allow-charge
  EDPOAddon.Special-Svc-code
  EDPOAddon.hand-meth
  EDPOAddon.Number
  EDPOAddon.Percent
  EDPOAddon.Amount
  EDPOAddon.Qty
  EDPOAddon.order-line
"
&DATAEDIT   = " "
&TERMKEY    = " "
&UPFLDS     =
"
  EDPOAddon.Description[1]
  EDPOAddon.Description[2]
"
&HELPKEY    = " "
&DETEDIT    =
"
  if keylabel(lastkey) = 'f5' then do:
  update text(EDPOAddon.Note) with frame f-notes center overlay no-labels.
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
