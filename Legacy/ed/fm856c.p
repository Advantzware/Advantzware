{ed/sharedv.i}
{rc/statline.i}
{rc/stripvar.i new}
form
with frame f-det down row 3 column 2.
form
with frame f-details side-labels row 16 column 1.
form
with frame f-phys overlay center title "Physical Specifications"
    color value(c_det) side-labels.
find edshtran
where recid(edshtran) = ws_edshtran_rec exclusive-lock no-error.
if not avail edshtran
then do:
    bell.
    message color value(c_err) "Could not find edshtran".
    pause 2.
    return.
end.
{rc/scrvars.i}
{rc/scrfm3.i
&FUNCTIONS  = "NYYY"
&ROWS       = 6
&TITLE      = "SHIP NOTICE CARTON EDITOR"
&FILE       = "edshpack"
&INDEX      = " "
&CONDITION  = "where edshpack.partner = edshtran.partner
    and edshpack.seq = edshtran.seq"
&POSIT      = " "
&DETFUNCT   = " "
&CHOOSE     = "carton-mark"
&KEYEDIT    = "ws_edshpack_rec = recid(edshpack)."
&DISPLAYF   =
"
  edshpack.Tot-wght
  edshpack.Tot-cartons
  edshpack.Tot-volume
  edshpack.Package-Code
  pallet-mark
  edshpack.Cust-po
  edshpack.BOL-No
"
&DATAEDIT   = " "
&TERMKEY    = " "
&UPFLDS     =
"
"
&HELPKEY    = " "
&DETEDIT    = " "
&ADDCODE    = " "
&ADDPOST    =
"
 update
 with frame f-phys.
"
&delcode    = " "
}
/*
&DETGO      = " "
&DATAGO     = " "
&HASHDISP   = " "
&HASHPLUS   = " "
&HASHMINUS  = " "
*/
hide frame f-phys no-pause.
hide frame f-details no-pause.
hide frame f-det no-pause.
