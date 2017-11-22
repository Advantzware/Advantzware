{ed/sharedv.i}
{rc/statline.i}
form
  edivline.line
  edivLine.cust-item-no format 'x(15)'
  edivLine.Item-no      format 'x(15)'
  EDIVLine.Qty-shipped
  edivLine.Uom-code     column-label "UOM"
  edivLine.unit-price
with frame f-det down row 3 column 2.
form
with frame f-details side-labels.
form
with frame f-phys overlay center title "Physical Specifications"
    color value(c_det) side-labels.
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
&ROWS       = 5
&TITLE      = "INVOICE LINES EDITOR"
&FILE       = "edivline"
&INDEX      = " "
&CONDITION  = "of edivtran"
&POSIT      = "where true "
&DETFUNCT   = " "
&CHOOSE     = "line"
&KEYEDIT    = "ws_edivline_rec = recid(edivline)."
&DISPLAYF   =
"
  edivLine.cust-item-no
  edivLine.Item-no
  EDIVLine.Qty-shipped
  edivLine.Uom-code
  edivLine.unit-price
"
&DATAEDIT   = " "
&TERMKEY    = " "
&UPFLDS     =
"
  EDIVLine.Company
  EDIVLine.sf-code
  EDIVLine.By-code
  EDIVLine.Description[1]
  EDIVLine.Description[2]
  EDIVLine.UPC
  EDIVLine.Cust-po-line
  EDIVLine.Price-basis
  EDIVLine.Pack-size
  EDIVLine.Product-type
  EDIVLine.Special-svc-code
  EDIVLine.Taxable
  EDIVLine.Bo-flag
  EDIVLine.Selling-price
  EDIVLine.Qty-ord-orig
  EDIVLine.Qty-var
  EDIVLine.Config-code
"
&HELPKEY    = " "
&DETEDIT    = " "
&ADDCODE    = " "
&ADDPOST    =
"
 update
  EDIVLine.Color-desc
  EDIVLine.Size-desc
  EDIVLine.Size-qual[1]
  EDIVLine.Size-qual[2]
  EDIVLine.Size-qual[3]
  EDIVLine.Dimension[1]
  EDIVLine.Dimension[2]
  EDIVLine.Dimension[3]
  EDIVLine.Item-gross
  EDIVLine.Item-disc-amount
  EDIVLine.Item-net
  EDIVLine.Item-wght-each
  EDIVLine.Item-each-cube
  EDIVLine.Item-ctn-wght
  EDIVLine.Item-ctn-cube
  EDIVLine.ship-stat
 with frame f-phys.
"
&delcode    = "for each edivaddon of edivline: delete edivaddon. end."
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
