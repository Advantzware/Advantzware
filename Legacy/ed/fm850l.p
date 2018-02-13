{ed/sharedv.i}
{rc/statline.i}
form
with frame f-det down row 3 column 2.
form
with frame f-details side-labels row 12.
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
&ROWS       = 5
&TITLE      = "PURCHASE ORDER LINES EDITOR"
&FILE       = "edpoline"
&INDEX      = " "
&CONDITION  = "of edpotran"
&POSIT      = "where true "
&DETFUNCT   = " "
&CHOOSE     = "line"
&KEYEDIT    = "ws_edpoline_rec = recid(edpoline)."
&DISPLAYF   =
"
  EDPOLine.cust-po-line format 'x(03)'
  EDPOLine.cust-item-no format 'x(15)'
  EDPOLine.Item-no
  EDPOLine.Qty-orig-ord column-label 'Qty Ord'
  EDPOLine.Uom-code
  EDPOLine.unit-price   column-label 'Unit Price'
"
&DATAEDIT   = " "
&TERMKEY    = " "
&UPFLDS     =
"
  EDPOLine.Description[1]
  EDPOLine.Description[2]
  EDPOLine.by-code
  EDPOLine.st-code
  EDPOLine.sf-code
  EDPOLine.UPC
  EDPOLine.price-basis
  EDPOLine.pack-size
  EDPOLine.special-svc-code
  EDPOLine.taxable
  EDPOLine.bo-flag
  EDPOLine.qty-change
  EDPOLine.selling-price
  /*
  EDPOLine.product-type
  EDPOLine.color-desc
  EDPOLine.size-desc
  EDPOLine.size-qual[1]
  EDPOLine.size-qual[2]
  EDPOLine.size-qual[3]
  EDPOLine.dimension[1]
  EDPOLine.dimension[2]
  EDPOLine.dimension[3]
  */
"
&HELPKEY    = " "
&DETEDIT    = " "
&ADDCODE    = " "
&ADDPOST    = " "
&delcode    = "for each edpoaddon of edpoline: delete edpoaddon. end."
}
/*
&DETGO      = " "
&DATAGO     = " "
&HASHDISP   = " "
&HASHPLUS   = " "
&HASHMINUS  = " "
*/
hide frame f-details no-pause.
hide frame f-det no-pause.
