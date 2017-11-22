{ed/sharedv.i}
{rc/statline.i}
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
&ROWS       = 4
&TITLE      = "SHIP NOTICE LINES EDITOR"
&FILE       = "edshline"
&INDEX      = " "
&CONDITION  = "where edshline.partner = edshtran.partner
    and edshline.seq = edshtran.seq"
&POSIT      = " "
&DETFUNCT   = " "
&CHOOSE     = "cust-po"
&KEYEDIT    = "ws_edshline_rec = recid(edshline)."
&DISPLAYF   =
"
  EDSHLine.cust-po-line
  EDSHLine.cust-item-no skip
  EDSHLine.Qty-orig-ord     column-label 'Ordered'
  EDSHLine.qty-shipped      column-label 'Shipped'
  EDSHLine.Uom-code         column-label 'UOM'
  EDSHLine.unit-price       column-label 'Price'
  EDSHLine.pack-size        column-label 'Packing'
  EDSHLine.by-code          column-label 'BY-Code'
"
&DATAEDIT   = " "
&TERMKEY    = " "
&UPFLDS     =
"
  EDSHLine.st-code          format 'x(08)'
  EDSHLine.Order-no         format 'x(08)'
  EDSHLine.BOL-No           format 'x(15)'
  EDSHLine.Invoice-no       format 'x(08)'  label 'Invoice'
  EDSHLine.Cust-po          format 'x(15)'
  EDSHLine.Cust-po-date     label 'PO Date'
  EDSHLine.case-wght
  EDSHLine.tot-cartons
  EDSHLine.tot-wght
  EDSHLine.tot-volume skip
  EDSHLine.Carton-mark
  EDSHLine.Pallet-mark
"
&HELPKEY    = " "
&DETEDIT    = " "
&ADDCODE    = " "
&ADDPOST    =
"
 update
  EDSHLine.Item-no
  EDSHLine.UPC
  EDSHLine.Ship-Stat
  EDSHLine.color-desc
  EDSHLine.case-dim[1]
  EDSHLine.case-dim[2]
  EDSHLine.case-dim[3]
 with frame f-phys.
"
&delcode    = "run ed/shdellin.ip (recid(edshline))."
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
procedure ed/shdellin.ip:
def input param p_rec as recid no-undo.
    
    find edshline
    where recid(edshline) = p_rec exclusive-lock no-error.
    if not avail edshline then return error.
    
    find edshtran
    where edshtran.partner = edshline.partner
    and edshtran.seq = edshline.seq exclusive-lock no-error.
    
    find first edshord 
    where edshord.partner = edshline.partner
    and edshord.seq = edshline.seq
    and edshord.cust-po = edshline.cust-po
    and edshord.order-no = edshline.order-no 
    and edshord.bol-no = edshline.bol-no
    exclusive-lock no-error.
    
    find first edshtare 
    where edshtare.partner = edshline.partner
    and edshtare.seq = edshline.seq
    and edshtare.pallet-mark = edshline.pallet-mark
    and edshtare.bol-no = edshline.bol-no
    exclusive-lock no-error.
    
    find first edshpack
    where edshpack.partner = edshline.partner
    and edshpack.seq = edshline.seq
    and edshpack.pallet-mark = edshline.pallet-mark
    and edshpack.carton-mark = edshline.carton-mark 
    and edshpack.bol-no = edshline.bol-no
    exclusive-lock no-error.
    if avail edshtran then assign
        edshtran.tot-cartons    = edshtran.tot-cartons - edshline.tot-cartons
        edshtran.tot-volume     = edshtran.tot-volume - edshline.tot-volume
        edshtran.tot-wght       = edshtran.tot-wght - edshline.tot-wght
        edshtran.lines      = edshtran.lines - 1.
    if avail edshord then assign
        edshord.tot-cartons     = edshord.tot-cartons - edshline.tot-cartons
        edshord.tot-volume      = edshord.tot-volume - edshline.tot-volume
        edshord.tot-wght        = edshord.tot-wght - edshline.tot-wght
        edshord.lines       = edshord.lines - 1.
    if avail edshtare then assign
        edshtare.tot-cartons    = edshtare.tot-cartons - edshline.tot-cartons
        edshtare.tot-volume     = edshtare.tot-volume - edshline.tot-volume
        edshtare.tot-wght       = edshtare.tot-wght - edshline.tot-wght.
    if avail edshpack then assign
        edshpack.tot-cartons    = edshpack.tot-cartons - edshline.tot-cartons
        edshpack.tot-volume     = edshpack.tot-volume - edshline.tot-volume
        edshpack.tot-wght       = edshpack.tot-wght - edshline.tot-wght.
    
end procedure.
