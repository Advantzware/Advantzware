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
&TITLE      = "SHIP NOTICE ORDER EDITOR"
&FILE       = "edshord"
&INDEX      = " "
&CONDITION  = "where edshord.partner = edshtran.partner
    and edshord.seq = edshtran.seq"
&POSIT      = " "
&DETFUNCT   = " "
&CHOOSE     = "cust-po"
&KEYEDIT    = "ws_edshord_rec = recid(edshord)."
&DISPLAYF   =
"
  EDSHOrd.Cust-po
  EDSHOrd.By-code
  EDSHOrd.St-code
  EDSHOrd.Order-no  skip
  EDSHOrd.Cust-div  at 30
  EDSHOrd.Cust-dept
"
&DATAEDIT   = " "
&TERMKEY    = " "
&UPFLDS     =
"
  EDSHOrd.Cust-po-date
  EDSHOrd.BOL-No
  EDSHOrd.Invoice-no
  EDSHOrd.Invoice-date
  EDSHOrd.Ship-Stat
  EDSHOrd.Lines
  EDSHOrd.Last-line
  EDSHOrd.Tot-wght
  EDSHOrd.Tot-cartons
  EDSHOrd.Tot-volume
  EDSHOrd.Package-Code
"
&HELPKEY    = " "
&DETEDIT    = " "
&ADDCODE    = " "
&ADDPOST    =
"
 update
 with frame f-phys.
"
&delcode    = "for each edshline
 where edshline.partner = edshord.partner
 and edshline.seq = edshord.seq
 and edshline.cust-po = edshord.cust-po
 and edshline.order-no = edshord.order-no 
 and edshline.bol-no = edshord.bol-no no-lock:
    run ed/shdellin.ip (recid(edshline) ).
 end."
&HASHDISP   = " "
&HASHPLUS   = " "
&HASHMINUS  = " "
}
/*
&DETGO      = " "
&DATAGO     = " "
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
    
    delete edshline.
    
    
end procedure.
