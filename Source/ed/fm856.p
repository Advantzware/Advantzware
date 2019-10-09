{ed/sharedv.i}
{rc/stripvar.i new}
{rc/statline.i}
{rc/viewline.i &displayf="ws_partner"}
if not called then do:
    {ed/getpart.i}
end.
else do:    /* 9810 CAH: this allows scroll to position on current seq */
    find eddoc where recid(eddoc) = lib_recid_ret no-lock no-error.
    find edshtran of eddoc no-lock no-error.
    lib_recid_ret = if avail edshtran then recid(edshtran) else ?.
    release edshtran.
end.
form
  EDSHTran.seq
with frame f-det down row 3 column 2.
form
with frame f-details side-labels.
{rc/scrvars.i}
{rc/scrfm3.i
&FUNCTIONS  = "NYYY"
&ROWS       = 5
&TITLE      = "SHIP NOTICE EDITOR"
&FILE       = "edshtran"
&INDEX      = " "
&CONDITION  = "where edshtran.partner = ws_partner"
&POSIT      = " "
&DETFUNCT   = " "
&CHOOSE     = "seq"
&KEYEDIT    = "ws_edshtran_rec = recid(edshtran)."
&DISPLAYF   =
"
  EDSHTran.BOL-No                               format 'x(15)'
  EDSHTran.Ship-Date    column-label 'Shipped'
  EDSHTran.st-code
  EDSHTran.tot-wght     column-label 'Weight'
  EDSHTran.tot-cartons  column-label 'Cartons'
  EDSHTran.tot-volume   column-label 'Volume'
"
&DATAEDIT   = " "
&TERMKEY    = " "
&UPFLDS     = "
  EDSHTran.carrier
  EDSHTran.carrier-code
  EDSHTran.ship-method-code
  EDSHTran.Pro-Number
  EDSHTran.ship-pay-code
  EDSHTran.Trailer-Number
  EDSHTran.Equipment-code
  EDSHTran.Equipment-initial
  EDSHTran.BOL-Adddate
  EDSHTran.BOL-Addtime
  EDSHTran.lines
"
&HELPKEY    = " "
&DETEDIT    = " "
&ADDCODE    = " "
&ADDPOST    =
"repeat:
 strip-list = 'Orders,Pallets,Cartons,Lines,Shipping,Misc,Exit'.
 run rc/strip.p.
 if strip-f4 then leave.
 if strip-sel[1] begins 'Orders'    then run ed/fm856o.p. else
 if strip-sel[1] begins 'Pallets'   then run ed/fm856p.p. else
 if strip-sel[1] begins 'Cartons'   then run ed/fm856c.p. else
 if strip-sel[1] begins 'Lines'     then run ed/fm856l.p.
 else if strip-sel[1] begins 'Shipping' then do:
    update
  EDSHTran.Ship-name
  EDSHTran.Ship-address[1]
  EDSHTran.Ship-address[2]
  EDSHTran.Ship-address[3]
  EDSHTran.Ship-city
  EDSHTran.Ship-st
  EDSHTran.Ship-country
  EDSHTran.Ship-zip
  EDSHTran.routing[1]
  EDSHTran.routing[2]
  EDSHTran.routing[3]
  EDSHTran.routing[4]
  EDSHTran.sf-code
  EDSHTran.ship-loc-code
  EDSHTran.del-date-qual
  EDSHTran.del-date
  EDSHTran.Transit-time-qual
  EDSHTran.Transit-time
  EDSHTran.Ship-time
  EDSHTran.Ship-time-zone
    with frame f-shipto overlay 1 column.
    hide frame f-shipto no-pause.
 end.
 else if strip-sel[1] begins 'Misc' then do:
    update
  EDSHTran.purpose-code
  EDSHTran.ship-date-code
  EDSHTran.sf-code
  EDSHTran.vn-code
  EDSHTran.Package-Code
  EDSHTran.Ship-Stat
    with frame f-misc side-labels overlay 1 column.
    hide frame f-misc no-pause.
 end.
 else if strip-sel[1] begins 'exit' then leave.
 end. /* menu repeat */
 "
&delcode    =
"for each edshord  of edshtran: delete edshord.  end.
 for each edshtare of edshtran: delete edshtare. end.
 for each edshpack of edshtran: delete edshpack. end.
 for each edshline of edshtran: delete edshline. end. "
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
