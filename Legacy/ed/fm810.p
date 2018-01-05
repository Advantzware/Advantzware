{ed/sharedv.i}
{rc/stripvar.i new}
{rc/statline.i}
{rc/viewline.i &displayf="ws_partner"}
if not called then do:
    {ed/getpart.i}
end.
else do:    /* 9810 CAH: this allows scroll to position on current seq */
    find eddoc where recid(eddoc) = lib_recid_ret no-lock no-error.
    find edivtran of eddoc no-lock no-error.
    lib_recid_ret = if avail edivtran then recid(edivtran) else ?.
    release edivtran.
end.
form
 edivtran.seq
 edivtran.cust-po format 'x(12)'
 EDIVTran.Invoice-no format 'x(10)'
 edivtran.invoice-date column-label 'Inv-Date'
 edivtran.by-code column-label 'BY'
 edivtran.st-code column-label 'ST'
with frame f-det down row 3 column 2.
form
with frame f-details side-labels.
{rc/scrvars.i}
{rc/scrfm3.i
&FUNCTIONS  = "NYYY"
&ROWS       = 5
&TITLE      = "INVOICE EDITOR"
&FILE       = "edivtran"
&INDEX      = " "
&CONDITION  = "where edivtran.partner = ws_partner"
&POSIT      = " "
&DETFUNCT   = " "
&CHOOSE     = "seq"
&KEYEDIT    = "ws_edivtran_rec = recid(edivtran)."
&DISPLAYF   =
"edivtran.cust-po
 EDIVTran.Invoice-no
 edivtran.invoice-date
 edivtran.by-code
 edivtran.st-code "
&DATAEDIT   = " "
&TERMKEY    = " "
&UPFLDS     = "
  EDIVTran.Cust
  EDIVTran.Cust-po-date
  EDIVTran.Cust-dept
  EDIVTran.Cust-div
  EDIVTran.ship-stat
  EDIVTran.Lines
  EDIVTran.Last-line
  EDIVTran.Contract
  EDIVTran.Release-no
  EDIVTran.Promo-code
  EDIVTran.Special-svc-code
  EDIVTran.Tot-Gross
  EDIVTran.Tot-disc
  EDIVTran.Tot-net
  EDIVTran.Tot-frt
  EDIVTran.Tot-wght
  EDIVTran.Tot-cartons
  EDIVTran.Carton-uom-code
  EDIVTran.Tot-volume
  EDIVTran.Volume-uom
  EDIVTran.Tot-qty
  EDIVTran.Wght-uom "
&HELPKEY    = " "
&DETEDIT    = " "
&ADDCODE    = " "
&ADDPOST    =
"repeat:
 strip-list = 'Lines,Addons,Shipping,Terms,Misc,Exit'.
 run rc/strip.p.
 if strip-f4 then leave.
 if strip-sel[1] begins 'Lines'     then run ed/fm810l.p. else
 if strip-sel[1] begins 'Addons'    then run ed/fm810a.p.
 else if strip-sel[1] begins 'Shipping' then do:
    update
  EDIVTran.Ship-date-code
  EDIVTran.Ship-Date
  EDIVTran.BOL-No
  EDIVTran.Carrier
  EDIVTran.Carrier-code
  EDIVTran.Pro-Number
  EDIVTran.Trailer-Number
  EDIVTran.Contact-funct-code
  EDIVTran.Contact-name
  EDIVTran.Contact-phone-qual
  EDIVTran.Contact-phone
  EDIVTran.Del-date-qual
  EDIVTran.Del-date
  EDIVTran.Routing[1]
  EDIVTran.Routing[2]
  EDIVTran.Routing[3]
  EDIVTran.Routing[4]
    with frame f-shipto overlay 1 column.
 end.
 else if strip-sel[1] = 'Terms' then do:
    update
  EDIVTran.Terms
  EDIVTran.Terms-desc[1]
  EDIVTran.Terms-desc[2]
  EDIVTran.Terms-type
  EDIVTran.Terms-basis
  EDIVTran.Terms-disc-pct
  EDIVTran.Terms-disc-date
  EDIVTran.Terms-disc-days
  EDIVTran.Terms-net-date
  EDIVTran.Terms-net-days
  EDIVTran.Terms-disc-amt
  EDIVTran.Terms-day-of-month
  EDIVTran.FOB-Code
  EDIVTran.FOB-Qual
  EDIVTran.FOB-Text
  EDIVTran.Curr-buyer
  EDIVTran.Curr-rate-buyer
  EDIVTran.Curr-seller
  EDIVTran.Curr-rate-seller
    with frame f-currency overlay 1 column width 85.
 end.
 else if strip-sel[1] = 'Misc' then do:
    update
  EDIVTran.Company
  EDIVTran.st-code
  EDIVTran.Bt-code
  EDIVTran.Sf-code
  EDIVTran.Sn-code
  EDIVTran.Vendor
  EDIVTran.Vn-code
  EDIVTran.Re-code
  EDIVTran.Misc-date1-code
  EDIVTran.Misc-date1
  EDIVTran.Ref2-code
  EDIVTran.Ref2
  EDIVTran.Ref3-code
  EDIVTran.Ref3
    with frame f-misc side-labels overlay 1 column.
 end.
 else if strip-sel[1] begins 'exit' then leave.
 end. /* menu repeat */
 "
&delcode    = "for each edpoline of edivtran: delete edpoline. end.
    for each edpoaddon of edivtran: delete edpoaddon. end."
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
