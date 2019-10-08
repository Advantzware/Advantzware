/* 9712 CAH: Added ZZ code for Mark-for party */
{ed/sharedv.i}
{rc/stripvar.i new}
{rc/statline.i}
{rc/viewline.i &displayf="ws_partner"}
if not called then do:
    {ed/getpart.i}
end.
else do:    /* 9810 CAH: this allows scroll to position on current seq */
    find eddoc where recid(eddoc) = lib_recid_ret no-lock no-error.
    find edpotran of eddoc no-lock no-error.
    lib_recid_ret = if avail edpotran then recid(edpotran) else ?.
    release edpotran.
end.
form
with frame f-det down row 3 column 2.
form
with frame f-details side-labels.
{rc/scrvars.i}
{rc/scrfm3.i
&FUNCTIONS  = "NYYY"
&ROWS       = 5
&TITLE      = "PURCHASE ORDER EDITOR"
&FILE       = "edpotran"
&INDEX      = " "
&CONDITION  = "where edpotran.partner = ws_partner"
&POSIT      = " "
&DETFUNCT   = " "
&CHOOSE     = "seq"
&KEYEDIT    = "ws_edpotran_rec = recid(edpotran)."
&DISPLAYF   =
"edpotran.cust-po edpotran.order-date edpotran.by-code edpotran.st-code"
&DATAEDIT   = " "
&TERMKEY    = " "
&UPFLDS     =
  "
  EDPOTran.Cust
  EDPOTran.cust-div
  EDPOTran.cust-dept
  EDPOTran.ship-date-code
  EDPOTran.Request-date
  EDPOTran.cancel-date-code
  EDPOTran.Cancel-date
  EDPOTran.purpose-code
  EDPOTran.Order-type
  EDPOTran.Catalog
  EDPOTran.sf-code
  EDPOTran.bo-flag
  EDPOTran.ship-stat
  EDPOTran.contract
  EDPOTran.release-no
  EDPOTran.promo-code
  EDPOTran.package-code
  EDPOTran.order-amount
"
&HELPKEY    = " "
&DETEDIT    = " "
&ADDCODE    = " "
&ADDPOST    =
"repeat:
 strip-list = 'Lines,Addons,Shipping,Terms,Misc,Exit'.
 run rc/strip.p.
 if strip-f4 then leave.
 if strip-sel[1] begins 'Lines'     then run ed/fm850l.p. else
 if strip-sel[1] begins 'Addons'    then run ed/fm850a.p.
 else if strip-sel[1] begins 'Shipping' then do:
    update
  EDPOTran.Ship-name
  EDPOTran.Ship-address[1]
  EDPOTran.Ship-address[2]
  EDPOTran.Ship-address[3]
  EDPOTran.Ship-city
  EDPOTran.Ship-st
  EDPOTran.Ship-country
  EDPOTran.Ship-zip
  EDPOTran.routing[1]
  EDPOTran.routing[2]
  EDPOTran.routing[3]
  EDPOTran.routing[4]
  EDPOTran.ship-method-code
  EDPOTran.ship-pay-code
  EDPOTran.ship-loc-code
    with frame f-shipto overlay 1 column.
 end.
 else if strip-sel[1] = 'Terms' then do:
    update
  EDPOTran.terms
  EDPOTran.terms-desc[1]
  EDPOTran.terms-desc[2]
  EDPOTran.curr-buyer
  EDPOTran.curr-rate-buyer
  EDPOTran.curr-seller
  EDPOTran.curr-rate-seller
    with frame f-currency overlay 1 column.
 end.
 else if strip-sel[1] = 'Misc' then do:
    update
  EDPOTran.misc-date1-code
  EDPOTran.misc-date1
  EDPOTran.scheduled-code1
  EDPOTran.scheduled-code2
  EDPOTran.scheduled-code3
  EDPOTran.special-svc-code
  EDPOTran.ref2-code
  EDPOTran.ref2
  EDPOTran.ref3-code
  EDPOTran.ref3
  EDPOTran.tin
  EDPOTran.tin-code
  EDPOTran.tin-loc
  EDPOTran.bt-code
  EDPOTran.re-code
  EDPOTran.sf-code
  EDPOTran.sn-code
  EDPOTran.vn-code
  /* 9809 CAH edpotran.zz-code */
    with frame f-misc side-labels overlay 1 column.
 end.
 else if strip-sel[1] begins 'exit' then leave.
 end. /* menu repeat */
 "
&delcode    = "for each edpoline of edpotran: delete edpoline. end.
    for each edpoaddon of edpotran: delete edpoaddon. end."
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
