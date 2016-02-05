/* -------------------------------------------------- sys/ref/cust.f 2/92 cd  */
/*                                                                            */
/* CUST FORM STATEMENT                                                        */
/*                                                                            */
/* -------------------------------------------------------------------------- */

def var v-fax-area-code like cust.area-code format "(xxx)".
def var v-fax-phone     like cust.phone     format "xxx-xxxx".

form
  skip(1)
  "  Cust.#:"                       cust.cust-no
  "    Type:"               at 41   cust.type
  "Active:"                 to 67   cust.active                             skip
  "    Name:"                       cust.name
  " Contact:"               at 41   cust.contact                            skip
  " Address:"                       cust.addr[1]
  "Sales Rep:"              at 40   cust.sman       sman.sname              skip
  space(10)                         cust.addr[2]
  "  Tel. #:"               at 41   cust.area-code  format "(xxx)"
                                    cust.phone      format "xxx-xxxx"       skip
  " C/S/Zip:"                       cust.city       cust.state      cust.zip
  "  FAX  #:"               at 41   v-fax-area-code v-fax-phone             skip
  "AutoReprice:"                    cust.auto-reprice   format "Yes/No"
  "Customer Price Level:"   at 29   cust.cust-level
  "EDI:"                    to 67   cust.an-edi-cust    format "Yes/No"     skip
  " Bal Method:"                    cust.stat-type
  "Mfg/Whse Days:"          to 49   cust.ship-days
/*
  "Warehouse Days:"         to 49   cust.ship-days " Max"
*/
  "Pallet:"                 to 67   cust.pallet                             skip
  " Stmnt Freq:"                    cust.stat-grp
  "Territory:"              to 49   cust.terr
  "Case/Bundle:"            to 67   cust.case-bundle                        skip
  " Cr. Acct #:"                    cust.cr-use
  "Ord. Loc.:"              to 49   cust.loc                                skip
  " Cr. Rating:"                    cust.cr-rating
  "Hold Days/Inv:"                  cust.cr-hold-invdays
  "Carrier:"                to 49   cust.carrier
  "Del. Zone:"              to 67   cust.del-zone                           skip
  " Credit Lim:"                    cust.cr-lim
  "Partial Ship:"           to 49   cust.ship-part      format "Yes/No"
  "Fr Pay Meth:"            to 67   cust.frt-pay                            skip
  "  Order Lim: "                   cust.ord-lim
  "Taxable:"                to 49   cust.sort format "X"
  "FOB Orig/Dest:"          to 67   cust.fob-code                           skip
  " Cred. Hold:"                    cust.cr-hold
  "TAX Code:"               to 49   cust.tax-gr
  "  Fin. Chrgs:"                   cust.fin-chg                            skip
  " Invoice Per PO:"                cust.inv-meth       format "Yes/No"
  "TAX Resale ID#:"         to 41   cust.tax-id
  " Exp."                           cust.date-field[2]                      skip
  " Terms Code:"                    cust.terms      terms.dscr              
  "Date Added:"             to 67   cust.date-field[1]  format "99/99/99"   skip
  "   Discount:"                    cust.disc           format ">>9.99%"
  "MarkUp:"                 to 29   cust.markup         format "->>9.99%"
  "Underrun:"               to 49   cust.under-pct
  "  Overrun:"              to 67   cust.over-pct                           skip
  "E-Mail/Web Address:"             cust.email
  "Load Tags:"                      cust.int-field[1]   format ">>>"

  with frame cust overlay no-labels width 80 row 2 stream-io.

/* end ---------------------------------- copr. 1992  Advanced Software, Inc. */
