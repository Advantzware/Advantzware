/* -------------------------------------------------- sys/ref/cust.v 2/92 cd  */
/*                                                                            */
/* cust display statement                                                     */
/*                                                                            */
/* -------------------------------------------------------------------------- */

find first sman where sman.sman = cust.sman and sman.company = cocode
                                no-lock no-error.
find first terms where terms.t-code = cust.terms and terms.company = cocode
                                no-lock no-error.
                                
assign
 v-fax-area-code = substr(cust.fax,1,3)
 v-fax-phone     = substr(cust.fax,4,7).

display
  cust.cust-no
  cust.name
  cust.addr[1]
  cust.addr[2]
  cust.city
  cust.state
  cust.zip
  cust.type
  cust.active
  cust.contact
  cust.sman
  "" @ sman.sname
  sman.sname when available sman
  cust.area-code
  cust.phone
  v-fax-area-code
  v-fax-phone
  cust.auto-reprice
  cust.cust-level
  cust.an-edi-cust
  cust.stat-type
  cust.ship-days
  cust.pallet
  cust.case-bundle
  cust.stat-grp
  cust.del-zone
  cust.cr-use
  cust.carrier
  cust.cr-rating
  cust.cr-hold-invdays  /* 9508 CAH */
  cust.frt-pay
  cust.terr
  cust.cr-lim
  cust.ship-part
  cust.loc
  cust.ord-lim
  cust.fob-code
  cust.cr-hold
  cust.sort
  cust.tax-gr
  cust.fin-chg
  cust.inv-meth
  cust.tax-id
  cust.date-field[2]
  cust.terms
  "" @ terms.dscr
  terms.dscr when avail terms
  cust.date-field[1]
  cust.disc
  cust.markup
  cust.under-pct
  cust.over-pct
  cust.email
  cust.int-field[1].

IF tb_excel THEN
   PUT STREAM excel UNFORMATTED
       '"' cust.cust-no                        '",'
       '"' cust.NAME                           '",'
       '"' cust.addr[1]                        '",'
       '"' cust.addr[2]                        '",'
       '"' cust.city                           '",'
       '"' cust.state                          '",'
       '"' cust.zip                            '",'
       '"' if cust.phone ne "" then
              "(" + cust.area-code + ") "
              + STRING(cust.phone,"999-9999")
           else ""                             '",'
       '"' if v-fax-phone ne "" then
              "(" + v-fax-area-code + ") "
              + STRING(v-fax-phone,"999-9999")
           else ""                             '",'
       '"' cust.type                           '",'
       '"' cust.active                         '",'
       '"' cust.contact                        '",'
       '"' cust.sman                           '",'
       '"' if available sman then sman.sname
           else ""                             '",'
       '"' cust.auto-reprice                   '",'
       '"' cust.cust-level                     '",'
       '"' cust.an-edi-cust                    '",'
       '"' STRING(cust.stat-type,"O/F")        '",'
       '"' cust.ship-days                      '",'
       '"' cust.pallet                         '",'
       '"' cust.case-bundle                    '",'
       '"' cust.stat-grp                       '",'
       '"' cust.terr                           '",'
       '"' cust.cr-use                         '",'
       '"' cust.loc                            '",'
       '"' cust.carrier                        '",'
       '"' cust.del-zone                       '",'
       '"' cust.cr-rating                      '",'
       '"' cust.cr-hold-invdays                '",'
       '"' cust.cr-lim                         '",'
       '"' STRING(cust.ship-part,"YES/NO")     '",'
       '"' cust.frt-pay                        '",'
       '"' cust.ord-lim                        '",'
       '"' STRING(cust.sort,"X")               '",'
       '"' cust.fob-code                       '",'
       '"' cust.cr-hold                        '",'
       '"' cust.tax-gr                         '",'
       '"' cust.fin-chg                        '",'
       '"' STRING(cust.inv-meth,"YES/NO")      '",'
       '"' cust.tax-id                         '",'
       '"' cust.date-field[2]                  '",'
       '"' cust.terms                          '",'
       '"' if avail terms then terms.dscr
           else ""                             '",'
       '"' cust.date-field[1]                  '",'
       '"' cust.disc                           '",'
       '"' cust.markup                         '",'
       '"' cust.under-pct                      '",'
       '"' cust.over-pct                       '",'
       '"' cust.email                          '",'
       '"' cust.int-field[1]                   '",'
       skip.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
