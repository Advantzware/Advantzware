/* ---------------------------------------------- ar/rep/invbrick.i 02/00 EKW */
/* PRINT Brick Invoice                                                        */
/* -------------------------------------------------------------------------- */

page {1}.

hide {1} frame inv-bot2.
view {1} frame inv-bot1.

assign
 j           = int("{1}" eq "") + 1
 v-tot-sf[j] = 0.
 
 v-tot-disc-qty = 0.                                 /* ekw02010006 */

for each ar-invl
    where ar-invl.x-no eq ar-inv.x-no
    no-lock
    by ar-invl.line:

  assign okbill = ar-invl.billable.                    /* begin ekw02010006 */
  assign v-disc-pct = ar-inv.disc-%
                                                     /* end ekw02010006 */
  v-lines = 2.
  
  if ar-invl.i-dscr     ne "" then v-lines = v-lines + 1.
  if ar-invl.part-dscr1 ne "" then v-lines = v-lines + 1.
  if ar-invl.part-dscr2 ne "" then v-lines = v-lines + 1.

  if line-counter {2} - 1 + v-lines gt page-size {2} + 1 then page.

  find first oe-ordl
      where oe-ordl.company eq cocode
        and oe-ordl.ord-no  eq ar-invl.ord-no
        and oe-ordl.i-no    eq ar-invl.i-no
        and oe-ordl.line    eq ar-invl.line
      no-lock no-error.

  assign
   v-part-comp = if not avail oe-ordl              or
                   oe-ordl.ship-qty ge oe-ordl.qty then "C"
                 else "P"
   v-inv-price = ar-invl.amt / ar-invl.qty.

  if v-inv-price eq ? then v-inv-price = 0.

  v-t-price    = ar-invl.amt.

  if avail oe-ordl then
  case ar-invl.pr-qty-uom:
     when "L"  then
          v-t-price = ar-invl.unit-pr.
     when "EA" then
          v-t-price = ar-invl.inv-qty * ar-invl.unit-pr.
     when "C" then
          v-t-price = ar-invl.inv-qty * (ar-invl.unit-pr / 100).
     when "M" then
          v-t-price = ar-invl.inv-qty * (ar-invl.unit-pr / 1000).
     when "CS" then do:
          if avail oe-ordl and oe-ordl.cas-cnt > 0 then
             v-t-price  = ar-invl.inv-qty * (ar-invl.unit-pr / oe-ordl.cas-cnt). 
          else
             v-t-price    = ar-invl.inv-qty * ar-invl.unit-pr.
     end. /* when "CS" then do: */
  end case.

  display {1}
          ar-invl.inv-qty @ oe-ordl.qty
            ar-invl.qty when ar-invl.inv-qty eq 0 @ oe-ordl.qty
          ar-invl.unit-pr @ v-inv-price
          ar-invl.pr-qty-uom
          v-t-price @ ar-invl.amt

      with frame inv-mid1.
  down {1} with frame inv-mid1.

  if avail oe-ordl then
  do i = 1 to 4:
    v-part-dscr = if i eq 1 then ar-invl.i-name
                  else
                  if i eq 2 then ar-invl.part-dscr1
                  else
                  if i eq 3 then ar-invl.part-dscr2
                  else "".
                  /*else           ar-invl.i-no.*/

    if v-part-dscr ne "" then put {1} v-part-dscr at 15. /* ekw02010006 */
  end.
  else
  do i = 1 to 3:
    v-part-dscr = if i eq 1 then ar-invl.i-name
                  else
                  if i eq 2 then ar-invl.i-dscr
                  else           ar-invl.part-dscr2.

    if v-part-dscr ne "" then put {1} v-part-dscr at 15.
  end.

  put {1} skip(1).

  find first itemfg
      where itemfg.company eq cocode
        and itemfg.i-no    eq ar-invl.i-no
      no-lock no-error.
  v-tot-sf[j] = v-tot-sf[j] +
                (if avail itemfg then (ar-invl.qty * itemfg.t-sqft) else
                 if ar-invl.amt-msf ne 0 then (ar-invl.amt-msf * 1000)
                 else (ar-invl.qty * ar-invl.sf-sht)).
   
   if avail oe-ordl then
     assign
       v-tot-disc-qty = v-tot-disc-qty + 
                        if ar-invl.disc ne 0 then
                          round(v-t-price - round((v-t-price * (1 - (ar-invl.disc / 100))),2),2)
                        else 0.
   else
     assign
       v-tot-disc-qty = v-tot-disc-qty +
           (ar-invl.amt * (ar-inv.disc-% / 100 )).

   assign v-tax-total = v-tax-total +
                        if ar-invl.tax then ar-invl.amt else 0.


end. /* for each ar-invl */
 
  assign v-nt-tot = if avail oe-ordl then
                      ar-inv.gross - v-tax-total - ar-inv.tax-amt -
                        (if ar-inv.f-bill then ar-inv.freight else 0)
                    else
                      ar-inv.gross - v-tax-total -
                      ar-inv.tax-amt - v-tot-disc-qty -
                      (if ar-inv.f-bill then ar-inv.freight else 0).
                                            
  if v-nt-tot lt 0 then v-nt-tot = 0.

  if avail oe-ordl then
    assign v-tot-inv = ar-inv.gross.
  else
    assign v-tot-inv = (ar-inv.gross - v-tot-disc-qty + ar-inv.freight ).
                          
  assign v-tot-tax = v-tot-tax + ar-inv.tax-amt          /* ekw02010006 */
            v-tot-disc = v-tot-disc-qty                  /* ekw02010006 */
            v-tot-freight = v-tot-freight + ar-inv.freight. /* ekw02010006 */ 

hide {1} frame inv-bot1.
view {1} frame inv-bot2.

/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */

