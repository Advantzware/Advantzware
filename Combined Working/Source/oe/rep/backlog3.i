/* ----------------------------------------------- oe/rep/backlog3.i          */
/* -------------------------------------------------------------------------- */

def var v-stat as char no-undo.
DEF VAR ld-rec AS DEC NO-UNDO.
DEF VAR li-inv-qty LIKE oe-ordl.inv-qty NO-UNDO.
DEF VAR li-ship-qty LIKE oe-ordl.ship-qty NO-UNDO.


release oe-ordl.
release job.
release itemfg.
release w-ord2.

find first oe-ordl where recid(oe-ordl) eq tt-report.rec-id no-lock no-error.
if not avail oe-ordl then do:
  find job where recid(job) eq tt-report.rec-id no-lock no-error.
  if not avail job then
  find itemfg where recid(itemfg) eq tt-report.rec-id no-lock no-error.
end.

if avail oe-ordl then do:
  find first oe-ord of oe-ordl no-lock.
  
  if not avail oe-ord then next.

  RUN oe/ordlsqty.p (ROWID(oe-ordl), OUTPUT li-inv-qty, OUTPUT li-ship-qty).

  create w-ord2.
  assign
   w-ord2.qty-due   = oe-ordl.qty - li-ship-qty
   w-ord2.inv-qty   = li-inv-qty
   w-ord2.qty       = oe-ordl.qty
   w-ord2.cost      = (w-ord2.qty-due / 1000) * oe-ordl.cost
   w-ord2.price     = oe-ordl.price
   w-ord2.uom       = oe-ordl.pr-uom 
   w-ord2.disc      = oe-ordl.disc
   w-ord2.rel-date  = ?.

  RELEASE oe-rell.
  RELEASE oe-relh.

  for each oe-rell
      where oe-rell.company eq tt-sales-forecast.company
        and oe-rell.ord-no  eq oe-ordl.ord-no
        and oe-rell.i-no    eq oe-ordl.i-no
        and oe-rell.line    eq oe-ordl.line
        AND oe-rell.s-code  NE "T"
      use-index ord-no no-lock,

      FIRST oe-relh
      WHERE oe-relh.r-no    EQ oe-rell.r-no
        and oe-relh.posted  eq no
        and oe-relh.deleted eq no
        NO-LOCK
      by oe-relh.rel-date:
      
    leave.
  end.
  
  if avail oe-rell then
    assign
     w-ord2.rel-date = oe-relh.rel-date
     w-ord2.rel-stat = "A".
  
  else
  for each oe-rel
      where oe-rel.company eq tt-sales-forecast.company
        and oe-rel.ord-no  eq oe-ordl.ord-no
        and oe-rel.i-no    eq oe-ordl.i-no
        and oe-rel.line    eq oe-ordl.line
        AND oe-rel.s-code  EQ "T"
      no-lock:
      
    {oe/rel-stat.i v-stat}
    
    if index("ILS",v-stat) ne 0                                   and
       (oe-rel.rel-date lt w-ord2.rel-date or w-ord2.rel-date eq ?) then
      assign
       w-ord2.rel-date = oe-rel.rel-date
       w-ord2.rel-stat = v-stat.
  end.

  IF oe-ordl.po-no-po NE 0 THEN
  FOR EACH po-ordl
      WHERE po-ordl.company        EQ oe-ordl.company
        AND po-ordl.po-no          EQ oe-ordl.po-no-po
      NO-LOCK:

    IF NOT((po-ordl.item-type    EQ YES AND
            TRIM(oe-ordl.job-no) NE "" AND
            po-ordl.job-no       EQ oe-ordl.job-no AND
            po-ordl.job-no2      EQ oe-ordl.job-no2)      OR
            (po-ordl.item-type    EQ NO AND
             po-ordl.i-no         EQ oe-ordl.i-no)) THEN NEXT.

    ld-rec = po-ordl.t-rec-qty.

    IF po-ordl.item-type EQ YES THEN DO:
      FIND FIRST item
          WHERE item.company eq tt-sales-forecast.company
            AND item.i-no    eq po-ordl.i-no
          USE-INDEX i-no NO-LOCK NO-ERROR.

      IF AVAIL item AND INDEX("1234BPR",item.mat-type) GT 0 THEN DO:
        IF po-ordl.cons-uom NE "EA" THEN 
          RUN sys/ref/convquom.p (po-ordl.cons-uom, "EA",
                                item.basis-w, po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                                ld-rec, OUTPUT ld-rec).

        FOR EACH job-hdr FIELDS(sq-in)
            WHERE job-hdr.company EQ oe-ordl.company
              AND job-hdr.job-no  EQ oe-ordl.job-no
              AND job-hdr.ord-no  EQ oe-ordl.ord-no
              AND job-hdr.job-no2 EQ oe-ordl.job-no2
              AND job-hdr.i-no    EQ oe-ordl.i-no
              AND job-hdr.frm     EQ po-ordl.s-num
              AND job-hdr.sq-in   NE 0
            NO-LOCK:
          ld-rec = ld-rec * job-hdr.sq-in / 100.
          LEAVE.
        END.

        {sys/inc/roundup.i ld-rec}
      END.

      ELSE ld-rec = 0.
    END.
  END.
end.

else
if avail job then do:
  find first itemfg
      where itemfg.company eq tt-sales-forecast.company
        and itemfg.i-no    eq tt-report.key-03
      no-lock.
  find first cust 
      where cust.company eq tt-sales-forecast.company
        and cust.cust-no eq tt-report.key-05
      no-lock.
      
  create w-ord2.
  assign
   w-ord2.qty-due   = int(tt-report.key-04)
   w-ord2.price     = itemfg.sell-price
   w-ord2.uom       = itemfg.sell-uom.
      
  for each job-hdr FIELDS(qty std-tot-cost)
      where job-hdr.company eq tt-sales-forecast.company
        and job-hdr.job     eq job.job
        and job-hdr.job-no  eq job.job-no
        and job-hdr.job-no2 eq job.job-no2
        and job-hdr.i-no    eq tt-report.key-03
      no-lock:
        
    assign
     w-ord2.qty  = w-ord2.qty + job-hdr.qty
     w-ord2.cost = w-ord2.cost + (job-hdr.qty / 1000 * job-hdr.std-tot-cost).
  end.
end.

if avail w-ord2 then do:
/*   if w-ord2.uom begins "L" AND w-ord2.uom NE "LB" then                      */
/*         w-ord2.t-price = w-ord2.price -                                     */
/*                         round((w-ord2.price * w-ord2.disc) / 100, 2).       */
/*                                                                             */
/*       else                                                                  */
/*       if w-ord2.uom eq "CS"               and                               */
/*          (avail itemfg or avail oe-ordl) then                               */
/*         w-ord2.t-price = ((w-ord2.qty-due /                                 */
/*                           if avail oe-ordl then oe-ordl.cas-cnt             */
/*                                            else itemfg.case-count) *        */
/*                          w-ord2.price) -                                    */
/*                         round((((w-ord2.qty-due /                           */
/*                                  if avail oe-ordl then oe-ordl.cas-cnt      */
/*                                                   else itemfg.case-count) * */
/*                                 w-ord2.price) * w-ord2.disc) / 100, 2).     */
/*                                                                             */
/*       else                                                                  */
/*       if w-ord2.uom eq "C" then                                             */
/*         w-ord2.t-price = ((w-ord2.qty-due / 100) * w-ord2.price) -          */
/*                         round((((w-ord2.qty-due / 100) *                    */
/*                                  w-ord2.price) * w-ord2.disc) / 100, 2).    */
/*                                                                             */
/*       else                                                                  */
/*       if w-ord2.uom eq "M" then                                             */
/*         w-ord2.t-price = ((w-ord2.qty-due / 1000) * w-ord2.price) -         */
/*                         round((((w-ord2.qty-due / 1000) *                   */
/*                                  w-ord2.price) * w-ord2.disc) / 100, 2).    */
/*                                                                             */
/*       else                                                                  */
/*         w-ord2.t-price = ((w-ord2.qty-due) * w-ord2.price) -                */
/*                           round((((w-ord2.qty-due) *                        */
/*                                    w-ord2.price) * w-ord2.disc) / 100, 2).  */

  
    if w-ord2.uom begins "L" AND w-ord2.uom NE "LB" then
        w-ord2.t-price = w-ord2.price -
                    round((w-ord2.price * w-ord2.disc) / 100, 2).
                    
    else if w-ord2.uom eq "CS"               and
     (avail itemfg or avail oe-ordl) then
        w-ord2.t-price = (((w-ord2.qty  * w-ord2.price) /
                      if avail oe-ordl then oe-ordl.cas-cnt
                                       else itemfg.case-count) -
                    round((((w-ord2.qty * w-ord2.price)  /
                             if avail oe-ordl then oe-ordl.cas-cnt
                                              else itemfg.case-count) * w-ord2.disc) / 100, 2)) -
                   (((w-ord2.inv-qty  * w-ord2.price) /
                      if avail oe-ordl then oe-ordl.cas-cnt
                                       else itemfg.case-count) -
                    round((((w-ord2.inv-qty * w-ord2.price)  /
                             if avail oe-ordl then oe-ordl.cas-cnt
                                              else itemfg.case-count) * w-ord2.disc) / 100, 2)).
    ELSE if w-ord2.uom eq "C" then
        w-ord2.t-price = ((w-ord2.qty * w-ord2.price) / 100) -
                    round((((w-ord2.qty * w-ord2.price) / 100) * w-ord2.disc) / 100, 2) -
                    ((w-ord2.inv-qty * w-ord2.price) / 100) -
                    round((((w-ord2.inv-qty * w-ord2.price) / 100)  * w-ord2.disc) / 100, 2).
                             
    ELSE if w-ord2.uom eq "M" then
        w-ord2.t-price = ((w-ord2.qty * w-ord2.price) / 1000) -
                    round((((w-ord2.qty * w-ord2.price) / 1000) * w-ord2.disc) / 100, 2) -
                    ((w-ord2.inv-qty * w-ord2.price) / 1000) -
                    round((((w-ord2.inv-qty * w-ord2.price) / 1000) * w-ord2.disc) / 100, 2) 
                    
                    .
    ELSE w-ord2.t-price = ((w-ord2.qty * w-ord2.price) - round(((w-ord2.qty * w-ord2.price) * w-ord2.disc) / 100, 2)) -
                     ((w-ord2.inv-qty * w-ord2.price) - round((((w-ord2.inv-qty) *
                               w-ord2.price) * w-ord2.disc) / 100, 2)) .
    IF w-ord2.t-price < 0  THEN
      ASSIGN w-ord2.t-price = 0.

  
end.

/* end ---------------------------------- copr. 1997  Advanced Software, Inc. */
