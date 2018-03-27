/* ----------------------------------------------- oe/rep/backlog.i 12/97 JLF */
/* Order Backlog Summary / Detail Report                                      */
/* -------------------------------------------------------------------------- */

def var v-stat as char no-undo.
DEF VAR ld-rec AS DEC NO-UNDO.
DEF VAR li-inv-qty LIKE oe-ordl.inv-qty NO-UNDO.
DEF VAR li-ship-qty LIKE oe-ordl.ship-qty NO-UNDO.


release oe-ordl.
release job.
release itemfg.
release w-ord.

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

  create w-ord.
  assign
   w-ord.ord-no    = trim(string(oe-ord.ord-no,">>>>>9"))
   w-ord.est-no    = oe-ordl.est-no
   w-ord.due-date  = oe-ordl.req-date
   w-ord.ord-date  = oe-ord.ord-date
   w-ord.cust-no   = oe-ord.cust-no
   w-ord.cust-name = oe-ord.cust-name
   w-ord.i-no      = oe-ordl.i-no
   w-ord.i-name    = oe-ordl.i-name
   w-ord.part-no   = oe-ordl.part-no
   w-ord.qty-due   = oe-ordl.qty - li-ship-qty
   w-ord.qty       = oe-ordl.qty
   w-ord.cost      = (w-ord.qty-due / 1000) * oe-ordl.cost
   w-ord.price     = oe-ordl.price
   w-ord.uom       = oe-ordl.pr-uom 
   w-ord.disc      = oe-ordl.disc
   w-ord.sman      = oe-ord.sman[1]
   w-ord.po-num    = oe-ordl.po-no
   w-ord.job-no    = oe-ordl.job-no
   w-ord.job-no2   = oe-ordl.job-no2
   w-ord.stat      = oe-ordl.prom-code
   w-ord.inv-qty   = li-inv-qty 
   w-ord.rel-date  = ?.

  RELEASE oe-rell.
  RELEASE oe-relh.

  for each oe-rell
      where oe-rell.company eq cocode
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
     w-ord.rel-date = oe-relh.rel-date
     w-ord.rel-stat = "A".
  
  else
  for each oe-rel
      where oe-rel.company eq cocode
        and oe-rel.ord-no  eq oe-ordl.ord-no
        and oe-rel.i-no    eq oe-ordl.i-no
        and oe-rel.line    eq oe-ordl.line
        AND oe-rel.s-code  EQ "T"
      no-lock:
      
   {oe/rel-stat.i v-stat}
    
    if index("ILS",v-stat) ne 0                                   and
       (oe-rel.rel-date lt w-ord.rel-date or w-ord.rel-date eq ?) then
      assign
       w-ord.rel-date = oe-rel.rel-date
       w-ord.rel-stat = v-stat.
  end.

  IF oe-ordl.po-no-po NE 0 THEN
  FOR EACH po-ordl
      WHERE po-ordl.company        EQ oe-ordl.company
        AND po-ordl.po-no          EQ oe-ordl.po-no-po
        AND ((po-ordl.item-type    EQ YES AND
              TRIM(oe-ordl.job-no) NE "" AND
              po-ordl.job-no       EQ oe-ordl.job-no AND
              po-ordl.job-no2      EQ oe-ordl.job-no2)      OR
             (po-ordl.item-type    EQ NO AND
              po-ordl.i-no         EQ oe-ordl.i-no))
      NO-LOCK:

    ld-rec = po-ordl.t-rec-qty.

    IF po-ordl.item-type EQ YES THEN DO:
      FIND FIRST item
          WHERE item.company eq cocode
            AND item.i-no    eq po-ordl.i-no
          USE-INDEX i-no NO-LOCK NO-ERROR.

      IF AVAIL item AND INDEX("1234BPR",item.mat-type) GT 0 THEN DO:
        IF po-ordl.cons-uom NE "EA" THEN 
          RUN sys/ref/convquom.p (po-ordl.cons-uom, "EA",
                                item.basis-w, po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                                ld-rec, OUTPUT ld-rec).

        FOR EACH job-hdr
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

    w-ord.po-received = w-ord.po-received + ld-rec.
  END.
end.

else
if avail job then do:
  find first itemfg
      where itemfg.company eq cocode
        and itemfg.i-no    eq tt-report.key-03
      no-lock.
  find first cust 
      where cust.company eq cocode
        and cust.cust-no eq tt-report.key-05
      no-lock.
      
  create w-ord.
  assign
   w-ord.ord-no    = trim(job.job-no) + "-" + string(job.job-no2,"99")
   w-ord.est-no    = job.est-no
   w-ord.due-date  = IF job.due-date NE ? THEN job.due-date ELSE job.start-date
   w-ord.cust-no   = tt-report.key-05
   w-ord.cust-name = cust.name
   w-ord.i-no      = tt-report.key-03
   w-ord.i-name    = itemfg.i-name
   w-ord.part-no   = itemfg.part-no
   w-ord.qty-due   = int(tt-report.key-04)
   w-ord.price     = itemfg.sell-price
   w-ord.uom       = itemfg.sell-uom 
   w-ord.sman      = cust.sman
   w-ord.job-no    = job.job-no
   w-ord.job-no2   = job.job-no2
   w-ord.stat      = "JOB".
      
  for each job-hdr
      where job-hdr.company eq cocode
        and job-hdr.job     eq job.job
        and job-hdr.job-no  eq job.job-no
        and job-hdr.job-no2 eq job.job-no2
        and job-hdr.i-no    eq tt-report.key-03
      no-lock:
        
    assign
     w-ord.qty  = w-ord.qty + job-hdr.qty
     w-ord.cost = w-ord.cost + (job-hdr.qty / 1000 * job-hdr.std-tot-cost).
  end.
end.

else
if avail itemfg then do:
  find first cust 
      where cust.company eq cocode
        and cust.cust-no eq itemfg.cust-no
      no-lock.
      
  create w-ord.
  assign
   w-ord.est-no    = itemfg.est-no
   w-ord.due-date  = today
   w-ord.cust-no   = itemfg.cust-no
   w-ord.cust-name = cust.name
   w-ord.i-no      = itemfg.i-no
   w-ord.i-name    = itemfg.i-name
   w-ord.part-no   = itemfg.part-no
   w-ord.price     = itemfg.sell-price
   w-ord.uom       = itemfg.sell-uom 
   w-ord.sman      = cust.sman
   w-ord.stat      = "ITEM".
end.

if avail w-ord then do:
  for each fg-bin
      where fg-bin.company   eq cocode
        and fg-bin.i-no      eq w-ord.i-no
        and ((fg-bin.job-no  eq w-ord.job-no and
              fg-bin.job-no2 eq w-ord.job-no2) or v-fg-qty)
      no-lock:
        
    assign
     w-ord.qty-onh = w-ord.qty-onh + fg-bin.qty
     v-qty-pal     = (if fg-bin.case-count   eq 0
                      then 1 else fg-bin.case-count)   *
                     (if fg-bin.cases-unit   eq 0
                      then 1 else fg-bin.cases-unit)   *
                     (if fg-bin.units-pallet eq 0
                      then 1 else fg-bin.units-pallet)
     w-ord.pallets = w-ord.pallets + (fg-bin.qty / v-qty-pal).
     
    {sys/inc/roundup.i w-ord.pallets}
  end.

  IF v-sales-qty THEN do: /* Shipped Value*/
  if w-ord.uom begins "L" AND w-ord.uom NE "LB" then
    w-ord.t-price = w-ord.price -
                    round((w-ord.price * w-ord.disc) / 100, 2).
                    
  else
  if w-ord.uom eq "CS"               and
     (avail itemfg or avail oe-ordl) then
    w-ord.t-price = ((w-ord.qty-due / 
                      if avail oe-ordl then oe-ordl.cas-cnt
                                       else itemfg.case-count) *
                     w-ord.price) -
                    round((((w-ord.qty-due /
                             if avail oe-ordl then oe-ordl.cas-cnt
                                              else itemfg.case-count) *
                            w-ord.price) * w-ord.disc) / 100, 2).

  else
  if w-ord.uom eq "C" then
    w-ord.t-price = ((w-ord.qty-due / 100) * w-ord.price) -
                    round((((w-ord.qty-due / 100) *
                             w-ord.price) * w-ord.disc) / 100, 2).
                             
  else
  if w-ord.uom eq "M" then
    w-ord.t-price = ((w-ord.qty-due / 1000) * w-ord.price) -
                    round((((w-ord.qty-due / 1000) * w-ord.price) * w-ord.disc) / 100, 2)  .

  else
    w-ord.t-price = ((w-ord.qty-due) * w-ord.price) -
                      round((((w-ord.qty-due) *
                               w-ord.price) * w-ord.disc) / 100, 2).
    
  END.
  ELSE DO: /* Invoiced Value  w-ord.inv-qty */
          if w-ord.uom begins "L" AND w-ord.uom NE "LB" then
    w-ord.t-price = w-ord.price -
                    round((w-ord.price * w-ord.disc) / 100, 2).
                    
  else
  if w-ord.uom eq "CS"               and
     (avail itemfg or avail oe-ordl) then
    w-ord.t-price = (((w-ord.qty  * w-ord.price) /
                      if avail oe-ordl then oe-ordl.cas-cnt
                                       else itemfg.case-count) -
                    round((((w-ord.qty * w-ord.price)  /
                             if avail oe-ordl then oe-ordl.cas-cnt
                                              else itemfg.case-count) * w-ord.disc) / 100, 2)) -
                   (((w-ord.inv-qty  * w-ord.price) /
                      if avail oe-ordl then oe-ordl.cas-cnt
                                       else itemfg.case-count) -
                    round((((w-ord.inv-qty * w-ord.price)  /
                             if avail oe-ordl then oe-ordl.cas-cnt
                                              else itemfg.case-count) * w-ord.disc) / 100, 2)).

  else
  if w-ord.uom eq "C" then
    w-ord.t-price = ((w-ord.qty * w-ord.price) / 100) -
                    round((((w-ord.qty * w-ord.price) / 100) * w-ord.disc) / 100, 2) -
                    ((w-ord.inv-qty * w-ord.price) / 100) -
                    round((((w-ord.inv-qty * w-ord.price) / 100)  * w-ord.disc) / 100, 2).
                             
  else
  if w-ord.uom eq "M" then
    w-ord.t-price = ((w-ord.qty * w-ord.price) / 1000) -
                    round((((w-ord.qty * w-ord.price) / 1000) * w-ord.disc) / 100, 2) -
                    ((w-ord.inv-qty * w-ord.price) / 1000) -
                    round((((w-ord.inv-qty * w-ord.price) / 1000) * w-ord.disc) / 100, 2) 
                    
                    .

  else
    w-ord.t-price = ((w-ord.qty * w-ord.price) - round(((w-ord.qty * w-ord.price) * w-ord.disc) / 100, 2)) -
                     ((w-ord.inv-qty * w-ord.price) - round((((w-ord.inv-qty) *
                               w-ord.price) * w-ord.disc) / 100, 2)) .
  END.
  IF w-ord.t-price < 0  THEN
      ASSIGN w-ord.t-price = 0.
end.

/* end ---------------------------------- copr. 1997  Advanced Software, Inc. */
