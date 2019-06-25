/* -------------------------------------------------- fg/updfgcs1.p 02/99 JLF */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM ip-recid AS RECID NO-UNDO.
DEF INPUT PARAM ip-recalc AS LOG NO-UNDO.

{sys/inc/var.i NEW SHARED}

DEF BUFFER b-itemfg FOR itemfg.

def var v-qty             as   int.
def var v-binqty          as   int.
def var v-date            as   date init 01/01/0001.
def var v-uom             like fg-rcpth.pur-uom.
def var v-cost-ea         as   dec.
def var v-cost            as   dec extent 8.
def var v-r-no            like fg-rcpth.r-no.
def var v-rec             as   log init no.
def var ll-all-empty-bins as   log no-undo.

{fg/fullset.i NEW}


DISABLE TRIGGERS FOR LOAD OF itemfg.

find itemfg where recid(itemfg) eq ip-recid.

cocode = itemfg.company.

find first fg-ctrl where fg-ctrl.company eq itemfg.company no-lock no-error.

IF itemfg.isaset AND itemfg.alloc THEN RUN fg/fullset.p (ROWID(itemfg)).

FIND FIRST tt-fg-set NO-ERROR.

IF AVAIL tt-fg-set AND tt-fg-set.part-no NE itemfg.i-no THEN DO:
  RELEASE tt-fg-set.

  FOR EACH tt-fg-set,

      FIRST b-itemfg
      WHERE b-itemfg.company EQ cocode
        AND b-itemfg.i-no    EQ tt-fg-set.part-no
      NO-LOCK:
    
    if b-itemfg.prod-uom eq "EA" then
      v-cost-ea = b-itemfg.avg-cost.
    else
      run sys/ref/convcuom.p(b-itemfg.prod-uom, "EA", 0, 0, 0, 0,
                             b-itemfg.avg-cost, output v-cost-ea).

    v-cost[1] = v-cost[1] + (v-cost-ea * tt-fg-set.part-qty-dec).

    if b-itemfg.prod-uom eq "EA" then
      v-cost-ea = b-itemfg.last-cost.
    else
      run sys/ref/convcuom.p(b-itemfg.prod-uom, "EA", 0, 0, 0, 0,
                             b-itemfg.last-cost, output v-cost-ea).

    v-cost[2] = v-cost[2] + (v-cost-ea * tt-fg-set.part-qty-dec).

    if b-itemfg.prod-uom eq "EA" then
      v-cost-ea = b-itemfg.std-mat-cost.
    else
      run sys/ref/convcuom.p(b-itemfg.prod-uom, "EA", 0, 0, 0, 0,
                             b-itemfg.std-mat-cost, output v-cost-ea).

    v-cost[3] = v-cost[3] + (v-cost-ea * tt-fg-set.part-qty-dec).

    if b-itemfg.prod-uom eq "EA" then
      v-cost-ea = b-itemfg.std-lab-cost.
    else
      run sys/ref/convcuom.p(b-itemfg.prod-uom, "EA", 0, 0, 0, 0,
                             b-itemfg.std-lab-cost, output v-cost-ea).

    v-cost[4] = v-cost[4] + (v-cost-ea * tt-fg-set.part-qty-dec).

    if b-itemfg.prod-uom eq "EA" then
      v-cost-ea = b-itemfg.std-var-cost.
    else
      run sys/ref/convcuom.p(b-itemfg.prod-uom, "EA", 0, 0, 0, 0,
                             b-itemfg.std-var-cost, output v-cost-ea).

    v-cost[5] = v-cost[5] + (v-cost-ea * tt-fg-set.part-qty-dec).

    if b-itemfg.prod-uom eq "EA" then
      v-cost-ea = b-itemfg.std-fix-cost.
    else
      run sys/ref/convcuom.p(b-itemfg.prod-uom, "EA", 0, 0, 0, 0,
                             b-itemfg.std-fix-cost, output v-cost-ea).

    v-cost[6] = v-cost[6] + (v-cost-ea * tt-fg-set.part-qty-dec).
  END.

  v-qty = 1.
END.

ELSE DO:
 IF ip-recalc THEN
 FOR EACH fg-bin
     WHERE fg-bin.company EQ itemfg.company
       AND fg-bin.i-no    EQ itemfg.i-no:

   IF fg-bin.job-no NE "" THEN RUN oe/fgbincst.p (ROWID(fg-bin)).

   ELSE DO:
     ASSIGN
      fg-bin.std-tot-cost = 0
      fg-bin.std-lab-cost = 0
      fg-bin.std-mat-cost = 0
      fg-bin.std-var-cost = 0
      fg-bin.std-fix-cost = 0.

     RUN fg/upfgbinc.p (ROWID(fg-bin)).
   END.
 END.

 ll-all-empty-bins = NOT CAN-FIND(FIRST fg-bin
                                  WHERE fg-bin.company EQ itemfg.company
                                    AND fg-bin.i-no    EQ itemfg.i-no
                                    AND fg-bin.qty     NE 0).

 for each fg-bin
     where fg-bin.company eq itemfg.company
       and fg-bin.i-no    eq itemfg.i-no
     break by fg-bin.i-no:

  if fg-bin.pur-uom eq "" then fg-bin.pur-uom = itemfg.prod-uom.

  if fg-bin.pur-uom eq "EA" then
    v-cost-ea = fg-bin.std-tot-cost.
  else
    run sys/ref/convcuom.p(fg-bin.pur-uom, "EA", 0, 0, 0, 0,
                           fg-bin.std-tot-cost, output v-cost-ea).

  v-binqty = fg-bin.qty * (if fg-bin.qty lt 0 then -1 else 1).

  if ll-all-empty-bins then v-binqty = 1.

  assign
   v-cost[1] = v-cost[1] + (v-binqty * v-cost-ea)
   v-qty     = v-qty + v-binqty.

  release fg-rcpth.
  release fg-rdtlh.
/*
  IF TRIM(fg-bin.tag) EQ "" THEN
  for each fg-rcpth
      where fg-rcpth.company   eq fg-bin.company
        and fg-rcpth.i-no      eq fg-bin.i-no
        and fg-rcpth.job-no    eq fg-bin.job-no
        and fg-rcpth.job-no2   eq fg-bin.job-no2
        and fg-rcpth.rita-code eq "R"
      use-index i-no no-lock,

      first fg-rdtlh
      where fg-rdtlh.r-no    eq fg-rcpth.r-no
        and fg-rdtlh.loc     eq fg-bin.loc
        and fg-rdtlh.loc-bin eq fg-bin.loc-bin
        and fg-rdtlh.tag     eq fg-bin.tag
        AND fg-rdtlh.cust-no EQ fg-bin.cust-no
        and fg-rdtlh.qty     gt 0
      use-index rm-rdtl no-lock

      by fg-rcpth.trans-date desc
      by fg-rcpth.r-no       desc
      by recid(fg-rdtlh)     desc:
    leave.
  end.

  ELSE
  for each fg-rdtlh
      where fg-rdtlh.company   eq fg-bin.company
        and fg-rdtlh.loc       eq fg-bin.loc
        and fg-rdtlh.tag       eq fg-bin.tag
        and fg-rdtlh.loc-bin   eq fg-bin.loc-bin
        and fg-rdtlh.rita-code eq "R"
        AND fg-rdtlh.cust-no   EQ fg-bin.cust-no
        and fg-rdtlh.qty       gt 0
      use-index tag no-lock,

      first fg-rcpth
      where fg-rcpth.r-no      eq fg-rdtlh.r-no
        and fg-rcpth.i-no      eq fg-bin.i-no
        and fg-rcpth.job-no    eq fg-bin.job-no
        and fg-rcpth.job-no2   eq fg-bin.job-no2
      use-index r-no no-lock

      by fg-rcpth.trans-date desc
      by fg-rcpth.r-no       desc
      by recid(fg-rdtlh)     desc:
    leave.
  end.

  if (avail fg-rdtlh                            and
      ((fg-rcpth.trans-date  gt v-date    or
        (fg-rcpth.trans-date eq v-date and
         fg-rcpth.r-no       gt v-r-no))     or
       v-date                eq 01/01/0001   or
       v-date                eq ?))                 or
     (not v-rec                                 and
      not avail fg-rdtlh                        and
      fg-bin.aging-date      gt v-date)             or
     first(fg-bin.i-no)                             then do:

    if avail fg-rdtlh then do:
      assign
       v-rec     = yes
       v-r-no    = fg-rcpth.r-no
       v-date    = fg-rcpth.trans-date
       v-uom     = "M"
       v-cost[2] = fg-rdtlh.cost.

      FIND FIRST fg-rctd WHERE fg-rctd.r-no EQ fg-rcpth.r-no USE-INDEX fg-rctd NO-LOCK NO-ERROR.

      IF AVAIL fg-rctd THEN
        ASSIGN
         v-uom     = "EA"
         v-cost[2] = fg-rctd.ext-cost / fg-rctd.t-qty.
    END.

    else
      assign
       v-r-no    = 0
       v-date    = fg-bin.aging-date
       v-uom     = fg-bin.pur-uom
       v-cost[2] = fg-bin.std-tot-cost.

    if v-uom ne "EA" then
      run sys/ref/convcuom.p(v-uom, "EA", 0, 0, 0, 0,
                             v-cost[2], output v-cost[2]).

    if fg-ctrl.inv-meth eq "L" then
      assign
       v-cost[3] = fg-bin.std-mat-cost / fg-bin.std-tot-cost * v-cost[2]
       v-cost[4] = fg-bin.std-lab-cost / fg-bin.std-tot-cost * v-cost[2]
       v-cost[5] = fg-bin.std-var-cost / fg-bin.std-tot-cost * v-cost[2]
       v-cost[6] = fg-bin.std-fix-cost / fg-bin.std-tot-cost * v-cost[2].
  end.
*/
  if fg-ctrl.inv-meth eq "A" then do:
    if fg-bin.pur-uom eq "EA" then
      v-cost-ea = fg-bin.std-mat-cost.
    else
      run sys/ref/convcuom.p(fg-bin.pur-uom, "EA", 0, 0, 0, 0,
                             fg-bin.std-mat-cost, output v-cost-ea).

    v-cost[3] = v-cost[3] + (v-binqty * v-cost-ea).

    if fg-bin.pur-uom eq "EA" then
      v-cost-ea = fg-bin.std-lab-cost.
    else
      run sys/ref/convcuom.p(fg-bin.pur-uom, "EA", 0, 0, 0, 0,
                             fg-bin.std-lab-cost, output v-cost-ea).

    v-cost[4] = v-cost[4] + (v-binqty * v-cost-ea).

    if fg-bin.pur-uom eq "EA" then
      v-cost-ea = fg-bin.std-var-cost.
    else
      run sys/ref/convcuom.p(fg-bin.pur-uom, "EA", 0, 0, 0, 0,
                             fg-bin.std-var-cost, output v-cost-ea).

    v-cost[5] = v-cost[5] + (v-binqty * v-cost-ea).

    if fg-bin.pur-uom eq "EA" then
      v-cost-ea = fg-bin.std-fix-cost.
    else
      run sys/ref/convcuom.p(fg-bin.pur-uom, "EA", 0, 0, 0, 0,
                             fg-bin.std-fix-cost, output v-cost-ea).

    v-cost[6] = v-cost[6] + (v-binqty * v-cost-ea).
  end.
 end.
END.

if v-qty ne 0 or ll-all-empty-bins then do:
  v-cost[1] = v-cost[1] / v-qty.

  if fg-ctrl.inv-meth eq "A" then
    assign
     v-cost[3] = v-cost[3] / v-qty
     v-cost[4] = v-cost[4] / v-qty
     v-cost[5] = v-cost[5] / v-qty
     v-cost[6] = v-cost[6] / v-qty.

  if itemfg.prod-uom eq "EA" then
    itemfg.avg-cost = v-cost[1].
  else
    run sys/ref/convcuom.p("EA", itemfg.prod-uom, 0, 0, 0, 0,
                           v-cost[1], output itemfg.avg-cost).
/*
  if itemfg.prod-uom eq "EA" then
    itemfg.last-cost = v-cost[2].
  else
    run sys/ref/convcuom.p("EA", itemfg.prod-uom, 0, 0, 0, 0,
                           v-cost[2], output itemfg.last-cost).
*/
  if itemfg.prod-uom eq "EA" then
    itemfg.std-mat-cost = v-cost[3].
  else
    run sys/ref/convcuom.p("EA", itemfg.prod-uom, 0, 0, 0, 0,
                           v-cost[3], output itemfg.std-mat-cost).

  if itemfg.prod-uom eq "EA" then
    itemfg.std-lab-cost = v-cost[4].
  else
    run sys/ref/convcuom.p("EA", itemfg.prod-uom, 0, 0, 0, 0,
                           v-cost[4], output itemfg.std-lab-cost).

  if itemfg.prod-uom eq "EA" then
    itemfg.std-var-cost = v-cost[5].
  else
    run sys/ref/convcuom.p("EA", itemfg.prod-uom, 0, 0, 0, 0,
                           v-cost[5], output itemfg.std-var-cost).

  if itemfg.prod-uom eq "EA" then
    itemfg.std-fix-cost = v-cost[6].
  else
    run sys/ref/convcuom.p("EA", itemfg.prod-uom, 0, 0, 0, 0,
                           v-cost[6], output itemfg.std-fix-cost).

  assign
   itemfg.std-tot-cost   = itemfg.std-mat-cost + itemfg.std-lab-cost +
                           itemfg.std-var-cost + itemfg.std-fix-cost
   itemfg.total-std-cost = itemfg.std-tot-cost.
end.

if itemfg.avg-cost       eq ? then itemfg.avg-cost       = 0.
if itemfg.last-cost      eq ? then itemfg.last-cost      = 0.
if itemfg.std-mat-cost   eq ? then itemfg.std-mat-cost   = 0.
if itemfg.std-lab-cost   eq ? then itemfg.std-lab-cost   = 0.
if itemfg.std-var-cost   eq ? then itemfg.std-var-cost   = 0.
if itemfg.std-fix-cost   eq ? then itemfg.std-fix-cost   = 0.
if itemfg.std-tot-cost   eq ? then itemfg.std-tot-cost   = 0.
if itemfg.total-std-cost eq ? then itemfg.total-std-cost = 0.

/* end ---------------------------------- copr. 1999  Advanced Software, Inc. */
