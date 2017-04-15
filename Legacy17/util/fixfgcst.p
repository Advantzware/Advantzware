/* ------------------------------------------------ util/fixfgcst.p 04/04 JLF */
/* -------------------------------------------------------------------------- */

def input parameter ip-rowid as ROWID.

{sys/inc/var.i shared}

def var v-qty            as   int.
def var v-binqty         as   int.
def var v-date           as   date init 01/01/0001.
def var v-uom            like fg-rcpth.pur-uom.
def var v-cost-ea        as   dec.
def var v-cost           as   dec extent 8.
def var v-r-no           like fg-rcpth.r-no.
def var v-rec            as   log init no.
def var ll-one-empty-bin as   log no-undo.
DEF VAR ld-cvt-cost AS DEC NO-UNDO.
DEF VAR lv-uom LIKE fg-rctd.cost-uom NO-UNDO.
DEF VAR fg-uom-list AS CHAR NO-UNDO.

DEF BUFFER b-itemfg FOR itemfg.
DEF BUFFER b-fg-rcpth FOR fg-rcpth.
DEF BUFFER b-fg-rdtlh FOR fg-rdtlh.

DEF TEMP-TABLE tt-itemfg NO-UNDO LIKE itemfg.
DEF TEMP-TABLE tt-fg-bin NO-UNDO LIKE fg-bin.
DEF TEMP-TABLE tt-fg-rctd NO-UNDO LIKE fg-rctd.

{fg/fullset.i NEW}


RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

find itemfg where ROWID(itemfg) eq ip-rowid NO-ERROR.

IF AVAIL itemfg THEN DO:
  FIND FIRST fg-ctrl WHERE fg-ctrl.company eq itemfg.company NO-LOCK NO-ERROR.

  IF itemfg.isaset AND itemfg.alloc THEN RUN fg/fullset.p (ROWID(itemfg)).

  FIND FIRST tt-fg-set NO-ERROR.

  IF AVAIL tt-fg-set AND tt-fg-set.part-no NE itemfg.i-no THEN DO:
    RUN cost-from-set.

    FIND b-itemfg WHERE ROWID(b-itemfg) EQ ROWID(itemfg) NO-LOCK.

    STATUS DEFAULT "Please wait...  Updating item: " + TRIM(b-itemfg.i-no).

    RUN create-temp-fg.
    RUN cost-from-vars.
  END.

  ELSE DO:
    FIND b-itemfg WHERE ROWID(b-itemfg) EQ ROWID(itemfg) NO-LOCK.
    RUN cost-from-hist.
  END.

  FIND FIRST tt-itemfg NO-ERROR.

  IF AVAIL tt-itemfg THEN
    ASSIGN
     itemfg.avg-cost       = tt-itemfg.avg-cost
     itemfg.last-cost      = tt-itemfg.last-cost
     itemfg.std-mat-cost   = tt-itemfg.std-mat-cost
     itemfg.std-lab-cost   = tt-itemfg.std-lab-cost 
     itemfg.std-var-cost   = tt-itemfg.std-var-cost
     itemfg.std-fix-cost   = tt-itemfg.std-fix-cost
     itemfg.std-tot-cost   = tt-itemfg.std-tot-cost
     itemfg.total-std-cost = tt-itemfg.total-std-cost.
END.

RETURN.

PROCEDURE cost-from-hist.
  DEF VAR v-binqty AS INT NO-UNDO.
  DEF VAR v-qty AS INT NO-UNDO.
  DEF VAR v-cost AS DEC NO-UNDO.
  DEF VAR v-tagcost AS DEC NO-UNDO.


  RUN create-temp-fg.

  FOR EACH fg-rcpth
      WHERE fg-rcpth.company EQ tt-itemfg.company
        AND fg-rcpth.i-no    EQ tt-itemfg.i-no
      USE-INDEX tran NO-LOCK,

      EACH fg-rdtlh
      WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
        AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
      USE-INDEX rm-rdtl NO-LOCK

      BREAK BY fg-rdtlh.loc
            BY fg-rdtlh.loc-bin
            BY fg-rdtlh.tag
            BY fg-rcpth.job-no
            BY fg-rcpth.job-no2
            BY fg-rcpth.trans-date
            BY fg-rdtlh.trans-time
            BY fg-rcpth.r-no:

    STATUS DEFAULT "Please wait...  Updating item: " + TRIM(tt-itemfg.i-no) +
        " " + STRING(fg-rcpth.r-no,">>>>>>>>>>").

    FIND FIRST tt-fg-bin
        WHERE tt-fg-bin.company EQ fg-rcpth.company
          AND tt-fg-bin.i-no    EQ fg-rcpth.i-no
          AND tt-fg-bin.loc     EQ fg-rdtlh.loc
          AND tt-fg-bin.loc-bin EQ fg-rdtlh.loc-bin
          AND tt-fg-bin.tag     EQ fg-rdtlh.tag
          AND tt-fg-bin.job-no  EQ fg-rcpth.job-no
          AND tt-fg-bin.job-no2 EQ fg-rcpth.job-no2
        USE-INDEX co-ino NO-ERROR.
    IF NOT AVAIL tt-fg-bin THEN DO:
      CREATE tt-fg-bin.
      ASSIGN
       tt-fg-bin.company      = fg-rdtlh.company
       tt-fg-bin.job-no       = fg-rcpth.job-no
       tt-fg-bin.job-no2      = fg-rcpth.job-no2
       tt-fg-bin.loc          = fg-rdtlh.loc
       tt-fg-bin.loc-bin      = fg-rdtlh.loc-bin
       tt-fg-bin.tag          = fg-rdtlh.tag
       tt-fg-bin.i-no         = fg-rcpth.i-no
       tt-fg-bin.aging-date   = fg-rcpth.trans-date
       tt-fg-bin.pur-uom      = itemfg.prod-uom.
    END.

    IF fg-rcpth.rita-code EQ "R" THEN DO:
      IF tt-fg-bin.case-count   LE 0 AND fg-rdtlh.qty-case     GT 0 THEN
        tt-fg-bin.case-count   = fg-rdtlh.qty-case.
      IF tt-fg-bin.units-pallet LE 0 AND fg-rdtlh.units-pallet GT 0 THEN
        tt-fg-bin.units-pallet = fg-rdtlh.units-pallet.
      IF tt-fg-bin.cases-unit   LE 0 AND fg-rdtlh.stacks-unit  GT 0 THEN
        tt-fg-bin.cases-unit   = fg-rdtlh.stacks-unit.
    END.

    IF INDEX("RTA",fg-rcpth.rita-code) GT 0                       OR
       (fg-rcpth.rita-code EQ "C" AND FIRST-OF(fg-rcpth.job-no2)) THEN DO:
      FIND FIRST fg-rctd WHERE fg-rctd.r-no EQ fg-rcpth.r-no USE-INDEX fg-rctd NO-LOCK NO-ERROR.

      CREATE tt-fg-rctd.

      IF AVAIL fg-rctd         AND
         fg-rctd.t-qty NE 0    AND
         fg-rctd.ext-cost NE 0 AND
         fg-rctd.ext-cost NE ? THEN
        BUFFER-COPY fg-rctd TO tt-fg-rctd
        ASSIGN
         tt-fg-rctd.rita-code = fg-rcpth.rita-code.

      ELSE DO:
        BUFFER-COPY fg-rcpth TO tt-fg-rctd.
        BUFFER-COPY fg-rdtlh TO tt-fg-rctd
        ASSIGN
         tt-fg-rctd.t-qty    = fg-rdtlh.qty
         tt-fg-rctd.ext-cost = fg-rdtlh.cost *
                               (fg-rdtlh.qty / IF fg-rcpth.pur-uom EQ "M" THEN 1000 ELSE 1).
      END.

      IF fg-rcpth.rita-code EQ "R" AND
         tt-fg-rctd.t-qty NE 0     AND
         tt-fg-rctd.ext-cost NE 0  AND
         tt-fg-rctd.ext-cost NE ?  THEN
        ASSIGN
         lv-uom      = "M"
         ld-cvt-cost = tt-fg-rctd.ext-cost / tt-fg-rctd.t-qty * 1000.
      ELSE
        ASSIGN
         lv-uom      = itemfg.prod-uom
         ld-cvt-cost = fg-rdtlh.cost.

      IF ld-cvt-cost NE 0 AND ld-cvt-cost NE ?                  AND
         (tt-fg-rctd.t-qty NE 0 OR tt-fg-rctd.rita-code EQ "A") AND
         tt-fg-rctd.t-qty NE ?                                  THEN DO:
        find first job-hdr where job-hdr.company eq cocode
                          and job-hdr.job-no  eq tt-fg-bin.job-no
                          and job-hdr.job-no2 eq tt-fg-bin.job-no2
                          and job-hdr.i-no    eq tt-fg-bin.i-no
                no-lock no-error.
        {fg/upd-bin.i "tt-fg-bin" "lv-uom" "ld-cvt-cost" tt-fg-rctd}
      END.

      ELSE DO:
        {fg/fg-mkbin.i tt-}
      END.

      DELETE tt-fg-rctd.
    END.

    ELSE DO:
      IF fg-rcpth.rita-code EQ "S"   AND
         tt-fg-bin.std-tot-cost NE ? AND
         tt-fg-bin.std-tot-cost NE 0 THEN DO:
        FIND b-fg-rcpth WHERE ROWID(b-fg-rcpth) EQ ROWID(fg-rcpth) NO-ERROR.
        FIND b-fg-rdtlh WHERE ROWID(b-fg-rdtlh) EQ ROWID(fg-rdtlh) NO-ERROR.
        IF AVAIL b-fg-rcpth AND AVAIL b-fg-rdtlh THEN
          ASSIGN
           b-fg-rcpth.pur-uom = tt-fg-bin.pur-uom
           b-fg-rdtlh.cost    = tt-fg-bin.std-tot-cost.
        FIND b-fg-rcpth WHERE ROWID(b-fg-rcpth) EQ ROWID(fg-rcpth) NO-LOCK NO-ERROR.
        FIND b-fg-rdtlh WHERE ROWID(b-fg-rdtlh) EQ ROWID(fg-rdtlh) NO-LOCK NO-ERROR.
      END.

      {fg/fg-mkbin.i tt-}
    END.
  END. /* each fg-rcpth */

  RUN cost-from-bins.
END.

PROCEDURE cost-from-set.
  def var lv-cost like v-cost.


  RELEASE tt-fg-set.

  FOR EACH tt-fg-set,

      FIRST b-itemfg
      WHERE b-itemfg.company EQ tt-fg-set.company
        AND b-itemfg.i-no    EQ tt-fg-set.part-no
      NO-LOCK:

    RUN cost-from-hist.

    FIND FIRST tt-itemfg.
    
    if tt-itemfg.prod-uom eq "EA" then
      v-cost-ea = tt-itemfg.avg-cost.
    else
      run sys/ref/convcuom.p(tt-itemfg.prod-uom, "EA", 0, 0, 0, 0,
                             tt-itemfg.avg-cost, output v-cost-ea).

    lv-cost[1] = lv-cost[1] + (v-cost-ea * tt-fg-set.part-qty-dec).

    if tt-itemfg.prod-uom eq "EA" then
      v-cost-ea = tt-itemfg.last-cost.
    else
      run sys/ref/convcuom.p(tt-itemfg.prod-uom, "EA", 0, 0, 0, 0,
                             tt-itemfg.last-cost, output v-cost-ea).

    lv-cost[2] = lv-cost[2] + (v-cost-ea * tt-fg-set.part-qty-dec).

    if tt-itemfg.prod-uom eq "EA" then
      v-cost-ea = tt-itemfg.std-mat-cost.
    else
      run sys/ref/convcuom.p(tt-itemfg.prod-uom, "EA", 0, 0, 0, 0,
                             tt-itemfg.std-mat-cost, output v-cost-ea).

    lv-cost[3] = lv-cost[3] + (v-cost-ea * tt-fg-set.part-qty-dec).

    if tt-itemfg.prod-uom eq "EA" then
      v-cost-ea = tt-itemfg.std-lab-cost.
    else
      run sys/ref/convcuom.p(tt-itemfg.prod-uom, "EA", 0, 0, 0, 0,
                             tt-itemfg.std-lab-cost, output v-cost-ea).

    lv-cost[4] = lv-cost[4] + (v-cost-ea * tt-fg-set.part-qty-dec).

    if tt-itemfg.prod-uom eq "EA" then
      v-cost-ea = tt-itemfg.std-var-cost.
    else
      run sys/ref/convcuom.p(tt-itemfg.prod-uom, "EA", 0, 0, 0, 0,
                             tt-itemfg.std-var-cost, output v-cost-ea).

    lv-cost[5] = lv-cost[5] + (v-cost-ea * tt-fg-set.part-qty-dec).

    if tt-itemfg.prod-uom eq "EA" then
      v-cost-ea = tt-itemfg.std-fix-cost.
    else
      run sys/ref/convcuom.p(tt-itemfg.prod-uom, "EA", 0, 0, 0, 0,
                             tt-itemfg.std-fix-cost, output v-cost-ea).

    lv-cost[6] = lv-cost[6] + (v-cost-ea * tt-fg-set.part-qty-dec).
  END.

  ASSIGN
   v-qty     = 1
   v-cost[1] = lv-cost[1]
   v-cost[2] = lv-cost[2]
   v-cost[3] = lv-cost[3]
   v-cost[4] = lv-cost[4]
   v-cost[5] = lv-cost[5]
   v-cost[6] = lv-cost[6]
   v-cost[7] = lv-cost[7]
   v-cost[8] = lv-cost[8].

END PROCEDURE.

PROCEDURE cost-from-bins:
assign
 v-qty  = 0
 v-cost = 0.

for each tt-fg-bin
    where tt-fg-bin.company eq tt-itemfg.company
      and tt-fg-bin.i-no    eq tt-itemfg.i-no
    break by tt-fg-bin.i-no:

  ll-one-empty-bin = first(tt-fg-bin.i-no) and last(tt-fg-bin.i-no) and tt-fg-bin.qty eq 0.

  if tt-fg-bin.pur-uom eq "" then tt-fg-bin.pur-uom = tt-itemfg.prod-uom.

  if tt-fg-bin.pur-uom eq "EA" then
    v-cost-ea = tt-fg-bin.std-tot-cost.
  else
    run sys/ref/convcuom.p(tt-fg-bin.pur-uom, "EA", 0, 0, 0, 0,
                           tt-fg-bin.std-tot-cost, output v-cost-ea).

  v-binqty = tt-fg-bin.qty * (if tt-fg-bin.qty lt 0 then -1 else 1).

  if ll-one-empty-bin then v-binqty = 1.

  assign
   v-cost[1] = v-cost[1] + (v-binqty * v-cost-ea)
   v-qty     = v-qty + v-binqty.

  release b-fg-rcpth.
  release b-fg-rdtlh.

  for each b-fg-rcpth
      where b-fg-rcpth.company   eq tt-itemfg.company
        and b-fg-rcpth.i-no      eq tt-fg-bin.i-no
        and b-fg-rcpth.job-no    eq tt-fg-bin.job-no
        and b-fg-rcpth.job-no2   eq tt-fg-bin.job-no2
        and b-fg-rcpth.rita-code eq "R"
      use-index i-no no-lock,

      first b-fg-rdtlh
      where b-fg-rdtlh.r-no    eq b-fg-rcpth.r-no
        and b-fg-rdtlh.loc     eq tt-fg-bin.loc
        and b-fg-rdtlh.loc-bin eq tt-fg-bin.loc-bin
        and b-fg-rdtlh.tag     eq tt-fg-bin.tag
        and b-fg-rdtlh.qty     gt 0
      use-index rm-rdtl no-lock

      by b-fg-rcpth.trans-date desc
      BY b-fg-rdtlh.trans-time DESC
      by b-fg-rcpth.r-no       desc
      by recid(b-fg-rdtlh)     desc:
    leave.
  end.

  if (avail b-fg-rdtlh                            and
      ((b-fg-rcpth.trans-date  gt v-date    or
        (b-fg-rcpth.trans-date eq v-date and
         b-fg-rcpth.r-no       gt v-r-no))     or
       v-date                eq 01/01/0001   or
       v-date                eq ?))                 or
     (not v-rec                                 and
      not avail b-fg-rdtlh                        and
      tt-fg-bin.aging-date      gt v-date)             or
     first(tt-fg-bin.i-no)                             then do:

    if avail b-fg-rdtlh then
      assign
       v-rec     = yes
       v-r-no    = b-fg-rcpth.r-no
       v-date    = b-fg-rcpth.trans-date
       v-uom     = "M"
       v-cost[2] = b-fg-rdtlh.cost.
    else
      assign
       v-r-no    = 0
       v-date    = tt-fg-bin.aging-date
       v-uom     = tt-fg-bin.pur-uom
       v-cost[2] = tt-fg-bin.std-tot-cost.

    if v-uom ne "EA" then
      run sys/ref/convcuom.p(v-uom, "EA", 0, 0, 0, 0,
                             v-cost[2], output v-cost[2]).

    if fg-ctrl.inv-meth eq "L" then
      assign
       v-cost[3] = tt-fg-bin.std-mat-cost / tt-fg-bin.std-tot-cost * v-cost[2]
       v-cost[4] = tt-fg-bin.std-lab-cost / tt-fg-bin.std-tot-cost * v-cost[2]
       v-cost[5] = tt-fg-bin.std-var-cost / tt-fg-bin.std-tot-cost * v-cost[2]
       v-cost[6] = tt-fg-bin.std-fix-cost / tt-fg-bin.std-tot-cost * v-cost[2].
  end.

  if fg-ctrl.inv-meth eq "A" then do:
    if tt-fg-bin.pur-uom eq "EA" then
      v-cost-ea = tt-fg-bin.std-mat-cost.
    else
      run sys/ref/convcuom.p(tt-fg-bin.pur-uom, "EA", 0, 0, 0, 0,
                             tt-fg-bin.std-mat-cost, output v-cost-ea).

    v-cost[3] = v-cost[3] + (v-binqty * v-cost-ea).

    if tt-fg-bin.pur-uom eq "EA" then
      v-cost-ea = tt-fg-bin.std-lab-cost.
    else
      run sys/ref/convcuom.p(tt-fg-bin.pur-uom, "EA", 0, 0, 0, 0,
                             tt-fg-bin.std-lab-cost, output v-cost-ea).

    v-cost[4] = v-cost[4] + (v-binqty * v-cost-ea).

    if tt-fg-bin.pur-uom eq "EA" then
      v-cost-ea = tt-fg-bin.std-var-cost.
    else
      run sys/ref/convcuom.p(tt-fg-bin.pur-uom, "EA", 0, 0, 0, 0,
                             tt-fg-bin.std-var-cost, output v-cost-ea).

    v-cost[5] = v-cost[5] + (v-binqty * v-cost-ea).

    if tt-fg-bin.pur-uom eq "EA" then
      v-cost-ea = tt-fg-bin.std-fix-cost.
    else
      run sys/ref/convcuom.p(tt-fg-bin.pur-uom, "EA", 0, 0, 0, 0,
                             tt-fg-bin.std-fix-cost, output v-cost-ea).

    v-cost[6] = v-cost[6] + (v-binqty * v-cost-ea).
  end.
end.

RUN cost-from-vars.
END PROCEDURE.

PROCEDURE cost-from-vars.
if v-qty ne 0 or ll-one-empty-bin then do:
  v-cost[1] = v-cost[1] / v-qty.

  if fg-ctrl.inv-meth eq "A" then
    assign
     v-cost[3] = v-cost[3] / v-qty
     v-cost[4] = v-cost[4] / v-qty
     v-cost[5] = v-cost[5] / v-qty
     v-cost[6] = v-cost[6] / v-qty.

  if tt-itemfg.prod-uom eq "EA" then
    tt-itemfg.avg-cost = v-cost[1].
  else
    run sys/ref/convcuom.p("EA", tt-itemfg.prod-uom, 0, 0, 0, 0,
                           v-cost[1], output tt-itemfg.avg-cost).

  if tt-itemfg.prod-uom eq "EA" then
    tt-itemfg.last-cost = v-cost[2].
  else
    run sys/ref/convcuom.p("EA", tt-itemfg.prod-uom, 0, 0, 0, 0,
                           v-cost[2], output tt-itemfg.last-cost).

  if tt-itemfg.prod-uom eq "EA" then
    tt-itemfg.std-mat-cost = v-cost[3].
  else
    run sys/ref/convcuom.p("EA", tt-itemfg.prod-uom, 0, 0, 0, 0,
                           v-cost[3], output tt-itemfg.std-mat-cost).

  if tt-itemfg.prod-uom eq "EA" then
    tt-itemfg.std-lab-cost = v-cost[4].
  else
    run sys/ref/convcuom.p("EA", tt-itemfg.prod-uom, 0, 0, 0, 0,
                           v-cost[4], output tt-itemfg.std-lab-cost).

  if tt-itemfg.prod-uom eq "EA" then
    tt-itemfg.std-var-cost = v-cost[5].
  else
    run sys/ref/convcuom.p("EA", tt-itemfg.prod-uom, 0, 0, 0, 0,
                           v-cost[5], output tt-itemfg.std-var-cost).

  if tt-itemfg.prod-uom eq "EA" then
    tt-itemfg.std-fix-cost = v-cost[6].
  else
    run sys/ref/convcuom.p("EA", tt-itemfg.prod-uom, 0, 0, 0, 0,
                           v-cost[6], output tt-itemfg.std-fix-cost).

  assign
   tt-itemfg.std-tot-cost   = tt-itemfg.std-mat-cost + tt-itemfg.std-lab-cost +
                              tt-itemfg.std-var-cost + tt-itemfg.std-fix-cost
   tt-itemfg.total-std-cost = tt-itemfg.std-tot-cost.
end.

if tt-itemfg.avg-cost       eq ? then tt-itemfg.avg-cost       = 0.
if tt-itemfg.last-cost      eq ? then tt-itemfg.last-cost      = 0.
if tt-itemfg.std-mat-cost   eq ? then tt-itemfg.std-mat-cost   = 0.
if tt-itemfg.std-lab-cost   eq ? then tt-itemfg.std-lab-cost   = 0.
if tt-itemfg.std-var-cost   eq ? then tt-itemfg.std-var-cost   = 0.
if tt-itemfg.std-fix-cost   eq ? then tt-itemfg.std-fix-cost   = 0.
if tt-itemfg.std-tot-cost   eq ? then tt-itemfg.std-tot-cost   = 0.
if tt-itemfg.total-std-cost eq ? then tt-itemfg.total-std-cost = 0.
END PROCEDURE.

PROCEDURE create-temp-fg.
  FOR EACH tt-itemfg:
    DELETE tt-itemfg.
  END.

  FOR EACH tt-fg-bin:
    DELETE tt-fg-bin.
  END.

  CREATE tt-itemfg.
  BUFFER-COPY b-itemfg TO tt-itemfg
  ASSIGN
   tt-itemfg.avg-cost       = 0
   tt-itemfg.last-cost      = 0
   tt-itemfg.std-mat-cost   = 0
   tt-itemfg.std-lab-cost   = 0
   tt-itemfg.std-var-cost   = 0
   tt-itemfg.std-fix-cost   = 0
   tt-itemfg.std-tot-cost   = 0
   tt-itemfg.total-std-cost = 0.
END PROCEDURE.

/* end ---------------------------------- copr. 2004  Advanced Software, Inc. */
