/* --------------------------------------------- cec/box/pr42-mch.p 07/96 JLF */
/* Machine Operations for setup boxes                                         */
/* -------------------------------------------------------------------------- */

def shared var cocode as cha no-undo.
def shared var locode as cha no-undo.
def shared var k as int no-undo.
def var xxx as int no-undo.
def shared var qty as int NO-UNDO.
def var zzz as int no-undo.
def var i as int no-undo.
def var j as int no-undo.

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
def shared buffer xop for est-op.

{cec/print4.i shared shared}
{cec/print42.i shared}

def buffer xcar for car.
DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-qty FOR reftable.

def var v-num-up as int NO-UNDO.
def var v-num-in like est-op.num-sh NO-UNDO.
def var vn-out like xef.n-out-l init 1 NO-UNDO.
def var v-outw like xef.n-out NO-UNDO.
def var v-outl like xef.n-out-l NO-UNDO.
def var v-outf as dec NO-UNDO.
def var v-on-f as dec NO-UNDO.
def var v-on-l as dec NO-UNDO.
def var sh-tmp like sh-len NO-UNDO.
def var v-widp as log NO-UNDO.
def var v-factor as dec NO-UNDO.
def var v-lab-fac as dec extent 2 NO-UNDO.
def var v-fix-fac as dec extent 2 NO-UNDO.
def var v-var-fac as dec extent 2 NO-UNDO.
def var v-yld as dec NO-UNDO.
def var v-msf as dec NO-UNDO.
DEF VAR v-rm$ AS DEC NO-UNDO.
def var v-on-s as DEC NO-UNDO.
DEF VAR v-len LIKE xef.gsh-len NO-UNDO.
DEF VAR ll-unitize AS LOG NO-UNDO.
DEF VAR v-n-out AS INT NO-UNDO.
DEF VAR li-die AS INT NO-UNDO.
DEF VAR v-parts AS DEC NO-UNDO.
DEF VAR ld-hand-pct AS DEC NO-UNDO.
DEF VAR opmr LIKE est-op.op-mr NO-UNDO.
DEF VAR opsp AS DEC NO-UNDO.

DEF TEMP-TABLE tt-ei NO-UNDO
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20.

DEF SHARED WORKFILE w-form
    FIELD form-no LIKE xef.form-no
    FIELD min-msf AS   LOG.

{cec/msfcalc.i}

{sys/inc/ceprice.i}


v-parts = 0.
FOR EACH eb FIELDS(yld-qty) NO-LOCK
    WHERE eb.company EQ xest.company
      AND eb.est-no  EQ xest.est-no
      AND eb.form-no NE 0:
  v-parts = v-parts +
            (IF eb.yld-qty LT 0 THEN (-1 / eb.yld-qty) ELSE eb.yld-qty).
END.

/* machines */
for each xef
    where xef.company = xest.company
      and xef.est-no eq xest.est-no
      and (xef.form-no eq v-form-no or (not vmclean2)):

  find first w-form where w-form.form-no eq xef.form-no no-error.
  if not avail w-form then do:
    create w-form.
    w-form.form-no = xef.form-no.
  end.

  for each est-op
      where est-op.company eq xest.company
        and est-op.est-no  eq xest.est-no
        and est-op.qty     eq v-op-qty
        and est-op.s-num   eq xef.form-no
        and est-op.line    gt 500
      by est-op.b-num by est-op.d-seq by est-op.op-pass
      with frame ae down no-labels no-box  stream-io:

    find first mach
        {sys/look/machW.i}
          and mach.m-code eq est-op.m-code
        no-lock no-error.
    if not avail mach then next.

    find first eb
        where eb.company   eq est-op.company
          and eb.est-no    eq est-op.est-no
          and eb.form-no   eq est-op.s-num
          and (eb.blank-no eq est-op.b-num or est-op.b-num eq 0)
        no-lock no-error.

    IF INDEX("AP",mach.p-type) GT 0 THEN
      ASSIGN
       v-num-up = 1
       v-n-out  = 1
       v-on-s   = 1
       v-on-f   = 1.

    ELSE DO:
      RUN sys/inc/numup.p (xef.company,xef.est-no, xef.form-no, OUTPUT v-num-up).
      RUN est/ef-#out.p (ROWID(xef), OUTPUT v-n-out).
      RUN sys/inc/numout.p (RECID(est-op), OUTPUT v-on-f).
    END.

    IF est-op.dept EQ "DC" AND est-op.n-out GT 0 THEN DO:
      FIND FIRST ef-nsh OF xef
          WHERE ef-nsh.pass-no EQ est-op.op-pass
            AND ef-nsh.dept    EQ est-op.dept
          NO-LOCK NO-ERROR.
      IF AVAIL ef-nsh THEN DO:
        RUN cec/foamplus.p (ROWID(ef-nsh), OUTPUT li-die).
        v-on-f = v-on-f * (est-op.n-out + INT(li-die GT 0)).
      END.
    END.

    ASSIGN
     v-on-s = v-num-up * v-n-out
     v-len  = IF est-op.dept EQ "LM" THEN xef.nsh-len ELSE xef.gsh-len.

    ll-unitize = NO.
    FOR EACH mstd
        WHERE mstd.company eq mach.company
          AND mstd.loc     eq mach.loc
          AND mstd.m-code  eq mach.m-code
        NO-LOCK
        BREAK BY mstd.style DESC:
      IF LAST(mstd.style)        OR
         mstd.style EQ eb.style THEN DO:
        ll-unitize = mstd.rs-x EQ 98 OR mstd.rs-y EQ 98.
        LEAVE.
      END.
    END.

    ASSIGN
     opsp = est-op.op-speed
     opmr = est-op.op-mr.

    IF opsp NE 0 THEN
      IF ll-unitize THEN DO:
        FOR EACH cas
            WHERE cas.snum EQ eb.form-no
              AND cas.bnum EQ eb.blank-no
              AND cas.typ  EQ 3
            NO-LOCK:
          ACCUMULATE cas.qty (TOTAL).
        END.
        oprun = (ACCUM TOTAL cas.qty) / opsp.
      END.
      ELSE
      IF mach.p-type EQ "P" THEN
        oprun = (est-op.num-sh - est-op.op-waste) * v-parts / opsp.
      ELSE
      IF mach.therm                                  AND
         mach.p-type NE "A"                          AND
         (mach.p-type EQ "R" OR est-op.dept EQ "LM") THEN
        oprun = (((est-op.num-sh * v-on-f) - est-op.op-waste) *
                 (v-len / 12)) / opsp.
      ELSE
      IF est-op.op-sb THEN
        oprun = ((est-op.num-sh * v-on-f) - est-op.op-waste) / opsp.
      ELSE
        oprun = ((est-op.num-sh * v-on-s) - est-op.op-waste) / opsp.
    ELSE oprun = 0.

    IF w-form.min-msf AND mach.dept[1] EQ "RC" THEN
      ASSIGN
       opmr$     = 0
       oprun$    = 0
       optot$    = 0
       v-lab-fac = 0
       v-var-fac = 0
       v-fix-fac = 0.

    ELSE DO:
      ASSIGN
       opmr$        = opmr  * est-op.op-rate[1]
       oprun$       = oprun * est-op.op-rate[2]
       optot$       = opmr$ + oprun$

       v-lab-fac[1] = opmr  * est-op.op-crew[1] * mach.lab-rate[lab-drate]
       v-lab-fac[2] = oprun * est-op.op-crew[2] * mach.lab-rate[lab-drate]

       v-var-fac[1] = opmr  * mach.mr-varoh
       v-var-fac[2] = oprun * mach.run-varoh

       v-fix-fac[1] = opmr  * mach.mr-fixoh
       v-fix-fac[2] = oprun * mach.run-fixoh.

      IF v-min-mchg THEN DO:
        IF ceprice-chr EQ "MR+Run" AND mach.mrk-rate GT optot$ THEN
          ASSIGN
           v-factor = (mach.mrk-rate / optot$)
           opmr$    = opmr$  * v-factor
           oprun$   = oprun$ * v-factor.

        ELSE
        IF ceprice-chr EQ "RunOnly" AND mach.mrk-rate GT oprun$ THEN
          oprun$ = mach.mrk-rate.

        ASSIGN
         v-factor     = optot$
         optot$       = oprun$ + opmr$
         v-factor     = optot$ / v-factor.

        IF v-factor EQ ? THEN v-factor = 0.

        ASSIGN
         v-lab-fac[1] = v-lab-fac[1] * v-factor
         v-lab-fac[2] = v-lab-fac[2] * v-factor
         v-var-fac[1] = v-var-fac[1] * v-factor
         v-var-fac[2] = v-var-fac[2] * v-factor
         v-fix-fac[1] = v-fix-fac[1] * v-factor
         v-fix-fac[2] = v-fix-fac[2] * v-factor.
      END.

      ASSIGN
       opsplit$[1] = opsplit$[1] + v-lab-fac[1] + v-lab-fac[2]
       opsplit$[2] = opsplit$[2] + v-var-fac[1] + v-var-fac[2]
       opsplit$[3] = opsplit$[3] + v-fix-fac[1] + v-fix-fac[2].

      if est-op.b-num ne 0 then do:
        find first blk
            where blk.snum eq est-op.s-num
              and blk.bnum eq est-op.b-num
            no-error.
        assign
         blk.lab  = blk.lab  + optot$
         blk.cost = blk.cost + optot$.

        find first xjob
            where xjob.i-no eq blk.id
              and xjob.qty  eq blk.qreq.
/*
              and xjob.qty  eq qtty[vmcl].
*/
        assign
         xjob.lab = xjob.lab + v-lab-fac[1] + v-lab-fac[2]
         xjob.voh = xjob.voh + v-var-fac[1] + v-var-fac[2]
         xjob.foh = xjob.foh + v-fix-fac[1] + v-fix-fac[2].
      end.

      else
      for each blk where blk.snum eq est-op.s-num:
        assign
         blk.lab  = blk.lab  + (optot$ * blk.pct)
         blk.cost = blk.cost + (optot$ * blk.pct).
        find first xjob
            where xjob.i-no eq blk.id
              and xjob.qty  eq blk.qreq.
/*
              and xjob.qty  eq qtty[vmcl].
*/
        assign
         xjob.lab = xjob.lab + ((v-lab-fac[1] + v-lab-fac[2]) * blk.pct)
         xjob.voh = xjob.voh + ((v-var-fac[1] + v-var-fac[2]) * blk.pct)
         xjob.foh = xjob.foh + ((v-fix-fac[1] + v-fix-fac[2]) * blk.pct).
      end.
    end.

    if est-op.m-code ne "" then do:
      create op.
      assign
       op.blank-no = est-op.b-num
       op.dept     = est-op.dept
       op.form-no  = est-op.s-num
       op.i-no     = eb.stock-no
       op.line     = est-op.line
       op.m-code   = est-op.m-code
       op.mr-fixoh = mach.mr-fixoh
       op.mr-hr    = opmr
       op.mr-rate  = mach.mr-rate
       op.mr-varoh = mach.mr-varoh
       op.mr-waste = est-op.op-waste
       op.pass     = est-op.op-pass
       op.run-hr   = oprun
       op.wst-prct = est-op.op-spoil.

      IF mach.p-type EQ "P" THEN DO:
        op.run-qty = est-op.num-sh * v-parts.
        {sys/inc/roundup.i op.run-qty}
      END.
      ELSE
      IF est-op.op-sb THEN
        op.run-qty = est-op.num-sh * v-on-f.
      ELSE
        op.run-qty = est-op.num-sh * v-num-up * v-n-out.
            
      op.speed = IF ll-unitize THEN (op.run-qty / oprun)
                               ELSE opsp.

      v-yld = if eb.yld-qty lt 0 then -1 / eb.yld-qty else eb.yld-qty.

      if vmclean then oprun$ = oprun$ / ((qtty[vmcl] * v-yld) / 1000).

      display string(est-op.s-num,"99") + "-" +
              string(est-op.b-num,"9") format "x(4)"
              est-op.m-dscr     format "x(13)"
              est-op.n-out      format ">>>9"
                   v-on-s / v-on-f WHEN est-op.n-out EQ 0 @ est-op.n-out
                   1 WHEN NOT est-op.op-sb                @ est-op.n-out
              opmr              format ">>9.99"
              oprun             format ">>9.99"      to 36
              opsp              format ">>>>9"       to 42
              est-op.op-rate[2] format ">>>9.99"     to 51
              opmr$             format ">>>>9.99"    to 60
              oprun$            format ">>>>9.99"    to 69
              optot$            format ">>>>>>9.99" to 80 SKIP WITH STREAM-IO.

      if vmclean then oprun$ = oprun$ * ((qtty[vmcl] * v-yld) / 1000).

      if avail itemfg and est-op.b-num ne 0 then op.i-name = itemfg.i-name.
      if op.line gt 500 then op.line = op.line - 500.
    end.

    assign
     op-tot[1] = op-tot[1] + opmr
     op-tot[2] = op-tot[2] + oprun
     op-tot[3] = op-tot[3] + opmr$
     op-tot[4] = op-tot[4] + oprun$
     op-tot[5] = op-tot[5] + optot$.

    r-spoil = r-spoil + (est-op.num-sh * (est-op.op-spoil / 100)).

    assign
     op-tot[6] = op-tot[6] + v-var-fac[1] + v-var-fac[2]
     op-tot[7] = op-tot[7] + v-fix-fac[1] + v-fix-fac[2].
  end.
end.

ctrl2[2] = 0.

FOR EACH blk WHERE blk.snum EQ v-form-no OR NOT vmclean2:
  find first xjob
      where xjob.i-no eq blk.id
        and xjob.qty  eq blk.qreq.

  ld-hand-pct = IF blk.pur-man THEN hand-pct-f ELSE ctrl[2].

  IF ld-hand-pct NE 0 THEN
    ASSIGN
     v-rm$    = xjob.mat * ld-hand-pct
     blk.cost = blk.cost + v-rm$
     blk.lab  = blk.lab  + v-rm$
     ctrl2[2] = ctrl2[2] + v-rm$
     xjob.lab = xjob.lab + v-rm$.
END.

if ctrl2[2] + ctrl2[3] ne 0 then do:
  put "Raw Mat'l Handling" (ctrl2[2] + ctrl2[3]) to 80 skip.
  op-tot[5] = op-tot[5] + (ctrl2[2] + ctrl2[3]).
end.

fr-tot = 0.

for each xef
    where xef.company = xest.company
      and xef.est-no eq xest.est-no
      and (xef.form-no eq v-form-no or (not vmclean2)):

  release item.
  release e-item.

  if xef.form-no eq 1 then do:
    find first xeb of xef no-lock.
    find first item
        {sys/look/itemW.i}
          and item.i-no eq xeb.tr-no
        no-lock no-error.
    if avail item then
      find first e-item of item
          no-lock no-error.
  end.

  if avail e-item and item.mat-type eq "Z" then do:
    find first xeb
        where xeb.company  eq xest.company
          and xeb.est-no   eq xest.est-no
          and xeb.form-no  eq 0
          and xeb.blank-no eq 0
        no-lock.

    assign
     v-yld  = if xeb.yld-qty lt 0 then -1 / xeb.yld-qty else xeb.yld-qty
     tr-tot = ((xeb.len * xeb.wid * xeb.dep) *
               (qtty[vmcl] * v-yld)) /
               (item.case-l * item.case-w * item.case-d).

    EMPTY TEMP-TABLE tt-ei.
    CREATE tt-ei.
    DO j = 1 TO 10:
       ASSIGN
          tt-ei.run-qty[j] = e-item.run-qty[j]
          tt-ei.run-cost[j] = e-item.run-cost[j].
    END.

    FIND FIRST b-qty WHERE
         b-qty.reftable = "blank-vend-qty" AND
         b-qty.company = e-item.company AND
         b-qty.CODE    = e-item.i-no
         NO-LOCK NO-ERROR.

    IF AVAIL b-qty THEN
    DO:
       FIND FIRST b-cost WHERE
            b-cost.reftable = "blank-vend-cost" AND
            b-cost.company = e-item.company AND
            b-cost.CODE    = e-item.i-no
            NO-LOCK NO-ERROR.

       DO j = 1 TO 10:
          ASSIGN
             tt-ei.run-qty[j + 10] = b-qty.val[j]
             tt-ei.run-cost[j + 10] = b-cost.val[j].
       END.
    END.


    do j = 1 to 20:
      if tt-ei.run-qty[j] < tr-tot then next.
      fr-tot = round(tr-tot * tt-ei.run-cost[j],2).
      leave.
    end.
  end.

  else
  for each xeb
      where xeb.company eq xest.company
        and xeb.est-no  eq xest.est-no
        and xeb.form-no eq xef.form-no
      no-lock:
        
    v-yld  = if xeb.yld-qty lt 0 then -1 / xeb.yld-qty else xeb.yld-qty.
    
    find first carrier
        where carrier.company eq cocode
          and carrier.loc     eq locode
          and carrier.carrier eq xeb.carrier
        no-lock no-error.
    if avail carrier then
         find first carr-mtx 
        where carr-mtx.company  eq cocode
          and carr-mtx.loc      eq locode
          and carr-mtx.carrier  eq carrier.carrier
          and carr-mtx.del-zone eq xeb.dest-code
          AND carr-mtx.del-zip EQ xeb.ship-zip
        no-lock no-error.
    IF NOT AVAIL carr-mtx THEN 
       find first carr-mtx
        where carr-mtx.company  eq cocode
          and carr-mtx.loc      eq locode
          and carr-mtx.carrier  eq carrier.carrier
          and carr-mtx.del-zone eq xeb.dest-code
        no-lock no-error.
    find first car where car.id eq xeb.part-no no-error.
    if not avail car then do:
      create car.
      assign
       car.carrier = carrier.carrier
       car.dscr    = carr-mtx.del-zone
       car.id      = xeb.part-no
       car.snum    = xeb.form-no
       car.bnum    = xeb.blank-no.
    end.
    v-msf = (if v-corr then
               ((xeb.t-sqin - xeb.t-win) * qtty[vmcl] * v-yld * .007)
             else
               ((xeb.t-sqin - xeb.t-win) * qtty[vmcl] * v-yld / 144))
            / 1000.
                           /* now msf */
    zzz = xef.weight.
    if xef.medium ne "" then do:
      find first item
          {sys/look/itemW.i}
            and item.i-no eq xef.medium
          no-lock no-error.
      if avail item then
        zzz = zzz + (item.basis-w * (1 - (item.shrink / 100))).
    end.
    if xef.flute ne "" then do:
      find first item
          {sys/look/itemW.i}
            and item.i-no eq xef.flute
          no-lock no-error.
      if avail item then
        zzz = zzz + item.basis-w.
    end.
    /*if xef.lam-code ne "" then do:
      find first item
          {sys/look/itemW.i}
            and item.i-no eq xef.lam-code
          no-lock no-error.
      if avail item then
        zzz = zzz + item.basis-w.
    end.*/
    assign
     car.qty = car.qty + (v-msf * zzz)    /* total weight */
     car.msf = car.msf + v-msf.

    /* add pallet & case for total weight */
    for each cas where cas.id eq xeb.part-no,
        first item
        where item.company eq cocode
          and item.i-no    eq cas.ino
          and index("DC",item.mat-type) gt 0
        no-lock:
      car.qty = car.qty + (cas.qty * item.basis-w).
    end.
  end.

  IF CAN-FIND(FIRST cas WHERE cas.typ EQ 1 AND cas.snum EQ 0) THEN
  FOR EACH xeb
      WHERE xeb.company EQ xest.company
        AND xeb.est-no  EQ xest.est-no
        AND xeb.form-no EQ 0
      NO-LOCK:
 
    FIND FIRST car WHERE car.snum EQ 0 NO-ERROR.
    IF NOT AVAIL car THEN DO:
      CREATE car.
      ASSIGN
       car.carrier = carrier.carrier
       car.dscr    = carr-mtx.del-zone
       car.id      = xeb.part-no
       car.snum    = xeb.form-no
       car.bnum    = xeb.blank-no.
    END.

    FOR EACH xcar WHERE ROWID(xcar) NE ROWID(car):
      ASSIGN
       car.qty = car.qty + xcar.qty
       car.msf = car.msf + xcar.msf.
    END.

    LEAVE.
  END.
end.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
