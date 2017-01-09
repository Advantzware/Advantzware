/* --------------------------------------------------- ce/pr4-mch.p 02/98 JLF */
/* Machine Operations                                                         */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

def new shared buffer xop for est-op.

DEF SHARED VAR qty AS INT NO-UNDO.

{ce/print4.i shared shared}

def var v-on-f as DEC NO-UNDO.
def var v-factor as dec NO-UNDO.
def var v-lab-fac as dec extent 2 NO-UNDO.
def var v-fix-fac as dec extent 2 NO-UNDO.
def var v-var-fac as dec extent 2 NO-UNDO.
def var v-num-up as int NO-UNDO.
DEF VAR v-len LIKE xef.gsh-len NO-UNDO.
DEF VAR v-gu-out AS INT INIT 1 NO-UNDO.
DEF VAR v-printed-lit AS LOG NO-UNDO.

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

{sys/inc/ceprice.i}

  ASSIGN
     opsplit$ = 0
     v-printed-lit = CAN-FIND(FIRST prodl WHERE prodl.company EQ cocode AND
                     prodl.prolin EQ 'Printed' AND
                     prodl.procat EQ xeb.procat).

  /* machines */
  for each est-op where est-op.company = xest.company 
                    AND est-op.est-no eq xest.est-no
                    and est-op.qty   eq v-op-qty
                    and est-op.line  gt 500
      break by est-op.s-num
            by est-op.b-num
            by est-op.d-seq
            by est-op.op-pass
      with frame ae down no-labels no-box:

    find first mach
        {sys/ref/machW.i}
          and mach.m-code eq est-op.m-code
        no-lock no-error.
    if not avail mach then next.

    run sys/inc/numout.p (recid(est-op), output v-on-f).

    v-len = IF est-op.dept EQ "LM" THEN xef.nsh-len ELSE xef.gsh-len.

    if est-op.op-speed ne 0 then
    DO:
       if mach.therm and (mach.p-type eq "R" OR est-op.dept EQ "LM") then
          oprun = (((est-op.num-sh * v-on-f * v-gu-out) - est-op.op-waste) *
                    (v-len / 12)) / est-op.op-speed.
       else
       if est-op.op-sb then
          oprun = ((est-op.num-sh * v-on-f * v-gu-out) - est-op.op-waste) / est-op.op-speed.
       else
          IF NOT v-printed-lit OR v-gu-out EQ 1 THEN
             oprun = ((est-op.num-sh * 
                      xeb.num-up * (if xef.n-out   eq 0 then 1 else xef.n-out) *
                                   (if xef.n-out-l eq 0 then 1 else xef.n-out-l)) -
                     est-op.op-waste) / est-op.op-speed.
       ELSE
          oprun = ((est-op.num-sh * v-gu-out) - est-op.op-waste) / est-op.op-speed.
    END.
    else
       oprun = 0.

    IF v-printed-lit AND est-op.n-out GT 1 THEN
       v-gu-out = v-gu-out * est-op.n-out.

    assign
     opmr$        = est-op.op-mr * est-op.op-rate[1]
     oprun$       = oprun        * est-op.op-rate[2]
     optot$       = opmr$ + oprun$

     v-lab-fac[1] = est-op.op-mr * est-op.op-crew[1] * mach.lab-rate[lab-drate]
     v-lab-fac[2] = oprun        * est-op.op-crew[2] * mach.lab-rate[lab-drate]

     v-var-fac[1] = est-op.op-mr * mach.mr-varoh
     v-var-fac[2] = oprun        * mach.run-varoh

     v-fix-fac[1] = est-op.op-mr * mach.mr-fixoh
     v-fix-fac[2] = oprun        * mach.run-fixoh.

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

    assign
     opsplit$[1] = opsplit$[1] + v-lab-fac[1] + v-lab-fac[2]
     opsplit$[2] = opsplit$[2] + v-var-fac[1] + v-var-fac[2]
     opsplit$[3] = opsplit$[3] + v-fix-fac[1] + v-fix-fac[2].

    if est-op.m-code ne "" then do:
      find itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq xeb.stock-no
          no-lock no-error.
      create op.
      assign
       op.blank-no = est-op.b-num
       op.dept     = est-op.dept
       op.form-no  = est-op.s-num
       op.i-no     = xeb.stock-no
       op.line     = est-op.line
       op.m-code   = est-op.m-code
       op.mr-fixoh = mach.mr-fixoh
       op.mr-hr    = est-op.op-mr
       op.mr-rate  = mach.mr-rate
       op.mr-varoh = mach.mr-varoh
       op.mr-waste = est-op.op-waste
       op.pass     = est-op.op-pass
       op.run-hr   = oprun
       op.wst-prct = est-op.op-spoil
       op.speed    = est-op.op-speed.
      if est-op.op-sb then
        op.run-qty = est-op.num-sh * v-on-f.
      else
        op.run-qty = est-op.num-sh * xeb.num-up *
                     (if xef.n-out   eq 0 then 1 else xef.n-out) *
                     (if xef.n-out-l eq 0 then 1 else xef.n-out-l).
      if avail itemfg and est-op.b-num ne 0 then op.i-name = itemfg.i-name.
      if op.line ge 500 then op.line = op.line - 500.
    end.

    if est-op.m-code ne "" then
      display est-op.m-dscr   format "x(18)"
              est-op.op-mr    format ">>>9.99"
              oprun           format ">>>>9.99"     to 35
              est-op.op-speed format ">>>>>9"        to 42
              est-op.op-rate[2] format ">>9.99"     to 48 when ce-ctrl.sho-labor
              opmr$           format ">>>>9.99"     to 57
              oprun$          format ">>>>>>9.99"   to 68
              optot$          format ">,>>>,>>9.99" to 80 skip WITH STREAM-IO.

    assign
     op-tot[1] = op-tot[1] + est-op.op-mr
     op-tot[2] = op-tot[2] + oprun
     op-tot[3] = op-tot[3] + opmr$
     op-tot[4] = op-tot[4] + oprun$
     op-tot[5] = op-tot[5] + optot$
     op-tot[6] = op-tot[6] + v-var-fac[1] + v-var-fac[2]
     op-tot[7] = op-tot[7] + v-fix-fac[1] + v-fix-fac[2].
  end.

/* end ---------------------------------- copr. 1998  advanced software, inc. */
