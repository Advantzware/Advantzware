/* ----------------------------------------------- ce/tan/pr4-mch.p 01/98 JLF */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
def shared buffer xop for est-op.

{ce/print4.i shared shared}

def var v-on-f as DEC NO-UNDO.
def var v-factor as dec NO-UNDO.
def var v-lab-fac as dec extent 2 NO-UNDO.
def var v-fix-fac as dec extent 2 NO-UNDO.
def var v-var-fac as dec extent 2 NO-UNDO.
def var v-num-up as int NO-UNDO.
DEF VAR v-len LIKE xef.gsh-len NO-UNDO.

DEF BUFFER bf-eb FOR eb.


{sys/inc/ceprice.i}

  opsplit$ = 0.

  for each est-op where est-op.company = xest.company
                    AND est-op.est-no = xest.est-no and est-op.line gt 500
      by est-op.d-seq by est-op.b-num by est-op.op-pass
      with frame ae down no-labels no-box:
    find first mach {sys/look/mach.w} and mach.m-code = est-op.m-code
      no-lock no-error.
    if not avail mach then next.

    run sys/inc/numout.p (recid(est-op), output v-on-f).

    v-len = IF est-op.dept EQ "LM" THEN xef.nsh-len ELSE xef.gsh-len.

    if est-op.op-speed ne 0 then
      if mach.therm and (mach.p-type eq "R" OR est-op.dept EQ "LM") then
	oprun = (((est-op.num-sh * v-on-f) - est-op.op-waste) *
		 (v-len / 12)) / est-op.op-speed.
      else if est-op.op-sb = true then
	oprun = ((est-op.num-sh * v-on-f) - est-op.op-waste) / est-op.op-speed.
      else
	oprun = ((est-op.num-sh *
		  xeb.num-up * (if xef.n-out   eq 0 then 1 else xef.n-out) *
			       (if xef.n-out-l eq 0 then 1 else xef.n-out-l)) -
		 est-op.op-waste) / est-op.op-speed.
    else oprun = 0.

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

    find first bf-eb where bf-eb.company = xest.company
                       AND bf-eb.est-no    = xest.est-no   and
			bf-eb.form-no  = est-op.s-num and
			bf-eb.blank-no = est-op.b-num no-lock no-error.
    create op.
    assign op.blank-no = est-op.b-num
	   op.dept     = est-op.dept
	   op.form-no   = est-op.s-num
	   op.i-no     = if avail bf-eb then bf-eb.stock-no else ""
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
    find first itemfg
	where itemfg.company eq cocode
	  and itemfg.i-no    eq op.i-no
	no-lock no-error.
    if avail itemfg and est-op.b-num ne 0 then
       op.i-name = itemfg.i-name.
    if op.line ge 500 then
       op.line = op.line - 500.

    if est-op.m-code ne "" then
    display
      string(est-op.b-num,"99") format "x(2)" when est-op.b-num ne 0
      est-op.m-dscr   format "x(17)"
      est-op.op-mr    format ">>9.99"
      oprun           format ">>>9.99"     TO 35
      est-op.op-speed format ">>>>>9"       TO 42
      est-op.op-rate[2] format ">>9.99"      TO 49
      opmr$           format ">>>>9.99"    TO 58
      oprun$          format ">>>>>9.99"   TO 68
      optot$          format ">,>>>,>>9.99" TO 80 SKIP WITH STREAM-IO.

    op-tot[1] = op-tot[1] + est-op.op-mr.
    op-tot[2] = op-tot[2] + oprun.
    op-tot[3] = op-tot[3] + opmr$.
    op-tot[4] = op-tot[4] + oprun$.
    op-tot[5] = op-tot[5] + optot$.

    assign
     op-tot[6] = op-tot[6] + v-var-fac[1] + v-var-fac[2]
     op-tot[7] = op-tot[7] + v-fix-fac[1] + v-fix-fac[2]
     r-spoil   = r-spoil + (est-op.num-sh * (est-op.op-spoil / 100)).
  end.

/* end ---------------------------------- copr. 1998  advanced software, inc. */
