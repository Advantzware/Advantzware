/* ---------------------------------------------- ce/box/pr42-mch.p 07/96 JLF */
/* Machine Operations for setup boxes                                         */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
def new shared buffer xop for est-op.

{ce/print4.i shared shared}

DEF SHARED VAR qty AS INT NO-UNDO.

def var v-on-f as DEC NO-UNDO.
def var v-factor as dec NO-UNDO.
def var v-lab-fac as dec extent 2 NO-UNDO.
def var v-fix-fac as dec extent 2 NO-UNDO.
def var v-var-fac as dec extent 2 NO-UNDO.
def var v-num-up as int NO-UNDO.
DEF VAR v-len LIKE xef.gsh-len NO-UNDO.
DEF VAR li-die AS INT NO-UNDO.
DEF VAR v-gu-out AS INT INIT 1 NO-UNDO.
DEF VAR v-printed-lit AS LOG NO-UNDO.

{sys/inc/ceprice.i}

   ASSIGN
     opsplit$ = 0
     v-printed-lit = CAN-FIND(FIRST prodl WHERE prodl.company EQ cocode AND
                     prodl.prolin EQ 'Printed' AND
                     prodl.procat EQ xeb.procat).

   for each est-op
       where est-op.company eq xest.company
         and est-op.est-no  eq xest.est-no
         and est-op.line    gt 500,

       first mach
       {sys/look/machW.i}
         and mach.m-code eq est-op.m-code
       no-lock,

       first xef
       where xef.company eq est-op.company
         and xef.est-no  eq est-op.est-no
         and xef.form-no eq est-op.s-num
       no-lock

       break by est-op.s-num by est-op.b-num by est-op.d-seq by est-op.op-pass
       with frame ae down no-labels no-box:

     find first xeb
         where xeb.company  eq est-op.company
           and xeb.est-no   eq est-op.est-no
           and xeb.form-no  eq est-op.s-num
	       and xeb.blank-no eq est-op.b-num
	     no-lock no-error.
     if not avail xeb then
     find first xeb
         where xeb.company  eq est-op.company
           and xeb.est-no   eq est-op.est-no
           and xeb.form-no  eq est-op.s-num
	     no-lock.

      run sys/inc/numup.p (xef.company, xef.est-no, xef.form-no, output v-num-up).

      run sys/inc/numout.p (recid(est-op), output v-on-f).

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

      v-len = IF est-op.dept EQ "LM" THEN xef.nsh-len ELSE xef.gsh-len.

      if est-op.op-speed ne 0 then
      DO:
         if mach.therm and (mach.p-type eq "R" OR est-op.dept EQ "LM") then
	        oprun = (((est-op.num-sh * v-on-f * v-gu-out) - est-op.op-waste) *
	                (v-len / 12)) / est-op.op-speed.
	     else
	     if est-op.op-sb = true then
	        oprun = ((est-op.num-sh * v-on-f * v-gu-out) - est-op.op-waste) /
	                est-op.op-speed.
	     ELSE
            IF NOT v-printed-lit OR v-gu-out EQ 1 THEN
	           oprun = ((est-op.num-sh *
	                     v-num-up *
	                    (if xef.n-out   eq 0 then 1 else xef.n-out) *
	                    (if xef.n-out-l eq 0 then 1 else xef.n-out-l)) -
	                    est-op.op-waste) / est-op.op-speed.
         ELSE
            oprun = ((est-op.num-sh * v-gu-out) - est-op.op-waste) / est-op.op-speed.
	  end.
      else oprun = 0.

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

    if est-op.b-num ne 0 then do:
	find first blk where blk.snum = est-op.s-num and
			     blk.bnum = est-op.b-num no-error.
	blk.lab  = blk.lab  + optot$. blk.cost = blk.cost + optot$.
	find first xjob
	    where xjob.i-no eq blk.id
	      and xjob.qty  eq qtty[vmcl].
	assign
	xjob.lab = xjob.lab +
	    (est-op.op-mr * (est-op.op-crew[1] * mach.lab-rate[lab-drate])) +
	    (oprun        * (est-op.op-crew[2] * mach.lab-rate[lab-drate]))
	xjob.voh = xjob.voh + v-var-fac[1] + v-var-fac[2]
	xjob.foh = xjob.foh + v-fix-fac[1] + v-fix-fac[2].
      end.
      else
      for each blk where blk.snum = est-op.s-num:
     ASSIGN
	 blk.lab  = blk.lab  + (optot$ * blk.pct)
	 blk.cost = blk.cost + (optot$ * blk.pct).
	 find first xjob
	     where xjob.i-no eq blk.id
	       and xjob.qty  eq qtty[vmcl].
	 assign
	 xjob.lab = xjob.lab +
	   (((est-op.op-mr * (est-op.op-crew[1] * mach.lab-rate[lab-drate]))  +
	     (oprun        * (est-op.op-crew[2] * mach.lab-rate[lab-drate])))
	    * blk.pct)
	 xjob.voh = xjob.voh + ((v-var-fac[1] + v-var-fac[2]) * blk.pct)
	 xjob.foh = xjob.foh + ((v-fix-fac[1] + v-fix-fac[2]) * blk.pct).
      end.

      if est-op.m-code ne "" then do:
	 find itemfg where itemfg.company = cocode and
			   itemfg.i-no    = xeb.stock-no
			   no-lock no-error.
	 create OP.
	 assign OP.blank-no = est-op.b-num
		OP.dept     = est-op.dept
		OP.form-no   = est-op.s-num
		OP.i-no     = xeb.stock-no
		OP.line     = est-op.line
		OP.m-code   = est-op.m-code
		OP.mr-fixoh = mach.mr-fixoh
		OP.mr-hr    = est-op.op-mr
		OP.mr-rate  = mach.mr-rate
		OP.mr-varoh = mach.mr-varoh
		OP.mr-waste = est-op.op-waste
		OP.pass     = est-op.op-pass
		OP.run-hr   = oprun
		OP.wst-prct = est-op.op-spoil
		OP.speed    = est-op.op-speed.
	 if est-op.op-sb then
	   OP.run-qty = est-op.num-sh * v-on-f.
	 else
	   OP.run-qty = est-op.num-sh * v-num-up *
			  (if xef.n-out   eq 0 then 1 else xef.n-out) *
			  (if xef.n-out-l eq 0 then 1 else xef.n-out-l).
	 if available itemfg and est-op.b-num <> 0 then
	    OP.i-name = itemfg.i-name.
	 if OP.line >= 500 then
	    OP.line = OP.line - 500.
      end.

      if est-op.m-code ne "" then display
	      string(est-op.s-num,"9") + "-" +
	      string(est-op.b-num,"99") format "x(4)"
	      est-op.m-dscr   format "x(17)"
	      est-op.op-mr    format ">>9.99"
	      oprun           format ">>9.99"  to 35
	      est-op.op-speed format ">>>>>9"   to 42
	      est-op.op-rate[2] format ">>9.99"  to 50
	      opmr$           format ">>>>9.99" to 59
	      oprun$          format ">>>>>9.99" to 69
	      optot$          format ">>>>,>>9.99" to 80 skip WITH STREAM-IO.

      ASSIGN
      op-tot[1] = op-tot[1] + est-op.op-mr
      op-tot[2] = op-tot[2] + oprun
      op-tot[3] = op-tot[3] + opmr$
      op-tot[4] = op-tot[4] + oprun$
      op-tot[5] = op-tot[5] + optot$
      r-spoil = r-spoil + (est-op.num-sh * (est-op.op-spoil / 100))
      op-tot[6] = op-tot[6] + v-var-fac[1] + v-var-fac[2]
      op-tot[7] = op-tot[7] + v-fix-fac[1] + v-fix-fac[2].
   end.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
