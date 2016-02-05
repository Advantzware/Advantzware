/* -------------------------------------------------- cec/probeu2ip 03/97 JLF */
/* 'What if' UPDATE                                                           */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
def shared buffer xpro for probe.

{ce/print4.i shared shared}
{ce/print42.i shared}

DEF BUFFER b-probemk FOR reftable.
DEF BUFFER probe-ref FOR reftable.

def var qm as de.
def var v as int.
def var v-comm like tt-tot.
def var v-yld as dec.
def var v-skip-pct as log.
DEF VAR lv-com AS DEC NO-UNDO.
DEF VAR v-probe-fmt AS CHAR NO-UNDO.

def shared var tab-inout as ch format "X".
def shared var head as ch format "x(78)" extent 20.

def shared frame est.

{sys/form/s-top.f}
{ce/est.f &fil=xest &fil2=xef &fil3=xeb}


{sys/inc/cerun.i F}
{ce/msfcalc.i}

{cec/combasis.i}

lv-com = v-com.


for each probe where probe.e-num eq xest.e-num by probe.line {1}:
   /*IF probe.comm NE 0 THEN*/ lv-com = probe.comm.

   ASSIGN
    qty   = probe.est-qty
    qm    = qty / 1000
    v-pct = if ce-ctrl.sell-by = "G" then probe.gross-profit
				     else probe.net-profit
    v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999".

   if vmclean2 then
   for each probeit where probeit.e-num eq probe.e-num
		      and probeit.bl-qty eq probe.est-qty no-lock:
      find first blk where blk.id eq probeit.part-no no-error.
      find first eb where eb.e-num    eq xest.e-num
		      and eb.form-no  eq blk.snum
		      and eb.blank-no eq blk.bnum no-lock no-error.

      v-form-no = blk.snum.

      if opsys eq "MSDOS" then
	 dos silent copy value(tmp-dir + string(xest.e-num,"99999") + "-"  +
					 string(v-form-no,"99")     + ".v" +
					 string(probe.line,v-probe-fmt))
			 value(tmp-dir + string(xest.e-num,"99999") + "-"  +
					 string(v-form-no,"99")     + ".s" +
					 string(probe.line,v-probe-fmt)).
      else if opsys eq "unix" then
	 unix silent copy value(tmp-dir + string(xest.e-num,"99999") + "-"  +
					  string(v-form-no,"99")     + ".v" +
					  string(probe.line,v-probe-fmt))
			  value(tmp-dir + string(xest.e-num,"99999") + "-"  +
					  string(v-form-no,"99")     + ".s" +
					  string(probe.line,v-probe-fmt)).

      output to value(tmp-dir + string(xest.e-num,"99999") + "-"  +
				string(v-form-no,"99")     + ".s" +
				string(probe.line,v-probe-fmt)) append.

      ASSIGN
         vmcl = probe.LINE
         v-com = lv-com.

      {ce/probepr1.i 50}

      if vmclean then do:
	 {ce/pr4-mcl.i "probe.line"}
      end.

      output close.

      v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999".

      if opsys eq "MSDOS" then
	 dos silent type value(tmp-dir + string(xest.e-num,"99999") + "-"  +
					 string(v-form-no,"99")     + ".a" +
					 string(probe.line,v-probe-fmt))            >>
			 value(tmp-dir + string(xest.e-num,"99999") + "-"  +
					 string(v-form-no,"99")     + ".s" +
					 string(probe.line,v-probe-fmt)).
      else if opsys eq "unix" then
	 unix silent cat value(tmp-dir + string(xest.e-num,"99999") + "-"  +
					 string(v-form-no,"99")     + ".a" +
					 string(probe.line,v-probe-fmt))            >>
			 value(tmp-dir + string(xest.e-num,"99999") + "-"  +
					 string(v-form-no,"99")     + ".s" +
					 string(probe.line,v-probe-fmt)).
   end.

   else do:
      
      if xest.est-type eq 1 then do:
	 if opsys eq "MSDOS" then
	   dos silent copy value(tmp-dir + string(xest.e-num,"99999") + "-01.v"
						     + string(probe.line,v-probe-fmt))
			   value(tmp-dir + string(xest.e-num,"99999") + "-01.s"
						     + string(probe.line,v-probe-fmt)).
	 else if opsys eq "unix" then
	   unix silent copy value(tmp-dir + string(xest.e-num,"99999") + "-01.v"
						     + string(probe.line,v-probe-fmt))
			    value(tmp-dir + string(xest.e-num,"99999") + "-01.s"
						    + string(probe.line,v-probe-fmt)).

	 output to value(tmp-dir + string(xest.e-num,"99999") + "-01.s"
					      + string(probe.line,v-probe-fmt)) append.
      end.

      else do:
	 if opsys eq "MSDOS" then
	   dos silent copy value(tmp-dir + string(xest.e-num,"99999999") + ".v"
						     + string(probe.line,v-probe-fmt))
			   value(tmp-dir + string(xest.e-num,"99999999") + ".s"
						     + string(probe.line,v-probe-fmt)).
	 else if opsys eq "unix" then
	   unix silent copy value(tmp-dir + string(xest.e-num,"99999999") + ".v"
						     + string(probe.line,v-probe-fmt))
			    value(tmp-dir + string(xest.e-num,"99999999") + ".s"
						    + string(probe.line,v-probe-fmt)).

	 output to value(tmp-dir + string(xest.e-num,"99999999") + ".s"
					      + string(probe.line,v-probe-fmt)) append.
      end.

      vmcl = probe.line.

      if xest.est-type eq 1 then do:
	 {ce/probepr2.i 50}
      end.

      else do:
	 {ce/probepr2.i 48}
      end.

      if vmclean then do:
	 {ce/pr4-mcl.i "probe.line"}
      end.

      output close.

      v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999".

      if xest.est-type eq 1 then do:
	 if opsys eq "MSDOS" then
	    dos silent type value(tmp-dir + string(xest.e-num,"99999") + "-01.a"
						   + string(probe.line,v-probe-fmt)) >>
			    value(tmp-dir + string(xest.e-num,"99999") + "-01.s"
						   + string(probe.line,v-probe-fmt)).
	 else if opsys eq "unix" then
	    unix silent cat value(tmp-dir + string(xest.e-num,"99999") + "-01.a"
						   + string(probe.line,v-probe-fmt)) >>
			    value(tmp-dir + string(xest.e-num,"99999") + "-01.s"
						   + string(probe.line,v-probe-fmt)).
      end.

      else do:
	 if opsys eq "MSDOS" then
	    dos silent type value(tmp-dir + string(xest.e-num,"99999999") + ".a"
						   + string(probe.line,v-probe-fmt)) >>
			    value(tmp-dir + string(xest.e-num,"99999999") + ".s"
						   + string(probe.line,v-probe-fmt)).
	 else if opsys eq "unix" then
	    unix silent cat value(tmp-dir + string(xest.e-num,"99999999") + ".a"
						   + string(probe.line,v-probe-fmt)) >>
			    value(tmp-dir + string(xest.e-num,"99999999") + ".s"
						   + string(probe.line,v-probe-fmt)).
      end.
   end.
end.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
