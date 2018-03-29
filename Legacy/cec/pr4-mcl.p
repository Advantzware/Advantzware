/* -------------------------------------------------- cec/pr4-mcl.p 06/96 JLF */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared var tmp-dir as cha no-undo. 
def shared buffer xest for est.
def shared buffer xef for ef.
def shared buffer xeb for eb.

{cec/print4.i shared shared}
{cec/print42.i shared}

def var v as int no-undo.
def var v-yld as dec no-undo.
def var v-skip-pct as log no-undo.
def var ls-outfile as cha no-undo.
DEF VAR v-probe-fmt AS CHAR NO-UNDO.
DEF VAR mclean2yld AS DEC NO-UNDO.

{cec/msfcalc.i}

{cec/rollfac.i}

if vmclean2 then
  find first xeb where xeb.company = xest.company 
                   and xeb.est-no   eq xest.est-no
		   and xeb.form-no eq v-form-no no-lock.

do k = 1 to 28:
  if qtty[k] eq 0 then leave.

  FOR EACH probe
      WHERE probe.company    EQ xest.company
        AND probe.est-no     EQ xest.est-no
        AND probe.probe-date EQ TODAY
        AND probe.est-qty    EQ qtty[k]
        AND probe.freight    EQ rels[k]
      NO-LOCK
      BY probe.probe-time DESC:
    LEAVE.
  END.

  v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999".

  if xest.est-type eq 6 then
    assign
     outfile1 = tmp-dir + trim(xest.est-no) + "-"  +
			  string(v-form-no,"99")     + ".v" + string(probe.line,v-probe-fmt)
     outfile2 = tmp-dir + trim(xest.est-no) + "-"  +
			  string(v-form-no,"99")     + ".a" + string(probe.line,v-probe-fmt)
     outfile3 = tmp-dir + trim(xest.est-no) + "-"  +
			  string(v-form-no,"99")     + ".s" + string(probe.line,v-probe-fmt)
     outfile4 = tmp-dir + trim(xest.est-no) + "-"  +
			  string(v-form-no,"99")     + ".z" + string(probe.line,v-probe-fmt)
     ls-outfile = tmp-dir + trim(xest.est-no) + "-"  +
              	  string(v-form-no,"99")     + ".p" + string(probe.line,v-probe-fmt).
  else
    assign
     outfile1 = tmp-dir + trim(xest.est-no) + ".v" + string(probe.line,v-probe-fmt)
     outfile2 = tmp-dir + trim(xest.est-no) + ".a" + string(probe.line,v-probe-fmt)
     outfile3 = tmp-dir + trim(xest.est-no) + ".s" + string(probe.line,v-probe-fmt)
     outfile4 = tmp-dir + trim(xest.est-no) + ".z" + string(probe.line,v-probe-fmt)
     ls-outfile = tmp-dir + trim(xest.est-no) + ".p" + string(probe.line,v-probe-fmt).

  if vmclean then do:
    output to value(outfile3) append.

    DEF VAR cerunc-dec AS DEC NO-UNDO.

    FIND FIRST sys-ctrl WHERE
         sys-ctrl.company EQ xest.company AND
         sys-ctrl.NAME EQ "CERUNC"
         NO-LOCK.

    cerunc-dec = sys-ctrl.dec-fld.

    {cec/pr4-mcl.i k}

    output close.
  end.

  os-append value(outfile2) value(outfile3).
  os-copy value(outfile3) value(ls-outfile).
end.

/* end ---------------------------------- copr. 1996  advanced software, inc. */

