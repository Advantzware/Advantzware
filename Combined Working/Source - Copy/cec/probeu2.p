/* -------------------------------------------------- cec/probeu2.p 03/97 JLF */
/* 'What if' UPDATE                                                           */
/* -------------------------------------------------------------------------- */

def input parameter v-recid as recid.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

def shared var tmp-dir as cha no-undo.

{cec/print4.i shared shared}
{cec/print42.i shared}

def var qm as de.
def var v as int.
def var v-comm like tt-tot.
def var v-yld as dec.
def var v-skip-pct as log.
def var v-line as char.
def var v-loop as log no-undo.
DEF VAR v-probe-fmt AS CHAR NO-UNDO.
DEF VAR v-quotercmd AS CHAR NO-UNDO.
{cec/msfcalc.i}

{cec/rollfac.i}

{cec/combasis.i}


find probe where recid(probe) eq v-recid no-lock no-error.

v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999".

if vmclean2 then
  output to value(tmp-dir + trim(xest.est-no) + ".s" +
                            string(probe.line,v-probe-fmt))  /*page-size 63 /*99999 */*/.
else
  output to value(tmp-dir + trim(xest.est-no) + ".s" +
                            string(probe.line,v-probe-fmt)).

for each ef
    where ef.company eq xest.company
      and ef.est-no  eq xest.est-no
    no-lock:
  assign
   v-form-no = ef.form-no
   outfile3  = tmp-dir + trim(xest.est-no) + "-"  +
                         string(v-form-no,"99")   + ".s" +
                         string(probe.line,v-probe-fmt).

  v-quotercmd = SEARCH("quoter.exe").
  v-quotercmd = v-quotercmd + " -c 1-85 " + outfile3 + " > " +
                          (tmp-dir + trim(xest.est-no) + ".q" + string(probe.line,v-probe-fmt)).

  DOS SILENT VALUE(v-quotercmd).

/*   if opsys eq "unix" then                                 */
/*     unix silent quoter -c 1-85 value(outfile3) >          */
/*          value(tmp-dir + trim(xest.est-no) + ".q"         */
/*                        + string(probe.line,v-probe-fmt)). */
/*   else                                                    */
/*     dos  silent quoter -c 1-85 value(outfile3) >          */
/*          value(tmp-dir + trim(xest.est-no) + ".q"         */
/*                        + string(probe.line,v-probe-fmt)). */

  input from value(tmp-dir + trim(xest.est-no) + ".q"
                           + string(probe.line,v-probe-fmt)).

  repeat:
    v-line = "".
    import v-line.
    if v-line eq "" then put skip(1).
    else put unformatted v-line skip.
  end.
  input close.

  if (not vmclean2) then leave.

  page. 
end.

OUTPUT CLOSE.
output to value(tmp-dir + trim(xest.est-no) + ".s" +
                            string(probe.line,v-probe-fmt)) APPEND.
IF vmclean2 THEN RUN cec/pr4-mcl2.p (RECID(probe)).

output close.


/* end ---------------------------------- copr. 1997  advanced software, inc. */
