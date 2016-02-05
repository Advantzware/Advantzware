/* --------------------------------------------------- cec/probev.p 07/96 JLF */
/* probe    view                                                              */
/* -------------------------------------------------------------------------- */

def input param v-recid as recid.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared buffer xest for est.

DEF VAR tmp-dir AS CHAR NO-UNDO.


run cec/probeu1.p (v-recid).

find probe where recid(probe) eq v-recid no-lock no-error.

pause 0.

IF probe.LINE LT 100 THEN
DO:
  if opsys eq "unix" then 
     unix silent cp  value(tmp-dir + string(xest.e-num,"99999999") + ".s"
                                   + string(probe.line,"99"))
                     value(tmp-dir + string(xest.e-num,"99999999") + ".p"
                                   + string(probe.line,"99")).
  else
     dos silent copy value(tmp-dir + string(xest.e-num,"99999999") + ".s"
                                   + string(probe.line,"99"))
                     value(tmp-dir + string(xest.e-num,"99999999") + ".p"
                                   + string(probe.line,"99")).

  run sys/inc/screen.p (input (tmp-dir + string(xest.e-num,"99999999") + ".p"
                                     + string(probe.line,"99"))).
END.
ELSE
DO:
   if opsys eq "unix" then 
     unix silent cp  value(tmp-dir + string(xest.e-num,"99999999") + ".s"
                                   + string(probe.line,"999"))
                     value(tmp-dir + string(xest.e-num,"99999999") + ".p"
                                   + string(probe.line,"999")).
   else
      dos silent copy value(tmp-dir + string(xest.e-num,"99999999") + ".s"
                                    + string(probe.line,"999"))
                      value(tmp-dir + string(xest.e-num,"99999999") + ".p"
                                    + string(probe.line,"999")).

   run sys/inc/screen.p (input (tmp-dir + string(xest.e-num,"99999999") + ".p"
                                     + string(probe.line,"999"))).
END.
                                      

                                       
/* end ---------------------------------- copr. 1996  advanced software, inc. */
