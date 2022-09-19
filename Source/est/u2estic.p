/* --------------------------------------------------- cec/u2esti.p 07/97 JLF */
/* Cost Estimating - Page 1 UPDATE Part2 - Get Scoring                        */
/* -------------------------------------------------------------------------- */

def input        parameter v-field-no  as   char.
def input        parameter v-dim-fit   as   log.
def input-output parameter v-field     like eb.k-wid.
/*
{sys/inc/var.i shared}

def shared var tab-inout as ch format "X".
def shared var head as ch format "x(78)" extent 20.

def shared frame est.
{sys/form/s-top.f}
{cec/est.f &fil=xest &fil2=xef &fil3=xeb}

*/

def shared buffer xeb  for eb.

def var v-sq-box like eb.k-wid.
def var k_frac as dec init 6.25 no-undo.
DEF VAR cocode AS CHAR NO-UNDO.
DEFINE VARIABLE dBoxFit        AS DECIMAL NO-UNDO.
DEFINE VARIABLE hdFormulaProcs AS HANDLE  NO-UNDO.

RUN system/FormulaProcs.p PERSISTENT SET hdFormulaProcs.


cocode = xeb.company.

{sys/inc/f16to32.i}

find first reftable
    where reftable.reftable eq "STYFLU"
      and reftable.company  eq xeb.style
      and reftable.loc      eq xeb.flute
      and reftable.code     eq v-field-no
    no-lock no-error.
if not avail reftable then leave.    

v-field = trunc(reftable.val[13],0) +
          ((reftable.val[13] - trunc(reftable.val[13],0)) * k_frac).

if v-dim-fit then do:  
  RUN Formula_GetSquareBoxFitForStyleAndFlute IN hdFormulaProcs (cocode, xeb.style, xeb.flute, OUTPUT dBoxFit).
  v-field = v-field - (dBoxFit / 6.25 * k_frac * 2).
end.

IF VALID-HANDLE(hdFormulaProcs) THEN
  DELETE PROCEDURE hdFormulaProcs.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
