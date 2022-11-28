/* --------------------------------------------------- cec/u1kinc2.p 12/93 cd */
/* calculate each of the 12 formulas in style file! (7-12)                    */
/* -------------------------------------------------------------------------- */

def input parameter ip-rfq-id as recid.
def input parameter ip-item-id as recid.
def buffer xrfq for rfq.
def buffer xitem for rfqitem.
def shared temp-table formule field formule as dec extent 12.

def var op as ch extent 100.
def var nextop as int.
def var num as de extent 100.
def var curnum as ch.
def var kar as ch format "x".  /* style formula kalk variables */
def var tmpstore as cha no-undo.
def var i as int no-undo.
DEF VAR K_frac AS DEC INIT 6.25 NO-UNDO.

/* JLF added 02/28/96 */
def var v-dim-fit like style.dim-fit.
/* JLF added 02/28/96 */

DEF SHARED VAR cocode AS cha NO-UNDO.

{sys/inc/f16to32.i}

DEFINE VARIABLE lRound  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hdFormulaProcs AS HANDLE  NO-UNDO.

RUN system/FormulaProcs.p PERSISTENT SET hdFormulaProcs.

RUN sys/ref/nk1look.p (cocode, "ROUND", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
lRound = lFound AND cReturn EQ "YES".

blok:
do on error undo:
      find xrfq where recid(xrfq) = ip-rfq-id no-lock.
      find xitem where recid(xitem) = ip-item-id no-lock. 
      find first style where style.company = xrfq.company and style.style = xitem.style
      no-lock no-error.
      find first formule no-error.

      RUN Formula_GetSquareBoxFitForStyleAndFlute IN hdFormulaProcs (xitem.company, xitem.style, xitem.flute, OUTPUT v-dim-fit).
/* All references to style.dim-fit changed to v-dim-fit -JLF- 02/28/96 */

      tmpstore = style.formula[7].
         {rfq/kstylec.i &for=7 &l=xitem.len &w=xitem.wid &d=xitem.dep
            &k=xitem.k-wid &t=xitem.tuck &g=xitem.gluelap &b=xitem.fpanel
            &f=xitem.dust  &o=xitem.lock &i=v-dim-fit}

      tmpstore = style.formula[8].
         {rfq/kstylec.i &for=8 &l=xitem.len &w=xitem.wid &d=xitem.dep
            &k=xitem.k-len &t=xitem.tuck &g=xitem.gluelap &b=xitem.fpanel
            &f=xitem.dust  &o=xitem.lock &i=v-dim-fit}

      tmpstore = style.formula[9].
         {rfq/kstylec.i &for=9 &l=xitem.len &w=xitem.wid &d=xitem.dep
            &k=xitem.k-wid &t=xitem.tuck &g=xitem.gluelap &b=xitem.fpanel
            &f=xitem.dust  &o=xitem.lock &i=v-dim-fit}

      tmpstore = style.formula[10].
         {rfq/kstylec.i &for=10 &l=xitem.len &w=xitem.wid &d=xitem.dep
            &k=xitem.k-len &t=xitem.tuck &g=xitem.gluelap &b=xitem.fpanel
            &f=xitem.dust  &o=xitem.lock &i=v-dim-fit}

      tmpstore = style.formula[11].
         {rfq/kstylec.i &for=11 &l=xitem.len &w=xitem.wid &d=xitem.dep
            &k=xitem.k-wid &t=xitem.tuck &g=xitem.gluelap &b=xitem.fpanel
            &f=xitem.dust  &o=xitem.lock &i=v-dim-fit}

      tmpstore = style.formula[12].
         {rfq/kstylec.i &for=12 &l=xitem.len &w=xitem.wid &d=xitem.dep
            &k=xitem.k-wid &t=xitem.tuck &g=xitem.gluelap &b=xitem.fpanel
            &f=xitem.dust  &o=xitem.lock &i=v-dim-fit}
end.

IF VALID-HANDLE(hdFormulaProcs) THEN
  DELETE PROCEDURE hdFormulaProcs.
/* end ---------------------------------- copr. 1993  advanced software, inc. */
