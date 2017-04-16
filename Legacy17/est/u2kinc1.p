/* ------------------------------------------------------ cec/u1kinc1.p 12/93 */
/* calculate each of the 12 formulas in style file! (1-6)                     */
/* -------------------------------------------------------------------------- */
/*
{sys/inc/var.i shared}
def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer x2-eb  for eb.
def shared var formule as de extent 12 no-undo. */
def input parameter ip-item-id as recid.
def buffer x2-eb for eb.
def shared temp-table formule field formule as dec extent 12.

DEF VAR K_frac AS DEC INIT 6.25 NO-UNDO.
def var op as ch extent 100 no-undo.
def var nextop as int no-undo.
def var num as de extent 100 no-undo.
def var curnum as ch no-undo.
def var kar as ch format "x" no-undo.  /* style formula kalk variables */
/* JLF added 02/28/96 */
def var v-dim-fit like style.dim-fit no-undo.
def var tmpstore as cha no-undo.
def var i as int no-undo.
/* JLF added 02/28/96 */


blok:
do on error undo:

      find x2-eb where recid(x2-eb) = ip-item-id no-lock. 
      find first style where style.company = x2-eb.company and style.style = x2-eb.style
      no-lock no-error.
      find first formule no-error.
           if not avail formule then create formule.
/* JLF added 02/28/96 */
      find first reftable
          where reftable.reftable eq "STYFLU"
            and reftable.company  eq x2-eb.style
            and reftable.loc      eq x2-eb.flute
            and reftable.code     eq "DIM-FIT"
          no-lock no-error.

      v-dim-fit = if avail reftable then (reftable.val[1] / 6.25 * k_frac) else 0.
/* JLF added 02/28/96 */

/* All references to style.dim-fit changed to v-dim-fit -JLF- 02/28/96 */

      tmpstore = style.formula[1].
         {sys/inc/kstyle.i &for=1 &l=x2-eb.len &w=x2-eb.wid &d=x2-eb.dep
            &k=x2-eb.k-wid &t=x2-eb.tuck &g=x2-eb.gluelap &b=x2-eb.fpanel
            &f=x2-eb.dust  &o=x2-eb.lock &i=v-dim-fit}

      tmpstore = style.formula[2].
         {sys/inc/kstyle.i &for=2 &l=x2-eb.len &w=x2-eb.wid &d=x2-eb.dep
            &k=x2-eb.k-len &t=x2-eb.tuck &g=x2-eb.gluelap &b=x2-eb.fpanel
            &f=x2-eb.dust  &o=x2-eb.lock &i=v-dim-fit}

      tmpstore = style.formula[3].
         {sys/inc/kstyle.i &for=3 &l=x2-eb.len &w=x2-eb.wid &d=x2-eb.dep
            &k=x2-eb.k-wid &t=x2-eb.tuck &g=x2-eb.gluelap &b=x2-eb.fpanel
            &f=x2-eb.dust  &o=x2-eb.lock &i=v-dim-fit}

      tmpstore = style.formula[4].
         {sys/inc/kstyle.i &for=4 &l=x2-eb.len &w=x2-eb.wid &d=x2-eb.dep
            &k=x2-eb.k-len &t=x2-eb.tuck &g=x2-eb.gluelap &b=x2-eb.fpanel
            &f=x2-eb.dust  &o=x2-eb.lock &i=v-dim-fit}

      tmpstore = style.formula[5].
         {sys/inc/kstyle.i &for=5 &l=x2-eb.len &w=x2-eb.wid &d=x2-eb.dep
            &k=x2-eb.k-wid &t=x2-eb.tuck &g=x2-eb.gluelap &b=x2-eb.fpanel
            &f=x2-eb.dust  &o=x2-eb.lock &i=v-dim-fit}

      tmpstore = style.formula[6].
         {sys/inc/kstyle.i &for=6 &l=x2-eb.len &w=x2-eb.wid &d=x2-eb.dep
            &k=x2-eb.k-len &t=x2-eb.tuck &g=x2-eb.gluelap &b=x2-eb.fpanel
            &f=x2-eb.dust  &o=x2-eb.lock &i=v-dim-fit}

end.

/* end ---------------------------------- copr. 1993  advanced software, inc. */
