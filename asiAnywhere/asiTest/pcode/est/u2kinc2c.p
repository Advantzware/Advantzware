/* --------------------------------------------------- cec/u1kinc2.p 12/93 cd */
/* calculate each of the 12 formulas in style file! (7-12)                    */
/* -------------------------------------------------------------------------- */

/*
{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer x2-eb  for eb.
def shared var formule as de extent 12.
*/
def input parameter ip-item-id as recid.
def buffer x2-eb for eb.
def shared temp-table formule field formule as dec extent 12.

DEF SHARED VAR cocode AS cha NO-UNDO.
DEF SHARED VAR locode AS cha NO-UNDO.
DEF VAR K_frac AS DEC INIT 6.25 NO-UNDO.
def var op as ch extent 100.
def var nextop as int.
def var num as de extent 100.
def var curnum as ch.
def var kar as ch format "x".  /* style formula kalk variables */
def var tmpstore as cha no-undo.
def var i as int no-undo.
def var v-in-paren as log init no no-undo.

/* JLF added 02/28/96 */
def var v-dim-fit like style.dim-fit.
/* JLF added 02/28/96 */

{sys/inc/f16to32.i}
DEF VAR v-multiplier AS INT NO-UNDO.
v-multiplier = IF v-16-or-32 <> 0.32 THEN 16 ELSE 32.

blok:
do on error undo:
      find x2-eb where recid(x2-eb) = ip-item-id no-lock. 
      find first style where style.company = x2-eb.company and style.style = x2-eb.style
      no-lock no-error.
      find first formule no-error.

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

      tmpstore = style.formula[7].
         {est/kstylec.i &for=7 &l=x2-eb.len &w=x2-eb.wid &d=x2-eb.dep
            &k=x2-eb.k-wid &t=x2-eb.tuck &g=x2-eb.gluelap &b=x2-eb.fpanel
            &f=x2-eb.dust  &o=x2-eb.lock &i=v-dim-fit}

      tmpstore = style.formula[8].
         {est/kstylec.i &for=8 &l=x2-eb.len &w=x2-eb.wid &d=x2-eb.dep
            &k=x2-eb.k-len &t=x2-eb.tuck &g=x2-eb.gluelap &b=x2-eb.fpanel
            &f=x2-eb.dust  &o=x2-eb.lock &i=v-dim-fit}

      tmpstore = style.formula[9].
         {est/kstylec.i &for=9 &l=x2-eb.len &w=x2-eb.wid &d=x2-eb.dep
            &k=x2-eb.k-wid &t=x2-eb.tuck &g=x2-eb.gluelap &b=x2-eb.fpanel
            &f=x2-eb.dust  &o=x2-eb.lock &i=v-dim-fit}

      tmpstore = style.formula[10].
         {est/kstylec.i &for=10 &l=x2-eb.len &w=x2-eb.wid &d=x2-eb.dep
            &k=x2-eb.k-len &t=x2-eb.tuck &g=x2-eb.gluelap &b=x2-eb.fpanel
            &f=x2-eb.dust  &o=x2-eb.lock &i=v-dim-fit}

      tmpstore = style.formula[11].
         {est/kstylec.i &for=11 &l=x2-eb.len &w=x2-eb.wid &d=x2-eb.dep
            &k=x2-eb.k-wid &t=x2-eb.tuck &g=x2-eb.gluelap &b=x2-eb.fpanel
            &f=x2-eb.dust  &o=x2-eb.lock &i=v-dim-fit}

      tmpstore = style.formula[12].
         {est/kstylec.i &for=12 &l=x2-eb.len &w=x2-eb.wid &d=x2-eb.dep
            &k=x2-eb.k-wid &t=x2-eb.tuck &g=x2-eb.gluelap &b=x2-eb.fpanel
            &f=x2-eb.dust  &o=x2-eb.lock &i=v-dim-fit}
end.

/* end ---------------------------------- copr. 1993  advanced software, inc. */
