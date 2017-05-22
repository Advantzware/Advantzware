/* ------------------------------------------------------ cec/u1kinc1.p 12/93 */
/* calculate each of the 12 formulas in style file! (1-6)                     */
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

blok:
do on error undo:
      find xrfq where recid(xrfq) = ip-rfq-id no-lock.
      find xitem where recid(xitem) = ip-item-id no-lock. 
      find first style where style.company = xrfq.company and style.style = xitem.style
      no-lock no-error.
      find first formule no-error.
      if not avail formule then create formule.

/* JLF added 02/28/96 */
      find first reftable
          where reftable.reftable eq "STYFLU"
            and reftable.company  eq xitem.style
            and reftable.loc      eq xitem.flute
            and reftable.code     eq "DIM-FIT"
          no-lock no-error.

      v-dim-fit = if avail reftable then reftable.val[1] else 0.
/* JLF added 02/28/96 */

/* All references to style.dim-fit changed to v-dim-fit -JLF- 02/28/96 */

      tmpstore = style.formula[1].
         {rfq/kstylec.i &for=1 &l=xitem.len &w=xitem.wid &d=xitem.dep
            &k=xitem.k-wid &t=xitem.tuck &g=xitem.gluelap &b=xitem.fpanel
            &f=xitem.dust  &o=xitem.lock &i=v-dim-fit}

      tmpstore = style.formula[2].
         {rfq/kstylec.i &for=2 &l=xitem.len &w=xitem.wid &d=xitem.dep
            &k=xitem.k-len &t=xitem.tuck &g=xitem.gluelap &b=xitem.fpanel
            &f=xitem.dust  &o=xitem.lock &i=v-dim-fit}

      tmpstore = style.formula[3].
         {rfq/kstylec.i &for=3 &l=xitem.len &w=xitem.wid &d=xitem.dep
            &k=xitem.k-wid &t=xitem.tuck &g=xitem.gluelap &b=xitem.fpanel
            &f=xitem.dust  &o=xitem.lock &i=v-dim-fit}

      tmpstore = style.formula[4].
         {rfq/kstylec.i &for=4 &l=xitem.len &w=xitem.wid &d=xitem.dep
            &k=xitem.k-len &t=xitem.tuck &g=xitem.gluelap &b=xitem.fpanel
            &f=xitem.dust  &o=xitem.lock &i=v-dim-fit}

      tmpstore = style.formula[5].
         {rfq/kstylec.i &for=5 &l=xitem.len &w=xitem.wid &d=xitem.dep
            &k=xitem.k-wid &t=xitem.tuck &g=xitem.gluelap &b=xitem.fpanel
            &f=xitem.dust  &o=xitem.lock &i=v-dim-fit}

      tmpstore = style.formula[6].
         {rfq/kstylec.i &for=6 &l=xitem.len &w=xitem.wid &d=xitem.dep
            &k=xitem.k-len &t=xitem.tuck &g=xitem.gluelap &b=xitem.fpanel
            &f=xitem.dust  &o=xitem.lock &i=v-dim-fit}

end.

/* end ---------------------------------- copr. 1993  advanced software, inc. */
