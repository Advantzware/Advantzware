/* -------------------------------------------------- cec/pr4-str.i 06/97 JLF */

def buffer strap for reftable.
def buffer stackPattern for stackPattern.
def var v-numstacks as int.
def var v-stackcode as char.
def var v-est-id as recid.
def var v-cas-pal like xeb.cas-pal.
def var v-tr-cnt like xeb.tr-cnt.
def var v-error as logical.

def var formule as de extent 12.
def var op as ch extent 100.
def var nextop as int.
def var num as de extent 100.
def var curnum as ch.
def var kar as ch format "x".  /* style formula kalk variables */
def var v-dim-fit like style.dim-fit.
def var k_frac as dec init 6.25 no-undo.

def var strap-qty as dec.
def var strap-cst as dec.

{sys/inc/f16to32.i}


   strap-qty = 0.

   v-stackcode = "D".

   IF xeb.form-no NE 0 THEN
     run cec/kpallet.p(input v-est-id,
                       output v-cas-pal,
                       output v-tr-cnt,
                       output v-numstacks,
                       output v-stackcode,
                       output v-error).

   if not v-error then do:
/*     find first strap                        */
/*         where strap.reftable eq "STACKSTRAP"*/
/*           and strap.company  eq ""          */
/*           and strap.loc      eq ""          */
/*           and strap.code     eq v-stackcode */
/*         no-lock no-error.                   */
/*                                             */
/*     if avail strap then do:                 */
/*       tmpstore = strap.dscr.                */
     find first stackPattern
         where stackPattern.stackCode     eq v-stackcode
         no-lock no-error.

     if avail stackPattern then do:
       tmpstore = stackPattern.strapFormula.
     
       {cec/kstyle.i &for=1 &l=xeb.tr-len &w=xeb.tr-wid &d=xeb.tr-dep
                     &k=0 &t=0 &g=0 &b=0 &f=0 &o=0 &i=0}

       strap-qty = formule[1].
     end.
   end.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
