/* ------------------------------------------------ sys/inc/numup.p 09/97 JLF */
/* Calculate Number of Blanks on Net Sheet                                    */
/* -------------------------------------------------------------------------- */

def input  param v-company      like eb.company .
def input  param v-est-no       like eb.est-no.
def input  param v-form-no      like eb.form-no.
def output param v-num-up       like eb.num-up.

find est where est.company = v-company 
           and est.est-no eq v-est-no no-lock.

v-num-up = 0.
for each eb where eb.company = v-company 
              and eb.est-no    eq v-est-no
              and eb.form-no  eq v-form-no
         no-lock:
   v-num-up = v-num-up + eb.num-up.
   if est.est-type eq 3 then leave.
end.

/*if est.est-type eq 2 AND est.form-qty eq 1 then v-num-up = v-num-up / 2.*/

/* end ---------------------------------- copr. 1997  advanced software, inc. */

