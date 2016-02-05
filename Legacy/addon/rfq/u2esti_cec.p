/* --------------------------------------------------- cec/u2esti.p 07/97 JLF */
/* Cost Estimating - Page 1 UPDATE Part2 - Get Scoring                        */
/* -------------------------------------------------------------------------- */
def input parameter v-style  like style.style no-undo.
def input parameter v-flute as cha no-undo.
def input        parameter v-field-no  as   char.
def input        parameter v-dim-fit   as   log.
def input-output parameter v-field     like eb.k-wid.

/*
{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

def shared var tab-inout as ch format "X".
def shared var head as ch format "x(78)" extent 20.

def var v-sq-box like eb.k-wid.

def shared frame est.
{sys/form/s-top.f}
{cec/est.f &fil=xest &fil2=xef &fil3=xeb}

*/

find first reftable
    where reftable.reftable eq "STYFLU"
      and reftable.company  eq v-style
      and reftable.loc      eq v-flute
      and reftable.code     eq v-field-no
    no-lock no-error.
if not avail reftable then leave.    

v-field = trunc(reftable.val[13],0) +
          ((reftable.val[13] - trunc(reftable.val[13],0)) * 6.25).

if v-dim-fit then do:
  find first reftable
      where reftable.reftable eq "STYFLU"
        and reftable.company  eq v-style
        and reftable.loc      eq v-flute
        and reftable.code     eq "DIM-FIT"
      no-lock no-error.

  if avail reftable then v-field  = v-field - (reftable.val[1] * 2).
end.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
