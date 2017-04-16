/* -------------------------------------------------- fg/updfgcst.p 03/97 JLF */
/* -------------------------------------------------------------------------- */

def input parameter v-i-no like itemfg.i-no.

{sys/inc/var.i shared}


for each itemfg
    where itemfg.company eq cocode
      and (if v-i-no eq "*" then yes
	   else itemfg.i-no eq v-i-no)
    no-lock:

  status default "Please wait...  Updating item: " + trim(itemfg.i-no).

  IF itemfg.isaset AND itemfg.alloc THEN
    RUN util/fixfgcst.p (ROWID(itemfg)).
  ELSE
    RUN fg/updfgcs1.p (recid(itemfg), YES).
end.

status default "".

/* end ---------------------------------- copr. 1997  Advanced Software, Inc. */

