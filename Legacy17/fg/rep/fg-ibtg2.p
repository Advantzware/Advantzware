/* ---------------------------------------------- fg/rep/fg-ibtg2.p 10/99 JLF */
/* FINISHED GOODS - INVENTORY ON HAND BY BIN / TAG by Item Number             */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{fg/rep/fg-ibtg1.i "shared"}

for each tt-itemfg use-index i-no no-lock,
  {fg/rep/fg-ibtag.i tt-itemfg.i-no 1}
end.

/* end ---------------------------------- copr. 1999  advanced software, inc. */

