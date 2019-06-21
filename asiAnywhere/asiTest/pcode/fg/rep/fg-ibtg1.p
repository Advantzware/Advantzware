/* ---------------------------------------------- fg/rep/fg-ibtg1.p 10/99 JLF */
/* FINISHED GOODS - INVENTORY ON HAND BY BIN / TAG by Customer Number         */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{fg/rep/fg-ibtg1.i "shared"}

for each tt-itemfg use-index cust-no no-lock,
  {fg/rep/fg-ibtag.i tt-itemfg.cust-no tt-itemfg.i-no}
end.

/* end ---------------------------------- copr. 1999  advanced software, inc. */

