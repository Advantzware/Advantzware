/* ---------------------------------------------- fg/rep/fg-ibtg4.p 10/99 JLF */
/* FINISHED GOODS - INVENTORY ON HAND BY BIN / TAG by Customer Part#          */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{fg/rep/fg-ibtg1.i "shared"}

for each tt-itemfg use-index part-no no-lock,
  {fg/rep/fg-ibtag.i tt-itemfg.part-cust tt-itemfg.i-no}
end.

/* end ---------------------------------- copr. 1999  advanced software, inc. */

