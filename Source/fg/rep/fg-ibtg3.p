/* ---------------------------------------------- fg/rep/fg-ibtg3.p 10/99 JLF */
/* FINISHED GOODS - INVENTORY ON HAND BY BIN / TAG by Product Category        */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{fg/rep/fg-ibtg1.i "shared"}

for each tt-itemfg use-index procat no-lock,
  {fg/rep/fg-ibtag.i tt-itemfg.procat tt-itemfg.i-no}
end.

/* end ---------------------------------- copr. 1999  advanced software, inc. */

