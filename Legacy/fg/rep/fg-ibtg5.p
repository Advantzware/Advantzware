/* ---------------------------------------------- fg/rep/fg-ibtg5.p 10/04 JLF */
/* FINISHED GOODS - INVENTORY ON HAND BY BIN / TAG by WHS BIN TAG             */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{fg/rep/fg-ibtg1.i "shared"}

for each tt-itemfg use-index loc-bin-tag no-lock,
  {fg/rep/fg-ibtag.i tt-itemfg.loc-bin-tag tt-itemfg.i-no "by tt-itemfg.tag"}
end.

/* end ---------------------------------- copr. 1999  advanced software, inc. */
