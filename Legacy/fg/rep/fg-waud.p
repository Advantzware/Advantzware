/* ---------------------------------------------- fg/rep/fg-waud.p 06/2011    */
/* FINISHED GOODS - INVENTORY ON HAND BY BIN / TAG by Item Number             */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{fg/rep/fg-waud1.i "shared"}

DEF VAR iRandom AS INT NO-UNDO.
DEF VAR iCount AS INT NO-UNDO.
DEF VAR iRecordNum AS INT NO-UNDO.

iRandom = random(1,100) .

for each tt-itemfg use-index i-no no-lock,
  {fg/rep/fg-waud.i tt-itemfg.i-no tt-itemfg.case-count}
end.

/* end ---------------------------------- copr. 1999  advanced software, inc. */

