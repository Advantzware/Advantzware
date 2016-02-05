/* mmtx.i */

IF CAN-FIND(FIRST mmtx2 OF mmtx) THEN
DO:
  FIND FIRST mmtx2 OF mmtx EXCLUSIVE-LOCK.
  DELETE mmtx2.
END.
