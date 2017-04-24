/* carrier.i */

IF CAN-FIND(FIRST carr-mtx OF carrier) THEN
FOR EACH carr-mtx OF carrier EXCLUSIVE-LOCK:
  DELETE carr-mtx.
END.
