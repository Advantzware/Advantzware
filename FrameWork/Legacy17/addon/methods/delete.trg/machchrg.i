/* machchrg.i */

FOR EACH machseq OF machchrg EXCLUSIVE-LOCK:
  DELETE machseq.
END.
