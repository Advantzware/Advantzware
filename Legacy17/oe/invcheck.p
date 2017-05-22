
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

{sys/inc/var.i SHARED}
{sys/form/s-top.f}


DISABLE TRIGGERS FOR LOAD OF inv-head.

FIND inv-head WHERE ROWID(inv-head) EQ ip-rowid EXCLUSIVE-LOCK NO-ERROR.

IF AVAIL inv-head THEN DO:
  FOR EACH inv-line OF inv-head EXCLUSIVE-LOCK:
    IF inv-line.ship-qty EQ 0 AND inv-line.inv-qty EQ 0 THEN DELETE inv-line.
  END.

  FIND FIRST inv-line OF inv-head EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL inv-line THEN DELETE inv-head.
END.

/* end ---------------------------------- copr. 2003 advanced software, inc. */
