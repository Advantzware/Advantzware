
SESSION:SET-WAIT-STATE ("general").

FOR EACH oe-bolh NO-LOCK,
    EACH oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no:
  oe-boll.cust-no = oe-bolh.cust-no.
END.

SESSION:SET-WAIT-STATE ("").
