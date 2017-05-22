
DEF VAR ld AS DEC DECIMALS 10 NO-UNDO.
DEF VAR lv-stat LIKE po-ord.stat NO-UNDO.
DEF VAR v-cons-qty AS INT NO-UNDO.
  
{sys/inc/var.i "NEW SHARED"}

DEF BUFFER b-po-ordl FOR po-ordl.


SESSION:SET-WAIT-STATE ("general").

FOR EACH item WHERE i-code EQ "R" AND stocked EQ NO AND mat-type EQ "B": 
  stocked = YES.
END.

FOR EACH po-ord:
  lv-stat = po-ord.stat.

  DO TRANSACTION:
    po-ord.stat = "U".
  END.

  DO TRANSACTION:
    po-ord.stat = lv-stat.
  END.
END.

FOR EACH po-ord WHERE po-ord.stat EQ "C":
  cocode = po-ord.company.

  FOR EACH po-ordl
      {po/look/pobyven1.i}
        AND po-ordl.stat EQ "C"
      USE-INDEX po-no NO-LOCK:

    RUN po/rec-inv.p (ROWID(po-ordl), OUTPUT ld).

    IF ld GT 0 OR po-ordl.stat NE "C" THEN DO:
      po-ord.stat = "U".
      LEAVE.
    END.
  END.
END.

SESSION:SET-WAIT-STATE ("").
