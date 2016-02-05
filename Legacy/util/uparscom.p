DEF VAR li AS INT NO-UNDO.


FOR EACH ar-invl WHERE ar-invl.bol-no NE 0,
    FIRST ar-inv WHERE ar-inv.x-no EQ ar-invl.x-no NO-LOCK,
    FIRST cust
    WHERE cust.company EQ ar-inv.company
      AND cust.cust-no EQ ar-inv.cust-no
    NO-LOCK,
    FIRST itemfg
    WHERE itemfg.company EQ ar-inv.company
      AND itemfg.i-no    EQ ar-invl.i-no
    NO-LOCK:

  DO li = 1 TO 3:
    IF ar-invl.sman[li] EQ "" AND li EQ 1 THEN ar-invl.sman = cust.sman.

    IF ar-invl.sman[li] NE "" THEN
      RUN sys/inc/getsmncm.p (ar-inv.cust-no,
                              INPUT-OUTPUT ar-invl.sman[li],
                              itemfg.procat,
                              0,
                              OUTPUT ar-invl.s-comm[li]).
  END.
END.
