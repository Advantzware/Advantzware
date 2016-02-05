
DEF VAR lv-stat LIKE oe-ord.stat NO-UNDO.


SESSION:SET-WAIT-STATE ("general").

FOR EACH oe-ord:
  lv-stat = oe-ord.stat.

  DO TRANSACTION:
    oe-ord.stat = "".
  END.

  DO TRANSACTION:
    oe-ord.stat = lv-stat.
  END.
END.

FOR EACH job:
  lv-stat = job.stat.

  DO TRANSACTION:
    job.stat = "".
  END.

  DO TRANSACTION:
    job.stat = lv-stat.
  END.
END.

FOR EACH po-ord:
  lv-stat = po-ord.stat.

  DO TRANSACTION:
    po-ord.stat = "".
  END.

  DO TRANSACTION:
    po-ord.stat = lv-stat.
  END.
END.

SESSION:SET-WAIT-STATE ("").
