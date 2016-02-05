DEF VAR a LIKE fg-rcpth.po-no.
    
FOR EACH fg-rcpth WHERE po-no NE "":
  a = po-no.
  DO TRANSACTION:
    po-no = "".
  END.
  DO TRANSACTION:
    po-no = a.
  END.
END.

FOR EACH rm-rcpth WHERE po-no NE "":
  a = po-no.
  DO TRANSACTION:
    po-no = "".
  END.
  DO TRANSACTION:
    po-no = a.
  END.
END.
