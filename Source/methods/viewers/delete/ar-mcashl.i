/* ar-cashl.i */

DEF BUFFER bf-mcash FOR ar-mcash.


IF AVAIL ar-mcash THEN
FOR EACH bf-mcash
    WHERE bf-mcash.company      EQ ar-mcash.company
      AND bf-mcash.posted       EQ ar-mcash.posted
      AND bf-mcash.payer        EQ ar-mcash.payer
      AND bf-mcash.check-date   EQ ar-mcash.check-date
      AND bf-mcash.bank-code    EQ ar-mcash.bank-code
      AND bf-mcash.curr-code[1] EQ ar-mcash.curr-code[1]
      AND ROWID(bf-mcash)       NE ROWID(ar-mcash):
  DELETE bf-mcash.
END.
