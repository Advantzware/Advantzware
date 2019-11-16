/* ar-cashl.i */

IF ar-mcash.posted THEN
DO:
   MESSAGE "Cannot Delete Posted Misc. Cash Receipt."
       VIEW-AS ALERT-BOX ERROR BUTTONS OK.
   RETURN.
END.


DEF BUFFER bf-mcash FOR ar-mcash.
DEF BUFFER bf4-reftable FOR reftable.

IF AVAIL ar-mcash THEN
FOR EACH bf-mcash
    WHERE bf-mcash.company      EQ ar-mcash.company
      AND bf-mcash.posted       EQ ar-mcash.posted
      AND bf-mcash.payer        EQ ar-mcash.payer
      AND bf-mcash.check-date   EQ ar-mcash.check-date
      AND bf-mcash.bank-code    EQ ar-mcash.bank-code
      AND bf-mcash.curr-code[1] EQ ar-mcash.curr-code[1]
      AND ROWID(bf-mcash)       NE ROWID(ar-mcash),
    FIRST bf4-reftable WHERE
          bf4-reftable.reftable = "AR-MCASH" AND
          bf4-reftable.company  = bf-mcash.company AND
          bf4-reftable.loc      = STRING(bf-mcash.m-no,">>>>>>9") AND
          bf4-reftable.code     = bf-mcash.rec_key AND
          bf4-reftable.code2    = fl_checkno:SCREEN-VALUE IN FRAME {&FRAME-NAME}
          NO-LOCK:

    DELETE bf-mcash.
END.
