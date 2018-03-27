
&IF "{&{1}-TABLE}" NE "" &THEN

DEF BUFFER {&{1}-TABLE}-{2} FOR {&{1}-TABLE}.
DEF VAR checkus1-upd-user-{2} AS CHAR NO-UNDO.
IF AVAIL {&{1}-TABLE} THEN DO TRANSACTION:
  FIND {&{1}-TABLE}-{2}
      WHERE ROWID({&{1}-TABLE}-{2}) EQ
            ROWID({&{1}-TABLE})
      EXCLUSIVE NO-WAIT NO-ERROR.

  IF NOT AVAIL {&{1}-TABLE}-{2} THEN DO:
      
    RUN custom/get-lock-user.p (INPUT "{&{1}-TABLE}", 
                                INPUT ROWID({&{1}-TABLE}), 
                                OUTPUT checkus1-upd-user-{2}).
    MESSAGE "Table ("                              +
            TRIM("{&{1}-TABLE}")                   +
            ") is being changed by someone else" + 
             " (" + checkus1-upd-user-{2} + ") " + ", " +
            "wait a moment and try again..."
        VIEW-AS ALERT-BOX ERROR.
    IF "{3}" EQ "" THEN DO:
      RUN dispatch ("cancel-record").
      RETURN "ADM-ERROR":U.
    END.
    ELSE RETURN ERROR.
  END.
  ELSE
  FIND {&{1}-TABLE}-{2}
      WHERE ROWID({&{1}-TABLE}-{2}) EQ
            ROWID({&{1}-TABLE})
      NO-LOCK NO-ERROR.
END.

RELEASE {&{1}-TABLE}-{2}.

&ENDIF
