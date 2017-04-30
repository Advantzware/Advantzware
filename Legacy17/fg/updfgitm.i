  
DEF BUFFER b-{1}{4} FOR {1}.


DISABLE TRIGGERS FOR LOAD OF b-{1}{4}.

RELEASE b-{1}{4}.

FIND FIRST asi._file WHERE asi._file._file-name EQ "{1}" NO-LOCK NO-ERROR.

STATUS DEFAULT "Waiting for " +
               TRIM(IF AVAIL asi._file       AND
                       asi._file._desc NE "" THEN asi._file._desc
                                             ELSE "{1}") +
               "...".

DO TRANSACTION WHILE NOT AVAIL b-{1}{4}:
  FIND b-{1}{4} WHERE ROWID(b-{1}{4}) EQ ROWID({1}) EXCLUSIVE NO-WAIT NO-ERROR.
  IF AVAIL b-{1}{4} THEN DO:
    IF {3} AND AVAIL b-itemfg THEN DELETE {1}.
    ELSE b-{1}{4}.{2} = v-new-item.
  END.
  ELSE
  IF NOT LOCKED b-{1}{4} THEN LEAVE.
END.

STATUS DEFAULT "".
