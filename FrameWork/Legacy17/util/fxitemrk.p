DEF BUFFER b-item FOR item.


SESSION:SET-WAIT-STATE ("general").

FOR EACH b-item NO-LOCK USE-INDEX rec_key:
  FOR EACH item
      WHERE item.rec_key EQ b-item.rec_key
        AND ROWID(item)  NE ROWID(b-item)
      EXCLUSIVE USE-INDEX rec_key TRANSACTION:
    STATUS DEFAULT "Processing RM Item#: " + TRIM(item.i-no).
    {methods/triggers/create.i &TABLENAME=item}
  END.
END.

STATUS DEFAULT "".

SESSION:SET-WAIT-STATE ("").
