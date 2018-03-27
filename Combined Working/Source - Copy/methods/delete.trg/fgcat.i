/* fgcat.i */

DEF VAR li AS INT NO-UNDO.

IF CAN-FIND(FIRST itemfg
            WHERE itemfg.company EQ {&TABLENAME}.company
              AND itemfg.procat EQ {&TABLENAME}.procat AND {&TABLENAME}.procat NE "") THEN DO:
  MESSAGE "Product Category exists in the FG File, cannot delete..."
      VIEW-AS ALERT-BOX ERROR.
  RETURN ERROR.
END.

DO li = 1 TO EXTENT(sman-mtx.procat):
  FOR EACH sman-mtx
      WHERE sman-mtx.company EQ {&TABLENAME}.company
        AND sman-mtx.procat[li] EQ {&TABLENAME}.procat:
    sman-mtx.procat[li] = "".
  END.
END.

IF CAN-FIND(FIRST asi._file WHERE asi._file._file-name EQ "smanmtrx") THEN
  RUN custom/delsmtrx.p ({&TABLENAME}.company, 3, {&TABLENAME}.procat).

FIND FIRST reftable WHERE
     reftable.reftable EQ "chargecode" AND
     reftable.company  EQ {&TABLENAME}.company AND
     reftable.loc      EQ {&TABLENAME}.procat
     NO-ERROR.

IF AVAIL reftable THEN
   DELETE reftable.

