/* sman.i */

FOR EACH sman-mtx
    WHERE sman-mtx.company EQ {&TABLENAME}.company
      AND sman-mtx.sman    EQ {&TABLENAME}.sman:
  DELETE sman-mtx.
END.

IF CAN-FIND(FIRST asi._file WHERE asi._file._file-name EQ "smanmtrx") THEN
  RUN custom/delsmtrx.p ({&TABLENAME}.company, 2, {&TABLENAME}.sman).
