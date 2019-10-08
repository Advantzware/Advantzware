
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.


FIND notes WHERE ROWID(notes) EQ ip-rowid NO-LOCK NO-ERROR.
IF AVAIL notes THEN
FOR EACH est WHERE est.rec_key EQ notes.rec_key:
  ASSIGN
   est.updated-date = TODAY
   est.updated-id   = USERID("nosweat").
  LEAVE.
END.
