&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME oe-boll

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

/*for each oe-boll-qty of {&TABLENAME}:
  delete oe-boll-qty.
end.*/

FOR EACH oe-bolh WHERE oe-bolh.b-no EQ {&TABLENAME}.b-no:
  ASSIGN
   oe-bolh.upd-date = TODAY
   oe-bolh.upd-time = TIME
   oe-bolh.user-id  = USERID("nosweat").

  RUN oe/bolhtots.p (ROWID(oe-bolh)).

  LEAVE.
END.

IF {&TABLENAME}.rec_key NE "" THEN
FOR EACH reftable WHERE reftable.rec_key EQ {&TABLENAME}.rec_key
    USE-INDEX rec_key:
  DELETE reftable.
END.


IF {&TABLENAME}.ord-no NE 0 THEN
FOR EACH oe-rel
    WHERE oe-rel.company EQ {&TABLENAME}.company
      AND oe-rel.ord-no  EQ {&TABLENAME}.ord-no:
  RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT oe-rel.stat).
END.

