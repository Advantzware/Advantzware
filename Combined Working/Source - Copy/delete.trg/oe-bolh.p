&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME oe-bolh

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

DEF TEMP-TABLE tt-ord NO-UNDO FIELD ord-no LIKE oe-boll.ord-no.

{methods/triggers/delete.i}
    

FOR EACH oe-boll NO-LOCK
    WHERE oe-boll.company EQ {&TABLENAME}.company
      AND oe-boll.b-no    EQ {&TABLENAME}.b-no:
  CREATE tt-ord.
  tt-ord.ord-no = oe-boll.ord-no.
END.

FOR EACH tt-ord WHERE tt-ord.ord-no NE 0
    BREAK BY tt-ord.ord-no:

  IF LAST-OF(tt-ord.ord-no) THEN
  FOR EACH oe-rel
      WHERE oe-rel.company EQ {&TABLENAME}.company
        AND oe-rel.ord-no  EQ tt-ord.ord-no:
    RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT oe-rel.stat).
  END.

  DELETE tt-ord.
END.
