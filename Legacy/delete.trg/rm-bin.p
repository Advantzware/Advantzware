&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME rm-bin

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

DEF BUFFER rm-bin-age-date FOR reftable.


IF {&TABLENAME}.tag NE "" THEN
FOR EACH loadtag
    WHERE loadtag.company      EQ {&TABLENAME}.company
      AND loadtag.item-type    EQ YES
      AND loadtag.tag-no       EQ {&TABLENAME}.tag
      AND loadtag.i-no         EQ {&TABLENAME}.i-no
      AND loadtag.is-case-tag  EQ NO:
  ASSIGN
   loadtag.qty          = 0
   loadtag.qty-case     = 0
   loadtag.pallet-count = 0
   loadtag.tot-cases    = 0.
END.

FOR EACH rm-bin-age-date
    WHERE rm-bin-age-date.reftable EQ "rm-bin.age-date"
      AND rm-bin-age-date.company  EQ {&TABLENAME}.company
      AND rm-bin-age-date.loc      EQ {&TABLENAME}.i-no
      AND rm-bin-age-date.code     EQ STRING({&TABLENAME}.loc,"x(50)") +
                                      STRING({&TABLENAME}.loc-bin,"x(50)")
      AND rm-bin-age-date.code2    EQ {&TABLENAME}.tag:
  DELETE rm-bin-age-date.
END.
