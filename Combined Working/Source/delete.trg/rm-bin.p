&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME rm-bin

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}



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


