&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME rm-bin

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}


IF {&TABLENAME}.tag NE "" THEN
FOR EACH loadtag
    WHERE loadtag.company      EQ {&TABLENAME}.company
      AND loadtag.item-type    EQ YES
      AND loadtag.tag-no       EQ {&TABLENAME}.tag
      AND loadtag.i-no         EQ {&TABLENAME}.i-no
      AND loadtag.is-case-tag  EQ NO:
  ASSIGN
   loadtag.qty          = {&TABLENAME}.qty
   loadtag.qty-case     = {&TABLENAME}.qty
   loadtag.pallet-count = {&TABLENAME}.qty
   loadtag.tot-cases    = {&TABLENAME}.qty.
END.
