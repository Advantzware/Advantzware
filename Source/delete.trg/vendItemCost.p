&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME vendItemCost

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

DEFINE BUFFER bf-vendItemCostLevel FOR vendItemCostLevel.

{methods/triggers/delete.i}

FOR EACH bf-vendItemCostLevel EXCLUSIVE-LOCK 
    WHERE bf-vendItemCostLevel.vendItemCostID EQ {&TABLENAME}.vendItemCostID:
    DELETE bf-vendItemCostLevel.
END.
