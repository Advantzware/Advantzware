&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME storageCost

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

DEFINE BUFFER bf-palletSize FOR palletSize.

FOR EACH bf-palletSize EXCLUSIVE-LOCK
    WHERE bf-palletSize.company   EQ storageCost.company
      AND bf-palletSize.location  EQ storageCost.location
      AND bf-palletSize.positions EQ storageCost.positions:
    DELETE bf-palletSize.
END.       