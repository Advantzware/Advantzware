&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME oe-reth

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}


FOR EACH oe-retl WHERE oe-retl.company = oe-reth.company AND
                       oe-retl.r-no = oe-reth.r-no :
    DELETE oe-retl.
END.
