&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME ap-pay

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

    FOR EACH ap-payl WHERE ap-payl.c-no = ap-pay.c-no:
        DELETE ap-payl.
    END.
