&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME loc

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}


FIND FIRST usercomp WHERE usercomp.company = loc.company AND
                          usercomp.loc = loc.loc NO-LOCK NO-ERROR.
IF AVAIL usercomp THEN DO:

   MESSAGE "You must change user ID warehouse before deleting this warehouse!"
         VIEW-AS ALERT-BOX ERROR.
   RETURN ERROR.
END.
