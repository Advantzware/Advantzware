&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME dynForm

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{AOA/includes/dynFormDelete.i "dynFormElement"}
{AOA/includes/dynFormDelete.i "dynFormLayout"}
{AOA/includes/dynFormDelete.i "dynFormTarget"}

{methods/triggers/delete.i}
