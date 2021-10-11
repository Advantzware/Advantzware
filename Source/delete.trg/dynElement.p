&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME dynElement

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

FOR EACH dynFormElement EXCLUSIVE-LOCK
    WHERE dynFormElement.elementID EQ dynElement.elementID
    :
    DELETE dynFormElement.
END. /* each dynformelement */

{methods/triggers/delete.i}
