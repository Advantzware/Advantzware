&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME settingType

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

FOR EACH setting EXCLUSIVE-LOCK
    WHERE setting.settingTypeID EQ {&TABLENAME}.settingTypeID
    :
    DELETE setting.
END. // each setting

{methods/triggers/delete.i}
