&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME dynParamValue

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

FOR EACH dynValueParam EXCLUSIVE-LOCK
    WHERE dynValueParam.subjectID    EQ {&TABLENAME}.subjectID
      AND dynValueParam.user-id      EQ {&TABLENAME}.user-id
      AND dynValueParam.prgmName     EQ {&TABLENAME}.prgmName
      AND dynValueParam.paramValueID EQ {&TABLENAME}.paramValueID
    :
    DELETE dynValueParam.
END. /* each dynvalueparam */

FOR EACH dynValueParamSet EXCLUSIVE-LOCK
    WHERE dynValueParamSet.subjectID    EQ {&TABLENAME}.subjectID
      AND dynValueParamSet.user-id      EQ {&TABLENAME}.user-id
      AND dynValueParamSet.prgmName     EQ {&TABLENAME}.prgmName
      AND dynValueParamSet.paramValueID EQ {&TABLENAME}.paramValueID
    :
    DELETE dynValueParamSet.
END. /* each dynvalueparamset */

FOR EACH dynValueColumn EXCLUSIVE-LOCK
    WHERE dynValueColumn.subjectID    EQ {&TABLENAME}.subjectID
      AND dynValueColumn.user-id      EQ {&TABLENAME}.user-id
      AND dynValueColumn.prgmName     EQ {&TABLENAME}.prgmName
      AND dynValueColumn.paramValueID EQ {&TABLENAME}.paramValueID
    :
    DELETE dynValueColumn.
END. /* each dynvaluecolumn */

{methods/triggers/delete.i}
