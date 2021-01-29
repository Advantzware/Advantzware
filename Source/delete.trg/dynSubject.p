&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME dynSubject

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{AOA/includes/dynSubjctDelete.i "dynParamValue"}
{AOA/includes/dynSubjctDelete.i "dynSubjectTable"}
{AOA/includes/dynSubjctDelete.i "dynSubjectWhere"}
{AOA/includes/dynSubjctDelete.i "dynSubjectColumn"}
{AOA/includes/dynSubjctDelete.i "dynSubjectParamSet"}

{methods/triggers/delete.i}
