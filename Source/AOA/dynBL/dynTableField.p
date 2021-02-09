/*------------------------------------------------------------------------
  File:         dynTableField.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 1.17.2021
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttTableField
DEFINE TEMP-TABLE ttTableField NO-UNDO
    FIELD subjectTitle LIKE dynSubject.subjectTitle
    FIELD subjectID    LIKE dynSubject.subjectID
    FIELD tableName    LIKE dynSubjectColumn.tableName
    FIELD fieldName    LIKE dynSubjectColumn.fieldName
    .
/* Parameters Definitions ---                                           */

&Scoped-define subjectID 158
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    FOR EACH dynSubjectColumn NO-LOCK
        WHERE dynSubjectColumn.subjectID GT 0
          AND dynSubjectColumn.tableName GT ""
          AND dynSubjectColumn.tableName LT "_"
          AND dynSubjectColumn.fieldName GT "",
        FIRST dynSubject NO-LOCK
        WHERE dynSubject.subjectID EQ dynSubjectColumn.subjectID
          AND dynSubject.isActive  EQ YES
        :
        IF NUM-ENTRIES(dynSubjectColumn.fieldName,".") LT 2 THEN NEXT.
        CREATE ttTableField.
        ASSIGN
            ttTableField.subjectTitle = dynSubject.subjectTitle
            ttTableField.subjectID    = dynSubjectColumn.subjectID
            ttTableField.tableName    = dynSubjectColumn.tableName
            ttTableField.fieldName    = ENTRY(2,dynSubjectColumn.fieldName,".")
            .
    END. /* each dynsubject */

END PROCEDURE.
