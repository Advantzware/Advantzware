/* ttTemplate.i */

/* Template.rpa */
DEFINE TEMP-TABLE ttTemplate NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD field1 AS CHARACTER LABEL "Character Field"
    FIELD field2 AS INTEGER   LABEL "Integer Field"
    FIELD field3 AS DECIMAL   LABEL "Decimal Field"
    FIELD field4 AS DATE      LABEL "Date Field"      FORMAT "99/99/9999"
    FIELD field5 AS LOGICAL   LABEL "Logical Field"
    .
