/* ttUDF.i */

DEFINE TEMP-TABLE ttUDF NO-UNDO
    FIELD udfRecKey   AS CHARACTER FORMAT "x(16)"  LABEL "Rec Key"
    FIELD udfTab      AS INTEGER   FORMAT ">9"     LABEL "Tab"
    FIELD udfOrder    AS INTEGER   FORMAT ">>9"    LABEL "Order"
    FIELD udfID       AS CHARACTER FORMAT "x(5)"   LABEL "ID"
    FIELD udfDataType AS CHARACTER FORMAT "x(9)"   LABEL "Data Type"
    FIELD udfFormat   AS CHARACTER FORMAT "x(20)"  LABEL "Format"
    FIELD udfLabel    AS CHARACTER FORMAT "x(50)"  LABEL "Label"
    FIELD udfValue    AS CHARACTER FORMAT "x(50)"  LABEL "UDF Value"
    FIELD udfColLabel AS CHARACTER FORMAT "x(20)"  LABEL "Col Label"
    FIELD udfSBField  AS INTEGER   FORMAT ">9"     LABEL "SB Field #"
    FIELD udfEsko     AS LOGICAL   FORMAT "yes/no" LABEL "Esko"
        INDEX ttUDF IS PRIMARY udfRecKey udfTab udfOrder
    .
