/* ttColumn.i */

DEFINE TEMP-TABLE ttColumn NO-UNDO
    FIELD ttOrder        AS INTEGER   LABEL "Order"             FORMAT ">>9"
    FIELD ttField        AS CHARACTER LABEL "Field"             FORMAT "x(20)"
    FIELD ttLabel        AS CHARACTER LABEL "Column"            FORMAT "x(30)"
    FIELD ttType         AS CHARACTER LABEL "Type"              FORMAT "x(10)"
    FIELD ttFormat       AS CHARACTER LABEL "Format"            FORMAT "x(20)"
    FIELD ttWidth        AS INTEGER   LABEL "Width"             FORMAT ">>>9"
    FIELD ttSize         AS INTEGER   LABEL "Size"              FORMAT ">>>>9"
    FIELD ttJasperSize   AS INTEGER   LABEL "Size"              FORMAT ">>>>9"
    FIELD ttJasperColumn AS INTEGER   LABEL "Column"            FORMAT ">>>>9"
    FIELD isActive       AS LOGICAL   LABEL "Active"
    FIELD isGroup        AS LOGICAL   LABEL "Group"
    FIELD ttGroupLabel   AS CHARACTER LABEL "Group Label"       FORMAT "x(20)"
    FIELD ttGroupCalc    AS CHARACTER LABEL "Group:Calculation" FORMAT "x(200)"
    FIELD ttPending      AS LOGICAL   LABEL "Pending"
        INDEX ttSubject IS PRIMARY
            ttField
        INDEX ttOrder
            ttOrder
            .
DEFINE TEMP-TABLE ttGroupCalc NO-UNDO 
    FIELD ttField    AS CHARACTER
    FIELD ttGroup    AS CHARACTER 
    FIELD ttCalcType AS CHARACTER
        INDEX ttCalcGroup IS PRIMARY
            ttField
            ttGroup
            ttCalcType
            . 
