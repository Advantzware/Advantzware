/* ttGroupCalc.i - rstark - 2.18.2019 */

DEFINE TEMP-TABLE ttGroupCalc NO-UNDO 
    FIELD subjectID AS INTEGER
    FIELD fieldName AS CHARACTER
    FIELD groupName AS CHARACTER 
    FIELD calcType  AS CHARACTER
        INDEX ttCalcGroup IS PRIMARY
            subjectID
            fieldName
            groupName
            calcType
            . 
