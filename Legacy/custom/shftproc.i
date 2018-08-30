/* shftproc.i */

PROCEDURE Get-Shift:
    DEFINE INPUT  PARAMETER ip-company  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ip-machine  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ip-time     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ip-optype   AS CHARACTER NO-UNDO.
    
    DEFINE OUTPUT PARAMETER op-shift    AS CHARACTER NO-UNDO.

    DEFINE VARIABLE counter             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE tmp-shift           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iEarliestShiftStart AS INTEGER   NO-UNDO.
    
    {custom/getshift.i
        &file="machshft"
        &where="WHERE machshft.company EQ ip-company
                  AND machshft.machine EQ ip-machine"}
    IF op-shift EQ "" THEN DO:
        {custom/getshift.i
            &file="shifts"
            &where="WHERE shifts.company EQ ip-company"}
    END.
END PROCEDURE.

PROCEDURE Missing-Shift:
    DEFINE INPUT  PARAMETER ip-company      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ip-machine      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ip-startshift   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ip-endshift     AS CHARACTER NO-UNDO.
    
    DEFINE OUTPUT PARAMETER op-missingshift AS CHARACTER NO-UNDO.

    {custom/mssgshft.i
        &file="machshft"
        &where="AND machshft.machine EQ ip-machine"}
    {custom/mssgshft.i
        &file="shifts"}
END PROCEDURE.

PROCEDURE Shift-Data:
    DEFINE INPUT  PARAMETER ip-company   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ip-machine   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ip-shift     AS CHARACTER NO-UNDO.
    
    DEFINE OUTPUT PARAMETER op-starttime AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER op-endtime   AS INTEGER NO-UNDO.

    {custom/shftdata.i
        &file="machshft"
        &where="machshft.company EQ ip-company
            AND machshft.machine EQ ip-machine AND "}
    IF op-starttime NE 0 OR op-endtime NE 0 THEN RETURN.
    {custom/shftdata.i
        &file="shifts"
        &where="shifts.company EQ ip-company AND "}
END PROCEDURE.
