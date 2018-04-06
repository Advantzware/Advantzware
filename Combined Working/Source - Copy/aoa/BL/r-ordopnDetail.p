/* r-ordopnDetail.p */

{aoa/tempTable/ttOpenOrderReportDetail.i}

DEFINE OUTPUT PARAMETER TABLE FOR ttOpenOrderReportDetail.
{aoa/includes/aoaInputDefParams.i}

DEFINE VARIABLE tmpFile AS CHARACTER NO-UNDO.
    
tmpFile = SEARCH("aoa/tmp/ttOpenOrderReportDetail." +
                 ipcCompany + "." +
                 STRING(ipiBatch) + "." +
                 ipcUserID + ".dat"
                 ).

IF tmpFile NE ? THEN DO:
    INPUT FROM VALUE(tmpFile).
    REPEAT:
        CREATE ttOpenOrderReportDetail.
        IMPORT ttOpenOrderReportDetail.
    END. /* repeat */
    INPUT CLOSE.
    IF ttOpenOrderReportDetail.xxIndex EQ 0 THEN
    DELETE ttOpenOrderReportDetail.
END. /* tmpfile ne ? */

