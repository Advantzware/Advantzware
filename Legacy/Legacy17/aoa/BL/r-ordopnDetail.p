/* r-ordopnDetail.p */

{aoa/tempTable/ttOpenOrderReportDetail.i}

DEFINE OUTPUT PARAMETER TABLE FOR ttOpenOrderReportDetail.
{aoa/includes/aoaInputDefParams.i}

DEFINE VARIABLE tmpFile AS CHARACTER NO-UNDO.

DEFINE BUFFER bttOpenOrderReportDetail FOR ttOpenOrderReportDetail.
    
tmpFile = SEARCH("AOA.ttOpenOrderReportDetail."
        + ipcCompany       + "."
        + STRING(ipiBatch) + "."
        + ipcUserID        + ".dat"
        ).
IF tmpFile NE ? THEN DO:
    CREATE bttOpenOrderReportDetail.
    INPUT FROM VALUE(tmpFile).
    REPEAT:
        IMPORT bttOpenOrderReportDetail.
        CREATE ttOpenOrderReportDetail.
        BUFFER-COPY bttOpenOrderReportDetail TO ttOpenOrderReportDetail.
    END. /* repeat */
    INPUT CLOSE.
END. /* tmpfile ne ? */
