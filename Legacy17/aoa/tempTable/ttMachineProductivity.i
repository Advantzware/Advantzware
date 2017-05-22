/* ttMachineProductivity.i */

/* Machine Productivity.rpa */
DEFINE TEMP-TABLE ttMachineProductivity NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD msfPerHr       AS DECIMAL   LABEL "1000 Sq Ft per Total Mach Hr" FORMAT "->,>>>,>>9.999"
    FIELD sqFtPerSetup   AS DECIMAL   LABEL "1000 Sq Ft per Setup"         FORMAT "->,>>>,>>9.999"
    FIELD sqFtPiece      AS DECIMAL   LABEL "Sq Ft per Piece"              FORMAT "->,>>>,>>9.999"
    FIELD manHrPerSqFt   AS DECIMAL   LABEL "Man Hr per 1000 Sq Ft"        FORMAT "->,>>>,>>9.999"
    FIELD machine        AS CHARACTER LABEL "Machine Specs"                FORMAT "x(6)"
    FIELD piecesPerHr    AS DECIMAL   LABEL "Pieces per Total Mach Hr"     FORMAT "->,>>>,>>9.999"
    FIELD piecesRunHr    AS DECIMAL   LABEL "Pieces per Run Mach Hr"       FORMAT "->,>>>,>>9.999"
    FIELD piecesPerSetup AS DECIMAL   LABEL "Pieces per Setup"             FORMAT "->,>>>,>>9.999"
    FIELD minPerSetup    AS DECIMAL   LABEL "Minutes per Setup"            FORMAT "->,>>>,>>9.999"
    FIELD avgCrew        AS DECIMAL   LABEL "Avg Crew"                     FORMAT "->,>>>,>>9.999"
    FIELD downTimePct    AS DECIMAL   LABEL "Down Time Pct"                FORMAT "->,>>>,>>9.999"
    FIELD xxSetups       AS INTEGER   LABEL "Setups"                       FORMAT ">>>>>9"
    FIELD xxPieces       AS DECIMAL   LABEL "Pieces"                       FORMAT ">,>>>,>>9"
    FIELD xxTotalMSF     AS DECIMAL   LABEL "Total MSF"                    FORMAT ">>,>>9.99"
    FIELD xxDTActHr      AS DECIMAL   LABEL "Act D/T Hr"                   FORMAT ">>>>9.99"
    FIELD xxMRActHr      AS DECIMAL   LABEL "MR Act Hr"                    FORMAT ">>>>9.99"
    FIELD xxRunActHr     AS DECIMAL   LABEL "Run Act Hr"                   FORMAT ">>>>9.99"
    FIELD xxTotActHrs    AS DECIMAL   LABEL "Total Act Hrs"                FORMAT ">>>>9.99"
    FIELD xxTotLabHrs    AS DECIMAL   LABEL "Total Labor Hrs"              FORMAT "->,>>>,>>9.999"
    FIELD xxTotMRHrs     AS DECIMAL   LABEL "Total MR Hrs"                 FORMAT "->>>9.99"
    FIELD xxTotRunHrs    AS DECIMAL   LABEL "Total MR Hrs"                 FORMAT "->>>9.99"
        INDEX sortBy IS PRIMARY rowType machine
        .
