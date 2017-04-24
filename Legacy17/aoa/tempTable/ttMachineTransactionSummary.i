/* ttMachineTransactionSummary.i */

/* Machine Transaction Summary.rpa */
DEFINE TEMP-TABLE ttMachineTransactionSummary NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD machine          AS CHARACTER LABEL "Machine"       FORMAT "x(6)"
    FIELD custPartNo       AS CHARACTER LABEL "Cust Part"     FORMAT "x(15)"
    FIELD custName         AS CHARACTER LABEL "Customer"      FORMAT "x(30)"
    FIELD jobNumber        AS CHARACTER LABEL "Job"           FORMAT "x(10)"
    FIELD mrHours          AS DECIMAL   LABEL "MR Hrs"        FORMAT ">>>9.99"
    FIELD runHours         AS DECIMAL   LABEL "Run Hrs"       FORMAT ">>>9.99"
    FIELD dtHours          AS DECIMAL   LABEL "DT Hrs"        FORMAT ">>>9.99"
    FIELD totalHours       AS DECIMAL   LABEL "Tot Hrs"       FORMAT ">>>9.99"
    FIELD laborHours       AS DECIMAL   LABEL "Lab Hrs"       FORMAT ">>>9.99"
    FIELD netPieces        AS INTEGER   LABEL "Net Pieces"    FORMAT ">>>,>>9"
    FIELD piecesPerHour    AS INTEGER   LABEL "Pieces Hr"     FORMAT ">>>,>>9"
    FIELD numberOn         AS INTEGER   LABEL "Number On"     FORMAT ">>>,>>9"
    FIELD kicks            AS INTEGER   LABEL "Kicks"         FORMAT ">>>,>>9"
    FIELD piecesPerManHour AS INTEGER   LABEL "Pieces Man Hr" FORMAT ">>>,>>9"
    FIELD mrWaste          AS INTEGER   LABEL "MR Waste"      FORMAT ">>>,>>9"
    FIELD runWaste         AS INTEGER   LABEL "Run Waste"     FORMAT ">>>,>>9"
    FIELD totalWaste       AS INTEGER   LABEL "Tot Waste"     FORMAT ">>>,>>9"
    FIELD wastePct         AS DECIMAL   LABEL "Waste Pct"     FORMAT ">>>9.99"
        INDEX sortBy IS PRIMARY rowType machine jobNumber
        .
