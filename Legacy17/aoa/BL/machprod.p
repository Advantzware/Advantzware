/*------------------------------------------------------------------------
  File: machprod.p
  Description: Business Logic
*/

/* ***************************  Definitions  ***************************/

/* Customer Inventory.rpa */
{aoa/tempTable/ttProductionAnalysis.i}
/* Machine Productivity.rpa */
{aoa/tempTable/ttMachineProductivity.i}
{sys/ref/CustList.i NEW}

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttMachineProductivity.
{aoa/includes/pMachineProductivity.i}

/* local variables */
DEFINE VARIABLE cStartDept AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndDept   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSort      AS CHARACTER NO-UNDO.

ASSIGN
    cStartDept = CHR(32)
    cEndDept   = CHR(254)
    cSort      = "Alphabetically"
    .

{aoa/BL/r-prodlys.i}

FOR EACH ttProductionAnalysis
    WHERE ttProductionAnalysis.rowType EQ "Data"
    BREAK BY ttProductionAnalysis.machine
    :
    IF FIRST-OF(ttProductionAnalysis.machine) THEN DO:
        CREATE ttMachineProductivity.
        ttMachineProductivity.machine = ttProductionAnalysis.machine.
    END. /* first-of */

    ASSIGN
        ttMachineProductivity.xxSetups       = ttMachineProductivity.xxSetups    + 1
        ttMachineProductivity.sqFtPiece      = ttMachineProductivity.sqFtPiece   + ttProductionAnalysis.sqFtPiece
        ttMachineProductivity.avgCrew        = ttMachineProductivity.avgCrew     + ttProductionAnalysis.crew
        ttMachineProductivity.xxPieces       = ttMachineProductivity.xxPieces    + ttProductionAnalysis.pieces
        ttMachineProductivity.xxTotalMSF     = ttMachineProductivity.xxTotalMSF  + ttProductionAnalysis.totalMSF
        ttMachineProductivity.xxDTActHr      = ttMachineProductivity.xxDTActHr   + ttProductionAnalysis.dtActHr
        ttMachineProductivity.xxMRActHr      = ttMachineProductivity.xxMRActHr   + ttProductionAnalysis.mrActHr
        ttMachineProductivity.xxRunActHr     = ttMachineProductivity.xxRunActHr  + ttProductionAnalysis.runActHr
        ttMachineProductivity.xxTotActHrs    = ttMachineProductivity.xxTotActHrs + ttProductionAnalysis.totActHrs
        ttMachineProductivity.xxTotLabHrs    = ttMachineProductivity.xxTotLabHrs + ttProductionAnalysis.totLabHrs
        ttMachineProductivity.xxTotMRHrs     = ttMachineProductivity.xxTotMRHrs  + ttProductionAnalysis.xxTotMRHrs
        ttMachineProductivity.xxTotRunHrs    = ttMachineProductivity.xxTotRunHrs + ttProductionAnalysis.xxTotRunHrs
        .
END. /* each ttMachineProductivity */

FOR EACH ttMachineProductivity
    :
    ASSIGN
        ttMachineProductivity.msfPerHr       = ttMachineProductivity.xxTotalMSF  / ttMachineProductivity.xxTotActHrs
        ttMachineProductivity.sqFtPerSetup   = ttMachineProductivity.xxTotalMSF  / ttMachineProductivity.xxSetups
        ttMachineProductivity.sqFtPiece      = ttMachineProductivity.sqFtPiece   / ttMachineProductivity.xxPieces    * 1000
        ttMachineProductivity.manHrPerSqFt   = ttMachineProductivity.xxTotLabHrs / ttMachineProductivity.xxTotalMSF
        ttMachineProductivity.piecesPerHr    = ttMachineProductivity.xxPieces    / ttMachineProductivity.xxTotActHrs
        ttMachineProductivity.piecesRunHr    = ttMachineProductivity.xxPieces    / ttMachineProductivity.xxRunActHr
        ttMachineProductivity.piecesPerSetup = ttMachineProductivity.xxPieces    / ttMachineProductivity.xxSetups
        ttMachineProductivity.minPerSetup    = ttMachineProductivity.xxMRActHr   / ttMachineProductivity.xxSetups    * 60
        ttMachineProductivity.avgCrew        = ttMachineProductivity.avgCrew     / ttMachineProductivity.xxSetups
        ttMachineProductivity.downTimePct    = ttMachineProductivity.xxDTActHr   / ttMachineProductivity.xxTotActHrs * 100
        .
    IF ttMachineProductivity.msfPerHr       EQ ? THEN ttMachineProductivity.msfPerHr       = 0.
    IF ttMachineProductivity.sqFtPerSetup   EQ ? THEN ttMachineProductivity.sqFtPerSetup   = 0.
    IF ttMachineProductivity.sqFtPiece      EQ ? THEN ttMachineProductivity.sqFtPiece      = 0.
    IF ttMachineProductivity.manHrPerSqFt   EQ ? THEN ttMachineProductivity.manHrPerSqFt   = 0.
    IF ttMachineProductivity.piecesPerHr    EQ ? THEN ttMachineProductivity.piecesPerHr    = 0.
    IF ttMachineProductivity.piecesRunHr    EQ ? THEN ttMachineProductivity.piecesRunHr    = 0.
    IF ttMachineProductivity.piecesPerSetup EQ ? THEN ttMachineProductivity.piecesPerSetup = 0.
    IF ttMachineProductivity.minPerSetup    EQ ? THEN ttMachineProductivity.minPerSetup    = 0.
    IF ttMachineProductivity.avgCrew        EQ ? THEN ttMachineProductivity.avgCrew        = 0.
    IF ttMachineProductivity.downTimePct    EQ ? THEN ttMachineProductivity.downTimePct    = 0.
END. /* each ttMachineProductivity */
