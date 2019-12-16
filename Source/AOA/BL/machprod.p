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
        ttMachineProductivity.zzSetups       = ttMachineProductivity.zzSetups    + 1
        ttMachineProductivity.sqFtPiece      = ttMachineProductivity.sqFtPiece   + ttProductionAnalysis.sqFtPiece
        ttMachineProductivity.avgCrew        = ttMachineProductivity.avgCrew     + ttProductionAnalysis.crew
        ttMachineProductivity.zzPieces       = ttMachineProductivity.zzPieces    + ttProductionAnalysis.pieces
        ttMachineProductivity.zzTotalMSF     = ttMachineProductivity.zzTotalMSF  + ttProductionAnalysis.totalMSF
        ttMachineProductivity.zzDTActHr      = ttMachineProductivity.zzDTActHr   + ttProductionAnalysis.dtActHr
        ttMachineProductivity.zzMRActHr      = ttMachineProductivity.zzMRActHr   + ttProductionAnalysis.mrActHr
        ttMachineProductivity.zzRunActHr     = ttMachineProductivity.zzRunActHr  + ttProductionAnalysis.runActHr
        ttMachineProductivity.zzTotActHrs    = ttMachineProductivity.zzTotActHrs + ttProductionAnalysis.totActHrs
        ttMachineProductivity.zzTotLabHrs    = ttMachineProductivity.zzTotLabHrs + ttProductionAnalysis.totLabHrs
        ttMachineProductivity.zzTotMRHrs     = ttMachineProductivity.zzTotMRHrs  + ttProductionAnalysis.zzTotMRHrs
        ttMachineProductivity.zzTotRunHrs    = ttMachineProductivity.zzTotRunHrs + ttProductionAnalysis.zzTotRunHrs
        .
END. /* each ttMachineProductivity */

FOR EACH ttMachineProductivity
    :
    ASSIGN
        ttMachineProductivity.msfPerHr       = ttMachineProductivity.zzTotalMSF  / ttMachineProductivity.zzTotActHrs
        ttMachineProductivity.sqFtPerSetup   = ttMachineProductivity.zzTotalMSF  / ttMachineProductivity.zzSetups
        ttMachineProductivity.sqFtPiece      = ttMachineProductivity.sqFtPiece   / ttMachineProductivity.zzPieces    * 1000
        ttMachineProductivity.manHrPerSqFt   = ttMachineProductivity.zzTotLabHrs / ttMachineProductivity.zzTotalMSF
        ttMachineProductivity.piecesPerHr    = ttMachineProductivity.zzPieces    / ttMachineProductivity.zzTotActHrs
        ttMachineProductivity.piecesRunHr    = ttMachineProductivity.zzPieces    / ttMachineProductivity.zzRunActHr
        ttMachineProductivity.piecesPerSetup = ttMachineProductivity.zzPieces    / ttMachineProductivity.zzSetups
        ttMachineProductivity.minPerSetup    = ttMachineProductivity.zzMRActHr   / ttMachineProductivity.zzSetups    * 60
        ttMachineProductivity.avgCrew        = ttMachineProductivity.avgCrew     / ttMachineProductivity.zzSetups
        ttMachineProductivity.downTimePct    = ttMachineProductivity.zzDTActHr   / ttMachineProductivity.zzTotActHrs * 100
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
