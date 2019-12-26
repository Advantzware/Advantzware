
/*------------------------------------------------------------------------
    File        : EstimateProcs.p
    Purpose     : Start moving some repetitive code into common procedures

    Syntax      :

    Description : Will houses common procedures for calculating estimates and jobs

    Author(s)   : BV
    Created     : Thu Jun 14 18:19:14 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcBaseDir AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcSubDir  AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */

RUN pGetEstimateDir (ipcCompany, OUTPUT opcBaseDir, OUTPUT opcSubDir).

/* **********************  Internal Procedures  *********************** */

PROCEDURE pGetEstimateDir:
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcBaseDir AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcSubDir  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dSubDir AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL NO-UNDO.
    
    RUN sys/ref/nk1Look.p (
        ipcCompany,
        "CEBrowse",
        "C",
        NO,
        NO,
        "",
        "",
        OUTPUT opcBaseDir,
        OUTPUT lFound
        ).
    RUN sys/ref/nk1Look.p (
        ipcCompany,
        "CEBrowse",
        "D",
        NO,
        NO,
        "",
        "",
        OUTPUT opcSubDir,
        OUTPUT lFound
        ).    
    IF opcBaseDir EQ "" THEN
    opcBaseDir = "users\".
    
    ASSIGN 
          opcBaseDir = REPLACE(opcBaseDir,"/","\") /*replace slashes in wrong direction*/
          opcBaseDir = TRIM(opcBaseDir,"\") + "\"  /*ensure there is a slash on the end*/
          dSubDir    = DECIMAL(opcSubDir)
          .
    
    IF dSubDir EQ 0 THEN
    dSubDir = YEAR(TODAY) + MONTH(TODAY) / 100.

    ASSIGN
        opcSubDir = STRING(dSubDir,"9999.99")
        opcSubDir = opcBaseDir + opcSubDir + "\"
        .
    FILE-INFO:FILE-NAME = opcSubDir.
    IF FILE-INFO:FULL-PATHNAME EQ ? THEN
    OS-CREATE-DIR VALUE(opcSubDir).        

END PROCEDURE.
