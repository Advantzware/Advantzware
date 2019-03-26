
/*------------------------------------------------------------------------
    File        : EstimateProcsOld.p
    Purpose     : Start moving some repetitive code into common procedures

    Syntax      :

    Description : Temporary Solution that will be replaced by call to Proc within Persistent
                    EstimateProcs.p
    Author(s)   : BV
    Created     : Thu Jun 14 18:19:14 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcBaseDir AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcSubDir AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

RUN GetEstimateDir (ipcCompany, OUTPUT opcBaseDir, OUTPUT opcSubDir).


/* **********************  Internal Procedures  *********************** */

PROCEDURE GetEstimateDir:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcBaseDir AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcSubDir AS CHARACTER NO-UNDO.

DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.

RUN sys/ref/nk1Look.p(INPUT ipcCompany,
                      INPUT "CEBrowse",
                      INPUT "C",
                      INPUT NO,
                      INPUT NO,
                      INPUT "",
                      INPUT "",
                      OUTPUT opcBaseDir,
                      OUTPUT lFound).
RUN sys/ref/nk1Look.p(INPUT ipcCompany,
                      INPUT "CEBrowse",
                      INPUT "D",
                      INPUT NO,
                      INPUT NO,
                      INPUT "",
                      INPUT "",
                      OUTPUT opcSubDir,
                      OUTPUT lFound).

IF opcBaseDir EQ "" THEN
   opcBaseDir = "users\".

ASSIGN 
      opcBaseDir = REPLACE(opcBaseDir,"/","\")  /*replace slashes in wrong direction*/
      opcBaseDir = TRIM(opcBaseDir,"\") + "\"  /*ensure there is a slash on the end*/
      .

IF DEC(opcSubDir) GT 0 THEN DO:
    opcSubDir = opcBaseDir + opcSubDir + "\".
    FILE-INFO:FILE-NAME = opcSubDir.
    IF FILE-INFO:FULL-PATHNAME = ? THEN
        OS-CREATE-DIR VALUE(opcSubDir).        
END.
ELSE 
    opcSubDir = opcBaseDir.


END PROCEDURE.

