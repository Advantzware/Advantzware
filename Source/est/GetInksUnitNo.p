
/*------------------------------------------------------------------------
    File        : GetInksUnitNo.p
    Purpose     : 

    Syntax      :

    Description : Determines the Unit no for Inks/Pack Tab

    Author(s)   : Sakshi.Singh 
    Created     : Mon Sep 13 05:54:29 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */
DEFINE        PARAMETER BUFFER ipbf-eb FOR eb.
DEFINE INPUT  PARAMETER ipiIndex       AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER ipcInkCode     AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opiCalcUnitNo  AS INTEGER NO-UNDO.

DEFINE VARIABLE iLastBlank              AS INTEGER NO-UNDO.
DEFINE VARIABLE iUnitNo                 AS INTEGER NO-UNDO.
DEFINE VARIABLE iCnt                    AS INTEGER NO-UNDO.
DEFINE VARIABLE iLastUnitUsed           AS INTEGER NO-UNDO.
DEFINE VARIABLE IsItemInk               AS LOGICAL NO-UNDO.
DEFINE VARIABLE lInkFoundinCurrentBlank AS LOGICAL NO-UNDO.


DEFINE BUFFER bf-eb    FOR eb.
DEFINE BUFFER bf-item  FOR item.

/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */


FIND FIRST bf-Item NO-LOCK
    WHERE bf-Item.company EQ ipbf-eb.company
    AND bf-Item.i-no    EQ ipcInkCode
    AND INDEX("IV",bf-Item.mat-type) GT 0 NO-ERROR. 
    
/* Check If Ink. Skip if Coatings */  
IF NOT AVAILABLE bf-Item OR bf-Item.ink-type EQ "A" THEN
    RETURN.
    
opiCalcUnitNo = ipiIndex.

