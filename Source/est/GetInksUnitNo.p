
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

DEFINE VARIABLE iLastBlank    AS INTEGER NO-UNDO.
DEFINE VARIABLE iUnitNo       AS INTEGER NO-UNDO.
DEFINE VARIABLE iCnt          AS INTEGER NO-UNDO.
DEFINE VARIABLE iLastUnitUsed AS INTEGER NO-UNDO.

DEFINE BUFFER bf-ef    FOR ef.
DEFINE BUFFER bf-eb    FOR eb.
DEFINE BUFFER bf-item  FOR item.

/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */



iLastBlank = IF ipbf-eb.blank-no = 1 THEN 1 ELSE ipbf-eb.blank-no - 1.
    
FIND FIRST bf-ef NO-LOCK
    WHERE bf-ef.company = ipbf-eb.company
      AND bf-ef.est-no  = ipbf-eb.est-no
      AND bf-ef.form-no = ipbf-eb.form-no NO-ERROR.
    
IF NOT AVAILABLE bf-ef THEN 
    RETURN.

/* Check If Ink. Skip if Coatings */
IF CAN-FIND (FIRST item NO-LOCK
    WHERE item.company EQ ipbf-eb.company
    AND item.i-no    EQ ipcInkCode
    AND item.ink-type EQ "A"
    AND INDEX("IV",item.mat-type) GT 0) THEN
    RETURN.
            
IF ipbf-eb.blank-no = 1 THEN
    iUnitNo = ipiIndex. 
        
ELSE
DO:
    FOR FIRST bf-eb NO-LOCK 
        WHERE bf-eb.company = bf-ef.company
        AND bf-eb.est-no  = bf-ef.est-no
        AND bf-eb.form-no = bf-ef.form-no
        AND bf-eb.blank-no = iLastBlank:
                      
        DO iCnt = 1 TO 17:
            IF bf-eb.i-code2[iCnt] EQ ipcInkCode THEN
            DO:
                iUnitNo = bf-eb.unitno[iCnt].
                LEAVE.
            END.
            iLastUnitUsed = MAX(iLastUnitUsed,bf-eb.unitno[iCnt]).
                    
        END. /* do iCnt */
    END. /* each bf-eb */
    
    IF iUnitNo = 0 THEN
    DO:
        RUN pCheckMaxUnit(BUFFER ipbf-eb, ipiIndex, INPUT-OUTPUT iLastUnitUsed).
        
        iUnitNo = (IF iLastUnitUsed NE 0 THEN iLastUnitUsed + 1 ELSE ipiIndex). 
    END. 
            
END. /* IF iBlank NE 1 THEN */  
        
ASSIGN
    opiCalcUnitNo = iUnitNo.
    
    
PROCEDURE pCheckMaxUnit PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf2-eb FOR eb.
    DEFINE INPUT  PARAMETER ipiIdx           AS INTEGER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER opiMaxUnit AS INTEGER NO-UNDO.

    DEFINE VARIABLE iExt      AS INTEGER NO-UNDO.
    DEFINE VARIABLE iTempUnit AS INTEGER NO-UNDO.

    DO iExt = 1 TO (ipiIdx - 1):
        iTempUnit = MAX(iTempUnit,ipbf2-eb.unitno[iExt]).
                    
    END. /* do iExt */
    
    opiMaxUnit = MAX(iTempUnit,opiMaxUnit).

END PROCEDURE.    
    