
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


iLastBlank = ipbf-eb.blank-no - 1.

FIND FIRST bf-Item NO-LOCK
    WHERE bf-Item.company EQ ipbf-eb.company
    AND bf-Item.i-no    EQ ipcInkCode
    AND INDEX("IV",bf-Item.mat-type) GT 0 NO-ERROR. 
    
/* Check If Ink. Skip if Coatings */  
IF NOT AVAILABLE bf-Item OR bf-Item.ink-type EQ "A" THEN
    RETURN. 
 
IF bf-Item.mat-type EQ "I" THEN
    IsItemInk = YES.
  
    
RELEASE bf-Item.    
    

/* Check if found in current Blank. If found the treat it as seperate unit */
DO iCnt = 1 TO (ipiIndex - 1):
    IF ipbf-eb.i-code2[iCnt] EQ ipcInkCode THEN
    DO:
        lInkFoundinCurrentBlank = YES.
        LEAVE.
    END.
END.
            
IF ipbf-eb.blank-no NE 1 AND NOT lInkFoundinCurrentBlank THEN
DO:
    FOR FIRST bf-eb NO-LOCK 
        WHERE bf-eb.company = ipbf-eb.company
        AND bf-eb.est-no  = ipbf-eb.est-no
        AND bf-eb.form-no = ipbf-eb.form-no
        AND bf-eb.blank-no = iLastBlank:
                      
        DO iCnt = 1 TO 17:
            IF bf-eb.i-code2[iCnt] EQ ipcInkCode THEN
            DO:
                iUnitNo = bf-eb.unitno[iCnt].
                LEAVE.
            END.
            
            /* Count Unit in same Item Type */
            FIND FIRST bf-Item NO-LOCK
                WHERE bf-Item.company EQ bf-eb.company
                AND bf-Item.i-no    EQ bf-eb.i-code2[iCnt]
                AND INDEX("IV",bf-Item.mat-type) GT 0 NO-ERROR. 
            
            IF NOT AVAILABLE bf-Item OR bf-Item.ink-type EQ "A" THEN
                NEXT.
                    
            IF IsItemInk AND bf-Item.mat-type NE "I" THEN
                NEXT. 
                
            IF NOT IsItemInk AND bf-Item.mat-type EQ "I" THEN
                NEXT.
            
            iLastUnitUsed = MAX(iLastUnitUsed, bf-eb.unitno[iCnt]).
                    
        END. /* do iCnt */
    END. /* each bf-eb */
            
END. /* IF iBlank NE 1 THEN */  
 
    
IF iUnitNo = 0 THEN
DO:
    RUN pCheckMaxUnit(BUFFER ipbf-eb, ipiIndex, IsItemInk, INPUT-OUTPUT iLastUnitUsed).
        
    iUnitNo = iLastUnitUsed + 1. 
END.  
        
ASSIGN
    opiCalcUnitNo = iUnitNo.
    
    
    
PROCEDURE pCheckMaxUnit PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Check Max unit on current EB record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf2-eb FOR eb.
    DEFINE INPUT  PARAMETER ipiIdx           AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER iplCheckInk      AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER opiMaxUnit AS INTEGER NO-UNDO.

    DEFINE VARIABLE iExt      AS INTEGER NO-UNDO.
    DEFINE VARIABLE iTempUnit AS INTEGER NO-UNDO.

    DEFINE BUFFER bf2-Item FOR item.
    
     
    DO iExt = 1 TO (ipiIdx - 1):
        
        /* Check last Unit in same Item Type */
        FIND FIRST bf2-Item NO-LOCK
            WHERE bf2-Item.company EQ ipbf2-eb.company
              AND bf2-Item.i-no    EQ ipbf2-eb.i-code2[iExt]
              AND INDEX("IV",bf2-Item.mat-type) GT 0 NO-ERROR. 
            
        IF NOT AVAILABLE bf2-Item OR bf2-Item.ink-type EQ "A" THEN
            NEXT.
                    
        IF iplCheckInk AND bf2-Item.mat-type NE "I" THEN
            NEXT. 
                
        IF NOT iplCheckInk AND bf2-Item.mat-type EQ "I" THEN
            NEXT. 
        
        iTempUnit = MAX(iTempUnit,ipbf2-eb.unitno[iExt]).
                    
    END. /* do iExt */
    
    opiMaxUnit = MAX(iTempUnit,opiMaxUnit).

END PROCEDURE.    
    