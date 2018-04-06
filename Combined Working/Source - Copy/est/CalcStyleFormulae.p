
/*------------------------------------------------------------------------
    File        : CalcStyleFormulae.p
    Purpose     : Given a RowID for eb, will calculate the Formulae in the Style File based on eb dimensions 

    Syntax      :Define new shared temp-table formule
                    field formule as decimal extent 12.
                    run est/CalcStyleFormulae.p (rowid(eb).

    Description : Calculates the 12 formulas in the style file.  Replaces u2kinc1c.p and u2kinc2c.p.

    Author(s)   : BV
    Created     : Tue Jun 20 08:50:05 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipriEb               AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER iplRound             AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER iplDecimal           AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipdDecimalFactor     AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER ipdConversionFactor  AS DECIMAL NO-UNDO.


DEFINE VARIABLE iIndex        AS INTEGER NO-UNDO.
DEFINE VARIABLE dSquareBoxFit AS DECIMAL NO-UNDO.
DEFINE VARIABLE dScores       AS DECIMAL NO-UNDO.



/*REFACTOR - consider better temp-table with more descriptive fields*/
DEFINE SHARED TEMP-TABLE formule 
    FIELD formule AS DECIMAL EXTENT 12.


/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetSquareBoxFit RETURNS DECIMAL 
    ( ipcStyle AS CHARACTER, ipcFlute AS CHARACTER, ipdKFrac AS DECIMAL  ) FORWARD.


/* ***************************  Main Block  *************************** */

FIND eb WHERE ROWID(eb) = ipriEb NO-LOCK.
FIND FIRST style NO-LOCK  
    WHERE style.company EQ eb.company 
    AND style.style EQ eb.style
    NO-ERROR.
IF NOT AVAILABLE style THEN RETURN.
    
/*REFACTOR - consider better temp-table with more descriptive fields*/
FIND FIRST formule NO-ERROR.
IF NOT AVAILABLE formule THEN 
    CREATE formule.
    
        
dSquareBoxFit = fGetSquareBoxFit(eb.style, eb.flute, ipdConversionFactor).  /*REFACTOR - PURPOSE?*/
    
/*Set Scoring Values based on Tab in and Joint Material*/
IF style.type <> "F" THEN /*If Not Foam*/
    RUN pApplyProperScoring (ROWID(eb), ipdConversionFactor).
 
/*Calculate all 12 forumlae in the Style file*/        
DO iIndex = 1 TO 12:
    IF iIndex MOD 2 EQ 1 OR iIndex EQ 12 THEN /*If Width formula (odd value) or Die Rule Formula (12)*/
        dScores = eb.k-wid.             
    ELSE /*Length*/
        dScores = eb.k-len.
         
    RUN pCalcFormula(style.formula[iIndex],     /*Style Formula*/
        iIndex,                    /*formula number*/
        iplRound,                    /*Round Logical from NK1 ROUND*/
        iplDecimal,                  /*Show Decimals from NK1 CESCRN*/
        ipdDecimalFactor,            /*Multiplier for decimal display from NK1 CESCRN*/
        eb.len,                    /* 'L' - Inner Length*/
        eb.wid,                    /* 'W' - Inner Width*/
        eb.dep,                    /* 'D' - Inner Depth*/
        dScores,                   /* 'S' - Scores on Width or Length*/
        eb.tuck,                   /* 'T' - Tuck*/
        eb.gluelap,                /* 'J' - Joint Tab Width*/
        eb.fpanel,                 /* 'B' - Bottom Flap*/    
        eb.dust,                   /* 'F' - Flap - Top/Dust*/
        eb.lock,                   /* 'O' - Lock Tab*/
        dSquareBoxFit,             /* 'I' - Sq Box */
        OUTPUT formule[iIndex]).                                           
END. /*iIndex 1 to 12*/
    

/* **********************  Internal Procedures  *********************** */

PROCEDURE pApplyProperScoring:
    /*------------------------------------------------------------------------------
     Purpose:  Sets values for eb for Joint Tab Width, Scores on Width and
        Scores on Length based on Joint Material 
     Notes:  From b-estitm.w / calc-blank-size2
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriEB AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipdConversionFactor AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-eb FOR eb.
    
    DEFINE VARIABLE cLengthScoreField AS CHARACTER NO-UNDO.

    FIND bf-eb EXCLUSIVE-LOCK 
        WHERE ROWID(bf-eb) = ipriEB.
    
    /*Set the Joint Tab Width*/
    RUN pModifyScoring (bf-eb.style,
        bf-eb.flute,
        '1',
        ipdConversionFactor,
        NO,
        INPUT-OUTPUT bf-eb.gluelap).
                 

    /*Set the Scores on Length*/
    FIND FIRST item NO-LOCK  
        WHERE item.company EQ bf-eb.company
        AND item.i-no EQ bf-eb.adhesive
        NO-ERROR.
    IF AVAILABLE item THEN 
    DO:
        IF item.mat-type EQ "G" THEN  /*Glue Joint*/ 
            IF bf-eb.tab-in  THEN
                cLengthScoreField = '3'.
            ELSE
                cLengthScoreField = '4'.
        ELSE IF item.mat-type EQ "S" THEN /*Stitch/Staple Joint*/ 
                IF bf-eb.tab-in  THEN 
                    cLengthScoreField = '5'.
                ELSE 
                    cLengthScoreField = '6'.
            ELSE  /*Tape Joint*/ 
            DO:
                bf-eb.tab-in = NO.
                cLengthScoreField = '7'.
            END.
          
    END.
    ELSE 
    DO:
        bf-eb.tab-in = NO.
        cLengthScoreField = '7'.       
    END.
    RUN pModifyScoring (bf-eb.style,
        bf-eb.flute,
        cLengthScoreField,
        ipdConversionFactor,
        NO,
        INPUT-OUTPUT bf-eb.k-len).
    
    /*Set the Scores on Width*/
    IF bf-eb.len EQ bf-eb.wid THEN /*Square Box*/
        RUN pModifyScoring (bf-eb.style,
            bf-eb.flute,
            '2',
            ipdConversionFactor,
            YES, /*Square Box*/
            INPUT-OUTPUT bf-eb.k-wid).
    ELSE  
        RUN pModifyScoring (bf-eb.style,
            bf-eb.flute,
            '2',
            ipdConversionFactor,
            NO,
            INPUT-OUTPUT bf-eb.k-wid).             
 
END PROCEDURE.

PROCEDURE pCalcFormula:
    /*------------------------------------------------------------------------------
     Purpose:  Accepts Values for valid formula letters and the formula equation and returns a calculated value
     Notes: Deprecates kstylec.i
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFormula AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormulaNum AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplRound AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplDecimal AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdDecimalFactor AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdLength AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdWidth AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdDepth AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdScores AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdTuck AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdJointTabWidth AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdBottomFlap AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdTopDustFlap AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdLockTab AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdSquareBoxFit AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdValue AS DECIMAL NO-UNDO.

    DEFINE VARIABLE iIndex            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cOperations       AS CHARACTER EXTENT 100.
    DEFINE VARIABLE dCalculations     AS DECIMAL   EXTENT 100.
    DEFINE VARIABLE iNextOperation    AS INTEGER.
    DEFINE VARIABLE cFormulaNumber    AS CHARACTER.
    DEFINE VARIABLE cFormulaCharacter AS CHARACTER.
    DEFINE VARIABLE lInsideParen      AS LOGICAL   INIT NO NO-UNDO.

    /* Remove any blank/space or invalid character in formula */
    DO iIndex = 1 TO LENGTH(ipcFormula):
        IF KEYCODE(SUBSTRING(ipcFormula,iIndex,1)) = 32  /*space character*/ 
            OR INDEX("0123456789.+-*/LWDTFJBOSI", SUBSTRING(ipcFormula,iIndex,1)) = 0 /*Invalid*/ 
            THEN
            ipcFormula = SUBSTRING(ipcFormula,1,iIndex - 1) + SUBSTRING(ipcFormula,iIndex + 1).
    END.

    ASSIGN /*initialize arrays and counters*/
        dCalculations  = 0
        cOperations    = ""
        iNextOperation = 1.

    loop:
    DO iIndex = 1 TO LENGTH(ipcFormula):

        cFormulaCharacter = SUBSTRING(ipcFormula,iIndex,1).
        IF INDEX("+-/*",cFormulaCharacter) > 0 THEN
        DO:
            ASSIGN 
                cOperations[iNextOperation] = cFormulaCharacter
                iNextOperation              = iNextOperation + 1.
            NEXT.
        END.
        IF INDEX("LWDJTSFBOIXY",cFormulaCharacter) > 0 AND dCalculations[iNextOperation] = 0 THEN 
        DO:
            IF cFormulaCharacter = "L" THEN dCalculations[iNextOperation] = ipdLength .
            IF cFormulaCharacter = "W" THEN dCalculations[iNextOperation] = ipdWidth .
            IF cFormulaCharacter = "D" THEN dCalculations[iNextOperation] = ipdDepth .
            IF cFormulaCharacter = "J" THEN dCalculations[iNextOperation] = ipdJointTabWidth .
            IF cFormulaCharacter = "T" THEN dCalculations[iNextOperation] = ipdTuck .
            IF cFormulaCharacter = "S" THEN dCalculations[iNextOperation] = ipdScores .
            IF cFormulaCharacter = "F" THEN dCalculations[iNextOperation] = ipdTopDustFlap .
            IF cFormulaCharacter = "B" THEN dCalculations[iNextOperation] = ipdBottomFlap .
            IF cFormulaCharacter = "O" THEN dCalculations[iNextOperation] = ipdLockTab .
            IF cFormulaCharacter = "I" THEN dCalculations[iNextOperation] = ipdSquareBoxFit .
            NEXT.
        END.
        ELSE IF INDEX("LWDJTSFBOIXY",cFormulaCharacter) > 0 AND dCalculations[iNextOperation] NE 0 THEN 
            DO:
                IF cFormulaCharacter = "L" THEN dCalculations[iNextOperation] = ipdLength * dCalculations[iNextOperation].
                IF cFormulaCharacter = "W" THEN dCalculations[iNextOperation] = ipdWidth * dCalculations[iNextOperation].
                IF cFormulaCharacter = "D" THEN dCalculations[iNextOperation] = ipdDepth * dCalculations[iNextOperation].
                IF cFormulaCharacter = "J" THEN dCalculations[iNextOperation] = ipdJointTabWidth * dCalculations[iNextOperation].
                IF cFormulaCharacter = "T" THEN dCalculations[iNextOperation] = ipdTuck * dCalculations[iNextOperation].
                IF cFormulaCharacter = "S" THEN dCalculations[iNextOperation] = ipdScores * dCalculations[iNextOperation].
                IF cFormulaCharacter = "F" THEN dCalculations[iNextOperation] = ipdTopDustFlap * dCalculations[iNextOperation].
                IF cFormulaCharacter = "B" THEN dCalculations[iNextOperation] = ipdBottomFlap * dCalculations[iNextOperation].
                IF cFormulaCharacter = "O" THEN dCalculations[iNextOperation] = ipdLockTab * dCalculations[iNextOperation].
                IF cFormulaCharacter = "I" THEN dCalculations[iNextOperation] = ipdSquareBoxFit * dCalculations[iNextOperation].
                NEXT.
            END.
            ELSE /*Numeric Value in formula*/
            DO:
                cFormulaNumber = "".
                DO WHILE (KEYCODE(cFormulaCharacter) >= 48 AND keycode(cFormulaCharacter) <= 57) OR keycode(cFormulaCharacter) = 46:
                    ASSIGN 
                        cFormulaNumber    = cFormulaNumber + cFormulaCharacter
                        iIndex            = iIndex + 1
                        cFormulaCharacter = SUBSTRING(ipcFormula,iIndex,1).
                END.
                iIndex = iIndex - 1.
                IF dCalculations[iNextOperation] NE 0
                    THEN dCalculations[iNextOperation] = DECIMAL(cFormulaNumber) * dCalculations[iNextOperation].
                ELSE dCalculations[iNextOperation] = DECIMAL(cFormulaNumber).
            END.
    END.
    
    /*Process NK1 ROUND*/
    DO iIndex = 1 TO EXTENT(dCalculations):
        dCalculations[iIndex] = dCalculations[iIndex] * ipdDecimalFactor /*16 or 32*/.
        IF iplRound THEN
        DO:
        {sys/inc/roundup.i dCalculations[iIndex]}
        END.
        ELSE IF NOT iplDecimal THEN
                dCalculations[iIndex] = TRUNCATE(dCalculations[iIndex],0).
        dCalculations[iIndex] = dCalculations[iIndex] / ipdDecimalFactor /*16 or 32*/.
    END.

    opdValue = dCalculations[1].
    DO iIndex = 2 TO EXTENT(cOperations):
        IF cOperations[iIndex - 1] = "+" THEN 
            opdValue = opdValue + dCalculations[iIndex].
        ELSE IF cOperations[iIndex - 1] = "-" THEN 
                opdValue = opdValue - dCalculations[iIndex].
    END.


END PROCEDURE.



PROCEDURE pModifyScoring:
    /*------------------------------------------------------------------------------
     Purpose: Modifies the Score Value for given style, flute and Field number
     Notes: Replaces u2estc.i and u2estic.p
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcStyle AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFlute AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcField AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdConversionFactor AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER iplSquareBoxFit AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdScoring AS DECIMAL NO-UNDO.


    FIND FIRST reftable NO-LOCK 
        WHERE reftable.reftable EQ "STYFLU"
        AND reftable.company  EQ ipcStyle
        AND reftable.loc      EQ ipcFlute
        AND reftable.code     EQ ipcField
        NO-ERROR.
    IF NOT AVAILABLE reftable THEN LEAVE.    
    iopdScoring = TRUNCATE(reftable.val[13],0) +
        ((reftable.val[13] - TRUNCATE(reftable.val[13],0)) * ipdConversionFactor).

    IF iplSquareBoxFit THEN 
    DO:
        FIND FIRST reftable NO-LOCK 
            WHERE reftable.reftable EQ "STYFLU"
            AND reftable.company  EQ ipcStyle
            AND reftable.loc      EQ ipcFlute
            AND reftable.code     EQ "DIM-FIT"
            NO-ERROR.
        IF AVAILABLE reftable THEN 
            iopdScoring = iopdScoring - (reftable.val[1] / 6.25 * ipdConversionFactor * 2).
    END.


END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION fGetSquareBoxFit RETURNS DECIMAL 
    ( ipcStyle AS CHARACTER, ipcFlute AS CHARACTER, ipdConversionFactor AS DECIMAL ):
    /*------------------------------------------------------------------------------
     Purpose:  Returns the value of Sq Box Fit for the Style (*REFACTOR*)
     Notes: Original Note: "JLF added 02/28/96"
    ------------------------------------------------------------------------------*/	

    DEFINE VARIABLE dResult AS DECIMAL NO-UNDO.
    
    FIND FIRST reftable NO-LOCK 
        WHERE reftable.reftable EQ "STYFLU"
        AND reftable.company  EQ ipcStyle
        AND reftable.loc      EQ ipcFlute
        AND reftable.code     EQ "DIM-FIT"
        NO-ERROR.

    dResult = IF AVAILABLE reftable THEN (reftable.val[1] / 6.25 * ipdConversionFactor) ELSE 0.
    
    RETURN dResult.
		
END FUNCTION.




