
/*------------------------------------------------------------------------
    File        : FormulaProcs.p
    Purpose     : Deprecate kstyle.i

    Syntax      :

    Description : Procedures and functions for processing formulas

    Author(s)   : BV
    Created     : Tue Dec 03 15:44:36 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/*DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcFormula AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipdL AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER ipdW AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER ipdD AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER ipdG AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER ipdT AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER ipdK AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER ipdF AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER ipdB AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER ipdO AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER ipdI AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opdCalculation AS DECIMAL NO-UNDO.*/

{system\FormulaProcs.i}

DEFINE VARIABLE gcValidNumbers   AS CHARACTER NO-UNDO INITIAL "0123456789.".
DEFINE VARIABLE gcValidOperators AS CHARACTER NO-UNDO INITIAL "+-/*".
DEFINE VARIABLE gcValidVariables AS CHARACTER NO-UNDO INITIAL "LWDTFJBOSI".
DEFINE VARIABLE gdDecimalFactor  AS DECIMAL   NO-UNDO INITIAL 6.25. 
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

//RUN ProcessStyleFormula(ipcCompany, ipcFormula, ipdL, ipdW, ipdD, ipdG, ipdT, ipdK, ipdF, ipdB, ipdO, ipdI, OUTPUT opdCalculation).

/* **********************  Internal Procedures  *********************** */

PROCEDURE CalculatePanels:
    /*------------------------------------------------------------------------------
     Purpose: Given an eb Rowid and ttPanel temp-table, calculate Panels
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriEb AS ROWID NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttPanel.

    DEFINE BUFFER bf-eb    FOR eb.
    DEFINE BUFFER bf-style FOR style.

    DEFINE VARIABLE dScoresW AS DECIMAL EXTENT 20.
    DEFINE VARIABLE cScoreTypesW AS CHARACTER EXTENT 20.
    DEFINE VARIABLE dScoresL AS DECIMAL EXTENT 20.
    DEFINE VARIABLE cScoreTypesL AS CHARACTER EXTENT 20.
    
    FIND FIRST bf-eb NO-LOCK 
        WHERE ROWID(bf-eb) EQ ipriEb
        NO-ERROR.
    IF NOT AVAILABLE bf-eb THEN RETURN.
    FIND FIRST bf-style NO-LOCK 
        WHERE bf-style.company EQ bf-eb.company
        AND bf-style.style EQ bf-eb.style
        NO-ERROR. 
    FOR EACH ttPanel EXCLUSIVE-LOCK
        BREAK BY ttPanel.cPanelType:
        IF FIRST-OF(ttPanel.cPanelType) AND ttPanel.cPanelType BEGINS "W" THEN 
            RUN pGetScoring(BUFFER bf-eb, YES, OUTPUT dScoresW, OUTPUT cScoreTypesW).
        IF FIRST-OF(ttPanel.cPanelType) AND NOT ttPanel.cPanelType BEGINS "W" THEN 
            RUN pGetScoring(BUFFER bf-eb, NO, OUTPUT dScoresL, OUTPUT cScoreTypesL).
        IF ttPanel.cPanelType BEGINS "W" THEN
            ASSIGN 
                ttPanel.dScoringAllowance = dScoresW[ttPanel.iPanelNum] * gdDecimalFactor
                ttPanel.cScoreType = cScoreTypesW[ttPanel.iPanelNum]
                .
        ELSE 
            ASSIGN 
                ttPanel.dScoringAllowance = dScoresL[ttPanel.iPanelNum] * gdDecimalFactor
                ttPanel.cScoreType = cScoreTypesL[ttPanel.iPanelNum]
                .
        RUN ProcessStyleFormula (bf-eb.company,
                                 ttPanel.cPanelFormula,
                                 bf-eb.len, //L
                                 bf-eb.wid, //W
                                 bf-eb.dep, //D
                                 bf-eb.gluelap, //G
                                 bf-eb.tuck, //T
                                 0, //K
                                 bf-eb.dust, //F
                                 bf-eb.fpanel, //B
                                 bf-eb.lock, //O
                                 IF AVAILABLE bf-style THEN bf-style.dim-fit ELSE 0, //I
                                 OUTPUT ttPanel.dPanelSize).
        ttPanel.dPanelSize = ttPanel.dPanelSize + ttPanel.dScoringAllowance.
    END.
END PROCEDURE.

PROCEDURE ParsePanels:
    /*------------------------------------------------------------------------------
     Purpose: Given a Style Formula, this will produce a simple temp table for each panel in the forumula
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFormula AS CHARACTER.
    DEFINE INPUT PARAMETER ipcPanelType AS CHARACTER.
    DEFINE OUTPUT PARAMETER TABLE FOR ttPanel.
    
    DEFINE VARIABLE iPanel        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iChar         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCharNextPlus AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cChar         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCharPrev     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCharNext     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPanelFormula AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lParenActive  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cParenFormula AS CHARACTER NO-UNDO.

    ASSIGN
        iChar     = 1
        iPanel    = 0
        cCharPrev = ''
        cCharNext = ''
        .
    
    /* Loop through each character in the formula (Style 'L' formula eg. L+W+L+W+J+S for RSC) */
    DO WHILE iPanel LE 20 AND iChar LE LENGTH(ipcFormula):
        ASSIGN  /* pick the previous and next character in formula */
            cChar     = SUBSTRING(ipcFormula,iChar,1)
            cCharPrev = IF iChar EQ 1 THEN '+' ELSE SUBSTRING(ipcFormula,iChar - 1,1)    
            cCharNext = IF iChar EQ LENGTH(ipcFormula) THEN '+' ELSE SUBSTRING(ipcFormula,iChar + 1,1).
              
        IF cCharPrev EQ '' THEN cCharPrev = '+'.
        IF cCharNext EQ '' THEN cCharNext = '+'.
             
        IF cChar NE '+' AND cCharPrev EQ '+' THEN 
        DO: 
            iPanel = iPanel + 1.
            CREATE ttPanel.
            ASSIGN 
                ttPanel.cPanelType    = ipcPanelType
                ttPanel.iPanelNum     = iPanel
                ttPanel.cPanelFormula = cChar
                .
        END.
        ELSE IF cChar NE '+' THEN 
            DO:  
                FIND FIRST ttPanel
                    WHERE ttPanel.cPanelType EQ ipcPanelType
                    AND ttPanel.iPanelNum EQ iPanel NO-ERROR.
                IF AVAILABLE ttPanel THEN
                    ttPanel.cPanelFormula = ttPanel.cPanelFormula + cChar.
            END. /*valid panel character*/
        iChar = iChar + 1.
    END. /*loop through each character in the formula*/ 
    
    /*Combine Parentheses*/
    IF CAN-FIND(FIRST ttPanel WHERE INDEX(ttPanel.cPanelFormula,"(") GT 0) THEN 
    DO:
        FOR EACH ttPanel EXCLUSIVE-LOCK
            WHERE ttPanel.cPanelType EQ ipcPanelType
            BY ttPanel.iPanelNum DESCENDING:
            IF SUBSTRING(ttPanel.cPanelFormula, LENGTH(ttPanel.cPanelFormula), 1) EQ ")" THEN 
            DO:
                ASSIGN 
                    lParenActive  = YES
                    cParenFormula = "+" + SUBSTRING(ttPanel.cPanelFormula, 1, LENGTH(ttPanel.cPanelFormula) - 1)
                    .
                DELETE ttPanel.
            END.
            ELSE IF SUBSTRING(ttPanel.cPanelFormula, 1, 1) EQ "(" THEN 
                DO:
                    ASSIGN 
                        lParenActive          = NO
                        cParenFormula         = SUBSTRING(ttPanel.cPanelFormula, 2, LENGTH(ttPanel.cPanelFormula) - 1) + cParenFormula
                        ttPanel.cPanelFormula = cParenFormula
                        .
                END.
                ELSE IF lParenActive THEN 
                    DO:
                        cParenFormula = "+" + ttPanel.cPanelFormula.
                        DELETE ttPanel.
                    END.
        END.
        
        /*Renumber Panels*/
        iPanel = 0.
        FOR EACH ttPanel EXCLUSIVE-LOCK 
            WHERE ttPanel.cPanelType EQ ipcPanelType
            BY ttPanel.iPanelNum:
            ASSIGN 
                iPanel            = iPanel + 1
                ttPanel.iPanelNum = iPanel
                .
        END.
    END.

END PROCEDURE.

PROCEDURE pGetScoring PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given an eb buffer, style buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
    DEFINE INPUT PARAMETER iplWidth AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdScores AS DECIMAL EXTENT 20.
    DEFINE OUTPUT PARAMETER opcScoreTypes AS CHARACTER EXTENT 20.

    DEFINE BUFFER bf-item     FOR ITEM.
    DEFINE BUFFER bf-reftable FOR reftable.

    DEFINE VARIABLE cScoreSet AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndex    AS INTEGER   NO-UNDO.

    IF NOT AVAILABLE ipbf-eb THEN RETURN.
    IF iplWidth THEN cScoreSet = "2".
    ELSE 
    DO:
        FIND FIRST bf-item NO-LOCK 
            WHERE bf-item.company EQ ipbf-eb.company
            AND bf-item.i-no EQ ipbf-eb.adhesive
            NO-ERROR.
        cScoreSet = "7".
        IF AVAIL bf-item THEN
            IF bf-item.mat-type EQ "G" THEN 
                cScoreSet = IF ipbf-eb.tab-in THEN "3" ELSE "4".
            ELSE IF bf-item.mat-type EQ "S" THEN 
                    cScoreSet = IF ipbf-eb.tab-in THEN "5" ELSE "6".
    END.
    FIND FIRST bf-reftable
        WHERE bf-reftable.reftable EQ "STYSCORE"
        AND bf-reftable.company  EQ ipbf-eb.style
        AND bf-reftable.loc      EQ ipbf-eb.flute
        AND bf-reftable.code     EQ cScoreSet
        AND bf-reftable.code2    EQ ""
        NO-LOCK NO-ERROR.


    IF AVAIL bf-reftable THEN
    DO iIndex = 1 TO EXTENT(opcScoreTypes):
        opcScoreTypes[iIndex] = SUBSTRING(bf-reftable.dscr, iIndex, 1).
    END.
    RELEASE bf-reftable.
    FIND FIRST bf-reftable
        WHERE bf-reftable.reftable EQ "STYFLU"
        AND bf-reftable.company  EQ ipbf-eb.style
        AND bf-reftable.loc      EQ ipbf-eb.flute
        AND bf-reftable.code     EQ cScoreSet
        NO-LOCK NO-ERROR.
    IF AVAIL bf-reftable THEN
    DO iIndex = 1 TO 12: /* must be 12, total is in 13 */
        opdScores[iIndex] = bf-reftable.val[iIndex].
    END.
    RELEASE bf-reftable.
    FIND FIRST bf-reftable
        WHERE bf-reftable.reftable EQ "STYFLU"
        AND bf-reftable.company  EQ ipbf-eb.style
        AND bf-reftable.loc      EQ ipbf-eb.flute
        AND bf-reftable.code     EQ cScoreSet
        AND bf-reftable.code2    EQ "1"
        NO-LOCK NO-ERROR.
        
    IF AVAIL bf-reftable THEN
    DO iIndex = 1 TO 8: /* must be 8 (12 + 8 = 20) */
        opdScores[12 + iIndex] = bf-reftable.val[iIndex].
    END.
    
END PROCEDURE.

PROCEDURE pGetSettingRound PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Gets the setting to Round scoring allowances
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplRound AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.

    RUN sys/ref/nk1look.p (ipcCompany, "ROUND", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    oplRound = lFound AND cReturn EQ "YES".


END PROCEDURE.

PROCEDURE ProcessStyleFormula:
    /*------------------------------------------------------------------------------
     Purpose: Given a style formula, process the elements passed in and calculate
     the result.
     Notes:
     Syntax: RUN ProcessStyleFormula (ipcCompany, ipcFormula, ipdL, iplW, ipdW, 
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFormula AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdL AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdW AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdD AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdG AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdT AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdK AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdF AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdB AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdO AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdI AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCalculation AS DECIMAL NO-UNDO.

    DEFINE VARIABLE lRound        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iChar         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCalc         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iOp           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dCalculations AS DECIMAL   NO-UNDO EXTENT 100.
    DEFINE VARIABLE cOperations   AS CHARACTER NO-UNDO EXTENT 100.
    DEFINE VARIABLE cCharacter    AS CHARACTER NO-UNDO FORMAT "X".
    DEFINE VARIABLE iNextOp       AS INTEGER   NO-UNDO.  
    DEFINE VARIABLE cConstant     AS CHARACTER NO-UNDO.
    
    
    RUN pGetSettingRound(ipcCompany, OUTPUT lRound).

    /* get rid of any blank/space or invalid character */
    DO iChar = 1 TO LENGTH(ipcFormula):
        IF KEYCODE(SUBSTRING(ipcFormula, iChar, 1)) = 32 OR
            INDEX(gcValidNumbers + gcValidOperators + gcValidVariables, SUBSTRING(ipcFormula, iChar, 1)) = 0 THEN 
            ipcFormula = SUBSTRING(ipcFormula, 1, iChar - 1) + SUBSTRING(ipcFormula, iChar + 1).
    END.

    ASSIGN 
        dCalculations = 0     /* zero arrays */
        cOperations   = ""
        iNextOp       = 1.

    loop:
    DO iChar = 1 TO LENGTH(ipcFormula):

        cCharacter = SUBSTRING(ipcFormula,iChar,1).
        IF INDEX("+-/*",cCharacter) > 0 THEN
        DO:
            ASSIGN 
                cOperations[iNextOp] = cCharacter
                iNextOp              = iNextOp + 1.
            NEXT.
        END.
        IF INDEX("LWDJTSFBOIXY",cCharacter) > 0 AND dCalculations[iNextOp] = 0 THEN 
        DO:
            IF cCharacter = "L" THEN dCalculations[iNextOp] = ipdL .
            IF cCharacter = "W" THEN dCalculations[iNextOp] = ipdW .
            IF cCharacter = "D" THEN dCalculations[iNextOp] = ipdD .
            IF cCharacter = "J" THEN dCalculations[iNextOp] = ipdG .
            IF cCharacter = "T" THEN dCalculations[iNextOp] = ipdT .
            IF cCharacter = "S" THEN dCalculations[iNextOp] = ipdK .
            IF cCharacter = "F" THEN dCalculations[iNextOp] = ipdF .
            IF cCharacter = "B" THEN dCalculations[iNextOp] = ipdB .
            IF cCharacter = "O" THEN dCalculations[iNextOp] = ipdO .
            IF cCharacter = "I" THEN dCalculations[iNextOp] = ipdI .
            NEXT.
        END.
        ELSE IF INDEX("LWDJTSFBOIXY",cCharacter) > 0 AND dCalculations[iNextOp] NE 0 THEN 
            DO:
                IF cCharacter = "L" THEN dCalculations[iNextOp] = ipdL * dCalculations[iNextOp].
                IF cCharacter = "W" THEN dCalculations[iNextOp] = ipdW * dCalculations[iNextOp].
                IF cCharacter = "D" THEN dCalculations[iNextOp] = ipdD * dCalculations[iNextOp].
                IF cCharacter = "J" THEN dCalculations[iNextOp] = ipdG * dCalculations[iNextOp].
                IF cCharacter = "T" THEN dCalculations[iNextOp] = ipdT * dCalculations[iNextOp].
                IF cCharacter = "S" THEN dCalculations[iNextOp] = ipdK * dCalculations[iNextOp].
                IF cCharacter = "F" THEN dCalculations[iNextOp] = ipdF * dCalculations[iNextOp].
                IF cCharacter = "B" THEN dCalculations[iNextOp] = ipdB * dCalculations[iNextOp].
                IF cCharacter = "O" THEN dCalculations[iNextOp] = ipdO * dCalculations[iNextOp].
                IF cCharacter = "I" THEN dCalculations[iNextOp] = ipdI * dCalculations[iNextOp].
                NEXT.
            END.
            ELSE 
            DO:
                cConstant = "".
                DO WHILE (KEYCODE(cCharacter) >= 48 AND keycode(cCharacter) <= 57) OR keycode(cCharacter) = 46:
                    ASSIGN 
                        cConstant  = cConstant + cCharacter
                        iChar      = iChar + 1
                        cCharacter = SUBSTRING(ipcFormula,iChar,1).
                END.
                iChar = iChar - 1.
                IF dCalculations[iNextOp] NE 0
                    THEN dCalculations[iNextOp] = DECIMAL(cConstant) * dCalculations[iNextOp].
                ELSE dCalculations[iNextOp] = DECIMAL(cConstant).
            END.
    END.
    /*Does 16th and 32nd */
    /*    DO iCalc = 1 TO EXTENT(dCalculations):                              */
    /*        dCalculations[iCalc] = dCalculations[iCalc] * li-16-32.         */
    /*        IF lRound THEN                                                  */
    /*        DO:                                                             */
    /*        {sys/inc/roundup.i dCalculations[iCalc]}                        */
    /*        END.                                                            */
    /*        ELSE IF v-cecscrn-char NE "Decimal" THEN                        */
    /*                dCalculations[iCalc] = TRUNCATE(dCalculations[iCalc],0).*/
    /*        dCalculations[iCalc] = dCalculations[iCalc] / li-16-32.         */
    /*    END.                                                                */

    opdCalculation = dCalculations[1].
    DO iOp = 2 TO EXTENT(cOperations):
        IF cOperations[iOp - 1] = "+" THEN opdCalculation = opdCalculation + dCalculations[iOp].
        ELSE IF cOperations[iOp - 1] = "-" THEN opdCalculation = opdCalculation - dCalculations[iOp].
    END.


END PROCEDURE.

