
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


DEFINE VARIABLE gcValidNumbers   AS CHARACTER NO-UNDO INITIAL "0123456789.".
DEFINE VARIABLE gcValidOperators AS CHARACTER NO-UNDO INITIAL "+-/*".
DEFINE VARIABLE gcValidVariables AS CHARACTER NO-UNDO INITIAL "LWDTFJBOSI".
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

RUN ProcessStyleFormula(ipcCompany, ipcFormula, ipdL, ipdW, ipdD, ipdG, ipdT, ipdK, ipdF, ipdB, ipdO, ipdI, OUTPUT opdCalculation).

/* **********************  Internal Procedures  *********************** */

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

