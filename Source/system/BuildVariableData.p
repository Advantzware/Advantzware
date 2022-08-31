
/*------------------------------------------------------------------------
    File        : system/BuildVariableData.p
    Purpose     : Builds temp-table for a given data

    Syntax      :

    Description : 

    Author(s)   : DEVA$!
    Created     : Sat Apr 09 23:10:03 IST 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

{system/ttVariable.i}

DEFINE INPUT  PARAMETER iplcRequestData AS LONGCHAR NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttVariable.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

DEFINE VARIABLE iChar      AS INT64     NO-UNDO.
DEFINE VARIABLE iIndex     AS INT64     NO-UNDO.
DEFINE VARIABLE lIsFirst$  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cChar      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLen       AS INTEGER   NO-UNDO.
DEFINE VARIABLE lFirst     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lHasQuote  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lHasFormat AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lHasType   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lHasAlign  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lHasFunc1  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lHasFunc2  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lHasFunc3  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lHasFunc4  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lHasFunc5  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cFormat    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cType      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAlign     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFunc1     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFunc2     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFunc3     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFunc4     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFunc5     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iSequence  AS INTEGER   NO-UNDO.

/* Read through all characters in the input */
DO iChar = 1 TO LENGTH(iplcRequestData):
    cChar = SUBSTRING(iplcRequestData, iChar, 1).
    iLen = iLen + 1.
    
    /* If a $ is found then create a ttVariable record */
    IF cChar = "$" AND NOT lIsFirst$ THEN 
    DO:
        ASSIGN
            lFirst    = TRUE
            lIsFirst$ = TRUE
            iSequence = iSequence + 1
            .
            
        CREATE ttVariable.
        ASSIGN
            ttVariable.varPosition = iChar
            ttVariable.sequenceID  = iSequence
            .
    END.
    
    IF lIsFirst$ AND (cChar NE "$" OR lHasFormat OR lHasFunc1 OR lHasFunc2 OR lHasFunc3 OR lHasFunc4 OR lHasFunc5) THEN 
    DO:
        /* If there is $ right after a |, but if not inside a quote */
        IF cChar EQ "$" AND NOT lHasQuote THEN DO:
            IF lHasFunc1 THEN
                lHasFunc1 = FALSE.
            IF lHasFunc2 THEN
                lHasFunc2 = FALSE.
            IF lHasFunc3 THEN
                lHasFunc3 = FALSE.
            IF lHasFunc4 THEN
                lHasFunc4 = FALSE.
            IF lHasFunc5 THEN
                lHasFunc5 = FALSE.
        END.
  
        IF cChar = "|" AND NOT lHasQuote THEN 
        DO:                           
            IF NOT lHasFormat AND NOT lHasType AND NOT lHasAlign AND NOT lHasFunc1 AND NOT lHasFunc2 AND NOT lHasFunc3 AND NOT lHasFunc4 AND NOT lHasFunc5 THEN
                lHasFormat = TRUE.
            ELSE IF lHasFormat AND NOT lHasType THEN
                ASSIGN
                    lHasFormat = FALSE
                    lHasType   = TRUE
                    .
            ELSE IF lHasType AND NOT lHasAlign THEN
                ASSIGN
                    lHasType  = FALSE
                    lHasAlign = TRUE
                    .
            ELSE IF lHasAlign AND NOT lHasFunc1 THEN
                ASSIGN
                    lHasAlign = FALSE
                    lHasFunc1 = TRUE
                    .
            ELSE IF lHasFunc1 AND NOT lHasFunc2 THEN
                ASSIGN
                    lHasFunc1 = FALSE
                    lHasFunc2 = TRUE
                    .
            ELSE IF lHasFunc2 AND NOT lHasFunc3 THEN
                ASSIGN
                    lHasFunc2 = FALSE
                    lHasFunc3 = TRUE
                    .
            ELSE IF lHasFunc3 AND NOT lHasFunc4 THEN
                ASSIGN
                    lHasFunc3 = FALSE
                    lHasFunc4 = TRUE
                    .
            ELSE IF lHasFunc4 AND NOT lHasFunc5 THEN
                ASSIGN
                    lHasFunc4 = FALSE
                    lHasFunc5 = TRUE
                    .
            ELSE IF lHasFunc5 THEN
                lHasFunc5 = FALSE.
        END.
        
        IF cChar = '"' THEN
            lHasQuote = NOT lHasQuote.
        
        IF lHasFormat AND cChar NE "|" THEN
            ttVariable.varFormat = ttVariable.varFormat + cChar.
        ELSE IF lHasType AND cChar NE "|" THEN
            ttVariable.varDataType = ttVariable.varDataType + cChar.    
        ELSE IF lHasAlign AND cChar NE "|" THEN
            ttVariable.varAlign = ttVariable.varAlign + cChar.
        ELSE IF lHasFunc1 AND (cChar NE "|" OR lHasQuote) THEN
            ttVariable.varFunction1 = ttVariable.varFunction1 + cChar.
        ELSE IF lHasFunc2 AND (cChar NE "|" OR lHasQuote) THEN
            ttVariable.varFunction2 = ttVariable.varFunction2 + cChar.
        ELSE IF lHasFunc3 AND (cChar NE "|" OR lHasQuote) THEN
            ttVariable.varFunction3 = ttVariable.varFunction3 + cChar.
        ELSE IF lHasFunc4 AND (cChar NE "|" OR lHasQuote) THEN
            ttVariable.varFunction4 = ttVariable.varFunction4 + cChar.
        ELSE IF lHasFunc5 AND (cChar NE "|" OR lHasQuote) THEN
            ttVariable.varFunction5 = ttVariable.varFunction5 + cChar.
        ELSE IF cChar NE "|" AND cChar NE "$" THEN
            ttVariable.varName = ttVariable.varName + cChar.
    
    END.
    
    IF lIsFirst$ AND cChar EQ "$" AND NOT lFirst AND NOT lHasQuote THEN DO:                
        ASSIGN
            ttVariable.varFull   = ttVariable.varFull + cChar.
            ttVariable.varLength = iChar - ttVariable.varPos + 1
            .
    
        IF NOT lhasFormat AND NOT lHasFunc1 AND NOT lHasFunc2 AND 
            NOT lHasFunc2 AND NOT lHasFunc3 AND NOT lHasFunc4 AND NOT lHasFunc5 THEN DO:
            ASSIGN
                lIsFirst$  = FALSE
                lHasFormat = FALSE
                lHasType   = FALSE
                lHasAlign  = FALSE
                lHasFunc1  = FALSE
                lHasFunc2  = FALSE
                lHasFunc3  = FALSE
                lHasFunc4  = FALSE
                lHasFunc5  = FALSE
                lHasQuote  = FALSE
                .
            
            IF ttVariable.varDataType EQ "" OR ttVariable.varDataType EQ "Character" THEN
                ttVariable.varDataType = "Character".
            ELSE IF ttVariable.varDataType EQ "INT" OR ttVariable.varDataType EQ "Integer" THEN
                ttVariable.varDataType = "Integer".
            ELSE IF ttVariable.varDataType EQ "DEC" OR ttVariable.varDataType EQ "Decimal" THEN
                ttVariable.varDataType = "Decimal".
            ELSE IF ttVariable.varDataType EQ "LOG" THEN
                ttVariable.varDataType = "Logical".
            ELSE IF ttVariable.varDataType EQ "DEC-INT" OR ttVariable.varDataType EQ "DECIMAL-INTEGER" THEN
                ttVariable.varDataType = "Decimal-Integer".
                
        END.
    END. 
    ELSE IF lIsFirst$ THEN
        ttVariable.varFull = ttVariable.varFull + cChar.
    
    lFirst = FALSE.
END.
