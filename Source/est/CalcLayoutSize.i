/*------------------------------------------------------------------------
    File        : CalcLayoutSize.i
    Purpose     : 

    Syntax      : 

    Description : Supporting procedures to be included in CalcLayoutSize.i and calc-dim.p

    Author(s)   : DEVA$!
    Created     : Wed Jul 27 21:50:32 EST 2022
    Notes       :
  ----------------------------------------------------------------------*/
  
PROCEDURE pCalculatePartitionStyleDieSize PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef FOR ef.
    DEFINE OUTPUT PARAMETER opdDieSizeWidth  AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdDieSizeLength AS DECIMAL NO-UNDO.

    DEFINE VARIABLE dLongWidth           AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dLongLength          AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iNumberOnLongWidth   AS INTEGER NO-UNDO.
    DEFINE VARIABLE iNumberOnLongLength  AS INTEGER NO-UNDO.
    DEFINE VARIABLE dShortWidth          AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dShortLength         AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iNumberOnShortWidth  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iNumberOnShortLength AS INTEGER NO-UNDO.
    DEFINE VARIABLE lBlankAvailable      AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-eb  FOR eb.
    DEFINE BUFFER bf-est FOR est.
        
    IF NOT AVAILABLE ipbf-ef THEN
        RETURN.
    
    FIND FIRST bf-est NO-LOCK
         WHERE bf-est.company EQ ipbf-ef.company
           AND bf-est.est-no  EQ ipbf-ef.est-no
         NO-ERROR.
    IF NOT AVAILABLE bf-est THEN
        RETURN.
        
    FOR EACH bf-eb NO-LOCK
        WHERE bf-eb.company EQ ipbf-ef.company
          AND bf-eb.est-no  EQ ipbf-ef.est-no
          AND bf-eb.eqty    EQ ipbf-ef.eqty
          AND bf-eb.form-no EQ ipbf-ef.form-no
        BREAK BY bf-eb.est-no:
        lBlankAvailable = TRUE.
        
        IF FIRST-OF(bf-eb.est-no) THEN DO:
            ASSIGN
                dLongLength = bf-eb.t-len
                dLongWidth  = bf-eb.t-wid
                .

            /* Field values num-wid and num-len are swapped if est-type is ge 5 */
            IF bf-est.est-type GE 5 THEN
                ASSIGN
                    iNumberOnLongLength = bf-eb.num-wid
                    iNumberOnLongWidth  = bf-eb.num-len
                    .
            ELSE
                ASSIGN
                    iNumberOnLongLength = bf-eb.num-len
                    iNumberOnLongWidth  = bf-eb.num-wid
                    .
        END.
        ELSE IF LAST-OF(bf-eb.est-no) THEN DO:
            ASSIGN
                dShortLength = bf-eb.t-len
                dShortWidth  = bf-eb.t-wid
                .

            IF bf-est.est-type GE 5 THEN
                ASSIGN
                    iNumberOnShortLength = bf-eb.num-wid
                    iNumberOnShortWidth  = bf-eb.num-len
                    .
            ELSE
                ASSIGN
                    iNumberOnShortLength = bf-eb.num-len
                    iNumberOnShortWidth  = bf-eb.num-wid
                    .
        END.
    END.
    
    IF lBlankAvailable THEN
        RUN pGetPartitionStyleDieSize (
            INPUT  dLongWidth,
            INPUT  dLongLength,
            INPUT  iNumberOnLongWidth,
            INPUT  iNumberOnLongLength,
            INPUT  dShortWidth,
            INPUT  dShortLength,
            INPUT  iNumberOnShortWidth,
            INPUT  iNumberOnShortLength,
            OUTPUT opdDieSizeWidth, 
            OUTPUT opdDieSizeLength
            ).    

END PROCEDURE.

PROCEDURE pGetPartitionStyleDieSize PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipdLongWidth           AS DECIMAL NO-UNDO.
    DEFINE INPUT  PARAMETER ipdLongLength          AS DECIMAL NO-UNDO.
    DEFINE INPUT  PARAMETER ipiNumberOnLongWidth   AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiNumberOnLongLength  AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdShortWidth          AS DECIMAL NO-UNDO.
    DEFINE INPUT  PARAMETER ipdShortLength         AS DECIMAL NO-UNDO.
    DEFINE INPUT  PARAMETER ipiNumberOnShortWidth  AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiNumberOnShortLength AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdDieSizeWidth        AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdDieSizeLength       AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE iRows      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCols      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iRow       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCol       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dMaxWidth  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cLongShort AS CHARACTER NO-UNDO.
    
    ASSIGN
        iRows     = MAX(ipiNumberOnLongWidth, ipiNumberOnShortWidth) 
        iCols     = ipiNumberOnLongLength + ipiNumberOnShortLength
        dMaxWidth = MAX(ipdLongWidth, ipdShortWidth)
        .
    
    DO iRow = 1 TO iRows:
        DO iCol = 1 TO iCols:
            IF iCol NE 1 AND iRow NE 1 THEN
                NEXT.

            cLongShort = "".
            
            IF iCol LE iCols AND iRow LE iRows THEN DO:
                IF iCol LE ipiNumberOnLongLength THEN 
                    cLongShort = "Long".
                ELSE
                    cLongShort = "Short".
            END.

            IF cLongShort NE "" THEN DO:
                IF iCol EQ 1 THEN
                    opdDieSizeWidth = opdDieSizeWidth + dMaxWidth.
                
                IF iRow EQ 1 THEN            
                    opdDieSizeLength = opdDieSizeLength 
                                     + (IF iCol LE ipiNumberOnLongLength THEN ipdLongLength ELSE ipdShortLength). 
            END.
        END.
    END.
END PROCEDURE.        

PROCEDURE pAreBlanksPartitionStyleType PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef FOR ef.
    DEFINE OUTPUT PARAMETER oplStyleTypePartition AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-eb FOR eb.
    
    IF NOT AVAILABLE ipbf-ef THEN
        RETURN.
    
    oplStyleTypePartition = TRUE.
        
    FOR EACH bf-eb NO-LOCK
        WHERE bf-eb.company EQ ipbf-ef.company
          AND bf-eb.est-no  EQ ipbf-ef.est-no
          AND bf-eb.eqty    EQ ipbf-ef.eqty
          AND bf-eb.form-no EQ ipbf-ef.form-no,
        FIRST style NO-LOCK
        WHERE style.company EQ bf-eb.company
          AND style.style   EQ bf-eb.style:
        IF style.type NE "P" THEN DO:
            oplStyleTypePartition = FALSE.
            LEAVE.
        END.    
    END.
    
    RELEASE bf-eb.
END PROCEDURE.