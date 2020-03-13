
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

DEFINE VARIABLE gcPanelLinkTypePO       AS CHARACTER NO-UNDO INITIAL "P".
DEFINE VARIABLE gcPanelLinkTypeEstimate AS CHARACTER NO-UNDO INITIAL "E".
DEFINE VARIABLE gcPanelLinkTypeStyle    AS CHARACTER NO-UNDO INITIAL "S".
DEFINE VARIABLE gcValidPanelDecimals    AS CHARACTER NO-UNDO.

DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DO iIndex = 0 TO 63:
    gcValidPanelDecimals = gcValidPanelDecimals + "," + SUBSTRING(STRING(iIndex / 64), 1, 3).    
END.

gcValidPanelDecimals = TRIM(gcValidPanelDecimals).
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
                                 OUTPUT ttPanel.dPanelSizeFromFormula).
        ttPanel.dPanelSize = ttPanel.dPanelSizeFromFormula + ttPanel.dScoringAllowance.
    END.
END PROCEDURE.

PROCEDURE GetSizeFactor:
/*------------------------------------------------------------------------------
 Purpose: Fetch the decimal factor from NK1 CECSCRN
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdSizeFactor AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcSizeFormat AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplDecimalLog AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE cReturnChar AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound   AS LOGICAL   NO-UNDO.
    
    ASSIGN
        opcSizeFormat = "Decimal"
        opdSizeFactor = 1
        oplDecimalLog = FALSE
        .
    
    RUN sys/ref/nk1look.p (
        INPUT ipcCompany,     /* Company Code */ 
        INPUT "CECSCRN",      /* sys-ctrl name */
        INPUT "C",            /* Output return value */
        INPUT NO,             /* Use ship-to */
        INPUT NO,             /* ship-to vendor */
        INPUT "",             /* ship-to vendor value */
        INPUT "",             /* shi-id value */
        OUTPUT cReturnChar, 
        OUTPUT lRecFound
        ).
    
    IF cReturnChar EQ "16th's" THEN
        ASSIGN
            opcSizeFormat = "16th's"
            opdSizeFactor = 6.25
            .
    ELSE IF cReturnChar EQ "32nd's" THEN
        ASSIGN
            opcSizeFormat = "32nd's"
            opdSizeFactor = 3.125
            .
    ELSE IF cReturnChar EQ "Decimal" THEN DO:
        RUN sys/ref/nk1look.p (
            INPUT ipcCompany,     /* Company Code */ 
            INPUT "CECSCRN",      /* sys-ctrl name */
            INPUT "L",            /* Output return value */
            INPUT NO,             /* Use ship-to */
            INPUT NO,             /* ship-to vendor */
            INPUT "",             /* ship-to vendor value */
            INPUT "",             /* shi-id value */
            OUTPUT cReturnChar, 
            OUTPUT lRecFound
            ).

        ASSIGN
            opcSizeFormat = "Decimal"
            opdSizeFactor = 1
            oplDecimalLog = LOGICAL(cReturnChar)
            .
    END.            
END PROCEDURE.

PROCEDURE GetPanelDetailsForEstimate:
/*------------------------------------------------------------------------------
 Purpose: Fetches panelDetail records for a given Estimate
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimateID AS CHARACTE  NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormNo     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankNo    AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE         FOR ttPanel.    
    
    DEFINE BUFFER bf-panelHeader FOR panelHeader.
    
    FIND FIRST bf-panelHeader NO-LOCK
         WHERE bf-panelHeader.company    EQ ipcCompany
           AND bf-panelHeader.linkType   EQ gcPanelLinkTypeEstimate
           AND bf-panelHeader.estimateID EQ ipcEstimateID
           AND bf-panelHeader.formNo     EQ ipiFormNo
           AND bf-panelHeader.blankNo    EQ ipiBlankNo
         NO-ERROR.
    IF AVAILABLE bf-panelHeader THEN
        RUN pBuildttPanel (
            INPUT  bf-panelHeader.panelHeaderID,
            OUTPUT TABLE ttPanel
            ).

    RELEASE bf-panelHeader.                       
END PROCEDURE.

PROCEDURE GetPanelDetailsForPO:
/*------------------------------------------------------------------------------
 Purpose: Fetches panelDetail records for a given Purchase Order
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoID    AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoLine  AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE      FOR ttPanel.
        
    DEFINE BUFFER bf-panelHeader FOR panelHeader.
    
    FIND FIRST bf-panelHeader NO-LOCK
         WHERE bf-panelHeader.company  EQ ipcCompany
           AND bf-panelHeader.linkType EQ gcPanelLinkTypePO
           AND bf-panelHeader.poID     EQ ipiPoID
           AND bf-panelHeader.poLine   EQ ipiPoLine
         NO-ERROR.
    IF AVAILABLE bf-panelHeader THEN
        RUN pBuildttPanel (
            INPUT  bf-panelHeader.panelHeaderID,
            OUTPUT TABLE ttPanel
            ).
            
    RELEASE bf-panelHeader.            
END PROCEDURE.

PROCEDURE GetPanelDetailsForStyle:
/*------------------------------------------------------------------------------
 Purpose: Fetches panelDetail records for a given Style
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcStyleID      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFluteID      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcScoreSetType AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE           FOR ttPanel.
    
    DEFINE BUFFER bf-panelHeader FOR panelHeader.
    
    FIND FIRST bf-panelHeader NO-LOCK
         WHERE bf-panelHeader.company      EQ ipcCompany
           AND bf-panelHeader.linkType     EQ gcPanelLinkTypeStyle
           AND bf-panelHeader.styleID      EQ ipcStyleID
           AND bf-panelHeader.fluteID      EQ ipcFluteID
           AND bf-panelHeader.scoreSetType EQ ipcScoreSetType
         NO-ERROR.
    IF AVAILABLE bf-panelHeader THEN
        RUN pBuildttPanel (
            INPUT  bf-panelHeader.panelHeaderID,
            OUTPUT TABLE ttPanel
            ).
            
    RELEASE bf-panelHeader.  
END PROCEDURE.

PROCEDURE GetPanelScoreAndTypeForEstimate:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to fetch panel size and score type from estimate
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimateID AS CHARACTE  NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormNo     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankNo    AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPanelType  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdScores     AS DECIMAL   NO-UNDO EXTENT 20.
    DEFINE OUTPUT PARAMETER opcScoreTypes AS CHARACTER NO-UNDO EXTENT 20.

    RUN GetPanelDetailsForEstimate (
        INPUT  ipcCompany,
        INPUT  ipcEstimateID,
        INPUT  ipiFormNo,
        INPUT  ipiBlankNo,
        OUTPUT TABLE ttPanel
        ).

    RUN pGetScoreAndTypes (
        INPUT  ipcPanelType,
        OUTPUT opdScores,
        OUTPUT opcScoreTypes
        ).
END PROCEDURE.

PROCEDURE GetPanelScoreAndTypeForPO:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to fetch panel size and score type from PO
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoID       AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoLine     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPanelType  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdScores     AS DECIMAL   NO-UNDO EXTENT 20.
    DEFINE OUTPUT PARAMETER opcScoreTypes AS CHARACTER NO-UNDO EXTENT 20.

    RUN GetPanelDetailsForPO (
        INPUT  ipcCompany,
        INPUT  ipiPoID,
        INPUT  ipiPoLine,
        OUTPUT TABLE ttPanel
        ).
    
    RUN pGetScoreAndTypes (
        INPUT  ipcPanelType,
        OUTPUT opdScores,
        OUTPUT opcScoreTypes
        ).
END PROCEDURE.

PROCEDURE GetPanelScoreAndTypeForStyle:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to fetch panel size and score type from Style
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcStyleID      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFluteID      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcScoreSetType AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPanelType    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdScores       AS DECIMAL   NO-UNDO EXTENT 20.
    DEFINE OUTPUT PARAMETER opcScoreTypes   AS CHARACTER NO-UNDO EXTENT 20.

    RUN GetPanelDetailsForStyle (
        INPUT  ipcCompany,
        INPUT  ipcStyleID,
        INPUT  ipcFluteID,
        INPUT  ipcScoreSetType,
        OUTPUT TABLE ttPanel
        ).
    
    RUN pGetScoreAndTypes (
        INPUT  ipcPanelType,
        OUTPUT opdScores,
        OUTPUT opcScoreTypes
        ).

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

PROCEDURE pBuildttPanel PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Builds ttPanel temp-table for a given panel header id
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiPanelHeaderID AS INT64 NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE            FOR ttPanel.
    
    DEFINE BUFFER bf-panelDetail FOR panelDetail.
    
    EMPTY TEMP-TABLE ttPanel.
    
    FOR EACH bf-panelDetail NO-LOCK
        WHERE bf-panelDetail.panelHeaderID EQ ipiPanelHeaderID:
        CREATE ttPanel.
        ASSIGN
            ttPanel.cPanelType            = bf-panelDetail.panelType
            ttPanel.iPanelNum             = bf-panelDetail.panelNo
            ttPanel.cPanelFormula         = bf-panelDetail.panelFormula
            ttPanel.dScoringAllowance     = bf-panelDetail.scoringAllowance
            ttPanel.cScoreType            = bf-panelDetail.scoreType
            ttPanel.dPanelSize            = bf-panelDetail.panelSize
            ttPanel.dPanelSizeFromFormula = bf-panelDetail.panelSizeFromFormula
            .            
    END.    
    
    RELEASE bf-panelDetail.
END PROCEDURE.

PROCEDURE pGetScoreAndTypes PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to retreive score and score type from ttPanel to arrays
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcPanelType  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdScores     AS DECIMAL   NO-UNDO EXTENT 20.
    DEFINE OUTPUT PARAMETER opcScoreTypes AS CHARACTER NO-UNDO EXTENT 20.

    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.

    FOR EACH ttPanel
        WHERE ttPanel.cPanelType EQ ipcPanelType:
        iIndex = iIndex + 1.
        ASSIGN
            opdScores[iIndex]     = ttPanel.dPanelSize
            opcScoreTypes[iIndex] = ttPanel.cScoreType
            .
    END.
END PROCEDURE.

PROCEDURE pUpdatePanelDetail PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Update/Create panelDetail record
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany              AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPanelHeaderID        AS INT64     NO-UNDO.
    DEFINE INPUT PARAMETER ipcPanelType            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPanelNo              AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcPanelFormula         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdScoringAllowance     AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcScoreType            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdPanelSize            AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdPanelSizeFromFormula AS DECIMAL   NO-UNDO.

    DEFINE BUFFER bf-panelDetail FOR panelDetail.
    
    FIND FIRST bf-panelDetail EXCLUSIVE-LOCK
         WHERE bf-panelDetail.company       EQ ipcCompany
           AND bf-panelDetail.panelHeaderID EQ ipiPanelHeaderID
           AND bf-panelDetail.panelType     EQ ipcPanelType
           AND bf-panelDetail.panelNo       EQ ipiPanelNo
         NO-ERROR.
    IF NOT AVAILABLE bf-panelDetail THEN
        RUN pCreatePanelDetail (
            INPUT ipcCompany,
            INPUT ipiPanelHeaderID,
            INPUT ipcPanelType,
            INPUT ipiPanelNo,
            INPUT ipcPanelFormula,
            INPUT ipdScoringAllowance,
            INPUT ipcScoreType,
            INPUT ipdPanelSize,
            INPUT ipdPanelSizeFromFormula            
            ).
    ELSE DO:
        ASSIGN
            bf-panelDetail.panelFormula         = ipcPanelFormula
            bf-panelDetail.scoringAllowance     = ipdScoringAllowance
            bf-panelDetail.scoreType            = ipcScoreType
            bf-panelDetail.panelSize            = ipdPanelSize
            bf-panelDetail.panelSizeFromFormula = ipdPanelSizeFromFormula
            .        
    END.
    
    RELEASE bf-panelDetail.
END PROCEDURE.

PROCEDURE pCreatePanelHeader PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Creates a panelHeader record
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLinkType      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoID          AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoLine        AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimateID    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormNo        AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankNo       AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcStyleID       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFluteID       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcScoreSetType  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiPanelHeaderID AS INT64     NO-UNDO.
    
    DEFINE BUFFER bf-panelHeader FOR panelHeader.
    
    CREATE bf-panelHeader.
    ASSIGN
        bf-panelHeader.company      = ipcCompany
        bf-panelHeader.linkType     = ipcLinkType
        bf-panelHeader.poID         = ipiPoID
        bf-panelHeader.poLine       = ipiPoLine
        bf-panelHeader.estimateID   = ipcEstimateID
        bf-panelHeader.formNo       = ipiFormNo
        bf-panelHeader.blankNo      = ipiBlankNo
        bf-panelHeader.styleID      = ipcStyleID
        bf-panelHeader.fluteID      = ipcFluteID
        bf-panelHeader.scoreSetType = ipcScoreSetType
        .

    opiPanelHeaderID = bf-panelHeader.panelHeaderID.
    
    RELEASE bf-panelHeader. 
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
/*        DO iCalc = 1 TO EXTENT(dCalculations):                              */
/*            dCalculations[iCalc] = dCalculations[iCalc] * li-16-32.         */
/*            IF lRound THEN                                                  */
/*            DO:                                                             */
/*            {sys/inc/roundup.i dCalculations[iCalc]}                        */
/*            END.                                                            */
/*            ELSE IF v-cecscrn-char NE "Decimal" THEN                        */
/*                    dCalculations[iCalc] = TRUNCATE(dCalculations[iCalc],0).*/
/*            dCalculations[iCalc] = dCalculations[iCalc] / li-16-32.         */
/*        END.                                                                */

    opdCalculation = dCalculations[1].
    DO iOp = 2 TO EXTENT(cOperations):
        IF cOperations[iOp - 1] = "+" THEN opdCalculation = opdCalculation + dCalculations[iOp].
        ELSE IF cOperations[iOp - 1] = "-" THEN opdCalculation = opdCalculation - dCalculations[iOp].
    END.


END PROCEDURE.

PROCEDURE pCreatePanelDetail PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Create panelDetail record
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany              AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPanelHeaderID        AS INT64     NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPanelType            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPanelNo              AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPanelFormula         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdScoringAllowance     AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcScoreType            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdPanelSize            AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdPanelSizeFromFormula AS DECIMAL   NO-UNDO.

    DEFINE BUFFER bf-panelDetail FOR panelDetail.

    CREATE bf-panelDetail.
    ASSIGN
        bf-panelDetail.company              = ipcCompany
        bf-panelDetail.panelHeaderID        = ipiPanelHeaderID
        bf-panelDetail.panelType            = ipcPanelType
        bf-panelDetail.panelNo              = ipiPanelNo
        bf-panelDetail.panelFormula         = ipcPanelFormula
        bf-panelDetail.scoringAllowance     = ipdScoringAllowance
        bf-panelDetail.scoreType            = ipcScoreType
        bf-panelDetail.panelSize            = ipdPanelSize
        bf-panelDetail.panelSizeFromFormula = ipdPanelSizeFromFormula
        .

    RELEASE bf-panelDetail.
END PROCEDURE.

PROCEDURE SwitchPanelSizeFormat:
/*------------------------------------------------------------------------------
 Purpose: Converts the size format among 16th's, 32nd's and decimal
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCurrentSizeFormat  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcSwitchToSizeFormat AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdPanelSize          AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdPanelSize          AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE dCurrentSizeFactor  AS DECIMAL NO-UNDO INITIAL 1.
    DEFINE VARIABLE dSwitchToSizeFactor AS DECIMAL NO-UNDO INITIAL 1.
    
    opdPanelSize = ipdPanelSize.    
    IF ipcCurrentSizeFormat EQ ipcSwitchToSizeFormat THEN
        RETURN.
    
    IF ipcCurrentSizeFormat NE "16th's" AND
       ipcCurrentSizeFormat NE "32nd's" AND
       ipcCurrentSizeFormat NE "Decimal" THEN
        RETURN.

    IF ipcSwitchToSizeFormat NE "16th's" AND
       ipcSwitchToSizeFormat NE "32nd's" AND
       ipcSwitchToSizeFormat NE "Decimal" THEN
        RETURN.

    IF ipcCurrentSizeFormat EQ "16th's" THEN
        dCurrentSizeFactor = 6.25.
    ELSE IF ipcCurrentSizeFormat EQ "32nd's" THEN
        dCurrentSizeFactor = 3.125.
    ELSE IF ipcCurrentSizeFormat EQ "Decimal" THEN
        dCurrentSizeFactor = 1.
    
    IF ipcSwitchToSizeFormat EQ "16th's" THEN
        dSwitchToSizeFactor = 6.25.
    ELSE IF ipcSwitchToSizeFormat EQ "32nd's" THEN
        dSwitchToSizeFactor = 3.125.
    ELSE IF ipcSwitchToSizeFormat EQ "Decimal" THEN
        dSwitchToSizeFactor = 1.
    
    ASSIGN
        opdPanelSize = TRUNCATE(ipdPanelSize,0) + ((ipdPanelSize - TRUNCATE(ipdPanelSize,0)) * dCurrentSizeFactor)
        opdPanelSize = TRUNCATE(opdPanelSize,0) + ((opdPanelSize - TRUNCATE(opdPanelSize,0)) / dSwitchToSizeFactor)
        .
    
END PROCEDURE.

PROCEDURE SwitchPanelSizeFormatForttPanel:
/*------------------------------------------------------------------------------
 Purpose: Converts the ttPanel records as per input size format
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCurrentSizeFormat  AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcSwitchToSizeFormat AS CHARACTER NO-UNDO.    
    DEFINE INPUT-OUTPUT PARAMETER TABLE                 FOR ttPanel.

    FOR EACH ttPanel:
        RUN SwitchPanelSizeFormat (
            INPUT  ipcCurrentSizeFormat,
            INPUT  ipcSwitchToSizeFormat,
            INPUT  ttPanel.dPanelSize,
            OUTPUT ttPanel.dPanelSize
            ).

        RUN SwitchPanelSizeFormat (
            INPUT  ipcCurrentSizeFormat,
            INPUT  ipcSwitchToSizeFormat,
            INPUT  ttPanel.dScoringAllowance,
            OUTPUT ttPanel.dScoringAllowance
            ).
    END.
END PROCEDURE.

PROCEDURE UpdatePanelDetailsForEstimate:
/*------------------------------------------------------------------------------
 Purpose: Updates/Creates panelHeader and panelDetail records for a given Estimate
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateID AS CHARACTE  NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo     AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo    AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER TABLE         FOR ttPanel.
    
    DEFINE VARIABLE iPanelHeaderID AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-panelHeader FOR panelHeader.
    
    FIND FIRST bf-panelHeader NO-LOCK
         WHERE bf-panelHeader.company    EQ ipcCompany
           AND bf-panelHeader.linkType   EQ gcPanelLinkTypeEstimate
           AND bf-panelHeader.estimateID EQ ipcEstimateID
           AND bf-panelHeader.formNo     EQ ipiFormNo
           AND bf-panelHeader.blankNo    EQ ipiBlankNo
         NO-ERROR.
    IF NOT AVAILABLE bf-panelHeader THEN DO:        
        RUN pCreatePanelHeader (
            INPUT  ipcCompany,
            INPUT  gcPanelLinkTypeEstimate, /* "E" */
            INPUT  0,                       /* Purchase Order */
            INPUT  0,                       /* Purchase Order Line */
            INPUT  ipcEstimateID,           /* Estimate ID */
            INPUT  ipiFormNo,               /* Form No */
            INPUT  ipiBlankNo,              /* Blank No */
            INPUT  "",                      /* Style ID */
            INPUT  "",                      /* Flute ID */
            INPUT  "",                      /* Score set Type */
            OUTPUT iPanelHeaderID        
            ).
            
        FIND FIRST bf-panelHeader NO-LOCK
             WHERE bf-panelHeader.panelHeaderID EQ iPanelHeaderID
             NO-ERROR.
    END.

    IF AVAILABLE bf-panelHeader THEN DO:
        FOR EACH ttPanel
            BY ttPanel.iPanelNum:
            RUN pUpdatePanelDetail (
                INPUT  bf-panelHeader.company,
                INPUT  bf-panelHeader.panelHeaderID,
                INPUT  ttPanel.cPanelType,
                INPUT  ttPanel.iPanelNum,
                INPUT  ttPanel.cPanelFormula,
                INPUT  ttPanel.dScoringAllowance,
                INPUT  ttPanel.cScoreType,
                INPUT  ttPanel.dPanelSize,
                INPUT  ttPanel.dPanelSizeFromFormula
                ).                        
        END.  
    END.
            
    RELEASE bf-panelHeader. 
END PROCEDURE.

PROCEDURE UpdatePanelDetailsForPO:
/*------------------------------------------------------------------------------
 Purpose: Updates/Creates panelHeader and panelDetail records for a given PO
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPoID    AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiPoLine  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER TABLE      FOR ttPanel.
    
    DEFINE VARIABLE iPanelHeaderID AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-panelHeader FOR panelHeader.
    
    FIND FIRST bf-panelHeader NO-LOCK
         WHERE bf-panelHeader.company  EQ ipcCompany
           AND bf-panelHeader.linkType EQ gcPanelLinkTypePO
           AND bf-panelHeader.poID     EQ ipiPoID
           AND bf-panelHeader.poLine   EQ ipiPoLine
         NO-ERROR.
    IF NOT AVAILABLE bf-panelHeader THEN DO:        
        RUN pCreatePanelHeader (
            INPUT  ipcCompany,
            INPUT  gcPanelLinkTypePO, /* "P" */
            INPUT  ipiPoID,           /* Purchase Order */
            INPUT  ipiPoLine,         /* Purchase Order Line */
            INPUT  "",                /* Estimate ID */
            INPUT  0,                 /* Form No */
            INPUT  0,                 /* Blank No */
            INPUT  "",                /* Style ID */
            INPUT  "",                /* Flute ID */
            INPUT  "",                /* Score set Type */
            OUTPUT iPanelHeaderID        
            ).
            
        FIND FIRST bf-panelHeader NO-LOCK
             WHERE bf-panelHeader.panelHeaderID EQ iPanelHeaderID
             NO-ERROR.
    END.

    IF AVAILABLE bf-panelHeader THEN DO:
        FOR EACH ttPanel
            BY ttPanel.iPanelNum:
            RUN pUpdatePanelDetail (
                INPUT  bf-panelHeader.company,
                INPUT  bf-panelHeader.panelHeaderID,
                INPUT  ttPanel.cPanelType,
                INPUT  ttPanel.iPanelNum,
                INPUT  ttPanel.cPanelFormula,
                INPUT  ttPanel.dScoringAllowance,
                INPUT  ttPanel.cScoreType,
                INPUT  ttPanel.dPanelSize,
                INPUT  ttPanel.dPanelSizeFromFormula
                ).                        
        END.  
    END.
            
    RELEASE bf-panelHeader. 
END PROCEDURE.

PROCEDURE UpdatePanelDetailsForStyle:
/*------------------------------------------------------------------------------
 Purpose: Updates/Creates panelHeader and panelDetail records for a given Style
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcStyleID      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFluteID      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcScoreSetType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER TABLE           FOR ttPanel.
    
    DEFINE VARIABLE iPanelHeaderID AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-panelHeader FOR panelHeader.
    
    FIND FIRST bf-panelHeader NO-LOCK
         WHERE bf-panelHeader.company      EQ ipcCompany
           AND bf-panelHeader.linkType     EQ gcPanelLinkTypeStyle
           AND bf-panelHeader.styleID      EQ ipcStyleID
           AND bf-panelHeader.fluteID      EQ ipcFluteID
           AND bf-panelHeader.scoreSetType EQ ipcScoreSetType
         NO-ERROR.
    IF NOT AVAILABLE bf-panelHeader THEN DO:        
        RUN pCreatePanelHeader (
            INPUT  ipcCompany,
            INPUT  gcPanelLinkTypeStyle, /* "S" */
            INPUT  0,                    /* Purchase Order */
            INPUT  0,                    /* Purchase Order Line */
            INPUT  "",                   /* Estimate ID */
            INPUT  0,                    /* Form No */
            INPUT  0,                    /* Blank No */
            INPUT  ipcStyleID,           /* Style ID */
            INPUT  ipcFluteID,           /* Flute ID */
            INPUT  ipcScoreSetType,      /* Score set Type */
            OUTPUT iPanelHeaderID        
            ).
            
        FIND FIRST bf-panelHeader NO-LOCK
             WHERE bf-panelHeader.panelHeaderID EQ iPanelHeaderID
             NO-ERROR.
    END.

    IF AVAILABLE bf-panelHeader THEN DO:
        FOR EACH ttPanel
            BY ttPanel.iPanelNum:
            RUN pUpdatePanelDetail (
                INPUT  bf-panelHeader.company,
                INPUT  bf-panelHeader.panelHeaderID,
                INPUT  ttPanel.cPanelType,
                INPUT  ttPanel.iPanelNum,
                INPUT  ttPanel.cPanelFormula,
                INPUT  ttPanel.dScoringAllowance,
                INPUT  ttPanel.cScoreType,
                INPUT  ttPanel.dPanelSize,
                INPUT  ttPanel.dPanelSizeFromFormula
                ).                        
        END.  
    END.
            
    RELEASE bf-panelHeader. 
END PROCEDURE.

PROCEDURE ValidatePanelSize:
/*------------------------------------------------------------------------------
 Purpose: Validate the size of the panel given a size format 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipdInput       AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcSizeFormat  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplDecimalFlag AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess     AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE dDecimalValue      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cInputDecimalValue AS CHARACTER NO-UNDO.
    
    IF ipcSizeFormat EQ "Decimal" THEN
        dDecimalValue = 1.
    ELSE IF ipcSizeFormat EQ "16th's" THEN
        dDecimalValue = 0.16.
    ELSE IF ipcSizeFormat EQ "32nd's" THEN
        dDecimalValue = 0.32.

    IF ipdInput - TRUNCATE(ipdInput,0) GE dDecimalValue THEN DO:
        ASSIGN
            opcMessage = "Can not have more than " + STRING(dDecimalValue - 0.01) + " as decimal"
            oplSuccess = FALSE
            .
        RETURN.
    END.

    cInputDecimalValue = SUBSTRING(STRING(ipdInput - TRUNCATE(ipdInput, 0)), 1, 3).

    IF iplDecimalFlag AND ipcSizeFormat EQ "Decimal" THEN DO:
        IF LOOKUP(cInputDecimalValue, gcValidPanelDecimals) EQ 0 THEN DO:
            ASSIGN
                opcMessage = "Invalid dimension"
                oplSuccess = FALSE
                .            
            RETURN.
        END.
    END.
        
    ASSIGN
        opcMessage = "Success"
        oplSuccess = TRUE
        .  
END PROCEDURE.

