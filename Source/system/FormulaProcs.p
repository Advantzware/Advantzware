
/*------------------------------------------------------------------------
    File        : FormulaProcs.p
    Purpose     : Deprecate kstyle.i

    Syntax      :

    Description : Procedures and functions for processing formulas

    Author(s)   : BV
    Created     : Tue Dec 03 15:44:36 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/
  /*  Mod: Ticket - 103137 Format Change for Order No. and Job No.       */

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
    DEFINE BUFFER bf-item  FOR item.
    
    DEFINE VARIABLE dScoresW AS DECIMAL EXTENT 20.
    DEFINE VARIABLE cScoreTypesW AS CHARACTER EXTENT 20.
    DEFINE VARIABLE dScoresL AS DECIMAL EXTENT 20.
    DEFINE VARIABLE cScoreTypesL AS CHARACTER EXTENT 20.
   
    DEFINE VARIABLE cScoreSet     AS CHARACTER NO-UNDO.
    
    FIND FIRST bf-eb NO-LOCK 
         WHERE ROWID(bf-eb) EQ ipriEb
         NO-ERROR.
    IF NOT AVAILABLE bf-eb THEN 
        RETURN.

    FIND FIRST bf-style NO-LOCK 
         WHERE bf-style.company EQ bf-eb.company
           AND bf-style.style   EQ bf-eb.style
         NO-ERROR.
          
    FOR EACH ttPanel
        BREAK BY ttPanel.cPanelType:
        IF FIRST-OF(ttPanel.cPanelType) AND ttPanel.cPanelType BEGINS "W" THEN DO:   
            IF AVAILABLE bf-style AND bf-style.formula[20] NE "" THEN
                cScoreSet = "POBlankWidth".
            ELSE
                cScoreSet = "2".
                           
            RUN pGetScoring(
                INPUT  bf-eb.company,
                INPUT  bf-eb.style,
                INPUT  bf-eb.flute, 
                INPUT  cScoreSet, 
                OUTPUT dScoresW, 
                OUTPUT cScoreTypesW
                ).
        END.
        IF FIRST-OF(ttPanel.cPanelType) AND NOT ttPanel.cPanelType BEGINS "W" THEN DO: 
            FIND FIRST bf-item NO-LOCK 
                 WHERE bf-item.company EQ bf-eb.company
                   AND bf-item.i-no    EQ bf-eb.adhesive
                 NO-ERROR.
                 
            cScoreSet = "7".
            
            IF AVAILABLE bf-item THEN DO:
                IF bf-item.mat-type EQ "G" THEN 
                    cScoreSet = IF bf-eb.tab-in THEN 
                                    "3" 
                                ELSE 
                                    "4".
                ELSE IF bf-item.mat-type EQ "S" THEN 
                    cScoreSet = IF bf-eb.tab-in THEN 
                                    "5" 
                                ELSE 
                                    "6".
            END.
            
            RUN pGetScoring (
                INPUT  bf-eb.company,
                INPUT  bf-eb.style,
                INPUT  bf-eb.flute, 
                INPUT  cScoreSet, 
                OUTPUT dScoresL, 
                OUTPUT cScoreTypesL
                ).
        END.
        IF ttPanel.cPanelType BEGINS "W" THEN
            ASSIGN 
                ttPanel.dScoringAllowance = dScoresW[ttPanel.iPanelNum] * gdDecimalFactor
                ttPanel.cScoreType        = cScoreTypesW[ttPanel.iPanelNum]
                .
        ELSE 
            ASSIGN 
                ttPanel.dScoringAllowance = dScoresL[ttPanel.iPanelNum] * gdDecimalFactor
                ttPanel.cScoreType        = cScoreTypesL[ttPanel.iPanelNum]
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

        ttPanel.dPanelSize = ttPanel.dPanelSizeFromFormula.

        IF ttPanel.lAddAllowanceToSize THEN                        
            ttPanel.dPanelSize = ttPanel.dPanelSize + ttPanel.dScoringAllowance.
    END.

    /* Logic to validate square box and fetch square box fit value */
    IF bf-eb.len EQ bf-eb.wid THEN DO:
        RUN pAdjustSquareBoxFit(
            INPUT bf-eb.company,
            INPUT bf-eb.style,
            INPUT bf-eb.flute
            ).
    END.
    
    RUN pRoundPanelSize (
        INPUT bf-eb.company
        ).
END PROCEDURE.

PROCEDURE Formula_GetFormulaFromttPanel:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER TABLE FOR ttPanel.
    DEFINE OUTPUT PARAMETER opcFormulaLength AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFormulaWidth  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lAddBraces               AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iChar                    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cFormula                 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAddScoreAllowanceLength AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lAddScoreAllowanceWidth  AS LOGICAL   NO-UNDO.
    
    FOR EACH ttPanel
        BY ttPanel.iPanelNum:
        lAddBraces = FALSE.
        
        DO iChar = 1 TO LENGTH(gcValidOperators):
            IF INDEX(ttPanel.cPanelFormula, SUBSTRING(gcValidOperators, iChar, 1)) GT 0 THEN DO:
                lAddBraces = TRUE.
                LEAVE.
            END.
        END.
        
        cFormula = ttPanel.cPanelFormula.
        IF lAddBraces THEN
            cFormula = "(" + cFormula + ")".
            
        IF ttPanel.cPanelType EQ "W" THEN DO:
            opcFormulaWidth = opcFormulaWidth + "+" + cFormula.
            
            /* If atleast one of the panel size from formula does not match panel size then score allowance is added */
            lAddScoreAllowanceWidth = lAddScoreAllowanceWidth OR ttPanel.dPanelSizeFromFormula NE ttPanel.dPanelSize.
        END.
        ELSE IF ttPanel.cPanelType EQ "L" THEN DO:
            opcFormulaLength = opcFormulaLength + "+" + cFormula.
            
            /* If atleast one of the panel size from formula does not match panel size then score allowance is added */
            lAddScoreAllowanceLength = lAddScoreAllowanceLength OR ttPanel.dPanelSizeFromFormula NE ttPanel.dPanelSize.
        END.  
    END.
    
    IF lAddScoreAllowanceLength THEN
        opcFormulaLength = opcFormulaLength + "+" + "S".

    IF lAddScoreAllowanceWidth THEN
        opcFormulaWidth = opcFormulaWidth + "+" + "S".

    ASSIGN
        opcFormulaLength = TRIM(opcFormulaLength, "+")
        opcFormulaWidth  = TRIM(opcFormulaWidth, "+")
        .        
END PROCEDURE.

PROCEDURE Formula_GetReverseGrainAndEstimateTypeForPOLine:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriPOOrdl      AS ROWID     NO-UNDO.
    DEFINE OUTPUT PARAMETER opcReverseGrain AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiEstimateType AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    DEFINE BUFFER bf-eb      FOR eb.
    DEFINE BUFFER bf-ef      FOR ef.
    
    FIND FIRST bf-po-ordl NO-LOCK
         WHERE ROWID(bf-po-ordl) EQ ipriPOOrdl
         NO-ERROR.
    IF NOT AVAILABLE bf-po-ordl THEN
        RETURN.
        
    RUN pGetBuffersForPOLine (BUFFER bf-po-ordl, BUFFER bf-eb, BUFFER bf-ef).
    
    IF AVAILABLE bf-ef THEN
        ASSIGN
            opcReverseGrain = bf-ef.xgrain
            opiEstimateType = bf-ef.est-type
            .    
END PROCEDURE.

PROCEDURE Formula_ParseDesignScores:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimateID   AS CHARACTE  NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormNo       AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankNo      AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiDesignNo     AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER iplPrintMetric  AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttScoreLine.
    
    
    DEFINE VARIABLE cScoreLine  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dSizeFactor AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cSizeFormat AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCecScrnLog AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cCurrentSizeFormat AS CHARACTER NO-UNDO INIT "Decimal".
    
    
    DEFINE BUFFER bf-box-design-hdr  FOR box-design-hdr.
    DEFINE BUFFER bf-box-design-line FOR box-design-line.
    DEFINE BUFFER bf-eb              FOR eb.
    DEFINE BUFFER bf-est             FOR est.

    EMPTY TEMP-TABLE ttPanel.
    EMPTY TEMP-TABLE ttScoreLine.
    
    FIND FIRST bf-eb NO-LOCK
        WHERE bf-eb.company = ipcCompany
        AND bf-eb.est-no    = ipcEstimateID
        AND bf-eb.form-no   = ipiFormNo
        AND bf-eb.blank-no  = ipiBlankNo NO-ERROR.
    
    IF NOT AVAILABLE bf-eb THEN
        RETURN.
        
    FIND FIRST bf-est NO-LOCK
        WHERE bf-est.company = ipcCompany
        AND bf-est.est-no    = ipcEstimateID NO-ERROR.
    
    IF NOT AVAILABLE bf-est THEN
        RETURN.
    
    RUN Formula_GetPanelDetailsForPOScores (
        INPUT  bf-eb.company,
        INPUT  bf-eb.est-no,
        INPUT  bf-eb.form-no,
        INPUT  bf-eb.blank-no,
        OUTPUT TABLE ttPanel
        ).

    IF NOT CAN-FIND(FIRST ttPanel) THEN
        RETURN.
    
    RUN GetSizeFactor (
        INPUT  bf-eb.company,
        OUTPUT dSizeFactor,
        OUTPUT cSizeFormat,
        OUTPUT lCecScrnLog
        ). 
    
    IF cCurrentSizeFormat NE cSizeFormat THEN
        RUN SwitchPanelSizeFormatForttPanel (
            INPUT cCurrentSizeFormat,
            INPUT cSizeFormat,
            INPUT-OUTPUT TABLE ttPanel
            ). 


    FIND FIRST bf-box-design-hdr NO-LOCK
        WHERE bf-box-design-hdr.company = bf-eb.company
        AND bf-box-design-hdr.design-no = ipiDesignNo NO-ERROR.
    
    IF NOT AVAILABLE bf-box-design-hdr THEN
        RETURN.
    
    IF iplPrintMetric AND NOT bf-est.metric THEN
        RUN pConvertIntoMetricSizeForttPanel (INPUT-OUTPUT TABLE ttPanel).
       
    RUN pCreateScoreLine (bf-box-design-hdr.lscore, "L", NO,1, cCurrentSizeFormat, cSizeFormat).
    
    RUN pCreateScoreLine (bf-box-design-hdr.lcum-score, "L", YES,1, cCurrentSizeFormat, cSizeFormat).
    
    FOR EACH bf-box-design-line OF bf-box-design-hdr NO-LOCK:

        RUN pCreateScoreLine (bf-box-design-line.wscore, "W", NO, bf-box-design-line.line-no, cCurrentSizeFormat, cSizeFormat).
        RUN pCreateScoreLine (bf-box-design-line.wcum-score, "W", YES, bf-box-design-line.line-no, cCurrentSizeFormat, cSizeFormat).

    END.

END PROCEDURE.

PROCEDURE Formula_ReBuildBoxDesignForEstimate:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriEb AS ROWID NO-UNDO.
    
    DEFINE VARIABLE iExt AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-eb  FOR eb.
        
    EMPTY TEMP-TABLE ttPanel.
    
    RUN pBuildPanelDetailsForEstimate (
        INPUT  ipriEb,
        INPUT  TRUE,  /* Re-build */
        INPUT  TRUE,  /* Save */
        INPUT  "L,W", /* Panel Types to build */
        OUTPUT TABLE ttPanel
        ).
    
    DO TRANSACTION:
        FIND FIRST bf-eb EXCLUSIVE-LOCK
            WHERE ROWID(bf-eb) = ipriEb NO-ERROR.
        
        IF AVAILABLE bf-eb THEN
        DO:     
            IF CAN-FIND(FIRST ttPanel WHERE ttPanel.cPanelType EQ "L"
                AND ttPanel.dPanelSize NE 0) THEN 
            DO iExt = 1 TO EXTENT(bf-eb.k-len-array2): 
            
                FIND FIRST ttPanel WHERE ttPanel.cPanelType EQ "L"
                    AND ttPanel.iPanelNum  EQ iExt NO-ERROR.
    
                IF AVAILABLE ttPanel THEN
                    ASSIGN
                        bf-eb.k-len-array2[iExt]    = ttPanel.dPanelSize
                        bf-eb.k-len-scr-type2[iExt] = ttPanel.cScoreType
                        .
             
            END.  
        
            IF CAN-FIND(FIRST ttPanel WHERE ttPanel.cPanelType EQ "W"
                AND ttPanel.dPanelSize NE 0) THEN 
            DO iExt = 1 TO EXTENT(bf-eb.k-wid-array2): 
            
                FIND FIRST ttPanel WHERE ttPanel.cPanelType EQ "W"
                    AND ttPanel.iPanelNum  EQ iExt NO-ERROR.
    
                IF AVAILABLE ttPanel THEN
                    ASSIGN
                        bf-eb.k-wid-array2[iExt]    = ttPanel.dPanelSize
                        bf-eb.k-wid-scr-type2[iExt] = ttPanel.cScoreType
                        .
             
            END.
        END. // IF AVAILABLE bf-eb THEN
        
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

PROCEDURE Formula_GetPanelDetailsForPOScores:
/*------------------------------------------------------------------------------
 Purpose: A public method to return Estimate's Alt Design or PO Scores
 Notes: Fetches panelDetail records for a given Estimate
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimateID AS CHARACTE  NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormNo     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankNo    AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE         FOR ttPanel.    
    
    
    RUN GetPanelDetailsForEstimate (
        INPUT  ipcCompany,
        INPUT  ipcEstimateID,
        INPUT  ipiFormNo,
        INPUT  ipiBlankNo,
        INPUT gcPanelLinkTypeEstimate,
        OUTPUT TABLE ttPanel
        ).
                          
END PROCEDURE.

PROCEDURE GetPanelDetailsForEstimate PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Fetches panelDetail records for a given Estimate
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimateID AS CHARACTE  NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormNo     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankNo    AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPanelLinkType AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE         FOR ttPanel.    
    
    DEFINE BUFFER bf-panelHeader FOR panelHeader.
    
    FIND FIRST bf-panelHeader NO-LOCK
         WHERE bf-panelHeader.company    EQ ipcCompany
           AND bf-panelHeader.linkType   EQ ipcPanelLinkType
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

PROCEDURE DeletePanelDetailsForEstimate:
/*------------------------------------------------------------------------------
 Purpose: Fetches panelDetail records for a given Estimate
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimateID AS CHARACTE  NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormNo     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankNo    AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-panelHeader FOR panelHeader.
    
    FIND FIRST bf-panelHeader NO-LOCK
         WHERE bf-panelHeader.company    EQ ipcCompany
           AND bf-panelHeader.linkType   EQ gcPanelLinkTypeEstimate
           AND bf-panelHeader.estimateID EQ ipcEstimateID
           AND bf-panelHeader.formNo     EQ ipiFormNo
           AND bf-panelHeader.blankNo    EQ ipiBlankNo
         NO-ERROR.
    IF AVAILABLE bf-panelHeader THEN
        RUN pDeletePanelHeader (
            INPUT  bf-panelHeader.panelHeaderID
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

PROCEDURE DeletePanelDetailsForPO:
/*------------------------------------------------------------------------------
 Purpose: Fetches panelDetail records for a given Purchase Order
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoID    AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoLine  AS INTEGER   NO-UNDO.
        
    DEFINE BUFFER bf-panelHeader FOR panelHeader.
    
    FIND FIRST bf-panelHeader NO-LOCK
         WHERE bf-panelHeader.company  EQ ipcCompany
           AND bf-panelHeader.linkType EQ gcPanelLinkTypePO
           AND bf-panelHeader.poID     EQ ipiPoID
           AND bf-panelHeader.poLine   EQ ipiPoLine
         NO-ERROR.
    IF AVAILABLE bf-panelHeader THEN
        RUN pDeletePanelHeader (
            INPUT  bf-panelHeader.panelHeaderID
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

PROCEDURE DeletePanelDetailsForStyle:
/*------------------------------------------------------------------------------
 Purpose: Fetches panelDetail records for a given Style
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcStyleID      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFluteID      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcScoreSetType AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-panelHeader FOR panelHeader.
    
    FIND FIRST bf-panelHeader NO-LOCK
         WHERE bf-panelHeader.company      EQ ipcCompany
           AND bf-panelHeader.linkType     EQ gcPanelLinkTypeStyle
           AND bf-panelHeader.styleID      EQ ipcStyleID
           AND bf-panelHeader.fluteID      EQ ipcFluteID
           AND bf-panelHeader.scoreSetType EQ ipcScoreSetType
         NO-ERROR.
    IF AVAILABLE bf-panelHeader THEN
        RUN pDeletePanelHeader (
            INPUT  bf-panelHeader.panelHeaderID
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
        INPUT gcPanelLinkTypeEstimate,
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

PROCEDURE GetTotalScoreAllowanaceForStyle:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to fetch panel size and score type from Style
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcStyleID             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFluteID             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcScoreSetType        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdTotalScoreAllowance AS DECIMAL   NO-UNDO.

    EMPTY TEMP-TABLE ttPanel.
    
    RUN GetPanelDetailsForStyle (
        INPUT  ipcCompany,
        INPUT  ipcStyleID,
        INPUT  ipcFluteID,
        INPUT  ipcScoreSetType,
        OUTPUT TABLE ttPanel
        ).
    
    FOR EACH ttPanel:
        opdTotalScoreAllowance = opdTotalScoreAllowance + ttPanel.dScoringAllowance.
    END.
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
        INPUT  "", /* Panel Type. Empty for style */
        OUTPUT opdScores,
        OUTPUT opcScoreTypes
        ).

END PROCEDURE.

PROCEDURE pAdjustSquareBoxFit:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcStyle   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFlute   AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dSizeFactor   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cSizeFormat   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCecScrnLog   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE dSquareBoxFit AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cChar         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iChar         AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-reftable FOR reftable.
        
    RUN GetSizeFactor (
        INPUT  ipcCompany,
        OUTPUT dSizeFactor,
        OUTPUT cSizeFormat,
        OUTPUT lCecScrnLog
        ).    
    IF NOT lCecScrnLog THEN DO:
        FIND FIRST bf-reftable NO-LOCK
             WHERE bf-reftable.reftable EQ "STYFLU"
               AND bf-reftable.company  EQ ipcStyle
               AND bf-reftable.loc      EQ ipcFlute
               AND bf-reftable.code     EQ "DIM-FIT"
             NO-ERROR.
        IF AVAILABLE bf-reftable THEN 
            dSquareBoxFit = bf-reftable.val[1].        
    END.    
    
    /* Subtract square fit score from all the panels that contains "W" in its panel formula */
    FOR EACH ttPanel
        WHERE ttPanel.cPanelType BEGINS "W"
        BY ttPanel.iPanelNum DESCENDING:
        IF INDEX(ttPanel.cPanelFormula, "W") EQ 0 THEN
            NEXT.
            
        DO iChar = 1 TO LENGTH(ttPanel.cPanelFormula):
            cChar = SUBSTRING(ttPanel.cPanelFormula, iChar, 1).
            IF cChar EQ "W" THEN 
                ASSIGN
                    ttPanel.dScoringAllowance = ttPanel.dScoringAllowance - dSquareBoxFit
                    ttPanel.dPanelSize        = ttPanel.dPanelSize - dSquareBoxFit
                    .
        END.
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
    
    DEFINE VARIABLE iPanel         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iChar          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCharNextPlus  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cChar          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCharPrev      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPanelFormula  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lParentActive  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cParentFormula AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lAddScoreAllowanceToSize AS LOGICAL NO-UNDO.
    
    ASSIGN
        iChar     = 1
        iPanel    = 0
        cCharPrev = ''
        .
    
    IF INDEX(ipcFormula, "S") GT 0 THEN
        lAddScoreAllowanceToSize = TRUE.
    
    /* replace space character */
    ipcFormula =REPLACE(ipcFormula, " ", "").
    
    /* Loop through each character in the formula (Style 'L' formula eg. L+W+L+W+J+S for RSC) */
    DO WHILE iPanel LE 20 AND iChar LE LENGTH(ipcFormula):
        ASSIGN  /* pick the previous and next character in formula */
            cChar     = SUBSTRING(ipcFormula,iChar,1)
            cCharPrev = IF iChar EQ 1 THEN '+' ELSE SUBSTRING(ipcFormula,iChar - 1,1)    
            .
            
        IF cChar EQ "S" THEN DO:
            iChar = iChar + 1.
            NEXT.
        END.
                  
        IF cCharPrev EQ '' THEN cCharPrev = '+'.
             
        IF cChar NE '+' AND cCharPrev EQ '+' THEN DO: 
            iPanel = iPanel + 1.
            CREATE ttPanel.
            ASSIGN 
                ttPanel.cPanelType          = ipcPanelType
                ttPanel.iPanelNum           = iPanel
                ttPanel.cPanelFormula       = cChar
                ttPanel.lAddAllowanceToSize = lAddScoreAllowanceToSize
                .
        END.
        ELSE IF cChar NE '+' THEN DO:  
            FIND FIRST ttPanel
                 WHERE ttPanel.cPanelType EQ ipcPanelType
                   AND ttPanel.iPanelNum  EQ iPanel 
                 NO-ERROR.
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
            IF SUBSTRING(ttPanel.cPanelFormula, LENGTH(ttPanel.cPanelFormula), 1) EQ ")" AND SUBSTRING(ttPanel.cPanelFormula, 1, 1) EQ "(" THEN
                NEXT.            
            ELSE IF SUBSTRING(ttPanel.cPanelFormula, LENGTH(ttPanel.cPanelFormula), 1) EQ ")" THEN DO:
                ASSIGN 
                    lParentActive  = YES
                    cParentFormula = "+" + SUBSTRING(ttPanel.cPanelFormula, 1, LENGTH(ttPanel.cPanelFormula) - 1)
                    .
                DELETE ttPanel.
            END.            
            ELSE IF SUBSTRING(ttPanel.cPanelFormula, 1, 1) EQ "(" THEN DO:
                ASSIGN 
                    lParentActive         = NO
                    cParentFormula        = SUBSTRING(ttPanel.cPanelFormula, 2, LENGTH(ttPanel.cPanelFormula) - 1) + cParentFormula
                    ttPanel.cPanelFormula = cParentFormula
                    .
            END.            
            ELSE IF lParentActive THEN DO:
                cParentFormula = "+" + ttPanel.cPanelFormula + cParentFormula.
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

PROCEDURE Formula_ReBuildAndSavePanelDetailsForEstimate:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriEb AS ROWID NO-UNDO.
    
    EMPTY TEMP-TABLE ttPanel.
    
    RUN pBuildPanelDetailsForEstimate (
        INPUT  ipriEb,
        INPUT  TRUE,  /* Re-build */
        INPUT  TRUE,  /* Save */
        INPUT  "L,W", /* Panel Types to build */
        OUTPUT TABLE ttPanel
        ).
END PROCEDURE.

PROCEDURE Formula_ReBuildPanelDetailsForEstimate:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriEb AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttPanel.
    
    EMPTY TEMP-TABLE ttPanel.
    
    RUN pBuildPanelDetailsForEstimate (
        INPUT  ipriEb,
        INPUT  TRUE,   /* Re-build */
        INPUT  FALSE,  /* Save */
        INPUT  "L,W",  /* Panel Types to build */
        OUTPUT TABLE ttPanel
        ).
END PROCEDURE.

PROCEDURE Formula_BuildPanelDetailsForEstimate:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriEb AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttPanel.
    
    EMPTY TEMP-TABLE ttPanel.
    
    RUN pBuildPanelDetailsForEstimate (
        INPUT  ipriEb,
        INPUT  FALSE,  /* Re-build */
        INPUT  FALSE,  /* Save */
        INPUT  "L,W",  /* Panel Types to build */
        OUTPUT TABLE ttPanel
        ).
END PROCEDURE.

PROCEDURE Formula_BuildAndSavePanelDetailsForEstimate:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriEb AS ROWID NO-UNDO.
    
    EMPTY TEMP-TABLE ttPanel.
    
    RUN pBuildPanelDetailsForEstimate (
        INPUT  ipriEb,
        INPUT  FALSE,  /* Re-build */
        INPUT  TRUE,   /* Save */
        INPUT  "L,W",  /* Panel Types to build */
        OUTPUT TABLE ttPanel
        ).
END PROCEDURE.

PROCEDURE pBuildPanelDetailsForEstimate PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriEb        AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER iplReBuild    AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplSave       AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPanelTypes AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttPanel.
    
    DEFINE VARIABLE lRebuild       AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lRebuildLength AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lRebuildWidth  AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-eb    FOR eb.
    DEFINE BUFFER bf-style FOR style.
    
    FIND FIRST bf-eb NO-LOCK 
         WHERE ROWID(bf-eb) EQ ipriEb
         NO-ERROR.
    IF NOT AVAILABLE bf-eb THEN
        RETURN.     

    ASSIGN
        lRebuildLength = iplRebuild
        lRebuildWidth  = iplRebuild
        .
        
    RUN GetPanelDetailsForEstimate (
        INPUT  bf-eb.company,
        INPUT  bf-eb.est-no,
        INPUT  bf-eb.form-no,
        INPUT  bf-eb.blank-no,
        INPUT gcPanelLinkTypeEstimate,
        OUTPUT TABLE ttPanel
        ).
    IF NOT iplRebuild THEN DO:
        IF LOOKUP("W", ipcPanelTypes) GT 0 THEN DO:
            FIND FIRST ttPanel 
                 WHERE ttPanel.cPanelType EQ "W"
                 NO-ERROR.
            IF NOT AVAILABLE ttPanel THEN
                ASSIGN
                    lRebuild       = TRUE
                    lRebuildLength = TRUE
                    .    
        END.

        IF LOOKUP("L", ipcPanelTypes) GT 0 THEN DO:
            FIND FIRST ttPanel 
                 WHERE ttPanel.cPanelType EQ "L"
                 NO-ERROR.
            IF NOT AVAILABLE ttPanel THEN
                ASSIGN
                    lRebuild      = TRUE
                    lRebuildWidth = TRUE
                    .    
        END.
        
        IF NOT lRebuild THEN
            RETURN.
    END.
     
    FIND FIRST bf-style NO-LOCK 
         WHERE bf-style.company EQ bf-eb.company
           AND bf-style.style   EQ bf-eb.style
         NO-ERROR.
    IF AVAILABLE bf-style THEN DO:
        /* Use formula[20] for 2UP and formula[1] for 1UP */
        IF LOOKUP("W", ipcPanelTypes) GT 0 AND lRebuildWidth THEN DO:
            FOR EACH ttPanel 
                WHERE ttPanel.cPanelType EQ "W":
                DELETE ttPanel.
            END.
            RUN ParsePanels(
                INPUT  IF bf-style.formula[20] NE "" THEN bf-style.formula[20] ELSE bf-style.formula[1], 
                INPUT  "W", /* Widths */
                OUTPUT TABLE ttPanel
                ).
        END.
        IF LOOKUP("L", ipcPanelTypes) GT 0 AND lRebuildLength THEN DO: 
            FOR EACH ttPanel 
                WHERE ttPanel.cPanelType EQ "L":
                DELETE ttPanel.
            END.
            RUN ParsePanels(
                INPUT  bf-style.formula[2], 
                INPUT  "L",  /* Lengths */
                OUTPUT TABLE ttPanel
                ).
        END.
        
        RUN CalculatePanels (
            INPUT        ROWID(bf-eb),
            INPUT-OUTPUT TABLE ttPanel
            ).            
    END.
    
    IF iplSave THEN
        RUN pUpdatePanelDetails (
            INPUT  gcPanelLinkTypeEstimate,
            INPUT  bf-eb.company,
            INPUT  0,                    /* Purchase Order */
            INPUT  0,                    /* Purchase Order Line */
            INPUT  bf-eb.est-no,
            INPUT  bf-eb.form-no,
            INPUT  bf-eb.blank-no,
            INPUT  bf-eb.style,           /* Style ID */
            INPUT  bf-eb.flute,           /* Flute ID */
            INPUT  "",                    /* Score set Type */
            INPUT  TABLE ttPanel
            ).    
END PROCEDURE.

PROCEDURE Formula_ReBuildAndSavePanelDetailsForPO:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriPOOrdl AS ROWID NO-UNDO.
    
    EMPTY TEMP-TABLE ttPanel.
    
    RUN pBuildPanelDetailsForPO (
        INPUT  ipriPOOrdl,
        INPUT  TRUE,   /* Re-build */
        INPUT  TRUE,   /* Save */
        INPUT  "L,W",   /* Panel Types to build */
        OUTPUT TABLE ttPanel
        ).
END PROCEDURE.

PROCEDURE Formula_ReBuildPanelDetailsForPO:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriPOOrdl AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttPanel.
    
    EMPTY TEMP-TABLE ttPanel.
    
    RUN pBuildPanelDetailsForPO (
        INPUT  ipriPOOrdl,
        INPUT  TRUE,   /* Re-build */
        INPUT  TRUE,   /* Save */
        INPUT  "L,W",  /* Panel Types to build */
        OUTPUT TABLE ttPanel
        ).
END PROCEDURE.

PROCEDURE Formula_BuildPanelDetailsForPO:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriPOOrdl AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttPanel.
    
    EMPTY TEMP-TABLE ttPanel.
    
    RUN pBuildPanelDetailsForPO (
        INPUT  ipriPOOrdl,
        INPUT  FALSE,  /* Re-build */
        INPUT  FALSE,  /* Save */
        INPUT  "L,W",  /* Panel Types to build */
        OUTPUT TABLE ttPanel
        ).
END PROCEDURE.

PROCEDURE Formula_BuildAndSavePanelDetailsForPO:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriPOOrdl AS ROWID NO-UNDO.
    
    EMPTY TEMP-TABLE ttPanel.
    
    RUN pBuildPanelDetailsForPO (
        INPUT  ipriPOOrdl,
        INPUT  FALSE,  /* Re-build */
        INPUT  TRUE,   /* Save */
        INPUT  "L,W",  /* Panel Types to build */
        OUTPUT TABLE ttPanel
        ).
END PROCEDURE.

PROCEDURE pBuildPanelDetailsForPO PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriPOOrdl    AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER iplReBuild    AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplSave       AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPanelTypes AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttPanel.

    DEFINE VARIABLE lRebuild AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-eb      FOR eb.
    DEFINE BUFFER bf-ef      FOR ef.
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    DEFINE BUFFER bf-style   FOR style.
    
    FIND FIRST bf-po-ordl NO-LOCK
         WHERE ROWID(bf-po-ordl) EQ ipriPOOrdl
         NO-ERROR.
    IF NOT AVAILABLE bf-po-ordl THEN
        RETURN.
        
    RUN GetPanelDetailsForPO (
        INPUT  bf-po-ordl.company,
        INPUT  bf-po-ordl.po-no,
        INPUT  bf-po-ordl.line,
        OUTPUT TABLE ttPanel
        ).        
    IF NOT iplRebuild THEN DO:
        IF LOOKUP("W", ipcPanelTypes) GT 0 THEN DO:
            FIND FIRST ttPanel 
                 WHERE ttPanel.cPanelType EQ "W"
                 NO-ERROR.
            IF NOT AVAILABLE ttPanel THEN
                lRebuild = TRUE.    
        END.

        IF LOOKUP("L", ipcPanelTypes) GT 0 THEN DO:
            FIND FIRST ttPanel 
                 WHERE ttPanel.cPanelType EQ "L"
                 NO-ERROR.
            IF NOT AVAILABLE ttPanel THEN
                lRebuild = TRUE.    
        END.
        
        IF NOT lRebuild THEN
            RETURN.
    END.
    
    RUN pGetBuffersForPOLine (BUFFER bf-po-ordl, BUFFER bf-eb, BUFFER bf-ef).
    
    IF AVAILABLE bf-eb THEN DO:
        EMPTY TEMP-TABLE ttPanel.

        FIND FIRST bf-style NO-LOCK
             WHERE bf-style.company  EQ bf-eb.company
               AND bf-style.style    EQ bf-eb.style
               AND bf-style.type     EQ "B"
               AND bf-style.industry EQ "2"
             NO-ERROR.
        IF AVAILABLE bf-style THEN        
            RUN pBuildPanelDetailsForEstimate (
                INPUT  ROWID(bf-eb),
                INPUT  FALSE,  /* Re-build */
                INPUT  FALSE,  /* Save */
                INPUT  ipcPanelTypes,   /* Panel Types to build */
                OUTPUT TABLE ttPanel
                ).       
    END.
    
    IF iplSave AND AVAILABLE bf-eb AND AVAILABLE bf-style THEN
        RUN pUpdatePanelDetails (
            INPUT  gcPanelLinkTypePO,
            INPUT  bf-eb.company,
            INPUT  bf-po-ordl.po-no,
            INPUT  bf-po-ordl.line,
            INPUT  bf-eb.est-no,
            INPUT  bf-eb.form-no,
            INPUT  bf-eb.blank-no,
            INPUT  bf-eb.style,           /* Style ID */
            INPUT  bf-eb.flute,           /* Flute ID */
            INPUT  "",                     /* Score set Type */
            INPUT  TABLE ttPanel
            ). 
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
            ttPanel.dPanelSizeDecimal     = bf-panelDetail.panelSize
            .            
    END.    
    
    RELEASE bf-panelDetail.
END PROCEDURE.

PROCEDURE pDeletePanelHeader PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiPanelHeaderID AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-panelHeader FOR panelHeader.
    DEFINE BUFFER bf-panelDetail FOR panelDetail.
    
    FIND FIRST bf-panelHeader EXCLUSIVE-LOCK
         WHERE bf-panelHeader.panelHeaderID EQ ipiPanelHeaderID
         NO-ERROR.
    IF AVAILABLE bf-panelHeader THEN DO:
        RUN pDeletePanelDetail (
            INPUT bf-panelHeader.panelHeaderID
            ).
               
        DELETE bf-panelHeader.
    END.

END PROCEDURE.

PROCEDURE pDeletePanelDetail PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiPanelHeaderID AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-panelDetail FOR panelDetail.
    
    FOR EACH bf-panelDetail EXCLUSIVE-LOCK
        WHERE bf-panelDetail.panelHeaderID EQ ipiPanelHeaderID:
        DELETE bf-panelDetail.
    END.    
END PROCEDURE.

PROCEDURE pGetBuffersForPOLine:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-po-ordl FOR po-ordl.
    DEFINE PARAMETER BUFFER opbf-eb      FOR eb.
    DEFINE PARAMETER BUFFER opbf-ef      FOR ef.
    
    DEFINE VARIABLE cReturnChar      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lPOFarmOutScores AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE riEb             AS ROWID     NO-UNDO.
    
    DEFINE VARIABLE lRebuild AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-itemfg  FOR itemfg.
    DEFINE BUFFER bf-job     FOR job.
    DEFINE BUFFER bf-est-qty FOR est-qty.
    DEFINE BUFFER bf-job-mat FOR job-mat.
    DEFINE BUFFER bf-item    FOR item.
    DEFINE BUFFER bf-est     FOR est.
    DEFINE BUFFER bf-style   FOR style.
    
    IF NOT AVAILABLE ipbf-po-ordl THEN
        RETURN.
          
    RUN sys/ref/nk1look.p (
        INPUT ipbf-po-ordl.company, /* Company Code */ 
        INPUT "POFarmOutScores",  /* sys-ctrl name */
        INPUT "L",                /* Output return value */
        INPUT NO,                 /* Use ship-to */
        INPUT NO,                 /* ship-to vendor */
        INPUT "",                 /* ship-to vendor value */
        INPUT "",                 /* shi-id value */
        OUTPUT cReturnChar, 
        OUTPUT lRecFound
        ).
    lPOFarmOutScores = cReturnChar EQ "YES".
    
    IF ipbf-po-ordl.i-no NE "" AND NOT ipbf-po-ordl.item-type AND lPOFarmOutScores THEN DO:
        FIND FIRST bf-itemfg NO-LOCK
             WHERE bf-itemfg.company EQ ipbf-po-ordl.company
               AND bf-itemfg.i-no    EQ ipbf-po-ordl.i-no
             NO-ERROR.
        IF AVAILABLE bf-itemfg AND bf-itemfg.est-no NE "" THEN DO:            
            FOR LAST bf-est-qty NO-LOCK
                WHERE bf-est-qty.company EQ ipbf-po-ordl.company
                  AND bf-est-qty.est-no  EQ bf-itemfg.est-no 
                USE-INDEX est-qty,
                FIRST opbf-ef NO-LOCK
                WHERE opbf-ef.company EQ bf-est-qty.company 
                  AND opbf-ef.est-no  EQ bf-est-qty.est-no 
                  AND opbf-ef.eqty    EQ bf-est-qty.eqty 
                USE-INDEX est-qty,
                FIRST opbf-eb NO-LOCK 
                WHERE opbf-eb.company  EQ opbf-ef.company
                  AND opbf-eb.est-no   EQ opbf-ef.est-no 
                  AND opbf-eb.stock-no EQ ipbf-po-ordl.i-no
                  AND opbf-eb.eqty     EQ opbf-ef.eqty 
                USE-INDEX est-qty:
                riEb = ROWID(opbf-eb).
            END.
            
            IF riEb NE ? THEN
                FIND FIRST opbf-eb NO-LOCK
                     WHERE ROWID(opbf-eb) EQ riEb
                     NO-ERROR.  
        END.
    END.

    IF ipbf-po-ordl.i-no NE "" AND ipbf-po-ordl.item-type THEN DO:
        FIND FIRST bf-item NO-LOCK
             WHERE bf-item.company EQ ipbf-po-ordl.company
               AND bf-item.i-no    EQ ipbf-po-ordl.i-no
             NO-ERROR.
        IF AVAILABLE bf-item AND LOOKUP(bf-item.mat-type, "B,P") EQ 0 THEN
            NEXT.
            
        IF ipbf-po-ordl.job-no NE "" THEN
            FIND FIRST bf-job-mat NO-LOCK 
                 WHERE bf-job-mat.company    EQ ipbf-po-ordl.company
                   AND bf-job-mat.rm-i-no    EQ ipbf-po-ordl.i-no
                   AND trim(bf-job-mat.job-no) EQ trim(ipbf-po-ordl.job-no)  
                   AND bf-job-mat.job-no2    EQ ipbf-po-ordl.job-no2
                   AND bf-job-mat.i-no       EQ ipbf-po-ordl.i-no
                   AND ((bf-job-mat.frm      EQ ipbf-po-ordl.s-num AND ipbf-po-ordl.s-num NE 0) OR
                         ipbf-po-ordl.s-num    EQ 0 OR 
                         ipbf-po-ordl.s-num    EQ ?)
                   AND ((bf-job-mat.blank-no EQ ipbf-po-ordl.b-num AND ipbf-po-ordl.b-num NE 0) OR
                         ipbf-po-ordl.b-num    EQ 0)
                 USE-INDEX i-no 
                 NO-ERROR.

        IF AVAILABLE bf-job-mat THEN
            FIND FIRST bf-job NO-LOCK 
                 WHERE bf-job.company EQ bf-job-mat.company
                   AND bf-job.job     EQ bf-job-mat.job
                   AND bf-job.job-no  EQ bf-job-mat.job-no
                   AND bf-job.job-no2 EQ bf-job-mat.job-no2
                 NO-ERROR.
                 
        IF AVAILABLE bf-job THEN
            FIND FIRST bf-est NO-LOCK
                 WHERE bf-est.company EQ bf-job.company
                   AND bf-est.est-no  EQ bf-job.est-no
                 NO-ERROR.
                 
        IF AVAILABLE bf-est THEN
            FIND FIRST opbf-ef NO-LOCK 
                 WHERE opbf-ef.company EQ bf-est.company
                   AND opbf-ef.est-no  EQ bf-est.est-no
                   AND opbf-ef.form-no EQ bf-job-mat.frm
                 NO-ERROR.
                
        IF AVAILABLE opbf-ef THEN
            FIND FIRST opbf-eb NO-LOCK
                 WHERE opbf-eb.company EQ opbf-ef.company
                   AND opbf-eb.est-no  EQ opbf-ef.est-no
                   AND opbf-eb.form-no EQ opbf-ef.form-no
                 NO-ERROR.
    END.     

END PROCEDURE.

PROCEDURE pGetRounded16ths PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER iopdSize AS DECIMAL NO-UNDO.
    
    ASSIGN
        iopdSize = iopdSize * 16
        iopdSize = TRUNCATE(iopdSize, 0)
        iopdSize = iopdSize / 16
        . 
END PROCEDURE.

PROCEDURE pGetRounded32nds PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER iopdSize AS DECIMAL NO-UNDO.
    
    ASSIGN
        iopdSize = iopdSize * 32
        iopdSize = TRUNCATE(iopdSize, 0)
        iopdSize = iopdSize / 32
        . 
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
        WHERE ttPanel.cPanelType EQ ipcPanelType
        BY ttPanel.iPanelNum:
        
        IF ttPanel.iPanelNum GT 20 THEN
            NEXT.
            
        ASSIGN
            opdScores[ttPanel.iPanelNum]     = ttPanel.dPanelSize
            opcScoreTypes[ttPanel.iPanelNum] = ttPanel.cScoreType
            .
    END.
END PROCEDURE.

PROCEDURE pCreateScoreLine PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcScoreTxt         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPanelType        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplTotal            AS LOGICAL NO-UNDO.
    DEFINE INPUT  PARAMETER ipiLineNo           AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcOriginalFormat   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcNewFormat        AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iCnt         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cTempVal     AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cScrNum      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cScoreLine   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iSpaceCount  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dTmpScore    AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE cRepChar     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTempChar    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cNewText     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iNum         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE deFinalScore AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE deCumulScore AS DECIMAL   NO-UNDO.


    ASSIGN 
        cNewText = ipcScoreTxt.

    DO iCnt = 1 TO LENGTH(ipcScoreTxt):
        ASSIGN 
            cTempChar = TRIM(SUBSTRING(ipcScoreTxt, iCnt, 1)).

        IF (cTempChar = "" OR iCnt = LENGTH(ipcScoreTxt)) and cRepChar <> "" Then
        DO: 
            IF iCnt = LENGTH(ipcScoreTxt) AND cTempChar <> "" THEN
                ASSIGN cRepChar = cRepChar + cTempChar.

            ASSIGN 
                iNum = INTEGER(REPLACE(REPLACE(cRepChar, "[", ""), "]", "")) NO-ERROR.

            IF NOT ERROR-STATUS:ERROR THEN
            DO:
                IF ipcPanelType = "W" THEN
                    FOR EACH ttPanel 
                        WHERE ttPanel.cPanelType EQ ipcPanelType
                        AND ttPanel.iPanelNum  LT iNum:

                        ASSIGN 
                            dTmpScore = ttPanel.dPanelSize.


                        IF iplTotal THEN
                            deCumulScore = deCumulScore + ttPanel.dPanelSizeDecimal.
                    END.

                FIND FIRST ttPanel 
                    WHERE ttPanel.cPanelType EQ ipcPanelType
                    AND ttPanel.iPanelNum  EQ iNum NO-ERROR.

                IF AVAILABLE ttPanel THEN
                DO:

                    ASSIGN 
                        dTmpScore   = ttPanel.dPanelSize.

                     IF iplTotal THEN DO:
                        deCumulScore = deCumulScore + ttPanel.dPanelSizeDecimal.
                        IF ipcOriginalFormat NE ipcNewFormat THEN 
                            RUN SwitchPanelSizeFormat (
                                INPUT  ipcOriginalFormat,
                                INPUT  ipcNewFormat,
                                INPUT  deCumulScore,
                                OUTPUT deFinalScore
                                ).                           
                     END.
                    ELSE
                        deFinalScore  = dTmpScore.
                     
                    cNewText = REPLACE(cNewText, cRepChar, string(deFinalScore)) . 
                END.                        

            END.
            ASSIGN 
                cRepChar = "".

        END.

        IF cTempChar <> "" THEN
            ASSIGN cRepChar = cRepChar + cTempChar.

    END.
    
    FIND FIRST ttScoreLine
        WHERE ttScoreLine.PanelType = ipcPanelType
          AND ttScoreLine.LineNum = ipiLineNo NO-ERROR.
          
    IF NOT AVAILABLE ttScoreLine THEN
    DO: 
        CREATE ttScoreLine.
        ASSIGN 
            ttScoreLine.PanelType = ipcPanelType
            ttScoreLine.LineNum   = ipiLineNo.
    END.  
  
    IF iplTotal THEN
        ttScoreLine.ScoreLineTotal = cNewText.
    ELSE
        ttScoreLine.ScoreLine      = cNewText.
            

END PROCEDURE.

PROCEDURE pRoundPanelSize PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cSizeFormat AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lRound      AS LOGICAL   NO-UNDO.
        
    RUN pGetSettingRound (
        INPUT  ipcCompany,
        OUTPUT lRound
        ).
    
    RUN sys/ref/nk1look.p (
        INPUT ipcCompany,     /* Company Code */ 
        INPUT "CECSCRN",      /* sys-ctrl name */
        INPUT "C",            /* Output return value */
        INPUT NO,             /* Use ship-to */
        INPUT NO,             /* ship-to vendor */
        INPUT "",             /* ship-to vendor value */
        INPUT "",             /* shi-id value */
        OUTPUT cSizeFormat, 
        OUTPUT lRecFound
        ).
        
    IF cSizeFormat NE "Decimal" THEN DO:
        FOR EACH ttPanel:
            IF cSizeFormat EQ "16th's" THEN DO:
                IF lRound THEN DO:
                    ttPanel.dPanelSize = ttPanel.dPanelSize * 16.
                    
                    IF (ttPanel.dPanelSize - INTEGER(ttPanel.dPanelSize)) GT 0 THEN 
                        ttPanel.dPanelSize = INTEGER(ttPanel.dPanelSize) + 1.
                    ELSE 
                        ttPanel.dPanelSize = INTEGER(ttPanel.dPanelSize).
                        
                    ttPanel.dPanelSize = ttPanel.dPanelSize / 16.
                END.
                ELSE
                    RUN pGetRounded16ths(
                        INPUT-OUTPUT ttPanel.dPanelSize
                        ).
            END.
            ELSE IF cSizeFormat EQ "32nd's" THEN DO:
                IF lRound THEN DO:
                    ttPanel.dPanelSize = ttPanel.dPanelSize * 32.
                    
                    IF (ttPanel.dPanelSize - INTEGER(ttPanel.dPanelSize)) GT 0 THEN 
                        ttPanel.dPanelSize = INTEGER(ttPanel.dPanelSize) + 1.
                    ELSE 
                        ttPanel.dPanelSize = INTEGER(ttPanel.dPanelSize).
                        
                    ttPanel.dPanelSize = ttPanel.dPanelSize / 32.
                END.
                ELSE
                    RUN pGetRounded32nds(
                        INPUT-OUTPUT ttPanel.dPanelSize
                        ).
            END.
        END.
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
            bf-panelDetail.scoreType            = IF ipcScoreType EQ ? THEN "" ELSE ipcScoreType 
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
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcStyle      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFlute      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcScoreSet   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdScores     AS DECIMAL   NO-UNDO EXTENT 20.
    DEFINE OUTPUT PARAMETER opcScoreTypes AS CHARACTER NO-UNDO EXTENT 20.

    DEFINE BUFFER bf-reftable    FOR reftable.
    DEFINE BUFFER bf-panelHeader FOR panelHeader.
    DEFINE BUFFER bf-panelDetail FOR panelDetail.

    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.

    IF ipcScoreSet EQ "POBlankWidth" THEN DO:
        FIND FIRST bf-panelHeader NO-LOCK
             WHERE bf-panelHeader.company      EQ ipcCompany
               AND bf-panelHeader.linkType     EQ gcPanelLinkTypeStyle
               AND bf-panelHeader.styleID      EQ ipcStyle
               AND bf-panelHeader.fluteID      EQ ipcFlute
               AND bf-panelHeader.scoreSetType EQ ipcScoreSet
             NO-ERROR.
        IF AVAILABLE bf-panelHeader THEN DO:           
            FOR EACH bf-panelDetail NO-LOCK
                WHERE bf-panelDetail.panelHeaderID EQ bf-panelHeader.panelHeaderID:
                ASSIGN
                    opdScores[bf-panelDetail.panelNo]     = bf-panelDetail.scoringAllowance
                    opcScoreTypes[bf-panelDetail.panelNo] = bf-panelDetail.scoreType
                    .            

                /* As lengths are in 16th's, need to convert score allownace to 16th's before adding */
                RUN ConvertDecimalTo16ths (
                    INPUT-OUTPUT opdScores[bf-panelDetail.panelNo]
                    ).                
            END.         
        END.
          
        RETURN.
    END.
    
    FIND FIRST bf-reftable NO-LOCK
         WHERE bf-reftable.reftable EQ "STYSCORE"
           AND bf-reftable.company  EQ ipcStyle
           AND bf-reftable.loc      EQ ipcFlute
           AND bf-reftable.code     EQ ipcScoreSet
           AND bf-reftable.code2    EQ ""
         NO-ERROR.
    IF AVAIL bf-reftable THEN
        DO iIndex = 1 TO EXTENT(opcScoreTypes):
            opcScoreTypes[iIndex] = SUBSTRING(bf-reftable.dscr, iIndex, 1).
        END.

    FIND FIRST bf-reftable NO-LOCK
         WHERE bf-reftable.reftable EQ "STYFLU"
           AND bf-reftable.company  EQ ipcStyle
           AND bf-reftable.loc      EQ ipcFlute
           AND bf-reftable.code     EQ ipcScoreSet
         NO-ERROR.
    IF AVAIL bf-reftable THEN
        DO iIndex = 1 TO 12: /* must be 12, total is in 13 */
            opdScores[iIndex] = bf-reftable.val[iIndex].
        END.

    FIND FIRST bf-reftable NO-LOCK
         WHERE bf-reftable.reftable EQ "STYFLU"
           AND bf-reftable.company  EQ ipcStyle
           AND bf-reftable.loc      EQ ipcFlute
           AND bf-reftable.code     EQ ipcScoreSet
           AND bf-reftable.code2    EQ "1"
         NO-ERROR.        
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

    DEFINE VARIABLE iChar         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCalc         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iOp           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dCalculations AS DECIMAL   NO-UNDO EXTENT 100.
    DEFINE VARIABLE cOperations   AS CHARACTER NO-UNDO EXTENT 100.
    DEFINE VARIABLE cCharacter    AS CHARACTER NO-UNDO FORMAT "X".
    DEFINE VARIABLE iNextOp       AS INTEGER   NO-UNDO.  
    DEFINE VARIABLE cConstant     AS CHARACTER NO-UNDO.
    
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
        bf-panelDetail.scoreType            = IF ipcScoreType EQ ? THEN "" ELSE ipcScoreType
        bf-panelDetail.panelSize            = ipdPanelSize
        bf-panelDetail.panelSizeFromFormula = ipdPanelSizeFromFormula
        .

    RELEASE bf-panelDetail.
END PROCEDURE.

PROCEDURE ConvertDecimalTo16ths:
/*------------------------------------------------------------------------------
 Purpose: Converts the size format from decimal to 16ths
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER iopdSize AS DECIMAL NO-UNDO.

    RUN SwitchPanelSizeFormat (
        INPUT  "Decimal",
        INPUT  "16th's",
        INPUT  iopdSize,
        OUTPUT iopdSize
        ).    
END PROCEDURE.

PROCEDURE Convert16thsToDecimal:
/*------------------------------------------------------------------------------
 Purpose: Converts the size format from decimal to 16ths
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER iopdSize AS DECIMAL NO-UNDO.

    RUN SwitchPanelSizeFormat (
        INPUT  "16th's",
        INPUT  "Decimal",
        INPUT  iopdSize,
        OUTPUT iopdSize
        ).    
END PROCEDURE.

PROCEDURE Convert32ndsToDecimal:
/*------------------------------------------------------------------------------
 Purpose: Converts the size format from decimal to 16ths
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER iopdSize AS DECIMAL NO-UNDO.

    RUN SwitchPanelSizeFormat (
        INPUT  "32nd's",
        INPUT  "Decimal",
        INPUT  iopdSize,
        OUTPUT iopdSize
        ).    
END PROCEDURE.

PROCEDURE pUpdatePanelDetailsPOLegacy PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Updates the legacy reftable based on Panel Details
     Notes: Once we deprecate the use of POLSCORE reftable, this procedure and all
     callers should be removed
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPoID    AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiPoLine  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER TABLE      FOR ttPanel.
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    
    FIND FIRST bf-po-ordl NO-LOCK 
        WHERE bf-po-ordl.company EQ ipcCompany
        AND bf-po-ordl.po-no EQ ipiPoID
        AND bf-po-ordl.line EQ ipiPoLine 
        NO-ERROR.
    IF AVAILABLE bf-po-ordl AND bf-po-ordl.spare-char-1 EQ "LENGTH" THEN
        RUN pUpdatePanelDetailsPOLegacyDetail(ipcCompany, ipiPOID, ipiPOLine, "L", TABLE ttPanel BY-REFERENCE). 
    ELSE 
        RUN pUpdatePanelDetailsPOLegacyDetail(ipcCompany, ipiPOID, ipiPOLine, "W", TABLE ttPanel BY-REFERENCE).
        
END PROCEDURE.

PROCEDURE pUpdatePanelDetailsPOLegacyDetail PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Purpose:  Updates the legacy reftable based on Panel Details
         Notes: Once we deprecate the use of POLSCORE reftable, this procedure and all
         callers should be removed
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPoID        AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiPoLine      AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcPanelType   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER TABLE      FOR ttPanel.
    
    DEFINE BUFFER bf-scoreReftable1 FOR reftable.
    DEFINE BUFFER bf-scoreReftable2 FOR reftable.
    
    DEFINE VARIABLE dScore AS DECIMAL NO-UNDO.
    
    FIND FIRST bf-scoreReftable1 EXCLUSIVE-LOCK
        WHERE bf-scoreReftable1.reftable EQ "POLSCORE"
          AND bf-scoreReftable1.company  EQ ipcCompany
          AND bf-scoreReftable1.loc      EQ "1"
          AND bf-scoreReftable1.code     EQ STRING(ipiPoID,"9999999999")
          AND bf-scoreReftable1.code2    EQ STRING(ipiPoLine, "9999999999")
        NO-ERROR.

    FIND FIRST bf-scoreReftable2 EXCLUSIVE-LOCK
        WHERE bf-scoreReftable2.reftable EQ "POLSCORE"
          AND bf-scoreReftable2.company  EQ ipcCompany
          AND bf-scoreReftable2.loc      EQ "2"
          AND bf-scoreReftable2.code     EQ STRING(ipiPoID,"9999999999")
          AND bf-scoreReftable2.code2    EQ STRING(ipiPoLine, "9999999999")
        NO-ERROR.

    IF AVAILABLE bf-scoreReftable1 THEN
        ASSIGN  //Clear out data arrays 
            bf-scoreReftable1.val  = 0
            bf-scoreReftable1.dscr = ""
            . 

    IF AVAILABLE bf-scoreReftable2 THEN
        ASSIGN  //Clear out data arrays 
            bf-scoreReftable2.val  = 0
            bf-scoreReftable2.dscr = ""
            . 

    FOR EACH ttPanel
        WHERE ttPanel.cPanelType EQ ipcPanelType  //W or L
        BY ttPanel.iPanelNum:
        dScore = ttPanel.dPanelSize.

        RUN ConvertDecimalTo16ths(INPUT-OUTPUT dScore).

        IF AVAILABLE bf-scoreReftable1 AND ttPanel.iPanelNum LE 12 THEN                                
            ASSIGN 
                bf-scoreReftable1.val[ttPanel.iPanelNum] = dScore
                bf-scoreReftable1.dscr                   = bf-scoreReftable1.dscr + (IF ttPanel.cScoreType EQ "" THEN " " ELSE IF ttPanel.cScoreType EQ ? THEN " " ELSE ttPanel.cScoreType)
                .                                    
        ELSE IF AVAILABLE bf-scoreReftable2 AND ttPanel.iPanelNum LE 20  THEN                                
            ASSIGN 
                bf-scoreReftable2.val[ttPanel.iPanelNum - 12] = dScore
                bf-scoreReftable2.dscr                        = bf-scoreReftable2.dscr + (IF ttPanel.cScoreType EQ "" THEN " " ELSE IF ttPanel.cScoreType EQ ? THEN " " ELSE ttPanel.cScoreType)
                .                                            
    END.

    RELEASE bf-scoreReftable1.
    RELEASE bf-scoreReftable2.

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

PROCEDURE pConvertIntoMetricSizeForttPanel PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Comverts the dimensions into Metric Numbers
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER TABLE                 FOR ttPanel.

    FOR EACH ttPanel:
        ttPanel.dPanelSize =  ROUND(ttPanel.dPanelSize * 25.4, 0).
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
    
    RUN pUpdatePanelDetails (
        INPUT  gcPanelLinkTypeEstimate,
        INPUT  ipcCompany,
        INPUT  0,                    /* Purchase Order */
        INPUT  0,                    /* Purchase Order Line */
        INPUT  ipcEstimateID,        /* Estimate ID */
        INPUT  ipiFormNo,            /* Form No */
        INPUT  ipiBlankNo,           /* Blank No */
        INPUT  "",                   /* Style ID */
        INPUT  "",                   /* Flute ID */
        INPUT  "",                   /* Score set Type */
        INPUT  TABLE ttPanel
        ).
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
    
    RUN pUpdatePanelDetails (
        INPUT  gcPanelLinkTypePO,
        INPUT  ipcCompany,
        INPUT  ipiPoID,              /* Purchase Order */
        INPUT  ipiPoLine,            /* Purchase Order Line */
        INPUT  "",                   /* Estimate ID */
        INPUT  ?,                    /* Form No */
        INPUT  ?,                    /* Blank No */
        INPUT  "",                   /* Style ID */
        INPUT  "",                   /* Flute ID */
        INPUT  "",                   /* Score set Type */
        INPUT  TABLE ttPanel
        ). 
    
    //Deprecate when POLSCORE Reftable is removed
    RUN pUpdatePanelDetailsPOLegacy(
        INPUT ipcCompany,
        INPUT ipiPOID,
        INPUT ipiPOLine,
        INPUT TABLE ttPanel BY-REFERENCE).
        
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
    
    RUN pUpdatePanelDetails (
        INPUT  gcPanelLinkTypeStyle,
        INPUT  ipcCompany,
        INPUT  0,                    /* Purchase Order */
        INPUT  0,                    /* Purchase Order Line */
        INPUT  "",                   /* Estimate ID */
        INPUT  0,                    /* Form No */
        INPUT  0,                    /* Blank No */
        INPUT  ipcStyleID,           /* Style ID */
        INPUT  ipcFluteID,           /* Flute ID */
        INPUT  ipcScoreSetType,      /* Score set Type */
        INPUT  TABLE ttPanel
        ).
END PROCEDURE.

PROCEDURE pUpdatePanelDetails PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Updates/Creates panelHeader and panelDetail records for a given Style
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcPanelLinkType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPoID          AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiPoLine        AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateID    AS CHARACTE  NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo        AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo       AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcStyleID       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFluteID       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcScoreSetType  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER TABLE            FOR ttPanel.
    
    DEFINE VARIABLE iPanelHeaderID AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-panelHeader FOR panelHeader.
    
    IF ipcPanelLinkType EQ gcPanelLinkTypeStyle THEN
        FIND FIRST bf-panelHeader NO-LOCK
             WHERE bf-panelHeader.company      EQ ipcCompany
               AND bf-panelHeader.linkType     EQ ipcPanelLinkType
               AND bf-panelHeader.styleID      EQ ipcStyleID
               AND bf-panelHeader.fluteID      EQ ipcFluteID
               AND bf-panelHeader.scoreSetType EQ ipcScoreSetType
             NO-ERROR.
    ELSE IF ipcPanelLinkType EQ gcPanelLinkTypePO THEN
        FIND FIRST bf-panelHeader EXCLUSIVE-LOCK
             WHERE bf-panelHeader.company  EQ ipcCompany
               AND bf-panelHeader.linkType EQ ipcPanelLinkType
               AND bf-panelHeader.poID     EQ ipiPoID
               AND bf-panelHeader.poLine   EQ ipiPoLine
             NO-ERROR.
    ELSE IF ipcPanelLinkType EQ gcPanelLinkTypeEstimate THEN
        FIND FIRST bf-panelHeader EXCLUSIVE-LOCK
             WHERE bf-panelHeader.company    EQ ipcCompany
               AND bf-panelHeader.linkType   EQ ipcPanelLinkType
               AND bf-panelHeader.estimateID EQ ipcEstimateID
               AND bf-panelHeader.formNo     EQ ipiFormNo
               AND bf-panelHeader.blankNo    EQ ipiBlankNo
             NO-ERROR.

    IF NOT AVAILABLE bf-panelHeader THEN DO:        
        RUN pCreatePanelHeader (
            INPUT  ipcCompany,
            INPUT  ipcPanelLinkType,
            INPUT  ipiPoID,              /* Purchase Order */
            INPUT  ipiPoLine,            /* Purchase Order Line */
            INPUT  ipcEstimateID,        /* Estimate ID */
            INPUT  ipiFormNo,            /* Form No */
            INPUT  ipiBlankNo,           /* Blank No */
            INPUT  ipcStyleID,           /* Style ID */
            INPUT  ipcFluteID,           /* Flute ID */
            INPUT  ipcScoreSetType,      /* Score set Type */
            OUTPUT iPanelHeaderID        
            ).
            
        FIND FIRST bf-panelHeader EXCLUSIVE-LOCK
             WHERE bf-panelHeader.panelHeaderID EQ iPanelHeaderID
             NO-ERROR.
    END.

    IF ipcPanelLinkType EQ gcPanelLinkTypeEstimate THEN
        ASSIGN
            bf-panelHeader.styleID      = IF ipcStyleID NE bf-panelHeader.styleID AND ipcStyleID NE "" THEN ipcStyleID ELSE bf-panelHeader.styleID
            bf-panelHeader.fluteID      = IF ipcFluteID NE bf-panelHeader.fluteID AND ipcFluteID NE "" THEN ipcFluteID ELSE bf-panelHeader.fluteID
            .    
    ELSE IF ipcPanelLinkType EQ gcPanelLinkTypePO THEN
        ASSIGN
            bf-panelHeader.estimateID   = IF ipcEstimateID NE bf-panelHeader.estimateID AND ipcEstimateID NE "" THEN ipcEstimateID ELSE bf-panelHeader.estimateID
            bf-panelHeader.formNo       = IF ipiFormNo NE ? THEN ipiFormNo ELSE bf-panelHeader.formNo
            bf-panelHeader.blankNo      = IF ipiBlankNo NE ? THEN ipiBlankNo ELSE bf-panelHeader.blankNo
            bf-panelHeader.styleID      = IF ipcStyleID NE "" THEN ipcStyleID ELSE bf-panelHeader.styleID
            bf-panelHeader.fluteID      = IF ipcFluteID NE bf-panelHeader.fluteID AND ipcFluteID NE "" THEN ipcFluteID ELSE bf-panelHeader.fluteID
            .    
    
    IF AVAILABLE bf-panelHeader THEN DO:
        RUN pDeletePanelDetail (
            INPUT bf-panelHeader.panelHeaderID
            ).        
        FOR EACH ttPanel
            BY ttPanel.iPanelNum:
            IF ttPanel.cPanelFormula EQ "" AND ttPanel.dScoringAllowance EQ 0 AND (ttPanel.cScoreType EQ "" OR ttPanel.cScoreType EQ ?) AND ttPanel.dPanelSize EQ 0 AND ttPanel.dPanelSizeFromFormula EQ 0 THEN
                NEXT.
                
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

