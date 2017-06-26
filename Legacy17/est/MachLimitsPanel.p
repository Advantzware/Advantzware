
/*------------------------------------------------------------------------
    File        : MachLimitsPanel.p
    Purpose     : Specific Logic for Panel Limits on Machines

    Syntax      : Run est\MachLimitsPanel.p (ROWID(style), ROWID(mach), ROWID(xeb), OUTPUT tt-mach-exc.reason).

    Description : Returns a reason character for all failures. 
                  Blank return value indicates valid sizes
                  **Deprecates cec\mach-pan.i**
                  

    Author(s)   : BV
    Created     : Tue May 09 14:17:27 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipriStyle AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipriMach AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipriEb AS ROWID NO-UNDO.
DEFINE OUTPUT PARAMETER opcReason AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-eb    FOR eb.
DEFINE BUFFER bf-style FOR style.
DEFINE BUFFER bf-mach  FOR mach.

DEFINE TEMP-TABLE ttPanels
    FIELD cPanelType    AS CHARACTER
    FIELD iPanelNum     AS INTEGER 
    FIELD cPanelFormula AS CHARACTER 
    .

DEFINE VARIABLE dDimensionToTest AS DECIMAL NO-UNDO.
DEFINE VARIABLE dLW              AS DECIMAL NO-UNDO.
DEFINE VARIABLE lAddedL          AS LOGICAL NO-UNDO.
DEFINE VARIABLE lAddedW          AS LOGICAL NO-UNDO.
 

/* ********************  Preprocessor Definitions  ******************** */


/* ************************  Function Prototypes ********************** */


FUNCTION fTestPanels RETURNS LOGICAL 
    (ipcCompany AS CHARACTER) FORWARD.


/* ***************************  Main Block  *************************** */

FIND bf-eb NO-LOCK WHERE ROWID(bf-eb) EQ ipriEb NO-ERROR.
FIND bf-mach NO-LOCK WHERE ROWID(bf-mach) EQ ipriMach NO-ERROR.
FIND bf-style NO-LOCK WHERE ROWID(bf-style) EQ ipriStyle NO-ERROR.

IF AVAILABLE bf-eb AND AVAILABLE bf-mach AND AVAILABLE bf-style THEN 
DO:
    IF fTestPanels(bf-style.company) AND bf-style.type EQ 'B' THEN 
    DO:
        EMPTY TEMP-TABLE ttPanels.
        RUN pParsePanels(bf-style.formula[2],'L'). /*(Style "L" formula eg. L+W+L+W+J+S for RSC)*/
        RUN pParsePanels(bf-style.formula[1],'W'). /*(Style "W" forumla eg. .5W+D+.5W+S for RSC*/
        
        ASSIGN 
            dDimensionToTest = 0
            dLW              = 0
            lAddedW = NO
            lAddedL = NO
            .
        
        /*For each Length Panel*/
        FOR EACH ttPanels 
            WHERE ttPanels.cPanelType EQ 'L'
            AND ttPanels.cPanelFormula NE 'J'
            AND ttPanels.cPanelFormula NE 'S'
            BY ttPanels.iPanelNum:
            
            dDimensionToTest = bf-eb.k-len-array2[ttPanels.iPanelNum].  /*Panel on the Blank Length*/
            
            IF dDimensionToTest NE 0 THEN 
            DO:
                /*Any non-joint or non-score panel outside Head to Head panel on Length*/
                IF dDimensionToTest LT bf-mach.min_hd_hd THEN 
                    opcReason = opcReason + 'Panel: ' + STRING(ttPanels.iPanelNum) + ' Size: ' + STRING(dDimensionToTest) + ' Under Min Panel (Hd-Hd) of ' + STRING(bf-mach.min_hd_hd) + ', '.
                IF dDimensionToTest GT bf-mach.max_hd_hd AND bf-mach.max_hd_hd GT 0 THEN 
                    opcReason = opcReason + 'Panel: ' + STRING(ttPanels.iPanelNum) + ' Size: ' + STRING(dDimensionToTest) + ' Over Max Panel (Hd-Hd) of ' + STRING(bf-mach.max_hd_hd) + ', '.             
                IF opcReason NE '' THEN LEAVE.
                
                IF INDEX(ttPanels.cPanelFormula, 'L') GT 0 THEN 
                DO:
                    IF dDimensionToTest LT bf-mach.min-pan-l THEN 
                        opcReason = opcReason + 'Panel: ' + STRING(ttPanels.iPanelNum) + ' Size: ' + STRING(dDimensionToTest) + ' Under Min Score L of ' + STRING(bf-mach.min-pan-l) + ', '.
                    IF dDimensionToTest GT bf-mach.max-pan-l AND bf-mach.max-pan-l GT 0 THEN 
                        opcReason = opcReason + 'Panel: ' + STRING(ttPanels.iPanelNum) + ' Size: ' + STRING(dDimensionToTest) + ' Over Max Score L of ' + STRING(bf-mach.max-pan-l) + ', '.
                    
                    /*Build L+W value to check limits*/
                    IF NOT lAddedL THEN 
                        ASSIGN
                            lAddedL = YES
                            dLW     = dLW + dDimensionToTest.                      
                END.
                
                IF INDEX(ttPanels.cPanelFormula, 'W') GT 0 THEN 
                DO:
                    IF dDimensionToTest LT bf-mach.min-pan-w THEN 
                        opcReason = opcReason + 'Panel: ' + STRING(ttPanels.iPanelNum) + ' Size: ' + STRING(dDimensionToTest) + ' Under Min Score W of ' + STRING(bf-mach.min-pan-w) + ', '.
                    IF dDimensionToTest GT bf-mach.max-pan-w AND bf-mach.max-pan-w GT 0 THEN 
                        opcReason = opcReason + 'Panel: ' + STRING(ttPanels.iPanelNum) + ' Size: ' + STRING(dDimensionToTest) + ' Over Max Score W of ' + STRING(bf-mach.max-pan-w) + ', '.
                    
                    /*Build L+W value to check limits*/
                    IF NOT lAddedW THEN 
                        ASSIGN
                            lAddedW = YES
                            dLW     = dLW + dDimensionToTest. 
                END.
                
                IF lAddedW AND lAddedL AND dLW GT 0 THEN 
                DO:
                    IF dLW LT bf-mach.min_pan_lw THEN 
                        opcReason = opcReason + STRING(dLW) + ' Under Min Score L + W of ' + STRING(bf-mach.min_pan_lw) + ', '.
                    IF dLW GT bf-mach.max_pan_lw AND bf-mach.max_pan_lw GT 0 THEN 
                        opcReason = opcReason + STRING(dLW) + ' Over Max Score L + W of ' + STRING(bf-mach.max_pan_lw) + ', '.
                    
                    /*Reset so to not re-report*/
                    ASSIGN 
                        lAddedW = NO
                        lAddedL = NO
                        dLW = 0
                        . 
                END.
  
            END. /*dDimension to Test not 0*/    
        END. /*for each Length Panel*/
                
        dDimensionToTest = 0.
        /*For each Width Panel (first 2 panels only)*/
        FOR EACH ttPanels 
            WHERE ttPanels.cPanelType EQ 'W'
            AND ttPanels.cPanelFormula NE 'J'
            AND ttPanels.cPanelFormula NE 'S'
            AND ttPanels.iPanelNum LE 2
            BY ttPanels.iPanelNum:
            dDimensionToTest = dDimensionToTest + bf-eb.k-wid-array2[ttPanels.iPanelNum].
        END.
        IF dDimensionToTest NE 0 THEN 
        DO:
            IF dDimensionToTest LT bf-mach.min_slot_score THEN 
                opcReason = opcReason + STRING(dDimensionToTest) + ' Under Min Slot/Score Panel of ' + STRING(bf-mach.min_slot_score) + ', '.
            IF dDimensionToTest GT bf-mach.max_slot_score AND bf-mach.max_slot_score GT 0 THEN 
                opcReason = opcReason + STRING(dDimensionToTest) + ' Over Max Slot/Score Panel of ' + STRING(bf-mach.max_slot_score) + ', '.
        END.
    END. /*NK1CEPanels is yes and the Style is a Box*/    
END. /*Key records are all available*/ 

IF opcReason NE '' THEN opcReason = TRIM(opcReason,', ').               


/* **********************  Internal Procedures  *********************** */

PROCEDURE pParsePanels:
    /*------------------------------------------------------------------------------
     Purpose: Given a Style Formula, this will produce a simple temp table for each panel in the forumula
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFormula AS CHARACTER.
    DEFINE INPUT PARAMETER ipcPanelType AS CHARACTER.

    DEFINE VARIABLE iPanel        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iChar         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCharNextPlus AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cChar         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCharPrev     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCharNext     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPanelFormula AS CHARACTER NO-UNDO.

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
            CREATE ttPanels.
            ASSIGN 
                ttPanels.cPanelType    = ipcPanelType
                ttPanels.iPanelNum     = iPanel
                ttPanels.cPanelFormula = cChar
                .
        END.
        ELSE IF cChar NE '+' THEN 
            DO:  
                FIND FIRST ttPanels
                    WHERE ttPanels.cPanelType EQ ipcPanelType
                    AND ttPanels.iPanelNum EQ iPanel NO-ERROR.
                IF AVAILABLE ttPanels THEN
                    ttPanels.cPanelFormula = ttPanels.cPanelFormula + cChar.
            END. /*valid panel character*/
        iChar = iChar + 1.
    END. /*loop through each character in the formula*/ 
    
    
END PROCEDURE.

/* ************************  Function Implementations ***************** */


FUNCTION fTestPanels RETURNS LOGICAL 
    (ipcCompany AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:  Tests NK1 CEPANEL Logical Value
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE lResult AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO. 
    
    RUN sys\ref\nk1look.p (ipcCompany,
        'CEPANEL',
        'L',
        NO,
        NO,
        '',
        '', 
        OUTPUT cReturn,
        OUTPUT lFound).
    lResult = lFound AND cReturn EQ 'YES'.
    
    RETURN lResult.
		
END FUNCTION.


