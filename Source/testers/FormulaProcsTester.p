
/*------------------------------------------------------------------------
    File        : FormulaProcsTester.p
    Purpose     : 

    Syntax      :

    Description : Tester for FormulaProcs		

    Author(s)   : BV
    Created     : Sun Dec 22 12:28:29 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE hdFormulaProcs AS HANDLE NO-UNDO.

{system\FormulaProcs.i}
DEFINE VARIABLE cFormula AS CHARACTER NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

RUN system\FormulaProcs.p PERSISTENT SET hdFormulaProcs.

//RUN testCalculatePanels.
RUN testParseDesignScores.

PROCEDURE testCalculatePanels:
    
    cFormula = ".5W+D+(W+.25)+D+.5W+S".
    EMPTY TEMP-TABLE ttPanel.
    RUN ParsePanels IN hdFormulaProcs (INPUT cFormula, INPUT "W2Up", OUTPUT TABLE ttPanel).
    FIND FIRST eb NO-LOCK 
        WHERE eb.company EQ '001'
        AND eb.est-no EQ '   13959'
        NO-ERROR.
    RUN CalculatePanels IN hdFormulaProcs (ROWID(eb), INPUT-OUTPUT TABLE ttPanel).
    FOR EACH ttPanel:
        DISPLAY ttPanel.
    END.

END.

PROCEDURE testParseDesignScores:
    
    DEFINE VARIABLE iprEBID             AS ROWID     NO-UNDO .
    DEFINE VARIABLE iprbox-design-hdrID AS ROWID     NO-UNDO.
    DEFINE VARIABLE cCompany            AS CHARACTER NO-UNDO INIT "001".
    DEFINE VARIABLE cEstimateNo         AS CHARACTER NO-UNDO INIT "  103243".
    DEFINE VARIABLE iFormNo             AS INTEGER   NO-UNDO INIT 1.
    DEFINE VARIABLE iBlankNo            AS INTEGER   NO-UNDO INIT 1.
    
    DEFINE BUFFER bf-EB FOR eb.
    DEFINE BUFFER bf-Ef FOR ef.
    
    FIND FIRST bf-EB NO-LOCK
        WHERE bf-EB.company = cCompany
        AND bf-EB.est-no = cEstimateNo
        AND bf-EB.form-no = iFormNo
        AND bf-EB.blank-no = iBlankNo NO-ERROR.
        
    IF NOT AVAILABLE bf-EB THEN
        RETURN.
          
    FIND FIRST style NO-LOCK 
        WHERE style.style EQ bf-EB.style NO-ERROR.
            
    IF AVAILABLE style THEN
    DO:
        FIND FIRST box-design-hdr NO-LOCK
            WHERE box-design-hdr.design-no = style.designIDAlt 
            AND box-design-hdr.company = style.company NO-ERROR.
       
        IF AVAILABLE box-design-hdr THEN
            RUN Formula_ParseDesignScores IN hdFormulaProcs (
                INPUT bf-eb.company,
                INPUT bf-eb.est-no,
                INPUT bf-eb.form-no,
                INPUT bf-eb.blank-no,
                INPUT style.designIDAlt,
                INPUT NO,
                OUTPUT TABLE ttScoreLine
                ).
                
                
            FOR EACH ttScoreLine:
                
               disp ttScoreLine.
                
            END.
    END.
END.                

