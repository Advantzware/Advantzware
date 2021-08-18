
/*------------------------------------------------------------------------
    File        : EstimateScoresFix.p
    Purpose     : 

    Syntax      :

    Description : Fixes the overriden scores in estimate to panel tables			

    Author(s)   : DEVA$!
    Created     : Tue Aug 17 09:10:00 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
{system/FormulaProcs.i}
   
DEFINE VARIABLE hdFormulaProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE iIndex         AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCtr           AS INTEGER NO-UNDO.

DEFINE BUFFER bf-eb          FOR eb.
DEFINE BUFFER bf-style       FOR style.
DEFINE BUFFER bf-po-ordl     FOR po-ordl.
DEFINE BUFFER bf-panelHeader FOR panelHeader.

RUN system/FormulaProcs.p PERSISTENT SET hdFormulaProcs.

FOR EACH company NO-LOCK:
    
    FOR EACH bf-eb NO-LOCK WHERE 
        bf-eb.company EQ company.company:   

        FIND FIRST bf-style NO-LOCK
             WHERE bf-style.company EQ bf-eb.company
               AND bf-style.style   EQ bf-eb.style
             NO-ERROR.                 
        IF AVAILABLE bf-style AND bf-style.type EQ "B" AND bf-style.formula[20] EQ "" THEN DO:
            EMPTY TEMP-TABLE ttPanel.
        
            DO iIndex = 1 TO EXTENT(bf-eb.k-wid-array2):
                CREATE ttPanel.
                ASSIGN
                    ttPanel.iPanelNum  = iIndex
                    ttPanel.cPanelType = "W"
                    ttPanel.cScoreType = bf-eb.k-wid-scr-type2[iIndex]
                    ttPanel.dPanelSize = bf-eb.k-wid-array2[iIndex]
                    .
            END.
        
            DO iIndex = 1 TO EXTENT(bf-eb.k-len-array2):
                CREATE ttPanel.
                ASSIGN
                    ttPanel.iPanelNum  = iIndex
                    ttPanel.cPanelType = "L"
                    ttPanel.cScoreType = bf-eb.k-len-scr-type2[iIndex]
                    ttPanel.dPanelSize = bf-eb.k-len-array2[iIndex]
                    .
            END.
        
            RUN UpdatePanelDetailsForEstimate IN hdFormulaProcs (
                INPUT bf-eb.company,
                INPUT bf-eb.est-no,
                INPUT bf-eb.form-no,
                INPUT bf-eb.blank-no,
                INPUT TABLE ttPanel
                ).    
        END.     
    END.

    FOR EACH bf-po-ordl NO-LOCK
        WHERE bf-po-ordl.company   EQ company.company
          AND bf-po-ordl.opened    EQ TRUE
          AND bf-po-ordl.item-type EQ TRUE,
        FIRST bf-panelHeader NO-LOCK
        WHERE bf-panelHeader.company  EQ bf-po-ordl.company
          AND bf-panelHeader.linkType EQ "P"
          AND bf-panelHeader.poID     EQ bf-po-ordl.po-no
          AND bf-panelHeader.poLine   EQ bf-po-ordl.line,
        FIRST bf-style NO-LOCK
        WHERE bf-style.company EQ bf-panelHeader.company
          AND bf-style.style   EQ bf-panelHeader.styleID
          AND bf-style.type    NE "B":
        RUN DeletePanelDetailsForPO IN hdFormulaProcs (
            INPUT bf-po-ordl.company,
            INPUT bf-po-ordl.po-no,
            INPUT bf-po-ordl.line
            ).
    END.    
END.

DELETE PROCEDURE hdFormulaProcs.
