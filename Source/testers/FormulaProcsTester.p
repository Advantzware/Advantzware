
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
