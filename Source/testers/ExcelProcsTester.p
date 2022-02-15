
/*------------------------------------------------------------------------
    File        : ExcelProcsTester.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Feb 01 18:59:17 EST 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE ghExcelProcs AS HANDLE NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RUN system\ExcelProcs.p PERSISTENT SET ghExcelProcs.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE(ghExcelProcs).

RUN pRunTest.

DELETE OBJECT ghExcelProcs.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pRunTest PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTemplate AS CHARACTER NO-UNDO INITIAL "c:\tmp\JobSummaryN.xlt".
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS LOGICAL NO-UNDO.
        
    RUN Excel_InitializeTemplate(cTemplate, OUTPUT lError, OUTPUT cMessage).
    RUN Excel_SetCellValue("C6","Hello World").
    RUN Excel_Cleanup.

END PROCEDURE.

