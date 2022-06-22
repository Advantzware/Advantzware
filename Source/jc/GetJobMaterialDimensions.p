
/*------------------------------------------------------------------------
    File        : jc/GetJobMaterialDimensions.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed May 25 19:48:59 IST 2022
    Notes       : Code copy of rm/pol-dims.i
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE INPUT        PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER ipcJobNo        AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER ipiJobNo2       AS INTEGER   NO-UNDO.
DEFINE INPUT        PARAMETER ipiFormNo       AS INTEGER   NO-UNDO.
DEFINE INPUT        PARAMETER ipcItemID       AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopdLength      AS DECIMAL   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopdWidth       AS DECIMAL   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopdDepth       AS DECIMAL   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopdBasisWeight AS DECIMAL   NO-UNDO.

DEFINE BUFFER bf-job     FOR job.
DEFINE BUFFER bf-job-mat FOR job-mat.
DEFINE BUFFER bf-item    FOR item.

FIND FIRST bf-item NO-LOCK
     WHERE bf-item.company EQ ipcCompany
       AND bf-item.i-no    EQ ipcItemID
     NO-ERROR.
IF NOT AVAILABLE bf-item THEN 
    RETURN.
    
IF (iopdLength EQ 0 OR iopdWidth EQ 0 OR iopdBasisWeight EQ 0) THEN DO:
    FIND FIRST bf-job NO-LOCK
         WHERE bf-job.company EQ ipcCompany
           AND bf-job.job-no  EQ ipcJobNo
           AND bf-job.job-no2 EQ ipiJobNo2
         NO-ERROR.

    IF AVAILABLE bf-job THEN DO :
        FOR EACH bf-job-mat NO-LOCK
            WHERE bf-job-mat.company EQ bf-job.company
              AND bf-job-mat.job     EQ bf-job.job
              AND bf-job-mat.job-no  EQ bf-job.job-no
              AND bf-job-mat.job-no2 EQ bf-job.job-no2
              AND bf-job-mat.i-no    EQ ipcItemID
            BY bf-job-mat.frm DESCENDING:
              
            IF bf-job-mat.frm EQ ipiFormNo THEN 
                LEAVE.
        END.
          
        IF AVAILABLE bf-job-mat THEN
            ASSIGN
                iopdLength      = IF iopdLength EQ 0 THEN bf-job-mat.len ELSE iopdLength
                iopdWidth       = IF iopdWidth EQ 0 THEN bf-job-mat.wid ELSE iopdWidth
                iopdBasisWeight = IF iopdBasisWeight EQ 0 THEN bf-job-mat.basis-w ELSE iopdBasisWeight
                .
    END.

    IF iopdLength EQ 0 THEN iopdLength = bf-item.s-len.

    IF iopdWidth EQ 0 THEN
        iopdWidth = IF bf-item.r-wid NE 0 THEN bf-item.r-wid ELSE bf-item.s-wid.

    IF iopdBasisWeight EQ 0 THEN iopdBasisWeight = bf-item.basis-w.
END.
