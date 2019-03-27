/* **********************  Internal Procedures  *********************** */

PROCEDURE pUpdateCadOnCorrugated:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipRowid     AS ROWID NO-UNDO.
    DEFINE INPUT  PARAMETER iplAddExt   AS LOGICAL NO-UNDO.
    DEFINE INPUT  PARAMETER iplBoxImg   AS LOGICAL NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFileName AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFilePath AS CHARACTER NO-UNDO.
    
    FIND FIRST est NO-LOCK
                WHERE ROWID(est) EQ ipRowid NO-ERROR .

    IF AVAILABLE est  THEN DO:
        FOR EACH eb EXCLUSIVE-LOCK
            WHERE eb.company EQ est.company 
            AND eb.est-no EQ est.est-no :

            IF eb.cad-no EQ "" THEN
                eb.cad-no = ipcFileName .

            IF iplAddExt AND eb.cad-no NE "" THEN DO:
                IF INDEX(eb.cad-no,".ard") EQ 0 THEN
                    ASSIGN eb.cad-no = eb.cad-no + ".ard" .
            END.

            IF iplBoxImg THEN DO:
                 FIND FIRST box-design-hdr WHERE box-design-hdr.design-no = 0 AND
                     box-design-hdr.company = eb.company 
                     AND box-design-hdr.est-no = eb.est-no     
                     AND box-design-hdr.form-no   EQ eb.form-no
                     AND box-design-hdr.blank-no  EQ eb.blank-no NO-ERROR.

                 IF AVAILABLE box-design-hdr THEN DO:
                     ASSIGN 
                         box-design-hdr.box-image = ipcFilePath . /*".jpg"*/.
                 END.
            END.
        END.
    END.
    RELEASE eb .

END PROCEDURE.
