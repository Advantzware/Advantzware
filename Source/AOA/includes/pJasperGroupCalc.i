/* pJasperGroupCalc.i - rstark - 2.18.2019 */

FUNCTION fJasperFields RETURNS CHARACTER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFields AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bttSubjectColumn FOR ttSubjectColumn.
        
    FOR EACH bttSubjectColumn
        WHERE bttSubjectColumn.subjectID EQ dynSubject.subjectID
        :
        cFields = cFields + "$F~{" + bttSubjectColumn.fieldName + "},".
    END. /* each ttSubjectColumn*/
    RETURN TRIM(cFields,",").

END FUNCTION.

FUNCTION fJasperGroupCalc RETURNS CHARACTER
  (ipcField AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cGroupCalc AS CHARACTER NO-UNDO.
    
    FOR EACH ttGroupCalc
        WHERE ttGroupCalc.subjectID EQ dynSubject.subjectID
          AND ttGroupCalc.fieldName EQ ipcField
        :
        IF ttGroupCalc.groupName NE "" THEN 
        cGroupCalc = cGroupCalc
                   + ttGroupCalc.groupName + ","
                   + ttGroupCalc.calcType + ","
                   .
    END. /* each ttgroupcalc */
    RETURN TRIM(cGroupCalc,",").

END FUNCTION.

FUNCTION fJasperGroups RETURNS CHARACTER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cGroups AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER ttSubjectColumn FOR ttSubjectColumn.
    
    /* create list of groups */
    FOR EACH ttSubjectColumn
        WHERE ttSubjectColumn.subjectID EQ dynSubject.subjectID
          AND ttSubjectColumn.isGroup   EQ YES
        :
        cGroups = cGroups + "[Group] " + ttSubjectColumn.fieldLabel + ",".
    END. /* each bttSubjectColumn*/
    RETURN "Column," + cGroups + "Page,Report".

END FUNCTION.

FUNCTION fJasperVariables RETURNS CHARACTER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cVariables  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResetGroup AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cName       AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bttSubjectColumn FOR {1}SubjectColumn.
        
    FOR EACH bttSubjectColumn
        WHERE bttSubjectColumn.subjectID EQ dynSubject.subjectID
          AND bttSubjectColumn.groupCalc NE "",
        EACH ttGroupCalc
        WHERE ttGroupCalc.subjectID EQ bttSubjectColumn.subjectID
          AND ttGroupCalc.fieldName EQ bttSubjectColumn.fieldName
        :
        ASSIGN
            cResetGroup = REPLACE(REPLACE(ttGroupCalc.groupName,"[Group] ","")," ","_") + "_Group"
            cName       = ttGroupCalc.fieldName + "_"
                        + IF ttGroupCalc.groupName BEGINS "[Group] " THEN cResetGroup
                          ELSE ttGroupCalc.groupName + "Footer" 
            cVariables  = cVariables + "$V~{" + cName + "},".
                        .
    END. /* each bttSubjectColumn */
    RETURN TRIM (cVariables,",").

END FUNCTION.

PROCEDURE pJasperGroupCalc:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cGroupCalc AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSave      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bttSubjectColumn FOR ttSubjectColumn.
    
    IF NOT AVAILABLE ttSubjectColumn THEN RETURN.

    FOR EACH ttGroupCalc
        WHERE ttGroupCalc.subjectID EQ ttSubjectColumn.subjectID
          AND ttGroupCalc.fieldName EQ ttSubjectColumn.fieldName
        :
        cGroupCalc = cGroupCalc
                   + ttGroupCalc.groupName + ","
                   + ttGroupCalc.calcType + ","
                   .
    END. /* each ttgroupcalc */
    cGroupCalc = TRIM(cGroupCalc,",").
    RUN AOA/jasperGroupCalc.w (
        ttSubjectColumn.fieldLabel,
        ttSubjectColumn.fieldName,
        fJasperGroups(),
        fJasperFields(),
        fJasperVariables(),
        INPUT-OUTPUT cGroupCalc,
        OUTPUT lSave
        ).
    IF lSave THEN DO:
        &IF "{&prgmName}" EQ "userCols." &THEN
        RUN pUpdateMode (YES).
        &ELSE
        fSetSaveButton (YES).
        &ENDIF
        FOR EACH ttGroupCalc
            WHERE ttGroupCalc.subjectID EQ ttSubjectColumn.subjectID
              AND ttGroupCalc.fieldName EQ ttSubjectColumn.fieldName
            :
            DELETE ttGroupCalc.
        END. /* each ttgroupcalc */
        IF cGroupCalc NE "" THEN
        DO idx = 1 TO NUM-ENTRIES(cGroupCalc) BY 2:
            CREATE ttGroupCalc.
            ASSIGN
                ttGroupCalc.subjectID = ttSubjectColumn.subjectID
                ttGroupCalc.fieldName = ttSubjectColumn.fieldName
                ttGroupCalc.groupName = ENTRY(idx,cGroupCalc)
                ttGroupCalc.calcType  = ENTRY(idx + 1,cGroupCalc)
                .
        END. /* do idx */
        ttSubjectColumn.groupCalc = fJasperGroupCalc(ttSubjectColumn.fieldName).
        BROWSE subjectColumnBrowse:REFRESH() NO-ERROR.
    END. /* if lsave */

END PROCEDURE.
