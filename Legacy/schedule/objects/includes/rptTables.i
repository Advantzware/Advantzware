&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : rptTables.i
    Purpose     : 

    Syntax      : {{includes}/rptTables.i}

    Description : Report Table defs and procedures

    Author(s)   : Ron Stark
    Created     : 10.1.2005
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE alternateName AS CHARACTER NO-UNDO.
DEFINE VARIABLE excludeFormat AS LOGICAL NO-UNDO.
DEFINE VARIABLE layoutFormat AS CHARACTER NO-UNDO.
DEFINE VARIABLE selectedReport AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE rptFields NO-UNDO
  FIELD rptID AS CHARACTER
  FIELD fieldLabel AS CHARACTER
  FIELD fieldName AS CHARACTER
  FIELD fieldFormat AS CHARACTER
  FIELD filterField AS LOGICAL
  FIELD fieldGroup AS CHARACTER
    INDEX rptFields IS PRIMARY fieldName
    INDEX fieldLabel fieldLabel fieldName.

DEFINE TEMP-TABLE browseRptFields LIKE rptFields.

DEFINE TEMP-TABLE rptNames NO-UNDO
  FIELD rptID AS CHARACTER
  FIELD rptName AS CHARACTER
  FIELD rptTitle AS CHARACTER
  FIELD exclude AS LOGICAL
    INDEX rptNames IS PRIMARY UNIQUE rptName.

DEFINE TEMP-TABLE rptFormat NO-UNDO
  FIELD rptID AS CHARACTER
  FIELD rptName AS CHARACTER
  FIELD rptFormat AS CHARACTER
  FIELD exclude AS LOGICAL
  FIELD rptAltName AS CHARACTER
    INDEX rptFormat IS PRIMARY UNIQUE rptName rptFormat.

DEFINE TEMP-TABLE rptLayout NO-UNDO
  FIELD rptID AS CHARACTER
  FIELD rptName AS CHARACTER
  FIELD rptFormat AS CHARACTER
  FIELD fieldLabel AS CHARACTER
  FIELD fieldName AS CHARACTER
  FIELD rptLine AS INTEGER
  FIELD rptColumn AS INTEGER
  FIELD excelColumn AS INTEGER
  FIELD exclude AS LOGICAL
  FIELD rptAltName AS CHARACTER
    INDEX rptLayout IS PRIMARY rptName rptFormat fieldLabel fieldName.

{{&includes}/configVars.i}
{{&includes}/configVersion.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

DEFINE TEMP-TABLE ttblRptFields NO-UNDO LIKE rptFields.
CREATE ttblRptFields.
DEFINE TEMP-TABLE ttblRptNames NO-UNDO LIKE rptNames.
CREATE ttblRptNames.
DEFINE TEMP-TABLE ttblRptFormat NO-UNDO LIKE rptFormat.
CREATE ttblRptFormat.
DEFINE TEMP-TABLE ttblRptLayout NO-UNDO LIKE rptLayout.
CREATE ttblRptLayout.

RUN loadRptFields.
RUN loadRptNames.
RUN loadRptFormat.
RUN loadRptLayout.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadRptFields Include 
PROCEDURE loadRptFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE inputFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE inputColumns AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cLabel AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cName AS CHARACTER NO-UNDO.

  EMPTY TEMP-TABLE rptFields.
  EMPTY TEMP-TABLE browseRptFields.
  
  inputColumns = findProgram('{&data}/',ID,'/columns.dat').
  INPUT FROM VALUE(inputColumns) NO-ECHO.
  REPEAT:
    IMPORT ^ ^ cLabel cName.
    IF NOT cName BEGINS "udfField" THEN NEXT.
    IF cLabel EQ "Unused" THEN NEXT.
    CREATE browseRptFields.
    ASSIGN
        browseRptFields.fieldLabel = cLabel
        browseRptFields.fieldName  = cName
        browseRptFields.fieldFormat = FILL("X",20)
        .
    CREATE rptFields.
    BUFFER-COPY browseRptFields TO rptFields.
  END. /* repeat */
  INPUT CLOSE.
  
  inputFile = findProgram('{&data}/','','rptFields.dat').
  INPUT FROM VALUE(inputFile) NO-ECHO.
  REPEAT:
    IMPORT ttblRptFields.
    CREATE browseRptFields.
    BUFFER-COPY ttblRptFields TO browseRptFields.
    IF ttblRptFields.rptID EQ '' OR
       ID BEGINS ttblRptFields.rptID THEN DO:
      FIND FIRST rptFields EXCLUSIVE-LOCK
           WHERE rptFields.fieldName EQ ttblRptFields.fieldName
             AND ttblRptFields.fieldName NE 'calcTimeField'
           NO-ERROR.
      IF NOT AVAILABLE rptFields THEN
      CREATE rptFields.
      BUFFER-COPY ttblRptFields TO rptFields.
    END. /* if rptid */
  END. /* repeat */
  INPUT CLOSE.
  RUN setFilterField.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadRptFormat Include 
PROCEDURE loadRptFormat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE inputFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvID AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  EMPTY TEMP-TABLE rptFormat.
  DO i = 1 TO 2:
    ASSIGN
      lvID = IF i EQ 1 THEN '' ELSE ID
      inputFile = findProgram('{&data}/',lvID,'/rptFormat.dat')
      .
    INPUT FROM VALUE(inputFile) NO-ECHO.
    REPEAT:
      IMPORT ttblRptFormat.
      IF ttblRptFormat.rptID EQ '' OR
         ID BEGINS ttblRptFormat.rptID THEN DO:
        IF CAN-FIND(FIRST rptFormat
                    WHERE rptFormat.rptName EQ ttblRptFormat.rptName
                      AND rptFormat.rptFormat EQ ttblRptFormat.rptFormat) THEN
        NEXT.
        CREATE rptFormat.
        BUFFER-COPY ttblRptFormat TO rptFormat.
      END. /* if */
    END. /* repeat */
    INPUT CLOSE.
  END. /* do i */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadRptLayout Include 
PROCEDURE loadRptLayout :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE inputFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvID AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  EMPTY TEMP-TABLE rptLayout.
  DO i = 1 TO 2:
    ASSIGN
      lvID = IF i EQ 1 THEN '' ELSE ID
      inputFile = findProgram('{&data}/',lvID,'/rptLayout.dat')
      .
    INPUT FROM VALUE(inputFile) NO-ECHO.
    REPEAT:
      IMPORT ttblRptLayout.
      IF ttblRptLayout.rptID EQ '' OR
         ID BEGINS ttblRptLayout.rptID THEN DO:
        FIND FIRST rptLayout EXCLUSIVE-LOCK
             WHERE rptLayout.rptName EQ ttblRptLayout.rptName
               AND rptLayout.rptFormat EQ ttblRptLayout.rptFormat
               AND rptLayout.fieldLabel EQ ttblRptLayout.fieldLabel
               AND rptLayout.fieldName EQ ttblRptLayout.fieldName
             NO-ERROR.
        IF NOT AVAILABLE rptLayout THEN DO:
          CREATE rptLayout.
          BUFFER-COPY ttblRptLayout TO rptLayout.
        END. /* not avail */
        ELSE
        ASSIGN
          rptLayout.rptLine = ttblRptLayout.rptLine
          rptLayout.rptColumn = ttblRptLayout.rptColumn
          rptLayout.excelColumn = ttblRptLayout.excelColumn
          .
      END. /* if */
    END. /* repeat */
    INPUT CLOSE.
  END. /* do i */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadRptNames Include 
PROCEDURE loadRptNames :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE inputFile AS CHARACTER NO-UNDO.

  EMPTY TEMP-TABLE rptNames.
  inputFile = findProgram('{&data}/','','rptNames.dat').
  INPUT FROM VALUE(inputFile) NO-ECHO.
  REPEAT:
    IMPORT ttblRptNames.
    IF ttblRptNames.rptID EQ '' OR
       ID BEGINS ttblRptNames.rptID THEN DO:
      CREATE rptNames.
      BUFFER-COPY ttblRptNames TO rptNames.
    END. /* if */
  END. /* repeat */
  INPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveRptFields Include 
PROCEDURE saveRptFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  OUTPUT TO VALUE(staticDat + '{&data}/rptFields.dat').
  FOR EACH browseRptFields
      BY browseRptFields.rptID BY browseRptFields.fieldName
      :
    EXPORT browseRptFields.
  END.
  OUTPUT CLOSE.
  OS-COPY VALUE(staticDat + '{&data}/rptFields.dat')
          VALUE(staticDat + '{&data}/rptFields.sav').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveRptFormat Include 
PROCEDURE saveRptFormat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  OUTPUT TO VALUE(clientDat + '{&data}/' + ID + '/rptFormat.dat').
  FOR EACH rptFormat
      WHERE rptFormat.rptID EQ ID
      :
    EXPORT rptFormat.
  END. /* each rptFormat */
  OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveRptLayout Include 
PROCEDURE saveRptLayout :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH rptFormat
      WHERE rptFormat.rptID EQ ID
      :
    DELETE rptFormat.
  END. /* each rptformat */
  OUTPUT TO VALUE(clientDat + '{&data}/' + ID + '/rptLayout.dat').
  FOR EACH rptLayout
      WHERE rptLayout.rptID EQ ID
        AND (rptLayout.rptLine NE 0
         OR rptLayout.rptColumn NE 0
         OR rptLayout.excelColumn NE 0)
      BREAK BY rptLayout.rptName BY rptLayout.rptFormat
      :
    &IF DEFINED(designMode) NE 0 &THEN
    IF rptLayout.rptName EQ selectedReport AND
       rptLayout.rptFormat EQ layoutFormat THEN
    ASSIGN
      rptLayout.exclude = excludeFormat
      rptLayout.rptAltName = alternateName
      .
    &ENDIF
    IF FIRST-OF(rptLayout.rptFormat) THEN DO:
      IF NOT CAN-FIND(FIRST rptFormat
                      WHERE rptFormat.rptName EQ rptLayout.rptName
                        AND rptFormat.rptFormat EQ rptLayout.rptFormat) THEN DO:
        CREATE rptFormat.
        ASSIGN
          rptFormat.rptID = rptLayout.rptID
          rptFormat.rptName = rptLayout.rptName
          rptFormat.rptFormat = rptLayout.rptFormat
          rptFormat.exclude = rptLayout.exclude
          rptFormat.rptAltName = rptLayout.rptAltName
          .
      END. /* if not can-find */
    END. /* if first-of */
    EXPORT rptLayout.
  END. /* each rptLayout */
  OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFilterField Include 
PROCEDURE setFilterField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF SEARCH(staticDat + '{&data}/rptFields.sav') EQ ? THEN RETURN.
  INPUT FROM VALUE(staticDat + '{&data}/rptFields.sav') NO-ECHO.
  REPEAT:
    IMPORT ttblRptFields.
    FIND FIRST browseRptFields
         WHERE browseRptFields.rptID EQ ttblRptFields.rptID
           AND browseRptFields.fieldLabel EQ ttblRptFields.fieldLabel
           AND browseRptFields.fieldName EQ ttblRptFields.fieldName
         NO-ERROR.
    IF AVAILABLE browseRptFields THEN
    browseRptFields.filterField = ttblRptFields.filterField.
    FIND FIRST rptFields
         WHERE rptFields.rptID EQ ttblRptFields.rptID
           AND rptFields.fieldLabel EQ ttblRptFields.fieldLabel
           AND rptFields.fieldName EQ ttblRptFields.fieldName
         NO-ERROR.
    IF AVAILABLE rptFields THEN
    rptFields.filterField = ttblRptFields.filterField.
  END. /* repeat */
  INPUT CLOSE.
  RUN saveRptFields.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

