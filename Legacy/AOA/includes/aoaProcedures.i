&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : aoaProcecures.i
    Purpose     : shared procedures

    Syntax      : AOA/includes/aoaProcecures.i

    Description : Shared AOA/Jasper Procedures

    Author(s)   : Ron Stark
    Created     : 12.5.2018
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fJasperGroupCalc Include
FUNCTION fJasperGroupCalc RETURNS CHARACTER 
  (ipcField AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fJasperGroups Include
FUNCTION fJasperGroups RETURNS CHARACTER 
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateTempTableColumn Include 
PROCEDURE pCreateTempTableColumn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hTable AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx    AS INTEGER NO-UNDO.
    
    IF VALID-HANDLE(hAppSrv) THEN
    DO WITH FRAME frameColumns:
        hTable = DYNAMIC-FUNCTION('fGetTableHandle' IN hAppSrv, aoaProgramID).
        IF NOT VALID-HANDLE(hTable) THEN RETURN.
        
        EMPTY TEMP-TABLE ttColumn.
    
        hTable = hTable:DEFAULT-BUFFER-HANDLE.
        DO idx = 1 TO hTable:NUM-FIELDS:
            IF CAN-DO("RECID,ROWID",hTable:BUFFER-FIELD(idx):DATA-TYPE) THEN NEXT.
            IF hTable:BUFFER-FIELD(idx):NAME EQ "rowType"     THEN NEXT.
            IF hTable:BUFFER-FIELD(idx):NAME EQ "parameters"  THEN NEXT.
            IF hTable:BUFFER-FIELD(idx):NAME EQ "recDataType" THEN NEXT.
            CREATE ttColumn.
            ASSIGN
                ttColumn.ttField  = hTable:BUFFER-FIELD(idx):NAME
                ttColumn.ttOrder  = IF cSelectedColumns EQ "" THEN idx
                                     ELSE LOOKUP(ttColumn.ttField,cSelectedColumns)
                ttColumn.isActive = CAN-DO(cSelectedColumns,ttColumn.ttField) OR
                                    (cSelectedColumns EQ "" AND NOT ttColumn.ttField BEGINS "xx")
                ttColumn.ttLabel  = hTable:BUFFER-FIELD(idx):LABEL
                ttColumn.ttType   = hTable:BUFFER-FIELD(idx):DATA-TYPE
                ttColumn.ttFormat = hTable:BUFFER-FIELD(idx):FORMAT
                ttColumn.ttWidth  = hTable:BUFFER-FIELD(idx):WIDTH
                ttColumn.ttSize   = MAX(hTable:BUFFER-FIELD(idx):WIDTH,
                                  LENGTH(hTable:BUFFER-FIELD(idx):LABEL))
                .
            IF ttColumn.ttOrder EQ 0 THEN
            ASSIGN
                ttColumn.ttOrder  = 999
                ttColumn.isActive = NO
                .
        END. /* do idx */
        RUN pGetJasperUserPrint.
        RUN pSetGroupListItems.
        RUN pSetColumnOrder.
        {&OPEN-QUERY-ttColumn}
    END. /* valid happsrv */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetJasperUserPrint Include 
PROCEDURE pGetJasperUserPrint :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER ttColumn FOR ttColumn.
    
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    DEFINE VARIABLE jdx AS INTEGER NO-UNDO.
                    
    EMPTY TEMP-TABLE ttGroupCalc.
    
    RELEASE jasperUserPrint.
    /* get isgroup, ttgroup and ttcalctype values for user */
    IF lUseDefault EQ NO THEN
    FIND FIRST jasperUserPrint NO-LOCK
         WHERE jasperUserPrint.company    EQ user-print.company
           AND jasperUserPrint.program-id EQ user-print.program-id
           AND jasperUserPrint.user-id    EQ user-print.user-id
           AND jasperUserPrint.batch      EQ user-print.batch
           AND jasperUserPrint.batch-seq  EQ user-print.batch-seq
           AND jasperUserPrint.prgmName   EQ "Jasper"
         NO-ERROR.
    /* if no user found, get default values */
    IF NOT AVAILABLE jasperUserPrint THEN
    FIND FIRST jasperUserPrint NO-LOCK
         WHERE jasperUserPrint.company    EQ user-print.company
           AND jasperUserPrint.program-id EQ user-print.program-id
           AND jasperUserPrint.user-id    EQ "_default"
           AND jasperUserPrint.batch      EQ ""
           AND jasperUserPrint.prgmName   EQ "Jasper"
         NO-ERROR.
    IF AVAILABLE jasperUserPrint THEN    
    DO idx = 1 TO EXTENT(jasperUserPrint.field-name):
        IF jasperUserPrint.field-name[idx] EQ "" THEN LEAVE.
        FIND FIRST ttColumn
             WHERE ttColumn.ttField EQ jasperUserPrint.field-name[idx]
             NO-ERROR.
        IF NOT AVAILABLE ttColumn THEN NEXT.
        ttColumn.isGroup = jasperUserPrint.field-label[idx] EQ "yes".
        IF jasperUserPrint.field-value[idx] NE "" THEN DO:
            DO jdx = 1 TO NUM-ENTRIES(jasperUserPrint.field-value[idx]) BY 2:
                IF ENTRY(jdx,jasperUserPrint.field-value[idx]) EQ "Label" THEN DO:
                    ttColumn.ttGroupLabel = ENTRY(jdx + 1,jasperUserPrint.field-value[idx]).
                    NEXT.
                END. /* if label */
                CREATE ttGroupCalc.
                ASSIGN 
                    ttGroupCalc.ttField    = jasperUserPrint.field-name[idx]
                    ttGroupCalc.ttGroup    = ENTRY(jdx,jasperUserPrint.field-value[idx])
                    ttGroupCalc.ttCalcType = ENTRY(jdx + 1,jasperUserPrint.field-value[idx])
                    .
            END. /* do jdx */
            ttColumn.ttGroupCalc = fJasperGroupCalc(ttColumn.ttField).
        END. /* if field-value */
    END. /* do idx */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetGroupListItems Include 
PROCEDURE pSetGroupListItems :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cGroups AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER ttColumn FOR ttColumn.
    
    cGroups = fJasperGroups().
    /* check for invalid groups */
    FOR EACH ttGroupCalc
        WHERE LOOKUP(ttGroupCalc.ttGroup,cGroups) EQ 0
        :
        DELETE ttGroupCalc.
    END. /* each ttgroupcalc */
    FOR EACH ttColumn
        :
        ttColumn.ttGroupCalc = fJasperGroupCalc(ttColumn.ttField).
    END. /* each bttColumn*/
    &IF DEFINED(noBrowseRefresh) EQ 0 &THEN
    BROWSE ttColumn:REFRESH() NO-ERROR.
    &ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fJasperGroupCalc Include
FUNCTION fJasperGroupCalc RETURNS CHARACTER 
  (ipcField AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cGroupCalc AS CHARACTER NO-UNDO.
    
    FOR EACH ttGroupCalc
        WHERE ttGroupCalc.ttField EQ ipcField
        :
        IF ttGroupCalc.ttGroup NE "" THEN 
        cGroupCalc = cGroupCalc
                   + ttGroupCalc.ttGroup + ","
                   + ttGroupCalc.ttCalcType + ","
                   .
    END. /* each ttgroupcalc */
    RETURN TRIM(cGroupCalc,",").

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fJasperGroups Include
FUNCTION fJasperGroups RETURNS CHARACTER 
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cGroups AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER ttColumn FOR ttColumn.
    
    /* create list of groups */
    FOR EACH ttColumn
        WHERE ttColumn.isGroup  EQ YES
        :
        cGroups = cGroups + "[Group] " + ttColumn.ttLabel + ",".
    END. /* each bttColumn*/
    RETURN "Column," + cGroups + "Page,Report".

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



