&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: userTasks.w

  Description: from SMART.W - Template for basic SmartObject

  Author: Ron Stark
  Created: 1.14.2019

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

&Scoped-define prgmName userCols.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle  AS HANDLE    NO-UNDO.
DEFINE VARIABLE rRowID   AS ROWID     NO-UNDO.

DEFINE TEMP-TABLE ttSubjectColumn NO-UNDO LIKE dynSubjectColumn.

{AOA/tempTable/ttGroupCalc.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME subjectColumnBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttSubjectColumn

/* Definitions for BROWSE subjectColumnBrowse                           */
&Scoped-define FIELDS-IN-QUERY-subjectColumnBrowse ttSubjectColumn.sortOrder ttSubjectColumn.isActive ttSubjectColumn.fieldLabel ttSubjectColumn.sortCol ttSubjectColumn.sortDescending ttSubjectColumn.isGroup ttSubjectColumn.groupLabel ttSubjectColumn.fieldName ttSubjectColumn.isCalcField ttSubjectColumn.fieldFormat ttSubjectColumn.calcProc ttSubjectColumn.calcParam ttSubjectColumn.groupCalc   
&Scoped-define ENABLED-FIELDS-IN-QUERY-subjectColumnBrowse ttSubjectColumn.isActive ttSubjectColumn.fieldLabel ttSubjectColumn.sortCol ttSubjectColumn.sortDescending ttSubjectColumn.isGroup ttSubjectColumn.groupLabel   
&Scoped-define ENABLED-TABLES-IN-QUERY-subjectColumnBrowse ttSubjectColumn
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-subjectColumnBrowse ttSubjectColumn
&Scoped-define SELF-NAME subjectColumnBrowse
&Scoped-define QUERY-STRING-subjectColumnBrowse FOR EACH ttSubjectColumn WHERE ttSubjectColumn.subjectID EQ dynSubject.subjectID BY ttSubjectColumn.sortOrder
&Scoped-define OPEN-QUERY-subjectColumnBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttSubjectColumn WHERE ttSubjectColumn.subjectID EQ dynSubject.subjectID BY ttSubjectColumn.sortOrder.
&Scoped-define TABLES-IN-QUERY-subjectColumnBrowse ttSubjectColumn
&Scoped-define FIRST-TABLE-IN-QUERY-subjectColumnBrowse ttSubjectColumn


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnGroupCalc btnMoveDown btnMoveUp ~
subjectColumnBrowse btnSave 

/* Custom List Definitions                                              */
/* columnObjects,columnPanel,List-3,List-4,List-5,List-6                */
&Scoped-define columnObjects btnGroupCalc btnMoveDown btnMoveUp 
&Scoped-define columnPanel btnGroupCalc btnMoveDown btnMoveUp 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fSetSaveButton s-object 
FUNCTION fSetSaveButton RETURNS LOGICAL
  (iplSave AS LOGICAL) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnGroupCalc 
     IMAGE-UP FILE "Graphics/16x16/spreadsheet_sum.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Group Calculations".

DEFINE BUTTON btnMoveDown 
     IMAGE-UP FILE "Graphics/16x16/navigate_down.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Move Down".

DEFINE BUTTON btnMoveUp 
     IMAGE-UP FILE "Graphics/16x16/navigate_up.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Move Up".

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "Graphics/16x16/floppy_disk.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Save".

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY subjectColumnBrowse FOR 
      ttSubjectColumn SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE subjectColumnBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS subjectColumnBrowse s-object _FREEFORM
  QUERY subjectColumnBrowse DISPLAY
      ttSubjectColumn.sortOrder
ttSubjectColumn.isActive VIEW-AS TOGGLE-BOX
ttSubjectColumn.fieldLabel
ttSubjectColumn.sortCol
ttSubjectColumn.sortDescending VIEW-AS TOGGLE-BOX
ttSubjectColumn.isGroup VIEW-AS TOGGLE-BOX
ttSubjectColumn.groupLabel
ttSubjectColumn.fieldName
ttSubjectColumn.isCalcField VIEW-AS TOGGLE-BOX
ttSubjectColumn.fieldFormat
ttSubjectColumn.isStatusField VIEW-AS TOGGLE-BOX
ttSubjectColumn.statusCompare
ttSubjectColumn.compareValue
ttSubjectColumn.calcProc
ttSubjectColumn.calcParam
ttSubjectColumn.groupCalc
ttSubjectColumn.calcFormula
ENABLE
ttSubjectColumn.isActive
ttSubjectColumn.fieldLabel
ttSubjectColumn.sortCol
ttSubjectColumn.sortDescending
ttSubjectColumn.isGroup
ttSubjectColumn.groupLabel
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 151 BY 26.95
         TITLE "Columns".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnGroupCalc AT ROW 6.24 COL 2 HELP
          "Group Calculations" WIDGET-ID 272
     btnMoveDown AT ROW 5.05 COL 2 HELP
          "Move Down" WIDGET-ID 62
     btnMoveUp AT ROW 3.86 COL 2 HELP
          "Move Up" WIDGET-ID 64
     subjectColumnBrowse AT ROW 1 COL 7 WIDGET-ID 200
     btnSave AT ROW 7.43 COL 2 HELP
          "Save" WIDGET-ID 274
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic,Browse
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW s-object ASSIGN
         HEIGHT             = 27.48
         WIDTH              = 160.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB s-object 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW s-object
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB subjectColumnBrowse 1 F-Main */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE
       FRAME F-Main:HEIGHT           = 26.95
       FRAME F-Main:WIDTH            = 158.

/* SETTINGS FOR BUTTON btnGroupCalc IN FRAME F-Main
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnMoveDown IN FRAME F-Main
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnMoveUp IN FRAME F-Main
   1 2                                                                  */
ASSIGN 
       btnSave:HIDDEN IN FRAME F-Main           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE subjectColumnBrowse
/* Query rebuild information for BROWSE subjectColumnBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttSubjectColumn
WHERE ttSubjectColumn.subjectID EQ dynSubject.subjectID
BY ttSubjectColumn.sortOrder.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE subjectColumnBrowse */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btnGroupCalc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGroupCalc s-object
ON CHOOSE OF btnGroupCalc IN FRAME F-Main
DO:
    IF AVAILABLE ttSubjectColumn THEN
    RUN pJasperGroupCalc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveDown s-object
ON CHOOSE OF btnMoveDown IN FRAME F-Main
DO:
    RUN pMove (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveUp s-object
ON CHOOSE OF btnMoveUp IN FRAME F-Main
DO:
    RUN pMove (-1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave s-object
ON CHOOSE OF btnSave IN FRAME F-Main
DO:
    RUN pSave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME subjectColumnBrowse
&Scoped-define SELF-NAME subjectColumnBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL subjectColumnBrowse s-object
ON DEFAULT-ACTION OF subjectColumnBrowse IN FRAME F-Main /* Columns */
DO:
    APPLY "CHOOSE":U TO btnGroupCalc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL subjectColumnBrowse s-object
ON ROW-LEAVE OF subjectColumnBrowse IN FRAME F-Main /* Columns */
DO:
    IF dynParamValue.user-id NE "_default" THEN
    fSetSaveButton (BROWSE subjectColumnBrowse:MODIFIED).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK s-object 


/* ***************************  Main Block  *************************** */

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{AOA/includes/pJasperGroupCalc.i "dyn"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI s-object  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize s-object 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view s-object 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN pUserColumns.
  fSetSaveButton (NO).
  FRAME {&FRAME-NAME}:MOVE-TO-TOP().
  IF AVAILABLE dynParamValue AND dynParamValue.user-id EQ "_default" THEN
  HIDE {&columnPanel} btnSave IN FRAME {&FRAME-NAME}.
  ELSE
  VIEW {&columnPanel} IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSettings s-object 
PROCEDURE pGetSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMove s-object 
PROCEDURE pMove :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiMove AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE iCurrent   AS INTEGER NO-UNDO.
    DEFINE VARIABLE iMoveTo    AS INTEGER NO-UNDO.
    DEFINE VARIABLE iSubjectID AS INTEGER NO-UNDO.
    DEFINE VARIABLE rRowID     AS ROWID   NO-UNDO.
    
    DEFINE BUFFER bttSubjectColumn FOR ttSubjectColumn.
    
    IF NOT AVAILABLE dynSubject THEN RETURN.
    iSubjectID = dynSubject.subjectID.
    {AOA/includes/pMove.i "ttSubjectColumn" "subjectColumnBrowse"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSave s-object 
PROCEDURE pSave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    DEFINE BUFFER ttSubjectColumn FOR ttSubjectColumn.
    
    DO TRANSACTION:
        FOR EACH dynValueColumn EXCLUSIVE-LOCK
            WHERE dynValueColumn.subjectID    EQ dynParamValue.subjectID
              AND dynValueColumn.user-id      EQ dynParamValue.user-id
              AND dynValueColumn.prgmName     EQ dynParamValue.prgmName
              AND dynValueColumn.paramValueID EQ dynParamValue.paramValueID
               BY dynValueColumn.sortOrder
            :
            DELETE dynValueColumn.
        END. /* each dynvaluecolumn */
        FOR EACH ttSubjectColumn
              BY ttSubjectColumn.sortOrder
            :
            CREATE dynValueColumn.
            ASSIGN
                dynValueColumn.subjectID      = dynParamValue.subjectID
                dynValueColumn.user-id        = dynParamValue.user-id
                dynValueColumn.prgmName       = dynParamValue.prgmName
                dynValueColumn.paramValueID   = dynParamValue.paramValueID
                dynValueColumn.sortOrder      = ttSubjectColumn.sortOrder
                dynValueColumn.calcFormula    = ttSubjectColumn.calcFormula
                dynValueColumn.calcParam      = ttSubjectColumn.calcParam
                dynValueColumn.calcProc       = ttSubjectColumn.calcProc
                dynValueColumn.cellColor      = ttSubjectColumn.cellColor
                dynValueColumn.colFormat      = ttSubjectColumn.fieldFormat
                dynValueColumn.colLabel       = ttSubjectColumn.fieldLabel
                dynValueColumn.colName        = ttSubjectColumn.fieldName
                dynValueColumn.columnSize     = ttSubjectColumn.columnSize
                dynValueColumn.compareValue   = ttSubjectColumn.compareValue
                dynValueColumn.dataType       = ttSubjectColumn.dataType
                dynValueColumn.groupCalc      = ttSubjectColumn.groupCalc
                dynValueColumn.groupLabel     = ttSubjectColumn.groupLabel
                dynValueColumn.isActive       = ttSubjectColumn.isActive
                dynValueColumn.isCalcField    = ttSubjectColumn.isCalcField
                dynValueColumn.isGroup        = ttSubjectColumn.isGroup
                dynValueColumn.isReturnValue  = ttSubjectColumn.isReturnValue
                dynValueColumn.isSearchable   = ttSubjectColumn.isSearchable
                dynValueColumn.isSortable     = ttSubjectColumn.isSortable
                dynValueColumn.isStatusField  = ttSubjectColumn.isStatusField
                dynValueColumn.sortCol        = ttSubjectColumn.sortCol
                dynValueColumn.sortDescending = ttSubjectColumn.sortDescending
                dynValueColumn.statusAction   = ttSubjectColumn.statusAction
                dynValueColumn.statusCompare  = ttSubjectColumn.statusCompare
                dynValueColumn.textColor      = ttSubjectColumn.textColor
                .
        END. /* each ttSubjectColumn */

/*        /* rstark - remove when depricated */                                     */
/*        FIND CURRENT dynParamValue EXCLUSIVE-LOCK.                                */
/*        ASSIGN                                                                    */
/*            dynParamValue.isActive       = NO                                     */
/*            dynParamValue.colName        = ""                                     */
/*            dynParamValue.colLabel       = ""                                     */
/*            dynParamValue.colFormat      = ""                                     */
/*            dynParamValue.columnSize     = 0                                      */
/*            dynParamValue.dataType       = ""                                     */
/*            dynParamValue.sortCol        = 0                                      */
/*            dynParamValue.sortDescending = NO                                     */
/*            dynParamValue.isGroup        = NO                                     */
/*            dynParamValue.isReturnValue  = NO                                     */
/*            dynParamValue.isSearchable   = NO                                     */
/*            dynParamValue.isSortable     = NO                                     */
/*            dynParamValue.groupLabel     = ""                                     */
/*            dynParamValue.groupCalc      = ""                                     */
/*            dynParamValue.isCalcField    = NO                                     */
/*            dynParamValue.calcProc       = ""                                     */
/*            dynParamValue.calcParam      = ""                                     */
/*            dynParamValue.calcFormula    = ""                                     */
/*            .                                                                     */
/*        FOR EACH ttSubjectColumn                                                  */
/*              BY ttSubjectColumn.sortOrder                                        */
/*            :                                                                     */
/*            ASSIGN                                                                */
/*                idx                               = ttSubjectColumn.sortOrder     */
/*                dynParamValue.isActive[idx]       = ttSubjectColumn.isActive      */
/*                dynParamValue.colName[idx]        = ttSubjectColumn.fieldName     */
/*                dynParamValue.colLabel[idx]       = ttSubjectColumn.fieldLabel    */
/*                dynParamValue.colFormat[idx]      = ttSubjectColumn.fieldFormat   */
/*                dynParamValue.columnSize[idx]     = ttSubjectColumn.columnSize    */
/*                dynParamValue.dataType[idx]       = ttSubjectColumn.dataType      */
/*                dynParamValue.sortCol[idx]        = ttSubjectColumn.sortCol       */
/*                dynParamValue.sortDescending[idx] = ttSubjectColumn.sortDescending*/
/*                dynParamValue.isGroup[idx]        = ttSubjectColumn.isGroup       */
/*                dynParamValue.isReturnValue[idx]  = ttSubjectColumn.isReturnValue */
/*                dynParamValue.isSearchable[idx]   = ttSubjectColumn.isSearchable  */
/*                dynParamValue.isSortable[idx]     = ttSubjectColumn.isSortable    */
/*                dynParamValue.groupLabel[idx]     = ttSubjectColumn.groupLabel    */
/*                dynParamValue.groupCalc[idx]      = ttSubjectColumn.groupCalc     */
/*                dynParamValue.isCalcField[idx]    = ttSubjectColumn.isCalcField   */
/*                dynParamValue.calcProc[idx]       = ttSubjectColumn.calcProc      */
/*                dynParamValue.calcParam[idx]      = ttSubjectColumn.calcParam     */
/*                dynParamValue.calcFormula[idx]    = ttSubjectColumn.calcFormula   */
/*                .                                                                 */
/*        END. /* each ttSubjectColumn */                                           */
/*        FIND CURRENT dynParamValue NO-LOCK.                                       */
    END. /* do trans */
    BROWSE subjectColumnBrowse:MODIFIED = NO.
    fSetSaveButton (NO).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUserColumns s-object 
PROCEDURE pUserColumns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    DEFINE VARIABLE jdx AS INTEGER NO-UNDO.

    EMPTY TEMP-TABLE ttSubjectColumn.
    {&OPEN-QUERY-subjectColumnBrowse}
    {methods/run_link.i "CONTAINER" "pGetParamValueRowID" "(OUTPUT rRowID)"}
    IF rRowID EQ ? THEN RETURN.
    FIND FIRST dynParamValue NO-LOCK WHERE ROWID(dynParamValue) EQ rRowID.
    FIND FIRST dynSubject NO-LOCK
         WHERE dynSubject.subjectID EQ dynParamValue.subject
         NO-ERROR.
    IF NOT AVAILABLE dynSubject THEN RETURN.
    FOR EACH dynValueColumn NO-LOCK
        WHERE dynValueColumn.subjectID    EQ dynParamValue.subjectID
          AND dynValueColumn.user-id      EQ dynParamValue.user-id
          AND dynValueColumn.prgmName     EQ dynParamValue.prgmName
          AND dynValueColumn.paramValueID EQ dynParamValue.paramValueID
           BY dynValueColumn.sortOrder
        :
        CREATE ttSubjectColumn.
        ASSIGN
            ttSubjectColumn.subjectID      = dynValueColumn.subjectID     
            ttSubjectColumn.sortOrder      = dynValueColumn.sortOrder     
            ttSubjectColumn.calcFormula    = dynValueColumn.calcFormula   
            ttSubjectColumn.calcParam      = dynValueColumn.calcParam     
            ttSubjectColumn.calcProc       = dynValueColumn.calcProc
            ttSubjectColumn.cellColor      = dynValueColumn.cellColor      
            ttSubjectColumn.columnSize     = dynValueColumn.columnSize    
            ttSubjectColumn.compareValue   = dynValueColumn.compareValue
            ttSubjectColumn.dataType       = dynValueColumn.dataType      
            ttSubjectColumn.fieldFormat    = dynValueColumn.colFormat     
            ttSubjectColumn.fieldLabel     = dynValueColumn.colLabel      
            ttSubjectColumn.fieldName      = dynValueColumn.colName       
            ttSubjectColumn.groupCalc      = dynValueColumn.groupCalc     
            ttSubjectColumn.groupLabel     = dynValueColumn.groupLabel    
            ttSubjectColumn.isActive       = dynValueColumn.isActive      
            ttSubjectColumn.isCalcField    = dynValueColumn.isCalcField   
            ttSubjectColumn.isGroup        = dynValueColumn.isGroup       
            ttSubjectColumn.isReturnValue  = dynValueColumn.isReturnValue 
            ttSubjectColumn.isSearchable   = dynValueColumn.isSearchable  
            ttSubjectColumn.isSortable     = dynValueColumn.isSortable    
            ttSubjectColumn.isStatusField  = dynValueColumn.isStatusField
            ttSubjectColumn.sortCol        = dynValueColumn.sortCol       
            ttSubjectColumn.sortDescending = dynValueColumn.sortDescending
            ttSubjectColumn.statusAction   = dynValueColumn.statusAction
            ttSubjectColumn.statusCompare  = dynValueColumn.statusCompare
            ttSubjectColumn.textColor      = dynValueColumn.textColor
            .
        IF ttSubjectColumn.groupCalc NE "" THEN
        DO jdx = 1 TO NUM-ENTRIES(ttSubjectColumn.groupCalc) BY 2:
            CREATE ttGroupCalc.
            ASSIGN
                ttGroupCalc.subjectID = ttSubjectColumn.subjectID
                ttGroupCalc.fieldName = ttSubjectColumn.fieldName
                ttGroupCalc.groupName = ENTRY(jdx,ttSubjectColumn.groupCalc)
                ttGroupCalc.calcType  = ENTRY(jdx + 1,ttSubjectColumn.groupCalc)
                .
        END. /* do idx */
    END. /* each dynvaluecolumn */
/*    /* rstark - remove when depricated */                                       */
/*    DO idx = 1 TO EXTENT(dynParamValue.colName):                                */
/*        IF dynParamValue.colName[idx] EQ "" THEN LEAVE.                         */
/*        CREATE ttSubjectColumn.                                                 */
/*        ASSIGN                                                                  */
/*            ttSubjectColumn.subjectID      = dynParamValue.subjectID            */
/*            ttSubjectColumn.sortOrder      = idx                                */
/*            ttSubjectColumn.isActive       = dynParamValue.isActive[idx]        */
/*            ttSubjectColumn.fieldName      = dynParamValue.colName[idx]         */
/*            ttSubjectColumn.fieldLabel     = dynParamValue.colLabel[idx]        */
/*            ttSubjectColumn.fieldFormat    = dynParamValue.colFormat[idx]       */
/*            ttSubjectColumn.dataType       = dynParamValue.dataType[idx]        */
/*            ttSubjectColumn.sortCol        = dynParamValue.sortCol[idx]         */
/*            ttSubjectColumn.sortDescending = dynParamValue.sortDescending[idx]  */
/*            ttSubjectColumn.isGroup        = dynParamValue.isGroup[idx]         */
/*            ttSubjectColumn.groupLabel     = dynParamValue.groupLabel[idx]      */
/*            ttSubjectColumn.groupCalc      = dynParamValue.groupCalc[idx]       */
/*            ttSubjectColumn.isCalcField    = dynParamValue.isCalcField[idx]     */
/*            ttSubjectColumn.calcProc       = dynParamValue.calcProc[idx]        */
/*            ttSubjectColumn.calcParam      = dynParamValue.calcParam[idx]       */
/*            ttSubjectColumn.calcFormula    = dynParamValue.calcFormula[idx]     */
/*            .                                                                   */
/*        IF ttSubjectColumn.groupCalc NE "" THEN                                 */
/*        DO jdx = 1 TO NUM-ENTRIES(ttSubjectColumn.groupCalc) BY 2:              */
/*            CREATE ttGroupCalc.                                                 */
/*            ASSIGN                                                              */
/*                ttGroupCalc.subjectID = ttSubjectColumn.subjectID               */
/*                ttGroupCalc.fieldName = ttSubjectColumn.fieldName               */
/*                ttGroupCalc.groupName = ENTRY(jdx,ttSubjectColumn.groupCalc)    */
/*                ttGroupCalc.calcType  = ENTRY(jdx + 1,ttSubjectColumn.groupCalc)*/
/*                .                                                               */
/*        END. /* do idx */                                                       */
/*    END. /* do idx */                                                           */
    {&OPEN-QUERY-subjectColumnBrowse}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWinReSize s-object 
PROCEDURE pWinReSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdHeight AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdWidth  AS DECIMAL NO-UNDO.
    
    HIDE FRAME {&FRAME-NAME}.
    ASSIGN
        FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = ipdHeight
        FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = ipdWidth
        FRAME {&FRAME-NAME}:HEIGHT         = ipdHeight
        FRAME {&FRAME-NAME}:WIDTH          = ipdWidth
        BROWSE subjectColumnBrowse:HEIGHT  = ipdHeight
        BROWSE subjectColumnBrowse:WIDTH   = ipdWidth - BROWSE subjectColumnBrowse:COL + 1
        .
    VIEW FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed s-object 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     Receive and process 'state-changed' methods
               (issued by 'new-state' event).
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
  END CASE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fSetSaveButton s-object 
FUNCTION fSetSaveButton RETURNS LOGICAL
  (iplSave AS LOGICAL):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    IF AVAILABLE dynParamValue AND dynParamValue.user-id EQ "_default" THEN
    RETURN FALSE.
    
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            btnSave:HIDDEN    = NOT iplSave
            btnSave:SENSITIVE = iplSave
            .
    END. /* with frame */
    RETURN iplSave.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

