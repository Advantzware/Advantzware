&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/DateRules.w - RStark - 6.30.2020

  Description: from VIEWER.W - Template for SmartViewer Objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE hTable        AS HANDLE  NO-UNDO.
DEFINE VARIABLE hField        AS HANDLE  NO-UNDO.
DEFINE VARIABLE iHourMax      AS INTEGER NO-UNDO.
DEFINE VARIABLE iHourMin      AS INTEGER NO-UNDO.

ASSIGN
    iHourMax = DYNAMIC-FUNCTION("sfCommon_HourMax")
    iHourMin = DYNAMIC-FUNCTION("sfCommon_HourMin")
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES DateRules
&Scoped-define FIRST-EXTERNAL-TABLE DateRules


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR DateRules.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS dateRules.scope dateRules.scopeID ~
dateRules.baseTable dateRules.baseField dateRules.days ~
dateRules.resultTable dateRules.resultField 
&Scoped-define ENABLED-TABLES dateRules
&Scoped-define FIRST-ENABLED-TABLE dateRules
&Scoped-Define DISPLAYED-FIELDS dateRules.dateRuleID dateRules.scope ~
dateRules.scopeID dateRules.baseTable dateRules.baseField dateRules.days ~
dateRules.resultTable dateRules.resultField 
&Scoped-define DISPLAYED-TABLES dateRules
&Scoped-define FIRST-DISPLAYED-TABLE dateRules
&Scoped-Define DISPLAYED-OBJECTS dbTables dbFields lSunday lMonday lTuesday ~
lWednesday lThursday lFriday lSaturday lHoliday skipHour skipMinute ~
skipampm 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,SKIP-TIME,F1 */
&Scoped-define ADM-ASSIGN-FIELDS dateRules.dateRuleID 
&Scoped-define SKIP-TIME skipHour skipMinute skipampm 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE skipampm AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS COMBO-BOX INNER-LINES 2
     LIST-ITEMS "PM","AM" 
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE skipHour AS CHARACTER FORMAT "X(2)":U 
     LABEL "Skip After Time" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE skipMinute AS CHARACTER FORMAT "X(2)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 156 BY 18.57.

DEFINE VARIABLE dbFields AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 46 BY 17.38
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE dbTables AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 47 BY 17.38
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE lFriday AS LOGICAL INITIAL no 
     LABEL "Friday" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .81 NO-UNDO.

DEFINE VARIABLE lHoliday AS LOGICAL INITIAL no 
     LABEL "Holiday" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE lMonday AS LOGICAL INITIAL no 
     LABEL "Monday" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE lSaturday AS LOGICAL INITIAL no 
     LABEL "Saturday" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .81 NO-UNDO.

DEFINE VARIABLE lSunday AS LOGICAL INITIAL no 
     LABEL "Sunday" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE lThursday AS LOGICAL INITIAL no 
     LABEL "Thursday" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .81 NO-UNDO.

DEFINE VARIABLE lTuesday AS LOGICAL INITIAL no 
     LABEL "Tuesday" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .81 NO-UNDO.

DEFINE VARIABLE lWednesday AS LOGICAL INITIAL no 
     LABEL "Wednesday" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     dateRules.dateRuleID AT ROW 1.24 COL 25 COLON-ALIGNED WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 
     dbTables AT ROW 1.95 COL 62 NO-LABEL WIDGET-ID 56
     dbFields AT ROW 1.95 COL 110 NO-LABEL WIDGET-ID 58
     dateRules.scope AT ROW 2.43 COL 25 COLON-ALIGNED WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
          BGCOLOR 15 
     dateRules.scopeID AT ROW 3.62 COL 25 COLON-ALIGNED WIDGET-ID 48
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
          BGCOLOR 15 
     dateRules.baseTable AT ROW 4.81 COL 25 COLON-ALIGNED WIDGET-ID 38
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
          BGCOLOR 15 
     dateRules.baseField AT ROW 6 COL 25 COLON-ALIGNED WIDGET-ID 36
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
          BGCOLOR 15 
     dateRules.days AT ROW 7.19 COL 25 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
          BGCOLOR 15 
     lSunday AT ROW 8.38 COL 27 WIDGET-ID 64
     lMonday AT ROW 9.33 COL 27 WIDGET-ID 66
     lTuesday AT ROW 10.29 COL 27 WIDGET-ID 68
     lWednesday AT ROW 11.24 COL 27 WIDGET-ID 70
     lThursday AT ROW 12.19 COL 27 WIDGET-ID 72
     lFriday AT ROW 13.14 COL 27 WIDGET-ID 74
     lSaturday AT ROW 14.1 COL 27 WIDGET-ID 76
     lHoliday AT ROW 15.05 COL 27 WIDGET-ID 78
     skipHour AT ROW 16 COL 25 COLON-ALIGNED HELP
          "Enter Starting Hour" WIDGET-ID 92
     skipMinute AT ROW 16 COL 31 COLON-ALIGNED HELP
          "Enter Starting Minute" WIDGET-ID 94
     skipampm AT ROW 16 COL 36 COLON-ALIGNED NO-LABEL WIDGET-ID 90
     dateRules.resultTable AT ROW 17.19 COL 25 COLON-ALIGNED WIDGET-ID 44
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
          BGCOLOR 15 
     dateRules.resultField AT ROW 18.38 COL 25 COLON-ALIGNED WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
          BGCOLOR 15 
     "Days to Skip:" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 8.38 COL 13 WIDGET-ID 80
     "Tables (double-click to select)" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 1.24 COL 62 WIDGET-ID 60
     "Fields (double-click to select)" VIEW-AS TEXT
          SIZE 28 BY .62 AT ROW 1.24 COL 110 WIDGET-ID 62
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FGCOLOR 1 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.DateRules
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 18.57
         WIDTH              = 156.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       dateRules.baseField:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       dateRules.baseTable:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN dateRules.dateRuleID IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR SELECTION-LIST dbFields IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR SELECTION-LIST dbTables IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX lFriday IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX lHoliday IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX lMonday IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX lSaturday IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX lSunday IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX lThursday IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX lTuesday IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX lWednesday IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       dateRules.resultField:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       dateRules.resultTable:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR COMBO-BOX skipampm IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN skipHour IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN skipMinute IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME dateRules.baseField
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dateRules.baseField V-table-Win
ON ENTRY OF dateRules.baseField IN FRAME F-Main /* Base Field */
DO:
    ASSIGN
        hTable = dateRules.baseTable:HANDLE
        hField = dateRules.baseField:HANDLE
        .
    RUN pSetTableField.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dateRules.baseTable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dateRules.baseTable V-table-Win
ON ENTRY OF dateRules.baseTable IN FRAME F-Main /* Base Table */
DO:
    ASSIGN
        hTable = dateRules.baseTable:HANDLE
        hField = dateRules.baseField:HANDLE
        .
    RUN pSetTableField.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dbFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dbFields V-table-Win
ON DEFAULT-ACTION OF dbFields IN FRAME F-Main
DO:
    hField:SCREEN-VALUE = {&SELF-NAME}:SCREEN-VALUE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dbTables
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dbTables V-table-Win
ON DEFAULT-ACTION OF dbTables IN FRAME F-Main
DO:
    hTable:SCREEN-VALUE = {&SELF-NAME}:SCREEN-VALUE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dbTables V-table-Win
ON VALUE-CHANGED OF dbTables IN FRAME F-Main
DO:
    RUN pGetDBFields.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dateRules.resultField
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dateRules.resultField V-table-Win
ON ENTRY OF dateRules.resultField IN FRAME F-Main /* Result Field */
DO:
    ASSIGN
        hTable = dateRules.resultTable:HANDLE
        hField = dateRules.resultField:HANDLE
        .
    RUN pSetTableField.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dateRules.resultTable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dateRules.resultTable V-table-Win
ON ENTRY OF dateRules.resultTable IN FRAME F-Main /* Result Table */
DO:
    ASSIGN
        hTable = dateRules.resultTable:HANDLE
        hField = dateRules.resultField:HANDLE
        .
    RUN pSetTableField.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME skipHour
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL skipHour V-table-Win
ON LEAVE OF skipHour IN FRAME F-Main /* Skip After Time */
DO:
    correct-error = INTEGER(SELF:SCREEN-VALUE) LT iHourMin OR
                    INTEGER(SELF:SCREEN-VALUE) GT iHourMax.
    {methods/entryerr.i &error-message="Invalid Hour Entered"}
/*    ASSIGN {&SELF-NAME}.                                 */
/*    IF {&SELF-NAME} GT 24 THEN DO:                       */
/*        MESSAGE                                          */
/*            "Invalid Hour, please enter between 0 and 24"*/
/*        VIEW-AS ALERT-BOX ERROR.                         */
/*        RETURN NO-APPLY.                                 */
/*    END.                                                 */
/*    IF {&SELF-NAME} EQ 24 THEN                           */
/*    iSkipMinute:SCREEN-VALUE = "0".                      */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME skipMinute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL skipMinute V-table-Win
ON LEAVE OF skipMinute IN FRAME F-Main
DO:
    correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR
                    INTEGER(SELF:SCREEN-VALUE) GT 59.
    {methods/entryerr.i &error-message="Invalid Minute Entered"}
/*    ASSIGN {&SELF-NAME}.                                   */
/*    IF {&SELF-NAME} GT 59 THEN DO:                         */
/*        MESSAGE                                            */
/*            "Invalid Minute, please enter between 0 and 59"*/
/*        VIEW-AS ALERT-BOX ERROR.                           */
/*        RETURN NO-APPLY.                                   */
/*    END.                                                   */
/*    IF iSkipHour EQ 24 THEN                                */
/*    {&SELF-NAME}:SCREEN-VALUE = "0".                       */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "DateRules"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "DateRules"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iDateRuleID AS INTEGER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  DO WHILE TRUE WITH FRAME {&FRAME-NAME}:
      IF DateRules.dateRuleID:SCREEN-VALUE NE "0" THEN
      LEAVE.
      iDateRuleID = NEXT-VALUE(DateRules_seq).
      IF CAN-FIND(FIRST DateRules
                  WHERE DateRules.dateRuleID EQ iDateRuleID) THEN
      NEXT.
      DateRules.dateRuleID:SCREEN-VALUE = STRING(iDateRuleID).
      LEAVE.
  END. /* do while */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
      skipHour skipMinute skipampm lSunday lMonday lTuesday lWednesday lThursday lFriday lSaturday lHoliday
      skipHour = STRING(INTEGER(skipHour) + (IF iHourMax EQ 12 AND skipampm EQ "PM" AND INTEGER(skipHour) LT 12 THEN 12 ELSE 0)) 
      DateRules.skipTime = INTEGER(skipHour) * 3600 + INTEGER(skipMinute) * 60
      DateRules.skipDays = STRING(lSunday,"Y/N")
                         + STRING(lMonday,"Y/N")
                         + STRING(lTuesday,"Y/N")
                         + STRING(lWednesday,"Y/N")
                         + STRING(lThursday,"Y/N")
                         + STRING(lFriday,"Y/N")
                         + STRING(lSaturday,"Y/N")
                         + STRING(lHoliday,"Y/N")
                         .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN pGetDBTables.
  ASSIGN
      hTable = dateRules.baseTable:HANDLE IN FRAME {&FRAME-NAME}
      hField = dateRules.baseField:HANDLE
      skipampm:HIDDEN = iHourMax EQ 24
      .
  RUN pSetTableField.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetDBFields V-table-Win 
PROCEDURE pGetDBFields :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        dbFields:LIST-ITEM-PAIRS = ?.
        FIND FIRST ASI._file NO-LOCK
             WHERE ASI._file._file-name EQ dbTables:SCREEN-VALUE
             NO-ERROR.
        IF AVAILABLE ASI._file THEN
        FOR EACH ASI._field OF ASI._file NO-LOCK
            :
            dbFields:ADD-LAST((IF ASI._field._label NE ? THEN ASI._field._label + " "
                               ELSE "") + "(" + ASI._field._field-name + ")",ASI._field._field-name).
        END. /* each _file */
    END. /* do with */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetDBTables V-table-Win 
PROCEDURE pGetDBTables :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        dbTables:LIST-ITEM-PAIRS = ?.
        FOR EACH ASI._file NO-LOCK
            WHERE ASI._file._tbl-type EQ "T"
            :
            dbTables:ADD-LAST((IF ASI._file._file-label NE ? THEN ASI._file._file-label + " "
                               ELSE "") + "(" + ASI._file._file-name + ")",ASI._file._file-name).
        END. /* each _file */
    END. /* do with */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetTableField V-table-Win 
PROCEDURE pSetTableField :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        IF hTable:SCREEN-VALUE NE "" THEN DO:
            dbTables:SCREEN-VALUE = hTable:SCREEN-VALUE.
            APPLY "VALUE-CHANGED":U TO dbTables.
            IF hField:SCREEN-VALUE NE "" THEN
            dbFields:SCREEN-VALUE = hField:SCREEN-VALUE.
        END. /* if */
    END. /* do frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "DateRules"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

