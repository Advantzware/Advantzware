&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          emptrack         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/shifts.w

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

{custom/gcompany.i}

{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
ASSIGN cocode = g_company
       locode = g_loc.


&SCOPED-DEFINE enable-proc enable-proc

DEFINE VARIABLE cDaysList AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cUseDayListScreenValue AS CHARACTER   NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES shifts
&Scoped-define FIRST-EXTERNAL-TABLE shifts


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR shifts.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS shifts.description shifts.lunch_paid 
&Scoped-define ENABLED-TABLES shifts
&Scoped-define FIRST-ENABLED-TABLE shifts
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-3 rsAllOrSpecificDays tgMon ~
tgWeds tgThur tgFri tgTues tgSat tgSun 
&Scoped-Define DISPLAYED-FIELDS shifts.description shifts.lunch_paid 
&Scoped-define DISPLAYED-TABLES shifts
&Scoped-define FIRST-DISPLAYED-TABLE shifts
&Scoped-Define DISPLAYED-OBJECTS fi_shift start_hour start_minute ~
start_second start_ampm end_hour end_minute end_second end_ampm ~
lunch_start_hour lunch_start_minute lunch_start_second lunch_start_ampm ~
lunch_end_hour lunch_end_minute lunch_end_second lunch_end_ampm ~
rsAllOrSpecificDays tgMon tgWeds tgThur tgFri tgTues tgSat tgSun 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,TIME-FIELDS,F1 */
&Scoped-define ADM-CREATE-FIELDS fi_shift 
&Scoped-define TIME-FIELDS start_hour start_minute start_second start_ampm ~
end_hour end_minute end_second end_ampm lunch_start_hour lunch_start_minute ~
lunch_start_second lunch_start_ampm lunch_end_hour lunch_end_minute ~
lunch_end_second lunch_end_ampm shifts.lunch_paid 

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
DEFINE VARIABLE end_ampm AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "PM","AM" 
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE lunch_end_ampm AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "PM","AM" 
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE lunch_start_ampm AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "PM","AM" 
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE start_ampm AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "PM","AM" 
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE end_hour AS CHARACTER FORMAT "X(2)":U 
     LABEL "End Time" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE end_minute AS CHARACTER FORMAT "X(2)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE end_second AS CHARACTER FORMAT "X(2)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE fi_shift AS INTEGER FORMAT ">>" INITIAL 0 
     LABEL "Shift" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE lunch_end_hour AS CHARACTER FORMAT "X(2)":U 
     LABEL "End Time" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE lunch_end_minute AS CHARACTER FORMAT "X(2)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE lunch_end_second AS CHARACTER FORMAT "X(2)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE lunch_start_hour AS CHARACTER FORMAT "X(2)":U 
     LABEL "Lunch Start Time" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE lunch_start_minute AS CHARACTER FORMAT "X(2)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE lunch_start_second AS CHARACTER FORMAT "X(2)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE start_hour AS CHARACTER FORMAT "X(2)":U 
     LABEL "Start Time" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE start_minute AS CHARACTER FORMAT "X(2)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE start_second AS CHARACTER FORMAT "X(2)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE rsAllOrSpecificDays AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "All Days", "All",
"Specific Days", "Specific"
     SIZE 20 BY 2.14 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 55 BY 8.57.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 55 BY 5.14.

DEFINE VARIABLE tgFri AS LOGICAL INITIAL no 
     LABEL "Fri" 
     VIEW-AS TOGGLE-BOX
     SIZE 9.6 BY .81 NO-UNDO.

DEFINE VARIABLE tgMon AS LOGICAL INITIAL no 
     LABEL "Mon" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .81 NO-UNDO.

DEFINE VARIABLE tgSat AS LOGICAL INITIAL no 
     LABEL "Sat" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE tgSun AS LOGICAL INITIAL no 
     LABEL "Sun" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .81 NO-UNDO.

DEFINE VARIABLE tgThur AS LOGICAL INITIAL no 
     LABEL "Thu" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE tgTues AS LOGICAL INITIAL no 
     LABEL "Tue" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE tgWeds AS LOGICAL INITIAL no 
     LABEL "Wed" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi_shift AT ROW 1.24 COL 15 COLON-ALIGNED HELP
          "Enter Shift"
     shifts.description AT ROW 2.43 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
          BGCOLOR 15 FONT 4
     start_hour AT ROW 3.62 COL 15 COLON-ALIGNED HELP
          "Enter Starting Hour"
     start_minute AT ROW 3.62 COL 21 COLON-ALIGNED HELP
          "Enter Starting Minute"
     start_second AT ROW 3.62 COL 27.2 COLON-ALIGNED HELP
          "Enter Starting Second"
     start_ampm AT ROW 3.62 COL 32 COLON-ALIGNED NO-LABEL
     end_hour AT ROW 4.81 COL 15 COLON-ALIGNED HELP
          "Enter Ending Hour"
     end_minute AT ROW 4.81 COL 21 COLON-ALIGNED HELP
          "Enter Ending Minute"
     end_second AT ROW 4.81 COL 27.2 COLON-ALIGNED HELP
          "Enter Ending Second"
     end_ampm AT ROW 4.81 COL 32 COLON-ALIGNED NO-LABEL
     lunch_start_hour AT ROW 6.48 COL 21.4 COLON-ALIGNED
     lunch_start_minute AT ROW 6.48 COL 28 COLON-ALIGNED
     lunch_start_second AT ROW 6.48 COL 35 COLON-ALIGNED
     lunch_start_ampm AT ROW 6.48 COL 40 COLON-ALIGNED NO-LABEL
     lunch_end_hour AT ROW 7.43 COL 21.4 COLON-ALIGNED
     lunch_end_minute AT ROW 7.43 COL 28 COLON-ALIGNED
     lunch_end_second AT ROW 7.43 COL 35 COLON-ALIGNED
     lunch_end_ampm AT ROW 7.43 COL 40 COLON-ALIGNED NO-LABEL
     shifts.lunch_paid AT ROW 8.38 COL 21.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 
     rsAllOrSpecificDays AT ROW 10.76 COL 2 NO-LABEL WIDGET-ID 2
     tgMon AT ROW 12.91 COL 5 WIDGET-ID 6
     tgWeds AT ROW 12.91 COL 25 WIDGET-ID 10
     tgThur AT ROW 12.91 COL 35 WIDGET-ID 12
     tgFri AT ROW 12.91 COL 44 WIDGET-ID 14
     tgTues AT ROW 12.95 COL 16 WIDGET-ID 8
     tgSat AT ROW 13.86 COL 5 WIDGET-ID 16
     tgSun AT ROW 13.86 COL 16 WIDGET-ID 18
     "Applies To:" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 9.57 COL 4 WIDGET-ID 22
          FGCOLOR 9 
     RECT-1 AT ROW 1 COL 1
     RECT-3 AT ROW 9.81 COL 1 WIDGET-ID 24
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: shifts
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY
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
         HEIGHT             = 13.95
         WIDTH              = 55.
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

/* SETTINGS FOR COMBO-BOX end_ampm IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN end_hour IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN end_minute IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN end_second IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN fi_shift IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR COMBO-BOX lunch_end_ampm IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN lunch_end_hour IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN lunch_end_minute IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN lunch_end_second IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN shifts.lunch_paid IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR COMBO-BOX lunch_start_ampm IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN lunch_start_hour IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN lunch_start_minute IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN lunch_start_second IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR COMBO-BOX start_ampm IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN start_hour IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN start_minute IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN start_second IN FRAME F-Main
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

&Scoped-define SELF-NAME end_hour
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_hour V-table-Win
ON LEAVE OF end_hour IN FRAME F-Main /* End Time */
DO:
  {&methods/lValidateError.i YES}
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 12.
  {methods/entryerr.i &error-message="Invalid Hour, range = 0 to 12"}
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_minute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_minute V-table-Win
ON LEAVE OF end_minute IN FRAME F-Main
DO:
  {&methods/lValidateError.i YES}
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 59.
  {methods/entryerr.i &error-message="Invalid Minute, range = 0 to 59"}
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_second
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_second V-table-Win
ON LEAVE OF end_second IN FRAME F-Main
DO:
  {&methods/lValidateError.i YES}
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 59.
  {methods/entryerr.i &error-message="Invalid Second, range = 0 to 59"}
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_shift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_shift V-table-Win
ON LEAVE OF fi_shift IN FRAME F-Main /* Shift */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-fi_shift (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lunch_end_ampm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lunch_end_ampm V-table-Win
ON VALUE-CHANGED OF lunch_end_ampm IN FRAME F-Main
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lunch_end_hour
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lunch_end_hour V-table-Win
ON LEAVE OF lunch_end_hour IN FRAME F-Main /* End Time */
DO:
   {&methods/lValidateError.i YES}
      assign {&self-name}.
   correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 12.
   {methods/entryerr.i &error-message="Invalid Hour, range = 0 to 12"}
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lunch_end_minute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lunch_end_minute V-table-Win
ON LEAVE OF lunch_end_minute IN FRAME F-Main
DO:
  {&methods/lValidateError.i YES}
      assign {&self-name}.
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 59.
  {methods/entryerr.i &error-message="Invalid Minute, range = 0 to 59"}
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lunch_end_second
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lunch_end_second V-table-Win
ON LEAVE OF lunch_end_second IN FRAME F-Main
DO:
  {&methods/lValidateError.i YES}
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 59.
  {methods/entryerr.i &error-message="Invalid Second, range = 0 to 59"}
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lunch_start_ampm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lunch_start_ampm V-table-Win
ON VALUE-CHANGED OF lunch_start_ampm IN FRAME F-Main
DO:
      assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lunch_start_hour
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lunch_start_hour V-table-Win
ON LEAVE OF lunch_start_hour IN FRAME F-Main /* Lunch Start Time */
DO:
  {&methods/lValidateError.i YES}
      assign {&self-name} .
   correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 12.
  {methods/entryerr.i &error-message="Invalid Hour, range = 0 to 12"}
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lunch_start_minute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lunch_start_minute V-table-Win
ON LEAVE OF lunch_start_minute IN FRAME F-Main
DO:
  {&methods/lValidateError.i YES}
      assign {&self-name}.
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 59.
  {methods/entryerr.i &error-message="Invalid Minute, range = 0 to 59"}
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lunch_start_second
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lunch_start_second V-table-Win
ON LEAVE OF lunch_start_second IN FRAME F-Main
DO:
  {&methods/lValidateError.i YES}
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 59.
  {methods/entryerr.i &error-message="Invalid Second, range = 0 to 59"}
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsAllOrSpecificDays
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsAllOrSpecificDays V-table-Win
ON VALUE-CHANGED OF rsAllOrSpecificDays IN FRAME F-Main
DO:
    ASSIGN {&SELF}.

  IF rsAllOrSpecificDays:SCREEN-VALUE EQ "ALL" THEN DO WITH FRAME {&FRAME-NAME}:
    DISABLE tgSun tgMon tgTues tgWeds tgThur tgFri tgSat.
  END.
  ELSE DO WITH FRAME {&FRAME-NAME}:    
     ENABLE tgSun tgMon tgTues tgWeds tgThur tgFri tgSat.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME start_hour
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL start_hour V-table-Win
ON LEAVE OF start_hour IN FRAME F-Main /* Start Time */
DO:
  {&methods/lValidateError.i YES}
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 1 OR INTEGER(SELF:SCREEN-VALUE) GT 12.
  {methods/entryerr.i &error-message="Invalid Hour, range = 1 to 12"}
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME start_minute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL start_minute V-table-Win
ON LEAVE OF start_minute IN FRAME F-Main
DO:
  {&methods/lValidateError.i YES}
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 59.
  {methods/entryerr.i &error-message="Invalid Minute, range = 0 to 59"}
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME start_second
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL start_second V-table-Win
ON LEAVE OF start_second IN FRAME F-Main
DO:
  {&methods/lValidateError.i YES}
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 59.
  {methods/entryerr.i &error-message="Invalid Second, range = 0 to 59"}
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  {custom/getcmpny.i}

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
  {src/adm/template/row-list.i "shifts"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "shifts"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-proc V-table-Win 
PROCEDURE disable-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    DISABLE fi_shift {&TIME-FIELDS} WITH FRAME {&FRAME-NAME}.
    DISABLE rsAllOrSpecificDays tgSun tgMon tgTues tgWeds tgThur tgFri tgSat
      WITH FRAME {&FRAME-NAME}.

  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-proc V-table-Win 
PROCEDURE enable-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /*DO WITH FRAME {&FRAME-NAME}:
    ENABLE fi_shift.
  END.*/
  DO WITH FRAME {&FRAME-NAME}:

   ENABLE rsAllOrSpecificDays.

    IF rsAllOrSpecificDays:SCREEN-VALUE EQ "ALL" THEN
      DISABLE tgSun tgMon tgTues tgWeds tgThur tgFri tgSat
      WITH FRAME {&FRAME-NAME}.
    ELSE
      ENABLE tgSun tgMon tgTues tgWeds tgThur tgFri tgSat
        WITH FRAME {&FRAME-NAME}.
  END.

    IF adm-new-record THEN DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
       rsAllOrSpecificDays:SCREEN-VALUE = "All"
       tgSun:SCREEN-VALUE = "NO"
       tgMon:SCREEN-VALUE = "NO"
       tgTues:SCREEN-VALUE = "NO"
       tgWeds:SCREEN-VALUE = "NO"
       tgThur:SCREEN-VALUE = "NO"
       tgFri:SCREEN-VALUE = "NO"
       tgSat:SCREEN-VALUE = "NO".
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var ll-new-record as log no-undo.

  ll-new-record = adm-new-record.
  /* Code placed here will execute PRIOR to standard behavior. */


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN rsAllOrSpecificDays tgSun tgMon tgTues tgWeds tgThur tgFri tgSat
    .
  END.


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/viewers/assign/shifts.i}

  if not ll-new-record then do:
    {custom/set_time2.i
      &field="shifts.lunch_start"
      &hour="lunch_start_hour"
      &minute="lunch_start_minute"
      &second="lunch_start_second"
      &ampm="lunch_start_ampm"
     }

     {custom/set_time2.i
      &field="shifts.lunch_end"
      &hour="lunch_end_hour"
      &minute="lunch_end_minute"
      &second="lunch_end_second"
      &ampm="lunch_end_ampm"
     }
     /* =========== display 12 Hr */   
     {custom/get_time2.i
      &field="shifts.lunch_start"
      &hour="lunch_start_hour"
      &minute="lunch_start_minute"
      &second="lunch_start_second"
      &ampm="lunch_start_ampm"
     }
     {custom/get_time2.i
      &field="shifts.lunch_end"
      &hour="lunch_end_hour"
      &minute="lunch_end_minute"
      &second="lunch_end_second"
      &ampm="lunch_end_ampm"
     }
  end.

  shifts.shift = TRIM(STRING(fi_shift,">>")).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN disable-proc.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
    RUN valid-fi_shift (fi_shift:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/viewers/create/shifts.i}

  {custom/set_time2.i
      &field="shifts.lunch_start"
      &hour="lunch_start_hour"
      &minute="lunch_start_minute"
      &second="lunch_start_second"
      &ampm="lunch_start_ampm"
  }

  {custom/set_time2.i
      &field="shifts.lunch_end"
      &hour="lunch_end_hour"
      &minute="lunch_end_minute"
      &second="lunch_end_second"
      &ampm="lunch_end_ampm"
   }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR i AS INT NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
  IF AVAIL shifts AND NOT adm-new-record THEN DO:
    fi_shift = INT(shifts.shift) NO-ERROR.
  END.

  {custom/get_time2.i
      &field="shifts.lunch_start"
      &hour="lunch_start_hour"
      &minute="lunch_start_minute"
      &second="lunch_start_second"
      &ampm="lunch_start_ampm"
  }
  {custom/get_time2.i
      &field="shifts.lunch_end"
      &hour="lunch_end_hour"
      &minute="lunch_end_minute"
      &second="lunch_end_second"
      &ampm="lunch_end_ampm"
  }
  DO WITH FRAME {&FRAME-NAME}:
         ASSIGN
            tgSun:SCREEN-VALUE = "NO"
            tgMon:SCREEN-VALUE = "NO"
            tgTues:SCREEN-VALUE = "NO"
            tgWeds:SCREEN-VALUE = "NO"
            tgThur:SCREEN-VALUE = "NO"
            tgFri:SCREEN-VALUE = "NO"
            tgSat:SCREEN-VALUE = "NO"
            tgSun = NO
            tgMon = NO
            tgTues = NO
            tgWeds = NO
            tgThur = NO
            tgFri = NO
            tgSat = NO
            .
  IF AVAIL shifts THEN DO:
  ENABLE tgSun tgMon tgTues tgWeds tgThur tgFri tgSat
  WITH FRAME {&FRAME-NAME}.
      DO i = 1 TO NUM-ENTRIES(shifts.dayList):
        CASE i:
          WHEN 1 THEN
            ASSIGN tgSun:SCREEN-VALUE = ENTRY(i, shifts.dayList)
                   tgSun = LOGICAL(ENTRY(i, shifts.dayList)).
          WHEN 2 THEN
            ASSIGN tgMon:SCREEN-VALUE = ENTRY(i, shifts.dayList)
                   tgMon = LOGICAL(ENTRY(i, shifts.dayList)).
          WHEN 3 THEN
            ASSIGN tgTues:SCREEN-VALUE = ENTRY(i, shifts.dayList)
                   tgTues = LOGICAL(ENTRY(i, shifts.dayList)).
          WHEN 4 THEN
            ASSIGN tgWeds:SCREEN-VALUE = ENTRY(i, shifts.dayList)
                   tgWeds = LOGICAL(ENTRY(i, shifts.dayList)).
          WHEN 5 THEN
            ASSIGN tgThur:SCREEN-VALUE = ENTRY(i, shifts.dayList)
                   tgThur = LOGICAL(ENTRY(i, shifts.dayList)).
          WHEN 6 THEN
            ASSIGN tgFri:SCREEN-VALUE = ENTRY(i, shifts.dayList)
                   tgFri = LOGICAL(ENTRY(i, shifts.dayList)).
          WHEN 7 THEN
            ASSIGN tgSat:SCREEN-VALUE = ENTRY(i, shifts.dayList)
                   tgSat = LOGICAL(ENTRY(i, shifts.dayList)).

        END CASE.
      END.

      rsAllOrSpecificDays = IF shifts.useSpecificDays EQ TRUE THEN "Specific" ELSE "All".
    END.
    ELSE 
      rsAllOrSpecificDays = "All".
  disABLE tgSun tgMon tgTues tgWeds tgThur tgFri tgSat
  WITH FRAME {&FRAME-NAME}.
    IF rsAllOrSpecificDays:SCREEN-VALUE EQ "ALL" THEN
      DISABLE tgSun tgMon tgTues tgWeds tgThur tgFri tgSat.
  END. /* Do wth Frame */
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN disable-proc.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR char-hdl AS CHAR NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */  

    cDaysList = "".
    IF adm-new-record THEN DO WITH FRAME {&FRAME-NAME}:
    cDaysList =  STRING(tgSun:SCREEN-VALUE) + "," +
                    STRING(tgMon:SCREEN-VALUE) + "," +
                    STRING(tgTues:SCREEN-VALUE) + "," +
                    STRING(tgWeds:SCREEN-VALUE) + "," +
                    STRING(tgThur:SCREEN-VALUE) + "," +
                    STRING(tgFri:SCREEN-VALUE) + "," +
                    STRING(tgSat:SCREEN-VALUE).
    cUseDayListScreenValue = rsAllOrSpecificDays:SCREEN-VALUE.
  END.


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF NOT adm-new-record THEN DO WITH FRAME {&FRAME-NAME}:
    ASSIGN rsAllOrSpecificDays tgSun tgMon tgTues tgWeds tgThur tgFri tgSat
      .
  END.


  IF adm-new-record THEN DO WITH FRAME {&FRAME-NAME}:
      ASSIGN shifts.dayList =  cDaysList
             shifts.useSpecificDays = (IF cUseDayListScreenValue EQ "Specific" THEN TRUE ELSE FALSE).
      
     ASSIGN
       rsAllOrSpecificDays:SCREEN-VALUE = cUseDayListScreenValue
       tgSun:SCREEN-VALUE = ENTRY(1, cDaysList)
       tgMon:SCREEN-VALUE = ENTRY(2, cDaysList) 
       tgTues:SCREEN-VALUE = ENTRY(3, cDaysList) 
       tgWeds:SCREEN-VALUE = ENTRY(4, cDaysList) 
       tgThur:SCREEN-VALUE = ENTRY(5, cDaysList) 
       tgFri:SCREEN-VALUE = ENTRY(6, cDaysList) 
       tgSat:SCREEN-VALUE = ENTRY(7, cDaysList)
       rsAllOrSpecificDays = cUseDayListScreenValue
       tgSun = LOGICAL(ENTRY(1, cDaysList))
       tgMon = LOGICAL(ENTRY(2, cDaysList)) 
       tgTues = LOGICAL(ENTRY(3, cDaysList)) 
       tgWeds = LOGICAL(ENTRY(4, cDaysList)) 
       tgThur = LOGICAL(ENTRY(5, cDaysList)) 
       tgFri = LOGICAL(ENTRY(6, cDaysList)) 
       tgSat = LOGICAL(ENTRY(7, cDaysList))       .
      /* adm-new-record not reset after this for some reason */
      adm-new-record = NO.
      RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
      RUN refresh-and-reposition IN WIDGET-HANDLE(char-hdl) (ROWID(shifts)). 
  END.
  ELSE DO:

    shifts.dayList =  STRING(tgSun) + "," +
                      STRING(tgMon) + "," +
                      STRING(tgTues) + "," +
                      STRING(tgWeds) + "," +
                      STRING(tgThur) + "," +
                      STRING(tgFri) + "," +
                      STRING(tgSat).
    shifts.useSpecificDays = (IF rsAllOrSpecificDays EQ "Specific" THEN TRUE ELSE FALSE).
  END.                   


  RUN disable-proc.

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
  {src/adm/template/snd-list.i "shifts"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-fi_shift V-table-Win 
PROCEDURE valid-fi_shift :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.

  DEF BUFFER b-shifts FOR shifts.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF CAN-FIND(FIRST b-shifts
                WHERE b-shifts.company    EQ cocode
                  AND INT(b-shifts.shift) EQ INT(ip-focus:SCREEN-VALUE)) THEN DO:
      MESSAGE TRIM(ip-focus:LABEL) + ": " + TRIM(ip-focus:SCREEN-VALUE) +
              " already exists, please re-enter...".
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

