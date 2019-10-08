&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File:

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

{custom/globdefs.i}

g_batch = YES.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES user-print user-batch
&Scoped-define FIRST-EXTERNAL-TABLE user-print


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR user-print, user-batch.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS user-print.frequency user-print.prog-title ~
user-batch.startDate user-batch.endDate user-batch.dayOfWeek[1] ~
user-batch.dayOfWeek[2] user-batch.dayOfWeek[3] user-batch.dayOfWeek[4] ~
user-batch.dayOfWeek[5] user-batch.dayOfWeek[6] user-batch.dayOfWeek[7] ~
user-batch.repeatWeekly user-print.last-date user-print.next-date ~
user-print.printer-name user-print.user-id 
&Scoped-define ENABLED-TABLES user-print user-batch
&Scoped-define FIRST-ENABLED-TABLE user-print
&Scoped-define SECOND-ENABLED-TABLE user-batch
&Scoped-Define ENABLED-OBJECTS RECT-41 ed-values 
&Scoped-Define DISPLAYED-FIELDS user-print.batch-seq user-print.batch ~
user-print.program-id user-print.frequency user-print.prog-title ~
user-batch.startDate user-batch.endDate user-batch.dayOfWeek[1] ~
user-batch.dayOfWeek[2] user-batch.dayOfWeek[3] user-batch.dayOfWeek[4] ~
user-batch.dayOfWeek[5] user-batch.dayOfWeek[6] user-batch.dayOfWeek[7] ~
user-batch.repeatWeekly user-print.last-date user-print.next-date ~
user-print.printer-name user-print.user-id 
&Scoped-define DISPLAYED-TABLES user-print user-batch
&Scoped-define FIRST-DISPLAYED-TABLE user-print
&Scoped-define SECOND-DISPLAYED-TABLE user-batch
&Scoped-Define DISPLAYED-OBJECTS v-start-time v-end-time v-last-time ~
v-next-time ed-values 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS user-print.program-id 
&Scoped-define ADM-ASSIGN-FIELDS user-print.batch-seq user-print.batch 

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
DEFINE VARIABLE ed-values AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 82 BY 4.29
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE v-end-time AS CHARACTER FORMAT "99:99" 
     LABEL "Time" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE v-last-time AS CHARACTER FORMAT "99:99" 
     LABEL "Time" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE v-next-time AS CHARACTER FORMAT "99:99" 
     LABEL "Time" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE v-start-time AS CHARACTER FORMAT "99:99" 
     LABEL "Time" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 98 BY 17.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     user-print.batch-seq AT ROW 1.24 COL 14 COLON-ALIGNED
          LABEL "Batch Seq"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 
     user-print.batch AT ROW 2.43 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 59 BY 1
          BGCOLOR 15 
     user-print.program-id AT ROW 3.62 COL 14 COLON-ALIGNED
          LABEL "Program-Id"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 
     user-print.frequency AT ROW 3.62 COL 67 COLON-ALIGNED
          LABEL "# of Copies?" FORMAT "x"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
          BGCOLOR 15 
     user-print.prog-title AT ROW 4.81 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 59 BY 1
          BGCOLOR 15 
     user-batch.startDate AT ROW 6 COL 14 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 
     v-start-time AT ROW 6 COL 36 COLON-ALIGNED WIDGET-ID 6
     user-batch.endDate AT ROW 7.19 COL 14 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 
     v-end-time AT ROW 7.19 COL 36 COLON-ALIGNED WIDGET-ID 8
     user-batch.dayOfWeek[1] AT ROW 8.38 COL 16 WIDGET-ID 44
          LABEL "S"
          VIEW-AS TOGGLE-BOX
          SIZE 5 BY .81
     user-batch.dayOfWeek[2] AT ROW 8.38 COL 22 WIDGET-ID 46
          LABEL "M"
          VIEW-AS TOGGLE-BOX
          SIZE 5 BY .81
     user-batch.dayOfWeek[3] AT ROW 8.38 COL 28 WIDGET-ID 48
          LABEL "T"
          VIEW-AS TOGGLE-BOX
          SIZE 5 BY .81
     user-batch.dayOfWeek[4] AT ROW 8.38 COL 33 WIDGET-ID 50
          LABEL "W"
          VIEW-AS TOGGLE-BOX
          SIZE 6 BY .81
     user-batch.dayOfWeek[5] AT ROW 8.38 COL 39 WIDGET-ID 52
          LABEL "T"
          VIEW-AS TOGGLE-BOX
          SIZE 5 BY .81
     user-batch.dayOfWeek[6] AT ROW 8.38 COL 44 WIDGET-ID 54
          LABEL "F"
          VIEW-AS TOGGLE-BOX
          SIZE 5 BY .81
     user-batch.dayOfWeek[7] AT ROW 8.38 COL 49 WIDGET-ID 56
          LABEL "S"
          VIEW-AS TOGGLE-BOX
          SIZE 5 BY .81
     user-batch.repeatWeekly AT ROW 8.38 COL 56 WIDGET-ID 10
          VIEW-AS TOGGLE-BOX
          SIZE 19 BY .81
     user-print.last-date AT ROW 9.33 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 
     v-last-time AT ROW 9.33 COL 36 COLON-ALIGNED
     user-print.next-date AT ROW 10.48 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 
     v-next-time AT ROW 10.52 COL 36 COLON-ALIGNED
     user-print.printer-name AT ROW 11.71 COL 14 COLON-ALIGNED
          LABEL "Printer Name"
          VIEW-AS FILL-IN 
          SIZE 59 BY 1
          BGCOLOR 15 
     user-print.user-id AT ROW 12.91 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 
     ed-values AT ROW 14.1 COL 16 NO-LABEL
     "Day of Week:" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 8.57 COL 2 WIDGET-ID 60
     RECT-41 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.user-print,asi.user-batch
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
         HEIGHT             = 17.62
         WIDTH              = 98.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN user-print.batch IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN user-print.batch-seq IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX user-batch.dayOfWeek[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX user-batch.dayOfWeek[2] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX user-batch.dayOfWeek[3] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX user-batch.dayOfWeek[4] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX user-batch.dayOfWeek[5] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX user-batch.dayOfWeek[6] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX user-batch.dayOfWeek[7] IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       ed-values:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN user-print.frequency IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN user-print.printer-name IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN user-print.program-id IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN v-end-time IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-last-time IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-next-time IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-start-time IN FRAME F-Main
   NO-ENABLE                                                            */
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

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
     DEF VAR char-val AS cha NO-UNDO.

     CASE FOCUS:NAME:
         WHEN "program-id" THEN DO:
             RUN windows/l-prgm.w (g_company,FOCUS:SCREEN-VALUE,OUTPUT char-val).
             IF char-val <> "" THEN DO:
                FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
             END.
         END.
     END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-end-time
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-end-time V-table-Win
ON LEAVE OF v-end-time IN FRAME F-Main /* Time */
DO:
{&methods/lValidateError.i YES}
  ASSIGN {&SELF-NAME}.
  IF INTEGER(SUBSTR({&SELF-NAME},1,2)) * 3600 +
     INTEGER(SUBSTR({&SELF-NAME},3,2)) * 60 LT 0 OR
     INTEGER(SUBSTR({&SELF-NAME},1,2)) * 3600 +
     INTEGER(SUBSTR({&SELF-NAME},3,2)) * 60 GT 86400 THEN DO:
    MESSAGE "Time Entered is Invalid!" VIEW-AS ALERT-BOX ERROR.
    APPLY "ENTRY" TO {&SELF-NAME}.
    RETURN NO-APPLY.
  END.
{&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-last-time
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-last-time V-table-Win
ON LEAVE OF v-last-time IN FRAME F-Main /* Time */
DO:
{&methods/lValidateError.i YES}
  ASSIGN {&SELF-NAME}.
  IF INTEGER(SUBSTR({&SELF-NAME},1,2)) * 3600 +
     INTEGER(SUBSTR({&SELF-NAME},3,2)) * 60 LT 0 OR
     INTEGER(SUBSTR({&SELF-NAME},1,2)) * 3600 +
     INTEGER(SUBSTR({&SELF-NAME},3,2)) * 60 GT 86400 THEN DO:
    MESSAGE "Time Entered is Invalid!" VIEW-AS ALERT-BOX ERROR.
    APPLY "ENTRY" TO {&SELF-NAME}.
    RETURN NO-APPLY.
  END.
{&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-next-time
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-next-time V-table-Win
ON LEAVE OF v-next-time IN FRAME F-Main /* Time */
DO:
{&methods/lValidateError.i YES}
  ASSIGN {&SELF-NAME}.
  IF INTEGER(SUBSTR({&SELF-NAME},1,2)) * 3600 +
     INTEGER(SUBSTR({&SELF-NAME},3,2)) * 60 LT 0 OR
     INTEGER(SUBSTR({&SELF-NAME},1,2)) * 3600 +
     INTEGER(SUBSTR({&SELF-NAME},3,2)) * 60 GT 86400 THEN DO:
    MESSAGE "Time Entered is Invalid!" VIEW-AS ALERT-BOX ERROR.
    APPLY "ENTRY" TO {&SELF-NAME}.
    RETURN NO-APPLY.
  END.
{&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-start-time
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-start-time V-table-Win
ON LEAVE OF v-start-time IN FRAME F-Main /* Time */
DO:
{&methods/lValidateError.i YES}
  ASSIGN {&SELF-NAME}.
  IF INTEGER(SUBSTR({&SELF-NAME},1,2)) * 3600 +
     INTEGER(SUBSTR({&SELF-NAME},3,2)) * 60 LT 0 OR
     INTEGER(SUBSTR({&SELF-NAME},1,2)) * 3600 +
     INTEGER(SUBSTR({&SELF-NAME},3,2)) * 60 GT 86400 THEN DO:
    MESSAGE "Time Entered is Invalid!" VIEW-AS ALERT-BOX ERROR.
    APPLY "ENTRY" TO {&SELF-NAME}.
    RETURN NO-APPLY.
  END.
{&methods/lValidateError.i NO}
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
  {src/adm/template/row-list.i "user-print"}
  {src/adm/template/row-list.i "user-batch"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "user-print"}
  {src/adm/template/row-find.i "user-batch"}

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
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR persist-hd AS HANDLE NO-UNDO.
  DEF VAR RUN-PROC AS cha NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
    user-batch.startTime = INTEGER(SUBSTR(v-start-time,1,2)) * 3600 +
                           INTEGER(SUBSTR(v-start-time,3,2)) * 60
    user-batch.endTime   = INTEGER(SUBSTR(v-end-time,1,2)) * 3600 +
                           INTEGER(SUBSTR(v-end-time,3,2)) * 60
    user-print.last-time = INTEGER(SUBSTR(v-last-time,1,2)) * 3600 +
                           INTEGER(SUBSTR(v-last-time,3,2)) * 60
    user-print.next-time = INTEGER(SUBSTR(v-next-time,1,2)) * 3600 +
                           INTEGER(SUBSTR(v-next-time,3,2)) * 60
    .


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
  DEF VAR lv-last-batch AS INT NO-UNDO.
  DEF BUFFER bf-batch FOR user-print.

  FOR EACH bf-batch NO-LOCK
      WHERE bf-batch.company EQ g_company
      BY bf-batch.batch-seq DESC.
    lv-last-batch = bf-batch.BATCH-seq.
    LEAVE.
  END.
  lv-last-batch = lv-last-batch + 1.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  ASSIGN
    user-print.company = g_company
    user-print.batch-seq = lv-last-batch
    user-print.batch = "Batch"
    user-print.user-id = USERID('nosweat')
    user-batch.company = user-print.company
    user-batch.batch-seq = user-print.batch-seq
    user-batch.prog-seq = user-print.prog-seq.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {custom/askdel.i}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FOR EACH user-batch EXCLUSIVE-LOCK
      WHERE user-batch.company = user-print.company
        AND user-batch.batch-seq = user-print.batch-seq
        AND user-batch.prog-seq = user-print.prog-seq:
    DELETE user-batch.
  END. /* each user-batch */


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
  ed-values = "".

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAIL user-print THEN DO:
     ASSIGN
       v-start-time = REPLACE(STRING(user-batch.startTime,'hh:mm'),':','')
       v-end-time   = REPLACE(STRING(user-batch.endTime,'hh:mm'),':','')
       v-last-time  = REPLACE(STRING(user-print.last-time,'hh:mm'),':','')
       v-next-time  = REPLACE(STRING(user-print.next-time,'hh:mm'),':','')
       .
     DO i = 1 TO 100:
        IF user-print.field-value[i] = ? THEN ed-values = ed-values + "".
        ELSE ed-values = ed-values + user-print.FIELD-value[i] + ",".
     END.
  END.
  ELSE ed-values = "".
  DISPLAY v-start-time v-end-time v-last-time v-next-time ed-values
    WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ENABLE v-start-time v-end-time v-last-time v-next-time
    WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-prt FOR user-print.
  DEF VAR char-hdl AS cha NO-UNDO.
{&methods/lValidateError.i YES}
  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN
    g_batch-rowid = ROWID(user-print)
    g_batch = YES.

  IF int(user-print.FREQUENCY) < 0 OR int(user-print.FREQUENCY) > 9 THEN DO:
     MESSAGE "Invalid # of copies. " VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO user-print.FREQUENCY IN FRAME {&FRAME-NAME}.
     RETURN .
  END.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DISABLE v-start-time v-end-time v-last-time v-next-time WITH FRAME {&FRAME-NAME}.

  MESSAGE "Do you want to update report parameters? " VIEW-AS ALERT-BOX QUESTION
      BUTTON YES-NO UPDATE lv-ans AS LOG.
  IF lv-ans THEN RUN update-batch.

  ASSIGN
    g_batch = NO
    g_batch-rowid = ?.
{&methods/lValidateError.i NO}

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
  {src/adm/template/snd-list.i "user-print"}
  {src/adm/template/snd-list.i "user-batch"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-batch V-table-Win 
PROCEDURE update-batch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR persist-hd AS HANDLE NO-UNDO.
  DEF VAR RUN-PROC AS cha NO-UNDO.
  DEF VAR i AS INT NO-UNDO.


    g_batch-rowid = ROWID(user-print).
    /*RUN VALUE(prgrms.DIR_group + "\" + prgrms.prgmname + "r"). */
    RUN nosweat/persist.p PERSISTENT SET Persist-Hd.
    RUN Get_Procedure IN Persist-Hd (user-print.prgmName, OUTPUT RUN-PROC, NO).

    IF run-proc NE "" THEN RUN VALUE(run-proc).

    FIND CURRENT user-print NO-LOCK NO-ERROR.
    ed-values = "".
    DO i = 1 TO 100:
      IF user-print.field-value[i] = ? THEN ed-values = ed-values + "".
      ELSE ed-values = ed-values + user-print.field-value[i] + ",".
    END.
    DISPLAY ed-values WITH FRAME {&FRAME-NAME}.
 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

