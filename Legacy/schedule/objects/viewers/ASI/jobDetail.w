&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: jobDetail.w

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters: <none>

  Output Parameters: <none>

  History: 
          
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

{schedule/scopDir.i}
{{&includes}/defBoard.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{{&includes}/sharedvars.i}

DEFINE VARIABLE boardHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE boardType AS CHARACTER NO-UNDO.
DEFINE VARIABLE estType AS CHARACTER NO-UNDO.
DEFINE VARIABLE estTypeSave AS CHARACTER NO-UNDO.
DEFINE VARIABLE jobDetail AS CHARACTER NO-UNDO.
DEFINE VARIABLE ttblJobRowID AS ROWID NO-UNDO.

{{&includes}/ttblJob.i}
{{&includes}/{&Board}/calcEnd.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Record-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES est
&Scoped-define FIRST-EXTERNAL-TABLE est


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR est.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnComplete btnJobNotes 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 btnDataCollection 
&Scoped-define List-2 btnDataCollection 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcJobTime W-Win 
FUNCTION calcJobTime RETURNS INTEGER
  (ipMr AS DECIMAL, ipRun AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD numericDateTime W-Win 
FUNCTION numericDateTime RETURNS DECIMAL
  (ipDate AS DATE,ipTime AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_cust AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cust-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folding AS HANDLE NO-UNDO.
DEFINE VARIABLE h_item AS HANDLE NO-UNDO.
DEFINE VARIABLE h_item-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itempanel AS HANDLE NO-UNDO.
DEFINE VARIABLE h_job AS HANDLE NO-UNDO.
DEFINE VARIABLE h_job-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_job-mat AS HANDLE NO-UNDO.
DEFINE VARIABLE h_job-mch AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobdetail AS HANDLE NO-UNDO.
DEFINE VARIABLE h_machine AS HANDLE NO-UNDO.
DEFINE VARIABLE h_notes AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnComplete 
     IMAGE-UP FILE "schedule/images/save.bmp":U
     LABEL "" 
     SIZE 5 BY 1.19 TOOLTIP "Complete Job".

DEFINE BUTTON btnDataCollection 
     IMAGE-UP FILE "schedule/images/datacollection.bmp":U
     LABEL "" 
     SIZE 5 BY 1.19 TOOLTIP "Access Data Collection".

DEFINE BUTTON btnJobNotes 
     IMAGE-UP FILE "schedule/images/notetack.bmp":U
     LABEL "" 
     SIZE 5 BY 1.19 TOOLTIP "Job Notes".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnComplete AT ROW 1 COL 134 HELP
          "Click to Complete Job"
     btnJobNotes AT ROW 1 COL 140 HELP
          "Click to Access Notes"
     btnDataCollection AT ROW 1 COL 146 HELP
          "Click to Access Data Collection"
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 180 BY 9.52.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: asi.est
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Job Detail - Scheduler"
         HEIGHT             = 9.52
         WIDTH              = 180
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 183.2
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 183.2
         SHOW-IN-TASKBAR    = no
         MAX-BUTTON         = no
         TOP-ONLY           = yes
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT W-Win:LOAD-ICON("schedule/images/scheduler.ico":U) THEN
    MESSAGE "Unable to load icon: schedule/images/scheduler.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
ASSIGN 
       btnComplete:PRIVATE-DATA IN FRAME F-Main     = 
                "Complete Job".

/* SETTINGS FOR BUTTON btnDataCollection IN FRAME F-Main
   NO-ENABLE 1 2                                                        */
ASSIGN 
       btnDataCollection:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btnJobNotes:PRIVATE-DATA IN FRAME F-Main     = 
                "Job Notes".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Job Detail - Scheduler */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Job Detail - Scheduler */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnComplete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnComplete W-Win
ON CHOOSE OF btnComplete IN FRAME F-Main
DO:
  {{&includes}/{&Board}/btnComplete.i ttblJobRowID boardHandle}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDataCollection
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDataCollection W-Win
ON CHOOSE OF btnDataCollection IN FRAME F-Main
DO:
  RUN dataCollection IN boardHandle (sharedRowIDs).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnJobNotes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnJobNotes W-Win
ON CHOOSE OF btnJobNotes IN FRAME F-Main
DO:
  &SCOPED-DEFINE boardType
  {{&includes}/{&Board}/btnJobNotes.i ttblJobRowID}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

{{&viewers}/includes/winTitle.i}

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Job|Items|Materials|Machine|Depart. Notes|Jobs/Routings' + ',
                     FOLDER-TAB-TYPE = 1':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 1.00 , 1.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 9.52 , 180.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'schedule/objects/viewers/asi/query/job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_job ).
       RUN set-position IN h_job ( 1.00 , 169.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.86 , 10.80 ) */

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Links to SmartQuery h_job. */
       RUN add-link IN adm-broker-hdl ( h_job , 'Record':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_folder ,
             btnComplete:HANDLE IN FRAME F-Main , 'BEFORE':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
           &IF DEFINED(UIB_is_Running) ne 0 &THEN
             INPUT  'schedule/objects/viewers/jobdetailholder.w':U ,
           &ELSE
             INPUT jobDetail ,
           &ENDIF
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_jobdetail ).
       RUN set-position IN h_jobdetail ( 2.43 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.29 , 32.40 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'schedule/objects/viewers/asi/viewers/cust.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_cust-2 ).
       RUN set-position IN h_cust-2 ( 5.29 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 5.10 , 178.40 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'schedule/objects/viewers/asi/query/cust.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_cust ).
       RUN set-position IN h_cust ( 2.43 , 157.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.86 , 10.80 ) */

       /* Links to SmartViewer h_cust-2. */
       RUN add-link IN adm-broker-hdl ( h_cust , 'Record':U , h_cust-2 ).

       /* Links to SmartQuery h_cust. */
       RUN add-link IN adm-broker-hdl ( h_job , 'Record':U , h_cust ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_jobdetail ,
             btnDataCollection:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_cust-2 ,
             h_jobdetail , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'schedule/objects/viewers/asi/browsers/item.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_item ).
       RUN set-position IN h_item ( 2.67 , 2.00 ) NO-ERROR.
       RUN set-size IN h_item ( 7.43 , 62.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'schedule/objects/viewers/asi/viewers/item.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_item-2 ).
       RUN set-position IN h_item-2 ( 2.67 , 64.00 ) NO-ERROR.
       /* Size in UIB:  ( 7.38 , 56.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'schedule/objects/viewers/asi/viewers/itempanel.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itempanel ).
       RUN set-position IN h_itempanel ( 2.67 , 120.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 60.00 ) */

       RUN init-object IN THIS-PROCEDURE (
           &IF DEFINED(UIB_is_Running) ne 0 &THEN
             INPUT  'schedule/objects/viewers/asi/viewers/folding.w':U ,
           &ELSE
             INPUT estType ,
           &ENDIF
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_folding ).
       RUN set-position IN h_folding ( 4.33 , 120.00 ) NO-ERROR.
       /* Size in UIB:  ( 5.71 , 60.00 ) */

       /* Links to SmartBrowser h_item. */
       RUN add-link IN adm-broker-hdl ( h_job , 'Record':U , h_item ).

       /* Links to SmartViewer h_item-2. */
       RUN add-link IN adm-broker-hdl ( h_item , 'Record':U , h_item-2 ).

       /* Links to SmartViewer h_itempanel. */
       RUN add-link IN adm-broker-hdl ( h_item , 'Record':U , h_itempanel ).

       /* Links to SmartViewer h_folding. */
       RUN add-link IN adm-broker-hdl ( h_item , 'Record':U , h_folding ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_item ,
             btnDataCollection:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_item-2 ,
             h_item , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_itempanel ,
             h_item-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_folding ,
             h_itempanel , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'schedule/objects/viewers/asi/browsers/job-mat.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_job-mat ).
       RUN set-position IN h_job-mat ( 2.67 , 23.00 ) NO-ERROR.
       RUN set-size IN h_job-mat ( 7.38 , 145.00 ) NO-ERROR.

       /* Links to SmartBrowser h_job-mat. */
       RUN add-link IN adm-broker-hdl ( h_job , 'Record':U , h_job-mat ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_job-mat ,
             btnDataCollection:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'schedule/objects/viewers/asi/viewers/machine.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_machine ).
       RUN set-position IN h_machine ( 2.43 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 7.95 , 177.80 ) */

       /* Links to SmartViewer h_machine. */
       RUN add-link IN adm-broker-hdl ( h_job , 'Record':U , h_machine ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_machine ,
             btnDataCollection:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 4 */
    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'schedule/objects/viewers/asi/browsers/notes.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_notes ).
       RUN set-position IN h_notes ( 2.67 , 3.00 ) NO-ERROR.
       RUN set-size IN h_notes ( 7.43 , 176.00 ) NO-ERROR.

       /* Links to SmartBrowser h_notes. */
       RUN add-link IN adm-broker-hdl ( h_job , 'Record':U , h_notes ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_notes ,
             btnDataCollection:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 5 */
    WHEN 6 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'schedule/objects/viewers/asi/browsers/job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_job-2 ).
       RUN set-position IN h_job-2 ( 2.67 , 3.00 ) NO-ERROR.
       RUN set-size IN h_job-2 ( 7.43 , 97.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'schedule/objects/viewers/asi/browsers/job-mch.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_job-mch ).
       RUN set-position IN h_job-mch ( 2.67 , 101.00 ) NO-ERROR.
       RUN set-size IN h_job-mch ( 7.43 , 78.00 ) NO-ERROR.

       /* Links to SmartBrowser h_job-2. */
       RUN add-link IN adm-broker-hdl ( h_job , 'Record':U , h_job-2 ).

       /* Links to SmartBrowser h_job-mch. */
       RUN add-link IN adm-broker-hdl ( h_job-2 , 'Record':U , h_job-mch ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_job-2 ,
             btnDataCollection:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_job-mch ,
             h_job-2 , 'AFTER':U ).
    END. /* Page 6 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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
  {src/adm/template/row-list.i "est"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "est"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE downtimeSpan W-Win 
PROCEDURE downtimeSpan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {{&includes}/{&Board}/downtimeSpan.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  ENABLE btnComplete btnJobNotes 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE job W-Win 
PROCEDURE job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.
  DEFINE INPUT PARAMETER ipHandle AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER ipYCoord AS INTEGER NO-UNDO.

  DEFINE VARIABLE autoMonitor AS LOGICAL NO-UNDO.
  
  IF ipYCoord NE ? AND {&WINDOW-NAME}:WINDOW-STATE NE 2 THEN
  {&WINDOW-NAME}:Y = ipYCoord.
  RUN displayFields IN h_jobDetail (ipRowID).
  RUN dispatch IN h_job ('initialize':U).
  ASSIGN
    ttblJobRowID = ipRowID
    boardHandle = ipHandle
    .
  RUN boardType IN boardHandle (OUTPUT boardType).
  IF boardType NE '{&Board}' THEN DO:
    btnComplete:HIDDEN IN FRAME {&FRAME-NAME} = YES.
    RUN setUpdateButtons IN h_cust-2 (NO).
  END. /* if not pro */
  ELSE DO:
    RUN autoMonitorFlag IN boardHandle (OUTPUT autoMonitor).
    IF autoMonitor THEN
    DISABLE btnComplete btnJobNotes WITH FRAME {&FRAME-NAME}.
    ELSE
    ENABLE btnComplete btnJobNotes WITH FRAME {&FRAME-NAME}.
    RUN setUpdateButtons IN h_cust-2 (autoMonitor).
  END. /* else */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-change-page W-Win 
PROCEDURE local-change-page :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  jobDetail = '{&viewers}/jobDetail.w'.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'change-page':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available W-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  DEFINE VARIABLE currentPage AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  currentPage = INTEGER(RETURN-VALUE).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF estType EQ '' THEN
  ASSIGN
    estType = findProgram('{&viewers}/',ID,'/viewers/folding.w')
    estTypeSave = estType.
  
  IF AVAILABLE est THEN
  estType = findProgram('{&viewers}/',ID,'/viewers/' +
           (IF est.est-type LE 4 THEN 'folding.w' ELSE 'corrugated.w')).
  IF estType NE estTypeSave AND currentPage EQ 2 THEN
  DO:
    estTypeSave = estType.
    IF VALID-HANDLE(h_folding) THEN
    RUN dispatch IN h_folding ('destroy':U).
    RUN init-object IN THIS-PROCEDURE (
        &IF DEFINED(UIB_is_Running) ne 0 &THEN
          INPUT  'schedule/objects/viewers/ASI/viewers/folding.w':U ,
        &ELSE
          INPUT estType ,
        &ENDIF
          INPUT  FRAME F-Main:HANDLE ,
          INPUT  'Layout = ':U ,
          OUTPUT h_folding ).
    RUN set-position IN h_folding (4.33,120.00) NO-ERROR.
    RUN add-link IN adm-broker-hdl (h_item,'Record':U,h_folding).
    RUN dispatch IN h_folding ('initialize':U).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newEnd W-Win 
PROCEDURE newEnd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {{&includes}/{&Board}/newEnd.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowIDs W-Win 
PROCEDURE rowIDs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER opJobRowId AS ROWID NO-UNDO.
  DEFINE OUTPUT PARAMETER opJobMchRowID AS ROWID NO-UNDO.

  ASSIGN
    opJobRowID = TO-ROWID(ENTRY(1,sharedRowIDs))
    opJobMchRowID = TO-ROWID(ENTRY(2,sharedRowIDs))
    btnDataCollection:HIDDEN IN FRAME {&FRAME-NAME} = NOT CONNECTED('emptrack')
    btnDataCollection:SENSITIVE = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "est"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valueChange W-Win 
PROCEDURE valueChange :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.

  FIND job-mch NO-LOCK WHERE ROWID(job-mch) EQ ipRowID NO-ERROR.
  IF NOT AVAILABLE job-mch THEN RETURN.
  FIND ttblJob EXCLUSIVE-LOCK WHERE ROWID(ttblJob) EQ ttblJobRowID NO-ERROR.
  IF NOT AVAILABLE ttblJob THEN RETURN.
  ASSIGN
    ttblJob.startDate = job-mch.start-date-su
    ttblJob.startTime = job-mch.start-time-su
    ttblJob.userField15 = IF job-mch.run-qty EQ ? THEN '' ELSE LEFT-TRIM(STRING(job-mch.run-qty,'zz,zzz,zz9')).
  RUN calcEnd (ttblJob.startDate,ttblJob.startTime,job-mch.mr-hr,job-mch.run-hr,
               OUTPUT ttblJob.endDate,OUTPUT ttblJob.endTime).
  ttblJob.timeSpan = calcJobTime(job-mch.mr-hr,job-mch.run-hr).
  RUN downtimeSpan (ttblJob.resource,ttblJob.timeSpan,ttblJob.startDate,ttblJob.startTime,
                    OUTPUT ttblJob.endDate,OUTPUT ttblJob.endTime,OUTPUT ttblJob.downtimeSpan).
  ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime).
  ttblJob.endDateTime = numericDateTime(ttblJob.endDate,ttblJob.endTime).
  RUN buildBoard IN boardHandle (NO).
  RUN detailJob IN boardHandle (ROWID(ttblJob),ttblJob.rowIDs).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcJobTime W-Win 
FUNCTION calcJobTime RETURNS INTEGER
  (ipMr AS DECIMAL, ipRun AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF ipMR EQ ? THEN ipMR = 0.
  IF ipRun EQ ? THEN ipRun = 0.
  RETURN INTEGER(ipMR * 3600 + ipRun * 3600).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION numericDateTime W-Win 
FUNCTION numericDateTime RETURNS DECIMAL
  (ipDate AS DATE,ipTime AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  {{&includes}/numericDateTime.i}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

