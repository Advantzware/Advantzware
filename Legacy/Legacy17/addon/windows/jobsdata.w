&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          jobs             PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: windows/jobsdata.w

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE jobitems AS CHARACTER NO-UNDO.
DEFINE VARIABLE jobitems-save AS CHARACTER NO-UNDO INITIAL 'viewers/jobsfold.w'.
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE jobform NO-UNDO
  FIELD form_number AS INTEGER
        INDEX jobform IS PRIMARY UNIQUE
              form_number.

ldummy = SESSION:SET-WAIT-STATE('').

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Record-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES jobs
&Scoped-define FIRST-EXTERNAL-TABLE jobs


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR jobs.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Close 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_job AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobcad AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobform AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobitems AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobitems-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobmach AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobmatl AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobnotes AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobnotes-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobprep AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobs AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobs-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobsheet AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobsimpt AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobsinfo AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobsinfo-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobsinfo-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobsinfo-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobsinfo-5 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobsinfo-6 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobsinfo-7 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_jobstack AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-jobsdl AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Close 
     LABEL "&Close" 
     SIZE 16 BY 1.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Btn_Close AT ROW 1 COL 135 HELP
          "CLOSE this Window"
     SPACE(0.00) SKIP(22.62)
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: JOBS.jobs
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 8
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "CADCAM Job Load"
         HEIGHT             = 24.24
         WIDTH              = 151.8
         MAX-HEIGHT         = 24.24
         MAX-WIDTH          = 151.8
         VIRTUAL-HEIGHT     = 24.24
         VIRTUAL-WIDTH      = 151.8
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow.i}
{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   Size-to-Fit                                                          */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* CADCAM Job Load */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* CADCAM Job Load */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Close W-Win
ON CHOOSE OF Btn_Close IN FRAME F-Main /* Close */
DO:
  APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/*
if today ge 1/1/2001 then
do:
  message "CADCAM/Scheduling Module has Timed Out." skip
            "Please contact ASI at 215-369-1160." view-as alert-box error.
  APPLY 'CLOSE' TO THIS-PROCEDURE.
  quit.
end.
*/

ON ESC OF {&WINDOW-NAME} ANYWHERE DO:
  APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

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
             INPUT  'FOLDER-LABELS = ':U + 'Jobs|Items|Materials|Machines|Sheet/CAD|Stack Info|Instruction|Preparation' + ',
                     FOLDER-TAB-TYPE = 2':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 1.00 , 1.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 23.81 , 150.00 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_job ).
       RUN set-position IN h_job ( 2.67 , 3.00 ) NO-ERROR.
       RUN set-size IN h_job ( 21.67 , 30.40 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/jobsimpt.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_jobsimpt ).
       RUN set-position IN h_jobsimpt ( 22.67 , 34.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.67 , 78.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/jobs.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_jobs ).
       RUN set-position IN h_jobs ( 2.67 , 34.00 ) NO-ERROR.
       RUN set-size IN h_jobs ( 9.91 , 115.40 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/jobs.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_jobs-2 ).
       RUN set-position IN h_jobs-2 ( 12.67 , 34.00 ) NO-ERROR.
       /* Size in UIB:  ( 9.76 , 115.40 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/v-jobsdl.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-jobsdl ).
       RUN set-position IN h_v-jobsdl ( 22.91 , 116.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 31.00 ) */

       /* Links to SmartBrowser h_jobs. */
       RUN add-link IN adm-broker-hdl ( h_jobs , 'Record':U , THIS-PROCEDURE ).

       /* Links to SmartViewer h_jobs-2. */
       RUN add-link IN adm-broker-hdl ( h_jobs , 'Record':U , h_jobs-2 ).

       /* Links to SmartViewer h_v-jobsdl. */
       RUN add-link IN adm-broker-hdl ( h_jobs , 'Record':U , h_v-jobsdl ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/jobsinfo.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_jobsinfo ).
       RUN set-position IN h_jobsinfo ( 2.67 , 7.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 98.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/jobitems.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_jobitems ).
       RUN set-position IN h_jobitems ( 4.57 , 3.00 ) NO-ERROR.
       RUN set-size IN h_jobitems ( 10.95 , 136.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/jobsfold.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_jobitems-2 ).
       RUN set-position IN h_jobitems-2 ( 16.00 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 8.67 , 143.40 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1') NO-ERROR.

       /* Links to SmartViewer h_jobsinfo. */
       RUN add-link IN adm-broker-hdl ( h_jobs , 'Record':U , h_jobsinfo ).

       /* Links to SmartBrowser h_jobitems. */
       RUN add-link IN adm-broker-hdl ( h_jobs , 'Record':U , h_jobitems ).

       /* Links to SmartViewer h_jobitems-2. */
       RUN add-link IN adm-broker-hdl ( h_jobitems , 'Record':U , h_jobitems-2 ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/jobsinfo.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_jobsinfo-2 ).
       RUN set-position IN h_jobsinfo-2 ( 2.67 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 98.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/jobmatl.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_jobmatl ).
       RUN set-position IN h_jobmatl ( 4.33 , 4.00 ) NO-ERROR.
       RUN set-size IN h_jobmatl ( 19.52 , 143.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1') NO-ERROR.

       /* Links to SmartViewer h_jobsinfo-2. */
       RUN add-link IN adm-broker-hdl ( h_jobs , 'Record':U , h_jobsinfo-2 ).

       /* Links to SmartBrowser h_jobmatl. */
       RUN add-link IN adm-broker-hdl ( h_jobs , 'Record':U , h_jobmatl ).

    END. /* Page 3 */

    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/jobsinfo.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_jobsinfo-3 ).
       RUN set-position IN h_jobsinfo-3 ( 2.67 , 28.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 98.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/jobmach.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_jobmach ).
       RUN set-position IN h_jobmach ( 4.57 , 15.00 ) NO-ERROR.
       RUN set-size IN h_jobmach ( 19.05 , 126.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1') NO-ERROR.

       /* Links to SmartViewer h_jobsinfo-3. */
       RUN add-link IN adm-broker-hdl ( h_jobs , 'Record':U , h_jobsinfo-3 ).

       /* Links to SmartBrowser h_jobmach. */
       RUN add-link IN adm-broker-hdl ( h_jobs , 'Record':U , h_jobmach ).

    END. /* Page 4 */

    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/jobsinfo.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_jobsinfo-4 ).
       RUN set-position IN h_jobsinfo-4 ( 2.67 , 5.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 98.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/jobsheet.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_jobsheet ).
       RUN set-position IN h_jobsheet ( 4.33 , 6.00 ) NO-ERROR.
       RUN set-size IN h_jobsheet ( 8.81 , 139.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/jobcad.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_jobcad ).
       RUN set-position IN h_jobcad ( 13.62 , 6.00 ) NO-ERROR.
       RUN set-size IN h_jobcad ( 10.00 , 66.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1') NO-ERROR.

       /* Links to SmartViewer h_jobsinfo-4. */
       RUN add-link IN adm-broker-hdl ( h_jobs , 'Record':U , h_jobsinfo-4 ).

       /* Links to SmartBrowser h_jobsheet. */
       RUN add-link IN adm-broker-hdl ( h_jobs , 'Record':U , h_jobsheet ).

       /* Links to SmartBrowser h_jobcad. */
       RUN add-link IN adm-broker-hdl ( h_jobs , 'Record':U , h_jobcad ).

    END. /* Page 5 */

    WHEN 6 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/jobsinfo.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_jobsinfo-5 ).
       RUN set-position IN h_jobsinfo-5 ( 2.67 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 98.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/jobstack.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_jobstack ).
       RUN set-position IN h_jobstack ( 4.33 , 4.00 ) NO-ERROR.
       RUN set-size IN h_jobstack ( 20.00 , 143.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1') NO-ERROR.

       /* Links to SmartViewer h_jobsinfo-5. */
       RUN add-link IN adm-broker-hdl ( h_jobs , 'Record':U , h_jobsinfo-5 ).

       /* Links to SmartBrowser h_jobstack. */
       RUN add-link IN adm-broker-hdl ( h_jobs , 'Record':U , h_jobstack ).

    END. /* Page 6 */

    WHEN 7 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/jobsinfo.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_jobsinfo-6 ).
       RUN set-position IN h_jobsinfo-6 ( 8.86 , 28.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 98.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/jobform.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_jobform ).
       RUN set-position IN h_jobform ( 10.52 , 14.00 ) NO-ERROR.
       RUN set-size IN h_jobform ( 6.71 , 12.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/jobnotes.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_jobnotes ).
       RUN set-position IN h_jobnotes ( 10.52 , 28.00 ) NO-ERROR.
       RUN set-size IN h_jobnotes ( 6.71 , 16.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/jobnotes.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_jobnotes-2 ).
       RUN set-position IN h_jobnotes-2 ( 10.52 , 45.00 ) NO-ERROR.
       /* Size in UIB:  ( 6.71 , 81.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1') NO-ERROR.

       /* Links to SmartViewer h_jobsinfo-6. */
       RUN add-link IN adm-broker-hdl ( h_jobs , 'Record':U , h_jobsinfo-6 ).

       /* Links to SmartBrowser h_jobform. */
       RUN add-link IN adm-broker-hdl ( h_jobsinfo-6 , 'Record':U , h_jobform ).

       /* Links to SmartBrowser h_jobnotes. */
       RUN add-link IN adm-broker-hdl ( h_jobform , 'Record':U , h_jobnotes ).

       /* Links to SmartViewer h_jobnotes-2. */
       RUN add-link IN adm-broker-hdl ( h_jobnotes , 'Record':U , h_jobnotes-2 ).

    END. /* Page 7 */

    WHEN 8 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/jobsinfo.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_jobsinfo-7 ).
       RUN set-position IN h_jobsinfo-7 ( 3.62 , 17.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 98.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/jobprep.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_jobprep ).
       RUN set-position IN h_jobprep ( 6.00 , 13.00 ) NO-ERROR.
       RUN set-size IN h_jobprep ( 17.86 , 129.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1') NO-ERROR.

       /* Links to SmartViewer h_jobsinfo-7. */
       RUN add-link IN adm-broker-hdl ( h_jobs , 'Record':U , h_jobsinfo-7 ).

       /* Links to SmartBrowser h_jobprep. */
       RUN add-link IN adm-broker-hdl ( h_jobs , 'Record':U , h_jobprep ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 8 */

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
  {src/adm/template/row-list.i "jobs"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "jobs"}

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
  ENABLE Btn_Close 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Job_Load W-Win 
PROCEDURE Job_Load :
/*------------------------------------------------------------------------------
  Purpose:     Load Selected Job into Jobs Database
  Parameters:  <none>
  notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER option-selected AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER job-rowid AS ROWID NO-UNDO.
  DEFINE OUTPUT PARAMETER jobs-rowid AS ROWID NO-UNDO.

  DEFINE VARIABLE job-num AS CHARACTER NO-UNDO.
  def buffer bf-job-hdr for job-hdr.
  def buffer bf-ord for oe-ord.
  FIND optconfg WHERE optconfg.description = option-selected NO-LOCK.
  FIND job WHERE ROWID(job) = job-rowid NO-LOCK.
  /* ================ need to check order's status ============ */
  find first oe-ctrl where oe-ctrl.company = job.company no-lock no-error.
  if oe-ctrl.p-fact = no then do:                        
     for each bf-job-hdr no-lock where bf-job-hdr.company = job.company
                                AND bf-job-hdr.job = job.job
                                AND bf-job-hdr.job-no = job.job-no
                                AND bf-job-hdr.job-no2 = job.job-no2
                                 and bf-job-hdr.ord-no <> 0:
         find bf-ord where bf-ord.company = bf-job-hdr.company
                    and bf-ord.ord-no = bf-job-hdr.ord-no no-lock no-error.
         if avail bf-ord and bf-ord.stat = "H" then do:  /* on hold */
            message "Order " bf-ord.ord-no " is on Hold. " view-as alert-box error.
            return error.
         end.                    
     end. 
  end.
  /* =========== end of status check ==============*/
  job-num = job.job-no + '-' + STRING(job.job-no2).

  FIND jobs WHERE jobs.job = job-num EXCLUSIVE-LOCK NO-ERROR.
  IF AVAILABLE jobs THEN
  DO:
    CASE optconfg.loadtype:
      WHEN 'BOTH' THEN
      ASSIGN
        jobs.cadcam_status = 'Pending'
        jobs.scheduling_status = 'Pending'.
      WHEN 'CADCAM' THEN
      jobs.cadcam_status = 'Pending'.
      WHEN 'SCHEDULING' THEN
      jobs.scheduling_status = 'Pending'.
    END CASE.
    FOR EACH jobcad OF jobs EXCLUSIVE-LOCK:
      DELETE jobcad.
    END.
    FOR EACH jobitems OF jobs EXCLUSIVE-LOCK:
      DELETE jobitems.
    END.
    FOR EACH jobmach OF jobs EXCLUSIVE-LOCK:
      DELETE jobmach.
    END.
    FOR EACH jobmatl OF jobs EXCLUSIVE-LOCK:
      DELETE jobmatl.
    END.
    FOR EACH jobnotes OF jobs EXCLUSIVE-LOCK:
      DELETE jobnotes.
    END.
    FOR EACH jobsheet OF jobs EXCLUSIVE-LOCK:
      DELETE jobsheet.
    END.
    FOR EACH jobstack OF jobs EXCLUSIVE-LOCK:
      DELETE jobstack.
    END.
    /* ========  YSK 05/03/01 */
    for each jobprep of jobs exclusive-lock:
      delete jobprep.
    end.
  END.
  FOR EACH ef NO-LOCK WHERE ef.company = job.company AND ef.est-no = job.est-no:
    FOR EACH eb OF ef NO-LOCK:
      FIND job-hdr WHERE job-hdr.company = job.company
                     AND job-hdr.job = job.job
                     AND job-hdr.job-no = job.job-no
                     AND job-hdr.job-no2 = job.job-no2
                     AND job-hdr.frm = eb.form-no
                     AND job-hdr.blank-no = eb.blank-no
                   NO-LOCK NO-ERROR.
      IF NOT AVAILABLE job-hdr THEN
      FIND job-hdr WHERE job-hdr.company = job.company
                     AND job-hdr.job = job.job
                     AND job-hdr.job-no = job.job-no
                     AND job-hdr.job-no2 = job.job-no2
                     AND job-hdr.frm = eb.form-no
                   NO-LOCK NO-ERROR.
      IF NOT AVAILABLE job-hdr THEN
      FIND job-hdr WHERE job-hdr.company = job.company
                     AND job-hdr.job = job.job
                     AND job-hdr.job-no = job.job-no
                     AND job-hdr.job-no2 = job.job-no2
                   NO-LOCK NO-ERROR.
      IF NOT AVAILABLE job-hdr THEN
      NEXT.
      FIND FIRST est WHERE est.company = job.company
                       AND est.est-no = job.est-no NO-LOCK NO-ERROR.
      IF NOT AVAILABLE est THEN
      NEXT.
      FIND FIRST itemfg WHERE itemfg.company = job.company
                          AND itemfg.i-no = job-hdr.i-no NO-LOCK NO-ERROR.
      IF NOT AVAILABLE itemfg THEN
      NEXT.
      FIND cust OF job-hdr NO-LOCK NO-ERROR.
      IF NOT AVAILABLE cust THEN
      NEXT.
      IF job-hdr.ord-no NE 0 THEN
      FIND FIRST oe-ordl NO-LOCK
          WHERE oe-ordl.company EQ job-hdr.company
            AND oe-ordl.ord-no  EQ job-hdr.ord-no
            AND oe-ordl.i-no    EQ job-hdr.i-no
          NO-ERROR.
      IF job-hdr.ord-no NE 0 THEN
      FIND FIRST oe-ord NO-LOCK
          WHERE oe-ord.company EQ job-hdr.company
            AND oe-ord.ord-no  EQ job-hdr.ord-no
          NO-ERROR.
      IF NOT AVAILABLE jobs THEN
      DO:
        CREATE jobs.
        ASSIGN
          jobs.job = job-num
          jobs.estimate = job-hdr.est-no
          jobs.estimate_type = est.est-type
          jobs.customer = job-hdr.cust-no
          jobs.name = cust.name
          jobs.address1 = cust.addr[1]
          jobs.address2 = cust.addr[2]
          jobs.city = cust.city
          jobs.state = cust.state
          jobs.zip = cust.zip
          jobs.ship_name = eb.ship-name
          jobs.ship_address1 = eb.ship-addr[1]
          jobs.ship_address2 = eb.ship-addr[2]
          jobs.ship_city = eb.ship-city
          jobs.ship_state = eb.ship-state
          jobs.ship_zip = eb.ship-zip
          jobs.salesrep = IF AVAILABLE oe-ord THEN oe-ord.sman[1] + ',' +
                                                   oe-ord.sman[2] + ',' +
                                                   oe-ord.sman[3]
                          ELSE cust.sman
          jobs.overrun = IF AVAIL oe-ordl THEN oe-ordl.over-pct ELSE
                         IF AVAIL oe-ord  THEN oe-ord.over-pct  ELSE 0
          jobs.underrun = IF AVAIL oe-ordl THEN oe-ordl.under-pct ELSE
                          IF AVAIL oe-ord  THEN oe-ord.under-pct  ELSE 0
          jobs.date_entered = job.start-date
          jobs.run_number = IF est.est-type = 3 THEN job-hdr.blank-no
                            ELSE job-hdr.frm.
        CASE optconfg.loadtype:
          WHEN 'BOTH' THEN
          ASSIGN
            jobs.cadcam_status = 'Pending'
            jobs.scheduling_status = 'Pending'.
          WHEN 'CADCAM' THEN
          jobs.cadcam_status = 'Pending'.
          WHEN 'SCHEDULING' THEN
          jobs.scheduling_status = 'Pending'.
        END CASE.
      END. /* not avail jobs */
      FIND style OF eb NO-LOCK NO-ERROR.
      IF AVAILABLE style THEN
      DO:
        FIND FIRST oe-ordl
             WHERE oe-ordl.company = job.company
               AND oe-ordl.ord-no = job-hdr.ord-no
               AND oe-ordl.job-no = job-hdr.job-no
               AND oe-ordl.job-no2 = job-hdr.job-no2
               AND oe-ordl.i-no = job-hdr.i-no
             NO-LOCK NO-ERROR.
        CREATE jobitems.

        ASSIGN
          jobitems.job = job.job-no + '-' + STRING(job.job-no2)
          jobitems.form_number = eb.form-no
          jobitems.blank_number = eb.blank-no
          /* ========= change for corrugate box set is below ======
          jobitems.fg_item = job-hdr.i-no  /* ysk*/
          jobitems.part_number = IF eb.part-no = '' AND
                                    AVAILABLE itemfg THEN itemfg.part-no
             ELSE IF AVAILABLE oe-ordl AND oe-ordl.part-no NE '' THEN oe-ordl.part-no
             ELSE eb.part-no
          jobitems.description = IF AVAILABLE itemfg THEN itemfg.i-name
                                 ELSE eb.part-dscr1
          ========================*/                       
          jobitems.item_length = eb.len
          jobitems.item_width = eb.wid
          jobitems.item_depth = eb.dep
          jobitems.style = eb.style
          jobitems.style_description = IF AVAILABLE style THEN style.dscr ELSE ''
          jobitems.po = IF AVAILABLE oe-ordl THEN oe-ordl.po-no ELSE ''
          jobitems.order_number = job-hdr.ord-no
          jobitems.number_up = eb.num-up
          jobitems.qty = job-hdr.qty
          jobitems.spc_qa = eb.spc-no
          jobitems.upc = eb.upc-no
          jobitems.due_date = IF AVAILABLE oe-ordl THEN oe-ordl.prom-date ELSE ?
          jobitems.square_inch = job-hdr.sq-in
          jobitems.blank_length = eb.t-len
          jobitems.blank_width = eb.t-wid
          jobitems.lock_tab = eb.lock
          jobitems.tuck = eb.tuck.
          /* ========== change for set ===========*/
          if est.est-type <> 6 then 
             assign jobitems.fg_item = job-hdr.i-no  
                    jobitems.part_number = IF eb.part-no = '' AND
                                      AVAILABLE itemfg THEN itemfg.part-no
                                      ELSE IF AVAILABLE oe-ordl AND oe-ordl.part-no NE '' THEN oe-ordl.part-no
                                      ELSE eb.part-no
                    jobitems.description = IF AVAILABLE itemfg THEN itemfg.i-name
                                           ELSE eb.part-dscr1
                                           .
          else assign jobitems.fg_item = eb.stock-no
                      jobitems.part_number = if eb.part-no = '' AND AVAILABLE itemfg THEN itemfg.part-no
                                             ELSE eb.part-no
                      jobitems.description = if eb.part-dscr1 = "" and AVAILABLE itemfg THEN itemfg.i-name
                                             ELSE eb.part-dscr1                                 
                      .          

        DO i = 1 TO EXTENT(jobitems.width_panels):
          ASSIGN
            jobitems.width_panels[i] = eb.k-wid-array2[i]
            jobitems.length_panels[i] = eb.k-len-array2[i].
        END.
        IF est.est-type LE 4 THEN
        ASSIGN
          jobitems.adhesive = eb.adhesive
          jobitems.dust_flap = eb.dust
          jobitems.fifth_panel = eb.fpanel
          jobitems.glue_lap = eb.gluelap
          jobitems.double_knife_length = eb.k-len
          jobitems.double_knife_width = eb.k-wid
          jobitems.linear_inches = eb.lin-in
          jobitems.blank_sq_inches = eb.t-sqin.
        ELSE /* est-type GE 5 */
        ASSIGN
          jobitems.joint_material = eb.adhesive
          jobitems.top_dust_flap = eb.dust
          jobitems.bottom_flap = eb.fpanel
          jobitems.joint_tab_width = eb.gluelap
          jobitems.scores_on_length = eb.k-len
          jobitems.scores_on_width = eb.k-wid
          jobitems.joint_length = eb.lin-in
          jobitems.blank_sq_feet = eb.t-sqin / 144.
      END. /* avail stype */

      FOR EACH job-mat NO-LOCK
          WHERE job-mat.company = job-hdr.company
            AND job-mat.job = job-hdr.job
            AND job-mat.job-no = job-hdr.job-no
            AND job-mat.job-no2 = job-hdr.job-no2
            AND job-mat.frm = eb.form-no,
        FIRST item NO-LOCK
             WHERE item.company = job-mat.company
               AND item.i-no = job-mat.i-no:
        IF NOT CAN-FIND(jobmatl
               WHERE jobmatl.job = job-num
                 AND jobmatl.form_number = eb.form-no
                 AND jobmatl.blank_number = eb.blank-no
                 AND jobmatl.item_number = job-mat.i-no) THEN
        DO:
          CREATE jobmatl.
          ASSIGN
            jobmatl.job = job-num
            jobmatl.form_number = eb.form-no
            jobmatl.blank_number = eb.blank-no
            jobmatl.item_number = job-mat.i-no
            jobmatl.item_description = item.i-name
            jobmatl.qty = job-mat.qty
            jobmatl.qty_uom = job-mat.qty-uom
            jobmatl.item_length = job-mat.len
            jobmatl.item_width = job-mat.wid
            jobmatl.number_up = job-mat.n-up
            jobmatl.basis_weight = job-mat.basis-w
            jobmatl.material_type = item.mat-type.
          IF item.mat-type = 'b' AND NOT CAN-FIND(jobsheet
                 WHERE jobsheet.job = job-num
                   AND jobsheet.form_number = eb.form-no
                   AND jobsheet.blank_number = eb.blank-no
                   AND jobsheet.item_number = job-mat.i-no) THEN
          DO:
            FIND FIRST job-mch
                 WHERE job-mch.company = job-hdr.company
                   AND job-mch.job = job-hdr.job
                   AND job-mch.job-no = job-hdr.job-no
                   AND job-mch.job-no2 = job-hdr.job-no2
                   AND job-mch.frm = ef.form-no
                 NO-LOCK NO-ERROR.
            CREATE jobsheet.
            ASSIGN
              jobsheet.job = job-num
              jobsheet.form_number = eb.form-no
              jobsheet.blank_number = eb.blank-no
              jobsheet.item_number = job-mat.i-no
              jobsheet.sheet_width = ef.gsh-wid
              jobsheet.sheet_length = ef.gsh-len
              jobsheet.number_sheets = IF AVAILABLE job-mch THEN job-mch.run-qty
                                       ELSE ?
              jobsheet.caliper = ef.cal
              jobsheet.roll_width = ef.roll-wid
              jobsheet.number_cuts = ef.n-cuts
              jobsheet.die_number = IF AVAILABLE eb THEN eb.die-no ELSE ?
              jobsheet.die_size = ef.die-in.
          END. /* mat-type = 'b' */
        END. /* not can-find */
      END. /* each job-mat */
      FOR EACH job-mch NO-LOCK
          WHERE job-mch.company = job-hdr.company
            AND job-mch.job = job-hdr.job
            AND job-mch.job-no = job-hdr.job-no
            AND job-mch.job-no2 = job-hdr.job-no2
            AND job-mch.frm = eb.form-no:
        IF NOT CAN-FIND(jobmach
             WHERE jobmach.job = job-num
               AND jobmach.sequence = job-mch.line) THEN
        DO:
          FIND FIRST mach WHERE mach.company = job-mch.company
                            AND mach.m-code = job-mch.m-code NO-LOCK NO-ERROR.
          CREATE jobmach.
          ASSIGN
            jobmach.job = job-num
            jobmach.form_number = job-mch.frm
            jobmach.blank_number = job-mch.blank-no
            jobmach.sequence = job-mch.line
            jobmach.machine = job-mch.m-code
            jobmach.description = IF AVAILABLE mach THEN mach.m-dscr ELSE ''
            jobmach.department = job-mch.dept
            jobmach.units = job-mch.run-qty
            jobmach.make_ready = job-mch.mr-hr
            jobmach.waste = job-mch.mr-waste
            jobmach.run_time = job-mch.run-hr
            jobmach.speed = job-mch.speed
            jobmach.pass = job-mch.pass.
        END.
      END. /* each job-mch */
      /* =========*/
      for each job-prep no-lock where job-prep.company = job-hdr.company
                                  and job-prep.job = job-hdr.job
                                  and job-prep.job-no = job-hdr.job-no
                                  and job-prep.job-no2 = job-hdr.job-no2
                                  and job-prep.frm = eb.form-no
                                  :
          if not can-find(first jobprep WHERE jobprep.job = job-num
                                   and jobprep.form_number = job-prep.frm
                                   and jobprep.blank_number = job-prep.blank-no
                                   AND jobprep.code = job-prep.code) THEN
          DO:
              create jobprep.
              assign jobprep.job = job-num
                     jobprep.form_number = job-prep.frm
                     jobprep.blank_number = job-prep.blank-no
                     jobprep.code = job-prep.code
                     jobprep.std-cost = job-prep.std-cost
                     jobprep.ml = job-prep.ml
                     jobprep.simon = job-prep.simon
                     jobprep.cost-m = job-prep.cost-m
                     jobprep.qty = job-prep.qty
                     .
          end.                          
      end.  
      /* ============= */                            
      IF eb.cad-no NE '' AND NOT CAN-FIND(jobcad
          WHERE jobcad.job = job-num
            AND jobcad.form_number = eb.form-no
            AND jobcad.blank_number = eb.blank-no
            AND jobcad.cad_number = eb.cad-no) THEN
      DO:
        CREATE jobcad.
        ASSIGN
          jobcad.job = job-num
          jobcad.form_number = eb.form-no
          jobcad.blank_number = eb.blank-no
          jobcad.cad_number = eb.cad-no.
      END.
      FIND FIRST carrier
           WHERE carrier.company = job.company
             AND carrier.carrier = eb.carrier
           NO-LOCK NO-ERROR.
      CREATE jobstack.
      ASSIGN
        jobstack.job = job-num
        jobstack.form_number = eb.form-no
        jobstack.blank_number = eb.blank-no
        jobstack.shipvia = eb.carrier
        jobstack.shipvia_description = IF AVAILABLE carrier THEN carrier.dscr
                                       ELSE ''
        jobstack.bundles = eb.cas-pal
        jobstack.ties = IF eb.tr-cnt GT 0 THEN job-hdr.qty / eb.tr-cnt ELSE 0
        jobstack.stacks = eb.stacks
        jobstack.layers = eb.tr-cas
        jobstack.pattern = eb.stack-code
        jobstack.unit_count = eb.tr-cnt
        jobstack.bands = 0
        jobstack.tags = IF eb.tr-cnt GT 0 THEN job-hdr.qty / eb.tr-cnt + 1
                        ELSE 0.
    END. /* each eb */
  END. /* each ef */
  FOR EACH est-inst NO-LOCK
      WHERE est-inst.est-no = job.est-no:
    /* ===  07/10/01  YSK exclude note for a department if sys-ctrl.char-fld <> "" */
    find first sys-ctrl where sys-ctrl.company = est-inst.company and
                              sys-ctrl.name = "NOTES"
                              no-lock no-error.
    if avail sys-ctrl and sys-ctrl.char-fld <> "" and
       can-do(sys-ctrl.char-fld,est-inst.dept) 
       then next.
    /* ============ end of mods =================*/                               
    FIND first jobnotes
         WHERE jobnotes.job = job.job-no + '-' + STRING(job.job-no2)
           and jobnotes.line-no = est-inst.line-no   /* YSK 05/03/01*/
           AND jobnotes.department = est-inst.dept
         EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE jobnotes THEN
    DO:
      CREATE jobnotes.
      ASSIGN
        jobnotes.job = job.job-no + '-' + STRING(job.job-no2)
        jobnotes.line-no = est-inst.line-no
        jobnotes.department = est-inst.dept.
    END.
    jobnotes.note = ''.
    DO i = 1 TO EXTENT(est-inst.inst):
      IF est-inst.inst[i] NE '' THEN
      jobnotes.note = jobnotes.note + TRIM(est-inst.inst[i]) + CHR(10).
    END.
  END. /* each est-inst */

  IF optconfg.prgmname NE '' THEN
  DO:
    FIND prgrms WHERE prgrms.prgmname = optconfg.prgmname NO-LOCK NO-ERROR.
    IF AVAILABLE prgrms AND
      (SEARCH(prgrms.dir_group + '\' + optconfg.prgmname + 'r') NE ? OR
       SEARCH(prgrms.dir_group + '\' + optconfg.prgmname + 'p') NE ?) THEN
    RUN VALUE(prgrms.dir_group + '\' + optconfg.prgmname + 'p')
        (option-selected,ROWID(jobs),ROWID(job)).
    ELSE
    MESSAGE '"' + optconfg.description '" program "' + optconfg.prgmname + '"'
            'is not implemented.' VIEW-AS ALERT-BOX.
  END.
  jobs-rowid = ROWID(jobs).

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'change-page':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  IF INTEGER(RETURN-VALUE) = 2 AND AVAILABLE jobs THEN
  DO:
    jobitems = IF jobs.estimate_type LE 4 THEN 'viewers/jobsfold.w'
               ELSE 'viewers/jobscorr.w'.
    IF jobitems NE jobitems-save THEN
    DO:
      jobitems-save = jobitems.
      RUN dispatch IN h_jobitems-2 ('destroy').
      RUN init-object IN THIS-PROCEDURE (
            INPUT jobitems ,
            INPUT  FRAME F-Main:HANDLE ,
            INPUT  'Layout = ':U ,
            OUTPUT h_jobitems-2 ).
      RUN set-position IN h_jobitems-2 ( 16.00 , 4.00 ) NO-ERROR.
      RUN add-link IN adm-broker-hdl (h_jobitems,'RECORD',h_jobitems-2).
      RUN dispatch IN h_jobitems-2 ('initialize').
    END.
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Open_Jobs_Query W-Win 
PROCEDURE Open_Jobs_Query :
/*------------------------------------------------------------------------------
  Purpose:     open jobs query in browers/jobs.w
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER jobs-rowid AS ROWID NO-UNDO.

  RUN Position_Jobs IN h_jobs (jobs-rowid).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Open_Job_Query W-Win 
PROCEDURE Open_Job_Query :
/*------------------------------------------------------------------------------
  Purpose:     open job query in browers/job.w
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN Build_TTBL IN h_job.

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
  {src/adm/template/snd-list.i "jobs"}

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

