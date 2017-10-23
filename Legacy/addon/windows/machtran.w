&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          emptrack         PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: windows/machtran.w
          
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


DEFINE VARIABLE company_code AS CHARACTER NO-UNDO.
DEFINE VARIABLE machine_code AS CHARACTER NO-UNDO.
DEFINE VARIABLE job_number AS CHARACTER NO-UNDO.
DEFINE VARIABLE job_sub AS CHARACTER NO-UNDO.
DEFINE VARIABLE form_number AS CHARACTER NO-UNDO.
DEFINE VARIABLE blank_number AS CHARACTER NO-UNDO.
DEFINE VARIABLE pass_sequence AS CHARACTER NO-UNDO.
DEFINE VARIABLE label_language AS CHARACTER NO-UNDO.

DEFINE VARIABLE hCaller AS HANDLE NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES machtran
&Scoped-define FIRST-EXTERNAL-TABLE machtran


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR machtran.
/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_machemp AS HANDLE NO-UNDO.
DEFINE VARIABLE h_machemp-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_machtra2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_machtran AS HANDLE NO-UNDO.
DEFINE VARIABLE h_machtran-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_machtran-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_miscfldsad AS HANDLE NO-UNDO.
DEFINE VARIABLE h_options AS HANDLE NO-UNDO.
DEFINE VARIABLE h_optnote-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_smartmsg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_wiptag AS HANDLE NO-UNDO.
DEFINE VARIABLE h_export AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 150 BY 24
         BGCOLOR 15 .

DEFINE FRAME message-frame
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 90 ROW 2.91
         SIZE 61 BY 1.43
         BGCOLOR 15 .

DEFINE FRAME OPTIONS-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 1
         SIZE 148 BY 1.91
         BGCOLOR 15 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: machtran
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
         TITLE              = "Machine Transactions"
         HEIGHT             = 24
         WIDTH              = 150
         MAX-HEIGHT         = 24
         MAX-WIDTH          = 150
         VIRTUAL-HEIGHT     = 24
         VIRTUAL-WIDTH      = 150
         RESIZE             = NO
         SCROLL-BARS        = NO
         STATUS-AREA        = YES
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT W-Win:LOAD-ICON("adeicon\progress":U) THEN
    MESSAGE "Unable to load icon: adeicon\progress"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{methods/template/windows.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME message-frame:FRAME = FRAME F-Main:HANDLE
       FRAME OPTIONS-FRAME:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME OPTIONS-FRAME:MOVE-BEFORE-TAB-ITEM (FRAME message-frame:HANDLE)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FRAME message-frame
                                                                        */
/* SETTINGS FOR FRAME OPTIONS-FRAME
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME message-frame
/* Query rebuild information for FRAME message-frame
     _Query            is NOT OPENED
*/  /* FRAME message-frame */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME OPTIONS-FRAME
/* Query rebuild information for FRAME OPTIONS-FRAME
     _Query            is NOT OPENED
*/  /* FRAME OPTIONS-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Machine Transactions */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Machine Transactions */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */

  RUN exit-procedure.

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}


&SCOPED-DEFINE sysCtrlCompany company_code
&SCOPED-DEFINE sysCtrlName MiscJobCL
&SCOPED-DEFINE mfRecKey company_code + "|jh" + STRING(job-hdr.j-no)
&SCOPED-DEFINE mfHeader " Job: " + job-hdr.job-no + "-" + STRING(job-hdr.job-no2) + " Item#: " + job-hdr.i-no
{methods/miscfldsad.i}

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
             INPUT  'smartobj/smartmsg.w':U ,
             INPUT  FRAME message-frame:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_smartmsg ).
       RUN set-position IN h_smartmsg ( 1.00 , 30.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.14 , 32.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/options.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_options ).
       RUN set-position IN h_options ( 1.00 , 85.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 55.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Browse Trans|View Trans|Employees|Posted Trans' + ',
                     FOLDER-TAB-TYPE = 1':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 3.14 , 2.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 21.67 , 148.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/optnote.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_optnote-2 ).
       RUN set-position IN h_optnote-2 ( 1.00 , 93.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/exit.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.00 , 141.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/miscfldsad.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_miscfldsad ).
       RUN set-position IN h_miscfldsad ( 1.05 , 76.80 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_optnote-2 ,
             h_options , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_folder ,
             FRAME message-frame:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_exit ,
             h_optnote-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_miscfldsad ,
             h_exit , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/export.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_export ).
       RUN set-position IN h_export ( 1.05 , 61.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/machtran.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_machtran ).
       RUN set-position IN h_machtran ( 4.76 , 3.60 ) NO-ERROR.
       RUN set-size IN h_machtran ( 19.52 , 144.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/wiptag.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_wiptag ).
       RUN set-position IN h_wiptag ( 1.00 , 69.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       /* Links to SmartViewer h_export. */
       RUN add-link IN adm-broker-hdl ( h_machtran , 'export-xl':U , h_export ).

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartNavBrowser h_machtran. */
       RUN add-link IN adm-broker-hdl ( h_p-navico , 'Navigation':U , h_machtran ).
       RUN add-link IN adm-broker-hdl ( h_wiptag , 'wiptag':U , h_machtran ).
       RUN add-link IN adm-broker-hdl ( h_machtran , 'Record':U , THIS-PROCEDURE ).

    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/machtran.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_machtran-2 ).
       RUN set-position IN h_machtran-2 ( 8.14 , 19.00 ) NO-ERROR.
       /* Size in UIB:  ( 7.38 , 115.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-navico.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navico ).
       RUN set-position IN h_p-navico ( 16.95 , 19.00 ) NO-ERROR.
       RUN set-size IN h_p-navico ( 2.14 , 38.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav ).
       RUN set-position IN h_p-updsav ( 16.95 , 77.00 ) NO-ERROR.
       RUN set-size IN h_p-updsav ( 2.14 , 57.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_machtran-2. */
       RUN add-link IN adm-broker-hdl ( h_machtran , 'Record':U , h_machtran-2 ).
       RUN add-link IN adm-broker-hdl ( h_p-updsav , 'TableIO':U , h_machtran-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_machtran-2 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navico ,
             h_machtran-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updsav ,
             h_p-navico , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/export.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_export ).
       RUN set-position IN h_export ( 1.05 , 69.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewerid/machtran.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_machtran-3 ).
       RUN set-position IN h_machtran-3 ( 4.57 , 3.80 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 140.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/machemp.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_machemp ).
       RUN set-position IN h_machemp ( 6.71 , 4.00 ) NO-ERROR.
       RUN set-size IN h_machemp ( 10.00 , 144.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/machemp.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_machemp-2 ).
       RUN set-position IN h_machemp-2 ( 16.95 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 5.00 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav-3 ).
       RUN set-position IN h_p-updsav-3 ( 22.43 , 64.00 ) NO-ERROR.
       RUN set-size IN h_p-updsav-3 ( 2.14 , 82.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_export. */
       RUN add-link IN adm-broker-hdl ( h_machemp , 'export-xl':U , h_export ).

       /* Links to SmartViewer h_machtran-3. */
       RUN add-link IN adm-broker-hdl ( h_machtran , 'Record':U , h_machtran-3 ).

       /* Links to SmartNavBrowser h_machemp. */
       RUN add-link IN adm-broker-hdl ( h_machtran , 'Record':U , h_machemp ).

       /* Links to SmartViewer h_machemp-2. */
       RUN add-link IN adm-broker-hdl ( h_machemp , 'Record':U , h_machemp-2 ).
       RUN add-link IN adm-broker-hdl ( h_p-updsav-3 , 'TableIO':U , h_machemp-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_machemp ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_machemp-2 ,
             h_machemp , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updsav-3 ,
             h_machemp-2 , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/export.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_export ).
       RUN set-position IN h_export ( 1.05 , 69.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/machtra2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_machtra2 ).
       RUN set-position IN h_machtra2 ( 4.81 , 4.00 ) NO-ERROR.
       RUN set-size IN h_machtra2 ( 19.52 , 144.00 ) NO-ERROR.

        /* Links to SmartViewer h_export. */
       RUN add-link IN adm-broker-hdl ( h_machtra2 , 'export-xl':U , h_export ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_machtra2 ,
             h_folder , 'AFTER':U ).
    END. /* Page 4 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page EQ 0 
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
  {src/adm/template/row-list.i "machtran"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "machtran"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Disable-Folder-Tabs W-Win 
PROCEDURE Disable-Folder-Tabs :
/*------------------------------------------------------------------------------
  Purpose:     Disable folder tabs when running Job Notes from Data Collection menu.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER phCaller AS HANDLE NO-UNDO.

  ASSIGN hCaller = phCaller.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE, "page-source", OUTPUT char-hdl).
  RUN disable-folder-page IN WIDGET-HANDLE(char-hdl) (INPUT 2) NO-ERROR.
  RUN disable-folder-page IN WIDGET-HANDLE(char-hdl) (INPUT 3) NO-ERROR.
  RUN disable-folder-page IN WIDGET-HANDLE(char-hdl) (INPUT 4) NO-ERROR.

  /* Disable paging functionality in browser. */
  RUN disable-pages IN h_machtran.


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
  VIEW FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW FRAME OPTIONS-FRAME IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-OPTIONS-FRAME}
  VIEW FRAME message-frame IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-message-frame}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exit-procedure W-Win 
PROCEDURE exit-procedure :
/*------------------------------------------------------------------------------
  Purpose:     Instruct Data Collection menu to destroy its persistent handle.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  IF VALID-HANDLE(hCaller) THEN
      RUN Destroy-JobNotes IN hCaller NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get-Values W-Win 
PROCEDURE Get-Values :
/*------------------------------------------------------------------------------
  Purpose:     Set value for calling viewer
  Parameters:  OUTPUT company
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER op-company AS CHARACTER NO-UNDO.

  op-company = IF AVAILABLE machtran THEN machtran.company ELSE ''.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Label_Language W-Win 
PROCEDURE Label_Language :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipHandle AS HANDLE NO-UNDO.

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

  RUN GET-ATTRIBUTE ('current-page').
  IF int(RETURN-VALUE) = 10 THEN DO:
     DEF VAR machtran_rec_key AS cha NO-UNDO.
     RUN get-machtran IN h_machtra2 (OUTPUT machtran_rec_key).     
     RUN addon/touch/d-mchemp2.w (machtran_rec_key).
     RUN select-page ('4').
     RETURN .
  END.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'change-page':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy W-Win 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  RUN exit-procedure.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable W-Win 
PROCEDURE local-disable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  RUN exit-procedure.

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

   RUN exit-procedure.

   APPLY "CLOSE":U TO THIS-PROCEDURE.

   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selectMiscFlds W-Win 
PROCEDURE selectMiscFlds :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN sysCtrlMiscFlds (OUTPUT miscFlds).
  IF NOT miscFlds THEN RETURN.

  IF AVAIL machtran AND machtran.job_number <> "" THEN
  DO:
     FIND FIRST job-hdr WHERE
          job-hdr.company EQ machtran.company AND
          job-hdr.job-no = machtran.job_number AND
          job-hdr.job-no2 = machtran.job_sub AND
          job-hdr.frm = machtran.form_number AND
          job-hdr.blank-no = machtran.blank_number
          NO-LOCK NO-ERROR.

     IF NOT AVAIL job-hdr THEN
        FIND FIRST job-hdr WHERE
             job-hdr.company = machtran.company AND
             job-hdr.job-no = machtran.job_number AND
             job-hdr.job-no2 = machtran.job_sub AND
             job-hdr.frm = machtran.form_number
             NO-LOCK NO-ERROR.

  END.
  ELSE
  DO:
     FIND FIRST job-hdr WHERE
          job-hdr.company EQ company_code AND
          job-hdr.job-no = job_number AND
          job-hdr.job-no2 = INT(job_sub) AND
          job-hdr.frm = INT(form_number) AND
          job-hdr.blank-no = INT(blank_number)
          NO-LOCK NO-ERROR.

     IF NOT AVAIL job-hdr THEN
        FIND FIRST job-hdr WHERE
             job-hdr.company EQ company_code AND
             job-hdr.job-no = job_number AND
             job-hdr.job-no2 = INT(job_sub) AND
             job-hdr.frm = INT(form_number)
             NO-LOCK NO-ERROR.

  END.


     IF AVAIL job-hdr THEN
       RUN UDF/mfvalues.w (sys-ctrl.char-fld,{&mfRecKey},{&mfHeader},h_smartmsg).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select_dept2 W-Win 
PROCEDURE select_dept2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF AVAIL machtran AND machtran.job_number <> "" THEN DO:     
     
     FIND FIRST job WHERE job.company EQ machtran.company AND
                          job.job-no = machtran.job_number AND
                          job.job-no2 = machtran.job_sub NO-LOCK NO-ERROR.

     IF AVAIL job THEN
     DO:        
       
        rec_key_value = job.rec_key.
       
        RUN windows/specnott.w (rec_key_value,HEADER_value,machtran.machine,machtran.form_number).       
        
     END.
  END.
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
  {src/adm/template/snd-list.i "machtran"}

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

