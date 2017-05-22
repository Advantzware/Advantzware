&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          emptrack         PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: windows/employee.w

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
&Scoped-define EXTERNAL-TABLES employee
&Scoped-define FIRST-EXTERNAL-TABLE employee


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR employee.
/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_address AS HANDLE NO-UNDO.
DEFINE VARIABLE h_allnote2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_emplogin AS HANDLE NO-UNDO.
DEFINE VARIABLE h_emplogin-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_employee AS HANDLE NO-UNDO.
DEFINE VARIABLE h_employee-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_employee-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_employee-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_employee-5 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_empmach AS HANDLE NO-UNDO.
DEFINE VARIABLE h_empmach-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_export AS HANDLE NO-UNDO.
DEFINE VARIABLE h_export-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_notetext AS HANDLE NO-UNDO.
DEFINE VARIABLE h_options AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_phone AS HANDLE NO-UNDO.
DEFINE VARIABLE h_rate AS HANDLE NO-UNDO.
DEFINE VARIABLE h_rate-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_smartmsg AS HANDLE NO-UNDO.

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
         AT COL 88 ROW 2.91
         SIZE 63 BY 1.43
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
   External Tables: employee
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
         TITLE              = "Employee Tracking and Data Collection"
         HEIGHT             = 24
         WIDTH              = 150
         MAX-HEIGHT         = 24
         MAX-WIDTH          = 150
         VIRTUAL-HEIGHT     = 24
         VIRTUAL-WIDTH      = 150
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
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
THEN W-Win:HIDDEN = yes.

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
ON END-ERROR OF W-Win /* Employee Tracking and Data Collection */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Employee Tracking and Data Collection */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
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
       RUN set-position IN h_smartmsg ( 1.00 , 5.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.14 , 32.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/options.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_options ).
       RUN set-position IN h_options ( 1.00 , 69.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 55.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/phone.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_phone ).
       RUN set-position IN h_phone ( 1.00 , 125.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Brws Emp|View Emp|Pay Rates|Log In/Out|Machines|View Notes' + ',
                     FOLDER-TAB-TYPE = 2':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 3.14 , 2.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 21.67 , 148.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/address.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_address ).
       RUN set-position IN h_address ( 1.00 , 133.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/exit.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.00 , 141.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_phone ,
             h_options , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_folder ,
             FRAME message-frame:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_address ,
             h_phone , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_exit ,
             h_address , 'AFTER':U ).
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
             INPUT  'browsers/employee.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_employee ).
       RUN set-position IN h_employee ( 4.81 , 7.00 ) NO-ERROR.
       RUN set-size IN h_employee ( 19.52 , 138.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartNavBrowser h_employee. */
       RUN add-link IN adm-broker-hdl ( h_p-navico , 'Navigation':U , h_employee ).
       RUN add-link IN adm-broker-hdl ( h_employee , 'Record':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_export ,
             h_exit , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_employee ,
             h_folder , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/employee.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_employee-2 ).
       RUN set-position IN h_employee-2 ( 5.76 , 23.00 ) NO-ERROR.
       /* Size in UIB:  ( 12.14 , 105.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-navico.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navico ).
       RUN set-position IN h_p-navico ( 18.86 , 24.00 ) NO-ERROR.
       RUN set-size IN h_p-navico ( 2.14 , 38.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-updsav.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav ).
       RUN set-position IN h_p-updsav ( 18.86 , 73.00 ) NO-ERROR.
       RUN set-size IN h_p-updsav ( 2.14 , 56.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_employee-2. */
       RUN add-link IN adm-broker-hdl ( h_employee , 'Record':U , h_employee-2 ).
       RUN add-link IN adm-broker-hdl ( h_p-updsav , 'TableIO':U , h_employee-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_employee-2 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navico ,
             h_employee-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updsav ,
             h_p-navico , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewerid/employee.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_employee-3 ).
       RUN set-position IN h_employee-3 ( 4.57 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 121.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/rate.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_rate ).
       RUN set-position IN h_rate ( 6.71 , 4.00 ) NO-ERROR.
       RUN set-size IN h_rate ( 17.62 , 84.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/rate.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_rate-2 ).
       RUN set-position IN h_rate-2 ( 6.71 , 92.00 ) NO-ERROR.
       /* Size in UIB:  ( 15.24 , 56.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-updsav.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav-2 ).
       RUN set-position IN h_p-updsav-2 ( 22.19 , 92.00 ) NO-ERROR.
       RUN set-size IN h_p-updsav-2 ( 2.14 , 56.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_employee-3. */
       RUN add-link IN adm-broker-hdl ( h_employee , 'Record':U , h_employee-3 ).

       /* Links to SmartNavBrowser h_rate. */
       RUN add-link IN adm-broker-hdl ( h_employee , 'Record':U , h_rate ).

       /* Links to SmartViewer h_rate-2. */
       RUN add-link IN adm-broker-hdl ( h_p-updsav-2 , 'TableIO':U , h_rate-2 ).
       RUN add-link IN adm-broker-hdl ( h_rate , 'Record':U , h_rate-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_employee-3 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_rate ,
             h_employee-3 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_rate-2 ,
             h_rate , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updsav-2 ,
             h_rate-2 , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewerid/employee.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_employee-4 ).
       RUN set-position IN h_employee-4 ( 4.57 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 121.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/export.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_export-2 ).
       RUN set-position IN h_export-2 ( 1.05 , 61.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/emplogin.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_emplogin ).
       RUN set-position IN h_emplogin ( 6.71 , 3.00 ) NO-ERROR.
       RUN set-size IN h_emplogin ( 17.62 , 98.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/emplogin.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_emplogin-2 ).
       RUN set-position IN h_emplogin-2 ( 6.71 , 104.00 ) NO-ERROR.
       /* Size in UIB:  ( 8.57 , 44.20 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-updsav.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav-3 ).
       RUN set-position IN h_p-updsav-3 ( 15.52 , 102.00 ) NO-ERROR.
       RUN set-size IN h_p-updsav-3 ( 3.33 , 46.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_employee-4. */
       RUN add-link IN adm-broker-hdl ( h_employee , 'Record':U , h_employee-4 ).

       /* Links to SmartNavBrowser h_emplogin. */
       RUN add-link IN adm-broker-hdl ( h_employee , 'Record':U , h_emplogin ).

       /* Links to SmartViewer h_emplogin-2. */
       RUN add-link IN adm-broker-hdl ( h_emplogin , 'Record':U , h_emplogin-2 ).
       RUN add-link IN adm-broker-hdl ( h_p-updsav-3 , 'TableIO':U , h_emplogin-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_employee-4 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_export-2 ,
             h_exit , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_emplogin ,
             h_employee-4 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_emplogin-2 ,
             h_emplogin , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updsav-3 ,
             h_emplogin-2 , 'AFTER':U ).
    END. /* Page 4 */
    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewerid/employee.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_employee-5 ).
       RUN set-position IN h_employee-5 ( 4.57 , 17.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 121.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/empmach.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_empmach ).
       RUN set-position IN h_empmach ( 6.71 , 28.00 ) NO-ERROR.
       RUN set-size IN h_empmach ( 14.76 , 98.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/empmach.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_empmach-2 ).
       RUN set-position IN h_empmach-2 ( 21.71 , 28.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.62 , 53.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-updsav.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav-4 ).
       RUN set-position IN h_p-updsav-4 ( 21.95 , 82.00 ) NO-ERROR.
       RUN set-size IN h_p-updsav-4 ( 2.14 , 56.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_employee-5. */
       RUN add-link IN adm-broker-hdl ( h_employee , 'Record':U , h_employee-5 ).

       /* Links to SmartNavBrowser h_empmach. */
       RUN add-link IN adm-broker-hdl ( h_employee , 'Record':U , h_empmach ).

       /* Links to SmartViewer h_empmach-2. */
       RUN add-link IN adm-broker-hdl ( h_empmach , 'Record':U , h_empmach-2 ).
       RUN add-link IN adm-broker-hdl ( h_p-updsav-4 , 'TableIO':U , h_empmach-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_employee-5 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_empmach ,
             h_employee-5 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_empmach-2 ,
             h_empmach , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updsav-4 ,
             h_empmach-2 , 'AFTER':U ).
    END. /* Page 5 */
    WHEN 6 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/allnote2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_allnote2 ).
       RUN set-position IN h_allnote2 ( 5.29 , 5.00 ) NO-ERROR.
       RUN set-size IN h_allnote2 ( 9.00 , 136.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/notetext.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_notetext ).
       RUN set-position IN h_notetext ( 14.10 , 7.00 ) NO-ERROR.
       /* Size in UIB:  ( 7.38 , 129.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartBrowser h_allnote2. */
       RUN add-link IN adm-broker-hdl ( h_employee , 'Record':U , h_allnote2 ).

       /* Links to SmartViewer h_notetext. */
       RUN add-link IN adm-broker-hdl ( h_allnote2 , 'note-text':U , h_notetext ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_allnote2 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_notetext ,
             h_allnote2 , 'AFTER':U ).
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
  {src/adm/template/row-list.i "employee"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "employee"}

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
  {src/adm/template/snd-list.i "employee"}

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

