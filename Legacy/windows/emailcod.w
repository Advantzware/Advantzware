&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          nosweat          PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: windows/emailcod.w

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

&SCOPED-DEFINE winReSize
&SCOPED-DEFINE h_Browse01 h_emailcod

/* Variables */
DEFINE VARIABLE lvPhoneType   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lvRowID       AS ROWID      NO-UNDO.

DEFINE VARIABLE lvEmpAlertType   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lvEmpAlertRowID       AS ROWID      NO-UNDO.

DEF VAR page-hdl AS CHAR NO-UNDO.

/* Includes */
{methods\defines\phone.i &new="NEW"}

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
&Scoped-define EXTERNAL-TABLES emailcod
&Scoped-define FIRST-EXTERNAL-TABLE emailcod


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR emailcod.
/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_cuserphon AS HANDLE NO-UNDO.
DEFINE VARIABLE h_custphon AS HANDLE NO-UNDO.
DEFINE VARIABLE h_econtact AS HANDLE NO-UNDO.
DEFINE VARIABLE h_econtact-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_econtact-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_econtact-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ecstuser AS HANDLE NO-UNDO.
DEFINE VARIABLE h_emailcod AS HANDLE NO-UNDO.
DEFINE VARIABLE h_emailcod-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_emailcst AS HANDLE NO-UNDO.
DEFINE VARIABLE h_emailship AS HANDLE NO-UNDO.
DEFINE VARIABLE h_emailsold AS HANDLE NO-UNDO.
DEFINE VARIABLE h_emailvnd AS HANDLE NO-UNDO.
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_options AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_phone AS HANDLE NO-UNDO.
DEFINE VARIABLE h_shipphon AS HANDLE NO-UNDO.
DEFINE VARIABLE h_smartmsg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_soldphon AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vendphon AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 158.6 BY 21.91
         BGCOLOR 15 .

DEFINE FRAME FRAME-A
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         NO-LABELS NO-UNDERLINE 
         AT COL 133 ROW 3.14
         SIZE 26 BY 1.19
         BGCOLOR 15  WIDGET-ID 100.

DEFINE FRAME message-frame
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.95
         SIZE 63 BY .95
         BGCOLOR 15 .

DEFINE FRAME OPTIONS-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 158 BY 1.91
         BGCOLOR 15 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: NOSWEAT.emailcod
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 6
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Email Codes"
         HEIGHT             = 21.91
         WIDTH              = 158.6
         MAX-HEIGHT         = 23.24
         MAX-WIDTH          = 158.6
         VIRTUAL-HEIGHT     = 23.24
         VIRTUAL-WIDTH      = 158.6
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
{methods/template/windows.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-A:FRAME = FRAME F-Main:HANDLE
       FRAME message-frame:FRAME = FRAME F-Main:HANDLE
       FRAME OPTIONS-FRAME:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
ASSIGN 
       FRAME FRAME-A:SENSITIVE        = FALSE.

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

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
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
ON END-ERROR OF W-Win /* Email Codes */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Email Codes */
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
IF NOT CAN-FIND(FIRST emaildtl) THEN CREATE emaildtl.
/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

IF NOT CAN-FIND(FIRST asi._file WHERE asi._file._FILE-NAME = "empalert") THEN
DO:
   RUN get-link-handle IN adm-broker-hdl
                      (THIS-PROCEDURE, 'PAGE-SOURCE',OUTPUT page-hdl).
   RUN delete-folder-page IN WIDGET-HANDLE(page-hdl) (INPUT 5).
END.

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
             INPUT  'smartobj/options.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_options ).
       RUN set-position IN h_options ( 1.00 , 86.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 55.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/phone.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_phone ).
       RUN set-position IN h_phone ( 1.00 , 142.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/exit.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.00 , 150.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Email Codes|Cust Contacts|Vend Contacts|Ship Contacts|Emp. Alerts|Sold Contact' + ',
                     FOLDER-TAB-TYPE = 1':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 3.14 , 1.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 19.76 , 158.00 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_phone ,
             h_options , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_exit ,
             h_phone , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_folder ,
             FRAME message-frame:HANDLE , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/smartmsg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_smartmsg ).
       RUN set-position IN h_smartmsg ( 1.71 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.14 , 32.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/emailcod.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_emailcod ).
       RUN set-position IN h_emailcod ( 4.81 , 13.00 ) NO-ERROR.
       RUN set-size IN h_emailcod ( 17.62 , 63.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/emailcod.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_emailcod-2 ).
       RUN set-position IN h_emailcod-2 ( 7.91 , 82.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.62 , 56.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-updsav.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav ).
       RUN set-position IN h_p-updsav ( 11.71 , 82.00 ) NO-ERROR.
       RUN set-size IN h_p-updsav ( 2.14 , 56.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2,4':U) NO-ERROR.

       /* Links to SmartNavBrowser h_emailcod. */
       RUN add-link IN adm-broker-hdl ( h_p-navico , 'Navigation':U , h_emailcod ).
       RUN add-link IN adm-broker-hdl ( h_p-navico-2 , 'Navigation':U , h_emailcod ).
       RUN add-link IN adm-broker-hdl ( h_emailcod , 'Record':U , THIS-PROCEDURE ).

       /* Links to SmartViewer h_emailcod-2. */
       RUN add-link IN adm-broker-hdl ( h_emailcod , 'Record':U , h_emailcod-2 ).
       RUN add-link IN adm-broker-hdl ( h_p-updsav , 'TableIO':U , h_emailcod-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_smartmsg ,
             FRAME OPTIONS-FRAME:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_emailcod ,
             FRAME FRAME-A:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_emailcod-2 ,
             h_emailcod , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updsav ,
             h_emailcod-2 , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/econtact.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_econtact ).
       RUN set-position IN h_econtact ( 4.33 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 70.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-navico.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navico ).
       RUN set-position IN h_p-navico ( 4.33 , 79.00 ) NO-ERROR.
       RUN set-size IN h_p-navico ( 1.43 , 42.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/emailcst.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_emailcst ).
       RUN set-position IN h_emailcst ( 5.76 , 4.00 ) NO-ERROR.
       RUN set-size IN h_emailcst ( 8.57 , 138.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/custphon.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_custphon ).
       RUN set-position IN h_custphon ( 14.33 , 4.00 ) NO-ERROR.
       RUN set-size IN h_custphon ( 8.57 , 138.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_econtact. */
       RUN add-link IN adm-broker-hdl ( h_emailcod , 'Record':U , h_econtact ).

       /* Links to SmartNavBrowser h_emailcst. */
       RUN add-link IN adm-broker-hdl ( h_custphon , 'Refresh':U , h_emailcst ).
       RUN add-link IN adm-broker-hdl ( h_emailcod , 'Record':U , h_emailcst ).

       /* Links to SmartNavBrowser h_custphon. */
       RUN add-link IN adm-broker-hdl ( h_emailcod , 'Record':U , h_custphon ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_econtact ,
             FRAME FRAME-A:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navico ,
             h_econtact , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_emailcst ,
             h_p-navico , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_custphon ,
             h_emailcst , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/econtact.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_econtact-2 ).
       RUN set-position IN h_econtact-2 ( 4.33 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 70.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/emailvnd.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_emailvnd ).
       RUN set-position IN h_emailvnd ( 5.76 , 4.00 ) NO-ERROR.
       RUN set-size IN h_emailvnd ( 8.57 , 138.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/vendphon.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vendphon ).
       RUN set-position IN h_vendphon ( 14.33 , 4.00 ) NO-ERROR.
       RUN set-size IN h_vendphon ( 8.57 , 138.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_econtact-2. */
       RUN add-link IN adm-broker-hdl ( h_emailcod , 'Record':U , h_econtact-2 ).

       /* Links to SmartNavBrowser h_emailvnd. */
       RUN add-link IN adm-broker-hdl ( h_emailcod , 'Record':U , h_emailvnd ).
       RUN add-link IN adm-broker-hdl ( h_vendphon , 'Refresh':U , h_emailvnd ).

       /* Links to SmartNavBrowser h_vendphon. */
       RUN add-link IN adm-broker-hdl ( h_emailcod , 'Record':U , h_vendphon ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_econtact-2 ,
             FRAME FRAME-A:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_emailvnd ,
             h_econtact-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_vendphon ,
             h_emailvnd , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/econtact.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_econtact-3 ).
       RUN set-position IN h_econtact-3 ( 4.33 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 70.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-navico.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navico-2 ).
       RUN set-position IN h_p-navico-2 ( 4.33 , 79.00 ) NO-ERROR.
       RUN set-size IN h_p-navico-2 ( 1.43 , 42.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/emailship.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_emailship ).
       RUN set-position IN h_emailship ( 5.76 , 4.00 ) NO-ERROR.
       RUN set-size IN h_emailship ( 8.57 , 138.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/shipphon.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_shipphon ).
       RUN set-position IN h_shipphon ( 14.33 , 4.00 ) NO-ERROR.
       RUN set-size IN h_shipphon ( 8.57 , 138.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1,2':U) NO-ERROR.

       /* Links to SmartViewer h_econtact-3. */
       RUN add-link IN adm-broker-hdl ( h_emailcod , 'Record':U , h_econtact-3 ).

       /* Links to SmartNavBrowser h_emailship. */
       RUN add-link IN adm-broker-hdl ( h_emailcst , 'Record':U , h_emailship ).
       RUN add-link IN adm-broker-hdl ( h_shipphon , 'Refresh':U , h_emailship ).

       /* Links to SmartNavBrowser h_shipphon. */
       RUN add-link IN adm-broker-hdl ( h_emailcod , 'Record':U , h_shipphon ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_econtact-3 ,
             FRAME FRAME-A:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navico-2 ,
             h_econtact-3 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_emailship ,
             h_p-navico-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_shipphon ,
             h_emailship , 'AFTER':U ).
    END. /* Page 4 */
    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/econtact.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_econtact-4 ).
       RUN set-position IN h_econtact-4 ( 4.33 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 70.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/ecstuser.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_ecstuser ).
       RUN set-position IN h_ecstuser ( 5.76 , 4.00 ) NO-ERROR.
       RUN set-size IN h_ecstuser ( 8.57 , 138.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/cuserphon.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_cuserphon ).
       RUN set-position IN h_cuserphon ( 14.33 , 4.00 ) NO-ERROR.
       RUN set-size IN h_cuserphon ( 8.57 , 138.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_econtact-4. */
       RUN add-link IN adm-broker-hdl ( h_emailcod , 'Record':U , h_econtact-4 ).

       /* Links to SmartNavBrowser h_ecstuser. */
       RUN add-link IN adm-broker-hdl ( h_cuserphon , 'Refresh':U , h_ecstuser ).
       RUN add-link IN adm-broker-hdl ( h_emailcod , 'Record':U , h_ecstuser ).

       /* Links to SmartNavBrowser h_cuserphon. */
       RUN add-link IN adm-broker-hdl ( h_emailcod , 'Record':U , h_cuserphon ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_econtact-4 ,
             FRAME FRAME-A:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_ecstuser ,
             h_econtact-4 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_cuserphon ,
             h_ecstuser , 'AFTER':U ).
    END. /* Page 5 */
    WHEN 6 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/emailsold.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_emailsold ).
       RUN set-position IN h_emailsold ( 5.05 , 4.00 ) NO-ERROR.
       RUN set-size IN h_emailsold ( 8.57 , 138.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/soldphon.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_soldphon ).
       RUN set-position IN h_soldphon ( 13.86 , 4.20 ) NO-ERROR.
       RUN set-size IN h_soldphon ( 8.57 , 138.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2,1':U) NO-ERROR.

       /* Links to SmartNavBrowser h_emailsold. */
       RUN add-link IN adm-broker-hdl ( h_emailcst , 'Record':U , h_emailsold ).
       RUN add-link IN adm-broker-hdl ( h_soldphon , 'Refresh':U , h_emailsold ).

       /* Links to SmartNavBrowser h_soldphon. */
       RUN add-link IN adm-broker-hdl ( h_emailcod , 'Record':U , h_soldphon ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_emailsold ,
             FRAME FRAME-A:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_soldphon ,
             h_emailsold , 'AFTER':U ).
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
  {src/adm/template/row-list.i "emailcod"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "emailcod"}

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
  VIEW FRAME FRAME-A IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  FRAME FRAME-A:SENSITIVE = NO.
  VIEW W-Win.
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
  RUN GET-ATTRIBUTE('CURRENT-PAGE').

  case INT (RETURN-VALUE):
    when 3 then s-rec_key = string (dynamic-function ('GetCurrVendor' in h_vendphon)).
    when 4 then s-rec_key = string (dynamic-function ('GetCurrShipTo' in h_shipphon)).
    WHEN 5 THEN s-rec_key = STRING (DYNAMIC-FUNCTION ('GetCurrBillTo' in h_cuserphon)).
    otherwise   s-rec_key = string (dynamic-function ('GetCurrBillTo' in h_custphon)).
  end case.

  CASE INT (RETURN-VALUE):
    WHEN 3 THEN DYNAMIC-FUNCTION ('ApplyValueChanged' IN h_vendphon).
    WHEN 5 THEN DYNAMIC-FUNCTION ('ApplyValueChanged' IN h_cuserphon).
    OTHERWISE   DYNAMIC-FUNCTION ('ApplyValueChanged' IN h_custphon).
  END CASE.

  {methods/winReSizePgChg.i}

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
  {src/adm/template/snd-list.i "emailcod"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setEmpAlertValues W-Win 
PROCEDURE setEmpAlertValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipEmpAlertType AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.

  ASSIGN
    lvEmpAlertType = ipEmpAlertType
    lvEmpAlertRowID     = ipRowID.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setPhoneValues W-Win 
PROCEDURE setPhoneValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipPhoneType AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.

  ASSIGN
    lvPhoneType = ipPhoneType
    lvRowID     = ipRowID.

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

