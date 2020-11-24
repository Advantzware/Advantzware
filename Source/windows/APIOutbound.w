&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: windows/APIOutBound.w

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

&SCOPED-DEFINE winReSize
&SCOPED-DEFINE h_Browse01 h_APIOutBound
&SCOPED-DEFINE EXCLUDE-select-page
/* Do not edit/remove the below line. This is required to override the standard
   select-page procudure */ 


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
&Scoped-define EXTERNAL-TABLES APIOutbound
&Scoped-define FIRST-EXTERNAL-TABLE APIOutbound


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR APIOutbound.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-5 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_apiclient AS HANDLE NO-UNDO.
DEFINE VARIABLE h_apiclientxref1 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_apiclientxref1-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_apioutbound AS HANDLE NO-UNDO.
DEFINE VARIABLE h_apioutbound-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_apioutbounddetail AS HANDLE NO-UNDO.
DEFINE VARIABLE h_apioutbounddetail-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_apioutboundtrigger AS HANDLE NO-UNDO.
DEFINE VARIABLE h_apioutboundtrigger-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_options3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updapi AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updapi-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updapi-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updapi-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updapi-5 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-apiclient AS HANDLE NO-UNDO.
DEFINE VARIABLE h_smartmsg AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RECT-5 AT ROW 6.24 COL 1 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.

DEFINE FRAME message-frame
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 111 ROW 2.95
         SIZE 50 BY 1.43
         BGCOLOR 15  WIDGET-ID 200.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: ASI.APIOutbound
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 1
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Outbound API Maintenance"
         HEIGHT             = 28.57
         WIDTH              = 160
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
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

{src/adm/method/containr.i}
{methods/template/windows.i}
{methods/template/selectpage.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME message-frame:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME message-frame
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME message-frame
/* Query rebuild information for FRAME message-frame
     _Query            is NOT OPENED
*/  /* FRAME message-frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Outbound API Maintenance */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Outbound API Maintenance */
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
             INPUT  'smartobj/options3.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_options3 ).
       RUN set-position IN h_options3 ( 1.00 , 89.20 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 63.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/exit.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.00 , 153.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Outbound|View Outbound|Detail|Triggers|ClientXref' + ',
                     FOLDER-TAB-TYPE = 1':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 3.14 , 1.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 26.43 , 160.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartObject h_options3. */
       RUN add-link IN adm-broker-hdl ( h_apioutbound , 'attach':U , h_options3 ).

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).
	   RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'udficon':U , h_options3 ).
       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_options3 ,
             FRAME message-frame:HANDLE , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_exit ,
             h_options3 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_folder ,
             FRAME message-frame:HANDLE , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/smartmsg.w':U ,
             INPUT  FRAME message-frame:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_smartmsg ).
       RUN set-position IN h_smartmsg ( 1.10 , 19.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.14 , 32.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/apioutbound.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = ?':U ,
             OUTPUT h_apioutbound ).
       RUN set-position IN h_apioutbound ( 4.81 , 2.00 ) NO-ERROR.
       RUN set-size IN h_apioutbound ( 23.33 , 157.60 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartBrowser h_apioutbound. */
       RUN add-link IN adm-broker-hdl ( h_p-navico , 'Navigation':U , h_apioutbound ).
       RUN add-link IN adm-broker-hdl ( h_apioutbound , 'Record':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_apioutbound ,
             h_folder , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/apioutbound.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = ?':U ,
             OUTPUT h_apioutbound-2 ).
       RUN set-position IN h_apioutbound-2 ( 6.71 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 18.57 , 157.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-navico.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navico ).
       RUN set-position IN h_p-navico ( 26.71 , 3.00 ) NO-ERROR.
       RUN set-size IN h_p-navico ( 2.38 , 51.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-updapi.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updapi ).
       RUN set-position IN h_p-updapi ( 26.71 , 79.00 ) NO-ERROR.
       RUN set-size IN h_p-updapi ( 2.38 , 81.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_apioutbound-2. */
       RUN add-link IN adm-broker-hdl ( h_apioutbound , 'Record':U , h_apioutbound-2 ).
       RUN add-link IN adm-broker-hdl ( h_p-updapi , 'TableIO':U , h_apioutbound-2 ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'add-outbound':U , h_apioutbound-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_apioutbound-2 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navico ,
             h_apioutbound-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updapi ,
             h_p-navico , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/apioutbounddetail.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_apioutbounddetail ).
       RUN set-position IN h_apioutbounddetail ( 6.24 , 19.80 ) NO-ERROR.
       RUN set-size IN h_apioutbounddetail ( 6.71 , 127.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/apioutbounddetail.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = ?':U ,
             OUTPUT h_apioutbounddetail-2 ).
       RUN set-position IN h_apioutbounddetail-2 ( 14.33 , 24.00 ) NO-ERROR.
       /* Size in UIB:  ( 10.24 , 116.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-updapi.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updapi-2 ).
       RUN set-position IN h_p-updapi-2 ( 25.76 , 40.00 ) NO-ERROR.
       RUN set-size IN h_p-updapi-2 ( 2.14 , 82.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartBrowser h_apioutbounddetail. */
       RUN add-link IN adm-broker-hdl ( h_apioutbound-2 , 'Record':U , h_apioutbounddetail ).

       /* Links to SmartViewer h_apioutbounddetail-2. */
       RUN add-link IN adm-broker-hdl ( h_apioutbounddetail , 'Record':U , h_apioutbounddetail-2 ).
       RUN add-link IN adm-broker-hdl ( h_p-updapi-2 , 'TableIO':U , h_apioutbounddetail-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_apioutbounddetail ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_apioutbounddetail-2 ,
             h_apioutbounddetail , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updapi-2 ,
             h_apioutbounddetail-2 , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/apioutboundtrigger.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_apioutboundtrigger ).
       RUN set-position IN h_apioutboundtrigger ( 6.14 , 5.00 ) NO-ERROR.
       RUN set-size IN h_apioutboundtrigger ( 6.71 , 153.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/apioutboundtrigger.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_apioutboundtrigger-2 ).
       RUN set-position IN h_apioutboundtrigger-2 ( 14.81 , 17.40 ) NO-ERROR.
       /* Size in UIB:  ( 7.38 , 127.20 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-updapi.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updapi-3 ).
       RUN set-position IN h_p-updapi-3 ( 24.57 , 34.20 ) NO-ERROR.
       RUN set-size IN h_p-updapi-3 ( 2.24 , 97.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartBrowser h_apioutboundtrigger. */
       RUN add-link IN adm-broker-hdl ( h_apioutbound-2 , 'Record':U , h_apioutboundtrigger ).

       /* Links to SmartViewer h_apioutboundtrigger-2. */
       RUN add-link IN adm-broker-hdl ( h_apioutboundtrigger , 'Record':U , h_apioutboundtrigger-2 ).
       RUN add-link IN adm-broker-hdl ( h_p-updapi-3 , 'TableIO':U , h_apioutboundtrigger-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_apioutboundtrigger ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_apioutboundtrigger-2 ,
             h_apioutboundtrigger , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updapi-3 ,
             h_apioutboundtrigger-2 , 'AFTER':U ).
    END. /* Page 4 */
    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/apiclient.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_apiclient ).
       RUN set-position IN h_apiclient ( 4.81 , 5.20 ) NO-ERROR.
       /* Size in UIB:  ( 4.76 , 120.60 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-updapi.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updapi-4 ).
       RUN set-position IN h_p-updapi-4 ( 4.81 , 126.20 ) NO-ERROR.
       RUN set-size IN h_p-updapi-4 ( 4.76 , 32.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/apiclientxref1.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_apiclientxref1 ).
       RUN set-position IN h_apiclientxref1 ( 9.86 , 20.40 ) NO-ERROR.
       RUN set-size IN h_apiclientxref1 ( 12.86 , 123.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/apiclientxref1.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_apiclientxref1-2 ).
       RUN set-position IN h_apiclientxref1-2 ( 23.00 , 20.40 ) NO-ERROR.
       /* Size in UIB:  ( 4.76 , 88.20 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-updapi.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updapi-5 ).
       RUN set-position IN h_p-updapi-5 ( 23.00 , 109.00 ) NO-ERROR.
       RUN set-size IN h_p-updapi-5 ( 4.76 , 35.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/q-apiclient.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-apiclient ).
       RUN set-position IN h_q-apiclient ( 9.86 , 147.60 ) NO-ERROR.
       /* Size in UIB:  ( 1.86 , 10.80 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_apiclient. */
       RUN add-link IN adm-broker-hdl ( h_p-updapi-4 , 'TableIO':U , h_apiclient ).
       RUN add-link IN adm-broker-hdl ( h_q-apiclient , 'Record':U , h_apiclient ).

       /* Links to SmartBrowser h_apiclientxref1. */
       RUN add-link IN adm-broker-hdl ( h_apiclient , 'Record':U , h_apiclientxref1 ).

       /* Links to SmartViewer h_apiclientxref1-2. */
       RUN add-link IN adm-broker-hdl ( h_apiclientxref1 , 'Record':U , h_apiclientxref1-2 ).
       RUN add-link IN adm-broker-hdl ( h_p-updapi-5 , 'TableIO':U , h_apiclientxref1-2 ).

       /* Links to SmartQuery h_q-apiclient. */
       RUN add-link IN adm-broker-hdl ( h_apioutbound-2 , 'Record':U , h_q-apiclient ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_apiclient ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updapi-4 ,
             h_apiclient , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_apiclientxref1 ,
             h_p-updapi-4 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_apiclientxref1-2 ,
             h_apiclientxref1 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updapi-5 ,
             h_apiclientxref1-2 , 'AFTER':U ).
    END. /* Page 5 */

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
  {src/adm/template/row-list.i "APIOutbound"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "APIOutbound"}

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
  ENABLE RECT-5 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW FRAME message-frame IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-message-frame}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select_add W-Win 
PROCEDURE select_add :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
    
    RUN SELECT-PAGE(2).
    RUN get-link-handle IN adm-broker-hdl (
        INPUT  THIS-PROCEDURE,
        INPUT  "add-outbound-TARGET",
        OUTPUT char-hdl
        ).

    IF char-hdl NE "" THEN    
        RUN SelectAndCopyAPIOutbound IN WIDGET-HANDLE (
            char-hdl
            ).
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
  {src/adm/template/snd-list.i "APIOutbound"}

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

