&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File              : windows/cust.w
          
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

/* Preprocessors */

&scoped-define item_spec Customer
&SCOPED-DEFINE winReSize
&SCOPED-DEFINE h_Browse01 h_cust
&SCOPED-DEFINE h_Object02 h_soldto-2
&SCOPED-DEFINE h_Object03 h_p-cstsld
&SCOPED-DEFINE h_Object04 h_custmark-2
&SCOPED-DEFINE h_Object05 h_p-updsav
&SCOPED-DEFINE moveRight {&h_Object04},{&h_Object05}

/* Parameters Definitions ---                                           */
DEF INPUT PARAMETER ip-cust AS RECID NO-UNDO .
/* Variables */
{methods/defines/hndldefs.i}
DEF VAR lv-cust-rec-key   AS char NO-UNDO.
DEF VAR rec_key_value AS CHAR NO-UNDO.
DEF VAR v-att AS LOG NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES cust
&Scoped-define FIRST-EXTERNAL-TABLE cust


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR cust.
/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-ctcusi AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cust AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cust-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cust-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cust-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cust-5 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cust-6 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cust-tot AS HANDLE NO-UNDO.
DEFINE VARIABLE h_custmark AS HANDLE NO-UNDO.
DEFINE VARIABLE h_custmark-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-csthd AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-cstshp AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-cstsld AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-csttot AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_shipto AS HANDLE NO-UNDO.
DEFINE VARIABLE h_shipto-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_soldto AS HANDLE NO-UNDO.
DEFINE VARIABLE h_soldto-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-ctcusi AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 157.2 BY 26
         BGCOLOR 4 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: ASI.cust
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Customer File Maintenance"
         HEIGHT             = 23.91
         WIDTH              = 157.4
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
/*{methods/template/windows.i}*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
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
ON END-ERROR OF W-Win /* Customer File Maintenance */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Customer File Maintenance */
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
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'View Customer|Ship To|Sold To|Totals|Pricing|Credit Status' + ',
                     FOLDER-TAB-TYPE = 1':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 1.00 , 1.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 23.67 , 155.00 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */
    
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/cust.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_cust-2 ).
       RUN set-position IN h_cust-2 ( 2.60 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 18.10 , 145.20 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-navico.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navico ).
       RUN set-position IN h_p-navico ( 21.91 , 15.00 ) NO-ERROR.
       RUN set-size IN h_p-navico ( 1.67 , 38.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-csthd.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-csthd ).
       RUN set-position IN h_p-csthd ( 21.91 , 60.00 ) NO-ERROR.
       RUN set-size IN h_p-csthd ( 1.67 , 82.00 ) NO-ERROR.

       /* Links to SmartViewer h_cust-2. */
       RUN add-link IN adm-broker-hdl ( /*h_cust*/ THIS-PROCEDURE , 'Record':U , h_cust-2 ).
       RUN add-link IN adm-broker-hdl ( h_p-csthd , 'TableIO':U , h_cust-2 ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'add-item':U , h_cust-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_cust-2 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navico ,
             h_cust-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-csthd ,
             h_p-navico , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewerid/cust.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_cust-3 ).
       RUN set-position IN h_cust-3 ( 2.60 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.14 , 107.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/shipto.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_shipto-2 ).
       RUN set-position IN h_shipto-2 ( 2.60 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 19.05 , 149.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/shipto.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_shipto ).
       RUN set-position IN h_shipto ( 4.71 , 3.00 ) NO-ERROR.
       RUN set-size IN h_shipto ( 6.43 , 107.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-cstshp.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-cstshp ).
       RUN set-position IN h_p-cstshp ( 22.00 , 28.00 ) NO-ERROR.
       RUN set-size IN h_p-cstshp ( 1.67 , 92.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_cust-3. */
       RUN add-link IN adm-broker-hdl ( h_cust-2 , 'Record':U , h_cust-3 ).

       /* Links to SmartViewer h_shipto-2. */
       RUN add-link IN adm-broker-hdl ( h_p-cstshp , 'TableIO':U , h_shipto-2 ).
       RUN add-link IN adm-broker-hdl ( h_shipto , 'Record':U , h_shipto-2 ).

       /* Links to SmartNavBrowser h_shipto. */
       RUN add-link IN adm-broker-hdl ( h_cust-2 , 'Record':U , h_shipto ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'shipto':U , h_shipto ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_shipto-2 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_shipto ,
             h_shipto-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-cstshp ,
             h_shipto , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewerid/cust.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_cust-4 ).
       RUN set-position IN h_cust-4 ( 2.57 , 23.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.14 , 107.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/soldto.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_soldto ).
       RUN set-position IN h_soldto ( 4.71 , 23.00 ) NO-ERROR.
       RUN set-size IN h_soldto ( 11.19 , 107.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/soldto.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_soldto-2 ).
       RUN set-position IN h_soldto-2 ( 16.14 , 23.00 ) NO-ERROR.
       /* Size in UIB:  ( 6.19 , 71.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-cstsld.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-cstsld ).
       RUN set-position IN h_p-cstsld ( 16.14 , 110.00 ) NO-ERROR.
       RUN set-size IN h_p-cstsld ( 6.29 , 20.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_cust-4. */
       RUN add-link IN adm-broker-hdl ( h_cust-2 , 'Record':U , h_cust-4 ).

       /* Links to SmartNavBrowser h_soldto. */
       RUN add-link IN adm-broker-hdl ( h_cust-2 , 'Record':U , h_soldto ).

       /* Links to SmartViewer h_soldto-2. */
       RUN add-link IN adm-broker-hdl ( h_p-cstsld , 'TableIO':U , h_soldto-2 ).
       RUN add-link IN adm-broker-hdl ( h_soldto , 'Record':U , h_soldto-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_cust-4 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_soldto ,
             h_cust-4 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_soldto-2 ,
             h_soldto , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-cstsld ,
             h_soldto-2 , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewerid/cust.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_cust-5 ).
       RUN set-position IN h_cust-5 ( 3.29 , 23.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.14 , 107.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/cust-tot.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_cust-tot ).
       RUN set-position IN h_cust-tot ( 5.43 , 23.00 ) NO-ERROR.
       /* Size in UIB:  ( 13.81 , 107.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/p-csttot.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-csttot ).
       RUN set-position IN h_p-csttot ( 21.24 , 23.00 ) NO-ERROR.
       RUN set-size IN h_p-csttot ( 1.76 , 107.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_cust-5. */
       RUN add-link IN adm-broker-hdl ( h_cust-2 , 'Record':U , h_cust-5 ).

       /* Links to SmartViewer h_cust-tot. */
       RUN add-link IN adm-broker-hdl ( h_cust-2 , 'Record':U , h_cust-tot ).
       RUN add-link IN adm-broker-hdl ( h_p-csttot , 'TableIO':U , h_cust-tot ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_cust-5 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_cust-tot ,
             h_cust-5 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-csttot ,
             h_cust-tot , 'AFTER':U ).
    END. /* Page 4 */
    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewerid/cust.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_cust-6 ).
       RUN set-position IN h_cust-6 ( 2.57 , 23.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.14 , 107.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/custmark.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = ?':U ,
             OUTPUT h_custmark ).
       RUN set-position IN h_custmark ( 4.71 , 3.00 ) NO-ERROR.
       RUN set-size IN h_custmark ( 17.86 , 96.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/custmark.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Key-Name = ,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_custmark-2 ).
       RUN set-position IN h_custmark-2 ( 4.71 , 99.00 ) NO-ERROR.
       /* Size in UIB:  ( 15.24 , 57.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-updsav.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav ).
       RUN set-position IN h_p-updsav ( 20.19 , 99.00 ) NO-ERROR.
       RUN set-size IN h_p-updsav ( 2.38 , 57.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_cust-6. */
       RUN add-link IN adm-broker-hdl ( h_cust-2 , 'Record':U , h_cust-6 ).

       /* Links to SmartNavBrowser h_custmark. */
       RUN add-link IN adm-broker-hdl ( h_cust-2 , 'Record':U , h_custmark ).

       /* Links to SmartViewer h_custmark-2. */
       RUN add-link IN adm-broker-hdl ( h_custmark , 'Record':U , h_custmark-2 ).
       RUN add-link IN adm-broker-hdl ( h_p-updsav , 'TableIO':U , h_custmark-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_cust-6 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_custmark ,
             h_cust-6 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_custmark-2 ,
             h_custmark , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updsav ,
             h_custmark-2 , 'AFTER':U ).
    END. /* Page 5 */
    WHEN 6 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/v-ctcusi.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-ctcusi ).
       RUN set-position IN h_v-ctcusi ( 2.57 , 6.20 ) NO-ERROR.
       /* Size in UIB:  ( 7.14 , 147.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/b-ctcusi.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-ctcusi ).
       RUN set-position IN h_b-ctcusi ( 9.81 , 26.60 ) NO-ERROR.
       RUN set-size IN h_b-ctcusi ( 12.67 , 92.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_v-ctcusi. */
       RUN add-link IN adm-broker-hdl ( h_cust-2 , 'Record':U , h_v-ctcusi ).

       /* Links to SmartBrowser h_b-ctcusi. */
       RUN add-link IN adm-broker-hdl ( h_v-ctcusi , 'Record':U , h_b-ctcusi ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-ctcusi ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-ctcusi ,
             h_v-ctcusi , 'AFTER':U ).
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
  {src/adm/template/row-list.i "cust"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "cust"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allow-create W-Win 
PROCEDURE allow-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER op-flag AS LOGICAL NO-UNDO.
  op-flag = yes.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allow-delete W-Win 
PROCEDURE allow-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*&Scoped-define ACCESSTYPE delete
   {methods/template/security.i}
  */
  DEFINE OUTPUT PARAMETER op-flag AS LOGICAL NO-UNDO.
  op-flag = yes.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allow-update W-Win 
PROCEDURE allow-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*&Scoped-define ACCESSTYPE delete
  {methods/template/security.i}
*/
DEFINE OUTPUT PARAMETER op-flag AS LOGICAL NO-UNDO.
op-flag = yes.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-note W-Win 
PROCEDURE disable-note :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def output parameter op-need-note as log no-undo.
  
  op-need-note = no.
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
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-g_rec_key W-Win 
PROCEDURE get-g_rec_key :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE OUTPUT PARAMETER op-rec_key AS CHARACTER NO-UNDO.
/*
  IF g_rec_key NE "" THEN
  op-rec_key = ENTRY(NUM-ENTRIES(g_rec_key),g_rec_key).
*/ 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-change-page W-Win 
PROCEDURE local-change-page :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iCurrentPage AS INT.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'change-page':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /*{methods/winReSizePgChg.i}*/

  RUN GET-ATTRIBUTE('CURRENT-PAGE').
  iCurrentPage = INT(RETURN-VALUE).
  IF iCurrentPage = 2 
    THEN assign s-rec_key     = string (dynamic-function ('GetCurrShipTo' in h_shipto)).
    else assign rec_key_value = lv-cust-rec-key
                s-rec_key     = lv-cust-rec-key.
  IF iCurrentPage = 1 THEN DO:
      RUN dispatch IN h_p-navico ( INPUT 'adm-initialize':U ) .
  END.
/*   MESSAGE 'windows\cust.w'    skip                     */
/*           'local-change-page' skip                     */
/*           'Page:'             int (return-value)  skip */
/*           'rec_key_value:'    rec_key_value       skip */
/*           'lv-cust-rec-key:'  lv-cust-rec-key          */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                 */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
 FIND FIRST cust WHERE recid(cust) = ip-cust NO-LOCK NO-ERROR.
 
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  
  APPLY "ENTRY" TO FRAME {&FRAME-NAME}.
  
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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  assign lv-cust-rec-key = cust.rec_key when avail cust no-error.

  v-att = CAN-FIND(FIRST asi.attach WHERE
          attach.company = cust.company and
          attach.rec_key = lv-cust-rec-key AND
          ATTACH.est-no EQ "").

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'attach-target':U, OUTPUT char-hdl).
  
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
     RUN pushpin-image IN WIDGET-HANDLE(char-hdl) (INPUT v-att).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Select_Add W-Win 
PROCEDURE Select_Add :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR char-hdl AS CHAR NO-UNDO.
  
   RUN select-page(1).
   RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"add-item-target", OUTPUT char-hdl).
   RUN add-item IN WIDGET-HANDLE(char-hdl).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Select_att W-Win 
PROCEDURE Select_att :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR header_value AS CHAR NO-UNDO.
    
    {methods/select_attcust.i rec_key_value header_value 0}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select_empalert W-Win 
PROCEDURE select_empalert :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF CAN-FIND(FIRST asi._file WHERE asi._file._FILE-NAME = "empalert") THEN
      RUN windows/empalert.w(rec_key_value,"").
   ELSE
      MESSAGE "Database Change Needed.  Contact ASI."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select_exit W-Win 
PROCEDURE select_exit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   def var char-hdl as cha no-undo.   
   
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
  {src/adm/template/snd-list.i "cust"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-s-rec_key W-Win 
PROCEDURE set-s-rec_key :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-rec_key AS CHAR NO-UNDO.

   s-rec_key = ip-rec_key.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE value-changed-proc W-Win 
PROCEDURE value-changed-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  assign lv-cust-rec-key = cust.rec_key when avail cust no-error.

  v-att = CAN-FIND(FIRST asi.attach WHERE
          attach.company = cust.company and
          attach.rec_key = lv-cust-rec-key AND
          ATTACH.est-no  = "").

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'attach-target':U, OUTPUT char-hdl).
  
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
     RUN pushpin-image IN WIDGET-HANDLE(char-hdl) (INPUT v-att).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

