&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: oe\w-oeinv.w

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

&SCOPED-DEFINE setUserPrint
&SCOPED-DEFINE winReSize
&SCOPED-DEFINE h_Browse01 h_b-oeinv
&SCOPED-DEFINE h_Object01 h_p-invmis
&SCOPED-DEFINE h_Object02 h_p-bolitm

/* Variables */
DEF VAR li-prev-page    AS INTEGER  NO-UNDO.
DEF VAR li-cur-page     AS INTEGER  NO-UNDO.
DEF VAR vlInitialized   AS LOGICAL  NO-UNDO.

DEF BUFFER b-inv-head FOR inv-head.
DEF BUFFER b-inv-line FOR inv-line.
DEF BUFFER b-inv-misc FOR inv-misc.
DEF BUFFER b-cust FOR cust.

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
&Scoped-define EXTERNAL-TABLES inv-head
&Scoped-define FIRST-EXTERNAL-TABLE inv-head


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR inv-head.
/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_attachcust AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-inmisc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-invitm AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-oeboll AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-oeinv AS HANDLE NO-UNDO.
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_options AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-bolitm AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-invbil AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-invmis AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-invvw AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-invbol AS HANDLE NO-UNDO.
DEFINE VARIABLE h_smartmsg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-invbil AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-oebolh AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-oeinv AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-invh AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vp-initm AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vp-ivhld AS HANDLE NO-UNDO.
DEFINE VARIABLE h_w-invesf AS HANDLE NO-UNDO.
DEFINE VARIABLE h_w-invest AS HANDLE NO-UNDO.
DEFINE VARIABLE h_w-invfg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_export AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 150 BY 23.81
         BGCOLOR 15 .

DEFINE FRAME message-frame
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 117 ROW 2.91
         SIZE 34 BY 1.43
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
   External Tables: asi.inv-head
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Invoice Maintenance"
         HEIGHT             = 23.81
         WIDTH              = 150
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
ON END-ERROR OF W-Win /* Invoice Maintenance */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Invoice Maintenance */
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
             INPUT  'smartobj/attachcust.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_attachcust ).
       RUN set-position IN h_attachcust ( 1.00 , 69.20 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/smartmsg.w':U ,
             INPUT  FRAME message-frame:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_smartmsg ).
       RUN set-position IN h_smartmsg ( 1.24 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.14 , 32.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/optionse.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_options ).
       RUN set-position IN h_options ( 1.00 , 77.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 63.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/exit.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.00 , 141.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Brwse Inv|View Inv|Items|Misc|FG Item|Estimate|Bill Notes|BOL' + ',
                     FOLDER-TAB-TYPE = 2':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 3.14 , 2.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 21.67 , 148.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartObject h_attachcust. */
       RUN add-link IN adm-broker-hdl ( h_b-oeinv , 'attachcust':U , h_attachcust ).

       /* Links to SmartObject h_options. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'oeinvopt':U , h_options ).

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_options ,
             h_attachcust , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_exit ,
             h_options , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_folder ,
             FRAME message-frame:HANDLE , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/export.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_export ).
       RUN set-position IN h_export ( 1.00 , 61.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/b-oeinv.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = ?':U ,
             OUTPUT h_b-oeinv ).
       RUN set-position IN h_b-oeinv ( 4.81 , 4.00 ) NO-ERROR.
       RUN set-size IN h_b-oeinv ( 19.52 , 145.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartNavBrowser h_b-oeinv. */
       RUN add-link IN adm-broker-hdl ( h_p-navico , 'Navigation':U , h_b-oeinv ).
       RUN add-link IN adm-broker-hdl ( h_b-oeinv , 'Record':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_b-oeinv , 'export-xl':U , h_export ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-oeinv ,
             h_folder , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/v-oeinv.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Key-Name = ,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_v-oeinv ).
       RUN set-position IN h_v-oeinv ( 4.81 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 16.67 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-navico.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navico ).
       RUN set-position IN h_p-navico ( 22.19 , 4.00 ) NO-ERROR.
       RUN set-size IN h_p-navico ( 2.14 , 38.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/p-invvw.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-invvw ).
       RUN set-position IN h_p-invvw ( 22.19 , 52.00 ) NO-ERROR.
       RUN set-size IN h_p-invvw ( 2.14 , 72.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/vp-ivhld.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vp-ivhld ).
       RUN set-position IN h_vp-ivhld ( 22.19 , 135.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.14 , 13.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_v-oeinv. */
       RUN add-link IN adm-broker-hdl ( h_b-oeinv , 'Record':U , h_v-oeinv ).
       RUN add-link IN adm-broker-hdl ( h_p-invvw , 'TableIO':U , h_v-oeinv ).
       RUN add-link IN adm-broker-hdl ( h_vp-ivhld , 'hold':U , h_v-oeinv ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'addPlusButton':U , h_v-oeinv ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-oeinv ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navico ,
             h_v-oeinv , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-invvw ,
             h_p-navico , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_vp-ivhld ,
             h_p-invvw , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/vp-initm.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vp-initm ).
       RUN set-position IN h_vp-initm ( 4.86 , 22.20 ) NO-ERROR.
       /* Size in UIB:  ( 1.29 , 105.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/b-invitm.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-invitm ).
       RUN set-position IN h_b-invitm ( 6.24 , 4.00 ) NO-ERROR.
       RUN set-size IN h_b-invitm ( 14.76 , 145.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartViewer h_vp-initm. */
       RUN add-link IN adm-broker-hdl ( h_b-invitm , 'Record':U , h_vp-initm ).
       RUN add-link IN adm-broker-hdl ( h_v-oeinv , 'auto-add':U , h_vp-initm ).

       /* Links to SmartNavBrowser h_b-invitm. */
       RUN add-link IN adm-broker-hdl ( h_v-oeinv , 'Record':U , h_b-invitm ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'estimate':U , h_b-invitm ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-invitm ,
             h_folder , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/b-inmisc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_b-inmisc ).
       RUN set-position IN h_b-inmisc ( 5.05 , 3.00 ) NO-ERROR.
       RUN set-size IN h_b-inmisc ( 15.00 , 145.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/p-invmis.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-invmis ).
       RUN set-position IN h_p-invmis ( 21.00 , 32.00 ) NO-ERROR.
       RUN set-size IN h_p-invmis ( 1.76 , 82.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('3':U) NO-ERROR.

       /* Links to SmartNavBrowser h_b-inmisc. */
       RUN add-link IN adm-broker-hdl ( h_b-invitm , 'Record':U , h_b-inmisc ).
       RUN add-link IN adm-broker-hdl ( h_p-invmis , 'TableIO':U , h_b-inmisc ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-inmisc ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-invmis ,
             h_b-inmisc , 'AFTER':U ).
    END. /* Page 4 */
    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/w-invfg.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_w-invfg ).
       /* Position in AB:  ( 6.00 , 8.00 ) */
       /* Size in UIB:  ( 1.86 , 10.80 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('3':U) NO-ERROR.

       /* Links to SmartWindow h_w-invfg. */
       RUN add-link IN adm-broker-hdl ( h_b-invitm , 'Record':U , h_w-invfg ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'quote':U , h_w-invfg ).

    END. /* Page 5 */
    WHEN 6 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/w-invest.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_w-invest ).
       /* Position in AB:  ( 6.24 , 11.00 ) */
       /* Size in UIB:  ( 1.86 , 10.80 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('3':U) NO-ERROR.

       /* Links to SmartWindow h_w-invest. */
       RUN add-link IN adm-broker-hdl ( h_b-invitm , 'Record':U , h_w-invest ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'quote':U , h_w-invest ).

    END. /* Page 6 */
    WHEN 7 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/v-invbil.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-invbil ).
       RUN set-position IN h_v-invbil ( 8.19 , 21.20 ) NO-ERROR.
       /* Size in UIB:  ( 5.48 , 109.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/vi-invh.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-invh ).
       RUN set-position IN h_vi-invh ( 4.81 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/p-invbil.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-invbil ).
       RUN set-position IN h_p-invbil ( 16.00 , 50.00 ) NO-ERROR.
       RUN set-size IN h_p-invbil ( 1.91 , 34.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_v-invbil. */
       RUN add-link IN adm-broker-hdl ( h_b-oeinv , 'Record':U , h_v-invbil ).
       RUN add-link IN adm-broker-hdl ( h_p-invbil , 'TableIO':U , h_v-invbil ).

       /* Links to SmartViewer h_vi-invh. */
       RUN add-link IN adm-broker-hdl ( h_b-oeinv , 'Record':U , h_vi-invh ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_vi-invh ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-invbil ,
             h_vi-invh , 'AFTER':U ).
    END. /* Page 7 */
    WHEN 8 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/v-oebolh.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-oebolh ).
       RUN set-position IN h_v-oebolh ( 5.05 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 8.33 , 143.60 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/b-oeboll.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-oeboll ).
       RUN set-position IN h_b-oeboll ( 13.86 , 4.00 ) NO-ERROR.
       RUN set-size IN h_b-oeboll ( 7.86 , 145.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-bolitm.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-bolitm ).
       RUN set-position IN h_p-bolitm ( 22.43 , 37.00 ) NO-ERROR.
       RUN set-size IN h_p-bolitm ( 1.76 , 82.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/q-invbol.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-invbol ).
       RUN set-position IN h_q-invbol ( 22.19 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.86 , 10.80 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('3':U) NO-ERROR.

       /* Links to SmartViewer h_v-oebolh. */
       RUN add-link IN adm-broker-hdl ( h_q-invbol , 'Record':U , h_v-oebolh ).

       /* Links to SmartNavBrowser h_b-oeboll. */
       RUN add-link IN adm-broker-hdl ( h_p-bolitm , 'TableIO':U , h_b-oeboll ).
       RUN add-link IN adm-broker-hdl ( h_v-oebolh , 'Record':U , h_b-oeboll ).

       /* Links to SmartPanel h_p-bolitm. */
       RUN add-link IN adm-broker-hdl ( h_b-oeboll , 'panel':U , h_p-bolitm ).

       /* Links to SmartQuery h_q-invbol. */
       RUN add-link IN adm-broker-hdl ( h_b-invitm , 'Record':U , h_q-invbol ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-oebolh ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-oeboll ,
             h_v-oebolh , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-bolitm ,
             h_b-oeboll , 'AFTER':U ).
    END. /* Page 8 */
    WHEN 9 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/w-invesf.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_w-invesf ).
       /* Position in AB:  ( 5.52 , 6.00 ) */
       /* Size in UIB:  ( 1.86 , 10.80 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('3':U) NO-ERROR.

       /* Links to SmartWindow h_w-invesf. */
       RUN add-link IN adm-broker-hdl ( h_b-invitm , 'Record':U , h_w-invesf ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'quote':U , h_w-invesf ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 9 */

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
  {src/adm/template/row-list.i "inv-head"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "inv-head"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FakeChangePage W-Win 
PROCEDURE FakeChangePage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE viCurrPage  AS INTEGER    NO-UNDO.

  RUN get-attribute ("current-page").
    viCurrPage = INT (RETURN-VALUE).

  IF NOT vlInitialized THEN DO:
    RUN Select-Page (2).
    RUN Select-Page (3).
    RUN Select-Page (4).
    RUN Select-Page (7).
    RUN Select-Page (8).
    RUN Select-Page (1).
    vlInitialized = YES.
  END.

  RUN Select-Page (viCurrPage).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hide-estimate W-Win 
PROCEDURE hide-estimate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 run select-page (li-prev-page).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HideSmartObjects W-Win 
PROCEDURE HideSmartObjects :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN local-hide IN h_b-invitm   NO-ERROR.
  RUN local-hide IN h_b-inmisc   NO-ERROR.
  RUN local-hide IN h_b-oeboll   NO-ERROR.
  RUN local-hide IN h_v-oeinv    NO-ERROR.
  RUN local-hide IN h_p-navico   NO-ERROR.
  RUN local-hide IN h_p-invvw    NO-ERROR.
  RUN local-hide IN h_vp-ivhld   NO-ERROR.
  RUN local-hide IN h_vp-initm   NO-ERROR.
  RUN local-hide IN h_p-invmis   NO-ERROR.
  RUN local-hide IN h_vi-invh    NO-ERROR.
  RUN local-hide IN h_v-invbil   NO-ERROR.
  RUN local-hide IN h_p-invbil   NO-ERROR.
  RUN local-hide IN h_v-oebolh   NO-ERROR.
  RUN local-hide IN h_p-bolitm   NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-change-page W-Win 
PROCEDURE local-change-page :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ls-est-no AS cha NO-UNDO.
  DEF VAR li-last-page AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  run get-attribute ("current-page").
  assign li-prev-page = li-cur-page
         li-cur-page = int(return-value).

  if li-cur-page = 6 then do:  /* estimate */
     li-last-page = li-prev-page.
     run get-link-handle in adm-broker-hdl (this-procedure,"estimate-target",output char-hdl).

     if valid-handle(widget-handle(char-hdl)) then do:
        run get-line-est in widget-handle(char-hdl) (output ls-est-no).        
        if ls-est-no = "" then do:
           message "SORRY, NO ESTIMATE EXISTS FOR THIS INVOICE." 
               view-as alert-box error.      
           run hide-estimate.
           return no-apply.        
        end.  
        ELSE DO:
            FIND FIRST est WHERE est.company = g_company AND
                                est.est-no = ls-est-no NO-LOCK NO-ERROR.
            IF AVAIL est AND est.est-type <= 4 THEN do:
                 RUN select-page (9).  
                 li-prev-page = li-last-page.
                 RETURN.
            END.
        END.
    end.                  
  end.

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
  IF AVAILABLE inv-head THEN
  RUN custom/setUserPrint.p (inv-head.company,'inv-hea_.',
                             'begin_inv,end_inv,begin_cust,end_cust,tb_reprint,tb_posted',
                             STRING(inv-head.inv-no) + ',' + STRING(inv-head.inv-no) + ',' +
                             inv-head.cust-no + ',' + inv-head.cust-no + ',' +
                             STRING(inv-head.printed) + ',' + STRING(inv-head.posted)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refreshBrowse W-Win 
PROCEDURE refreshBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN refreshBrowse IN h_b-oeinv.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refreshViewer W-Win 
PROCEDURE refreshViewer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* gdm - 11060802 - refreshes the fields on the viewer when called by the 
                     smart panel - specifically - print buton */
   RUN local-display-fields IN h_v-oeinv.

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
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.

  RUN select-page(2).
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,'addPlusButton-target',OUTPUT char-hdl).
  RUN addPlusButton IN WIDGET-HANDLE(char-hdl).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select_attcust W-Win 
PROCEDURE select_attcust :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF BUFFER b-oe-ordl FOR oe-ordl.
 DEF BUFFER b-oe-ord FOR oe-ord.
 DEF BUFFER b-cust FOR cust.

 DEF VAR v-order-no AS INT NO-UNDO.

 FIND FIRST b-inv-head WHERE
      b-inv-head.rec_key EQ rec_key_value
      NO-LOCK NO-ERROR.

 IF AVAIL b-inv-head THEN
 DO:
    FIND FIRST b-inv-line OF b-inv-head 
         WHERE b-inv-line.ord-no NE 0
         NO-LOCK NO-ERROR.

    IF NOT AVAIL b-inv-line THEN
       FIND FIRST b-inv-misc OF b-inv-head 
            WHERE b-inv-misc.ord-no NE 0
            NO-LOCK NO-ERROR.

    v-order-no = IF AVAIL b-inv-line THEN b-inv-line.ord-no ELSE
                 IF AVAIL b-inv-misc THEN b-inv-misc.ord-no ELSE 0.

    FIND FIRST b-cust WHERE
         b-cust.company EQ b-inv-head.company AND
         b-cust.cust-no EQ b-inv-head.cust-no
         NO-LOCK NO-ERROR.

     IF AVAIL b-cust THEN DO:
         {methods/select_attcust.i
            b-cust.rec_key
            "'Customer: ' + b-cust.cust-no + ' - ' + 'Name: ' + b-cust.name"
            v-order-no}
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
  {src/adm/template/snd-list.i "inv-head"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setUserPrint W-Win 
PROCEDURE setUserPrint :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF AVAILABLE inv-head THEN
  RUN custom/setUserPrint.p (inv-head.company,'inv-hea_.',
                             'begin_cust-no,end_cust-no,begin_inv,end_inv,begin_date,end_date,begin_bol,end_bol,tb_reprint,tb_posted',
                             inv-head.cust-no + ',' + inv-head.cust-no + ',' +
                             STRING(inv-head.inv-no) + ',' + STRING(inv-head.inv-no) + ',' +
                             STRING(inv-head.inv-date) + ',' + STRING(inv-head.inv-date) + ',' +
                             STRING(inv-head.bol-no) + ',' + STRING(inv-head.bol-no) + ',' +
                             STRING(inv-head.printed) + ',' + STRING(inv-head.posted)).

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

