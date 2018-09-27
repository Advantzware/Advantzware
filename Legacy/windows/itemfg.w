&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: windows/itemfg.w
          
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
&SCOPED-DEFINE h_Browse01 h_b-itemfg
&SCOPED-DEFINE h_Object01 h_p-fg-bj-l
&SCOPED-DEFINE h_Object02 h_p-fgset
&SCOPED-DEFINE h_Object03 h_v-eitem
&SCOPED-DEFINE h_Object04 h_p-updven
&SCOPED-DEFINE h_Object05 h_pricechg
&SCOPED-DEFINE moveRight {&h_Object03},{&h_Object04},{&h_Object05}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
&scoped-define item_spec FGITEM

DEF VAR ll-secure AS LOG INIT NO NO-UNDO.
DEF VAR h_fileload AS HANDLE NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES itemfg
&Scoped-define FIRST-EXTERNAL-TABLE itemfg


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR itemfg.
/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_attach AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-ebfg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-eitem AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-itemfg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-itemsp AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-ordfgi AS HANDLE NO-UNDO.
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_export AS HANDLE NO-UNDO.
DEFINE VARIABLE h_export2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_f-add AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fg-set AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fgijob AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_import AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemfg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemfg-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemfg-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemfg-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemfg2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemfg2-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemfgpo AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemfgqty AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemfgt AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itmfgink AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itmfgink-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_locw AS HANDLE NO-UNDO.
DEFINE VARIABLE h_movecol AS HANDLE NO-UNDO.
DEFINE VARIABLE h_movecol-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_options AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-calcc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-calcq AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-fg-bj-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-fg-bj-l AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-fgset AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-overq AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updclr AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updimg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updinv AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updinv-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-upditm AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updncp AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updven AS HANDLE NO-UNDO.
DEFINE VARIABLE h_pricechg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_pv-graph AS HANDLE NO-UNDO.
DEFINE VARIABLE h_smartmsg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-eitem AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-fgimg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-itemsp AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-navest AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-navest-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-spcard AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 159.6 BY 23.95
         BGCOLOR 15 .

DEFINE FRAME OPTIONS-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 61 ROW 1
         SIZE 99 BY 1.91
         BGCOLOR 15 .

DEFINE FRAME message-frame
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 1
         SIZE 52 BY 2
         BGCOLOR 15 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: ASI.itemfg
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 5
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Finished Goods Item Inventory"
         HEIGHT             = 24.24
         WIDTH              = 160.6
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

ASSIGN XXTABVALXX = FRAME message-frame:MOVE-BEFORE-TAB-ITEM (FRAME OPTIONS-FRAME:HANDLE)
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
ON END-ERROR OF W-Win /* Finished Goods Item Inventory */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Finished Goods Item Inventory */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
    IF VALID-HANDLE(h_fileload) THEN
     DELETE OBJECT h_fileload.

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */
SESSION:SET-WAIT-STATE('').

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
       RUN set-position IN h_smartmsg ( 1.48 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.14 , 32.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/options.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_options ).
       RUN set-position IN h_options ( 1.00 , 29.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 55.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/f-add.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_f-add ).
       RUN set-position IN h_f-add ( 1.00 , 29.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Brws Items|View Item|Inventory|Totals/CP#|Bin/Jobs|Set parts|Colors|Vend Cost|History|Image|POs' + ',
                     FOLDER-TAB-TYPE = 2':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 2.91 , 1.60 ) NO-ERROR.
       RUN set-size IN h_folder ( 21.91 , 159.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/attach.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_attach ).
       RUN set-position IN h_attach ( 1.00 , 85.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/exit.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.00 , 92.20 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartObject h_options. */
       RUN add-link IN adm-broker-hdl ( h_b-itemfg , 'spec':U , h_options ).

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Links to SmartObject h_attach. */
       RUN add-link IN adm-broker-hdl ( h_b-itemfg , 'attach':U , h_attach ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_folder ,
             FRAME OPTIONS-FRAME:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_f-add ,
             h_options , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_attach ,
             h_f-add , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_exit ,
             h_attach , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/import.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_import ).
       RUN set-position IN h_import ( 1.00 , 5.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/export.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_export ).
       RUN set-position IN h_export ( 1.00 , 13.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/movecol.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_movecol-2 ).
       RUN set-position IN h_movecol-2 ( 1.00 , 81.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/b-itemfg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-itemfg ).
       RUN set-position IN h_b-itemfg ( 4.33 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 20.00 , 156.00 ) */

       /* Links to SmartViewer h_import. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'import':U , h_import ).

       /* Links to SmartObject h_export. */
       RUN add-link IN adm-broker-hdl ( h_b-itemfg , 'export-xl':U , h_export ).

       /* Links to SmartObject h_movecol-2. */
       RUN add-link IN adm-broker-hdl ( h_b-itemfg , 'move-columns':U , h_movecol-2 ).

       /* Links to SmartNavBrowser h_b-itemfg. */
       RUN add-link IN adm-broker-hdl ( h_b-itemfg , 'Record':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_export ,
             h_import , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_movecol-2 ,
             FRAME OPTIONS-FRAME:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-itemfg ,
             h_folder , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/itemfg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_itemfg-2 ).
       RUN set-position IN h_itemfg-2 ( 4.81 , 9.00 ) NO-ERROR.
       /* Size in UIB:  ( 17.14 , 144.60 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/p-upditm.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-upditm ).
       RUN set-position IN h_p-upditm ( 22.19 , 58.00 ) NO-ERROR.
       RUN set-size IN h_p-upditm ( 2.14 , 59.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/p-calcc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_p-calcc ).
       RUN set-position IN h_p-calcc ( 22.19 , 117.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.14 , 17.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/v-spcard.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-spcard ).
       RUN set-position IN h_v-spcard ( 22.19 , 134.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.14 , 15.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-navest.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-navest-2 ).
       RUN set-position IN h_v-navest-2 ( 22.43 , 12.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 34.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_itemfg-2. */
       RUN add-link IN adm-broker-hdl ( h_b-itemfg , 'Record':U , h_itemfg-2 ).
       RUN add-link IN adm-broker-hdl ( h_p-upditm , 'TableIO':U , h_itemfg-2 ).
       RUN add-link IN adm-broker-hdl ( h_v-spcard , 'spec-card':U , h_itemfg-2 ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'add-item':U , h_itemfg-2 ).

       /* Links to SmartViewer h_p-calcc. */
       RUN add-link IN adm-broker-hdl ( h_itemfg-2 , 'calc-cost':U , h_p-calcc ).

       /* Links to SmartViewer h_v-navest-2. */
       RUN add-link IN adm-broker-hdl ( h_b-itemfg , 'nav-itm':U , h_v-navest-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_itemfg-2 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-upditm ,
             h_itemfg-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-calcc ,
             h_p-upditm , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-spcard ,
             h_p-calcc , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-navest-2 ,
             h_v-spcard , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/itemfg2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itemfg2 ).
       RUN set-position IN h_itemfg2 ( 6.00 , 9.00 ) NO-ERROR.
       /* Size in UIB:  ( 10.00 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/p-updinv.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updinv ).
       RUN set-position IN h_p-updinv ( 16.24 , 115.00 ) NO-ERROR.
       RUN set-size IN h_p-updinv ( 2.24 , 31.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-navest.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-navest ).
       RUN set-position IN h_v-navest ( 16.95 , 23.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 34.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_itemfg2. */
       RUN add-link IN adm-broker-hdl ( h_b-itemfg , 'Record':U , h_itemfg2 ).
       RUN add-link IN adm-broker-hdl ( h_p-updinv , 'TableIO':U , h_itemfg2 ).

       /* Links to SmartViewer h_v-navest. */
       RUN add-link IN adm-broker-hdl ( h_b-itemfg , 'nav-itm':U , h_v-navest ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_itemfg2 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updinv ,
             h_itemfg2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-navest ,
             h_p-updinv , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewerid/itemfg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itemfg-3 ).
       RUN set-position IN h_itemfg-3 ( 4.57 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 155.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/itemfgt.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itemfgt ).
       RUN set-position IN h_itemfgt ( 6.71 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 14.52 , 99.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/p-updinv.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updinv-2 ).
       RUN set-position IN h_p-updinv-2 ( 21.48 , 35.00 ) NO-ERROR.
       RUN set-size IN h_p-updinv-2 ( 2.24 , 38.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartViewer h_itemfg-3. */
       RUN add-link IN adm-broker-hdl ( h_itemfg-2 , 'Record':U , h_itemfg-3 ).

       /* Links to SmartViewer h_itemfgt. */
       RUN add-link IN adm-broker-hdl ( h_itemfg-2 , 'Record':U , h_itemfgt ).
       RUN add-link IN adm-broker-hdl ( h_p-updinv-2 , 'TableIO':U , h_itemfgt ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_itemfg-3 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_itemfgt ,
             h_itemfg-3 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updinv-2 ,
             h_itemfgt , 'AFTER':U ).
    END. /* Page 4 */
    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewerid/itemfg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itemfg ).
       RUN set-position IN h_itemfg ( 4.33 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 155.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/locw.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_locw ).
       RUN set-position IN h_locw ( 6.24 , 3.00 ) NO-ERROR.
       RUN set-size IN h_locw ( 5.24 , 157.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/p-fg-bj-2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_p-fg-bj-2 ).
       RUN set-position IN h_p-fg-bj-2 ( 11.71 , 52.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.10 , 95.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/fgijob.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_fgijob ).
       RUN set-position IN h_fgijob ( 12.91 , 3.00 ) NO-ERROR.
       RUN set-size IN h_fgijob ( 6.91 , 157.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/itemfgqty.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itemfgqty ).
       RUN set-position IN h_itemfgqty ( 19.81 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.76 , 130.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/p-fg-bj-l.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_p-fg-bj-l ).
       RUN set-position IN h_p-fg-bj-l ( 19.81 , 132.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.52 , 28.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/p-calcq.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_p-calcq ).
       RUN set-position IN h_p-calcq ( 21.48 , 138.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.52 , 17.20 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/p-overq.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_p-overq ).
       RUN set-position IN h_p-overq ( 23.14 , 138.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.52 , 17.20 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_itemfg. */
       RUN add-link IN adm-broker-hdl ( h_b-itemfg , 'Record':U , h_itemfg ).

       /* Links to SmartBrowser h_locw. */
       RUN add-link IN adm-broker-hdl ( h_b-itemfg , 'Record':U , h_locw ).

       /* Links to SmartViewer h_p-fg-bj-2. */
       RUN add-link IN adm-broker-hdl ( h_b-itemfg , 'Record':U , h_p-fg-bj-2 ).

       /* Links to SmartBrowser h_fgijob. */
       RUN add-link IN adm-broker-hdl ( h_b-itemfg , 'Record':U , h_fgijob ).

       /* Links to SmartViewer h_itemfgqty. */
       RUN add-link IN adm-broker-hdl ( h_b-itemfg , 'Record':U , h_itemfgqty ).

       /* Links to SmartViewer h_p-fg-bj-l. */
       RUN add-link IN adm-broker-hdl ( h_fgijob , 'cost':U , h_p-fg-bj-l ).

       /* Links to SmartViewer h_p-calcq. */
       RUN add-link IN adm-broker-hdl ( h_itemfgqty , 'calc-qty':U , h_p-calcq ).

       /* Links to SmartViewer h_p-overq. */
       RUN add-link IN adm-broker-hdl ( h_itemfgqty , 'override-qty':U , h_p-overq ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_itemfg ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_locw ,
             h_itemfg , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-fg-bj-2 ,
             h_locw , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_fgijob ,
             h_p-fg-bj-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_itemfgqty ,
             h_fgijob , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-fg-bj-l ,
             h_itemfgqty , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-calcq ,
             h_p-fg-bj-l , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-overq ,
             h_p-calcq , 'AFTER':U ).
    END. /* Page 5 */
    WHEN 6 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewerid/itemfg2.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itemfg2-2 ).
       RUN set-position IN h_itemfg2-2 ( 4.52 , 3.20 ) NO-ERROR.
       /* Size in UIB:  ( 3.29 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/export.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_export2 ).
       RUN set-position IN h_export2 ( 1.00 , 21.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/fg-set.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_fg-set ).
       RUN set-position IN h_fg-set ( 8.86 , 9.00 ) NO-ERROR.
       RUN set-size IN h_fg-set ( 8.62 , 141.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-fgset.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-fgset ).
       RUN set-position IN h_p-fgset ( 18.86 , 32.00 ) NO-ERROR.
       RUN set-size IN h_p-fgset ( 1.91 , 81.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartViewer h_itemfg2-2. */
       RUN add-link IN adm-broker-hdl ( h_itemfg-2 , 'Record':U , h_itemfg2-2 ).

       /* Links to SmartObject h_export2. */
       RUN add-link IN adm-broker-hdl ( h_fg-set , 'export-xl':U , h_export2 ).

       /* Links to SmartBrowser h_fg-set. */
       RUN add-link IN adm-broker-hdl ( h_itemfg-2 , 'Record':U , h_fg-set ).
       RUN add-link IN adm-broker-hdl ( h_p-fgset , 'TableIO':U , h_fg-set ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_fg-set ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-fgset ,
             h_fg-set , 'AFTER':U ).
    END. /* Page 6 */
    WHEN 7 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewerid/itemfg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itemfg-4 ).
       RUN set-position IN h_itemfg-4 ( 4.57 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 155.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/itmfgink.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_itmfgink ).
       RUN set-position IN h_itmfgink ( 6.71 , 3.00 ) NO-ERROR.
       RUN set-size IN h_itmfgink ( 17.62 , 93.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/itmfgink.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_itmfgink-2 ).
       RUN set-position IN h_itmfgink-2 ( 6.71 , 97.00 ) NO-ERROR.
       /* Size in UIB:  ( 6.29 , 61.40 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/p-updclr.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updclr ).
       RUN set-position IN h_p-updclr ( 13.14 , 97.00 ) NO-ERROR.
       RUN set-size IN h_p-updclr ( 2.14 , 61.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/b-ebfg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-ebfg ).
       RUN set-position IN h_b-ebfg ( 15.29 , 97.00 ) NO-ERROR.
       RUN set-size IN h_b-ebfg ( 9.10 , 61.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2,1':U) NO-ERROR.

       /* Links to SmartViewer h_itemfg-4. */
       RUN add-link IN adm-broker-hdl ( h_itemfg-2 , 'Record':U , h_itemfg-4 ).

       /* Links to SmartNavBrowser h_itmfgink. */
       RUN add-link IN adm-broker-hdl ( h_b-itemfg , 'Record':U , h_itmfgink ).

       /* Links to SmartViewer h_itmfgink-2. */
       RUN add-link IN adm-broker-hdl ( h_itmfgink , 'Record':U , h_itmfgink-2 ).
       RUN add-link IN adm-broker-hdl ( h_p-updclr , 'TableIO':U , h_itmfgink-2 ).

       /* Links to SmartNavBrowser h_b-ebfg. */
       RUN add-link IN adm-broker-hdl ( h_b-itemfg , 'Record':U , h_b-ebfg ).
       RUN add-link IN adm-broker-hdl ( h_itmfgink-2 , 'ebfg':U , h_b-ebfg ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_itemfg-4 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_itmfgink ,
             h_itemfg-4 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_itmfgink-2 ,
             h_itmfgink , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updclr ,
             h_itmfgink-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-ebfg ,
             h_p-updclr , 'AFTER':U ).
    END. /* Page 7 */
    WHEN 8 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/b-eitem.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-eitem ).
       RUN set-position IN h_b-eitem ( 4.57 , 3.00 ) NO-ERROR.
       RUN set-size IN h_b-eitem ( 19.52 , 58.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/v-eitem.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_v-eitem ).
       RUN set-position IN h_v-eitem ( 4.57 , 60.00 ) NO-ERROR.
       /* Size in UIB:  ( 17.62 , 99.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/p-updven.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updven ).
       RUN set-position IN h_p-updven ( 22.43 , 65.00 ) NO-ERROR.
       RUN set-size IN h_p-updven ( 2.14 , 59.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/pricechg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_pricechg ).
       RUN set-position IN h_pricechg ( 22.43 , 127.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.05 , 17.20 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartNavBrowser h_b-eitem. */
       RUN add-link IN adm-broker-hdl ( h_b-itemfg , 'Record':U , h_b-eitem ).

       /* Links to SmartViewer h_v-eitem. */
       RUN add-link IN adm-broker-hdl ( h_b-eitem , 'Record':U , h_v-eitem ).
       RUN add-link IN adm-broker-hdl ( h_p-updven , 'TableIO':U , h_v-eitem ).

       /* Links to SmartViewer h_pricechg. */
       RUN add-link IN adm-broker-hdl ( h_v-eitem , 'price-change':U , h_pricechg ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-eitem ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-eitem ,
             h_b-eitem , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updven ,
             h_v-eitem , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_pricechg ,
             h_p-updven , 'AFTER':U ).
    END. /* Page 8 */
    WHEN 9 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/movecol.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_movecol ).
       RUN set-position IN h_movecol ( 1.00 , 81.20 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oeinq/b-ordfgi.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-ordfgi ).
       RUN set-position IN h_b-ordfgi ( 4.33 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 20.00 , 156.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartObject h_movecol. */
       RUN add-link IN adm-broker-hdl ( h_b-ordfgi , 'move-columns':U , h_movecol ).

       /* Links to SmartNavBrowser h_b-ordfgi. */
       RUN add-link IN adm-broker-hdl ( h_b-itemfg , 'Record':U , h_b-ordfgi ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_movecol ,
             FRAME OPTIONS-FRAME:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-ordfgi ,
             h_folder , 'AFTER':U ).
    END. /* Page 9 */
    WHEN 10 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/v-fgimg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-fgimg ).
       RUN set-position IN h_v-fgimg ( 4.57 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 17.14 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/p-updimg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updimg ).
       RUN set-position IN h_p-updimg ( 22.19 , 51.00 ) NO-ERROR.
       RUN set-size IN h_p-updimg ( 1.76 , 31.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/pv-graph.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_pv-graph ).
       RUN set-position IN h_pv-graph ( 22.19 , 83.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.76 , 17.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_v-fgimg. */
       RUN add-link IN adm-broker-hdl ( h_b-itemfg , 'Record':U , h_v-fgimg ).
       RUN add-link IN adm-broker-hdl ( h_p-updimg , 'TableIO':U , h_v-fgimg ).
       RUN add-link IN adm-broker-hdl ( h_pv-graph , 'graph':U , h_v-fgimg ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-fgimg ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updimg ,
             h_v-fgimg , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_pv-graph ,
             h_p-updimg , 'AFTER':U ).
    END. /* Page 10 */
    WHEN 11 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/itemfgpo.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itemfgpo ).
       RUN set-position IN h_itemfgpo ( 4.81 , 9.00 ) NO-ERROR.
       RUN set-size IN h_itemfgpo ( 19.52 , 145.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartNavBrowser h_itemfgpo. */
       RUN add-link IN adm-broker-hdl ( h_b-itemfg , 'Record':U , h_itemfgpo ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_itemfgpo ,
             h_folder , 'AFTER':U ).
    END. /* Page 11 */
    WHEN 12 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/b-itemsp.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-itemsp ).
       RUN set-position IN h_b-itemsp ( 6.24 , 5.00 ) NO-ERROR.
       RUN set-size IN h_b-itemsp ( 5.95 , 145.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/v-itemsp.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_v-itemsp ).
       RUN set-position IN h_v-itemsp ( 13.14 , 21.00 ) NO-ERROR.
       /* Size in UIB:  ( 6.67 , 107.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updncp.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updncp ).
       RUN set-position IN h_p-updncp ( 20.52 , 44.00 ) NO-ERROR.
       RUN set-size IN h_p-updncp ( 1.76 , 46.80 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartNavBrowser h_b-itemsp. */
       RUN add-link IN adm-broker-hdl ( h_itemfg-2 , 'Record':U , h_b-itemsp ).

       /* Links to SmartViewer h_v-itemsp. */
       RUN add-link IN adm-broker-hdl ( h_b-itemsp , 'Record':U , h_v-itemsp ).
       RUN add-link IN adm-broker-hdl ( h_p-updncp , 'TableIO':U , h_v-itemsp ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-itemsp ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-itemsp ,
             h_b-itemsp , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updncp ,
             h_v-itemsp , 'AFTER':U ).
    END. /* Page 12 */

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
  {src/adm/template/row-list.i "itemfg"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "itemfg"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changeRecord W-Win 
PROCEDURE changeRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipMove AS CHARACTER NO-UNDO.

  {methods/run_link.i "RECORD-SOURCE" "changeRecord" "(ipMove)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-security W-Win 
PROCEDURE check-security :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAM ip-type   AS INT NO-UNDO.
  DEF OUTPUT PARAM op-secure AS LOG NO-UNDO.


  IF NOT ll-secure THEN RUN sys/ref/d-passwd.w (ip-type, OUTPUT ll-secure).

  op-secure = ll-secure.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE containerName W-Win 
PROCEDURE containerName :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER opContainerName AS CHARACTER NO-UNDO.

  opContainerName = PROGRAM-NAME(1).

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
  VIEW FRAME message-frame IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-message-frame}
  VIEW FRAME OPTIONS-FRAME IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-OPTIONS-FRAME}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filterTagBins W-Win 
PROCEDURE filterTagBins :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER iplShowZeroBins AS LOG NO-UNDO.
DEF INPUT PARAMETER iplTagBins AS CHAR NO-UNDO.
RUN filterTagBins IN h_fgijob (INPUT iplShowZeroBins, INPUT iplTagBins ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filterZeroBins W-Win 
PROCEDURE filterZeroBins :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER iplShowZeroBins AS LOG NO-UNDO.
RUN filterZeroBins IN h_fgijob (INPUT iplShowZeroBins).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE import-file W-Win 
PROCEDURE import-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN util/updatefg.w PERSISTENT SET h_fileload.
RUN adm-initialize IN h_fileload.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-objects W-Win 
PROCEDURE local-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-current-page AS INT NO-UNDO.
  DEFINE VARIABLE h_custpart AS HANDLE NO-UNDO.
  DEFINE VARIABLE h_p-updsav-3 AS HANDLE NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN v-current-page = INTEGER(RETURN-VALUE).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-objects':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF v-current-page EQ 4                                                AND
    CAN-FIND(FIRST asi._file WHERE asi._file._file-name EQ "cust-part") THEN DO:
      RUN init-object IN THIS-PROCEDURE (
            INPUT  'browsers/custpart.w':U ,
            INPUT  FRAME F-Main:HANDLE ,
            INPUT  'Initial-Lock = NO-LOCK,
                    Hide-on-Init = no,
                    Disable-on-Init = no,
                    Layout = ,
                    Create-On-Add = Yes':U ,
            OUTPUT h_custpart ).
      RUN set-position IN h_custpart ( 8.95 , 102.00 ) NO-ERROR.
      RUN set-size IN h_custpart ( 15.71 , 47.00 ) NO-ERROR.

      RUN init-object IN THIS-PROCEDURE (
            INPUT  'fg/p-upditm.w':U ,
            INPUT  FRAME F-Main:HANDLE ,
            INPUT  'Edge-Pixels = 2,
                    SmartPanelType = Update,
                    AddFunction = One-Record':U ,
            OUTPUT h_p-updsav-3 ).
      RUN set-position IN h_p-updsav-3 ( 6.95 , 102.00 ) NO-ERROR.
      RUN set-size IN h_p-updsav-3 ( 1.76 , 47.00 ) NO-ERROR.

      /* Links to SmartBrowser h_custpart. */
      RUN add-link IN adm-broker-hdl ( h_itemfg-2 , 'Record':U , h_custpart ).
      RUN add-link IN adm-broker-hdl ( h_p-updsav-3 , 'TableIO':U , h_custpart ).
      RUN view-page (4).
 END.

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
    IF VALID-HANDLE(h_fileload) THEN
     DELETE OBJECT h_fileload.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

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
    IF VALID-HANDLE(h_fileload) THEN
     DELETE OBJECT h_fileload.
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view W-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/winReSizeMax.i}
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
  def var char-hdl as cha no-undo.
  
  run select-page(2).
  run get-link-handle in adm-broker-hdl(this-procedure,"add-item-target", output char-hdl).
  run add-item in widget-handle(char-hdl).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select_att W-Win 
PROCEDURE select_att :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {methods/select_att.i}

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
  {src/adm/template/snd-list.i "itemfg"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-loc W-Win 
PROCEDURE set-loc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-loc AS CHARACTER NO-UNDO.
    
    RUN set-pass-loc IN h_fgijob (INPUT ip-loc).

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

