&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: ap\w-apinv.w
          
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
&SCOPED-DEFINE h_Browse01 h_b-apinq2
&SCOPED-DEFINE h_Object01 h_p-cashl
&SCOPED-DEFINE h_Object02 h_v-recbrd

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
&SCOPED-DEFINE asi-exit local-exit2

/* Hardcoded for now. Should have a field where we can link the import with api */
DEFINE VARIABLE cImportAPIRoute AS CHARACTER NO-UNDO INITIAL "/api/810APInvoice".

DEFINE VARIABLE hdInboundProcs AS HANDLE    NO-UNDO.
RUN api/InboundProcs.p PERSISTENT SET hdInboundProcs.

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
&Scoped-define EXTERNAL-TABLES ap-inv
&Scoped-define FIRST-EXTERNAL-TABLE ap-inv


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ap-inv.
/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_attachvinv-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-apinq2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-apinvl AS HANDLE NO-UNDO.
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_f-add AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_import AS HANDLE NO-UNDO.
DEFINE VARIABLE h_importapi AS HANDLE NO-UNDO.
DEFINE VARIABLE h_movecol AS HANDLE NO-UNDO.
DEFINE VARIABLE h_options AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-arinv AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-cashl AS HANDLE NO-UNDO.
DEFINE VARIABLE h_smartmsg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-apinv AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-navest AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-recbrd AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vp-aphld AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-poordl AS HANDLE NO-UNDO.
DEFINE VARIABLE h_f-porec AS HANDLE NO-UNDO. 
DEFINE VARIABLE h_movecol-2 AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 155 BY 24
         BGCOLOR 15 .

DEFINE FRAME message-frame
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 46 ROW 2.91
         SIZE 105 BY 1.43
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
   External Tables: asi.ap-inv
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
         TITLE              = "Vendor Invoices"
         HEIGHT             = 24
         WIDTH              = 155
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
         RESIZE             = yes
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
ON END-ERROR OF W-Win /* Vendor Invoices */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */

  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Vendor Invoices */
DO:
    DEF VAR op-delete-choice AS LOG NO-UNDO.
    
    IF VALID-HANDLE(hdInboundProcs) THEN
        DELETE PROCEDURE hdInboundProcs.

    /* This ADM code must be left here in order for the SmartWindow
       and its descendents to terminate properly on exit. */
    RUN exit-window(OUTPUT op-delete-choice).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}
{custom/initializeprocs.i}
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
       RUN set-position IN h_smartmsg ( 1.00 , 72.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.14 , 32.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/attachvinv.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_attachvinv-2 ).
       RUN set-position IN h_attachvinv-2 ( 1.00 , 61.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Browse|Detail|Receipts' + ',
                     FOLDER-TAB-TYPE = 2':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 3.14 , 2.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 21.67 , 153.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/f-add.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_f-add ).
       RUN set-position IN h_f-add ( 1.00 , 77.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/options.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_options ).
       RUN set-position IN h_options ( 1.00 , 85.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 55.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/exit.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.00 , 141.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartObject h_attachvinv-2. */
       RUN add-link IN adm-broker-hdl ( h_b-apinq2 , 'attachinv':U , h_attachvinv-2 ).
	   RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'udficon':U , h_options ).
       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_folder ,
             FRAME message-frame:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_f-add ,
             h_attachvinv-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_options ,
             h_f-add , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_exit ,
             h_options , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/importapi.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_importapi ).
       RUN set-position IN h_importapi ( 1.00 , 45.40 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/import.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_import ).
       RUN set-position IN h_import ( 1.00 , 53.20 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'apinq/b-apinq2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-apinq2 ).
       RUN set-position IN h_b-apinq2 ( 4.57 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 19.52 , 145.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/movecol.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_movecol ).
       RUN set-position IN h_movecol ( 1.00 , 69.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       /* Links to SmartViewer h_importapi. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'import-api':U , h_importapi ).

       /* Links to SmartViewer h_import. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'import':U , h_import ).

       /* Links to SmartNavBrowser h_b-apinq2. */
       RUN add-link IN adm-broker-hdl ( h_b-apinq2 , 'Record':U , THIS-PROCEDURE ).

       /* Links to SmartObject h_movecol. */
       RUN add-link IN adm-broker-hdl ( h_b-apinq2 , 'move-columns':U , h_movecol ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_import ,
             h_importapi , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-apinq2 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_movecol ,
             h_attachvinv-2 , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'ap/v-apinv.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = No':U ,
             OUTPUT h_v-apinv ).
       RUN set-position IN h_v-apinv ( 4.81 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 5.95 , 149.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-navest.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-navest ).
       RUN set-position IN h_v-navest ( 11.00 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 34.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-arinv.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-arinv ).
       RUN set-position IN h_p-arinv ( 11.00 , 42.40 ) NO-ERROR.
       RUN set-size IN h_p-arinv ( 1.67 , 82.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'ap/vp-aphld.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vp-aphld ).
       RUN set-position IN h_vp-aphld ( 11.00 , 128.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.67 , 17.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'ap/b-apinvl.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_b-apinvl ).
       RUN set-position IN h_b-apinvl ( 12.91 , 3.00 ) NO-ERROR.
       RUN set-size IN h_b-apinvl ( 9.05 , 150.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-cashl.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-cashl ).
       RUN set-position IN h_p-cashl ( 22.19 , 18.00 ) NO-ERROR.
       RUN set-size IN h_p-cashl ( 2.00 , 82.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'ap/v-recbrd.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = ?':U ,
             OUTPUT h_v-recbrd ).
       RUN set-position IN h_v-recbrd ( 22.19 , 101.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.14 , 47.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_v-apinv. */
       RUN add-link IN adm-broker-hdl ( h_b-apinq2 , 'Record':U , h_v-apinv ).
       RUN add-link IN adm-broker-hdl ( h_p-arinv , 'TableIO':U , h_v-apinv ).
       RUN add-link IN adm-broker-hdl ( h_vp-aphld , 'hold':U , h_v-apinv ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'add-inv':U , h_v-apinv ).

       /* Links to SmartViewer h_v-navest. */
       RUN add-link IN adm-broker-hdl ( h_b-apinq2 , 'nav-itm':U , h_v-navest ).

       /* Links to SmartNavBrowser h_b-apinvl. */
       RUN add-link IN adm-broker-hdl ( h_p-cashl , 'TableIO':U , h_b-apinvl ).
       RUN add-link IN adm-broker-hdl ( h_v-apinv , 'clear':U , h_b-apinvl ).
       RUN add-link IN adm-broker-hdl ( h_v-apinv , 'Record':U , h_b-apinvl ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'add-line':U , h_b-apinvl ).

       /* Links to SmartPanel h_p-cashl. */
       RUN add-link IN adm-broker-hdl ( h_v-apinv , 'adding-line':U , h_p-cashl ).

       /* Links to SmartViewer h_v-recbrd. */
       RUN add-link IN adm-broker-hdl ( h_b-apinvl , 'recalc':U , h_v-recbrd ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-apinv ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-navest ,
             h_v-apinv , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-arinv ,
             h_v-navest , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_vp-aphld ,
             h_p-arinv , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-apinvl ,
             h_vp-aphld , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-cashl ,
             h_b-apinvl , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-recbrd ,
             h_p-cashl , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'po/vi-poord1.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-poordl ).
       RUN set-position IN h_vi-poordl ( 5.05 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.38 , 144.00 ) */
    
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'poinq/f-porec.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_f-porec ).
       RUN set-position IN h_f-porec ( 8.00 , 4.00 ) NO-ERROR.
      RUN set-size IN h_f-porec ( 12.38 , 146.00 ) NO-ERROR. 
       
        RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/movecol.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_movecol-2 ).
       RUN set-position IN h_movecol-2 ( 1.00 , 24.80 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */
       
       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.
       
       /* Links to SmartViewer h_movecol. */
       RUN add-link IN adm-broker-hdl ( h_f-porec , 'move-columns':U , h_movecol-2 ). 
              
       /* Adjust the tab order of the smart objects. */
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
  {src/adm/template/row-list.i "ap-inv"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ap-inv"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exit-window W-Win 
PROCEDURE exit-window :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF OUTPUT PARAM op-delete-choice AS LOG INIT YES NO-UNDO.
    
   DEF VAR char-hdl AS cha NO-UNDO.
   
   RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"add-line-target",OUTPUT char-hdl).

   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN undo-added IN WIDGET-HANDLE(char-hdl) (OUTPUT op-delete-choice).

   IF op-delete-choice THEN
   DO:
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      RETURN NO-APPLY.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE import-api-file W-Win 
PROCEDURE import-api-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lSuccess       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cImportPath    AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE cProcessedPath AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE cFailedPath    AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE cCharHdl       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFileName      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFullFileName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcRequestData  AS LONGCHAR  NO-UNDO.

    MESSAGE "Do you want to import EDI invoices?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
        UPDATE lAnswer AS LOGICAL.    

    /* The below code should be moved to a utility where user will be prompted
       to select the files from the API import path to process through API */
    IF NOT lAnswer THEN
        RETURN.

    RUN Inbound_GetAPIRouteImportPath IN hdInboundProcs (
        INPUT  g_company,
        INPUT  cImportAPIRoute,
        OUTPUT cImportPath,
        OUTPUT lSuccess,
        OUTPUT cMessage
        ).

    IF NOT lSuccess THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    
    RUN FileSys_ValidateDirectory (
        INPUT  cImportPath,
        OUTPUT lSuccess,
        OUTPUT cMessage
        ).
    IF NOT lSuccess THEN DO:
        MESSAGE "Import path '" + cImportPath + "' does not exist!"
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    FILE-INFO:FILE-NAME = cImportPath.

    ASSIGN
        cImportPath    = FILE-INFO:FULL-PATHNAME
        cProcessedPath = cImportPath + "\" + "Processed"
        cFailedPath    = cImportPath + "\" + "Failed"
        .
        
    /* Validates and creates a Processed directory inside import path */
    RUN FileSys_CreateDirectory (
        INPUT  cProcessedPath,
        OUTPUT lSuccess,
        OUTPUT cMessage
        ) NO-ERROR.

    /* Validates and creates a Failed directory inside import path */
    RUN FileSys_CreateDirectory (
        INPUT  cFailedPath,
        OUTPUT lSuccess,
        OUTPUT cMessage
        ) NO-ERROR.

    SESSION:SET-WAIT-STATE("GENERAL").
    
    INPUT FROM OS-DIR(cImportPath).
    REPEAT:
        IMPORT cFileName.
       
        IF cFileName MATCHES "*.XML" THEN DO:
            cFullFileName = cImportPath + "\" + cFileName.

            COPY-LOB FROM FILE cFullFileName TO lcRequestData.

            RUN Inbound_CreateAndProcessRequestForAPIRoute IN hdInboundProcs (
                INPUT  g_company,
                INPUT  cImportAPIRoute,
                INPUT  lcRequestData,
                OUTPUT lSuccess,
                OUTPUT cMessage
                ).

            IF lSuccess THEN
                OS-COPY VALUE(cFullFileName) VALUE(cProcessedPath + "\" + cFileName).
            ELSE
                OS-COPY VALUE(cFullFileName) VALUE(cFailedPath + "\" + cFileName).
                
            OS-DELETE VALUE(cFullFileName).
        END.
    END.    
    INPUT CLOSE.
    
    SESSION:SET-WAIT-STATE("").
    
    MESSAGE "Import complete"
        VIEW-AS ALERT-BOX INFORMATION.
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
    DEFINE VARIABLE cCharHdl AS CHARACTER NO-UNDO.
    
    RUN util/dev/ImpAP.p.
    
    RUN get-link-handle IN adm-broker-hdl (
        THIS-PROCEDURE, 
        'import-target':U, 
        OUTPUT cCharHdl
        ).

    IF VALID-HANDLE(WIDGET-HANDLE(cCharHdl)) THEN
        RUN local-open-query IN h_b-apinq2.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-change-page W-Win 
PROCEDURE local-change-page :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lAvail      AS LOGICAL NO-UNDO.
  DEFINE VARIABLE lUpdateDate AS LOGICAL NO-UNDO.
  DEFINE VARIABLE li-prev-page AS INT NO-UNDO.
  DEFINE VARIABLE li-cur-page  AS INT NO-UNDO.
    
  ASSIGN 
      li-prev-page = li-cur-page.    
  RUN GET-ATTRIBUTE ("current-page").
  ASSIGN 
      li-cur-page = INT(RETURN-VALUE).       
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'change-page':U ) .
  
  IF li-cur-page EQ 3 THEN
    RUN pGetPoRecs.

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/winReSizePgChg.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable W-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUN pInit.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit2 W-Win 
PROCEDURE local-exit2 :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER op-delete-value AS LOG NO-UNDO.

  RUN exit-window(OUTPUT op-delete-value).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetPoRecs W-Win 
PROCEDURE pGetPoRecs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE iPONum AS INTEGER NO-UNDO.
   DEFINE VARIABLE iPOLine AS INTEGER NO-UNDO.
   
   iPONum = DYNAMIC-FUNCTION ('GetCurrentPO' IN h_b-apinvl).
   iPOLine = DYNAMIC-FUNCTION ('display-line' IN h_b-apinvl).

   IF VALID-HANDLE(h_f-porec) THEN
      RUN populate-tt IN h_f-porec(
          INPUT iPONum, 
          INPUT iPOLine
          ).
   
   IF VALID-HANDLE(h_vi-poordl) THEN
      RUN pGetPONumber IN h_vi-poordl(
          INPUT iPONum          
          ).       
          
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit W-Win 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lActive  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCharHdl AS CHARACTER NO-UNDO.   
    
    RUN Inbound_GetAPIRouteStatus IN hdInboundProcs (
        INPUT  g_company,
        INPUT  cImportAPIRoute,
        OUTPUT lActive,
        OUTPUT cMessage
        ).

    IF NOT lActive THEN DO:    
        RUN get-link-handle IN adm-broker-hdl (
            THIS-PROCEDURE, 
            'import-api-target':U, 
            OUTPUT cCharHdl
            ).
    
        IF VALID-HANDLE(WIDGET-HANDLE(cCharHdl)) THEN  
            RUN DisableImport IN WIDGET-HANDLE(cCharHdl).        
    END.
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
 run get-link-handle in adm-broker-hdl(this-procedure,"add-inv-target", output char-hdl).
 run add-inv in widget-handle(char-hdl).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select_attvinv W-Win 
PROCEDURE select_attvinv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF BUFFER b-ap-inv FOR ap-inv.
   DEF BUFFER b-ap-invl FOR ap-invl.
   DEF BUFFER b-vend FOR vend.

   FIND FIRST b-ap-inv WHERE
        b-ap-inv.rec_key EQ rec_key_value
        NO-LOCK NO-ERROR.

   IF AVAIL b-ap-inv THEN
   DO:
      FIND FIRST b-vend WHERE
           b-vend.company EQ b-ap-inv.company AND
           b-vend.vend-no EQ b-ap-inv.vend-no
           NO-LOCK NO-ERROR.

      IF AVAIL b-vend THEN
         RUN windows/vinattch.w(b-vend.rec_key,'Vendor: ' + b-vend.vend-no,b-ap-inv.inv-no). 
   END.
   ELSE
   DO:
      FIND FIRST b-ap-invl WHERE
           b-ap-invl.rec_key EQ rec_key_value
           NO-LOCK NO-ERROR.

      IF AVAIL b-ap-invl THEN
      DO:
          FIND FIRST b-ap-inv WHERE
               b-ap-inv.i-no EQ b-ap-invl.i-no
               NO-LOCK NO-ERROR.

          IF AVAIL b-ap-inv THEN
          DO:
             FIND FIRST b-vend WHERE
                  b-vend.company EQ b-ap-inv.company AND
                  b-vend.vend-no EQ b-ap-inv.vend-no
                  NO-LOCK NO-ERROR.
  
             IF AVAIL b-vend THEN
                RUN windows/vinattch.w(b-vend.rec_key,'Vendor: ' + b-vend.vend-no,b-ap-inv.inv-no). 
          END.
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
  {src/adm/template/snd-list.i "ap-inv"}

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

