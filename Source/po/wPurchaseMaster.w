&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: windows/<table>.w

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
/*  Mod: Ticket - 103137 Format Change for Order No. and Job No.       */     

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE winReSize
&SCOPED-DEFINE h_Browse01 h_b-po-inq
&SCOPED-DEFINE h_Object01 h_vp-poord
&SCOPED-DEFINE h_Object02 h_vp-viewp
&SCOPED-DEFINE h_Object03 h_vp-clsp2


/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER ipcScreen AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
    def var li-prev-page as int no-undo.
    def var li-cur-page as int no-undo.
    DEF NEW SHARED VAR lNewOrd AS LOG NO-UNDO.

&scoped-define item_spec FGITEM
&SCOPED-DEFINE proc-init proc-init

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
&Scoped-define EXTERNAL-TABLES po-ordl po-ord
&Scoped-define FIRST-EXTERNAL-TABLE po-ordl


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR po-ordl, po-ord.
/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_attach        AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-po-inq      AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bi-poord      AS HANDLE NO-UNDO.
DEFINE VARIABLE h_exit          AS HANDLE NO-UNDO.
DEFINE VARIABLE h_export        AS HANDLE NO-UNDO.
DEFINE VARIABLE h_f-add         AS HANDLE NO-UNDO.
DEFINE VARIABLE h_f-addbes      AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder        AS HANDLE NO-UNDO.
DEFINE VARIABLE h_movecol       AS HANDLE NO-UNDO.
DEFINE VARIABLE h_options       AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-poh         AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-pobox       AS HANDLE NO-UNDO.
DEFINE VARIABLE h_smartmsg      AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-boxdee      AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-navest      AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-pohold      AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-purord      AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-poord      AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-poord-2    AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-poord-3    AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vp-poord      AS HANDLE NO-UNDO.
DEFINE VARIABLE h_import        AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-polinq      AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-poliin      AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-polinq-2    AS HANDLE NO-UNDO.
DEFINE VARIABLE h_f-porec       AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-posum       AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vp-clspo      AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vp-clsp2      AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vp-viewp      AS HANDLE NO-UNDO.
DEFINE VARIABLE h_movecol-2     AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-poord-4    AS HANDLE NO-UNDO.
/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 156.6 BY 24
         BGCOLOR 15 .

DEFINE FRAME OPTIONS-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 34 ROW 1
         SIZE 119 BY 1.91
         BGCOLOR 15 .

DEFINE FRAME message-frame
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 105 ROW 2.91
         SIZE 63 BY 1.43
         BGCOLOR 15 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: asi.po-ordl,asi.po-ord
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 3
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Purchase Order"
         HEIGHT             = 24
         WIDTH              = 156.6
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
ON END-ERROR OF W-Win /* Purchase Order */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Purchase Order */
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
SESSION:SET-WAIT-STATE('').
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
       RUN set-position IN h_smartmsg ( 1.00 , 32.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.14 , 32.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/f-add.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_f-add ).
       RUN set-position IN h_f-add ( 1.00 , 32.60 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Browse|Detail|Items|Design|Invoices|Receipts|Summary' + ',
                     FOLDER-TAB-TYPE = 2':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 3.14 , 2.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 21.67 , 155.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/f-addbes.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_f-addbes ).
       RUN set-position IN h_f-addbes ( 1.00 , 40.40 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */                  

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/attach.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_attach ).
       RUN set-position IN h_attach ( 1.00 , 48.20 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/options.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_options ).
       RUN set-position IN h_options ( 1.00 , 56.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 55.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/exit.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.00 , 112.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Links to SmartObject h_attach. */
       RUN add-link IN adm-broker-hdl ( h_b-po-inq , 'attach':U , h_attach ).
       
        /* Links to SmartObject h_options. */
       RUN add-link IN adm-broker-hdl ( h_b-po-inq , 'spec':U , h_options ).
	   RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'udficon':U , h_options ).
       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_folder ,
             FRAME message-frame:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_f-addbes ,
             h_f-add , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_attach ,
             h_f-addbes , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_options ,
             h_attach , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_exit ,
             h_options , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/import.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_import ).
       RUN set-position IN h_import ( 1.00 , 9.50 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */
       
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/export.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_export ).
       RUN set-position IN h_export ( 1.00 , 17.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/movecol.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_movecol ).
       RUN set-position IN h_movecol ( 1.00 , 24.80 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'poinq/b-po-inq.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = ?':U ,
             OUTPUT h_b-po-inq ).
       RUN set-position IN h_b-po-inq ( 4.57 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 20.00 , 152.00 ) */
       
       /* Links to SmartViewer h_import. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'import':U , h_import ).

       /* Links to SmartViewer h_export. */
       RUN add-link IN adm-broker-hdl ( h_b-po-inq , 'export-xl':U , h_export ).

       /* Links to SmartViewer h_movecol. */
       RUN add-link IN adm-broker-hdl ( h_b-po-inq , 'move-columns':U , h_movecol ).

       /* Links to SmartNavBrowser h_b-po-inq. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'add-po-best':U , h_b-po-inq ).
       RUN add-link IN adm-broker-hdl ( h_b-po-inq , 'Record':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_movecol ,
             h_export , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-po-inq ,
             h_folder , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'po/v-purord.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_v-purord ).
       RUN set-position IN h_v-purord ( 4.81 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 15.48 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-poh.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-poh ).
       RUN set-position IN h_p-poh ( 21.24 , 45.00 ) NO-ERROR.
       RUN set-size IN h_p-poh ( 2.14 , 71.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'po/v-pohold.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-pohold ).
       RUN set-position IN h_v-pohold ( 21.24 , 116.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.14 , 18.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-navest.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-navest ).
       RUN set-position IN h_v-navest ( 21.24 , 8.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 34.00 ) */
       
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'po/vp-clspo.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vp-clspo ).
       RUN set-position IN h_vp-clspo ( 21.24 , 137.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.14 , 17.00 ) */
       
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'poinq/v-polinq.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-polinq-2 ).
       RUN set-position IN h_v-polinq-2 ( 21.20 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.62 , 146.00 ) */                             

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_v-purord. */
       RUN add-link IN adm-broker-hdl ( h_b-po-inq , 'Record':U , h_v-purord ).
       RUN add-link IN adm-broker-hdl ( h_p-poh , 'TableIO':U , h_v-purord ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'add-po':U , h_v-purord ).

       /* Links to SmartViewer h_v-pohold. */
       RUN add-link IN adm-broker-hdl ( h_v-purord , 'hold':U , h_v-pohold ).

       /* Links to SmartViewer h_v-navest. */
       RUN add-link IN adm-broker-hdl ( h_b-po-inq , 'nav-itm':U , h_v-navest ).
       
       /* Links to SmartViewer h_vp-clspo. */
       RUN add-link IN adm-broker-hdl ( h_v-purord , 'Record':U , h_vp-clspo ).
       
       /* Links to SmartViewer h_v-polinq-2. */
       RUN add-link IN adm-broker-hdl ( h_b-po-inq , 'Record':U , h_v-polinq-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-purord ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-poh ,
             h_v-purord , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-pohold ,
             h_p-poh , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-navest ,
             h_v-pohold , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'po/vi-poord.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-poord ).
       RUN set-position IN h_vi-poord ( 5.05 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.38 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'po/bi-poord.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_bi-poord ).
       RUN set-position IN h_bi-poord ( 7.67 , 3.00 ) NO-ERROR.
       RUN set-size IN h_bi-poord ( 14.76 , 145.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'po/vp-poord.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vp-poord ).
       RUN set-position IN h_vp-poord ( 22.91 , 12.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.29 , 136.00 ) */
       
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'po/vp-viewp.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vp-viewp ).
       RUN set-position IN h_vp-viewp ( 22.91 , 61.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.29 , 15.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'po/vp-clsp2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vp-clsp2 ).
       RUN set-position IN h_vp-clsp2 ( 22.91 , 133.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.29 , 15.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartViewer h_vi-poord. */
       RUN add-link IN adm-broker-hdl ( h_v-purord , 'Record':U , h_vi-poord ).

       /* Links to SmartNavBrowser h_bi-poord. */
       RUN add-link IN adm-broker-hdl ( h_v-purord , 'Record':U , h_bi-poord ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'show-notes':U , h_bi-poord ).

       /* Links to SmartViewer h_vp-poord. */
       RUN add-link IN adm-broker-hdl ( h_bi-poord , 'buttons':U , h_vp-poord ).
       RUN add-link IN adm-broker-hdl ( h_bi-poord , 'Record':U , h_vp-poord ).

       /* Links to SmartObject h_options. */
       RUN add-link IN adm-broker-hdl (  h_bi-poord  , 'specpo':U , h_options ).
       
       /* Links to SmartViewer h_vp-viewp. */
       RUN add-link IN adm-broker-hdl ( h_bi-poord , 'Record':U , h_vp-viewp ).

       /* Links to SmartViewer h_vp-clsp2. */
       RUN add-link IN adm-broker-hdl ( h_bi-poord , 'Record':U , h_vp-clsp2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_vi-poord ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_bi-poord ,
             h_vi-poord , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_vp-poord ,
             h_bi-poord , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'cec/v-boxdee.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-boxdee ).
       RUN set-position IN h_v-boxdee ( 5.05 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 16.67 , 149.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'po/q-pobox.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-pobox ).
       RUN set-position IN h_q-pobox ( 4.81 , 10.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.86 , 10.80 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('3':U) NO-ERROR.

       /* Links to SmartViewer h_v-boxdee. */
       RUN add-link IN adm-broker-hdl ( h_q-pobox , 'Record':U , h_v-boxdee ).

       /* Links to SmartQuery h_q-pobox. */
       RUN add-link IN adm-broker-hdl ( h_bi-poord , 'Record':U , h_q-pobox ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-boxdee ,
             h_folder , 'AFTER':U ).
    END. /* Page 4 */
    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'po/vi-poord.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-poord-2 ).
       RUN set-position IN h_vi-poord-2 ( 4.81 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.38 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'poinq/v-polinq.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-polinq ).
       RUN set-position IN h_v-polinq ( 7.43 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.62 , 146.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'poinq/b-poliin.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-poliin ).
       RUN set-position IN h_b-poliin ( 10.29 , 3.00 ) NO-ERROR.
       RUN set-size IN h_b-poliin ( 12.38 , 146.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_vi-poord-2. */
       RUN add-link IN adm-broker-hdl ( h_b-po-inq , 'Record':U , h_vi-poord-2 ).

       /* Links to SmartViewer h_v-polinq. */
       RUN add-link IN adm-broker-hdl ( h_b-po-inq , 'Record':U , h_v-polinq ).

       /* Links to SmartBrowser h_b-poliin. */
       RUN add-link IN adm-broker-hdl ( h_b-po-inq , 'Record':U , h_b-poliin ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'winSize':U , h_b-poliin ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_vi-poord-2 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-polinq ,
             h_vi-poord-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-poliin ,
             h_v-polinq , 'AFTER':U ).       
    END. /* Page 5 */
    WHEN 6 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'po/vi-poord1.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-poord-4 ).
       RUN set-position IN h_vi-poord-4 ( 5.05 , 4.00 ) NO-ERROR.
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
       
       /* Links to SmartViewer h_movecol. */
       RUN add-link IN adm-broker-hdl ( h_f-porec , 'move-columns':U , h_movecol-2 ). 
       
       /* Links to SmartViewer h_vi-poord-3. */
       RUN add-link IN adm-broker-hdl ( h_b-po-inq , 'Record':U , h_vi-poord-4 ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 6 */
    WHEN 7 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'po/vi-poord1.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-poord-3 ).
       RUN set-position IN h_vi-poord-3 ( 5.05 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.38 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'po/b-posum.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_b-posum ).
       RUN set-position IN h_b-posum ( 8.55 , 4.00 ) NO-ERROR.
       RUN set-size IN h_b-poliin ( 12.38 , 146.00 ) NO-ERROR.  

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_vi-poord-3. */
       RUN add-link IN adm-broker-hdl ( h_b-posum , 'Record':U , h_vi-poord-3 ).

       /* Links to SmartQuery h_b-posum. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'Record':U , h_b-posum ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_vi-poord-3 ,
             h_folder , 'AFTER':U ).
    END. /* Page 7 */

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
  {src/adm/template/row-list.i "po-ordl"}
  {src/adm/template/row-list.i "po-ord"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "po-ordl"}
  {src/adm/template/row-find.i "po-ord"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChangePanelState W-Win 
PROCEDURE ChangePanelState :
/*------------------------------------------------------------------------------
 Purpose: Procedure to enable/disbale panels based on order statuss
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiCurrentPage AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcScreen      AS CHARACTER NO-UNDO.

        RUN pDisablePanels(
            INPUT ipiCurrentPage,
            INPUT ipcScreen
            ).     
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-po-recs W-Win 
PROCEDURE get-po-recs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE vcPONum AS INTEGER NO-UNDO.
   DEFINE VARIABLE iPOLine AS INTEGER NO-UNDO.
   
   vcPONum = DYNAMIC-FUNCTION ('GetCurrentPO' IN h_b-po-inq).
   iPOLine = DYNAMIC-FUNCTION ('GetPOLine'    IN h_b-po-inq).

   IF VALID-HANDLE(h_f-porec) THEN
      RUN populate-tt IN h_f-porec(
          INPUT vcPONum, 
          INPUT iPOLine
          ).
   
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetScreenType W-Win
PROCEDURE GetScreenType:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcScreenType AS CHARACTER NO-UNDO.
    
    opcScreenType = ipcScreen.
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

 RUN util/dev/impPo.p .
 IF VALID-HANDLE(h_b-po-inq) THEN
 RUN local-open-query IN h_b-po-inq .

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
    
    ASSIGN 
        li-prev-page = li-cur-page.    
    RUN GET-ATTRIBUTE ("current-page").
    ASSIGN 
        li-cur-page = INT(RETURN-VALUE).       
    IF li-prev-page EQ 2 THEN DO:
        IF lNewOrd THEN DO:
            MESSAGE "You must save your record with a valid vendor before entering lines."
            VIEW-AS ALERT-BOX.
            RUN select-page (2).
            RETURN.
        END.
        RUN pCheckUpdateMode IN h_v-purord (OUTPUT lUpdateDate).
        IF lUpdateDate THEN
        DO:
            MESSAGE "You must save your record or cancel."
            VIEW-AS ALERT-BOX INFO.
            RUN select-page (2).
            RETURN.  
        END.
    END.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'change-page':U ) .
  
    IF li-cur-page = 3 THEN
        RUN dept-pan-image-proc IN h_bi-poord .
    ELSE
        RUN dept-pan-image-proc IN h_b-po-inq .
       
    IF li-prev-page EQ 2 
    AND li-cur-page EQ 3 
    AND lNewOrd THEN DO:
        ASSIGN 
            lNewOrd = FALSE.
        RUN addItem IN h_vp-poord.
    END.

    IF li-cur-page EQ 6 THEN
    RUN get-po-recs.
      
    RUN ChangePanelState(
      INPUT li-cur-page,
      INPUT ipcScreen
      ).         
    
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
  RUN setUserPrint.
  
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
  DEF VAR char-hdl AS cha NO-UNDO.
  DEFINE VARIABLE lAllowCreate AS LOGICAL NO-UNDO.
  
  RUN select-page(2).
  
  IF VALID-HANDLE(h_p-poh) THEN 
  DO:
      RUN pAllowCreate IN h_p-poh (OUTPUT lAllowCreate).
      IF NOT lAllowCreate THEN RETURN.
  END.  
  
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"add-po-target", OUTPUT char-hdl).
  RUN add-po IN WIDGET-HANDLE(char-hdl).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select_addbest W-Win 
PROCEDURE select_addbest :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR char-hdl AS cha NO-UNDO.
  
   RUN select-page(1).
   RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"add-po-best-target", OUTPUT char-hdl).
   RUN add-po-best IN WIDGET-HANDLE(char-hdl).

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
    /* Tkt 22432 - Revert from 20414 - PO has special attachment program */
    RUN windows/attachpo.w(rec_key_value,HEADER_value).
    /* {methods/select_att.i} */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Select_Note W-Win 
PROCEDURE Select_Note :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    RUN windows/notes.w(rec_key_value,HEADER_value).
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pChangeWindowTitle W-Win
PROCEDURE pChangeWindowTitle PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Change the windows's title based o the input screen type
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcScreen AS CHARACTER NO-UNDO.
    
    CASE ipcScreen:
        WHEN "PU1" THEN 
            {&WINDOW-NAME}:TITLE = REPLACE({&WINDOW-NAME}:TITLE,"Purchase Order","Order Inquiry").
         WHEN "PQ1" THEN 
            {&WINDOW-NAME}:TITLE = REPLACE({&WINDOW-NAME}:TITLE,"Purchase Order","PO Inquiry").
         WHEN "PU4" THEN 
            {&WINDOW-NAME}:TITLE = REPLACE({&WINDOW-NAME}:TITLE,"Purchase Order","Purchase Orders Open/Close").                                          
    END.    
END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisablePanels W-Win
PROCEDURE pDisablePanels:
/*------------------------------------------------------------------------------
 Purpose: Disable panels based on page number
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiCurrentPage AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcScreen      AS CHARACTER NO-UNDO.
    
    CASE ipiCurrentPage: 
        WHEN 1 THEN DO:                  
            
        END.
        WHEN 2 THEN DO:
        /* Detail Tab */
            IF ipcScreen NE "PU1" THEN DO:
                RUN disable-all IN h_v-navest.
                RUN pHideFrame IN h_p-poh.
                RUN pHideFrame IN h_v-pohold.                               
            END.
            
            IF ipcScreen NE "PU4" AND ipcScreen NE "PU1" THEN
            RUN pHideFrame IN h_vp-clspo.
            
            IF ipcScreen NE "PQ1" THEN
            RUN pHideFrame IN h_v-polinq-2.              
            
        END.         
        WHEN 3 THEN DO:
        /* Detail Tab */
            IF ipcScreen NE "PU1" THEN
            RUN pHideFrame IN h_vp-poord.
             
            IF ipcScreen NE "PU4" THEN DO:                 
                RUN pHideFrame IN h_vp-viewp.               
            END.
            IF ipcScreen NE "PU4" AND ipcScreen NE "PU1" THEN do:
                RUN pHideFrame IN h_vp-clspo.
                RUN pHideFrame IN h_vp-clsp2.
            END.    
        END.
    END CASE.
    
END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-init W-Win
PROCEDURE proc-init:
/*------------------------------------------------------------------------------
 Purpose: Disable panels based on page number
 Notes:
------------------------------------------------------------------------------*/
    IF ipcScreen NE "PU1" THEN DO: 
        RUN disable-add-button IN h_f-add.
        RUN disable-button IN h_import.
        RUN disable-button IN h_f-addbes.
        RUN disable-button IN h_attach.
        RUN pChangeWindowTitle(
            INPUT ipcScreen
            ). 
    END. 
    IF ipcScreen EQ "PU4" THEN
    DO:
       RUN disable-button IN  h_export.
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
  {src/adm/template/snd-list.i "po-ordl"}
  {src/adm/template/snd-list.i "po-ord"}

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
 IF AVAIL po-ord THEN
    RUN custom/setUserPrint.p (g_company,'po-ordl_.',
                               'begin_po-no,end_po-no,begin_vend-no,end_vend-no,tb_reprint,tb_reprint-closed',
                               STRING(po-ord.po-no) + ',' + STRING(po-ord.po-no) + ',' +
                               po-ord.vend-no + ',' + po-ord.vend-no + ',' + STRING(po-ord.printed)+ ',' +
                               (IF po-ord.stat EQ "C" AND po-ord.printed THEN "YES" ELSE "NO")).

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

