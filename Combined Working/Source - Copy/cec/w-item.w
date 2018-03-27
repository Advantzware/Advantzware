&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: windows/item.w

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
&SCOPED-DEFINE h_Browse01 h_b-item
&SCOPED-DEFINE h_Object01 h_p-rmcost
&SCOPED-DEFINE h_Object02 h_itemspec-2
&SCOPED-DEFINE h_Object03 h_p-updncp

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* to have item spec note window instead regular specnote window */
&scoped-define item_spec RMItem  

def var li-current-page as int no-undo.

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
&Scoped-define EXTERNAL-TABLES item
&Scoped-define FIRST-EXTERNAL-TABLE item


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR item.
/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-item AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-rmainq AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-rmiinq AS HANDLE NO-UNDO.
DEFINE VARIABLE h_e-item AS HANDLE NO-UNDO.
DEFINE VARIABLE h_e-item-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_export AS HANDLE NO-UNDO.
DEFINE VARIABLE h_f-add AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_item AS HANDLE NO-UNDO.
DEFINE VARIABLE h_item-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_item-bom AS HANDLE NO-UNDO.
DEFINE VARIABLE h_item-pos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemspec AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemspec-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_movecol2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_options AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-item AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-rmbom AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-rmcost AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-rmvend AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-rmview AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updncp AS HANDLE NO-UNDO.
DEFINE VARIABLE h_rm-ibin AS HANDLE NO-UNDO.
DEFINE VARIABLE h_smartmsg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-item AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-item2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-navest AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-navest-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vp-price AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vp-rmov AS HANDLE NO-UNDO.
DEFINE VARIABLE h_movecolH AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 150 BY 24
         BGCOLOR 15 .

DEFINE FRAME FRAME-C
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         THREE-D 
         AT COL 145 ROW 3.14
         SIZE 6 BY 1.19
         BGCOLOR 15 .

DEFINE FRAME OPTIONS-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 61 ROW 1
         SIZE 89 BY 1.91
         BGCOLOR 15 .

DEFINE FRAME message-frame
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 70 BY 1.91
         BGCOLOR 15 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: ASI.item
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
         TITLE              = "Raw Materials (Corrugated)"
         HEIGHT             = 23.95
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
ASSIGN FRAME FRAME-C:FRAME = FRAME F-Main:HANDLE
       FRAME message-frame:FRAME = FRAME F-Main:HANDLE
       FRAME OPTIONS-FRAME:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME message-frame:MOVE-BEFORE-TAB-ITEM (FRAME OPTIONS-FRAME:HANDLE)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FRAME FRAME-C
   UNDERLINE                                                            */
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

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-C
/* Query rebuild information for FRAME FRAME-C
     _Query            is NOT OPENED
*/  /* FRAME FRAME-C */
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
ON END-ERROR OF W-Win /* Raw Materials (Corrugated) */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Raw Materials (Corrugated) */
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
       RUN set-position IN h_smartmsg ( 1.71 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.14 , 32.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Browse Items|View Item|Bom|Inventory|Vend Cost|POs|Jobs|Bin|Spec|History' + ',
                     FOLDER-TAB-TYPE = 2':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 3.14 , 1.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 21.67 , 150.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/f-add.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_f-add ).
       RUN set-position IN h_f-add ( 1.00 , 18.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/options.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_options ).
       RUN set-position IN h_options ( 1.00 , 25.60 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 55.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/exit.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.00 , 81.20 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_folder ,
             FRAME OPTIONS-FRAME:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_options ,
             h_f-add , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_exit ,
             h_options , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/export.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_export ).
       RUN set-position IN h_export ( 1.00 , 10.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/movecol.w':U ,         /*Task# 01071407*/
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_movecol2 ).
       RUN set-position IN h_movecol2 ( 1.00 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'cec/b-item.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-item ).
       RUN set-position IN h_b-item ( 4.81 , 5.00 ) NO-ERROR.
       RUN set-size IN h_b-item ( 19.52 , 118.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartViewer h_export. */
       RUN add-link IN adm-broker-hdl ( h_b-item , 'export-xl':U , h_export ).

       /* Links to SmartViewer h_movecol2. */
       RUN add-link IN adm-broker-hdl ( h_b-item , 'move-columns':U , h_movecol2 ).

       /* Links to SmartNavBrowser h_b-item. */
       RUN add-link IN adm-broker-hdl ( h_p-navico , 'Navigation':U , h_b-item ).
       RUN add-link IN adm-broker-hdl ( h_b-item , 'Record':U , THIS-PROCEDURE ).

       /* Links to SmartObject h_options. */
       RUN add-link IN adm-broker-hdl ( h_b-item , 'spechk':U , h_options ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_movecol2 ,
             h_export , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-item ,
             FRAME FRAME-C:HANDLE , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'cec/v-item.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_v-item ).
       RUN set-position IN h_v-item ( 4.81 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 17.14 , 142.00 ) */

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
             INPUT  'rm/p-rmview.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-rmview ).
       RUN set-position IN h_p-rmview ( 22.19 , 88.00 ) NO-ERROR.
       RUN set-size IN h_p-rmview ( 2.14 , 58.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_v-item. */
       RUN add-link IN adm-broker-hdl ( h_b-item , 'Record':U , h_v-item ).
       RUN add-link IN adm-broker-hdl ( h_p-rmview , 'TableIO':U , h_v-item ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'add-item':U , h_v-item ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-item ,
             FRAME FRAME-C:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navico ,
             h_v-item , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-rmview ,
             h_p-navico , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/item-bom.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_item-bom ).
       RUN set-position IN h_item-bom ( 7.19 , 27.20 ) NO-ERROR.
       /* Size in UIB:  ( 12.52 , 88.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewerid/item.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_item-2 ).
       RUN set-position IN h_item-2 ( 5.29 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'rm/p-rmbom.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-rmbom ).
       RUN set-position IN h_p-rmbom ( 19.81 , 50.00 ) NO-ERROR.
       RUN set-size IN h_p-rmbom ( 2.24 , 31.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('4,2':U) NO-ERROR.

       /* Links to SmartViewer h_item-bom. */
       RUN add-link IN adm-broker-hdl ( h_p-rmbom , 'TableIO':U , h_item-bom ).
       RUN add-link IN adm-broker-hdl ( h_v-item2 , 'Record':U , h_item-bom ).

       /* Links to SmartViewer h_item-2. */
       RUN add-link IN adm-broker-hdl ( h_v-item , 'Record':U , h_item-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_item-2 ,
             FRAME FRAME-C:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-rmbom ,
             h_item-2 , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'cec/v-item2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-item2 ).
       RUN set-position IN h_v-item2 ( 4.81 , 10.00 ) NO-ERROR.
       /* Size in UIB:  ( 17.43 , 130.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-item.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-item ).
       RUN set-position IN h_p-item ( 22.33 , 55.00 ) NO-ERROR.
       RUN set-size IN h_p-item ( 2.14 , 62.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/vp-rmov.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vp-rmov ).
       RUN set-position IN h_vp-rmov ( 22.33 , 117.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.14 , 17.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-navest.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-navest ).
       RUN set-position IN h_v-navest ( 22.43 , 16.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 34.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2,1':U) NO-ERROR.

       /* Links to SmartViewer h_v-item2. */
       RUN add-link IN adm-broker-hdl ( h_p-item , 'TableIO':U , h_v-item2 ).
       RUN add-link IN adm-broker-hdl ( h_v-item , 'Record':U , h_v-item2 ).
       RUN add-link IN adm-broker-hdl ( h_vp-rmov , 'override':U , h_v-item2 ).

       /* Links to SmartViewer h_v-navest. */
       RUN add-link IN adm-broker-hdl ( h_b-item , 'nav-itm':U , h_v-navest ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-item2 ,
             FRAME FRAME-C:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-item ,
             h_v-item2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_vp-rmov ,
             h_p-item , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-navest ,
             h_vp-rmov , 'AFTER':U ).
    END. /* Page 4 */
    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/e-item.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_e-item-2 ).
       RUN set-position IN h_e-item-2 ( 4.81 , 2.00 ) NO-ERROR.
       RUN set-size IN h_e-item-2 ( 19.52 , 55.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/e-item.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_e-item ).
       RUN set-position IN h_e-item ( 4.81 , 58.00 ) NO-ERROR.
       /* Size in UIB:  ( 17.62 , 92.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'rm/p-rmvend.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-rmvend ).
       RUN set-position IN h_p-rmvend ( 22.43 , 92.00 ) NO-ERROR.
       RUN set-size IN h_p-rmvend ( 2.14 , 41.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'rm/vp-price.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vp-price ).
       RUN set-position IN h_vp-price ( 22.43 , 133.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.05 , 17.20 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-navest.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-navest-2 ).
       RUN set-position IN h_v-navest-2 ( 22.91 , 58.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 34.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartNavBrowser h_e-item-2. */
       RUN add-link IN adm-broker-hdl ( h_b-item , 'Record':U , h_e-item-2 ).

       /* Links to SmartViewer h_e-item. */
       RUN add-link IN adm-broker-hdl ( h_e-item-2 , 'Record':U , h_e-item ).
       RUN add-link IN adm-broker-hdl ( h_p-rmvend , 'TableIO':U , h_e-item ).

       /* Links to SmartViewer h_vp-price. */
       RUN add-link IN adm-broker-hdl ( h_e-item , 'price-change':U , h_vp-price ).

       /* Links to SmartViewer h_v-navest-2. */
       RUN add-link IN adm-broker-hdl ( h_b-item , 'nav-itm':U , h_v-navest-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_e-item-2 ,
             FRAME FRAME-C:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_e-item ,
             h_e-item-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-rmvend ,
             h_e-item , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_vp-price ,
             h_p-rmvend , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-navest-2 ,
             h_vp-price , 'AFTER':U ).
    END. /* Page 5 */
    WHEN 6 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/item-pos.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_item-pos ).
       RUN set-position IN h_item-pos ( 4.81 , 3.00 ) NO-ERROR.
       RUN set-size IN h_item-pos ( 19.52 , 145.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartNavBrowser h_item-pos. */
       RUN add-link IN adm-broker-hdl ( h_v-item , 'Record':U , h_item-pos ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_item-pos ,
             FRAME FRAME-C:HANDLE , 'AFTER':U ).
    END. /* Page 6 */
    WHEN 7 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'rminq/b-rmainq.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-rmainq ).
       RUN set-position IN h_b-rmainq ( 4.57 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 19.76 , 148.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartNavBrowser h_b-rmainq. */
       RUN add-link IN adm-broker-hdl ( h_b-item , 'Record':U , h_b-rmainq ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-rmainq ,
             FRAME FRAME-C:HANDLE , 'AFTER':U ).
    END. /* Page 7 */
    WHEN 8 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/rm-ibin.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_rm-ibin ).
       RUN set-position IN h_rm-ibin ( 4.81 , 4.00 ) NO-ERROR.
       RUN set-size IN h_rm-ibin ( 16.67 , 145.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/p-rmcost.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_p-rmcost ).
       RUN set-position IN h_p-rmcost ( 21.95 , 60.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.05 , 36.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1,3':U) NO-ERROR.

       /* Links to SmartNavBrowser h_rm-ibin. */
       RUN add-link IN adm-broker-hdl ( h_b-item , 'Record':U , h_rm-ibin ).
       RUN add-link IN adm-broker-hdl ( h_item-2 , 'bin':U , h_rm-ibin ).

       /* Links to SmartViewer h_p-rmcost. */
       RUN add-link IN adm-broker-hdl ( h_rm-ibin , 'cost':U , h_p-rmcost ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_rm-ibin ,
             FRAME FRAME-C:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-rmcost ,
             h_rm-ibin , 'AFTER':U ).
    END. /* Page 8 */
    WHEN 9 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewerid/item.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_item ).
       RUN set-position IN h_item ( 4.81 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/itemspec.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itemspec ).
       RUN set-position IN h_itemspec ( 6.95 , 3.00 ) NO-ERROR.
       RUN set-size IN h_itemspec ( 5.95 , 145.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/itemspec.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_itemspec-2 ).
       RUN set-position IN h_itemspec-2 ( 13.86 , 19.00 ) NO-ERROR.
       /* Size in UIB:  ( 6.67 , 107.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updncp.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updncp ).
       RUN set-position IN h_p-updncp ( 21.00 , 45.00 ) NO-ERROR.
       RUN set-size IN h_p-updncp ( 1.76 , 46.80 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartViewer h_item. */
       RUN add-link IN adm-broker-hdl ( h_v-item , 'Record':U , h_item ).

       /* Links to SmartNavBrowser h_itemspec. */
       RUN add-link IN adm-broker-hdl ( h_v-item , 'Record':U , h_itemspec ).

       /* Links to SmartViewer h_itemspec-2. */
       RUN add-link IN adm-broker-hdl ( h_itemspec , 'Record':U , h_itemspec-2 ).
       RUN add-link IN adm-broker-hdl ( h_p-updncp , 'TableIO':U , h_itemspec-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_item ,
             FRAME FRAME-C:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_itemspec ,
             h_item , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_itemspec-2 ,
             h_itemspec , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updncp ,
             h_itemspec-2 , 'AFTER':U ).
    END. /* Page 9 */
    WHEN 10 THEN DO:
       RUN init-object IN THIS-PROCEDURE (                  
             INPUT  'viewers/movecol.w':U ,                     /*Task# 07291402*/
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_movecolH ).
       RUN set-position IN h_movecolH ( 1.00 , 10.10 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'rminq/b-rmiinq.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-rmiinq ).
       RUN set-position IN h_b-rmiinq ( 4.57 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 19.76 , 148.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_movecolH. */
       RUN add-link IN adm-broker-hdl ( h_b-rmiinq , 'move-columns':U , h_movecolH ).

       /* Links to SmartNavBrowser h_b-rmiinq. */
       RUN add-link IN adm-broker-hdl ( h_b-item , 'history':U , h_b-rmiinq ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-rmiinq ,
             h_movecolH , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-rmiinq ,
             FRAME FRAME-C:HANDLE , 'AFTER':U ).
    END. /* Page 10 */

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
  {src/adm/template/row-list.i "item"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "item"}

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
  VIEW FRAME message-frame IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-message-frame}
  VIEW FRAME OPTIONS-FRAME IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-OPTIONS-FRAME}
  VIEW FRAME FRAME-C IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-C}
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
    DEF VAR lv-current-page AS INT NO-UNDO.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'change-page':U ) .

    RUN get-attribute ('Current-Page':U).
    ASSIGN 
        lv-current-page = int(return-value).
    IF VALID-HANDLE(h_vp-rmov) THEN DO:
        IF lv-current-page EQ 4 THEN 
            RUN ipShowBtn IN h_vp-rmov (TRUE).
        ELSE
            RUN ipShowBtn IN h_vp-rmov (FALSE).
    END.
    
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
  def var char-hdl as cha no-undo.
  
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-objects':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
/* not for corrugated item
  if avail item and item.i-code = "E" then do:
     run get-link-handle in adm-broker-hdl(this-procedure, "page-source", output char-hdl).
     RUN disable-folder-page IN widget-handle(char-hdl) (INPUT 5).
     RUN disable-folder-page IN widget-handle(char-hdl) (INPUT 7).
     RUN disable-folder-page IN widget-handle(char-hdl) (INPUT 8).
     RUN disable-folder-page IN widget-handle(char-hdl) (INPUT 9).
  end.  
*/  
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
  def var char-hdl as cha no-undo.
  
  run select-page(2).
  run get-link-handle in adm-broker-hdl(this-procedure,"add-item-target", output char-hdl).
  run add-item in widget-handle(char-hdl).

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
  {src/adm/template/snd-list.i "item"}

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

