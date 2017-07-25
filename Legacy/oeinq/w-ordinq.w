&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: oeinq\w-ordinq.w
          
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
&SCOPED-DEFINE h_Browse01 h_b-ordinq
&SCOPED-DEFINE h_Object01 h_v-navest
&SCOPED-DEFINE h_Object02 h_p-disabl
&SCOPED-DEFINE h_Object03 h_v-ordt-2
&SCOPED-DEFINE h_Object04 h_p-ordrel
&SCOPED-DEFINE h_Object05 h_p-orel
&SCOPED-DEFINE h_Object06 h_p-obol

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
&scoped-define item_spec  FGITEM 
def var li-prev-page as int init 1 no-undo.
def var li-cur-page as int init 1 no-undo.
def var h-detail as handle no-undo.
DEF VAR li-last-page AS INT NO-UNDO.  /* for folding estimate page */

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
&Scoped-define EXTERNAL-TABLES oe-ord
&Scoped-define FIRST-EXTERNAL-TABLE oe-ord


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-ord.
/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_attach-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_attcust AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-ordinq AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-ordm AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-ordrel AS HANDLE NO-UNDO.
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_expxls AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_movecol AS HANDLE NO-UNDO.
DEFINE VARIABLE h_optionse AS HANDLE NO-UNDO.
DEFINE VARIABLE h_optonote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-disabl AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-obol AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-ordrel AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-orel AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updcan AS HANDLE NO-UNDO.
DEFINE VARIABLE h_relticket AS HANDLE NO-UNDO.
DEFINE VARIABLE h_smartmsg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-navest AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-oeqtys AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-ord AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-ordlin AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-ordt AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-ordt-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-shpnot AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-ord AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-ord-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-ord-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-ordl AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-ordlr AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vp-ordli AS HANDLE NO-UNDO.
DEFINE VARIABLE h_w-oeinvl AS HANDLE NO-UNDO.
DEFINE VARIABLE h_w-ordesf AS HANDLE NO-UNDO.
DEFINE VARIABLE h_w-ordest AS HANDLE NO-UNDO.
DEFINE VARIABLE h_w-ordfg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_w-ordjob AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 159 BY 24
         BGCOLOR 15 .

DEFINE FRAME OPTIONS-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 34 ROW 1
         SIZE 125 BY 1.91
         BGCOLOR 15 .

DEFINE FRAME message-frame
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 1.71
         SIZE 37 BY 1.43
         BGCOLOR 15 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: ASI.oe-ord
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
         TITLE              = "Order Inquiry"
         HEIGHT             = 24
         WIDTH              = 159
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
ON END-ERROR OF W-Win /* Order Inquiry */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Order Inquiry */
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
       RUN set-position IN h_smartmsg ( 1.29 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.14 , 32.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/attachcust.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_attcust ).
       RUN set-position IN h_attcust ( 1.10 , 30.60 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Brws Order|View Order|View Item|Misc Chgs|Job prod|Releases|Estimate|Order Total|FG Item|Invoices|Ship Notes' + ',
                     FOLDER-TAB-TYPE = 2':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 3.14 , 1.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 21.67 , 159.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/expxls.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_expxls ).
       RUN set-position IN h_expxls ( 1.10 , 46.40 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/attach.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_attach-2 ).
       RUN set-position IN h_attach-2 ( 1.10 , 54.40 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/optionse.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_optionse ).
       RUN set-position IN h_optionse ( 1.10 , 62.20 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 63.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/exit.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.10 , 118.20 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartObject h_attcust. */
       RUN add-link IN adm-broker-hdl ( h_b-ordinq , 'attachcust':U , h_attcust ).

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Links to SmartObject h_attach-2. */
       RUN add-link IN adm-broker-hdl ( h_b-ordinq , 'attach':U , h_attach-2 ).

       /* Links to SmartObject h_optionse. */
       RUN add-link IN adm-broker-hdl ( h_b-ordinq , 'spec':U , h_optionse ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_folder ,
             FRAME message-frame:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_expxls ,
             h_attcust , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_attach-2 ,
             h_expxls , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_optionse ,
             h_attach-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_exit ,
             h_optionse , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/optonote.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_optonote ).
       RUN set-position IN h_optonote ( 1.10 , 23.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/movecol.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_movecol ).
       RUN set-position IN h_movecol ( 1.10 , 38.60 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oeinq/b-ordinq.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-ordinq ).
       RUN set-position IN h_b-ordinq ( 4.81 , 6.00 ) NO-ERROR.
       /* Size in UIB:  ( 19.52 , 148.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartViewer h_movecol. */
       RUN add-link IN adm-broker-hdl ( h_b-ordinq , 'move-columns':U , h_movecol ).

       /* Links to SmartNavBrowser h_b-ordinq. */
       RUN add-link IN adm-broker-hdl ( h_expxls , 'sort-data':U , h_b-ordinq ).
       RUN add-link IN adm-broker-hdl ( h_p-navico , 'Navigation':U , h_b-ordinq ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'estimate':U , h_b-ordinq ).
       RUN add-link IN adm-broker-hdl ( h_b-ordinq , 'Record':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_movecol ,
             h_attcust , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-ordinq ,
             h_folder , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/v-ord.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_v-ord ).
       RUN set-position IN h_v-ord ( 4.81 , 3.60 ) NO-ERROR.
       /* Size in UIB:  ( 17.48 , 152.40 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-navico.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navico ).
       RUN set-position IN h_p-navico ( 22.48 , 61.60 ) NO-ERROR.
       RUN set-size IN h_p-navico ( 2.14 , 38.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_v-ord. */
       RUN add-link IN adm-broker-hdl ( h_b-ordinq , 'Record':U , h_v-ord ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-ord ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navico ,
             h_v-ord , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/vi-ord.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-ord ).
       RUN set-position IN h_vi-ord ( 4.57 , 8.60 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oeinq/v-ordlin.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = ?':U ,
             OUTPUT h_v-ordlin ).
       RUN set-position IN h_v-ordlin ( 6.24 , 7.60 ) NO-ERROR.
       /* Size in UIB:  ( 13.57 , 146.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/v-oeqtys.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-oeqtys ).
       RUN set-position IN h_v-oeqtys ( 19.95 , 4.60 ) NO-ERROR.
       /* Size in UIB:  ( 2.86 , 152.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oeinq/vp-ordli.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vp-ordli ).
       RUN set-position IN h_vp-ordli ( 22.86 , 72.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.67 , 17.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_vi-ord. */
       RUN add-link IN adm-broker-hdl ( h_b-ordinq , 'Record':U , h_vi-ord ).

       /* Links to SmartViewer h_v-ordlin. */
       RUN add-link IN adm-broker-hdl ( h_b-ordinq , 'Record':U , h_v-ordlin ).

       /* Links to SmartViewer h_v-oeqtys. */
       RUN add-link IN adm-broker-hdl ( h_b-ordinq , 'Record':U , h_v-oeqtys ).

       /* Links to SmartViewer h_vp-ordli. */
       RUN add-link IN adm-broker-hdl ( h_b-ordinq , 'Record':U , h_vp-ordli ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_vi-ord ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-ordlin ,
             h_vi-ord , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-oeqtys ,
             h_v-ordlin , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_vp-ordli ,
             h_v-oeqtys , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/vi-ord.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-ord-3 ).
       RUN set-position IN h_vi-ord-3 ( 4.81 , 8.60 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/b-ordm.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-ordm ).
       RUN set-position IN h_b-ordm ( 6.48 , 12.60 ) NO-ERROR.
       RUN set-size IN h_b-ordm ( 10.71 , 136.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-disabl.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-disabl ).
       RUN set-position IN h_p-disabl ( 17.43 , 118.00 ) NO-ERROR.
       RUN set-size IN h_p-disabl ( 1.76 , 31.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-navest.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-navest ).
       RUN set-position IN h_v-navest ( 17.67 , 12.20 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 34.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/v-ordt.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-ordt-2 ).
       RUN set-position IN h_v-ordt-2 ( 19.57 , 20.60 ) NO-ERROR.
       /* Size in UIB:  ( 4.52 , 120.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1,2':U) NO-ERROR.

       /* Links to SmartViewer h_vi-ord-3. */
       RUN add-link IN adm-broker-hdl ( h_b-ordinq , 'Record':U , h_vi-ord-3 ).

       /* Links to SmartBrowser h_b-ordm. */
       RUN add-link IN adm-broker-hdl ( h_p-disabl , 'TableIO':U , h_b-ordm ).
       RUN add-link IN adm-broker-hdl ( h_v-ord , 'Record':U , h_b-ordm ).

       /* Links to SmartViewer h_v-navest. */
       RUN add-link IN adm-broker-hdl ( h_b-ordinq , 'nav-itm':U , h_v-navest ).

       /* Links to SmartViewer h_v-ordt-2. */
       RUN add-link IN adm-broker-hdl ( h_b-ordinq , 'Record':U , h_v-ordt-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_vi-ord-3 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-ordm ,
             h_vi-ord-3 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-disabl ,
             h_b-ordm , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-navest ,
             h_p-disabl , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-ordt-2 ,
             h_v-navest , 'AFTER':U ).
    END. /* Page 4 */
    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oeinq/w-ordjob.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_w-ordjob ).
       /* Position in AB:  ( 6.00 , 40.00 ) */
       /* Size in UIB:  ( 1.86 , 10.80 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartWindow h_w-ordjob. */
       RUN add-link IN adm-broker-hdl ( h_b-ordinq , 'Record':U , h_w-ordjob ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'quote':U , h_w-ordjob ).

    END. /* Page 5 */
    WHEN 6 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/relticket.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_relticket ).
       RUN set-position IN h_relticket ( 1.10 , 54.20 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/vi-ordl.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-ordl ).
       RUN set-position IN h_vi-ordl ( 4.57 , 9.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.19 , 146.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/b-ordrel.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_b-ordrel ).
       RUN set-position IN h_b-ordrel ( 8.86 , 9.00 ) NO-ERROR.
       RUN set-size IN h_b-ordrel ( 13.57 , 147.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-ordrel.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-ordrel ).
       RUN set-position IN h_p-ordrel ( 22.67 , 20.00 ) NO-ERROR.
       RUN set-size IN h_p-ordrel ( 1.91 , 85.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-orel.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-orel ).
       RUN set-position IN h_p-orel ( 22.67 , 105.00 ) NO-ERROR.
       RUN set-size IN h_p-orel ( 1.91 , 15.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-obol.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-obol ).
       RUN set-position IN h_p-obol ( 22.67 , 120.00 ) NO-ERROR.
       RUN set-size IN h_p-obol ( 1.91 , 13.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1,3':U) NO-ERROR.

       /* Links to SmartObject h_relticket. */
       RUN add-link IN adm-broker-hdl ( h_relticket , 'relTicket':U , THIS-PROCEDURE ).

       /* Links to SmartViewer h_vi-ordl. */
       RUN add-link IN adm-broker-hdl ( h_b-ordinq , 'Record':U , h_vi-ordl ).
       RUN add-link IN adm-broker-hdl ( h_v-oeqtys , 'oe-qtys':U , h_vi-ordl ).

       /* Links to SmartBrowser h_b-ordrel. */
       RUN add-link IN adm-broker-hdl ( h_b-ordinq , 'Record':U , h_b-ordrel ).
       RUN add-link IN adm-broker-hdl ( h_p-obol , 'ordbol':U , h_b-ordrel ).
       RUN add-link IN adm-broker-hdl ( h_p-ordrel , 'TableIO':U , h_b-ordrel ).
       RUN add-link IN adm-broker-hdl ( h_p-orel , 'ordrel':U , h_b-ordrel ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_relticket ,
             h_expxls , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_vi-ordl ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-ordrel ,
             h_vi-ordl , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-ordrel ,
             h_b-ordrel , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-orel ,
             h_p-ordrel , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-obol ,
             h_p-orel , 'AFTER':U ).
    END. /* Page 6 */
    WHEN 7 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/w-ordest.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_w-ordest ).
       /* Position in AB:  ( 5.19 , 5.60 ) */
       /* Size in UIB:  ( 2.05 , 11.60 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartWindow h_w-ordest. */
       RUN add-link IN adm-broker-hdl ( h_b-ordinq , 'Record':U , h_w-ordest ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'quote':U , h_w-ordest ).

    END. /* Page 7 */
    WHEN 8 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/vi-ord.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-ord-2 ).
       RUN set-position IN h_vi-ord-2 ( 5.29 , 8.60 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/v-ordt.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-ordt ).
       RUN set-position IN h_v-ordt ( 7.43 , 20.60 ) NO-ERROR.
       /* Size in UIB:  ( 4.52 , 120.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_vi-ord-2. */
       RUN add-link IN adm-broker-hdl ( h_b-ordinq , 'Record':U , h_vi-ord-2 ).

       /* Links to SmartViewer h_v-ordt. */
       RUN add-link IN adm-broker-hdl ( h_b-ordinq , 'Record':U , h_v-ordt ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_vi-ord-2 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-ordt ,
             h_vi-ord-2 , 'AFTER':U ).
    END. /* Page 8 */
    WHEN 9 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/w-ordfg.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_w-ordfg ).
       /* Position in AB:  ( 5.76 , 4.00 ) */
       /* Size in UIB:  ( 2.05 , 11.60 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartWindow h_w-ordfg. */
       RUN add-link IN adm-broker-hdl ( h_b-ordinq , 'Record':U , h_w-ordfg ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'quote':U , h_w-ordfg ).

    END. /* Page 9 */
    WHEN 10 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oeinq/w-oeinvl.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_w-oeinvl ).
       /* Position in AB:  ( 4.57 , 3.20 ) */
       /* Size in UIB:  ( 1.86 , 10.80 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartWindow h_w-oeinvl. */
       RUN add-link IN adm-broker-hdl ( h_b-ordinq , 'Record':U , h_w-oeinvl ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'quote':U , h_w-oeinvl ).

    END. /* Page 10 */
    WHEN 11 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/vi-ordlr.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-ordlr ).
       RUN set-position IN h_vi-ordlr ( 5.05 , 8.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.19 , 145.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/v-shpnot.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-shpnot ).
       RUN set-position IN h_v-shpnot ( 10.05 , 23.60 ) NO-ERROR.
       /* Size in UIB:  ( 6.43 , 114.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updcan.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updcan ).
       RUN set-position IN h_p-updcan ( 17.67 , 65.00 ) NO-ERROR.
       RUN set-size IN h_p-updcan ( 1.76 , 31.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('6,3':U) NO-ERROR.

       /* Links to SmartViewer h_vi-ordlr. */
       RUN add-link IN adm-broker-hdl ( h_b-ordrel , 'Record':U , h_vi-ordlr ).
       RUN add-link IN adm-broker-hdl ( h_v-oeqtys , 'oe-qtys':U , h_vi-ordlr ).

       /* Links to SmartViewer h_v-shpnot. */
       RUN add-link IN adm-broker-hdl ( h_b-ordrel , 'Record':U , h_v-shpnot ).
       RUN add-link IN adm-broker-hdl ( h_p-updcan , 'TableIO':U , h_v-shpnot ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_vi-ordlr ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-shpnot ,
             h_vi-ordlr , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updcan ,
             h_v-shpnot , 'AFTER':U ).
    END. /* Page 11 */
    WHEN 12 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/w-ordesf.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_w-ordesf ).
       /* Position in AB:  ( 6.48 , 15.00 ) */
       /* Size in UIB:  ( 1.86 , 10.80 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('8':U) NO-ERROR.

       /* Links to SmartWindow h_w-ordesf. */
       RUN add-link IN adm-broker-hdl ( h_v-ordt , 'Record':U , h_w-ordesf ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'quote':U , h_w-ordesf ).

       /* Adjust the tab order of the smart objects. */
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
  {src/adm/template/row-list.i "oe-ord"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-ord"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-credit-inq W-Win 
PROCEDURE init-credit-inq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input param ip-handle as handle no-undo.
  
  if valid-handle(h-detail) then run dispatch in h-detail ('destroy').    
  
  run init-object
      ('oe/w-credit.w', {&window-name}:handle, 'Edge-Pixels=0', output h-detail).

 /* run set-position in h-detail (2.00, 2.00).                 */
/*  run set-size in h-detail  */

  run add-link in adm-broker-hdl (ip-handle,"record", h-detail) no-error.

  run dispatch in h-detail ('initialize').
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-history W-Win 
PROCEDURE init-history :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input param ip-handle as handle no-undo.

  if valid-handle(h-detail) then run dispatch in h-detail ('destroy').

  run init-object
      ('oe/w-oehist.w', {&window-name}:handle, 'Edge-Pixels=0', output h-detail).

 /* run set-position  in h-detail  (2.00, 2.50) .               */
   /* run set-size in h_detail*/
  run add-link in adm-broker-hdl (ip-handle,"record", h-detail) no-error.

  run dispatch in h-detail ('initialize').
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-change-page W-Win 
PROCEDURE local-change-page :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var ls-est-no as cha no-undo.
  
  /* Code placed here will execute PRIOR to standard behavior. */
  
  run get-attribute ("current-page").
  assign li-prev-page = li-cur-page
         li-cur-page = int(return-value).

  if li-cur-page = 7 then do:  /* estimate */
     li-last-page = li-prev-page.
     run get-link-handle in adm-broker-hdl (this-procedure,"estimate-target",output char-hdl).
     if valid-handle(widget-handle(char-hdl)) then do:
        run get-line-est in widget-handle(char-hdl) (output ls-est-no).
        if ls-est-no = "" then do:
           message "SORRY, NO ESTIMATE EXISTS FOR THIS ESTIMATE." 
               view-as alert-box error.      
           run hide-estimate.
           return no-apply.        
        end. 
        ELSE DO:
           FIND FIRST est WHERE est.company = g_company AND
                                est.est-no = ls-est-no NO-LOCK NO-ERROR.
           IF AVAIL est AND est.est-type <= 4 THEN do:
              RUN select-page (12).  
              li-prev-page = li-last-page.
              RETURN.
           END.
        END.
     END.
  end.
         
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'change-page':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/winReSizePgChg.i}
  SESSION:SET-WAIT-STATE('').
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print_RelTicket W-Win 
PROCEDURE print_RelTicket :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN Get_Procedure IN Persistent-Handle ('oe-relh_.',OUTPUT run-proc,yes).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE relTicketEnabled W-Win 
PROCEDURE relTicketEnabled :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipEnabled AS LOGICAL NO-UNDO.

  {methods/run_link.i "relTicket-source" "setEnabled" "(ipEnabled)"}

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
   /*RUN Get_Procedure IN Persistent-Handle ('attach.',OUTPUT run-proc,no).
   IF run-proc NE '' THEN {methods/smartrun.i (rec_key_value,header_value)} .*/

   {methods/select_att.i}

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

 FIND FIRST b-oe-ordl WHERE
      b-oe-ordl.rec_key EQ rec_key_value
      NO-LOCK NO-ERROR.

 IF AVAIL b-oe-ordl THEN
 DO:
    v-order-no = INT(substring(header_value,1,INDEX(HEADER_value,"-") - 1)).

    FIND FIRST b-oe-ord WHERE
         b-oe-ord.company EQ b-oe-ordl.company AND
         b-oe-ord.ord-no  EQ v-order-no
         NO-LOCK NO-ERROR.

    IF AVAIL b-oe-ord THEN
       FIND FIRST b-cust WHERE
            b-cust.company EQ b-oe-ord.company AND
            b-cust.cust-no EQ b-oe-ord.cust-no
            NO-LOCK NO-ERROR.

     IF AVAIL b-cust THEN DO:
         {methods/select_attcust.i
            b-cust.rec_key
            "'Customer: ' + b-cust.cust-no + ' - ' + 'Name: ' + b-cust.name"
            b-oe-ord.ord-no}
     END.                          
 END.
 
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select_ONote W-Win 
PROCEDURE select_ONote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {methods/select_ONote.i rec_key_value cHeaderValue """" 1}

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
  {src/adm/template/snd-list.i "oe-ord"}

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

