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

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE winReSize
&SCOPED-DEFINE h_Browse01 h_b-crdinq
&SCOPED-DEFINE h_Object01 h_v-oeqtys
&SCOPED-DEFINE h_Object02 h_vp-webitm-2
&SCOPED-DEFINE h_Object03 h_p-navico-2
&SCOPED-DEFINE h_Object04 h_v-ordt-2
&SCOPED-DEFINE initialBrowse checkInitialBrowse

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
&scoped-define item_spec  FGITEM 
def var li-prev-page as int init 1 no-undo.
def var li-cur-page as int init 1 no-undo.
def var h-detail as handle no-undo.
DEF VAR lv-initial AS LOG INIT YES NO-UNDO.
/* DEF VAR  h_b-crdinqA AS HANDLE NO-UNDO. */

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


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setWindowFreeze W-Win 
FUNCTION setWindowFreeze RETURNS LOGICAL
  ( plWindowsLocked as logical)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-crdinq AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-crdinqa AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-oprmtx AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-ordlt AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-ordm AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-ordrel AS HANDLE NO-UNDO.
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_optionse AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-credapp-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updcan AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updcan-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_smartmsg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-hldapp AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-navest AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-oebill AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-oeqtys AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-ord AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-ordt AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-ordt-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-shpnot AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-ord AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-ord-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-ordl AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-ordlr AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vp-webitm-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_w-ordest AS HANDLE NO-UNDO.
DEFINE VARIABLE h_w-ordfg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_export AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SIZE 169.4 BY 24.24
         BGCOLOR 15 .

DEFINE FRAME OPTIONS-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 50 ROW 1
         SIZE 108 BY 2.14
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
         TITLE              = "Credit Hold / Approve Orders"
         HEIGHT             = 24.1
         WIDTH              = 159.4
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
ON END-ERROR OF W-Win /* Credit Hold / Approve Orders */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Credit Hold / Approve Orders */
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
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
procedure SendMessageA external "user32.dll":
    define input  parameter hwnd   as long no-undo.
    define input  parameter wmsg   as long no-undo.
    define input  parameter wparam as long no-undo.
    define input  parameter lparam as long no-undo.
    define return parameter rc     as long no-undo.
end procedure.

procedure RedrawWindow external "user32.dll":
    def input parameter v-hwnd  as long no-undo.
    def input parameter v-rect  as long no-undo.
    def input parameter v-rgn   as long no-undo.
    def input parameter v-flags as long no-undo.
    def return parameter v-ret  as long no-undo.
end procedure.
&ENDIF
/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-prmtx-link W-Win 
PROCEDURE add-prmtx-link :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*
  def output param op-handle as handle no-undo.
  
  RUN add-link IN adm-broker-hdl ( h_q-orpmtx , 'Record':U , h_oe-prmtx ).
  op-handle = h_oe-prmtx.
 */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
             INPUT  'smartobj/optionse.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_optionse ).
       RUN set-position IN h_optionse ( 1.00 , 45.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 63.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/exit.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.00 , 101.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Brws Order|View Order|Items|Misc. Chgs|Release|Estimate|Order Total|FG Item|Matrix|Ship Notes|Bill Notes' + ',
                     FOLDER-TAB-TYPE = 2':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 3.14 , 1.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 21.91 , 159.00 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_exit ,
             h_optionse , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_folder ,
             FRAME message-frame:HANDLE , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/export.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_export ).
       RUN set-position IN h_export ( 1.00 , 37.20 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/b-crdinq.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-crdinq ).
       RUN set-position IN h_b-crdinq ( 4.81 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 19.52 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/b-crdinqa.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_b-crdinqa ).
       RUN set-position IN h_b-crdinqa ( 4.81 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 19.52 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/p-credapp.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_p-credapp-2 ).
       RUN set-position IN h_p-credapp-2 ( 5.00 , 130.00 ) NO-ERROR.
       /* Size in UIB:  ( 3.10 , 17.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('4':U) NO-ERROR.

        /* Links to SmartViewer h_export. */
       RUN add-link IN adm-broker-hdl ( h_b-crdinqa , 'export-xl':U , h_export ).
       RUN add-link IN adm-broker-hdl ( h_b-crdinq , 'export-xl':U , h_export ).

       /* Links to SmartNavBrowser h_b-crdinq. */
       RUN add-link IN adm-broker-hdl ( h_p-navico-2 , 'Navigation':U , h_b-crdinq ).
       RUN add-link IN adm-broker-hdl ( h_b-crdinq , 'Record':U , THIS-PROCEDURE ).

       /* Links to SmartViewer h_p-credapp-2. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'hold-approve':U , h_p-credapp-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-crdinq ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-crdinqa ,
             h_b-crdinq , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-credapp-2 ,
             h_b-crdinqa , 'AFTER':U ).
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
       RUN set-position IN h_v-ord ( 4.81 , 6.60 ) NO-ERROR.
       /* Size in UIB:  ( 17.48 , 152.40 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/v-hldapp.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-hldapp ).
       RUN set-position IN h_v-hldapp ( 22.43 , 90.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.38 , 40.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-navest.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-navest ).
       RUN set-position IN h_v-navest ( 23.00 , 25.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 34.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_v-ord. */
       RUN add-link IN adm-broker-hdl ( h_b-crdinq , 'Record':U , h_v-ord ).

       /* Links to SmartViewer h_v-hldapp. */
       RUN add-link IN adm-broker-hdl ( h_v-ord , 'hold-approve':U , h_v-hldapp ).
       RUN add-link IN adm-broker-hdl ( h_v-ord , 'Record':U , h_v-hldapp ).

       /* Links to SmartViewer h_v-navest. */
       RUN add-link IN adm-broker-hdl ( h_b-crdinq , 'nav-itm':U , h_v-navest ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-ord ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-hldapp ,
             h_v-ord , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-navest ,
             h_v-hldapp , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/vi-ord.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-ord ).
       RUN set-position IN h_vi-ord ( 4.81 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/b-ordlt.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-ordlt ).
       RUN set-position IN h_b-ordlt ( 6.71 , 4.00 ) NO-ERROR.
       RUN set-size IN h_b-ordlt ( 14.81 , 145.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/v-oeqtys.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-oeqtys ).
       RUN set-position IN h_v-oeqtys ( 20.52 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.86 , 152.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/vp-webitm.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vp-webitm-2 ).
       RUN set-position IN h_vp-webitm-2 ( 23.62 , 26.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.29 , 105.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2,1':U) NO-ERROR.

       /* Links to SmartViewer h_vi-ord. */
       RUN add-link IN adm-broker-hdl ( h_v-ord , 'Record':U , h_vi-ord ).

       /* Links to SmartNavBrowser h_b-ordlt. */
       RUN add-link IN adm-broker-hdl ( h_v-ord , 'Record':U , h_b-ordlt ).
       RUN add-link IN adm-broker-hdl ( h_vp-webitm-2 , 'oeitem':U , h_b-ordlt ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'estimate':U , h_b-ordlt ).

       /* Links to SmartViewer h_v-oeqtys. */
       RUN add-link IN adm-broker-hdl ( h_b-ordlt , 'Record':U , h_v-oeqtys ).

       /* Links to SmartViewer h_vp-webitm-2. */
       RUN add-link IN adm-broker-hdl ( h_b-crdinq , 'Record':U , h_vp-webitm-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_vi-ord ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-ordlt ,
             h_vi-ord , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-oeqtys ,
             h_b-ordlt , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_vp-webitm-2 ,
             h_v-oeqtys , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/b-ordm.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_b-ordm ).
       RUN set-position IN h_b-ordm ( 5.29 , 6.00 ) NO-ERROR.
       RUN set-size IN h_b-ordm ( 10.71 , 136.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-navico.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navico-2 ).
       RUN set-position IN h_p-navico-2 ( 16.48 , 56.00 ) NO-ERROR.
       RUN set-size IN h_p-navico-2 ( 1.76 , 33.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/v-ordt.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-ordt-2 ).
       RUN set-position IN h_v-ordt-2 ( 19.10 , 13.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.52 , 120.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartBrowser h_b-ordm. */
       RUN add-link IN adm-broker-hdl ( h_v-ord , 'Record':U , h_b-ordm ).

       /* Links to SmartViewer h_v-ordt-2. */
       RUN add-link IN adm-broker-hdl ( h_v-ord , 'Record':U , h_v-ordt-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-ordm ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navico-2 ,
             h_b-ordm , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-ordt-2 ,
             h_p-navico-2 , 'AFTER':U ).
    END. /* Page 4 */
    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/vi-ordl.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-ordl ).
       RUN set-position IN h_vi-ordl ( 4.81 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.19 , 146.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/b-ordrel.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_b-ordrel ).
       /* Position in AB:  ( 9.57 , 2.00 ) */
       /* Size in UIB:  ( 13.57 , 147.00 ) */
       RUN set-position IN h_b-ordrel ( 9.10 , 3.00 ) NO-ERROR.
       RUN set-size IN h_b-ordrel ( 13.57 , 147.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('3':U) NO-ERROR.

       /* Links to SmartViewer h_vi-ordl. */
       RUN add-link IN adm-broker-hdl ( h_b-ordlt , 'Record':U , h_vi-ordl ).

       /* Links to  h_b-ordrel. */
       RUN add-link IN adm-broker-hdl ( h_b-ordlt , 'Record':U , h_b-ordrel ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_vi-ordl ,
             h_folder , 'AFTER':U ).
    END. /* Page 5 */
    WHEN 6 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/w-ordest.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_w-ordest ).
       /* Position in AB:  ( 5.19 , 5.60 ) */
       /* Size in UIB:  ( 2.05 , 11.60 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('3':U) NO-ERROR.

       /* Links to SmartWindow h_w-ordest. */
       RUN add-link IN adm-broker-hdl ( h_b-ordlt , 'Record':U , h_w-ordest ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'quote':U , h_w-ordest ).

    END. /* Page 6 */
    WHEN 7 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/v-ordt.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-ordt ).
       RUN set-position IN h_v-ordt ( 5.52 , 9.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.52 , 120.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartViewer h_v-ordt. */
       RUN add-link IN adm-broker-hdl ( h_v-ord , 'Record':U , h_v-ordt ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-ordt ,
             h_folder , 'AFTER':U ).
    END. /* Page 7 */
    WHEN 8 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/w-ordfg.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_w-ordfg ).
       /* Position in AB:  ( 5.76 , 4.00 ) */
       /* Size in UIB:  ( 2.05 , 11.60 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('3':U) NO-ERROR.

       /* Links to SmartWindow h_w-ordfg. */
       RUN add-link IN adm-broker-hdl ( h_b-ordlt , 'Record':U , h_w-ordfg ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'quote':U , h_w-ordfg ).

    END. /* Page 8 */
    WHEN 9 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/b-oprmtx.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-oprmtx ).
       RUN set-position IN h_b-oprmtx ( 4.81 , 3.00 ) NO-ERROR.
       RUN set-size IN h_b-oprmtx ( 19.52 , 145.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('3':U) NO-ERROR.

       /* Links to SmartNavBrowser h_b-oprmtx. */
       RUN add-link IN adm-broker-hdl ( h_b-ordlt , 'Record':U , h_b-oprmtx ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-oprmtx ,
             h_folder , 'AFTER':U ).
    END. /* Page 9 */
    WHEN 10 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/vi-ordlr.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-ordlr ).
       RUN set-position IN h_vi-ordlr ( 5.05 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.19 , 145.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/v-shpnot.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-shpnot ).
       RUN set-position IN h_v-shpnot ( 9.81 , 11.00 ) NO-ERROR.
       /* Size in UIB:  ( 6.43 , 114.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updcan.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updcan ).
       RUN set-position IN h_p-updcan ( 16.24 , 46.00 ) NO-ERROR.
       RUN set-size IN h_p-updcan ( 2.14 , 31.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('5':U) NO-ERROR.

       /* Links to SmartViewer h_vi-ordlr. */
       RUN add-link IN adm-broker-hdl ( h_b-ordrel , 'Record':U , h_vi-ordlr ).

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
    END. /* Page 10 */
    WHEN 11 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/vi-ord.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-ord-2 ).
       RUN set-position IN h_vi-ord-2 ( 5.29 , 8.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/v-oebill.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-oebill ).
       RUN set-position IN h_v-oebill ( 8.14 , 21.00 ) NO-ERROR.
       /* Size in UIB:  ( 6.14 , 113.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updcan.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updcan-2 ).
       RUN set-position IN h_p-updcan-2 ( 15.76 , 57.00 ) NO-ERROR.
       RUN set-size IN h_p-updcan-2 ( 1.76 , 31.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_vi-ord-2. */
       RUN add-link IN adm-broker-hdl ( h_b-crdinq , 'Record':U , h_vi-ord-2 ).

       /* Links to SmartViewer h_v-oebill. */
       RUN add-link IN adm-broker-hdl ( h_b-crdinq , 'Record':U , h_v-oebill ).
       RUN add-link IN adm-broker-hdl ( h_p-updcan-2 , 'TableIO':U , h_v-oebill ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_vi-ord-2 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-oebill ,
             h_vi-ord-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updcan-2 ,
             h_v-oebill , 'AFTER':U ).
    END. /* Page 11 */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkInitialBrowse W-Win 
PROCEDURE checkInitialBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cSelection AS CHAR NO-UNDO.
/* Program needs to switch between browsers initially, otherwise, */
/* it reverts to the approved browse by itself on changing pages */
RUN getSelectApprovedHold IN h_p-credapp-2 (OUTPUT cSelection).
IF cSelection EQ "H" THEN DO:
  RUN switch-ord-status (INPUT "A").
  RUN switch-ord-status (INPUT "H").
END.
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
  VIEW FRAME OPTIONS-FRAME IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-OPTIONS-FRAME}
  VIEW FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
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

  if li-cur-page = 6 then do:  /* estimate */
     run get-link-handle in adm-broker-hdl (this-procedure,"estimate-target",output char-hdl).
     if valid-handle(widget-handle(char-hdl)) then do:
        run get-line-est in widget-handle(char-hdl) (output ls-est-no).
        if ls-est-no = "" then do:
           message "SORRY, NO ESTIMATE EXISTS FOR THIS ESTIMATE." 
               view-as alert-box error.      
         run hide-estimate.
         return no-apply.        
      end.    
    end.                            
  end.
  if li-cur-page = 1 AND VALID-HANDLE(h_p-credapp-2) then do:
       RUN adm-hide IN h_p-credapp-2.
       RUN adm-view IN h_p-credapp-2.
  END.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'change-page':U ) .
  if li-cur-page = 1 AND VALID-HANDLE(h_p-credapp-2) THEN do:
       RUN adm-hide IN h_p-credapp-2.
       RUN adm-view IN h_p-credapp-2.
  END.
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
  IF VALID-HANDLE(h_b-crdinqa) THEN
      RUN adm-hide IN h_b-crdinqa.
         RUN adm-hide IN h_p-credapp-2.
       RUN adm-view IN h_p-credapp-2.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lockWindow W-Win 
PROCEDURE lockWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  define input parameter phWindow as handle  no-undo.
  define input parameter plLock   as logical no-undo.

  define variable iRet as integer no-undo. 

&IF DEFINED(FWD-VERSION) EQ 0 &THEN
  &GLOBAL-DEFINE WM_SETREDRAW     11
  &GLOBAL-DEFINE RDW_ALLCHILDREN 128
  &GLOBAL-DEFINE RDW_ERASE         4
  &GLOBAL-DEFINE RDW_INVALIDATE    1
&ENDIF

  /* Now, only lock when the semaphore is increased to 1 */
  if plLock  then
  do:

&IF DEFINED(FWD-VERSION) EQ 0 &THEN
    run SendMessageA( phWindow:hwnd /* {&window-name}:hwnd */
                    , {&WM_SETREDRAW}
                    , 0
                    , 0
                    , output iRet
                    ).
&ELSE
    phWindow:DISABLE-REDRAW = TRUE.
&ENDIF
  end.

  /* And only unlock after the last unlock command */
  else 
  do:
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
    run SendMessageA( phWindow:hwnd /* {&window-name}:hwnd */
                    , {&WM_SETREDRAW}
                    , 1
                    , 0
                    , output iRet
                    ).
    
    run RedrawWindow( phWindow:hwnd /* {&window-name}:hwnd */
                    , 0
                    , 0
                    , {&RDW_ALLCHILDREN} + {&RDW_ERASE} + {&RDW_INVALIDATE}
                    , output iRet
                    ).
&ELSE
    phWindow:DISABLE-REDRAW = FALSE.
&ENDIF

  end. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pass-set-item-rec W-Win 
PROCEDURE pass-set-item-rec :
/*------------------------------------------------------------------------------
  Purpose:     placeholder - used in oe/w-order.w
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER iprOeOrdRec AS RECID NO-UNDO.
DEF INPUT PARAMETER iprOeOrdlRec AS RECID NO-UNDO.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE stat-order W-Win 
PROCEDURE stat-order :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE switch-ord-status W-Win 
PROCEDURE switch-ord-status :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipStatus AS CHAR NO-UNDO.
    DEF VAR ll AS LOGICAL no-undo.
    ll = setWindowFreeze(YES).
    DO ON ERROR UNDO, LEAVE:
      
      IF ipStatus EQ "A" THEN DO:
  
          /* Links to SmartNavBrowser h_b-crdinq. */
         /* RUN remove-link IN adm-broker-hdl ( h_p-navico , 'Navigation':U , h_b-crdinq ). */
          IF VALID-HANDLE(h_b-crdinqa) THEN
              RUN adm-view IN h_b-crdinqa.
         IF VALID-HANDLE(h_b-crdinq) THEN DO:     
           IF VALID-HANDLE(h_v-navest) THEN
            RUN remove-link IN adm-broker-hdl ( h_b-crdinq , 'nav-itm':U , h_v-navest ).
           IF VALID-HANDLE(h_vp-webitm-2) THEN
             RUN remove-link IN adm-broker-hdl ( h_b-crdinq , 'Record':U , h_vp-webitm-2 ).
           IF VALID-HANDLE(h_vi-ord-2) THEN
             RUN remove-link IN adm-broker-hdl ( h_b-crdinq , 'Record':U , h_vi-ord-2 ).
           IF VALID-HANDLE(h_v-oebill) THEN
             RUN remove-link IN adm-broker-hdl ( h_b-crdinq , 'Record':U , h_v-oebill ).
  
             RUN adm-hide IN h_b-crdinq.
  /*            RUN adm-destroy IN h_b-crdinq. */
             DELETE OBJECT h_b-crdinq.
         END.
  
          RUN cleanup-links IN adm-broker-hdl.
         IF VALID-HANDLE(h_b-crdinqa) THEN DO: 
             RUN adm-hide IN h_b-crdinqa.
  /*            RUN adm-destroy IN h_b-crdinq. */
             DELETE OBJECT h_b-crdinqa.
         END.
          RUN cleanup-links IN adm-broker-hdl.           
  /*         RUN remove-link IN adm-broker-hdl ( h_p-navico-2 , 'Navigation':U , h_b-crdinq ). */
  /*         RUN remove-link IN adm-broker-hdl ( h_b-crdinq , 'Record':U , THIS-PROCEDURE ).   */
  
  
  /*         RUN adm-hide IN h_b-crdinq. */
         RUN init-object IN THIS-PROCEDURE (
               INPUT  'oe/b-crdinqA.w':U ,
               INPUT  FRAME F-Main:HANDLE ,
               INPUT  'Layout = ':U ,
               OUTPUT h_b-crdinqA ).
         RUN set-position IN h_b-crdinqA ( 4.57 , 2.00 ) NO-ERROR.
         /* Size in UIB:  ( 19.52 , 148.00 ) */  
  
         /* Initialize other pages that this page requires. */
         RUN init-pages IN THIS-PROCEDURE ('2,4':U) NO-ERROR.
         RUN adm-view IN h_b-crdinqA.
       
   
         RUN local-initialize IN h_b-crdinqA. 
         RUN local-open-query IN h_b-crdinqA. 
         RUN adm-hide IN h_p-credapp-2.
         RUN adm-view IN h_p-credapp-2.
  
         /* Size in UIB:  ( 1.43 , 34.00 ) */
          RUN add-link IN adm-broker-hdl ( h_p-navico-2 , 'Navigation':U , h_b-crdinqA ). 
          RUN add-link IN adm-broker-hdl ( h_b-crdinqA , 'Record':U , THIS-PROCEDURE ).   
          RUN add-link IN adm-broker-hdl ( h_b-crdinqA , 'Record':U , h_v-ord ).          
          RUN add-link IN adm-broker-hdl ( h_b-crdinqA , 'nav-itm':U , h_v-navest ). 
  
          IF VALID-HANDLE(h_vp-webitm-2) THEN
            RUN add-link IN adm-broker-hdl ( h_b-crdinqA , 'Record':U , h_vp-webitm-2 ).
          IF VALID-HANDLE(h_vi-ord-2) THEN
            RUN add-link IN adm-broker-hdl ( h_b-crdinqA , 'Record':U , h_vi-ord-2 ).
          IF VALID-HANDLE(h_v-oebill) THEN
            RUN add-link IN adm-broker-hdl ( h_b-crdinqA , 'Record':U , h_v-oebill ).

           /* Links to SmartViewer h_export. */
         RUN add-link IN adm-broker-hdl ( h_b-crdinqa , 'export-xl':U , h_export ).
       /*RUN add-link IN adm-broker-hdl ( h_b-crdinq , 'export-xl':U , h_export ).*/
         
          RUN local-get-first IN h_b-crdinqa.
          RUN adm-row-available IN h_b-crdinqA.
          RUN local-initialize IN h_v-navest.
  
          /* Adjust the tab order of the smart objects. */
          RUN adjust-tab-order IN adm-broker-hdl ( h_b-crdinqA ,
               h_folder , 'AFTER':U ).    
  
          RUN adm-view IN h_p-credapp-2.
          RUN apply-arrow IN h_b-crdinqa. 
          RUN notify ('row-available':U).
  
  
      END.
      ELSE DO:
  
          IF VALID-HANDLE(h_b-crdinqA) THEN DO: 
  
            IF VALID-HANDLE(h_vp-webitm-2) THEN
              RUN remove-link IN adm-broker-hdl ( h_b-crdinqA , 'Record':U , h_vp-webitm-2 ).
            IF VALID-HANDLE(h_vi-ord-2) THEN
              RUN remove-link IN adm-broker-hdl ( h_b-crdinqA , 'Record':U , h_vi-ord-2 ).
            IF VALID-HANDLE(h_v-oebill) THEN
              RUN remove-link IN adm-broker-hdl ( h_b-crdinqA , 'Record':U , h_v-oebill ).
              RUN adm-hide IN h_b-crdinqA.
              DELETE OBJECT h_b-crdinqA.
          END.
          RUN cleanup-links IN adm-broker-hdl.
          IF VALID-HANDLE(h_b-crdinq) THEN DO: 
  
             RUN adm-hide IN h_b-crdinq.
             DELETE OBJECT h_b-crdinq.
          END.        
          RUN init-object IN THIS-PROCEDURE (
               INPUT  'oe/b-crdinq.w':U ,
               INPUT  FRAME F-Main:HANDLE ,
               INPUT  'Layout = ':U ,
               OUTPUT h_b-crdinq ).
         RUN set-position IN h_b-crdinq ( 4.57 , 2.00 ) NO-ERROR.
         /* Size in UIB:  ( 19.52 , 148.00 ) */  
         /* Adjust the tab order of the smart objects. */
         RUN adjust-tab-order IN adm-broker-hdl ( h_b-crdinq ,
               h_folder , 'AFTER':U ).       
         /* Initialize other pages that this page requires. */
         RUN init-pages IN THIS-PROCEDURE ('2,4':U) NO-ERROR.
         RUN adm-view IN h_b-crdinq.
  
  /*        IF VALID-HANDLE(h_b-crdinq) THEN */
  /*          RUN adm-view IN h_b-crdinq.    */
         RUN local-initialize IN h_b-crdinq.
         RUN local-open-query IN h_b-crdinq.
         RUN adm-hide IN h_p-credapp-2.
         RUN adm-view IN h_p-credapp-2.
  
  
  
         /* Links to SmartNavBrowser h_b-crdinq. */  
         RUN add-link IN adm-broker-hdl ( h_p-navico-2 , 'Navigation':U , h_b-crdinq ).
         RUN add-link IN adm-broker-hdl ( h_b-crdinq , 'Record':U , THIS-PROCEDURE ).
         RUN add-link IN adm-broker-hdl ( h_b-crdinq , 'Record':U , h_v-ord ).
         RUN add-link IN adm-broker-hdl ( h_b-crdinq , 'nav-itm':U , h_v-navest ).
          /* Links to SmartViewer h_export. */
         RUN add-link IN adm-broker-hdl ( h_b-crdinq , 'export-xl':U , h_export ).
  /*        RUN add-link IN adm-broker-hdl ( h_b-crdinq , 'Record':U , h_vp-webitm-2 ). */
  /*        RUN add-link IN adm-broker-hdl ( h_b-crdinq , 'Record':U , h_vi-ord-2 ).    */
  /*        RUN add-link IN adm-broker-hdl ( h_b-crdinq , 'Record':U , h_v-oebill ).    */
                 IF VALID-HANDLE(h_vp-webitm-2) THEN
            RUN add-link IN adm-broker-hdl ( h_b-crdinq , 'Record':U , h_vp-webitm-2 ).
          IF VALID-HANDLE(h_vi-ord-2) THEN
            RUN add-link IN adm-broker-hdl ( h_b-crdinq , 'Record':U , h_vi-ord-2 ).
          IF VALID-HANDLE(h_v-oebill) THEN
            RUN add-link IN adm-broker-hdl ( h_b-crdinq , 'Record':U , h_v-oebill ).
          RUN local-get-first IN h_b-crdinq.
          RUN adm-row-available IN h_b-crdinq.
  
          /* Adjust the tab order of the smart objects. */
          RUN adjust-tab-order IN adm-broker-hdl ( h_b-crdinq ,
               h_folder , 'AFTER':U ).  
  
  
         RUN apply-arrow IN h_b-crdinq. 
         RUN notify ('row-available':U).
         RUN local-open-query IN h_b-crdinq.
  
      END. /* View Hold Status */
    END. /* On error... */
ll = setWindowFreeze(NO).
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setWindowFreeze W-Win 
FUNCTION setWindowFreeze RETURNS LOGICAL
  ( plWindowsLocked as logical) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/


  run LockWindow (input W-Win:handle, input plWindowsLocked).

  return true.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

