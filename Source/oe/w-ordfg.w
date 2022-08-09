&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: 

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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR ll-secure AS LOG INIT NO NO-UNDO.
DEFINE VARIABLE rec_key_value AS CHARACTER NO-UNDO.
DEFINE VARIABLE header_value AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i "WIN"}

def var li-current-page as int INIT 1 no-undo.
def var li-prev-page as int INIT 1 no-undo.
DEF VAR li-page-b4VendCost AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Record-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES oe-ordl
&Scoped-define FIRST-EXTERNAL-TABLE oe-ordl


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-ordl.
/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-ebfg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-eitem AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-ordfgi AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fg-set AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fgijob AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fgijob-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fgijobw AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemfg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemfg-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemfg-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemfg2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemfg2-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemfgt AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itmfgink AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itmfgink-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_locw AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-boxupd AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-calcc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-calcq AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-fg-bj AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-fgset AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navfg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updcan AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updcan-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updclr AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-upditm AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updncp AS HANDLE NO-UNDO.
DEFINE VARIABLE h_pricechg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_pv-graph AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-ordlfg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-eitem AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-fgimg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vendcostmtx AS HANDLE NO-UNDO.
/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 174 BY 28.57.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: ASI.oe-ordl
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
         TITLE              = "FG Item"
         HEIGHT             = 28.57
         WIDTH              = 174
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
         RESIZE             = YES
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* FG Item */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* FG Item */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  
  def var char-hdl as cha no-undo.
  run get-link-handle in adm-broker-hdl(this-procedure,"quote-source", output char-hdl).
  run hide-estimate in widget-handle(char-hdl).

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
{sys/inc/f3helpw.i}
{sys/inc/var.i new shared}
ASSIGN 
    cocode = g_Company
    locode = g_Loc.
{sys/inc/vendItemCost.i}

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
             INPUT  'FOLDER-LABELS = ':U + 'Browse|Detail|Totals|Bin/Jobs|Set parts|Colors|Vend Cost|History|Image' + ',
                     FOLDER-TAB-TYPE = 2':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 1.00 , 1.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 28.57 , 172.00 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/itemfg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itemfg ).
       RUN set-position IN h_itemfg ( 2.67 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 17.14 , 144.60 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-navfg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navfg ).
       RUN set-position IN h_p-navfg ( 23.52 , 3.00 ) NO-ERROR.
       RUN set-size IN h_p-navfg ( 2.14 , 40.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/p-upditm.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-upditm ).
       RUN set-position IN h_p-upditm ( 23.52 , 65.00 ) NO-ERROR.
       RUN set-size IN h_p-upditm ( 2.14 , 64.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/p-calcc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_p-calcc ).
       RUN set-position IN h_p-calcc ( 23.52 , 131.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.14 , 17.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/q-ordlfg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-ordlfg ).
       RUN set-position IN h_q-ordlfg ( 23.52 , 49.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.05 , 11.60 ) */

       /* Links to SmartViewer h_itemfg. */
       RUN add-link IN adm-broker-hdl ( h_p-upditm , 'TableIO':U , h_itemfg ).
       RUN add-link IN adm-broker-hdl ( h_q-ordlfg , 'Record':U , h_itemfg ).

       /* Links to SmartViewer h_p-calcc. */
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'calc-cost':U , h_p-calcc ).

       /* Links to SmartQuery h_q-ordlfg. */
       RUN add-link IN adm-broker-hdl ( h_p-navfg , 'Navigation':U , h_q-ordlfg ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'Record':U , h_q-ordlfg ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_itemfg ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navfg ,
             h_itemfg , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-upditm ,
             h_p-navfg , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-calcc ,
             h_p-upditm , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/itemfg2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itemfg2 ).
       RUN set-position IN h_itemfg2 ( 4.10 , 8.00 ) NO-ERROR.
       /* Size in UIB:  ( 5.71 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updcan.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 1,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updcan ).
       RUN set-position IN h_p-updcan ( 15.29 , 22.00 ) NO-ERROR.
       RUN set-size IN h_p-updcan ( 2.10 , 31.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_itemfg2. */
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'Record':U , h_itemfg2 ).
       RUN add-link IN adm-broker-hdl ( h_p-updcan , 'TableIO':U , h_itemfg2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_itemfg2 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updcan ,
             h_itemfg2 , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewerid/itemfg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itemfg-2 ).
       RUN set-position IN h_itemfg-2 ( 2.67 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 155.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/itemfgt.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itemfgt ).
       RUN set-position IN h_itemfgt ( 5.05 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 14.52 , 99.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updcan.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 1,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updcan-2 ).
       RUN set-position IN h_p-updcan-2 ( 19.62 , 28.00 ) NO-ERROR.
       RUN set-size IN h_p-updcan-2 ( 2.14 , 52.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_itemfg-2. */
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'Record':U , h_itemfg-2 ).

       /* Links to SmartViewer h_itemfgt. */
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'Record':U , h_itemfgt ).
       RUN add-link IN adm-broker-hdl ( h_p-updcan-2 , 'TableIO':U , h_itemfgt ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_itemfg-2 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_itemfgt ,
             h_itemfg-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updcan-2 ,
             h_itemfgt , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/locw.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_locw ).
       RUN set-position IN h_locw ( 2.67 , 2.00 ) NO-ERROR.
       RUN set-size IN h_locw ( 23.19 , 157.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartBrowser h_locw. */
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'Record':U , h_locw ).
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'repo-query':U , h_locw ).
       RUN add-link IN adm-broker-hdl ( h_locw , 'viewDetail':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_locw ,
             h_folder , 'AFTER':U ).
    END. /* Page 4 */
    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewerid/itemfg2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itemfg2-2 ).
       RUN set-position IN h_itemfg2-2 ( 2.67 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 3.29 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/fg-set.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_fg-set ).
       RUN set-position IN h_fg-set ( 7.19 , 9.00 ) NO-ERROR.
       RUN set-size IN h_fg-set ( 8.62 , 141.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-fgset.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-fgset ).
       RUN set-position IN h_p-fgset ( 17.19 , 35.00 ) NO-ERROR.
       RUN set-size IN h_p-fgset ( 2.24 , 74.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_itemfg2-2. */
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'Record':U , h_itemfg2-2 ).

       /* Links to SmartBrowser h_fg-set. */
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'Record':U , h_fg-set ).
       RUN add-link IN adm-broker-hdl ( h_p-fgset , 'TableIO':U , h_fg-set ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_itemfg2-2 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_fg-set ,
             h_itemfg2-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-fgset ,
             h_fg-set , 'AFTER':U ).
    END. /* Page 5 */
    WHEN 6 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewerid/itemfg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itemfg-3 ).
       RUN set-position IN h_itemfg-3 ( 2.43 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 155.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/itmfgink.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itmfgink-2 ).
       RUN set-position IN h_itmfgink-2 ( 4.57 , 96.00 ) NO-ERROR.
       /* Size in UIB:  ( 6.29 , 61.40 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/itmfgink.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itmfgink ).
       RUN set-position IN h_itmfgink ( 4.81 , 2.00 ) NO-ERROR.
       RUN set-size IN h_itmfgink ( 17.62 , 93.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/p-updclr.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updclr ).
       RUN set-position IN h_p-updclr ( 11.00 , 96.00 ) NO-ERROR.
       RUN set-size IN h_p-updclr ( 2.14 , 61.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/b-ebfg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-ebfg ).
       RUN set-position IN h_b-ebfg ( 13.38 , 96.00 ) NO-ERROR.
       RUN set-size IN h_b-ebfg ( 9.24 , 61.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_itemfg-3. */
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'Record':U , h_itemfg-3 ).

       /* Links to SmartViewer h_itmfgink-2. */
       RUN add-link IN adm-broker-hdl ( h_itmfgink , 'Record':U , h_itmfgink-2 ).
       RUN add-link IN adm-broker-hdl ( h_p-updclr , 'TableIO':U , h_itmfgink-2 ).

       /* Links to SmartNavBrowser h_itmfgink. */
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'Record':U , h_itmfgink ).

       /* Links to SmartNavBrowser h_b-ebfg. */
       RUN add-link IN adm-broker-hdl ( h_itmfgink-2 , 'ebfg':U , h_b-ebfg ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'Record':U , h_b-ebfg ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_itemfg-3 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_itmfgink-2 ,
             h_itemfg-3 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_itmfgink ,
             h_itmfgink-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updclr ,
             h_itmfgink , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-ebfg ,
             h_p-updclr , 'AFTER':U ).
    END. /* Page 6 */
    WHEN 7 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/b-eitem.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-eitem ).
       RUN set-position IN h_b-eitem ( 2.67 , 2.00 ) NO-ERROR.
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
       RUN set-position IN h_v-eitem ( 2.67 , 59.00 ) NO-ERROR.
       /* Size in UIB:  ( 17.62 , 99.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updncp.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updncp ).
       RUN set-position IN h_p-updncp ( 22.52 , 69.00 ) NO-ERROR.
       RUN set-size IN h_p-updncp ( 2.14 , 58.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/pricechg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_pricechg ).
       RUN set-position IN h_pricechg ( 22.52 , 129.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.05 , 17.20 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartNavBrowser h_b-eitem. */
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'Record':U , h_b-eitem ).

       /* Links to SmartViewer h_v-eitem. */
       RUN add-link IN adm-broker-hdl ( h_b-eitem , 'Record':U , h_v-eitem ).
       RUN add-link IN adm-broker-hdl ( h_p-updncp , 'TableIO':U , h_v-eitem ).

       /* Links to SmartViewer h_pricechg. */
       RUN add-link IN adm-broker-hdl ( h_v-eitem , 'price-change':U , h_pricechg ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-eitem ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-eitem ,
             h_b-eitem , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updncp ,
             h_v-eitem , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_pricechg ,
             h_p-updncp , 'AFTER':U ).
    END. /* Page 7 */
    WHEN 8 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oeinq/b-ordfgi.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-ordfgi ).
       RUN set-position IN h_b-ordfgi ( 2.43 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 20.00 , 156.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartNavBrowser h_b-ordfgi. */
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'Record':U , h_b-ordfgi ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-ordfgi ,
             h_folder , 'AFTER':U ).
    END. /* Page 8 */
    WHEN 9 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/v-fgimg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-fgimg ).
       RUN set-position IN h_v-fgimg ( 2.67 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 17.14 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-boxupd.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-boxupd ).
       RUN set-position IN h_p-boxupd ( 20.29 , 38.00 ) NO-ERROR.
       RUN set-size IN h_p-boxupd ( 1.76 , 31.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/pv-graph.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_pv-graph ).
       RUN set-position IN h_pv-graph ( 20.52 , 102.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.76 , 17.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_v-fgimg. */
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'Record':U , h_v-fgimg ).
       RUN add-link IN adm-broker-hdl ( h_p-boxupd , 'TableIO':U , h_v-fgimg ).
       RUN add-link IN adm-broker-hdl ( h_pv-graph , 'graph':U , h_v-fgimg ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-fgimg ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-boxupd ,
             h_v-fgimg , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_pv-graph ,
             h_p-boxupd , 'AFTER':U ).
    END. /* Page 9 */
    WHEN 13 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/fgijob.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_fgijob-2 ).
       RUN set-position IN h_fgijob-2 ( 2.43 , 2.00 ) NO-ERROR.
       RUN set-size IN h_fgijob-2 ( 21.19 , 157.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/p-fg-bj.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = ?':U ,
             OUTPUT h_p-fg-bj ).
       RUN set-position IN h_p-fg-bj ( 26.95 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.14 , 65.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/p-calcq.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_p-calcq ).
       RUN set-position IN h_p-calcq ( 27.43 , 131.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.52 , 26.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('14,4':U) NO-ERROR.

       /* Links to SmartBrowser h_fgijob-2. */
       RUN add-link IN adm-broker-hdl ( h_fgijob , 'Record':U , h_fgijob-2 ).
       RUN add-link IN adm-broker-hdl ( h_fgijob-2 , 'viewDetail':U , THIS-PROCEDURE ).

       /* Links to SmartViewer h_p-fg-bj. */
       RUN add-link IN adm-broker-hdl ( h_fgijob-2 , 'cost':U , h_p-fg-bj ).

       /* Links to SmartViewer h_p-calcq. */
       RUN add-link IN adm-broker-hdl ( h_locw , 'calc-qty':U , h_p-calcq ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_fgijob-2 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-fg-bj ,
             h_fgijob-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-calcq ,
             h_p-fg-bj , 'AFTER':U ).
    END. /* Page 13 */
    WHEN 14 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/fgijobw.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = yes,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = ?':U ,
             OUTPUT h_fgijobw ).
       RUN set-position IN h_fgijobw ( 3.62 , 37.00 ) NO-ERROR.
       RUN set-size IN h_fgijobw ( 5.24 , 119.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/fgijob.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_fgijob ).
       RUN set-position IN h_fgijob ( 3.62 , 37.20 ) NO-ERROR.
       /* Size in UIB:  ( 5.00 , 118.40 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('3,1':U) NO-ERROR.

       /* Links to SmartBrowser h_fgijobw. */
       RUN add-link IN adm-broker-hdl ( h_itemfg-2 , 'Record':U , h_fgijobw ).

       /* Links to SmartViewer h_fgijob. */
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'Record':U , h_fgijob ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_fgijobw ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_fgijob ,
             h_fgijobw , 'AFTER':U ).
    END. /* Page 14 */

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
  {src/adm/template/row-list.i "oe-ordl"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-ordl"}

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
  &Scoped-define ACCESSTYPE create
  {methods/template/security.i}

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
  &Scoped-define ACCESSTYPE delete
   {methods/template/security.i}
  
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
&Scoped-define ACCESSTYPE update
  {methods/template/security.i}

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
  VIEW W-Win.
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
RUN filterZeroBins IN h_fgijob-2 (INPUT iplShowZeroBins).
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



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hideVendorCost W-Win
PROCEDURE hideVendorCost:
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
------------------------------------------------------------------------------*/
    RUN SELECT-page (li-page-b4VendCost).

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-change-page W-Win
PROCEDURE local-change-page:
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
------------------------------------------------------------------------------*/


    /* Code placed here will execute PRIOR to standard behavior. */
    li-prev-page = li-current-page.
    run get-attribute ("current-page").
    assign 
        li-current-page = int(return-value).      
 if li-current-page = 7 AND lNewVendorItemCost then 
    do:
        RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCostSourceFrom = "OF"' ).
        RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCost = ' + quoter(oe-ordl.i-no) ).      
        RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCostType = "FG" '  ).
        RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCostCustomer = ' + quoter(oe-ordl.cust-no)  ).
        IF oe-ordl.est-no NE "" THEN 
            RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCostEst# = ' + quoter(oe-ordl.est-no)).       
        li-page-b4VendCost = li-prev-page.     
        RUN select-page (15).               
        RETURN.     
    END.
    
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'change-page':U ) .

  /* Code placed here will execute AFTER standard behavior.    */



END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-objects W-Win 
PROCEDURE local-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE v-current-page AS INTEGER NO-UNDO.
  DEFINE VARIABLE h_custpart AS HANDLE NO-UNDO.
  DEFINE VARIABLE h_p-updsav-3 AS HANDLE NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  v-current-page = INTEGER(RETURN-VALUE).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-objects':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF v-current-page EQ 3 AND
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
      RUN set-position IN h_custpart ( 5.05 , 105.00 ) NO-ERROR.
      RUN set-size IN h_custpart ( 15.71 , 47.00 ) NO-ERROR.

      RUN init-object IN THIS-PROCEDURE (
            INPUT  'adm/objects/p-updsav.r':U ,
            INPUT  FRAME F-Main:HANDLE ,
            INPUT  'Edge-Pixels = 2,
                    SmartPanelType = Update,
                    AddFunction = One-Record':U ,
            OUTPUT h_p-updsav-3 ).
      RUN set-position IN h_p-updsav-3 ( 21.00 , 105.00 ) NO-ERROR.
      RUN set-size IN h_p-updsav-3 ( 1.76 , 47.00 ) NO-ERROR.

      /* Links to SmartBrowser h_custpart. */
      RUN add-link IN adm-broker-hdl ( h_itemfg , 'Record':U , h_custpart ).
      RUN add-link IN adm-broker-hdl ( h_p-updsav-3 , 'TableIO':U , h_custpart ).
      RUN view-page (3).
  END.

    IF v-current-page = 15 THEN /* new vendor cost tab */
    DO:
        RUN init-object IN THIS-PROCEDURE (
            INPUT  'windows/vendcostmtx.w':U ,
            INPUT  {&WINDOW-NAME} ,
            INPUT  'Layout = ':U ,
            OUTPUT h_vendcostmtx ).
        /* Position in AB:  ( 5.91 , 7.60 ) */
        /* Size in UIB:  ( 1.86 , 10.80 ) */
    
        /* Initialize other pages that this page requires. */
        RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.
        RUN set-position IN h_vendcostmtx ( 5.91 , 7.60 ) NO-ERROR. 
        /* Links to SmartWindow */
        /*    RUN add-link IN adm-broker-hdl ( h_b-ordlt , 'Record':U , h_vendcostmtx ).    */
        RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'VendCost':U , h_vendcostmtx ).
    
    /* Adjust the tab order of the smart objects. */
    END. /* Page 10 */
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

   RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCostSourceFrom = ""' ).
   RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCostEst# =""').
   RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCost = "" ').
   RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCostType = "" ' ).
   RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCostCustomer = "" ' ).
   RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCostForm# = "" ' ).
   RUN set-attribute-list IN adm-broker-hdl ('OneVendItemCostBlank# = "" ' ). 
       
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
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF access-close THEN DO:  /* YSK  not leave window on after closed */
      APPLY 'CLOSE' TO THIS-PROCEDURE.
      RETURN.
  END.

  DEFINE VARIABLE IsASet AS LOGICAL NO-UNDO.
  
  RUN IsASet IN h_itemfg (OUTPUT IsASet).
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE, "PAGE-SOURCE", OUTPUT char-hdl).
  IF IsASet THEN RUN enable-folder-page IN WIDGET-HANDLE(char-hdl) (5) NO-ERROR.
  ELSE RUN disable-folder-page IN WIDGET-HANDLE(char-hdl) (5) NO-ERROR.
  
  APPLY "ENTRY" TO FRAME {&FRAME-NAME}.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mf-message W-Win 
PROCEDURE mf-message :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER ip-misc-flds AS LOGICAL NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE notes-message W-Win 
PROCEDURE notes-message :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ip-notes AS LOGICAL NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pViewDetail W-Win 
PROCEDURE pViewDetail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiPage AS INTEGER NO-UNDO.
    
    /* two smart objects in windows/itemfg.w are shared in this window container
       however, itemfg.w page 5 objects are located on page 4 here, and page 14 
       objects are located on page 13 here, so switch */
    IF ipiPage EQ 5  THEN ipiPage = 4.
    IF ipiPage EQ 14 THEN ipiPage = 13.
    
    RUN select-page IN THIS-PROCEDURE (ipiPage).

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
  {src/adm/template/snd-list.i "oe-ordl"}

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
    
    IF VALID-HANDLE(h_fgijob) THEN
    RUN set-pass-loc IN h_fgijob (INPUT ip-loc).

    IF VALID-HANDLE(h_fgijobw) THEN
    RUN set-pass-loc IN h_fgijobw (INPUT ip-loc).

    IF VALID-HANDLE(h_fgijob-2) THEN
    RUN set-pass-loc IN h_fgijob-2 (INPUT ip-loc).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-rec-key_header W-Win 
PROCEDURE set-rec-key_header :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER ip-rec_key AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ip-header AS CHARACTER NO-UNDO.

  ASSIGN
    rec_key_value = ip-rec_key
    header_value = ip-header.

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

