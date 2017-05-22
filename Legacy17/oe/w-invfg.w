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
{methods/defines/globdefs.i }

DEF VAR ll-secure AS LOG INIT NO NO-UNDO.
DEFINE VARIABLE rec_key_value AS CHARACTER NO-UNDO.
DEFINE VARIABLE header_value AS CHARACTER NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES inv-line
&Scoped-define FIRST-EXTERNAL-TABLE inv-line


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR inv-line.
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
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemfg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemfg-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemfg-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemfg2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemfg2-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemfgt AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itmfgink AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itmfgink-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-calcc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-calcq AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-fg-bj AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-fgset AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-overq AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updclr AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updimg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updinv AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updinv-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-upditm AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updven AS HANDLE NO-UNDO.
DEFINE VARIABLE h_pricechg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_pv-graph AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-invlfg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-eitem AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-fgimg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-spcard AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 157.4 BY 22.38.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: ASI.inv-line
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
         HEIGHT             = 22.38
         WIDTH              = 157.4
         MAX-HEIGHT         = 24.14
         MAX-WIDTH          = 172
         VIRTUAL-HEIGHT     = 24.14
         VIRTUAL-WIDTH      = 172
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
             INPUT  'FOLDER-LABELS = ':U + 'View Item|Inventory|Totals CP#|Bin/Jobs|Set parts|Colors|Vend Cost|History|Image' + ',
                     FOLDER-TAB-TYPE = 2':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 1.00 , 1.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 22.38 , 157.00 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/itemfg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itemfg ).
       RUN set-position IN h_itemfg ( 2.67 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 17.14 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/p-upditm.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-upditm ).
       RUN set-position IN h_p-upditm ( 20.52 , 30.00 ) NO-ERROR.
       RUN set-size IN h_p-upditm ( 2.14 , 82.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/p-calcc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_p-calcc ).
       RUN set-position IN h_p-calcc ( 20.52 , 113.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.14 , 17.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/v-spcard.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-spcard ).
       RUN set-position IN h_v-spcard ( 20.52 , 130.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.14 , 15.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/q-invlfg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-invlfg ).
       RUN set-position IN h_q-invlfg ( 3.62 , 8.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.86 , 10.80 ) */

       /* Links to SmartViewer h_itemfg. */
       RUN add-link IN adm-broker-hdl ( h_p-upditm , 'TableIO':U , h_itemfg ).
       RUN add-link IN adm-broker-hdl ( h_q-invlfg , 'Record':U , h_itemfg ).
       RUN add-link IN adm-broker-hdl ( h_v-spcard , 'spec-card':U , h_itemfg ).

       /* Links to SmartViewer h_p-calcc. */
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'calc-cost':U , h_p-calcc ).

       /* Links to SmartQuery h_q-invlfg. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'Record':U , h_q-invlfg ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_itemfg ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-upditm ,
             h_itemfg , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-calcc ,
             h_p-upditm , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-spcard ,
             h_p-calcc , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/itemfg2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itemfg2 ).
       RUN set-position IN h_itemfg2 ( 2.91 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 14.52 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/p-updinv.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updinv ).
       RUN set-position IN h_p-updinv ( 17.67 , 52.00 ) NO-ERROR.
       RUN set-size IN h_p-updinv ( 2.14 , 31.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/p-calcq.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_p-calcq ).
       RUN set-position IN h_p-calcq ( 17.67 , 85.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.14 , 17.20 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/p-overq.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_p-overq ).
       RUN set-position IN h_p-overq ( 17.67 , 103.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.14 , 17.20 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_itemfg2. */
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'Record':U , h_itemfg2 ).
       RUN add-link IN adm-broker-hdl ( h_p-updinv , 'TableIO':U , h_itemfg2 ).

       /* Links to SmartViewer h_p-calcq. */
       RUN add-link IN adm-broker-hdl ( h_itemfg2 , 'calc-qty':U , h_p-calcq ).

       /* Links to SmartViewer h_p-overq. */
       RUN add-link IN adm-broker-hdl ( h_itemfg2 , 'override-q':U , h_p-overq ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_itemfg2 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updinv ,
             h_itemfg2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-calcq ,
             h_p-updinv , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-overq ,
             h_p-calcq , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewerid/itemfg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itemfg-2 ).
       RUN set-position IN h_itemfg-2 ( 2.67 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 155.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/itemfgt.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itemfgt ).
       RUN set-position IN h_itemfgt ( 5.05 , 17.00 ) NO-ERROR.
       /* Size in UIB:  ( 12.86 , 99.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/p-updinv.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updinv-2 ).
       RUN set-position IN h_p-updinv-2 ( 19.86 , 49.00 ) NO-ERROR.
       RUN set-size IN h_p-updinv-2 ( 2.14 , 32.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_itemfg-2. */
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'Record':U , h_itemfg-2 ).

       /* Links to SmartViewer h_itemfgt. */
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'Record':U , h_itemfgt ).
       RUN add-link IN adm-broker-hdl ( h_p-updinv-2 , 'TableIO':U , h_itemfgt ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_itemfg-2 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_itemfgt ,
             h_itemfg-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updinv-2 ,
             h_itemfgt , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/fgijob.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_fgijob ).
       RUN set-position IN h_fgijob ( 3.62 , 16.00 ) NO-ERROR.
       /* Size in UIB:  ( 5.00 , 118.40 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/fgijob.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_fgijob-2 ).
       RUN set-position IN h_fgijob-2 ( 9.33 , 3.00 ) NO-ERROR.
       RUN set-size IN h_fgijob-2 ( 7.38 , 144.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/p-fg-bj.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = ?':U ,
             OUTPUT h_p-fg-bj ).
       RUN set-position IN h_p-fg-bj ( 18.14 , 49.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.14 , 65.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_fgijob. */
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'Record':U , h_fgijob ).

       /* Links to SmartBrowser h_fgijob-2. */
       RUN add-link IN adm-broker-hdl ( h_fgijob , 'Record':U , h_fgijob-2 ).

       /* Links to SmartViewer h_p-fg-bj. */
       RUN add-link IN adm-broker-hdl ( h_fgijob-2 , 'cost':U , h_p-fg-bj ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_fgijob ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_fgijob-2 ,
             h_fgijob , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-fg-bj ,
             h_fgijob-2 , 'AFTER':U ).
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
       RUN set-position IN h_fg-set ( 7.19 , 21.00 ) NO-ERROR.
       RUN set-size IN h_fg-set ( 8.62 , 102.00 ) NO-ERROR.

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
       RUN add-link IN adm-broker-hdl ( h_p-fgset , 'TableIO':U , h_itemfg2-2 ).

       /* Links to SmartBrowser h_fg-set. */
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'Record':U , h_fg-set ).

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
       RUN set-position IN h_itemfg-3 ( 2.43 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 155.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/itmfgink.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itmfgink ).
       RUN set-position IN h_itmfgink ( 4.33 , 2.00 ) NO-ERROR.
       RUN set-size IN h_itmfgink ( 17.62 , 93.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/itmfgink.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itmfgink-2 ).
       RUN set-position IN h_itmfgink-2 ( 4.33 , 95.00 ) NO-ERROR.
       /* Size in UIB:  ( 6.29 , 61.40 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/p-updclr.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updclr ).
       RUN set-position IN h_p-updclr ( 10.76 , 98.00 ) NO-ERROR.
       RUN set-size IN h_p-updclr ( 2.00 , 55.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/b-ebfg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-ebfg ).
       RUN set-position IN h_b-ebfg ( 13.14 , 96.00 ) NO-ERROR.
       RUN set-size IN h_b-ebfg ( 9.10 , 61.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_itemfg-3. */
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'Record':U , h_itemfg-3 ).

       /* Links to SmartNavBrowser h_itmfgink. */
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'Record':U , h_itmfgink ).

       /* Links to SmartViewer h_itmfgink-2. */
       RUN add-link IN adm-broker-hdl ( h_itmfgink , 'Record':U , h_itmfgink-2 ).
       RUN add-link IN adm-broker-hdl ( h_p-updclr , 'TableIO':U , h_itmfgink-2 ).

       /* Links to SmartNavBrowser h_b-ebfg. */
       RUN add-link IN adm-broker-hdl ( h_itmfgink-2 , 'ebfg':U , h_b-ebfg ).
       RUN add-link IN adm-broker-hdl ( h_q-invlfg , 'Record':U , h_b-ebfg ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_itemfg-3 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_itmfgink ,
             h_itemfg-3 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_itmfgink-2 ,
             h_itmfgink , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updclr ,
             h_itmfgink-2 , 'AFTER':U ).
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
             INPUT  'Layout = ':U ,
             OUTPUT h_v-eitem ).
       RUN set-position IN h_v-eitem ( 2.67 , 59.00 ) NO-ERROR.
       /* Size in UIB:  ( 17.62 , 99.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/p-updven.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updven ).
       RUN set-position IN h_p-updven ( 20.57 , 65.00 ) NO-ERROR.
       RUN set-size IN h_p-updven ( 2.38 , 54.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/pricechg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_pricechg ).
       RUN set-position IN h_pricechg ( 20.57 , 120.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.05 , 17.20 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartNavBrowser h_b-eitem. */
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'Record':U , h_b-eitem ).

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
             INPUT  'fg/p-updimg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updimg ).
       RUN set-position IN h_p-updimg ( 20.29 , 73.00 ) NO-ERROR.
       RUN set-size IN h_p-updimg ( 1.81 , 31.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/pv-graph.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_pv-graph ).
       RUN set-position IN h_pv-graph ( 20.29 , 104.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.76 , 17.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_v-fgimg. */
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'Record':U , h_v-fgimg ).
       RUN add-link IN adm-broker-hdl ( h_p-updimg , 'TableIO':U , h_v-fgimg ).
       RUN add-link IN adm-broker-hdl ( h_pv-graph , 'graph':U , h_v-fgimg ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-fgimg ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updimg ,
             h_v-fgimg , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_pv-graph ,
             h_p-updimg , 'AFTER':U ).
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
  {src/adm/template/row-list.i "inv-line"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "inv-line"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get-g_rec_key W-Win 
PROCEDURE Get-g_rec_key :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER op-rec_key AS CHARACTER NO-UNDO.

  IF g_rec_key NE "" THEN
  op-rec_key = ENTRY(NUM-ENTRIES(g_rec_key),g_rec_key).

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
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  apply "entry" to frame {&frame-name}.

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
  {src/adm/template/snd-list.i "inv-line"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

