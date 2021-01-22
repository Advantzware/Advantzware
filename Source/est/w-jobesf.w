&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File:   copied from est/w-estc.w 

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
/*{custom/globdefs.i}*/
{methods/defines/hndldefs.i}
/*{methods/prgsecur.i}*/
{custom/globdefs.i}
DEFINE VARIABLE rec_key_value AS CHARACTER NO-UNDO.
DEFINE VARIABLE header_value AS CHARACTER NO-UNDO.

def var li-page as int extent 2 no-undo.

DEFINE VARIABLE efbrowse AS CHARACTER NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES job
&Scoped-define FIRST-EXTERNAL-TABLE job


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR job.
/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-estitm AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-estop AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-estprp AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-box23d AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-boxupd AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-dieimg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-estop AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-estprp AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-fest1 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-inkpak AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-layouf AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-probe AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-rfqsiz AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updc&c AS HANDLE NO-UNDO.
DEFINE VARIABLE h_probe AS HANDLE NO-UNDO.
DEFINE VARIABLE h_pv-grap2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-boxdes AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-jobest AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-boxdee AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-est AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-est2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-est3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-est4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-naveb AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-naveb-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-navef AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-est AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-est-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-est-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-est-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-est-5 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vp-est AS HANDLE NO-UNDO.
DEFINE VARIABLE h_w-qtest AS HANDLE NO-UNDO.
DEFINE VARIABLE sh_v-naveb-3 AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160.2 BY 23.76.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: asi.job
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 7
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Estimate - Folding"
         HEIGHT             = 23.76
         WIDTH              = 157.2
         MAX-HEIGHT         = 24.81
         MAX-WIDTH          = 157.2
         VIRTUAL-HEIGHT     = 24.81
         VIRTUAL-WIDTH      = 157.2
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
ON END-ERROR OF W-Win /* Estimate - Folding */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Estimate - Folding */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */

  def var char-hdl as cha no-undo.
  run get-link-handle in adm-broker-hdl(this-procedure,"estimate-source", output char-hdl).
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
             INPUT  'FOLDER-LABELS = ':U + 'Browse|Detail|Specs|Layout|Inks/Pack|Prep/Rout|Misc/Farm|Design|Print|Quote' + ',
                     FOLDER-TAB-TYPE = 2':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 1.00 , 1.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 23.57 , 159.80 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
           &IF DEFINED(UIB_is_Running) ne 0 &THEN
             INPUT  'ce/b-estitm.w':U ,
           &ELSE
             INPUT efbrowse ,
           &ENDIF
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-estitm ).
       RUN set-position IN h_b-estitm ( 3.14 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 17.62 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-fest1.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-fest1 ).
       RUN set-position IN h_p-fest1 ( 22.19 , 49.00 ) NO-ERROR.
       RUN set-size IN h_p-fest1 ( 1.91 , 52.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/vp-est.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vp-est ).
       RUN set-position IN h_vp-est ( 22.19 , 102.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 35.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'jc/q-jobest.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-jobest ).
       RUN set-position IN h_q-jobest ( 21.71 , 8.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.05 , 11.60 ) */

       /* Links to SmartNavBrowser h_b-estitm. */
       RUN add-link IN adm-broker-hdl ( h_p-fest1 , 'TableIO':U , h_b-estitm ).
       RUN add-link IN adm-broker-hdl ( h_q-jobest , 'Record':U , h_b-estitm ).

       /* Links to SmartViewer h_vp-est. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_vp-est ).

       /* Links to SmartQuery h_q-jobest. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'Record':U , h_q-jobest ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-estitm ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-fest1 ,
             h_b-estitm , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_vp-est ,
             h_p-fest1 , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'ce/v-est.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-est ).
       RUN set-position IN h_v-est ( 2.91 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 16.91 , 156.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-rfqsiz.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-rfqsiz ).
       RUN set-position IN h_p-rfqsiz ( 20.52 , 71.00 ) NO-ERROR.
       RUN set-size IN h_p-rfqsiz ( 2.00 , 52.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-naveb.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-naveb-2 ).
       RUN set-position IN h_v-naveb-2 ( 21.00 , 15.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 42.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartViewer h_v-est. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_v-est ).
       RUN add-link IN adm-broker-hdl ( h_p-rfqsiz , 'TableIO':U , h_v-est ).

       /* Links to SmartViewer h_v-naveb-2. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'nav-itm':U , h_v-naveb-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-est ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-rfqsiz ,
             h_v-est , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-naveb-2 ,
             h_p-rfqsiz , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/vi-est-.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-est ).
       RUN set-position IN h_vi-est ( 2.67 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 146.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'ce/v-est2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-est2 ).
       RUN set-position IN h_v-est2 ( 4.81 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 15.95 , 147.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-navef.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-navef ).
       RUN set-position IN h_v-navef ( 21.24 , 11.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 42.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-layouf.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-layouf ).
       RUN set-position IN h_p-layouf ( 21.24 , 60.00 ) NO-ERROR.
       RUN set-size IN h_p-layouf ( 1.76 , 87.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartViewer h_vi-est. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_vi-est ).

       /* Links to SmartViewer h_v-est2. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_v-est2 ).
       RUN add-link IN adm-broker-hdl ( h_p-layouf , 'TableIO':U , h_v-est2 ).

       /* Links to SmartViewer h_v-navef. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'nav-itm':U , h_v-navef ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_vi-est ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-est2 ,
             h_vi-est , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-navef ,
             h_v-est2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-layouf ,
             h_v-navef , 'AFTER':U ).
    END. /* Page 4 */
    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/vi-est-.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-est-2 ).
       RUN set-position IN h_vi-est-2 ( 2.67 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 146.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'ce/v-est3.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-est3 ).
       RUN set-position IN h_v-est3 ( 5.05 , 1.80 ) NO-ERROR.
       /* Size in UIB:  ( 16.19 , 155.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-inkpak.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-inkpak ).
       RUN set-position IN h_p-inkpak ( 21.95 , 60.00 ) NO-ERROR.
       RUN set-size IN h_p-inkpak ( 1.76 , 89.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-naveb.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT sh_v-naveb-3 ).
       RUN set-position IN sh_v-naveb-3 ( 22.19 , 11.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 42.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartViewer h_vi-est-2. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_vi-est-2 ).

       /* Links to SmartViewer h_v-est3. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_v-est3 ).
       RUN add-link IN adm-broker-hdl ( h_p-inkpak , 'TableIO':U , h_v-est3 ).

       /* Links to SmartViewer sh_v-naveb-3. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'nav-itm':U , sh_v-naveb-3 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_vi-est-2 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-est3 ,
             h_vi-est-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-inkpak ,
             h_v-est3 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( sh_v-naveb-3 ,
             h_p-inkpak , 'AFTER':U ).
    END. /* Page 5 */
    WHEN 6 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/vi-est-.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-est-3 ).
       RUN set-position IN h_vi-est-3 ( 2.67 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 146.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/b-estprp.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-estprp ).
       RUN set-position IN h_b-estprp ( 5.05 , 5.00 ) NO-ERROR.
       RUN set-size IN h_b-estprp ( 7.86 , 123.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-estprp.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-estprp ).
       RUN set-position IN h_p-estprp ( 5.29 , 130.00 ) NO-ERROR.
       RUN set-size IN h_p-estprp ( 7.62 , 15.80 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'ce/b-estop.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-estop ).
       RUN set-position IN h_b-estop ( 13.86 , 5.00 ) NO-ERROR.
       RUN set-size IN h_b-estop ( 8.24 , 128.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-estop.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-estop ).
       RUN set-position IN h_p-estop ( 13.86 , 135.00 ) NO-ERROR.
       RUN set-size IN h_p-estop ( 7.38 , 13.80 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartViewer h_vi-est-3. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_vi-est-3 ).

       /* Links to SmartBrowser h_b-estprp. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_b-estprp ).
       RUN add-link IN adm-broker-hdl ( h_p-estprp , 'TableIO':U , h_b-estprp ).
       RUN add-link IN adm-broker-hdl ( h_b-estprp , 'buttons':U , h_p-estprp ).
       RUN add-link IN adm-broker-hdl ( h_b-estprp  , 'Record':U , h_p-estprp ).

       /* Links to SmartBrowser h_b-estop. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_b-estop ).
       RUN add-link IN adm-broker-hdl ( h_p-estop , 'TableIO':U , h_b-estop ).
       RUN add-link IN adm-broker-hdl ( h_b-estop , 'buttons':U , h_p-estop ).
       RUN add-link IN adm-broker-hdl ( h_b-estop , 'Record':U , h_p-estop ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_vi-est-3 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-estprp ,
             h_vi-est-3 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-estprp ,
             h_b-estprp , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-estop ,
             h_p-estprp , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-estop ,
             h_b-estop , 'AFTER':U ).
    END. /* Page 6 */
    WHEN 7 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/vi-est-.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-est-4 ).
       RUN set-position IN h_vi-est-4 ( 2.91 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 146.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-est4.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-est4 ).
       RUN set-position IN h_v-est4 ( 5.52 , 5.00 ) NO-ERROR.
       /* Size in UIB:  ( 17.05 , 110.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updc&c.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updc&c ).
       RUN set-position IN h_p-updc&c ( 7.67 , 119.00 ) NO-ERROR.
       RUN set-size IN h_p-updc&c ( 11.91 , 25.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartViewer h_vi-est-4. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_vi-est-4 ).

       /* Links to SmartViewer h_v-est4. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_v-est4 ).
       RUN add-link IN adm-broker-hdl ( h_p-updc&c , 'TableIO':U , h_v-est4 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_vi-est-4 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-est4 ,
             h_vi-est-4 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updc&c ,
             h_v-est4 , 'AFTER':U ).
    END. /* Page 7 */
    WHEN 8 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'ce/v-boxdee.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-boxdee ).
       RUN set-position IN h_v-boxdee ( 3.38 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 16.43 , 146.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-naveb.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-naveb ).
       RUN set-position IN h_v-naveb ( 20.52 , 5.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 42.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-boxupd.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-boxupd ).
       RUN set-position IN h_p-boxupd ( 20.52 , 65.00 ) NO-ERROR.
       RUN set-size IN h_p-boxupd ( 1.43 , 31.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-dieimg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_p-dieimg ).
       RUN set-position IN h_p-dieimg ( 20.52 , 96.00 ) NO-ERROR.
       RUN set-size IN h_p-dieimg ( 1.43 , 14.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-box23d.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-box23d ).
       RUN set-position IN h_p-box23d ( 20.52 , 110.00 ) NO-ERROR.
       RUN set-size IN h_p-box23d ( 1.43 , 26.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/pv-grap2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_pv-grap2 ).
       RUN set-position IN h_pv-grap2 ( 20.52 , 136.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.38 , 13.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/q-boxdes.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-boxdes ).
       RUN set-position IN h_q-boxdes ( 21.95 , 39.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.05 , 6.80 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartViewer h_v-boxdee. */
       RUN add-link IN adm-broker-hdl ( h_p-box23d , 'TableIO':U , h_v-boxdee ).
       RUN add-link IN adm-broker-hdl ( h_p-boxupd , 'TableIO':U , h_v-boxdee ).
       RUN add-link IN adm-broker-hdl ( h_p-dieimg , 'TableIO':U , h_v-boxdee ).
       RUN add-link IN adm-broker-hdl ( h_pv-grap2 , 'graph':U , h_v-boxdee ).
       RUN add-link IN adm-broker-hdl ( h_q-boxdes , 'Record':U , h_v-boxdee ).

       /* Links to SmartViewer h_v-naveb. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'nav-itm':U , h_v-naveb ).

       /* Links to SmartQuery h_q-boxdes. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_q-boxdes ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-boxdee ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-naveb ,
             h_v-boxdee , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-boxupd ,
             h_v-naveb , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-dieimg ,
             h_p-boxupd , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-box23d ,
             h_p-dieimg , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_pv-grap2 ,
             h_p-box23d , 'AFTER':U ).
    END. /* Page 8 */
    WHEN 9 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/vi-est-.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-est-5 ).
       RUN set-position IN h_vi-est-5 ( 2.91 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 146.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'ce/probe.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_probe ).
       RUN set-position IN h_probe ( 5.29 , 6.00 ) NO-ERROR.
       RUN set-size IN h_probe ( 13.10 , 143.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-probe.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-probe ).
       RUN set-position IN h_p-probe ( 19.81 , 20.00 ) NO-ERROR.
       RUN set-size IN h_p-probe ( 2.38 , 118.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartViewer h_vi-est-5. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_vi-est-5 ).

       /* Links to SmartBrowser h_probe. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_probe ).
       RUN add-link IN adm-broker-hdl ( h_p-probe , 'TableIO':U , h_probe ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_vi-est-5 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_probe ,
             h_vi-est-5 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-probe ,
             h_probe , 'AFTER':U ).
    END. /* Page 9 */
    WHEN 10 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/w-qtest.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_w-qtest ).
       /* Position in AB:  ( 3.57 , 7.00 ) */
       /* Size in UIB:  ( 2.05 , 11.60 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartWindow h_w-qtest. */
       RUN add-link IN adm-broker-hdl ( h_q-jobest , 'Record':U , h_w-qtest ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'quote':U , h_w-qtest ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 10 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 2 ).

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
  {src/adm/template/row-list.i "job"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "job"}

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
/*  &Scoped-define ACCESSTYPE create
  {methods/template/security.i}
*/
 
  def output param op-flag as log no-undo.
  op-flag = YES.
 
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
/* &Scoped-define ACCESSTYPE delete
  {methods/template/security.i}
*/
def output param op-flag as log no-undo.
  op-flag = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Allow-Update W-Win 
PROCEDURE Allow-Update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*  program master record error and entry error with w-order.w
  &Scoped-define ACCESSTYPE update
  {methods/template/security.i}
*/
  def output param op-flag as log no-undo.
  op-flag = YES.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-enable-farm W-Win 
PROCEDURE disable-enable-farm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {est/farmTab.i}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hide-quote W-Win 
PROCEDURE hide-quote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  run select-page (li-page[2]).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-box-design W-Win 
PROCEDURE init-box-design :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-handle AS HANDLE NO-UNDO.


  RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/q-boxdes.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-boxdes ).

  RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_q-boxdes ).
  RUN add-link IN adm-broker-hdl ( ip-handle, 'box-calc':U , h_q-boxdes ).
  RUN dispatch IN h_q-boxdes ('initialize').
  
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
  run get-attribute ("current-page").
   
  assign
   li-page[2] = li-page[1]
   li-page[1] = int(return-value).
  
  if li-page[1] = 10 then do:  /* quote */
    def buffer bf-quote for quotehd .
    find first bf-quote where bf-quote.company = g_company and
                            bf-quote.loc = g_loc and
                            bf-quote.est-no = oe-ordl.est-no
                            no-lock no-error.
    if not avail bf-quote then do:
       message "SORRY, NO QUOTE EXISTS FOR THIS ESTIMATE." SKIP
               "YOU MUST CREATE QUOTE VIA THE PRINT FOLDER."
               view-as alert-box error.      
       run hide-quote.
       return no-apply.        
    end.                            
  end.
   
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

  /* Code placed here will execute PRIOR to standard behavior. */
  FIND FIRST sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ g_company
                                AND sys-ctrl.name    EQ 'EFBROWSE' NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN sys-ctrl.company  = g_company
           sys-ctrl.name     = 'EFBROWSE'
           sys-ctrl.descrip  = 'Estimate Browser Used'
           sys-ctrl.char-fld = 'Std'
           sys-ctrl.log-fld  = NO.
  END.
  CASE sys-ctrl.char-fld:
    WHEN 'Std' THEN
    efbrowse = 'ce/b-estitm.w'.
    WHEN 'Std & Paper1/2' THEN
    efbrowse = 'ce/b-estitm1.w'.
  END CASE.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-objects':U ) .

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
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mf-message W-Win 
PROCEDURE Mf-message :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER ip-misc-flds AS LOGICAL NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Notes-Message W-Win 
PROCEDURE Notes-Message :
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
  {src/adm/template/snd-list.i "job"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Rec-Key_header W-Win 
PROCEDURE Set-Rec-Key_header :
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

