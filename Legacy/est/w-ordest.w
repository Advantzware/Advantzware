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

{methods/defines/hndldefs.i}
{custom/globdefs.i}

DEFINE VARIABLE rec_key_value AS CHARACTER NO-UNDO.
DEFINE VARIABLE header_value AS CHARACTER NO-UNDO.


def var li-page as int extent 2 no-undo.

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
DEFINE VARIABLE h_b-eitem2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-estitm AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-estop AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-estprp AS HANDLE NO-UNDO.
DEFINE VARIABLE h_farmnav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-box23d AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-dieimg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-estbox AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-estc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-estop AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-estprp AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-inkpak AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-layouf AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-probe AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-rfqsiz AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updc&c AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updven AS HANDLE NO-UNDO.
DEFINE VARIABLE h_pricechg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_probe AS HANDLE NO-UNDO.
DEFINE VARIABLE h_pv-grap2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-boxdes AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-est3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-ordest AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-boxdee AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-eitem2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-est AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-est2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-est3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-est4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-naveb AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-navef AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-navest AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-navest-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-est AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-est-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-est-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-est-5 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-est2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-est3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vp-box AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vp-est AS HANDLE NO-UNDO.
DEFINE VARIABLE h_w-qtest AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 159.4 BY 23.76.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: ASI.oe-ordl
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 2
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Estimate"
         HEIGHT             = 23.76
         WIDTH              = 159.4
         MAX-HEIGHT         = 23.76
         MAX-WIDTH          = 159.4
         VIRTUAL-HEIGHT     = 23.76
         VIRTUAL-WIDTH      = 159.4
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
ON END-ERROR OF W-Win /* Estimate */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Estimate */
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
             INPUT  'FOLDER-LABELS = ':U + 'Brws Est|Estimate|Specs|Layout|Inks/Pack|Prep/Route|Misc/Sub|Box Design|Print|Quote|Farm' + ',
                     FOLDER-TAB-TYPE = 2':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 1.00 , 1.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 23.57 , 159.00 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'cec/b-estitm.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_b-estitm ).
       RUN set-position IN h_b-estitm ( 2.91 , 7.00 ) NO-ERROR.
       /* Size in UIB:  ( 16.43 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-estc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-estc ).
       RUN set-position IN h_p-estc ( 20.52 , 57.00 ) NO-ERROR.
       RUN set-size IN h_p-estc ( 1.91 , 53.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/vp-est.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vp-est ).
       RUN set-position IN h_vp-est ( 20.52 , 114.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 35.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-navest.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-navest ).
       RUN set-position IN h_v-navest ( 20.76 , 18.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 34.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'oe/q-ordest.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-ordest ).
       RUN set-position IN h_q-ordest ( 19.81 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.05 , 11.60 ) */

       /* Links to SmartNavBrowser h_b-estitm. */
       RUN add-link IN adm-broker-hdl ( h_p-estc , 'TableIO':U , h_b-estitm ).
       RUN add-link IN adm-broker-hdl ( h_q-ordest , 'Record':U , h_b-estitm ).

       /* Links to SmartViewer h_vp-est. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_vp-est ).
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'set-goto':U , h_vp-est ).

       /* Links to SmartViewer h_v-navest. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'next-prev':U , h_v-navest ).

       /* Links to SmartQuery h_q-ordest. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'Record':U , h_q-ordest ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-estitm ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-estc ,
             h_b-estitm , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_vp-est ,
             h_p-estc , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-navest ,
             h_vp-est , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'cec/v-est.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-est ).
       RUN set-position IN h_v-est ( 2.91 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 16.67 , 153.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-rfqsiz.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-rfqsiz ).
       RUN set-position IN h_p-rfqsiz ( 21.24 , 49.00 ) NO-ERROR.
       RUN set-size IN h_p-rfqsiz ( 1.76 , 52.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartViewer h_v-est. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_v-est ).
       RUN add-link IN adm-broker-hdl ( h_p-rfqsiz , 'TableIO':U , h_v-est ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-est ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-rfqsiz ,
             h_v-est , 'AFTER':U ).
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
             INPUT  'cec/v-est2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-est2 ).
       RUN set-position IN h_v-est2 ( 4.81 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 16.19 , 147.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-layouf.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-layouf ).
       RUN set-position IN h_p-layouf ( 21.48 , 25.00 ) NO-ERROR.
       RUN set-size IN h_p-layouf ( 1.67 , 118.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartViewer h_vi-est. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_vi-est ).

       /* Links to SmartViewer h_v-est2. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_v-est2 ).
       RUN add-link IN adm-broker-hdl ( h_p-layouf , 'TableIO':U , h_v-est2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_vi-est ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-est2 ,
             h_vi-est , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-layouf ,
             h_v-est2 , 'AFTER':U ).
    END. /* Page 4 */
    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/vi-est3.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-est3 ).
       RUN set-position IN h_vi-est3 ( 2.91 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 146.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'cec/v-est3.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-est3 ).
       RUN set-position IN h_v-est3 ( 4.81 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 15.71 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-inkpak.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-inkpak ).
       RUN set-position IN h_p-inkpak ( 21.71 , 42.00 ) NO-ERROR.
       RUN set-size IN h_p-inkpak ( 1.76 , 106.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-navest.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-navest-2 ).
       RUN set-position IN h_v-navest-2 ( 21.95 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 34.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('12':U) NO-ERROR.

       /* Links to SmartViewer h_vi-est3. */
       RUN add-link IN adm-broker-hdl ( h_q-est3 , 'Record':U , h_vi-est3 ).

       /* Links to SmartViewer h_v-est3. */
       RUN add-link IN adm-broker-hdl ( h_p-inkpak , 'TableIO':U , h_v-est3 ).
       RUN add-link IN adm-broker-hdl ( h_q-est3 , 'Record':U , h_v-est3 ).

       /* Links to SmartViewer h_v-navest-2. */
       RUN add-link IN adm-broker-hdl ( h_q-est3 , 'nav-itm':U , h_v-navest-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_vi-est3 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-est3 ,
             h_vi-est3 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-inkpak ,
             h_v-est3 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-navest-2 ,
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
             INPUT  'Layout = ':U ,
             OUTPUT h_p-estprp ).
       RUN set-position IN h_p-estprp ( 5.05 , 129.00 ) NO-ERROR.
       /* Size in UIB:  ( 5.76 , 14.20 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/b-estop.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-estop ).
       RUN set-position IN h_b-estop ( 13.86 , 4.00 ) NO-ERROR.
       RUN set-size IN h_b-estop ( 8.24 , 130.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-estop.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_p-estop ).
       RUN set-position IN h_p-estop ( 13.86 , 141.00 ) NO-ERROR.
       /* Size in UIB:  ( 7.38 , 14.20 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartViewer h_vi-est-3. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_vi-est-3 ).

       /* Links to SmartBrowser h_b-estprp. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_b-estprp ).
       RUN add-link IN adm-broker-hdl ( h_p-estprp , 'TableIO':U , h_b-estprp ).

       /* Links to SmartViewer h_p-estprp. */
       RUN add-link IN adm-broker-hdl ( h_b-estprp , 'buttons':U , h_p-estprp ).
       RUN add-link IN adm-broker-hdl ( h_b-estprp , 'Record':U , h_p-estprp ).

       /* Links to SmartBrowser h_b-estop. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_b-estop ).
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'route':U , h_b-estop ).
       RUN add-link IN adm-broker-hdl ( h_p-estop , 'TableIO':U , h_b-estop ).

       /* Links to SmartViewer h_p-estop. */
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
       RUN set-position IN h_v-est4 ( 5.05 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 17.05 , 114.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updc&c.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updc&c ).
       RUN set-position IN h_p-updc&c ( 5.76 , 125.00 ) NO-ERROR.
       RUN set-size IN h_p-updc&c ( 11.67 , 24.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-navef.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-navef ).
       RUN set-position IN h_v-navef ( 22.43 , 116.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 42.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartViewer h_vi-est-4. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_vi-est-4 ).

       /* Links to SmartViewer h_v-est4. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_v-est4 ).
       RUN add-link IN adm-broker-hdl ( h_p-updc&c , 'TableIO':U , h_v-est4 ).

       /* Links to SmartViewer h_v-navef. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'nav-itm':U , h_v-navef ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_vi-est-4 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-est4 ,
             h_vi-est-4 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updc&c ,
             h_v-est4 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-navef ,
             h_p-updc&c , 'AFTER':U ).
    END. /* Page 7 */
    WHEN 8 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'cec/v-boxdee.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-boxdee ).
       RUN set-position IN h_v-boxdee ( 3.14 , 5.00 ) NO-ERROR.
       /* Size in UIB:  ( 16.67 , 149.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-naveb.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-naveb ).
       RUN set-position IN h_v-naveb ( 20.52 , 5.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 42.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-estbox.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-estbox ).
       RUN set-position IN h_p-estbox ( 20.52 , 53.00 ) NO-ERROR.
       RUN set-size IN h_p-estbox ( 1.43 , 44.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-dieimg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_p-dieimg ).
       RUN set-position IN h_p-dieimg ( 20.52 , 99.00 ) NO-ERROR.
       RUN set-size IN h_p-dieimg ( 1.43 , 13.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-box23d.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-box23d ).
       RUN set-position IN h_p-box23d ( 20.52 , 112.00 ) NO-ERROR.
       RUN set-size IN h_p-box23d ( 1.43 , 28.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/pv-grap2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_pv-grap2 ).
       RUN set-position IN h_pv-grap2 ( 20.52 , 140.00 ) NO-ERROR.
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
       RUN add-link IN adm-broker-hdl ( h_p-dieimg , 'TableIO':U , h_v-boxdee ).
       RUN add-link IN adm-broker-hdl ( h_p-estbox , 'TableIO':U , h_v-boxdee ).
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
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-estbox ,
             h_v-naveb , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-dieimg ,
             h_p-estbox , 'AFTER':U ).
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
             INPUT  'browsers/probe.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_probe ).
       RUN set-position IN h_probe ( 5.05 , 2.00 ) NO-ERROR.
       RUN set-size IN h_probe ( 14.19 , 143.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-probe.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-probe ).
       RUN set-position IN h_p-probe ( 21.71 , 5.00 ) NO-ERROR.
       RUN set-size IN h_p-probe ( 2.38 , 121.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'cec/vp-box.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vp-box ).
       RUN set-position IN h_vp-box ( 21.71 , 133.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.33 , 14.80 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartViewer h_vi-est-5. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_vi-est-5 ).

       /* Links to SmartBrowser h_probe. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_probe ).
       RUN add-link IN adm-broker-hdl ( h_p-probe , 'TableIO':U , h_probe ).
       RUN add-link IN adm-broker-hdl ( h_vp-box , 'boxprt':U , h_probe ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_vi-est-5 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_probe ,
             h_vi-est-5 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-probe ,
             h_probe , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_vp-box ,
             h_p-probe , 'AFTER':U ).
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
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_w-qtest ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'quote':U , h_w-qtest ).

    END. /* Page 10 */
    WHEN 11 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/vi-est2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-est2 ).
       RUN set-position IN h_vi-est2 ( 2.67 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 146.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/b-eitem2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-eitem2 ).
       RUN set-position IN h_b-eitem2 ( 5.05 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-eitem2 ( 15.95 , 58.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/v-eitem2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-eitem2 ).
       RUN set-position IN h_v-eitem2 ( 5.05 , 60.00 ) NO-ERROR.
       /* Size in UIB:  ( 15.71 , 99.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/farmnav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_farmnav ).
       RUN set-position IN h_farmnav ( 21.71 , 13.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.14 , 34.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/p-updven.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updven ).
       RUN set-position IN h_p-updven ( 21.71 , 60.00 ) NO-ERROR.
       RUN set-size IN h_p-updven ( 2.14 , 58.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/pricechg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_pricechg ).
       RUN set-position IN h_pricechg ( 21.71 , 121.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.05 , 17.20 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartViewer h_vi-est2. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_vi-est2 ).

       /* Links to SmartNavBrowser h_b-eitem2. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_b-eitem2 ).

       /* Links to SmartViewer h_v-eitem2. */
       RUN add-link IN adm-broker-hdl ( h_b-eitem2 , 'Record':U , h_v-eitem2 ).
       RUN add-link IN adm-broker-hdl ( h_p-updven , 'TableIO':U , h_v-eitem2 ).

       /* Links to SmartViewer h_farmnav. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'farmNav':U , h_farmnav ).

       /* Links to SmartViewer h_pricechg. */
       RUN add-link IN adm-broker-hdl ( h_v-eitem2 , 'price-change':U , h_pricechg ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_vi-est2 ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-eitem2 ,
             h_vi-est2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-eitem2 ,
             h_b-eitem2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_farmnav ,
             h_v-eitem2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updven ,
             h_farmnav , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_pricechg ,
             h_p-updven , 'AFTER':U ).
    END. /* Page 11 */
    WHEN 12 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/q-est3.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-est3 ).
       RUN set-position IN h_q-est3 ( 5.52 , 7.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.86 , 10.80 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartQuery h_q-est3. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'form-blank':U , h_q-est3 ).
       RUN add-link IN adm-broker-hdl ( h_q-ordest , 'Record':U , h_q-est3 ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 12 */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get-g_rec_key W-Win 
PROCEDURE Get-g_rec_key :
/*------------------------------------------------------------------------------
  Purpose:     does nothing
  Parameters:  op-rec_key
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER op-rec_key AS CHARACTER NO-UNDO.

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
             INPUT  FRAME F-Main:HANDLE,
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
  {src/adm/template/snd-list.i "oe-ordl"}

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

