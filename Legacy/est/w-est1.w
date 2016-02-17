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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
  /* spec note selection */
&scoped-define item_spec  FGITEM

def var li-page as int extent 2 no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Record-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME est

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES est
&Scoped-define FIRST-EXTERNAL-TABLE est


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR est.
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
DEFINE VARIABLE h_b-estq AS HANDLE NO-UNDO.
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_optionse AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-box23d AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-boxupd AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-bximg2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-estop AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-inkpak AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-layouf AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-probe AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-rfqsiz AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updc&c AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_probe AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-boxdes AS HANDLE NO-UNDO.
DEFINE VARIABLE h_smartmsg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-boxdee AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-est AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-est2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-est3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-est4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-naveb AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-naveb-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-naveb-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-navef AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-navest AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-est AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-est-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-est-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-est-5 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-est2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vp-est AS HANDLE NO-UNDO.
DEFINE VARIABLE h_w-qtest AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME est
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 150 BY 24.48
         BGCOLOR 4 .

DEFINE FRAME message-frame
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 58 BY 2
         BGCOLOR 4 .

DEFINE FRAME OPTIONS-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 58 ROW 1
         SIZE 93 BY 1.91
         BGCOLOR 4 .

DEFINE FRAME FRAME-A
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         NO-LABELS NO-UNDERLINE THREE-D 
         AT COL 145 ROW 3.14
         SIZE 6 BY 1.19
         BGCOLOR 4 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: ASI.est
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
         WIDTH              = 150.6
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
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

{Advantzware/WinKit/embedwindow.i}
{src/adm/method/containr.i}
{methods/template/windows.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-A:FRAME = FRAME est:HANDLE
       FRAME message-frame:FRAME = FRAME est:HANDLE
       FRAME OPTIONS-FRAME:FRAME = FRAME est:HANDLE.

/* SETTINGS FOR FRAME est
                                                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME message-frame:MOVE-BEFORE-TAB-ITEM (FRAME OPTIONS-FRAME:HANDLE)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FRAME FRAME-A
                                                                        */
/* SETTINGS FOR FRAME message-frame
                                                                        */
/* SETTINGS FOR FRAME OPTIONS-FRAME
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME est
/* Query rebuild information for FRAME est
     _Query            is NOT OPENED
*/  /* FRAME est */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
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
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */
session:data-entry-return = yes.


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
             INPUT  'smartobj/optionse.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_optionse ).
       RUN set-position IN h_optionse ( 1.00 , 30.20 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 63.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/smartmsg.w':U ,
             INPUT  FRAME message-frame:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_smartmsg ).
       RUN set-position IN h_smartmsg ( 1.71 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.14 , 32.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/exit.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.00 , 86.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Brws Est|Estimate|Specs|Layout|Inks/Pack|Prep/Route|Misc/Farm|Box Design|Print|Quote' + ',
                     FOLDER-TAB-TYPE = 2':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 3.14 , 1.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 21.67 , 150.00 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'ce/b-estq.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-estq ).
       RUN set-position IN h_b-estq ( 4.81 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 20.00 , 146.00 ) */

       /* Links to SmartNavBrowser h_b-estq. */
       RUN add-link IN adm-broker-hdl ( h_b-estq , 'Record':U , THIS-PROCEDURE ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'ce/b-estitm.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_b-estitm ).
       RUN set-position IN h_b-estitm ( 5.05 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 17.62 , 148.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-updsav.r':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav-2 ).
       RUN set-position IN h_p-updsav-2 ( 21.95 , 64.00 ) NO-ERROR.
       RUN set-size IN h_p-updsav-2 ( 1.91 , 59.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/vp-est.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vp-est ).
       RUN set-position IN h_vp-est ( 21.95 , 116.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 35.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-navest.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-navest ).
       RUN set-position IN h_v-navest ( 22.19 , 14.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 34.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1') NO-ERROR.

       /* Links to SmartNavBrowser h_b-estitm. */
       RUN add-link IN adm-broker-hdl ( h_b-estq , 'Record':U , h_b-estitm ).
       RUN add-link IN adm-broker-hdl ( h_p-updsav-2 , 'TableIO':U , h_b-estitm ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'add-est':U , h_b-estitm ).

       /* Links to SmartViewer h_vp-est. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_vp-est ).

       /* Links to SmartViewer h_v-navest. */
       RUN add-link IN adm-broker-hdl ( h_b-estq , 'nav-itm':U , h_v-navest ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'ce/v-est.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-est ).
       RUN set-position IN h_v-est ( 5.05 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 16.91 , 150.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-rfqsiz.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-rfqsiz ).
       RUN set-position IN h_p-rfqsiz ( 22.19 , 75.00 ) NO-ERROR.
       RUN set-size IN h_p-rfqsiz ( 1.76 , 66.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-naveb.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-naveb ).
       RUN set-position IN h_v-naveb ( 22.43 , 19.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 42.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2') NO-ERROR.

       /* Links to SmartViewer h_v-est. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_v-est ).
       RUN add-link IN adm-broker-hdl ( h_p-rfqsiz , 'TableIO':U , h_v-est ).

       /* Links to SmartViewer h_v-naveb. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'nav-itm':U , h_v-naveb ).

    END. /* Page 3 */

    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/vi-est2.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-est2 ).
       RUN set-position IN h_vi-est2 ( 4.81 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 146.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'ce/v-est2.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-est2 ).
       RUN set-position IN h_v-est2 ( 6.71 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 15.95 , 147.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-navef.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-navef ).
       RUN set-position IN h_v-navef ( 22.67 , 19.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 42.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-layouf.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-layouf ).
       RUN set-position IN h_p-layouf ( 22.67 , 77.00 ) NO-ERROR.
       RUN set-size IN h_p-layouf ( 1.67 , 64.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2') NO-ERROR.

       /* Links to SmartViewer h_vi-est2. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_vi-est2 ).

       /* Links to SmartViewer h_v-est2. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_v-est2 ).
       RUN add-link IN adm-broker-hdl ( h_p-layouf , 'TableIO':U , h_v-est2 ).

       /* Links to SmartViewer h_v-navef. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'nav-itm':U , h_v-navef ).

    END. /* Page 4 */

    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/vi-est-.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-est-3 ).
       RUN set-position IN h_vi-est-3 ( 4.81 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 146.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'ce/v-est3.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-est3 ).
       RUN set-position IN h_v-est3 ( 6.71 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 16.19 , 150.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-naveb.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-naveb-2 ).
       RUN set-position IN h_v-naveb-2 ( 23.14 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 42.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-inkpak.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-inkpak ).
       RUN set-position IN h_p-inkpak ( 23.14 , 45.00 ) NO-ERROR.
       RUN set-size IN h_p-inkpak ( 1.43 , 104.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2') NO-ERROR.

       /* Links to SmartViewer h_vi-est-3. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_vi-est-3 ).

       /* Links to SmartViewer h_v-est3. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_v-est3 ).
       RUN add-link IN adm-broker-hdl ( h_p-inkpak , 'TableIO':U , h_v-est3 ).

       /* Links to SmartViewer h_v-naveb-2. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'nav-itm':U , h_v-naveb-2 ).

    END. /* Page 5 */

    WHEN 6 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/vi-est-.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-est-4 ).
       RUN set-position IN h_vi-est-4 ( 4.57 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 146.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/b-estprp.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_b-estprp ).
       RUN set-position IN h_b-estprp ( 6.95 , 6.00 ) NO-ERROR.
       RUN set-size IN h_b-estprp ( 7.86 , 121.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-updsav.r':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav-3 ).
       RUN set-position IN h_p-updsav-3 ( 7.19 , 133.00 ) NO-ERROR.
       RUN set-size IN h_p-updsav-3 ( 7.38 , 13.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'ce/b-estop.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_b-estop ).
       RUN set-position IN h_b-estop ( 15.76 , 4.00 ) NO-ERROR.
       RUN set-size IN h_b-estop ( 8.24 , 128.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-estop.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-estop ).
       RUN set-position IN h_p-estop ( 15.76 , 134.00 ) NO-ERROR.
       RUN set-size IN h_p-estop ( 8.10 , 14.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2') NO-ERROR.

       /* Links to SmartViewer h_vi-est-4. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_vi-est-4 ).

       /* Links to SmartBrowser h_b-estprp. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_b-estprp ).
       RUN add-link IN adm-broker-hdl ( h_p-updsav-3 , 'TableIO':U , h_b-estprp ).

       /* Links to SmartBrowser h_b-estop. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_b-estop ).
       RUN add-link IN adm-broker-hdl ( h_p-estop , 'TableIO':U , h_b-estop ).

    END. /* Page 6 */

    WHEN 7 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/vi-est-.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-est ).
       RUN set-position IN h_vi-est ( 4.81 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 146.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updc&c.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updc&c ).
       RUN set-position IN h_p-updc&c ( 9.57 , 118.00 ) NO-ERROR.
       RUN set-size IN h_p-updc&c ( 10.95 , 22.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-est4.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-est4 ).
       RUN set-position IN h_v-est4 ( 6.91 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 17.05 , 105.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2') NO-ERROR.

       /* Links to SmartViewer h_vi-est. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_vi-est ).

       /* Links to SmartViewer h_v-est4. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_v-est4 ).
       RUN add-link IN adm-broker-hdl ( h_p-updc&c , 'TableIO':U , h_v-est4 ).

    END. /* Page 7 */

    WHEN 8 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'ce/v-boxdee.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-boxdee ).
       RUN set-position IN h_v-boxdee ( 5.29 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 16.43 , 146.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-naveb.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-naveb-3 ).
       RUN set-position IN h_v-naveb-3 ( 22.19 , 6.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 42.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-boxupd.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-boxupd ).
       RUN set-position IN h_p-boxupd ( 22.19 , 66.00 ) NO-ERROR.
       RUN set-size IN h_p-boxupd ( 1.76 , 31.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-box23d.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-box23d ).
       RUN set-position IN h_p-box23d ( 22.19 , 97.00 ) NO-ERROR.
       RUN set-size IN h_p-box23d ( 1.76 , 19.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-bximg2.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-bximg2 ).
       RUN set-position IN h_p-bximg2 ( 22.19 , 116.00 ) NO-ERROR.
       RUN set-size IN h_p-bximg2 ( 1.76 , 33.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/q-boxdes.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-boxdes ).
       RUN set-position IN h_q-boxdes ( 4.57 , 135.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.86 , 10.80 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2') NO-ERROR.

       /* Links to SmartViewer h_v-boxdee. */
       RUN add-link IN adm-broker-hdl ( h_p-box23d , 'TableIO':U , h_v-boxdee ).
       RUN add-link IN adm-broker-hdl ( h_p-boxupd , 'TableIO':U , h_v-boxdee ).
       RUN add-link IN adm-broker-hdl ( h_p-bximg2 , 'image':U , h_v-boxdee ).
       RUN add-link IN adm-broker-hdl ( h_p-bximg2 , 'TableIO':U , h_v-boxdee ).
       RUN add-link IN adm-broker-hdl ( h_q-boxdes , 'Record':U , h_v-boxdee ).
       RUN add-link IN adm-broker-hdl ( h_v-boxdee , 'boxdes':U , THIS-PROCEDURE ).

       /* Links to SmartViewer h_v-naveb-3. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'nav-itm':U , h_v-naveb-3 ).

       /* Links to SmartQuery h_q-boxdes. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_q-boxdes ).

    END. /* Page 8 */

    WHEN 9 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'ce/probe.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_probe ).
       RUN set-position IN h_probe ( 7.19 , 5.00 ) NO-ERROR.
       RUN set-size IN h_probe ( 13.10 , 143.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-probe.w':U ,
             INPUT  FRAME est:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-probe ).
       RUN set-position IN h_p-probe ( 21.48 , 14.00 ) NO-ERROR.
       RUN set-size IN h_p-probe ( 2.38 , 116.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/vi-est-.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-est-5 ).
       RUN set-position IN h_vi-est-5 ( 4.95 , 3.40 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 146.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2') NO-ERROR.

       /* Links to SmartBrowser h_probe. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_probe ).
       RUN add-link IN adm-broker-hdl ( h_p-probe , 'TableIO':U , h_probe ).

       /* Links to SmartViewer h_vi-est-5. */
       RUN add-link IN adm-broker-hdl ( h_b-estitm , 'Record':U , h_vi-est-5 ).

    END. /* Page 9 */

    WHEN 10 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/w-qtest.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_w-qtest ).
       /* Position in AB:  ( 5.43 , 5.20 ) */
       /* Size in UIB:  ( 2.05 , 11.60 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1') NO-ERROR.

       /* Links to SmartWindow h_w-qtest. */
       RUN add-link IN adm-broker-hdl ( h_b-estq , 'Record':U , h_w-qtest ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'quote':U , h_w-qtest ).

       /* Adjust the tab order of the smart objects. */
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
  {src/adm/template/row-list.i "est"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "est"}

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
  VIEW FRAME est IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-est}
  VIEW FRAME message-frame IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-message-frame}
  VIEW FRAME OPTIONS-FRAME IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-OPTIONS-FRAME}
  VIEW FRAME FRAME-A IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
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
             INPUT  FRAME est:HANDLE ,
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
                            bf-quote.est-no = est.est-no
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
  IF li-page[1] = 8 THEN DO:  /* box design */
     RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"boxdes-source", OUTPUT char-hdl).
     IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
         RUN redisplay-design IN WIDGET-HANDLE(char-hdl).
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Select_Add W-Win 
PROCEDURE Select_Add :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  def var char-hdl as cha no-undo.
  
  run select-page(2).
  run get-link-handle in adm-broker-hdl(this-procedure,"add-est-target", output char-hdl).
  run add-estimate in widget-handle(char-hdl).
  
    
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
  {src/adm/template/snd-list.i "est"}

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

