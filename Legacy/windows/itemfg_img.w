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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
&scoped-define item_spec FGITEM

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Record-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES itemfg
&Scoped-define FIRST-EXTERNAL-TABLE itemfg


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR itemfg.
/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-eitem AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-itemsp AS HANDLE NO-UNDO.
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fg-set AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fgijob AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fgijob-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemfg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemfg-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemfg-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemfg-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemfg2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemfg2-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itemfgt AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itmfgink AS HANDLE NO-UNDO.
DEFINE VARIABLE h_itmfgink-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_options AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-boximg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-bximg2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-calcc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-calcq AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-fg-bj AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-fgset AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-overq AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updcan AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updcan-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updcan-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updcan-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updncp AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updncp-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_pricechg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_smartmsg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-eitem AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-fgimg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-fgimg3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-itemsp AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 150 BY 24
         BGCOLOR 4 .

DEFINE FRAME OPTIONS-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 62 ROW 1.24
         SIZE 89 BY 1.67
         BGCOLOR 4 .

DEFINE FRAME FRAME-C
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 1.24
         SIZE 43 BY 1.67
         BGCOLOR 4 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: ASI.itemfg
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 10
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Finished Goods Item Inventory"
         HEIGHT             = 23.95
         WIDTH              = 149.8
         MAX-HEIGHT         = 26.62
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 26.62
         VIRTUAL-WIDTH      = 160
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

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
ASSIGN FRAME FRAME-C:FRAME = FRAME F-Main:HANDLE
       FRAME OPTIONS-FRAME:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
                                                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-C:MOVE-BEFORE-TAB-ITEM (FRAME OPTIONS-FRAME:HANDLE)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FRAME FRAME-C
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

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME OPTIONS-FRAME
/* Query rebuild information for FRAME OPTIONS-FRAME
     _Query            is NOT OPENED
*/  /* FRAME OPTIONS-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Finished Goods Item Inventory */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Finished Goods Item Inventory */
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
             INPUT  FRAME FRAME-C:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_smartmsg ).
       RUN set-position IN h_smartmsg ( 1.48 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.14 , 32.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/options.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_options ).
       RUN set-position IN h_options ( 1.00 , 25.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.67 , 55.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/exit.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.00 , 81.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.67 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Brws Items|View Item|2nd Page|Totals|Bin/Jobs|Set parts|Colors|Vend Cost|Image|3D Image' + ',
                     FOLDER-TAB-TYPE = 2':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 3.14 , 2.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 21.67 , 148.00 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/itemfg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itemfg ).
       RUN set-position IN h_itemfg ( 4.81 , 5.00 ) NO-ERROR.
       RUN set-size IN h_itemfg ( 19.52 , 145.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2') NO-ERROR.

       /* Links to SmartNavBrowser h_itemfg. */
       RUN add-link IN adm-broker-hdl ( h_p-navico , 'Navigation':U , h_itemfg ).
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'Record':U , THIS-PROCEDURE ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/itemfg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itemfg-2 ).
       RUN set-position IN h_itemfg-2 ( 4.57 , 5.00 ) NO-ERROR.
       /* Size in UIB:  ( 17.38 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-navico.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navico ).
       RUN set-position IN h_p-navico ( 22.19 , 12.00 ) NO-ERROR.
       RUN set-size IN h_p-navico ( 2.14 , 38.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-updsav.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav ).
       RUN set-position IN h_p-updsav ( 22.19 , 69.00 ) NO-ERROR.
       RUN set-size IN h_p-updsav ( 2.14 , 56.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/p-calcc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_p-calcc ).
       RUN set-position IN h_p-calcc ( 22.19 , 125.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.14 , 17.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1') NO-ERROR.

       /* Links to SmartViewer h_itemfg-2. */
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'Record':U , h_itemfg-2 ).
       RUN add-link IN adm-broker-hdl ( h_p-updsav , 'TableIO':U , h_itemfg-2 ).

       /* Links to SmartViewer h_p-calcc. */
       RUN add-link IN adm-broker-hdl ( h_itemfg-2 , 'calc-cost':U , h_p-calcc ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/itemfg2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itemfg2 ).
       RUN set-position IN h_itemfg2 ( 5.05 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 13.81 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updcan.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updcan ).
       RUN set-position IN h_p-updcan ( 19.81 , 42.00 ) NO-ERROR.
       RUN set-size IN h_p-updcan ( 2.14 , 31.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/p-calcq.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_p-calcq ).
       RUN set-position IN h_p-calcq ( 19.81 , 73.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.14 , 17.20 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/p-overq.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_p-overq ).
       RUN set-position IN h_p-overq ( 19.81 , 90.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.14 , 17.20 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1') NO-ERROR.

       /* Links to SmartViewer h_itemfg2. */
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'Record':U , h_itemfg2 ).
       RUN add-link IN adm-broker-hdl ( h_p-updcan , 'TableIO':U , h_itemfg2 ).

       /* Links to SmartViewer h_p-calcq. */
       RUN add-link IN adm-broker-hdl ( h_itemfg2 , 'calc-qty':U , h_p-calcq ).

       /* Links to SmartViewer h_p-overq. */
       RUN add-link IN adm-broker-hdl ( h_itemfg2 , 'override-qty':U , h_p-overq ).

    END. /* Page 3 */

    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewerid/itemfg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itemfg-3 ).
       RUN set-position IN h_itemfg-3 ( 5.05 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/itemfgt.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itemfgt ).
       RUN set-position IN h_itemfgt ( 7.91 , 16.00 ) NO-ERROR.
       /* Size in UIB:  ( 12.86 , 99.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updcan.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updcan-2 ).
       RUN set-position IN h_p-updcan-2 ( 21.48 , 51.00 ) NO-ERROR.
       RUN set-size IN h_p-updcan-2 ( 2.24 , 31.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2') NO-ERROR.

       /* Links to SmartViewer h_itemfg-3. */
       RUN add-link IN adm-broker-hdl ( h_itemfg-2 , 'Record':U , h_itemfg-3 ).

       /* Links to SmartViewer h_itemfgt. */
       RUN add-link IN adm-broker-hdl ( h_itemfg-2 , 'Record':U , h_itemfgt ).
       RUN add-link IN adm-broker-hdl ( h_p-updcan-2 , 'TableIO':U , h_itemfgt ).

    END. /* Page 4 */

    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/fgijob.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_fgijob-2 ).
       RUN set-position IN h_fgijob-2 ( 4.95 , 12.80 ) NO-ERROR.
       /* Size in UIB:  ( 5.00 , 118.40 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/fgijob.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_fgijob ).
       RUN set-position IN h_fgijob ( 10.52 , 4.00 ) NO-ERROR.
       RUN set-size IN h_fgijob ( 8.57 , 144.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/p-fg-bj.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_p-fg-bj ).
       RUN set-position IN h_p-fg-bj ( 20.67 , 58.60 ) NO-ERROR.
       /* Size in UIB:  ( 2.14 , 36.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2,1') NO-ERROR.

       /* Links to SmartViewer h_fgijob-2. */
       RUN add-link IN adm-broker-hdl ( h_itemfg-2 , 'Record':U , h_fgijob-2 ).

       /* Links to SmartBrowser h_fgijob. */
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'Record':U , h_fgijob ).

       /* Links to SmartViewer h_p-fg-bj. */
       RUN add-link IN adm-broker-hdl ( h_fgijob , 'cost':U , h_p-fg-bj ).

    END. /* Page 5 */

    WHEN 6 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/fg-set.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_fg-set ).
       RUN set-position IN h_fg-set ( 9.00 , 15.20 ) NO-ERROR.
       RUN set-size IN h_fg-set ( 8.62 , 102.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-fgset.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-fgset ).
       RUN set-position IN h_p-fgset ( 18.86 , 32.00 ) NO-ERROR.
       RUN set-size IN h_p-fgset ( 1.91 , 81.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewerid/itemfg2.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itemfg2-2 ).
       RUN set-position IN h_itemfg2-2 ( 4.95 , 4.80 ) NO-ERROR.
       /* Size in UIB:  ( 3.29 , 144.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2') NO-ERROR.

       /* Links to SmartBrowser h_fg-set. */
       RUN add-link IN adm-broker-hdl ( h_itemfg-2 , 'Record':U , h_fg-set ).
       RUN add-link IN adm-broker-hdl ( h_p-fgset , 'TableIO':U , h_fg-set ).

       /* Links to SmartViewer h_itemfg2-2. */
       RUN add-link IN adm-broker-hdl ( h_itemfg-2 , 'Record':U , h_itemfg2-2 ).

    END. /* Page 6 */

    WHEN 7 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewerid/itemfg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_itemfg-4 ).
       RUN set-position IN h_itemfg-4 ( 4.33 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/itmfgink.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_itmfgink ).
       RUN set-position IN h_itmfgink ( 6.48 , 4.00 ) NO-ERROR.
       RUN set-size IN h_itmfgink ( 17.62 , 89.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/itmfgink.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_itmfgink-2 ).
       RUN set-position IN h_itmfgink-2 ( 9.57 , 94.00 ) NO-ERROR.
       /* Size in UIB:  ( 6.76 , 55.40 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-updsav.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav-2 ).
       RUN set-position IN h_p-updsav-2 ( 17.43 , 94.00 ) NO-ERROR.
       RUN set-size IN h_p-updsav-2 ( 2.38 , 55.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2,1') NO-ERROR.

       /* Links to SmartViewer h_itemfg-4. */
       RUN add-link IN adm-broker-hdl ( h_itemfg-2 , 'Record':U , h_itemfg-4 ).

       /* Links to SmartNavBrowser h_itmfgink. */
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'Record':U , h_itmfgink ).

       /* Links to SmartViewer h_itmfgink-2. */
       RUN add-link IN adm-broker-hdl ( h_itmfgink , 'Record':U , h_itmfgink-2 ).
       RUN add-link IN adm-broker-hdl ( h_p-updsav-2 , 'TableIO':U , h_itmfgink-2 ).

    END. /* Page 7 */

    WHEN 8 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/b-eitem.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-eitem ).
       RUN set-position IN h_b-eitem ( 4.81 , 4.00 ) NO-ERROR.
       RUN set-size IN h_b-eitem ( 19.52 , 51.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/v-eitem.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-eitem ).
       RUN set-position IN h_v-eitem ( 4.81 , 56.00 ) NO-ERROR.
       /* Size in UIB:  ( 16.19 , 92.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updncp.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updncp-2 ).
       RUN set-position IN h_p-updncp-2 ( 21.71 , 58.00 ) NO-ERROR.
       RUN set-size IN h_p-updncp-2 ( 1.76 , 46.80 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/pricechg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_pricechg ).
       RUN set-position IN h_pricechg ( 21.71 , 108.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.05 , 17.20 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1') NO-ERROR.

       /* Links to SmartNavBrowser h_b-eitem. */
       RUN add-link IN adm-broker-hdl ( h_itemfg , 'Record':U , h_b-eitem ).

       /* Links to SmartViewer h_v-eitem. */
       RUN add-link IN adm-broker-hdl ( h_b-eitem , 'Record':U , h_v-eitem ).
       RUN add-link IN adm-broker-hdl ( h_p-updncp-2 , 'TableIO':U , h_v-eitem ).

       /* Links to SmartViewer h_pricechg. */
       RUN add-link IN adm-broker-hdl ( h_v-eitem , 'price-change':U , h_pricechg ).

    END. /* Page 8 */

    WHEN 9 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/v-fgimg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-fgimg ).
       RUN set-position IN h_v-fgimg ( 4.81 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 17.14 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updcan.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updcan-3 ).
       RUN set-position IN h_p-updcan-3 ( 22.19 , 45.00 ) NO-ERROR.
       RUN set-size IN h_p-updcan-3 ( 2.14 , 37.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-boximg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_p-boximg ).
       RUN set-position IN h_p-boximg ( 22.19 , 82.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.14 , 33.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2') NO-ERROR.

       /* Links to SmartViewer h_v-fgimg. */
       RUN add-link IN adm-broker-hdl ( h_itemfg-2 , 'Record':U , h_v-fgimg ).
       RUN add-link IN adm-broker-hdl ( h_p-boximg , 'image':U , h_v-fgimg ).
       RUN add-link IN adm-broker-hdl ( h_p-updcan-3 , 'TableIO':U , h_v-fgimg ).

    END. /* Page 9 */

    WHEN 10 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/v-fgimg3.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-fgimg3 ).
       RUN set-position IN h_v-fgimg3 ( 4.81 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 16.43 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updcan.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updcan-4 ).
       RUN set-position IN h_p-updcan-4 ( 21.71 , 43.00 ) NO-ERROR.
       RUN set-size IN h_p-updcan-4 ( 2.14 , 35.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-bximg2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-bximg2 ).
       RUN set-position IN h_p-bximg2 ( 21.71 , 79.00 ) NO-ERROR.
       RUN set-size IN h_p-bximg2 ( 2.14 , 39.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2') NO-ERROR.

       /* Links to SmartViewer h_v-fgimg3. */
       RUN add-link IN adm-broker-hdl ( h_itemfg-2 , 'Record':U , h_v-fgimg3 ).
       RUN add-link IN adm-broker-hdl ( h_p-bximg2 , 'image':U , h_v-fgimg3 ).
       RUN add-link IN adm-broker-hdl ( h_p-bximg2 , 'TableIO':U , h_v-fgimg3 ).
       RUN add-link IN adm-broker-hdl ( h_p-updcan-4 , 'TableIO':U , h_v-fgimg3 ).

    END. /* Page 10 */

    WHEN 11 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/b-itemsp.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-itemsp ).
       RUN set-position IN h_b-itemsp ( 6.24 , 5.00 ) NO-ERROR.
       RUN set-size IN h_b-itemsp ( 5.95 , 145.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'fg/v-itemsp.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_v-itemsp ).
       RUN set-position IN h_v-itemsp ( 13.14 , 21.00 ) NO-ERROR.
       /* Size in UIB:  ( 6.67 , 107.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updncp.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updncp ).
       RUN set-position IN h_p-updncp ( 20.52 , 44.00 ) NO-ERROR.
       RUN set-size IN h_p-updncp ( 1.76 , 46.80 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2') NO-ERROR.

       /* Links to SmartNavBrowser h_b-itemsp. */
       RUN add-link IN adm-broker-hdl ( h_itemfg-2 , 'Record':U , h_b-itemsp ).

       /* Links to SmartViewer h_v-itemsp. */
       RUN add-link IN adm-broker-hdl ( h_b-itemsp , 'Record':U , h_v-itemsp ).
       RUN add-link IN adm-broker-hdl ( h_p-updncp , 'TableIO':U , h_v-itemsp ).

       /* Adjust the tab order of the smart objects. */
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
  {src/adm/template/row-list.i "itemfg"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "itemfg"}

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
  VIEW FRAME FRAME-C IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-C}
  VIEW FRAME OPTIONS-FRAME IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-OPTIONS-FRAME}
  VIEW W-Win.
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
  {src/adm/template/snd-list.i "itemfg"}

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

