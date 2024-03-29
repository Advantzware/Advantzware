&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: windows/carrier.w

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
&Scoped-define EXTERNAL-TABLES mach
&Scoped-define FIRST-EXTERNAL-TABLE mach


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR mach.
/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_mach AS HANDLE NO-UNDO.
DEFINE VARIABLE h_mach-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_mmtx AS HANDLE NO-UNDO.
DEFINE VARIABLE h_mmtx-sp AS HANDLE NO-UNDO.
DEFINE VARIABLE h_mmty AS HANDLE NO-UNDO.
DEFINE VARIABLE h_mstd AS HANDLE NO-UNDO.
DEFINE VARIABLE h_mstd-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_options AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-dwup AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-dwup-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-dwup-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updcan AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updcan-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updcan-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-mmtx AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-mmtxsp AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-mmty AS HANDLE NO-UNDO.
DEFINE VARIABLE h_runbtn AS HANDLE NO-UNDO.
DEFINE VARIABLE h_smartmsg AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 150 BY 24
         BGCOLOR 4 .

DEFINE FRAME message-frame
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 89 ROW 2.91
         SIZE 62 BY 1.43
         BGCOLOR 4 .

DEFINE FRAME OPTIONS-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 1
         SIZE 148 BY 1.91
         BGCOLOR 4 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: ASI.mach
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Machine File"
         HEIGHT             = 24.29
         WIDTH              = 149.8
         MAX-HEIGHT         = 24.29
         MAX-WIDTH          = 150
         VIRTUAL-HEIGHT     = 24.29
         VIRTUAL-WIDTH      = 150
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
                                                                        */

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
ON END-ERROR OF W-Win /* Machine File */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Machine File */
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
       RUN set-position IN h_smartmsg ( 1.00 , 26.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.14 , 32.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/options.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_options ).
       RUN set-position IN h_options ( 1.00 , 85.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 55.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/exit.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.00 , 141.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Browse|Detail|Standard|Setup|Run|Spoilage' + ',
                     FOLDER-TAB-TYPE = 2':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 3.14 , 2.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 21.67 , 148.00 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).
	   RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'udficon':U , h_options ).
    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/mach.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_mach ).
       RUN set-position IN h_mach ( 4.81 , 4.00 ) NO-ERROR.
       RUN set-size IN h_mach ( 19.52 , 145.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2') NO-ERROR.

       /* Links to SmartNavBrowser h_mach. */
       RUN add-link IN adm-broker-hdl ( h_p-navico , 'Navigation':U , h_mach ).
       RUN add-link IN adm-broker-hdl ( h_mach , 'Record':U , THIS-PROCEDURE ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/mach.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_mach-2 ).
       RUN set-position IN h_mach-2 ( 4.95 , 3.20 ) NO-ERROR.
       /* Size in UIB:  ( 16.95 , 143.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-navico.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navico ).
       RUN set-position IN h_p-navico ( 22.43 , 17.00 ) NO-ERROR.
       RUN set-size IN h_p-navico ( 1.76 , 29.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-updsav.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav ).
       RUN set-position IN h_p-updsav ( 22.43 , 71.00 ) NO-ERROR.
       RUN set-size IN h_p-updsav ( 1.76 , 56.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1') NO-ERROR.

       /* Links to SmartViewer h_mach-2. */
       RUN add-link IN adm-broker-hdl ( h_mach , 'Record':U , h_mach-2 ).
       RUN add-link IN adm-broker-hdl ( h_p-updsav , 'TableIO':U , h_mach-2 ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/mstd.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_mstd-2 ).
       RUN set-position IN h_mstd-2 ( 5.05 , 12.00 ) NO-ERROR.
       RUN set-size IN h_mstd-2 ( 6.71 , 66.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-updsav.r':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav-2 ).
       RUN set-position IN h_p-updsav-2 ( 22.29 , 34.20 ) NO-ERROR.
       RUN set-size IN h_p-updsav-2 ( 1.76 , 56.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/mstd.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_mstd ).
       RUN set-position IN h_mstd ( 12.91 , 7.00 ) NO-ERROR.
       /* Size in UIB:  ( 8.81 , 99.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2') NO-ERROR.

       /* Links to SmartBrowser h_mstd-2. */
       RUN add-link IN adm-broker-hdl ( h_mach-2 , 'Record':U , h_mstd-2 ).

       /* Links to SmartViewer h_mstd. */
       RUN add-link IN adm-broker-hdl ( h_mstd-2 , 'Record':U , h_mstd ).
       RUN add-link IN adm-broker-hdl ( h_p-updsav-2 , 'TableIO':U , h_mstd ).

    END. /* Page 3 */

    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/mmty.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_mmty ).
       RUN set-position IN h_mmty ( 5.29 , 5.00 ) NO-ERROR.
       /* Size in UIB:  ( 18.33 , 125.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-dwup.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_p-dwup ).
       RUN set-position IN h_p-dwup ( 7.67 , 131.00 ) NO-ERROR.
       RUN set-size IN h_p-dwup ( 8.81 , 16.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/q-mmty.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  '':U ,
             OUTPUT h_q-mmty ).
       RUN set-position IN h_q-mmty ( 5.33 , 135.60 ) NO-ERROR.
       /* Size in UIB:  ( 2.05 , 11.60 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updcan.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updcan ).
       RUN set-position IN h_p-updcan ( 16.71 , 131.00 ) NO-ERROR.
       RUN set-size IN h_p-updcan ( 4.29 , 16.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('3') NO-ERROR.

       /* Links to SmartViewer h_mmty. */
       RUN add-link IN adm-broker-hdl ( h_p-updcan , 'TableIO':U , h_mmty ).
       RUN add-link IN adm-broker-hdl ( h_q-mmty , 'Record':U , h_mmty ).

       /* Links to SmartQuery h_q-mmty. */
       RUN add-link IN adm-broker-hdl ( h_mstd , 'Record':U , h_q-mmty ).
       RUN add-link IN adm-broker-hdl ( h_mstd , 'st-open-y':U , h_q-mmty ).
       RUN add-link IN adm-broker-hdl ( h_p-dwup , 'TableIO':U , h_q-mmty ).

       /* Links to SmartPanel h_p-updcan. */
       RUN add-link IN adm-broker-hdl ( h_p-updcan , 'st-button':U , THIS-PROCEDURE ).

    END. /* Page 4 */

    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/mmtx.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_mmtx ).
       RUN set-position IN h_mmtx ( 4.81 , 5.00 ) NO-ERROR.
       /* Size in UIB:  ( 18.33 , 123.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-dwup.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_p-dwup-2 ).
       RUN set-position IN h_p-dwup-2 ( 5.05 , 131.00 ) NO-ERROR.
       RUN set-size IN h_p-dwup-2 ( 10.00 , 14.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updcan.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updcan-2 ).
       RUN set-position IN h_p-updcan-2 ( 15.05 , 131.00 ) NO-ERROR.
       RUN set-size IN h_p-updcan-2 ( 3.57 , 14.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/q-mmtx.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  '':U ,
             OUTPUT h_q-mmtx ).
       RUN set-position IN h_q-mmtx ( 4.57 , 137.80 ) NO-ERROR.
       /* Size in UIB:  ( 2.71 , 11.60 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/runbtn.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_runbtn ).
       RUN set-position IN h_runbtn ( 18.86 , 131.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.76 , 14.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('3') NO-ERROR.

       /* Links to SmartViewer h_mmtx. */
       RUN add-link IN adm-broker-hdl ( h_p-updcan-2 , 'TableIO':U , h_mmtx ).
       RUN add-link IN adm-broker-hdl ( h_q-mmtx , 'Record':U , h_mmtx ).

       /* Links to SmartPanel h_p-updcan-2. */
       RUN add-link IN adm-broker-hdl ( h_p-updcan-2 , 'st-button1':U , THIS-PROCEDURE ).

       /* Links to SmartQuery h_q-mmtx. */
       RUN add-link IN adm-broker-hdl ( h_mstd , 'Record':U , h_q-mmtx ).
       RUN add-link IN adm-broker-hdl ( h_mstd , 'st-open-x':U , h_q-mmtx ).
       RUN add-link IN adm-broker-hdl ( h_p-dwup-2 , 'TableIO':U , h_q-mmtx ).
       RUN add-link IN adm-broker-hdl ( h_runbtn , 'State':U , h_q-mmtx ).

    END. /* Page 5 */

    WHEN 6 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/mmtx-sp.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_mmtx-sp ).
       RUN set-position IN h_mmtx-sp ( 4.81 , 6.00 ) NO-ERROR.
       /* Size in UIB:  ( 18.33 , 122.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-dwup.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_p-dwup-3 ).
       RUN set-position IN h_p-dwup-3 ( 8.14 , 134.00 ) NO-ERROR.
       RUN set-size IN h_p-dwup-3 ( 11.67 , 12.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updcan.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updcan-3 ).
       RUN set-position IN h_p-updcan-3 ( 19.81 , 134.00 ) NO-ERROR.
       RUN set-size IN h_p-updcan-3 ( 3.33 , 12.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/q-mmtxsp.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-mmtxsp ).
       RUN set-position IN h_q-mmtxsp ( 5.52 , 132.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.05 , 11.60 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('3') NO-ERROR.

       /* Links to SmartViewer h_mmtx-sp. */
       RUN add-link IN adm-broker-hdl ( h_p-updcan-3 , 'TableIO':U , h_mmtx-sp ).
       RUN add-link IN adm-broker-hdl ( h_q-mmtxsp , 'Record':U , h_mmtx-sp ).

       /* Links to SmartPanel h_p-updcan-3. */
       RUN add-link IN adm-broker-hdl ( h_p-updcan-3 , 'st-button2':U , THIS-PROCEDURE ).

       /* Links to SmartQuery h_q-mmtxsp. */
       RUN add-link IN adm-broker-hdl ( h_mstd , 'Record':U , h_q-mmtxsp ).
       RUN add-link IN adm-broker-hdl ( h_mstd , 'st-open-xsp':U , h_q-mmtxsp ).
       RUN add-link IN adm-broker-hdl ( h_p-dwup-3 , 'TableIO':U , h_q-mmtxsp ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 6 */

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
  {src/adm/template/row-list.i "mach"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "mach"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE change-page W-Win 
PROCEDURE change-page :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-page-no AS INTEGER NO-UNDO.

  run select-page (ip-page-no).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-change-page W-Win 
PROCEDURE local-change-page :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
   def var char-hdl as cha no-undo.
   def var li-page as int no-undo.
   
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'change-page':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run get-attribute ('current-page').  
  li-page = integer(return-value).
  if li-page >= 4 then do:
/*     message "change to " return-value. */
     if li-page = 4 then run get-link-handle in adm-broker-hdl (this-procedure, 'st-button-source', output char-hdl).
     else if li-page = 5 then run get-link-handle in adm-broker-hdl (this-procedure, 'st-button1-source', output char-hdl).
     else if li-page = 6 then run get-link-handle in adm-broker-hdl (this-procedure, 'st-button2-source', output char-hdl). 
     run dispatch in widget-handle(char-hdl) ('initialize').
     
  end.

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
  {src/adm/template/snd-list.i "mach"}

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

