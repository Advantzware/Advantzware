&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          rfq              PROGRESS
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
def var ll-enable-trx as log no-undo.
{custom/gcompany.i}

/*&SCOPED-DEFINE setUserPrint*/

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
&Scoped-define EXTERNAL-TABLES rfq
&Scoped-define FIRST-EXTERNAL-TABLE rfq


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR rfq.
/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-rfqlst AS HANDLE NO-UNDO.
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_optionse2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-rfqsiz AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updcan-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updcan-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updcan-5 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updprt AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_rfq AS HANDLE NO-UNDO.
DEFINE VARIABLE h_rfq-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_rfq-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_rfqiprt AS HANDLE NO-UNDO.
DEFINE VARIABLE h_rfqiship AS HANDLE NO-UNDO.
DEFINE VARIABLE h_rfqispec AS HANDLE NO-UNDO.
DEFINE VARIABLE h_rfqitem AS HANDLE NO-UNDO.
DEFINE VARIABLE h_rfqitem-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_rfqitem-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_rfqitem-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_rfqmat AS HANDLE NO-UNDO.
DEFINE VARIABLE h_rfqsize AS HANDLE NO-UNDO.
DEFINE VARIABLE h_rfqtoest AS HANDLE NO-UNDO.
DEFINE VARIABLE h_rfqtool AS HANDLE NO-UNDO.
DEFINE VARIABLE h_smartmsg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-set AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 151.2 BY 24.1
         BGCOLOR 4 .

DEFINE FRAME message-frame2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 117 ROW 2.91
         SIZE 35 BY 1.19
         BGCOLOR 4 .

DEFINE FRAME OPTIONS-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 64 ROW 1
         SIZE 86 BY 1.91
         BGCOLOR 4 .

DEFINE FRAME message-frame
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE 
         AT COL 2 ROW 1.48
         SIZE 57 BY 1.43
         BGCOLOR 4 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: rfq.rfq
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Request For Quote"
         HEIGHT             = 24.05
         WIDTH              = 151
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
       FRAME message-frame2:FRAME = FRAME F-Main:HANDLE
       FRAME OPTIONS-FRAME:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
                                                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME OPTIONS-FRAME:MOVE-BEFORE-TAB-ITEM (FRAME message-frame:HANDLE)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FRAME message-frame
                                                                        */
/* SETTINGS FOR FRAME message-frame2
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

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME message-frame2
/* Query rebuild information for FRAME message-frame2
     _Query            is NOT OPENED
*/  /* FRAME message-frame2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME OPTIONS-FRAME
/* Query rebuild information for FRAME OPTIONS-FRAME
     _Query            is NOT OPENED
*/  /* FRAME OPTIONS-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Request For Quote */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Request For Quote */
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
{custom/getcmpny.i}
/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

 run valid-license no-error.
 if error-status:error then do:

    apply "close" to this-procedure.   
    return no-apply.
 end.

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
             INPUT  'smartobj/optionse2.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_optionse2 ).
       RUN set-position IN h_optionse2 ( 1.00 , 23.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 55.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/smartmsg.w':U ,
             INPUT  FRAME message-frame:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_smartmsg ).
       RUN set-position IN h_smartmsg ( 1.24 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.14 , 32.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/exit.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.00 , 79.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Brws Part#|View RFQ|RFQ Item|Size|Item Spec|Materials|Printing|Shipping' + ',
                     FOLDER-TAB-TYPE = 2':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 2.91 , 2.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 22.14 , 150.00 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'addon/rfq/b-rfqlst.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-rfqlst ).
       RUN set-position IN h_b-rfqlst ( 4.57 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 20.00 , 146.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartNavBrowser h_b-rfqlst. */
       RUN add-link IN adm-broker-hdl ( h_p-navico , 'Navigation':U , h_b-rfqlst ).
       RUN add-link IN adm-broker-hdl ( h_b-rfqlst , 'Record':U , THIS-PROCEDURE ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/rfqtool.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_rfqtool ).
       RUN set-position IN h_rfqtool ( 5.05 , 8.00 ) NO-ERROR.
       /* Size in UIB:  ( 14.86 , 139.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-navico.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navico ).
       RUN set-position IN h_p-navico ( 20.52 , 15.00 ) NO-ERROR.
       RUN set-size IN h_p-navico ( 2.14 , 38.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-updsav.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav ).
       RUN set-position IN h_p-updsav ( 20.76 , 82.00 ) NO-ERROR.
       RUN set-size IN h_p-updsav ( 2.14 , 56.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_rfqtool. */
       RUN add-link IN adm-broker-hdl ( h_b-rfqlst , 'Record':U , h_rfqtool ).
       RUN add-link IN adm-broker-hdl ( h_p-updsav , 'TableIO':U , h_rfqtool ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'add-rfq':U , h_rfqtool ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewerid/rfq.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_rfq-3 ).
       RUN set-position IN h_rfq-3 ( 5.05 , 7.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 130.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'browsers/rfqitem.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_rfqitem ).
       RUN set-position IN h_rfqitem ( 7.43 , 5.00 ) NO-ERROR.
       RUN set-size IN h_rfqitem ( 13.81 , 142.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'addon/rfq/v-set.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-set ).
       RUN set-position IN h_v-set ( 21.71 , 85.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.14 , 17.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'windows/rfqtoest.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_rfqtoest ).
       RUN set-position IN h_rfqtoest ( 21.71 , 103.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.24 , 35.20 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-updsav.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav-2 ).
       RUN set-position IN h_p-updsav-2 ( 21.95 , 19.00 ) NO-ERROR.
       RUN set-size IN h_p-updsav-2 ( 1.76 , 56.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_rfq-3. */
       RUN add-link IN adm-broker-hdl ( h_b-rfqlst , 'Record':U , h_rfq-3 ).

       /* Links to SmartBrowser h_rfqitem. */
       RUN add-link IN adm-broker-hdl ( h_b-rfqlst , 'Record':U , h_rfqitem ).
       RUN add-link IN adm-broker-hdl ( h_p-updsav-2 , 'TableIO':U , h_rfqitem ).
       RUN add-link IN adm-broker-hdl ( h_rfqtoest , 'transfer':U , h_rfqitem ).
       RUN add-link IN adm-broker-hdl ( h_v-set , 'set':U , h_rfqitem ).

       /* Links to SmartFrame h_rfqtoest. */
       RUN add-link IN adm-broker-hdl ( h_rfqitem , 'Record':U , h_rfqtoest ).

    END. /* Page 3 */

    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewerid/rfq.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_rfq-2 ).
       RUN set-position IN h_rfq-2 ( 5.62 , 9.80 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 130.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/rfqsize.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = ?':U ,
             OUTPUT h_rfqsize ).
       RUN set-position IN h_rfqsize ( 8.86 , 18.00 ) NO-ERROR.
       /* Size in UIB:  ( 9.29 , 103.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-rfqsiz.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-rfqsiz ).
       RUN set-position IN h_p-rfqsiz ( 19.10 , 39.00 ) NO-ERROR.
       RUN set-size IN h_p-rfqsiz ( 1.76 , 52.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('5,3':U) NO-ERROR.

       /* Links to SmartViewer h_rfq-2. */
       RUN add-link IN adm-broker-hdl ( h_rfq , 'Record':U , h_rfq-2 ).

       /* Links to SmartViewer h_rfqsize. */
       RUN add-link IN adm-broker-hdl ( h_p-rfqsiz , 'TableIO':U , h_rfqsize ).
       RUN add-link IN adm-broker-hdl ( h_rfqitem , 'Record':U , h_rfqsize ).

    END. /* Page 4 */

    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewerid/rfq.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_rfq ).
       RUN set-position IN h_rfq ( 6.00 , 11.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 130.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/rfqispec.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_rfqispec ).
       RUN set-position IN h_rfqispec ( 8.86 , 16.00 ) NO-ERROR.
       /* Size in UIB:  ( 9.52 , 117.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updcan.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updcan-2 ).
       RUN set-position IN h_p-updcan-2 ( 19.38 , 58.00 ) NO-ERROR.
       RUN set-size IN h_p-updcan-2 ( 1.76 , 31.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1,3':U) NO-ERROR.

       /* Links to SmartViewer h_rfq. */
       RUN add-link IN adm-broker-hdl ( h_b-rfqlst , 'Record':U , h_rfq ).

       /* Links to SmartViewer h_rfqispec. */
       RUN add-link IN adm-broker-hdl ( h_p-updcan-2 , 'TableIO':U , h_rfqispec ).
       RUN add-link IN adm-broker-hdl ( h_rfqitem , 'Record':U , h_rfqispec ).

    END. /* Page 5 */

    WHEN 6 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewerid/rfqitem.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_rfqitem-4 ).
       RUN set-position IN h_rfqitem-4 ( 4.57 , 10.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.67 , 139.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/rfqmat.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_rfqmat ).
       RUN set-position IN h_rfqmat ( 6.48 , 14.00 ) NO-ERROR.
       /* Size in UIB:  ( 15.00 , 116.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updcan.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updcan-3 ).
       RUN set-position IN h_p-updcan-3 ( 22.57 , 54.40 ) NO-ERROR.
       RUN set-size IN h_p-updcan-3 ( 1.76 , 31.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('3':U) NO-ERROR.

       /* Links to SmartViewer h_rfqitem-4. */
       RUN add-link IN adm-broker-hdl ( h_rfqitem , 'Record':U , h_rfqitem-4 ).

       /* Links to SmartViewer h_rfqmat. */
       RUN add-link IN adm-broker-hdl ( h_p-updcan-3 , 'TableIO':U , h_rfqmat ).
       RUN add-link IN adm-broker-hdl ( h_rfqitem , 'Record':U , h_rfqmat ).

    END. /* Page 6 */

    WHEN 7 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewerid/rfqitem.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_rfqitem-3 ).
       RUN set-position IN h_rfqitem-3 ( 5.29 , 10.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.67 , 139.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/rfqiprt.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_rfqiprt ).
       RUN set-position IN h_rfqiprt ( 7.43 , 13.00 ) NO-ERROR.
       /* Size in UIB:  ( 14.29 , 115.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updprt.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updprt ).
       RUN set-position IN h_p-updprt ( 22.67 , 49.00 ) NO-ERROR.
       RUN set-size IN h_p-updprt ( 1.76 , 31.40 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('3,4':U) NO-ERROR.

       /* Links to SmartViewer h_rfqitem-3. */
       RUN add-link IN adm-broker-hdl ( h_rfqitem , 'Record':U , h_rfqitem-3 ).

       /* Links to SmartViewer h_rfqiprt. */
       RUN add-link IN adm-broker-hdl ( h_p-updprt , 'TableIO':U , h_rfqiprt ).
       RUN add-link IN adm-broker-hdl ( h_rfqsize , 'Record':U , h_rfqiprt ).

    END. /* Page 7 */

    WHEN 8 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewerid/rfqitem.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_rfqitem-2 ).
       RUN set-position IN h_rfqitem-2 ( 5.05 , 9.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.67 , 139.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'viewers/rfqiship.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_rfqiship ).
       RUN set-position IN h_rfqiship ( 8.14 , 12.00 ) NO-ERROR.
       /* Size in UIB:  ( 10.76 , 118.20 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updcan.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updcan-5 ).
       RUN set-position IN h_p-updcan-5 ( 20.05 , 55.40 ) NO-ERROR.
       RUN set-size IN h_p-updcan-5 ( 1.76 , 31.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('4,3':U) NO-ERROR.

       /* Links to SmartViewer h_rfqitem-2. */
       RUN add-link IN adm-broker-hdl ( h_rfqsize , 'Record':U , h_rfqitem-2 ).

       /* Links to SmartViewer h_rfqiship. */
       RUN add-link IN adm-broker-hdl ( h_p-updcan-5 , 'TableIO':U , h_rfqiship ).
       RUN add-link IN adm-broker-hdl ( h_rfqitem , 'Record':U , h_rfqiship ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 8 */

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
  {src/adm/template/row-list.i "rfq"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "rfq"}

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
  VIEW FRAME message-frame2 IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-message-frame2}
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

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'change-page':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  if integer(return-value) > 3 and not ll-enable-trx then do:
     ll-enable-trx = yes.
    /* RUN enable_UI IN h_rfqtoest NO-ERROR .*/
     run dispatch in h_rfqtoest ('enable').     
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select_add W-Win 
PROCEDURE select_add :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 def var char-hdl as cha no-undo.
  
  run select-page(2).
  run get-link-handle in adm-broker-hdl(this-procedure,"add-rfq-target", output char-hdl).
  run add-rfq in widget-handle(char-hdl).

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
  {src/adm/template/snd-list.i "rfq"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setUserPrint W-Win 
PROCEDURE setUserPrint :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 IF AVAILABLE rfq THEN
  RUN custom/setUserPrint.p (rfq.company,'rfq_.','','') .
                             

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-license W-Win 
PROCEDURE valid-license :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  find first sys-ctrl where name = "timeout"
                      no-lock no-error.
  if avail sys-ctrl and sys-ctrl.log-fld then do: 
        
     if today - sys-ctrl.date-fld >= sys-ctrl.int-fld then do:
        
        message "This is a fully functional unregistered version "
                "for evaluation use only." 
                view-as alert-box warning button ok title "Warning!  Advantzware RFQ".
     
        return error.
     end.
     
     
     return .         
  end. 
  if not avail sys-ctrl then do:
     create sys-ctrl.
     assign sys-ctrl.company = gcompany
            sys-ctrl.name = "timeout"
            sys-ctrl.log-fld = yes
            sys-ctrl.date-fld = today
            sys-ctrl.int-fld = 60
            .
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

