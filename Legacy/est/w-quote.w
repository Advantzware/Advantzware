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
&SCOPED-DEFINE h_Browse01 h_b-qthd
&SCOPED-DEFINE h_Object01 h_p-qtmisc
&SCOPED-DEFINE h_Object02 h_p-qtqty
&SCOPED-DEFINE h_Object03 h_vp-qtrpc

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

&scoped-define item_spec  FGITEM
DEF VAR rQuoteRow AS ROWID NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES quotehd
&Scoped-define FIRST-EXTERNAL-TABLE quotehd


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR quotehd.
/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-qtchg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-qthd AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-qtitm AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-qtqty AS HANDLE NO-UNDO.
DEFINE VARIABLE h_changevalue AS HANDLE NO-UNDO.
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_options AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-qtitem AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-qtmisc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-qtnote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-qtqty AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-quote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_phone AS HANDLE NO-UNDO.
DEFINE VARIABLE h_printquo AS HANDLE NO-UNDO.
DEFINE VARIABLE h_smartmsg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-qthd AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-qtnote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-qthd AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-qtitm AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vi-qtqty AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vp-prmtx AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vp-qtrpc AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 150 BY 25.71
         BGCOLOR 15  .

DEFINE FRAME OPTIONS-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 1
         SIZE 148 BY 1.91
        BGCOLOR 15  .

DEFINE FRAME message-frame
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 112 ROW 2.91
         SIZE 39 BY 1.43
         BGCOLOR 15  .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   External Tables: asi.quotehd
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
         TITLE              = "Quote Maintenance"
         HEIGHT             = 25.71
         WIDTH              = 150
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
ON END-ERROR OF W-Win /* Quote Maintenance */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:

  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
  DEF VAR lQuoteitmExists AS LOG NO-UNDO.
   
  IF VALID-HANDLE(h_v-qthd) THEN DO:  
    RUN quoteitm-exists IN h_v-qthd (OUTPUT lQuoteitmExists, OUTPUT rQuoterow).
    IF NOT lQuoteitmExists THEN DO:
      MESSAGE "Please add an item to this quote or delete it"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Quote Maintenance */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  
  DEF VAR lQuoteitmExists AS LOG NO-UNDO.
  IF VALID-HANDLE(h_v-qthd) THEN DO:  
    RUN quoteitm-exists IN h_v-qthd (OUTPUT lQuoteitmExists, OUTPUT rQuoterow).
    IF NOT lQuoteitmExists THEN DO:
      MESSAGE "Please add an item to this quote or delete it"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
    END.
  END.
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
             INPUT  'smartobj/optionse.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_options ).
       RUN set-position IN h_options ( 1.00 , 69.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 63.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/smartmsg.w':U ,
             INPUT  FRAME message-frame:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_smartmsg ).
       RUN set-position IN h_smartmsg ( 1.00 , 8.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.14 , 32.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/printquo.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_printquo ).
       RUN set-position IN h_printquo ( 1.00 , 125.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/phone.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_phone ).
       RUN set-position IN h_phone ( 1.00 , 133.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Browse Quote|View Quote|Quantities|Prep/Misc Chg|Notes' + ',
                     FOLDER-TAB-TYPE = 1':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 3.14 , 2.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 23.57 , 148.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/exit.w':U ,
             INPUT  FRAME OPTIONS-FRAME:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.00 , 141.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.80 ) */

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_printquo ,
             h_options , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_phone ,
             h_printquo , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_folder ,
             FRAME message-frame:HANDLE , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_exit ,
             h_phone , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/b-qthd.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-qthd ).
       RUN set-position IN h_b-qthd ( 4.81 , 7.00 ) NO-ERROR.
       RUN set-size IN h_b-qthd ( 19.52 , 138.00 ) NO-ERROR.

       /* Links to SmartNavBrowser h_b-qthd. */
       RUN add-link IN adm-broker-hdl ( h_b-qthd , 'Record':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-qthd ,
             h_folder , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-qthd.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_v-qthd ).
       RUN set-position IN h_v-qthd ( 4.57 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 10.48 , 143.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/changevalue.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_changevalue ).
       RUN set-position IN h_changevalue ( 15.05 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.67 , 34.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-quote.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-quote ).
       RUN set-position IN h_p-quote ( 15.05 , 41.00 ) NO-ERROR.
       RUN set-size IN h_p-quote ( 1.57 , 60.20 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/vp-prmtx.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vp-prmtx ).
       RUN set-position IN h_vp-prmtx ( 15.14 , 103.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.43 , 44.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/b-qtitm.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-qtitm ).
       RUN set-position IN h_b-qtitm ( 16.71 , 17.00 ) NO-ERROR.
       RUN set-size IN h_b-qtitm ( 7.86 , 130.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-qtitem.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-qtitem ).
       RUN set-position IN h_p-qtitem ( 16.95 , 4.00 ) NO-ERROR.
       RUN set-size IN h_p-qtitem ( 7.62 , 12.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_v-qthd. */
       RUN add-link IN adm-broker-hdl ( h_b-qthd , 'Record':U , h_v-qthd ).
       RUN add-link IN adm-broker-hdl ( h_p-quote , 'TableIO':U , h_v-qthd ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'addPlusButton':U , h_v-qthd ).

       /* Links to SmartViewer h_changevalue. */
       RUN add-link IN adm-broker-hdl ( h_b-qthd , 'changeValue':U , h_changevalue ).

       /* Links to SmartViewer h_vp-prmtx. */
       RUN add-link IN adm-broker-hdl ( h_v-qthd , 'Record':U , h_vp-prmtx ).

       /* Links to SmartNavBrowser h_b-qtitm. */
       RUN add-link IN adm-broker-hdl ( h_p-qtitem , 'TableIO':U , h_b-qtitm ).
       RUN add-link IN adm-broker-hdl ( h_v-qthd , 'bottom':U , h_b-qtitm ).
       RUN add-link IN adm-broker-hdl ( h_v-qthd , 'Record':U , h_b-qtitm ).
       RUN add-link IN adm-broker-hdl ( h_b-qtitm , 'itemqt':U , h_b-qthd ).
       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-qthd ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_changevalue ,
             h_v-qthd , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-quote ,
             h_changevalue , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_vp-prmtx ,
             h_p-quote , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-qtitm ,
             h_vp-prmtx , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-qtitem ,
             h_b-qtitm , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/vi-qtitm.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-qtitm ).
       RUN set-position IN h_vi-qtitm ( 4.81 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/b-qtqty.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-qtqty ).
       RUN set-position IN h_b-qtqty ( 7.19 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 11.43 , 143.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-qtqty.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-qtqty ).
       RUN set-position IN h_p-qtqty ( 19.81 , 20.00 ) NO-ERROR.
       RUN set-size IN h_p-qtqty ( 1.91 , 94.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/vp-qtrpc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vp-qtrpc ).
       RUN set-position IN h_vp-qtrpc ( 19.81 , 114.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 17.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2':U) NO-ERROR.

       /* Links to SmartViewer h_vi-qtitm. */
       RUN add-link IN adm-broker-hdl ( h_b-qtitm , 'Record':U , h_vi-qtitm ).

       /* Links to SmartNavBrowser h_b-qtqty. */
       RUN add-link IN adm-broker-hdl ( h_b-qtitm , 'Record':U , h_b-qtqty ).
       RUN add-link IN adm-broker-hdl ( h_p-qtqty , 'TableIO':U , h_b-qtqty ).

       /* Links to SmartViewer h_vp-qtrpc. */
       RUN add-link IN adm-broker-hdl ( h_b-qtqty , 'Record':U , h_vp-qtrpc ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_vi-qtitm ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-qtqty ,
             h_vi-qtitm , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-qtqty ,
             h_b-qtqty , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_vp-qtrpc ,
             h_p-qtqty , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/vi-qtqty.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-qtqty ).
       RUN set-position IN h_vi-qtqty ( 5.29 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/b-qtchg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = Yes':U ,
             OUTPUT h_b-qtchg ).
       RUN set-position IN h_b-qtchg ( 7.91 , 3.00 ) NO-ERROR.
       RUN set-size IN h_b-qtchg ( 10.95 , 146.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-qtmisc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-qtmisc ).
       RUN set-position IN h_p-qtmisc ( 19.81 , 30.00 ) NO-ERROR.
       RUN set-size IN h_p-qtmisc ( 2.38 , 94.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('3':U) NO-ERROR.

       /* Links to SmartViewer h_vi-qtqty. */
       RUN add-link IN adm-broker-hdl ( h_b-qtqty , 'Record':U , h_vi-qtqty ).

       /* Links to SmartBrowser h_b-qtchg. */
       RUN add-link IN adm-broker-hdl ( h_b-qtqty , 'Record':U , h_b-qtchg ).
       RUN add-link IN adm-broker-hdl ( h_p-qtmisc , 'TableIO':U , h_b-qtchg ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_vi-qtqty ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-qtchg ,
             h_vi-qtqty , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-qtmisc ,
             h_b-qtchg , 'AFTER':U ).
    END. /* Page 4 */
    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/vi-qthd.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vi-qthd ).
       RUN set-position IN h_vi-qthd ( 5.29 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 144.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/v-qtnote.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-qtnote ).
       RUN set-position IN h_v-qtnote ( 7.91 , 10.00 ) NO-ERROR.
       /* Size in UIB:  ( 6.43 , 119.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panels/p-qtnote.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-qtnote ).
       RUN set-position IN h_p-qtnote ( 15.29 , 50.00 ) NO-ERROR.
       RUN set-size IN h_p-qtnote ( 2.38 , 48.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartViewer h_vi-qthd. */
       RUN add-link IN adm-broker-hdl ( h_b-qthd , 'Record':U , h_vi-qthd ).

       /* Links to SmartViewer h_v-qtnote. */
       RUN add-link IN adm-broker-hdl ( h_b-qthd , 'Record':U , h_v-qtnote ).
       RUN add-link IN adm-broker-hdl ( h_p-qtnote , 'TableIO':U , h_v-qtnote ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'copy-comment':U , h_v-qtnote ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_vi-qthd ,
             h_folder , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-qtnote ,
             h_vi-qthd , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-qtnote ,
             h_v-qtnote , 'AFTER':U ).
    END. /* Page 5 */

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
  {src/adm/template/row-list.i "quotehd"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "quotehd"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-change-page W-Win 
PROCEDURE local-change-page :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lQuoteitmExists AS LOG NO-UNDO.
   
  DEF VAR adm-current-page AS INT NO-UNDO.
     RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).
  /* Code placed here will execute PRIOR to standard behavior. */
  IF VALID-HANDLE(h_v-qthd) AND adm-current-page NE 2 THEN DO:     
    RUN quoteitm-exists IN h_v-qthd (OUTPUT lQuoteitmExists, OUTPUT rQuoterow).
    IF NOT lQuoteitmExists THEN DO:
      MESSAGE "Please add an item to this quote or delete it"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RUN select-page IN THIS-PROCEDURE ( 2 ).
    END.
  END.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'change-page':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/winReSizePgChg.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy W-Win 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lQuoteitmExists AS LOG NO-UNDO.
  

  rQuoteRow = ?.
  IF VALID-HANDLE(h_v-qthd) THEN DO:      
    RUN quoteitm-exists IN h_v-qthd (OUTPUT lQuoteitmExists, OUTPUT rQuoterow).
    IF NOT lQuoteitmExists THEN DO:
      MESSAGE "Note: This quote will be deleted since it contains no items."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
     
    END.
  END.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF lQuoteItmExists EQ NO AND rQuoteRow NE ? THEN DO:
    FIND quotehd WHERE ROWID(quotehd) EQ rQuoteRow EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL quotehd THEN DO:
      DELETE quotehd.
    END.
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

  DEF VAR lQuoteitmExists AS LOG NO-UNDO.
   
  IF VALID-HANDLE(h_v-qthd) THEN DO:  
    RUN quoteitm-exists IN h_v-qthd (OUTPUT lQuoteitmExists, OUTPUT rQuoterow).
    IF NOT lQuoteitmExists THEN DO:
      MESSAGE "Please add an item to this quote or delete it"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
    END.
  END.
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveToTop W-Win 
PROCEDURE moveToTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printQuo W-Win 
PROCEDURE printQuo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT AVAILABLE quotehd THEN RETURN.

  IF NOT VALID-HANDLE(h_v-qthd) THEN DO:
     RUN INIT-pages /*select-page*/ (2). 
  END.
  RUN print-quote IN h_v-qthd.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresh-quantities W-Win 
PROCEDURE refresh-quantities :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF VALID-HANDLE(h_b-qtqty) THEN
  RUN dispatch IN h_b-qtqty ( INPUT 'open-query':U ).
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
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
  
  RUN select-page(2).
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,'addPlusButton-target',OUTPUT char-hdl).
  RUN addPlusButton IN WIDGET-HANDLE(char-hdl).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* Compiler says this is defined elsewhere
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select_phone W-Win 
PROCEDURE select_phone :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lv-cust-rowid AS ROWID NO-UNDO.

RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
RUN GET-cust-rowid IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-cust-rowid).

IF lv-cust-rowid <> ? THEN FIND cust WHERE ROWID(cust) = lv-cust-rowid NO-LOCK NO-ERROR.
IF NOT AVAIL cust THEN RETURN.
/*
RUN Get_Procedure IN Persistent-Handle ('phone.',OUTPUT run-proc,no). 
IF run-proc NE '' THEN 
     /* {methods/smartrun.i cust.rec_key, {methods/headers/cust.i}}.*/
*/
RUN windows/phone2.w (cust.rec_key,{methods/headers/cust.i} ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
*/
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
  {src/adm/template/snd-list.i "quotehd"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE switchTabs W-Win 
PROCEDURE switchTabs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Prevent error message of buffer being different than result list */
RUN select-page IN THIS-PROCEDURE ( 1 ).
RUN select-page IN THIS-PROCEDURE ( 2 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

