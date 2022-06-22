&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: sharpshooter/w-createLoadtag.w

  Description: Creates a load tag from different sources

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
{api/inbound/ttItem.i}
DEFINE TEMP-TABLE ttPOLine NO-UNDO
    LIKE po-ordl
    FIELD   quantity            AS INTEGER     
    FIELD   quantityInSubUnit   AS INTEGER
    FIELD   subUnitsPerUnit     AS INTEGER
    .
    
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUser    AS CHARACTER NO-UNDO.

/* Required for run_link.i */
DEFINE VARIABLE char-hdl  AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle   AS HANDLE    NO-UNDO.
 
DEFINE VARIABLE oLoadTag  AS Inventory.Loadtag  NO-UNDO.
DEFINE VARIABLE oKeyboard AS system.Keyboard    NO-UNDO.

DEFINE VARIABLE glShowVirtualKeyboard AS LOGICAL   NO-UNDO.
DEFINE VARIABLE gcShowSettings        AS CHARACTER NO-UNDO.

RUN spGetSessionParam ("Company", OUTPUT cCompany).
    
oKeyboard = NEW system.Keyboard().

RUN spSetSettingContext.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btPrint btDelete btJob btPO btRelease ~
btReturn btReprint btSplit btBOL statusMessage btnExitText btnSettingsText ~
btnDeleteText btnPrintText btnNumPad rHighlight btClear btnClearText 
&Scoped-Define DISPLAYED-OBJECTS statusMessage btnExitText btnSettingsText ~
btnDeleteText btnPrintText btnClearText 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_adjustwindowsize AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-loadtags-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_f-boltag AS HANDLE NO-UNDO.
DEFINE VARIABLE h_f-job AS HANDLE NO-UNDO.
DEFINE VARIABLE h_f-poprint AS HANDLE NO-UNDO.
DEFINE VARIABLE h_f-releasetag AS HANDLE NO-UNDO.
DEFINE VARIABLE h_f-reprint AS HANDLE NO-UNDO.
DEFINE VARIABLE h_f-return AS HANDLE NO-UNDO.
DEFINE VARIABLE h_f-splittag AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatefirst AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatelast AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatenext AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigateprev AS HANDLE NO-UNDO.
DEFINE VARIABLE h_setting AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btBOL 
     LABEL "BOL" 
     SIZE 22.4 BY 1.43.

DEFINE BUTTON btClear 
     IMAGE-UP FILE "Graphics/32x32/back_white.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/back_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset" 
     SIZE 8 BY 1.91.

DEFINE BUTTON btDelete 
     IMAGE-UP FILE "Graphics/32x32/garbage_can.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Delete" 
     SIZE 8 BY 1.91 TOOLTIP "Delete currently selected record".

DEFINE BUTTON btJob 
     LABEL "JOB" 
     SIZE 22.4 BY 1.43.

DEFINE BUTTON btnNumPad 
     IMAGE-UP FILE "Graphics/32x32/numeric_keypad.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "NumPad" 
     SIZE 8 BY 1.91 TOOLTIP "Numeric Keypad".

DEFINE BUTTON btPO 
     LABEL "PO" 
     SIZE 22.4 BY 1.43.

DEFINE BUTTON btPrint 
     IMAGE-UP FILE "Graphics/32x32/print_new.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Print" 
     SIZE 8 BY 1.91.

DEFINE BUTTON btRelease 
     LABEL "RELEASE" 
     SIZE 22.4 BY 1.43.

DEFINE BUTTON btReprint 
     LABEL "RE-PRINT" 
     SIZE 22.4 BY 1.43.

DEFINE BUTTON btReturn 
     LABEL "RETURN" 
     SIZE 22.4 BY 1.43.

DEFINE BUTTON btSplit 
     LABEL "SPLIT" 
     SIZE 22.4 BY 1.43.

DEFINE VARIABLE btnClearText AS CHARACTER FORMAT "X(256)":U INITIAL "RESET" 
      VIEW-AS TEXT 
     SIZE 12 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnDeleteText AS CHARACTER FORMAT "X(256)":U INITIAL "DELETE" 
      VIEW-AS TEXT 
     SIZE 15 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnExitText AS CHARACTER FORMAT "X(256)":U INITIAL "EXIT" 
      VIEW-AS TEXT 
     SIZE 8 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnPrintText AS CHARACTER FORMAT "X(256)":U INITIAL "PRINT" 
      VIEW-AS TEXT 
     SIZE 12 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnSettingsText AS CHARACTER FORMAT "X(256)":U INITIAL "SETTINGS" 
      VIEW-AS TEXT 
     SIZE 18 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE statusMessage AS CHARACTER FORMAT "X(256)":U INITIAL "STATUS MESSAGE" 
      VIEW-AS TEXT 
     SIZE 116 BY 1.43 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 10 BY 2.38
     BGCOLOR 12 .

DEFINE RECTANGLE rHighlight
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 24.4 BY 1.81
     BGCOLOR 15 FGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btPrint AT ROW 31.95 COL 201 WIDGET-ID 30
     btDelete AT ROW 31.95 COL 17 WIDGET-ID 62
     btJob AT ROW 2.62 COL 3 WIDGET-ID 2 NO-TAB-STOP 
     btPO AT ROW 2.62 COL 27.4 WIDGET-ID 24 NO-TAB-STOP 
     btRelease AT ROW 2.62 COL 99.2 WIDGET-ID 48 NO-TAB-STOP 
     btReturn AT ROW 2.62 COL 146.8 WIDGET-ID 50 NO-TAB-STOP 
     btReprint AT ROW 2.62 COL 75.4 WIDGET-ID 52 NO-TAB-STOP 
     btSplit AT ROW 2.62 COL 51.4 WIDGET-ID 54 NO-TAB-STOP 
     btBOL AT ROW 2.62 COL 123 WIDGET-ID 64 NO-TAB-STOP 
     statusMessage AT ROW 32.19 COL 30 COLON-ALIGNED NO-LABEL
     btnExitText AT ROW 1.24 COL 191 COLON-ALIGNED NO-LABEL
     btnSettingsText AT ROW 32.19 COL 160 COLON-ALIGNED NO-LABEL
     btnDeleteText AT ROW 32.19 COL 2 NO-LABEL
     btnPrintText AT ROW 32.19 COL 187 COLON-ALIGNED NO-LABEL
     btnNumPad AT ROW 1.91 COL 176 WIDGET-ID 120 NO-TAB-STOP 
     btClear AT ROW 3.19 COL 194.8 WIDGET-ID 146
     btnClearText AT ROW 3.38 COL 182 NO-LABEL WIDGET-ID 148
     RECT-2 AT ROW 1.71 COL 175 WIDGET-ID 130
     rHighlight AT ROW 2.43 COL 2 WIDGET-ID 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 234 BY 33
         BGCOLOR 21 FGCOLOR 15 FONT 38 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Create LoadTag"
         HEIGHT             = 33
         WIDTH              = 234
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
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
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN btnClearText IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN btnDeleteText IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Create LoadTag */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Create LoadTag */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btBOL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btBOL W-Win
ON CHOOSE OF btBOL IN FRAME F-Main /* BOL */
DO:
    IF VALID-OBJECT (oKeyboard) THEN
        oKeyboard:CloseKeyboard().

    RUN select-page(7).

    rHighlight:X = SELF:X - 5.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btClear W-Win
ON CHOOSE OF btClear IN FRAME F-Main /* Reset */
DO:
    DEFINE VARIABLE iCurrentPage AS INTEGER NO-UNDO.
    
    RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
    
    iCurrentPage = INTEGER(RETURN-VALUE).
        
    RUN pStatusMessage ("", 0).
    
    CASE iCurrentPage:
        WHEN 1 THEN
            {methods/run_link.i "JOB-SOURCE" "Reset"}
        WHEN 2 THEN
            {methods/run_link.i "PO-SOURCE" "Reset"}
        WHEN 3 THEN
            {methods/run_link.i "RELEASE-SOURCE" "Reset"}
        WHEN 4 THEN
            {methods/run_link.i "RETURN-SOURCE" "Reset"}
        WHEN 5 THEN
            {methods/run_link.i "REPRINT-SOURCE" "Reset"}
        WHEN 6 THEN
            {methods/run_link.i "SPLIT-SOURCE" "Reset"}
        WHEN 7 THEN
            {methods/run_link.i "BOL-SOURCE" "Reset"}
    END CASE.
    
    {methods/run_link.i "LOADTAG-SOURCE" "EmptyTTLoadtag"} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDelete W-Win
ON CHOOSE OF btDelete IN FRAME F-Main /* Delete */
DO:
    {methods/run_link.i "LOADTAG-SOURCE" "DeleteSelected"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btJob W-Win
ON CHOOSE OF btJob IN FRAME F-Main /* JOB */
DO:
    IF VALID-OBJECT (oKeyboard) THEN
        oKeyboard:CloseKeyboard().
        
    RUN select-page(1).

    rHighlight:X = SELF:X - 5.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearText W-Win
ON MOUSE-SELECT-CLICK OF btnClearText IN FRAME F-Main
DO:
    APPLY "CHOOSE" TO btClear.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDeleteText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeleteText W-Win
ON MOUSE-SELECT-CLICK OF btnDeleteText IN FRAME F-Main
DO:
  APPLY "CHOOSE":U TO btDelete.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExitText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExitText W-Win
ON MOUSE-SELECT-CLICK OF btnExitText IN FRAME F-Main
DO:
    RUN dispatch ("exit").
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNumPad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNumPad W-Win
ON CHOOSE OF btnNumPad IN FRAME F-Main /* NumPad */
DO:
    ASSIGN
        oKeyboard:DisplayKeyboard = NOT oKeyboard:DisplayKeyboard
        RECT-2:BGCOLOR = IF oKeyboard:DisplayKeyboard THEN 10 ELSE 12
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrintText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrintText W-Win
ON MOUSE-SELECT-CLICK OF btnPrintText IN FRAME F-Main
DO:
    APPLY "CHOOSE":U TO btPrint.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSettingsText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSettingsText W-Win
ON MOUSE-SELECT-CLICK OF btnSettingsText IN FRAME F-Main
DO:
    RUN OpenSetting.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPO W-Win
ON CHOOSE OF btPO IN FRAME F-Main /* PO */
DO:
    IF VALID-OBJECT (oKeyboard) THEN
        oKeyboard:CloseKeyboard().

    RUN select-page(2).
    
    rHighlight:X = SELF:X - 5.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPrint W-Win
ON CHOOSE OF btPrint IN FRAME F-Main /* Print */
DO:
    RUN state-changed (
        INPUT THIS-PROCEDURE,
        INPUT "print-tags"
        ).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btRelease
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btRelease W-Win
ON CHOOSE OF btRelease IN FRAME F-Main /* RELEASE */
DO:
    IF VALID-OBJECT (oKeyboard) THEN
        oKeyboard:CloseKeyboard().

    RUN select-page(3).

    rHighlight:X = SELF:X - 5.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btReprint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btReprint W-Win
ON CHOOSE OF btReprint IN FRAME F-Main /* RE-PRINT */
DO:
    IF VALID-OBJECT (oKeyboard) THEN
        oKeyboard:CloseKeyboard().

    RUN select-page(5).
    
    rHighlight:X = SELF:X - 5.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btReturn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btReturn W-Win
ON CHOOSE OF btReturn IN FRAME F-Main /* RETURN */
DO:
    IF VALID-OBJECT (oKeyboard) THEN
        oKeyboard:CloseKeyboard().

    RUN select-page(4).

    rHighlight:X = SELF:X - 5.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSplit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSplit W-Win
ON CHOOSE OF btSplit IN FRAME F-Main /* SPLIT */
DO:
    IF VALID-OBJECT (oKeyboard) THEN
        oKeyboard:CloseKeyboard().

    RUN select-page(6).

    rHighlight:X = SELF:X - 5.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

{sharpshooter/pStatusMessage.i}
{sharpshooter/ChangeWindowSize.i}

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
             INPUT  'sharpshooter/smartobj/adjustwindowsize.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_adjustwindowsize ).
       RUN set-position IN h_adjustwindowsize ( 1.00 , 183.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 32.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/b-loadtags.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-loadtags-3 ).
       RUN set-position IN h_b-loadtags-3 ( 24.10 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-loadtags-3 ( 7.86 , 199.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/exit.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.00 , 200.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/setting.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_setting ).
       RUN set-position IN h_setting ( 32.14 , 181.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.60 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartBrowser h_b-loadtags-3. */
       RUN add-link IN adm-broker-hdl ( h_navigatefirst , 'NAV-FIRST':U , h_b-loadtags-3 ).
       RUN add-link IN adm-broker-hdl ( h_navigatelast , 'NAV-LAST':U , h_b-loadtags-3 ).
       RUN add-link IN adm-broker-hdl ( h_navigatenext , 'NAV-NEXT':U , h_b-loadtags-3 ).
       RUN add-link IN adm-broker-hdl ( h_navigateprev , 'NAV-PREV':U , h_b-loadtags-3 ).
       RUN add-link IN adm-broker-hdl ( h_b-loadtags-3 , 'LOADTAG':U , THIS-PROCEDURE ).

       /* Links to SmartObject h_setting. */
       RUN add-link IN adm-broker-hdl ( h_setting , 'Setting':U , THIS-PROCEDURE ).

    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigateprev.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigateprev ).
       RUN set-position IN h_navigateprev ( 26.95 , 201.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/f-job.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_f-job ).
       RUN set-position IN h_f-job ( 4.57 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 15.24 , 221.80 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatefirst.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatefirst ).
       RUN set-position IN h_navigatefirst ( 25.05 , 201.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatelast.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatelast ).
       RUN set-position IN h_navigatelast ( 30.76 , 201.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatenext.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatenext ).
       RUN set-position IN h_navigatenext ( 28.86 , 201.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       /* Links to SmartFrame h_f-job. */
       RUN add-link IN adm-broker-hdl ( h_f-job , 'JOB':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_f-job , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigateprev ,
             h_setting , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatefirst ,
             h_f-job , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatelast ,
             h_navigatefirst , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatenext ,
             h_navigatelast , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/f-poprint.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_f-poprint ).
       RUN set-position IN h_f-poprint ( 4.57 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 19.38 , 151.60 ) */

       /* Links to SmartFrame h_f-poprint. */
       RUN add-link IN adm-broker-hdl ( h_f-poprint , 'PO':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_f-poprint , 'State':U , THIS-PROCEDURE ).

    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/f-releasetag.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_f-releasetag ).
       RUN set-position IN h_f-releasetag ( 4.43 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.52 , 65.40 ) */

       /* Links to SmartFrame h_f-releasetag. */
       RUN add-link IN adm-broker-hdl ( h_f-releasetag , 'RELEASE':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_f-releasetag , 'State':U , THIS-PROCEDURE ).

    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/f-return.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_f-return ).
       RUN set-position IN h_f-return ( 4.33 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 7.00 , 126.00 ) */

       /* Links to SmartFrame h_f-return. */
       RUN add-link IN adm-broker-hdl ( h_f-return , 'RETURN':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_f-return , 'State':U , THIS-PROCEDURE ).

    END. /* Page 4 */
    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/f-reprint.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_f-reprint ).
       RUN set-position IN h_f-reprint ( 4.57 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.91 , 88.40 ) */

       /* Links to SmartFrame h_f-reprint. */
       RUN add-link IN adm-broker-hdl ( h_f-reprint , 'REPRINT':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_f-reprint , 'State':U , THIS-PROCEDURE ).

    END. /* Page 5 */
    WHEN 6 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/f-splittag.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_f-splittag ).
       RUN set-position IN h_f-splittag ( 4.33 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 6.95 , 113.60 ) */

       /* Links to SmartFrame h_f-splittag. */
       RUN add-link IN adm-broker-hdl ( h_f-splittag , 'SPLIT':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_f-splittag , 'State':U , THIS-PROCEDURE ).

    END. /* Page 6 */
    WHEN 7 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/f-boltag.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_f-boltag ).
       RUN set-position IN h_f-boltag ( 4.57 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.29 , 61.40 ) */

       /* Links to SmartFrame h_f-boltag. */
       RUN add-link IN adm-broker-hdl ( h_f-boltag , 'BOL':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_f-boltag , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 7 */

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
  DISPLAY statusMessage btnExitText btnSettingsText btnDeleteText btnPrintText 
          btnClearText 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE btPrint btDelete btJob btPO btRelease btReturn btReprint btSplit btBOL 
         statusMessage btnExitText btnSettingsText btnDeleteText btnPrintText 
         btnNumPad rHighlight btClear btnClearText 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetConfig W-Win 
PROCEDURE GetConfig :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opoConfig AS system.Config NO-UNDO.
    
    DEFINE VARIABLE iCurrentPage AS INTEGER NO-UNDO.
    
    RUN get-attribute IN THIS-PROCEDURE (
        'Current-Page':U
        ).
    
    iCurrentPage = INTEGER(RETURN-VALUE).
    
    CASE iCurrentPage:
        WHEN 1 THEN
            opoConfig = system.ConfigLoader:Instance:GetConfig("SSLoadTag").
        
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetDesignConfig W-Win 
PROCEDURE GetDesignConfig :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opoDesignConfig AS system.Config NO-UNDO.
    
    DEFINE VARIABLE iCurrentPage AS INTEGER NO-UNDO.
    
    RUN get-attribute IN THIS-PROCEDURE (
        'Current-Page':U
        ).
    
    iCurrentPage = INTEGER(RETURN-VALUE).
    
    CASE iCurrentPage:
        WHEN 1 THEN
            opoDesignConfig = system.ConfigLoader:Instance:GetConfig("SSLoadTagJobDesign").
        
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetKeyboard W-Win 
PROCEDURE GetKeyboard :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opoKeyboard AS system.Keyboard NO-UNDO.

    opoKeyboard = oKeyboard.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Key_Stroke W-Win 
PROCEDURE Key_Stroke :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcKeyStroke AS CHARACTER NO-UNDO.
    
    IF VALID-OBJECT (oKeyboard) THEN
        oKeyboard:KeyStroke(ipcKeyStroke).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy W-Win 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */
  IF VALID-OBJECT(oKeyboard) THEN
      DELETE OBJECT oKeyboard.
      
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable W-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    RUN pWinReSize.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUN pInit.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenSetting W-Win 
PROCEDURE OpenSetting :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN windows/setting-dialog.w.    
    {sharpshooter/settingChangeDialog.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBOLTagPrint W-Win 
PROCEDURE pBOLTagPrint :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iBOLID   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCopies  AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE oBOLHeader AS bol.BOLHeader NO-UNDO.
    
    {methods/run_link.i "BOL-SOURCE" "GetBOLHeader" "(OUTPUT oBOLHeader)"}
    
    IF VALID-OBJECT(oBOLHeader) THEN DO:
        ASSIGN
            cCompany = oBOLHeader:GetValue("Company")
            iBOLID   = INTEGER(oBOLHeader:GetValue("BOLID"))
            .
    
        {methods/run_link.i "LOADTAG-SOURCE" "BuildLoadTagsFromBOL" "(INPUT cCompany, INPUT iBOLID, INPUT 1)"}
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateTagFromJob W-Win 
PROCEDURE pCreateTagFromJob PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cJobNo             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iJobNo2            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iFormNo            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iBlankNo           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cItemID            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustID            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iQuantity          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iQuantityInSubUnit AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iSubUnitsPerUnit   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCopies            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cUserField1        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserField2        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserField3        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserFieldValue1   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserFieldValue2   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserFieldValue3   AS CHARACTER NO-UNDO.
        
    {methods/run_link.i "JOB-SOURCE" "GetDetails" "(OUTPUT cJobNo,OUTPUT iJobNo2,OUTPUT iFormNo,OUTPUT iBlankNo,OUTPUT cItemID,OUTPUT cCustID,OUTPUT iQuantity,OUTPUT iQuantityInSubUnit,OUTPUT iSubUnitsPerUnit,OUTPUT iCopies,OUTPUT cUserField1,OUTPUT cUserField2,OUTPUT cUserField3,OUTPUT cUserFieldValue1,OUTPUT cUserFieldValue2,OUTPUT cUserFieldValue3)"}

    {methods/run_link.i "LOADTAG-SOURCE" "BuildLoadTagsFromJob" "(INPUT cCompany, INPUT cJobNo, INPUT iJobNo2, INPUT iFormNo, INPUT iBlankNo, INPUT cItemID, INPUT iQuantity, INPUT iQuantityInSubUnit, INPUT iSubUnitsPerUnit, INPUT iCopies, INPUT cUserField1, INPUT cUserField2, INPUT cUserField3, INPUT cUserFieldValue1, INPUT cUserFieldValue2, INPUT cUserFieldValue3)" }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit W-Win 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cReturnValue  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cSettingValue AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN spGetSessionParam("UserID", OUTPUT cUser).
    RUN pStatusMessage ("", 0).
    
    RUN spGetSettingByName ("ShowVirtualKeyboard", OUTPUT cSettingValue).
    glShowVirtualKeyboard = LOGICAL(cSettingValue) NO-ERROR.
    
    RUN spGetSettingByName ("ShowSettings", OUTPUT gcShowSettings).

    oKeyboard:SetWindow({&WINDOW-NAME}:HANDLE).
    oKeyboard:SetProcedure(THIS-PROCEDURE).
    oKeyboard:SetFrame(FRAME {&FRAME-NAME}:HANDLE).

    ASSIGN
        btnSettingsText:VISIBLE = INDEX(gcShowSettings, "Text") GT 0
        btnNumPad:VISIBLE       = glShowVirtualKeyboard
        RECT-2:VISIBLE          = glShowVirtualKeyboard
        .  
    
    IF INDEX(gcShowSettings, "Icon") EQ 0 THEN
        {methods/run_link.i "Setting-SOURCE" "HideSettings"}        
        
    {methods/run_link.i "JOB-SOURCE" "ValidateSameJobScan" "(FALSE)"}    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPOTagPrint W-Win 
PROCEDURE pPOTagPrint PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cCompany           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iPOID              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iPOLine            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iQuantity          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cQuantityUOM       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError             AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage           AS CHARACTER NO-UNDO.
    
    {methods/run_link.i "PO-SOURCE" "GetPOLineTT" "(OUTPUT TABLE ttPOLine)"}
    
    FOR EACH ttPOLine:
        {methods/run_link.i "LOADTAG-SOURCE" "BuildLoadTagsFromPO" "(INPUT ttPOLine.company, INPUT ttPOLine.po-no, INPUT ttPOLine.line, INPUT ttPOLine.quantity, INPUT ttPOLine.quantityInSubUnit, INPUT ttPOLine.subUnitsPerUnit, INPUT ttPOLine.pr-qty-uom, INPUT 1, OUTPUT lError, OUTPUT cMessage)"}
        IF lError THEN
            LEAVE.
    END.
    
    IF lError THEN
        RUN pStatusMessage(INPUT cMessage, INPUT 3).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReleaseTagPrint W-Win 
PROCEDURE pReleaseTagPrint :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iReleaseID AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCopies    AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE oReleaseHeader AS oe.ReleaseHeader NO-UNDO.
    
    {methods/run_link.i "RELEASE-SOURCE" "GetRelease" "(OUTPUT oReleaseHeader)"}

    IF VALID-OBJECT(oReleaseHeader) THEN DO:
        ASSIGN
            cCompany   = oReleaseHeader:GetValue("Company")
            iReleaseID = INTEGER(oReleaseHeader:GetValue("ReleaseID"))
            .

        {methods/run_link.i "LOADTAG-SOURCE" "BuildLoadTagsFromRelease" "(INPUT cCompany, INPUT iReleaseID, INPUT 1)"}
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReprintTag W-Win 
PROCEDURE pReprintTag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTag     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
    
    {methods/run_link.i "REPRINT-SOURCE" "GetTag" "(OUTPUT oLoadtag)"}

    IF VALID-OBJECT(oLoadtag) THEN DO:
        ASSIGN
            cCompany = oLoadTag:GetValue("Company")
            cTag     = oLoadTag:GetValue("Tag")
            .

        {methods/run_link.i "LOADTAG-SOURCE" "BuildLoadTagsFromTag" "(INPUT cCompany, INPUT cTag, INPUT 1)"}         
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReturnTag W-Win 
PROCEDURE pReturnTag :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE hdInventoryProcs AS HANDLE    NO-UNDO.
    DEFINE VARIABLE dReturnQuantity  AS DECIMAL   NO-UNDO.
    
    DEFINE VARIABLE lError           AS LOG.
    DEFINE VARIABLE cMessage         AS CHARACTER.
    DEFINE VARIABLE cTag             AS CHARACTER.
    
    {methods/run_link.i "RETURN-SOURCE" "GetTag" "(OUTPUT oLoadtag)"}   

    IF VALID-OBJECT(oLoadTag) THEN 
    DO:        
        {methods/run_link.i "RETURN-SOURCE" "GetQuantity" "(OUTPUT dReturnQuantity)"}   
    
        ASSIGN
            cCompany = oLoadtag:GetValue("Company")
            cTag     = oLoadtag:GetValue("Tag")
            .   

        RUN inventory/inventoryprocs.p PERSISTENT SET hdInventoryProcs.
    
        RUN Inventory_CreateReturnFromTag IN hdInventoryProcs(
            INPUT  cCompany,
            INPUT  cTag,
            INPUT  dReturnQuantity,
            INPUT  TRUE,
            OUTPUT lError,
            OUTPUT cMessage
            ).

        DELETE PROCEDURE hdInventoryProcs.

        IF lError THEN
            RUN pStatusMessage (cMessage, 3).
        ELSE
            {methods/run_link.i "LOADTAG-SOURCE" "BuildLoadTagsFromTag" "(INPUT cCompany, INPUT cTag, INPUT 1)"}
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSplitTag W-Win 
PROCEDURE pSplitTag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTag              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemID           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dSplitQuantity    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cItemType         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess          AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage          AS CHARACTER NO-UNDO. 

    {methods/run_link.i "SPLIT-SOURCE" "GetTag" "(OUTPUT oLoadtag)"}   

    IF VALID-OBJECT(oLoadTag) THEN DO:        
        {methods/run_link.i "SPLIT-SOURCE" "GetQuantity" "(OUTPUT dSplitQuantity)"}   
    
        ASSIGN
            cCompany       = oLoadtag:GetValue("Company")
            cTag           = oLoadtag:GetValue("Tag")
            cItemID        = oLoadtag:GetValue("ItemID")
            cItemType      = STRING(LOGICAL(oLoadtag:GetValue("ItemType")), "RM/FG")
            .   

        RUN api\inbound\SplitTag.p (
            INPUT  cCompany, 
            INPUT  cTag,
            INPUT  cItemID,
            INPUT  dSplitQuantity,
            INPUT  cItemType,
            INPUT  cUser,
            OUTPUT lSuccess,
            OUTPUT cMessage,
            OUTPUT TABLE ttItem
            ) NO-ERROR. 

        IF NOT lSuccess THEN DO:
            RUN pStatusMessage (cMessage, 3). 
        END.    
        ELSE DO:
            {methods/run_link.i "LOADTAG-SOURCE" "BuildLoadTagsFromTag" "(INPUT cCompany, INPUT cTag, INPUT 1)"}
            FOR EACH ttItem:
                {methods/run_link.i "LOADTAG-SOURCE" "BuildLoadTagsFromTag" "(INPUT cCompany, INPUT ttItem.tag, INPUT 1)"}
            END.
        END.       
    END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWinReSize W-Win 
PROCEDURE pWinReSize :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dCol    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dColTmp AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dRow    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dHeight AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dWidth  AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE dScreenHeight AS INTEGER NO-UNDO.
    DEFINE VARIABLE dScreenWidth  AS INTEGER NO-UNDO.
    DEFINE VARIABLE dScreenTop    AS INTEGER NO-UNDO.
    DEFINE VARIABLE dScreenLeft   AS INTEGER NO-UNDO.

    SESSION:SET-WAIT-STATE("General").
    
    RUN spGetScreenWorkingAreaSize (OUTPUT dScreenWidth, OUTPUT dScreenHeight).
    RUN spGetScreenStartPosition (OUTPUT dScreenTop, OUTPUT dScreenLeft).

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            btPrint:ROW                          = {&WINDOW-NAME}:HEIGHT - 1.1
            btPrint:COL                          = {&WINDOW-NAME}:WIDTH  - btPrint:WIDTH - 1
            btnPrintText:ROW                     = {&WINDOW-NAME}:HEIGHT - .86
            btnPrintText:COL                     = btPrint:COL - btnPrintText:WIDTH - 1
            btDelete:ROW                         = {&WINDOW-NAME}:HEIGHT - 1.1
            btnDeleteText:ROW                    = {&WINDOW-NAME}:HEIGHT - .86
            statusMessage:ROW                    = {&WINDOW-NAME}:HEIGHT - .86
            dCol                                 = {&WINDOW-NAME}:WIDTH  - 8
            btnExitText:COL                      = dCol - 9
            btnSettingsText:ROW                  = {&WINDOW-NAME}:HEIGHT - .86
            btnSettingsText:COL                  = btnPrintText:COL - btnSettingsText:WIDTH - 10            
            btnClearText:COL                     = dCol - 12
            btClear:COL                          = dCol 
            .

        RUN set-position IN h_setting ( {&WINDOW-NAME}:HEIGHT - 1.1 , btnSettingsText:COL + 18 ) NO-ERROR.            
        RUN set-position IN h_exit ( 1.00 , dCol ) NO-ERROR.
        RUN get-position IN h_navigatefirst ( OUTPUT dRow , OUTPUT dColTmp ) NO-ERROR.
        RUN set-position IN h_navigatefirst ( dRow , dCol ) NO-ERROR.
        dRow = dRow + 1.9.
        RUN set-position IN h_navigateprev ( dRow , dCol ) NO-ERROR.
        dRow = dRow + 1.9.
        RUN set-position IN h_navigatenext ( dRow , dCol ) NO-ERROR.
        dRow = dRow + 1.9.
        RUN set-position IN h_navigatelast ( dRow , dCol ) NO-ERROR.
        RUN get-position IN h_b-loadtags-3 ( OUTPUT dRow , OUTPUT dColTmp ) NO-ERROR.
        ASSIGN
            dWidth  = dCol - 2
            dHeight = {&WINDOW-NAME}:HEIGHT - dRow - 1.33
            .
        RUN set-size IN h_b-loadtags-3 ( dHeight , dWidth ) NO-ERROR.
        RUN set-position IN h_adjustwindowsize ( 1.00 , dCol - 45 ) NO-ERROR.
    END. /* do with */
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Select_Exit W-Win 
PROCEDURE Select_Exit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.    
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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus W-Win 
PROCEDURE Set-Focus :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    {methods/run_link.i "JOB-SOURCE" "Set-Focus"}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShowKeyboard W-Win 
PROCEDURE ShowKeyboard :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplShowKeyboard AS LOGICAL NO-UNDO.

    oplShowKeyboard = glShowVirtualKeyboard.
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
    DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cStatusMessage     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iStatusMessageType AS INTEGER   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.    
    
    CASE p-state:
        WHEN "create-tags-job" THEN DO:
            RUN pCreateTagFromJob.
        END. 
        WHEN "print-tags" THEN DO:
            SESSION:SET-WAIT-STATE ("GENERAL").
            
            {methods/run_link.i "LOADTAG-SOURCE" "CreateLoadTagFromTT"}

            SESSION:SET-WAIT-STATE ("").
        END.
        WHEN "create-tag-reprint" THEN DO:
            RUN pReprintTag.
        END.
        WHEN "split-tag" THEN DO:
            RUN pSplitTag.
        END.
        WHEN "bol-tag-print" THEN DO:
            RUN pBOLTagPrint.
        END.
        WHEN "release-tag-print" THEN DO:
            RUN pReleaseTagPrint.
        END.
        WHEN "create-tag-po" THEN DO:
            RUN pPOTagPrint.
        END.
        WHEN "return-tag" THEN DO:
            RUN pReturnTag.
        END.
        WHEN "job-error" THEN DO:
            {methods/run_link.i "JOB-SOURCE" "GetMessageAndType" "(OUTPUT cStatusMessage, OUTPUT iStatusMessageType)"}
            
            RUN pStatusMessage (cStatusMessage, iStatusMessageType).
        END.
        WHEN "po-error" THEN DO:
            {methods/run_link.i "PO-SOURCE" "GetMessageAndType" "(OUTPUT cStatusMessage, OUTPUT iStatusMessageType)"}
            
            RUN pStatusMessage (cStatusMessage, iStatusMessageType).
        END.      
        WHEN "release-error" THEN DO:
            {methods/run_link.i "RELEASE-SOURCE" "GetMessageAndType" "(OUTPUT cStatusMessage, OUTPUT iStatusMessageType)"}
            
            RUN pStatusMessage (cStatusMessage, iStatusMessageType).
        END.                  
        WHEN "split-error" THEN DO:
            {methods/run_link.i "SPLIT-SOURCE" "GetMessageAndType" "(OUTPUT cStatusMessage, OUTPUT iStatusMessageType)"}
            
            RUN pStatusMessage (cStatusMessage, iStatusMessageType).
        END.                
        WHEN "reprint-error" THEN DO:
            {methods/run_link.i "REPRINT-SOURCE" "GetMessageAndType" "(OUTPUT cStatusMessage, OUTPUT iStatusMessageType)"}
            
            RUN pStatusMessage (cStatusMessage, iStatusMessageType).
        END.
        WHEN "return-error" THEN DO:
            {methods/run_link.i "RETURN-SOURCE" "GetMessageAndType" "(OUTPUT cStatusMessage, OUTPUT iStatusMessageType)"}
            
            RUN pStatusMessage (cStatusMessage, iStatusMessageType).
        END.        
        WHEN "bol-error" THEN DO:
            {methods/run_link.i "BOL-SOURCE" "GetMessageAndType" "(OUTPUT cStatusMessage, OUTPUT iStatusMessageType)"}
            
            RUN pStatusMessage (cStatusMessage, iStatusMessageType).
        END.        
        WHEN "empty-message" THEN DO:
            RUN pStatusMessage(INPUT "", INPUT 0).
        END.        
    END CASE.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

