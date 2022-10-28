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

  File: sharpshooter/w-tagStatusUpdate.w

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
{inventory/ttBrowseInventory.i}
{methods/template/brwcustomdef.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
 
DEFINE VARIABLE cCompany         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUser            AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdInventoryProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE cInventoryStatus AS CHARACTER NO-UNDO.

/* Required for run_link.i */
DEFINE VARIABLE char-hdl  AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle   AS HANDLE    NO-UNDO.

DEFINE VARIABLE cStatusMessage     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iStatusMessageType AS INTEGER   NO-UNDO.
DEFINE VARIABLE cUserID            AS CHARACTER NO-UNDO.

DEFINE VARIABLE gcShowSettings           AS CHARACTER NO-UNDO.
DEFINE VARIABLE glShowVirtualKeyboard    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glAllowOnHoldTagScan     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE gcDefaultTagScanStatusID AS CHARACTER NO-UNDO.

DEFINE VARIABLE oKeyboard AS system.Keyboard   NO-UNDO.
DEFINE VARIABLE oLoadtag  AS inventory.Loadtag NO-UNDO.
DEFINE VARIABLE oFGBin    AS fg.FGBin          NO-UNDO.

RUN spGetSessionParam ("Company", OUTPUT cCompany).
RUN spGetSessionParam ("UserID", OUTPUT cUserID).
    
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
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttBrowseInventory

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 ttBrowseInventory.quantity ttBrowseInventory.tag ttBrowseInventory.warehouseID ttBrowseInventory.jobID ttBrowseInventory.poID fGetInventoryStatus() @ cInventoryStatus   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH ttBrowseInventory BY ttBrowseInventory.lastTransTime DESCENDING
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH ttBrowseInventory BY ttBrowseInventory.lastTransTime DESCENDING.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 ttBrowseInventory
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 ttBrowseInventory


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnPrevious btClear btnFirst btnLast btnNext ~
BROWSE-1 btnNumPad btnExitText btnClearText btnSettingsText statusMessage 
&Scoped-Define DISPLAYED-OBJECTS fiTagStatus fiTagStatusDescription ~
btnExitText btnClearText btnSettingsText statusMessage 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetInventoryStatus W-Win 
FUNCTION fGetInventoryStatus RETURNS CHARACTER
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_adjustwindowsize AS HANDLE NO-UNDO.
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_setting AS HANDLE NO-UNDO.
DEFINE VARIABLE h_tagfilter AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btClear 
     IMAGE-UP FILE "Graphics/32x32/back_white.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/back_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset" 
     SIZE 8 BY 1.91.

DEFINE BUTTON btnFirst 
     IMAGE-UP FILE "Graphics/32x32/first.png":U NO-FOCUS FLAT-BUTTON
     LABEL "First" 
     SIZE 8 BY 1.91 TOOLTIP "First".

DEFINE BUTTON btnLast 
     IMAGE-UP FILE "Graphics/32x32/last.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Last" 
     SIZE 8 BY 1.91 TOOLTIP "Last".

DEFINE BUTTON btnNext 
     IMAGE-UP FILE "Graphics/32x32/next.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Next" 
     SIZE 8 BY 1.91 TOOLTIP "Next".

DEFINE BUTTON btnNumPad 
     IMAGE-UP FILE "Graphics/32x32/numeric_keypad.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "NumPad" 
     SIZE 8 BY 1.91 TOOLTIP "Numeric Keypad".

DEFINE BUTTON btnPrevious 
     IMAGE-UP FILE "Graphics/32x32/previous.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Previous" 
     SIZE 8 BY 1.91 TOOLTIP "Previous".

DEFINE VARIABLE btnClearText AS CHARACTER FORMAT "X(256)":U INITIAL "RESET" 
      VIEW-AS TEXT 
     SIZE 12 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnExitText AS CHARACTER FORMAT "X(256)":U INITIAL "EXIT" 
      VIEW-AS TEXT 
     SIZE 8 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnSettingsText AS CHARACTER FORMAT "X(256)":U INITIAL "SETTINGS" 
      VIEW-AS TEXT 
     SIZE 18 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE fiTagStatus AS CHARACTER FORMAT "X(256)":U 
     LABEL "Status" 
     VIEW-AS FILL-IN 
     SIZE 25.6 BY 1.38 TOOLTIP "Scan Tag status"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiTagStatusDescription AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 73 BY 1.38 TOOLTIP "Tag Status Description"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE statusMessage AS CHARACTER FORMAT "X(256)":U INITIAL "STATUS MESSAGE" 
      VIEW-AS TEXT 
     SIZE 146 BY 1.43 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 10 BY 2.38
     BGCOLOR 12 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      ttBrowseInventory SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 W-Win _FREEFORM
  QUERY BROWSE-1 DISPLAY
      ttBrowseInventory.quantity    WIDTH 25 COLUMN-LABEL "Qty On-hand" FORMAT ">>>,>>>,>>>9":U  
      ttBrowseInventory.tag         WIDTH 50 COLUMN-LABEL "Tag #"    FORMAT "X(30)"   
      ttBrowseInventory.warehouseID WIDTH 25 COLUMN-LABEL "Location" FORMAT "X(12)"
      ttBrowseInventory.jobID       WIDTH 20 COLUMN-LABEL "Job #"    FORMAT "X(20)"
      ttBrowseInventory.poID        WIDTH 20 COLUMN-LABEL "PO #"     FORMAT ">>>>>9"       
      fGetInventoryStatus() @ cInventoryStatus WIDTH 50 COLUMN-LABEL "Status"   FORMAT "X(40)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS NO-SCROLLBAR-VERTICAL NO-TAB-STOP SIZE 195 BY 12.62
         BGCOLOR 15 FGCOLOR 0 FONT 36 ROW-HEIGHT-CHARS 1 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnPrevious AT ROW 8.14 COL 200 WIDGET-ID 40
     btClear AT ROW 3.19 COL 194.8 WIDGET-ID 146
     btnFirst AT ROW 6.24 COL 200 WIDGET-ID 44
     fiTagStatus AT ROW 5 COL 13.4 COLON-ALIGNED WIDGET-ID 150
     btnLast AT ROW 11.95 COL 200 WIDGET-ID 46
     fiTagStatusDescription AT ROW 5 COL 40 COLON-ALIGNED NO-LABEL WIDGET-ID 152
     btnNext AT ROW 10.05 COL 200 WIDGET-ID 42
     BROWSE-1 AT ROW 6.67 COL 2 WIDGET-ID 200
     btnNumPad AT ROW 3.1 COL 118 WIDGET-ID 120 NO-TAB-STOP 
     btnExitText AT ROW 1.24 COL 189 COLON-ALIGNED NO-LABEL WIDGET-ID 70
     btnClearText AT ROW 3.38 COL 182 NO-LABEL WIDGET-ID 148
     btnSettingsText AT ROW 19.81 COL 147 COLON-ALIGNED NO-LABEL WIDGET-ID 72
     statusMessage AT ROW 19.91 COL 2 NO-LABEL WIDGET-ID 66
     RECT-2 AT ROW 2.91 COL 117 WIDGET-ID 130
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 207.8 BY 20.62
         BGCOLOR 21 FGCOLOR 15 FONT 38 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 1
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Update Tag Status"
         HEIGHT             = 20.62
         WIDTH              = 207.8
         MAX-HEIGHT         = 37.76
         MAX-WIDTH          = 307.2
         VIRTUAL-HEIGHT     = 37.76
         VIRTUAL-WIDTH      = 307.2
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
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
/* BROWSE-TAB BROWSE-1 btnNext F-Main */
/* SETTINGS FOR FILL-IN btnClearText IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fiTagStatus IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTagStatusDescription IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN statusMessage IN FRAME F-Main
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttBrowseInventory BY ttBrowseInventory.lastTransTime DESCENDING.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Update Tag Status */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Update Tag Status */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 W-Win
ON ROW-DISPLAY OF BROWSE-1 IN FRAME F-Main
DO:
    {methods/template/brwrowdisplay.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btClear W-Win
ON CHOOSE OF btClear IN FRAME F-Main /* Reset */
DO:

    RUN pStatusMessage ("", 0).
    {methods/run_link.i "SPLIT-SOURCE" "Reset"}

    {methods/run_link.i "LOADTAG-SOURCE" "EmptyTTLoadtag"}
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


&Scoped-define SELF-NAME btnExitText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExitText W-Win
ON MOUSE-SELECT-CLICK OF btnExitText IN FRAME F-Main
DO:
    RUN dispatch ("exit").
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFirst W-Win
ON CHOOSE OF btnFirst IN FRAME F-Main /* First */
DO:
    RUN pNavigate (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLast W-Win
ON CHOOSE OF btnLast IN FRAME F-Main /* Last */
DO:
    RUN pNavigate (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNext W-Win
ON CHOOSE OF btnNext IN FRAME F-Main /* Next */
DO:
    RUN pNavigate (SELF).
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


&Scoped-define SELF-NAME btnPrevious
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrevious W-Win
ON CHOOSE OF btnPrevious IN FRAME F-Main /* Previous */
DO:
    RUN pNavigate (SELF).
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


&Scoped-define SELF-NAME fiTagStatus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTagStatus W-Win
ON ENTRY OF fiTagStatus IN FRAME F-Main /* Status */
DO:
    SELF:BGCOLOR = 30.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTagStatus W-Win
ON LEAVE OF fiTagStatus IN FRAME F-Main /* Status */
DO:
    DEFINE VARIABLE lValidStatusID AS LOGICAL NO-UNDO.

    IF (((LASTKEY LT 609 OR LASTKEY GT 652) AND LASTKEY NE -1) OR (VALID-OBJECT (oKeyboard) AND oKeyboard:IsKeyboardOpen())) AND SELF:SCREEN-VALUE NE "" THEN DO:
        RUN Inventory_ValidateStatusID in hdInventoryProcs(
            INPUT  SELF:SCREEN-VALUE,
            OUTPUT lValidStatusID
            ). 
        IF lValidStatusID THEN
            RUN pUpdateTagStatus (SELF:SCREEN-VALUE).
        ELSE DO:
            fiTagStatusDescription:SCREEN-VALUE = "".
            RUN pStatusMessage ("Invalid status '" + SELF:SCREEN-VALUE + "'", 3).
            SELF:SET-SELECTION(1, -1).
            RETURN NO-APPLY.
        END.    
    END.    
    SELF:BGCOLOR = 15.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTagStatusDescription
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTagStatusDescription W-Win
ON ENTRY OF fiTagStatusDescription IN FRAME F-Main
DO:
    SELF:BGCOLOR = 30.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTagStatusDescription W-Win
ON LEAVE OF fiTagStatusDescription IN FRAME F-Main
DO:
    
    SELF:BGCOLOR = 15.
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
       RUN set-position IN h_adjustwindowsize ( 1.00 , 158.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 32.00 ) */

    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/exit.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.00 , 200.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/tagfilter.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_tagfilter ).
       RUN set-position IN h_tagfilter ( 2.91 , 4.20 ) NO-ERROR.
       /* Size in UIB:  ( 2.29 , 85.40 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'smartobj/setting.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_setting ).
       RUN set-position IN h_setting ( 19.57 , 168.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.81 , 7.60 ) */

       /* Links to SmartObject h_tagfilter. */
       RUN add-link IN adm-broker-hdl ( h_tagfilter , 'State':U , THIS-PROCEDURE ).
       RUN add-link IN adm-broker-hdl ( h_tagfilter , 'TAG':U , THIS-PROCEDURE ).

       /* Links to SmartObject h_setting. */
       RUN add-link IN adm-broker-hdl ( h_setting , 'SETTING':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_tagfilter ,
             h_exit , 'AFTER':U ).
    END. /* Page 1 */

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
  DISPLAY fiTagStatus fiTagStatusDescription btnExitText btnClearText 
          btnSettingsText statusMessage 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE btnPrevious btClear btnFirst btnLast btnNext BROWSE-1 btnNumPad 
         btnExitText btnClearText btnSettingsText statusMessage 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetKeyboard W-Win 
PROCEDURE GetKeyboard :
/*------------------------------------------------------------------------------
 Purpose:
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
  Purpose:     Override standard ADM method
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
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit W-Win 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
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
    
    RUN spGetSettingByName ("AllowOnHoldTagScan", OUTPUT cSettingValue).
    glAllowOnHoldTagScan = LOGICAL(cSettingValue) NO-ERROR.

    RUN spGetSettingByName ("DefaultTagScanStatusID", OUTPUT gcDefaultTagScanStatusID).
        
    oKeyboard:SetWindow({&WINDOW-NAME}:HANDLE).
    oKeyboard:SetProcedure(THIS-PROCEDURE).
    oKeyboard:SetFrame(FRAME {&FRAME-NAME}:HANDLE).
    
    {methods/run_link.i "TAG-SOURCE" "Set-Focus"}
    {methods/run_link.i "TAG-SOURCE" "DisableErrorAlerts"}

    ASSIGN
        btnSettingsText:VISIBLE = INDEX(gcShowSettings, "Text") GT 0
        btnNumPad:VISIBLE       = glShowVirtualKeyboard
        RECT-2:VISIBLE          = glShowVirtualKeyboard
        .  
        
    IF INDEX(gcShowSettings, "Icon") EQ 0 THEN
        {methods/run_link.i "Setting-SOURCE" "HideSettings"}        
    
    RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNavigate W-Win 
PROCEDURE pNavigate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphNavPanel AS HANDLE NO-UNDO.
    
    IF AVAILABLE ttBrowseInventory THEN
    CASE iphNavPanel:LABEL:
        WHEN "First" THEN
        APPLY "HOME":U TO BROWSE {&BROWSE-NAME}.
        WHEN "Previous" THEN
        BROWSE {&BROWSE-NAME}:SELECT-PREV-ROW().
        WHEN "Next" THEN
        BROWSE {&BROWSE-NAME}:SELECT-NEXT-ROW().
        WHEN "Last" THEN
        APPLY "END":U TO BROWSE {&BROWSE-NAME}.
    END CASE.
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
    
    DEFINE VARIABLE oLoadTag AS Inventory.Loadtag  NO-UNDO.

    {methods/run_link.i "TAG-SOURCE" "GetTag" "(OUTPUT oLoadtag)"}   

    IF VALID-OBJECT(oLoadTag) THEN DO:        
        ASSIGN
            cCompany       = oLoadtag:GetValue("Company")
            cTag           = oLoadtag:GetValue("Tag")
            cItemID        = oLoadtag:GetValue("ItemID")
            cItemType      = STRING(LOGICAL(oLoadtag:GetValue("ItemType")), "RM/FG")
            .

        IF NOT lSuccess THEN
            RUN pStatusMessage(INPUT cMessage, INPUT 3).
        ELSE DO:
        END.       
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateTagStatus W-Win 
PROCEDURE pUpdateTagStatus :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcStatusID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    IF VALID-OBJECT(oFGBin) AND oFGBin:IsAvailable() THEN DO:
        RUN api/inbound/UpdateTagStatus.p (
            INPUT  oFGBin:GetValue("Company"),
            INPUT  INTEGER(oFGBin:GetValue("POID")),
            INPUT  oFGBin:GetValue("JobID"),
            INPUT  INTEGER(oFGBin:GetValue("JobID2")),
            INPUT  oFGBin:GetValue("ItemID"),
            INPUT  "FG",
            INPUT  INTEGER(oFGBin:GetValue("BOLID")),
            INPUT  oFGBin:GetValue("Tag"),
            INPUT  oFGBin:GetValue("Warehouse"),
            INPUT  oFGBin:GetValue("Location"),
            INPUT  ipcStatusID,
            INPUT  cUserID,
            OUTPUT lSuccess,
            OUTPUT cMessage
            ).
        
        IF lSuccess THEN DO:
            FIND FIRST ttBrowseInventory
                 WHERE ttBrowseInventory.inventoryStockID EQ oFGBin:GetValue("ROWID")
                 NO-ERROR.
            IF NOT AVAILABLE ttBrowseInventory THEN
                CREATE ttBrowseInventory.
            
            ASSIGN
                ttBrowseInventory.inventoryStockID = oFGBin:GetValue("ROWID")
                ttBrowseInventory.quantity         = DECIMAL(oFGBin:GetValue("Quantity"))
                ttBrowseInventory.company          = oFGBin:GetValue("Company")
                ttBrowseInventory.primaryID        = oFGBin:GetValue("ItemID")
                ttBrowseInventory.tag              = oFGBin:GetValue("Tag")
                ttBrowseInventory.warehouseID      = oFGBin:GetValue("Warehouse")
                ttBrowseInventory.locationID       = oFGBin:GetValue("Location")
                ttBrowseInventory.jobID            = oFGBin:GetValue("JobID")
                ttBrowseInventory.jobID2           = INTEGER(oFGBin:GetValue("JobID2"))
                ttBrowseInventory.poID             = INTEGER(oFGBin:GetValue("POID"))
                ttBrowseInventory.inventoryStatus  = ipcStatusID
                ttBrowseInventory.lastTransTime    = NOW
                .
        END.
        ELSE
            RUN pStatusMessage (cMessage, 3).
        
        ASSIGN
            fiTagStatus:SENSITIVE               = FALSE
            fiTagStatus:SCREEN-VALUE            = ""
            fiTagStatusDescription:SCREEN-VALUE = ""
            .

        {&OPEN-QUERY-{&BROWSE-NAME}}
        
        {methods/run_link.i "TAG-SOURCE" "EmptyTag"}
    
        {methods/run_link.i "TAG-SOURCE" "ScanNextTag"}
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWinReSize W-Win 
PROCEDURE pWinReSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dCol    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dColTmp AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dRow    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dHeight AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dWidth  AS DECIMAL NO-UNDO.

    SESSION:SET-WAIT-STATE("General").
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            statusMessage:ROW                  = {&WINDOW-NAME}:HEIGHT - .86
            dCol                               = {&WINDOW-NAME}:WIDTH  - 8
            btnFirst:COL                       = dCol
            btnPrevious:COL                    = dCol
            btnNext:COL                        = dCol
            btnLast:COL                        = dCol
            btnExitText:COL                    = dCol - 9
            btnSettingsText:ROW                = {&WINDOW-NAME}:HEIGHT - .86
            btnSettingsText:COL                = dCol - 20            
            btnClearText:COL                   = dCol - 12
            btClear:COL                        = dCol
            BROWSE {&BROWSE-NAME}:HEIGHT       = {&WINDOW-NAME}:HEIGHT - BROWSE {&BROWSE-NAME}:ROW - 1.62
            BROWSE {&BROWSE-NAME}:WIDTH        = dCol - 2.5            
            .
        RUN set-position IN h_exit ( 1.00 , dCol ) NO-ERROR.
        RUN set-position IN h_setting ( {&WINDOW-NAME}:HEIGHT - 1.1 , btnSettingsText:COL + 18 ) NO-ERROR.
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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "ttBrowseInventory"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Foucs W-Win 
PROCEDURE Set-Foucs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {methods/run_link.i "SPLIT-SOURCE" "Set-Focus"}
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
    
    DEFINE VARIABLE cTagStatusDescription AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValidStatusID        AS LOGICAL   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.

    RUN new-state ("empty-message").
        
    CASE p-state:
        WHEN "tag-valid" THEN DO:
            {methods/run_link.i "TAG-SOURCE" "GetTag" "(OUTPUT oLoadtag)"}
            
            ASSIGN
                fiTagStatus:SCREEN-VALUE            = ""
                fiTagStatusDescription:SCREEN-VALUE = ""
                .
            
            IF VALID-OBJECT(oLoadtag) AND NOT LOGICAL(oLoadTag:GetValue("ItemType")) THEN DO:
                IF NOT VALID-OBJECT(oFGBin) THEN
                    oFGBin = NEW fg.FGBin().
                    
                oFGBin:SetContext(oLoadTag:GetValue("Company"), oLoadtag:GetValue("ItemID"), oLoadtag:GetValue("Tag")).
                IF oFGBin:IsAvailable() THEN DO:
                    IF gcDefaultTagScanStatusID NE "" AND gcDefaultTagScanStatusID NE ? THEN DO:
                        RUN Inventory_ValidateStatusID in hdInventoryProcs(
                            INPUT  gcDefaultTagScanStatusID,
                            OUTPUT lValidStatusID
                            ). 
                        IF lValidStatusID THEN DO: 
                            fiTagStatus:SCREEN-VALUE = gcDefaultTagScanStatusID.
                                                   
                            RUN Inventory_GetStatusDescription in hdInventoryProcs(
                                INPUT  gcDefaultTagScanStatusID,
                                OUTPUT cTagStatusDescription
                                ).
                                    
                            fiTagStatusDescription:SCREEN-VALUE = cTagStatusDescription.
                        END.
                        ELSE
                            RUN pStatusMessage ("Invalid Status in setting 'DefaultTagScanStatus'", 3).
                    END.
                    
                    IF NOT lValidStatusID THEN DO:
                        fiTagStatus:SCREEN-VALUE = oFGBin:GetValue("Status").
                        
                        RUN Inventory_GetStatusDescription in hdInventoryProcs(
                            INPUT  fiTagStatus:SCREEN-VALUE,
                            OUTPUT cTagStatusDescription
                            ).
                                
                        fiTagStatusDescription:SCREEN-VALUE = cTagStatusDescription.                            
                    END.
                                        
                    IF LOGICAL(oFGBin:GetValue("OnHold")) THEN DO:
                        IF glAllowOnHoldTagScan THEN
                            RUN pStatusMessage ("Tag is on hold", 2).    
                        ELSE DO:
                            RUN pStatusMessage ("Tag is on hold. Cannot change status", 3).
                            
                            {methods/run_link.i "TAG-SOURCE" "EmptyTag"}
                            
                            {methods/run_link.i "TAG-SOURCE" "ScanNextTag"}
                            
                            RETURN.    
                        END.
                    END.
                    
                    IF lValidStatusID THEN
                        RUN pUpdateTagStatus(gcDefaultTagScanStatusID).
                    ELSE DO:
                        fiTagStatus:SENSITIVE = TRUE.
                            
                        APPLY "ENTRY" TO fiTagStatus.                         
                    END.
                END.
                ELSE
                    RUN pStatusMessage ("Inventory does not exist", 3).
            END.
        END.
        WHEN "tag-invalid" THEN DO:
            fiTagStatus:SENSITIVE = FALSE.
        END.
        WHEN "tag-error" THEN DO:
            {methods/run_link.i "TAG-SOURCE" "GetMessageAndType" "(OUTPUT cStatusMessage, OUTPUT iStatusMessageType)"}
            RUN pStatusMessage (cStatusMessage, iStatusMessageType).
        END.        
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetInventoryStatus W-Win 
FUNCTION fGetInventoryStatus RETURNS CHARACTER
  (  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cStatusDescription AS CHARACTER NO-UNDO.
    
    RUN Inventory_GetStatusDescription in hdInventoryProcs(
        INPUT  ttBrowseInventory.inventoryStatus,
        OUTPUT cStatusDescription
        ).
    IF cStatusDescription NE "" THEN
        cResult = ttBrowseInventory.inventoryStatus + " - " + cStatusDescription.

    RETURN cResult.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

