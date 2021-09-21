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

  File: sharpshooter/w-rmInquiry.w

  Description: RM Inquiry and quantity adjustment

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
{custom/globdefs.i}
{sys/inc/var.i "NEW SHARED"}
{sys/inc/varasgn.i}

DEFINE VARIABLE cCompany   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemID    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemName  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cConsUOM   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWarehouse AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobNo     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iJobNo2    AS INTEGER   NO-UNDO.
DEFINE VARIABLE lError     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage   AS CHARACTER NO-UNDO.

DEFINE VARIABLE cCharHandle AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdItemBins  AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdItemSumm  AS HANDLE    NO-UNDO.

DEFINE VARIABLE lSummQueryRan AS LOGICAL NO-UNDO.
DEFINE VARIABLE lBinsQueryRan AS LOGICAL NO-UNDO.

DEFINE VARIABLE hdInventoryProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE iWarehouseLength AS INTEGER   NO-UNDO.

DEFINE VARIABLE oKeyboard AS system.Keyboard NO-UNDO.

oKeyboard = NEW system.Keyboard().

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
&Scoped-Define ENABLED-OBJECTS btItemHelp btLocHelp btNameHelp ~
btnKeyboardItem btnKeyboardLoc fiRMItem fiLocation btnNumPad ~
btnKeyboardName fiRMName btnViewSummary btnExitText btnAdjustQtyText 
&Scoped-Define DISPLAYED-OBJECTS fiRMItem fiLocation fiRMName fiConsUOM ~
btnExitText statusMessage btnAdjustQtyText 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_adjustqty AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-rminqbins AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-rminqsumm AS HANDLE NO-UNDO.
DEFINE VARIABLE h_exit AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatefirst AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatelast AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigatenext AS HANDLE NO-UNDO.
DEFINE VARIABLE h_navigateprev AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btItemHelp 
     IMAGE-UP FILE "Graphics/32x32/search_new.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 6.6 BY 1.57.

DEFINE BUTTON btLocHelp 
     IMAGE-UP FILE "Graphics/32x32/search_new.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 6.6 BY 1.57.

DEFINE BUTTON btNameHelp 
     IMAGE-UP FILE "Graphics/32x32/search_new.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 6.6 BY 1.57.

DEFINE BUTTON btnKeyboardItem 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btnKeyboardLoc 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btnKeyboardName 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btnNumPad 
     IMAGE-UP FILE "Graphics/32x32/numeric_keypad.ico":U
     LABEL "NumPad" 
     SIZE 8 BY 1.91 TOOLTIP "Numeric Keypad".

DEFINE BUTTON btnViewSummary 
     LABEL "VIEW SUMMARY" 
     SIZE 31 BY 1.43.

DEFINE VARIABLE btnAdjustQtyText AS CHARACTER FORMAT "X(256)":U INITIAL "ADJUST QTY" 
      VIEW-AS TEXT 
     SIZE 23 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnExitText AS CHARACTER FORMAT "X(256)":U INITIAL "EXIT" 
      VIEW-AS TEXT 
     SIZE 9 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE fiConsUOM AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1.43
     BGCOLOR 15 FGCOLOR 0 FONT 36 NO-UNDO.

DEFINE VARIABLE fiLocation AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32.2 BY 1.43
     BGCOLOR 15 FGCOLOR 0 FONT 36 NO-UNDO.

DEFINE VARIABLE fiRMItem AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 69.8 BY 1.43
     BGCOLOR 15 FGCOLOR 0 FONT 36 NO-UNDO.

DEFINE VARIABLE fiRMName AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 69.8 BY 1.43
     BGCOLOR 15 FGCOLOR 0 FONT 36 NO-UNDO.

DEFINE VARIABLE statusMessage AS CHARACTER FORMAT "X(256)":U INITIAL "STATUS MESSAGE" 
      VIEW-AS TEXT 
     SIZE 116 BY 1.43 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 10 BY 2.38
     BGCOLOR 12 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btItemHelp AT ROW 2.43 COL 92.6 WIDGET-ID 138 NO-TAB-STOP 
     btLocHelp AT ROW 2.43 COL 162 WIDGET-ID 146 NO-TAB-STOP 
     btNameHelp AT ROW 4.05 COL 92.6 WIDGET-ID 152 NO-TAB-STOP 
     btnKeyboardItem AT ROW 2.43 COL 100.2 WIDGET-ID 136 NO-TAB-STOP 
     btnKeyboardLoc AT ROW 2.43 COL 169.2 WIDGET-ID 144 NO-TAB-STOP 
     fiRMItem AT ROW 2.48 COL 19.6 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     fiLocation AT ROW 2.48 COL 126.8 COLON-ALIGNED NO-LABEL WIDGET-ID 142
     btnNumPad AT ROW 3.19 COL 184.6 WIDGET-ID 120 NO-TAB-STOP 
     btnKeyboardName AT ROW 4.05 COL 100.2 WIDGET-ID 154 NO-TAB-STOP 
     fiRMName AT ROW 4.14 COL 19.6 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     fiConsUOM AT ROW 4.14 COL 126.8 COLON-ALIGNED NO-LABEL WIDGET-ID 148
     btnViewSummary AT ROW 4.19 COL 144.6 WIDGET-ID 166
     btnExitText AT ROW 1.24 COL 185 NO-LABEL WIDGET-ID 24
     statusMessage AT ROW 31.95 COL 3 NO-LABEL WIDGET-ID 28
     btnAdjustQtyText AT ROW 31.95 COL 170 COLON-ALIGNED NO-LABEL WIDGET-ID 134
     "LOCATION:" VIEW-AS TEXT
          SIZE 19.8 BY 1.1 AT ROW 2.67 COL 109 WIDGET-ID 160
     "RM NAME:" VIEW-AS TEXT
          SIZE 18 BY 1.1 AT ROW 4.29 COL 3 WIDGET-ID 158
     "RM ITEM:" VIEW-AS TEXT
          SIZE 16 BY 1.1 AT ROW 2.67 COL 5 WIDGET-ID 156
     "UOM:" VIEW-AS TEXT
          SIZE 10.4 BY 1.1 AT ROW 4.24 COL 118 WIDGET-ID 162
     RECT-2 AT ROW 3 COL 183.6 WIDGET-ID 130
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 202 BY 32.81
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
         TITLE              = "RM Inquiry"
         HEIGHT             = 32.81
         WIDTH              = 202
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
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
/* SETTINGS FOR FILL-IN btnExitText IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       btnKeyboardItem:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btnKeyboardLoc:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btnKeyboardName:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fiConsUOM IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN statusMessage IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* RM Inquiry */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* RM Inquiry */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btItemHelp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btItemHelp W-Win
ON CHOOSE OF btItemHelp IN FRAME F-Main
DO:
    APPLY "HELP" TO fiRMItem.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btLocHelp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btLocHelp W-Win
ON CHOOSE OF btLocHelp IN FRAME F-Main
DO:
    APPLY "HELP" TO fiLocation.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdjustQtyText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdjustQtyText W-Win
ON MOUSE-SELECT-CLICK OF btnAdjustQtyText IN FRAME F-Main
DO:
    RUN AdjustQuantity IN h_b-rminqbins.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btNameHelp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btNameHelp W-Win
ON CHOOSE OF btNameHelp IN FRAME F-Main
DO:
    APPLY "HELP" TO fiRMName.
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


&Scoped-define SELF-NAME btnKeyboardItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyboardItem W-Win
ON CHOOSE OF btnKeyboardItem IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY":U TO fiRMItem.
    oKeyboard:OpenKeyboardOverride (fiRMItem:HANDLE, "Qwerty").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKeyboardLoc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyboardLoc W-Win
ON CHOOSE OF btnKeyboardLoc IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY":U TO fiLocation.
    oKeyboard:OpenKeyboardOverride (fiLocation:HANDLE, "Qwerty").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKeyboardName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyboardName W-Win
ON CHOOSE OF btnKeyboardName IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY":U TO fiRMName.
    oKeyboard:OpenKeyboardOverride (fiRMName:HANDLE, "Qwerty").
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


&Scoped-define SELF-NAME btnViewSummary
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnViewSummary W-Win
ON CHOOSE OF btnViewSummary IN FRAME F-Main /* VIEW SUMMARY */
DO:
    IF SELF:LABEL EQ "VIEW SUMMARY" THEN DO:
        RUN select-page(2).
        IF NOT lSummQueryRan THEN
            RUN pScanItem.
    END.
    ELSE
        RUN select-page(1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLocation
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLocation W-Win
ON ENTRY OF fiLocation IN FRAME F-Main
DO:
    SELF:SET-SELECTION (1, -1).
    oKeyboard:OpenKeyboard (SELF, "Qwerty").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLocation W-Win
ON HELP OF fiLocation IN FRAME F-Main
DO:
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
  
    RUN system/openlookup.p (
        INPUT  "",  /* company */ 
        INPUT  "",  /* lookup field */
        INPUT  150, /* Subject ID */
        INPUT  "",  /* User ID */
        INPUT  0,   /* Param value ID */
        OUTPUT returnFields, 
        OUTPUT lookupField, 
        OUTPUT recVal
        ). 

    IF lookupField NE "" THEN DO:
        fiLocation:SCREEN-VALUE = IF NUM-ENTRIES(returnFields,"|") GE 2 THEN
                                       ENTRY(2, returnFields, "|")
                                  ELSE
                                      "".
        
        APPLY "LEAVE" TO SELF.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLocation W-Win
ON LEAVE OF fiLocation IN FRAME F-Main
DO:        
    IF cWarehouse EQ SUBSTRING(fiLocation:SCREEN-VALUE, 1, iWarehouseLength) AND cLocation EQ SUBSTRING(fiLocation:SCREEN-VALUE, iWarehouseLength + 1) THEN
        RETURN.
        
    ASSIGN
        cWarehouse = SUBSTRING(fiLocation:SCREEN-VALUE, 1, iWarehouseLength)
        cLocation  = SUBSTRING(fiLocation:SCREEN-VALUE, iWarehouseLength + 1)
        .        

    RUN pScanItem.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiRMItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiRMItem W-Win
ON ENTRY OF fiRMItem IN FRAME F-Main
DO:
    SELF:SET-SELECTION (1, -1).
    oKeyboard:OpenKeyboard (SELF, "Qwerty").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiRMItem W-Win
ON HELP OF fiRMItem IN FRAME F-Main
DO:
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
  
    RUN system/openlookup.p (
        INPUT  "",  /* company */ 
        INPUT  "",  /* lookup field */
        INPUT  26,  /* Subject ID */
        INPUT  "",  /* User ID */
        INPUT  0,   /* Param value ID */
        OUTPUT returnFields, 
        OUTPUT lookupField, 
        OUTPUT recVal
        ). 

    IF lookupField NE "" THEN DO:
        ASSIGN
            fiRMName:SCREEN-VALUE = IF NUM-ENTRIES(returnFields,"|") GE 4 THEN
                                          ENTRY(4, returnFields, "|")
                                      ELSE
                                          ""
            fiRMItem:SCREEN-VALUE = IF NUM-ENTRIES(returnFields,"|") GE 2 THEN
                                        ENTRY(2, returnFields, "|")
                                    ELSE
                                        "".
        
        APPLY "LEAVE" TO SELF.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiRMItem W-Win
ON LEAVE OF fiRMItem IN FRAME F-Main
DO:
    IF cItemID EQ SELF:SCREEN-VALUE AND cItemID NE "" THEN
        RETURN.

    IF SELF:SCREEN-VALUE EQ "" THEN
        RETURN.
        
    ASSIGN        
        cItemID               = SELF:SCREEN-VALUE
        fiRMName:SCREEN-VALUE = ""
        cItemName             = ""
        .
    
    RUN pScanItem.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiRMName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiRMName W-Win
ON ENTRY OF fiRMName IN FRAME F-Main
DO:
    SELF:SET-SELECTION (1, -1).
    oKeyboard:OpenKeyboard (SELF, "Qwerty").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiRMName W-Win
ON HELP OF fiRMName IN FRAME F-Main
DO:
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
  
    RUN system/openlookup.p (
        INPUT  "",  /* company */ 
        INPUT  "",  /* lookup field */
        INPUT  26,  /* Subject ID */
        INPUT  "",  /* User ID */
        INPUT  0,   /* Param value ID */
        OUTPUT returnFields, 
        OUTPUT lookupField, 
        OUTPUT recVal
        ). 

    IF lookupField NE "" THEN DO:
        ASSIGN
            fiRMName:SCREEN-VALUE = IF NUM-ENTRIES(returnFields,"|") GE 4 THEN
                                          ENTRY(4, returnFields, "|")
                                      ELSE
                                          ""
            fiRMItem:SCREEN-VALUE = IF NUM-ENTRIES(returnFields,"|") GE 2 THEN
                                        ENTRY(2, returnFields, "|")
                                    ELSE
                                        "".
        
        APPLY "LEAVE" TO SELF.
    END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiRMName W-Win
ON LEAVE OF fiRMName IN FRAME F-Main
DO:
    IF cItemName EQ SELF:SCREEN-VALUE AND cItemName NE "" THEN
        RETURN.

    IF SELF:SCREEN-VALUE EQ "" THEN
        RETURN.
        
    ASSIGN        
        cItemName             = SELF:SCREEN-VALUE
        fiRMName:SCREEN-VALUE = ""
        cItemID               = ""
        .

    RUN pScanItem.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}
{sharpshooter/smartobj/windowExit.i}
{sharpshooter/pStatusMessage.i}

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
             INPUT  'sharpshooter/smartobj/exit.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_exit ).
       RUN set-position IN h_exit ( 1.00 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/b-rminqbins.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-rminqbins ).
       RUN set-position IN h_b-rminqbins ( 5.91 , 2.60 ) NO-ERROR.
       RUN set-size IN h_b-rminqbins ( 25.57 , 192.40 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatefirst.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatefirst ).
       RUN set-position IN h_navigatefirst ( 6.71 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigateprev.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigateprev ).
       RUN set-position IN h_navigateprev ( 8.62 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatenext.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatenext ).
       RUN set-position IN h_navigatenext ( 10.52 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/navigatelast.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_navigatelast ).
       RUN set-position IN h_navigatelast ( 12.43 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/smartobj/adjustqty.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_adjustqty ).
       RUN set-position IN h_adjustqty ( 31.71 , 195.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.91 , 8.00 ) */

       /* Links to SmartBrowser h_b-rminqbins. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'Bins':U , h_b-rminqbins ).

       /* Links to SmartObject h_navigatefirst. */
       RUN add-link IN adm-broker-hdl ( h_b-rminqbins , 'NAV-FIRST':U , h_navigatefirst ).

       /* Links to SmartObject h_navigateprev. */
       RUN add-link IN adm-broker-hdl ( h_b-rminqbins , 'NAV-PREV':U , h_navigateprev ).

       /* Links to SmartObject h_navigatenext. */
       RUN add-link IN adm-broker-hdl ( h_b-rminqbins , 'NAV-NEXT':U , h_navigatenext ).

       /* Links to SmartObject h_navigatelast. */
       RUN add-link IN adm-broker-hdl ( h_b-rminqbins , 'NAV-LAST':U , h_navigatelast ).

       /* Links to SmartObject h_adjustqty. */
       RUN add-link IN adm-broker-hdl ( h_b-rminqbins , 'ADJUST':U , h_adjustqty ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-rminqbins ,
             btnViewSummary:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatefirst ,
             h_b-rminqbins , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigateprev ,
             h_navigatefirst , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatenext ,
             h_navigateprev , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_navigatelast ,
             h_navigatenext , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_adjustqty ,
             h_navigatelast , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sharpshooter/b-rminqsumm.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-rminqsumm ).
       RUN set-position IN h_b-rminqsumm ( 5.91 , 2.60 ) NO-ERROR.
       RUN set-size IN h_b-rminqsumm ( 25.57 , 199.40 ) NO-ERROR.

       /* Links to SmartBrowser h_b-rminqsumm. */
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'Summ':U , h_b-rminqsumm ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-rminqsumm ,
             btnViewSummary:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 2 */

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
  DISPLAY fiRMItem fiLocation fiRMName fiConsUOM btnExitText statusMessage 
          btnAdjustQtyText 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE btItemHelp btLocHelp btNameHelp btnKeyboardItem btnKeyboardLoc 
         fiRMItem fiLocation btnNumPad btnKeyboardName fiRMName btnViewSummary 
         btnExitText btnAdjustQtyText 
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
    DEFINE INPUT PARAMETER ipcKeyStroke AS CHARACTER NO-UNDO.
    
    IF VALID-OBJECT (oKeyboard) THEN
        oKeyboard:KeyStroke(ipcKeyStroke).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-change-page W-Win 
PROCEDURE local-change-page :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page AS INTEGER NO-UNDO.
  DEFINE VARIABLE dCol             AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dColTmp          AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dRow             AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dHeight          AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dWidth           AS DECIMAL NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'change-page':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  DO WITH FRAME {&FRAME-NAME}:
      CASE adm-current-page:
          WHEN 1 THEN
          ASSIGN
              btnViewSummary:LABEL    = "VIEW SUMMARY"
              btnAdjustQtyText:HIDDEN = NO
              .
          WHEN 2 THEN DO:
            RUN get-position IN h_b-rminqsumm ( OUTPUT dRow , OUTPUT dColTmp ) NO-ERROR.
            RUN get-size IN h_b-rminqsumm ( OUTPUT dHeight , OUTPUT dWidth ) NO-ERROR.
            ASSIGN
                btnViewSummary:LABEL    = "VIEW DETAIL"
                btnAdjustQtyText:HIDDEN = YES
                dCol    = {&WINDOW-NAME}:WIDTH  - 1
                dHeight = {&WINDOW-NAME}:HEIGHT - dRow - 1.33
                dWidth  = dCol - 3
                .
            RUN set-size IN h_b-rminqsumm ( dHeight , dWidth ) NO-ERROR.
          END.
      END CASE.
  END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy W-Win
PROCEDURE local-destroy:
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit W-Win 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.
    FIND FIRST company NO-LOCK 
         WHERE company.company EQ cocode
         NO-ERROR .
    IF AVAILABLE company THEN
    {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE
                         + " - " + DYNAMIC-FUNCTION("sfVersion") + " - " 
                         + STRING(company.name) + " - " + locode.
    oKeyboard:SetWindow({&WINDOW-NAME}:HANDLE).
    oKeyboard:SetProcedure(THIS-PROCEDURE).
    oKeyboard:SetFrame(FRAME {&FRAME-NAME}:HANDLE).
    cCompany = cocode.
    RUN Inventory_GetWarehouseLength IN hdInventoryProcs (
        INPUT  cCompany,
        OUTPUT iWarehouseLength
        ).
    RUN pStatusMessage ("", 0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pScanItem W-Win 
PROCEDURE pScanItem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.    

    IF cCompany EQ "" THEN
        RETURN.
    
    IF cItemName EQ "" AND cItemID EQ "" THEN
        RETURN.
    
    IF NOT VALID-HANDLE(hdItemBins) THEN DO:
        RUN get-link-handle IN adm-broker-hdl(
            INPUT  THIS-PROCEDURE,
            INPUT  "Bins-TARGET",
            OUTPUT cCharHandle
            ).
        hdItemBins = HANDLE(cCharHandle).
    END.
    
    IF VALID-HANDLE(hdItemBins) THEN DO:
        lBinsQueryRan = TRUE.
        RUN ScanItem IN hdItemBins (
            INPUT        cCompany,
            INPUT        cWarehouse,
            INPUT        cLocation,
            INPUT-OUTPUT cItemID,
            INPUT-OUTPUT cItemName,
            INPUT        FALSE,  /* Include Zero qty bins */
            INPUT        TRUE,   /* Include empty tag bins */
            OUTPUT       cConsUOM,
            OUTPUT       lError,
            OUTPUT       cMessage
            ).
    END.

    IF NOT VALID-HANDLE(hdItemSumm) THEN DO:
        RUN get-link-handle IN adm-broker-hdl(
            INPUT  THIS-PROCEDURE,
            INPUT  "Summ-TARGET",
            OUTPUT cCharHandle
            ).
        hdItemSumm = HANDLE(cCharHandle).
    END.
    
    IF VALID-HANDLE(hdItemSumm) THEN DO:
        lSummQueryRan = TRUE.
        RUN ScanItem IN hdItemSumm (
            INPUT  cCompany,
            INPUT  cItemID,
            OUTPUT lError,
            OUTPUT cMessage
            ).
    END.
    
    ASSIGN
        fiRMItem:SCREEN-VALUE  = cItemID
        fiRMName:SCREEN-VALUE  = cItemName
        fiConsUOM:SCREEN-VALUE = cConsUOM
        .

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

    SESSION:SET-WAIT-STATE("General").
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            {&WINDOW-NAME}:ROW                 = 1
            {&WINDOW-NAME}:COL                 = 1
            {&WINDOW-NAME}:VIRTUAL-HEIGHT      = SESSION:HEIGHT - 1
            {&WINDOW-NAME}:VIRTUAL-WIDTH       = SESSION:WIDTH  - 1
            {&WINDOW-NAME}:HEIGHT              = {&WINDOW-NAME}:VIRTUAL-HEIGHT
            {&WINDOW-NAME}:WIDTH               = {&WINDOW-NAME}:VIRTUAL-WIDTH
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = {&WINDOW-NAME}:WIDTH
            FRAME {&FRAME-NAME}:HEIGHT         = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:WIDTH          = {&WINDOW-NAME}:WIDTH
            statusMessage:ROW                  = {&WINDOW-NAME}:HEIGHT - .86
            dCol                               = {&WINDOW-NAME}:WIDTH  - 8
            btnExitText:COL                    = dCol - 9
            btnAdjustQtyText:COL               = dCol - btnAdjustQtyText:WIDTH - 1
            btnAdjustQtyText:ROW               = {&WINDOW-NAME}:HEIGHT - .86
            .
        RUN set-position IN h_exit ( 1.00 , dCol ) NO-ERROR.
        RUN get-position IN h_navigatefirst ( OUTPUT dRow , OUTPUT dColTmp ) NO-ERROR.
        RUN set-position IN h_navigatefirst ( dRow , dCol ) NO-ERROR.
        dRow = dRow + 1.9.
        RUN set-position IN h_navigateprev ( dRow , dCol ) NO-ERROR.
        dRow = dRow + 1.9.
        RUN set-position IN h_navigatenext ( dRow , dCol ) NO-ERROR.
        dRow = dRow + 1.9.
        RUN set-position IN h_navigatelast ( dRow , dCol ) NO-ERROR.
        dRow = {&WINDOW-NAME}:HEIGHT - 1.62.
        RUN set-position IN h_adjustqty ( dRow , dCol ) NO-ERROR.
        RUN get-position IN h_b-rminqbins ( OUTPUT dRow , OUTPUT dColTmp ) NO-ERROR.
        RUN get-size IN h_b-rminqbins ( OUTPUT dHeight , OUTPUT dWidth ) NO-ERROR.
        ASSIGN
            dHeight = {&WINDOW-NAME}:HEIGHT - dRow - 1.33
            dWidth  = dCol - 3
            .
        RUN set-size IN h_b-rminqbins ( dHeight , dWidth ) NO-ERROR.
    END. /* do with */
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScanItem W-Win 
PROCEDURE ScanItem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcWarehouse AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocation  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemName  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2    AS INTEGER   NO-UNDO.

    ASSIGN
        cCompany   = ipcCompany  
        cWarehouse = ipcWarehouse
        cLocation  = ipcLocation 
        cItemID    = ipcItemID   
        cItemName  = ipcItemName
        cJobNo     = ipcJobNo    
        iJobNo2    = ipiJobNo2
        .   

    RUN pScanItem.

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

