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

  File: addon/fg/w-TagStatusUpdate.w

  Description: Scans a tag and change its status

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
     
USING system.SharedConfig.

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
    DEFINE VARIABLE hdInventoryProcs AS HANDLE    NO-UNDO.
    DEFINE VARIABLE scInstance       AS CLASS system.SharedConfig NO-UNDO.
    DEFINE VARIABLE lRecFound        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cRtnValue        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSSTagStatus     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSSTagStatus     AS LOGICAL   NO-UNDO.    
    
    RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.
    
    {system/sysconst.i}
    {inventory/ttInventory.i "NEW SHARED"}
    {wip/keyboardDefs.i}
    {custom/globdefs.i}
    
    DEFINE TEMP-TABLE ttFGBin NO-UNDO
        FIELD company         AS CHARACTER
        FIELD quantity        AS DECIMAL
        FIELD warehouseID     AS CHARACTER
        FIELD tag             AS CHARACTER
        FIELD jobID           AS CHARACTER
        FIELD inventoryStatus AS CHARACTER
        FIELD poID            AS INTEGER 
        .

    RUN sys/ref/nk1look.p(
        INPUT  g_company,
        INPUT  "SSTagStatus",
        INPUT  "C",
        INPUT  NO,
        INPUT  NO,
        INPUT  "",
        INPUT  "",
        OUTPUT cRtnValue,
        OUTPUT lRecFound    
        ).
    cSSTagStatus = cRtnValue.
       
    RUN sys/ref/nk1look.p(
        INPUT  g_company,
        INPUT  "SSTagStatus",
        INPUT  "C",
        INPUT  NO,
        INPUT  NO,
        INPUT  "",
        INPUT  "",
        OUTPUT cRtnValue,
        OUTPUT lRecFound    
        ).        
    lSSTagStatus = LOGICAL(cRtnValue) NO-ERROR.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttFGBin

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 ttFGBin.quantity ttFGBin.tag ttFGBin.warehouseID ttFGBin.jobID ttFGBin.poID ttFGBin.inventoryStatus   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH ttFGBin
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH ttFGBin.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 ttFGBin
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 ttFGBin


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnExit fiTagNo btnKeyboard BROWSE-2 ~
btnFirst btnPrevious btnNext btnLast 
&Scoped-Define DISPLAYED-OBJECTS fiTagNo fiTagStatus fiStatusDescription 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnExit AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U
     LABEL "Exit" 
     SIZE 9.6 BY 2.29 TOOLTIP "Exit".

DEFINE BUTTON btnFirst 
     IMAGE-UP FILE "Graphics/32x32/navigate_up2.ico":U
     LABEL "First" 
     SIZE 9.6 BY 2.29 TOOLTIP "First".

DEFINE BUTTON btnKeyboard 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U
     LABEL "KeyBoard" 
     SIZE 6.4 BY 1.52 TOOLTIP "KeyBoard".

DEFINE BUTTON btnKeyBoard2 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U
     LABEL "Button 9" 
     SIZE 6.4 BY 1.52.

DEFINE BUTTON btnLast 
     IMAGE-UP FILE "Graphics/32x32/navigate_down2.ico":U
     LABEL "Last" 
     SIZE 9.6 BY 2.29 TOOLTIP "Last".

DEFINE BUTTON btnNext 
     IMAGE-UP FILE "Graphics/32x32/navigate_down.ico":U
     LABEL "Next" 
     SIZE 9.6 BY 2.29 TOOLTIP "Next".

DEFINE BUTTON btnPrevious 
     IMAGE-UP FILE "Graphics/32x32/navigate_up.ico":U
     LABEL "Previous" 
     SIZE 9.6 BY 2.29 TOOLTIP "Previous".

DEFINE VARIABLE fiStatusDescription AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 54.8 BY 1.52 TOOLTIP "Status Description"
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiTagNo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 79 BY 1.52
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiTagStatus AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.52 TOOLTIP "Tag Status"
     FONT 37 NO-UNDO.

DEFINE RECTANGLE RECT-30
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 202 BY 5.48.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      ttFGBin SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 W-Win _FREEFORM
  QUERY BROWSE-2 DISPLAY
      ttFGBin.quantity    WIDTH 25 COLUMN-LABEL "Qty On-hand"   
      ttFGBin.tag         WIDTH 50 COLUMN-LABEL "Tag #"    FORMAT "X(30)"   
      ttFGBin.warehouseID WIDTH 30 COLUMN-LABEL "Location" FORMAT "X(12)"
      ttFGBin.jobID       WIDTH 25 COLUMN-LABEL "Job #"    FORMAT "X(20)"
      ttFGBin.poID        WIDTH 25 COLUMN-LABEL "PO #"     FORMAT ">>>>>9"
      ttFGBin.inventoryStatus      COLUMN-LABEL "Status"   FORMAT "X(15)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 187 BY 26.43
         FONT 36 ROW-HEIGHT-CHARS .95 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnExit AT ROW 1.24 COL 192.4 WIDGET-ID 26
     fiTagNo AT ROW 2 COL 39 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     btnKeyboard AT ROW 2 COL 121.4 WIDGET-ID 6
     fiTagStatus AT ROW 4 COL 39.2 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     fiStatusDescription AT ROW 4 COL 99.6 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     btnKeyBoard2 AT ROW 4.1 COL 64.4 WIDGET-ID 30
     BROWSE-2 AT ROW 6.71 COL 3 WIDGET-ID 200
     btnFirst AT ROW 6.95 COL 191.6 WIDGET-ID 18
     btnPrevious AT ROW 11.24 COL 191.6 WIDGET-ID 20
     btnNext AT ROW 25.62 COL 191.6 WIDGET-ID 22
     btnLast AT ROW 30.52 COL 191.6 WIDGET-ID 24
     "Tag:" VIEW-AS TEXT
          SIZE 8 BY 1.19 AT ROW 2.14 COL 31.8 WIDGET-ID 4
          FONT 36
     "Status Description:" VIEW-AS TEXT
          SIZE 27 BY 1.52 AT ROW 4 COL 74 WIDGET-ID 14
          FONT 36
     "Status:" VIEW-AS TEXT
          SIZE 10 BY 1.52 AT ROW 4 COL 28.6 WIDGET-ID 10
          FONT 36
     RECT-30 AT ROW 1 COL 1 WIDGET-ID 16
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 202 BY 32.91
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.


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
         TITLE              = "Update Tag Status"
         HEIGHT             = 32.81
         WIDTH              = 202
         MAX-HEIGHT         = 33.57
         MAX-WIDTH          = 273.2
         VIRTUAL-HEIGHT     = 33.57
         VIRTUAL-WIDTH      = 273.2
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
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
/* BROWSE-TAB BROWSE-2 btnKeyBoard2 F-Main */
/* SETTINGS FOR BUTTON btnKeyBoard2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiStatusDescription IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTagStatus IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-30 IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttFGBin.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
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


&Scoped-define SELF-NAME btnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExit W-Win
ON CHOOSE OF btnExit IN FRAME F-Main /* Exit */
DO:
    IF VALID-HANDLE(hdInventoryProcs) THEN 
        DELETE PROCEDURE hdInventoryProcs.
    IF VALID-HANDLE(hKeyboard) THEN
        DELETE PROCEDURE hKeyboard.
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFirst W-Win
ON CHOOSE OF btnFirst IN FRAME F-Main /* First */
DO:
    IF AVAILABLE ttFGBin THEN 
        APPLY "HOME":U TO BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKeyboard
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyboard W-Win
ON CHOOSE OF btnKeyboard IN FRAME F-Main /* KeyBoard */
DO:
    APPLY "ENTRY":U TO fiTagNo.
    RUN pKeyboard(
        INPUT fiTagNo:HANDLE,
        INPUT "Qwerty"
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKeyBoard2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyBoard2 W-Win
ON CHOOSE OF btnKeyBoard2 IN FRAME F-Main /* Button 9 */
DO:
    APPLY "ENTRY":U TO fiTagStatus.
    
    RUN pKeyboard(
        INPUT fiTagStatus:HANDLE,
        INPUT "Qwerty"
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLast W-Win
ON CHOOSE OF btnLast IN FRAME F-Main /* Last */
DO: 
    IF AVAILABLE ttFGBin THEN
        APPLY "END":U TO BROWSE {&BROWSE-NAME}.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNext W-Win
ON CHOOSE OF btnNext IN FRAME F-Main /* Next */
DO:
    IF AVAILABLE ttFGBin THEN
        BROWSE {&BROWSE-NAME}:SELECT-NEXT-ROW().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrevious
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrevious W-Win
ON CHOOSE OF btnPrevious IN FRAME F-Main /* Previous */
DO:
    IF AVAILABLE ttFGBin THEN
        BROWSE {&BROWSE-NAME}:SELECT-PREV-ROW().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&Scoped-define SELF-NAME fiTagNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTagNo W-Win
ON LEAVE OF fiTagNo IN FRAME F-Main
DO:
    DEFINE VARIABLE lSuccess     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturnFocus AS CHARACTER NO-UNDO.
    
     RUN pValidateTag(
         INPUT  fiTagNo:SCREEN-VALUE,
         OUTPUT lSuccess,
         OUTPUT cMessage
         ).
    IF NOT lSuccess THEN DO:
        fiTagStatus:SENSITIVE = NO.
        MESSAGE cMessage 
            VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.    
    END.
    RUN pEnableTagStatus(
        OUTPUT cReturnFocus
        ).
    IF cReturnFocus EQ "TagStatus" THEN 
        APPLY "ENTRY":U TO fiTagStatus.
    RETURN NO-APPLY.    
                 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTagStatus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTagStatus W-Win
ON LEAVE OF fiTagStatus IN FRAME F-Main
DO:
    DEFINE VARIABLE lValidStatusID AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lOnHold        AS LOGICAL NO-UNDO.
    
    RUN Inventory_ValidateStatusID in hdInventoryProcs(
        INPUT  fiTagStatus:SCREEN-VALUE,
        OUTPUT lValidStatusID
        ).
           
    IF NOT lValidStatusID THEN DO:
        MESSAGE "Invalid status ID"
            VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.           
    END.  
    RUN Inventory_GetStatusOnHold IN hdInventoryProcs(
       INPUT  fiTagStatus:SCREEN-VALUE,
       OUTPUT lOnHold
       ).             
    RUN pUpdateBinStatus(
        INPUT g_company,
        INPUT fiTagNo:SCREEN-VALUE,
        INPUT fiTagStatus:SCREEN-VALUE,
        INPUT lOnHold
        ). 
    APPLY "ENTRY":U TO fiTagNo.
    RETURN NO-APPLY.                                        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTagStatus W-Win
ON VALUE-CHANGED OF fiTagStatus IN FRAME F-Main
DO:
    DEFINE VARIABLE cStatusDescription AS CHARACTER NO-UNDO.
    
    RUN Inventory_GetStatusDescription in hdInventoryProcs(
        INPUT  fiTagStatus:SCREEN-VALUE,
        OUTPUT cStatusDescription
        ).
    IF cStatusDescription NE "" THEN
        fiStatusDescription:SCREEN-VALUE = cStatusDescription.                
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}
{wip/pNavigate.i}
{wip/pKeyboard.i}

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
  DISPLAY fiTagNo fiTagStatus fiStatusDescription 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE btnExit fiTagNo btnKeyboard BROWSE-2 btnFirst btnPrevious btnNext 
         btnLast 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateTTRecord W-Win 
PROCEDURE pCreateTTRecord PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocation AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTagNo    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobID    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcPoNo     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcStatusID AS CHARACTER NO-UNDO.

    FIND FIRST ttFGBin 
         WHERE ttFGBin.company EQ ipcCompany
           AND ttFGBin.tag     EQ ipcTagNo
         NO-ERROR.
         
    IF AVAILABLE ttFGBin THEN 
        ttFGBin.inventoryStatus = ipcStatusID.
                        
    ELSE DO:            
        CREATE ttFGBin.
        ASSIGN
            ttFGBin.company         = ipcCompany
            ttFGBin.quantity        = ipdQuantity
            ttFGBin.warehouseID     = ipcLocation    
            ttFGBin.tag             = ipcTagNo       
            ttFGBin.jobID           = ipcJobID       
            ttFGBin.inventoryStatus = ipcStatusID    
            ttFGBin.poID            = INTEGER(ipcPoNo)
             .
    END.               
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pEnableTagStatus W-Win 
PROCEDURE pEnableTagStatus PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcReturnFocus  AS CHARACTER NO-UNDO.
   
    DEFINE VARIABLE lValidStatusID     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cStatusDescription AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemName          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lResponse          AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lOnHold            AS LOGICAL   NO-UNDO.
         
    FIND fg-bin NO-LOCK 
        WHERE fg-bin.company  EQ g_company
          AND fg-bin.tag      EQ fiTagNo:SCREEN-VALUE IN FRAME {&FRAME-NAME}
          AND fg-bin.qty      NE 0
        NO-ERROR.
    IF AVAILABLE fg-bin THEN DO:
        IF fg-bin.onHold THEN DO:
            IF lSSTagStatus THEN
                ASSIGN
                    fiTagStatus:SENSITIVE   = TRUE
                    btnKeyBoard2:SENSITIVE  = TRUE 
                    opcReturnFocus          = "TagStatus"
                    .
            ELSE DO:
                IF scInstance EQ ? THEN 
                    scInstance = SharedConfig:instance.
                RUN Inventory_GetStatusDescription in hdInventoryProcs(
                    INPUT  fg-bin.statusID,
                    OUTPUT cStatusDescription
                    ).  
                FIND FIRST itemfg NO-LOCK
                     WHERE itemfg.company EQ g_company
                       AND itemfg.i-no    EQ fg-bin.i-no
                     NO-ERROR.
                IF AVAILABLE itemfg THEN 
                    cItemName = itemfg.i-name.
                                                   
                scInstance:SetValue(
                    INPUT "StockId",
                    INPUT fg-bin.tag
                    ). 
                 scInstance:SetValue(
                    INPUT "TagStatus",
                    INPUT IF fg-bin.onHold THEN "Hold" ELSE "Not on Hold"
                    ).
                 scInstance:SetValue(
                    INPUT "TagStatusDescription",
                    INPUT cStatusDescription 
                    ).   
                 scInstance:SetValue(
                    INPUT "ItemName",
                    INPUT cItemName          
                    ).                                                                                         
                RUN displayMessageQuestionLog(
                    INPUT "55",
                    OUTPUT lResponse
                    ).   
                scInstance:DeleteValue(
                    INPUT "StockID"
                    ).  
                scInstance:DeleteValue(
                    INPUT "TagStatus"
                    ).
                scInstance:DeleteValue(
                    INPUT "TagStatusDescription"
                    ). 
                 scInstance:DeleteValue(
                    INPUT "ItemName"
                    ). 
                IF lResponse THEN 
                    ASSIGN
                        fiTagStatus:SENSITIVE  = TRUE
                        btnKeyboard2:SENSITIVE = TRUE
                        opcReturnFocus         = "TagStatus"
                        .
                ELSE                                                                                       
                    opcReturnFocus = "TagNo".        
            END.               
        END.
        ELSE DO:
            IF cSSTagStatus NE "" THEN DO:
                RUN Inventory_ValidateStatusID in hdInventoryProcs(
                    INPUT  cSSTagStatus,
                    OUTPUT lValidStatusID
                    ). 
                IF lValidStatusID THEN DO:
                    RUN Inventory_GetStatusOnHold IN hdInventoryProcs(
                        INPUT  cSSTagStatus,
                        OUTPUT lOnHold
                        ).
                    RUN pUpdateBinStatus(
                        INPUT g_company,
                        INPUT fiTagNo:SCREEN-VALUE,
                        INPUT cSSTagStatus,
                        INPUT lOnHold
                        ).
                    opcReturnFocus = "TagNo".                            
                END.  
                ELSE     
                    MESSAGE "Invalid Status ID defined in SSTagStatus NK1 character value"
                        VIEW-AS ALERT-BOX ERROR.                  
            END.
            ELSE 
                ASSIGN
                    fiTagStatus:SENSITIVE  = TRUE
                    btnKeyBoard2:SENSITIVE = TRUE
                    opcReturnFocus         = "TagStatus"
                    .                               
        END.     
    END.
    ELSE IF AMBIGUOUS(fg-bin) THEN
        ASSIGN
            fiTagStatus:SENSITIVE  = TRUE
            btnKeyBoard2:SENSITIVE = TRUE
            opcReturnFocus         = "TagStatus"
            .     
                     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateBinStatus W-Win 
PROCEDURE pUpdateBinStatus PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTag      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcStatusID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplOnHold   AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER bf-fg-bin FOR fg-bin.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    FOR EACH bf-fg-bin EXCLUSIVE-LOCK
        WHERE bf-fg-bin.company EQ ipcCompany
          AND bf-fg-bin.tag     EQ ipcTag:
        ASSIGN
            bf-fg-bin.statusID = ipcStatusID
            bf-fg-bin.onHold   = iplOnHold
            .
       
        RUN pCreateTTRecord(
            INPUT ipcCompany,
            INPUT bf-fg-bin.qty,
            INPUT bf-fg-bin.loc,
            INPUT bf-fg-bin.tag,
            INPUT bf-fg-bin.job-no,
            INPUT bf-fg-bin.po-no,
            INPUT ipcStatusID
            ).
                   
        RELEASE bf-fg-bin.                      
    END.
    ASSIGN
        fiTagStatus:SENSITIVE            = FALSE
        btnKeyboard2:SENSITIVE           = FALSE
        fiStatusDescription:SCREEN-VALUE = ""
        fiTagStatus:SCREEN-VALUE         = ""
        fiTagNo:SCREEN-VALUE             = ""
        .          
    {&OPEN-BROWSERS-IN-QUERY-F-Main}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidateTag W-Win 
PROCEDURE pValidateTag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcTagNo   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    IF ipcTagNo EQ "" THEN DO: 
/*        ASSIGN                                  */
/*            oplSuccess = NO                     */
/*            opcMessage = "Blank Tag not allowed"*/
/*            .                                   */
        RETURN.        
    END.        
    ELSE IF NOT CAN-FIND(FIRST fg-bin 
                        WHERE fg-bin.company EQ "001"
                          AND fg-bin.tag     EQ ipcTagNo) THEN DO:
        ASSIGN
            oplSuccess = NO
            opcMessage = "Invalid Tag No " + TRIM(ipcTagNo) + " , Please enter a valid tag"
            .
        RETURN.                                              
    END. 
    oplSuccess = YES.           
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
  {src/adm/template/snd-list.i "ttFGBin"}

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

