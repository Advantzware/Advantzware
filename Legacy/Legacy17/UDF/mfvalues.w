&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File:              mfvalues.w

  Description:       Misc. Field Values

  Author:            Ron Stark

  Created:           03/01/98 (updated 11.29.2016)

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

&IF DEFINED(dbnm) EQ 0 &THEN
DEFINE INPUT PARAMETER ipcGroup    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcRecKey   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcHeader   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iphSmartMsg AS HANDLE    NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{custom/mfvalues.i}
{UDF/mfttdefs.i &NEW="NEW SHARED"}

DEFINE VARIABLE tabImage      AS WIDGET-HANDLE NO-UNDO EXTENT 18.
DEFINE VARIABLE tabLabel      AS WIDGET-HANDLE NO-UNDO EXTENT 18.
DEFINE VARIABLE labelWidget   AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE dynWidget     AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE currentWidget AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE editorWidget  AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE mfgrpList     AS CHARACTER     NO-UNDO.
DEFINE VARIABLE tabLabels     AS CHARACTER     NO-UNDO.
DEFINE VARIABLE ldummy        AS LOGICAL       NO-UNDO.
DEFINE VARIABLE currentTab    AS INTEGER       NO-UNDO.
DEFINE VARIABLE i             AS INTEGER       NO-UNDO.
DEFINE VARIABLE folderHeight  AS INTEGER       NO-UNDO INITIAL 50.
DEFINE VARIABLE folderWidth   AS INTEGER       NO-UNDO INITIAL 500.

DEFINE TEMP-TABLE ttMFValues NO-UNDO LIKE {&dbnm}mfvalues.

&SCOPED-DEFINE trigger-code ~
TRIGGERS: ~
  ON VALUE-CHANGED ~
    PERSISTENT RUN valueChange IN THIS-PROCEDURE (dynWidget:HANDLE). ~
  ON ENTRY ~
    PERSISTENT RUN widgetEntry IN THIS-PROCEDURE (dynWidget:HANDLE). ~
  ON LEAVE ~
    PERSISTENT RUN widgetLeave IN THIS-PROCEDURE (dynWidget:HANDLE). ~
END TRIGGERS.

{methods/lockWindowUpdate.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnDelete btnSave Rect-Top Rect-Left ~
Rect-Right Rect-Bottom btnExit 
&Scoped-Define DISPLAYED-OBJECTS mfgroupList mfRecKey mfgroupLabel ~
mfRecKeyLabel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnDelete 
     IMAGE-UP FILE "Graphics/32x32/delete.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Delete" 
     SIZE 8 BY 1.91 TOOLTIP "Delete"
     FONT 4.

DEFINE BUTTON btnExit 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "E&xit" 
     SIZE 8 BY 1.91 TOOLTIP "Exit"
     FONT 4.

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Save" 
     SIZE 8 BY 1.91 TOOLTIP "Save"
     FONT 4.

DEFINE VARIABLE mfgroupList AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 1
     DROP-DOWN-LIST
     SIZE 56.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE mfgroupLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Group:" 
      VIEW-AS TEXT 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE mfRecKey AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE mfRecKeyLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Rec Key:" 
      VIEW-AS TEXT 
     SIZE 9 BY 1 NO-UNDO.

DEFINE RECTANGLE Rect-Bottom
     EDGE-PIXELS 0    
     SIZE 90 BY .19
     BGCOLOR 7 .

DEFINE RECTANGLE Rect-Left
     EDGE-PIXELS 0    
     SIZE .6 BY 19
     BGCOLOR 15 .

DEFINE RECTANGLE Rect-Main
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 90.4 BY 19.24
     BGCOLOR 8 FGCOLOR 0 .

DEFINE RECTANGLE Rect-Right
     EDGE-PIXELS 0    
     SIZE .6 BY 18.86
     BGCOLOR 7 .

DEFINE RECTANGLE Rect-Top
     EDGE-PIXELS 0    
     SIZE 90 BY .19
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnDelete AT ROW 21.71 COL 68 HELP
          "Delete" WIDGET-ID 4
     btnSave AT ROW 21.71 COL 76 HELP
          "Save" WIDGET-ID 6
     btnExit AT ROW 21.71 COL 84 HELP
          "Exit Design Layout Window" WIDGET-ID 2
     mfgroupList AT ROW 21.48 COL 9 COLON-ALIGNED HELP
          "Select Group Name" NO-LABEL
     mfRecKey AT ROW 22.67 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     mfgroupLabel AT ROW 21.48 COL 3 NO-LABEL
     mfRecKeyLabel AT ROW 22.67 COL 1 NO-LABEL WIDGET-ID 10
     Rect-Main AT ROW 2.05 COL 1.6
     Rect-Top AT ROW 2.1 COL 1.8
     Rect-Left AT ROW 2.14 COL 1.8
     Rect-Right AT ROW 2.24 COL 91.2
     Rect-Bottom AT ROW 21.1 COL 1.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 91.2 BY 22.67.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "User Defined Fields Viewer"
         HEIGHT             = 22.67
         WIDTH              = 91.2
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN mfgroupLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR COMBO-BOX mfgroupList IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mfRecKey IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       mfRecKey:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN mfRecKeyLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR RECTANGLE Rect-Main IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* User Defined Fields Viewer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* User Defined Fields Viewer */
DO:
  IF hMFPersist NE ? THEN DELETE PROCEDURE hMFPersist.
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete C-Win
ON CHOOSE OF btnDelete IN FRAME DEFAULT-FRAME /* Delete */
DO:
  RUN deleteValues.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExit C-Win
ON CHOOSE OF btnExit IN FRAME DEFAULT-FRAME /* Exit */
DO:
  IF hMFPersist NE ? THEN DELETE PROCEDURE hMFPersist.
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME /* Save */
DO:
  RUN saveValues.
  IF VALID-HANDLE(iphSmartMsg) THEN
  RUN Show-MF-Message IN iphSmartMsg (YES).
  MESSAGE "UDF Values Saved" VIEW-AS ALERT-BOX TITLE "Save".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mfgroupList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mfgroupList C-Win
ON VALUE-CHANGED OF mfgroupList IN FRAME DEFAULT-FRAME
DO:
  {methods/wait.i}
  FOR EACH ttAttrb
      WHERE ttAttrb.attr_mfgroup EQ mfgroupList:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        AND ttAttrb.attr_type NE "TEXT"
        AND ttAttrb.attr_type NE "RECTANGLE"
      :
    FIND ttMFValues
        WHERE ttMFValues.rec_key EQ ipcRecKey
          AND ttMFValues.mf_id EQ ttAttrb.attr_id
        NO-ERROR.
    IF AVAILABLE ttMFValues THEN NEXT.
    CREATE ttMFValues.
    ASSIGN
      ttMFValues.rec_key = ipcRecKey
      ttMFValues.mf_id = ttAttrb.attr_id
      ttMFValues.mf_value = ttAttrb.attr_default
      ttMFValues.mf_datatype = ttAttrb.attr_datatype
      .
  END.
  currentTab = 1.
  RUN createTabs.
  RUN createWidgets.
  {methods/nowait.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN winReSize.
  {methods/wait.i}
  FOR EACH {&dbnm}mfvalues NO-LOCK
      WHERE {&dbnm}mfvalues.rec_key = ipcRecKey
      :
    CREATE ttMFValues.
    BUFFER-COPY {&dbnm}mfvalues TO ttMFValues.
  END.
  IF hMFPersist EQ ? THEN
  RUN UDF/mfPersist.p PERSISTENT SET hMFPersist.
  RUN loadWidgetData.
  {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + " - " + ipcHeader.
  {methods/nowait.i}
  IF RETURN-VALUE EQ "EMPTY" THEN
  DISABLE btnSave btnDelete WITH FRAME {&FRAME-NAME}.
  ELSE
  APPLY "ENTRY" TO btnSave IN FRAME {&FRAME-NAME}.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createLabelWidget C-Win 
PROCEDURE createLabelWidget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipType AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipLabel AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipX AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipY AS INTEGER NO-UNDO.

  IF NOT CAN-DO("Text,Toggle-Box",ipType) THEN DO: /* text and toggle-box have no labels */
    CREATE TEXT labelWidget IN WIDGET-POOL "attr-widget"
      ASSIGN
        FRAME = FRAME {&FRAME-NAME}:HANDLE
        AUTO-RESIZE = YES
        Y = ipY
        FORMAT = "X(" +
          (IF LENGTH(ipLabel) NE 0 THEN STRING(LENGTH(ipLabel) + 1)
          ELSE "1") + ")"
        SENSITIVE = YES
        SCREEN-VALUE = IF ipLabel EQ "" THEN "" ELSE ipLabel + ":".
    ASSIGN
      labelWidget:FONT = ? /* 8 */
      labelWidget:X = IF ipX - labelWidget:WIDTH-PIXELS - 1 LT 0 THEN 0
                      ELSE ipX - labelWidget:WIDTH-PIXELS - 1
      labelWidget:HEIGHT-CHARS = 1
      ldummy = labelWidget:MOVE-TO-TOP()
      labelWidget:HIDDEN = NO
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createTabs C-Win 
PROCEDURE createTabs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  IF Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive EQ FALSE THEN
  RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
  DELETE WIDGET-POOL "widget-pool-tabs" NO-ERROR.
  CREATE WIDGET-POOL "widget-pool-tabs" PERSISTENT.
  FIND {&dbnm}mfgroup NO-LOCK
       WHERE {&dbnm}mfgroup.mfgroup_data BEGINS mfgroupList:SCREEN-VALUE IN FRAME {&FRAME-NAME}
       NO-ERROR.
  IF NOT AVAILABLE {&dbnm}mfgroup THEN RETURN.
  DO i = 1 TO NUM-ENTRIES({&dbnm}mfgroup.mfgroup_tabs):
      folderWidth = MAX(folderWidth,3 + (i - 1) * 72 + 72).
      IF folderWidth + 15 GT FRAME {&FRAME-NAME}:WIDTH-PIXELS THEN
      ASSIGN
        {&WINDOW-NAME}:WIDTH-PIXELS = folderWidth + 10
        {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
        FRAME {&FRAME-NAME}:WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
        FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
        .
    CREATE IMAGE tabImage[i] IN WIDGET-POOL "widget-pool-tabs"
      ASSIGN
        FRAME = FRAME {&FRAME-NAME}:HANDLE
        X = 3 + (i - 1) * 72
        Y = 1
        WIDTH-PIXEL = 72
        HEIGHT-PIXEL = 24
        SENSITIVE = YES
      TRIGGERS:      
        ON MOUSE-SELECT-CLICK 
           PERSISTENT RUN labelTrigger IN THIS-PROCEDURE (i).
      END TRIGGERS.         
      CREATE TEXT tabLabel[i] IN WIDGET-POOL "widget-pool-tabs"
        ASSIGN 
          FRAME = FRAME {&FRAME-NAME}:HANDLE
          X = tabImage[i]:X + 9
          Y = tabImage[i]:Y + 5
          WIDTH-PIXEL = tabImage[i]:WIDTH-PIXEL - 16
          HEIGHT-PIXEL = tabImage[i]:HEIGHT-PIXEL - 12
          FORMAT = "X(13)":U
          SENSITIVE = YES 
          FONT = ?
          BGCOLOR = 8
          SCREEN-VALUE = ENTRY(1,ENTRY(i,{&dbnm}mfgroup.mfgroup_tabs),"|")
        TRIGGERS:      
          ON MOUSE-SELECT-CLICK 
             PERSISTENT RUN labelTrigger IN THIS-PROCEDURE (i).
        END TRIGGERS.
      /* show tabs as down tabs initially */
      ASSIGN      
        ldummy = tabImage[i]:LOAD-IMAGE("adeicon/ts-dn72")
        ldummy = tabImage[i]:MOVE-TO-TOP()
        ldummy = tabLabel[i]:MOVE-TO-TOP()
        tabImage[i]:HIDDEN = NO
        tabLabel[i]:HIDDEN = NO
        .
  END.
  /* use up tab image for currently selected tab */
  ASSIGN
    tabImage[currentTab]:HEIGHT-PIXEL = 27
    ldummy = tabImage[currentTab]:LOAD-IMAGE("adeicon/ts-up72")
    ldummy = tabImage[currentTab]:MOVE-TO-TOP()
    ldummy = tabLabel[currentTab]:MOVE-TO-TOP()
    .
  RUN LockWindowUpdate (0,OUTPUT i).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createWidgets C-Win 
PROCEDURE createWidgets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  {methods/wait.i}
  /* RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i). */
  DELETE WIDGET-POOL "attr-widget" NO-ERROR.
  CREATE WIDGET-POOL "attr-widget" PERSISTENT.
  FOR EACH ttAttrb NO-LOCK
      WHERE ttAttrb.attr_mfgroup EQ mfgroupList:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        AND ttAttrb.attr_tab EQ currentTab
      BY ttAttrb.attr_order
      :
    RUN dynamicWidget.
  END.
  RUN moveObjects.
  /* RUN LockWindowUpdate (0,OUTPUT i). */
  {methods/nowait.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteValues C-Win 
PROCEDURE deleteValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF mfgroupList:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ ? AND ipcGroup EQ "" THEN RETURN.
  MESSAGE "Delete UDF Values?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Delete"
      UPDATE deleteUDF AS LOGICAL.
  IF NOT deleteUDF THEN RETURN.
  {methods/wait.i}
  RUN widgetLeave (?).
  FOR EACH ttMFValues
      WHERE ttMFValues.rec_key EQ ipcRecKey
      :
    DELETE ttMFValues.
  END. /* each ttmfvalues */
  {methods/nowait.i}
  RUN saveValues.
  IF VALID-HANDLE(iphSmartMsg) THEN
  RUN Show-MF-Message IN iphSmartMsg (NO).
  APPLY "CLOSE":U TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dynamicWidget C-Win 
PROCEDURE dynamicWidget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE horzBar AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE vertBar AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE initsv  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE sv      AS CHARACTER NO-UNDO.

&SCOPED-DEFINE defaults-code ~
          ASSIGN ~
            FRAME = FRAME {&FRAME-NAME}:HANDLE ~
            X = ttAttrb.attr_x ~
            Y = ttAttrb.attr_y ~
            SENSITIVE = YES ~
            PRIVATE-DATA = STRING(ttAttrb.attr_id) ~
            WIDTH-PIXELS = ttAttrb.attr_width
&SCOPED-DEFINE widget-code ~
            {&defaults-code} ~
            FONT = ? ~
            HEIGHT-PIXELS = ttAttrb.attr_height ~
            SIDE-LABEL-HANDLE = labelWidget
&SCOPED-DEFINE combo-box-code ~
            {&defaults-code} ~
            HIDDEN = yes ~
            FORMAT = ttAttrb.attr_settings ~
            FONT = ? ~
            SIDE-LABEL-HANDLE = labelWidget ~
            INNER-LINES = NUM-ENTRIES(ttAttrb.attr_values) ~
            LIST-ITEMS = ttAttrb.attr_values ~
            HELP = 'Select ''' + ttAttrb.attr_label + ''' from COMBO-BOX List' ~
            SCREEN-VALUE = ttMFValues.mf_value ~
            {&trigger-code}
&SCOPED-DEFINE editor-code ~
            {&widget-code} ~
            FONT = ? ~
            SCROLLBAR-VERTICAL = vertBar ~
            SCROLLBAR-HORIZONTAL = horzBar ~
            HELP = 'Enter ''' + ttAttrb.attr_label + ''' in EDITOR Field' ~
            SCREEN-VALUE = ttMFValues.mf_value ~
            {&trigger-code}
&SCOPED-DEFINE fill-in-code ~
            {&widget-code} ~
            FONT = ? ~
            DATA-TYPE = ttAttrb.attr_datatype ~
            FORMAT = ttAttrb.attr_settings ~
            HELP = 'Enter ''' + ttAttrb.attr_label + ''' in FILL-IN Field' ~
            SCREEN-VALUE = ttMFValues.mf_value ~
            {&trigger-code}
&SCOPED-DEFINE radio-set-code ~
            {&widget-code} ~
            FONT = ? ~
            HORIZONTAL = horzBar ~
            RADIO-BUTTONS = ttAttrb.attr_values ~
            HELP = 'Select ''' + ttAttrb.attr_label + ''' from RADIO-SET Choices' ~
            SCREEN-VALUE = ttMFValues.mf_value ~
            {&trigger-code}
&SCOPED-DEFINE rectangle-code ~
            {&defaults-code} ~
            EDGE-PIXELS = 1 ~
            FILLED = NO ~
            HEIGHT-PIXELS = ttAttrb.attr_height ~
            WIDTH-PIXELS = ttAttrb.attr_width ~
            {&trigger-code}
&SCOPED-DEFINE selection-list-code ~
            {&widget-code} ~
            FONT = ? ~
            LIST-ITEMS = ttAttrb.attr_values ~
            HELP = 'Select ''' + ttAttrb.attr_label + ''' from SELECTION-LIST' ~
            SCREEN-VALUE = ttMFValues.mf_value ~
            {&trigger-code}
&SCOPED-DEFINE slider-code ~
            {&widget-code} ~
            FONT = ? ~
            HORIZONTAL = horzBar ~
            MIN-VALUE = INT(ENTRY(1,ttAttrb.attr_values)) ~
            MAX-VALUE = INT(ENTRY(2,ttAttrb.attr_values)) ~
            HELP = 'Set ''' + ttAttrb.attr_label + ''' from SLIDER Range' ~
            SCREEN-VALUE = ttMFValues.mf_value ~
            {&trigger-code}
&SCOPED-DEFINE text-code ~
            {&defaults-code} ~
            PRIVATE-DATA = ? ~
            FORMAT = 'X(80)' ~
            FONT = INTEGER(ttAttrb.attr_settings) ~
            HEIGHT-PIXELS = ttAttrb.attr_height ~
            HELP = '''' + ttAttrb.attr_label + ''' is a TEXT Widget' ~
            SCREEN-VALUE = ttAttrb.attr_label.
&SCOPED-DEFINE toggle-box-code ~
            {&defaults-code} ~
            FONT = ? ~
            LABEL = ttAttrb.attr_label ~
            HEIGHT-PIXELS = ttAttrb.attr_height ~
            HELP = 'Set ''' + ttAttrb.attr_label + ''' TOGGLE-BOX On/Off' ~
            SCREEN-VALUE = ttMFValues.mf_value ~
            {&trigger-code}

  ASSIGN
    horzBar = IF NUM-ENTRIES(ttAttrb.attr_settings) NE 0 AND
                 ENTRY(1,ttAttrb.attr_settings) EQ "yes" THEN YES ELSE NO
    vertBar = IF NUM-ENTRIES(ttAttrb.attr_settings) GT 1 AND
                 ENTRY(2,ttAttrb.attr_settings) EQ "yes" THEN YES ELSE NO
    folderHeight = MAX(folderHeight,ttAttrb.attr_y + ttAttrb.attr_height)
    folderWidth = MAX(folderWidth,ttAttrb.attr_x + ttAttrb.attr_width)
    .
  IF folderHeight + btnExit:HEIGHT-PIXELS IN FRAME {&FRAME-NAME} + 15 GT FRAME {&FRAME-NAME}:HEIGHT-PIXELS THEN
  ASSIGN
    {&WINDOW-NAME}:HEIGHT-PIXELS = folderHeight + btnExit:HEIGHT-PIXELS + 30
    {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
    FRAME {&FRAME-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
    FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
    .
  IF folderWidth + 15 GT FRAME {&FRAME-NAME}:WIDTH-PIXELS THEN
  ASSIGN
    {&WINDOW-NAME}:WIDTH-PIXELS = folderWidth + 10
    {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
    FRAME {&FRAME-NAME}:WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
    FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
    .
  RUN createLabelWidget (ttAttrb.attr_type,ttAttrb.attr_label,ttAttrb.attr_x,ttAttrb.attr_y).
  FIND FIRST ttMFValues NO-LOCK
       WHERE ttMFValues.rec_key EQ ipcRecKey
         AND ttMFValues.mf_id EQ ttAttrb.attr_id NO-ERROR.
  DO ON ERROR UNDO, RETURN "CREATE-ERROR":
    CASE ttAttrb.attr_type:
&SCOPED-DEFINE widget-type COMBO-BOX
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "attr-widget"
        {&{&widget-type}-code}
      END.
&SCOPED-DEFINE widget-type EDITOR
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "attr-widget"
        {&{&widget-type}-code}
      END.
&SCOPED-DEFINE widget-type FILL-IN
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "attr-widget"
        {&{&widget-type}-code}
      END.
&SCOPED-DEFINE widget-type RADIO-SET
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "attr-widget"
        {&{&widget-type}-code}
      END.
&SCOPED-DEFINE widget-type RECTANGLE
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "attr-widget"
        {&{&widget-type}-code}
      END.
&SCOPED-DEFINE widget-type SELECTION-LIST
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "attr-widget"
        {&{&widget-type}-code}
      END.
&SCOPED-DEFINE widget-type SLIDER
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "attr-widget"
        {&{&widget-type}-code}
      END.
&SCOPED-DEFINE widget-type TEXT
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "attr-widget"
        {&{&widget-type}-code}
      END.
&SCOPED-DEFINE widget-type TOGGLE-BOX
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "attr-widget"
        {&{&widget-type}-code}
      END.
    END CASE.
    IF VALID-HANDLE(dynWidget) THEN
    ASSIGN
        dynWidget:SENSITIVE = ttAttrb.attr_enabled
        ldummy = dynWidget:MOVE-TO-TOP()
        dynWidget:HIDDEN = NO
        .
    IF hMFPersist NE ? AND ttAttrb.attr_proc NE "" AND ttAttrb.attr_proc NE ? THEN DO:
      RUN VALUE(ttAttrb.attr_proc) IN hMFPersist
          (ttMFValues.mf_id,ttMFValues.mf_value,ttMFValues.rec_key,
           OUTPUT sv,OUTPUT initsv).
      IF CAN-DO("COMBO-BOX,SELECTION-LIST",ttAttrb.attr_type) THEN
      ASSIGN
        dynWidget:LIST-ITEMS = sv
        dynWidget:SCREEN-VALUE = IF CAN-DO(sv,initsv) THEN initsv
                            ELSE IF CAN-DO(sv,ttMFValues.mf_value) THEN ttMFValues.mf_value
                            ELSE ?
        .
      ELSE dynWidget:SCREEN-VALUE = sv.
    END.
  END.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY mfgroupList mfRecKey mfgroupLabel mfRecKeyLabel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnDelete btnSave Rect-Top Rect-Left Rect-Right Rect-Bottom btnExit 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE labelTrigger C-Win 
PROCEDURE labelTrigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER tabno AS INTEGER NO-UNDO.
    
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    ASSIGN
        tabImage[currentTab]:HEIGHT-PIXEL = 24
        ldummy = tabImage[currentTab]:LOAD-IMAGE("adeicon/ts-dn72")
        ldummy = tabImage[currentTab]:MOVE-TO-TOP()
        ldummy = tabLabel[currentTab]:MOVE-TO-TOP()
        currentTab = tabno
        .
    RUN createWidgets.
    ASSIGN
        tabImage[currentTab]:HEIGHT-PIXEL = 27
        ldummy = tabImage[currentTab]:LOAD-IMAGE("adeicon/ts-up72")
        ldummy = tabImage[currentTab]:MOVE-TO-TOP()
        ldummy = tabLabel[currentTab]:MOVE-TO-TOP()
        .
    RUN moveObjects.
    RUN LockWindowUpdate (0,OUTPUT i).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadWidgetData C-Win 
PROCEDURE loadWidgetData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST {&dbnm}mfgroup) THEN DO:
    MESSAGE "No '" + ipcGroup + "' Group Exists!!!" VIEW-AS ALERT-BOX INFORMATION.
    RETURN "EMPTY".
  END.
  FOR EACH {&dbnm}mfgroup NO-LOCK:
    mfgrpList = mfgrpList
              + (IF mfgrpList NE "" THEN "," ELSE "")
              + ENTRY(1,{&dbnm}mfgroup.mfgroup_data,"|").
  END.
  IF NOT CAN-DO(mfgrpList,ipcGroup) THEN DO:
    MESSAGE "No '" + ipcGroup + "' Group Exists!!!" VIEW-AS ALERT-BOX INFORMATION.
    RETURN "EMPTY".
  END.
  OUTPUT TO VALUE("users/" + USERID("NOSWEAT") + "/miscflds.dat").
  FOR EACH {&dbnm}mfdata NO-LOCK:
    PUT UNFORMATTED {&dbnm}mfdata.miscflds_data SKIP.
  END.
  OUTPUT CLOSE.
  INPUT FROM VALUE("users/" + USERID("NOSWEAT") + "/miscflds.dat") NO-ECHO.
  REPEAT:
    CREATE ttAttrb.
    IMPORT ttAttrb.
  END.
  INPUT CLOSE.
  IF ttAttrb.attr_type EQ "" THEN DELETE ttAttrb.
  ASSIGN
    mfgroupList:INNER-LINES IN FRAME {&FRAME-NAME} = NUM-ENTRIES(mfgrpList)
    mfgroupList:LIST-ITEMS = mfgrpList
    mfgroupList:SCREEN-VALUE = ipcGroup
    mfRecKey:SCREEN-VALUE = ipcRecKey
    .
  APPLY "VALUE-CHANGED" TO mfgroupList.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveObjects C-Win 
PROCEDURE moveObjects :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iHeight AS INTEGER NO-UNDO.
    DEFINE VARIABLE iWidth  AS INTEGER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            iHeight = IF folderHeight NE 0 THEN folderHeight - 15
                      ELSE FRAME {&FRAME-NAME}:WIDTH-PIXELS - 6
            iWidth  = IF folderWidth NE 0 THEN folderWidth + 5
                      ELSE FRAME {&FRAME-NAME}:WIDTH-PIXELS - 6
            FRAME {&FRAME-NAME}:HEIGHT-PIXELS = iHeight + btnExit:HEIGHT-PIXELS + 35
            FRAME {&FRAME-NAME}:WIDTH-PIXELS = iWidth + 10
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS = FRAME {&FRAME-NAME}:WIDTH-PIXELS
            Rect-Main:HEIGHT-PIXELS = iHeight
            Rect-Main:WIDTH-PIXELS = iWidth
            Rect-Top:WIDTH-PIXELS = Rect-Main:WIDTH-PIXELS - 2
            Rect-Left:HEIGHT-PIXELS = Rect-Main:HEIGHT-PIXELS - 5
            Rect-Bottom:WIDTH-PIXELS = Rect-Main:WIDTH-PIXELS - 2
            Rect-Bottom:Y = Rect-Main:HEIGHT-PIXELS + 17
            Rect-Right:HEIGHT-PIXELS = Rect-Main:HEIGHT-PIXELS - 8
            Rect-Right:X = Rect-Main:WIDTH-PIXELS - 1
            mfgroupLabel:Y = Rect-Main:HEIGHT-PIXELS + 26
            mfgroupList:Y = Rect-Main:HEIGHT-PIXELS + 26
            mfRecKeyLabel:Y = Rect-Main:HEIGHT-PIXELS + 51
            mfRecKey:Y = Rect-Main:HEIGHT-PIXELS + 51
            btnSave:Y = Rect-Main:HEIGHT-PIXELS + 31
            btnDelete:Y = Rect-Main:HEIGHT-PIXELS + 31
            btnExit:Y = Rect-Main:HEIGHT-PIXELS + 31
            {&WINDOW-NAME}:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS
            {&WINDOW-NAME}:WIDTH-PIXELS = FRAME {&FRAME-NAME}:WIDTH-PIXELS
            {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
            {&WINDOW-NAME}:VIRTUAL-WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
            .
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveValues C-Win 
PROCEDURE saveValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF mfgroupList:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ ? AND ipcGroup EQ "" THEN RETURN.
  IF mfgroupList:SCREEN-VALUE NE ipcGroup AND ipcGroup NE "" THEN DO:
    ldummy = NO.
    MESSAGE "The Group Name has been changed from '" ipcGroup "' to '"
            mfgroupList:SCREEN-VALUE IN FRAME {&FRAME-NAME} "'" SKIP(1)
            "Attribute Values for Group '" ipcGroup
            "' will be Deleted, Continue? (yes/no)"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ldummy.
    IF NOT ldummy THEN RETURN "NO-APPLY".
  END.
  {methods/wait.i}
  IF mfgroupList:SCREEN-VALUE NE ipcGroup THEN DO WITH FRAME {&FRAME-NAME}:
    FOR EACH ttMFValues
        WHERE ttMFValues.rec_key EQ ipcRecKey
        :
      IF CAN-FIND(FIRST ttAttrb
                  WHERE ttAttrb.attr_id EQ ttMFValues.mf_id
                    AND ttAttrb.attr_mfgroup EQ ipcGroup) THEN
      DELETE ttMFValues.
    END.
    ipcGroup = IF mfgroupList:SCREEN-VALUE EQ ? THEN ""
               ELSE mfgroupList:SCREEN-VALUE.
  END.
  RUN widgetLeave (?).
  DELETE FROM {&dbnm}mfvalues WHERE {&dbnm}mfvalues.rec_key = ipcRecKey.
  FOR EACH ttMFValues
      WHERE ttMFValues.rec_key EQ ipcRecKey
      :
    CREATE {&dbnm}mfvalues.
    BUFFER-COPY ttMFValues TO {&dbnm}mfvalues.
  END. /* each ttmfvalues */
  {methods/nowait.i}
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus C-Win 
PROCEDURE Set-Focus :
/*------------------------------------------------------------------------------
  Purpose:     Set Focus
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/setfocus.i btnExit}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valueChange C-Win 
PROCEDURE valueChange :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER attribute AS WIDGET-HANDLE NO-UNDO.

  FIND FIRST ttMFValues EXCLUSIVE-LOCK
       WHERE ttMFValues.rec_key EQ ipcRecKey
         AND ttMFValues.mf_id   EQ attribute:PRIVATE-DATA
       NO-ERROR.
  IF AVAILABLE ttMFValues THEN
  ASSIGN
    ttMFValues.mf_value = attribute:SCREEN-VALUE
    editorWidget = ?
    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE widgetEntry C-Win 
PROCEDURE widgetEntry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER attribute AS WIDGET-HANDLE NO-UNDO.

  IF attribute:TYPE EQ "EDITOR" THEN editorWidget = attribute:HANDLE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE widgetLeave C-Win 
PROCEDURE widgetLeave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER attribute AS WIDGET-HANDLE NO-UNDO.

  IF VALID-HANDLE(attribute)    THEN RUN valueChange (attribute:HANDLE).
  IF VALID-HANDLE(editorWidget) THEN RUN valueChange (editorWidget:HANDLE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE winReSize C-Win 
PROCEDURE winReSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF {&WINDOW-NAME}:HEIGHT-PIXELS LT 600 THEN {&WINDOW-NAME}:HEIGHT-PIXELS = 600.
  IF {&WINDOW-NAME}:WIDTH-PIXELS LT 800 THEN {&WINDOW-NAME}:WIDTH-PIXELS = 800.
  ASSIGN
    {&WINDOW-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS - 60
    {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
    FRAME {&FRAME-NAME}:WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
    FRAME {&FRAME-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

