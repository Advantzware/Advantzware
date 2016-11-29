&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File:              mfvalues.w

  Description:       Misc. Field Values

  Author:            Ron Stark

  Created:           03/01/98 (updated 11.28.2016)

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
DEFINE INPUT PARAMETER ip-group   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ip-rec_key AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ip-header  AS CHARACTER NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{custom/mfvalues.i}
{methods/defines/miscflds.i &NEW="NEW"}

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
DEFINE VARIABLE folderHeight  AS INTEGER       NO-UNDO.
DEFINE VARIABLE folderWidth   AS INTEGER       NO-UNDO.
DEFINE VARIABLE saveHeight    AS INTEGER       NO-UNDO.
DEFINE VARIABLE saveWidth     AS INTEGER       NO-UNDO.

DEFINE TEMP-TABLE t-mfvalues NO-UNDO LIKE {&dbnm}mfvalues.

&scoped-define trigger-code ~
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
&Scoped-Define ENABLED-OBJECTS btnExit btnApply Rect-Top Rect-Left btnSave ~
Rect-Right Rect-Bottom 
&Scoped-Define DISPLAYED-OBJECTS mfgroupList mfgroupLabel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnApply 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk_window.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Apply" 
     SIZE 8 BY 1.91 TOOLTIP "Save"
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
     SIZE 58.2 BY 1 NO-UNDO.

DEFINE VARIABLE mfgroupLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Group:" 
      VIEW-AS TEXT 
     SIZE 7 BY 1 NO-UNDO.

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
     btnExit AT ROW 21.48 COL 84 HELP
          "Exit Design Layout Window" WIDGET-ID 2
     btnApply AT ROW 21.48 COL 68 HELP
          "Apply" WIDGET-ID 4
     btnSave AT ROW 21.48 COL 76 HELP
          "Save" WIDGET-ID 6
     mfgroupList AT ROW 21.95 COL 7 COLON-ALIGNED HELP
          "Select Group Name" NO-LABEL
     mfgroupLabel AT ROW 21.95 COL 2 NO-LABEL
     Rect-Main AT ROW 2.05 COL 1.6
     Rect-Top AT ROW 2.1 COL 1.8
     Rect-Left AT ROW 2.14 COL 1.8
     Rect-Right AT ROW 2.24 COL 91.2
     Rect-Bottom AT ROW 21.1 COL 1.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 91.2 BY 22.57.


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
         TITLE              = "User Defined Field Values"
         HEIGHT             = 22.57
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
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
ON END-ERROR OF C-Win /* User Defined Field Values */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* User Defined Field Values */
DO:
  IF mfpersist NE ? THEN DELETE PROCEDURE mfpersist.
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnApply
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnApply C-Win
ON CHOOSE OF btnApply IN FRAME DEFAULT-FRAME /* Apply */
DO:
  RUN applyChanges.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExit C-Win
ON CHOOSE OF btnExit IN FRAME DEFAULT-FRAME /* Exit */
DO:
  IF mfpersist NE ? THEN DELETE PROCEDURE mfpersist.
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME /* Save */
DO:
    RUN applyChanges.
    IF RETURN-VALUE NE "NO-APPLY" THEN
    APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mfgroupList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mfgroupList C-Win
ON VALUE-CHANGED OF mfgroupList IN FRAME DEFAULT-FRAME
DO:
  {methods/wait.i}
  FOR EACH attrb
      WHERE attrb.attr_mfgroup EQ mfgroupList:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        AND attrb.attr_type NE "TEXT" NO-LOCK:
    FIND t-mfvalues NO-LOCK
         WHERE t-mfvalues.rec_key EQ ip-rec_key
           AND t-mfvalues.mf_id EQ attrb.attr_id NO-ERROR.
    IF AVAILABLE t-mfvalues THEN NEXT.
    CREATE t-mfvalues.
    ASSIGN
      t-mfvalues.rec_key = ip-rec_key
      t-mfvalues.mf_id = attrb.attr_id
      t-mfvalues.mf_value = attrb.attr_default
      t-mfvalues.mf_datatype = attrb.attr_datatype.
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
      WHERE {&dbnm}mfvalues.rec_key = ip-rec_key
      :
    CREATE t-mfvalues.
    BUFFER-COPY {&dbnm}mfvalues TO t-mfvalues.
  END.
  IF mfpersist EQ ? THEN
  RUN nosweat/mfpersist.p PERSISTENT SET mfpersist.
  RUN loadWidgetData.
  {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + " - " + ip-header.
  {methods/nowait.i}
  IF RETURN-VALUE EQ "EMPTY" THEN
  DISABLE btnSave btnApply WITH FRAME {&FRAME-NAME}.
  ELSE
  APPLY "ENTRY" TO btnSave IN FRAME {&FRAME-NAME}.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applyChanges C-Win 
PROCEDURE applyChanges :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF mfgroupList:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ ? AND ip-group EQ "" THEN RETURN.
  IF mfgroupList:SCREEN-VALUE NE ip-group AND ip-group NE "" THEN DO:
    ldummy = NO.
    MESSAGE "The Group Name has been changed from '" ip-group "' to '"
            mfgroupList:SCREEN-VALUE IN FRAME {&FRAME-NAME} "'" SKIP(1)
            "Attribute Values for Group '" ip-group
            "' will be Deleted, Continue? (yes/no)"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ldummy.
    IF NOT ldummy THEN RETURN "NO-APPLY".
  END.
  {methods/wait.i}
  IF mfgroupList:SCREEN-VALUE NE ip-group THEN DO WITH FRAME {&FRAME-NAME}:
    FOR EACH t-mfvalues WHERE t-mfvalues.rec_key EQ ip-rec_key EXCLUSIVE-LOCK:
      IF CAN-FIND(attrb WHERE attrb.attr_id EQ t-mfvalues.mf_id
                          AND attrb.attr_mfgroup EQ ip-group) THEN
      DELETE t-mfvalues.
    END.
    ip-group = IF mfgroupList:SCREEN-VALUE EQ ? THEN ""
               ELSE mfgroupList:SCREEN-VALUE.
  END.
  RUN widgetLeave (?).
  DELETE FROM {&dbnm}mfvalues WHERE {&dbnm}mfvalues.rec_key = ip-rec_key.
  FOR EACH t-mfvalues NO-LOCK WHERE t-mfvalues.rec_key EQ ip-rec_key:
    CREATE {&dbnm}mfvalues.
    BUFFER-COPY t-mfvalues TO {&dbnm}mfvalues.
  END.
  {methods/nowait.i}
  btnExit:LABEL IN FRAME {&FRAME-NAME} = "&Close".
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
      labelWidget:HIDDEN = NO.
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

  RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
  DELETE WIDGET-POOL "widget-pool-tabs" NO-ERROR.
  CREATE WIDGET-POOL "widget-pool-tabs" PERSISTENT.
  FIND {&dbnm}mfgroup NO-LOCK
       WHERE {&dbnm}mfgroup.mfgroup_data EQ mfgroupList:SCREEN-VALUE IN FRAME {&FRAME-NAME}
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
    ldummy = tabLabel[currentTab]:MOVE-TO-TOP().
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
  FOR EACH attrb NO-LOCK
      WHERE attrb.attr_mfgroup EQ mfgroupList:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        AND attrb.attr_tab EQ currentTab
      BY attrb.attr_order
      :
    RUN dynamicWidget.
  END.
  RUN moveObjects.
  /* RUN LockWindowUpdate (0,OUTPUT i). */
  {methods/nowait.i}

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

&scoped-define defaults-code ~
          ASSIGN ~
            FRAME = FRAME {&FRAME-NAME}:HANDLE ~
            X = attrb.attr_x ~
            Y = attrb.attr_y ~
            SENSITIVE = YES ~
            PRIVATE-DATA = STRING(attrb.attr_id) ~
            WIDTH-PIXELS = attrb.attr_width
&scoped-define widget-code ~
            {&defaults-code} ~
            FONT = ? ~
            HEIGHT-PIXELS = attrb.attr_height ~
            SIDE-LABEL-HANDLE = labelWidget
&scoped-define combo-box-code ~
            {&defaults-code} ~
            HIDDEN = yes ~
            FORMAT = attrb.attr_settings ~
            FONT = ? ~
            SIDE-LABEL-HANDLE = labelWidget ~
            INNER-LINES = NUM-ENTRIES(attrb.attr_values) ~
            LIST-ITEMS = attrb.attr_values ~
            HELP = 'Select ''' + attrb.attr_label + ''' from COMBO-BOX List' ~
            SCREEN-VALUE = t-mfvalues.mf_value ~
            {&trigger-code}
&scoped-define editor-code ~
            {&widget-code} ~
            FONT = ? ~
            SCROLLBAR-VERTICAL = vertBar ~
            SCROLLBAR-HORIZONTAL = horzBar ~
            HELP = 'Enter ''' + attrb.attr_label + ''' in EDITOR Field' ~
            SCREEN-VALUE = t-mfvalues.mf_value ~
            {&trigger-code}
&scoped-define fill-in-code ~
            {&widget-code} ~
            FONT = ? ~
            DATA-TYPE = attrb.attr_datatype ~
            FORMAT = attrb.attr_settings ~
            HELP = 'Enter ''' + attrb.attr_label + ''' in FILL-IN Field' ~
            SCREEN-VALUE = t-mfvalues.mf_value ~
            {&trigger-code}
&scoped-define radio-set-code ~
            {&widget-code} ~
            FONT = ? ~
            HORIZONTAL = horzBar ~
            RADIO-BUTTONS = attrb.attr_values ~
            HELP = 'Select ''' + attrb.attr_label + ''' from RADIO-SET Choices' ~
            SCREEN-VALUE = t-mfvalues.mf_value ~
            {&trigger-code}
&scoped-define rectangle-code ~
            {&defaults-code} ~
            EDGE-PIXELS = 1 ~
            FILLED = NO ~
            HEIGHT-PIXELS = attrb.attr_height ~
            WIDTH-PIXELS = attrb.attr_width ~
            {&trigger-code}
&scoped-define selection-list-code ~
            {&widget-code} ~
            FONT = ? ~
            LIST-ITEMS = attrb.attr_values ~
            HELP = 'Select ''' + attrb.attr_label + ''' from SELECTION-LIST' ~
            SCREEN-VALUE = t-mfvalues.mf_value ~
            {&trigger-code}
&scoped-define slider-code ~
            {&widget-code} ~
            FONT = ? ~
            HORIZONTAL = horzBar ~
            MIN-VALUE = INT(ENTRY(1,attrb.attr_values)) ~
            MAX-VALUE = INT(ENTRY(2,attrb.attr_values)) ~
            HELP = 'Set ''' + attrb.attr_label + ''' from SLIDER Range' ~
            SCREEN-VALUE = t-mfvalues.mf_value ~
            {&trigger-code}
&scoped-define text-code ~
            {&defaults-code} ~
            PRIVATE-DATA = ? ~
            FORMAT = 'X(80)' ~
            FONT = INTEGER(attrb.attr_settings) ~
            HEIGHT-PIXELS = attrb.attr_height ~
            HELP = '''' + attrb.attr_label + ''' is a TEXT Widget' ~
            SCREEN-VALUE = attrb.attr_label.
&scoped-define toggle-box-code ~
            {&defaults-code} ~
            FONT = ? ~
            LABEL = attrb.attr_label ~
            HEIGHT-PIXELS = attrb.attr_height ~
            HELP = 'Set ''' + attrb.attr_label + ''' TOGGLE-BOX On/Off' ~
            SCREEN-VALUE = t-mfvalues.mf_value ~
            {&trigger-code}

  ASSIGN
    horzBar = IF NUM-ENTRIES(attrb.attr_settings) NE 0 AND
                 ENTRY(1,attrb.attr_settings) EQ "yes" THEN YES ELSE NO
    vertBar = IF NUM-ENTRIES(attrb.attr_settings) GT 1 AND
                 ENTRY(2,attrb.attr_settings) EQ "yes" THEN YES ELSE NO
    folderHeight = MAX(folderHeight,attrb.attr_y + attrb.attr_height)
    folderWidth = MAX(folderWidth,attrb.attr_x + attrb.attr_width)
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
  RUN createLabelWidget (attrb.attr_type,attrb.attr_label,attrb.attr_x,attrb.attr_y).
  FIND t-mfvalues NO-LOCK
       WHERE t-mfvalues.rec_key = ip-rec_key
         AND t-mfvalues.mf_id = attrb.attr_id NO-ERROR.
  DO ON ERROR UNDO, RETURN "CREATE-ERROR":
    CASE attrb.attr_type:
&scoped-define widget-type COMBO-BOX
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "attr-widget"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type EDITOR
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "attr-widget"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type FILL-IN
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "attr-widget"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type RADIO-SET
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "attr-widget"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type RECTANGLE
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "attr-widget"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type SELECTION-LIST
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "attr-widget"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type SLIDER
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "attr-widget"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type TEXT
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "attr-widget"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type TOGGLE-BOX
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "attr-widget"
        {&{&widget-type}-code}
      END.
    END CASE.
    IF VALID-HANDLE(dynWidget) THEN
    ASSIGN
        dynWidget:SENSITIVE = attrb.attr_enabled
        ldummy = dynWidget:MOVE-TO-TOP()
        dynWidget:HIDDEN = NO
        .
    IF mfpersist NE ? AND attrb.attr_proc NE "" AND attrb.attr_proc NE ? THEN DO:
      RUN VALUE(attrb.attr_proc) IN mfpersist
          (t-mfvalues.mf_id,t-mfvalues.mf_value,t-mfvalues.rec_key,
           OUTPUT sv,OUTPUT initsv).
      IF CAN-DO("COMBO-BOX,SELECTION-LIST",attrb.attr_type) THEN
      ASSIGN
        dynWidget:LIST-ITEMS = sv
        dynWidget:SCREEN-VALUE = IF CAN-DO(sv,initsv) THEN initsv
                            ELSE IF CAN-DO(sv,t-mfvalues.mf_value) THEN t-mfvalues.mf_value
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
  DISPLAY mfgroupList mfgroupLabel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnExit btnApply Rect-Top Rect-Left btnSave Rect-Right Rect-Bottom 
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
    MESSAGE "No '" + ip-group + "' Group Exists!!!" VIEW-AS ALERT-BOX INFORMATION.
    RETURN "EMPTY".
  END.
  FOR EACH {&dbnm}mfgroup NO-LOCK:
    mfgrpList = mfgrpList + (IF mfgrpList NE "" THEN "," ELSE "") + {&dbnm}mfgroup.mfgroup_data.
  END.
  IF NOT CAN-DO(mfgrpList,ip-group) THEN DO:
    MESSAGE "No '" + ip-group + "' Group Exists!!!" VIEW-AS ALERT-BOX INFORMATION.
    RETURN "EMPTY".
  END.
  OUTPUT TO VALUE("users/" + USERID("NOSWEAT") + "/miscflds.dat").
  FOR EACH {&dbnm}mfdata NO-LOCK:
    PUT UNFORMATTED {&dbnm}mfdata.miscflds_data SKIP.
  END.
  OUTPUT CLOSE.
  INPUT FROM VALUE("users/" + USERID("NOSWEAT") + "/miscflds.dat") NO-ECHO.
  REPEAT:
    CREATE attrb.
    IMPORT attrb.
  END.
  INPUT CLOSE.
  IF attrb.attr_type = "" THEN DELETE attrb.
  ASSIGN
    mfgroupList:INNER-LINES IN FRAME {&FRAME-NAME} = NUM-ENTRIES(mfgrpList)
    mfgroupList:LIST-ITEMS IN FRAME {&FRAME-NAME} = mfgrpList
    mfgroupList:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ip-group
    /* headerValue:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ip-header */
    .
  APPLY "VALUE-CHANGED" TO mfgroupList IN FRAME {&FRAME-NAME}.

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
            FRAME {&FRAME-NAME}:HEIGHT-PIXELS = iHeight + btnExit:HEIGHT-PIXELS + 30
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
            /* headerValue:WIDTH-PIXELS = Rect-Main:WIDTH-PIXELS */
            /* headerValue:Y = Rect-Main:HEIGHT-PIXELS + 26 */
            mfgroupLabel:Y = Rect-Main:HEIGHT-PIXELS + 36
            mfgroupList:Y = Rect-Main:HEIGHT-PIXELS + 36
            btnSave:Y = Rect-Main:HEIGHT-PIXELS + 26
            btnApply:Y = Rect-Main:HEIGHT-PIXELS + 26
            btnExit:Y = Rect-Main:HEIGHT-PIXELS + 26
            {&WINDOW-NAME}:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS
            {&WINDOW-NAME}:WIDTH-PIXELS = FRAME {&FRAME-NAME}:WIDTH-PIXELS
            {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
            {&WINDOW-NAME}:VIRTUAL-WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
            .
    END.

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

  FIND t-mfvalues EXCLUSIVE-LOCK
      WHERE t-mfvalues.rec_key = ip-rec_key
        AND t-mfvalues.mf_id = attribute:PRIVATE-DATA.
  ASSIGN
    t-mfvalues.mf_value = attribute:SCREEN-VALUE
    editorWidget = ?.

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

  IF VALID-HANDLE(attribute) THEN RUN valueChange (attribute:HANDLE).
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
  DEFINE VARIABLE offSet AS INTEGER NO-UNDO.

  /*
  {&WINDOW-NAME}:WINDOW-STATE = 1.
  offSet = IF {&WINDOW-NAME}:HEIGHT-PIXELS GT 600 THEN 60 ELSE 0.
  */
  offSet = 60.
  IF {&WINDOW-NAME}:HEIGHT-PIXELS LT 600 THEN {&WINDOW-NAME}:HEIGHT-PIXELS = 600.
  IF {&WINDOW-NAME}:WIDTH-PIXELS LT 800 THEN {&WINDOW-NAME}:WIDTH-PIXELS = 800.
  ASSIGN
    {&WINDOW-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS - offSet
    {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
    FRAME {&FRAME-NAME}:WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
    FRAME {&FRAME-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

