&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File:              mfvalues.w

  Description:       Misc. Field Values

  Input Parameters:  <none>

  Output Parameters: <none>

  Author:            Ron Stark

  Created:           03/01/98

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

&IF DEFINED(UIB_is_Running) = 0 &THEN
DEFINE INPUT PARAMETER ip-group AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ip-rec_key AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ip-header AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE ip-group AS CHARACTER NO-UNDO.
DEFINE VARIABLE ip-rec_key AS CHARACTER NO-UNDO.
DEFINE VARIABLE ip-header AS CHARACTER NO-UNDO.
FIND prgrms WHERE prgrms.prgmname = "prgrms." NO-LOCK.
ASSIGN
  ip-group = prgrms.mfgroup
  ip-rec_key = prgrms.rec_key
  ip-header = {methods/headers/prgrms.i}.
&ENDIF

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{custom/mfvalues.i}
{methods/defines/miscflds.i &NEW="NEW"}

DEFINE VARIABLE tab-image AS WIDGET-HANDLE EXTENT 6 NO-UNDO.
DEFINE VARIABLE tab-label AS WIDGET-HANDLE EXTENT 6 NO-UNDO.
DEFINE VARIABLE label-widget AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE dyn-widget AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE current-widget AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE editor-widget AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE mfgroup-list AS CHARACTER NO-UNDO.
DEFINE VARIABLE tab-labels AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE current-tab AS INTEGER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE t-mfvalues NO-UNDO LIKE mfvalues.

&scoped-define trigger-code ~
TRIGGERS: ~
  ON VALUE-CHANGED ~
    PERSISTENT RUN Value_Change IN THIS-PROCEDURE (dyn-widget:HANDLE). ~
  ON ENTRY ~
    PERSISTENT RUN Widget_Entry IN THIS-PROCEDURE (dyn-widget:HANDLE). ~
  ON LEAVE ~
    PERSISTENT RUN Widget_Leave IN THIS-PROCEDURE (dyn-widget:HANDLE). ~
END TRIGGERS.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Rect-Top Rect-Left Rect-Right Rect-Bottom ~
Btn_OK Btn_Apply Btn_Close 
&Scoped-Define DISPLAYED-OBJECTS mfgroup_list header_value 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Apply 
     LABEL "&Apply" 
     SIZE 11.2 BY 1.38
     FONT 4.

DEFINE BUTTON Btn_Close 
     LABEL "&Cancel" 
     SIZE 11.2 BY 1.38
     FONT 4.

DEFINE BUTTON Btn_OK 
     LABEL "&OK" 
     SIZE 11.2 BY 1.38
     FONT 4.

DEFINE VARIABLE mfgroup_list AS CHARACTER FORMAT "X(256)":U 
     LABEL "Group" 
     VIEW-AS COMBO-BOX INNER-LINES 1
     LIST-ITEMS "","" 
     SIZE 45.2 BY 1 NO-UNDO.

DEFINE VARIABLE header_value AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 90 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

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
     mfgroup_list AT ROW 22.67 COL 7 COLON-ALIGNED HELP
          "Select Group Name"
     Btn_OK AT ROW 22.67 COL 55.6 HELP
          "Save and Close Window"
     Btn_Apply AT ROW 22.67 COL 68.2 HELP
          "APPLY Changes"
     Btn_Close AT ROW 22.67 COL 80.8 HELP
          "CLOSE Attribute Window"
     header_value AT ROW 21.48 COL 2 NO-LABEL
     Rect-Main AT ROW 2.05 COL 1.6
     Rect-Top AT ROW 2.1 COL 1.8
     Rect-Left AT ROW 2.19 COL 1.8
     Rect-Right AT ROW 2.29 COL 91.2
     Rect-Bottom AT ROW 21.05 COL 1.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 91.6 BY 23.25.


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
         TITLE              = "Misc. Field Values"
         HEIGHT             = 23.24
         WIDTH              = 91.6
         MAX-HEIGHT         = 23.24
         MAX-WIDTH          = 112
         VIRTUAL-HEIGHT     = 23.24
         VIRTUAL-WIDTH      = 112
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

IF NOT C-Win:LOAD-ICON("images\progress":U) THEN
    MESSAGE "Unable to load icon: images\progress"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR FILL-IN header_value IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR COMBO-BOX mfgroup_list IN FRAME DEFAULT-FRAME
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
ON END-ERROR OF C-Win /* Misc. Field Values */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Misc. Field Values */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Apply
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Apply C-Win
ON CHOOSE OF Btn_Apply IN FRAME DEFAULT-FRAME /* Apply */
DO:
  RUN Apply_Changes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Close C-Win
ON CHOOSE OF Btn_Close IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  RUN Apply_Changes.
  IF RETURN-VALUE NE "NO-APPLY" THEN
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mfgroup_list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mfgroup_list C-Win
ON VALUE-CHANGED OF mfgroup_list IN FRAME DEFAULT-FRAME /* Group */
DO:
  {methods/wait.i}
  FOR EACH attrb
      WHERE attrb.attr_mfgroup = mfgroup_list:SCREEN-VALUE IN FRAME {&FRAME-NAME} AND
            attrb.attr_type NE "TEXT" NO-LOCK:
    FIND t-mfvalues
        WHERE t-mfvalues.rec_key = ip-rec_key AND
              t-mfvalues.mf_id = attrb.attr_id NO-LOCK NO-ERROR.
    IF AVAILABLE t-mfvalues THEN
    NEXT.
    CREATE t-mfvalues.
    ASSIGN
      t-mfvalues.rec_key = ip-rec_key
      t-mfvalues.mf_id = attrb.attr_id
      t-mfvalues.mf_value = attrb.attr_default
      t-mfvalues.mf_datatype = attrb.attr_datatype.
  END.
  current-tab = 1.
  RUN Create_Tabs.
  RUN Create_Widgets.
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
  {methods/wait.i}
  FOR EACH mfvalues WHERE mfvalues.rec_key = ip-rec_key NO-LOCK:
    CREATE t-mfvalues.
    BUFFER-COPY mfvalues TO t-mfvalues.
  END.
  RUN Load_Widget_Data.
  {methods/nowait.i}
  IF RETURN-VALUE = "EMPTY" THEN
  DISABLE Btn_OK Btn_Apply WITH FRAME {&FRAME-NAME}.
  ELSE
  APPLY "ENTRY" TO Btn_OK IN FRAME {&FRAME-NAME}.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Apply_Changes C-Win 
PROCEDURE Apply_Changes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF mfgroup_list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ? AND ip-group = "" THEN
  RETURN.
  IF mfgroup_list:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE ip-group AND
     ip-group NE "" THEN
  DO:
    ldummy = NO.
    MESSAGE "The Group Name has been changed from '" ip-group "' to '"
            mfgroup_list:SCREEN-VALUE IN FRAME {&FRAME-NAME} "'" SKIP(1)
            "Attribute Values for Group '" ip-group
            "' will be Deleted, Continue? (yes/no)"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ldummy.
    IF NOT ldummy THEN
    RETURN "NO-APPLY".
  END.
  {methods/wait.i}
  IF mfgroup_list:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE ip-group THEN
  DO WITH FRAME {&FRAME-NAME}:
    FOR EACH t-mfvalues WHERE t-mfvalues.rec_key = ip-rec_key EXCLUSIVE-LOCK:
      IF CAN-FIND(attrb WHERE attrb.attr_id = t-mfvalues.mf_id AND 
                              attrb.attr_mfgroup = ip-group) THEN
      DELETE t-mfvalues.
    END.
    ip-group = IF mfgroup_list:SCREEN-VALUE = ? THEN ""
               ELSE mfgroup_list:SCREEN-VALUE.
  END.
  RUN Widget_Leave (?).
  DELETE FROM mfvalues WHERE mfvalues.rec_key = ip-rec_key.
  FOR EACH t-mfvalues WHERE t-mfvalues.rec_key = ip-rec_key NO-LOCK:
    CREATE mfvalues.
    BUFFER-COPY t-mfvalues TO mfvalues.
  END.
  {methods/nowait.i}
  Btn_Close:LABEL IN FRAME {&FRAME-NAME} = "&Close".
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Create_Label_Widget C-Win 
PROCEDURE Create_Label_Widget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-type AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ip-label AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ip-x AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ip-y AS INTEGER NO-UNDO.

  IF NOT CAN-DO("Text,Toggle-Box",ip-type) THEN /* text and toggle-box have no labels */
  DO:
    CREATE TEXT label-widget IN WIDGET-POOL "attr-widget"
      ASSIGN
        FRAME = FRAME {&FRAME-NAME}:HANDLE
        AUTO-RESIZE = YES
        Y = ip-y
        FORMAT = "X(" +
          (IF LENGTH(ip-label) NE 0 THEN STRING(LENGTH(ip-label) + 1)
          ELSE "1") + ")"
        SENSITIVE = YES
        SCREEN-VALUE = IF ip-label = "" THEN ""
                       ELSE ip-label + ":".
    ASSIGN
      label-widget:FONT = ? /* 8 */
      label-widget:X = IF ip-x - label-widget:WIDTH-PIXELS - 1 LT 0 THEN 0
                       ELSE ip-x - label-widget:WIDTH-PIXELS - 1
      label-widget:HEIGHT-CHARS = 1
      ldummy = label-widget:MOVE-TO-TOP()
      label-widget:HIDDEN = NO.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Create_Tabs C-Win 
PROCEDURE Create_Tabs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DELETE WIDGET-POOL "widget-pool-tabs" NO-ERROR.
  CREATE WIDGET-POOL "widget-pool-tabs" PERSISTENT.
  FIND mfgroup
      WHERE mfgroup.mfgroup = mfgroup_list:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
  IF NOT AVAILABLE mfgroup THEN
  RETURN.
  DO i = 1 TO NUM-ENTRIES(mfgroup.tab-labels):
    CREATE IMAGE tab-image[i] IN WIDGET-POOL "widget-pool-tabs"
      ASSIGN
        FRAME = FRAME {&FRAME-NAME}:HANDLE
        X = 3 + (i - 1) * 72
        Y = 1
        WIDTH-PIXEL = 72
        HEIGHT-PIXEL = 24
        SENSITIVE = YES
      TRIGGERS:      
        ON MOUSE-SELECT-CLICK 
           PERSISTENT RUN Label_Trigger IN THIS-PROCEDURE (i).
      END TRIGGERS.         
      CREATE TEXT tab-label[i] IN WIDGET-POOL "widget-pool-tabs"
        ASSIGN 
          FRAME = FRAME {&FRAME-NAME}:HANDLE
          X = tab-image[i]:X + 9
          Y = tab-image[i]:Y + 5
          WIDTH-PIXEL = tab-image[i]:WIDTH-PIXEL - 16
          HEIGHT-PIXEL = tab-image[i]:HEIGHT-PIXEL - 12
          FORMAT = "X(13)":U
          SENSITIVE = YES 
          FONT = ?
          BGCOLOR = 8
          SCREEN-VALUE = ENTRY(i,mfgroup.tab-label)
        TRIGGERS:      
          ON MOUSE-SELECT-CLICK 
             PERSISTENT RUN Label_Trigger IN THIS-PROCEDURE (i).
        END TRIGGERS.
      /* show tabs as down tabs initially */
      ASSIGN      
        ldummy = tab-image[i]:LOAD-IMAGE("adeicon/ts-dn72")
        ldummy = tab-image[i]:MOVE-TO-TOP()
        ldummy = tab-label[i]:MOVE-TO-TOP()
        tab-image[i]:HIDDEN = NO
        tab-label[i]:HIDDEN = NO.
  END.
  /* use up tab image for currently selected tab */
  ASSIGN
    tab-image[current-tab]:HEIGHT-PIXEL = 27
    ldummy = tab-image[current-tab]:LOAD-IMAGE("adeicon/ts-up72")
    ldummy = tab-image[current-tab]:MOVE-TO-TOP()
    ldummy = tab-label[current-tab]:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Create_Widgets C-Win 
PROCEDURE Create_Widgets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/wait.i}
  DELETE WIDGET-POOL "attr-widget" NO-ERROR.
  CREATE WIDGET-POOL "attr-widget" PERSISTENT.
  FOR EACH attrb
      WHERE attrb.attr_mfgroup = mfgroup_list:SCREEN-VALUE IN FRAME {&FRAME-NAME} AND
            attrb.attr_tab = current-tab NO-LOCK:
    RUN Dynamic_Widget.
  END.
  {methods/nowait.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Dynamic_Widget C-Win 
PROCEDURE Dynamic_Widget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE horz-bar AS LOGICAL NO-UNDO.
  DEFINE VARIABLE vert-bar AS LOGICAL NO-UNDO.

&scoped-define defaults-code ~
          ASSIGN ~
            FRAME = FRAME {&FRAME-NAME}:HANDLE ~
            FONT = ? ~
            X = attrb.attr_x ~
            Y = attrb.attr_y ~
            SENSITIVE = YES ~
            PRIVATE-DATA = STRING(attrb.attr_id) ~
            WIDTH-PIXELS = attrb.attr_width
&scoped-define widget-code ~
            {&defaults-code} ~
            HEIGHT-PIXELS = attrb.attr_height ~
            SIDE-LABEL-HANDLE = label-widget
&scoped-define combo-box-code ~
            {&defaults-code} ~
            HIDDEN = yes ~
            FORMAT = attrb.attr_settings ~
            SIDE-LABEL-HANDLE = label-widget ~
            INNER-LINES = NUM-ENTRIES(attrb.attr_values) ~
            LIST-ITEMS = attrb.attr_values ~
            HELP = 'Select ''' + attrb.attr_label + ''' from COMBO-BOX List' ~
            SCREEN-VALUE = t-mfvalues.mf_value ~
            {&trigger-code}
&scoped-define editor-code ~
            {&widget-code} ~
            SCROLLBAR-VERTICAL = vert-bar ~
            SCROLLBAR-HORIZONTAL = horz-bar ~
            HELP = 'Enter ''' + attrb.attr_label + ''' in EDITOR Field' ~
            SCREEN-VALUE = t-mfvalues.mf_value ~
            {&trigger-code}
&scoped-define fill-in-code ~
            {&widget-code} ~
            DATA-TYPE = attrb.attr_datatype ~
            FORMAT = attrb.attr_settings ~
            HELP = 'Enter ''' + attrb.attr_label + ''' in FILL-IN Field' ~
            SCREEN-VALUE = t-mfvalues.mf_value ~
            {&trigger-code}
&scoped-define radio-set-code ~
            {&widget-code} ~
            HORIZONTAL = horz-bar ~
            RADIO-BUTTONS = attrb.attr_values ~
            HELP = 'Select ''' + attrb.attr_label + ''' from RADIO-SET Choices' ~
            SCREEN-VALUE = t-mfvalues.mf_value ~
            {&trigger-code}
&scoped-define selection-list-code ~
            {&widget-code} ~
            LIST-ITEMS = attrb.attr_values ~
            HELP = 'Select ''' + attrb.attr_label + ''' from SELECTION-LIST' ~
            SCREEN-VALUE = t-mfvalues.mf_value ~
            {&trigger-code}
&scoped-define slider-code ~
            {&widget-code} ~
            HORIZONTAL = horz-bar ~
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
            LABEL = attrb.attr_label ~
            HEIGHT-PIXELS = attrb.attr_height ~
            HELP = 'Set ''' + attrb.attr_label + ''' TOGGLE-BOX On/Off' ~
            SCREEN-VALUE = t-mfvalues.mf_value ~
            {&trigger-code}

  ASSIGN
    horz-bar = IF NUM-ENTRIES(attrb.attr_settings) NE 0 AND
                  ENTRY(1,attrb.attr_settings) = "yes" THEN yes
               ELSE no
    vert-bar = IF NUM-ENTRIES(attrb.attr_settings) GT 1 AND
                  ENTRY(2,attrb.attr_settings) = "yes" THEN yes
               ELSE no.
  RUN Create_Label_Widget (attrb.attr_type,attrb.attr_label,attrb.attr_x,attrb.attr_y).
  FIND t-mfvalues
      WHERE t-mfvalues.rec_key = ip-rec_key AND
            t-mfvalues.mf_id = attrb.attr_id
      NO-LOCK NO-ERROR.
  DO ON ERROR UNDO, RETURN "CREATE-ERROR":
    CASE attrb.attr_type:
&scoped-define widget-type COMBO-BOX
      WHEN "{&widget-type}" THEN
      DO:
        CREATE {&widget-type} dyn-widget IN WIDGET-POOL "attr-widget"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type EDITOR
      WHEN "{&widget-type}" THEN
      DO:
        CREATE {&widget-type} dyn-widget IN WIDGET-POOL "attr-widget"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type FILL-IN
      WHEN "{&widget-type}" THEN
      DO:
        CREATE {&widget-type} dyn-widget IN WIDGET-POOL "attr-widget"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type RADIO-SET
      WHEN "{&widget-type}" THEN
      DO:
        CREATE {&widget-type} dyn-widget IN WIDGET-POOL "attr-widget"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type SELECTION-LIST
      WHEN "{&widget-type}" THEN
      DO:
        CREATE {&widget-type} dyn-widget IN WIDGET-POOL "attr-widget"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type SLIDER
      WHEN "{&widget-type}" THEN
      DO:
        CREATE {&widget-type} dyn-widget IN WIDGET-POOL "attr-widget"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type TEXT
      WHEN "{&widget-type}" THEN
      DO:
        CREATE {&widget-type} dyn-widget IN WIDGET-POOL "attr-widget"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type TOGGLE-BOX
      WHEN "{&widget-type}" THEN
      DO:
        CREATE {&widget-type} dyn-widget IN WIDGET-POOL "attr-widget"
        {&{&widget-type}-code}
      END.
    END CASE.
    IF VALID-HANDLE(dyn-widget) THEN
    ASSIGN
      ldummy = dyn-widget:MOVE-TO-TOP()
      dyn-widget:HIDDEN = NO.
  END.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win _DEFAULT-ENABLE
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
  DISPLAY mfgroup_list header_value 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE Rect-Top Rect-Left Rect-Right Rect-Bottom Btn_OK Btn_Apply Btn_Close 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Label_Trigger C-Win 
PROCEDURE Label_Trigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER tabno AS INTEGER NO-UNDO.

  ASSIGN      
    tab-image[current-tab]:HEIGHT-PIXEL = 24
    ldummy = tab-image[current-tab]:LOAD-IMAGE("adeicon/ts-dn72")
    ldummy = tab-image[current-tab]:MOVE-TO-TOP()
    ldummy = tab-label[current-tab]:MOVE-TO-TOP()
    current-tab = tabno.
  RUN Create_Widgets.
  ASSIGN
    tab-image[current-tab]:HEIGHT-PIXEL = 27
    ldummy = tab-image[current-tab]:LOAD-IMAGE("adeicon/ts-up72")
    ldummy = tab-image[current-tab]:MOVE-TO-TOP()
    ldummy = tab-label[current-tab]:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Load_Widget_Data C-Win 
PROCEDURE Load_Widget_Data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND FIRST mfdata NO-LOCK.
  DO i = 1 TO NUM-ENTRIES(REPLACE(ENTRY(1,mfdata.mfgroup_data,";"),'"','')):
    CREATE mfgroup.
    ASSIGN
      mfgroup.mfgroup = ENTRY(i,REPLACE(ENTRY(1,mfdata.mfgroup_data,";"),'"',''))
      mfgroup.tab-labels = REPLACE(ENTRY(i + 1,mfdata.mfgroup_data,";"),'"','')
      mfgroup-list = IF mfgroup-list = "" THEN mfgroup.mfgroup
                     ELSE mfgroup-list + "," + mfgroup.mfgroup.
  END.
  IF NOT CAN-DO(mfgroup-list,ip-group) THEN
  DO:
    MESSAGE "No '" + ip-group + "' Group Exists!!!" VIEW-AS ALERT-BOX INFORMATION.
    RETURN "EMPTY".
  END.
  OUTPUT TO VALUE("users/" + USERID("NOSWEAT") + "/miscflds.dat").
  DO i = 1 TO NUM-ENTRIES(mfdata.miscflds_data,";") - 1:
    PUT UNFORMATTED ENTRY(i,mfdata.miscflds_data,";") AT 1.
  END.
  OUTPUT CLOSE.
  INPUT FROM VALUE("users/" + USERID("NOSWEAT") + "/miscflds.dat") NO-ECHO.
  REPEAT:
    CREATE attrb.
    IMPORT attrb.
  END.
  INPUT CLOSE.
  IF attrb.attr_type = "" THEN
  DELETE attrb.
  ASSIGN
    mfgroup_list:INNER-LINES IN FRAME {&FRAME-NAME} = NUM-ENTRIES(mfgroup-list)
    mfgroup_list:LIST-ITEMS IN FRAME {&FRAME-NAME} = mfgroup-list
    mfgroup_list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ip-group
    header_value:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ip-header.
  APPLY "VALUE-CHANGED" TO mfgroup_list IN FRAME {&FRAME-NAME}.

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
  {methods/setfocus.i Btn_Close}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Value_Change C-Win 
PROCEDURE Value_Change :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER attribute AS WIDGET-HANDLE NO-UNDO.

  FIND t-mfvalues
      WHERE t-mfvalues.rec_key = ip-rec_key AND
            t-mfvalues.mf_id = attribute:PRIVATE-DATA
      EXCLUSIVE-LOCK.
  ASSIGN
    t-mfvalues.mf_value = attribute:SCREEN-VALUE
    editor-widget = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Widget_Entry C-Win 
PROCEDURE Widget_Entry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER attribute AS WIDGET-HANDLE NO-UNDO.

  IF attribute:TYPE = "EDITOR" THEN
  editor-widget = attribute:HANDLE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Widget_Leave C-Win 
PROCEDURE Widget_Leave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER attribute AS WIDGET-HANDLE NO-UNDO.

  IF VALID-HANDLE(attribute) THEN
  RUN Value_Change (attribute:HANDLE).

  IF VALID-HANDLE(editor-widget) THEN
  RUN Value_Change (editor-widget:HANDLE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


