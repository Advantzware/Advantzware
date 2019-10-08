&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File:              miscflds.w

  Description:       Misc. Fields Design Layout Window

  Input Parameters:  <none>

  Output Parameters: <none>

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

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

&SCOPED-DEFINE widgetPoolName dynamic-widget

{UDF/mfttdefs.i &NEW="NEW SHARED"}

DEFINE TEMP-TABLE ttDeleteMFValues NO-UNDO
  FIELD mfvalues_recid AS RECID.

DEFINE VARIABLE tabImage      AS WIDGET-HANDLE NO-UNDO EXTENT 18.
DEFINE VARIABLE tabLabel      AS WIDGET-HANDLE NO-UNDO EXTENT 18.
DEFINE VARIABLE labelWidget   AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE dynWidget     AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE saveWidget    AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE currentWidget AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE currentLabel  AS CHARACTER     NO-UNDO.
DEFINE VARIABLE cdummy        AS CHARACTER     NO-UNDO.
DEFINE VARIABLE tabOrder      AS CHARACTER     NO-UNDO.
DEFINE VARIABLE ldummy        AS LOGICAL       NO-UNDO.
DEFINE VARIABLE newMFGroup    AS LOGICAL       NO-UNDO.
DEFINE VARIABLE copyMFGroup   AS LOGICAL       NO-UNDO.
DEFINE VARIABLE newTab        AS LOGICAL       NO-UNDO.
DEFINE VARIABLE leftright     AS LOGICAL       NO-UNDO.
DEFINE VARIABLE continue      AS LOGICAL       NO-UNDO.
DEFINE VARIABLE currentTab    AS INTEGER       NO-UNDO.
DEFINE VARIABLE lastID        AS INTEGER       NO-UNDO.
DEFINE VARIABLE i             AS INTEGER       NO-UNDO.
DEFINE VARIABLE savePrompt    AS LOGICAL       NO-UNDO.
DEFINE VARIABLE lError        AS LOGICAL       NO-UNDO.

DEFINE BUFFER bAttrb FOR ttAttrb.

/* trigger code attached to each created dynamic widget */
&SCOPED-DEFINE trigger-code ~
TRIGGERS: ~
  ON SELECTION ~
    PERSISTENT RUN selectWidget IN THIS-PROCEDURE. ~
  ON DESELECTION ~
    PERSISTENT RUN deselectWidget IN THIS-PROCEDURE. ~
  ON END-MOVE ~
    PERSISTENT RUN moveWidget IN THIS-PROCEDURE. ~
  ON MOUSE-SELECT-DBLCLICK, RETURN ~
    PERSISTENT RUN propertySheet IN THIS-PROCEDURE. ~
  ON DELETE-CHARACTER  ~
    PERSISTENT RUN cutWidgets IN THIS-PROCEDURE. ~
  ON END-RESIZE ~
    PERSISTENT RUN resizeWidget IN THIS-PROCEDURE (dynWidget:HANDLE). ~
END TRIGGERS.

&IF DEFINED(FWD-VERSION) EQ 0 &THEN
   {methods/lockWindowUpdate.i}
&ENDIF

SESSION:SET-WAIT-STATE("").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMiscFlds

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnMFPrgrms tabLabelsRect RECT-5 RECT-4 ~
widgetRect tabNavRect RECT-2 RECT-3 RECT-8 portRect prgrmsRect mfgroupList ~
mfgroupTab btnCombo-Box btnAddTab tabLabels btnEditor btnFill-In ~
btnRadio-Set btnRectangle btnSelection-List btnSlider btnText btnToggle-Box ~
btnCopy btnCut btnDeleteTab btnDownTab btnExit btnExport btnImport ~
btnNextTab btnPaste btnPrevTab btnProperty btnRenameTab btnUpTab ~
btnAddGroup btnCopyGroup btnDeleteGroup btnRenameGroup btnRestore btnSave ~
btnTabOrder btnTest 
&Scoped-Define DISPLAYED-OBJECTS mfgroupList mfgroupTab tabLabels ~
widgetLabel tabCBLabel pointerLabel tabNavLabel combo-BoxLabel editorLabel ~
fill-InLabel radio-SetLabel rectangleLabel selection-ListLabel sliderLabel ~
textLabel toggle-BoxLabel gapFieldLabel prgrmsLabel 

/* Custom List Definitions                                              */
/* notFoundIn234,widgetLabels,widgetButtons,propertyCutCopy,List-5,List-6 */
&Scoped-define notFoundIn234 widgetRect mfgroupTab btnPointer btnCombo-Box ~
btnAddTab tabLabels btnEditor btnFill-In btnRadio-Set btnRectangle ~
btnSelection-List btnSlider btnText btnToggle-Box btnDeleteTab btnDownTab ~
btnNextTab btnPrevTab btnRenameTab btnUpTab btnDeleteGroup btnRenameGroup ~
btnTest widgetLabel pointerLabel tabNavLabel combo-BoxLabel editorLabel ~
fill-InLabel radio-SetLabel rectangleLabel selection-ListLabel sliderLabel ~
textLabel toggle-BoxLabel prgrmsLabel 
&Scoped-define widgetLabels pointerLabel combo-BoxLabel editorLabel ~
fill-InLabel radio-SetLabel rectangleLabel selection-ListLabel sliderLabel ~
textLabel toggle-BoxLabel 
&Scoped-define widgetButtons btnPointer btnCombo-Box btnEditor btnFill-In ~
btnRadio-Set btnRectangle btnSelection-List btnSlider btnText btnToggle-Box 
&Scoped-define propertyCutCopy btnCopy btnCut btnProperty 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_File 
       MENU-ITEM m_Reset        LABEL "&Reset"        
       MENU-ITEM m_Save         LABEL "&Save"         
       RULE
       MENU-ITEM m_Exit         LABEL "E&xit"         .

DEFINE SUB-MENU m_Edit 
       MENU-ITEM m_Cut          LABEL "Cu&t"          
       MENU-ITEM m_Copy         LABEL "&Copy"         
       MENU-ITEM m_Paste        LABEL "&Paste"        .

DEFINE SUB-MENU m_Group 
       MENU-ITEM m_addGroup     LABEL "&Add Group"    
       MENU-ITEM m_copyGroup    LABEL "&Copy Group"   
       MENU-ITEM m_Change_Group LABEL "&Rename Group" 
       MENU-ITEM m_deleteGroup  LABEL "&Delete Group" .

DEFINE SUB-MENU m_Tabs 
       MENU-ITEM m_Add_Tab      LABEL "&Add Tab"      
       MENU-ITEM m_Rename_Tab   LABEL "&Rename Tab"   
       MENU-ITEM m_Delete_Tab   LABEL "&Delete Tab"   
       RULE
       MENU-ITEM m_Move_Tab_Up  LABEL "Move Tab &Up"  
       MENU-ITEM m_Move_Tab_Down LABEL "Move Tab &Down"
       RULE
       MENU-ITEM m_Next_Tab     LABEL "&Next Tab"     
       MENU-ITEM m_Previous_Tab LABEL "&Previous Tab" .

DEFINE SUB-MENU m_Widget 
       MENU-ITEM m_Tab_Order    LABEL "Tab &Order"    
       RULE
       MENU-ITEM m_Attributes   LABEL "Attributes"    
       RULE
       MENU-ITEM m_Pointer      LABEL "&Pointer"      
       MENU-ITEM m_Combo_Box    LABEL "&Combo Box"    
       MENU-ITEM m_Editor       LABEL "&Editor"       
       MENU-ITEM m_Fill_In      LABEL "&Fill In"      
       MENU-ITEM m_Radio_Set    LABEL "&Radio Set"    
       MENU-ITEM m_Selection_List LABEL "Selection &List"
       MENU-ITEM m_Slider       LABEL "&Slider"       
       MENU-ITEM m_Text         LABEL "&Text"         
       MENU-ITEM m_Toggle_Box   LABEL "Toggle &Box"   .

DEFINE SUB-MENU m_Gap_Spacing 
       MENU-ITEM m_1_LeftRight_Sides LABEL "&1 Left/Right Sides"
       MENU-ITEM m_2_TopBottom_Sides LABEL "&2 Top/Bottom Sides".

DEFINE SUB-MENU m_Align 
       MENU-ITEM m_Left_Sides   LABEL "&Left Sides"   
       MENU-ITEM m_Right_Sides  LABEL "&Right Sides"  
       RULE
       MENU-ITEM m_Top_Sides    LABEL "&Top Sides"    
       MENU-ITEM m_Bottom_Sides LABEL "&Bottom Sides" 
       RULE
       SUB-MENU  m_Gap_Spacing  LABEL "&Gap Spacing"  .

DEFINE MENU MENU-BAR-C-Win MENUBAR
       SUB-MENU  m_File         LABEL "&File"         
       SUB-MENU  m_Edit         LABEL "&Edit"         
       SUB-MENU  m_Group        LABEL "&Group"        
       SUB-MENU  m_Tabs         LABEL "&Tabs"         
       SUB-MENU  m_Widget       LABEL "&Widgets"      
       SUB-MENU  m_Align        LABEL "&Align"        .


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAddGroup 
     IMAGE-UP FILE "Graphics/16x16/plus.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Add Group" 
     SIZE 4.2 BY 1 TOOLTIP "Add New Group".

DEFINE BUTTON btnAddTab 
     IMAGE-UP FILE "Graphics/16x16/plus.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Add" 
     SIZE 4.2 BY 1 TOOLTIP "Add New Tab (Page)"
     FONT 4.

DEFINE BUTTON btnCombo-Box 
     LABEL "Combo Box" 
     SIZE 6.4 BY 1.52 TOOLTIP "Combo Box"
     FONT 4.

DEFINE BUTTON btnCopy 
     IMAGE-UP FILE "Graphics/32x32/copy.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/sign_forbidden.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Copy" 
     SIZE 8 BY 1.91 TOOLTIP "Copy Selections to Clipboard".

DEFINE BUTTON btnCopyGroup 
     IMAGE-UP FILE "Graphics/16x16/copy.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Add Group" 
     SIZE 4.2 BY 1 TOOLTIP "Add New Group".

DEFINE BUTTON btnCut 
     IMAGE-UP FILE "Graphics/32x32/cut.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/sign_forbidden.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Cut" 
     SIZE 8 BY 1.91 TOOLTIP "Cut Selections to Clipboard"
     FONT 4.

DEFINE BUTTON btnDeleteGroup 
     IMAGE-UP FILE "Graphics/16x16/delete.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Delete Group" 
     SIZE 4.2 BY 1 TOOLTIP "Delete Current Group".

DEFINE BUTTON btnDeleteTab 
     IMAGE-UP FILE "Graphics/16x16/delete.jpg":U
     IMAGE-INSENSITIVE FILE "Graphics/16x16/sign_forbidden.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Delete" 
     SIZE 4.2 BY 1 TOOLTIP "Delete Current Tab (Page)"
     FONT 4.

DEFINE BUTTON btnDownTab 
     IMAGE-UP FILE "Graphics/16x16/down.jpg":U
     IMAGE-INSENSITIVE FILE "Graphics/16x16/sign_forbidden.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Move Dn" 
     SIZE 4.2 BY 1 TOOLTIP "Move Current Tab (Page) Down (Right)"
     FONT 4.

DEFINE BUTTON btnEditor 
     LABEL "Editor" 
     SIZE 6.4 BY 1.52 TOOLTIP "Editor"
     FONT 4.

DEFINE BUTTON btnExit 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "E&xit" 
     SIZE 8 BY 1.91 TOOLTIP "Exit"
     FONT 4.

DEFINE BUTTON btnExport 
     IMAGE-UP FILE "Graphics/32x32/export.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Export" 
     SIZE 8 BY 1.91 TOOLTIP "Export"
     FONT 4.

DEFINE BUTTON btnFill-In 
     LABEL "Fill In" 
     SIZE 6.4 BY 1.52 TOOLTIP "Fill In"
     FONT 4.

DEFINE BUTTON btnImport 
     IMAGE-UP FILE "Graphics/32x32/import.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Import" 
     SIZE 8 BY 1.91 TOOLTIP "Import"
     FONT 4.

DEFINE BUTTON btnMFPrgrms 
     IMAGE-UP FILE "Graphics/32x32/windows.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Programs" 
     SIZE 8 BY 1.91 TOOLTIP "Programs"
     FONT 4.

DEFINE BUTTON btnNextTab 
     IMAGE-UP FILE "Graphics/16x16/next.jpg":U
     IMAGE-INSENSITIVE FILE "Graphics/16x16/sign_forbidden.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Next Tab" 
     SIZE 4.2 BY 1 TOOLTIP "Change to Next Tab (Page)"
     FONT 4.

DEFINE BUTTON btnPaste 
     IMAGE-UP FILE "Graphics/32x32/clipboard_paste.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/sign_forbidden.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Paste" 
     SIZE 8 BY 1.91 TOOLTIP "Paste from Clipboard".

DEFINE BUTTON btnPointer 
     LABEL "Pointer" 
     SIZE 6.4 BY 1.52 TOOLTIP "Pointer".

DEFINE BUTTON btnPrevTab 
     IMAGE-UP FILE "Graphics/16x16/previous.jpg":U
     IMAGE-INSENSITIVE FILE "Graphics/16x16/sign_forbidden.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Previous Tab" 
     SIZE 4.2 BY 1 TOOLTIP "Change to Previous Tab (Page)"
     FONT 4.

DEFINE BUTTON btnProperty 
     IMAGE-UP FILE "Graphics/32x32/form.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/sign_forbidden.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Property" 
     SIZE 8 BY 1.91 TOOLTIP "Attributes"
     FONT 4.

DEFINE BUTTON btnRadio-Set 
     LABEL "Radio Set" 
     SIZE 6.4 BY 1.52 TOOLTIP "Radio Set"
     FONT 4.

DEFINE BUTTON btnRectangle 
     LABEL "Rectangle" 
     SIZE 6.4 BY 1.52 TOOLTIP "Toggle Box"
     FONT 4.

DEFINE BUTTON btnRenameGroup 
     IMAGE-UP FILE "Graphics/16x16/rename.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Rename Group" 
     SIZE 4.2 BY 1 TOOLTIP "Rename Current Group".

DEFINE BUTTON btnRenameTab 
     IMAGE-UP FILE "Graphics/16x16/rename.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Rename" 
     SIZE 4.2 BY 1 TOOLTIP "Rename Current Tab (Page)"
     FONT 4.

DEFINE BUTTON btnRestore 
     IMAGE-UP FILE "Graphics/32x32/refresh.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset" 
     SIZE 8 BY 1.91 TOOLTIP "Reset"
     FONT 4.

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Save" 
     SIZE 8 BY 1.91 TOOLTIP "Save"
     FONT 4.

DEFINE BUTTON btnSelection-List 
     LABEL "Selection List" 
     SIZE 6.4 BY 1.52 TOOLTIP "Selection List"
     FONT 4.

DEFINE BUTTON btnSlider 
     LABEL "Slider" 
     SIZE 6.4 BY 1.52 TOOLTIP "Slider"
     FONT 4.

DEFINE BUTTON btnTabOrder 
     IMAGE-UP FILE "Graphics/32x32/elements_cascade.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Tab Order" 
     SIZE 8 BY 1.91 TOOLTIP "Set Tab Order"
     FONT 4.

DEFINE BUTTON btnTest 
     IMAGE-UP FILE "Graphics/32x32/window_dialog.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Test" 
     SIZE 8 BY 1.91.

DEFINE BUTTON btnText 
     LABEL "Text" 
     SIZE 6.4 BY 1.52 TOOLTIP "Text"
     FONT 4.

DEFINE BUTTON btnToggle-Box 
     LABEL "Toggle Box" 
     SIZE 6.4 BY 1.52 TOOLTIP "Toggle Box"
     FONT 4.

DEFINE BUTTON btnUpTab 
     IMAGE-UP FILE "Graphics/16x16/up.jpg":U
     IMAGE-INSENSITIVE FILE "Graphics/16x16/sign_forbidden.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Move Up" 
     SIZE 4.2 BY 1 TOOLTIP "Move Current Tab (Page) Up (Left)"
     FONT 4.

DEFINE VARIABLE mfgroupList AS CHARACTER FORMAT "X(256)":U 
     LABEL "Group" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 20
     DROP-DOWN-LIST
     SIZE 48 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE mfgroupTab AS INTEGER FORMAT ">9":U INITIAL 1 
     VIEW-AS COMBO-BOX INNER-LINES 1
     LIST-ITEMS "1" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 TOOLTIP "Change Current Tab (Page)" NO-UNDO.

DEFINE VARIABLE combo-BoxLabel AS CHARACTER FORMAT "X(256)":U INITIAL " Combo Box" 
      VIEW-AS TEXT 
     SIZE 14 BY 1.52
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE editorLabel AS CHARACTER FORMAT "X(256)":U INITIAL " Editor" 
      VIEW-AS TEXT 
     SIZE 14 BY 1.52
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE fill-InLabel AS CHARACTER FORMAT "X(256)":U INITIAL " Fill In" 
      VIEW-AS TEXT 
     SIZE 14 BY 1.52
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE gapField AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6.6 BY .91
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE gapFieldLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Align Gap" 
      VIEW-AS TEXT 
     SIZE 13 BY .91 NO-UNDO.

DEFINE VARIABLE mfgroupField AS CHARACTER FORMAT "X(256)":U 
     LABEL "Group" 
     VIEW-AS FILL-IN 
     SIZE 48 BY 1 NO-UNDO.

DEFINE VARIABLE pointerLabel AS CHARACTER FORMAT "X(256)":U INITIAL " Pointer" 
      VIEW-AS TEXT 
     SIZE 14 BY 1.52
     BGCOLOR 8 FONT 4 NO-UNDO.

DEFINE VARIABLE prgrmsLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Programs" 
      VIEW-AS TEXT 
     SIZE 9 BY .62 NO-UNDO.

DEFINE VARIABLE radio-SetLabel AS CHARACTER FORMAT "X(256)":U INITIAL " Radio Set" 
      VIEW-AS TEXT 
     SIZE 14 BY 1.52
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE rectangleLabel AS CHARACTER FORMAT "X(256)":U INITIAL " Rectangle" 
      VIEW-AS TEXT 
     SIZE 14 BY 1.52
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE selection-ListLabel AS CHARACTER FORMAT "X(256)":U INITIAL " Selection List" 
      VIEW-AS TEXT 
     SIZE 14 BY 1.52
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE sliderLabel AS CHARACTER FORMAT "X(256)":U INITIAL " Slider" 
      VIEW-AS TEXT 
     SIZE 14 BY 1.52
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE tabCBLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Tab:" 
      VIEW-AS TEXT 
     SIZE 4.6 BY .91 NO-UNDO.

DEFINE VARIABLE tabField AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1
     FONT 4 NO-UNDO.

DEFINE VARIABLE tabNavLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Tab Labels | Tab Order" 
      VIEW-AS TEXT 
     SIZE 22 BY .62 NO-UNDO.

DEFINE VARIABLE textLabel AS CHARACTER FORMAT "X(256)":U INITIAL " Text" 
      VIEW-AS TEXT 
     SIZE 14 BY 1.52
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE toggle-BoxLabel AS CHARACTER FORMAT "X(256)":U INITIAL " Toggle Box" 
      VIEW-AS TEXT 
     SIZE 14 BY 1.52
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE widgetLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Widgets" 
      VIEW-AS TEXT 
     SIZE 8 BY .62 NO-UNDO.

DEFINE RECTANGLE portRect
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 23 BY 3.33
     BGCOLOR 8 .

DEFINE RECTANGLE prgrmsRect
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 15 BY 3.33
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 141 BY 3.19.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE .2 BY 3.14.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE .2 BY 3.14.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE .2 BY 3.14.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE .2 BY 3.14.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE .2 BY 3.14.

DEFINE RECTANGLE tabLabelsRect
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 27.6 BY 14.52.

DEFINE RECTANGLE tabNavRect
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 27.6 BY 1.43
     BGCOLOR 8 .

DEFINE RECTANGLE widgetRect
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 23.4 BY 18.81.

DEFINE VARIABLE tabLabels AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SCROLLBAR-VERTICAL 
     SIZE 26 BY 12.14 TOOLTIP "Change Current Tab (Page)" NO-UNDO.

DEFINE VARIABLE udfHeightSize AS INTEGER INITIAL 0 
     VIEW-AS SLIDER MIN-VALUE 460 MAX-VALUE 9999 VERTICAL 
     TIC-MARKS LEFT FREQUENCY 100
     SIZE 12 BY 3.81 NO-UNDO.

DEFINE VARIABLE udfWidthSize AS INTEGER INITIAL 0 
     VIEW-AS SLIDER MIN-VALUE 460 MAX-VALUE 9999 HORIZONTAL 
     TIC-MARKS TOP FREQUENCY 100
     SIZE 27 BY 2.38 NO-UNDO.

DEFINE RECTANGLE Rect-Bottom
     EDGE-PIXELS 0    
     SIZE 95 BY .19
     BGCOLOR 7 .

DEFINE RECTANGLE Rect-Left
     EDGE-PIXELS 0    
     SIZE .6 BY 21.33
     BGCOLOR 15 .

DEFINE RECTANGLE Rect-Main
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 95.4 BY 21.57
     BGCOLOR 8 FGCOLOR 0 .

DEFINE RECTANGLE Rect-Right
     EDGE-PIXELS 0    
     SIZE .6 BY 21.19
     BGCOLOR 7 .

DEFINE RECTANGLE Rect-Top
     EDGE-PIXELS 0    
     SIZE 95 BY .19
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMiscFlds
     btnMFPrgrms AT ROW 23.86 COL 138 HELP
          "Programs" WIDGET-ID 34
     mfgroupField AT ROW 1.71 COL 8 COLON-ALIGNED HELP
          "Enter Group Name"
     mfgroupList AT ROW 1.71 COL 8 COLON-ALIGNED HELP
          "Select Group Name"
     mfgroupTab AT ROW 4.81 COL 133.6 COLON-ALIGNED HELP
          "Select Tab Number" NO-LABEL
     btnPointer AT ROW 5.57 COL 3.2 HELP
          "Restore Mouse Cursor to ARROW POINTER"
     btnCombo-Box AT ROW 7.19 COL 3.2 HELP
          "Create New COMBO-BOX"
     btnAddTab AT ROW 7.19 COL 125 HELP
          "ADD New Tab"
     tabField AT ROW 7.19 COL 124 HELP
          "Enter Tab Label" NO-LABEL
     tabLabels AT ROW 8.38 COL 124 HELP
          "Select tab Label" NO-LABEL
     btnEditor AT ROW 8.86 COL 3.2 HELP
          "Create New EDITOR"
     btnFill-In AT ROW 10.52 COL 3.2 HELP
          "Create New FILL-IN"
     btnRadio-Set AT ROW 12.19 COL 3.2 HELP
          "Create New RADIO-SET"
     btnRectangle AT ROW 13.86 COL 3 HELP
          "Create New Rectangle"
     btnSelection-List AT ROW 15.52 COL 3 HELP
          "Create New SELECTION-LIST"
     btnSlider AT ROW 17.19 COL 3 HELP
          "Create New SLIDER"
     btnText AT ROW 18.86 COL 3 HELP
          "Create New TEXT"
     btnToggle-Box AT ROW 20.52 COL 3 HELP
          "Create New TOGGLE-BOX"
     udfWidthSize AT ROW 21 COL 123 NO-LABEL WIDGET-ID 44
     gapField AT ROW 22.19 COL 1 COLON-ALIGNED HELP
          "Enter Gap Amount in Pixels" NO-LABEL
     udfHeightSize AT ROW 23.38 COL 122 NO-LABEL WIDGET-ID 42
     btnCopy AT ROW 1.48 COL 88 HELP
          "Copy Selections"
     btnCut AT ROW 1.48 COL 80 HELP
          "Cut Selections"
     btnDeleteTab AT ROW 7.19 COL 135 HELP
          "DELETE Tab"
     btnDownTab AT ROW 7.19 COL 140 HELP
          "Move Tab DOWN"
     btnExit AT ROW 1.48 COL 134 HELP
          "Exit Design Layout Window"
     btnExport AT ROW 23.86 COL 15 HELP
          "Export" WIDGET-ID 26
     btnImport AT ROW 23.86 COL 4 HELP
          "Import" WIDGET-ID 24
     btnNextTab AT ROW 4.81 COL 144.6 HELP
          "Next Tab"
     btnPaste AT ROW 1.48 COL 96 HELP
          "Paste Selections"
     btnPrevTab AT ROW 4.81 COL 125.6 HELP
          "Previous tab"
     btnProperty AT ROW 1.48 COL 60 HELP
          "Attributes"
     btnRenameTab AT ROW 7.19 COL 130 HELP
          "RENAME Tab"
     btnUpTab AT ROW 7.19 COL 145 HELP
          "Move Tab UP"
     btnAddGroup AT ROW 3.14 COL 10 HELP
          "ADD GROU[ Name"
     btnCopyGroup AT ROW 3.14 COL 20 HELP
          "ADD GROU[ Name"
     btnDeleteGroup AT ROW 3.19 COL 45 HELP
          "DELETE GROU[ Name"
     btnRenameGroup AT ROW 3.14 COL 31.2 HELP
          "CHANGE GROU[ Name"
     btnRestore AT ROW 1.48 COL 116 HELP
          "Reset"
     btnSave AT ROW 1.48 COL 124 HELP
          "Save"
     btnTabOrder AT ROW 1.48 COL 70 HELP
          "Set Tab Order"
     btnTest AT ROW 1.48 COL 106
     widgetLabel AT ROW 4.81 COL 8 COLON-ALIGNED NO-LABEL
     tabCBLabel AT ROW 4.81 COL 129 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     pointerLabel AT ROW 5.57 COL 8.2 COLON-ALIGNED NO-LABEL
     tabNavLabel AT ROW 6.48 COL 126 NO-LABEL WIDGET-ID 14
     combo-BoxLabel AT ROW 7.19 COL 8.2 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 149.6 BY 26.24.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMiscFlds
     editorLabel AT ROW 8.86 COL 8.2 COLON-ALIGNED NO-LABEL
     fill-InLabel AT ROW 10.52 COL 8.2 COLON-ALIGNED NO-LABEL
     radio-SetLabel AT ROW 12.19 COL 8.2 COLON-ALIGNED NO-LABEL
     rectangleLabel AT ROW 13.86 COL 8 COLON-ALIGNED NO-LABEL
     selection-ListLabel AT ROW 15.52 COL 8 COLON-ALIGNED NO-LABEL
     sliderLabel AT ROW 17.19 COL 8 COLON-ALIGNED NO-LABEL
     textLabel AT ROW 18.86 COL 8 COLON-ALIGNED NO-LABEL
     toggle-BoxLabel AT ROW 20.52 COL 8 COLON-ALIGNED NO-LABEL
     gapFieldLabel AT ROW 22.19 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     prgrmsLabel AT ROW 26 COL 138 NO-LABEL WIDGET-ID 36
     "Tab Order" VIEW-AS TEXT
          SIZE 10 BY .76 AT ROW 3.62 COL 69
          FONT 4
     "Save" VIEW-AS TEXT
          SIZE 5 BY .76 AT ROW 3.62 COL 125 WIDGET-ID 12
          FONT 4
     "Paste" VIEW-AS TEXT
          SIZE 6 BY .76 AT ROW 3.62 COL 97 WIDGET-ID 10
          FONT 4
     "Reset" VIEW-AS TEXT
          SIZE 6 BY .76 AT ROW 3.62 COL 117
          FONT 4
     "Cut" VIEW-AS TEXT
          SIZE 4 BY .76 AT ROW 3.62 COL 82
          FONT 4
     "Add" VIEW-AS TEXT
          SIZE 4 BY .95 AT ROW 3.14 COL 15
          FONT 4
     "Rename" VIEW-AS TEXT
          SIZE 8.6 BY .95 AT ROW 3.19 COL 36
          FONT 4
     "Copy" VIEW-AS TEXT
          SIZE 5 BY .76 AT ROW 3.62 COL 89 WIDGET-ID 8
          FONT 4
     "Export" VIEW-AS TEXT
          SIZE 7 BY .76 AT ROW 26 COL 15 WIDGET-ID 30
          FONT 4
     "E&xit" VIEW-AS TEXT
          SIZE 4.2 BY .76 AT ROW 3.62 COL 136
          FONT 4
     "Delete" VIEW-AS TEXT
          SIZE 7 BY .95 AT ROW 3.19 COL 50
          FONT 4
     "Preview" VIEW-AS TEXT
          SIZE 8 BY .76 AT ROW 3.62 COL 106 WIDGET-ID 6
          FONT 4
     "Import" VIEW-AS TEXT
          SIZE 6 BY .76 AT ROW 26 COL 5 WIDGET-ID 28
          FONT 4
     "Copy" VIEW-AS TEXT
          SIZE 5 BY .95 AT ROW 3.19 COL 25
     "Atrributes" VIEW-AS TEXT
          SIZE 9 BY .76 AT ROW 3.62 COL 59
          FONT 4
     tabLabelsRect AT ROW 6.24 COL 123
     RECT-1 AT ROW 1.24 COL 2
     RECT-5 AT ROW 1.24 COL 133
     RECT-4 AT ROW 1.24 COL 105
     widgetRect AT ROW 4.57 COL 2
     tabNavRect AT ROW 4.57 COL 123
     RECT-2 AT ROW 1.24 COL 58
     RECT-3 AT ROW 1.24 COL 79
     RECT-8 AT ROW 1.24 COL 115 WIDGET-ID 4
     portRect AT ROW 23.62 COL 2 WIDGET-ID 20
     prgrmsRect AT ROW 23.62 COL 135 WIDGET-ID 32
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 149.6 BY 26.24.

DEFINE FRAME folderFrm
     Rect-Main AT ROW 2.05 COL 1.6
     Rect-Top AT ROW 2.14 COL 1.8
     Rect-Left AT ROW 2.19 COL 1.8
     Rect-Right AT ROW 2.43 COL 96.2
     Rect-Bottom AT ROW 23.38 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 26 ROW 4.48
         SIZE 96.2 BY 22.67.


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
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "User Defined Fields Builder"
         HEIGHT             = 26.24
         WIDTH              = 149.6
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
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

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-C-Win:HANDLE.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT W-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMiscFlds
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON btnAddTab IN FRAME fMiscFlds
   1                                                                    */
/* SETTINGS FOR BUTTON btnCombo-Box IN FRAME fMiscFlds
   1 3                                                                  */
/* SETTINGS FOR BUTTON btnCopy IN FRAME fMiscFlds
   4                                                                    */
/* SETTINGS FOR BUTTON btnCut IN FRAME fMiscFlds
   4                                                                    */
/* SETTINGS FOR BUTTON btnDeleteGroup IN FRAME fMiscFlds
   1                                                                    */
/* SETTINGS FOR BUTTON btnDeleteTab IN FRAME fMiscFlds
   1                                                                    */
/* SETTINGS FOR BUTTON btnDownTab IN FRAME fMiscFlds
   1                                                                    */
/* SETTINGS FOR BUTTON btnEditor IN FRAME fMiscFlds
   1 3                                                                  */
/* SETTINGS FOR BUTTON btnFill-In IN FRAME fMiscFlds
   1 3                                                                  */
/* SETTINGS FOR BUTTON btnNextTab IN FRAME fMiscFlds
   1                                                                    */
/* SETTINGS FOR BUTTON btnPointer IN FRAME fMiscFlds
   NO-ENABLE 1 3                                                        */
/* SETTINGS FOR BUTTON btnPrevTab IN FRAME fMiscFlds
   1                                                                    */
/* SETTINGS FOR BUTTON btnProperty IN FRAME fMiscFlds
   4                                                                    */
/* SETTINGS FOR BUTTON btnRadio-Set IN FRAME fMiscFlds
   1 3                                                                  */
/* SETTINGS FOR BUTTON btnRectangle IN FRAME fMiscFlds
   1 3                                                                  */
/* SETTINGS FOR BUTTON btnRenameGroup IN FRAME fMiscFlds
   1                                                                    */
/* SETTINGS FOR BUTTON btnRenameTab IN FRAME fMiscFlds
   1                                                                    */
/* SETTINGS FOR BUTTON btnSelection-List IN FRAME fMiscFlds
   1 3                                                                  */
/* SETTINGS FOR BUTTON btnSlider IN FRAME fMiscFlds
   1 3                                                                  */
/* SETTINGS FOR BUTTON btnTest IN FRAME fMiscFlds
   1                                                                    */
/* SETTINGS FOR BUTTON btnText IN FRAME fMiscFlds
   1 3                                                                  */
/* SETTINGS FOR BUTTON btnToggle-Box IN FRAME fMiscFlds
   1 3                                                                  */
/* SETTINGS FOR BUTTON btnUpTab IN FRAME fMiscFlds
   1                                                                    */
/* SETTINGS FOR FILL-IN combo-BoxLabel IN FRAME fMiscFlds
   NO-ENABLE 1 2                                                        */
ASSIGN 
       combo-BoxLabel:READ-ONLY IN FRAME fMiscFlds        = TRUE.

/* SETTINGS FOR FILL-IN editorLabel IN FRAME fMiscFlds
   NO-ENABLE 1 2                                                        */
ASSIGN 
       editorLabel:READ-ONLY IN FRAME fMiscFlds        = TRUE.

/* SETTINGS FOR FILL-IN fill-InLabel IN FRAME fMiscFlds
   NO-ENABLE 1 2                                                        */
ASSIGN 
       fill-InLabel:READ-ONLY IN FRAME fMiscFlds        = TRUE.

/* SETTINGS FOR FILL-IN gapField IN FRAME fMiscFlds
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       gapField:HIDDEN IN FRAME fMiscFlds           = TRUE.

/* SETTINGS FOR FILL-IN gapFieldLabel IN FRAME fMiscFlds
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mfgroupField IN FRAME fMiscFlds
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       mfgroupField:HIDDEN IN FRAME fMiscFlds           = TRUE.

/* SETTINGS FOR COMBO-BOX mfgroupTab IN FRAME fMiscFlds
   1                                                                    */
/* SETTINGS FOR FILL-IN pointerLabel IN FRAME fMiscFlds
   NO-ENABLE 1 2                                                        */
ASSIGN 
       pointerLabel:READ-ONLY IN FRAME fMiscFlds        = TRUE.

/* SETTINGS FOR FILL-IN prgrmsLabel IN FRAME fMiscFlds
   NO-ENABLE ALIGN-L 1                                                  */
/* SETTINGS FOR FILL-IN radio-SetLabel IN FRAME fMiscFlds
   NO-ENABLE 1 2                                                        */
ASSIGN 
       radio-SetLabel:READ-ONLY IN FRAME fMiscFlds        = TRUE.

/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME fMiscFlds
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rectangleLabel IN FRAME fMiscFlds
   NO-ENABLE 1 2                                                        */
ASSIGN 
       rectangleLabel:READ-ONLY IN FRAME fMiscFlds        = TRUE.

/* SETTINGS FOR FILL-IN selection-ListLabel IN FRAME fMiscFlds
   NO-ENABLE 1 2                                                        */
ASSIGN 
       selection-ListLabel:READ-ONLY IN FRAME fMiscFlds        = TRUE.

/* SETTINGS FOR FILL-IN sliderLabel IN FRAME fMiscFlds
   NO-ENABLE 1 2                                                        */
ASSIGN 
       sliderLabel:READ-ONLY IN FRAME fMiscFlds        = TRUE.

/* SETTINGS FOR FILL-IN tabCBLabel IN FRAME fMiscFlds
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tabField IN FRAME fMiscFlds
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       tabField:HIDDEN IN FRAME fMiscFlds           = TRUE.

/* SETTINGS FOR SELECTION-LIST tabLabels IN FRAME fMiscFlds
   1                                                                    */
/* SETTINGS FOR FILL-IN tabNavLabel IN FRAME fMiscFlds
   NO-ENABLE ALIGN-L 1                                                  */
/* SETTINGS FOR FILL-IN textLabel IN FRAME fMiscFlds
   NO-ENABLE 1 2                                                        */
ASSIGN 
       textLabel:READ-ONLY IN FRAME fMiscFlds        = TRUE.

/* SETTINGS FOR FILL-IN toggle-BoxLabel IN FRAME fMiscFlds
   NO-ENABLE 1 2                                                        */
ASSIGN 
       toggle-BoxLabel:READ-ONLY IN FRAME fMiscFlds        = TRUE.

/* SETTINGS FOR SLIDER udfHeightSize IN FRAME fMiscFlds
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       udfHeightSize:HIDDEN IN FRAME fMiscFlds           = TRUE.

/* SETTINGS FOR SLIDER udfWidthSize IN FRAME fMiscFlds
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       udfWidthSize:HIDDEN IN FRAME fMiscFlds           = TRUE.

/* SETTINGS FOR FILL-IN widgetLabel IN FRAME fMiscFlds
   NO-ENABLE 1                                                          */
/* SETTINGS FOR RECTANGLE widgetRect IN FRAME fMiscFlds
   1                                                                    */
/* SETTINGS FOR FRAME folderFrm
                                                                        */
ASSIGN 
       FRAME folderFrm:BOX-SELECTABLE   = TRUE
       FRAME folderFrm:SELECTABLE       = TRUE.

/* SETTINGS FOR RECTANGLE Rect-Main IN FRAME folderFrm
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMiscFlds
/* Query rebuild information for FRAME fMiscFlds
     _Query            is NOT OPENED
*/  /* FRAME fMiscFlds */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME folderFrm
/* Query rebuild information for FRAME folderFrm
     _Query            is NOT OPENED
*/  /* FRAME folderFrm */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* User Defined Fields Builder */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* User Defined Fields Builder */
DO:
  IF hMFPersist NE ? THEN DELETE PROCEDURE hMFPersist.
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-RESIZED OF W-Win /* User Defined Fields Builder */
DO:
  RUN winReSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME folderFrm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL folderFrm W-Win
ON ENTRY OF FRAME folderFrm
DO:
  FRAME folderFrm:SELECTED = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL folderFrm W-Win
ON LEAVE OF FRAME folderFrm
DO:
  FRAME folderFrm:SELECTED = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL folderFrm W-Win
ON MOUSE-SELECT-CLICK OF FRAME folderFrm
DO:
  /* only done when a new widget is being placed in design folderFrm */
  IF CAN-DO("COMBO-BOX,EDITOR,FILL-IN,RADIO-SET,RECTANGLE,SELECTION-LIST,SLIDER,TEXT,TOGGLE-BOX",currentLabel) THEN DO:
    RUN newWidget.
    APPLY "CHOOSE" TO btnPointer IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAddGroup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddGroup W-Win
ON CHOOSE OF btnAddGroup IN FRAME fMiscFlds /* Add Group */
DO:
  ASSIGN
    newMFGroup = YES
    copyMFGroup = NO
    .
  ENABLE mfgroupField WITH FRAME {&FRAME-NAME}.
  mfgroupField:SCREEN-VALUE = "".
  APPLY "ENTRY" TO mfgroupField.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAddTab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddTab W-Win
ON CHOOSE OF btnAddTab IN FRAME fMiscFlds /* Add */
DO:
  newTab = YES.
  ENABLE tabField WITH FRAME {&FRAME-NAME}.
  tabField:SCREEN-VALUE = "".
  APPLY "ENTRY" TO tabField.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCombo-Box
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCombo-Box W-Win
ON CHOOSE OF btnCombo-Box IN FRAME fMiscFlds /* Combo Box */
DO:
  RUN selectNewWidgetType ("Combo-Box","images/combbox.cur").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCopy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCopy W-Win
ON CHOOSE OF btnCopy IN FRAME fMiscFlds /* Copy */
DO:
  RUN copyWidgets.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCopyGroup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCopyGroup W-Win
ON CHOOSE OF btnCopyGroup IN FRAME fMiscFlds /* Add Group */
DO:
  ASSIGN
    copyMFGroup = YES
    newMFGroup = NO
    .
  ENABLE mfgroupField WITH FRAME {&FRAME-NAME}.
  APPLY "ENTRY" TO mfgroupField IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCut W-Win
ON CHOOSE OF btnCut IN FRAME fMiscFlds /* Cut */
DO:
  RUN cutWidgets.
  savePrompt = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDeleteGroup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeleteGroup W-Win
ON CHOOSE OF btnDeleteGroup IN FRAME fMiscFlds /* Delete Group */
DO:
  RUN deleteGroup.
  savePrompt = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDeleteTab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeleteTab W-Win
ON CHOOSE OF btnDeleteTab IN FRAME fMiscFlds /* Delete */
DO:
  RUN deleteTab.
  savePrompt = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDownTab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDownTab W-Win
ON CHOOSE OF btnDownTab IN FRAME fMiscFlds /* Move Dn */
DO:
  RUN moveTab ("Down").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEditor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEditor W-Win
ON CHOOSE OF btnEditor IN FRAME fMiscFlds /* Editor */
DO:
  RUN selectNewWidgetType ("Editor","images/editor.cur").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExit W-Win
ON CHOOSE OF btnExit IN FRAME fMiscFlds /* Exit */
DO:
    IF savePrompt THEN DO:
        MESSAGE "Unsaved changes exist. Do you want to save?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
            TITLE "Warning - Changes Not Saved"
            UPDATE promptToSave AS LOGICAL.
        IF promptToSave THEN
        APPLY "CHOOSE":U TO btnSave IN FRAME {&FRAME-NAME}.
        ELSE IF promptToSave EQ ? THEN RETURN NO-APPLY.
    END.
    IF VALID-HANDLE(hMFPersist) THEN DELETE PROCEDURE hMFPersist.
    APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExport W-Win
ON CHOOSE OF btnExport IN FRAME fMiscFlds /* Export */
DO:
  RUN exportLayouts.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFill-In
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFill-In W-Win
ON CHOOSE OF btnFill-In IN FRAME fMiscFlds /* Fill In */
DO:
  RUN selectNewWidgetType ("Fill-In","images/fill_in.cur").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnImport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnImport W-Win
ON CHOOSE OF btnImport IN FRAME fMiscFlds /* Import */
DO:
  RUN importLayouts.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMFPrgrms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMFPrgrms W-Win
ON CHOOSE OF btnMFPrgrms IN FRAME fMiscFlds /* Programs */
DO:
    RUN attachPrgrms.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNextTab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNextTab W-Win
ON CHOOSE OF btnNextTab IN FRAME fMiscFlds /* Next Tab */
DO:
  mfgroupTab:SCREEN-VALUE IN FRAME {&FRAME-NAME} =
      STRING(INTEGER(mfgroupTab:SCREEN-VALUE IN FRAME {&FRAME-NAME}) + 1).
  RUN createWidgets.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPaste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPaste W-Win
ON CHOOSE OF btnPaste IN FRAME fMiscFlds /* Paste */
DO:
  RUN pasteFromClipboard.
  savePrompt = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPointer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPointer W-Win
ON CHOOSE OF btnPointer IN FRAME fMiscFlds /* Pointer */
DO:
  RUN selectNewWidgetType ("","arrow").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrevTab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrevTab W-Win
ON CHOOSE OF btnPrevTab IN FRAME fMiscFlds /* Previous Tab */
DO:
  mfgroupTab:SCREEN-VALUE IN FRAME {&FRAME-NAME} =
      STRING(INTEGER(mfgroupTab:SCREEN-VALUE IN FRAME {&FRAME-NAME}) - 1).
  RUN createWidgets.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnProperty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnProperty W-Win
ON CHOOSE OF btnProperty IN FRAME fMiscFlds /* Property */
DO:
  RUN propertySheet.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRadio-Set
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRadio-Set W-Win
ON CHOOSE OF btnRadio-Set IN FRAME fMiscFlds /* Radio Set */
DO:
  RUN selectNewWidgetType ("Radio-Set","images/radioset.cur").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRectangle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRectangle W-Win
ON CHOOSE OF btnRectangle IN FRAME fMiscFlds /* Rectangle */
DO:
  RUN selectNewWidgetType ("Rectangle","images/rectangle.cur").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRenameGroup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRenameGroup W-Win
ON CHOOSE OF btnRenameGroup IN FRAME fMiscFlds /* Rename Group */
DO:
  ASSIGN
    newMFGroup = NO
    copyMFGroup = NO
    .
  ENABLE mfgroupField WITH FRAME {&FRAME-NAME}.
  mfgroupField:SCREEN-VALUE = "".
  APPLY "ENTRY" TO mfgroupField.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRenameTab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRenameTab W-Win
ON CHOOSE OF btnRenameTab IN FRAME fMiscFlds /* Rename */
DO:
  newTab = NO.
  ENABLE tabField WITH FRAME {&FRAME-NAME}.
  ASSIGN
    tabField:SCREEN-VALUE = ENTRY(1,tabLabels:SCREEN-VALUE,"|")
    tabOrder = IF NUM-ENTRIES(tabLabels:SCREEN-VALUE,"|") GT 1 THEN
               ENTRY(2,tabLabels:SCREEN-VALUE,"|") ELSE "Default"
    .
  APPLY "ENTRY" TO tabField.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRestore
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRestore W-Win
ON CHOOSE OF btnRestore IN FRAME fMiscFlds /* Reset */
DO:
  mfgroupTab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1".
  RUN loadWidgetData.
  savePrompt = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave W-Win
ON CHOOSE OF btnSave IN FRAME fMiscFlds /* Save */
DO:
  {methods/wait.i}
  RUN saveLayout.
  savePrompt = NO.
  {methods/nowait.i}
  MESSAGE "UDF Build Saved" VIEW-AS ALERT-BOX TITLE "Save".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelection-List
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelection-List W-Win
ON CHOOSE OF btnSelection-List IN FRAME fMiscFlds /* Selection List */
DO:
  RUN selectNewWidgetType ("Selection-List","images/slctlist.cur").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSlider
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSlider W-Win
ON CHOOSE OF btnSlider IN FRAME fMiscFlds /* Slider */
DO:
  RUN selectNewWidgetType ("Slider","images/slider.cur").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTabOrder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTabOrder W-Win
ON CHOOSE OF btnTabOrder IN FRAME fMiscFlds /* Tab Order */
DO:
  RUN tabOrder.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTest W-Win
ON CHOOSE OF btnTest IN FRAME fMiscFlds /* Test */
DO:
  currentWidget = SESSION:FIRST-PROCEDURE.
  DO WHILE currentWidget NE ?:
    IF INDEX(currentWidget:FILE-NAME,"mfvalues.") NE 0 THEN DO:
      saveWidget = currentWidget:NEXT-SIBLING.
      APPLY "CLOSE" TO currentWidget.
      currentWidget = saveWidget.
    END.
    ELSE currentWidget = currentWidget:NEXT-SIBLING.
  END.
  FIND prgrms WHERE prgrms.prgmname = "prgrms." NO-LOCK.
  RUN UDF/mfvalues.w PERSISTENT
      (mfgroupList:SCREEN-VALUE IN FRAME {&FRAME-NAME},
       prgrms.rec_key,{methods/headers/prgrms.i},?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnText W-Win
ON CHOOSE OF btnText IN FRAME fMiscFlds /* Text */
DO:
  RUN selectNewWidgetType ("Text","images/text.cur").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnToggle-Box
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnToggle-Box W-Win
ON CHOOSE OF btnToggle-Box IN FRAME fMiscFlds /* Toggle Box */
DO:
  RUN selectNewWidgetType ("Toggle-Box","images/toggle.cur").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUpTab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUpTab W-Win
ON CHOOSE OF btnUpTab IN FRAME fMiscFlds /* Move Up */
DO:
  RUN moveTab ("Up").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gapField
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gapField W-Win
ON LEAVE OF gapField IN FRAME fMiscFlds
DO:
  APPLY "RETURN" TO {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gapField W-Win
ON RETURN OF gapField IN FRAME fMiscFlds
DO:
  IF {&SELF-NAME}:SCREEN-VALUE NE "0" THEN
  RUN alignWidgets ("Gap").
  {&SELF-NAME}:HIDDEN = YES.
  DISABLE {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mfgroupField
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mfgroupField W-Win
ON LEAVE OF mfgroupField IN FRAME fMiscFlds /* Group */
DO:
  APPLY "RETURN" TO {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mfgroupField W-Win
ON RETURN OF mfgroupField IN FRAME fMiscFlds /* Group */
DO:
  IF {&SELF-NAME}:SCREEN-VALUE NE "" THEN DO:
    ASSIGN
      cdummy = {&SELF-NAME}:SCREEN-VALUE
      ldummy = NO
      currentLabel = ""
      savePrompt = YES
      .
    DO i = 1 TO mfgroupList:NUM-ITEMS: /* see if item already exists */
      IF mfgroupList:ENTRY(i) EQ cdummy THEN ldummy = YES.
    END.
    IF newMFGroup THEN RUN addGroup.
    ELSE IF copyMFGroup THEN RUN copyGroup.
    ELSE RUN renameGroup.
  END.
  ELSE cdummy = mfgroupList:SCREEN-VALUE.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      mfgroupList:SCREEN-VALUE = cdummy
      {&SELF-NAME}:HIDDEN = YES
      .
    DISABLE {&SELF-NAME}.
    APPLY "ENTRY" TO mfgroupList.
    APPLY "VALUE-CHANGED" TO mfgroupList.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mfgroupList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mfgroupList W-Win
ON VALUE-CHANGED OF mfgroupList IN FRAME fMiscFlds /* Group */
DO:
  mfgroupTab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1". /* set to first tab */
  RUN createWidgets.
  RUN btnMFPrgrmsToolTip.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mfgroupTab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mfgroupTab W-Win
ON VALUE-CHANGED OF mfgroupTab IN FRAME fMiscFlds
DO:
  DO i = 1 TO tabLabels:NUM-ITEMS: /* get index value of selected label */
    IF tabLabels:SCREEN-VALUE = tabLabels:ENTRY(i) THEN
    LEAVE.
  END.
  IF i = INTEGER(mfgroupTab:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN
  RETURN NO-APPLY.
  RUN createWidgets.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_1_LeftRight_Sides
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_1_LeftRight_Sides W-Win
ON CHOOSE OF MENU-ITEM m_1_LeftRight_Sides /* 1 Left/Right Sides */
DO:
  leftright = YES.
  ENABLE gapField WITH FRAME {&FRAME-NAME}.
  gapField:SCREEN-VALUE = "".
  APPLY "ENTRY" TO gapField.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_2_TopBottom_Sides
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_2_TopBottom_Sides W-Win
ON CHOOSE OF MENU-ITEM m_2_TopBottom_Sides /* 2 Top/Bottom Sides */
DO:
  leftright = NO.
  ENABLE gapField WITH FRAME {&FRAME-NAME}.
  gapField:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  APPLY "ENTRY" TO gapField IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_addGroup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_addGroup W-Win
ON CHOOSE OF MENU-ITEM m_addGroup /* Add Group */
DO:
  APPLY "CHOOSE" TO btnAddGroup IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Add_Tab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Add_Tab W-Win
ON CHOOSE OF MENU-ITEM m_Add_Tab /* Add Tab */
DO:
  APPLY "CHOOSE" TO btnAddTab IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Attributes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Attributes W-Win
ON CHOOSE OF MENU-ITEM m_Attributes /* Attributes */
DO:
  APPLY "CHOOSE" TO btnProperty IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Bottom_Sides
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Bottom_Sides W-Win
ON CHOOSE OF MENU-ITEM m_Bottom_Sides /* Bottom Sides */
DO:
  RUN alignWidgets ("Bottom").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Change_Group
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Change_Group W-Win
ON CHOOSE OF MENU-ITEM m_Change_Group /* Rename Group */
DO:
  APPLY "CHOOSE" TO btnRenameGroup IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Combo_Box
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Combo_Box W-Win
ON CHOOSE OF MENU-ITEM m_Combo_Box /* Combo Box */
DO:
  APPLY "CHOOSE" TO btnCombo-Box IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Copy W-Win
ON CHOOSE OF MENU-ITEM m_Copy /* Copy */
DO:
  APPLY "CHOOSE" TO btnCopy IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_copyGroup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_copyGroup W-Win
ON CHOOSE OF MENU-ITEM m_copyGroup /* Copy Group */
DO:
  APPLY "CHOOSE" TO btnCopyGroup IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Cut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Cut W-Win
ON CHOOSE OF MENU-ITEM m_Cut /* Cut */
DO:
  APPLY "CHOOSE" TO btnCut IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_deleteGroup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_deleteGroup W-Win
ON CHOOSE OF MENU-ITEM m_deleteGroup /* Delete Group */
DO:
  APPLY "CHOOSE" TO btnDeleteGroup IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Delete_Tab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Delete_Tab W-Win
ON CHOOSE OF MENU-ITEM m_Delete_Tab /* Delete Tab */
DO:
  APPLY "CHOOSE" TO btnDeleteTab IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Editor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Editor W-Win
ON CHOOSE OF MENU-ITEM m_Editor /* Editor */
DO:
  APPLY "CHOOSE" TO btnEditor IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Exit W-Win
ON CHOOSE OF MENU-ITEM m_Exit /* Exit */
DO:
  APPLY "CHOOSE" TO btnExit IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Fill_In
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Fill_In W-Win
ON CHOOSE OF MENU-ITEM m_Fill_In /* Fill In */
DO:
  APPLY "CHOOSE" TO btnFill-In IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Left_Sides
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Left_Sides W-Win
ON CHOOSE OF MENU-ITEM m_Left_Sides /* Left Sides */
DO:
  RUN alignWidgets ("Left").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Move_Tab_Down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Move_Tab_Down W-Win
ON CHOOSE OF MENU-ITEM m_Move_Tab_Down /* Move Tab Down */
DO:
  APPLY "CHOOSE" TO btnDownTab IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Move_Tab_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Move_Tab_Up W-Win
ON CHOOSE OF MENU-ITEM m_Move_Tab_Up /* Move Tab Up */
DO:
  APPLY "CHOOSE" TO btnUpTab IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Next_Tab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Next_Tab W-Win
ON CHOOSE OF MENU-ITEM m_Next_Tab /* Next Tab */
DO:
  APPLY "CHOOSE" TO btnNextTab IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Paste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Paste W-Win
ON CHOOSE OF MENU-ITEM m_Paste /* Paste */
DO:
  APPLY "CHOOSE" TO btnPaste IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Pointer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Pointer W-Win
ON CHOOSE OF MENU-ITEM m_Pointer /* Pointer */
DO:
  APPLY "CHOOSE" TO btnPointer IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Previous_Tab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Previous_Tab W-Win
ON CHOOSE OF MENU-ITEM m_Previous_Tab /* Previous Tab */
DO:
  APPLY "CHOOSE" TO btnPrevTab IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Radio_Set
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Radio_Set W-Win
ON CHOOSE OF MENU-ITEM m_Radio_Set /* Radio Set */
DO:
  APPLY "CHOOSE" TO btnRadio-Set IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Rename_Tab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Rename_Tab W-Win
ON CHOOSE OF MENU-ITEM m_Rename_Tab /* Rename Tab */
DO:
  APPLY "CHOOSE" TO btnRenameTab IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Reset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Reset W-Win
ON CHOOSE OF MENU-ITEM m_Reset /* Reset */
DO:
  APPLY "CHOOSE" TO btnRestore IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Right_Sides
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Right_Sides W-Win
ON CHOOSE OF MENU-ITEM m_Right_Sides /* Right Sides */
DO:
  RUN alignWidgets ("Right").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Save W-Win
ON CHOOSE OF MENU-ITEM m_Save /* Save */
DO:
  APPLY "CHOOSE" TO btnSave IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Selection_List
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Selection_List W-Win
ON CHOOSE OF MENU-ITEM m_Selection_List /* Selection List */
DO:
  APPLY "CHOOSE" TO btnSelection-List IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Slider
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Slider W-Win
ON CHOOSE OF MENU-ITEM m_Slider /* Slider */
DO:
  APPLY "CHOOSE" TO btnSlider IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Tab_Order
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Tab_Order W-Win
ON CHOOSE OF MENU-ITEM m_Tab_Order /* Tab Order */
DO:
  APPLY "CHOOSE" TO btnTabOrder IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Text
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Text W-Win
ON CHOOSE OF MENU-ITEM m_Text /* Text */
DO:
  APPLY "CHOOSE" TO btnText IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Toggle_Box
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Toggle_Box W-Win
ON CHOOSE OF MENU-ITEM m_Toggle_Box /* Toggle Box */
DO:
  APPLY "CHOOSE" TO btnToggle-Box IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Top_Sides
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Top_Sides W-Win
ON CHOOSE OF MENU-ITEM m_Top_Sides /* Top Sides */
DO:
  RUN alignWidgets ("Top").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tabField
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tabField W-Win
ON LEAVE OF tabField IN FRAME fMiscFlds
DO:
  APPLY "RETURN" TO {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tabField W-Win
ON RETURN OF tabField IN FRAME fMiscFlds
DO:
  IF {&SELF-NAME}:SCREEN-VALUE NE "" THEN DO:
    IF newTab THEN RUN addTab.
    ELSE RUN renameTab.
    savePrompt = YES.
  END.
  tabField:HIDDEN = YES.
  DISABLE tabField.
  APPLY "VALUE-CHANGED" TO tabLabels.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tabLabels
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tabLabels W-Win
ON VALUE-CHANGED OF tabLabels IN FRAME fMiscFlds
DO:
  DO i = 1 TO {&SELF-NAME}:NUM-ITEMS: /* get index value of selected label */
    IF {&SELF-NAME}:SCREEN-VALUE = {&SELF-NAME}:ENTRY(i) THEN
    LEAVE.
  END.
  IF i = INTEGER(mfgroupTab:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN
  RETURN NO-APPLY.
  mfgroupTab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(i).
  RUN createWidgets.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME udfHeightSize
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL udfHeightSize W-Win
ON VALUE-CHANGED OF udfHeightSize IN FRAME fMiscFlds
DO:
    ASSIGN {&SELF-NAME} udfWidthSize.
    RUN setLayoutSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME udfWidthSize
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL udfWidthSize W-Win
ON VALUE-CHANGED OF udfWidthSize IN FRAME fMiscFlds
DO:
    ASSIGN {&SELF-NAME} udfHeightSize.
    RUN setLayoutSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


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

ON ESC OF {&WINDOW-NAME} ANYWHERE DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
RUN util/CheckModule.p ("ASI","UDFB", YES, OUTPUT continue ).
&ELSE
continue = YES.
&ENDIF

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  IF continue THEN DO:
      RUN loadImages.
      RUN enable_UI.
      RUN winReSize.
      RUN loadWidgetData.
      DISABLE btnPaste btnPointer {&propertyCutCopy} WITH FRAME {&FRAME-NAME}.
      IF hMFPersist EQ ? THEN
      RUN UDF/mfPersist.p PERSISTENT SET hMFPersist.
      RUN btnMFPrgrmsToolTip.
  END. /* continue */
  savePrompt = NO.
  {methods/nowait.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
  IF NOT continue THEN
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addGroup W-Win 
PROCEDURE addGroup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT ldummy THEN DO:
    ldummy = mfgroupList:ADD-LAST(cdummy) IN FRAME {&FRAME-NAME}.
    CREATE ttMFGroup.
    ASSIGN
      ttMFGroup.mfgroup_data = cdummy
      ttMFGroup.mfgroup_tabs = "Tab|Default".
  END.
  ELSE MESSAGE "Group" cdummy "Already Exists!" VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addTab W-Win 
PROCEDURE addTab :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    DO i = 1 TO tabLabels:NUM-ITEMS:   /* insert is after selected item, */
      IF tabLabels:IS-SELECTED(i) THEN /* find out which one is selected */
      LEAVE.
    END.
    FIND ttMFGroup
        WHERE ttMFGroup.mfgroup_data EQ mfgroupList:SCREEN-VALUE.
    ASSIGN
      ldummy = tabLabels:INSERT(tabField:SCREEN-VALUE + "|Default",i + 1)
      tabLabels:SCREEN-VALUE = tabLabels:ENTRY(i + 1)
      ldummy = mfgroupTab:ADD-LAST(STRING(tabLabels:NUM-ITEMS))
      ttMFGroup.mfgroup_tabs = tabLabels:LIST-ITEMS
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE alignWidgets W-Win 
PROCEDURE alignWidgets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER alignType AS CHARACTER NO-UNDO.
  DEFINE VARIABLE newCoord AS INTEGER NO-UNDO.
  DEFINE VARIABLE prevCoord AS INTEGER NO-UNDO.
  DEFINE VARIABLE recidStr AS CHARACTER NO-UNDO.

  CASE alignType:
    WHEN "Left" OR WHEN "Top" THEN
    newCoord = 9999.
    WHEN "Right" OR WHEN "Bottom" THEN
    newCoord = -9999.
    WHEN "Gap" THEN
    newCoord = INTEGER(gapField:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  END CASE.

  ASSIGN
    currentWidget = FRAME folderFrm:HANDLE
    currentWidget = currentWidget:FIRST-CHILD
    currentWidget = currentWidget:FIRST-CHILD
    .
  DO WHILE currentWidget NE ?:
    IF currentWidget:SELECTED THEN
    CASE alignType:
      WHEN "Left" THEN
      IF currentWidget:X LE newCoord THEN newCoord = currentWidget:X.
      WHEN "Right" THEN
      IF currentWidget:X + currentWidget:WIDTH-PIXELS GE newCoord THEN
      newCoord = currentWidget:X + currentWidget:WIDTH-PIXELS.
      WHEN "Top" THEN
      IF currentWidget:Y LE newCoord THEN newCoord = currentWidget:Y.
      WHEN "Bottom" THEN
      IF currentWidget:Y + currentWidget:HEIGHT-PIXELS GE newCoord THEN
      newCoord = currentWidget:Y + currentWidget:HEIGHT-PIXELS.
      WHEN "Gap" THEN
      recidStr = recidStr + ENTRY(1,currentWidget:PRIVATE-DATA,"|") + ",".
    END CASE.
    currentWidget = currentWidget:NEXT-SIBLING.
  END.

  IF alignType = "Gap" THEN DO:
    RUN createWidgets.
    ASSIGN
      currentWidget = FRAME folderFrm:HANDLE
      currentWidget = currentWidget:FIRST-CHILD
      currentWidget = currentWidget:FIRST-CHILD
      .
    DO WHILE currentWidget NE ?:
      IF currentWidget:PRIVATE-DATA NE ? AND
         LOOKUP(ENTRY(1,currentWidget:PRIVATE-DATA,"|"),recidStr) NE 0 THEN
      currentWidget:SELECTED = YES.
      currentWidget = currentWidget:NEXT-SIBLING.
    END.
  END.

  ASSIGN
    currentWidget = FRAME folderFrm:HANDLE
    currentWidget = currentWidget:FIRST-CHILD
    currentWidget = currentWidget:FIRST-CHILD
    .
  DO WHILE currentWidget NE ?:
    IF currentWidget:SELECTED THEN DO:
      CASE alignType:
        WHEN "Left" THEN
        currentWidget:X = newCoord.
        WHEN "Right" THEN
        currentWidget:X = newCoord - currentWidget:WIDTH-PIXELS.
        WHEN "Top" THEN
        currentWidget:Y = newCoord.
        WHEN "Bottom" THEN
        currentWidget:Y = newCoord - currentWidget:HEIGHT-PIXELS.
        WHEN "Gap" THEN
        CASE LeftRight:
          WHEN yes THEN DO:
            IF prevCoord NE 0 THEN
            currentWidget:X = prevCoord + newCoord.
            prevCoord = currentWidget:X + currentWidget:WIDTH-PIXELS.
          END.
          WHEN no THEN DO:
            IF prevCoord NE 0 THEN
            currentWidget:Y = prevCoord + newCoord.
            prevCoord = currentWidget:Y + currentWidget:HEIGHT-PIXELS.
          END.
        END CASE.
      END CASE.
      RUN updateAttrb.
      IF NOT CAN-DO("RECTANGLE,TEXT,TOGGLE-BOX",currentWidget:TYPE) THEN DO:
        labelWidget = currentWidget:SIDE-LABEL-HANDLE.
        DELETE WIDGET labelWidget.
      END.
      saveWidget = currentWidget:NEXT-SIBLING.
      DELETE WIDGET currentWidget.
      RUN dynamicWidget.
      currentWidget = saveWidget.
    END.
    ELSE
    currentWidget = currentWidget:NEXT-SIBLING.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE attachPrgrms W-Win 
PROCEDURE attachPrgrms :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN UDF/mfprgrms.w (mfgroupList:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                        INPUT-OUTPUT savePrompt).
    RUN btnMFPrgrmsToolTip.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnMFPrgrmsToolTip W-Win 
PROCEDURE btnMFPrgrmsToolTip :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cToolTip AS CHARACTER NO-UNDO.

    FOR EACH ttMFPrgrms
        WHERE ttMFPrgrms.mfgroup EQ mfgroupList:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        :
        cToolTip = cToolTip + CHR(10)
                 + ttMFPrgrms.prgmname + ": "
                 + ttMFPrgrms.prgtitle.
    END. /* each ttmfprgrms */
    btnMFPrgrms:TOOLTIP = "Attached Programs:" + chr(10) + cToolTip.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clearClipboard W-Win 
PROCEDURE clearClipboard :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH wtClipboard:
    DELETE wtClipboard.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyGroup W-Win 
PROCEDURE copyGroup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-ttMFGroup FOR ttMFGroup.

  IF NOT ldummy THEN DO:
     SESSION:SET-WAIT-STATE("general").
     mfgroupList:ADD-LAST(cdummy) IN FRAME {&FRAME-NAME}.
     FIND b-ttMFGroup
         WHERE b-ttMFGroup.mfgroup_data EQ mfgroupList:SCREEN-VALUE
         NO-ERROR.
     IF AVAIL b-ttMFGroup THEN DO:
        CREATE ttMFGroup.
        ASSIGN
          ttMFGroup.mfgroup_data = cdummy
          ttMFGroup.mfgroup_tabs = b-ttMFGroup.mfgroup_tabs
          .
     END.
     FOR EACH ttAttrb
         WHERE ttAttrb.attr_mfgroup EQ mfgroupList:SCREEN-VALUE IN FRAME {&FRAME-NAME}
         :
         CREATE bAttrb.
         RUN setAttrID-buffer.
         BUFFER-COPY ttAttrb EXCEPT attr_id attr_mfgroup TO bAttrb
             ASSIGN bAttrb.attr_mfgroup = mfgroupField:SCREEN-VALUE.
         RELEASE bAttrb.
     END.
     SESSION:SET-WAIT-STATE("").
  END.
  ELSE MESSAGE "Group" cdummy "Already Exists!" VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyToClipboard W-Win 
PROCEDURE copyToClipboard :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND ttAttrb
       WHERE ROWID(ttAttrb) EQ TO-ROWID(ENTRY(1,currentWidget:PRIVATE-DATA,"|")).
  CREATE wtClipboard.
  BUFFER-COPY ttAttrb TO wtClipboard.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyWidgets W-Win 
PROCEDURE copyWidgets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN clearClipboard.
  ASSIGN
    currentWidget = FRAME folderFrm:HANDLE
    currentWidget = currentWidget:FIRST-CHILD
    currentWidget = currentWidget:FIRST-CHILD
    .
  DO WHILE currentWidget NE ?:
    IF currentWidget:SELECTED THEN
    RUN copyToClipboard.
    currentWidget = currentWidget:NEXT-SIBLING.
  END.
  IF CAN-FIND(FIRST wtClipboard) THEN
  ENABLE btnPaste WITH FRAME {&FRAME-NAME}.
  ELSE
  DISABLE btnPaste WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createLabelWidget W-Win 
PROCEDURE createLabelWidget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcType  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipcLabel AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipiX     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipiY     AS INTEGER NO-UNDO.

  IF NOT CAN-DO("RECTANGLE,TEXT,TOGGLE-BOX",ipcType) THEN DO: /* text and toggle-box have no labels */
    CREATE TEXT labelWidget IN WIDGET-POOL "{&widgetPoolName}"
      ASSIGN
        FRAME = FRAME folderFrm:HANDLE
        AUTO-RESIZE = YES
        Y = ipiY
        FORMAT = "X(" +
          (IF LENGTH(ipcLabel) NE 0 THEN STRING(LENGTH(ipcLabel) + 1)
           ELSE "1") + ")"
        SENSITIVE = YES
        SCREEN-VALUE = IF ipcLabel = "" THEN "" ELSE ipcLabel + ":"
        .
    ASSIGN
      labelWidget:FONT = ?
      labelWidget:X = IF ipiX - labelWidget:WIDTH-PIXELS - 1 LT 0 THEN 0
                      ELSE ipiX - labelWidget:WIDTH-PIXELS - 1
      labelWidget:HEIGHT-CHARS = 1
      ldummy = labelWidget:MOVE-TO-TOP()
      labelWidget:HIDDEN = NO
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createTabs W-Win 
PROCEDURE createTabs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

&IF DEFINED(FWD-VERSION) EQ 0 &THEN
  RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
&ELSE
  ACTIVE-WINDOW:DISABLE-REDRAW = TRUE.
&ENDIF
  DO WITH FRAME {&FRAME-NAME}:
    FIND ttMFGroup
         WHERE ttMFGroup.mfgroup_data EQ mfgroupList:SCREEN-VALUE
         NO-ERROR.
    IF NOT AVAILABLE ttMFGroup THEN DO:
      ASSIGN
        mfgroupTab:LIST-ITEMS = "1"
        tabLabels:LIST-ITEMS = ""
        tabLabels:INNER-LINES = 1
        .
      DISABLE {&notFoundIn234} {&widgetButtons}.
      RETURN.
    END.
    ELSE DO:
      ASSIGN
        udfWidthSize = ttMFGroup.udfWidth
        udfHeightSize = ttMFGroup.udfHeight
        .
      RUN setLayoutSize.
      ENABLE {&notFoundIn234} {&widgetButtons}.
      RUN selectNewWidgetType ("","arrow").
    END.
    ASSIGN
      currentTab = INTEGER(mfgroupTab:SCREEN-VALUE)
      cdummy = "1"
      .
    DO i = 2 TO NUM-ENTRIES(ttMFGroup.mfgroup_tabs):
      cdummy = cdummy + "," + STRING(i).
    END.
    ASSIGN
      mfgroupTab:INNER-LINES = NUM-ENTRIES(ttMFGroup.mfgroup_tabs)
      mfgroupTab:LIST-ITEMS = cdummy
      mfgroupTab:SCREEN-VALUE = STRING(currentTab)
      tabLabels:INNER-LINES = NUM-ENTRIES(ttMFGroup.mfgroup_tabs)
      tabLabels:LIST-ITEMS = ttMFGroup.mfgroup_tabs
      tabLabels:SCREEN-VALUE = tabLabels:ENTRY(currentTab)
      .
    IF currentTab EQ 1 THEN DISABLE btnPrevTab.
    IF currentTab EQ NUM-ENTRIES(ttMFGroup.mfgroup_tabs) THEN DISABLE btnNextTab.
    DO i = 1 TO tabLabels:NUM-ITEMS:
      IF tabLabels:IS-SELECTED(i) THEN LEAVE.
    END.
    IF tabLabels:NUM-ITEMS EQ EXTENT(tabLabel) THEN DISABLE btnAddTab.
    IF tabLabels:NUM-ITEMS EQ 1 THEN DISABLE btnDeleteTab.
    IF i EQ 1 THEN DISABLE btnUpTab.
    IF i EQ tabLabels:NUM-ITEMS THEN DISABLE btnDownTab.
  END.
  DO i = 1 TO tabLabels:NUM-ITEMS:
    IF NUM-ENTRIES(tabLabels:ENTRY(i),"|") GT 1 THEN NEXT.
    tabLabels:REPLACE(tabLabels:ENTRY(i) + "|Default",tabLabels:ENTRY(i)).
  END.
  DO i = 1 TO NUM-ENTRIES(ttMFGroup.mfgroup_tabs):
    IF (3 + (i - 1) * 72) + 72 GT FRAME folderFrm:WIDTH-PIXELS THEN DO:
      lError = YES.
      RETURN.
    END. /* check if tab will fit */
    CREATE IMAGE tabImage[i] IN WIDGET-POOL "{&widgetPoolName}"
      ASSIGN
        FRAME = FRAME folderFrm:HANDLE
        X = 3 + (i - 1) * 72
        Y = 1
        WIDTH-PIXEL = 72
        HEIGHT-PIXEL = 24
        SENSITIVE = YES
      TRIGGERS:      
        ON MOUSE-SELECT-CLICK 
           PERSISTENT RUN labelTrigger IN THIS-PROCEDURE (i).
      END TRIGGERS.         
      CREATE TEXT tabLabel[i] IN WIDGET-POOL "{&widgetPoolName}"
        ASSIGN 
          FRAME = FRAME folderFrm:HANDLE
          X = tabImage[i]:X + 9
          Y = tabImage[i]:Y + 5
          WIDTH-PIXEL = tabImage[i]:WIDTH-PIXEL - 16
          HEIGHT-PIXEL = tabImage[i]:HEIGHT-PIXEL - 12
          FORMAT = "X(13)":U
          SENSITIVE = YES 
          FONT = ?
          BGCOLOR = 8
          SCREEN-VALUE = ENTRY(1, ENTRY(i,ttMFGroup.mfgroup_tabs) ,"|")
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
    DISABLE {&propertyCutCopy} WITH FRAME {&FRAME-NAME}.
  END.
  /* use up tab image for currently selected tab */
  ASSIGN
    mfgroupTab:SCREEN-VALUE = STRING(currentTab)
    tabImage[currentTab]:HEIGHT-PIXEL = 27
    ldummy = tabImage[currentTab]:LOAD-IMAGE("adeicon/ts-up72")
    ldummy = tabImage[currentTab]:MOVE-TO-TOP()
    ldummy = tabLabel[currentTab]:MOVE-TO-TOP()
    .
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
  RUN LockWindowUpdate (0,OUTPUT i).
&ELSE
  ACTIVE-WINDOW:DISABLE-REDRAW = FALSE.
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createWidgets W-Win 
PROCEDURE createWidgets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

&IF DEFINED(FWD-VERSION) EQ 0 &THEN
  RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
&ELSE
  ACTIVE-WINDOW:DISABLE-REDRAW = TRUE.
&ENDIF
  DELETE WIDGET-POOL "{&widgetPoolName}" NO-ERROR.
  CREATE WIDGET-POOL "{&widgetPoolName}" PERSISTENT.
  RUN createTabs.
  IF lError EQ NO THEN
  FOR EACH ttAttrb
      WHERE ttAttrb.attr_mfgroup EQ mfgroupList:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        AND ttAttrb.attr_tab EQ INTEGER(mfgroupTab:SCREEN-VALUE)
      :
    RUN dynamicWidget.
    IF lError THEN LEAVE.
  END.
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
  RUN LockWindowUpdate (0,OUTPUT i).
&ELSE
  ACTIVE-WINDOW:DISABLE-REDRAW = TRUE.
&ENDIF
  IF lError THEN DO:
    MESSAGE
      "Design Screen Resolution is too Small for this UDF Layout!"
    VIEW-AS ALERT-BOX ERROR.
    lError = NO.
  END. /* if error */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cutWidgets W-Win 
PROCEDURE cutWidgets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN clearClipboard.
  ASSIGN
    currentWidget = FRAME folderFrm:HANDLE
    currentWidget = currentWidget:FIRST-CHILD
    currentWidget = currentWidget:FIRST-CHILD
    .
  WIDGET-TREE:
  DO WHILE currentWidget NE ?:
    IF currentWidget:SELECTED THEN DO:
      RUN copyToClipboard.
      IF CAN-FIND(FIRST mfvalues WHERE mfvalues.mf_id = ttAttrb.attr_id) THEN DO:
        ldummy = NO.
        MESSAGE "Attribute Value Records Exist!" SKIP(1)
                "Cut Attribute Widget & Delete Attribute Value Records When Layout SAVED? (yes/no)" SKIP(1)
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Attribute Widget: " + ttAttrb.attr_label
                UPDATE ldummy.
        IF NOT ldummy THEN DO:
          DELETE wtClipboard.
          currentWidget = currentWidget:NEXT-SIBLING.
          NEXT WIDGET-TREE.
        END.
      END.
      RUN deleteMFValues.
      DELETE ttAttrb.
      IF NOT CAN-DO("RECTANGLE,TEXT,TOGGLE-BOX",currentWidget:TYPE) THEN DO:
        labelWidget = currentWidget:SIDE-LABEL-HANDLE.
        DELETE WIDGET labelWidget.
      END.
      dynWidget = currentWidget:NEXT-SIBLING.
      DELETE WIDGET currentWidget.
      currentWidget = dynWidget.
    END.
    ELSE
    currentWidget = currentWidget:NEXT-SIBLING.
  END.
  IF CAN-FIND(FIRST wtClipboard) THEN
  ENABLE btnPaste WITH FRAME {&FRAME-NAME}.
  ELSE
  DISABLE btnPaste WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteGroup W-Win 
PROCEDURE deleteGroup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    ldummy = NO.
    MESSAGE "Delete Group and Associated Attribute Widgets?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE " Delete Group "
        UPDATE ldummy.
    IF NOT ldummy THEN RETURN.
    FOR EACH ttAttrb
        WHERE ttAttrb.attr_mfgroup EQ mfgroupList:SCREEN-VALUE
        :
      IF CAN-FIND(FIRST mfvalues WHERE mfvalues.mf_id = ttAttrb.attr_id) THEN DO:
        ldummy = NO.
        MESSAGE "Attribute Value Records Exist!" SKIP(1)
                "Delete Group & Delete Attribute Value Records When Layout SAVED? (yes/no)"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Delete Group: " + ttAttrb.attr_mfgroup
            UPDATE ldummy.
        IF NOT ldummy THEN RETURN.
        LEAVE.
      END.
    END.
    FIND ttMFGroup
        WHERE ttMFGroup.mfgroup_data EQ mfgroupList:SCREEN-VALUE.
    DELETE ttMFGroup.
    FOR EACH ttAttrb
        WHERE ttAttrb.attr_mfgroup EQ mfgroupList:SCREEN-VALUE
        :
      RUN deleteMFValues.
      DELETE ttAttrb.
    END.
    ASSIGN
      ldummy = mfgroupList:DELETE(mfgroupList:SCREEN-VALUE)
      mfgroupList:SCREEN-VALUE = IF mfgroupList:NUM-ITEMS GT 1 THEN mfgroupList:ENTRY(2)
                                 ELSE mfgroupList:ENTRY(1)
      .
    APPLY "ENTRY" TO mfgroupList.
    APPLY "VALUE-CHANGED" TO mfgroupList.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteMFValues W-Win 
PROCEDURE deleteMFValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH mfvalues
      WHERE mfvalues.mf_id = ttAttrb.attr_id
      :
    FIND ttDeleteMFValues
        WHERE mfvalues_recid EQ RECID(mfvalues)
        NO-ERROR.
    IF AVAILABLE ttDeleteMFValues THEN NEXT.
    CREATE ttDeleteMFValues.
    ttDeleteMFValues.mfvalues_recid = RECID(mfvalues).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteTab W-Win 
PROCEDURE deleteTab :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    DO i = 1 TO tabLabels:NUM-ITEMS:
      IF tabLabels:IS-SELECTED(i) THEN
      LEAVE.
    END.
    FOR EACH ttAttrb
        WHERE ttAttrb.attr_mfgroup EQ mfgroupList:SCREEN-VALUE
          AND ttAttrb.attr_tab EQ i
        :
      IF CAN-FIND(FIRST mfvalues
                  WHERE mfvalues.mf_id = ttAttrb.attr_id) THEN DO:
        ldummy = NO.
        MESSAGE "Attribute Value Records Exist!" SKIP(1)
                "Remove Tab & Delete Attribute Value Records When Layout SAVED? (yes/no)"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Remove Tab: " + tabLabels:ENTRY(i)
            UPDATE ldummy.
        IF NOT ldummy THEN RETURN.
        LEAVE.
      END.
    END.
    FOR EACH ttAttrb
        WHERE ttAttrb.attr_mfgroup EQ mfgroupList:SCREEN-VALUE
          AND ttAttrb.attr_tab EQ i
        :
      RUN deleteMFValues.
      DELETE ttAttrb.
    END.
    FIND ttMFGroup
        WHERE ttMFGroup.mfgroup_data EQ mfgroupList:SCREEN-VALUE.
    ASSIGN
      ldummy = tabLabels:DELETE(i)
      tabLabels:SCREEN-VALUE = tabLabels:ENTRY(1)
      ldummy = mfgroupTab:DELETE(i)
      mfgroupTab:SCREEN-VALUE = mfgroupTab:ENTRY(1)
      ttMFGroup.mfgroup_tabs = tabLabels:LIST-ITEMS
      .
    RUN createWidgets.
    DISABLE {&propertyCutCopy} WITH FRAME {&FRAME-NAME}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deselectWidget W-Win 
PROCEDURE deselectWidget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF FRAME folderFrm:NUM-SELECTED-WIDGETS EQ 2 THEN
  ENABLE btnProperty WITH FRAME {&FRAME-NAME}.
  ELSE
  DISABLE btnProperty WITH FRAME {&FRAME-NAME}.

  IF FRAME folderFrm:NUM-SELECTED-WIDGETS EQ 1 THEN
  DISABLE {&propertyCutCopy} WITH FRAME {&FRAME-NAME}.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dynamicWidget W-Win 
PROCEDURE dynamicWidget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE horz-bar AS LOGICAL NO-UNDO.
  DEFINE VARIABLE vert-bar AS LOGICAL NO-UNDO.

&SCOPED-DEFINE defaults-code ~
          ASSIGN ~
            FRAME = FRAME folderFrm:HANDLE ~
            NAME = ttAttrb.attr_name ~
            X = ttAttrb.attr_x ~
            Y = ttAttrb.attr_y ~
            SENSITIVE = YES ~
            MOVABLE = YES ~
            SELECTABLE = YES ~
            RESIZABLE = YES ~
            PRIVATE-DATA = STRING(ROWID(ttAttrb)) + "|" + ~
                           mfgroupTab:SCREEN-VALUE IN FRAME {&FRAME-NAME} ~
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
            SCREEN-VALUE = ttAttrb.attr_default ~
            {&trigger-code}
&SCOPED-DEFINE editor-code ~
            {&widget-code} ~
            FONT = ? ~
            SCROLLBAR-VERTICAL = vert-bar ~
            SCROLLBAR-HORIZONTAL = horz-bar ~
            HELP = 'Enter ''' + ttAttrb.attr_label + ''' in EDITOR Field' ~
            SCREEN-VALUE = ttAttrb.attr_default ~
            {&trigger-code}
&SCOPED-DEFINE fill-in-code ~
            {&widget-code} ~
            FONT = ? ~
            DATA-TYPE = ttAttrb.attr_datatype ~
            FORMAT = ttAttrb.attr_settings ~
            HELP = 'Enter ''' + ttAttrb.attr_label + ''' in FILL-IN Field' ~
            SCREEN-VALUE = ttAttrb.attr_default ~
            {&trigger-code}
&SCOPED-DEFINE radio-set-code ~
            {&widget-code} ~
            FONT = ? ~
            HORIZONTAL = horz-bar ~
            RADIO-BUTTONS = ttAttrb.attr_values ~
            HELP = 'Select ''' + ttAttrb.attr_label + ''' from RADIO-SET Choices' ~
            SCREEN-VALUE = ttAttrb.attr_default ~
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
            SCREEN-VALUE = ttAttrb.attr_default ~
            {&trigger-code}
&SCOPED-DEFINE slider-code ~
            {&widget-code} ~
            FONT = ? ~
            HORIZONTAL = horz-bar ~
            MIN-VALUE = INT(ENTRY(1,ttAttrb.attr_values)) ~
            MAX-VALUE = INT(ENTRY(2,ttAttrb.attr_values)) ~
            HELP = 'Set ''' + ttAttrb.attr_label + ''' from SLIDER Range' ~
            SCREEN-VALUE = ttAttrb.attr_default ~
            {&trigger-code}
&SCOPED-DEFINE text-code ~
            {&defaults-code} ~
            FORMAT = 'X(80)' ~
            FONT = INTEGER(ttAttrb.attr_settings) ~
            HEIGHT-PIXELS = ttAttrb.attr_height ~
            HELP = '''' + ttAttrb.attr_label + ''' is a TEXT Widget' ~
            SCREEN-VALUE = ttAttrb.attr_label ~
            {&trigger-code}
&SCOPED-DEFINE toggle-box-code ~
            {&defaults-code} ~
            FONT = ? ~
            LABEL = ttAttrb.attr_label ~
            HEIGHT-PIXELS = ttAttrb.attr_height ~
            HELP = 'Set ''' + ttAttrb.attr_label + ''' TOGGLE-BOX On/Off' ~
            SCREEN-VALUE = ttAttrb.attr_default ~
            {&trigger-code}
  ASSIGN
    horz-bar = IF NUM-ENTRIES(ttAttrb.attr_settings) NE 0 AND
                  ENTRY(1,ttAttrb.attr_settings) = "yes" THEN YES ELSE NO
    vert-bar = IF NUM-ENTRIES(ttAttrb.attr_settings) GT 1 AND
                  ENTRY(2,ttAttrb.attr_settings) = "yes" THEN YES ELSE NO
    .
  IF ttAttrb.attr_y + ttAttrb.attr_height GT FRAME folderFrm:HEIGHT-PIXELS OR
     ttAttrb.attr_x + ttAttrb.attr_width GT FRAME folderFrm:WIDTH-PIXELS THEN DO:
    lError = YES.
    RETURN.
  END. /* check if can fit in frame */
  RUN createLabelWidget (ttAttrb.attr_type,ttAttrb.attr_label,ttAttrb.attr_x,ttAttrb.attr_y).
  DO ON ERROR UNDO, RETURN "CREATE-ERROR":
    CASE ttAttrb.attr_type:
&SCOPED-DEFINE widget-type COMBO-BOX
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "{&widgetPoolName}"
        {&{&widget-type}-code}
      END.
&SCOPED-DEFINE widget-type EDITOR
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "{&widgetPoolName}"
        {&{&widget-type}-code}
      END.
&SCOPED-DEFINE widget-type FILL-IN
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "{&widgetPoolName}"
        {&{&widget-type}-code}
      END.
&SCOPED-DEFINE widget-type RADIO-SET
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "{&widgetPoolName}"
        {&{&widget-type}-code}
      END.
&SCOPED-DEFINE widget-type RECTANGLE
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "{&widgetPoolName}"
        {&{&widget-type}-code}
      END.
&SCOPED-DEFINE widget-type SELECTION-LIST
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "{&widgetPoolName}"
        {&{&widget-type}-code}
      END.
&SCOPED-DEFINE widget-type SLIDER
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "{&widgetPoolName}"
        {&{&widget-type}-code}
      END.
&SCOPED-DEFINE widget-type TEXT
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "{&widgetPoolName}"
        {&{&widget-type}-code}
      END.
&SCOPED-DEFINE widget-type TOGGLE-BOX
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "{&widgetPoolName}"
        {&{&widget-type}-code}
      END.
    END CASE.
    IF VALID-HANDLE(dynWidget) THEN
    ASSIGN
      ldummy = dynWidget:MOVE-TO-TOP()
      dynWidget:HIDDEN = NO
      .
  END.
  RETURN.

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
  DISPLAY mfgroupList mfgroupTab tabLabels widgetLabel tabCBLabel pointerLabel 
          tabNavLabel combo-BoxLabel editorLabel fill-InLabel radio-SetLabel 
          rectangleLabel selection-ListLabel sliderLabel textLabel 
          toggle-BoxLabel gapFieldLabel prgrmsLabel 
      WITH FRAME fMiscFlds IN WINDOW W-Win.
  ENABLE btnMFPrgrms tabLabelsRect RECT-5 RECT-4 widgetRect tabNavRect RECT-2 
         RECT-3 RECT-8 portRect prgrmsRect mfgroupList mfgroupTab btnCombo-Box 
         btnAddTab tabLabels btnEditor btnFill-In btnRadio-Set btnRectangle 
         btnSelection-List btnSlider btnText btnToggle-Box btnCopy btnCut 
         btnDeleteTab btnDownTab btnExit btnExport btnImport btnNextTab 
         btnPaste btnPrevTab btnProperty btnRenameTab btnUpTab btnAddGroup 
         btnCopyGroup btnDeleteGroup btnRenameGroup btnRestore btnSave 
         btnTabOrder btnTest 
      WITH FRAME fMiscFlds IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-fMiscFlds}
  ENABLE Rect-Top Rect-Left Rect-Right Rect-Bottom 
      WITH FRAME folderFrm IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-folderFrm}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportLayouts W-Win 
PROCEDURE exportLayouts :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    MESSAGE "Export Functionality Not Yet Enabled" VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE importLayouts W-Win 
PROCEDURE importLayouts :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    MESSAGE "Import Functionality Not Yet Enabled" VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE labelTrigger W-Win 
PROCEDURE labelTrigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER tabno AS INTEGER NO-UNDO.

  IF tabno EQ INTEGER(mfgroupTab:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN
  RETURN.
  mfgroupTab:SCREEN-VALUE = STRING(tabno).
  RUN createWidgets.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadImages W-Win 
PROCEDURE loadImages :
/*------------------------------------------------------------------------------
  Purpose:     Load Button Graphics
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      ldummy = btnPointer:LOAD-IMAGE-UP("Graphics/wp_up",29,253,28,28)
      ldummy = btnCombo-Box:LOAD-IMAGE-UP("Graphics/wp_up",1,141,28,28)
      ldummy = btnEditor:LOAD-IMAGE-UP("Graphics/wp_up",29,113,28,28)
      ldummy = btnFill-In:LOAD-IMAGE-UP("Graphics/wp_up",29,141,28,28)
      ldummy = btnRadio-Set:LOAD-IMAGE-UP("Graphics/wp_up",1,57,28,28)
      ldummy = btnSelection-List:LOAD-IMAGE-UP("Graphics/wp_up",1,113,28,28)
      ldummy = btnSlider:LOAD-IMAGE-UP("Graphics/wp_up",1,85,28,28)
      ldummy = btnRectangle:LOAD-IMAGE-UP("Graphics/wp_up",1,29,28,28)
      ldummy = btnText:LOAD-IMAGE-UP("Graphics/wp_up",1,169,28,28)
      ldummy = btnToggle-Box:LOAD-IMAGE-UP("Graphics/wp_up",29,57,28,28)
      ldummy = btnPointer:LOAD-IMAGE-INSENSITIVE("Graphics/wp_down",29,253,28,28)
      ldummy = btnCombo-Box:LOAD-IMAGE-INSENSITIVE("Graphics/wp_down",1,141,28,28)
      ldummy = btnEditor:LOAD-IMAGE-INSENSITIVE("Graphics/wp_down",29,113,28,28)
      ldummy = btnFill-In:LOAD-IMAGE-INSENSITIVE("Graphics/wp_down",29,141,28,28)
      ldummy = btnRadio-Set:LOAD-IMAGE-INSENSITIVE("Graphics/wp_down",1,57,28,28)
      ldummy = btnSelection-List:LOAD-IMAGE-INSENSITIVE("Graphics/wp_down",1,113,28,28)
      ldummy = btnSlider:LOAD-IMAGE-INSENSITIVE("Graphics/wp_down",1,85,28,28)
      ldummy = btnRectangle:LOAD-IMAGE-INSENSITIVE("Graphics/wp_down",1,29,28,28)
      ldummy = btnText:LOAD-IMAGE-INSENSITIVE("Graphics/wp_down",1,169,28,28)
      ldummy = btnToggle-Box:LOAD-IMAGE-INSENSITIVE("Graphics/wp_down",29,57,28,28)
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadWidgetData W-Win 
PROCEDURE loadWidgetData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE groupList AS CHARACTER NO-UNDO.
  DEFINE VARIABLE sizeList AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  IF NOT CAN-FIND(FIRST mfgroup) THEN RETURN.

  EMPTY TEMP-TABLE ttMFPrgrms.

  FOR EACH mfgroup NO-LOCK:
    ASSIGN
      groupList = groupList
                + ENTRY(1,mfgroup.mfgroup_data,"|") + ","
      sizeList  = sizeList
                + STRING(mfgroup.udfWidth) + "x"
                + STRING(mfgroup.udfHeight) + ","
                .
    DO i = 2 TO NUM-ENTRIES(mfgroup.mfgroup_data,"|"):
      FIND FIRST prgrms NO-LOCK
           WHERE prgrms.prgmname EQ ENTRY(i,mfgroup.mfgroup_data,"|")
           NO-ERROR.
      IF NOT AVAILABLE prgrms THEN NEXT.
      CREATE ttMFPrgrms.
      ASSIGN
        ttMFPrgrms.mfgroup  = ENTRY(1,mfgroup.mfgroup_data,"|")
        ttMFPrgrms.prgmname = prgrms.prgmname
        ttMFPrgrms.prgtitle = prgrms.prgtitle
        .
    END. /* do i */
  END. /* each mfgroup */
  ASSIGN
    groupList = TRIM(groupList,",")
    sizeList = TRIM(sizeList,",")
    .

  OUTPUT TO VALUE("users/" + USERID("NOSWEAT") + "/mfgroup.dat").
  EXPORT groupList.
  EXPORT sizeList.
  FOR EACH mfgroup NO-LOCK:
    EXPORT mfgroup.mfgroup_tabs.
  END.
  OUTPUT CLOSE.

  OUTPUT TO VALUE("users/" + USERID("NOSWEAT") + "/mfdata.dat").
  FOR EACH mfdata NO-LOCK:
    PUT UNFORMATTED mfdata.miscflds_data SKIP.
  END.
  OUTPUT CLOSE.

  EMPTY TEMP-TABLE ttMFGroup.
  EMPTY TEMP-TABLE ttAttrb.
  EMPTY TEMP-TABLE ttDeleteMFValues.

  INPUT FROM VALUE("users/" + USERID("NOSWEAT") + "/mfdata.dat") NO-ECHO.
  REPEAT:
    CREATE ttAttrb.
    IMPORT ttAttrb.
  END. /* repeat */
  INPUT CLOSE.
  IF ttAttrb.attr_type EQ "" THEN DELETE ttAttrb.

  INPUT FROM VALUE("users/" + USERID("NOSWEAT") + "/mfgroup.dat").
  IMPORT cdummy.
  IMPORT sizeList.
  DO i = 1 TO NUM-ENTRIES(cdummy):
    IMPORT currentLabel.
    CREATE ttMFGroup.
    ASSIGN
      ttMFGroup.mfgroup_data = ENTRY(i,cdummy)
      ttMFGroup.mfgroup_tabs = currentLabel
      ttMFGroup.udfWidth = INTEGER(ENTRY(1,ENTRY(i,sizeList),"x"))
      ttMFGroup.udfHeight = INTEGER(ENTRY(2,ENTRY(i,sizeList),"x"))
      .
  END.
  INPUT CLOSE.

  FIND LAST ttAttrb USE-INDEX si-attrb NO-ERROR.
  DO WITH FRAME {&FRAME-NAME}:
    lastID = IF AVAILABLE ttAttrb THEN INTEGER(ttAttrb.attr_id) ELSE 0 NO-ERROR.
    ASSIGN
      mfgroupList:LIST-ITEMS = IF cdummy NE "" THEN cdummy ELSE " "
      mfgroupList:SCREEN-VALUE =
          IF mfgroupList:NUM-ITEMS GT 1 THEN mfgroupList:ENTRY(2)
          ELSE mfgroupList:ENTRY(1)
      .
  END.
  RUN createWidgets.
  APPLY "ENTRY" TO mfgroupList IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveObjects W-Win 
PROCEDURE moveObjects :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE offSet AS INTEGER NO-UNDO.

  ASSIGN
    offSet = FRAME folderFrm:WIDTH-PIXELS
    FRAME folderFrm:WIDTH-PIXELS = FRAME {&FRAME-NAME}:WIDTH-PIXELS - 291
    FRAME folderFrm:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 80
    offSet = FRAME folderFrm:WIDTH-PIXELS - offSet
    tabNavRect:X = tabNavRect:X + offSet
    btnPrevTab:X = btnPrevTab:X + offSet
    tabCBLabel:X = tabCBLabel:X + offSet
    mfgroupTab:X = mfgroupTab:X + offSet
    btnNextTab:X = btnNextTab:X + offSet
    tabLabelsRect:X = tabLabelsRect:X + offSet
    tabNavLabel:X = tabNavLabel:X + offSet
    btnAddTab:X = btnAddTab:X + offSet
    btnRenameTab:X = btnRenameTab:X + offSet
    btnDeleteTab:X = btnDeleteTab:X + offSet
    btnDownTab:X = btnDownTab:X + offSet
    btnUpTab:X = btnUpTab:X + offSet
    tabLabels:X = tabLabels:X + offSet
    tabField:X = tabField:X + offSet
    Rect-Main:WIDTH-PIXELS IN FRAME folderFrm = FRAME folderFrm:WIDTH-PIXELS - 6
    Rect-Main:HEIGHT-PIXELS = FRAME folderFrm:HEIGHT-PIXELS - 23
    Rect-Top:WIDTH-PIXELS = Rect-Main:WIDTH-PIXELS - 2
    Rect-Left:HEIGHT-PIXELS = Rect-Main:HEIGHT-PIXELS - 5
    Rect-Bottom:WIDTH-PIXELS = Rect-Main:WIDTH-PIXELS - 2
    Rect-Bottom:Y = Rect-Main:HEIGHT-PIXELS + 17
    Rect-Right:HEIGHT-PIXELS = Rect-Main:HEIGHT-PIXELS - 8
    Rect-Right:X = Rect-Main:WIDTH-PIXELS - 1
    prgrmsRect:X = prgrmsRect:X + offSet
    btnMFPrgrms:X = btnMFPrgrms:X + offSet
    prgrmsLabel:X = prgrmsLabel:X + offSet
    udfHeightSize:X = udfHeightSize:X + offSet
    udfWidthSize:X = udfWidthSize:X + offSet
    udfHeightSize:MAX-VALUE = FRAME folderFrm:HEIGHT-PIXELS
    udfWidthSize:MAX-VALUE = FRAME folderFrm:WIDTH-PIXELS
    .
  IF udfHeightSize EQ 0 THEN
  udfHeightSize = udfHeightSize:MAX-VALUE.
  IF udfWidthSize EQ 0 THEN
  udfWidthSize = udfWidthSize:MAX-VALUE.

  ENABLE udfWidthSize udfHeightSize WITH FRAME {&FRAME-NAME}.
  DISPLAY udfWidthSize udfHeightSize WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveTab W-Win 
PROCEDURE moveTab :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER moveTab AS CHARACTER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    FIND ttMFGroup
        WHERE ttMFGroup.mfgroup_data EQ mfgroupList:SCREEN-VALUE.
    DO i = 1 TO tabLabels:NUM-ITEMS:
      IF tabLabels:IS-SELECTED(i) THEN LEAVE.
    END.
    ASSIGN
      j = IF moveTab EQ "Down" THEN i + 1 ELSE i - 1
      ldummy = IF moveTab EQ "Down" THEN tabLabels:INSERT(tabLabels:SCREEN-VALUE,j + 1)
                                    ELSE tabLabels:INSERT(tabLabels:SCREEN-VALUE,j)
      ldummy = IF moveTab EQ "Down" THEN tabLabels:DELETE(i)
                                    ELSE tabLabels:DELETE(i + 1)
      ttMFGroup.mfgroup_tabs = tabLabels:LIST-ITEMS
      tabLabels:SCREEN-VALUE = tabLabels:ENTRY(j)
      .
    FOR EACH ttAttrb
        WHERE ttAttrb.attr_mfgroup EQ mfgroupList:SCREEN-VALUE
          AND ttAttrb.attr_tab EQ i
        :
      ttAttrb.attr_tab = 0.
    END.
    FOR EACH ttAttrb
        WHERE ttAttrb.attr_mfgroup EQ mfgroupList:SCREEN-VALUE
          AND ttAttrb.attr_tab EQ j
        :
      ttAttrb.attr_tab = i.
    END.
    FOR EACH ttAttrb
        WHERE ttAttrb.attr_mfgroup EQ mfgroupList:SCREEN-VALUE
          AND ttAttrb.attr_tab EQ 0
        :
      ttAttrb.attr_tab = j.
    END.
    savePrompt = YES.
  END.
  APPLY "VALUE-CHANGED" TO tabLabels IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveWidget W-Win 
PROCEDURE moveWidget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
    currentWidget = FRAME folderFrm:HANDLE
    currentWidget = currentWidget:FIRST-CHILD
    currentWidget = currentWidget:FIRST-CHILD
    .
  DO WHILE currentWidget NE ?:
    IF currentWidget:SELECTED THEN DO:
      RUN updateAttrb.
      IF NOT CAN-DO("RECTANGLE,TEXT,TOGGLE-BOX",currentWidget:TYPE) THEN DO:
        labelWidget = currentWidget:SIDE-LABEL-HANDLE.
        DELETE WIDGET labelWidget.
      END.
      saveWidget = currentWidget:NEXT-SIBLING.
      DELETE WIDGET currentWidget.
      RUN dynamicWidget.
      currentWidget = saveWidget.
    END.
    ELSE currentWidget = currentWidget:NEXT-SIBLING.
  END.
  DISABLE {&propertyCutCopy} WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newWidget W-Win 
PROCEDURE newWidget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  CREATE ttAttrb.
  RUN setAttrID.
  ASSIGN
    ttAttrb.attr_mfgroup = mfgroupList:SCREEN-VALUE IN FRAME {&FRAME-NAME}
    ttAttrb.attr_tab = INTEGER(mfgroupTab:SCREEN-VALUE IN FRAME {&FRAME-NAME})
    ttAttrb.attr_type = currentLabel
    ttAttrb.attr_label = IF currentLabel EQ "RECTANGLE" THEN ""
                         ELSE "NEW " + currentLabel
    ttAttrb.attr_values = IF currentLabel EQ "RADIO-SET" THEN "Empty,0"
                     ELSE IF currentLabel EQ "RECTANGLE" THEN ""
                     ELSE IF currentLabel EQ "SLIDER" THEN "0,100"
                     ELSE IF currentLabel EQ "TEXT" THEN ""
                     ELSE "Empty"
    ttAttrb.attr_default = IF CAN-DO("RADIO-SET,SLIDER",currentLabel) THEN "0"
                      ELSE IF currentLabel EQ "RECTANGLE" THEN ""
                      ELSE IF currentLabel EQ "TEXT" THEN ""
                      ELSE IF currentLabel EQ "TOGGLE-BOX" THEN "no"
                      ELSE "Empty"
    ttAttrb.attr_x = LAST-EVENT:X
    ttAttrb.attr_y = LAST-EVENT:Y
    ttAttrb.attr_width = IF currentLabel EQ "SLIDER" THEN 70 ELSE 105
    ttAttrb.attr_height = IF currentLabel EQ "SLIDER" THEN 31 ELSE 21
    ttAttrb.attr_settings = IF currentLabel EQ "EDITOR" THEN "no,yes"
                       ELSE IF currentLabel EQ "FILL-IN" THEN "X(256)"
                       ELSE IF currentLabel EQ "COMBO-BOX" THEN "X(256)"
                       ELSE IF currentLabel EQ "RADIO-SET" THEN "yes"
                       ELSE IF currentLabel EQ "SLIDER" THEN "yes"
                       ELSE IF currentLabel EQ "TEXT" THEN "4"
                       ELSE ""
    ttAttrb.attr_datatype = IF currentLabel EQ "SLIDER" THEN "integer"
                       ELSE IF currentLabel EQ "TOGGLE-BOX" THEN "logical"
                       ELSE "character"
    ttAttrb.attr_enabled = NOT CAN-DO("RECTANGLE,TEXT",currentLabel)
    .
  RUN dynamicWidget.
  IF RETURN-VALUE EQ "CREATE-ERROR" THEN DELETE ttAttrb.
  ELSE DO:
    dynWidget:SELECTED = YES.
    ENABLE {&propertyCutCopy} WITH FRAME {&FRAME-NAME}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pasteFromClipboard W-Win 
PROCEDURE pasteFromClipboard :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH wtClipboard:
    CREATE ttAttrb.
    RUN setAttrID.
    ASSIGN
      ttAttrb.attr_mfgroup = mfgroupList:SCREEN-VALUE IN FRAME {&FRAME-NAME}
      ttAttrb.attr_tab = INTEGER(mfgroupTab:SCREEN-VALUE IN FRAME {&FRAME-NAME})
      ttAttrb.attr_type = wtClipboard.attr_type
      ttAttrb.attr_label = wtClipboard.attr_label
      ttAttrb.attr_name = wtClipboard.attr_name
      ttAttrb.attr_values = wtClipboard.attr_values
      ttAttrb.attr_default = wtClipboard.attr_default
      ttAttrb.attr_x = wtClipboard.attr_x
      ttAttrb.attr_y = wtClipboard.attr_y
      ttAttrb.attr_height = wtClipboard.attr_height
      ttAttrb.attr_width = wtClipboard.attr_width
      ttAttrb.attr_settings = wtClipboard.attr_settings
      ttAttrb.attr_datatype = wtClipboard.attr_datatype
      .
    RUN dynamicWidget.
    dynWidget:SELECTED = YES.
    ENABLE btnCut btnCopy WITH FRAME {&FRAME-NAME}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE propertySheet W-Win 
PROCEDURE propertySheet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
    currentWidget = FRAME folderFrm:HANDLE
    currentWidget = currentWidget:FIRST-CHILD
    currentWidget = currentWidget:FIRST-CHILD
    .
  DO WHILE currentWidget NE ?:
    IF currentWidget:SELECTED THEN
    DO WITH FRAME {&FRAME-NAME}:
      RUN UDF/mfdialog.w (ENTRY(1,currentWidget:PRIVATE-DATA,"|"),
                          INPUT-OUTPUT savePrompt).
      FIND ttAttrb
          WHERE ROWID(ttAttrb) EQ TO-ROWID(ENTRY(1,currentWidget:PRIVATE-DATA,"|")).
      IF NOT CAN-DO("RECTANGLE,TEXT,TOGGLE-BOX",currentWidget:TYPE) THEN DO:
        labelWidget = currentWidget:SIDE-LABEL-HANDLE.
        DELETE WIDGET labelWidget.
      END.
      saveWidget = currentWidget:NEXT-SIBLING.
      DELETE WIDGET currentWidget.
      RUN dynamicWidget.
      dynWidget:SELECTED = YES.
      RETURN.
    END.
    ELSE
    currentWidget = currentWidget:NEXT-SIBLING.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE renameGroup W-Win 
PROCEDURE renameGroup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE save_labels AS CHARACTER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF NOT ldummy THEN
    MESSAGE "Rename Group" mfgroupList:SCREEN-VALUE "to" cdummy "?" VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE " Change Group " UPDATE ldummy.
    ELSE
    MESSAGE "Group" cdummy "Already Exists, Merge" mfgroupList:SCREEN-VALUE "with" cdummy "?" VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE " Change Group " UPDATE ldummy.
    IF ldummy THEN DO:
      FOR EACH ttAttrb
          WHERE ttAttrb.attr_mfgroup EQ mfgroupList:SCREEN-VALUE
          :
        ttAttrb.attr_mfgroup = cdummy.
      END.
      DO i = mfgroupList:NUM-ITEMS TO 1 BY -1:
        IF mfgroupList:ENTRY(i) = mfgroupField:SCREEN-VALUE THEN
        currentLabel = "yes".
        IF mfgroupList:ENTRY(i) = mfgroupList:SCREEN-VALUE THEN DO:
          FIND ttMFGroup
              WHERE ttMFGroup.mfgroup_data EQ mfgroupList:ENTRY(i).
          ASSIGN
            save_labels = ttMFGroup.mfgroup_tabs
            ldummy = mfgroupList:DELETE(i)
            .
          DELETE mfgroup.
        END.
        IF mfgroupList:ENTRY(i) = mfgroupField:SCREEN-VALUE THEN
        currentLabel = "yes".
      END.
      IF currentLabel NE "yes" THEN DO:
        CREATE ttMFGroup.
        ASSIGN
          ttMFGroup.mfgroup_data = mfgroupField:SCREEN-VALUE
          ttMFGroup.mfgroup_tabs = save_labels
          ldummy = mfgroupList:ADD-LAST(mfgroupField:SCREEN-VALUE)
          currentLabel = ""
          .
      END.
    END.
    ELSE cdummy = mfgroupList:SCREEN-VALUE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE renameTab W-Win 
PROCEDURE renameTab :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    FIND ttMFGroup
        WHERE ttMFGroup.mfgroup_data EQ mfgroupList:SCREEN-VALUE.
    ASSIGN
      tabField:SCREEN-VALUE = tabField:SCREEN-VALUE + "|" + tabOrder
      tabField
      ldummy = tabLabels:REPLACE(tabField:SCREEN-VALUE,tabLabels:SCREEN-VALUE)
      tabLabels:SCREEN-VALUE = tabField:SCREEN-VALUE
      ttMFGroup.mfgroup_tabs = tabLabels:LIST-ITEMS
      .
    RUN createTabs.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resizeWidget W-Win 
PROCEDURE resizeWidget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER thisWidget AS WIDGET-HANDLE NO-UNDO.

  currentWidget = thisWidget:HANDLE.
  RUN updateAttrb.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveLayout W-Win 
PROCEDURE saveLayout :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE textstring AS CHARACTER NO-UNDO.

  IF CAN-FIND(FIRST ttDeleteMFValues) THEN DO:
    ldummy = NO.
    MESSAGE "Attribute Value Records are marked for DELETION, Continue? (yes/no)"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ldummy.
    IF ldummy THEN
    FOR EACH ttDeleteMFValues:
      FIND FIRST mfvalues
           WHERE RECID(mfvalues) EQ ttDeleteMFValues.mfvalues_recid.
      DELETE mfvalues.
      DELETE ttDeleteMFValues.
    END. /* each ttdeletemfvalues */
  END. /* if can-find */

  OUTPUT TO VALUE("users/" + USERID("NOSWEAT") + "/mfgroup.dat").
  EXPORT mfgroupList:LIST-ITEMS IN FRAME {&FRAME-NAME}.
  DO i = 1 TO mfgroupList:NUM-ITEMS:
    FIND FIRST ttMFGroup
         WHERE ttMFGroup.mfgroup_data EQ ENTRY(i,mfgroupList:LIST-ITEMS)
         NO-ERROR.
    IF AVAILABLE ttMFGroup THEN
    EXPORT ttMFGroup.mfgroup_tabs.
    ELSE
    EXPORT " ".
  END. /* do i */
  OUTPUT CLOSE.

  FOR EACH mfgroup:
      DELETE mfgroup.
  END. /* each mfgroup */
  FOR EACH ttMFGroup:
    textstring = "".
    FOR EACH ttMFPrgrms
        WHERE ttMFPrgrms.mfgroup EQ ttMFGroup.mfgroup_data
        :
        textstring = textstring + "|" + ttMFPrgrms.prgmname.
    END.
    CREATE mfgroup.
    BUFFER-COPY ttMFGroup TO mfgroup.
    mfgroup.mfgroup_data = mfgroup.mfgroup_data + textstring.
  END. /* each ttmfprgrms */

  OUTPUT TO VALUE("users/" + USERID("NOSWEAT") + "/miscflds.dat").
  FOR EACH ttAttrb:
    EXPORT ttAttrb.attr_mfgroup.
    EXPORT ttAttrb.
  END. /* each ttattrb */
  OUTPUT CLOSE.
  
  FOR EACH mfdata:
      DELETE mfdata.
  END. /* each mfdata */
  INPUT FROM VALUE("users/" + USERID("NOSWEAT") + "/miscflds.dat").
  REPEAT:
    IMPORT textstring.
    CREATE mfdata.
    mfdata.mfgroup_data = textstring.
    IMPORT UNFORMATTED mfdata.miscflds_data.
  END. /* repeat */
  INPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selectNewWidgetType W-Win 
PROCEDURE selectNewWidgetType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER newWidgetType AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER mousePointer AS CHARACTER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      pointerLabel:BGCOLOR = ?
      combo-BoxLabel:BGCOLOR = ?
      editorLabel:BGCOLOR = ?
      fill-InLabel:BGCOLOR = ?
      radio-SetLabel:BGCOLOR = ?
      rectangleLabel:BGCOLOR = ?
      selection-ListLabel:BGCOLOR = ?
      sliderLabel:BGCOLOR = ?
      textLabel:BGCOLOR = ?
      toggle-BoxLabel:BGCOLOR = ?
      .
    ENABLE {&notFoundIn234} {&widgetButtons}.

    CASE newWidgetType:
      WHEN "" THEN DO:
        pointerLabel:BGCOLOR = 8.
        DISABLE btnPointer.
      END.
&SCOPED-DEFINE widgettype Combo-Box
      WHEN "{&widgettype}" THEN DO:
        {&widgettype}Label:BGCOLOR = 8.
        DISABLE btn{&widgettype}.
      END.
&SCOPED-DEFINE widgettype Editor
      WHEN "{&widgettype}" THEN DO:
        {&widgettype}Label:BGCOLOR = 8.
        DISABLE btn{&widgettype}.
      END.
&SCOPED-DEFINE widgettype Fill-In
      WHEN "{&widgettype}" THEN DO:
        {&widgettype}Label:BGCOLOR = 8.
        DISABLE btn{&widgettype}.
      END.
&SCOPED-DEFINE widgettype Radio-Set
      WHEN "{&widgettype}" THEN DO:
        {&widgettype}Label:BGCOLOR = 8.
        DISABLE btn{&widgettype}.
      END.
&SCOPED-DEFINE widgettype Rectangle
      WHEN "{&widgettype}" THEN DO:
        {&widgettype}Label:BGCOLOR = 8.
        DISABLE btn{&widgettype}.
      END.
&SCOPED-DEFINE widgettype Selection-List
      WHEN "{&widgettype}" THEN DO:
        {&widgettype}Label:BGCOLOR = 8.
        DISABLE btn{&widgettype}.
      END.
&SCOPED-DEFINE widgettype Slider
      WHEN "{&widgettype}" THEN DO:
        {&widgettype}Label:BGCOLOR = 8.
        DISABLE btn{&widgettype}.
      END.
&SCOPED-DEFINE widgettype Text
      WHEN "{&widgettype}" THEN DO:
        {&widgettype}Label:BGCOLOR = 8.
        DISABLE btn{&widgettype}.
      END.
&SCOPED-DEFINE widgettype Toggle-Box
      WHEN "{&widgettype}" THEN DO:
        {&widgettype}Label:BGCOLOR = 8.
        DISABLE btn{&widgettype}.
      END.
    END CASE.
  END.

  ASSIGN
    ldummy = FRAME folderFrm:LOAD-MOUSE-POINTER(mousePointer)
    currentLabel = newWidgetType
    savePrompt = YES
    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selectWidget W-Win 
PROCEDURE selectWidget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF FRAME folderFrm:NUM-SELECTED-WIDGETS EQ 0 THEN
  ENABLE {&propertyCutCopy} WITH FRAME {&FRAME-NAME}.
  ELSE
  DISABLE btnProperty WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus W-Win 
PROCEDURE Set-Focus :
/*------------------------------------------------------------------------------
  Purpose:     Set Focus
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/setfocus.i mfgroupList}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setAttrID W-Win 
PROCEDURE setAttrID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO i = 1 TO lastID:
     IF NOT CAN-FIND(ttAttrb
                     WHERE ttAttrb.attr_id EQ STRING(i,'99999')) THEN
     LEAVE.
  END.
  IF i GT lastID THEN lastID = i.
  ttAttrb.attr_id = STRING(i,'99999').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setAttrID-buffer W-Win 
PROCEDURE setAttrID-buffer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO i = 1 TO lastID:
     IF NOT CAN-FIND(ttAttrb
                     WHERE ttAttrb.attr_id EQ STRING(i,'99999')) THEN
     LEAVE.
  END.
  IF i GT lastID THEN lastID = i.
  bAttrb.attr_id = STRING(i,'99999').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setLayoutSize W-Win 
PROCEDURE setLayoutSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    IF udfWidthSize LT udfWidthSize:MIN-VALUE THEN
    udfWidthSize = udfWidthSize:MAX-VALUE.
    IF udfHeightSize LT udfHeightSize:MIN-VALUE THEN
    udfHeightSize = udfHeightSize:MAX-VALUE.
    DISPLAY udfWidthSize udfHeightSize.
  END. /* with frame */
  
  DO WITH FRAME folderFrm:
    ASSIGN
      Rect-Main:HIDDEN = YES
      Rect-Top:HIDDEN = YES
      Rect-Left:HIDDEN = YES
      Rect-Right:HIDDEN = YES
      Rect-Bottom:HIDDEN = YES
      FRAME folderFrm:HEIGHT-PIXELS = udfHeightSize
      FRAME folderFrm:VIRTUAL-HEIGHT-PIXELS = udfHeightSize
      FRAME folderFrm:WIDTH-PIXELS = udfWidthSize
      FRAME folderFrm:VIRTUAL-WIDTH-PIXELS = udfWidthSize
      Rect-Main:HEIGHT-PIXELS = udfHeightSize - 23
      Rect-Main:WIDTH-PIXELS = udfWidthSize - 6
      Rect-Left:HEIGHT-PIXELS = Rect-Main:HEIGHT-PIXELS - 5
      Rect-Right:HEIGHT-PIXELS = Rect-Main:HEIGHT-PIXELS - 8
      Rect-Right:X = Rect-Main:WIDTH-PIXELS - 1
      Rect-Top:WIDTH-PIXELS = Rect-Main:WIDTH-PIXELS - 2
      Rect-Bottom:WIDTH-PIXELS = Rect-Main:WIDTH-PIXELS - 2
      Rect-Bottom:Y = Rect-Main:HEIGHT-PIXELS + 17
      Rect-Main:HIDDEN = NO
      Rect-Top:HIDDEN = NO
      Rect-Left:HIDDEN = NO
      Rect-Right:HIDDEN = NO
      Rect-Bottom:HIDDEN = NO
      ttMFGroup.udfWidth = udfWidthSize
      ttMFGroup.udfHeight = udfHeightSize
      .
  END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tabOrder W-Win 
PROCEDURE tabOrder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  tabOrder = IF NUM-ENTRIES(tabLabels:SCREEN-VALUE IN FRAME {&FRAME-NAME},"|") GT 1 THEN
             ENTRY(2,tabLabels:SCREEN-VALUE,"|") ELSE 'Default'.
  RUN UDF/mftaborder.w (FRAME folderFrm:HANDLE,
                        ENTRY(1,tabLabels:SCREEN-VALUE,"|"),
                        INPUT-OUTPUT tabOrder,
                        INPUT-OUTPUT savePrompt).
  tabLabels:REPLACE(ENTRY(1,tabLabels:SCREEN-VALUE,"|") + "|" + tabOrder,
                    tabLabels:SCREEN-VALUE).
  FIND ttMFGroup
      WHERE ttMFGroup.mfgroup_data EQ mfgroupList:SCREEN-VALUE.
  ttMFGroup.mfgroup_tabs = tabLabels:LIST-ITEMS.
  RUN createWidgets.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateAttrb W-Win 
PROCEDURE updateAttrb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND ttAttrb
      WHERE ROWID(ttAttrb) EQ TO-ROWID(ENTRY(1,currentWidget:PRIVATE-DATA,"|"))
      NO-ERROR.
  IF NOT AVAILABLE ttAttrb THEN DO:
    CREATE ttAttrb.
    RUN setAttrID.
  END.
  ASSIGN
    ttAttrb.attr_mfgroup = mfgroupList:SCREEN-VALUE IN FRAME {&FRAME-NAME}
    ttAttrb.attr_tab = INTEGER(ENTRY(2,currentWidget:PRIVATE-DATA,"|"))
    ttAttrb.attr_type = currentWidget:TYPE
    ttAttrb.attr_label = IF currentWidget:TYPE EQ "RECTANGLE" THEN ""
      ELSE IF currentWidget:TYPE EQ "TOGGLE-BOX" THEN currentWidget:LABEL
      ELSE IF currentWidget:TYPE EQ "TEXT" THEN currentWidget:SCREEN-VALUE
      ELSE SUBSTR(currentWidget:LABEL,1,LENGTH(currentWidget:LABEL) - 1)
    ttAttrb.attr_values =
           IF CAN-DO("COMBO-BOX,SELECTION-LIST",currentWidget:TYPE) THEN currentWidget:LIST-ITEMS
      ELSE IF currentWidget:TYPE EQ "RADIO-SET" THEN currentWidget:RADIO-BUTTONS
      ELSE IF currentWidget:TYPE EQ "SLIDER" THEN STRING(currentWidget:MIN-VALUE) + "," + STRING(currentWidget:MAX-VALUE)
      ELSE ""
    ttAttrb.attr_default = IF currentWidget:TYPE EQ "RECTANGLE" THEN ""
                      ELSE IF currentWidget:TYPE EQ "TEXT" THEN ""
                      ELSE currentWidget:SCREEN-VALUE
    ttAttrb.attr_x = currentWidget:X
    ttAttrb.attr_y = currentWidget:Y
    ttAttrb.attr_height = currentWidget:HEIGHT-PIXELS
    ttAttrb.attr_width = currentWidget:WIDTH-PIXELS
    ttAttrb.attr_settings =
           IF ttAttrb.attr_type EQ "EDITOR" THEN
           STRING(currentWidget:SCROLLBAR-HORIZONTAL) + "," + STRING(currentWidget:SCROLLBAR-VERTICAL)
      ELSE IF ttAttrb.attr_type EQ "FILL-IN" THEN currentWidget:FORMAT
      ELSE IF ttAttrb.attr_type EQ "COMBO-BOX" THEN currentWidget:FORMAT
      ELSE IF ttAttrb.attr_type EQ "RADIO-SET" THEN STRING(currentWidget:HORIZONTAL)
      ELSE IF ttAttrb.attr_type EQ "SLIDER" THEN STRING(currentWidget:HORIZONTAL)
      ELSE IF ttAttrb.attr_type EQ "TEXT" THEN STRING(currentWidget:FONT)
      ELSE ""
    ttAttrb.attr_datatype = currentWidget:DATA-TYPE
    savePrompt = YES
    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE winReSize W-Win 
PROCEDURE winReSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE offSet  AS INTEGER NO-UNDO.
  DEFINE VARIABLE iHeight AS INTEGER NO-UNDO.
  DEFINE VARIABLE iWidth  AS INTEGER NO-UNDO.

  ASSIGN
    {&WINDOW-NAME}:WINDOW-STATE = 1
    offSet = IF {&WINDOW-NAME}:HEIGHT-PIXELS GT 600 THEN 50 ELSE 0
    .  
  IF {&WINDOW-NAME}:HEIGHT-PIXELS LT 600 THEN {&WINDOW-NAME}:HEIGHT-PIXELS = 600.
  IF {&WINDOW-NAME}:WIDTH-PIXELS LT 800 THEN {&WINDOW-NAME}:WIDTH-PIXELS = 800.
  ASSIGN
    {&WINDOW-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS - offSet
    {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
    FRAME {&FRAME-NAME}:WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
    FRAME {&FRAME-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
    FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
    FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
    FRAME folderFrm:GRID-FACTOR-VERTICAL = 4
    FRAME folderFrm:GRID-FACTOR-HORIZONTAL = 10
    FRAME folderFrm:GRID-UNIT-WIDTH-PIXELS = 5
    FRAME folderFrm:GRID-UNIT-HEIGHT-PIXELS = 5
    FRAME folderFrm:GRID-SNAP = YES
    FRAME folderFrm:GRID-VISIBLE = YES
    .
  RUN moveObjects.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

