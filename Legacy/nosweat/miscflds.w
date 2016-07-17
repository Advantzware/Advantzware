&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wMiscFlds
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wMiscFlds 
/*------------------------------------------------------------------------

  File:              nosweat\miscflds.w

  Description:       Misc. Fields Design Layout Window

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

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

&scoped-define widget-pool-name dynamic-widget

{methods/defines/miscflds.i &NEW="NEW"}

DEFINE TEMP-TABLE del-mfvalues NO-UNDO
  FIELD mfvalues_recid AS RECID.

DEFINE VARIABLE tabImage AS WIDGET-HANDLE EXTENT 18 NO-UNDO.
DEFINE VARIABLE tabLabel AS WIDGET-HANDLE EXTENT 18 NO-UNDO.
DEFINE VARIABLE labelWidget AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE dynWidget AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE saveWidget AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE currentWidget AS WIDGET-HANDLE NO-UNDO.

DEFINE VARIABLE currentLabel AS CHARACTER NO-UNDO.
DEFINE VARIABLE cdummy AS CHARACTER NO-UNDO.
DEFINE VARIABLE tabOrder AS CHARACTER NO-UNDO.

DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE newMFGroup AS LOGICAL NO-UNDO.
DEFINE VARIABLE copyMFGroup AS LOGICAL NO-UNDO.
DEFINE VARIABLE newTab AS LOGICAL NO-UNDO.
DEFINE VARIABLE leftright AS LOGICAL NO-UNDO.
DEFINE VARIABLE continue AS LOGICAL NO-UNDO.

DEFINE VARIABLE currentTab AS INTEGER NO-UNDO.
DEFINE VARIABLE lastID AS INTEGER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

DEF BUFFER b-mfgroup FOR mfgroup.
DEF BUFFER b-mfdata FOR mfdata.
DEF BUFFER b-attrb FOR attrb.

/* trigger code attached to each created dynamic widget */

&scoped-define trigger-code ~
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

/* to delete a selected widget if focus is outside the design frame folderFrm */
/*
ON DELETE-CHARACTER ANYWHERE DO:
  RUN cutWidgets.
END. */

{methods/lockWindowUpdate.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMiscFlds

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-5 RECT-4 widgetRect RECT-6 ~
RECT-2 RECT-3 mfgroupList btnProperty btnTabOrder btnCut btnCopy btnPaste ~
btnRestore btnSave btnExit btnAddGroup btnCopyGroup btnRenameGroup ~
btnDeleteGroup btnTest mfgroupTab btnNextTab btnPrevTab btnComboBox ~
btnEditor tabLabels btnFillIn btnRadioSet btnRectangle btnSelectionList ~
btnSlider btnText btnAddTab btnRenameTab btnDeleteTab btnToggleBox ~
btnDownTab btnUpTab 
&Scoped-Define DISPLAYED-OBJECTS mfgroupList mfgroupTab tabLabels ~
gapFieldLabel widgetLabel pointerLabel comboBoxLabel editorLabel ~
fillInLabel radioSetLabel rectangleLabel selectionListLabel sliderLabel ~
textLabel toggleBoxLabel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 widgetRect btnRenameGroup btnDeleteGroup btnTest ~
gapField mfgroupTab btnPointer btnNextTab btnPrevTab btnComboBox btnEditor ~
tabLabels btnFillIn btnRadioSet btnRectangle btnSelectionList btnSlider ~
btnText btnAddTab btnRenameTab btnDeleteTab btnToggleBox btnDownTab ~
btnUpTab widgetLabel pointerLabel comboBoxLabel editorLabel fillInLabel ~
radioSetLabel rectangleLabel selectionListLabel sliderLabel textLabel ~
toggleBoxLabel 
&Scoped-define List-2 pointerLabel comboBoxLabel editorLabel fillInLabel ~
radioSetLabel rectangleLabel selectionListLabel sliderLabel textLabel ~
toggleBoxLabel 
&Scoped-define List-3 btnPointer btnComboBox btnEditor btnFillIn ~
btnRadioSet btnRectangle btnSelectionList btnSlider btnText btnToggleBox 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wMiscFlds AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_File 
       MENU-ITEM m_Restore      LABEL "&Restore"      
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
       MENU-ITEM m_Property_Sheet LABEL "Propert&y Sheet"
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
     IMAGE-UP FILE "images/new-au":U
     IMAGE-DOWN FILE "images/new-ad":U
     LABEL "Add Group" 
     SIZE 5.6 BY 1.38 TOOLTIP "Add New Group".

DEFINE BUTTON btnAddTab 
     LABEL "Add" 
     SIZE 11.2 BY 1 TOOLTIP "Add New Tab (Page)"
     FONT 4.

DEFINE BUTTON btnComboBox 
     LABEL "Combo Box" 
     SIZE 7 BY 1.67 TOOLTIP "Combo Box"
     FONT 4.

DEFINE BUTTON btnCopy 
     IMAGE-UP FILE "images/copy-u":U
     IMAGE-DOWN FILE "images/copy-d":U
     IMAGE-INSENSITIVE FILE "images/copy-i":U
     LABEL "Copy" 
     SIZE 8.4 BY 2 TOOLTIP "Copy Selections to Clipboard".

DEFINE BUTTON btnCopyGroup 
     IMAGE-UP FILE "images/copy-u.bmp":U
     IMAGE-DOWN FILE "images/copy-d.bmp":U
     LABEL "Add Group" 
     SIZE 5.6 BY 1.38 TOOLTIP "Add New Group".

DEFINE BUTTON btnCut 
     IMAGE-UP FILE "images/cut-u":U
     IMAGE-DOWN FILE "images/cut-d":U
     IMAGE-INSENSITIVE FILE "images/cut-i":U
     LABEL "Cut" 
     SIZE 8.4 BY 2 TOOLTIP "Cut Selections to Clipboard"
     FONT 4.

DEFINE BUTTON btnDeleteGroup 
     IMAGE-UP FILE "images/del-au":U
     IMAGE-DOWN FILE "images/del-ad":U
     IMAGE-INSENSITIVE FILE "images/del-ai":U
     LABEL "Delete Group" 
     SIZE 5.6 BY 1.38 TOOLTIP "Delete Current Group".

DEFINE BUTTON btnDeleteTab 
     LABEL "Delete" 
     SIZE 11.2 BY 1 TOOLTIP "Delete Current Tab (Page)"
     FONT 4.

DEFINE BUTTON btnDownTab 
     LABEL "Move Dn" 
     SIZE 11.2 BY 1 TOOLTIP "Move Current Tab (Page) Down (Right)"
     FONT 4.

DEFINE BUTTON btnEditor 
     LABEL "Editor" 
     SIZE 7 BY 1.67 TOOLTIP "Editor"
     FONT 4.

DEFINE BUTTON btnExit 
     IMAGE-UP FILE "images/exit-au":U
     LABEL "E&xit" 
     SIZE 8.4 BY 2 TOOLTIP "Exit"
     FONT 4.

DEFINE BUTTON btnFillIn 
     LABEL "Fill In" 
     SIZE 7 BY 1.67 TOOLTIP "Fill In"
     FONT 4.

DEFINE BUTTON btnNextTab 
     IMAGE-UP FILE "images/pvforw":U
     IMAGE-DOWN FILE "images/pvforwd":U
     IMAGE-INSENSITIVE FILE "images/pvforwx":U
     LABEL "Next Tab" 
     SIZE 8 BY 1.38 TOOLTIP "Change to Next Tab (Page)"
     FONT 4.

DEFINE BUTTON btnPaste 
     IMAGE-UP FILE "images/paste-u":U
     IMAGE-DOWN FILE "images/paste-d":U
     IMAGE-INSENSITIVE FILE "images/paste-i":U
     LABEL "Paste" 
     SIZE 8.4 BY 2 TOOLTIP "Paste from Clipboard".

DEFINE BUTTON btnPointer 
     LABEL "Pointer" 
     SIZE 7 BY 1.67 TOOLTIP "Pointer".

DEFINE BUTTON btnPrevTab 
     IMAGE-UP FILE "images/pvback":U
     IMAGE-DOWN FILE "images/pvbackd":U
     IMAGE-INSENSITIVE FILE "images/pvbackx":U
     LABEL "Previous Tab" 
     SIZE 8 BY 1.38 TOOLTIP "Change to Previous Tab (Page)"
     FONT 4.

DEFINE BUTTON btnProperty 
     IMAGE-UP FILE "images/props-u":U
     IMAGE-DOWN FILE "images/props-d":U
     IMAGE-INSENSITIVE FILE "images/props-i":U
     LABEL "Property" 
     SIZE 8.4 BY 2 TOOLTIP "Current SelectionProperty Sheet"
     FONT 4.

DEFINE BUTTON btnRadioSet 
     LABEL "Radio Set" 
     SIZE 7 BY 1.67 TOOLTIP "Radio Set"
     FONT 4.

DEFINE BUTTON btnRectangle 
     LABEL "Rectangle" 
     SIZE 7 BY 1.67 TOOLTIP "Toggle Box"
     FONT 4.

DEFINE BUTTON btnRenameGroup 
     IMAGE-UP FILE "images/r-sort":U
     IMAGE-DOWN FILE "images/r-sortd":U
     LABEL "Rename Group" 
     SIZE 5.6 BY 1.38 TOOLTIP "Rename Current Group".

DEFINE BUTTON btnRenameTab 
     LABEL "Rename" 
     SIZE 11.2 BY 1 TOOLTIP "Rename Current Tab (Page)"
     FONT 4.

DEFINE BUTTON btnRestore 
     IMAGE-UP FILE "images/reset-au":U
     LABEL "Restore" 
     SIZE 8.4 BY 2 TOOLTIP "Restore"
     FONT 4.

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "images/save-u":U
     IMAGE-DOWN FILE "images/save-d":U
     LABEL "Save" 
     SIZE 8.4 BY 2 TOOLTIP "Save"
     FONT 4.

DEFINE BUTTON btnSelectionList 
     LABEL "Selection List" 
     SIZE 7 BY 1.67 TOOLTIP "Selection List"
     FONT 4.

DEFINE BUTTON btnSlider 
     LABEL "Slider" 
     SIZE 7 BY 1.67 TOOLTIP "Slider"
     FONT 4.

DEFINE BUTTON btnTabOrder 
     IMAGE-UP FILE "images/idxup.bmp":U
     LABEL "Tab Order" 
     SIZE 8.4 BY 2 TOOLTIP "Current SelectionProperty Sheet"
     FONT 4.

DEFINE BUTTON btnTest 
     LABEL "Test" 
     SIZE 13.8 BY 1.24.

DEFINE BUTTON btnText 
     LABEL "Text" 
     SIZE 7 BY 1.67 TOOLTIP "Text"
     FONT 4.

DEFINE BUTTON btnToggleBox 
     LABEL "Toggle Box" 
     SIZE 7 BY 1.67 TOOLTIP "Toggle Box"
     FONT 4.

DEFINE BUTTON btnUpTab 
     LABEL "Move Up" 
     SIZE 11.2 BY 1 TOOLTIP "Move Current Tab (Page) Up (Left)"
     FONT 4.

DEFINE VARIABLE mfgroupList AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX SORT INNER-LINES 20
     DROP-DOWN-LIST
     SIZE 48 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE mfgroupTab AS INTEGER FORMAT ">9":U INITIAL 1 
     VIEW-AS COMBO-BOX INNER-LINES 1
     LIST-ITEMS "1" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 TOOLTIP "Change Current Tab (Page)"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE comboBoxLabel AS CHARACTER FORMAT "X(256)":U INITIAL " Combo Box" 
      VIEW-AS TEXT 
     SIZE 16.8 BY 1.62
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE editorLabel AS CHARACTER FORMAT "X(256)":U INITIAL " Editor" 
      VIEW-AS TEXT 
     SIZE 16.8 BY 1.62
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE fillInLabel AS CHARACTER FORMAT "X(256)":U INITIAL " Fill In" 
      VIEW-AS TEXT 
     SIZE 16.8 BY 1.62
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE gapField AS INTEGER FORMAT "z9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5.6 BY 1.24
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE gapFieldLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Gap:" 
      VIEW-AS TEXT 
     SIZE 5 BY .62 NO-UNDO.

DEFINE VARIABLE mfgroupField AS CHARACTER FORMAT "X(256)":U 
     LABEL "Group" 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE pointerLabel AS CHARACTER FORMAT "X(256)":U INITIAL " Pointer" 
      VIEW-AS TEXT 
     SIZE 16.8 BY 1.62
     BGCOLOR 7 FONT 4 NO-UNDO.

DEFINE VARIABLE radioSetLabel AS CHARACTER FORMAT "X(256)":U INITIAL " Radio Set" 
      VIEW-AS TEXT 
     SIZE 16.8 BY 1.62
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE rectangleLabel AS CHARACTER FORMAT "X(256)":U INITIAL " Rectangle" 
      VIEW-AS TEXT 
     SIZE 17 BY 1.62
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE selectionListLabel AS CHARACTER FORMAT "X(256)":U INITIAL " Selection List" 
      VIEW-AS TEXT 
     SIZE 16.8 BY 1.62
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE sliderLabel AS CHARACTER FORMAT "X(256)":U INITIAL " Slider" 
      VIEW-AS TEXT 
     SIZE 16.8 BY 1.62
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE tabField AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 23.8 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE textLabel AS CHARACTER FORMAT "X(256)":U INITIAL " Text" 
      VIEW-AS TEXT 
     SIZE 16.8 BY 1.62
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE toggleBoxLabel AS CHARACTER FORMAT "X(256)":U INITIAL " Toggle Box" 
      VIEW-AS TEXT 
     SIZE 16.8 BY 1.62
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE widgetLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Widgets" 
      VIEW-AS TEXT 
     SIZE 8 BY .62
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 147 BY 3.19
     BGCOLOR 8 .

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

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 26.6 BY 4.52
     BGCOLOR 4 .

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 26.6 BY 16.19
     BGCOLOR 4 .

DEFINE RECTANGLE widgetRect
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 26.6 BY 20.95
     BGCOLOR 15 FGCOLOR 0 .

DEFINE VARIABLE tabLabels AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SCROLLBAR-VERTICAL 
     SIZE 23.8 BY 10 TOOLTIP "Change Current Tab (Page)"
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE Rect-Bottom
     EDGE-PIXELS 0    
     SIZE 89.8 BY .19
     BGCOLOR 7 .

DEFINE RECTANGLE Rect-Left
     EDGE-PIXELS 0    
     SIZE .6 BY 20.95
     BGCOLOR 15 .

DEFINE RECTANGLE Rect-Main
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 90.4 BY 21.19
     BGCOLOR 8 FGCOLOR 0 .

DEFINE RECTANGLE Rect-Right
     EDGE-PIXELS 0    
     SIZE .6 BY 20.81
     BGCOLOR 7 .

DEFINE RECTANGLE Rect-Top
     EDGE-PIXELS 0    
     SIZE 89.8 BY .19
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMiscFlds
     mfgroupList AT ROW 1.67 COL 9 COLON-ALIGNED HELP
          "Select Group Name" NO-LABEL
     btnProperty AT ROW 1.71 COL 62 HELP
          "Access PROPERTY SHEET of Currently Selected Widget"
     btnTabOrder AT ROW 1.71 COL 73 HELP
          "Set Tab Order"
     btnCut AT ROW 1.71 COL 85 HELP
          "CUT Selections"
     btnCopy AT ROW 1.71 COL 95.2 HELP
          "COPY Selections"
     btnPaste AT ROW 1.71 COL 105.4 HELP
          "PASTE Selections"
     btnRestore AT ROW 1.71 COL 118 HELP
          "RESTORE ALL Widgets"
     btnSave AT ROW 1.71 COL 128.2 HELP
          "SAVE Current Screen Layout"
     btnExit AT ROW 1.71 COL 140 HELP
          "CLOSE Design Layout Window"
     btnAddGroup AT ROW 2.86 COL 3 HELP
          "ADD GROU[ Name"
     btnCopyGroup AT ROW 2.86 COL 14 HELP
          "ADD GROU[ Name"
     btnRenameGroup AT ROW 2.86 COL 26.2 HELP
          "CHANGE GROU[ Name"
     btnDeleteGroup AT ROW 2.86 COL 42.4 HELP
          "DELETE GROU[ Name"
     mfgroupField AT ROW 4.67 COL 9 COLON-ALIGNED HELP
          "Enter Group Name"
     btnTest AT ROW 4.67 COL 122.8
     gapField AT ROW 4.67 COL 141.8 COLON-ALIGNED HELP
          "Enter Gap Amount in Pixels" NO-LABEL
     mfgroupTab AT ROW 6.48 COL 18 COLON-ALIGNED HELP
          "Select Tab Number" NO-LABEL
     btnPointer AT ROW 7 COL 124.2 HELP
          "Restore Mouse Cursor to ARROW POINTER"
     btnNextTab AT ROW 7.43 COL 20 HELP
          "Next Tab"
     btnPrevTab AT ROW 8.86 COL 20 HELP
          "Previous tab"
     btnComboBox AT ROW 9 COL 124.2 HELP
          "Create New COMBO-BOX"
     btnEditor AT ROW 11.05 COL 124.2 HELP
          "Create New EDITOR"
     tabLabels AT ROW 11.95 COL 3.8 HELP
          "Select tab Label" NO-LABEL
     btnFillIn AT ROW 12.95 COL 124.2 HELP
          "Create New FILL-IN"
     btnRadioSet AT ROW 15.05 COL 124.2 HELP
          "Create New RADIO-SET"
     btnRectangle AT ROW 16.95 COL 124 HELP
          "Create New Rectangle"
     btnSelectionList AT ROW 18.86 COL 124 HELP
          "Create New SELECTION-LIST"
     btnSlider AT ROW 20.86 COL 124 HELP
          "Create New SLIDER"
     tabField AT ROW 22.19 COL 2 COLON-ALIGNED HELP
          "Enter Tab Label" NO-LABEL
     btnText AT ROW 22.91 COL 124 HELP
          "Create New TEXT"
     btnAddTab AT ROW 23.38 COL 4 HELP
          "ADD New Tab"
     btnRenameTab AT ROW 23.38 COL 16.6 HELP
          "RENAME Tab"
     btnDeleteTab AT ROW 24.52 COL 9.6 HELP
          "DELETE Tab"
     btnToggleBox AT ROW 24.91 COL 124 HELP
          "Create New TOGGLE-BOX"
     btnDownTab AT ROW 25.76 COL 4 HELP
          "Move Tab DOWN"
     btnUpTab AT ROW 25.76 COL 16.6 HELP
          "Move Tab UP"
     gapFieldLabel AT ROW 4.81 COL 136 COLON-ALIGNED NO-LABEL
     widgetLabel AT ROW 6.24 COL 130 COLON-ALIGNED NO-LABEL
     pointerLabel AT ROW 7 COL 129.2 COLON-ALIGNED NO-LABEL
     comboBoxLabel AT ROW 9 COL 129.2 COLON-ALIGNED NO-LABEL
     editorLabel AT ROW 11.05 COL 129.2 COLON-ALIGNED NO-LABEL
     fillInLabel AT ROW 12.95 COL 129.2 COLON-ALIGNED NO-LABEL
     radioSetLabel AT ROW 15.05 COL 129.2 COLON-ALIGNED NO-LABEL
     rectangleLabel AT ROW 16.95 COL 129 COLON-ALIGNED NO-LABEL
     selectionListLabel AT ROW 18.86 COL 129 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 149.8 BY 26.24
         BGCOLOR 3 .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMiscFlds
     sliderLabel AT ROW 20.86 COL 129 COLON-ALIGNED NO-LABEL
     textLabel AT ROW 22.91 COL 129 COLON-ALIGNED NO-LABEL
     toggleBoxLabel AT ROW 24.91 COL 129 COLON-ALIGNED NO-LABEL
     "Tab Order" VIEW-AS TEXT
          SIZE 10.6 BY .76 AT ROW 3.76 COL 72
          BGCOLOR 8 FONT 4
     "File Functions" VIEW-AS TEXT
          SIZE 14 BY .76 AT ROW 3.76 COL 120.6
          BGCOLOR 8 FONT 4
     "Group:" VIEW-AS TEXT
          SIZE 7 BY .76 AT ROW 1.71 COL 4
          BGCOLOR 8 
     "Sheet" VIEW-AS TEXT
          SIZE 6 BY .76 AT ROW 3.76 COL 63.4
          BGCOLOR 8 FONT 4
     "E&xit" VIEW-AS TEXT
          SIZE 4.2 BY .76 AT ROW 3.76 COL 142
          BGCOLOR 8 FONT 4
     "Delete" VIEW-AS TEXT
          SIZE 7.2 BY .76 AT ROW 3.14 COL 49.4
          BGCOLOR 8 FONT 4
     "Rename" VIEW-AS TEXT
          SIZE 8.6 BY .76 AT ROW 3.14 COL 32.8
          BGCOLOR 8 FONT 4
     "Current Tab #" VIEW-AS TEXT
          SIZE 14 BY .76 AT ROW 6.67 COL 6
          BGCOLOR 4 FGCOLOR 15 FONT 4
     "Previous Tab" VIEW-AS TEXT
          SIZE 14 BY .76 AT ROW 9.1 COL 6
          BGCOLOR 4 FGCOLOR 15 FONT 4
     "Add" VIEW-AS TEXT
          SIZE 5 BY .76 AT ROW 3.14 COL 9
          BGCOLOR 8 FONT 4
     "Tab Labels | Tab Order" VIEW-AS TEXT
          SIZE 23 BY .76 AT ROW 11 COL 4
          BGCOLOR 4 FGCOLOR 15 FONT 4
     "Edit Functions" VIEW-AS TEXT
          SIZE 14 BY .76 AT ROW 3.76 COL 92.4
          BGCOLOR 8 FONT 4
     "Next Tab" VIEW-AS TEXT
          SIZE 10.6 BY .76 AT ROW 7.76 COL 9.4
          BGCOLOR 4 FGCOLOR 15 FONT 4
     "Copy" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 3.14 COL 20
          BGCOLOR 8 
     RECT-7 AT ROW 10.76 COL 2.4
     RECT-1 AT ROW 1.38 COL 2.4
     RECT-5 AT ROW 1.38 COL 138
     RECT-4 AT ROW 1.38 COL 116
     widgetRect AT ROW 6 COL 122.8
     RECT-6 AT ROW 6 COL 2.4
     RECT-2 AT ROW 1.38 COL 59.2
     RECT-3 AT ROW 1.38 COL 83
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 149.8 BY 26.24
         BGCOLOR 3 .

DEFINE FRAME folderFrm
     Rect-Main AT ROW 2.05 COL 1.6
     Rect-Top AT ROW 2.14 COL 1.8
     Rect-Left AT ROW 2.19 COL 1.8
     Rect-Right AT ROW 2.29 COL 91.2
     Rect-Bottom AT ROW 23 COL 1.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 30.4 ROW 4.67
         SIZE 91.6 BY 22.29.


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
  CREATE WINDOW wMiscFlds ASSIGN
         HIDDEN             = YES
         TITLE              = "Misc. Fields Design Layout Window"
         HEIGHT             = 26.24
         WIDTH              = 149.8
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

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-C-Win:HANDLE.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT wMiscFlds:LOAD-ICON("Graphics\xRemove.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\xRemove.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wMiscFlds
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMiscFlds
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON btnAddTab IN FRAME fMiscFlds
   1                                                                    */
/* SETTINGS FOR BUTTON btnComboBox IN FRAME fMiscFlds
   1 3                                                                  */
/* SETTINGS FOR BUTTON btnDeleteGroup IN FRAME fMiscFlds
   1                                                                    */
/* SETTINGS FOR BUTTON btnDeleteTab IN FRAME fMiscFlds
   1                                                                    */
/* SETTINGS FOR BUTTON btnDownTab IN FRAME fMiscFlds
   1                                                                    */
/* SETTINGS FOR BUTTON btnEditor IN FRAME fMiscFlds
   1 3                                                                  */
/* SETTINGS FOR BUTTON btnFillIn IN FRAME fMiscFlds
   1 3                                                                  */
/* SETTINGS FOR BUTTON btnNextTab IN FRAME fMiscFlds
   1                                                                    */
/* SETTINGS FOR BUTTON btnPointer IN FRAME fMiscFlds
   NO-ENABLE 1 3                                                        */
/* SETTINGS FOR BUTTON btnPrevTab IN FRAME fMiscFlds
   1                                                                    */
/* SETTINGS FOR BUTTON btnRadioSet IN FRAME fMiscFlds
   1 3                                                                  */
/* SETTINGS FOR BUTTON btnRectangle IN FRAME fMiscFlds
   1 3                                                                  */
/* SETTINGS FOR BUTTON btnRenameGroup IN FRAME fMiscFlds
   1                                                                    */
/* SETTINGS FOR BUTTON btnRenameTab IN FRAME fMiscFlds
   1                                                                    */
/* SETTINGS FOR BUTTON btnSelectionList IN FRAME fMiscFlds
   1 3                                                                  */
/* SETTINGS FOR BUTTON btnSlider IN FRAME fMiscFlds
   1 3                                                                  */
/* SETTINGS FOR BUTTON btnTest IN FRAME fMiscFlds
   1                                                                    */
/* SETTINGS FOR BUTTON btnText IN FRAME fMiscFlds
   1 3                                                                  */
/* SETTINGS FOR BUTTON btnToggleBox IN FRAME fMiscFlds
   1 3                                                                  */
/* SETTINGS FOR BUTTON btnUpTab IN FRAME fMiscFlds
   1                                                                    */
/* SETTINGS FOR FILL-IN comboBoxLabel IN FRAME fMiscFlds
   NO-ENABLE 1 2                                                        */
ASSIGN 
       comboBoxLabel:READ-ONLY IN FRAME fMiscFlds        = TRUE.

/* SETTINGS FOR FILL-IN editorLabel IN FRAME fMiscFlds
   NO-ENABLE 1 2                                                        */
ASSIGN 
       editorLabel:READ-ONLY IN FRAME fMiscFlds        = TRUE.

/* SETTINGS FOR FILL-IN fillInLabel IN FRAME fMiscFlds
   NO-ENABLE 1 2                                                        */
ASSIGN 
       fillInLabel:READ-ONLY IN FRAME fMiscFlds        = TRUE.

/* SETTINGS FOR FILL-IN gapField IN FRAME fMiscFlds
   NO-DISPLAY NO-ENABLE 1                                               */
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

/* SETTINGS FOR FILL-IN radioSetLabel IN FRAME fMiscFlds
   NO-ENABLE 1 2                                                        */
ASSIGN 
       radioSetLabel:READ-ONLY IN FRAME fMiscFlds        = TRUE.

/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME fMiscFlds
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rectangleLabel IN FRAME fMiscFlds
   NO-ENABLE 1 2                                                        */
ASSIGN 
       rectangleLabel:READ-ONLY IN FRAME fMiscFlds        = TRUE.

/* SETTINGS FOR FILL-IN selectionListLabel IN FRAME fMiscFlds
   NO-ENABLE 1 2                                                        */
ASSIGN 
       selectionListLabel:READ-ONLY IN FRAME fMiscFlds        = TRUE.

/* SETTINGS FOR FILL-IN sliderLabel IN FRAME fMiscFlds
   NO-ENABLE 1 2                                                        */
ASSIGN 
       sliderLabel:READ-ONLY IN FRAME fMiscFlds        = TRUE.

/* SETTINGS FOR FILL-IN tabField IN FRAME fMiscFlds
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tabField:HIDDEN IN FRAME fMiscFlds           = TRUE.

/* SETTINGS FOR SELECTION-LIST tabLabels IN FRAME fMiscFlds
   1                                                                    */
/* SETTINGS FOR FILL-IN textLabel IN FRAME fMiscFlds
   NO-ENABLE 1 2                                                        */
ASSIGN 
       textLabel:READ-ONLY IN FRAME fMiscFlds        = TRUE.

/* SETTINGS FOR FILL-IN toggleBoxLabel IN FRAME fMiscFlds
   NO-ENABLE 1 2                                                        */
ASSIGN 
       toggleBoxLabel:READ-ONLY IN FRAME fMiscFlds        = TRUE.

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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wMiscFlds)
THEN wMiscFlds:HIDDEN = no.

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

&Scoped-define SELF-NAME wMiscFlds
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wMiscFlds wMiscFlds
ON END-ERROR OF wMiscFlds /* Misc. Fields Design Layout Window */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wMiscFlds wMiscFlds
ON WINDOW-CLOSE OF wMiscFlds /* Misc. Fields Design Layout Window */
DO:
  IF mfpersist NE ? THEN DELETE PROCEDURE mfpersist.
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wMiscFlds wMiscFlds
ON WINDOW-RESIZED OF wMiscFlds /* Misc. Fields Design Layout Window */
DO:
  RUN winReSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME folderFrm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL folderFrm wMiscFlds
ON ENTRY OF FRAME folderFrm
DO:
  FRAME folderFrm:SELECTED = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL folderFrm wMiscFlds
ON LEAVE OF FRAME folderFrm
DO:
  FRAME folderFrm:SELECTED = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL folderFrm wMiscFlds
ON MOUSE-SELECT-CLICK OF FRAME folderFrm
DO:
  IF CAN-DO("COMBO-BOX,EDITOR,FILL-IN,RADIO-SET,RECTANGLE,SELECTION-LIST,SLIDER,TEXT,TOGGLE-BOX",currentLabel) THEN
  DO: /* only done when a new widget is being placed in design folderFrm */
    RUN newWidget.
    APPLY "CHOOSE" TO btnPointer IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAddGroup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddGroup wMiscFlds
ON CHOOSE OF btnAddGroup IN FRAME fMiscFlds /* Add Group */
DO:
  ASSIGN
     newMFGroup = YES
     copyMFGroup = NO.
  ENABLE mfgroupField WITH FRAME {&FRAME-NAME}.
  mfgroupField:SCREEN-VALUE = "".
  APPLY "ENTRY" TO mfgroupField.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAddTab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddTab wMiscFlds
ON CHOOSE OF btnAddTab IN FRAME fMiscFlds /* Add */
DO:
  newTab = YES.
  ENABLE tabField WITH FRAME {&FRAME-NAME}.
  tabField:SCREEN-VALUE = "".
  APPLY "ENTRY" TO tabField.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnComboBox
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnComboBox wMiscFlds
ON CHOOSE OF btnComboBox IN FRAME fMiscFlds /* Combo Box */
DO:
  RUN selectNewWidgetType ("comboBox","images/combbox.cur").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCopy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCopy wMiscFlds
ON CHOOSE OF btnCopy IN FRAME fMiscFlds /* Copy */
DO:
  RUN copyWidgets.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCopyGroup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCopyGroup wMiscFlds
ON CHOOSE OF btnCopyGroup IN FRAME fMiscFlds /* Add Group */
DO:
  ASSIGN
     copyMFGroup = YES
     newMFGroup = NO.
  ENABLE mfgroupField WITH FRAME {&FRAME-NAME}.
  APPLY "ENTRY" TO mfgroupField IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCut wMiscFlds
ON CHOOSE OF btnCut IN FRAME fMiscFlds /* Cut */
DO:
  RUN cutWidgets.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDeleteGroup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeleteGroup wMiscFlds
ON CHOOSE OF btnDeleteGroup IN FRAME fMiscFlds /* Delete Group */
DO:
  RUN deleteGroup.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDeleteTab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeleteTab wMiscFlds
ON CHOOSE OF btnDeleteTab IN FRAME fMiscFlds /* Delete */
DO:
  RUN deleteTab.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDownTab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDownTab wMiscFlds
ON CHOOSE OF btnDownTab IN FRAME fMiscFlds /* Move Dn */
DO:
  RUN moveTab ("Down").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEditor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEditor wMiscFlds
ON CHOOSE OF btnEditor IN FRAME fMiscFlds /* Editor */
DO:
  RUN selectNewWidgetType ("editor","images/editor.cur").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExit wMiscFlds
ON CHOOSE OF btnExit IN FRAME fMiscFlds /* Exit */
DO:
  IF mfpersist NE ? THEN DELETE PROCEDURE mfpersist.
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFillIn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFillIn wMiscFlds
ON CHOOSE OF btnFillIn IN FRAME fMiscFlds /* Fill In */
DO:
  RUN selectNewWidgetType ("fillIn","images/fill_in.cur").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNextTab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNextTab wMiscFlds
ON CHOOSE OF btnNextTab IN FRAME fMiscFlds /* Next Tab */
DO:
  mfgroupTab:SCREEN-VALUE IN FRAME {&FRAME-NAME} =
      STRING(INTEGER(mfgroupTab:SCREEN-VALUE IN FRAME {&FRAME-NAME}) + 1).
  RUN createWidgets.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPaste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPaste wMiscFlds
ON CHOOSE OF btnPaste IN FRAME fMiscFlds /* Paste */
DO:
  RUN pasteFromClipboard.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPointer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPointer wMiscFlds
ON CHOOSE OF btnPointer IN FRAME fMiscFlds /* Pointer */
DO:
  RUN selectNewWidgetType ("","arrow").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrevTab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrevTab wMiscFlds
ON CHOOSE OF btnPrevTab IN FRAME fMiscFlds /* Previous Tab */
DO:
  mfgroupTab:SCREEN-VALUE IN FRAME {&FRAME-NAME} =
      STRING(INTEGER(mfgroupTab:SCREEN-VALUE IN FRAME {&FRAME-NAME}) - 1).
  RUN createWidgets.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnProperty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnProperty wMiscFlds
ON CHOOSE OF btnProperty IN FRAME fMiscFlds /* Property */
DO:
  RUN propertySheet.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRadioSet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRadioSet wMiscFlds
ON CHOOSE OF btnRadioSet IN FRAME fMiscFlds /* Radio Set */
DO:
  RUN selectNewWidgetType ("radioSet","images/radioset.cur").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRectangle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRectangle wMiscFlds
ON CHOOSE OF btnRectangle IN FRAME fMiscFlds /* Rectangle */
DO:
  RUN selectNewWidgetType ("rectangle","images/rectangle.cur").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRenameGroup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRenameGroup wMiscFlds
ON CHOOSE OF btnRenameGroup IN FRAME fMiscFlds /* Rename Group */
DO:
  ASSIGN
     newMFGroup = NO
     copyMFGroup = NO.
  ENABLE mfgroupField WITH FRAME {&FRAME-NAME}.
  mfgroupField:SCREEN-VALUE = "".
  APPLY "ENTRY" TO mfgroupField.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRenameTab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRenameTab wMiscFlds
ON CHOOSE OF btnRenameTab IN FRAME fMiscFlds /* Rename */
DO:
  newTab = NO.
  ENABLE tabField WITH FRAME {&FRAME-NAME}.
  ASSIGN
    tabField:SCREEN-VALUE = ENTRY(1,tabLabels:SCREEN-VALUE,"|")
    tabOrder = IF NUM-ENTRIES(tabLabels:SCREEN-VALUE,"|") GT 1 THEN
               ENTRY(2,tabLabels:SCREEN-VALUE,"|") ELSE 'Default'.
  APPLY "ENTRY" TO tabField.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRestore
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRestore wMiscFlds
ON CHOOSE OF btnRestore IN FRAME fMiscFlds /* Restore */
DO:
  mfgroupTab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1".
  RUN loadWidgetData.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave wMiscFlds
ON CHOOSE OF btnSave IN FRAME fMiscFlds /* Save */
DO:
  RUN saveLayout.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelectionList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelectionList wMiscFlds
ON CHOOSE OF btnSelectionList IN FRAME fMiscFlds /* Selection List */
DO:
  RUN selectNewWidgetType ("selectionList","images/slctlist.cur").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSlider
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSlider wMiscFlds
ON CHOOSE OF btnSlider IN FRAME fMiscFlds /* Slider */
DO:
  RUN selectNewWidgetType ("slider","images/slider.cur").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTabOrder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTabOrder wMiscFlds
ON CHOOSE OF btnTabOrder IN FRAME fMiscFlds /* Tab Order */
DO:
  RUN tabOrder.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTest wMiscFlds
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
  RUN nosweat/mfvalues.w PERSISTENT
      (mfgroupList:SCREEN-VALUE IN FRAME {&FRAME-NAME},
       prgrms.rec_key,{methods/headers/prgrms.i}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnText wMiscFlds
ON CHOOSE OF btnText IN FRAME fMiscFlds /* Text */
DO:
  RUN selectNewWidgetType ("text","images/text.cur").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnToggleBox
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnToggleBox wMiscFlds
ON CHOOSE OF btnToggleBox IN FRAME fMiscFlds /* Toggle Box */
DO:
  RUN selectNewWidgetType ("toggleBox","images/toggle.cur").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUpTab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUpTab wMiscFlds
ON CHOOSE OF btnUpTab IN FRAME fMiscFlds /* Move Up */
DO:
  RUN moveTab ("Up").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gapField
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gapField wMiscFlds
ON LEAVE OF gapField IN FRAME fMiscFlds
DO:
  APPLY "RETURN" TO {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gapField wMiscFlds
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mfgroupField wMiscFlds
ON LEAVE OF mfgroupField IN FRAME fMiscFlds /* Group */
DO:
  APPLY "RETURN" TO {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mfgroupField wMiscFlds
ON RETURN OF mfgroupField IN FRAME fMiscFlds /* Group */
DO:
  IF {&SELF-NAME}:SCREEN-VALUE NE "" THEN DO:
    ASSIGN
      cdummy = {&SELF-NAME}:SCREEN-VALUE
      ldummy = NO
      currentLabel = "".
    DO i = 1 TO mfgroupList:NUM-ITEMS: /* see if item already exists */
      IF mfgroupList:ENTRY(i) = cdummy THEN ldummy = YES.
    END.
    IF newMFGroup THEN RUN addGroup.
    ELSE IF copyMFGroup THEN RUN copyGroup.
    ELSE RUN renameGroup.
  END.
  ELSE cdummy = mfgroupList:SCREEN-VALUE.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      mfgroupList:SCREEN-VALUE = cdummy
      {&SELF-NAME}:HIDDEN = YES.
    DISABLE {&SELF-NAME}.
    APPLY "ENTRY" TO mfgroupList.
    APPLY "VALUE-CHANGED" TO mfgroupList.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mfgroupList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mfgroupList wMiscFlds
ON VALUE-CHANGED OF mfgroupList IN FRAME fMiscFlds
DO:
  mfgroupTab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1". /* set to first tab */
  RUN createWidgets.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mfgroupTab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mfgroupTab wMiscFlds
ON VALUE-CHANGED OF mfgroupTab IN FRAME fMiscFlds
DO:
  DO i = 1 TO tabLabels:NUM-ITEMS: /* get index value of selected label */
    IF tabLabels:SCREEN-VALUE = tabLabels:ENTRY(i) THEN LEAVE.
  END.
  IF i = INTEGER(mfgroupTab:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN RETURN NO-APPLY.
  RUN createWidgets.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_1_LeftRight_Sides
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_1_LeftRight_Sides wMiscFlds
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_2_TopBottom_Sides wMiscFlds
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_addGroup wMiscFlds
ON CHOOSE OF MENU-ITEM m_addGroup /* Add Group */
DO:
  APPLY "CHOOSE" TO btnAddGroup IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Add_Tab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Add_Tab wMiscFlds
ON CHOOSE OF MENU-ITEM m_Add_Tab /* Add Tab */
DO:
  APPLY "CHOOSE" TO btnAddTab IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Bottom_Sides
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Bottom_Sides wMiscFlds
ON CHOOSE OF MENU-ITEM m_Bottom_Sides /* Bottom Sides */
DO:
  RUN alignWidgets ("Bottom").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Change_Group
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Change_Group wMiscFlds
ON CHOOSE OF MENU-ITEM m_Change_Group /* Rename Group */
DO:
  APPLY "CHOOSE" TO btnRenameGroup IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Combo_Box
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Combo_Box wMiscFlds
ON CHOOSE OF MENU-ITEM m_Combo_Box /* Combo Box */
DO:
  APPLY "CHOOSE" TO btnComboBox IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Copy wMiscFlds
ON CHOOSE OF MENU-ITEM m_Copy /* Copy */
DO:
  APPLY "CHOOSE" TO btnCopy IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_copyGroup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_copyGroup wMiscFlds
ON CHOOSE OF MENU-ITEM m_copyGroup /* Copy Group */
DO:
   APPLY "CHOOSE" TO btnCopyGroup IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Cut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Cut wMiscFlds
ON CHOOSE OF MENU-ITEM m_Cut /* Cut */
DO:
  APPLY "CHOOSE" TO btnCut IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_deleteGroup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_deleteGroup wMiscFlds
ON CHOOSE OF MENU-ITEM m_deleteGroup /* Delete Group */
DO:
  APPLY "CHOOSE" TO btnDeleteGroup IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Delete_Tab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Delete_Tab wMiscFlds
ON CHOOSE OF MENU-ITEM m_Delete_Tab /* Delete Tab */
DO:
  APPLY "CHOOSE" TO btnDeleteTab IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Editor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Editor wMiscFlds
ON CHOOSE OF MENU-ITEM m_Editor /* Editor */
DO:
  APPLY "CHOOSE" TO btnEditor IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Exit wMiscFlds
ON CHOOSE OF MENU-ITEM m_Exit /* Exit */
DO:
  APPLY "CHOOSE" TO btnExit IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Fill_In
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Fill_In wMiscFlds
ON CHOOSE OF MENU-ITEM m_Fill_In /* Fill In */
DO:
  APPLY "CHOOSE" TO btnFillIn IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Left_Sides
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Left_Sides wMiscFlds
ON CHOOSE OF MENU-ITEM m_Left_Sides /* Left Sides */
DO:
  RUN alignWidgets ("Left").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Move_Tab_Down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Move_Tab_Down wMiscFlds
ON CHOOSE OF MENU-ITEM m_Move_Tab_Down /* Move Tab Down */
DO:
  APPLY "CHOOSE" TO btnDownTab IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Move_Tab_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Move_Tab_Up wMiscFlds
ON CHOOSE OF MENU-ITEM m_Move_Tab_Up /* Move Tab Up */
DO:
  APPLY "CHOOSE" TO btnUpTab IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Next_Tab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Next_Tab wMiscFlds
ON CHOOSE OF MENU-ITEM m_Next_Tab /* Next Tab */
DO:
  APPLY "CHOOSE" TO btnNextTab IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Paste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Paste wMiscFlds
ON CHOOSE OF MENU-ITEM m_Paste /* Paste */
DO:
  APPLY "CHOOSE" TO btnPaste IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Pointer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Pointer wMiscFlds
ON CHOOSE OF MENU-ITEM m_Pointer /* Pointer */
DO:
  APPLY "CHOOSE" TO btnPointer IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Previous_Tab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Previous_Tab wMiscFlds
ON CHOOSE OF MENU-ITEM m_Previous_Tab /* Previous Tab */
DO:
  APPLY "CHOOSE" TO btnPrevTab IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Property_Sheet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Property_Sheet wMiscFlds
ON CHOOSE OF MENU-ITEM m_Property_Sheet /* Property Sheet */
DO:
  APPLY "CHOOSE" TO btnProperty IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Radio_Set
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Radio_Set wMiscFlds
ON CHOOSE OF MENU-ITEM m_Radio_Set /* Radio Set */
DO:
  APPLY "CHOOSE" TO btnRadioSet IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Rename_Tab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Rename_Tab wMiscFlds
ON CHOOSE OF MENU-ITEM m_Rename_Tab /* Rename Tab */
DO:
  APPLY "CHOOSE" TO btnRenameTab IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Restore
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Restore wMiscFlds
ON CHOOSE OF MENU-ITEM m_Restore /* Restore */
DO:
  APPLY "CHOOSE" TO btnRestore IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Right_Sides
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Right_Sides wMiscFlds
ON CHOOSE OF MENU-ITEM m_Right_Sides /* Right Sides */
DO:
  RUN alignWidgets ("Right").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Save wMiscFlds
ON CHOOSE OF MENU-ITEM m_Save /* Save */
DO:
  APPLY "CHOOSE" TO btnSave IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Selection_List
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Selection_List wMiscFlds
ON CHOOSE OF MENU-ITEM m_Selection_List /* Selection List */
DO:
  APPLY "CHOOSE" TO btnSelectionList IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Slider
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Slider wMiscFlds
ON CHOOSE OF MENU-ITEM m_Slider /* Slider */
DO:
  APPLY "CHOOSE" TO btnSlider IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Tab_Order
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Tab_Order wMiscFlds
ON CHOOSE OF MENU-ITEM m_Tab_Order /* Tab Order */
DO:
  APPLY "CHOOSE" TO btnTabOrder IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Text
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Text wMiscFlds
ON CHOOSE OF MENU-ITEM m_Text /* Text */
DO:
  APPLY "CHOOSE" TO btnText IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Toggle_Box
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Toggle_Box wMiscFlds
ON CHOOSE OF MENU-ITEM m_Toggle_Box /* Toggle Box */
DO:
  APPLY "CHOOSE" TO btnToggleBox IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Top_Sides
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Top_Sides wMiscFlds
ON CHOOSE OF MENU-ITEM m_Top_Sides /* Top Sides */
DO:
  RUN alignWidgets ("Top").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tabField
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tabField wMiscFlds
ON LEAVE OF tabField IN FRAME fMiscFlds
DO:
  APPLY "RETURN" TO {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tabField wMiscFlds
ON RETURN OF tabField IN FRAME fMiscFlds
DO:
  IF {&SELF-NAME}:SCREEN-VALUE NE "" THEN DO:
    IF newTab THEN RUN addTab.
    ELSE RUN renameTab.
  END.
  tabField:HIDDEN = YES.
  DISABLE tabField.
  APPLY "VALUE-CHANGED" TO tabLabels.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tabLabels
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tabLabels wMiscFlds
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wMiscFlds 


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
  APPLY 'CLOSE' TO THIS-PROCEDURE.
END.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN setTabOrder. /* natural order is left to right, we want top to bottom */
  RUN loadImages.
  RUN enable_UI.
  RUN winReSize.
  RUN loadWidgetData.
  DISABLE btnPaste btnPointer btnProperty btnCut btnCopy WITH FRAME {&FRAME-NAME}.
  IF mfpersist EQ ? THEN
  RUN nosweat/mfpersist.p PERSISTENT SET mfpersist.
  {methods/nowait.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
  &IF DEFINED(UIB_is_Running) EQ 0 &THEN
  RUN sys/ref/d-passmi.w (OUTPUT continue).
  &ELSE
  continue = YES.
  &ENDIF
  IF NOT continue THEN
  APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addGroup wMiscFlds 
PROCEDURE addGroup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT ldummy THEN DO:
    ldummy = mfgroupList:ADD-LAST(cdummy) IN FRAME {&FRAME-NAME}.
    CREATE tmfgroup.
    ASSIGN
      tmfgroup.mfgroup_data = cdummy
      tmfgroup.mfgroup_tabs = " ".
  END.
  ELSE MESSAGE "Group" cdummy "Already Exists!" VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addTab wMiscFlds 
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
    FIND tmfgroup EXCLUSIVE-LOCK WHERE tmfgroup.mfgroup_data = mfgroupList:SCREEN-VALUE.
    ASSIGN
      ldummy = tabLabels:INSERT(tabField:SCREEN-VALUE + "|Default",i + 1)
      tabLabels:SCREEN-VALUE = tabLabels:ENTRY(i + 1)
      ldummy = mfgroupTab:ADD-LAST(STRING(tabLabels:NUM-ITEMS))
      tmfgroup.mfgroup_tabs = tabLabels:LIST-ITEMS.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE alignWidgets wMiscFlds 
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
    currentWidget = currentWidget:FIRST-CHILD.
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
      currentWidget = currentWidget:FIRST-CHILD.
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
    currentWidget = currentWidget:FIRST-CHILD.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clearClipboard wMiscFlds 
PROCEDURE clearClipboard :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH wtbl-clipboard EXCLUSIVE-LOCK:
    DELETE wtbl-clipboard.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyGroup wMiscFlds 
PROCEDURE copyGroup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-tmfgroup FOR tmfgroup.

  IF NOT ldummy THEN DO:
     SESSION:SET-WAIT-STATE("general").
     mfgroupList:ADD-LAST(cdummy) IN FRAME {&FRAME-NAME}.
     FIND b-tmfgroup WHERE b-tmfgroup.mfgroup_data EQ mfgroupList:SCREEN-VALUE NO-ERROR.
     IF AVAIL b-tmfgroup THEN DO:
        CREATE tmfgroup.
        ASSIGN
           tmfgroup.mfgroup_data = cdummy
           tmfgroup.mfgroup_tabs = b-tmfgroup.mfgroup_tabs.
     END.
     FOR EACH attrb WHERE
         attrb.attr_mfgroup EQ mfgroupList:SCREEN-VALUE IN FRAME {&FRAME-NAME}:
         CREATE b-attrb.
         RUN setAttrID-buffer.
         BUFFER-COPY attrb EXCEPT attr_id ATTR_mfgroup TO b-attrb
             ASSIGN b-attrb.ATTR_mfgroup = mfgroupField:SCREEN-VALUE.
         RELEASE b-attrb.
     END.
     SESSION:SET-WAIT-STATE("").
  END.
  ELSE MESSAGE "Group" cdummy "Already Exists!" VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyToClipboard wMiscFlds 
PROCEDURE copyToClipboard :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND attrb EXCLUSIVE-LOCK
       WHERE ROWID(attrb) = TO-ROWID(ENTRY(1,currentWidget:PRIVATE-DATA,"|")).
  CREATE wtbl-clipboard.
  BUFFER-COPY attrb TO wtbl-clipboard.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyWidgets wMiscFlds 
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
    currentWidget = currentWidget:FIRST-CHILD.
  DO WHILE currentWidget NE ?:
    IF currentWidget:SELECTED THEN
    RUN copyToClipboard.
    currentWidget = currentWidget:NEXT-SIBLING.
  END.
  IF CAN-FIND(FIRST wtbl-clipboard) THEN
  ENABLE btnPaste WITH FRAME {&FRAME-NAME}.
  ELSE
  DISABLE btnPaste WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createLabelWidget wMiscFlds 
PROCEDURE createLabelWidget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-type AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ip-label AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ip-x AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ip-y AS INTEGER NO-UNDO.

  IF NOT CAN-DO("RECTANGLE,TEXT,TOGGLE-BOX",ip-type) THEN DO: /* text and toggle-box have no labels */
    CREATE TEXT labelWidget IN WIDGET-POOL "{&widget-pool-name}"
      ASSIGN
        FRAME = FRAME folderFrm:HANDLE
        AUTO-RESIZE = YES
        Y = ip-y
        FORMAT = "X(" +
          (IF LENGTH(ip-label) NE 0 THEN STRING(LENGTH(ip-label) + 1)
          ELSE "1") + ")"
        SENSITIVE = YES
        SCREEN-VALUE = IF ip-label = "" THEN ""
                       ELSE ip-label + ":".
    
    ASSIGN
      labelWidget:FONT = ?
      labelWidget:X = IF ip-x - labelWidget:WIDTH-PIXELS - 1 LT 0 THEN 0
                       ELSE ip-x - labelWidget:WIDTH-PIXELS - 1
      labelWidget:HEIGHT-CHARS = 1
      ldummy = labelWidget:MOVE-TO-TOP()
      labelWidget:HIDDEN = NO.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createTabs wMiscFlds 
PROCEDURE createTabs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
  DO WITH FRAME {&FRAME-NAME}:
    FIND tmfgroup NO-LOCK WHERE tmfgroup.mfgroup_data EQ mfgroupList:SCREEN-VALUE NO-ERROR.
    IF NOT AVAILABLE tmfgroup THEN DO:
      ASSIGN
        mfgroupTab:LIST-ITEMS = "1"
        tabLabels:LIST-ITEMS = ""
        tabLabels:INNER-LINES = 1.
      DISABLE {&LIST-1} {&LIST-3}.
      RETURN.
    END.
    ELSE DO:
      ENABLE {&LIST-1} {&LIST-3}.
      RUN selectNewWidgetType ("","arrow").
    END.
    ASSIGN
      currentTab = INTEGER(mfgroupTab:SCREEN-VALUE)
      cdummy = "1".
    DO i = 2 TO NUM-ENTRIES(tmfgroup.mfgroup_tabs):
      cdummy = cdummy + "," + STRING(i).
    END.
    ASSIGN
      mfgroupTab:INNER-LINES = NUM-ENTRIES(tmfgroup.mfgroup_tabs)
      mfgroupTab:LIST-ITEMS = cdummy
      mfgroupTab:SCREEN-VALUE = STRING(currentTab)
      tabLabels:INNER-LINES = NUM-ENTRIES(tmfgroup.mfgroup_tabs)
      tabLabels:LIST-ITEMS = tmfgroup.mfgroup_tabs
      tabLabels:SCREEN-VALUE = tabLabels:ENTRY(currentTab).
    IF currentTab EQ 1 THEN DISABLE btnPrevTab.
    IF currentTab EQ NUM-ENTRIES(tmfgroup.mfgroup_tabs) THEN DISABLE btnNextTab.
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
  DO i = 1 TO NUM-ENTRIES(tmfgroup.mfgroup_tabs):
    CREATE IMAGE tabImage[i] IN WIDGET-POOL "{&widget-pool-name}"
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
      CREATE TEXT tabLabel[i] IN WIDGET-POOL "{&widget-pool-name}"
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
          SCREEN-VALUE = ENTRY(1, ENTRY(i,tmfgroup.mfgroup_tabs) ,"|")
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
        tabLabel[i]:HIDDEN = NO.
    DISABLE btnCut btnCopy WITH FRAME {&FRAME-NAME}.
  END.
  /* use up tab image for currently selected tab */
  ASSIGN
    mfgroupTab:SCREEN-VALUE = STRING(currentTab)
    tabImage[currentTab]:HEIGHT-PIXEL = 27
    ldummy = tabImage[currentTab]:LOAD-IMAGE("adeicon/ts-up72")
    ldummy = tabImage[currentTab]:MOVE-TO-TOP()
    ldummy = tabLabel[currentTab]:MOVE-TO-TOP().
  RUN LockWindowUpdate (0,OUTPUT i).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createWidgets wMiscFlds 
PROCEDURE createWidgets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
  DELETE WIDGET-POOL "{&widget-pool-name}" NO-ERROR.
  CREATE WIDGET-POOL "{&widget-pool-name}" PERSISTENT.
  RUN createTabs.
  FOR EACH attrb NO-LOCK
      WHERE attrb.attr_mfgroup EQ mfgroupList:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        AND attrb.attr_tab EQ INTEGER(mfgroupTab:SCREEN-VALUE):
    RUN dynamicWidget.
  END.
  RUN LockWindowUpdate (0,OUTPUT i).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cutWidgets wMiscFlds 
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
    currentWidget = currentWidget:FIRST-CHILD.
  WIDGET-TREE:
  DO WHILE currentWidget NE ?:
    IF currentWidget:SELECTED THEN DO:
      RUN copyToClipboard.
      IF CAN-FIND(FIRST mfvalues WHERE mfvalues.mf_id = attrb.attr_id) THEN DO:
        ldummy = NO.
        MESSAGE "Attribute Value Records Exist!" SKIP(1)
                "Cut Attribute Widget & Delete Attribute Value Records When Layout SAVED? (yes/no)" SKIP(1)
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Attribute Widget: " + attrb.attr_label
                UPDATE ldummy.
        IF NOT ldummy THEN DO:
          DELETE wtbl-clipboard.
          currentWidget = currentWidget:NEXT-SIBLING.
          NEXT WIDGET-TREE.
        END.
      END.
      RUN deleteMFValues.
      DELETE attrb.
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
  IF CAN-FIND(FIRST wtbl-clipboard) THEN
  ENABLE btnPaste WITH FRAME {&FRAME-NAME}.
  ELSE
  DISABLE btnPaste WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteGroup wMiscFlds 
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
    FOR EACH attrb WHERE attrb.attr_mfgroup = mfgroupList:SCREEN-VALUE NO-LOCK:
      IF CAN-FIND(FIRST mfvalues WHERE mfvalues.mf_id = attrb.attr_id) THEN DO:
        ldummy = NO.
        MESSAGE "Attribute Value Records Exist!" SKIP(1)
                "Delete Group & Delete Attribute Value Records When Layout SAVED? (yes/no)"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Delete Group: " + attrb.attr_mfgroup
            UPDATE ldummy.
        IF NOT ldummy THEN RETURN.
        LEAVE.
      END.
    END.
    FIND tmfgroup WHERE tmfgroup.mfgroup_data = mfgroupList:SCREEN-VALUE EXCLUSIVE-LOCK.
    DELETE tmfgroup.
    FOR EACH attrb WHERE attrb.attr_mfgroup = mfgroupList:SCREEN-VALUE EXCLUSIVE-LOCK:
      RUN deleteMFValues.
      DELETE attrb.
    END.
    ASSIGN
      ldummy = mfgroupList:DELETE(mfgroupList:SCREEN-VALUE)
      mfgroupList:SCREEN-VALUE = IF mfgroupList:NUM-ITEMS GT 1 THEN mfgroupList:ENTRY(2)
                                ELSE mfgroupList:ENTRY(1).
    APPLY "ENTRY" TO mfgroupList.
    APPLY "VALUE-CHANGED" TO mfgroupList.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteMFValues wMiscFlds 
PROCEDURE deleteMFValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH mfvalues WHERE mfvalues.mf_id = attrb.attr_id NO-LOCK:
    FIND del-mfvalues WHERE mfvalues_recid = RECID(mfvalues) NO-LOCK NO-ERROR.
    IF AVAILABLE del-mfvalues THEN NEXT.
    CREATE del-mfvalues.
    del-mfvalues.mfvalues_recid = RECID(mfvalues).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteTab wMiscFlds 
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
    FOR EACH attrb NO-LOCK
        WHERE attrb.attr_mfgroup = mfgroupList:SCREEN-VALUE
          AND attrb.attr_tab = i:
      IF CAN-FIND(FIRST mfvalues WHERE mfvalues.mf_id = attrb.attr_id) THEN DO:
        ldummy = NO.
        MESSAGE "Attribute Value Records Exist!" SKIP(1)
                "Remove Tab & Delete Attribute Value Records When Layout SAVED? (yes/no)"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Remove Tab: " + tabLabels:ENTRY(i)
            UPDATE ldummy.
        IF NOT ldummy THEN RETURN.
        LEAVE.
      END.
    END.
    FOR EACH attrb EXCLUSIVE-LOCK
        WHERE attrb.attr_mfgroup = mfgroupList:SCREEN-VALUE AND attrb.attr_tab = i:
      RUN deleteMFValues.
      DELETE attrb.
    END.
    FIND tmfgroup WHERE tmfgroup.mfgroup_data EQ mfgroupList:SCREEN-VALUE EXCLUSIVE-LOCK.
    ASSIGN
      ldummy = tabLabels:DELETE(i)
      tabLabels:SCREEN-VALUE = tabLabels:ENTRY(1)
      ldummy = mfgroupTab:DELETE(i)
      mfgroupTab:SCREEN-VALUE = mfgroupTab:ENTRY(1)
      tmfgroup.mfgroup_tabs = tabLabels:LIST-ITEMS.
    RUN createWidgets.
    DISABLE btnCut btnCopy WITH FRAME {&FRAME-NAME}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deselectWidget wMiscFlds 
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
  DISABLE btnCut btnCopy WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wMiscFlds  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wMiscFlds)
  THEN DELETE WIDGET wMiscFlds.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dynamicWidget wMiscFlds 
PROCEDURE dynamicWidget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE horz-bar AS LOGICAL NO-UNDO.
  DEFINE VARIABLE vert-bar AS LOGICAL NO-UNDO.

&scoped-define defaults-code ~
          ASSIGN ~
            FRAME = FRAME folderFrm:HANDLE ~
            NAME = attrb.attr_name ~
            X = attrb.attr_x ~
            Y = attrb.attr_y ~
            SENSITIVE = YES ~
            MOVABLE = YES ~
            SELECTABLE = YES ~
            RESIZABLE = YES ~
            PRIVATE-DATA = STRING(ROWID(attrb)) + "|" + ~
                           mfgroupTab:SCREEN-VALUE IN FRAME {&FRAME-NAME} ~
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
            SCREEN-VALUE = attrb.attr_default ~
            {&trigger-code}
&scoped-define editor-code ~
            {&widget-code} ~
            FONT = ? ~
            SCROLLBAR-VERTICAL = vert-bar ~
            SCROLLBAR-HORIZONTAL = horz-bar ~
            HELP = 'Enter ''' + attrb.attr_label + ''' in EDITOR Field' ~
            SCREEN-VALUE = attrb.attr_default ~
            {&trigger-code}
&scoped-define fill-in-code ~
            {&widget-code} ~
            FONT = ? ~
            DATA-TYPE = attrb.attr_datatype ~
            FORMAT = attrb.attr_settings ~
            HELP = 'Enter ''' + attrb.attr_label + ''' in FILL-IN Field' ~
            SCREEN-VALUE = attrb.attr_default ~
            {&trigger-code}
&scoped-define radio-set-code ~
            {&widget-code} ~
            FONT = ? ~
            HORIZONTAL = horz-bar ~
            RADIO-BUTTONS = attrb.attr_values ~
            HELP = 'Select ''' + attrb.attr_label + ''' from RADIO-SET Choices' ~
            SCREEN-VALUE = attrb.attr_default ~
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
            SCREEN-VALUE = attrb.attr_default ~
            {&trigger-code}
&scoped-define slider-code ~
            {&widget-code} ~
            FONT = ? ~
            HORIZONTAL = horz-bar ~
            MIN-VALUE = INT(ENTRY(1,attrb.attr_values)) ~
            MAX-VALUE = INT(ENTRY(2,attrb.attr_values)) ~
            HELP = 'Set ''' + attrb.attr_label + ''' from SLIDER Range' ~
            SCREEN-VALUE = attrb.attr_default ~
            {&trigger-code}
&scoped-define text-code ~
            {&defaults-code} ~
            FORMAT = 'X(80)' ~
            FONT = INTEGER(attrb.attr_settings) ~
            HEIGHT-PIXELS = attrb.attr_height ~
            HELP = '''' + attrb.attr_label + ''' is a TEXT Widget' ~
            SCREEN-VALUE = attrb.attr_label ~
            {&trigger-code}
&scoped-define toggle-box-code ~
            {&defaults-code} ~
            FONT = ? ~
            LABEL = attrb.attr_label ~
            HEIGHT-PIXELS = attrb.attr_height ~
            HELP = 'Set ''' + attrb.attr_label + ''' TOGGLE-BOX On/Off' ~
            SCREEN-VALUE = attrb.attr_default ~
            {&trigger-code}
  ASSIGN
    horz-bar = IF NUM-ENTRIES(attrb.attr_settings) NE 0 AND
                  ENTRY(1,attrb.attr_settings) = "yes" THEN YES ELSE no
    vert-bar = IF NUM-ENTRIES(attrb.attr_settings) GT 1 AND
                  ENTRY(2,attrb.attr_settings) = "yes" THEN yes ELSE no.
  RUN createLabelWidget (attrb.attr_type,attrb.attr_label,attrb.attr_x,attrb.attr_y).
  DO ON ERROR UNDO, RETURN "CREATE-ERROR":
    CASE attrb.attr_type:
&scoped-define widget-type COMBO-BOX
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "{&widget-pool-name}"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type EDITOR
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "{&widget-pool-name}"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type FILL-IN
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "{&widget-pool-name}"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type RADIO-SET
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "{&widget-pool-name}"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type RECTANGLE
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "{&widget-pool-name}"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type SELECTION-LIST
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "{&widget-pool-name}"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type SLIDER
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "{&widget-pool-name}"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type TEXT
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "{&widget-pool-name}"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type TOGGLE-BOX
      WHEN "{&widget-type}" THEN DO:
        CREATE {&widget-type} dynWidget IN WIDGET-POOL "{&widget-pool-name}"
        {&{&widget-type}-code}
      END.
    END CASE.
    IF VALID-HANDLE(dynWidget) THEN
    ASSIGN
      ldummy = dynWidget:MOVE-TO-TOP()
      dynWidget:HIDDEN = NO.
  END.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wMiscFlds  _DEFAULT-ENABLE
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
  DISPLAY mfgroupList mfgroupTab tabLabels gapFieldLabel widgetLabel 
          pointerLabel comboBoxLabel editorLabel fillInLabel radioSetLabel 
          rectangleLabel selectionListLabel sliderLabel textLabel toggleBoxLabel 
      WITH FRAME fMiscFlds IN WINDOW wMiscFlds.
  ENABLE RECT-7 RECT-5 RECT-4 widgetRect RECT-6 RECT-2 RECT-3 mfgroupList 
         btnProperty btnTabOrder btnCut btnCopy btnPaste btnRestore btnSave 
         btnExit btnAddGroup btnCopyGroup btnRenameGroup btnDeleteGroup btnTest 
         mfgroupTab btnNextTab btnPrevTab btnComboBox btnEditor tabLabels 
         btnFillIn btnRadioSet btnRectangle btnSelectionList btnSlider btnText 
         btnAddTab btnRenameTab btnDeleteTab btnToggleBox btnDownTab btnUpTab 
      WITH FRAME fMiscFlds IN WINDOW wMiscFlds.
  {&OPEN-BROWSERS-IN-QUERY-fMiscFlds}
  ENABLE Rect-Top Rect-Left Rect-Right Rect-Bottom 
      WITH FRAME folderFrm IN WINDOW wMiscFlds.
  {&OPEN-BROWSERS-IN-QUERY-folderFrm}
  VIEW wMiscFlds.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE labelTrigger wMiscFlds 
PROCEDURE labelTrigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER tabno AS INTEGER NO-UNDO.

  IF tabno = INTEGER(mfgroupTab:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN RETURN.
  mfgroupTab:SCREEN-VALUE = STRING(tabno).
  RUN createWidgets.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadImages wMiscFlds 
PROCEDURE loadImages :
/*------------------------------------------------------------------------------
  Purpose:     Load Button Images
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      ldummy = btnPointer:LOAD-IMAGE-UP("images/wp_up",29,253,28,28)
      ldummy = btnComboBox:LOAD-IMAGE-UP("images/wp_up",1,141,28,28)
      ldummy = btnEditor:LOAD-IMAGE-UP("images/wp_up",29,113,28,28)
      ldummy = btnFillIn:LOAD-IMAGE-UP("images/wp_up",29,141,28,28)
      ldummy = btnRadioSet:LOAD-IMAGE-UP("images/wp_up",1,57,28,28)
      ldummy = btnSelectionList:LOAD-IMAGE-UP("images/wp_up",1,113,28,28)
      ldummy = btnSlider:LOAD-IMAGE-UP("images/wp_up",1,85,28,28)
      ldummy = btnRectangle:LOAD-IMAGE-UP("images/wp_up",1,29,28,28)
      ldummy = btnText:LOAD-IMAGE-UP("images/wp_up",1,169,28,28)
      ldummy = btnToggleBox:LOAD-IMAGE-UP("images/wp_up",29,57,28,28)
      ldummy = btnPointer:LOAD-IMAGE-INSENSITIVE("images/wp_down",29,253,28,28)
      ldummy = btnComboBox:LOAD-IMAGE-INSENSITIVE("images/wp_down",1,141,28,28)
      ldummy = btnEditor:LOAD-IMAGE-INSENSITIVE("images/wp_down",29,113,28,28)
      ldummy = btnFillIn:LOAD-IMAGE-INSENSITIVE("images/wp_down",29,141,28,28)
      ldummy = btnRadioSet:LOAD-IMAGE-INSENSITIVE("images/wp_down",1,57,28,28)
      ldummy = btnSelectionList:LOAD-IMAGE-INSENSITIVE("images/wp_down",1,113,28,28)
      ldummy = btnSlider:LOAD-IMAGE-INSENSITIVE("images/wp_down",1,85,28,28)
      ldummy = btnRectangle:LOAD-IMAGE-INSENSITIVE("images/wp_down",1,29,28,28)
      ldummy = btnText:LOAD-IMAGE-INSENSITIVE("images/wp_down",1,169,28,28)
      ldummy = btnToggleBox:LOAD-IMAGE-INSENSITIVE("images/wp_down",29,57,28,28).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadWidgetData wMiscFlds 
PROCEDURE loadWidgetData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE groupList AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  IF NOT CAN-FIND(FIRST mfgroup) THEN RETURN.
  FOR EACH mfgroup NO-LOCK:
    groupList = groupList + (IF groupList NE "" THEN "," ELSE "") + mfgroup.mfgroup_data.
  END.

  OUTPUT TO VALUE("users/" + USERID("NOSWEAT") + "/mfgroup.dat").
  EXPORT groupList.
  FOR EACH mfgroup:
    EXPORT mfgroup.mfgroup_tabs.
  END.
  OUTPUT CLOSE.

  OUTPUT TO VALUE("users/" + USERID("NOSWEAT") + "/mfdata.dat").
  FOR EACH mfdata NO-LOCK:
    PUT UNFORMATTED mfdata.miscflds_data SKIP.
  END.
  OUTPUT CLOSE.

  EMPTY TEMP-TABLE tmfgroup.
  EMPTY TEMP-TABLE attrb.
  EMPTY TEMP-TABLE del-mfvalues.

  INPUT FROM VALUE("users/" + USERID("NOSWEAT") + "/mfdata.dat") NO-ECHO.
  REPEAT:
    CREATE attrb.
    IMPORT attrb.
 /* attrb.attr_name = "".
    IF CAN-DO("Text,rectangle",attrb.attr_type) THEN
    attrb.attr_name = attrb.attr_id.
    ELSE
    ASSIGN
      attrb.attr_name = IF attrb.attr_label EQ "" THEN attrb.attr_id
                        ELSE REPLACE(attrb.attr_label," ","_")
      attrb.attr_name = "Tab_" + STRING(attrb.attr_tab) + "_" + attrb.attr_name. */
  END.
  INPUT CLOSE.
  IF attrb.attr_type EQ "" THEN DELETE attrb.

  INPUT FROM VALUE("users/" + USERID("NOSWEAT") + "/mfgroup.dat").
  IMPORT cdummy.
  DO i = 1 TO NUM-ENTRIES(cdummy):
    IMPORT currentLabel.
    CREATE tmfgroup.
    ASSIGN
      tmfgroup.mfgroup_data = ENTRY(i,cdummy)
      tmfgroup.mfgroup_tabs = currentLabel.
  END.
  INPUT CLOSE.

  FIND LAST attrb NO-LOCK USE-INDEX si-attrb NO-ERROR.
  DO WITH FRAME {&FRAME-NAME}:
    lastID = IF AVAILABLE attrb THEN INTEGER(attrb.attr_id) ELSE 0 NO-ERROR.
    ASSIGN
      mfgroupList:LIST-ITEMS = IF cdummy NE "" THEN cdummy ELSE " "
      mfgroupList:SCREEN-VALUE =
          IF mfgroupList:NUM-ITEMS GT 1 THEN mfgroupList:ENTRY(2)
          ELSE mfgroupList:ENTRY(1).
  END.

  RUN createWidgets.
  APPLY "ENTRY" TO mfgroupList IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveObjects wMiscFlds 
PROCEDURE moveObjects :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE offset AS INTEGER NO-UNDO.

  ASSIGN
    offset = FRAME folderFrm:WIDTH-PIXELS
    FRAME folderFrm:WIDTH-PIXELS = FRAME {&FRAME-NAME}:WIDTH-PIXELS - 291
    FRAME folderFrm:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 80
    offset = FRAME folderFrm:WIDTH-PIXELS - offset
    gapField:X IN FRAME {&FRAME-NAME} = gapField:X + offset
    gapFieldLabel:X = gapFieldLabel:X + offset
    btnTest:X = btnTest:X + offset
    widgetRect:X = widgetRect:X + offset
    widgetLabel:X = widgetLabel:X + offset
    btnPointer:X = btnPointer:X + offset
    pointerLabel:X = pointerLabel:X + offset
    btnComboBox:X = btnComboBox:X + offset
    comboBoxLabel:X = comboBoxLabel:X + offset
    btnEditor:X = btnEditor:X + offset
    editorLabel:X = editorLabel:X + offset
    btnFillIn:X = btnFillIn:X + offset
    fillInLabel:X = fillInLabel:X + offset
    btnRadioSet:X = btnRadioSet:X + offset
    radioSetLabel:X = radioSetLabel:X + offset
    btnRectangle:X = btnRectangle:X + offset
    rectangleLabel:X = rectangleLabel:X + offset
    btnSelectionList:X = btnSelectionList:X + offset
    selectionListLabel:X = selectionListLabel:X + offset
    btnSlider:X = btnSlider:X + offset
    sliderLabel:X = sliderLabel:X + offset
    btnText:X = btnText:X + offset
    textLabel:X = textLabel:X + offset
    btnToggleBox:X = btnToggleBox:X + offset
    toggleBoxLabel:X = toggleBoxLabel:X + offset
    Rect-Main:WIDTH-PIXELS IN FRAME folderFrm = FRAME folderFrm:WIDTH-PIXELS - 6
    Rect-Main:HEIGHT-PIXELS = FRAME folderFrm:HEIGHT-PIXELS - 23
    Rect-Top:WIDTH-PIXELS = Rect-Main:WIDTH-PIXELS - 2
    Rect-Left:HEIGHT-PIXELS = Rect-Main:HEIGHT-PIXELS - 5
    Rect-Bottom:WIDTH-PIXELS = Rect-Main:WIDTH-PIXELS - 2
    Rect-Bottom:Y = Rect-Main:HEIGHT-PIXELS + 17
    Rect-Right:HEIGHT-PIXELS = Rect-Main:HEIGHT-PIXELS - 8
    Rect-Right:X = Rect-Main:WIDTH-PIXELS - 1
    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveTab wMiscFlds 
PROCEDURE moveTab :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER moveTab AS CHARACTER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    FIND tmfgroup EXCLUSIVE-LOCK WHERE tmfgroup.mfgroup_data EQ mfgroupList:SCREEN-VALUE.
    DO i = 1 TO tabLabels:NUM-ITEMS:
      IF tabLabels:IS-SELECTED(i) THEN LEAVE.
    END.
    ASSIGN
      j = IF moveTab EQ "Down" THEN i + 1 ELSE i - 1
      ldummy = IF moveTab EQ "Down" THEN tabLabels:INSERT(tabLabels:SCREEN-VALUE,j + 1)
                                    ELSE tabLabels:INSERT(tabLabels:SCREEN-VALUE,j)
      ldummy = IF moveTab EQ "Down" THEN tabLabels:DELETE(i)
                                    ELSE tabLabels:DELETE(i + 1)
      tmfgroup.mfgroup_tabs = tabLabels:LIST-ITEMS
      tabLabels:SCREEN-VALUE = tabLabels:ENTRY(j).
    FOR EACH attrb EXCLUSIVE-LOCK
        WHERE attrb.attr_mfgroup EQ mfgroupList:SCREEN-VALUE
          AND attrb.attr_tab = i:
      attrb.attr_tab = 0.
    END.
    FOR EACH attrb EXCLUSIVE-LOCK
        WHERE attrb.attr_mfgroup EQ mfgroupList:SCREEN-VALUE
          AND attrb.attr_tab = j:
      attrb.attr_tab = i.
    END.
    FOR EACH attrb EXCLUSIVE-LOCK
        WHERE attrb.attr_mfgroup EQ mfgroupList:SCREEN-VALUE
          AND attrb.attr_tab = 0:
      attrb.attr_tab = j.
    END.
  END.
  APPLY "VALUE-CHANGED" TO tabLabels IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveWidget wMiscFlds 
PROCEDURE moveWidget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
    currentWidget = FRAME folderFrm:HANDLE
    currentWidget = currentWidget:FIRST-CHILD
    currentWidget = currentWidget:FIRST-CHILD.
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
  DISABLE btnCut btnCopy WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newWidget wMiscFlds 
PROCEDURE newWidget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  CREATE attrb.
  RUN setAttrID.
  ASSIGN
    attrb.attr_mfgroup = mfgroupList:SCREEN-VALUE IN FRAME {&FRAME-NAME}
    attrb.attr_tab = INTEGER(mfgroupTab:SCREEN-VALUE IN FRAME {&FRAME-NAME})
    attrb.attr_type = currentLabel
    attrb.attr_label = IF currentLabel = "RECTANGLE" THEN ""
                       ELSE "NEW " + currentLabel
    attrb.attr_values = IF currentLabel = "RADIO-SET" THEN "Empty,0"
                   ELSE IF currentLabel = "RECTANGLE" THEN ""
                   ELSE IF currentLabel = "SLIDER" THEN "0,100"
                   ELSE IF currentLabel = "TEXT" THEN ""
                   ELSE "Empty"
    attrb.attr_default = IF CAN-DO("RADIO-SET,SLIDER",currentLabel) THEN "0"
                    ELSE IF currentLabel = "RECTANGLE" THEN ""
                    ELSE IF currentLabel = "TEXT" THEN ""
                    ELSE IF currentLabel = "TOGGLE-BOX" THEN "no"
                    ELSE "Empty"
    attrb.attr_x = LAST-EVENT:X
    attrb.attr_y = LAST-EVENT:Y
    attrb.attr_width = IF currentLabel = "SLIDER" THEN 70 ELSE 105
    attrb.attr_height = IF currentLabel = "SLIDER" THEN 31 ELSE 21
    attrb.attr_settings = IF currentLabel = "EDITOR" THEN "no,yes"
                     ELSE IF currentLabel = "FILL-IN" THEN "X(256)"
                     ELSE IF currentLabel = "COMBO-BOX" THEN "X(256)"
                     ELSE IF currentLabel = "RADIO-SET" THEN "yes"
                     ELSE IF currentLabel = "SLIDER" THEN "yes"
                     ELSE IF currentLabel = "TEXT" THEN "4"
                     ELSE ""
    attrb.attr_datatype = IF currentLabel = "SLIDER" THEN "integer"
                     ELSE IF currentLabel = "TOGGLE-BOX" THEN "logical"
                     ELSE "character".
  RUN dynamicWidget.
  IF RETURN-VALUE = "CREATE-ERROR" THEN DELETE attrb.
  ELSE DO:
    dynWidget:SELECTED = YES.
    ENABLE btnCut btnCopy WITH FRAME {&FRAME-NAME}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pasteFromClipboard wMiscFlds 
PROCEDURE pasteFromClipboard :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH wtbl-clipboard:
    CREATE attrb.
    RUN setAttrID.
    ASSIGN
      attrb.attr_mfgroup = mfgroupList:SCREEN-VALUE IN FRAME {&FRAME-NAME}
      attrb.attr_tab = INTEGER(mfgroupTab:SCREEN-VALUE IN FRAME {&FRAME-NAME})
      attrb.attr_type = wtbl-clipboard.attr_type
      attrb.attr_label = wtbl-clipboard.attr_label
      attrb.attr_name = wtbl-clipboard.attr_name
      attrb.attr_values = wtbl-clipboard.attr_values
      attrb.attr_default = wtbl-clipboard.attr_default
      attrb.attr_x = wtbl-clipboard.attr_x
      attrb.attr_y = wtbl-clipboard.attr_y
      attrb.attr_height = wtbl-clipboard.attr_height
      attrb.attr_width = wtbl-clipboard.attr_width
      attrb.attr_settings = wtbl-clipboard.attr_settings
      attrb.attr_datatype = wtbl-clipboard.attr_datatype.
    RUN dynamicWidget.
    dynWidget:SELECTED = YES.
    ENABLE btnCut btnCopy WITH FRAME {&FRAME-NAME}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE propertySheet wMiscFlds 
PROCEDURE propertySheet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
    currentWidget = FRAME folderFrm:HANDLE
    currentWidget = currentWidget:FIRST-CHILD
    currentWidget = currentWidget:FIRST-CHILD.
  DO WHILE currentWidget NE ?:
    IF currentWidget:SELECTED THEN
    DO WITH FRAME {&FRAME-NAME}:
      RUN nosweat/mfdialog.w (ENTRY(1,currentWidget:PRIVATE-DATA,"|")).
      FIND attrb NO-LOCK
           WHERE ROWID(attrb) = TO-ROWID(ENTRY(1,currentWidget:PRIVATE-DATA,"|")).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE renameGroup wMiscFlds 
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
      FOR EACH attrb WHERE attrb.attr_mfgroup = mfgroupList:SCREEN-VALUE EXCLUSIVE-LOCK:
        attrb.attr_mfgroup = cdummy.
      END.
      DO i = mfgroupList:NUM-ITEMS TO 1 BY -1:
        IF mfgroupList:ENTRY(i) = mfgroupField:SCREEN-VALUE THEN
        currentLabel = "yes".
        IF mfgroupList:ENTRY(i) = mfgroupList:SCREEN-VALUE THEN DO:
          FIND tmfgroup WHERE tmfgroup.mfgroup_data EQ mfgroupList:ENTRY(i) EXCLUSIVE-LOCK.
          ASSIGN
            save_labels = tmfgroup.mfgroup_tabs
            ldummy = mfgroupList:DELETE(i).
          DELETE mfgroup.
        END.
        IF mfgroupList:ENTRY(i) = mfgroupField:SCREEN-VALUE THEN
        currentLabel = "yes".
      END.
      IF currentLabel NE "yes" THEN DO:
        CREATE tmfgroup.
        ASSIGN
          tmfgroup.mfgroup_data = mfgroupField:SCREEN-VALUE
          tmfgroup.mfgroup_tabs = save_labels
          ldummy = mfgroupList:ADD-LAST(mfgroupField:SCREEN-VALUE)
          currentLabel = "".
      END.
    END.
    ELSE cdummy = mfgroupList:SCREEN-VALUE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE renameTab wMiscFlds 
PROCEDURE renameTab :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    FIND tmfgroup EXCLUSIVE-LOCK WHERE tmfgroup.mfgroup_data EQ mfgroupList:SCREEN-VALUE.
    ASSIGN
      tabField:SCREEN-VALUE = tabField:SCREEN-VALUE + "|" + tabOrder
      tabField
      ldummy = tabLabels:REPLACE(tabField:SCREEN-VALUE,tabLabels:SCREEN-VALUE)
      tabLabels:SCREEN-VALUE = tabField:SCREEN-VALUE
      tmfgroup.mfgroup_tabs = tabLabels:LIST-ITEMS.
    RUN createTabs.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resizeWidget wMiscFlds 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveLayout wMiscFlds 
PROCEDURE saveLayout :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE textstring AS CHARACTER NO-UNDO.

  IF CAN-FIND(FIRST del-mfvalues) THEN DO:
    ldummy = NO.
    MESSAGE "Attribute Value Records are marked for DELETION, Continue? (yes/no)"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ldummy.
    IF ldummy THEN
    FOR EACH del-mfvalues:
      FIND mfvalues WHERE RECID(mfvalues) EQ del-mfvalues.mfvalues_recid EXCLUSIVE-LOCK.
      DELETE mfvalues.
      DELETE del-mfvalues.
    END.
  END.

  OUTPUT TO VALUE("users/" + USERID("NOSWEAT") + "/mfgroup.dat").
  EXPORT mfgroupList:LIST-ITEMS IN FRAME {&FRAME-NAME}.
  DO i = 1 TO mfgroupList:NUM-ITEMS WITH FRAME {&FRAME-NAME}:
    FIND tmfgroup WHERE tmfgroup.mfgroup_data EQ ENTRY(i,mfgroupList:LIST-ITEMS) NO-LOCK NO-ERROR.
    IF AVAILABLE tmfgroup THEN
    EXPORT tmfgroup.mfgroup_tabs.
    ELSE
    EXPORT " ".
  END.
  OUTPUT CLOSE.

  DELETE FROM mfgroup.
  FOR EACH tmfgroup:
    CREATE mfgroup.
    BUFFER-COPY tmfgroup TO mfgroup.
  END.

  OUTPUT TO VALUE("users/" + USERID("NOSWEAT") + "/miscflds.dat").
  FOR EACH attrb:
 /* IF NOT CAN-DO('text,rectangle',attrb.ATTR_type) THEN
    attrb.attr_enabled = YES. */
    EXPORT attrb.attr_mfgroup.
    EXPORT attrb.
  END.
  OUTPUT CLOSE.
  
  DELETE FROM mfdata.
  INPUT FROM VALUE("users/" + USERID("NOSWEAT") + "/miscflds.dat").
  REPEAT:
    IMPORT textstring.
    CREATE mfdata.
    mfdata.mfgroup_data = textstring.
    IMPORT UNFORMATTED mfdata.miscflds_data.
  END.
  INPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selectNewWidgetType wMiscFlds 
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
      pointerLabel:BGCOLOR = 15
      comboBoxLabel:BGCOLOR = 15
      editorLabel:BGCOLOR = 15
      fillInLabel:BGCOLOR = 15
      radioSetLabel:BGCOLOR = 15
      rectangleLabel:BGCOLOR = 15
      selectionListLabel:BGCOLOR = 15
      sliderLabel:BGCOLOR = 15
      textLabel:BGCOLOR = 15
      toggleBoxLabel:BGCOLOR = 15.
    ENABLE {&LIST-3}.

    CASE newWidgetType:
      WHEN "" THEN DO:
        pointerLabel:BGCOLOR = 7.
        DISABLE btnpointer.
      END.
&scoped-define widgettype comboBox
      WHEN "{&widgettype}" THEN DO:
        {&widgettype}Label:BGCOLOR = 7.
        DISABLE btn{&widgettype}.
      END.
&scoped-define widgettype editor
      WHEN "{&widgettype}" THEN DO:
        {&widgettype}Label:BGCOLOR = 7.
        DISABLE btn{&widgettype}.
      END.
&scoped-define widgettype fillIn
      WHEN "{&widgettype}" THEN DO:
        {&widgettype}Label:BGCOLOR = 7.
        DISABLE btn{&widgettype}.
      END.
&scoped-define widgettype radioSet
      WHEN "{&widgettype}" THEN DO:
        {&widgettype}Label:BGCOLOR = 7.
        DISABLE btn{&widgettype}.
      END.
&scoped-define widgettype rectangle
      WHEN "{&widgettype}" THEN DO:
        {&widgettype}Label:BGCOLOR = 7.
        DISABLE btn{&widgettype}.
      END.
&scoped-define widgettype selectionList
      WHEN "{&widgettype}" THEN DO:
        {&widgettype}Label:BGCOLOR = 7.
        DISABLE btn{&widgettype}.
      END.
&scoped-define widgettype slider
      WHEN "{&widgettype}" THEN DO:
        {&widgettype}Label:BGCOLOR = 7.
        DISABLE btn{&widgettype}.
      END.
&scoped-define widgettype text
      WHEN "{&widgettype}" THEN DO:
        {&widgettype}Label:BGCOLOR = 7.
        DISABLE btn{&widgettype}.
      END.
&scoped-define widgettype toggleBox
      WHEN "{&widgettype}" THEN DO:
        {&widgettype}Label:BGCOLOR = 7.
        DISABLE btn{&widgettype}.
      END.
    END CASE.
  END.

  ASSIGN
    ldummy = FRAME folderFrm:LOAD-MOUSE-POINTER(mousePointer)
    currentLabel = newWidgetType.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selectWidget wMiscFlds 
PROCEDURE selectWidget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF FRAME folderFrm:NUM-SELECTED-WIDGETS = 0 THEN
  ENABLE btnProperty btnCut btnCopy WITH FRAME {&FRAME-NAME}.
  ELSE
  DISABLE btnProperty WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus wMiscFlds 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setAttrID wMiscFlds 
PROCEDURE setAttrID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO i = 1 TO lastID:
     IF NOT CAN-FIND(attrb WHERE attrb.attr_id EQ STRING(i,'99999')) THEN LEAVE.
  END.
  IF i GT lastID THEN lastID = i.
  attrb.attr_id = STRING(i,'99999').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setAttrID-buffer wMiscFlds 
PROCEDURE setAttrID-buffer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO i = 1 TO lastID:
     IF NOT CAN-FIND(attrb WHERE attrb.attr_id EQ STRING(i,'99999')) THEN LEAVE.
  END.
  IF i GT lastID THEN lastID = i.
  b-attrb.attr_id = STRING(i,'99999').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setTabOrder wMiscFlds 
PROCEDURE setTabOrder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      ldummy = btnAddGroup:MOVE-AFTER(mfgroupList:HANDLE)
      ldummy = btnRenameGroup:MOVE-AFTER(btnAddGroup:HANDLE)
      ldummy = btnDeleteGroup:MOVE-AFTER(btnRenameGroup:HANDLE)
      ldummy = mfgroupTab:MOVE-AFTER(btnExit:HANDLE)
      ldummy = btnNextTab:MOVE-AFTER(mfgroupTab:HANDLE)
      ldummy = btnPrevTab:MOVE-AFTER(btnNextTab:HANDLE)
      ldummy = tabLabels:MOVE-AFTER(btnPrevTab:HANDLE)
      ldummy = btnAddTab:MOVE-AFTER(tabLabels:HANDLE)
      ldummy = btnRenameTab:MOVE-AFTER(btnAddTab:HANDLE)
      ldummy = btnDeleteTab:MOVE-AFTER(btnRenameTab:HANDLE)
      ldummy = btnDownTab:MOVE-AFTER(btnDeleteTab:HANDLE)
      ldummy = btnUpTab:MOVE-AFTER(btnDownTab:HANDLE).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tabOrder wMiscFlds 
PROCEDURE tabOrder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  tabOrder = IF NUM-ENTRIES(tabLabels:SCREEN-VALUE IN FRAME {&FRAME-NAME},"|") GT 1 THEN
             ENTRY(2,tabLabels:SCREEN-VALUE,"|") ELSE 'Default'.
  RUN nosweat/mftaborder.w (FRAME folderFrm:HANDLE,
                            ENTRY(1,tabLabels:SCREEN-VALUE,"|"),
                            INPUT-OUTPUT tabOrder).
  tabLabels:REPLACE(ENTRY(1,tabLabels:SCREEN-VALUE,"|") + "|" + tabOrder,
                    tabLabels:SCREEN-VALUE).
  FIND tmfgroup EXCLUSIVE-LOCK WHERE tmfgroup.mfgroup_data EQ mfgroupList:SCREEN-VALUE.
  tmfgroup.mfgroup_tabs = tabLabels:LIST-ITEMS.
  RUN createWidgets.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateAttrb wMiscFlds 
PROCEDURE updateAttrb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND attrb
      WHERE ROWID(attrb) = TO-ROWID(ENTRY(1,currentWidget:PRIVATE-DATA,"|"))
          EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAILABLE attrb THEN DO:
    CREATE attrb.
    RUN setAttrID.
  END.
  ASSIGN
    attrb.attr_mfgroup = mfgroupList:SCREEN-VALUE IN FRAME {&FRAME-NAME}
    attrb.attr_tab = INTEGER(ENTRY(2,currentWidget:PRIVATE-DATA,"|"))
    attrb.attr_type = currentWidget:TYPE
    attrb.attr_label = IF currentWidget:TYPE EQ "RECTANGLE" THEN ""
      ELSE IF currentWidget:TYPE EQ "TOGGLE-BOX" THEN currentWidget:LABEL
      ELSE IF currentWidget:TYPE EQ "TEXT" THEN currentWidget:SCREEN-VALUE
      ELSE SUBSTR(currentWidget:LABEL,1,LENGTH(currentWidget:LABEL) - 1)
    attrb.attr_values =
           IF CAN-DO("COMBO-BOX,SELECTION-LIST",currentWidget:TYPE) THEN currentWidget:LIST-ITEMS
      ELSE IF currentWidget:TYPE EQ "RADIO-SET" THEN currentWidget:RADIO-BUTTONS
      ELSE IF currentWidget:TYPE EQ "SLIDER" THEN STRING(currentWidget:MIN-VALUE) + "," + STRING(currentWidget:MAX-VALUE)
      ELSE ""
    attrb.attr_default = IF currentWidget:TYPE EQ "RECTANGLE" THEN ""
                    ELSE IF currentWidget:TYPE EQ "TEXT" THEN ""
                    ELSE currentWidget:SCREEN-VALUE
    attrb.attr_x = currentWidget:X
    attrb.attr_y = currentWidget:Y
    attrb.attr_height = currentWidget:HEIGHT-PIXELS
    attrb.attr_width = currentWidget:WIDTH-PIXELS
    attrb.attr_settings =
           IF attrb.attr_type EQ "EDITOR" THEN
           STRING(currentWidget:SCROLLBAR-HORIZONTAL) + "," + STRING(currentWidget:SCROLLBAR-VERTICAL)
      ELSE IF attrb.attr_type EQ "FILL-IN" THEN currentWidget:FORMAT
      ELSE IF attrb.attr_type EQ "COMBO-BOX" THEN currentWidget:FORMAT
      ELSE IF attrb.attr_type EQ "RADIO-SET" THEN STRING(currentWidget:HORIZONTAL)
      ELSE IF attrb.attr_type EQ "SLIDER" THEN STRING(currentWidget:HORIZONTAL)
      ELSE IF attrb.attr_type EQ "TEXT" THEN STRING(currentWidget:FONT)
      ELSE ""
    attrb.attr_datatype = currentWidget:DATA-TYPE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE winReSize wMiscFlds 
PROCEDURE winReSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE offSet AS INTEGER NO-UNDO.

  &IF DEFINED(UIB_is_Running) EQ 0 &THEN
  MESSAGE "Not Running in App. Builder Mode" VIEW-AS ALERT-BOX.
  &ENDIF

  {&WINDOW-NAME}:WINDOW-STATE = 1.
  offSet = IF {&WINDOW-NAME}:HEIGHT-PIXELS GT 600 THEN 50 ELSE 0.
  IF {&WINDOW-NAME}:HEIGHT-PIXELS LT 600 THEN {&WINDOW-NAME}:HEIGHT-PIXELS = 600.
  IF {&WINDOW-NAME}:WIDTH-PIXELS LT 800 THEN {&WINDOW-NAME}:WIDTH-PIXELS = 800.
  ASSIGN
    {&WINDOW-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS - offSet
    {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
    FRAME {&FRAME-NAME}:WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
    FRAME {&FRAME-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
    FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
    FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
    FRAME folderFrm:GRID-FACTOR-VERTICAL = 5
    FRAME folderFrm:GRID-FACTOR-HORIZONTAL = 5
    FRAME folderFrm:GRID-UNIT-WIDTH-PIXELS = 5
    FRAME folderFrm:GRID-UNIT-HEIGHT-PIXELS = 5
    FRAME folderFrm:GRID-SNAP = YES
    FRAME folderFrm:GRID-VISIBLE = YES.
  RUN moveObjects.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

