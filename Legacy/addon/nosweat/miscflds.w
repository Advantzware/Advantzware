&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File:              mfdata.w

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

DEFINE VARIABLE tab-image AS WIDGET-HANDLE EXTENT 6 NO-UNDO.
DEFINE VARIABLE tab-label AS WIDGET-HANDLE EXTENT 6 NO-UNDO.
DEFINE VARIABLE label-widget AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE dyn-widget AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE save-widget AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE current-widget AS WIDGET-HANDLE NO-UNDO.

DEFINE VARIABLE current-label AS CHARACTER NO-UNDO.
DEFINE VARIABLE cdummy AS CHARACTER NO-UNDO.

DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE new-mfgroup AS LOGICAL NO-UNDO.
DEFINE VARIABLE new-tab AS LOGICAL NO-UNDO.
DEFINE VARIABLE leftright AS LOGICAL NO-UNDO.

DEFINE VARIABLE current-tab AS INTEGER NO-UNDO.
DEFINE VARIABLE last_id AS INTEGER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

/* trigger code attached to each created dynamic widget */

&scoped-define trigger-code ~
TRIGGERS: ~
  ON SELECTION ~
    PERSISTENT RUN Selection_Widget IN THIS-PROCEDURE. ~
  ON DESELECTION ~
    PERSISTENT RUN Deselection_Widget IN THIS-PROCEDURE. ~
  ON END-MOVE ~
    PERSISTENT RUN Move_Widget IN THIS-PROCEDURE. ~
  ON MOUSE-SELECT-DBLCLICK, RETURN ~
    PERSISTENT RUN Property_Sheet IN THIS-PROCEDURE. ~
  ON DELETE-CHARACTER  ~
    PERSISTENT RUN Cut_Widgets IN THIS-PROCEDURE. ~
  ON END-RESIZE ~
    PERSISTENT RUN Resize_Widget IN THIS-PROCEDURE (dyn-widget:HANDLE). ~
END TRIGGERS.

/*
/* to delete a selected widget if focus is outside the design frame folder-frm */
ON DELETE-CHARACTER ANYWHERE
DO:
  RUN Cut_Widgets.
END.
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-5 RECT-4 RECT-8 RECT-6 RECT-2 ~
RECT-3 mfgroup-list Btn_Property Btn_Cut Btn_Copy Btn_Paste Btn_Restore ~
Btn_Save Btn_Exit Btn_Add_Group Btn_Rename_Group Btn_Delete_Group Btn_Test ~
mfgroup-tab Btn_Next_Tab Btn_Combo-Box Btn_Prev_Tab Btn_Editor tab-labels ~
Btn_Fill-In Btn_Radio-Set Btn_Selection-List Btn_Slider Btn_Add_Tab ~
Btn_Rename_Tab Btn_Text Btn_Delete_Tab Btn_Toggle-Box Btn_Down_Tab ~
Btn_Up_Tab 
&Scoped-Define DISPLAYED-OBJECTS mfgroup-list mfgroup-tab pointer-label ~
combo-box-label editor-label tab-labels fill-in-label radio-set-label ~
selection-list-label slider-label text-label toggle-box-label 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Btn_Rename_Group Btn_Delete_Group mfgroup-tab ~
Btn_Pointer Btn_Next_Tab Btn_Combo-Box Btn_Prev_Tab Btn_Editor tab-labels ~
Btn_Fill-In Btn_Radio-Set Btn_Selection-List Btn_Slider Btn_Add_Tab ~
Btn_Rename_Tab Btn_Text Btn_Delete_Tab Btn_Toggle-Box Btn_Down_Tab ~
Btn_Up_Tab 
&Scoped-define List-2 pointer-label combo-box-label editor-label ~
fill-in-label radio-set-label selection-list-label slider-label text-label ~
toggle-box-label 
&Scoped-define List-3 Btn_Pointer Btn_Combo-Box Btn_Editor Btn_Fill-In ~
Btn_Radio-Set Btn_Selection-List Btn_Slider Btn_Text Btn_Toggle-Box 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

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
       MENU-ITEM m_Add_Group    LABEL "&Add Group"    
       MENU-ITEM m_Change_Group LABEL "&Rename Group" 
       MENU-ITEM m_Delete_Group LABEL "&Delete Group" .

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
       MENU-ITEM m_Top_Sdies    LABEL "&Top Sdies"    
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
DEFINE BUTTON Btn_Add_Group 
     IMAGE-UP FILE "images/new-au":U
     IMAGE-DOWN FILE "images/new-ad":U
     LABEL "Add Group" 
     SIZE 5.6 BY 1.38 TOOLTIP "Add New Group".

DEFINE BUTTON Btn_Add_Tab 
     LABEL "Add" 
     SIZE 11.2 BY 1.38 TOOLTIP "Add New Tab (Page)"
     FONT 4.

DEFINE BUTTON Btn_Combo-Box 
     LABEL "Combo Box" 
     SIZE 7 BY 1.67 TOOLTIP "Combo Box"
     FONT 4.

DEFINE BUTTON Btn_Copy 
     IMAGE-UP FILE "images/copy-u":U
     IMAGE-DOWN FILE "images/copy-d":U
     IMAGE-INSENSITIVE FILE "images/copy-i":U
     LABEL "Copy" 
     SIZE 8.4 BY 2 TOOLTIP "Copy Selections to Clipboard".

DEFINE BUTTON Btn_Cut 
     IMAGE-UP FILE "images/cut-u":U
     IMAGE-DOWN FILE "images/cut-d":U
     IMAGE-INSENSITIVE FILE "images/cut-i":U
     LABEL "Cut" 
     SIZE 8.4 BY 2 TOOLTIP "Cut Selections to Clipboard"
     FONT 4.

DEFINE BUTTON Btn_Delete_Group 
     IMAGE-UP FILE "images/del-au":U
     IMAGE-DOWN FILE "images/del-ad":U
     IMAGE-INSENSITIVE FILE "images/del-ai":U
     LABEL "Delete Group" 
     SIZE 5.6 BY 1.38 TOOLTIP "Delete Current Group".

DEFINE BUTTON Btn_Delete_Tab 
     LABEL "Delete" 
     SIZE 11.2 BY 1.38 TOOLTIP "Delete Current Tab (Page)"
     FONT 4.

DEFINE BUTTON Btn_Down_Tab 
     LABEL "Move Dn" 
     SIZE 11.2 BY 1.38 TOOLTIP "Move Current Tab (Page) Down (Right)"
     FONT 4.

DEFINE BUTTON Btn_Editor 
     LABEL "Editor" 
     SIZE 7 BY 1.67 TOOLTIP "Editor"
     FONT 4.

DEFINE BUTTON Btn_Exit 
     IMAGE-UP FILE "images/exit-au":U
     LABEL "E&xit" 
     SIZE 8.4 BY 2 TOOLTIP "Exit"
     FONT 4.

DEFINE BUTTON Btn_Fill-In 
     LABEL "Fill In" 
     SIZE 7 BY 1.67 TOOLTIP "Fill In"
     FONT 4.

DEFINE BUTTON Btn_Next_Tab 
     IMAGE-UP FILE "images/pvforw":U
     IMAGE-DOWN FILE "images/pvforwd":U
     IMAGE-INSENSITIVE FILE "images/pvforwx":U
     LABEL "Next Tab" 
     SIZE 7 BY 1.38 TOOLTIP "Change to Next Tab (Page)"
     FONT 4.

DEFINE BUTTON Btn_Paste 
     IMAGE-UP FILE "images/paste-u":U
     IMAGE-DOWN FILE "images/paste-d":U
     IMAGE-INSENSITIVE FILE "images/paste-i":U
     LABEL "Paste" 
     SIZE 8.4 BY 2 TOOLTIP "Paste from Clipboard".

DEFINE BUTTON Btn_Pointer 
     LABEL "Pointer" 
     SIZE 7 BY 1.67 TOOLTIP "Pointer".

DEFINE BUTTON Btn_Prev_Tab 
     IMAGE-UP FILE "images/pvback":U
     IMAGE-DOWN FILE "images/pvbackd":U
     IMAGE-INSENSITIVE FILE "images/pvbackx":U
     LABEL "Previous Tab" 
     SIZE 7 BY 1.38 TOOLTIP "Change to Previous Tab (Page)"
     FONT 4.

DEFINE BUTTON Btn_Property 
     IMAGE-UP FILE "images/props-u":U
     IMAGE-DOWN FILE "images/props-d":U
     IMAGE-INSENSITIVE FILE "images/props-i":U
     LABEL "Property" 
     SIZE 8.4 BY 2 TOOLTIP "Current SelectionProperty Sheet"
     FONT 4.

DEFINE BUTTON Btn_Radio-Set 
     LABEL "Radio Set" 
     SIZE 7 BY 1.67 TOOLTIP "Radio Set"
     FONT 4.

DEFINE BUTTON Btn_Rename_Group 
     IMAGE-UP FILE "images/r-sort":U
     IMAGE-DOWN FILE "images/r-sortd":U
     LABEL "Rename Group" 
     SIZE 5.6 BY 1.38 TOOLTIP "Rename Current Group".

DEFINE BUTTON Btn_Rename_Tab 
     LABEL "Rename" 
     SIZE 11.2 BY 1.38 TOOLTIP "Rename Current Tab (Page)"
     FONT 4.

DEFINE BUTTON Btn_Restore 
     IMAGE-UP FILE "images/reset-au":U
     LABEL "Restore" 
     SIZE 8.4 BY 2 TOOLTIP "Restore"
     FONT 4.

DEFINE BUTTON Btn_Save 
     IMAGE-UP FILE "images/save-u":U
     IMAGE-DOWN FILE "images/save-d":U
     LABEL "Save" 
     SIZE 8.4 BY 2 TOOLTIP "Save"
     FONT 4.

DEFINE BUTTON Btn_Selection-List 
     LABEL "Selection List" 
     SIZE 7 BY 1.67 TOOLTIP "Selection List"
     FONT 4.

DEFINE BUTTON Btn_Slider 
     LABEL "Slider" 
     SIZE 7 BY 1.67 TOOLTIP "Slider"
     FONT 4.

DEFINE BUTTON Btn_Test 
     LABEL "Test" 
     SIZE 13.8 BY 1.24.

DEFINE BUTTON Btn_Text 
     LABEL "Text" 
     SIZE 7 BY 1.67 TOOLTIP "Text"
     FONT 4.

DEFINE BUTTON Btn_Toggle-Box 
     LABEL "Toggle Box" 
     SIZE 7 BY 1.67 TOOLTIP "Toggle Box"
     FONT 4.

DEFINE BUTTON Btn_Up_Tab 
     LABEL "Move Up" 
     SIZE 11.2 BY 1.38 TOOLTIP "Move Current Tab (Page) Up (Left)"
     FONT 4.

DEFINE VARIABLE mfgroup-list AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX SORT INNER-LINES 20
     LIST-ITEMS "","" 
     SIZE 48 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE mfgroup-tab AS INTEGER FORMAT "9":U INITIAL 1 
     VIEW-AS COMBO-BOX INNER-LINES 1
     LIST-ITEMS "1" 
     SIZE 7 BY 1 TOOLTIP "Change Current Tab (Page)"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE combo-box-label AS CHARACTER FORMAT "X(256)":U INITIAL " Combo Box" 
     VIEW-AS FILL-IN 
     SIZE 16.8 BY 1.67
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE editor-label AS CHARACTER FORMAT "X(256)":U INITIAL " Editor" 
     VIEW-AS FILL-IN 
     SIZE 16.8 BY 1.67
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE fill-in-label AS CHARACTER FORMAT "X(256)":U INITIAL " Fill In" 
     VIEW-AS FILL-IN 
     SIZE 16.8 BY 1.67
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE gap-field AS INTEGER FORMAT "z9":U INITIAL 0 
     LABEL "Gap" 
     VIEW-AS FILL-IN 
     SIZE 5.6 BY 1.24
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE mfgroup-field AS CHARACTER FORMAT "X(256)":U 
     LABEL "Group" 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE pointer-label AS CHARACTER FORMAT "X(256)":U INITIAL " Pointer" 
     VIEW-AS FILL-IN 
     SIZE 16.8 BY 1.67
     BGCOLOR 7 FONT 4 NO-UNDO.

DEFINE VARIABLE radio-set-label AS CHARACTER FORMAT "X(256)":U INITIAL " Radio Set" 
     VIEW-AS FILL-IN 
     SIZE 16.8 BY 1.67
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE selection-list-label AS CHARACTER FORMAT "X(256)":U INITIAL " Selection List" 
     VIEW-AS FILL-IN 
     SIZE 16.8 BY 1.67
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE slider-label AS CHARACTER FORMAT "X(256)":U INITIAL " Slider" 
     VIEW-AS FILL-IN 
     SIZE 16.8 BY 1.67
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE tab-field AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 23.8 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE text-label AS CHARACTER FORMAT "X(256)":U INITIAL " Text" 
     VIEW-AS FILL-IN 
     SIZE 16.8 BY 1.67
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE toggle-box-label AS CHARACTER FORMAT "X(256)":U INITIAL " Toggle Box" 
     VIEW-AS FILL-IN 
     SIZE 16.8 BY 1.67
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  
     SIZE 147 BY 3.14
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
     SIZE 26.6 BY 5.38
     BGCOLOR 4 .

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 1 GRAPHIC-EDGE  
     SIZE 26.6 BY 13.33
     BGCOLOR 4 .

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 1 GRAPHIC-EDGE  
     SIZE 26.6 BY 19
     BGCOLOR 15 FGCOLOR 0 .

DEFINE VARIABLE tab-labels AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG 
     SIZE 23.8 BY 5 TOOLTIP "Change Current Tab (Page)"
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE Rect-Bottom
     EDGE-PIXELS 0  
     SIZE 89.8 BY .19
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
     SIZE 89.8 BY .19
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     mfgroup-list AT ROW 1.62 COL 9 COLON-ALIGNED HELP
          "Select Group Name" NO-LABEL
     Btn_Property AT ROW 1.62 COL 61.2 HELP
          "Access PROPERTY SHEET of Currently Selected Widget"
     Btn_Cut AT ROW 1.62 COL 75.2 HELP
          "CUT Selections"
     Btn_Copy AT ROW 1.62 COL 86.4 HELP
          "COPY Selections"
     Btn_Paste AT ROW 1.62 COL 97.6 HELP
          "PASTE Selections"
     Btn_Restore AT ROW 1.62 COL 113 HELP
          "RESTORE ALL Widgets"
     Btn_Save AT ROW 1.62 COL 124.2 HELP
          "SAVE Current Screen Layout"
     Btn_Exit AT ROW 1.62 COL 139 HELP
          "CLOSE Design Layout Window"
     Btn_Add_Group AT ROW 3 COL 3.8 HELP
          "ADD GROU[ Name"
     Btn_Rename_Group AT ROW 3 COL 20.4 HELP
          "CHANGE GROU[ Name"
     Btn_Delete_Group AT ROW 3 COL 40.2 HELP
          "DELETE GROU[ Name"
     mfgroup-field AT ROW 4.67 COL 9 COLON-ALIGNED HELP
          "Enter Group Name"
     Btn_Test AT ROW 4.67 COL 122.8
     gap-field AT ROW 4.67 COL 141.8 COLON-ALIGNED HELP
          "Enter Gap Amount in Pixels"
     mfgroup-tab AT ROW 6.38 COL 18.6 COLON-ALIGNED HELP
          "Select Tab Number" NO-LABEL
     Btn_Pointer AT ROW 7 COL 124.2 HELP
          "Restore Mouse Cursor to ARROW POINTER"
     pointer-label AT ROW 7 COL 129.2 COLON-ALIGNED NO-LABEL
     Btn_Next_Tab AT ROW 8 COL 20.6 HELP
          "Next Tab"
     Btn_Combo-Box AT ROW 9 COL 124.2 HELP
          "Create New COMBO-BOX"
     combo-box-label AT ROW 9 COL 129.2 COLON-ALIGNED NO-LABEL
     Btn_Prev_Tab AT ROW 9.67 COL 20.6 HELP
          "Previous tab"
     Btn_Editor AT ROW 11.05 COL 124.2 HELP
          "Create New EDITOR"
     editor-label AT ROW 11.05 COL 129.2 COLON-ALIGNED NO-LABEL
     tab-labels AT ROW 12.95 COL 3.8 HELP
          "Select tab Label" NO-LABEL
     Btn_Fill-In AT ROW 12.95 COL 124.2 HELP
          "Create New FILL-IN"
     fill-in-label AT ROW 12.95 COL 129.2 COLON-ALIGNED NO-LABEL
     Btn_Radio-Set AT ROW 15.05 COL 124.2 HELP
          "Create New RADIO-SET"
     radio-set-label AT ROW 15.05 COL 129.2 COLON-ALIGNED NO-LABEL
     Btn_Selection-List AT ROW 17 COL 124.2 HELP
          "Create New SELECTION-LIST"
     selection-list-label AT ROW 17 COL 129.2 COLON-ALIGNED NO-LABEL
     tab-field AT ROW 18.33 COL 1.8 COLON-ALIGNED HELP
          "Enter Tab Label" NO-LABEL
     Btn_Slider AT ROW 19 COL 124.2 HELP
          "Create New SLIDER"
     slider-label AT ROW 19 COL 129.2 COLON-ALIGNED NO-LABEL
     Btn_Add_Tab AT ROW 20 COL 3.8 HELP
          "ADD New Tab"
     Btn_Rename_Tab AT ROW 20 COL 16.4 HELP
          "RENAME Tab"
     Btn_Text AT ROW 21 COL 124.2 HELP
          "Create New TEXT"
     text-label AT ROW 21 COL 129.2 COLON-ALIGNED NO-LABEL
     Btn_Delete_Tab AT ROW 21.62 COL 9.4 HELP
          "DELETE Tab"
     Btn_Toggle-Box AT ROW 23 COL 124.2 HELP
          "Create New TOGGLE-BOX"
     toggle-box-label AT ROW 23 COL 129.2 COLON-ALIGNED NO-LABEL
     Btn_Down_Tab AT ROW 23.33 COL 3.8 HELP
          "Move Tab DOWN"
     Btn_Up_Tab AT ROW 23.33 COL 16.4 HELP
          "Move Tab UP"
     "Tab Labels" VIEW-AS TEXT
          SIZE 11.2 BY .76 AT ROW 11.95 COL 8.8
          BGCOLOR 4 FGCOLOR 15 FONT 4
.
/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     RECT-7 AT ROW 11.67 COL 2.4
     RECT-1 AT ROW 1.38 COL 2.4
     "Sheet" VIEW-AS TEXT
          SIZE 7 BY .76 AT ROW 3.67 COL 62.6
          BGCOLOR 8 FONT 4
     "Delete Group" VIEW-AS TEXT
          SIZE 12.6 BY .76 AT ROW 3.29 COL 45.8
          BGCOLOR 8 FONT 4
     "Rename Group" VIEW-AS TEXT
          SIZE 14 BY .76 AT ROW 3.29 COL 26.2
          BGCOLOR 8 FONT 4
     "Add Group" VIEW-AS TEXT
          SIZE 10.4 BY .76 AT ROW 3.29 COL 9.4
          BGCOLOR 8 FONT 4
     "Group:" VIEW-AS TEXT
          SIZE 7 BY .76 AT ROW 1.71 COL 4
          BGCOLOR 8 
     RECT-5 AT ROW 1.38 COL 136.2
     RECT-4 AT ROW 1.38 COL 108.8
     "Previous Tab" VIEW-AS TEXT
          SIZE 14 BY .76 AT ROW 10.05 COL 6
          BGCOLOR 4 FGCOLOR 15 FONT 4
     "Next Tab" VIEW-AS TEXT
          SIZE 10.6 BY .76 AT ROW 8.33 COL 9.4
          BGCOLOR 4 FGCOLOR 15 FONT 4
     "Current Tab #" VIEW-AS TEXT
          SIZE 14 BY .76 AT ROW 6.67 COL 6
          BGCOLOR 4 FGCOLOR 15 FONT 4
     "Widgets" VIEW-AS TEXT
          SIZE 8.4 BY .62 AT ROW 6.14 COL 132
          BGCOLOR 15 FONT 4
     RECT-8 AT ROW 6 COL 122.8
     RECT-6 AT ROW 6 COL 2.4
     "E&xit" VIEW-AS TEXT
          SIZE 4.2 BY .76 AT ROW 3.67 COL 141
          BGCOLOR 8 FONT 4
     "File Functions" VIEW-AS TEXT
          SIZE 14 BY .76 AT ROW 3.67 COL 116.6
          BGCOLOR 8 FONT 4
     "Edit Functions" VIEW-AS TEXT
          SIZE 14 BY .76 AT ROW 3.67 COL 83.6
          BGCOLOR 8 FONT 4
     RECT-2 AT ROW 1.38 COL 59.2
     RECT-3 AT ROW 1.38 COL 71.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 149.8 BY 24.14
         BGCOLOR 3 .

DEFINE FRAME Folder-Frm
     Rect-Main AT ROW 2.05 COL 1.6
     Rect-Top AT ROW 2.14 COL 1.8
     Rect-Left AT ROW 2.19 COL 1.8
     Rect-Right AT ROW 2.29 COL 91.2
     Rect-Bottom AT ROW 21.05 COL 1.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 30.4 ROW 4.66
         SIZE 91.6 BY 20.33.


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
         TITLE              = "Misc. Fields Design Layout Window"
         HEIGHT             = 24.19
         WIDTH              = 149.8
         MAX-HEIGHT         = 36.76
         MAX-WIDTH          = 223.8
         VIRTUAL-HEIGHT     = 36.76
         VIRTUAL-WIDTH      = 223.8
         RESIZE             = yes
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

IF NOT C-Win:LOAD-ICON("images\uib":U) THEN
    MESSAGE "Unable to load icon: images\uib"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR BUTTON Btn_Add_Tab IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Combo-Box IN FRAME DEFAULT-FRAME
   1 3                                                                  */
/* SETTINGS FOR BUTTON Btn_Delete_Group IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Delete_Tab IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Down_Tab IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Editor IN FRAME DEFAULT-FRAME
   1 3                                                                  */
/* SETTINGS FOR BUTTON Btn_Fill-In IN FRAME DEFAULT-FRAME
   1 3                                                                  */
/* SETTINGS FOR BUTTON Btn_Next_Tab IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Pointer IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 3                                                        */
/* SETTINGS FOR BUTTON Btn_Prev_Tab IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Radio-Set IN FRAME DEFAULT-FRAME
   1 3                                                                  */
/* SETTINGS FOR BUTTON Btn_Rename_Group IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Rename_Tab IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Selection-List IN FRAME DEFAULT-FRAME
   1 3                                                                  */
/* SETTINGS FOR BUTTON Btn_Slider IN FRAME DEFAULT-FRAME
   1 3                                                                  */
/* SETTINGS FOR BUTTON Btn_Text IN FRAME DEFAULT-FRAME
   1 3                                                                  */
/* SETTINGS FOR BUTTON Btn_Toggle-Box IN FRAME DEFAULT-FRAME
   1 3                                                                  */
/* SETTINGS FOR BUTTON Btn_Up_Tab IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN combo-box-label IN FRAME DEFAULT-FRAME
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN editor-label IN FRAME DEFAULT-FRAME
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fill-in-label IN FRAME DEFAULT-FRAME
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN gap-field IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       gap-field:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN mfgroup-field IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       mfgroup-field:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX mfgroup-tab IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN pointer-label IN FRAME DEFAULT-FRAME
   NO-ENABLE 2                                                          */
ASSIGN 
       pointer-label:SELECTABLE IN FRAME DEFAULT-FRAME       = TRUE.

/* SETTINGS FOR FILL-IN radio-set-label IN FRAME DEFAULT-FRAME
   NO-ENABLE 2                                                          */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN selection-list-label IN FRAME DEFAULT-FRAME
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN slider-label IN FRAME DEFAULT-FRAME
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN tab-field IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tab-field:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR SELECTION-LIST tab-labels IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN text-label IN FRAME DEFAULT-FRAME
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN toggle-box-label IN FRAME DEFAULT-FRAME
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FRAME Folder-Frm
                                                                        */
ASSIGN 
       FRAME Folder-Frm:BOX-SELECTABLE   = TRUE
       FRAME Folder-Frm:SELECTABLE       = TRUE.

/* SETTINGS FOR RECTANGLE Rect-Main IN FRAME Folder-Frm
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

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME Folder-Frm
/* Query rebuild information for FRAME Folder-Frm
     _Query            is NOT OPENED
*/  /* FRAME Folder-Frm */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Misc. Fields Design Layout Window */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Misc. Fields Design Layout Window */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Folder-Frm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Folder-Frm C-Win
ON ENTRY OF FRAME Folder-Frm
DO:
  FRAME Folder-Frm:SELECTED = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Folder-Frm C-Win
ON LEAVE OF FRAME Folder-Frm
DO:
  FRAME Folder-Frm:SELECTED = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Folder-Frm C-Win
ON MOUSE-SELECT-CLICK OF FRAME Folder-Frm
DO:
  IF CAN-DO("COMBO-BOX,EDITOR,FILL-IN,RADIO-SET,SELECTION-LIST,SLIDER,TEXT,TOGGLE-BOX",current-label) THEN
  DO: /* only done when a new widget is being placed in design folder-frm */
    RUN New_Widget.
    APPLY "CHOOSE" TO Btn_Pointer IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add_Group
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add_Group C-Win
ON CHOOSE OF Btn_Add_Group IN FRAME DEFAULT-FRAME /* Add Group */
DO:
  new-mfgroup = YES.
  ENABLE mfgroup-field WITH FRAME {&FRAME-NAME}.
  mfgroup-field:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  APPLY "ENTRY" TO mfgroup-field IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add_Tab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add_Tab C-Win
ON CHOOSE OF Btn_Add_Tab IN FRAME DEFAULT-FRAME /* Add */
DO:
  new-tab = YES.
  ENABLE tab-field WITH FRAME {&FRAME-NAME}.
  tab-field:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  APPLY "ENTRY" TO tab-field IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Combo-Box
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Combo-Box C-Win
ON CHOOSE OF Btn_Combo-Box IN FRAME DEFAULT-FRAME /* Combo Box */
DO:
  RUN Select_New_Widget_Type ("COMBO-BOX","images/combbox.cur").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Copy C-Win
ON CHOOSE OF Btn_Copy IN FRAME DEFAULT-FRAME /* Copy */
DO:
  RUN Copy_Widgets.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cut C-Win
ON CHOOSE OF Btn_Cut IN FRAME DEFAULT-FRAME /* Cut */
DO:
  RUN Cut_Widgets.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Delete_Group
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Delete_Group C-Win
ON CHOOSE OF Btn_Delete_Group IN FRAME DEFAULT-FRAME /* Delete Group */
DO:
  RUN Delete_Group.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Delete_Tab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Delete_Tab C-Win
ON CHOOSE OF Btn_Delete_Tab IN FRAME DEFAULT-FRAME /* Delete */
DO:
  RUN Delete_Tab.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Down_Tab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Down_Tab C-Win
ON CHOOSE OF Btn_Down_Tab IN FRAME DEFAULT-FRAME /* Move Dn */
DO:
  RUN Move_Tab ("Down").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Editor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Editor C-Win
ON CHOOSE OF Btn_Editor IN FRAME DEFAULT-FRAME /* Editor */
DO:
  RUN Select_New_Widget_Type ("EDITOR","images/editor.cur").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Exit C-Win
ON CHOOSE OF Btn_Exit IN FRAME DEFAULT-FRAME /* Exit */
DO:
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Fill-In
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Fill-In C-Win
ON CHOOSE OF Btn_Fill-In IN FRAME DEFAULT-FRAME /* Fill In */
DO:
  RUN Select_New_Widget_Type ("FILL-IN","images/fill_in.cur").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Next_Tab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Next_Tab C-Win
ON CHOOSE OF Btn_Next_Tab IN FRAME DEFAULT-FRAME /* Next Tab */
DO:
  mfgroup-tab:SCREEN-VALUE IN FRAME {&FRAME-NAME} =
      STRING(INTEGER(mfgroup-tab:SCREEN-VALUE IN FRAME {&FRAME-NAME}) + 1).
  RUN Create_Widgets.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Paste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Paste C-Win
ON CHOOSE OF Btn_Paste IN FRAME DEFAULT-FRAME /* Paste */
DO:
  RUN Paste_from_Clipboard.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Pointer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Pointer C-Win
ON CHOOSE OF Btn_Pointer IN FRAME DEFAULT-FRAME /* Pointer */
DO:
  RUN Select_New_Widget_Type ("","arrow").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Prev_Tab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Prev_Tab C-Win
ON CHOOSE OF Btn_Prev_Tab IN FRAME DEFAULT-FRAME /* Previous Tab */
DO:
  mfgroup-tab:SCREEN-VALUE IN FRAME {&FRAME-NAME} =
      STRING(INTEGER(mfgroup-tab:SCREEN-VALUE IN FRAME {&FRAME-NAME}) - 1).
  RUN Create_Widgets.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Property
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Property C-Win
ON CHOOSE OF Btn_Property IN FRAME DEFAULT-FRAME /* Property */
DO:
  RUN Property_Sheet.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Radio-Set
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Radio-Set C-Win
ON CHOOSE OF Btn_Radio-Set IN FRAME DEFAULT-FRAME /* Radio Set */
DO:
  RUN Select_New_Widget_Type ("RADIO-SET","images/radioset.cur").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Rename_Group
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Rename_Group C-Win
ON CHOOSE OF Btn_Rename_Group IN FRAME DEFAULT-FRAME /* Rename Group */
DO:
  new-mfgroup = NO.
  ENABLE mfgroup-field WITH FRAME {&FRAME-NAME}.
  mfgroup-field:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  APPLY "ENTRY" TO mfgroup-field IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Rename_Tab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Rename_Tab C-Win
ON CHOOSE OF Btn_Rename_Tab IN FRAME DEFAULT-FRAME /* Rename */
DO:
  new-tab = NO.
  ENABLE tab-field WITH FRAME {&FRAME-NAME}.
  tab-field:SCREEN-VALUE IN FRAME {&FRAME-NAME} = tab-labels:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
  APPLY "ENTRY" TO tab-field IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Restore
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Restore C-Win
ON CHOOSE OF Btn_Restore IN FRAME DEFAULT-FRAME /* Restore */
DO:
  mfgroup-tab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1".
  RUN Load_Widget_Data.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Save C-Win
ON CHOOSE OF Btn_Save IN FRAME DEFAULT-FRAME /* Save */
DO:
  RUN Save_Layout.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Selection-List
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Selection-List C-Win
ON CHOOSE OF Btn_Selection-List IN FRAME DEFAULT-FRAME /* Selection List */
DO:
  RUN Select_New_Widget_Type ("SELECTION-LIST","images/slctlist.cur").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Slider
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Slider C-Win
ON CHOOSE OF Btn_Slider IN FRAME DEFAULT-FRAME /* Slider */
DO:
  RUN Select_New_Widget_Type ("SLIDER","images/slider.cur").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Test
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Test C-Win
ON CHOOSE OF Btn_Test IN FRAME DEFAULT-FRAME /* Test */
DO:
  current-widget = SESSION:FIRST-PROCEDURE.
  DO WHILE current-widget NE ?:
    IF INDEX(current-widget:FILE-NAME,"mfvalues.") NE 0 THEN
    DO:
      save-widget = current-widget:NEXT-SIBLING.
      APPLY "CLOSE" TO current-widget.
      current-widget = save-widget.
    END.
    ELSE
    current-widget = current-widget:NEXT-SIBLING.
  END.
  FIND prgrms WHERE prgrms.prgmname = "prgrms." NO-LOCK.
  RUN nosweat/mfvalues.w PERSISTENT
      (mfgroup-list:SCREEN-VALUE IN FRAME {&FRAME-NAME},
       prgrms.rec_key,{methods/headers/prgrms.i}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Text
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Text C-Win
ON CHOOSE OF Btn_Text IN FRAME DEFAULT-FRAME /* Text */
DO:
  RUN Select_New_Widget_Type ("TEXT","images/text.cur").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Toggle-Box
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Toggle-Box C-Win
ON CHOOSE OF Btn_Toggle-Box IN FRAME DEFAULT-FRAME /* Toggle Box */
DO:
  RUN Select_New_Widget_Type ("TOGGLE-BOX","images/toggle.cur").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Up_Tab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Up_Tab C-Win
ON CHOOSE OF Btn_Up_Tab IN FRAME DEFAULT-FRAME /* Move Up */
DO:
  RUN Move_Tab ("Up").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gap-field
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gap-field C-Win
ON LEAVE OF gap-field IN FRAME DEFAULT-FRAME /* Gap */
DO:
  APPLY "RETURN" TO {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gap-field C-Win
ON RETURN OF gap-field IN FRAME DEFAULT-FRAME /* Gap */
DO:
  IF {&SELF-NAME}:SCREEN-VALUE NE "0" THEN
  RUN Align_Widgets ("Gap").
  {&SELF-NAME}:HIDDEN = YES.
  DISABLE {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mfgroup-field
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mfgroup-field C-Win
ON LEAVE OF mfgroup-field IN FRAME DEFAULT-FRAME /* Group */
DO:
  APPLY "RETURN" TO {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mfgroup-field C-Win
ON RETURN OF mfgroup-field IN FRAME DEFAULT-FRAME /* Group */
DO:
  IF {&SELF-NAME}:SCREEN-VALUE NE "" THEN
  DO:
    ASSIGN
      cdummy = {&SELF-NAME}:SCREEN-VALUE
      ldummy = NO
      current-label = "".
    DO i = 1 TO mfgroup-list:NUM-ITEMS: /* see if item already exists */
      IF mfgroup-list:ENTRY(i) = cdummy THEN
      ldummy = YES.
    END.
    IF new-mfgroup THEN
    RUN Add_Group.
    ELSE
    RUN Rename_Group.
  END.
  ELSE
  cdummy = mfgroup-list:SCREEN-VALUE.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      mfgroup-list:SCREEN-VALUE = cdummy
      {&SELF-NAME}:HIDDEN = YES.
    DISABLE {&SELF-NAME}.
    APPLY "ENTRY" TO mfgroup-list.
    APPLY "VALUE-CHANGED" TO mfgroup-list.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mfgroup-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mfgroup-list C-Win
ON VALUE-CHANGED OF mfgroup-list IN FRAME DEFAULT-FRAME
DO:
  mfgroup-tab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1". /* set to first tab */
  RUN Create_Widgets.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mfgroup-tab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mfgroup-tab C-Win
ON VALUE-CHANGED OF mfgroup-tab IN FRAME DEFAULT-FRAME
DO:
  DO i = 1 TO tab-labels:NUM-ITEMS: /* get index value of selected label */
    IF tab-labels:SCREEN-VALUE = tab-labels:ENTRY(i) THEN
    LEAVE.
  END.
  IF i = INTEGER(mfgroup-tab:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN
  RETURN NO-APPLY.
  RUN Create_Widgets.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_1_LeftRight_Sides
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_1_LeftRight_Sides C-Win
ON CHOOSE OF MENU-ITEM m_1_LeftRight_Sides /* 1 Left/Right Sides */
DO:
  leftright = YES.
  ENABLE gap-field WITH FRAME {&FRAME-NAME}.
  gap-field:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  APPLY "ENTRY" TO gap-field IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_2_TopBottom_Sides
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_2_TopBottom_Sides C-Win
ON CHOOSE OF MENU-ITEM m_2_TopBottom_Sides /* 2 Top/Bottom Sides */
DO:
  leftright = NO.
  ENABLE gap-field WITH FRAME {&FRAME-NAME}.
  gap-field:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  APPLY "ENTRY" TO gap-field IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Add_Group
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Add_Group C-Win
ON CHOOSE OF MENU-ITEM m_Add_Group /* Add Group */
DO:
  APPLY "CHOOSE" TO Btn_Add_Group IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Add_Tab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Add_Tab C-Win
ON CHOOSE OF MENU-ITEM m_Add_Tab /* Add Tab */
DO:
  APPLY "CHOOSE" TO Btn_Add_Tab IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Bottom_Sides
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Bottom_Sides C-Win
ON CHOOSE OF MENU-ITEM m_Bottom_Sides /* Bottom Sides */
DO:
  RUN Align_Widgets ("Bottom").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Change_Group
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Change_Group C-Win
ON CHOOSE OF MENU-ITEM m_Change_Group /* Rename Group */
DO:
  APPLY "CHOOSE" TO Btn_Rename_Group IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Combo_Box
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Combo_Box C-Win
ON CHOOSE OF MENU-ITEM m_Combo_Box /* Combo Box */
DO:
  APPLY "CHOOSE" TO Btn_Combo-Box IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Copy C-Win
ON CHOOSE OF MENU-ITEM m_Copy /* Copy */
DO:
  APPLY "CHOOSE" TO Btn_Copy IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Cut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Cut C-Win
ON CHOOSE OF MENU-ITEM m_Cut /* Cut */
DO:
  APPLY "CHOOSE" TO Btn_Cut IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Delete_Group
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Delete_Group C-Win
ON CHOOSE OF MENU-ITEM m_Delete_Group /* Delete Group */
DO:
  APPLY "CHOOSE" TO Btn_Delete_Group IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Delete_Tab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Delete_Tab C-Win
ON CHOOSE OF MENU-ITEM m_Delete_Tab /* Delete Tab */
DO:
  APPLY "CHOOSE" TO Btn_Delete_Tab IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Editor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Editor C-Win
ON CHOOSE OF MENU-ITEM m_Editor /* Editor */
DO:
  APPLY "CHOOSE" TO Btn_Editor IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Exit C-Win
ON CHOOSE OF MENU-ITEM m_Exit /* Exit */
DO:
  APPLY "CHOOSE" TO Btn_Exit IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Fill_In
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Fill_In C-Win
ON CHOOSE OF MENU-ITEM m_Fill_In /* Fill In */
DO:
  APPLY "CHOOSE" TO Btn_Fill-In IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Left_Sides
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Left_Sides C-Win
ON CHOOSE OF MENU-ITEM m_Left_Sides /* Left Sides */
DO:
  RUN Align_Widgets ("Left").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Move_Tab_Down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Move_Tab_Down C-Win
ON CHOOSE OF MENU-ITEM m_Move_Tab_Down /* Move Tab Down */
DO:
  APPLY "CHOOSE" TO Btn_Down_Tab IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Move_Tab_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Move_Tab_Up C-Win
ON CHOOSE OF MENU-ITEM m_Move_Tab_Up /* Move Tab Up */
DO:
  APPLY "CHOOSE" TO Btn_Up_Tab IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Next_Tab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Next_Tab C-Win
ON CHOOSE OF MENU-ITEM m_Next_Tab /* Next Tab */
DO:
  APPLY "CHOOSE" TO Btn_Next_Tab IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Paste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Paste C-Win
ON CHOOSE OF MENU-ITEM m_Paste /* Paste */
DO:
  APPLY "CHOOSE" TO Btn_Paste IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Pointer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Pointer C-Win
ON CHOOSE OF MENU-ITEM m_Pointer /* Pointer */
DO:
  APPLY "CHOOSE" TO Btn_Pointer IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Previous_Tab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Previous_Tab C-Win
ON CHOOSE OF MENU-ITEM m_Previous_Tab /* Previous Tab */
DO:
  APPLY "CHOOSE" TO Btn_Prev_Tab IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Property_Sheet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Property_Sheet C-Win
ON CHOOSE OF MENU-ITEM m_Property_Sheet /* Property Sheet */
DO:
  APPLY "CHOOSE" TO Btn_Property IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Radio_Set
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Radio_Set C-Win
ON CHOOSE OF MENU-ITEM m_Radio_Set /* Radio Set */
DO:
  APPLY "CHOOSE" TO Btn_Radio-Set IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Rename_Tab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Rename_Tab C-Win
ON CHOOSE OF MENU-ITEM m_Rename_Tab /* Rename Tab */
DO:
  APPLY "CHOOSE" TO Btn_Rename_Tab IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Restore
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Restore C-Win
ON CHOOSE OF MENU-ITEM m_Restore /* Restore */
DO:
  APPLY "CHOOSE" TO Btn_Restore IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Right_Sides
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Right_Sides C-Win
ON CHOOSE OF MENU-ITEM m_Right_Sides /* Right Sides */
DO:
  RUN Align_Widgets ("Right").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Save C-Win
ON CHOOSE OF MENU-ITEM m_Save /* Save */
DO:
  APPLY "CHOOSE" TO Btn_Save IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Selection_List
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Selection_List C-Win
ON CHOOSE OF MENU-ITEM m_Selection_List /* Selection List */
DO:
  APPLY "CHOOSE" TO Btn_Selection-List IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Slider
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Slider C-Win
ON CHOOSE OF MENU-ITEM m_Slider /* Slider */
DO:
  APPLY "CHOOSE" TO Btn_Slider IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Text
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Text C-Win
ON CHOOSE OF MENU-ITEM m_Text /* Text */
DO:
  APPLY "CHOOSE" TO Btn_Text IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Toggle_Box
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Toggle_Box C-Win
ON CHOOSE OF MENU-ITEM m_Toggle_Box /* Toggle Box */
DO:
  APPLY "CHOOSE" TO Btn_Toggle-Box IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Top_Sdies
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Top_Sdies C-Win
ON CHOOSE OF MENU-ITEM m_Top_Sdies /* Top Sdies */
DO:
  RUN Align_Widgets ("Top").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tab-field
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tab-field C-Win
ON LEAVE OF tab-field IN FRAME DEFAULT-FRAME
DO:
  APPLY "RETURN" TO {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tab-field C-Win
ON RETURN OF tab-field IN FRAME DEFAULT-FRAME
DO:
  IF {&SELF-NAME}:SCREEN-VALUE NE "" THEN
  DO:
    IF new-tab THEN
    RUN Add_Tab.
    ELSE
    RUN Rename_Tab.
  END.
  tab-field:HIDDEN = YES.
  DISABLE tab-field.
  APPLY "VALUE-CHANGED" TO tab-labels.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tab-labels
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tab-labels C-Win
ON VALUE-CHANGED OF tab-labels IN FRAME DEFAULT-FRAME
DO:
  DO i = 1 TO {&SELF-NAME}:NUM-ITEMS: /* get index value of selected label */
    IF {&SELF-NAME}:SCREEN-VALUE = {&SELF-NAME}:ENTRY(i) THEN
    LEAVE.
  END.
  IF i = INTEGER(mfgroup-tab:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN
  RETURN NO-APPLY.
  mfgroup-tab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(i).
  RUN Create_Widgets.
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

ON ESC OF {&WINDOW-NAME} ANYWHERE DO:
  APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN Set_Tab_Order. /* natural order is left to right, we want top to bottom */
  RUN Load_Images.
  RUN enable_UI.
  RUN Load_Widget_Data.
  DISABLE Btn_Paste Btn_Pointer Btn_Property Btn_Cut Btn_Copy WITH FRAME {&FRAME-NAME}.
  {methods/nowait.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Add_Group C-Win 
PROCEDURE Add_Group :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT ldummy THEN
  DO:
    ldummy = mfgroup-list:ADD-LAST(cdummy) IN FRAME {&FRAME-NAME}.
    CREATE mfgroup.
    ASSIGN
      mfgroup.mfgroup = cdummy
      mfgroup.tab-label = " ".
  END.
  ELSE
  MESSAGE "Group" cdummy "Already Exists!" VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Add_Tab C-Win 
PROCEDURE Add_Tab :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    DO i = 1 TO tab-labels:NUM-ITEMS:   /* insert is after selected item, */
      IF tab-labels:IS-SELECTED(i) THEN /* find out which one is selected */
      LEAVE.
    END.
    FIND mfgroup WHERE mfgroup.mfgroup = mfgroup-list:SCREEN-VALUE EXCLUSIVE-LOCK.
    ASSIGN
      ldummy = tab-labels:INSERT(tab-field:SCREEN-VALUE,i + 1)
      tab-labels:SCREEN-VALUE = tab-labels:ENTRY(i + 1)
      ldummy = mfgroup-tab:INSERT(STRING(i + 1),i + 1) /* add new tab number also, */
      mfgroup.tab-label = tab-labels:LIST-ITEMS. /* this is only really needed if */
  END.                                         /* this is new last tab */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Align_Widgets C-Win 
PROCEDURE Align_Widgets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER align-type AS CHARACTER NO-UNDO.
  DEFINE VARIABLE new-coord AS INTEGER NO-UNDO.
  DEFINE VARIABLE prev-coord AS INTEGER NO-UNDO.
  DEFINE VARIABLE recidstr AS CHARACTER NO-UNDO.

  CASE align-type:
    WHEN "Left" OR WHEN "Top" THEN
    new-coord = 9999.
    WHEN "Right" OR WHEN "Bottom" THEN
    new-coord = -9999.
    WHEN "Gap" THEN
    new-coord = INTEGER(gap-field:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  END CASE.

  ASSIGN
    current-widget = FRAME Folder-Frm:HANDLE
    current-widget = current-widget:FIRST-CHILD
    current-widget = current-widget:FIRST-CHILD.
  DO WHILE current-widget NE ?:
    IF current-widget:SELECTED THEN
    CASE align-type:
      WHEN "Left" THEN
      IF current-widget:X LE new-coord THEN new-coord = current-widget:X.
      WHEN "Right" THEN
      IF current-widget:X + current-widget:WIDTH-PIXELS GE new-coord THEN
      new-coord = current-widget:X + current-widget:WIDTH-PIXELS.
      WHEN "Top" THEN
      IF current-widget:Y LE new-coord THEN new-coord = current-widget:Y.
      WHEN "Bottom" THEN
      IF current-widget:Y + current-widget:HEIGHT-PIXELS GE new-coord THEN
      new-coord = current-widget:Y + current-widget:HEIGHT-PIXELS.
      WHEN "Gap" THEN
      recidstr = recidstr + ENTRY(1,current-widget:PRIVATE-DATA) + ",".
    END CASE.
    current-widget = current-widget:NEXT-SIBLING.
  END.

  IF align-type = "Gap" THEN
  DO:
    RUN Create_Widgets.
    ASSIGN
      current-widget = FRAME Folder-Frm:HANDLE
      current-widget = current-widget:FIRST-CHILD
      current-widget = current-widget:FIRST-CHILD.
    DO WHILE current-widget NE ?:
      IF current-widget:PRIVATE-DATA NE ? AND
         LOOKUP(ENTRY(1,current-widget:PRIVATE-DATA),recidstr) NE 0 THEN
      current-widget:SELECTED = YES.
      current-widget = current-widget:NEXT-SIBLING.
    END.
  END.

  ASSIGN
    current-widget = FRAME Folder-Frm:HANDLE
    current-widget = current-widget:FIRST-CHILD
    current-widget = current-widget:FIRST-CHILD.
  DO WHILE current-widget NE ?:
    IF current-widget:SELECTED THEN
    DO:
      CASE align-type:
        WHEN "Left" THEN
        current-widget:X = new-coord.
        WHEN "Right" THEN
        current-widget:X = new-coord - current-widget:WIDTH-PIXELS.
        WHEN "Top" THEN
        current-widget:Y = new-coord.
        WHEN "Bottom" THEN
        current-widget:Y = new-coord - current-widget:HEIGHT-PIXELS.
        WHEN "Gap" THEN
        CASE LeftRight:
          WHEN yes THEN
          DO:
            IF prev-coord NE 0 THEN
            current-widget:X = prev-coord + new-coord.
            prev-coord = current-widget:X + current-widget:WIDTH-PIXELS.
          END.
          WHEN no THEN
          DO:
            IF prev-coord NE 0 THEN
            current-widget:Y = prev-coord + new-coord.
            prev-coord = current-widget:Y + current-widget:HEIGHT-PIXELS.
          END.
        END CASE.
      END CASE.
      RUN Update_attrb.
      IF NOT CAN-DO("TEXT,TOGGLE-BOX",current-widget:TYPE) THEN
      DO:
        label-widget = current-widget:SIDE-LABEL-HANDLE.
        DELETE WIDGET label-widget.
      END.
      save-widget = current-widget:NEXT-SIBLING.
      DELETE WIDGET current-widget.
      RUN Dynamic_Widget.
      current-widget = save-widget.
    END.
    ELSE
    current-widget = current-widget:NEXT-SIBLING.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Clear_Clipboard C-Win 
PROCEDURE Clear_Clipboard :
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Copy_to_Clipboard C-Win 
PROCEDURE Copy_to_Clipboard :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND attrb
      WHERE ROWID(attrb) = TO-ROWID(ENTRY(1,current-widget:PRIVATE-DATA))
          EXCLUSIVE-LOCK.
  CREATE wtbl-clipboard.
  BUFFER-COPY attrb TO wtbl-clipboard.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Copy_Widgets C-Win 
PROCEDURE Copy_Widgets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN Clear_Clipboard.
  ASSIGN
    current-widget = FRAME Folder-Frm:HANDLE
    current-widget = current-widget:FIRST-CHILD
    current-widget = current-widget:FIRST-CHILD.
  DO WHILE current-widget NE ?:
    IF current-widget:SELECTED THEN
    RUN Copy_to_Clipboard.
    current-widget = current-widget:NEXT-SIBLING.
  END.
  IF CAN-FIND(FIRST wtbl-clipboard) THEN
  ENABLE Btn_Paste WITH FRAME {&FRAME-NAME}.
  ELSE
  DISABLE Btn_Paste WITH FRAME {&FRAME-NAME}.

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
    CREATE TEXT label-widget IN WIDGET-POOL "{&widget-pool-name}"
      ASSIGN
        FRAME = FRAME Folder-Frm:HANDLE
        AUTO-RESIZE = YES
        Y = ip-y
        FORMAT = "X(" +
          (IF LENGTH(ip-label) NE 0 THEN STRING(LENGTH(ip-label) + 1)
          ELSE "1") + ")"
        SENSITIVE = YES
        SCREEN-VALUE = IF ip-label = "" THEN ""
                       ELSE ip-label + ":".
    
    ASSIGN
      label-widget:FONT = ?
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
  DO WITH FRAME {&FRAME-NAME}:
    FIND mfgroup WHERE mfgroup.mfgroup = mfgroup-list:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF NOT AVAILABLE mfgroup THEN
    DO:
      ASSIGN
        mfgroup-tab:LIST-ITEMS = "1"
        tab-labels:LIST-ITEMS = ""
        tab-labels:INNER-LINES = 1.
      DISABLE {&LIST-1} {&LIST-3}.
      RETURN.
    END.
    ELSE
    DO:
      ENABLE {&LIST-1} {&LIST-3}.
      RUN Select_New_Widget_Type ("","arrow").
    END.
    ASSIGN
      current-tab = INTEGER(mfgroup-tab:SCREEN-VALUE)
      cdummy = "1".
    DO i = 2 TO NUM-ENTRIES(mfgroup.tab-label):
      cdummy = cdummy + "," + STRING(i).
    END.
    ASSIGN
      mfgroup-tab:INNER-LINES = NUM-ENTRIES(mfgroup.tab-label)
      mfgroup-tab:LIST-ITEMS = cdummy
      mfgroup-tab:SCREEN-VALUE = STRING(current-tab)
      tab-labels:INNER-LINES = NUM-ENTRIES(mfgroup.tab-label)
      tab-labels:LIST-ITEMS = mfgroup.tab-label
      tab-labels:SCREEN-VALUE = tab-labels:ENTRY(current-tab).
    IF current-tab = 1 THEN
    DISABLE Btn_Prev_Tab.
    IF current-tab = NUM-ENTRIES(mfgroup.tab-label) THEN
    DISABLE Btn_Next_Tab.
    DO i = 1 TO tab-labels:NUM-ITEMS:
      IF tab-labels:IS-SELECTED(i) THEN
      LEAVE.
    END.
    IF tab-labels:NUM-ITEMS = 6 THEN
    DISABLE Btn_Add_Tab.
    IF tab-labels:NUM-ITEMS = 1 THEN
    DISABLE Btn_Delete_Tab.
    IF i = 1 THEN
    DISABLE Btn_Up_Tab.
    IF i = tab-labels:NUM-ITEMS THEN
    DISABLE Btn_Down_Tab.
  END.
  DO i = 1 TO NUM-ENTRIES(mfgroup.tab-labels):
    CREATE IMAGE tab-image[i] IN WIDGET-POOL "{&widget-pool-name}"
      ASSIGN
        FRAME = FRAME Folder-Frm:HANDLE
        X = 3 + (i - 1) * 72
        Y = 1
        WIDTH-PIXEL = 72
        HEIGHT-PIXEL = 24
        SENSITIVE = YES
      TRIGGERS:      
        ON MOUSE-SELECT-CLICK 
           PERSISTENT RUN Label_Trigger IN THIS-PROCEDURE (i).
      END TRIGGERS.         
      CREATE TEXT tab-label[i] IN WIDGET-POOL "{&widget-pool-name}"
        ASSIGN 
          FRAME = FRAME Folder-Frm:HANDLE
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
    DISABLE Btn_Cut Btn_Copy WITH FRAME {&FRAME-NAME}.
  END.
  /* use up tab image for currently selected tab */
  ASSIGN
    mfgroup-tab:SCREEN-VALUE = STRING(current-tab)
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
  DELETE WIDGET-POOL "{&widget-pool-name}" NO-ERROR.
  CREATE WIDGET-POOL "{&widget-pool-name}" PERSISTENT.
  RUN Create_Tabs.
  FOR EACH attrb
      WHERE attrb.attr_mfgroup = mfgroup-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} AND
            attrb.attr_tab = INTEGER(mfgroup-tab:SCREEN-VALUE IN FRAME {&FRAME-NAME})
          NO-LOCK:
    RUN Dynamic_Widget.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cut_Widgets C-Win 
PROCEDURE Cut_Widgets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN Clear_Clipboard.
  ASSIGN
    current-widget = FRAME Folder-Frm:HANDLE
    current-widget = current-widget:FIRST-CHILD
    current-widget = current-widget:FIRST-CHILD.
  WIDGET-TREE:
  DO WHILE current-widget NE ?:
    IF current-widget:SELECTED THEN
    DO:
      RUN Copy_to_Clipboard.
      IF CAN-FIND(FIRST mfvalues WHERE mfvalues.mf_id = attrb.attr_id) THEN
      DO:
        ldummy = NO.
        MESSAGE "Attribute Value Records Exist!" SKIP(1)
                "Cut Attribute Widget & Delete Attribute Value Records When Layout SAVED? (yes/no)" SKIP(1)
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Attribute Widget: " + attrb.attr_label
                UPDATE ldummy.
        IF NOT ldummy THEN
        DO:
          DELETE wtbl-clipboard.
          current-widget = current-widget:NEXT-SIBLING.
          NEXT WIDGET-TREE.
        END.
      END.
      RUN Delete_mfvalues.
      DELETE attrb.
      IF NOT CAN-DO("TEXT,TOGGLE-BOX",current-widget:TYPE) THEN
      DO:
        label-widget = current-widget:SIDE-LABEL-HANDLE.
        DELETE WIDGET label-widget.
      END.
      dyn-widget = current-widget:NEXT-SIBLING.
      DELETE WIDGET current-widget.
      current-widget = dyn-widget.
    END.
    ELSE
    current-widget = current-widget:NEXT-SIBLING.
  END.
  IF CAN-FIND(FIRST wtbl-clipboard) THEN
  ENABLE Btn_Paste WITH FRAME {&FRAME-NAME}.
  ELSE
  DISABLE Btn_Paste WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Delete_Group C-Win 
PROCEDURE Delete_Group :
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
    IF NOT ldummy THEN
    RETURN.
    FOR EACH attrb WHERE attrb.attr_mfgroup = mfgroup-list:SCREEN-VALUE NO-LOCK:
      IF CAN-FIND(FIRST mfvalues WHERE mfvalues.mf_id = attrb.attr_id) THEN
      DO:
        ldummy = NO.
        MESSAGE "Attribute Value Records Exist!" SKIP(1)
                "Delete Group & Delete Attribute Value Records When Layout SAVED? (yes/no)"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Delete Group: " + attrb.attr_mfgroup
                UPDATE ldummy.
        IF NOT ldummy THEN
        RETURN.
        LEAVE.
      END.
    END.
    FIND mfgroup WHERE mfgroup.mfgroup = mfgroup-list:SCREEN-VALUE EXCLUSIVE-LOCK.
    DELETE mfgroup.
    FOR EACH attrb WHERE attrb.attr_mfgroup = mfgroup-list:SCREEN-VALUE EXCLUSIVE-LOCK:
      RUN Delete_mfvalues.
      DELETE attrb.
    END.
    ASSIGN
      ldummy = mfgroup-list:DELETE(mfgroup-list:SCREEN-VALUE)
      mfgroup-list:SCREEN-VALUE = IF mfgroup-list:NUM-ITEMS GT 1 THEN mfgroup-list:ENTRY(2)
                                ELSE mfgroup-list:ENTRY(1).
    APPLY "ENTRY" TO mfgroup-list.
    APPLY "VALUE-CHANGED" TO mfgroup-list.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Delete_mfvalues C-Win 
PROCEDURE Delete_mfvalues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH mfvalues WHERE mfvalues.mf_id = attrb.attr_id NO-LOCK:
    FIND del-mfvalues WHERE mfvalues_recid = RECID(mfvalues) NO-LOCK NO-ERROR.
    IF AVAILABLE del-mfvalues THEN
    NEXT.
    CREATE del-mfvalues.
    del-mfvalues.mfvalues_recid = RECID(mfvalues).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Delete_Tab C-Win 
PROCEDURE Delete_Tab :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    DO i = 1 TO tab-labels:NUM-ITEMS:
      IF tab-labels:IS-SELECTED(i) THEN
      LEAVE.
    END.
    FOR EACH attrb
        WHERE attrb.attr_mfgroup = mfgroup-list:SCREEN-VALUE AND
              attrb.attr_tab = i NO-LOCK:
      IF CAN-FIND(FIRST mfvalues WHERE mfvalues.mf_id = attrb.attr_id) THEN
      DO:
        ldummy = NO.
        MESSAGE "Attribute Value Records Exist!" SKIP(1)
                "Remove Tab & Delete Attribute Value Records When Layout SAVED? (yes/no)"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Remove Tab: " + tab-labels:ENTRY(i)
                UPDATE ldummy.
        IF NOT ldummy THEN
        RETURN.
        LEAVE.
      END.
    END.
    FOR EACH attrb
        WHERE attrb.attr_mfgroup = mfgroup-list:SCREEN-VALUE AND attrb.attr_tab = i
            EXCLUSIVE-LOCK:
      RUN Delete_mfvalues.
      DELETE attrb.
    END.
    FIND mfgroup WHERE mfgroup.mfgroup = mfgroup-list:SCREEN-VALUE EXCLUSIVE-LOCK.
    ASSIGN
      ldummy = tab-labels:DELETE(i)
      tab-labels:SCREEN-VALUE = tab-labels:ENTRY(1)
      ldummy = mfgroup-tab:DELETE(i)
      mfgroup-tab:SCREEN-VALUE = mfgroup-tab:ENTRY(1)
      mfgroup.tab-label = tab-labels:LIST-ITEMS.
    RUN Create_Widgets.
    DISABLE Btn_Cut Btn_Copy WITH FRAME {&FRAME-NAME}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Deselection_Widget C-Win 
PROCEDURE Deselection_Widget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF FRAME Folder-Frm:NUM-SELECTED-WIDGETS = 2 THEN
  ENABLE Btn_Property WITH FRAME {&FRAME-NAME}.
  ELSE
  DISABLE Btn_Property WITH FRAME {&FRAME-NAME}.

  IF FRAME Folder-Frm:NUM-SELECTED-WIDGETS = 1 THEN
  DISABLE Btn_Cut Btn_Copy WITH FRAME {&FRAME-NAME}.

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
            FRAME = FRAME Folder-Frm:HANDLE ~
            FONT = ? ~
            X = attrb.attr_x ~
            Y = attrb.attr_y ~
            SENSITIVE = YES ~
            MOVABLE = YES ~
            SELECTABLE = YES ~
            RESIZABLE = YES ~
            PRIVATE-DATA = STRING(ROWID(attrb)) + "," + ~
                           mfgroup-tab:SCREEN-VALUE IN FRAME {&FRAME-NAME} ~
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
            SCREEN-VALUE = attrb.attr_default ~
            {&trigger-code}
&scoped-define editor-code ~
            {&widget-code} ~
            SCROLLBAR-VERTICAL = vert-bar ~
            SCROLLBAR-HORIZONTAL = horz-bar ~
            HELP = 'Enter ''' + attrb.attr_label + ''' in EDITOR Field' ~
            SCREEN-VALUE = attrb.attr_default ~
            {&trigger-code}
&scoped-define fill-in-code ~
            {&widget-code} ~
            DATA-TYPE = attrb.attr_datatype ~
            FORMAT = attrb.attr_settings ~
            HELP = 'Enter ''' + attrb.attr_label + ''' in FILL-IN Field' ~
            SCREEN-VALUE = attrb.attr_default ~
            {&trigger-code}
&scoped-define radio-set-code ~
            {&widget-code} ~
            HORIZONTAL = horz-bar ~
            RADIO-BUTTONS = attrb.attr_values ~
            HELP = 'Select ''' + attrb.attr_label + ''' from RADIO-SET Choices' ~
            SCREEN-VALUE = attrb.attr_default ~
            {&trigger-code}
&scoped-define selection-list-code ~
            {&widget-code} ~
            LIST-ITEMS = attrb.attr_values ~
            HELP = 'Select ''' + attrb.attr_label + ''' from SELECTION-LIST' ~
            SCREEN-VALUE = attrb.attr_default ~
            {&trigger-code}
&scoped-define slider-code ~
            {&widget-code} ~
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
            LABEL = attrb.attr_label ~
            HEIGHT-PIXELS = attrb.attr_height ~
            HELP = 'Set ''' + attrb.attr_label + ''' TOGGLE-BOX On/Off' ~
            SCREEN-VALUE = attrb.attr_default ~
            {&trigger-code}

  ASSIGN
    horz-bar = IF NUM-ENTRIES(attrb.attr_settings) NE 0 AND
                  ENTRY(1,attrb.attr_settings) = "yes" THEN yes
               ELSE no
    vert-bar = IF NUM-ENTRIES(attrb.attr_settings) GT 1 AND
                  ENTRY(2,attrb.attr_settings) = "yes" THEN yes
               ELSE no.
  RUN Create_Label_Widget (attrb.attr_type,attrb.attr_label,attrb.attr_x,attrb.attr_y).
  DO ON ERROR UNDO, RETURN "CREATE-ERROR":
    CASE attrb.attr_type:
&scoped-define widget-type COMBO-BOX
      WHEN "{&widget-type}" THEN
      DO:
        CREATE {&widget-type} dyn-widget IN WIDGET-POOL "{&widget-pool-name}"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type EDITOR
      WHEN "{&widget-type}" THEN
      DO:
        CREATE {&widget-type} dyn-widget IN WIDGET-POOL "{&widget-pool-name}"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type FILL-IN
      WHEN "{&widget-type}" THEN
      DO:
        CREATE {&widget-type} dyn-widget IN WIDGET-POOL "{&widget-pool-name}"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type RADIO-SET
      WHEN "{&widget-type}" THEN
      DO:
        CREATE {&widget-type} dyn-widget IN WIDGET-POOL "{&widget-pool-name}"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type SELECTION-LIST
      WHEN "{&widget-type}" THEN
      DO:
        CREATE {&widget-type} dyn-widget IN WIDGET-POOL "{&widget-pool-name}"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type SLIDER
      WHEN "{&widget-type}" THEN
      DO:
        CREATE {&widget-type} dyn-widget IN WIDGET-POOL "{&widget-pool-name}"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type TEXT
      WHEN "{&widget-type}" THEN
      DO:
        CREATE {&widget-type} dyn-widget IN WIDGET-POOL "{&widget-pool-name}"
        {&{&widget-type}-code}
      END.
&scoped-define widget-type TOGGLE-BOX
      WHEN "{&widget-type}" THEN
      DO:
        CREATE {&widget-type} dyn-widget IN WIDGET-POOL "{&widget-pool-name}"
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
  DISPLAY mfgroup-list mfgroup-tab pointer-label combo-box-label editor-label 
          tab-labels fill-in-label radio-set-label selection-list-label 
          slider-label text-label toggle-box-label 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-7 RECT-5 RECT-4 RECT-8 RECT-6 RECT-2 RECT-3 mfgroup-list 
         Btn_Property Btn_Cut Btn_Copy Btn_Paste Btn_Restore Btn_Save Btn_Exit 
         Btn_Add_Group Btn_Rename_Group Btn_Delete_Group Btn_Test mfgroup-tab 
         Btn_Next_Tab Btn_Combo-Box Btn_Prev_Tab Btn_Editor tab-labels 
         Btn_Fill-In Btn_Radio-Set Btn_Selection-List Btn_Slider Btn_Add_Tab 
         Btn_Rename_Tab Btn_Text Btn_Delete_Tab Btn_Toggle-Box Btn_Down_Tab 
         Btn_Up_Tab 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE Rect-Top Rect-Left Rect-Right Rect-Bottom 
      WITH FRAME Folder-Frm IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Folder-Frm}
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

  IF tabno = INTEGER(mfgroup-tab:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN
  RETURN.
  mfgroup-tab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(tabno).
  RUN Create_Widgets.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Load_Images C-Win 
PROCEDURE Load_Images :
/*------------------------------------------------------------------------------
  Purpose:     Load Button Images
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      ldummy = Btn_Pointer:LOAD-IMAGE-UP("images/wp_up",29,253,28,28)
      ldummy = Btn_Combo-Box:LOAD-IMAGE-UP("images/wp_up",1,141,28,28)
      ldummy = Btn_Editor:LOAD-IMAGE-UP("images/wp_up",29,113,28,28)
      ldummy = Btn_Fill-In:LOAD-IMAGE-UP("images/wp_up",29,141,28,28)
      ldummy = Btn_Radio-Set:LOAD-IMAGE-UP("images/wp_up",1,57,28,28)
      ldummy = Btn_Selection-List:LOAD-IMAGE-UP("images/wp_up",1,113,28,28)
      ldummy = Btn_Slider:LOAD-IMAGE-UP("images/wp_up",1,85,28,28)
      ldummy = Btn_Text:LOAD-IMAGE-UP("images/wp_up",1,169,28,28)
      ldummy = Btn_Toggle-Box:LOAD-IMAGE-UP("images/wp_up",29,57,28,28)
      ldummy = Btn_Pointer:LOAD-IMAGE-INSENSITIVE("images/wp_down",29,253,28,28)
      ldummy = Btn_Combo-Box:LOAD-IMAGE-INSENSITIVE("images/wp_down",1,141,28,28)
      ldummy = Btn_Editor:LOAD-IMAGE-INSENSITIVE("images/wp_down",29,113,28,28)
      ldummy = Btn_Fill-In:LOAD-IMAGE-INSENSITIVE("images/wp_down",29,141,28,28)
      ldummy = Btn_Radio-Set:LOAD-IMAGE-INSENSITIVE("images/wp_down",1,57,28,28)
      ldummy = Btn_Selection-List:LOAD-IMAGE-INSENSITIVE("images/wp_down",1,113,28,28)
      ldummy = Btn_Slider:LOAD-IMAGE-INSENSITIVE("images/wp_down",1,85,28,28)
      ldummy = Btn_Text:LOAD-IMAGE-INSENSITIVE("images/wp_down",1,169,28,28)
      ldummy = Btn_Toggle-Box:LOAD-IMAGE-INSENSITIVE("images/wp_down",29,57,28,28).
  END.

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
      mfgroup.tab-labels = REPLACE(ENTRY(i + 1,mfdata.mfgroup_data,";"),'"','').
  END.
  OUTPUT TO VALUE("users/" + USERID("NOSWEAT") + "/mfdata.dat").
  DO i = 1 TO NUM-ENTRIES(mfdata.miscflds_data,";") - 1:
    PUT UNFORMATTED ENTRY(i,mfdata.miscflds_data,";") AT 1.
  END.
  OUTPUT CLOSE.
  FOR EACH mfgroup EXCLUSIVE-LOCK:
    DELETE mfgroup.
  END.
  FOR EACH attrb EXCLUSIVE-LOCK:
    DELETE attrb.
  END.
  FOR EACH del-mfvalues EXCLUSIVE-LOCK:
    DELETE del-mfvalues.
  END.
  INPUT FROM VALUE("users/" + USERID("NOSWEAT") + "/mfdata.dat") NO-ECHO.
  REPEAT:
    CREATE attrb.
    IMPORT attrb.
  END.
  INPUT CLOSE.
  IF attrb.attr_type = "" THEN
  DELETE attrb.
  INPUT FROM VALUE("users/" + USERID("NOSWEAT") + "/mfgroup.dat").
  IMPORT cdummy.
  DO i = 1 TO NUM-ENTRIES(cdummy):
    IMPORT current-label.
    CREATE mfgroup.
    ASSIGN
      mfgroup.mfgroup = ENTRY(i,cdummy)
      mfgroup.tab-labels = current-label.
  END.
  INPUT CLOSE.
  FIND LAST attrb USE-INDEX si-attrb NO-LOCK NO-ERROR.
  DO WITH FRAME {&FRAME-NAME}:
    last_id = IF AVAILABLE attrb THEN INTEGER(attrb.attr_id) ELSE 0 NO-ERROR.
    ASSIGN
      mfgroup-list:LIST-ITEMS = IF cdummy NE "" THEN cdummy ELSE " "
      mfgroup-list:SCREEN-VALUE =
          IF mfgroup-list:NUM-ITEMS GT 1 THEN mfgroup-list:ENTRY(2)
          ELSE mfgroup-list:ENTRY(1).
  END.
  RUN Create_Widgets.
  APPLY "ENTRY" TO mfgroup-list IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move_Tab C-Win 
PROCEDURE Move_Tab :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER move AS CHARACTER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    FIND mfgroup WHERE mfgroup.mfgroup = mfgroup-list:SCREEN-VALUE EXCLUSIVE-LOCK.
    DO i = 1 TO tab-labels:NUM-ITEMS:
      IF tab-labels:IS-SELECTED(i) THEN
      LEAVE.
    END.
    ASSIGN
      j = IF move = "Down" THEN i + 1 ELSE i - 1
      ldummy = IF move = "Down" THEN tab-labels:INSERT(tab-labels:SCREEN-VALUE,j + 1)
                                ELSE tab-labels:INSERT(tab-labels:SCREEN-VALUE,j)
      ldummy = IF move = "Down" THEN tab-labels:DELETE(i)
                                ELSE tab-labels:DELETE(i + 1)
      mfgroup.tab-label = tab-labels:LIST-ITEMS
      tab-labels:SCREEN-VALUE = tab-labels:ENTRY(j).
    FOR EACH attrb
        WHERE attrb.attr_mfgroup = mfgroup-list:SCREEN-VALUE AND attrb.attr_tab = i
            EXCLUSIVE-LOCK:
      attrb.attr_tab = 0.
    END.
    FOR EACH attrb
        WHERE attrb.attr_mfgroup = mfgroup-list:SCREEN-VALUE AND attrb.attr_tab = j
            EXCLUSIVE-LOCK:
      attrb.attr_tab = i.
    END.
    FOR EACH attrb
        WHERE attrb.attr_mfgroup = mfgroup-list:SCREEN-VALUE AND attrb.attr_tab = 0
            EXCLUSIVE-LOCK:
      attrb.attr_tab = j.
    END.
  END.
  APPLY "VALUE-CHANGED" TO tab-labels IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move_Widget C-Win 
PROCEDURE Move_Widget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
    current-widget = FRAME Folder-Frm:HANDLE
    current-widget = current-widget:FIRST-CHILD
    current-widget = current-widget:FIRST-CHILD.
  DO WHILE current-widget NE ?:
    IF current-widget:SELECTED THEN
    DO:
      RUN Update_attrb.
      IF NOT CAN-DO("TEXT,TOGGLE-BOX",current-widget:TYPE) THEN
      DO:
        label-widget = current-widget:SIDE-LABEL-HANDLE.
        DELETE WIDGET label-widget.
      END.
      save-widget = current-widget:NEXT-SIBLING.
      DELETE WIDGET current-widget.
      RUN Dynamic_Widget.
      current-widget = save-widget.
    END.
    ELSE
    current-widget = current-widget:NEXT-SIBLING.
  END.
  DISABLE Btn_Cut Btn_Copy WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE New_Widget C-Win 
PROCEDURE New_Widget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  CREATE attrb.
  RUN Set_attr_id.
  ASSIGN
    attrb.attr_mfgroup = mfgroup-list:SCREEN-VALUE IN FRAME {&FRAME-NAME}
    attrb.attr_tab = INTEGER(mfgroup-tab:SCREEN-VALUE IN FRAME {&FRAME-NAME})
    attrb.attr_type = current-label
    attrb.attr_label = "NEW " + current-label
    attrb.attr_values = IF current-label = "RADIO-SET" THEN "Empty,0" 
                   ELSE IF current-label = "SLIDER" THEN "0,100"
                   ELSE IF current-label = "TEXT" THEN ""
                   ELSE "Empty"
    attrb.attr_default = IF CAN-DO("RADIO-SET,SLIDER",current-label) THEN "0"
                    ELSE IF current-label = "TEXT" THEN ""
                    ELSE IF current-label = "TOGGLE-BOX" THEN "no"
                    ELSE "Empty"
    attrb.attr_x = LAST-EVENT:X
    attrb.attr_y = LAST-EVENT:Y
    attrb.attr_width = IF current-label = "SLIDER" THEN 70 ELSE 105
    attrb.attr_height = IF current-label = "SLIDER" THEN 31 ELSE 21
    attrb.attr_settings = IF current-label = "EDITOR" THEN "no,yes"
                     ELSE IF current-label = "FILL-IN" THEN "X(8)"
                     ELSE IF current-label = "COMBO-BOX" THEN "X(8)"
                     ELSE IF current-label = "RADIO-SET" THEN "yes"
                     ELSE IF current-label = "SLIDER" THEN "yes"
                     ELSE IF current-label = "TEXT" THEN "4"
                     ELSE ""
    attrb.attr_datatype = IF current-label = "SLIDER" THEN "integer"
                     ELSE IF current-label = "TOGGLE-BOX" THEN "logical"
                     ELSE "character".
  RUN Dynamic_Widget.
  IF RETURN-VALUE = "CREATE-ERROR" THEN
  DELETE attrb.
  ELSE
  DO:
    dyn-widget:SELECTED = YES.
    ENABLE Btn_Cut Btn_Copy WITH FRAME {&FRAME-NAME}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Paste_from_Clipboard C-Win 
PROCEDURE Paste_from_Clipboard :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH wtbl-clipboard:
    CREATE attrb.
    RUN Set_attr_id.
    ASSIGN
      attrb.attr_mfgroup = mfgroup-list:SCREEN-VALUE IN FRAME {&FRAME-NAME}
      attrb.attr_tab = INTEGER(mfgroup-tab:SCREEN-VALUE IN FRAME {&FRAME-NAME})
      attrb.attr_type = wtbl-clipboard.attr_type
      attrb.attr_label = wtbl-clipboard.attr_label
      attrb.attr_values = wtbl-clipboard.attr_values
      attrb.attr_default = wtbl-clipboard.attr_default
      attrb.attr_x = wtbl-clipboard.attr_x
      attrb.attr_y = wtbl-clipboard.attr_y
      attrb.attr_height = wtbl-clipboard.attr_height
      attrb.attr_width = wtbl-clipboard.attr_width
      attrb.attr_settings = wtbl-clipboard.attr_settings
      attrb.attr_datatype = wtbl-clipboard.attr_datatype.
    RUN Dynamic_Widget.
    dyn-widget:SELECTED = YES.
    ENABLE Btn_Cut Btn_Copy WITH FRAME {&FRAME-NAME}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Property_Sheet C-Win 
PROCEDURE Property_Sheet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
    current-widget = FRAME Folder-Frm:HANDLE
    current-widget = current-widget:FIRST-CHILD
    current-widget = current-widget:FIRST-CHILD.
  DO WHILE current-widget NE ?:
    IF current-widget:SELECTED THEN
    DO WITH FRAME {&FRAME-NAME}:
      RUN nosweat/mfdialog.w (ENTRY(1,current-widget:PRIVATE-DATA)).
      FIND attrb
          WHERE ROWID(attrb) = TO-ROWID(ENTRY(1,current-widget:PRIVATE-DATA))
              NO-LOCK.
      IF NOT CAN-DO("TEXT,TOGGLE-BOX",current-widget:TYPE) THEN
      DO:
        label-widget = current-widget:SIDE-LABEL-HANDLE.
        DELETE WIDGET label-widget.
      END.
      save-widget = current-widget:NEXT-SIBLING.
      DELETE WIDGET current-widget.
      RUN Dynamic_Widget.
      dyn-widget:SELECTED = YES.
      RETURN.
    END.
    ELSE
    current-widget = current-widget:NEXT-SIBLING.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rename_Group C-Win 
PROCEDURE Rename_Group :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE save_labels AS CHARACTER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF NOT ldummy THEN
    MESSAGE "Rename Group" mfgroup-list:SCREEN-VALUE "to" cdummy "?" VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE " Change Group " UPDATE ldummy.
    ELSE
    MESSAGE "Group" cdummy "Already Exists, Merge" mfgroup-list:SCREEN-VALUE "with" cdummy "?" VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE " Change Group " UPDATE ldummy.
    IF ldummy THEN
    DO:
      FOR EACH attrb WHERE attrb.attr_mfgroup = mfgroup-list:SCREEN-VALUE EXCLUSIVE-LOCK:
        attrb.attr_mfgroup = cdummy.
      END.
      DO i = mfgroup-list:NUM-ITEMS TO 1 BY -1:
        IF mfgroup-list:ENTRY(i) = mfgroup-field:SCREEN-VALUE THEN
        current-label = "yes".
        IF mfgroup-list:ENTRY(i) = mfgroup-list:SCREEN-VALUE THEN
        DO:
          FIND mfgroup WHERE mfgroup.mfgroup = mfgroup-list:ENTRY(i) EXCLUSIVE-LOCK.
          ASSIGN
            save_labels = mfgroup.tab-labels
            ldummy = mfgroup-list:DELETE(i).
          DELETE mfgroup.
        END.
        IF mfgroup-list:ENTRY(i) = mfgroup-field:SCREEN-VALUE THEN
        current-label = "yes".
      END.
      IF current-label NE "yes" THEN
      DO:
        CREATE mfgroup.
        ASSIGN
          mfgroup.mfgroup = mfgroup-field:SCREEN-VALUE
          mfgroup.tab-label = save_labels
          ldummy = mfgroup-list:ADD-LAST(mfgroup-field:SCREEN-VALUE)
          current-label = "".
      END.
    END.
    ELSE
    cdummy = mfgroup-list:SCREEN-VALUE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rename_Tab C-Win 
PROCEDURE Rename_Tab :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    FIND mfgroup WHERE mfgroup.mfgroup = mfgroup-list:SCREEN-VALUE EXCLUSIVE-LOCK.
    ASSIGN
      ldummy = tab-labels:REPLACE(tab-field:SCREEN-VALUE,tab-labels:SCREEN-VALUE)
      tab-labels:SCREEN-VALUE = tab-field:SCREEN-VALUE
      mfgroup.tab-label = tab-labels:LIST-ITEMS.
    RUN Create_Tabs.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Resize_Widget C-Win 
PROCEDURE Resize_Widget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER this-widget AS WIDGET-HANDLE NO-UNDO.

  current-widget = this-widget:HANDLE.
  RUN Update_attrb.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Save_Layout C-Win 
PROCEDURE Save_Layout :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE textstring AS CHARACTER NO-UNDO.

  IF CAN-FIND(FIRST del-mfvalues) THEN
  DO:
    ldummy = NO.
    MESSAGE "Attribute Value Records are marked for DELETION, Continue? (yes/no)"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ldummy.
    IF ldummy THEN
    FOR EACH del-mfvalues:
      FIND mfvalues WHERE RECID(mfvalues) = del-mfvalues.mfvalues_recid EXCLUSIVE-LOCK.
      DELETE mfvalues.
      DELETE del-mfvalues.
    END.
  END.
  OUTPUT TO VALUE("users/" + USERID("NOSWEAT") + "/mfgroup.dat").
  EXPORT mfgroup-list:LIST-ITEMS IN FRAME {&FRAME-NAME}.
  DO i = 1 TO mfgroup-list:NUM-ITEMS WITH FRAME {&FRAME-NAME}:
    FIND mfgroup WHERE mfgroup.mfgroup = ENTRY(i,mfgroup-list:LIST-ITEMS) NO-LOCK NO-ERROR.
    IF AVAILABLE mfgroup THEN
    EXPORT mfgroup.tab-label.
    ELSE
    EXPORT " ".
  END.
  OUTPUT CLOSE.
  OUTPUT TO VALUE("users/" + USERID("NOSWEAT") + "/miscflds.dat").
  FOR EACH attrb NO-LOCK:
    EXPORT attrb.
  END.
  OUTPUT CLOSE.
  FIND FIRST mfdata EXCLUSIVE-LOCK.
  ASSIGN
    mfdata.mfgroup_data = ""
    mfdata.miscflds_data = "".
  INPUT FROM VALUE("users/" + USERID("NOSWEAT") + "/mfgroup.dat").
  REPEAT:
    IMPORT UNFORMATTED textstring.
    mfdata.mfgroup_data = mfdata.mfgroup_data + textstring + ";".
  END.
  INPUT CLOSE.
  INPUT FROM VALUE("users/" + USERID("NOSWEAT") + "/miscflds.dat").
  REPEAT:
    IMPORT UNFORMATTED textstring.
    mfdata.miscflds_data = mfdata.miscflds_data + textstring + ";".
  END.
  INPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Selection_Widget C-Win 
PROCEDURE Selection_Widget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF FRAME Folder-Frm:NUM-SELECTED-WIDGETS = 0 THEN
  ENABLE Btn_Property Btn_Cut Btn_Copy WITH FRAME {&FRAME-NAME}.
  ELSE
  DISABLE Btn_Property WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Select_New_Widget_Type C-Win 
PROCEDURE Select_New_Widget_Type :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER new-widget-type AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER mouse-pointer AS CHARACTER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      pointer-label:BGCOLOR = 15
      combo-box-label:BGCOLOR = 15
      editor-label:BGCOLOR = 15
      fill-in-label:BGCOLOR = 15
      radio-set-label:BGCOLOR = 15
      selection-list-label:BGCOLOR = 15
      slider-label:BGCOLOR = 15
      text-label:BGCOLOR = 15
      toggle-box-label:BGCOLOR = 15.
    ENABLE {&LIST-3}.

    CASE new-widget-type:
      WHEN "" THEN
      DO:
        pointer-label:BGCOLOR = 7.
        DISABLE Btn_pointer.
      END.
&scoped-define widgettype combo-box
      WHEN "{&widgettype}" THEN
      DO:
        {&widgettype}-label:BGCOLOR = 7.
        DISABLE Btn_{&widgettype}.
      END.
&scoped-define widgettype editor
      WHEN "{&widgettype}" THEN
      DO:
        {&widgettype}-label:BGCOLOR = 7.
        DISABLE Btn_{&widgettype}.
      END.
&scoped-define widgettype fill-in
      WHEN "{&widgettype}" THEN
      DO:
        {&widgettype}-label:BGCOLOR = 7.
        DISABLE Btn_{&widgettype}.
      END.
&scoped-define widgettype radio-set
      WHEN "{&widgettype}" THEN
      DO:
        {&widgettype}-label:BGCOLOR = 7.
        DISABLE Btn_{&widgettype}.
      END.
&scoped-define widgettype selection-list
      WHEN "{&widgettype}" THEN
      DO:
        {&widgettype}-label:BGCOLOR = 7.
        DISABLE Btn_{&widgettype}.
      END.
&scoped-define widgettype slider
      WHEN "{&widgettype}" THEN
      DO:
        {&widgettype}-label:BGCOLOR = 7.
        DISABLE Btn_{&widgettype}.
      END.
&scoped-define widgettype text
      WHEN "{&widgettype}" THEN
      DO:
        {&widgettype}-label:BGCOLOR = 7.
        DISABLE Btn_{&widgettype}.
      END.
&scoped-define widgettype toggle-box
      WHEN "{&widgettype}" THEN
      DO:
        {&widgettype}-label:BGCOLOR = 7.
        DISABLE Btn_{&widgettype}.
      END.
    END CASE.
  END.

  ASSIGN
    ldummy = FRAME Folder-Frm:LOAD-MOUSE-POINTER(mouse-pointer)
    current-label = new-widget-type.

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
  {methods/setfocus.i mfgroup-list}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set_attr_id C-Win 
PROCEDURE Set_attr_id :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO i = 1 TO last_id:
    IF NOT CAN-FIND(attrb WHERE attr_id = STRING(i)) THEN
    LEAVE.
  END.
  IF i GT last_id THEN
  last_id = i.
  attrb.attr_id = STRING(i).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set_Tab_Order C-Win 
PROCEDURE Set_Tab_Order :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      ldummy = Btn_Add_Group:MOVE-AFTER(mfgroup-list:HANDLE)
      ldummy = Btn_Rename_Group:MOVE-AFTER(Btn_Add_Group:HANDLE)
      ldummy = Btn_Delete_Group:MOVE-AFTER(Btn_Rename_Group:HANDLE)
      ldummy = mfgroup-tab:MOVE-AFTER(Btn_Exit:HANDLE)
      ldummy = Btn_Next_Tab:MOVE-AFTER(mfgroup-tab:HANDLE)
      ldummy = Btn_Prev_Tab:MOVE-AFTER(Btn_Next_Tab:HANDLE)
      ldummy = tab-labels:MOVE-AFTER(Btn_Prev_Tab:HANDLE)
      ldummy = Btn_Add_Tab:MOVE-AFTER(tab-labels:HANDLE)
      ldummy = Btn_Rename_Tab:MOVE-AFTER(Btn_Add_Tab:HANDLE)
      ldummy = Btn_Delete_Tab:MOVE-AFTER(Btn_Rename_Tab:HANDLE)
      ldummy = Btn_Down_Tab:MOVE-AFTER(Btn_Delete_Tab:HANDLE)
      ldummy = Btn_Up_Tab:MOVE-AFTER(Btn_Down_Tab:HANDLE).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Update_attrb C-Win 
PROCEDURE Update_attrb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND attrb
      WHERE ROWID(attrb) = TO-ROWID(ENTRY(1,current-widget:PRIVATE-DATA))
          EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAILABLE attrb THEN
  DO:
    CREATE attrb.
    RUN Set_attr_id.
  END.
  ASSIGN
    attrb.attr_mfgroup = mfgroup-list:SCREEN-VALUE IN FRAME {&FRAME-NAME}
    attrb.attr_tab = INTEGER(ENTRY(2,current-widget:PRIVATE-DATA))
    attrb.attr_type = current-widget:TYPE
    attrb.attr_label = IF current-widget:TYPE = "TOGGLE-BOX" THEN current-widget:LABEL
      ELSE IF current-widget:TYPE = "TEXT" THEN current-widget:SCREEN-VALUE
      ELSE SUBSTR(current-widget:LABEL,1,LENGTH(current-widget:LABEL) - 1)
    attrb.attr_values =
           IF CAN-DO("COMBO-BOX,SELECTION-LIST",current-widget:TYPE) THEN current-widget:LIST-ITEMS
      ELSE IF current-widget:TYPE = "RADIO-SET" THEN current-widget:RADIO-BUTTONS
      ELSE IF current-widget:TYPE = "SLIDER" THEN STRING(current-widget:MIN-VALUE) + "," + STRING(current-widget:MAX-VALUE)
      ELSE ""
    attrb.attr_default = IF current-widget:TYPE = "TEXT" THEN ""
      ELSE current-widget:SCREEN-VALUE
    attrb.attr_x = current-widget:X
    attrb.attr_y = current-widget:Y
    attrb.attr_height = current-widget:HEIGHT-PIXELS
    attrb.attr_width = current-widget:WIDTH-PIXELS
    attrb.attr_settings =
           IF attrb.attr_type = "EDITOR" THEN
             STRING(current-widget:SCROLLBAR-HORIZONTAL) + "," + STRING(current-widget:SCROLLBAR-VERTICAL)
      ELSE IF attrb.attr_type = "FILL-IN" THEN current-widget:FORMAT
      ELSE IF attrb.attr_type = "COMBO-BOX" THEN current-widget:FORMAT
      ELSE IF attrb.attr_type = "RADIO-SET" THEN STRING(current-widget:HORIZONTAL)
      ELSE IF attrb.attr_type = "SLIDER" THEN STRING(current-widget:HORIZONTAL)
      ELSE IF attrb.attr_type = "TEXT" THEN STRING(current-widget:FONT)
      ELSE ""
    attrb.attr_datatype = current-widget:DATA-TYPE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


