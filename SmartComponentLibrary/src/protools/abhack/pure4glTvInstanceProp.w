&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Attribute-Dlg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Attribute-Dlg 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation ("PSC"),       *
* 14 Oak Park, Bedford, MA 01730, and other contributors as listed   *
* below.  All Rights Reserved.                                       *
*                                                                    *
* The Initial Developer of the Original Code is PSC.  The Original   *
* Code is Progress IDE code released to open source December 1, 2000.*
*                                                                    *
* The contents of this file are subject to the Possenet Public       *
* License Version 1.0 (the "License"); you may not use this file     *
* except in compliance with the License.  A copy of the License is   *
* available as of the date of this notice at                         *
* http://www.possenet.org/license.html                               *
*                                                                    *
* Software distributed under the License is distributed on an "AS IS"*
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. You*
* should refer to the License for the specific language governing    *
* rights and limitations under the License.                          *
*                                                                    *
* Contributors:                                                      *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: pure4glTvd.w 

  Description: Dialog for getting settable attributes for a 
               SmartTreeView.

  Input Parameters:
     phSmrtObj -- Procedure Handle of calling pure4glTv.

  Output Parameters:
      <none>

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/*
{adeuib/uibhlp.i}          /* Help File Preprocessor Directives         */
  */
  
&GLOBAL-DEFINE WIN95-Btn YES 

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER phSmrtObj AS HANDLE NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE FocSelNodeFgColor AS INTEGER    NO-UNDO.
DEFINE VARIABLE FocSelNodeBgColor AS INTEGER    NO-UNDO.
DEFINE VARIABLE UnfSelNodeFgColor AS INTEGER    NO-UNDO.
DEFINE VARIABLE UnfSelNodeBgColor AS INTEGER    NO-UNDO.
DEFINE VARIABLE tvnodeDefaultFont AS INTEGER    NO-UNDO.

&GLOBAL-DEFINE xpresizeVertical
&GLOBAL-DEFINE xpresizeHorizontal
&GLOBAL-DEFINE xpDragSource
&GLOBAL-DEFINE xpautoSort
DEFINE VARIABLE ghProp AS HANDLE     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Attribute-Dlg

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-5 cbTreeStyle ~
cbwindowsSkin tvIterationHeight btnDefaultFont btnFocusedSelectedNodeColors ~
btnUnfocusedSelectedNodeColors cbwineMode DragSource tgAutoSort ~
resizeHorizontal resizeVertical picCacheCoef labCacheCoef ~
MSkeyScrollForcePaint buOK buCancel BtnAbout 
&Scoped-Define DISPLAYED-OBJECTS cbTreeStyle cbwindowsSkin ~
tvIterationHeight nodeDefaultFont FocusedSelectedNodeColors ~
UnfocusedSelectedNode cbwineMode DragSource tgAutoSort resizeHorizontal ~
resizeVertical picCacheCoef labCacheCoef MSkeyScrollForcePaint 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CalledFromPure4GlTvInstPropDialog Attribute-Dlg 
FUNCTION CalledFromPure4GlTvInstPropDialog RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getInstalledSkin Attribute-Dlg 
FUNCTION getInstalledSkin RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnAbout 
     LABEL "About pure4glTv" 
     SIZE 22 BY 1.14.

DEFINE BUTTON btnDefaultFont 
     LABEL "font" 
     SIZE 8 BY 1.91 TOOLTIP "To choose the default font of the treeview node.".

DEFINE BUTTON btnFocusedSelectedNodeColors 
     LABEL "Color" 
     SIZE 8 BY 1.91 TOOLTIP "To choose the color of the selected node when the treeview has the focus".

DEFINE BUTTON btnUnfocusedSelectedNodeColors 
     LABEL "Color" 
     SIZE 8 BY 1.91 TOOLTIP "To choose the color of the selected node when the treeview HAS NOT the focus".

DEFINE BUTTON buCancel AUTO-END-KEY DEFAULT 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON buOK AUTO-GO DEFAULT 
     LABEL "&OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE cbTreeStyle AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 3 
     LABEL "Tree Style" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEM-PAIRS "Image & text",1,
                     "Plus/minus & text",2,
                     "Plus/minus & image & text",3
     DROP-DOWN-LIST
     SIZE 41 BY 1 TOOLTIP "Style of the treeview.  See result in the picture on the right."
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cbwindowsSkin AS CHARACTER FORMAT "X(256)":U 
     LABEL "Windows skin" 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "Classic","Aero","Luna","Royale","Automatic" 
     DROP-DOWN-LIST
     SIZE 24 BY 1 TOOLTIP "Use Automatic for real usage, Classic or XPStyle to test pure4GlTv"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cbwineMode AS CHARACTER FORMAT "X(256)":U INITIAL "no" 
     LABEL "wineMode" 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEM-PAIRS "Force wine","yes",
                     "Force MS WINDOWS","no",
                     "Automatic","Automatic"
     DROP-DOWN-LIST
     SIZE 22 BY 1 TOOLTIP "Use Automatic for real usage, other values to test the rendering optimization"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE DragSource AS CHARACTER FORMAT "X(256)":U 
     LABEL "DragSource" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "none","some","all" 
     DROP-DOWN-LIST
     SIZE 14 BY 1 TOOLTIP "Tooltip set in main block"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tvIterationHeight AS CHARACTER 
     LABEL "Iteration Height" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "17","33" 
     DROP-DOWN
     SIZE 9 BY 1 TOOLTIP "Free Value  default is 17"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FocusedSelectedNodeColors AS CHARACTER FORMAT "X(256)":U INITIAL "Focused Selected Node" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 TOOLTIP "Color of the selected node when the treeview has the focus" NO-UNDO.

DEFINE VARIABLE labCacheCoef AS DECIMAL FORMAT ">9.99":U INITIAL 1 
     LABEL "Label Cache Coefficient" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .95
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE nodeDefaultFont AS CHARACTER FORMAT "X(256)":U INITIAL "Node default font" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 TOOLTIP "Default font of node in the treeview"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE picCacheCoef AS DECIMAL FORMAT ">9.99":U INITIAL 1 
     LABEL "Picture Cache Coefficient" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE UnfocusedSelectedNode AS CHARACTER FORMAT "X(256)":U INITIAL "Unfocused Selected Node" 
     VIEW-AS FILL-IN 
     SIZE 27 BY .95 TOOLTIP "Color of the selected node when the treeview HAS NOT the focus" NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 72 BY 10.48.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 72 BY 6.19.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 51 BY 6.67
     BGCOLOR 15 .

DEFINE VARIABLE MSkeyScrollForcePaint AS LOGICAL INITIAL no 
     LABEL "MSkeyScrollForcePaint" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81 NO-UNDO.

DEFINE VARIABLE resizeHorizontal AS LOGICAL INITIAL no 
     LABEL "resizeHorizontal" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 TOOLTIP "Query this property from the container at run time to call resizeObject" NO-UNDO.

DEFINE VARIABLE resizeVertical AS LOGICAL INITIAL no 
     LABEL "resizeVertical" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 TOOLTIP "Query this property from the container at run time to call resizeObject" NO-UNDO.

DEFINE VARIABLE tgAutoSort AS LOGICAL INITIAL no 
     LABEL "Auto Sort" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.2 BY .81 TOOLTIP "Taken into account *only* when adding nodes (one can move them afterwards)" NO-UNDO.

DEFINE IMAGE imgTreeStyle
     FILENAME "adeicon/blank":U
     SIZE 6.4 BY .86 TOOLTIP "This is how a node will look like in the treeview".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Attribute-Dlg
     cbTreeStyle AT ROW 1.95 COL 19 COLON-ALIGNED
     cbwindowsSkin AT ROW 3.38 COL 19 COLON-ALIGNED
     tvIterationHeight AT ROW 3.38 COL 61 COLON-ALIGNED
     btnDefaultFont AT ROW 5.05 COL 58
     nodeDefaultFont AT ROW 5.52 COL 29 COLON-ALIGNED NO-LABEL
     btnFocusedSelectedNodeColors AT ROW 7.19 COL 58
     FocusedSelectedNodeColors AT ROW 7.67 COL 29 COLON-ALIGNED NO-LABEL
     btnUnfocusedSelectedNodeColors AT ROW 9.33 COL 58
     UnfocusedSelectedNode AT ROW 9.81 COL 27 COLON-ALIGNED NO-LABEL
     cbwineMode AT ROW 13.38 COL 19 COLON-ALIGNED
     DragSource AT ROW 13.38 COL 56 COLON-ALIGNED
     tgAutoSort AT ROW 14.81 COL 10
     resizeHorizontal AT ROW 14.81 COL 32
     resizeVertical AT ROW 14.81 COL 55
     picCacheCoef AT ROW 16.24 COL 28 COLON-ALIGNED
     labCacheCoef AT ROW 16.24 COL 61 COLON-ALIGNED
     MSkeyScrollForcePaint AT ROW 17.67 COL 10
     buOK AT ROW 19.57 COL 4
     buCancel AT ROW 19.57 COL 20
     BtnAbout AT ROW 19.57 COL 53
     " Behaviour" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 12.62 COL 4.2
     " Appearance" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 1.24 COL 4.6
     RECT-1 AT ROW 1.24 COL 3
     RECT-2 AT ROW 12.91 COL 3
     RECT-5 AT ROW 4.81 COL 21
     SPACE(4.39) SKIP(9.84)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "SmartTreeView Properties":L
         DEFAULT-BUTTON buOK CANCEL-BUTTON buCancel.

DEFINE FRAME frameImg
     imgTreeStyle AT ROW 1.14 COL 2.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 63 ROW 1.95
         SIZE 9 BY 1.1
         BGCOLOR 15 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* REPARENT FRAME */
ASSIGN FRAME frameImg:FRAME = FRAME Attribute-Dlg:HANDLE.

/* SETTINGS FOR DIALOG-BOX Attribute-Dlg
   FRAME-NAME L-To-R                                                    */
ASSIGN 
       FRAME Attribute-Dlg:SCROLLABLE       = FALSE
       FRAME Attribute-Dlg:HIDDEN           = TRUE.

ASSIGN 
       btnFocusedSelectedNodeColors:AUTO-RESIZE IN FRAME Attribute-Dlg      = TRUE.

ASSIGN 
       btnUnfocusedSelectedNodeColors:AUTO-RESIZE IN FRAME Attribute-Dlg      = TRUE.

/* SETTINGS FOR FILL-IN FocusedSelectedNodeColors IN FRAME Attribute-Dlg
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nodeDefaultFont IN FRAME Attribute-Dlg
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN UnfocusedSelectedNode IN FRAME Attribute-Dlg
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME frameImg
                                                                        */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Attribute-Dlg
/* Query rebuild information for DIALOG-BOX Attribute-Dlg
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Attribute-Dlg */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Attribute-Dlg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Attribute-Dlg Attribute-Dlg
ON GO OF FRAME Attribute-Dlg /* SmartTreeView Properties */
DO:  /* Put the properties back into the SmartTreeView. */

DEFINE VARIABLE ghProp AS HANDLE     NO-UNDO.
DEFINE VARIABLE glADMOK AS LOGICAL    NO-UNDO.

ASSIGN {&DISPLAYED-OBJECTS}.
 
IF picCacheCoef < 1 OR labCacheCoef < 1 THEN DO:
    MESSAGE "The Picture Cache Coefficient and the Label Cache Coefficient"
     "must be greater than 1."
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN NO-APPLY.
END.
 
 
DYNAMIC-FUNC('setTreeStyle':U         IN phSmrtObj ,cbTreeStyle).
DYNAMIC-FUNC('settvIterationHeight':U IN phSmrtObj ,tvIterationHeight).
DYNAMIC-FUNC('setFocSelNodeBgColor':U IN phSmrtObj ,FocSelNodeBgColor).
DYNAMIC-FUNC('setFocSelNodeFgColor':U IN phSmrtObj ,FocSelNodeFgColor).
DYNAMIC-FUNC('setUnfSelNodeBgColor':U IN phSmrtObj ,UnfSelNodeBgColor).
DYNAMIC-FUNC('setUnfSelNodeFgColor':U IN phSmrtObj ,UnfSelNodeFgColor).
DYNAMIC-FUNC('setwindowsSkin':U       IN phSmrtObj ,cbwindowsSkin).
DYNAMIC-FUNC('setwineMode':U          IN phSmrtObj ,cbwineMode).
DYNAMIC-FUNC('setpicCacheCoef':U      IN phSmrtObj ,picCacheCoef).
DYNAMIC-FUNC('setlabCacheCoef':U      IN phSmrtObj ,labCacheCoef).
DYNAMIC-FUNC('settvnodeDefaultFont':U IN phSmrtObj ,tvnodeDefaultFont).
DYNAMIC-FUNC('setMSkeyScrollForcePaint':U IN phSmrtObj ,MSkeyScrollForcePaint).



{set resizeVertical   resizeVertical   phSmrtObj}.
{set resizeHorizontal resizeHorizontal phSmrtObj}.
{set dragSource dragSource phSmrtObj}.
{set autosort tgautoSort phSmrtObj}.

RUN initializeObject IN phSmrtObj.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Attribute-Dlg Attribute-Dlg
ON HELP OF FRAME Attribute-Dlg /* SmartTreeView Properties */
OR HELP OF FRAME {&FRAME-NAME}
DO: 
  MESSAGE 'No help provided yet.  You might want to read the source code of pure4glTv'
   'and write the help, it is full of comment'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
/*
  /* Help for this Frame */
  RUN adecomm/_adehelp.p
                ("ICAB":U, "CONTEXT":U, {&SmTreeView_Instance_Properties_Dialog_Box}  , "":U).
*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Attribute-Dlg Attribute-Dlg
ON WINDOW-CLOSE OF FRAME Attribute-Dlg /* SmartTreeView Properties */
DO:
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnAbout
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnAbout Attribute-Dlg
ON CHOOSE OF BtnAbout IN FRAME Attribute-Dlg /* About pure4glTv */
DO:
  MESSAGE "pure4glTv is a Treeview made in pure 4GL designed by Sebastien Lacroix"
   "Technical Support Engineer at the Progress European Tech Support Center, October 2004."
   "~nAchieved mainly at home on Linux Mandrake with KDE and Wine."
   VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDefaultFont
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDefaultFont Attribute-Dlg
ON CHOOSE OF btnDefaultFont IN FRAME Attribute-Dlg /* font */
DO:
  DEFINE VARIABLE dummy          AS INTEGER    NO-UNDO.
  
  RUN adecomm/_chsfont.p  (INPUT "Choose the default font of the Treeview",
                           INPUT 0,
                           INPUT-OUTPUT tvnodeDefaultFont,
                           OUTPUT dummy).
                           
  /* unknown makes problem in the instance properties... */
  IF tvnodeDefaultFont = ? THEN tvnodeDefaultFont = 1.
  
  nodeDefaultFont:FONT = tvnodeDefaultFont.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFocusedSelectedNodeColors
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFocusedSelectedNodeColors Attribute-Dlg
ON CHOOSE OF btnFocusedSelectedNodeColors IN FRAME Attribute-Dlg /* Color */
DO:
  DEFINE VARIABLE l_ok           AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE parent_bgcolor AS INTEGER INITIAL ? NO-UNDO.
  DEFINE VARIABLE parent_fgcolor AS INTEGER INITIAL ? NO-UNDO. 
  DEFINE VARIABLE dummy          AS INTEGER    NO-UNDO.
   
  ASSIGN
   parent_bgcolor = ?
   parent_fgcolor = ?.
  
  RUN adecomm/_chscolr.p
       (INPUT "Choose the Color of the Selected Node when the TV has the Focus",
        INPUT "",
        INPUT FALSE,
        INPUT parent_bgcolor,
        INPUT parent_fgcolor,
        INPUT ?,
        INPUT-OUTPUT FocSelNodeBgColor,
        INPUT-OUTPUT FocSelNodeFgColor,
        INPUT-OUTPUT dummy,
        OUTPUT l_ok).
  ASSIGN
   FocusedSelectedNodeColors:BGCOLOR = FocSelNodeBgColor
   FocusedSelectedNodeColors:FGCOLOR = FocSelNodeFgColor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUnfocusedSelectedNodeColors
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUnfocusedSelectedNodeColors Attribute-Dlg
ON CHOOSE OF btnUnfocusedSelectedNodeColors IN FRAME Attribute-Dlg /* Color */
DO:
  DEFINE VARIABLE l_ok           AS LOGICAL           NO-UNDO.
  DEFINE VARIABLE parent_bgcolor AS INTEGER INITIAL ? NO-UNDO.
  DEFINE VARIABLE parent_fgcolor AS INTEGER INITIAL ? NO-UNDO. 
  DEFINE VARIABLE dummy          AS INTEGER    NO-UNDO.
   
  ASSIGN
   parent_bgcolor = ?
   parent_fgcolor = ?.
  
  RUN adecomm/_chscolr.p
       (INPUT "Choose the Color of the Selected Node when the TV has the Focus",
        INPUT "",
        INPUT FALSE,
        INPUT parent_bgcolor,
        INPUT parent_fgcolor,
        INPUT ?,
        INPUT-OUTPUT unfSelNodeBgColor,
        INPUT-OUTPUT unfSelNodeFgColor,
        INPUT-OUTPUT dummy,
        OUTPUT l_ok).
  ASSIGN
   UnfocusedSelectedNode:BGCOLOR = unfSelNodeBgColor
   UnfocusedSelectedNode:FGCOLOR = unfSelNodeFgColor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbTreeStyle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbTreeStyle Attribute-Dlg
ON VALUE-CHANGED OF cbTreeStyle IN FRAME Attribute-Dlg /* Tree Style */
DO:
  CASE {&SELF-NAME}:SCREEN-VALUE:
    WHEN "1" THEN imgTreeStyle:LOAD-IMAGE("tvpics/FoldOpen.bmp") IN FRAME frameImg.
    WHEN "2" THEN imgTreeStyle:LOAD-IMAGE("tvpics/plus.bmp").
    WHEN "3" THEN imgTreeStyle:LOAD-IMAGE("tvpics/FoldMinus.bmp").
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbwindowsSkin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbwindowsSkin Attribute-Dlg
ON VALUE-CHANGED OF cbwindowsSkin IN FRAME Attribute-Dlg /* Windows skin */
DO:
  /* 29-oct-2004  now it is supported 
  IF SELF:SCREEN-VALUE = "automatic" THEN DO:
      MESSAGE "'Automatic' is not supported yet"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      SELF:SCREEN-VALUE = {&SELF-NAME}.
      RETURN.
  END.*/
  {&SELF-NAME} = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbwineMode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbwineMode Attribute-Dlg
ON VALUE-CHANGED OF cbwineMode IN FRAME Attribute-Dlg /* wineMode */
DO:
  IF SELF:SCREEN-VALUE = "?" THEN DO:
      MESSAGE "'Automatic' is not supported yet"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      SELF:SCREEN-VALUE = {&SELF-NAME}.
      RETURN.
  END.
  {&SELF-NAME} = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tvIterationHeight
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tvIterationHeight Attribute-Dlg
ON VALUE-CHANGED OF tvIterationHeight IN FRAME Attribute-Dlg /* Iteration Height */
DO:
  DEFINE VARIABLE idummy AS INTEGER    NO-UNDO.
  
  /* allow only integer */
  idummy = INT(SELF:SCREEN-VALUE) NO-ERROR.
  IF ERROR-STATUS:ERROR
   OR idummy <= 0
   OR LENGTH(SELF:SCREEN-VALUE) > 2
   THEN SELF:SCREEN-VALUE = STRING({&SELF-NAME}).
  
  {&SELF-NAME} = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Attribute-Dlg 


/* ************************ Standard Setup **************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Define Context ID's for HELP files */
{ src/adm2/support/admhlp.i }    

/* ***************************  Main Block  *************************** */

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   
  /* Get the values of the attributes in the SmartTreeView that can be 
   * changed in this dialog-box. */
  cbwindowsSkin:LIST-ITEMS = getInstalledSkin().
  RUN fetchSmartObjectAttributes.
  
  IF cbwindowsSkin:LOOKUP(cbwindowsSkin) = 0 THEN DO:
      MESSAGE "The skin" cbwindowsSkin "is not in list of supported skins" SKIP
          "'Automatic' will therefore be used instead"
          "If you want to add the supoprt to a specific skin, then create a directory"
          "with the name of the skin/theme in the 'tvskin' directory, with the necessary"
          "pictures in it"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      ASSIGN
          cbwindowsSkin = "Automatic".
  END.
  /* Enable the interface. */         
  RUN enable_UI.
  btnFocusedSelectedNodeColors:LOAD-IMAGE("adeicon/color1-u.bmp" , 2,2,32,32) NO-ERROR.
  btnUnfocusedSelectedNodeColors:LOAD-IMAGE("adeicon/color1-u.bmp" , 2,2,32,32) NO-ERROR.
  btnDefaultFont:LOAD-IMAGE("adeicon/font1-u.bmp" , 2,2,32,32) NO-ERROR.
  
  picCacheCoef:TOOLTIP =
   "Default is 1.  Greater value can improve rendering performance (especially on wine) with a kind of~n"
   + "over-caching that handles a higher number of rendering dynamic images than the number of iteration~n" 
   + "so some widgets can quickly be reused by switching their VISIBLE attribute (think of expand and collapse)".
  labCacheCoef:TOOLTIP = "Same as picCacheCoef for the label dynamic texts".
  
  APPLY 'VALUE-CHANGED' TO cbTreeStyle.
  btnDefaultFont:TOOLTIP = btnDefaultFont:TOOLTIP + "  See the gettvnodeDefaultFont API in pure4GLTv.w for more information".
  
  dragSource:TOOLTIP = "Nodes that can be a drag source.~n"
   + "When 'none', then no node can be a drag source~n"
   + "When 'some', then the node option 'dragSource' will enable the drag event for the node~n"
   + "When 'all' , all nodes will enable the drag event except those with the node option 'nodragSource'~n".

  MSkeyScrollForcePaint:TOOLTIP =
   "Used only on MS Windows.  Keep default of YES to avoid flickering when scrolling up/down by keeping~n"
   + "a navigation key pressed (the down/up button in the scrollbar do not make this problem)".
  
  /* Set the cursor */
  RUN adecomm/_setcurs.p ("":U).
 
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.  
END.

RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Attribute-Dlg  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Attribute-Dlg.
  HIDE FRAME frameImg.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Attribute-Dlg  _DEFAULT-ENABLE
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
  DISPLAY cbTreeStyle cbwindowsSkin tvIterationHeight nodeDefaultFont 
          FocusedSelectedNodeColors UnfocusedSelectedNode cbwineMode DragSource 
          tgAutoSort resizeHorizontal resizeVertical picCacheCoef labCacheCoef 
          MSkeyScrollForcePaint 
      WITH FRAME Attribute-Dlg.
  ENABLE RECT-1 RECT-2 RECT-5 cbTreeStyle cbwindowsSkin tvIterationHeight 
         btnDefaultFont btnFocusedSelectedNodeColors 
         btnUnfocusedSelectedNodeColors cbwineMode DragSource tgAutoSort 
         resizeHorizontal resizeVertical picCacheCoef labCacheCoef 
         MSkeyScrollForcePaint buOK buCancel BtnAbout 
      WITH FRAME Attribute-Dlg.
  VIEW FRAME Attribute-Dlg.
  {&OPEN-BROWSERS-IN-QUERY-Attribute-Dlg}
  ENABLE imgTreeStyle 
      WITH FRAME frameImg.
  {&OPEN-BROWSERS-IN-QUERY-frameImg}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fetchSmartObjectAttributes Attribute-Dlg 
PROCEDURE fetchSmartObjectAttributes :
/*------------------------------------------------------------------------------
 Requests the current setting for the SmartTreeView properties and sets the
 widgets on the screen accordingly.
------------------------------------------------------------------------------*/


DEFINE VARIABLE glADMOK AS LOGICAL    NO-UNDO.

ASSIGN 
 cbTreeStyle       = DYNAMIC-FUNC('getTreeStyle':U         IN phSmrtObj)
 tvIterationHeight = DYNAMIC-FUNC('gettvIterationHeight':U IN phSmrtObj)
 FocSelNodeBgColor = DYNAMIC-FUNC('getFocSelNodeBgColor':U IN phSmrtObj)
 FocSelNodeFgColor = DYNAMIC-FUNC('getFocSelNodeFgColor':U IN phSmrtObj)
 UnfSelNodeBgColor = DYNAMIC-FUNC('getUnfSelNodeBgColor':U IN phSmrtObj)
 UnfSelNodeFgColor = DYNAMIC-FUNC('getUnfSelNodeFgColor':U IN phSmrtObj)
 cbwindowsSkin     = DYNAMIC-FUNC('getwindowsSkin':U       IN phSmrtObj) 
 cbwineMode        = DYNAMIC-FUNC('getwineMode':U          IN phSmrtObj)
 picCacheCoef      = DYNAMIC-FUNC('getpicCacheCoef':U      IN phSmrtObj)
 labCacheCoef      = DYNAMIC-FUNC('getlabCacheCoef':U      IN phSmrtObj)
 tvnodeDefaultFont = DYNAMIC-FUNC('gettvnodeDefaultFont':U IN phSmrtObj).

/* tgautoSort                   = DYNAMIC-FUNC('getautoSort':U         IN phSmrtObj).*/


{get resizeVertical   resizeVertical   phSmrtObj}.
{get resizeHorizontal resizeHorizontal phSmrtObj}.
{get dragSource dragSource phSmrtObj}.
{get autoSort   tgAutoSort phSmrtObj}.
{get MSkeyScrollForcePaint   MSkeyScrollForcePaint phSmrtObj}.


/* default value */
IF resizeVertical = ?    THEN resizeVertical = YES.
IF resizeHorizontal = ?  THEN resizeHorizontal = YES.

ASSIGN   
 FocusedSelectedNodeColors:BGCOLOR IN FRAME {&FRAME-NAME} = FocSelNodeBgColor
 FocusedSelectedNodeColors:FGCOLOR = FocSelNodeFgColor
 UnfocusedSelectedNode:BGCOLOR     = unfSelNodeBgColor
 UnfocusedSelectedNode:FGCOLOR     = unfSelNodeFgColor
 nodeDefaultFont:FONT = tvnodeDefaultFont.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CalledFromPure4GlTvInstPropDialog Attribute-Dlg 
FUNCTION CalledFromPure4GlTvInstPropDialog RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  when chaning some properties in pure4GlTv, it will ask if it is
  done by this instanc eproperties dialog.  If yes, then all right,
  otherwize, he will perform an action to reinitialize itself.
    Notes:  
------------------------------------------------------------------------------*/

  RETURN YES.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getInstalledSkin Attribute-Dlg 
FUNCTION getInstalledSkin RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE cfileName                       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFullPath                       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAttr                           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cListSkins                      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSkinsDirectory                 AS CHARACTER  NO-UNDO.

FILE-INFO:FILE-NAME = "tvSkins/.".
ASSIGN
    cListSkins = "Automatic"
    cSkinsDirectory = FILE-INFO:FULL-PATHNAME.

INPUT FROM OS-DIR(cSkinsDirectory).
REPEAT:
    IMPORT cfileName cFullPath cAttr.
    
    IF cFileName = ".."  THEN NEXT.
    IF cFileName = "."   THEN NEXT.
    
    IF cAttr = "D" THEN
        ASSIGN
            cListSkins = cListSkins + (IF cListSkins = "" THEN "" ELSE ",") + cfileName.

END.
INPUT CLOSE.

RETURN cListSkins.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

