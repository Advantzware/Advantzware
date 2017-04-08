&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: brwsdefs.w

  Description: Browser Global Defines Create/Update

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 02/25/98

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

DEFINE VARIABLE brwsdefs AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE item_list AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS db-name tablename fieldnames selected-fields ~
sort-order Btn_Add Btn_Remove Btn_Up Btn_Down sort-label enhance use-sortby ~
cascade-sort useindex Btn_Open Btn_Delete Btn_Save Btn_Cancel Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS db-name tablename fieldnames ~
selected-fields sort-order index-names sort-label index-label enhance ~
use-sortby cascade-sort useindex 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 index-names index-label 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Add 
     LABEL "-->> &Add -->>" 
     SIZE 14 BY 1.14.

DEFINE BUTTON Btn_Cancel 
     LABEL "&Cancel" 
     SIZE 14 BY 1.14.

DEFINE BUTTON Btn_Delete 
     LABEL "&Delete" 
     SIZE 14 BY 1.14.

DEFINE BUTTON Btn_Down 
     LABEL "Move Do&wn" 
     SIZE 14 BY 1.14.

DEFINE BUTTON Btn_OK 
     LABEL "&OK" 
     SIZE 14 BY 1.14.

DEFINE BUTTON Btn_Open 
     LABEL "&Open" 
     SIZE 14 BY 1.14.

DEFINE BUTTON Btn_Remove 
     LABEL "&Remove <<--" 
     SIZE 14 BY 1.14.

DEFINE BUTTON Btn_Save 
     LABEL "&Save" 
     SIZE 14 BY 1.14.

DEFINE BUTTON Btn_Up 
     LABEL "Move &Up" 
     SIZE 14 BY 1.14.

DEFINE VARIABLE db-name AS CHARACTER FORMAT "X(256)":U 
     LABEL "Database" 
     VIEW-AS COMBO-BOX INNER-LINES 4
     LIST-ITEMS "None" 
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE index-label AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 13
     LIST-ITEMS "?" 
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE tablename AS CHARACTER FORMAT "X(256)":U 
     LABEL "Table" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS "None" 
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE sort-label AS CHARACTER FORMAT "X(256)":U 
     LABEL "Override Sort Field Label" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE fieldnames AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 23 BY 10.95 NO-UNDO.

DEFINE VARIABLE index-names AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 23 BY 8.57 NO-UNDO.

DEFINE VARIABLE selected-fields AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 23 BY 8.57 NO-UNDO.

DEFINE VARIABLE sort-order AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 23 BY 8.57 NO-UNDO.

DEFINE VARIABLE cascade-sort AS LOGICAL INITIAL no 
     LABEL "Cascading Sort Order" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE enhance AS LOGICAL INITIAL yes 
     LABEL "Enhance" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE use-sortby AS LOGICAL INITIAL no 
     LABEL "Use Sort-By Phrase" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE useindex AS LOGICAL INITIAL no 
     LABEL "Use Index" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     db-name AT ROW 1.24 COL 10 COLON-ALIGNED HELP
          "Select Database"
     tablename AT ROW 1.24 COL 49 COLON-ALIGNED HELP
          "Select Table"
     fieldnames AT ROW 2.43 COL 12 HELP
          "Select Field Name" NO-LABEL
     selected-fields AT ROW 2.43 COL 51 HELP
          "Selected Fields" NO-LABEL
     sort-order AT ROW 2.43 COL 75 HELP
          "Sort Field Labels" NO-LABEL
     index-names AT ROW 2.43 COL 99 HELP
          "Use Index Names" NO-LABEL
     Btn_Add AT ROW 4.57 COL 36 HELP
          "Add Field to Selected Fields"
     Btn_Remove AT ROW 6 COL 36 HELP
          "Remove Field from Selected Fields"
     Btn_Up AT ROW 8.38 COL 36 HELP
          "Move Selected Field Up"
     Btn_Down AT ROW 9.81 COL 36 HELP
          "Move Selected Field Down"
     sort-label AT ROW 11.24 COL 73 COLON-ALIGNED HELP
          "Enter Sort Label"
     index-label AT ROW 11.24 COL 97 COLON-ALIGNED NO-LABEL
     enhance AT ROW 12.43 COL 36 HELP
          "Enhance Indicator"
     use-sortby AT ROW 12.43 COL 51 HELP
          "Use Sort-By Phrase Option"
     cascade-sort AT ROW 12.43 COL 75 HELP
          "Sort Order Phrase includes all Sort Order selections"
     useindex AT ROW 12.43 COL 108 HELP
          "Use Index Indicator"
     Btn_Open AT ROW 13.62 COL 16 HELP
          "OPEN List Parameter File"
     Btn_Delete AT ROW 13.62 COL 51 HELP
          "DELETE Current List Parameter File"
     Btn_Save AT ROW 13.62 COL 67 HELP
          "SAVE Current List Parameters"
     Btn_Cancel AT ROW 13.62 COL 92 HELP
          "CANCEL and EXIT List Parameters Create/Update"
     Btn_OK AT ROW 13.62 COL 108 HELP
          "OK to SAVE and EXIT List Parameter Create/Update"
     "Selected Fields:" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 2.43 COL 36
     "Fields:" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 2.43 COL 5
     "Use Index Names" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 1.71 COL 101
     "Sort Field Labels" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 1.71 COL 77
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 121.5 BY 14.


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
         TITLE              = "Browser Global Defines Create/Update"
         HEIGHT             = 14
         WIDTH              = 121.6
         MAX-HEIGHT         = 14
         MAX-WIDTH          = 121.6
         VIRTUAL-HEIGHT     = 14
         VIRTUAL-WIDTH      = 121.6
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

/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR COMBO-BOX index-label IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR SELECTION-LIST index-names IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Browser Global Defines Create/Update */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Browser Global Defines Create/Update */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add C-Win
ON CHOOSE OF Btn_Add IN FRAME DEFAULT-FRAME /* -->> Add -->> */
DO:
  APPLY "DEFAULT-ACTION" TO fieldnames.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:06:43 pm */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY "CLOSE" TO THIS-PROCEDURE.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:06:43 pm */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Delete C-Win
ON CHOOSE OF Btn_Delete IN FRAME DEFAULT-FRAME /* Delete */
DO:
  IF SEARCH(brwsdefs) = ? THEN
  RETURN NO-APPLY.
  MESSAGE "Delete this Browser Global Defines File?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
          UPDATE deleteok AS LOGICAL.
  IF NOT deleteok THEN
  RETURN NO-APPLY.
  OS-DELETE VALUE(brwsdefs).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:06:43 pm */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Down C-Win
ON CHOOSE OF Btn_Down IN FRAME DEFAULT-FRAME /* Move Down */
DO:
  RUN Move-Field ("Down").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:06:43 pm */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  APPLY "CHOOSE"TO Btn_Save.
  APPLY "CLOSE" TO THIS-PROCEDURE.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:06:43 pm */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Open
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Open C-Win
ON CHOOSE OF Btn_Open IN FRAME DEFAULT-FRAME /* Open */
DO:
  RUN Open-brwsdefs.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:06:43 pm */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove C-Win
ON CHOOSE OF Btn_Remove IN FRAME DEFAULT-FRAME /* Remove <<-- */
DO:
  APPLY "DEFAULT-ACTION" TO selected-fields.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:06:43 pm */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Save C-Win
ON CHOOSE OF Btn_Save IN FRAME DEFAULT-FRAME /* Save */
DO:
  RUN Save-brwsdefs.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:06:43 pm */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Up C-Win
ON CHOOSE OF Btn_Up IN FRAME DEFAULT-FRAME /* Move Up */
DO:
  RUN Move-Field ("Up").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:06:43 pm */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME db-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL db-name C-Win
ON VALUE-CHANGED OF db-name IN FRAME DEFAULT-FRAME /* Database */
DO:
  tablename:LIST-ITEMS = "None".
  IF {&SELF-NAME}:SCREEN-VALUE NE "None" THEN
  RUN Get-Tables.
  tablename:SCREEN-VALUE = tablename:ENTRY(1).
  APPLY "VALUE-CHANGED" TO tablename.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fieldnames
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fieldnames C-Win
ON DEFAULT-ACTION OF fieldnames IN FRAME DEFAULT-FRAME
DO:
  IF selected-fields:NUM-ITEMS = 13 THEN
  DO:
    MESSAGE "Maximum Fields allowed already selected!" VIEW-AS ALERT-BOX INFORMATION.
    RETURN NO-APPLY.
  END.
  RUN Get_Procedure IN Persistent-Handle (INPUT "fld_lbls.",OUTPUT run-proc,no).
  IF run-proc NE "" THEN
  RUN VALUE(run-proc) (tablename:SCREEN-VALUE,{&SELF-NAME}:SCREEN-VALUE,OUTPUT item_list).
  ASSIGN
    ldummy = selected-fields:ADD-LAST({&SELF-NAME}:SCREEN-VALUE)
    ldummy = sort-order:ADD-LAST(item_list)
    ldummy = index-names:ADD-LAST("")
    selected-fields:SCREEN-VALUE = selected-fields:ENTRY(selected-fields:NUM-ITEMS)
    sort-order:SCREEN-VALUE = sort-order:ENTRY(sort-order:NUM-ITEMS)
    sort-label:SCREEN-VALUE = sort-order:SCREEN-VALUE
    index-names:SCREEN-VALUE = index-names:ENTRY(index-names:NUM-ITEMS).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME index-label
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL index-label C-Win
ON VALUE-CHANGED OF index-label IN FRAME DEFAULT-FRAME
DO:
  DO i = 1 TO selected-fields:NUM-ITEMS:
    IF selected-fields:IS-SELECTED(i) THEN
    ASSIGN
      ldummy = index-names:REPLACE({&SELF-NAME}:SCREEN-VALUE,i)
      index-names:SCREEN-VALUE = index-names:ENTRY(i).
  END.
  {&SELF-NAME}:SCREEN-VALUE = {&SELF-NAME}:ENTRY(1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME index-names
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL index-names C-Win
ON DEFAULT-ACTION OF index-names IN FRAME DEFAULT-FRAME
DO:
  DO i = 1 TO {&SELF-NAME}:NUM-ITEMS:
    IF {&SELF-NAME}:IS-SELECTED(i) THEN
    ASSIGN
      ldummy = selected-fields:DELETE(i)
      ldummy = sort-order:DELETE(i)
      ldummy = {&SELF-NAME}:DELETE(i).
  END.
  IF {&SELF-NAME}:NUM-ITEMS NE 0 THEN
  ASSIGN
    selected-fields:SCREEN-VALUE = selected-fields:ENTRY(1)
    sort-order:SCREEN-VALUE = sort-order:ENTRY(1)
    sort-label:SCREEN-VALUE = sort-order:SCREEN-VALUE
    {&SELF-NAME}:SCREEN-VALUE = {&SELF-NAME}:ENTRY(1).
  ELSE
  sort-label:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL index-names C-Win
ON VALUE-CHANGED OF index-names IN FRAME DEFAULT-FRAME
DO:
  DO i = 1 TO {&SELF-NAME}:NUM-ITEMS:
    IF {&SELF-NAME}:IS-SELECTED(i) THEN
    ASSIGN
      selected-fields:SCREEN-VALUE = selected-fields:ENTRY(i)
      sort-order:SCREEN-VALUE = sort-order:ENTRY(i)
      sort-label:SCREEN-VALUE = sort-order:SCREEN-VALUE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME selected-fields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL selected-fields C-Win
ON DEFAULT-ACTION OF selected-fields IN FRAME DEFAULT-FRAME
DO:
  DO i = 1 TO {&SELF-NAME}:NUM-ITEMS:
    IF {&SELF-NAME}:IS-SELECTED(i) THEN
    ASSIGN
      ldummy = sort-order:DELETE(i)
      ldummy = index-names:DELETE(i)
      ldummy = {&SELF-NAME}:DELETE(i).
  END.
  IF {&SELF-NAME}:NUM-ITEMS NE 0 THEN
  ASSIGN
    {&SELF-NAME}:SCREEN-VALUE = {&SELF-NAME}:ENTRY(1)
    sort-order:SCREEN-VALUE = sort-order:ENTRY(1)
    sort-label:SCREEN-VALUE = sort-order:SCREEN-VALUE
    index-names:SCREEN-VALUE = index-names:ENTRY(1).
  ELSE
  sort-label:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL selected-fields C-Win
ON VALUE-CHANGED OF selected-fields IN FRAME DEFAULT-FRAME
DO:
  DO i = 1 TO {&SELF-NAME}:NUM-ITEMS:
    IF {&SELF-NAME}:IS-SELECTED(i) THEN
    ASSIGN
      sort-order:SCREEN-VALUE = sort-order:ENTRY(i)
      sort-label:SCREEN-VALUE = sort-order:SCREEN-VALUE
      index-names:SCREEN-VALUE = index-names:ENTRY(i).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sort-label
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sort-label C-Win
ON LEAVE OF sort-label IN FRAME DEFAULT-FRAME /* Override Sort Field Label */
DO:
  IF {&SELF-NAME}:SCREEN-VALUE = "" THEN
  DO:
    RUN Get_Procedure IN Persistent-Handle (INPUT "fld_lbls.",OUTPUT run-proc,no).
    IF run-proc NE "" THEN
    RUN VALUE(run-proc)
        (tablename:SCREEN-VALUE,selected-fields:SCREEN-VALUE,OUTPUT item_list).
    {&SELF-NAME}:SCREEN-VALUE = item_list.
  END.
  DO i = 1 TO sort-order:NUM-ITEMS:
    IF sort-order:IS-SELECTED(i) THEN
    ASSIGN
      ldummy = sort-order:REPLACE({&SELF-NAME}:SCREEN-VALUE,i)
      sort-order:SCREEN-VALUE = sort-order:ENTRY(i).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sort-label C-Win
ON RETURN OF sort-label IN FRAME DEFAULT-FRAME /* Override Sort Field Label */
DO:
  APPLY "LEAVE" TO {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sort-order
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sort-order C-Win
ON DEFAULT-ACTION OF sort-order IN FRAME DEFAULT-FRAME
DO:
  DO i = 1 TO {&SELF-NAME}:NUM-ITEMS:
    IF {&SELF-NAME}:IS-SELECTED(i) THEN
    ASSIGN
      ldummy = selected-fields:DELETE(i)
      ldummy = {&SELF-NAME}:DELETE(i)
      ldummy = index-names:DELETE(i).
  END.
  IF {&SELF-NAME}:NUM-ITEMS NE 0 THEN
  ASSIGN
    selected-fields:SCREEN-VALUE = selected-fields:ENTRY(1)
    {&SELF-NAME}:SCREEN-VALUE = {&SELF-NAME}:ENTRY(1)
    sort-label:SCREEN-VALUE = {&SELF-NAME}:SCREEN-VALUE
    index-names:SCREEN-VALUE = index-names:ENTRY(1).
  ELSE
  sort-label:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sort-order C-Win
ON VALUE-CHANGED OF sort-order IN FRAME DEFAULT-FRAME
DO:
  DO i = 1 TO {&SELF-NAME}:NUM-ITEMS:
    IF {&SELF-NAME}:IS-SELECTED(i) THEN
    ASSIGN
      selected-fields:SCREEN-VALUE = selected-fields:ENTRY(i)
      sort-label:SCREEN-VALUE = {&SELF-NAME}:SCREEN-VALUE
      index-names:SCREEN-VALUE = index-names:ENTRY(i).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tablename
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tablename C-Win
ON VALUE-CHANGED OF tablename IN FRAME DEFAULT-FRAME /* Table */
DO:
  ASSIGN
    fieldnames:LIST-ITEMS = ""
    selected-fields:LIST-ITEMS = ""
    sort-order:LIST-ITEMS = ""
    sort-label:SCREEN-VALUE = ""
    index-names:LIST-ITEMS = "".
    index-label:LIST-ITEMS = "".
  IF {&SELF-NAME}:SCREEN-VALUE NE "None" THEN
  RUN Get-Fields.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME use-sortby
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL use-sortby C-Win
ON VALUE-CHANGED OF use-sortby IN FRAME DEFAULT-FRAME /* Use Sort-By Phrase */
DO:
  IF {&SELF-NAME}:SCREEN-VALUE = "yes" THEN
  ENABLE cascade-sort WITH FRAME {&FRAME-NAME}.
  ELSE
  DO:
    cascade-sort:SCREEN-VALUE = "no".
    DISABLE cascade-sort WITH FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME useindex
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL useindex C-Win
ON VALUE-CHANGED OF useindex IN FRAME DEFAULT-FRAME /* Use Index */
DO:
  IF {&SELF-NAME}:SCREEN-VALUE = "yes" THEN
  ENABLE {&LIST-1} WITH FRAME {&FRAME-NAME}.
  ELSE
  DO:
    DO i = 1 TO selected-fields:NUM-ITEMS:
      ldummy = index-names:REPLACE("",i).
    END.
    DISABLE {&LIST-1} WITH FRAME {&FRAME-NAME}.
  END.
  index-label:SCREEN-VALUE = index-label:ENTRY(1).
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
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:06:43 pm */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  {methods/enhance.i}
  RUN Get-DBs.
  {methods/nowait.i}
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:06:43 pm */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY db-name tablename fieldnames selected-fields sort-order index-names 
          sort-label index-label enhance use-sortby cascade-sort useindex 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE db-name tablename fieldnames selected-fields sort-order Btn_Add 
         Btn_Remove Btn_Up Btn_Down sort-label enhance use-sortby cascade-sort 
         useindex Btn_Open Btn_Delete Btn_Save Btn_Cancel Btn_OK 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get-DBs C-Win 
PROCEDURE Get-DBs :
/*------------------------------------------------------------------------------
  Purpose:     Load Database Names
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    DO i = 1 TO NUM-DBS:
      ldummy = db-name:ADD-LAST(LDBNAME(i)).
    END.
    ASSIGN
      db-name:SCREEN-VALUE = db-name:ENTRY(1)
      tablename:SCREEN-VALUE = tablename:ENTRY(1).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get-Fields C-Win 
PROCEDURE Get-Fields :
/*------------------------------------------------------------------------------
  Purpose:     Get Database Fields
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    CREATE ALIAS dictdb FOR DATABASE VALUE(db-name:SCREEN-VALUE).
    RUN Get_Procedure IN Persistent-Handle (INPUT "fld_list.",OUTPUT run-proc,no).
    IF run-proc NE "" THEN
    RUN VALUE(run-proc) (tablename:SCREEN-VALUE,OUTPUT item_list).
    fieldnames:LIST-ITEMS = item_list.
    RUN Get_Procedure IN Persistent-Handle (INPUT "indxlist.",OUTPUT run-proc,no).
    IF run-proc NE "" THEN
    RUN VALUE(run-proc) (tablename:SCREEN-VALUE,OUTPUT item_list).
    index-label:LIST-ITEMS = "," + item_list.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get-Tables C-Win 
PROCEDURE Get-Tables :
/*------------------------------------------------------------------------------
  Purpose:     Get Database Tables
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    tablename:LIST-ITEMS = "None".
    CREATE ALIAS dictdb FOR DATABASE VALUE(db-name:SCREEN-VALUE).
    RUN Get_Procedure IN Persistent-Handle (INPUT "filelist.",OUTPUT run-proc,no).
    IF run-proc NE "" THEN
    RUN VALUE(run-proc) (OUTPUT item_list).
    tablename:LIST-ITEMS = tablename:LIST-ITEMS + "," + item_list.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field C-Win 
PROCEDURE Move-Field :
/* -----------------------------------------------------------
  Purpose: Move Field Up or Down
  Parameters: Up/Down
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER move AS CHARACTER NO-UNDO.

  DO i = 1 TO selected-fields:NUM-ITEMS IN FRAME {&FRAME-NAME}
      WITH FRAME {&FRAME-NAME}:
    IF selected-fields:IS-SELECTED(i) THEN
    DO:
      IF move = "Down" AND i NE selected-fields:NUM-ITEMS THEN
      ASSIGN
        ldummy = selected-fields:INSERT(selected-fields:SCREEN-VALUE,i + 2)
        ldummy = selected-fields:DELETE(i)
        selected-fields:SCREEN-VALUE = selected-fields:ENTRY(i + 1)
        ldummy = sort-order:INSERT(sort-order:SCREEN-VALUE,i + 2)
        ldummy = sort-order:DELETE(i)
        sort-order:SCREEN-VALUE = sort-order:ENTRY(i + 1)
        ldummy = index-names:INSERT(index-names:SCREEN-VALUE,i + 2)
        ldummy = index-names:DELETE(i)
        index-names:SCREEN-VALUE = index-names:ENTRY(i + 1).
      ELSE
      IF move = "Up" AND i NE 1 THEN
      ASSIGN
        ldummy = selected-fields:INSERT(selected-fields:SCREEN-VALUE,i - 1)
        ldummy = selected-fields:DELETE(i + 1)
        selected-fields:SCREEN-VALUE = selected-fields:ENTRY(i - 1)
        ldummy = sort-order:INSERT(sort-order:SCREEN-VALUE,i - 1)
        ldummy = sort-order:DELETE(i + 1)
        sort-order:SCREEN-VALUE = sort-order:ENTRY(i - 1)
        ldummy = index-names:INSERT(index-names:SCREEN-VALUE,i - 1)
        ldummy = index-names:DELETE(i + 1)
        index-names:SCREEN-VALUE = index-names:ENTRY(i - 1).
      LEAVE.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Open-brwsdefs C-Win 
PROCEDURE Open-brwsdefs :
/*------------------------------------------------------------------------------
  Purpose:     Open List Parameters File
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.
  DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
  DEFINE VARIABLE chrfld AS CHARACTER EXTENT 45 NO-UNDO.

  ASSIGN
    init-dir = "methods\browsers"
    brwsdefs = "".
  SYSTEM-DIALOG GET-FILE brwsdefs
      TITLE      "Choose List Parameter to OPEN..."
      FILTERS    "Browser Global Define Files (*.i)" "*.i"
      INITIAL-DIR init-dir
      MUST-EXIST
      UPDATE OKpressed.
  IF NOT OKpressed THEN
  RETURN NO-APPLY.
  INPUT FROM VALUE(brwsdefs) NO-ECHO.
  IMPORT ^.
  IMPORT chrfld[1] chrfld[2].
  DO i = 3 TO 44 BY 3:
    IMPORT chrfld[i] chrfld[i + 1] chrfld[i + 2].
  END.
  IMPORT chrfld[45].
  INPUT CLOSE.
  IF chrfld[1] NE "" THEN
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      fieldnames:LIST-ITEMS = ""
      selected-fields:LIST-ITEMS = ""
      sort-order:LIST-ITEMS = ""
      sort-label:SCREEN-VALUE = ""
      index-names:LIST-ITEMS = ""
      index-label:LIST-ITEMS = ""
      db-name:SCREEN-VALUE = chrfld[1].
    RUN Get-Tables.
    tablename:SCREEN-VALUE = chrfld[2].
    RUN Get-Fields.
    DO i = 3 TO 41 BY 3:
      IF chrfld[i] NE "" THEN
      ASSIGN
        ldummy = selected-fields:ADD-LAST(chrfld[i])
        ldummy = sort-order:ADD-LAST(chrfld[i + 1])
        ldummy = index-names:ADD-LAST(chrfld[i + 2]).
    END.
    ASSIGN
      fieldnames:SCREEN-VALUE = fieldnames:ENTRY(1)
      selected-fields:SCREEN-VALUE = selected-fields:ENTRY(1)
      sort-order:SCREEN-VALUE = sort-order:ENTRY(1)
      index-names:SCREEN-VALUE = index-names:ENTRY(1)
      sort-label:SCREEN-VALUE = sort-order:SCREEN-VALUE
      enhance:SCREEN-VALUE = chrfld[42]
      cascade-sort:SCREEN-VALUE = chrfld[43]
      useindex:SCREEN-VALUE = chrfld[44]
      use-sortby:SCREEN-VALUE = chrfld[45].
    APPLY "VALUE-CHANGED" TO useindex.
    APPLY "VALUE-CHANGED" TO use-sortby.
  END.
  ELSE
  MESSAGE "This is a custom file, use Editor for any Modifications!"
      VIEW-AS ALERT-BOX WARNING.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Output-Defines C-Win 
PROCEDURE Output-Defines :
/*------------------------------------------------------------------------------
  Purpose:     Output selected values
  Parameters:  field#, field name, and description
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER field# AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER fldname AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER descrip AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER idxname AS CHARACTER NO-UNDO.

  DEFINE VARIABLE data-type AS CHARACTER NO-UNDO.
  DEFINE VARIABLE data-format AS CHARACTER NO-UNDO.
  DEFINE VARIABLE fld-cnt AS INTEGER NO-UNDO.

  PUT UNFORMATTED "~&Global-define FLDNAME" field# " ".
  IF fldname NE ? THEN
  DO WITH FRAME {&FRAME-NAME}:
    PUT UNFORMATTED tablename:SCREEN-VALUE "." fldname.
    data-type = "STRING".
    RUN Get_Procedure IN Persistent-Handle ("get_type.",OUTPUT run-proc,no).
    IF run-proc NE "" THEN
    RUN VALUE(run-proc) (tablename:SCREEN-VALUE,fldname,OUTPUT data-type).
    IF data-type NE "STRING" THEN
    PUT UNFORMATTED SKIP "~&Global-define DATATYP" field# " " CAPS(data-type).
    RUN Get_Procedure IN Persistent-Handle ("get_frmt.",OUTPUT run-proc,no).
    IF run-proc NE "" THEN
    RUN VALUE(run-proc) (tablename:SCREEN-VALUE,fldname,OUTPUT data-format).
    PUT UNFORMATTED SKIP "~&Global-define FORMAT-" field# " " CAPS(data-format).
  END.
  PUT UNFORMATTED SKIP "~&Global-define SORTBY-" field# " ".
  IF use-sortby:SCREEN-VALUE = "yes" AND fldname NE ? THEN
  DO:
    PUT UNFORMATTED "BY ~{&FLDNAME" field# "}".
    IF cascade-sort:SCREEN-VALUE = "yes" AND INTEGER(field#) GT 1 THEN
    PUT UNFORMATTED " ~{&SORTBY-" INTEGER(field#) - 1 "}".
  END.
  PUT UNFORMATTED SKIP "~&Global-define IDXNAME" field# " ".
  IF useindex:SCREEN-VALUE = "yes" AND idxname NE ? AND idxname NE "" THEN
  PUT UNFORMATTED "USE-INDEX " idxname.
  PUT UNFORMATTED SKIP "~&Global-define DESCRIP" field# " " descrip SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Save-brwsdefs C-Win 
PROCEDURE Save-brwsdefs :
/*------------------------------------------------------------------------------
  Purpose:     Save List Parameter File
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    IF tablename:SCREEN-VALUE = "None" THEN
    RETURN.
    OUTPUT TO VALUE("methods/browsers/" + tablename:SCREEN-VALUE + ".i").
    PUT UNFORMATTED "/* " tablename:SCREEN-VALUE ".i" SKIP.
    EXPORT db-name:SCREEN-VALUE tablename:SCREEN-VALUE.
    DO i = 1 TO selected-fields:NUM-ITEMS:
      EXPORT selected-fields:ENTRY(i) sort-order:ENTRY(i) index-names:ENTRY(i).
    END.
    DO i = selected-fields:NUM-ITEMS + 1 TO 13:
      EXPORT "" "" "".
    END.
    EXPORT enhance:SCREEN-VALUE cascade-sort:SCREEN-VALUE useindex:SCREEN-VALUE.
    EXPORT use-sortby:SCREEN-VALUE.
    PUT UNFORMATTED "*/" SKIP(1).
    DO i = 1 TO selected-fields:NUM-ITEMS:
      RUN Output-Defines
          (STRING(i),selected-fields:ENTRY(i),sort-order:ENTRY(i),index-names:ENTRY(i)).
    END.
    DO i = selected-fields:NUM-ITEMS + 1 TO 13:
      RUN Output-Defines (STRING(i),?,"","").
    END.
    PUT UNFORMATTED "~&Global-define ENHANCE " enhance:SCREEN-VALUE SKIP.
    OUTPUT CLOSE.
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
  {methods/setfocus.i Btn_Open}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

