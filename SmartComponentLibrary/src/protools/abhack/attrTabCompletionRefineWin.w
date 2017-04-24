&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME hWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS hWin 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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

DEFINE TEMP-TABLE ttAttr NO-UNDO LABEL "ttAttr (for completion of attributes and methods on TAB Key)"
 FIELD cAttr AS CHARACTER
 FIELD cObjTypes AS CHARACTER  /* list of Object/widget types for a given attribute/method */
 INDEX cAttr cAttr
 INDEX cObjTypes IS WORD-INDEX cObjTypes.

DEFINE TEMP-TABLE ttAttr2nd NO-UNDO LIKE ttAttr LABEL "ttAttr2nd to be used in a dummy prodataset".

DEFINE TEMP-TABLE ttObjType NO-UNDO LABEL "ttObjType (contains object types defined in attrTabCompletion<X>.txt)"
 FIELD cObjType AS CHARACTER
 INDEX cObjType cObjType.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 IMAGE-1 btnLoad ed RADIO-SET-1 ~
FILL-IN-1 SELECT-1 SLIDER-1 cbDropDown TOGGLE-1 cbDropDownList TEXT-1 
&Scoped-Define DISPLAYED-OBJECTS ed RADIO-SET-1 FILL-IN-1 SELECT-1 SLIDER-1 ~
cbDropDown TOGGLE-1 cbDropDownList TEXT-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR hWin AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_sub-menu 
       MENU-ITEM m_menu-toggle  LABEL "menu-toggle"   
              TOGGLE-BOX.

DEFINE MENU MENU-BAR-hWin MENUBAR
       MENU-ITEM m_menu-item    LABEL "menu-item"     
       SUB-MENU  m_sub-menu     LABEL "sub-menu"      .


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnDumpObjTypes 
     LABEL "Dump all object type to clipboard" 
     SIZE 62 BY 1.14 TOOLTIP "Up to you to paste it in the protools\abhack\attrtabcompletion<n>.txt file".

DEFINE BUTTON btnLoad 
     LABEL "Load the protools~\abhack~\attrtabcompletion<n>.txt file" 
     SIZE 62 BY 1.14.

DEFINE BUTTON btnProcessAttrs 
     LABEL "Process & attributes and dump definitions to clipboard" 
     SIZE 62 BY 1.14 TOOLTIP "Up to you to paste it in the protools\abhack\attrtabcompletion<n>.txt file".

DEFINE VARIABLE cbDropDown AS CHARACTER 
     LABEL "cbDropDown" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cbDropDownList AS CHARACTER FORMAT "X(256)":U 
     LABEL "cbDropDownList" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE ed AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 62 BY 4.05 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 1" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE TEXT-1 AS CHARACTER FORMAT "X(256)":U INITIAL "TEXT-1" 
      VIEW-AS TEXT 
     SIZE 10 BY .62 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "protools/abhack/tux.bmp":U
     SIZE 5 BY 1.67.

DEFINE VARIABLE RADIO-SET-1 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Item 1", 1,
"Item 2", 2,
"Item 3", 3
     SIZE 12 BY 3 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY 1.19.

DEFINE VARIABLE SELECT-1 AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 10 BY 2 NO-UNDO.

DEFINE VARIABLE SLIDER-1 AS INTEGER INITIAL 0 
     VIEW-AS SLIDER MIN-VALUE 0 MAX-VALUE 100 VERTICAL 
     TIC-MARKS NONE 
     SIZE 9 BY 3 NO-UNDO.

DEFINE VARIABLE TOGGLE-1 AS LOGICAL INITIAL no 
     LABEL "Toggle 1" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     btnLoad AT ROW 1.24 COL 2
     btnProcessAttrs AT ROW 2.43 COL 2
     btnDumpObjTypes AT ROW 3.62 COL 2
     ed AT ROW 5.05 COL 2 NO-LABEL
     RADIO-SET-1 AT ROW 9.1 COL 47 NO-LABEL
     FILL-IN-1 AT ROW 9.33 COL 6 COLON-ALIGNED
     SELECT-1 AT ROW 10.52 COL 14 NO-LABEL
     SLIDER-1 AT ROW 10.76 COL 4 NO-LABEL
     cbDropDown AT ROW 13.14 COL 44 COLON-ALIGNED
     TOGGLE-1 AT ROW 14.33 COL 3
     cbDropDownList AT ROW 14.33 COL 44 COLON-ALIGNED
     TEXT-1 AT ROW 10.76 COL 29 COLON-ALIGNED NO-LABEL
     "Literal, handle retrieved by program" VIEW-AS TEXT
          SIZE 35 BY .62 AT ROW 12.19 COL 28
     RECT-1 AT ROW 9.1 COL 27
     IMAGE-1 AT ROW 9.1 COL 40
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 65.6 BY 19.24.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW hWin ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 19.24
         WIDTH              = 65.6
         MAX-HEIGHT         = 19.24
         MAX-WIDTH          = 97.4
         VIRTUAL-HEIGHT     = 19.24
         VIRTUAL-WIDTH      = 97.4
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-hWin:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW hWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON btnDumpObjTypes IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnProcessAttrs IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(hWin)
THEN hWin:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 12.91
       COLUMN          = 20
       HEIGHT          = 1.43
       WIDTH           = 6
       HELP            = "We care only about the ctrl-frame progress widget"
       HIDDEN          = yes
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame:MOVE-AFTER(SLIDER-1:HANDLE IN FRAME fMain).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME hWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL hWin hWin
ON END-ERROR OF hWin /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL hWin hWin
ON WINDOW-CLOSE OF hWin /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDumpObjTypes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDumpObjTypes hWin
ON CHOOSE OF btnDumpObjTypes IN FRAME fMain /* Dump all object type to clipboard */
DO:

OUTPUT TO "CLIPBOARD".
FOR EACH ttObjType:
    PUT UNFORMATTED ttObjType.cObjType SKIP.
END.

OUTPUT CLOSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLoad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLoad hWin
ON CHOOSE OF btnLoad IN FRAME fMain /* Load the protools\abhack\attrtabcompletion<n>.txt file */
DO:
  RUN loadttAttr.
  btnProcessAttrs:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnProcessAttrs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnProcessAttrs hWin
ON CHOOSE OF btnProcessAttrs IN FRAME fMain /* Process  attributes and dump definitions to clipboard */
DO:
RUN processttAttr.

OUTPUT TO "CLIPBOARD".
FOR EACH ttAttr:
    PUT UNFORMATTED ttAttr.cAttr " "
     FILL(" ", 27 - LENGTH(ttAttr.cAttr))
     ttAttr.cObjTypes SKIP.
END.

OUTPUT CLOSE.

btnDumpObjTypes:SENSITIVE = YES.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK hWin 


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
  ed:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "The point of this window is to refine the protools\abhack\attrtabcompletion<n>.txt file"
   + " with the object/widget types for each attribute/method.  Use the load and process/save buttons to do the job~n"
   + "Note the window contains all the possible widget/object types in order to test the validity of each attribute/method for each object/widget".
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load hWin  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "attrTabCompletionRefineWin.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "attrTabCompletionRefineWin.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI hWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(hWin)
  THEN DELETE WIDGET hWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI hWin  _DEFAULT-ENABLE
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
  RUN control_load.
  DISPLAY ed RADIO-SET-1 FILL-IN-1 SELECT-1 SLIDER-1 cbDropDown TOGGLE-1 
          cbDropDownList TEXT-1 
      WITH FRAME fMain IN WINDOW hWin.
  ENABLE RECT-1 IMAGE-1 btnLoad ed RADIO-SET-1 FILL-IN-1 SELECT-1 SLIDER-1 
         cbDropDown TOGGLE-1 cbDropDownList TEXT-1 
      WITH FRAME fMain IN WINDOW hWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW hWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadttAttr hWin 
PROCEDURE loadttAttr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cLine      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFileName  AS CHARACTER  NO-UNDO.

EMPTY TEMP-TABLE ttAttr.

cFileName = SEARCH("protools/abhack/attrTabCompletion" + ENTRY(1, PROVERSION, ".") + ".txt").
INPUT FROM VALUE(cFileName).
REPEAT:
    IMPORT UNFORMATTED cLine.
    IF cLine BEGINS "# " THEN NEXT.
    IF cLine = "" THEN NEXT.
    CREATE ttAttr.
    ASSIGN
     ttAttr.cAttr     = ENTRY(1, cLine, " ")
     ttAttr.cObjTypes = TRIM(SUBSTRING(cLine, INDEX(cLine, " ") + 1)).
END.
INPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcessThisAttr hWin 
PROCEDURE ProcessThisAttr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT         PARAMETER phObject      AS HANDLE      NO-UNDO.
DEFINE INPUT         PARAMETER pcAttr        AS CHARACTER   NO-UNDO.
DEFINE INPUT-OUTPUT  PARAMETER pcObjectTypes AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cType AS CHARACTER   NO-UNDO.

DEFINE BUFFER ttObjType FOR ttObjType.


IF NOT VALID-HANDLE(phObject) THEN RETURN.

pcAttr = ENTRY(1, pcAttr, "("). /* for methods,remove the '()' */

IF phObject:TYPE <> "pseudo-widget"    THEN cType = phObject:TYPE.
ELSE IF phObject = FILE-INFO:HANDLE    THEN cType = "FILE-INFO".
ELSE IF phObject = SESSION:HANDLE      THEN cType = "SESSION".
ELSE IF phObject = ERROR-STATUS:HANDLE THEN cType = "ERROR-STATUS".
ELSE IF phObject = COMPILER:HANDLE     THEN cType = "COMPILER".
ELSE IF phObject = LAST-EVENT:HANDLE   THEN cType = "LAST-EVENT".
ELSE IF phObject = COLOR-TABLE:HANDLE  THEN cType = "COLOR-TABLE".
ELSE IF phObject = FONT-TABLE:HANDLE   THEN cType = "FONT-TABLE".
ELSE IF phObject = DEBUGGER:HANDLE     THEN cType = "DEBUGGER".
ELSE IF phObject = CLIPBOARD:HANDLE    THEN cType = "CLIPBOARD".
ELSE IF phObject = RCODE-INFO:HANDLE   THEN cType = "RCODE-INFO".

/* special case for this guy that claim to a fill-in, actually 'fill-in' will be fine for ABHack, but we might want to use the .txt file for other needs...  */
IF cType = "FILL-IN" AND phObject:NAME = "BrowseCol" THEN cType = "BROWSE-COLUMN".

FIND FIRST ttObjType WHERE ttObjType.cObjType = cType NO-ERROR.
IF NOT AVAILABLE ttObjType THEN DO:
    CREATE ttObjType.
    ttObjType.cObjType = cType.
END.

IF CAN-DO(pcObjectTypes, cType) THEN RETURN.

IF NOT CAN-QUERY(phObject, pcAttr) THEN RETURN.
IF pcObjectTypes > "" THEN pcObjectTypes = pcObjectTypes + ",".
pcObjectTypes = pcObjectTypes + cType.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE processttAttr hWin 
PROCEDURE processttAttr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE hBrowse        AS HANDLE   NO-UNDO.
DEFINE VARIABLE hBrowseCol     AS HANDLE   NO-UNDO.
DEFINE VARIABLE hCall          AS HANDLE   NO-UNDO.
DEFINE VARIABLE hDset          AS HANDLE   NO-UNDO.
DEFINE VARIABLE hDsrc          AS HANDLE   NO-UNDO.
DEFINE VARIABLE hLitteral      AS HANDLE   NO-UNDO.
DEFINE VARIABLE hRel           AS HANDLE   NO-UNDO.
DEFINE VARIABLE hSaxAttributes AS HANDLE   NO-UNDO.
DEFINE VARIABLE hSaxReader     AS HANDLE   NO-UNDO.
DEFINE VARIABLE hSaxWriter     AS HANDLE   NO-UNDO.
DEFINE VARIABLE hServer        AS HANDLE   NO-UNDO.
DEFINE VARIABLE hSocket        AS HANDLE   NO-UNDO.
DEFINE VARIABLE hWidget        AS HANDLE   NO-UNDO. /* dyn widget for some widget that are hard to refer statically */.
DEFINE VARIABLE hxdoc          AS HANDLE   NO-UNDO.
DEFINE VARIABLE hxNode         AS HANDLE   NO-UNDO.


DEFINE BUFFER ttAttrYo   FOR ttAttr.
DEFINE QUERY q FOR ttAttrYo.
DEFINE FRAME Dialog-Frame WITH VIEW-AS DIALOG-BOX.

/* reset the ttAttr.cObjTypes field */
FOR EACH ttAttr:
    ttAttr.cObjTypes = "".
END.


CREATE WIDGET-POOL.

OPEN QUERY q FOR EACH ttAttrYo.


CREATE VALUE('DATASET') hDset NO-ERROR. /* using VALUE to avoid error in earlier version than OE10 */
CREATE VALUE("DATA-SOURCE") hDsrc NO-ERROR.
IF VALID-HANDLE(hDset) THEN DO:
    hDset:SET-BUFFERS(BUFFER ttAttrYo:HANDLE, BUFFER ttAttr2nd:HANDLE).
    /* this trick is required to avoid errors in V9  */
    DEFINE VARIABLE hBuff1 AS HANDLE     NO-UNDO.
    DEFINE VARIABLE hBuff2 AS HANDLE     NO-UNDO.
    hBuff1 = BUFFER ttAttrYo:HANDLE.
    hBuff2 = BUFFER ttAttr2nd:HANDLE.
    RUN protools/abhack/attrTabCompletionSetRelation.p
        (hDset
        ,hBuff1
        ,hBuff2
        ,"cAttr,cAttr"
        ,OUTPUT hRel). 
END.


CREATE BROWSE hBrowse ASSIGN
 FRAME     = FRAME fMain:HANDLE
 Y         = TOGGLE-1:Y + TOGGLE-1:HEIGHT-PIXELS + 3
 X         = 1
 VISIBLE   = YES
 SENSITIVE = YES.
hBrowseCol = hBrowse:ADD-CALC-COLUMN('CHARACTER', 'X', 'yo', 'yo').
hBrowseCol:NAME = "BrowseCol".

CREATE X-DOCUMENT hxdoc.
CREATE X-NODEREF  hxnode.
CREATE CALL hCall.
CREATE SERVER hServer.
CREATE SAX-READER hSaxReader.
CREATE SAX-WRITER hSaxWriter.
CREATE SAX-ATTRIBUTES hSaxAttributes.

hLitteral = FRAME {&FRAME-NAME}:FIRST-CHILD. /* field group */
hLitteral = hLitteral:FIRST-CHILD. /* first widget */
DO WHILE hLitteral <> ?:
    IF hLitteral:TYPE = "literal" THEN LEAVE.
    hLitteral = hLitteral:NEXT-SIBLING.
END.

FOR EACH  ttAttr WITH FRAME fMain:
    RUN ProcessThisAttr (FILL-IN-1:HANDLE      , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    
    hWidget = BUFFER ttAttr:HANDLE.
    RUN ProcessThisAttr (hWidget  , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    hWidget = BUFFER ttAttr:BUFFER-FIELD("cAttr").
    RUN ProcessThisAttr (hWidget               , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    hWidget = TEMP-TABLE ttAttr:HANDLE.
    RUN ProcessThisAttr (hWidget               , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    
    RUN ProcessThisAttr (THIS-PROCEDURE        , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (FRAME fMain:HANDLE    , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (hBrowse               , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (hBrowseCol            , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (ed:HANDLE             , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (hwin:HANDLE           , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (btnLoad:HANDLE        , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (RECT-1:HANDLE         , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (IMAGE-1:HANDLE        , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (RADIO-SET-1:HANDLE    , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (SELECT-1:HANDLE       , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (SLIDER-1:HANDLE       , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (cbDropDown:HANDLE     , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (cbDropDownList:HANDLE , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (CtrlFrame:HANDLE      , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (hLitteral             , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (TEXT-1:HANDLE         , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (TOGGLE-1:HANDLE       , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (FRAME Dialog-Frame:HANDLE  , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).

   
    RUN ProcessThisAttr (MENU MENU-BAR-hWin:HANDLE , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    hWidget = MENU-ITEM m_menu-item:HANDLE IN MENU MENU-BAR-hWin.
    RUN ProcessThisAttr (hWidget               , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    hWidget = SUB-MENU m_sub-menu:HANDLE IN MENU MENU-BAR-hWin.
    RUN ProcessThisAttr (hWidget               , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    hWidget = MENU-ITEM m_menu-toggle:HANDLE IN SUB-MENU m_sub-menu.
    RUN ProcessThisAttr (hWidget               , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    
    RUN ProcessThisAttr (QUERY q:HANDLE        , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    
    RUN ProcessThisAttr (hCall                 , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (hSocket               , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (hxdoc                 , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (hxNode                , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (hSaxReader            , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (hSaxWriter            , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (hSaxAttributes        , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (hDsrc                 , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (hDset                 , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (hRel                  , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    
    RUN ProcessThisAttr (FILE-INFO:HANDLE      , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (SESSION:HANDLE        , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (ERROR-STATUS:HANDLE   , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (COMPILER:HANDLE       , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (LAST-EVENT:HANDLE     , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (COLOR-TABLE:HANDLE    , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (FONT-TABLE:HANDLE     , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (DEBUGGER:HANDLE       , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (RCODE-INFO:HANDLE     , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
    RUN ProcessThisAttr (CLIPBOARD:HANDLE      , ttAttr.cAttr, INPUT-OUTPUT ttAttr.cObjTypes).
END.

DELETE WIDGET-POOL.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

