&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: abhackSection.w by slacroix JUN-2007 to display in a treeview the
   section layout of the current source code managed by abhack

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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

&SCOPED-DEFINE tvpicspath protools/abhack/tvpics/

/* this one is not available in Version 9
{src/adm2/widgetprto.i} */

DEFINE VARIABLE ghAttachedEditor AS HANDLE      NO-UNDO.
DEFINE VARIABLE gshABHackWin     AS HANDLE      NO-UNDO.

{protools/abhack/ABHackResourcesTT.i &SHARED="SHARED"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fContainer

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE hpure4gltv AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fContainer
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 46.4 BY 15.57.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Section layout"
         HEIGHT             = 15.57
         WIDTH              = 46.4
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.2
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.2
         SMALL-TITLE        = yes
         SHOW-IN-TASKBAR    = no
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = yes
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fContainer
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME fContainer:HANDLE
       ROW             = 15.29
       COLUMN          = 1
       HEIGHT          = 1.29
       WIDTH           = 6
       HIDDEN          = yes
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimerStickToWindow */
      RUN adjustTabOrder ( hpure4gltv , CtrlFrame , 'BEFORE':U ).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Section layout */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Section layout */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  
  /* 14-NOV-2007 sla: more consistent way of closing this guy */
  MESSAGE 'Please, close this window by choosing "No Section Outline" in the radiot-set at the top of the Main ABHack Window'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  
  /* APPLY "CLOSE":U TO THIS-PROCEDURE. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-RESIZED OF wWin /* Section layout */
DO:
  DEFINE VARIABLE iHorizontalGap    AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE iTvWidth          AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE iVerticalGap      AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE lresizeHorizontal AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE lresizeVertical   AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE lTimerEnabled     AS LOGICAL   NO-UNDO.
  
  /* do that to avoid bad flickering */
  lTimerEnabled = chCtrlFrame:PSTimerStickToWindow:ENABLED NO-ERROR.
  IF lTimerEnabled THEN DO:
      chCtrlFrame:PSTimerStickToWindow:ENABLED = NO.
      chCtrlFrame:PSTimerStickToWindow:ENABLED = YES.
  END.
  
  
  {get resizeVertical lresizeVertical hpure4glTv}.
  IF lresizeVertical = ? THEN lresizeVertical = YES.
  {get resizeHorizontal lresizeHorizontal hpure4glTv}.
  IF lresizeHorizontal = ? THEN lresizeHorizontal = YES.
  
  iTvWidth = FRAME fContainer:WIDTH-CHARS.

  iVerticalGap = SELF:HEIGHT-CHAR - FRAME fContainer:HEIGHT-CHARS.
  iHorizontalGap = SELF:WIDTH-CHAR - FRAME fContainer:WIDTH-CHARS.
  
  IF iVerticalGap > 0 THEN ASSIGN
   FRAME fContainer:HEIGHT-CHARS = SELF:HEIGHT-CHAR.
  
  IF iHorizontalGap > 0 THEN ASSIGN
   FRAME fContainer:WIDTH-CHARS = SELF:WIDTH-CHAR.
  
  RUN resizeObject IN hpure4glTv
    (IF lresizeVertical THEN SELF:HEIGHT-CHARS ELSE ?
    ,IF lresizeHorizontal THEN iTVWidth + iHorizontalGap ELSE ?).

  IF iVerticalGap < 0 THEN ASSIGN
   FRAME fContainer:HEIGHT-CHARS = SELF:HEIGHT-CHAR.

  IF iHorizontalGap < 0 THEN ASSIGN
   FRAME fContainer:WIDTH-CHARS = SELF:WIDTH-CHAR.


   /* no scrollbar when shrinking please */
   FRAME fContainer:VIRTUAL-HEIGHT-CHARS = SELF:HEIGHT-CHAR.
   FRAME fContainer:VIRTUAL-WIDTH-CHARS = SELF:WIDTH-CHAR.
   
   /* 21-MAR-2005: To fix a problem in 10.0B where the scroll thumb may 
    disappear because of the above call to get rid off native scrollbars
    in the container...
    This problem may go away in 10.0B02 */
   RUN hideObject IN hpure4glTv.
   RUN viewObject IN hpure4glTv.
   
   RUN saveSetting. /* so abhackwin is aware of the new width of this window */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame wWin OCX.Tick
PROCEDURE CtrlFrame.PSTimerStickToWindow.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iwinX AS INTEGER     NO-UNDO.
DEFINE VARIABLE iwinY AS INTEGER     NO-UNDO.

IF NOT VALID-HANDLE(ghAttachedEditor) THEN RETURN.

ASSIGN 
 iwinX = ghAttachedEditor:WINDOW:X - {&WINDOW-NAME}:WIDTH-PIXELS - 7
 iwinY = ghAttachedEditor:WINDOW:Y + 28.

IF   {&WINDOW-NAME}:X = iwinX
 AND {&WINDOW-NAME}:Y = iwinY
 THEN RETURN.

ASSIGN 
 {&WINDOW-NAME}:X = iwinX
 {&WINDOW-NAME}:Y = iwinY.
{&WINDOW-NAME}:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'protools/abhack/pure4gltv.w':U ,
             INPUT  FRAME fContainer:HANDLE ,
             INPUT  'wineModeAutomaticwindowsSkinAutomaticpicCacheCoef1labCacheCoef1tvIterationHeight17TreeStyle3FocSelNodeBgColor1UnfSelNodeBgColor8tvnodeDefaultFont4FocSelNodeFgColor15UnfSelNodeFgColor0resizeVerticalyesresizeHorizontalyesDragSourcenoneautoSortyesMSkeyScrollForcePaintyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT hpure4gltv ).
       RUN repositionObject IN hpure4gltv ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN hpure4gltv ( 13.81 , 46.00 ) NO-ERROR.

       /* Adjust the tab order of the smart objects. */
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AttachMode wWin 
PROCEDURE AttachMode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcMode AS CHARACTER   NO-UNDO.

/* 12-SEP-2008 sla: on some machine, it seems it is possible to become enabled with a temporary non ´free´ value 
perhaps it can also explain why a first saved osition is not the expected one
chCtrlFrame:PSTimerStickToWindow:ENABLED = pcMode <> "free". */
chCtrlFrame:PSTimerStickToWindow:ENABLED = pcMode = "stick". /* 12-SEP-2008 sla: on some machine, it seems it is possible to become enabled with a temporary non ´free´ value */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load wWin  _CONTROL-LOAD
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

OCXFile = SEARCH( "abhackSections.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "abhackSections.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE destroyObject wWin 
PROCEDURE destroyObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN saveSetting.

RUN SUPER. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  VIEW FRAME fContainer IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fContainer}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

APPLY "CLOSE":U TO THIS-PROCEDURE.
RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  gshABHackWin = SOURCE-PROCEDURE.
  
  
  RUN restoreSetting.
  
  SUBSCRIBE TO "abhackSectionOutlineWindowExit" ANYWHERE RUN-PROCEDURE "destroyObject".
  SUBSCRIBE TO "ListSections"      IN gshABHackWin.
  SUBSCRIBE TO "AttachMode"        IN gshABHackWin.
  SUBSCRIBE TO "SectionSelectMark" IN gshABHackWin.
  SUBSCRIBE PROCEDURE gshABHackWin TO "JumpToSectionMark" IN TARGET-PROCEDURE.
  
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  chCtrlFrame:PSTimerStickToWindow:ENABLED. /* 15-SEP-2008 sla: make sure we start with this default (have seen strange behavior on some machines) */
  SUBSCRIBE TO "tvNodeEvent" IN hpure4gltv.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ListSections wWin 
PROCEDURE ListSections :
/*------------------------------------------------------------------------------
  Purpose:    For now, just load the block marks in the treeview in the order
    of their corresponding line number.
   Later one, I will perform different queries to first add the def block, then
   the main block (if no mark for method) then the triggers, then procedures then
   function, all sorted in alphabetic order.
   
   I could probably implement a little radio set or popup menu to specify how we
   want nodes to be sorted (line numbers versus block type and names)
  
  Notes:  The main block is caught via a special blockType of 'mainBlock'
   ttMark for return points to main are ignored (they have a blockType of 'main')
   
   02-AUG-2007 sla: performance is starting to become an issue for large source file...
   For example, for the abhackwin.w produre, I have now 1591 nodes to load, against 660 when
   we don't load buffer usages.  The loading process can now take about one second
------------------------------------------------------------------------------*/

/* note that the buffer handles passed bellow have a limited scope in the calling
  procedures, so they are valid only in this API, but not later on */
DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.


DEFINE VARIABLE cBlockName              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNodeOptions            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cParameterPic           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cParameters             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cParamWhere             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cParentNode             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPicName                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cWidgetNamePart         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCommentIndex           AS INTEGER     NO-UNDO.
DEFINE VARIABLE iParameter              AS INTEGER     NO-UNDO.
DEFINE VARIABLE iParameterMax           AS INTEGER     NO-UNDO.
DEFINE VARIABLE iWidgetNamePos          AS INTEGER     NO-UNDO.
DEFINE VARIABLE lFunctionBranchCreated  AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lMethodBranchCreated    AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lProcedureBranchCreated AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lTriggerBranchCreated   AS LOGICAL     NO-UNDO.


DEFINE BUFFER ttEdt           FOR ttEdt.
DEFINE BUFFER ttMark          FOR ttMark.
DEFINE BUFFER ttProc          FOR ttProc.
DEFINE BUFFER ttFunc          FOR ttFunc.
DEFINE BUFFER ttMethod        FOR ttMethod.
DEFINE BUFFER ttReferedBuffer FOR ttReferedBuffer.
 
RUN emptyTree IN hpure4gltv.

FIND FIRST ttEdt WHERE ttEdt.hEditor = phEditor.
ghAttachedEditor = phEditor.

DEFINE QUERY qNode FOR ttMark, ttReferedBuffer.
OPEN QUERY qnode FOR EACH ttMark WHERE
     ttMark.hEditor = ttEdt.hEditor
 AND ttMark.cBlockType <> 'main'
 ,FIRST ttReferedBuffer OF ttMark OUTER-JOIN.

DO WHILE TRUE:
    GET NEXT qNode.
    IF QUERY-OFF-END("qNode") THEN LEAVE.
    
    ttMarkBlock:
    DO:
        cBlockName = ttMark.cBlockName.
        CASE ttMark.cBlockType:
            WHEN "TRIGGER"   THEN DO:
                ASSIGN 
                 cPicName    = "{&tvpicspath}trigger.bmp"
                 cParentNode = "Branch Triggers".
                IF NOT lTriggerBranchCreated THEN DO:
                    RUN addNode IN hpure4gltv
                      (cParentNode
                      ,""
                      ,"Triggers"
                      ,cPicName
                      ,"")  NO-ERROR.
                    lTriggerBranchCreated = YES.
                END.
                IF cBlockName BEGINS "ON " THEN cBlockName = SUBSTRING(cBlockName, 4).
                iWidgetNamePos = INDEX(cBlockName, " OF ") + 4.
                IF iWidgetNamePos > 4 THEN DO:
                    cWidgetNamePart = SUBSTRING(cBlockName, iWidgetNamePos - 1).
                     iCommentIndex = R-INDEX(cWidgetNamePart, "/~*").
                    IF iCommentIndex > 0 THEN ASSIGN
                     cWidgetNamePart = DYNAMIC-FUNCTION('removeLineComments' IN gshABHackWin , cWidgetNamePart, iCommentIndex).
                    cBlockName =  cWidgetNamePart + " " + LC(SUBSTRING(cBlockName, 1, iWidgetNamePos - 4)). /* make things tiner, especially interesting for large keywords like VALUE-CHANGED */
                END.
            END.
            WHEN "PROCEDURE" THEN DO:
                ASSIGN 
                 cPicName    = "{&tvpicspath}ip.bmp"
                 cParentNode = "Branch Procedures".
                IF NOT lProcedureBranchCreated THEN DO:
                    RUN addNode IN hpure4gltv
                      (cParentNode
                      ,""
                      ,"Procedures"
                      ,cPicName
                      ,"expanded")  NO-ERROR.
                    lProcedureBranchCreated = YES.
                END.
            END.
            WHEN "FUNCTION"  THEN DO:
                ASSIGN 
                 cPicName    = "{&tvpicspath}fn.bmp"
                 cParentNode = "Branch Functions".
                IF NOT lFunctionBranchCreated THEN DO:
                    RUN addNode IN hpure4gltv
                      (cParentNode
                      ,""
                      ,"Functions"
                      ,cPicName
                      ,"expanded")  NO-ERROR.
                    lFunctionBranchCreated = YES.
                END.
            END.
            WHEN "METHOD"    THEN DO:
                ASSIGN
                 cParentNode = "Branch Methods"
                 cPicName    = "{&tvpicspath}method.bmp".
                IF NOT lMethodBranchCreated THEN DO:
                    RUN addNode IN hpure4gltv
                      (cParentNode
                      ,""
                      ,"Methods"
                      ,cPicName
                      ,"expanded")  NO-ERROR.
                    lMethodBranchCreated = YES.
                END.
            END.
            OTHERWISE ASSIGN
             cPicName    = "{&tvpicspath}books05.bmp" /* for the main block */
             cParentNode = "". /* so will create a root node */
        END CASE.
        
        ASSIGN
         cParameters  = ""
         cNodeOptions = "".
        
        /*  for now, the point is to know whether we should use the addOnExpand option if there
         are buffers OR parameter.  If we know there is a buffer, then no need to loose time 
         at looking for the existence of a parameter  */
        IF NOT AVAILABLE ttReferedBuffer THEN CASE ttMark.cBlockType:
            WHEN "PROCEDURE" THEN DO:
                FIND FIRST ttProc WHERE ttProc.hEditor = phEditor AND ttProc.cName = cBlockName NO-ERROR.
                IF AVAILABLE ttProc THEN cParameters = ttProc.cParameters.
            END.
                
            WHEN "FUNCTION" THEN DO:
                FIND FIRST ttFunc WHERE ttFunc.hEditor = phEditor AND ttFunc.cName = cBlockName NO-ERROR.
                IF AVAILABLE ttFunc THEN cParameters = ttFunc.cParameters.
            END.
    
            WHEN "METHOD" THEN DO:
                FIND FIRST ttMethod WHERE ttMethod.hEditor = phEditor AND ttMethod.cName = cBlockName NO-ERROR.
                IF AVAILABLE ttMethod THEN DO:
                    cParameters = ttMethod.cParameters.
                    CASE ttMethod.cAccessMode:
                        WHEN "private"   THEN cPicName = "{&tvpicspath}privateMethod.bmp".
                        WHEN "protected" THEN cPicName = "{&tvpicspath}protectedMethod.bmp".
                        OTHERWISE cPicName = "{&tvpicspath}method.bmp".
                    END CASE.
                END.
            END.
        END CASE.
        
        IF AVAILABLE ttReferedBuffer OR cParameters > ""
         THEN cNodeOptions = "addOnExpand".
        
        RUN addNode IN hpure4gltv
          ("mark=" + STRING(ROWID(ttMark))
          ,cParentNode
          ,cBlockName
          ,cPicName
          ,cNodeOptions)  NO-ERROR.
    END. /* ttmarkBlock */

    
END. /* DO WHILE TRUE: [...]  hQuery:GET-NEXT(). */

CLOSE QUERY qNode.

/* 03-AUG-2007 sla: now load all buffer usage in a top node "buffer usage", each buffer having the API's as children node */
IF CAN-FIND(FIRST ttReferedBuffer WHERE ttReferedBuffer.heditor = phEditor)
 THEN RUN addNode IN hpure4gltv
  ("BufferScopes"
  ,""
  ,"Buffer Scopes"
  ,"{&tvpicspath}DynBrow.bmp"
  ,"addOnExpand")  NO-ERROR.


{&WINDOW-NAME}:TITLE = "Section Layout " + ttEdt.cFileName.
{&WINDOW-NAME}:MOVE-TO-TOP(). /* let's to this way for now */

DYNAMIC-FUNCTION('tvRefresh':U IN hpure4gltv).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadBufferScopeNodes wWin 
PROCEDURE loadBufferScopeNodes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER ttEdt           FOR ttEdt.
DEFINE BUFFER ttMark          FOR ttMark.
DEFINE BUFFER ttProc          FOR ttProc.
DEFINE BUFFER ttFunc          FOR ttFunc.
DEFINE BUFFER ttMethod        FOR ttMethod.
DEFINE BUFFER ttReferedBuffer FOR ttReferedBuffer.


DEFINE VARIABLE cBlockName      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cBufferName     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCurrDBBuffer   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cParameterPic   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cParameters     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPicName        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cWidgetNamePart AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCommentIndex   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iWidgetNamePos  AS INTEGER     NO-UNDO.

FIND FIRST ttEdt WHERE ttEdt.hEditor = ghAttachedEditor NO-ERROR.
IF NOT AVAILABLE ttEdt THEN RETURN.

DEFINE QUERY qNode FOR ttReferedBuffer, ttMark.
OPEN QUERY qNode FOR EACH ttReferedBuffer WHERE ttReferedBuffer.hEditor = ttEdt.hEditor
 , FIRST ttMark OF ttReferedBuffer
 BY ttReferedBuffer.cDatabase BY ttReferedBuffer.cBufferName BY ttMark.cBlockType BY ttMark.cBlockName.
 
BufferScopeBlock:
DO WHILE TRUE:
    GET NEXT qNode.
    IF QUERY-OFF-END("qNode") THEN LEAVE.

    IF cCurrDBBuffer <> ttReferedBuffer.cDataBase + "." + ttReferedBuffer.cBufferName THEN DO:
        ASSIGN 
         cCurrDBBuffer = ttReferedBuffer.cDataBase + "." + ttReferedBuffer.cBufferName
         cBufferName   = ttReferedBuffer.cBufferName
         cParameterPic = IF ttReferedBuffer.cDataBase = "" THEN "{&tvpicspath}browser.bmp" ELSE "{&tvpicspath}db.bmp".
        IF cBufferName > "" THEN cBufferName = " (" + cBufferName + ")".
        cBufferName = ttReferedBuffer.cBufferName + cBufferName.
        RUN addNode IN hpure4gltv
          ("@@" + cCurrDBBuffer
          ,"BufferScopes"
          ,cBufferName 
          ,cParameterPic
          ,"autoSort=no")  NO-ERROR. /* 19-JUN-2007 sla: don't sort parameters */
    END.
    
    cBlockName = ttMark.cBlockName.
    CASE ttMark.cBlockType:
        WHEN "TRIGGER" THEN DO:
            cPicName    = "{&tvpicspath}trigger.bmp".
            IF cBlockName BEGINS "ON " THEN cBlockName = SUBSTRING(cBlockName, 4).
            iWidgetNamePos = INDEX(cBlockName, " OF ") + 4.
            IF iWidgetNamePos > 4 THEN DO:
                cWidgetNamePart = SUBSTRING(cBlockName, iWidgetNamePos - 1).
                 iCommentIndex = R-INDEX(cWidgetNamePart, "/~*").
                IF iCommentIndex > 0 THEN ASSIGN
                 cWidgetNamePart = DYNAMIC-FUNCTION('removeLineComments' IN gshABHackWin , cWidgetNamePart, iCommentIndex).
                cBlockName =  cWidgetNamePart + " " + LC(SUBSTRING(cBlockName, 1, iWidgetNamePos - 4)). /* make things tiner, especially interesting for large keywords like VALUE-CHANGED */
            END.
        
        END.
        WHEN "PROCEDURE" THEN cPicName = "{&tvpicspath}ip.bmp".
        WHEN "FUNCTION"  THEN cPicName = "{&tvpicspath}fn.bmp".
        WHEN "METHOD"    THEN cPicName = "{&tvpicspath}method.bmp".
        OTHERWISE             cPicName = "{&tvpicspath}books05.bmp". /* for the main block */
    END CASE.
    RUN addNode IN hpure4gltv
      ("ScopedBufferAPI=" + STRING(ROWID(ttMark)) + ";" + cCurrDBBuffer
      ,"@@" + cCurrDBBuffer
      ,cBlockName
      ,cPicName
      ,"")  NO-ERROR.
END. /* bufferScopeBlock */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadMarkInfo wWin 
PROCEDURE loadMarkInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcMarkNode AS CHARACTER   NO-UNDO.

DEFINE VARIABLE rttmark AS ROWID       NO-UNDO.

DEFINE BUFFER ttMark          FOR ttMark.
DEFINE BUFFER ttProc          FOR ttProc.
DEFINE BUFFER ttFunc          FOR ttFunc.
DEFINE BUFFER ttMethod        FOR ttMethod.
DEFINE BUFFER ttReferedBuffer FOR ttReferedBuffer.

DEFINE VARIABLE cBlockName      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cBufferName     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCurrDBBuffer   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cOneParameter   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cParamWhere     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cParameterPic   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cParameters     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPicName        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cWidgetNamePart AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hQuery          AS HANDLE      NO-UNDO.
DEFINE VARIABLE iCommentIndex   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iParameterMax   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iParameter      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iWidgetNamePos  AS INTEGER     NO-UNDO.
DEFINE VARIABLE lBufferNodeDone AS LOGICAL     NO-UNDO.

/* buffer field handles for performance reasons (I call the BUFFER-FIELD() METHOD only once for them ) */

rttmark = TO-ROWID(SUBSTRING(pcMarkNode, 6)) NO-ERROR. /* remove 'mark=' prefix */
IF ERROR-STATUS:ERROR THEN DO: /* sanity check */
    MESSAGE "Invalid rowid passed to " PROGRAM-NAME(1) "!!!"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

FIND FIRST ttMark WHERE ROWID(ttMark) = rttmark NO-ERROR.
IF NOT AVAILABLE ttMark THEN DO:
    MESSAGE "Could not find ttMark record with passed rowid" pcMarkNode
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN. /* that editor is not valid anymore */
END.

CASE ttMark.cBlockType:
    WHEN "PROCEDURE" THEN DO:
        FIND FIRST ttProc WHERE ttProc.hEditor = ttMark.hEditor AND ttProc.cName = ttMark.cBlockName NO-ERROR.
        IF AVAILABLE ttProc THEN cParameters = ttProc.cParameters.
    END.
        
    WHEN "FUNCTION" THEN DO:
        FIND FIRST ttFunc WHERE ttFunc.hEditor = ttMark.hEditor AND ttFunc.cName = ttMark.cBlockName NO-ERROR.
        IF AVAILABLE ttFunc THEN cParameters = ttFunc.cParameters.
    END.

    WHEN "METHOD" THEN DO:
        FIND FIRST ttMethod WHERE ttMethod.hEditor = ttMethod.hEditor AND ttMethod.cName = ttMark.cBlockName NO-ERROR.
        IF AVAILABLE ttMethod THEN cParameters = ttMethod.cParameters.
    END.
END CASE.

IF cParameters > "" THEN DO:
    iParameterMax = NUM-ENTRIES(cParameters).
    RUN addNode IN hpure4gltv
      ("Parameters" + pcMarkNode
      ,pcMarkNode
      ,"Parameter" + (IF iParameterMax = 1 THEN "" ELSE "s") /* manage plural ;) */
      ,"{&tvpicspath}present1"
      ,"autoSort=no" + CHR(1) + "expanded")  NO-ERROR.
    DO iParameter = 1 TO iParameterMax:
        cParameterPic = "{&tvpicspath}field". /* default pic */
        cOneParameter = ENTRY(iParameter, cParameters).
        IF cOneParameter BEGINS "BUFFER " THEN cParameterPic = "{&tvpicspath}browser.bmp".
        IF cOneParameter BEGINS "INPUT " THEN cParameterPic = "{&tvpicspath}rightArrow.bmp".
        IF cOneParameter BEGINS "OUTPUT " THEN cParameterPic = "{&tvpicspath}leftArrow.bmp".
        IF cOneParameter BEGINS "INPUT-OUTPUT " THEN cParameterPic = "{&tvpicspath}doubleArrow.bmp".
        RUN addNode IN hpure4gltv
          ("#" + pcMarkNode + ";" + STRING(iParameter)
          ,"Parameters" + pcMarkNode
          ,cOneParameter
          ,cParameterPic
          ,"autoSort=no")  NO-ERROR. /* 19-JUN-2007 sla: don't sort parameters */
    END.
END. /* IF cParameters > ""  */

DEFINE QUERY qNode FOR ttMark, ttReferedBuffer.
OPEN QUERY qNode FOR EACH ttMark WHERE ROWID(ttMark) = rttmark
 , EACH ttReferedBuffer OF ttMark  BY ttReferedBuffer.cDataBase BY ttReferedBuffer.cBufferName.

referedBuffer:
REPEAT:
    GET NEXT qNode.
    IF QUERY-OFF-END('qNode') THEN LEAVE.
    IF lBufferNodeDone = NO THEN DO:
        RUN addNode IN hpure4gltv
              ("ScopedBuffers" + pcMarkNode
              ,pcMarkNode
              ,"Scoped buffers"
              ,"{&tvpicspath}DynBrow.bmp"
              ,"autoSort=no" + CHR(1) + "expanded")  NO-ERROR.
        lBufferNodeDone = YES.
    END.
    ASSIGN 
     cBufferName   = ttReferedBuffer.cDataBase
     cParameterPic = IF cBufferName = "" THEN "{&tvpicspath}browser.bmp" ELSE "{&tvpicspath}db.bmp".
    IF cBufferName > "" THEN cBufferName = " (" + cBufferName + ")".
    cBufferName = ttReferedBuffer.cBufferName + cBufferName.
    RUN addNode IN hpure4gltv
      ("@" + STRING(ROWID(ttReferedBuffer))
      ,"ScopedBuffers" + pcMarkNode
      ,cBufferName 
      ,cParameterPic
      ,"autoSort=no")  NO-ERROR. /* 19-JUN-2007 sla: don't sort parameters */
END.  /* referedBuffer: */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE restoreSetting wWin 
PROCEDURE restoreSetting :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE lSectionLayoutWinTopOnly AS LOGICAL     NO-UNDO.
DEFINE VARIABLE iSectionLayoutWinHeight  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSectionLayoutWinWidth   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSectionLayoutWinX       AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSectionLayoutWinY       AS INTEGER     NO-UNDO.

IF NOT VALID-HANDLE(gshABHackWin) THEN RETURN.

RUN getSectionLayoutWinProfile IN gshABHackWin
 (OUTPUT lSectionLayoutWinTopOnly
 ,OUTPUT iSectionLayoutWinHeight 
 ,OUTPUT iSectionLayoutWinWidth  
 ,OUTPUT iSectionLayoutWinX      
 ,OUTPUT iSectionLayoutWinY).

ASSIGN
 {&WINDOW-NAME}:TOP-ONLY      = lSectionLayoutWinTopOnly
 {&WINDOW-NAME}:HEIGHT-PIXELS = iSectionLayoutWinHeight  WHEN iSectionLayoutWinHeight <> 0
 {&WINDOW-NAME}:WIDTH-PIXELS  = iSectionLayoutWinWidth   WHEN iSectionLayoutWinWidth <> 0
 {&WINDOW-NAME}:X             = iSectionLayoutWinX       WHEN iSectionLayoutWinX <> 0
 {&WINDOW-NAME}:Y             = iSectionLayoutWinY       WHEN iSectionLayoutWinY <> 0
 NO-ERROR.

APPLY 'WINDOW-RESIZED' TO {&WINDOW-NAME}.

/* MENU-ITEM m_top-only:CHECKED IN MENU POPUP-MENU-gcSectionLayout = glSectionLayoutWinTopOnly. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveSetting wWin 
PROCEDURE saveSetting :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF VALID-HANDLE(gshABHackWin)
 THEN RUN setSectionLayoutWinProfile IN gshABHackWin
  ({&WINDOW-NAME}:TOP-ONLY
  ,{&WINDOW-NAME}:HEIGHT-PIXELS
  ,{&WINDOW-NAME}:WIDTH-PIXELS
  ,{&WINDOW-NAME}:X
  ,{&WINDOW-NAME}:Y).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SectionSelectMark wWin 
PROCEDURE SectionSelectMark :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcMarkToSelect AS CHARACTER   NO-UNDO.


/* first unsubsribe to this event otherwize, we will catch the selection to reposition to the beginning of the concerned section... */
UNSUBSCRIBE TO "tvNodeEvent" IN hpure4gltv.
DYNAMIC-FUNCTION('selectNode' IN hpure4gltv, INPUT "mark=" + pcMarkToSelect).
SUBSCRIBE TO "tvNodeEvent" IN hpure4gltv.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tvnodeEvent wWin 
PROCEDURE tvnodeEvent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcEvent   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER pcnodeKey AS CHARACTER  NO-UNDO.

DEFINE VARIABLE cMarkRowid AS CHARACTER   NO-UNDO.

CASE pcEvent:
    WHEN "SELECT" THEN DO:
        IF pcnodeKey BEGINS "mark=" THEN  cMarkRowid = ENTRY(2, pcnodeKey, "="). /* remove 'mark=' prefix */
        IF pcnodeKey BEGINS "ScopedBufferAPI=" THEN cMarkRowid = ENTRY(2, ENTRY(1,pcnodeKey, ";"), "=").
        
        IF cMarkRowid > "" THEN DO:
            PUBLISH "JumpToSectionMark" (ghAttachedEditor, cMarkRowid).
            RUN applyEntry IN hpure4gltv ("").
        END.
    END.
    
    WHEN "addOnExpand" THEN DO:
        IF pcnodeKey = "BufferScopes" THEN RUN loadBufferScopeNodes.
        ELSE RUN loadMarkInfo (pcnodeKey).
    END.
END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

