&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME ghInfoWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS ghInfoWin 
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
DEFINE NEW GLOBAL SHARED VARIABLE gshABHackWin          AS HANDLE     NO-UNDO.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
/* 20-AUG-2007 sla: little fix: default to non 0 so we can effectively restore the black color */
DEFINE VARIABLE giInfoWinBgcolor   AS INTEGER   INITIAL -1 NO-UNDO.
DEFINE VARIABLE giInfoWinFgcolor   AS INTEGER   INITIAL -1 NO-UNDO.
DEFINE VARIABLE giInfoWinHeight    AS INTEGER   NO-UNDO.
DEFINE VARIABLE giInfoWinWidth     AS INTEGER   NO-UNDO.
DEFINE VARIABLE giInfoWinX         AS INTEGER   NO-UNDO.
DEFINE VARIABLE giInfoWinY         AS INTEGER   NO-UNDO.
DEFINE VARIABLE giRestoreWinHeight AS INTEGER   NO-UNDO.
DEFINE VARIABLE giRestoreWinWidth  AS INTEGER   NO-UNDO.
DEFINE VARIABLE giRestoreWinX      AS INTEGER   NO-UNDO.
DEFINE VARIABLE giRestoreWinY      AS INTEGER   NO-UNDO.
DEFINE VARIABLE glInfoWinTopOnly   AS LOGICAL   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS gcinfo 
&Scoped-Define DISPLAYED-OBJECTS gcinfo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR ghInfoWin AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-gcinfo 
       MENU-ITEM m_top-only     LABEL "Top-only"      
              TOGGLE-BOX
       MENU-ITEM m_Display_Info_published_by_A LABEL "Display Info published by ABHack"
              TOGGLE-BOX
       MENU-ITEM m_Run_another_instance LABEL "Run another instance (and unsubscribe)"
       MENU-ITEM m_Change_colors LABEL "Change colors" .


/* Definitions of the field level widgets                               */
DEFINE VARIABLE gcinfo AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-VERTICAL
     SIZE 39.4 BY 2.86 TOOLTIP "Rich click => popup options for this window"
     BGCOLOR 0 FGCOLOR 10 FONT 6 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     gcinfo AT ROW 1 COL 1 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 39.4 BY 2.91
         FONT 4.


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
  CREATE WINDOW ghInfoWin ASSIGN
         HIDDEN             = YES
         TITLE              = "ABHack info"
         HEIGHT             = 2.91
         WIDTH              = 39.4
         MAX-HEIGHT         = 46.52
         MAX-WIDTH          = 288
         VIRTUAL-HEIGHT     = 46.52
         VIRTUAL-WIDTH      = 288
         SMALL-TITLE        = yes
         SHOW-IN-TASKBAR    = no
         CONTROL-BOX        = yes
         MIN-BUTTON         = no
         MAX-BUTTON         = no
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT ghInfoWin:LOAD-ICON("protools/abhack/tux.ico":U) THEN
    MESSAGE "Unable to load icon: protools/abhack/tux.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW ghInfoWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
ASSIGN 
       gcinfo:POPUP-MENU IN FRAME fMain       = MENU POPUP-MENU-gcinfo:HANDLE
       gcinfo:READ-ONLY IN FRAME fMain        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(ghInfoWin)
THEN ghInfoWin:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME ghInfoWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ghInfoWin ghInfoWin
ON END-ERROR OF ghInfoWin /* ABHack info */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ghInfoWin ghInfoWin
ON WINDOW-CLOSE OF ghInfoWin /* ABHack info */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ghInfoWin ghInfoWin
ON WINDOW-RESIZED OF ghInfoWin /* ABHack info */
DO:
  IF SELF:HEIGHT-PIXEL < 1 THEN RETURN.
  IF SELF:WIDTH-PIXEL < 1 THEN RETURN.
  
  
  IF SELF:WIDTH-PIXEL > FRAME fMain:WIDTH-PIXELS THEN 
   FRAME fMain:WIDTH-PIXELS = SELF:WIDTH-PIXEL.

  IF SELF:HEIGHT-PIXEL > FRAME fMain:HEIGHT-PIXELS THEN 
   FRAME fMain:HEIGHT-PIXELS = SELF:HEIGHT-PIXEL.
   
  gcinfo:WIDTH-PIXELS = SELF:WIDTH-PIXELS.
  
  gcinfo:HEIGHT-PIXELS = SELF:HEIGHT-PIXELS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gcinfo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gcinfo ghInfoWin
ON LEFT-MOUSE-DBLCLICK OF gcinfo IN FRAME fMain
DO:
  RUN toggleMaximizeMinimize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Change_colors
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Change_colors ghInfoWin
ON CHOOSE OF MENU-ITEM m_Change_colors /* Change colors */
DO:
DEFINE VARIABLE l_ok           AS LOGICAL           NO-UNDO.
DEFINE VARIABLE ibgcolor AS INTEGER INITIAL ? NO-UNDO.
DEFINE VARIABLE ifgcolor AS INTEGER INITIAL ? NO-UNDO. 
DEFINE VARIABLE dummy          AS INTEGER    NO-UNDO.

ASSIGN
 ibgcolor = gcinfo:BGCOLOR IN FRAME {&FRAME-NAME}
 ifgcolor = gcinfo:FGCOLOR.

RUN adecomm/_chscolr.p
   (INPUT "Choose the Colors for the Floating ABHack Info Window",
    INPUT "",
    INPUT FALSE,
    INPUT ?,
    INPUT ?,
    INPUT ?,
    INPUT-OUTPUT ibgcolor,
    INPUT-OUTPUT ifgcolor,
    INPUT-OUTPUT dummy,
    OUTPUT l_ok).

IF NOT l_ok THEN RETURN.

ASSIGN
 gcinfo:BGCOLOR = ibgcolor
 gcinfo:FGCOLOR = ifgcolor.
 
RUN saveSetting IN TARGET-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Display_Info_published_by_A
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Display_Info_published_by_A ghInfoWin
ON VALUE-CHANGED OF MENU-ITEM m_Display_Info_published_by_A /* Display Info published by ABHack */
DO:
  IF SELF:CHECKED THEN SUBSCRIBE TO "abhackDisplayInfo" IN gshABHackWin.
  ELSE UNSUBSCRIBE TO "abhackDisplayInfo".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Run_another_instance
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Run_another_instance ghInfoWin
ON CHOOSE OF MENU-ITEM m_Run_another_instance /* Run another instance (and unsubscribe) */
DO:
    RUN protools/abhack/abhackinfowin.w PERSISTENT.
    MENU-ITEM m_Display_Info_published_by_A:CHECKED IN MENU POPUP-MENU-gcinfo = NO.
    APPLY 'VALUE-CHANGED' TO MENU-ITEM m_Display_Info_published_by_A IN MENU POPUP-MENU-gcinfo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_top-only
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_top-only ghInfoWin
ON VALUE-CHANGED OF MENU-ITEM m_top-only /* Top-only */
DO:
  glInfoWinTopOnly = SELF:CHECKED.
  
  ghInfoWin:TOP-ONLY = glInfoWinTopOnly.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK ghInfoWin 


/* ***************************  Main Block  *************************** */


/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
    RUN saveSetting IN TARGET-PROCEDURE.
   RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
 
 {&WINDOW-NAME}:MAX-HEIGHT-PIXELS = SESSION:WORK-AREA-HEIGHT-PIXELS - 20.
  
  RUN enable_UI.
  
  RUN restoreSetting IN TARGET-PROCEDURE. 
  
  MENU-ITEM m_Display_Info_published_by_A:CHECKED IN MENU POPUP-MENU-gcinfo = YES.
  RUN SubScribeEvents IN TARGET-PROCEDURE.
  gcinfo:LOAD-MOUSE-POINTER("GLOVE").
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE abhackDisplayInfo ghInfoWin 
PROCEDURE abhackDisplayInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER cMsg AS CHARACTER   NO-UNDO.

gcinfo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cMsg.
gcInfo:TOOLTIP = ENTRY(1, gcinfo:TOOLTIP, "~n")
 + "~nGot info at " + STRING(TIME, "hh:mm:ss").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE abhackExit ghInfoWin 
PROCEDURE abhackExit :
/*------------------------------------------------------------------------------
  Purpose: 18-SEP-2007 sla: finally implemented this PROCEDURE that fill fire
           when abhackwin publishes the abhackExit event
           
           at first, I thought it could be handy to keep this window running
           but it is not handy to save the size and position
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

APPLY "CLOSE" TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI ghInfoWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(ghInfoWin)
  THEN DELETE WIDGET ghInfoWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI ghInfoWin  _DEFAULT-ENABLE
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
  DISPLAY gcinfo 
      WITH FRAME fMain IN WINDOW ghInfoWin.
  ENABLE gcinfo 
      WITH FRAME fMain IN WINDOW ghInfoWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW ghInfoWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE isAnyRunningABHackWinInfo ghInfoWin 
PROCEDURE isAnyRunningABHackWinInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER hMySelf AS HANDLE     NO-UNDO.

hMySelf = THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE restoreSetting ghInfoWin 
PROCEDURE restoreSetting :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(gshABHackWin) THEN RETURN.

RUN getInfoWinProfile IN gshABHackWin
 (OUTPUT glInfoWinTopOnly
 ,OUTPUT giInfoWinHeight 
 ,OUTPUT giInfoWinWidth  
 ,OUTPUT giInfoWinX      
 ,OUTPUT giInfoWinY
 ,OUTPUT giInfoWinBgcolor
 ,OUTPUT giInfoWinFgcolor).

ASSIGN
 ghInfoWin:TOP-ONLY      = glInfoWinTopOnly
 ghInfoWin:HEIGHT-PIXELS = giInfoWinHeight  WHEN giInfoWinHeight <> 0
 ghInfoWin:WIDTH-PIXELS  = giInfoWinWidth   WHEN giInfoWinWidth <> 0
 ghInfoWin:X             = giInfoWinX       WHEN giInfoWinX <> 0
 ghInfoWin:Y             = giInfoWinY       WHEN giInfoWinY <> 0
 gcinfo:BGCOLOR IN FRAME {&FRAME-NAME} = giInfoWinBgcolor WHEN giInfoWinBgcolor <> -1
 gcinfo:FGCOLOR          = giInfoWinFgcolor WHEN giInfoWinFgcolor <> -1
 NO-ERROR.

APPLY 'WINDOW-RESIZED' TO ghInfoWin.

MENU-ITEM m_top-only:CHECKED IN MENU POPUP-MENU-gcinfo = glInfoWinTopOnly.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveSetting ghInfoWin 
PROCEDURE saveSetting :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN
 glInfoWinTopOnly = ghInfoWin:TOP-ONLY
 giInfoWinHeight  = ghInfoWin:HEIGHT-PIXELS
 giInfoWinWidth   = ghInfoWin:WIDTH-PIXELS
 giInfoWinX       = ghInfoWin:X
 giInfoWinY       = ghInfoWin:Y
 giInfoWinBgcolor = gcinfo:BGCOLOR IN FRAME {&FRAME-NAME}
 giInfoWinFgcolor = gcinfo:FGCOLOR.

IF VALID-HANDLE(gshABHackWin)
 THEN RUN setInfoWinProfile IN gshABHackWin
  (glInfoWinTopOnly
  ,giInfoWinHeight 
  ,giInfoWinWidth  
  ,giInfoWinX      
  ,giInfoWinY
  ,giInfoWinBgcolor
  ,giInfoWinFgcolor).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SubScribeEvents ghInfoWin 
PROCEDURE SubScribeEvents :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  APPLY 'VALUE-CHANGED' TO MENU-ITEM m_Display_Info_published_by_A IN MENU POPUP-MENU-gcinfo.
  SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "isAnyRunningABHackWinInfo" ANYWHERE.
  SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "abhackExit" ANYWHERE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE toggleMaximizeMinimize ghInfoWin 
PROCEDURE toggleMaximizeMinimize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF {&WINDOW-NAME}:WINDOW-STATE = WINDOW-NORMAL THEN DO:
    ASSIGN
     giRestoreWinHeight = FRAME {&FRAME-NAME}:HEIGHT-PIXELS
     giRestoreWinWidth  = FRAME {&FRAME-NAME}:WIDTH-PIXELS
     giRestoreWinX      = {&WINDOW-NAME}:X
     giRestoreWinY      = {&WINDOW-NAME}:Y.
    {&WINDOW-NAME}:MAX-HEIGHT-PIXELS = SESSION:WORK-AREA-HEIGHT-PIXELS - 30. /* 03-DEC-2007 sla: It seems we have to hardocde this 30 for small title windows */
    {&WINDOW-NAME}:MAX-WIDTH-PIXELS = giRestoreWinWidth.
    /* {&WINDOW-NAME}:Y = 1. */
    {&WINDOW-NAME}:WINDOW-STATE = WINDOW-MAXIMIZED.
    APPLY "window-resized" TO {&WINDOW-NAME}.
    {&WINDOW-NAME}:MAX-WIDTH-PIXELS = SESSION:WORK-AREA-WIDTH-PIXELS.
END.
ELSE IF {&WINDOW-NAME}:WINDOW-STATE = WINDOW-MAXIMIZED THEN DO:
    ASSIGN
     {&WINDOW-NAME}:HEIGHT-PIXELS = giRestoreWinHeight
     {&WINDOW-NAME}:WIDTH-PIXELS  = giRestoreWinWidth
     {&WINDOW-NAME}:X             = giRestoreWinX
     {&WINDOW-NAME}:Y             = giRestoreWinY.
    {&WINDOW-NAME}:WINDOW-STATE = WINDOW-NORMAL.
    APPLY "window-resized" TO {&WINDOW-NAME}.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

