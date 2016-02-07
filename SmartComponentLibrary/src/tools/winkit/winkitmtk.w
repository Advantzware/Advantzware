&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS C-Win 
ROUTINE-LEVEL ON ERROR UNDO, THROW . 

USING Consultingwerk.Framework.* .
USING Progress.Lang.* .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/
/*------------------------------------------------------------------------
    File        : winkitmtk.w
    Purpose     : WinKit Migration Toolkit for AppBuilder based .w files 
                  (Windows, SmartWindowS)

    Syntax      : Add to ProTools palette as described here:
                  http://wiki.dynamics4.net/D4wiki/WinKit/DevelopersReference/WinKitMigrationTool

    Description : This file is part of the WinKit MTK

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jul 03 10:03:01 CEST 2010
    Notes       : This utility supports multiple typical convertion steps 
                  such as:
                      - inserting the embedwindow.i as a method library
                      - removing the window icon
                      - inserting the triggerend.i to each trigger known to the AppBuilder
                      - inserting a local-initialize procedure for ADM1 SmartWindows 
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

{Consultingwerk\annotate-definitions.i}

DEFINE STREAM instream .

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{adeuib/sharvars.i}            /* Standard shared definitions               */
{adeuib/uniwidg.i}             /* Definition of Universal Widget TEMP-TABLE */
{adeuib/brwscols.i}            /* Temp-table definitions of browse columns  */     
{adeuib/triggers.i}            /* Definition of Triggers TEMP-TABLE         */

{src/tools/winkit/ttcodesections.i}

DEFINE BUFFER b_U FOR _U.
DEFINE BUFFER b_C FOR _C.

DEFINE VARIABLE cBaseKey AS CHARACTER NO-UNDO
    INIT "Software~\Consultingwerk Ltd.~\WinKit Tools~\Migration Tool":U .

DEFINE VARIABLE lCurrentFileModified AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rsProcess fiListFile btnSelect toTrigger ~
toProcedures toFunctions toCheckMenu toAddToLocalInitialize ~
toCreateLocalInitialize toRemoveIcon toAddMethodLibrary toBuildFrameList ~
edLog btnProcess 
&Scoped-Define DISPLAYED-OBJECTS rsProcess fiListFile toTrigger ~
toProcedures toFunctions toCheckMenu toAddToLocalInitialize ~
toCreateLocalInitialize toRemoveIcon toAddMethodLibrary toBuildFrameList ~
edLog 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_File 
       MENU-ITEM m_Select_list_file LABEL "&Select file/list" ACCELERATOR "F3"
       MENU-ITEM m_Save_Log_file LABEL "Save &Log file" ACCELERATOR "F8"
       RULE
       MENU-ITEM m_Exit         LABEL "&Exit"         .

DEFINE MENU MENU-BAR-C-Win MENUBAR
       SUB-MENU  m_File         LABEL "&File"         .


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnProcess 
     LABEL "Process file(s)" 
     SIZE 87 BY 1.14.

DEFINE BUTTON btnSelect 
     LABEL "&Select" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE edLog AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 85 BY 12.86
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiListFile AS CHARACTER FORMAT "X(256)":U 
     LABEL "File/List" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE rsProcess AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Process single file", 1,
"Process list file", 2,
"Process current file", 3
     SIZE 74 BY .71 NO-UNDO.

DEFINE VARIABLE toAddMethodLibrary AS LOGICAL INITIAL no 
     LABEL "Add method library src/winkit/embedwindow.i" 
     VIEW-AS TOGGLE-BOX
     SIZE 74 BY .81 NO-UNDO.

DEFINE VARIABLE toAddToLocalInitialize AS LOGICAL INITIAL no 
     LABEL "Add embedfinalize.i to local-initialize" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY .81 NO-UNDO.

DEFINE VARIABLE toBuildFrameList AS LOGICAL INITIAL no 
     LABEL "Build List of Frames" 
     VIEW-AS TOGGLE-BOX
     SIZE 73 BY .81 NO-UNDO.

DEFINE VARIABLE toCheckMenu AS LOGICAL INITIAL yes 
     LABEL "Check for MENU-BAR and add &SCOPED-DEFINE NOMENU if required" 
     VIEW-AS TOGGLE-BOX
     SIZE 78 BY .81 NO-UNDO.

DEFINE VARIABLE toCreateLocalInitialize AS LOGICAL INITIAL no 
     LABEL "Create local-initialize if missing" 
     VIEW-AS TOGGLE-BOX
     SIZE 39.8 BY .81 NO-UNDO.

DEFINE VARIABLE toFunctions AS LOGICAL INITIAL no 
     LABEL "Add functionend.i to every function" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY .81 NO-UNDO.

DEFINE VARIABLE toProcedures AS LOGICAL INITIAL yes 
     LABEL "Add procedureend.i to every procedure" 
     VIEW-AS TOGGLE-BOX
     SIZE 43 BY .81 NO-UNDO.

DEFINE VARIABLE toRemoveIcon AS LOGICAL INITIAL yes 
     LABEL "Remove ICON and SMALL-ICON" 
     VIEW-AS TOGGLE-BOX
     SIZE 78 BY .81 NO-UNDO.

DEFINE VARIABLE toTrigger AS LOGICAL INITIAL yes 
     LABEL "Add triggerend.i to every trigger" 
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rsProcess AT ROW 1.48 COL 11 NO-LABEL WIDGET-ID 16
     fiListFile AT ROW 2.67 COL 9 COLON-ALIGNED WIDGET-ID 4
     btnSelect AT ROW 2.67 COL 74 WIDGET-ID 2
     toTrigger AT ROW 5.05 COL 11 WIDGET-ID 8
     toProcedures AT ROW 6 COL 11 WIDGET-ID 10
     toFunctions AT ROW 6 COL 52 WIDGET-ID 12
     toCheckMenu AT ROW 6.95 COL 11 WIDGET-ID 14
     toAddToLocalInitialize AT ROW 7.91 COL 11 WIDGET-ID 22
     toCreateLocalInitialize AT ROW 7.91 COL 49.2 WIDGET-ID 24
     toRemoveIcon AT ROW 8.86 COL 11 WIDGET-ID 26
     toAddMethodLibrary AT ROW 9.81 COL 11 WIDGET-ID 28
     toBuildFrameList AT ROW 10.76 COL 11 WIDGET-ID 30
     edLog AT ROW 12.19 COL 3 NO-LABEL WIDGET-ID 20
     btnProcess AT ROW 25.29 COL 2 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 88.4 BY 25.57 DROP-TARGET WIDGET-ID 100.


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
         TITLE              = "WinKit Migration Tool"
         HEIGHT             = 25.57
         WIDTH              = 88.4
         MAX-HEIGHT         = 25.57
         MAX-WIDTH          = 90.4
         VIRTUAL-HEIGHT     = 25.57
         VIRTUAL-WIDTH      = 90.4
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

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-C-Win:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       edLog:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* WinKit Migration Tool */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* WinKit Migration Tool */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* WinKit Migration Tool */
DO:
    DYNAMIC-FUNCTION("SetWidgetResize":U, 
                     THIS-PROCEDURE:CURRENT-WINDOW,
                     THIS-PROCEDURE:CURRENT-WINDOW,
                     "Resize":U,
                     "":U).    
                     
    ASSIGN btnProcess:HEIGHT IN FRAME {&frame-name} = 1.14 .                     
                     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME DEFAULT-FRAME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DEFAULT-FRAME C-Win
ON DROP-FILE-NOTIFY OF FRAME DEFAULT-FRAME
DO:
  
    ASSIGN fiListFile:SCREEN-VALUE = SELF:GET-DROPPED-FILE (1) . 

    IF fiListFile:SCREEN-VALUE MATCHES "*~.w":U THEN
        rsProcess:SCREEN-VALUE IN FRAME {&frame-name} = "1":U .
    ELSE 
        rsProcess:SCREEN-VALUE IN FRAME {&frame-name} = "2":U .  

    SELF:END-FILE-DROP () .



END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnProcess
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnProcess C-Win
ON CHOOSE OF btnProcess IN FRAME DEFAULT-FRAME /* Process file(s) */
DO:
    DO ON STOP UNDO, RETURN:

        ASSIGN FRAME {&frame-name}
               rsProcess 
               fiListFile 
               toTrigger 
               toProcedures 
               toFunctions 
               toCheckMenu 
               toAddToLocalInitialize
               toCreateLocalInitialize
               toRemoveIcon
               toAddMethodLibrary
               toBuildFrameList
               .
    
        edLog:SCREEN-VALUE IN FRAME {&frame-name} = "":U .
    
        CASE rsProcess:
            WHEN 1 THEN 
                RUN ProcessFile . 
            WHEN 2 THEN 
                RUN ProcessListFile . 
            WHEN 3 THEN 
                RUN ProcessCurrentFile . 
        END.
      
        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err, "WinKit Migration Tool":U) .        
        END CATCH.
            
    END.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelect C-Win
ON CHOOSE OF btnSelect IN FRAME DEFAULT-FRAME /* Select */
DO:
    RUN openFile . 
  
    CATCH err AS Progress.Lang.Error :
        Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err, "WinKit Migration Tool":U) .        
    END CATCH.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Exit C-Win
ON CHOOSE OF MENU-ITEM m_Exit /* Exit */
DO:
  APPLY "window-close":U TO {&WINDOW-NAME} .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Save_Log_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Save_Log_file C-Win
ON CHOOSE OF MENU-ITEM m_Save_Log_file /* Save Log file */
DO:
    DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lok       AS LOGICAL   NO-UNDO.

    SYSTEM-DIALOG GET-FILE cFileName
        FILTERS "Log Files (*.log)":U "*.log":U,
                "All files (*.*)":U "*.*":U
        INITIAL-FILTER 1
        DEFAULT-EXTENSION "log":U
        ASK-OVERWRITE 
        CREATE-TEST-FILE 
        SAVE-AS
        TITLE "Save log file":U
        USE-FILENAME 
        UPDATE lOk
        IN WINDOW {&window-name} .

    IF lOk THEN 
        edLog:SAVE-FILE (cFileName) IN FRAME {&frame-name} .
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Select_list_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Select_list_file C-Win
ON CHOOSE OF MENU-ITEM m_Select_list_file /* Select file/list */
DO:
    RUN openFile .   
  
    CATCH err AS Progress.Lang.Error :
        Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err, "WinKit Migration Tool":U) .        
    END CATCH.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME toAddToLocalInitialize
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL toAddToLocalInitialize C-Win
ON VALUE-CHANGED OF toAddToLocalInitialize IN FRAME DEFAULT-FRAME /* Add embedfinalize.i to local-initialize */
DO:
  ASSIGN toCreateLocalInitialize:SENSITIVE = toAddToLocalInitialize:CHECKED .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

{ Consultingwerk\annotate-main-block.i }
{ src/ResizeLib/resizelib.i }

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   RUN StoreSettings . 
   
   RUN disable_UI.
END.


/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

RUN CheckPreConditions .

ASSIGN {&window-name}:MIN-WIDTH = {&window-name}:WIDTH 
       {&window-name}:MIN-HEIGHT = {&window-name}:HEIGHT
    .

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.

  DYNAMIC-FUNCTION("setAddResizeX":U,
                   {&WINDOW-NAME}:HANDLE,
                   FRAME {&FRAME-NAME}:HANDLE,
                   "fiListFile,btnProcess":U).

  DYNAMIC-FUNCTION("SetOrgWinSize":U,                                
                   {&WINDOW-NAME}:HANDLE,                   
                   300,                                             
                   230,                                             
                   {&WINDOW-NAME}:WIDTH-PIXELS ,                    
                   {&WINDOW-NAME}:HEIGHT-PIXELS).                   

  RUN RestoreSettings .

  APPLY "WINDOW-RESIZED":U TO {&WINDOW-NAME} .

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

CATCH err AS Progress.Lang.Error :
    Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err, "WinKit Migration Tool":U) .
END CATCH.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AddLogMessage C-Win 
PROCEDURE AddLogMessage :
/*------------------------------------------------------------------------------
        Purpose:                                                                                                                                          
        Notes:                                                                                                                                            
------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER pcLogMessage AS CHARACTER NO-UNDO.

    edLog:MOVE-TO-EOF () IN FRAME {&frame-name} .
    edLog:INSERT-STRING (pcLogMessage + CHR (10)) .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AddMethodLibrary C-Win 
PROCEDURE AddMethodLibrary :
/*------------------------------------------------------------------------------
        Purpose: Add src/winkit/embedwindow.i to the included sections                                                                                                                                    
        Notes:                                                                                                                                            
------------------------------------------------------------------------------*/

    DEFINE BUFFER b_U FOR _U . 
    DEFINE BUFFER b_P FOR _P . 
    DEFINE BUFFER b_TRG FOR _TRG .

    DEFINE VARIABLE cCurrentMLi AS CHARACTER NO-UNDO.
    DEFINE VARIABLE c_info      AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hWindow     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE win_recid   AS RECID     NO-UNDO.
    
    RUN adeuib/_uibinfo.p (?, ?, "HANDLE":U, OUTPUT c_info).      
      
    IF c_info = ? THEN 
        RETURN . 
/*        UNDO, THROW NEW AppError ("No file currently opened.":U, 0) .*/
      
    FIND b_U WHERE b_U._HANDLE = WIDGET-HANDLE (c_info) .    
     
    ASSIGN hWindow   = b_U._WINDOW-HANDLE
           win_recid = RECID (b_U) . 

    FIND b_P WHERE b_P._WINDOW-HANDLE = hWindow.
  
    /* Process internal procedures */
    FIND FIRST b_TRG WHERE b_TRG._wRECID = b_p._u-recid 
                       AND b_TRG._tEVENT = "_INCLUDED-LIB":U
                    NO-ERROR.    
 
    IF NOT AVAILABLE b_TRG THEN 
        ASSIGN cCurrentMLi = "":U . 
    ELSE 
        ASSIGN cCurrentMLi = b_TRG._tCODE .
        
    IF NOT cCurrentMLi MATCHES "*src/winkit/embedwindow.i*":U AND 
       NOT cCurrentMLi MATCHES "*src\winkit\embedwindow.i*":U THEN DO:
        
        ASSIGN cCurrentMLi = cCurrentMLi + 
                             System.Environment:NewLine + 
                             "~{src/winkit/embedwindow.i~}":U .      
    
        IF NOT AVAILABLE b_TRG THEN DO:
            CREATE b_TRG.
            ASSIGN b_TRG._pRECID   = (IF AVAIL(b_P) THEN RECID(b_P) ELSE ?)
                   b_TRG._wRECID   = /*RECID(b_U)*/ b_p._u-recid 
                   b_TRG._tSECTION = "_CUSTOM":U
                   b_TRG._tEVENT   = "_INCLUDED-LIB":U
                   b_TRG._tCODE    = cCurrentMLi 
                   b_TRG._STATUS   = "NORMAL":U .
        END.
        ELSE 
            ASSIGN b_TRG._tCODE = cCurrentMLi .
            
        RUN AddLogMessage ("Added method library include":U) .            
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

 
&IF DEFINED(EXCLUDE-BuildFrameList) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildFrameList Include
PROCEDURE BuildFrameList:
/*------------------------------------------------------------------------------
    Purpose:  																	  
    Notes:  																	  
------------------------------------------------------------------------------*/

    DEFINE VARIABLE hWindow AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cFrames AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cCode   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE reRecid AS RECID     NO-UNDO.
    DEFINE VARIABLE hFrame  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cFrame  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  ASSIGN hWindow = b_U._HANDLE 
         cFrames = Consultingwerk.Util.WidgetHelper:FindAllFrames (b_U._HANDLE).

  ASSIGN reRecid = ? . 
    
  ASSIGN cCode = "/*------------------------------------------------------------------------------":U  + CHR(10) + 
                 "    Purpose: Assigns a list of FRAME Handles, so that they are accessible also ":U   + CHR(10) + 
                 "             after reparenting them from the DEFAULT-FRAME":U                        + CHR(10) + 
                 "    Notes:   Added by the WinKitMTK ":U                                              + CHR(10) + 
                 "------------------------------------------------------------------------------*/":U  + CHR(10) + CHR(10) + 
                 
                 "ASSIGN cWinKitListOfFrameHandles = ":U.
                  
  DO i = 1 TO NUM-ENTRIES (cFrames):
      
      ASSIGN hFrame = WIDGET-HANDLE (ENTRY (i, cFrames)) .
      
      RUN adeuib/_uibinfo.p (?, "HANDLE " + STRING (hFrame), "NAME":U, OUTPUT cFrame).
      
      ASSIGN cCode  = cCode + 
                      (IF i > 1 THEN " + ~",~" + ":U ELSE "":U) + 
                      SUBSTITUTE ("STRING(FRAME &1:HANDLE)":U, cFrame) . 
  END.                  
                  
  ASSIGN cCode = cCode + " .":U + CHR(10) + CHR(10) + "END PROCEDURE.":U + CHR(10).
    
  RUN adeuib/_accsect.p
        (INPUT "SET":U,             /* We want to read the code */
         INPUT ?,                   /* The context id from above */
         INPUT "PROCEDURE:WinKitAssignFrameHandles:":U, /* The trigger we want */
         INPUT-OUTPUT reRecid, /*  */
         INPUT-OUTPUT cCode). /* The code block */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckAddFunction C-Win 
PROCEDURE CheckAddFunction :
/*------------------------------------------------------------------------------
    Purpose:                                                                      
    Notes:                                                                        
------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER pReContext AS RECID     NO-UNDO.
    DEFINE INPUT  PARAMETER pcName     AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cEnd    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCode   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE reRecid AS RECID     NO-UNDO.
    
    ASSIGN reRecid = ? . 
    
    RUN adeuib/_accsect.p
        (INPUT "GET":U,                        /* We want to read the code */
         INPUT pReContext,                   /* The context id from above */
         INPUT pcName,                       /* The trigger we want */
         INPUT-OUTPUT reRecid, /* The trigger context so we can
                                  reference this block directly
                                  in the future. IMPORTANT: This
                                  must be ? intially or _accsect
                                  will attempt to use it instead
                                  of parameter 2 */
         INPUT-OUTPUT cCode). /* The code block */
    
    IF NOT cCode MATCHES "*winkit/functionend~.i*":U AND 
       NOT cCode MATCHES "*winkit\functionend~.i*":U THEN DO:
        
        RUN StripEndStatement (INPUT-OUTPUT cCode, OUTPUT cEnd) .
                
        ASSIGN cCode = SUBSTITUTE ("&1&3&3  /* Added by WinKit Migration tool &2 */&3  ~{ src/winkit/functionend.i ~"&4~"}&3&3&5":U,
                                   cCode,
                                   STRING(NOW, "99.99.9999 HH:MM:SS":U),
                                   CHR (10),
                                   ENTRY (2, pcName, ":":U),
                                   cEnd) .

        ASSIGN lCurrentFileModified = TRUE . 

        RUN AddLogMessage ("Added include file to ":U + pcName) .

        RUN adeuib/_accsect.p
            (INPUT "SET":U,                        /* We want to read the code */
             INPUT ?,                   /* The context id from above */
             INPUT ?,    /* The trigger we want */
             INPUT-OUTPUT reRecid, /* The trigger context so we can
                                      reference this block directly
                                      in the future. IMPORTANT: This
                                      must be ? intially or _accsect
                                      will attempt to use it instead
                                      of parameter 2 */
             INPUT-OUTPUT cCode). /* The code block */
    END.   
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckAddNoMenu C-Win 
PROCEDURE CheckAddNoMenu :
/*------------------------------------------------------------------------------
        Purpose:                                                                                                                                          
        Notes:                                                                                                                                            
------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER pReContext AS RECID NO-UNDO.
    
    DEFINE VARIABLE cCode   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE reRecid AS RECID NO-UNDO.
    
    ASSIGN reRecid = ? . 
    
    RUN adeuib/_accsect.p
        (INPUT "GET":U,                        /* We want to read the code */
         INPUT pReContext,                   /* The context id from above */
         INPUT "DEFINITIONS":U,    /* The trigger we want */
         INPUT-OUTPUT reRecid, /* The trigger context so we can
                                  reference this block directly
                                  in the future. IMPORTANT: This
                                  must be ? intially or _accsect
                                  will attempt to use it instead
                                  of parameter 2 */
         INPUT-OUTPUT cCode). /* The code block */
    
    IF NOT cCode MATCHES "*SCOPED-DEFINE NOMENU*":U THEN DO:
        
        ASSIGN cCode = SUBSTITUTE ("&1&3&3/* Added by WinKit Migration tool &2 */&3&&SCOPED-DEFINE NOMENU":U,
                                   cCode,
                                   STRING(NOW, "99.99.9999 HH:MM:SS":U),
                                   CHR (10)) .

        ASSIGN lCurrentFileModified = TRUE . 

        RUN AddLogMessage ("Added ~&SCOPED-DEFINE NOMENU":U) .

        RUN adeuib/_accsect.p
            (INPUT "SET":U,                        /* We want to read the code */
             INPUT ?,                   /* The context id from above */
             INPUT ?,    /* The trigger we want */
             INPUT-OUTPUT reRecid, /* The trigger context so we can
                                      reference this block directly
                                      in the future. IMPORTANT: This
                                      must be ? intially or _accsect
                                      will attempt to use it instead
                                      of parameter 2 */
             INPUT-OUTPUT cCode). /* The code block */
    END.   
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckAddProcedure C-Win 
PROCEDURE CheckAddProcedure :
/*------------------------------------------------------------------------------
    Purpose:                                                                      
    Notes:                                                                        
------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER pReContext AS RECID     NO-UNDO.
    DEFINE INPUT  PARAMETER pcName     AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cEnd    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCode   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE reRecid AS RECID     NO-UNDO.
    
    ASSIGN reRecid = ? . 
    
    RUN adeuib/_accsect.p
        (INPUT "GET":U,                        /* We want to read the code */
         INPUT pReContext,                   /* The context id from above */
         INPUT pcName,                       /* The trigger we want */
         INPUT-OUTPUT reRecid, /* The trigger context so we can
                                  reference this block directly
                                  in the future. IMPORTANT: This
                                  must be ? intially or _accsect
                                  will attempt to use it instead
                                  of parameter 2 */
         INPUT-OUTPUT cCode). /* The code block */
    
    IF NOT cCode MATCHES "*winkit/procedureend~.i*":U AND 
       NOT cCode MATCHES "*winkit\procedureend~.i*":U THEN DO:
        
        RUN StripEndStatement (INPUT-OUTPUT cCode, OUTPUT cEnd) .
                
        ASSIGN cCode = SUBSTITUTE ("&1&3&3  /* Added by WinKit Migration tool &2 */&3  ~{ src/winkit/procedureend.i ~"&4~"}&3&3&5":U,
                                   cCode,
                                   STRING(NOW, "99.99.9999 HH:MM:SS":U),
                                   CHR (10),
                                   ENTRY (2, pcName, ":":U),
                                   cEnd) .

        ASSIGN lCurrentFileModified = TRUE . 

        RUN AddLogMessage ("Added include file to ":U + pcName) .

        RUN adeuib/_accsect.p
            (INPUT "SET":U,                        /* We want to read the code */
             INPUT ?,                   /* The context id from above */
             INPUT ?,    /* The trigger we want */
             INPUT-OUTPUT reRecid, /* The trigger context so we can
                                      reference this block directly
                                      in the future. IMPORTANT: This
                                      must be ? intially or _accsect
                                      will attempt to use it instead
                                      of parameter 2 */
             INPUT-OUTPUT cCode). /* The code block */
    END.   
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckAddTrigger C-Win 
PROCEDURE CheckAddTrigger :
/*------------------------------------------------------------------------------
        Purpose:                                                                                                                                          
        Notes:                                                                                                                                            
------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER pReContext AS RECID     NO-UNDO.
    DEFINE INPUT  PARAMETER pcName     AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cEnd    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCode   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE reRecid AS RECID     NO-UNDO.
    
    /* Mike Fechner, Consultingwerk Ltd. 24.07.2010
       Ignore pseudo trigger for freeform browsers */
    IF pcName = "TRIGGER:OPEN_QUERY":U OR pcName = "TRIGGER:DISPLAY":U THEN
        RETURN .
    
    ASSIGN reRecid = ? . 
    
    RUN adeuib/_accsect.p
        (INPUT "GET":U,                        /* We want to read the code */
         INPUT pReContext,                   /* The context id from above */
         INPUT pcName,                       /* The trigger we want */
         INPUT-OUTPUT reRecid, /* The trigger context so we can
                                  reference this block directly
                                  in the future. IMPORTANT: This
                                  must be ? intially or _accsect
                                  will attempt to use it instead
                                  of parameter 2 */
         INPUT-OUTPUT cCode). /* The code block */
    
    IF NOT cCode MATCHES "*winkit/triggerend~.i*":U AND 
       NOT cCode MATCHES "*winkit\triggerend~.i*":U THEN DO:
        
        RUN StripEndStatement (INPUT-OUTPUT cCode, OUTPUT cEnd) .
                
        ASSIGN cCode = SUBSTITUTE ("&1&3&3  /* Added by WinKit Migration tool &2 */&3  ~{ Advantzware/WinKit/winkit-panel-triggerend.i ~"&4~"}&3&3&5":U,
                                   cCode,
                                   STRING(NOW, "99.99.9999 HH:MM:SS":U),
                                   CHR (10),
                                   ENTRY (2, pcName, ":":U),
                                   cEnd) .

        ASSIGN lCurrentFileModified = TRUE . 

        RUN AddLogMessage ("Added include file to ":U + pcName) .

        RUN adeuib/_accsect.p
            (INPUT "SET":U,                        /* We want to read the code */
             INPUT ?,                   /* The context id from above */
             INPUT ?,    /* The trigger we want */
             INPUT-OUTPUT reRecid, /* The trigger context so we can
                                      reference this block directly
                                      in the future. IMPORTANT: This
                                      must be ? intially or _accsect
                                      will attempt to use it instead
                                      of parameter 2 */
             INPUT-OUTPUT cCode). /* The code block */
    END.   
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckLocalInitialize C-Win 
PROCEDURE CheckLocalInitialize :
/*------------------------------------------------------------------------------
        Purpose:                                                                                                                                          
        Notes:                                                                                                                                            
------------------------------------------------------------------------------*/

    DEFINE VARIABLE cEnd    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCode   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE reRecid AS RECID     NO-UNDO.
    
    ASSIGN reRecid = ? . 
    
    RUN adeuib/_accsect.p
        (INPUT "GET":U,                      /* We want to read the code */
         INPUT ?,                            /* The context id from above */
         INPUT "PROCEDURE:local-initialize":U, /* The trigger we want */
         INPUT-OUTPUT reRecid, /* The trigger context so we can
                                  reference this block directly
                                  in the future. IMPORTANT: This
                                  must be ? intially or _accsect
                                  will attempt to use it instead
                                  of parameter 2 */
         INPUT-OUTPUT cCode). /* The code block */
    
    IF cCode = ? THEN DO:
        IF toCreateLocalInitialize THEN DO:
            ASSIGN cCode = SUBSTITUTE ("/*------------------------------------------------------------------------------&1":U +
                                       "  Purpose:     Override standard ADM method&1":U +
                                       "  Notes:       &1":U +
                                       "------------------------------------------------------------------------------*/&1&1":U +
                                       "  /* Code placed here will execute PRIOR to standard behavior. */&1&2":U +
                                       "  /* Dispatch standard ADM method.                             */&1&1":U +
                                       "  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .&1&1":U +
                                       "   /* Code placed here will execute AFTER standard behavior.    */&1&1":U + 
                                       "END PROCEDURE.&1":U,
                                       CHR (10)) .
            RUN AddLogMessage ("Added local-initialize":U) .
        END.          
        ELSE 
            RETURN . 
    END.
    
    
    IF NOT cCode MATCHES "*winkit/embedfinalize~.i*":U AND 
       NOT cCode MATCHES "*winkit\embedfinalize~.i*":U THEN DO:
        
        RUN StripEndStatement (INPUT-OUTPUT cCode, OUTPUT cEnd) .
                
        ASSIGN cCode = SUBSTITUTE ("&1&3&3  /* Added by WinKit Migration tool &2 */&3  ~{ src/winkit/embedfinalize.i}&3&3&4":U,
                                   cCode,
                                   STRING(NOW, "99.99.9999 HH:MM:SS":U),
                                   CHR (10),
                                   cEnd) .

        ASSIGN lCurrentFileModified = TRUE . 

        RUN AddLogMessage ("Added include file to local-initialize":U) .

        IF reRecid <> ? THEN 
            RUN adeuib/_accsect.p
                (INPUT "SET":U,                        /* We want to read the code */
                 INPUT ?,                   /* The context id from above */
                 INPUT ?,    /* The trigger we want */
                 INPUT-OUTPUT reRecid, /* The trigger context so we can
                                          reference this block directly
                                          in the future. IMPORTANT: This
                                          must be ? intially or _accsect
                                          will attempt to use it instead
                                          of parameter 2 */
                 INPUT-OUTPUT cCode). /* The code block */
        ELSE 
            RUN adeuib/_accsect.p
                (INPUT "SET":U,                        /* We want to read the code */
                 INPUT ?,                   /* The context id from above */
                 INPUT "PROCEDURE:local-initialize":U,    /* The trigger we want */
                 INPUT-OUTPUT reRecid, /* The trigger context so we can
                                          reference this block directly
                                          in the future. IMPORTANT: This
                                          must be ? intially or _accsect
                                          will attempt to use it instead
                                          of parameter 2 */
                 INPUT-OUTPUT cCode). /* The code block */
    END.   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckPreConditions C-Win 
PROCEDURE CheckPreConditions :
/*------------------------------------------------------------------------------
        Purpose: Checks pre-conditions for this tool, raises error when not met.                                                                                                                                          
        Notes:   The WinKit migration tool is intended to be started persistently
                 from within an AppBuilder session                                                                                                                                
------------------------------------------------------------------------------*/

    IF NOT THIS-PROCEDURE:PERSISTENT THEN 
        UNDO, THROW NEW AppError ("WinKit migration tool needs to be run presistently.":U, 0) .

    IF NOT VALID-HANDLE (_h_UIB) THEN 
        UNDO, THROW NEW AppError ("AppBuilder must be running before using the WinKit migration tool.":U, 0) .

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
  DISPLAY rsProcess fiListFile toTrigger toProcedures toFunctions toCheckMenu 
          toAddToLocalInitialize toCreateLocalInitialize toRemoveIcon 
          toAddMethodLibrary toBuildFrameList edLog 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rsProcess fiListFile btnSelect toTrigger toProcedures toFunctions 
         toCheckMenu toAddToLocalInitialize toCreateLocalInitialize 
         toRemoveIcon toAddMethodLibrary toBuildFrameList edLog btnProcess 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetInternalProcedures C-Win 
PROCEDURE GetInternalProcedures :
/*------------------------------------------------------------------------------
        Purpose: Builds a list of code sections in the current program                                                                                                                                            
        Notes:                                                                                                                                            
------------------------------------------------------------------------------*/

    DEFINE VARIABLE hWindow     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE win_recid   AS RECID     NO-UNDO.

    EMPTY TEMP-TABLE ttCodeSections . 
      
    ASSIGN hWindow   = b_U._WINDOW-HANDLE
           win_recid = RECID (b_U) . 

    /* Process Triggers */
    FOR EACH b_U WHERE b_U._WINDOW-HANDLE = hWindow
                  AND b_U._STATUS <> "DELETED":U:
        FOR EACH _TRG WHERE _TRG._wRECID = RECID (b_U)
                        AND _TRG._STATUS <> "DELETED":U
                        AND _TRG._tSECTION = "_CONTROL":U:
  
            CREATE ttCodeSections .
            ASSIGN ttCodeSections.widget_name  = b_U._NAME 
                   ttCodeSections.code_context = RECID (b_U) 
                   ttCodeSections.code_block   = "TRIGGER:":U + _TRG._tEVENT .             
        END.
        
        IF b_U._TYPE = "BROWSE":U THEN DO:
            FOR EACH _BC WHERE _BC._x-recid = RECID(b_U),
                EACH _TRG WHERE _TRG._wRECID = RECID (_BC) AND
                                _TRG._STATUS <> "DELETED":U AND
                                _TRG._tSECTION = "_CONTROL":U:

                CREATE ttCodeSections .
                ASSIGN ttCodeSections.widget_name  = _BC._DISP-NAME 
                       ttCodeSections.code_context = RECID (_BC) 
                       ttCodeSections.code_block   = "TRIGGER:":U + _TRG._tEVENT .             
            END.
        END.
    END.

    /* Process internal procedures */
    FOR EACH _TRG WHERE _TRG._wRECID = win_recid  AND _TRG._STATUS <> "DELETED":U
                          AND _TRG._tSECTION = "_PROCEDURE":U
                          AND _TRG._tSPECIAL = ? /* ignore AppBuilder maintained procedures */:
                              
        CREATE ttCodeSections .
        ASSIGN ttCodeSections.widget_name  = ? 
               ttCodeSections.code_context = win_recid 
               ttCodeSections.code_block   = "PROCEDURE:":U + _TRG._tEVENT .             
    END.

    /* Process user defined functions */
    FOR EACH _TRG WHERE _TRG._wRECID = win_recid  AND _TRG._STATUS <> "DELETED":U
                          AND _TRG._tSECTION = "_FUNCTION":U:
                              
        CREATE ttCodeSections .
        ASSIGN ttCodeSections.widget_name  = ? 
               ttCodeSections.code_context = win_recid 
               ttCodeSections.code_block   = "FUNCTION:":U + _TRG._tEVENT .             
    END.

    FINALLY:

        RELEASE ttCodeSections NO-ERROR . 
        
    END FINALLY.        

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openFile C-Win 
PROCEDURE openFile :
/*------------------------------------------------------------------------------
        Purpose:                                                                                                                                          
        Notes:                                                                                                                                            
------------------------------------------------------------------------------*/

    DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lOk       AS LOGICAL   NO-UNDO.

    ASSIGN cFileName = fiListFile:SCREEN-VALUE IN FRAME {&frame-name} .

    SYSTEM-DIALOG GET-FILE cFileName
        FILTERS "AppBuilder Files (*.w)":U "*.w":U,
                "List Files (*.txt)":U "*.txt":U,
                "All files (*.*)":U "*.*"
        INITIAL-FILTER 1
        DEFAULT-EXTENSION "w":U
        MUST-EXIST 
        TITLE "Open file":U
        USE-FILENAME 
        UPDATE lOk
        IN WINDOW {&window-name} .

    IF lOk THEN 
        fiListFile:SCREEN-VALUE IN FRAME {&frame-name} = cFileName .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcessCurrentFile C-Win 
PROCEDURE ProcessCurrentFile :
/*------------------------------------------------------------------------------
        Purpose: Processes the currently opened file                                                                                                                                      
        Notes:                                                                                                                                            
------------------------------------------------------------------------------*/

    DEFINE VARIABLE reRecid   AS RECID     NO-UNDO.
    DEFINE VARIABLE cCode     AS CHARACTER NO-UNDO.      
    DEFINE VARIABLE c_info    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hWindow   AS HANDLE    NO-UNDO. 
    DEFINE VARIABLE char-hndl AS CHARACTER NO-UNDO. 
    
    ASSIGN lCurrentFileModified = FALSE . 
    
    RUN adeuib/_uibinfo.p (?, ?, "HANDLE":U, OUTPUT c_info).      
      
    IF c_info = ? THEN 
        RETURN . 
/*        UNDO, THROW NEW AppError ("No file currently opened.":U, 0) .*/
      
    FIND b_U WHERE b_U._HANDLE = WIDGET-HANDLE (c_info) .
    FIND b_C WHERE RECID(b_C)  = b_U._x-recid.

    ASSIGN hWindow   = b_U._WINDOW-HANDLE .
    
    IF toCheckMenu AND CAN-QUERY (hWindow, "MENU-BAR":U) AND NOT VALID-HANDLE (hWindow:MENU-BAR) THEN 
        RUN CheckAddNoMenu (RECID (b_U)).        

    IF toAddToLocalInitialize THEN 
        RUN CheckLocalInitialize .

    IF toRemoveIcon THEN 
        RUN RemoveIcon .
        
    IF toBuildFrameList THEN  
        RUN BuildFrameList .

    IF toAddMethodLibrary THEN 
        RUN AddMethodLibrary .

    RUN GetInternalProcedures .
                
    FOR EACH ttCodeSections:

        IF ttCodeSections.code_block BEGINS "TRIGGER:":U AND toTrigger THEN 
            RUN CheckAddTrigger (ttCodeSections.code_context,
                                 ttCodeSections.code_block) .
                                 
        IF ttCodeSections.code_block BEGINS "PROCEDURE:":U AND toProcedures THEN 
            RUN CheckAddProcedure (ttCodeSections.code_context,
                                 ttCodeSections.code_block) .

        IF ttCodeSections.code_block BEGINS "FUNCTION:":U AND toFunctions THEN 
            RUN CheckAddFunction (ttCodeSections.code_context,
                                 ttCodeSections.code_block) .
    END .              

    IF lCurrentFileModified THEN DO:
        /* Mark the file for save */
        /* First get the handle of the design window */
        RUN adeuib/_uibinfo.p
                    (INPUT ?, /* We don’t know the context id */
                     INPUT "WINDOW ?":U, /* We want the handle of the design window */
                     INPUT "HANDLE":U, /* We want the HANDLE */
                     OUTPUT char-hndl). /* Returns a string of the handle */
    
        /* Now indicate that the window needs to be saved again */
        RUN adeuib/_winsave.p (HANDLE(char-hndl), FALSE).    
    END.        

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcessFile C-Win 
PROCEDURE ProcessFile :
/*------------------------------------------------------------------------------
        Purpose:                                                                                                                                          
        Notes:                                                                                                                                            
------------------------------------------------------------------------------*/

    FILE-INFO:FILE-NAME = fiListFile .

    IF FILE-INFO:FULL-PATHNAME = ? THEN 
        UNDO, THROW NEW AppError (SUBSTITUTE("Unable to process file ~"&1~". File not found.":U, 
                                  fiListFile), 0).

    RUN ProcessFileName (FILE-INFO:FULL-PATHNAME).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcessFileName C-Win 
PROCEDURE ProcessFileName :
/*------------------------------------------------------------------------------
        Purpose:                                                                                                                                          
        Notes:                                                                                                                                            
------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER pcFileName AS CHARACTER NO-UNDO.

    RUN AddLogMessage ("Processing ":U + pcFileName) .

    RUN adeuib/_open-w.p 
        (pcFileName, '':U, 'OPEN':U).

    RUN ProcessCurrentFile .
    
    RUN choose_file_save IN _h_UIB.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcessListFile C-Win 
PROCEDURE ProcessListFile :
/*------------------------------------------------------------------------------
        Purpose:                                                                                                                                          
        Notes:                                                                                                                                            
------------------------------------------------------------------------------*/

    INPUT STREAM instream FROM VALUE (fiListFile) . 
    
    DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO .
    DEFINE VARIABLE i         AS INTEGER   NO-UNDO . 

    REPEAT:
        IMPORT STREAM instream UNFORMATTED cFileName .
        
        ASSIGN i = i + 1 .

        {&WINDOW-NAME}:TITLE = SUBSTITUTE ("Processing - &1 &2", i, cFileName) .
        PROCESS EVENTS . 

        FILE-INFO:FILE-NAME = cFileName . 
        
        IF FILE-INFO:FULL-PATHNAME > "" THEN DO:
            RUN ProcessFileName (FILE-INFO:FULL-PATHNAME) . 
        
            RUN choose_close IN _h_UIB.  
        END.
        
        PROCESS EVENTS.       
    END.

    INPUT STREAM instream CLOSE . 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RemoveIcon C-Win 
PROCEDURE RemoveIcon :
/*------------------------------------------------------------------------------
        Purpose:                                                                                                                                          
        Notes:                                                                                                                                            
------------------------------------------------------------------------------*/

  DEFINE VARIABLE hWindow AS HANDLE NO-UNDO.

  ASSIGN hWindow = b_U._HANDLE .

  IF AVAILABLE b_C THEN 
    ASSIGN b_C._ICON       = "":U
           b_C._SMALL-ICON = "":U
        . 

  IF hWindow:TYPE = Consultingwerk.WidgetTypeEnum:Window THEN DO:
      hWindow:LOAD-ICON ("":U) .
      hWindow:LOAD-SMALL-ICON ("":U) .
  END.
 
  ASSIGN lCurrentFileModified = TRUE . 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RestoreSettings C-Win 
PROCEDURE RestoreSettings :
/*------------------------------------------------------------------------------
        Purpose: Loads previous settings from the Windows Registry                                                                                                                                        
        Notes:                                                                                                                                            
------------------------------------------------------------------------------*/

    DEFINE VARIABLE cPosition AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iX AS INTEGER NO-UNDO.
    DEFINE VARIABLE iY AS INTEGER NO-UNDO.
    DEFINE VARIABLE iW AS INTEGER NO-UNDO.
    DEFINE VARIABLE iH AS INTEGER NO-UNDO.

    DEFINE VARIABLE iProcess AS INTEGER NO-UNDO.

    ASSIGN cPosition = Consultingwerk.Framework.Registry:GetRegistryValue 
                       ("User":U,
                        cBaseKey, 
                        "Position":U) .

    IF NUM-ENTRIES (cPosition) > 3 THEN DO:
        ASSIGN iX = INTEGER (ENTRY (1, cPosition))
               iY = INTEGER (ENTRY (2, cPosition))
               iW = INTEGER (ENTRY (3, cPosition))
               iH = INTEGER (ENTRY (4, cPosition))
            NO-ERROR .
            
        IF iX <> ? AND iY <> ? AND iW <> ? AND iH <> ? THEN DO: 
            ASSIGN {&window-name}:X = iX
                   {&window-name}:Y = iY .
            
            IF iW >= {&window-name}:MIN-WIDTH-PIXELS THEN 
                {&window-name}:WIDTH-PIXELS = iW .       

            IF iH >= {&window-name}:MIN-HEIGHT-PIXELS THEN 
                {&window-name}:HEIGHT-PIXELS = iH .       
        END.                      
    END.

    iProcess = INTEGER(Consultingwerk.Framework.Registry:GetRegistryValue
                       ("User":U, 
                        cBaseKey,
                        "Process":U)) . 
     
    IF iProcess >= 1 AND iProcess <= 3 THEN 
        ASSIGN rsProcess = iProcess .
        
    fiListFile = Consultingwerk.Framework.Registry:GetRegistryValue
                                          ("User":U, 
                                           cBaseKey,
                                           "FileName":U) . 

    toCheckMenu = Consultingwerk.Framework.Registry:GetRegistryValue
                                           ("User":U, 
                                            cBaseKey,
                                            "CheckMenu":U) = "yes":U . 

    toFunctions = Consultingwerk.Framework.Registry:GetRegistryValue
                                           ("User":U, 
                                            cBaseKey,
                                            "Functions":U) = "yes":U . 

    toProcedures = Consultingwerk.Framework.Registry:GetRegistryValue
                                            ("User":U, 
                                             cBaseKey,
                                             "Procedures":U) = "Yes":U. 

    toTrigger = Consultingwerk.Framework.Registry:GetRegistryValue
                                         ("User":U, 
                                          cBaseKey,
                                          "Trigger":U) = "yes":U . 


    toAddToLocalInitialize = Consultingwerk.Framework.Registry:GetRegistryValue
                                                      ("User":U, 
                                                       cBaseKey,
                                                       "AddToEmbedFinalize":U) = "yes":U NO-ERROR . 

    toCreateLocalInitialize = Consultingwerk.Framework.Registry:GetRegistryValue
                                                       ("User":U, 
                                                        cBaseKey,
                                                        "CreateEmbedFinalize":U) = "yes":U NO-ERROR. 

    toRemoveIcon = Consultingwerk.Framework.Registry:GetRegistryValue
                                            ("User":U, 
                                             cBaseKey,
                                             "RemoveIcon":U) = "yes":U NO-ERROR. 

    toAddMethodLibrary = Consultingwerk.Framework.Registry:GetRegistryValue
                                                  ("User":U, 
                                                   cBaseKey,
                                                   "AddMethodLibrary":U) = "yes":U NO-ERROR . 
                                                   
    toBuildFrameList = Consultingwerk.Framework.Registry:GetRegistryValue
                                                  ("User":U, 
                                                   cBaseKey,
                                                   "BuildFrameList":U) = "yes":U NO-ERROR . 

    DISPLAY rsProcess 
            fiListFile 
            toTrigger 
            toProcedures 
            toFunctions 
            toCheckMenu
            toAddToLocalInitialize
            toCreateLocalInitialize
            toRemoveIcon
            toAddMethodLibrary
            toBuildFrameList
            WITH FRAME {&frame-name} . 

    APPLY "VALUE-CHANGED":U TO toAddToLocalInitialize IN FRAME {&FRAME-NAME} . 

    CATCH err AS Progress.Lang.Error :
        /* Ignore errors while retrieving registry data */      
    END CATCH.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StoreSettings C-Win 
PROCEDURE StoreSettings :
/*------------------------------------------------------------------------------
        Purpose: Store current settings and window position to registry                                                                                                                                           
        Notes:                                                                                                                                            
------------------------------------------------------------------------------*/

    DEFINE VARIABLE cPosition AS CHARACTER NO-UNDO.

    {&window-name}:WINDOW-STATE = WINDOW-NORMAL .
    
    ASSIGN cPosition = SUBSTITUTE ("&1,&2,&3,&4":U, 
                                   {&window-name}:X,
                                   {&window-name}:Y,
                                   {&window-name}:WIDTH-PIXELS, 
                                   {&window-name}:HEIGHT-PIXELS) . 
    
     Consultingwerk.Framework.Registry:SetRegistryValue(
                              "User":U, 
                              cBaseKey,
                              "Position":U,
                              cPosition) .
    
     Consultingwerk.Framework.Registry:SetRegistryValue
                              ("User":U, 
                               cBaseKey,
                               "Process":U,
                               rsProcess:SCREEN-VALUE IN FRAME {&frame-name}) . 
        
     Consultingwerk.Framework.Registry:SetRegistryValue
                              ("User":U, 
                               cBaseKey,
                               "FileName":U,
                               fiListFile:SCREEN-VALUE IN FRAME {&frame-name}) . 

     Consultingwerk.Framework.Registry:SetRegistryValue
                              ("User":U, 
                               cBaseKey,
                               "CheckMenu":U,
                               toCheckMenu:SCREEN-VALUE IN FRAME {&frame-name}) . 

     Consultingwerk.Framework.Registry:SetRegistryValue
                              ("User":U, 
                               cBaseKey,
                               "Functions":U,
                               toFunctions:SCREEN-VALUE IN FRAME {&frame-name}) . 

     Consultingwerk.Framework.Registry:SetRegistryValue
                              ("User":U, 
                               cBaseKey,
                               "Procedures":U,
                               toProcedures:SCREEN-VALUE IN FRAME {&frame-name}) . 

     Consultingwerk.Framework.Registry:SetRegistryValue
                              ("User":U, 
                               cBaseKey,
                               "Trigger":U,
                               toTrigger:SCREEN-VALUE IN FRAME {&frame-name}) . 

     Consultingwerk.Framework.Registry:SetRegistryValue
                             ("User":U, 
                              cBaseKey,
                              "AddToEmbedFinalize":U,
                              toAddToLocalInitialize:SCREEN-VALUE IN FRAME {&frame-name}) . 

     Consultingwerk.Framework.Registry:SetRegistryValue
                             ("User":U, 
                              cBaseKey,
                              "CreateEmbedFinalize":U,
                              toCreateLocalInitialize:SCREEN-VALUE IN FRAME {&frame-name}) . 

     Consultingwerk.Framework.Registry:SetRegistryValue
                             ("User":U, 
                              cBaseKey,
                              "RemoveIcon":U,
                              toRemoveIcon:SCREEN-VALUE IN FRAME {&frame-name}) . 

     Consultingwerk.Framework.Registry:SetRegistryValue
                             ("User":U, 
                              cBaseKey,
                              "AddMethodLibrary":U,
                              toAddMethodLibrary:SCREEN-VALUE IN FRAME {&frame-name}) . 

     Consultingwerk.Framework.Registry:SetRegistryValue
                             ("User":U, 
                              cBaseKey,
                              "BuildFrameList":U,
                              toBuildFrameList:SCREEN-VALUE IN FRAME {&frame-name}) . 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StripEndStatement C-Win 
PROCEDURE StripEndStatement :
/*------------------------------------------------------------------------------
        Purpose:                                                                                                                                          
        Notes:                                                                                                                                            
------------------------------------------------------------------------------*/

    DEFINE INPUT-OUTPUT PARAMETER pcCode AS CHARACTER NO-UNDO.
    DEFINE OUTPUT       PARAMETER pcEnd  AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iPos             AS INTEGER NO-UNDO.
    
    ASSIGN iPos = R-INDEX (pcCode, "END":U) . 

    IF iPos > 0 THEN 
        ASSIGN pcEnd  = SUBSTRING (pcCode, iPos)  
               pcCode = SUBSTRING (pcCode, 1, iPos - 1)
               . 
               

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

