&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  Name: query-tester.w
  Desc: Analyzes queries and displays the result in an other window

  Author: M.C. Fiere (fiere1@zonnet.nl)
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Datadigger */
{ DataDigger.i }

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttTestQuery.

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE ttBuffer NO-UNDO
  FIELD hBuffer    AS HANDLE
  FIELD cDatabase  AS CHARACTER
  FIELD cTableName AS CHARACTER
  INDEX iPrimary cDatabase cTableName.

&SCOPED-DEFINE CleanUp DELETE OBJECT hQry NO-ERROR. ~~n~
                       RUN clean-temp-table IN THIS-PROCEDURE.

DEFINE VARIABLE lShowError     AS LOGICAL INITIAL TRUE NO-UNDO.
DEFINE VARIABLE lErrorDetected AS LOGICAL NO-UNDO.
DEFINE VARIABLE h-browser      AS HANDLE  NO-UNDO.
DEFINE VARIABLE h-ProgName     AS HANDLE  NO-UNDO.
DEFINE VARIABLE h-QueryName    AS HANDLE  NO-UNDO.
DEFINE VARIABLE h-SeqName      AS HANDLE  NO-UNDO.
DEFINE QUERY q1 FOR ttTestQuery SCROLLING.

DEFINE TEMP-TABLE ttVstTableInfo NO-UNDO
  FIELD cDatabase    AS CHARACTER
  FIELD cTableName   AS CHARACTER
  FIELD iTableRead   AS DECIMAL DECIMALS 0
  FIELD lDataFetched AS LOGICAL INITIAL FALSE
  INDEX cTableName IS PRIMARY UNIQUE cDataBase cTableName.

DEFINE TEMP-TABLE ttVstIndexInfo NO-UNDO
  FIELD cDatabase    AS CHARACTER
  FIELD cTableName   AS CHARACTER
  FIELD cIndexName   AS CHARACTER
  FIELD iIndexRead   AS DECIMAL DECIMALS 0
  FIELD lDataFetched AS LOGICAL INITIAL FALSE
  INDEX iPrim IS PRIMARY UNIQUE cDataBase cTableName cIndexName.


/* window resize definition code */

/* The following temp-table is needed for window resizing to store the
   calculated position of each widget. This is necessary because the
   smallest positioning unit is of course a pixel so with every
   resize operation rounding errors occur. A couple of times
   repeating maximize/restore would already render the frame
   useless if we would not correct for these rounding errors.
   Therefore the recalculated position of a widget is stored in
   this temp-table so that subsequent resize operations can be
   based on more exact co-ordinates */

DEFINE TEMP-TABLE temp-widget NO-UNDO
  FIELD whand AS WIDGET-HANDLE
  FIELD hx    AS DECIMAL DECIMALS 10     /* calculated x-position in pixels */
  FIELD hy    AS DECIMAL DECIMALS 10     /* calculated y-position in pixels */
  FIELD hwidt AS DECIMAL DECIMALS 10  /* calculated width in pixels */
  FIELD hheig AS DECIMAL DECIMALS 10  /* calculated height in pixels */
  INDEX whand IS PRIMARY UNIQUE whand.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 btnClearQuery ed-qry btnTestQuery ~
btnRunQuery resultset btnPopOut 
&Scoped-Define DISPLAYED-OBJECTS ed-qry resultset 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClearQuery 
     LABEL "&Clear" 
     SIZE-PIXELS 60 BY 21.

DEFINE BUTTON btnPopOut 
     LABEL "&Pop out" 
     SIZE-PIXELS 60 BY 21 TOOLTIP "Show text in separate window".

DEFINE BUTTON btnRunQuery 
     LABEL "&Run" 
     SIZE-PIXELS 60 BY 21 TOOLTIP "Run the query".

DEFINE BUTTON btnTestQuery 
     LABEL "&Test" 
     SIZE-PIXELS 60 BY 21 TOOLTIP "Test the query".

DEFINE VARIABLE ed-qry AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 4000 SCROLLBAR-VERTICAL LARGE
     SIZE-PIXELS 500 BY 150 NO-UNDO.

DEFINE VARIABLE resultset AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE-PIXELS 500 BY 150 TOOLTIP "result previous analyze" NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 100 BY 5.95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnClearQuery AT Y 5 X 510
     ed-qry AT Y 135 X 5 NO-LABEL
     btnTestQuery AT Y 135 X 510
     btnRunQuery AT Y 165 X 510
     resultset AT Y 290 X 5 NO-LABEL
     btnPopOut AT Y 290 X 510 WIDGET-ID 2
     RECT-1 AT ROW 1.24 COL 2 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT X 0 Y 0
         SIZE-PIXELS 580 BY 453.


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
         TITLE              = "MCF's Query Tester"
         HEIGHT-P           = 455
         WIDTH-P            = 580
         MAX-HEIGHT-P       = 817
         MAX-WIDTH-P        = 1152
         VIRTUAL-HEIGHT-P   = 817
         VIRTUAL-WIDTH-P    = 1152
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       ed-qry:RETURN-INSERTED IN FRAME DEFAULT-FRAME  = TRUE.

ASSIGN 
       resultset:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* MCF's Query Tester */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* MCF's Query Tester */
DO:
  /* This event will close the window and terminate the procedure.  */
  PUBLISH "killquerywindow":U.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* MCF's Query Tester */
DO:
  RUN resizeFrame IN this-procedure (INPUT FRAME {&FRAME-NAME}:handle).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearQuery
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearQuery C-Win
ON CHOOSE OF btnClearQuery IN FRAME DEFAULT-FRAME /* Clear */
DO:
  FOR EACH ttTestQuery:
    DELETE ttTestQuery.
  END.

  CLOSE QUERY q1.
  OPEN QUERY q1 FOR EACH ttTestQuery.
  ASSIGN 
    ed-qry:SCREEN-VALUE = ""
    resultset:SCREEN-VALUE = "".

  RUN enableButtons IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPopOut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPopOut C-Win
ON CHOOSE OF btnPopOut IN FRAME DEFAULT-FRAME /* Pop out */
DO:
  SESSION:SET-WAIT-STATE("GENERAL":U).
  RUN value(REPLACE(THIS-PROCEDURE:FILE-NAME,"query-tester","query-data")) PERSISTENT
    (INPUT ed-qry,
     INPUT resultset:SCREEN-VALUE).
  SESSION:SET-WAIT-STATE("":U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRunQuery
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRunQuery C-Win
ON CHOOSE OF btnRunQuery IN FRAME DEFAULT-FRAME /* Run */
DO:
  SESSION:SET-WAIT-STATE("GENERAL":U).
  RUN test-query IN THIS-PROCEDURE (INPUT TRUE, INPUT TRUE ,OUTPUT lErrorDetected).
  SESSION:SET-WAIT-STATE("":U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTestQuery
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTestQuery C-Win
ON CHOOSE OF btnTestQuery IN FRAME DEFAULT-FRAME /* Test */
DO:
  SESSION:SET-WAIT-STATE("GENERAL":U).
  RUN test-query IN THIS-PROCEDURE (INPUT FALSE, INPUT TRUE ,OUTPUT lErrorDetected).
  SESSION:SET-WAIT-STATE("":U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME}
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}
       {&WINDOW-NAME}:VIRTUAL-WIDTH-PIXELS = SESSION:WORK-AREA-WIDTH-PIXELS
       {&WINDOW-NAME}:MAX-WIDTH = {&WINDOW-NAME}:VIRTUAL-WIDTH
       {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS = SESSION:WORK-AREA-HEIGHT-PIXELS
       {&WINDOW-NAME}:MAX-HEIGHT = {&WINDOW-NAME}:VIRTUAL-HEIGHT
       {&WINDOW-NAME}:MIN-HEIGHT-PIXELS = 300
       {&WINDOW-NAME}:MIN-WIDTH-PIXELS = 400.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

SUBSCRIBE TO "query" ANYWHERE RUN-PROCEDURE "processQuery".
SUBSCRIBE TO "Melding" ANYWHERE RUN-PROCEDURE "processMessage".
SUBSCRIBE TO "Message" ANYWHERE RUN-PROCEDURE "processMessage".
SUBSCRIBE TO "getScreenMessage" ANYWHERE RUN-PROCEDURE "processMessage".

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  /* Datadigger */
  c-win:FONT = getFont("Default").
  FRAME {&frame-name}:font = getFont("Default").
  ed-qry:font = getFont("Fixed").

  RUN enable_UI.

  DEF VARIABLE lhFrameHdl AS HANDLE NO-UNDO.
  lhFrameHdl = FRAME {&FRAME-NAME}:HANDLE.
  OPEN QUERY q1 FOR EACH ttTestQuery NO-LOCK.

  /* query browser */
  CREATE BROWSE h-browser
    ASSIGN FRAME            = lhFrameHdl
           QUERY            = QUERY q1:HANDLE
           Y                = RECT-1:x
           X                = RECT-1:y
           WIDTH-PIXELS     = RECT-1:width-pixels
           HEIGHT-PIXELS    = RECT-1:height-pixels
           SEPARATORS       = TRUE
           ROW-MARKERS      = FALSE
           EXPANDABLE       = TRUE
           COLUMN-RESIZABLE = TRUE
           COLUMN-MOVABLE   = FALSE
           VISIBLE          = FALSE
           READ-ONLY        = TRUE
    TRIGGERS:
      ON "value-changed":U ANYWHERE DO:
        ASSIGN ed-qry:SCREEN-VALUE IN FRAME {&FRAME-NAME} = REPLACE(ttTestQuery.cQueryTxt,",",",~n").
        RUN test-query IN THIS-PROCEDURE (INPUT FALSE,INPUT FALSE, OUTPUT lErrorDetected).
      END.

      ON "row-display":U ANYWHERE DO:
        IF VALID-HANDLE(h-SeqName)   THEN h-SeqName:SCREEN-VALUE   = STRING(ttTestQuery.iId).
        IF VALID-HANDLE(h-ProgName)  THEN h-ProgName:SCREEN-VALUE  = STRING(ttTestQuery.cProgName).
        IF VALID-HANDLE(h-QueryName) THEN h-QueryName:SCREEN-VALUE = STRING(ttTestQuery.cQueryTxt).
      END.
    END TRIGGERS.

    RECT-1:visible = TRUE.
    h-SeqName   = h-Browser:ADD-CALC-COLUMN("INTEGER",">,>>9","","Seq").
    h-ProgName  = h-Browser:ADD-CALC-COLUMN("CHARACTER","x(30)","","Table").
    h-QueryName = h-Browser:ADD-CALC-COLUMN("CHARACTER","x(105)","","Query").

  ASSIGN
    h-browser:LABELS = TRUE
    h-browser:SENSITIVE = TRUE
    h-browser:VISIBLE = TRUE.

  RUN enableButtons IN THIS-PROCEDURE.
  RUN resizeFrame IN this-procedure (INPUT FRAME {&FRAME-NAME}:handle).

  IF TEMP-TABLE ttTestQuery:HAS-RECORDS THEN
  DO:
    APPLY "value-changed" TO h-browser.
    h-browser:SELECT-FOCUSED-ROW().
  END.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE FOCUS h-browser.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ask-table-from-user C-Win 
PROCEDURE ask-table-from-user PRIVATE :
/* Let user select a table
  */
  DEFINE INPUT PARAMETER ipc-current-name AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opc-TableName AS CHARACTER NO-UNDO.

  DEFINE VARIABLE lcDataBase AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lOkUsed AS LOGICAL NO-UNDO.

  MESSAGE
    "Unable to determine which table in which database is meant with" ipc-current-name
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

  ASSIGN lcDataBase = ""
         opc-TableName = ""
         .

  RUN adecomm\_tblsel.r (INPUT FALSE, /* one and only one to be selected */
                         INPUT ?,    /* no temp-tables to be passed */
                         INPUT-OUTPUT lcDataBase, /* all database are to be used */
                         INPUT-OUTPUT opc-TableName,
                         OUTPUT lOkUsed).

  IF lOkUsed THEN ASSIGN opc-TableName = SUBSTITUTE("&1.&2", lcDataBase, opc-TableName).

END PROCEDURE. /* ask-table-from-user */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clean-temp-table C-Win 
PROCEDURE clean-temp-table PRIVATE :
/* Clean up internal temp-tables
  */
  DEFINE BUFFER bf-ttBuffer FOR ttBuffer.
  DEFINE BUFFER bf-ttVstTableInfo FOR ttVstTableInfo.
  DEFINE BUFFER bf-ttVstIndexInfo FOR ttVstIndexInfo.

  FOR EACH bf-ttBuffer:
    DELETE OBJECT bf-ttBuffer.hBuffer NO-ERROR.
    DELETE bf-ttBuffer.
  END.

  FOR EACH bf-ttVstTableInfo:
    DELETE bf-ttVstTableInfo.
  END.

  FOR EACH bf-ttVstIndexInfo:
    DELETE bf-ttVstIndexInfo.
  END.

  RUN enableButtons IN THIS-PROCEDURE.
END PROCEDURE. /* clean-temp-table */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableButtons C-Win 
PROCEDURE enableButtons :
/* Set the sensitivity of the buttons
  */
  DEFINE VARIABLE hTt AS HANDLE NO-UNDO.

  ASSIGN hTt = TEMP-TABLE ttTestQuery:HANDLE.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN btnClearQuery:SENSITIVE = hTt:HAS-RECORDS.
  END.

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
  DISPLAY ed-qry resultset 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 btnClearQuery ed-qry btnTestQuery btnRunQuery resultset 
         btnPopOut 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE processMessage C-Win 
PROCEDURE processMessage :
/* Parse FOR EACH expressions out of debug messages
  */
  DEFINE INPUT PARAMETER ipiLevel AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipcQueryString AS CHARACTER NO-UNDO.  /* will mostly contain not valid queries (running procecure etc...) */

  IF ipiLevel < 70 /* above it  is meaningless */
    AND INDEX(ipcQueryString,"FOR EACH":U) > 0 THEN
    RUN processQuery IN THIS-PROCEDURE (INPUT ipcQueryString).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE processQuery C-Win 
PROCEDURE processQuery :
/* Analyze the query
  */
  DEFINE INPUT PARAMETER ipcQueryString AS CHARACTER NO-UNDO.

  DEFINE VARIABLE lcOldString AS CHARACTER NO-UNDO.
  DEFINE BUFFER bf-ttTestQuery FOR ttTestQuery.

  /* <BEU> */
  /* FORWARD-ONLY attribute:                                                             */
  /* Lets you avoid building result-lists for static and dynamic queries. Set to TRUE to */
  /* avoid building result-lists for queries. Set to FALSE to build result-lists for     */
  /* queries. The default is FALSE. When TRUE, you cannot use the GET PREV, GET LAST,    */
  /* REPOSITION, or BROWSE methods or statements with these queries. If you do, the AVM  */
  /* generates an error.                                                                 */
/*  ipcQueryString = REPLACE(ipcQueryString,"INDEXED-REPOSITION","").*/
  /* </BEU> */

  DO WITH FRAME {&FRAME-NAME}:

    ASSIGN
      lShowError = FALSE
      lcOldString = ed-qry:SCREEN-VALUE
      ed-qry:SCREEN-VALUE = REPLACE(SUBSTRING(ipcQueryString,INDEX(ipcQueryString,"FOR EACH":U)),",",",~n").

    RUN test-query IN THIS-PROCEDURE (INPUT FALSE,
                                      INPUT FALSE,
                                      OUTPUT lErrorDetected).
    ASSIGN lShowError = TRUE.

    IF NOT lErrorDetected THEN
    DO:
      FIND FIRST bf-ttTestQuery NO-ERROR.

      CLOSE QUERY q1.
      OPEN QUERY q1 FOR EACH ttTestQuery NO-LOCK.

      IF AVAILABLE bf-ttTestQuery THEN
        REPOSITION q1 TO ROWID ROWID(bf-ttTestQuery) NO-ERROR.
    END.
    ELSE
      ASSIGN ed-qry:SCREEN-VALUE = lcOldString.
  END.

  RUN enableButtons.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resizeFrame C-Win 
PROCEDURE resizeFrame :
/* Resize the frame and all the widget it contains to the new window size
  */
  DEFINE INPUT PARAMETER wfram# AS WIDGET-HANDLE NO-UNDO.

  DEFINE VARIABLE whand# AS WIDGET-HANDLE NO-UNDO. /* general purpose widget handle */
  DEFINE VARIABLE afacthori# AS DECIMAL DECIMALS 10 NO-UNDO.
  DEFINE VARIABLE afactvert# AS DECIMAL DECIMALS 10 NO-UNDO.

  ASSIGN wfram#:SCROLLABLE = TRUE
         afacthori# = {&WINDOW-NAME}:WIDTH-PIXELS / wfram#:WIDTH-PIXELS
         afactvert# = {&WINDOW-NAME}:HEIGHT-PIXELS / wfram#:HEIGHT-PIXELS.

  /* prevent multiple calls of this procedure on window-maximized event */

  IF afacthori# = 1 AND afactvert# = 1 THEN
    RETURN.

  IF afacthori# > 1 THEN
    ASSIGN wfram#:WIDTH-PIXELS = {&window-name}:WIDTH-PIXELS.

  IF afactvert# > 1 THEN
    ASSIGN wfram#:HEIGHT-PIXELS = {&window-name}:HEIGHT-PIXELS.

  ASSIGN whand# = wfram#:FIRST-CHILD  /* first field group */
         whand# = whand#:FIRST-CHILD. /* first field-level widget */

  DO WHILE VALID-HANDLE(whand#):

    /* find the last calculated positions */

    FIND temp-widget WHERE temp-widget.whand = whand# NO-ERROR.

    IF NOT AVAILABLE temp-widget THEN DO:
      CREATE temp-widget.
      ASSIGN temp-widget.whand = whand#
             temp-widget.hx    = whand#:X
             temp-widget.hy    = whand#:Y
             temp-widget.hwidt = whand#:WIDTH-PIXELS
             temp-widget.hheig = whand#:HEIGHT-PIXELS.
    END.

    ASSIGN temp-widget.hwidt = temp-widget.hwidt * afacthori#
           temp-widget.hx    = temp-widget.hx * afacthori#
           temp-widget.hy    = temp-widget.hy * afactvert#.

    IF LOOKUP(whand#:TYPE,"fill-in,text,literal,button") = 0 THEN
      ASSIGN temp-widget.hheig = temp-widget.hheig * afactvert#.

    ASSIGN whand#:X = temp-widget.hx
           whand#:Y = temp-widget.hy
           whand#:WIDTH-PIXELS  = temp-widget.hwidt
           whand#:HEIGHT-PIXELS = temp-widget.hheig.

    ASSIGN whand# = whand#:NEXT-SIBLING.

  END.

  IF afacthori# < 1 THEN
    ASSIGN wfram#:WIDTH-PIXELS = {&window-name}:WIDTH-PIXELS
           wfram#:VIRTUAL-WIDTH-PIXELS = wfram#:WIDTH-PIXELS.

  IF afactvert# < 1 THEN
    ASSIGN wfram#:HEIGHT-PIXELS = {&window-name}:HEIGHT-PIXELS
           wfram#:VIRTUAL-HEIGHT-PIXELS = wfram#:HEIGHT-PIXELS.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE scanVST C-Win 
PROCEDURE scanVST PRIVATE :
/* Scan the VST tables
  */
  DEFINE INPUT PARAMETER iplInitialData AS LOGICAL NO-UNDO. /* get the initial data or get the number of reads from the query */

  DEFINE BUFFER bf-ttBuffer FOR ttBuffer.
  DEFINE BUFFER bf-ttVstTableInfo FOR ttVstTableInfo.
  DEFINE BUFFER bf-ttVstIndexInfo FOR ttVstIndexInfo.

  DEFINE VARIABLE hQry AS HANDLE NO-UNDO.
  DEFINE VARIABLE hBufferVstTable AS HANDLE NO-UNDO.
  DEFINE VARIABLE hBufferVstIndex AS HANDLE NO-UNDO.
  DEFINE VARIABLE hBuffer_index AS HANDLE NO-UNDO.
  DEFINE VARIABLE hBuffer_file AS HANDLE NO-UNDO.

  DEFINE VARIABLE hFieldVstIndexName AS HANDLE NO-UNDO.
  DEFINE VARIABLE hFieldVstTableRead AS HANDLE NO-UNDO.
  DEFINE VARIABLE hFieldVstIndexRead AS HANDLE NO-UNDO.

  FOR EACH bf-ttBuffer NO-LOCK:

    CREATE BUFFER hBufferVstTable FOR TABLE SUBSTITUTE("&1._tablestat",bf-ttBuffer.hBuffer:DBNAME). /* this is the information on a table */
    CREATE QUERY hQry.

    hQry:SET-BUFFERS(hBufferVstTable).
    hQry:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1.&2 WHERE &1.&2._tablestat-id EQ &3",hBufferVstTable:DBNAME,hBufferVstTable:TABLE,bf-ttBuffer.hBuffer:TABLE-NUMBER)).
    hQry:QUERY-OPEN().
    hQry:GET-FIRST().

    IF NOT hQry:QUERY-OFF-END THEN
    DO:
      hFieldVstTableRead = hBufferVstTable:BUFFER-FIELD("_tablestat-read":U). /* only interested in reads */

      FIND bf-ttVstTableInfo
        WHERE bf-ttVstTableInfo.cDatabase EQ bf-ttBuffer.hBuffer:DBNAME
        AND bf-ttVstTableInfo.cTableName EQ bf-ttBuffer.hBuffer:TABLE
            NO-ERROR.

      IF iplInitialData THEN
      DO:
        IF NOT AVAILABLE bf-ttVstTableInfo THEN
        DO:
          CREATE bf-ttVstTableInfo.
          ASSIGN
            bf-ttVstTableInfo.cDatabase = bf-ttBuffer.hBuffer:DBNAME
            bf-ttVstTableInfo.cTableName = bf-ttBuffer.hBuffer:TABLE
            bf-ttVstTableInfo.iTableRead = hFieldVstTableRead:BUFFER-VALUE.
        END.
      END.
      ELSE IF AVAILABLE bf-ttVstTableInfo AND bf-ttVstTableInfo.lDataFetched = FALSE THEN
      DO:
        ASSIGN
          bf-ttVstTableInfo.lDataFetched = TRUE
          bf-ttVstTableInfo.iTableRead = hFieldVstTableRead:BUFFER-VALUE - bf-ttVstTableInfo.iTableRead.
      END.
    END.

    hQry:QUERY-CLOSE().
    DELETE OBJECT hQry NO-ERROR.

    CREATE QUERY hQry.

    /* index data is not yet finished */
    CREATE BUFFER hBufferVstIndex FOR TABLE SUBSTITUTE("&1._indexstat",bf-ttBuffer.hBuffer:DBNAME). /* this is the information on a index */
    CREATE BUFFER hBuffer_index FOR TABLE SUBSTITUTE("&1._index",bf-ttBuffer.hBuffer:DBNAME).       /* this is the _index table */
    CREATE BUFFER hBuffer_file FOR TABLE SUBSTITUTE("&1._file",bf-ttBuffer.hBuffer:DBNAME).         /* this is the _file table */

    hQry:SET-BUFFERS(hBuffer_file,
                     hBuffer_index,
                     hBufferVstIndex).

    hQry:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1.&2 WHERE &1.&2._file-number EQ &3 NO-LOCK, EACH &1.&4 OF &1.&2 NO-LOCK, EACH &1.&5 WHERE &1.&5._indexstat-id EQ &1.&4._idx-num":U,
                                  bf-ttBuffer.hBuffer:DBNAME,
                                  hBuffer_file:NAME,
                                  bf-ttBuffer.hBuffer:TABLE-NUMBER,
                                  hBuffer_index:NAME,
                                  hBufferVstIndex:NAME)).

    ASSIGN hFieldVstIndexName = hBuffer_index:BUFFER-FIELD("_index-name":U)
           hFieldVstIndexRead = hBufferVstIndex:BUFFER-FIELD("_indexstat-read":U).

    hQry:QUERY-OPEN().
    hQry:GET-FIRST(NO-LOCK).
    REPEAT WHILE NOT hQry:QUERY-OFF-END:
      FIND bf-ttVstIndexInfo
        WHERE bf-ttVstIndexInfo.cDatabase EQ bf-ttBuffer.hBuffer:DBNAME
        AND bf-ttVstIndexInfo.cTableName EQ bf-ttBuffer.hBuffer:TABLE
        AND bf-ttVstIndexInfo.cIndexName EQ hFieldVstIndexName:BUFFER-VALUE
            NO-ERROR.

      IF iplInitialData THEN
      DO:
        IF NOT AVAILABLE bf-ttVstIndexInfo THEN
        DO:
          CREATE bf-ttVstIndexInfo.
          ASSIGN
            bf-ttVstIndexInfo.cDatabase = bf-ttBuffer.hBuffer:DBNAME
            bf-ttVstIndexInfo.cTableName = bf-ttBuffer.hBuffer:TABLE
            bf-ttVstIndexInfo.cIndexName = hFieldVstIndexName:BUFFER-VALUE
            bf-ttVstIndexInfo.iIndexRead = hFieldVstIndexRead:BUFFER-VALUE.
        END.
      END.
      ELSE IF AVAILABLE bf-ttVstIndexInfo  AND bf-ttVstIndexInfo.lDataFetched = FALSE THEN
      DO:
        ASSIGN
          bf-ttVstIndexInfo.lDataFetched = TRUE
          bf-ttVstIndexInfo.iIndexRead = hFieldVstIndexRead:BUFFER-VALUE - bf-ttVstIndexInfo.iIndexRead.
      END.

      hQry:GET-NEXT(NO-LOCK).
    END.

    DELETE OBJECT hQry NO-ERROR.

    DELETE OBJECT hBufferVstTable NO-ERROR.
    DELETE OBJECT hBufferVstIndex NO-ERROR.
    DELETE OBJECT hBuffer_index NO-ERROR.
    DELETE OBJECT hBuffer_file NO-ERROR.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE test-query C-Win 
PROCEDURE test-query PRIVATE :
/* test the query
  */
  DEFINE INPUT  PARAMETER iplPerfromQuery AS LOGICAL NO-UNDO.
  DEFINE INPUT  PARAMETER iplShowQuery    AS LOGICAL NO-UNDO.
  DEFINE OUTPUT PARAMETER oplErrorOccured AS LOGICAL INITIAL TRUE NO-UNDO.

  DEFINE BUFFER bf-ttVstTableInfo FOR ttVstTableInfo.
  DEFINE BUFFER bf-ttVstIndexInfo FOR ttVstIndexInfo.
  DEFINE BUFFER bf-ttBuffer       FOR ttBuffer.

  DEFINE VARIABLE hQry          AS HANDLE      NO-UNDO.
  DEFINE VARIABLE lc-old-string AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE lcBufferName  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE lcCurrentName AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE lcPrevName    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE lhBuffer      AS HANDLE      NO-UNDO.
  DEFINE VARIABLE liNumWords    AS INTEGER     NO-UNDO.
  DEFINE VARIABLE liSeconds     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE liWord        AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lOk           AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE liNumResults  AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lStop         AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE liDelayStart  AS INTEGER     NO-UNDO.
  DEFINE VARIABLE liDelayTime   AS INTEGER     NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

    IF TRIM(ed-qry:SCREEN-VALUE) = "" THEN RETURN.

    SESSION:SET-WAIT-STATE("general").

    ASSIGN
      ed-qry = REPLACE(ed-qry:SCREEN-VALUE,CHR(10)," ")
      ed-qry = REPLACE(ed-qry,CHR(13)," ")
      ed-qry = REPLACE(ed-qry,","," ")
      ed-qry = REPLACE(ed-qry,"exclusive-lock":U,"no-lock":U)
      ed-qry = REPLACE(ed-qry,"share-lock":U,"no-lock":U) NO-ERROR.

    DO WHILE ed-qry NE lc-old-string:
      ASSIGN
        lc-old-string = ed-qry
        ed-qry        = REPLACE(ed-qry,"  "," ").
    END.

    /* determine the buffers used by this query */
    /* it's assummed we don't use any duplicate tables in multiple databases */
    ASSIGN
      liNumWords    = NUM-ENTRIES(ed-qry," ")
      lcCurrentName = "".

    CREATE QUERY hQry.
    
    /* If user specifies INDEXED-REPOSITION in 
     * the query, we cannot use FORWARD-ONLY. 
     */
    hQry:FORWARD-ONLY = ( LOOKUP('INDEXED-REPOSITION', ed-qry,' ') = 0 ).
    ed-qry = REPLACE(ed-qry,'INDEXED-REPOSITION','').
    
    DO FOR bf-ttBuffer liWord = 1 TO liNumWords:
      ASSIGN
        lcPrevName    = lcCurrentName
        lcCurrentName = TRIM(ENTRY(liWord,ed-qry," "))
        lcBufferName  = "".

      IF CAN-DO("EACH,LAST,FIRST",lcPrevName) THEN
      DO:
        CREATE bf-ttBuffer.
        ASSIGN bf-ttBuffer.cTableName = lcCurrentName.
        CREATE BUFFER bf-ttBuffer.hBuffer FOR TABLE lcCurrentName NO-ERROR.

        /* using a buffer ? */
        IF NOT VALID-HANDLE(bf-ttBuffer.hBuffer) THEN
        DO:
          IF   lcCurrentName BEGINS "bf-"
            OR lcCurrentName BEGINS "buf" THEN
            ASSIGN lcBufferName = TRIM(SUBSTRING(lcCurrentName,4),"-").

          ELSE
          IF lcCurrentName BEGINS "b":U
            AND lcCurrentName NE "b":U THEN
            ASSIGN lcBufferName = TRIM(SUBSTRING(lcCurrentName,2),"-").

          CREATE BUFFER bf-ttBuffer.hBuffer FOR TABLE lcBufferName BUFFER-NAME lcCurrentName NO-ERROR.
        END.

        /* if it is still a not valid table ask the user which table he means */
        IF NOT VALID-HANDLE(bf-ttBuffer.hBuffer)
          AND KEYWORD-ALL(lcCurrentName) EQ ? THEN
        DO:
          ASSIGN lcBufferName = "".
          SESSION:SET-WAIT-STATE("").
          RUN ask-table-from-user (INPUT lcCurrentName, OUTPUT lcBufferName).
          SESSION:SET-WAIT-STATE("general").
          CREATE BUFFER bf-ttBuffer.hBuffer FOR TABLE lcBufferName BUFFER-NAME lcCurrentName NO-ERROR.
        END.

        IF NOT VALID-HANDLE(bf-ttBuffer.hBuffer) THEN
        DO:
          DELETE bf-ttBuffer. /* it's invalid so no need to bother deleting the object */
          {&CleanUp}
          SESSION:SET-WAIT-STATE("").
          RETURN.
        END.

        hQry:ADD-BUFFER(bf-ttBuffer.hBuffer).
      END.
    END.

    ASSIGN
      ed-qry = REPLACE(ed-qry:SCREEN-VALUE,CHR(10)," ")
      ed-qry = REPLACE(ed-qry,CHR(13)," ").

    ASSIGN
      resultset:SCREEN-VALUE = "Preparing Query".

    ASSIGN lOk = hQry:QUERY-PREPARE(ed-qry) NO-ERROR.
    IF NOT lOk OR ERROR-STATUS:ERROR THEN
    DO:
      SESSION:SET-WAIT-STATE("").
      resultset:SCREEN-VALUE = SUBSTITUTE("Unable to prepare the query ~n")
                             + SUBSTITUTE("Query string : &1 ~n", ed-qry )
                             + SUBSTITUTE("Error status : &1 ~n", ERROR-STATUS:ERROR )
                             + SUBSTITUTE("Error message: &1 ~n", ERROR-STATUS:GET-MESSAGE(1) )
                             .
      {&CleanUp}
      ASSIGN hQry = ?.
      RETURN.
    END.

    ASSIGN
      liNumWords = hQry:NUM-BUFFERS
      .

    IF iplPerfromQuery THEN
    DO:
      ASSIGN
        resultset:SCREEN-VALUE = "Opening Query".

      RUN scanVST IN THIS-PROCEDURE (TRUE). /* what are the current values in the VST's */

      ASSIGN lOk = hQry:QUERY-OPEN() NO-ERROR.
      IF NOT lOk OR ERROR-STATUS:ERROR THEN
      DO:
        SESSION:SET-WAIT-STATE("").
        resultset:SCREEN-VALUE = SUBSTITUTE("Unable to open the query ~n")
                               + SUBSTITUTE("Query string : &1 ~n", ed-qry )
                               + SUBSTITUTE("Error status : &1 ~n", ERROR-STATUS:ERROR )
                               + SUBSTITUTE("Error message: &1 ~n", ERROR-STATUS:GET-MESSAGE(1) )
                               .
        {&CleanUp}
        ASSIGN hQry = ?.
        RETURN.
      END.

      ASSIGN
        resultset:SCREEN-VALUE = "Performing Query".

      ETIME(TRUE).
      hQry:GET-FIRST.
      liNumResults = 0.
      lStop = ?.

      #QueryLoop:
      DO WHILE NOT hQry:QUERY-OFF-END:
        liNumResults = liNumResults + 1.
        hQry:GET-NEXT.

        IF ETIME > 5000 AND lStop = ? THEN
        DO:
          liDelayStart = ETIME.
          MESSAGE 'This is taking quite some time, do you want to stop the query?' VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE lStop.
          IF lStop THEN LEAVE #QueryLoop.
          liDelayTime = ETIME - liDelayStart.
        END.
      END.

      ASSIGN
        liSeconds = ETIME(FALSE) - liDelayTime.

      RUN scanVST IN THIS-PROCEDURE (FALSE). /* the data coming from this query, assuming there were no other activities on the table */
    END.

    ASSIGN
      resultset:SCREEN-VALUE = SUBSTITUTE("Test finished at &1 on &2~n~n&3~n~n":U,TODAY,STRING(TIME,"hh:mm:ss":U),ed-qry:SCREEN-VALUE).

    DO liWord = 1 TO liNumWords:
      ASSIGN lhBuffer = hQry:GET-BUFFER-HANDLE(liWord)
        resultset:SCREEN-VALUE = resultset:SCREEN-VALUE +
                                 SUBSTITUTE("Buffer &1&2 uses index&3 &4.~n"
                                           , CAPS(lhBuffer:NAME)
                                           , (IF lhBuffer:NAME <> lhBuffer:TABLE 
                                                THEN SUBSTITUTE(' (table name &1)', CAPS(lhBuffer:TABLE)) ELSE '')
                                           , (IF NUM-ENTRIES(hQry:INDEX-INFORMATION) > 1 THEN "es" ELSE "")
                                           , hQry:INDEX-INFORMATION(liWord)
                                           )
        NO-ERROR. 
    END.

    IF iplPerfromQuery THEN
    DO:
      ASSIGN
        resultset:SCREEN-VALUE = resultset:SCREEN-VALUE + SUBSTITUTE("~nNumber of query results is &1 in &2 seconds.~n"
                                                                    , liNumResults
                                                                    , TRIM(STRING(liSeconds / 1000,">>,>>9.99")))
      NO-ERROR.

      DO liWord = 1 TO liNumWords:
        ASSIGN lhBuffer = hQry:GET-BUFFER-HANDLE(liWord).

        FOR EACH bf-ttVstTableInfo
          WHERE bf-ttVstTableInfo.cDatabase EQ lhBuffer:DBNAME
            AND bf-ttVstTableInfo.cTableName EQ lhBuffer:TABLE:

          ASSIGN
            resultset:SCREEN-VALUE = resultset:SCREEN-VALUE
                                   + SUBSTITUTE("~ntable &1 has &2 reads~n"
                                               , SUBSTITUTE("&1.&2",bf-ttVstTableInfo.cDatabase,bf-ttVstTableInfo.cTableName)
                                               , bf-ttVstTableInfo.iTableRead
                                               ) NO-ERROR.

          {&_proparse_ prolint-nowarn(oflink)}
          FOR EACH bf-ttVstIndexInfo OF bf-ttVstTableInfo:
            ASSIGN resultset:SCREEN-VALUE = resultset:SCREEN-VALUE + SUBSTITUTE("-  index &1 has &2 reads~n",bf-ttVstIndexInfo.cIndexName,bf-ttVstIndexInfo.iIndexRead) NO-ERROR.
            DELETE bf-ttVstIndexInfo.
          END.

          DELETE bf-ttVstTableInfo.
        END.
      END.

      hQry:QUERY-CLOSE.
    END.

    {&CleanUp}
    SESSION:SET-WAIT-STATE("").
    ASSIGN oplErrorOccured = FALSE.

    /* <BEU> */
/*     IF iplShowQuery THEN                                                                  */
/*       RUN VALUE(REPLACE(THIS-PROCEDURE:FILE-NAME,"query-tester","query-data")) PERSISTENT */
/*         ( INPUT ed-qry                                                                    */
/*         , INPUT resultset:SCREEN-VALUE                                                    */
/*         ).                                                                                */
    /* </BEU> */

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

