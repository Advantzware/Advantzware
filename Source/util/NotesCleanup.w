&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
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
{methods/defines/hndldefs.i}
{custom/globdefs.i}
{sys/inc/var.i "new shared"}

DEF STREAM outFile.
DEF STREAM dumpFile.

DEF VAR cMessage AS CHAR NO-UNDO.
DEF VAR daTargetDate AS DATE NO-UNDO.
DEF VAR hPurge AS HANDLE NO-UNDO.
DEF VAR iCtr AS INT NO-UNDO.
DEF VAR jCtr AS INT NO-UNDO.
DEF VAR iStartYear AS INT NO-UNDO.
DEF VAR iEndYear AS INT NO-UNDO.
DEF VAR iEndPeriod AS INT NO-UNDO.
DEF VAR lError AS LOG NO-UNDO.
DEF VAR lTested AS LOG NO-UNDO.
DEF VAR lReviewed AS LOG NO-UNDO.
DEF VAR lPurged AS LOG NO-UNDO.
DEF VAR lWarned AS LOG NO-UNDO.
DEF VAR iNoteCount AS INT NO-UNDO.
DEFINE VARIABLE lReturnError AS LOGICAL NO-UNDO.
DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO .
{src/adm2/widgetprto.i}
{util/ttPurge.i NEW}

ASSIGN
    cocode = g_company
    locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bExit RECT-5 bProcess fiSearchText tbTitles ~
tbText rsAction fiReplaceWith fiOutput rsProcess rsMode 
&Scoped-Define DISPLAYED-OBJECTS fiSearchText tbTitles tbText rsAction ~
fiReplaceWith fiOutput fiNoteCount rsProcess rsMode tText2 tText3 tAction ~
tText1 tProgress tTotalCount tMode 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fProgressBar wWin 
FUNCTION fProgressBar RETURNS LOGICAL
  ( INPUT ipiProcessed AS INT ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fWriteToFile wWin 
FUNCTION fWriteToFile RETURNS LOGICAL
  ( INPUT cType AS CHAR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bExit AUTO-END-KEY  NO-FOCUS
     LABEL "Exit" 
     SIZE 20 BY 2 TOOLTIP "Exit"
     FONT 37.

DEFINE BUTTON bProcess  NO-FOCUS
     LABEL "PROCESS" 
     SIZE 20 BY 2 TOOLTIP "Process"
     FONT 37.

DEFINE VARIABLE fiNoteCount AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE fiOutput AS CHARACTER FORMAT "X(256)":U 
     LABEL "List file for processed Notes" 
     VIEW-AS FILL-IN 
     SIZE 71 BY 1 NO-UNDO.

DEFINE VARIABLE fiReplaceWith AS CHARACTER FORMAT "X(256)":U 
     LABEL "with (leave blank to delete text)" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE fiSearchText AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search for notes containing the text" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE tAction AS CHARACTER FORMAT "X(256)":U INITIAL "If found, then" 
      VIEW-AS TEXT 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE tMode AS CHARACTER FORMAT "X(256)":U INITIAL "  Mode" 
      VIEW-AS TEXT 
     SIZE 7 BY .62 NO-UNDO.

DEFINE VARIABLE tProgress AS CHARACTER FORMAT "X(256)":U INITIAL "Progress:" 
      VIEW-AS TEXT 
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE tText1 AS CHARACTER FORMAT "X(256)":U INITIAL "(dbl-click to open)" 
      VIEW-AS TEXT 
     SIZE 21 BY .62 NO-UNDO.

DEFINE VARIABLE tText2 AS CHARACTER FORMAT "X(256)":U INITIAL "in" 
      VIEW-AS TEXT 
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE tText3 AS CHARACTER FORMAT "X(256)":U INITIAL "and/or" 
      VIEW-AS TEXT 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE tTotalCount AS CHARACTER FORMAT "X(256)":U INITIAL "Note count:" 
      VIEW-AS TEXT 
     SIZE 13 BY .62 NO-UNDO.

DEFINE VARIABLE rsAction AS CHARACTER INITIAL "Replace" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Delete the Note", "Delete",
"Replace the text", "Replace"
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE rsMode AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Simple", "Simple",
"Credit Card", "Credit"
     SIZE 15 BY 1.43 NO-UNDO.

DEFINE VARIABLE rsProcess AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Simulate", "Simulate",
"Purge", "Purge"
     SIZE 13 BY 2.14 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 20 BY 3.1.

DEFINE RECTANGLE rProgress1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 71 BY .91.

DEFINE RECTANGLE rProgress2
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE .2 BY .91
     BGCOLOR 10 .

DEFINE VARIABLE tbText AS LOGICAL INITIAL yes 
     LABEL "Note Text" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE tbTitles AS LOGICAL INITIAL yes 
     LABEL "Note Titles" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     bExit AT ROW 9.1 COL 87 WIDGET-ID 2
     bProcess AT ROW 9.1 COL 36 WIDGET-ID 18
     fiSearchText AT ROW 1.48 COL 40 COLON-ALIGNED WIDGET-ID 20
     tbTitles AT ROW 1.48 COL 84 WIDGET-ID 28
     tbText AT ROW 1.48 COL 107 WIDGET-ID 30
     rsAction AT ROW 2.91 COL 21 NO-LABEL WIDGET-ID 50
     fiReplaceWith AT ROW 2.91 COL 93 COLON-ALIGNED WIDGET-ID 26
     fiOutput AT ROW 4.81 COL 34 COLON-ALIGNED WIDGET-ID 16
     fiNoteCount AT ROW 8.14 COL 119 COLON-ALIGNED NO-LABEL WIDGET-ID 58 NO-TAB-STOP 
     rsProcess AT ROW 9.1 COL 14 NO-LABEL WIDGET-ID 32
     rsMode AT ROW 10.29 COL 118 NO-LABEL WIDGET-ID 22
     tText2 AT ROW 1.48 COL 78 COLON-ALIGNED NO-LABEL WIDGET-ID 46 NO-TAB-STOP 
     tText3 AT ROW 1.48 COL 97 COLON-ALIGNED NO-LABEL WIDGET-ID 68 NO-TAB-STOP 
     tAction AT ROW 2.91 COL 4 COLON-ALIGNED NO-LABEL WIDGET-ID 54 NO-TAB-STOP 
     tText1 AT ROW 5.05 COL 107 COLON-ALIGNED NO-LABEL WIDGET-ID 66 NO-TAB-STOP 
     tProgress AT ROW 6.95 COL 24 COLON-ALIGNED NO-LABEL WIDGET-ID 64 NO-TAB-STOP 
     tTotalCount AT ROW 7.43 COL 119 COLON-ALIGNED NO-LABEL WIDGET-ID 56 NO-TAB-STOP 
     tMode AT ROW 8.38 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 38 NO-TAB-STOP 
     RECT-5 AT ROW 8.62 COL 9 WIDGET-ID 36
     rProgress1 AT ROW 6.95 COL 36 WIDGET-ID 60
     rProgress2 AT ROW 6.95 COL 36 WIDGET-ID 62
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 144.6 BY 11.1 WIDGET-ID 100.


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
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Note Cleanup Utility"
         HEIGHT             = 11.1
         WIDTH              = 144.6
         MAX-HEIGHT         = 20
         MAX-WIDTH          = 149.4
         VIRTUAL-HEIGHT     = 20
         VIRTUAL-WIDTH      = 149.4
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN fiNoteCount IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       fiNoteCount:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fiOutput:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR RECTANGLE rProgress1 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rProgress2 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tAction IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       tAction:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN tMode IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       tMode:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN tProgress IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       tProgress:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN tText1 IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       tText1:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN tText2 IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       tText2:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN tText3 IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       tText3:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN tTotalCount IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       tTotalCount:READ-ONLY IN FRAME fMain        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Note Cleanup Utility */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Note Cleanup Utility */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bProcess
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bProcess wWin
ON CHOOSE OF bProcess IN FRAME fMain /* PROCESS */
DO:
    DEF VAR cOutputFile AS CHAR NO-UNDO.
    DEF VAR lSuccess AS LOG NO-UNDO.
    DEF VAR cErrorMessage AS CHAR NO-UNDO.
    
    RUN filesys_newfile ("NotePurge",
                        "CSV",
                        "RPT",
                        "DateTime",
                        OUTPUT cOutputFile,
                        OUTPUT lSuccess,
                        OUTPUT cErrorMessage).
    IF NOT lSuccess THEN DO:
        MESSAGE 
            "File creation failed due to:" SKIP 
            cErrorMessage SKIP 
            "Please try again."
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    
    ASSIGN 
        fiOutput:SCREEN-VALUE = cOutputFile.  
    OUTPUT STREAM outFile TO VALUE(cOutputFile).
    fWriteToFile("Header").
    IF rsAction:SCREEN-VALUE EQ "Delete" THEN 
        OUTPUT STREAM dumpfile TO VALUE(REPLACE(cOutputFile,".CSV",".d")).

    ASSIGN 
        bProcess:SENSITIVE = FALSE  
        bExit:SENSITIVE = FALSE.
    STATUS INPUT "Processing...". 
    
    RUN pProcess.     
    
    IF fiSearchText:SCREEN-VALUE BEGINS "<Credi" THEN DO:
        ASSIGN 
            fiSearchText:SCREEN-VALUE = "".
        APPLY 'leave' TO fiSearchText.
    END. 
    
    ASSIGN 
        bProcess:SENSITIVE = TRUE   
        bExit:SENSITIVE = TRUE.
    STATUS INPUT "".
    OUTPUT STREAM outFile CLOSE. 
    OUTPUT STREAM dumpFile CLOSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiOutput
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiOutput wWin
ON HELP OF fiOutput IN FRAME fMain /* List file for processed Notes */
DO:
    DEF VAR cSelectedDir AS CHAR NO-UNDO.
    SYSTEM-DIALOG GET-DIR cSelectedDir
        INITIAL-DIR SESSION:TEMP-DIRECTORY 
        TITLE "Select a GL Account Purge Directory".
    ASSIGN 
        SELF:SCREEN-VALUE = cSelectedDir.
    APPLY 'value-changed' TO SELF.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiOutput wWin
ON MOUSE-SELECT-DBLCLICK OF fiOutput IN FRAME fMain /* List file for processed Notes */
DO:
    OS-COMMAND SILENT VALUE(fiOutput:SCREEN-VALUE IN FRAME {&frame-name}).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiSearchText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSearchText wWin
ON LEAVE OF fiSearchText IN FRAME fMain /* Search for notes containing the text */
DO:
    ASSIGN 
        rsMode:SCREEN-VALUE = IF SELF:SCREEN-VALUE BEGINS "<Credit" THEN "Credit" ELSE "Simple".  
    IF rsMode:SCREEN-VALUE = "Credit" THEN DO:
        ASSIGN 
            fiReplaceWith:VISIBLE = FALSE
            rsAction:SCREEN-VALUE = "Delete".
    END.
    ELSE IF rsMode:SCREEN-VALUE = "Simple" THEN DO:
        ASSIGN 
            fiReplaceWith:VISIBLE  = TRUE.
    END.
    APPLY 'value-chaged' TO rsAction.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsAction
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsAction wWin
ON VALUE-CHANGED OF rsAction IN FRAME fMain
DO:
    IF SELF:SCREEN-VALUE = "Replace" 
    AND rsMode:SCREEN-VALUE NE "Credit" THEN DO:
        ASSIGN
            fiReplaceWith:VISIBLE = TRUE.
    END.
    ELSE IF SELF:SCREEN-VALUE = "Delete" THEN DO:
        ASSIGN
            fiReplaceWith:VISIBLE = FALSE.
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsMode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsMode wWin
ON VALUE-CHANGED OF rsMode IN FRAME fMain
DO:
    IF SELF:SCREEN-VALUE = "Credit" THEN DO:
        ASSIGN 
            fiSearchText:VISIBLE = FALSE 
            fiReplaceWith:VISIBLE = FALSE
            rsAction:SCREEN-VALUE = "Delete".
    END.
    ELSE IF SELF:SCREEN-VALUE = "Simple" THEN DO:
        ASSIGN 
            fiSearchText:VISIBLE = TRUE  
            fiReplaceWith:VISIBLE  = TRUE.
    END.
    APPLY 'value-changed' TO rsAction.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


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

    RUN util/purgeProcs.p PERSISTENT SET hPurge.
 
    /* Finds current company */
    FIND FIRST company NO-LOCK WHERE 
        company.company EQ cocode
        NO-ERROR.
    IF NOT AVAIL company THEN DO:
        MESSAGE 
            "There is an issue with your company code." SKIP 
            "Please exit Advantzware and login again."
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    
    ASSIGN 
        rsMode:VISIBLE = FALSE.

    STATUS INPUT "Determining total note count.".
    STATUS DEFAULT "Determining total note count.".
    SESSION:SET-WAIT-STATE("General").
    FOR EACH notes NO-LOCK.
        ASSIGN 
            iNoteCount = iNoteCount + 1.
    END.
    ASSIGN 
        fiNoteCount:SCREEN-VALUE = STRING(iNoteCount).
    SESSION:SET-WAIT-STATE("").
    STATUS DEFAULT "".
    STATUS INPUT "".

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY fiSearchText tbTitles tbText rsAction fiReplaceWith fiOutput 
          fiNoteCount rsProcess rsMode tText2 tText3 tAction tText1 tProgress 
          tTotalCount tMode 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE bExit RECT-5 bProcess fiSearchText tbTitles tbText rsAction 
         fiReplaceWith fiOutput rsProcess rsMode 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProcess wWin 
PROCEDURE pProcess :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF BUFFER bNotes FOR notes.
    DEF VAR iProcessed AS INT NO-UNDO.
    DEF VAR lProcessMe AS LOG NO-UNDO.
    DEF VAR cString AS CHAR NO-UNDO.
    DEF VAR iCount AS INT NO-UNDO.
    
    IF rsMode:SCREEN-VALUE IN FRAME {&frame-name} EQ "Simple" THEN DO:
        FOR EACH notes NO-LOCK: 
            ASSIGN 
                lProcessMe = FALSE
                iProcessed = iProcessed + 1.
            fProgressBar(iProcessed).
            
            IF tbTitles:CHECKED 
            AND INDEX(notes.note_title,fiSearchText:SCREEN-VALUE) GT 0 THEN ASSIGN 
                lProcessMe = TRUE.  
            IF tbText:CHECKED 
            AND INDEX(notes.note_text,fiSearchText:SCREEN-VALUE) GT 0 THEN ASSIGN 
                lProcessMe = TRUE.
                
            IF lProcessMe THEN DO:
                fWriteToFile("").
                IF rsProcess:SCREEN-VALUE EQ "Purge" THEN DO:
                    FIND bNotes EXCLUSIVE WHERE 
                        ROWID(bNotes) EQ ROWID(notes)
                        NO-ERROR.
                    IF AVAIL bNotes THEN DO:
                        IF rsAction:SCREEN-VALUE EQ "Delete" THEN DO:
                            EXPORT STREAM dumpFile bNotes.
                            DELETE bNotes.
                            ASSIGN 
                                fiNoteCount:SCREEN-VALUE = STRING(INTEGER(fiNoteCount:SCREEN-VALUE) - 1).
                        END.
                        ELSE DO:
                            IF tbTitles:CHECKED 
                            AND INDEX(bnotes.note_title,fiSearchText:SCREEN-VALUE) GT 0 THEN ASSIGN
                                bNotes.note_title = REPLACE(bNotes.note_title, fiSearchText:SCREEN-VALUE, fiReplaceWith:SCREEN-VALUE).
                            IF tbText:CHECKED 
                            AND INDEX(bnotes.note_text,fiSearchText:SCREEN-VALUE) GT 0 THEN ASSIGN
                                bNotes.note_text = REPLACE(bNotes.note_text, fiSearchText:SCREEN-VALUE, fiReplaceWith:SCREEN-VALUE).
                        END.
                    END.
                END.
            END.
        END.
    END.
    ELSE IF rsMode:SCREEN-VALUE IN FRAME {&frame-name} EQ "Credit" THEN DO:
        FOR EACH notes NO-LOCK: 
            ASSIGN 
                lProcessMe = FALSE
                iProcessed = iProcessed + 1.
            fProgressBar(iProcessed).
            
            /* Credit Card logic here */
            IF INDEX(notes.note_text,"EXP") NE 0 
            AND (INDEX(notes.note_text,"CVV") NE 0 OR INDEX(notes.note_text,"CVV") NE 0) 
            THEN ASSIGN 
                lProcessMe = TRUE.
            
            IF INDEX(notes.note_text,"AMEX") NE 0 
            OR INDEX(notes.note_text,"American Express") NE 0
            OR INDEX(notes.note_text," MC ") NE 0
            OR INDEX(notes.note_text,"MC ") EQ 1
            OR INDEX(notes.note_text,"Mastercard") NE 0
            OR INDEX(notes.note_text,"Master card") NE 0
            OR (INDEX(notes.note_text,"Credit Card") NE 0 
                AND INDEX(notes.note_text,"by Credit Card") EQ 0
                AND INDEX(notes.note_text,"via Credit Card") EQ 0
                AND INDEX(notes.note_text,"Credit Card.") EQ 0
                ) 
            OR INDEX(notes.note_text,"VISA") NE 0 
            OR INDEX(notes.note_text," CVC") NE 0 
            OR INDEX(notes.note_text," CVV") NE 0 
            OR (INDEX(notes.note_text,"CC ") NE 0 AND INDEX(notes.note_text,"Ex") NE 0)
            THEN ASSIGN 
                lProcessMe = TRUE.
                
            IF INDEX(notes.note_title,"Credit Card") NE 0 
            THEN ASSIGN
                lProcessMe = TRUE.  
            /* End CC logic */
            
            IF lProcessMe THEN DO:
                fWriteToFile("").
                IF rsProcess:SCREEN-VALUE EQ "Purge" THEN DO:
                    FIND bNotes EXCLUSIVE WHERE 
                        ROWID(bNotes) EQ ROWID(notes)
                        NO-ERROR.
                    IF AVAIL bNotes THEN DO:
                        IF rsAction:SCREEN-VALUE EQ "Delete" THEN DO: 
                            EXPORT STREAM dumpFile bNotes.
                            DELETE bNotes.
                            ASSIGN 
                                fiNoteCount:SCREEN-VALUE = STRING(INTEGER(fiNoteCount:SCREEN-VALUE) - 1).
                        END.
                        ELSE DO:
                            IF tbTitles:CHECKED THEN DO:
                                ASSIGN 
                                    cString = bNotes.note_title.
                                DO iCount = 1 TO LENGTH(cString):
                                    IF INDEX("0123456789",SUBSTRING(cString,iCount,1)) NE 0 THEN ASSIGN 
                                        SUBSTRING(cString,iCount,1) = "X".
                                END.
                                ASSIGN
                                    bNotes.note_title = cString.
                            END.
                            IF tbText:CHECKED THEN DO:
                                ASSIGN 
                                    cString = bNotes.note_text.
                                DO iCount = 1 TO LENGTH(cString):
                                    IF INDEX("0123456789",SUBSTRING(cString,iCount,1)) NE 0 THEN ASSIGN 
                                        SUBSTRING(cString,iCount,1) = "X".
                                END.
                                ASSIGN
                                    bNotes.note_text = cString.
                            END.
                        END.
                    END.
                END.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fProgressBar wWin 
FUNCTION fProgressBar RETURNS LOGICAL
  ( INPUT ipiProcessed AS INT ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    ASSIGN 
        rProgress2:WIDTH IN FRAME {&frame-name} = rProgress1:WIDTH * (ipiProcessed / iNoteCount).

    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fWriteToFile wWin 
FUNCTION fWriteToFile RETURNS LOGICAL
  ( INPUT cType AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lReturn AS LOGICAL NO-UNDO.

    IF cType EQ "Header" THEN DO:
        PUT STREAM outFile UNFORMATTED
            "Type:" + "," + rsMode:SCREEN-VALUE IN FRAME {&frame-name} + CHR(10) +
            "Action:" + "," + rsAction:SCREEN-VALUE + "," + fiSearchText:SCREEN-VALUE + CHR(10) +
            "" + "," + "With" + "," + fiReplaceWith:SCREEN-VALUE + CHR(10) +
            "Context:" + "," + 
                (IF tbTitles:CHECKED THEN "Titles" ELSE "") + "," +
                (IF tbText:CHECKED THEN "Text" ELSE "") + CHR(10) +
            "Mode:" + "," + rsProcess:SCREEN-VALUE + CHR(10) +
            CHR(10) + 
            "Title" + "," + "Text" + "," + "Action" + CHR(10). 
    END.
    ELSE DO:
        PUT STREAM outFile UNFORMATTED
            REPLACE(REPLACE(REPLACE(notes.note_title,","," "),chr(10)," "),chr(13)," ") + "," +
            REPLACE(REPLACE(REPLACE(notes.note_text,","," "),chr(10)," "),chr(13)," ") + "," + 
            (IF rsProcess:SCREEN-VALUE EQ "Purge" THEN rsAction:SCREEN-VALUE + "d" ELSE "To be " + rsAction:SCREEN-VALUE + "d") +
            CHR(10).
    END.
        
    RETURN lReturn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

