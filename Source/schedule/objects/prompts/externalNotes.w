&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME notesFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS notesFrame 
/*------------------------------------------------------------------------

  File: externalNotes.w

  Description: schedule board job notes 

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 1.31.2022 (copied from schedule/objects/prompts/jobNotes.w)
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcJobNo   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipiJobNo2  AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER ipiFormNo  AS INTEGER   NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cNoteTime AS CHARACTER NO-UNDO FORMAT "x(11)" LABEL "Time".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME notesFrame
&Scoped-define BROWSE-NAME notesBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES sbNote

/* Definitions for BROWSE notesBrowse                                   */
&Scoped-define FIELDS-IN-QUERY-notesBrowse sbNote.noteDate ~
fNoteTime(sbNote.noteTime) @ cNoteTime sbNote.m-code 
&Scoped-define ENABLED-FIELDS-IN-QUERY-notesBrowse 
&Scoped-define QUERY-STRING-notesBrowse FOR EACH sbNote ~
      WHERE sbNote.company EQ ipcCompany AND ~
sbNote.job-no EQ ipcJobNo AND ~
sbNote.job-no2 EQ ipiJobNo2 AND ~
sbNote.frm EQ ipiFormNo NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-notesBrowse OPEN QUERY notesBrowse FOR EACH sbNote ~
      WHERE sbNote.company EQ ipcCompany AND ~
sbNote.job-no EQ ipcJobNo AND ~
sbNote.job-no2 EQ ipiJobNo2 AND ~
sbNote.frm EQ ipiFormNo NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-notesBrowse sbNote
&Scoped-define FIRST-TABLE-IN-QUERY-notesBrowse sbNote


/* Definitions for DIALOG-BOX notesFrame                                */
&Scoped-define OPEN-BROWSERS-IN-QUERY-notesFrame ~
    ~{&OPEN-QUERY-notesBrowse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS notesBrowse jobNotesText btnAdd btnDelete ~
btnSave btnReset btnExit 
&Scoped-Define DISPLAYED-OBJECTS jobNotesText 

/* Custom List Definitions                                              */
/* noteButton,List-2,List-3,List-4,List-5,List-6                        */
&Scoped-define noteButton btnAdd btnDelete btnSave btnReset 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fNoteTime notesFrame 
FUNCTION fNoteTime RETURNS CHARACTER
  (iNoteTime AS INTEGER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAdd 
     IMAGE-UP FILE "schedule/images/add.bmp":U
     LABEL "" 
     SIZE 5 BY 1.19 TOOLTIP "Add New Job Note"
     BGCOLOR 8 .

DEFINE BUTTON btnDelete 
     IMAGE-UP FILE "schedule/images/cancel.bmp":U
     LABEL "" 
     SIZE 5 BY 1.19 TOOLTIP "Delete Job Note"
     BGCOLOR 8 .

DEFINE BUTTON btnExit 
     IMAGE-UP FILE "schedule/images/exit1.bmp":U
     LABEL "" 
     SIZE 5 BY 1.19
     BGCOLOR 8 .

DEFINE BUTTON btnReset 
     IMAGE-UP FILE "schedule/images/rollback.bmp":U
     LABEL "" 
     SIZE 5 BY 1.19 TOOLTIP "Reset Job Note"
     BGCOLOR 8 .

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "schedule/images/commit.bmp":U
     LABEL "" 
     SIZE 5 BY 1.19 TOOLTIP "Save Job Note"
     BGCOLOR 8 .

DEFINE VARIABLE jobNotesText AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 69 BY 16.67
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 28 BY 1.67.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY notesBrowse FOR 
      sbNote SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE notesBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS notesBrowse notesFrame _STRUCTURED
  QUERY notesBrowse NO-LOCK DISPLAY
      sbNote.noteDate FORMAT "99/99/9999":U
      fNoteTime(sbNote.noteTime) @ cNoteTime
      sbNote.m-code FORMAT "x(6)":U WIDTH 17.6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 41 BY 14.76
         TITLE "SB Notes" ROW-HEIGHT-CHARS .67 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME notesFrame
     notesBrowse AT ROW 1.24 COL 2
     jobNotesText AT ROW 1.24 COL 43 NO-LABEL
     btnAdd AT ROW 16.48 COL 9
     btnDelete AT ROW 16.48 COL 14
     btnSave AT ROW 16.48 COL 19
     btnReset AT ROW 16.48 COL 24
     btnExit AT ROW 16.48 COL 30
     RECT-1 AT ROW 16.24 COL 8
     SPACE(75.99) SKIP(0.08)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Job Notes".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX notesFrame
   FRAME-NAME                                                           */
/* BROWSE-TAB notesBrowse RECT-1 notesFrame */
ASSIGN 
       FRAME notesFrame:SCROLLABLE       = FALSE
       FRAME notesFrame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnAdd IN FRAME notesFrame
   1                                                                    */
/* SETTINGS FOR BUTTON btnDelete IN FRAME notesFrame
   1                                                                    */
/* SETTINGS FOR BUTTON btnReset IN FRAME notesFrame
   1                                                                    */
/* SETTINGS FOR BUTTON btnSave IN FRAME notesFrame
   1                                                                    */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME notesFrame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE notesBrowse
/* Query rebuild information for BROWSE notesBrowse
     _TblList          = "ASI.sbNote"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "sbNote.company EQ ipcCompany AND
sbNote.job-no EQ ipcJobNo AND
sbNote.job-no2 EQ ipiJobNo2 AND
sbNote.frm EQ ipiFormNo"
     _FldNameList[1]   = ASI.sbNote.noteDate
     _FldNameList[2]   > "_<CALC>"
"fNoteTime(sbNote.noteTime) @ cNoteTime" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.sbNote.m-code
"sbNote.m-code" ? ? "character" ? ? ? ? ? ? no ? no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE notesBrowse */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME notesFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL notesFrame notesFrame
ON WINDOW-CLOSE OF FRAME notesFrame /* Job Notes */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd notesFrame
ON CHOOSE OF btnAdd IN FRAME notesFrame
DO:
    DEFINE VARIABLE cChanges   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResources AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx        AS INTEGER   NO-UNDO.

    DEFINE BUFFER bJobMch FOR job-mch.
   
    RUN schedule/objects/prompts/resources.w (
        ipcCompany,
        ipcJobNo,
        ipiJobNo2,
        ipiFormNo,
        OUTPUT cResources,
        OUTPUT cChanges
        ).
    DO idx = 1 TO NUM-ENTRIES(cResources):
        FOR EACH job-mch NO-LOCK
            WHERE job-mch.company EQ ipcCompany
              AND job-mch.m-code  EQ ENTRY(idx,cResources)
              AND job-mch.job-no  EQ ipcJobNo
              AND job-mch.job-no2 EQ ipiJobNo2
              AND job-mch.frm     EQ ipiFormNo
               BY job-mch.seq
            :
            IF CAN-FIND(FIRST job-mat
                WHERE job-mat.company EQ ipcCompany
                  AND job-mat.job-no  EQ ipcJobNo
                  AND job-mat.job-no2 EQ ipiJobNo2
                  AND job-mat.frm     EQ ipiFormNo) THEN DO:
                // check if machine change has been indicated
                DO jdx = 1 TO NUM-ENTRIES(cChanges) BY 2:
                    IF ENTRY(jdx,cChanges) EQ job-mch.m-code THEN DO:
                        // change routing to new machine
                        FIND FIRST bJobMch EXCLUSIVE-LOCK
                             WHERE ROWID(bJobMch) EQ ROWID(job-mch).
                        bJobMch.m-code = ENTRY(jdx + 1,cChanges).
                        RELEASE bJobMch.
                    END. // if entry(jdx)
                END. // do jdx
                CREATE sbNote.
                ASSIGN
                    sbNote.company  = job-mch.company
                    sbNote.m-code   = job-mch.m-code
                    sbNote.job-no   = job-mch.job-no
                    sbNote.job-no2  = job-mch.job-no2
                    sbNote.frm      = job-mch.frm
                    sbNote.noteDate = TODAY
                    sbNote.noteTime = TIME
                    .
                FOR EACH job-mat NO-LOCK
                    WHERE job-mat.company EQ ipcCompany
                      AND job-mat.job-no  EQ ipcJobNo
                      AND job-mat.job-no2 EQ ipiJobNo2
                      AND job-mat.frm     EQ ipiFormNo,
                    FIRST item NO-LOCK
                    WHERE item.company EQ job-mat.company
                      AND item.i-no    EQ job-mat.rm-i-no
                      AND LOOKUP(item.mat-type,"1,2,3,4,A,B,R,P") GT 0
                    :
                    sbNote.jobNote = sbNote.jobNote
                                   + job-mat.rm-i-no + " = "
                                   + STRING(job-mat.qty-all)
                                   + CHR(10)
                                   .
                END. // each job-mat
                sbNote.jobNote = TRIM(sbNote.jobNote,CHR(10)).
            END. // if can-find
        END. // each job-mch
    END. // do idx
    {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
    APPLY 'VALUE-CHANGED':U TO notesBrowse.
    APPLY 'ENTRY' TO jobNotesText.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete notesFrame
ON CHOOSE OF btnDelete IN FRAME notesFrame
DO:
  IF NOT AVAILABLE sbNote THEN
  RETURN NO-APPLY.
  MESSAGE 'Delete Job Note?' VIEW-AS ALERT-BOX
    QUESTION BUTTONS YES-NO UPDATE deleteOK AS LOGICAL.
  IF NOT deleteOK THEN
  RETURN NO-APPLY.
  DO TRANSACTION:
      FIND CURRENT sbNote EXCLUSIVE-LOCK.
      DELETE sbNote.
      jobNotesText:SCREEN-VALUE = "".
  END.
  {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
  APPLY 'VALUE-CHANGED' TO {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExit notesFrame
ON CHOOSE OF btnExit IN FRAME notesFrame
DO:
  APPLY 'END-ERROR':U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset notesFrame
ON CHOOSE OF btnReset IN FRAME notesFrame
DO:
  IF NOT AVAILABLE sbNote THEN
  RETURN NO-APPLY.
  jobNotesText:SCREEN-VALUE = sbNote.jobNote.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave notesFrame
ON CHOOSE OF btnSave IN FRAME notesFrame
DO:
  IF NOT AVAILABLE sbNote THEN
  RETURN NO-APPLY.
  DO TRANSACTION:
      FIND CURRENT sbNote EXCLUSIVE-LOCK.
      sbNote.jobNote = jobNotesText:SCREEN-VALUE.
      FIND CURRENT sbNote NO-LOCK.
  END.
  MESSAGE 'Note Text Saved!' VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME notesBrowse
&Scoped-define SELF-NAME notesBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL notesBrowse notesFrame
ON VALUE-CHANGED OF notesBrowse IN FRAME notesFrame /* SB Notes */
DO:
    IF AVAILABLE sbNote THEN DO:
        jobNotesText:SCREEN-VALUE = sbNote.jobNote.
/*        RUN createJobsToggleBoxes.*/
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK notesFrame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  FRAME {&FRAME-NAME}:TITLE = "SB Notes for Job: "
                            + ipcJobNo + "-"
                            + STRING(ipiJobNo2) + "."
                            + STRING(ipiFormNo)
                            .
  RUN enable_UI.
  {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
  APPLY 'VALUE-CHANGED' TO {&BROWSE-NAME}.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI notesFrame  _DEFAULT-DISABLE
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
  HIDE FRAME notesFrame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI notesFrame  _DEFAULT-ENABLE
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
  DISPLAY jobNotesText 
      WITH FRAME notesFrame.
  ENABLE notesBrowse jobNotesText btnAdd btnDelete btnSave btnReset btnExit 
      WITH FRAME notesFrame.
  VIEW FRAME notesFrame.
  {&OPEN-BROWSERS-IN-QUERY-notesFrame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fNoteTime notesFrame 
FUNCTION fNoteTime RETURNS CHARACTER
  (iNoteTime AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RETURN STRING(iNoteTime,"hh:mm:ss am").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

