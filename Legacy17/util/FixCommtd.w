&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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
DEF VAR fjob-no AS CHAR FORMAT "x(8)" NO-UNDO.
DEF VAR tjob-no AS CHAR FORMAT "x(8)" NO-UNDO.

DEF VAR fjob-no2 AS INT NO-UNDO.
DEF VAR tjob-no2 AS INT NO-UNDO.

DEF TEMP-TABLE tt-job-mat NO-UNDO
    FIELD t-j-no    LIKE job-mat.j-no
    FIELD t-job-no  LIKE job-mat.job-no
    FIELD t-job-no2 LIKE job-mat.job-no2
    FIELD t-rm-i-no LIKE job-mat.rm-i-no
    FIELD t-qty-all LIKE job-mat.qty-all
    FIELD t-all-flg LIKE job-mat.all-flg
    FIELD t-rowid   AS ROWID
    .                     

DEF BUFFER b-tt-job-mat FOR tt-job-mat.

DEF BUFFER bf-tt-job-mat FOR tt-job-mat.

{custom/globdefs.i}

{sys/inc/VAR.i "new shared"}

ASSIGN
 cocode = g_company
 locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-job-mat

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt-job-mat.t-job-no tt-job-mat.t-job-no2 tt-job-mat.t-rm-i-no tt-job-mat.t-qty-all tt-job-mat.t-all-flg /*   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 tt-job-mat.new-i-no */   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-2 tt-job-mat
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-2 tt-job-mat
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tt-job-mat
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tt-job-mat.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt-job-mat
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt-job-mat


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 begin_job1 begin_job2 btn_go end_job1 ~
end_job2 BROWSE-2 Btn_Select Btn_Deselect Btn_process Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_job1 begin_job2 end_job1 end_job2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 20 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Deselect 
     LABEL "Unselect All" 
     SIZE 20 BY 1.14.

DEFINE BUTTON btn_go 
     LABEL "Go" 
     SIZE 17 BY 1.

DEFINE BUTTON Btn_process 
     LABEL "DeAllocate" 
     SIZE 20 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Select 
     LABEL "Select All" 
     SIZE 20 BY 1.14.

DEFINE VARIABLE begin_job1 AS CHARACTER FORMAT "X(6)":U 
     LABEL "From Job #" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job2 AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE end_job1 AS CHARACTER FORMAT "X(6)":U 
     LABEL "     To Job #" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_job2 AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 83 BY 2.62.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tt-job-mat SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 D-Dialog _FREEFORM
  QUERY BROWSE-2 DISPLAY
      tt-job-mat.t-job-no  COLUMN-LABEL "Job#"          WIDTH 10
      tt-job-mat.t-job-no2 LABEL ""              WIDTH 2
      tt-job-mat.t-rm-i-no LABEL "Rm Item#"      WIDTH 35
      tt-job-mat.t-qty-all COLUMN-LABEL "Qty ! Committed" WIDTH 10
      tt-job-mat.t-all-flg LABEL "Committed"     WIDTH 5


/*       ENABLE tt-job-mat.new-i-no */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 83 BY 16.19
         BGCOLOR 8  FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     begin_job1 AT ROW 1.71 COL 27 COLON-ALIGNED WIDGET-ID 14
     begin_job2 AT ROW 1.71 COL 42 COLON-ALIGNED WIDGET-ID 6
     btn_go AT ROW 2.43 COL 55
     end_job1 AT ROW 2.86 COL 27 COLON-ALIGNED WIDGET-ID 16
     end_job2 AT ROW 3 COL 42 COLON-ALIGNED WIDGET-ID 10
     BROWSE-2 AT ROW 4.38 COL 1
     Btn_Select AT ROW 21.24 COL 7
     Btn_Deselect AT ROW 21.24 COL 57
     Btn_process AT ROW 24.33 COL 15
     Btn_Cancel AT ROW 24.33 COL 50
     RECT-2 AT ROW 20.52 COL 1 WIDGET-ID 12
     SPACE(0.39) SKIP(3.80)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Deallcation By Job #"
         DEFAULT-BUTTON btn_go CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow.i}
{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BROWSE-2 end_job2 D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-job-mat
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME begin_job1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job1 D-Dialog
ON LEAVE OF begin_job1 IN FRAME D-Dialog /* From Job # */
DO:

    ASSIGN 
        {&self-name}
        {&self-name}:SCREEN-VALUE = CAPS({&self-name})
        end_job1:SCREEN-VALUE = {&self-name}:SCREEN-VALUE.



END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job2 D-Dialog
ON LEAVE OF begin_job2 IN FRAME D-Dialog /* - */
DO:

    ASSIGN {&self-name}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Deselect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Deselect D-Dialog
ON CHOOSE OF Btn_Deselect IN FRAME D-Dialog /* Unselect All */
DO:
  {&browse-name}:DESELECT-ROWS ().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go D-Dialog
ON CHOOSE OF btn_go IN FRAME D-Dialog /* Go */
DO:

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            fjob-no   = FILL(" ",6 - LENGTH(TRIM(begin_job1:SCREEN-VALUE))) +
                             TRIM(begin_job1:SCREEN-VALUE)
            tjob-no   = FILL(" ",6 - LENGTH(TRIM(end_job1:SCREEN-VALUE))) +
                             trim(end_job1:SCREEN-VALUE)
            fjob-no2  = INT(begin_job2:SCREEN-VALUE)
            tjob-no2  = INT(end_job2:SCREEN-VALUE)
            fjob-no   = FILL(" ",6 - LENGTH(TRIM(fjob-no))) + TRIM(fjob-no) +
                             STRING(fjob-no2,"99")
            tjob-no   = FILL(" ",6 - LENGTH(TRIM(tjob-no))) + TRIM(tjob-no) +
                             STRING(tjob-no2,"99").
    END.

    RUN check-job.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_process D-Dialog
ON CHOOSE OF Btn_process IN FRAME D-Dialog /* DeAllocate */
DO:
  DEF VAR li AS INT NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.


  SESSION:SET-WAIT-STATE ("general").

  ASSIGN LL = {&browse-name}:NUM-SELECTED-ROWS GT 0.

  IF NOT ll THEN 
    MESSAGE 
       "Please select a job # to Deallocate."
      VIEW-AS ALERT-BOX.


  IF ll THEN DO:
    ll = NO.
    MESSAGE 
        "Are you sure you want to deallocate all of the selected Job #?"
     VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
     UPDATE ll.
  END.

  IF ll THEN DO:
      ASSIGN ll = FALSE.

      DO li = 1 TO {&browse-name}:NUM-SELECTED-ROWS:

          {&browse-name}:FETCH-SELECTED-ROW (li) NO-ERROR.

          IF AVAIL tt-job-mat THEN DO:

              FIND FIRST job-mat NO-LOCK
                  WHERE job-mat.company EQ cocode
                    AND ROWID(job-mat) EQ tt-job-mat.t-ROWID NO-ERROR.
              IF AVAIL job-mat THEN DO:

                  RUN jc/jc-all2.p (ROWID(job-mat), -1).

                  ASSIGN  ll = TRUE.

              END.
          END.
      END.
  END.


  SESSION:SET-WAIT-STATE ("").

  IF ll THEN 
    MESSAGE 
       "All of the selected jobs were deallocated."
      VIEW-AS ALERT-BOX.

  APPLY "CHOOSE" TO btn_go.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Select
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Select D-Dialog
ON CHOOSE OF Btn_Select IN FRAME D-Dialog /* Select All */
DO:
  {&browse-name}:SELECT-ALL ().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job1 D-Dialog
ON LEAVE OF end_job1 IN FRAME D-Dialog /*      To Job # */
DO:

    ASSIGN {&self-name}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job2 D-Dialog
ON LEAVE OF end_job2 IN FRAME D-Dialog /* - */
DO:

    ASSIGN {&self-name}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
  {src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table D-Dialog 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ip-job-no  LIKE job-mat.job-no NO-UNDO.
DEF INPUT PARAM ip-job-no2 LIKE job-mat.job-no2 NO-UNDO.


SESSION:SET-WAIT-STATE ("general").

FOR EACH job NO-LOCK
   WHERE job.company  EQ cocode
      AND job.job-no  EQ ip-job-no 
      AND job.job-no2 EQ ip-job-no2,
 EACH job-mat NO-LOCK
   WHERE job-mat.company EQ job.company
      AND job-mat.job     EQ job.job
      AND job-mat.job-no  EQ job.job-no
      AND job-mat.job-no2 EQ job.job-no2
      AND job-mat.qty-all GT 0
      AND job-mat.all-flg
    USE-INDEX seq-idx,
   FIRST item OF job-mat NO-LOCK
    WHERE item.company EQ job-mat.company 
      AND item.i-no EQ job-mat.rm-i-no:

    CREATE tt-job-mat.
    ASSIGN
        tt-job-mat.t-j-no    = job-mat.j-no
        tt-job-mat.t-job-no  = job-mat.job-no
        tt-job-mat.t-job-no2 = job-mat.job-no2
        tt-job-mat.t-rm-i-no = job-mat.rm-i-no
        tt-job-mat.t-qty-all = job-mat.qty-all
        tt-job-mat.t-all-flg = job-mat.all-flg
        tt-job-mat.t-ROWID   = ROWID(job-mat).

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-job D-Dialog 
PROCEDURE check-job :
EMPTY TEMP-TABLE tt-job-mat.
FOR EACH job NO-LOCK
  WHERE job.company                    EQ cocode 
    AND job.job-no                     GE SUBSTR(fjob-no,1,6)
    AND job.job-no                     LE SUBSTR(tjob-no,1,6)
    AND FILL(" ",6 - length(trim(job.job-no))) +
               TRIM(job.job-no) +
               STRING(job.job-no2,"99")  GE fjob-no
   AND FILL(" ",6 - length(trim(job.job-no))) +
               TRIM(job.job-no) +
               STRING(job.job-no2,"99")  LE tjob-no
   AND job.opened
   AND INDEX("LRAW",job.stat) > 0 ,
  FIRST job-mat NO-LOCK
   WHERE job-mat.company EQ job.company
     AND job-mat.job     EQ job.job
     AND job-mat.job-no  EQ job.job-no
     AND job-mat.job-no2 EQ job.job-no2
     AND job-mat.qty-all GE 0
     AND job-mat.all-flg
  USE-INDEX seq-idx,
  FIRST item OF ASI.job-mat NO-LOCK
    WHERE item.company EQ job-mat.company 
      AND item.i-no EQ job-mat.rm-i-no 
      AND item.i-code  EQ "R"
    :


   RUN build-table (job-mat.job-no,
                   job-mat.job-no2).

END.

{&OPEN-QUERY-{&BROWSE-NAME}}

SESSION:SET-WAIT-STATE ("").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY begin_job1 begin_job2 end_job1 end_job2 
      WITH FRAME D-Dialog.
  ENABLE RECT-2 begin_job1 begin_job2 btn_go end_job1 end_job2 BROWSE-2 
         Btn_Select Btn_Deselect Btn_process Btn_Cancel 
      WITH FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-job-mat"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

