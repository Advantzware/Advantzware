&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: windows\d-impbud.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}

DEF TEMP-TABLE tt-bud NO-UNDO
    FIELD sman AS CHAR
    FIELD cust-no AS CHAR
    FIELD procat AS CHAR
    FIELD amt AS DEC EXTENT 13
    FIELD msf AS INT EXTENT 13
    FIELD tons AS INT EXTENT 13
    INDEX tt-bud-idx cust-no procat.
DEF VAR ll-secure AS LOGICAL NO-UNDO.
ASSIGN cocode = g_company
       locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 RECT-41 tb-clear-before-import ~
lv-budget-year in-file-name bt-search-file btn-import btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tb-clear-before-import lv-budget-year ~
in-file-name 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-search-file 
     IMAGE-UP FILE "Graphics/32x32/document_view.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/inactive.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Search" 
     SIZE 7.8 BY 1.81 TOOLTIP "Search".

DEFINE BUTTON btn-cancel AUTO-GO 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-import 
     LABEL "&Start Import" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE in-file-name AS CHARACTER FORMAT "X(256)":U 
     LABEL "Import From File" 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1 NO-UNDO.

DEFINE VARIABLE lv-budget-year AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Budget Year" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 5.24.

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY 8.33.

DEFINE VARIABLE tb-clear-before-import AS LOGICAL INITIAL no 
     LABEL "Zero out all budgets before uploading new budgets?" 
     VIEW-AS TOGGLE-BOX
     SIZE 54 BY 1.19 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     tb-clear-before-import AT ROW 2.67 COL 33 WIDGET-ID 2
     lv-budget-year AT ROW 2.91 COL 17 COLON-ALIGNED
     in-file-name AT ROW 4.33 COL 17 COLON-ALIGNED
     bt-search-file AT ROW 4.33 COL 72 WIDGET-ID 4
     btn-import AT ROW 7.43 COL 21
     btn-cancel AT ROW 7.43 COL 60
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 1.71 COL 6
     RECT-17 AT ROW 1.24 COL 2
     RECT-41 AT ROW 1 COL 1
     SPACE(1.39) SKIP(1.90)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Copy Budget Periods".


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
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Copy Budget Periods */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-search-file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-search-file Dialog-Frame
ON CHOOSE OF bt-search-file IN FRAME Dialog-Frame
DO:
   def var ls-filename as cha no-undo.
   def var ll-ok as log no-undo.
   
   system-dialog get-file ls-filename 
                 title "Select Image File to insert"
                 filters "Excel Comma delimited Files    (*.csv)" "*.csv",
                         "All Files    (*.*) " "*.*"
                 initial-dir  "c:\"
                 MUST-EXIST
                 USE-FILENAME
                 UPDATE ll-ok.
      
    IF ll-ok THEN in-file-name:screen-value = ls-filename.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel Dialog-Frame
ON CHOOSE OF btn-cancel IN FRAME Dialog-Frame /* Cancel */
DO:
    apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-import
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-import Dialog-Frame
ON CHOOSE OF btn-import IN FRAME Dialog-Frame /* Start Import */
DO:
  DEF VAR v-process AS LOG NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  
    IF lv-budget-year EQ 0 THEN
    DO:
        MESSAGE "Invalid Year."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        APPLY "ENTRY" TO lv-budget-year.
        RETURN NO-APPLY.
    END.

    IF SEARCH(in-file-name) eq ? THEN
    DO:
       MESSAGE "ERROR: Could Not Find " in-file-name "." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO in-file-name.
       RETURN NO-APPLY.
    END.

    MESSAGE "Are you sure you want to import sales rep budgets for the selected parameters?"
           VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.
        
    IF v-process THEN
    DO:
       RUN run-process.
       MESSAGE "Sales Rep Budgets were imported."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    ELSE APPLY "choose" TO btn-cancel.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME in-file-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL in-file-name Dialog-Frame
ON HELP OF in-file-name IN FRAME Dialog-Frame /* Import From File */
DO:
   def var ls-filename as cha no-undo.
   def var ll-ok as log no-undo.
   
   system-dialog get-file ls-filename 
                 title "Select File to insert"
                 filters "Excel File (*.csv) " "*.csv",
                         "Text File  (*.txt) " "*.txt",
                         "All Files    (*.*) " "*.*"
                 initial-dir "c:\"
                 MUST-EXIST
                 USE-FILENAME
                 UPDATE ll-ok.
      
    IF ll-ok THEN self:screen-value = ls-filename.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL in-file-name Dialog-Frame
ON LEAVE OF in-file-name IN FRAME Dialog-Frame /* Import From File */
DO:
  IF LASTKEY = -1 THEN RETURN.
  ASSIGN in-file-name.

  IF SEARCH(in-file-name) eq ? THEN
  DO:
    MESSAGE "ERROR: Could Not Find " in-file-name "!" VIEW-AS ALERT-BOX ERROR.
    RETURN NO-apply.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb-clear-before-import
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb-clear-before-import Dialog-Frame
ON VALUE-CHANGED OF tb-clear-before-import IN FRAME Dialog-Frame /* Zero out all budgets before uploading new budgets? */
DO:

  IF NOT ll-secure THEN RUN sys/ref/d-passwd.w (2, OUTPUT ll-secure).

  IF NOT ll-secure THEN DO:
      SELF:SCREEN-VALUE = "NO".
      RETURN NO-APPLY.

  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:

DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
 
  RUN enable_UI.

  lv-budget-year:SCREEN-VALUE = STRING(YEAR(TODAY)).

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.

RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY tb-clear-before-import lv-budget-year in-file-name 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-17 RECT-41 tb-clear-before-import lv-budget-year in-file-name 
         bt-search-file btn-import btn-cancel 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process Dialog-Frame 
PROCEDURE run-process :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR qtr-file-name  AS char NO-UNDO.
   DEF VAR lv-input AS CHAR NO-UNDO.
   DEF VAR lv-index AS INT NO-UNDO.
   DEF VAR v-yend AS INT NO-UNDO.
   DEF VAR i-amt AS INT NO-UNDO.
   DEF VAR i-msf AS INT NO-UNDO.
   DEF VAR i-ton AS INT NO-UNDO.
   
   DO WITH FRAME {&FRAME-NAME}:

      SESSION:SET-WAIT-STATE("general").

      EMPTY TEMP-TABLE tt-bud.

      FIND FIRST company WHERE
           company.company = cocode
           NO-LOCK NO-ERROR.

      ASSIGN
         v-yend = IF AVAIL company THEN company.yend-off ELSE 12
         i-amt = IF v-yend EQ 13 THEN 40 ELSE 37
         i-msf = IF v-yend EQ 13 THEN 41 ELSE 38
         i-ton = IF v-yend EQ 13 THEN 42 ELSE 39.

      RELEASE company.

      ASSIGN
         in-file-name = SEARCH(in-file-name)
         qtr-file-name = in-file-name.
      IF tb-clear-before-import THEN DO:
         FOR EACH smanbcst WHERE
              smanbcst.company = cocode AND
              smanbcst.budget-yr = lv-budget-year 
    /*          smanbcst.budget-period = i AND */
    /*          smanbcst.procat = tt-bud.procat AND */
    /*          smanbcst.cust-no = tt-bud.cust-no */
              EXCLUSIVE-LOCK:
        
    
             FOR EACH smanbugt WHERE
                  smanbugt.company = cocode AND
                  smanbugt.sman = smanbcst.sman AND
                  smanbugt.budget-yr = lv-budget-year
                  EXCLUSIVE-LOCK:
                 DELETE smanbugt.
             END.

             DELETE smanbcst.
         END.
      END.

      INPUT FROM VALUE(qtr-file-name) no-echo.

      repeat:
         IMPORT lv-input.
         
         IF ENTRY(1,lv-input) NE "Salesman" THEN
         DO:
            CREATE tt-bud.
            ASSIGN tt-bud.sman = ENTRY(1,lv-input)
                   tt-bud.cust-no = ENTRY(2,lv-input)
                   tt-bud.procat  = ENTRY(3,lv-input)
                   lv-index = 0.
            DO i = 4 TO i-amt BY 3:
               lv-index = lv-index + 1.
               tt-bud.amt[lv-index] = dec(ENTRY(i,lv-input)).
            END.
            lv-index = 0.
            DO i = 5 TO i-msf BY 3:
               lv-index = lv-index + 1.
               tt-bud.msf[lv-index] = dec(ENTRY(i,lv-input)).
            END.
            lv-index = 0.
            DO i = 6 TO i-ton BY 3:
               lv-index = lv-index + 1.
               tt-bud.tons[lv-index] = dec(ENTRY(i,lv-input)).
            END.
         END.
      END.

      INPUT CLOSE.

      FOR EACH tt-bud:

          IF NOT CAN-FIND(FIRST sman WHERE
             sman.company EQ cocode AND
             sman.sman EQ tt-bud.sman) OR
             NOT CAN-FIND(FIRST fgcat WHERE
             fgcat.company EQ cocode AND
             fgcat.procat EQ tt-bud.procat) OR
             NOT CAN-FIND(FIRST cust WHERE
             cust.company EQ cocode AND
             cust.cust-no EQ tt-bud.cust-no) THEN NEXT.

          DO i = 1 TO 12 /*v-yend*/:

             FIND FIRST smanbcst WHERE
                  smanbcst.company = cocode AND
                  smanbcst.sman = tt-bud.sman AND
                  smanbcst.budget-yr = lv-budget-year AND
                  smanbcst.budget-period = i AND
                  smanbcst.procat = tt-bud.procat AND
                  smanbcst.cust-no = tt-bud.cust-no
                  EXCLUSIVE-LOCK NO-ERROR.
            
             IF NOT AVAIL smanbcst THEN
             DO:
                CREATE smanbcst.
                ASSIGN smanbcst.company = cocode
                       smanbcst.sman = tt-bud.sman
                       smanbcst.budget-yr = lv-budget-year
                       smanbcst.budget-period = i
                       smanbcst.procat = tt-bud.procat
                       smanbcst.cust-no = tt-bud.cust-no.
             END.

             FIND FIRST smanbugt WHERE
                  smanbugt.company = cocode AND
                  smanbugt.sman = tt-bud.sman AND
                  smanbugt.budget-yr = lv-budget-year
                  EXCLUSIVE-LOCK NO-ERROR.

             IF AVAIL smanbugt THEN
             DO:
                ASSIGN 
                  smanbugt.budget-amt[i] = smanbugt.budget-amt[i] - smanbcst.budget-amt
                  smanbugt.msf[i] = smanbugt.msf[i] - smanbcst.msf
                  smanbugt.ton[i] = smanbugt.ton[i] - smanbcst.ton.
             END.
             ELSE
             DO:
                CREATE smanbugt.
                ASSIGN smanbugt.company = cocode
                       smanbugt.sman = tt-bud.sman
                       smanbugt.budget-yr = lv-budget-year.
             END.

             FIND FIRST smanbcat WHERE
                  smanbcat.company = cocode AND
                  smanbcat.sman = tt-bud.sman AND
                  smanbcat.budget-yr = lv-budget-year AND
                  smanbcat.budget-period = i AND
                  smanbcat.procat = tt-bud.procat
                  EXCLUSIVE-LOCK NO-ERROR.

             IF AVAIL smanbcat THEN
             DO:
                ASSIGN
                  smanbcat.budget-amt = smanbcat.budget-amt - smanbcst.budget-amt
                  smanbcat.msf = smanbcat.msf - smanbcst.msf
                  smanbcat.tons = smanbcat.tons - smanbcst.ton.
             END.
             ELSE
             DO:
                CREATE smanbcat.
                ASSIGN smanbcat.company = cocode
                       smanbcat.sman = tt-bud.sman
                       smanbcat.budget-yr = lv-budget-year
                       smanbcat.budget-period = i
                       smanbcat.procat = tt-bud.procat.
             END.

             ASSIGN
               smanbcst.budget-amt = tt-bud.amt[i]
               smanbcst.msf = tt-bud.msf[i]
               smanbcst.ton = tt-bud.tons[i] 
               smanbugt.budget-amt[i] = smanbugt.budget-amt[i] + tt-bud.amt[i]
               smanbugt.msf[i] = smanbugt.msf[i] + tt-bud.msf[i]
               smanbugt.ton[i] = smanbugt.ton[i] + tt-bud.tons[i]
               smanbcat.budget-amt = smanbcat.budget-amt + tt-bud.amt[i]
               smanbcat.msf = smanbcat.msf + tt-bud.msf[i]
               smanbcat.tons = smanbcat.tons + tt-bud.tons[i].

             RELEASE smanbugt.
             RELEASE smanbcst.
             RELEASE smanbcat.
          END. /* DO i = 1 to v-yend*/
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

