&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: completeJob.w

  Description: set status/complete job

  Input Parameters: ttblJob rowid

  Output Parameters: <none>

  Author: Ron Stark

  Created: 6.25.2004
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&IF DEFINED(jobTable) EQ 0 &THEN
&SCOPED-DEFINE jobTable ttblJob
&SCOPED-DEFINE buffTable buffJob
&ELSE
&SCOPED-DEFINE buffTable buffPending
&ENDIF

{schedule/scopDir.i}
{{&includes}/defBoard.i}

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipBoard AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE statusButton AS WIDGET-HANDLE NO-UNDO EXTENT 32.
DEFINE VARIABLE statusValue AS WIDGET-HANDLE NO-UNDO EXTENT 32.
DEFINE VARIABLE lastStatus AS INTEGER NO-UNDO.
DEFINE VARIABLE startPrep AS INTEGER NO-UNDO.
DEFINE VARIABLE endPrep AS INTEGER NO-UNDO.

{{&includes}/sharedVars.i}
{{&includes}/ttblJob.i}

/* configuration vars */
{{&includes}/configVars.i}
/* configuration version procedures */
{{&includes}/configVersion.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnComplete customCheckoffValue ~
completedCheckoffValue RECT-11 
&Scoped-Define DISPLAYED-OBJECTS customCheckoffValue completedCheckoffValue 

/* Custom List Definitions                                              */
/* checkoffButton,List-2,List-3,List-4,List-5,List-6                    */
&Scoped-define checkoffButton btnJobStatus btnPrepStatus btnStatus-1 ~
btnStatus-2 btnStatus-3 btnStatus-4 btnStatus-5 btnStatus-6 btnStatus-7 ~
btnStatus-8 btnStatus-9 btnStatus-10 btnStatus-11 btnStatus-12 btnStatus-13 ~
btnStatus-14 btnStatus-15 btnStatus-16 btnStatus-17 btnStatus-18 ~
btnStatus-19 btnStatus-20 btnStatus-21 btnStatus-22 btnStatus-23 ~
btnStatus-24 btnStatus-25 btnStatus-26 btnStatus-27 btnStatus-28 ~
btnStatus-29 btnStatus-30 customCheckoffValue completedCheckoffValue 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnComplete AUTO-GO 
     LABEL "&Quick Complete and Close" 
     SIZE 45 BY 1.14.

DEFINE BUTTON btnJobStatus 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnPrepStatus 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-1 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-10 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-11 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-12 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-13 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-14 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-15 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-16 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-17 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-18 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-19 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-2 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-20 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-21 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-22 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-23 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-24 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-25 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-26 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-27 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-28 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-29 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-3 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-30 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-4 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-5 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-6 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-7 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-8 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE BUTTON btnStatus-9 
     LABEL "%" 
     SIZE 30 BY 1.

DEFINE VARIABLE jobStatus AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 4 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE prepStatus AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 4 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-1 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-10 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-11 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-12 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-13 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-14 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-15 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-16 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-17 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-18 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-19 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-2 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-20 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-21 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-22 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-23 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-24 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-25 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-26 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-27 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-28 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-29 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-3 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-30 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-4 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-5 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-6 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-7 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-8 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE status-9 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 47 BY 2.62.

DEFINE VARIABLE completedCheckoffValue AS LOGICAL INITIAL no 
     LABEL "&Apply Completed Checkoff to Whole Job" 
     VIEW-AS TOGGLE-BOX
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE customCheckoffValue AS LOGICAL INITIAL no 
     LABEL "&Apply Custom Value Checkoff to Whole Job" 
     VIEW-AS TOGGLE-BOX
     SIZE 45 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnComplete AT ROW 1 COL 2
     btnJobStatus AT ROW 6.24 COL 2
     btnPrepStatus AT ROW 5.05 COL 2
     btnStatus-1 AT ROW 7.43 COL 2
     btnStatus-2 AT ROW 7.43 COL 2
     btnStatus-3 AT ROW 7.43 COL 2
     btnStatus-4 AT ROW 7.43 COL 2
     btnStatus-5 AT ROW 7.43 COL 2
     btnStatus-6 AT ROW 7.43 COL 2
     btnStatus-7 AT ROW 7.43 COL 2
     btnStatus-8 AT ROW 7.43 COL 2
     btnStatus-9 AT ROW 7.43 COL 2
     btnStatus-10 AT ROW 7.43 COL 2
     btnStatus-11 AT ROW 7.43 COL 2
     btnStatus-12 AT ROW 7.43 COL 2
     btnStatus-13 AT ROW 7.43 COL 2
     btnStatus-14 AT ROW 7.43 COL 2
     btnStatus-15 AT ROW 7.43 COL 2
     btnStatus-16 AT ROW 7.43 COL 2
     btnStatus-17 AT ROW 7.43 COL 2
     btnStatus-18 AT ROW 7.43 COL 2
     btnStatus-19 AT ROW 7.43 COL 2
     btnStatus-20 AT ROW 7.43 COL 2
     btnStatus-21 AT ROW 7.43 COL 2
     btnStatus-22 AT ROW 7.43 COL 2
     btnStatus-23 AT ROW 7.43 COL 2
     btnStatus-24 AT ROW 7.43 COL 2
     btnStatus-25 AT ROW 7.43 COL 2
     btnStatus-26 AT ROW 7.43 COL 2
     btnStatus-27 AT ROW 7.43 COL 2
     btnStatus-28 AT ROW 7.43 COL 2
     btnStatus-29 AT ROW 7.43 COL 2
     btnStatus-30 AT ROW 7.43 COL 2
     status-7 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-25 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-16 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-15 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-27 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-18 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-17 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-2 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-26 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-1 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-13 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-8 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-30 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-14 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-5 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-11 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-20 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-24 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-12 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-21 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-19 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-23 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-4 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-22 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-29 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-9 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-10 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-28 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-6 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     status-3 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     prepStatus AT ROW 5.05 COL 31 COLON-ALIGNED NO-LABEL
     jobStatus AT ROW 6.24 COL 31 COLON-ALIGNED NO-LABEL
     customCheckoffValue AT ROW 2.43 COL 2 HELP
          "Select to Apply Checkoff to Whole Job vs. Each Resource"
     completedCheckoffValue AT ROW 3.62 COL 2 HELP
          "Select to Apply Completed Checkoff to Whole Job vs. Each Resour"
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 7 .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     RECT-11 AT ROW 2.19 COL 1
     SPACE(0.00) SKIP(3.80)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 7 
         TITLE "Complete Job (Status)".


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
   Custom                                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnJobStatus IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnJobStatus:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnPrepStatus IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnPrepStatus:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-1 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-1:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-10 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-10:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-11 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-11:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-12 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-12:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-13 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-13:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-14 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-14:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-15 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-15:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-16 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-16:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-17 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-17:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-18 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-18:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-19 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-19:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-2 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-2:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-20 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-20:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-21 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-21:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-22 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-22:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-23 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-23:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-24 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-24:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-25 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-25:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-26 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-26:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-27 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-27:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-28 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-28:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-29 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-29:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-3 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-3:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-30 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-30:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-4 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-4:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-5 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-5:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-6 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-6:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-7 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-7:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-8 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-8:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnStatus-9 IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       btnStatus-9:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR TOGGLE-BOX completedCheckoffValue IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX customCheckoffValue IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN jobStatus IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       jobStatus:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN prepStatus IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       prepStatus:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-1 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-1:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-10 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-10:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-11 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-11:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-12 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-12:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-13 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-13:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-14 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-14:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-15 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-15:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-16 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-16:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-17 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-17:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-18 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-18:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-19 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-19:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-2 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-2:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-20 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-20:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-21 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-21:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-22 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-22:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-23 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-23:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-24 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-24:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-25 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-25:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-26 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-26:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-27 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-27:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-28 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-28:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-29 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-29:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-3 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-3:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-30 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-30:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-4 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-4:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-5 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-5:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-6 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-6:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-7 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-7:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-8 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-8:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN status-9 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       status-9:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Complete Job (Status) */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnComplete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnComplete Dialog-Frame
ON CHOOSE OF btnComplete IN FRAME Dialog-Frame /* Quick Complete and Close */
DO:
  IF ipBoard NE '{&Board}' THEN RETURN NO-APPLY.
  {&jobTable}.jobCompleted = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnJobStatus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnJobStatus Dialog-Frame
ON CHOOSE OF btnJobStatus IN FRAME Dialog-Frame /* % */
DO:
  IF ipBoard NE '{&Board}' THEN RETURN NO-APPLY.
  ASSIGN
    jobStatus = NOT jobStatus
    jobStatus:SCREEN-VALUE = STRING(jobStatus)
    jobStatus:BGCOLOR = IF jobStatus THEN 2 ELSE 4
    prepStatus:BGCOLOR = jobStatus:BGCOLOR
    {&jobTable}.prepCompleted = jobStatus
    {&jobTable}.jobCompleted = jobStatus.
  RUN setJobStatus (jobStatus).
  RUN setWholeJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrepStatus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrepStatus Dialog-Frame
ON CHOOSE OF btnPrepStatus IN FRAME Dialog-Frame /* % */
DO:
  IF ipBoard NE '{&Board}' THEN RETURN NO-APPLY.
  ASSIGN
    prepStatus = NOT prepStatus
    prepStatus:SCREEN-VALUE = STRING(prepStatus)
    prepStatus:BGCOLOR = IF prepStatus THEN 2 ELSE 4
    {&jobTable}.prepCompleted = prepStatus
    {&jobTable}.jobCompleted = NO.
  RUN setPrepStatus (prepStatus).
  RUN setCompletedStatus.
  RUN setStatusValues.
  RUN setWholeJob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-1 Dialog-Frame
ON CHOOSE OF btnStatus-1 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-10 Dialog-Frame
ON CHOOSE OF btnStatus-10 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 10}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-11 Dialog-Frame
ON CHOOSE OF btnStatus-11 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 11}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-12 Dialog-Frame
ON CHOOSE OF btnStatus-12 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 12}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-13 Dialog-Frame
ON CHOOSE OF btnStatus-13 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 13}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-14 Dialog-Frame
ON CHOOSE OF btnStatus-14 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 14}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-15 Dialog-Frame
ON CHOOSE OF btnStatus-15 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 15}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-16 Dialog-Frame
ON CHOOSE OF btnStatus-16 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 16}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-17
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-17 Dialog-Frame
ON CHOOSE OF btnStatus-17 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 17}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-18 Dialog-Frame
ON CHOOSE OF btnStatus-18 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 18}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-19 Dialog-Frame
ON CHOOSE OF btnStatus-19 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 19}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-2 Dialog-Frame
ON CHOOSE OF btnStatus-2 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-20 Dialog-Frame
ON CHOOSE OF btnStatus-20 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 20}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-21
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-21 Dialog-Frame
ON CHOOSE OF btnStatus-21 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 21}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-22
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-22 Dialog-Frame
ON CHOOSE OF btnStatus-22 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 22}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-23
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-23 Dialog-Frame
ON CHOOSE OF btnStatus-23 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 23}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-24
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-24 Dialog-Frame
ON CHOOSE OF btnStatus-24 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 24}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-25
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-25 Dialog-Frame
ON CHOOSE OF btnStatus-25 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 25}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-26
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-26 Dialog-Frame
ON CHOOSE OF btnStatus-26 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 26}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-27
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-27 Dialog-Frame
ON CHOOSE OF btnStatus-27 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 27}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-28
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-28 Dialog-Frame
ON CHOOSE OF btnStatus-28 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 28}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-29
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-29 Dialog-Frame
ON CHOOSE OF btnStatus-29 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 29}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-3 Dialog-Frame
ON CHOOSE OF btnStatus-3 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 3}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-30
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-30 Dialog-Frame
ON CHOOSE OF btnStatus-30 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 30}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-4 Dialog-Frame
ON CHOOSE OF btnStatus-4 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 4}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-5 Dialog-Frame
ON CHOOSE OF btnStatus-5 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 5}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-6 Dialog-Frame
ON CHOOSE OF btnStatus-6 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 6}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-7 Dialog-Frame
ON CHOOSE OF btnStatus-7 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 7}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-8 Dialog-Frame
ON CHOOSE OF btnStatus-8 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 8}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus-9 Dialog-Frame
ON CHOOSE OF btnStatus-9 IN FRAME Dialog-Frame /* % */
DO:
  {{&includes}/{&Board}/btnStatus.i 9}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME completedCheckoffValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL completedCheckoffValue Dialog-Frame
ON VALUE-CHANGED OF completedCheckoffValue IN FRAME Dialog-Frame /* Apply Completed Checkoff to Whole Job */
DO:
  ASSIGN {&SELF-NAME}
    completedCheckoff = {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME customCheckoffValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL customCheckoffValue Dialog-Frame
ON VALUE-CHANGED OF customCheckoffValue IN FRAME Dialog-Frame /* Apply Custom Value Checkoff to Whole Job */
DO:
  ASSIGN {&SELF-NAME}
    customCheckoff = {&SELF-NAME}.
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
  FIND {&jobTable} EXCLUSIVE-LOCK WHERE ROWID({&jobTable}) EQ ipRowID NO-ERROR.
  IF NOT AVAILABLE {&jobTable} THEN
  DO:
    MESSAGE 'No Job Selected on Board' VIEW-AS ALERT-BOX.
    RUN disable_UI.
    RETURN.
  END.
  FRAME {&FRAME-NAME}:TITLE = 'Job: ' + {&jobTable}.job + ' (' + {&jobTable}.resource + ')'.
  RUN getConfiguration.
  RUN createStatusObjects.
  RUN enable_UI.
  ASSIGN
    completedCheckOffValue:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(completedCheckoff)
    customCheckOffValue:SCREEN-VALUE = STRING(customCheckoff).
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createStatusObjects Dialog-Frame 
PROCEDURE createStatusObjects :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE yCoord AS INTEGER NO-UNDO INITIAL 85.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE k AS INTEGER NO-UNDO.

  DO i = 1 TO EXTENT(statusObject):
    IF statusObject[i] EQ '' THEN LEAVE.
  END. /* do i */
  j = (IF NUM-ENTRIES(customValueList) EQ 0 THEN 1
       ELSE NUM-ENTRIES(customValueList)) + 1.
  FRAME {&FRAME-NAME}:HEIGHT-PIXELS = (i + j) * 26 + 5.
  DO j = 1 TO i - 1:
    IF ENTRY(1,statusObject[j]) EQ '' THEN DO:
      DO k = 1 TO NUM-ENTRIES(customValueList) - 1:
        IF customLabel[k] NE '' THEN DO:
          endPrep = endPrep + 1.
          RUN setObject (j + k - 1,customLabel[k],
                         ENTRY(2,statusObject[j]),INPUT-OUTPUT yCoord).
        END.
      END. /* do k */
      RUN setObject (31,'****** PREP ******','Ready/Pending',INPUT-OUTPUT yCoord).
      ASSIGN
        startPrep = j
        /* endPrep = j + k - 2 */
        yCoord = yCoord + 10.
    END. /* if entry(1) */
    ELSE
    RUN setObject (j + endPrep - startPrep,ENTRY(1,statusObject[j]),
                   ENTRY(2,statusObject[j]),INPUT-OUTPUT yCoord).
  END. /* do j */
  yCoord = yCoord + 10.
  RUN setObject (32,'*** COMPLETE JOB ***','Completed/Pending',INPUT-OUTPUT yCoord).
  FRAME {&FRAME-NAME}:HEIGHT-PIXELS = yCoord + 26 + 15.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY customCheckoffValue completedCheckoffValue 
      WITH FRAME Dialog-Frame.
  ENABLE btnComplete customCheckoffValue completedCheckoffValue RECT-11 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getConfiguration Dialog-Frame 
PROCEDURE getConfiguration :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/config.dat')) NO-ECHO.
  IMPORT version.
  INPUT CLOSE.
  RUN VALUE('get' + version).
  RUN setStatusValues.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setCompletedStatus Dialog-Frame 
PROCEDURE setCompletedStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  ASSIGN
    {&jobTable}.jobCompleted = NO
    {&jobTable}.prepCompleted = NO.
  DO i = startPrep TO endPrep:
    IF NOT {&jobTable}.jobStatus[i] THEN RETURN.
  END.
  {&jobTable}.prepCompleted = YES.
  DO i = 1 TO lastStatus:
    IF NOT statusButton[i]:SENSITIVE THEN NEXT.
    IF NOT {&jobTable}.jobStatus[i] THEN RETURN.
  END.
  {&jobTable}.jobCompleted = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setJobStatus Dialog-Frame 
PROCEDURE setJobStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipStatus AS LOGICAL NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DO i = 1 TO lastStatus:
    {&jobTable}.jobStatus[i] = ipStatus.
  END.
  RUN setStatusValues.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setObject Dialog-Frame 
PROCEDURE setObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipIdx AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipLabel AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipFormat AS CHARACTER NO-UNDO.

  DEFINE INPUT-OUTPUT PARAMETER ipY AS INTEGER NO-UNDO.

  IF ipIdx LE EXTENT({&jobTable}.jobStatus) AND ipIdx GT lastStatus THEN
  lastStatus = ipIdx.
  ASSIGN
    statusButton[ipIdx]:Y = ipY
    statusButton[ipIdx]:LABEL = REPLACE(statusButton[ipIdx]:LABEL,'%',ipLabel)
    statusButton[ipIdx]:HIDDEN = NO
    statusButton[ipIdx]:SENSITIVE = YES
    statusButton[ipIdx]:PRIVATE-DATA = STRING(ipIdx)
    statusValue[ipIdx]:Y = ipY
    statusValue[ipIdx]:FORMAT = ipFormat
    statusValue[ipIdx]:HIDDEN = NO
    ipY = ipY + 26.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setPrepStatus Dialog-Frame 
PROCEDURE setPrepStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipStatus AS LOGICAL NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DO i = startPrep TO endPrep:
    {&jobTable}.jobStatus[i] = ipStatus.
  END.
  RUN setStatusValues.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setStatusValues Dialog-Frame 
PROCEDURE setStatusValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      {{&includes}/{&Board}/jobStatus.i 1}
      {{&includes}/{&Board}/jobStatus.i 2}
      {{&includes}/{&Board}/jobStatus.i 3}
      {{&includes}/{&Board}/jobStatus.i 4}
      {{&includes}/{&Board}/jobStatus.i 5}
      {{&includes}/{&Board}/jobStatus.i 6}
      {{&includes}/{&Board}/jobStatus.i 7}
      {{&includes}/{&Board}/jobStatus.i 8}
      {{&includes}/{&Board}/jobStatus.i 9}
      {{&includes}/{&Board}/jobStatus.i 10}.
    ASSIGN
      {{&includes}/{&Board}/jobStatus.i 11}
      {{&includes}/{&Board}/jobStatus.i 12}
      {{&includes}/{&Board}/jobStatus.i 13}
      {{&includes}/{&Board}/jobStatus.i 14}
      {{&includes}/{&Board}/jobStatus.i 15}
      {{&includes}/{&Board}/jobStatus.i 16}
      {{&includes}/{&Board}/jobStatus.i 17}
      {{&includes}/{&Board}/jobStatus.i 18}
      {{&includes}/{&Board}/jobStatus.i 19}
      {{&includes}/{&Board}/jobStatus.i 20}.
    ASSIGN
      {{&includes}/{&Board}/jobStatus.i 21}
      {{&includes}/{&Board}/jobStatus.i 22}
      {{&includes}/{&Board}/jobStatus.i 23}
      {{&includes}/{&Board}/jobStatus.i 24}
      {{&includes}/{&Board}/jobStatus.i 25}
      {{&includes}/{&Board}/jobStatus.i 26}
      {{&includes}/{&Board}/jobStatus.i 27}
      {{&includes}/{&Board}/jobStatus.i 28}
      {{&includes}/{&Board}/jobStatus.i 29}
      {{&includes}/{&Board}/jobStatus.i 30}.
    ASSIGN
      prepStatus = {&jobTable}.prepCompleted
      prepStatus:SCREEN-VALUE = STRING(prepStatus)
      prepStatus:BGCOLOR = IF prepStatus THEN 2 ELSE 4
      statusButton[31] = btnPrepStatus:HANDLE
      statusValue[31] = prepStatus:HANDLE
      jobStatus = {&jobTable}.jobCompleted
      jobStatus:SCREEN-VALUE = STRING(jobStatus)
      jobStatus:BGCOLOR = IF jobStatus THEN 2 ELSE 4
      statusButton[32] = btnJobStatus:HANDLE
      statusValue[32] = jobStatus:HANDLE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setWholeJob Dialog-Frame 
PROCEDURE setWholeJob :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  IF customCheckoff THEN
  FOR EACH {&buffTable} EXCLUSIVE-LOCK
      WHERE {&buffTable}.job EQ {&jobTable}.job
        AND ROWID({&buffTable}) NE ROWID({&jobTable}):
    DO i = 1 TO EXTENT({&jobTable}.jobStatus):
      {&buffTable}.jobStatus[i] = {&jobTable}.jobStatus[i].
    END.
    {&buffTable}.prepCompleted = {&jobTable}.prepCompleted.
    IF completedCheckoff THEN
    {&buffTable}.jobCompleted = {&jobTable}.jobCompleted.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

