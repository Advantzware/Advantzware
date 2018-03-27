&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: d-artios.w
   
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-ref-rowid AS ROWID NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.

{cecrep/tt-artios.i}

DEF BUFFER d-fibre FOR reftable.
DEF BUFFER job-reft FOR reftable.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS v-see-dept v-see-plain Btn_OK RECT-1 
&Scoped-Define DISPLAYED-OBJECTS fi_job-no v-form-no v-blank-no v-see-dept ~
v-see-plain 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fi_job-no AS CHARACTER FORMAT "x(9)" 
     LABEL "Job#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE v-blank-no AS INTEGER FORMAT ">>9" INITIAL 1 
     LABEL "Blank" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE v-form-no AS INTEGER FORMAT ">>9" INITIAL 1 
     LABEL "Form" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 30 BY 3.05.

DEFINE VARIABLE v-see-dept AS LOGICAL INITIAL no 
     LABEL "See Dept. Notes" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE v-see-plain AS LOGICAL INITIAL no 
     LABEL "See Plain" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fi_job-no AT ROW 1.24 COL 10 COLON-ALIGNED HELP
          "Enter Job Number."
     v-form-no AT ROW 1.24 COL 41 COLON-ALIGNED
     v-blank-no AT ROW 1.24 COL 61 COLON-ALIGNED
     v-see-dept AT ROW 3.62 COL 12
     v-see-plain AT ROW 4.57 COL 12
     Btn_OK AT ROW 7.71 COL 11.8
     RECT-1 AT ROW 2.95 COL 11
     SPACE(51.79) SKIP(3.47)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Options"
         DEFAULT-BUTTON Btn_OK.


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

/* SETTINGS FOR FILL-IN fi_job-no IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-blank-no IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-form-no IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Options */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
   ASSIGN {&displayed-objects}.

   FIND FIRST tt-artios WHERE
        tt-artios.tt-job-no = job-hdr.job-no AND
        tt-artios.tt-job-no2 = job-hdr.job-no2 AND
        tt-artios.tt-frm = v-form-no AND
        tt-artios.tt-blank = v-blank-no
        NO-ERROR.

   IF NOT AVAIL tt-artios THEN DO:
      CREATE tt-artios.
      ASSIGN tt-artios.tt-job-no  = job-hdr.job-no
             tt-artios.tt-job-no2 = job-hdr.job-no2
             tt-artios.tt-frm     = v-form-no
             tt-artios.tt-blank   = v-blank-no.
   END.

   ASSIGN
      tt-artios.tt-see-dept   = v-see-dept
      tt-artios.tt-see-plain = v-see-plain.

   FIND FIRST d-fibre WHERE
        d-fibre.reftable EQ "cecrep/d-artios.w" AND
        d-fibre.company  EQ cocode AND
        d-fibre.loc      EQ TRIM(job-hdr.job-no) AND
        d-fibre.code     EQ STRING(job-hdr.job-no2,"9999999999") AND
        d-fibre.code2    EQ STRING(v-form-no,"999") AND
        d-fibre.val[1]   EQ v-blank-no
        NO-ERROR.

    IF NOT AVAIL d-fibre THEN DO:
      CREATE d-fibre.
      ASSIGN
       d-fibre.reftable = "cecrep/d-artios.w"
       d-fibre.company  = cocode
       d-fibre.loc      = TRIM(job-hdr.job-no)
       d-fibre.code     = STRING(job-hdr.job-no2,"9999999999")
       d-fibre.code2    = STRING(v-form-no,"999")
       d-fibre.val[1]   = v-blank-no.
    END.

    ASSIGN d-fibre.val[3] = INT(tt-artios.tt-see-dept)
           d-fibre.val[4] = INT(tt-artios.tt-see-plain).
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

  FIND job-hdr WHERE ROWID(job-hdr) EQ ip-rowid NO-LOCK NO-ERROR.
  FIND job-reft WHERE ROWID(job-reft) EQ ip-ref-rowid NO-LOCK NO-ERROR.
  IF AVAIL job-hdr THEN
     fi_job-no = TRIM(job-hdr.job-no) + "-" + STRING(job-hdr.job-no2,"99").

  IF AVAIL job-hdr THEN
  DO:
     FIND FIRST d-fibre WHERE
          d-fibre.reftable EQ "cecrep/d-artios.w"
          AND d-fibre.company  EQ cocode
          AND d-fibre.loc      EQ TRIM(job-hdr.job-no)
          AND d-fibre.code     EQ STRING(job-hdr.job-no2,"9999999999")
          AND d-fibre.code2    EQ string(job-reft.val[12],"999")  /*form-no*/
          AND d-fibre.val[1]   EQ job-reft.val[13]          /*blank-no*/
          NO-LOCK NO-ERROR.

     IF NOT AVAIL d-fibre THEN
        FIND FIRST d-fibre WHERE
          d-fibre.reftable EQ "cecrep/d-artios.w"
          AND d-fibre.company  EQ cocode
          AND d-fibre.loc      EQ TRIM(job-hdr.job-no)
          AND d-fibre.code     EQ STRING(job-hdr.job-no2,"9999999999")
          AND d-fibre.code2    EQ string(1,"999")  /*form-no*/
          AND d-fibre.val[1]   EQ 1          /*blank-no*/
          NO-LOCK NO-ERROR.

     IF AVAIL d-fibre THEN
        ASSIGN v-see-dept = LOGICAL(d-fibre.val[3])
               v-see-plain = LOGICAL(d-fibre.val[4]).

     IF AVAIL job-reft THEN
        ASSIGN v-form-no = job-reft.val[12]
               v-blank-no = job-reft.val[13].
  END.

  RUN enable_UI. 
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
  DISPLAY fi_job-no v-form-no v-blank-no v-see-dept v-see-plain 
      WITH FRAME Dialog-Frame.
  ENABLE v-see-dept v-see-plain Btn_OK RECT-1 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

