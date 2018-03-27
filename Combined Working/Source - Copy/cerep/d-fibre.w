&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.

DEF WORKFILE w-jm
   FIELD d-seq    LIKE mach.d-seq
   FIELD rec-id   AS   RECID.

DEF TEMP-TABLE ttItemList
    FIELD i-no LIKE job-hdr.i-no
    FIELD part-no LIKE itemfg.part-no.

DEF BUFFER bf-job-hdr FOR job-hdr.
DEF BUFFER bf-itemfg FOR itemfg.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-4

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttItemList job-mat job-mch

/* Definitions for BROWSE BROWSE-4                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-4 ttItemList.i-no ttItemList.part-no   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-4   
&Scoped-define SELF-NAME BROWSE-4
&Scoped-define QUERY-STRING-BROWSE-4 FOR EACH ttItemList
&Scoped-define OPEN-QUERY-BROWSE-4 OPEN QUERY {&SELF-NAME} FOR EACH ttItemList.
&Scoped-define TABLES-IN-QUERY-BROWSE-4 ttItemList
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-4 ttItemList


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame job-mat.frm job-mat.rm-i-no ~
job-mch.run-qty 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame job-mch.run-qty 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame job-mch
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame job-mch
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-4}
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH job-mat SHARE-LOCK, ~
      EACH job-mch WHERE TRUE /* Join to job-mat incomplete */ SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH job-mat SHARE-LOCK, ~
      EACH job-mch WHERE TRUE /* Join to job-mat incomplete */ SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame job-mat job-mch
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame job-mat
&Scoped-define SECOND-TABLE-IN-QUERY-Dialog-Frame job-mch


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS job-mch.run-qty 
&Scoped-define ENABLED-TABLES job-mch
&Scoped-define FIRST-ENABLED-TABLE job-mch
&Scoped-Define ENABLED-OBJECTS BROWSE-4 Btn_OK Btn_Cancel RECT-42 
&Scoped-Define DISPLAYED-FIELDS job-mat.frm job-mat.rm-i-no job-mch.run-qty 
&Scoped-define DISPLAYED-TABLES job-mat job-mch
&Scoped-define FIRST-DISPLAYED-TABLE job-mat
&Scoped-define SECOND-DISPLAYED-TABLE job-mch
&Scoped-Define DISPLAYED-OBJECTS fi_job-no 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fi_job-no AS CHARACTER FORMAT "x(9)" 
     LABEL "Job#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-42
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54 BY 5.71.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-4 FOR 
      ttItemList SCROLLING.

DEFINE QUERY Dialog-Frame FOR 
      job-mat, 
      job-mch SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-4 Dialog-Frame _FREEFORM
  QUERY BROWSE-4 DISPLAY
      ttItemList.i-no FORMAT "X(15)" LABEL "FG Item #" WIDTH 23
      ttItemList.part-no FORMAT "X(30)" LABEL "Customer Part #"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 55 BY 9.29
         FONT 6 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-4 AT ROW 1.24 COL 56 WIDGET-ID 100
     fi_job-no AT ROW 1.48 COL 28 COLON-ALIGNED HELP
          "Enter Job Number."
     job-mat.frm AT ROW 2.67 COL 28 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     job-mat.rm-i-no AT ROW 3.86 COL 28 COLON-ALIGNED
          LABEL "RM Item#"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     job-mch.run-qty AT ROW 5.05 COL 28 COLON-ALIGNED
          LABEL "Estimated Sheets" FORMAT ">>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     Btn_OK AT ROW 11 COL 36
     Btn_Cancel AT ROW 11 COL 64
     RECT-42 AT ROW 1 COL 1
     SPACE(58.79) SKIP(5.76)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Edit Estimated Sheets for Job/Form"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB BROWSE-4 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi_job-no IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN job-mat.frm IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN job-mat.rm-i-no IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN job-mch.run-qty IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-4
/* Query rebuild information for BROWSE BROWSE-4
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttItemList.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "asi.job-mat,asi.job-mch WHERE asi.job-mat ..."
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Edit Estimated Sheets for Job/Form */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  DEF VAR ld AS DEC NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ld = DEC(job-mch.run-qty:SCREEN-VALUE) * job-mch.n-on.

    RUN jc/machshts.p (ROWID(job-mat), ld, 0).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-4
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

  IF AVAIL job-hdr THEN DO:
      FOR EACH bf-job-hdr 
        WHERE bf-job-hdr.company EQ job-hdr.company
          AND bf-job-hdr.job     EQ job-hdr.job
          AND bf-job-hdr.job-no  EQ job-hdr.job-no
          AND bf-job-hdr.job-no2 EQ job-hdr.job-no2
          AND bf-job-hdr.frm     EQ job-hdr.frm
          NO-LOCK:
          FIND FIRST bf-itemfg NO-LOCK
            WHERE bf-itemfg.company EQ bf-job-hdr.company
                AND bf-itemfg.i-no    EQ bf-job-hdr.i-no
            NO-ERROR.
          CREATE ttItemList.
          ASSIGN 
              ttItemList.i-no = bf-job-hdr.i-no
              ttItemList.part-no = IF AVAIL bf-itemfg THEN bf-itemfg.part-no ELSE "". 
      END.
    FOR EACH job-mch
        WHERE job-mch.company EQ job-hdr.company
          AND job-mch.job     EQ job-hdr.job
          AND job-mch.job-no  EQ job-hdr.job-no
          AND job-mch.job-no2 EQ job-hdr.job-no2
        USE-INDEX line-idx:

      FIND FIRST mach
          {sys/look/machW.i}
            AND mach.m-code EQ job-mch.m-code
          NO-LOCK NO-ERROR.
      CREATE w-jm.
      ASSIGN
       w-jm.d-seq  = IF AVAIL mach THEN mach.d-seq ELSE 0
       w-jm.rec-id = RECID(job-mch).
    END.

    i = 0.
    FOR EACH w-jm, 
        FIRST job-mch WHERE RECID(job-mch) EQ w-jm.rec-id

        BY job-mch.frm
        BY w-jm.d-seq
        BY job-mch.blank-no
        BY job-mch.pass:
  
      i = i + 1.
      job-mch.line = i.
    END.

    FOR EACH job-mch
        WHERE job-mch.company EQ job-hdr.company
          AND job-mch.job     EQ job-hdr.job
          AND job-mch.job-no  EQ job-hdr.job-no
          AND job-mch.job-no2 EQ job-hdr.job-no2
          AND (job-mch.frm    EQ job-hdr.frm OR
               CAN-FIND(FIRST est
                        WHERE est.company   EQ job-hdr.company
                          AND est.est-no    EQ job-hdr.est-no
                          AND (est.est-type EQ 2 OR est.est-type EQ 6)))
        NO-LOCK,
        EACH job-mat
        WHERE job-mat.company EQ job-mch.company
          AND job-mat.job     EQ job-mch.job
          AND job-mat.job-no  EQ job-mch.job-no
          AND job-mat.job-no2 EQ job-mch.job-no2
          AND job-mat.frm     EQ job-mch.frm
        NO-LOCK,
        FIRST item
        WHERE item.company EQ job-mat.company
          AND item.i-no    EQ job-mat.rm-i-no
          AND INDEX("1234BPR",item.mat-type) GT 0
        NO-LOCK
        BREAK BY job-mch.frm
              BY job-mch.line
              BY job-mat.j-no:

      IF FIRST-OF(job-mch.frm) THEN DO:
        fi_job-no = TRIM(job-mch.job-no) + "-" + STRING(job-mch.job-no2,"99").

        RUN enable_UI.
 
        WAIT-FOR GO OF FRAME {&FRAME-NAME}.
      END.
    END.
  END.
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
  DISPLAY fi_job-no 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE job-mat THEN 
    DISPLAY job-mat.frm job-mat.rm-i-no 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE job-mch THEN 
    DISPLAY job-mch.run-qty 
      WITH FRAME Dialog-Frame.
  ENABLE BROWSE-4 job-mch.run-qty Btn_OK Btn_Cancel RECT-42 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

