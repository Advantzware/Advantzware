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
def input   param icJobNo       as char   no-undo.
def input   param iiJobNo2      as inte   no-undo.
def input   param iiForm        as inte   no-undo.
def output  param ocMillNum1    as char   no-undo.
def output  param ocMillNum2    as char   no-undo.
def output  param ocMillNum3    as char   no-undo.
def output  param ocMillNum4    as char   no-undo.
def output  param olInkProcess  as log    no-undo.
DEF OUTPUT  PARAM olPoly        AS LOG    NO-UNDO.


/* def output param ocMillNum1 as char   no-undo. */
/* def output param ocMillNum2 as char   no-undo. */
/* def output param ocMillNum3 as char   no-undo. */
/* def output param ocMillNum4 as char   no-undo. */
/* def output param olInkProc  as log    no-undo. */
/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.

DEF WORKFILE w-jm
   FIELD d-seq    LIKE mach.d-seq
   FIELD rec-id   AS   RECID.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES job-mat job-mch

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH job-mat SHARE-LOCK, ~
      EACH job-mch WHERE TRUE /* Join to job-mat incomplete */ SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH job-mat SHARE-LOCK, ~
      EACH job-mch WHERE TRUE /* Join to job-mat incomplete */ SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame job-mat job-mch
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame job-mat
&Scoped-define SECOND-TABLE-IN-QUERY-Dialog-Frame job-mch


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tb_InkProcess Btn_OK Btn_Cancel fi_bmill_no1 ~
fi_bmill_no2 fi_bmill_no3 fi_bmill_no4 RECT-43 tb_Poly 
&Scoped-Define DISPLAYED-OBJECTS tb_InkProcess fi_job-no fi_bmill_no1 ~
fi_bmill_no2 fi_bmill_no3 fi_bmill_no4 fi_Form tb_Poly 

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

DEFINE VARIABLE fi_bmill_no1 AS CHARACTER FORMAT "x(9)" 
     LABEL "Board Mill #1" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fi_bmill_no2 AS CHARACTER FORMAT "x(9)" 
     LABEL "Board Mill #2" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fi_bmill_no3 AS CHARACTER FORMAT "x(9)" 
     LABEL "Board Mill #3" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fi_bmill_no4 AS CHARACTER FORMAT "x(9)" 
     LABEL "Board Mill #4" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fi_Form AS CHARACTER FORMAT "x(9)" 
     LABEL "Form #" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE fi_job-no AS CHARACTER FORMAT "x(9)" 
     LABEL "Job#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-43
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 69.4 BY 10.24.

DEFINE VARIABLE tb_InkProcess AS LOGICAL INITIAL no 
     LABEL "Ink Process?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23.4 BY .81 NO-UNDO.

DEFINE VARIABLE tb_Poly AS LOGICAL INITIAL no 
     LABEL "Poly?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23.4 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      job-mat, 
      job-mch SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     tb_InkProcess AT ROW 9.33 COL 29
     fi_job-no AT ROW 1.48 COL 27.4 COLON-ALIGNED HELP
          "Enter Job Number."
     job-mat.frm AT ROW 4.57 COL 48 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     job-mat.rm-i-no AT ROW 1.48 COL 45 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     job-mch.run-qty AT ROW 2.91 COL 45 COLON-ALIGNED NO-LABEL FORMAT ">>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     Btn_OK AT ROW 11.62 COL 18
     Btn_Cancel AT ROW 11.62 COL 41.2
     fi_bmill_no1 AT ROW 4.43 COL 27.4 COLON-ALIGNED HELP
          "Enter Job Number."
     fi_bmill_no2 AT ROW 5.62 COL 27.4 COLON-ALIGNED HELP
          "Enter Job Number."
     fi_bmill_no3 AT ROW 6.81 COL 27.4 COLON-ALIGNED HELP
          "Enter Job Number."
     fi_bmill_no4 AT ROW 8 COL 27.4 COLON-ALIGNED HELP
          "Enter Job Number."
     fi_Form AT ROW 2.62 COL 27.4 COLON-ALIGNED HELP
          "Enter Job Number."
     tb_Poly AT ROW 10.29 COL 29 WIDGET-ID 2
     RECT-43 AT ROW 1.1 COL 2
     SPACE(0.00) SKIP(1.46)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Edit Board Mills  for Job/Form"
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
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi_Form IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_job-no IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN job-mat.frm IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       job-mat.frm:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN job-mat.rm-i-no IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE EXP-LABEL                                       */
/* SETTINGS FOR FILL-IN job-mch.run-qty IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE EXP-LABEL EXP-FORMAT                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

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
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Edit Board Mills  for Job/Form */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO: 

/*   DEF VAR ld AS DEC NO-UNDO.                            */
/*                                                         */
  assign
    ocMillNum1    = fi_bmill_no1:screen-value
    ocMillNum2    = fi_bmill_no2:screen-value
    ocMillNum3    = fi_bmill_no3:screen-value
    ocMillNum4    = fi_bmill_no4:screen-value
    olInkProcess  = logical (tb_InkProcess:screen-value)
    olPoly        = logical (tb_Poly:screen-value).

/*   DO WITH FRAME {&FRAME-NAME}:                             */
/*     ld = DEC(job-mch.run-qty:SCREEN-VALUE) * job-mch.n-on. */
/*                                                            */
/*     RUN jc/machshts.p (ROWID(job-mat), ld, 0).             */
/*   END.                                                     */
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

/*   FIND job WHERE ROWID(job) EQ ip-rowid NO-LOCK NO-ERROR. */

/* /*   IF AVAIL job-hdr THEN DO: */                                              */
/*   for first job-hdr of job:                                                    */
/*                                                                                */
/*     FOR EACH job-mch                                                           */
/*         WHERE job-mch.company EQ job-hdr.company                               */
/*           AND job-mch.job     EQ job-hdr.job                                   */
/*           AND job-mch.job-no  EQ job-hdr.job-no                                */
/*           AND job-mch.job-no2 EQ job-hdr.job-no2                               */
/*         USE-INDEX line-idx:                                                    */
/*                                                                                */
/*       FIND FIRST mach                                                          */
/*           {sys/look/machW.i}                                                    */
/*             AND mach.m-code EQ job-mch.m-code                                  */
/*           NO-LOCK NO-ERROR.                                                    */
/*       CREATE w-jm.                                                             */
/*       ASSIGN                                                                   */
/*        w-jm.d-seq  = IF AVAIL mach THEN mach.d-seq ELSE 0                      */
/*        w-jm.rec-id = RECID(job-mch).                                           */
/*     END.                                                                       */
/*                                                                                */
/*     i = 0.                                                                     */
/*     FOR EACH w-jm,                                                             */
/*         FIRST job-mch WHERE RECID(job-mch) EQ w-jm.rec-id                      */
/*                                                                                */
/*         BY job-mch.frm                                                         */
/*         BY w-jm.d-seq                                                          */
/*         BY job-mch.blank-no                                                    */
/*         BY job-mch.pass:                                                       */
/*                                                                                */
/*       i = i + 1.                                                               */
/*       job-mch.line = i.                                                        */
/*     END.                                                                       */
/*                                                                                */
/*     FOR EACH job-mch                                                           */
/*         WHERE job-mch.company EQ job-hdr.company                               */
/*           AND job-mch.job     EQ job-hdr.job                                   */
/*           AND job-mch.job-no  EQ job-hdr.job-no                                */
/*           AND job-mch.job-no2 EQ job-hdr.job-no2                               */
/*           AND (job-mch.frm    EQ job-hdr.frm OR                                */
/*                CAN-FIND(FIRST est                                              */
/*                         WHERE est.company   EQ job-hdr.company                 */
/*                           AND est.est-no    EQ job-hdr.est-no                  */
/*                           AND (est.est-type EQ 2 OR est.est-type EQ 6)))       */
/*         NO-LOCK,                                                               */
/*         EACH job-mat                                                           */
/*         WHERE job-mat.company EQ job-mch.company                               */
/*           AND job-mat.job     EQ job-mch.job                                   */
/*           AND job-mat.job-no  EQ job-mch.job-no                                */
/*           AND job-mat.job-no2 EQ job-mch.job-no2                               */
/*           AND job-mat.frm     EQ job-mch.frm                                   */
/*         NO-LOCK,                                                               */
/*         FIRST item                                                             */
/*         WHERE item.company EQ job-mat.company                                  */
/*           AND item.i-no    EQ job-mat.rm-i-no                                  */
/*           AND INDEX("1234BPR",item.mat-type) GT 0                              */
/*         NO-LOCK                                                                */
/*         BREAK BY job-mch.frm                                                   */
/*               BY job-mch.line                                                  */
/*               BY job-mat.j-no:                                                 */
/*                                                                                */
/*       IF FIRST-OF(job-mch.frm) THEN DO:                                        */
/*         fi_job-no = TRIM(job-mch.job-no) + "-" + STRING(job-mch.job-no2,"99"). */

        RUN enable_UI.

        assign
          fi_job-no:screen-value  = icJobNo + '-' + string (iiJobNo2, '99')
            fi_Form:screen-value  = string (iiForm).
 
        apply 'entry':u to fi_bmill_no1.

        WAIT-FOR GO OF FRAME {&FRAME-NAME}.
/*       END. */
/*     END.   */
/*   END.     */
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
  DISPLAY tb_InkProcess fi_job-no fi_bmill_no1 fi_bmill_no2 fi_bmill_no3 
          fi_bmill_no4 fi_Form tb_Poly 
      WITH FRAME Dialog-Frame.
  ENABLE tb_InkProcess Btn_OK Btn_Cancel fi_bmill_no1 fi_bmill_no2 fi_bmill_no3 
         fi_bmill_no4 RECT-43 tb_Poly 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

