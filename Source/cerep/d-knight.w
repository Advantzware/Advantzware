&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
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
DEF INPUT PARAMETER ip-rowid-eb     AS ROWID NO-UNDO.

DEF INPUT PARAMETER ip-job-no       AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-job-no2      AS INT NO-UNDO.
DEF INPUT PARAMETER ip-frm          AS INT NO-UNDO.
DEF INPUT PARAMETER ip-i-no         AS CHAR NO-UNDO.

DEF BUFFER b-eb FOR eb.
DEF BUFFER b-ef FOR ef.
DEF VAR    lv-rowid   AS ROWID.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i new shared}

ASSIGN
   cocode = g_company
   locode = g_loc.

{ cerep/tt-wrk-ink.i "SHARED" }

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES wrk-ink

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 wrk-ink.rm-i-no wrk-ink.i-name wrk-ink.i-qty wrk-ink.qty-uom   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 wrk-ink.i-qty   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-1 wrk-ink
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-1 wrk-ink
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH wrk-ink WHERE wrk-ink.form-no = eb.form-no                                            AND wrk-ink.blank-no = job-hdr.blank-no                                             BY wrk-ink.i-code
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH wrk-ink WHERE wrk-ink.form-no = eb.form-no                                            AND wrk-ink.blank-no = job-hdr.blank-no                                             BY wrk-ink.i-code.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 wrk-ink
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 wrk-ink


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS v-i-no v-form v-blank 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 v-i-no 

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

DEFINE VARIABLE v-blank AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Blank#" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE v-form AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Form#" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE v-i-no AS CHARACTER FORMAT "X(15)":U INITIAL "0" 
     LABEL "FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      wrk-ink SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _FREEFORM
  QUERY BROWSE-1 DISPLAY
      wrk-ink.rm-i-no
wrk-ink.i-name
wrk-ink.i-qty 
wrk-ink.qty-uom


ENABLE  wrk-ink.i-qty
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 92.2 BY 15
         FONT 6 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     v-i-no AT ROW 1.19 COL 7
     v-form AT ROW 2.19 COL 10.4
     v-blank AT ROW 3.19 COL 9.4
     BROWSE-1 AT ROW 4.33 COL 5.8
     Btn_OK AT ROW 19.81 COL 34
     Btn_Cancel AT ROW 19.81 COL 52
     SPACE(36.19) SKIP(1.09)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "More Information for Factory Ticket"
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
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-1 v-blank Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN v-blank IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN v-form IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN v-i-no IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 1                                                  */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH wrk-ink WHERE wrk-ink.form-no = eb.form-no
                                           AND wrk-ink.blank-no = job-hdr.blank-no
                                            BY wrk-ink.i-code
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* More Information for Factory Ticket */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
   DEF VAR v-li AS INT NO-UNDO.
   DEF VAR v-t-win AS DEC DECIMALS 4 NO-UNDO.
   DEF BUFFER b-item FOR ITEM.
   
   IF eb.est-type EQ 1 THEN
   do v-li = 1 TO 4:
      find first b-item WHERE
           b-item.company EQ b-ef.company and
           b-item.i-no eq b-ef.leaf[v-li]
           no-lock no-error.
   
      if avail b-item and b-item.mat-type eq "W" and
         (b-ef.leaf-l[v-li] ne 0 and b-ef.leaf-w[v-li] ne 0) then
         DO:
            IF b-ef.leaf-bnum[v-li] EQ 0 THEN
               v-t-win = v-t-win + (b-ef.leaf-l[v-li] * b-ef.leaf-w[v-li] / eb.num-up).
            ELSE
               v-t-win = v-t-win + (b-ef.leaf-l[v-li] * b-ef.leaf-w[v-li]).
         END.
   end.
   ELSE
      v-t-win = eb.t-win.

   FOR EACH wrk-ink:

      IF wrk-ink.i-qty <> wrk-ink.old-qty THEN 
         ASSIGN
            eb.i-%2[wrk-ink.cnt] = ROUND( (wrk-ink.i-qty * wrk-ink.yield * 100) /
                                          ((eb.t-sqin - v-t-win) * wrk-ink.num-sheets *
                                            eb.num-up * b-ef.n-out * b-ef.n-out-l),0). 
      
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
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


  FRAME {&FRAME-NAME}:TITLE = TRIM(FRAME {&FRAME-NAME}:TITLE) + " for Job#/Form: " +
                              TRIM(ip-job-no) + "-" + STRING(ip-job-no2,"99") +
                              "/" + TRIM(STRING(ip-frm,">>>>")).

  FIND FIRST eb WHERE ROWID(eb) = ip-rowid-eb NO-ERROR.
  FIND FIRST b-ef WHERE b-ef.company = cocode
                    AND b-ef.est-no  = eb.est-no NO-ERROR.
  FIND FIRST est WHERE est.company = cocode
                   AND est.est-no  = eb.est-no NO-ERROR.
  FIND FIRST job-hdr WHERE job-hdr.company = cocode
                       AND job-hdr.est-no  = eb.est-no
                       AND job-hdr.job-no  = ip-job-no
                       AND job-hdr.job-no2 = ip-job-no2
                       AND job-hdr.frm     = asi.eb.form-no
                       AND job-hdr.blank-no = asi.eb.blank-no NO-LOCK NO-ERROR. 

  ASSIGN  
     v-form             = eb.form-no
     v-blank            = eb.blank-no.          
     v-i-no             = job-hdr.i-no.

  RUN create-wrk-job-mat.

  RUN enable_UI.
 

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-wrk-job-mat Dialog-Frame 
PROCEDURE create-wrk-job-mat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/          
   DEF VAR v-num-sheets AS DEC NO-UNDO.

   FOR EACH job-mat WHERE job-mat.company  EQ cocode
                      AND job-mat.job      EQ job-hdr.job
                      AND job-mat.frm      EQ eb.form-no NO-LOCK,
       FIRST ITEM WHERE ITEM.company EQ cocode 
                    AND ITEM.i-no EQ job-mat.rm-i-no 
                    AND ITEM.mat-type = "B" NO-LOCK:

       v-num-sheets = job-mat.qty.
       
       LEAVE.
   END.

   FOR EACH job-mat WHERE job-mat.company  EQ cocode
                      AND job-mat.job      EQ job-hdr.job
                      AND job-mat.frm      EQ eb.form-no
                      AND job-mat.blank-no EQ eb.blank-no NO-LOCK,
      FIRST ITEM where
            (item.company = cocode and 
            (item.mat-type = "I" or item.mat-type = "V"))
            AND item.i-no EQ job-mat.rm-i-no NO-LOCK:

      DO i = 1 TO 16:
         IF eb.i-code2[i] EQ job-mat.i-no THEN DO:

            CREATE wrk-ink.
            ASSIGN
               wrk-ink.rm-i-no      = job-mat.rm-i-no
               wrk-ink.i-name       = ITEM.i-name
               wrk-ink.i-code       = eb.i-code2[i]
               wrk-ink.form-no      = eb.form-no
               wrk-ink.blank-no     = eb.blank-no
               wrk-ink.i-dscr       = eb.i-dscr2[i]
               wrk-ink.i-pass       = eb.i-ps2[i]
               wrk-ink.i-qty        = job-mat.qty
               wrk-ink.qty-uom      = job-mat.qty-uom
               wrk-ink.old-qty      = job-mat.qty
               wrk-ink.cnt = i
               wrk-ink.num-sheets = v-num-sheets
               wrk-ink.yield = ITEM.yield.
         END.
      END.
      {&CLOSE-QUERY-BROWSE-1}   
      {&OPEN-QUERY-BROWSE-1}
   END.

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
  DISPLAY v-i-no v-form v-blank 
      WITH FRAME Dialog-Frame.
  ENABLE BROWSE-1 Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

