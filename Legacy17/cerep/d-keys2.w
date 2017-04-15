&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
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
DEF INPUT PARAM ip-job-no AS cha NO-UNDO.
DEF INPUT PARAM ip-job-no2 AS INT NO-UNDO.
DEF INPUT PARAM ip-frm AS INT NO-UNDO.
DEF INPUT PARAM ip-blank AS INT NO-UNDO.
DEF INPUT PARAM ip-i-no AS cha NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.

{cerep/jc-keys2.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-40 RECT-41 v-aql v-glue-cnt v-style ~
v-style-2 Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS v-i-no v-aql v-glue-cnt v-style v-style-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 v-i-no v-aql v-glue-cnt v-style v-style-2 v-newsize ~
v-tdesc v-tqty v-mb 

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

DEFINE VARIABLE v-aql AS CHARACTER FORMAT "X(25)":U 
     LABEL "AQL" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE v-glue-cnt AS CHARACTER FORMAT "X(20)":U 
     LABEL "Gluing Pick Up Count" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE v-i-no AS CHARACTER FORMAT "X(25)":U 
     LABEL "FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE v-mb AS CHARACTER FORMAT "X(25)":U 
     LABEL "M/B" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE v-newsize AS CHARACTER FORMAT "X(25)":U 
     LABEL "New Size" 
     VIEW-AS FILL-IN 
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE v-style AS CHARACTER FORMAT "X(25)":U 
     LABEL "Style" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE v-style-2 AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE v-tdesc AS CHARACTER FORMAT "X(25)":U 
     LABEL "Tray Desc" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE v-tqty AS INTEGER FORMAT ">>>>>>>>>>9":U INITIAL 0 
     LABEL "Qty Per Tray" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 125 BY 10.95.

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 132 BY 14.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     v-i-no AT ROW 2.19 COL 31 COLON-ALIGNED
     v-aql AT ROW 3.62 COL 31 COLON-ALIGNED
     v-glue-cnt AT ROW 4.81 COL 31 COLON-ALIGNED
     v-style AT ROW 6.48 COL 31 COLON-ALIGNED
     v-style-2 AT ROW 7.43 COL 31 COLON-ALIGNED NO-LABEL
     v-newsize AT ROW 9.81 COL 74 COLON-ALIGNED
     v-tdesc AT ROW 10.05 COL 19 COLON-ALIGNED
     v-tqty AT ROW 11 COL 60 COLON-ALIGNED
     v-mb AT ROW 11 COL 92 COLON-ALIGNED
     Btn_OK AT ROW 13.38 COL 33
     Btn_Cancel AT ROW 13.38 COL 81
     RECT-40 AT ROW 1.48 COL 3
     RECT-41 AT ROW 1 COL 1
     SPACE(3.99) SKIP(1.32)
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
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN v-aql IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN v-glue-cnt IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN v-i-no IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN v-mb IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE 1                                               */
ASSIGN 
       v-mb:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN v-newsize IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE 1                                               */
ASSIGN 
       v-newsize:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN v-style IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN v-style-2 IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN v-tdesc IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE 1                                               */
ASSIGN 
       v-tdesc:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN v-tqty IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE 1                                               */
ASSIGN 
       v-tqty:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
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
    FIND current tt-key2 NO-ERROR.
   /* IF NOT AVAIL tt-key2 THEN CREATE tt-key2. */

    ASSIGN {&list-1}.

    ASSIGN  tt-key2.tt-tray-desc = v-tdesc 
            tt-key2.tt-tray-qty = v-tqty
            tt-key2.tt-mb = v-mb
            tt-key2.tt-newsize  = v-newsize
            tt-key2.tt-aql = v-aql
            tt-key2.tt-glue-cnt = v-glue-cnt
            tt-key2.tt-style[1] = v-style
            tt-key2.tt-style[2] = v-style-2
            .
    RUN get-upd-reft (NO).
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

  FIND FIRST tt-key2
      WHERE tt-key2.tt-job-no  EQ ip-job-no
        AND tt-key2.tt-job-no2 EQ ip-job-no2
        AND tt-key2.tt-frm     EQ ip-frm
        AND tt-key2.tt-blank   EQ ip-blank
        AND tt-key2.tt-i-no    EQ ip-i-no
      NO-LOCK NO-ERROR.
  IF AVAIL tt-key2 THEN v-i-no = tt-key2.tt-i-no.

  FRAME {&FRAME-NAME}:TITLE = TRIM(FRAME {&FRAME-NAME}:TITLE) + " for Job#/Form: " +
                              TRIM(ip-job-no) + "-" + STRING(ip-job-no2,"99") +
                              "/" + TRIM(STRING(ip-frm,">>>>")).

  RUN get-upd-reft (YES).

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
  DISPLAY v-i-no v-aql v-glue-cnt v-style v-style-2 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-40 RECT-41 v-aql v-glue-cnt v-style v-style-2 Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-upd-reft Dialog-Frame 
PROCEDURE get-upd-reft :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-get AS LOG NO-UNDO.

  DEF VAR li AS INT NO-UNDO.


  li = 0.

  DO li = 1 TO 9:
    FIND FIRST reftable
        WHERE reftable.reftable EQ "cerep/d-keys2.w"
          AND reftable.company  EQ cocode
          AND reftable.loc      EQ TRIM(ip-job-no)
          AND reftable.code     EQ STRING(ip-job-no2,"9999999999")
          AND reftable.code2    EQ ip-i-no
          AND reftable.val[1]   EQ ip-frm
          AND reftable.val[2]   EQ li
        NO-ERROR.
    IF NOT AVAIL reftable THEN DO:
      CREATE reftable.
      ASSIGN
       reftable.reftable = "cerep/d-keys2.w"
       reftable.company  = cocode
       reftable.loc      = TRIM(ip-job-no)
       reftable.code     = TRIM(STRING(ip-job-no2,"9999999999"))
       reftable.code2    = ip-i-no
       reftable.val[1]   = ip-frm
       reftable.val[2]   = li.
    END.

    CASE li:
      WHEN 01 THEN IF ip-get THEN v-i-no        = IF v-i-no EQ "" THEN
                                                    reftable.dscr ELSE v-i-no.
                             ELSE reftable.dscr = v-i-no.
      WHEN 02 THEN IF ip-get THEN v-aql         = reftable.dscr.
                             ELSE reftable.dscr = v-aql.
      WHEN 03 THEN IF ip-get THEN v-glue-cnt    = reftable.dscr.
                             ELSE reftable.dscr = v-glue-cnt.
      WHEN 04 THEN IF ip-get THEN v-style       = reftable.dscr.
                             ELSE reftable.dscr = v-style.
      WHEN 05 THEN IF ip-get THEN v-style-2     = reftable.dscr.
                             ELSE reftable.dscr = v-style-2.
      WHEN 06 THEN IF ip-get THEN v-newsize     = reftable.dscr.
                             ELSE reftable.dscr = v-newsize.
      WHEN 07 THEN IF ip-get THEN v-tdesc       = reftable.dscr.
                             ELSE reftable.dscr = v-tdesc.
      WHEN 08 THEN IF ip-get THEN v-tqty        = INT(reftable.dscr).
                             ELSE reftable.dscr = STRING(v-tqty).
      WHEN 09 THEN IF ip-get THEN v-mb          = reftable.dscr.
                             ELSE reftable.dscr = v-mb.
    END CASE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

