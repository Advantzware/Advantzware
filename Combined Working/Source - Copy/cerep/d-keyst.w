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

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.

{cerep/jc-keyst.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS v-mill v-negs v-plate2 v-die2 v-copy v-size ~
v-sheets-req v-plate v-plate-2 v-die v-die-2 Btn_OK Btn_Cancel RECT-40 ~
RECT-41 
&Scoped-Define DISPLAYED-OBJECTS v-mill v-negs v-plate2 v-die2 v-copy ~
v-size v-sheets-req v-plate v-plate-2 v-die v-die-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 v-mill v-negs v-plate2 v-die2 v-copy v-size ~
v-sheets-req v-plate v-plate-2 v-die v-die-2 

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

DEFINE VARIABLE v-copy AS CHARACTER FORMAT "X(25)":U 
     LABEL "Copy" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE v-die AS CHARACTER FORMAT "X(25)":U 
     LABEL "Dies" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE v-die-2 AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE v-die2 AS CHARACTER FORMAT "X(25)":U 
     LABEL "Dies" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE v-mill AS CHARACTER FORMAT "X(25)":U 
     LABEL "Board Mill" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE v-negs AS CHARACTER FORMAT "X(25)":U 
     LABEL "Original Negs" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE v-plate AS CHARACTER FORMAT "X(25)":U 
     LABEL "Plates" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE v-plate-2 AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE v-plate2 AS CHARACTER FORMAT "X(25)":U 
     LABEL "Plates" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE v-sheets-req AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "Sheets Req'd" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE v-size AS CHARACTER FORMAT "X(25)":U 
     LABEL "Size && Style" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 125 BY 10.95.

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 132 BY 14.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     v-mill AT ROW 1.71 COL 18.8 COLON-ALIGNED
     v-negs AT ROW 2.67 COL 18.8 COLON-ALIGNED
     v-plate2 AT ROW 3.62 COL 18.8 COLON-ALIGNED
     v-die2 AT ROW 4.57 COL 18.8 COLON-ALIGNED
     v-copy AT ROW 5.52 COL 18.8 COLON-ALIGNED
     v-size AT ROW 6.48 COL 18.8 COLON-ALIGNED
     v-sheets-req AT ROW 7.48 COL 18.8 COLON-ALIGNED
     v-plate AT ROW 9.62 COL 18.8 COLON-ALIGNED
     v-plate-2 AT ROW 10.57 COL 18.8 COLON-ALIGNED NO-LABEL
     v-die AT ROW 9.62 COL 70.8 COLON-ALIGNED
     v-die-2 AT ROW 10.57 COL 70.8 COLON-ALIGNED NO-LABEL
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
   Custom                                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN v-copy IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN v-die IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN v-die-2 IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN v-die2 IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN v-mill IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN v-negs IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN v-plate IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN v-plate-2 IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN v-plate2 IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN v-sheets-req IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN v-size IN FRAME Dialog-Frame
   1                                                                    */
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


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
    FIND FIRST tt-keyst NO-ERROR.
    IF NOT AVAIL tt-keyst THEN CREATE tt-keyst.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    FIND FIRST tt-keyst
      WHERE tt-keyst.tt-job-no  EQ ip-job-no
        AND tt-keyst.tt-job-no2 EQ ip-job-no2
        AND tt-keyst.tt-frm     EQ ip-frm
        NO-ERROR.
    IF NOT AVAIL tt-keyst THEN CREATE tt-keyst.

    ASSIGN {&list-1}.

    ASSIGN  tt-keyst.tt-job-no = ip-job-no
            tt-keyst.tt-job-no2 = ip-job-no2
            tt-keyst.tt-frm = ip-frm
            tt-keyst.tt-mill = v-mill
            tt-keyst.tt-negs = v-negs
            tt-keyst.tt-plate[1] = v-plate
            tt-keyst.tt-plate[2] = v-plate-2
            tt-keyst.tt-die[1] = v-die
            tt-keyst.tt-die[2] = v-die-2
            tt-keyst.tt-copy = v-copy
            tt-keyst.tt-size = v-size
            tt-keyst.tt-sheets-req = v-sheets-req
            /*tt-keyst.tt-tray-desc = v-tdesc 
            tt-keyst.tt-tray-qty = v-tqty
            tt-keyst.tt-mb = v-mb
            tt-keyst.tt-newsize  = v-newsize
            tt-keyst.tt-aql = v-aql
            tt-keyst.tt-glue-cnt = v-glue-cnt
            */
            tt-keyst.tt-plate2 = v-plate2
            tt-keyst.tt-die2 = v-die2
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

  RUN get-upd-reft (YES).

  FRAME {&FRAME-NAME}:TITLE = TRIM(FRAME {&FRAME-NAME}:TITLE) + " for Job#/Form: " +
                              TRIM(ip-job-no) + "-" + STRING(ip-job-no2,"99") +
                              "/" + TRIM(STRING(ip-frm,">>>>")).

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
  DISPLAY v-mill v-negs v-plate2 v-die2 v-copy v-size v-sheets-req v-plate 
          v-plate-2 v-die v-die-2 
      WITH FRAME Dialog-Frame.
  ENABLE v-mill v-negs v-plate2 v-die2 v-copy v-size v-sheets-req v-plate 
         v-plate-2 v-die v-die-2 Btn_OK Btn_Cancel RECT-40 RECT-41 
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

  DO li = 1 TO 11:
    FIND FIRST reftable
        WHERE reftable.reftable EQ "cerep/d-keyst.w"
          AND reftable.company  EQ cocode
          AND reftable.loc      EQ TRIM(ip-job-no)
          AND reftable.code     EQ STRING(ip-job-no2,"9999999999")
          AND reftable.code2    EQ ""
          AND reftable.val[1]   EQ li
          AND (reftable.val[2]  EQ ip-frm OR reftable.val[2] EQ 0)
        NO-ERROR.
    IF NOT AVAIL reftable THEN DO:
      CREATE reftable.
      ASSIGN
       reftable.reftable = "cerep/d-keyst.w"
       reftable.company  = cocode
       reftable.loc      = TRIM(ip-job-no)
       reftable.code     = STRING(ip-job-no2,"9999999999")
       reftable.code2    = ""
       reftable.val[1]   = li.
    END.
    reftable.val[2] = ip-frm.

    CASE li:
      WHEN 01 THEN IF ip-get THEN v-mill        = reftable.dscr.
                             ELSE reftable.dscr = v-mill.
      WHEN 02 THEN IF ip-get THEN v-negs        = reftable.dscr.
                             ELSE reftable.dscr = v-negs.
      WHEN 03 THEN IF ip-get THEN v-plate2      = reftable.dscr.
                             ELSE reftable.dscr = v-plate2.
      WHEN 04 THEN IF ip-get THEN v-die2        = reftable.dscr.
                             ELSE reftable.dscr = v-die2.
      WHEN 05 THEN IF ip-get THEN v-copy        = reftable.dscr.
                             ELSE reftable.dscr = v-copy.
      WHEN 06 THEN IF ip-get THEN v-size        = reftable.dscr.
                             ELSE reftable.dscr = v-size.
      WHEN 07 THEN IF ip-get THEN v-plate       = reftable.dscr.
                             ELSE reftable.dscr = v-plate.
      WHEN 08 THEN IF ip-get THEN v-die         = reftable.dscr.
                             ELSE reftable.dscr = v-die.
      WHEN 09 THEN IF ip-get THEN v-plate-2     = reftable.dscr.
                             ELSE reftable.dscr = v-plate-2.
      WHEN 10 THEN IF ip-get THEN v-die-2       = reftable.dscr.
                             ELSE reftable.dscr = v-die-2.
      WHEN 11 THEN IF ip-get THEN v-sheets-req  = DEC(reftable.dscr).
                             ELSE reftable.dscr = STRING(v-sheets-req).
    END CASE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

