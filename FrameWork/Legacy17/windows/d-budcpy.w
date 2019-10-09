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
DEF INPUT PARAM ip-recid AS recid NO-UNDO.
DEF INPUT PARAM ip-year AS INT NO-UNDO.
DEF INPUT PARAM ip-prd AS INT NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}

ASSIGN cocode = g_company
       locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 RECT-41 begin_prd end_prd ~
btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_prd end_prd v-status 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-GO 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE begin_prd AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Copy Period From" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE end_prd AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Up To Copy Period To" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE v-status AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 88 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 3.81.

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 91 BY 8.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     begin_prd AT ROW 3.14 COL 22 COLON-ALIGNED
     end_prd AT ROW 3.14 COL 55 COLON-ALIGNED
     v-status AT ROW 5.76 COL 2 NO-LABEL
     btn-process AT ROW 7.43 COL 21
     btn-cancel AT ROW 7.43 COL 60
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 1.71 COL 6
     RECT-17 AT ROW 1.24 COL 2
     RECT-41 AT ROW 1 COL 1
     SPACE(1.39) SKIP(1.85)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Copy  Budget Periods".


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

/* SETTINGS FOR FILL-IN v-status IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Copy  Budget Periods */
DO:
  APPLY "END-ERROR":U TO SELF.
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


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process Dialog-Frame
ON CHOOSE OF btn-process IN FRAME Dialog-Frame /* Start Process */
DO:
  DEF VAR v-process AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  MESSAGE "Existing budgets will de deleted." SKIP
          "Are you sure you want to copy period for the selected parameters?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.
        
  IF v-process THEN RUN run-process.
  ELSE APPLY "choose" TO btn-cancel.
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

  begin_prd = ip-prd.
  DISPLAY begin_prd WITH FRAME {&FRAME-NAME}.

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
  DISPLAY begin_prd end_prd v-status 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-17 RECT-41 begin_prd end_prd btn-process btn-cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
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
DEF BUFFER bf-sman FOR sman.
DEF BUFFER bf-bugt FOR smanbugt.
DEF BUFFER bf-bcat FOR smanbcat.
DEF BUFFER bf-bcst FOR smanbcst.

DEF VAR v-index AS INT NO-UNDO.
DEF VAR v-index-begin AS INT NO-UNDO.
DEF VAR v-index-end AS INT NO-UNDO.

SESSION:SET-WAIT-STATE("general").

FIND FIRST bf-sman WHERE RECID(bf-sman) = ip-recid NO-LOCK .

FOR EACH bf-bugt OF bf-sman:
  
  IF begin_prd LT END_prd THEN
    DO v-index = begin_prd + 1 TO END_prd:
      ASSIGN bf-bugt.budget-amt[v-index] = bf-bugt.budget-amt[begin_prd]
           bf-bugt.msf[v-index] = bf-bugt.msf[begin_prd]
           bf-bugt.ton[v-index] = bf-bugt.ton[begin_prd].

      FOR each smanbcat WHERE smanbcat.company = bf-bugt.company
                                AND smanbcat.sman = bf-bugt.sman
                                AND smanbcat.budget-yr = bf-bugt.budget-yr
                                AND smanbcat.budget-period = v-index:
        DELETE smanbcat.
      END.
      FOR EACH smanbcst WHERE smanbcst.company = bf-bugt.company
                                AND smanbcst.sman = bf-bugt.sman
                                AND smanbcst.budget-yr = bf-bugt.budget-yr
                                AND smanbcst.budget-period = v-index:
        DELETE smanbcst.
      END.
    
      FOR each smanbcat NO-LOCK WHERE smanbcat.company = bf-bugt.company
                                AND smanbcat.sman = bf-bugt.sman
                                AND smanbcat.budget-yr = bf-bugt.budget-yr
                                AND smanbcat.budget-period = begin_prd:
        FIND FIRST bf-bcat WHERE bf-bcat.company = bf-bugt.company
                             AND bf-bcat.sman = bf-bugt.sman
                             AND bf-bcat.budget-yr = bf-bugt.budget-yr
                             AND bf-bcat.budget-period = v-index 
                             AND bf-bcat.procat = smanbcat.procat NO-ERROR.
        IF NOT AVAIL bf-bcat THEN do:
           CREATE bf-bcat.           
        END.
        BUFFER-COPY smanbcat EXCEPT smanbcat.budget-period TO bf-bcat.
        bf-bcat.budget-period = v-index.        
      END.
      FOR EACH smanbcst NO-LOCK WHERE smanbcst.company = bf-bugt.company
                                AND smanbcst.sman = bf-bugt.sman
                                AND smanbcst.budget-yr = bf-bugt.budget-yr
                                AND smanbcst.budget-period = begin_prd:
        FIND FIRST bf-bcst WHERE bf-bcst.company = bf-bugt.company
                             AND bf-bcst.sman = bf-bugt.sman
                             AND bf-bcst.budget-yr = bf-bugt.budget-yr
                             AND bf-bcst.budget-period = v-index 
                             AND bf-bcst.procat = smanbcst.procat 
                             AND bf-bcst.cust-no = smanbcst.cust-no NO-ERROR.
        IF NOT AVAIL bf-bcst THEN CREATE bf-bcst.           
        BUFFER-COPY smanbcst EXCEPT smanbcst.budget-period TO bf-bcst.
        bf-bcst.budget-period = v-index.
      END. 
    END.
  ELSE
  IF begin_prd GT END_prd THEN
    DO v-index = begin_prd - 1 TO END_prd BY -1:

      IF v-index NE 0 THEN DO:
        ASSIGN bf-bugt.budget-amt[v-index] = bf-bugt.budget-amt[begin_prd]
              bf-bugt.msf[v-index] = bf-bugt.msf[begin_prd]
              bf-bugt.ton[v-index] = bf-bugt.ton[begin_prd].
        
         FOR each smanbcat WHERE smanbcat.company = bf-bugt.company
                                   AND smanbcat.sman = bf-bugt.sman
                                   AND smanbcat.budget-yr = bf-bugt.budget-yr
                                   AND smanbcat.budget-period = v-index:
           DELETE smanbcat.
         END.
         FOR EACH smanbcst WHERE smanbcst.company = bf-bugt.company
                                   AND smanbcst.sman = bf-bugt.sman
                                   AND smanbcst.budget-yr = bf-bugt.budget-yr
                                   AND smanbcst.budget-period = v-index:
           DELETE smanbcst.
         END.
        
         FOR each smanbcat NO-LOCK WHERE smanbcat.company = bf-bugt.company
                                   AND smanbcat.sman = bf-bugt.sman
                                   AND smanbcat.budget-yr = bf-bugt.budget-yr
                                   AND smanbcat.budget-period = begin_prd:
           FIND FIRST bf-bcat WHERE bf-bcat.company = bf-bugt.company
                                AND bf-bcat.sman = bf-bugt.sman
                                AND bf-bcat.budget-yr = bf-bugt.budget-yr
                                AND bf-bcat.budget-period = v-index 
                                AND bf-bcat.procat = smanbcat.procat NO-ERROR.
           IF NOT AVAIL bf-bcat THEN do:
              CREATE bf-bcat.           
           END.
           BUFFER-COPY smanbcat EXCEPT smanbcat.budget-period TO bf-bcat.
           bf-bcat.budget-period = v-index.        
         END.
         FOR EACH smanbcst NO-LOCK WHERE smanbcst.company = bf-bugt.company
                                   AND smanbcst.sman = bf-bugt.sman
                                   AND smanbcst.budget-yr = bf-bugt.budget-yr
                                   AND smanbcst.budget-period = begin_prd:
           FIND FIRST bf-bcst WHERE bf-bcst.company = bf-bugt.company
                                AND bf-bcst.sman = bf-bugt.sman
                                AND bf-bcst.budget-yr = bf-bugt.budget-yr
                                AND bf-bcst.budget-period = v-index 
                                AND bf-bcst.procat = smanbcst.procat 
                                AND bf-bcst.cust-no = smanbcst.cust-no NO-ERROR.
           IF NOT AVAIL bf-bcst THEN CREATE bf-bcst.           
           BUFFER-COPY smanbcst EXCEPT smanbcst.budget-period TO bf-bcst.
           bf-bcst.budget-period = v-index.
         END.
      END.
    END.
END.

STATUS DEFAULT "".

SESSION:SET-WAIT-STATE("").

MESSAGE " Process Is Completed." VIEW-AS ALERT-BOX.

APPLY "go" TO FRAME {&frame-name}.
  
/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

