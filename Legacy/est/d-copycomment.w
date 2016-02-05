&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File:  est/d-copycomment.w

  Description:  Copy comments from selected quote to range of quotes.

  Input Parameters:
      piQuote - integer:  Selected quote to copy comments from

  Output Parameters:
      <none>

  Author: Stacey Brooks

  Created: Oct 2011
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER piQuote  AS INT NO-UNDO.


/* Local Variable Definitions ---                                       */

DEF BUFFER bf-quotehd FOR quotehd.  /* Buffer for selected quote. */
DEF BUFFER bf-quotehd2 FOR quotehd. /* Buffer for "copy-to" range of quotes. */

{custom/globdefs.i}

DEFINE BUFFER buf-cust FOR cust.

DEF VAR char-val AS CHAR no-undo.
DEF VAR look-recid AS RECID NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 Btn_OK fiCustFrom fiCustTo Btn_Cancel ~
fiFromQuote fiToQuote 
&Scoped-Define DISPLAYED-OBJECTS fiCustFrom fiCustTo fiFromQuote fiToQuote 

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

DEFINE VARIABLE fiCustFrom AS CHARACTER FORMAT "X(20)":U 
     LABEL "From Customer" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiCustTo AS CHARACTER FORMAT "X(20)":U INITIAL "zzzzzzzzzzzzzz" 
     LABEL "To Customer" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiFromQuote AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "From Quote" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiToQuote AS INTEGER FORMAT ">>>>9":U INITIAL 99999 
     LABEL "To Quote" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 85 BY 9.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_OK AT ROW 2 COL 69.2
     fiCustFrom AT ROW 3 COL 17.8 COLON-ALIGNED WIDGET-ID 2
     fiCustTo AT ROW 3 COL 47.8 COLON-ALIGNED WIDGET-ID 4
     Btn_Cancel AT ROW 3.24 COL 69.2
     fiFromQuote AT ROW 4.33 COL 17.8 COLON-ALIGNED WIDGET-ID 6
     fiToQuote AT ROW 4.33 COL 48 COLON-ALIGNED WIDGET-ID 8
     RECT-6 AT ROW 1.14 COL 1.4 WIDGET-ID 10
     SPACE(0.59) SKIP(0.14)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Copy Quote Comments"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


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
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Copy Quote Comments */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  RUN Copy-Comment.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiCustFrom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCustFrom Dialog-Frame
ON HELP OF fiCustFrom IN FRAME Dialog-Frame /* From Customer */
DO:
    RUN windows/l-custact.w (g_company, {&self-name}:SCREEN-VALUE, OUTPUT char-val, OUTPUT look-recid).
    FIND buf-cust WHERE RECID(buf-cust) EQ look-recid NO-LOCK NO-ERROR.
    IF AVAIL buf-cust THEN
        ASSIGN {&self-name}:SCREEN-VALUE = buf-cust.cust-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiCustTo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCustTo Dialog-Frame
ON HELP OF fiCustTo IN FRAME Dialog-Frame /* To Customer */
DO:
    RUN windows/l-custact.w (g_company, {&self-name}:SCREEN-VALUE, OUTPUT char-val, OUTPUT look-recid).
    FIND buf-cust WHERE RECID(buf-cust) EQ look-recid NO-LOCK NO-ERROR.
    IF AVAIL buf-cust THEN
        ASSIGN {&self-name}:SCREEN-VALUE = buf-cust.cust-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFromQuote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFromQuote Dialog-Frame
ON HELP OF fiFromQuote IN FRAME Dialog-Frame /* From Quote */
DO:
  /* There is no lookup object for quotes. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiToQuote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiToQuote Dialog-Frame
ON HELP OF fiToQuote IN FRAME Dialog-Frame /* To Quote */
DO:
  /* There is no lookup object for quotes. */
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
  RUN Init-Screen.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Copy-Comment Dialog-Frame 
PROCEDURE Copy-Comment :
/*------------------------------------------------------------------------------
  Purpose:     Copy comments from one quote to a range of quotes.
  Parameters:  <none>
  Notes:       S.Brooks  Task# 10191101
------------------------------------------------------------------------------*/
  DEF VAR viCount AS INT NO-UNDO INIT 0.
    

  /* Find the selected quote to copy from */                     
  FIND FIRST bf-quotehd WHERE 
             bf-quotehd.q-no = piQuote NO-LOCK NO-ERROR.

  /* Message and return if record not found. */
  IF NOT AVAILABLE bf-quotehd THEN DO:
      MESSAGE "Cannot locate selected quote header: " + STRING(piQuote)
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN.
  END.

  DO WITH FRAME {&FRAME-NAME}:

      /* Process the selected range of quotes to copy to. */
      FOR EACH bf-quotehd2 EXCLUSIVE-LOCK WHERE
               bf-quotehd2.cust-no >= fiCustFrom:SCREEN-VALUE AND
               bf-quotehd2.cust-no <= fiCustTo:SCREEN-VALUE AND
               bf-quotehd2.q-no >= INT(fiFromQuote:SCREEN-VALUE) AND
               bf-quotehd2.q-no <= INT(fiToQuote:SCREEN-VALUE):

          /* Copy comments from selected record. */
          ASSIGN bf-quotehd2.comment[1] = bf-quotehd.comment[1]
                 bf-quotehd2.comment[2] = bf-quotehd.comment[2]
                 bf-quotehd2.comment[3] = bf-quotehd.comment[3]
                 bf-quotehd2.comment[4] = bf-quotehd.comment[4]
                 bf-quotehd2.comment[5] = bf-quotehd.comment[5]
                 viCount = viCount + 1.

      END. /* FOR EACH bf-quotehd2 */

  END. /* DO WITH FRAME  */
                                        
  /* Message user how many copies were done. */
  IF viCount = 0 THEN
      MESSAGE "No records were selected for copy."
        VIEW-AS ALERT-BOX WARNING BUTTONS OK.
  ELSE
      MESSAGE "Copy Completed for " viCount " records."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

  RETURN.

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
  DISPLAY fiCustFrom fiCustTo fiFromQuote fiToQuote 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-6 Btn_OK fiCustFrom fiCustTo Btn_Cancel fiFromQuote fiToQuote 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Init-Screen Dialog-Frame 
PROCEDURE Init-Screen :
/*------------------------------------------------------------------------------
  Purpose:     Initialize the frame header to indicate selected quote.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DO WITH FRAME {&FRAME-NAME}:

       /* Find the selected quote to copy from */                     
       FIND FIRST bf-quotehd WHERE 
                  bf-quotehd.q-no = piQuote NO-LOCK NO-ERROR.

       /* If found, then set the frame header and default the input values. */
       IF AVAILABLE bf-quotehd THEN
           ASSIGN FRAME Dialog-Frame:TITLE = "Copy Quote " + STRING(bf-quotehd.q-no) + " Comments"
                  fiCustFrom:SCREEN-VALUE = STRING(bf-quotehd.cust-no)
                  fiCustTo:SCREEN-VALUE = STRING(bf-quotehd.cust-no)
                  fiFromQuote:SCREEN-VALUE = STRING(bf-quotehd.q-no)
                  fiToQuote:SCREEN-VALUE = STRING(bf-quotehd.q-no).
       ELSE
           ASSIGN FRAME Dialog-Frame:TITLE = "Copy Quote Comments".

   END.

   RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

