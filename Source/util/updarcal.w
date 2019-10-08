&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: util/updarcal.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-comp-no lv-cust-no lv-chk-no lv-inv-no ~
lv-inv-date lv-disc lv-paid lv-due lv-on Btn_OK Btn_Cancel RECT-32 RECT-33 
&Scoped-Define DISPLAYED-OBJECTS lv-comp-no lv-cust-no lv-chk-no lv-inv-no ~
lv-inv-date lv-disc lv-paid lv-due lv-on 

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

DEFINE VARIABLE lv-chk-no AS CHARACTER FORMAT "X(10)":U 
     LABEL "Check#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE lv-comp-no AS CHARACTER FORMAT "X(256)":U INITIAL "001" 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE lv-cust-no AS CHARACTER FORMAT "X(256)":U 
     LABEL "Customer" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE lv-disc AS DECIMAL FORMAT "->,>>>,>>9.99":U INITIAL 0 
     LABEL "Discount Amount" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE lv-due AS DECIMAL FORMAT "->,>>>,>>9.99":U INITIAL 0 
     LABEL "Due Amount" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE lv-inv-date AS DATE FORMAT "99/99/9999":U 
     LABEL "Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE lv-inv-no AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE lv-on AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     LABEL "On Account" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE lv-paid AS DECIMAL FORMAT "->,>>>,>>9.99":U INITIAL 0 
     LABEL "Paid Amount" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-32
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 65 BY 5.71.

DEFINE RECTANGLE RECT-33
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 65 BY 6.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     lv-comp-no AT ROW 1.71 COL 21 COLON-ALIGNED
     lv-cust-no AT ROW 2.91 COL 21 COLON-ALIGNED
     lv-chk-no AT ROW 4.1 COL 21 COLON-ALIGNED
     lv-inv-no AT ROW 5.29 COL 21 COLON-ALIGNED
     lv-inv-date AT ROW 7.43 COL 22 COLON-ALIGNED
     lv-disc AT ROW 8.38 COL 22 COLON-ALIGNED
     lv-paid AT ROW 9.33 COL 22 COLON-ALIGNED
     lv-due AT ROW 10.29 COL 22 COLON-ALIGNED
     lv-on AT ROW 11.48 COL 22 COLON-ALIGNED
     Btn_OK AT ROW 14.33 COL 18
     Btn_Cancel AT ROW 14.57 COL 51
     RECT-32 AT ROW 1.24 COL 3
     RECT-33 AT ROW 6.95 COL 3
     SPACE(23.79) SKIP(3.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Update AR Cash Line"
         CANCEL-BUTTON Btn_Cancel.


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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Update AR Cash Line */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     lv-comp-no
     lv-cust-no
     lv-chk-no
     lv-inv-no     
     lv-inv-date
     lv-disc
     lv-paid
     lv-due
     lv-on.

    IF NOT CAN-FIND(FIRST ar-cashl
                    WHERE ar-cashl.company  EQ lv-comp-no
                      AND ar-cashl.cust-no  EQ lv-cust-no
                      AND ar-cashl.check-no EQ lv-chk-no
                      AND ar-cashl.inv-no   EQ lv-inv-no)
    THEN DO:
      MESSAGE "Invalid Check# for the customer entered." VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
    END.

    MESSAGE "Are you sure you want to update ? " VIEW-AS ALERT-BOX WARNING BUTTON YES-NO
        UPDATE ll-ans AS LOG.
    IF ll-ans THEN DO:
      FIND CURRENT ar-cashl EXCLUSIVE-LOCK.
      IF AVAIL ar-cashl THEN
        ASSIGN
         ar-cashl.inv-date   = lv-inv-date
         ar-cashl.amt-disc   = lv-disc                 
         ar-cashl.amt-paid   = lv-paid
         ar-cashl.amt-due    = lv-due
         ar-cashl.on-account = lv-on.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-chk-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-chk-no Dialog-Frame
ON VALUE-CHANGED OF lv-chk-no IN FRAME Dialog-Frame /* Check# */
DO:
  RUN new-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-comp-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-comp-no Dialog-Frame
ON VALUE-CHANGED OF lv-comp-no IN FRAME Dialog-Frame /* Company */
DO:
  RUN new-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-cust-no Dialog-Frame
ON VALUE-CHANGED OF lv-cust-no IN FRAME Dialog-Frame /* Customer */
DO:
  RUN new-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-inv-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-inv-no Dialog-Frame
ON VALUE-CHANGED OF lv-inv-no IN FRAME Dialog-Frame /* Invoice# */
DO:
  RUN new-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
SESSION:DATA-ENTRY-RETURN = yes.

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
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
  DISPLAY lv-comp-no lv-cust-no lv-chk-no lv-inv-no lv-inv-date lv-disc lv-paid 
          lv-due lv-on 
      WITH FRAME Dialog-Frame.
  ENABLE lv-comp-no lv-cust-no lv-chk-no lv-inv-no lv-inv-date lv-disc lv-paid 
         lv-due lv-on Btn_OK Btn_Cancel RECT-32 RECT-33 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-record Dialog-Frame 
PROCEDURE new-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    RELEASE ar-cashl.

    lv = lv-chk-no:SCREEN-VALUE.

    IF TRIM(lv-comp-no:SCREEN-VALUE) NE "" AND
       TRIM(lv-cust-no:SCREEN-VALUE) NE "" AND
       lv                            NE "" AND
       INT(lv-inv-no:SCREEN-VALUE)   NE 0  THEN
    DO WHILE LENGTH(TRIM(lv)) LT 11:
      FIND FIRST ar-cashl
          WHERE ar-cashl.company  EQ lv-comp-no:SCREEN-VALUE
            AND ar-cashl.cust-no  EQ lv-cust-no:SCREEN-VALUE
            AND ar-cashl.check-no EQ lv
            AND ar-cashl.inv-no   EQ INT(lv-inv-no:SCREEN-VALUE)
          NO-LOCK NO-ERROR.

      IF AVAIL ar-cashl THEN DO:
        ASSIGN
         lv-chk-no:SCREEN-VALUE   = ar-cashl.check-no
         lv-inv-date:SCREEN-VALUE = STRING(ar-cashl.inv-date)
         lv-disc:SCREEN-VALUE     = STRING(ar-cashl.amt-disc)
         lv-on:SCREEN-VALUE       = STRING(ar-cashl.on-account)
         lv-paid:SCREEN-VALUE     = STRING(ar-cashl.amt-paid)
         lv-due:SCREEN-VALUE      = STRING(ar-cashl.amt-due).
        LEAVE.
      END.

      ELSE lv = "0" + TRIM(lv).
    END. 
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

