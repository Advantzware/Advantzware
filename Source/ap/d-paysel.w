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
DEF INPUT PARAM ip-due-date AS DATE FORM "99/99/9999" NO-UNDO.
DEF INPUT PARAM ip-age AS INT NO-UNDO.
DEF INPUT PARAM ip-discount AS LOG NO-UNDO.
DEF INPUT PARAM ip-cc AS LOG NO-UNDO.
DEF INPUT PARAM ip-cc-only AS LOG NO-UNDO.
DEF INPUT PARAM ip-bp-only AS LOG NO-UNDO.
DEF OUTPUT PARAM op-show-disc AS LOG NO-UNDO.
DEF OUTPUT PARAM op-choice AS LOG NO-UNDO.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tg-show-disc Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS tg-show-disc lv-text2 FILL-IN-6 FILL-IN-9 ~
FILL-IN-10 FILL-IN-11 FILL-IN-7 FILL-IN-8 

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

DEFINE VARIABLE FILL-IN-10 AS CHARACTER FORMAT "X(256)":U INITIAL "Credit Card/ACH & Bill Pay Vendors will be excluded." 
     VIEW-AS FILL-IN 
     SIZE 72 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-11 AS CHARACTER FORMAT "X(256)":U INITIAL "This will only include Credit Card/ACH Vendors." 
     VIEW-AS FILL-IN 
     SIZE 72 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-6 AS CHARACTER FORMAT "X(256)":U INITIAL "will be selected for payment." 
     VIEW-AS FILL-IN 
     SIZE 64 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-7 AS CHARACTER FORMAT "X(256)":U INITIAL "Also, the selection file will be cleared!" 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-8 AS CHARACTER FORMAT "X(256)":U INITIAL "Continue?" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-9 AS CHARACTER FORMAT "X(256)":U INITIAL "This will include invoices with a discount date in the range." 
     VIEW-AS FILL-IN 
     SIZE 72 BY 1 NO-UNDO.

DEFINE VARIABLE lv-text2 AS CHARACTER FORMAT "X(256)":U INITIAL "All Invoices due on" 
     VIEW-AS FILL-IN 
     SIZE 64 BY 1 NO-UNDO.

DEFINE VARIABLE tg-show-disc AS LOGICAL INITIAL no 
     LABEL "Show Discount Regardless of Discount Date?" 
     VIEW-AS TOGGLE-BOX
     SIZE 61 BY 1.19 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     tg-show-disc AT ROW 1.48 COL 13
     lv-text2 AT ROW 3.14 COL 5 COLON-ALIGNED NO-LABEL
     FILL-IN-6 AT ROW 4.1 COL 5 COLON-ALIGNED NO-LABEL
     FILL-IN-9 AT ROW 5.14 COL 5 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     FILL-IN-10 AT ROW 6.14 COL 5 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     FILL-IN-11 AT ROW 6.14 COL 5 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     FILL-IN-7 AT ROW 7.19 COL 5 COLON-ALIGNED NO-LABEL
     FILL-IN-8 AT ROW 8.62 COL 5 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 10.29 COL 23
     Btn_Cancel AT ROW 10.29 COL 44
     SPACE(22.79) SKIP(0.61)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Payment Selection Confirmation"
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
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-10 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-10:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-11 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-11:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-6 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-7 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-8 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-9 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-9:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN lv-text2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Payment Selection Confirmation */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
   op-choice  = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    ASSIGN tg-show-disc
           op-show-disc = tg-show-disc
           op-choice = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE cMessage AS CHARACTER   NO-UNDO.
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    lv-text2 = lv-text2 + " " + string(ip-due-date,"99/99/9999") + " " + "or " +
             string(ip-age) + " days thereafter" .
    RUN enable_UI.
    IF ip-discount THEN FILL-IN-9:HIDDEN IN FRAME {&FRAME-NAME}  = NO.
    ELSE FILL-IN-9:HIDDEN IN FRAME {&FRAME-NAME}  = YES.
    IF ip-cc THEN FILL-IN-10:HIDDEN IN FRAME {&FRAME-NAME}  = NO.
    ELSE FILL-IN-10:HIDDEN IN FRAME {&FRAME-NAME}  = YES.
    IF ip-cc-only OR ip-bp-only THEN DO: 
        FILL-IN-11:HIDDEN IN FRAME {&FRAME-NAME}  = NO.
        cMessage = "This will only include ".
        IF ip-cc-only AND ip-bp-only THEN 
            cMessage = cMessage + "CC/ACH and Bill Pay Vendors." .
        ELSE IF ip-cc-only THEN
            cMessage = cMessage + "Credit Card/ACH Vendors." .
        ELSE cMessage = cMessage + "Bill Pay Vendors." .
        FILL-IN-11:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cMessage.
    END.
    ELSE FILL-IN-11:HIDDEN IN FRAME {&FRAME-NAME}  = YES.
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
  DISPLAY tg-show-disc lv-text2 FILL-IN-6 FILL-IN-9 FILL-IN-10 FILL-IN-11 
          FILL-IN-7 FILL-IN-8 
      WITH FRAME Dialog-Frame.
  ENABLE tg-show-disc Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

