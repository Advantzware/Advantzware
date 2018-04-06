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
&Scoped-Define ENABLED-OBJECTS lv-comp lv-cust-no lv-inv-no lv-net lv-paid ~
lv-due lv-gross lv-tax lv-frt lv-disc lv-inv-date Btn_OK Btn_Cancel RECT-22 ~
RECT-23 
&Scoped-Define DISPLAYED-OBJECTS lv-comp lv-cust-no lv-name lv-inv-no ~
lv-net lv-paid lv-due lv-gross lv-tax lv-frt lv-disc lv-inv-date 

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

DEFINE VARIABLE lv-comp AS CHARACTER FORMAT "X(3)":U INITIAL "001" 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE lv-cust-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Customer#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE lv-disc AS DECIMAL FORMAT "->>>>,>>9.99":U INITIAL 0 
     LABEL "Discount" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lv-due AS DECIMAL FORMAT "->>>>,>>9.99":U INITIAL 0 
     LABEL "Due" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lv-frt AS DECIMAL FORMAT "->>>>,>>9.99":U INITIAL 0 
     LABEL "Freight" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lv-gross AS DECIMAL FORMAT "->>>>,>>9.99":U INITIAL 0 
     LABEL "Gross" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lv-inv-date AS DATE FORMAT "99/99/9999":U 
     LABEL "Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lv-inv-no AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE lv-name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1 NO-UNDO.

DEFINE VARIABLE lv-net AS DECIMAL FORMAT "->>>>,>>9.99":U INITIAL 0 
     LABEL "Net" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lv-paid AS DECIMAL FORMAT "->>>>,>>9.99":U INITIAL 0 
     LABEL "Paid" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lv-tax AS DECIMAL FORMAT "->>>>,>>9.99":U INITIAL 0 
     LABEL "Tax Amt" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 95 BY 4.05.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 11.43.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     lv-comp AT ROW 1.48 COL 16 COLON-ALIGNED
     lv-cust-no AT ROW 2.67 COL 16 COLON-ALIGNED
     lv-name AT ROW 2.67 COL 38 COLON-ALIGNED NO-LABEL
     lv-inv-no AT ROW 3.86 COL 16 COLON-ALIGNED
     lv-net AT ROW 5.52 COL 16 COLON-ALIGNED
     lv-paid AT ROW 6.71 COL 16 COLON-ALIGNED
     lv-due AT ROW 7.91 COL 16 COLON-ALIGNED
     lv-gross AT ROW 9.1 COL 16 COLON-ALIGNED
     lv-tax AT ROW 10.29 COL 16 COLON-ALIGNED
     lv-frt AT ROW 11.48 COL 16 COLON-ALIGNED
     lv-disc AT ROW 12.67 COL 16 COLON-ALIGNED
     lv-inv-date AT ROW 14.57 COL 16 COLON-ALIGNED
     Btn_OK AT ROW 17.91 COL 21
     Btn_Cancel AT ROW 17.91 COL 58
     RECT-22 AT ROW 1 COL 1
     RECT-23 AT ROW 5.29 COL 2
     SPACE(5.99) SKIP(2.94)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Update AR Invoice"
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

/* SETTINGS FOR FILL-IN lv-name IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Update AR Invoice */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    MESSAGE "Are you sure you want to update? " 
        VIEW-AS ALERT-BOX WARNING BUTTON YES-NO UPDATE ll-ans AS LOG.
    IF ll-ans THEN DO:

       FIND FIRST ar-inv WHERE ar-inv.company = lv-comp 
                       AND ar-inv.cust-no = lv-cust-no
                       AND ar-inv.inv-no = lv-inv-no
                       NO-ERROR.
       IF NOT AVAIL ar-inv THEN DO:
          MESSAGE "There is no invoice for the customer."
                  VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO lv-cust-no.
          RETURN NO-APPLY.
        END.

       ASSIGN ar-inv.net = lv-net
              ar-inv.due = lv-due
              ar-inv.paid = lv-paid
              ar-inv.gross = lv-gross
              ar-inv.tax-amt = lv-tax
              ar-inv.freight = lv-frt
              ar-inv.disc-taken = lv-disc
              ar-inv.inv-date = lv-inv-date.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-comp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-comp Dialog-Frame
ON LEAVE OF lv-comp IN FRAME Dialog-Frame /* Company */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-cust-no Dialog-Frame
ON LEAVE OF lv-cust-no IN FRAME Dialog-Frame /* Customer# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-disc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-disc Dialog-Frame
ON LEAVE OF lv-disc IN FRAME Dialog-Frame /* Discount */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-due
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-due Dialog-Frame
ON LEAVE OF lv-due IN FRAME Dialog-Frame /* Due */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-frt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-frt Dialog-Frame
ON LEAVE OF lv-frt IN FRAME Dialog-Frame /* Freight */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-gross
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-gross Dialog-Frame
ON LEAVE OF lv-gross IN FRAME Dialog-Frame /* Gross */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-inv-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-inv-date Dialog-Frame
ON LEAVE OF lv-inv-date IN FRAME Dialog-Frame /* Invoice Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-inv-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-inv-no Dialog-Frame
ON LEAVE OF lv-inv-no IN FRAME Dialog-Frame /* Invoice# */
DO:
   IF LASTKEY = -1 THEN RETURN.
   ASSIGN {&self-name}.

   FIND FIRST ar-inv WHERE ar-inv.company = lv-comp 
                       AND ar-inv.cust-no = lv-cust-no
                       AND ar-inv.inv-no = lv-inv-no
                       NO-LOCK NO-ERROR.
   IF NOT AVAIL ar-inv THEN DO:
      MESSAGE "There is no invoice for the customer."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO lv-cust-no.
      RETURN NO-APPLY.
   END.

   ASSIGN lv-net = ar-inv.net
          lv-due = ar-inv.due
          lv-paid = ar-inv.paid
          lv-gross = ar-inv.gross
          lv-tax = ar-inv.tax-amt
          lv-frt = ar-inv.freight
          lv-inv-date = ar-inv.inv-date
          lv-disc = ar-inv.disc-taken.

   DISPLAY lv-net
           lv-due
           lv-paid lv-gross lv-tax lv-frt lv-inv-date lv-disc
           WITH FRAME {&FRAME-NAME}.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-net
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-net Dialog-Frame
ON LEAVE OF lv-net IN FRAME Dialog-Frame /* Net */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-paid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-paid Dialog-Frame
ON LEAVE OF lv-paid IN FRAME Dialog-Frame /* Paid */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-tax
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-tax Dialog-Frame
ON LEAVE OF lv-tax IN FRAME Dialog-Frame /* Tax Amt */
DO:
  ASSIGN {&self-name}.
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
  DISPLAY lv-comp lv-cust-no lv-name lv-inv-no lv-net lv-paid lv-due lv-gross 
          lv-tax lv-frt lv-disc lv-inv-date 
      WITH FRAME Dialog-Frame.
  ENABLE lv-comp lv-cust-no lv-inv-no lv-net lv-paid lv-due lv-gross lv-tax 
         lv-frt lv-disc lv-inv-date Btn_OK Btn_Cancel RECT-22 RECT-23 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

