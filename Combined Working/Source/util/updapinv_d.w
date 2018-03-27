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
{custom/globdefs.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-32 RECT-33 lv-vend-no lv-inv-no ~
lv-inv-date lv-due-date lv-gross lv-net lv-paid lv-due Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS lv-vend-no lv-inv-no lv-inv-date ~
lv-due-date lv-gross lv-net lv-paid lv-due 

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

DEFINE VARIABLE lv-due AS DECIMAL FORMAT "->,>>>,>>9.99":U INITIAL 0 
     LABEL "Due" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE lv-due-date AS DATE FORMAT "99/99/9999":U 
     LABEL "Due Date" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE lv-gross AS DECIMAL FORMAT "->,>>>,>>9.99":U INITIAL 0 
     LABEL "Gross" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE lv-inv-date AS DATE FORMAT "99/99/9999":U 
     LABEL "Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE lv-inv-no AS CHARACTER FORMAT "X(256)":U 
     LABEL "Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE lv-net AS DECIMAL FORMAT "->,>>>,>>9.99":U INITIAL 0 
     LABEL "Net" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE lv-paid AS DECIMAL FORMAT "->,>>>,>>9.99":U INITIAL 0 
     LABEL "Paid" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE lv-vend-no AS CHARACTER FORMAT "X(256)":U 
     LABEL "Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-32
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 65 BY 3.1.

DEFINE RECTANGLE RECT-33
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 65 BY 8.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     lv-vend-no AT ROW 1.71 COL 15 COLON-ALIGNED
     lv-inv-no AT ROW 2.91 COL 15 COLON-ALIGNED
     lv-inv-date AT ROW 4.81 COL 15 COLON-ALIGNED
     lv-due-date AT ROW 6 COL 15 COLON-ALIGNED
     lv-gross AT ROW 7.19 COL 15 COLON-ALIGNED
     lv-net AT ROW 8.14 COL 15 COLON-ALIGNED
     lv-paid AT ROW 9.1 COL 15 COLON-ALIGNED
     lv-due AT ROW 10.05 COL 15 COLON-ALIGNED
     Btn_OK AT ROW 13.38 COL 18
     Btn_Cancel AT ROW 13.38 COL 51
     RECT-32 AT ROW 1.24 COL 3
     RECT-33 AT ROW 4.57 COL 3
     SPACE(23.79) SKIP(3.65)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Update AP Invoice"
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
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Update AP Invoice */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    MESSAGE "Are you sure to update ? " VIEW-AS ALERT-BOX WARNING BUTTON YES-NO
        UPDATE ll-ans AS LOG.
    IF ll-ans THEN DO:
       ASSIGN lv-inv-date lv-due-date lv-gross lv-net lv-paid lv-due.

       FIND CURRENT ap-inv EXCLUSIVE-LOCK.

       FIND FIRST ap-ledger
           WHERE ap-ledger.company  EQ ap-inv.company
             AND ap-ledger.vend-no  EQ ap-inv.vend-no
             AND ap-ledger.ref-date EQ ap-inv.inv-date
             AND ap-ledger.refnum   EQ "INV# " + ap-inv.inv-no
           EXCLUSIVE NO-ERROR.

       ASSIGN
        ap-inv.inv-date = lv-inv-date
        ap-inv.due-date = lv-due-date
        ap-inv.gross    = lv-gross
        ap-inv.net      = lv-net
        ap-inv.paid     = lv-paid
        ap-inv.due      = lv-due.

       IF AVAIL ap-ledger THEN
         ASSIGN
          ap-ledger.amt      = ap-inv.net
          ap-ledger.ref-date = ap-inv.inv-date.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-inv-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-inv-no Dialog-Frame
ON LEAVE OF lv-inv-no IN FRAME Dialog-Frame /* Invoice# */
DO:
  
   IF LASTKEY = -1 THEN RETURN.
   ASSIGN lv-vend-no lv-inv-no.

   FIND FIRST ap-inv WHERE ap-inv.company = g_company AND
                          ap-inv.vend-no = Lv-vend-no AND
                          ap-inv.inv-no = Lv-inv-no NO-LOCK NO-ERROR.
   IF NOT AVAIL ap-inv THEN DO:
      MESSAGE "Invalid Invoice# for the vendor entered." VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.

   ASSIGN lv-inv-date = ap-inv.inv-date
          lv-due-date = ap-inv.due-date
          lv-gross = ap-inv.gross
          lv-net = ap-inv.net
          lv-paid = ap-inv.paid
          lv-due = ap-inv.due.
   DISPLAY lv-inv-date lv-due-date lv-gross lv-net lv-net
           lv-paid lv-due WITH FRAME {&FRAME-NAME}.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
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
  DISPLAY lv-vend-no lv-inv-no lv-inv-date lv-due-date lv-gross lv-net lv-paid 
          lv-due 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-32 RECT-33 lv-vend-no lv-inv-no lv-inv-date lv-due-date lv-gross 
         lv-net lv-paid lv-due Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

