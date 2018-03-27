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
/*          This .W file was created with the Progress AppBulder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.

def new shared var v-tax-rate as dec.
def new shared var v-frt-tax-rate as dec.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-25 begin_cust tb_acct-bal tb_avg-days ~
tb_high-bal Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust tb_acct-bal tb_avg-days ~
tb_high-bal 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 FONT 6.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 FONT 6.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)":U 
     LABEL "Customer#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 64 BY 5.48.

DEFINE VARIABLE tb_acct-bal AS LOGICAL INITIAL yes 
     LABEL "Recalculate Account Balances" 
     VIEW-AS TOGGLE-BOX
     SIZE 42 BY .81 NO-UNDO.

DEFINE VARIABLE tb_avg-days AS LOGICAL INITIAL no 
     LABEL "Recalculate Average Days To Pay" 
     VIEW-AS TOGGLE-BOX
     SIZE 48 BY .81 NO-UNDO.

DEFINE VARIABLE tb_high-bal AS LOGICAL INITIAL no 
     LABEL "Recalculate High Balance" 
     VIEW-AS TOGGLE-BOX
     SIZE 48 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     begin_cust AT ROW 1.86 COL 15 COLON-ALIGNED HELP
          "Please enter customer number to be processed"
     tb_acct-bal AT ROW 3.48 COL 6 WIDGET-ID 2
     tb_avg-days AT ROW 4.48 COL 6 WIDGET-ID 4
     tb_high-bal AT ROW 5.43 COL 6 WIDGET-ID 6
     Btn_OK AT ROW 6.95 COL 11
     Btn_Cancel AT ROW 6.95 COL 41
     "Enter * for all customers" VIEW-AS TEXT
          SIZE 28 BY 1 AT ROW 1.86 COL 33
          FONT 6
     RECT-25 AT ROW 1.24 COL 1
     SPACE(0.19) SKIP(1.75)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Recalculate Customer Acct Balances"
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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Recalculate Customer Acct Balances */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust Dialog-Frame
ON LEAVE OF begin_cust IN FRAME Dialog-Frame /* Customer# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
  apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  DEF VAR lProcess as log INIT NO NO-UNDO.
/*   DEF VAR cMessage AS CHAR NO-UNDO.                                       */
/*                                                                           */
/*                                                                           */
/*   IF tb_acct-bal THEN DO:                                                 */
/*       cMessage = "recalculate customer account balances".                 */
/*       IF tb_avg-days THEN                                                 */
/*           cMessage                                                        */
/*   END.                                                                    */
/*                                                                           */
/*   message "Are you sure you want to " + trim(FRAME {&FRAME-NAME}:title) + */
/*           " within the selection parameters?"                             */
/*           view-as alert-box question button yes-no update v-process.      */
  RUN validateInputs(OUTPUT lProcess).  
  IF lProcess THEN RUN runProcess.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_acct-bal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_acct-bal Dialog-Frame
ON VALUE-CHANGED OF tb_acct-bal IN FRAME Dialog-Frame /* Recalculate Account Balances */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_avg-days
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_avg-days Dialog-Frame
ON VALUE-CHANGED OF tb_avg-days IN FRAME Dialog-Frame /* Recalculate Average Days To Pay */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_high-bal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_high-bal Dialog-Frame
ON VALUE-CHANGED OF tb_high-bal IN FRAME Dialog-Frame /* Recalculate High Balance */
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

  FIND cust WHERE ROWID(cust) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL cust THEN begin_cust = cust.cust-no.

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
  DISPLAY begin_cust tb_acct-bal tb_avg-days tb_high-bal 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-25 begin_cust tb_acct-bal tb_avg-days tb_high-bal Btn_OK 
         Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE runProcess Dialog-Frame 
PROCEDURE runProcess :
/* --------------------------------------------------- ar/updcust.p 11/96 JLF */
/* -------------------------------------------------------------------------- */

DEF BUFFER b-cust   FOR cust.
DEF BUFFER b-oe-ord FOR oe-ord.

DEF VAR fstat LIKE oe-ord.stat INIT "" NO-UNDO.
DEF VAR tstat LIKE fstat NO-UNDO.
DEF VAR ld-due AS DEC NO-UNDO.
DEF VAR ld-paid AS DEC NO-UNDO.
DEF VAR ld-ord-bal AS DEC NO-UNDO.


SESSION:SET-WAIT-STATE("General").

FOR EACH cust NO-LOCK
    WHERE cust.company EQ cocode
      AND (begin_cust EQ "*" OR cust.cust-no EQ begin_cust):

  STATUS DEFAULT "Please wait...  Updating Customer: " + TRIM(cust.cust-no).

    IF tb_acct-bal THEN DO:    
      ASSIGN
       ld-due  = 0
       ld-paid = 0.
    
      FOR EACH ar-inv NO-LOCK
          WHERE ar-inv.company EQ cust.company
            AND ar-inv.cust-no EQ cust.cust-no
            AND ar-inv.posted  EQ YES
/*             AND ar-inv.due     LT 0 */
          USE-INDEX posted-due:
        ld-due = ld-due + ar-inv.due.
      END.
    
/*       FOR EACH ar-inv NO-LOCK                  */
/*           WHERE ar-inv.company EQ cust.company */
/*             AND ar-inv.cust-no EQ cust.cust-no */
/*             AND ar-inv.posted  EQ YES          */
/*             AND ar-inv.due     GT 0            */
/*           USE-INDEX posted-due:                */
/*         ld-due = ld-due + ar-inv.due.          */
/*       END.                                     */
    
      FOR EACH ar-cashl NO-LOCK
          WHERE ar-cashl.company    EQ cust.company
            AND ar-cashl.posted     EQ YES
            AND ar-cashl.cust-no    EQ cust.cust-no
            AND ar-cashl.on-account EQ YES
          USE-INDEX inv-no:
        ld-paid = ld-paid + ar-cashl.amt-paid.
      END.
    
      RUN ar/updcust1.p (YES, BUFFER cust, OUTPUT ld-ord-bal).
    
      DO TRANSACTION:
        FIND b-cust EXCLUSIVE WHERE ROWID(b-cust) EQ ROWID(cust)
            NO-ERROR NO-WAIT.
          
        IF AVAIL b-cust THEN
          ASSIGN
           b-cust.on-account = ld-paid
           b-cust.acc-bal    = ld-due - ld-paid
           b-cust.ord-bal    = ld-ord-bal.
      END.
    END. /*if tb_acct-bal*/
    
    IF tb_avg-days THEN 
        RUN ar/calcAvgDays.p (ROWID(cust), NO, 0).
    IF tb_high-bal THEN
        RUN ar/calcHighBal.p (ROWID(cust)).
END. /*each cust*/

SESSION:SET-WAIT-STATE("").  
/*                                                                      */
/* MESSAGE TRIM(FRAME {&FRAME-NAME}:TITLE) + " Process Is Completed..." */
/*     VIEW-AS ALERT-BOX.                                               */
MESSAGE "Recalculation Process Is Completed."
    VIEW-AS ALERT-BOX.

APPLY "close" TO THIS-PROCEDURE.

/* end ---------------------------------- copr. 2002  Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateInputs Dialog-Frame 
PROCEDURE validateInputs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER oplProcess AS LOG INIT NO.

DEF VAR cMessage AS CHAR NO-UNDO.
    
IF tb_acct-bal THEN DO:
    cMessage = "recalculate account balances".
    IF tb_avg-days AND tb_high-bal THEN
        cMessage = cMessage + ", average days to pay and high balance".
    IF tb_avg-days AND NOT tb_high-bal THEN
        cMessage = cMessage + " and average days to pay".
    IF NOT tb_avg-days AND tb_high-bal THEN
        cMessage = cMessage + " and high balance".
END.
ELSE IF tb_avg-days OR tb_high-bal THEN DO: 
    IF tb_avg-days AND tb_high-bal THEN
        cMessage = cMessage + "recalculate average days to pay and high balance".
    IF tb_avg-days AND NOT tb_high-bal THEN 
        cMessage = "recalculate average days to pay".
    IF NOT tb_avg-days AND tb_high-bal THEN
        cMessage = "recalculate high balance".
END.
ELSE DO:
    MESSAGE "You must select a recalculation option."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
END.
IF begin_cust = "*" THEN
    cMessage = cMessage + " for all customers?".
ELSE IF begin_cust NE "" THEN
    cMessage = cMessage + " for customer " + begin_cust + "?".
ELSE DO:
    MESSAGE "You must enter a customer."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    APPLY "entry" TO begin_cust IN FRAME {&FRAME-NAME}.
    RETURN.
END.
    
MESSAGE "Are you sure you want to " + cMessage
    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE oplProcess.
/*   message "Are you sure you want to " + trim(FRAME {&FRAME-NAME}:title) + */
/*           " within the selection parameters?"                             */
/*           view-as alert-box question button yes-no update oplProcess.     */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

