&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
 
{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 begin_cust-no end_cust-no begin_Inv ~
end_inv end_date begin_date tgUpdateTax btn-ok btn-cancel cProcessStatus 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no begin_Inv ~
end_inv end_date begin_date tgUpdateTax cProcessStatus 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" 
     LABEL "From Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_Inv AS INTEGER FORMAT ">>>>>>>>>" INITIAL 0 
     LABEL "From Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999" 
     LABEL "To Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_inv AS INTEGER FORMAT ">>>>>>>>>" INITIAL 999999999
     LABEL "To Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.      

DEFINE VARIABLE cProcessStatus AS CHARACTER FORMAT "X(200)"       
     VIEW-AS FILL-IN 
     SIZE 56 BY 1.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 8.76.

DEFINE VARIABLE tgUpdateTax AS LOGICAL INITIAL no 
     LABEL "Update Tax" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     begin_cust-no AT ROW 3.14 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 6
     end_cust-no AT ROW 3.14 COL 71 COLON-ALIGNED HELP
          "Enter Ending Customer Number" WIDGET-ID 16
     begin_Inv AT ROW 4.52 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Invoice Number" WIDGET-ID 108
     end_inv AT ROW 4.52 COL 71 COLON-ALIGNED HELP
          "Enter Beginning Invoice Number" WIDGET-ID 110      
     begin_date AT ROW 6.1 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Date Number" WIDGET-ID 112
     end_date AT ROW 6.05 COL 71 COLON-ALIGNED HELP
          "Enter Beginning Date Number" WIDGET-ID 114     
     tgUpdateTax AT ROW 7.67 COL 30 WIDGET-ID 116
     btn-ok AT ROW 10.57 COL 30.2 WIDGET-ID 14
     btn-cancel AT ROW 10.57 COL 60.4 WIDGET-ID 12
     cProcessStatus AT ROW 11.87 COL 10.2 NO-LABEL WIDGET-ID 18
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5 WIDGET-ID 36
          BGCOLOR 2 
     RECT-7 AT ROW 1.05 COL 2 WIDGET-ID 38
     SPACE(0.79) SKIP(3.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Check Invoice Register" WIDGET-ID 100.


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

ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       begin_Inv:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_inv:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Check Invoice Register */
DO:
DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.
DEF VAR ls-cur-val AS CHAR NO-UNDO.
DEF VAR char-val AS CHAR NO-UNDO.

   lw-focus = FOCUS.

   case lw-focus:name :

       when "begin_cust-no" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-cust.w (cocode,ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* cust-no*/  
       when "end_cust-no" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-cust.w (cocode,ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* cust-no*/  
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Check Invoice Register */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no Dialog-Frame
ON LEAVE OF begin_cust-no IN FRAME Dialog-Frame /* Beginning Customer# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date Dialog-Frame
ON LEAVE OF begin_date IN FRAME Dialog-Frame /* From Invoice Date */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_Inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_Inv Dialog-Frame
ON LEAVE OF begin_Inv IN FRAME Dialog-Frame /* From Invoice# */
DO:
   assign {&self-name}.
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


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok Dialog-Frame
ON CHOOSE OF btn-ok IN FRAME Dialog-Frame /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.
        
  run run-process.
  
  cProcessStatus:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no Dialog-Frame
ON LEAVE OF end_cust-no IN FRAME Dialog-Frame /* Ending Customer# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date Dialog-Frame
ON LEAVE OF end_date IN FRAME Dialog-Frame /* To Invoice Date */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_inv Dialog-Frame
ON LEAVE OF end_inv IN FRAME Dialog-Frame /* To Invoice# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpd.i}
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
 
  RUN enable_UI.
   {methods/nowait.i}
   DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    cProcessStatus = "" .
    cProcessStatus:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
    APPLY "entry" TO begin_cust-no.
  END.
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
  DISPLAY begin_cust-no end_cust-no begin_Inv end_inv end_date begin_date 
          tgUpdateTax cProcessStatus 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-7 begin_cust-no end_cust-no begin_Inv end_inv end_date begin_date 
         tgUpdateTax btn-ok btn-cancel cProcessStatus 
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
    DEFINE VARIABLE hPostInvoices AS HANDLE NO-UNDO.
    DEFINE VARIABLE iCountProcess AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCountValid AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCountPost AS INTEGER NO-UNDO.
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
        
    cProcessStatus:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Invoice Processing ...".
    
    RUN oe/PostInvoices.p PERSISTENT SET hPostInvoices.
         
    RUN ValidateInvoices IN hPostInvoices (cocode,
                                        begin_Inv,
                                        end_inv,
                                        DATE(begin_date),
                                        date(end_date),
                                        begin_cust-no,
                                        end_cust-no,
                                        date(TODAY),
                                        INPUT tgUpdateTax:CHECKED, /* Update Tax */
                                        OUTPUT iCountProcess,
                                        OUTPUT iCountValid,
                                        OUTPUT iCountPost,
                                        OUTPUT lError,
                                        OUTPUT cMessage) .    
    MESSAGE cMessage STRING(iCountProcess) "Invoices processed," STRING(iCountValid) "Approved," STRING(iCountProcess - iCountValid) "Problems."  VIEW-AS ALERT-BOX INFO .
    
    IF VALID-HANDLE(hPostInvoices) THEN
    DELETE OBJECT hPostInvoices.    
    
    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
    cProcessStatus:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

