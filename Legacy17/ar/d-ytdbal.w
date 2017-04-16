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

DEF VAR start-date AS DATE EXTENT 2 INIT 01/01/0001.
DEF VAR end-date LIKE start-date INIT 01/01/0001.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_cust begin_year Btn_OK Btn_Cancel ~
RECT-25 
&Scoped-Define DISPLAYED-OBJECTS begin_cust begin_year 

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
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_year AS INTEGER FORMAT ">>>>":U INITIAL 0 
     LABEL "Which Year" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 69 BY 4.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     begin_cust AT ROW 2.19 COL 16 COLON-ALIGNED HELP
          "Please enter customer number to be processed"
     begin_year AT ROW 4.1 COL 16 COLON-ALIGNED
     Btn_OK AT ROW 6.71 COL 14
     Btn_Cancel AT ROW 6.71 COL 43
     RECT-25 AT ROW 1.24 COL 1
     "Enter * for all customers" VIEW-AS TEXT
          SIZE 30 BY 1 AT ROW 2.19 COL 39
     "is Last Year's Data?" VIEW-AS TEXT
          SIZE 30 BY 1 AT ROW 4.1 COL 39
     SPACE(1.19) SKIP(3.60)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Recalculate Customer Prior Yr/YTD Balances"
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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Recalculate Customer Prior Yr/YTD Balances */
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
  DEF VAR ll-process AS LOG INIT NO NO-UNDO.
  DEF VAR li AS INT NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  DO li = 1 TO 2:
    FIND FIRST period
        WHERE period.company EQ cocode
          AND period.yr      EQ begin_year + (li - 1)
        NO-LOCK NO-ERROR.
    IF AVAIL period THEN DO:
      start-date[li] = period.pst.
      IF li EQ 1 THEN
      FIND LAST period
          WHERE period.company EQ cocode
            AND period.yr      EQ begin_year
          NO-LOCK NO-ERROR.
      ELSE
      find LAST period
          WHERE period.company EQ cocode
            AND period.pstat   EQ YES
          NO-LOCK NO-ERROR.

      IF AVAIL period THEN end-date[li] = period.pend.
    END.

    IF NOT AVAIL period THEN
      MESSAGE "No start and/or end periods found for " +
              (IF begin_year EQ begin_year + (li - 1) THEN "last" ELSE "this")
              " year, balances will not be calculated"
              VIEW-AS ALERT-BOX.
  END.

  MESSAGE "Are you sure you want to " + TRIM(FRAME {&FRAME-NAME}:TITLE) +
          " within the selection parameters?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-process.
          
  IF ll-process THEN RUN run-process.
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

  begin_year = year(today) - 1.

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
  DISPLAY begin_cust begin_year 
      WITH FRAME Dialog-Frame.
  ENABLE begin_cust begin_year Btn_OK Btn_Cancel RECT-25 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process Dialog-Frame 
PROCEDURE run-process :
/* ------------------------------------------------ sys/ref/updytd.p 1/97 FWK */
/* -------------------------------------------------------------------------- */

/* if start-date = 01/01/0001 or end-date = 01/01/0001 then undo, leave. */

if start-date[1] ne 01/01/0001 and end-date[1] ne 01/01/0001 then
do:

for each cust
    where cust.company = cocode
      and (if begin_cust = "*" then yes
           else cust.cust-no = begin_cust):

  assign cust.lyytd-msf = 0
/*         cust.sales[6] = 0 */
         cust.lyr-sales = 0       
         cust.cost[6]   = 0
         cust.comm[6]   = 0.

  status default "Please wait...  Updating Customer: " + trim(cust.cust-no).
  for each ar-ledger where ar-ledger.company = cocode and
                           ar-ledger.tr-date >= start-date[1] and
                           ar-ledger.tr-date <= end-date[1] and
                           ar-ledger.cust-no = cust.cust-no and
                           ar-ledger.ref-num BEGINS "INV#" no-lock:

    find first ar-inv where ar-inv.company = cocode and
                            ar-inv.cust-no = cust.cust-no and
                            ar-inv.posted and
                            ar-inv.inv-no = INT(SUBSTRING(ar-ledger.ref-num,6,LENGTH(ar-ledger.ref-num)))
                            USE-INDEX posted no-lock no-error.

    if avail ar-inv then
    do:
      for each ar-invl where ar-invl.company = cocode and
                             ar-invl.cust-no = ar-inv.cust-no and
                             ar-invl.inv-no = ar-inv.inv-no
                             use-index inv-no no-lock:
        if ar-invl.amt-msf <> 0 then
          assign cust.lyytd-msf = cust.lyytd-msf + ar-invl.amt-msf.
        else
        do:
          find first itemfg where itemfg.company = cocode and
                                  itemfg.i-no = ar-invl.i-no
                                  use-index i-no no-lock no-error.
          if avail itemfg then
            assign cust.lyytd-msf = cust.lyytd-msf +
                                    ((ar-invl.inv-qty / 1000) * itemfg.t-sqft).
        end.
      end.

/*      assign cust.sales[6] = cust.sales[6] + if (ar-inv.net - ar-inv.tax-amt) eq ? then 0 else
                                                (ar-inv.net - ar-inv.tax-amt)
*/
/*
      assign cust.lyr-sales = cust.lyr-sales + if (ar-inv.net - ar-inv.tax-amt) eq ? then 0 else
                                                (ar-inv.net - ar-inv.tax-amt)
*/
      assign cust.lyr-sales = cust.lyr-sales + 
             if ((if ar-inv.gross gt ar-inv.net then ar-inv.gross else ar-inv.net) - ar-inv.tax-amt) eq ? then 0 
             else ((if ar-inv.gross gt ar-inv.net then ar-inv.gross else ar-inv.net) - ar-inv.tax-amt)
             cust.cost[6]  = cust.cost[6] + if ar-inv.t-cost eq ? then 0 else
                                               ar-inv.t-cost
             cust.comm[6]  = cust.comm[6] + if ar-inv.t-comm eq ? then 0 else
                                               ar-inv.t-comm.

    end. /* if avail ar-inv */

  end. /* for each ar-ledger INV */

  for each ar-ledger where ar-ledger.company = cocode and
                           ar-ledger.tr-date >= start-date[1] and
                           ar-ledger.tr-date <= end-date[1] and
                           ar-ledger.cust-no = cust.cust-no and
                           ar-ledger.ref-num BEGINS "Memo#" no-lock:

    find first ar-cash where ar-cash.company = cocode and
                            ar-cash.cust-no = cust.cust-no and
                            ar-cash.posted and
                            ar-cash.check-no = INT(SUBSTRING(ar-ledger.ref-num,6,8))
                            USE-INDEX posted no-lock no-error.

      for each ar-cashl where ar-cashl.company = cocode and
                              ar-cashl.c-no = ar-cash.c-no
                              use-index c-no no-lock:
/*        assign cust.sales[6] = cust.sales[6] +
                                (ar-cashl.amt-paid - ar-cashl.amt-disc).
*/
        assign cust.lyr-sales = cust.lyr-sales +
                                (ar-cashl.amt-paid - ar-cashl.amt-disc).
      end.

  end. /* for each ar-ledger MEMO */

end. /* for each cust */
status default "".
end. /* if start-date[1] end-date[1] ne 01/01/0001 */

/****************************/
/*  Recalculate YTD Numbers */
/****************************/

if start-date[1] = 01/01/0001 or end-date[1] = 01/01/0001 then undo, leave.

for each cust
    where cust.company = cocode
      and (if begin_cust = "*" then yes
           else cust.cust-no = begin_cust):

  assign cust.ytd-msf  = 0
/*
         cust.sales[13] = 0
*/
         cust.ytd-sales = 0
         cust.cost[5]   = 0
         cust.comm[5]   = 0.

  status default "Please wait...  Updating Customer: " + trim(cust.cust-no).
  for each ar-ledger where ar-ledger.company = cocode and
                           ar-ledger.tr-date >= start-date[2] and
                           ar-ledger.tr-date <= end-date[2] and
                           ar-ledger.cust-no = cust.cust-no and
                           ar-ledger.ref-num BEGINS "INV#" no-lock:

    find first ar-inv where ar-inv.company = cocode and
                            ar-inv.cust-no = cust.cust-no and
                            ar-inv.posted and
                            ar-inv.inv-no = INT(SUBSTRING(ar-ledger.ref-num,6,LENGTH(ar-ledger.ref-num)))
                            USE-INDEX posted no-lock no-error.

    if avail ar-inv then
    do:
      for each ar-invl where ar-invl.company = cocode and
                             ar-invl.cust-no = ar-inv.cust-no and
                             ar-invl.inv-no = ar-inv.inv-no
                             use-index inv-no no-lock:
        if ar-invl.amt-msf <> 0 then
          assign cust.ytd-msf = cust.ytd-msf + ar-invl.amt-msf.
        else
        do:
          find first itemfg where itemfg.company = cocode and
                                  itemfg.i-no = ar-invl.i-no
                                  use-index i-no no-lock no-error.
          if avail itemfg then
            assign cust.ytd-msf = cust.ytd-msf +
                                    ((ar-invl.inv-qty / 1000) * itemfg.t-sqft).
        end.
      end.

/*      assign cust.sales[13] = cust.sales[13] + (ar-inv.net - ar-inv.tax-amt)
*/
/*      assign cust.ytd-sales = cust.ytd-sales + (ar-inv.net - ar-inv.tax-amt) */
      assign cust.ytd-sales = cust.ytd-sales + 
                    ((if ar-inv.gross gt ar-inv.net then ar-inv.gross else ar-inv.net) - ar-inv.tax-amt)
             cust.cost[5] = cust.cost[5] + ar-inv.t-cost
             cust.comm[5] = cust.comm[5] + ar-inv.t-comm.
    end. /* if avail ar-inv */
  end. /* for each ar-ledger INV */

  for each ar-ledger where ar-ledger.company = cocode and
                           ar-ledger.tr-date >= start-date[2] and
                           ar-ledger.tr-date <= end-date[2] and
                           ar-ledger.cust-no = cust.cust-no and
                           ar-ledger.ref-num BEGINS "Memo#" no-lock:
                           
    find first ar-cash where ar-cash.company = cocode and
                            ar-cash.cust-no = cust.cust-no and
                            ar-cash.posted and
                            ar-cash.check-date ge start-date[2] and
                            ar-cash.check-date le end-date[2] and
                            ar-cash.check-no = INT(SUBSTRING(ar-ledger.ref-num,6,8))
                            USE-INDEX posted no-lock no-error.

      for each ar-cashl where ar-cashl.company = cocode and
                              ar-cashl.c-no = ar-cash.c-no 
                              use-index c-no no-lock:
/*         assign cust.sales[13] = cust.sales[13] +
                                (ar-cashl.amt-paid - ar-cashl.amt-disc).
*/
         assign cust.ytd-sales = cust.ytd-sales +
                                (ar-cashl.amt-paid - ar-cashl.amt-disc).
      end.
  end. /* for each ar-ledger MEMO */

  RUN ar/ptdsales (ROWID(cust)).
end. /* for each cust */

session:set-wait-state("").  

message trim(FRAME {&FRAME-NAME}:title) + " Process Is Completed." view-as alert-box.

apply "close" to this-procedure.

/* end ---------------------------------- copr. 2002  Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

