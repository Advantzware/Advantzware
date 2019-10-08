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

def var fisc-yr as int format "9999".
fisc-yr = year(today) - 1.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_vend begin_year Btn_OK Btn_Cancel ~
RECT-25 
&Scoped-Define DISPLAYED-OBJECTS begin_vend begin_year 

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

DEFINE VARIABLE begin_vend AS CHARACTER FORMAT "X(8)":U 
     LABEL "Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE begin_year AS INTEGER FORMAT ">>>>":U INITIAL 0 
     LABEL "Which Year" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 64 BY 4.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     begin_vend AT ROW 2.19 COL 17 COLON-ALIGNED HELP
          "Please enter vendor number to be processed"
     begin_year AT ROW 4.1 COL 17 COLON-ALIGNED
     Btn_OK AT ROW 6.71 COL 11
     Btn_Cancel AT ROW 6.71 COL 41
     RECT-25 AT ROW 1.24 COL 1
     "Enter * for all vendors" VIEW-AS TEXT
          SIZE 26 BY 1 AT ROW 2.19 COL 34
          FONT 6
     "is Last Year's Data?" VIEW-AS TEXT
          SIZE 26 BY 1 AT ROW 4.1 COL 34
          FONT 6
     SPACE(5.19) SKIP(3.51)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Recalculate Vendor Last YR & YTD Balances"
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
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Recalculate Vendor Last YR  YTD Balances */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend Dialog-Frame
ON LEAVE OF begin_vend IN FRAME Dialog-Frame /* Vendor# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_year
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_year Dialog-Frame
ON LEAVE OF begin_year IN FRAME Dialog-Frame /* Which Year */
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
  DEF VAR v-process AS LOG INIT NO NO-UNDO.
  DEF VAR v AS INT NO-UNDO.


  DO v = 1 TO 2:
    FIND FIRST period
        WHERE period.company EQ cocode
          AND period.yr      EQ begin_year + (v - 1)
        NO-LOCK NO-ERROR.
    IF AVAIL period THEN DO:
      start-date[v] = period.pst.
      FIND LAST period
          WHERE period.company EQ cocode
            AND period.yr      EQ begin_year + (v - 1)
          NO-LOCK NO-ERROR.
      IF AVAIL period THEN end-date[v] = period.pend.
    END.

    IF NOT AVAIL period THEN
      MESSAGE "No start and/or end periods found for " +
              (IF begin_year EQ begin_year + (v - 1) THEN "last" ELSE "this")
              " year, balances will not be calculated"
              VIEW-AS ALERT-BOX.
  END.

  message "Are you sure you want to " + trim(FRAME {&FRAME-NAME}:title) +
          " within the selection parameters?"
          view-as alert-box question button yes-no update v-process.
          
  if v-process then run run-process.
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

  FIND vend WHERE ROWID(vend) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL vend THEN begin_vend = vend.vend-no.

  begin_year = YEAR(TODAY) - 1.

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
  DISPLAY begin_vend begin_year 
      WITH FRAME Dialog-Frame.
  ENABLE begin_vend begin_year Btn_OK Btn_Cancel RECT-25 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process Dialog-Frame 
PROCEDURE run-process :
/* ---------------------------------------------- sys/ref/updvend.p 03/97 FWK */
/* Vendor Last Yr and YTD Totals Recalculate                                  */
/* -------------------------------------------------------------------------- */

DEF BUFFER b-period FOR period.

session:set-wait-state("General").

/* Vend Processing  */
IF start-date[1] NE 01/01/0001 AND end-date[1] NE 01/01/0001 THEN
for each vend where vend.company = cocode and
                   (if begin_vend = "*" then yes
                    else vend.vend-no = begin_vend):

  assign vend.last-year = 0
         vend.lyytd     = 0.

  for each ap-ledger where ap-ledger.company = cocode and
                           ap-ledger.tr-date >= start-date[1] and
                           ap-ledger.tr-date <= end-date[1] and
                           ap-ledger.vend-no = vend.vend-no and
                           ap-ledger.refnum BEGINS "INV#" no-lock,
      FIRST period
      WHERE period.company EQ ap-ledger.company
        AND period.pst     LE ap-ledger.tr-date
        AND period.pend    GE ap-ledger.tr-date
      NO-LOCK:

    find first ap-inv where ap-inv.company = cocode and
                            ap-inv.vend-no = vend.vend-no and
                            ap-inv.posted and
                            ap-inv.inv-no = SUBSTRING(ap-ledger.refnum,6,LENGTH(ap-ledger.refnum))
                            USE-INDEX ap-inv no-lock no-error.

    if avail ap-inv then
    do:
      for each ap-invl where ap-invl.company = cocode and
                             ap-invl.inv-no = ap-inv.inv-no and
                             ap-invl.i-no = ap-inv.i-no
                             use-index i-no no-lock:
        if ap-invl.amt-msf <> 0 then
          assign vend.lyytd = vend.lyytd + ap-invl.amt-msf.
/*
                 vend.ytd-msf = vend.ytd-msf - ap-invl.amt-msf.
*/
        else
        do:
          find first itemfg where itemfg.company = cocode and
                                  itemfg.i-no = STRING(ap-invl.i-no)
                                  use-index i-no no-lock no-error.
          if avail itemfg then
            assign vend.lyytd = vend.lyytd +
                                    ((ap-invl.qty / 1000) * itemfg.t-sqft).
/*
                   vend.ytd-msf = vend.ytd-msf - 
                                    ((ap-invl.qty / 1000) * itemfg.t-sqft).
*/
        end.
      end.

    end. /* if avail ap-inv */

/*
    assign vend.purch[13] = vend.purch[13] - ap-ledger.amt
*/
    assign vend.last-year = vend.last-year + ap-ledger.amt.

  end. /* for each ap-ledger INV */

  for each ap-ledger where ap-ledger.company = cocode and
                           ap-ledger.tr-date >= start-date[1] and
                           ap-ledger.tr-date <= end-date[1] and
                           ap-ledger.vend-no = vend.vend-no and
                           (ap-ledger.refnum BEGINS "Memo#" or
                            ap-ledger.refnum BEGINS "Chk#") no-lock,
      FIRST period
      WHERE period.company EQ ap-ledger.company
        AND period.pst     LE ap-ledger.tr-date
        AND period.pend    GE ap-ledger.tr-date
      NO-LOCK:

/*
    assign vend.purch[13] = vend.purch[13] - ap-ledger.amt
*/
    assign vend.last-year = vend.last-year - ap-ledger.amt.

  end. /* for each ap-ledger MEMO */

end. /* for each vend */

/****************************/
/*  Recalculate YTD Numbers */
/****************************/
IF start-date[2] NE 01/01/0001 AND end-date[2] NE 01/01/0001 THEN
for each vend where vend.company = cocode and
                   (if begin_vend = "*" then yes
                    else vend.vend-no = begin_vend):

  status default "Please Wait...Updating Vendor: " + trim(vend.vend-no).

  assign vend.purch = 0
         vend.ytd-msf = 0.

  for each ap-ledger where ap-ledger.company = cocode and
                           ap-ledger.tr-date >= start-date[2] and
                           ap-ledger.tr-date <= end-date[2] and
                           ap-ledger.vend-no = vend.vend-no and
                           ap-ledger.refnum BEGINS "INV#" no-lock,

      FIRST period
      WHERE period.company EQ ap-ledger.company
        AND period.pst     LE ap-ledger.tr-date
        AND period.pend    GE ap-ledger.tr-date
      NO-LOCK:

    find first ap-inv where ap-inv.company = cocode and
                            ap-inv.vend-no = vend.vend-no and
                            ap-inv.posted and
                            ap-inv.inv-no = SUBSTRING(ap-ledger.refnum,6,LENGTH(ap-ledger.refnum))
                            USE-INDEX ap-inv no-lock no-error.

    if avail ap-inv then
    do:
      for each ap-invl where ap-invl.company = cocode and
                             ap-invl.inv-no = ap-inv.inv-no and
                             ap-invl.i-no = ap-inv.i-no
                             use-index i-no no-lock:
        if ap-invl.amt-msf <> 0 then
/*
          assign vend.lyytd = vend.lyytd + ap-invl.amt-msf.
*/
          assign vend.ytd-msf = vend.ytd-msf + ap-invl.amt-msf.
        else
        do:
          find first itemfg where itemfg.company = cocode and
                                  itemfg.i-no = STRING(ap-invl.i-no)
                                  use-index i-no no-lock no-error.
          if avail itemfg then
/*
            assign vend.lyytd = vend.lyytd +
                                    ((ap-invl.qty / 1000) * itemfg.t-sqft).
*/
            assign vend.ytd-msf = vend.ytd-msf + 
                                    ((ap-invl.qty / 1000) * itemfg.t-sqft).
        end.
      end.

    end. /* if avail ap-inv */

    assign 
     vend.purch[period.pnum] = vend.purch[period.pnum] + ap-ledger.amt
     vend.purch[13]          = vend.purch[13] + ap-ledger.amt.
/*
    assign vend.last-year = vend.last-year + ap-ledger.amt.
*/

  end. /* for each ap-ledger INV */

  for each ap-ledger where ap-ledger.company = cocode and
                           ap-ledger.tr-date >= start-date[2] and
                           ap-ledger.tr-date <= end-date[2] and
                           ap-ledger.vend-no = vend.vend-no and
                           (ap-ledger.refnum BEGINS "Memo#" /* or
                            ap-ledger.refnum BEGINS "Chk#" */) no-lock,

      FIRST period
      WHERE period.company EQ ap-ledger.company
        AND period.pst     LE ap-ledger.tr-date
        AND period.pend    GE ap-ledger.tr-date
      NO-LOCK:

    assign 
     vend.purch[period.pnum] = vend.purch[period.pnum] - ap-ledger.amt
     vend.purch[13]          = vend.purch[13] - ap-ledger.amt.
/*
    assign vend.last-year = vend.last-year + ap-ledger.amt.
*/

  end. /* for each ap-ledger MEMO */

  RUN ap/vendobal.p (ROWID(vend)).
end. /* for each vend */

session:set-wait-state("").  

message trim(FRAME {&FRAME-NAME}:title) + " Process Is Completed." view-as alert-box.

apply "close" to this-procedure.

/* end ---------------------------------- copr. 2002  Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

