&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/var.i NEW SHARED}
DEF BUFFER brm-rdtlh FOR rm-rdtlh.
DEF TEMP-TABLE ttRm-rdtlh LIKE rm-rdtlh.

ASSIGN
 cocode = g_company .

DEFINE TEMP-TABLE ttJobNo LIKE job.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS eInstructions fiEndDate rsCompany bReview ~
bCreate bExit slReceiptList 
&Scoped-Define DISPLAYED-OBJECTS eInstructions fiEndDate rsCompany ~
fiNeedTxns fiColumnHeader slReceiptList 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bCreate 
     LABEL "Create Txns" 
     SIZE 18 BY 1.14.

DEFINE BUTTON bExit AUTO-END-KEY 
     LABEL "Exit" 
     SIZE 15 BY 1.14.

DEFINE BUTTON bReview 
     LABEL "Review and Display" 
     SIZE 25 BY 1.14.

DEFINE VARIABLE eInstructions AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 72 BY 4.05 NO-UNDO.

DEFINE VARIABLE fiColumnHeader AS CHARACTER FORMAT "X(256)":U INITIAL "Company   Job       Tag               Rcpt Date" 
     VIEW-AS FILL-IN 
     SIZE 71 BY .81
     FONT 3 NO-UNDO.

DEFINE VARIABLE fiEndDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Review Receipts Through Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fiNeedTxns AS CHARACTER FORMAT "X(256)":U INITIAL "The following receipts need matching issues" 
     VIEW-AS FILL-IN 
     SIZE 71 BY .81 NO-UNDO.

DEFINE VARIABLE rsCompany AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "This Company", "This",
"All Companies", "All"
     SIZE 34 BY .95 NO-UNDO.

DEFINE VARIABLE slReceiptList AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 72 BY 13.1
     FONT 3 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     eInstructions AT ROW 1.24 COL 4 NO-LABEL WIDGET-ID 16
     fiEndDate AT ROW 5.76 COL 33 COLON-ALIGNED WIDGET-ID 2
     rsCompany AT ROW 6.95 COL 35 NO-LABEL WIDGET-ID 4
     bReview AT ROW 8.14 COL 5 WIDGET-ID 10
     bCreate AT ROW 8.14 COL 36 WIDGET-ID 12
     bExit AT ROW 8.14 COL 61 WIDGET-ID 14
     fiNeedTxns AT ROW 9.57 COL 3 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     fiColumnHeader AT ROW 10.29 COL 3 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     slReceiptList AT ROW 11.24 COL 5 NO-LABEL WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 78.2 BY 25.43
         CANCEL-BUTTON bExit WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Create Missing Issue Transactions"
         HEIGHT             = 25.43
         WIDTH              = 78.2
         MAX-HEIGHT         = 26.57
         MAX-WIDTH          = 137
         VIRTUAL-HEIGHT     = 26.57
         VIRTUAL-WIDTH      = 137
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = NO
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       eInstructions:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiColumnHeader IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiNeedTxns IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Create Missing Issue Transactions */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Create Missing Issue Transactions */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bReview
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bReview C-Win
ON CHOOSE OF bReview IN FRAME DEFAULT-FRAME /* Review and Display */
OR CHOOSE OF bCreate
DO:
    DEF VAR iCtr AS INT NO-UNDO.
    
    CASE SELF:NAME:
        WHEN "bReview" THEN DO:
            ASSIGN 
                slReceiptList:LIST-ITEMS = "".

            FOR EACH rm-rdtlh NO-LOCK WHERE 
                rm-rdtlh.company EQ (IF rsCompany:SCREEN-VALUE = "All" THEN rm-rdtlh.company ELSE cocode) AND 
                rm-rdtlh.job-no NE "" AND
                rm-rdtlh.trans-date LT DATE(fiEndDate:SCREEN-VALUE) AND 
                rm-rdtlh.rita-code EQ "R":
       
                FIND FIRST brm-rdtlh NO-LOCK WHERE 
                    brm-rdtlh.company EQ rm-rdtlh.company AND
                    brm-rdtlh.job-no EQ rm-rdtlh.job-no AND
                    brm-rdtlh.job-no2 EQ rm-rdtlh.job-no2 AND
                    brm-rdtlh.tag EQ rm-rdtlh.tag AND 
                    brm-rdtlh.trans-date EQ rm-rdtlh.trans-date AND
                    brm-rdtlh.i-no EQ rm-rdtlh.i-no AND 
                    brm-rdtlh.rita-code EQ "I"
                    NO-ERROR.
                IF NOT AVAIL brm-rdtlh THEN 
                DO:
                    CREATE ttRm-rdtlh.
                    BUFFER-COPY rm-rdtlh TO ttRm-rdtlh.
                    slReceiptList:ADD-LAST(
                        STRING(rm-rdtlh.company,"x(4)") + "   " +
                        STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', rm-rdtlh.job-no, rm-rdtlh.job-no2)) + "   " +
                        STRING(rm-rdtlh.tag,"x(16)") + "   " +
                        STRING(rm-rdtlh.trans-date,"99/99/99")
                        ).
                END.
            END.
            FOR EACH ttRm-rdtlh:
                iCtr = iCtr + 1.
            END.
            IF iCtr EQ 0 THEN MESSAGE 
                "No missing Issues detected."
                VIEW-AS ALERT-BOX.
        END.
        WHEN "bCreate" THEN DO:
            FOR EACH ttRm-rdtlh:
                FIND brm-rdtlh NO-LOCK WHERE 
                    brm-rdtlh.rec_key EQ ttRm-rdtlh.rec_key
                    NO-ERROR.
                FIND FIRST item NO-LOCK WHERE 
                    item.company EQ ttrm-rdtlh.company AND 
                    item.i-no EQ ttrm-rdtlh.i-no
                    NO-ERROR.
                
                CREATE rm-rctd.
                ASSIGN
                    rm-rctd.r-no            = DYNAMIC-NEXT-VALUE("rm_rcpt_seq", "ASI")
                    rm-rctd.company         = ttrm-rdtlh.company
                    rm-rctd.loc             = ttrm-rdtlh.loc
                    rm-rctd.loc-bin         = ttrm-rdtlh.loc-bin
                    rm-rctd.tag             = ttrm-rdtlh.tag
                    rm-rctd.qty             = ttrm-rdtlh.qty
                    rm-rctd.cost            = ttrm-rdtlh.cost
                    rm-rctd.loc2            = ttrm-rdtlh.loc2
                    rm-rctd.loc-bin2        = ttrm-rdtlh.loc-bin2
                    rm-rctd.tag2            = ttrm-rdtlh.tag2
                    rm-rctd.rita-code       = "I"
                    rm-rctd.s-num           = ttrm-rdtlh.s-num
                    rm-rctd.b-num           = ttrm-rdtlh.b-num
                    rm-rctd.job-no          = ttrm-rdtlh.job-no
                    rm-rctd.job-no2         = ttrm-rdtlh.job-no2
                    rm-rctd.receiver-no     = ttrm-rdtlh.receiver-no
                    rm-rctd.i-no            = ttrm-rdtlh.i-no
                    rm-rctd.i-name          = IF AVAIL item THEN item.i-name ELSE ""
                    rm-rctd.pur-uom         = IF AVAIL item THEN item.pur-uom ELSE ""
                    rm-rctd.rec_key         = STRING(YEAR(TODAY),"9999") + 
                                          STRING(MONTH(TODAY),"99") + 
                                          STRING(DAY(TODAY),"99") + 
                                          STRING(TIME,"99999") + 
                                          STRING(NEXT-VALUE(rec_key_seq,ASI),"99999999")
                    rm-rctd.rct-date        = ttrm-rdtlh.upd-date
                    rm-rctd.post-date       = ?
                    rm-rctd.upd-date        = TODAY
                    rm-rctd.upd-time        = TIME
                    rm-rctd.BOL             = ttrm-rdtlh.BOL
                    rm-rctd.frt-cost        = ttrm-rdtlh.frt-cost
                    rm-rctd.user-id         = ttrm-rdtlh.user-id
                    rm-rctd.reject-code[ 1] = ttrm-rdtlh.reject-code[ 1]
                    rm-rctd.reject-code[ 2] = ttrm-rdtlh.reject-code[ 2]
                    rm-rctd.reject-code[ 3] = ttrm-rdtlh.reject-code[ 3]
                    rm-rctd.reject-code[ 4] = ttrm-rdtlh.reject-code[ 4]
                    rm-rctd.reject-code[ 5] = ttrm-rdtlh.reject-code[ 5]
                    rm-rctd.reject-code[ 6] = ttrm-rdtlh.reject-code[ 6]
                    rm-rctd.reject-code[ 7] = ttrm-rdtlh.reject-code[ 7]
                    rm-rctd.reject-code[ 8] = ttrm-rdtlh.reject-code[ 8]
                    rm-rctd.reject-code[ 9] = ttrm-rdtlh.reject-code[ 9]
                    rm-rctd.reject-code[10] = ttrm-rdtlh.reject-code[10]
                    rm-rctd.trans-time      = ttrm-rdtlh.trans-time
                    rm-rctd.trans-timex     = ttrm-rdtlh.trans-timex
                    rm-rctd.spare-char-1    = ttrm-rdtlh.spare-char-1
                    rm-rctd.spare-char-2    = ttrm-rdtlh.spare-char-2
                    rm-rctd.spare-char-3    = ttrm-rdtlh.spare-char-3
                    rm-rctd.spare-char-4    = ttrm-rdtlh.spare-char-4
                    rm-rctd.spare-char-5    = ttrm-rdtlh.spare-char-5
                    rm-rctd.spare-dec-1     = ttrm-rdtlh.spare-dec-1
                    rm-rctd.spare-dec-2     = ttrm-rdtlh.spare-dec-2
                    rm-rctd.spare-dec-3     = ttrm-rdtlh.spare-dec-3
                    rm-rctd.spare-dec-4     = ttrm-rdtlh.spare-dec-4
                    rm-rctd.spare-dec-5     = ttrm-rdtlh.spare-dec-5
                    rm-rctd.spare-int-1     = ttrm-rdtlh.spare-int-1
                    rm-rctd.spare-int-2     = ttrm-rdtlh.spare-int-2
                    rm-rctd.spare-int-3     = ttrm-rdtlh.spare-int-3
                    rm-rctd.spare-int-4     = ttrm-rdtlh.spare-int-4
                    rm-rctd.spare-int-5     = ttrm-rdtlh.spare-int-5
                    rm-rctd.enteredBy       = ttrm-rdtlh.enteredBy
                    rm-rctd.enteredDT       = ttrm-rdtlh.enteredDT
                    rm-rctd.adjustmentCode  = ""
                    .
            END.
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  
  ASSIGN 
    eInstructions:SCREEN-VALUE =
        'This function will scan your database for any job-associated RM Receipt records where there are no corresponding Issue transactions, ' +
        'and will (optionally) allow you to create the records needed to issue these items using MR2.'
    fiEndDate:SCREEN-VALUE = STRING(TODAY)
    .
  
    
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY eInstructions fiEndDate rsCompany fiNeedTxns fiColumnHeader 
          slReceiptList 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE eInstructions fiEndDate rsCompany bReview bCreate bExit slReceiptList 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

