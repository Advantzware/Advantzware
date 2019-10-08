&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util/autoclosejobs.w

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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

DEFINE STREAM log-out.
DEFINE STREAM account-bak.

DEFINE BUFFER gbf-company FOR company.  /*global company buffer*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi_company fi_level fi_find fi_replace ~
btn-process btn-cancel RECT-17 
&Scoped-Define DISPLAYED-OBJECTS fi_company fi_level fi_find fi_replace ~
EDITOR-1 fi_account-format fi_digits 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE EDITOR-1 AS CHARACTER INITIAL "This utility will search all GL Account Numbers in the system for the specified company and specified account section and replace what is specified in the Find box with what is in the Replace With box." 
     VIEW-AS EDITOR
     SIZE 79 BY 2.38 NO-UNDO.

DEFINE VARIABLE fi_account-format AS CHARACTER FORMAT "X(30)":U 
     LABEL "Company Account Format" 
      VIEW-AS TEXT 
     SIZE 40 BY .62 NO-UNDO.

DEFINE VARIABLE fi_company AS CHARACTER FORMAT "X(6)":U 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_digits AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Digits" 
      VIEW-AS TEXT 
     SIZE 8 BY .62 TOOLTIP "Enter Account Number Level to Change" NO-UNDO.

DEFINE VARIABLE fi_find AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Find" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE fi_level AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Level to Change" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 TOOLTIP "Enter Account Number Level to Change" NO-UNDO.

DEFINE VARIABLE fi_replace AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Replace With" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 9.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fi_company AT ROW 8.14 COL 30 COLON-ALIGNED HELP
          "Enter string that you want to replace" WIDGET-ID 10
     fi_level AT ROW 10.57 COL 30 COLON-ALIGNED HELP
          "Enter string that you want to replace" WIDGET-ID 12
     fi_find AT ROW 11.71 COL 30 COLON-ALIGNED HELP
          "Enter string that you want to replace"
     fi_replace AT ROW 12.91 COL 30 COLON-ALIGNED HELP
          "Enter string that you want to replace" WIDGET-ID 16
     btn-process AT ROW 14.81 COL 23
     btn-cancel AT ROW 14.81 COL 53
     EDITOR-1 AT ROW 5.29 COL 5 NO-LABEL WIDGET-ID 6
     fi_account-format AT ROW 9.48 COL 30 COLON-ALIGNED HELP
          "Enter string that you want to replace" WIDGET-ID 18
     fi_digits AT ROW 10.67 COL 46.6 COLON-ALIGNED HELP
          "Enter string that you want to replace" WIDGET-ID 20
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90.2 BY 15.76.

DEFINE FRAME FRAME-B
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 76 BY .95 AT ROW 2.91 COL 8
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.95 COL 4
          BGCOLOR 11 FGCOLOR 12 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.2 BY 3.81
         BGCOLOR 11 .


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
         TITLE              = "Find and Replace GL Account Number"
         HEIGHT             = 16
         WIDTH              = 90.2
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-B:FRAME = FRAME FRAME-A:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME Custom                                                    */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


/* SETTINGS FOR EDITOR EDITOR-1 IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_account-format IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       fi_account-format:READ-ONLY IN FRAME FRAME-A        = TRUE.

/* SETTINGS FOR FILL-IN fi_digits IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       fi_digits:READ-ONLY IN FRAME FRAME-A        = TRUE.

/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Find and Replace GL Account Number */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Find and Replace GL Account Number */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
    apply "close" to this-procedure.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
    DEFINE VARIABLE lProcess AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER   NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN {&displayed-objects}.
        RUN ValidateCompany(OUTPUT lProcess).
        IF lProcess THEN RUN ValidateLevel(OUTPUT lProcess).
        IF lProcess THEN RUN ValidateFind(OUTPUT lProcess).
        IF lProcess THEN RUN ValidateReplace(OUTPUT lProcess).
        IF lProcess THEN DO:            
            cMessage = "Are you sure you want to find '" + fi_find + "' and ".
            cMessage = cMessage + " replace it with '" + fi_replace + "' for ".
            cMessage = cMessage + " level number " + STRING(fi_level).
            cMessage = cMessage + " of all accounts for company " + fi_company.
            cMessage = cMessage + "?".
            MESSAGE cMessage
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE lProcess.
        END.
    END.
    IF lProcess THEN RUN RunProcess.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_company
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_company C-Win
ON LEAVE OF fi_company IN FRAME FRAME-A /* Company */
DO:
  DEFINE VARIABLE lValid AS LOGICAL     NO-UNDO.
  IF LASTKEY NE -1 THEN DO:
      ASSIGN {&displayed-objects}.
      RUN ValidateCompany(OUTPUT lValid).
      IF NOT lValid THEN DO:
          APPLY "entry" TO fi_company.
          RETURN NO-APPLY.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_find
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_find C-Win
ON LEAVE OF fi_find IN FRAME FRAME-A /* Find */
DO:
  DEFINE VARIABLE lValid AS LOGICAL     NO-UNDO.
  IF LASTKEY NE -1 THEN DO:
      ASSIGN {&displayed-objects}.
      RUN ValidateFind(OUTPUT lValid).
      IF NOT lValid THEN DO:
          APPLY "entry" TO fi_find.
          RETURN NO-APPLY.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_level
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_level C-Win
ON LEAVE OF fi_level IN FRAME FRAME-A /* Level to Change */
DO:
    DEFINE VARIABLE lValid AS LOGICAL     NO-UNDO.

    IF LASTKEY NE -1 THEN DO:
        ASSIGN {&displayed-objects}.
        RUN ValidateLevel(OUTPUT lValid).
        IF NOT lValid THEN DO:
            APPLY "entry" TO fi_company.
            RETURN NO-APPLY.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_replace
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_replace C-Win
ON LEAVE OF fi_replace IN FRAME FRAME-A /* Replace With */
DO:
    DEFINE VARIABLE lValid AS LOGICAL     NO-UNDO.
    IF LASTKEY NE -1 THEN DO:
        ASSIGN {&displayed-objects}.
        RUN ValidateReplace(OUTPUT lValid).
        IF NOT lValid THEN DO:
          APPLY "entry" TO fi_company.
          RETURN NO-APPLY.
      END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  DO WITH FRAME {&FRAME-NAME}:
    APPLY "entry" TO fi_company.
  END.
  WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckHistory C-Win 
PROCEDURE CheckHistory :
/*------------------------------------------------------------------------------
  Purpose:  Checks History for existence of account number that might be affected   
  Parameters:  accepts company and account and level returns OK or not
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER ipiLevel AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAccount AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplOK AS LOGICAL NO-UNDO.

    DEFINE VARIABLE iStart AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iLength AS INTEGER     NO-UNDO.

    DEFINE BUFFER bf-glhist FOR glhist.
    DEFINE BUFFER bf-gltrans FOR gltrans.


    IF NOT AVAIL gbf-company THEN 
        RUN ValidateCompany(OUTPUT oplOK).
    ELSE 
        oplOK = YES.
    IF oplOK THEN DO:
        RUN GetSubstringArgs(INPUT ipiLevel, OUTPUT iStart, OUTPUT iLength).
        FIND FIRST bf-gltrans 
            WHERE bf-gltrans.company EQ gbf-company.company
              AND SUBSTRING(bf-gltrans.actnum, iStart, iLength) EQ ipcAccount
            NO-LOCK NO-ERROR.
        IF NOT AVAIL bf-gltrans THEN
            FIND FIRST bf-glhist 
                WHERE bf-glhist.company EQ gbf-company.company
                  AND SUBSTRING(bf-glhist.actnum, iStart, iLength) EQ ipcAccount
                NO-LOCK NO-ERROR.
        oplOK = NOT AVAIL bf-gltrans AND NOT AVAIL bf-glhist.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY fi_company fi_level fi_find fi_replace EDITOR-1 fi_account-format 
          fi_digits 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE fi_company fi_level fi_find fi_replace btn-process btn-cancel RECT-17 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSubstringArgs C-Win 
PROCEDURE GetSubstringArgs :
/*------------------------------------------------------------------------------
  Purpose:    Get the arguments needed to get substring of account for given level 
  Parameters:  level, output start and length.
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiLevel AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiStart AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiLength AS INTEGER NO-UNDO.

    DEFINE VARIABLE lOK AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE iLevCount AS INTEGER NO-UNDO.

    IF NOT AVAIL gbf-company  THEN
        RUN ValidateCompany(OUTPUT lOK).
    ELSE
        lOK = YES.
    IF lOK THEN DO:
        opiLength = gbf-company.acc-dig[ipiLevel].
        opiStart = 1.
        IF ipiLevel GT 1 THEN DO:
            opiStart = opiStart + ipiLevel - 1 . /*add count for dashes*/
            DO iLevCount = 2 TO ipiLevel:
                opiStart = opiStart + gbf-company.acc-dig[iLevCount - 1].
            END.
        END.

    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RunProcess C-Win 
PROCEDURE RunProcess :
DEFINE VARIABLE lProcess AS LOGICAL     NO-UNDO.
DEFINE VARIABLE iStart AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLength AS INTEGER     NO-UNDO.
DEFINE VARIABLE cNewAccount AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNewAccountStart AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNewAccountEnd AS CHARACTER   NO-UNDO.
DEFINE BUFFER bf-account FOR account.

DISABLE TRIGGERS FOR LOAD OF bf-account.

SESSION:SET-WAIT-STATE("General").

OUTPUT STREAM log-out TO VALUE("C:\tmp\log-find-replace-account.txt").
OUTPUT STREAM account-bak TO VALUE("C:\tmp\account" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY)) + STRING(DAY(TODAY)) + ".d") APPEND.

RUN CheckHistory(INPUT fi_level,
                 INPUT fi_find,
                 OUTPUT lProcess).
IF lProcess THEN DO:
    RUN GetSubstringArgs(INPUT fi_level,
                         OUTPUT iStart,
                         OUTPUT iLength).
    FOR EACH bf-account
        WHERE bf-account.company EQ gbf-company.company
          AND SUBSTRING(bf-account.actnum, iStart, iLength) EQ fi_find
        EXCLUSIVE-LOCK:
        EXPORT STREAM account-bak bf-account.
        cNewAccount = bf-account.actnum.
        IF iStart EQ 1 THEN cNewAccountStart = "".
        ELSE cNewAccountStart = SUBSTRING(cNewAccount, 1 , iStart - 2) + "-".
        IF fi_level EQ gbf-company.acc-level THEN cNewAccountEnd = "".
        ELSE cNewAccountEnd = "-" + SUBSTRING(cNewAccount, 
                                        iStart + iLength + 1, 
                                        LENGTH(cNewAccount) - iStart + iLength).
        cNewAccount = cNewAccountStart + STRING(fi_replace) + cNewAccountEnd.
        PUT STREAM log-out "Old Account: " bf-account.actnum FORMAT "x(30)" " Changed to: " cNewAccount FORMAT "x(30)" SKIP. 

        bf-account.actnum = cNewAccount.

    END.
END.
ELSE 
    MESSAGE 'GL History was found that matches "Find" criteria.  Cannot process.'   
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

OUTPUT STREAM log-out CLOSE.
OUTPUT STREAM account-bak CLOSE.

STATUS DEFAULT "".

SESSION:SET-WAIT-STATE("").

MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.
APPLY "close" TO THIS-PROCEDURE.

RETURN NO-APPLY.

/* end ---------------------------------- copr. 2006  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetAccountFormat C-Win 
PROCEDURE SetAccountFormat :
/*------------------------------------------------------------------------------
  Purpose:    Display the Account format for the company 
  Parameters:  none - Uses global control variables
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE lValid AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cFormatString AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iLevel AS INTEGER     NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
    IF NOT AVAIL gbf-company THEN
        RUN ValidateCompany(OUTPUT lValid).
    IF AVAIL gbf-company THEN DO:
        DO iLevel = 1 TO gbf-company.acc-level :
            cFormatString = cFormatString + FILL("#",gbf-company.acc-dig[iLevel]).
            IF iLevel NE gbf-company.acc-level THEN
                cFormatString = cFormatString + "-".
        END.
    END.
    ASSIGN fi_account-format = cFormatString.
    DISPLAY fi_account-format.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidateCompany C-Win 
PROCEDURE ValidateCompany :
/*------------------------------------------------------------------------------
  Purpose:    Validate the company 
  Parameters:  none - Uses global control variables
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
    oplValid = YES.
    FIND FIRST gbf-company
        WHERE gbf-company.company EQ fi_company
        NO-LOCK NO-ERROR.
    IF NOT AVAIL gbf-company THEN DO:
        oplValid = NO.
        MESSAGE "Invalid Company"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    ELSE
        RUN SetAccountFormat.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidateFind C-Win 
PROCEDURE ValidateFind :
/*------------------------------------------------------------------------------
  Purpose:  Validate the Find String   
  Parameters:  output validity
  Notes:       
/* ------------------------------------------------------------------------------*/ */
DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
DEFINE VARIABLE cMsg AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iLetter AS INTEGER     NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
    oplValid = YES.
    RUN ValidateCompany(OUTPUT oplValid).
    IF oplValid THEN RUN ValidateLevel(OUTPUT oplValid).
    IF oplValid THEN DO:
        IF oplValid AND LENGTH(fi_find) NE fi_digits THEN DO:
            cMsg = "Find must be " + STRING(fi_digits) + " digits".
            oplValid = NO.
        END.
        IF NOT oplValid THEN DO:
            MESSAGE cMsg
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY "entry" TO fi_find.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidateLevel C-Win 
PROCEDURE ValidateLevel :
/*------------------------------------------------------------------------------
  Purpose:    Validate the level 
  Parameters:  none - Uses global control variables
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.

DEFINE VARIABLE cMsg AS CHARACTER   NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
    oplValid = YES.
     IF fi_level LE 0 THEN DO:
        cMsg = "Level to Change field must be greater than 0".
        oplValid = NO.
    END.
    ELSE DO:
        IF NOT AVAIL gbf-company THEN
            RUN ValidateCompany(OUTPUT oplValid).
        IF AVAIL gbf-company THEN DO:
            IF fi_level GT gbf-company.acc-level THEN DO:
                cMsg = "Level must be less than or equal to " + STRING(gbf-company.acc-level).
                oplValid = NO.
            END.
            ELSE DO:
                ASSIGN fi_digits = gbf-company.acc-dig[fi_level].
                DISPLAY fi_digits.
            END.
        END.
    END.
    IF NOT oplValid THEN DO:
        MESSAGE cMsg
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "entry" TO fi_level.
        RETURN NO-APPLY.
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidateReplace C-Win 
PROCEDURE ValidateReplace :
/*------------------------------------------------------------------------------
  Purpose:  Validate the Replace String   
  Parameters:  output validity
  Notes:       
/* ------------------------------------------------------------------------------*/ */
DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
    oplValid = YES.
    RUN ValidateFind(OUTPUT oplValid).
    IF oplValid THEN DO:
        IF LENGTH(fi_replace) NE LENGTH(fi_find) THEN DO:
            MESSAGE "Replace With # of digits must be the same as Find (" STRING(LENGTH(fi_find)) ")"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            oplValid = NO.
            APPLY "entry" TO fi_find.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

