&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*----------------------------------------------------------------------*/
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

{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEFINE TEMP-TABLE ttInvoices
    FIELD company       AS CHARACTER
    FIELD ItemNo        AS CHARACTER
    FIELD InvoiceNo     AS INTEGER
    FIELD BOLNo         AS INTEGER
    FIELD InvoiceDate   AS CHARACTER FORMAT "x(12)"
    FIELD BOLDate       AS CHARACTER FORMAT "x(12)"
    FIELD InvoiceAmount AS DECIMAL
    FIELD Source        AS CHARACTER
    .
DEFINE VARIABLE v-process    AS LOG           NO-UNDO.
DEFINE VARIABLE lRecordFound AS LOG           NO-UNDO.
DEFINE STREAM sOutput.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 RECT-18 begin_bol# end_bol# fi_file ~
out-process bol-changed change_date btnCalendar-1 btn-process btn-cancel  ~
boldate inv-number inv_date
&Scoped-Define DISPLAYED-OBJECTS bolDate begin_bol# end_bol# fi_file ~
bol-changed inv-number inv_date change_date 

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

DEFINE BUTTON btnCalendar-1 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON out-process 
     LABEL "S&tart Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE begin_bol# AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     LABEL "Beginning BOL#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE bol-changed AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     LABEL "BOL#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE bolDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Bol Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE change_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/00 
     LABEL "Change Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE end_bol# AS INTEGER FORMAT ">>>>>>>9" INITIAL 99999999 
     LABEL "Ending BOL#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\InvBOLDiffs.csv" 
     LABEL "Ouput File" 
     VIEW-AS FILL-IN 
     SIZE 58 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE inv-number AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     LABEL "Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE inv_date AS DATE FORMAT "99/99/9999":U
     LABEL "Inv Date#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 125 BY 13.33.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 125 BY 5.95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_bol# AT ROW 7.38 COL 26 COLON-ALIGNED HELP
          "Enter Beginning BOL Number" WIDGET-ID 30
     end_bol# AT ROW 7.48 COL 67 COLON-ALIGNED HELP
          "Enter Ending BOL Number" WIDGET-ID 32
     fi_file AT ROW 8.86 COL 26 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 34
     out-process AT ROW 8.81 COL 89 WIDGET-ID 38
     bol-changed AT ROW 12.24 COL 26 COLON-ALIGNED HELP
          "Enter Beginning BOL Number" WIDGET-ID 42
     inv-number AT ROW 12.29 COL 65.4 COLON-ALIGNED HELP
          "Enter Beginning BOL Number" WIDGET-ID 44
     bolDate AT ROW 13.81 COL 26.4 COLON-ALIGNED WIDGET-ID 8
     inv_date AT ROW 13.67 COL 65.6 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 46
     change_date AT ROW 15.29 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Date" WIDGET-ID 48
     btnCalendar-1 AT ROW 15.24 COL 45 WIDGET-ID 50
     btn-process AT ROW 16.29 COL 82.8 WIDGET-ID 52
     btn-cancel AT ROW 16.33 COL 105.4 WIDGET-ID 54
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.05 COL 3.8
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     "Search for date Differences:" VIEW-AS TEXT
          SIZE 41.8 BY .62 AT ROW 6.14 COL 7.4 WIDGET-ID 36
          FONT 0
     "Fix Bol Date:" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 11.24 COL 6 WIDGET-ID 40
          FONT 0
     RECT-17 AT ROW 4.81 COL 1
     RECT-18 AT ROW 4.86 COL 1 WIDGET-ID 28
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 126 BY 17.71.

DEFINE FRAME FRAME-B
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 79 BY .95 AT ROW 2.91 COL 26
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.95 COL 21
          BGCOLOR 11 FGCOLOR 12 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 3.81
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
         TITLE              = "Check and fix Invoice Bol Date Differences"
         HEIGHT             = 17.71
         WIDTH              = 126.2
         MAX-HEIGHT         = 19.76
         MAX-WIDTH          = 126.2
         VIRTUAL-HEIGHT     = 19.76
         VIRTUAL-WIDTH      = 126.2
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-B:FRAME = FRAME FRAME-A:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME Custom                                                    */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-B:MOVE-AFTER-TAB-ITEM (bolDate:HANDLE IN FRAME FRAME-A)
/* END-ASSIGN-TABS */.

ASSIGN 
       begin_bol#:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       bol-changed:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN bolDate IN FRAME FRAME-A
                                                               */
ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       change_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_bol#:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN inv-number IN FRAME FRAME-A
                                                               */
ASSIGN 
       inv-number:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN inv_date IN FRAME FRAME-A
                                                               */
ASSIGN 
       inv_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       out-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Purge FG History */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Purge FG History */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
  
&Scoped-define SELF-NAME boldate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL boldate C-Win
ON ENTRY OF boldate IN FRAME FRAME-A /* BOL# */
DO:
    APPLY "tab" TO SELF.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
&Scoped-define SELF-NAME inv-number
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-number C-Win
ON ENTRY OF inv-number IN FRAME FRAME-A /* BOL# */
DO:
    APPLY "tab" TO SELF.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME inv_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv_date C-Win
ON ENTRY OF inv_date IN FRAME FRAME-A /* BOL# */
DO:
       APPLY "tab" TO SELF.
       RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bol-changed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bol-changed C-Win
ON LEAVE OF bol-changed IN FRAME FRAME-A /* BOL# */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-bol NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bol-changed C-Win
ON VALUE-CHANGED OF bol-changed IN FRAME FRAME-A /* BOL# */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            FIND FIRST oe-bolh NO-LOCK
                WHERE oe-bolh.company EQ cocode
                AND oe-bolh.bol-no EQ int(bol-changed:SCREEN-VALUE) 
                AND oe-bolh.bol-no NE 0 NO-ERROR .
            IF AVAILABLE oe-bolh THEN 
            DO:
                FIND FIRST ar-invl NO-LOCK
                    WHERE ar-invl.company EQ oe-bolh.company 
                    AND ar-invl.b-no EQ oe-bolh.b-no NO-ERROR . 
                IF AVAILABLE ar-invl THEN 
                DO:
                    FIND FIRST ar-inv NO-LOCK
                        WHERE ar-inv.x-no EQ ar-invl.x-no
                        AND ar-inv.company EQ ar-inv.company NO-ERROR .
                    IF AVAILABLE ar-inv THEN
                        ASSIGN
                            inv_date:SCREEN-VALUE   = STRING(ar-inv.inv-date)
                            inv-number:SCREEN-VALUE = STRING(ar-invl.inv-no)
                            boldate:SCREEN-VALUE = STRING(oe-bolh.bol-date)
                            change_date:SCREEN-VALUE = STRING(oe-bolh.bol-date).
          
                END.

                IF NOT AVAILABLE inv-line THEN
                    FIND FIRST inv-line NO-LOCK
                        WHERE inv-line.company EQ oe-bolh.company
                        AND inv-line.b-no EQ oe-bolh.b-no NO-ERROR .
                IF AVAILABLE inv-line THEN 
                DO:
                    FIND FIRST inv-head NO-LOCK
                        WHERE inv-head.r-no EQ inv-line.r-no
                        AND inv-head.company EQ inv-line.company NO-ERROR .
                    IF AVAILABLE inv-head THEN
                        ASSIGN
                            inv_date:SCREEN-VALUE   = STRING(inv-head.inv-date)
                            inv-number:SCREEN-VALUE = STRING(inv-line.inv-no) 
                            boldate:SCREEN-VALUE = STRING(oe-bolh.bol-date)
                            change_date:SCREEN-VALUE = STRING(oe-bolh.bol-date) .
                END.
            END.
    
        END.
 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bolDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bolDate C-Win
ON HELP OF bolDate IN FRAME FRAME-A /* Bol Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
        v-process  = NO.

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.
        
        RUN valid-Bol NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.


        FIND FIRST oe-bolh NO-LOCK
            WHERE oe-bolh.company EQ cocode 
            AND oe-bolh.bol-no EQ int(bol-changed:SCREEN-VALUE) 
            AND oe-bolh.bol-no NE 0 NO-ERROR .
 
        IF MONTH(date(inv_date)) NE MONTH(change_date) OR 
            YEAR(date(inv_date)) NE YEAR(change_date)  THEN 
        DO:
     
            MESSAGE "Note: This could cause a problem with month end reporting " SKIP
                "if the invoice date is not the same period as the BOL.  Continue?"
                VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE v-process.
            IF NOT v-process THEN DO: 
                APPLY "entry" TO change_date .
                RETURN NO-APPLY .
            END.
        END.


        MESSAGE "Are you sure you want to change date "
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE v-process.

        IF v-process THEN RUN run-process.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 C-Win
ON CHOOSE OF btnCalendar-1 IN FRAME FRAME-A
DO:
  {methods/btnCalendar.i change_date}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME change_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL change_date C-Win
ON HELP OF change_date IN FRAME FRAME-A /* Change Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* Ouput File */
DO:
        ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME out-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL out-process C-Win
ON CHOOSE OF out-process IN FRAME FRAME-A /* Srart Process */
DO:
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN {&displayed-objects}.
    END.

    v-process  = NO.
    EMPTY TEMP-TABLE ttInvoices .

    RUN run-report.
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
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   /*IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.*/

    RUN enable_UI.
   DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
        boldate:READ-ONLY = YES 
        inv-number:READ-ONLY = YES
        inv_date:READ-ONLY = YES .
       
   END.

/*    {methods/nowait.i}*/
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
  DISPLAY bolDate begin_bol# end_bol# fi_file bol-changed inv-number inv_date 
          change_date 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 RECT-18 begin_bol# end_bol# fi_file out-process bol-changed 
         change_date btnCalendar-1 btn-process btn-cancel boldate inv-number inv_date
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pExportTempTable C-Win 
PROCEDURE pExportTempTable PRIVATE :
/*------------------------------------------------------------------------------ 
    
         Purpose: Exports the contents of any temp-table into CSV    
    
         Notes: 
    
        ------------------------------------------------------------------------------*/ 

    DEFINE INPUT PARAMETER iphTT AS HANDLE NO-UNDO. 
    DEFINE INPUT PARAMETER ipcFileName AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE hQuery  AS HANDLE    NO-UNDO.            
    DEFINE VARIABLE iIndex  AS INTEGER   NO-UNDO.            
    DEFINE VARIABLE cTTName AS CHARACTER NO-UNDO.            

    cTTName = iphTT:NAME. 

    OUTPUT STREAM sOutput to VALUE(ipcFileName). 

    CREATE QUERY hQuery. 

    hQuery:SET-BUFFERS (iphTT:DEFAULT-BUFFER-HANDLE). 
    hQuery:QUERY-PREPARE("FOR EACH " + cTTName).      
    hQuery:QUERY-OPEN(). 

  

    DO iIndex = 1 TO iphTT:DEFAULT-BUFFER-HANDLE:NUM-FIELDS: 
        PUT STREAM sOutput UNFORMATTED iphTT:DEFAULT-BUFFER-HANDLE:buffer-field(iIndex):COLUMN-LABEL + ",". 
        
    END. 

    PUT STREAM sOutput UNFORMATTED SKIP. 
    REPEAT:   

        hQuery:GET-NEXT().   
        IF hQuery:QUERY-OFF-END THEN LEAVE.   

        DO iIndex = 1 TO iphTT:DEFAULT-BUFFER-HANDLE:NUM-FIELDS: 
            PUT STREAM sOutput UNFORMATTED                       
                '"' iphTT:DEFAULT-BUFFER-HANDLE:buffer-field(iIndex):buffer-value '",'. 
        END. 

        PUT STREAM sOutput UNFORMATTED SKIP. 

    END. 

    OUTPUT STREAM sOutput CLOSE. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST oe-bolh EXCLUSIVE-LOCK
            WHERE oe-bolh.company EQ cocode
            AND oe-bolh.bol-no EQ int(bol-changed:SCREEN-VALUE) 
            AND oe-bolh.bol-no NE 0 NO-ERROR .
        IF AVAILABLE oe-bolh THEN 
        DO:
            ASSIGN 
                oe-bolh.bol-date = DATE(change_date:SCREEN-VALUE) .

            FOR EACH oe-boll EXCLUSIVE-LOCK 
                WHERE oe-boll.company EQ oe-bolh.company 
                AND oe-boll.b-no EQ oe-bolh.b-no :
                ASSIGN 
                    oe-boll.bol-date = DATE(change_date:SCREEN-VALUE) .
                FOR EACH fg-rcpth EXCLUSIVE-LOCK
                    WHERE fg-rcpth.company EQ oe-boll.company
                    AND fg-rcpth.b-no    EQ oe-boll.b-no
                    AND fg-rcpth.job-no  EQ oe-boll.job-no
                    AND fg-rcpth.job-no2 EQ oe-boll.job-no2
                    AND fg-rcpth.i-no    EQ oe-boll.i-no USE-INDEX b-no:
                    fg-rcpth.trans-date = DATE(change_date:SCREEN-VALUE) .
                END.
            END.  /* oe-boll */
        END.  /* avail oe-bolh */
        RELEASE oe-bolh .
        RELEASE fg-rcpth .
        RELEASE oe-boll .
        MESSAGE "Process complete......" VIEW-AS ALERT-BOX INFORMATION . 
    END.
     
/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
FOR EACH ar-invl NO-LOCK 
    WHERE ar-invl.company EQ cocode,
        FIRST ar-inv NO-LOCK
        WHERE ar-inv.x-no EQ ar-invl.x-no,
        FIRST oe-bolh NO-LOCK
        WHERE oe-bolh.b-no EQ ar-invl.b-no         
        AND oe-bolh.bol-no GT begin_bol#
        AND oe-bolh.bol-no LT end_bol# :
        IF MONTH(oe-bolh.bol-date) NE MONTH(ar-inv.inv-date) THEN 
        DO:

            CREATE ttInvoices.
            ASSIGN 
                ttInvoices.company       = ar-invl.company
                ttInvoices.InvoiceNo     = ar-inv.inv-no
                ttInvoices.BOLNo         = oe-bolh.bol-no
                ttInvoices.InvoiceDate   = STRING(ar-inv.inv-date,"99/99/9999")
                ttInvoices.ItemNo        = ar-invl.i-no
                ttInvoices.BOLDate       = STRING(oe-bolh.bol-date,"99/99/9999")
                ttInvoices.InvoiceAmount = ar-invl.amt
                ttInvoices.Source        = "Posted Invoices"
                .
        END.
    END.

    FOR EACH inv-line NO-LOCK
        WHERE inv-line.company EQ cocode,
        FIRST inv-head NO-LOCK
        WHERE inv-head.r-no EQ inv-line.r-no,
        FIRST oe-bolh NO-LOCK
        WHERE oe-bolh.b-no EQ inv-line.b-no        
        AND oe-bolh.bol-no GT begin_bol#
        AND oe-bolh.bol-no LT end_bol#:
        IF oe-bolh.bol-date NE inv-head.inv-date THEN 
        DO:
            CREATE ttInvoices.
            ASSIGN 
                ttInvoices.company       = inv-line.company
                ttInvoices.InvoiceNo     = inv-head.inv-no
                ttInvoices.BOLNo         = oe-bolh.bol-no
                ttInvoices.InvoiceDate   = STRING(inv-head.inv-date,"99/99/9999")
                ttInvoices.ItemNo        = inv-line.i-no
                ttInvoices.BOLDate       = STRING(oe-bolh.bol-date,"99/99/9999")
                ttInvoices.InvoiceAmount = inv-line.amt
                ttInvoices.Source        = "Un-Posted Invoices (OB1)"
                .
        END.
    END.

    RUN pExportTempTable(TEMP-TABLE ttINvoices:HANDLE, fi_file) .

    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).


/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-bol C-Win 
PROCEDURE valid-bol :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    {methods/lValidateError.i YES}
    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-FIND(FIRST oe-bolh  WHERE oe-bolh.company EQ cocode AND
            oe-bolh.bol-no EQ int(bol-changed:SCREEN-VALUE) AND oe-bolh.bol-no NE 0 )
            THEN 
        DO:
            MESSAGE "Invalid Bol# ..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO bol-changed .
            RETURN ERROR.
        END.
    END.

    {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

