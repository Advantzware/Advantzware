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
{sys/inc/var.i "new shared"}
{src/adm2/widgetprto.i}
DEF STREAM sOutfile.
ASSIGN
    cocode = g_company
    locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-8 tbHaveFile RECT-9 fiFileName ~
BOLNoBegin BOLNoEnd custNoBegin custNoEnd userIDBegin userIDEnd ~
updateDateBegin updateDateEnd tbVerifyBlanks tbInvNoZero tbInvMustExist ~
tbSimulation tbShowWarnings bProcess bExit fiOutputFile 
&Scoped-Define DISPLAYED-OBJECTS tbHaveFile fiFileName eInstructions ~
BOLNoBegin BOLNoEnd custNoBegin custNoEnd userIDBegin userIDEnd ~
updateDateBegin updateDateEnd tbVerifyBlanks tbInvNoZero tbInvMustExist ~
tbSimulation tbShowWarnings fiBegin fiEnd fiOptions fiOutputFile 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bExit AUTO-END-KEY 
     LABEL "EXIT" 
     SIZE 15 BY 1.91
     FONT 6.

DEFINE BUTTON bProcess 
     LABEL "PROCESS" 
     SIZE 15 BY 1.91
     FONT 6.

DEFINE VARIABLE eInstructions AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE
     SIZE 159 BY 7.14 NO-UNDO.

DEFINE VARIABLE BOLNoBegin AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "BOL Number" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 TOOLTIP "Enter beginning BOL number" NO-UNDO.

DEFINE VARIABLE BOLNoEnd AS INTEGER FORMAT ">>>>>>9":U INITIAL 9999999 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 TOOLTIP "Enter beginning BOL number" NO-UNDO.

DEFINE VARIABLE custNoBegin AS CHARACTER FORMAT "X(8)":U 
     LABEL "Customer No." 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE custNoEnd AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fiBegin AS CHARACTER FORMAT "X(256)":U INITIAL "BEGIN" 
      VIEW-AS TEXT 
     SIZE 8 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiEnd AS CHARACTER FORMAT "X(256)":U INITIAL "END" 
      VIEW-AS TEXT 
     SIZE 6 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiFileName AS CHARACTER FORMAT "X(256)":U 
     LABEL "File Name (F1 to search)" 
     VIEW-AS FILL-IN 
     SIZE 71 BY 1 NO-UNDO.

DEFINE VARIABLE fiOptions AS CHARACTER FORMAT "X(256)":U INITIAL "OPTIONS" 
      VIEW-AS TEXT 
     SIZE 11 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiOutputFile AS CHARACTER FORMAT "X(256)":U 
     LABEL "Results will be listed in this file (double-click to open)" 
      VIEW-AS TEXT 
     SIZE 71 BY .62 NO-UNDO.

DEFINE VARIABLE updateDateBegin AS DATE FORMAT "99/99/9999":U INITIAL 01/01/20 
     LABEL "Update Date" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE updateDateEnd AS DATE FORMAT "99/99/9999":U INITIAL 12/31/2099 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE userIDBegin AS CHARACTER FORMAT "X(256)":U 
     LABEL "User ID" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE userIDEnd AS CHARACTER FORMAT "X(256)":U INITIAL "zzzzzzzzzzzz" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 63 BY 8.33.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 51 BY 8.81.

DEFINE VARIABLE tbHaveFile AS LOGICAL INITIAL no 
     LABEL "I have a file with BOL and/or Invoice numbers" 
     VIEW-AS TOGGLE-BOX
     SIZE 48 BY 1 NO-UNDO.

DEFINE VARIABLE tbInvMustExist AS LOGICAL INITIAL yes 
     LABEL "Invoice MUST exist" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE tbInvNoZero AS LOGICAL INITIAL yes 
     LABEL "Invoice number MUST be zero" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE tbShowWarnings AS LOGICAL INITIAL no 
     LABEL "Show Warnings in report" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE tbSimulation AS LOGICAL INITIAL yes 
     LABEL "Simulation mode ONLY" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE tbVerifyBlanks AS LOGICAL INITIAL yes 
     LABEL "At least one blank tag exists in BOL lines" 
     VIEW-AS TOGGLE-BOX
     SIZE 45 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     tbHaveFile AT ROW 9.57 COL 4
     fiFileName AT ROW 9.57 COL 80 COLON-ALIGNED
     eInstructions AT ROW 1.95 COL 4 NO-LABEL NO-TAB-STOP 
     BOLNoBegin AT ROW 12.91 COL 17 COLON-ALIGNED
     BOLNoEnd AT ROW 12.91 COL 40 COLON-ALIGNED NO-LABEL
     custNoBegin AT ROW 14.33 COL 17 COLON-ALIGNED
     custNoEnd AT ROW 14.33 COL 40 COLON-ALIGNED NO-LABEL
     userIDBegin AT ROW 15.76 COL 17 COLON-ALIGNED
     userIDEnd AT ROW 15.76 COL 40 COLON-ALIGNED NO-LABEL
     updateDateBegin AT ROW 17.19 COL 17 COLON-ALIGNED
     updateDateEnd AT ROW 17.19 COL 40 COLON-ALIGNED NO-LABEL
     tbVerifyBlanks AT ROW 12.91 COL 75
     tbInvNoZero AT ROW 14.33 COL 75
     tbInvMustExist AT ROW 15.76 COL 75
     tbSimulation AT ROW 17.19 COL 75
     tbShowWarnings AT ROW 18.62 COL 75
     bProcess AT ROW 12.19 COL 136
     bExit AT ROW 15.29 COL 136
     fiBegin AT ROW 11.95 COL 22 COLON-ALIGNED NO-LABEL
     fiEnd AT ROW 11.95 COL 45 COLON-ALIGNED NO-LABEL
     fiOptions AT ROW 11.95 COL 84 COLON-ALIGNED NO-LABEL
     fiOutputFile AT ROW 20.52 COL 56 COLON-ALIGNED
     RECT-8 AT ROW 11.24 COL 4
     RECT-9 AT ROW 11.24 COL 70
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 165.2 BY 20.76.


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
         TITLE              = "Delete OE Invoices/Unpost BOLs"
         HEIGHT             = 20.76
         WIDTH              = 165.2
         MAX-HEIGHT         = 25.19
         MAX-WIDTH          = 177
         VIRTUAL-HEIGHT     = 25.19
         VIRTUAL-WIDTH      = 177
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR EDITOR eInstructions IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       eInstructions:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiBegin IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiBegin:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiEnd IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiEnd:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiOptions IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiOptions:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiOutputFile:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Delete OE Invoices/Unpost BOLs */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Delete OE Invoices/Unpost BOLs */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BOLNoEnd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BOLNoEnd C-Win
ON LEAVE OF BOLNoEnd IN FRAME DEFAULT-FRAME
OR LEAVE OF userIdEnd
OR LEAVE OF updateDateEnd
DO:
    CASE SELF:NAME:
        WHEN "bolNoEnd" THEN DO:
            IF INTEGER(SELF:SCREEN-VALUE) LT INTEGER(bolNoBegin:SCREEN-VALUE) THEN DO:
                MESSAGE 
                    "Ending BOL number must be greater than beginning BOL number."
                    VIEW-AS ALERT-BOX ERROR.
                APPLY 'entry' TO SELF.
                RETURN NO-APPLY.
            END. 
        END.  
        WHEN "userIdEnd" THEN DO:
            IF SELF:SCREEN-VALUE LT userIdBegin:SCREEN-VALUE THEN DO:
                MESSAGE 
                    "Ending User ID must be greater than beginning User ID."
                    VIEW-AS ALERT-BOX ERROR.
                APPLY 'entry' TO SELF.
                RETURN NO-APPLY.
            END. 
        END.  
        WHEN "updateDateEnd" THEN DO:
            IF DATE(SELF:SCREEN-VALUE) LT DATE(updateDateBegin:SCREEN-VALUE) THEN DO:
                MESSAGE 
                    "Ending date must be greater than beginning date."
                    VIEW-AS ALERT-BOX ERROR.
                APPLY 'entry' TO SELF.
                RETURN NO-APPLY.
            END. 
        END.  
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bProcess
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bProcess C-Win
ON CHOOSE OF bProcess IN FRAME DEFAULT-FRAME /* PROCESS */
DO:
    ASSIGN 
        CURRENT-WINDOW = c-Win
        fiOutputFile:SCREEN-VALUE = 
            SESSION:TEMP-DIRECTORY + "UNpostBOLs-" + 
            STRING(YEAR(TODAY),"9999") +
            STRING(MONTH(TODAY),"99") +
            STRING(DAY(TODAY),"99") +
            "-" + STRING(TIME,"99999" + ".csv").
    
    STATUS DEFAULT "Scanning BOLs...".
    STATUS INPUT "Scanning BOLs...".

    RUN pWriteLog ("Initialize", "").
    
    IF tbHaveFile:CHECKED THEN 
        RUN pProcessFromFile.
    ELSE 
        RUN pProcessFromParms.  
    
    STATUS DEFAULT "Processing complete".
    STATUS INPUT "Processing complete".
    
    RUN pWriteLog ("Close", "").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFileName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFileName C-Win
ON HELP OF fiFileName IN FRAME DEFAULT-FRAME /* File Name (F1 to search) */
DO: 
    DEF VAR lOKPressed AS LOG NO-UNDO.
    DEF VAR cFileName AS CHAR NO-UNDO.
    DEF VAR cSelectedDir AS CHAR NO-UNDO.
    
    SYSTEM-DIALOG GET-FILE cFileName  
        TITLE   "Select a file for import"
        FILTERS "Test files (*.txt)"    "*.txt",
        "CSV files (*.csv)"     "*.csv"
        MUST-EXIST 
        USE-FILENAME 
        UPDATE lOKpressed.   

    IF lOKPressed THEN ASSIGN 
        SELF:SCREEN-VALUE = cFileName.
                
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiOutputFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiOutputFile C-Win
ON MOUSE-SELECT-DBLCLICK OF fiOutputFile IN FRAME DEFAULT-FRAME /* Results will be listed in this file (double-click to open) */
DO:
    OS-COMMAND SILENT VALUE("START " + SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbHaveFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbHaveFile C-Win
ON VALUE-CHANGED OF tbHaveFile IN FRAME DEFAULT-FRAME /* I have a file with BOL and/or Invoice numbers */
DO:
    ASSIGN 
        fiFileName:VISIBLE = SELF:CHECKED
        fiFileName:SENSITIVE = SELF:CHECKED
        BolNoBegin:SENSITIVE = NOT SELF:CHECKED 
        BolNoEnd:SENSITIVE = NOT SELF:CHECKED 
        userIdBegin:SENSITIVE = NOT SELF:CHECKED 
        userIDEnd:SENSITIVE = NOT SELF:CHECKED 
        updateDateBegin:SENSITIVE = NOT SELF:CHECKED 
        updateDateEnd:SENSITIVE = NOT SELF:CHECKED 
        .
    IF SELF:CHECKED THEN 
        APPLY 'entry' TO fiFileName.
    ELSE 
        APPLY 'entry' TO BolNoBegin.
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
  
  ASSIGN 
    eInstructions =
        "Use this function to UNPOST Bills of Lading from invoices, and delete the invoice header/line records." + CHR(10) + CHR(10) +
        "You may select BOLs using ranges of BOL numbers, user IDs, and/or update dates.  If you have a file " +
        "containing a list of BOL numbers, you can optionally use that file as a source for this process.  (Note " +
        "that the file must contain a 'header row' with the header 'BOL#' to be processed.)  Source files may be " +
        "either .txt or .csv file types." + CHR(10) + CHR(10) +
        "It is recommended to run this process in 'Simulation Mode' first to generate a report of BOLs/Invoices that " +
        "are candidates for deletion.  This report will be created in the directory shown at the bottom of this " +
        "screen.  When the Simulation Mode option is turned OFF, the program will UNPOST the selected BOLs and " +
        "DELETE the associated Invoice records."
    fiOutputFile = SESSION:TEMP-DIRECTORY + "UNpostBOLs-" + 
                    STRING(YEAR(TODAY),"9999") +
                    STRING(MONTH(TODAY),"99") +
                    STRING(DAY(TODAY),"99") +
                    "-" + STRING(TIME,"99999" + ".csv").
    RUN enable_UI.
    
    DISPLAY 
        eInstructions
        fiOutputFile
        WITH FRAME default-frame.
        
    APPLY 'value-changed' TO tbHaveFile.

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
  DISPLAY tbHaveFile fiFileName eInstructions BOLNoBegin BOLNoEnd custNoBegin 
          custNoEnd userIDBegin userIDEnd updateDateBegin updateDateEnd 
          tbVerifyBlanks tbInvNoZero tbInvMustExist tbSimulation tbShowWarnings 
          fiBegin fiEnd fiOptions fiOutputFile 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-8 tbHaveFile RECT-9 fiFileName BOLNoBegin BOLNoEnd custNoBegin 
         custNoEnd userIDBegin userIDEnd updateDateBegin updateDateEnd 
         tbVerifyBlanks tbInvNoZero tbInvMustExist tbSimulation tbShowWarnings 
         bProcess bExit fiOutputFile 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProcessFromFile C-Win 
PROCEDURE pProcessFromFile :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF VAR cHeaderList AS CHAR NO-UNDO.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR iTableCol AS INT NO-UNDO.
    DEF VAR cRawLine AS CHAR NO-UNDO.
    DEF VAR iBolNo AS INT NO-UNDO. 
        
    IF SEARCH(fiFileName:SCREEN-VALUE IN FRAME default-frame) EQ ? THEN DO:
        MESSAGE 
            "Unable to locate file: " + fiFileName:SCREEN-VALUE + " Please select another file."
            VIEW-AS ALERT-BOX ERROR.
        APPLY 'entry' TO fiFileName.
        RETURN.
    END.
    
    INPUT FROM VALUE(fiFileName:SCREEN-VALUE).
    IMPORT UNFORMATTED cHeaderList.
    DO iCtr = 1 TO NUM-ENTRIES(cHeaderList):
        IF ENTRY(iCtr,cHeaderList) = "BOL#" THEN ASSIGN 
            iTableCol = iCtr.
    END.
    REPEAT:
        IMPORT UNFORMATTED cRawLine.
        ASSIGN 
            iBolNo = INTEGER(ENTRY(iTableCol,cRawLine)).
        FIND FIRST oe-bolh NO-LOCK WHERE 
            oe-bolh.company EQ cocode AND 
            oe-bolh.bol-no EQ iBolNo
            NO-ERROR.
        IF AVAIL oe-bolh THEN 
            RUN pProcessOneBol (ROWID(oe-bolh)).
        ELSE 
            RUN pWriteLog ("Error", STRING(iBolNo) + "," + "Unable to locate BOL#").
    END.
    INPUT CLOSE.
            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProcessFromParms C-Win 
PROCEDURE pProcessFromParms :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    FOR EACH oe-bolh NO-LOCK WHERE 
        oe-bolh.bol-no GE INTEGER(bolNoBegin:SCREEN-VALUE IN FRAME default-frame) AND 
        oe-bolh.bol-no LE INTEGER(bolNoEnd:SCREEN-VALUE) AND 
        oe-bolh.cust-no GE custNoBegin:SCREEN-VALUE AND 
        oe-bolh.cust-no LE custNoEnd:SCREEN-VALUE AND 
        oe-bolh.user-id GE userIdBegin:SCREEN-VALUE AND 
        oe-bolh.user-id LE userIdEnd:SCREEN-VALUE AND 
        oe-bolh.upd-date GE DATE(updateDateBegin:SCREEN-VALUE) AND 
        oe-bolh.upd-date LE DATE(updateDateEnd:SCREEN-VALUE)
        :
        RUN pProcessOneBol (ROWID(oe-bolh)).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProcessOneBol C-Win 
PROCEDURE pProcessOneBol :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER roe-bolh AS ROWID.
    DEF BUFFER boe-bolh FOR oe-bolh.
    DEF BUFFER boe-boll FOR oe-boll.
    DEF BUFFER binv-head FOR inv-head.
    
    FIND FIRST boe-bolh NO-LOCK WHERE 
        ROWID(boe-bolh) EQ roe-bolh
        NO-ERROR.
    IF NOT AVAIL boe-bolh THEN DO:
        RUN pWriteLog ("Error", "BOL header was deleted while processing").
        RETURN.
    END.
    ELSE DO:
        STATUS DEFAULT "Processing BOL# " + STRING(boe-bolh.bol-no).
        STATUS INPUT "Processing BOL# " + STRING(boe-bolh.bol-no).

        IF tbVerifyBlanks:CHECKED IN FRAME default-frame THEN DO:
            FIND FIRST boe-boll NO-LOCK WHERE 
                boe-boll.company EQ boe-bolh.company AND 
                boe-boll.b-no    EQ boe-bolh.b-no AND 
                boe-boll.tag     EQ ""
                NO-ERROR.
            IF NOT AVAIL boe-boll THEN DO:
                RUN pWriteLog ("Warning", STRING(boe-bolh.bol-no) + ",BOL# skipped; no blank tags").
                RETURN .
            END.
        END.
        IF tbInvNoZero:CHECKED THEN DO:
            IF boe-bolh.inv-no NE 0 THEN DO:
                RUN pWriteLog ("Warning", STRING(boe-bolh.bol-no) + ",BOL# skipped; Invoice Number not zero").
                RETURN .
            END.
        END.
        
        FIND FIRST binv-head EXCLUSIVE WHERE
            binv-head.company EQ boe-bolh.company AND 
            binv-head.bol-no EQ boe-bolh.bol-no
            NO-ERROR. 
        IF NOT AVAIL binv-head THEN DO:
            RUN pWriteLog ("Warning", STRING(boe-bolh.bol-no) + ",No invoice record for BOL#").
            IF NOT tbSimulation:CHECKED IN FRAME default-frame THEN DO:
                ASSIGN 
                    boe-bolh.posted = NO.
                FOR EACH boe-boll EXCLUSIVE WHERE 
                    boe-boll.company EQ boe-bolh.company AND 
                    boe-boll.b-no    EQ boe-bolh.b-no:
                    ASSIGN 
                        boe-boll.posted = NO.
                END.
            END.
            RUN pWriteLog ("Unposted", STRING(boe-bolh.bol-no) + ",Unposted BOL#").
        END.
        ELSE IF tbInvMustExist:CHECKED THEN DO:
            RUN pWriteLog ("Deleted", STRING(ROWID(boe-bolh))).
            IF NOT tbSimulation:CHECKED THEN 
                DELETE binv-head.
        END. 
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWriteLog C-Win 
PROCEDURE pWriteLog :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcAction AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcValue AS CHAR NO-UNDO.
    DEF BUFFER coe-bolh FOR oe-bolh.
    DEF BUFFER coe-boll FOR oe-boll.
    
    CASE ipcAction:
        WHEN "Initialize" THEN DO:
            OUTPUT STREAM sOutFile TO VALUE(fiOutputFile:SCREEN-VALUE IN FRAME default-frame).
        END.
        WHEN "Error" THEN DO:
            PUT STREAM sOutFile UNFORMATTED 
                ipcAction + "," + ipcValue + CHR(10).
        END.
        WHEN "Warning" THEN DO:
            IF tbShowWarnings:CHECKED THEN PUT STREAM sOutFile UNFORMATTED 
                ipcAction + "," + ipcValue + CHR(10).
        END.
        WHEN "Unposted" THEN DO:
            PUT STREAM sOutFile UNFORMATTED 
                (IF tbSimulation:CHECKED THEN SUBSTRING(ipcAction,1,length(ipcAction) - 2) ELSE ipcAction) + "," + ipcValue + CHR(10).
        END.
        WHEN "Deleted" THEN DO:
            FIND coe-bolh NO-LOCK WHERE 
                ROWID(coe-bolh) EQ TO-ROWID(ipcValue)
                NO-ERROR.
            FIND FIRST coe-boll OF coe-bolh NO-LOCK NO-ERROR.
            IF AVAIL coe-bolh THEN PUT STREAM sOutFile UNFORMATTED 
            (IF tbSimulation:CHECKED THEN SUBSTRING(ipcAction,1,length(ipcAction) - 1) ELSE ipcAction) + "," +
                STRING(coe-bolh.bol-no) + "," +
                "Invoice# " + STRING(coe-bolh.inv-no) + "," +
                "Order# " + (IF AVAIL coe-boll THEN STRING(coe-boll.ord-no) ELSE STRING(coe-bolh.ord-no)) + "," +
                "BOL Date=" + STRING(coe-bolh.bol-date) + "," +
                "Cust# " STRING(coe-bolh.cust-no) + CHR(10).
        END.
        WHEN "Close" THEN DO:
            OUTPUT STREAM sOutFile CLOSE.
        END.
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

