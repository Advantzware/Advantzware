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

/* Local Variable Definitions ---    
                                   */

{custom/globdefs.i}


{methods/defines/hndldefs.i}
/*{methods/prgsecur.i}*/
DEFINE BUFFER b-prgrms FOR prgrms.
DEFINE VARIABLE period_pos AS INTEGER NO-UNDO.
DEFINE VARIABLE v-prgmname LIKE b-prgrms.prgmname NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
v-prgmname = USERID("NOSWEAT") + "..".
ELSE
ASSIGN
  period_pos = INDEX(PROGRAM-NAME(1),".")
  v-prgmname = SUBSTR(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"/",period_pos - 9) + 1)
  v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i NEW SHARED}


ASSIGN
 cocode = g_company
 locode = g_loc.

 def var cExcelFile as cha no-undo.
 def var lFileOK as log no-undo.

 &GLOBAL-DEFINE LOG-FILE shipto-import-log.txt

 DEFINE VAR gcTempDir AS CHAR INIT "C:\tmp\" NO-UNDO.
 DEFINE VAR gcLogFile AS CHAR INIT "C:\tmp\{&LOG-FILE}" NO-UNDO.
 DEF VAR iRowCount AS INT INIT 2  NO-UNDO.
 DEF VAR chExcelApplication AS COM-HANDLE NO-UNDO.
 DEF VAR v-rowcnt AS INT INIT 1 NO-UNDO.
 DEF VAR chWorkBook   AS COM-HANDLE  NO-UNDO.
 DEF VAR chWorksheet  AS COM-HANDLE  NO-UNDO.
 DEF TEMP-TABLE tt-shipto 
    FIELD spare-char-1 AS CHAR
    FIELD ship-zip AS CHAR 
    FIELD dest-code AS CHAR
    FIELD valid    AS LOGICAL INIT TRUE
    FIELD row-no   AS INTEGER
    FIELD refcode  AS CHAR
    FIELD cout     AS INT.

 DEFINE STREAM log-file.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_cust-no end_cust-no ~
begin_ship-to end_ship-to enter_loc begin_carr-no lv-file def_zone ~
btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no begin_ship-to ~
end_ship-to enter_loc begin_carr-no lv-file def_zone 

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


DEFINE VARIABLE begin_carr-no AS CHARACTER FORMAT "X(5)":U 
     LABEL "New Ship to Carrier" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "From Customer" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ship-to AS CHARACTER FORMAT "X(8)":U 
     LABEL "From Shipto" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE def_zone AS CHARACTER FORMAT "X(5)":U 
     LABEL "Default Delivery Zone" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "To Customer" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_ship-to AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "To Shipto" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE enter_loc AS CHARACTER FORMAT "X(5)":U 
     LABEL "Update Carrier Whse" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE lv-file AS CHARACTER FORMAT "X(150)":U 
     LABEL "Excel File" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 8.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust-no AT ROW 6.1 COL 23 COLON-ALIGNED HELP
          "Enter the Beginning Customer Number" WIDGET-ID 12
     end_cust-no AT ROW 6.1 COL 64 COLON-ALIGNED HELP
          "Enter the Ending Customer Number" WIDGET-ID 14
     begin_ship-to AT ROW 7.29 COL 23 COLON-ALIGNED HELP
          "Enter the Beginning Shipto Number"
     end_ship-to AT ROW 7.29 COL 64 COLON-ALIGNED HELP
          "Enter the Ending Shipto Number"
     enter_loc AT ROW 8.48 COL 64 COLON-ALIGNED
     begin_carr-no AT ROW 9.67 COL 64 COLON-ALIGNED
     def_zone AT ROW 10.86 COL 64 COLON-ALIGNED WIDGET-ID 8
     lv-file AT ROW 12.05 COL 22.8 COLON-ALIGNED WIDGET-ID 10
     btn-process AT ROW 13.62 COL 22
     btn-cancel AT ROW 13.62 COL 53
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 13.95.

DEFINE FRAME FRAME-B
     "" VIEW-AS TEXT
          SIZE 6.2 BY .95 AT ROW 2.91 COL 82.8
          BGCOLOR 11 
     "" VIEW-AS TEXT
          SIZE 88.8 BY .95 AT ROW 1 COL 1
          BGCOLOR 11 
     "" VIEW-AS TEXT
          SIZE 3 BY .95 AT ROW 1.95 COL 1
          BGCOLOR 11 
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 76 BY .95 AT ROW 2.91 COL 8
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.95 COL 4
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "" VIEW-AS TEXT
          SIZE 7 BY .95 AT ROW 2.91 COL 1
          BGCOLOR 11 
     "" VIEW-AS TEXT
          SIZE 88.8 BY .95 AT ROW 3.76 COL 1
          BGCOLOR 11 
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
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Update Shipto Zones"
         HEIGHT             = 14.19
         WIDTH              = 90.2
         MAX-HEIGHT         = 19.76
         MAX-WIDTH          = 98.2
         VIRTUAL-HEIGHT     = 19.76
         VIRTUAL-WIDTH      = 98.2
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
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

ASSIGN 
       begin_carr-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".


/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Fix FG History Trans Date */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Fix FG History Trans Date */
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME begin_carr-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_carr-no C-Win
ON LEAVE OF begin_carr-no IN FRAME FRAME-A /* Carrier */
DO:
  IF LASTKEY NE -1 THEN DO:
      RUN valid-carrier NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME enter_loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL enter_loc C-Win
ON LEAVE OF enter_loc IN FRAME FRAME-A /* location */
DO:
  IF LASTKEY NE -1 THEN DO:
      RUN valid-loc NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME lv-file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-file C-Win
ON LEAVE OF lv-file IN FRAME FRAME-A /* location */
DO:
  IF LASTKEY NE -1 THEN DO:
      RUN valid-file-path NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO: 
  DEF VAR ll AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  ASSIGN
      cExcelFile = lv-file .

    RUN valid-carrier NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    RUN valid-loc NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    RUN valid-file-path NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  FOR EACH tt-shipto:
    DELETE tt-shipto.
   END.

  IF lFileOK THEN DO:
    IF LENGTH(cExcelFile) LT 4 OR
        (SUBSTR(cExcelFile,LENGTH(cExcelFile) - 3) NE ".xls" AND
         SUBSTR(cExcelFile,LENGTH(cExcelFile) - 4) NE ".xlsx" AND
         SUBSTR(cExcelFile,LENGTH(cExcelFile) - 3) NE ".csv") THEN DO:
        MESSAGE "Invalid File.  Must Choose Excel or CSV File."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        LEAVE.
    END.   
  END. /*lFileOK*/

    SESSION:SET-WAIT-STATE ("general").

    /* Initialize Excel. */
    CREATE "Excel.Application" chExcelApplication NO-ERROR.

    /* Check if Excel got initialized. */
    IF NOT (VALID-HANDLE (chExcelApplication)) THEN DO:
        MESSAGE "Unable to Start Excel." VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY. 
    END.

    /* Open our Excel File. */  
    chExcelApplication:VISIBLE = FALSE.
    chWorkbook = chExcelApplication:Workbooks:OPEN(cExcelFile) NO-ERROR.

    /* Do not display Excel error messages. */
    chExcelApplication:DisplayAlerts = FALSE NO-ERROR.

    /* Go to the Active Sheet. */
    chWorkbook:WorkSheets(1):Activate NO-ERROR.

    ASSIGN
        chWorkSheet = chExcelApplication:Sheets:ITEM(1).
    REPEAT:
        IF chWorkSheet:Range("A" + STRING(iRowCount)):VALUE = ? THEN LEAVE.

        FIND FIRST tt-shipto WHERE tt-shipto.cout EQ iRowCount NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-shipto THEN
        CREATE tt-shipto.
        tt-shipto.spare-char-1       = chWorkSheet:Range("A" + STRING(iRowCount)):VALUE NO-ERROR.
        tt-shipto.ship-zip           = chWorkSheet:Range("B" + STRING(iRowCount)):VALUE NO-ERROR.
        tt-shipto.dest-code          = chWorkSheet:Range("C" + STRING(iRowCount)):VALUE NO-ERROR.
        tt-shipto.cout               = v-rowcnt .

        v-rowcnt = v-rowcnt + 1 .
        ASSIGN
            tt-shipto.row-no = iRowCount
            iRowCount = iRowCount + 1.
    END. /*REPEAT*/


  /*Free memory*/
  chWorkbook = chExcelApplication:Workbooks:CLOSE() NO-ERROR.
  RELEASE OBJECT chWorkbook NO-ERROR.
  RELEASE OBJECT chWorkSheet NO-ERROR.
  RELEASE OBJECT chExcelApplication NO-ERROR. 
DEF VAR vcat AS INT NO-UNDO.
FOR EACH tt-shipto NO-LOCK:
    IF INDEX(tt-shipto.ship-zip,".") > 0 THEN DO:
    vcat =  INDEX(tt-shipto.ship-zip,".") .
    tt-shipto.ship-zip = SUBSTRING(tt-shipto.ship-zip,1,vcat - 1 ) .
    END.
END.

 FOR EACH shipto WHERE shipto.company = cocode
                  AND shipto.cust-no GE begin_cust-no
                  AND shipto.cust-no LE end_cust-no
                  AND shipto.ship-id GE begin_ship-to 
                  AND shipto.ship-id LE end_ship-to
                  AND shipto.carrier EQ begin_carr-no 
                  AND shipto.loc EQ enter_loc EXCLUSIVE-LOCK:
     FIND FIRST tt-shipto WHERE tt-shipto.ship-zip EQ shipto.ship-zip  NO-LOCK NO-ERROR.
     IF AVAIL tt-shipto THEN
         ASSIGN shipto.dest-code = tt-shipto.dest-code .
     ELSE
         ASSIGN shipto.dest-code =  def_zone .
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE("").
    MESSAGE " Process Complete..." VIEW-AS ALERT-BOX.
    apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-file C-Win
ON HELP OF lv-file IN FRAME FRAME-A
DO:
   /*RUN InitializeLogFile. */
    gcTempDir = lv-file .

   SYSTEM-DIALOG GET-FILE cExcelFile
            TITLE "Select File to Import"
            FILTERS "Excel File (*.csv,*.xls,*.xlsx) " "*.csv,*.xls,*.xlsx"
            INITIAL-DIR gcTempDir
            MUST-EXIST
            USE-FILENAME
            UPDATE lFileOK.

  IF lFileOK THEN self:screen-value = cExcelFile .


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FRAME-A
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A C-Win
ON HELP OF FRAME FRAME-A
DO:
    DEF VAR char-val AS cha NO-UNDO.
    DEF VAR lv-handle AS HANDLE NO-UNDO.


    CASE FOCUS:NAME :
         when "begin_ship-to" then do:
            run windows/l-cust.w  (cocode,focus:screen-value, output char-val). 
            if char-val <> "" then DO:
               focus:screen-value in frame {&frame-name} = entry(1,char-val).
            END.
          end.
          when "end_ship-to" then do:
            run windows/l-cust.w  (cocode,focus:screen-value, output char-val). 
            if char-val <> "" then DO:
               focus:screen-value in frame {&frame-name} = entry(1,char-val).
            END.
          end.
          when "enter_loc" then do:
            run windows/l-loc.w  (cocode,focus:screen-value, output char-val). 
            if char-val <> "" then 
              focus:screen-value in frame {&frame-name} = entry(1,char-val).
          end.

          when "begin_carr-no" then do:
            run windows/l-carrie.w  (cocode, enter_loc:SCREEN-VALUE IN FRAME {&FRAME-NAME}, focus:screen-value, output char-val). 
            if char-val <> "" then 
              focus:screen-value in frame {&frame-name} = entry(1,char-val).
          end. 
          when "def_zone" then do:
            run windows/l-delzon.w  (cocode, enter_loc:SCREEN-VALUE IN FRAME {&FRAME-NAME}, begin_carr-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}, focus:screen-value, output char-val). 
            if char-val <> "" then 
              focus:screen-value in frame {&frame-name} = entry(1,char-val).
          end.
          otherwise do:
           lv-handle = focus:handle.
           run applhelp.p.

           if g_lookup-var <> "" then do:
              lv-handle:screen-value = g_lookup-var.

           end.   /* g_lookup-var <> "" */
           apply "entry" to lv-handle.
           return no-apply.
          end.  /* otherwise */

    END CASE.
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

 /* ASSIGN
   begin_date = TODAY - 1
   end_date   = TODAY.*/

  RUN enable_UI.

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
        APPLY "entry" TO begin_ship-to.
  END.

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
  DISPLAY begin_cust-no end_cust-no begin_ship-to end_ship-to enter_loc 
          begin_carr-no lv-file def_zone 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 begin_cust-no end_cust-no begin_ship-to end_ship-to enter_loc 
         begin_carr-no lv-file def_zone btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-carrier V-table-Win 
PROCEDURE valid-carrier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:

      IF begin_carr-no:SCREEN-VALUE EQ "" THEN do:
        MESSAGE "Truck/Carrier may not be blank, Try help..." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO begin_carr-no.
         RETURN ERROR.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc V-table-Win 
PROCEDURE valid-loc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF enter_loc:SCREEN-VALUE EQ "" THEN do:
        MESSAGE "Carrier Location may not be blank, Try help..." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO enter_loc.
         RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-file-path V-table-Win 
PROCEDURE valid-file-path :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF lv-file:SCREEN-VALUE EQ "" THEN do:
        MESSAGE "Please enter excel file path..." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO lv-file.
         RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeLogFile Procedure 
PROCEDURE InitializeLogFile :
/*------------------------------------------------------------------------------
  Purpose: InitializeLogFile    
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
FIND FIRST users 
    WHERE users.user_id EQ USERID("nosweat")  
    NO-LOCK NO-ERROR.

IF AVAIL users AND users.user_program[2] NE "" THEN
     ASSIGN 
        gcTempDir = users.user_program[2]
        gcLogFile = gcTempDir +  "\{&LOG-FILE}" .

IF SEARCH(gcLogFile) <> ? THEN
    OS-DELETE VALUE(gcLogFile).

OUTPUT STREAM log-file TO VALUE(gcLogFile).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
