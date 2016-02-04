&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  browsers/<table>.w

  Description: from BROWSER.W - Basic SmartBrowser Object Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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

&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/var.i NEW SHARED}
ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR li-help-job AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target,Navigation-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Browser-Table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES pc-misc

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table pc-misc.ml pc-misc.misc-date ~
pc-misc.job-no pc-misc.job-no2 pc-misc.frm pc-misc.blank-no pc-misc.i-no ~
pc-misc.m-code pc-misc.dscr pc-misc.cost-type pc-misc.cost pc-misc.cost-m 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table pc-misc.ml ~
pc-misc.misc-date pc-misc.job-no pc-misc.job-no2 pc-misc.frm ~
pc-misc.blank-no pc-misc.m-code pc-misc.cost pc-misc.cost-m 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table pc-misc
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table pc-misc
&Scoped-define QUERY-STRING-Browser-Table FOR EACH pc-misc WHERE ~{&KEY-PHRASE} ~
      AND pc-misc.company = g_company NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH pc-misc WHERE ~{&KEY-PHRASE} ~
      AND pc-misc.company = g_company NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table pc-misc
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table pc-misc


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     LABEL "Auto Find" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 67 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      pc-misc
    FIELDS(pc-misc.ml
      pc-misc.misc-date
      pc-misc.job-no
      pc-misc.job-no2
      pc-misc.frm
      pc-misc.blank-no
      pc-misc.i-no
      pc-misc.m-code
      pc-misc.dscr
      pc-misc.cost-type
      pc-misc.cost
      pc-misc.cost-m) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      pc-misc.ml FORMAT "M/L":U
      pc-misc.misc-date COLUMN-LABEL "Date" FORMAT "99/99/9999":U
            WIDTH 14
      pc-misc.job-no COLUMN-LABEL "Job #" FORMAT "x(6)":U
      pc-misc.job-no2 COLUMN-LABEL "" FORMAT ">9":U
      pc-misc.frm FORMAT ">>9":U WIDTH 6.2
      pc-misc.blank-no FORMAT ">9":U WIDTH 7.2
      pc-misc.i-no COLUMN-LABEL "Item #" FORMAT "x(10)":U
      pc-misc.m-code COLUMN-LABEL "Prep Code" FORMAT "x(15)":U
            WIDTH 16
      pc-misc.dscr FORMAT "x(30)":U
      pc-misc.cost-type COLUMN-LABEL "Cost!Type" FORMAT "XXX":U
      pc-misc.cost FORMAT "->>>,>>9.99":U WIDTH 16
      pc-misc.cost-m FORMAT "->,>>9.9999":U
  ENABLE
      pc-misc.ml
      pc-misc.misc-date HELP "Enter Date"
      pc-misc.job-no
      pc-misc.job-no2
      pc-misc.frm
      pc-misc.blank-no
      pc-misc.m-code
      pc-misc.cost
      pc-misc.cost-m
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 15.71
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 17.19 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 17.19 COL 82 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 17.19 COL 131 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 17.19 COL 1
     RECT-4 AT ROW 16.95 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 17.38
         WIDTH              = 145.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{methods/template/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Browser-Table TEXT-1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.pc-misc"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED"
     _Where[1]         = "ASI.pc-misc.company = g_company"
     _FldNameList[1]   > ASI.pc-misc.ml
"pc-misc.ml" ? ? "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.pc-misc.misc-date
"pc-misc.misc-date" "Date" ? "date" ? ? ? ? ? ? yes "Enter Date" no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.pc-misc.job-no
"pc-misc.job-no" "Job #" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.pc-misc.job-no2
"pc-misc.job-no2" "" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.pc-misc.frm
"pc-misc.frm" ? ? "integer" ? ? ? ? ? ? yes ? no no "6.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.pc-misc.blank-no
"pc-misc.blank-no" ? ? "integer" ? ? ? ? ? ? yes ? no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.pc-misc.i-no
"pc-misc.i-no" "Item #" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.pc-misc.m-code
"pc-misc.m-code" "Prep Code" "x(15)" "character" ? ? ? ? ? ? yes ? no no "16" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   = ASI.pc-misc.dscr
     _FldNameList[10]   > ASI.pc-misc.cost-type
"pc-misc.cost-type" "Cost!Type" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.pc-misc.cost
"pc-misc.cost" ? "->>>,>>9.99" "decimal" ? ? ? ? ? ? yes ? no no "16" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.pc-misc.cost-m
"pc-misc.cost-m" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE Browser-Table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME Browser-Table
&Scoped-define SELF-NAME Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON HELP OF Browser-Table IN FRAME F-Main
DO:
    DEF VAR CHAR-val AS cha NO-UNDO.
    DEF VAR rec-val AS RECID NO-UNDO.
    DEF VAR li-frm AS INT NO-UNDO.
    DEF VAR ll-ml AS LOG NO-UNDO.

    CASE FOCUS:NAME :
        WHEN "job-no" THEN DO:
             RUN windows/l-jobno.w (g_company,FOCUS:SCREEN-VALUE IN BROWSE {&browse-name},OUTPUT char-val, OUTPUT rec-val).
             IF rec-val <> ? THEN DO:
                FIND job-hdr WHERE RECID(job-hdr) = rec-val NO-LOCK NO-ERROR.
                IF AVAIL job-hdr THEN 
                   ASSIGN pc-misc.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = job-hdr.job-no
                          pc-misc.job-no2:SCREEN-VALUE IN BROWSE {&browse-name} = string(job-hdr.job-no2)
                          pc-misc.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = job-hdr.i-no
                          li-help-job = job-hdr.job.
                            

             END.
        END.
        WHEN "m-code" THEN DO:
            IF li-help-job = 0 THEN DO:
               FIND FIRST job WHERE job.company = g_company
                                AND job.job-no = pc-misc.job-no:SCREEN-VALUE
                                AND job.job-no2 = INT(pc-misc.job-no2:SCREEN-VALUE)
                                NO-LOCK NO-ERROR.
               IF AVAIL job THEN li-help-job = job.job.
            END.
            ASSIGN li-frm = int(pc-misc.frm:SCREEN-VALUE) 
                   ll-ml = IF pc-misc.ml:SCREEN-VALUE = "M" THEN YES ELSE NO.

            RUN windows/l-jobprp.w (g_company,li-help-job,li-frm,ll-ml,FOCUS:SCREEN-VALUE IN BROWSE {&browse-name},OUTPUT char-val).
            IF char-val <> "" THEN DO:
               ASSIGN FOCUS:SCREEN-VALUE = entry(1,char-val)
                      pc-misc.blank-no:SCREEN-VALUE = ENTRY(2,char-val).
               FIND FIRST prep WHERE prep.company = g_company
                                 AND prep.CODE = pc-misc.m-code:SCREEN-VALUE NO-LOCK NO-ERROR.
               IF AVAIL prep THEN ASSIGN pc-misc.dscr:SCREEN-VALUE = prep.dscr
                                         pc-misc.cost-type:SCREEN-VALUE = prep.cost-type.
               ELSE DO:
                    FIND FIRST job WHERE job.company = g_company AND
                                         job.job = li-help-job NO-LOCK NO-ERROR.
                    IF AVAIL job THEN DO:
                       find first ef where ef.company = g_company
                                    AND ef.est-no eq job.est-no
                                    and ef.form-no eq int(pc-misc.frm:SCREEN-VALUE)
                                    no-lock no-error.
                       if avail ef then do:
                          if substr(input pc-misc.m-code,1,3)  eq "MIS"  and
                             (substr(input pc-misc.m-code,5,1) ge "1" or
                             substr(input pc-misc.m-code,5,1) le "5")   then
                                  pc-misc.dscr:SCREEN-VALUE = ef.mis-cost[int(substr(
                                                   input pc-misc.m-code,5,1))].
      
                          if substr(input pc-misc.m-code,4,1) eq "M" then
                                pc-misc.dscr:SCREEN-VALUE = pc-misc.dscr:SCREEN-VALUE + " - Mat".      
                          ELSE if substr(input pc-misc.m-code,4,1) eq "L" then
                               pc-misc.dscr:SCREEN-VALUE = pc-misc.dscr:SCREEN-VALUE + " - Lab".
                       end.
                    END. /* AVAIL job*/
               end.  /* else */          
            END.  /* char-val */
        END.  /* m-code */
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON return OF Browser-Table IN FRAME F-Main
ANYWHERE
DO:
    APPLY "tab" TO SELF.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
 /*  {src/adm/template/brsleave.i}  */
    {brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  {methods/template/local/setvalue.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pc-misc.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pc-misc.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF pc-misc.job-no IN BROWSE Browser-Table /* Job # */
DO:
    IF LASTKEY = -1 THEN RETURN.

    RUN validate-job.
    IF RETURN-VALUE = "ERROR" THEN RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pc-misc.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pc-misc.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF pc-misc.job-no2 IN BROWSE Browser-Table
DO:
  IF LASTKEY = -1 THEN RETURN.

    RUN validate-job2.
    IF RETURN-VALUE = "ERROR" THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pc-misc.frm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pc-misc.frm Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF pc-misc.frm IN BROWSE Browser-Table /* Form */
DO:
  IF LASTKEY = -1 THEN RETURN.

    RUN validate-frm.
    IF RETURN-VALUE <> "" THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pc-misc.blank-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pc-misc.blank-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF pc-misc.blank-no IN BROWSE Browser-Table /* BLANK */
DO:
  IF LASTKEY = -1 THEN RETURN.

    RUN validate-blank.

    IF RETURN-VALUE = "ERROR" THEN RETURN NO-APPLY.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pc-misc.m-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pc-misc.m-code Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF pc-misc.m-code IN BROWSE Browser-Table /* Prep Code */
DO:
    IF LASTKEY = -1 THEN RETURN.

    RUN validate-prep.
    IF RETURN-VALUE = "ERROR" THEN RETURN NO-APPLY.
    /*
    IF SELF:MODIFIED THEN DO:
       FIND FIRST prep WHERE prep.company = g_company
                         AND prep.CODE = pc-misc.m-code:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
       IF AVAIL prep THEN ASSIGN pc-misc.dscr:SCREEN-VALUE = prep.dscr
                                 pc-misc.cost-type:SCREEN-VALUE = prep.cost-type.
       ELSE DO:
           IF li-help-job = 0 THEN DO:
               FIND FIRST job WHERE job.company = g_company
                                AND job.job-no = pc-misc.job-no:SCREEN-VALUE
                                AND job.job-no2 = INT(pc-misc.job-no2:SCREEN-VALUE)
                                NO-LOCK NO-ERROR.
               IF AVAIL job THEN li-help-job = job.job.
           END.
           IF NOT AVAIL job THEN 
                FIND FIRST job WHERE job.company = g_company AND
                                job.job = li-help-job NO-LOCK NO-ERROR.
           IF AVAIL job THEN DO:
                       find first ef where ef.company = g_company
                                    AND ef.est-no eq job.est-no
                                    and ef.form-no eq int(pc-misc.frm:SCREEN-VALUE)
                                    no-lock no-error.
                       if avail ef then do:
                          if substr(input pc-misc.m-code,1,3)  eq "MIS"  and
                             (substr(input pc-misc.m-code,5,1) ge "1" or
                             substr(input pc-misc.m-code,5,1) le "5")   then
                                  pc-misc.dscr:SCREEN-VALUE = ef.mis-cost[int(substr(
                                                   input pc-misc.m-code,5,1))].
      
                          if substr(input pc-misc.m-code,4,1) eq "M" then
                                pc-misc.dscr:SCREEN-VALUE = pc-misc.dscr:SCREEN-VALUE + " - Mat".      
                          ELSE if substr(input pc-misc.m-code,4,1) eq "L" then
                               pc-misc.dscr:SCREEN-VALUE = pc-misc.dscr:SCREEN-VALUE + " - Lab".
                       end.
                    END. /* AVAIL job*/
       end.  /* else */          
    END.
    
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement B-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN pc-misc.i-no = pc-misc.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
         pc-misc.dscr = pc-misc.dscr:SCREEN-VALUE
         pc-misc.cost-type = pc-misc.cost-type:SCREEN-VALUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-misc FOR pc-misc.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  
  ASSIGN pc-misc.company = g_company
         pc-misc.misc-date = TODAY
         pc-misc.opn = YES.

  FIND LAST bf-misc WHERE bf-misc.company = g_company 
                      AND bf-misc.misc-date = pc-misc.misc-date            
       USE-INDEX date-idx NO-LOCK NO-ERROR.
  
  
  IF AVAIL bf-misc THEN pc-misc.ml = bf-misc.ml .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY "entry" TO pc-misc.ml IN BROWSE {&browse-name} .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN validate-job .    
  IF RETURN-VALUE = "ERROR" THEN RETURN.
  
  RUN validate-job2.    
  IF RETURN-VALUE = "ERROR" THEN RETURN.

  RUN validate-frm.    
  IF RETURN-VALUE = "ERROR" THEN RETURN.

  RUN validate-blank.
  IF RETURN-VALUE = "ERROR" THEN RETURN.

  RUN validate-prep.
  IF RETURN-VALUE = "ERROR" THEN RETURN.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "pc-misc"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-blank B-table-Win 
PROCEDURE validate-blank :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-tmp-job AS cha NO-UNDO.
   DEF VAR v-ml AS LOG NO-UNDO.

   v-tmp-job = FILL(" ",6 - LENGTH(TRIM(pc-misc.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
               TRIM(pc-misc.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

   v-ml = IF pc-misc.ml:SCREEN-VALUE = "M" THEN YES ELSE NO.

   FIND FIRST job WHERE job.company = g_company AND
                        job.job-no = v-tmp-job AND
                        job.job-no2 = int(pc-misc.job-no2:SCREEN-VALUE )
                      USE-INDEX job-no NO-LOCK NO-ERROR.
   IF AVAIL job THEN DO:
      FIND FIRST job-prep WHERE job-prep.company = g_company
                            AND job-prep.job = job.job
                            AND job-prep.ml =  v-ml
                            AND job-prep.frm = INT(pc-misc.frm:SCREEN-VALUE )
                            AND job-prep.blank-no = INT(pc-misc.blank-no:SCREEN-VALUE)
                            NO-LOCK NO-ERROR.
      IF NOT AVAIL job-prep THEN DO:
         MESSAGE "Invalid Blank." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO pc-misc.blank-no.
         RETURN "ERROR".
      END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-frm B-table-Win 
PROCEDURE validate-frm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-tmp-job AS cha NO-UNDO.
   DEF VAR v-ml AS LOG NO-UNDO.

   v-tmp-job = FILL(" ",6 - LENGTH(TRIM(pc-misc.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
               TRIM(pc-misc.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

   v-ml = IF pc-misc.ml:SCREEN-VALUE = "M" THEN YES ELSE NO.

   FIND FIRST job WHERE job.company = g_company AND
                        job.job-no = v-tmp-job AND
                        job.job-no2 = int(pc-misc.job-no2:SCREEN-VALUE )
                      USE-INDEX job-no NO-LOCK NO-ERROR.
   IF AVAIL job THEN DO:
      FIND FIRST job-prep WHERE job-prep.company = g_company
                            AND job-prep.job = job.job
                            AND job-prep.ml =  v-ml
                            AND job-prep.frm = INT(pc-misc.frm:SCREEN-VALUE )
                            NO-LOCK NO-ERROR.
      IF NOT AVAIL job-prep THEN DO:
         MESSAGE "Invalid Form." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO pc-misc.frm.
         RETURN "ERROR".
      END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-job B-table-Win 
PROCEDURE validate-job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-tmp-job AS cha NO-UNDO.

   ASSIGN
    v-tmp-job = FILL(" ",6 - LENGTH(TRIM(pc-misc.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
                TRIM(pc-misc.job-no:SCREEN-VALUE IN BROWSE {&browse-name})
    pc-misc.job-no:SCREEN-VALUE IN BROWSE {&browse-name} = v-tmp-job.

   FIND FIRST job WHERE job.company = g_company AND
                        job.job-no = v-tmp-job
                      USE-INDEX job-no NO-LOCK NO-ERROR.
   IF NOT AVAIL job THEN DO:
      MESSAGE "Invalid Job. Try help. " VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO pc-misc.job-no.
      RETURN "ERROR".
   END.

   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-job2 B-table-Win 
PROCEDURE validate-job2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-tmp-job AS cha NO-UNDO.
   DEF VAR v-good AS LOG NO-UNDO.

   v-tmp-job = FILL(" ",6 - LENGTH(TRIM(pc-misc.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
               TRIM(pc-misc.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

   FIND FIRST job WHERE job.company = g_company AND
                        job.job-no = v-tmp-job AND
                        job.job-no2 = int(pc-misc.job-no2:SCREEN-VALUE )
                      USE-INDEX job-no NO-LOCK NO-ERROR.
   IF NOT AVAIL job THEN DO:
      MESSAGE "Invalid Job. Try help. " VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO pc-misc.job-no2.
      RETURN "ERROR".
   END.

   RUN jc/chk-stat.p (RECID(job), 2, YES, OUTPUT v-good).

   IF NOT v-good THEN DO:
      MESSAGE "Invalid Job. Try help." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO pc-misc.job-no.
      RETURN "ERROR".

   END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-prep B-table-Win 
PROCEDURE validate-prep :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-tmp-job AS cha NO-UNDO.
   DEF VAR v-ml AS LOG NO-UNDO.

   v-tmp-job = FILL(" ",6 - LENGTH(TRIM(pc-misc.job-no:SCREEN-VALUE IN BROWSE {&browse-name}))) +
               TRIM(pc-misc.job-no:SCREEN-VALUE IN BROWSE {&browse-name}).

   v-ml = IF pc-misc.ml:SCREEN-VALUE = "M" THEN YES ELSE NO.

   FIND FIRST job WHERE job.company = g_company AND
                        job.job-no = v-tmp-job AND
                        job.job-no2 = int(pc-misc.job-no2:SCREEN-VALUE )
                      USE-INDEX job-no NO-LOCK NO-ERROR.
   IF AVAIL job THEN DO:
      FIND FIRST job-prep WHERE job-prep.company = g_company
                            AND job-prep.job = job.job                          
                            AND job-prep.frm = INT(pc-misc.frm:SCREEN-VALUE )
                            AND job-prep.blank-no = INT(pc-misc.blank-no:SCREEN-VALUE)
                            AND job-prep.CODE = pc-misc.m-code:SCREEN-VALUE
                            AND job-prep.ml =  v-ml
                            NO-LOCK NO-ERROR.
      IF NOT AVAIL job-prep OR pc-misc.m-code:SCREEN-VALUE = "" THEN DO:
         MESSAGE "Invalid Prep Code." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO pc-misc.m-code.
         RETURN "ERROR".
      END.


       FIND FIRST prep WHERE prep.company = g_company
                         AND prep.CODE = pc-misc.m-code:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
       IF AVAIL prep THEN ASSIGN pc-misc.dscr:SCREEN-VALUE = prep.dscr
                                 pc-misc.cost-type:SCREEN-VALUE = prep.cost-type.
       ELSE DO:
           IF li-help-job = 0 THEN DO:
               FIND FIRST job WHERE job.company = g_company
                                AND job.job-no = pc-misc.job-no:SCREEN-VALUE
                                AND job.job-no2 = INT(pc-misc.job-no2:SCREEN-VALUE)
                                NO-LOCK NO-ERROR.
               IF AVAIL job THEN li-help-job = job.job.
           END.
           IF NOT AVAIL job THEN 
                FIND FIRST job WHERE job.company = g_company AND
                                job.job = li-help-job NO-LOCK NO-ERROR.
           IF AVAIL job THEN DO:
                       find first ef where ef.company = g_company
                                    AND ef.est-no eq job.est-no
                                    and ef.form-no eq int(pc-misc.frm:SCREEN-VALUE)
                                    no-lock no-error.
                       if avail ef then do:
                          if substr(input pc-misc.m-code,1,3)  eq "MIS"  and
                             (substr(input pc-misc.m-code,5,1) ge "1" or
                             substr(input pc-misc.m-code,5,1) le "5")   then
                                  pc-misc.dscr:SCREEN-VALUE = ef.mis-cost[int(substr(
                                                   input pc-misc.m-code,5,1))].
      
                          if substr(input pc-misc.m-code,4,1) eq "M" then
                                pc-misc.dscr:SCREEN-VALUE = pc-misc.dscr:SCREEN-VALUE + " - Mat".      
                          ELSE if substr(input pc-misc.m-code,4,1) eq "L" then
                               pc-misc.dscr:SCREEN-VALUE = pc-misc.dscr:SCREEN-VALUE + " - Lab".
                       end.
                    END. /* AVAIL job*/
       end.  /* else */     

   END. /* avail job*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

