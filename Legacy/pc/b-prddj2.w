&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  pc\b-prddj2.w

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
 
&SCOPED-DEFINE yellowColumnsName tt-prdd
&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF NEW SHARED VAR v-est-type LIKE est.est-type INIT 1.

DEF VAR ls-start AS cha FORM "x(5)" NO-UNDO.
DEF VAR ls-stop AS cha FORM "x(5)" NO-UNDO.
DEF VAR li-help-job LIKE job.job NO-UNDO.
DEF VAR lv-is-first-prdd AS LOG NO-UNDO.
DEF VAR lv-adding-mode AS LOG NO-UNDO.
DEF VAR ll-no-frm AS LOG NO-UNDO.
DEF VAR ll-no-blk AS LOG NO-UNDO.
DEF VAR ll-skip AS LOG NO-UNDO.
DEF VAR lv-depts LIKE mach.dept NO-UNDO.
DEF VAR lv-dept LIKE pc-prdd.dept NO-UNDO.
DEF VAR lv-schmch LIKE mach.m-code NO-UNDO.

DEF TEMP-TABLE tt-prdd NO-UNDO LIKE pc-prdd
    FIELD row-id AS ROWID
    INDEX row-id row-id.

DEF TEMP-TABLE tt-job-mch NO-UNDO LIKE job-mch
    FIELD row-id AS ROWID
    FIELD xtra-copy AS LOG.

DEF BUFFER bf-prdd FOR pc-prdd.

DEF TEMP-TABLE w-jm NO-UNDO
   FIELD frm      LIKE job-mch.frm
   FIELD d-seq    LIKE mach.d-seq
   FIELD blank-no LIKE job-mch.blank-no
   FIELD row-id   AS   ROWID.

DEF VAR lv-prev-prdd-rowid AS rowid NO-UNDO.
DEF VAR lv-initial AS LOG INIT YES NO-UNDO.
DEF TEMP-TABLE tt-prdd2 NO-UNDO LIKE tt-prdd.

DO TRANSACTION:
   {sys/inc/tspostfg.i}
   {sys/inc/fgrecpt.i}
END.

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

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES pc-prdd
&Scoped-define FIRST-EXTERNAL-TABLE pc-prdd


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR pc-prdd.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-prdd

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table tt-prdd.m-code tt-prdd.frm tt-prdd.blank-no tt-prdd.pass tt-prdd.i-no tt-prdd.i-name tt-prdd.code /* display-time(tt-prdd.start) @ tt-prdd.start tt-prdd.start display-time(tt-prdd.start) @ tt-prdd.start display-time(tt-prdd.stopp) @ tt-prdd.stopp tt-prdd.stopp display-time(tt-prdd.stopp) @ tt-prdd.stopp */ tt-prdd.startx tt-prdd.stopx tt-prdd.hours tt-prdd.crew tt-prdd.qty tt-prdd.waste tt-prdd.complete   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table tt-prdd.m-code ~
tt-prdd.frm ~
tt-prdd.blank-no ~
tt-prdd.pass ~
tt-prdd.i-no ~
tt-prdd.code ~
tt-prdd.startx ~
tt-prdd.stopx ~
tt-prdd.hours ~
tt-prdd.crew ~
tt-prdd.qty ~
tt-prdd.waste ~
tt-prdd.complete   
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table tt-prdd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table tt-prdd
&Scoped-define SELF-NAME Browser-Table
&Scoped-define QUERY-STRING-Browser-Table FOR EACH tt-prdd     ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY {&SELF-NAME} FOR EACH tt-prdd     ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table tt-prdd
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table tt-prdd


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD actual-entered B-table-Win 
FUNCTION actual-entered RETURNS LOGICAL
  ( INPUT ip-m-code AS CHAR, INPUT ip-job AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-time B-table-Win 
FUNCTION display-time RETURNS CHARACTER
  ( INPUT ip-time AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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
     SIZE 60 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 70 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 55 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      tt-prdd SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _FREEFORM
  QUERY Browser-Table NO-LOCK DISPLAY
      tt-prdd.m-code    LABEL "Machine"               LABEL-BGCOLOR 14
      tt-prdd.frm       LABEL "S"   FORMAT ">>9":U    LABEL-BGCOLOR 14
      tt-prdd.blank-no  LABEL "B"   FORMAT ">9":U     LABEL-BGCOLOR 14
      tt-prdd.pass      LABEL "P"   FORMAT ">>9":U    LABEL-BGCOLOR 14
      tt-prdd.i-no                  FORMAT "x(15)":U  LABEL-BGCOLOR 14 WIDTH 20
      tt-prdd.i-name                FORMAT "x(30)":U  LABEL-BGCOLOR 14
      tt-prdd.code                  FORMAT "x(5)":U   LABEL-BGCOLOR 14
/*
      display-time(tt-prdd.start) @ tt-prdd.start
      tt-prdd.start FORMAT ">>,>>9":U
      display-time(tt-prdd.start) @ tt-prdd.start
      display-time(tt-prdd.stopp) @ tt-prdd.stopp
      tt-prdd.stopp FORMAT ">>,>>9":U
      display-time(tt-prdd.stopp) @ tt-prdd.stopp
      */
      tt-prdd.startx    FORMAT "99:99"       LABEL-BGCOLOR 14
      tt-prdd.stopx     FORMAT "99:99"       LABEL-BGCOLOR 14
      tt-prdd.hours     FORMAT ">>9.99-":U   LABEL-BGCOLOR 14
      tt-prdd.crew      FORMAT ">9.9":U      LABEL-BGCOLOR 14
      tt-prdd.qty       FORMAT ">>>>>>>9-":U LABEL-BGCOLOR 14
      tt-prdd.waste     FORMAT ">>>>9-":U    LABEL-BGCOLOR 14
      tt-prdd.complete  FORMAT "Y/N":U       LABEL-BGCOLOR 14
  ENABLE
      tt-prdd.m-code
      tt-prdd.frm
      tt-prdd.blank-no
      tt-prdd.pass
      tt-prdd.i-no
      tt-prdd.code
      tt-prdd.startx
      tt-prdd.stopx
      tt-prdd.hours
      tt-prdd.crew
      tt-prdd.qty
      tt-prdd.waste
      tt-prdd.complete
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 9.05
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 10.29 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     fi_sortby AT ROW 10.29 COL 38 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     auto_find AT ROW 10.29 COL 70 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 10.29 COL 132 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 10.29 COL 2
     RECT-4 AT ROW 10.05 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: ASI.pc-prdd
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
         HEIGHT             = 10.48
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
/* BROWSE-TAB Browser-Table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi_sortby:HIDDEN IN FRAME F-Main           = TRUE
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-prdd
    ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,"
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
ON DEFAULT-ACTION OF Browser-Table IN FRAME F-Main
DO:
    def var phandle as widget-handle no-undo.
    def var char-hdl as cha no-undo.   
    RUN get-link-handle IN adm-broker-hdl
       (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
    phandle = WIDGET-HANDLE(char-hdl).

    RUN new-state in phandle ('update-begin':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON HELP OF Browser-Table IN FRAME F-Main
DO:
    DEF VAR char-val AS cha NO-UNDO.
    DEF VAR rec-val AS RECID NO-UNDO.
    DEF VAR lw-focus AS HANDLE NO-UNDO.


    lw-focus = FOCUS.

    CASE lw-focus:NAME:
         WHEN "m-code" THEN DO:
             RUN windows/l-mach.w (g_company,g_loc,lw-focus:SCREEN-VALUE, OUTPUT char-val).
             IF char-val NE "" THEN lw-focus:SCREEN-VALUE = ENTRY(1,char-val). 
        END.
        WHEN "frm"      THEN RUN item-help (lw-focus).
        WHEN "blank-no" THEN RUN item-help (lw-focus).
        WHEN "i-no"     THEN RUN item-help (lw-focus).
        WHEN "code" THEN DO:
            RUN windows/l-jobcod.w (lw-focus:SCREEN-VALUE , OUTPUT char-val).
            IF char-val NE "" THEN lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
        END.        
    END CASE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON return OF Browser-Table IN FRAME F-Main
anywhere
DO:
   apply "tab" to self.
   return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}

  lv-dept = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   /*{src/adm/template/brsleave.i} */
   {brsleave.i}
/*   
    RUN dispatch ('update-record').
*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON START-SEARCH OF Browser-Table IN FRAME F-Main
DO:
   RUN startSearch.
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/*===========  browser field triggers ===========*/

ON 'entry':U OF  tt-prdd.m-code IN BROWSE {&browse-name}
DO:
   IF SELF:SCREEN-VALUE <> "" AND lv-adding-mode AND ll-skip THEN DO:
      APPLY "entry" TO tt-prdd.CODE IN BROWSE {&browse-name}.
      RETURN NO-APPLY.
   END.
END.

ON 'leave':U OF tt-prdd.m-code  IN BROWSE {&browse-name}
DO:
  IF LASTKEY NE -1 THEN DO:
    IF SELF:MODIFIED THEN RUN new-m-code.

    RUN valid-m-code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.  
END.

ON 'entry':U OF tt-prdd.frm IN BROWSE {&browse-name}
DO:
  IF ll-no-frm THEN DO:
    APPLY "leave" TO SELF.
    RETURN NO-APPLY.
  END.
END.

ON 'leave':U OF tt-prdd.frm IN BROWSE {&browse-name}
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-frm (SELF:MODIFIED) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

ON 'entry':U OF tt-prdd.blank-no IN BROWSE {&browse-name}
DO:
  IF ll-no-blk THEN DO:
    APPLY "leave" TO SELF.
    RETURN NO-APPLY.
  END.
END.

ON 'leave':U OF tt-prdd.blank-no IN BROWSE {&browse-name}
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-blank-no (SELF:MODIFIED) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

ON 'entry':U OF tt-prdd.pass IN BROWSE {&browse-name}
DO:
  IF lv-depts[1] EQ "GL" THEN DO:
    IF KEYLABEL(LASTKEY) EQ "shift-tab" THEN
       APPLY "shift-tab" TO SELF.
    ELSE
       APPLY "tab" TO SELF.
    RETURN NO-APPLY.
  END.
END.

ON 'leave':U OF tt-prdd.pass IN BROWSE {&browse-name}
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-pass NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

ON 'entry':U OF tt-prdd.i-no IN BROWSE {&browse-name}
DO:
  IF ll-no-blk THEN DO:
    APPLY "leave" TO SELF.
    RETURN NO-APPLY.
  END.
END.

ON 'leave':U OF tt-prdd.i-no IN BROWSE {&browse-name}
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-i-no (SELF:MODIFIED) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

ON 'leave':U OF tt-prdd.CODE  IN BROWSE {&browse-name}
DO:
   IF LASTKEY = -1 THEN RETURN.

   FIND FIRST job-code WHERE job-code.CODE = tt-prdd.CODE:SCREEN-VALUE IN BROWSE {&browse-name}
              NO-LOCK NO-ERROR.
   IF NOT AVAIL job-code THEN DO:
      MESSAGE "Invalid Job Code. Try Help. " VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.
   tt-prdd.CODE:SCREEN-VALUE = CAPS(tt-prdd.CODE:SCREEN-VALUE).
END.

ON 'value-changed':U OF tt-prdd.CODE  IN BROWSE {&browse-name}
DO:
  FIND FIRST job-code
      WHERE job-code.code EQ tt-prdd.CODE:SCREEN-VALUE IN BROWSE {&browse-name}
      NO-LOCK NO-ERROR.
  IF AVAIL job-code THEN RUN show-crew (job-code.cat).
END.

ON LEAVE OF tt-prdd.startx IN BROWSE {&browse-name} DO:
    IF SELF:MODIFIED IN BROWSE {&browse-name} THEN RUN new-mm.
    IF int(SUBSTRING(tt-prdd.startx:SCREEN-VALUE IN BROWSE {&browse-name} ,1,2)) >= 24 THEN DO:
        MESSAGE "Invalid Hours." 
                VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
     END.
     IF int(SUBSTRING(tt-prdd.startx:SCREEN-VALUE,4,2)) < 0 OR 
        int(SUBSTRING(tt-prdd.startx:SCREEN-VALUE,4,2)) >= 60 THEN DO:
        MESSAGE "Invalid Minites." VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
     END.
     tt-prdd.hours:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(
        ROUND(( 
                (int(SUBSTRING(tt-prdd.stopx:SCREEN-VALUE,1,2)) * 3600 +
           INT(SUBSTRING(tt-prdd.stopx:SCREEN-VALUE,4,2)) * 60 ) -
         ( int(SUBSTRING(tt-prdd.startx:SCREEN-VALUE,1,2)) * 3600 +
           INT(SUBSTRING(tt-prdd.startx:SCREEN-VALUE,4,2)) * 60 )
        ) / 3600 ,2) ).
END.

ON VALUE-CHANGED OF tt-prdd.startx IN BROWSE {&browse-name} DO:
    RUN new-hh.
END.

ON 'leave':U OF tt-prdd.stopx IN BROWSE {&browse-name} DO:
    IF SELF:MODIFIED IN BROWSE {&browse-name} THEN RUN new-mm.
    IF int(SUBSTRING(tt-prdd.stopx:SCREEN-VALUE IN BROWSE {&browse-name} ,1,2)) > 24 THEN DO:
        MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
     END.
     IF int(SUBSTRING(tt-prdd.stopx:SCREEN-VALUE,4,2)) < 0 OR 
        int(SUBSTRING(tt-prdd.stopx:SCREEN-VALUE,4,2)) >= 60 or
        (int(SUBSTRING(tt-prdd.stopx:SCREEN-VALUE IN BROWSE {&browse-name},1,2)) EQ 24 AND
         int(SUBSTRING(tt-prdd.stopx:SCREEN-VALUE,4,2)) NE 0) THEN DO:
        MESSAGE "Invalid Minites." VIEW-AS ALERT-BOX ERROR.        
        RETURN NO-APPLY.
     END.

     tt-prdd.hours:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(
        ROUND(( 
                (int(SUBSTRING(tt-prdd.stopx:SCREEN-VALUE,1,2)) * 3600 +
           INT(SUBSTRING(tt-prdd.stopx:SCREEN-VALUE,4,2)) * 60 ) -
         ( int(SUBSTRING(tt-prdd.startx:SCREEN-VALUE,1,2)) * 3600 +
           INT(SUBSTRING(tt-prdd.startx:SCREEN-VALUE,4,2)) * 60 )
        ) / 3600 ,2) ).
END.

ON VALUE-CHANGED OF tt-prdd.stopx IN BROWSE {&browse-name} DO:
  RUN new-hh.
END.

ON 'leave':U OF tt-prdd.crew IN BROWSE {&browse-name}
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-crew NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* ***************************  Main Block  *************************** */
{custom/yellowColumns.i}
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "pc-prdd"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "pc-prdd"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE auto-add B-table-Win 
PROCEDURE auto-add :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
    def var phandle as widget-handle no-undo.
    def var char-hdl as cha no-undo.   

    lv-is-first-prdd = PROGRAM-NAME(2) MATCHES "*v-prddj*".

    RUN get-link-handle IN adm-broker-hdl
       (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
    phandle = WIDGET-HANDLE(char-hdl).
    RUN auto-add in phandle .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE auto-add-next B-table-Win 
PROCEDURE auto-add-next :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-prev FOR tt-prdd2.

  /*FIND bf-prev WHERE Rowid(bf-prev) = RECID(tt-prdd) NO-LOCK NO-ERROR. 
         /* not working, rowid,recid are resetted   task# 06060510  YSK */
  */
  FIND FIRST bf-prev NO-LOCK NO-ERROR.

  RUN auto-add.
  ASSIGN tt-prdd.m-code:SCREEN-VALUE IN BROWSE {&browse-name} = bf-prev.m-code         
         tt-prdd.blank-no:SCREEN-VALUE = STRING(bf-prev.blank-no)
         tt-prdd.frm:SCREEN-VALUE = STRING(bf-prev.frm)
         tt-prdd.i-no:SCREEN-VALUE = bf-prev.i-no
         tt-prdd.i-name:SCREEN-VALUE = bf-prev.i-name
         tt-prdd.job = bf-prev.job
         lv-dept = "".
  APPLY "entry" TO tt-prdd.CODE IN BROWSE {&browse-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE auto-update B-table-Win 
PROCEDURE auto-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN local-create-record.
    RUN dispatch ('display-fields').

    def var phandle as widget-handle no-undo.
    def var char-hdl as cha no-undo.   
    RUN get-link-handle IN adm-broker-hdl
       (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
    phandle = WIDGET-HANDLE(char-hdl).

    RUN new-state in phandle ('update-begin':U).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table B-table-Win 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH tt-prdd:
  DELETE tt-prdd.
END.

FOR EACH bf-prdd WHERE bf-prdd.company = pc-prdd.company 
                   AND bf-prdd.job-no = pc-prdd.job-no
                   AND bf-prdd.job-no2 = pc-prdd.job-no2
                   AND bf-prdd.op-date = pc-prdd.op-date
                   AND bf-prdd.shift = pc-prdd.shift NO-LOCK:
    CREATE tt-prdd.
    BUFFER-COPY bf-prdd TO tt-prdd
    ASSIGN tt-prdd.row-id = ROWID(bf-prdd).
END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-item B-table-Win 
PROCEDURE display-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF NOT AVAIL job-hdr THEN
    FIND FIRST job-hdr
        WHERE job-hdr.company EQ pc-prdd.company
          AND job-hdr.job     EQ pc-prdd.job
          AND job-hdr.job-no  EQ pc-prdd.job-no
          AND job-hdr.job-no2 EQ pc-prdd.job-no2
          AND (job-hdr.frm    EQ INT(tt-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name}) OR
               v-est-type EQ 2 OR v-est-type EQ 6)
          AND (job-hdr.blank-no EQ INT(tt-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) OR
               v-est-type EQ 2 OR v-est-type EQ 6)
        NO-LOCK NO-ERROR.

    IF AVAIL job-hdr THEN DO:
      RELEASE reftable.

      FIND FIRST mach
          WHERE mach.company EQ cocode
            AND mach.loc     EQ locode
            AND mach.m-code  EQ tt-prdd.m-code:SCREEN-VALUE
          NO-LOCK NO-ERROR.

      IF (v-est-type EQ 2 OR v-est-type EQ 6)             AND
         (NOT AVAIL mach OR INDEX("AP",mach.p-type) LE 0) THEN
      FIND FIRST reftable
          WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job-hdr.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job-hdr.job,"999999999")
            AND reftable.val[12]  EQ INT(tt-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name})
            AND (reftable.val[13] EQ INT(tt-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) OR
                 INT(tt-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0)
          NO-LOCK NO-ERROR.

      tt-prdd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} =
          IF AVAIL reftable THEN reftable.code2 ELSE job-hdr.i-no.

      FIND FIRST itemfg
          WHERE itemfg.company EQ job-hdr.company
            AND itemfg.i-no    EQ tt-prdd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          NO-LOCK NO-ERROR.

      tt-prdd.i-name:SCREEN-VALUE IN BROWSE {&browse-name} =
          IF AVAIL itemfg THEN itemfg.i-name ELSE "".
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-job-hdr B-table-Win 
PROCEDURE display-job-hdr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ip-recid AS RECID NO-UNDO.


DO WITH FRAME {&FRAME-NAME}:
  FIND job-hdr WHERE RECID(job-hdr) EQ ip-recid NO-LOCK NO-ERROR.

  IF AVAIL job-hdr AND
     (job-hdr.frm NE INT(tt-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name})             OR
      (job-hdr.blank-no NE 0 AND
       job-hdr.blank-no NE INT(tt-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name})) OR
      job-hdr.i-no NE tt-prdd.i-no:SCREEN-VALUE IN BROWSE {&browse-name})
  THEN DO:
    RUN display-item.

    IF v-est-type NE 2 AND v-est-type NE 6 THEN DO:
      IF NOT ll-no-frm THEN
        tt-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(job-hdr.frm).
      IF NOT ll-no-blk AND job-hdr.blank-no NE 0 THEN
        tt-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name} =
                                                        STRING(job-hdr.blank-no).
    END.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-num-on B-table-Win 
PROCEDURE get-num-on :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    tt-job-mch.n-on = tt-job-mch.n-out.

    FIND FIRST mach NO-LOCK
        WHERE mach.company EQ tt-job-mch.company
          AND mach.m-code  EQ tt-job-mch.m-code
        NO-ERROR.
    IF AVAIL mach AND NOT CAN-DO("A,P,B",mach.p-type) THEN
    FOR EACH job NO-LOCK
        WHERE job.company      EQ tt-job-mch.company
          AND job.job          EQ tt-job-mch.job
          AND job.job-no       EQ tt-job-mch.job-no
          AND job.job-no2      EQ tt-job-mch.job-no2
          AND TRIM(job.est-no) NE "",
        FIRST ef NO-LOCK
        WHERE ef.company EQ job.company
          AND ef.est-no  EQ job.est-no
          AND ef.form-no EQ tt-job-mch.frm:
      RUN sys/inc/numup.p (ef.company, ef.est-no, ef.form-no, OUTPUT tt-job-mch.n-on).
      tt-job-mch.n-on = tt-job-mch.n-on * tt-job-mch.n-out.
      LEAVE.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE item-help B-table-Win 
PROCEDURE item-help :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.

  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR rec-val AS RECID NO-UNDO.
  DEF VAR lv-jobitm AS CHAR NO-UNDO.
  DEF BUFFER bf-mach FOR mach.


  DO WITH FRAME {&FRAME-NAME}:
    lv-jobitm = TRIM(tt-prdd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}) + "," +
                TRIM(tt-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name}) + ","  +
                TRIM(tt-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    RUN windows/l-jobit2.w (g_company, pc-prdd.job-no, pc-prdd.job-no2, lv-jobitm, OUTPUT char-val, OUTPUT rec-val).
    IF char-val NE "" THEN DO:
      ASSIGN tt-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name}      = ENTRY(2,char-val).
      
      /*Don't apply blank number if machine is sheet or roll fed - task 02281303*/
      FIND FIRST bf-mach WHERE bf-mach.company = cocode 
          AND bf-mach.m-code = tt-prdd.m-code:SCREEN-VALUE IN BROWSE {&browse-name} NO-LOCK NO-ERROR.
      IF AVAIL bf-mach AND LOOKUP(bf-mach.p-type,"R,S") = 0  THEN
          tt-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name} = ENTRY(3,char-val).

      RUN display-job-hdr (rec-val).
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE list-edit B-table-Win 
PROCEDURE list-edit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT-OUTPUT PARAM io-list AS CHAR NO-UNDO.


   io-list = TRIM(io-list).
   IF LENGTH(io-list) LT 1 THEN RETURN.

   IF SUBSTR(io-list,LENGTH(io-list),1) EQ "," THEN
     SUBSTR(io-list,LENGTH(io-list),1) = "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record B-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT AVAIL tt-prdd THEN RUN dispatch ("open-query").

  IF (AVAIL tt-prdd                    AND
      tt-prdd.row-id EQ ROWID(pc-prdd) AND
      tt-prdd.m-code EQ "")            THEN lv-is-first-prdd = YES.

  /* Dispatch standard ADM method.                             */
  IF NOT lv-is-first-prdd THEN
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
  ELSE RUN auto-update.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-name AS cha NO-UNDO.
  DEF VAR ll-job-mch AS LOG NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR v-bf-prdd-rowid AS ROWID NO-UNDO.
  DEFINE VARIABLE lNewJobMch AS LOGICAL NO-UNDO.
  
  /* Code placed here will execute PRIOR to standard behavior. */
  lv-name = tt-prdd.i-name:SCREEN-VALUE IN BROWSE {&browse-name}.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  li-help-job = 0.

  FIND FIRST mach
      WHERE mach.company EQ g_company
        AND mach.m-code  EQ tt-prdd.m-code
      NO-LOCK NO-ERROR.

  ASSIGN
   tt-prdd.dept   = IF lv-dept EQ "" THEN mach.dept[1] ELSE lv-dept
   tt-prdd.i-name = lv-name
   tt-prdd.START  = (INT(SUBSTR(tt-prdd.startx,1,2)) * 3600) +
                    (INT(SUBSTR(tt-prdd.startx,3,2)) * 60)
   tt-prdd.stopp  = (INT(SUBSTR(tt-prdd.stopx,1,2)) * 3600) +
                    (INT(SUBSTR(tt-prdd.stopx,3,2)) * 60).

  FIND bf-prdd WHERE ROWID(bf-prdd) EQ tt-prdd.row-id NO-ERROR.
  IF NOT AVAIL bf-prdd THEN DO:
    CREATE bf-prdd.
    tt-prdd.row-id = ROWID(bf-prdd).
  END.

  BUFFER-COPY tt-prdd TO bf-prdd.

  ASSIGN
   bf-prdd.code = CAPS(bf-prdd.code)
   bf-prdd.job  = pc-prdd.job
   v-bf-prdd-rowid = ROWID(bf-prdd)   .
  /* for notes - same as job and estimate */
  FIND FIRST job WHERE job.company = bf-prdd.company
                       AND job.job-no = bf-prdd.job-no
                       AND job.job-no2 = bf-prdd.job-no2
                       NO-LOCK NO-ERROR.
  IF AVAIL job THEN bf-prdd.rec_key = job.rec_key.

  FIND CURRENT bf-prdd NO-LOCK.

  find first pc-prdh
      where pc-prdh.company    eq g_company
        and pc-prdh.m-code     eq tt-prdd.m-code
        and pc-prdh.trans-date eq tt-prdd.op-date
        and pc-prdh.shift      eq tt-prdd.shift
      no-lock no-error.
  if not avail pc-prdh then do:
    create pc-prdh.
    assign
     pc-prdh.company    = g_company
     pc-prdh.m-code     = tt-prdd.m-code
     pc-prdh.trans-date = tt-prdd.op-date
     pc-prdh.shift      = tt-prdd.shift
     pc-prdh.dept       = tt-prdd.dept.
  END.

  ll-job-mch = NO.
  FOR EACH tt-job-mch:
    FIND FIRST job-mch WHERE ROWID(job-mch) EQ tt-job-mch.row-id NO-ERROR.
    lNewJobMch = NOT AVAILABLE job-mch.
    IF NOT AVAIL job-mch THEN CREATE job-mch.
    BUFFER-COPY tt-job-mch TO job-mch NO-ERROR.

    IF tt-job-mch.row-id EQ ? THEN DO:
      IF ERROR-STATUS:ERROR THEN DELETE job-mch.
      
      IF AVAIL job-mch THEN DO:
        ASSIGN
         ll-job-mch       = YES
         job-mch.j-no     = 1
         job-mch.i-name   = tt-prdd.i-name
         job-mch.dept     = tt-prdd.dept
         job-mch.run-hr   = tt-prdd.hours
         job-mch.mr-rate  = mach.mr-rate
         job-mch.mr-varoh = mach.mr-varoh
         job-mch.mr-fixoh = mach.mr-fixoh         
         job-mch.wst-prct = mach.run-spoil.
        IF tt-prdd.speed GT 0 THEN
          job-mch.speed    = tt-prdd.speed.
        IF lNewJobMch THEN 
          job-mch.est-op_rec_key = "".
      END.

    END.

    FIND CURRENT job-mch NO-LOCK NO-ERROR.

    RELEASE job-mch.

    DELETE tt-job-mch.
  END.

  IF ll-job-mch THEN DO:
    EMPTY TEMP-TABLE w-jm.

    FOR EACH job-mch
        WHERE job-mch.company EQ tt-prdd.company
          AND job-mch.job     EQ tt-prdd.job
          AND job-mch.job-no  EQ tt-prdd.job-no
          AND job-mch.job-no2 EQ tt-prdd.job-no2
        NO-LOCK:
      FIND FIRST mach
          WHERE mach.company EQ g_company
            AND mach.m-code  EQ job-mch.m-code
          NO-LOCK NO-ERROR.
       CREATE w-jm.
       ASSIGN
        w-jm.frm      = job-mch.frm
        w-jm.d-seq    = IF AVAIL mach THEN mach.d-seq ELSE 9999
        w-jm.blank-no = job-mch.blank-no
        w-jm.row-id   = ROWID(job-mch).
    END.

    li = 0.
    FOR EACH w-jm BY w-jm.frm BY w-jm.d-seq BY w-jm.blank-no:
      FIND job-mch WHERE ROWID(job-mch) EQ w-jm.row-id.
      li = li + 1.
      job-mch.LINE = li.
      FIND CURRENT job-mch NO-LOCK NO-ERROR.
      RELEASE job-mch.
    END.
  END.

  /*
  /* task 11170511 */  
  IF pc-prdd.code EQ "RUN" AND fgrecpt-char EQ "AUTOPOST" THEN
  FOR EACH job-mch
      WHERE job-mch.company EQ cocode
        AND job-mch.job     EQ pc-prdd.job
        AND job-mch.job-no  EQ pc-prdd.job-no
        AND job-mch.job-no2 EQ pc-prdd.job-no2
        AND job-mch.frm     EQ pc-prdd.frm
      USE-INDEX line-idx NO-LOCK,
      FIRST mach
      {sys/look/machW.i}
        AND mach.m-code EQ job-mch.m-code
        AND INDEX("AP",mach.p-type) LE 0
      NO-LOCK
      BY job-mch.line DESC:

    IF job-mch.m-code EQ pc-prdd.m-code THEN RUN proc-form-cmplt.

    LEAVE.
  END.  /* run */
  /* end of mods task 11170511*/
  */

  /* for adding mode */
  FIND FIRST tt-prdd2 NO-ERROR.
  IF AVAIL tt-prdd2 THEN DELETE tt-prdd2.
  CREATE tt-prdd2.
  BUFFER-COPY tt-prdd TO tt-prdd2.

  FOR EACH pc-prdh
      WHERE pc-prdh.company    EQ g_company
        AND pc-prdh.m-code     EQ ""
        AND pc-prdh.trans-date EQ tt-prdd.op-date
        AND pc-prdh.shift      EQ tt-prdd.shift:
    DELETE pc-prdh.
  END.
    
  RUN pc/pcprdd3u.p (v-bf-prdd-rowid) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN "adm-error".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record B-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-delete AS LOG NO-UNDO.

  
  /* Code placed here will execute PRIOR to standard behavior. */
  ll-delete = tt-prdd.row-id EQ ROWID(pc-prdd) AND tt-prdd.m-code EQ "".

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF ll-delete THEN RUN dispatch ("delete-record").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  lv-adding-mode = YES.

  /* Dispatch standard ADM method.                             */
  IF NOT lv-is-first-prdd THEN
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ).

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST pc-prdh WHERE pc-prdh.company = pc-prdd.company
                       AND pc-prdh.m-code = pc-prdd.m-code
                       AND pc-prdh.TRANS-date = pc-prdd.op-date
                       AND pc-prdh.shift = pc-prdd.shift
                       NO-LOCK NO-ERROR.
  
  FIND mach WHERE mach.company = g_company AND
                  mach.m-code = pc-prdh.m-code 
                  NO-LOCK NO-ERROR.
  ASSIGN tt-prdd.company = g_company
         tt-prdd.job = pc-prdd.job
         tt-prdd.job-no = pc-prdd.job-no
         tt-prdd.job-no2 = pc-prdd.job-no2
         tt-prdd.op-date = pc-prdd.op-date
         tt-prdd.op-time = TIME
         tt-prdd.shift = pc-prdd.shift         
         tt-prdd.opn = YES
         tt-prdd.START = TIME
         tt-prdd.startx = "0000"
         tt-prdd.stopx = "0000"
         tt-prdd.crew = 0
         lv-is-first-prdd = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rowid AS ROWID NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  lv-adding-mode = NO.

  IF NOT adm-new-record AND tt-prdd.m-code NE "" THEN DO:
    {custom/askdel.i}
  END.

  lv-rowid = tt-prdd.row-id.

  FIND bf-prdd WHERE ROWID(bf-prdd) EQ tt-prdd.row-id NO-ERROR.

  /* Dispatch standard ADM method */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
  IF AVAIL bf-prdd THEN DO:
    IF lv-rowid EQ ROWID(pc-prdd) THEN DO:
      lv-is-first-prdd = NO.
      RUN dispatch ("create-record").
      BUFFER-COPY tt-prdd TO bf-prdd.
      DELETE tt-prdd.
    END.

    ELSE DO:
      DISABLE TRIGGERS FOR LOAD OF bf-prdd.
      DELETE bf-prdd.
    END.
  END.

  /*FIND FIRST tt-prdd NO-ERROR.
  IF NOT AVAIL tt-prdd THEN DO:
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
    RUN reopen-query IN WIDGET-HANDLE(char-hdl).
  END.*/

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
  FOR EACH tt-job-mch:
    DELETE tt-job-mch.
  END.

  ASSIGN
   lv-depts    = ""
   lv-depts[1] = tt-prdd.dept.

  find first job
      where job.company eq cocode
        and job.job-no  eq pc-prdd.job-no
        and job.job-no2 eq pc-prdd.job-no2
      use-index job-no no-lock no-error.

  v-est-type = 1.
  if avail job then do:
    find first est
        where est.company eq job.company
          and est.est-no  eq job.est-no
        no-lock no-error.
    if avail est then v-est-type = est.est-type - if est.est-type gt 4 then 4 else 0.
  end.

  ll-no-frm = v-est-type EQ 1.
  
  FIND FIRST mach
      WHERE mach.company EQ g_company
        AND mach.m-code  EQ tt-prdd.m-code
        NO-LOCK NO-ERROR.

  IF AVAIL mach THEN
    ASSIGN
     lv-depts[1] = CAPS(mach.dept[1])
     lv-depts[2] = CAPS(mach.dept[2])
     lv-depts[3] = CAPS(mach.dept[3])
     lv-depts[4] = CAPS(mach.dept[4])
     ll-no-blk   = v-est-type EQ 1 OR (mach.p-type NE "B" AND (v-est-type NE 3 OR mach.dept[1] NE "PR")).

  ll-skip = YES.

  DO WITH FRAME {&FRAME-NAME}:
    APPLY "entry" TO tt-prdd.m-code IN BROWSE {&browse-name}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-end-update B-table-Win 
PROCEDURE local-end-update :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'end-update':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
 
  IF lv-adding-mode THEN DO:      
    RUN repo-query (ROWID(tt-prdd)).
    RUN auto-add-next.     
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN build-table.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    APPLY "VALUE-CHANGED" TO browse-order.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rowid AS ROWID NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  lv-rowid = ROWID(tt-prdd).

  RUN valid-m-code NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-frm (NO) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-blank-no (NO) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-pass NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-i-no (NO) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  IF int(SUBSTRING(tt-prdd.startx:SCREEN-VALUE IN BROWSE {&browse-name} ,1,2)) >= 24 THEN DO:
        MESSAGE "Invalid Hours." 
                VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO tt-prdd.startx.
        RETURN NO-APPLY.
     END.
     IF int(SUBSTRING(tt-prdd.startx:SCREEN-VALUE,4,2)) < 0 OR 
        int(SUBSTRING(tt-prdd.startx:SCREEN-VALUE,4,2)) >= 60 THEN DO:
        MESSAGE "Invalid Minites." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO tt-prdd.startx.
        RETURN .
     END.
     IF int(SUBSTRING(tt-prdd.stopx:SCREEN-VALUE IN BROWSE {&browse-name} ,1,2)) > 24 THEN DO:
        MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO tt-prdd.stopx.
        RETURN NO-APPLY.
     END.
     IF int(SUBSTRING(tt-prdd.stopx:SCREEN-VALUE,4,2)) < 0 OR 
        int(SUBSTRING(tt-prdd.stopx:SCREEN-VALUE,4,2)) >= 60 or
        (int(SUBSTRING(tt-prdd.stopx:SCREEN-VALUE IN BROWSE {&browse-name},1,2)) EQ 24 AND
         int(SUBSTRING(tt-prdd.stopx:SCREEN-VALUE,4,2)) NE 0) THEN DO:
        MESSAGE "Invalid Minites." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO tt-prdd.stopx.        
        RETURN NO-APPLY.
     END.

     tt-prdd.hours:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(
        ROUND(( 
                (int(SUBSTRING(tt-prdd.stopx:SCREEN-VALUE,1,2)) * 3600 +
           INT(SUBSTRING(tt-prdd.stopx:SCREEN-VALUE,4,2)) * 60 ) -
         ( int(SUBSTRING(tt-prdd.startx:SCREEN-VALUE,1,2)) * 3600 +
           INT(SUBSTRING(tt-prdd.startx:SCREEN-VALUE,4,2)) * 60 )
        ) / 3600 ,2) ).
     IF INT(tt-prdd.hours:SCREEN-VALUE IN BROWSE {&browse-name}) < 0 THEN DO:
        message "Time entered will create negative hours, OK to proceed? "
                VIEW-AS ALERT-BOX WARNING BUTTON YES-NO UPDATE ll-ans AS LOG.
        IF NOT ll-ans OR ll-ans = ? THEN DO:
           APPLY "entry" TO tt-prdd.stopx.
           RETURN .
        END.
     END.

  RUN valid-crew NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF NOT lv-adding-mode THEN RUN repo-query (lv-rowid).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-hh B-table-Win 
PROCEDURE new-hh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-hh AS CHAR NO-UNDO.
  DEF VAR lv-mm AS CHAR NO-UNDO.

  
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     lv-hh = SUBSTR(FOCUS:SCREEN-VALUE IN BROWSE {&browse-name},1,2)
     lv-mm = SUBSTR(FOCUS:SCREEN-VALUE IN BROWSE {&browse-name},4,2).

    IF INT(lv-hh) GT 2 AND INT(lv-hh) LT 10 THEN DO:
      FOCUS:SCREEN-VALUE IN BROWSE {&browse-name} =
          STRING("0" + STRING(INT(lv-hh),"9") + lv-mm,FOCUS:FORMAT IN BROWSE {&browse-name}).
      APPLY "cursor-right" TO FOCUS IN BROWSE {&browse-name}.
      APPLY "cursor-right" TO FOCUS IN BROWSE {&browse-name}.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-job-hdr B-table-Win 
PROCEDURE new-job-hdr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST job-hdr
        WHERE job-hdr.company    EQ pc-prdd.company
          AND job-hdr.job-no     EQ pc-prdd.job-no
          AND job-hdr.job-no2    EQ pc-prdd.job-no2
          AND (job-hdr.frm       EQ INT(tt-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name}) OR
               v-est-type EQ 2 OR v-est-type EQ 6)
          AND ((job-hdr.blank-no EQ INT(tt-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) AND
                INT(tt-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0) OR
               (job-hdr.i-no     EQ tt-prdd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} AND
                tt-prdd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "") OR
               v-est-type EQ 2 OR v-est-type EQ 6 OR
               INT(tt-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0)
        NO-LOCK NO-ERROR.
    IF AVAIL job-hdr THEN RUN display-job-hdr (RECID(job-hdr)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-m-code B-table-Win 
PROCEDURE new-m-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-frm LIKE tt-prdd.frm INIT 1 NO-UNDO.
  DEF VAR lv-blk LIKE tt-prdd.blank-no INIT 0 NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     lv-dept = ""
     tt-prdd.m-code:SCREEN-VALUE IN BROWSE {&browse-name} =
        CAPS(tt-prdd.m-code:SCREEN-VALUE IN BROWSE {&browse-name}).

    FIND FIRST mach
        WHERE mach.company EQ g_company
          AND mach.m-code  EQ tt-prdd.m-code:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.

    IF AVAIL mach THEN DO:
      
      ASSIGN
       lv-depts[1] = CAPS(mach.dept[1])
       lv-depts[2] = CAPS(mach.dept[2])
       lv-depts[3] = CAPS(mach.dept[3])
       lv-depts[4] = CAPS(mach.dept[4])
       lv-schmch   = IF mach.sch-m-code NE "" THEN mach.sch-m-code
                     ELSE mach.m-code
       lv-blk = IF mach.p-type EQ "B" THEN 1 ELSE 0
       ll-no-blk = v-est-type EQ 1 OR (mach.p-type NE "B" AND (v-est-type NE 3 OR mach.dept[1] NE "PR")).
    END.

    FIND FIRST job-mch
        WHERE job-mch.company EQ pc-prdd.company
          AND job-mch.job-no  EQ pc-prdd.job-no
          AND job-mch.job-no2 EQ pc-prdd.job-no2
          AND job-mch.m-code  EQ tt-prdd.m-code:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.

    IF AVAIL job-mch THEN
      ASSIGN
       lv-frm = job-mch.frm
       lv-blk = job-mch.blank-no.
        
    FIND FIRST job-hdr
        WHERE job-hdr.company   EQ pc-prdd.company
          AND job-hdr.job-no    EQ pc-prdd.job-no
          AND job-hdr.job-no2   EQ pc-prdd.job-no2
          AND job-hdr.frm       EQ lv-frm
          AND (job-hdr.blank-no EQ lv-blk OR lv-blk EQ 0)
        NO-LOCK NO-ERROR.
    IF AVAIL job-hdr THEN DO:
      tt-prdd.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = job-hdr.i-no.
      
      FIND FIRST itemfg
          WHERE itemfg.company EQ job-hdr.company
            AND itemfg.i-no    EQ job-hdr.i-no
          NO-LOCK NO-ERROR.
      IF AVAIL itemfg THEN tt-prdd.i-name:SCREEN-VALUE = itemfg.i-name.
    END.

    ASSIGN
     tt-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name}      = STRING(lv-frm)
     tt-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(lv-blk).

    RUN new-job-hdr.

    RUN show-crew ("RUN").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-mm B-table-Win 
PROCEDURE new-mm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-hh AS CHAR NO-UNDO.
  DEF VAR lv-mm AS CHAR NO-UNDO.

  
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     lv-hh = SUBSTR(FOCUS:SCREEN-VALUE IN BROWSE {&browse-name},1,2)
     lv-mm = SUBSTR(FOCUS:SCREEN-VALUE IN BROWSE {&browse-name},4,2).

    lv-hh = STRING(INT(lv-hh),"99"). 

    FOCUS:SCREEN-VALUE IN BROWSE {&browse-name} =
        STRING(lv-hh + TRIM(lv-mm) + SUBSTR("00",1,2 - LENGTH(TRIM(lv-mm))),FOCUS:FORMAT IN BROWSE {&browse-name}).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-form-cmplt B-table-Win 
PROCEDURE proc-form-cmplt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /* from pc/pcprdd3u.p pcprdd4u.p */
   DEF VAR v-est-type           LIKE est.est-type NO-UNDO.
   DEF VAR v-loc                LIKE fg-bin.loc NO-UNDO.
   DEF VAR v-loc-bin            LIKE fg-bin.loc-bin NO-UNDO.
   DEF VAR v-qty                AS   INT NO-UNDO.
   DEF VAR choice               AS   LOG NO-UNDO.
   DEF VAR v-assembled AS   LOG NO-UNDO.
   DEF VAR v-runqty AS INT NO-UNDO.
   DEF VAR X AS INT NO-UNDO.
   DEF VAR v-up AS INT NO-UNDO.
   DEF VAR v-out AS INT NO-UNDO.
   def var v-up-hs     like eb.num-up NO-UNDO.
   def var v-on        like eb.num-up NO-UNDO.
   DEF VAR h_updbin AS HANDLE NO-UNDO.
   DEF VAR li-units AS INT NO-UNDO.

   DEF BUFFER b-reftable FOR reftable.

   FIND FIRST job WHERE job.company EQ tt-prdd.company
        AND job.job-no  EQ tt-prdd.job-no
        AND job.job-no2 EQ tt-prdd.job-no2
      USE-INDEX job-no NO-LOCK NO-ERROR.

  FIND FIRST est
      WHERE est.company EQ job.company
        AND est.est-no  EQ job.est-no
      NO-LOCK NO-ERROR.
  v-est-type = IF AVAIL est THEN est.est-type ELSE 1.

  IF v-est-type GT 4 THEN v-est-type = v-est-type - 4.

  v-assembled = NO.

 /* IF v-assembled THEN do for both assembled or unassembled */
  FOR EACH reftable
      WHERE reftable.reftable EQ "jc/jc-calc.p"
        AND reftable.company  EQ job-mch.company
        AND reftable.loc      EQ ""
        AND reftable.code     EQ STRING(job-mch.job,"999999999")
        AND reftable.val[12]  EQ job-mch.frm
        AND (reftable.val[13] EQ job-mch.blank-no OR
             job-mch.blank-no EQ 0),
      EACH job-hdr
      WHERE job-hdr.company   EQ cocode
        AND job-hdr.job-no    EQ job-mch.job-no
        AND job-hdr.job-no2   EQ job-mch.job-no2
        AND (job-hdr.frm      EQ job-mch.frm OR v-est-type       EQ 2 )
        AND (job-hdr.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0 ),

      FIRST itemfg
      WHERE itemfg.company EQ job-hdr.company
        AND itemfg.i-no    EQ reftable.code2 NO-LOCK:

    /*IF itemfg.isaset AND itemfg.alloc NE YES THEN DO:
      ASSIGN
       v-set  = itemfg.i-no
       v-qty  = pc-prdd.qty.
            
      RUN fg/checkset.p (RECID(itemfg), ?, INPUT-OUTPUT v-qty).
          
      IF v-qty LT pc-prdd.qty THEN DO:
        choice = NO.
        MESSAGE "Insufficient components for AUTOPOST, process anyway?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE choice.
        IF NOT choice THEN RETURN ERROR.
      END.
    END.*/    

    RUN fg/autopost.p (ROWID(itemfg), job-hdr.job-no, job-hdr.job-no2,
                       OUTPUT v-loc, OUTPUT v-loc-bin).

    FIND FIRST fg-bin
        WHERE fg-bin.company EQ itemfg.company
          AND fg-bin.i-no    EQ itemfg.i-no
          AND fg-bin.loc     EQ v-loc
          AND fg-bin.loc-bin EQ v-loc-bin  
          AND fg-bin.tag     EQ ""
          AND fg-bin.job-no  EQ job-hdr.job-no
          AND fg-bin.job-no2 EQ job-hdr.job-no2
        NO-ERROR.
    IF NOT AVAIL fg-bin THEN DO:
      CREATE fg-bin.
      ASSIGN
       fg-bin.company      = itemfg.company
       fg-bin.loc          = v-loc
       fg-bin.loc-bin      = v-loc-bin
       fg-bin.i-no         = reftable.code2
       fg-bin.tag          = ""
       fg-bin.job-no       = job-hdr.job-no
       fg-bin.job-no2      = job-hdr.job-no2
       fg-bin.std-mat-cost = reftable.val[2]
       fg-bin.std-lab-cost = reftable.val[1]
       fg-bin.std-fix-cost = reftable.val[4]
       fg-bin.std-var-cost = reftable.val[3]
       fg-bin.std-tot-cost = reftable.val[5]
       fg-bin.last-cost    = job-hdr.std-tot-cost
       fg-bin.unit-count   = itemfg.case-count.
    END.
      
    IF fg-bin.cases-unit   LE 0 THEN fg-bin.cases-unit   = 1.
    IF fg-bin.units-pallet LE 0 THEN fg-bin.units-pallet = 1.
    
    FIND FIRST b-reftable
        WHERE b-reftable.reftable EQ "ts/jobdata.p"
          AND b-reftable.company  EQ cocode
          AND b-reftable.code     EQ job-hdr.rec_key
        EXCLUSIVE NO-ERROR.
    IF AVAIL b-reftable THEN DELETE b-reftable.
    CREATE b-reftable.
    ASSIGN
     b-reftable.reftable = "ts/jobdata.p"
     b-reftable.company  = cocode
     b-reftable.code     = job-hdr.rec_key
     b-reftable.code2    = fg-bin.rec_key
     li-units            = b-reftable.val[1]
     v-runqty = 0. 

    FOR EACH bf-prdd fields(qty) WHERE bf-prdd.company = tt-prdd.company 
                       AND bf-prdd.m-code = tt-prdd.m-code
                       AND bf-prdd.job-no = tt-prdd.job-no
                       AND bf-prdd.job-no2 = tt-prdd.job-no2
                       AND bf-prdd.FRM = tt-prdd.frm
                       AND bf-prdd.blank-no = tt-prdd.blank-no
                       AND bf-prdd.pass = tt-prdd.pass
                       NO-LOCK:
        v-runqty = v-runqty + bf-prdd.qty.
    END.                                      /*employee_code*/
    /*RUN addon/touch/d-updbin.w  (ROWID(fg-bin), v-runqty,'',cocode). /* pc-prdd.qty*/*/
    RUN pc/d-updbin.w  (ROWID(fg-bin), v-runqty, INPUT-OUTPUT li-units). /* pc-prdd.qty*/

    FIND CURRENT fg-bin NO-LOCK.

    ASSIGN
     b-reftable.val[1] = li-units
     b-reftable.val[2] = fg-bin.case-count
     b-reftable.val[3] = fg-bin.cases-unit.
  END.  /* v-assembled */

    FIND FIRST job WHERE job.company EQ cocode
                        AND job.job-no  EQ tt-prdd.job-no
                        AND job.job-no2 EQ tt-prdd.job-no2
                        USE-INDEX job-no NO-ERROR.
    ASSIGN v-up  = 1
           v-out = 1
           v-on  = 1.

    FIND FIRST est WHERE est.company EQ job.company
                     AND est.est-no  EQ job.est-no
                     NO-LOCK NO-ERROR.
    v-est-type = IF AVAIL est THEN est.est-type ELSE 1.
    IF v-est-type GT 4 THEN v-est-type = v-est-type - 4.

    FIND FIRST mach WHERE mach.company = job-mch.company
                       AND mach.m-code = job-mch.m-code NO-LOCK NO-ERROR.

    IF AVAIL mach THEN
       FOR EACH mach-part WHERE
           mach-part.company EQ mach.company AND
           mach-part.m-code EQ mach.m-code
           EXCLUSIVE-LOCK:
           
           mach-part.total-impressions-run = mach-part.total-impressions-run
                                           + tt-prdd.qty + tt-prdd.waste.

           FIND FIRST reftable WHERE
                reftable.reftable EQ "MACHPARTHOURS" AND
                reftable.company  EQ mach-part.company AND
                reftable.loc      EQ mach-part.m-code AND
                reftable.code     EQ mach-part.rm-part-code
                EXCLUSIVE-LOCK NO-ERROR.

           IF NOT AVAIL reftable THEN DO:
              CREATE reftable.
              ASSIGN
                 reftable.reftable = "MACHPARTHOURS"
                 reftable.company  = mach-part.company
                 reftable.loc      = mach-part.m-code
                 reftable.code     = mach-part.rm-part-code. 
           END.

           reftable.val[1] = reftable.val[1]
                           + tt-prdd.hours.

           RELEASE reftable.
       END.

    IF mach.dept[1] EQ "PR" OR mach.dept[2] EQ "PR" OR
       mach.dept[3] EQ "PR" OR mach.dept[4] EQ "PR" THEN
       RUN update-plate-die ("P", v-est-type).

    IF mach.dept[1] EQ "DC" OR mach.dept[2] EQ "DC" OR
       mach.dept[3] EQ "DC" OR mach.dept[4] EQ "DC" THEN
       RUN update-plate-die ("D", v-est-type).

    if avail est then do:
       run sys/inc/numup.p (est.company, est.est-no, job-mch.frm, output v-up).

    find first ef
        where ef.company eq est.company
          and ef.est-no  eq est.est-no
          and ef.form-no eq job-mch.frm
        no-lock no-error.

    IF AVAIL ef THEN DO:
      RUN est/ef-#out.p (ROWID(ef), OUTPUT v-on).
      v-on = v-up * v-on.
    END.
                      
    find first est-op
        where est-op.company eq est.company
          and est-op.est-no  eq est.est-no
          and est-op.s-num   eq job-mch.frm
          and (est-op.b-num  eq job-mch.blank-no OR job-mch.blank-no eq 0)
          and est-op.m-code  eq job-mch.m-code
          and est-op.op-pass eq job-mch.pass
          and est-op.dept    eq job-mch.dept
          and est-op.line    lt 500
        no-lock no-error.

    if ((avail est-op) and est-op.op-sb)           or
       ((not avail est-op) and mach.p-type ne "B") then do:

      if avail est-op THEN run sys/inc/numout.p (recid(est-op), output v-out).
      else v-out = 1.
      v-up = v-up * v-out.
    end.
    else v-up = 1.

    v-on = v-on / v-up.
  end.
           
  v-up-hs = 1.

  if job-mch.dept eq "HS" and
     avail est            and
     mach.therm           and
     mach.p-type eq "S"   then
    run sys/inc/numup.p (est.company, est.est-no, job-mch.frm, output v-up-hs).

 /* Don't create wip
    {touch/pcmchact.i}  /* from {pc/pcmchact.i}  mch-act creatation */
 */
/*==== 
 /* IF v-assembled THEN */
    IF tt-prdd.qty > 0 OR v-runqty > 0 THEN
    FOR EACH reftable
        WHERE reftable.reftable EQ "jc/jc-calc.p"
          AND reftable.company  EQ job-mch.company
          AND reftable.loc      EQ ""
          AND reftable.code     EQ STRING(job-mch.job,"999999999")
          AND reftable.val[12]  EQ job-mch.frm
          AND (reftable.val[13] EQ job-mch.blank-no OR
               job-mch.blank-no EQ 0),
        EACH job-hdr
      WHERE job-hdr.company   EQ cocode
        AND job-hdr.job-no    EQ job-mch.job-no
        AND job-hdr.job-no2   EQ job-mch.job-no2
        AND (job-hdr.frm      EQ job-mch.frm OR v-est-type <> 4)
        AND (job-hdr.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0 OR v-est-type <> 4) ,
        first itemfg
        where itemfg.company    eq cocode
          and itemfg.i-no       eq reftable.code2
          and itemfg.case-count gt 0 NO-LOCK:

      x = 1.
      FOR EACH fg-rctd no-lock BY fg-rctd.r-no DESC:
        LEAVE.
      END.
      if avail fg-rctd then x = fg-rctd.r-no.

      find last fg-rcpth use-index r-no no-lock no-error.
      if avail fg-rcpth and fg-rcpth.r-no GT x then x = fg-rcpth.r-no.

      create fg-rctd.
      assign
       fg-rctd.r-no       = X + 1
       fg-rctd.rct-date   = TODAY /*c-prdd.op-date*/
       fg-rctd.company    = job-hdr.company
       fg-rctd.rita-code  = "R"
       fg-rctd.i-name     = itemfg.i-name
       fg-rctd.i-no       = reftable.code2
       fg-rctd.job-no     = job-hdr.job-no
       fg-rctd.job-no2    = job-hdr.job-no2.
                 
      assign
       v-up  = 1
       v-out = 1.
      
      if avail est and mach.p-type ne "B" then do:
        run sys/inc/numup.p (est.company, est.est-no, job-mch.frm, output v-up).
                 
        find first est-op
            where est-op.company eq est.company
              and est-op.est-no  eq est.est-no
              and est-op.s-num   eq job-hdr.frm
              and (est-op.b-num  eq job-hdr.blank-no or
                   job-hdr.blank-no eq 0)
              and est-op.m-code  eq job-mch.m-code
              and est-op.op-pass eq job-mch.pass
              and est-op.dept    eq job-mch.dept
              and est-op.line    lt 500
            no-lock no-error.
        if avail est-op and est-op.n-out ne 0 then v-out = est-op.n-out.
      end.

      ASSIGN
       fg-rctd.b-num      = reftable.val[13]
       fg-rctd.s-num      = reftable.val[12]
       fg-rctd.t-qty      = (IF tt-prdd.qty = 0 THEN v-runqty ELSE tt-prdd.qty)
                                 / v-up-hs * v-out * v-up  /*v-runqty*/
       fg-rctd.pur-uom    = itemfg.prod-uom
       fg-rctd.cost-uom   = itemfg.prod-uom
       fg-rctd.std-cost   = reftable.val[5]
       fg-rctd.ext-cost   = (fg-rctd.t-qty / 1000) * fg-rctd.std-cost
       fg-rctd.qty-case   = itemfg.case-count
       fg-rctd.partial    = fg-rctd.t-qty modulo itemfg.case-count
       fg-rctd.cases      = trunc(fg-rctd.t-qty / itemfg.case-count,0)
       fg-rctd.cases-unit = 1.

      if fg-rctd.t-qty le 0 then fg-rctd.cases = 0.

      release fg-bin.
      
      FIND FIRST b-reftable
          WHERE b-reftable.reftable EQ "ts/jobdata.p"
            AND b-reftable.company  EQ cocode
            AND b-reftable.code     EQ STRING(RECID(job-hdr))
          NO-LOCK NO-ERROR.

      IF AVAIL b-reftable THEN 
      FIND FIRST fg-bin WHERE RECID(fg-bin) EQ INT(b-reftable.code2) NO-LOCK NO-ERROR.
      
      IF AVAIL fg-bin THEN
        ASSIGN
         v-loc       = fg-bin.loc
         v-loc-bin   = fg-bin.loc-bin
         fg-rctd.tag = fg-bin.tag.
                
      ELSE
        RUN fg/autopost.p (ROWID(itemfg), fg-rctd.job-no, fg-rctd.job-no2,
                           OUTPUT v-loc, OUTPUT v-loc-bin).

      ASSIGN
       fg-rctd.loc     = v-loc
       fg-rctd.loc-bin = v-loc-bin.

      FIND FIRST fg-bin
          WHERE fg-bin.company EQ fg-rctd.company
            AND fg-bin.i-no    EQ fg-rctd.i-no
            AND fg-bin.job-no  EQ job-hdr.job-no
            AND fg-bin.job-no2 EQ job-hdr.job-no2
            AND fg-bin.loc     EQ fg-rctd.loc
            AND fg-bin.loc-bin EQ fg-rctd.loc-bin
            AND fg-bin.tag     EQ fg-rctd.tag
          NO-LOCK NO-ERROR.

      IF AVAIL fg-bin AND fg-bin.cases-unit <> 0 THEN fg-rctd.cases-unit = fg-bin.cases-unit.

      RUN fg/comprcpt.p (ROWID(fg-rctd)).
    END. /* v-assembled */
/*====
    ELSE DO:
       FOR EACH job-hdr WHERE job-hdr.company   EQ cocode
          AND job-hdr.job-no    EQ job-mch.job-no
          AND job-hdr.job-no2   EQ job-mch.job-no2         
          AND (job-hdr.frm      EQ job-mch.frm OR v-est-type       EQ 2 )
          AND (job-hdr.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0 ) :

          FIND first itemfg where itemfg.company    eq cocode
                        and itemfg.i-no       eq job-hdr.i-no
                        and itemfg.case-count gt 0
                        NO-LOCK NO-ERROR.
          IF NOT AVAIL itemfg THEN NEXT.
          {addon/touch/jobrcpt.i}
===    END.
  END.  /* for each job-hdr */
===    */
/* end of fg receipt creation */
===*/

  RELEASE fg-rctd.
  RELEASE job.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-set-cmplt B-table-Win 
PROCEDURE proc-set-cmplt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* from pc/pcprdd3u.p pcprdd4u.p */
   DEF VAR v-est-type           LIKE est.est-type NO-UNDO.
   DEF VAR v-loc                LIKE fg-bin.loc NO-UNDO.
   DEF VAR v-loc-bin            LIKE fg-bin.loc-bin NO-UNDO.
   DEF VAR v-qty                AS   INT NO-UNDO.
   DEF VAR choice               AS   LOG NO-UNDO.
   DEF VAR v-assembled AS   LOG NO-UNDO.
   DEF VAR v-runqty AS INT NO-UNDO.
   DEF VAR X AS INT NO-UNDO.
   DEF VAR v-up AS INT NO-UNDO.
   DEF VAR v-out AS INT NO-UNDO.
   def var v-up-hs     like eb.num-up NO-UNDO.
   def var v-on        like eb.num-up NO-UNDO.
   DEF VAR h_updbin AS HANDLE NO-UNDO.
   DEF VAR li-units AS INT NO-UNDO.
   
   DEF BUFFER b-reftable FOR reftable.

   FIND FIRST job WHERE job.company EQ cocode
        AND job.job-no  EQ tt-prdd.job-no
        AND job.job-no2 EQ tt-prdd.job-no2
      USE-INDEX job-no NO-LOCK NO-ERROR.

  FIND FIRST est
      WHERE est.company EQ job.company
        AND est.est-no  EQ job.est-no
      NO-LOCK NO-ERROR.
  v-est-type = IF AVAIL est THEN est.est-type ELSE 1.

  IF v-est-type GT 4 THEN v-est-type = v-est-type - 4.

  v-assembled = NO.

 /* IF v-assembled THEN do for both assembled or unassembled */
/* FOR EACH reftable
      WHERE reftable.reftable EQ "jc/jc-calc.p"
        AND reftable.company  EQ job-mch.company
        AND reftable.loc      EQ ""
        AND reftable.code     EQ STRING(job-mch.job,"999999999")
        AND reftable.val[12]  EQ job-mch.frm
        AND (reftable.val[13] EQ job-mch.blank-no OR
             job-mch.blank-no EQ 0),
    */         

  FOR EACH job-hdr
      WHERE job-hdr.company   EQ cocode
        AND job-hdr.job-no    EQ job-mch.job-no
        AND job-hdr.job-no2   EQ job-mch.job-no2
        AND (job-hdr.frm      EQ job-mch.frm OR v-est-type       EQ 2 )
        AND (job-hdr.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0 ),

      FIRST itemfg
      WHERE itemfg.company EQ job-hdr.company
        AND itemfg.i-no    EQ job-hdr.i-no NO-LOCK:

    /*IF itemfg.isaset AND itemfg.alloc NE YES THEN DO:
      ASSIGN
       v-set  = itemfg.i-no
       v-qty  = pc-prdd.qty.
            
      RUN fg/checkset.p (RECID(itemfg), ?, INPUT-OUTPUT v-qty).
          
      IF v-qty LT pc-prdd.qty THEN DO:
        choice = NO.
        MESSAGE "Insufficient components for AUTOPOST, process anyway?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE choice.
        IF NOT choice THEN RETURN ERROR.
      END.
    END.*/    

    RUN fg/autopost.p (ROWID(itemfg), job-hdr.job-no, job-hdr.job-no2,
                       OUTPUT v-loc, OUTPUT v-loc-bin).

    FIND FIRST fg-bin
        WHERE fg-bin.company EQ itemfg.company
          AND fg-bin.i-no    EQ itemfg.i-no
          AND fg-bin.loc     EQ v-loc
          AND fg-bin.loc-bin EQ v-loc-bin  
          AND fg-bin.tag     EQ ""
          AND fg-bin.job-no  EQ job-hdr.job-no
          AND fg-bin.job-no2 EQ job-hdr.job-no2
        NO-ERROR.
    IF NOT AVAIL fg-bin THEN DO:
      CREATE fg-bin.
      ASSIGN
       fg-bin.company      = itemfg.company
       fg-bin.loc          = v-loc
       fg-bin.loc-bin      = v-loc-bin
       fg-bin.i-no         = job-hdr.i-no 
       fg-bin.tag          = ""
       fg-bin.job-no       = job-hdr.job-no
       fg-bin.job-no2      = job-hdr.job-no2
       fg-bin.std-mat-cost = job-hdr.std-mat-cost
       fg-bin.std-lab-cost = job-hdr.std-lab-cost
       fg-bin.std-fix-cost = job-hdr.std-fix-cost
       fg-bin.std-var-cost = job-hdr.std-var-cost
       fg-bin.std-tot-cost = job-hdr.std-tot-cost
       fg-bin.last-cost    = job-hdr.std-tot-cost
       fg-bin.unit-count   = itemfg.case-count.
    END.
      
    IF fg-bin.cases-unit   LE 0 THEN fg-bin.cases-unit   = 1.
    IF fg-bin.units-pallet LE 0 THEN fg-bin.units-pallet = 1.
    
    FIND FIRST b-reftable
        WHERE b-reftable.reftable EQ "ts/jobdata.p"
          AND b-reftable.company  EQ cocode
          AND b-reftable.code     EQ job-hdr.rec_key
        EXCLUSIVE NO-ERROR.
    IF AVAIL b-reftable THEN DELETE b-reftable.
    CREATE b-reftable.
    ASSIGN
     b-reftable.reftable = "ts/jobdata.p"
     b-reftable.company  = cocode
     b-reftable.code     = job-hdr.rec_key
     b-reftable.code2    = fg-bin.rec_key
     li-units            = b-reftable.val[1]
     v-runqty = 0.

    FOR EACH bf-prdd FIELDS(qty) WHERE bf-prdd.company = tt-prdd.company 
                       AND bf-prdd.m-code = tt-prdd.m-code
                       AND bf-prdd.job-no = tt-prdd.job-no
                       AND bf-prdd.job-no2 = tt-prdd.job-no2
                       AND bf-prdd.FRM = tt-prdd.frm
                       AND bf-prdd.blank-no = tt-prdd.blank-no
                       AND bf-prdd.pass = tt-prdd.pass
                       NO-LOCK:
        v-runqty = v-runqty + bf-prdd.qty.
    END.                                      /*employee_code*/
    /*RUN addon/touch/d-updbin.w  (ROWID(fg-bin), v-runqty,'',cocode). /* pc-prdd.qty*/*/
    RUN pc/d-updbin.w  (ROWID(fg-bin), v-runqty, INPUT-OUTPUT li-units). /* pc-prdd.qty*/

    FIND CURRENT fg-bin NO-LOCK.

    ASSIGN
     b-reftable.val[1] = li-units
     b-reftable.val[2] = fg-bin.case-count
     b-reftable.val[3] = fg-bin.cases-unit.
  END.  /* v-assembled */

/* === NEED more code later
  ELSE DO:     /* for unassembled sets
       THIS CODE WILL POST BOTH COMPONENTS AND SETS ON EVERY FORM, WHICH IS A BUG. 
       ADDITIONAL CODE MUST BE WRITTEN TO ONLY POST ON LAST OPERATION OF LAST FORM 
               */     
        {addon/touch/jobbin.i}
  END.
===*/

  /*=========== create fg receipt : from pc/r-wippst.w */
  /*FOR EACH bf-machtran WHERE bf-machtran.company = cocode AND
                                      bf-machtran.machine = machine_code AND
                                      bf-machtran.job_number = job_number AND
                                      bf-machtran.job_sub = INTEGER(job_sub) AND
                                      bf-machtran.form_number = INTEGER(form_number) AND
                                      bf-machtran.blank_number = INTEGER(blank_number) AND
                                      bf-machtran.pass_sequence = INTEGER(pass_sequence) NO-LOCK:
                                      */
    FIND FIRST job WHERE job.company EQ cocode
                        AND job.job-no  EQ tt-prdd.job-no
                        AND job.job-no2 EQ tt-prdd.job-no2
                        USE-INDEX job-no NO-ERROR.
    ASSIGN v-up  = 1
           v-out = 1
           v-on  = 1.

    FIND FIRST est WHERE est.company EQ job.company
                     AND est.est-no  EQ job.est-no
                     NO-LOCK NO-ERROR.
    v-est-type = IF AVAIL est THEN est.est-type ELSE 1.
    IF v-est-type GT 4 THEN v-est-type = v-est-type - 4.

    FIND FIRST mach WHERE mach.company = job-mch.company
                       AND mach.m-code = job-mch.m-code NO-LOCK NO-ERROR.

    IF AVAIL mach THEN
       FOR EACH mach-part WHERE
           mach-part.company EQ mach.company AND
           mach-part.m-code EQ mach.m-code
           EXCLUSIVE-LOCK:
           mach-part.total-impressions-run = mach-part.total-impressions-run
                                           + tt-prdd.qty + tt-prdd.waste.
              
           FIND FIRST reftable WHERE
                reftable.reftable EQ "MACHPARTHOURS" AND
                reftable.company  EQ mach-part.company AND
                reftable.loc      EQ mach-part.m-code AND
                reftable.code     EQ mach-part.rm-part-code
                EXCLUSIVE-LOCK NO-ERROR.

           IF NOT AVAIL reftable THEN DO:
              CREATE reftable.
              ASSIGN
                 reftable.reftable = "MACHPARTHOURS"
                 reftable.company  = mach-part.company
                 reftable.loc      = mach-part.m-code
                 reftable.code     = mach-part.rm-part-code. 
           END.

           reftable.val[1] = reftable.val[1]
                           + tt-prdd.hours.

           RELEASE reftable.
       END.

    IF mach.dept[1] EQ "PR" OR mach.dept[2] EQ "PR" OR
       mach.dept[3] EQ "PR" OR mach.dept[4] EQ "PR" THEN
       RUN update-plate-die ("P", v-est-type).

    IF mach.dept[1] EQ "DC" OR mach.dept[2] EQ "DC" OR
       mach.dept[3] EQ "DC" OR mach.dept[4] EQ "DC" THEN
       RUN update-plate-die ("D", v-est-type).

    if avail est then do:
       run sys/inc/numup.p (est.company, est.est-no, job-mch.frm, output v-up).

    find first ef
        where ef.company eq est.company
          and ef.est-no  eq est.est-no
          and ef.form-no eq job-mch.frm
        no-lock no-error.

    if avail ef then
      v-on = v-up *
             (if ef.n-out   eq 0 then 1 else ef.n-out) *
             (if ef.n-out-l eq 0 then 1 else ef.n-out-l) *
             (if ef.n-out-d eq 0 then 1 else ef.n-out-d).
                      
    find first est-op
        where est-op.company eq est.company
          and est-op.est-no  eq est.est-no
          and est-op.s-num   eq job-mch.frm
          and (est-op.b-num  eq job-mch.blank-no OR job-mch.blank-no eq 0)
          and est-op.m-code  eq job-mch.m-code
          and est-op.op-pass eq job-mch.pass
          and est-op.dept    eq job-mch.dept
          and est-op.line    lt 500
        no-lock no-error.

    if ((avail est-op) and est-op.op-sb)           or
       ((not avail est-op) and mach.p-type ne "B") then do:

      if avail est-op THEN run sys/inc/numout.p (recid(est-op), output v-out).
      else v-out = 1.
      v-up = v-up * v-out.
    end.
    else v-up = 1.

    v-on = v-on / v-up.
  end.
           
  v-up-hs = 1.

  if job-mch.dept eq "HS" and
     avail est            and
     mach.therm           and
     mach.p-type eq "S"   then
    run sys/inc/numup.p (est.company, est.est-no, job-mch.frm, output v-up-hs).

 /* Don't create wip
    {touch/pcmchact.i}  /* from {pc/pcmchact.i}  mch-act creatation */
 */

 /* IF v-assembled THEN */
    IF tt-prdd.qty > 0 OR v-runqty > 0 THEN
    /*FOR EACH reftable
        WHERE reftable.reftable EQ "jc/jc-calc.p"
          AND reftable.company  EQ job-mch.company
          AND reftable.loc      EQ ""
          AND reftable.code     EQ STRING(job-mch.job,"999999999")
          AND reftable.val[12]  EQ job-mch.frm
          AND (reftable.val[13] EQ job-mch.blank-no OR
               job-mch.blank-no EQ 0), */
     FOR EACH job-hdr
      WHERE job-hdr.company   EQ cocode
        AND job-hdr.job-no    EQ job-mch.job-no
        AND job-hdr.job-no2   EQ job-mch.job-no2
        AND (job-hdr.frm      EQ job-mch.frm OR v-est-type       EQ 2 )
        AND (job-hdr.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0 ) ,
        first itemfg
        where itemfg.company    eq cocode
          and itemfg.i-no       eq job-hdr.i-no
          and itemfg.case-count gt 0 NO-LOCK:

      x = 1.
      FOR EACH fg-rctd no-lock BY fg-rctd.r-no DESC:
        LEAVE.
      END.
      if avail fg-rctd then x = fg-rctd.r-no.

      find last fg-rcpth use-index r-no no-lock no-error.
      if avail fg-rcpth and fg-rcpth.r-no GT x then x = fg-rcpth.r-no.

      create fg-rctd.
      assign
       fg-rctd.r-no       = X + 1
       fg-rctd.rct-date   = TODAY /*c-prdd.op-date*/
       fg-rctd.trans-time = TIME
       fg-rctd.company    = job-hdr.company
       fg-rctd.rita-code  = "R"
       fg-rctd.i-name     = itemfg.i-name
       fg-rctd.i-no       = job-hdr.i-no
       fg-rctd.job-no     = job-hdr.job-no
       fg-rctd.job-no2    = job-hdr.job-no2.
                 
      assign
       v-up  = 1
       v-out = 1.
      
      if avail est and mach.p-type ne "B" then do:
        run sys/inc/numup.p (est.company, est.est-no, job-mch.frm, output v-up).
                 
        find first est-op
            where est-op.company eq est.company
              and est-op.est-no  eq est.est-no
              and est-op.s-num   eq job-hdr.frm
              and (est-op.b-num  eq job-hdr.blank-no or
                   job-hdr.blank-no eq 0)
              and est-op.m-code  eq job-mch.m-code
              and est-op.op-pass eq job-mch.pass
              and est-op.dept    eq job-mch.dept
              and est-op.line    lt 500
            no-lock no-error.
        if avail est-op and est-op.n-out ne 0 then v-out = est-op.n-out.
      end.

      ASSIGN
       fg-rctd.b-num      = job-mch.blank-no
       fg-rctd.s-num      = job-mch.frm
       fg-rctd.t-qty      = (IF tt-prdd.qty = 0 THEN v-runqty ELSE tt-prdd.qty) 
                               / v-up-hs * v-out * v-up  /*v-runqty*/
       fg-rctd.pur-uom    = itemfg.prod-uom
       fg-rctd.cost-uom   = itemfg.prod-uom
       fg-rctd.std-cost   = job-hdr.std-tot-cost
       fg-rctd.ext-cost   = (fg-rctd.t-qty / 1000) * fg-rctd.std-cost
       fg-rctd.qty-case   = itemfg.case-count
       fg-rctd.partial    = fg-rctd.t-qty modulo itemfg.case-count
       fg-rctd.cases      = trunc(fg-rctd.t-qty / itemfg.case-count,0)
       fg-rctd.cases-unit = 1.

      if fg-rctd.t-qty le 0 then fg-rctd.cases = 0.

      release fg-bin.
      
      FIND FIRST b-reftable
          WHERE b-reftable.reftable EQ "ts/jobdata.p"
            AND b-reftable.company  EQ cocode
            AND b-reftable.code     EQ job-hdr.rec_key
          NO-LOCK NO-ERROR.

      IF AVAIL b-reftable THEN 
      FIND FIRST fg-bin WHERE fg-bin.rec_key EQ b-reftable.code2 NO-LOCK NO-ERROR.
      
      IF AVAIL fg-bin THEN
        ASSIGN
         v-loc       = fg-bin.loc
         v-loc-bin   = fg-bin.loc-bin
         fg-rctd.tag = fg-bin.tag.
                
      ELSE
        RUN fg/autopost.p (ROWID(itemfg), fg-rctd.job-no, fg-rctd.job-no2,
                           OUTPUT v-loc, OUTPUT v-loc-bin).

      ASSIGN
       fg-rctd.loc     = v-loc
       fg-rctd.loc-bin = v-loc-bin.

      FIND FIRST fg-bin
          WHERE fg-bin.company EQ fg-rctd.company
            AND fg-bin.i-no    EQ fg-rctd.i-no
            AND fg-bin.job-no  EQ job-hdr.job-no
            AND fg-bin.job-no2 EQ job-hdr.job-no2
            AND fg-bin.loc     EQ fg-rctd.loc
            AND fg-bin.loc-bin EQ fg-rctd.loc-bin
            AND fg-bin.tag     EQ fg-rctd.tag
          NO-LOCK NO-ERROR.

      IF AVAIL fg-bin AND fg-bin.cases-unit <> 0 THEN fg-rctd.cases-unit = fg-bin.cases-unit.

      RUN fg/comprcpt.p (ROWID(fg-rctd)).
    END. /* v-assembled */
/*====
    ELSE DO:
       FOR EACH job-hdr WHERE job-hdr.company   EQ cocode
          AND job-hdr.job-no    EQ job-mch.job-no
          AND job-hdr.job-no2   EQ job-mch.job-no2         
          AND (job-hdr.frm      EQ job-mch.frm OR v-est-type       EQ 2 )
          AND (job-hdr.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0 ) :

          FIND first itemfg where itemfg.company    eq cocode
                        and itemfg.i-no       eq job-hdr.i-no
                        and itemfg.case-count gt 0
                        NO-LOCK NO-ERROR.
          IF NOT AVAIL itemfg THEN NEXT.
          {addon/touch/jobrcpt.i}
===    END.
  END.  /* for each job-hdr */
===    */

   /* end of fg receipt creation */
 
  RELEASE fg-rctd.
  RELEASE job.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query B-table-Win 
PROCEDURE repo-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

  DEF BUFFER b-tt-prdd FOR tt-prdd.

  FOR EACH b-tt-prdd WHERE ROWID(b-tt-prdd) EQ ip-rowid,
      EACH bf-prdd WHERE ROWID(bf-prdd) EQ b-tt-prdd.row-id NO-LOCK:
    ip-rowid = ROWID(bf-prdd).
    LEAVE.
  END.

  RUN dispatch ('open-query').

  FOR EACH bf-prdd WHERE ROWID(bf-prdd) EQ ip-rowid NO-LOCK,
      EACH b-tt-prdd WHERE b-tt-prdd.row-id EQ ROWID(bf-prdd):
    ip-rowid = ROWID(b-tt-prdd).
    LEAVE.
  END.

  REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
  
  RUN dispatch ('row-changed').
  
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
  {src/adm/template/snd-list.i "pc-prdd"}
  {src/adm/template/snd-list.i "tt-prdd"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-crew B-table-Win 
PROCEDURE show-crew :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-type LIKE job-code.cat NO-UNDO.

  DEF VAR lv-crew LIKE tt-prdd.crew NO-UNDO.
  DEF VAR lv-frm LIKE tt-prdd.frm INIT 1 NO-UNDO.
  DEF VAR lv-blk LIKE tt-prdd.blank-no INIT 0 NO-UNDO.


  IF ip-type EQ "MR" THEN ip-type = "M R".

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST job
        WHERE job.company EQ tt-prdd.company
          AND job.job-no  EQ tt-prdd.job-no
          AND job.job-no2 EQ tt-prdd.job-no2
        NO-LOCK NO-ERROR.

    RELEASE mach.
    IF AVAIL job THEN
    FIND FIRST mach
        WHERE mach.company EQ tt-prdd.company
          AND mach.m-code  EQ tt-prdd.m-code:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.

    IF AVAIL mach THEN DO:
      ASSIGN
       lv-crew = IF ip-type EQ "RUN" THEN mach.run-crusiz ELSE mach.mr-crusiz
       lv-frm  = INT(tt-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name})
       lv-blk  = INT(tt-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}).

      RELEASE eb.
      IF TRIM(job.est-no) NE "" THEN
      FIND FIRST eb
          WHERE eb.company   EQ job.company
            AND eb.est-no    EQ job.est-no
            AND eb.form-no   EQ lv-frm
            AND (eb.blank-no EQ lv-blk OR lv-blk EQ 0)
          NO-LOCK NO-ERROR.

      IF AVAIL eb THEN
        RUN est/getcrusz.p (ROWID(mach), ROWID(eb), mach.dept[1], ip-type,
                            INPUT-OUTPUT lv-crew).
    END.

    tt-prdd.crew:SCREEN-VALUE IN BROWSE {&browse-name} =
                                 STRING(IF lv-crew EQ 0 THEN 1 ELSE lv-crew).
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE undo-added B-table-Win 
PROCEDURE undo-added :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cRecId AS CHARACTER NO-UNDO .

  FOR EACH _Lock: 
   cRecId = cRecId + string(_Lock._Lock-RECID) + "," .
  END.

  IF adm-new-record THEN RUN dispatch ("cancel-record").

  FOR EACH bf-prdd WHERE bf-prdd.company = pc-prdd.company 
                   AND bf-prdd.job-no = pc-prdd.job-no
                   AND bf-prdd.job-no2 = pc-prdd.job-no2
                   AND bf-prdd.op-date = pc-prdd.op-date
                   AND bf-prdd.shift = pc-prdd.shift
                   AND bf-prdd.m-code = "":
      IF LOOKUP(STRING(RECID(bf-prdd)),cRecId ) > 0 THEN LEAVE .
    DELETE bf-prdd.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-plate-die B-table-Win 
PROCEDURE update-plate-die :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-upd-type AS   CHAR NO-UNDO.
  DEF INPUT PARAM ip-est-type LIKE est.est-type NO-UNDO.
  
  IF AVAIL itemfg THEN DO:
      IF ip-upd-type EQ "P" AND itemfg.plate-no NE "" THEN
      FIND FIRST prep
          WHERE prep.company EQ cocode
            AND prep.code    EQ itemfg.plate-no
          NO-ERROR.

      ELSE
      IF ip-upd-type EQ "D" AND itemfg.die-no NE "" THEN
      FIND FIRST prep
          WHERE prep.company EQ cocode
            AND prep.code    EQ itemfg.die-no
          NO-ERROR.

      IF AVAIL prep THEN DO:
        ASSIGN prep.no-of-impressions = prep.no-of-impressions +
                                        tt-prdd.qty +
                                        tt-prdd.waste
               prep.last-date         = tt-prdd.op-date
               prep.last-job-no       = tt-prdd.job-no
               prep.last-job-no2      = tt-prdd.job-no2
               .
        RELEASE prep.
      END.
  END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-blank-no B-table-Win 
PROCEDURE valid-blank-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-changed AS LOG NO-UNDO.

  IF NOT ll-no-blk THEN DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST job-mch
                    WHERE job-mch.company  EQ pc-prdd.company
                      AND job-mch.job      EQ pc-prdd.job
                      AND job-mch.job-no   EQ pc-prdd.job-no
                      AND job-mch.job-no2  EQ pc-prdd.job-no2
                      AND job-mch.frm      EQ INT(tt-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name})
                      AND job-mch.blank-no EQ INT(tt-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}))
    THEN DO:
      IF v-est-type EQ 2 OR v-est-type EQ 6 THEN
      FIND FIRST job
          WHERE job.company EQ pc-prdd.company
            AND job.job     EQ pc-prdd.job
            AND job.job-no  EQ pc-prdd.job-no
            AND job.job-no2 EQ pc-prdd.job-no2
          NO-LOCK NO-ERROR.
      IF NOT AVAIL job OR
         NOT CAN-FIND(FIRST eb
                      WHERE eb.company  EQ pc-prdd.company
                        AND eb.est-no   EQ job.est-no
                        AND eb.form-no  EQ INT(tt-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name})
                        AND eb.blank-no EQ INT(tt-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}))
      THEN DO:
        MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO tt-prdd.blank-no IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.
    END.

    IF ip-changed THEN DO:
      tt-prdd.i-no:SCREEN-VALUE = "".
      RUN new-job-hdr.
    END.
  END.

  IF ll-no-blk OR NOT ip-changed THEN RUN display-item.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-crew B-table-Win 
PROCEDURE valid-crew :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF DEC(tt-prdd.crew:SCREEN-VALUE IN BROWSE {&browse-name}) LE 0 THEN DO:
      MESSAGE TRIM(tt-prdd.crew:LABEL IN BROWSE {&browse-name}) +
              " must be greater than 0..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO tt-prdd.crew IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-frm B-table-Win 
PROCEDURE valid-frm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-changed AS LOG NO-UNDO.

  IF NOT ll-no-frm THEN DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST job-mch
                    WHERE job-mch.company EQ pc-prdd.company
                      AND job-mch.job     EQ pc-prdd.job
                      AND job-mch.job-no  EQ pc-prdd.job-no
                      AND job-mch.job-no2 EQ pc-prdd.job-no2
                      AND job-mch.frm     EQ INT(tt-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name}))
    THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO tt-prdd.frm IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.

    IF ip-changed THEN DO:
      tt-prdd.i-no:SCREEN-VALUE = "".
      RUN new-job-hdr.
    END.
  END.

  IF ll-no-frm OR NOT ip-changed THEN RUN display-item.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no B-table-Win 
PROCEDURE valid-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-changed AS LOG NO-UNDO.


  IF NOT ll-no-blk THEN DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST job-hdr
                    WHERE job-hdr.company  EQ pc-prdd.company
                      AND job-hdr.job      EQ pc-prdd.job
                      AND job-hdr.job-no   EQ pc-prdd.job-no
                      AND job-hdr.job-no2  EQ pc-prdd.job-no2
                      AND (job-hdr.frm     EQ INT(tt-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name}) OR
                           v-est-type EQ 2 OR v-est-type EQ 6)
                      AND job-hdr.i-no     EQ tt-prdd.i-no:SCREEN-VALUE IN BROWSE {&browse-name})
    THEN DO:
      IF v-est-type EQ 2 OR v-est-type EQ 6 THEN
      FIND FIRST job
          WHERE job.company EQ pc-prdd.company
            AND job.job     EQ pc-prdd.job
            AND job.job-no  EQ pc-prdd.job-no
            AND job.job-no2 EQ pc-prdd.job-no2
          NO-LOCK NO-ERROR.
      IF NOT AVAIL job OR
         NOT CAN-FIND(FIRST eb
                      WHERE eb.company  EQ pc-prdd.company
                        AND eb.est-no   EQ job.est-no
                        AND eb.form-no  EQ INT(tt-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name})
                        AND eb.stock-no EQ tt-prdd.i-no:SCREEN-VALUE IN BROWSE {&browse-name})
      THEN DO:
        MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO tt-prdd.i-no IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.
    END.

    IF ip-changed THEN RUN new-job-hdr.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-m-code B-table-Win 
PROCEDURE valid-m-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-msg AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ll-skip = NO.

    FIND FIRST mach NO-LOCK
        WHERE mach.company EQ g_company
          AND mach.m-code  EQ tt-prdd.m-code:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-ERROR.
    IF NOT AVAIL mach THEN lv-msg = "Invalid entry, try help".

    IF lv-msg EQ ""                           AND
      mach.obsolete THEN DO:
      lv-msg = "Machine is Inactive, please enter new machine".
    END.
    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO tt-prdd.m-code IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.

    ll-skip = YES.

    RUN display-item.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-pass B-table-Win 
PROCEDURE valid-pass :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR choice AS LOG NO-UNDO.
  DEF VAR lv-m-code AS CHAR NO-UNDO.
  DEF VAR lv-msg AS CHAR EXTENT 2 NO-UNDO.
  DEF VAR lv-listdept AS CHAR NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lj AS INT NO-UNDO.

  DEF BUFFER b-tt-job-mch FOR tt-job-mch.

           
  DO WITH FRAME {&FRAME-NAME}:
    lv-m-code = tt-prdd.m-code:SCREEN-VALUE IN BROWSE {&browse-name}.

    IF lv-dept EQ "" THEN DO:
      lv-listdept = lv-listdept + TRIM(lv-depts[1]) + ",".

      DO li = 2 TO EXTENT(lv-depts):
        IF lv-depts[li] NE ""          AND
           CAN-FIND(FIRST job-mch
                    WHERE job-mch.company EQ tt-prdd.company
                      AND job-mch.job     EQ tt-prdd.job
                      AND job-mch.job-no  EQ tt-prdd.job-no
                      AND job-mch.job-no2 EQ tt-prdd.job-no2
                      AND job-mch.frm     EQ INT(tt-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name})
                      AND (job-mch.blank-no EQ INT(tt-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) or
                           ll-no-blk)
                      AND job-mch.dept    EQ lv-depts[li]
                      AND job-mch.pass    EQ INT(tt-prdd.pass:SCREEN-VALUE IN BROWSE {&browse-name})
                    USE-INDEX seq-idx) THEN
          lv-listdept = lv-listdept + TRIM(lv-depts[li]) + ",".
      END.
      RUN list-edit (INPUT-OUTPUT lv-listdept).

      lv-dept = TRIM(ENTRY(1,lv-listdept)).

      IF NUM-ENTRIES(lv-listdept) GT 1 THEN update-dept: DO WHILE TRUE.
        MESSAGE "Which department? (" + TRIM(lv-listdept) + "):" UPDATE lv-dept.
        lv-dept = CAPS(lv-dept).
        IF lv-dept EQ ""                    OR
           LOOKUP(lv-dept,lv-listdept) GT 0 THEN LEAVE.
      END.

      IF lv-dept EQ "" THEN DO:
        APPLY "entry" TO tt-prdd.pass.
        RETURN ERROR.
      END.
    END.

    FIND FIRST tt-job-mch
        WHERE tt-job-mch.company EQ tt-prdd.company
          AND tt-job-mch.job     EQ tt-prdd.job
          AND tt-job-mch.job-no  EQ tt-prdd.job-no
          AND tt-job-mch.job-no2 EQ tt-prdd.job-no2
          AND tt-job-mch.m-code  EQ lv-m-code
          AND tt-job-mch.frm     EQ INT(tt-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name})
          AND (tt-job-mch.blank-no EQ INT(tt-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) or
               ll-no-blk)
          AND tt-job-mch.dept    EQ lv-dept
          AND tt-job-mch.pass    EQ INT(tt-prdd.pass:SCREEN-VALUE IN BROWSE {&browse-name})
        USE-INDEX seq-idx NO-LOCK NO-ERROR.

    IF AVAIL tt-job-mch THEN DO:
        FOR EACH b-tt-job-mch WHERE ROWID(b-tt-job-mch) NE ROWID(tt-job-mch) AND b-tt-job-mch.xtra-copy = NO:
          DELETE b-tt-job-mch.
        END.
    END.

    ELSE DO:
      FOR EACH job-mch
          WHERE job-mch.company EQ tt-prdd.company
            AND job-mch.job     EQ tt-prdd.job
            AND job-mch.job-no  EQ tt-prdd.job-no
            AND job-mch.job-no2 EQ tt-prdd.job-no2
            AND job-mch.frm     EQ INT(tt-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name})
            AND (job-mch.blank-no EQ INT(tt-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) or
                 ll-no-blk)
            AND job-mch.dept    EQ lv-dept
            AND job-mch.pass    EQ INT(tt-prdd.pass:SCREEN-VALUE IN BROWSE {&browse-name})
          USE-INDEX seq-idx NO-LOCK,
          FIRST mach
          WHERE mach.company    EQ job-mch.company
            AND mach.m-code     EQ job-mch.m-code
            AND (job-mch.m-code EQ lv-m-code OR
                 mach.sch-m-code EQ lv-schmch)
          NO-LOCK
          BY INT(job-mch.m-code EQ lv-m-code) DESC:
        LEAVE.
      END.

      IF AVAIL job-mch THEN
        tt-prdd.m-code:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = job-mch.m-code NO-ERROR.

      ELSE DO:
        choice = NO.

        FIND FIRST job-mch
            WHERE job-mch.company EQ tt-prdd.company
              AND job-mch.job     EQ tt-prdd.job
              AND job-mch.job-no  EQ tt-prdd.job-no
              AND job-mch.job-no2 EQ tt-prdd.job-no2
              AND job-mch.frm     EQ INT(tt-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name})
              AND (job-mch.blank-no EQ INT(tt-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) or
                   ll-no-blk)
              AND job-mch.dept    EQ lv-dept
              AND job-mch.pass    EQ INT(tt-prdd.pass:SCREEN-VALUE IN BROWSE {&browse-name})
            USE-INDEX seq-idx NO-LOCK NO-ERROR.

        IF NOT AVAIL job-mch THEN
        /* search without using dept */
        FIND FIRST job-mch
            WHERE job-mch.company EQ tt-prdd.company
              AND job-mch.job     EQ tt-prdd.job
              AND job-mch.job-no  EQ tt-prdd.job-no
              AND job-mch.job-no2 EQ tt-prdd.job-no2
              AND job-mch.m-code  EQ lv-m-code
              AND job-mch.frm     EQ INT(tt-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name})
              AND (job-mch.blank-no EQ INT(tt-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name}) or
                   ll-no-blk)
              AND job-mch.pass    EQ INT(tt-prdd.pass:SCREEN-VALUE IN BROWSE {&browse-name})
            USE-INDEX seq-idx NO-LOCK NO-ERROR.
    
        IF AVAIL job-mch THEN DO:
          IF actual-entered(job-mch.m-code, job-mch.job) = NO AND job-mch.run-hr = 0 then
          ASSIGN
           lv-msg[1] = "Machine " +
                       TRIM(lv-m-code) +
                       " is not defined in job standards for this job/form/blank/pass."
           lv-msg[2] = "Would you like to replace machine " +
                       TRIM(job-mch.m-code) +
                       " with machine " +
                       TRIM(lv-m-code) + "?".
          ELSE 
            ASSIGN
           lv-msg[1] = "Machine " +
                       TRIM(lv-m-code) +
                       " is not defined in job standards for this job/form/blank/pass."
           lv-msg[2] = "Would you like to copy machine " +
                       TRIM(job-mch.m-code) +
                       " to machine " +
                       TRIM(lv-m-code) + "?".
        END.
        ELSE
          ASSIGN
           lv-msg[1] = "ERROR: This dept is not valid for this job/form/blank/pass."
           lv-msg[2] = "Would you like to add the department to job standards?".

        MESSAGE lv-msg[1] SKIP lv-msg[2]
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice.
        DEF VAR v-save-n-out LIKE job-mch.n-out.
        DEF VAR v-save-original-mach LIKE job-mch.m-code NO-UNDO.
        IF AVAIL(job-mch) THEN
          v-save-original-mach = job-mch.m-code.

        IF choice THEN DO:
          CREATE tt-job-mch.
          
          IF AVAIL job-mch THEN
            BUFFER-COPY job-mch TO tt-job-mch
            ASSIGN
             tt-job-mch.row-id = ROWID(job-mch)
             tt-job-mch.m-code = lv-m-code.

          ELSE
            ASSIGN
             tt-job-mch.row-id   = ?
             tt-job-mch.company  = tt-prdd.company
             tt-job-mch.job      = tt-prdd.job
             tt-job-mch.job-no   = tt-prdd.job-no
             tt-job-mch.job-no2  = tt-prdd.job-no2
             tt-job-mch.frm      = INT(tt-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name})
             tt-job-mch.blank-no = INT(tt-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name})
             tt-job-mch.pass     = INT(tt-prdd.pass:SCREEN-VALUE IN BROWSE {&browse-name})
             tt-job-mch.m-code   = lv-m-code
             tt-job-mch.dept     = lv-dept
             tt-job-mch.n-out    = 0
             tt-job-mch.n-on     = 0.

          /* 08281203 If has mr hr, do not replace, but copy and zero out mr-hr */
          IF AVAIL(job-mch) AND actual-entered(job-mch.m-code, job-mch.job) = YES THEN
              tt-job-mch.mr-hr = 0.

          IF tt-job-mch.n-out EQ 0 THEN tt-job-mch.n-out = 1.

          IF tt-job-mch.n-on  EQ 0 THEN RUN get-num-on.

          IF CAN-DO("CR,RC,GU",lv-dept) THEN DO:
            tt-job-mch.n-on = tt-job-mch.n-on / tt-job-mch.n-out.

            MESSAGE "Please enter #out for this pass?"
                UPDATE tt-job-mch.n-out.
            v-save-n-out = tt-job-mch.n-out.
            tt-job-mch.n-on = tt-job-mch.n-on * tt-job-mch.n-out.
          END.
          
          IF avail(job-mch) AND actual-entered(job-mch.m-code, job-mch.job) = YES THEN DO:
            /* task 08281203 - create 2nd record to copy instead of replace */
            CREATE tt-job-mch.
            
            IF AVAIL job-mch THEN
              BUFFER-COPY job-mch TO tt-job-mch
              ASSIGN
               /* tt-job-mch.row-id = ROWID(job-mch) */
               tt-job-mch.m-code = v-save-original-mach /* job-mch.m-code */
               tt-job-mch.row-id = ?
               tt-job-mch.xtra-copy = TRUE.
          
            ELSE
              ASSIGN
               tt-job-mch.row-id   = ?
               tt-job-mch.company  = pc-prdd.company
               tt-job-mch.job      = li-help-job
               tt-job-mch.job-no   = tt-prdd.job-no
               tt-job-mch.job-no2  = tt-prdd.job-no2
               tt-job-mch.frm      = INT(tt-prdd.frm:SCREEN-VALUE IN BROWSE {&browse-name})
               tt-job-mch.blank-no = INT(tt-prdd.blank-no:SCREEN-VALUE IN BROWSE {&browse-name})
               tt-job-mch.pass     =  INT(tt-prdd.pass:SCREEN-VALUE IN BROWSE {&browse-name})
               tt-job-mch.m-code   = pc-prdd.m-code
               tt-job-mch.dept     = pc-prdd.dept
               tt-job-mch.n-out    = 0
               tt-job-mch.n-on     = 0.
          
            IF tt-job-mch.n-out EQ 0 THEN tt-job-mch.n-out = 1.
          
            IF tt-job-mch.n-on  EQ 0 THEN RUN get-num-on.
          
            IF CAN-DO("CR,RC,GU",pc-prdd.dept) THEN DO:
              tt-job-mch.n-on = tt-job-mch.n-on / tt-job-mch.n-out.
          
  /*            MESSAGE "Please enter #out for this pass?"
                  UPDATE tt-job-mch.n-out. */

              tt-job-mch.n-out = v-save-n-out.
          
              tt-job-mch.n-on = tt-job-mch.n-on * tt-job-mch.n-out.
            
            END. 
          END.                          
        END.

        ELSE DO:
          APPLY "entry" TO tt-prdd.pass.
          RETURN ERROR.
        END.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION actual-entered B-table-Win 
FUNCTION actual-entered RETURNS LOGICAL
  ( INPUT ip-m-code AS CHAR, INPUT ip-job AS INT) :

DEF VAR v-qty AS DEC NO-UNDO.

v-qty = 0.
for each mch-act where mch-act.company = cocode and
                       mch-act.job = ip-job AND
                       mch-act.m-code = ip-m-code
                       no-lock:
  v-qty = v-qty + mch-act.hours.
END.

IF v-qty GT 0 THEN
  RETURN TRUE.
ELSE
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-time B-table-Win 
FUNCTION display-time RETURNS CHARACTER
  ( INPUT ip-time AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  RETURN STRING(ip-time,"HH:MM") .   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

