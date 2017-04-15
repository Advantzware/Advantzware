&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: jcinq\b-updmac.w

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
DEF INPUT PARAM ip-job-no AS CHAR NO-UNDO.
DEF INPUT PARAM ip-job-no2 AS INT NO-UNDO.
DEF INPUT PARAM ip-sheet-num AS INT NO-UNDO.
DEF INPUT PARAM ip-blank-num AS INT NO-UNDO.
DEF INPUT PARAM ip-mach-code AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE tt-mch-tran NO-UNDO
  FIELD tran-type AS CHAR
  FIELD tran-date AS DATE
  FIELD job-no AS CHAR
  FIELD job-no2 AS INT
  FIELD machine-code AS CHAR
  FIELD sheet-no AS INT
  FIELD blank-no AS INT
  FIELD pass LIKE mch-act.pass
  FIELD i-no AS CHAR
  FIELD charge-code AS CHAR
  FIELD start-time AS CHAR
  FIELD start-am AS CHAR
  FIELD end-time AS CHAR
  FIELD end-am AS CHAR
  FIELD total-hours AS DEC
  FIELD qty-posted AS DEC
  FIELD qty-waste AS DEC
  FIELD complete AS LOG FORMAT "Y/N"
  FIELD dl-rate AS DEC
  FIELD fixoh AS DEC
  FIELD varoh AS DEC
  FIELD ROWID AS ROWID
  FIELD crew AS INT
  FIELD dept AS CHAR
  INDEX tt-mch-tran-idx IS PRIMARY tran-date ASC.

DEF VAR op-valid AS LOG NO-UNDO.

{custom/globdefs.i}
{sys/inc/VAR.i "new shared" }
ASSIGN cocode = g_company
       locode = g_loc.


DEF VAR v-jcmchupd-sec AS CHAR NO-UNDO.
DEF VAR l-jcmchupd-sec AS LOG NO-UNDO.
DEF VAR v-access-close AS LOG NO-UNDO.
DEF VAR v-access-list AS CHAR NO-UNDO.
RUN methods/prgsecur.p
    (INPUT "jcMchUpd",
     INPUT "ALL", /* based on run, create, update, delete or all */
     INPUT NO,    /* use the directory in addition to the program */
     INPUT NO,    /* Show a message if not authorized */
     INPUT NO,    /* Group overrides user security? */
     OUTPUT v-jcmchupd-sec, /* Allowed? Yes/NO */
     OUTPUT v-access-close, /* used in template/windows.i  */
     OUTPUT v-access-list). /* list 1's and 0's indicating yes or no to run, create, update, delete */
l-jcmchupd-sec = IF v-jcmchupd-sec = "YES" THEN YES ELSE NO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-mch-tran

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt-mch-tran.tran-type tt-mch-tran.tran-date tt-mch-tran.job-no tt-mch-tran.job-no2 tt-mch-tran.machine-code tt-mch-tran.dept tt-mch-tran.sheet-no tt-mch-tran.blank-no tt-mch-tran.pass tt-mch-tran.i-no tt-mch-tran.charge-code tt-mch-tran.start-time tt-mch-tran.start-am tt-mch-tran.end-time tt-mch-tran.end-am tt-mch-tran.total-hours tt-mch-tran.qty-posted tt-mch-tran.qty-waste tt-mch-tran.complete tt-mch-tran.dl-rate tt-mch-tran.varoh tt-mch-tran.fixoh tt-mch-tran.crew   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 tt-mch-tran.tran-date ~
  tt-mch-tran.machine-code ~
  tt-mch-tran.dept ~
  tt-mch-tran.sheet-no ~
  tt-mch-tran.blank-no ~
  tt-mch-tran.pass ~
  tt-mch-tran.i-no ~
  tt-mch-tran.charge-code ~
  tt-mch-tran.start-time ~
  tt-mch-tran.start-am ~
  tt-mch-tran.end-time ~
  tt-mch-tran.end-am ~
  tt-mch-tran.total-hours ~
  tt-mch-tran.qty-posted ~
  tt-mch-tran.qty-waste ~
  tt-mch-tran.COMPLETE ~
  tt-mch-tran.varoh ~
  tt-mch-tran.fixoh ~
  tt-mch-tran.crew   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-2 tt-mch-tran
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-2 tt-mch-tran
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tt-mch-tran
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tt-mch-tran.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt-mch-tran
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt-mch-tran


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tt-mch-tran SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 D-Dialog _FREEFORM
  QUERY BROWSE-2 DISPLAY
      tt-mch-tran.tran-type     COLUMN-LABEL "Type"
   tt-mch-tran.tran-date        COLUMN-LABEL "Date" FORMAT "99/99/99"
   tt-mch-tran.job-no           COLUMN-LABEL "Job #" FORMAT "X(6)" WIDTH 8.5
   tt-mch-tran.job-no2          COLUMN-LABEL "" FORMAT ">9" WIDTH 2.8
   tt-mch-tran.machine-code     COLUMN-LABEL "Machine" FORMAT "X(15)" WIDTH 18
   tt-mch-tran.dept             COLUMN-LABEL "Dept"
   tt-mch-tran.sheet-no         COLUMN-LABEL "Sheet" FORMAT "ZZ9" WIDTH 7
   tt-mch-tran.blank-no         COLUMN-LABEL "Blank" FORMAT "ZZ9" WIDTH 7
   tt-mch-tran.pass             COLUMN-LABEL "Pass" FORMAT ">>>" WIDTH 6
   tt-mch-tran.i-no             COLUMN-LABEL "FG Item #" FORMAT "X(15)" WIDTH 20
   tt-mch-tran.charge-code      COLUMN-LABEL "Charge"
   tt-mch-tran.start-time       COLUMN-LABEL "Start Time"
   tt-mch-tran.start-am         COLUMN-LABEL "" WIDTH 4
   tt-mch-tran.end-time         COLUMN-LABEL "End Time"
   tt-mch-tran.end-am           COLUMN-LABEL "" WIDTH 4
   tt-mch-tran.total-hours      COLUMN-LABEL "Total Hours" FORMAT ">>>>9.99-" WIDTH 18
   tt-mch-tran.qty-posted       COLUMN-LABEL "Qty. Posted" FORMAT ">>>>>>>9-" WIDTH 20
   tt-mch-tran.qty-waste        COLUMN-LABEL "Qty. Wasted" FORMAT ">>>>9-" WIDTH 16
   tt-mch-tran.complete         COLUMN-LABEL "Completed"
   tt-mch-tran.dl-rate          COLUMN-LABEL "D.L. Rate"
   tt-mch-tran.varoh            COLUMN-LABEL "Var OH"
   tt-mch-tran.fixoh            COLUMN-LABEL "Fixed OH"
   tt-mch-tran.crew             COLUMN-LABEL "Crew"


   ENABLE tt-mch-tran.tran-date
          tt-mch-tran.machine-code
          tt-mch-tran.dept
          tt-mch-tran.sheet-no
          tt-mch-tran.blank-no
          tt-mch-tran.pass
          tt-mch-tran.i-no
          tt-mch-tran.charge-code
          tt-mch-tran.start-time
          tt-mch-tran.start-am
          tt-mch-tran.end-time
          tt-mch-tran.end-am
          tt-mch-tran.total-hours
          tt-mch-tran.qty-posted
          tt-mch-tran.qty-waste
          tt-mch-tran.COMPLETE
          tt-mch-tran.varoh
          tt-mch-tran.fixoh
          tt-mch-tran.crew
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 135 BY 15.95
         BGCOLOR 8 FONT 2 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     BROWSE-2 AT ROW 1 COL 2
     SPACE(0.19) SKIP(0.14)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Design Page: 2
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow.i}
{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-2 1 D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-mch-tran
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 D-Dialog
ON MOUSE-SELECT-DBLCLICK OF BROWSE-2 IN FRAME D-Dialog
DO:  
  IF (USERID("nosweat") EQ "asi" OR l-jcMchUpd-sec) AND
    AVAILABLE tt-mch-tran THEN DO:
    RUN set-read-only (INPUT NO,
                       INPUT tt-mch-tran.tran-type EQ "HRS").

    APPLY "entry" TO tt-mch-tran.tran-date IN BROWSE {&browse-name}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 D-Dialog
ON ROW-LEAVE OF BROWSE-2 IN FRAME D-Dialog
DO:
     RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
SESSION:DATA-ENTRY-RETURN = YES.

ON 'leave':U OF tt-mch-tran.start-am DO:
  IF LASTKEY NE -1 THEN DO:
    IF LOOKUP(tt-mch-tran.start-am:SCREEN-VALUE IN BROWSE {&browse-name},"AM,PM") = 0 THEN DO:
        MESSAGE "Must be AM or PM."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN NO-APPLY.
    END.
    ELSE RUN calc-num-hours-proc.
    RUN update-record2.
  END.
END.

ON 'leave':U OF tt-mch-tran.end-am DO:
  IF LASTKEY NE -1 THEN DO:
    IF LOOKUP(tt-mch-tran.end-am:SCREEN-VALUE IN BROWSE {&browse-name},"AM,PM") = 0 THEN DO:
        MESSAGE "Must be AM or PM."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN NO-APPLY.
    END.
    ELSE RUN calc-num-hours-proc.
    RUN update-record2.
  END.
END.

ON 'leave':U OF tt-mch-tran.start-time IN BROWSE {&browse-name} DO:

  IF LASTKEY NE -1 THEN DO:
    RUN valid-time(INPUT tt-mch-tran.start-time:SCREEN-VALUE IN BROWSE {&browse-name},
                   OUTPUT op-valid).

    IF NOT op-valid THEN
      RETURN NO-APPLY.
    ELSE RUN calc-num-hours-proc.
    RUN update-record2.
  END.
END.

ON 'leave':U OF tt-mch-tran.end-time IN BROWSE {&browse-name} DO:

  IF LASTKEY NE -1 THEN DO:
    RUN valid-time(INPUT tt-mch-tran.end-time:SCREEN-VALUE IN BROWSE {&browse-name},
                   OUTPUT op-valid).

    IF NOT op-valid THEN
      RETURN NO-APPLY.
    ELSE RUN calc-num-hours-proc.
    RUN update-record2.
  END.
END.

ON 'leave':U  OF tt-mch-tran.tran-date, tt-mch-tran.machine-code, tt-mch-tran.dept, tt-mch-tran.sheet-no,
    tt-mch-tran.blank-no, tt-mch-tran.pass, tt-mch-tran.i-no, tt-mch-tran.charge-code, tt-mch-tran.total-hours, tt-mch-tran.qty-posted,
    tt-mch-tran.qty-waste, tt-mch-tran.COMPLETE, tt-mch-tran.varoh, tt-mch-tran.fixoh
    IN BROWSE {&browse-name} DO:
    RUN update-record2.
END.

ON 'leave':U OF tt-mch-tran.crew IN BROWSE {&browse-name} DO:
    IF LASTKEY NE -1 THEN DO:
         RUN update-record.  
    END.
END.

ON 'return':U OF tt-mch-tran.tran-date, tt-mch-tran.machine-code, tt-mch-tran.sheet-no,
   tt-mch-tran.blank-no, tt-mch-tran.i-no, tt-mch-tran.charge-code,
   tt-mch-tran.total-hours, tt-mch-tran.qty-posted, tt-mch-tran.qty-waste,
   tt-mch-tran.COMPLETE, tt-mch-tran.fixoh, tt-mch-tran.varoh, tt-mch-tran.dept
   IN BROWSE {&browse-name} DO:
   RUN update-record.
END.

ON 'return':U OF tt-mch-tran.start-time IN BROWSE {&browse-name}
DO:
    RUN valid-time(INPUT tt-mch-tran.start-time:SCREEN-VALUE IN BROWSE {&browse-name},
                   OUTPUT op-valid).
    IF op-valid THEN DO:
      RUN calc-num-hours-proc.
      RUN update-record.
    END.
    ELSE RETURN NO-APPLY.
END.

ON 'return':U OF tt-mch-tran.end-time IN BROWSE {&browse-name}
DO:
    RUN valid-time(INPUT tt-mch-tran.end-time:SCREEN-VALUE IN BROWSE {&browse-name},
                   OUTPUT op-valid).
    IF op-valid THEN DO:
      RUN calc-num-hours-proc.
      RUN update-record.
    END.
    ELSE RETURN NO-APPLY.
END.

ON 'return':U OF tt-mch-tran.start-am IN BROWSE {&browse-name}
DO:
  IF LOOKUP(tt-mch-tran.start-am:SCREEN-VALUE IN BROWSE {&browse-name},"AM,PM") = 0 THEN DO:
      MESSAGE "Must be AM or PM."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN NO-APPLY.
  END.
  RUN calc-num-hours-proc.
  RUN update-record.
END.

ON 'return':U OF tt-mch-tran.end-am IN BROWSE {&browse-name}
DO:
  IF LOOKUP(tt-mch-tran.end-am:SCREEN-VALUE IN BROWSE {&browse-name},"AM,PM") = 0 THEN DO:
      MESSAGE "Must be AM or PM."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN NO-APPLY.
  END.
  RUN calc-num-hours-proc.
  RUN update-record.
END.


ON 'leave':U OF tt-mch-tran.dept IN BROWSE {&browse-name}
DO:
  IF tt-mch-tran.dept:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN DO:
      RUN valid-dept(INPUT tt-mch-tran.dept:SCREEN-VALUE IN BROWSE {&browse-name},
                     INPUT tt-mch-tran.machine-code:SCREEN-VALUE IN BROWSE {&browse-name},
                     OUTPUT op-valid).
  END.
  IF NOT op-valid THEN DO:

        MESSAGE "Department entered must match the first department in the machine file."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
  END.

END.


FRAME {&FRAME-NAME}:TITLE = "Job #: " + ip-job-no + "-" + STRING(ip-job-no2)
                          + " Sheet: " + STRING(ip-sheet-num)
                          + " Blank: " + STRING(ip-blank-num)
                          + " Machine: " + ip-mach-code.

RUN set-read-only(INPUT YES,
                  INPUT NO).

RUN build-table.

APPLY "entry" TO BROWSE {&browse-name}.

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table D-Dialog 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-count AS INT NO-UNDO.

  EMPTY TEMP-TABLE tt-mch-tran.

  for each mch-act WHERE
    mch-act.company eq cocode AND
    mch-act.job-no  eq ip-job-no AND
    mch-act.job-no2 EQ ip-job-no2 AND
    mch-act.frm     EQ ip-sheet-num AND
    mch-act.blank-no EQ ip-blank-num AND
    mch-act.m-code   EQ ip-mach-code
    no-lock:

    CREATE tt-mch-tran.
    ASSIGN tt-mch-tran.tran-type = "HRS"
           tt-mch-tran.tran-date = mch-act.op-date
           tt-mch-tran.job-no = mch-act.job-no
           tt-mch-tran.job-no2 = mch-act.job-no2
           tt-mch-tran.machine-code = mch-act.m-code
           tt-mch-tran.dept = mch-act.dept
           tt-mch-tran.sheet-no = mch-act.frm
           tt-mch-tran.blank-no = mch-act.blank-no
           tt-mch-tran.pass = mch-act.pass
           tt-mch-tran.i-no = mch-act.i-no
           tt-mch-tran.charge-code = mch-act.CODE
           tt-mch-tran.total-hours = mch-act.hours
           tt-mch-tran.qty-posted = mch-act.qty
           tt-mch-tran.qty-waste = mch-act.waste
           tt-mch-tran.COMPLETE = mch-act.COMPLETE
           tt-mch-tran.ROWID = ROWID(mch-act)
           tt-mch-tran.start-am = IF mch-act.START LT 43140 OR
                                     mch-act.START EQ 86400 THEN "AM" ELSE "PM"
           tt-mch-tran.end-am = IF mch-act.stopp LT 43140 OR
                                   mch-act.stopp EQ 86400 THEN "AM" ELSE "PM"
           tt-mch-tran.start-time = IF tt-mch-tran.start-am = "AM" THEN STRING(mch-act.START,"HH:MM")
                                    ELSE STRING(mch-act.START - 43200,"HH:MM")
           tt-mch-tran.end-time = IF tt-mch-tran.end-am = "AM" THEN STRING(mch-act.stopp,"HH:MM")
                                  ELSE STRING(mch-act.stopp - 43200,"HH:MM")
           tt-mch-tran.fixoh = mch-act.fixoh
           tt-mch-tran.varoh = mch-act.varoh
           tt-mch-tran.crew = mch-act.crew.


    DO v-count = 1 TO 50:
       tt-mch-tran.dl-rate = tt-mch-tran.dl-rate + mch-act.rate[v-count].
    END.

    RELEASE tt-mch-tran.

  END.

  for each misc-act WHERE
    misc-act.company eq cocode AND
    misc-act.job-no  eq ip-job-no AND
    misc-act.job-no2 EQ ip-job-no2 AND
    misc-act.frm     EQ ip-sheet-num AND
    misc-act.blank-no EQ ip-blank-num AND
    misc-act.m-code   EQ ip-mach-code
    no-lock:

    CREATE tt-mch-tran.
    ASSIGN tt-mch-tran.tran-type = IF misc-act.ml THEN "MSC-M" ELSE "MSC-H"
           tt-mch-tran.tran-date = misc-act.misc-date
           tt-mch-tran.job-no = misc-act.job-no
           tt-mch-tran.job-no2 = misc-act.job-no2
           tt-mch-tran.machine-code = misc-act.m-code
           tt-mch-tran.dept = misc-act.dept
           tt-mch-tran.sheet-no = misc-act.frm
           tt-mch-tran.blank-no = misc-act.blank-no
           tt-mch-tran.i-no = misc-act.i-no
           tt-mch-tran.qty-posted = misc-act.qty
           tt-mch-tran.ROWID = ROWID(tt-mch-tran)
           tt-mch-tran.dl-rate = ?
           tt-mch-tran.fixoh = ?
           tt-mch-tran.varoh = ?.
    RELEASE tt-mch-tran.
  END.

  OPEN QUERY {&browse-name} FOR EACH tt-mch-tran NO-LOCK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-num-hours-proc D-Dialog 
PROCEDURE calc-num-hours-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR tmp-time1 AS INT NO-UNDO.
  DEF VAR tmp-time2 AS INT NO-UNDO.

  ASSIGN
      tmp-time1 = INT(SUBSTR(tt-mch-tran.start-time:SCREEN-VALUE IN BROWSE {&browse-name},1,2)) * 3600
                + INT(SUBSTR(tt-mch-tran.start-time:SCREEN-VALUE IN BROWSE {&browse-name},4,2)) * 60
      tmp-time2 = INT(SUBSTR(tt-mch-tran.end-time:SCREEN-VALUE IN BROWSE {&browse-name},1,2)) * 3600
                + INT(SUBSTR(tt-mch-tran.end-time:SCREEN-VALUE IN BROWSE {&browse-name},4,2)) * 60.

    IF tt-mch-tran.start-am:SCREEN-VALUE IN BROWSE {&browse-name} = "PM" THEN
      tmp-time1 = tmp-time1 + 43200.

    IF tt-mch-tran.end-am:SCREEN-VALUE IN BROWSE {&browse-name} = "PM" THEN
      tmp-time2 = tmp-time2 + 43200.

    tt-mch-tran.total-hours:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(ROUND(DEC(tmp-time2 - tmp-time1) / 3600,2)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  ENABLE BROWSE-2 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-mch-tran"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-read-only D-Dialog 
PROCEDURE set-read-only :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-log AS LOG NO-UNDO.
  DEF INPUT PARAM ip-hrs AS LOG NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

    ASSIGN
      tt-mch-tran.tran-date:read-only IN BROWSE {&browse-name} = ip-log
      tt-mch-tran.machine-code:read-only IN BROWSE {&browse-name} = ip-log
      tt-mch-tran.sheet-no:read-only IN BROWSE {&browse-name} = ip-log
      tt-mch-tran.blank-no:read-only IN BROWSE {&browse-name} = ip-log
      tt-mch-tran.i-no:read-only IN BROWSE {&browse-name} = ip-log
      tt-mch-tran.qty-posted:read-only IN BROWSE {&browse-name} = ip-log.

    IF ip-log OR ip-hrs THEN
      ASSIGN
        tt-mch-tran.pass:read-only IN BROWSE {&browse-name} = ip-log
        tt-mch-tran.charge-code:read-only IN BROWSE {&browse-name} = ip-log
        tt-mch-tran.dept:read-only IN BROWSE {&browse-name} = ip-log
        tt-mch-tran.start-time:read-only IN BROWSE {&browse-name} = ip-log
        tt-mch-tran.start-am:READ-ONLY IN BROWSE {&browse-name} = ip-log
        tt-mch-tran.end-time:read-only IN BROWSE {&browse-name} = ip-log
        tt-mch-tran.end-am:read-only IN BROWSE {&browse-name} = ip-log
        tt-mch-tran.total-hours:read-only IN BROWSE {&browse-name} = ip-log
        tt-mch-tran.qty-waste:read-only IN BROWSE {&browse-name} = ip-log
        tt-mch-tran.complete:read-only IN BROWSE {&browse-name} = ip-log
        tt-mch-tran.fixoh:read-only IN BROWSE {&browse-name} = ip-log
        tt-mch-tran.varoh:read-only IN BROWSE {&browse-name} = ip-log
        tt-mch-tran.crew:READ-ONLY IN BROWSE {&browse-name} = ip-log.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-record D-Dialog 
PROCEDURE update-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rowid AS ROWID NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

    lv-rowid = ROWID(tt-mch-tran).

    IF tt-mch-tran.tran-type:SCREEN-VALUE IN BROWSE {&browse-name} = "HRS" THEN DO:
      FIND mch-act WHERE ROWID(mch-act) EQ tt-mch-tran.ROWID EXCLUSIVE-LOCK NO-ERROR.

      IF AVAILABLE mch-act THEN DO:

        ASSIGN
          tt-mch-tran.tran-date
          tt-mch-tran.job-no
          tt-mch-tran.job-no2
          tt-mch-tran.machine-code
          tt-mch-tran.dept
          tt-mch-tran.sheet-no
          tt-mch-tran.blank-no
          tt-mch-tran.pass
          tt-mch-tran.i-no
          tt-mch-tran.charge-code
          tt-mch-tran.total-hours
          tt-mch-tran.qty-posted
          tt-mch-tran.qty-waste
          tt-mch-tran.complete
          tt-mch-tran.start-time
          tt-mch-tran.end-time
          tt-mch-tran.start-am
          tt-mch-tran.end-am
          tt-mch-tran.fixoh
          tt-mch-tran.varoh
          tt-mch-tran.crew
          mch-act.op-date = tt-mch-tran.tran-date
          mch-act.job-no = tt-mch-tran.job-no
          mch-act.job-no2 = tt-mch-tran.job-no2
          mch-act.m-code = tt-mch-tran.machine-code
          mch-act.dept = tt-mch-tran.dept
          mch-act.frm = tt-mch-tran.sheet-no
          mch-act.blank-no = tt-mch-tran.blank-no
          mch-act.pass = tt-mch-tran.pass
          mch-act.i-no = tt-mch-tran.i-no 
          mch-act.CODE = tt-mch-tran.charge-code 
          mch-act.hours = tt-mch-tran.total-hours
          mch-act.qty = tt-mch-tran.qty-posted
          mch-act.waste = tt-mch-tran.qty-waste
          mch-act.COMPLETE = tt-mch-tran.COMPLETE
          mch-act.START = (INT(SUBSTR(tt-mch-tran.start-time:SCREEN-VALUE IN BROWSE {&browse-name},1,2)) * 3600) +
                          (INT(SUBSTR(tt-mch-tran.start-time:SCREEN-VALUE IN BROWSE {&browse-name},4,2)) * 60)
          mch-act.stopp = (INT(SUBSTR(tt-mch-tran.end-time:SCREEN-VALUE IN BROWSE {&browse-name},1,2)) * 3600) +
                          (INT(SUBSTR(tt-mch-tran.end-time:SCREEN-VALUE IN BROWSE {&browse-name},4,2)) * 60)
          mch-act.fixoh = tt-mch-tran.fixoh
          mch-act.varoh = tt-mch-tran.varoh
          mch-act.crew = tt-mch-tran.crew.

        IF tt-mch-tran.start-am = "PM" THEN
          mch-act.START = mch-act.START + 43200.

        IF tt-mch-tran.end-am = "PM" THEN
          mch-act.stopp = mch-act.stopp + 43200.

        RELEASE mch-act.
      END.
    END.
    ELSE DO:
      FIND misc-act WHERE ROWID(misc-act) EQ tt-mch-tran.ROWID EXCLUSIVE-LOCK NO-ERROR.

      IF AVAILABLE misc-act THEN DO:
        ASSIGN
           tt-mch-tran.tran-date
           tt-mch-tran.job-no
           tt-mch-tran.job-no2
           tt-mch-tran.machine-code
           tt-mch-tran.dept
           tt-mch-tran.sheet-no
           tt-mch-tran.blank-no
           tt-mch-tran.i-no
           tt-mch-tran.qty-posted
           misc-act.misc-date = tt-mch-tran.tran-date
           misc-act.job-no = tt-mch-tran.job-no
           misc-act.job-no2 = tt-mch-tran.job-no2
           misc-act.m-code = tt-mch-tran.machine-code
           misc-act.dept = tt-mch-tran.dept
           misc-act.frm = tt-mch-tran.sheet-no
           misc-act.blank-no = tt-mch-tran.blank-no
           misc-act.i-no = tt-mch-tran.i-no
           misc-act.qty = tt-mch-tran.qty-posted.

        RELEASE misc-act.
      END.
    END.

    RUN set-read-only(INPUT YES,
                      INPUT NO).

    CLOSE QUERY {&browse-name}.
    OPEN QUERY {&browse-name} FOR EACH tt-mch-tran NO-LOCK.

    REPOSITION {&browse-name} TO ROWID lv-rowid NO-ERROR.
    APPLY "ENTRY" TO {&browse-name} IN FRAME {&FRAME-NAME}.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-record2 D-Dialog 
PROCEDURE update-record2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rowid AS ROWID NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

    lv-rowid = ROWID(tt-mch-tran).

    IF tt-mch-tran.tran-type:SCREEN-VALUE IN BROWSE {&browse-name} = "HRS" THEN DO:
      FIND mch-act WHERE ROWID(mch-act) EQ tt-mch-tran.ROWID EXCLUSIVE-LOCK NO-ERROR.

      IF AVAILABLE mch-act THEN DO:

        ASSIGN
          tt-mch-tran.tran-date
          tt-mch-tran.job-no
          tt-mch-tran.job-no2
          tt-mch-tran.machine-code
          tt-mch-tran.dept
          tt-mch-tran.sheet-no
          tt-mch-tran.blank-no
          tt-mch-tran.pass
          tt-mch-tran.i-no
          tt-mch-tran.charge-code
          tt-mch-tran.total-hours
          tt-mch-tran.qty-posted
          tt-mch-tran.qty-waste
          tt-mch-tran.complete
          tt-mch-tran.start-time
          tt-mch-tran.end-time
          tt-mch-tran.start-am
          tt-mch-tran.end-am
          tt-mch-tran.fixoh
          tt-mch-tran.varoh
          tt-mch-tran.crew
          mch-act.op-date = tt-mch-tran.tran-date
          mch-act.job-no = tt-mch-tran.job-no
          mch-act.job-no2 = tt-mch-tran.job-no2
          mch-act.m-code = tt-mch-tran.machine-code
          mch-act.dept = tt-mch-tran.dept
          mch-act.frm = tt-mch-tran.sheet-no
          mch-act.blank-no = tt-mch-tran.blank-no
          mch-act.pass = tt-mch-tran.pass
          mch-act.i-no = tt-mch-tran.i-no 
          mch-act.CODE = tt-mch-tran.charge-code 
          mch-act.hours = tt-mch-tran.total-hours
          mch-act.qty = tt-mch-tran.qty-posted
          mch-act.waste = tt-mch-tran.qty-waste
          mch-act.COMPLETE = tt-mch-tran.COMPLETE
          mch-act.START = (INT(SUBSTR(tt-mch-tran.start-time:SCREEN-VALUE IN BROWSE {&browse-name},1,2)) * 3600) +
                          (INT(SUBSTR(tt-mch-tran.start-time:SCREEN-VALUE IN BROWSE {&browse-name},4,2)) * 60)
          mch-act.stopp = (INT(SUBSTR(tt-mch-tran.end-time:SCREEN-VALUE IN BROWSE {&browse-name},1,2)) * 3600) +
                          (INT(SUBSTR(tt-mch-tran.end-time:SCREEN-VALUE IN BROWSE {&browse-name},4,2)) * 60)
          mch-act.fixoh = tt-mch-tran.fixoh
          mch-act.varoh = tt-mch-tran.varoh
          mch-act.crew = tt-mch-tran.crew.

        IF tt-mch-tran.start-am = "PM" THEN
          mch-act.START = mch-act.START + 43200.

        IF tt-mch-tran.end-am = "PM" THEN
          mch-act.stopp = mch-act.stopp + 43200.

        RELEASE mch-act.
      END.
    END.
    ELSE DO:
      FIND misc-act WHERE ROWID(misc-act) EQ tt-mch-tran.ROWID EXCLUSIVE-LOCK NO-ERROR.

      IF AVAILABLE misc-act THEN DO:
        ASSIGN
           tt-mch-tran.tran-date
           tt-mch-tran.job-no
           tt-mch-tran.job-no2
           tt-mch-tran.machine-code
           tt-mch-tran.dept
           tt-mch-tran.sheet-no
           tt-mch-tran.blank-no
           tt-mch-tran.i-no
           tt-mch-tran.qty-posted
           misc-act.misc-date = tt-mch-tran.tran-date
           misc-act.job-no = tt-mch-tran.job-no
           misc-act.job-no2 = tt-mch-tran.job-no2
           misc-act.m-code = tt-mch-tran.machine-code
           misc-act.dept = tt-mch-tran.dept
           misc-act.frm = tt-mch-tran.sheet-no
           misc-act.blank-no = tt-mch-tran.blank-no
           misc-act.i-no = tt-mch-tran.i-no
           misc-act.qty = tt-mch-tran.qty-posted.

        RELEASE misc-act.
      END.
    END.

  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-dept D-Dialog 
PROCEDURE valid-dept :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ip-dept AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ip-mach AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER op-valid AS LOG INIT YES NO-UNDO.
DEFINE BUFFER lb-mach FOR mach.
    FIND FIRST lb-mach WHERE lb-mach.company EQ cocode 
                AND  lb-mach.m-code EQ ip-mach NO-LOCK NO-ERROR.
    IF AVAIL lb-mach THEN
        IF lb-mach.dept[1] NE ip-dept  THEN
            op-valid = FALSE.
        ELSE 
            op-valid = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-time D-Dialog 
PROCEDURE valid-time :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-time AS CHAR NO-UNDO.
  DEFINE OUTPUT PARAMETER op-valid AS LOG INIT YES NO-UNDO.

  DEF VAR lv-hour AS INT NO-UNDO.
  DEF VAR lv-min AS INT NO-UNDO.

  lv-hour = INT(SUBSTR(ip-time,1,2)) NO-ERROR.
  IF ERROR-STATUS:ERROR OR NOT (lv-hour GE 0 AND lv-hour LE 12) THEN DO:
     MESSAGE "Invalid Time."
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     op-valid = NO.
     LEAVE.
  END.

  lv-min = INT(SUBSTR(ip-time,4,2)) NO-ERROR.
  IF ERROR-STATUS:ERROR OR NOT (lv-min GE 0 AND lv-min LE 59) THEN DO:
     MESSAGE "Invalid Time."
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     op-valid = NO.
     LEAVE.
  END.

  IF SUBSTR(ip-time,3,1) NE ":" THEN DO:
     MESSAGE "Invalid Time."
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     op-valid = NO.
     LEAVE.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

