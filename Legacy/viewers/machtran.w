&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          emptrack         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: addon/viewers/machtran.w

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

&SCOPED-DEFINE Translation YES
&SCOPED-DEFINE translationInclude touch/localview.i

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/gcompany.i}

{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
ASSIGN cocode = g_company
       locode = g_loc.

{methods/defines/jobmach.i &NEW="NEW"}
{custom/emprate.i}

&SCOPED-DEFINE update-tran update-tran
&SCOPED-DEFINE enable-proc enable-proc

DEF VAR lv-mach-list AS cha NO-UNDO. /* sch-machine list*/

DEF TEMP-TABLE tt-mach NO-UNDO
    FIELD machine AS CHAR
    INDEX machine machine.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES machtran
&Scoped-define FIRST-EXTERNAL-TABLE machtran


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR machtran.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS machtran.machine machtran.form_number machtran.blank_number ~
machtran.pass_sequence machtran.start_date machtran.end_date ~
machtran.charge_code machtran.run_qty machtran.waste_qty machtran.completed 
&Scoped-define ENABLED-TABLES machtran
&Scoped-define FIRST-ENABLED-TABLE machtran
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS machtran.machine machtran.job_number ~
machtran.job_sub machtran.form_number machtran.blank_number ~
machtran.pass_sequence machtran.start_date machtran.end_date ~
machtran.total_time machtran.charge_code machtran.run_qty ~
machtran.waste_qty machtran.completed 
&Scoped-define DISPLAYED-TABLES machtran
&Scoped-define FIRST-DISPLAYED-TABLE machtran
&Scoped-Define DISPLAYED-OBJECTS mach_m-dscr start_hour start_minute ~
start_ampm end_hour end_minute end_ampm fi_shift job-code_cat job-code_dscr ~
F1 F-3 F-4 F-5 F-6 F-7 F-9 F-10 F-8 F-2 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,TIME-FIELDS,F1 */
&Scoped-define ADM-CREATE-FIELDS machtran.machine machtran.job_number ~
machtran.job_sub machtran.form_number machtran.blank_number ~
machtran.pass_sequence 
&Scoped-define ADM-ASSIGN-FIELDS fi_shift 
&Scoped-define DISPLAY-FIELD machtran.machine machtran.charge_code 
&Scoped-define TIME-FIELDS start_hour start_minute start_ampm end_hour ~
end_minute end_ampm 
&Scoped-define F1 F1 F-3 F-4 F-5 F-6 F-7 F-9 F-10 F-8 F-2 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE end_ampm AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "","PM","AM" 
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE start_ampm AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "PM","AM" 
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE end_hour AS CHARACTER FORMAT "X(2)":U 
     LABEL "End Time" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE end_minute AS CHARACTER FORMAT "X(2)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-10 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-2 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-3 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-4 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-5 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-6 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-7 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-8 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-9 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F1 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE fi_shift AS INTEGER FORMAT ">>" INITIAL 0 
     LABEL "Shift" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE job-code_cat AS CHARACTER FORMAT "x(3)" 
     LABEL "Category" 
     VIEW-AS FILL-IN 
     SIZE 6.6 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4.

DEFINE VARIABLE job-code_dscr AS CHARACTER FORMAT "x(45)" 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4.

DEFINE VARIABLE mach_m-dscr AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4.

DEFINE VARIABLE start_hour AS CHARACTER FORMAT "X(2)":U 
     LABEL "Start Time" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE start_minute AS CHARACTER FORMAT "X(2)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE text-5 AS CHARACTER FORMAT "X(256)":U INITIAL "Create/Update Emp. Trans. Manually" 
      VIEW-AS TEXT 
     SIZE 43 BY .62
     FGCOLOR 12  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 115 BY 7.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     text-5 AT ROW 1.24 COL 65 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     machtran.machine AT ROW 1.24 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 15 FONT 4
     mach_m-dscr AT ROW 1.24 COL 36 COLON-ALIGNED NO-LABEL
     machtran.job_number AT ROW 2.43 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
          BGCOLOR 15 FONT 4
     machtran.job_sub AT ROW 2.43 COL 36 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
          BGCOLOR 15 FONT 4
     machtran.form_number AT ROW 2.43 COL 62 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
          BGCOLOR 15 FONT 4
     machtran.blank_number AT ROW 2.43 COL 90 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
          BGCOLOR 15 FONT 4
     machtran.pass_sequence AT ROW 2.43 COL 106 COLON-ALIGNED
          LABEL "Pass"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     machtran.start_date AT ROW 3.62 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     start_hour AT ROW 4.81 COL 24 COLON-ALIGNED HELP
          "Enter Starting Hour"
     start_minute AT ROW 4.81 COL 30 COLON-ALIGNED HELP
          "Enter Starting Minute"
     start_ampm AT ROW 4.81 COL 35 COLON-ALIGNED NO-LABEL
     machtran.end_date AT ROW 3.62 COL 62 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     end_hour AT ROW 4.81 COL 62 COLON-ALIGNED HELP
          "Enter Ending Hour"
     end_minute AT ROW 4.81 COL 68 COLON-ALIGNED HELP
          "Enter Ending Minute"
     end_ampm AT ROW 4.81 COL 73 COLON-ALIGNED NO-LABEL
     fi_shift AT ROW 3.62 COL 106 COLON-ALIGNED
     machtran.total_time AT ROW 4.81 COL 99 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     machtran.charge_code AT ROW 6 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15 FONT 4
     job-code_cat AT ROW 6 COL 54 COLON-ALIGNED
     job-code_dscr AT ROW 6 COL 62 COLON-ALIGNED NO-LABEL
     machtran.run_qty AT ROW 7.19 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
          BGCOLOR 15 FONT 4
     machtran.waste_qty AT ROW 7.19 COL 62 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
          BGCOLOR 15 FONT 4
     F1 AT ROW 1.24 COL 36 NO-LABEL
     F-3 AT ROW 2.43 COL 35 NO-LABEL
     F-4 AT ROW 2.43 COL 42 NO-LABEL
     F-5 AT ROW 2.43 COL 70 NO-LABEL
     F-6 AT ROW 2.43 COL 96 NO-LABEL
     F-7 AT ROW 2.43 COL 113 NO-LABEL
     F-9 AT ROW 3.62 COL 40 NO-LABEL
     F-10 AT ROW 3.62 COL 78 NO-LABEL
     F-8 AT ROW 3.62 COL 113 NO-LABEL
     F-2 AT ROW 6 COL 34 NO-LABEL
     machtran.completed AT ROW 7.19 COL 106 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     "-" VIEW-AS TEXT
          SIZE 2 BY .62 AT ROW 2.67 COL 36
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: machtran
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 7.38
         WIDTH              = 115.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN machtran.blank_number IN FRAME F-Main
   1                                                                    */
ASSIGN 
       machtran.blank_number:PRIVATE-DATA IN FRAME F-Main     = 
                "Blank Number".

/* SETTINGS FOR FILL-IN machtran.charge_code IN FRAME F-Main
   4                                                                    */
ASSIGN 
       machtran.charge_code:PRIVATE-DATA IN FRAME F-Main     = 
                "Charge Code".

ASSIGN 
       machtran.completed:PRIVATE-DATA IN FRAME F-Main     = 
                "Completed?".

/* SETTINGS FOR COMBO-BOX end_ampm IN FRAME F-Main
   NO-ENABLE 5                                                          */
ASSIGN 
       machtran.end_date:PRIVATE-DATA IN FRAME F-Main     = 
                "End Date".

/* SETTINGS FOR FILL-IN end_hour IN FRAME F-Main
   NO-ENABLE 5                                                          */
ASSIGN 
       end_hour:PRIVATE-DATA IN FRAME F-Main     = 
                "End Time".

/* SETTINGS FOR FILL-IN end_minute IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN F-10 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-10:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-2 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-2:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-3 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-3:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-4 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-4:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-5 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-5:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-6 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-6:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-7 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-7:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-8 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-8:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-9 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-9:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F1 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F1:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fi_shift IN FRAME F-Main
   NO-ENABLE 2                                                          */
ASSIGN 
       fi_shift:PRIVATE-DATA IN FRAME F-Main     = 
                "Shift".

/* SETTINGS FOR FILL-IN machtran.form_number IN FRAME F-Main
   1                                                                    */
ASSIGN 
       machtran.form_number:PRIVATE-DATA IN FRAME F-Main     = 
                "Form Number".

/* SETTINGS FOR FILL-IN job-code_cat IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       job-code_cat:PRIVATE-DATA IN FRAME F-Main     = 
                "Category".

/* SETTINGS FOR FILL-IN job-code_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN machtran.job_number IN FRAME F-Main
   NO-ENABLE 1                                                          */
ASSIGN 
       machtran.job_number:PRIVATE-DATA IN FRAME F-Main     = 
                "Job Number".

/* SETTINGS FOR FILL-IN machtran.job_sub IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN machtran.machine IN FRAME F-Main
   NO-ENABLE 1 4                                                        */
ASSIGN 
       machtran.machine:PRIVATE-DATA IN FRAME F-Main     = 
                "Machine".

/* SETTINGS FOR FILL-IN mach_m-dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN machtran.pass_sequence IN FRAME F-Main
   1 EXP-LABEL                                                          */
ASSIGN 
       machtran.pass_sequence:PRIVATE-DATA IN FRAME F-Main     = 
                "Pass".

ASSIGN 
       machtran.run_qty:PRIVATE-DATA IN FRAME F-Main     = 
                "Run Quantity".

/* SETTINGS FOR COMBO-BOX start_ampm IN FRAME F-Main
   NO-ENABLE 5                                                          */
ASSIGN 
       machtran.start_date:PRIVATE-DATA IN FRAME F-Main     = 
                "Start Date".

/* SETTINGS FOR FILL-IN start_hour IN FRAME F-Main
   NO-ENABLE 5                                                          */
ASSIGN 
       start_hour:PRIVATE-DATA IN FRAME F-Main     = 
                "Start Time".

/* SETTINGS FOR FILL-IN start_minute IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN text-5 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-5:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN machtran.total_time IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       machtran.total_time:PRIVATE-DATA IN FRAME F-Main     = 
                "Total Time".

ASSIGN 
       machtran.waste_qty:PRIVATE-DATA IN FRAME F-Main     = 
                "Waste Quantity".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME machtran.blank_number
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL machtran.blank_number V-table-Win
ON ENTRY OF machtran.blank_number IN FRAME F-Main /* Blank Number */
DO:
  ASSIGN
    s-machine = machtran.machine:SCREEN-VALUE
    s-job_number = machtran.job_number:SCREEN-VALUE
    s-job_sub = machtran.job_sub:SCREEN-VALUE
    s-form_number = machtran.form_number:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL machtran.blank_number V-table-Win
ON LEAVE OF machtran.blank_number IN FRAME F-Main /* Blank Number */
DO:
  {&methods/lValidateError.i YES}
  {methods/entryerr.i
      &can-find="FIRST job-mch WHERE job-mch.company = gcompany
                                 AND job-mch.m-code = s-machine
                                 AND job-mch.job-no = s-job_number
                                 AND job-mch.job-no2 = INTEGER(s-job_sub)
                                 AND job-mch.frm = INTEGER(s-form_number)
                                 AND job-mch.blank-no = INTEGER(SELF:SCREEN-VALUE)"
      &error-message="Invalid Blank Number"}
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME machtran.charge_code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL machtran.charge_code V-table-Win
ON LEAVE OF machtran.charge_code IN FRAME F-Main /* Charge Code */
DO:
  {&methods/lValidateError.i YES}
  {methods/dispflds.i}
  {methods/entryerr.i
      &can-find="job-code WHERE job-code.code = SELF:SCREEN-VALUE"
      &error-message="Invalid Charge Code"}
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME machtran.end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL machtran.end_date V-table-Win
ON HELP OF machtran.end_date IN FRAME F-Main /* End Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_hour
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_hour V-table-Win
ON LEAVE OF end_hour IN FRAME F-Main /* End Time */
DO:
  {&methods/lValidateError.i YES}
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 12.
  {methods/entryerr.i &error-message="Invalid Hour, range = 0 to 12"}
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_minute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_minute V-table-Win
ON LEAVE OF end_minute IN FRAME F-Main
DO:
  {&methods/lValidateError.i YES}
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 59.
  {methods/entryerr.i &error-message="Invalid Minute, range = 0 to 59"}
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_shift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_shift V-table-Win
ON LEAVE OF fi_shift IN FRAME F-Main /* Shift */
DO:
  {&methods/lValidateError.i YES}
  IF LASTKEY NE -1 THEN DO:
    {methods/entryerr.i
        &can-find="FIRST shifts WHERE shifts.company = gcompany
                                  AND shifts.shift = SELF:SCREEN-VALUE"
        &error-message="Invalid Shift"}
  END.
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME machtran.form_number
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL machtran.form_number V-table-Win
ON ENTRY OF machtran.form_number IN FRAME F-Main /* Form Number */
DO:
  ASSIGN
    s-machine = machtran.machine:SCREEN-VALUE
    s-job_number = machtran.job_number:SCREEN-VALUE
    s-job_sub = machtran.job_sub:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL machtran.form_number V-table-Win
ON LEAVE OF machtran.form_number IN FRAME F-Main /* Form Number */
DO:
    {&methods/lValidateError.i YES}
    IF s-machine EQ "" THEN
        ASSIGN
        s-machine = machtran.machine:SCREEN-VALUE
        s-job_number = machtran.job_number:SCREEN-VALUE
        s-job_sub = machtran.job_sub:SCREEN-VALUE.

  {methods/entryerr.i
      &can-find="FIRST job-mch WHERE job-mch.company = gcompany
                                 AND job-mch.m-code = s-machine
                                 AND job-mch.job-no = s-job_number
                                 AND job-mch.job-no2 = INTEGER(s-job_sub)
                                 AND job-mch.frm = INTEGER(SELF:SCREEN-VALUE)"
      &error-message="Invalid Form Number"}
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME machtran.job_number
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL machtran.job_number V-table-Win
ON ENTRY OF machtran.job_number IN FRAME F-Main /* Job Number */
DO:
  s-machine = machtran.machine:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL machtran.job_number V-table-Win
ON HELP OF machtran.job_number IN FRAME F-Main /* Job Number */
DO:
   DEF VAR char-val AS cha NO-UNDO.
   DEF VAR rec-val AS RECID NO-UNDO.

   RUN windows/l-jobno.w (g_company,FOCUS:SCREEN-VALUE ,OUTPUT char-val, OUTPUT rec-val).
   IF rec-val <> ? THEN DO:
      FIND job-hdr WHERE RECID(job-hdr) = rec-val NO-LOCK NO-ERROR.
      IF AVAIL job-hdr THEN 
         ASSIGN machtran.job_number:SCREEN-VALUE = job-hdr.job-no
                machtran.job_sub:SCREEN-VALUE = string(job-hdr.job-no2).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME   


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL machtran.job_number V-table-Win
ON LEAVE OF machtran.job_number IN FRAME F-Main /* Job Number */
DO:
  {&methods/lValidateError.i YES}
  SELF:SCREEN-VALUE = FILL(' ',6 - LENGTH(SELF:SCREEN-VALUE)) + SELF:SCREEN-VALUE.
  /* edit for scheduled machine */
  IF NOT CAN-FIND(FIRST job-mch WHERE job-mch.company = gcompany
                                 AND job-mch.m-code = s-machine
                                 AND job-mch.job-no = SELF:SCREEN-VALUE) 
  THEN DO:
     RUN valid-sch-machine.
     FIND FIRST job-mch WHERE job-mch.company = gcompany
                          AND job-mch.job-no = SELF:SCREEN-VALUE
                          AND lookup(job-mch.m-code,lv-mach-list) > 0 NO-LOCK NO-ERROR.
     IF AVAIL job-mch THEN ASSIGN machtran.machine:SCREEN-VALUE = job-mch.m-code
                                  s-machine = job-mch.m-code.
  END.

  {methods/entryerr.i
      &can-find="FIRST job-mch WHERE job-mch.company = gcompany
                                 AND job-mch.m-code = s-machine
                                 AND job-mch.job-no = SELF:SCREEN-VALUE"
      &error-message="Invalid Job Number"}
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME machtran.charge_code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL machtran.charge_code V-table-Win
ON HELP OF machtran.charge_code IN FRAME F-Main /* charge code */
DO:
   DEF VAR char-val AS cha NO-UNDO.
   DEF VAR rec-val AS RECID NO-UNDO.

   RUN windows/l-jobcod.w (FOCUS:SCREEN-VALUE ,OUTPUT char-val).
   IF char-val <> ? THEN DO:
         ASSIGN machtran.charge_code:SCREEN-VALUE = ENTRY(1,char-val)
                .
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME  


&Scoped-define SELF-NAME machtran.job_sub
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL machtran.job_sub V-table-Win
ON ENTRY OF machtran.job_sub IN FRAME F-Main /* Sub */
DO:
  ASSIGN
    s-machine = machtran.machine:SCREEN-VALUE
    s-job_number = machtran.job_number:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL machtran.job_sub V-table-Win
ON LEAVE OF machtran.job_sub IN FRAME F-Main /* Sub */
DO:
  {&methods/lValidateError.i YES}
  {methods/entryerr.i
      &can-find="FIRST job-mch WHERE job-mch.company = gcompany
                                 AND job-mch.m-code = s-machine
                                 AND job-mch.job-no = s-job_number
                                 AND job-mch.job-no2 = INTEGER(SELF:SCREEN-VALUE)"
      &error-message="Invalid Job Sub Number"}
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME machtran.machine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL machtran.machine V-table-Win
ON HELP OF machtran.machine IN FRAME F-Main /* Machine */
DO:        
   DEF VAR char-val AS CHAR NO-UNDO.

   RUN windows/l-mach.w (g_company,g_loc, {&self-name}:SCREEN-VALUE, OUTPUT char-val).
   IF char-val NE "" THEN 
       ASSIGN {&self-name}:SCREEN-VALUE = ENTRY(1,char-val)
              mach_m-dscr:SCREEN-VALUE = ENTRY(2,char-val). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL machtran.machine V-table-Win
ON LEAVE OF machtran.machine IN FRAME F-Main /* Machine */
DO:
  {&methods/lValidateError.i YES}
  {methods/dispflds.i}
  {methods/entryerr.i
      &can-find="mach WHERE mach.company = gcompany
                        AND mach.m-code = SELF:SCREEN-VALUE"
      &error-message="Invalid Machine Code"}
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME machtran.pass_sequence
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL machtran.pass_sequence V-table-Win
ON ENTRY OF machtran.pass_sequence IN FRAME F-Main /* Pass */
DO:
  ASSIGN
    s-machine = machtran.machine:SCREEN-VALUE
    s-job_number = machtran.job_number:SCREEN-VALUE
    s-job_sub = machtran.job_sub:SCREEN-VALUE
    s-form_number = machtran.form_number:SCREEN-VALUE
    s-blank_number = machtran.blank_number:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL machtran.pass_sequence V-table-Win
ON LEAVE OF machtran.pass_sequence IN FRAME F-Main /* Pass */
DO:
  {&methods/lValidateError.i YES}
  {methods/entryerr.i
      &can-find="FIRST job-mch WHERE job-mch.company = gcompany
                                 AND job-mch.m-code = s-machine
                                 AND job-mch.job-no = s-job_number
                                 AND job-mch.job-no2 = INTEGER(s-job_sub)
                                 AND job-mch.frm = INTEGER(s-form_number)
                                 AND job-mch.blank-no = INTEGER(s-blank_number)
                                 AND job-mch.pass = INTEGER(SELF:SCREEN-VALUE)"
      &error-message="Invalid Pass Sequence"}
     {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME machtran.start_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL machtran.start_date V-table-Win
ON HELP OF machtran.start_date IN FRAME F-Main /* Start Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME start_hour
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL start_hour V-table-Win
ON LEAVE OF start_hour IN FRAME F-Main /* Start Time */
DO:
  {&methods/lValidateError.i YES}
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 1 OR INTEGER(SELF:SCREEN-VALUE) GT 12.
  {methods/entryerr.i &error-message="Invalid Hour, range = 1 to 12"}
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME start_minute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL start_minute V-table-Win
ON LEAVE OF start_minute IN FRAME F-Main
DO:
  {&methods/lValidateError.i YES}
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 59.
  {methods/entryerr.i &error-message="Invalid Minute, range = 0 to 59"}
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  {custom/getcmpny.i}

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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
  {src/adm/template/row-list.i "machtran"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "machtran"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crt-emp-trans V-table-Win 
PROCEDURE crt-emp-trans :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /*
    /* ======= all employee for the machine ===========*/
      for each empmach no-lock where empmach.company = machtran.company
                         and empmach.machine = machtran.machine
                         break by empmach.employee :       
         if first-of(empmach.employee) then do:
            find employee of empmach no-lock .
            CREATE machemp.
            ASSIGN machemp.table_rec_key = machtran.rec_key
                machemp.employee = empmach.employee
                machemp.start_date = machtran.start_date
                machemp.start_time = machtran.start_time
                machemp.shift = machtran.shift
                machemp.ratetype = 'Standard' 
                machemp.rate_usage = employee.rate_usage
                machemp.end_date = machtran.end_date
                machemp.end_time = machtran.end_time.

             RUN Employee-Rate(machtran.company,machemp.employee,machemp.shift,machtran.machine,
                          machemp.rate_usage,machemp.ratetype,OUTPUT machemp.rate).
             {custom/calctime.i &file="machemp"}

             FIND FIRST emplogin WHERE emplogin.company = machtran.company
                        AND emplogin.employee = machemp.employee
                        AND emplogin.machine = machtran.machine
                        and emplogin.start_date = machtran.start_date
                        and emplogin.start_time = machtran.start_time
                      EXCLUSIVE-LOCK NO-ERROR.
             IF not avaILABLE emplogin THEN
             DO:
                CREATE emplogin.
                ASSIGN
                  emplogin.company = machtran.company
                  emplogin.employee = machemp.employee
                  emplogin.machine = machtran.machine
                  emplogin.start_date = machtran.start_date
                  emplogin.start_time = machtran.start_time
                  emplogin.end_date = machtran.end_date
                  emplogin.end_time = machtran.end_time
                  emplogin.shift = machtran.shift.
                {custom/calctime.i &file="emplogin"}
             end.
         end. /* first-of */
      END. /* each emplogin */ */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crt-lunch-trans V-table-Win 
PROCEDURE crt-lunch-trans :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define buffer bf-machtran for machtran.
  def var bf-machtran-rowid as rowid no-undo.
  def var run-qty like machtran.run_qty no-undo.
  def var waste-qty like machtran.waste_qty no-undo.

      find shifts where shifts.company = machtran.company and
                        shifts.shift = machtran.shift
                        no-lock no-error.

      CREATE bf-machtran.
      ASSIGN
        bf-machtran.company = machtran.company
        bf-machtran.machine = machtran.machine
        bf-machtran.job_number = machtran.job_number
        bf-machtran.job_sub = machtran.job_sub
        bf-machtran.form_number = machtran.form_number
        bf-machtran.blank_number = machtran.blank_number
        bf-machtran.pass_sequence = machtran.pass_sequence
        bf-machtran.start_date = TODAY
        bf-machtran.start_time = shifts.lunch_start /*time-hour * 3600 + time-minute * 60 + ampm*/
        bf-machtran.jobseq = IF AVAILABLE jobseq THEN jobseq.jobseq ELSE 0
        bf-machtran.charge_code = "Lunch"
        bf-machtran.end_date = today
        bf-machtran.end_time = shifts.lunch_end
        bf-machtran.shift = machtran.shift
        bf-machtran-rowid = ROWID(bf-machtran)
        . 
        {custom/calctime.i &file="bf-machtran"}                          

/*      RUN Get-Shift(company_code,machine_code,machtran.start_time,job_sequence,
             OUTPUT machtran.shift). 
      {methods/run_link.i "CONTAINER" "Set_MachTran_Rowid" "(bf-machtran-rowid)"}
*/    
   /* =============== 
     /* get active employees logged into this machine */
      FOR EACH emplogin NO-LOCK
          WHERE emplogin.company = machtran.company
            AND emplogin.machine = machtran.machine
            AND emplogin.end_time = 0
            AND emplogin.total_time = 0,
          FIRST employee OF emplogin NO-LOCK:
         CREATE machemp.
         ASSIGN machemp.table_rec_key = bf-machtran.rec_key
                machemp.employee = emplogin.employee
                machemp.start_date = bf-machtran.start_date
                machemp.start_time = bf-machtran.start_time
                machemp.shift = bf-machtran.shift
                machemp.ratetype = if shifts.lunch_paid then 'Standard' else ""
                machemp.rate_usage = employee.rate_usage
                machemp.end_date = bf-machtran.end_date
                machemp.end_time = bf-machtran.end_time
                bf-machtran.run_qty = run-qty  /*???*/
                bf-machtran.waste_qty = waste-qty  /*???*/
                .
          RUN Employee-Rate(machtran.company,machemp.employee,machemp.shift,machtran.machine,
                          machemp.rate_usage,machemp.ratetype,OUTPUT machemp.rate).
          {custom/calctime.i &file="machemp"}                          
      END. /* each emplogin */
    ================================================*/

    /* ======= all employee for the machine ===========*/
      for each empmach no-lock where empmach.company = machtran.company
                         and empmach.machine = machtran.machine
                         break by empmach.employee :       
         if first-of(empmach.employee) then do:
            find employee of empmach no-lock .
            CREATE machemp.
            ASSIGN machemp.table_rec_key = bf-machtran.rec_key
                machemp.employee = empmach.employee
                machemp.start_date = bf-machtran.start_date
                machemp.start_time = bf-machtran.start_time
                machemp.shift = bf-machtran.shift
                machemp.ratetype = if shifts.lunch_paid then 'Standard' else ""
                machemp.rate_usage = employee.rate_usage
                machemp.end_date = bf-machtran.end_date
                machemp.end_time = bf-machtran.end_time
                bf-machtran.run_qty = run-qty  /*???*/
                bf-machtran.waste_qty = waste-qty  /*???*/.

             RUN Employee-Rate(machtran.company,machemp.employee,machemp.shift,machtran.machine,
                          machemp.rate_usage,machemp.ratetype,OUTPUT machemp.rate).
             {custom/calctime.i &file="machemp"}

           RELEASE machemp.           
         end. 
      END. /* each emplogin */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-proc V-table-Win 
PROCEDURE disable-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    DISABLE fi_shift {&TIME-FIELDS} WITH FRAME {&FRAME-NAME}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-proc V-table-Win 
PROCEDURE enable-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ENABLE fi_shift.
    IF NOT adm-new-record THEN
        DISABLE machtran.machine WITH FRAME {&FRAME-NAME}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var ll-new-record as log no-undo.
  /* Code placed here will execute PRIOR to standard behavior. */
  ll-new-record = adm-new-record.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/viewers/assign/machtran.i}
  /*if ll-new-record then run crt-emp-trans.*/
  /*ELSE RUN upd-emp-trans.*/

  ASSIGN
     text-5:HIDDEN = NO
     machtran.shift = TRIM(STRING(fi_shift,">>")).

  DISPLAY text-5 WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN disable-proc.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/viewers/create/machtran.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF AVAIL machtran AND NOT adm-new-record THEN
    fi_shift = INT(machtran.shift) NO-ERROR.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  DEF VAR lv-new-record AS LOG NO-UNDO.

  lv-new-record = adm-new-record.

  RUN check-date-time NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN.

  RUN VALIDATE-date-time NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN disable-proc.

  IF NOT lv-new-record THEN DO:
     RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
     RUN reopen-query IN WIDGET-HANDLE(char-hdl) (ROWID(machtran)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "machtran"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
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
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE upd-emp-trans V-table-Win 
PROCEDURE upd-emp-trans :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     for each  machemp where machemp.table_rec_key = machtran.rec_key
                        /* and machemp.employee = machtran  
                          and machemp.start_date = machtran.start_date */
                          :
         ASSIGN machemp.start_date = machtran.start_date
                machemp.start_time = machtran.start_time
                machemp.shift = machtran.shift
                machemp.ratetype = 'Standard' 
               /* machemp.rate_usage = employee.rate_usage */
                machemp.end_date = machtran.end_date
                machemp.end_time = machtran.end_time.

        RUN Employee-Rate(machtran.company,machemp.employee,machemp.shift,machtran.machine,
                     machemp.rate_usage,machemp.ratetype,OUTPUT machemp.rate).

        {custom/calctime.i &file="machemp"}
    end.                      

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-tran V-table-Win 
PROCEDURE update-tran :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{&methods/lValidateError.i YES}
IF AVAIL machtran AND machtran.posted THEN DO:
   MESSAGE "It's already Posted. Can't Update." VIEW-AS ALERT-BOX ERROR.
   RUN dispatch ('disable-fields').
   RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
   RUN undo-update IN WIDGET-HANDLE(char-hdl).
   RETURN ERROR.
END.
{&methods/lValidateError.i NO}
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateRouting V-table-Win 
PROCEDURE updateRouting :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {touch/updateRouting.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-sch-machine V-table-Win 
PROCEDURE valid-sch-machine :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER buf-mach FOR mach.

  {methods/lValidateError.i YES}
  lv-mach-list = "".

  FIND FIRST mach NO-LOCK WHERE mach.company = gcompany
                            AND mach.m-code = machtran.machine:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-ERROR.

  IF mach.sch-m-code <> "" THEN
    FOR EACH buf-mach FIELDS(m-code) WHERE
        buf-mach.company EQ mach.company AND
        buf-mach.loc     EQ mach.loc AND
        buf-mach.sch-m-code = mach.sch-m-code
        NO-LOCK:
        lv-mach-list = lv-mach-list + buf-mach.m-code + ",".
     END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-date-time V-table-Win 
PROCEDURE validate-date-time :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
  /* task# 10110517 allow duplicate time if gang jobs is yes*/
  DEF BUFFER bf-mach FOR mach.

  FIND FIRST bf-mach WHERE
       bf-mach.company EQ gcompany AND
       bf-mach.m-code EQ machtran.machine:SCREEN-VALUE IN FRAME {&FRAME-NAME}
       NO-LOCK NO-ERROR.

  IF AVAIL bf-mach AND bf-mach.gang-jobs THEN RETURN.

  /* task# 10100511  validate duplicate time */
  DEF BUFFER bf-machtran FOR machtran.
  DEF VAR v-start-time AS INT NO-UNDO.
  DEF VAR v-end-time AS INT NO-UNDO.
  DEF VAR v-start-date AS DATE NO-UNDO.
  DEF VAR v-end-date AS DATE NO-UNDO.
  DEF VAR v-start-date-time AS CHAR NO-UNDO.
  DEF VAR v-end-date-time AS CHAR NO-UNDO.
  DEF VAR v-bf-machtran-start-date-time AS CHAR NO-UNDO.
  DEF VAR v-bf-machtran-end-date-time AS CHAR NO-UNDO.

  DEF VAR v-index AS INT NO-UNDO.
  DEF VAR v-start AS INT INIT 1 NO-UNDO.

  DEF VAR ampm AS INT INIT 43200 NO-UNDO.
  DEF VAR v-valid AS LOG INIT TRUE NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN START_hour START_minute START_ampm
         end_hour end_minute end_ampm.
  END.
  IF start_ampm = "AM" AND int(START_hour) = 12 THEN START_hour = "".
  IF end_ampm = "AM" AND int(end_hour) = 12 THEN end_hour = "".

  ASSIGN v-start-time = INT(START_hour) * 3600 + int(start_minute) * 60 +
                        IF start_ampm = "PM" AND INT(start_hour) <> 12 THEN ampm ELSE 0
         v-end-time = INT(end_hour) * 3600 + int(end_minute) * 60 +
                        IF end_ampm = "PM" AND INT(END_hour) <> 12 THEN ampm ELSE 0
         v-start-date = date(machtran.start_date:SCREEN-VALUE)
         v-end-date = date(machtran.end_date:SCREEN-VALUE)
         v-start-date-time = STRING(YEAR(v-start-date),"9999") +
                             STRING(MONTH(v-start-date),"99")  +
                             STRING(DAY(v-start-date),"99")    +
                             STRING(v-start-time,"99999")
         v-end-date-time = STRING(YEAR(v-end-date),"9999") +
                           STRING(MONTH(v-end-date),"99")  +
                           STRING(DAY(v-end-date),"99")    +
                           STRING(v-end-time,"99999").

  RUN valid-sch-machine.

  EMPTY TEMP-TABLE tt-mach.

  CREATE tt-mach.
  ASSIGN tt-mach.machine = bf-mach.m-code.
  RELEASE tt-mach.

  DO v-index = 1 TO LENGTH(lv-mach-list):

     IF SUBSTRING(lv-mach-list,v-index,1) EQ "," AND
        bf-mach.m-code NE SUBSTRING(lv-mach-list,v-start,v-index - v-start) THEN
        DO:
           CREATE tt-mach.
           ASSIGN tt-mach.machine = SUBSTRING(lv-mach-list,v-start,v-index - v-start)
                  v-start = v-index + 1.
           RELEASE tt-mach.
        END.
  END.

  FOR EACH tt-mach,
      EACH bf-machtran FIELDS(START_date START_time END_date END_time
           charge_code job_number job_sub FORM_number BLANK_number) NO-LOCK
      WHERE bf-machtran.company  EQ gcompany
        AND bf-machtran.machine EQ tt-mach.machine
        AND bf-machtran.end_date NE ?
        AND bf-machtran.END_date GE v-start-date
        AND (v-end-date EQ ? OR
             (ROWID(bf-machtran)  NE ROWID(machtran)))
      USE-INDEX menddate:

       ASSIGN
          v-bf-machtran-start-date-time = STRING(YEAR(bf-machtran.start_date),"9999") +
                                          STRING(MONTH(bf-machtran.start_date),"99")  +
                                          STRING(DAY(bf-machtran.start_date),"99")    +
                                          STRING(bf-machtran.start_time,"99999")
          v-bf-machtran-end-date-time = STRING(YEAR(bf-machtran.end_date),"9999") +
                                        STRING(MONTH(bf-machtran.end_date),"99")  +
                                        STRING(DAY(bf-machtran.end_date),"99") +
                                        STRING(bf-machtran.end_time,"99999").

      IF v-end-date NE ? THEN
      DO:
         IF v-start-date-time EQ v-bf-machtran-start-date-time THEN
            v-valid = FALSE.
         ELSE IF v-start-date-time GT v-bf-machtran-start-date-time THEN
         DO:
             IF v-start-date-time LT v-bf-machtran-end-date-time THEN
                v-valid = FALSE.
         END.
         ELSE IF v-bf-machtran-start-date-time LT v-end-date-time THEN
             v-valid = FALSE.
      END.
      ELSE /*v-end-date eq ?*/
         IF v-bf-machtran-start-date-time LE v-start-date-time AND
            v-start-date-time LT v-bf-machtran-end-date-time THEN
            v-valid = FALSE.

      IF NOT v-valid THEN DO:
         MESSAGE "Machine transaction " bf-machtran.charge_code  "exists for job " bf-machtran.job_number
                 "-" bf-machtran.job_sub " form:" string(bf-machtran.FORM_number)
                 " blank:" STRING(bf-machtran.BLANK_number) bf-machtran.START_date
                 string(bf-machtran.START_time,"hh:mm") "-" bf-machtran.END_date
                 string(bf-machtran.end_time,"hh:mm")
                 SKIP
                 "Validate operation's start date and time."
                VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO END_hour IN FRAME {&FRAME-NAME}.
         RETURN error.
      END.
  END. /*end each tt-mach*/

  /*
  FIND FIRST bf-machtran WHERE bf-machtran.company = gcompany
                           AND (bf-machtran.machine = machtran.machine:SCREEN-VALUE IN FRAME {&FRAME-NAME} OR
                               LOOKUP(bf-machtran.machine,lv-mach-list) > 0 ) 
                           AND bf-machtran.START_date = date(machtran.START_date:SCREEN-VALUE)
                           AND bf-machtran.end_date = date(machtran.end_date:SCREEN-VALUE)
                           AND bf-machtran.START_time <= v-start-time
                           AND bf-machtran.END_time > v-start-time
                           AND (RECID(bf-machtran) <> recid(machtran) OR adm-new-record)
                           NO-LOCK NO-ERROR.
  IF NOT AVAIL bf-machtran THEN
     FIND FIRST bf-machtran WHERE bf-machtran.company = gcompany
                              AND (bf-machtran.machine = machtran.machine:SCREEN-VALUE IN FRAME {&FRAME-NAME} OR
                                  LOOKUP(bf-machtran.machine,lv-mach-list) > 0 ) 
                              AND bf-machtran.START_date = date(machtran.START_date:SCREEN-VALUE)
                              AND bf-machtran.end_date = date(machtran.end_date:SCREEN-VALUE)
                              AND bf-machtran.START_time < v-end-time
                              AND bf-machtran.END_time >= v-end-time
                              AND (RECID(bf-machtran) <> recid(machtran) OR adm-new-record)
                              NO-LOCK NO-ERROR.
  IF NOT AVAIL bf-machtran THEN
     FIND FIRST bf-machtran WHERE bf-machtran.company = gcompany
                           AND (bf-machtran.machine = machtran.machine:SCREEN-VALUE IN FRAME {&FRAME-NAME} OR
                                 LOOKUP(bf-machtran.machine,lv-mach-list) > 0 )
                           AND bf-machtran.START_date >= date(machtran.START_date:SCREEN-VALUE)
                           AND bf-machtran.end_date <= date(machtran.end_date:SCREEN-VALUE)
                           AND bf-machtran.START_time >= v-start-time
                           AND bf-machtran.END_time <= v-end-time
                           AND (RECID(bf-machtran) <> recid(machtran) OR adm-new-record)
                           NO-LOCK NO-ERROR.

  IF AVAIL bf-machtran THEN DO: */

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-date-time V-table-Win 
PROCEDURE check-date-time :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE istarttime AS INTEGER NO-UNDO.
  DEFINE VARIABLE iendtime AS INTEGER NO-UNDO.
  DEFINE VARIABLE dtstartdate AS DATE NO-UNDO.
  DEFINE VARIABLE dtenddate AS DATE NO-UNDO.
  DEFINE VARIABLE itotaltime AS INTEGER NO-UNDO.
  DEFINE VARIABLE iindex AS INTEGER NO-UNDO.
  DEFINE VARIABLE istart AS INTEGER INIT 1 NO-UNDO.
  DEFINE VARIABLE iampm AS INTEGER INIT 43200 NO-UNDO.
  DEFINE VARIABLE lvalid AS LOGICAL INIT TRUE NO-UNDO.
  {&methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN START_hour START_minute START_ampm
         end_hour end_minute end_ampm.
  END.
  IF start_ampm = "AM" AND int(START_hour) = 12 THEN START_hour = "".
  IF end_ampm = "AM" AND int(end_hour) = 12 THEN end_hour = "".

  ASSIGN istarttime = INT(START_hour) * 3600 + int(start_minute) * 60 +
                        IF start_ampm = "PM" AND INT(start_hour) <> 12 THEN iampm ELSE 0
         iendtime = INT(end_hour) * 3600 + int(end_minute) * 60 +
                        IF end_ampm = "PM" AND INT(END_hour) <> 12 THEN iampm ELSE 0
         dtstartdate = date(machtran.start_date:SCREEN-VALUE)
         dtenddate = date(machtran.end_date:SCREEN-VALUE) .

  IF dtstartdate = dtenddate THEN
      itotaltime = iendtime - istarttime .
  ELSE
      itotaltime = (86400 - istarttime) +
                     (dtenddate - dtstartdate - 1) * 86400 +
                      iendtime.

   IF itotaltime <= 0 THEN DO:
       MESSAGE "Stop time must be after Start time..."
           VIEW-AS ALERT-BOX INFO.
       APPLY "entry" TO END_hour IN FRAME {&FRAME-NAME}.
       RETURN error.
   END.
   {&methods/lValidateError.i NO}
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
