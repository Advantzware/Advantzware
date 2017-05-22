&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: addon\touch\r-empts.w

  Description: Time Sheet

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


{methods/defines/hndldefs.i}
{methods/prgsecur.i} 

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

DEFINE NEW SHARED VARIABLE cocode AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE locode AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE LvWeekending AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE LvEmpNum AS CHAR NO-UNDO.
DEFINE NEW SHARED VARIABLE LvTSFolder AS CHAR NO-UNDO.
DEFINE BUFFER buf-employee FOR employee.
DEFINE VARIABLE CurrDir AS CHAR NO-UNDO.

DEFINE VARIABLE Hdl-TS-Date AS HANDLE EXTENT 7 NO-UNDO.
DEFINE VARIABLE Hdl-TimeIn AS HANDLE EXTENT 7 NO-UNDO.
DEFINE VARIABLE Hdl-TimeOut AS HANDLE EXTENT 7 NO-UNDO.
DEFINE VARIABLE Hdl-TimeIn-PL AS HANDLE EXTENT 7 NO-UNDO.
DEFINE VARIABLE Hdl-TimeOut-PL AS HANDLE EXTENT 7 NO-UNDO.
DEFINE VARIABLE Hdl-WrkHrs AS HANDLE EXTENT 7 NO-UNDO.
DEFINE VARIABLE Hdl-OTHrs AS HANDLE EXTENT 7 NO-UNDO.
DEFINE VARIABLE Hdl-DTHrs AS HANDLE EXTENT 7 NO-UNDO.
DEFINE VARIABLE Hdl-MHrs AS HANDLE EXTENT 7 NO-UNDO.
DEFINE VARIABLE Hdl-VACHrs AS HANDLE EXTENT 7 NO-UNDO.
DEFINE NEW SHARED TEMP-TABLE tt-tsrep
    FIELD tt-date AS DATE
    FIELD tt-time-in1 AS CHAR 
    FIELD tt-time-out1 AS CHAR
    FIELD tt-time-in2 AS CHAR
    FIELD tt-time-out2 AS CHAR
    FIELD tt-wrk-hrs AS CHAR
    FIELD tt-ot-hrs AS CHAR
    FIELD tt-dt-hrs AS CHAR
INDEX pi-tsrep tt-date.

DEFINE NEW SHARED TEMP-TABLE tt-note NO-UNDO
  FIELD employee LIKE emplogin.employee
  FIELD rec_key LIKE nosweat.notes.rec_key
  FIELD note_date LIKE nosweat.notes.note_date
  FIELD note_title LIKE nosweat.notes.note_title
  FIELD note_text LIKE nosweat.notes.note_text
  FIELD note_src AS CHARACTER.

DEF TEMP-TABLE tt-emp
    FIELD emp-name AS CHAR
    FIELD employee AS CHAR
    INDEX employee employee.

DEF VAR j AS INT NO-UNDO.
DEF BUFFER b-employee FOR employee.

ASSIGN
 cocode = gcompany
 locode = gloc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-9 RECT-10 Lvemployee Lvpassword ~
BtnLogin BtnReport WeekendingDate LvEmployeeList TS-MHrs-1 TS-VacHrs-1 ~
TS-MHrs-2 TS-VacHrs-2 TS-MHrs-3 TS-VacHrs-3 TS-MHrs-4 TS-VacHrs-4 TS-MHrs-5 ~
TS-VacHrs-5 TS-MHrs-6 TS-VacHrs-6 TS-MHrs-7 TS-VacHrs-7 LvNotes 
&Scoped-Define DISPLAYED-OBJECTS Lvemployee Lvpassword WeekendingDate ~
LvEmployeeList LvStatus TS-Date-1 TS-TimeIn-1 TS-TimeOut-1 TS-TimeIn-PL-1 ~
TS-TimeOut-PL-1 TS-WrkHrs-1 TS-OTHrs-1 TS-DTHrs-1 TS-MHrs-1 TS-VacHrs-1 ~
TS-Date-2 TS-TimeIn-2 TS-TimeOut-2 TS-TimeIn-PL-2 TS-TimeOut-PL-2 ~
TS-WrkHrs-2 TS-OTHrs-2 TS-DTHrs-2 TS-MHrs-2 TS-VacHrs-2 TS-Date-3 ~
TS-TimeIn-3 TS-Timeout-3 TS-TimeIn-PL-3 TS-TimeOut-PL-3 TS-WrkHrs-3 ~
TS-OTHrs-3 TS-DTHrs-3 TS-MHrs-3 TS-VacHrs-3 TS-Date-4 TS-TimeIn-4 ~
TS-Timeout-4 TS-TimeIn-PL-4 TS-TimeOut-PL-4 TS-WrkHrs-4 TS-OTHrs-4 ~
TS-DTHrs-4 TS-MHrs-4 TS-VacHrs-4 TS-Date-5 TS-TimeIn-5 TS-Timeout-5 ~
TS-TimeIn-PL-5 TS-TimeOut-PL-5 TS-WrkHrs-5 TS-OTHrs-5 TS-DTHrs-5 TS-MHrs-5 ~
TS-VacHrs-5 TS-Date-6 TS-TimeIn-6 TS-Timeout-6 TS-TimeIn-PL-6 ~
TS-TimeOut-PL-6 TS-WrkHrs-6 TS-OTHrs-6 TS-DtHrs-6 TS-MHrs-6 TS-VacHrs-6 ~
TS-Date-7 TS-TimeIn-7 TS-Timeout-7 TS-TimeIn-PL-7 TS-TimeOut-PL-7 ~
TS-WrkHrs-7 TS-OTHrs-7 TS-DTHrs-7 TS-MHrs-7 TS-VacHrs-7 TS-WrkHrs-tot ~
TS-OTHrs-tot TS-DTHrs-tot TS-MHrs-tot TS-VacHrs-tot LvNotes 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 TS-Date-1 TS-TimeIn-1 TS-TimeOut-1 TS-TimeIn-PL-1 ~
TS-TimeOut-PL-1 TS-WrkHrs-1 TS-OTHrs-1 TS-DTHrs-1 TS-MHrs-1 TS-VacHrs-1 ~
TS-Date-2 TS-TimeIn-2 TS-TimeOut-2 TS-TimeIn-PL-2 TS-TimeOut-PL-2 ~
TS-WrkHrs-2 TS-OTHrs-2 TS-DTHrs-2 TS-MHrs-2 TS-VacHrs-2 TS-Date-3 ~
TS-TimeIn-3 TS-Timeout-3 TS-TimeIn-PL-3 TS-TimeOut-PL-3 TS-WrkHrs-3 ~
TS-OTHrs-3 TS-DTHrs-3 TS-MHrs-3 TS-VacHrs-3 TS-Date-4 TS-TimeIn-4 ~
TS-Timeout-4 TS-TimeIn-PL-4 TS-TimeOut-PL-4 TS-WrkHrs-4 TS-OTHrs-4 ~
TS-DTHrs-4 TS-MHrs-4 TS-VacHrs-4 TS-Date-5 TS-TimeIn-5 TS-Timeout-5 ~
TS-TimeIn-PL-5 TS-TimeOut-PL-5 TS-WrkHrs-5 TS-OTHrs-5 TS-DTHrs-5 TS-MHrs-5 ~
TS-VacHrs-5 TS-Date-6 TS-TimeIn-6 TS-Timeout-6 TS-TimeIn-PL-6 ~
TS-TimeOut-PL-6 TS-WrkHrs-6 TS-OTHrs-6 TS-DtHrs-6 TS-MHrs-6 TS-VacHrs-6 ~
TS-Date-7 TS-TimeIn-7 TS-Timeout-7 TS-TimeIn-PL-7 TS-TimeOut-PL-7 ~
TS-WrkHrs-7 TS-OTHrs-7 TS-DTHrs-7 TS-MHrs-7 TS-VacHrs-7 TS-WrkHrs-tot ~
TS-OTHrs-tot TS-DTHrs-tot TS-MHrs-tot TS-VacHrs-tot 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnApprove 
     LABEL "Approve" 
     SIZE 15 BY 1.19
     FONT 6.

DEFINE BUTTON BtnDecline 
     LABEL "Delete Timesheet" 
     SIZE 23 BY 1.19
     FONT 6.

DEFINE BUTTON BtnLogin 
     LABEL "Login" 
     SIZE 15 BY 1.14
     FONT 6.

DEFINE BUTTON BtnReport 
     LABEL "Excel Report" 
     SIZE 23 BY 1.14
     FONT 6.

DEFINE BUTTON BtnTS 
     LABEL "Submit Time Sheet" 
     SIZE 27 BY 1.14
     FONT 6.

DEFINE VARIABLE LvEmployeeList AS CHARACTER FORMAT "X(256)":U 
     LABEL "Submit/Approve Time Sheet for" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE LvNotes AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 125 BY 3.1 NO-UNDO.

DEFINE VARIABLE Lvemployee AS CHARACTER FORMAT "X(5)" 
     LABEL "Employee" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.

DEFINE VARIABLE Lvpassword AS CHARACTER FORMAT "X(256)":U 
     LABEL "Password" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE LvStatus AS CHARACTER FORMAT "X(256)":U 
     LABEL "Status" 
     VIEW-AS FILL-IN 
     SIZE 89 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE TS-Date-1 AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-Date-2 AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-Date-3 AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-Date-4 AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-Date-5 AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-Date-6 AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-Date-7 AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-DTHrs-1 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-DTHrs-2 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-DTHrs-3 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-DTHrs-4 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-DTHrs-5 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-DtHrs-6 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-DTHrs-7 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-DTHrs-tot AS DECIMAL FORMAT ">99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 4 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-MHrs-1 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-MHrs-2 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-MHrs-3 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-MHrs-4 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-MHrs-5 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-MHrs-6 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-MHrs-7 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-MHrs-tot AS DECIMAL FORMAT ">99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 4 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-OTHrs-1 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-OTHrs-2 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-OTHrs-3 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-OTHrs-4 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-OTHrs-5 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-OTHrs-6 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-OTHrs-7 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-OTHrs-tot AS DECIMAL FORMAT ">99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 4 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-TimeIn-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-TimeIn-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-TimeIn-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-TimeIn-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-TimeIn-5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-TimeIn-6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-TimeIn-7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-TimeIn-PL-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-TimeIn-PL-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-TimeIn-PL-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-TimeIn-PL-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-TimeIn-PL-5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-TimeIn-PL-6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-TimeIn-PL-7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-TimeOut-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-TimeOut-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-Timeout-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-Timeout-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-Timeout-5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-Timeout-6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-Timeout-7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-TimeOut-PL-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-TimeOut-PL-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-TimeOut-PL-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-TimeOut-PL-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-TimeOut-PL-5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-TimeOut-PL-6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-TimeOut-PL-7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-VacHrs-1 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-VacHrs-2 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-VacHrs-3 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-VacHrs-4 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-VacHrs-5 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-VacHrs-6 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-VacHrs-7 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-VacHrs-tot AS DECIMAL FORMAT ">99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 4 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-WrkHrs-1 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-WrkHrs-2 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-WrkHrs-3 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-WrkHrs-4 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-WrkHrs-5 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-WrkHrs-6 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-WrkHrs-7 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TS-WrkHrs-tot AS DECIMAL FORMAT ">99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 4 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WeekendingDate AS DATE FORMAT "99/99/9999" 
     LABEL "Week Ending" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 14 .

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 133 BY 14.76.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL 
     SIZE 133 BY 6.19.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 133 BY 1.67
     BGCOLOR 18 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Lvemployee AT ROW 2.19 COL 39 COLON-ALIGNED HELP
          "Enter Beginning Employee"
     Lvpassword AT ROW 2.19 COL 65 COLON-ALIGNED BLANK 
     BtnLogin AT ROW 2.19 COL 85
     BtnReport AT ROW 2.19 COL 107
     WeekendingDate AT ROW 3.62 COL 39 COLON-ALIGNED HELP
          "Enter Beginning Employee"
     BtnApprove AT ROW 4.57 COL 85
     BtnDecline AT ROW 4.57 COL 107
     LvEmployeeList AT ROW 4.81 COL 39 COLON-ALIGNED
     LvStatus AT ROW 6 COL 39 COLON-ALIGNED
     TS-Date-1 AT ROW 9.1 COL 5 COLON-ALIGNED NO-LABEL
     TS-TimeIn-1 AT ROW 9.1 COL 22 COLON-ALIGNED NO-LABEL
     TS-TimeOut-1 AT ROW 9.1 COL 34 COLON-ALIGNED NO-LABEL
     TS-TimeIn-PL-1 AT ROW 9.1 COL 49 COLON-ALIGNED NO-LABEL
     TS-TimeOut-PL-1 AT ROW 9.1 COL 61 COLON-ALIGNED NO-LABEL
     TS-WrkHrs-1 AT ROW 9.1 COL 75 COLON-ALIGNED NO-LABEL
     TS-OTHrs-1 AT ROW 9.1 COL 87 COLON-ALIGNED NO-LABEL
     TS-DTHrs-1 AT ROW 9.1 COL 99 COLON-ALIGNED NO-LABEL
     TS-MHrs-1 AT ROW 9.1 COL 111 COLON-ALIGNED NO-LABEL
     TS-VacHrs-1 AT ROW 9.1 COL 123 COLON-ALIGNED NO-LABEL
     TS-Date-2 AT ROW 10.29 COL 5 COLON-ALIGNED NO-LABEL
     TS-TimeIn-2 AT ROW 10.29 COL 22 COLON-ALIGNED NO-LABEL
     TS-TimeOut-2 AT ROW 10.29 COL 34 COLON-ALIGNED NO-LABEL
     TS-TimeIn-PL-2 AT ROW 10.29 COL 49 COLON-ALIGNED NO-LABEL
     TS-TimeOut-PL-2 AT ROW 10.29 COL 61 COLON-ALIGNED NO-LABEL
     TS-WrkHrs-2 AT ROW 10.29 COL 75 COLON-ALIGNED NO-LABEL
     TS-OTHrs-2 AT ROW 10.29 COL 87 COLON-ALIGNED NO-LABEL
     TS-DTHrs-2 AT ROW 10.29 COL 99 COLON-ALIGNED NO-LABEL
     TS-MHrs-2 AT ROW 10.29 COL 111 COLON-ALIGNED NO-LABEL
     TS-VacHrs-2 AT ROW 10.29 COL 123 COLON-ALIGNED NO-LABEL
     TS-Date-3 AT ROW 11.48 COL 5 COLON-ALIGNED NO-LABEL
     TS-TimeIn-3 AT ROW 11.48 COL 22 COLON-ALIGNED NO-LABEL
     TS-Timeout-3 AT ROW 11.48 COL 34 COLON-ALIGNED NO-LABEL
     TS-TimeIn-PL-3 AT ROW 11.48 COL 49 COLON-ALIGNED NO-LABEL
     TS-TimeOut-PL-3 AT ROW 11.48 COL 61 COLON-ALIGNED NO-LABEL
     TS-WrkHrs-3 AT ROW 11.48 COL 75 COLON-ALIGNED NO-LABEL
     TS-OTHrs-3 AT ROW 11.48 COL 87 COLON-ALIGNED NO-LABEL
     TS-DTHrs-3 AT ROW 11.48 COL 99 COLON-ALIGNED NO-LABEL
     TS-MHrs-3 AT ROW 11.48 COL 111 COLON-ALIGNED NO-LABEL
     TS-VacHrs-3 AT ROW 11.48 COL 123 COLON-ALIGNED NO-LABEL
     TS-Date-4 AT ROW 12.67 COL 5 COLON-ALIGNED NO-LABEL
     TS-TimeIn-4 AT ROW 12.67 COL 22 COLON-ALIGNED NO-LABEL
     TS-Timeout-4 AT ROW 12.67 COL 34 COLON-ALIGNED NO-LABEL
     TS-TimeIn-PL-4 AT ROW 12.67 COL 49 COLON-ALIGNED NO-LABEL
     TS-TimeOut-PL-4 AT ROW 12.67 COL 61 COLON-ALIGNED NO-LABEL
     TS-WrkHrs-4 AT ROW 12.67 COL 75 COLON-ALIGNED NO-LABEL
     TS-OTHrs-4 AT ROW 12.67 COL 87 COLON-ALIGNED NO-LABEL
     TS-DTHrs-4 AT ROW 12.67 COL 99 COLON-ALIGNED NO-LABEL
     TS-MHrs-4 AT ROW 12.67 COL 111 COLON-ALIGNED NO-LABEL
     TS-VacHrs-4 AT ROW 12.67 COL 123 COLON-ALIGNED NO-LABEL
     TS-Date-5 AT ROW 13.86 COL 5 COLON-ALIGNED NO-LABEL
     TS-TimeIn-5 AT ROW 13.86 COL 22 COLON-ALIGNED NO-LABEL
     TS-Timeout-5 AT ROW 13.86 COL 34 COLON-ALIGNED NO-LABEL
     TS-TimeIn-PL-5 AT ROW 13.86 COL 49 COLON-ALIGNED NO-LABEL
     TS-TimeOut-PL-5 AT ROW 13.86 COL 61 COLON-ALIGNED NO-LABEL
     TS-WrkHrs-5 AT ROW 13.86 COL 75 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 140.8 BY 23.67
         BGCOLOR 8 .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     TS-OTHrs-5 AT ROW 13.86 COL 87 COLON-ALIGNED NO-LABEL
     TS-DTHrs-5 AT ROW 13.86 COL 99 COLON-ALIGNED NO-LABEL
     TS-MHrs-5 AT ROW 13.86 COL 111 COLON-ALIGNED NO-LABEL
     TS-VacHrs-5 AT ROW 13.86 COL 123 COLON-ALIGNED NO-LABEL
     TS-Date-6 AT ROW 15.05 COL 5 COLON-ALIGNED NO-LABEL
     TS-TimeIn-6 AT ROW 15.05 COL 22 COLON-ALIGNED NO-LABEL
     TS-Timeout-6 AT ROW 15.05 COL 34 COLON-ALIGNED NO-LABEL
     TS-TimeIn-PL-6 AT ROW 15.05 COL 49 COLON-ALIGNED NO-LABEL
     TS-TimeOut-PL-6 AT ROW 15.05 COL 61 COLON-ALIGNED NO-LABEL
     TS-WrkHrs-6 AT ROW 15.05 COL 75 COLON-ALIGNED NO-LABEL
     TS-OTHrs-6 AT ROW 15.05 COL 87 COLON-ALIGNED NO-LABEL
     TS-DtHrs-6 AT ROW 15.05 COL 99 COLON-ALIGNED NO-LABEL
     TS-MHrs-6 AT ROW 15.05 COL 111 COLON-ALIGNED NO-LABEL
     TS-VacHrs-6 AT ROW 15.05 COL 123 COLON-ALIGNED NO-LABEL
     TS-Date-7 AT ROW 16.24 COL 5 COLON-ALIGNED NO-LABEL
     TS-TimeIn-7 AT ROW 16.24 COL 22 COLON-ALIGNED NO-LABEL
     TS-Timeout-7 AT ROW 16.24 COL 34 COLON-ALIGNED NO-LABEL
     TS-TimeIn-PL-7 AT ROW 16.24 COL 49 COLON-ALIGNED NO-LABEL
     TS-TimeOut-PL-7 AT ROW 16.24 COL 61 COLON-ALIGNED NO-LABEL
     TS-WrkHrs-7 AT ROW 16.24 COL 75 COLON-ALIGNED NO-LABEL
     TS-OTHrs-7 AT ROW 16.24 COL 87 COLON-ALIGNED NO-LABEL
     TS-DTHrs-7 AT ROW 16.24 COL 99 COLON-ALIGNED NO-LABEL
     TS-MHrs-7 AT ROW 16.24 COL 111 COLON-ALIGNED NO-LABEL
     TS-VacHrs-7 AT ROW 16.24 COL 123 COLON-ALIGNED NO-LABEL
     TS-WrkHrs-tot AT ROW 17.43 COL 75 COLON-ALIGNED NO-LABEL
     TS-OTHrs-tot AT ROW 17.43 COL 87 COLON-ALIGNED NO-LABEL
     TS-DTHrs-tot AT ROW 17.43 COL 99 COLON-ALIGNED NO-LABEL
     TS-MHrs-tot AT ROW 17.43 COL 111 COLON-ALIGNED NO-LABEL
     TS-VacHrs-tot AT ROW 17.43 COL 123 COLON-ALIGNED NO-LABEL
     LvNotes AT ROW 19.1 COL 11 NO-LABEL
     BtnTS AT ROW 22.91 COL 109
     "L" VIEW-AS TEXT
          SIZE 2 BY .62 AT ROW 10.52 COL 48
          FGCOLOR 4 FONT 6
     "Time Out" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 8.14 COL 62
          FONT 6
     "T" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 20.29 COL 8
          FONT 6
     "Time Out" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 8.14 COL 36
          FONT 6
     "Wrk Hrs" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 8.14 COL 77
          FONT 6
     "Totals:" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 17.67 COL 67
          FGCOLOR 4 FONT 6
     "DT" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 8.14 COL 101
          FONT 6
     "Date" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 8.14 COL 7
          FONT 6
     "S" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 21.71 COL 8
          FONT 6
     "OT" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 8.14 COL 89
          FONT 6
     "Time In" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 8.14 COL 51
          FONT 6
     "U" VIEW-AS TEXT
          SIZE 2 BY .62 AT ROW 11.71 COL 48
          FGCOLOR 4 FONT 6
     "Time In" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 8.14 COL 24
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 140.8 BY 23.67
         BGCOLOR 8 .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     "E" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 21 COL 8
          FONT 6
     "PTO/Vac" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 8.14 COL 124
          FONT 6
     "H" VIEW-AS TEXT
          SIZE 2 BY .62 AT ROW 15.29 COL 48
          FGCOLOR 4 FONT 6
     "Makeup" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 8.14 COL 113
          FONT 6
     "O" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 19.57 COL 8
          FONT 6
     "C" VIEW-AS TEXT
          SIZE 2 BY .62 AT ROW 14.1 COL 48
          FGCOLOR 4 FONT 6
     "N" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 18.86 COL 8
          FONT 6
     "N" VIEW-AS TEXT
          SIZE 2 BY .62 AT ROW 12.91 COL 48
          FGCOLOR 4 FONT 6
     RECT-7 AT ROW 1 COL 4
     RECT-9 AT ROW 22.67 COL 4
     RECT-10 AT ROW 7.67 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 140.8 BY 23.67
         BGCOLOR 8 .


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
         TITLE              = "Time Sheet"
         HEIGHT             = 23.67
         WIDTH              = 140.8
         MAX-HEIGHT         = 23.67
         MAX-WIDTH          = 158
         VIRTUAL-HEIGHT     = 23.67
         VIRTUAL-WIDTH      = 158
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
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
                                                                        */
/* SETTINGS FOR BUTTON BtnApprove IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BtnDecline IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BtnTS IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Lvemployee:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "save".

ASSIGN 
       LvNotes:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN LvStatus IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TS-Date-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-Date-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-Date-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-Date-4 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-Date-5 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-Date-6 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-Date-7 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-DTHrs-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-DTHrs-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-DTHrs-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-DTHrs-4 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-DTHrs-5 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-DtHrs-6 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-DTHrs-7 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-DTHrs-tot IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-MHrs-1 IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN TS-MHrs-2 IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN TS-MHrs-3 IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN TS-MHrs-4 IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN TS-MHrs-5 IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN TS-MHrs-6 IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN TS-MHrs-7 IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN TS-MHrs-tot IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-OTHrs-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-OTHrs-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-OTHrs-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-OTHrs-4 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-OTHrs-5 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-OTHrs-6 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-OTHrs-7 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-OTHrs-tot IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-TimeIn-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-TimeIn-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-TimeIn-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-TimeIn-4 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-TimeIn-5 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-TimeIn-6 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-TimeIn-7 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-TimeIn-PL-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-TimeIn-PL-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-TimeIn-PL-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-TimeIn-PL-4 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-TimeIn-PL-5 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-TimeIn-PL-6 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-TimeIn-PL-7 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-TimeOut-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-TimeOut-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-Timeout-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-Timeout-4 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-Timeout-5 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-Timeout-6 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-Timeout-7 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-TimeOut-PL-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-TimeOut-PL-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-TimeOut-PL-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-TimeOut-PL-4 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-TimeOut-PL-5 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-TimeOut-PL-6 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-TimeOut-PL-7 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-VacHrs-1 IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN TS-VacHrs-2 IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN TS-VacHrs-3 IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN TS-VacHrs-4 IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN TS-VacHrs-5 IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN TS-VacHrs-6 IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN TS-VacHrs-7 IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN TS-VacHrs-tot IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-WrkHrs-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-WrkHrs-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-WrkHrs-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-WrkHrs-4 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-WrkHrs-5 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-WrkHrs-6 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-WrkHrs-7 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN TS-WrkHrs-tot IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       WeekendingDate:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "save".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Time Sheet */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Time Sheet */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnApprove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnApprove C-Win
ON CHOOSE OF BtnApprove IN FRAME DEFAULT-FRAME /* Approve */
DO:

  DO WITH FRAME {&FRAME-NAME} :

    RUN UTIL/CurrDir.p (output CurrDir).
    IF SEARCH(CurrDir + "\Signature\" + LvEmployee:screen-value + ".jpg") = ? THEN
    DO:
      MESSAGE "Signature file does not exist for " LvEmployee:screen-value + "."
          VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
    END.
    IF NOT AVAILABLE timesheet THEN
    DO:
        MESSAGE "No Time Sheet Record available." VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
    
    FIND CURRENT timesheet EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE timesheet THEN
    DO:
      ASSIGN TimeSheet.ApprovedBy = LvEmployee:screen-value
          TimeSheet.ApprovedOn = TODAY.
      RELEASE TImesheet.
      RUN PROCESS(INPUT NO).
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDecline
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDecline C-Win
ON CHOOSE OF BtnDecline IN FRAME DEFAULT-FRAME /* Delete Timesheet */
DO:
  IF AVAILABLE TimeSheet THEN
  DO WITH FRAME {&FRAME-NAME} :

    DO:
      MESSAGE "Do you really want to delete the timesheet?" VIEW-AS ALERT-BOX
       QUESTION BUTTONS YES-NO                    
      TITLE "" UPDATE choice AS LOGICAL.
      IF choice = TRUE THEN
      DO:
        FIND CURRENT TimeSheet EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE TimeSheet THEN
        DELETE TimeSheet.
        DO j = 1 TO 7 :
            ASSIGN 
              Hdl-TS-Date[j]:SCREEN-VALUE = ""
              Hdl-TimeIn[j]:SCREEN-VALUE = ""
              Hdl-TimeOut[j]:SCREEN-VALUE = ""
              Hdl-TimeIn-PL[j]:SCREEN-VALUE = ""
              Hdl-TimeOut-PL[j]:SCREEN-VALUE = ""
              Hdl-WrkHrs[j]:SCREEN-VALUE = "00.00"
              Hdl-OTHrs[j]:SCREEN-VALUE = "00.00"
              Hdl-DTHrs[j]:SCREEN-VALUE = "00.00"
              Hdl-MHrs[j]:SCREEN-VALUE = "00.00"
              Hdl-VacHrs[j]:SCREEN-VALUE = "00.00"
             .
        END.
        ASSIGN LvNotes:SCREEN-VALUE = ""
               LvStatus:SCREEN-VALUE = "".
        APPLY "ENTRY" TO WeekEndingDate.
      END.
    END.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnLogin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnLogin C-Win
ON CHOOSE OF BtnLogin IN FRAME DEFAULT-FRAME /* Login */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN   Lvemployee Lvpassword.
    IF Lvemployee = "" THEN
    DO:
        MESSAGE "Employee# can not be blank." VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
    
    IF BtnLogin:LABEL = "Login" THEN
    DO:
      FIND FIRST employee NO-LOCK WHERE
           employee.company = cocode AND
           employee.employee = LvEmployee and
           employee.passwd = LvPassword 
           NO-ERROR.

      IF AVAILABLE employee THEN
      DO:
         ASSIGN LvEmployee:SENSITIVE = FALSE
                LvPassword:SENSITIVE = FALSE
                WeekendingDate:SCREEN-VALUE = ""
                WeekendingDate:SENSITIVE = TRUE 
                BtnLogin:LABEL = "Logout"
                LvEmployee
                LvPassword. 

         IF CAN-FIND(FIRST b-employee WHERE
            b-employee.ref_no EQ employee.employee) THEN
            DO j = 1 TO 7:
               ASSIGN
                  Hdl-OTHrs[j]:SENSITIVE = YES
                  Hdl-DTHrs[j]:SENSITIVE = YES.
            END.

         APPLY "entry" TO WeekendingDate.

         EMPTY TEMP-TABLE tt-emp.

         CREATE tt-emp.
         ASSIGN LvEmployeeList:LIST-ITEMS = ""
                LvEmployeeList:LIST-ITEMS = replace(employee.FIRST_name, "," , " ") + " " +  
                                            replace(employee.last_name, "," , " ") + ","
                tt-emp.employee = employee.employee
                tt-emp.emp-name = replace(employee.FIRST_name, "," , " ") + " " +  
                                  replace(employee.last_name, "," , " ").
         RELEASE tt-emp.

         FOR EACH buf-employee NO-LOCK WHERE
             buf-employee.company EQ cocode AND
             buf-employee.ref_no = LvEmployee AND
             buf-employee.employee NE employee.employee:
             LvEmployeeList:ADD-LAST(replace(buf-employee.FIRST_name, "," , " ") + " " +  
                                     replace(buf-employee.last_name, "," , " ") ). 

             CREATE tt-emp.
             ASSIGN tt-emp.employee = buf-employee.employee
                    tt-emp.emp-name = replace(buf-employee.FIRST_name, "," , " ") + " " +  
                                      replace(buf-employee.last_name, "," , " ").
             RELEASE tt-emp.
         END.

         ASSIGN LvEmployeeList:SCREEN-VALUE = ENTRY(1, LvEmployeeList:LIST-ITEMS).
         IF WEEKDAY(TODAY) = 7 THEN
             ASSIGN WeekEndingDate:SCREEN-VALUE = STRING(TODAY + 1).
         ELSE IF WEEKDAY(TODAY) = 2 THEN
             ASSIGN WeekEndingDate:SCREEN-VALUE = STRING(TODAY - 1).
         APPLY "leave" TO   WeekendingDate.
      END.
      ELSE DO :  
        MESSAGE "Invalid Employee/Password." VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
      END.
    END.
    ELSE DO :
        ASSIGN LvEmployee:SENSITIVE = TRUE
               LvPassword:SENSITIVE = TRUE
               LvEmployee:SCREEN-VALUE = ""
               LvPassword:SCREEN-VALUE = ""
               BtnTS:SENSITIVE = FALSE
               WeekendingDate:SCREEN-VALUE = ""
               WeekendingDate:SENSITIVE = FALSE 
               BtnLogin:LABEL = "Login"
               LvEmployeeList:LIST-ITEMS = "".

         EMPTY TEMP-TABLE tt-emp.

          DO j = 1 TO 7:
            ASSIGN 
              Hdl-TS-Date[j]:SCREEN-VALUE = ""
              Hdl-TimeIn[j]:SCREEN-VALUE = ""
              Hdl-TimeOut[j]:SCREEN-VALUE = ""
              Hdl-TimeIn-PL[j]:SCREEN-VALUE = ""
              Hdl-TimeOut-PL[j]:SCREEN-VALUE = ""
              Hdl-WrkHrs[j]:SCREEN-VALUE = "00.00"
              Hdl-OTHrs[j]:SCREEN-VALUE = "00.00"
              Hdl-DTHrs[j]:SCREEN-VALUE = "00.00"
              Hdl-MHrs[j]:SCREEN-VALUE = "00.00"
              Hdl-VacHrs[j]:SCREEN-VALUE = "00.00"
              Hdl-OTHrs[j]:SENSITIVE = NO
              Hdl-DTHrs[j]:SENSITIVE = NO.
          END.
          ASSIGN
              TS-DTHrs-tot:SCREEN-VALUE = "00.00" 
              TS-MHrs-tot:SCREEN-VALUE = "00.00" 
              TS-OTHrs-tot:SCREEN-VALUE = "00.00" 
              TS-WrkHrs-tot:SCREEN-VALUE = "00.00" 
              TS-VacHrs-tot:SCREEN-VALUE = "00.00"
              LvStatus:SCREEN-VALUE = ""
              LvNotes:SCREEN-VALUE = ""
              LvEmployee
              LvPassword.
          APPLY "entry" TO LvEmployee.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnReport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnReport C-Win
ON CHOOSE OF BtnReport IN FRAME DEFAULT-FRAME /* Excel Report */
DO:
  IF AVAILABLE timesheet THEN
  RUN touch/r-timesheet.p (INPUT timesheet.weekending,
                           INPUT TIMEsheet.employee,
                           INPUT DECIMAL(Hdl-OTHrs[1]:SCREEN-VALUE),
                           INPUT DECIMAL(Hdl-OTHrs[2]:SCREEN-VALUE),
                           INPUT DECIMAL(Hdl-OTHrs[3]:SCREEN-VALUE),
                           INPUT DECIMAL(Hdl-OTHrs[4]:SCREEN-VALUE),
                           INPUT DECIMAL(Hdl-OTHrs[5]:SCREEN-VALUE),
                           INPUT DECIMAL(Hdl-OTHrs[6]:SCREEN-VALUE),
                           INPUT DECIMAL(Hdl-OTHrs[7]:SCREEN-VALUE),
                           INPUT DECIMAL(Hdl-DTHrs[1]:SCREEN-VALUE),
                           INPUT DECIMAL(Hdl-DTHrs[2]:SCREEN-VALUE),
                           INPUT DECIMAL(Hdl-DTHrs[3]:SCREEN-VALUE),
                           INPUT DECIMAL(Hdl-DTHrs[4]:SCREEN-VALUE),
                           INPUT DECIMAL(Hdl-DTHrs[5]:SCREEN-VALUE),
                           INPUT DECIMAL(Hdl-DTHrs[6]:SCREEN-VALUE),
                           INPUT DECIMAL(Hdl-DTHrs[7]:SCREEN-VALUE),
                           INPUT DECIMAL(Hdl-MHrs[1]:SCREEN-VALUE),
                           INPUT DECIMAL(Hdl-MHrs[2]:SCREEN-VALUE),
                           INPUT DECIMAL(Hdl-MHrs[3]:SCREEN-VALUE),
                           INPUT DECIMAL(Hdl-MHrs[4]:SCREEN-VALUE),
                           INPUT DECIMAL(Hdl-MHrs[5]:SCREEN-VALUE),
                           INPUT DECIMAL(Hdl-MHrs[6]:SCREEN-VALUE),
                           INPUT DECIMAL(Hdl-MHrs[7]:SCREEN-VALUE),
                           INPUT DECIMAL(Hdl-VACHrs[1]:SCREEN-VALUE),
                           INPUT DECIMAL(Hdl-VACHrs[2]:SCREEN-VALUE),
                           INPUT DECIMAL(Hdl-VACHrs[3]:SCREEN-VALUE),
                           INPUT DECIMAL(Hdl-VACHrs[4]:SCREEN-VALUE),
                           INPUT DECIMAL(Hdl-VACHrs[5]:SCREEN-VALUE),
                           INPUT DECIMAL(Hdl-VACHrs[6]:SCREEN-VALUE),
                           INPUT DECIMAL(Hdl-VACHrs[7]:SCREEN-VALUE)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnTS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnTS C-Win
ON CHOOSE OF BtnTS IN FRAME DEFAULT-FRAME /* Submit Time Sheet */
DO:
  DO WITH FRAME {&FRAME-NAME} :
  
    RELEASE employee.

    FIND FIRST tt-emp WHERE
         tt-emp.emp-name = LvEmployeeList:SCREEN-VALUE.

    IF AVAIL tt-emp THEN
       FIND FIRST employee NO-LOCK WHERE
            employee.company EQ cocode AND
            employee.employee = tt-emp.employee
            NO-ERROR.

    IF NOT AVAILABLE employee  THEN
      MESSAGE "Invalid Employee." VIEW-AS ALERT-BOX.

    /* Check if Signature File exist for an employee */
    RUN UTIL/CurrDir.p (output CurrDir).
    IF SEARCH(CurrDir + "\Signature\" + employee.employee + ".jpg") = ? THEN
    DO:
      MESSAGE "Signature file does not exist for " employee.employee + "."
          VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
    END.

    ASSIGN WeekendingDate LvEmployee .
    
    CREATE TimeSheet.
    ASSIGN 
      TimeSheet.WeekEnding = WeekEndingDate
      TimeSheet.employee  = employee.employee
      TimeSheet.SubmittedBy = LvEmployee
      TimeSheet.SubmittedOn = TODAY
      Timesheet.notes = LvNotes:SCREEN-VALUE.

    DO j = 1 TO 7 :
      ASSIGN 
          timesheet.Time_Login1[j]     = Hdl-TimeIn[j]:SCREEN-VALUE .
          timesheet.Time_Logout1[j]    = Hdl-TimeOut[j]:SCREEN-VALUE .
          timesheet.Time_Login2[j]     = Hdl-TimeIn-PL[j]:SCREEN-VALUE .
          timesheet.Time_Logout2[j]    = Hdl-TimeOut-PL[j]:SCREEN-VALUE.
          timesheet.Reg_Hrs[j]         = decimal(Hdl-WrkHrs[j]:SCREEN-VALUE ).
          timesheet.OT_Hrs[j]          = decimal(Hdl-OTHrs[j]:SCREEN-VALUE) .
          timesheet.DT_Hrs[j]        = decimal(Hdl-DTHrs[j]:SCREEN-VALUE) .
          timesheet.MakeUp_Hrs[j]      = decimal(Hdl-MHrs[j]:SCREEN-VALUE) .
          timesheet.PTO_VAC_Hrs[j]         = decimal(Hdl-VacHrs[j]:SCREEN-VALUE) .
      .

    END.
    RELEASE TImesheet.

    RUN PROCESS(INPUT YES).
  END.                   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LvEmployeeList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LvEmployeeList C-Win
ON VALUE-CHANGED OF LvEmployeeList IN FRAME DEFAULT-FRAME /* Submit/Approve Time Sheet for */
DO:
  IF lvemployeelist NE lvemployeelist:SCREEN-VALUE THEN
     APPLY "leave" TO WeekEndingDate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TS-DTHrs-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TS-DTHrs-1 C-Win
ON LEAVE OF TS-DTHrs-1 IN FRAME DEFAULT-FRAME
DO:
  DO WITH FRAME {&FRAME-NAME} :
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) - TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > .59 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

  END.
  RUN tot-dt-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TS-DTHrs-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TS-DTHrs-2 C-Win
ON LEAVE OF TS-DTHrs-2 IN FRAME DEFAULT-FRAME
DO:
  DO WITH FRAME {&FRAME-NAME} :
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) - TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > .59 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

  END.
  RUN tot-dt-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TS-DTHrs-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TS-DTHrs-3 C-Win
ON LEAVE OF TS-DTHrs-3 IN FRAME DEFAULT-FRAME
DO:
  DO WITH FRAME {&FRAME-NAME} :
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) - TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > .59 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

  END.
  RUN tot-dt-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TS-DTHrs-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TS-DTHrs-4 C-Win
ON LEAVE OF TS-DTHrs-4 IN FRAME DEFAULT-FRAME
DO:
  DO WITH FRAME {&FRAME-NAME} :
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) - TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > .59 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

  END.
  RUN tot-dt-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TS-DTHrs-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TS-DTHrs-5 C-Win
ON LEAVE OF TS-DTHrs-5 IN FRAME DEFAULT-FRAME
DO:
  DO WITH FRAME {&FRAME-NAME} :
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) - TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > .59 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

  END.
  RUN tot-dt-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TS-DtHrs-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TS-DtHrs-6 C-Win
ON LEAVE OF TS-DtHrs-6 IN FRAME DEFAULT-FRAME
DO:
  DO WITH FRAME {&FRAME-NAME} :
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) - TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > .59 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

  END.
  RUN tot-dt-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TS-DTHrs-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TS-DTHrs-7 C-Win
ON LEAVE OF TS-DTHrs-7 IN FRAME DEFAULT-FRAME
DO:
  DO WITH FRAME {&FRAME-NAME} :
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) - TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > .59 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

  END.
  RUN tot-dt-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TS-MHrs-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TS-MHrs-1 C-Win
ON LEAVE OF TS-MHrs-1 IN FRAME DEFAULT-FRAME
DO:
  DO WITH FRAME {&FRAME-NAME} :
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) - TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > .59 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

  END.
  RUN tot-m-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TS-MHrs-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TS-MHrs-2 C-Win
ON LEAVE OF TS-MHrs-2 IN FRAME DEFAULT-FRAME
DO:
    DO WITH FRAME {&FRAME-NAME} :
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) - TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > .59 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

  END.
  RUN tot-m-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TS-MHrs-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TS-MHrs-3 C-Win
ON LEAVE OF TS-MHrs-3 IN FRAME DEFAULT-FRAME
DO:
    DO WITH FRAME {&FRAME-NAME} :
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) - TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > .59 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

  END.
  RUN tot-m-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TS-MHrs-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TS-MHrs-4 C-Win
ON LEAVE OF TS-MHrs-4 IN FRAME DEFAULT-FRAME
DO:
    DO WITH FRAME {&FRAME-NAME} :
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) - TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > .59 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

  END.
  RUN tot-m-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TS-MHrs-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TS-MHrs-5 C-Win
ON LEAVE OF TS-MHrs-5 IN FRAME DEFAULT-FRAME
DO:
    DO WITH FRAME {&FRAME-NAME} :
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) - TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > .59 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

  END.
  RUN tot-m-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TS-MHrs-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TS-MHrs-6 C-Win
ON LEAVE OF TS-MHrs-6 IN FRAME DEFAULT-FRAME
DO:
    DO WITH FRAME {&FRAME-NAME} :
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) - TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > .59 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

  END.
  RUN tot-m-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TS-MHrs-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TS-MHrs-7 C-Win
ON LEAVE OF TS-MHrs-7 IN FRAME DEFAULT-FRAME
DO:
    DO WITH FRAME {&FRAME-NAME} :
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) > 24 THEN
      DO:
          MESSAGE "Invalid Hours" VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) - TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > .59 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

  END.
  RUN tot-m-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TS-OTHrs-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TS-OTHrs-1 C-Win
ON LEAVE OF TS-OTHrs-1 IN FRAME DEFAULT-FRAME
DO:
  DO WITH FRAME {&FRAME-NAME} :
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) - TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > .59 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

  END.
  RUN tot-ot-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TS-OTHrs-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TS-OTHrs-2 C-Win
ON LEAVE OF TS-OTHrs-2 IN FRAME DEFAULT-FRAME
DO:
  DO WITH FRAME {&FRAME-NAME} :
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) - TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > .59 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

  END.
  RUN tot-ot-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TS-OTHrs-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TS-OTHrs-3 C-Win
ON LEAVE OF TS-OTHrs-3 IN FRAME DEFAULT-FRAME
DO:
  DO WITH FRAME {&FRAME-NAME} :
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) - TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > .59 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

  END.
  RUN tot-ot-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TS-OTHrs-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TS-OTHrs-4 C-Win
ON LEAVE OF TS-OTHrs-4 IN FRAME DEFAULT-FRAME
DO:
  DO WITH FRAME {&FRAME-NAME} :
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) - TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > .59 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

  END.
  RUN tot-ot-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TS-OTHrs-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TS-OTHrs-5 C-Win
ON LEAVE OF TS-OTHrs-5 IN FRAME DEFAULT-FRAME
DO:
  DO WITH FRAME {&FRAME-NAME} :
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) - TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > .59 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

  END.
  RUN tot-ot-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TS-OTHrs-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TS-OTHrs-6 C-Win
ON LEAVE OF TS-OTHrs-6 IN FRAME DEFAULT-FRAME
DO:
  DO WITH FRAME {&FRAME-NAME} :
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) - TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > .59 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

  END.
  RUN tot-ot-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TS-OTHrs-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TS-OTHrs-7 C-Win
ON LEAVE OF TS-OTHrs-7 IN FRAME DEFAULT-FRAME
DO:
  DO WITH FRAME {&FRAME-NAME} :
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) - TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > .59 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

  END.
  RUN tot-ot-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TS-VacHrs-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TS-VacHrs-1 C-Win
ON LEAVE OF TS-VacHrs-1 IN FRAME DEFAULT-FRAME
DO:
    DO WITH FRAME {&FRAME-NAME} :
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) - TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > .59 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

  END.
  RUN tot-vac-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TS-VacHrs-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TS-VacHrs-2 C-Win
ON LEAVE OF TS-VacHrs-2 IN FRAME DEFAULT-FRAME
DO:
    DO WITH FRAME {&FRAME-NAME} :
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) - TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > .59 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

  END.
  RUN tot-vac-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TS-VacHrs-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TS-VacHrs-3 C-Win
ON LEAVE OF TS-VacHrs-3 IN FRAME DEFAULT-FRAME
DO:
    DO WITH FRAME {&FRAME-NAME} :
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) - TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > .59 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

  END.
  RUN tot-vac-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TS-VacHrs-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TS-VacHrs-4 C-Win
ON LEAVE OF TS-VacHrs-4 IN FRAME DEFAULT-FRAME
DO:
    DO WITH FRAME {&FRAME-NAME} :
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) - TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > .59 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

  END.
  RUN tot-vac-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TS-VacHrs-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TS-VacHrs-5 C-Win
ON LEAVE OF TS-VacHrs-5 IN FRAME DEFAULT-FRAME
DO:
    DO WITH FRAME {&FRAME-NAME} :
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) - TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > .59 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

  END.
  RUN tot-vac-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TS-VacHrs-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TS-VacHrs-6 C-Win
ON LEAVE OF TS-VacHrs-6 IN FRAME DEFAULT-FRAME
DO:
    DO WITH FRAME {&FRAME-NAME} :
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) - TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > .59 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

  END.
  RUN tot-vac-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TS-VacHrs-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TS-VacHrs-7 C-Win
ON LEAVE OF TS-VacHrs-7 IN FRAME DEFAULT-FRAME
DO:
    DO WITH FRAME {&FRAME-NAME} :
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

      IF TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > 24 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      
      IF DECIMAL({&SELF-NAME}:SCREEN-VALUE) - TRUNCATE( DECIMAL({&SELF-NAME}:SCREEN-VALUE), 0 ) > .59 THEN
      DO:
          MESSAGE "Invalid Hours." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.

  END.
  RUN tot-vac-hrs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME WeekendingDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WeekendingDate C-Win
ON LEAVE OF WeekendingDate IN FRAME DEFAULT-FRAME /* Week Ending */
DO:
  IF DATE(WeekEndingDate:SCREEN-VALUE) <> ? THEN
     RUN PROCESS(INPUT YES).
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WeekendingDate C-Win
ON RETURN OF WeekendingDate IN FRAME DEFAULT-FRAME /* Week Ending */
DO:
  RUN PROCESS(INPUT YES).
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



/*   find first sys-ctrl where                                                           */
/*   sys-ctrl.company eq cocode AND                                                      */
/*   sys-ctrl.name    eq "TIMESHEETFOL"                                                  */
/*   no-lock no-error.                                                                   */
/*                                                                                       */
/*   if not avail sys-ctrl then DO TRANSACTION:                                          */
/*       create sys-ctrl.                                                                */
/*       assign sys-ctrl.company = cocode                                                */
/*              sys-ctrl.name    = "TIMESHEETFOL"                                        */
/*              sys-ctrl.descrip = "Time Sheet Folder"                                   */
/*              sys-ctrl.log-fld = YES                                                   */
/*              sys-ctrl.char-fld = ""                                                   */
/*              sys-ctrl.int-fld = 0.                                                    */
/*   end.                                                                                */
/*   ASSIGN LvTSFolder = sys-ctrl.char-fld.                                              */
/* /*   MESSAGE sys-ctrl.char-fld VIEW-AS ALERT-BOX. */                                  */
/*   IF  sys-ctrl.char-fld = ""  THEN                                                    */
/*   DO:                                                                                 */
/*     MESSAGE "Please update N-K-1 Parameter for TimeSheetFol" VIEW-AS ALERT-BOX INFO.  */
/*     APPLY "close" TO THIS-PROCEDURE.                                                  */
/*   END.                                                                                */
  RUN INIT.
  {methods/nowait.i}
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
  DISPLAY Lvemployee Lvpassword WeekendingDate LvEmployeeList LvStatus TS-Date-1 
          TS-TimeIn-1 TS-TimeOut-1 TS-TimeIn-PL-1 TS-TimeOut-PL-1 TS-WrkHrs-1 
          TS-OTHrs-1 TS-DTHrs-1 TS-MHrs-1 TS-VacHrs-1 TS-Date-2 TS-TimeIn-2 
          TS-TimeOut-2 TS-TimeIn-PL-2 TS-TimeOut-PL-2 TS-WrkHrs-2 TS-OTHrs-2 
          TS-DTHrs-2 TS-MHrs-2 TS-VacHrs-2 TS-Date-3 TS-TimeIn-3 TS-Timeout-3 
          TS-TimeIn-PL-3 TS-TimeOut-PL-3 TS-WrkHrs-3 TS-OTHrs-3 TS-DTHrs-3 
          TS-MHrs-3 TS-VacHrs-3 TS-Date-4 TS-TimeIn-4 TS-Timeout-4 
          TS-TimeIn-PL-4 TS-TimeOut-PL-4 TS-WrkHrs-4 TS-OTHrs-4 TS-DTHrs-4 
          TS-MHrs-4 TS-VacHrs-4 TS-Date-5 TS-TimeIn-5 TS-Timeout-5 
          TS-TimeIn-PL-5 TS-TimeOut-PL-5 TS-WrkHrs-5 TS-OTHrs-5 TS-DTHrs-5 
          TS-MHrs-5 TS-VacHrs-5 TS-Date-6 TS-TimeIn-6 TS-Timeout-6 
          TS-TimeIn-PL-6 TS-TimeOut-PL-6 TS-WrkHrs-6 TS-OTHrs-6 TS-DtHrs-6 
          TS-MHrs-6 TS-VacHrs-6 TS-Date-7 TS-TimeIn-7 TS-Timeout-7 
          TS-TimeIn-PL-7 TS-TimeOut-PL-7 TS-WrkHrs-7 TS-OTHrs-7 TS-DTHrs-7 
          TS-MHrs-7 TS-VacHrs-7 TS-WrkHrs-tot TS-OTHrs-tot TS-DTHrs-tot 
          TS-MHrs-tot TS-VacHrs-tot LvNotes 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-7 RECT-9 RECT-10 Lvemployee Lvpassword BtnLogin BtnReport 
         WeekendingDate LvEmployeeList TS-MHrs-1 TS-VacHrs-1 TS-MHrs-2 
         TS-VacHrs-2 TS-MHrs-3 TS-VacHrs-3 TS-MHrs-4 TS-VacHrs-4 TS-MHrs-5 
         TS-VacHrs-5 TS-MHrs-6 TS-VacHrs-6 TS-MHrs-7 TS-VacHrs-7 LvNotes 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Init C-Win 
PROCEDURE Init :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE J AS INT NO-UNDO.
DO WITH FRAME {&FRAME-NAME} :
  ASSIGN Hdl-TS-Date[1] = TS-Date-1:HANDLE
         Hdl-TS-Date[2] = TS-Date-2:HANDLE
         Hdl-TS-Date[3] = TS-Date-3:HANDLE
         Hdl-TS-Date[4] = TS-Date-4:HANDLE
         Hdl-TS-Date[5] = TS-Date-5:HANDLE
         Hdl-TS-Date[6] = TS-Date-6:HANDLE
         Hdl-TS-Date[7] = TS-Date-7:HANDLE
         Hdl-TimeIn[1] = TS-TimeIn-1:HANDLE
         Hdl-TimeIn[2] = TS-TimeIn-2:HANDLE
         Hdl-TimeIn[3] = TS-TimeIn-3:HANDLE
         Hdl-TimeIn[4] = TS-TimeIn-4:HANDLE
         Hdl-TimeIn[5] = TS-TimeIn-5:HANDLE
         Hdl-TimeIn[6] = TS-TimeIn-6:HANDLE
         Hdl-TimeIn[7] = TS-TimeIn-7:HANDLE
         Hdl-TimeOut[1] = TS-TimeOut-1:HANDLE
         Hdl-TimeOut[2] = TS-TimeOut-2:HANDLE
         Hdl-TimeOut[3] = TS-TimeOut-3:HANDLE
         Hdl-TimeOut[4] = TS-TimeOut-4:HANDLE
         Hdl-TimeOut[5] = TS-TimeOut-5:HANDLE
         Hdl-TimeOut[6] = TS-TimeOut-6:HANDLE
         Hdl-TimeOut[7] = TS-TimeOut-7:HANDLE
         Hdl-TimeIn-PL[1] = TS-TimeIn-PL-1:HANDLE
         Hdl-TimeIn-PL[2] = TS-TimeIn-PL-2:HANDLE
         Hdl-TimeIn-PL[3] = TS-TimeIn-PL-3:HANDLE
         Hdl-TimeIn-PL[4] = TS-TimeIn-PL-4:HANDLE
         Hdl-TimeIn-PL[5] = TS-TimeIn-PL-5:HANDLE
         Hdl-TimeIn-PL[6] = TS-TimeIn-PL-6:HANDLE
         Hdl-TimeIn-PL[7] = TS-TimeIn-PL-7:HANDLE
         Hdl-TimeOut-PL[1] = TS-TimeOut-PL-1:HANDLE
         Hdl-TimeOut-PL[2] = TS-TimeOut-PL-2:HANDLE
         Hdl-TimeOut-PL[3] = TS-TimeOut-PL-3:HANDLE
         Hdl-TimeOut-PL[4] = TS-TimeOut-PL-4:HANDLE
         Hdl-TimeOut-PL[5] = TS-TimeOut-PL-5:HANDLE
         Hdl-TimeOut-PL[6] = TS-TimeOut-PL-6:HANDLE
         Hdl-TimeOut-PL[7] = TS-TimeOut-PL-7:HANDLE
         Hdl-WrkHrs[1] = TS-WrkHrs-1:HANDLE
         Hdl-WrkHrs[2] = TS-WrkHrs-2:HANDLE
         Hdl-WrkHrs[3] = TS-WrkHrs-3:HANDLE
         Hdl-WrkHrs[4] = TS-WrkHrs-4:HANDLE
         Hdl-WrkHrs[5] = TS-WrkHrs-5:HANDLE
         Hdl-WrkHrs[6] = TS-WrkHrs-6:HANDLE
         Hdl-WrkHrs[7] = TS-WrkHrs-7:HANDLE
         Hdl-OTHrs[1] = TS-OTHrs-1:HANDLE
         Hdl-OTHrs[2] = TS-OTHrs-2:HANDLE
         Hdl-OTHrs[3] = TS-OTHrs-3:HANDLE
         Hdl-OTHrs[4] = TS-OTHrs-4:HANDLE
         Hdl-OTHrs[5] = TS-OTHrs-5:HANDLE
         Hdl-OTHrs[6] = TS-OTHrs-6:HANDLE
         Hdl-OTHrs[7] = TS-OTHrs-7:HANDLE
         Hdl-DTHrs[1] = TS-DTHrs-1:HANDLE
         Hdl-DTHrs[2] = TS-DTHrs-2:HANDLE
         Hdl-DTHrs[3] = TS-DTHrs-3:HANDLE
         Hdl-DTHrs[4] = TS-DTHrs-4:HANDLE
         Hdl-DTHrs[5] = TS-DTHrs-5:HANDLE
         Hdl-DTHrs[6] = TS-DTHrs-6:HANDLE
         Hdl-DTHrs[7] = TS-DTHrs-7:HANDLE
         Hdl-MHrs[1] = TS-MHrs-1:HANDLE
         Hdl-MHrs[2] = TS-MHrs-2:HANDLE
         Hdl-MHrs[3] = TS-MHrs-3:HANDLE
         Hdl-MHrs[4] = TS-MHrs-4:HANDLE
         Hdl-MHrs[5] = TS-MHrs-5:HANDLE
         Hdl-MHrs[6] = TS-MHrs-6:HANDLE
         Hdl-MHrs[7] = TS-MHrs-7:HANDLE
         Hdl-VacHrs[1] = TS-VacHrs-1:HANDLE
         Hdl-VacHrs[2] = TS-VacHrs-2:HANDLE
         Hdl-VacHrs[3] = TS-VacHrs-3:HANDLE
         Hdl-VacHrs[4] = TS-VacHrs-4:HANDLE
         Hdl-VacHrs[5] = TS-VacHrs-5:HANDLE
         Hdl-VacHrs[6] = TS-VacHrs-6:HANDLE
         Hdl-VacHrs[7] = TS-VacHrs-7:HANDLE
         LvEmployee:SCREEN-VALUE = ""
         Lvpassword:SCREEN-VALUE = ""
         WeekendingDate:SCREEN-VALUE = ""
         WeekendingDate:SENSITIVE = FALSE
         btnTs:SENSITIVE = FALSE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE process C-Win 
PROCEDURE process :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ip-clear-scr AS LOG NO-UNDO.

DO WITH FRAME {&FRAME-NAME} :
  ASSIGN Lvemployee LvEmployeeList WeekEndingDate
         LvNotes:SCREEN-VALUE = "".
  DISABLE BtnApprove BtnDecline BtnTS.
  
  IF WeekendingDate = ? OR weekday(WeekEndingDate) <> 1 THEN
  DO:
      MESSAGE "Week Ending Date must be Sunday." VIEW-AS ALERT-BOX ERROR.
      RETURN .
  END.

  RELEASE employee.

  FIND FIRST tt-emp WHERE
       tt-emp.emp-name = LvEmployeeList:SCREEN-VALUE.

  IF AVAIL tt-emp THEN
     FIND FIRST employee NO-LOCK WHERE
          employee.company EQ cocode AND
          employee.employee = tt-emp.employee
          NO-ERROR.

  IF NOT AVAILABLE employee  THEN
  DO:
    MESSAGE "Invalid Employee." VIEW-AS ALERT-BOX.
  END.

  RUN UTIL/CurrDir.p (output CurrDir).
  IF SEARCH(CurrDir + "\Signature\" + Lvemployee + ".jpg") = ? THEN
  DO:
    MESSAGE "Signature file does not exist for " LvEmployee + "."
        VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY.
  END.

  IF SEARCH(CurrDir + "\Signature\" + employee.employee + ".jpg") = ? THEN
  DO:
    MESSAGE "Signature file does not exist for " LvEmployeeList:SCREEN-VALUE + "."
        VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY.
  END.

  IF ip-clear-scr THEN
  DO:
     DO j = 1 TO 7 :
        ASSIGN 
          Hdl-TS-Date[j]:SCREEN-VALUE = ""
          Hdl-TimeIn[j]:SCREEN-VALUE = ""
          Hdl-TimeOut[j]:SCREEN-VALUE = ""
          Hdl-TimeIn-PL[j]:SCREEN-VALUE = ""
          Hdl-TimeOut-PL[j]:SCREEN-VALUE = ""
          Hdl-WrkHrs[j]:SCREEN-VALUE = "00.00"
          Hdl-OTHrs[j]:SCREEN-VALUE = "00.00"
          Hdl-DTHrs[j]:SCREEN-VALUE = "00.00"
          Hdl-MHrs[j]:SCREEN-VALUE = "00.00"
          Hdl-VacHrs[j]:SCREEN-VALUE = "00.00".
     END.
     ASSIGN
       TS-DTHrs-tot:SCREEN-VALUE = "00.00" 
       TS-MHrs-tot:SCREEN-VALUE = "00.00" 
       TS-OTHrs-tot:SCREEN-VALUE = "00.00" 
       TS-WrkHrs-tot:SCREEN-VALUE = "00.00" 
       TS-VacHrs-tot:SCREEN-VALUE = "00.00".
  END.

  /* Find if the timesheet is submitted or not */
           
  FIND FIRST timesheet WHERE
       timesheet.employee =  employee.employee AND
       timesheet.WeekEnding = DATE(WeekendingDate:SCREEN-VALUE)
       NO-LOCK NO-ERROR.

  /* If TimeSheet Available */
  IF AVAILABLE TIMESHEET THEN
  DO:
    BtnTS:SENSITIVE = FALSE.
    DO j = 1 TO 7:
      ASSIGN 
         Hdl-TS-Date[j]:SCREEN-VALUE = STRING(weekEndingdate - 7 + j )
         Hdl-TimeIn[j]:SCREEN-VALUE = STRING(timesheet.Time_Login1[j])
         Hdl-TimeOut[j]:SCREEN-VALUE = STRING(timesheet.Time_Logout1[j])
         Hdl-TimeIn-PL[j]:SCREEN-VALUE = STRING(timesheet.Time_Login2[j])
         Hdl-TimeOut-PL[j]:SCREEN-VALUE = STRING(timesheet.Time_Logout2[j]).

      IF ip-clear-scr THEN
         ASSIGN
          Hdl-OTHrs[j]:SCREEN-VALUE = STRING(timesheet.OT_Hrs[j])
          Hdl-DTHrs[j]:SCREEN-VALUE = STRING(timesheet.DT_Hrs[j])
          Hdl-MHrs[j]:SCREEN-VALUE = STRING(timesheet.MakeUp_Hrs[j])
          Hdl-VacHrs[j]:SCREEN-VALUE = STRING(timesheet.PTO_VAC_Hrs[j])
          Hdl-WrkHrs[j]:SCREEN-VALUE = STRING(timesheet.Reg_Hrs[j]).
    END.

    ASSIGN LvNotes:SCREEN-VALUE = timesheet.notes.
    IF TimeSheet.ApprovedBy <> "" THEN
    DO:
      ASSIGN BtnApprove:SENSITIVE = FALSE.

      FIND FIRST buf-employee NO-LOCK WHERE
           buf-employee.company EQ cocode AND
           buf-employee.employee = TimeSheet.ApprovedBy
           NO-ERROR.

      IF AVAILABLE buf-employee THEN
      ASSIGN LvStatus:SCREEN-VALUE = "Time Sheet Approved By " +  buf-employee.FIRST_name + " " + buf-employee.last_name +
                                     " On: " + STRING(TimeSheet.ApprovedOn).
      ELSE
      ASSIGN LvStatus:SCREEN-VALUE = "Time Sheet Approved By " +  TimeSheet.ApprovedBy +
                                         " On: " + STRING(TimeSheet.ApprovedOn).
      IF TimeSheet.ApprovedBy = LvEmployee THEN
      ASSIGN BtnDecline:SENSITIVE = TRUE.

    END.
    ELSE
    DO:
      LvEmpNum = employee.employee.

      FIND FIRST employee NO-LOCK WHERE
           employee.company = cocode AND
           employee.employee = LvEmpNum AND
           employee.ref_no = LvEmployee
           NO-ERROR.

      IF AVAILABLE employee THEN
         ASSIGN BtnApprove:SENSITIVE = TRUE.
      ELSE
          ASSIGN BtnApprove:SENSITIVE = FALSE.

      FIND FIRST buf-employee NO-LOCK WHERE
           buf-employee.company EQ cocode AND
           buf-employee.employee = TimeSheet.SubmittedBy
           NO-ERROR.

      IF AVAILABLE buf-employee THEN
      ASSIGN LvStatus:SCREEN-VALUE = "Timesheet Submitted By " +  buf-employee.FIRST_name + " " + buf-employee.last_name +
                                     " On: " + STRING(TimeSheet.SubmittedOn).
      ELSE
      ASSIGN LvStatus:SCREEN-VALUE = "Timsheet Submitted By " +  TimeSheet.SubmittedBy +
                                         " On: " + STRING(TimeSheet.SubmittedOn).
      IF TimeSheet.SubmittedBy = LvEmployee THEN
      ASSIGN BtnDecline:SENSITIVE = TRUE.
    END.
  END.
  
  ELSE DO :
    assign
        LvEmpNum = employee.employee
        LvWeekEnding = WeekEndingDate
        LvStatus:SCREEN-VALUE = "New Timesheet".
    FOR EACH tt-tsrep :
      DELETE tt-tsrep.
    END.
    FOR EACH tt-note :
       DELETE tt-note.
    END.

    RUN touch/r-tsrep.p.

    DO j = 1 TO 7 :
      ASSIGN 
          Hdl-TS-Date[j]:SCREEN-VALUE = STRING(WeekEndingDate - 7 + j ).
      FIND FIRST tt-tsrep NO-LOCK WHERE tt-tsrep.tt-date =  ( WeekEndingDate - 7 + j ) NO-ERROR.
      IF AVAILABLE tt-tsrep THEN
      ASSIGN
          Hdl-TimeIn[j]:SCREEN-VALUE = STRING(tt-time-in1)
          Hdl-TimeOut[j]:SCREEN-VALUE = STRING(tt-time-out1)
          Hdl-TimeIn-PL[j]:SCREEN-VALUE = STRING(tt-time-in2)
          Hdl-TimeOut-PL[j]:SCREEN-VALUE = STRING(tt-time-out2)
          Hdl-WrkHrs[j]:SCREEN-VALUE = STRING(replace(tt-wrk-hrs, ":", "."))
          Hdl-OTHrs[j]:SCREEN-VALUE = STRING(replace(tt-ot-hrs, ":", "."))
          Hdl-DTHrs[j]:SCREEN-VALUE = STRING(replace(tt-dt-hrs, ":", ".")).
    END.
    for each tt-note BY tt-note.note_date:
      IF LvNotes:SCREEN-VALUE = "" THEN
      ASSIGN LvNotes:SCREEN-VALUE = string(tt-note.note_date) + " "  + tt-note.note_title + CHR(10) + tt-note.note_text.
      ELSE
      ASSIGN LvNotes:SCREEN-VALUE = LvNotes:SCREEN-VALUE + chr(10) + string(tt-note.note_date) + " "  + tt-note.note_title + CHR(10) + tt-note.note_text.
    END.
    ENABLE BtnTS.
    /*APPLY "entry" TO TS-MHrs-1.*/
  END.
  RUN tot-reg-hrs.
  RUN tot-ot-hrs.
  RUN tot-dt-hrs.
  RUN tot-m-hrs.
  RUN tot-vac-hrs.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tes C-Win 
PROCEDURE tes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

PROCEDURE ShellExecuteA EXTERNAL "shell32" :
   define input parameter hwnd as long.
   define input parameter lpOperation as char.
   define input parameter lpFile as char.
   define input parameter lpParameters as char.
   define input parameter lpDirectory as char.
   define input parameter nShowCmd as long.
   define return parameter hInstance as long.
END PROCEDURE. /* ShellExecuteA */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tot-dt-hrs C-Win 
PROCEDURE tot-dt-hrs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE LvTotDTHrs AS INT NO-UNDO.

DO WITH FRAME {&FRAME-NAME} :
  ASSIGN LvTotDTHrs = 0.
  DO j = 1 TO 7 :
    ASSIGN LvtotDTHrs = LvtotDTHrs +   
      ( TRUNCATE( decimal(Hdl-DTHrs[j]:SCREEN-VALUE) , 0 ) * 3600 ) +
      ( (( decimal(Hdl-DTHrs[j]:SCREEN-VALUE) - TRUNCATE( decimal(Hdl-DTHrs[j]:SCREEN-VALUE) , 0 ) ) * 100 ) * 60)
    .
  END.
  
  ASSIGN TS-DTHrs-tot:SCREEN-VALUE = string( truncate(LvTotDTHrs / 3600,0), ">99" ) + "." + STRING(truncate((LvTotDTHrs mod 3600) / 60,0), "99").
.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tot-m-hrs C-Win 
PROCEDURE tot-m-hrs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE LvTotmHrs AS INT NO-UNDO.

DO WITH FRAME {&FRAME-NAME} :
  ASSIGN LvTotmHrs = 0.
  DO j = 1 TO 7 :
    ASSIGN LvtotmHrs = LvtotmHrs +   
      ( TRUNCATE( decimal(Hdl-mHrs[j]:SCREEN-VALUE) , 0 ) * 3600 ) +
      ( (( decimal(Hdl-mHrs[j]:SCREEN-VALUE) - TRUNCATE( decimal(Hdl-mHrs[j]:SCREEN-VALUE) , 0 ) ) * 100 ) * 60)
    .
  END.
  
  ASSIGN TS-mHrs-tot:SCREEN-VALUE = string( truncate(LvTotmHrs / 3600,0), ">99" ) + "." + STRING(truncate((LvTotmHrs mod 3600) / 60,0), "99").
.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tot-ot-hrs C-Win 
PROCEDURE tot-ot-hrs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE LvTotOTHrs AS INT NO-UNDO.

DO WITH FRAME {&FRAME-NAME} :
  ASSIGN LvTotOTHrs = 0.
  DO j = 1 TO 7 :
    ASSIGN LvtotOTHrs = LvtotOTHrs +   
      ( TRUNCATE( decimal(Hdl-OTHrs[j]:SCREEN-VALUE) , 0 ) * 3600 ) +
      ( (( decimal(Hdl-OTHrs[j]:SCREEN-VALUE) - TRUNCATE( decimal(Hdl-OTHrs[j]:SCREEN-VALUE) , 0 ) ) * 100 ) * 60)
    .
  END.
  
  ASSIGN TS-OTHrs-tot:SCREEN-VALUE = string( truncate(LvTotOTHrs / 3600,0), ">99" ) + "." + STRING(truncate((LvTotOTHrs mod 3600) / 60,0), "99").
.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tot-reg-hrs C-Win 
PROCEDURE tot-reg-hrs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE LvTotWrkHrs AS INT NO-UNDO.

DO WITH FRAME {&FRAME-NAME} :
  ASSIGN LvTotWrkHrs = 0.
  DO j = 1 TO 7 :
    ASSIGN LvtotWrkHrs = LvtotWrkHrs +   
      ( TRUNCATE( decimal(Hdl-WrkHrs[j]:SCREEN-VALUE) , 0 ) * 3600 ) +
      ( (( decimal(Hdl-WrkHrs[j]:SCREEN-VALUE) - TRUNCATE( decimal(Hdl-WrkHrs[j]:SCREEN-VALUE) , 0 ) ) * 100 ) * 60)
    .
  END.
  
  ASSIGN TS-WrkHrs-tot:SCREEN-VALUE = string( truncate(LvTotWrkHrs / 3600,0), ">99" ) + "." + STRING(truncate((LvTotWrkHrs mod 3600) / 60,0), "99").
.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tot-vac-hrs C-Win 
PROCEDURE tot-vac-hrs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE LvTotVACHrs AS INT NO-UNDO.

DO WITH FRAME {&FRAME-NAME} :
  ASSIGN LvTotVACHrs = 0.
  DO j = 1 TO 7 :
    ASSIGN LvtotVACHrs = LvtotVACHrs +   
      ( TRUNCATE( decimal(Hdl-VACHrs[j]:SCREEN-VALUE) , 0 ) * 3600 ) +
      ( (( decimal(Hdl-VACHrs[j]:SCREEN-VALUE) - TRUNCATE( decimal(Hdl-VACHrs[j]:SCREEN-VALUE) , 0 ) ) * 100 ) * 60)
    .
  END.
  
  ASSIGN TS-VACHrs-tot:SCREEN-VALUE = string( truncate(LvTotVACHrs / 3600,0), ">99" ) + "." + STRING(truncate((LvTotVACHrs mod 3600) / 60,0), "99").
.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

