&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: custom/batParam.w

  Description: Printer and Batch Spooler Parameter Dialog

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 3.5.2013 (copied from custom/d-printB.w)
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEF OUTPUT PARAM opPrinterName AS CHAR NO-UNDO.
DEF OUTPUT PARAM opCopies AS INT NO-UNDO.
DEF OUTPUT PARAM opStartDate AS DATE NO-UNDO.
DEF OUTPUT PARAM opStartTime AS INT NO-UNDO.
DEF OUTPUT PARAM opEndDate AS DATE NO-UNDO.
DEF OUTPUT PARAM opEndTime AS INT NO-UNDO.
DEF OUTPUT PARAM opDayOfWeek-1 AS LOGICAL NO-UNDO.
DEF OUTPUT PARAM opDayOfWeek-2 AS LOGICAL NO-UNDO.
DEF OUTPUT PARAM opDayOfWeek-3 AS LOGICAL NO-UNDO.
DEF OUTPUT PARAM opDayOfWeek-4 AS LOGICAL NO-UNDO.
DEF OUTPUT PARAM opDayOfWeek-5 AS LOGICAL NO-UNDO.
DEF OUTPUT PARAM opDayOfWeek-6 AS LOGICAL NO-UNDO.
DEF OUTPUT PARAM opDayOfWeek-7 AS LOGICAL NO-UNDO.
DEF OUTPUT PARAM opRepeatWeekly AS LOGICAL NO-UNDO.
DEF OUTPUT PARAM opContinue AS LOGICAL NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF VAR ip-print-file AS CHAR NO-UNDO.
DEF VAR lv-printer-list AS CHAR NO-UNDO.
DEF VAR lv-printer-names AS CHAR NO-UNDO.
DEF VAR lv-printer-count AS INT NO-UNDO.
DEF VAR lv-prt-cmd AS CHAR NO-UNDO.
DEF VAR lv-prt-port AS CHAR NO-UNDO.
DEF VAR i AS INT NO-UNDO.

DEF TEMP-TABLE tt-printer NO-UNDO
  FIELD prt-num AS INT
  FIELD prt-port AS CHAR
  FIELD prt-name AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-4 cb-printer v-copies ~
start_date start_time end_date end_time dayOfWeek-1 dayOfWeek-2 dayOfWeek-3 ~
dayOfWeek-4 dayOfWeek-5 dayOfWeek-6 dayOfWeek-7 Btn_OK Btn_Cancel ~
repeatWeekly 
&Scoped-Define DISPLAYED-OBJECTS cb-printer lv-port v-copies start_date ~
start_time end_date end_time dayOfWeek-1 dayOfWeek-2 dayOfWeek-3 ~
dayOfWeek-4 dayOfWeek-5 dayOfWeek-6 dayOfWeek-7 repeatWeekly 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 cb-printer v-copies start_date start_time end_date ~
end_time dayOfWeek-1 dayOfWeek-2 dayOfWeek-3 dayOfWeek-4 dayOfWeek-5 ~
dayOfWeek-6 dayOfWeek-7 repeatWeekly 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 14 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 14 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE cb-printer AS CHARACTER FORMAT "X(256)":U 
     LABEL "Name" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 73 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U 
     LABEL "End Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE end_time AS CHARACTER FORMAT "99:99" 
     LABEL "Time" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE lv-port AS CHARACTER FORMAT "X(256)":U 
     LABEL "Where" 
     VIEW-AS FILL-IN 
     SIZE 73 BY 1 NO-UNDO.

DEFINE VARIABLE start_date AS DATE FORMAT "99/99/9999":U 
     LABEL "Start Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE start_time AS CHARACTER FORMAT "99:99" 
     LABEL "Time" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE v-copies AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 1 
     LABEL "Number of copies" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 83 BY 2.86.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY 1.43.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54 BY 5.71.

DEFINE VARIABLE dayOfWeek-1 AS LOGICAL INITIAL no 
     LABEL "S" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE dayOfWeek-2 AS LOGICAL INITIAL no 
     LABEL "M" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE dayOfWeek-3 AS LOGICAL INITIAL no 
     LABEL "T" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE dayOfWeek-4 AS LOGICAL INITIAL no 
     LABEL "W" 
     VIEW-AS TOGGLE-BOX
     SIZE 6 BY .81 NO-UNDO.

DEFINE VARIABLE dayOfWeek-5 AS LOGICAL INITIAL no 
     LABEL "T" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE dayOfWeek-6 AS LOGICAL INITIAL no 
     LABEL "F" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE dayOfWeek-7 AS LOGICAL INITIAL no 
     LABEL "S" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE repeatWeekly AS LOGICAL INITIAL no 
     LABEL "Repeat Weekly" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     cb-printer AT ROW 1.71 COL 8 COLON-ALIGNED
     lv-port AT ROW 2.91 COL 8 COLON-ALIGNED
     v-copies AT ROW 4.57 COL 73 COLON-ALIGNED
     start_date AT ROW 5.05 COL 14 COLON-ALIGNED WIDGET-ID 82
     start_time AT ROW 5.05 COL 39 COLON-ALIGNED WIDGET-ID 6
     end_date AT ROW 6.48 COL 14 COLON-ALIGNED WIDGET-ID 74
     end_time AT ROW 6.48 COL 39 COLON-ALIGNED WIDGET-ID 8
     dayOfWeek-1 AT ROW 7.91 COL 16 WIDGET-ID 60
     dayOfWeek-2 AT ROW 7.91 COL 22 WIDGET-ID 62
     dayOfWeek-3 AT ROW 7.91 COL 28 WIDGET-ID 64
     dayOfWeek-4 AT ROW 7.91 COL 33 WIDGET-ID 66
     dayOfWeek-5 AT ROW 7.91 COL 39 WIDGET-ID 68
     dayOfWeek-6 AT ROW 7.91 COL 44 WIDGET-ID 70
     dayOfWeek-7 AT ROW 7.91 COL 49 WIDGET-ID 72
     Btn_OK AT ROW 8.86 COL 56
     Btn_Cancel AT ROW 8.86 COL 71
     repeatWeekly AT ROW 9.1 COL 16 WIDGET-ID 80
     "Day of Week:" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 7.91 COL 2 WIDGET-ID 86
     " Spool Request Parameters" VIEW-AS TEXT
          SIZE 26 BY .62 AT ROW 4.1 COL 2 WIDGET-ID 42
     " Copies" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 4.1 COL 57
     " Printer" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1 COL 2
     RECT-1 AT ROW 1.24 COL 1
     RECT-2 AT ROW 4.33 COL 56
     RECT-4 AT ROW 4.33 COL 1 WIDGET-ID 78
     SPACE(30.00) SKIP(0.00)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Print"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX cb-printer IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX dayOfWeek-1 IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX dayOfWeek-2 IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX dayOfWeek-3 IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX dayOfWeek-4 IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX dayOfWeek-5 IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX dayOfWeek-6 IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX dayOfWeek-7 IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN end_date IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN end_time IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN lv-port IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX repeatWeekly IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN start_date IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN start_time IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN v-copies IN FRAME Dialog-Frame
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Print */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  ASSIGN {&List-1}.
  
  FIND FIRST tt-printer NO-LOCK WHERE tt-printer.prt-name EQ cb-printer NO-ERROR.
  IF AVAIL tt-printer THEN
  ASSIGN
    lv-port = tt-printer.prt-port
    lv-prt-port = lv-port.
  
  ASSIGN
    opPrinterName = IF cb-printer BEGINS "\\" OR lv-prt-port BEGINS "usb" THEN cb-printer
                      ELSE lv-port
    opCopies = v-copies
    opStartDate = start_date
    opStartTime = INTEGER(SUBSTR(start_time,1,2)) * 3600 +
                  INTEGER(SUBSTR(start_time,3,2)) * 60
    opEndDate = end_date
    opEndTime = INTEGER(SUBSTR(end_time,1,2)) * 3600 +
                INTEGER(SUBSTR(end_time,3,2)) * 60
    opDayOfWeek-1 = dayOfWeek-1
    opDayOfWeek-2 = dayOfWeek-2
    opDayOfWeek-3 = dayOfWeek-3
    opDayOfWeek-4 = dayOfWeek-4
    opDayOfWeek-5 = dayOfWeek-5
    opDayOfWeek-6 = dayOfWeek-6
    opDayOfWeek-7 = dayOfWeek-7
    opRepeatWeekly = repeatWeekly
    opContinue = YES
    .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-printer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-printer Dialog-Frame
ON VALUE-CHANGED OF cb-printer IN FRAME Dialog-Frame /* Name */
DO:
   ASSIGN {&SELF-NAME}.
   FIND FIRST tt-printer NO-LOCK WHERE tt-printer.prt-name EQ cb-printer NO-ERROR.
   IF AVAIL tt-printer THEN
   ASSIGN
     lv-port:SCREEN-VALUE = tt-printer.prt-port
     lv-port.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_time
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_time Dialog-Frame
ON LEAVE OF end_time IN FRAME Dialog-Frame /* Time */
DO:
  ASSIGN {&SELF-NAME}.
  IF INTEGER(SUBSTR({&SELF-NAME},1,2)) * 3600 +
     INTEGER(SUBSTR({&SELF-NAME},3,2)) * 60 LT 0 OR
     INTEGER(SUBSTR({&SELF-NAME},1,2)) * 3600 +
     INTEGER(SUBSTR({&SELF-NAME},3,2)) * 60 GT 86400 THEN DO:
    MESSAGE "Time Entered is Invalid!" VIEW-AS ALERT-BOX ERROR.
    APPLY "ENTRY" TO {&SELF-NAME}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME start_time
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL start_time Dialog-Frame
ON LEAVE OF start_time IN FRAME Dialog-Frame /* Time */
DO:
  ASSIGN {&SELF-NAME}.
  IF INTEGER(SUBSTR({&SELF-NAME},1,2)) * 3600 +
     INTEGER(SUBSTR({&SELF-NAME},3,2)) * 60 LT 0 OR
     INTEGER(SUBSTR({&SELF-NAME},1,2)) * 3600 +
     INTEGER(SUBSTR({&SELF-NAME},3,2)) * 60 GT 86400 THEN DO:
    MESSAGE "Time Entered is Invalid!" VIEW-AS ALERT-BOX ERROR.
    APPLY "ENTRY" TO {&SELF-NAME}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   /*RUN custom/printapi.p (OUTPUT lv-printer-list, OUTPUT lv-printer-names).*/
   RUN aderb/_prlist.p (OUTPUT lv-printer-names,OUTPUT lv-printer-list,OUTPUT lv-printer-count).

  DO i = 1 TO lv-printer-count:
    FIND FIRST tt-printer NO-LOCK
         WHERE tt-printer.prt-name EQ ENTRY(i,lv-printer-names) NO-ERROR.
    IF NOT AVAIL tt-printer THEN DO:
      cb-printer:ADD-LAST(ENTRY(i,lv-printer-names)).
      CREATE tt-printer.
      ASSIGN
        tt-printer.prt-num  = i
        tt-printer.prt-name = ENTRY(i,lv-printer-names)
        tt-printer.prt-port = ENTRY(i,lv-printer-list).
    END.
  END.

  RUN enable_UI.

  ASSIGN
    cb-printer:SCREEN-VALUE = SESSION:PRINTER-NAME
    lv-port:SCREEN-VALUE = SESSION:PRINTER-PORT
    start_date:SCREEN-VALUE = STRING(TODAY)
    start_time:SCREEN-VALUE = STRING(TIME,'hh:mm')
    end_date:SCREEN-VALUE = STRING(TODAY + 30)
    end_time:SCREEN-VALUE = '23:59'.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY cb-printer lv-port v-copies start_date start_time end_date end_time 
          dayOfWeek-1 dayOfWeek-2 dayOfWeek-3 dayOfWeek-4 dayOfWeek-5 
          dayOfWeek-6 dayOfWeek-7 repeatWeekly 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 RECT-2 RECT-4 cb-printer v-copies start_date start_time 
         end_date end_time dayOfWeek-1 dayOfWeek-2 dayOfWeek-3 dayOfWeek-4 
         dayOfWeek-5 dayOfWeek-6 dayOfWeek-7 Btn_OK Btn_Cancel repeatWeekly 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

