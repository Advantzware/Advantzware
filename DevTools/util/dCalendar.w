&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME dCalendar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS dCalendar 
/*------------------------------------------------------------------------

  File: dCalendar.w

  Description: This program displays a calendar and allows a user to select
                a date in the calendar, returning the selected date to the
                calling program

  Input Parameters:
      ipInput - type character, should be in a format that will convert to a date

  Output Parameters:
      opOutput - type character, place in screen value of a date-type field

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF /* VAR */ INPUT PARAMETER ipInput AS CHAR NO-UNDO.
DEF /* VAR */ OUTPUT PARAMETER opOutput AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF VAR daInputDate AS DATE NO-UNDO.
DEF VAR daOutputDate AS DATE NO-UNDO.
DEF VAR i AS INT NO-UNDO.

IF ipInput = "" THEN ASSIGN
    daInputDate = TODAY.
ELSE ASSIGN
    daInputDate = DATE(ipInput).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME dCalendar

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 cbMonth ~
cbYear fi-1 fi-2 fi-3 fi-4 fi-5 fi-6 fi-7 fi-8 fi-9 fi-10 fi-11 fi-12 fi-13 ~
fi-14 fi-15 fi-16 fi-17 fi-18 fi-19 fi-20 fi-21 fi-22 fi-23 fi-24 fi-25 ~
fi-26 fi-27 fi-28 fi-29 fi-30 fi-31 fi-32 fi-33 fi-34 fi-35 fi-36 fi-37 ~
fi-38 fi-39 fi-40 fi-41 fi-42 
&Scoped-Define DISPLAYED-OBJECTS cbMonth cbYear fi-Sun fi-Mon fi-Tue fi-Wed ~
fi-Thu fi-Fri fi-Sat fi-1 fi-2 fi-3 fi-4 fi-5 fi-6 fi-7 fi-8 fi-9 fi-10 ~
fi-11 fi-12 fi-13 fi-14 fi-15 fi-16 fi-17 fi-18 fi-19 fi-20 fi-21 fi-22 ~
fi-23 fi-24 fi-25 fi-26 fi-27 fi-28 fi-29 fi-30 fi-31 fi-32 fi-33 fi-34 ~
fi-35 fi-36 fi-37 fi-38 fi-39 fi-40 fi-41 fi-42 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cbMonth AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC" 
     DROP-DOWN-LIST
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE cbYear AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEMS "1949" 
     DROP-DOWN-LIST
     SIZE 9 BY 1
     FONT 12 NO-UNDO.

DEFINE VARIABLE fi-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-10 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-11 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-12 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-13 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-14 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-15 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-16 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-17 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-18 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-19 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-20 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-21 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-22 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-23 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-24 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-25 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-26 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-27 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-28 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-29 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-30 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-31 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-32 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-33 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-34 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-35 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-36 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-37 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-38 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-39 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-40 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-41 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-42 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-8 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-9 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-Fri AS CHARACTER FORMAT "X(256)":U INITIAL "    Fri" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-Mon AS CHARACTER FORMAT "X(256)":U INITIAL "   Mon" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-Sat AS CHARACTER FORMAT "X(256)":U INITIAL "   Sat" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-Sun AS CHARACTER FORMAT "X(256)":U INITIAL "   Sun" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-Thu AS CHARACTER FORMAT "X(256)":U INITIAL "   Thu" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-Tue AS CHARACTER FORMAT "X(256)":U INITIAL "   Tue" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-Wed AS CHARACTER FORMAT "X(256)":U INITIAL "  Wed" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE IMAGE IMAGE-2
     FILENAME "img/tinyleftarrow.jpg":U
     SIZE 4 BY 1.

DEFINE IMAGE IMAGE-3
     FILENAME "img/tinyrightarrow.jpg":U
     SIZE 4 BY 1.

DEFINE IMAGE IMAGE-4
     FILENAME "img/tinyleftarrow2.jpg":U
     SIZE 5 BY 1.

DEFINE IMAGE IMAGE-5
     FILENAME "img/tinyrightarrow2.jpg":U
     SIZE 5.86 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME dCalendar
     cbMonth AT ROW 1.25 COL 15 COLON-ALIGNED NO-LABEL
     cbYear AT ROW 1.25 COL 25 COLON-ALIGNED NO-LABEL
     fi-Sun AT ROW 2.5 COL 2 NO-LABEL NO-TAB-STOP 
     fi-Mon AT ROW 2.5 COL 9 NO-LABEL NO-TAB-STOP 
     fi-Tue AT ROW 2.5 COL 16 NO-LABEL NO-TAB-STOP 
     fi-Wed AT ROW 2.5 COL 23 NO-LABEL NO-TAB-STOP 
     fi-Thu AT ROW 2.5 COL 30 NO-LABEL NO-TAB-STOP 
     fi-Fri AT ROW 2.5 COL 37 NO-LABEL NO-TAB-STOP 
     fi-Sat AT ROW 2.5 COL 44 NO-LABEL NO-TAB-STOP 
     fi-1 AT ROW 3.46 COL 2 NO-LABEL
     fi-2 AT ROW 3.46 COL 9 NO-LABEL
     fi-3 AT ROW 3.46 COL 16 NO-LABEL
     fi-4 AT ROW 3.46 COL 23 NO-LABEL
     fi-5 AT ROW 3.46 COL 30 NO-LABEL
     fi-6 AT ROW 3.46 COL 37 NO-LABEL
     fi-7 AT ROW 3.46 COL 44 NO-LABEL
     fi-8 AT ROW 4.63 COL 2 NO-LABEL
     fi-9 AT ROW 4.63 COL 9 NO-LABEL
     fi-10 AT ROW 4.63 COL 16 NO-LABEL
     fi-11 AT ROW 4.63 COL 23 NO-LABEL
     fi-12 AT ROW 4.63 COL 30 NO-LABEL
     fi-13 AT ROW 4.63 COL 37 NO-LABEL
     fi-14 AT ROW 4.63 COL 44 NO-LABEL
     fi-15 AT ROW 5.83 COL 2 NO-LABEL
     fi-16 AT ROW 5.83 COL 9 NO-LABEL
     fi-17 AT ROW 5.83 COL 16 NO-LABEL
     fi-18 AT ROW 5.83 COL 23 NO-LABEL
     fi-19 AT ROW 5.83 COL 30 NO-LABEL
     fi-20 AT ROW 5.83 COL 37 NO-LABEL
     fi-21 AT ROW 5.83 COL 44 NO-LABEL
     fi-22 AT ROW 6.96 COL 2 NO-LABEL
     fi-23 AT ROW 6.96 COL 9 NO-LABEL
     fi-24 AT ROW 6.96 COL 16 NO-LABEL
     fi-25 AT ROW 6.96 COL 23 NO-LABEL
     fi-26 AT ROW 6.96 COL 30 NO-LABEL
     fi-27 AT ROW 6.96 COL 37 NO-LABEL
     fi-28 AT ROW 6.96 COL 44 NO-LABEL
     fi-29 AT ROW 8.21 COL 2 NO-LABEL
     fi-30 AT ROW 8.21 COL 9 NO-LABEL
     fi-31 AT ROW 8.21 COL 16 NO-LABEL
     fi-32 AT ROW 8.21 COL 23 NO-LABEL
     fi-33 AT ROW 8.21 COL 30 NO-LABEL
     fi-34 AT ROW 8.21 COL 37 NO-LABEL
     fi-35 AT ROW 8.21 COL 44 NO-LABEL
     fi-36 AT ROW 9.42 COL 2 NO-LABEL
     fi-37 AT ROW 9.42 COL 9 NO-LABEL
     fi-38 AT ROW 9.42 COL 16 NO-LABEL
     fi-39 AT ROW 9.42 COL 23 NO-LABEL
     fi-40 AT ROW 9.42 COL 30 NO-LABEL
     fi-41 AT ROW 9.42 COL 37 NO-LABEL
     fi-42 AT ROW 9.42 COL 44 NO-LABEL
     IMAGE-2 AT ROW 1.3 COL 12
     IMAGE-3 AT ROW 1.3 COL 38
     IMAGE-4 AT ROW 1.3 COL 7
     IMAGE-5 AT ROW 1.3 COL 42.14
     SPACE(3.28) SKIP(8.44)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Calendar".


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
/* SETTINGS FOR DIALOG-BOX dCalendar
   FRAME-NAME                                                           */
ASSIGN 
       FRAME dCalendar:SCROLLABLE       = FALSE
       FRAME dCalendar:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-1 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-10 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-11 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-12 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-13 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-14 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-15 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-16 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-17 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-18 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-19 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-2 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-20 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-21 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-22 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-23 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-24 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-25 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-26 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-27 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-28 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-29 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-3 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-30 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-31 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-32 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-33 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-34 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-35 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-36 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-37 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-38 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-39 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-4 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-40 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-41 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-42 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-5 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-6 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-7 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-8 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-9 IN FRAME dCalendar
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-Fri IN FRAME dCalendar
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fi-Mon IN FRAME dCalendar
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fi-Sat IN FRAME dCalendar
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fi-Sun IN FRAME dCalendar
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fi-Thu IN FRAME dCalendar
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fi-Tue IN FRAME dCalendar
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fi-Wed IN FRAME dCalendar
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME dCalendar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dCalendar dCalendar
ON CURSOR-UP OF FRAME dCalendar /* Calendar */
OR mouse-select-click OF image-4
OR mouse-select-click OF image-5
OR CURSOR-DOWN OF FRAME dCalendar ANYWHERE
DO:
    IF LAST-EVENT:FUNCTION = "cursor-up" THEN DO:
        ASSIGN
            daInputDate = DATE(
                MONTH(daInputDate),
                DAY(daInputDate),
                YEAR(daInputDate) - 1
                ).
        RUN ipChangeDates IN THIS-PROCEDURE. 
    END.
    ELSE IF last-event:FUNCTION = "cursor-down" THEN DO:
        ASSIGN
            daInputDate = DATE(
                MONTH(daInputDate),
                DAY(daInputDate),
                YEAR(daInputDate) + 1
                ).
        RUN ipChangeDates IN THIS-PROCEDURE. 
    END.
    ELSE DO:
        CASE SELF:NAME:
            WHEN "image-4" THEN DO:
                ASSIGN 
                    i = LOOKUP(cbYear:SCREEN-VALUE,cbYear:LIST-ITEMS)
                    i = i - 1
                    cbYear:SCREEN-VALUE = ENTRY(i,cbYear:LIST-ITEMS).
                APPLY 'value-changed' TO cbYear.
            END.
            WHEN "image-5" THEN DO:
                ASSIGN 
                    i = LOOKUP(cbYear:SCREEN-VALUE,cbYear:LIST-ITEMS)
                    i = i + 1
                    cbYear:SCREEN-VALUE = ENTRY(i,cbYear:LIST-ITEMS).
                APPLY 'value-changed' TO cbYear.
            END.
        END CASE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dCalendar dCalendar
ON WINDOW-CLOSE OF FRAME dCalendar /* Calendar */
DO:
    ASSIGN opOutput = string(daOutputDate).
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbMonth
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbMonth dCalendar
ON VALUE-CHANGED OF cbMonth IN FRAME dCalendar
OR VALUE-CHANGED OF cbYear
DO:
    ASSIGN
        daInputDate = DATE(
            LOOKUP(cbMonth:SCREEN-VALUE,"Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec"),
            1,
            INTEGER(cbYear:SCREEN-VALUE)).
    RUN ipChangeDates IN THIS-PROCEDURE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-1 dCalendar
ON MOUSE-SELECT-DBLCLICK OF fi-1 IN FRAME dCalendar
OR mouse-select-dblclick OF fi-2
OR mouse-select-dblclick OF fi-3
OR mouse-select-dblclick OF fi-4
OR mouse-select-dblclick OF fi-5
OR mouse-select-dblclick OF fi-6
OR mouse-select-dblclick OF fi-7
OR mouse-select-dblclick OF fi-8
OR mouse-select-dblclick OF fi-9
OR mouse-select-dblclick OF fi-10
OR mouse-select-dblclick OF fi-11
OR mouse-select-dblclick OF fi-12
OR mouse-select-dblclick OF fi-13
OR mouse-select-dblclick OF fi-14
OR mouse-select-dblclick OF fi-15
OR mouse-select-dblclick OF fi-16
OR mouse-select-dblclick OF fi-17
OR mouse-select-dblclick OF fi-18
OR mouse-select-dblclick OF fi-19
OR mouse-select-dblclick OF fi-20
OR mouse-select-dblclick OF fi-21
OR mouse-select-dblclick OF fi-22
OR mouse-select-dblclick OF fi-23
OR mouse-select-dblclick OF fi-24
OR mouse-select-dblclick OF fi-25
OR mouse-select-dblclick OF fi-26
OR mouse-select-dblclick OF fi-27
OR mouse-select-dblclick OF fi-28
OR mouse-select-dblclick OF fi-29
OR mouse-select-dblclick OF fi-30
OR mouse-select-dblclick OF fi-31
OR mouse-select-dblclick OF fi-32
OR mouse-select-dblclick OF fi-33
OR mouse-select-dblclick OF fi-34
OR mouse-select-dblclick OF fi-35
OR mouse-select-dblclick OF fi-36
OR mouse-select-dblclick OF fi-37
OR mouse-select-dblclick OF fi-38
OR mouse-select-dblclick OF fi-39
OR mouse-select-dblclick OF fi-40
OR mouse-select-dblclick OF fi-41
OR mouse-select-dblclick OF fi-41
DO:
    IF SELF:BGCOLOR <> 8 THEN DO:
        ASSIGN
            daOutputDate = DATE(
            MONTH(daInputDate),
            INTEGER(SELF:SCREEN-VALUE),
            YEAR(daInputDate)
            ).
        APPLY "GO" TO FRAME dCalendar.
    END.
        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-1 dCalendar
ON RETURN OF fi-1 IN FRAME dCalendar
OR RETURN OF fi-2
OR RETURN OF fi-3
OR RETURN OF fi-4
OR RETURN OF fi-5
OR RETURN OF fi-6
OR RETURN OF fi-7
OR RETURN OF fi-8
OR RETURN OF fi-9
OR RETURN OF fi-10
OR RETURN OF fi-11
OR RETURN OF fi-12
OR RETURN OF fi-13
OR RETURN OF fi-14
OR RETURN OF fi-15
OR RETURN OF fi-16
OR RETURN OF fi-17
OR RETURN OF fi-18
OR RETURN OF fi-19
OR RETURN OF fi-20
OR RETURN OF fi-21
OR RETURN OF fi-22
OR RETURN OF fi-23
OR RETURN OF fi-24
OR RETURN OF fi-25
OR RETURN OF fi-26
OR RETURN OF fi-27
OR RETURN OF fi-28
OR RETURN OF fi-29
OR RETURN OF fi-30
OR RETURN OF fi-31
OR RETURN OF fi-32
OR RETURN OF fi-33
OR RETURN OF fi-34
OR RETURN OF fi-35
OR RETURN OF fi-36
OR RETURN OF fi-37
OR RETURN OF fi-38
OR RETURN OF fi-39
OR RETURN OF fi-40
OR RETURN OF fi-41
OR RETURN OF fi-41
DO:
    IF SELF:BGCOLOR <> 8 THEN DO:
        ASSIGN
            daOutputDate = DATE(
            MONTH(daInputDate),
            INTEGER(SELF:SCREEN-VALUE),
            YEAR(daInputDate)
            ).
        APPLY "GO" TO FRAME dCalendar.
    END.
        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME IMAGE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IMAGE-2 dCalendar
ON MOUSE-SELECT-CLICK OF IMAGE-2 IN FRAME dCalendar
OR CURSOR-LEFT OF FRAME dCalendar ANYWHERE
DO:
    IF MONTH(daInputDate) > 1 THEN ASSIGN
        daInputDate = DATE(
            MONTH(daInputDate) - 1,
            1,
            YEAR(daInputDate)
            ).        
    ELSE ASSIGN
        daInputDate = DATE(
            12,
            1,
            YEAR(daInputDate) - 1
            ).        
    RUN ipChangeDates IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME IMAGE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IMAGE-3 dCalendar
ON MOUSE-SELECT-CLICK OF IMAGE-3 IN FRAME dCalendar
OR CURSOR-RIGHT OF FRAME dCalendar ANYWHERE
DO:
    IF MONTH(daInputDate) < 12 THEN ASSIGN
        daInputDate = DATE(
            MONTH(daInputDate) + 1,
            1,
            YEAR(daInputDate)
            ).        
    ELSE ASSIGN
        daInputDate = DATE(
            1,
            1,
            YEAR(daInputDate) + 1
            ).        
    RUN ipChangeDates IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK dCalendar 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  
  IF SEARCH("img/tinyleftarrow.jpg") = ? THEN
    image-2:LOAD-IMAGE("./tinyleftarrow.jpg") NO-ERROR.
  IF SEARCH("img/tinyrightarrow.jpg") = ? THEN
    image-3:LOAD-IMAGE("./tinyrightarrow.jpg") NO-ERROR.

  DO i = 1950 TO 2100:
      cbYear:ADD-LAST(STRING(i)).
  END.
  RUN ipChangeDates IN THIS-PROCEDURE.
  
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
  
  ASSIGN
    opOutput = STRING(daOutputDate).

END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI dCalendar  _DEFAULT-DISABLE
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
  HIDE FRAME dCalendar.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI dCalendar  _DEFAULT-ENABLE
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
  DISPLAY cbMonth cbYear fi-Sun fi-Mon fi-Tue fi-Wed fi-Thu fi-Fri fi-Sat fi-1 
          fi-2 fi-3 fi-4 fi-5 fi-6 fi-7 fi-8 fi-9 fi-10 fi-11 fi-12 fi-13 fi-14 
          fi-15 fi-16 fi-17 fi-18 fi-19 fi-20 fi-21 fi-22 fi-23 fi-24 fi-25 
          fi-26 fi-27 fi-28 fi-29 fi-30 fi-31 fi-32 fi-33 fi-34 fi-35 fi-36 
          fi-37 fi-38 fi-39 fi-40 fi-41 fi-42 
      WITH FRAME dCalendar.
  ENABLE IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 cbMonth cbYear fi-1 fi-2 fi-3 fi-4 
         fi-5 fi-6 fi-7 fi-8 fi-9 fi-10 fi-11 fi-12 fi-13 fi-14 fi-15 fi-16 
         fi-17 fi-18 fi-19 fi-20 fi-21 fi-22 fi-23 fi-24 fi-25 fi-26 fi-27 
         fi-28 fi-29 fi-30 fi-31 fi-32 fi-33 fi-34 fi-35 fi-36 fi-37 fi-38 
         fi-39 fi-40 fi-41 fi-42 
      WITH FRAME dCalendar.
  VIEW FRAME dCalendar.
  {&OPEN-BROWSERS-IN-QUERY-dCalendar}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipChangeDates dCalendar 
PROCEDURE ipChangeDates :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR daFirstDate AS DATE NO-UNDO.
    DEF VAR daThisDate AS DATE NO-UNDO.
    DEF VAR iCheckDay AS INT NO-UNDO.
    DEF VAR hStartWidget AS HANDLE NO-UNDO.
    DEF VAR iCtr AS INT NO-UNDO.
    
    /* Delete existing dates and set bgcolor to grey for all fill-ins */
    ASSIGN
        hStartWidget = fi-1:HANDLE IN FRAME dCalendar.
        
    DO iCtr = 1 TO 42:
        ASSIGN 
            hStartWidget:SCREEN-VALUE = ""
            hStartWidget:BGCOLOR = 8
            hStartWidget = hStartWidget:NEXT-SIBLING.
    END.
    
    /* Build the Month/Year header and calculate first day of new month plus day-of-week */
    ASSIGN
        cbMonth:SCREEN-VALUE IN FRAME dCalendar = STRING(ENTRY(MONTH(daInputDate),"Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec"))
        cbYear:SCREEN-VALUE = STRING(YEAR(daInputDate))
        /*
        fiMonthYear:SCREEN-VALUE IN FRAME dCalendar = 
        STRING(ENTRY(MONTH(daInputDate),"January,February,March,April,May,June,July,August,September,October,November,December")) +
        " " + STRING(YEAR(daInputDate))
        fiMonthYear:WIDTH IN FRAME dCalendar = LENGTH(fiMonthYear:SCREEN-VALUE IN FRAME dCalendar) + 2.5
        fiMonthYear:COL IN FRAME dCalendar = (49.8 - fiMonthYear:WIDTH IN FRAME dCalendar) / 2
        */
        daFirstDate = DATE(
            MONTH(daInputDate),
            1,
            YEAR(daInputDate)
            )
        hStartWidget = fi-1:HANDLE IN FRAME dCalendar
        daThisDate = daFirstDate + 1 - WEEKDAY(daFirstDate).
    
    /* Rebuild month calendar, only current month's days are white bg */
    DO iCtr = 1 TO 42:
        ASSIGN 
            hStartWidget:SCREEN-VALUE = STRING(DAY(daThisDate))
            hStartWidget:BGCOLOR = 
                IF daThisDate = TODAY THEN 12 ELSE
                IF MONTH(daThisDate) = MONTH(daFirstDate) THEN 15 ELSE 8
            daThisDate = daThisDate + 1
            hStartWidget = hStartWidget:NEXT-SIBLING.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

