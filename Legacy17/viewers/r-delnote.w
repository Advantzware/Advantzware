&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: viewers\r-delnote.w

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
DEF INPUT PARAM ip-rec-key AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */

{custom/globdefs.i}
{sys/inc/VAR.i "new shared" }
ASSIGN cocode = g_company
       locode = g_loc.

DEFINE TEMP-TABLE tt-dept NO-UNDO
   FIELD dept AS CHAR
   FIELD COUNT AS INT
   INDEX dept dept
   INDEX COUNT COUNT.

DEF VAR v-dept-count AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btn-all btn-selected RECT-5 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-all 
     LABEL "Delete All Notes" 
     SIZE 26 BY 1.14.

DEFINE BUTTON btn-selected 
     LABEL "Delete Selected Departments" 
     SIZE 37 BY 1.14.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 76 BY 12.38.

DEFINE VARIABLE TOGGLE-1 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-10 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-11 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-12 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-13 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-14 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-15 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-16 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-17 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-18 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-19 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-2 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-20 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-21 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-22 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-23 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-24 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-25 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-26 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-27 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-28 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-29 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-3 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-30 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-31 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-32 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-33 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-34 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-35 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-36 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-37 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-38 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-39 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-4 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-40 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-5 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-6 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-7 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-8 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-9 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     btn-all AT ROW 1.71 COL 32.4 WIDGET-ID 6
     TOGGLE-1 AT ROW 5.05 COL 9 WIDGET-ID 14
     TOGGLE-2 AT ROW 6.05 COL 9 WIDGET-ID 16
     TOGGLE-3 AT ROW 7.05 COL 9 WIDGET-ID 18
     TOGGLE-4 AT ROW 8.05 COL 9 WIDGET-ID 20
     TOGGLE-5 AT ROW 9.05 COL 9 WIDGET-ID 22
     TOGGLE-6 AT ROW 10.05 COL 9 WIDGET-ID 24
     TOGGLE-7 AT ROW 11.05 COL 9 WIDGET-ID 26
     TOGGLE-8 AT ROW 12.05 COL 9 WIDGET-ID 28
     TOGGLE-9 AT ROW 13.05 COL 9 WIDGET-ID 30
     TOGGLE-10 AT ROW 14.05 COL 9 WIDGET-ID 32
     TOGGLE-11 AT ROW 5.05 COL 27.2 WIDGET-ID 34
     TOGGLE-12 AT ROW 6.05 COL 27.2 WIDGET-ID 38
     TOGGLE-13 AT ROW 7.05 COL 27.2 WIDGET-ID 40
     TOGGLE-14 AT ROW 8.05 COL 27.2 WIDGET-ID 42
     TOGGLE-15 AT ROW 9.05 COL 27.2 WIDGET-ID 44
     TOGGLE-16 AT ROW 10.05 COL 27.2 WIDGET-ID 46
     TOGGLE-17 AT ROW 11.05 COL 27.2 WIDGET-ID 48
     TOGGLE-18 AT ROW 12.05 COL 27.2 WIDGET-ID 50
     TOGGLE-19 AT ROW 13.05 COL 27.2 WIDGET-ID 52
     TOGGLE-20 AT ROW 14.05 COL 27.2 WIDGET-ID 36
     TOGGLE-21 AT ROW 5.05 COL 44 WIDGET-ID 54
     TOGGLE-22 AT ROW 6.05 COL 44 WIDGET-ID 58
     TOGGLE-23 AT ROW 7.05 COL 44 WIDGET-ID 60
     TOGGLE-24 AT ROW 8.05 COL 44 WIDGET-ID 62
     TOGGLE-25 AT ROW 9.05 COL 44 WIDGET-ID 64
     TOGGLE-26 AT ROW 10.05 COL 44 WIDGET-ID 66
     TOGGLE-27 AT ROW 11.05 COL 44 WIDGET-ID 68
     TOGGLE-28 AT ROW 12.05 COL 44 WIDGET-ID 70
     TOGGLE-29 AT ROW 13.05 COL 44 WIDGET-ID 72
     TOGGLE-30 AT ROW 14.05 COL 44 WIDGET-ID 56
     TOGGLE-31 AT ROW 5.05 COL 62 WIDGET-ID 74
     TOGGLE-32 AT ROW 6.05 COL 62 WIDGET-ID 78
     TOGGLE-33 AT ROW 7.05 COL 62 WIDGET-ID 80
     TOGGLE-34 AT ROW 8.05 COL 62 WIDGET-ID 82
     TOGGLE-35 AT ROW 9.05 COL 62 WIDGET-ID 84
     TOGGLE-36 AT ROW 10.05 COL 62 WIDGET-ID 86
     TOGGLE-37 AT ROW 11.05 COL 62 WIDGET-ID 88
     TOGGLE-38 AT ROW 12.05 COL 62 WIDGET-ID 90
     TOGGLE-39 AT ROW 13.05 COL 62 WIDGET-ID 92
     TOGGLE-40 AT ROW 14.05 COL 62 WIDGET-ID 76
     btn-selected AT ROW 16.48 COL 28.2 WIDGET-ID 8
     "Department Notes to Delete" VIEW-AS TEXT
          SIZE 33 BY .62 AT ROW 4.1 COL 29 WIDGET-ID 12
          FONT 6
     RECT-5 AT ROW 3.62 COL 5.6 WIDGET-ID 10
     SPACE(5.19) SKIP(3.37)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Delete Notes".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Design Page: 2
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
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-1 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-1:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-10 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-10:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-11 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-11:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-12 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-12:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-13 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-13:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-14 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-14:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-15 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-15:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-16 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-16:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-17 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-17:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-18 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-18:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-19 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-19:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-2 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-2:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-20 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-20:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-21 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-21:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-22 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-22:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-23 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-23:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-24 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-24:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-25 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-25:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-26 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-26:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-27 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-27:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-28 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-28:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-29 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-29:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-3 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-3:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-30 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-30:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-31 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-31:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-32 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-32:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-33 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-33:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-34 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-34:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-35 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-35:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-36 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-36:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-37 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-37:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-38 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-38:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-39 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-39:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-4 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-4:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-40 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-40:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-5 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-5:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-6 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-6:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-7 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-7:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-8 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-8:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-9 IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-9:HIDDEN IN FRAME D-Dialog           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Delete Notes */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-all D-Dialog
ON CHOOSE OF btn-all IN FRAME D-Dialog /* Delete All Notes */
DO:
   DEF VAR ll-ans AS LOG NO-UNDO.

   MESSAGE "Are you sure you wish to delete all notes?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE ll-ans.

   IF ll-ans THEN
   DO:
      SESSION:SET-WAIT-STATE("GENERAL").
      FOR EACH notes WHERE 
          notes.rec_key = ip-rec-key AND
          notes.note_type <> "S":

          DELETE notes.
      END.
      SESSION:SET-WAIT-STATE(""). 

      MESSAGE "All Notes Deleted."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

      APPLY "END-ERROR":U TO SELF.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-selected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-selected D-Dialog
ON CHOOSE OF btn-selected IN FRAME D-Dialog /* Delete Selected Departments */
DO:
   DEF VAR ll-ans AS LOG NO-UNDO.
   DEF VAR v-dept-list AS CHAR FORMAT "X(120)".

   MESSAGE "Are you sure you wish to delete selected departments?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE ll-ans.

   IF ll-ans THEN
      DO WITH FRAME {&FRAME-NAME}:

         ASSIGN
            toggle-1 toggle-2 toggle-3 toggle-4 toggle-5 toggle-6
            toggle-7 toggle-8 toggle-9 toggle-10 toggle-11 toggle-12
            toggle-13 toggle-14 toggle-15 toggle-16 toggle-17 toggle-18
            toggle-19 toggle-20 toggle-21 toggle-22 toggle-23 toggle-24
            toggle-25 toggle-26 toggle-27 toggle-28 toggle-29 toggle-30
            toggle-31 toggle-32 toggle-33 toggle-34 toggle-35 toggle-36
            toggle-37 toggle-38 toggle-39 toggle-40.

         IF toggle-1 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 1
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-2 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 2
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-3 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 3
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-4 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 4
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-5 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 5
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-6 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 6
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-7 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 7
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-8 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 8
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-9 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 9
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-10 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 10
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-11 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 11
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-12 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 12
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-13 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 13
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-14 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 14
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-15 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 15
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-16 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 16
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-17 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 17
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-18 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 18
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-19 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 19
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-20 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 20
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-21 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 21
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-22 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 22
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-23 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 23
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-24 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 24
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-25 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 25
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-26 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 26
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-27 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 27
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-28 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 28
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-29 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 29
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-30 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 30
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-31 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 31
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-32 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 32
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-33 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 33
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-34 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 34
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-35 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 35
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-36 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 36
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-37 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 37
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-38 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 38
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-39 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 39
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         IF toggle-40 EQ YES THEN
         DO:
            FIND FIRST tt-dept WHERE
                 tt-dept.COUNT = 40
                 NO-ERROR.

              IF AVAIL tt-dept THEN
              DO:
                 v-dept-list = tt-dept.dept + ",".
                 RELEASE tt-dept.
              END.
         END.

         SESSION:SET-WAIT-STATE("GENERAL").
         FOR EACH notes WHERE 
             notes.rec_key = ip-rec-key AND
             notes.note_type <> "S" AND
             notes.note_code NE "" AND
             LOOKUP(notes.note_code,v-dept-list) GT 0:

             DELETE notes.
         END.
         SESSION:SET-WAIT-STATE("").

         MESSAGE "Selected Notes Deleted."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

         APPLY "END-ERROR":U TO SELF.
      END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

RUN init-boxes-proc.

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
  ENABLE btn-all btn-selected RECT-5 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-boxes-proc D-Dialog 
PROCEDURE init-boxes-proc :
EMPTY TEMP-TABLE tt-dept.

   FOR EACH notes fields(note_code) WHERE 
       notes.rec_key = ip-rec-key AND
       notes.note_type <> "S" AND
       notes.note_code NE ""
       NO-LOCK
       BREAK BY notes.note_code:

       IF FIRST-OF(notes.note_code) THEN
       DO:
          CREATE tt-dept.
          ASSIGN tt-dept.dept = notes.note_code
                 v-dept-count = v-dept-count + 1
                 tt-dept.COUNT = v-dept-count.
          RELEASE tt-dept.
       END.
   END.

   DO WITH FRAME {&FRAME-NAME}:

      FOR EACH tt-dept:

          CASE tt-dept.COUNT:

             WHEN 1 THEN
                ASSIGN toggle-1:LABEL = tt-dept.dept
                       toggle-1:SENSITIVE = YES
                       toggle-1:HIDDEN = NO.

             WHEN 2 THEN
                ASSIGN toggle-2:LABEL = tt-dept.dept
                       toggle-2:SENSITIVE = YES
                       toggle-2:HIDDEN = NO.

             WHEN 3 THEN
                ASSIGN toggle-3:LABEL = tt-dept.dept
                       toggle-3:SENSITIVE = YES
                       toggle-3:HIDDEN = NO.

             WHEN 4 THEN
                ASSIGN toggle-4:LABEL = tt-dept.dept
                       toggle-4:SENSITIVE = YES
                       toggle-4:HIDDEN = NO.

             WHEN 5 THEN
                ASSIGN toggle-5:LABEL = tt-dept.dept
                       toggle-5:SENSITIVE = YES
                       toggle-5:HIDDEN = NO.

             WHEN 6 THEN
                ASSIGN toggle-6:LABEL = tt-dept.dept
                       toggle-6:SENSITIVE = YES
                       toggle-6:HIDDEN = NO.

             WHEN 7 THEN
                ASSIGN toggle-7:LABEL = tt-dept.dept
                       toggle-7:SENSITIVE = YES
                       toggle-7:HIDDEN = NO.

             WHEN 8 THEN
                ASSIGN toggle-8:LABEL = tt-dept.dept
                       toggle-8:SENSITIVE = YES
                       toggle-8:HIDDEN = NO.

             WHEN 9 THEN
                ASSIGN toggle-9:LABEL = tt-dept.dept
                       toggle-9:SENSITIVE = YES
                       toggle-9:HIDDEN = NO.

             WHEN 10 THEN
                ASSIGN toggle-10:LABEL = tt-dept.dept
                       toggle-10:SENSITIVE = YES
                       toggle-10:HIDDEN = NO.

             WHEN 11 THEN
                ASSIGN toggle-11:LABEL = tt-dept.dept
                       toggle-11:SENSITIVE = YES
                       toggle-11:HIDDEN = NO.

             WHEN 12 THEN
                ASSIGN toggle-12:LABEL = tt-dept.dept
                       toggle-12:SENSITIVE = YES
                       toggle-12:HIDDEN = NO.

             WHEN 13 THEN
                ASSIGN toggle-13:LABEL = tt-dept.dept
                       toggle-13:SENSITIVE = YES
                       toggle-13:HIDDEN = NO.

             WHEN 14 THEN
                ASSIGN toggle-14:LABEL = tt-dept.dept
                       toggle-14:SENSITIVE = YES
                       toggle-14:HIDDEN = NO.

             WHEN 15 THEN
                ASSIGN toggle-15:LABEL = tt-dept.dept
                       toggle-15:SENSITIVE = YES
                       toggle-15:HIDDEN = NO.

             WHEN 16 THEN
                ASSIGN toggle-16:LABEL = tt-dept.dept
                       toggle-16:SENSITIVE = YES
                       toggle-16:HIDDEN = NO.

             WHEN 17 THEN
                ASSIGN toggle-17:LABEL = tt-dept.dept
                       toggle-17:SENSITIVE = YES
                       toggle-17:HIDDEN = NO.

             WHEN 18 THEN
                ASSIGN toggle-18:LABEL = tt-dept.dept
                       toggle-18:SENSITIVE = YES
                       toggle-18:HIDDEN = NO.

             WHEN 19 THEN
                ASSIGN toggle-19:LABEL = tt-dept.dept
                       toggle-19:SENSITIVE = YES
                       toggle-19:HIDDEN = NO.

             WHEN 20 THEN
                ASSIGN toggle-20:LABEL = tt-dept.dept
                       toggle-20:SENSITIVE = YES
                       toggle-20:HIDDEN = NO.

             WHEN 21 THEN
                ASSIGN toggle-21:LABEL = tt-dept.dept
                       toggle-21:SENSITIVE = YES
                       toggle-21:HIDDEN = NO.

             WHEN 22 THEN
                ASSIGN toggle-22:LABEL = tt-dept.dept
                       toggle-22:SENSITIVE = YES
                       toggle-22:HIDDEN = NO.

             WHEN 23 THEN
                ASSIGN toggle-23:LABEL = tt-dept.dept
                       toggle-23:SENSITIVE = YES
                       toggle-23:HIDDEN = NO.

             WHEN 24 THEN
                ASSIGN toggle-24:LABEL = tt-dept.dept
                       toggle-24:SENSITIVE = YES
                       toggle-24:HIDDEN = NO.

             WHEN 25 THEN
                ASSIGN toggle-25:LABEL = tt-dept.dept
                       toggle-25:SENSITIVE = YES
                       toggle-25:HIDDEN = NO.

             WHEN 26 THEN
                ASSIGN toggle-26:LABEL = tt-dept.dept
                       toggle-26:SENSITIVE = YES
                       toggle-26:HIDDEN = NO.

             WHEN 27 THEN
                ASSIGN toggle-27:LABEL = tt-dept.dept
                       toggle-27:SENSITIVE = YES
                       toggle-27:HIDDEN = NO.

             WHEN 28 THEN
                ASSIGN toggle-28:LABEL = tt-dept.dept
                       toggle-28:SENSITIVE = YES
                       toggle-28:HIDDEN = NO.

             WHEN 29 THEN
                ASSIGN toggle-29:LABEL = tt-dept.dept
                       toggle-29:SENSITIVE = YES
                       toggle-29:HIDDEN = NO.

             WHEN 30 THEN
                ASSIGN toggle-30:LABEL = tt-dept.dept
                       toggle-30:SENSITIVE = YES
                       toggle-30:HIDDEN = NO.

             WHEN 31 THEN
                ASSIGN toggle-31:LABEL = tt-dept.dept
                       toggle-31:SENSITIVE = YES
                       toggle-31:HIDDEN = NO.

             WHEN 32 THEN
                ASSIGN toggle-32:LABEL = tt-dept.dept
                       toggle-32:SENSITIVE = YES
                       toggle-32:HIDDEN = NO.

             WHEN 33 THEN
                ASSIGN toggle-33:LABEL = tt-dept.dept
                       toggle-33:SENSITIVE = YES
                       toggle-33:HIDDEN = NO.

             WHEN 34 THEN
                ASSIGN toggle-34:LABEL = tt-dept.dept
                       toggle-34:SENSITIVE = YES
                       toggle-34:HIDDEN = NO.

             WHEN 35 THEN
                ASSIGN toggle-35:LABEL = tt-dept.dept
                       toggle-35:SENSITIVE = YES
                       toggle-35:HIDDEN = NO.

             WHEN 36 THEN
                ASSIGN toggle-36:LABEL = tt-dept.dept
                       toggle-36:SENSITIVE = YES
                       toggle-36:HIDDEN = NO.

             WHEN 37 THEN
                ASSIGN toggle-37:LABEL = tt-dept.dept
                       toggle-37:SENSITIVE = YES
                       toggle-37:HIDDEN = NO.

             WHEN 38 THEN
                ASSIGN toggle-38:LABEL = tt-dept.dept
                       toggle-38:SENSITIVE = YES
                       toggle-38:HIDDEN = NO.

             WHEN 39 THEN
                ASSIGN toggle-39:LABEL = tt-dept.dept
                       toggle-39:SENSITIVE = YES
                       toggle-39:HIDDEN = NO.

             WHEN 40 THEN
                ASSIGN toggle-40:LABEL = tt-dept.dept
                       toggle-40:SENSITIVE = YES
                       toggle-40:HIDDEN = NO.
          END CASE.
      END.
   END.

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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

