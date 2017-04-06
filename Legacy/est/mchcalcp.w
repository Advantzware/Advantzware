&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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
DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-rowid AS ROWID NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tb_new-mach tb_old-mach Btn_OK Btn_Cancel ~
RECT-17 RECT-39 RECT-40 
&Scoped-Define DISPLAYED-OBJECTS tb_new-mach begin_mach end_mach begin_year ~
end_year begin_date end_date tb_old-mach begin_week end_week finish_date 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 tb_new-mach begin_mach end_mach begin_year end_year ~
begin_date end_date tb_old-mach 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U 
     LABEL "From Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_mach AS CHARACTER FORMAT "X(6)":U 
     LABEL "From Machine" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_week AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_year AS INTEGER FORMAT ">>>>":U INITIAL 0 
     LABEL "From Year" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U 
     LABEL "To Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_mach AS CHARACTER FORMAT "X(6)":U 
     LABEL "To Machine" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_week AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_year AS INTEGER FORMAT ">>>>":U INITIAL 0 
     LABEL "To Year" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE finish_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/2099 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 18.1.

DEFINE RECTANGLE RECT-39
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 86 BY 5.95.

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 86 BY 5.95.

DEFINE VARIABLE tb_new-mach AS LOGICAL INITIAL no 
     LABEL "Copy one machine to another" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_old-mach AS LOGICAL INITIAL no 
     LABEL "Copy within machine" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY 1
     FONT 6 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     tb_new-mach AT ROW 5.05 COL 21
     begin_mach AT ROW 6.48 COL 23 COLON-ALIGNED
     end_mach AT ROW 6.48 COL 59 COLON-ALIGNED
     begin_year AT ROW 7.67 COL 23 COLON-ALIGNED
     end_year AT ROW 7.67 COL 59 COLON-ALIGNED
     begin_date AT ROW 8.86 COL 23 COLON-ALIGNED
     end_date AT ROW 8.86 COL 59 COLON-ALIGNED
     tb_old-mach AT ROW 13.38 COL 21
     begin_week AT ROW 14.81 COL 51 COLON-ALIGNED NO-LABEL
     end_week AT ROW 14.81 COL 68 COLON-ALIGNED NO-LABEL
     finish_date AT ROW 17.19 COL 52 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 20.29 COL 20
     Btn_Cancel AT ROW 20.29 COL 55
     RECT-17 AT ROW 1 COL 1
     RECT-39 AT ROW 4.57 COL 2
     RECT-40 AT ROW 12.67 COL 2
     "If you wish to copy one record, press cancel..." VIEW-AS TEXT
          SIZE 57 BY 1 AT ROW 1.71 COL 4
          FONT 6
     "-" VIEW-AS TEXT
          SIZE 1 BY 1 AT ROW 14.81 COL 69
          FONT 6
     "by copying this Monday to every Monday, Tuesday to Tuesday, and" VIEW-AS TEXT
          SIZE 83 BY 1 AT ROW 16 COL 4
          FONT 6
     "Wednesday to Wednesday, and so on until:" VIEW-AS TEXT
          SIZE 50 BY 1 AT ROW 17.19 COL 4
          FONT 6
     "OR" VIEW-AS TEXT
          SIZE 5 BY 1.24 AT ROW 11.24 COL 38
          FONT 6
     "OR" VIEW-AS TEXT
          SIZE 5 BY 1 AT ROW 3.38 COL 38
          FONT 6
     "This will copy all records from the week of" VIEW-AS TEXT
          SIZE 49 BY 1 AT ROW 14.81 COL 4
          FONT 6
     SPACE(36.00) SKIP(6.99)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Copy Machine Calendar"
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
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
                                                                        */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN begin_date IN FRAME D-Dialog
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN begin_mach IN FRAME D-Dialog
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN begin_week IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN begin_year IN FRAME D-Dialog
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN end_date IN FRAME D-Dialog
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN end_mach IN FRAME D-Dialog
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN end_week IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN end_year IN FRAME D-Dialog
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN finish_date IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_new-mach IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tb_old-mach IN FRAME D-Dialog
   1                                                                    */
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
ON HELP OF FRAME D-Dialog /* Copy Machine Calendar */
DO:
  DEF VAR lv-char AS CHAR NO-UNDO.


  CASE FOCUS:NAME:
    WHEN "begin_mach" THEN RUN lookup-mach.
    WHEN "end_mach" THEN RUN lookup-mach.
  END CASE.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Copy Machine Calendar */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date D-Dialog
ON LEAVE OF begin_date IN FRAME D-Dialog /* From Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mach D-Dialog
ON LEAVE OF begin_mach IN FRAME D-Dialog /* From Machine */
DO:
  ASSIGN {&self-name}.

  RUN valid-mach ({&self-name}) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO WITH FRAME {&FRAME-NAME}:
    APPLY "entry" TO {&self-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mach D-Dialog
ON VALUE-CHANGED OF begin_mach IN FRAME D-Dialog /* From Machine */
DO:
  DEF VAR li AS INT NO-UNDO.


  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  DO li = 1 TO LENGTH({&self-name}:SCREEN-VALUE):
    APPLY "cursor-right" TO {&self-name}.
  END.

  RUN enable-dates.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_week
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_week D-Dialog
ON LEAVE OF begin_week IN FRAME D-Dialog
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_year
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_year D-Dialog
ON LEAVE OF begin_year IN FRAME D-Dialog /* From Year */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_year D-Dialog
ON VALUE-CHANGED OF begin_year IN FRAME D-Dialog /* From Year */
DO:
  RUN enable-dates.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel D-Dialog
ON CHOOSE OF Btn_Cancel IN FRAME D-Dialog /* Cancel */
DO:
  op-rowid = ?.
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* OK */
DO:
  DEF VAR v-process AS LOG NO-UNDO.

  ASSIGN {&LIST-1}.
  RUN valid-end_year NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  v-process = NO.

  MESSAGE "Are you sure you want to copy using these parameters?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

  IF v-process THEN DO:
    RUN copy-records.

    MESSAGE "Copy Process Is Completed" VIEW-AS ALERT-BOX.

    APPLY "close" TO THIS-PROCEDURE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date D-Dialog
ON LEAVE OF end_date IN FRAME D-Dialog /* To Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_mach D-Dialog
ON LEAVE OF end_mach IN FRAME D-Dialog /* To Machine */
DO:
  ASSIGN {&self-name}.

  RUN valid-mach ({&self-name}) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO WITH FRAME {&FRAME-NAME}:
    APPLY "entry" TO {&self-name}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_mach D-Dialog
ON VALUE-CHANGED OF end_mach IN FRAME D-Dialog /* To Machine */
DO:
  DEF VAR li AS INT NO-UNDO.


  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  DO li = 1 TO LENGTH({&self-name}:SCREEN-VALUE):
    APPLY "cursor-right" TO {&self-name}.
  END.

  RUN enable-dates.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_week
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_week D-Dialog
ON LEAVE OF end_week IN FRAME D-Dialog
DO:
  ASSIGN {&self-name}.

  IF finish_date LT end_week THEN DO:
    MESSAGE "Must after week ending date..." VIEW-AS ALERT-BOX ERROR.
    APPLY "entry" TO finish_date.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_year
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_year D-Dialog
ON LEAVE OF end_year IN FRAME D-Dialog /* To Year */
DO:
  RUN valid-end_year NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_year D-Dialog
ON VALUE-CHANGED OF end_year IN FRAME D-Dialog /* To Year */
DO:
  RUN enable-dates.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME finish_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL finish_date D-Dialog
ON LEAVE OF finish_date IN FRAME D-Dialog
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_new-mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_new-mach D-Dialog
ON VALUE-CHANGED OF tb_new-mach IN FRAME D-Dialog /* Copy one machine to another */
DO:
  ASSIGN {&self-name}.

  IF {&self-name} THEN DO WITH FRAME {&FRAME-NAME}:
    ENABLE begin_mach end_mach begin_year end_year btn_ok.

    DISABLE finish_date.

    ASSIGN
     tb_old-mach:SCREEN-VALUE = "no"
     tb_old-mach              = NO.

    RUN enable-dates.
  END.

  ELSE RUN dispatch ("local-enable").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_old-mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_old-mach D-Dialog
ON VALUE-CHANGED OF tb_old-mach IN FRAME D-Dialog /* Copy within machine */
DO:
  ASSIGN {&self-name}.

  IF {&self-name} THEN DO WITH FRAME {&FRAME-NAME}:
    ENABLE finish_date btn_ok.

    DISABLE begin_mach end_mach begin_year end_year begin_date end_date.

    ASSIGN
     tb_new-mach:SCREEN-VALUE = "no"
     tb_new-mach              = NO.

    RUN enable-dates.
  END.

  ELSE RUN dispatch ("local-enable").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
FIND mach-calendar WHERE ROWID(mach-calendar) EQ ip-rowid NO-LOCK NO-ERROR.

IF NOT AVAIL mach-calendar THEN APPLY "choose" TO btn_cancel.

ASSIGN
 cocode = g_company
 locode = g_loc.

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adjust-date D-Dialog 
PROCEDURE adjust-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT-OUTPUT PARAM io-date AS DATE NO-UNDO.

  DEF VAR li-adj-days AS INT NO-UNDO.


  li-adj-days = WEEKDAY(mach-calendar.m-date) - WEEKDAY(io-date).

  IF li-adj-days LT 0 THEN li-adj-days = li-adj-days + 7.

  io-date = io-date + li-adj-days.

  IF YEAR(io-date) NE end_year THEN
    ASSIGN
     li-adj-days = (IF end_year MODULO 4 EQ 0 THEN 366 ELSE 365) *
                   (IF YEAR(io-date) GT end_year THEN -1 ELSE 1)
     io-date     = io-date + li-adj-days.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-records D-Dialog 
PROCEDURE copy-records :
/* --------------------------------------------------- ce/op-copy.i 06/98 JLF */
/* Routing Copy                                                               */
/* -------------------------------------------------------------------------- */
DEF VAR ll-one-day AS LOG NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR ld AS DEC NO-UNDO.
DEF VAR lv-new-date AS DATE NO-UNDO.
DEF VAR ll-new-mach AS LOG NO-UNDO.


SESSION:SET-WAIT-STATE ("general").

ASSIGN
 ll-new-mach = begin_mach NE end_mach
 ll-one-day  = (NOT ll-new-mach) AND begin_year EQ end_year AND tb_new-mach.

IF NOT ll-new-mach THEN end_year = begin_year.

IF tb_new-mach THEN
FOR EACH mach-calendar
    WHERE mach-calendar.company EQ cocode
      AND mach-calendar.m-code  EQ begin_mach
      AND ((YEAR(mach-calendar.m-date) EQ begin_year AND NOT ll-new-mach) OR
           (YEAR(mach-calendar.m-date) GE begin_year AND
            YEAR(mach-calendar.m-date) LE end_year AND ll-new-mach) OR
           (mach-calendar.m-date EQ begin_date AND ll-one-day))
    NO-LOCK
    BREAK BY mach-calendar.m-date:

  lv-new-date = IF ll-new-mach THEN mach-calendar.m-date
                ELSE
                IF ll-one-day THEN end_date
                              ELSE (DATE(MONTH(mach-calendar.m-date),
                                         DAY(mach-calendar.m-date),
                                         end_year))
                NO-ERROR.

  IF NOT ERROR-STATUS:ERROR THEN DO:
    IF NOT ll-one-day                                        AND
       begin_year NE end_year                                AND
       WEEKDAY(mach-calendar.m-date) NE WEEKDAY(lv-new-date) THEN
      RUN adjust-date (INPUT-OUTPUT lv-new-date).

    RUN create-new-record (FIRST-OF(mach-calendar.m-date), lv-new-date).
  END.
END.

ELSE
ASSIGN
 begin_mach = mach-calendar.m-code
 end_mach   = begin_mach.

IF tb_old-mach THEN DO:
  ld = (finish_date - end_week) / 7.
  {sys/inc/roundup.i ld}

  FOR EACH mach-calendar
      WHERE mach-calendar.company EQ cocode
        AND mach-calendar.m-code  EQ begin_mach
        AND mach-calendar.m-date  GE begin_week
        AND mach-calendar.m-date  LE end_week
      NO-LOCK
      BREAK BY mach-calendar.m-date:

    DO li = 1 TO ld:
      lv-new-date = mach-calendar.m-date + (li * 7).

      IF lv-new-date LE finish_date THEN
        RUN create-new-record (FIRST-OF(mach-calendar.m-date), lv-new-date).
    END.
  END.
END.

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-new-record D-Dialog 
PROCEDURE create-new-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-first-of AS LOG NO-UNDO.
  DEF INPUT PARAM ip-new-date AS DATE NO-UNDO.

  DEF BUFFER b-mach-cal FOR mach-calendar.


  IF ip-first-of THEN
  FOR EACH b-mach-cal
      WHERE b-mach-cal.company EQ mach-calendar.company
        AND b-mach-cal.m-code  EQ end_mach
        AND b-mach-cal.m-date  EQ ip-new-date:
    DELETE b-mach-cal.
  END.

  CREATE b-mach-cal.
  BUFFER-COPY mach-calendar EXCEPT rec_key TO b-mach-cal
  ASSIGN
   b-mach-cal.m-code = CAPS(end_mach)
   b-mach-cal.m-date = ip-new-date
   op-rowid          = ROWID(b-mach-cal).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-dates D-Dialog 
PROCEDURE enable-dates :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     begin_date:SENSITIVE = NO
     end_date:SENSITIVE   = NO
     begin_date:HIDDEN    = NO
     end_date:HIDDEN      = NO.

    IF FOCUS:NAME NE "end_year" THEN end_year:SENSITIVE = NO.

    IF tb_new-mach:SCREEN-VALUE EQ "yes" THEN DO:
      IF FOCUS:NAME NE "end_year" THEN
        IF begin_mach:SCREEN-VALUE EQ end_mach:SCREEN-VALUE THEN
          end_year:SCREEN-VALUE = begin_year:SCREEN-VALUE.
        ELSE
          end_year:SENSITIVE = YES.

      IF begin_mach:SCREEN-VALUE EQ end_mach:SCREEN-VALUE AND
         begin_year:SCREEN-VALUE EQ end_year:SCREEN-VALUE THEN
        ASSIGN
         begin_date:SENSITIVE = YES
         end_date:SENSITIVE   = YES.
      ELSE
        ASSIGN
         begin_date:HIDDEN    = YES
         end_date:HIDDEN      = YES.  
    END.
  END.

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
  DISPLAY tb_new-mach begin_mach end_mach begin_year end_year begin_date 
          end_date tb_old-mach begin_week end_week finish_date 
      WITH FRAME D-Dialog.
  ENABLE tb_new-mach tb_old-mach Btn_OK Btn_Cancel RECT-17 RECT-39 RECT-40 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable D-Dialog 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE ALL.

    ENABLE tb_new-mach tb_old-mach btn_cancel.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN
   begin_mach = CAPS(mach-calendar.m-code)
   end_mach   = begin_mach
   begin_year = YEAR(mach-calendar.m-date)
   end_year   = begin_year
   begin_date = mach-calendar.m-date
   end_date   = mach-calendar.m-date + 1
   begin_week = mach-calendar.m-date - WEEKDAY(mach-calendar.m-date) + 1
   end_week   = begin_week + 6.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lookup-mach D-Dialog 
PROCEDURE lookup-mach :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-char AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    RUN windows/l-mach.w (cocode, locode, FOCUS:SCREEN-VALUE, OUTPUT lv-char).
    IF lv-char NE "" AND lv-char NE FOCUS:SCREEN-VALUE THEN DO:
      FOCUS:SCREEN-VALUE = ENTRY(1,lv-char).
      APPLY "value-changed" TO FOCUS.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-end_year D-Dialog 
PROCEDURE valid-end_year :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     begin_year
     end_year.
    /*
    IF (begin_mach EQ end_mach AND begin_year GE end_year AND TB_NEW-MACH ) OR
       (begin_mach NE end_mach AND begin_year GT end_year) THEN DO:


      IF begin_mach EQ end_mach THEN
        MESSAGE  TRIM(end_year:LABEL) + " must be greater than " + TRIM(begin_year:LABEL)
            VIEW-AS ALERT-BOX ERROR.
      ELSE
        MESSAGE TRIM(end_year:LABEL) + " must not be less than " + TRIM(begin_year:LABEL)
            VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO end_year.
      RETURN ERROR.
    END.
    */

    IF TB_NEW-MACH THEN DO:
       IF begin_mach = END_mach THEN DO:
          MESSAGE "Choose 'Copy within machine' option please..." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO tb_old-mach.
          RETURN ERROR.
       END.
       IF begin_date > END_date THEN DO:
          MESSAGE "From Date must not be less than To Date..." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO END_date.
          RETURN ERROR.
       END.
    END.
    ELSE IF TB_OLD-MACH THEN DO:
        IF finish_date <= END_week THEN DO:
           MESSAGE "Invalid Date to copy..." VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO finish_date.
           RETURN ERROR.
        END.

    END.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-mach D-Dialog 
PROCEDURE valid-mach :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-mach LIKE mach-calendar.m-code NO-UNDO.


  IF NOT CAN-FIND(FIRST mach
                  WHERE mach.company EQ cocode
                    AND mach.loc     EQ locode
                    AND mach.m-code  EQ ip-mach) THEN DO:
    MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

