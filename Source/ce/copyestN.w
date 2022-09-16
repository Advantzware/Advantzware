&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ce\copyest.w

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

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

DEFINE INPUT PARAMETER ip-copy-est AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ip-new-est AS CHARACTER NO-UNDO .
DEFINE OUTPUT PARAMETER op-new-est AS CHARACTER NO-UNDO .

DEFINE VARIABLE lv-cust-no LIKE eb.cust-no NO-UNDO.
DEFINE BUFFER bv-cust   FOR cust.
DEFINE BUFFER bv-shipto FOR shipto.
{sys/inc/var.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 RECT-18 RECT-19 RECT-20 from_company ~
from_est tb_copy tb_copy-i-name tb_die tb_copy-dscr-1 tb_plate ~
tb_copy-dscr-2 tb_i-no tb_copy-notes tb_farm tb_dept-notes tb_copy-prep ~
tb_clip-att tbCopyCosts to_company fi_cust fi_part fi_shipto fi_rep ~
btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS from_company from_name from_est from_ship ~
from_part tb_copy tb_copy-i-name tb_die tb_copy-dscr-1 tb_plate ~
tb_copy-dscr-2 tb_i-no tb_copy-notes tb_farm tb_dept-notes tb_copy-prep ~
tb_clip-att tbCopyCosts to_company to_name to_est fi_cust fi_part fi_shipto ~
fi_rep 

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
     SIZE 16 BY 1.29.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 16 BY 1.29.

DEFINE VARIABLE fi_cust AS CHARACTER FORMAT "X(8)":U 
     LABEL "New Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_part AS CHARACTER FORMAT "X(15)":U 
     LABEL "New Customer Part#" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE fi_rep AS CHARACTER FORMAT "X(5)":U 
     LABEL "New Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_shipto AS CHARACTER FORMAT "X(15)":U 
     LABEL "New Ship To#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE from_company AS CHARACTER FORMAT "XXX" INITIAL "001" 
     LABEL "From Company" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE from_est AS CHARACTER FORMAT "X(8)" 
     LABEL "From Estimate#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE from_name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 48 BY 1 NO-UNDO.

DEFINE VARIABLE from_part AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 48 BY 1 NO-UNDO.

DEFINE VARIABLE from_ship AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 48 BY 1 NO-UNDO.

DEFINE VARIABLE to_company AS CHARACTER FORMAT "XXX" INITIAL "001" 
     LABEL "To Company" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE to_est AS CHARACTER FORMAT "X(8)" 
     LABEL "To Estimate#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE to_name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 48 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY 19.67.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 10.95.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 7.14.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 71 BY 6.86.

DEFINE VARIABLE tbCopyCosts AS LOGICAL INITIAL yes 
     LABEL "Copy Additional Costs" 
     VIEW-AS TOGGLE-BOX
     SIZE 24.6 BY 1 NO-UNDO.

DEFINE VARIABLE tb_clip-att AS LOGICAL INITIAL yes 
     LABEL "Copy Paper Clip Attachments?" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE tb_copy AS LOGICAL INITIAL yes 
     LABEL "Copy Routing?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE tb_copy-dscr-1 AS LOGICAL INITIAL yes 
     LABEL "Copy Item Description 1?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE tb_copy-dscr-2 AS LOGICAL INITIAL yes 
     LABEL "Copy Item Description 2?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE tb_copy-i-name AS LOGICAL INITIAL yes 
     LABEL "Copy Item Name?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE tb_copy-notes AS LOGICAL INITIAL yes 
     LABEL "Copy Book Icon Spec Notes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE tb_copy-prep AS LOGICAL INITIAL yes 
     LABEL "Copy Prep?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE tb_dept-notes AS LOGICAL INITIAL yes 
     LABEL " Copy Fountain Pen Icon Dept Notes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE tb_die AS LOGICAL INITIAL yes 
     LABEL "Copy Die#?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE tb_farm AS LOGICAL INITIAL no 
     LABEL "Copy Farm Out Costs?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_i-no AS LOGICAL INITIAL no 
     LABEL "Copy FG Item#?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE tb_plate AS LOGICAL INITIAL no 
     LABEL "Copy Plate#?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     from_company AT ROW 3.14 COL 22 COLON-ALIGNED HELP
          "Enter Company To Copy From"
     from_name AT ROW 3.14 COL 39 COLON-ALIGNED NO-LABEL
     from_est AT ROW 4.1 COL 22 COLON-ALIGNED HELP
          "Enter Estimate# to Be Copied"
     from_ship AT ROW 4.1 COL 39 COLON-ALIGNED NO-LABEL
     from_part AT ROW 5.05 COL 39 COLON-ALIGNED NO-LABEL
     tb_copy AT ROW 6.62 COL 16.4
     tb_copy-i-name AT ROW 6.62 COL 42.6 WIDGET-ID 4
     tb_die AT ROW 7.52 COL 16.4
     tb_copy-dscr-1 AT ROW 7.52 COL 42.6 WIDGET-ID 6
     tb_plate AT ROW 8.43 COL 16.4
     tb_copy-dscr-2 AT ROW 8.43 COL 42.6 WIDGET-ID 8
     tb_i-no AT ROW 9.33 COL 16.4
     tb_copy-notes AT ROW 9.33 COL 42.6 WIDGET-ID 2
     tb_farm AT ROW 10.24 COL 16.4
     tb_dept-notes AT ROW 10.24 COL 42.6 WIDGET-ID 14
     tb_copy-prep AT ROW 11.1 COL 16.4 WIDGET-ID 20
     tb_clip-att AT ROW 11.14 COL 42.6 WIDGET-ID 16
     tbCopyCosts AT ROW 12 COL 16.4 WIDGET-ID 22
     to_company AT ROW 14.76 COL 26 COLON-ALIGNED HELP
          "Copy To Company"
     to_name AT ROW 14.76 COL 43 COLON-ALIGNED NO-LABEL
     to_est AT ROW 15.71 COL 26 COLON-ALIGNED HELP
          "Enter Estimate# to Be To"
     fi_cust AT ROW 16.67 COL 26 COLON-ALIGNED HELP
          "Enter New Customer Number"
     fi_part AT ROW 17.62 COL 26 COLON-ALIGNED HELP
          "Enter New Customer Part Number"
     fi_shipto AT ROW 18.57 COL 26 COLON-ALIGNED HELP
          "Enter New Customer Part Number" WIDGET-ID 10
     fi_rep AT ROW 19.52 COL 26 COLON-ALIGNED HELP
          "Enter New Customer Part Number" WIDGET-ID 12
     btn-process AT ROW 21.57 COL 26.8
     btn-cancel AT ROW 21.57 COL 53
     " C O P Y  T O" VIEW-AS TEXT
          SIZE 13 BY 1 AT ROW 13.38 COL 41
          BGCOLOR 8 
     " Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .95 AT ROW 1.24 COL 4
     " C O P Y  F R O M" VIEW-AS TEXT
          SIZE 17 BY 1 AT ROW 1.95 COL 38
          BGCOLOR 8 
     RECT-17 AT ROW 1.81 COL 3
     RECT-18 AT ROW 2.43 COL 4
     RECT-19 AT ROW 13.86 COL 5
     RECT-20 AT ROW 6.29 COL 12.6 WIDGET-ID 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 94.8 BY 22.1
         BGCOLOR 15 .


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
         TITLE              = "Copy Estimate"
         HEIGHT             = 22.1
         WIDTH              = 94.8
         MAX-HEIGHT         = 26.62
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 26.62
         VIRTUAL-WIDTH      = 160
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

ASSIGN 
       from_company:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       from_est:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN from_name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN from_part IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN from_ship IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       to_company:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN to_est IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       to_est:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN to_name IN FRAME FRAME-A
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Copy Estimate */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Copy Estimate */
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
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
        DEFINE VARIABLE v-process AS LOG INIT NO NO-UNDO.


        APPLY "entry" TO from_company.
        RUN valid-company NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-est NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        APPLY "entry" TO to_company.
        RUN valid-company NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-cust NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-part NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-shipto NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-rep NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-terms NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        ASSIGN {&DISPLAYED-OBJECTS}.

        DO TRANSACTION:

            REPEAT:

                FIND FIRST ce-ctrl
                    WHERE ce-ctrl.company EQ to_company
                    AND ce-ctrl.loc     EQ locode
                    EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

                IF AVAILABLE ce-ctrl THEN
                DO:
                    ASSIGN
                        to_est              = STRING(ce-ctrl.e-num + 1,">>>>>>>>")
                        to_est:SCREEN-VALUE = to_est
                        ce-ctrl.e-num       = ce-ctrl.e-num + 1.
                    op-new-est  = to_est:SCREEN-VALUE .
                    FIND CURRENT ce-ctrl NO-LOCK.
                    LEAVE.
                END.
            END.
        END. /* do for ce-ctrl */

        RELEASE ce-ctrl.

        MESSAGE "Are you sure you want to copy this estimate?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

        IF v-process THEN RUN run-process.

        ELSE
        DO TRANSACTION:

            REPEAT:

                FIND FIRST ce-ctrl
                    WHERE ce-ctrl.company EQ to_company
                    AND ce-ctrl.loc     EQ locode
                    EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

                IF AVAILABLE ce-ctrl THEN
                DO:

                    IF ce-ctrl.e-num EQ INT(to_est) THEN ce-ctrl.e-num = ce-ctrl.e-num - 1.
                    ASSIGN
                        to_est              = ""
                        to_est:SCREEN-VALUE = to_est.
                    FIND CURRENT ce-ctrl NO-LOCK.
                    LEAVE.
                END.
            END.
        END.

        RELEASE ce-ctrl.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cust C-Win
ON LEAVE OF fi_cust IN FRAME FRAME-A /* New Customer# */
DO:
        {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).

        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-cust NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            ASSIGN {&self-name}.
            FIND FIRST bv-cust
                WHERE bv-cust.company EQ to_company:SCREEN-VALUE
                AND bv-cust.cust-no EQ fi_cust:SCREEN-VALUE NO-LOCK NO-ERROR.
            IF AVAILABLE bv-cust THEN 
            DO:
                fi_rep:SCREEN-VALUE = bv-cust.sman .
            END.
            FIND FIRST bv-shipto
                WHERE bv-shipto.company EQ to_company:SCREEN-VALUE
                AND bv-shipto.cust-no EQ fi_cust:SCREEN-VALUE
                AND bv-shipto.ship-id EQ fi_cust:SCREEN-VALUE NO-LOCK NO-ERROR .
            IF NOT AVAILABLE bv-shipto THEN 
            DO:
                FIND FIRST bv-shipto
                    WHERE bv-shipto.company EQ to_company:SCREEN-VALUE
                    AND bv-shipto.cust-no EQ fi_cust:SCREEN-VALUE NO-LOCK NO-ERROR .
            END.
            IF AVAILABLE bv-shipto THEN 
            DO:
                fi_shipto:SCREEN-VALUE = bv-shipto.ship-id .
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_part
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_part C-Win
ON LEAVE OF fi_part IN FRAME FRAME-A /* New Customer Part# */
DO:
        {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).

        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-part NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            ASSIGN {&self-name}.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_rep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_rep C-Win
ON LEAVE OF fi_rep IN FRAME FRAME-A /* New Sales Rep# */
DO:
        {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).

        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-rep NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            ASSIGN {&self-name}.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_shipto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_shipto C-Win
ON HELP OF fi_shipto IN FRAME FRAME-A /* New Ship To# */
DO:
        DEFINE VARIABLE char-val AS cha NO-UNDO.

        RUN windows/l-shipto.w (gcompany,"",fi_cust:SCREEN-VALUE,"", OUTPUT char-val).
        IF char-val NE "" THEN 
        DO:
            {&self-name}:SCREEN-VALUE = ENTRY(1,char-val).
            APPLY "value-changed" TO {&self-name}.
        END.

        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_shipto C-Win
ON LEAVE OF fi_shipto IN FRAME FRAME-A /* New Ship To# */
DO:
        {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).

        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-shipto NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            ASSIGN {&self-name}.
            FIND FIRST bv-shipto
                WHERE bv-shipto.company EQ to_company:SCREEN-VALUE
                AND bv-shipto.cust-no EQ fi_cust:SCREEN-VALUE
                AND bv-shipto.ship-id EQ fi_shipto:SCREEN-VALUE NO-LOCK NO-ERROR .
            IF AVAILABLE bv-shipto THEN 
            DO:
                fi_rep:SCREEN-VALUE = bv-shipto.spare-char-1 .
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME from_company
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL from_company C-Win
ON HELP OF from_company IN FRAME FRAME-A /* From Company */
DO:
        RUN lookups/company.p.

        IF g_lookup-var NE ""                        AND 
            g_lookup-var NE {&self-name}:SCREEN-VALUE THEN 
        DO:
            {&self-name}:SCREEN-VALUE = g_lookup-var.
            APPLY "value-changed" TO {&self-name}.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL from_company C-Win
ON LEAVE OF from_company IN FRAME FRAME-A /* From Company */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-company NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            ASSIGN {&self-name}.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL from_company C-Win
ON VALUE-CHANGED OF from_company IN FRAME FRAME-A /* From Company */
DO:
        RUN new-company.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME from_est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL from_est C-Win
ON HELP OF from_est IN FRAME FRAME-A /* From Estimate# */
DO:
        DEFINE VARIABLE char-val AS cha NO-UNDO.


        RUN windows/l-est.w (from_company:SCREEN-VALUE, locode, {&self-name}:SCREEN-VALUE, OUTPUT char-val).
        FIND eb WHERE RECID(eb) EQ INT(char-val) NO-LOCK NO-ERROR.
        IF AVAILABLE eb AND TRIM(eb.est-no) NE TRIM({&self-name}:SCREEN-VALUE) THEN 
        DO:
            {&self-name}:SCREEN-VALUE = eb.est-no.
            APPLY "value-changed" TO {&self-name}.
        END.

        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL from_est C-Win
ON LEAVE OF from_est IN FRAME FRAME-A /* From Estimate# */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-est NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            ASSIGN {&self-name}.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL from_est C-Win
ON VALUE-CHANGED OF from_est IN FRAME FRAME-A /* From Estimate# */
DO:
        FOR EACH eb
            WHERE eb.company EQ from_company:SCREEN-VALUE
            AND eb.est-no  EQ FILL(" ",8 - LENGTH(TRIM(from_est:SCREEN-VALUE))) +
            TRIM(from_est:SCREEN-VALUE)
            NO-LOCK
            BREAK BY eb.form-no  DESCENDING
            BY eb.blank-no DESCENDING
            WITH FRAME {&FRAME-NAME}:

            IF eb.form-no GT 0 THEN
                ASSIGN
                    from_ship:SCREEN-VALUE = eb.ship-name
                    from_part:SCREEN-VALUE = eb.part-dscr1
                    lv-cust-no             = eb.cust-no
                    fi_cust:SCREEN-VALUE   = lv-cust-no 
                    fi_shipto:SCREEN-VALUE = eb.ship-id  
                    fi_rep:SCREEN-VALUE    = eb.sman .

            IF LAST(eb.blank-no) THEN 
            DO:
                fi_part:SCREEN-VALUE = IF eb.form-no EQ 0    OR
                    FIRST(eb.blank-no) THEN eb.part-no
                    ELSE "".

                IF (eb.est-type GE 3 AND eb.est-type LE 4) OR eb.est-type EQ 8 THEN
                    fi_part:HIDDEN = YES.
                ELSE
                    fi_part:HIDDEN = NO.
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbCopyCosts
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbCopyCosts C-Win
ON VALUE-CHANGED OF tbCopyCosts IN FRAME FRAME-A /* Copy Additional Costs */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_clip-att
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_clip-att C-Win
ON VALUE-CHANGED OF tb_clip-att IN FRAME FRAME-A /* Copy Paper Clip Attachments? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_copy C-Win
ON VALUE-CHANGED OF tb_copy IN FRAME FRAME-A /* Copy Routing? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_copy-dscr-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_copy-dscr-1 C-Win
ON VALUE-CHANGED OF tb_copy-dscr-1 IN FRAME FRAME-A /* Copy Item Description 1? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_copy-dscr-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_copy-dscr-2 C-Win
ON VALUE-CHANGED OF tb_copy-dscr-2 IN FRAME FRAME-A /* Copy Item Description 2? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_copy-i-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_copy-i-name C-Win
ON VALUE-CHANGED OF tb_copy-i-name IN FRAME FRAME-A /* Copy Item Name? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_copy-notes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_copy-notes C-Win
ON VALUE-CHANGED OF tb_copy-notes IN FRAME FRAME-A /* Copy Book Icon Spec Notes? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_copy-prep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_copy-prep C-Win
ON VALUE-CHANGED OF tb_copy-prep IN FRAME FRAME-A /* Copy Prep? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_dept-notes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_dept-notes C-Win
ON VALUE-CHANGED OF tb_dept-notes IN FRAME FRAME-A /*  Copy Fountain Pen Icon Dept Notes? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_die
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_die C-Win
ON VALUE-CHANGED OF tb_die IN FRAME FRAME-A /* Copy Die#? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_farm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_farm C-Win
ON VALUE-CHANGED OF tb_farm IN FRAME FRAME-A /* Copy Farm Out Costs? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_i-no C-Win
ON VALUE-CHANGED OF tb_i-no IN FRAME FRAME-A /* Copy FG Item#? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_plate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_plate C-Win
ON VALUE-CHANGED OF tb_plate IN FRAME FRAME-A /* Copy Plate#? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME to_company
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL to_company C-Win
ON HELP OF to_company IN FRAME FRAME-A /* To Company */
DO:
        RUN lookups/company.p.

        IF g_lookup-var NE ""                        AND 
            g_lookup-var NE {&self-name}:SCREEN-VALUE THEN 
        DO:
            {&self-name}:SCREEN-VALUE = g_lookup-var.
            APPLY "value-changed" TO {&self-name}.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL to_company C-Win
ON LEAVE OF to_company IN FRAME FRAME-A /* To Company */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-company NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            ASSIGN {&self-name}.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL to_company C-Win
ON VALUE-CHANGED OF to_company IN FRAME FRAME-A /* To Company */
DO:
        RUN new-company.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME to_est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL to_est C-Win
ON LEAVE OF to_est IN FRAME FRAME-A /* To Estimate# */
DO:
        ASSIGN {&self-name}.  
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
    IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN.
    END.

    DO TRANSACTION:
        {ce/cecopy.i}
    END.

    ASSIGN
        tb_copy      = sys-ctrl.int-fld EQ 1
        from_company = cocode
        to_company   = cocode.
   
    btn-process:LOAD-IMAGE("Graphics/32x32/startprocess.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.

    {methods/nowait.i}

    DO WITH FRAME {&frame-name}:
        DISPLAY from_company to_company.
        {custom/usrprint.i}
        IF ip-copy-est EQ "copy" THEN
            ASSIGN from_est:SCREEN-VALUE = ip-new-est.
        to_est:SCREEN-VALUE = "".
        APPLY "entry" TO to_company.
        RUN new-company.
        APPLY "entry" TO from_company.
        RUN new-company.
    END.

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
  DISPLAY from_company from_name from_est from_ship from_part tb_copy 
          tb_copy-i-name tb_die tb_copy-dscr-1 tb_plate tb_copy-dscr-2 tb_i-no 
          tb_copy-notes tb_farm tb_dept-notes tb_copy-prep tb_clip-att 
          tbCopyCosts to_company to_name to_est fi_cust fi_part fi_shipto fi_rep 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 RECT-18 RECT-19 RECT-20 from_company from_est tb_copy 
         tb_copy-i-name tb_die tb_copy-dscr-1 tb_plate tb_copy-dscr-2 tb_i-no 
         tb_copy-notes tb_farm tb_dept-notes tb_copy-prep tb_clip-att 
         tbCopyCosts to_company fi_cust fi_part fi_shipto fi_rep btn-process 
         btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-company C-Win 
PROCEDURE new-company :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        FIND company WHERE company.company BEGINS FOCUS:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF AVAILABLE company THEN 
        DO:
            FOCUS:SCREEN-VALUE = company.company.

            IF FOCUS:NAME EQ "from_company" THEN from_name:SCREEN-VALUE = company.NAME.
            ELSE to_name:SCREEN-VALUE   = company.NAME. 

            APPLY "value-changed" TO from_est.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
/* ---------------------------------------------------- ce/cp-est.p 10/94 gb */
    /* copy estimate & standards files                                           */
    /* -------------------------------------------------------------------------- */
    DEFINE BUFFER kest          FOR est.
    DEFINE BUFFER kqty          FOR est-qty.
    DEFINE BUFFER kef           FOR ef.
    DEFINE BUFFER keb           FOR eb.
    DEFINE BUFFER kprep         FOR est-prep.
    DEFINE BUFFER kop           FOR est-op.
    DEFINE BUFFER kinst         FOR est-inst.
    DEFINE BUFFER kflm          FOR est-flm.
    DEFINE BUFFER kref          FOR reftable.
    DEFINE BUFFER kbdh          FOR box-design-hdr.
    DEFINE BUFFER kbdl          FOR box-design-line.
    DEFINE BUFFER knsh          FOR ef-nsh.
    DEFINE BUFFER kei           FOR e-itemfg.
    DEFINE BUFFER keiv          FOR e-itemfg-vend.
    DEFINE BUFFER knot          FOR notes.
    DEFINE BUFFER b-itemfg      FOR itemfg.
    DEFINE BUFFER b-attach      FOR ATTACH .
    DEFINE BUFFER bf-estPacking FOR estPacking.
    DEFINE BUFFER bf-estMisc    FOR estMisc.

    DEFINE VARIABLE txno       AS INTEGER.

    DEFINE VARIABLE fcom       LIKE company.company.
    DEFINE VARIABLE fest       LIKE est.est-no.
    DEFINE VARIABLE fest-mr    AS LOG     INIT NO.
    DEFINE VARIABLE tcom       LIKE company.company.
    DEFINE VARIABLE test       LIKE est.est-no.
    DEFINE VARIABLE ls-key     AS cha     FORMAT "x(20)" NO-UNDO.
    DEFINE VARIABLE li         AS INTEGER NO-UNDO.
    DEFINE VARIABLE lj         AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-part-no LIKE eb.part-no NO-UNDO.
    DEFINE VARIABLE lCopyPrep  AS LOGICAL NO-UNDO .


    SESSION:SET-WAIT-STATE("general").

    ASSIGN
        fcom      = from_company
        fest      = from_est
        fest-mr   = tb_copy
        tcom      = to_company
        test      = to_est
        lCopyPrep = tb_copy-prep .

    FIND FIRST est
        WHERE est.company EQ fcom
        AND est.est-no  EQ FILL(" ",8 - LENGTH(TRIM(fest))) + TRIM(fest)
        NO-LOCK NO-ERROR.

    ls-key = DYNAMIC-FUNCTION("sfGetNextRecKey").
    /*  create rec_key.                   */
    /*  assign rec_key.rec_key = ls-key   */
    /*         rec_key.table_name = "EST".*/

    CREATE kest.
    BUFFER-COPY est TO kest
        ASSIGN
        kest.company  = tcom
        kest.est-no   = FILL(" ",8 - LENGTH(TRIM(test))) + TRIM(test)
        kest.e-num    = INT(est.est-no)
        kest.ord-no   = 0
        kest.ord-date = ?
        kest.est-date = TODAY
        kest.rec_key = ls-key  
        kest.entered-id = USERID("nosweat")     .

    DISABLE TRIGGERS FOR LOAD OF keb.

    FOR EACH eb
        WHERE eb.company EQ est.company
        AND eb.est-no  EQ est.est-no
        NO-LOCK
        BY eb.form-no
        BY eb.blank-no:

        IF eb.form-no EQ 0 THEN lv-part-no = eb.part-no.
        ELSE li = li + 1.

        CREATE keb.
        BUFFER-COPY eb EXCEPT rec_key die-no plate-no stock-no ord-no TO keb
            ASSIGN
            keb.company  = kest.company
            keb.est-no   = kest.est-no
            keb.est-int  = int(kest.est-no)
            keb.cust-no  = IF fi_cust NE lv-cust-no THEN fi_cust ELSE eb.cust-no
            keb.part-no  = eb.part-no
            keb.stock-no = IF tb_i-no THEN eb.stock-no ELSE keb.stock-no
            keb.part-dscr1 = IF tb_copy-i-name THEN eb.part-dscr1 ELSE keb.part-dscr1
            keb.part-dscr2 = IF tb_copy-dscr-1 THEN eb.part-dscr2 ELSE keb.part-dscr2
            keb.master-est-no = est.est-no  .


        IF tb_copy-notes AND tb_i-no AND tcom NE fcom THEN 
        DO: /* task 05291502 */
            FIND FIRST b-itemfg WHERE b-itemfg.company = tcom
                AND b-itemfg.i-no = keb.stock-no NO-LOCK NO-ERROR.
            IF AVAILABLE b-itemfg THEN 
            DO:
                FIND FIRST itemfg WHERE itemfg.company = est.company
                    AND itemfg.i-no = eb.stock-no NO-LOCK NO-ERROR.
                IF AVAILABLE itemfg THEN 
                DO:
                    FOR EACH notes WHERE  notes.rec_key = itemfg.rec_key  
                        AND notes.note_type = "S" NO-LOCK :
                        CREATE knot.
                        BUFFER-COPY notes TO knot
                            ASSIGN 
                            knot.rec_key = b-itemfg.rec_key.
                    END.
                END.
            END.
            IF NOT AVAILABLE b-itemfg THEN 
            DO:
                FIND FIRST itemfg WHERE itemfg.company = est.company
                    AND itemfg.i-no = eb.stock-no NO-LOCK NO-ERROR.
                IF AVAILABLE itemfg THEN 
                DO:
                    CREATE b-itemfg.
                    BUFFER-COPY itemfg EXCEPT company rec_key TO b-itemfg.
                    ASSIGN 
                        b-itemfg.company = tcom .
                    FOR EACH notes WHERE  notes.rec_key = itemfg.rec_key  
                        AND notes.note_type = "S" NO-LOCK :
                        CREATE knot.
                        BUFFER-COPY notes TO knot
                            ASSIGN 
                            knot.rec_key = b-itemfg.rec_key.
                    END.
                END.
            END.
        END.

        IF fi_part NE lv-part-no                  AND
            (eb.est-type LE 2 OR eb.est-type GE 5) AND
            eb.est-type NE 8                       THEN 
        DO:
            keb.part-no = fi_part.

            IF (eb.est-type EQ 2 OR eb.est-type EQ 6) AND
                eb.form-no NE 0 AND est.form-qty GE 2 THEN
                keb.part-no = TRIM(SUBSTR(keb.part-no,1,12)) + "-" + TRIM(STRING(li,">9")).
        END.
        /*IF fi_cust NE lv-cust-no THEN DO:*/
        FIND cust WHERE cust.company = tcom AND
            cust.cust-no = keb.cust-no
            NO-LOCK NO-ERROR.
        keb.sman = IF AVAILABLE cust THEN cust.sman ELSE "".
        FIND sman WHERE sman.company = gcompany
            AND sman.sman = keb.sman
            NO-LOCK NO-ERROR.
        ASSIGN 
            keb.comm = IF AVAILABLE sman THEN sman.scomm ELSE 0. 
        /*END.*/
        IF fi_shipto NE "" THEN 
        DO:
            ASSIGN
                keb.ship-id = fi_shipto .
            FIND FIRST shipto WHERE shipto.company = tcom
                AND shipto.cust-no = keb.cust-no NO-LOCK NO-ERROR.
            IF AVAILABLE shipto AND shipto.spare-char-1 NE "" THEN 
            DO:
                ASSIGN 
                    keb.sman = shipto.spare-char-1 .
                FIND sman WHERE sman.company = gcompany
                    AND sman.sman = keb.sman
                    NO-LOCK NO-ERROR.
                ASSIGN 
                    keb.comm = IF AVAILABLE sman THEN sman.scomm ELSE 0.
            END.
        END.
        IF fi_rep NE "" THEN 
        DO:
            ASSIGN 
                keb.sman = fi_rep .
            FIND sman WHERE sman.company = gcompany
                AND sman.sman = keb.sman
                NO-LOCK NO-ERROR.
            ASSIGN 
                keb.comm = IF AVAILABLE sman THEN sman.scomm ELSE 0. 
        END.

        IF fi_rep:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE eb.sman THEN 
        DO:         /* Task#  03191501*/
            FIND sman WHERE sman.company = gcompany
                AND sman.sman = keb.sman
                NO-LOCK NO-ERROR.
            ASSIGN 
                keb.comm = IF AVAILABLE sman THEN sman.scomm ELSE 0.
        END.
        ELSE 
        DO:
            ASSIGN 
                keb.comm = eb.comm .
        END.


        IF tb_farm THEN 
        DO:
            FOR EACH e-itemfg-vend NO-LOCK
                WHERE e-itemfg-vend.company  EQ eb.company
                AND e-itemfg-vend.est-no   EQ eb.est-no
                AND e-itemfg-vend.form-no  EQ eb.form-no
                AND e-itemfg-vend.blank-no EQ eb.blank-no
                BREAK BY e-itemfg-vend.vend-no:
  
                CREATE keiv.
                BUFFER-COPY e-itemfg-vend EXCEPT rec_key TO keiv
                    ASSIGN
                    keiv.company = keb.company
                    keiv.est-no  = keb.est-no
                    keiv.i-no    = keb.stock-no.
            END.
            RUN CopyVendItemCost(
                INPUT eb.company,
                INPUT eb.est-no,  /* Old Estimate Number */
                INPUT keb.stock-no,
                INPUT eb.form-no,
                INPUT eb.blank-no,
                INPUT keb.est-no  /* New Estimate Number */
                ).
        END.    

        IF fi_cust NE eb.cust-no THEN
            FOR EACH shipto
                WHERE shipto.company EQ keb.company
                AND shipto.cust-no EQ keb.cust-no
                AND (shipto.ship-id EQ keb.ship-id OR fi_shipto = "")
                NO-LOCK
                BREAK BY shipto.ship-id:
                IF fi_shipto = "" THEN 
                DO:
                    IF shipto.ship-id EQ shipto.cust-no OR LAST(shipto.ship-id) THEN 
                    DO:
                        ASSIGN
                            keb.ship-id      = shipto.ship-id
                            keb.carrier      = shipto.carrier
                            keb.ship-name    = shipto.ship-name
                            keb.ship-addr[1] = shipto.ship-addr[1]
                            keb.ship-addr[2] = shipto.ship-addr[2]
                            keb.ship-city    = shipto.ship-city
                            keb.ship-state   = shipto.ship-state
                            keb.ship-zip     = shipto.ship-zip.
                        LEAVE.
                    END.
                END.
                ELSE 
                DO:
                    ASSIGN
                        keb.ship-id      = shipto.ship-id
                        keb.carrier      = shipto.carrier
                        keb.ship-name    = shipto.ship-name
                        keb.ship-addr[1] = shipto.ship-addr[1]
                        keb.ship-addr[2] = shipto.ship-addr[2]
                        keb.ship-city    = shipto.ship-city
                        keb.ship-state   = shipto.ship-state
                        keb.ship-zip     = shipto.ship-zip.
                    LEAVE.
                END.
            END.

        IF tb_die   THEN keb.die-no   = eb.die-no.
        IF tb_plate THEN keb.plate-no = eb.plate-no.

        {sys/inc/box-del.i keb}

        IF est.est-type LT 5 THEN
        DO:

            FIND FIRST reftable
                WHERE reftable.reftable EQ "cedepth"
                AND reftable.company  EQ eb.company
                AND reftable.loc      EQ eb.est-no
                AND reftable.code     EQ STRING(eb.form-no,"9999999999")
                AND reftable.code2    EQ STRING(eb.blank-no,"9999999999")
                NO-LOCK NO-ERROR.

            IF AVAILABLE reftable THEN
            DO:
                FIND FIRST kref
                    WHERE kref.reftable EQ reftable.reftable
                    AND kref.company  EQ keb.company
                    AND kref.loc      EQ keb.est-no
                    AND kref.code     EQ STRING(keb.form-no,"9999999999")
                    AND kref.code2    EQ STRING(keb.blank-no,"9999999999")
                    NO-ERROR.

                IF NOT AVAILABLE kref THEN
                DO:
                    CREATE kref.
                    ASSIGN
                        kref.reftable = reftable.reftable
                        kref.company  = keb.company
                        kref.loc      = keb.est-no
                        kref.code     = STRING(keb.form-no,"9999999999")
                        kref.code2    = STRING(keb.blank-no,"9999999999").
                END.

                ASSIGN
                    kref.val[1] = reftable.val[1]
                    kref.val[2] = reftable.val[2].

                RELEASE reftable.
            END.
        END.
    END.

    FOR EACH est-qty
        WHERE est-qty.company EQ est.company
        AND est-qty.est-no  EQ est.est-no
        NO-LOCK:

        CREATE kqty.
        BUFFER-COPY est-qty EXCEPT rec_key TO kqty
            ASSIGN
            kqty.company = kest.company
            kqty.est-no  = kest.est-no.
    END.

    FOR EACH ef-nsh
        WHERE ef-nsh.company EQ est.company
        AND ef-nsh.est-no  EQ est.est-no
        NO-LOCK:
        CREATE knsh.
        BUFFER-COPY ef-nsh EXCEPT rec_key TO knsh
            ASSIGN
            knsh.company = kest.company
            knsh.est-no  = kest.est-no.
    END.

    FOR EACH ef
        WHERE ef.company EQ est.company
        AND ef.est-no  EQ est.est-no
        NO-LOCK:
        CREATE kef.
        BUFFER-COPY ef EXCEPT rec_key TO kef
            ASSIGN
            kef.company = kest.company
            kef.est-no  = kest.est-no.

        IF NOT fest-mr THEN kef.op-lock = NO.

        IF (est.est-type EQ 3 OR
            est.est-type EQ 4 OR
            est.est-type EQ 7 OR
            est.est-type EQ 8)                           AND
            CAN-FIND(FIRST est-flm
            WHERE est-flm.company EQ ef.company
            AND est-flm.est-no  EQ ef.est-no
            AND est-flm.snum    EQ ef.form-no) THEN
            ASSIGN
                kef.leaf      = ""
                kef.leaf-dscr = ""
                kef.leaf-bnum = 0
                kef.leaf-w    = 0
                kef.leaf-l    = 0.

        FOR EACH reftable {ce/est-mrpl.i ef} NO-LOCK:
            CREATE kref.
            BUFFER-COPY reftable EXCEPT rec_key TO kref
                ASSIGN
                kref.company = kef.company
                kref.code    = TRIM(kef.est-no) + string(kef.form-no,"/99").
        END.

        FOR EACH reftable
            WHERE reftable.reftable EQ "EST-MISC"
            AND reftable.company  EQ ef.company
            AND reftable.loc      EQ ef.loc
            AND reftable.code     EQ trim(ef.est-no) + string(ef.form-no,"/99")
            NO-LOCK:
            CREATE kref.
            BUFFER-COPY reftable EXCEPT rec_key TO kref
                ASSIGN
                kref.company = kef.company
                kref.code    = TRIM(kef.est-no) + string(kef.form-no,"/99").  
        END.
    END.
  
    FOR EACH estPacking NO-LOCK
        WHERE estPacking.company = est.company 
        AND estPacking.estimateNo = est.est-no :
                 
        CREATE bf-estPacking .
        BUFFER-COPY estPacking EXCEPT rec_key company estimateNo TO bf-estPacking .
        ASSIGN 
            bf-estPacking.company    = kest.company
            bf-estPacking.estimateNo = kest.est-no . 
       
    END.  /* FOR EACH estPacking*/

    IF lCopyPrep  THEN
        FOR EACH est-prep
            WHERE est-prep.company EQ est.company
            AND est-prep.est-no  EQ est.est-no
            NO-LOCK:
            CREATE kprep.
            BUFFER-COPY est-prep EXCEPT rec_key TO kprep
                ASSIGN
                kprep.company = kest.company
                kprep.est-no  = kest.est-no
                kprep.orderID = ""   .
        END.

    IF fest-mr THEN
        FOR EACH est-op
            WHERE est-op.company EQ est.company
            AND est-op.est-no  EQ est.est-no
            AND est-op.line    LT 500
            NO-LOCK:
            CREATE kop.
            BUFFER-COPY est-op EXCEPT rec_key TO kop
                ASSIGN
                kop.company = kest.company
                kop.est-no  = kest.est-no.
        END.

    FOR EACH est-inst
        WHERE est-inst.company EQ est.company
        AND est-inst.est-no  EQ est.est-no
        NO-LOCK:
        CREATE kinst.
        BUFFER-COPY est-inst EXCEPT rec_key TO kinst
            ASSIGN
            kinst.company = kest.company
            kinst.est-no  = kest.est-no.
    END.

    IF est.est-type EQ 3 OR
        est.est-type EQ 4 OR
        est.est-type EQ 7 OR
        est.est-type EQ 8 THEN
        FOR EACH est-flm
            WHERE est-flm.company EQ est.company
            AND est-flm.est-no  EQ est.est-no
            AND NOT CAN-FIND(FIRST kflm
            WHERE kflm.company EQ kest.company
            AND kflm.est-no  EQ kest.est-no
            AND kflm.eqty    EQ est-flm.eqty
            AND kflm.line    EQ est-flm.line)
            NO-LOCK
            BREAK BY est-flm.line:

            CREATE kflm.
            BUFFER-COPY est-flm EXCEPT rec_key TO kflm
                ASSIGN
                kflm.company = kest.company
                kflm.est-no  = kest.est-no.
        END.

    FOR EACH box-design-hdr
        WHERE box-design-hdr.design-no EQ 0
        AND box-design-hdr.company   EQ est.company
        AND box-design-hdr.est-no    EQ est.est-no
        NO-LOCK:

        IF NOT CAN-FIND(FIRST kbdh WHERE
            kbdh.design-no = 0 AND
            kbdh.company EQ kest.company AND
            kbdh.est-no EQ kest.est-no AND
            kbdh.eqty EQ box-design-hdr.eqty AND
            kbdh.form-no EQ box-design-hdr.form-no AND
            kbdh.blank-no EQ box-design-hdr.blank-no) THEN
        DO:
            CREATE kbdh.
            BUFFER-COPY box-design-hdr EXCEPT rec_key TO kbdh
                ASSIGN
                kbdh.design-no = 0
                kbdh.company   = kest.company
                kbdh.est-no    = kest.est-no.
        END.

        FOR EACH box-design-line OF box-design-hdr NO-LOCK:

            IF NOT CAN-FIND(FIRST kbdl WHERE
                kbdl.design-no EQ 0 AND
                kbdl.company EQ kest.company AND
                kbdl.est-no EQ kest.est-no AND
                kbdl.eqty EQ box-design-line.eqty AND
                kbdl.form-no EQ box-design-line.form-no AND
                kbdl.blank-no EQ box-design-line.blank-no AND
                kbdl.line-no  EQ box-design-line.line-no) THEN
            DO:
                CREATE kbdl.
                BUFFER-COPY box-design-line EXCEPT rec_key TO kbdl
                    ASSIGN
                    kbdl.design-no = 0
                    kbdl.company   = kest.company
                    kbdl.est-no    = kest.est-no.
            END.
        END.
    END.

    IF tb_dept-notes THEN
        FOR EACH notes WHERE notes.rec_key EQ est.rec_key NO-LOCK:
            CREATE knot.
            BUFFER-COPY notes TO knot
                ASSIGN 
                knot.rec_key = ls-key.
        END.

    IF tb_clip-att THEN 
    DO: 
        FOR EACH attach WHERE attach.company = est.company AND 
            attach.rec_key = est.rec_key  AND 
            trim(attach.est-no) = trim(est.est-no) NO-LOCK:
            CREATE b-attach .
            BUFFER-COPY ATTACH TO b-attach.
            ASSIGN 
                b-attach.rec_key = ls-key
                b-attach.company = tcom
                b-attach.est-no  = kest.est-no .
        END.
    END.
    
    IF tbCopyCosts THEN DO:
        FOR EACH estMisc NO-LOCK
            WHERE estMisc.company    EQ est.company
              AND estMisc.estimateNo EQ est.est-no:
            CREATE bf-estMisc.
            BUFFER-COPY estMisc EXCEPT rec_key estimateNo TO bf-estMisc.
            ASSIGN
                bf-estMisc.company    = kest.company
                bf-estMisc.estimateNo = kest.est-no
                .
        END.
    END.
    
    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE("").

    MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.
    
    APPLY 'CLOSE' TO THIS-PROCEDURE.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-company C-Win 
PROCEDURE valid-company :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-msg AS CHARACTER INIT "" NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-FIND(FIRST company WHERE company.company EQ FOCUS:SCREEN-VALUE) THEN
            lv-msg = "Invalid entry, try help".

        IF lv-msg EQ "" AND FOCUS:NAME EQ "from_company" AND
            NOT CAN-FIND(FIRST eb WHERE eb.company EQ FOCUS:SCREEN-VALUE) THEN
            lv-msg = "Sorry, no estimates exist for this company".

        IF lv-msg NE "" THEN 
        DO:
            MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO FOCUS.
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust C-Win 
PROCEDURE valid-cust :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-FIND(FIRST cust
            WHERE cust.company EQ to_company:SCREEN-VALUE
            AND cust.cust-no EQ fi_cust:SCREEN-VALUE)
            THEN 
        DO:
            MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO fi_cust.
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-est C-Win 
PROCEDURE valid-est :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-msg AS CHARACTER NO-UNDO.

    DEFINE BUFFER b-eb FOR eb.


    DO WITH FRAME {&FRAME-NAME}:
        v-msg = "".

        FIND FIRST eb
            WHERE eb.company EQ from_company:SCREEN-VALUE
            AND eb.est-no  EQ FILL(" ", 8 - LENGTH(TRIM(from_est:SCREEN-VALUE))) +
            TRIM(from_est:SCREEN-VALUE)
            AND eb.form-no NE 0
            NO-LOCK NO-ERROR.

        IF v-msg EQ "" THEN
            IF NOT AVAILABLE eb THEN v-msg = "Invalid entry, try help".

        IF v-msg EQ "" THEN
            IF AVAILABLE eb                                                   AND
                (eb.est-type EQ 2 OR eb.est-type EQ 5 OR eb.est-type EQ 6) THEN
                FOR EACH b-eb
                    WHERE b-eb.company EQ eb.company
                    AND b-eb.est-no  EQ eb.est-no
                    AND ROWID(b-eb)  NE ROWID(eb)
                    NO-LOCK BY b-eb.form-no:
                    IF b-eb.form-no NE 0 THEN v-msg = "Set estimate has no header".
                    LEAVE.
                END.

        IF v-msg NE "" THEN 
        DO:
            MESSAGE TRIM(v-msg) + "..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO from_est.
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-part C-Win 
PROCEDURE valid-part :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        IF fi_part:HIDDEN EQ NO AND fi_part:SCREEN-VALUE EQ "" THEN 
        DO:
            MESSAGE TRIM(fi_part:LABEL) + " must not be spaces..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO fi_part.
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-rep C-Win 
PROCEDURE valid-rep :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        IF fi_rep:SCREEN-VALUE <> "" THEN
            IF NOT CAN-FIND(FIRST sman
                WHERE sman.company EQ to_company:SCREEN-VALUE
                AND sman.sman = fi_rep:SCREEN-VALUE )
                THEN 
            DO:
                MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO fi_rep.
                RETURN ERROR.
            END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-shipto C-Win 
PROCEDURE valid-shipto :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        IF fi_shipto:SCREEN-VALUE NE "" THEN 
        DO:
            IF NOT CAN-FIND(FIRST shipto
                WHERE shipto.company EQ to_company:SCREEN-VALUE
                AND shipto.cust-no EQ fi_cust:SCREEN-VALUE
                AND shipto.ship-id EQ fi_shipto:SCREEN-VALUE)
                THEN 
            DO:
                MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO fi_shipto.
                RETURN ERROR.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-terms C-Win 
PROCEDURE valid-terms :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cMissingTerms AS CHARACTER NO-UNDO.
    DEFINE VARIABLE fest          AS CHARACTER NO-UNDO.

    cMissingTerms = "".
    DO WITH FRAME {&FRAME-NAME}:

        fest = TRIM(from_est:SCREEN-VALUE).  
        FOR EACH quotehd WHERE quotehd.company EQ FROM_company:SCREEN-VALUE
            AND quotehd.loc EQ locode
            AND quotehd.est-no EQ FILL(" ",8 - LENGTH(TRIM(fest))) + TRIM(fest) NO-LOCK:
            IF NOT CAN-FIND(FIRST terms WHERE terms.company EQ to_company:SCREEN-VALUE
                AND terms.t-code EQ quotehd.terms)
                THEN 
            DO:
                cMissingTerms = quotehd.terms.    
                LEAVE.
            END.
        END.

        IF cMissingTerms GT "" THEN 
        DO:
            MESSAGE "Terms code " + cMissingTerms + 
                ", used on a related quote but is not defined in company "
                +  to_company:SCREEN-VALUE VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO fi_cust.
            RETURN ERROR.
        END.

    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

