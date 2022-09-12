&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ce-ctrl.w.w

  Description: Cost Estimating Control File

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 01/12/2000

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
/*  Mod: Ticket - 103137 Format Change for Order No. and Job No.       */     

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS cha       NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.

DEFINE VARIABLE v-print-fmt    AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tt-report LIKE report
    FIELD ink-code AS CHARACTER EXTENT 15
    FIELD rm-name  AS CHARACTER EXTENT 15
    FIELD rm-dscr  AS CHARACTER EXTENT 15 
    FIELD qty      AS CHARACTER EXTENT 15 .

DEFINE STREAM excel.


DEFINE VARIABLE ll-secure          AS LOG       NO-UNDO.

DEFINE VARIABLE excel-header-var-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE excel-header-var-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE excel-header-var-3 AS CHARACTER NO-UNDO.


DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS cha       NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS cha       NO-UNDO.
DEFINE VARIABLE cFieldLength       AS cha       NO-UNDO.
DEFINE VARIABLE cFieldType         AS cha       NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE BUFFER b-job-hdr FOR job-hdr .
DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO .
DEFINE VARIABLE hdOutputProcs      AS HANDLE    NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.


ASSIGN 
    cTextListToSelect  = "JOB#,FORM,CUSTOMER,NAME,FG ITEM #,BOARD,ADDER 1,ADDER 2,DUE DATE,PCT-COV," +
                           "QTY-SHEETS,INK1,INK2,INK3,INK4,INK5,INK6,RM NAME,RM NAME2,RM NAME3," +
                           "RM NAME4,RM NAME5,RM NAME6,RM DESCRIPTION,RM DESCRIPTION2,RM DESCRIPTION3,RM DESCRIPTION4," +
                           "RM DESCRIPTION5,RM DESCRIPTION6,QTY PER INK,QTY PER INK2,QTY PER INK3,QTY PER INK4,QTY PER INK5,QTY PER INK6"
    cFieldListToSelect = "job,frm,cust,name,ino,board,addr1,addr2,date,cov," +
                                "qty,ink1,ink2,ink3,ink4,ink5,ink6,ink1rmname,ink2rmname,ink3rmname," +
                                "ink4rmname,ink5rmname,ink6rmname,ink1rmdscr,ink2rmdscr,ink3rmdscr,ink4rmdscr," +
                                "ink5rmdscr,ink6rmdscr,qtyperink1,qtyperink2,qtyperink3,qtyperink4,qtyperink5,qtyperink6"
    cFieldLength       = "13,4,8,20,15,10,10,10,8,7," + "10,11,11,11,11,11,11,30,30,30," + "30,30,30,30,30,30,30," +
                          "30,30,12,12,12,12,12,12"
    cFieldType         = "c,i,c,c,c,c,c,c,c,i," + "i,c,c,c,c,c,c,c,c,c," + "c,c,c,c,c,c,c," + "c,c,c,c,c,c,c,c"
    .

{sys/inc/ttRptSel.i}

{sys/inc/oereordr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-8 tb_fold tb_corr begin_mach-no ~
end_mach-no begin_job-no begin_job-no2 end_job-no end_job-no2 begin_date ~
end_date sl_avail sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest ~
fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_fold lbl_industry tb_corr begin_mach-no ~
end_mach-no begin_job-no begin_job-no2 end_job-no end_job-no2 begin_date ~
end_date sl_avail sl_selected rd-dest fi_file tb_OpenCSV tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GEtFieldValue C-Win 
FUNCTION GEtFieldValue RETURNS CHARACTER
    ( hipField AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.29.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.29.

DEFINE BUTTON Btn_Add 
     LABEL "&Add >>" 
     SIZE 16 BY 1.1.

DEFINE BUTTON btn_down 
     LABEL "Move Down" 
     SIZE 16 BY 1.1.

DEFINE BUTTON Btn_Remove 
     LABEL "<< &Remove" 
     SIZE 16 BY 1.1.

DEFINE BUTTON btn_Up 
     LABEL "Move Up" 
     SIZE 16 BY 1.1.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 15.8 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(9)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 15.8 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-999":U INITIAL "000" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5.4 BY 1 NO-UNDO.

DEFINE VARIABLE begin_mach-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Machine#" 
     VIEW-AS FILL-IN 
     SIZE 15.8 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 15.8 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(9)":U INITIAL "zzzzzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 15.8 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-999":U INITIAL "999" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5.6 BY 1 NO-UNDO.

DEFINE VARIABLE end_mach-no AS CHARACTER FORMAT "X(256)":U INITIAL "zzzzzzz" 
     LABEL "Ending Machine#" 
     VIEW-AS FILL-IN 
     SIZE 15.8 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\r-cstshp.csv" 
     LABEL "Name" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 45 BY 1.

DEFINE VARIABLE lbl_industry AS CHARACTER FORMAT "X(256)":U INITIAL "Industry?" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .95 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Portrait", "P",
"Landscape", "L"
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To Email", 5,
"To CSV", 3
     SIZE 16 BY 4.76 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 5.48.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 6.67.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 5.71 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 5.71 NO-UNDO.

DEFINE VARIABLE tbAutoClose AS LOGICAL INITIAL no 
     LABEL "Auto Close" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_corr AS LOGICAL INITIAL no 
     LABEL "Corrugated" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .71 NO-UNDO.

DEFINE VARIABLE tb_fold AS LOGICAL INITIAL no 
     LABEL "Folding" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .71 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV AS LOGICAL INITIAL no 
     LABEL "Open CSV?" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.2 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tb_fold AT ROW 2.14 COL 48
     lbl_industry AT ROW 2.38 COL 33 COLON-ALIGNED NO-LABEL
     tb_corr AT ROW 2.86 COL 48
     begin_mach-no AT ROW 4.05 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Machine Number"
     end_mach-no AT ROW 4.05 COL 66 COLON-ALIGNED HELP
          "Enter Ending Machine Number"
     begin_job-no AT ROW 5.24 COL 25 COLON-ALIGNED HELP
          "Enter the Beginning Job Number"
     begin_job-no2 AT ROW 5.24 COL 41.6 COLON-ALIGNED HELP
          "Enter Beginning Job number"
     end_job-no AT ROW 5.24 COL 66 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 5.24 COL 82.6 COLON-ALIGNED HELP
          "Enter Ending Job number"
     begin_date AT ROW 6.43 COL 25 COLON-ALIGNED
     end_date AT ROW 6.43 COL 66 COLON-ALIGNED
     sl_avail AT ROW 8.67 COL 4.4 NO-LABEL WIDGET-ID 26
     sl_selected AT ROW 8.67 COL 62.6 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 9.29 COL 41.4 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 10.86 COL 41.4 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 12.05 COL 41.4 WIDGET-ID 40
     btn_down AT ROW 13.33 COL 41.6 WIDGET-ID 42
     lv-font-no AT ROW 15.52 COL 77 COLON-ALIGNED
     rd-dest AT ROW 15.62 COL 6 NO-LABEL
     lv-ornt AT ROW 15.76 COL 41 NO-LABEL
     lines-per-page AT ROW 15.95 COL 85 COLON-ALIGNED
     lv-font-name AT ROW 16.48 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 17.91 COL 72
     fi_file AT ROW 19.19 COL 26.8 COLON-ALIGNED HELP
          "Enter File Name"
     tb_OpenCSV AT ROW 19.24 COL 91 RIGHT-ALIGNED
     tbAutoClose AT ROW 20.71 COL 29 WIDGET-ID 16
     btn-ok AT ROW 21.62 COL 28.8
     btn-cancel AT ROW 21.62 COL 53.8
     " Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.14 COL 5.2
          BGCOLOR 15 
     " Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 14.81 COL 5.6
     RECT-6 AT ROW 15.14 COL 4
     RECT-8 AT ROW 1.48 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SIZE 95.8 BY 24.52
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
         TITLE              = "Ink By Machine"
         HEIGHT             = 22.24
         WIDTH              = 96
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
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
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_mach-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_mach-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_industry IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lines-per-page IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lines-per-page:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-font-name:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN lv-font-no IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-font-no:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR RADIO-SET lv-ornt IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-ornt:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       tb_corr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_fold:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       td-show-parm:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Ink By Machine */
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
ON WINDOW-CLOSE OF C-Win /* Ink By Machine */
DO:
        /* This event will close the window and terminate the procedure.  */
        DELETE PROCEDURE hdOutputProcs.
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Date */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no C-Win
ON LEAVE OF begin_job-no IN FRAME FRAME-A /* Beginning Job# */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no2 C-Win
ON LEAVE OF begin_job-no2 IN FRAME FRAME-A
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_mach-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mach-no C-Win
ON LEAVE OF begin_mach-no IN FRAME FRAME-A /* Beginning Machine# */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
        DELETE PROCEDURE hdOutputProcs.
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.
  
        IF rd-dest EQ 3 THEN
        DO:
            ASSIGN 
                fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
            RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
            fi_file:SCREEN-VALUE =  cFileName.
        END.
  
        SESSION:SET-WAIT-STATE("general").
        RUN GetSelectionList.
        RUN run-report. 

        STATUS DEFAULT "Processing Complete". 
        SESSION:SET-WAIT-STATE("").

        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN 
                DO:
                    IF NOT tb_OpenCSV THEN 
                    DO:        
                        MESSAGE  "CSV file have been created." SKIP(1)
                            "~"OK~" to open CSV file?"
                            VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL
                            TITLE "" UPDATE lChoice AS LOGICAL.
               
                        IF lChoice THEN
                        DO:
                            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)). 
                        END.
                    END.
                END. /* WHEN 3 THEN DO: */
            WHEN 4 THEN 
                DO:
                    /*run output-to-fax.*/
                    {custom/asifax.i &type= ''
                            &begin_cust= "begin_mach-no"
                            &END_cust= "begin_mach-no" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                END. 
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        {custom/asimail.i &TYPE = ''
                             &begin_cust= "begin_mach-no"
                             &END_cust= "begin_mach-no"
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = ''
                                  &begin_cust="begin_mach-no"
                                  &END_cust="begin_mach-no"
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }
                    END.
                END.
            WHEN 6 THEN RUN OUTPUT-to-port.
        END CASE.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add C-Win
ON CHOOSE OF Btn_Add IN FRAME FRAME-A /* Add >> */
DO:
        DEFINE VARIABLE cSelectedList AS cha NO-UNDO.

        APPLY "DEFAULT-ACTION" TO sl_avail.

    /*
    DO i = 1 TO sl_avail:NUM-ITEMS WITH FRAME {&FRAME-NAME}:
      IF sl_avail:IS-SELECTED(i) AND
        (NOT CAN-DO(sl_selected:LIST-ITEM-PAIRS,sl_avail:ENTRY(i)) OR sl_selected:NUM-ITEMS = 0) THEN
      /*ldummy = sl_selected:ADD-LAST(sl_avail:ENTRY(i)).*/
          cSelectedList = cSelectedList +
                          entry(i,cTextListToSelect) + "," + entry(i,cFieldListToSelect) + ",".
    END.
    cSelectedList = SUBSTRING(cSelectedList,1,LENGTH(cSelectedList) - 1).
    sl_selected:LIST-ITEM-PAIRS = cSelectedList.
    sl_avail:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
    */
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down C-Win
ON CHOOSE OF btn_down IN FRAME FRAME-A /* Move Down */
DO:
        RUN Move-Field ("Down").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove C-Win
ON CHOOSE OF Btn_Remove IN FRAME FRAME-A /* << Remove */
DO:
        /* DO i = sl_selected:NUM-ITEMS TO 1 BY -1 WITH FRAME {&FRAME-NAME}:
           IF sl_selected:IS-SELECTED(i) THEN
           ldummy = sl_selected:DELETE(i).
         END
         */
        APPLY "DEFAULT-ACTION" TO sl_selected  .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up C-Win
ON CHOOSE OF btn_Up IN FRAME FRAME-A /* Move Up */
DO:
        RUN Move-Field ("Up").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no C-Win
ON LEAVE OF end_job-no IN FRAME FRAME-A /* Ending Job# */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no2 C-Win
ON LEAVE OF end_job-no2 IN FRAME FRAME-A
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_mach-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_mach-no C-Win
ON LEAVE OF end_mach-no IN FRAME FRAME-A /* Ending Machine# */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* Name */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-A /* Lines Per Page */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-font-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON HELP OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
        DEFINE VARIABLE char-val AS cha NO-UNDO.

        RUN WINDOWS/l-fonts.w (FOCUS:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE        = ENTRY(1,char-val)
                LV-FONT-NAME:SCREEN-VALUE = ENTRY(2,char-val).

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON LEAVE OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
        ASSIGN lv-font-no.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-ornt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt C-Win
ON LEAVE OF lv-ornt IN FRAME FRAME-A
DO:
        ASSIGN lv-ornt.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt C-Win
ON VALUE-CHANGED OF lv-ornt IN FRAME FRAME-A
DO:
        {custom/chgfont.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
DO:
        ASSIGN {&self-name}.
        RUN pChangeDest.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail C-Win
ON DEFAULT-ACTION OF sl_avail IN FRAME FRAME-A
DO:

        IF (NOT CAN-DO(sl_selected:LIST-ITEMs,{&SELF-NAME}:SCREEN-VALUE) OR
            sl_selected:NUM-ITEMS = 0)
            THEN ASSIGN ldummy = sl_selected:ADD-LAST({&SELF-NAME}:SCREEN-VALUE)
                ldummy = {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE)
                /* sl_selected:SCREEN-VALUE = sl_selected:ENTRY(sl_selected:NUM-ITEMS) */
                .


    /* for pairs
        DEF VAR cSelectedList AS cha NO-UNDO.
        cSelectedList = sl_Selected:LIST-ITEM-PAIRS.
        DO i = 1 TO sl_avail:NUM-ITEMS WITH FRAME {&FRAME-NAME}:
        IF sl_avail:IS-SELECTED(i) AND
          (NOT CAN-DO(sl_selected:LIST-ITEM-PAIRS,sl_avail:ENTRY(i)) OR
             sl_selected:NUM-ITEMS = 0) THEN
        /*ldummy = sl_selected:ADD-LAST(sl_avail:ENTRY(i)).*/
            cSelectedList = cSelectedList +
                            entry(i,cTextListToSelect) + "," + entry(i,cFieldListToSelect) + ",".
        MESSAGE i sl_avail:IS-SELECTED(i) NOT CAN-DO(sl_selected:LIST-ITEM-PAIRS,sl_avail:ENTRY(i))
            sl_selected:NUM-ITEMS
            SKIP cSelectedList
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
      END.
      cSelectedList = SUBSTRING(cSelectedList,1,LENGTH(cSelectedList) - 1).
      sl_selected:LIST-ITEM-PAIRS = cSelectedList.
      sl_avail:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
      */

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_selected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected C-Win
ON DEFAULT-ACTION OF sl_selected IN FRAME FRAME-A
DO:
        DO i = 1 TO {&SELF-NAME}:NUM-ITEMS:
            IF {&SELF-NAME}:IS-SELECTED(i) THEN 
            DO:
                ASSIGN 
                    ldummy = sl_Avail:add-last({&SELF-NAME}:SCREEN-VALUE)
                    ldummy = /*{&SELF-NAME}:DELETE(i)*/
                       {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE)
                    .
            END.           
        END.
        IF {&SELF-NAME}:NUM-ITEMS NE 0 THEN
            ASSIGN
                {&SELF-NAME}:SCREEN-VALUE = {&SELF-NAME}:ENTRY(1)
                .


    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_corr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_corr C-Win
ON VALUE-CHANGED OF tb_corr IN FRAME FRAME-A /* Corrugated */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_fold
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fold C-Win
ON VALUE-CHANGED OF tb_fold IN FRAME FRAME-A /* Folding */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV C-Win
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME FRAME-A /* Open CSV? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-A /* Show Parameters? */
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

    /* security check need {methods/prgsecur.i} in definition section */
    IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "CEMENU"
        NO-LOCK NO-ERROR.
    IF AVAILABLE sys-ctrl THEN
        ASSIGN
            tb_fold = sys-ctrl.char-fld NE "Corrware"
            tb_corr = sys-ctrl.char-fld NE "Foldware".

    RUN DisplaySelectionList.
    btn-ok:load-image("Graphics/32x32/Ok.png").
    btn-cancel:load-image("Graphics/32x32/cancel.png").
    Btn_Add:load-image("Graphics/32x32/additem.png").
    Btn_Remove:load-image("Graphics/32x32/remove.png").
    btn_Up:load-image("Graphics/32x32/moveup.png").
    btn_down:load-image("Graphics/32x32/movedown.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "MR8" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO begin_mach-no.
    END.
    RUN pChangeDest.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList C-Win 
PROCEDURE DisplaySelectionList :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE cListContents AS cha     NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER NO-UNDO.

    IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN 
    DO:

        RETURN.
    END.

    EMPTY TEMP-TABLE ttRptList.

    DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

        cListContents = cListContents +
            /* (IF cListContents = "" THEN ""  ELSE ",") +
             ENTRY(iCount,cTextListToSelect) + "," +
             ENTRY(1,cFieldListToSelect)
             paris */

            (IF cListContents = "" THEN ""  ELSE ",") +
            ENTRY(iCount,cTextListToSelect)   .
        CREATE ttRptList.
        ASSIGN 
            ttRptList.TextList  = ENTRY(iCount,cTextListToSelect)
            ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
            .
    END.

    /* sl_avail:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListContents. */

    sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 C-Win 
PROCEDURE DisplaySelectionList2 :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListContents AS cha     NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER NO-UNDO.
  
    IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN 
    DO:
        RETURN.
    END.

    EMPTY TEMP-TABLE ttRptList.

    DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

        cListContents = cListContents +
            (IF cListContents = "" THEN ""  ELSE ",") +
            ENTRY(iCount,cTextListToSelect)   .
        CREATE ttRptList.
        ASSIGN 
            ttRptList.TextList  = ENTRY(iCount,cTextListToSelect)
            ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect) .
    END.
    /* sl_avail:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListContents. */
    sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

    DO iCount = 1 TO sl_selected:NUM-ITEMS:
        ldummy = sl_avail:DELETE(sl_selected:ENTRY(iCount)).
    END.

    {sys/ref/SelColCorrect.i}

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
  DISPLAY tb_fold lbl_industry tb_corr begin_mach-no end_mach-no begin_job-no 
          begin_job-no2 end_job-no end_job-no2 begin_date end_date sl_avail 
          sl_selected rd-dest fi_file tb_OpenCSV tbAutoClose 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-8 tb_fold tb_corr begin_mach-no end_mach-no begin_job-no 
         begin_job-no2 end_job-no end_job-no2 begin_date end_date sl_avail 
         sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest fi_file 
         tb_OpenCSV tbAutoClose btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList C-Win 
PROCEDURE GetSelectionList :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTmpList AS cha NO-UNDO.

    EMPTY TEMP-TABLE ttRptSelected.
    cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    iColumnLength = 0.

    DO i = 1 TO sl_selected:NUM-ITEMS /* IN FRAME {&FRAME-NAME}*/ :
        FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-LOCK NO-ERROR.     

        CREATE ttRptSelected.
        ASSIGN 
            ttRptSelected.TextList        = ENTRY(i,cTmpList)
            ttRptSelected.FieldList       = ttRptList.FieldList
            ttRptSelected.FieldLength     = int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
            ttRptSelected.DisplayOrder    = i
            ttRptSelected.HeadingFromLeft = IF ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldType) = "C" THEN YES ELSE NO
            iColumnLength                 = iColumnLength + ttRptSelected.FieldLength + 1.
        .        

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field C-Win 
PROCEDURE Move-Field :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER move AS CHARACTER NO-UNDO.

    DO i = 1 TO sl_selected:NUM-ITEMS IN FRAME {&FRAME-NAME}
        WITH FRAME {&FRAME-NAME}:
        IF sl_selected:IS-SELECTED(i) THEN
        DO:
            IF move = "Down" AND i NE sl_selected:NUM-ITEMS THEN
                ASSIGN
                    ldummy                   = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i + 2)
                    ldummy                   = sl_selected:DELETE(i)
                    sl_selected:SCREEN-VALUE = sl_selected:ENTRY(i + 1)
                    .
            ELSE
                IF move = "Up" AND i NE 1 THEN
                    ASSIGN
                        ldummy                   = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i - 1)
                        ldummy                   = sl_selected:DELETE(i + 1)
                        sl_selected:SCREEN-VALUE = sl_selected:ENTRY(i - 1)
                        .
            LEAVE.
        END.

    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-file C-Win 
PROCEDURE output-to-file :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /*     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.
    
         if init-dir = "" then init-dir = "c:\temp" .
         SYSTEM-DIALOG GET-FILE list-name
             TITLE      "Enter Listing Name to SAVE AS ..."
             FILTERS    "Listing Files (*.rpt)" "*.rpt",
                        "All Files (*.*)" "*.*"
             INITIAL-DIR init-dir
             ASK-OVERWRITE
        /*     CREATE-TEST-FILE*/
             SAVE-AS
             USE-FILENAME
    
             UPDATE OKpressed.
    
         IF NOT OKpressed THEN  RETURN NO-APPLY.  */

    {custom/out2file.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-port C-Win 
PROCEDURE output-to-port :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    RUN custom/d-print.w (list-name).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-printer C-Win 
PROCEDURE output-to-printer :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /*     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
         DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
         DEFINE VARIABLE result AS LOGICAL NO-UNDO.
    
    /*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
         IF NOT printok THEN
         RETURN NO-APPLY.
    */
    
      /* Use Progress Print. Always use Font#9 in Registry (set above) */
         RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                                INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
                                        /* use-dialog(1) and landscape(2) */
     */

    RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-screen C-Win 
PROCEDURE output-to-screen :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    RUN scr-rpt.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pChangeDest C-Win 
PROCEDURE pChangeDest :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        IF rd-dest:SCREEN-VALUE EQ "3" THEN
            ASSIGN
                tb_OpenCSV:SCREEN-VALUE = "Yes"
                fi_file:SENSITIVE       = YES
                tb_OpenCSV:SENSITIVE    = YES       
                .
        ELSE 
            ASSIGN
                tb_OpenCSV:SCREEN-VALUE = "NO"
                fi_file:SENSITIVE       = NO
                tb_OpenCSV:SENSITIVE    = NO       
                .
       
        ASSIGN 
            fi_file:SCREEN-VALUE = "c:\tmp\r-cstshp.csv".
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ---------------------------------------------- rm/rep/inkbymchN.p 12/00 JLF */
    /* Ink by Machine                                                             */
    /* -------------------------------------------------------------------------- */

    /*{sys/form/r-topw.f}*/

    DEFINE BUFFER b-jh FOR job-hdr.
    DEFINE BUFFER b-jm FOR job-mat.

    DEFINE VARIABLE v-indus        AS CHARACTER FORMAT "!" INIT "B".
    DEFINE VARIABLE v-fmch         LIKE job-mch.m-code INIT "".
    DEFINE VARIABLE v-tmch         LIKE v-fmch INIT "zzzzzz".
    DEFINE VARIABLE v-fjob         LIKE job.job-no.
    DEFINE VARIABLE v-tjob         LIKE v-fjob INIT "zzzzzzzzz".
    DEFINE VARIABLE v-fjob2        LIKE job.job-no2.
    DEFINE VARIABLE v-tjob2        LIKE v-fjob2 INIT 999.
    DEFINE VARIABLE v-fdat         LIKE job.start-date INIT 01/01/0001
        FORMAT "99/99/9999".
    DEFINE VARIABLE v-tdat         LIKE v-fdat INIT 12/31/9999.

    DEFINE VARIABLE v-skip         AS LOG.
    DEFINE VARIABLE v-pct          AS DECIMAL.
    DEFINE VARIABLE v-ink          AS DECIMAL.
    DEFINE VARIABLE v-cov          AS DECIMAL.
    DEFINE VARIABLE v-board        LIKE job-mat.i-no.
    DEFINE VARIABLE v-adder        LIKE job-mat.i-no EXTENT 3.
    DEFINE VARIABLE v-qty          AS DECIMAL.
    DEFINE VARIABLE v-t-lbs        AS DECIMAL   FORMAT "->,>>>,>>>,>>9.99" EXTENT 3.
    DEFINE VARIABLE v-set          AS CHARACTER.
    DEFINE VARIABLE v-date         LIKE job.start-date.
    DEFINE VARIABLE str-tit4       AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-tit5       AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-line       AS cha       FORM "x(300)" NO-UNDO.
    DEFINE VARIABLE cDisplay       AS cha       NO-UNDO.
    DEFINE VARIABLE cExcelDisplay  AS cha       NO-UNDO.
    DEFINE VARIABLE hField         AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField      AS CHA       NO-UNDO.
    DEFINE VARIABLE cVarValue      AS cha       NO-UNDO.
    DEFINE VARIABLE cExcelVarValue AS cha       NO-UNDO.
    DEFINE VARIABLE cSelectedList  AS cha       NO-UNDO.
    DEFINE VARIABLE cFieldName     AS cha       NO-UNDO.
    DEFINE VARIABLE v-page         AS LOGICAL   NO-UNDO.
    {sys/form/r-topw3.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.


    ASSIGN
        str-tit2 = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        v-indus  = IF tb_fold THEN
             IF tb_corr THEN "B" ELSE "F"
           ELSE
             IF tb_corr THEN "C" ELSE ""
        v-fmch   = begin_mach-no
        v-tmch   = end_mach-no
        v-fjob   = begin_job-no
        v-tjob   = end_job-no
        v-fjob2  = int(begin_job-no2)
        v-tjob2  = int(end_job-no2)
        v-fdat   = begin_date
        v-tdat   = end_date. 

    DEFINE VARIABLE cslist AS cha NO-UNDO.
    FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

        IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
            THEN ASSIGN str-tit4    = str-tit4 + ttRptSelected.TextList + " "
                str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                excelheader = excelHeader + ttRptSelected.TextList + "," .        
        ELSE 
            ASSIGN str-tit4    = str-tit4 + 
            (IF ttRptSelected.HeadingFromLeft THEN
                ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
            ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
                str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                excelheader = excelHeader + ttRptSelected.TextList + ","
                .        
        cSlist = cSlist + ttRptSelected.FieldList + ",".

        IF LOOKUP(ttRptSelected.TextList, "QTY ON HAND,MSF ON HAND,TOTAL COST,MSF Remain,MAT COST,LABOR COST") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " .
    END.

    IF rd-dest EQ 3 THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    SESSION:SET-WAIT-STATE ("general").

    FOR EACH tt-report:
        DELETE tt-report.
    END.

    VIEW FRAME r-top.



    ASSIGN
        v-fjob = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', v-fjob, v-fjob2)) 
        v-tjob = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', v-tjob, v-tjob2)) .

    /* {custom/statusMsg.i "'Processing...'"}  */

    FOR EACH job-hdr
        WHERE job-hdr.opened EQ YES
        {rm/rep/inkbymchN.i}

        FOR EACH tt-report WHERE tt-report.term-id EQ "",

            FIRST job-mat WHERE RECID(job-mat) EQ tt-report.rec-id NO-LOCK,

            FIRST mach
            WHERE mach.company EQ cocode
            AND mach.loc     EQ locode
            AND mach.m-code  EQ tt-report.key-01
            NO-LOCK,

            FIRST job-hdr
            WHERE job-hdr.company EQ cocode
            AND job-hdr.job     EQ job-mat.job
            AND (job-hdr.frm     EQ job-mat.frm OR tt-report.key-08 = "SET")
            NO-LOCK,

            FIRST cust
            WHERE cust.company EQ cocode
            AND cust.cust-no EQ job-hdr.cust-no
            NO-LOCK

            BREAK BY tt-report.key-01
            BY tt-report.key-02
            BY tt-report.key-03
            BY tt-report.key-04
            BY tt-report.key-05

            TRANSACTION:

            {custom/statusMsg.i "'Processing Item # ' + string(job-hdr.i-no)"} 

            IF FIRST-OF(tt-report.key-01) THEN 
            DO:


                PAGE.
                PUT 

                    str-tit3 FORMAT "x(112)"
                    SKIP
                    str-tit4 FORMAT "x(800)"
                    SKIP
                    str-tit5 FORMAT "x(800)"
                    SKIP(1)  .

            END.

            IF FIRST-OF(tt-report.key-03) THEN 
            DO:
                FIND FIRST b-jm
                    WHERE b-jm.company EQ cocode
                    AND b-jm.job     EQ job-hdr.job
                    AND b-jm.job-no  EQ job-hdr.job-no
                    AND b-jm.job-no2 EQ job-hdr.job-no2
                    AND b-jm.frm     EQ job-hdr.frm
                    AND CAN-FIND(FIRST item WHERE item.company  EQ cocode
                    AND item.i-no     EQ b-jm.i-no
                    AND item.mat-type EQ "B")
                    NO-LOCK NO-ERROR.
                v-board = IF AVAILABLE b-jm THEN b-jm.i-no ELSE "".

                ASSIGN
                    v-adder = ""
                    i       = 0.

                FOR EACH b-jm
                    WHERE b-jm.company EQ cocode
                    AND b-jm.job     EQ job-hdr.job
                    AND b-jm.job-no  EQ job-hdr.job-no
                    AND b-jm.job-no2 EQ job-hdr.job-no2
                    AND b-jm.frm     EQ job-hdr.frm
                    AND CAN-FIND(FIRST item WHERE item.company  EQ cocode
                    AND item.i-no     EQ b-jm.i-no
                    AND item.mat-type EQ "A")
                    NO-LOCK:

                    i = i + 1.

                    IF i GT 3 THEN LEAVE.

                    v-adder[i] = b-jm.i-no.
                END.    
            END.

            ASSIGN
                v-date = DATE(int(substr(tt-report.key-02,5,2)),
                     int(substr(tt-report.key-02,7,2)),
                     int(substr(tt-report.key-02,1,4)))
                v-cov  = dec(tt-report.key-06)
                v-qty  = dec(tt-report.key-07).

            IF LAST-OF(tt-report.key-03) THEN 
            DO:

                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "".

                BUFFER b-job-hdr:FIND-BY-ROWID(ROWID(job-hdr), NO-LOCK) .
                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).

                    IF INDEX(cTmpField,".") > 0 THEN 
                    DO:
                        cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                        IF cTmpField = "Cust-no" OR
                            cTmpField = "Sman" THEN hField = BUFFER b-job-hdr:BUFFER-FIELD(cTmpField).
                        ELSE hField = BUFFER b-job-hdr:BUFFER-FIELD(cTmpField).
                        cTmpField = SUBSTRING(GetFieldValue(hField),1,int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                        cDisplay = cDisplay + cTmpField + 
                            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField)).

                        cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".      
                    END.
                    ELSE 
                    DO:            
                        CASE cTmpField:               
                            WHEN "job" THEN 
                                cVarValue = STRING(TRIM(substr(tt-report.key-02,9,13))) .
                            WHEN "frm" THEN 
                                cvarValue = STRING(job-mat.frm,">>9") .
                            WHEN "cust" THEN 
                                cVarValue = STRING(job-hdr.cust-no) .
                            WHEN "name" THEN 
                                cVarValue = STRING(cust.NAME,"x(20)") .
                            WHEN "ino" THEN 
                                cVarValue = STRING(job-hdr.i-no).
                            WHEN "board" THEN 
                                cVarValue =     STRING(v-board).
                            WHEN "addr1" THEN 
                                cVarValue = STRING(v-adder[1]).
                            WHEN "addr2" THEN 
                                cVarValue = STRING(v-adder[2]).
                            WHEN "date" THEN 
                                cVarValue = STRING(v-date) . 
                            WHEN "cov" THEN 
                                cVarValue = STRING(v-cov,">>9").
                            WHEN "qty" THEN 
                                cVarValue =     STRING(v-qty,">,>>>,>>9").
                            WHEN "ink1" THEN 
                                cVarValue = STRING(tt-report.ink-code[1]).
                            WHEN "ink2" THEN 
                                cVarValue = STRING(tt-report.ink-code[2]).
                            WHEN "ink3" THEN 
                                cVarValue = STRING(tt-report.ink-code[3]).
                            WHEN "ink4" THEN 
                                cVarValue = STRING(tt-report.ink-code[4]).
                            WHEN "ink5" THEN 
                                cVarValue = STRING(tt-report.ink-code[5]).
                            WHEN "ink6" THEN 
                                cVarValue = STRING(tt-report.ink-code[6]).    
                            /*WHEN "qtylb" THEN cVarValue = STRING(job-mat.qty,">>,>>9").*/
                            WHEN "ink1rmname" THEN 
                                cVarValue = STRING(tt-report.rm-name[1]).
                            WHEN "ink2rmname" THEN 
                                cVarValue = STRING(tt-report.rm-name[2]).
                            WHEN "ink3rmname" THEN 
                                cVarValue = STRING(tt-report.rm-name[3]).
                            WHEN "ink4rmname" THEN 
                                cVarValue = STRING(tt-report.rm-name[4]).
                            WHEN "ink5rmname" THEN 
                                cVarValue = STRING(tt-report.rm-name[5]).
                            WHEN "ink6rmname" THEN 
                                cVarValue = STRING(tt-report.rm-name[6]).
                            WHEN "ink1rmdscr" THEN 
                                cVarValue = STRING(tt-report.rm-dscr[1]).
                            WHEN "ink2rmdscr" THEN 
                                cVarValue = STRING(tt-report.rm-dscr[2]).
                            WHEN "ink3rmdscr" THEN 
                                cVarValue = STRING(tt-report.rm-dscr[3]).
                            WHEN "ink4rmdscr" THEN 
                                cVarValue = STRING(tt-report.rm-dscr[4]).
                            WHEN "ink5rmdscr" THEN 
                                cVarValue = STRING(tt-report.rm-dscr[5]).
                            WHEN "ink6rmdscr" THEN 
                                cVarValue = STRING(tt-report.rm-dscr[6]).
                            WHEN "qtyperink1" THEN 
                                cVarValue = STRING(tt-report.qty[1]).
                            WHEN "qtyperink2" THEN 
                                cVarValue = STRING(tt-report.qty[2]).
                            WHEN "qtyperink3" THEN 
                                cVarValue = STRING(tt-report.qty[3]).
                            WHEN "qtyperink4" THEN 
                                cVarValue = STRING(tt-report.qty[4]).
                            WHEN "qtyperink5" THEN 
                                cVarValue = STRING(tt-report.qty[5]).
                            WHEN "qtyperink6" THEN 
                                cVarValue = STRING(tt-report.qty[6]).

                        END CASE.

                        IF  cTmpField = "date" THEN
                             cExcelVarValue = IF v-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",v-date) ELSE "".
                        ELSE cExcelVarValue = cVarValue.  
                        cDisplay = cDisplay + cVarValue +
                            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
                        cExcelDisplay = cExcelDisplay + quoter(DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs,cExcelVarValue)) + ",".            
                    END.
                END.
                PUT UNFORMATTED cDisplay SKIP.
                IF rd-dest EQ 3 THEN 
                DO:
                    PUT STREAM excel UNFORMATTED  
                        cExcelDisplay SKIP.
                END.
            END.
            DELETE tt-report.
        END.


        RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

        SESSION:SET-WAIT-STATE ("").

        IF rd-dest EQ 3 THEN 
        DO:
            OUTPUT STREAM excel CLOSE.
            IF tb_OpenCSV THEN
                OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
        END.

    /* END ---------------------------------- copr. 2001  Advanced Software, Inc. */

    END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-param C-Win 
PROCEDURE show-param :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-frame-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE  NO-UNDO.
    DEFINE VARIABLE parm-fld-list AS cha     NO-UNDO.
    DEFINE VARIABLE parm-lbl-list AS cha     NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-label      AS cha.

    lv-frame-hdl = FRAME {&frame-name}:handle.
    lv-group-hdl = lv-frame-hdl:FIRST-CHILD.
    lv-field-hdl = lv-group-hdl:FIRST-CHILD .

    DO WHILE TRUE:
        IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
        IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0
            THEN 
        DO:
            IF lv-field-hdl:LABEL <> ? THEN 
                ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                    parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + "," 
                    .
            ELSE 
            DO:  /* radio set */
                ASSIGN 
                    parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                    .
                lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
                REPEAT:
                    IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                    IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN 
                    DO:
                        parm-lbl-list = parm-lbl-list + lv-field2-hdl:SCREEN-VALUE + ",".
                    END.
                    lv-field2-hdl = lv-field2-hdl:NEXT-SIBLING.                 
                END.       
            END.                 
        END.            
        lv-field-hdl = lv-field-hdl:NEXT-SIBLING.   
    END.

    PUT SPACE(28)
        "< Selection Parameters >"
        SKIP(1).

    DO i = 1 TO NUM-ENTRIES(parm-fld-list,","):
        IF ENTRY(i,parm-fld-list) NE "" OR
            entry(i,parm-lbl-list) NE "" THEN 
        DO:

            lv-label = FILL(" ",34 - length(TRIM(ENTRY(i,parm-lbl-list)))) +
                trim(ENTRY(i,parm-lbl-list)) + ":".

            PUT lv-label FORMAT "x(35)" AT 5
                SPACE(1)
                TRIM(ENTRY(i,parm-fld-list)) FORMAT "x(40)"
                SKIP.              
        END.
    END.

    PUT FILL("-",80) FORMAT "x(80)" SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GEtFieldValue C-Win 
FUNCTION GEtFieldValue RETURNS CHARACTER
    ( hipField AS HANDLE ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    /*RETURN string(hField:BUFFER-VALUE, hField:FORMAT) */
    RETURN STRING(hipField:BUFFER-VALUE).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

