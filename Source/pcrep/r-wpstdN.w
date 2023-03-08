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
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
    cocode = gcompany
    locode = gloc.

DEFINE NEW SHARED BUFFER xest FOR est.

{sys/inc/msfcalc.i}

{ce/mach-ink.i NEW}

DEFINE STREAM st-excell.


DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-m-code           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdOutputProcs      AS HANDLE    NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.

ASSIGN 
    cTextListToSelect  = "Machine#,DP,F,B,P,Charge Code,Charge Cat,Date,Job#,Shift,Hours,Start,Stop,CR,Qty," +  /*14*/     
                         "Waste,C,FG Item,Style,Length,Width,Depth,Blank Len,Blank Wid," +  /*9*/     
                         "Blank Sq In.,Board,Board Cal,MSF,Wgt/MSF,Roll Width," +  /*6*/     
                         "Gross S Wid,Gross S Len,Net Sht Wid,Net Sht Len," +  /*4*/     
                         "Film Wid,Film Len,# Colors,Die Inches,Number Up,Number Out,Glue Inches,Tot Job Run Qty," + /*8*/     
                         "Cust#,Cust Name,Price,UOM,Sales Value,User ID,Cuts" /* 7 */
    cFieldListToSelect = "mch-act.m-code,deprt,mch-act.frm,mch-act.blank-no,pass,mch-act.code,job-code,op-date,job-no,mch-act.shift,mch-act.hours,start,stop,crew,mch-act.qty," +
                         "mch-act.waste,comp,stock-no,style,len,wid,dep,t-len,t-wid," +
                         "t-sqin,board,cal,ld-msf,weight,roll-wid," +
                         "gsh-wid,gsh-len,nsh-wid,nsh-len," +
                         "flm-len,flm-wid,inkc,die-in,li-up,n-out,lin-in,tot-job-qty," +
                         "cust-no,name,price,uom,sale-value,user-id,n-cuts"
    cFieldLength       = "8,2,3,2,3,11,10,8,13,5,8,5,5,2,10,"
                       + "6,1,15,8,9,9,7,9,9,"
                       + "12,8,9,9,9,10,"
                       + "11,11,11,11,"
                       + "9,9,8,10,9,10,11,15,"
                       + "8,30,17,4,20,10,7"
    cFieldType         = "c,c,i,i,i,c,c,c,c,i,i,c,c,i,i,"
                       + "i,c,c,c,i,i,i,i,i,"
                       + "i,c,i,i,i,i,"
                       + "i,i,i,i,"
                       + "i,i,i,i,i,i,i,i,"
                       + "c,c,i,c,i,c,i"
                       .

{sys/inc/ttRptSel.i}
cTextListToDefault = "Machine#,F,B,Charge Code,Charge Cat,Date,Job#,Shift,Hours,Qty," +  /*14*/     
                     "Waste,FG Item,Style,Length,Width,Depth,Blank Len,Blank Wid," +  /*9*/     
                     "Blank Sq In.,Board,Board Cal,MSF,Wgt/MSF,Roll Width," +  /*6*/     
                     "Gross S Wid,Gross S Len,Net Sht Wid,Net Sht Len," +  /*4*/     
                     "Film Wid,Film Len,# Colors,Die Inches,Number Up,Number Out,Glue Inches" /*7*/
                     .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_mach end_mach begin_date ~
end_date begin_shift end_shift select-mach tb_comp rd_sort sl_avail ~
sl_selected Btn_Def Btn_Add Btn_Remove btn_down btn_Up rd-dest fi_file ~
tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_mach end_mach begin_date end_date ~
begin_shift end_shift select-mach tb_comp lbl_sort rd_sort sl_avail ~
sl_selected rd-dest fi_file tb_OpenCSV tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD cvt-time-to-string C-Win 
FUNCTION cvt-time-to-string RETURNS CHARACTER
    (INPUT ip-type AS CHARACTER, INPUT ip-stime AS INTEGER, INPUT ip-hour AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetFieldValue C-Win 
FUNCTION GetFieldValue RETURNS CHARACTER
    ( hipField AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 16 BY 1.29.

DEFINE BUTTON Btn_Add 
     LABEL "&Add >>" 
     SIZE 16 BY 1.1.

DEFINE BUTTON Btn_Def 
     LABEL "&Default" 
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
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_mach AS CHARACTER FORMAT "X(6)" 
     LABEL "Beginning Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_shift AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Beginning Shift" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_mach AS CHARACTER FORMAT "X(6)" INITIAL "zzzzzz" 
     LABEL "Ending Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_shift AS INTEGER FORMAT ">9" INITIAL 99 
     LABEL "Ending Shift" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\WIPStandardsDetail.csv" 
     LABEL "Name" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 49 BY 1.

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

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

DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "L" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Portrait", "P",
"Landscape", "L"
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To CSV", 3
     SIZE 15 BY 4.76 NO-UNDO.

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Machine Code" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Machine Code", "Machine Code",
"Job#", "Job#",
"Date / Time for each individual Job#", "Date / Time for each individual Job#"
     SIZE 65.8 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 5.67.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 9.71.

DEFINE VARIABLE select-mach AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 24 BY 3.81 NO-UNDO.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 6.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 6.19 NO-UNDO.

DEFINE VARIABLE tbAutoClose AS LOGICAL INITIAL no 
     LABEL "Auto Close" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_comp AS LOGICAL INITIAL yes 
     LABEL "Show only Completed Machines?" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_OpenCSV AS LOGICAL INITIAL no 
     LABEL "Open CSV?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.4 BY .95 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_mach AT ROW 2.29 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Machine"
     end_mach AT ROW 2.29 COL 64 COLON-ALIGNED HELP
          "Enter Ending Machine"
     begin_date AT ROW 3.52 COL 26 COLON-ALIGNED
     end_date AT ROW 3.52 COL 64 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     begin_shift AT ROW 4.81 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Shift" WIDGET-ID 150
     end_shift AT ROW 4.81 COL 64 COLON-ALIGNED HELP
          "Enter Ending Shift" WIDGET-ID 152
     select-mach AT ROW 6 COL 59 HELP
          "Enter description of this Material Type." NO-LABEL
     tb_comp AT ROW 7.43 COL 20
     lbl_sort AT ROW 9.95 COL 19.8 NO-LABEL
     rd_sort AT ROW 9.95 COL 26.8 NO-LABEL
     sl_avail AT ROW 12.05 COL 4 NO-LABEL WIDGET-ID 146
     sl_selected AT ROW 12.05 COL 60.6 NO-LABEL WIDGET-ID 148
     Btn_Def AT ROW 12.19 COL 40.6 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     Btn_Add AT ROW 13.38 COL 40.6 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 138
     Btn_Remove AT ROW 14.62 COL 40.6 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 142
     btn_down AT ROW 15.86 COL 40.6 WIDGET-ID 140
     btn_Up AT ROW 17.1 COL 40.6 WIDGET-ID 144
     lv-ornt AT ROW 19.29 COL 31 NO-LABEL
     lines-per-page AT ROW 19.29 COL 84 COLON-ALIGNED
     rd-dest AT ROW 19.76 COL 6 NO-LABEL
     lv-font-no AT ROW 20.05 COL 34 COLON-ALIGNED
     tb_excel AT ROW 20.29 COL 76 RIGHT-ALIGNED
     lv-font-name AT ROW 21.1 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 22.19 COL 28.6
     fi_file AT ROW 23.14 COL 26.6 COLON-ALIGNED HELP
          "Enter File Name"
     tb_OpenCSV AT ROW 23.14 COL 92.4 RIGHT-ALIGNED
     tbAutoClose AT ROW 24.86 COL 28.6 WIDGET-ID 78
     btn-ok AT ROW 25.76 COL 28.6
     btn-cancel AT ROW 25.76 COL 48.6
     " Output Destination" VIEW-AS TEXT
          SIZE 19 BY 1 AT ROW 18.62 COL 5
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 11.29 COL 60.4 WIDGET-ID 44
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 11.29 COL 4.4 WIDGET-ID 38
     "Exclude Machines:" VIEW-AS TEXT
          SIZE 17.6 BY .62 AT ROW 6 COL 41 WIDGET-ID 154
     " Selection Parameters" VIEW-AS TEXT
          SIZE 21.2 BY .71 AT ROW 1.14 COL 5
     RECT-6 AT ROW 19.14 COL 4
     RECT-7 AT ROW 1.48 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96 BY 26.86
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
         TITLE              = "WIP Standards Detail Report"
         HEIGHT             = 26.86
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
       begin_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_shift:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       end_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_shift:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       lbl_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

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
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_comp:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
       tb_excel:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* WIP Standards Detail Report */
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
ON WINDOW-CLOSE OF C-Win /* WIP Standards Detail Report */
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


&Scoped-define SELF-NAME begin_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mach C-Win
ON LEAVE OF begin_mach IN FRAME FRAME-A /* Beginning Machine */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_shift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_shift C-Win
ON LEAVE OF begin_shift IN FRAME FRAME-A /* Beginning Shift */
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
                        MESSAGE "CSV file have been created." SKIP(1)
                            "~"OK"~" to open CSV file?"
                            VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL
                            TITLE "" UPDATE lChoice AS LOGICAL.
                 
                        IF lChoice THEN
                        DO:
                            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
                        END.
                    END.
                    ELSE DO:
                        OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
                    END.
                END. /* WHEN 3 THEN DO: */
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
        DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

        APPLY "DEFAULT-ACTION" TO sl_avail.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def C-Win
ON CHOOSE OF Btn_Def IN FRAME FRAME-A /* Default */
DO:
        DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

        RUN DisplaySelectionDefault.  /* task 04041406 */ 
        RUN DisplaySelectionList2 .

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


&Scoped-define SELF-NAME end_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_mach C-Win
ON LEAVE OF end_mach IN FRAME FRAME-A /* Ending Machine */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_shift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_shift C-Win
ON LEAVE OF end_shift IN FRAME FRAME-A /* Ending Shift */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON HELP OF fi_file IN FRAME FRAME-A /* Name */
DO:
        DEFINE VARIABLE ls-filename AS CHARACTER NO-UNDO.
        DEFINE VARIABLE ll-ok       AS LOG       NO-UNDO.

        SYSTEM-DIALOG GET-FILE ls-filename 
            TITLE "Select File to Save "
            FILTERS "Excel Files    (*.csv)" "*.csv",
            "All Files    (*.*) " "*.*"
            INITIAL-DIR "c:\tmp"
            MUST-EXIST
            USE-FILENAME
            UPDATE ll-ok.

        IF ll-ok THEN SELF:SCREEN-VALUE = ls-filename.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

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


&Scoped-define SELF-NAME select-mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL select-mach C-Win
ON VALUE-CHANGED OF select-mach IN FRAME FRAME-A
DO:
        ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
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
    FIND FIRST sys-ctrl WHERE sys-ctrl.company = cocode
        AND sys-ctrl.NAME = "AutoPDC" NO-LOCK NO-ERROR.
    IF AVAILABLE sys-ctrl THEN 
        ASSIGN
            select-mach:LIST-ITEMS = sys-ctrl.char-fld.

    ASSIGN
        begin_date = DATE (1,1,YEAR(TODAY))
        end_date   = DATE (12,31,YEAR(TODAY)).
    RUN DisplaySelectionList.
    btn-ok:load-image("Graphics/32x32/Ok.png").
    btn-cancel:load-image("Graphics/32x32/cancel.png").
    Btn_Def:load-image("Graphics/32x32/default.png").
    Btn_Add:load-image("Graphics/32x32/additem.png").
    Btn_Remove:load-image("Graphics/32x32/remove.png").
    btn_Up:load-image("Graphics/32x32/moveup.png").
    btn_down:load-image("Graphics/32x32/movedown.png").
    RUN enable_UI.
    {methods/nowait.i}
    {sys/inc/reportsConfigNK1.i "DR14" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
RUN DisplaySelectionList2.
APPLY "entry" TO begin_mach.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionDefault C-Win 
PROCEDURE DisplaySelectionDefault :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.

    DO iCount = 1 TO NUM-ENTRIES(cTextListToDefault):

        cListContents = cListContents +                   
            (IF cListContents = "" THEN ""  ELSE ",") +
            ENTRY(iCount,cTextListToDefault)   .
    END.            
    sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

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
    DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.

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
            ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
            .    

    END.

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
    DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.
  
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
  DISPLAY begin_mach end_mach begin_date end_date begin_shift end_shift 
          select-mach tb_comp lbl_sort rd_sort sl_avail sl_selected rd-dest 
          fi_file tb_OpenCSV tbAutoClose 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_mach end_mach begin_date end_date begin_shift 
         end_shift select-mach tb_comp rd_sort sl_avail sl_selected Btn_Def 
         Btn_Add Btn_Remove btn_down btn_Up rd-dest fi_file tb_OpenCSV 
         tbAutoClose btn-ok btn-cancel 
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
    DEFINE VARIABLE cTmpList AS CHARACTER NO-UNDO.

    EMPTY TEMP-TABLE ttRptSelected.
    cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    iColumnLength = 0.

    DO i = 1 TO sl_selected:NUM-ITEMS /* IN FRAME {&FRAME-NAME}*/ :
        FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-LOCK NO-ERROR.

        IF NOT AVAILABLE ttRptList THEN
            MESSAGE "no " i ENTRY(i,ctmplist) SKIP
                ctmplist
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
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
    DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

    IF init-dir = "" THEN init-dir = "c:\temp" .
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

    IF NOT OKpressed THEN  RETURN NO-APPLY.


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

    /*list-name = fi_file.*/

    RUN scr-rpt.w (list-name,c-win:TITLE,INT(lv-font-no),lv-ornt). /* open file-name, title */ 

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
                tb_excel                = YES
                .
        ELSE
            ASSIGN
                tb_OpenCSV:SCREEN-VALUE = "NO"
                fi_file:SENSITIVE       = NO
                tb_OpenCSV:SENSITIVE    = NO
                tb_excel                = NO
                .
        ASSIGN 
            fi_file:SCREEN-VALUE = "c:\tmp\WIPStandardsDetail.csv".   
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report PRIVATE :
DEFINE VARIABLE str-tit4 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE str-tit5 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE str-line AS cha       FORM "x(300)" NO-UNDO.  
 /*{sys/form/r-top5DL3.f}*/
    {sys/form/r-top3w.f}
    DEFINE VARIABLE str_buffa      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-hdr          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-rc-seq      LIKE dept.fc NO-UNDO.
    DEFINE VARIABLE ld-msf         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE li-up          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-out         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-mach         AS CHARACTER NO-UNDO.

    DEFINE VARIABLE excelheader    AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cDisplay       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelDisplay  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hField         AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVarValue      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelVarValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedList  AS CHARACTER NO-UNDO.
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE tot-job-qty AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cUOM        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dPrice      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dSaleValue  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iPrint-line AS INTEGER   NO-UNDO .
    DEFINE BUFFER b-mch-act  FOR mch-act.
    DEFINE BUFFER bf-mch-act FOR mch-act.

    SESSION:SET-WAIT-STATE ("general").

    ASSIGN
        str-tit2 = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        v-hdr    = "Machine#,F,B,Charge Code,Charge Category,Date,Job#,Shift,Hours,Qt" +
         "y,Waste,FG Item,Style,Length,Width,Depth,Blank Length,Blank Width" +
         ",Blank Square Inches,Board Code,Board Caliper,MSF,Wgt/MSF,Roll Wi" +
         "dth,Gross Sheet Width,Gross Sheet Length,Net Sheet Width,Net Shee" +
         "t Length,Film Width,Film Length,# Colors,Die Inches,Number Up,Num" +
         "ber Out,Glue Inches,Cust#,Cust Name,Price,UOM,Sales Value,Cuts".

    DO WITH FRAME {&frame-name}:  
        DO i = 1 TO select-mach:num-items:
            IF select-mach:is-selected(i) THEN 
            DO:
                v-mach = v-mach + trim(substr(select-mach:entry(i),1,5)) + "," .
            END.
        END.

        IF LENGTH(TRIM(v-mach)) NE 0 AND
            substr(v-mach,LENGTH(TRIM(v-mach)),1) EQ "," THEN
            substr(v-mach,LENGTH(TRIM(v-mach)),1) = "".
    END.

    FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

        ASSIGN 
            str-tit4    = str-tit4 + 
               ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength + 1 - LENGTH(ttRptSelected.TextList))
            str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
            excelheader = excelHeader + ttRptSelected.TextList + ",".        

    /* IF LOOKUP(ttRptSelected.TextList, "Inv Amt,Amt Paid,Delta,GrossProfit%,Comm Amt,Comm Pct") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
     ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . */
    END.

    {sys/inc/print1.i}

    {sys/inc/outprint.i VALUE(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    RUN est/rc-seq.p (OUTPUT lv-rc-seq).

    DISPLAY "" WITH FRAME r-top.
    PUT str-tit4 FORMAT "x(485)" SKIP
        str-tit5 FORMAT "x(485)" SKIP .

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM st-excell TO VALUE(cFileName).

        /* PUT STREAM st-excell UNFORMATTED v-hdr SKIP.*/
        PUT STREAM st-excell UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    FOR EACH mch-act NO-LOCK
        WHERE mch-act.company EQ cocode
        AND mch-act.m-code  GE begin_mach
        AND mch-act.m-code  LE end_mach
        AND mch-act.op-date GE begin_date
        AND mch-act.op-date LE end_date
        AND mch-act.shift   GE begin_shift
        AND mch-act.shift   LE end_shift
        AND (( mch-act.COMPLETE AND tb_comp ) OR ( NOT tb_comp))
        AND LOOKUP(mch-act.m-code,v-mach) EQ 0
        USE-INDEX dly-idx,

        FIRST mach NO-LOCK
        WHERE mach.company EQ mch-act.company
        AND mach.m-code  EQ mch-act.m-code,

        FIRST job NO-LOCK
        WHERE job.company EQ mch-act.company
        AND job.job     EQ mch-act.job
        AND job.job-no  EQ mch-act.job-no
        AND job.job-no2 EQ mch-act.job-no2,
        FIRST job-hdr OF job NO-LOCK
        USE-INDEX job BY (IF rd_sort BEGINS "M" THEN mch-act.m-code ELSE "")
        BY (IF rd_sort BEGINS "J" THEN STRING(mch-act.job-no,"x(9)") + STRING(mch-act.job-no2,"999") ELSE "")
        BY (IF rd_sort BEGINS "D" THEN STRING(YEAR(mch-act.op-date),"9999") + STRING(MONTH(mch-act.op-date),"99") + STRING(DAY(mch-act.op-date),"99") + STRING(mch-act.start,"999999") ELSE "")
        :

        {custom/statusMsg.i "'Processing Machine # ' + string(mch-act.m-code)"} 

        FIND FIRST job-code WHERE job-code.code EQ mch-act.code NO-LOCK NO-ERROR.

        FIND FIRST job-mch NO-LOCK
            WHERE job-mch.company  EQ mch-act.company
            AND job-mch.job      EQ mch-act.job
            AND job-mch.job-no   EQ mch-act.job-no
            AND job-mch.job-no2  EQ mch-act.job-no2
            AND job-mch.frm      EQ mch-act.frm
            AND job-mch.blank-no EQ mch-act.blank-no
            AND job-mch.m-code   EQ mch-act.m-code
            AND job-mch.pass     EQ mch-act.pass
            NO-ERROR.
    
        FIND FIRST cust NO-LOCK
            WHERE cust.company EQ mch-act.company
            AND cust.cust-no EQ job-hdr.cust-no
            NO-ERROR.
        FIND FIRST oe-ordl NO-LOCK
            WHERE oe-ordl.company EQ mch-act.company
            AND oe-ordl.i-no    EQ mch-act.i-no
            AND oe-ordl.ord-no  EQ job-hdr.ord-no
            NO-ERROR.
        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ mch-act.company
            AND itemfg.i-no    EQ mch-act.i-no
            NO-ERROR.

        RELEASE est.
        RELEASE ef.
        RELEASE eb.

        IF TRIM(job.est-no) NE "" THEN
            FIND FIRST est NO-LOCK
                WHERE est.company EQ job.company
                AND est.est-no  EQ job.est-no
                NO-ERROR.

        IF AVAILABLE est THEN
            FIND FIRST ef NO-LOCK
                WHERE ef.company EQ est.company
                AND ef.est-no  EQ est.est-no
                AND ef.form-no EQ mch-act.frm
                NO-ERROR.

        IF AVAILABLE ef THEN
            FIND FIRST eb NO-LOCK
                WHERE eb.company   EQ ef.company
                AND eb.est-no    EQ ef.est-no
                AND eb.form-no   EQ ef.form-no
                AND (eb.blank-no EQ mch-act.blank-no OR mch-act.blank-no EQ 0)
                NO-ERROR.

        IF AVAILABLE ef THEN
            FIND FIRST est-flm NO-LOCK
                WHERE est-flm.company  EQ ef.company
                AND est-flm.est-no   EQ ef.est-no
                AND est-flm.snum     EQ ef.form-no
                AND est-flm.bnum     EQ mch-act.blank-no
                NO-ERROR.

        IF AVAILABLE est THEN
            FIND FIRST est-op NO-LOCK
                WHERE est-op.company EQ est.company
                AND est-op.est-no  EQ est.est-no
                AND est-op.s-num   EQ mch-act.frm
                AND (est-op.b-num  EQ mch-act.blank-no OR
                mch-act.blank-no EQ 0)
                AND est-op.m-code  EQ mch-act.m-code
                AND est-op.op-pass EQ mch-act.pass
                AND est-op.dept    EQ mch-act.dept
                AND est-op.line    LT 500
                NO-ERROR.

        ld-msf = 0.
        IF AVAILABLE ef THEN 
        DO:
            IF mach.d-seq LT lv-rc-seq THEN
                ld-msf = ef.nsh-len * ef.nsh-wid.
            ELSE
                ld-msf = ef.gsh-len * ef.gsh-wid.

            IF v-corr THEN ld-msf = ld-msf * .007.
            ELSE ld-msf = ld-msf / 144.

            ld-msf = ld-msf / 1000.
        END.

        RELEASE w-ink.
        IF AVAILABLE est THEN 
        DO:
            FIND xest WHERE ROWID(xest) EQ ROWID(est) NO-LOCK NO-ERROR.
            RUN ce/mach-ink.p.
            FIND FIRST w-ink
                WHERE w-ink.form-no EQ mch-act.frm
                AND w-ink.pass    EQ mch-act.pass
                NO-ERROR.
        END.

        li-up = 1.
        IF AVAILABLE job-mch THEN 
        DO:
            IF job-mch.n-out GT 0 THEN li-up = job-mch.n-out.
            IF job-mch.n-on  GT 0 THEN li-up = job-mch.n-on / li-up.
            IF li-up EQ ? THEN li-up = 1.
        END.
        ASSIGN 
            tot-job-qty = 0 .
        FOR EACH bf-mch-act NO-LOCK
            WHERE bf-mch-act.company EQ cocode
            AND bf-mch-act.m-code  GE mch-act.m-code
            /*AND mch-act.op-date GE begin_date
            AND mch-act.op-date LE end_date*/
            AND bf-mch-act.shift   EQ mch-act.shift
            AND bf-mch-act.job-no  EQ mch-act.job-no
            AND bf-mch-act.job-no2  EQ mch-act.job-no2 :

            tot-job-qty =  tot-job-qty +  bf-mch-act.qty .
        END.

        IF AVAILABLE mch-act THEN
            BUFFER b-mch-act:FIND-BY-ROWID(ROWID(mch-act), NO-LOCK) .
        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "".    

        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).

            IF INDEX(cTmpField,".") > 0 THEN 
            DO:
                cFieldName = cTmpField.
                cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                IF cFieldName BEGINS "mch-act" THEN hField = IF AVAILABLE b-mch-act THEN BUFFER b-mch-act:BUFFER-FIELD(cTmpField) ELSE ?.

                IF hField <> ? THEN 
                DO:                 
                    cTmpField = SUBSTRING(GetFieldValue(hField),1,int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                    IF ENTRY(i,cSelectedList) = "Hours" THEN  
                        cTmpField =  IF cTmpField <> "" THEN  STRING(mch-act.hours,"->>>9.9<<") ELSE "". 
                    IF ENTRY(i,cSelectedList) = "Qty" THEN  
                        cTmpField =  IF cTmpField <> "" THEN  STRING(mch-act.qty,"->>>>>>>>>") ELSE "".
                    IF ENTRY(i,cSelectedList) = "Waste" THEN  
                        cTmpField =  IF cTmpField <> "" THEN  STRING(mch-act.waste,"->>>>>") ELSE "".
                    IF ENTRY(i,cSelectedList) = "F" THEN  
                        cTmpField =  IF cTmpField <> "" THEN  STRING(mch-act.frm,">>9") ELSE "".
                    IF ENTRY(i,cSelectedList) = "B" THEN  
                        cTmpField =  IF cTmpField <> "" THEN  STRING(mch-act.blank-no,">9") ELSE "".  
                     
                    cDisplay = cDisplay + cTmpField + 
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField))
                        .
                    cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".
                END.
                ELSE 
                DO:
                    cTmpField = SUBSTRING(cFieldName,1,int( ENTRY( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                    cDisplay = cDisplay + FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).
                    cExcelDisplay = cExcelDisplay + quoter(" ")  /*GetFieldValue(hField))*/ + ",".
                END.
            END.
            ELSE 
            DO: 

                CASE cTmpField:
                    WHEN "user-id" THEN 
                        cVarValue = IF AVAILABLE mch-act THEN mch-act.user-id ELSE "". 
                    WHEN "job-code" THEN 
                        cVarValue = IF AVAILABLE job-code THEN STRING(job-code.cat,"x(10)") ELSE "". 
                    WHEN "job-no" THEN 
                        cVarValue = STRING(TRIM(job.job-no) + "-" + STRING(job.job-no2,"999")) .
                    WHEN "stock-no" THEN 
                        cVarValue = IF AVAILABLE mch-act THEN mch-act.i-no ELSE IF AVAILABLE eb THEN STRING(eb.stock-no) ELSE "".
                    WHEN "style" THEN 
                        cVarValue = IF AVAILABLE eb THEN STRING(eb.style) ELSE "".          
                    WHEN "len" THEN 
                        cVarValue = IF AVAILABLE eb THEN STRING(eb.len,">>>>>9.9<<<<<") ELSE "".            
                    WHEN "wid" THEN 
                        cVarValue = IF AVAILABLE eb THEN STRING(eb.wid,">>>>>9.9<<<<<") ELSE "".            
                    WHEN "dep" THEN 
                        cVarValue = IF AVAILABLE eb THEN STRING(eb.dep,">>9.9<<") ELSE "".
                    WHEN "t-len" THEN 
                        cVarValue = IF AVAILABLE eb THEN STRING(eb.t-len,">>>>>9.9<<<<<") ELSE "".
                    WHEN "t-wid" THEN 
                        cVarValue = IF AVAILABLE eb THEN STRING(eb.t-wid,">>>>>9.9<<<<<") ELSE "".
                    WHEN "t-sqin" THEN 
                        cVarValue = IF AVAILABLE eb THEN STRING(eb.t-sqin,">>>>>>>9.9<<") ELSE "".
                    WHEN "board" THEN 
                        cVarValue = IF AVAILABLE ef THEN STRING(ef.board,"x(8)") ELSE "".
                    WHEN "cal" THEN 
                        cVarValue = IF AVAILABLE ef THEN STRING(ef.cal,">>>>9.9<<") ELSE "".
                    WHEN "ld-msf" THEN 
                        cVarValue = IF AVAILABLE ef THEN STRING(ld-msf,">>>>9.9<<") ELSE "".
                    WHEN "weight" THEN 
                        cVarValue = IF AVAILABLE ef THEN STRING(ef.weight,">>>>9.9<<") ELSE "".
                    WHEN "roll-wid" THEN 
                        cVarValue = IF AVAILABLE ef THEN STRING(ef.roll-wid,">>>>>9.9<<") ELSE "".
                    WHEN "gsh-wid" THEN 
                        cVarValue = IF AVAILABLE ef THEN STRING(ef.gsh-wid,">>>>>9.9<<") ELSE "".
                    WHEN "gsh-len" THEN 
                        cVarValue = IF AVAILABLE ef THEN STRING(ef.gsh-len,">>>>>9.9<<") ELSE "".
                    WHEN "nsh-wid" THEN 
                        cVarValue = IF AVAILABLE ef THEN STRING(ef.nsh-wid,">>>>>9.9<<") ELSE "".         
                    WHEN "nsh-len" THEN 
                        cVarValue = IF AVAILABLE ef THEN STRING(ef.nsh-len,">>>>>9.9<<<<<") ELSE "".
                    WHEN "flm-len" THEN 
                        cVarValue = IF AVAILABLE est-flm THEN STRING(est-flm.len,">>>>>9.9<<<<<") ELSE "".
                    WHEN "flm-wid" THEN 
                        cVarValue = IF AVAILABLE est-flm THEN STRING(est-flm.wid,">>>>>9.9<<<<<") ELSE "".
                    WHEN "inkc" THEN 
                        cVarValue = IF AVAILABLE w-ink THEN STRING(w-ink.inks + w-ink.varn,">>") ELSE "".
                    WHEN "die-in" THEN 
                        cVarValue = IF AVAILABLE ef THEN STRING(ef.die-in,">>>>9") ELSE "".
                    WHEN "li-up" THEN 
                        cVarValue = STRING(li-up,">>>") .
                    WHEN "n-out" THEN 
                        cVarValue = IF AVAILABLE job-mch THEN STRING(job-mch.n-out,">>>>9") ELSE "".
                    WHEN "lin-in" THEN 
                        cVarValue =IF AVAILABLE eb THEN STRING(eb.lin-in,">>>>9.9<<") ELSE "".
                    WHEN "deprt" THEN 
                        cVarValue = IF AVAILABLE job-mch THEN STRING(mch-act.dep,"x(2)") ELSE "".
                    WHEN "pass" THEN 
                        cVarValue = IF AVAILABLE job-mch THEN STRING(mch-act.pass,">>9") ELSE "".
                    WHEN "start" THEN 
                        cVarValue = IF AVAILABLE mch-act THEN cvt-time-to-string('',mch-act.start,0.00) ELSE "".
                    WHEN "stop" THEN 
                        cVarValue = IF AVAILABLE mch-act THEN cvt-time-to-string('',mch-act.stopp,0.00) ELSE "".
                    WHEN "crew" THEN 
                        cVarValue = IF AVAILABLE mch-act THEN STRING(mch-act.crew,">>") ELSE "".
                    WHEN "comp" THEN 
                        cVarValue = IF AVAILABLE mch-act AND mch-act.COMPLETE THEN "Y" ELSE "N".  
                    WHEN "tot-job-qty" THEN 
                        cVarValue = STRING(tot-job-qty,"->>,>>>,>>>,>>9").
                    WHEN "cust-no" THEN 
                        cVarValue = job-hdr.cust-no.
                    WHEN "name" THEN 
                        cVarValue = IF AVAILABLE cust THEN cust.name ELSE "".
                    WHEN "price" THEN
                        ASSIGN
                            dPrice    = IF AVAILABLE oe-ordl THEN oe-ordl.price
                            ELSE IF AVAILABLE itemfg THEN itemfg.sell-price
                            ELSE 0
                            cVarValue = STRING(dPrice,">>,>>>,>>9.99<<<<")
                            .
                    WHEN "uom" THEN
                        ASSIGN
                            cUOM      = IF AVAILABLE oe-ordl THEN oe-ordl.pr-uom
                          ELSE IF AVAILABLE itemfg THEN itemfg.pur-uom
                          ELSE ""
                            cVarValue = cUOM
                            .
                    WHEN "sale-value" THEN 
                        DO:
                            dSaleValue = dPrice * (mch-act.qty / IF cUOM EQ "M" THEN 1000 ELSE 1).
                            IF CAN-DO("A,R,S",mach.p-type) THEN
                                dSaleValue = dSaleValue * li-up * (IF AVAILABLE job-mch AND job-mch.n-out GT 0 THEN job-mch.n-out ELSE 1).
                            cVarValue = STRING(dSaleValue,">,>>>,>>>,>>9.99<<<<").
                        END. /* sale-value */
                    WHEN "n-cuts" THEN
                        cVarValue = IF AVAILABLE ef THEN STRING(ef.n-cuts,">>>,>>9") ELSE "".
                    WHEN "op-date" THEN
                        cVarValue = IF mch-act.op-date NE ? THEN STRING(mch-act.op-date,"99/99/99") ELSE "".
                END CASE.

                IF  cTmpField = "op-date" THEN
                     cExcelVarValue = IF mch-act.op-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",mch-act.op-date) ELSE "".
                ELSE cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs, cExcelVarValue)) + ",".            
            END. 
        END.
      
        iPrint-line = iPrint-line + 1 .
        IF iPrint-line GE lines-per-page - 12 THEN 
        DO:
            PAGE.
            iPrint-line = 0 .
            PUT str-tit4 FORMAT "x(485)" SKIP
                str-tit5 FORMAT "x(485)" SKIP .
        END.
        PUT UNFORMATTED cDisplay SKIP.
        IF tb_excel THEN 
        DO:
            cExcelDisplay = cExcelDisplay.
            PUT STREAM st-excell UNFORMATTED  
                cExcelDisplay SKIP.
        END.

    END.

    SESSION:SET-WAIT-STATE("").

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM st-excell CLOSE.
    END.

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

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
    DEFINE VARIABLE lv-frame-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE    NO-UNDO.
    DEFINE VARIABLE parm-fld-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE parm-lbl-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION cvt-time-to-string C-Win 
FUNCTION cvt-time-to-string RETURNS CHARACTER
    (INPUT ip-type AS CHARACTER, INPUT ip-stime AS INTEGER, INPUT ip-hour AS DECIMAL ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/

    IF ip-type = "END" THEN 
    DO:
        DEFINE VARIABLE li-end-time AS INTEGER NO-UNDO.
        li-end-time = ip-stime + ip-hour * 3600.
        RETURN STRING(li-end-time,"HH:MM").
    END.
    ELSE
        RETURN STRING(ip-stime,"HH:MM").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetFieldValue C-Win 
FUNCTION GetFieldValue RETURNS CHARACTER
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

