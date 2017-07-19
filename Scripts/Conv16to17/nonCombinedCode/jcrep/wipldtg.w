&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: jcrep/wipldtg.w

  Description: WIP Tag Creation

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

/* Parameters Definitions ---    */

DEFINE INPUT PARAMETER ipc-RMTagNo AS CHAR FORMAT "x(20)" INIT "" NO-UNDO.
DEFINE INPUT PARAMETER ipc-job-no AS CHAR FORMAT "x(6)" INIT "" NO-UNDO.
DEFINE INPUT PARAMETER ipi-job-no2 AS INT FORMAT "99" INIT 0 NO-UNDO.
DEFINE INPUT PARAMETER ipc-i-no AS CHAR FORMAT "x(10)" INIT "" NO-UNDO.
DEFINE INPUT PARAMETER ipi-form AS INT FORMAT ">9" INIT 0 NO-UNDO.
DEFINE INPUT PARAMETER ipi-blank AS INT FORMAT ">9" INIT 0 NO-UNDO.
DEFINE INPUT PARAMETER ipd-qty AS DEC FORMAT "->>>>>>9.9<<<<<" INIT 0 NO-UNDO.
DEFINE INPUT PARAMETER ipc-pur-uom AS CHAR FORMAT "x(3)" INIT "" NO-UNDO. 

DEFINE OUTPUT PARAMETER opc-output AS CHAR FORMAT "x(200)" INIT "" NO-UNDO.    

/* Local Variable Definitions ---                                       */
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEFINE VARIABLE correct-error AS LOGICAL NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

DEF VAR lines-per-page AS INT NO-UNDO.
DEF var v-out AS char FORMAT "x(40)" NO-UNDO.
DEF var v-job AS char FORMAT "x(9)" NO-UNDO.

DEF VAR v-loadtag AS CHAR NO-UNDO INIT "ASI". /* sys ctrl option */
DEF VAR v-mult    AS INT  NO-UNDO INIT 0. /* sys ctrl option */
DEF VAR v-cas-lab AS LOG  NO-UNDO. /* sys ctrl option */
DEF VAR v-tags    AS DEC  NO-UNDO INIT 0. /* sys ctrl option */
DEF VAR v-count   AS INT  NO-UNDO INIT 0.
DEF VAR v-calc    AS LOG  NO-UNDO. /* sys ctrl cption */
DEF VAR v-bin     AS CHAR NO-UNDO. /* sys ctrl cption */
DEF VAR v-wipbin-whs AS CHAR NO-UNDO.
DEF VAR v-wipbin-bin AS CHAR NO-UNDO.

DEF VAR vl-InputParameters AS LOGICAL INIT FALSE NO-UNDO.

DEF BUFFER b-company FOR company.

DEF TEMP-TABLE w-file FIELD w-key AS ROWID.
DEF TEMP-TABLE tt-tag FIELD tt-recid AS RECID.

DEF TEMP-TABLE tt-job-mat NO-UNDO LIKE job-mat
    FIELD row-id AS ROWID.

DEF TEMP-TABLE tt-s-num NO-UNDO
    FIELD s-num LIKE po-ordl.s-num
    FIELD row-id AS ROWID.

DEF NEW SHARED VAR fil_id AS RECID NO-UNDO.

DEF NEW SHARED TEMP-TABLE item-chg NO-UNDO
    FIELD i-no LIKE job-mat.i-no
    FIELD rec-id AS RECID.

{jcrep/wiptg.i NEW}
{jcrep/wip-tt.i NEW}
{sys/form/r-top3.f}

DEF TEMP-TABLE tt-job UNDO LIKE w-job.
DEF BUFFER bu-tt-rmtags FOR tt-rmtags.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS reprintTag fi_rmtagno fi_toRMTag ~
ed_singletags tg_wip-reported begin_job begin_job2 begin_job_form ~
end_job_form fi_pallet_height fi_exp-sheets-roll fi_sheets-pallet ~
fi_total-wip-tags tb_16ths btn-ok btn-cancel RECT-7 RECT-8 
&Scoped-Define DISPLAYED-OBJECTS fi_UOM-label reprintTag fi_Total-MRP-Qty ~
fi_rmtagno fi_toRMTag fi-single-label ed_singletags tg_wip-reported ~
fi_tag-date fi_tag-hr fi_oh-lf fi_tag-min fi_tag-ampm begin_mach fi_wip-whs ~
fi_wip-bin begin_job begin_job2 begin_job_form end_job_form ~
fi_pallet_height fi_exp-sheets-roll fi_sheets-pallet begin_job_blank ~
fi_total-wip-tags tb_16ths end_job_blank begin_rm-i-no end_rm-i-no 

/* Custom List Definitions                                              */
/* paramFields,triad,List-3,List-4,List-5,F1                            */
&Scoped-define paramFields fi_rmtagno fi_toRMTag fi-single-label begin_job ~
begin_job2 begin_job_form end_job_form tb_16ths 
&Scoped-define List-5 fi_tag-hr fi_tag-min fi_tag-ampm 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD checkWhsBin C-Win 
FUNCTION checkWhsBin RETURNS LOGICAL
  (ipCompany AS CHARACTER,ipLoc AS CHARACTER,ipLocBin AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD removeChars C-Win 
FUNCTION removeChars RETURNS CHARACTER
  (ipField AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fi_tag-ampm AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "AM","PM" 
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE ed_singletags AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 47 BY 3.91 NO-UNDO.

DEFINE VARIABLE begin_job AS CHARACTER FORMAT "X(6)":U 
     LABEL "Job#" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job2 AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job_blank AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "From Blank#" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job_form AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "From Form#" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE begin_mach AS CHARACTER FORMAT "X(6)":U 
     LABEL "First Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_rm-i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "From Board RM Item#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_job_blank AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "To Blank#" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE end_job_form AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "To Form#" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE end_rm-i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "To Board RM Item#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fi-single-label AS CHARACTER FORMAT "X(256)":U INITIAL "Enter RM Tag#s:" 
      VIEW-AS TEXT 
     SIZE 17 BY .62 NO-UNDO.

DEFINE VARIABLE fi_exp-sheets-roll AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "Expected Sheets" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fi_oh-lf AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 11 BY .62 NO-UNDO.

DEFINE VARIABLE fi_pallet_height AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 50 
     LABEL "Pallet Height" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fi_rmtagno AS CHARACTER FORMAT "X(256)":U 
     LABEL "From RM Tag#" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sheets-pallet AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "Sheets/Pallet" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fi_tag-date AS DATE FORMAT "99/99/9999":U 
     LABEL "Tag Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_tag-hr AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Tag Time" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE fi_tag-min AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE fi_toRMTag AS CHARACTER FORMAT "X(256)":U 
     LABEL "To RM Tag#" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE fi_Total-MRP-Qty AS DECIMAL FORMAT ">,>>>,>>9.9<<<<<":U INITIAL 0 
     LABEL "Shts /Job's Forms" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 TOOLTIP "Total MRP Qty for listed forms." NO-UNDO.

DEFINE VARIABLE fi_total-wip-tags AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 1 
     LABEL "Total WIP Tags" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fi_UOM-label AS CHARACTER FORMAT "x(6)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81 TOOLTIP "Consumption UOM" NO-UNDO.

DEFINE VARIABLE fi_wip-bin AS CHARACTER FORMAT "X(256)":U 
     LABEL "WIP Bin" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi_wip-whs AS CHARACTER FORMAT "X(256)":U 
     LABEL "WIP Whs" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE reprintwiptagno AS CHARACTER FORMAT "X(256)":U 
     LABEL "&WIP Tag#" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 104 BY 25.29.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 69.8 BY 4.91.

DEFINE VARIABLE reprintTag AS LOGICAL INITIAL no 
     LABEL "&Reprint Tag" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tb_16ths AS LOGICAL INITIAL no 
     LABEL "Show LWD in 16ths?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tg_wip-reported AS LOGICAL INITIAL NO 
     LABEL "WIP Reported?" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fi_UOM-label AT ROW 2.91 COL 89 COLON-ALIGNED HELP
          "This is the consumption UOM from the RM Item table." NO-LABEL WIDGET-ID 62 NO-TAB-STOP 
     reprintTag AT ROW 1.24 COL 28
     fi_Total-MRP-Qty AT ROW 5.05 COL 79.8 COLON-ALIGNED WIDGET-ID 60 NO-TAB-STOP 
     reprintwiptagno AT ROW 1.24 COL 58.4 COLON-ALIGNED HELP
          "Enter WIP Tag# or Press Help"
     fi_rmtagno AT ROW 2.91 COL 21 COLON-ALIGNED HELP
          "Enter RM Tag# or Press Help" WIDGET-ID 2
     fi_toRMTag AT ROW 4.19 COL 21 COLON-ALIGNED HELP
          "Enter RM Tag# or Press Help" WIDGET-ID 52
     fi-single-label AT ROW 6.81 COL 3.8 COLON-ALIGNED NO-LABEL WIDGET-ID 58
     ed_singletags AT ROW 6.62 COL 23 NO-LABEL WIDGET-ID 56
     tg_wip-reported AT ROW 11.52 COL 24 WIDGET-ID 22
     fi_tag-date AT ROW 12.48 COL 32 COLON-ALIGNED WIDGET-ID 24
     fi_tag-hr AT ROW 12.48 COL 61 COLON-ALIGNED HELP
          "Enter Starting Hour" WIDGET-ID 44
     fi_oh-lf AT ROW 3 COL 76.8 COLON-ALIGNED NO-LABEL WIDGET-ID 12 NO-TAB-STOP 
     fi_tag-min AT ROW 12.48 COL 67 COLON-ALIGNED HELP
          "Enter Starting Minute" WIDGET-ID 46
     fi_tag-ampm AT ROW 12.43 COL 72 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     begin_mach AT ROW 13.48 COL 35.8 COLON-ALIGNED WIDGET-ID 30
     fi_wip-whs AT ROW 14.62 COL 32 COLON-ALIGNED WIDGET-ID 32
     fi_wip-bin AT ROW 14.62 COL 60.8 COLON-ALIGNED WIDGET-ID 34
     begin_job AT ROW 16.81 COL 21.8 COLON-ALIGNED HELP
          "Enter Job Number"
     begin_job2 AT ROW 16.76 COL 36.8 COLON-ALIGNED HELP
          "Enter Job Number"
     begin_job_form AT ROW 17.81 COL 21.8 COLON-ALIGNED HELP
          "Enter From Form#" WIDGET-ID 4
     end_job_form AT ROW 17.81 COL 65 COLON-ALIGNED HELP
          "Enter To Form#" WIDGET-ID 6
     fi_pallet_height AT ROW 20.86 COL 21.8 COLON-ALIGNED HELP
          "Enter Pallet Height" WIDGET-ID 14
     fi_exp-sheets-roll AT ROW 20.86 COL 65 COLON-ALIGNED HELP
          "Enter Expected Sheets/Roll" WIDGET-ID 20
     fi_sheets-pallet AT ROW 22 COL 21.8 COLON-ALIGNED HELP
          "Enter Sheets/Pallet" WIDGET-ID 16
     begin_job_blank AT ROW 18.81 COL 21.8 COLON-ALIGNED HELP
          "Enter From Blank#" WIDGET-ID 8 NO-TAB-STOP 
     fi_total-wip-tags AT ROW 23 COL 22 COLON-ALIGNED HELP
          "Enter Total WIP Tags" WIDGET-ID 18
     tb_16ths AT ROW 22.67 COL 67
     end_job_blank AT ROW 18.81 COL 65 COLON-ALIGNED HELP
          "Enter To Blank#" WIDGET-ID 10 NO-TAB-STOP 
     begin_rm-i-no AT ROW 19.81 COL 21.8 COLON-ALIGNED HELP
          "Enter From Board RM Item#" NO-TAB-STOP 
     btn-ok AT ROW 24.62 COL 23.8
     end_rm-i-no AT ROW 19.81 COL 65 COLON-ALIGNED HELP
          "Enter To Board RM Item#" NO-TAB-STOP 
     btn-cancel AT ROW 24.62 COL 67
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 2
          BGCOLOR 8 
     RECT-7 AT ROW 1.19 COL 1
     RECT-8 AT ROW 11.14 COL 23.2 WIDGET-ID 26
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 104.4 BY 25.57.


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
         TITLE              = "WIP Tag Creation"
         HEIGHT             = 25.14
         WIDTH              = 105
         MAX-HEIGHT         = 27.57
         MAX-WIDTH          = 105
         VIRTUAL-HEIGHT     = 27.57
         VIRTUAL-WIDTH      = 105
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

/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME Custom                                                    */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


/* SETTINGS FOR FILL-IN begin_job IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
       begin_job:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN begin_job2 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
       begin_job2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN begin_job_blank IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       begin_job_blank:READ-ONLY IN FRAME FRAME-A        = TRUE
       begin_job_blank:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN begin_job_form IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR FILL-IN begin_mach IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN begin_rm-i-no IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       begin_rm-i-no:READ-ONLY IN FRAME FRAME-A        = TRUE
       begin_rm-i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN end_job_blank IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       end_job_blank:READ-ONLY IN FRAME FRAME-A        = TRUE.

/* SETTINGS FOR FILL-IN end_job_form IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR FILL-IN end_rm-i-no IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       end_rm-i-no:READ-ONLY IN FRAME FRAME-A        = TRUE
       end_rm-i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN fi-single-label IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
ASSIGN 
       fi-single-label:READ-ONLY IN FRAME FRAME-A        = TRUE.

/* SETTINGS FOR FILL-IN fi_oh-lf IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       fi_oh-lf:READ-ONLY IN FRAME FRAME-A        = TRUE.

/* SETTINGS FOR FILL-IN fi_rmtagno IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR COMBO-BOX fi_tag-ampm IN FRAME FRAME-A
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN fi_tag-date IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_tag-hr IN FRAME FRAME-A
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN fi_tag-min IN FRAME FRAME-A
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN fi_toRMTag IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_Total-MRP-Qty IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       fi_Total-MRP-Qty:READ-ONLY IN FRAME FRAME-A        = TRUE.

/* SETTINGS FOR FILL-IN fi_UOM-label IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       fi_UOM-label:READ-ONLY IN FRAME FRAME-A        = TRUE.

/* SETTINGS FOR FILL-IN fi_wip-bin IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_wip-whs IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN reprintwiptagno IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       reprintwiptagno:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_16ths IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
       tb_16ths:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* WIP Tag Creation */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* WIP Tag Creation */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job C-Win
ON HELP OF begin_job IN FRAME FRAME-A /* Job# */
DO:
  DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.
  DEFINE VARIABLE help-recid AS RECID NO-UNDO.
  run windows/l-openjobs.w (cocode,begin_job:screen-value, 
                            output char-val, OUTPUT help-recid).
  IF NUM-ENTRIES(char-val) > 1 THEN
     DO WITH FRAME {&FRAME-NAME}:
        ASSIGN begin_job:SCREEN-VALUE  = ENTRY(1,char-val)
               begin_job2:SCREEN-VALUE = ENTRY(2,char-val).
        APPLY "entry" TO begin_job2.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job C-Win
ON LEAVE OF begin_job IN FRAME FRAME-A /* Job# */
DO:
   DO WITH FRAME {&FRAME-NAME}:
      begin_job:SCREEN-VALUE = FILL(" ",6 - LENGTH(TRIM(begin_job:SCREEN-VALUE))) +
                                begin_job:SCREEN-VALUE.
      RUN validBeginJob IN THIS-PROCEDURE.
      /*APPLY "VALUE-CHANGED" TO end_job_form.*/
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job2 C-Win
ON LEAVE OF begin_job2 IN FRAME FRAME-A /* - */
DO:
   RUN validBeginJob IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job_form
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job_form C-Win
ON ENTRY OF begin_job_form IN FRAME FRAME-A /* From Form# */
DO:

/*     DEF VAR vd-MRP-qty AS DECIMAL FORMAT ">,>>>,>>9.9<<<<<" NO-UNDO.               */
/*                                                                                    */
/*     FIND FIRST job WHERE                                                           */
/*          job.company EQ cocode AND                                                 */
/*          job.job-no EQ begin_job:SCREEN-VALUE AND /* loadtag.job-no */             */
/*          job.job-no2 EQ integer(begin_job2:SCREEN-VALUE)  /* loadtag.job-no2 */    */
/*          NO-LOCK NO-ERROR.                                                         */
/*                                                                                    */
/*     IF AVAIL job THEN                                                              */
/*     DO:                                                                            */
/*        FOR EACH job-mat FIELDS(blank-no frm qty) WHERE                             */
/*            job-mat.company EQ cocode AND                                           */
/*            job-mat.job EQ job.job AND                                              */
/*            job-mat.job-no EQ job.job-no AND                                        */
/*            job-mat.job-no2 EQ job.job-no2 AND                                      */
/*            job-mat.i-no = begin_rm-i-no:SCREEN-VALUE AND                           */
/*            (job-mat.frm >= integer(begin_job_form:SCREEN-VALUE) AND                */
/*             job-mat.frm <= integer(END_job_form:SCREEN-VALUE))                     */
/*            NO-LOCK:                                                                */
/*                                                                                    */
/*            ASSIGN vd-MRP-qty = vd-MRP-qty + job-mat.qty.                           */
/*        END.                                                                        */
/*     END.                                                                           */
/*     ASSIGN fi_Total-MRP-Qty:SCREEN-VALUE = STRING(vd-MRP-qty,">,>>>,>>9.9<<<<<").  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job_form C-Win
ON VALUE-CHANGED OF begin_job_form IN FRAME FRAME-A /* From Form# */
DO:

  DEF VAR vd-MRP-qty AS DECIMAL FORMAT ">,>>>,>>9.9<<<<<" NO-UNDO.

  FIND FIRST job WHERE
       job.company EQ cocode AND
       job.job-no EQ begin_job:SCREEN-VALUE AND /* loadtag.job-no */
       job.job-no2 EQ integer(begin_job2:SCREEN-VALUE)  /* loadtag.job-no2 */
       NO-LOCK NO-ERROR.

  IF AVAIL job THEN
  DO:
     FIND FIRST job-mat  WHERE
         job-mat.company EQ cocode AND
         job-mat.job EQ job.job AND
         job-mat.job-no EQ job.job-no AND
         job-mat.job-no2 EQ job.job-no2 AND
         job-mat.i-no = begin_rm-i-no:SCREEN-VALUE AND
         (job-mat.frm >= integer(begin_job_form:SCREEN-VALUE) AND
          job-mat.frm <= integer(END_job_form:SCREEN-VALUE))
         NO-LOCK NO-ERROR.
         IF AVAIL job-mat THEN
         ASSIGN vd-MRP-qty = vd-MRP-qty + job-mat.qty.
  END.
  ASSIGN fi_Total-MRP-Qty:SCREEN-VALUE = STRING(vd-MRP-qty,">,>>>,>>9.9<<<<<").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mach C-Win
ON LEAVE OF begin_mach IN FRAME FRAME-A /* First Machine */
DO:
   DO WITH FRAME {&FRAME-NAME}:

      ASSIGN begin_mach.

      IF begin_mach NE "" AND
         NOT can-find(FIRST mach where
         mach.company EQ cocode and
         mach.loc EQ locode AND
         mach.m-code EQ begin_mach) THEN
         DO:
            MESSAGE "Invalid Machine."
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY "ENTRY" TO begin_mach.
            RETURN NO-APPLY.
         END.
      END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
   EMPTY TEMP-TABLE tt-rmtags NO-ERROR.
   EMPTY TEMP-TABLE tt-used-tags NO-ERROR.
   apply "close" to this-procedure.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:07 am */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  RUN ok-button.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:07 am */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ed_singletags
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ed_singletags C-Win
ON HELP OF ed_singletags IN FRAME FRAME-A
DO:

    DEFINE VARIABLE opCharVal AS CHARACTER NO-UNDO.
    DEFINE VARIABLE opRecID AS RECID NO-UNDO.

    RUN addon/windows/l-ldtaga.w (cocode, YES, no, FOCUS:SCREEN-VALUE, OUTPUT opCharVal, OUTPUT opRecID).

    IF opCharVal NE '' AND SELF:SCREEN-VALUE = "" THEN
        ASSIGN SELF:SCREEN-VALUE = ENTRY(1,opCharVal).
    ELSE
        ASSIGN SELF:SCREEN-VALUE = SELF:SCREEN-VALUE + "," + ENTRY(1,opCharVal).
    APPLY 'leave':U TO SELF.
    APPLY 'value-changed' TO ed_singletags.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ed_singletags C-Win
ON LEAVE OF ed_singletags IN FRAME FRAME-A
DO:
    DEF VAR op-error AS LOG NO-UNDO.
    DEF VAR vicount AS INT INIT 0 NO-UNDO.
    DEF VAR vc-loadtag AS CHAR FORMAT "x(20)" INIT "" NO-UNDO.

    IF SELF:SCREEN-VALUE NE "" AND SELF:MODIFIED THEN DO:
        DO vicount = 1 TO NUM-ENTRIES(ed_singletags:SCREEN-VALUE):
            IF NOT CAN-FIND(FIRST tt-rmtags 
                            WHERE tt-rmtags.rmtagno = 
                                    ENTRY(vicount,ed_singletags:SCREEN-VALUE))
            THEN DO:
                CREATE tt-rmtags.
                ASSIGN tt-rmtags.rmtagno = ENTRY(vicount,ed_singletags:SCREEN-VALUE)
                       vc-loadtag = ENTRY(vicount,ed_singletags:SCREEN-VALUE).
            END.
        END.

      RUN validRMLoadtag(OUTPUT op-error).
      IF op-error THEN RETURN NO-APPLY.
      RUN get-consumption-uom (INPUT vc-loadtag).
      APPLY "entry" TO FRAME {&FRAME-NAME}.
    END.

    IF fi_rmtagno:SCREEN-VALUE = "" AND
       fi_toRMTag:SCREEN-VALUE = "" AND
       ed_singletags:SCREEN-VALUE = "" THEN
        ASSIGN fi_sheets-pallet:SCREEN-VALUE = ""
               fi_total-wip-tags:SCREEN-VALUE = ""
               fi_exp-sheets-roll:SCREEN-VALUE = ""
               begin_job_form:SCREEN-VALUE = "0"
               END_job_form:SCREEN-VALUE = "0"
               fi_UOM-label:SCREEN-VALUE = "".
    APPLY "leave" TO fi_UOM-label.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ed_singletags C-Win
ON VALUE-CHANGED OF ed_singletags IN FRAME FRAME-A
DO:

    DEF VAR op-error AS LOG NO-UNDO.
    DEF VAR vicount AS INT INIT 0 NO-UNDO.
    DEF VAR vc-loadtag AS CHAR FORMAT "x(20)" INIT "" NO-UNDO.

    IF SELF:SCREEN-VALUE NE "" AND SELF:MODIFIED THEN DO:
        EMPTY TEMP-TABLE tt-rmtags NO-ERROR.

        IF fi_rmtagno:SCREEN-VALUE <> "" AND fi_toRMTag:SCREEN-VALUE <> "" THEN DO:

            FOR EACH loadtag NO-LOCK 
                WHERE loadtag.company = cocode AND
                      loadtag.item-type = TRUE AND
                      (loadtag.tag-no >= fi_rmtagno:SCREEN-VALUE AND
                       loadtag.tag-no <= fi_toRMTag:SCREEN-VALUE):
                IF NOT CAN-FIND(FIRST tt-rmtags 
                                WHERE tt-rmtags.rmtagno =  loadtag.tag-no)
                THEN DO:
                    CREATE tt-rmtags.
                    ASSIGN tt-rmtags.rmtagno = loadtag.tag-no.
                END.                        
            END.
        END.

        DO vicount = 1 TO NUM-ENTRIES(ed_singletags:SCREEN-VALUE):
            IF NOT CAN-FIND(FIRST tt-rmtags 
                            WHERE tt-rmtags.rmtagno = 
                                    ENTRY(vicount,ed_singletags:SCREEN-VALUE))
                AND LENGTH(ENTRY(vicount,ed_singletags:SCREEN-VALUE)) >= 17
            THEN DO:
                CREATE tt-rmtags.
                ASSIGN tt-rmtags.rmtagno = ENTRY(vicount,ed_singletags:SCREEN-VALUE)
                       vc-loadtag = ENTRY(vicount,ed_singletags:SCREEN-VALUE).
            END.
            ELSE IF LENGTH(ENTRY(vicount,ed_singletags:SCREEN-VALUE)) < 17 THEN
                RETURN NO-APPLY.
        END.

      RUN validRMLoadtag(OUTPUT op-error).
      IF op-error THEN APPLY "Entry" TO ed_singletags.
      RUN get-consumption-uom (INPUT vc-loadtag).
/*       APPLY "entry" TO ed_singletags.  */
/*       APPLY "entry" TO FRAME {&FRAME-NAME}.  */
    END.

    IF fi_rmtagno:SCREEN-VALUE = "" AND
       fi_toRMTag:SCREEN-VALUE = "" AND
       ed_singletags:SCREEN-VALUE = "" THEN
        ASSIGN fi_sheets-pallet:SCREEN-VALUE = ""
               fi_total-wip-tags:SCREEN-VALUE = ""
               fi_exp-sheets-roll:SCREEN-VALUE = ""
               begin_job_form:SCREEN-VALUE = "0"
               END_job_form:SCREEN-VALUE = "0".
    APPLY "leave" TO fi_UOM-label.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job_form
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job_form C-Win
ON LEAVE OF end_job_form IN FRAME FRAME-A /* To Form# */
DO:

    DEF VAR vd-MRP-qty AS DECIMAL FORMAT ">,>>>,>>9.9<<<<<" NO-UNDO.

    FIND FIRST job WHERE
         job.company EQ cocode AND
         job.job-no EQ begin_job:SCREEN-VALUE AND /* loadtag.job-no */
         job.job-no2 EQ integer(begin_job2:SCREEN-VALUE)  /* loadtag.job-no2 */
         NO-LOCK NO-ERROR.

    IF AVAIL job THEN
    DO:
       FIND FIRST job-mat  WHERE
           job-mat.company EQ cocode AND
           job-mat.job EQ job.job AND
           job-mat.job-no EQ job.job-no AND
           job-mat.job-no2 EQ job.job-no2 AND
           job-mat.i-no = begin_rm-i-no:SCREEN-VALUE AND
           (job-mat.frm >= integer(begin_job_form:SCREEN-VALUE) AND
            job-mat.frm <= integer(END_job_form:SCREEN-VALUE))
           NO-LOCK NO-ERROR.
           IF AVAIL job-mat THEN
           ASSIGN vd-MRP-qty = vd-MRP-qty + job-mat.qty.

    END.
    ASSIGN fi_Total-MRP-Qty:SCREEN-VALUE = STRING(vd-MRP-qty,">,>>>,>>9.9<<<<<").
    APPLY "leave" TO fi_oh-lf.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job_form C-Win
ON VALUE-CHANGED OF end_job_form IN FRAME FRAME-A /* To Form# */
DO:

    DEF VAR vd-MRP-qty AS DECIMAL FORMAT ">,>>>,>>9.9<<<<<" NO-UNDO.

    FIND FIRST job WHERE
         job.company EQ cocode AND
         job.job-no EQ begin_job:SCREEN-VALUE AND /* loadtag.job-no */
         job.job-no2 EQ integer(begin_job2:SCREEN-VALUE)  /* loadtag.job-no2 */
         NO-LOCK NO-ERROR.

    IF AVAIL job THEN
    DO:
       FIND FIRST job-mat  WHERE
           job-mat.company EQ cocode AND
           job-mat.job EQ job.job AND
           job-mat.job-no EQ job.job-no AND
           job-mat.job-no2 EQ job.job-no2 AND
           job-mat.i-no = begin_rm-i-no:SCREEN-VALUE AND
           (job-mat.frm >= integer(begin_job_form:SCREEN-VALUE) AND
            job-mat.frm <= integer(END_job_form:SCREEN-VALUE))
           NO-LOCK NO-ERROR.
           IF AVAIL job-mat THEN
           ASSIGN vd-MRP-qty = vd-MRP-qty + job-mat.qty.

    END.
    ASSIGN fi_Total-MRP-Qty:SCREEN-VALUE = STRING(vd-MRP-qty,">,>>>,>>9.9<<<<<").
    APPLY "leave" TO fi_oh-lf.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_oh-lf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_oh-lf C-Win
ON LEAVE OF fi_oh-lf IN FRAME FRAME-A
DO:
   DEF VAR v-exp-sheets AS DEC NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:

      ASSIGN fi_oh-lf.

      IF AVAIL loadtag THEN
      DO:
         IF loadtag.job-no <> "" THEN
             FIND FIRST job WHERE
                  job.company EQ cocode AND
                  job.job-no EQ loadtag.job-no AND
                  job.job-no2 EQ loadtag.job-no2
                  NO-LOCK NO-ERROR.
         ELSE
             FIND FIRST job WHERE
                  job.company EQ cocode AND
                  job.job-no EQ begin_job:SCREEN-VALUE AND
                  job.job-no2 EQ INTEGER(begin_job2:SCREEN-VALUE)
                  NO-LOCK NO-ERROR.

         IF AVAIL job THEN
         DO:
            FIND FIRST job-mat WHERE
                 job-mat.company EQ cocode AND
                 job-mat.job EQ job.job AND
                 job-mat.job-no EQ job.job-no AND
                 job-mat.job-no2 EQ job.job-no2 AND
                 job-mat.i-no EQ loadtag.i-no
                 NO-LOCK NO-ERROR.

            IF AVAIL job-mat THEN
            DO:
               IF job-mat.len NE 0 THEN DO:
                  FOR EACH tt-rmtags:
                      IF vl-InputParameters THEN
                          ASSIGN tt-rmtags.origshtqty = (ipd-qty * 12 
                                                           / job-mat.len) + .49
                                 tt-rmtags.availshtqty = tt-rmtags.origshtqty.
                      ELSE
                          ASSIGN tt-rmtags.origshtqty = ( tt-rmtags.lfqty * 12 
                                                           / job-mat.len) + .49
                                 tt-rmtags.availshtqty = tt-rmtags.origshtqty.
                  END.

                  IF vl-InputParameters THEN
                     v-exp-sheets = ipd-qty * 12 / job-mat.len.
                  ELSE
                     v-exp-sheets = INT(fi_oh-lf:SCREEN-VALUE) * 12 
                                        / job-mat.len.
               END.

               {sys\inc\roundup.i v-exp-sheets}

               fi_exp-sheets-roll:SCREEN-VALUE = STRING(v-exp-sheets).

               RELEASE job-mat.

               APPLY "LEAVE" TO fi_pallet_height IN FRAME {&FRAME-NAME}.
            END.

            RELEASE job.
         END.
         IF fi_oh-lf:SCREEN-VALUE = "" THEN
            fi_UOM-label:SCREEN-VALUE = "".
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_pallet_height
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_pallet_height C-Win
ON LEAVE OF fi_pallet_height IN FRAME FRAME-A /* Pallet Height */
DO:
   DEF VAR v-wip-tags AS DEC NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:

      ASSIGN
         fi_pallet_height
         begin_rm-i-no
         fi_exp-sheets-roll.

      FIND FIRST ITEM WHERE
           ITEM.company EQ cocode AND
           ITEM.i-no EQ begin_rm-i-no:SCREEN-VALUE
           NO-LOCK NO-ERROR.

      IF AVAIL ITEM AND fi_pallet_height > 0 THEN
      DO:
         IF ITEM.cal NE 0 THEN
            fi_sheets-pallet:SCREEN-VALUE = STRING((fi_pallet_height - 5) / ITEM.cal).
         ELSE
            fi_sheets-pallet:SCREEN-VALUE = "0".

         ASSIGN fi_sheets-pallet.

         IF fi_sheets-pallet NE 0 THEN
         DO:
            v-wip-tags = fi_exp-sheets-roll / fi_sheets-pallet.

            {sys/inc/roundup.i v-wip-tags}

            fi_total-wip-tags:SCREEN-VALUE = STRING(v-wip-tags).
         END.
         ELSE
            fi_total-wip-tags:SCREEN-VALUE = "0".

         RELEASE ITEM.
      END.
      ELSE
          ASSIGN fi_total-wip-tags:SCREEN-VALUE = "0"
                 fi_sheets-pallet:SCREEN-VALUE = "0".
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_rmtagno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_rmtagno C-Win
ON HELP OF fi_rmtagno IN FRAME FRAME-A /* Fr RM Tag# */
DO:
  DEFINE VARIABLE opCharVal AS CHARACTER NO-UNDO.
  DEFINE VARIABLE opRecID AS RECID NO-UNDO.

  RUN addon/windows/l-ldtaga.w (cocode, YES, no, FOCUS:SCREEN-VALUE, OUTPUT opCharVal, OUTPUT opRecID).

  IF opCharVal NE '' THEN
  SELF:SCREEN-VALUE = ENTRY(1,opCharVal).
  APPLY 'leave':U TO SELF.
  APPLY 'value-changed' TO fi_rmtagno.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_rmtagno C-Win
ON LEAVE OF fi_rmtagno IN FRAME FRAME-A /* Fr RM Tag# */
DO:
  DEF VAR op-error AS LOG NO-UNDO.


  IF LENGTH(fi_rmtagno:SCREEN-VALUE) < 17 and
     fi_rmtagno:SCREEN-VALUE <> "" THEN RETURN NO-APPLY.

  IF fi_rmtagno:SCREEN-VALUE = "" AND
     fi_toRMTag:SCREEN-VALUE = "" AND
     ed_singletags:SCREEN-VALUE = "" THEN
      ASSIGN fi_sheets-pallet:SCREEN-VALUE = ""
             fi_total-wip-tags:SCREEN-VALUE = ""
             fi_exp-sheets-roll:SCREEN-VALUE = ""
             begin_job_form:SCREEN-VALUE = "0"
             END_job_form:SCREEN-VALUE = "0"
             fi_UOM-label:SCREEN-VALUE = "".
  APPLY "leave" TO fi_UOM-label.

  EMPTY TEMP-TABLE tt-rmtags NO-ERROR.

  IF fi_rmtagno:SCREEN-VALUE NE "" AND NOT CAN-FIND(FIRST tt-rmtags 
                  WHERE tt-rmtags.rmtagno = fi_rmtagno:SCREEN-VALUE)
  THEN DO:
      CREATE tt-rmtags.
      ASSIGN tt-rmtags.rmtagno = SELF:SCREEN-VALUE.
  END.
  IF SELF:MODIFIED THEN DO:
      RUN validRMLoadtag(OUTPUT op-error).
      IF op-error THEN RETURN NO-APPLY.
  END.
  RUN get-consumption-uom (INPUT fi_rmtagno:SCREEN-VALUE).
  APPLY "entry" TO FRAME {&FRAME-NAME}.
  ASSIGN fi_toRMTag:SCREEN-VALUE = fi_rmtagno:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_rmtagno C-Win
ON VALUE-CHANGED OF fi_rmtagno IN FRAME FRAME-A /* Fr RM Tag# */
DO:
    DEF VAR op-error AS LOG NO-UNDO.
    DEF VAR vicount AS INT INIT 0 NO-UNDO.
      IF LENGTH(fi_rmtagno:SCREEN-VALUE) < 17 and
         fi_rmtagno:SCREEN-VALUE <> ""  THEN RETURN NO-APPLY.

      IF fi_rmtagno:SCREEN-VALUE = "" AND
         fi_toRMTag:SCREEN-VALUE = "" AND
         ed_singletags:SCREEN-VALUE = "" THEN
          ASSIGN fi_sheets-pallet:SCREEN-VALUE = ""
                 fi_total-wip-tags:SCREEN-VALUE = ""
                 fi_exp-sheets-roll:SCREEN-VALUE = ""
                 begin_job_form:SCREEN-VALUE = "0"
                 END_job_form:SCREEN-VALUE = "0"
                 fi_UOM-label:SCREEN-VALUE = "".
      APPLY "leave" TO fi_UOM-label.

      EMPTY TEMP-TABLE tt-rmtags NO-ERROR.

      IF ed_singletags:SCREEN-VALUE <> "" THEN
        DO vicount = 1 TO NUM-ENTRIES(ed_singletags:SCREEN-VALUE):
            IF NOT CAN-FIND(FIRST tt-rmtags 
                            WHERE tt-rmtags.rmtagno = 
                                    ENTRY(vicount,ed_singletags:SCREEN-VALUE))
                AND LENGTH(ENTRY(vicount,ed_singletags:SCREEN-VALUE)) >= 17
            THEN DO:
                CREATE tt-rmtags.
                ASSIGN tt-rmtags.rmtagno = ENTRY(vicount,ed_singletags:SCREEN-VALUE).
            END.
        END.

      IF fi_rmtagno:SCREEN-VALUE NE "" AND NOT CAN-FIND(FIRST tt-rmtags 
                      WHERE tt-rmtags.rmtagno = fi_rmtagno:SCREEN-VALUE)
      THEN DO:
          CREATE tt-rmtags.
          ASSIGN tt-rmtags.rmtagno = SELF:SCREEN-VALUE.
      END.
      RUN validRMLoadtag(OUTPUT op-error).
      IF op-error THEN RETURN NO-APPLY.
      RUN get-consumption-uom (INPUT fi_rmtagno:SCREEN-VALUE).
      APPLY "entry" TO FRAME {&FRAME-NAME}.
      ASSIGN fi_toRMTag:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_sheets-pallet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_sheets-pallet C-Win
ON LEAVE OF fi_sheets-pallet IN FRAME FRAME-A /* Sheets/Pallet */
DO:
   DEF VAR v-wip-tags AS DEC NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:

      ASSIGN fi_sheets-pallet
             fi_exp-sheets-roll.

      IF fi_sheets-pallet NE 0 THEN
      DO:
         v-wip-tags = fi_exp-sheets-roll / fi_sheets-pallet.

         {sys/inc/roundup.i v-wip-tags}

         fi_total-wip-tags:SCREEN-VALUE = STRING(v-wip-tags).
      END.
      ELSE
         fi_total-wip-tags:SCREEN-VALUE = "0".
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_tag-hr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_tag-hr C-Win
ON LEAVE OF fi_tag-hr IN FRAME FRAME-A /* Tag Time */
DO:
  IF LASTKEY = -1 THEN RETURN.
  RUN valid-hour(SELF:SCREEN-VALUE).
  IF RETURN-VALUE <> "" THEN DO:
     MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_tag-min
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_tag-min C-Win
ON LEAVE OF fi_tag-min IN FRAME FRAME-A
DO:
    IF LASTKEY = -1 THEN RETURN.

  RUN valid-min(SELF:SCREEN-VALUE).
  IF RETURN-VALUE <> "" THEN DO:
     MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
     RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_toRMTag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_toRMTag C-Win
ON HELP OF fi_toRMTag IN FRAME FRAME-A /* To RM Tag# */
DO:

    DEFINE VARIABLE opCharVal AS CHARACTER NO-UNDO.
    DEFINE VARIABLE opRecID AS RECID NO-UNDO.

    RUN addon/windows/l-ldtaga.w (cocode, YES, no, FOCUS:SCREEN-VALUE, OUTPUT opCharVal, OUTPUT opRecID).

    IF opCharVal NE '' THEN
    SELF:SCREEN-VALUE = ENTRY(1,opCharVal).
    APPLY 'leave':U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_toRMTag C-Win
ON LEAVE OF fi_toRMTag IN FRAME FRAME-A /* To RM Tag# */
DO:

    DEF VAR op-error AS LOG NO-UNDO.


    IF LENGTH(fi_toRMTag:SCREEN-VALUE) < 17 AND 
       fi_toRMTag:SCREEN-VALUE <> "" THEN RETURN NO-APPLY.

    IF fi_rmtagno:SCREEN-VALUE = "" AND
       fi_toRMTag:SCREEN-VALUE = "" AND
       ed_singletags:SCREEN-VALUE = "" THEN
       ASSIGN fi_sheets-pallet:SCREEN-VALUE = ""
              fi_total-wip-tags:SCREEN-VALUE = ""
              fi_exp-sheets-roll:SCREEN-VALUE = ""
              begin_job_form:SCREEN-VALUE = "0"
              END_job_form:SCREEN-VALUE = "0"
              fi_UOM-label:SCREEN-VALUE = "".
    APPLY "leave" TO fi_UOM-label.

    IF fi_toRMTag:SCREEN-VALUE NE "" AND NOT CAN-FIND(FIRST tt-rmtags 
                    WHERE tt-rmtags.rmtagno = fi_toRMTag:SCREEN-VALUE)
    THEN DO:
        CREATE tt-rmtags.
        ASSIGN tt-rmtags.rmtagno = SELF:SCREEN-VALUE.
    END.

    RUN validRMLoadtag(OUTPUT op-error).
    IF op-error THEN RETURN NO-APPLY.
    RUN get-consumption-uom (INPUT fi_toRMTag:SCREEN-VALUE).
    APPLY "entry" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_toRMTag C-Win
ON VALUE-CHANGED OF fi_toRMTag IN FRAME FRAME-A /* To RM Tag# */
DO:

    DEF VAR op-error AS LOG NO-UNDO.
    DEF VAR vicount AS INT INIT 0 NO-UNDO.


      IF LENGTH(fi_toRMTag:SCREEN-VALUE) < 17 THEN RETURN NO-APPLY.

      IF fi_rmtagno:SCREEN-VALUE = "" AND
         fi_toRMTag:SCREEN-VALUE = "" AND
         ed_singletags:SCREEN-VALUE = "" THEN
          ASSIGN fi_sheets-pallet:SCREEN-VALUE = ""
                 fi_total-wip-tags:SCREEN-VALUE = ""
                 fi_exp-sheets-roll:SCREEN-VALUE = ""
                 begin_job_form:SCREEN-VALUE = "0"
                 END_job_form:SCREEN-VALUE = "0"
                 fi_UOM-label:SCREEN-VALUE = "".
      APPLY "leave" TO fi_UOM-label.

      EMPTY TEMP-TABLE tt-rmtags NO-ERROR.

      IF ed_singletags:SCREEN-VALUE <> "" THEN
        DO vicount = 1 TO NUM-ENTRIES(ed_singletags:SCREEN-VALUE):
            IF NOT CAN-FIND(FIRST tt-rmtags 
                            WHERE tt-rmtags.rmtagno = 
                                    ENTRY(vicount,ed_singletags:SCREEN-VALUE))
                AND LENGTH(ENTRY(vicount,ed_singletags:SCREEN-VALUE)) >= 17
            THEN DO:
                CREATE tt-rmtags.
                ASSIGN tt-rmtags.rmtagno = ENTRY(vicount,ed_singletags:SCREEN-VALUE).
            END.
        END.

      FOR EACH loadtag NO-LOCK 
          WHERE loadtag.company = cocode AND
                loadtag.item-type = TRUE AND
                (loadtag.tag-no >= fi_rmtagno:SCREEN-VALUE AND
                 loadtag.tag-no <= fi_toRMTag:SCREEN-VALUE):
          IF NOT CAN-FIND(FIRST tt-rmtags 
                          WHERE tt-rmtags.rmtagno =  loadtag.tag-no)
          THEN DO:
              CREATE tt-rmtags.
              ASSIGN tt-rmtags.rmtagno = loadtag.tag-no.
          END.                        
      END.

      RUN validRMLoadtag(OUTPUT op-error).
      IF op-error THEN RETURN NO-APPLY.
      RUN get-consumption-uom (INPUT fi_toRMTag:SCREEN-VALUE).
      APPLY "entry" TO FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_UOM-label
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_UOM-label C-Win
ON LEAVE OF fi_UOM-label IN FRAME FRAME-A
DO:
  IF fi_oh-lf:SCREEN-VALUE = "0" OR 
     fi_oh-lf:SCREEN-VALUE = ""
      THEN ASSIGN fi_uom-label:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_wip-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_wip-bin C-Win
ON HELP OF fi_wip-bin IN FRAME FRAME-A /* WIP Bin */
DO:
   DEF VAR char-val AS CHAR NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:
      run rm/l-wipbin.w (cocode,fi_wip-whs:SCREEN-VALUE, output char-val).
      if char-val <> "" then
         fi_wip-bin:screen-value = entry(1,char-val).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_wip-bin C-Win
ON LEAVE OF fi_wip-bin IN FRAME FRAME-A /* WIP Bin */
DO:
   DO WITH FRAME {&FRAME-NAME}:

      ASSIGN
         fi_wip-whs fi_wip-bin.

      IF fi_wip-bin NE "" THEN
      DO:
         IF NOT CAN-FIND(FIRST wip-bin WHERE
            wip-bin.company EQ cocode AND
            wip-bin.loc     EQ fi_wip-whs AND
            wip-bin.loc-bin EQ fi_wip-bin) THEN
            DO:
               MESSAGE "Invalid WIP Bin."
                   VIEW-AS ALERT-BOX ERROR BUTTONS OK.
               APPLY "entry" TO fi_wip-bin.
               RETURN NO-APPLY.
            END.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_wip-whs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_wip-whs C-Win
ON HELP OF fi_wip-whs IN FRAME FRAME-A /* WIP Whs */
DO:
   DEF VAR char-val AS CHAR NO-UNDO.
   DO WITH FRAME {&FRAME-NAME}:
      run rm/l-loc.w (cocode,fi_wip-whs:screen-value, output char-val).
      if char-val <> "" THEN
         fi_wip-whs:screen-value = entry(1,char-val).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_wip-whs C-Win
ON LEAVE OF fi_wip-whs IN FRAME FRAME-A /* WIP Whs */
DO:
   DO WITH FRAME {&FRAME-NAME}:

   ASSIGN fi_wip-whs.

   IF fi_wip-whs NE "" AND
      NOT CAN-FIND(FIRST loc WHERE
      loc.company EQ cocode AND
      loc.loc     EQ fi_wip-whs) THEN
      DO:
         MESSAGE "Invalid WIP Warehouse."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "entry" TO fi_wip-whs.
         RETURN NO-APPLY.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME reprintTag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL reprintTag C-Win
ON VALUE-CHANGED OF reprintTag IN FRAME FRAME-A /* Reprint Tag */
DO:
  ASSIGN {&SELF-NAME}.
  IF {&SELF-NAME} THEN DO WITH FRAME {&FRAME-NAME}:
    DISABLE {&paramFields}.
    ENABLE reprintwiptagno.
    ASSIGN
       fi_tag-date:SENSITIVE = YES
       fi_tag-hr:SENSITIVE = YES
       fi_tag-min:SENSITIVE = YES
       fi_tag-ampm:SENSITIVE = YES
       begin_mach:SENSITIVE = YES
       fi_wip-whs:SENSITIVE = YES
       fi_wip-bin:SENSITIVE = YES.

    APPLY 'ENTRY':U TO reprintwiptagno.
  END.
  ELSE DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      reprintwiptagno:SCREEN-VALUE = ''
      reprintwiptagno:HIDDEN = YES.

    ENABLE {&paramFields}.

    ASSIGN
       fi_tag-date:SENSITIVE = NO
       fi_tag-hr:SENSITIVE = NO
       fi_tag-min:SENSITIVE = NO
       fi_tag-ampm:SENSITIVE = NO
       begin_mach:SENSITIVE = NO
       fi_wip-whs:SENSITIVE = NO
       fi_wip-bin:SENSITIVE = NO.

    IF v-loadtag NE "TRIAD" THEN
    DISABLE {&triad}.
    APPLY 'ENTRY':U TO fi_rmtagno.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME reprintwiptagno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL reprintwiptagno C-Win
ON HELP OF reprintwiptagno IN FRAME FRAME-A /* WIP Tag# */
DO:
  DEFINE VARIABLE opCharVal AS CHARACTER NO-UNDO.
  DEFINE VARIABLE opRecID AS RECID NO-UNDO.

  RUN windows/l-wptag1.w (cocode,'',OUTPUT opCharVal,OUTPUT opRecID).
  IF opCharVal NE '' THEN
  SELF:SCREEN-VALUE = ENTRY(1,opCharVal).
  APPLY 'leave':U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL reprintwiptagno C-Win
ON LEAVE OF reprintwiptagno IN FRAME FRAME-A /* WIP Tag# */
DO:
  DEF VAR lv-error AS LOG NO-UNDO.

  IF SELF:SCREEN-VALUE NE "" AND SELF:MODIFIED THEN DO:
     RUN validWIPTag(OUTPUT lv-error).
     IF lv-error THEN RETURN NO-APPLY.
     APPLY "ENTRY" TO btn-ok IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_16ths
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_16ths C-Win
ON VALUE-CHANGED OF tb_16ths IN FRAME FRAME-A /* Show LWD in 16ths? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg_wip-reported
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg_wip-reported C-Win
ON VALUE-CHANGED OF tg_wip-reported IN FRAME FRAME-A /* WIP Reported? */
DO:
   DO WITH FRAME {&FRAME-NAME}:
      ASSIGN tg_wip-reported
             fi_tag-date:SENSITIVE = tg_wip-reported
             fi_tag-hr:SENSITIVE = tg_wip-reported
             fi_tag-min:SENSITIVE = tg_wip-reported
             fi_tag-ampm:SENSITIVE = tg_wip-reported
             begin_mach:SENSITIVE = tg_wip-reported
             fi_wip-whs:SENSITIVE = tg_wip-reported
             fi_wip-bin:SENSITIVE = tg_wip-reported.

      IF tg_wip-reported THEN
         RUN wip-reported-proc.
      ELSE
         ASSIGN
            fi_tag-date:SCREEN-VALUE = ?
            fi_tag-hr:SCREEN-VALUE = "00"
            fi_tag-min:SCREEN-VALUE = "00"
            fi_tag-ampm:SCREEN-VALUE = "AM"
            begin_mach:SCREEN-VALUE = ""
            fi_wip-whs:SCREEN-VALUE = ""
            fi_wip-bin:SCREEN-VALUE = "".
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


PROCEDURE WinExec EXTERNAL "KERNEL32.DLL":
     DEFINE INPUT  PARAMETER ProgramName AS CHARACTER.
     DEFINE INPUT  PARAMETER VisualStyle AS LONG.
     DEFINE RETURN PARAMETER StatusCode  AS LONG.
END PROCEDURE.



/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:07 am */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

/* security check need {methods/prgsecur.i} in definition section */

  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

  EMPTY TEMP-TABLE tt-rmtags NO-ERROR.

  ASSIGN vl-InputParameters = IF INDEX(PROGRAM-NAME(2),"b-issued") > 0 THEN TRUE
                              ELSE FALSE.

  FIND FIRST company WHERE company.company EQ gcompany NO-LOCK.

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ gcompany
        AND sys-ctrl.name    EQ "CEMENU"
      NO-LOCK NO-ERROR.
  tb_16ths  = AVAIL sys-ctrl AND sys-ctrl.char-fld EQ "Corrware".

  FIND FIRST sys-ctrl NO-LOCK
      WHERE sys-ctrl.company EQ gcompany
        AND sys-ctrl.name    EQ "LOADTAG" NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company  = gcompany
     sys-ctrl.name     = "LOADTAG"
     sys-ctrl.descrip  = "Special Load tag print options, e.g. barcode printer"
     sys-ctrl.char-fld = "ASI".
    MESSAGE "System control record NOT found. Please enter the load tag option"
            UPDATE sys-ctrl.char-fld.
    FIND CURRENT sys-ctrl NO-LOCK.
  END.
  ASSIGN
   v-loadtag = sys-ctrl.char-fld
   v-cas-lab = sys-ctrl.log-fld
   v-tags    = sys-ctrl.dec-fld.

  FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ gcompany
      AND sys-ctrl.name    EQ "RMTAGS" NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company  = gcompany
     sys-ctrl.name     = "RMTAGS"
     sys-ctrl.descrip  = "Integer # of RM Loadtags to Print?  Logical to Calculate # of Tags?".
    MESSAGE "System control record NOT found. Number of RM LoadTags to Print?"
            UPDATE sys-ctrl.int-fld.
    MESSAGE "System control record NOT found. Auto Calculate RM LoadTags to Print?"
            UPDATE sys-ctrl.log-fld.
    FIND CURRENT sys-ctrl NO-LOCK.
  END.

  ASSIGN
    v-mult = sys-ctrl.int-fld
    v-calc = sys-ctrl.log-fld.
  IF v-mult LE 0 THEN v-mult = 1.

  FIND FIRST sys-ctrl NO-LOCK
  WHERE sys-ctrl.company EQ gcompany
    AND sys-ctrl.name    EQ "RMWHSBIN" NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company  = gcompany
     sys-ctrl.name     = "RMWHSBIN"
     sys-ctrl.descrip  = "Default Location for RM Warehouse / Bin?"
     sys-ctrl.char-fld = "RMITEM".
    FIND CURRENT sys-ctrl NO-LOCK.
  END.
  v-bin = sys-ctrl.char-fld.

   FIND FIRST sys-ctrl NO-LOCK
  WHERE sys-ctrl.company EQ gcompany
    AND sys-ctrl.name    EQ "WIPWHSBIN" NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company  = gcompany
     sys-ctrl.name     = "WIPWHSBIN"
     sys-ctrl.descrip  = "Default Location for WIP Warehouse / Bin?"
     sys-ctrl.char-fld = "".
    FIND CURRENT sys-ctrl NO-LOCK.
  END.

  IF sys-ctrl.char-fld NE "" THEN
     ASSIGN
        v-wipbin-whs = SUBSTRING(sys-ctrl.char-fld,1,INDEX(sys-ctrl.char-fld," ") - 1)
        v-wipbin-bin = SUBSTRING(sys-ctrl.char-fld,INDEX(sys-ctrl.char-fld," ") + 1).

  DO WITH FRAME {&FRAME-NAME}:

    RUN local-enable_UI.

    IF v-loadtag NE "TRIAD" THEN DISABLE {&triad}.

    {methods/nowait.i}

    IF vl-InputParameters THEN DO:
        FOR EACH wiptag NO-LOCK WHERE wiptag.company = g_company AND
                                      wiptag.job-no = ipc-job-no AND
                                      wiptag.job-no2 = ipi-job-no2 AND
                                      wiptag.rm-i-no = ipc-i-no AND
                                      wiptag.form-no = ipi-form AND
                                      wiptag.blank-no = ipi-blank:
            IF NOT CAN-FIND(FIRST tt-issued-wiptags WHERE tt-issued-wiptags.wiptagno = wiptag.tag-no)
            THEN DO:
                CREATE tt-issued-wiptags.
                ASSIGN tt-issued-wiptags.wiptagno = wiptag.tag-no
                       tt-issued-wiptags.rmtagno1 = wiptag.rm-tag-no
                       tt-issued-wiptags.rmtagno2 = wiptag.spare-char-1
                       tt-issued-wiptags.job-no = wiptag.job-no
                       tt-issued-wiptags.job-no2 = wiptag.job-no2
                       tt-issued-wiptags.rm-i-no = wiptag.rm-i-no
                       tt-issued-wiptags.frm = wiptag.form-no
                       tt-issued-wiptags.blank-no = wiptag.blank-no
                       tt-issued-wiptags.sts = wiptag.sts
                       tt-issued-wiptags.qty = wiptag.pallet-count.
            END. /* IF NOT CAN-FIND(FIRST tt-issued-wiptag */

        END. /* FOR EACH wiptag NO-LOC */

        APPLY "entry" TO fi_sheets-pallet.
    END. /* IF vl-InputParameters THEN */    
    ELSE APPLY "entry" TO fi_rmtagno.
    {methods/setButton.i btn-cancel "Cancel"}
    {methods/setButton.i btn-ok "OK"}
  END.

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:07 am */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE breakout-wiptags C-Win 
PROCEDURE breakout-wiptags :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR vli-qty-per-pallet AS INT NO-UNDO.
  DEF VAR vli-qty-of-tags AS INT NO-UNDO.
  DEF VAR vli-exp-qty-of-sheets AS DECIMAL FORMAT ">,>>>,>>9.9<<<<<" NO-UNDO. 
  DEF VAR vld-total-MRP LIKE job-mat.qty NO-UNDO.
  DEF VAR vli-used-sheets AS INT NO-UNDO.
  DEF VAR vli-seq AS INT NO-UNDO.

  get-w-job: FOR EACH w-job BREAK BY w-job.frm:
      IF FIRST-OF(w-job.frm) THEN
          ASSIGN vld-total-MRP = w-job.qty /* total MRP qty from job-mat */ 
                 vli-qty-per-pallet = w-job.tag-qty /* qty per pallet */ 
                 vli-qty-of-tags = w-job.total-tags /* exp wiptags */  
                 vli-exp-qty-of-sheets = w-job.sheets-tag.  /* exp sheets */

      make-tags: DO WHILE TRUE:
          FIND FIRST tt-rmtags WHERE NOT tt-rmtags.used NO-ERROR. 
          IF NOT AVAIL tt-rmtags THEN LEAVE make-tags.

          IF tt-rmtags.availshtqty >= vli-exp-qty-of-sheets AND
             vli-exp-qty-of-sheets <> 0 and
             vli-exp-qty-of-sheets > vli-qty-per-pallet THEN DO:

                      FIND LAST tt-used-tags WHERE (tt-used-tags.rmtag = tt-rmtags.rmtagno OR
                                             tt-used-tags.rmtag2 = tt-rmtags.rmtagno OR
                                             tt-used-tags.rmtag3 = tt-rmtags.rmtagno) AND
                                            tt-used-tags.rm-i-no = w-job.rm-i-no AND
                                            tt-used-tags.frm = w-job.frm and
                                            tt-used-tags.blank-no = w-job.frm AND
                                            tt-used-tags.usedshts < vli-qty-per-pallet
                  NO-ERROR.
              IF NOT AVAIL tt-used-tags THEN
                  FIND LAST tt-used-tags WHERE (tt-used-tags.rmtag <> tt-rmtags.rmtagno OR
                                                 tt-used-tags.rmtag2 <> tt-rmtags.rmtagno OR
                                                 tt-used-tags.rmtag3 <> tt-rmtags.rmtagno) AND
                                                tt-used-tags.rm-i-no = w-job.rm-i-no AND
                                                tt-used-tags.frm = w-job.frm and
                                                tt-used-tags.blank-no = w-job.frm AND
                                                tt-used-tags.usedshts < vli-qty-per-pallet
                      NO-ERROR.

              IF NOT AVAIL tt-used-tags THEN DO: 
                  CREATE tt-used-tags.
                  ASSIGN vli-seq = vli-seq + 1
                         tt-used-tags.seq = vli-seq.
              END.

              IF tt-used-tags.rmtag = "" THEN ASSIGN tt-used-tags.rmtag = tt-rmtags.rmtag.
              ELSE IF tt-used-tags.rmtag <> "" AND tt-used-tags.rmtag2 = "" THEN 
                  ASSIGN tt-used-tags.rmtag2 = tt-rmtags.rmtag.
              ELSE IF tt-used-tags.rmtag <> "" AND tt-used-tags.rmtag2 <> "" THEN 
                  ASSIGN tt-used-tags.rmtag3 = tt-rmtags.rmtag.

              IF tt-used-tags.usedshts > 0 THEN DO:

                  IF vli-qty-per-pallet >=  tt-used-tags.usedshts + tt-rmtags.availshtqty
                  THEN DO:

                    ASSIGN 
                       tt-used-tags.usedshts = tt-used-tags.usedshts + tt-rmtags.availshtqty
                       vli-exp-qty-of-sheets = vli-exp-qty-of-sheets - tt-rmtags.availshtqty
                       tt-rmtags.availshtqty = 0.

                  END.
                  ELSE IF vli-qty-per-pallet <  tt-used-tags.usedshts + tt-rmtags.availshtqty
                  THEN DO:
                    ASSIGN 
                       vli-used-sheets = (vli-qty-per-pallet - tt-used-tags.usedshts)
                       tt-used-tags.usedshts = tt-used-tags.usedshts + vli-used-sheets /* (vli-qty-per-pallet - tt-used-tags.usedshts) */
                       vli-exp-qty-of-sheets = vli-exp-qty-of-sheets - vli-used-sheets /*(vli-qty-per-pallet - tt-used-tags.usedshts)*/
                       tt-rmtags.availshtqty = tt-rmtags.availshtqty - vli-used-sheets /*(vli-qty-per-pallet - tt-used-tags.usedshts)*/.

                  END.
              END.
              ELSE DO: 

                  IF tt-rmtags.availshtqty > vli-qty-per-pallet AND 
                     vli-exp-qty-of-sheets > vli-qty-per-pallet  THEN DO:

                      ASSIGN tt-used-tags.usedshts = vli-qty-per-pallet
                          vli-exp-qty-of-sheets = vli-exp-qty-of-sheets - tt-used-tags.usedshts 
                          tt-rmtags.availshtqty = tt-rmtags.availshtqty - tt-used-tags.usedshts.
                  END.
                  ELSE IF tt-rmtags.availshtqty < vli-qty-per-pallet AND 
                          vli-exp-qty-of-sheets < tt-rmtags.availshtqty  THEN DO:

                      ASSIGN tt-used-tags.usedshts = vli-exp-qty-of-sheets
                          vli-exp-qty-of-sheets = vli-exp-qty-of-sheets - tt-used-tags.usedshts 
                          tt-rmtags.availshtqty = tt-rmtags.availshtqty - tt-used-tags.usedshts.
                  END.
                  ELSE IF tt-rmtags.availshtqty < vli-qty-per-pallet AND 
                          vli-exp-qty-of-sheets > tt-rmtags.availshtqty  THEN DO:

                      ASSIGN tt-used-tags.usedshts = tt-rmtags.availshtqty
                        vli-exp-qty-of-sheets = vli-exp-qty-of-sheets - tt-used-tags.usedshts 
                        tt-rmtags.availshtqty = tt-rmtags.availshtqty - tt-used-tags.usedshts.
                  END.
              END. /* ELSE DO */

              ASSIGN
                  tt-used-tags.rm-i-no = w-job.rm-i-no
                  tt-used-tags.frm = w-job.frm
                  tt-used-tags.blank-no = w-job.blank-no.

              IF tt-rmtags.availshtqty <= 0 THEN ASSIGN tt-rmtags.used = TRUE.


          END.

          ELSE IF tt-rmtags.availshtqty >= vli-exp-qty-of-sheets AND
             vli-exp-qty-of-sheets <> 0 and
             vli-exp-qty-of-sheets < vli-qty-per-pallet THEN DO:

              CREATE tt-used-tags.
              ASSIGN
                  vli-seq = vli-seq + 1
                  tt-used-tags.seq = vli-seq
                  tt-used-tags.rmtag = tt-rmtags.rmtag
                  tt-used-tags.rm-i-no = w-job.rm-i-no
                  tt-used-tags.frm = w-job.frm
                  tt-used-tags.blank-no = w-job.blank-no
                  tt-used-tags.usedshts = vli-exp-qty-of-sheets
                  vli-exp-qty-of-sheets = vli-exp-qty-of-sheets - tt-used-tags.usedshts
                  tt-rmtags.availshtqty = tt-rmtags.availshtqty - vli-exp-qty-of-sheets.
              IF tt-rmtags.availshtqty <= 0 THEN ASSIGN tt-rmtags.used = TRUE.


          END.
          ELSE IF tt-rmtags.availshtqty < vli-exp-qty-of-sheets AND 
                  tt-rmtags.availshtqty > vli-qty-per-pallet
              THEN DO:

              CREATE tt-used-tags.

              ASSIGN
                  vli-seq = vli-seq + 1
                  tt-used-tags.seq = vli-seq
                  tt-used-tags.rmtag = tt-rmtags.rmtag
                  tt-used-tags.rm-i-no = w-job.rm-i-no
                  tt-used-tags.frm = w-job.frm
                  tt-used-tags.blank-no = w-job.blank-no
                  tt-used-tags.usedshts = vli-qty-per-pallet
                  vli-exp-qty-of-sheets = vli-exp-qty-of-sheets - tt-used-tags.usedshts
                  tt-rmtags.availshtqty = tt-rmtags.availshtqty  - vli-qty-per-pallet.
              IF tt-rmtags.availshtqty <= 0 THEN ASSIGN tt-rmtags.used = TRUE.

          END.
          ELSE IF tt-rmtags.availshtqty < vli-exp-qty-of-sheets AND 
                  tt-rmtags.availshtqty < vli-qty-per-pallet
              THEN DO:

              FIND LAST tt-used-tags WHERE (tt-used-tags.rmtag = tt-rmtags.rmtagno OR
                                             tt-used-tags.rmtag2 = tt-rmtags.rmtagno OR
                                             tt-used-tags.rmtag3 = tt-rmtags.rmtagno) AND
                                            tt-used-tags.rm-i-no = w-job.rm-i-no AND
                                            tt-used-tags.frm = w-job.frm and
                                            tt-used-tags.blank-no = w-job.frm AND
                                            tt-used-tags.usedshts < vli-qty-per-pallet
                  NO-ERROR.

              IF NOT AVAIL tt-used-tags THEN DO: 
                  CREATE tt-used-tags.
                  ASSIGN vli-seq = vli-seq + 1
                         tt-used-tags.seq = vli-seq.
              END.
              /* top */
              IF tt-used-tags.rmtag = "" THEN ASSIGN tt-used-tags.rmtag = tt-rmtags.rmtag.
              ELSE IF tt-used-tags.rmtag <> "" AND tt-used-tags.rmtag2 = "" THEN 
                  ASSIGN tt-used-tags.rmtag2 = tt-rmtags.rmtag.
              ELSE IF tt-used-tags.rmtag <> "" AND tt-used-tags.rmtag2 <> "" THEN 
                  ASSIGN tt-used-tags.rmtag3 = tt-rmtags.rmtag.


              IF tt-used-tags.usedshts > 0 THEN
                  IF (vli-qty-per-pallet - tt-used-tags.usedshts - tt-rmtags.availshtqty) >= 0
                  THEN
                    ASSIGN 
                       tt-used-tags.usedshts = tt-used-tags.usedshts + 
                                            (vli-qty-per-pallet - tt-used-tags.usedshts - tt-rmtags.availshtqty)
                       vli-exp-qty-of-sheets = vli-exp-qty-of-sheets - (vli-qty-per-pallet - tt-used-tags.usedshts - tt-rmtags.availshtqty)
                       tt-rmtags.availshtqty = tt-rmtags.availshtqty - (vli-qty-per-pallet - tt-used-tags.usedshts - tt-rmtags.availshtqty).

                  ELSE
                      ASSIGN 
                         tt-used-tags.usedshts = tt-used-tags.usedshts + 
                                               (vli-qty-per-pallet - tt-used-tags.usedshts)
                         vli-exp-qty-of-sheets = vli-exp-qty-of-sheets - (vli-qty-per-pallet - tt-used-tags.usedshts) 
                  tt-rmtags.availshtqty = tt-rmtags.availshtqty - (vli-qty-per-pallet - tt-used-tags.usedshts).

              ELSE DO:

                     IF (vli-qty-per-pallet - tt-used-tags.usedshts - tt-rmtags.availshtqty) >= 0 THEN
                      ASSIGN 
                       tt-used-tags.usedshts = tt-used-tags.usedshts + tt-rmtags.availshtqty +
                                            (vli-qty-per-pallet - tt-rmtags.availshtqty)
                       vli-exp-qty-of-sheets = vli-exp-qty-of-sheets - (tt-rmtags.availshtqty + (vli-qty-per-pallet -  tt-rmtags.availshtqty))
                       tt-rmtags.availshtqty = tt-rmtags.availshtqty - (tt-rmtags.availshtqty + (vli-qty-per-pallet -  tt-rmtags.availshtqty)) .

              END.

                 /*ASSIGN tt-used-tags.usedshts = tt-rmtags.availshtqty
                          vli-exp-qty-of-sheets = vli-exp-qty-of-sheets - tt-used-tags.usedshts 
                          tt-rmtags.availshtqty = tt-rmtags.availshtqty - tt-used-tags.usedshts.*/
              DEFINE VAR count-availshtqty AS INT NO-UNDO.

              ASSIGN
                  tt-used-tags.rm-i-no = w-job.rm-i-no
                  tt-used-tags.frm = w-job.frm
                  tt-used-tags.blank-no = w-job.blank-no
                  /* tt-used-tags.usedshts = tt-used-tags.usedshts + tt-rmtags.availshtqty*/
                 .
              count-availshtqty = tt-rmtags.availshtqty .
              IF tt-rmtags.availshtqty <= 0 THEN ASSIGN tt-rmtags.used = TRUE.
              FIND FIRST tt-rmtags WHERE NOT tt-rmtags.used NO-ERROR. 
              IF NOT AVAIL tt-rmtags THEN LEAVE make-tags.
                IF AVAIL tt-rmtags then ASSIGN
                    tt-used-tags.rmtag2 = tt-rmtags.rmtag
                    tt-rmtags.availshtqty =  tt-rmtags.availshtqty -  count-availshtqty .
                IF tt-rmtags.availshtqty <= 0 THEN ASSIGN tt-rmtags.used = TRUE.
              /* bottom */
          END.

          ELSE IF tt-rmtags.availshtqty < vli-exp-qty-of-sheets AND 
                  tt-rmtags.availshtqty > vli-qty-per-pallet
              THEN DO:

              /* btr */
              FIND LAST tt-used-tags WHERE (tt-used-tags.rmtag = tt-rmtags.rmtagno OR
                                             tt-used-tags.rmtag2 = tt-rmtags.rmtagno OR
                                             tt-used-tags.rmtag3 = tt-rmtags.rmtagno) AND
                                            tt-used-tags.rm-i-no = w-job.rm-i-no AND
                                            tt-used-tags.frm = w-job.frm and
                                            tt-used-tags.blank-no = w-job.frm AND
                                            tt-used-tags.usedshts < vli-qty-per-pallet
                  NO-ERROR.

              IF NOT AVAIL tt-used-tags THEN DO: 
                  CREATE tt-used-tags.
                  ASSIGN vli-seq = vli-seq + 1
                         tt-used-tags.seq = vli-seq.
              END.

              IF tt-used-tags.rmtag = "" THEN ASSIGN tt-used-tags.rmtag = tt-rmtags.rmtag.
              ELSE IF tt-used-tags.rmtag <> "" AND tt-used-tags.rmtag2 = "" THEN 
                  ASSIGN tt-used-tags.rmtag2 = tt-rmtags.rmtag.
              ELSE IF tt-used-tags.rmtag <> "" AND tt-used-tags.rmtag2 <> "" THEN 
                  ASSIGN tt-used-tags.rmtag3 = tt-rmtags.rmtag.

              IF tt-used-tags.usedshts > 0 THEN
                  ASSIGN 
                  tt-used-tags.usedshts = tt-used-tags.usedshts + 
                                           (vli-qty-per-pallet - tt-used-tags.usedshts).
              ELSE ASSIGN tt-used-tags.usedshts = vli-qty-per-pallet.

              ASSIGN

                  tt-used-tags.rm-i-no = w-job.rm-i-no
                  tt-used-tags.frm = w-job.frm
                  tt-used-tags.blank-no = w-job.blank-no

                  vli-exp-qty-of-sheets = vli-exp-qty-of-sheets - tt-used-tags.usedshts
                  tt-rmtags.availshtqty = tt-rmtags.availshtqty - tt-used-tags.usedshts.
              IF tt-rmtags.availshtqty <= 0 THEN ASSIGN tt-rmtags.used = TRUE.

          END.

          IF vli-exp-qty-of-sheets <= 0 THEN LEAVE make-tags.

      END. /* make-tags: DO WHILE TRUE */

  END. /* get-w-job: FOR EACH w-job BREAK BY w-job.frm */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-w-job C-Win 
PROCEDURE create-w-job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER iop-sheets-tag AS INT NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER iop-total-wip-tags AS INT NO-UNDO.

  DEF VAR vli-avail-rm-sheets AS INT INITIAL 0 NO-UNDO.
  DEF VAR vli-used-rm-sheets AS INT INITIAL 0 NO-UNDO.

  DEF VAR v-time AS INT NO-UNDO.
  DEF VAR v-date AS DATE NO-UNDO.

  DEF BUFFER bu-w-job FOR w-job.

  IF tg_wip-reported AND fi_tag-date NE ? THEN
  DO:
     IF fi_tag-ampm = "AM" THEN
        v-time = (fi_tag-hr * 3600) + (fi_tag-min * 60).
     ELSE
        v-time = ((fi_tag-hr + 12) * 3600) + (fi_tag-min * 60).
  END.
  ELSE
     v-time = TIME.

  CREATE w-job.
  ASSIGN
    v-date = IF tg_wip-reported AND fi_tag-date NE ? THEN fi_tag-date ELSE TODAY
    w-job.company  = job-mat.company
    w-job.location = locode
    w-job.blank-no = job-mat.blank-no
    w-job.cust-no = job-hdr.cust-no
    w-job.rm-i-name = ITEM.i-name
    w-job.cons-uom = ITEM.cons-uom
    w-job.rm-i-no = job-mat.rm-i-no
    w-job.job-no = job-mat.job-no
    w-job.job-no2 = job-mat.job-no2
    w-job.len     = job-mat.len
    w-job.qty     = job-mat.qty  /* total MRP */
    w-job.qty-uom = job-mat.qty-uom
    w-job.frm = job-mat.frm
    w-job.wid = job-mat.wid
    w-job.job-due-date = job-hdr.due-date
    w-job.tag-date = v-date
    w-job.tag-time = v-time
    w-job.tag-timex = STRING(w-job.tag-time,"HH:MM")
    w-job.upd-date = TODAY
    w-job.upd-time = TIME
    w-job.rm-whs = item.loc
    w-job.rm-bin = item.loc-bin
    w-job.first-mach-code = IF tg_wip-reported THEN begin_mach ELSE ""
    w-job.wip-whs = IF tg_wip-reported THEN fi_wip-whs ELSE ""
    w-job.wip-bin = IF tg_wip-reported THEN fi_wip-bin ELSE ""
    w-job.sheets-tag = iop-sheets-tag.

  FIND FIRST eb WHERE
       eb.company EQ cocode AND
       eb.est-no EQ job-hdr.est-no AND
       eb.form-no EQ job-mat.frm AND
       (eb.blank-no EQ job-mat.blank-no OR job-mat.blank-no EQ 0)
       NO-LOCK NO-ERROR.

  IF AVAIL eb THEN
  DO:
     w-job.num-up = eb.num-wid * eb.num-len.

     FIND FIRST ef WHERE
          ef.company EQ cocode AND
          ef.est-no EQ eb.est-no AND
          ef.form-no EQ eb.form-no
          NO-LOCK NO-ERROR.

     IF AVAIL ef THEN
     DO:
        FIND FIRST est WHERE
             est.company EQ cocode AND
             est.est-no EQ ef.est-no
             NO-LOCK NO-ERROR.

        IF AVAIL est THEN
        DO:
           ASSIGN
              w-job.sht-wid = ef.nsh-wid
              w-job.sht-len = ef.nsh-len.

           IF est.est-type < 5 THEN /*folding*/
              w-job.num-out = ef.n-out.
           ELSE /*corrugated*/
              w-job.num-out = ef.n-out * ef.n-out-l.

           RELEASE ef.
        END.
     END.
  END.

  FIND FIRST itemfg WHERE
       itemfg.company EQ cocode AND
       itemfg.i-no EQ job-hdr.i-no
       NO-LOCK NO-ERROR.

  IF AVAIL itemfg THEN
  DO:
     ASSIGN
       w-job.fg-i-name = itemfg.i-name
       w-job.fg-i-no   = itemfg.i-no.

     RELEASE itemfg.
  END.

  IF NOT checkWhsBin(cocode,w-job.rm-whs,w-job.rm-bin) THEN
  DO:
    IF v-bin NE 'RMITEM' THEN
       ASSIGN
          w-job.rm-whs = SUBSTR(v-bin,1,5)
          w-job.rm-bin = SUBSTR(v-bin,6).

    IF NOT checkWhsBin(cocode,w-job.rm-whs,w-job.rm-bin) THEN
    DO:
       FIND FIRST rm-bin WHERE
            rm-bin.company EQ cocode AND
            rm-bin.loc EQ locode AND
            rm-bin.i-no EQ '' AND
            rm-bin.loc-bin NE ''
            NO-LOCK NO-ERROR.

       ASSIGN
         w-job.rm-whs = IF AVAIL loc THEN loc.loc ELSE ''
         w-job.rm-bin = IF AVAIL rm-bin THEN rm-bin.loc-bin ELSE ''.
    END.
  END.

  ASSIGN
     w-job.tag-qty = fi_sheets-pallet
     w-job.total-tags = w-job.qty / w-job.tag-qty + .49.

  IF w-job.total-tags GT iop-total-wip-tags THEN
     ASSIGN w-job.total-tags = iop-total-wip-tags.

  ASSIGN
     iop-total-wip-tags = iop-total-wip-tags - w-job.total-tags
     iop-sheets-tag = iop-sheets-tag - (w-job.total-tags * w-job.tag-qty).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-wiptag C-Win 
PROCEDURE create-wiptag :
/*------------------------------------------------------------------------------
  Purpose:     
  PARAMs:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ipTagNo AS INT NO-UNDO.
  DEF INPUT PARAM ipQty AS INT NO-UNDO.

  DEF VAR i AS INT NO-UNDO.
  DEF VAR tagNo AS CHAR NO-UNDO.
  DEF VAR ld AS DEC NO-UNDO.
  DEF VAR vlc-rmtag AS CHAR FORMAT "x(20)" NO-UNDO.
  DEF VAR vlc-rmtag2 AS CHAR FORMAT "x(20)" NO-UNDO.
  DEF VAR vlc-rmtag3 AS CHAR FORMAT "x(20)" NO-UNDO.

  DO WHILE TRUE:
     tagNo = CAPS(STRING(w-job.job-no,"X(6)")) + STRING(w-job.job-no2,"99")
           + STRING(w-job.frm,"999") + STRING(w-job.blank-no,"99")
           + STRING(ipTagNo,'99999').

     IF NOT CAN-FIND(FIRST wiptag WHERE
        wiptag.company EQ cocode AND
        wiptag.tag-no EQ tagNo) THEN
        LEAVE.

    ipTagNo = ipTagNo + 1.
  END. /* do while */

  FIND FIRST tt-used-tags WHERE NOT tt-used-tags.seq-processed
      USE-INDEX idx-seq NO-ERROR.
  IF AVAIL tt-used-tags THEN
      ASSIGN tt-used-tags.seq-processed = TRUE
             vlc-rmtag = tt-used-tags.rmtag
             vlc-rmtag2 = tt-used-tags.rmtag2
             vlc-rmtag3 = tt-used-tags.rmtag3.
  ELSE ASSIGN 
      vlc-rmtag = fi_rmtagno
      vlc-rmtag2 = fi_toRMTag
      vlc-rmtag3 = "".

  CREATE wiptag.
  ASSIGN
   wiptag.company      = cocode
   wiptag.tag-no       = tagNo
   wiptag.job-no       = w-job.job-no
   wiptag.job-no2      = w-job.job-no2
   wiptag.cust-no      = w-job.cust-no
   wiptag.rm-i-no      = CAPS(w-job.rm-i-no)
   wiptag.rm-i-name    = w-job.rm-i-name
   wiptag.fg-i-no      = CAPS(w-job.fg-i-no)
   wiptag.fg-i-name    = w-job.fg-i-name
   wiptag.case-bundle  = 1
   wiptag.pallet-count = ipQty
   wiptag.rm-whs       = w-job.rm-whs
   wiptag.rm-bin       = w-job.rm-bin
   wiptag.tot-cases    = 0
   wiptag.sts          = IF vl-InputParameters THEN "Issued" ELSE "Printed"
   wiptag.tag-date     = w-job.tag-date
   wiptag.tag-time     = (INT(SUBSTRING(w-job.tag-timex,1,2)) * 3600) + (INT(SUBSTRING(w-job.tag-timex,4)) * 60)
   wiptag.crt-user     = USERID("NOSWEAT")
   wiptag.upd-user     = USERID("NOSWEAT")
   wiptag.crt-time     = w-job.upd-time
   wiptag.upd-time     = w-job.upd-time
   wiptag.crt-date     = w-job.upd-date
   wiptag.upd-date     = w-job.upd-date
   wiptag.num-up       = w-job.num-up
   wiptag.n-out        = w-job.num-out
   wiptag.rm-tag-no    = vlc-rmtag  /* fi_rmtagno btr */
   wiptag.spare-char-1 = vlc-rmtag2 /* btr */
   wiptag.pallet-qty   = INTEGER(fi_sheets-pallet:SCREEN-VALUE IN FRAME {&FRAME-NAME})
   wiptag.blank-no     = w-job.blank-no
   wiptag.form-no      = w-job.frm
   wiptag.cons-uom     = w-job.cons-uom
   wiptag.wip-warehouse  = w-job.wip-whs
   wiptag.wip-rm-bin     = w-job.wip-bin
   w-job.tag-no        = wiptag.tag-no.

  CREATE reftable.
  ASSIGN
     reftable.reftable = "WIPLEN"
     reftable.company = wiptag.company
     reftable.CODE = wiptag.tag-no
     reftable.val[1] = w-job.sht-wid 
     reftable.val[2] = w-job.sht-len.

  RELEASE reftable.

  IF w-job.first-mach-code NE "" THEN
  DO:
     FIND FIRST wiptag-mch WHERE
          wiptag-mch.company EQ wiptag.company AND
          wiptag-mch.tag-no  EQ wiptag.tag-no
          USE-INDEX seq-no
          NO-ERROR.

     IF NOT AVAIL wiptag-mch THEN
     DO:
        CREATE wiptag-mch.
        ASSIGN wiptag-mch.company = wiptag.company
               wiptag-mch.tag-no  = wiptag.tag-no
               wiptag-mch.seq-no  = 1.
     END.

     ASSIGN
        wiptag-mch.m-code  = w-job.first-mach-code
        wiptag-mch.produced-qty = wiptag.pallet-count.

     RELEASE wiptag-mch.
  END.

  ASSIGN opc-output = "WIP-Issued".

  FIND CURRENT wiptag NO-LOCK NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY fi_UOM-label reprintTag fi_Total-MRP-Qty fi_rmtagno fi_toRMTag 
          fi-single-label ed_singletags tg_wip-reported fi_tag-date fi_tag-hr 
          fi_oh-lf fi_tag-min fi_tag-ampm begin_mach fi_wip-whs fi_wip-bin 
          begin_job begin_job2 begin_job_form end_job_form fi_pallet_height 
          fi_exp-sheets-roll fi_sheets-pallet begin_job_blank fi_total-wip-tags 
          tb_16ths end_job_blank begin_rm-i-no end_rm-i-no 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE reprintTag fi_rmtagno fi_toRMTag ed_singletags tg_wip-reported 
         begin_job begin_job2 begin_job_form end_job_form fi_pallet_height 
         fi_exp-sheets-roll fi_sheets-pallet fi_total-wip-tags tb_16ths btn-ok 
         btn-cancel RECT-7 RECT-8 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-consumption-uom C-Win 
PROCEDURE get-consumption-uom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-tagno AS CHAR FORMAT "x(20)" INIT "" NO-UNDO.

  IF LENGTH(ip-tagno) < 17 THEN RETURN NO-APPLY.

  FIND FIRST loadtag WHERE loadtag.company = cocode AND
                           loadtag.item-type = TRUE AND
                           loadtag.tag-no = ip-tagno
      NO-LOCK NO-ERROR.
  IF AVAIL loadtag THEN DO:
      FIND FIRST ITEM NO-LOCK
          WHERE ITEM.company = cocode AND
                ITEM.i-no = loadtag.i-no NO-ERROR.
      IF AVAIL ITEM THEN 
          ASSIGN fi_UOM-label:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ITEM.cons-uom.
  END.
  ELSE DO:
     RETURN NO-APPLY.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable_UI C-Win 
PROCEDURE local-enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DISPLAY fi_UOM-label fi_Total-MRP-Qty ed_singletags fi-single-label fi_toRMTag 
          reprintTag fi_rmtagno fi_oh-lf tg_wip-reported fi_tag-date fi_tag-hr 
          fi_tag-min begin_mach fi_wip-whs fi_wip-bin begin_job begin_job2 
          begin_job_form end_job_form begin_job_blank end_job_blank 
          begin_rm-i-no end_rm-i-no fi_pallet_height fi_exp-sheets-roll 
          fi_sheets-pallet fi_total-wip-tags tb_16ths fi_tag-ampm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  IF vl-InputParameters THEN DO:
    ENABLE /* ed_singletags */ /* fi_toRMTag */ /* reprintTag */ /* fi_rmtagno */ 
          tg_wip-reported  /* begin_job begin_job2 begin_job_form 
         end_job_form */ fi_pallet_height 
         /* fi_exp-sheets-roll */ fi_sheets-pallet fi_total-wip-tags 
         tb_16ths btn-ok 
         btn-cancel RECT-7 RECT-8 
      WITH FRAME FRAME-A IN WINDOW C-Win.

    ASSIGN fi_rmtagno:SCREEN-VALUE = ipc-RMTagNo
           fi_toRMTag:SCREEN-VALUE = ipc-RMTagNo
           begin_job:SCREEN-VALUE = ipc-job-no
           begin_job2:SCREEN-VALUE = string(ipi-job-no2,"99")
           begin_rm-i-no:SCREEN-VALUE = ipc-i-no
           END_rm-i-no:SCREEN-VALUE = ipc-i-no
           begin_job_form:SCREEN-VALUE = string(ipi-form,">9") 
           end_job_form:SCREEN-VALUE = string(ipi-form,">9")
           begin_job_blank:SCREEN-VALUE = string(ipi-blank,">9")
           END_job_blank:SCREEN-VALUE = string(ipi-blank,">9").
    APPLY "leave" TO fi_toRMTag.
    APPLY "leave" TO begin_job2.
    APPLY "leave" TO fi_oh-lf.
    APPLY "value-changed" TO END_job_form.
    APPLY "LEAVE" TO fi_pallet_height.
  END.
  ELSE
    ENABLE ed_singletags fi_toRMTag reprintTag  fi_rmtagno 
           tg_wip-reported  begin_job begin_job2 begin_job_form 
           end_job_form fi_pallet_height 
           fi_exp-sheets-roll fi_sheets-pallet fi_total-wip-tags tb_16ths 
           btn-ok 
           btn-cancel RECT-7 RECT-8 
        WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ok-button C-Win 
PROCEDURE ok-button :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-error AS LOG NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN {&displayed-objects}.

     IF fi_pallet_height <= 0 THEN DO:
        MESSAGE "Please enter a valid Pallet Height" VIEW-AS ALERT-BOX.
        RETURN ERROR.
     END.

     IF tg_wip-reported THEN
     DO:
        IF fi_tag-hr GT 12 THEN
        DO:
           MESSAGE "Invalid Tag Time Hour."
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
           APPLY "ENTRY" TO fi_tag-hr.
           RETURN ERROR.
        END.

        IF fi_tag-min GT 59 THEN
        DO:
           MESSAGE "Invalid Tag Time Minutes."
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
           APPLY "ENTRY" TO fi_tag-min.
           RETURN ERROR.
        END.

        IF begin_mach NE "" AND
           NOT can-find(FIRST mach where
           mach.company EQ cocode and
           mach.m-code EQ begin_mach) THEN
           DO:
              MESSAGE "Invalid Machine."
                 VIEW-AS ALERT-BOX ERROR BUTTONS OK.
              APPLY "ENTRY" TO begin_mach.
              RETURN ERROR.
           END.

        IF fi_wip-whs NE "" AND
           NOT CAN-FIND(FIRST loc WHERE
           loc.company EQ cocode AND
           loc.loc     EQ fi_wip-whs) THEN
           DO:
              MESSAGE "Invalid WIP Warehouse."
                 VIEW-AS ALERT-BOX ERROR BUTTONS OK.
              APPLY "entry" TO fi_wip-whs.
              RETURN ERROR.
           END.

        IF fi_wip-bin NE "" AND
           NOT CAN-FIND(FIRST wip-bin WHERE
               wip-bin.company EQ cocode AND
               wip-bin.loc     EQ fi_wip-whs AND
               wip-bin.loc-bin EQ fi_wip-bin) THEN
               DO:
                  MESSAGE "Invalid WIP Bin."
                      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                  APPLY "entry" TO fi_wip-bin.
                  RETURN ERROR.
               END.

        IF fi_wip-whs EQ "" AND fi_wip-bin NE "" THEN
        DO:
           MESSAGE "Invalid WIP Warehouse."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           APPLY "entry" TO fi_wip-whs.
           RETURN ERROR.
        END.

        IF fi_wip-whs NE "" AND fi_wip-bin EQ "" THEN
        DO:
           MESSAGE "Invalid WIP Bin."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           APPLY "entry" TO fi_wip-bin.
           RETURN ERROR.
        END.
     END.

     IF reprintTag THEN DO:
        IF NOT CAN-FIND(FIRST wiptag WHERE
           wiptag.company EQ cocode AND
           wiptag.tag-no EQ reprintwiptagno) THEN DO:

           MESSAGE 'Invalid WIP Tag, Please Try Again ...' VIEW-AS ALERT-BOX ERROR.
           APPLY 'ENTRY':U TO reprintwiptagno.
        END.
        ELSE
        DO:
           RUN reprintTag.
           APPLY 'ENTRY':U TO reprintwiptagno IN FRAME {&FRAME-NAME}.
        END.
     END.
     ELSE DO:
        RUN run-report NO-ERROR. 
        IF ERROR-STATUS:ERROR THEN RETURN.
        APPLY "entry" TO fi_rmtagno IN FRAME {&FRAME-NAME}.
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE outputTagHeader C-Win 
PROCEDURE outputTagHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  PUT UNFORMATTED 
     "Tag#,Customer,Job Number,Blank #,Form #,"
     "RM Name,RM Item#,Total MRP,Unit of Measure,Pallet Count,FG Name,FG Item#,RM Whse,RM Bin,"
     "Last Machine,Last Produced Qty,Job Due Date,Partial Qty,Length,Width,Num Up, Num Out,Tag Date,"
     "Tag Time,Updated Date,Updated Time,Sht Len,Sht Wid,WIP Whs,WIP Bin,First Machine," SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE outputTagLine C-Win 
PROCEDURE outputTagLine :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipQty AS INTEGER NO-UNDO.

  PUT UNFORMATTED
    '"'   removeChars(w-job.tag-no)
    '","' removeChars(w-job.cust-no)
    '","' removeChars(w-job.job-no) + "-" + STRING(w-job.job-no2)
    '","' w-job.blank-no
    '","' w-job.frm
    '","' removeChars(w-job.rm-i-name)
    '","' removeChars(w-job.rm-i-no)
    '","' w-job.qty
    '","' removeChars(w-job.qty-uom)
    '","' w-job.tag-qty
    '","' removeChars(w-job.fg-i-name)
    '","' removeChars(w-job.fg-i-no)
    '","' removeChars(w-job.rm-whs)
    '","' removeChars(w-job.rm-bin)
    '","' removeChars(w-job.last-mach-code)
    '","' w-job.last-prod-qty
    '","' w-job.job-due-date
    '","' w-job.partial
    '","' w-job.len
    '","' w-job.wid
    '","' w-job.num-up
    '","' w-job.num-out
    '","' w-job.tag-date
    '","' w-job.tag-timex
    '","' w-job.upd-date
    '","' STRING(w-job.upd-time,"HH:MM")
    '","' w-job.sht-len
    '","' w-job.sht-wid
    '","' removeChars(w-job.wip-whs)
    '","' removeChars(w-job.wip-bin)
    '","' removeChars(w-job.first-mach-code)
    '"' SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE replace-job-mat C-Win 
PROCEDURE replace-job-mat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-form-no AS INT NO-UNDO.

   DEF VAR ld-job-up AS DEC NO-UNDO.
   DEF VAR ld-job-qty LIKE job-hdr.qty NO-UNDO.
   DEF VAR count-mat AS INT NO-UNDO.
   DEF VAR v-out AS INT NO-UNDO.
   DEF VAR choice AS LOG NO-UNDO.
   DEF VAR v-cost AS DEC NO-UNDO.
   DEF VAR lv-sheet AS INT NO-UNDO.
   DEF VAR lv-blank AS INT NO-UNDO.
   DEF VAR ll-layout AS LOG NO-UNDO.

   DEF BUFFER xitem FOR item.

   EMPTY TEMP-TABLE tt-s-num.

   CREATE tt-s-num.
   tt-s-num.s-num = ip-form-no.

   /*FOR EACH tt-s-num BREAK BY tt-s-num.s-num:*/
   FIND FIRST job WHERE
        job.company EQ loadtag.company AND
        job.job-no  EQ begin_job AND
        job.job-no2 EQ begin_job2
        NO-LOCK NO-ERROR.

   FIND FIRST ITEM WHERE
        item.company EQ job.company AND
        item.i-no    EQ loadtag.i-no
        NO-LOCK NO-ERROR.

   IF AVAIL job AND AVAIL ITEM THEN
   DO:
      ASSIGN
       fil_id    = ?
       ll-layout = CAN-DO("1,2,3,4,B,P,R",item.mat-type).

      FIND job-mat WHERE ROWID(job-mat) EQ tt-s-num.row-id NO-LOCK NO-ERROR.

      IF NOT AVAIL job-mat THEN DO:
        EMPTY TEMP-TABLE item-chg.

        FOR EACH job-mat FIELDS(company rm-i-no)
            WHERE job-mat.company  EQ job.company
              AND job-mat.job      EQ job.job
              AND job-mat.job-no   EQ job.job-no
              AND job-mat.job-no2  EQ job.job-no2
              AND job-mat.frm      EQ tt-s-num.s-num
            NO-LOCK,

            FIRST xitem FIELDS(i-no)
            WHERE xitem.company  EQ job-mat.company
              AND xitem.i-no     EQ job-mat.rm-i-no
              AND xitem.mat-type EQ item.mat-type
            NO-LOCK:

          count-mat = count-mat + 1.
          CREATE item-chg.
          ASSIGN
           item-chg.i-no   = xitem.i-no
           item-chg.rec-id = RECID(job-mat)
           fil_id          = RECID(item-chg).    
        END.

        IF count-mat GT 1 THEN RUN rm/g-itmchg.w.

        FIND FIRST item-chg WHERE RECID(item-chg) EQ fil_id NO-LOCK NO-ERROR.

        fil_id = ?.

        RELEASE job-mat.

        IF AVAIL item-chg THEN
        FIND job-mat WHERE RECID(job-mat) EQ item-chg.rec-id NO-ERROR.
      END. /*IF NOT AVAIL job-mat*/

      CREATE tt-job-mat.

      ASSIGN
       ld-job-up  = 0
       ld-job-qty = 0.

      FOR EACH job-hdr FIELDS(n-on qty) WHERE
          job-hdr.company EQ job.company AND
          job-hdr.job     EQ job.job AND
          job-hdr.job-no  EQ job.job-no AND
          job-hdr.job-no2 EQ job.job-no2 AND
          job-hdr.frm     EQ tt-s-num.s-num
          NO-LOCK:
          ASSIGN
             ld-job-up  = ld-job-up + job-hdr.n-on
             ld-job-qty = ld-job-qty + job-hdr.qty.  
      END.

      FIND FIRST est WHERE
           est.company EQ job.company AND
           est.est-no  EQ job.est-no
           NO-LOCK NO-ERROR.

      IF AVAIL est AND (est.est-type EQ 2 OR est.est-type EQ 6) THEN DO:
         FOR EACH eb FIELDS(num-up) WHERE
             eb.company EQ est.company AND
             eb.est-no  EQ est.est-no AND
             eb.form-no EQ tt-s-num.s-num
             NO-LOCK:
             ld-job-up = ld-job-up + eb.num-up.
         END.
      END.

      IF ll-layout OR ld-job-up EQ 0 THEN ld-job-up = 1.

      /*IF FIRST(tt-s-num.s-num) THEN DO:*/
        lv-sheet = ip-form-no.

        IF AVAIL job-mat THEN
          ASSIGN
           lv-blank = job-mat.blank-no
           v-out    = (job-mat.n-up / ld-job-up).
        /*ELSE
          ASSIGN
           lv-blank = 0
           v-out    = 1.*/

        IF ll-layout THEN DO:
          RUN rm/g-iss2.w (lv-sheet, lv-blank, INPUT-OUTPUT v-out). 

          IF AVAIL job-mat THEN DO:
            IF item.i-code EQ "R" THEN DO:
              IF (item.r-wid NE 0 AND item.r-wid LT job-mat.wid) OR
                 (item.r-wid EQ 0 AND (item.s-wid LT job-mat.wid OR
                                       item.s-len LT job-mat.len)) THEN DO:
                choice = NO.

                IF item.r-wid NE 0 THEN
                  RUN rm/g-iss21.w (job-mat.len, job-mat.len, item.r-wid, job-mat.wid, job-mat.frm,
                                    OUTPUT choice).
                ELSE
                  RUN rm/g-iss21.w (item.s-len, job-mat.len, item.s-wid,job-mat.wid, job-mat.frm,
                                    OUTPUT choice).

                IF NOT choice THEN DELETE tt-job-mat.
              END.
            END.
          END.
        END.
      /*END.*/

      IF AVAIL job-mat THEN DO:
        FIND FIRST xitem
            WHERE xitem.company EQ cocode
              AND xitem.i-no    EQ job-mat.rm-i-no
            NO-LOCK NO-ERROR.
        IF NOT AVAIL xitem THEN DELETE tt-job-mat.

        IF AVAIL tt-job-mat THEN
          BUFFER-COPY job-mat TO tt-job-mat
          ASSIGN tt-job-mat.row-id = ROWID(job-mat).
      END.
      /*ELSE DO:
        ASSIGN
         tt-job-mat.company  = job.company
         tt-job-mat.job      = job.job
         tt-job-mat.job-no   = job.job-no
         tt-job-mat.job-no2  = job.job-no2
         tt-job-mat.frm      = tt-s-num.s-num
         tt-job-mat.blank-no = lv-blank
         tt-job-mat.qty-uom  = item.cons-uom
         tt-job-mat.n-up     = ld-job-up.

        IF po-ordl.pr-qty-uom:SCREEN-VALUE NE ""  AND
           DEC(po-ordl.ord-qty:SCREEN-VALUE) NE 0 THEN
          ASSIGN
           tt-job-mat.qty-uom = po-ordl.pr-qty-uom:SCREEN-VALUE
           tt-job-mat.qty     = DEC(po-ordl.ord-qty:SCREEN-VALUE).

        IF po-ordl.pr-uom:SCREEN-VALUE NE ""   AND
           DEC(po-ordl.cost:SCREEN-VALUE) NE 0 THEN
          ASSIGN
           tt-job-mat.sc-uom   = po-ordl.pr-uom:SCREEN-VALUE
           tt-job-mat.std-cost = DEC(po-ordl.cost:SCREEN-VALUE).
      END.*/

      IF AVAIL tt-job-mat THEN DO:
        IF tt-job-mat.sc-uom EQ tt-job-mat.qty-uom THEN
          v-cost = tt-job-mat.std-cost.
        ELSE
          RUN sys/ref/convcuom.p(tt-job-mat.sc-uom,
                                 tt-job-mat.qty-uom,
                                 tt-job-mat.basis-w,
                                 tt-job-mat.len,
                                 tt-job-mat.wid,
                                 item.s-dep,
                                 tt-job-mat.std-cost,
                                 OUTPUT v-cost).

        v-cost = v-cost * tt-job-mat.qty.                       

        IF tt-job-mat.n-up LE 0 THEN tt-job-mat.n-up = 1.
        IF ld-job-up LE 0 THEN ld-job-up = 1.
        IF v-out LE 0 THEN v-out = 1.

        ASSIGN                 
         tt-job-mat.rm-i-no = item.i-no
         tt-job-mat.i-no    = item.i-no
         tt-job-mat.basis-w = item.basis-w
         tt-job-mat.qty     = tt-job-mat.qty * tt-job-mat.n-up
         tt-job-mat.n-up    = ld-job-up * v-out
         tt-job-mat.qty     = tt-job-mat.qty / tt-job-mat.n-up
         tt-job-mat.cost-m  = v-cost / (ld-job-qty / 1000).

        IF item.i-code EQ "R" OR NOT AVAIL job-mat THEN
          ASSIGN
           tt-job-mat.sc-uom  = item.cons-uom
           tt-job-mat.wid     = IF item.r-wid NE 0 THEN
                                item.r-wid ELSE item.s-wid
           tt-job-mat.len     = IF item.r-wid NE 0 THEN
                                tt-job-mat.len ELSE item.s-len.

        IF tt-job-mat.qty-uom EQ "EA" THEN DO:
          {sys/inc/roundup.i tt-job-mat.qty}
        END.

        v-cost = v-cost / tt-job-mat.qty.
        IF v-cost = ? THEN v-cost = 0.

        IF tt-job-mat.qty-uom EQ tt-job-mat.sc-uom THEN
          tt-job-mat.std-cost = v-cost.
        ELSE
          RUN sys/ref/convcuom.p(tt-job-mat.qty-uom,
                                 tt-job-mat.sc-uom,
                                 tt-job-mat.basis-w,
                                 tt-job-mat.len,
                                 tt-job-mat.wid,
                                 item.s-dep,
                                 v-cost,
                                 OUTPUT tt-job-mat.std-cost).
      END.

      DELETE tt-s-num.

      /* end. each tt-s-num*/
      FIND FIRST tt-job-mat WHERE
           tt-job-mat.frm EQ ip-form-no
           NO-ERROR.

      IF AVAIL tt-job-mat THEN
         fil_id = RECID(tt-job-mat).



   END. /*avail job and avail item*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reprintTag C-Win 
PROCEDURE reprintTag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR numberTags AS INTEGER NO-UNDO.
  DEF VAR choice AS LOG NO-UNDO.
  DEF VAR v-sheets-tag AS INT NO-UNDO.
  DEF VAR v-total-wip-tags AS INT NO-UNDO.

  FIND FIRST wiptag WHERE
       wiptag.company EQ cocode AND
       wiptag.tag-no EQ reprintwiptagno
       NO-LOCK NO-ERROR.

  IF NOT AVAILABLE wiptag THEN DO:
    MESSAGE 'WIP Tag Record Not Found.' VIEW-AS ALERT-BOX ERROR.
    RETURN.
  END.

  EMPTY TEMP-TABLE w-job.

  ASSIGN
     v-sheets-tag = fi_exp-sheets-roll
     v-total-wip-tags = fi_total-wip-tags.

  FOR FIRST job-mat WHERE
      job-mat.company EQ wiptag.company AND
      job-mat.rm-i-no EQ wiptag.rm-i-no AND
      job-mat.job-no EQ wiptag.job-no AND
      job-mat.job-no2 EQ wiptag.job-no2 AND
      job-mat.frm EQ wiptag.form-no AND
      job-mat.blank-no EQ wiptag.blank-no
      NO-LOCK,
      FIRST job-hdr FIELDS(cust-no i-no due-date est-no) WHERE
            job-hdr.company EQ job-mat.company AND
            job-hdr.job-no  EQ job-mat.job-no AND
            job-hdr.job-no2 EQ job-mat.job-no2 AND
            job-hdr.frm EQ job-mat.frm
            NO-LOCK,
      FIRST ITEM FIELDS(i-no i-name cons-uom loc loc-bin) WHERE
            ITEM.company EQ wiptag.company AND
            ITEM.i-no EQ wiptag.rm-i-no
            NO-LOCK:

            RUN create-w-job(INPUT-OUTPUT v-sheets-tag,
                             INPUT-OUTPUT v-total-wip-tags).

            ASSIGN
               w-job.tag-no    = wiptag.tag-no
               w-job.tag-qty   = wiptag.pallet-count
               w-job.cons-uom  = wiptag.cons-uom
               w-job.total-tags = 1.

            FIND LAST wiptag-mch WHERE
                 wiptag-mch.company EQ wiptag.company AND
                 wiptag-mch.tag-no  EQ wiptag.tag-no
                 USE-INDEX seq-no
                 NO-LOCK NO-ERROR.

            IF AVAIL wiptag-mch THEN
            DO:
               ASSIGN
                 w-job.last-mach-code = wiptag-mch.m-code
                 w-job.last-prod-qty  = wiptag-mch.produced-qty.

               RELEASE wiptag-mch.
            END.
  END.

  IF AVAIL w-job THEN
  DO:
     RUN jcrep/d-wiptg.w.

     IF CAN-FIND(FIRST w-job) THEN
     DO:
        MESSAGE "Are you Sure you Want to Reprint WIP Tag File? " 
           VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice.

        IF NOT choice THEN
           LEAVE.

        w-job.total-tags = IF AVAILABLE cust AND cust.int-field[1] GT 0 THEN cust.int-field[1]
                           ELSE IF v-mult GT 0 THEN v-mult ELSE 1.

        ERROR-STATUS:ERROR = NO.
        RUN setOutputFile.
        IF ERROR-STATUS:ERROR THEN RETURN.
        OUTPUT TO VALUE(v-out).
        RUN outputTagHeader.
        DO numberTags = 1 TO w-job.total-tags:
          RUN outputTagLine (w-job.tag-qty).
        END.
        OUTPUT CLOSE.

        FIND CURRENT wiptag EXCLUSIVE-LOCK NO-ERROR.

        ASSIGN
           wiptag.upd-date = TODAY
           wiptag.upd-time = TIME
           wiptag.upd-user = USERID("NOSWEAT")
           wiptag.tag-date = w-job.tag-date
           wiptag.tag-time = (INT(SUBSTRING(w-job.tag-timex,1,2)) * 3600) + (INT(SUBSTRING(w-job.tag-timex,4)) * 60)
           wiptag.partial  = w-job.partial
           wiptag.pallet-count = w-job.tag-qty
           wiptag.cons-uom = w-job.cons-uom
           wiptag.wip-warehouse  = w-job.wip-whs
           wiptag.wip-rm-bin     = w-job.wip-bin.

        FIND FIRST reftable WHERE
             reftable.reftable = "WIPLEN" AND
             reftable.company = wiptag.company AND
             reftable.CODE = wiptag.tag-no
             NO-ERROR.

        IF NOT AVAIL reftable THEN
        DO:
           CREATE reftable.
           ASSIGN
              reftable.reftable = "WIPLEN"
              reftable.company = wiptag.company
              reftable.CODE = wiptag.tag-no.
        END.

        ASSIGN
           reftable.val[1] = w-job.sht-wid 
           reftable.val[2] = w-job.sht-len.

        RELEASE reftable.

        IF w-job.first-mach-code NE "" THEN
        DO:
           FIND FIRST wiptag-mch WHERE
                wiptag-mch.company EQ wiptag.company AND
                wiptag-mch.tag-no  EQ wiptag.tag-no
                USE-INDEX seq-no
                NO-ERROR.

           IF NOT AVAIL wiptag-mch THEN
           DO:
              CREATE wiptag-mch.
              ASSIGN wiptag-mch.company = wiptag.company
                     wiptag-mch.tag-no  = wiptag.tag-no
                     wiptag-mch.seq-no  = 1.
           END.

           ASSIGN
              wiptag-mch.m-code  = w-job.first-mach-code
              wiptag-mch.produced-qty = wiptag.pallet-count.

           RELEASE wiptag-mch.
        END.

        RELEASE wiptag.

        MESSAGE 'WIP Tag Reprint Complete.' VIEW-AS ALERT-BOX.
        APPLY 'ENTRY':U TO reprintwiptagno IN FRAME {&FRAME-NAME}.
     END.
  END.
  ELSE
     MESSAGE "Job Material On Tag Not Found."
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR i AS INT NO-UNDO.
  DEF VAR j AS INT NO-UNDO.
  DEF VAR lv-job-no LIKE job.job-no NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR choice AS LOG NO-UNDO.

  DEF BUFFER b-item FOR item.
  DEF BUFFER b-w-job FOR w-job.
  DEF VAR lv-tag-no AS INT NO-UNDO.
  DEF VAR lv-how-many-tags AS INT NO-UNDO.
  DEF VAR v-b-wpo-created AS LOG NO-UNDO.
  DEF VAR v-sheets-tag AS INT NO-UNDO.
  DEF VAR v-total-wip-tags AS INT NO-UNDO.
  DEF VAR v-form-index AS INT NO-UNDO.
  DEF VAR ll-new-mat AS LOG NO-UNDO.
  DEF VAR lv-blank-no AS INT NO-UNDO.

  SESSION:SET-WAIT-STATE ("general").

  EMPTY TEMP-TABLE tt-tag.
  EMPTY TEMP-TABLE w-job.
  EMPTY TEMP-TABLE tt-job-mat.

  IF begin_job NE '' THEN
  DO:
     DO v-form-index = begin_job_form TO end_job_form:

        /*IF NOT CAN-FIND(FIRST job-mat WHERE
           job-mat.company = job.company AND
           job-mat.job = job.job AND
           job-mat.job-no = job.job-no AND
           job-mat.job-no2 = job.job-no2 AND
           job-mat.rm-i-no = begin_rm-i-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} AND /* loadtag.i-no */
           job-mat.frm = v-form-index) THEN
        DO:
              MESSAGE "Update item" begin_rm-i-no /* loadtag.i-no */ "on Job file?"
                 VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                 UPDATE ll-new-mat.

              IF ll-new-mat THEN
              DO:
                 RUN replace-job-mat(INPUT v-form-index).
                 IF NOT AVAIL tt-job-mat THEN
                    FIND tt-job-mat WHERE RECID(tt-job-mat) EQ fil_id NO-LOCK NO-ERROR.
                 ELSE
                    IF NOT AVAIL job-mat AND INDEX("MOXY789",ITEM.mat-type) EQ 0 THEN
                       DELETE tt-job-mat.

                 IF AVAIL tt-job-mat THEN
                    lv-blank-no = tt-job-mat.blank-no.

                 RELEASE job-mat.

                 FOR EACH tt-job-mat:
                    IF tt-job-mat.frm NE ? THEN DO:
                       FIND FIRST job-mat WHERE
                            ROWID(job-mat)  EQ tt-job-mat.row-id AND
                            job-mat.j-no    EQ 0
                            NO-ERROR.

                       ll = AVAIL job-mat.

                       IF ll THEN RUN jc/maydeletejob-mat.p (BUFFER job-mat, OUTPUT ll).

                       IF ll NE YES THEN CREATE job-mat.

                       BUFFER-COPY tt-job-mat EXCEPT rec_key TO job-mat
                       ASSIGN
                        job-mat.blank-no = lv-blank-no
                        job-mat.j-no     = 1
                        job-mat.qty-all  = job-mat.qty
                        job-mat.frm = v-form-index.
                    END. /* IF tt-job-mat.frm NE ? */
                    DELETE tt-job-mat.
                 END. /* FOR EACH tt-job-mat */
              END. /* IF ll-new-mat */
        END. /* IF NOT CAN-FIND(FIRST job-mat */  */
     END. /* DO v-form-index = begin_job_form TO end_job_form */



     ASSIGN
        lv-job-no = FILL(" ",6 - LENGTH(TRIM(begin_job))) + TRIM(begin_job)
        v-sheets-tag =  fi_exp-sheets-roll 
        v-total-wip-tags = fi_total-wip-tags.      

     FOR EACH job-mat WHERE
         job-mat.company EQ cocode AND
         job-mat.job-no EQ lv-job-no AND
         job-mat.job-no2 EQ begin_job2 AND
         job-mat.frm GE begin_job_form AND
         job-mat.frm LE end_job_form AND
         job-mat.blank-no GE begin_job_blank AND
         job-mat.blank-no LE end_job_blank AND
         job-mat.rm-i-no = begin_rm-i-no /* btr 02151105 */
         NO-LOCK,
         FIRST ITEM FIELDS(i-no i-name cons-uom loc loc-bin) WHERE
               ITEM.company EQ cocode AND
               ITEM.i-no EQ job-mat.rm-i-no AND
               ITEM.i-no GE begin_rm-i-no AND
               ITEM.i-no LE end_rm-i-no AND
               ITEM.mat-type EQ 'B'
               NO-LOCK,
         FIRST job-hdr FIELDS(cust-no i-no due-date est-no) WHERE
               job-hdr.company EQ job-mat.company AND
               job-hdr.job-no  EQ job-mat.job-no AND
               job-hdr.job-no2 EQ job-mat.job-no2 AND
               job-hdr.frm EQ job-mat.frm
               NO-LOCK:

               IF v-sheets-tag LE 0 OR
                  v-total-wip-tags LE 0 THEN
                  LEAVE.

               RUN create-w-job(INPUT-OUTPUT v-sheets-tag, /* expected shts */
                                INPUT-OUTPUT v-total-wip-tags). /* calcd tags */

               ASSIGN
                 w-job.last-mach-code = w-job.first-mach-code
                 w-job.last-prod-qty  = w-job.tag-qty.
     END. /* FOR EACH job-mat */ 
  END. /* IF begin_job NE '' */
  ELSE
  DO:
      MESSAGE "Job# Must Be Entered."  
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      LEAVE.
  END.
    EMPTY TEMP-TABLE w-file.

  RUN breakout-wiptags. /* take w-job tt and break into wiptags jobs */

  ASSIGN
    str-tit  = coname + " - " + loname
    str-tit2 = "DOWNLOAD WIPTAG DATA"
    x = (56 - LENGTH(str-tit)) / 2
    str-tit  = FILL(" ",x) + str-tit
    x = (56 - LENGTH(str-tit2)) / 2
    str-tit2 = FILL(" ",x) + str-tit2.

  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

  SESSION:SET-WAIT-STATE ("").

  IF CAN-FIND(FIRST w-job) THEN RUN jcrep/d-wiptg.w.
  ELSE DO:
      MESSAGE "An error occurred finding job information." skip
              "Check your data and try again."
          VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.   
  END.  /* btr */

  choice = NO.
  IF CAN-FIND(FIRST w-job) THEN
     MESSAGE "Are you Sure you Want to Create WIP Tag File? " 
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice.
  IF NOT choice THEN do:
      EMPTY TEMP-TABLE tt-used-tags NO-ERROR.
      ASSIGN opc-output = "No-WIP-Issued".
      RETURN ERROR.
  END.
  SESSION:SET-WAIT-STATE ("general").

  {sys/inc/print1.i}

  {sys/inc/outprint.i value(lines-per-page)} 

  VIEW FRAME r-top.
  VIEW FRAME top.

  RUN setOutputFile.

  OUTPUT TO VALUE(v-out).
  RUN outputTagHeader.

  FOR EACH w-job EXCLUSIVE-LOCK:

    IF tb_16ths THEN
       ASSIGN
          w-job.len = ROUND((w-job.len - TRUNC(w-job.len,0)) / 6.25,2) + TRUNC(w-job.len,0)
          w-job.wid = ROUND((w-job.wid - TRUNC(w-job.wid,0)) / 6.25,2) + TRUNC(w-job.wid,0)
          w-job.sht-len = ROUND((w-job.sht-len - TRUNC(w-job.sht-len,0)) / 6.25,2) + TRUNC(w-job.sht-len,0)
          w-job.sht-wid = ROUND((w-job.sht-wid - TRUNC(w-job.sht-wid,0)) / 6.25,2) + TRUNC(w-job.sht-wid,0).

    v-job = w-job.job-no + "-" + STRING(w-job.job-no2,"99").
    IF v-job BEGINS "-" THEN v-job = "".

    IF w-job.total-tags GT 0 AND w-job.partial = 0 THEN DO:
       DO j = 1 TO w-job.total-tags: /* wip tags generation */
         lv-how-many-tags = w-job.total-tags * v-mult - (IF w-job.partial NE 0 THEN 1 ELSE 0).
         DO i = 1 TO v-mult:
           IF i EQ 1 THEN RUN create-wiptag (j,w-job.tag-qty).
           RUN outputTagLine (w-job.tag-qty).
         END. /* do i */
       END. /* do j */
    END. /* IF w-job.total-tags GT 0 AND w-job.partial = 0 */

    ELSE IF w-job.total-tags GT 0 AND w-job.partial NE 0 THEN DO:
       DO i = 1 TO v-mult: /* for partial print */
         IF i EQ 1 THEN RUN create-wiptag (j,w-job.partial).
         RUN outputTagLine (w-job.tag-qty).
       END. /* do i */
    END. /* ELSE IF w-job.total-tags GT 0 AND w-job.partial NE 0 */

    DELETE w-job.
  END. /* each w-job */

  EMPTY TEMP-TABLE tt-rmtags NO-ERROR.
  EMPTY TEMP-TABLE tt-used-tags NO-ERROR.

  OUTPUT CLOSE.

  SESSION:SET-WAIT-STATE ("").

  IF vl-InputParameters THEN DO:
      MESSAGE "WIP Tags created. Returning to Issues."
          VIEW-AS ALERT-BOX INFO.
      APPLY "choose" TO btn-cancel.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setOutputFile C-Win 
PROCEDURE setOutputFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND FIRST sys-ctrl WHERE
     sys-ctrl.company EQ gcompany AND
     sys-ctrl.name    EQ "BARDIR"
     NO-LOCK NO-ERROR.

  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
     CREATE sys-ctrl.
     ASSIGN
        sys-ctrl.company  = gcompany
        sys-ctrl.name     = "BARDIR"
        sys-ctrl.descrip  = "C:\BA\Label\".
     FIND CURRENT sys-ctrl NO-LOCK.
  END.

  v-out = sys-ctrl.descrip.

  IF v-out = "" THEN v-out = "c:~\ba~\label~\wiptag.txt".
  ELSE DO:
     IF SUBSTRING(v-out,LENGTH(v-out),1) = "/" OR
        SUBSTRING(v-out,LENGTH(v-out),1) = "\" THEN .
     ELSE v-out = v-out + "/".
     v-out = v-out + "wiptag.txt".
  END.
  IF OPSYS EQ "UNIX" AND v-loadtag NE "TRIAD" THEN
  DO:
    MESSAGE "Unable to Create WIP Tag File for Non MSDOS Platform." VIEW-AS ALERT-BOX.
    RETURN ERROR.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE temp-create C-Win 
PROCEDURE temp-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

  CREATE w-file.
  w-file.w-key = ip-rowid.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-hour C-Win 
PROCEDURE valid-hour :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAMETER ip-hour AS CHAR.

   correct-error = INTEGER(ip-hour) LT 0 OR INTEGER(ip-hour) GT 12.
   IF correct-error THEN
      RETURN "Invalid Hour. Must be between 0 and 12... ".
   ELSE RETURN "". 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-min C-Win 
PROCEDURE valid-min :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAM ip-min AS cha NO-UNDO.

   correct-error = INTEGER(ip-min) LT 0 OR INTEGER(ip-min) GT 59.
   IF correct-error THEN
      RETURN "Invalid Hour. Must be between 0 and 59... ".
   ELSE RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validBeginJob C-Win 
PROCEDURE validBeginJob :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-min-blank AS INT INIT -1 NO-UNDO.
   DEF VAR v-min-form AS INT INIT -1 NO-UNDO.
   DEF VAR v-max-blank AS INT INIT 99 NO-UNDO.
   DEF VAR v-max-form AS INT INIT 999 NO-UNDO.
   DEF VAR v-exp-sheets AS DEC NO-UNDO.
   DEF VAR v-mat-length LIKE job-mat.len NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:

      SESSION:SET-WAIT-STATE("GENERAL").

      ASSIGN begin_job begin_job2.

      IF begin_job NE "" THEN DO:
        FIND FIRST job WHERE
             job.company EQ cocode AND
             job.job-no EQ begin_job AND
             job.job-no2 EQ begin_job2
             NO-LOCK NO-ERROR.

        IF AVAIL job THEN DO:
           ASSIGN v-mat-length = 0.
           IF NOT vl-InputParameters THEN
               FOR EACH job-mat FIELDS(blank-no frm len) WHERE
                   job-mat.company EQ cocode AND
                   job-mat.job EQ job.job AND
                   job-mat.job-no EQ job.job-no AND
                   job-mat.job-no2 EQ job.job-no2 
                   NO-LOCK:

                   IF v-min-blank EQ -1 THEN
                      ASSIGN
                         v-min-blank = job-mat.blank-no
                         v-min-form  = job-mat.frm
                         v-max-blank = job-mat.blank-no
                         v-max-form  = job-mat.frm.
                   ELSE DO:
                      IF job-mat.blank-no LT v-min-blank THEN
                         v-min-blank = job-mat.blank-no.

                      IF job-mat.frm LT v-min-form THEN
                         v-min-form = job-mat.frm.

                      IF job-mat.blank-no GT v-max-blank THEN
                         v-max-blank = job-mat.blank-no.

                      IF job-mat.frm GT v-max-form THEN
                         v-max-form = job-mat.frm.
                   END. /* else */

                   ASSIGN v-mat-length = v-mat-length + job-mat.len.
               END. /* for each */
           ELSE IF vl-InputParameters THEN
               FOR EACH job-mat FIELDS(blank-no frm len) WHERE
                   job-mat.company EQ cocode AND
                   job-mat.job EQ job.job AND
                   job-mat.job-no EQ job.job-no AND
                   job-mat.job-no2 EQ job.job-no2 AND
                   job-mat.i-no = begin_rm-i-no:SCREEN-VALUE
                   NO-LOCK:

                   ASSIGN v-mat-length = v-mat-length + job-mat.len.
               END. /* for each */

           IF v-mat-length = 0 THEN ASSIGN v-mat-length = 1.

           v-exp-sheets = INT(fi_oh-lf:SCREEN-VALUE) * 12 / v-mat-length.
           {sys\inc\roundup.i v-exp-sheets}

           fi_exp-sheets-roll:SCREEN-VALUE = STRING(v-exp-sheets).
        END. /* if avail job */

       IF v-min-blank NE -1 AND NOT vl-InputParameters THEN
          ASSIGN
             begin_job_blank:SCREEN-VALUE = STRING(v-min-blank)
             begin_job_form:SCREEN-VALUE  = STRING(v-min-form)
             end_job_blank:SCREEN-VALUE   = STRING(v-max-blank)
             end_job_form:SCREEN-VALUE    = STRING(v-max-form).
        APPLY "LEAVE" TO fi_pallet_height IN FRAME {&FRAME-NAME}.

        IF v-max-form GT 1 THEN 
           ASSIGN
              begin_job_form:SENSITIVE = YES
              end_job_form:SENSITIVE = YES .
       ELSE ASSIGN
              begin_job_form:SENSITIVE = NO
              end_job_form:SENSITIVE = NO .


        IF begin_job_form:SCREEN-VALUE <> "" AND 
             END_job_form:SCREEN-VALUE <> "" THEN DO:
              APPLY "value-changed" TO begin_job_form.
              APPLY "value-changed" TO END_job_form.
          END.

      END. /* if job no filled in */
     END. /* do with frame */

 SESSION:SET-WAIT-STATE("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validRMLoadTag C-Win 
PROCEDURE validRMLoadTag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO.

    DEF VAR v-min-blank AS INT INIT -1 NO-UNDO.
    DEF VAR v-min-form AS INT INIT -1 NO-UNDO.
    DEF VAR v-max-blank AS INT INIT 99 NO-UNDO.
    DEF VAR v-max-form AS INT INIT 999 NO-UNDO.
    DEF VAR v-log AS LOG NO-UNDO.

    DEF VAR vi-totalqty AS INT INIT 0 NO-UNDO.
    DEF VAR vc-job-no AS CHAR INIT "" NO-UNDO.
    DEF VAR vi-job-no2 AS INT INIT 0 NO-UNDO.
    DEF VAR vi-count AS INT INIT 0 NO-UNDO.
    DEF VAR vc-i-no AS CHAR FORMAT "x(15)" INIT "" NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:

       v-log = SESSION:SET-WAIT-STATE("GENERAL").

       ASSIGN fi_rmtagno
              fi_toRMTag
              ed_singletags.

       FOR EACH tt-rmtags:
           IF CAN-FIND(FIRST wiptag WHERE
              wiptag.company EQ cocode AND
              wiptag.rm-tag-no EQ tt-rmtags.rmtagno) THEN DO:
              MESSAGE "RM Loadtag " tt-rmtags.rmtagno SKIP
                      "has already been used on a WIP Tag." 
                  VIEW-AS ALERT-BOX ERROR.
              op-error = YES.
              RETURN NO-APPLY.
           END.
       END.

       FOR EACH tt-rmtags:
           IF tt-rmtags.rmtagno = ""  OR length(tt-rmtags.rmtagno) < 17 THEN do: 
               DELETE tt-rmtags.
               NEXT.
           END.

           FIND FIRST loadtag WHERE
                loadtag.company EQ cocode AND
                loadtag.item-type EQ YES AND
                loadtag.is-case-tag EQ NO AND
                loadtag.tag-no EQ tt-rmtags.rmtagno
                USE-INDEX tag
                NO-LOCK NO-ERROR.

           IF NOT AVAIL loadtag THEN DO:
              MESSAGE "RM Tag# " tt-rmtags.rmtagno skip
                      "Is not a valid RM Loadtag #, Please Try Again ..." VIEW-AS ALERT-BOX ERROR.
              RETURN NO-APPLY.
              /* APPLY "entry" TO FRAME {&FRAME-NAME}. */
           END.
       END.

       FOR EACH tt-rmtags:
          FIND FIRST loadtag WHERE
                     loadtag.company EQ cocode AND
                     loadtag.item-type EQ YES AND
                     loadtag.is-case-tag EQ NO AND
                     loadtag.tag-no EQ tt-rmtags.rmtagno
                      USE-INDEX tag
                      NO-LOCK NO-ERROR.
          IF AVAIL loadtag THEN DO:
            IF vl-InputParameters THEN DO:
                ASSIGN vc-job-no = begin_job:SCREEN-VALUE
                       vi-job-no2 = integer(begin_job2)
                       vc-i-no = begin_rm-i-no:SCREEN-VALUE.
                FIND FIRST rm-bin WHERE
                           rm-bin.company EQ cocode AND
                           rm-bin.i-no EQ loadtag.i-no AND
                           rm-bin.loc EQ loadtag.loc AND
                           rm-bin.loc-bin EQ loadtag.loc-bin AND
                           rm-bin.tag EQ loadtag.tag-no
                NO-LOCK NO-ERROR.

                IF AVAIL rm-bin THEN
                   ASSIGN vi-totalqty = vi-totalqty + rm-bin.qty
                          tt-rmtags.lfqty = rm-bin.qty
                          tt-rmtags.origshtqty = rm-bin.qty
                          tt-rmtags.availshtqty = rm-bin.qty.
                ELSE ASSIGN vi-totalqty = vi-totalqty + loadtag.qty
                            tt-rmtags.lfqty = loadtag.qty
                            tt-rmtags.origshtqty = loadtag.qty
                            tt-rmtags.availshtqty = loadtag.qty.
            END. /* IF vl-InputParameters THEN */
            ELSE DO:
                ASSIGN vc-job-no = loadtag.job-no
                       vi-job-no2 = loadtag.job-no2
                       vc-i-no = loadtag.i-no.
                FIND FIRST rm-bin WHERE
                           rm-bin.company EQ cocode AND
                           rm-bin.i-no EQ loadtag.i-no AND
                           rm-bin.loc EQ loadtag.loc AND
                           rm-bin.loc-bin EQ loadtag.loc-bin AND
                           rm-bin.tag EQ loadtag.tag-no
                NO-LOCK NO-ERROR.

                IF AVAIL rm-bin THEN
                   ASSIGN vi-totalqty = vi-totalqty + rm-bin.qty
                          tt-rmtags.lfqty = rm-bin.qty
                          tt-rmtags.origshtqty = rm-bin.qty
                          tt-rmtags.availshtqty = rm-bin.qty.
                ELSE ASSIGN vi-totalqty = vi-totalqty + loadtag.qty
                            tt-rmtags.lfqty = loadtag.qty
                            tt-rmtags.origshtqty = loadtag.qty
                            tt-rmtags.availshtqty = loadtag.qty.
            END. /* ELSE DO */

          END. /* if avail loadtag */

       END. /* for each tt-rmtags */

          ASSIGN
             begin_job:SCREEN-VALUE = vc-job-no
             begin_job2:SCREEN-VALUE = STRING(vi-job-no2)
             begin_rm-i-no:SCREEN-VALUE = vc-i-no
             end_rm-i-no:SCREEN-VALUE = vc-i-no
             fi_oh-lf:HIDDEN = NO
             fi_pallet_height:HIDDEN = NO
             fi_exp-sheets-roll:HIDDEN = NO
             fi_sheets-pallet:HIDDEN = NO
             fi_oh-lf:SENSITIVE = YES
             fi_pallet_height:SENSITIVE = YES
             fi_exp-sheets-roll:SENSITIVE = YES
             fi_sheets-pallet:SENSITIVE = YES.

          DISPLAY fi_oh-lf fi_pallet_height fi_exp-sheets-roll
                  fi_sheets-pallet WITH FRAME {&FRAME-NAME}.

          ASSIGN fi_oh-lf:SCREEN-VALUE = STRING(vi-totalqty).

          APPLY "LEAVE" TO fi_oh-lf IN FRAME {&FRAME-NAME}.

          IF vc-job-no NE "" THEN  /* loadtag.job-no */
          DO:
             FIND FIRST job WHERE
                  job.company EQ cocode AND
                  job.job-no EQ vc-job-no AND /* loadtag.job-no */
                  job.job-no2 EQ vi-job-no2   /* loadtag.job-no2 */
                  NO-LOCK NO-ERROR.

             IF AVAIL job THEN
             DO:
                FOR EACH job-mat FIELDS(blank-no frm) WHERE
                    job-mat.company EQ cocode AND
                    job-mat.job EQ job.job AND
                    job-mat.job-no EQ job.job-no AND
                    job-mat.job-no2 EQ job.job-no2 AND
                    job-mat.i-no = vc-i-no
                    NO-LOCK:

                    IF v-min-blank EQ -1 THEN
                       ASSIGN
                          v-min-blank = job-mat.blank-no
                          v-min-form  = job-mat.frm
                          v-max-blank = job-mat.blank-no
                          v-max-form  = job-mat.frm.
                    ELSE
                    DO:
                       IF job-mat.blank-no LT v-min-blank THEN
                          v-min-blank = job-mat.blank-no.

                       IF job-mat.frm LT v-min-form THEN
                          v-min-form = job-mat.frm.

                       IF job-mat.blank-no GT v-max-blank THEN
                          v-max-blank = job-mat.blank-no.

                       IF job-mat.frm GT v-max-form THEN
                          v-max-form = job-mat.frm.
                    END.
                END.


                IF v-min-blank NE -1 AND NOT vl-InputParameters THEN
                   ASSIGN
                      begin_job_blank:SCREEN-VALUE = STRING(v-min-blank)
                      begin_job_form:SCREEN-VALUE  = STRING(v-min-form)
                      end_job_blank:SCREEN-VALUE   = STRING(v-max-blank)
                      end_job_form:SCREEN-VALUE    = STRING(v-max-form).
                ELSE IF v-min-blank NE -1 AND vl-InputParameters THEN
                   ASSIGN
                      begin_job_blank:SCREEN-VALUE = STRING(ipi-blank)
                      begin_job_form:SCREEN-VALUE  = STRING(ipi-form)
                      end_job_blank:SCREEN-VALUE   = STRING(ipi-blank)
                      end_job_form:SCREEN-VALUE    = STRING(ipi-form).
             END.
          END. /* IF loadtag.job-no NE "" */

          IF v-max-form GT 1 THEN 
           ASSIGN
              begin_job_form:SENSITIVE = YES
              end_job_form:SENSITIVE = YES .
            ELSE ASSIGN
              begin_job_form:SENSITIVE = NO
              end_job_form:SENSITIVE = NO .

          IF begin_job_form:SCREEN-VALUE <> "" AND 
             END_job_form:SCREEN-VALUE <> "" THEN DO:
              APPLY "value-changed" TO begin_job_form.
              APPLY "value-changed" TO END_job_form.
          END.
  END.

  v-log = SESSION:SET-WAIT-STATE("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validWIPTag C-Win 
PROCEDURE validWIPTag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER op-error AS LOG NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

     ASSIGN reprintwiptagno.

     FIND FIRST wiptag WHERE
          wiptag.company EQ cocode AND
          wiptag.tag-no EQ reprintwiptagno
          NO-LOCK NO-ERROR.

     IF NOT AVAIL wiptag THEN DO:
        op-error = YES.
        MESSAGE 'Invalid WIP Tag, Please Try Again ...' VIEW-AS ALERT-BOX ERROR.
        APPLY 'ENTRY':U TO reprintwiptagno.
     END.
     ELSE
     DO:
        ASSIGN tg_wip-reported:SCREEN-VALUE = "YES"
               fi_tag-date:SCREEN-VALUE = STRING(wiptag.tag-date)
               fi_tag-hr:SCREEN-VALUE = SUBSTRING(STRING(wiptag.tag-time,"HH:MM AM"),1,2)
               fi_tag-min:SCREEN-VALUE = SUBSTRING(STRING(wiptag.tag-time,"HH:MM AM"),4,2)
               fi_tag-ampm:SCREEN-VALUE = SUBSTRING(STRING(wiptag.tag-time,"HH:MM AM"),7)
               fi_wip-whs:SCREEN-VALUE = wiptag.wip-warehouse
               fi_wip-bin:SCREEN-VALUE = wiptag.wip-rm-bin.

        FIND FIRST wiptag-mch WHERE
             wiptag-mch.company EQ wiptag.company AND
             wiptag-mch.tag-no EQ wiptag.tag-no
             USE-INDEX seq-no
             NO-LOCK NO-ERROR.

        IF AVAIL wiptag-mch THEN
        DO:
           begin_mach:SCREEN-VALUE = wiptag-mch.m-code.
           RELEASE wiptag-mch.
        END.

        RELEASE wiptag.
     END.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE wip-reported-proc C-Win 
PROCEDURE wip-reported-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-time AS INT NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:

      ASSIGN
         v-time = TIME
         fi_tag-date:SCREEN-VALUE = STRING(TODAY)
         fi_tag-hr:SCREEN-VALUE = SUBSTRING(STRING(v-time,"hh:mm am"),1,2)
         fi_tag-min:SCREEN-VALUE = SUBSTRING(STRING(v-time,"hh:mm am"),4,2)
         fi_tag-ampm:SCREEN-VALUE = SUBSTRING(STRING(v-time,"hh:mm am"),7,2)
         fi_wip-whs:SCREEN-VALUE = v-wipbin-whs
         fi_wip-bin:SCREEN-VALUE = v-wipbin-bin
         begin_mach:SCREEN-VALUE = "".

      IF begin_job:SCREEN-VALUE NE "" THEN
      DO:
         FIND FIRST job WHERE
              job.company EQ cocode AND
              job.job-no EQ begin_job:SCREEN-VALUE AND
              job.job-no2 EQ INT(begin_job2:SCREEN-VALUE)
              NO-LOCK NO-ERROR.

         IF AVAIL job THEN
            FIND FIRST job-mch WHERE
                 job-mch.company = job.company AND
                 job-mch.job = job.job AND
                 job-mch.job-no = job.job-no AND
                 job-mch.job-no2 = job.job-no2
                 use-index line-idx
                 NO-LOCK NO-ERROR.

         IF AVAIL job-mch THEN
            begin_mach:SCREEN-VALUE = job-mch.m-code.
      END.
      FIND FIRST loc WHERE loc.loc = "MAIN" 
         AND loc.company = cocode NO-LOCK NO-ERROR.
      IF AVAIL loc THEN DO:
         ASSIGN fi_wip-whs:SCREEN-VALUE = loc.loc .

      FIND FIRST wip-bin WHERE  wip-bin.company = cocode
          and wip-bin.loc = loc.loc NO-LOCK NO-ERROR.
       IF AVAIL wip-bin THEN
          ASSIGN fi_wip-bin:SCREEN-VALUE = wip-bin.loc-bin .

      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION checkWhsBin C-Win 
FUNCTION checkWhsBin RETURNS LOGICAL
  (ipCompany AS CHARACTER,ipLoc AS CHARACTER,ipLocBin AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN CAN-FIND(FIRST loc
                  WHERE loc.company EQ ipCompany
                    AND loc.loc EQ ipLoc) AND
         CAN-FIND(FIRST rm-bin
                  WHERE rm-bin.company EQ ipCompany
                    AND rm-bin.loc EQ ipLoc
                    AND rm-bin.i-no EQ ''
                    AND rm-bin.loc-bin EQ ipLocBin).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION removeChars C-Win 
FUNCTION removeChars RETURNS CHARACTER
  (ipField AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE invalidChars AS CHARACTER NO-UNDO INITIAL "~",#".
  DEFINE VARIABLE replaceChars AS CHARACTER NO-UNDO INITIAL "'',".
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE k AS INTEGER NO-UNDO.

  k = NUM-ENTRIES(invalidChars).
  DO i = 1 TO k:
    ipField = REPLACE(ipField,ENTRY(i,invalidChars),ENTRY(i,replaceChars)).
  END.
  RETURN ipField.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

