&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/<table>.w

  Description: from VIEWER.W - Template for SmartViewer Objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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
def var old-m-code like mach.m-code no-undo.

{custom/globdefs.i}

{sys/inc/var.i "new shared"}

assign
 cocode = g_company
 locode = g_loc.

&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF

DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.
def var lv-mach-recid as recid no-undo.
DEF VAR lv-label AS CHAR EXTENT 10 NO-UNDO.
def var lv-ind-list as cha init "Both,Folding,Corrugated,eXclude" no-undo.
DEF VAR ll-label AS LOG NO-UNDO.
DEF BUFFER mach-1 FOR mach.

&SCOPED-DEFINE where-plain-jobs                  ~
    WHERE reftable.reftable EQ "mach.plain-jobs" ~
      AND reftable.company  EQ mach.company      ~
      AND reftable.loc      EQ mach.loc          ~
      AND reftable.code     EQ mach.m-code


{est/d-sidsid.i}

/*{sys/inc/cepanel.i} - Use of CEPANEL Character value deprecated with ticket 17756*/

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
&Scoped-define EXTERNAL-TABLES mach
&Scoped-define FIRST-EXTERNAL-TABLE mach


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR mach.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS mach.m-dscr mach.sch-m-code mach.spare-int-2 ~
mach.loc mach.dept[1] mach.dept[2] mach.dept[3] mach.dept[4] mach.m-seq ~
mach.p-type mach.run-spoil mach.mr-waste mach.daily-prod-hours mach.therm ~
mach.gang-jobs mach.lab-rate[1] mach.lab-rate[2] mach.lab-rate[3] ~
mach.lab-drate mach.mrk-rate mach.mr-crusiz mach.run-crusiz mach.mr-varoh ~
mach.mr-fixoh mach.mr-trate mach.run-varoh mach.run-fixoh mach.run-trate ~
mach.min-len mach.max-len mach.min-trimw mach.min-wid mach.max-wid ~
mach.min-triml mach.min-cal mach.max-cal mach.min-pan-l mach.max-pan-l ~
mach.min-pan-w mach.max-pan-w mach.min_pan_lw mach.max_pan_lw ~
mach.min_slot_score mach.max_slot_score mach.min_hd_hd mach.max_hd_hd ~
mach.min-dep mach.max-dep mach.min-run mach.max-run mach.tan-mrp ~
mach.num-wid mach.num-len mach.spare-int-1 mach.machineImage[1] 
&Scoped-define ENABLED-TABLES mach
&Scoped-define FIRST-ENABLED-TABLE mach
&Scoped-Define DISPLAYED-FIELDS mach.m-code mach.m-dscr mach.sch-m-code ~
mach.spare-int-2 mach.loc mach.dept[1] mach.dept[2] mach.dept[3] ~
mach.dept[4] mach.d-seq mach.m-seq mach.p-type mach.run-spoil mach.mr-waste ~
mach.daily-prod-hours mach.therm mach.gang-jobs mach.lab-rate[1] ~
mach.lab-rate[2] mach.lab-rate[3] mach.lab-drate mach.mrk-rate ~
mach.mr-crusiz mach.run-crusiz mach.mr-rate mach.mr-varoh mach.mr-fixoh ~
mach.mr-trate mach.run-rate mach.run-varoh mach.run-fixoh mach.run-trate ~
mach.min-len mach.max-len mach.min-trimw mach.min-wid mach.max-wid ~
mach.min-triml mach.min-cal mach.max-cal mach.min-pan-l mach.max-pan-l ~
mach.min-pan-w mach.max-pan-w mach.min_pan_lw mach.max_pan_lw ~
mach.min_slot_score mach.max_slot_score mach.min_hd_hd mach.max_hd_hd ~
mach.min-dep mach.max-dep mach.min-run mach.max-run mach.pr-type ~
mach.washup mach.col-pass mach.max-color mach.coater mach.col-wastesh ~
mach.ink-waste mach.col-wastelb mach.tan-mrp mach.tan-mrf mach.num-wid ~
mach.num-len mach.spare-int-1 mach.machineImage[1] 
&Scoped-define DISPLAYED-TABLES mach
&Scoped-define FIRST-DISPLAYED-TABLE mach
&Scoped-Define DISPLAYED-OBJECTS cb_industry tb_plain-jobs tb_obsolete ~
ls-limit-lbl 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS mach.m-code 
&Scoped-define ADM-ASSIGN-FIELDS mach.d-seq cb_industry tb_plain-jobs ~
tb_obsolete mach.mr-rate mach.run-rate mach.pr-type mach.washup ~
mach.col-pass mach.max-color mach.coater mach.col-wastesh mach.ink-waste ~
mach.col-wastelb mach.tan-mrp mach.tan-mrf 

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
DEFINE VARIABLE cb_industry AS CHARACTER FORMAT "x(10)" 
     LABEL "Industry" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Both","Folding","Corrugated","eXclude" 
     DROP-DOWN-LIST
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE ls-limit-lbl AS CHARACTER FORMAT "X(256)":U INITIAL "LIMITS" 
      VIEW-AS TEXT 
     SIZE 9 BY .62
     FGCOLOR 9  NO-UNDO.

DEFINE IMAGE cMachImage
     FILENAME "adeicon/blank":U TRANSPARENT
     SIZE 11 BY 2.62.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 146 BY 4.29
     FGCOLOR 3 .

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 71 BY 9.29.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 75 BY 6.19.

DEFINE VARIABLE tb_obsolete AS LOGICAL INITIAL no 
     LABEL "Obsolete?" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE tb_plain-jobs AS LOGICAL INITIAL no 
     LABEL "Plain Jobs Only?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     mach.m-code AT ROW 1.24 COL 18 COLON-ALIGNED
          LABEL "Machine Code" FORMAT "x(6)"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     mach.m-dscr AT ROW 1.24 COL 45 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 47 BY 1
     mach.sch-m-code AT ROW 1.24 COL 116 COLON-ALIGNED
          LABEL "Schedule Machine"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     mach.spare-int-2 AT ROW 1.24 COL 138 COLON-ALIGNED HELP
          "Enter Machine DMI ID" WIDGET-ID 16
          LABEL "DMI ID" FORMAT "999"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1 TOOLTIP "Machine DMI ID"
     mach.loc AT ROW 2.19 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     mach.dept[1] AT ROW 2.19 COL 45 COLON-ALIGNED
          LABEL "Department" FORMAT "x(2)"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     mach.dept[2] AT ROW 2.19 COL 51 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     mach.dept[3] AT ROW 2.19 COL 57 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     mach.dept[4] AT ROW 2.19 COL 63 COLON-ALIGNED NO-LABEL FORMAT "x(2)"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     mach.d-seq AT ROW 2.19 COL 94 COLON-ALIGNED
          LABEL "Sequence"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     mach.m-seq AT ROW 2.19 COL 100 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     cb_industry AT ROW 2.19 COL 116 COLON-ALIGNED HELP
          "Enter whether machine is used for 1=Fold 2=Corr 3=Foam"
     mach.p-type AT ROW 3.14 COL 18 COLON-ALIGNED HELP
          "Enter (R)oll, (S)heet, (B)lank, (Parts) Fed or (A)ssemble Sets"
          LABEL "Feed"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "R- Roll","R",
                     "S- Sheet","S",
                     "B- Blank","B",
                     "P- Partition","P",
                     "A- Assembly","A"
          DROP-DOWN-LIST
          SIZE 19 BY 1
     mach.run-spoil AT ROW 3.14 COL 60 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     mach.mr-waste AT ROW 3.14 COL 94 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.8 BY 1
     tb_plain-jobs AT ROW 3.14 COL 118
     tb_obsolete AT ROW 4.1 COL 20
     mach.daily-prod-hours AT ROW 4.1 COL 55 COLON-ALIGNED
          LABEL "Lag Time"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mach.therm AT ROW 4.1 COL 75
          LABEL "Use Lineal Feet in RUN Matrix?"
          VIEW-AS TOGGLE-BOX
          SIZE 41 BY 1
     mach.gang-jobs AT ROW 4.1 COL 118
          VIEW-AS TOGGLE-BOX
          SIZE 19 BY 1
     mach.lab-rate[1] AT ROW 5.52 COL 18 COLON-ALIGNED
          LABEL "Labor Rate1" FORMAT ">,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     mach.lab-rate[2] AT ROW 6.48 COL 18 COLON-ALIGNED
          LABEL "Rate2" FORMAT ">,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     mach.lab-rate[3] AT ROW 7.43 COL 18 COLON-ALIGNED
          LABEL "Rate3" FORMAT ">,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     mach.lab-drate AT ROW 8.38 COL 18 COLON-ALIGNED
          LABEL "Default"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "1","2","3" 
          DROP-DOWN-LIST
          SIZE 12 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     mach.mrk-rate AT ROW 5.52 COL 56 COLON-ALIGNED
          LABEL "Min Charge" FORMAT ">,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     mach.mr-crusiz AT ROW 6.48 COL 56 COLON-ALIGNED
          LABEL "Setup Crew"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     mach.run-crusiz AT ROW 7.43 COL 56 COLON-ALIGNED
          LABEL "Run Crew"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     mach.mr-rate AT ROW 5.52 COL 99 COLON-ALIGNED
          LABEL "Setup D.L." FORMAT ">,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     mach.mr-varoh AT ROW 6.48 COL 99 COLON-ALIGNED
          LABEL "Var OH" FORMAT ">,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     mach.mr-fixoh AT ROW 7.43 COL 99 COLON-ALIGNED
          LABEL "Fixed OH" FORMAT ">,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     mach.mr-trate AT ROW 8.38 COL 99 COLON-ALIGNED
          LABEL "Total" FORMAT ">,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     mach.run-rate AT ROW 5.52 COL 132 COLON-ALIGNED
          LABEL "Run D.L" FORMAT ">,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     mach.run-varoh AT ROW 6.48 COL 132 COLON-ALIGNED
          LABEL "Var OH" FORMAT ">,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     mach.run-fixoh AT ROW 7.43 COL 132 COLON-ALIGNED
          LABEL "Fixed OH" FORMAT ">,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     mach.run-trate AT ROW 8.38 COL 132 COLON-ALIGNED
          LABEL "Total" FORMAT ">,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     mach.min-len AT ROW 10.38 COL 22 COLON-ALIGNED
          LABEL "Front-To-Back"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
     mach.max-len AT ROW 10.38 COL 40 COLON-ALIGNED NO-LABEL FORMAT ">>9.99999"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
     mach.min-trimw AT ROW 10.38 COL 56 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12.8 BY .81
     mach.min-wid AT ROW 11.19 COL 22 COLON-ALIGNED
          LABEL "Side-To-Side"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
     mach.max-wid AT ROW 11.19 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
     mach.min-triml AT ROW 11.19 COL 56 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12.8 BY .81
     mach.min-cal AT ROW 12 COL 22 COLON-ALIGNED
          LABEL "Caliper/Depth" FORMAT ">>9.99999"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
     mach.max-cal AT ROW 12 COL 40 COLON-ALIGNED NO-LABEL FORMAT ">>9.99999"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
     mach.min-pan-l AT ROW 12.81 COL 22 COLON-ALIGNED
          LABEL "Box Length"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
     mach.max-pan-l AT ROW 12.81 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
     mach.min-pan-w AT ROW 13.62 COL 22 COLON-ALIGNED
          LABEL "Box Width"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     mach.max-pan-w AT ROW 13.62 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
     mach.min_pan_lw AT ROW 14.43 COL 22 COLON-ALIGNED WIDGET-ID 12
          LABEL "Box L + W"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
     mach.max_pan_lw AT ROW 14.43 COL 40 COLON-ALIGNED NO-LABEL WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
     mach.min_slot_score AT ROW 15.24 COL 22 COLON-ALIGNED WIDGET-ID 14
          LABEL "Slot/Score Panel"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
     mach.max_slot_score AT ROW 15.24 COL 40 COLON-ALIGNED NO-LABEL WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
     mach.min_hd_hd AT ROW 16.05 COL 22 COLON-ALIGNED WIDGET-ID 10
          LABEL "Panel (Hd-Hd)"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
     mach.max_hd_hd AT ROW 16.05 COL 40 COLON-ALIGNED NO-LABEL WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
     mach.min-dep AT ROW 16.86 COL 22 COLON-ALIGNED
          LABEL "Slot Size"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
     mach.max-dep AT ROW 16.86 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
     mach.min-run AT ROW 17.67 COL 22 COLON-ALIGNED
          LABEL "Run Qty" FORMAT ">>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
     mach.max-run AT ROW 17.67 COL 40 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
     mach.pr-type AT ROW 9.81 COL 105 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     mach.washup AT ROW 10.76 COL 105 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     mach.col-pass AT ROW 10.76 COL 135 COLON-ALIGNED
          LABEL "Color/Pass"
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     mach.max-color AT ROW 11.71 COL 105 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     mach.coater AT ROW 11.71 COL 135 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     mach.col-wastesh AT ROW 12.67 COL 105 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.8 BY 1
     mach.ink-waste AT ROW 13.62 COL 105 COLON-ALIGNED
          LABEL "Ink Waste Lbs/MR"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mach.col-wastelb AT ROW 13.62 COL 135 COLON-ALIGNED
          LABEL "Lbs/Color"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mach.tan-mrp AT ROW 14.57 COL 105 COLON-ALIGNED
          LABEL "Tandem MR/Plate"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     mach.tan-mrf AT ROW 14.57 COL 135 COLON-ALIGNED
          LABEL "/Fountain"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     ls-limit-lbl AT ROW 9.76 COL 9 COLON-ALIGNED NO-LABEL
     mach.num-wid AT ROW 16 COL 105 COLON-ALIGNED
          LABEL "Max Num  Width"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     mach.num-len AT ROW 16 COL 135 COLON-ALIGNED
          LABEL "Max Num Length"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     mach.spare-int-1 AT ROW 8.38 COL 56 COLON-ALIGNED HELP
          "Enter Kicks Per Hour" WIDGET-ID 2
          LABEL "Kicks/Hr" FORMAT ">>,>>9"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     mach.machineImage[1] AT ROW 17.67 COL 92 COLON-ALIGNED WIDGET-ID 120
          LABEL "Image" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 53 BY 1
     "Printing Press" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 9.81 COL 73
          FGCOLOR 9 
     "R A T E S" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 5.05 COL 72
          FGCOLOR 9 
     "Max" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 9.76 COL 46
          FGCOLOR 9 
     "Min" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 9.76 COL 28
          FGCOLOR 9 
     "Trim" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 9.76 COL 61
          FGCOLOR 9 
     RECT-10 AT ROW 5.29 COL 1
     RECT-11 AT ROW 9.57 COL 1
     RECT-12 AT ROW 9.57 COL 72
     cMachImage AT ROW 16 COL 73 WIDGET-ID 118
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.mach
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY
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
         HEIGHT             = 17.86
         WIDTH              = 146.4.
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

/* SETTINGS FOR COMBO-BOX cb_industry IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR IMAGE cMachImage IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach.coater IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN mach.col-pass IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN mach.col-wastelb IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN mach.col-wastesh IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN mach.d-seq IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN mach.daily-prod-hours IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach.dept[1] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN mach.dept[4] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mach.ink-waste IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR COMBO-BOX mach.lab-drate IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach.lab-rate[1] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN mach.lab-rate[2] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN mach.lab-rate[3] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ls-limit-lbl IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach.m-code IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN mach.machineImage[1] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN mach.max-cal IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mach.max-color IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN mach.max-dep IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach.max-len IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mach.max-run IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mach.min-cal IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN mach.min-dep IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach.min-len IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach.min-pan-l IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach.min-pan-w IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach.min-run IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN mach.min-triml IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach.min-trimw IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach.min-wid IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach.min_hd_hd IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach.min_pan_lw IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach.min_slot_score IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach.mr-crusiz IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach.mr-fixoh IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN mach.mr-rate IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN mach.mr-trate IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN mach.mr-varoh IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN mach.mrk-rate IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN mach.num-len IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach.num-wid IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX mach.p-type IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN mach.pr-type IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR RECTANGLE RECT-10 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-11 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-12 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach.run-crusiz IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach.run-fixoh IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN mach.run-rate IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN mach.run-trate IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN mach.run-varoh IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN mach.sch-m-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach.spare-int-1 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN mach.spare-int-2 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN mach.tan-mrf IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN mach.tan-mrp IN FRAME F-Main
   2 EXP-LABEL                                                          */
/* SETTINGS FOR TOGGLE-BOX tb_obsolete IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR TOGGLE-BOX tb_plain-jobs IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR TOGGLE-BOX mach.therm IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mach.washup IN FRAME F-Main
   NO-ENABLE 2                                                          */
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

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
   def var char-val as cha no-undo.
   def var lv-handle as handle no-undo.


   g_lookup-var = "".

   case focus:name :
        when "loc" then do:
             run lookups/loc.p.
             if g_lookup-var <> "" then do:
                focus:screen-value = g_lookup-var.
             end.
        end.
        when "fi_sch-m-code" then do:
             run windows/l-machin.w (mach.company, mach.loc, mach.industry, focus:screen-value, output char-val).
             if char-val <> "" then focus:screen-value = ENTRY(1,char-val).
        end.
        when "dept" then do:
             lv-handle = focus.
             run lookups/dept.p.
             if g_lookup-var <> "" then do:
                case lv-handle:index:
                     when 1 then do:
                         assign mach.dept[1]:screen-value = g_lookup-var.
                         find first dept where dept.code = g_lookup-var and
                                               (dept.setup <> 99 or
                                               dept.fc <> 99 or
                                               dept.corr <> 99 or
                                               dept.therm <> 99)
                                               no-lock no-error.
                         if avail dept then do:
                            if mach.setup then mach.d-seq:screen-value = string(dept.setup).
                            else if mach.fc then mach.d-seq:screen-value = string(dept.fc).
                            else if mach.corr then mach.d-seq:screen-value = string(dept.corr).
                            else if mach.therm then mach.d-seq:screen-value = string(dept.therm).
                         end.                      
                     end.     
                     when 2 then mach.dept[2]:screen-value = g_lookup-var.
                     when 3 then mach.dept[3]:screen-value = g_lookup-var.
                     when 4 then mach.dept[4]:screen-value = g_lookup-var.
                end.
             end.
        end.
        otherwise do:
           lv-handle = focus:handle.
           run applhelp.p.

           if g_lookup-var <> "" then do:
              lv-handle:screen-value = g_lookup-var.
           end.  
        end.
   end case.

   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mach.dept[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach.dept[1] V-table-Win
ON LEAVE OF mach.dept[1] IN FRAME F-Main /* Department */
DO: 
  if lastkey ne -1 then do:
  {&methods/lValidateError.i YES}
    if self:screen-value ne "" and
       not can-find(dept where dept.code eq self:screen-value) then do:
      message "Invalid entry, try help..." view-as alert-box error.
      return no-apply.
    end.
    {&methods/lValidateError.i NO}
  end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach.dept[1] V-table-Win
ON VALUE-CHANGED OF mach.dept[1] IN FRAME F-Main /* Department */
DO:
  DEF BUFFER bf-mach FOR mach.

  DEF VAR v-seq LIKE mach.m-seq NO-UNDO.


  RUN dept-display.

  RUN dept-enable. 

  FIND FIRST dept WHERE dept.code EQ {&self-name}:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAIL dept THEN DO:
    mach.d-seq:SCREEN-VALUE = STRING(dept.fc).    
  END.

  IF adm-new-record THEN
  FOR EACH bf-mach
      WHERE bf-mach.company EQ cocode
        AND bf-mach.d-seq   EQ INT(mach.d-seq:SCREEN-VALUE)
      NO-LOCK
      BY bf-mach.m-seq:

    IF bf-mach.m-seq - v-seq GT 1 THEN DO:
      mach.m-seq:SCREEN-VALUE = STRING(v-seq + 1).
      LEAVE.
    END.

    v-seq = bf-mach.m-seq.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mach.dept[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach.dept[2] V-table-Win
ON LEAVE OF mach.dept[2] IN FRAME F-Main /* Dept[2] */
DO:
     {&methods/lValidateError.i YES}
     if lastkey <> -1 and self:screen-value <> "" and
      not can-find(dept where dept.code = self:screen-value)
      then do:
         message "Invalid Deptartment Code. Try Help." view-as alert-box error.
         return no-apply.
      end.


     disable mach.pr-type mach.washup mach.col-pass max-color coater col-wastesh
                mach.ink-waste mach.col-wastelb   
                /*mach.tan-mrp*/ mach.tan-mrf with frame {&frame-name}.

     if self:screen-value = "GL" 
           or mach.dept[1]:screen-value = "GL" or
             mach.dept[3]:screen-value = "GL"
             or mach.dept[4]:screen-value = "GL"
     then enable /*mach.tan-mrp*/ mach.tan-mrf with frame {&frame-name}.
     else if self:screen-value = "PR" or mach.dept[1]:screen-value = "PR" or
             mach.dept[3]:screen-value = "PR"
             or mach.dept[4]:screen-value = "PR"
     then
         enable mach.pr-type mach.washup mach.col-pass max-color coater col-wastesh
                mach.ink-waste mach.col-wastelb   
                /*mach.tan-mrp*/ mach.tan-mrf with frame {&frame-name}.
     else assign mach.pr-type:screen-value = ""
               mach.washup:screen-value = "0"
               mach.col-pass:screen-value = ?
               mach.max-color:screen-value = "0"
               mach.coater:screen-value = ?
               mach.col-wastesh:screen-value = "0"
               mach.ink-waste:screen-value = "0"
               mach.col-wastelb:screen-value = "0"
               .
      {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mach.dept[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach.dept[3] V-table-Win
ON LEAVE OF mach.dept[3] IN FRAME F-Main /* Dept[3] */
DO:
     {&methods/lValidateError.i YES}
     if lastkey <> -1 and self:screen-value <> "" and
      not can-find(dept where dept.code = self:screen-value)
      then do:
         message "Invalid Deptartment Code. Try Help." view-as alert-box error.
         return no-apply.
      end.


     disable mach.pr-type mach.washup mach.col-pass max-color coater col-wastesh
                mach.ink-waste mach.col-wastelb   
                /*mach.tan-mrp*/ mach.tan-mrf with frame {&frame-name}.

     if self:screen-value = "GL" 
           or mach.dept[2]:screen-value = "GL" or
             mach.dept[1]:screen-value = "GL"
             or mach.dept[4]:screen-value = "GL"
     then enable /*mach.tan-mrp*/ mach.tan-mrf with frame {&frame-name}.
     else if self:screen-value = "PR" 
             or mach.dept[1]:screen-value = "PR" or
             mach.dept[2]:screen-value = "PR"
             or mach.dept[4]:screen-value = "PR"

     then
         enable mach.pr-type mach.washup mach.col-pass max-color coater col-wastesh
                mach.ink-waste mach.col-wastelb   
                /*mach.tan-mrp*/ mach.tan-mrf with frame {&frame-name}.
     else assign mach.pr-type:screen-value = ""
               mach.washup:screen-value = "0"
               mach.col-pass:screen-value = ?
               mach.max-color:screen-value = "0"
               mach.coater:screen-value = ?
               mach.col-wastesh:screen-value = "0"
               mach.ink-waste:screen-value = "0"
               mach.col-wastelb:screen-value = "0"
               .
      {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mach.dept[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach.dept[4] V-table-Win
ON LEAVE OF mach.dept[4] IN FRAME F-Main /* Dept[4] */
DO:
     {&methods/lValidateError.i YES}
     if lastkey <> -1 and self:screen-value <> "" and
      not can-find(dept where dept.code = self:screen-value)
      then do:
         message "Invalid Deptartment Code. Try Help." view-as alert-box error.
         return no-apply.
      end.


     disable mach.pr-type mach.washup mach.col-pass max-color coater col-wastesh
                mach.ink-waste mach.col-wastelb   
                /*mach.tan-mrp*/ mach.tan-mrf with frame {&frame-name}.

     if self:screen-value = "GL" 
           or mach.dept[2]:screen-value = "GL" or
             mach.dept[3]:screen-value = "GL"
             or mach.dept[1]:screen-value = "GL"
     then enable /*mach.tan-mrp*/ mach.tan-mrf with frame {&frame-name}.
     else if self:screen-value = "PR" 
     or mach.dept[1]:screen-value = "PR" or
             mach.dept[3]:screen-value = "PR"
             or mach.dept[2]:screen-value = "PR"

     then
         enable mach.pr-type mach.washup mach.col-pass max-color coater col-wastesh
                mach.ink-waste mach.col-wastelb   
                /*mach.tan-mrp*/ mach.tan-mrf with frame {&frame-name}.
     else assign mach.pr-type:screen-value = ""
               mach.washup:screen-value = "0"
               mach.col-pass:screen-value = ?
               mach.max-color:screen-value = "0"
               mach.coater:screen-value = ?
               mach.col-wastesh:screen-value = "0"
               mach.ink-waste:screen-value = "0"
               mach.col-wastelb:screen-value = "0"
               .
      {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mach.lab-drate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach.lab-drate V-table-Win
ON VALUE-CHANGED OF mach.lab-drate IN FRAME F-Main /* Default */
DO:
      run calc-rate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mach.lab-rate[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach.lab-rate[1] V-table-Win
ON LEAVE OF mach.lab-rate[1] IN FRAME F-Main /* Labor Rate1 */
DO:
  /*assign mach.mr-rate:screen-value = string(
                                  decimal(mach.mr-crusiz:screen-value) *
                                  decimal(mach.lab-rate[int(mach.lab-drate:screen-value)])
                                  )
          mach.run-rate:screen-value = string(
                                  decimal(mach.run-crusiz:screen-value) *
                                  decimal(mach.lab-rate[int(mach.lab-drate:screen-value)])
                                  ).


  assign mach.mr-trate:screen-value = string(
                                  decimal(mach.mr-rate:screen-value) + 
                                  decimal(mach.mr-varoh:screen-value) +
                                  decimal(mach.mr-fixoh:screen-value)
                                  )
                                  .
  assign mach.run-trate:screen-value = string(
                                  decimal(mach.run-rate:screen-value) + 
                                  decimal(mach.run-varoh:screen-value) +
                                  decimal(mach.run-fixoh:screen-value)
                                  )
                                  .
  */ 
  run calc-rate.                              

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mach.lab-rate[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach.lab-rate[2] V-table-Win
ON LEAVE OF mach.lab-rate[2] IN FRAME F-Main /* Rate2 */
DO:
  /*   assign mach.mr-rate:screen-value = string(
                                  decimal(mach.mr-crusiz:screen-value) *
                                  decimal(mach.lab-rate[int(mach.lab-drate:screen-value)])
                                  )
          mach.run-rate:screen-value = string(
                                  decimal(mach.run-crusiz:screen-value) *
                                  decimal(mach.lab-rate[int(mach.lab-drate:screen-value)])
                                  ).
  assign mach.mr-trate:screen-value = string(
                                  decimal(mach.mr-rate:screen-value) + 
                                  decimal(mach.mr-varoh:screen-value) +
                                  decimal(mach.mr-fixoh:screen-value)
                                  )
                                  .
  assign mach.run-trate:screen-value = string(
                                  decimal(mach.run-rate:screen-value) + 
                                  decimal(mach.run-varoh:screen-value) +
                                  decimal(mach.run-fixoh:screen-value)
                                  )
                                  .

 */
   run calc-rate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mach.lab-rate[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach.lab-rate[3] V-table-Win
ON LEAVE OF mach.lab-rate[3] IN FRAME F-Main /* Rate3 */
DO:
  run calc-rate.                              

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mach.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach.loc V-table-Win
ON LEAVE OF mach.loc IN FRAME F-Main /* Location */
DO:
     {&methods/lValidateError.i YES}
     if lastkey <> -1 and self:screen-value <> "" and
      not can-find(loc where loc.company = cocode and loc.loc = self:screen-value)
      then do:
         message "Invalid Loacation. Try Help." view-as alert-box error.
         return no-apply.
      end.
      {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mach.m-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach.m-code V-table-Win
ON ENTRY OF mach.m-code IN FRAME F-Main /* Machine Code */
DO:
   old-m-code = mach.m-code.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach.m-code V-table-Win
ON LEAVE OF mach.m-code IN FRAME F-Main /* Machine Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-m-code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mach.m-seq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach.m-seq V-table-Win
ON LEAVE OF mach.m-seq IN FRAME F-Main /* M Seq */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-m-seq NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.                        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mach.machineImage[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach.machineImage[1] V-table-Win
ON HELP OF mach.machineImage[1] IN FRAME F-Main /* Image */
DO:
    DEFINE VARIABLE cImageFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cInitDir   AS CHARACTER NO-UNDO INITIAL ".\".
    DEFINE VARIABLE lOK        AS LOGICAL   NO-UNDO.

    SYSTEM-DIALOG GET-FILE cImageFile 
        TITLE "Select Image File"
        FILTERS "PNG Files    (*.png)" "*.png",
                "Bitmap files (*.bmp)" "*.bmp",
                "ICO Files    (*.ico)" "*.ico",
                "JPG Files    (*.jpg)" "*.jpg",                 
                "JPEG Files   (*.jpeg)" "*.jpeg",
                "All Files    (*.*) " "*.*"
        INITIAL-DIR cInitDir
        MUST-EXIST
        USE-FILENAME
        UPDATE lOK
        .
    IF lOK THEN DO:
        cMachImage:LOAD-IMAGE(cImageFile).
        SELF:SCREEN-VALUE = REPLACE(cImageFile,cInitDir,"").
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach.machineImage[1] V-table-Win
ON LEAVE OF mach.machineImage[1] IN FRAME F-Main /* Image */
DO:
    cMachImage:LOAD-IMAGE(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mach.max-wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach.max-wid V-table-Win
ON ENTRY OF mach.max-wid IN FRAME F-Main /* Max Width */
DO:
  {&methods/lValidateError.i YES}
  IF mach.min-wid:LABEL EQ lv-label[1] THEN DO:
    RUN est/d-sidsid.w (mach.m-code:SCREEN-VALUE, "Cylinder Diameters",
                        INPUT-OUTPUT TABLE tt-ss).

    FOR FIRST tt-ss BY tt-seq:
      {&self-name}:SCREEN-VALUE = STRING(tt-val).
    END.

    APPLY "leave" TO {&self-name}.
    RETURN NO-APPLY.
  END.
  {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mach.mr-crusiz
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach.mr-crusiz V-table-Win
ON ENTRY OF mach.mr-crusiz IN FRAME F-Main /* Setup Crew */
DO:
  {viewers/machcrew.i "M R"}

  APPLY "tab" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach.mr-crusiz V-table-Win
ON LEAVE OF mach.mr-crusiz IN FRAME F-Main /* Setup Crew */
DO:
  RUN calc-rate.                                  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mach.mr-fixoh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach.mr-fixoh V-table-Win
ON LEAVE OF mach.mr-fixoh IN FRAME F-Main /* Fixed OH */
DO:
       assign mach.mr-trate:screen-value = string(
                                  decimal(mach.mr-rate:screen-value) + 
                                  decimal(mach.mr-varoh:screen-value) +
                                  decimal(mach.mr-fixoh:screen-value)
                                  )
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mach.mr-trate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach.mr-trate V-table-Win
ON ENTRY OF mach.mr-trate IN FRAME F-Main /* Total */
DO:
     assign mach.mr-trate:screen-value = string(
                                  decimal(mach.mr-rate:screen-value) + 
                                  decimal(mach.mr-varoh:screen-value) +
                                  decimal(mach.mr-fixoh:screen-value)
                                  )
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mach.mr-varoh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach.mr-varoh V-table-Win
ON LEAVE OF mach.mr-varoh IN FRAME F-Main /* Var OH */
DO:
   assign mach.mr-trate:screen-value = string(
                                  decimal(mach.mr-rate:screen-value) + 
                                  decimal(mach.mr-varoh:screen-value) +
                                  decimal(mach.mr-fixoh:screen-value)
                                  )
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mach.p-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach.p-type V-table-Win
ON VALUE-CHANGED OF mach.p-type IN FRAME F-Main /* Feed */
DO:
  RUN p-type-display.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mach.run-crusiz
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach.run-crusiz V-table-Win
ON ENTRY OF mach.run-crusiz IN FRAME F-Main /* Run Crew */
DO:
  {viewers/machcrew.i "RUN"}

  APPLY "tab" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach.run-crusiz V-table-Win
ON LEAVE OF mach.run-crusiz IN FRAME F-Main /* Run Crew */
DO:
      run calc-rate.                                  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mach.run-fixoh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach.run-fixoh V-table-Win
ON LEAVE OF mach.run-fixoh IN FRAME F-Main /* Fixed OH */
DO:
       assign mach.run-trate:screen-value = string(
                                  decimal(mach.run-rate:screen-value) + 
                                  decimal(mach.run-varoh:screen-value) +
                                  decimal(mach.run-fixoh:screen-value)
                                  )
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mach.run-trate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach.run-trate V-table-Win
ON ENTRY OF mach.run-trate IN FRAME F-Main /* Total */
DO:
     assign mach.run-trate:screen-value = string(
                                  decimal(mach.run-rate:screen-value) + 
                                  decimal(mach.run-varoh:screen-value) +
                                  decimal(mach.run-fixoh:screen-value)
                                  )
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mach.run-varoh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach.run-varoh V-table-Win
ON LEAVE OF mach.run-varoh IN FRAME F-Main /* Var OH */
DO:
       assign mach.run-trate:screen-value = string(
                                  decimal(mach.run-rate:screen-value) + 
                                  decimal(mach.run-varoh:screen-value) +
                                  decimal(mach.run-fixoh:screen-value)
                                  )
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mach.sch-m-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mach.sch-m-code V-table-Win
ON LEAVE OF mach.sch-m-code IN FRAME F-Main /* Schedule Machine */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-fi_sch-m-code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

session:data-entry-return = true.  /* make return key  same as tab key */

IF CAN-FIND(FIRST reftable WHERE reftable.reftable EQ "mach.sch-m-code") THEN
  FOR EACH reftable WHERE reftable.reftable EQ "mach.sch-m-code"
      EXCLUSIVE-LOCK:

      FIND FIRST mach-1 WHERE
           mach-1.company EQ reftable.company AND            
           mach-1.m-code EQ reftable.CODE
           EXCLUSIVE-LOCK NO-ERROR.

      IF AVAIL mach-1 THEN
      DO:
         mach-1.sch-m-code = reftable.code2.
         RELEASE mach-1.
      END.

      DELETE reftable.
  END.

IF CAN-FIND(FIRST reftable WHERE reftable.reftable EQ "gangjobs") THEN
  FOR EACH reftable WHERE reftable.reftable EQ "gangjobs"
      EXCLUSIVE-LOCK:

      FOR each mach-1 WHERE
        mach-1.company EQ reftable.company AND
        mach-1.m-code EQ reftable.CODE
        EXCLUSIVE-LOCK:

        mach-1.gang-jobs = LOGICAL(reftable.code2).
      END.

      DELETE reftable.
  END.

ll-label = CAN-FIND(FIRST sys-ctrl
                    WHERE sys-ctrl.company EQ cocode
                      AND sys-ctrl.name    EQ "MACHFILE"
                      AND sys-ctrl.log-fld).

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-item V-table-Win 
PROCEDURE add-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN dispatch ('add-record').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  {src/adm/template/row-list.i "mach"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "mach"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-rate V-table-Win 
PROCEDURE calc-rate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
do with frame {&frame-name}:
   case int(mach.lab-drate:screen-value) :
   when 1 then 
        assign mach.mr-rate:screen-value = string(
                                  dec(mach.mr-crusiz:screen-value) *
                                  dec(mach.lab-rate[1]:screen-value)
                                  )
               mach.run-rate:screen-value = string(
                                  dec(mach.run-crusiz:screen-value) *
                                  dec(mach.lab-rate[1]:screen-value)
                                  ).
   when 2 then 
        assign mach.mr-rate:screen-value = string(
                                  dec(mach.mr-crusiz:screen-value) *
                                  dec(mach.lab-rate[2]:screen-value)
                                  )
               mach.run-rate:screen-value = string(
                                  dec(mach.run-crusiz:screen-value) *
                                  dec(mach.lab-rate[2]:screen-value)
                                  ).
   when 3 then 
        assign mach.mr-rate:screen-value = string(
                                  dec(mach.mr-crusiz:screen-value) *
                                  dec(mach.lab-rate[3]:screen-value)
                                  )
               mach.run-rate:screen-value = string(
                                  dec(mach.run-crusiz:screen-value) *
                                  dec(mach.lab-rate[3]:screen-value)
                                  ).

   end case.
   assign mach.mr-trate:screen-value = string(
                                  dec(mach.mr-rate:screen-value) + 
                                  dec(mach.mr-varoh:screen-value) +
                                  dec(mach.mr-fixoh:screen-value)
                                  )
                                  .
   assign mach.run-trate:screen-value = string(
                                  dec(mach.run-rate:screen-value) + 
                                  dec(mach.run-varoh:screen-value) +
                                  dec(mach.run-fixoh:screen-value)
                                  )
                                  .
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dept-display V-table-Win 
PROCEDURE dept-display :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF AVAIL mach THEN
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     mach.therm:LABEL   = IF mach.dept[1]:SCREEN-VALUE EQ "HS"
                          THEN "Convert Production from Blanks to Sheets?"
                          ELSE "Use Lineal Feet in Run Matrix?"
     mach.tan-mrp:LABEL = IF LOOKUP(mach.dept[1]:SCREEN-VALUE,"PR,CT") LE 0
                          THEN "Changeover Time"
                          ELSE "Tandem MR/Plate".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dept-enable V-table-Win 
PROCEDURE dept-enable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF AVAIL mach THEN
  do with frame {&frame-name}:
    disable mach.pr-type
            mach.washup
            mach.col-pass
            mach.max-color
            mach.coater
            mach.col-wastesh
            mach.ink-waste
            mach.col-wastelb
            /*mach.tan-mrp*/
            mach.tan-mrf.

    /*IF LOOKUP(mach.dept[1]:SCREEN-VALUE,"DC,GL,WN,WS") GT 0 OR
       LOOKUP(mach.dept[1]:SCREEN-VALUE,"DC,GL,WN,WS") GT 0 OR
       LOOKUP(mach.dept[1]:SCREEN-VALUE,"DC,GL,WN,WS") GT 0 OR
       LOOKUP(mach.dept[1]:SCREEN-VALUE,"DC,GL,WN,WS") GT 0 THEN
      ENABLE mach.tan-mrp.*/

    if mach.dept[1]:screen-value eq "PR" or
       mach.dept[2]:screen-value eq "PR" or
       mach.dept[3]:screen-value eq "PR" or
       mach.dept[4]:screen-value eq "PR" then
      enable mach.pr-type
             mach.washup
             mach.col-pass
             mach.max-color
             mach.coater
             mach.col-wastesh
             mach.ink-waste
             mach.col-wastelb   
             /*mach.tan-mrp*/
             mach.tan-mrf.

    else
      assign
       mach.pr-type:screen-value     = ""
       mach.washup:screen-value      = "0"
       mach.col-pass:screen-value    = "?"
       mach.max-color:screen-value   = "0"
       mach.coater:screen-value      = "?"
       mach.col-wastesh:screen-value = "0"
       mach.ink-waste:screen-value   = "0"
       mach.col-wastelb:screen-value = "0".
  end.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-mach V-table-Win 
PROCEDURE enable-mach :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.


  DO WITH FRAME {&frame-name}:
    APPLY "value-changed" TO mach.dept[1].

    /*IF CAN-FIND(FIRST est-op WHERE est-op.company EQ gcompany
                               AND est-op.m-code  EQ mach.m-code:SCREEN-VALUE)   OR
       CAN-FIND(FIRST job-mch WHERE job-mch.company EQ gcompany
                                AND job-mch.m-code  EQ mach.m-code:SCREEN-VALUE) OR
       CAN-FIND(FIRST ef WHERE ef.company EQ gcompany
                           AND ef.m-code  EQ mach.m-code:SCREEN-VALUE)           THEN      
      DISABLE mach.m-code.
    ELSE
      APPLY "entry" TO mach.m-code.*/

    IF adm-new-record THEN APPLY "entry" TO mach.m-code.
                      ELSE DISABLE mach.m-code.

    FOR EACH tt-ss:
      DELETE tt-ss.
    END.

    IF AVAIL mach THEN DO:
      CREATE tt-ss.
      tt-val = mach.max-wid.

      DO li = 1 TO EXTENT(mach.max-pan-ss):
        CREATE tt-ss.
        tt-val = mach.max-pan-ss[li] / 1000.
      END.
    END.

    ENABLE mach.sch-m-code cb_industry mach.gang-jobs tb_obsolete.

    IF mach.dept[1] EQ "DC" THEN ENABLE tb_plain-jobs.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE label-display V-table-Win 
PROCEDURE label-display :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN dept-display.

  RUN p-type-display.

/*  DO WITH FRAME {&FRAME-NAME}:            */
/*    IF v-cepanel-cha EQ "WminLmin" THEN   */
/*      ASSIGN                              */
/*       mach.min-pan-l:LABEL = "Box Length"*/
/*       mach.min-pan-w:LABEL = "Box Width".*/
/*  END.                                    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def buffer b-mmtx for mmtx.
  def buffer b-mmty for mmty.
  def buffer b-mstd for mstd.
  def var old-loc like mach.loc no-undo.
  def var old-dept like mach.dept[1] no-undo.
  DEF VAR li AS INT NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  assign old-m-code = mach.m-code
         old-loc = mach.loc
         old-dept = mach.dept[1].

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN reftable-values (NO).

  li = 0.
  FOR EACH tt-ss BY tt-seq:
    li = li + 1.
    IF li LE EXTENT(mach.max-pan-ss) THEN DO:
      mach.max-pan-ss[li] = tt-val * 1000.
    END.
  END.

  mach.industry = entry(lookup(cb_industry,lv-ind-list),",1,2,X").

if adm-adding-record then do:
  if mach.dept[1]:screen-value in frame {&frame-name} <> "" then do:
     find first dept where dept.code = self:screen-value no-lock no-error.
     if avail dept then do:
        mach.d-seq = dept.fc.
     end.  
  end.

  for each mstd where mstd.company = mach.company and
                      mstd.loc = mach.loc and
                      mstd.m-code = mach.m-code :
      for each mmtx of mstd:
          find first b-mmtx  where b-mmtx.company   eq mmtx.company
                              and b-mmtx.loc       eq mach.loc
                              and b-mmtx.m-code    eq mach.m-code 
                              and b-mmtx.dept      eq mach.dept[1]
                              and b-mmtx.style     eq mmtx.style
                              and b-mmtx.mr-run    eq mmtx.mr-run
                              and b-mmtx.page-no   eq mmtx.page-no
                              and b-mmtx.across-no eq mmtx.across-no
                              and recid(b-mmtx)    ne recid(mmtx)
                             no-error.
          if avail b-mmtx then delete b-mmtx.

          assign mmtx.loc  = mach.loc
                 mmtx.dept = mach.dept[1].
      end.
      for each mmty of mstd:
        find first b-mmty
            where b-mmty.company   eq mach.company
              and b-mmty.loc       eq mach.loc
              and b-mmty.m-code    eq mach.m-code 
              and b-mmty.dept      eq mach.dept[1]
              and b-mmty.style     eq mmty.style
              and b-mmty.page-no   eq mmty.page-no
              and b-mmty.across-no eq mmty.across-no
              and recid(b-mmty)    ne recid(mmty)
            no-error.

        if avail b-mmty then delete b-mmty.

        assign
         mmty.loc  = mach.loc
         mmty.dept = mach.dept[1].
      end.  /* each mmty */

      find first b-mstd where b-mstd.company eq mach.company
                          and b-mstd.loc     eq mach.loc
            and b-mstd.m-code  eq mach.m-code 
            and b-mstd.dept    eq mach.dept[1]
            and b-mstd.style   eq mstd.style
            and recid(b-mstd)  ne recid(mstd)
          no-error.

      if avail b-mstd then delete b-mstd.

      assign  mstd.loc  = mach.loc
              mstd.dept = mach.dept[1].
  end.

  disable mach.m-code cb_industry with frame {&frame-name}.
  def var char-hdl as cha no-undo.
  run get-link-handle in adm-broker-hdl (this-procedure, 'record-target', output char-hdl).
  if valid-handle(widget-handle(char-hdl)) then run dispatch in widget-handle(char-hdl) ('open-query').
 end.  /* for add only */  

 if adm-new-record and not adm-adding-record then do:  /* copy */   
    def buffer bf-mach for mach.
    def var li-next-mmtx as int no-undo.

    find bf-mach where bf-mach.company = cocode and
                       bf-mach.m-code = old-m-code
                       no-lock no-error.
    find last b-mmtx use-index mmtx-no no-lock no-error.
    li-next-mmtx = if avail b-mmtx then b-mmtx.mmtx-no + 1 else 1.

    for each mstd of bf-mach no-lock:
        for each mmty of mstd no-lock:
            create b-mmty.
            buffer-copy mmty except mmty.m-code to b-mmty.
            assign b-mmty.m-code = mach.m-code
                   b-mmty.loc = mach.loc
                   b-mmty.dept = mach.dept[1].    
        end.

        for each mmtx of mstd no-lock:
            create b-mmtx.
            buffer-copy mmtx except mmtx.m-code mmtx.mmtx-no to b-mmtx.
            assign b-mmtx.m-code = mach.m-code
                   b-mmtx.loc = mach.loc
                   b-mmtx.dept = mach.dept[1]
                   b-mmtx.mmtx-no = li-next-mmtx
                   li-next-mmtx = li-next-mmtx + 1.
        end.
        create b-mstd.
        buffer-copy mstd except mstd.m-code mstd.loc mstd.dept to b-mstd.
        assign b-mstd.m-code = mach.m-code
               b-mstd.loc = mach.loc
               b-mstd.dept = mach.dept[1]
               .
    end.
 end. 

   /* ======== m-code update */
 if not adm-new-record then do:
    if cb_industry = "Both" then mach.industry = "".
    else if cb_industry BEGINS "Fold" then mach.industry = "1".
    else if cb_industry BEGINS "Corr" then mach.industry = "2".

   if (old-m-code <> "" and old-m-code <> mach.m-code) or
      (old-dept <> "" and old-dept <> mach.dept[1]) or
      (old-loc <> "" and old-loc <> mach.loc)
   then do:
     for each mmtx
            where mmtx.company eq mach.company
              and mmtx.loc     eq old-loc
              and mmtx.m-code  eq old-m-code
            exclusive-lock:
          assign mmtx.m-code = mach.m-code
                 mmtx.loc = mach.loc
                 mmtx.dept = mach.dept[1].
          for each mmtx2
              where mmtx2.mmtx-no eq mmtx.mmtx-no
              exclusive-lock:
             assign mmtx2.m-code = mach.m-code
                    .
          end.
        end.
        for each mmty
            where mmty.company eq mach.company
              and mmty.loc     eq old-loc
              and mmty.m-code  eq old-m-code
            exclusive-lock:
          assign mmty.m-code = mach.m-code
                 mmty.loc = mach.loc
                 mmty.dept = mach.dept[1]
                 .
        end.
        for each mstd
            where mstd.company eq mach.company
              and mstd.loc     eq old-loc
              and mstd.m-code  eq old-m-code
            exclusive-lock:
          assign mstd.m-code = mach.m-code
                 mstd.loc = mach.loc
                 mstd.dept = mach.dept[1]
                 .
        end.

    end.
 end.  /* not new record */

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
  DISABLE ALL WITH FRAME {&FRAME-NAME}.

  RUN label-display.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-mach FOR mach.


  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  lv-mach-recid = RECID(mach).
  ASSIGN
   mach.company = cocode
   mach.loc     = locode.

  DO WITH FRAME {&FRAME-NAME}:
    IF adm-adding-record THEN DO:
      DISPLAY mach.loc mach.d-seq mach.m-seq.
      mach.sch-m-code:SCREEN-VALUE = mach.m-code.
    END.

    ASSIGN
     tb_plain-jobs:SCREEN-VALUE = "no"
     tb_obsolete:SCREEN-VALUE   = "no".
  END.

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
  IF AVAIL mach THEN DO:
    cb_industry = ENTRY(LOOKUP(TRIM(mach.industry),",1,2,X"),lv-ind-list).

    IF NOT adm-new-record THEN DO:
      ASSIGN
       tb_plain-jobs = NO
       tb_obsolete   = NO.

      IF mach.m-code NE "" THEN RUN reftable-values (YES).
    END.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DISABLE tb_plain-jobs tb_obsolete WITH FRAME {&FRAME-NAME}.

  RUN label-display.
  
  cMachImage:LOAD-IMAGE(mach.machineImage[1]:SCREEN-VALUE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE lAddRecord AS LOGICAL NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
    /* 33482 - Ensure blank record is not saved - MYT - 08/28/18 */
    IF adm-new-record 
    AND mach.m-code:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "" THEN DO:
        RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .
        RETURN NO-APPLY.
    END.
  ASSIGN lAddRecord = adm-new-record .
  RUN valid-m-code NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-fi_sch-m-code NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  {&methods/lValidateError.i YES}
  do with frame {&frame-name}:  /* validation */
      if mach.loc:screen-value <> "" and
         not can-find(loc where loc.company = cocode and loc.loc = mach.loc:screen-value)
      then do:
         message "Invalid Loacation. Try Help." view-as alert-box error.
         apply "entry" to mach.loc.
         return no-apply.
      end.
      if mach.dept[1]:screen-value <> "" and
         not can-find(dept where dept.code = mach.dept[1]:screen-value)
      then do:
         message "Invalid Department Code. Try Help." view-as alert-box error.
         apply "entry" to mach.dept[1].
         return no-apply.
      end.
      if mach.dept[2]:screen-value <> "" and
         not can-find(dept where dept.code = mach.dept[2]:screen-value)
      then do:
         message "Invalid Department Code. Try Help." view-as alert-box error.
         apply "entry" to mach.dept[2].
         return no-apply.
      end.
      if mach.dept[3]:screen-value <> "" and
         not can-find(dept where dept.code = mach.dept[3]:screen-value)
      then do:
         message "Invalid Department Code. Try Help." view-as alert-box error.
         apply "entry" to mach.dept[3].
         return no-apply.
      end.
      if mach.dept[4]:screen-value <> "" and
         not can-find(dept where dept.code = mach.dept[4]:screen-value)
      then do:
         message "Invalid Department Code. Try Help." view-as alert-box error.
         apply "entry" to mach.dept[4].
         return no-apply.
      end.
  end.
  {&methods/lValidateError.i NO}
  RUN valid-m-seq NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  /* ========== end of validation =================*/

  disable all with frame {&frame-name}.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF lAddRecord THEN RUN repo-query (ROWID(mach)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE p-type-display V-table-Win 
PROCEDURE p-type-display :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF lv-label[1] EQ "" THEN
      ASSIGN
       lv-label[1] = mach.min-len:LABEL
       lv-label[2] = mach.min-wid:LABEL.

    IF mach.p-type:SCREEN-VALUE EQ "R"                OR
       (mach.p-type:SCREEN-VALUE EQ "B" AND ll-label) THEN
      ASSIGN
       mach.min-len:LABEL = lv-label[2]
       mach.min-wid:LABEL = lv-label[1].
    ELSE
    IF CAN-DO("A,P",mach.p-type:SCREEN-VALUE) THEN
      ASSIGN
       mach.min-len:LABEL = "Set Length"
       mach.min-wid:LABEL = "Set Width".
    ELSE
      ASSIGN
       mach.min-len:LABEL = lv-label[1]
       mach.min-wid:LABEL = lv-label[2].
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reftable-values V-table-Win 
PROCEDURE reftable-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-display AS LOG NO-UNDO.


  IF AVAIL mach THEN DO:
    IF ip-display THEN
        ASSIGN 
            tb_plain-jobs = mach.plain-job
            tb_obsolete = mach.obsolete
            .
    ELSE
        ASSIGN 
            mach.plain-job = tb_plain-jobs
            mach.obsolete = tb_obsolete
            .
   
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query V-table-Win 
PROCEDURE repo-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

  DEF VAR char-hdl AS CHAR NO-UNDO.


  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"Record-Source", OUTPUT char-hdl).
  RUN repo-query IN WIDGET-HANDLE(char-hdl) (INPUT ip-rowid).

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
  {src/adm/template/snd-list.i "mach"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-fi_sch-m-code V-table-Win 
PROCEDURE valid-fi_sch-m-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-mach FOR mach.
  DEF VAR v-industry AS CHAR NO-UNDO.

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:

    ASSIGN
       cb_industry
       v-industry = entry(lookup(cb_industry,lv-ind-list),",1,2,X")
       mach.sch-m-code:SCREEN-VALUE = CAPS(mach.sch-m-code:SCREEN-VALUE).

    IF mach.sch-m-code:SCREEN-VALUE EQ ""                        OR
       (mach.sch-m-code:SCREEN-VALUE NE mach.m-code:SCREEN-VALUE AND
        NOT CAN-FIND(FIRST b-mach
                     WHERE b-mach.company  EQ mach.company
                       AND b-mach.industry EQ v-industry
                       AND b-mach.m-code   EQ mach.sch-m-code:SCREEN-VALUE
                       AND b-mach.sch-m-code EQ b-mach.m-code))
    THEN DO:
      MESSAGE TRIM(mach.sch-m-code:LABEL) + " invalid, try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO mach.sch-m-code.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-m-code V-table-Win 
PROCEDURE valid-m-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-mach FOR mach.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    mach.m-code:SCREEN-VALUE = CAPS(mach.m-code:SCREEN-VALUE).

    IF CAN-FIND(FIRST b-mach
                WHERE b-mach.company EQ mach.company
                  AND b-mach.m-code  EQ mach.m-code:SCREEN-VALUE
                  AND (ROWID(b-mach) NE ROWID(mach) OR
                       (adm-new-record AND NOT adm-adding-record))) THEN DO:
      MESSAGE TRIM(mach.m-code:LABEL) + " already exists..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO mach.m-code.
      RETURN ERROR.
    END.

    IF mach.sch-m-code:SCREEN-VALUE EQ "" THEN
      mach.sch-m-code:SCREEN-VALUE = mach.m-code:SCREEN-VALUE.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-m-seq V-table-Win 
PROCEDURE valid-m-seq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-mach FOR mach.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF CAN-FIND(FIRST b-mach
                WHERE b-mach.company EQ mach.company
                  AND b-mach.d-seq   EQ INT(mach.d-seq:SCREEN-VALUE)
                  AND b-mach.m-seq   EQ INT(mach.m-seq:SCREEN-VALUE)
                  AND ROWID(b-mach)  NE ROWID(mach)) THEN DO:

      MESSAGE "Machine with same sequence# already exists..."
              VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO mach.m-seq.
      RETURN ERROR.
    END.

    IF adm-new-record AND NOT adm-adding-record THEN DO: /* copy */
       IF CAN-FIND(FIRST b-mach WHERE b-mach.company EQ cocode
                          AND b-mach.d-seq   EQ INT(mach.d-seq:SCREEN-VALUE)
                          AND b-mach.m-seq   EQ INT(mach.m-seq:SCREEN-VALUE)
                   )
        THEN DO:
           MESSAGE "Machine with same sequence# already exists..."
                   VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO mach.m-seq.
           RETURN ERROR.
        END.
     END.


  END.



  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

