&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File:

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES mach
&Scoped-define FIRST-EXTERNAL-TABLE mach


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR mach.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 
&Scoped-Define DISPLAYED-FIELDS mach.m-code mach.lab-rate[1] mach.mr-rate ~
mach.m-dscr mach.lab-rate[2] mach.mr-varoh mach.min-len mach.max-len ~
mach.min-trimw mach.pr-type mach.loc mach.d-seq mach.m-seq mach.lab-rate[3] ~
mach.mr-fixoh mach.min-wid mach.max-wid mach.min-triml mach.washup ~
mach.col-pass mach.dept[1] mach.dept[2] mach.dept[3] mach.dept[4] ~
mach.lab-drate mach.mr-trate mach.min-cal mach.max-cal mach.max-color ~
mach.coater mach.industry mach.mrk-rate mach.run-rate mach.min-pan-l ~
mach.max-pan-l mach.col-wastesh mach.col-wastelb mach.p-type mach.mr-waste ~
mach.mr-crusiz mach.run-varoh mach.min-pan-w mach.max-pan-w mach.ink-waste ~
mach.run-spoil mach.run-crusiz mach.run-fixoh mach.min-dep mach.max-dep ~
mach.tan-mrp mach.tan-mrf mach.therm mach.run-trate mach.min-run ~
mach.max-run mach.num-wid mach.num-len 
&Scoped-define DISPLAYED-TABLES mach
&Scoped-define FIRST-DISPLAYED-TABLE mach


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

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
DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL 
     SIZE 177.8 BY 7.95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     mach.m-code AT ROW 1.24 COL 9 COLON-ALIGNED
          LABEL "Machine"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY .81
          BGCOLOR 15 
     mach.lab-rate[1] AT ROW 1.24 COL 46 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY .81
          BGCOLOR 15 
     mach.mr-rate AT ROW 1.24 COL 67 COLON-ALIGNED
          LABEL "Setup DL"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY .81
          BGCOLOR 15 
     mach.m-dscr AT ROW 2.19 COL 9 COLON-ALIGNED
          LABEL "Descrip"
          VIEW-AS FILL-IN 
          SIZE 27.6 BY .81
          BGCOLOR 15 
     mach.lab-rate[2] AT ROW 2.19 COL 46 COLON-ALIGNED
          LABEL "Rate[2]"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY .81
          BGCOLOR 15 
     mach.mr-varoh AT ROW 2.19 COL 67 COLON-ALIGNED
          LABEL "Var OH"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY .81
          BGCOLOR 15 
     mach.min-len AT ROW 2.19 COL 93 COLON-ALIGNED
          LABEL "Front-to-Back"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY .81
          BGCOLOR 15 
     mach.max-len AT ROW 2.19 COL 108 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14.6 BY .81
          BGCOLOR 15 
     mach.min-trimw AT ROW 2.19 COL 123 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14.6 BY .81
          BGCOLOR 15 
     mach.pr-type AT ROW 2.19 COL 148 COLON-ALIGNED
          LABEL "Printer"
          VIEW-AS FILL-IN 
          SIZE 4.2 BY .81
          BGCOLOR 15 
     mach.loc AT ROW 3.14 COL 9 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.6 BY .81
          BGCOLOR 15 
     mach.d-seq AT ROW 3.14 COL 26 COLON-ALIGNED
          LABEL "Seq"
          VIEW-AS FILL-IN 
          SIZE 4.8 BY .81
          BGCOLOR 15 
     mach.m-seq AT ROW 3.14 COL 31.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.8 BY .81
          BGCOLOR 15 
     mach.lab-rate[3] AT ROW 3.14 COL 46 COLON-ALIGNED
          LABEL "Rate[3]"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY .81
          BGCOLOR 15 
     mach.mr-fixoh AT ROW 3.14 COL 67 COLON-ALIGNED
          LABEL "Fixed OH"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY .81
          BGCOLOR 15 
     mach.min-wid AT ROW 3.14 COL 93 COLON-ALIGNED
          LABEL "Side-to-Side"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY .81
          BGCOLOR 15 
     mach.max-wid AT ROW 3.14 COL 108 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14.6 BY .81
          BGCOLOR 15 
     mach.min-triml AT ROW 3.14 COL 123 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14.6 BY .81
          BGCOLOR 15 
     mach.washup AT ROW 3.14 COL 148 COLON-ALIGNED
          LABEL "Washup"
          VIEW-AS FILL-IN 
          SIZE 9 BY .81
          BGCOLOR 15 
     mach.col-pass AT ROW 3.14 COL 172 COLON-ALIGNED
          LABEL "Color/Pass"
          VIEW-AS FILL-IN 
          SIZE 4.2 BY .81
          BGCOLOR 15 
     mach.dept[1] AT ROW 4.1 COL 9 COLON-ALIGNED
          LABEL "Dept"
          VIEW-AS FILL-IN 
          SIZE 6.4 BY .81
          BGCOLOR 15 
     mach.dept[2] AT ROW 4.1 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.4 BY .81
          BGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     mach.dept[3] AT ROW 4.1 COL 23 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.4 BY .81
          BGCOLOR 15 
     mach.dept[4] AT ROW 4.1 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.4 BY .81
          BGCOLOR 15 
     mach.lab-drate AT ROW 4.1 COL 46 COLON-ALIGNED
          LABEL "Default"
          VIEW-AS FILL-IN 
          SIZE 3.4 BY .81
          BGCOLOR 15 
     mach.mr-trate AT ROW 4.1 COL 67 COLON-ALIGNED
          LABEL "Total"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY .81
          BGCOLOR 15 
     mach.min-cal AT ROW 4.1 COL 93 COLON-ALIGNED
          LABEL "Caliper/Depth"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY .81
          BGCOLOR 15 
     mach.max-cal AT ROW 4.1 COL 108 COLON-ALIGNED NO-LABEL FORMAT ">9.99999"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY .81
          BGCOLOR 15 
     mach.max-color AT ROW 4.1 COL 148 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY .81
          BGCOLOR 15 
     mach.coater AT ROW 4.1 COL 172 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.2 BY .81
          BGCOLOR 15 
     mach.industry AT ROW 5.05 COL 9 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 4
          LIST-ITEM-PAIRS "Both","",
                     "Folding","1",
                     "Corrugated","2",
                     "Exclude","X"
          SIMPLE
          SIZE 16 BY .81
          BGCOLOR 15 
     mach.mrk-rate AT ROW 5.05 COL 46 COLON-ALIGNED
          LABEL "Min Charge"
          VIEW-AS FILL-IN 
          SIZE 9 BY .81
          BGCOLOR 15 
     mach.run-rate AT ROW 5.05 COL 67 COLON-ALIGNED
          LABEL "Run DL"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY .81
          BGCOLOR 15 
     mach.min-pan-l AT ROW 5.05 COL 93 COLON-ALIGNED
          LABEL "Slot/Score"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY .81
          BGCOLOR 15 
     mach.max-pan-l AT ROW 5.05 COL 108 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14.6 BY .81
          BGCOLOR 15 
     mach.col-wastesh AT ROW 5.05 COL 148 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY .81
          BGCOLOR 15 
     mach.col-wastelb AT ROW 5.05 COL 167 COLON-ALIGNED
          LABEL "Lbs/Color"
          VIEW-AS FILL-IN 
          SIZE 9.4 BY .81
          BGCOLOR 15 
     mach.p-type AT ROW 6 COL 9 COLON-ALIGNED
          LABEL "Feed"
          VIEW-AS FILL-IN 
          SIZE 4.2 BY .81
          BGCOLOR 15 
     mach.mr-waste AT ROW 6 COL 27.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY .81
          BGCOLOR 15 
     mach.mr-crusiz AT ROW 6 COL 46 COLON-ALIGNED
          LABEL "SU Crew"
          VIEW-AS FILL-IN 
          SIZE 9 BY .81
          BGCOLOR 15 
     mach.run-varoh AT ROW 6 COL 67 COLON-ALIGNED
          LABEL "Var OH"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY .81
          BGCOLOR 15 
     mach.min-pan-w AT ROW 6 COL 93 COLON-ALIGNED
          LABEL "Panel (Hd-Hd)"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY .81
          BGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     mach.max-pan-w AT ROW 6 COL 108 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14.6 BY .81
          BGCOLOR 15 
     mach.ink-waste AT ROW 6 COL 148 COLON-ALIGNED
          LABEL "Ink Waste Lbs/MR"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY .81
          BGCOLOR 15 
     mach.run-spoil AT ROW 6.95 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY .81
          BGCOLOR 15 
     mach.run-crusiz AT ROW 6.95 COL 46 COLON-ALIGNED
          LABEL "Run Crew"
          VIEW-AS FILL-IN 
          SIZE 9 BY .81
          BGCOLOR 15 
     mach.run-fixoh AT ROW 6.95 COL 67 COLON-ALIGNED
          LABEL "Fixed OH"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY .81
          BGCOLOR 15 
     mach.min-dep AT ROW 6.95 COL 93 COLON-ALIGNED
          LABEL "Slot Size"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY .81
          BGCOLOR 15 
     mach.max-dep AT ROW 6.95 COL 108 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14.6 BY .81
          BGCOLOR 15 
     mach.tan-mrp AT ROW 6.95 COL 148 COLON-ALIGNED
          LABEL "Tandem MR/Plate"
          VIEW-AS FILL-IN 
          SIZE 9 BY .81
          BGCOLOR 15 
     mach.tan-mrf AT ROW 6.95 COL 167 COLON-ALIGNED
          LABEL "/Fountain"
          VIEW-AS FILL-IN 
          SIZE 9 BY .81
          BGCOLOR 15 
     mach.therm AT ROW 7.91 COL 30.4 COLON-ALIGNED
          LABEL "Use Lineal feet in Run Matrix"
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
          BGCOLOR 15 
     mach.run-trate AT ROW 7.91 COL 67 COLON-ALIGNED
          LABEL "Total"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY .81
          BGCOLOR 15 
     mach.min-run AT ROW 7.91 COL 93 COLON-ALIGNED
          LABEL "Run Qty"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY .81
          BGCOLOR 15 
     mach.max-run AT ROW 7.91 COL 108 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     mach.num-wid AT ROW 7.91 COL 148 COLON-ALIGNED
          LABEL "Max Num Width"
          VIEW-AS FILL-IN 
          SIZE 4.8 BY .81
          BGCOLOR 15 
     mach.num-len AT ROW 7.91 COL 171 COLON-ALIGNED
          LABEL "Length"
          VIEW-AS FILL-IN 
          SIZE 4.8 BY .81
          BGCOLOR 15 
     "Limits" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1.24 COL 84
          FONT 6
     "Trim" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1.24 COL 129
          FONT 6
     "Printing Press" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 1.24 COL 142
          FONT 6
     "Maximum" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.24 COL 112
          FONT 6
     "Minimum" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 1.24 COL 97
          FONT 6
     RECT-3 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.mach
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
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
         HEIGHT             = 7.95
         WIDTH              = 177.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN mach.coater IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach.col-pass IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.col-wastelb IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.col-wastesh IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach.d-seq IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.dept[1] IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.dept[2] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach.dept[3] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach.dept[4] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX mach.industry IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach.ink-waste IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.lab-drate IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.lab-rate[1] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach.lab-rate[2] IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.lab-rate[3] IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.loc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach.m-code IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.m-dscr IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.m-seq IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach.max-cal IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN mach.max-color IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach.max-dep IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach.max-len IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach.max-pan-l IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach.max-pan-w IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach.max-run IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach.max-wid IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach.min-cal IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.min-dep IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.min-len IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.min-pan-l IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.min-pan-w IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.min-run IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.min-triml IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach.min-trimw IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach.min-wid IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.mr-crusiz IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.mr-fixoh IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.mr-rate IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.mr-trate IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.mr-varoh IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.mr-waste IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach.mrk-rate IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.num-len IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.num-wid IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.p-type IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.pr-type IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.run-crusiz IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.run-fixoh IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.run-rate IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.run-spoil IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mach.run-trate IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.run-varoh IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.tan-mrf IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.tan-mrp IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.therm IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mach.washup IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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

