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
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
&scoped-define enable-proc proc-enable
&scoped-define copy-proc proc-copy

{gl/gl-fs.i NEW}

DEF BUFFER XX-RPT FOR GL-RPT.

ASSIGN cocode = g_company
       locode = g_loc.

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
&Scoped-define EXTERNAL-TABLES gl-rpt
&Scoped-define FIRST-EXTERNAL-TABLE gl-rpt


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR gl-rpt.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS gl-rpt.rpt gl-rpt.dscr 
&Scoped-define ENABLED-TABLES gl-rpt
&Scoped-define FIRST-ENABLED-TABLE gl-rpt
&Scoped-Define ENABLED-OBJECTS RECT-11 RECT-13 RECT-14 RECT-18 RECT-19 ~
RECT-20 
&Scoped-Define DISPLAYED-FIELDS gl-rpt.rpt gl-rpt.dscr 
&Scoped-define DISPLAYED-TABLES gl-rpt
&Scoped-define FIRST-DISPLAYED-TABLE gl-rpt
&Scoped-Define DISPLAYED-OBJECTS v-hdr-1 v-hdr-2 v-hdr-3 v-hdr-4 v-hdr-5 ~
lv-c-bs lv-p-w lv-page-length lv-col-used lv-d-wid lv-no-col v-ch-1 v-ct-1 ~
v-per-1 v-ch-2 v-ct-2 v-per-2 v-ch-3 v-ct-3 v-per-3 v-ch-4 v-ct-4 v-per-4 ~
v-ch-5 v-ct-5 v-per-5 v-ch-6 v-ct-6 v-per-6 v-ch-7 v-ct-7 v-per-7 v-ch-8 ~
v-ct-8 v-per-8 v-ch-9 v-ct-9 v-per-9 lv-onevar v-vcol-1 v-vcol-2 v-vcol-3 ~
lv-twovar v-vcol-4 v-vcol-5 v-vcol-6 v-sub-1 v-sub-2 v-sub-3 v-sub-4 ~
v-sub-5 v-sub-6 v-sub-7 v-sub-8 v-sub-9 v-sub-10 v-sub-11 v-sub-12 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS gl-rpt.rpt 
&Scoped-define ADM-ASSIGN-FIELDS gl-rpt.rpt 
&Scoped-define List-5 v-hdr-1 v-hdr-2 v-hdr-3 v-hdr-4 v-hdr-5 lv-p-w ~
lv-page-length lv-d-wid lv-no-col v-ch-1 v-ct-1 v-per-1 v-ch-2 v-ct-2 ~
v-per-2 v-ch-3 v-ct-3 v-per-3 v-ch-4 v-ct-4 v-per-4 v-ch-5 v-ct-5 v-per-5 ~
v-ch-6 v-ct-6 v-per-6 v-ch-7 v-ct-7 v-per-7 v-ch-8 v-ct-8 v-per-8 v-ch-9 ~
v-ct-9 v-per-9 v-sub-1 v-sub-2 v-sub-3 v-sub-4 v-sub-5 v-sub-6 v-sub-7 ~
v-sub-8 v-sub-9 v-sub-10 v-sub-11 v-sub-12 

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
DEFINE VARIABLE lv-col-used AS INTEGER FORMAT "999":U INITIAL 0 
     LABEL "Columns Used" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-d-wid AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Description Width" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-no-col AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Number of Columns" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-onevar AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "1" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lv-p-w AS INTEGER FORMAT "999":U INITIAL 80 
     LABEL "Page Width" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-page-length AS INTEGER FORMAT ">9":U INITIAL 66 
     LABEL "Page Length" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-twovar AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "2" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE v-ch-1 AS CHARACTER FORMAT "x(13)":U 
     LABEL "1" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-ch-2 AS CHARACTER FORMAT "x(13)":U 
     LABEL "2" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-ch-3 AS CHARACTER FORMAT "x(13)":U 
     LABEL "3" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-ch-4 AS CHARACTER FORMAT "x(13)":U 
     LABEL "4" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-ch-5 AS CHARACTER FORMAT "x(13)":U 
     LABEL "5" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-ch-6 AS CHARACTER FORMAT "x(13)":U 
     LABEL "6" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-ch-7 AS CHARACTER FORMAT "x(13)":U 
     LABEL "7" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-ch-8 AS CHARACTER FORMAT "x(13)":U 
     LABEL "8" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-ch-9 AS CHARACTER FORMAT "x(13)":U 
     LABEL "9" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-ct-1 AS CHARACTER FORMAT "x(13)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-ct-2 AS CHARACTER FORMAT "x(13)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-ct-3 AS CHARACTER FORMAT "x(13)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-ct-4 AS CHARACTER FORMAT "x(13)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-ct-5 AS CHARACTER FORMAT "x(13)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-ct-6 AS CHARACTER FORMAT "x(13)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-ct-7 AS CHARACTER FORMAT "x(13)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-ct-8 AS CHARACTER FORMAT "x(13)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-ct-9 AS CHARACTER FORMAT "x(13)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-hdr-1 AS CHARACTER FORMAT "X(50)":U 
     LABEL "Header 1" 
     VIEW-AS FILL-IN 
     SIZE 90 BY 1 NO-UNDO.

DEFINE VARIABLE v-hdr-2 AS CHARACTER FORMAT "X(50)":U 
     LABEL "2" 
     VIEW-AS FILL-IN 
     SIZE 90 BY 1 NO-UNDO.

DEFINE VARIABLE v-hdr-3 AS CHARACTER FORMAT "X(50)":U 
     LABEL "3" 
     VIEW-AS FILL-IN 
     SIZE 90 BY 1 NO-UNDO.

DEFINE VARIABLE v-hdr-4 AS CHARACTER FORMAT "X(50)":U 
     LABEL "4" 
     VIEW-AS FILL-IN 
     SIZE 90 BY 1 NO-UNDO.

DEFINE VARIABLE v-hdr-5 AS CHARACTER FORMAT "X(50)":U 
     LABEL "5" 
     VIEW-AS FILL-IN 
     SIZE 90 BY 1 NO-UNDO.

DEFINE VARIABLE v-sub-1 AS CHARACTER FORMAT "x(13)":U 
     LABEL "1" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-sub-10 AS CHARACTER FORMAT "x(13)":U 
     LABEL "10" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-sub-11 AS CHARACTER FORMAT "x(13)":U 
     LABEL "11" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-sub-12 AS CHARACTER FORMAT "x(13)":U 
     LABEL "12" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-sub-2 AS CHARACTER FORMAT "x(13)":U 
     LABEL "2" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-sub-3 AS CHARACTER FORMAT "x(13)":U 
     LABEL "3" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-sub-4 AS CHARACTER FORMAT "x(13)":U 
     LABEL "4" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-sub-5 AS CHARACTER FORMAT "x(13)":U 
     LABEL "5" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-sub-6 AS CHARACTER FORMAT "x(13)":U 
     LABEL "6" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-sub-7 AS CHARACTER FORMAT "x(13)":U 
     LABEL "7" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-sub-8 AS CHARACTER FORMAT "x(13)":U 
     LABEL "8" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-sub-9 AS CHARACTER FORMAT "x(13)":U 
     LABEL "9" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE v-vcol-1 AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "= Col" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE v-vcol-2 AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "+ Col" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE v-vcol-3 AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "- Col" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE v-vcol-4 AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "= Col" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE v-vcol-5 AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "+ Col" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE v-vcol-6 AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "- Col" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 62 BY 2.62.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 105 BY 5.24.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 33 BY 6.67.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 63 BY 6.67.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71 BY 10.24.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 144 BY 17.48.

DEFINE VARIABLE lv-c-bs AS LOGICAL INITIAL no 
     LABEL "1 Col Balance Sheet" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE v-per-1 AS LOGICAL INITIAL no 
     LABEL "1" 
     VIEW-AS TOGGLE-BOX
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE v-per-2 AS LOGICAL INITIAL no 
     LABEL "2" 
     VIEW-AS TOGGLE-BOX
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE v-per-3 AS LOGICAL INITIAL no 
     LABEL "3" 
     VIEW-AS TOGGLE-BOX
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE v-per-4 AS LOGICAL INITIAL no 
     LABEL "4" 
     VIEW-AS TOGGLE-BOX
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE v-per-5 AS LOGICAL INITIAL no 
     LABEL "5" 
     VIEW-AS TOGGLE-BOX
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE v-per-6 AS LOGICAL INITIAL no 
     LABEL "6" 
     VIEW-AS TOGGLE-BOX
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE v-per-7 AS LOGICAL INITIAL no 
     LABEL "7" 
     VIEW-AS TOGGLE-BOX
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE v-per-8 AS LOGICAL INITIAL no 
     LABEL "8" 
     VIEW-AS TOGGLE-BOX
     SIZE 6 BY .81 NO-UNDO.

DEFINE VARIABLE v-per-9 AS LOGICAL INITIAL no 
     LABEL "9" 
     VIEW-AS TOGGLE-BOX
     SIZE 6 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     gl-rpt.rpt AT ROW 1.24 COL 18 COLON-ALIGNED
          LABEL "Report Code"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     gl-rpt.dscr AT ROW 1.24 COL 28 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 62 BY 1
     v-hdr-1 AT ROW 2.67 COL 14 COLON-ALIGNED
     v-hdr-2 AT ROW 3.62 COL 14 COLON-ALIGNED
     v-hdr-3 AT ROW 4.57 COL 14 COLON-ALIGNED
     v-hdr-4 AT ROW 5.52 COL 14 COLON-ALIGNED
     v-hdr-5 AT ROW 6.48 COL 14 COLON-ALIGNED
     lv-c-bs AT ROW 1.71 COL 113
     lv-p-w AT ROW 2.67 COL 132 COLON-ALIGNED
     lv-page-length AT ROW 3.62 COL 132 COLON-ALIGNED
     lv-col-used AT ROW 4.57 COL 132 COLON-ALIGNED
     lv-d-wid AT ROW 5.52 COL 132 COLON-ALIGNED
     lv-no-col AT ROW 6.48 COL 134 COLON-ALIGNED
     v-ch-1 AT ROW 8.86 COL 6 COLON-ALIGNED
     v-ct-1 AT ROW 8.86 COL 33 COLON-ALIGNED NO-LABEL
     v-per-1 AT ROW 8.86 COL 63
     v-ch-2 AT ROW 9.81 COL 6 COLON-ALIGNED
     v-ct-2 AT ROW 9.81 COL 33 COLON-ALIGNED NO-LABEL
     v-per-2 AT ROW 9.81 COL 63
     v-ch-3 AT ROW 10.76 COL 6 COLON-ALIGNED
     v-ct-3 AT ROW 10.76 COL 33 COLON-ALIGNED NO-LABEL
     v-per-3 AT ROW 10.76 COL 63
     v-ch-4 AT ROW 11.71 COL 6 COLON-ALIGNED
     v-ct-4 AT ROW 11.71 COL 33 COLON-ALIGNED NO-LABEL
     v-per-4 AT ROW 11.71 COL 63
     v-ch-5 AT ROW 12.67 COL 6 COLON-ALIGNED
     v-ct-5 AT ROW 12.67 COL 33 COLON-ALIGNED NO-LABEL
     v-per-5 AT ROW 12.67 COL 63
     v-ch-6 AT ROW 13.62 COL 6 COLON-ALIGNED
     v-ct-6 AT ROW 13.62 COL 33 COLON-ALIGNED NO-LABEL
     v-per-6 AT ROW 13.62 COL 63
     v-ch-7 AT ROW 14.57 COL 6 COLON-ALIGNED
     v-ct-7 AT ROW 14.57 COL 33 COLON-ALIGNED NO-LABEL
     v-per-7 AT ROW 14.57 COL 63
     v-ch-8 AT ROW 15.52 COL 6 COLON-ALIGNED
     v-ct-8 AT ROW 15.52 COL 33 COLON-ALIGNED NO-LABEL
     v-per-8 AT ROW 15.52 COL 63
     v-ch-9 AT ROW 16.48 COL 6 COLON-ALIGNED
     v-ct-9 AT ROW 16.48 COL 33 COLON-ALIGNED NO-LABEL
     v-per-9 AT ROW 16.48 COL 63
     lv-onevar AT ROW 8.62 COL 87 COLON-ALIGNED
     v-vcol-1 AT ROW 8.62 COL 101 COLON-ALIGNED
     v-vcol-2 AT ROW 8.62 COL 114 COLON-ALIGNED
     v-vcol-3 AT ROW 8.62 COL 127 COLON-ALIGNED
     lv-twovar AT ROW 9.57 COL 87 COLON-ALIGNED
     v-vcol-4 AT ROW 9.57 COL 101 COLON-ALIGNED
     v-vcol-5 AT ROW 9.57 COL 114 COLON-ALIGNED
     v-vcol-6 AT ROW 9.57 COL 127 COLON-ALIGNED
     v-sub-1 AT ROW 11.95 COL 82 COLON-ALIGNED
     v-sub-2 AT ROW 12.91 COL 82 COLON-ALIGNED
     v-sub-3 AT ROW 13.86 COL 82 COLON-ALIGNED
     v-sub-4 AT ROW 14.81 COL 82 COLON-ALIGNED
     v-sub-5 AT ROW 15.76 COL 82 COLON-ALIGNED
     v-sub-6 AT ROW 16.71 COL 82 COLON-ALIGNED
     v-sub-7 AT ROW 11.95 COL 114 COLON-ALIGNED
     v-sub-8 AT ROW 12.91 COL 114 COLON-ALIGNED
     v-sub-9 AT ROW 13.86 COL 114 COLON-ALIGNED
     v-sub-10 AT ROW 14.81 COL 114 COLON-ALIGNED
     v-sub-11 AT ROW 15.76 COL 114 COLON-ALIGNED
     v-sub-12 AT ROW 16.71 COL 114 COLON-ALIGNED
     RECT-11 AT ROW 8.14 COL 81
     RECT-13 AT ROW 2.43 COL 3
     RECT-14 AT ROW 1.24 COL 110
     RECT-18 AT ROW 11.48 COL 80
     RECT-19 AT ROW 7.91 COL 3
     RECT-20 AT ROW 1 COL 1
     "Column Headers" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 8.14 COL 9
     "Column Types" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 8.14 COL 36
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     "VAR. Col." VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 7.91 COL 88
     "Prt % of Sales" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 8.14 COL 57
     "Subtotal Descriptions" VIEW-AS TEXT
          SIZE 27 BY .62 AT ROW 11.24 COL 96
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.gl-rpt
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
         HEIGHT             = 19.76
         WIDTH              = 144.
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
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR TOGGLE-BOX lv-c-bs IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-col-used IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-d-wid IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN lv-no-col IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN lv-onevar IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-p-w IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN lv-page-length IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN lv-twovar IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN gl-rpt.rpt IN FRAME F-Main
   1 2 EXP-LABEL                                                        */
/* SETTINGS FOR FILL-IN v-ch-1 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-ch-2 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-ch-3 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-ch-4 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-ch-5 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-ch-6 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-ch-7 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-ch-8 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-ch-9 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-ct-1 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-ct-2 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-ct-3 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-ct-4 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-ct-5 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-ct-6 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-ct-7 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-ct-8 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-ct-9 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-hdr-1 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-hdr-2 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-hdr-3 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-hdr-4 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-hdr-5 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR TOGGLE-BOX v-per-1 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR TOGGLE-BOX v-per-2 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR TOGGLE-BOX v-per-3 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR TOGGLE-BOX v-per-4 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR TOGGLE-BOX v-per-5 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR TOGGLE-BOX v-per-6 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR TOGGLE-BOX v-per-7 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR TOGGLE-BOX v-per-8 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR TOGGLE-BOX v-per-9 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-sub-1 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-sub-10 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-sub-11 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-sub-12 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-sub-2 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-sub-3 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-sub-4 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-sub-5 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-sub-6 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-sub-7 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-sub-8 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-sub-9 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN v-vcol-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-vcol-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-vcol-3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-vcol-4 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-vcol-5 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-vcol-6 IN FRAME F-Main
   NO-ENABLE                                                            */
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
    DEF VAR char-val AS cha NO-UNDO.

    CASE FOCUS:NAME:
        WHEN "v-ct-1" OR  WHEN "v-ct-2" OR  WHEN "v-ct-3" OR  WHEN "v-ct-4" OR
        WHEN "v-ct-5" OR  WHEN "v-ct-6" OR  WHEN "v-ct-7" OR  WHEN "v-ct-8" OR  WHEN "v-ct-9"
        THEN DO:
             RUN gl/l-ctypes.w (OUTPUT char-val).
             IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = char-val.
             RETURN NO-APPLY.
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-c-bs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-c-bs V-table-Win
ON return OF lv-c-bs IN FRAME F-Main /* 1 Col Balance Sheet */
DO:
  APPLY "tab" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-no-col
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-no-col V-table-Win
ON LEAVE OF lv-no-col IN FRAME F-Main /* Number of Columns */
DO:
  ASSIGN {&self-name}.

  DEF VAR i AS INT NO-UNDO.
/*  DO i = 1 TO 9:
     IF i > lv-no-col THEN CASE i:         
        WHEN 1 THEN DISABLE v-ch-1 v-ct-1 WITH FRAME {&FRAME-NAME}.
        WHEN 2 THEN DISABLE v-ch-2 v-ct-2 WITH FRAME {&FRAME-NAME}.
        WHEN 3 THEN DISABLE v-ch-3 v-ct-3 WITH FRAME {&FRAME-NAME}.
        WHEN 4 THEN DISABLE v-ch-4 v-ct-4 WITH FRAME {&FRAME-NAME}.
        WHEN 5 THEN DISABLE v-ch-5 v-ct-5 WITH FRAME {&FRAME-NAME}.
        WHEN 6 THEN DISABLE v-ch-6 v-ct-6 WITH FRAME {&FRAME-NAME}.
        WHEN 7 THEN DISABLE v-ch-7 v-ct-7 WITH FRAME {&FRAME-NAME}.
        WHEN 8 THEN DISABLE v-ch-8 v-ct-8 WITH FRAME {&FRAME-NAME}.
        WHEN 9 THEN DISABLE v-ch-9 v-ct-9 WITH FRAME {&FRAME-NAME}.
     END.
  END.
*/

  DISABLE v-ch-1 v-ct-1 
          v-ch-2 v-ct-2 
          v-ch-3 v-ct-3 
          v-ch-4 v-ct-4 
          v-ch-5 v-ct-5 
          v-ch-6 v-ct-6 
          v-ch-7 v-ct-7 
          v-ch-8 v-ct-8 
          v-ch-9 v-ct-9       
          WITH FRAME {&FRAME-NAME}.

  DO i = 1 TO 9:     
     IF i <= lv-no-col THEN CASE i:         
        WHEN 1 THEN enable v-ch-1 v-ct-1 WITH FRAME {&FRAME-NAME}.            
        WHEN 2 THEN enable v-ch-2 v-ct-2 WITH FRAME {&FRAME-NAME}.           
        WHEN 3 THEN enable v-ch-3 v-ct-3 WITH FRAME {&FRAME-NAME}.
        WHEN 4 THEN enable v-ch-4 v-ct-4 WITH FRAME {&FRAME-NAME}.
        WHEN 5 THEN enable v-ch-5 v-ct-5 WITH FRAME {&FRAME-NAME}.            
        WHEN 6 THEN enable v-ch-6 v-ct-6 WITH FRAME {&FRAME-NAME}.            
        WHEN 7 THEN enable v-ch-7 v-ct-7 WITH FRAME {&FRAME-NAME}.
        WHEN 8 THEN enable v-ch-8 v-ct-8 WITH FRAME {&FRAME-NAME}.
        WHEN 9 THEN enable v-ch-9 v-ct-9 WITH FRAME {&FRAME-NAME}.
     END.

  END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gl-rpt.rpt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gl-rpt.rpt V-table-Win
ON ENTRY OF gl-rpt.rpt IN FRAME F-Main /* Report Code */
DO:
  IF NOT adm-new-record THEN DO:
    APPLY "tab" TO SELF.
    RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gl-rpt.rpt V-table-Win
ON LEAVE OF gl-rpt.rpt IN FRAME F-Main /* Report Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-rpt NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-ct-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-ct-1 V-table-Win
ON LEAVE OF v-ct-1 IN FRAME F-Main
DO:
  RUN calc-var (0) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-ct-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-ct-2 V-table-Win
ON LEAVE OF v-ct-2 IN FRAME F-Main
DO:
  RUN calc-var (0) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-ct-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-ct-3 V-table-Win
ON LEAVE OF v-ct-3 IN FRAME F-Main
DO:
  RUN calc-var (0) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-ct-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-ct-4 V-table-Win
ON LEAVE OF v-ct-4 IN FRAME F-Main
DO:
  RUN calc-var (0) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-ct-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-ct-5 V-table-Win
ON LEAVE OF v-ct-5 IN FRAME F-Main
DO:
  RUN calc-var (0) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-ct-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-ct-6 V-table-Win
ON LEAVE OF v-ct-6 IN FRAME F-Main
DO:
  RUN calc-var (0) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-ct-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-ct-7 V-table-Win
ON LEAVE OF v-ct-7 IN FRAME F-Main
DO:
  RUN calc-var (0) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-ct-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-ct-8 V-table-Win
ON LEAVE OF v-ct-8 IN FRAME F-Main
DO:
  RUN calc-var (0) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-ct-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-ct-9 V-table-Win
ON LEAVE OF v-ct-9 IN FRAME F-Main
DO:
  RUN calc-var (0) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
SESSION:DATA-ENTRY-RETURN = YES.

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
  {src/adm/template/row-list.i "gl-rpt"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "gl-rpt"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-var V-table-Win 
PROCEDURE calc-var :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-type AS INT NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR li-all-per AS INT NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.


  IF LASTKEY EQ -1 THEN Return .
  {&methods/lValidateError.i YES}
  IF LASTKEY NE -1 THEN
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     li-all-per = 0
     lv-onevar  = 0
     lv-twovar  = 0
     v-ct[1]    = TRIM(v-ct-1:SCREEN-VALUE)
     v-ct[2]    = TRIM(v-ct-2:SCREEN-VALUE)
     v-ct[3]    = TRIM(v-ct-3:SCREEN-VALUE)
     v-ct[4]    = TRIM(v-ct-4:SCREEN-VALUE)
     v-ct[5]    = TRIM(v-ct-5:SCREEN-VALUE)
     v-ct[6]    = TRIM(v-ct-6:SCREEN-VALUE)
     v-ct[7]    = TRIM(v-ct-7:SCREEN-VALUE)
     v-ct[8]    = TRIM(v-ct-8:SCREEN-VALUE)
     v-ct[9]    = TRIM(v-ct-9:SCREEN-VALUE)
     v-ch[1]    = TRIM(v-ch-1:SCREEN-VALUE)
     v-ch[2]    = TRIM(v-ch-2:SCREEN-VALUE)
     v-ch[3]    = TRIM(v-ch-3:SCREEN-VALUE)
     v-ch[4]    = TRIM(v-ch-4:SCREEN-VALUE)
     v-ch[5]    = TRIM(v-ch-5:SCREEN-VALUE)
     v-ch[6]    = TRIM(v-ch-6:SCREEN-VALUE)
     v-ch[7]    = TRIM(v-ch-7:SCREEN-VALUE)
     v-ch[8]    = TRIM(v-ch-8:SCREEN-VALUE)
     v-ch[9]    = TRIM(v-ch-9:SCREEN-VALUE).

    IF ip-type EQ 1 THEN
    DO i = 1 TO 9:
      IF v-ct[i] EQ "All Per & YTD" THEN DO:
        li-all-per = i.
        LEAVE.
      END.
    END.

    IF li-all-per NE 0 THEN DO:
      DO i = 1 TO 9:
        IF i NE li-all-per AND v-ct[i] NE "" THEN DO:
          MESSAGE "All Per & YTD can be the only column type..."
              VIEW-AS ALERT-BOX ERROR.
          RETURN ERROR.
        END.
      END.

      MESSAGE "Do you wish to use month names as column headers?" SKIP
              "(YES for Month Names or NO to print Period#)"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll.

      ASSIGN
       v-ch    = ""
       v-ct    = ""
       v-ch[1] = IF ll THEN "Months" ELSE "Period#"
       v-ct[1] = "All Per & YTD".
    END.

    ASSIGN
     v-ct-1:SCREEN-VALUE = v-ct[1]
     v-ct-2:SCREEN-VALUE = v-ct[2]
     v-ct-3:SCREEN-VALUE = v-ct[3]
     v-ct-4:SCREEN-VALUE = v-ct[4]
     v-ct-5:SCREEN-VALUE = v-ct[5]
     v-ct-6:SCREEN-VALUE = v-ct[6]
     v-ct-7:SCREEN-VALUE = v-ct[7]
     v-ct-8:SCREEN-VALUE = v-ct[8]
     v-ct-9:SCREEN-VALUE = v-ct[9]
     v-ch-1:SCREEN-VALUE = v-ch[1]
     v-ch-2:SCREEN-VALUE = v-ch[2]
     v-ch-3:SCREEN-VALUE = v-ch[3]
     v-ch-4:SCREEN-VALUE = v-ch[4]
     v-ch-5:SCREEN-VALUE = v-ch[5]
     v-ch-6:SCREEN-VALUE = v-ch[6]
     v-ch-7:SCREEN-VALUE = v-ch[7]
     v-ch-8:SCREEN-VALUE = v-ch[8]
     v-ch-9:SCREEN-VALUE = v-ch[9].

    DO i = 1 TO 9 :
      if lv-onevar = 0 THEN lv-onevar = (if v-ct[i] = "Variance" then i else 0).
          else if lv-onevar ne 0 and lv-twovar = 0 then
               lv-twovar = (if v-ct[i] = "Variance" and lv-onevar ne 0 then i else 0).
          if lv-onevar ne 0 and lv-twovar ne 0 and 
         (v-ct[i] = "Variance" AND i ne lv-onevar and i ne lv-twovar) 
      then do:
                   MESSAGE "Cannot have more than 2 Variance volumns..."
                       VIEW-AS ALERT-BOX ERROR.
               RETURN ERROR.
      end.   
    END.

    IF lv-onevar = 0 THEN ASSIGN v-vcol-1 = 0
                                 v-vcol-2 = 0
                                 v-vcol-3 = 0.
    IF lv-twovar = 0 THEN ASSIGN v-vcol-4 = 0
                                 v-vcol-5 = 0
                                 v-vcol-6 = 0.

    DISPLAY lv-onevar lv-twovar v-vcol-1 v-vcol-2 v-vcol-3 
            v-vcol-4 v-vcol-5 v-vcol-6.

    IF lv-onevar NE 0 THEN ENABLE v-vcol-1 v-vcol-2 v-vcol-3.
    IF lv-twovar NE 0 THEN ENABLE v-vcol-4 v-vcol-5 v-vcol-6.
  END.
  {&methods/lValidateError.i NO}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-prev-rpt LIKE gl-rpt.rpt NO-UNDO.
  DEF VAR lv-prev-rpt-recid  AS RECID NO-UNDO.
  DEF BUFFER bf-rpt FOR gl-rpt.
  DEF BUFFER new-rpt FOR gl-rpt.

  /* Code placed here will execute PRIOR to standard behavior. */
  lv-prev-rpt = gl-rpt.rpt.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF adm-new-record AND NOT adm-adding-record  THEN DO: /* copy*/
     FOR each bf-rpt WHERE bf-rpt.company = g_company
                       AND bf-rpt.rpt = lv-prev-rpt 
                       AND bf-rpt.LINE > 99 NO-LOCK .

         CREATE new-rpt.
         BUFFER-COPY bf-rpt EXCEPT bf-rpt.rpt bf-rpt.rec_key TO new-rpt.
         ASSIGN new-rpt.rpt = gl-rpt.rpt
                new-rpt.rec_key = gl-rpt.rec_key.

     END.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement V-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN {&list-5} .
     ASSIGN v-vcol-1 v-vcol-2 v-vcol-3
            v-vcol-4 v-vcol-5 v-vcol-6.
  END.

  ASSIGN v-hdr[1] = v-hdr-1
       v-hdr[2] = v-hdr-2
       v-hdr[3] = v-hdr-3
       v-hdr[4] = v-hdr-4
       v-hdr[5] = v-hdr-5
       v-ch[1] = v-ch-1
       v-ch[2] = v-ch-2
       v-ch[3] = v-ch-3
       v-ch[4] = v-ch-4
       v-ch[5] = v-ch-5
       v-ch[6] = v-ch-6
       v-ch[7] = v-ch-7
       v-ch[8] = v-ch-8
       v-ch[9] = v-ch-9
       v-ct[1] = v-ct-1
       v-ct[2] = v-ct-2
       v-ct[3] = v-ct-3
       v-ct[4] = v-ct-4
       v-ct[5] = v-ct-5
       v-ct[6] = v-ct-6
       v-ct[7] = v-ct-7
       v-ct[8] = v-ct-8
       v-ct[9] = v-ct-9
       v-c-bs = lv-c-bs
       v-p-w = lv-p-w
       v-page-length = lv-page-length
       v-col-used = lv-col-used
       v-d-wid = lv-d-wid
       v-no-col = lv-no-col
       v-onevar = lv-onevar
       v-twovar = lv-twovar
       v-vcol[1] = v-vcol-1
       v-vcol[2] = v-vcol-2
       v-vcol[3] = v-vcol-3
       v-vcol[4] = v-vcol-4
       v-vcol[5] = v-vcol-5
       v-vcol[6] = v-vcol-6
       v-per[1] = v-per-1
       v-per[2] = v-per-2
       v-per[3] = v-per-3
       v-per[4] = v-per-4
       v-per[5] = v-per-5
       v-per[6] = v-per-6
       v-per[7] = v-per-7
       v-per[8] = v-per-8
       v-per[9] = v-per-9
       v-sub[1] = v-sub-1
       v-sub[2] = v-sub-2
       v-sub[3] = v-sub-3
       v-sub[4] = v-sub-4
       v-sub[5] = v-sub-5
       v-sub[6] = v-sub-6
       v-sub[7] = v-sub-7
       v-sub[8] = v-sub-8
       v-sub[9] = v-sub-9
       v-sub[10] = v-sub-10
       v-sub[11] = v-sub-11
       v-sub[12] = v-sub-12
       .

 v-rpt = gl-rpt.rpt.
 RUN gl/gl-rpta.p . 

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DISABLE {&list-5} WITH FRAME {&FRAME-NAME}.
  DISABLE lv-onevar v-vcol-1 v-vcol-2 v-vcol-3
          lv-twovar v-vcol-4 v-vcol-5 v-vcol-6
         WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF adm-adding-record THEN CLEAR FRAME {&FRAME-NAME}.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */


  ASSIGN gl-rpt.company = g_company
         lv-page-length = 66
         lv-p-w = 80.

  DISPLAY lv-page-length lv-p-w WITH FRAME {&FRAME-NAME}.

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
  IF AVAIL gl-rpt THEN DO:
     fil_id = recid(gl-rpt).
     v-rpt = gl-rpt.rpt.
     run gl/gl-rptg.p (input fil_id, input no).
     ASSIGN v-hdr-1 = v-hdr[1]
            v-hdr-2 = v-hdr[2]
            v-hdr-3 = v-hdr[3]
            v-hdr-4 = v-hdr[4]
            v-hdr-5 = v-hdr[5]
            v-ch-1 = v-ch[1]
            v-ch-2 = v-ch[2]
            v-ch-3 = v-ch[3]
            v-ch-4 = v-ch[4]
            v-ch-5 = v-ch[5]
            v-ch-6 = v-ch[6]
            v-ch-7 = v-ch[7]
            v-ch-8 = v-ch[8]
            v-ch-9 = v-ch[9]
            v-ct-1 = v-ct[1]
            v-ct-2 = v-ct[2]
            v-ct-3 = v-ct[3]
            v-ct-4 = v-ct[4]
            v-ct-5 = v-ct[5]
            v-ct-6 = v-ct[6]
            v-ct-7 = v-ct[7]
            v-ct-8 = v-ct[8]
            v-ct-9 = v-ct[9]
            lv-c-bs = v-c-bs
            lv-p-w = v-p-w
            lv-page-length = v-page-length
            lv-col-used = v-col-used
            lv-d-wid = v-d-wid
            lv-no-col = v-no-col
            lv-onevar = v-onevar
            lv-twovar = v-twovar
            v-vcol-1 = v-vcol[1]
            v-vcol-2 = v-vcol[2]
            v-vcol-3 = v-vcol[3]
            v-vcol-4 = v-vcol[4]
            v-vcol-5 = v-vcol[5]
            v-vcol-6 = v-vcol[6]            
            v-per-1 = v-per[1]
            v-per-2 = v-per[2]
            v-per-3 = v-per[3]
            v-per-4 = v-per[4]
            v-per-5 = v-per[5]
            v-per-6 = v-per[6]
            v-per-7 = v-per[7]
            v-per-8 = v-per[8]
            v-per-9 = v-per[9]
            v-sub-1 = v-sub[1]
            v-sub-2 = v-sub[2]
            v-sub-3 = v-sub[3]
            v-sub-4 = v-sub[4]
            v-sub-5 = v-sub[5]
            v-sub-6 = v-sub[6]
            v-sub-7 = v-sub[7]
            v-sub-8 = v-sub[8]
            v-sub-9 = v-sub[9]
            v-sub-10 = v-sub[10]
            v-sub-11 = v-sub[11]
            v-sub-12 = v-sub[12]
            .
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li-tmp-used AS INT NO-UNDO.
  DEF VAR ll-tmp-new AS LOG NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-rpt NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  /* ====== validation ======== */
  {&methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
     v-rpt = gl-rpt.rpt.

     IF (INPUT lv-d-wid + (INPUT lv-no-col * 15)) > INPUT lv-p-w THEN DO:
         MESSAGE "The page width for this report is to small. Must increase it."
                 VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO lv-d-wid.
         RETURN NO-APPLY.
     END.
     li-tmp-used = INT(lv-d-wid:SCREEN-VALUE) + (int(lv-no-col:SCREEN-VALUE) * 15) .
     DO i = 1 TO int(lv-no-col:SCREEN-VALUE) :
        IF i = 1 THEN {gl\glrpti1.i 1} 
        ELSE IF i = 2 THEN {gl\glrpti1.i 2} 
        ELSE IF i = 3 THEN {gl\glrpti1.i 3} 
        ELSE IF i = 4 THEN {gl\glrpti1.i 4} 
        ELSE IF i = 5 THEN {gl\glrpti1.i 5} 
        ELSE IF i = 6 THEN {gl\glrpti1.i 6} 
        ELSE IF i = 7 THEN {gl\glrpti1.i 7} 
        ELSE IF i = 8 THEN {gl\glrpti1.i 8} 
        ELSE IF i = 9 THEN {gl\glrpti1.i 9} 
     END.
     IF li-tmp-used > int(lv-p-w:SCREEN-VALUE) THEN DO:
        MESSAGE "The page width for this report is to small. Must increase it." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO lv-p-w.
        RETURN NO-APPLY.
     END.
  END.
  {&methods/lValidateError.i NO}

  RUN calc-var (1) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

   /* end validateion=============*/
  ll-tmp-new = adm-new-record.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DISABLE {&list-5} WITH FRAME {&FRAME-NAME}.

  IF ll-tmp-new THEN do:
     RUN dispatch ('open-query').
     RUN dispatch ('row-changed').
  END.
  DISABLE lv-onevar v-vcol-1 v-vcol-2 v-vcol-3
          lv-twovar v-vcol-4 v-vcol-5 v-vcol-6
         WITH FRAME {&FRAME-NAME}.

END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-copy V-table-Win 
PROCEDURE proc-copy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-copied AS LOG NO-UNDO.

  DEF VAR ls-focus AS CHAR NO-UNDO.


  IF AVAIL gl-rpt THEN DO WITH FRAME {&FRAME-NAME}:
    ls-focus = FOCUS:SCREEN-VALUE.

    APPLY "entry" TO FRAME {&FRAME-NAME}.

    RUN gl/copystmt.w (ROWID(gl-rpt), OUTPUT ll-copied).

    FOCUS:SCREEN-VALUE = ls-focus.

    IF ll-copied THEN RUN dispatch ("cancel-record").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-enable V-table-Win 
PROCEDURE proc-enable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ENABLE {&list-5} WITH FRAME {&FRAME-NAME}.

  DEF VAR i AS INT NO-UNDO.
  DO i = 1 TO 9:
     IF i > lv-no-col THEN CASE i:         
        WHEN 1 THEN DISABLE v-ch-1 v-ct-1 v-per-1 WITH FRAME {&FRAME-NAME}.
        WHEN 2 THEN DISABLE v-ch-2 v-ct-2 v-per-2 WITH FRAME {&FRAME-NAME}.
        WHEN 3 THEN DISABLE v-ch-3 v-ct-3 v-per-3 WITH FRAME {&FRAME-NAME}.
        WHEN 4 THEN DISABLE v-ch-4 v-ct-4 v-per-4 WITH FRAME {&FRAME-NAME}.
        WHEN 5 THEN DISABLE v-ch-5 v-ct-5 v-per-5 WITH FRAME {&FRAME-NAME}.
        WHEN 6 THEN DISABLE v-ch-6 v-ct-6 v-per-6 WITH FRAME {&FRAME-NAME}.
        WHEN 7 THEN DISABLE v-ch-7 v-ct-7 v-per-7 WITH FRAME {&FRAME-NAME}.
        WHEN 8 THEN DISABLE v-ch-8 v-ct-8 v-per-8 WITH FRAME {&FRAME-NAME}.
        WHEN 9 THEN DISABLE v-ch-9 v-ct-9 v-per-9 WITH FRAME {&FRAME-NAME}.
     END.
  END.

  RUN calc-var (0) NO-ERROR.

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
  {src/adm/template/snd-list.i "gl-rpt"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-rpt V-table-Win 
PROCEDURE valid-rpt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF gl-rpt.rpt:SCREEN-VALUE EQ "" THEN DO:
        MESSAGE "Report ID can not be blank. " VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO gl-rpt.rpt.
        RETURN ERROR.
    END.
    IF adm-new-record AND
       CAN-FIND(FIRST xx-rpt WHERE xx-rpt.company EQ g_company
                               AND xx-rpt.rpt     EQ gl-rpt.rpt:SCREEN-VALUE
                               AND ROWID(xx-rpt)  NE ROWID(gl-rpt))
    THEN DO:
      MESSAGE "GL Report " gl-rpt.rpt:SCREEN-VALUE " alredy exists." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO gl-rpt.rpt.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

