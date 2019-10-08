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

{custom/gcompany.i}
{custom/gloc.i}

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
&Scoped-define EXTERNAL-TABLES style
&Scoped-define FIRST-EXTERNAL-TABLE style


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR style.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS style.dscr style.type style.design-no ~
style.royalty style.dim-gl style.dim-tk style.dim-dkl style.dim-dkw ~
style.dim-pan5 style.dim-fit style.material[1] style.material[2] ~
style.material[3] style.material[4] style.material[5] style.material[6] ~
style.material[7] style.qty-per-set style.m-code[1] style.m-code[2] ~
style.m-code[3] style.m-code[4] style.m-code[5] style.m-code[6] ~
style.m-code[7] style.formula[9] style.formula[10] style.formula[11] ~
style.formula[12] style.formula[1] style.formula[2] style.formula[3] ~
style.formula[4] style.formula[5] style.formula[6] style.formula[7] ~
style.formula[8] style.use-w[2] style.use-w[3] style.use-w[4] ~
style.use-w[5] style.use-w[6] style.use-w[7] style.use-w[8] style.use-w[9] ~
style.use-w[10] style.use-w[11] style.use-w[12] style.use-w[13] ~
style.use-l[2] style.use-l[3] style.use-l[4] style.use-l[5] style.use-l[6] ~
style.use-l[7] style.use-l[8] style.use-l[9] style.use-l[10] ~
style.use-l[11] style.use-l[12] style.use-l[13] style.spare-char-5 
&Scoped-define ENABLED-TABLES style
&Scoped-define FIRST-ENABLED-TABLE style
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-10 RECT-11 RECT-7 RECT-8 RECT-9 
&Scoped-Define DISPLAYED-FIELDS style.style style.dscr style.type ~
style.design-no style.royalty style.dim-gl style.dim-tk style.dim-dkl ~
style.dim-dkw style.dim-pan5 style.dim-fit style.material[1] ~
style.material[2] style.material[3] style.material[4] style.material[5] ~
style.material[6] style.material[7] style.qty-per-set style.m-code[1] ~
style.m-dscr[1] style.m-code[2] style.m-dscr[2] style.m-code[3] ~
style.m-dscr[3] style.m-code[4] style.m-dscr[4] style.m-code[5] ~
style.m-dscr[5] style.m-code[6] style.m-dscr[6] style.m-code[7] ~
style.m-dscr[7] style.formula[9] style.formula[10] style.formula[11] ~
style.formula[12] style.formula[1] style.formula[2] style.formula[3] ~
style.formula[4] style.formula[5] style.formula[6] style.formula[7] ~
style.formula[8] style.use-w[2] style.use-w[3] style.use-w[4] ~
style.use-w[5] style.use-w[6] style.use-w[7] style.use-w[8] style.use-w[9] ~
style.use-w[10] style.use-w[11] style.use-w[12] style.use-w[13] ~
style.use-l[2] style.use-l[3] style.use-l[4] style.use-l[5] style.use-l[6] ~
style.use-l[7] style.use-l[8] style.use-l[9] style.use-l[10] ~
style.use-l[11] style.use-l[12] style.use-l[13] style.spare-char-5 
&Scoped-define DISPLAYED-TABLES style
&Scoped-define FIRST-DISPLAYED-TABLE style


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS style.style 
&Scoped-define ADM-ASSIGN-FIELDS style.m-dscr[1] style.m-dscr[2] ~
style.m-dscr[3] style.m-dscr[4] style.m-dscr[5] style.m-dscr[6] ~
style.m-dscr[7] 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
company||y|ASI.style.company
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "company"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 142 BY 18.1.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 56.4 BY 8.33.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 56.4 BY 4.52.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49 BY 6.91.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 31 BY 9.76.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49 BY 7.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     style.style AT ROW 1.24 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 FONT 4
     style.dscr AT ROW 1.24 COL 46.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 37 BY 1
          BGCOLOR 15 FONT 4
     style.type AT ROW 1.24 COL 93 COLON-ALIGNED HELP
          "'B'ox, 'D'ie Cut, 'P'ocket Folder, 'R'igid Box, 'S'ignature, 'J'ig Saw or '' "
          LABEL "Type" FORMAT "x(1)"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     style.design-no AT ROW 1.24 COL 112 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     style.royalty AT ROW 1.24 COL 130 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
          BGCOLOR 15 FONT 4
     style.dim-gl AT ROW 3.14 COL 26 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     style.dim-tk AT ROW 4.14 COL 26 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     style.dim-dkl AT ROW 5.14 COL 26 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     style.dim-dkw AT ROW 6.14 COL 26 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     style.dim-pan5 AT ROW 7.14 COL 26 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     style.dim-fit AT ROW 8.14 COL 26 COLON-ALIGNED
          LABEL "Lock Tab"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     style.material[1] AT ROW 3.14 COL 66 COLON-ALIGNED
          LABEL "Board"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     style.material[2] AT ROW 4.14 COL 66 COLON-ALIGNED
          LABEL "Ink"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     style.material[3] AT ROW 5.14 COL 66 COLON-ALIGNED
          LABEL "Ink Cov %"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     style.material[4] AT ROW 6.14 COL 66 COLON-ALIGNED
          LABEL "Film"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     style.material[5] AT ROW 7.14 COL 66 COLON-ALIGNED
          LABEL "Leaf"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     style.material[6] AT ROW 8.14 COL 66 COLON-ALIGNED
          LABEL "Coating"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     style.material[7] AT ROW 9.1 COL 66 COLON-ALIGNED
          LABEL "Adhesive"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     style.qty-per-set AT ROW 10.05 COL 66 COLON-ALIGNED
          LABEL "Qty/Set"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 
     style.m-code[1] AT ROW 10.62 COL 5.2 COLON-ALIGNED
          LABEL "1"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     style.m-dscr[1] AT ROW 10.62 COL 19.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 FONT 4
     style.m-code[2] AT ROW 11.57 COL 5.2 COLON-ALIGNED
          LABEL "2"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     style.m-dscr[2] AT ROW 11.62 COL 19.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 FONT 4
     style.m-code[3] AT ROW 12.52 COL 5.2 COLON-ALIGNED
          LABEL "3"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     style.m-dscr[3] AT ROW 12.62 COL 19.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 FONT 4
     style.m-code[4] AT ROW 13.48 COL 5.2 COLON-ALIGNED
          LABEL "4"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     style.m-dscr[4] AT ROW 13.62 COL 19.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 FONT 4
     style.m-code[5] AT ROW 14.43 COL 5.2 COLON-ALIGNED
          LABEL "5"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1.19
          BGCOLOR 15 FONT 4
     style.m-dscr[5] AT ROW 14.62 COL 19.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 FONT 4
     style.m-code[6] AT ROW 15.62 COL 5.2 COLON-ALIGNED
          LABEL "6"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     style.m-dscr[6] AT ROW 15.62 COL 19.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 FONT 4
     style.m-code[7] AT ROW 16.57 COL 5.2 COLON-ALIGNED
          LABEL "7"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     style.m-dscr[7] AT ROW 16.57 COL 19.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 FONT 4
     style.formula[9] AT ROW 3.14 COL 106 COLON-ALIGNED
          LABEL "Lid Len ="
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     style.formula[10] AT ROW 4.05 COL 106 COLON-ALIGNED
          LABEL "Lid Wid ="
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     style.formula[11] AT ROW 5.05 COL 106 COLON-ALIGNED
          LABEL "Lid Die ="
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     style.formula[12] AT ROW 6.05 COL 106 COLON-ALIGNED
          LABEL "Box Die ="
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     style.formula[1] AT ROW 7.86 COL 106.4 COLON-ALIGNED
          LABEL "#1 Lower Left Wid"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     style.formula[2] AT ROW 8.81 COL 106.4 COLON-ALIGNED
          LABEL "Len"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     style.formula[3] AT ROW 9.76 COL 106.4 COLON-ALIGNED
          LABEL "#2     Nesting Wid"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     style.formula[4] AT ROW 10.71 COL 106.4 COLON-ALIGNED
          LABEL "Len"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     style.formula[5] AT ROW 11.67 COL 106.4 COLON-ALIGNED
          LABEL "#3     Stagger Wid"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     style.formula[6] AT ROW 12.62 COL 106.4 COLON-ALIGNED
          LABEL "Len"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     style.formula[7] AT ROW 13.57 COL 106.4 COLON-ALIGNED
          LABEL "Sq Inches Wid"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     style.formula[8] AT ROW 14.52 COL 106.4 COLON-ALIGNED
          LABEL "Len"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     style.use-w[2] AT ROW 16.67 COL 92 COLON-ALIGNED
          LABEL "Formula"
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-w[3] AT ROW 16.67 COL 96 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-w[4] AT ROW 16.67 COL 100 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-w[5] AT ROW 16.67 COL 104 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-w[6] AT ROW 16.67 COL 108 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-w[7] AT ROW 16.67 COL 112 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-w[8] AT ROW 16.67 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-w[9] AT ROW 16.67 COL 120 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-w[10] AT ROW 16.67 COL 124 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-w[11] AT ROW 16.67 COL 128 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-w[12] AT ROW 16.67 COL 132 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-w[13] AT ROW 16.67 COL 136 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-l[2] AT ROW 17.81 COL 92 COLON-ALIGNED
          LABEL "Formula"
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-l[3] AT ROW 17.81 COL 96 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-l[4] AT ROW 17.81 COL 100 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-l[5] AT ROW 17.81 COL 104 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-l[6] AT ROW 17.81 COL 108 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-l[7] AT ROW 17.81 COL 112 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     style.use-l[8] AT ROW 17.81 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-l[9] AT ROW 17.81 COL 120 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-l[10] AT ROW 17.81 COL 124 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-l[11] AT ROW 17.81 COL 128 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-l[12] AT ROW 17.81 COL 132 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-l[13] AT ROW 17.81 COL 136 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.spare-char-5 AT ROW 11 COL 66 COLON-ALIGNED WIDGET-ID 2
          LABEL "Pack Code"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 
     "  Default Dimensions" VIEW-AS TEXT 
          SIZE 24 BY .62 AT ROW 2.43 COL 15
          FGCOLOR 9 
     "  Die && Rule Formulas" VIEW-AS TEXT
          SIZE 25 BY .62 AT ROW 2.43 COL 104
          FGCOLOR 9 
     "  Layout Formulas" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 7.17 COL 103.4
          FGCOLOR 9 
     "  Default Machine Routing" VIEW-AS TEXT
          SIZE 30 BY .62 AT ROW 9.91 COL 14
          FGCOLOR 9 
     "  Default Material Codes" VIEW-AS TEXT
          SIZE 28 BY .70 AT ROW 2.43 COL 55
          FGCOLOR 9 
     "6" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 15.91 COL 111
     "3" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 15.91 COL 99
     "# On Len" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 18 COL 71.6
     "# On Wid" VIEW-AS TEXT
          SIZE 10.8 BY .62 AT ROW 16.86 COL 71.8
     "9" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 15.91 COL 123
     "5" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 15.91 COL 107
     "8" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 15.91 COL 119
     "7" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 15.91 COL 115
     "4" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 15.91 COL 103
     "13" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 15.91 COL 138
     "12" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 15.91 COL 134
     "2" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 15.91 COL 95
     "11" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 15.91 COL 130
     "10" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 15.91 COL 126
     RECT-1 AT ROW 1.1 COL 2
     RECT-10 AT ROW 7.38 COL 85
     RECT-11 AT ROW 2.67 COL 85
     RECT-7 AT ROW 2.67 COL 3
     RECT-8 AT ROW 2.67 COL 53
     RECT-9 AT ROW 10.14 COL 3.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.style
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
         HEIGHT             = 18.48
         WIDTH              = 143.6.
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

/* SETTINGS FOR FILL-IN style.dim-fit IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.formula[10] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.formula[11] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.formula[12] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.formula[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.formula[2] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.formula[3] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.formula[4] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.formula[5] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.formula[6] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.formula[7] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.formula[8] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.formula[9] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.m-code[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.m-code[2] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.m-code[3] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.m-code[4] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.m-code[5] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.m-code[6] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.m-code[7] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.m-dscr[1] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN style.m-dscr[2] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN style.m-dscr[3] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN style.m-dscr[4] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN style.m-dscr[5] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN style.m-dscr[6] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN style.m-dscr[7] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN style.material[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.material[2] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.material[3] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.material[4] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.material[5] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.material[6] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.material[7] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.qty-per-set IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.spare-char-5 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.style IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN style.type IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN style.use-l[2] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.use-w[2] IN FRAME F-Main
   EXP-LABEL                                                            */
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
   DEF VAR ls-cur-val AS cha NO-UNDO.
   DEF VAR lv-foam AS LOG NO-UNDO.
   DEF VAR lv-icode AS cha NO-UNDO.
   DEF VAR lv-mat-type AS cha NO-UNDO.
   DEF VAR lv-rowid AS ROWID NO-UNDO.
   DEF VAR lv-ind LIKE style.industry NO-UNDO.

   CASE FOCUS:NAME :
        WHEN 'design-no' THEN DO:
             RUN windows/l-boxdes.w (FOCUS:SCREEN-VALUE, OUTPUT char-val).
             IF char-val <> "" THEN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).    
        END.
       WHEN 'type' THEN DO:
           RUN windows/l-typcod.w (OUTPUT char-val).
             IF char-val <> "" THEN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).   

             IF FOCUS:SCREEN-VALUE EQ "P" THEN DO:
                 ASSIGN
                     style.material[1]:LABEL = "Paper" 
                     style.dim-fit:LABEL = "Pocket Size" 
                     style.dim-tk:LABEL = "Tab" 
                     style.material[7]:LABEL = "P Adhesive"
                     style.dim-gl:LABEL = "Pocket  Glue Width" 
                     style.dim-dkl:LABEL = "Spine Capacity" 
                     style.dim-dkw:LABEL = "Pocket Capacity".
             END.
             ELSE DO:
                 ASSIGN
                     style.material[1]:LABEL = "Board" 
                     style.dim-fit:LABEL = "Lock Tab" 
                     style.dim-tk:LABEL = "Tuck" 
                     style.material[7]:LABEL = "Adhesive"
                     style.dim-gl:LABEL = "Glue lap" 
                     style.dim-dkl:LABEL = "DK Length" 
                     style.dim-dkw:LABEL  = "DK Width" .
             END.

       END.
       WHEN 'material' THEN DO:
           CASE FOCUS:INDEX:
              WHEN 1 THEN DO: /*board*/

                 ASSIGN
                   ls-cur-val = FOCUS:SCREEN-VALUE
                   lv-ind = style.industry.

                 IF style.type = "f" THEN  /* foam */
                 DO:
                    RUN windows/l-boardf.w (style.company,lv-ind,ls-cur-val,OUTPUT char-val).
                    FOCUS:SCREEN-VALUE IN FRAME {&frame-name} = char-val.
                 END.
                 ELSE
                 DO:
                    RUN windows/l-board1.w (style.company,lv-ind,ls-cur-val, OUTPUT lv-rowid).
                    FIND FIRST ITEM WHERE ROWID(item) EQ lv-rowid NO-LOCK NO-ERROR.
                    IF AVAIL ITEM AND ITEM.i-no NE FOCUS:SCREEN-VALUE THEN 
                       FOCUS:SCREEN-VALUE IN FRAME {&frame-name} = item.i-no.
                 END.
              END.

              WHEN 2 THEN DO: /*ink*/
                 lv-ind = style.industry.
                 RUN windows/l-item2.w (style.company, lv-ind, "I",FOCUS:SCREEN-VALUE, OUTPUT char-val).
                 IF char-val <> "" THEN
                    FOCUS:SCREEN-VALUE IN FRAME {&frame-name} = entry(1,char-val).
              END.

              WHEN 4 THEN DO: /* film */
                 lv-ind = style.industry.
                 RUN windows/l-item.w (style.company,lv-ind,"F,W",FOCUS:SCREEN-VALUE,OUTPUT char-val).
                     IF char-val NE "" AND FOCUS:SCREEN-VALUE NE ENTRY(1,char-val) THEN
                        FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
              END.
              WHEN 5 THEN DO: /* leaf */
                    lv-ind = style.industry.
                    lv-mat-type =  "WLF".
                    RUN windows/l-itmsty.w (style.company, lv-ind, lv-mat-type, FOCUS:SCREEN-VALUE,lv-foam,lv-icode,
                                     OUTPUT char-val).
                    IF char-val <> "" THEN 
                       FOCUS:SCREEN-VALUE IN FRAME {&frame-name} = entry(1,char-val).
              END.

              WHEN 6 THEN DO: /* coating */
                 ASSIGN
                   lv-ind = style.industry
                   lv-mat-type =  "V".
                 RUN windows/l-itmsty.w (style.company, lv-ind, lv-mat-type, FOCUS:SCREEN-VALUE,lv-foam,lv-icode,
                                         OUTPUT char-val).
                 IF char-val <> "" THEN 
                    FOCUS:SCREEN-VALUE IN FRAME {&frame-name} = entry(1,char-val).
              END.

              WHEN 7 THEN DO: /*adhesive*/
                 RUN windows/l-item.w (style.company,"","G,T",FOCUS:SCREEN-VALUE,OUTPUT char-val).
                 IF char-val <> "" THEN FOCUS:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,char-val).
              END.
           END CASE.
       END.


        /*
         when 'material' then do:
             case focus:index :
                  when 1 then do:  /* board */
                         def var lv-ind like style.industry no-undo.
                         ls-cur-val = focus:screen-value.
                         if avail style then lv-ind = style.industry.
                         else lv-ind = "".  
                         if avail style and style.type:screen-value = "f" then  /* foam */
                            run windows/l-boardf.w (style.company,lv-ind,ls-cur-val,output char-val).
                         else run windows/l-board1.w (eb.company,lv-ind,focus:screen-value, output lv-rowid).
                         FIND FIRST ITEM WHERE ROWID(item) EQ lv-rowid NO-LOCK NO-ERROR.
                         IF AVAIL ITEM AND ITEM.i-no NE FOCUS:SCREEN-VALUE THEN DO:
                            assign focus:screen-value in frame {&frame-name} = item.i-no 
                            .
                        return no-apply.                  
                  end.
                  when 4 then do:  /* adder */
                    if avail style then assign lv-ind = style.industry
                                               lv-foam = style.type:screen-value = "F" .
                    else assign lv-ind = ""
                                lv-foam = no.
                    run windows/l-boarda.w (style.company,lv-ind, focus:screen-value, output char-val).
                    if char-val <> "" then 
                        assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                      .
                    return no-apply.

                  end.
                  /*=====????
                  when 2 or when 5 or when 6 or when 7 then do:
                     if avail style then assign lv-ind = style.industry
                                                lv-foam = style.type:screen-value = "F" .
                     else assign lv-ind = ""
                                 lv-foam = no
                                 .
                     lv-mat-type = if lv-foam then "1234" else "BPR".             
                     lv-icode = "B".    
                  /*   do while true:
                       message "Please enter item code ('R'eal, 'E'st or 'B'oth) " update lv-icode .
                       if index("REB", lv-icode) > 0 then leave. 
                     end.
                  */  
                     run windows/l-itmsty.w (style.company, lv-ind, lv-mat-type, focus:screen-value,lv-foam,lv-icode,
                                      output char-val).
                     if char-val <> "" then 
                         assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                         .
                     return no-apply.

                  end.
                  */
                  when 2 then do:
                     if avail style then assign lv-ind = style.industry.
                     else assign lv-ind = ""                     .
                     lv-mat-type =  "I".
                     run windows/l-itmsty.w (style.company, lv-ind, lv-mat-type, focus:screen-value,lv-foam,lv-icode,
                                      output char-val).
                     if char-val <> "" then 
                         assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                         .
                     return no-apply.
                  end.
                  when 5 then do:
                     if avail style then assign lv-ind = style.industry.
                     else assign lv-ind = ""                     .
                     lv-ind = "".
                     lv-mat-type =  "WLF".
                     run windows/l-itmsty.w (style.company, lv-ind, lv-mat-type, focus:screen-value,lv-foam,lv-icode,
                                      output char-val).
                     if char-val <> "" then 
                         assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                         .
                     return no-apply.
                  end.
                  when 6 then do:
                     if avail style then assign lv-ind = style.industry.
                     else assign lv-ind = ""                     .
                     lv-ind = "".
                     lv-mat-type =  "V".
                     run windows/l-itmsty.w (style.company, lv-ind, lv-mat-type, focus:screen-value,lv-foam,lv-icode,
                                      output char-val).
                     if char-val <> "" then 
                         assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                         .
                     return no-apply.
                  end.
                  when 7 then do:
                     if avail style then assign lv-ind = style.industry.
                     else assign lv-ind = ""                     .
                     lv-ind = "".
                     lv-mat-type =  "GTS".
                     run windows/l-itmsty.w (style.company, lv-ind, lv-mat-type, focus:screen-value,lv-foam,lv-icode,
                                      output char-val).
                     if char-val <> "" then 
                         assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                         .
                     return no-apply.
                  end.

             end case.
        end.  /* material*/
        when 'm-code' then do:
             run windows/l-mach.w (style.company, gloc, focus:screen-value, output char-val).
             if char-val <> "" then 
                case focus:index :
                     when 1 then assign style.m-code[1]:screen-value in frame {&frame-name} = entry(1,char-val)
                                        style.m-dscr[1]:screen-value = entry(2,char-val).
                     when 2 then assign style.m-code[2]:screen-value = entry(1,char-val)
                                        style.m-dscr[2]:screen-value = entry(2,char-val).
                     when 3 then assign style.m-code[3]:screen-value = entry(1,char-val)
                                        style.m-dscr[3]:screen-value = entry(2,char-val).
                     when 4 then assign style.m-code[4]:screen-value = entry(1,char-val)
                                        style.m-dscr[4]:screen-value = entry(2,char-val).
                     when 5 then assign style.m-code[5]:screen-value = entry(1,char-val)
                                        style.m-dscr[5]:screen-value = entry(2,char-val).
                     when 6 then assign style.m-code[6]:screen-value = entry(1,char-val)
                                        style.m-dscr[6]:screen-value = entry(2,char-val).
                     when 7 then assign style.m-code[7]:screen-value = entry(1,char-val)
                                        style.m-dscr[7]:screen-value = entry(2,char-val).

                end case.
        end.  /* m-code */
        */
         WHEN 'm-code' THEN DO:
             RUN windows/l-mach.w (style.company, gloc, FOCUS:SCREEN-VALUE, OUTPUT char-val).
             IF char-val <> "" THEN 
                CASE FOCUS:INDEX :
                     WHEN 1 THEN ASSIGN style.m-code[1]:screen-value IN FRAME {&frame-name} = ENTRY(1,char-val)
                                        style.m-dscr[1]:screen-value = ENTRY(2,char-val).
                     WHEN 2 THEN ASSIGN style.m-code[2]:screen-value = ENTRY(1,char-val)
                                        style.m-dscr[2]:screen-value = ENTRY(2,char-val).
                     WHEN 3 THEN ASSIGN style.m-code[3]:screen-value = ENTRY(1,char-val)
                                        style.m-dscr[3]:screen-value = ENTRY(2,char-val).
                     WHEN 4 THEN ASSIGN style.m-code[4]:screen-value = ENTRY(1,char-val)
                                        style.m-dscr[4]:screen-value = ENTRY(2,char-val).
                     WHEN 5 THEN ASSIGN style.m-code[5]:screen-value = ENTRY(1,char-val)
                                        style.m-dscr[5]:screen-value = ENTRY(2,char-val).
                     WHEN 6 THEN ASSIGN style.m-code[6]:screen-value = ENTRY(1,char-val)
                                        style.m-dscr[6]:screen-value = ENTRY(2,char-val).
                     WHEN 7 THEN ASSIGN style.m-code[7]:screen-value = ENTRY(1,char-val)
                                        style.m-dscr[7]:screen-value = ENTRY(2,char-val).

                END CASE.
        END.  /* m-code */

       WHEN "spare-char-5" THEN /*Pack Code*/
           DO:
               RUN windows/l-item.w (style.company, "1", "C", FOCUS:SCREEN-VALUE, OUTPUT char-val).
               IF char-val NE "" AND FOCUS:SCREEN-VALUE NE ENTRY(1,char-val) THEN 
               DO:
                   FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
                   APPLY "value-changed" TO style.spare-char-5.
               END.
           END.  
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.m-code[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.m-code[1] V-table-Win
ON LEAVE OF style.m-code[1] IN FRAME F-Main /* 1 */
DO:
    {&methods/lValidateError.i YES}
    IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
       NOT CAN-FIND(FIRST mach WHERE mach.company = style.company AND 
                  mach.m-code= SELF:screen-value )
    THEN DO:
       MESSAGE "Invalid Machine Code. Try help please." VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.

    FIND FIRST mach WHERE mach.company = style.company AND 
                          mach.m-code= SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAIL mach THEN style.m-dscr[1]:SCREEN-VALUE = mach.m-dscr.
    ELSE style.m-dscr[1]:SCREEN-VALUE = "".
    {&methods/lValidateError.i NO}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.m-code[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.m-code[2] V-table-Win
ON LEAVE OF style.m-code[2] IN FRAME F-Main /* 2 */
DO:
    {&methods/lValidateError.i YES}
    IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
       NOT CAN-FIND(FIRST mach WHERE mach.company = style.company AND 
                  mach.m-code= SELF:screen-value )
    THEN DO:
       MESSAGE "Invalid Machine Code. Try help please." VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.

    FIND FIRST mach WHERE mach.company = style.company AND 
                          mach.m-code= SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAIL mach THEN style.m-dscr[2]:SCREEN-VALUE = mach.m-dscr.
    ELSE style.m-dscr[2]:SCREEN-VALUE = "".
    {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.m-code[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.m-code[3] V-table-Win
ON LEAVE OF style.m-code[3] IN FRAME F-Main /* 3 */
DO:
    {&methods/lValidateError.i YES}
    IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
       NOT CAN-FIND(FIRST mach WHERE mach.company = style.company AND 
                  mach.m-code= SELF:screen-value )
    THEN DO:
       MESSAGE "Invalid Machine Code. Try help please." VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.

    FIND FIRST mach WHERE mach.company = style.company AND 
                          mach.m-code= SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAIL mach THEN style.m-dscr[3]:SCREEN-VALUE = mach.m-dscr.
    ELSE style.m-dscr[3]:SCREEN-VALUE = "".
    {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.m-code[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.m-code[4] V-table-Win
ON LEAVE OF style.m-code[4] IN FRAME F-Main /* 4 */
DO:
    {&methods/lValidateError.i YES}
    IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
       NOT CAN-FIND(FIRST mach WHERE mach.company = style.company AND 
                  mach.m-code= SELF:screen-value )
    THEN DO:
       MESSAGE "Invalid Machine Code. Try help please." VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.

    FIND FIRST mach WHERE mach.company = style.company AND 
                         mach.m-code= SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF AVAIL mach THEN style.m-dscr[4]:SCREEN-VALUE = mach.m-dscr.
   ELSE style.m-dscr[4]:SCREEN-VALUE = "".
   {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.m-code[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.m-code[5] V-table-Win
ON LEAVE OF style.m-code[5] IN FRAME F-Main /* 5 */
DO:
    {&methods/lValidateError.i YES}
    IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
       NOT CAN-FIND(FIRST mach WHERE mach.company = style.company AND 
                  mach.m-code= SELF:screen-value )
    THEN DO:
       MESSAGE "Invalid Machine Code. Try help please." VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.

    FIND FIRST mach WHERE mach.company = style.company AND 
                          mach.m-code= SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAIL mach THEN style.m-dscr[5]:SCREEN-VALUE = mach.m-dscr.
    ELSE style.m-dscr[5]:SCREEN-VALUE = "".

    {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.m-code[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.m-code[6] V-table-Win
ON LEAVE OF style.m-code[6] IN FRAME F-Main /* 6 */
DO:
    {&methods/lValidateError.i YES}
    IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
       NOT CAN-FIND(FIRST mach WHERE mach.company = style.company AND 
                  mach.m-code= SELF:screen-value )
    THEN DO:
       MESSAGE "Invalid Machine Code. Try help please." VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.

    FIND FIRST mach WHERE mach.company = style.company AND 
                         mach.m-code= SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF AVAIL mach THEN style.m-dscr[6]:SCREEN-VALUE = mach.m-dscr.
   ELSE style.m-dscr[6]:SCREEN-VALUE = "".
   {&methods/lValidateError.i NO}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.m-code[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.m-code[7] V-table-Win
ON LEAVE OF style.m-code[7] IN FRAME F-Main /* 7 */
DO:
    {&methods/lValidateError.i YES}
    IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
       NOT CAN-FIND(FIRST mach WHERE mach.company = style.company AND 
                  mach.m-code= SELF:screen-value )
    THEN DO:
       MESSAGE "Invalid Machine Code. Try help please." VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.

    FIND FIRST mach WHERE mach.company = style.company AND 
                         mach.m-code= SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF AVAIL mach THEN style.m-dscr[7]:SCREEN-VALUE = mach.m-dscr.
   ELSE style.m-dscr[7]:SCREEN-VALUE = "".
   {&methods/lValidateError.i NO}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.type V-table-Win
ON LEAVE OF style.type IN FRAME F-Main /* Type */
DO:
  IF LASTKEY NE -1 THEN DO:

    SELF:SCREEN-VALUE = CAPS(SELF:screen-value) .

    RUN valid-type NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    IF SELF:SCREEN-VALUE EQ "P" THEN DO:
        ASSIGN
            style.material[1]:LABEL = "Paper" 
            style.dim-fit:LABEL = "Pocket Size" 
            style.dim-tk:LABEL = "Tab" 
            style.material[7]:LABEL = "P Adhesive"
            style.dim-gl:LABEL = "Pocket  Glue Width" 
            style.dim-dkl:LABEL = "Spine Capacity" 
            style.dim-dkw:LABEL = "Pocket Capacity".
    END.
    ELSE DO:
        ASSIGN
            style.material[1]:LABEL = "Board" 
            style.dim-fit:LABEL = "Lock Tab" 
            style.dim-tk:LABEL = "Tuck" 
            style.material[7]:LABEL = "Adhesive"
            style.dim-gl:LABEL = "Glue lap" 
            style.dim-dkl:LABEL = "DK Length" 
            style.dim-dkw:LABEL  = "DK Width" .
    END.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.type V-table-Win
ON VALUE-CHANGED OF style.type IN FRAME F-Main /* Type */
DO:
  IF LASTKEY NE -1 THEN DO:

    SELF:SCREEN-VALUE = CAPS(SELF:screen-value) .

    IF SELF:SCREEN-VALUE EQ "P" THEN DO:
        ASSIGN
            style.material[1]:LABEL = "Paper" 
            style.dim-fit:LABEL = "Pocket Size" 
            style.dim-tk:LABEL = "Tab" 
            style.material[7]:LABEL = "P Adhesive"
            style.dim-gl:LABEL = "Pocket  Glue Width"
            style.dim-dkl:LABEL = "Spine Capacity" 
            style.dim-dkw:LABEL = "Pocket Capacity" .
    END.
    ELSE DO:
        ASSIGN
            style.material[1]:LABEL = "Board" 
            style.dim-fit:LABEL = "Lock Tab" 
            style.dim-tk:LABEL = "Tuck" 
            style.material[7]:LABEL = "Adhesive"
            style.dim-gl:LABEL = "Glue lap" 
            style.dim-dkl:LABEL = "DK Length" 
            style.dim-dkw:LABEL  = "DK Width" .
    END.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}
SESSION:DATA-ENTRY-RETURN = YES.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

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
  {src/adm/template/row-list.i "style"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "style"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-xl V-table-Win 
PROCEDURE export-xl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE lcStylFrom AS CHAR NO-UNDO.
DEFINE VARIABLE lcStylTo   AS CHAR NO-UNDO.

IF style.style NE "" THEN
    ASSIGN
        lcStylFrom = style.style
        lcStylTo = style.style .

RUN fg/stlf-exp.w (lcStylFrom,
                       lcStylTo).


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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   style.company  = gcompany
   style.industry = "1".  /* folding style*/

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



IF AVAIL style AND style.TYPE EQ "P" THEN DO:

        ASSIGN
            style.material[1]:LABEL IN FRAME {&FRAME-NAME} = "Paper" 
            style.dim-fit:LABEL IN FRAME {&FRAME-NAME} = "Pocket Size" 
            style.dim-tk:LABEL IN FRAME {&FRAME-NAME} = "Tab" 
            style.material[7]:LABEL IN FRAME {&FRAME-NAME} = "P Adhesive"
            style.dim-gl:LABEL IN FRAME {&FRAME-NAME} = "Pocket  Glue Width"
            style.dim-dkl:LABEL IN FRAME {&FRAME-NAME} = "Spine Capacity" 
            style.dim-dkw:LABEL IN FRAME {&FRAME-NAME} = "Pocket Capacity" .
    END.
    ELSE DO:
        ASSIGN
            style.material[1]:LABEL IN FRAME {&FRAME-NAME} = "Board" 
            style.dim-fit:LABEL IN FRAME {&FRAME-NAME} = "Lock Tab" 
            style.dim-tk:LABEL IN FRAME {&FRAME-NAME} = "Tuck" 
            style.material[7]:LABEL IN FRAME {&FRAME-NAME} = "Adhesive"
            style.dim-gl:LABEL IN FRAME {&FRAME-NAME}= "Glue lap"
            style.dim-dkl:LABEL IN FRAME {&FRAME-NAME} = "DK Length" 
            style.dim-dkw:LABEL IN FRAME {&FRAME-NAME} = "DK Width"   .
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
  DEF VAR i AS INT NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DEF VAR char-hdl AS cha NO-UNDO.

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-target",OUTPUT char-hdl).
  DO i = 1 TO NUM-ENTRIES(char-hdl):
     RUN reopen-query IN WIDGET-HANDLE(ENTRY(i,char-hdl)).
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "company" "style" "company"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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
  {src/adm/template/snd-list.i "style"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-type V-table-Win 
PROCEDURE valid-type :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:

      IF style.type:SCREEN-VALUE NE "" THEN DO:
          IF NOT CAN-DO("B,D,P,R,S,J",style.type:SCREEN-VALUE) THEN DO:
              MESSAGE "Invalid Type. Type must be 'B'ox, 'D'ie Cut, 'P'ocket Folder, 'R'igid Box, 'S'ignature or 'J'ig Saw..."
                  VIEW-AS ALERT-BOX ERROR.
              RETURN ERROR.
          END.
      END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

