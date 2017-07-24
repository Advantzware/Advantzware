&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/prod.w

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
{custom/format.i}

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
&Scoped-define EXTERNAL-TABLES prod
&Scoped-define FIRST-EXTERNAL-TABLE prod


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR prod.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS prod.dscr prod.wip-mat prod.wip-lab ~
prod.wip-vo prod.wip-fo prod.fg-mat prod.fg-lab prod.fg-vo prod.fg-fo ~
prod.cgs-mat prod.cgs-mu prod.cgs-dl prod.cgs-dlv prod.cgs-vo prod.cgs-vov ~
prod.cgs-fo prod.cgs-fov prod.aa-mat prod.aa-lab prod.aa-vo prod.aa-fo 
&Scoped-define ENABLED-TABLES prod
&Scoped-define FIRST-ENABLED-TABLE prod
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-5 RECT-6 RECT-7 RECT-8 
&Scoped-Define DISPLAYED-FIELDS prod.prolin prod.dscr prod.wip-mat ~
prod.wip-lab prod.wip-vo prod.wip-fo prod.fg-mat prod.fg-lab prod.fg-vo ~
prod.fg-fo prod.cgs-mat prod.cgs-mu prod.cgs-dl prod.cgs-dlv prod.cgs-vo ~
prod.cgs-vov prod.cgs-fo prod.cgs-fov prod.aa-mat prod.aa-lab prod.aa-vo ~
prod.aa-fo 
&Scoped-define DISPLAYED-TABLES prod
&Scoped-define FIRST-DISPLAYED-TABLE prod
&Scoped-Define DISPLAYED-OBJECTS F1 F-2 F-3 F-4 F-5 F-6 F-7 F-8 F-13 F-14 ~
F-15 F-16 F-17 F-18 F-19 F-20 F-9 F-10 F-11 F-12 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS prod.prolin 
&Scoped-define List-5 prod.wip-mat prod.wip-lab prod.wip-vo prod.wip-fo ~
prod.fg-mat prod.fg-lab prod.fg-vo prod.fg-fo prod.cgs-mat prod.cgs-mu ~
prod.cgs-dl prod.cgs-dlv prod.cgs-vo prod.cgs-vov prod.cgs-fo prod.cgs-fov ~
prod.aa-mat prod.aa-lab prod.aa-vo prod.aa-fo 
&Scoped-define F1 F1 F-2 F-3 F-4 F-5 F-6 F-7 F-8 F-13 F-14 F-15 F-16 F-17 ~
F-18 F-19 F-20 F-9 F-10 F-11 F-12 

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
DEFINE VARIABLE F-10 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-11 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-12 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-13 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-14 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-15 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-16 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-17 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-18 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-19 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-2 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-20 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-3 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-4 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-5 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-6 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-7 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-8 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-9 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F1 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 127 BY 16.91.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 55 BY 5.71.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 55 BY 5.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 5.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 69 BY 9.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     prod.prolin AT ROW 1.24 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 FONT 4
     prod.dscr AT ROW 1.24 COL 56 COLON-ALIGNED
          LABEL "Description"
          VIEW-AS FILL-IN 
          SIZE 56 BY 1
          BGCOLOR 15 FONT 4
     prod.wip-mat AT ROW 2.91 COL 20 COLON-ALIGNED
          LABEL "Material"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     prod.wip-lab AT ROW 4.1 COL 20 COLON-ALIGNED
          LABEL "Labor"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     prod.wip-vo AT ROW 5.29 COL 20 COLON-ALIGNED
          LABEL "Var. Overhead"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     prod.wip-fo AT ROW 6.48 COL 20 COLON-ALIGNED
          LABEL "Fixed Overhead"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     prod.fg-mat AT ROW 8.62 COL 20 COLON-ALIGNED
          LABEL "Material"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     prod.fg-lab AT ROW 9.81 COL 20 COLON-ALIGNED
          LABEL "Labor"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     prod.fg-vo AT ROW 11 COL 20 COLON-ALIGNED
          LABEL "Var. Overhead"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     prod.fg-fo AT ROW 12.19 COL 20 COLON-ALIGNED
          LABEL "Fixed Overhead"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     prod.cgs-mat AT ROW 2.91 COL 90 COLON-ALIGNED
          LABEL "Material"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     prod.cgs-mu AT ROW 4.1 COL 90 COLON-ALIGNED
          LABEL "Mat. Usage Variance"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     prod.cgs-dl AT ROW 5.29 COL 90 COLON-ALIGNED
          LABEL "Direct Labor"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     prod.cgs-dlv AT ROW 6.48 COL 90 COLON-ALIGNED
          LABEL "Dir. Labor Variance"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     prod.cgs-vo AT ROW 7.67 COL 90 COLON-ALIGNED
          LABEL "Variable Overhead"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     prod.cgs-vov AT ROW 8.86 COL 90 COLON-ALIGNED
          LABEL "Var. Ovrhd Effcy Variance"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     prod.cgs-fo AT ROW 10.05 COL 90 COLON-ALIGNED
          LABEL "Fixed Overhead"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     prod.cgs-fov AT ROW 11.24 COL 90 COLON-ALIGNED
          LABEL "Fixed Ovrhd Effcy Variance"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     prod.aa-mat AT ROW 12.91 COL 90 COLON-ALIGNED
          LABEL "Material"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     prod.aa-lab AT ROW 14.1 COL 90 COLON-ALIGNED
          LABEL "Labor"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     prod.aa-vo AT ROW 15.29 COL 90 COLON-ALIGNED
          LABEL "Var. Overhead"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     prod.aa-fo AT ROW 16.48 COL 90 COLON-ALIGNED
          LABEL "Fixed Overhead"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     F1 AT ROW 2.91 COL 54 NO-LABEL
     F-2 AT ROW 4.1 COL 54 NO-LABEL
     F-3 AT ROW 5.29 COL 54 NO-LABEL
     F-4 AT ROW 6.48 COL 54 NO-LABEL
     F-5 AT ROW 8.14 COL 54 NO-LABEL
     F-6 AT ROW 9.33 COL 54 NO-LABEL
     F-7 AT ROW 10.52 COL 54 NO-LABEL
     F-8 AT ROW 11.71 COL 54 NO-LABEL
     F-13 AT ROW 2.91 COL 124 NO-LABEL
     F-14 AT ROW 4.1 COL 124 NO-LABEL
     F-15 AT ROW 5.29 COL 124 NO-LABEL
     F-16 AT ROW 6.48 COL 124 NO-LABEL
     F-17 AT ROW 7.67 COL 124 NO-LABEL
     F-18 AT ROW 8.86 COL 124 NO-LABEL
     F-19 AT ROW 10.05 COL 124 NO-LABEL
     F-20 AT ROW 11.24 COL 124 NO-LABEL
     F-9 AT ROW 12.91 COL 124 NO-LABEL
     F-10 AT ROW 14.1 COL 124 NO-LABEL
     F-11 AT ROW 15.29 COL 124 NO-LABEL
     F-12 AT ROW 16.48 COL 124 NO-LABEL
     "Actual Applied" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 12.43 COL 69
          FGCOLOR 9 FONT 4
     "Cost of Goods Sold" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 2.43 COL 60
          FGCOLOR 9 FONT 4
     "W.I.P." VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 2.43 COL 4
          FGCOLOR 9 FONT 4
     "Finished Goods" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 7.67 COL 4
          FGCOLOR 9 FONT 4
     RECT-1 AT ROW 1 COL 1
     RECT-5 AT ROW 7.91 COL 2
     RECT-6 AT ROW 2.67 COL 2
     RECT-7 AT ROW 12.67 COL 68
     RECT-8 AT ROW 2.67 COL 58
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.prod
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
         HEIGHT             = 16.91
         WIDTH              = 127.
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

/* SETTINGS FOR FILL-IN prod.aa-fo IN FRAME F-Main
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN prod.aa-lab IN FRAME F-Main
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN prod.aa-mat IN FRAME F-Main
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN prod.aa-vo IN FRAME F-Main
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN prod.cgs-dl IN FRAME F-Main
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN prod.cgs-dlv IN FRAME F-Main
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN prod.cgs-fo IN FRAME F-Main
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN prod.cgs-fov IN FRAME F-Main
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN prod.cgs-mat IN FRAME F-Main
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN prod.cgs-mu IN FRAME F-Main
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN prod.cgs-vo IN FRAME F-Main
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN prod.cgs-vov IN FRAME F-Main
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN prod.dscr IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN F-10 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-10:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-11 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-11:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-12 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-12:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-13 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-13:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-14 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-14:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-15 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-15:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-16 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-16:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-17 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-17:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-18 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-18:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-19 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-19:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-2 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-2:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-20 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-20:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-3 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-3:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-4 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-4:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-5 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-5:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-6 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-6:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-7 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-7:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-8 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-8:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-9 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-9:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F1 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F1:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN prod.fg-fo IN FRAME F-Main
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN prod.fg-lab IN FRAME F-Main
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN prod.fg-mat IN FRAME F-Main
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN prod.fg-vo IN FRAME F-Main
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN prod.prolin IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN prod.wip-fo IN FRAME F-Main
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN prod.wip-lab IN FRAME F-Main
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN prod.wip-mat IN FRAME F-Main
   5 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN prod.wip-vo IN FRAME F-Main
   5 EXP-LABEL                                                          */
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

&Scoped-define SELF-NAME prod.aa-fo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.aa-fo V-table-Win
ON ENTRY OF prod.aa-fo IN FRAME F-Main /* Fixed Overhead */
DO:
  /*{custom/actentry.i}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.aa-fo V-table-Win
ON LEAVE OF prod.aa-fo IN FRAME F-Main /* Fixed Overhead */
DO:
  /*{custom/actleave.i} */
    IF LASTKEY = -1 THEN RETURN.

    RUN validate-actnum NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prod.aa-lab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.aa-lab V-table-Win
ON ENTRY OF prod.aa-lab IN FRAME F-Main /* Labor */
DO:
  /*{custom/actentry.i}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.aa-lab V-table-Win
ON LEAVE OF prod.aa-lab IN FRAME F-Main /* Labor */
DO:
  /*{custom/actleave.i} */
    IF LASTKEY = -1 THEN RETURN.

    RUN validate-actnum NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prod.aa-mat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.aa-mat V-table-Win
ON ENTRY OF prod.aa-mat IN FRAME F-Main /* Material */
DO:
  /*{custom/actentry.i}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.aa-mat V-table-Win
ON LEAVE OF prod.aa-mat IN FRAME F-Main /* Material */
DO:
  /*{custom/actleave.i} */
    IF LASTKEY = -1 THEN RETURN.

    RUN validate-actnum NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prod.aa-vo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.aa-vo V-table-Win
ON ENTRY OF prod.aa-vo IN FRAME F-Main /* Var. Overhead */
DO:
  /*{custom/actentry.i}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.aa-vo V-table-Win
ON LEAVE OF prod.aa-vo IN FRAME F-Main /* Var. Overhead */
DO:
  /*{custom/actleave.i} */
    IF LASTKEY = -1 THEN RETURN.

    RUN validate-actnum NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prod.cgs-dl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.cgs-dl V-table-Win
ON ENTRY OF prod.cgs-dl IN FRAME F-Main /* Direct Labor */
DO:
  /*{custom/actentry.i} */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.cgs-dl V-table-Win
ON LEAVE OF prod.cgs-dl IN FRAME F-Main /* Direct Labor */
DO:
  /*{custom/actleave.i} */
    IF LASTKEY = -1 THEN RETURN.

    RUN validate-actnum NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prod.cgs-dlv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.cgs-dlv V-table-Win
ON ENTRY OF prod.cgs-dlv IN FRAME F-Main /* Dir. Labor Variance */
DO:
  /*{custom/actentry.i}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.cgs-dlv V-table-Win
ON LEAVE OF prod.cgs-dlv IN FRAME F-Main /* Dir. Labor Variance */
DO:
  /*{custom/actleave.i} */
    IF LASTKEY = -1 THEN RETURN.

    RUN validate-actnum NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prod.cgs-fo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.cgs-fo V-table-Win
ON ENTRY OF prod.cgs-fo IN FRAME F-Main /* Fixed Overhead */
DO:
  /*{custom/actentry.i}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.cgs-fo V-table-Win
ON LEAVE OF prod.cgs-fo IN FRAME F-Main /* Fixed Overhead */
DO:
  /*{custom/actleave.i} */
    IF LASTKEY = -1 THEN RETURN.

    RUN validate-actnum NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prod.cgs-fov
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.cgs-fov V-table-Win
ON ENTRY OF prod.cgs-fov IN FRAME F-Main /* Fixed Ovrhd Effcy Variance */
DO:
  /*{custom/actentry.i} */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.cgs-fov V-table-Win
ON LEAVE OF prod.cgs-fov IN FRAME F-Main /* Fixed Ovrhd Effcy Variance */
DO:
  /*{custom/actleave.i} */
    IF LASTKEY = -1 THEN RETURN.

    RUN validate-actnum NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prod.cgs-mat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.cgs-mat V-table-Win
ON ENTRY OF prod.cgs-mat IN FRAME F-Main /* Material */
DO:
  /*{custom/actentry.i} */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.cgs-mat V-table-Win
ON LEAVE OF prod.cgs-mat IN FRAME F-Main /* Material */
DO:
  /*{custom/actleave.i} */
    IF LASTKEY = -1 THEN RETURN.

    RUN validate-actnum NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prod.cgs-mu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.cgs-mu V-table-Win
ON ENTRY OF prod.cgs-mu IN FRAME F-Main /* Mat. Usage Variance */
DO:
  /*{custom/actentry.i} */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.cgs-mu V-table-Win
ON LEAVE OF prod.cgs-mu IN FRAME F-Main /* Mat. Usage Variance */
DO:
  /*{custom/actleave.i} */
    IF LASTKEY = -1 THEN RETURN.

    RUN validate-actnum NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prod.cgs-vo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.cgs-vo V-table-Win
ON ENTRY OF prod.cgs-vo IN FRAME F-Main /* Variable Overhead */
DO:
  /*{custom/actentry.i}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.cgs-vo V-table-Win
ON LEAVE OF prod.cgs-vo IN FRAME F-Main /* Variable Overhead */
DO:
  /*{custom/actleave.i} */
    IF LASTKEY = -1 THEN RETURN.

    RUN validate-actnum NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prod.cgs-vov
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.cgs-vov V-table-Win
ON ENTRY OF prod.cgs-vov IN FRAME F-Main /* Var. Ovrhd Effcy Variance */
DO:
  /*{custom/actentry.i}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.cgs-vov V-table-Win
ON LEAVE OF prod.cgs-vov IN FRAME F-Main /* Var. Ovrhd Effcy Variance */
DO:
  /*{custom/actleave.i} */
    IF LASTKEY = -1 THEN RETURN.

    RUN validate-actnum NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prod.fg-fo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.fg-fo V-table-Win
ON ENTRY OF prod.fg-fo IN FRAME F-Main /* Fixed Overhead */
DO:
  /*{custom/actentry.i} */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.fg-fo V-table-Win
ON LEAVE OF prod.fg-fo IN FRAME F-Main /* Fixed Overhead */
DO:
    /*{custom/actleave.i} */
    IF LASTKEY = -1 THEN RETURN.

    RUN validate-actnum NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prod.fg-lab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.fg-lab V-table-Win
ON ENTRY OF prod.fg-lab IN FRAME F-Main /* Labor */
DO:
  /*{custom/actentry.i} */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.fg-lab V-table-Win
ON LEAVE OF prod.fg-lab IN FRAME F-Main /* Labor */
DO:
  /*{custom/actleave.i} */
    IF LASTKEY = -1 THEN RETURN.

    RUN validate-actnum NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prod.fg-mat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.fg-mat V-table-Win
ON ENTRY OF prod.fg-mat IN FRAME F-Main /* Material */
DO:
  /*{custom/actentry.i}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.fg-mat V-table-Win
ON LEAVE OF prod.fg-mat IN FRAME F-Main /* Material */
DO:
  /*{custom/actleave.i} */
    IF LASTKEY = -1 THEN RETURN.

    RUN validate-actnum NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prod.fg-vo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.fg-vo V-table-Win
ON ENTRY OF prod.fg-vo IN FRAME F-Main /* Var. Overhead */
DO:
  /*{custom/actentry.i} */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.fg-vo V-table-Win
ON LEAVE OF prod.fg-vo IN FRAME F-Main /* Var. Overhead */
DO:
  /*{custom/actleave.i} */
    IF LASTKEY = -1 THEN RETURN.

    RUN validate-actnum NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prod.wip-fo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.wip-fo V-table-Win
ON ENTRY OF prod.wip-fo IN FRAME F-Main /* Fixed Overhead */
DO:
  /*{custom/actentry.i}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.wip-fo V-table-Win
ON LEAVE OF prod.wip-fo IN FRAME F-Main /* Fixed Overhead */
DO:
  /*{custom/actleave.i} */
    IF LASTKEY = -1 THEN RETURN.

    RUN validate-actnum NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prod.wip-lab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.wip-lab V-table-Win
ON ENTRY OF prod.wip-lab IN FRAME F-Main /* Labor */
DO:
  /*{custom/actentry.i} */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.wip-lab V-table-Win
ON LEAVE OF prod.wip-lab IN FRAME F-Main /* Labor */
DO:
  /*{custom/actleave.i} */
    IF LASTKEY = -1 THEN RETURN.

    RUN validate-actnum NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prod.wip-mat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.wip-mat V-table-Win
ON ENTRY OF prod.wip-mat IN FRAME F-Main /* Material */
DO:
  /*{custom/actentry.i} */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.wip-mat V-table-Win
ON LEAVE OF prod.wip-mat IN FRAME F-Main /* Material */
DO:
    /*{custom/actleave.i} */
    IF LASTKEY = -1 THEN RETURN.

    RUN validate-actnum NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prod.wip-vo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.wip-vo V-table-Win
ON ENTRY OF prod.wip-vo IN FRAME F-Main /* Var. Overhead */
DO:
  /*{custom/actentry.i} */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prod.wip-vo V-table-Win
ON LEAVE OF prod.wip-vo IN FRAME F-Main /* Var. Overhead */
DO:
  /*{custom/actleave.i} */
    IF LASTKEY = -1 THEN RETURN.

    RUN validate-actnum NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}

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
  {src/adm/template/row-list.i "prod"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "prod"}

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
  {methods/viewers/create/prod.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN validate-actall NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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
  {src/adm/template/snd-list.i "prod"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-actall V-table-Win 
PROCEDURE validate-actall :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR ii AS INT NO-UNDO.
 DEF VAR hd1 AS WIDGET-HANDLE NO-UNDO.
 DEF VAR hd2 AS WIDGET-HANDLE NO-UNDO.
 DEF VAR lv-fld AS cha NO-UNDO.

  {methods/lValidateError.i YES}
  /* ==== Corrugated item validation ======== */
     hd1 = frame {&frame-name}:handle.
     hd1 = hd1:first-child.
     hd2 = hd1:first-child.



     do while valid-handle(hd2):
        lv-fld = "prod." + hd2:NAME.
        if hd2:type = "fill-in" and 
           hd2:data-type = "character" and 
           lookup(lv-fld,"{&list-5}"," ") > 0
           AND hd2:SCREEN-VALUE <> ""
        then do:
            FIND FIRST account WHERE account.company = gcompany
                        AND account.actnum = hd2:SCREEN-VALUE NO-LOCK NO-ERROR.
            IF NOT AVAIL account OR
               (AVAIL account AND account.TYPE = "T") THEN DO:
               MESSAGE "Invalid GL Account. Try Help... " VIEW-AS ALERT-BOX ERROR.               
               apply "entry" to hd2.
               return error.
            end.
        END.
        hd2 = hd2:next-sibling.
     end.       

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-actnum V-table-Win 
PROCEDURE validate-actnum :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
    FIND FIRST account WHERE account.company = gcompany
                         AND account.actnum = FOCUS:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF (NOT AVAIL account AND focus:SCREEN-VALUE <> "") or
       (AVAIL account AND account.TYPE = "T") 
    THEN DO:
       MESSAGE "Invalid GL Account#. Try Help..." 
             VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO SELF.
       RETURN ERROR.
    END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

