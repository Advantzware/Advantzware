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

&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

&scoped-def oe-prmtx-maint oe-prmtx

{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.

def new shared var uom-list as char init "M,EA,L,CS,C,LB,DRM,ROL,PKG,SET,DOZ,BDL" NO-UNDO.
def var v-invalid as log no-undo.
DEF VAR lv-cust-no AS cha NO-UNDO.
DEFINE BUFFER bf-oe-prmtx FOR oe-prmtx .

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
&Scoped-define EXTERNAL-TABLES oe-prmtx
&Scoped-define FIRST-EXTERNAL-TABLE oe-prmtx


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-prmtx.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS oe-prmtx.meth oe-prmtx.qty[1] oe-prmtx.uom[1] ~
oe-prmtx.qty[2] oe-prmtx.uom[2] oe-prmtx.qty[3] oe-prmtx.uom[3] ~
oe-prmtx.qty[4] oe-prmtx.uom[4] oe-prmtx.qty[5] oe-prmtx.uom[5] ~
oe-prmtx.qty[6] oe-prmtx.uom[6] oe-prmtx.qty[7] oe-prmtx.uom[7] ~
oe-prmtx.qty[8] oe-prmtx.uom[8] oe-prmtx.qty[9] oe-prmtx.uom[9] ~
oe-prmtx.qty[10] oe-prmtx.uom[10] 
&Scoped-define ENABLED-TABLES oe-prmtx
&Scoped-define FIRST-ENABLED-TABLE oe-prmtx
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-5 
&Scoped-Define DISPLAYED-FIELDS oe-prmtx.cust-no oe-prmtx.custype ~
oe-prmtx.i-no oe-prmtx.procat oe-prmtx.eff-date oe-prmtx.exp-date oe-prmtx.meth ~
oe-prmtx.qty[1] oe-prmtx.price[1] oe-prmtx.discount[1] oe-prmtx.uom[1] ~
oe-prmtx.qty[2] oe-prmtx.price[2] oe-prmtx.discount[2] oe-prmtx.uom[2] ~
oe-prmtx.qty[3] oe-prmtx.price[3] oe-prmtx.discount[3] oe-prmtx.uom[3] ~
oe-prmtx.qty[4] oe-prmtx.price[4] oe-prmtx.discount[4] oe-prmtx.uom[4] ~
oe-prmtx.qty[5] oe-prmtx.price[5] oe-prmtx.discount[5] oe-prmtx.uom[5] ~
oe-prmtx.qty[6] oe-prmtx.price[6] oe-prmtx.discount[6] oe-prmtx.uom[6] ~
oe-prmtx.qty[7] oe-prmtx.price[7] oe-prmtx.discount[7] oe-prmtx.uom[7] ~
oe-prmtx.qty[8] oe-prmtx.price[8] oe-prmtx.discount[8] oe-prmtx.uom[8] ~
oe-prmtx.qty[9] oe-prmtx.price[9] oe-prmtx.discount[9] oe-prmtx.uom[9] ~
oe-prmtx.qty[10] oe-prmtx.price[10] oe-prmtx.discount[10] oe-prmtx.uom[10] 
&Scoped-define DISPLAYED-TABLES oe-prmtx
&Scoped-define FIRST-DISPLAYED-TABLE oe-prmtx
&Scoped-Define DISPLAYED-OBJECTS F1 F-2 F-3 F-4 F-5 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS oe-prmtx.cust-no oe-prmtx.custype ~
oe-prmtx.i-no oe-prmtx.procat 
&Scoped-define ADM-ASSIGN-FIELDS oe-prmtx.cust-no oe-prmtx.custype ~
oe-prmtx.i-no oe-prmtx.procat oe-prmtx.eff-date oe-prmtx.exp-date oe-prmtx.price[1] ~
oe-prmtx.discount[1] oe-prmtx.price[2] oe-prmtx.discount[2] ~
oe-prmtx.price[3] oe-prmtx.discount[3] oe-prmtx.price[4] ~
oe-prmtx.discount[4] oe-prmtx.price[5] oe-prmtx.discount[5] ~
oe-prmtx.price[6] oe-prmtx.discount[6] oe-prmtx.price[7] ~
oe-prmtx.discount[7] oe-prmtx.price[8] oe-prmtx.discount[8] ~
oe-prmtx.price[9] oe-prmtx.discount[9] oe-prmtx.price[10] ~
oe-prmtx.discount[10] 
&Scoped-define F1 F1 F-2 F-3 F-4 F-5 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
company|y|y|ASI.oe-prmtx.company
uom||y|ASI.oe-prmtx.uom[1]
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "company",
     Keys-Supplied = "company,uom"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE F-2 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
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

DEFINE VARIABLE F1 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 142 BY 16.19.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 113 BY 13.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     oe-prmtx.cust-no AT ROW 1.24 COL 26 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     oe-prmtx.custype AT ROW 1.24 COL 51 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     oe-prmtx.i-no AT ROW 1.24 COL 78 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 24 BY 1
     oe-prmtx.procat AT ROW 1.24 COL 115 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     oe-prmtx.eff-date AT ROW 2.43 COL 78 COLON-ALIGNED
          LABEL "Effective Date"
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1
     oe-prmtx.exp-date AT ROW 2.43 COL 119 COLON-ALIGNED
          LABEL "Expiration Date"
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1
     oe-prmtx.meth AT ROW 2.67 COL 32 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Price", yes,
"Discount", no
          SIZE 28 BY .71
     oe-prmtx.qty[1] AT ROW 4.57 COL 33 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     oe-prmtx.price[1] FORMAT "->>,>>>,>>9.9999<<" AT ROW 4.57 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     oe-prmtx.discount[1] AT ROW 4.57 COL 87 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-prmtx.uom[1] AT ROW 4.57 COL 109 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     oe-prmtx.qty[2] AT ROW 5.76 COL 33 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     oe-prmtx.price[2] FORMAT "->>,>>>,>>9.9999<<" AT ROW 5.76 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     oe-prmtx.discount[2] AT ROW 5.76 COL 87 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-prmtx.uom[2] AT ROW 5.76 COL 109 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     oe-prmtx.qty[3] AT ROW 6.95 COL 33 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     oe-prmtx.price[3] FORMAT "->>,>>>,>>9.9999<<" AT ROW 6.95 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     oe-prmtx.discount[3] AT ROW 6.95 COL 87 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-prmtx.uom[3] AT ROW 6.95 COL 109 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     oe-prmtx.qty[4] AT ROW 8.14 COL 33 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     oe-prmtx.price[4] FORMAT "->>,>>>,>>9.9999<<" AT ROW 8.14 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     oe-prmtx.discount[4] AT ROW 8.14 COL 87 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-prmtx.uom[4] AT ROW 8.14 COL 109 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     oe-prmtx.qty[5] AT ROW 9.33 COL 33 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     oe-prmtx.price[5] FORMAT "->>,>>>,>>9.9999<<" AT ROW 9.33 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     oe-prmtx.discount[5] AT ROW 9.33 COL 87 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-prmtx.uom[5] AT ROW 9.33 COL 109 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     oe-prmtx.qty[6] AT ROW 10.52 COL 33 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     oe-prmtx.price[6] FORMAT "->>,>>>,>>9.9999<<" AT ROW 10.52 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     oe-prmtx.discount[6] AT ROW 10.52 COL 87 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-prmtx.uom[6] AT ROW 10.52 COL 109 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     oe-prmtx.qty[7] AT ROW 11.71 COL 33 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     oe-prmtx.price[7] FORMAT "->>,>>>,>>9.9999<<" AT ROW 11.71 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     oe-prmtx.discount[7] AT ROW 11.71 COL 87 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-prmtx.uom[7] AT ROW 11.71 COL 109 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     oe-prmtx.qty[8] AT ROW 12.91 COL 33 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     oe-prmtx.price[8] FORMAT "->>,>>>,>>9.9999<<" AT ROW 12.91 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     oe-prmtx.discount[8] AT ROW 12.91 COL 87 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-prmtx.uom[8] AT ROW 12.91 COL 109 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     oe-prmtx.qty[9] AT ROW 14.1 COL 33 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     oe-prmtx.price[9] FORMAT "->>,>>>,>>9.9999<<" AT ROW 14.1 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     oe-prmtx.discount[9] AT ROW 14.1 COL 87 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-prmtx.uom[9] AT ROW 14.1 COL 109 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     oe-prmtx.qty[10] AT ROW 15.29 COL 33 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     oe-prmtx.price[10] FORMAT "->>,>>>,>>9.9999<<" AT ROW 15.29 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     oe-prmtx.discount[10] AT ROW 15.29 COL 87 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     oe-prmtx.uom[10] AT ROW 15.29 COL 109 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     F1 AT ROW 1.24 COL 40 NO-LABEL
     F-2 AT ROW 1.24 COL 65 NO-LABEL
     F-3 AT ROW 1.24 COL 100 NO-LABEL
     F-4 AT ROW 1.24 COL 126 NO-LABEL
     F-5 AT ROW 4.57 COL 120 NO-LABEL
     "Quantity" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 3.86 COL 35
     "3" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 7.19 COL 20
     "Discount" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 3.86 COL 89
     "5" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 9.57 COL 20
     "Level" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 3.86 COL 20
     "7" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 11.95 COL 20
     "8" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 13.14 COL 20
     "Price Basis:" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 2.67 COL 14
     "9" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 14.33 COL 20
     "4" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 8.38 COL 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     "1" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 4.81 COL 20
     "2" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 6 COL 20
     "6" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 10.76 COL 20
     "UOM" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 3.86 COL 114
     "Price" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 3.86 COL 57
     "10" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 15.52 COL 20
     RECT-1 AT ROW 1 COL 1
     RECT-5 AT ROW 3.62 COL 15
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.oe-prmtx
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
         HEIGHT             = 17.86
         WIDTH              = 144.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer4.i}  /* task 10301314  */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN oe-prmtx.cust-no IN FRAME F-Main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN oe-prmtx.custype IN FRAME F-Main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN oe-prmtx.discount[10] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-prmtx.discount[1] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-prmtx.discount[2] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-prmtx.discount[3] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-prmtx.discount[4] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-prmtx.discount[5] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-prmtx.discount[6] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-prmtx.discount[7] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-prmtx.discount[8] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-prmtx.discount[9] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-prmtx.eff-date IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-prmtx.exp-date IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN F-2 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-2:HIDDEN IN FRAME F-Main           = TRUE.

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

/* SETTINGS FOR FILL-IN F1 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F1:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN oe-prmtx.i-no IN FRAME F-Main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN oe-prmtx.price[10] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-prmtx.price[1] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-prmtx.price[2] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-prmtx.price[3] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-prmtx.price[4] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-prmtx.price[5] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-prmtx.price[6] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-prmtx.price[7] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-prmtx.price[8] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-prmtx.price[9] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-prmtx.procat IN FRAME F-Main
   NO-ENABLE 1 2                                                        */
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

  {&methods/lValidateError.i YES}
  case focus:name :
    when "uom" then do:
      run windows/l-stduom.w (cocode, uom-list, focus:screen-value, output char-val).
      if char-val ne "" then 
        focus:screen-value in frame {&frame-name} = entry(1,char-val).
    end.

    otherwise do:
      lv-handle = focus:handle.
      run applhelp.p.

      if g_lookup-var ne "" then lv-handle:screen-value = g_lookup-var. 

      apply "entry" to lv-handle.
      return no-apply.
    end.
  end case.  
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-prmtx.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-prmtx.cust-no V-table-Win
ON ENTRY OF oe-prmtx.cust-no IN FRAME F-Main /* Customer */
DO:

  oe-prmtx.custype:sensitive = yes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-prmtx.cust-no V-table-Win
ON LEAVE OF oe-prmtx.cust-no IN FRAME F-Main /* Customer */
DO:
   {&methods/lValidateError.i YES}
   IF LASTKEY <> -1 AND oe-prmtx.cust-no:SCREEN-VALUE <> "" THEN do:
      RUN valid-cust-no NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         RETURN NO-APPLY.
      END.
   END.

  find first cust
      {sys/ref/custW.i}
        and cust.cust-no eq oe-prmtx.cust-no:screen-value in frame {&frame-name}
      no-lock no-error.
  if avail cust then do:
    assign
     oe-prmtx.cust-no:screen-value in frame {&frame-name} = cust.cust-no
     oe-prmtx.custype:screen-value in frame {&frame-name} = cust.type.

    oe-prmtx.custype:sensitive = no.
    apply "entry" to oe-prmtx.i-no.
  end.
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-prmtx.exp-date V-table-Win
ON LEAVE OF oe-prmtx.exp-date IN FRAME F-Main /* Item No */
DO:
   IF LASTKEY <> -1 THEN DO:
     RUN valid-expdate NO-ERROR.
     IF ERROR-STATUS:ERROR THEN DO:
        RETURN NO-APPLY.
     END.
  END.
    
  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-prmtx.custype
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-prmtx.custype V-table-Win
ON LEAVE OF oe-prmtx.custype IN FRAME F-Main /* Type */
DO:
   {&methods/lValidateError.i YES}
   IF LASTKEY <> -1 AND oe-prmtx.custype:SCREEN-VALUE <> "" THEN do:
       RUN valid-custtype NO-ERROR.
       IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END.
  find first itemfg
      where itemfg.company eq cocode
        and itemfg.i-no    eq oe-prmtx.i-no:screen-value in frame {&frame-name}
      no-lock no-error.
  if avail itemfg then do:
    assign
     oe-prmtx.i-no:screen-value   in frame {&frame-name} = itemfg.i-no
     oe-prmtx.procat:screen-value in frame {&frame-name} = itemfg.procat. 

    oe-prmtx.procat:sensitive = no.
    apply "entry" to oe-prmtx.meth.
  end. 
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-prmtx.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-prmtx.i-no V-table-Win
ON ENTRY OF oe-prmtx.i-no IN FRAME F-Main /* Item No */
DO:
  oe-prmtx.procat:sensitive = yes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-prmtx.i-no V-table-Win
ON LEAVE OF oe-prmtx.i-no IN FRAME F-Main /* Item No */
DO:

  IF LASTKEY <> -1 THEN DO:
     RUN valid-i-no NO-ERROR.
     IF ERROR-STATUS:ERROR THEN DO:
        RETURN NO-APPLY.
     END.
  END.


  find first itemfg
      where itemfg.company eq cocode
        and itemfg.i-no    eq oe-prmtx.i-no:screen-value in frame {&frame-name}
      no-lock no-error.
  if avail itemfg then do:
    assign
     oe-prmtx.i-no:screen-value   in frame {&frame-name} = itemfg.i-no
     oe-prmtx.procat:screen-value in frame {&frame-name} = itemfg.procat. 
/* 02061208 per BJ */
/*     oe-prmtx.procat:sensitive = no. */
/*     apply "entry" to oe-prmtx.meth. */

  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-prmtx.meth
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-prmtx.meth V-table-Win
ON LEAVE OF oe-prmtx.meth IN FRAME F-Main /* Price Basis */
DO:
  if self:screen-value eq "yes" then do:
    enable  oe-prmtx.price    with frame {&frame-name}.
    disable oe-prmtx.discount with frame {&frame-name}.
  end.

  else do:
    enable  oe-prmtx.discount with frame {&frame-name}.
    disable oe-prmtx.price    with frame {&frame-name}.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-prmtx.meth V-table-Win
ON VALUE-CHANGED OF oe-prmtx.meth IN FRAME F-Main /* Price Basis */
DO:
  if self:screen-value eq "yes" then do:
     disable oe-prmtx.discount with frame {&frame-name}.
     enable  oe-prmtx.price with frame {&frame-name}.
  end.
  else do:
    enable oe-prmtx.discount with frame {&frame-name}.
    disable oe-prmtx.price with frame {&frame-name}.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-prmtx.procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-prmtx.procat V-table-Win
ON LEAVE OF oe-prmtx.procat IN FRAME F-Main /* Category */
DO:
   IF LASTKEY <> -1 THEN RUN valid-procat  NO-ERROR.
   IF ERROR-STATUS:ERROR THEN DO:
      RETURN NO-APPLY.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-prmtx.uom[10]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-prmtx.uom[10] V-table-Win
ON LEAVE OF oe-prmtx.uom[10] IN FRAME F-Main /* Purchased UOM[10] */
DO:
  if lastkey ne -1 then do:
    run valid-uom-10.
    if v-invalid then return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-prmtx.uom[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-prmtx.uom[1] V-table-Win
ON LEAVE OF oe-prmtx.uom[1] IN FRAME F-Main /* Purchased UOM[1] */
DO:
  if lastkey ne -1 then do:
    run valid-uom-01.
    if v-invalid then return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-prmtx.uom[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-prmtx.uom[2] V-table-Win
ON LEAVE OF oe-prmtx.uom[2] IN FRAME F-Main /* Purchased UOM[2] */
DO:
  if lastkey ne -1 then do:
    run valid-uom-02.
    if v-invalid then return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-prmtx.uom[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-prmtx.uom[3] V-table-Win
ON LEAVE OF oe-prmtx.uom[3] IN FRAME F-Main /* Purchased UOM[3] */
DO:
  if lastkey ne -1 then do:
    run valid-uom-03.
    if v-invalid then return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-prmtx.uom[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-prmtx.uom[4] V-table-Win
ON LEAVE OF oe-prmtx.uom[4] IN FRAME F-Main /* Purchased UOM[4] */
DO:
  if lastkey ne -1 then do:
    run valid-uom-04.
    if v-invalid then return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-prmtx.uom[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-prmtx.uom[5] V-table-Win
ON LEAVE OF oe-prmtx.uom[5] IN FRAME F-Main /* Purchased UOM[5] */
DO:
  if lastkey ne -1 then do:
    run valid-uom-05.
    if v-invalid then return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-prmtx.uom[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-prmtx.uom[6] V-table-Win
ON LEAVE OF oe-prmtx.uom[6] IN FRAME F-Main /* Purchased UOM[6] */
DO:
  if lastkey ne -1 then do:
    run valid-uom-06.
    if v-invalid then return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-prmtx.uom[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-prmtx.uom[7] V-table-Win
ON LEAVE OF oe-prmtx.uom[7] IN FRAME F-Main /* Purchased UOM[7] */
DO:
  if lastkey ne -1 then do:
    run valid-uom-07.
    if v-invalid then return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-prmtx.uom[8]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-prmtx.uom[8] V-table-Win
ON LEAVE OF oe-prmtx.uom[8] IN FRAME F-Main /* Purchased UOM[8] */
DO:
  if lastkey ne -1 then do:
    run valid-uom-08.
    if v-invalid then return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-prmtx.uom[9]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-prmtx.uom[9] V-table-Win
ON LEAVE OF oe-prmtx.uom[9] IN FRAME F-Main /* Purchased UOM[9] */
DO:
  if lastkey ne -1 then do:
    run valid-uom-09.
    if v-invalid then return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  session:data-entry-return = yes.

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
  DEF VAR key-value AS CHAR NO-UNDO.
  DEF VAR row-avail-enabled AS LOGICAL NO-UNDO.

  /* LOCK status on the find depends on FIELDS-ENABLED. */
  RUN get-attribute ('FIELDS-ENABLED':U).
  row-avail-enabled = (RETURN-VALUE eq 'yes':U).
  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'company':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = oe-prmtx
           &WHERE = "WHERE oe-prmtx.company eq key-value"
       }
  END CASE.

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
  {src/adm/template/row-list.i "oe-prmtx"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-prmtx"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-oe-prmtx-field V-table-Win 
PROCEDURE enable-oe-prmtx-field :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ENABLE ALL.

    IF AVAIL oe-prmtx AND oe-prmtx.meth THEN DO:
      ENABLE  oe-prmtx.price.
      DISABLE oe-prmtx.discount.
    END.

    ELSE DO:
      ENABLE  oe-prmtx.discount.
      DISABLE oe-prmtx.price.
    END.

    IF AVAIL oe-prmtx AND oe-prmtx.cust-no NE "" THEN DISABLE oe-prmtx.custype.

    IF AVAIL oe-prmtx AND oe-prmtx.i-no NE "" THEN DISABLE oe-prmtx.procat.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
/*   RUN reftable-values (NO).                          */
/*                                                      */
/*   oe-prmtx.i-no = STRING(oe-prmtx.i-no,"x(100)") +   */
/*                   STRING(YEAR(fi_eff-date),"9999") + */
/*                   STRING(MONTH(fi_eff-date),"99")  + */
/*                   STRING(DAY(fi_eff-date),"99").     */

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
  disable all with frame {&frame-name}.

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

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  oe-prmtx.company = cocode.
  oe-prmtx.cust-no = lv-cust-no.

  IF adm-adding-record THEN
  DO WITH FRAME {&FRAME-NAME}:
    eff-date:SCREEN-VALUE = STRING(TODAY,"99/99/9999").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lAccess AS LOG NO-UNDO.
DEF VAR lAccessClose AS LOG NO-UNDO.
DEF VAR cAccessList AS CHAR NO-UNDO.

    RUN methods/prgsecur.p(INPUT "oe-prmtx.",
                           INPUT "delete",
                           INPUT NO,
                           INPUT NO,
                           INPUT NO,
                           OUTPUT lAccess,
                           OUTPUT lAccessClose,
                           OUTPUT cAccessList).
  /* Code placed here will execute PRIOR to standard behavior. */
    IF lAccess THEN DO:
        IF AVAIL oe-prmtx AND NOT adm-new-record THEN
            RUN cerep/del-prmtx.w (oe-prmtx.eff-date,oe-prmtx.cust-no, oe-prmtx.custype, oe-prmtx.procat, oe-prmtx.i-no).  /* task 10301314  */ 
        /* Dispatch standard ADM method.   */
        ELSE
            RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .
   /* Code placed here will execute AFTER standard behavior.    */
   /* task 10301314  */
        FIND CURRENT oe-prmtx NO-LOCK NO-ERROR .
        IF NOT AVAIL oe-prmtx THEN
            FIND FIRST oe-prmtx WHERE oe-prmtx.company = cocode NO-LOCK NO-ERROR.
        RUN local-display-fields.
        {methods/template/local/deleteAfter.i}       /* task 10301314  */
    END.
    ELSE
        MESSAGE "You do not have access to delete a Sales Price Matrix."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
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
  IF AVAIL oe-prmtx AND NOT adm-new-record THEN RUN reftable-values (YES).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
/*   DISABLE fi_eff-date WITH FRAME {&FRAME-NAME}. */

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
  IF  oe-prmtx.cust-no:SCREEN-VALUE IN frame {&FRAME-NAME} <> "" THEN DO:
      RUN valid-cust-no NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
     RETURN NO-APPLY.
  END.

  IF oe-prmtx.custype:SCREEN-VALUE <> "" THEN do:
     RUN valid-custtype NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  RUN valid-procat NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
     RETURN NO-APPLY.
  END.
  RUN valid-entry NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
     RETURN NO-APPLY.
  END.
  RUN valid-expdate NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
     RETURN NO-APPLY.
  END.
  
 

  run valid-uom-01.
  if v-invalid then return no-apply.

  run valid-uom-02.
  if v-invalid then return no-apply.

  run valid-uom-03.
  if v-invalid then return no-apply.

  run valid-uom-04.
  if v-invalid then return no-apply.

  run valid-uom-05.
  if v-invalid then return no-apply.

  run valid-uom-06.
  if v-invalid then return no-apply.

  run valid-uom-07.
  if v-invalid then return no-apply.

  run valid-uom-08.
  if v-invalid then return no-apply.

  run valid-uom-09.
  if v-invalid then return no-apply.

  run valid-uom-10.
  if v-invalid then return no-apply.

  disable all with frame {&frame-name}.

  IF adm-adding-record THEN lv-cust-no = oe-prmtx.cust-no:SCREEN-VALUE.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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


/*   IF AVAIL oe-prmtx THEN DO:                            */
/*     FIND FIRST reftable                                 */
/*         WHERE reftable.rec_key  EQ oe-prmtx.rec_key     */
/*           AND reftable.company  EQ "oe-prmtx"           */
/*         USE-INDEX rec_key NO-ERROR.                     */
/*     IF NOT AVAIL reftable THEN DO:                      */
/*       CREATE reftable.                                  */
/*       ASSIGN                                            */
/*        reftable.rec_key  = oe-prmtx.rec_key             */
/*        reftable.company  = "oe-prmtx"                   */
/*        reftable.code     = STRING(TODAY,"99/99/9999").  */
/*     END.                                                */
/*     IF ip-display THEN                                  */
/*       fi_eff-date = DATE(reftable.code).                */
/*     ELSE                                                */
/*       reftable.code = STRING(fi_eff-date,"99/99/9999"). */
/*   END.                                                  */

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
  {src/adm/template/sndkycas.i "company" "oe-prmtx" "company"}
  {src/adm/template/sndkycas.i "uom" "oe-prmtx" "uom[1]"}

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
  {src/adm/template/snd-list.i "oe-prmtx"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust-no V-table-Win 
PROCEDURE valid-cust-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  IF NOT CAN-FIND(FIRST cust WHERE cust.company = g_company
                      AND cust.cust-no = oe-prmtx.cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME})
  THEN DO:
      MESSAGE "Invalid Customer#. Try Help." VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-custtype V-table-Win 
PROCEDURE valid-custtype :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  IF NOT CAN-FIND(FIRST custype WHERE custype.company = g_company
                      AND custype.custype = oe-prmtx.custype:SCREEN-VALUE IN FRAME {&FRAME-NAME})
  THEN DO:
      MESSAGE "Invalid Type#. Try Help." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO oe-prmtx.custype.
      RETURN ERROR.
  END.



  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

  &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-expdate V-table-Win 
PROCEDURE valid-expdate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  IF DATE(oe-prmtx.exp-date:SCREEN-VALUE in frame {&frame-name}) LT DATE(oe-prmtx.eff-date:SCREEN-VALUE in frame {&frame-name}) THEN DO:
          MESSAGE "Expiration date should be greater than Effective Date." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO oe-prmtx.exp-date.
          RETURN ERROR.
      END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no V-table-Win 
PROCEDURE valid-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  IF oe-prmtx.i-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE ""
      AND NOT CAN-FIND(FIRST itemfg WHERE itemfg.company = g_company
                      AND itemfg.i-no = oe-prmtx.i-no:SCREEN-VALUE IN FRAME {&FRAME-NAME})
  THEN DO:
      MESSAGE "Invalid Item#. Try Help." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO oe-prmtx.i-no.
      RETURN ERROR.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no V-table-Win 
PROCEDURE valid-entry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}

   FIND FIRST bf-oe-prmtx NO-LOCK 
      WHERE bf-oe-prmtx.company EQ g_company
        AND bf-oe-prmtx.cust-no EQ oe-prmtx.cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        AND bf-oe-prmtx.i-no EQ oe-prmtx.i-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        AND bf-oe-prmtx.procat EQ oe-prmtx.procat:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        AND bf-oe-prmtx.custype EQ oe-prmtx.custype:SCREEN-VALUE IN FRAME {&FRAME-NAME}
      AND rowid(bf-oe-prmtx) NE rowid(oe-prmtx) NO-ERROR .

  IF AVAIL bf-oe-prmtx THEN DO:
      MESSAGE "This record is a duplicate of a previous entry; please adjust." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO oe-prmtx.cust-no.
      RETURN ERROR.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-procat V-table-Win 
PROCEDURE valid-procat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
IF NOT CAN-FIND(FIRST fgcat 
                    WHERE fgcat.company EQ g_company 
                      AND fgcat.procat EQ oe-prmtx.procat:SCREEN-VALUE IN FRAME {&FRAME-NAME})
    THEN DO:
        MESSAGE "Invalid Product Category. Try Help." 
             VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO oe-prmtx.procat.
        RETURN ERROR.
END.
FIND FIRST itemfg
    WHERE itemfg.company EQ cocode
      AND itemfg.i-no    EQ oe-prmtx.i-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}
      NO-LOCK NO-ERROR.
IF AVAIL itemfg 
    AND itemfg.procat NE oe-prmtx.procat:SCREEN-VALUE IN FRAME {&FRAME-NAME} 
    THEN DO:
        MESSAGE "Product Category must match Category for Item." 
            VIEW-AS ALERT-BOX ERROR.
        oe-prmtx.procat:SCREEN-VALUE IN FRAME {&FRAME-NAME} = itemfg.procat.
        APPLY "entry" TO oe-prmtx.procat.
        RETURN ERROR.
END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom-01 V-table-Win 
PROCEDURE valid-uom-01 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
v-invalid = no.

if int(oe-prmtx.qty[01]:screen-value in frame {&frame-name}) ne 0 then do:
  find first uom
      where uom.uom eq oe-prmtx.uom[01]:screen-value in frame {&frame-name}
        and lookup(uom.uom,uom-list) ne 0
      no-lock no-error.
  if avail uom then oe-prmtx.uom[01]:screen-value = uom.uom.

  else do:
    message "Must enter a valid UOM" view-as alert-box error.
    v-invalid = yes.
    apply "entry" to oe-prmtx.uom[01]. 
  end.
end.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom-02 V-table-Win 
PROCEDURE valid-uom-02 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
v-invalid = no.

if int(oe-prmtx.qty[02]:screen-value in frame {&frame-name}) ne 0 then do:
  find first uom
      where uom.uom eq oe-prmtx.uom[02]:screen-value in frame {&frame-name}
        and lookup(uom.uom,uom-list) ne 0
      no-lock no-error.
  if avail uom then oe-prmtx.uom[02]:screen-value = uom.uom.

  else do:
    message "Must enter a valid UOM" view-as alert-box error.
    v-invalid = yes.
    apply "entry" to oe-prmtx.uom[02]. 
  end.
end.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom-03 V-table-Win 
PROCEDURE valid-uom-03 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
v-invalid = no.

if int(oe-prmtx.qty [03]:screen-value in frame {&frame-name}) ne 0 then do:
  find first uom
      where uom.uom eq oe-prmtx.uom[03]:screen-value in frame {&frame-name}
        and lookup(uom.uom,uom-list) ne 0
      no-lock no-error.
  if avail uom then oe-prmtx.uom[03]:screen-value = uom.uom.

  else do:
    message "Must enter a valid UOM" view-as alert-box error.
    v-invalid = yes.
    apply "entry" to oe-prmtx.uom[03]. 
  end.
end.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom-04 V-table-Win 
PROCEDURE valid-uom-04 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
v-invalid = no.

if int(oe-prmtx.qty[04]:screen-value in frame {&frame-name}) ne 0 then do:
  find first uom
      where uom.uom eq oe-prmtx.uom[04]:screen-value in frame {&frame-name}
        and lookup(uom.uom,uom-list) ne 0
      no-lock no-error.
  if avail uom then oe-prmtx.uom[04]:screen-value = uom.uom.

  else do:
    message "Must enter a valid UOM" view-as alert-box error.
    v-invalid = yes.
    apply "entry" to oe-prmtx.uom[04]. 
  end.
end.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom-05 V-table-Win 
PROCEDURE valid-uom-05 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
v-invalid = no.

if int(oe-prmtx.qty[05]:screen-value in frame {&frame-name}) ne 0 then do:
  find first uom
      where uom.uom eq oe-prmtx.uom[05]:screen-value in frame {&frame-name}
        and lookup(uom.uom,uom-list) ne 0
      no-lock no-error.
  if avail uom then oe-prmtx.uom[05]:screen-value = uom.uom.

  else do:
    message "Must enter a valid UOM" view-as alert-box error.
    v-invalid = yes.
    apply "entry" to oe-prmtx.uom[05]. 
  end.
end.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom-06 V-table-Win 
PROCEDURE valid-uom-06 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
v-invalid = no.

if int(oe-prmtx.qty[06]:screen-value in frame {&frame-name}) ne 0 then do:
  find first uom
      where uom.uom eq oe-prmtx.uom[06]:screen-value in frame {&frame-name}
        and lookup(uom.uom,uom-list) ne 0
      no-lock no-error.
  if avail uom then oe-prmtx.uom[06]:screen-value = uom.uom.

  else do:
    message "Must enter a valid UOM" view-as alert-box error.
    v-invalid = yes.
    apply "entry" to oe-prmtx.uom[06]. 
  end.
end.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom-07 V-table-Win 
PROCEDURE valid-uom-07 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
v-invalid = no.

if int(oe-prmtx.qty[07]:screen-value in frame {&frame-name}) ne 0 then do:
  find first uom
      where uom.uom eq oe-prmtx.uom[07]:screen-value in frame {&frame-name}
        and lookup(uom.uom,uom-list) ne 0
      no-lock no-error.
  if avail uom then oe-prmtx.uom[07]:screen-value = uom.uom.

  else do:
    message "Must enter a valid UOM" view-as alert-box error.
    v-invalid = yes.
    apply "entry" to oe-prmtx.uom[07]. 
  end.
end.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom-08 V-table-Win 
PROCEDURE valid-uom-08 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
v-invalid = no.

if int(oe-prmtx.qty[08]:screen-value in frame {&frame-name}) ne 0 then do:
  find first uom
      where uom.uom eq oe-prmtx.uom[08]:screen-value in frame {&frame-name}
        and lookup(uom.uom,uom-list) ne 0
      no-lock no-error.
  if avail uom then oe-prmtx.uom[08]:screen-value = uom.uom.

  else do:
    message "Must enter a valid UOM" view-as alert-box error.
    v-invalid = yes.
    apply "entry" to oe-prmtx.uom[08]. 
  end.
end.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom-09 V-table-Win 
PROCEDURE valid-uom-09 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
v-invalid = no.

if int(oe-prmtx.qty[09]:screen-value in frame {&frame-name}) ne 0 then do:
  find first uom
      where uom.uom eq oe-prmtx.uom[09]:screen-value in frame {&frame-name}
        and lookup(uom.uom,uom-list) ne 0
      no-lock no-error.
  if avail uom then oe-prmtx.uom[09]:screen-value = uom.uom.

  else do:
    message "Must enter a valid UOM" view-as alert-box error.
    v-invalid = yes.
    apply "entry" to oe-prmtx.uom[09]. 
  end.
end.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom-10 V-table-Win 
PROCEDURE valid-uom-10 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
v-invalid = no.

if int(oe-prmtx.qty[10]:screen-value in frame {&frame-name}) ne 0 then do:
  find first uom
      where uom.uom eq oe-prmtx.uom[10]:screen-value in frame {&frame-name}
        and lookup(uom.uom,uom-list) ne 0
      no-lock no-error.
  if avail uom then oe-prmtx.uom[10]:screen-value = uom.uom.

  else do:
    message "Must enter a valid UOM" view-as alert-box error.
    v-invalid = yes.
    apply "entry" to oe-prmtx.uom[10]. 
  end.
end.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

