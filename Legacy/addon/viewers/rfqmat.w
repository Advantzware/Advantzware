&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          rfq              PROGRESS
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

def var char-val as cha no-undo.
def var ls-pre-mat as cha no-undo.  /* material code on entry event */
{methods/defines/hndlset.i}
{methods/defines/noreckey.i}

&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF

DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

DEF VAR li-sq AS INT NO-UNDO.
DEF VAR lv-spec-qty LIKE rfqitem.spec-qty DECIMALS 10 NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES rfqitem
&Scoped-define FIRST-EXTERNAL-TABLE rfqitem


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR rfqitem.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS rfqitem.board rfqitem.cal rfqitem.gsh-wid ~
rfqitem.gsh-len rfqitem.leaf[1] rfqitem.leaf-w[1] rfqitem.leaf-l[1] ~
rfqitem.leaf[2] rfqitem.leaf-w[2] rfqitem.leaf-l[2] rfqitem.leaf[3] ~
rfqitem.leaf-w[3] rfqitem.leaf-l[3] rfqitem.leaf[4] rfqitem.leaf-w[4] ~
rfqitem.leaf-l[4] rfqitem.spec-no[1] rfqitem.spec-dscr[1] rfqitem.adder[1] ~
rfqitem.spec-no[2] rfqitem.spec-dscr[2] rfqitem.adder[2] rfqitem.spec-no[3] ~
rfqitem.spec-dscr[3] rfqitem.adder[3] rfqitem.spec-no[4] ~
rfqitem.spec-dscr[4] rfqitem.adder[4] rfqitem.spec-no[5] ~
rfqitem.spec-dscr[5] rfqitem.adder[5] rfqitem.spec-no[6] ~
rfqitem.spec-dscr[6] rfqitem.adder[6] 
&Scoped-define ENABLED-TABLES rfqitem
&Scoped-define FIRST-ENABLED-TABLE rfqitem
&Scoped-Define ENABLED-OBJECTS RECT-17 RECT-18 RECT-19 
&Scoped-Define DISPLAYED-FIELDS rfqitem.board rfqitem.cal rfqitem.gsh-wid ~
rfqitem.gsh-len rfqitem.leaf[1] rfqitem.leaf-w[1] rfqitem.leaf-l[1] ~
rfqitem.leaf[2] rfqitem.leaf-w[2] rfqitem.leaf-l[2] rfqitem.leaf[3] ~
rfqitem.leaf-w[3] rfqitem.leaf-l[3] rfqitem.leaf[4] rfqitem.leaf-w[4] ~
rfqitem.leaf-l[4] rfqitem.flute rfqitem.test rfqitem.spec-no[1] ~
rfqitem.spec-dscr[1] rfqitem.adder[1] rfqitem.adder[7] rfqitem.spec-no[2] ~
rfqitem.spec-dscr[2] rfqitem.adder[2] rfqitem.adder[8] rfqitem.spec-no[3] ~
rfqitem.spec-dscr[3] rfqitem.adder[3] rfqitem.adder[9] rfqitem.spec-no[4] ~
rfqitem.spec-dscr[4] rfqitem.adder[4] rfqitem.adder[10] rfqitem.spec-no[5] ~
rfqitem.spec-dscr[5] rfqitem.adder[5] rfqitem.adder[11] rfqitem.spec-no[6] ~
rfqitem.spec-dscr[6] rfqitem.adder[6] rfqitem.adder[12] rfqitem.brd-dscr ~
rfqitem.leaf-dscr[1] rfqitem.leaf-dscr[2] rfqitem.leaf-dscr[3] ~
rfqitem.leaf-dscr[4] 
&Scoped-define DISPLAYED-TABLES rfqitem
&Scoped-define FIRST-DISPLAYED-TABLE rfqitem
&Scoped-Define DISPLAYED-OBJECTS fi_spec-qty-01 fi_spec-qty-02 ~
fi_spec-qty-03 fi_spec-qty-04 fi_spec-qty-05 fi_spec-qty-06 F1 adder-label ~
lv-adder lv-adder-dscr 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-ASSIGN-FIELDS fi_spec-qty-01 fi_spec-qty-02 ~
fi_spec-qty-03 fi_spec-qty-04 fi_spec-qty-05 fi_spec-qty-06 
&Scoped-define List-6 F1 

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
DEFINE VARIABLE adder-label AS CHARACTER FORMAT "X(256)":U INITIAL "Adders" 
      VIEW-AS TEXT 
     SIZE 8 BY .62 NO-UNDO.

DEFINE VARIABLE F1 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE fi_spec-qty-01 AS DECIMAL FORMAT ">>>,>>>,>>9.9<<<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.

DEFINE VARIABLE fi_spec-qty-02 AS DECIMAL FORMAT ">>>,>>>,>>9.9<<<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.

DEFINE VARIABLE fi_spec-qty-03 AS DECIMAL FORMAT ">>>,>>>,>>9.9<<<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.

DEFINE VARIABLE fi_spec-qty-04 AS DECIMAL FORMAT ">>>,>>>,>>9.9<<<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.

DEFINE VARIABLE fi_spec-qty-05 AS DECIMAL FORMAT ">>>,>>>,>>9.9<<<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.

DEFINE VARIABLE fi_spec-qty-06 AS DECIMAL FORMAT ">>>,>>>,>>9.9<<<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.

DEFINE VARIABLE lv-adder AS CHARACTER FORMAT "X(256)":U INITIAL "Item#" 
      VIEW-AS TEXT 
     SIZE 8 BY .62 NO-UNDO.

DEFINE VARIABLE lv-adder-dscr AS CHARACTER FORMAT "X(256)":U INITIAL "Description" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 116 BY 6.91.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 53 BY 7.62.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 63 BY 7.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     rfqitem.board AT ROW 1.95 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     rfqitem.cal AT ROW 1.95 COL 69 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     rfqitem.gsh-wid AT ROW 1.95 COL 85 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     rfqitem.gsh-len AT ROW 1.95 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     rfqitem.leaf[1] AT ROW 2.91 COL 14 COLON-ALIGNED
          LABEL "Window/Wax"
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     rfqitem.leaf-w[1] AT ROW 2.91 COL 85 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     rfqitem.leaf-l[1] AT ROW 2.91 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     rfqitem.leaf[2] AT ROW 3.86 COL 14 COLON-ALIGNED
          LABEL "Foil"
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     rfqitem.leaf-w[2] AT ROW 3.86 COL 85 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     rfqitem.leaf-l[2] AT ROW 3.86 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY .91
     rfqitem.leaf[3] AT ROW 4.81 COL 14 COLON-ALIGNED
          LABEL "Stamp"
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     rfqitem.leaf-w[3] AT ROW 4.81 COL 85 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY .95
     rfqitem.leaf-l[3] AT ROW 4.81 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     rfqitem.leaf[4] AT ROW 5.81 COL 14 COLON-ALIGNED
          LABEL "Laminate"
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     rfqitem.leaf-w[4] AT ROW 5.81 COL 85 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     rfqitem.leaf-l[4] AT ROW 5.81 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     rfqitem.flute AT ROW 6.71 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     rfqitem.test AT ROW 6.71 COL 32 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     rfqitem.spec-no[1] AT ROW 9.81 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     rfqitem.spec-dscr[1] AT ROW 9.81 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     fi_spec-qty-01 AT ROW 9.81 COL 43 COLON-ALIGNED HELP
          "Enter Quantity for this Item" NO-LABEL
     rfqitem.adder[1] AT ROW 9.81 COL 63 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     rfqitem.adder[7] AT ROW 9.81 COL 80 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
          BGCOLOR 7 FGCOLOR 15 
     rfqitem.spec-no[2] AT ROW 10.76 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     rfqitem.spec-dscr[2] AT ROW 10.76 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     fi_spec-qty-02 AT ROW 10.76 COL 43 COLON-ALIGNED NO-LABEL
     rfqitem.adder[2] AT ROW 10.76 COL 63 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     rfqitem.adder[8] AT ROW 10.76 COL 80 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
          BGCOLOR 7 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     rfqitem.spec-no[3] AT ROW 11.71 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     rfqitem.spec-dscr[3] AT ROW 11.71 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     fi_spec-qty-03 AT ROW 11.71 COL 43 COLON-ALIGNED NO-LABEL
     rfqitem.adder[3] AT ROW 11.71 COL 63 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     rfqitem.adder[9] AT ROW 11.71 COL 80 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
          BGCOLOR 7 FGCOLOR 15 
     rfqitem.spec-no[4] AT ROW 12.67 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     rfqitem.spec-dscr[4] AT ROW 12.67 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     fi_spec-qty-04 AT ROW 12.67 COL 43 COLON-ALIGNED NO-LABEL
     rfqitem.adder[4] AT ROW 12.67 COL 63 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     rfqitem.adder[10] AT ROW 12.67 COL 80 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
          BGCOLOR 7 FGCOLOR 15 
     rfqitem.spec-no[5] AT ROW 13.62 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     rfqitem.spec-dscr[5] AT ROW 13.62 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     fi_spec-qty-05 AT ROW 13.62 COL 43 COLON-ALIGNED NO-LABEL
     rfqitem.adder[5] AT ROW 13.62 COL 63 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     rfqitem.adder[11] AT ROW 13.62 COL 80 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
          BGCOLOR 7 FGCOLOR 15 
     rfqitem.spec-no[6] AT ROW 14.57 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     rfqitem.spec-dscr[6] AT ROW 14.57 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     fi_spec-qty-06 AT ROW 14.57 COL 43 COLON-ALIGNED NO-LABEL
     rfqitem.adder[6] AT ROW 14.57 COL 63 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     rfqitem.adder[12] AT ROW 14.57 COL 80 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
          BGCOLOR 7 FGCOLOR 15 
     rfqitem.brd-dscr AT ROW 1.95 COL 32 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 35 BY 1
          BGCOLOR 7 FGCOLOR 15 
     rfqitem.leaf-dscr[1] AT ROW 2.91 COL 32 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 35 BY 1
          BGCOLOR 7 FGCOLOR 15 
     F1 AT ROW 3.14 COL 32 NO-LABEL
     rfqitem.leaf-dscr[2] AT ROW 3.86 COL 32 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 35 BY 1
          BGCOLOR 7 FGCOLOR 15 
     rfqitem.leaf-dscr[3] AT ROW 4.81 COL 32 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 35 BY 1
          BGCOLOR 7 FGCOLOR 15 
     rfqitem.leaf-dscr[4] AT ROW 5.76 COL 32 COLON-ALIGNED NO-LABEL
           VIEW-AS TEXT 
          SIZE 35 BY 1
          BGCOLOR 7 FGCOLOR 15 
     adder-label AT ROW 8.14 COL 66 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     lv-adder AT ROW 9.1 COL 66 COLON-ALIGNED NO-LABEL
     lv-adder-dscr AT ROW 9.1 COL 88 COLON-ALIGNED NO-LABEL
     RECT-17 AT ROW 1 COL 1
     RECT-18 AT ROW 8.38 COL 64
     RECT-19 AT ROW 8.38 COL 1
     "Item #" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 9.1 COL 4
     "Special Materials" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 8.14 COL 4
     "Material" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 1.24 COL 21
     "Caliper" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.24 COL 77
     "Qty/FG" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 9.1 COL 47
     "Description" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 9.1 COL 22
     "Description" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 1.24 COL 45
     "Length" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.24 COL 103
     "Width" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1.24 COL 89
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: rfq.rfqitem
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
         HEIGHT             = 26.05
         WIDTH              = 141.8.
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

/* SETTINGS FOR FILL-IN adder-label IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rfqitem.adder[10] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rfqitem.adder[11] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rfqitem.adder[12] IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       rfqitem.adder[1]:PRIVATE-DATA IN FRAME F-Main     = 
                "adder1".

ASSIGN 
       rfqitem.adder[2]:PRIVATE-DATA IN FRAME F-Main     = 
                "adder2".

ASSIGN 
       rfqitem.adder[3]:PRIVATE-DATA IN FRAME F-Main     = 
                "adder3".

ASSIGN 
       rfqitem.adder[4]:PRIVATE-DATA IN FRAME F-Main     = 
                "adder4".

ASSIGN 
       rfqitem.adder[5]:PRIVATE-DATA IN FRAME F-Main     = 
                "adder5".

ASSIGN 
       rfqitem.adder[6]:PRIVATE-DATA IN FRAME F-Main     = 
                "adder6".

/* SETTINGS FOR FILL-IN rfqitem.adder[7] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rfqitem.adder[8] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rfqitem.adder[9] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rfqitem.brd-dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F1 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F1:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fi_spec-qty-01 IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_spec-qty-02 IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_spec-qty-03 IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_spec-qty-04 IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_spec-qty-05 IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_spec-qty-06 IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN rfqitem.flute IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rfqitem.leaf-dscr[1] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rfqitem.leaf-dscr[2] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rfqitem.leaf-dscr[3] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rfqitem.leaf-dscr[4] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rfqitem.leaf[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN rfqitem.leaf[2] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN rfqitem.leaf[3] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN rfqitem.leaf[4] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN lv-adder IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-adder-dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rfqitem.spec-no[1] IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN rfqitem.spec-no[2] IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN rfqitem.spec-no[3] IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN rfqitem.spec-no[4] IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN rfqitem.spec-no[5] IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN rfqitem.spec-no[6] IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN rfqitem.test IN FRAME F-Main
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
ON GO OF FRAME F-Main
DO:
   def var out-hdl-str as cha no-undo.
   run get-link-handle in adm-broker-hdl (this-procedure,"TABLEIO-SOURCE",output out-hdl-str).   
   run notify in widget-handle(out-hdl-str) ("update-record"). 


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
  def var lv-ind like style.industry no-undo.
  case focus:label :
     when "Board" then do:
           find style where style.company = rfq.company and
                            style.style = rfqitem.style
                            no-lock no-error.   
           if avail style then lv-ind = style.industry.
           else lv-ind = "".  
           if avail style and style.type = "f" then  /* foam */
                 run windows/l-boardf.w (rfqitem.company,lv-ind,focus:screen-value,output char-val).
           else run windows/l-board.w (rfqitem.company,lv-ind,focus:screen-value, output char-val).
           if char-val <> "" then do:
              assign rfqitem.board:screen-value in frame {&frame-name} = entry(1,char-val)
                     rfqitem.cal:screen-value in frame {&frame-name} = entry(2,char-val)
                     rfqitem.brd-dscr:screen-value in frame {&frame-name} = entry(3,char-val).  
              find item where item.company = rfqitem.company and
                              item.i-no = entry(1,char-val)
                              no-lock no-error.
              if avail item then assign rfqitem.gsh-wid:screen-value = if item.r-wid <> 0 then string(item.r-wid) else string(item.s-wid)
                                        rfqitem.gsh-len:screen-value = string(item.s-len) 
                                        rfqitem.test:screen-value = item.reg-no
                                        rfqitem.flute:screen-value = item.flute
                                        .
           end.
           return no-apply.   
     end.
     when "Window/Wax" then do:
       /*  Own lookup program  */
           find style where style.company = rfq.company and
                            style.style = rfqitem.style
                            no-lock no-error.   
           if avail style then lv-ind = style.industry.
           else lv-ind = "".  
           run windows/l-item.w (rfqitem.company,lv-ind,"W", focus:screen-value,output char-val).
           if char-val <> "" then do:
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     rfqitem.leaf-dscr[1]:screen-value in frame {&frame-name} = entry(2,char-val)
                     .
              find item where item.company = rfqitem.company and
                              item.i-no = entry(1,char-val)
                              no-lock no-error.
              if avail item then assign rfqitem.leaf-w[1]:screen-value = if item.r-wid <> 0 then string(item.r-wid) else string(item.s-wid)
                                        rfqitem.leaf-l[1]:screen-value = string(item.s-len) 
                                        .
           end.
           if true then return no-apply.   
        /*  toolkit lookup 
           run applhelp.p.
           if g_lookup-var <> "" then do:
                focus:screen-value = g_lookup-var.
                find item where item.company = rfqitem.company and
                                item.i-no = g_lookup-var
                                no-lock no-error.
                rfqitem.leaf-dscr[1]:screen-value = item.i-name.                
           end.
           ================== */
           if true then return no-apply.  
     end.    
     when "Foil" then do:
           find style where style.company = rfq.company and
                            style.style = rfqitem.style
                            no-lock no-error.   
           if avail style then lv-ind = style.industry.
           else lv-ind = "".  
           run windows/l-item.w (rfqitem.company,lv-ind,"F",focus:screen-value, output char-val).
           if char-val <> "" then do:
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     rfqitem.leaf-dscr[2]:screen-value in frame {&frame-name} = entry(2,char-val)
                     .
              find item where item.company = rfqitem.company and
                              item.i-no = entry(1,char-val)
                              no-lock no-error.
              if avail item then assign rfqitem.leaf-w[2]:screen-value = if item.r-wid <> 0 then string(item.r-wid) else string(item.s-wid)
                                        rfqitem.leaf-l[2]:screen-value = string(item.s-len) 
                                        .
           end.
           return no-apply.   
     end.  
     when "Stamp" then do:
           find style where style.company = rfq.company and
                            style.style = rfqitem.style
                            no-lock no-error.   
           if avail style then lv-ind = style.industry.
           else lv-ind = "".  
           run windows/l-item.w (rfqitem.company,lv-ind,"F",focus:screen-value, output char-val).
           if char-val <> "" then do:
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     rfqitem.leaf-dscr[3]:screen-value in frame {&frame-name} = entry(2,char-val)
                     .
              find item where item.company = rfqitem.company and
                              item.i-no = entry(1,char-val)
                              no-lock no-error.
              if avail item then assign rfqitem.leaf-w[3]:screen-value = if item.r-wid <> 0 then string(item.r-wid) else string(item.s-wid)
                                        rfqitem.leaf-l[3]:screen-value = string(item.s-len) 
                                        .
           end.
           return no-apply.   
     end.  
     /* no Glue lap any more
     when "Glue Lap" then do:
           find style where style.company = rfq.company and
                            style.style = rfqitem.style
                            no-lock no-error.   
           if avail style then lv-ind = style.industry.
           else lv-ind = "".  
           run windows/l-item.w (rfqitem.company,lv-ind,"G", focus:screen-value,output char-val).
           if char-val <> "" then do:
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     rfqitem.leaf-dscr[3]:screen-value in frame {&frame-name} = entry(2,char-val)
                     .
              find item where item.company = rfqitem.company and
                              item.i-no = entry(1,char-val)
                              no-lock no-error.
              if avail item then assign rfqitem.leaf-w[3]:screen-value = if item.r-wid <> 0 then string(item.r-wid) else string(item.s-wid)
                                        rfqitem.leaf-l[3]:screen-value = string(item.s-len) 
                                        . 
           end.
           return no-apply.   
     end.
     */  
     when "Laminate" then do:
           find style where style.company = rfq.company and
                            style.style = rfqitem.style
                            no-lock no-error.   
           if avail style then lv-ind = style.industry.
           else lv-ind = "".  
           run windows/l-item.w (rfqitem.company,lv-ind,"L",focus:screen-value, output char-val).
           if char-val <> "" then do:
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     rfqitem.leaf-dscr[4]:screen-value in frame {&frame-name} = entry(2,char-val)
                     .
              find item where item.company = rfqitem.company and
                              item.i-no = entry(1,char-val)
                              no-lock no-error.
              if avail item then assign rfqitem.leaf-w[4]:screen-value = if item.r-wid <> 0 then string(item.r-wid) else string(item.s-wid)
                                        rfqitem.leaf-l[4]:screen-value = string(item.s-len) 
                                        .      
           end.          
           return no-apply.   
     end.  
     when "Flute" then do:
           run windows/l-flute.w (rfqitem.company, focus:screen-value,output char-val).
           if char-val <> "" then 
              focus:screen-value in frame {&frame-name} = entry(1,char-val).
           return no-apply.   
     end. 
     when "Test" then do:
           run windows/l-test.w 
              (rfqitem.company, rfqitem.loc, rfqitem.flute:screen-value in frame {&frame-name}, output char-val).
           if char-val <> "" then 
              focus:screen-value in frame {&frame-name} = entry(1,char-val).
           return no-apply.  
     end. 
     otherwise do:  /* no labels */
       case focus:name :   
          when "adder" then do:
           find style where style.company = rfqitem.company and
                            style.style = focus:screen-value in frame {&frame-name}
                             no-lock no-error.   
           if avail style then assign lv-ind = style.industry.
           else assign lv-ind = "".
           run windows/l-boarda.w (rfqitem.company,lv-ind, focus:screen-value, output char-val).
           if char-val <> "" then do:
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                      .
              case focus:private-data :
                when "adder1" then rfqitem.adder[7]:screen-value in frame {&frame-name} = entry(2,char-val).
                when "adder2" then rfqitem.adder[8]:screen-value in frame {&frame-name} = entry(2,char-val).
                when "adder3" then rfqitem.adder[9]:screen-value in frame {&frame-name} = entry(2,char-val).
                when "adder4" then rfqitem.adder[10]:screen-value in frame {&frame-name} = entry(2,char-val).
                when "adder5" then rfqitem.adder[11]:screen-value in frame {&frame-name} = entry(2,char-val).
                when "adder6" then rfqitem.adder[12]:screen-value in frame {&frame-name} = entry(2,char-val).
              end case. 
           end.
           return no-apply.
         end .   /* adder */
         when "spec-no" then do:
       /*       run windows/l-itemf2.w  (rfqitem.company, focus:screen-value,output char-val). */
           find style where style.company = rfqitem.company and
                            style.style = focus:screen-value in frame {&frame-name}
                             no-lock no-error.   
           if avail style then assign lv-ind = style.industry.
           else assign lv-ind = "".

              run windows/l-item.w (rfqitem.company,lv-ind,"M",focus:screen-value,output char-val).
              if char-val <> "" then do:
                 assign focus:screen-value in frame {&frame-name} = entry(1,char-val).
                 case focus:index:
                      when 1 then rfqitem.spec-dscr[1]:screen-value in frame {&frame-name} = entry(2,char-val).        
                      when 2 then rfqitem.spec-dscr[2]:screen-value in frame {&frame-name} = entry(2,char-val).
                      when 3 then rfqitem.spec-dscr[3]:screen-value in frame {&frame-name} = entry(2,char-val).            
                      when 4 then rfqitem.spec-dscr[4]:screen-value in frame {&frame-name} = entry(2,char-val).
                      when 5 then rfqitem.spec-dscr[5]:screen-value in frame {&frame-name} = entry(2,char-val).
                      when 6 then rfqitem.spec-dscr[6]:screen-value in frame {&frame-name} = entry(2,char-val).                                            
                 end. 
              end.
              return no-apply.
         end.
         otherwise do:
             run applhelp.p.
             message g_lookup-var view-as alert-box.       
             if g_lookup-var <> "" then do:
                focus:screen-value = g_lookup-var.
             end.
             return no-apply.  
         end.
       end case.  /* case focus:name */           
     end.  /* otherwise */
  end case.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON return OF FRAME F-Main
anywhere
DO:
  /*  def var lv-wh as handle no-undo.
    lv-wh = focus:next-tab-item.
    do while lv-wh:sensitive = no :
       lv-wh = lv-wh:next-tab-item.
    end.

    apply "entry" to lv-wh.
    return no-apply.
  */
    return.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.adder[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.adder[1] V-table-Win
ON LEAVE OF rfqitem.adder[1] IN FRAME F-Main /* Adder[1] */
DO:
       if self:screen-value = "" then return.
  find item where item.company = rfqitem.company 
                 and item.i-no = self:screen-value
                     no-lock no-error.
     if avail item and item.mat-type = "A" 
     then    rfqitem.adder[7]:screen-value = item.i-name. 
       else if lastkey <> -1 then do:
        {&methods/lValidateError.i YES}
           message "Invalid Material. Try Help." view-as alert-box error.
           return no-apply.
       {&methods/lValidateError.i NO}
       end.

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.adder[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.adder[2] V-table-Win
ON LEAVE OF rfqitem.adder[2] IN FRAME F-Main /* Adder[2] */
DO:
     if self:screen-value = "" then return.  
  find item where item.company = rfqitem.company 
                 and item.i-no = self:screen-value
                     no-lock no-error.
     if avail item and item.mat-type = "A" 
     then    rfqitem.adder[8]:screen-value = item.i-name. 
       else if lastkey <> -1 then do:
       {&methods/lValidateError.i YES}
           message "Invalid Material. Try Help." view-as alert-box error.
           return no-apply.
       {&methods/lValidateError.i NO}
       end.

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.adder[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.adder[3] V-table-Win
ON LEAVE OF rfqitem.adder[3] IN FRAME F-Main /* Adder[3] */
DO:
       if self:screen-value = "" then return.
  find item where item.company = rfqitem.company 
                 and item.i-no = self:screen-value
                     no-lock no-error.
     if avail item and item.mat-type = "A" 
     then    rfqitem.adder[9]:screen-value = item.i-name. 
       else if lastkey <> -1 then do:
       {&methods/lValidateError.i YES}
           message "Invalid Material. Try Help." view-as alert-box error.
           return no-apply.
       {&methods/lValidateError.i NO}
       end.

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.adder[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.adder[4] V-table-Win
ON LEAVE OF rfqitem.adder[4] IN FRAME F-Main /* Adder[4] */
DO:
       if self:screen-value = "" then return.
  find item where item.company = rfqitem.company 
                 and item.i-no = self:screen-value
                     no-lock no-error.
     if avail item and item.mat-type = "A" 
     then    rfqitem.adder[10]:screen-value = item.i-name. 
       else if lastkey <> -1 then do:
       {&methods/lValidateError.i YES}
           message "Invalid Material. Try Help." view-as alert-box error.
           return no-apply.
        {&methods/lValidateError.i NO}
       end.

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.adder[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.adder[5] V-table-Win
ON LEAVE OF rfqitem.adder[5] IN FRAME F-Main /* Adder[5] */
DO:
       if self:screen-value = "" then return.
  find item where item.company = rfqitem.company 
                 and item.i-no = self:screen-value
                     no-lock no-error.
     if avail item and item.mat-type = "A" 
     then    rfqitem.adder[11]:screen-value = item.i-name. 
       else if lastkey <> -1 then do:
       {&methods/lValidateError.i YES}
           message "Invalid Material. Try Help." view-as alert-box error.
           return no-apply.
       {&methods/lValidateError.i NO}
       end.

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.adder[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.adder[6] V-table-Win
ON LEAVE OF rfqitem.adder[6] IN FRAME F-Main /* Adder[6] */
DO:
       if self:screen-value = "" then return.
  find item where item.company = rfqitem.company 
                 and item.i-no = self:screen-value
                     no-lock no-error.
     if avail item and item.mat-type = "A" 
     then    rfqitem.adder[12]:screen-value = item.i-name. 
       else if lastkey <> -1 then do:
       {&methods/lValidateError.i YES}
           message "Invalid Material. Try Help." view-as alert-box error.
           return no-apply.
       {&methods/lValidateError.i NO}
       end.

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.board
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.board V-table-Win
ON ENTRY OF rfqitem.board IN FRAME F-Main /* Board */
DO:
   ls-pre-mat = self:screen-value.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.board V-table-Win
ON LEAVE OF rfqitem.board IN FRAME F-Main /* Board */
DO:
  /*find item where item.company = rfqitem.company and
                   item.i-no = self:screen-value
             no-lock no-error.
   if avail item then do:
      brd-dscr:screen-value = item.i-name.
      if decimal(rfqitem.gsh-wid:screen-value) = 0 then 
         rfqitem.gsh-wid:screen-value = if item.r-wid <> 0 then string(item.r-wid) else string(item.s-wid).
      if decimal(rfqitem.gsh-len:screen-value) = 0 then rfqitem.gsh-len:screen-value = string(item.s-len) .
      if decimal(rfqitem.cal:screen-value) = 0 then 
         rfqitem.cal:screen-value in frame {&frame-name} = string(item.cal) .
      if rfqitem.test:screen-value = "" then rfqitem.test:screen-value = item.reg-no.
      if (rfqitem.flute:screen-value) = "" then rfqitem.flute:screen-value = item.flute.
   end.
   */
   if ls-pre-mat <> self:screen-value then do:
      find item where item.company = rfqitem.company and
                   item.i-no = self:screen-value
             no-lock no-error.
      if avail item and 
         ((item.mat-type >= "1" and item.mat-type <="4") 
           or item.mat-type = "B" or item.mat-type = "P")
      then do:
        assign brd-dscr:screen-value = item.i-name
               rfqitem.gsh-wid:screen-value in frame {&frame-name} = if item.r-wid <> 0 then string(item.r-wid) else string(item.s-wid)
               rfqitem.gsh-len:screen-value = string(item.s-len)
               rfqitem.cal:screen-value = string(item.cal) 
               rfqitem.test:screen-value = item.reg-no
               rfqitem.flute:screen-value = item.flute
               .             
      end.
      else if lastkey <> -1 then do:
           {&methods/lValidateError.i YES}
           message "Invalid Material. Try Help." view-as alert-box error.
           return no-apply.
           {&methods/lValidateError.i NO}
      end.
   end.

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.flute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.flute V-table-Win
ON LEAVE OF rfqitem.flute IN FRAME F-Main /* Flute */
DO:
    if self:screen-value = "" then return.
    find first reftable where reftable.reftable = "Flute" and 
                              reftable.company  = "" and
                              reftable.loc = "" and
                              reftable.code = self:screen-value
         no-lock no-error.                          
    if not avail reftable and lastkey <> -1 then do:
    {&methods/lValidateError.i YES}
       message "Invalid Flute. Try Help." view-as alert-box error.
       return no-apply.
     {&methods/lValidateError.i NO}
    end.     



END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.leaf[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.leaf[1] V-table-Win
ON ENTRY OF rfqitem.leaf[1] IN FRAME F-Main /* Window/Wax */
DO:
     ls-pre-mat = self:screen-value.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.leaf[1] V-table-Win
ON LEAVE OF rfqitem.leaf[1] IN FRAME F-Main /* Window/Wax */
DO:  
  if ls-pre-mat <> self:screen-value then do:
      find item where item.company = rfqitem.company and
                   item.i-no = self:screen-value
             no-lock no-error.
      if avail item and item.mat-type = "W" 
      then do:
      /*      rfqitem.leaf-dscr[2]:screen-value in frame {&frame-name} = item.i-name.             
        if decimal(rfqitem.leaf-w[2]:screen-value) = 0 then 
            rfqitem.leaf-w[2]:screen-value = if item.r-wid <> 0 then string(item.r-wid) else string(item.s-wid).
        if decimal(rfqitem.leaf-l[2]:screen-value) = 0 then      
            rfqitem.leaf-l[2]:screen-value = string(item.s-len) .
      */
        assign rfqitem.leaf-dscr[1]:screen-value in frame {&frame-name} = item.i-name
               rfqitem.leaf-w[1]:screen-value = if item.r-wid <> 0 then string(item.r-wid) else string(item.s-wid).
               rfqitem.leaf-l[1]:screen-value = string(item.s-len) .
               .             
       end.
       else if lastkey <> -1 then
       do:
       {&methods/lValidateError.i YES}
           message "Invalid Material. Try Help." view-as alert-box error.
           return no-apply.
       {&methods/lValidateError.i NO}
       end.
   end.

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.leaf[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.leaf[2] V-table-Win
ON ENTRY OF rfqitem.leaf[2] IN FRAME F-Main /* Foil */
DO:
     ls-pre-mat = self:screen-value.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.leaf[2] V-table-Win
ON LEAVE OF rfqitem.leaf[2] IN FRAME F-Main /* Foil */
DO:
  if ls-pre-mat <> self:screen-value then do:
      find item where item.company = rfqitem.company and
                   item.i-no = self:screen-value
             no-lock no-error.
      if avail item and item.mat-type = "F" 
      then do:
      /*      rfqitem.leaf-dscr[2]:screen-value in frame {&frame-name} = item.i-name.             
        if decimal(rfqitem.leaf-w[2]:screen-value) = 0 then 
            rfqitem.leaf-w[2]:screen-value = if item.r-wid <> 0 then string(item.r-wid) else string(item.s-wid).
        if decimal(rfqitem.leaf-l[2]:screen-value) = 0 then      
            rfqitem.leaf-l[2]:screen-value = string(item.s-len) .
      */
        assign rfqitem.leaf-dscr[2]:screen-value in frame {&frame-name} = item.i-name
               rfqitem.leaf-w[2]:screen-value = if item.r-wid <> 0 then string(item.r-wid) else string(item.s-wid).
               rfqitem.leaf-l[2]:screen-value = string(item.s-len) .
               .             
       end.
       else if lastkey <> -1 then
       do:
        {&methods/lValidateError.i YES}
           message "Invalid Material. Try Help." view-as alert-box error.
           return no-apply.
       {&methods/lValidateError.i NO}
       end.

   end.

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.leaf[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.leaf[3] V-table-Win
ON LEAVE OF rfqitem.leaf[3] IN FRAME F-Main /* Stamp */
DO:
  if ls-pre-mat <> self:screen-value then do:
      find item where item.company = rfqitem.company and
                   item.i-no = self:screen-value
             no-lock no-error.
      if avail item and item.mat-type = "G" 
      then do:
      /*      rfqitem.leaf-dscr[3]:screen-value in frame {&frame-name} = item.i-name.             
        if decimal(rfqitem.leaf-w[3]:screen-value) = 0 then 
            rfqitem.leaf-w[3]:screen-value = if item.r-wid <> 0 then string(item.r-wid) else string(item.s-wid).
        if decimal(rfqitem.leaf-l[3]:screen-value) = 0 then      
            rfqitem.leaf-l[3]:screen-value = string(item.s-len) .
      */
        assign rfqitem.leaf-dscr[3]:screen-value in frame {&frame-name} = item.i-name
               rfqitem.leaf-w[3]:screen-value = if item.r-wid <> 0 then string(item.r-wid) else string(item.s-wid).
               rfqitem.leaf-l[3]:screen-value = string(item.s-len) .
               .             
       end.
       else if lastkey <> -1 then
       do:
        {&methods/lValidateError.i YES}
           message "Invalid Material. Try Help." view-as alert-box error.
           return no-apply.
        {&methods/lValidateError.i NO}
       end.

   end.
.

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.leaf[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.leaf[4] V-table-Win
ON ENTRY OF rfqitem.leaf[4] IN FRAME F-Main /* Laminate */
DO:
       ls-pre-mat = self:screen-value.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.leaf[4] V-table-Win
ON LEAVE OF rfqitem.leaf[4] IN FRAME F-Main /* Laminate */
DO:
   if ls-pre-mat <> self:screen-value then do:
      find item where item.company = rfqitem.company and
                   item.i-no = self:screen-value
             no-lock no-error.
      if avail item and item.mat-type = "L" 
      then do:
      /*      rfqitem.leaf-dscr[4]:screen-value in frame {&frame-name} = item.i-name.             
        if decimal(rfqitem.leaf-w[4]:screen-value) = 0 then 
            rfqitem.leaf-w[4]:screen-value = if item.r-wid <> 0 then string(item.r-wid) else string(item.s-wid).
        if decimal(rfqitem.leaf-l[4]:screen-value) = 0 then      
            rfqitem.leaf-l[4]:screen-value = string(item.s-len) .
      */
        assign rfqitem.leaf-dscr[4]:screen-value in frame {&frame-name} = item.i-name
               rfqitem.leaf-w[4]:screen-value = if item.r-wid <> 0 then string(item.r-wid) else string(item.s-wid).
               rfqitem.leaf-l[4]:screen-value = string(item.s-len) .
               .             
       end.
       else if lastkey <> -1 then 
       do:
        {&methods/lValidateError.i YES}
           message "Invalid Material. Try Help." view-as alert-box error.
           return no-apply.
        {&methods/lValidateError.i NO}
       end.

   end.

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.spec-no[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.spec-no[1] V-table-Win
ON LEAVE OF rfqitem.spec-no[1] IN FRAME F-Main /* RM Item#[1] */
DO:
     if self:screen-value = "" then return.
     find item where item.company = rfqitem.company 
                 and item.i-no = self:screen-value
                     no-lock no-error.
     if avail item and INDEX("MOXY789@",ITEM.mat-type) GT 0
     then    rfqitem.spec-dscr[1]:screen-value = item.i-name. 
     else if lastkey <> -1 then do:
      {&methods/lValidateError.i YES}
           message "Invalid Material. Try Help." view-as alert-box error.
           return no-apply.
       {&methods/lValidateError.i NO}
    end.

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.spec-no[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.spec-no[2] V-table-Win
ON LEAVE OF rfqitem.spec-no[2] IN FRAME F-Main /* RM Item#[2] */
DO:
       if self:screen-value = "" then return.
     find item where item.company = rfqitem.company 
                 and item.i-no = self:screen-value
                     no-lock no-error.
     if avail item and INDEX("MOXY789",ITEM.mat-type) GT 0 
     then    rfqitem.spec-dscr[2]:screen-value = item.i-name. 
     else if lastkey <> -1 then do:
     {&methods/lValidateError.i YES}
           message "Invalid Material. Try Help." view-as alert-box error.
           return no-apply.
     {&methods/lValidateError.i NO}
     end.


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.spec-no[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.spec-no[3] V-table-Win
ON LEAVE OF rfqitem.spec-no[3] IN FRAME F-Main /* RM Item#[3] */
DO:
       if self:screen-value = "" then return.
     find item where item.company = rfqitem.company 
                 and item.i-no = self:screen-value
                     no-lock no-error.
     if avail item and INDEX("MOXY789",ITEM.mat-type) GT 0 
     then    rfqitem.spec-dscr[3]:screen-value = item.i-name. 
       else if lastkey <> -1 then do:
       {&methods/lValidateError.i YES}
           message "Invalid Material. Try Help." view-as alert-box error.
           return no-apply.
      {&methods/lValidateError.i NO}
       end.

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.spec-no[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.spec-no[4] V-table-Win
ON LEAVE OF rfqitem.spec-no[4] IN FRAME F-Main /* RM Item#[4] */
DO:
       if self:screen-value = "" then return.
     find item where item.company = rfqitem.company 
                 and item.i-no = self:screen-value
                     no-lock no-error.
     if avail item and INDEX("MOXY789",ITEM.mat-type) GT 0 
     then    rfqitem.spec-dscr[4]:screen-value = item.i-name.
     else if lastkey <> -1 then do:
     {&methods/lValidateError.i YES}
           message "Invalid Material. Try Help." view-as alert-box error.
           return no-apply.
      {&methods/lValidateError.i NO}
     end.


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.spec-no[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.spec-no[5] V-table-Win
ON LEAVE OF rfqitem.spec-no[5] IN FRAME F-Main /* RM Item#[5] */
DO:
       if self:screen-value = "" then return.
     find item where item.company = rfqitem.company 
                 and item.i-no = self:screen-value
                     no-lock no-error.
     if avail item and INDEX("MOXY789",ITEM.mat-type) GT 0 
     then    rfqitem.spec-dscr[5]:screen-value = item.i-name. 
     else if lastkey <> -1 then do:
     {&methods/lValidateError.i YES}
           message "Invalid Material. Try Help." view-as alert-box error.
           return no-apply.
     {&methods/lValidateError.i NO}
     end.

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.spec-no[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.spec-no[6] V-table-Win
ON LEAVE OF rfqitem.spec-no[6] IN FRAME F-Main /* RM Item#[6] */
DO:
       if self:screen-value = "" then return.
     find item where item.company = rfqitem.company 
                 and item.i-no = self:screen-value
                     no-lock no-error.
     if avail item and INDEX("MOXY789",ITEM.mat-type) GT 0 
     then    rfqitem.spec-dscr[6]:screen-value = item.i-name. 
       else if lastkey <> -1 then do:
        {&methods/lValidateError.i YES}
           message "Invalid Material. Try Help." view-as alert-box error.
           return no-apply.
        {&methods/lValidateError.i NO}
       end.


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.test
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.test V-table-Win
ON LEAVE OF rfqitem.test IN FRAME F-Main /* Test */
DO:
   if self:screen-value = "" then return.

   def var v-term as cha no-undo.
   v-term = string(year(today),"9999") +
            string(month(today),"99")  +
            string(day(today),"99") /* +
            STRING(time,"99999")*/ .

   find first report
      WHERE report.term-id begins v-term and  report.key-01  eq "TEST"  and
            report.key-02 = self:screen-value
      NO-LOCK no-error.
  if not avail report and lastkey <> -1 then do:
    {&methods/lValidateError.i YES}
     message "Invalid Test. Try Help." view-as alert-box error.
     return no-apply.
     {&methods/lValidateError.i NO}

  end.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
  session:data-entry-return = true.  /* return key will be like tab key */
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
  {src/adm/template/row-list.i "rfqitem"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "rfqitem"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hide-adders V-table-Win 
PROCEDURE hide-adders :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   find style where style.company = rfqitem.company and
                    style.style = rfqitem.style
                    no-lock no-error.
   if avail style and style.industry = "1" /* folding carton */                    
   then do:
        assign rfqitem.adder[1]:hidden  in frame {&frame-name} = true
               rfqitem.adder[2]:hidden  in frame {&frame-name} = true
               rfqitem.adder[3]:hidden  in frame {&frame-name} = true
               rfqitem.adder[4]:hidden  in frame {&frame-name} = true
               rfqitem.adder[5]:hidden  in frame {&frame-name} = true
               rfqitem.adder[6]:hidden  in frame {&frame-name} = true
               rfqitem.adder[7]:hidden  in frame {&frame-name} = true
               rfqitem.adder[8]:hidden  in frame {&frame-name} = true
               rfqitem.adder[9]:hidden  in frame {&frame-name} = true
               rfqitem.adder[10]:hidden  in frame {&frame-name} = true
               rfqitem.adder[11]:hidden  in frame {&frame-name} = true
               rfqitem.adder[12]:hidden  in frame {&frame-name} = true
               rect-18:hidden  in frame {&frame-name} = true
               adder-label:hidden  in frame {&frame-name} = true
               rfqitem.flute:hidden in frame {&frame-name} = true
               rfqitem.test:hidden in frame {&frame-name} = true               
               lv-adder:hidden in frame {&frame-name} = true
               lv-adder-dscr:hidden in frame {&frame-name} = true               
           /*  frame {&frame-name}:height = frame {&frame-name}:height - 2*/
               rfqitem.leaf[3]:label in frame {&frame-name} = "Stamp"  /* used be Glus lap */
               rfqitem.leaf[4]:label in frame {&frame-name} = "Laminate"
               .                 

   end.
   else do:

        assign rfqitem.adder[1]:hidden  in frame {&frame-name} = false
               rfqitem.adder[2]:hidden  in frame {&frame-name} = false
               rfqitem.adder[3]:hidden  in frame {&frame-name} = false
               rfqitem.adder[4]:hidden  in frame {&frame-name} = false
               rfqitem.adder[5]:hidden  in frame {&frame-name} = false
               rfqitem.adder[6]:hidden  in frame {&frame-name} = false
               rfqitem.adder[7]:hidden  in frame {&frame-name} = false
               rfqitem.adder[8]:hidden  in frame {&frame-name} = false
               rfqitem.adder[9]:hidden  in frame {&frame-name} = false
               rfqitem.adder[10]:hidden  in frame {&frame-name} = false
               rfqitem.adder[11]:hidden  in frame {&frame-name} = false
               rfqitem.adder[12]:hidden  in frame {&frame-name} = false
               rect-18:hidden  in frame {&frame-name} = false
               adder-label:hidden  in frame {&frame-name} = false
               rfqitem.flute:hidden in frame {&frame-name} = false
               rfqitem.test:hidden in frame {&frame-name} = false    
               lv-adder:hidden in frame {&frame-name} = false
               lv-adder-dscr:hidden in frame {&frame-name} = false               
              /* rfqitem.adder[1]:sensitive  in frame {&frame-name} = true 
               rfqitem.adder[2]:sensitive  in frame {&frame-name} = true 
               rfqitem.adder[3]:sensitive  in frame {&frame-name} = true 
               rfqitem.adder[4]:sensitive  in frame {&frame-name} = true 
               rfqitem.adder[5]:sensitive  in frame {&frame-name} = true
               rfqitem.adder[6]:sensitive  in frame {&frame-name} = true 
          /*   rfqitem.adder[7]:sensitive  in frame {&frame-name} = false
               rfqitem.adder[8]:sensitive  in frame {&frame-name} = false
               rfqitem.adder[9]:sensitive  in frame {&frame-name} = false
               rfqitem.adder[10]:sensitive  in frame {&frame-name} = false
               rfqitem.adder[11]:sensitive  in frame {&frame-name} = false
               rfqitem.adder[12]:sensitive  in frame {&frame-name} = false
            */*/
              /* rfqitem.flute:sensitive in frame {&frame-name} = true
               rfqitem.test:sensitive in frame {&frame-name} = true                  
              */
               rfqitem.leaf[3]:label in frame {&frame-name} = "Stamp"/*"Joint Mat."*/
               rfqitem.leaf[4]:label in frame {&frame-name} = "".                 
               .
         display rfqitem.adder[1 for 10]
                 rfqitem.flute
                 rfqitem.test
                 with frame {&frame-name}.  /* Progress bug not display value if you change row from item browser */
   end.
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
  ASSIGN
   lv-spec-qty[01] = fi_spec-qty-01
   lv-spec-qty[02] = fi_spec-qty-02
   lv-spec-qty[03] = fi_spec-qty-03
   lv-spec-qty[04] = fi_spec-qty-04
   lv-spec-qty[05] = fi_spec-qty-05
   lv-spec-qty[06] = fi_spec-qty-06.

  DO li-sq = 1 TO EXTENT(rfqitem.spec-qty):
    RUN custom/extradec.p (10000, lv-spec-qty[li-sq],
                           OUTPUT rfqitem.spec-qty[li-sq]).
  END.

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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
  lv-spec-qty = 0.

  IF AVAIL rfqitem THEN
  DO li-sq = 1 TO EXTENT(rfqitem.spec-qty):
    RUN custom/extradec.p (.0001, rfqitem.spec-qty[li-sq],
                           OUTPUT lv-spec-qty[li-sq]).
  END.

  ASSIGN
   fi_spec-qty-01 = lv-spec-qty[01]
   fi_spec-qty-02 = lv-spec-qty[02]
   fi_spec-qty-03 = lv-spec-qty[03]
   fi_spec-qty-04 = lv-spec-qty[04]
   fi_spec-qty-05 = lv-spec-qty[05]
   fi_spec-qty-06 = lv-spec-qty[06].

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  run hide-adders.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN hide-adders.

  DO WITH FRAME {&frame-name}:
    ENABLE fi_spec-qty-01 fi_spec-qty-02 fi_spec-qty-03
           fi_spec-qty-04 fi_spec-qty-05 fi_spec-qty-06.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-hide V-table-Win 
PROCEDURE local-hide :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  /* check mode where in update */
  DEFINE VARIABLE vlChangePages AS LOGICAL NO-UNDO.  
  RUN GET-ATTRIBUTE("FIELDS-ENABLED":U).
  IF RETURN-VALUE = "YES":U THEN
  DO:
    MESSAGE "Would you like to save changes before changing pages?":U
       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE vlChangePages.
    RUN dispatch IN THIS-PROCEDURE (IF vlChangePages THEN
                                      'update-record':U
                                    ELSE
                                      'cancel-record':U).
  END.


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'hide':U ) .

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

  /* Code placed here will execute PRIOR to standard behavior. */
  DISABLE ALL WITH FRAME {&FRAME-NAME}.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  assign rfqitem.brd-dscr = rfqitem.brd-dscr:screen-value in frame {&frame-name}
         rfqitem.leaf-dscr[1] = rfqitem.leaf-dscr[1]:screen-value in frame {&frame-name}
         rfqitem.leaf-dscr[2] = rfqitem.leaf-dscr[2]:screen-value in frame {&frame-name}
         rfqitem.leaf-dscr[3] = rfqitem.leaf-dscr[3]:screen-value in frame {&frame-name}
         rfqitem.leaf-dscr[4] = rfqitem.leaf-dscr[4]:screen-value in frame {&frame-name}
         rfqitem.adder[1] = rfqitem.adder[1]:screen-value in frame {&frame-name}
         rfqitem.adder[2] = rfqitem.adder[2]:screen-value in frame {&frame-name}
         rfqitem.adder[3] = rfqitem.adder[3]:screen-value in frame {&frame-name}
         rfqitem.adder[4] = rfqitem.adder[4]:screen-value in frame {&frame-name}
         rfqitem.adder[5] = rfqitem.adder[5]:screen-value in frame {&frame-name}
         rfqitem.adder[6] = rfqitem.adder[6]:screen-value in frame {&frame-name}       
         rfqitem.adder[7] = rfqitem.adder[7]:screen-value in frame {&frame-name}
         rfqitem.adder[8] = rfqitem.adder[8]:screen-value in frame {&frame-name}
         rfqitem.adder[9] = rfqitem.adder[9]:screen-value in frame {&frame-name}
         rfqitem.adder[10] = rfqitem.adder[10]:screen-value in frame {&frame-name}
         rfqitem.adder[11] = rfqitem.adder[11]:screen-value in frame {&frame-name}
         rfqitem.adder[12] = rfqitem.adder[12]:screen-value in frame {&frame-name}       
         .

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
  {src/adm/template/snd-list.i "rfqitem"}

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

