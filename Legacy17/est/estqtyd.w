&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

def input param ip-recid as recid no-undo.
def input param ip-eb-recid as recid no-undo.
def input param ip-qty as cha no-undo.
def output param op-char-val as cha no-undo.
def output param op-char-val2 as cha no-undo.
def output param op-date-val as cha no-undo.
def output param op-date-val2 as cha no-undo.

def var i as int no-undo.
def var cocode as cha no-undo.
DEF VAR ld-msf AS DEC NO-UNDO.

{custom/globdefs.i}
cocode = g_company.
{cec/msfcalc.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-qty1 lv-rel-1 lv-qty2 lv-rel-2 lv-qty3 ~
lv-rel-3 lv-qty4 lv-rel-4 lv-qty5 lv-rel-5 lv-qty6 lv-rel-6 lv-qty7 ~
lv-rel-7 lv-qty8 lv-rel-8 lv-qty9 lv-rel-9 lv-qty10 lv-rel-10 lv-qty11 ~
lv-rel-11 lv-qty12 lv-rel-12 lv-qty13 lv-rel-13 lv-qty14 lv-rel-14 lv-qty15 ~
lv-rel-15 lv-qty16 lv-rel-16 lv-qty17 lv-rel-17 lv-qty18 lv-rel-18 lv-qty19 ~
lv-rel-19 lv-qty20 lv-rel-20 Btn_OK Btn_Cancel RECT-6 
&Scoped-Define DISPLAYED-OBJECTS lv-qty1 lv-rel-1 lv-qty2 lv-rel-2 lv-qty3 ~
lv-rel-3 lv-qty4 lv-rel-4 lv-qty5 lv-rel-5 lv-qty6 lv-rel-6 lv-qty7 ~
lv-rel-7 lv-qty8 lv-rel-8 lv-qty9 lv-rel-9 lv-qty10 lv-rel-10 lv-qty11 ~
lv-rel-11 lv-qty12 lv-rel-12 lv-qty13 lv-rel-13 lv-qty14 lv-rel-14 lv-qty15 ~
lv-rel-15 lv-qty16 lv-rel-16 lv-qty17 lv-rel-17 lv-qty18 lv-rel-18 lv-qty19 ~
lv-rel-19 lv-qty20 lv-rel-20 lv-msf-2 lv-msf-8 lv-msf-7 lv-msf-6 lv-msf-5 ~
lv-msf-4 lv-msf-3 lv-msf-20 lv-msf-9 lv-msf-19 lv-msf-18 lv-msf-17 ~
lv-msf-16 lv-msf-15 lv-msf-14 lv-msf-12 lv-msf-13 lv-msf-11 lv-msf-10 ~
lv-msf-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE lv-msf-1 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-10 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-11 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-12 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-13 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-14 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-15 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-16 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-17 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-18 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-19 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-2 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-20 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-3 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-4 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-5 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-6 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-7 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-8 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-9 AS DECIMAL FORMAT "->>,>>>,>>9.9<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty1 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty10 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty11 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty12 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty13 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty14 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty15 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty16 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty17 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty18 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty19 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty2 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty20 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty3 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty4 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty5 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty6 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty7 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.1.

DEFINE VARIABLE lv-qty8 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty9 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-1 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-10 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-11 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-12 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-13 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-14 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-15 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-16 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-17 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-18 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-19 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-2 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-20 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-3 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-4 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-5 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-6 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-7 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.1.

DEFINE VARIABLE lv-rel-8 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-rel-9 AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 58 BY 23.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     lv-qty1 AT ROW 2.43 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-1 AT ROW 2.43 COL 20 COLON-ALIGNED NO-LABEL
     lv-qty2 AT ROW 3.48 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-2 AT ROW 3.48 COL 20 COLON-ALIGNED NO-LABEL
     lv-qty3 AT ROW 4.48 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-3 AT ROW 4.48 COL 20 COLON-ALIGNED NO-LABEL
     lv-qty4 AT ROW 5.33 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-4 AT ROW 5.33 COL 20 COLON-ALIGNED NO-LABEL
     lv-qty5 AT ROW 6.29 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-5 AT ROW 6.29 COL 20 COLON-ALIGNED NO-LABEL
     lv-qty6 AT ROW 7.29 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-6 AT ROW 7.29 COL 20 COLON-ALIGNED NO-LABEL
     lv-qty7 AT ROW 8.24 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-7 AT ROW 8.24 COL 20 COLON-ALIGNED NO-LABEL
     lv-qty8 AT ROW 9.33 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-8 AT ROW 9.33 COL 20 COLON-ALIGNED NO-LABEL
     lv-qty9 AT ROW 10.33 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-9 AT ROW 10.33 COL 20 COLON-ALIGNED NO-LABEL
     lv-qty10 AT ROW 11.29 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-10 AT ROW 11.29 COL 20 COLON-ALIGNED NO-LABEL
     lv-qty11 AT ROW 12.19 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-11 AT ROW 12.19 COL 20 COLON-ALIGNED NO-LABEL
     lv-qty12 AT ROW 13.14 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-12 AT ROW 13.14 COL 20 COLON-ALIGNED NO-LABEL
     lv-qty13 AT ROW 14.1 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-13 AT ROW 14.1 COL 20 COLON-ALIGNED NO-LABEL
     lv-qty14 AT ROW 15.05 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-14 AT ROW 15.05 COL 20 COLON-ALIGNED NO-LABEL
     lv-qty15 AT ROW 16 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-15 AT ROW 16 COL 20 COLON-ALIGNED NO-LABEL
     lv-qty16 AT ROW 16.95 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-16 AT ROW 16.95 COL 20 COLON-ALIGNED NO-LABEL
     lv-qty17 AT ROW 17.91 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-17 AT ROW 17.91 COL 20 COLON-ALIGNED NO-LABEL
     lv-qty18 AT ROW 18.86 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-18 AT ROW 18.86 COL 20 COLON-ALIGNED NO-LABEL
     lv-qty19 AT ROW 19.81 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-19 AT ROW 19.81 COL 20 COLON-ALIGNED NO-LABEL
     lv-qty20 AT ROW 20.76 COL 1 COLON-ALIGNED NO-LABEL
     lv-rel-20 AT ROW 20.76 COL 20 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 22.43 COL 13
     Btn_Cancel AT ROW 22.43 COL 29
     lv-msf-2 AT ROW 3.48 COL 39 COLON-ALIGNED NO-LABEL
     lv-msf-8 AT ROW 9.33 COL 39 COLON-ALIGNED NO-LABEL
     lv-msf-7 AT ROW 8.24 COL 39 COLON-ALIGNED NO-LABEL
     lv-msf-6 AT ROW 7.29 COL 39 COLON-ALIGNED NO-LABEL
     lv-msf-5 AT ROW 6.29 COL 39 COLON-ALIGNED NO-LABEL
     lv-msf-4 AT ROW 5.33 COL 39 COLON-ALIGNED NO-LABEL
     lv-msf-3 AT ROW 4.48 COL 39 COLON-ALIGNED NO-LABEL
     lv-msf-20 AT ROW 20.76 COL 39 COLON-ALIGNED NO-LABEL
     lv-msf-9 AT ROW 10.29 COL 39 COLON-ALIGNED NO-LABEL
     lv-msf-19 AT ROW 19.81 COL 39 COLON-ALIGNED NO-LABEL
     lv-msf-18 AT ROW 18.86 COL 39 COLON-ALIGNED NO-LABEL
     lv-msf-17 AT ROW 17.91 COL 39 COLON-ALIGNED NO-LABEL
     lv-msf-16 AT ROW 16.95 COL 39 COLON-ALIGNED NO-LABEL
     lv-msf-15 AT ROW 16 COL 39 COLON-ALIGNED NO-LABEL
     lv-msf-14 AT ROW 15.05 COL 39 COLON-ALIGNED NO-LABEL
     lv-msf-12 AT ROW 13.14 COL 39 COLON-ALIGNED NO-LABEL
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     lv-msf-13 AT ROW 14.1 COL 39 COLON-ALIGNED NO-LABEL
     lv-msf-11 AT ROW 12.19 COL 39 COLON-ALIGNED NO-LABEL
     lv-msf-10 AT ROW 11.24 COL 39 COLON-ALIGNED NO-LABEL
     lv-msf-1 AT ROW 2.43 COL 39 COLON-ALIGNED NO-LABEL
     RECT-6 AT ROW 1 COL 1
     "Quantity" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 1.48 COL 6
          FONT 6
     "Qty MSF" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.48 COL 42
          FONT 6
     "Releases" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 1.48 COL 25
          FONT 6
     SPACE(108.99) SKIP(21.99)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Quantity Detail Information"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   Custom                                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN lv-msf-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-10 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-11 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-12 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-13 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-14 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-15 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-16 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-17 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-18 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-19 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-20 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-3 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-4 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-5 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-6 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-7 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-8 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-msf-9 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Quantity Detail Information */
DO:
    op-char-val = "?".
    op-char-val2 = "?".
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
    op-char-val = "?".
    op-char-val2 = "?".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    assign lv-qty1 lv-qty2
           lv-qty3 lv-qty4
           lv-qty5 lv-qty6
           lv-qty7 lv-qty8
           lv-qty9 lv-qty10
           lv-rel-1 lv-rel-2
           lv-rel-3 lv-rel-4
           lv-rel-5 lv-rel-6
           lv-rel-7 lv-rel-8
           lv-rel-9 lv-rel-10.
    assign lv-qty11 lv-qty12
           lv-qty13 lv-qty14
           lv-qty15 lv-qty16
           lv-qty17 lv-qty18
           lv-qty19 lv-qty20
           lv-rel-11 lv-rel-12
           lv-rel-13 lv-rel-14
           lv-rel-15 lv-rel-16
           lv-rel-17 lv-rel-18
           lv-rel-19 lv-rel-20.
  
    op-char-val = string(lv-qty1) + "," + string(lv-qty2) + "," +
                  string(lv-qty3) + "," + string(lv-qty4) + "," +
                  string(lv-qty5) + "," + string(lv-qty6) + "," +
                  string(lv-qty7) + "," + string(lv-qty8) + "," +
                  string(lv-qty9) + "," + string(lv-qty10) + "," +
                  string(lv-rel-1) + "," + string(lv-rel-2) + "," +
                  string(lv-rel-3) + "," + string(lv-rel-4) + "," +
                  string(lv-rel-5) + "," + string(lv-rel-6) + "," +
                  string(lv-rel-7) + "," + string(lv-rel-8) + "," +
                  string(lv-rel-9) + "," + string(lv-rel-10)
                  .

    op-date-val = "".
  
    op-char-val2 = string(lv-qty11) + "," + string(lv-qty12) + "," +
                  string(lv-qty13) + "," + string(lv-qty14) + "," +
                  string(lv-qty15) + "," + string(lv-qty16) + "," +
                  string(lv-qty17) + "," + string(lv-qty18) + "," +
                  string(lv-qty19) + "," + string(lv-qty20) + "," +
                  string(lv-rel-11) + "," + string(lv-rel-12) + "," +
                  string(lv-rel-13) + "," + string(lv-rel-14) + "," +
                  string(lv-rel-15) + "," + string(lv-rel-16) + "," +
                  string(lv-rel-17) + "," + string(lv-rel-18) + "," +
                  string(lv-rel-19) + "," + string(lv-rel-20).
                  
   op-date-val2 = "".
                  
   find est-qty where recid(est-qty) = ip-recid no-error.
   assign est-qty.qty[1] = lv-qty1
          est-qty.qty[2] = lv-qty2
          est-qty.qty[3] = lv-qty3
          est-qty.qty[4] = lv-qty4
          est-qty.qty[5] = lv-qty5
          est-qty.qty[6] = lv-qty6
          est-qty.qty[7] = lv-qty7
          est-qty.qty[8] = lv-qty8
          est-qty.qty[9] = lv-qty9
          est-qty.qty[10] = lv-qty10
          est-qty.qty[11] = lv-qty11
          est-qty.qty[12] = lv-qty12
          est-qty.qty[13] = lv-qty13
          est-qty.qty[14] = lv-qty14
          est-qty.qty[15] = lv-qty15
          est-qty.qty[16] = lv-qty16
          est-qty.qty[17] = lv-qty17
          est-qty.qty[18] = lv-qty18
          est-qty.qty[19] = lv-qty19
          est-qty.qty[20] = lv-qty20
          .
   assign est-qty.qty[21] = lv-rel-1
          est-qty.qty[22] = lv-rel-2
          est-qty.qty[23] = lv-rel-3
          est-qty.qty[24] = lv-rel-4
          est-qty.qty[25] = lv-rel-5
          est-qty.qty[26] = lv-rel-6
          est-qty.qty[27] = lv-rel-7
          est-qty.qty[28] = lv-rel-8
          est-qty.qty[29] = lv-rel-9
          est-qty.qty[30] = lv-rel-10
          est-qty.qty[31] = lv-rel-11
          est-qty.qty[32] = lv-rel-12
          est-qty.qty[33] = lv-rel-13
          est-qty.qty[34] = lv-rel-14
          est-qty.qty[35] = lv-rel-15
          est-qty.qty[36] = lv-rel-16
          est-qty.qty[37] = lv-rel-17
          est-qty.qty[38] = lv-rel-18
          est-qty.qty[39] = lv-rel-19
          est-qty.qty[40] = lv-rel-20
          .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty1 Dialog-Frame
ON VALUE-CHANGED OF lv-qty1 IN FRAME Dialog-Frame
DO:
  RUN get-msf.
  lv-msf-1:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty10 Dialog-Frame
ON VALUE-CHANGED OF lv-qty10 IN FRAME Dialog-Frame
DO:
  RUN get-msf.
  lv-msf-10:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty11 Dialog-Frame
ON VALUE-CHANGED OF lv-qty11 IN FRAME Dialog-Frame
DO:
  RUN get-msf.
  lv-msf-11:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty12 Dialog-Frame
ON VALUE-CHANGED OF lv-qty12 IN FRAME Dialog-Frame
DO:
  RUN get-msf.
  lv-msf-12:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty13 Dialog-Frame
ON VALUE-CHANGED OF lv-qty13 IN FRAME Dialog-Frame
DO:
  RUN get-msf.
  lv-msf-13:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty14 Dialog-Frame
ON VALUE-CHANGED OF lv-qty14 IN FRAME Dialog-Frame
DO:
  RUN get-msf.
  lv-msf-14:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty15 Dialog-Frame
ON VALUE-CHANGED OF lv-qty15 IN FRAME Dialog-Frame
DO:
  RUN get-msf.
  lv-msf-15:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty16 Dialog-Frame
ON VALUE-CHANGED OF lv-qty16 IN FRAME Dialog-Frame
DO:
  RUN get-msf.
  lv-msf-16:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty17
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty17 Dialog-Frame
ON VALUE-CHANGED OF lv-qty17 IN FRAME Dialog-Frame
DO:
  RUN get-msf.
  lv-msf-17:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty18 Dialog-Frame
ON VALUE-CHANGED OF lv-qty18 IN FRAME Dialog-Frame
DO:
  RUN get-msf.
  lv-msf-18:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty19 Dialog-Frame
ON VALUE-CHANGED OF lv-qty19 IN FRAME Dialog-Frame
DO:
  RUN get-msf.
  lv-msf-19:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty2 Dialog-Frame
ON VALUE-CHANGED OF lv-qty2 IN FRAME Dialog-Frame
DO:
  RUN get-msf.
  lv-msf-2:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty20 Dialog-Frame
ON VALUE-CHANGED OF lv-qty20 IN FRAME Dialog-Frame
DO:
  RUN get-msf.
  lv-msf-20:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty3 Dialog-Frame
ON VALUE-CHANGED OF lv-qty3 IN FRAME Dialog-Frame
DO:
  RUN get-msf.
  lv-msf-3:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty4 Dialog-Frame
ON VALUE-CHANGED OF lv-qty4 IN FRAME Dialog-Frame
DO:
  RUN get-msf.
  lv-msf-4:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty5 Dialog-Frame
ON VALUE-CHANGED OF lv-qty5 IN FRAME Dialog-Frame
DO:
  RUN get-msf.
  lv-msf-5:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty6 Dialog-Frame
ON VALUE-CHANGED OF lv-qty6 IN FRAME Dialog-Frame
DO:
  RUN get-msf.
  lv-msf-6:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty7 Dialog-Frame
ON VALUE-CHANGED OF lv-qty7 IN FRAME Dialog-Frame
DO:
  RUN get-msf.
  lv-msf-7:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty8 Dialog-Frame
ON VALUE-CHANGED OF lv-qty8 IN FRAME Dialog-Frame
DO:
  RUN get-msf.
  lv-msf-8:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty9 Dialog-Frame
ON VALUE-CHANGED OF lv-qty9 IN FRAME Dialog-Frame
DO:
  RUN get-msf.
  lv-msf-9:SCREEN-VALUE = STRING(ld-msf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   DEF VAR lv-yld AS DEC NO-UNDO.

   find est-qty where recid(est-qty) = ip-recid no-lock no-error.
   if avail est-qty then do:
      find est where est.company = est-qty.company and
                     est.est-no = est-qty.est-no
                     no-lock no-error.
                     
      ASSIGN lv-qty2 = est-qty.qty[2]
             lv-qty3 = est-qty.qty[3]
             lv-qty4 = est-qty.qty[4]
             lv-qty5 = est-qty.qty[5]
             lv-qty6 = est-qty.qty[6]
             lv-qty7 = est-qty.qty[7]
             lv-qty8 = est-qty.qty[8]
             lv-qty9 = est-qty.qty[9]
             lv-qty10 = est-qty.qty[10]
             lv-qty11 = est-qty.qty[11]
             lv-qty12 = est-qty.qty[12]
             lv-qty13 = est-qty.qty[13]
             lv-qty14 = est-qty.qty[14]
             lv-qty15 = est-qty.qty[15]
             lv-qty16 = est-qty.qty[16]
             lv-qty17 = est-qty.qty[17]
             lv-qty18 = est-qty.qty[18]
             lv-qty19 = est-qty.qty[19]
             lv-qty20 = est-qty.qty[20].
                     
      ASSIGN lv-rel-1  = est-qty.qty[21]
             lv-rel-2  = est-qty.qty[22]
             lv-rel-3  = est-qty.qty[23]
             lv-rel-4  = est-qty.qty[24]
             lv-rel-5  = est-qty.qty[25]
             lv-rel-6  = est-qty.qty[26]
             lv-rel-7  = est-qty.qty[27]
             lv-rel-8  = est-qty.qty[28]
             lv-rel-9  = est-qty.qty[29]
             lv-rel-10 = est-qty.qty[30]
             lv-rel-11 = est-qty.qty[31]
             lv-rel-12 = est-qty.qty[32]
             lv-rel-13 = est-qty.qty[33]
             lv-rel-14 = est-qty.qty[34]
             lv-rel-15 = est-qty.qty[35]
             lv-rel-16 = est-qty.qty[36]
             lv-rel-17 = est-qty.qty[37]
             lv-rel-18 = est-qty.qty[38]
             lv-rel-19 = est-qty.qty[39]
             lv-rel-20 = est-qty.qty[40].
   end.

   find eb where recid(eb) = ip-eb-recid no-lock no-error.

   ASSIGN
    lv-qty1 = INT(ip-qty)
    lv-yld  = IF est.est-type LE 4 OR est.est-type EQ 8 THEN 1 ELSE
              (IF eb.yld-qty < 0 THEN -1 / eb.yld-qty ELSE eb.yld-qty).

   if lv-qty1 > 0 then lv-msf-1 = if v-corr then( (lv-qty1 * eb.t-len * eb.t-wid * .007)
                                                 * lv-yld
                                                 / 1000 )
                                            else ( (lv-qty1 * eb.t-len * eb.t-wid / 144)
                                                 * lv-yld
                                                  / 1000 )
                                            .

   do i = 2 to 20:
      if est-qty.qty[i] > 0 then
      case i :
           when 2 then lv-msf-2 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * lv-yld
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * lv-yld
                                                  / 1000 )
                                            .
           
           when 3 then lv-msf-3 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * lv-yld
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * lv-yld
                                                  / 1000 )
                                            .
           when 4 then lv-msf-4 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * lv-yld
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * lv-yld
                                                  / 1000 )
                                            .
           when 5 then lv-msf-5 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * lv-yld
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * lv-yld
                                                  / 1000 )
                                            .
           when 6 then lv-msf-6 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * lv-yld
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * lv-yld
                                                  / 1000 )
                                            .
           when 7 then lv-msf-7 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * lv-yld
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * lv-yld
                                                  / 1000 )
                                            .
           when 8 then lv-msf-8 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * lv-yld
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * lv-yld
                                                  / 1000 )
                                            .
           when 9 then lv-msf-9 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * lv-yld
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * lv-yld
                                                  / 1000 )
                                            .
           when 10 then lv-msf-10 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * lv-yld
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * lv-yld
                                                  / 1000 )
                                            .
           when 11 then lv-msf-11 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * lv-yld
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * lv-yld
                                                  / 1000 )
                                            .
           when 12 then lv-msf-12 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * lv-yld
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * lv-yld
                                                  / 1000 )
                                            .
           when 13 then lv-msf-13 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * lv-yld
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * lv-yld
                                                  / 1000 )
                                            .
           when 14 then lv-msf-14 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * lv-yld
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * lv-yld
                                                  / 1000 )
                                            .
           when 15 then lv-msf-15 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * lv-yld
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * lv-yld
                                                  / 1000 )
                                            .
           when 16 then lv-msf-16 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * lv-yld
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * lv-yld
                                                  / 1000 )
                                            .
           when 17 then lv-msf-17 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * lv-yld
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * lv-yld
                                                  / 1000 )
                                            .
           when 18 then lv-msf-18 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * lv-yld
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * lv-yld
                                                  / 1000 )
                                            .
           when 19 then lv-msf-19 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * lv-yld
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * lv-yld
                                                  / 1000 )
                                            .
           when 20 then lv-msf-20 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * lv-yld
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * lv-yld
                                                  / 1000 )
                                            .

      end case.
   end.
   RUN enable_UI. 

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY lv-qty1 lv-rel-1 lv-qty2 lv-rel-2 lv-qty3 lv-rel-3 lv-qty4 lv-rel-4 
          lv-qty5 lv-rel-5 lv-qty6 lv-rel-6 lv-qty7 lv-rel-7 lv-qty8 lv-rel-8 
          lv-qty9 lv-rel-9 lv-qty10 lv-rel-10 lv-qty11 lv-rel-11 lv-qty12 
          lv-rel-12 lv-qty13 lv-rel-13 lv-qty14 lv-rel-14 lv-qty15 lv-rel-15 
          lv-qty16 lv-rel-16 lv-qty17 lv-rel-17 lv-qty18 lv-rel-18 lv-qty19 
          lv-rel-19 lv-qty20 lv-rel-20 lv-msf-2 lv-msf-8 lv-msf-7 lv-msf-6 
          lv-msf-5 lv-msf-4 lv-msf-3 lv-msf-20 lv-msf-9 lv-msf-19 lv-msf-18 
          lv-msf-17 lv-msf-16 lv-msf-15 lv-msf-14 lv-msf-12 lv-msf-13 lv-msf-11 
          lv-msf-10 lv-msf-1 
      WITH FRAME Dialog-Frame.
  ENABLE lv-qty1 lv-rel-1 lv-qty2 lv-rel-2 lv-qty3 lv-rel-3 lv-qty4 lv-rel-4 
         lv-qty5 lv-rel-5 lv-qty6 lv-rel-6 lv-qty7 lv-rel-7 lv-qty8 lv-rel-8 
         lv-qty9 lv-rel-9 lv-qty10 lv-rel-10 lv-qty11 lv-rel-11 lv-qty12 
         lv-rel-12 lv-qty13 lv-rel-13 lv-qty14 lv-rel-14 lv-qty15 lv-rel-15 
         lv-qty16 lv-rel-16 lv-qty17 lv-rel-17 lv-qty18 lv-rel-18 lv-qty19 
         lv-rel-19 lv-qty20 lv-rel-20 Btn_OK Btn_Cancel RECT-6 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-msf Dialog-Frame 
PROCEDURE get-msf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     ld-msf = DEC(FOCUS:SCREEN-VALUE)
     ld-msf = (IF v-corr THEN (ld-msf * eb.t-len * eb.t-wid * .007)
                     ELSE (ld-msf * eb.t-len * eb.t-wid / 144)) *
          (IF eb.est-type EQ 2 THEN
             (IF eb.cust-% LT 0 THEN (-1 / eb.cust-%) ELSE eb.cust-%)
           ELSE
           IF eb.est-type EQ 6 THEN
             (IF eb.yld-qty LT 0 THEN -1 / (eb.yld-qty) ELSE eb.yld-qty)
           ELSE 1) / 1000.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

