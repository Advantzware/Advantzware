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
&Scoped-Define ENABLED-OBJECTS lv-qty1 lv-qty2 lv-qty3 lv-qty4 lv-qty5 ~
lv-qty6 lv-qty7 lv-qty8 lv-qty9 lv-qty10 lv-qty11 lv-qty12 lv-qty13 ~
lv-qty14 lv-qty15 lv-qty16 lv-qty17 lv-qty18 lv-qty19 lv-qty20 Btn_OK ~
Btn_Cancel lv-yqty-1 lv-yqty-12 lv-yqty-2 lv-yqty-3 lv-yqty-4 lv-yqty-5 ~
lv-yqty-6 lv-yqty-7 lv-yqty-8 lv-yqty-9 lv-yqty-10 lv-yqty-11 lv-yqty-13 ~
lv-yqty-14 lv-yqty-15 lv-yqty-16 lv-yqty-17 lv-yqty-18 lv-yqty-19 ~
lv-yqty-20 RECT-6 
&Scoped-Define DISPLAYED-OBJECTS lv-qty1 lv-qty2 lv-qty3 lv-qty4 lv-qty5 ~
lv-qty6 lv-qty7 lv-qty8 lv-qty9 lv-qty10 lv-qty11 lv-qty12 lv-qty13 ~
lv-qty14 lv-qty15 lv-qty16 lv-qty17 lv-qty18 lv-qty19 lv-qty20 lv-msf-2 ~
lv-msf-8 lv-msf-7 lv-msf-6 lv-msf-5 lv-msf-4 lv-msf-3 lv-msf-20 lv-msf-9 ~
lv-msf-19 lv-msf-18 lv-msf-17 lv-msf-16 lv-msf-15 lv-msf-14 lv-msf-12 ~
lv-msf-13 lv-msf-11 lv-msf-10 lv-msf-1 lv-yqty-1 lv-yqty-12 lv-yqty-2 ~
lv-yqty-3 lv-yqty-4 lv-yqty-5 lv-yqty-6 lv-yqty-7 lv-yqty-8 lv-yqty-9 ~
lv-yqty-10 lv-yqty-11 lv-yqty-13 lv-yqty-14 lv-yqty-15 lv-yqty-16 ~
lv-yqty-17 lv-yqty-18 lv-yqty-19 lv-yqty-20 

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

DEFINE VARIABLE lv-msf-1 AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-10 AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-11 AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-12 AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-13 AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-14 AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-15 AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-16 AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-17 AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-18 AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-19 AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-2 AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-20 AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-3 AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-4 AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-5 AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-6 AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-7 AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-8 AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-msf-9 AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty1 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty10 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty11 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty12 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty13 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty14 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty15 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty16 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty17 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty18 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty19 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty2 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty20 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty3 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty4 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty5 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty6 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty7 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.1.

DEFINE VARIABLE lv-qty8 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-qty9 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-yqty-1 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-yqty-10 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-yqty-11 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-yqty-12 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-yqty-13 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-yqty-14 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-yqty-15 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-yqty-16 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-yqty-17 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-yqty-18 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-yqty-19 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-yqty-2 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-yqty-20 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-yqty-3 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-yqty-4 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-yqty-5 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-yqty-6 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-yqty-7 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-yqty-8 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE lv-yqty-9 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 144 BY 23.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     lv-qty1 AT ROW 2.43 COL 1 COLON-ALIGNED NO-LABEL
     lv-qty2 AT ROW 3.48 COL 1 COLON-ALIGNED NO-LABEL
     lv-qty3 AT ROW 4.48 COL 1 COLON-ALIGNED NO-LABEL
     lv-qty4 AT ROW 5.33 COL 1 COLON-ALIGNED NO-LABEL
     lv-qty5 AT ROW 6.29 COL 1 COLON-ALIGNED NO-LABEL
     lv-qty6 AT ROW 7.29 COL 1 COLON-ALIGNED NO-LABEL
     lv-qty7 AT ROW 8.24 COL 1 COLON-ALIGNED NO-LABEL
     lv-qty8 AT ROW 9.33 COL 1 COLON-ALIGNED NO-LABEL
     lv-qty9 AT ROW 10.33 COL 1 COLON-ALIGNED NO-LABEL
     lv-qty10 AT ROW 11.29 COL 1 COLON-ALIGNED NO-LABEL
     lv-qty11 AT ROW 12.19 COL 1 COLON-ALIGNED NO-LABEL
     lv-qty12 AT ROW 13.14 COL 1 COLON-ALIGNED NO-LABEL
     lv-qty13 AT ROW 14.1 COL 1 COLON-ALIGNED NO-LABEL
     lv-qty14 AT ROW 15.05 COL 1 COLON-ALIGNED NO-LABEL
     lv-qty15 AT ROW 16 COL 1 COLON-ALIGNED NO-LABEL
     lv-qty16 AT ROW 16.95 COL 1 COLON-ALIGNED NO-LABEL
     lv-qty17 AT ROW 17.91 COL 1 COLON-ALIGNED NO-LABEL
     lv-qty18 AT ROW 18.86 COL 1 COLON-ALIGNED NO-LABEL
     lv-qty19 AT ROW 19.81 COL 1 COLON-ALIGNED NO-LABEL
     lv-qty20 AT ROW 20.76 COL 1 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 22.43 COL 4
     Btn_Cancel AT ROW 22.43 COL 22
     lv-msf-2 AT ROW 3.48 COL 20 COLON-ALIGNED NO-LABEL
     lv-msf-8 AT ROW 9.33 COL 20 COLON-ALIGNED NO-LABEL
     lv-msf-7 AT ROW 8.24 COL 20 COLON-ALIGNED NO-LABEL
     lv-msf-6 AT ROW 7.29 COL 20 COLON-ALIGNED NO-LABEL
     lv-msf-5 AT ROW 6.29 COL 20 COLON-ALIGNED NO-LABEL
     lv-msf-4 AT ROW 5.33 COL 20 COLON-ALIGNED NO-LABEL
     lv-msf-3 AT ROW 4.48 COL 20 COLON-ALIGNED NO-LABEL
     lv-msf-20 AT ROW 20.76 COL 20 COLON-ALIGNED NO-LABEL
     lv-msf-9 AT ROW 10.29 COL 20 COLON-ALIGNED NO-LABEL
     lv-msf-19 AT ROW 19.81 COL 20 COLON-ALIGNED NO-LABEL
     lv-msf-18 AT ROW 18.86 COL 20 COLON-ALIGNED NO-LABEL
     lv-msf-17 AT ROW 17.91 COL 20 COLON-ALIGNED NO-LABEL
     lv-msf-16 AT ROW 16.95 COL 20 COLON-ALIGNED NO-LABEL
     lv-msf-15 AT ROW 16 COL 20 COLON-ALIGNED NO-LABEL
     lv-msf-14 AT ROW 15.05 COL 20 COLON-ALIGNED NO-LABEL
     lv-msf-12 AT ROW 13.14 COL 20 COLON-ALIGNED NO-LABEL
     lv-msf-13 AT ROW 14.1 COL 20 COLON-ALIGNED NO-LABEL
     lv-msf-11 AT ROW 12.19 COL 20 COLON-ALIGNED NO-LABEL
     lv-msf-10 AT ROW 11.24 COL 20 COLON-ALIGNED NO-LABEL
     lv-msf-1 AT ROW 2.43 COL 20 COLON-ALIGNED NO-LABEL
     lv-yqty-1 AT ROW 2.43 COL 39 COLON-ALIGNED NO-LABEL
     lv-yqty-12 AT ROW 13.14 COL 39 COLON-ALIGNED NO-LABEL
     lv-yqty-2 AT ROW 3.62 COL 39 COLON-ALIGNED NO-LABEL
     lv-yqty-3 AT ROW 4.57 COL 39 COLON-ALIGNED NO-LABEL
     lv-yqty-4 AT ROW 5.52 COL 39 COLON-ALIGNED NO-LABEL
     lv-yqty-5 AT ROW 6.48 COL 39 COLON-ALIGNED NO-LABEL
     lv-yqty-6 AT ROW 7.43 COL 39 COLON-ALIGNED NO-LABEL
     lv-yqty-7 AT ROW 8.38 COL 39 COLON-ALIGNED NO-LABEL
     lv-yqty-8 AT ROW 9.33 COL 39 COLON-ALIGNED NO-LABEL
     lv-yqty-9 AT ROW 10.29 COL 39 COLON-ALIGNED NO-LABEL
     lv-yqty-10 AT ROW 11.24 COL 39 COLON-ALIGNED NO-LABEL
     lv-yqty-11 AT ROW 12.19 COL 39 COLON-ALIGNED NO-LABEL
     lv-yqty-13 AT ROW 14.1 COL 39 COLON-ALIGNED NO-LABEL
     lv-yqty-14 AT ROW 15.05 COL 39 COLON-ALIGNED NO-LABEL
     lv-yqty-15 AT ROW 16 COL 39 COLON-ALIGNED NO-LABEL
     lv-yqty-16 AT ROW 16.95 COL 39 COLON-ALIGNED NO-LABEL
.
/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     lv-yqty-17 AT ROW 17.91 COL 39 COLON-ALIGNED NO-LABEL
     lv-yqty-18 AT ROW 18.86 COL 39 COLON-ALIGNED NO-LABEL
     lv-yqty-19 AT ROW 19.81 COL 39 COLON-ALIGNED NO-LABEL
     lv-yqty-20 AT ROW 20.76 COL 39 COLON-ALIGNED NO-LABEL
     RECT-6 AT ROW 1 COL 1
     "Req. Quantity" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 1.48 COL 3
          FONT 6
     "Qty MSF" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.48 COL 23
          FONT 6
     "Yield  Quantity" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 1.48 COL 40
          FONT 6
     SPACE(88.99) SKIP(22.00)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Quantity Detail Information"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
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
      /*     lv-price-1 lv-price-2
           lv-price-3 lv-price-4
           lv-price-5 lv-price-6
           lv-price-7 lv-price-8
           lv-price-9 lv-price-10
           lv-uom-1 lv-uom-2
           lv-uom-3 lv-uom-4
           lv-uom-5 lv-uom-6           
           lv-uom-7 lv-uom-8
           lv-uom-9 lv-uom-10
           lv-date-1 lv-date-2   
           lv-date-3 lv-date-4   
           lv-date-5 lv-date-6   
           lv-date-7 lv-date-8   
           lv-date-9 lv-date-10   */
           .
    assign lv-qty11 lv-qty12
           lv-qty13 lv-qty14
           lv-qty15 lv-qty16
           lv-qty17 lv-qty18
           lv-qty19 lv-qty20
/*           lv-price-11 lv-price-12
           lv-price-13 lv-price-14
           lv-price-15 lv-price-16
           lv-price-17 lv-price-18
           lv-price-19 lv-price-20
           lv-uom-11 lv-uom-12
           lv-uom-13 lv-uom-14
           lv-uom-15 lv-uom-16           
           lv-uom-17 lv-uom-18
           lv-uom-19 lv-uom-20
           lv-date-11 lv-date-12   
           lv-date-13 lv-date-14   
           lv-date-15 lv-date-16   
           lv-date-17 lv-date-18   
           lv-date-19 lv-date-20                */
           . 
  
           
    op-char-val = string(lv-qty1) + "," + string(lv-qty2) + "," +
                  string(lv-qty3) + "," + string(lv-qty4) + "," +
                  string(lv-qty5) + "," + string(lv-qty6) + "," +
                  string(lv-qty7) + "," + string(lv-qty8) + "," +
                  string(lv-qty9) + "," + string(lv-qty10) 
       /*           + "," +
                  string(lv-price-1) + "," + string(lv-price-2) + "," +
                  string(lv-price-3) + "," + string(lv-price-4) + "," +
                  string(lv-price-5) + "," + string(lv-price-6) + "," +
                  string(lv-price-7) + "," + string(lv-price-8) + "," +
                  string(lv-price-9) + "," + string(lv-price-10) + "," +
                  string(lv-uom-1) + "," + string(lv-uom-2) + "," +
                  string(lv-uom-3) + "," + string(lv-uom-4) + "," +
                  string(lv-uom-5) + "," + string(lv-uom-6) + "," +
                  string(lv-uom-7) + "," + string(lv-uom-8) + "," +
                  string(lv-uom-9) + "," + string(lv-uom-10)*/
                  .

    op-date-val = /* string(lv-date-1,"99/99/9999") + "," + string(lv-date-2,"99/99.9999") + "," +
                  string(lv-date-3,"99/99/9999") + "," + string(lv-date-4,"99/99.9999") + "," +
                  string(lv-date-5,"99/99/9999") + "," + string(lv-date-6,"99/99.9999") + "," +
                  string(lv-date-7,"99/99/9999") + "," + string(lv-date-8,"99/99.9999") + "," +
                  string(lv-date-9,"99/99/9999") + "," + string(lv-date-10,"99/99.9999")                                    
                  */
                  "".
  
    op-char-val2 = string(lv-qty11) + "," + string(lv-qty12) + "," +
                  string(lv-qty13) + "," + string(lv-qty14) + "," +
                  string(lv-qty15) + "," + string(lv-qty16) + "," +
                  string(lv-qty17) + "," + string(lv-qty18) + "," +
                  string(lv-qty19) + "," + string(lv-qty20) 
                 /* + "," +
                  string(lv-price-11) + "," + string(lv-price-12) + "," +
                  string(lv-price-13) + "," + string(lv-price-14) + "," +
                  string(lv-price-15) + "," + string(lv-price-16) + "," +
                  string(lv-price-17) + "," + string(lv-price-18) + "," +
                  string(lv-price-19) + "," + string(lv-price-20) + "," +
                  string(lv-uom-11) + "," + string(lv-uom-12) + "," +
                  string(lv-uom-13) + "," + string(lv-uom-14) + "," +
                  string(lv-uom-15) + "," + string(lv-uom-16) + "," +
                  string(lv-uom-17) + "," + string(lv-uom-18) + "," +
                  string(lv-uom-19) + "," + string(lv-uom-20) */.
                  
   op-date-val2 = /*string(lv-date-11,"99/99/9999") + "," + string(lv-date-12,"99/99.9999") + "," +
                  string(lv-date-13,"99/99/9999") + "," + string(lv-date-14,"99/99.9999") + "," +
                  string(lv-date-15,"99/99/9999") + "," + string(lv-date-16,"99/99.9999") + "," +
                  string(lv-date-17,"99/99/9999") + "," + string(lv-date-18,"99/99.9999") + "," +
                  string(lv-date-19,"99/99/9999") + "," + string(lv-date-20,"99/99.9999")                   
                  */
                  "".
                  
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

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty1 Dialog-Frame
ON LEAVE OF lv-qty1 IN FRAME Dialog-Frame
DO:
  assign lv-qty1.
  if lv-qty1 > 0 then lv-msf-1 = if v-corr then( (lv-qty1 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty1 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 )
                                            .
  display lv-msf-1 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty10 Dialog-Frame
ON LEAVE OF lv-qty10 IN FRAME Dialog-Frame
DO:
  assign lv-qty10.
  if lv-qty10 > 0 then lv-msf-10 = if v-corr then( (lv-qty10 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty10 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-10 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty11 Dialog-Frame
ON LEAVE OF lv-qty11 IN FRAME Dialog-Frame
DO:
    assign lv-qty11.
  if lv-qty11 > 0 then lv-msf-11 = if v-corr then( (lv-qty11 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty11 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-11 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty12 Dialog-Frame
ON LEAVE OF lv-qty12 IN FRAME Dialog-Frame
DO:
    assign lv-qty12.
  if lv-qty12 > 0 then lv-msf-12 = if v-corr then( (lv-qty12 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty12 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-12 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty13 Dialog-Frame
ON LEAVE OF lv-qty13 IN FRAME Dialog-Frame
DO:
    assign lv-qty13.
  if lv-qty13 > 0 then lv-msf-13 = if v-corr then( (lv-qty13 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty13 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-13 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty14 Dialog-Frame
ON LEAVE OF lv-qty14 IN FRAME Dialog-Frame
DO:
    assign lv-qty14.
  if lv-qty14 > 0 then lv-msf-14 = if v-corr then( (lv-qty14 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty14 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-14 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty15 Dialog-Frame
ON LEAVE OF lv-qty15 IN FRAME Dialog-Frame
DO:
    assign lv-qty15.
  if lv-qty15 > 0 then lv-msf-15 = if v-corr then( (lv-qty15 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty15 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
.
  display lv-msf-15 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty16 Dialog-Frame
ON LEAVE OF lv-qty16 IN FRAME Dialog-Frame
DO:
    assign lv-qty16.
  if lv-qty16 > 0 then lv-msf-16 = if v-corr then( (lv-qty16 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty16 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-16 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty17
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty17 Dialog-Frame
ON LEAVE OF lv-qty17 IN FRAME Dialog-Frame
DO:
    assign lv-qty17.
  if lv-qty17 > 0 then lv-msf-17 =if v-corr then( (lv-qty17 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty17 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-17 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty18 Dialog-Frame
ON LEAVE OF lv-qty18 IN FRAME Dialog-Frame
DO:
    assign lv-qty18.
  if lv-qty18 > 0 then lv-msf-18 = if v-corr then( (lv-qty18 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty18 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-18 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty19 Dialog-Frame
ON LEAVE OF lv-qty19 IN FRAME Dialog-Frame
DO:
    assign lv-qty19.
  if lv-qty19 > 0 then lv-msf-19 = if v-corr then( (lv-qty19 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty19 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-19 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty2 Dialog-Frame
ON LEAVE OF lv-qty2 IN FRAME Dialog-Frame
DO:
    assign lv-qty1.
  if lv-qty1 > 0 then lv-msf-2 = if v-corr then( (lv-qty2 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty2 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-2 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty20 Dialog-Frame
ON LEAVE OF lv-qty20 IN FRAME Dialog-Frame
DO:
    assign lv-qty20.
  if lv-qty20 > 0 then lv-msf-20 = if v-corr then( (lv-qty20 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty20 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-20 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty3 Dialog-Frame
ON LEAVE OF lv-qty3 IN FRAME Dialog-Frame
DO:
    assign lv-qty3.
  if lv-qty3 > 0 then lv-msf-3 = if v-corr then( (lv-qty3 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty3 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-3 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty4 Dialog-Frame
ON LEAVE OF lv-qty4 IN FRAME Dialog-Frame
DO:
    assign lv-qty4.
  if lv-qty4 > 0 then lv-msf-4 = if v-corr then( (lv-qty4 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty4 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-4 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty5 Dialog-Frame
ON LEAVE OF lv-qty5 IN FRAME Dialog-Frame
DO:
    assign lv-qty5.
  if lv-qty5 > 0 then lv-msf-5 = if v-corr then( (lv-qty5 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty5 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-5 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty6 Dialog-Frame
ON LEAVE OF lv-qty6 IN FRAME Dialog-Frame
DO:
    assign lv-qty6.
  if lv-qty6 > 0 then lv-msf-6 = if v-corr then( (lv-qty6 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty6 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-6 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty7 Dialog-Frame
ON LEAVE OF lv-qty7 IN FRAME Dialog-Frame
DO:
    assign lv-qty7.
  if lv-qty7 > 0 then lv-msf-7 = if v-corr then( (lv-qty7 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty7 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-7 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty8 Dialog-Frame
ON LEAVE OF lv-qty8 IN FRAME Dialog-Frame
DO:
    assign lv-qty8.
  if lv-qty8 > 0 then lv-msf-8 = if v-corr then( (lv-qty8 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty8 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-8 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty9 Dialog-Frame
ON LEAVE OF lv-qty9 IN FRAME Dialog-Frame
DO:
    assign lv-qty9.
  if lv-qty9 > 0 then lv-msf-9 = if v-corr then( (lv-qty9 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty9 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-9 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-yqty-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-yqty-1 Dialog-Frame
ON LEAVE OF lv-yqty-1 IN FRAME Dialog-Frame
DO:
  assign lv-qty1.
  if lv-qty1 > 0 then lv-msf-1 = if v-corr then( (lv-qty1 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty1 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 )
                                            .
  display lv-msf-1 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-yqty-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-yqty-10 Dialog-Frame
ON LEAVE OF lv-yqty-10 IN FRAME Dialog-Frame
DO:
    assign lv-qty1.
  if lv-qty1 > 0 then lv-msf-2 = if v-corr then( (lv-qty2 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty2 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-2 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-yqty-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-yqty-11 Dialog-Frame
ON LEAVE OF lv-yqty-11 IN FRAME Dialog-Frame
DO:
    assign lv-qty1.
  if lv-qty1 > 0 then lv-msf-2 = if v-corr then( (lv-qty2 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty2 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-2 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-yqty-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-yqty-12 Dialog-Frame
ON LEAVE OF lv-yqty-12 IN FRAME Dialog-Frame
DO:
    assign lv-qty1.
  if lv-qty1 > 0 then lv-msf-2 = if v-corr then( (lv-qty2 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty2 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-2 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-yqty-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-yqty-13 Dialog-Frame
ON LEAVE OF lv-yqty-13 IN FRAME Dialog-Frame
DO:
    assign lv-qty1.
  if lv-qty1 > 0 then lv-msf-2 = if v-corr then( (lv-qty2 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty2 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-2 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-yqty-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-yqty-14 Dialog-Frame
ON LEAVE OF lv-yqty-14 IN FRAME Dialog-Frame
DO:
    assign lv-qty1.
  if lv-qty1 > 0 then lv-msf-2 = if v-corr then( (lv-qty2 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty2 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-2 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-yqty-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-yqty-15 Dialog-Frame
ON LEAVE OF lv-yqty-15 IN FRAME Dialog-Frame
DO:
    assign lv-qty1.
  if lv-qty1 > 0 then lv-msf-2 = if v-corr then( (lv-qty2 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty2 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-2 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-yqty-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-yqty-16 Dialog-Frame
ON LEAVE OF lv-yqty-16 IN FRAME Dialog-Frame
DO:
    assign lv-qty1.
  if lv-qty1 > 0 then lv-msf-2 = if v-corr then( (lv-qty2 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty2 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-2 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-yqty-17
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-yqty-17 Dialog-Frame
ON LEAVE OF lv-yqty-17 IN FRAME Dialog-Frame
DO:
    assign lv-qty1.
  if lv-qty1 > 0 then lv-msf-2 = if v-corr then( (lv-qty2 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty2 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-2 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-yqty-18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-yqty-18 Dialog-Frame
ON LEAVE OF lv-yqty-18 IN FRAME Dialog-Frame
DO:
    assign lv-qty1.
  if lv-qty1 > 0 then lv-msf-2 = if v-corr then( (lv-qty2 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty2 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-2 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-yqty-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-yqty-19 Dialog-Frame
ON LEAVE OF lv-yqty-19 IN FRAME Dialog-Frame
DO:
    assign lv-qty1.
  if lv-qty1 > 0 then lv-msf-2 = if v-corr then( (lv-qty2 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty2 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-2 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-yqty-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-yqty-2 Dialog-Frame
ON LEAVE OF lv-yqty-2 IN FRAME Dialog-Frame
DO:
    assign lv-qty1.
  if lv-qty1 > 0 then lv-msf-2 = if v-corr then( (lv-qty2 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty2 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-2 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-yqty-20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-yqty-20 Dialog-Frame
ON LEAVE OF lv-yqty-20 IN FRAME Dialog-Frame
DO:
    assign lv-qty1.
  if lv-qty1 > 0 then lv-msf-2 = if v-corr then( (lv-qty2 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty2 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-2 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-yqty-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-yqty-3 Dialog-Frame
ON LEAVE OF lv-yqty-3 IN FRAME Dialog-Frame
DO:
    assign lv-qty1.
  if lv-qty1 > 0 then lv-msf-2 = if v-corr then( (lv-qty2 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty2 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-2 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-yqty-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-yqty-4 Dialog-Frame
ON LEAVE OF lv-yqty-4 IN FRAME Dialog-Frame
DO:
    assign lv-qty1.
  if lv-qty1 > 0 then lv-msf-2 = if v-corr then( (lv-qty2 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty2 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-2 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-yqty-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-yqty-5 Dialog-Frame
ON LEAVE OF lv-yqty-5 IN FRAME Dialog-Frame
DO:
    assign lv-qty1.
  if lv-qty1 > 0 then lv-msf-2 = if v-corr then( (lv-qty2 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty2 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-2 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-yqty-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-yqty-6 Dialog-Frame
ON LEAVE OF lv-yqty-6 IN FRAME Dialog-Frame
DO:
    assign lv-qty1.
  if lv-qty1 > 0 then lv-msf-2 = if v-corr then( (lv-qty2 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty2 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-2 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-yqty-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-yqty-7 Dialog-Frame
ON LEAVE OF lv-yqty-7 IN FRAME Dialog-Frame
DO:
    assign lv-qty1.
  if lv-qty1 > 0 then lv-msf-2 = if v-corr then( (lv-qty2 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty2 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-2 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-yqty-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-yqty-8 Dialog-Frame
ON LEAVE OF lv-yqty-8 IN FRAME Dialog-Frame
DO:
    assign lv-qty1.
  if lv-qty1 > 0 then lv-msf-2 = if v-corr then( (lv-qty2 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty2 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-2 with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-yqty-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-yqty-9 Dialog-Frame
ON LEAVE OF lv-yqty-9 IN FRAME Dialog-Frame
DO:
    assign lv-qty1.
  if lv-qty1 > 0 then lv-msf-2 = if v-corr then( (lv-qty2 * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (lv-qty2 * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 ).
  display lv-msf-2 with frame {&frame-name}.
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

   find est-qty where recid(est-qty) = ip-recid no-lock no-error.
   if avail est-qty then do:
      find est where est.company = est-qty.company and
                     est.est-no = est-qty.est-no
                     no-lock no-error.
                     
      assign lv-qty2 = est-qty.qty[2]
             lv-qty3 = est-qty.qty[3]
             lv-qty4 = est-qty.qty[4]
             lv-qty5 = est-qty.qty[5]
             lv-qty6 = est-qty.qty[6]
             lv-qty7 = est-qty.qty[7]
             lv-qty8 = est-qty.qty[8]
             lv-qty9 = est-qty.qty[9]
             lv-qty10 = est-qty.qty[10]
           /*  lv-price-1 = est-qty.qty-price[1]
             lv-price-2 = est-qty.qty-price[2]
             lv-price-3 = est-qty.qty-price[3]
             lv-price-4 = est-qty.qty-price[4]
             lv-price-5 = est-qty.qty-price[5]
             lv-price-6 = est-qty.qty-price[6]
             lv-price-7 = est-qty.qty-price[7]
             lv-price-8 = est-qty.qty-price[8]
             lv-price-9 = est-qty.qty-price[9]
             lv-price-10 = est-qty.qty-price[10]
             lv-uom-1 = est-qty.qty-uom[1]
             lv-uom-2 = est-qty.qty-uom[2]
             lv-uom-3 = est-qty.qty-uom[3]
             lv-uom-4 = est-qty.qty-uom[4]
             lv-uom-5 = est-qty.qty-uom[5]
             lv-uom-6 = est-qty.qty-uom[6]
             lv-uom-7 = est-qty.qty-uom[7]
             lv-uom-8 = est-qty.qty-uom[8]
             lv-uom-9 = est-qty.qty-uom[9]
             lv-uom-10 = est-qty.qty-uom[10]
             lv-date-1 = est-qty.qty-date[1]
             lv-date-2 = est-qty.qty-date[2]
             lv-date-3 = est-qty.qty-date[3]
             lv-date-4 = est-qty.qty-date[4]
             lv-date-5 = est-qty.qty-date[5]
             lv-date-6 = est-qty.qty-date[6]
             lv-date-7 = est-qty.qty-date[7]
             lv-date-8 = est-qty.qty-date[8]
             lv-date-9 = est-qty.qty-date[9]
             lv-date-10 = est-qty.qty-date[10]
             */.
      assign lv-qty11 = est-qty.qty[11]
             lv-qty12 = est-qty.qty[12]
             lv-qty13 = est-qty.qty[13]
             lv-qty14 = est-qty.qty[14]
             lv-qty15 = est-qty.qty[15]
             lv-qty16 = est-qty.qty[16]
             lv-qty17 = est-qty.qty[17]
             lv-qty18 = est-qty.qty[18]
             lv-qty19 = est-qty.qty[19]
             lv-qty20 = est-qty.qty[20]
             /*lv-price-11 = est-qty.qty-price[11]
             lv-price-12 = est-qty.qty-price[12]
             lv-price-13 = est-qty.qty-price[13]
             lv-price-14 = est-qty.qty-price[14]
             lv-price-15 = est-qty.qty-price[15]
             lv-price-16 = est-qty.qty-price[16]
             lv-price-17 = est-qty.qty-price[17]
             lv-price-18 = est-qty.qty-price[18]
             lv-price-19 = est-qty.qty-price[19]
             lv-price-20 = est-qty.qty-price[20]
             lv-uom-11 = est-qty.qty-uom[11]
             lv-uom-12 = est-qty.qty-uom[12]
             lv-uom-13 = est-qty.qty-uom[13]
             lv-uom-14 = est-qty.qty-uom[14]
             lv-uom-15 = est-qty.qty-uom[15]
             lv-uom-16 = est-qty.qty-uom[16]
             lv-uom-17 = est-qty.qty-uom[17]
             lv-uom-18 = est-qty.qty-uom[18]
             lv-uom-19 = est-qty.qty-uom[19]
             lv-uom-20 = est-qty.qty-uom[20]
             lv-date-11 = est-qty.qty-date[11]
             lv-date-12 = est-qty.qty-date[12]
             lv-date-13 = est-qty.qty-date[13]
             lv-date-14 = est-qty.qty-date[14]
             lv-date-15 = est-qty.qty-date[15]
             lv-date-16 = est-qty.qty-date[16]
             lv-date-17 = est-qty.qty-date[17]
             lv-date-18 = est-qty.qty-date[18]
             lv-date-19 = est-qty.qty-date[19]
             lv-date-20 = est-qty.qty-date[20]
             */.
   end.
   lv-qty1 = integer(ip-qty).
   find eb where recid(eb) = ip-eb-recid no-lock no-error.
   if lv-qty1 > 0 then lv-msf-1 = if v-corr then( (est-qty.qty[1] * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (est-qty.qty[1] * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 )
                                            .

   do i = 2 to 20:
      if est-qty.qty[i] > 0 then
      case i :
           when 2 then lv-msf-2 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 )
                                            .
           
           when 3 then lv-msf-3 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 )
                                            .
           when 4 then lv-msf-4 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 )
                                            .
           when 5 then lv-msf-5 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 )
                                            .
           when 6 then lv-msf-6 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 )
                                            .
           when 7 then lv-msf-7 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 )
                                            .
           when 8 then lv-msf-8 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 )
                                            .
           when 9 then lv-msf-9 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 )
                                            .
           when 10 then lv-msf-10 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 )
                                            .
           when 11 then lv-msf-11 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 )
                                            .
           when 12 then lv-msf-12 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 )
                                            .
           when 13 then lv-msf-13 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 )
                                            .
           when 14 then lv-msf-14 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 )
                                            .
           when 15 then lv-msf-15 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 )
                                            .
           when 16 then lv-msf-16 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 )
                                            .
           when 17 then lv-msf-17 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 )
                                            .
           when 18 then lv-msf-18 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 )
                                            .
           when 19 then lv-msf-19 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                  / 1000 )
                                            .
           when 20 then lv-msf-20 = if v-corr then( (est-qty.qty[i] * eb.t-len * eb.t-wid * .007)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
                                                 / 1000 )
                                            else ( (est-qty.qty[i] * eb.t-len * eb.t-wid / 144)
                                                 * (if eb.yld-qty < 0 then -1 / eb.yld-qty else eb.yld-qty)
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
  DISPLAY lv-qty1 lv-qty2 lv-qty3 lv-qty4 lv-qty5 lv-qty6 lv-qty7 lv-qty8 
          lv-qty9 lv-qty10 lv-qty11 lv-qty12 lv-qty13 lv-qty14 lv-qty15 lv-qty16 
          lv-qty17 lv-qty18 lv-qty19 lv-qty20 lv-msf-2 lv-msf-8 lv-msf-7 
          lv-msf-6 lv-msf-5 lv-msf-4 lv-msf-3 lv-msf-20 lv-msf-9 lv-msf-19 
          lv-msf-18 lv-msf-17 lv-msf-16 lv-msf-15 lv-msf-14 lv-msf-12 lv-msf-13 
          lv-msf-11 lv-msf-10 lv-msf-1 lv-yqty-1 lv-yqty-12 lv-yqty-2 lv-yqty-3 
          lv-yqty-4 lv-yqty-5 lv-yqty-6 lv-yqty-7 lv-yqty-8 lv-yqty-9 lv-yqty-10 
          lv-yqty-11 lv-yqty-13 lv-yqty-14 lv-yqty-15 lv-yqty-16 lv-yqty-17 
          lv-yqty-18 lv-yqty-19 lv-yqty-20 
      WITH FRAME Dialog-Frame.
  ENABLE lv-qty1 lv-qty2 lv-qty3 lv-qty4 lv-qty5 lv-qty6 lv-qty7 lv-qty8 
         lv-qty9 lv-qty10 lv-qty11 lv-qty12 lv-qty13 lv-qty14 lv-qty15 lv-qty16 
         lv-qty17 lv-qty18 lv-qty19 lv-qty20 Btn_OK Btn_Cancel lv-yqty-1 
         lv-yqty-12 lv-yqty-2 lv-yqty-3 lv-yqty-4 lv-yqty-5 lv-yqty-6 lv-yqty-7 
         lv-yqty-8 lv-yqty-9 lv-yqty-10 lv-yqty-11 lv-yqty-13 lv-yqty-14 
         lv-yqty-15 lv-yqty-16 lv-yqty-17 lv-yqty-18 lv-yqty-19 lv-yqty-20 
         RECT-6 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

