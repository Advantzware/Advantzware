&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rfq              PROGRESS
*/
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
def input param ip-qty as cha no-undo.
def output param op-char-val as cha no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rfqitem

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH rfqitem SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH rfqitem SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame rfqitem
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame rfqitem


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-qty1 lv-delivery1 lv-price-1 lv-date-1 ~
lv-uom-1 lv-qty2 lv-delivery2 lv-price-2 lv-date-2 lv-uom-2 lv-qty3 ~
lv-delivery3 lv-price-3 lv-date-3 lv-uom-3 lv-qty4 lv-delivery4 lv-price-4 ~
lv-date-4 lv-uom-4 lv-qty5 lv-delivery5 lv-price-5 lv-date-5 lv-uom-5 ~
lv-qty6 lv-delivery6 lv-price-6 lv-date-6 lv-uom-6 lv-qty7 lv-delivery7 ~
lv-price-7 lv-date-7 lv-uom-7 lv-qty8 lv-delivery8 lv-price-8 lv-date-8 ~
lv-uom-8 lv-qty9 lv-delivery9 lv-price-9 lv-date-9 lv-uom-9 lv-qty10 ~
lv-delivery10 lv-price-10 lv-date-10 lv-uom-10 Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS lv-qty1 lv-delivery1 lv-price-1 lv-date-1 ~
lv-uom-1 lv-qty2 lv-delivery2 lv-price-2 lv-date-2 lv-uom-2 lv-qty3 ~
lv-delivery3 lv-price-3 lv-date-3 lv-uom-3 lv-qty4 lv-delivery4 lv-price-4 ~
lv-date-4 lv-uom-4 lv-qty5 lv-delivery5 lv-price-5 lv-date-5 lv-uom-5 ~
lv-qty6 lv-delivery6 lv-price-6 lv-date-6 lv-uom-6 lv-qty7 lv-delivery7 ~
lv-price-7 lv-date-7 lv-uom-7 lv-qty8 lv-delivery8 lv-price-8 lv-date-8 ~
lv-uom-8 lv-qty9 lv-delivery9 lv-price-9 lv-date-9 lv-uom-9 lv-qty10 ~
lv-delivery10 lv-price-10 lv-date-10 lv-uom-10 

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

DEFINE VARIABLE lv-date-1 AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lv-date-10 AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lv-date-2 AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lv-date-3 AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lv-date-4 AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lv-date-5 AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lv-date-6 AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lv-date-7 AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lv-date-8 AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lv-date-9 AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lv-delivery1 AS INTEGER FORMAT ">>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6.2 BY 1.

DEFINE VARIABLE lv-delivery10 AS INTEGER FORMAT ">>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6.2 BY 1.

DEFINE VARIABLE lv-delivery2 AS INTEGER FORMAT ">>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6.2 BY 1.

DEFINE VARIABLE lv-delivery3 AS INTEGER FORMAT ">>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6.2 BY 1.

DEFINE VARIABLE lv-delivery4 AS INTEGER FORMAT ">>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6.2 BY 1.

DEFINE VARIABLE lv-delivery5 AS INTEGER FORMAT ">>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6.2 BY 1.

DEFINE VARIABLE lv-delivery6 AS INTEGER FORMAT ">>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6.2 BY 1.

DEFINE VARIABLE lv-delivery7 AS INTEGER FORMAT ">>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6.2 BY 1.

DEFINE VARIABLE lv-delivery8 AS INTEGER FORMAT ">>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6.2 BY 1.

DEFINE VARIABLE lv-delivery9 AS INTEGER FORMAT ">>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6.2 BY 1.

DEFINE VARIABLE lv-price-1 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-price-10 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-price-2 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-price-3 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-price-4 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-price-5 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-price-6 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-price-7 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-price-8 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-price-9 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty1 AS INTEGER FORMAT "->>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY 1.

DEFINE VARIABLE lv-qty10 AS INTEGER FORMAT "->>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY 1.

DEFINE VARIABLE lv-qty2 AS INTEGER FORMAT "->>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY 1.

DEFINE VARIABLE lv-qty3 AS INTEGER FORMAT "->>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY 1.

DEFINE VARIABLE lv-qty4 AS INTEGER FORMAT "->>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY 1.

DEFINE VARIABLE lv-qty5 AS INTEGER FORMAT "->>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY 1.

DEFINE VARIABLE lv-qty6 AS INTEGER FORMAT "->>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY 1.

DEFINE VARIABLE lv-qty7 AS INTEGER FORMAT "->>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY 1.1.

DEFINE VARIABLE lv-qty8 AS INTEGER FORMAT "->>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY 1.

DEFINE VARIABLE lv-qty9 AS INTEGER FORMAT "->>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY 1.

DEFINE VARIABLE lv-uom-1 AS CHARACTER FORMAT "x(5)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-uom-10 AS CHARACTER FORMAT "x(5)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-uom-2 AS CHARACTER FORMAT "x(5)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-uom-3 AS CHARACTER FORMAT "x(5)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-uom-4 AS CHARACTER FORMAT "x(5)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-uom-5 AS CHARACTER FORMAT "x(5)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-uom-6 AS CHARACTER FORMAT "x(5)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-uom-7 AS CHARACTER FORMAT "x(5)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-uom-8 AS CHARACTER FORMAT "x(5)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-uom-9 AS CHARACTER FORMAT "x(5)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      rfqitem SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     lv-qty1 AT ROW 2.95 COL 8 COLON-ALIGNED NO-LABEL
     lv-delivery1 AT ROW 2.91 COL 29 COLON-ALIGNED NO-LABEL
     lv-price-1 AT ROW 2.95 COL 41 COLON-ALIGNED NO-LABEL
     lv-date-1 AT ROW 2.95 COL 74 COLON-ALIGNED NO-LABEL
     lv-uom-1 AT ROW 3 COL 57.8 COLON-ALIGNED NO-LABEL
     lv-qty2 AT ROW 3.91 COL 8 COLON-ALIGNED NO-LABEL
     lv-delivery2 AT ROW 3.91 COL 29 COLON-ALIGNED NO-LABEL
     lv-price-2 AT ROW 3.91 COL 41 COLON-ALIGNED NO-LABEL
     lv-date-2 AT ROW 3.91 COL 74 COLON-ALIGNED NO-LABEL
     lv-uom-2 AT ROW 4 COL 58 COLON-ALIGNED NO-LABEL
     lv-qty3 AT ROW 4.91 COL 8 COLON-ALIGNED NO-LABEL
     lv-delivery3 AT ROW 4.91 COL 29 COLON-ALIGNED NO-LABEL
     lv-price-3 AT ROW 4.91 COL 41 COLON-ALIGNED NO-LABEL
     lv-date-3 AT ROW 4.91 COL 74 COLON-ALIGNED NO-LABEL
     lv-uom-3 AT ROW 5 COL 58 COLON-ALIGNED NO-LABEL
     lv-qty4 AT ROW 5.76 COL 8 COLON-ALIGNED NO-LABEL
     lv-delivery4 AT ROW 5.91 COL 29 COLON-ALIGNED NO-LABEL
     lv-price-4 AT ROW 5.76 COL 41 COLON-ALIGNED NO-LABEL
     lv-date-4 AT ROW 5.76 COL 74 COLON-ALIGNED NO-LABEL
     lv-uom-4 AT ROW 5.86 COL 58 COLON-ALIGNED NO-LABEL
     lv-qty5 AT ROW 6.71 COL 8 COLON-ALIGNED NO-LABEL
     lv-delivery5 AT ROW 6.91 COL 29 COLON-ALIGNED NO-LABEL
     lv-price-5 AT ROW 6.71 COL 41 COLON-ALIGNED NO-LABEL
     lv-date-5 AT ROW 6.71 COL 74 COLON-ALIGNED NO-LABEL
     lv-uom-5 AT ROW 6.81 COL 58 COLON-ALIGNED NO-LABEL
     lv-qty6 AT ROW 7.71 COL 8 COLON-ALIGNED NO-LABEL
     lv-delivery6 AT ROW 7.91 COL 29 COLON-ALIGNED NO-LABEL
     lv-price-6 AT ROW 7.71 COL 41 COLON-ALIGNED NO-LABEL
     lv-date-6 AT ROW 7.71 COL 74 COLON-ALIGNED NO-LABEL
     lv-uom-6 AT ROW 7.81 COL 58 COLON-ALIGNED NO-LABEL
     lv-qty7 AT ROW 8.67 COL 8 COLON-ALIGNED NO-LABEL
     lv-delivery7 AT ROW 8.91 COL 29 COLON-ALIGNED NO-LABEL
     lv-price-7 AT ROW 8.76 COL 41 COLON-ALIGNED NO-LABEL
     lv-date-7 AT ROW 8.76 COL 74 COLON-ALIGNED NO-LABEL
     lv-uom-7 AT ROW 8.86 COL 58 COLON-ALIGNED NO-LABEL
     lv-qty8 AT ROW 9.81 COL 10 NO-LABEL
     lv-delivery8 AT ROW 9.91 COL 29 COLON-ALIGNED NO-LABEL
     lv-price-8 AT ROW 9.76 COL 41 COLON-ALIGNED NO-LABEL
     lv-date-8 AT ROW 9.76 COL 74 COLON-ALIGNED NO-LABEL
     lv-uom-8 AT ROW 9.86 COL 58.2 COLON-ALIGNED NO-LABEL
     lv-qty9 AT ROW 10.76 COL 8 COLON-ALIGNED NO-LABEL
     lv-delivery9 AT ROW 10.91 COL 29 COLON-ALIGNED NO-LABEL
     lv-price-9 AT ROW 10.76 COL 41 COLON-ALIGNED NO-LABEL
     lv-date-9 AT ROW 10.76 COL 74 COLON-ALIGNED NO-LABEL
     lv-uom-9 AT ROW 10.86 COL 58 COLON-ALIGNED NO-LABEL
     lv-qty10 AT ROW 11.71 COL 8 COLON-ALIGNED NO-LABEL
     lv-delivery10 AT ROW 11.91 COL 29 COLON-ALIGNED NO-LABEL
     lv-price-10 AT ROW 11.71 COL 41 COLON-ALIGNED NO-LABEL
     lv-date-10 AT ROW 11.71 COL 74 COLON-ALIGNED NO-LABEL
     lv-uom-10 AT ROW 11.81 COL 58 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 13.62 COL 17
     Btn_Cancel AT ROW 13.62 COL 66
     "Deliveries" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 2.19 COL 29
     "Date" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.14 COL 80.4
     "UOM" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.19 COL 63
     "Price" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.19 COL 45.8
     "Quantity" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.14 COL 14.8
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     "Quantity Detail" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 1 COL 9
     SPACE(80.39) SKIP(13.56)
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

/* SETTINGS FOR FILL-IN lv-qty8 IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "rfq.rfqitem"
     _Options          = "SHARE-LOCK"
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
           lv-price-1 lv-price-2
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
           lv-date-9 lv-date-10
           lv-delivery1 lv-delivery2                        
           lv-delivery3 lv-delivery4
           lv-delivery5 lv-delivery6
           lv-delivery7 lv-delivery8
           lv-delivery9 lv-delivery10                                           
           .
    op-char-val = string(lv-qty1) + "," + string(lv-qty2) + "," +
                  string(lv-qty3) + "," + string(lv-qty4) + "," +
                  string(lv-qty5) + "," + string(lv-qty6) + "," +
                  string(lv-qty7) + "," + string(lv-qty8) + "," +
                  string(lv-qty9) + "," + string(lv-qty10) 
                  + "," +
                  string(lv-price-1) + "," + string(lv-price-2) + "," +
                  string(lv-price-3) + "," + string(lv-price-4) + "," +
                  string(lv-price-5) + "," + string(lv-price-6) + "," +
                  string(lv-price-7) + "," + string(lv-price-8) + "," +
                  string(lv-price-9) + "," + string(lv-price-10) + "," +
                  string(lv-uom-1) + "," + string(lv-uom-2) + "," +
                  string(lv-uom-3) + "," + string(lv-uom-4) + "," +
                  string(lv-uom-5) + "," + string(lv-uom-6) + "," +
                  string(lv-uom-7) + "," + string(lv-uom-8) + "," +
                  string(lv-uom-9) + "," + string(lv-uom-10) + "," +
                  string(lv-date-1,"99/99/9999") + "," + string(lv-date-2,"99/99.9999") + "," +
                  string(lv-date-3,"99/99/9999") + "," + string(lv-date-4,"99/99.9999") + "," +
                  string(lv-date-5,"99/99/9999") + "," + string(lv-date-6,"99/99.9999") + "," +
                  string(lv-date-7,"99/99/9999") + "," + string(lv-date-8,"99/99.9999") + "," +
                  string(lv-date-9,"99/99/9999") + "," + string(lv-date-10,"99/99.9999") + "," +
                  string(lv-delivery1) + "," + string(lv-delivery2) + "," +
                  string(lv-delivery3) + "," + string(lv-delivery4) + "," +
                  string(lv-delivery5) + "," + string(lv-delivery6) + "," +
                  string(lv-delivery7) + "," + string(lv-delivery8) + "," +
                  string(lv-delivery9) + "," + string(lv-delivery10)                                                                                           
             .
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   find rfqitem where recid(rfqitem) = ip-recid no-lock.
   if avail rfqitem then 
      assign lv-qty2 = rfqitem.qty[2]
             lv-qty3 = rfqitem.qty[3]
             lv-qty4 = rfqitem.qty[4]
             lv-qty5 = rfqitem.qty[5]
             lv-qty6 = rfqitem.qty[6]
             lv-qty7 = rfqitem.qty[7]
             lv-qty8 = rfqitem.qty[8]
             lv-qty9 = rfqitem.qty[9]
             lv-qty10 = rfqitem.qty[10]
             lv-price-1 = rfqitem.qty-price[1]
             lv-price-2 = rfqitem.qty-price[2]
             lv-price-3 = rfqitem.qty-price[3]
             lv-price-4 = rfqitem.qty-price[4]
             lv-price-5 = rfqitem.qty-price[5]
             lv-price-6 = rfqitem.qty-price[6]
             lv-price-7 = rfqitem.qty-price[7]
             lv-price-8 = rfqitem.qty-price[8]
             lv-price-9 = rfqitem.qty-price[9]
             lv-price-10 = rfqitem.qty-price[10]
             lv-uom-1 = rfqitem.qty-uom[1]
             lv-uom-2 = rfqitem.qty-uom[2]
             lv-uom-3 = rfqitem.qty-uom[3]
             lv-uom-4 = rfqitem.qty-uom[4]
             lv-uom-5 = rfqitem.qty-uom[5]
             lv-uom-6 = rfqitem.qty-uom[6]
             lv-uom-7 = rfqitem.qty-uom[7]
             lv-uom-8 = rfqitem.qty-uom[8]
             lv-uom-9 = rfqitem.qty-uom[9]
             lv-uom-10 = rfqitem.qty-uom[10]
             lv-date-1 = rfqitem.qty-date[1]
             lv-date-2 = rfqitem.qty-date[2]
             lv-date-3 = rfqitem.qty-date[3]
             lv-date-4 = rfqitem.qty-date[4]
             lv-date-5 = rfqitem.qty-date[5]
             lv-date-6 = rfqitem.qty-date[6]
             lv-date-7 = rfqitem.qty-date[7]
             lv-date-8 = rfqitem.qty-date[8]
             lv-date-9 = rfqitem.qty-date[9]
             lv-date-10 = rfqitem.qty-date[10]
             .
      
   lv-qty1 = integer(ip-qty).
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
  DISPLAY lv-qty1 lv-delivery1 lv-price-1 lv-date-1 lv-uom-1 lv-qty2 
          lv-delivery2 lv-price-2 lv-date-2 lv-uom-2 lv-qty3 lv-delivery3 
          lv-price-3 lv-date-3 lv-uom-3 lv-qty4 lv-delivery4 lv-price-4 
          lv-date-4 lv-uom-4 lv-qty5 lv-delivery5 lv-price-5 lv-date-5 lv-uom-5 
          lv-qty6 lv-delivery6 lv-price-6 lv-date-6 lv-uom-6 lv-qty7 
          lv-delivery7 lv-price-7 lv-date-7 lv-uom-7 lv-qty8 lv-delivery8 
          lv-price-8 lv-date-8 lv-uom-8 lv-qty9 lv-delivery9 lv-price-9 
          lv-date-9 lv-uom-9 lv-qty10 lv-delivery10 lv-price-10 lv-date-10 
          lv-uom-10 
      WITH FRAME Dialog-Frame.
  ENABLE lv-qty1 lv-delivery1 lv-price-1 lv-date-1 lv-uom-1 lv-qty2 
         lv-delivery2 lv-price-2 lv-date-2 lv-uom-2 lv-qty3 lv-delivery3 
         lv-price-3 lv-date-3 lv-uom-3 lv-qty4 lv-delivery4 lv-price-4 
         lv-date-4 lv-uom-4 lv-qty5 lv-delivery5 lv-price-5 lv-date-5 lv-uom-5 
         lv-qty6 lv-delivery6 lv-price-6 lv-date-6 lv-uom-6 lv-qty7 
         lv-delivery7 lv-price-7 lv-date-7 lv-uom-7 lv-qty8 lv-delivery8 
         lv-price-8 lv-date-8 lv-uom-8 lv-qty9 lv-delivery9 lv-price-9 
         lv-date-9 lv-uom-9 lv-qty10 lv-delivery10 lv-price-10 lv-date-10 
         lv-uom-10 Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

