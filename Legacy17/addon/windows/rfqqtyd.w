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
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame rfqitem.qty[1] rfqitem.qty[2] ~
rfqitem.qty[3] rfqitem.qty[4] rfqitem.qty[5] rfqitem.qty[6] rfqitem.qty[7] ~
rfqitem.qty[8] rfqitem.qty[9] rfqitem.qty[10] rfqitem.delivery[1] ~
rfqitem.qty-price[1] rfqitem.qty-uom[1] rfqitem.qty-date[1] ~
rfqitem.delivery[2] rfqitem.qty-price[2] rfqitem.qty-uom[2] ~
rfqitem.qty-date[2] rfqitem.delivery[3] rfqitem.qty-price[3] ~
rfqitem.qty-uom[3] rfqitem.qty-date[3] rfqitem.delivery[4] ~
rfqitem.qty-price[4] rfqitem.qty-uom[4] rfqitem.qty-date[4] ~
rfqitem.delivery[5] rfqitem.qty-price[5] rfqitem.qty-uom[5] ~
rfqitem.qty-date[5] rfqitem.delivery[6] rfqitem.qty-price[6] ~
rfqitem.qty-uom[6] rfqitem.qty-date[6] rfqitem.delivery[7] ~
rfqitem.qty-price[7] rfqitem.qty-uom[7] rfqitem.qty-date[7] ~
rfqitem.delivery[8] rfqitem.qty-price[8] rfqitem.qty-uom[8] ~
rfqitem.qty-date[8] rfqitem.delivery[9] rfqitem.qty-price[9] ~
rfqitem.qty-uom[9] rfqitem.qty-date[9] rfqitem.delivery[10] ~
rfqitem.qty-price[10] rfqitem.qty-uom[10] rfqitem.qty-date[10] 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame rfqitem.qty[1] ~
rfqitem.qty[2] rfqitem.qty[3] rfqitem.qty[4] rfqitem.qty[5] rfqitem.qty[6] ~
rfqitem.qty[7] rfqitem.qty[8] rfqitem.qty[9] rfqitem.qty[10] ~
rfqitem.delivery[1] rfqitem.qty-price[1] rfqitem.qty-uom[1] ~
rfqitem.qty-date[1] rfqitem.delivery[2] rfqitem.qty-price[2] ~
rfqitem.qty-uom[2] rfqitem.qty-date[2] rfqitem.delivery[3] ~
rfqitem.qty-price[3] rfqitem.qty-uom[3] rfqitem.qty-date[3] ~
rfqitem.delivery[4] rfqitem.qty-price[4] rfqitem.qty-uom[4] ~
rfqitem.qty-date[4] rfqitem.delivery[5] rfqitem.qty-price[5] ~
rfqitem.qty-uom[5] rfqitem.qty-date[5] rfqitem.delivery[6] ~
rfqitem.qty-price[6] rfqitem.qty-uom[6] rfqitem.qty-date[6] ~
rfqitem.delivery[7] rfqitem.qty-price[7] rfqitem.qty-uom[7] ~
rfqitem.qty-date[7] rfqitem.delivery[8] rfqitem.qty-price[8] ~
rfqitem.qty-uom[8] rfqitem.qty-date[8] rfqitem.delivery[9] ~
rfqitem.qty-price[9] rfqitem.qty-uom[9] rfqitem.qty-date[9] ~
rfqitem.delivery[10] rfqitem.qty-price[10] rfqitem.qty-uom[10] ~
rfqitem.qty-date[10] 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame rfqitem
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame rfqitem
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH rfqitem SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH rfqitem SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame rfqitem
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame rfqitem


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS rfqitem.qty[1] rfqitem.qty[2] rfqitem.qty[3] ~
rfqitem.qty[4] rfqitem.qty[5] rfqitem.qty[6] rfqitem.qty[7] rfqitem.qty[8] ~
rfqitem.qty[9] rfqitem.qty[10] rfqitem.delivery[1] rfqitem.qty-price[1] ~
rfqitem.qty-uom[1] rfqitem.qty-date[1] rfqitem.delivery[2] ~
rfqitem.qty-price[2] rfqitem.qty-uom[2] rfqitem.qty-date[2] ~
rfqitem.delivery[3] rfqitem.qty-price[3] rfqitem.qty-uom[3] ~
rfqitem.qty-date[3] rfqitem.delivery[4] rfqitem.qty-price[4] ~
rfqitem.qty-uom[4] rfqitem.qty-date[4] rfqitem.delivery[5] ~
rfqitem.qty-price[5] rfqitem.qty-uom[5] rfqitem.qty-date[5] ~
rfqitem.delivery[6] rfqitem.qty-price[6] rfqitem.qty-uom[6] ~
rfqitem.qty-date[6] rfqitem.delivery[7] rfqitem.qty-price[7] ~
rfqitem.qty-uom[7] rfqitem.qty-date[7] rfqitem.delivery[8] ~
rfqitem.qty-price[8] rfqitem.qty-uom[8] rfqitem.qty-date[8] ~
rfqitem.delivery[9] rfqitem.qty-price[9] rfqitem.qty-uom[9] ~
rfqitem.qty-date[9] rfqitem.delivery[10] rfqitem.qty-price[10] ~
rfqitem.qty-uom[10] rfqitem.qty-date[10] 
&Scoped-define ENABLED-TABLES rfqitem
&Scoped-define FIRST-ENABLED-TABLE rfqitem
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Cancel RECT-17 
&Scoped-Define DISPLAYED-FIELDS rfqitem.qty[1] rfqitem.qty[2] ~
rfqitem.qty[3] rfqitem.qty[4] rfqitem.qty[5] rfqitem.qty[6] rfqitem.qty[7] ~
rfqitem.qty[8] rfqitem.qty[9] rfqitem.qty[10] rfqitem.delivery[1] ~
rfqitem.qty-price[1] rfqitem.qty-uom[1] rfqitem.qty-date[1] ~
rfqitem.delivery[2] rfqitem.qty-price[2] rfqitem.qty-uom[2] ~
rfqitem.qty-date[2] rfqitem.delivery[3] rfqitem.qty-price[3] ~
rfqitem.qty-uom[3] rfqitem.qty-date[3] rfqitem.delivery[4] ~
rfqitem.qty-price[4] rfqitem.qty-uom[4] rfqitem.qty-date[4] ~
rfqitem.delivery[5] rfqitem.qty-price[5] rfqitem.qty-uom[5] ~
rfqitem.qty-date[5] rfqitem.delivery[6] rfqitem.qty-price[6] ~
rfqitem.qty-uom[6] rfqitem.qty-date[6] rfqitem.delivery[7] ~
rfqitem.qty-price[7] rfqitem.qty-uom[7] rfqitem.qty-date[7] ~
rfqitem.delivery[8] rfqitem.qty-price[8] rfqitem.qty-uom[8] ~
rfqitem.qty-date[8] rfqitem.delivery[9] rfqitem.qty-price[9] ~
rfqitem.qty-uom[9] rfqitem.qty-date[9] rfqitem.delivery[10] ~
rfqitem.qty-price[10] rfqitem.qty-uom[10] rfqitem.qty-date[10] 
&Scoped-define DISPLAYED-TABLES rfqitem
&Scoped-define FIRST-DISPLAYED-TABLE rfqitem


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

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 98 BY 15.48.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      rfqitem SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     rfqitem.qty[1] AT ROW 2.91 COL 7 COLON-ALIGNED NO-LABEL FORMAT "->>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     rfqitem.qty[2] AT ROW 3.91 COL 7 COLON-ALIGNED NO-LABEL FORMAT "->>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     rfqitem.qty[3] AT ROW 4.91 COL 7 COLON-ALIGNED NO-LABEL FORMAT "->>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     rfqitem.qty[4] AT ROW 5.91 COL 7 COLON-ALIGNED NO-LABEL FORMAT "->>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     rfqitem.qty[5] AT ROW 6.91 COL 7 COLON-ALIGNED NO-LABEL FORMAT "->>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     rfqitem.qty[6] AT ROW 7.86 COL 7 COLON-ALIGNED NO-LABEL FORMAT "->>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     rfqitem.qty[7] AT ROW 8.76 COL 7 COLON-ALIGNED NO-LABEL FORMAT "->>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1.1
     rfqitem.qty[8] AT ROW 9.86 COL 7 COLON-ALIGNED NO-LABEL FORMAT "->>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     rfqitem.qty[9] AT ROW 10.86 COL 7 COLON-ALIGNED NO-LABEL FORMAT "->>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     rfqitem.qty[10] AT ROW 11.91 COL 7 COLON-ALIGNED NO-LABEL FORMAT "->>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     rfqitem.delivery[1] AT ROW 2.91 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     rfqitem.qty-price[1] AT ROW 2.91 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty-uom[1] AT ROW 2.91 COL 60 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     rfqitem.qty-date[1] AT ROW 2.91 COL 76 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.delivery[2] AT ROW 3.91 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     rfqitem.qty-price[2] AT ROW 3.86 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty-uom[2] AT ROW 3.86 COL 60 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     rfqitem.qty-date[2] AT ROW 3.91 COL 76 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.delivery[3] AT ROW 4.91 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     rfqitem.qty-price[3] AT ROW 4.86 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty-uom[3] AT ROW 4.81 COL 60 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     rfqitem.qty-date[3] AT ROW 4.91 COL 76 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.delivery[4] AT ROW 5.91 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     rfqitem.qty-price[4] AT ROW 5.86 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty-uom[4] AT ROW 5.81 COL 60 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     rfqitem.qty-date[4] AT ROW 5.91 COL 76 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     rfqitem.delivery[5] AT ROW 6.91 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     rfqitem.qty-price[5] AT ROW 6.86 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty-uom[5] AT ROW 6.81 COL 60 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     rfqitem.qty-date[5] AT ROW 6.91 COL 76 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.delivery[6] AT ROW 7.91 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     rfqitem.qty-price[6] AT ROW 7.86 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty-uom[6] AT ROW 7.81 COL 60 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     rfqitem.qty-date[6] AT ROW 7.91 COL 76 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.delivery[7] AT ROW 8.91 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     rfqitem.qty-price[7] AT ROW 8.86 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty-uom[7] AT ROW 8.81 COL 60 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     rfqitem.qty-date[7] AT ROW 8.91 COL 76 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.delivery[8] AT ROW 9.91 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     rfqitem.qty-price[8] AT ROW 9.86 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty-uom[8] AT ROW 9.81 COL 60 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     rfqitem.qty-date[8] AT ROW 9.91 COL 76 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.delivery[9] AT ROW 10.91 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     rfqitem.qty-price[9] AT ROW 10.86 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty-uom[9] AT ROW 10.81 COL 60 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     rfqitem.qty-date[9] AT ROW 10.91 COL 76 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.delivery[10] AT ROW 11.91 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     rfqitem.qty-price[10] AT ROW 11.86 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty-uom[10] AT ROW 11.81 COL 60 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     rfqitem.qty-date[10] AT ROW 11.91 COL 76 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     Btn_OK AT ROW 14.57 COL 21
     Btn_Cancel AT ROW 14.57 COL 64
     RECT-17 AT ROW 1 COL 2
     "Price" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.19 COL 43
     "Quantity" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.19 COL 13
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     "Date" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.19 COL 81
     "UOM" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.19 COL 66
     "Quantity Detail" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 1.24 COL 7
     "Deliveries" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 2.19 COL 29
     SPACE(63.99) SKIP(22.28)
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

/* SETTINGS FOR FILL-IN rfqitem.qty[10] IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN rfqitem.qty[1] IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN rfqitem.qty[2] IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN rfqitem.qty[3] IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN rfqitem.qty[4] IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN rfqitem.qty[5] IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN rfqitem.qty[6] IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN rfqitem.qty[7] IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN rfqitem.qty[8] IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN rfqitem.qty[9] IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
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
    assign rfqitem.qty[1 for 10]
           rfqitem.qty-price[1 for 10]
           rfqitem.qty-uom[1 for 10]
           rfqitem.qty-date[1 for 10]
           rfqitem.delivery[1 for 10]
           op-char-val = string(rfqitem.qty[1]).
       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   find rfqitem where recid(rfqitem) = ip-recid .
   RUN enable_UI.
 

   if rfqitem.qty[1]:screen-value in frame {&frame-name} <> ip-qty then
     rfqitem.qty[1]:screen-value in frame {&frame-name} = ip-qty.
  

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
  IF AVAILABLE rfqitem THEN 
    DISPLAY rfqitem.qty[1] rfqitem.qty[2] rfqitem.qty[3] rfqitem.qty[4] 
          rfqitem.qty[5] rfqitem.qty[6] rfqitem.qty[7] rfqitem.qty[8] 
          rfqitem.qty[9] rfqitem.qty[10] rfqitem.delivery[1] 
          rfqitem.qty-price[1] rfqitem.qty-uom[1] rfqitem.qty-date[1] 
          rfqitem.delivery[2] rfqitem.qty-price[2] rfqitem.qty-uom[2] 
          rfqitem.qty-date[2] rfqitem.delivery[3] rfqitem.qty-price[3] 
          rfqitem.qty-uom[3] rfqitem.qty-date[3] rfqitem.delivery[4] 
          rfqitem.qty-price[4] rfqitem.qty-uom[4] rfqitem.qty-date[4] 
          rfqitem.delivery[5] rfqitem.qty-price[5] rfqitem.qty-uom[5] 
          rfqitem.qty-date[5] rfqitem.delivery[6] rfqitem.qty-price[6] 
          rfqitem.qty-uom[6] rfqitem.qty-date[6] rfqitem.delivery[7] 
          rfqitem.qty-price[7] rfqitem.qty-uom[7] rfqitem.qty-date[7] 
          rfqitem.delivery[8] rfqitem.qty-price[8] rfqitem.qty-uom[8] 
          rfqitem.qty-date[8] rfqitem.delivery[9] rfqitem.qty-price[9] 
          rfqitem.qty-uom[9] rfqitem.qty-date[9] rfqitem.delivery[10] 
          rfqitem.qty-price[10] rfqitem.qty-uom[10] rfqitem.qty-date[10] 
      WITH FRAME Dialog-Frame.
  ENABLE rfqitem.qty[1] rfqitem.qty[2] rfqitem.qty[3] rfqitem.qty[4] 
         rfqitem.qty[5] rfqitem.qty[6] rfqitem.qty[7] rfqitem.qty[8] 
         rfqitem.qty[9] rfqitem.qty[10] rfqitem.delivery[1] 
         rfqitem.qty-price[1] rfqitem.qty-uom[1] rfqitem.qty-date[1] 
         rfqitem.delivery[2] rfqitem.qty-price[2] rfqitem.qty-uom[2] 
         rfqitem.qty-date[2] rfqitem.delivery[3] rfqitem.qty-price[3] 
         rfqitem.qty-uom[3] rfqitem.qty-date[3] rfqitem.delivery[4] 
         rfqitem.qty-price[4] rfqitem.qty-uom[4] rfqitem.qty-date[4] 
         rfqitem.delivery[5] rfqitem.qty-price[5] rfqitem.qty-uom[5] 
         rfqitem.qty-date[5] rfqitem.delivery[6] rfqitem.qty-price[6] 
         rfqitem.qty-uom[6] rfqitem.qty-date[6] rfqitem.delivery[7] 
         rfqitem.qty-price[7] rfqitem.qty-uom[7] rfqitem.qty-date[7] 
         rfqitem.delivery[8] rfqitem.qty-price[8] rfqitem.qty-uom[8] 
         rfqitem.qty-date[8] rfqitem.delivery[9] rfqitem.qty-price[9] 
         rfqitem.qty-uom[9] rfqitem.qty-date[9] rfqitem.delivery[10] 
         rfqitem.qty-price[10] rfqitem.qty-uom[10] rfqitem.qty-date[10] Btn_OK 
         Btn_Cancel RECT-17 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

