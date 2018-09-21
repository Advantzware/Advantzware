&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS gDialog 
/*------------------------------------------------------------------------

  File: oe\d-quotedprices.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAM ipxCompany LIKE oe-ordl.company NO-UNDO.
DEF INPUT PARAM ipxLoc LIKE quotehd.loc NO-UNDO.
DEF INPUT PARAM ipxEstNo LIKE oe-ordl.est-no NO-UNDO.
DEF INPUT PARAM ipxCustNo LIKE oe-ordl.cust-no NO-UNDO.
DEF INPUT PARAM ipxPartNo LIKE oe-ordl.part-no NO-UNDO.
DEF INPUT PARAM ipxINo LIKE oe-ordl.i-no NO-UNDO.
DEF OUTPUT PARAM opxPrice LIKE oe-ordl.price NO-UNDO.
DEF OUTPUT PARAM opxUom LIKE oe-ordl.pr-uom NO-UNDO.
DEF OUTPUT PARAM opxQty LIKE oe-ordl.qty NO-UNDO.
DEF OUTPUT PARAM opcChoice AS CHAR NO-UNDO. /*PRICE,PRICEQTY,CANCEL*/

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE ttQuoteList
    FIELD price LIKE oe-ordl.price
    FIELD qty LIKE oe-ordl.qty
    FIELD est-no LIKE oe-ordl.est-no
    FIELD q-no LIKE quotehd.q-no
    FIELD uom LIKE oe-ordl.pr-uom
    FIELD quote-date LIKE quotehd.quo-date
    FIELD rel LIKE quoteqty.rel
    FIELD rel-qty LIKE quoteqty.qty
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME gDialog
&Scoped-define BROWSE-NAME BROWSE-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttQuotelist

/* Definitions for BROWSE BROWSE-3                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-3 ttQuoteList.q-no ttQuoteList.qty ttQuoteList.price ttQuoteList.uom ttQuoteList.est-no ttQuoteList.quote-date ttQuoteList.rel ttQuoteList.rel-qty   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-3   
&Scoped-define SELF-NAME BROWSE-3
&Scoped-define QUERY-STRING-BROWSE-3 FOR EACH ttQuotelist
&Scoped-define OPEN-QUERY-BROWSE-3 OPEN QUERY {&SELF-NAME} FOR EACH ttQuotelist.
&Scoped-define TABLES-IN-QUERY-BROWSE-3 ttQuotelist
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-3 ttQuotelist


/* Definitions for DIALOG-BOX gDialog                                   */
&Scoped-define OPEN-BROWSERS-IN-QUERY-gDialog ~
    ~{&OPEN-QUERY-BROWSE-3}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-3 btn_price btn_priceqty Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS ed_Message 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD checkResults gDialog 
FUNCTION checkResults RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMessage gDialog 
FUNCTION getMessage RETURNS CHARACTER
  ( ipcMsgStart AS CHAR , iplError AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD quoteForEstimateExists gDialog 
FUNCTION quoteForEstimateExists RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 27 BY 1.29.

DEFINE BUTTON btn_price AUTO-GO 
     LABEL "Import Price" 
     SIZE 27 BY 1.29.

DEFINE BUTTON btn_priceqty AUTO-GO 
     LABEL "Import Price && Quantity" 
     SIZE 27 BY 1.29.

DEFINE VARIABLE ed_Message AS CHARACTER 
     VIEW-AS EDITOR NO-BOX
     SIZE 67 BY 2.86 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-3 FOR 
      ttQuotelist SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 gDialog _FREEFORM
  QUERY BROWSE-3 DISPLAY
      ttQuoteList.q-no LABEL "Quote#"
      ttQuoteList.qty FORM ">>>,>>>,>>9" LABEL "Quantity"
      ttQuoteList.price FORM "->>>,>>>,>>9.99<<<" LABEL "Price"
      ttQuoteList.uom LABEL "UOM" WIDTH 5
      ttQuoteList.est-no FORM "X(8)" LABEL "Estimate"
      ttQuoteList.quote-date LABEL "Date"
      ttQuoteList.rel FORM ">>9" LABEL "Rel" WIDTH 5
      ttQuoteList.rel-qty FORM ">>>,>>>,>>9" LABEL "Ship Qty"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 88 BY 8.81 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     ed_Message AT ROW 1.24 COL 11.2 NO-LABEL WIDGET-ID 12 NO-TAB-STOP 
     BROWSE-3 AT ROW 4.33 COL 2
     btn_price AT ROW 13.62 COL 3
     btn_priceqty AT ROW 13.62 COL 30.6 WIDGET-ID 2
     Btn_Cancel AT ROW 13.62 COL 58.6
     SPACE(5.39) SKIP(0.27)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Quoted Prices"
         DEFAULT-BUTTON btn_price CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB gDialog 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX gDialog
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-3 ed_Message gDialog */
ASSIGN 
       FRAME gDialog:SCROLLABLE       = FALSE
       FRAME gDialog:HIDDEN           = TRUE.

ASSIGN 
       BROWSE-3:COLUMN-RESIZABLE IN FRAME gDialog       = TRUE.

/* SETTINGS FOR EDITOR ed_Message IN FRAME gDialog
   NO-ENABLE                                                            */
ASSIGN 
       ed_Message:READ-ONLY IN FRAME gDialog        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-3
/* Query rebuild information for BROWSE BROWSE-3
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttQuotelist.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX gDialog
/* Query rebuild information for DIALOG-BOX gDialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX gDialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON WINDOW-CLOSE OF FRAME gDialog /* Quoted Prices */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-3
&Scoped-define SELF-NAME BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-3 gDialog
ON DEFAULT-ACTION OF BROWSE-3 IN FRAME gDialog
DO:
   APPLY "choose" TO btn_price.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_price
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_price gDialog
ON CHOOSE OF btn_price IN FRAME gDialog /* Import Price */
DO:
   ASSIGN
      opcChoice = "PRICE"
      opxPrice = ttQuoteList.price
      opxUom = ttQuoteList.uom.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_priceqty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_priceqty gDialog
ON CHOOSE OF btn_priceqty IN FRAME gDialog /* Import Price  Quantity */
DO:
   ASSIGN
      opcChoice = "PRICEQTY"
      opxPrice = ttQuoteList.price
      opxQty   = ttQuoteList.qty
      opxUom = ttQuoteList.uom.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK gDialog 


/* ***************************  Main Block  *************************** */

RUN BuildListing.
IF checkResults() THEN DO:
    RUN DisplayMessage.
    {src/adm2/dialogmn.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects gDialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildListing gDialog 
PROCEDURE BuildListing :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR llAllEstimates AS LOG NO-UNDO.

IF ipxEstNo NE "" THEN DO:
    llAllEstimates = NOT quoteForEstimateExists().
END.
FOR EACH quotehd FIELDS(q-no est-no company quo-date) NO-LOCK  WHERE
        quotehd.company EQ ipxCompany
        AND quotehd.cust-no EQ ipxCustNo
        AND quotehd.quo-date LE TODAY 
        AND (quotehd.expireDate GE TODAY OR quotehd.expireDate EQ ?)
        AND (
             ipxEstNo EQ ""
             OR llAllEstimates
             OR (
                 ipxEstNo NE "" 
                 AND quotehd.est-no EQ ipxEstNo
                )
            )
    ,
    EACH quoteitm NO-LOCK WHERE
        quoteitm.company EQ quotehd.company 
        AND quoteitm.loc     EQ ipxLoc 
        AND quoteitm.q-no    EQ quotehd.q-no 
        AND (
             quoteitm.part-no EQ ipxPartNo 
             OR (quoteitm.part-no EQ ipxINo 
                 AND ipxINo NE ""
                 )
            )
    ,
    EACH quoteqty NO-LOCK WHERE
        quoteqty.company = quoteitm.company
        AND quoteqty.loc = quoteitm.loc 
        AND quoteqty.q-no = quoteitm.q-no 
        AND quoteqty.line = quoteitm.LINE
    BY quotehd.q-no DESC:

    CREATE ttQuoteList.
    ASSIGN 
        ttQuoteList.q-no      = quoteqty.q-no
        ttQuoteList.price     = quoteqty.price
        ttQuoteList.qty       = quoteqty.qty
        ttQuoteList.uom       = quoteqty.uom
        ttQuoteList.est-no    = quotehd.est-no
        ttQuoteList.quote-date = quotehd.quo-date 
        ttQuoteList.rel        = IF quoteqty.rels GT 0 THEN quoteqty.rels ELSE 1
        ttQuoteList.rel-qty    = IF quoteqty.rels GT 0 THEN INT(ROUND(quoteqty.qty / quoteqty.rels ,0)) ELSE quoteqty.qty
        .
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI gDialog  _DEFAULT-DISABLE
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
  HIDE FRAME gDialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayMessage gDialog 
PROCEDURE DisplayMessage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ed_Message = getMessage ("Quoted Prices",NO).
DO WITH FRAME {&FRAME-NAME}:
    DISPLAY ed_Message .
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI gDialog  _DEFAULT-ENABLE
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
  DISPLAY ed_Message 
      WITH FRAME gDialog.
  ENABLE BROWSE-3 btn_price btn_priceqty Btn_Cancel 
      WITH FRAME gDialog.
  VIEW FRAME gDialog.
  {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION checkResults gDialog 
FUNCTION checkResults RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Validate that quotes exist to select
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR lcMsg AS CHAR NO-UNDO.
DEF VAR llQuotesExist AS LOG NO-UNDO.

llQuotesExist = CAN-FIND(FIRST ttQuoteList).
IF NOT llQuotesExist THEN
    MESSAGE  getMessage("No quotes exist", YES)
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

RETURN llQuotesExist.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMessage gDialog 
FUNCTION getMessage RETURNS CHARACTER
  ( ipcMsgStart AS CHAR , iplError AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR lcMsg AS CHAR NO-UNDO.

lcMsg = ipcMsgStart + " for Customer: " + ipxCustNo + CHR(10) +
    "Item #:" + ipxINo.
    IF ipxPartNo NE "" THEN
        lcMsg = lcMsg + " or Customer Part #: " + ipxPartNo.
    lcMsg = lcMsg + CHR(10) + "Estimate: " + ipxEstNo.
    IF NOT quoteForEstimateExists() AND NOT iplError THEN
        lcMsg = lcMsg + 
            " (No quotes available for this estimate. Displaying quotes from any estimates for this customer and item/part number)".
  RETURN lcMsg.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION quoteForEstimateExists gDialog 
FUNCTION quoteForEstimateExists RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Returns logical based on existence of quote for input estimate
    Notes:  
------------------------------------------------------------------------------*/
    
  RETURN CAN-FIND(FIRST quotehd WHERE 
            quotehd.company EQ ipxCompany 
            AND quotehd.cust-no EQ ipxCustNo
            AND quotehd.est-no EQ ipxEstNo).
    

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

