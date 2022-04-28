&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: sharpshooter/b-releasetags.w

  Description: from BROWSER.W - Basic SmartBrowser Object Template

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

&SCOPED-DEFINE exclude-brwCustom

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE gcBOLQtyPopup AS CHARACTER NO-UNDO.

DEFINE VARIABLE hdReleaseProcs AS HANDLE NO-UNDO.

{sharpshooter/ttReleaseTag.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 02/15/21 -  5:38 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cUnitization AS CHARACTER NO-UNDO.

&SCOPED-DEFINE exclude-brwCustom

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttReleaseTag

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table sequenceID tag quantity fGetUnitization() @ cUnitization trailerID itemID itemName orderID lineID jobID jobID2 location bin customerID custPoNo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table   
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH ttReleaseTag BY ttReleaseTag.sequenceID DESC
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH ttReleaseTag BY ttReleaseTag.sequenceID DESC.
&Scoped-define TABLES-IN-QUERY-br_table ttReleaseTag
&Scoped-define FIRST-TABLE-IN-QUERY-br_table ttReleaseTag


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS> 
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE> 
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetUnitization B-table-Win 
FUNCTION fGetUnitization RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      ttReleaseTag SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      sequenceID               COLUMN-LABEL "#"            WIDTH 5  
    tag                      COLUMN-LABEL "Tag #"           
    quantity                 COLUMN-LABEL "Quantity"
    fGetUnitization() @ cUnitization COLUMN-LABEL "Unitization" WIDTH 35 FORMAT "X(40)"       
    trailerID                COLUMN-LABEL "Trailer #"       
    itemID                   COLUMN-LABEL "Item #"          
    itemName                 COLUMN-LABEL "Item Name"       
    orderID                  COLUMN-LABEL "Order #" FORMAT ">>>>>>>9" WIDTH 18        
    lineID                   COLUMN-LABEL "Order!Line #"          
    jobID                    COLUMN-LABEL "Job #" FORMAT "x(9)" WIDTH 25          
    jobID2                   COLUMN-LABEL "Run" FORMAT "999" WIDTH 8            
    location                 COLUMN-LABEL "Location"       
    bin                      COLUMN-LABEL "Bin" WIDTH 15       
    customerID               COLUMN-LABEL "Customer #" WIDTH 20    
    custPoNo                 COLUMN-LABEL "Customer!PO #"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS NO-SCROLLBAR-VERTICAL NO-TAB-STOP SIZE 66 BY 6.71
         FONT 36 ROW-HEIGHT-CHARS 1 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 36 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 6.86
         WIDTH              = 66.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       br_table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttReleaseTag BY ttReleaseTag.sequenceID DESC.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{methods/template/brwcustomSharpShooter.i}

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{sharpshooter/smartobj/browseNavigate.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildReleaseTags B-table-Win 
PROCEDURE BuildReleaseTags :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID AS INTEGER   NO-UNDO.

    EMPTY TEMP-TABLE ttReleaseTag.
    
    RUN BuildReleaseTags IN hdReleaseProcs (
        INPUT  ipcCompany,
        INPUT  ipiReleaseID,
        INPUT-OUTPUT TABLE ttReleaseTag BY-REFERENCE
        ).

    RUN dispatch (
        INPUT "open-query"
        ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateBOLFromRelease B-table-Win 
PROCEDURE CreateBOLFromRelease :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID  AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess    AS LOGICAL   NO-UNDO. 
    DEFINE OUTPUT PARAMETER opcMessage    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiBOLID      AS INTEGER   NO-UNDO.
    
    IF NOT CAN-FIND (FIRST ttReleaseTag) THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "No Tags available to print"
            .
            
        RETURN.
    END.
    
    RUN CreateBOLFromRelease IN hdReleaseProcs (
        INPUT  ipcCompany,
        INPUT  ipiReleaseID,
        INPUT  TRUE, /* Create bol lines from ssrelbol */
        INPUT  USERID("ASI"),
        OUTPUT oplSuccess,
        OUTPUT opcMessage,
        OUTPUT opiBOLID
        ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateReleaseTag B-table-Win 
PROCEDURE CreateReleaseTag :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID        AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTag              AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcReleaseTrailerID AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTagTrailerID     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError            AS LOGICAL   NO-UNDO.   
    DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO. 

    DEFINE VARIABLE cRtnChar            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lValidateQtyExceed  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lSelectReleaseQty   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lPromptQtySelection AS LOGICAL   NO-UNDO.

    IF gcBOLQtyPopup EQ "Prompt" THEN
        ASSIGN
            lValidateQtyExceed = TRUE
            lSelectReleaseQty  = FALSE
            .
    ELSE
        ASSIGN
            lValidateQtyExceed = FALSE
            lSelectReleaseQty  = IF gcBOLQtyPopup EQ "Release" THEN
                                     TRUE
                                 ELSE IF gcBOLQtyPopup EQ "Pallet" THEN
                                     FALSE
                                 ELSE
                                     FALSE
            .
            
    RUN CreateReleaseTag IN hdReleaseProcs (
        INPUT  ipcCompany,
        INPUT  ipiReleaseID,
        INPUT  ipcTag,
        INPUT  ipcReleaseTrailerID,
        INPUT  ipcTagTrailerID,
        INPUT  lValidateQtyExceed,
        INPUT  lSelectReleaseQty,
        OUTPUT oplError,   
        OUTPUT opcMessage
        ).    
    
    /* Prompt only if SSBOLQtyPopup is "Prompt" and scanned quantity exceeded release quantity */
    lPromptQtySelection = system.SharedConfig:Instance:ConsumeValue("ReleaseProcs_QuantityExceededPrompt") EQ "TRUE" AND oplError AND gcBOLQtyPopup EQ "Prompt".
    
    /* Have to send a prompt to user to choose a full pallet quantity or release quantity */
    IF lPromptQtySelection THEN DO:
        MESSAGE  "Tag qty exceeds Scheduled Release Qty " SKIP  
                 "YES to import entire Pallet Quantity for the Selected Tag#." SKIP
                 "NO to import just the Release Quantity for the Selected Tag#."
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE lQtySelection AS LOGICAL.
        
        lSelectReleaseQty = NOT lQtySelection.
            
        RUN CreateReleaseTag IN hdReleaseProcs (
            INPUT  ipcCompany,
            INPUT  ipiReleaseID,
            INPUT  ipcTag,
            INPUT  ipcReleaseTrailerID,
            INPUT  ipcTagTrailerID,
            INPUT  FALSE, /* Validate if quantity exceeded */
            INPUT  lSelectReleaseQty,
            OUTPUT oplError,   
            OUTPUT opcMessage
            ).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteReleaseTag B-table-Win 
PROCEDURE DeleteReleaseTag :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cCompany   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iReleaseID AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lChoice    AS LOGICAL   NO-UNDO.

    IF AVAILABLE ttReleaseTag THEN DO:
        RUN sharpShooter/messageDialog.w (
            "DO YOU WANT TO DELETE SELECTED TAG ('" + ttReleaseTag.tag +  "')?",
            YES,
            YES,
            NO,
            OUTPUT lChoice
            ).
        IF NOT lChoice THEN
            RETURN.            
        ASSIGN
            cCompany   = ttReleaseTag.company
            iReleaseID = ttReleaseTag.releaseID
            .            
        RUN DeleteReleaseTag IN hdReleaseProcs (
            INPUT ttReleaseTag.sourceRowID
            ).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EmptyReleaseTags B-table-Win 
PROCEDURE EmptyReleaseTags :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttReleaseTag.
    
    RUN dispatch (
        INPUT "open-query"
        ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy B-table-Win 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    /* Code placed here will execute PRIOR to standard behavior. */
    IF VALID-HANDLE(hdReleaseProcs) THEN
        DELETE PROCEDURE hdReleaseProcs.
        
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable B-table-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    RUN pInit.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit B-table-Win 
PROCEDURE pInit PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pHandle  AS HANDLE    NO-UNDO.
    
    RUN oe/ReleaseProcs.p PERSISTENT SET hdReleaseProcs.
    
    RUN spGetSettingByName("SSBOLQuantitySelection", OUTPUT gcBOLQtyPopup).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "ttReleaseTag"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
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
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetUnitization B-table-Win 
FUNCTION fGetUnitization RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    IF AVAILABLE ttReleaseTag THEN
        RETURN TRIM(STRING(ttReleaseTag.quantityOfSubUnits, "->>>>>>>>9")) + " @ "
             + TRIM(STRING(ttReleaseTag.quantityInSubUnit, "->>>>>>>>9"))
             + IF ttReleaseTag.quantityPartial EQ 0 THEN 
                   "" 
               ELSE 
                   (" + " + TRIM(STRING(ttReleaseTag.quantityPartial, "->>>>>>>>9"))).
    ELSE    
        RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

