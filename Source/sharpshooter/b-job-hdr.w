&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: sharpshooter\b-job-hdr.w

  Description: SmartBrowser for job-hdr table

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
{inventory/ttInventory.i "NEW SHARED"}
{jc/jcgl-sh.i  NEW}

DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobNo   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iJobNo2  AS INTEGER   NO-UNDO.

DEFINE VARIABLE hdFGInquiry    AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdFGInquiryWin AS HANDLE    NO-UNDO.

DEFINE VARIABLE lHasAccess AS LOGICAL NO-UNDO.
DEFINE VARIABLE iTotalRcvd AS INTEGER NO-UNDO.

DEFINE VARIABLE hdPgmSecurity AS HANDLE  NO-UNDO.
RUN system/PgmMstrSecur.p PERSISTENT SET hdPgmSecurity.

DEFINE VARIABLE hdInventoryProcs AS HANDLE  NO-UNDO.
RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.

RUN epCanAccess IN hdPgmSecurity (
    INPUT  "sharpshooter/b-fgInqBins.w", 
    INPUT  "", 
    OUTPUT lHasAccess
    ).
    
DELETE OBJECT hdPgmSecurity.

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
&Scoped-define INTERNAL-TABLES job-hdr job itemfg

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table job-hdr.cust-no job-hdr.i-no job-hdr.qty fGetTotalReceived() @ iTotalRcvd itemfg.q-onh   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table   
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH job-hdr NO-LOCK       WHERE job-hdr.company EQ cCompany         AND job-hdr.job-no  EQ cJobNo         AND job-hdr.job-no2 EQ iJobNo2, ~
             FIRST job       WHERE job.company EQ job-hdr.company         AND job.job     EQ job-hdr.job         AND job.job-no  EQ job-hdr.job-no         AND job.job-no2 EQ job-hdr.job-no2, ~
             FIRST itemfg       WHERE itemfg.company EQ job-hdr.company         AND itemfg.i-no    EQ job-hdr.i-no     ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH job-hdr NO-LOCK       WHERE job-hdr.company EQ cCompany         AND job-hdr.job-no  EQ cJobNo         AND job-hdr.job-no2 EQ iJobNo2, ~
             FIRST job       WHERE job.company EQ job-hdr.company         AND job.job     EQ job-hdr.job         AND job.job-no  EQ job-hdr.job-no         AND job.job-no2 EQ job-hdr.job-no2, ~
             FIRST itemfg       WHERE itemfg.company EQ job-hdr.company         AND itemfg.i-no    EQ job-hdr.i-no     ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table job-hdr job itemfg
&Scoped-define FIRST-TABLE-IN-QUERY-br_table job-hdr
&Scoped-define SECOND-TABLE-IN-QUERY-br_table job
&Scoped-define THIRD-TABLE-IN-QUERY-br_table itemfg


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetTotalReceived B-table-Win 
FUNCTION fGetTotalReceived RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      job-hdr, 
      job, 
      itemfg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      job-hdr.cust-no FORMAT "x(8)":U WIDTH 30
      job-hdr.i-no FORMAT "x(15)":U WIDTH 40
      job-hdr.qty COLUMN-LABEL "Job Quantity" FORMAT "->>,>>>,>>9":U WIDTH 36
      fGetTotalReceived() @ iTotalRcvd COLUMN-LABEL "Job Qty Received" FORMAT "->>,>>>,>>9":U WIDTH 36
      itemfg.q-onh COLUMN-LABEL "Total On-Hand" FORMAT "->>,>>>,>>9":U WIDTH 40
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 181 BY 18.48
         FONT 19 ROW-HEIGHT-CHARS 1.05 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


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
         HEIGHT             = 18.62
         WIDTH              = 181.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}

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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH job-hdr NO-LOCK
      WHERE job-hdr.company EQ cCompany
        AND job-hdr.job-no  EQ cJobNo
        AND job-hdr.job-no2 EQ iJobNo2,
      FIRST job
      WHERE job.company EQ job-hdr.company
        AND job.job     EQ job-hdr.job
        AND job.job-no  EQ job-hdr.job-no
        AND job.job-no2 EQ job-hdr.job-no2,
      FIRST itemfg
      WHERE itemfg.company EQ job-hdr.company
        AND itemfg.i-no    EQ job-hdr.i-no
    ~{&SORTBY-PHRASE}.
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

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AdjustQuantity B-table-Win 
PROCEDURE AdjustQuantity :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dTotalQuantity   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dSubUnitCount    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dSubUnitsPerUnit AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dPartialQuantity AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cAdjReasonCode   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValueReturned   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cAdjustType      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dValue           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lError           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iProdQty         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dQtyToAdjust     AS DECIMAL   NO-UNDO.

    IF AVAILABLE job-hdr AND AVAILABLE itemfg THEN DO:
        /* If not automatically cleared by security level, ask for password */
        IF NOT lHasAccess THEN DO:
            RUN sys/ref/d-passwd.w (
                INPUT  10, 
                OUTPUT lHasAccess
                ). 
        END.
    
        IF NOT lHasAccess THEN
            RETURN.

        iProdQty = fGetTotalReceived().
        
        RUN inventory/adjustQuantityWithType.w (
            INPUT  itemfg.i-no,
            INPUT  itemfg.i-name,
            INPUT  IF job-hdr.qty LT 0 THEN 0 ELSE job-hdr.qty,
            INPUT  iProdQty,
            INPUT  IF itemfg.q-onh LT 0 THEN 0 ELSE itemfg.q-onh,
            INPUT  1,
            INPUT  1,
            INPUT  TRUE, /* Required Adj Reason  */
            INPUT  FALSE,  /* Allow decimal units */
            OUTPUT dTotalQuantity,
            OUTPUT dSubUnitCount,
            OUTPUT dSubUnitsPerUnit,
            OUTPUT dPartialQuantity,
            OUTPUT cAdjustType,
            OUTPUT cAdjReasonCode,
            OUTPUT lValueReturned,
            OUTPUT dValue
            ).
  
        IF lValueReturned THEN DO: 
            IF cAdjustType EQ "Reduce" THEN
                ASSIGN
                    dQtyToAdjust = -1 * dTotalQuantity 
                    cMessage     = "Reduce " + STRING(dTotalQuantity) + " quantity from total on-hand " + STRING(itemfg.q-onh) + "?"
                    .
            ELSE IF cAdjustType EQ "Add" THEN
                ASSIGN
                    dQtyToAdjust = dTotalQuantity 
                    cMessage     = "Add " + STRING(dTotalQuantity) + " quantity to total on-hand " + STRING(itemfg.q-onh) + "?"
                    .
            IF cAdjustType EQ "Count" THEN
                ASSIGN
                    dQtyToAdjust = dTotalQuantity - itemfg.q-onh 
                    cMessage     = "Adjust total on-hand to " + STRING(dTotalQuantity) + "?"
                    .
            
            IF dQtyToAdjust EQ 0 THEN DO:
                MESSAGE "Cannot adjust zero quantity value"
                    VIEW-AS ALERT-BOX ERROR.
                RETURN.    
            END.
            
            MESSAGE cMessage 
                    VIEW-AS ALERT-BOX QUESTION
                    BUTTON OK-CANCEL
                    TITLE "Adjust Quantity" UPDATE lContinue AS LOGICAL.
            IF lContinue THEN DO:
                RUN Inventory_FGQuantityAdjust IN hdInventoryProcs (
                    INPUT  job-hdr.company,
                    INPUT  job-hdr.i-no,
                    INPUT  job-hdr.loc,
                    INPUT  "",
                    INPUT  job-hdr.job-no,
                    INPUT  job-hdr.job-no2,
                    INPUT  dQtyToAdjust,
                    INPUT  cAdjReasonCode,
                    OUTPUT lError,
                    OUTPUT cMessage
                    ).

                IF lError THEN
                    MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.

                {&OPEN-QUERY-{&BROWSE-NAME}}

                APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOpenQuery B-table-Win 
PROCEDURE pOpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2  AS INTEGER   NO-UNDO.

    ASSIGN
        cCompany = ipcCompany
        cJobNo   = ipcJobNo
        iJobNo2  = ipiJobNo2
        .
        
    RUN dispatch ('open-query').
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
  {src/adm/template/snd-list.i "job-hdr"}
  {src/adm/template/snd-list.i "job"}
  {src/adm/template/snd-list.i "itemfg"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ViewFGInquiry B-table-Win 
PROCEDURE ViewFGInquiry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    IF NOT AVAILABLE job-hdr THEN
        RETURN.
        
    IF NOT VALID-HANDLE(hdFGInquiry) THEN DO:         
        RUN sharpshooter/w-fgInquiry.w PERSISTENT SET hdFGInquiry.

        RUN dispatch IN hdFGInquiry (
            INPUT 'initialize':U
            ) NO-ERROR.
        
        hdFGInquiryWin = hdFGInquiry:CURRENT-WINDOW.
    END.
                                                 
    IF VALID-HANDLE(hdFGInquiry) AND
        VALID-HANDLE(hdFGInquiryWin) THEN DO: 

        RUN ScanItem IN hdFGInquiry (
            INPUT job-hdr.company,
            INPUT "",
            INPUT "",
            INPUT job-hdr.i-no,
            INPUT "",
            INPUT "",
            INPUT 0
            ) NO-ERROR.            

        IF hdFGInquiryWin:WINDOW-STATE EQ 2 THEN ASSIGN 
            hdFGInquiryWin:WINDOW-STATE = 3.
        
        hdFGInquiryWin:MOVE-TO-TOP().
    END.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetTotalReceived B-table-Win 
FUNCTION fGetTotalReceived RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iJobProdQty AS INTEGER   NO-UNDO.
    
    IF AVAILABLE job-hdr THEN DO:
        RUN fg/GetProductionQty.p (
            INPUT  job-hdr.company,
            INPUT  job-hdr.job-no,
            INPUT  job-hdr.job-no2,
            INPUT  job-hdr.i-no,
            INPUT  NO,
            OUTPUT iJobProdQty
            ).
    END.

    RETURN iJobProdQty.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

