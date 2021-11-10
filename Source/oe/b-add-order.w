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

  File: oe/b-add-order.w

  Description: SmartBrowser for job-mat table

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
{custom/globdefs.i}

{oe\ttInputOrd.i}

DEFINE VARIABLE cEmptyColumn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCompany     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation    AS CHARACTER NO-UNDO.
DEFINE VARIABLE char-hdl     AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle      AS HANDLE    NO-UNDO.

RUN spGetSessionParam("Company", OUTPUT cCompany).
RUN spGetSessionParam("Location", OUTPUT cLocation).
           
           
DEFINE VARIABLE v-duelist      AS cha     INIT "AM,ASAP,BY,CPU,CR,HFR,HOLD,HOT,INK,MH,MUST,NB4,NCUST,NITEM,NCNI,OE,ON,PPR,RWRK,RUSH,TOOL,WO,$$$" NO-UNDO. /* Task 04081403 */
DEFINE VARIABLE ll-valid-po-no AS LOG     NO-UNDO.
DEFINE VARIABLE lOeprompt      AS LOGICAL NO-UNDO.
DEFINE VARIABLE cRtnChar       AS LOGICAL NO-UNDO.
DEFINE VARIABLE lRecFound      AS LOGICAL NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cCompany, "OEPROMPT", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lOeprompt = LOGICAL(cRtnChar) NO-ERROR.
    
DEFINE VARIABLE hOrderEntryProcs AS HANDLE NO-UNDO. 
RUN oe/OrderEntryProcs.p PERSISTENT SET hOrderEntryProcs.

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
&Scoped-define INTERNAL-TABLES ttInputOrdLine

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table ttInputOrdLine.line ttInputOrdLine.cItemType ttInputOrdLine.est-no ttInputOrdLine.i-no ttInputOrdLine.part-no ttInputOrdLine.i-name ttInputOrdLine.qty ttInputOrdLine.cQtyUom ttInputOrdLine.price ttInputOrdLine.pr-uom ttInputOrdLine.po-no ttInputOrdLine.t-price ttInputOrdLine.tax ttInputOrdLine.e-num  ttInputOrdLine.lCreateRel ttInputOrdLine.lCreateJob ttInputOrdLine.lCreatePo
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table   
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH ttInputOrdLine WHERE ttInputOrdLine.Company = cCompany ~         ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH ttInputOrdLine WHERE ttInputOrdLine.Company = cCompany ~         ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table ttInputOrdLine
&Scoped-define FIRST-TABLE-IN-QUERY-br_table ttInputOrdLine


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


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
     
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
    ttInputOrdLine SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
    QUERY br_table NO-LOCK DISPLAY
    ttInputOrdLine.line LABEL "Lin" WIDTH 8 LABEL-BGCOLOR 14 FORMAT ">9"
    ttInputOrdLine.cItemType LABEL "Item Type"  LABEL-BGCOLOR 14 FORMAT "x(10)"
    ttInputOrdLine.est-no LABEL "Estimate" FORMAT "x(8)" WIDTH 15 LABEL-BGCOLOR 14
    ttInputOrdLine.i-no LABEL "Item Id/Misc" FORMAT "x(15)" WIDTH 25 LABEL-BGCOLOR 14
    ttInputOrdLine.part-no LABEL "Customer Part" FORMAT "x(15)" WIDTH 25 LABEL-BGCOLOR 14
    ttInputOrdLine.i-name LABEL "Description" FORMAT "x(30)" WIDTH 40 LABEL-BGCOLOR 14
    ttInputOrdLine.qty LABEL "Qty" FORMAT ">>>,>>>,>>9" WIDTH 18 LABEL-BGCOLOR 14
    ttInputOrdLine.cQtyUom LABEL "Uom" WIDTH 9 LABEL-BGCOLOR 14
    ttInputOrdLine.price LABEL "Price" WIDTH 16 LABEL-BGCOLOR 14
    ttInputOrdLine.pr-uom LABEL "Uom" FORMAT "x(3)" WIDTH 8 LABEL-BGCOLOR 14
    ttInputOrdLine.po-no LABEL "PO #" FORMAT "x(15)" WIDTH 22 LABEL-BGCOLOR 14
    ttInputOrdLine.t-price LABEL "Total" FORMAT "->>>,>>>,>>9.99" LABEL-BGCOLOR 14
    ttInputOrdLine.tax LABEL "Tax" WIDTH 6 FORMAT "Yes/No" LABEL-BGCOLOR 14
    ttInputOrdLine.e-num LABEL "POLine" WIDTH 10 FORMAT ">9" LABEL-BGCOLOR 14
    ttInputOrdLine.lCreateRel LABEL "Create Release " WIDTH 20 FORMAT "Yes/No" LABEL-BGCOLOR 14
    ttInputOrdLine.lCreateJob LABEL "Create Job " WIDTH 20 FORMAT "Yes/No" LABEL-BGCOLOR 14
    ttInputOrdLine.lCreatePo LABEL "Create Po " WIDTH 20 FORMAT "Yes/No" LABEL-BGCOLOR 14
    cEmptyColumn COLUMN-LABEL ""
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 178 BY 18.48
         FONT 36 ROW-HEIGHT-CHARS .95 FIT-LAST-COLUMN.


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
   External Tables: ASI.job
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN 
DO:
    MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 18.48
         WIDTH              = 178.6.
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
    FRAME F-Main:SCROLLABLE = FALSE
    FRAME F-Main:HIDDEN     = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttInputOrdLine
WHERE ttInputOrdLine.company EQ cCompany NO-LOCK
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
{methods/template/brwcustomSharpShooter.i}

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

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

    /* Create a list of all the tables that we need to get.            */
    {src/adm/template/row-list.i "ttInputOrdLine"}

    /* Get the record ROWID's from the RECORD-SOURCE.                  */
    {src/adm/template/row-get.i}

    /* FIND each record specified by the RECORD-SOURCE.                */
    {src/adm/template/row-find.i "ttInputOrdLine"}

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValueChanged B-table-Win 
PROCEDURE pValueChanged :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&frame-name}:  
        APPLY "value-changed" TO BROWSE {&browse-name}.       
    END.
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
    {src/adm/template/snd-list.i "ttInputOrdLine"}    

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildTable B-table-Win 
PROCEDURE pBuildTable :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER TABLE FOR ttEstItem.
    DEFINE INPUT PARAMETER ipcSourceValue AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomerPo AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdtDueDate AS DATE NO-UNDO.
    DEFINE OUTPUT PARAMETER opiOver AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiUnder AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE iLine    AS INTEGER NO-UNDO.
    DEFINE VARIABLE lTaxable AS LOGICAL NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:  
     
        EMPTY TEMP-TABLE ttInputOrdLine.                       
        EMPTY TEMP-TABLE ttInputOrd.
      
        RUN OrderEntry_GetEstDetail IN hOrderEntryProcs(INPUT TABLE ttEstItem, INPUT cCompany, INPUT ipcSourceValue, OUTPUT TABLE ttInputOrdLine, OUTPUT TABLE ttInputOrd ).
            
        {&open-query-{&browse-name}}  
      
        FIND FIRST ttInputOrd NO-LOCK NO-ERROR.
        IF AVAILABLE ttInputOrd THEN 
            ASSIGN
                opdtDueDate = ttInputOrd.due-date
                opiOver     = ttInputOrd.over-pct
                opiUnder    = ttInputOrd.Under-pct.      
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetOrderTempTable B-table-Win 
PROCEDURE pGetOrderTempTable :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttInputOrd.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateTempTable B-table-Win 
PROCEDURE pUpdateTempTable :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER TABLE FOR ttInputOrd.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetOrderLineTempTable B-table-Win 
PROCEDURE pGetOrderLineTempTable :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttInputOrdLine.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
