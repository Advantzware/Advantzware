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

  File: oe/b-set-estitem.w

  Description: 

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

DEFINE VARIABLE cEmptyColumn   AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCompany       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation      AS CHARACTER NO-UNDO.
DEFINE VARIABLE char-hdl       AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle        AS HANDLE    NO-UNDO.
DEFINE VARIABLE lSelectTrigger AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iLine          AS INTEGER   NO-UNDO.

RUN spGetSessionParam("Company", OUTPUT cCompany).
RUN spGetSessionParam("Location", OUTPUT cLocation).
           
DEFINE VARIABLE giQuoteNumber AS INTEGER NO-UNDO.           
           
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
&Scoped-define INTERNAL-TABLES ttEstItem       

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table ttEstItem.isSelected ttEstItem.estLine ttEstItem.estCust ttEstItem.estItem ttEstItem.estPart ttEstItem.estDesc ttEstItem.estQty ttEstItem.estQtyUom ttEstItem.estPrice ttEstItem.estPrUom ttEstItem.estPo ttEstItem.estTotal ttEstItem.estQuote ttEstItem.estPriceMatrix cEmptyColumn 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table ttEstItem.isSelected ttEstItem.estQty ttEstItem.estQtyUom  
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table ttEstItem
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table ttEstItem
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH ttEstItem WHERE ttEstItem.Company = cCompany ~         ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH ttEstItem WHERE ttEstItem.Company = cCompany ~         ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table ttEstItem
&Scoped-define FIRST-TABLE-IN-QUERY-br_table ttEstItem


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
    ttEstItem SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
    QUERY br_table NO-LOCK DISPLAY
    ttEstItem.isSelected COLUMN-LABEL '[]'  VIEW-AS TOGGLE-BOX 
    ttEstItem.estLine  WIDTH 5 FORMAT ">9"
    ttEstItem.estCust LABEL-BGCOLOR 14 FORMAT "x(8)"
    ttEstItem.estItem FORMAT "x(15)" WIDTH 21 
    ttEstItem.estPart FORMAT "x(15)" WIDTH 18 
    ttEstItem.estDesc FORMAT "x(30)" WIDTH 32 
    ttEstItem.estQty FORMAT "->>,>>>,>>9" WIDTH 16 
    ttEstItem.estQtyUom  WIDTH 8 
    ttEstItem.estPrice FORMAT "->>,>>>,>>9.99" WIDTH 16 
    ttEstItem.estPrUom WIDTH 8 
    ttEstItem.estPo FORMAT "x(15)" WIDTH 18 
    ttEstItem.estTotal FORMAT "->>,>>>,>>>,>>9.99" WIDTH 21 
    ttEstItem.estQuote WIDTH 22 FORMAT "->>>,>>>,>>9.99" 
    ttEstItem.estPriceMatrix WIDTH 15 FORMAT "Yes/No"
    cEmptyColumn COLUMN-LABEL ""
    ENABLE ttEstItem.isSelected
           ttEstItem.estQty
           ttEstItem.estQtyUom
      
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
OPEN QUERY {&SELF-NAME} FOR EACH ttEstItem
WHERE ttEstItem.company EQ cCompany NO-LOCK
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

&Scoped-define BROWSE-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttEstItem.isSelected br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF ttEstItem.isSelected IN BROWSE br_table /* select */
    DO:  
        ASSIGN 
            ttEstItem.isSelected = LOGICAL(ttEstItem.isSelected:SCREEN-VALUE IN BROWSE {&browse-name}).
           
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttEstItem.estQty br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF ttEstItem.estQty IN BROWSE br_table /* qty */
    DO:
        ASSIGN 
            ttEstItem.estQty = INTEGER(ttEstItem.estQty:SCREEN-VALUE IN BROWSE {&browse-name}).
            
        RUN Conv_CalcTotalPrice(cCompany,            
            ttEstItem.estItem,
            DECIMAL(ttEstItem.estQty),
            DECIMAL(ttEstItem.estPrice),
            ttEstItem.estPrUom,
            0,
            0,    
            OUTPUT ttEstItem.estTotal).  
        ttEstItem.estTotal:SCREEN-VALUE IN BROWSE {&browse-name} =  string(ttEstItem.estTotal).               
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME 

&Scoped-define BROWSE-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttEstItem.estQtyUom br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF ttEstItem.estQtyUom IN BROWSE br_table /* qty-uom */
    DO:
        ASSIGN 
            ttEstItem.estQtyUom = ttEstItem.estQtyUom:SCREEN-VALUE IN BROWSE {&browse-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define BROWSE-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttEstItem.estQty br_table _BROWSE-COLUMN B-table-Win
ON HELP OF ttEstItem.estQty IN BROWSE br_table /* qty */
    DO:
        DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.
        IF giQuoteNumber GT 0 THEN
        DO:
            RUN windows/l-ordqty.w (cCompany, ttEstItem.estNo, "", OUTPUT cReturnValue).
        END.
        ELSE 
        DO:
            RUN windows/l-estqty.w (cCompany, ttEstItem.estNo, OUTPUT cReturnValue).
        END.
        IF cReturnValue NE "" THEN
            ttEstItem.estQty:SCREEN-VALUE IN BROWSE {&browse-name} =   ENTRY(1,cReturnValue).       
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON START-SEARCH OF br_table IN FRAME F-Main
    DO:  
        IF SELF:CURRENT-COLUMN:NAME EQ "isSelected" THEN 
        DO:
            lSelectTrigger = NOT lSelectTrigger.
                     
            FOR EACH ttEstItem:
                ttEstItem.isSelected = lSelectTrigger.  
            END.

            SELF:CURRENT-COLUMN:LABEL = IF lSelectTrigger THEN
                "[*] "
                ELSE
                "[ ] ".

            DO WITH FRAME {&FRAME-NAME}:
         
                {&open-query-{&browse-name}}
            END. 
        END.               
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
    {src/adm/template/row-list.i "ttEstItem"}

    /* Get the record ROWID's from the RECORD-SOURCE.                  */
    {src/adm/template/row-get.i}

    /* FIND each record specified by the RECORD-SOURCE.                */
    {src/adm/template/row-find.i "ttEstItem"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetItem B-table-Win 
PROCEDURE GetItem :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcItemID  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcJobNo   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiJobNo2  AS INTEGER   NO-UNDO.    
    DEFINE OUTPUT PARAMETER oplAvail   AS LOGICAL   NO-UNDO.
    
    IF AVAILABLE job-mat THEN 
    DO:
        ASSIGN
            opcCompany = job-mat.company
            opcItemID  = job-mat.i-no
            opcJobNo   = job-mat.job-no
            opiJobNo2  = job-mat.job-no2
            oplAvail   = TRUE
            .
    END.
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
    {src/adm/template/snd-list.i "ttEstItem"}    

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
    DEFINE INPUT PARAMETER ipcSourceValue AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomerPo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdPrice AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcPrUom AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiQty AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiQuoteNumber AS INTEGER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttEstItem.

    DEFINE VARIABLE lTaxable           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iLevel             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lMatrixExists      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lQtyDistinctMatch  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lQtyWithinRange    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMatrixMatchDetail AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ttEstItem FOR ttEstItem.
    
    DO WITH FRAME {&FRAME-NAME}:
    
        FIND FIRST bf-ttEstItem NO-LOCK 
            WHERE TRIM(bf-ttEstItem.estNo) EQ trim(ipcSourceValue) NO-ERROR .                        
               
        giQuoteNumber = ipiQuoteNumber .
        IF AVAILABLE bf-ttEstItem THEN 
        DO: 
            {&open-query-{&browse-name}}
            RETURN.
        END.
              
        FIND FIRST est NO-LOCK
            WHERE est.company EQ cCompany
            AND est.est-no  EQ FILL(" ",8 - LENGTH(TRIM(ipcSourceValue))) + TRIM(ipcSourceValue)
            NO-ERROR.
     
        IF AVAILABLE est THEN
        DO:            
            EMPTY TEMP-TABLE ttEstItem .
            iLine = 0.   
            FOR EACH eb NO-LOCK
                WHERE eb.company EQ cCompany
                AND eb.est-no EQ est.est-no:
                iLine = iLine + 1.
                            
                CREATE ttEstItem.
                ASSIGN
                    ttEstItem.company   = cCompany
                    ttEstItem.estLine   = iLine
                    ttEstItem.estCust   = eb.cust-no
                    ttEstItem.estShipId = eb.ship-id
                    ttEstItem.estItem   = eb.stock-no
                    ttEstItem.estPart   = eb.part-no
                    ttEstItem.estDesc   = eb.part-dscr1
                    ttEstItem.estQty    = eb.eqty
                    ttEstItem.estQtyUom = "EA"
                    ttEstItem.estTotal  = 0  
                    ttEstItem.estRowid  = ROWID(eb) 
                    ttEstItem.estNo     = eb.est-no
                    .
                  
                FIND FIRST itemfg NO-LOCK 
                    WHERE itemfg.company EQ eb.company
                    AND itemfg.i-no    EQ eb.stock-no
                    NO-ERROR.
     
                IF AVAILABLE itemfg THEN 
                DO:                      
                    RUN Price_GetPriceMatrixLevel(cCompany, eb.stock-no, eb.cust-no, eb.ship-id, eb.eqty, OUTPUT iLevel).
                    ASSIGN               
                        ttEstItem.estPriceMatrix = IF iLevel NE 0 THEN YES ELSE NO
                        ttEstItem.estPrUom       = itemfg.sell-uom
                        ttEstItem.estPo          = ipcCustomerPo
                        ttEstItem.estPrice       = itemfg.sell-price 
                        . 
                END. 
                
                IF ipiQuoteNumber GT 0 THEN
                DO:
                    ASSIGN
                        ttEstItem.estQty    = ipiQty
                        ttEstItem.estQtyUom = "EA" 
                        ttEstItem.estPrUom  = ipcPrUom
                        ttEstItem.estPrice  = ipdPrice
                        ttEstItem.estQuote  = ipiQuoteNumber.
                END.
                ELSE 
                DO:
                    RUN Price_GetPriceMatrixPrice (cCompany, ttEstItem.estItem, eb.cust-no, eb.ship-id, ttEstItem.estQty, 0,
                        OUTPUT lMatrixExists, OUTPUT cMatrixMatchDetail, INPUT-OUTPUT ttEstItem.estPrice, INPUT-OUTPUT ttEstItem.estPrUom,
                        OUTPUT lQtyDistinctMatch, OUTPUT lQtyWithinRange).
                END.
             
                
                RUN Conv_CalcTotalPrice(eb.company,            
                    ttEstItem.estItem,
                    DECIMAL(ttEstItem.estQty),
                    DECIMAL(ttEstItem.estPrice),
                    ttEstItem.estPrUom,
                    0,
                    0,    
                    OUTPUT ttEstItem.estTotal).               
            END.
            
            FOR EACH est-prep WHERE est-prep.company EQ est.company
                AND est-prep.est-no EQ est.est-no
                AND est-prep.simon EQ "S" 
                AND est-prep.orderID EQ ""  NO-LOCK :
                      
                FIND FIRST ar-ctrl WHERE ar-ctrl.company EQ est.company NO-LOCK NO-ERROR.
                FIND FIRST prep NO-LOCK
                    WHERE prep.company EQ est.company 
                    AND prep.code = est-prep.CODE NO-ERROR.
                           
                iLine = iLine + 1. 
                      
                CREATE ttEstItem.
                ASSIGN
                    ttEstItem.company   = cCompany
                    ttEstItem.estLine   = iLine
                    ttEstItem.estCust   = ""
                    ttEstItem.estShipId = ""
                    ttEstItem.estItem   = est-prep.code
                    ttEstItem.estPart   = ""
                    ttEstItem.estDesc   = IF est-prep.dscr <> "" THEN est-prep.dscr ELSE prep.dscr
                    ttEstItem.estQty    = est-prep.qty 
                    ttEstItem.estPrice  = est-prep.cost
                    ttEstItem.estQtyUom = "EA"
                    ttEstItem.estTotal  = 0 
                    ttEstItem.estPrUom  = "EA"
                    ttEstItem.estRowid  = ROWID(est-prep) 
                    ttEstItem.estNo     = est.est-no
                    .
                    
                RUN Conv_CalcTotalPrice(cCompany,            
                    ttEstItem.estItem,
                    DECIMAL(ttEstItem.estQty),
                    DECIMAL(ttEstItem.estPrice),
                    ttEstItem.estPrUom,
                    0,
                    0,    
                    OUTPUT ttEstItem.estTotal).
            END.  
          
        END.
        {&open-query-{&browse-name}}  
    END.   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildTableForReOrder B-table-Win 
PROCEDURE pBuildTableForReOrder:
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcSourceValue AS CHARACTER NO-UNDO.    
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttEstItem.

    DEFINE VARIABLE lTaxable           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iLevel             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lMatrixExists      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lQtyDistinctMatch  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lQtyWithinRange    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMatrixMatchDetail AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ttEstItem FOR ttEstItem.
    
    DO WITH FRAME {&FRAME-NAME}:
    
        FIND FIRST bf-ttEstItem NO-LOCK 
            WHERE bf-ttEstItem.ordNo EQ INTEGER(ipcSourceValue) NO-ERROR .                        
               
       // giQuoteNumber = ipiQuoteNumber .
        IF AVAILABLE bf-ttEstItem THEN 
        DO: 
            {&open-query-{&browse-name}}
            RETURN.
        END.
              
        FIND FIRST oe-ord NO-LOCK
            WHERE oe-ord.company EQ cCompany
            AND oe-ord.ord-no  EQ INTEGER(ipcSourceValue)
            NO-ERROR.
     
        IF AVAILABLE oe-ord THEN
        DO:            
            EMPTY TEMP-TABLE ttEstItem .
            iLine = 0.   
            FOR EACH oe-ordl NO-LOCK
                WHERE oe-ordl.company EQ cCompany
                AND oe-ordl.ord-no EQ oe-ord.ord-no:
                iLine = iLine + 1.
                            
                CREATE ttEstItem.
                ASSIGN
                    ttEstItem.company   = cCompany
                    ttEstItem.estLine   = iLine
                    ttEstItem.estCust   = oe-ordl.cust-no
                    ttEstItem.estShipId = oe-ordl.ship-id
                    ttEstItem.estItem   = oe-ordl.i-no
                    ttEstItem.estPart   = oe-ordl.part-no
                    ttEstItem.estDesc   = oe-ordl.part-dscr1
                    ttEstItem.estQty    = oe-ordl.qty
                    ttEstItem.estQtyUom = "EA"
                    ttEstItem.estTotal  = 0  
                    ttEstItem.estRowid  = ROWID(oe-ordl) 
                    ttEstItem.estNo     = oe-ordl.est-no
                    ttEstItem.ordNo     = oe-ordl.ord-no
                    .
                  
                FIND FIRST itemfg NO-LOCK 
                    WHERE itemfg.company EQ oe-ordl.company
                    AND itemfg.i-no    EQ oe-ordl.i-no
                    NO-ERROR.
     
               /* IF AVAILABLE itemfg THEN 
                DO:                      
                    RUN Price_GetPriceMatrixLevel(cCompany, eb.stock-no, eb.cust-no, eb.ship-id, eb.eqty, OUTPUT iLevel).
                    ASSIGN               
                        ttEstItem.estPriceMatrix = IF iLevel NE 0 THEN YES ELSE NO
                        ttEstItem.estPrUom       = itemfg.sell-uom
                        ttEstItem.estPo          = ipcCustomerPo
                        ttEstItem.estPrice       = itemfg.sell-price 
                        . 
                END. 
                
                IF ipiQuoteNumber GT 0 THEN
                DO:
                    ASSIGN
                        ttEstItem.estQty    = ipiQty
                        ttEstItem.estQtyUom = "EA" 
                        ttEstItem.estPrUom  = ipcPrUom
                        ttEstItem.estPrice  = ipdPrice
                        ttEstItem.estQuote  = ipiQuoteNumber.
                END.
                ELSE 
                DO:
                    RUN Price_GetPriceMatrixPrice (cCompany, ttEstItem.estItem, eb.cust-no, eb.ship-id, ttEstItem.estQty, 0,
                        OUTPUT lMatrixExists, OUTPUT cMatrixMatchDetail, INPUT-OUTPUT ttEstItem.estPrice, INPUT-OUTPUT ttEstItem.estPrUom,
                        OUTPUT lQtyDistinctMatch, OUTPUT lQtyWithinRange).
                END.
             
                
                RUN Conv_CalcTotalPrice(eb.company,            
                    ttEstItem.estItem,
                    DECIMAL(ttEstItem.estQty),
                    DECIMAL(ttEstItem.estPrice),
                    ttEstItem.estPrUom,
                    0,
                    0,    
                    OUTPUT ttEstItem.estTotal).               
            END.  */
            
          END.   
          
        END.
        {&open-query-{&browse-name}}  
    END.   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query B-table-Win 
PROCEDURE repo-query :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprwRowid AS ROWID NO-UNDO.     
    

    CLOSE QUERY br_table.
    DO WITH FRAME {&FRAME-NAME}:
         
        OPEN QUERY br_table FOR EACH ttEstItem
            NO-LOCK BY ttEstItem.estLine.              

        REPOSITION {&browse-name} TO ROWID iprwRowid NO-ERROR.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetTable B-table-Win 
PROCEDURE pGetTable :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    
    DEFINE OUTPUT PARAMETER TABLE FOR ttEstItem.
END.     
     




