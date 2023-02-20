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

  File: sharpshooter/b-job-mat.w

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
/*  Mod: Ticket - 103137 Format Change for Order No. and Job No.       */     

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE exclude-brwCustom

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/var.i "NEW SHARED"}
{sys/inc/varasgn.i}

DEFINE VARIABLE cCompany       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobNo         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iJobNo2        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iFormNo        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iBlankNo       AS INTEGER   NO-UNDO.
DEFINE VARIABLE cEmptyColumn   AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdRMInquiry    AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdRMInquiryWin AS HANDLE    NO-UNDO. 
DEFINE VARIABLE lReturnError   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE char-hdl       AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle        AS HANDLE    NO-UNDO.
DEFINE VARIABLE iAvailQty      AS INTEGER   NO-UNDO.
DEFINE VARIABLE dLinearFeet    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE iFGQty         AS INTEGER   NO-UNDO.
DEFINE VARIABLE dTons          AS DECIMAL NO-UNDO.
DEFINE VARIABLE cCreateSBNote  AS CHARACTER NO-UNDO.

RUN spGetSettingByName ("CreateSBNote", OUTPUT cCreateSBNote).

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
&Scoped-define INTERNAL-TABLES job-mat job item

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table job-mat.frm job-mat.blank-no job-mat.rm-i-no item.i-dscr getAvailQty() @ iAvailQty job-mat.qty-all job-mat.qty-uom job-mat.all-flg ~
fGetLinearFeet() @ dLinearFeet item.s-len item.s-wid item.cal fGetFGQty() @ iFGQty cEmptyColumn fGetTons () @ dTons    
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table job-mat
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table job-mat
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH job-mat WHERE job-mat.company EQ cCompany and job-mat.job-no  EQ cJobNo   AND job-mat.job-no2 EQ iJobNo2 AND (job-mat.frm      EQ iFormNo OR iFormNo EQ ?)         AND (job-mat.blank-no EQ iBlankNo OR iBlankNo EQ ?) USE-INDEX seq-idx NO-LOCK, ~
       FIRST job       WHERE job.company EQ job-mat.company         AND job.job     EQ job-mat.job         AND job.job-no  EQ job-mat.job-no         AND job.job-no2 EQ job-mat.job-no2, ~
       FIRST item NO-LOCK WHERE item.company EQ job-mat.company   AND item.i-no    EQ job-mat.rm-i-no and lookup(item.mat-type,"1,2,3,4,A,B,R,P") GT 0 ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH job-mat WHERE job-mat.company EQ cCompany  AND job-mat.job-no  EQ cJobNo   AND job-mat.job-no2 EQ iJobNo2 AND (job-mat.frm      EQ iFormNo OR iFormNo EQ ?)         AND (job-mat.blank-no EQ iBlankNo OR iBlankNo EQ ?) USE-INDEX seq-idx NO-LOCK, ~
       FIRST job       WHERE job.company EQ job-mat.company         AND job.job     EQ job-mat.job         AND job.job-no  EQ job-mat.job-no         AND job.job-no2 EQ job-mat.job-no2, ~
       FIRST item NO-LOCK WHERE item.company EQ job-mat.company   AND item.i-no    EQ job-mat.rm-i-no  and lookup(item.mat-type,"1,2,3,4,A,B,R,P") GT 0   ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table job-mat job item
&Scoped-define FIRST-TABLE-IN-QUERY-br_table job-mat
&Scoped-define SECOND-TABLE-IN-QUERY-br_table job
&Scoped-define THIRD-TABLE-IN-QUERY-br_table item


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAvailQty B-table-Win 
FUNCTION getAvailQty RETURNS INTEGER
  ( )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetLinearFeet B-table-Win 
FUNCTION fGetLinearFeet RETURNS DECIMAL
  ( )  FORWARD.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetFGQty B-table-Win 
FUNCTION fGetFGQty RETURNS INTEGER
  ( )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetTons B-table-Win 
FUNCTION fGetTons RETURNS DECIMAL
  (/* parameter-definitions */)  FORWARD.  

/* _UIB-CODE-BLOCK-END */
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
    job-mat,
    job,
    item SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
    QUERY br_table NO-LOCK DISPLAY
    job-mat.frm COLUMN-LABEL "Form" FORMAT "99" WIDTH 10
    job-mat.blank-no COLUMN-LABEL "Blank" FORMAT "99" WIDTH 10
    job-mat.rm-i-no COLUMN-LABEL "Item No" WIDTH 35
    item.i-dscr COLUMN-LABEL "Item Description" WIDTH 55 
    job-mat.qty-all COLUMN-LABEL "Allocation" FORMAT "->>,>>>,>>9.99<<<<":U WIDTH 27
    getAvailQty() @ iAvailQty COLUMN-LABEL "Qty Available" FORMAT "->>>,>>>,>>9.9<<<<<":U WIDTH 29      
    job-mat.qty-uom FORMAT "x(3)":U WIDTH 18 COLUMN-LABEL "Qty!UOM"
    job-mat.all-flg FORMAT "Yes/No":U WIDTH 18 COLUMN-LABEL "Allocated"
    fGetLinearFeet() @ dLinearFeet COLUMN-LABEL "Total Linear Feet" FORMAT "->,>>>,>>9.99":U WIDTH 29
    item.s-len COLUMN-LABEL "Board Length" FORMAT ">>>,>>9.99<<<<":U WIDTH 22
    item.s-wid COLUMN-LABEL "Board Width" FORMAT ">>>,>>9.99<<<<":U WIDTH 22
    item.cal COLUMN-LABEL "Board Caliper" FORMAT "9.99999":U WIDTH 22
    fGetFGQty() @ iFGQty COLUMN-LABEL "Total FG Qty for Job" FORMAT ">>,>>>,>>9":U WIDTH 35
    fGetTons () @ dTons COLUMN-LABEL "Weight per Ton" FORMAT "->>,>>>,>>9.99<<<<":U 
    cEmptyColumn COLUMN-LABEL ""          
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 198 BY 18.48
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
OPEN QUERY {&SELF-NAME} FOR EACH job-mat
WHERE job-mat.company EQ cCompany  
  AND job-mat.job-no  EQ cJobNo
  AND job-mat.job-no2 EQ iJobNo2
  AND (job-mat.frm      EQ iFormNo OR iFormNo EQ ?)
  AND (job-mat.blank-no EQ iBlankNo OR iBlankNo EQ ?)
USE-INDEX seq-idx NO-LOCK,
FIRST job
      WHERE job.company EQ job-mat.company
        AND job.job     EQ job-mat.job
        AND job.job-no  EQ job-mat.job-no
        AND job.job-no2 EQ job-mat.job-no2,
FIRST item NO-LOCK
WHERE item.company EQ job-mat.company
  AND item.i-no    EQ job-mat.rm-i-no
  and lookup(item.mat-type,"1,2,3,4,A,B,R,P") GT 0
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
  
        {methods/run_link.i "container-source" "pStatusClear" }
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
            opcItemID  = job-mat.rm-i-no
            opcJobNo   = job-mat.job-no
            opiJobNo2  = job-mat.job-no2
            oplAvail   = TRUE
            .
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunAlloc B-table-Win 
PROCEDURE pRunAlloc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-ask AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lv-alloc-char AS cha       NO-UNDO.
    DEFINE VARIABLE ll            AS LOG       NO-UNDO.
    DEFINE VARIABLE char-hdl      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCheck        AS LOGICAL   NO-UNDO.
    DEFINE BUFFER bf-job-mat FOR job-mat.
    
  
    FIND FIRST item
        WHERE item.company EQ cocode
        AND item.i-no    EQ job-mat.rm-i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        AND item.i-code  EQ "R"
        NO-LOCK NO-ERROR.
  
      
    IF AVAILABLE job-mat AND job-mat.all-flg THEN lv-alloc-char = "Deallocate".
    ELSE lv-alloc-char = "Allocate".
 
    IF job.stat = "H" THEN 
    DO:

        IF NOT AVAILABLE ITEM THEN 
        DO:
            opcMessage = "Estimated Material cannot be Allocated".
            RETURN .
        END.

        lCheck = YES .     
        RUN sharpshooter\messageDialog.w("The job status is Hold, are you sure you want to " + lv-alloc-char  + " this material ?",
            YES,
            YES,
            NO,
            OUTPUT ll
            ).
        IF NOT ll THEN 
        DO:
            RETURN ERROR.
        END.
    END.
    ELSE IF INDEX("LRAW",job.stat) = 0 THEN 
        DO:
            opcMessage = "The job status must be 'R'eleased in order to perform this selection".            
            RETURN .
        END.


    IF NOT AVAILABLE ITEM AND NOT lCheck THEN 
    DO:
        opcMessage = "Estimated Material cannot be Allocated" .
        RETURN .
    END.

    IF ip-ask THEN 
    DO:
        IF AVAILABLE job-mat AND job-mat.all-flg THEN lv-alloc-char = "Deallocate?".
        ELSE lv-alloc-char = "Allocate?".
        IF NOT lCheck THEN
            RUN sharpshooter\messageDialog.w("Are you sure you want to " + lv-alloc-char,
                YES,
                YES,
                NO,
                OUTPUT ll
                ).        
    END.

    ELSE ll = YES.

    IF ll THEN 
    DO:
        FIND bf-job-mat WHERE ROWID(bf-job-mat) EQ ROWID(job-mat) EXCLUSIVE-LOCK.
        bf-job-mat.qty-all = INTEGER(job-mat.qty-all:SCREEN-VALUE IN BROWSE {&browse-name}).
        bf-job-mat.all-flg = YES.  
        IF lv-alloc-char BEGINS "alloc" THEN do: 
           RUN jc/jc-all2.p (ROWID(bf-job-mat), 1).
           IF cCreateSBNote EQ "Yes" THEN
           RUN Notes_CreateSBNotes(BUFFER bf-job-mat).           
        END.   
        ELSE
        DO:
            RUN jc/jc-all2.p (ROWID(bf-job-mat), -1).
            FIND CURRENT ITEM EXCLUSIVE-LOCK NO-ERROR. 
            RUN rm/calcqcom.p (ROWID(item), OUTPUT item.q-comm).
            item.q-avail = item.q-onh + item.q-ono - item.q-comm.
            FIND CURRENT ITEM NO-LOCK NO-ERROR.
        END.
        RUN dispatch ("display-fields").
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetMaterial B-table-Win 
PROCEDURE pGetMaterial :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oprwRowid AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER opiForm AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiBlank AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRmItem AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRmItemDesc AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdAllocation AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdAvailQty AS DECIMAL NO-UNDO.
      
    IF AVAILABLE job-mat THEN
    DO:
        ASSIGN
            oprwRowid     = ROWID(job-mat)
            opiForm       = job-mat.frm
            opiBlank      = job-mat.blank-no
            opcRmItem     = job-mat.rm-i-no
            opdAllocation = job-mat.qty-all
            opdAvailQty   = ITEM.q-onh - item.q-comm
            opcRmItemDesc = item.i-dscr.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery B-table-Win 
PROCEDURE OpenQuery :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER   NO-UNDO.

    ASSIGN
        cCompany = ipcCompany
        cJobNo   = ipcJobNo
        iJobNo2  = ipiJobNo2
        iFormNo  = ipiFormNo
        iBlankNo = ipiBlankNo
        .
        
    RUN dispatch ('open-query').
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReOpenQuery B-table-Win 
PROCEDURE pReOpenQuery :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprwRowid AS ROWID NO-UNDO.
    
    {&OPEN-QUERY-{&BROWSE-NAME}} 
    REPOSITION {&browse-name} TO ROWID iprwRowid NO-ERROR.    
    
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
    {src/adm/template/snd-list.i "job"}
    {src/adm/template/snd-list.i "job-mat"}
    {src/adm/template/snd-list.i "item"}

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-rm-i-no B-table-Win 
PROCEDURE valid-rm-i-no :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplReturnError AS LOGICAL NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        job-mat.rm-i-no:SCREEN-VALUE IN BROWSE {&browse-name} =
            CAPS(job-mat.rm-i-no:SCREEN-VALUE IN BROWSE {&browse-name}).

        IF NOT CAN-FIND(FIRST ITEM
                    {sys/look/itemW.i}
                      AND item.i-no EQ job-mat.rm-i-no:SCREEN-VALUE IN BROWSE {&browse-name})
    THEN DO:
        MESSAGE "Must enter a valid RM Item..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO job-mat.rm-i-no IN BROWSE {&browse-name}.
        oplReturnError = YES.
        RETURN .
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAvailQty B-table-Win 
FUNCTION getAvailQty RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-result AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iResult AS INTEGER NO-UNDO.
    
    IF AVAILABLE ITEM THEN DO: 
       iResult = ITEM.q-onh - item.q-comm.
    END.
    RETURN iResult.   /* Function return value. */
    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetLinearFeet B-table-Win 
FUNCTION fGetLinearFeet RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-result AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dResult   AS DECIMAL   NO-UNDO.
    
    ASSIGN dResult = 0.
    
    FIND FIRST job-hdr
          WHERE job-hdr.company  EQ job-mat.company
            AND job-hdr.job      EQ job-mat.job
            AND job-hdr.job-no   EQ job-mat.job-no
            AND job-hdr.job-no2  EQ job-mat.job-no2
            AND job-hdr.frm      EQ job-mat.frm
          NO-LOCK NO-ERROR.

    IF AVAILABLE job-hdr THEN
    FIND FIRST eb WHERE
                eb.company  EQ job-hdr.company AND
                eb.est-no   EQ job-hdr.est-no AND
                eb.form-no  EQ job-hdr.frm 
                NO-LOCK NO-ERROR.
       
       dResult = IF AVAIL eb AND eb.t-len NE ? THEN (eb.t-len / 12) * eb.eqty  
                 ELSE 0 .
          
    RETURN dResult.   /* Function return value. */
    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetFGQty B-table-Win 
FUNCTION fGetFGQty RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-result AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iResult   AS INTEGER   NO-UNDO.
    
    ASSIGN iResult = 0.
    
    FIND FIRST job-hdr
          WHERE job-hdr.company  EQ job-mat.company
            AND job-hdr.job      EQ job-mat.job 
            AND job-hdr.job-no   EQ job-mat.job-no
            AND job-hdr.job-no2  EQ job-mat.job-no2
            AND job-hdr.frm      EQ job-mat.frm            
          NO-LOCK NO-ERROR.
    
    IF AVAILABLE job-hdr AND job-hdr.qty NE ? THEN DO: 
       iResult = job-hdr.qty.
    END.
    RETURN iResult.   /* Function return value. */
    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetTons B-table-Win 
FUNCTION fGetTons RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
     DEFINE VARIABLE dResult AS DECIMAL NO-UNDO.
     IF ITEM.cons-uom EQ "TON" THEN
     dResult = job-mat.qty-all.
     ELSE
     RUN custom/convquom.p(job-mat.company, item.cons-uom,"TON", item.basis-w,
                               (IF item.r-wid EQ 0 THEN item.s-len
                                                    ELSE 12),
                                (IF item.r-wid EQ 0 THEN item.s-wid
                                                    ELSE item.r-wid),
                                item.s-dep,                    
                                job-mat.qty-all, OUTPUT dResult).     
	RETURN dResult.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
