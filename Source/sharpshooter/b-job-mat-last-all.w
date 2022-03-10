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
DEFINE VARIABLE hdJobProcs     AS HANDLE    NO-UNDO.

RUN jc/JobProcs.p PERSISTENT SET hdJobProcs.

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
&Scoped-define FIELDS-IN-QUERY-br_table job-mat.frm job-mat.blank-no job-mat.rm-i-no item.i-dscr job-mat.qty-all job-mat.qty job-mat.qty-iss job-mat.qty-uom cEmptyColumn   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table   
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH job-mat NO-LOCK WHERE job-mat.company EQ cCompany AND job-mat.job-no EQ cJobNo AND job-mat.job-no NE "" AND job-mat.job-no2 EQ iJobNo2 AND (job-mat.frm EQ iFormNo OR iFormNo EQ ?) AND (job-mat.blank-no EQ iBlankNo OR iBlankNo EQ ?) AND job-mat.qty-all NE 0 USE-INDEX seq-idx, ~
       FIRST job NO-LOCK WHERE job.company EQ job-mat.company AND job.job EQ job-mat.job AND job.job-no EQ job-mat.job-no AND job.job-no2 EQ job-mat.job-no2, ~
       FIRST item NO-LOCK WHERE item.company EQ job-mat.company AND item.i-no EQ job-mat.rm-i-no AND LOOKUP(item.mat-type, ~
      "1 2 3 4 A B R P", ~
      " ") GT 0   ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH job-mat NO-LOCK WHERE job-mat.company EQ cCompany AND job-mat.job-no EQ cJobNo AND job-mat.job-no NE "" AND job-mat.job-no2 EQ iJobNo2 AND (job-mat.frm EQ iFormNo OR iFormNo EQ ?) AND (job-mat.blank-no EQ iBlankNo OR iBlankNo EQ ?) AND job-mat.qty-all NE 0 USE-INDEX seq-idx, ~
       FIRST job NO-LOCK WHERE job.company EQ job-mat.company AND job.job EQ job-mat.job AND job.job-no EQ job-mat.job-no AND job.job-no2 EQ job-mat.job-no2, ~
       FIRST item NO-LOCK WHERE item.company EQ job-mat.company AND item.i-no EQ job-mat.rm-i-no AND LOOKUP(item.mat-type, ~
      "1 2 3 4 A B R P", ~
      " ") GT 0   ~{&SORTBY-PHRASE}.
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
    job-mat.qty-all COLUMN-LABEL "Qty Allocated" FORMAT "->>,>>>,>>9.9<<<<<":U WIDTH 27
    job-mat.qty COLUMN-LABEL "Required" FORMAT "->>,>>>,>>9.9<<<<<":U WIDTH 27
    job-mat.qty-iss COLUMN-LABEL "Issued" FORMAT "->>,>>>,>>9.99<<<<":U WIDTH 27
    job-mat.qty-uom FORMAT "x(3)":U WIDTH 18 COLUMN-LABEL "Qty!UOM"
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
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH job-mat NO-LOCK
WHERE job-mat.company EQ cCompany
AND job-mat.job-no EQ cJobNo
AND job-mat.job-no NE ""
AND job-mat.job-no2 EQ iJobNo2
AND (job-mat.frm EQ iFormNo OR iFormNo EQ ?)
AND (job-mat.blank-no EQ iBlankNo OR iBlankNo EQ ?)
AND job-mat.qty-all NE 0
USE-INDEX seq-idx,
FIRST job NO-LOCK
WHERE job.company EQ job-mat.company
AND job.job EQ job-mat.job
AND job.job-no EQ job-mat.job-no
AND job.job-no2 EQ job-mat.job-no2,
FIRST item NO-LOCK
WHERE item.company EQ job-mat.company
AND item.i-no EQ job-mat.rm-i-no
AND LOOKUP(item.mat-type,"1 2 3 4 A B R P"," ") GT 0
  ~{&SORTBY-PHRASE}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy B-table-Win 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
        
    IF VALID-HANDLE(hdJobProcs) THEN
        DELETE PROCEDURE hdJobProcs.
                
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

/* Code placed here will execute AFTER standard behavior.    */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCopyJob B-table-Win 
PROCEDURE pCopyJob :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany   AS CHARACTER NO-UNDO.   
    DEFINE INPUT PARAMETER iprwRowId    AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER oplComplete AS LOGICAL NO-UNDO.
        
    RUN job_CopyMaterialPreviousJob IN hdJobProcs(INPUT ipcCompany, 
        INPUT iprwRowId, 
        INPUT cJobNo,
        INPUT iJobNo2,
        INPUT iFormNo, 
        INPUT iBlankNo,
        OUTPUT oplComplete).
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
  {src/adm/template/snd-list.i "job-mat"}
  {src/adm/template/snd-list.i "job"}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ViewRMInquiry B-table-Win 
PROCEDURE ViewRMInquiry :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    IF NOT AVAILABLE job-mat THEN
        RETURN.
        
    IF NOT VALID-HANDLE(hdRMInquiry) THEN 
    DO:         
        RUN sharpshooter/w-rmInquiry.w PERSISTENT SET hdRMInquiry.

        RUN dispatch IN hdRMInquiry (
            INPUT 'initialize':U
            ) NO-ERROR.
        
        hdRMInquiryWin = hdRMInquiry:CURRENT-WINDOW.
    END.
                                                 
    IF VALID-HANDLE(hdRMInquiry) AND
        VALID-HANDLE(hdRMInquiryWin) THEN 
    DO: 

        RUN ScanItem IN hdRMInquiry (
            INPUT job-mat.company,
            INPUT "",
            INPUT "",
            INPUT job-mat.rm-i-no,
            INPUT "",
            INPUT job-mat.job-no,
            INPUT job-mat.job-no2
            ) NO-ERROR.            

        IF hdRMInquiryWin:WINDOW-STATE EQ 2 THEN ASSIGN 
                hdRMInquiryWin:WINDOW-STATE = 3.
        
        hdRMInquiryWin:MOVE-TO-TOP().
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

