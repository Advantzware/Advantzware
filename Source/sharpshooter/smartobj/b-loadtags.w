&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
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

  File: sharpshooter/smartobj/b-loadtags.w

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
&SCOPED-DEFINE xlocal-destroy

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/var.i "NEW SHARED"}
{sys/inc/varasgn.i}
{oerep/ttLoadTag.i}
{oerep/r-loadtg.i NEW}

DEFINE NEW SHARED TEMP-TABLE tt-word-print LIKE w-ord 
       FIELD tag-no AS CHARACTER.

DEFINE VARIABLE char-hdl  AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle   AS HANDLE    NO-UNDO.

DEFINE VARIABLE cCompany   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSelectAll AS LOGICAL   NO-UNDO INITIAL TRUE.

DEFINE VARIABLE hdLoadTagProcs AS HANDLE NO-UNDO.

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
&Scoped-define INTERNAL-TABLES ttLoadTag

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table ttLoadTag.isSelected ttLoadTag.tagStatus ttLoadTag.custID ttLoadTag.orderID ttLoadTag.jobID ttLoadTag.jobID2 NO-LABEL ttLoadTag.itemID ttLoadTag.itemName ttLoadTag.tag ttLoadTag.quantityInUnit ttLoadTag.subUnitsPerUnit ttLoadTag.quantityInSubUnit ttLoadTag.quantityOfSubUnits ttLoadTag.jobQuantity ttLoadTag.printCopies ttLoadTag.ordQuantity ttLoadTag.relQuantity ttLoadTag.overPct ttLoadTag.partial ttLoadTag.totalTags ttLoadTag.quantityTotal ttLoadTag.unitWeight ttLoadTag.palletWeight ttLoadTag.lotID ttLoadTag.custPONo ttLoadTag.poline ttLoadTag.emptyColumn   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table ttLoadTag.isSelected   
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table ttLoadTag
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table ttLoadTag
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH ttLoadTag BY ttLoadTag.scannedDateTime DESC
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH ttLoadTag BY ttLoadTag.scannedDateTime DESC.
&Scoped-define TABLES-IN-QUERY-br_table ttLoadTag
&Scoped-define FIRST-TABLE-IN-QUERY-br_table ttLoadTag


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
      ttLoadTag SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      ttLoadTag.isSelected COLUMN-LABEL "[]All" WIDTH 10 VIEW-AS TOGGLE-BOX
      ttLoadTag.tagStatus COLUMN-LABEL "Status" WIDTH 15
      ttLoadTag.custID COLUMN-LABEL "Cust #" WIDTH 30
      ttLoadTag.orderID  COLUMN-LABEL "Order#" FORMAT ">>>>>>>9" WIDTH 18
      ttLoadTag.jobID COLUMN-LABEL "  Job#" FORMAT "x(9)" WIDTH 22
      ttLoadTag.jobID2 NO-LABEL FORMAT "999" WIDTH 9
      ttLoadTag.itemID COLUMN-LABEL "Item #" WIDTH 50
      ttLoadTag.itemName COLUMN-LABEL "Item!Name" WIDTH 60
      ttLoadTag.tag COLUMN-LABEL "Tag#" WIDTH 50
      ttLoadTag.quantityInUnit FORMAT "->,>>>,>>9" COLUMN-LABEL "Total Qty!Per Pallet" WIDTH 20
      ttLoadTag.subUnitsPerUnit FORMAT "->>>,>>9" COLUMN-LABEL "Units/!Pallet" WIDTH 17
      ttLoadTag.quantityInSubUnit FORMAT "->>>,>>9" COLUMN-LABEL "Unit!Count" WIDTH 17
      ttLoadTag.quantityOfSubUnits FORMAT "->>>,>>9" COLUMN-LABEL "Total!Units" WIDTH 17
      ttLoadTag.jobQuantity COLUMN-LABEL "Job!Quantity" WIDTH 17
      ttLoadTag.printCopies COLUMN-LABEL "Print!Copies" WIDTH 17
      ttLoadTag.ordQuantity COLUMN-LABEL "Ord Qty" WIDTH 17
      ttLoadTag.relQuantity COLUMN-LABEL "Rel Qty" WIDTH 17
      ttLoadTag.overPct FORMAT ">>9.99" COLUMN-LABEL "Overrun%" WIDTH 15
      ttLoadTag.partial COLUMN-LABEL "Partial" WIDTH 12
      ttLoadTag.totalTags COLUMN-LABEL "No. of!Tags" WIDTH 15
      ttLoadTag.quantityTotal FORMAT "->,>>>,>>9" COLUMN-LABEL "Total Qty" WIDTH 17
      ttLoadTag.unitWeight COLUMN-LABEL "Unit!Wt" WIDTH 15
      ttLoadTag.palletWeight COLUMN-LABEL "Pallet!Wt" WIDTH 15
      ttLoadTag.lotID FORMAT "X(20)" COLUMN-LABEL "FG Lot#" WIDTH 40
      ttLoadTag.custPONo COLUMN-LABEL "Customer!PO#" WIDTH 25
      ttLoadTag.poline COLUMN-LABEL "Ln" WIDTH 10
      ttLoadTag.emptyColumn COLUMN-LABEL ""
      ENABLE ttLoadTag.isSelected
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS NO-TAB-STOP SIZE 150 BY 6.69
         FONT 36 ROW-HEIGHT-CHARS .95 FIT-LAST-COLUMN.


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
         WIDTH              = 150.
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
OPEN QUERY {&SELF-NAME} FOR EACH ttLoadTag BY ttLoadTag.scannedDateTime DESC.
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
ON START-SEARCH OF br_table IN FRAME F-Main
DO:
    IF SELF:CURRENT-COLUMN:NAME EQ "isSelected" THEN DO:
        lSelectAll = NOT lSelectAll.
        
        FOR EACH ttLoadTag:
            ttLoadTag.isSelected = lSelectAll.
        END.
        
        SELF:CURRENT-COLUMN:LABEL = IF lSelectAll THEN
                                        "[*] All"
                                    ELSE
                                        "[ ] All".
                                        
        RUN dispatch ("open-query").   
    END.    
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
{methods/browsers/setCellColumns.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildLoadTagsFromBOL B-table-Win 
PROCEDURE BuildLoadTagsFromBOL :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBOLID   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiCopies  AS INTEGER   NO-UNDO.
    
    EMPTY TEMP-TABLE ttLoadTag.
    
    SESSION:SET-WAIT-STATE("GENERAL").
    
    RUN BuildLoadTagsFromBOL IN hdLoadTagProcs (
        INPUT  ipcCompany,
        INPUT  ipiBOLID,
        INPUT  ipiCopies,
        INPUT-OUTPUT TABLE ttLoadTag BY-REFERENCE
        ).
    
    RUN dispatch (
        INPUT "open-query"
        ).
    
    SESSION:SET-WAIT-STATE("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildLoadTagsFromJob B-table-Win 
PROCEDURE BuildLoadTagsFromJob :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobno             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobno2            AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormNo            AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankNo           AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQuantity          AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQuantityInSubUnit AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiSubUnitsPerUnit   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiCopies            AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserField1        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserField2        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserField3        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserFieldValue1   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserFieldValue2   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserFieldValue3   AS CHARACTER NO-UNDO.
    
    SESSION:SET-WAIT-STATE("GENERAL").
    
    RUN BuildLoadTagsFromJob IN hdLoadTagProcs (
        INPUT  ipcCompany,
        INPUT  ipcJobno,
        INPUT  ipiJobno2,
        INPUT  ipiFormNo,
        INPUT  ipiBlankNo,
        INPUT  ipcItemID,
        INPUT  ipiQuantity,
        INPUT  ipiQuantityInSubUnit,
        INPUT  ipiSubUnitsPerUnit,
        INPUT  ipiCopies,
        INPUT  ipcUserField1,
        INPUT  ipcUserField2,
        INPUT  ipcUserField3,
        INPUT  ipcUserFieldValue1,
        INPUT  ipcUserFieldValue2,
        INPUT  ipcUserFieldValue3,
        INPUT-OUTPUT TABLE ttLoadTag BY-REFERENCE
        ).    
    
    RUN dispatch (
        INPUT "open-query"
        ).
    
    SESSION:SET-WAIT-STATE("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildLoadTagsFromPO B-table-Win 
PROCEDURE BuildLoadTagsFromPO :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPOID              AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPOLine            AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQuantity          AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQuantityInSubUnit AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiSubUnitsPerUnit   AS INTEGER   NO-UNDO.      
    DEFINE INPUT  PARAMETER ipcQuantityUOM       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiCopies            AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError             AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage           AS CHARACTER NO-UNDO.
    
    RUN BuildLoadTagsFromPO IN hdLoadTagProcs (
        INPUT  ipcCompany,
        INPUT  ipiPOID,
        INPUT  ipiPOLine,
        INPUT  ipiQuantity,
        INPUT  ipiQuantityInSubUnit,
        INPUT  ipiSubUnitsPerUnit,        
        INPUT  ipcQuantityUOM,
        INPUT  ipiCopies,
        OUTPUT oplError,
        OUTPUT opcMessage,
        INPUT-OUTPUT TABLE ttLoadTag
        ).

    RUN dispatch (
        INPUT "open-query"
        ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildLoadTagsFromRelease B-table-Win 
PROCEDURE BuildLoadTagsFromRelease :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiCopies    AS INTEGER   NO-UNDO.
    
    EMPTY TEMP-TABLE ttLoadTag.
    
    SESSION:SET-WAIT-STATE("GENERAL").
    
    RUN BuildLoadTagsFromRelease IN hdLoadTagProcs (
        INPUT  ipcCompany,
        INPUT  ipiReleaseID,
        INPUT  ipiCopies,
        INPUT-OUTPUT TABLE ttLoadTag BY-REFERENCE
        ).
    
    RUN dispatch (
        INPUT "open-query"
        ).
    
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildLoadTagsFromTag B-table-Win 
PROCEDURE BuildLoadTagsFromTag :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTag     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiCopies  AS INTEGER   NO-UNDO.

    SESSION:SET-WAIT-STATE("GENERAL").
    
    RUN BuildLoadTagsFromTag IN hdLoadTagProcs (
        INPUT  ipcCompany,
        INPUT  ipcTag,
        INPUT  ipiCopies,
        INPUT-OUTPUT TABLE ttLoadTag BY-REFERENCE
        ).    
    
    RUN dispatch (
        INPUT "open-query"
        ).
    
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateLoadTagFromTT B-table-Win 
PROCEDURE CreateLoadTagFromTT :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN CreateLoadTagFromTT IN hdLoadTagProcs (
        INPUT cCompany,
        INPUT cLocation,
        INPUT TRUE, /* Print loadtag */
        INPUT FALSE, /* Empty ttLoadtag temp-table */
        INPUT-OUTPUT TABLE ttLoadTag BY-REFERENCE 
        ).

    RUN dispatch (
        INPUT "open-query"
        ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteSelected B-table-Win 
PROCEDURE DeleteSelected :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lChoice AS LOGICAL NO-UNDO.
    
    IF AVAILABLE ttLoadTag THEN DO:
        RUN sharpshooter/messageDialog.w (
            "DELETE SELECTED RECORD?",
            YES,
            YES,
            NO,
            OUTPUT lChoice
            ).
        
        IF lChoice THEN DO:
            DELETE ttLoadTag.
            
            RUN dispatch (
                INPUT "open-query"
                ).
        END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EmptyTTLoadTag B-table-Win 
PROCEDURE EmptyTTLoadTag :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttLoadTag.
    
    RUN dispatch (
        INPUT "open-query"
        ).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pHandle  AS HANDLE    NO-UNDO.
    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
    
    RUN setCellColumns NO-ERROR.

  /* Code placed here will execute PRIOR to standard behavior. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE No-Resize B-table-Win 
PROCEDURE No-Resize :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit B-table-Win 
PROCEDURE pInit PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hdBrowse AS HANDLE  NO-UNDO.
    DEFINE VARIABLE iColumn  AS INTEGER NO-UNDO.
    DEFINE VARIABLE hdColumn AS HANDLE  NO-UNDO.
    
    DEFINE VARIABLE oSSLoadTagDesignConfig AS system.Config NO-UNDO.
     
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN spGetSessionParam ("Company", OUTPUT cCompany).
    RUN spGetSessionParam ("Location", OUTPUT cLocation).
    
    RUN oerep/LoadTagProcs.p PERSISTENT SET hdLoadTagProcs.
    
    {methods/run_link.i "CONTAINER-SOURCE" "GetDesignConfig" "(OUTPUT oSSLoadTagDesignConfig)"}
    
    IF VALID-OBJECT(oSSLoadTagDesignConfig) THEN DO:
        hdBrowse = BROWSE {&BROWSE-NAME}:HANDLE.
    
        DO iColumn = 1 TO hdBrowse:NUM-COLUMNS :
            hdColumn = hdBrowse:GET-BROWSE-COLUMN (iColumn).
            
            CASE hdColumn:NAME:
                WHEN 'orderID' THEN DO:
                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "OrderID", "label") THEN
                        hdColumn:LABEL = oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "OrderID", "label").

                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "OrderID", "visible") THEN
                        hdColumn:VISIBLE = LOGICAL(oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "OrderID", "visible")).
                END.
                WHEN 'jobID' THEN DO:
                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "JobID", "label") THEN
                        hdColumn:LABEL = oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "JobID", "label").

                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "JobID", "visible") THEN
                        hdColumn:VISIBLE = LOGICAL(oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "JobID", "visible")).
                END.
                WHEN 'jobID2' THEN DO:
                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "JobID2", "label") THEN
                        hdColumn:LABEL = oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "JobID2", "label").

                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "JobID2", "visible") THEN
                        hdColumn:VISIBLE = LOGICAL(oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "JobID2", "visible")).
                END.
                WHEN 'custID' THEN DO:
                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "CustomerID", "label") THEN
                        hdColumn:LABEL = oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "CustomerID", "label").

                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "CustomerID", "visible") THEN
                        hdColumn:VISIBLE = LOGICAL(oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "CustomerID", "visible")).
                END.
                WHEN 'itemID' THEN DO:
                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "ItemID", "label") THEN
                        hdColumn:LABEL = oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "ItemID", "label").

                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "ItemID", "visible") THEN
                        hdColumn:VISIBLE = LOGICAL(oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "ItemID", "visible")).
                END.
                WHEN 'ordQuantity' THEN DO:
                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "OrderQuantity", "label") THEN
                        hdColumn:LABEL = oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "OrderQuantity", "label").

                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "OrderQuantity", "visible") THEN
                        hdColumn:VISIBLE = LOGICAL(oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "OrderQuantity", "visible")).
                END.
                WHEN 'relQuantity' THEN DO:
                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "ReleaseQuantity", "label") THEN
                        hdColumn:LABEL = oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "ReleaseQuantity", "label").

                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "ReleaseQuantity", "visible") THEN
                        hdColumn:VISIBLE = LOGICAL(oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "ReleaseQuantity", "visible")).
                END.
                WHEN 'overPct' THEN DO:
                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "OversPercent", "label") THEN
                        hdColumn:LABEL = oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "OversPercent", "label").

                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "OversPercent", "visible") THEN
                        hdColumn:VISIBLE = LOGICAL(oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "OversPercent", "visible")).
                END.
                WHEN 'quantityInSubUnit' THEN DO:
                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "QuantityInSubUnit", "label") THEN
                        hdColumn:LABEL = oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "QuantityInSubUnit", "label").

                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "QuantityInSubUnit", "visible") THEN
                        hdColumn:VISIBLE = LOGICAL(oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "QuantityInSubUnit", "visible")).
                END.
                WHEN 'subUnitsPerUnit' THEN DO:
                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "SubUnitsPerUnit", "label") THEN
                        hdColumn:LABEL = oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "SubUnitsPerUnit", "label").

                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "SubUnitsPerUnit", "visible") THEN
                        hdColumn:VISIBLE = LOGICAL(oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "SubUnitsPerUnit", "visible")).
                END.
                WHEN 'partial' THEN DO:
                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "Partial", "label") THEN
                        hdColumn:LABEL = oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "Partial", "label").

                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "Partial", "visible") THEN
                        hdColumn:VISIBLE = LOGICAL(oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "Partial", "visible")).
                END.
                WHEN 'quantityInUnit' THEN DO:
                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "QuantityInUnit", "label") THEN
                        hdColumn:LABEL = oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "QuantityInUnit", "label").

                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "QuantityInUnit", "visible") THEN
                        hdColumn:VISIBLE = LOGICAL(oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "QuantityInUnit", "visible")).
                END.
                WHEN 'totalTags' THEN DO:
                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "TotalTags", "label") THEN
                        hdColumn:LABEL = oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "TotalTags", "label").

                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "TotalTags", "visible") THEN
                        hdColumn:VISIBLE = LOGICAL(oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "TotalTags", "visible")).
                END.
                WHEN 'unitWeight' THEN DO:
                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "UnitWeight", "label") THEN
                        hdColumn:LABEL = oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "UnitWeight", "label").

                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "UnitWeight", "visible") THEN
                        hdColumn:VISIBLE = LOGICAL(oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "UnitWeight", "visible")).
                END.
                WHEN 'palletWeight' THEN DO:
                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "PalletWeight", "label") THEN
                        hdColumn:LABEL = oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "PalletWeight", "label").

                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "PalletWeight", "visible") THEN
                        hdColumn:VISIBLE = LOGICAL(oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "PalletWeight", "visible")).
                END.
                WHEN 'lotID' THEN DO:
                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "LotID", "label") THEN
                        hdColumn:LABEL = oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "LotID", "label").

                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "LotID", "visible") THEN
                        hdColumn:VISIBLE = LOGICAL(oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "LotID", "visible")).
                END.
                WHEN 'itemName' THEN DO:
                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "ItemName", "label") THEN
                        hdColumn:LABEL = oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "ItemName", "label").

                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "ItemName", "visible") THEN
                        hdColumn:VISIBLE = LOGICAL(oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "ItemName", "visible")).
                END.
                WHEN 'custPONo' THEN DO:
                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "CustomerPO", "label") THEN
                        hdColumn:LABEL = oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "CustomerPO", "label").

                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "CustomerPO", "visible") THEN
                        hdColumn:VISIBLE = LOGICAL(oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "CustomerPO", "visible")).
                END.
                WHEN 'poLineID' THEN DO:
                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "POLine", "label") THEN
                        hdColumn:LABEL = oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "POLine", "label").

                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "POLine", "visible") THEN
                        hdColumn:VISIBLE = LOGICAL(oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "POLine", "visible")).
                END.
                WHEN 'printCopies' THEN DO:
                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "PrintCopies", "label") THEN
                        hdColumn:LABEL = oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "PrintCopies", "label").

                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "PrintCopies", "visible") THEN
                        hdColumn:VISIBLE = LOGICAL(oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "PrintCopies", "visible")).
                END.
                WHEN 'tagStatus' THEN DO:
                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "Status", "label") THEN
                        hdColumn:LABEL = oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "Status", "label").

                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "Status", "visible") THEN
                        hdColumn:VISIBLE = LOGICAL(oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "Status", "visible")).
                END.
                WHEN 'jobQuantity' THEN DO:
                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "JobQuantity", "label") THEN
                        hdColumn:LABEL = oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "JobQuantity", "label").

                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "JobQuantity", "visible") THEN
                        hdColumn:VISIBLE = LOGICAL(oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "JobQuantity", "visible")).
                END.
                WHEN 'quantityTotal' THEN DO:
                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "TotalQuantity", "label") THEN
                        hdColumn:LABEL = oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "TotalQuantity", "label").

                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "TotalQuantity", "visible") THEN
                        hdColumn:VISIBLE = LOGICAL(oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "TotalQuantity", "visible")).
                END.
                WHEN 'isSelected' THEN DO:
                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "Select", "label") THEN
                        hdColumn:LABEL = oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "Select", "label").

                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "Select", "visible") THEN
                        hdColumn:VISIBLE = LOGICAL(oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "Select", "visible")).
                END.                
                WHEN 'tag' THEN DO:
                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "Tag", "label") THEN
                        hdColumn:LABEL = oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "Tag", "label").

                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "Tag", "visible") THEN
                        hdColumn:VISIBLE = LOGICAL(oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "Tag", "visible")).
                END.   
                WHEN 'quantityOfSubUnit' THEN DO:
                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "QuantityOfSubUnit", "label") THEN
                        hdColumn:LABEL = oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "QuantityOfSubUnit", "label").

                    IF oSSLoadTagDesignConfig:IsAttributeAvailable("LoadtagBrowse", "QuantityOfSubUnit", "visible") THEN
                        hdColumn:VISIBLE = LOGICAL(oSSLoadTagDesignConfig:GetAttributeValue("LoadtagBrowse", "QuantityOfSubUnit", "visible")).
                END.             
            END CASE.
        END.
    END.

    BROWSE {&BROWSE-NAME}:ROW-HEIGHT-CHARS = 1.
    BROWSE {&BROWSE-NAME}:FONT = 17.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintTTLoadTags B-table-Win 
PROCEDURE PrintTTLoadTags :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    SESSION:SET-WAIT-STATE("GENERAL").
    
    RUN PrintTTLoadTags IN hdLoadTagProcs (
        INPUT cCompany,
        INPUT cLocation,
        INPUT-OUTPUT TABLE ttLoadTag BY-REFERENCE
        ).

    RUN dispatch (
        INPUT "open-query"
        ).
    
    SESSION:SET-WAIT-STATE("").
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
  {src/adm/template/snd-list.i "ttLoadTag"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE xlocal-destroy B-table-Win 
PROCEDURE xlocal-destroy :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    IF VALID-HANDLE(hdLoadTagProcs) THEN
        DELETE PROCEDURE hdLoadTagProcs.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

