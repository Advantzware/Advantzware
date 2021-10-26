&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: browsers/scopeFilter.w

  Description: from SMART.W - Template for basic SmartObject

  Author:
  Created:

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
DEFINE VARIABLE cCompany            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lShowAdvancedFilter AS LOGICAL   NO-UNDO.

RUN spGetSessionParam("Company", OUTPUT cCompany).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btSearch RECT-2 fisearch fiUser cbStatus ~
fiProgram cbScope btAdvancedFilter 
&Scoped-Define DISPLAYED-OBJECTS fisearch fiUser cbStatus fiProgram cbScope ~
fiScopeField1 fiScopeField2 fiScopeField3 

/* Custom List Definitions                                              */
/* List-1,SCOPE-FILTER,List-3,List-4,List-5,List-6                      */
&Scoped-define SCOPE-FILTER RECT-2 fiUser cbStatus fiProgram cbScope ~
fiScopeField1 fiScopeField2 fiScopeField3 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btAdvancedFilter 
     IMAGE-UP FILE "Graphics/32x32/navigate_close.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Advanced Filter Options" 
     SIZE 8 BY 1.91 TOOLTIP "Show advanced filter options".

DEFINE BUTTON btSearch 
     IMAGE-UP FILE "Graphics/32x32/search_new.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Search" 
     SIZE 8 BY 1.91.

DEFINE VARIABLE cbScope AS CHARACTER FORMAT "X(256)":U INITIAL "All" 
     LABEL "Scope" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "All" 
     DROP-DOWN-LIST
     SIZE 22 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE cbStatus AS CHARACTER FORMAT "X(256)":U INITIAL "All" 
     LABEL "Status" 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "All","Active","Inactive" 
     DROP-DOWN-LIST
     SIZE 22 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiProgram AS CHARACTER FORMAT "X(256)":U 
     LABEL "Program" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiScopeField1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiScopeField2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiScopeField3 AS CHARACTER FORMAT "X(256)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fisearch AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 95 BY 1.19
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiUser AS CHARACTER FORMAT "X(256)":U 
     LABEL "User" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 161 BY 2.43
     BGCOLOR 21 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 180 BY 4.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btSearch AT ROW 1 COL 114.2 WIDGET-ID 20
     fisearch AT ROW 1.33 COL 16 COLON-ALIGNED WIDGET-ID 46
     fiUser AT ROW 3.1 COL 12 COLON-ALIGNED WIDGET-ID 34
     cbStatus AT ROW 3.1 COL 56 COLON-ALIGNED WIDGET-ID 10
     fiProgram AT ROW 3.1 COL 98 COLON-ALIGNED WIDGET-ID 36
     cbScope AT ROW 4.19 COL 12 COLON-ALIGNED WIDGET-ID 24
     fiScopeField1 AT ROW 4.19 COL 56 COLON-ALIGNED WIDGET-ID 28
     fiScopeField2 AT ROW 4.19 COL 98 COLON-ALIGNED WIDGET-ID 30
     fiScopeField3 AT ROW 4.19 COL 140 COLON-ALIGNED WIDGET-ID 32
     btAdvancedFilter AT ROW 1 COL 122.6 WIDGET-ID 48
     RECT-2 AT ROW 2.86 COL 5 WIDGET-ID 22
     RECT-6 AT ROW 1 COL 1 WIDGET-ID 50
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 21 FGCOLOR 15 FONT 6 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic
   Frames: 1
   Add Fields to: Neither
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
  CREATE WINDOW s-object ASSIGN
         HEIGHT             = 4.76
         WIDTH              = 182.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB s-object 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW s-object
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX cbScope IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR COMBO-BOX cbStatus IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR FILL-IN fiProgram IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR FILL-IN fiScopeField1 IN FRAME F-Main
   NO-ENABLE 2                                                          */
ASSIGN 
       fiScopeField1:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fiScopeField2 IN FRAME F-Main
   NO-ENABLE 2                                                          */
ASSIGN 
       fiScopeField2:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fiScopeField3 IN FRAME F-Main
   NO-ENABLE 2                                                          */
ASSIGN 
       fiScopeField3:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fiUser IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RECT-6:HIDDEN IN FRAME F-Main           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btAdvancedFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAdvancedFilter s-object
ON CHOOSE OF btAdvancedFilter IN FRAME F-Main /* Advanced Filter Options */
DO:
    lShowAdvancedFilter = NOT lShowAdvancedFilter.
    
    IF lShowAdvancedFilter THEN DO:
        btAdvancedFilter:LOAD-IMAGE ("Graphics\32x32\navigate_open.png").
        btAdvancedFilter:TOOLTIP = "Hide advanced filter options".
    END.
    ELSE DO:
        btAdvancedFilter:LOAD-IMAGE ("Graphics\32x32\navigate_close.png").
        btAdvancedFilter:TOOLTIP = "Show advanced filter options".
    END.
        
    ASSIGN
        cbScope:SCREEN-VALUE       = "All"
        cbStatus:SCREEN-VALUE      = "All"
        fiUser:SCREEN-VALUE        = ""
        fiProgram:SCREEN-VALUE     = ""
        fiScopeField1:SCREEN-VALUE = ""
        fiScopeField2:SCREEN-VALUE = ""
        fiScopeField3:SCREEN-VALUE = ""
        .
            
    RUN pInit.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSearch s-object
ON CHOOSE OF btSearch IN FRAME F-Main /* Search */
DO:
    RUN new-state("Search").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbScope
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbScope s-object
ON VALUE-CHANGED OF cbScope IN FRAME F-Main /* Scope */
DO:
    ASSIGN
        fiScopeField1:SCREEN-VALUE = ""
        fiScopeField2:SCREEN-VALUE = ""
        fiScopeField3:SCREEN-VALUE = ""
        fiScopeField1:HIDDEN       = TRUE
        fiScopeField2:HIDDEN       = TRUE
        fiScopeField3:HIDDEN       = TRUE
        .
    
    IF SELF:SCREEN-VALUE EQ "Company" THEN
        ASSIGN
            fiScopeField1:SCREEN-VALUE = cCompany
            fiScopeField1:LABEL        = "Company"
            fiScopeField1:HIDDEN       = FALSE
            fiScopeField1:SENSITIVE    = TRUE
            .
    ELSE IF SELF:SCREEN-VALUE EQ "Customer" THEN
        ASSIGN
            fiScopeField1:SCREEN-VALUE = cCompany
            fiScopeField1:LABEL        = "Company"
            fiScopeField1:HIDDEN       = FALSE
            fiScopeField1:SENSITIVE    = TRUE
            fiScopeField2:LABEL        = "Customer"
            fiScopeField2:HIDDEN       = FALSE
            fiScopeField2:SENSITIVE    = TRUE
            .
    ELSE IF SELF:SCREEN-VALUE EQ "Vendor" THEN
        ASSIGN
            fiScopeField1:SCREEN-VALUE = cCompany
            fiScopeField1:LABEL        = "Company"
            fiScopeField1:HIDDEN       = FALSE
            fiScopeField1:SENSITIVE    = TRUE
            fiScopeField2:LABEL        = "Vendor"
            fiScopeField2:HIDDEN       = FALSE
            fiScopeField2:SENSITIVE    = TRUE
            .            
    ELSE IF SELF:SCREEN-VALUE EQ "ShipTo" THEN
        ASSIGN
            fiScopeField1:SCREEN-VALUE = cCompany
            fiScopeField1:LABEL        = "Company"
            fiScopeField1:HIDDEN       = FALSE
            fiScopeField1:SENSITIVE    = TRUE
            fiScopeField2:LABEL        = "Customer"
            fiScopeField2:HIDDEN       = FALSE
            fiScopeField2:SENSITIVE    = TRUE
            fiScopeField3:LABEL        = "ShipTo"
            fiScopeField3:HIDDEN       = FALSE
            fiScopeField3:SENSITIVE    = TRUE
            .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiProgram
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiProgram s-object
ON RETURN OF fiProgram IN FRAME F-Main /* Program */
DO:
    APPLY "CHOOSE" TO btSearch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME fiScopeField1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiScopeField1 s-object
ON HELP OF fiScopeField1 IN FRAME F-Main
DO:
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
    
    RUN system/openlookup.p (
        cCompany,  /* company */ 
        "",  /* lookup field */
        41, /* Subject ID */
        "",  /* User ID */
        0,   /* Param value ID */
        OUTPUT returnFields, 
        OUTPUT lookupField, 
        OUTPUT recVal
        ). 

    IF lookupField NE "" THEN
        SELF:SCREEN-VALUE = lookupField.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiScopeField1 s-object
ON RETURN OF fiScopeField1 IN FRAME F-Main
DO:
    APPLY "CHOOSE" TO btSearch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiScopeField2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiScopeField2 s-object
ON HELP OF fiScopeField2 IN FRAME F-Main
DO:
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
    DEFINE VARIABLE iSubjectID   AS INTEGER   NO-UNDO.
    
    CASE cbScope:SCREEN-VALUE:
        WHEN "Customer" OR 
        WHEN "ShipTo" THEN
            iSubjectID = 23.
        WHEN "Vendor" THEN
            iSubjectID = 32.            
    END CASE.
    iSubjectID = DYNAMIC-FUNCTION("sfSubjectID",iSubjectID).
    RUN system/openlookup.p (
        cCompany,  /* company */ 
        "",  /* lookup field */
        iSubjectID, /* Subject ID */
        "",  /* User ID */
        0,   /* Param value ID */
        OUTPUT returnFields, 
        OUTPUT lookupField, 
        OUTPUT recVal
        ). 

    IF returnFields NE "" THEN DO:
        CASE cbScope:SCREEN-VALUE:
            WHEN "Customer" THEN
                SELF:SCREEN-VALUE = DYNAMIC-FUNCTION("sfDynLookupValue", "cust.cust-no", returnFields).
            WHEN "ShipTo" THEN
                SELF:SCREEN-VALUE = DYNAMIC-FUNCTION("sfDynLookupValue", "shipto.cust-no", returnFields).
            WHEN "Vendor" THEN
                SELF:SCREEN-VALUE = DYNAMIC-FUNCTION("sfDynLookupValue", "vend.vend-no", returnFields).
        END CASE.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiScopeField2 s-object
ON RETURN OF fiScopeField2 IN FRAME F-Main
DO:
    APPLY "CHOOSE" TO btSearch.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiScopeField3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiScopeField3 s-object
ON HELP OF fiScopeField3 IN FRAME F-Main
DO:
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
    DEFINE VARIABLE iSubjectID   AS INTEGER   NO-UNDO.
    
    CASE cbScope:SCREEN-VALUE:
        WHEN "ShipTo" THEN
            iSubjectID = 122.
    END CASE.
    iSubjectID = DYNAMIC-FUNCTION("sfSubjectID",iSubjectID).
    RUN system/openlookup.p (
        cCompany,  /* company */ 
        "",  /* lookup field */
        iSubjectID, /* Subject ID */
        "",  /* User ID */
        0,   /* Param value ID */
        OUTPUT returnFields, 
        OUTPUT lookupField, 
        OUTPUT recVal
        ). 

    IF returnFields NE "" THEN DO:
        CASE cbScope:SCREEN-VALUE:
            WHEN "ShipTo" THEN
                ASSIGN
                    fiScopeField2:SCREEN-VALUE = DYNAMIC-FUNCTION("sfDynLookupValue", "shipto.cust-no", returnFields)
                    fiScopeField3:SCREEN-VALUE = DYNAMIC-FUNCTION("sfDynLookupValue", "shipto.ship-id", returnFields)
                    .
        END CASE.
    END.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiScopeField3 s-object
ON RETURN OF fiScopeField3 IN FRAME F-Main
DO:
    APPLY "CHOOSE" TO btSearch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fisearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fisearch s-object
ON RETURN OF fisearch IN FRAME F-Main /* Search */
DO:
    APPLY "CHOOSE" TO btSearch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiUser
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiUser s-object
ON RETURN OF fiUser IN FRAME F-Main /* User */
DO:
    APPLY "CHOOSE" TO btSearch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK s-object 


/* ***************************  Main Block  *************************** */

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisableAdvancedFilter s-object 
PROCEDURE DisableAdvancedFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    btAdvancedFilter:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisableAll s-object 
PROCEDURE DisableAll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        fiSearch:SENSITIVE         = FALSE
        btSearch:SENSITIVE         = FALSE
        btAdvancedFilter:SENSITIVE = FALSE
        cbScope:SENSITIVE          = FALSE
        cbStatus:SENSITIVE         = FALSE
        fiUser:SENSITIVE           = FALSE
        fiProgram:SENSITIVE        = FALSE
        fiScopeField1:SENSITIVE    = FALSE
        fiScopeField2:SENSITIVE    = FALSE
        fiScopeField3:SENSITIVE    = FALSE
        .
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI s-object  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnableAll s-object 
PROCEDURE EnableAll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        fiSearch:SENSITIVE         = TRUE
        btSearch:SENSITIVE         = TRUE
        btAdvancedFilter:SENSITIVE = TRUE
        cbScope:SENSITIVE          = TRUE
        cbStatus:SENSITIVE         = TRUE
        fiUser:SENSITIVE           = TRUE
        fiProgram:SENSITIVE        = TRUE
        fiScopeField1:SENSITIVE    = TRUE
        fiScopeField2:SENSITIVE    = TRUE
        fiScopeField3:SENSITIVE    = TRUE
        .
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSearchFields s-object 
PROCEDURE GetSearchFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcGlobalSearch AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcScope        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcScopeField1  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcScopeField2  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcScopeField3  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcUser         AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcProgram      AS CHARACTER NO-UNDO.    
    DEFINE OUTPUT PARAMETER oplStatus       AS LOGICAL   NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:    
    END.

    ASSIGN
        opcGlobalSearch = fiSearch:SCREEN-VALUE
        opcScope        = IF cbScope:SCREEN-VALUE EQ ? OR cbScope:SCREEN-VALUE EQ "All" THEN
                              ""
                          ELSE
                              cbScope:SCREEN-VALUE
        opcScopeField1  = fiScopeField1:SCREEN-VALUE
        opcScopeField2  = fiScopeField2:SCREEN-VALUE
        opcScopeField3  = fiScopeField3:SCREEN-VALUE        
        opcUser         = fiUser:SCREEN-VALUE        
        opcProgram      = fiProgram:SCREEN-VALUE        
        oplStatus       = IF cbStatus:SCREEN-VALUE EQ ? OR cbStatus:SCREEN-VALUE EQ "" OR cbStatus:SCREEN-VALUE EQ "All" THEN
                              ?
                          ELSE IF cbStatus:SCREEN-VALUE EQ "Active" THEN
                              FALSE
                          ELSE
                              TRUE
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable s-object 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit s-object 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        IF lShowAdvancedFilter THEN
            VIEW {&SCOPE-FILTER}.
        ELSE
            HIDE {&SCOPE-FILTER}.
    END.    
            
    APPLY "VALUE-CHANGED" TO cbScope.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetScopeList s-object 
PROCEDURE SetScopeList :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcScopeList AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.

    ASSIGN
        cbScope:LIST-ITEMS   = ipcScopeList
        cbScope:SCREEN-VALUE = "All"
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed s-object 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     Receive and process 'state-changed' methods
               (issued by 'new-state' event).
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
  END CASE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE viewer-identifier s-object 
PROCEDURE viewer-identifier :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

