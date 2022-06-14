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

  File: browsers/settingFilter.w

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
DEFINE VARIABLE lShowAdvancedFilter AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btSearch RECT-1 RECT-7 fisearch ~
fiSettingName btAdvancedFilter cbCategory cbSettingType 
&Scoped-Define DISPLAYED-OBJECTS fisearch fiSettingName cbCategory 

/* Custom List Definitions                                              */
/* SETTING-FILTER,ALL-WIDGETS,List-3,List-4,List-5,List-6               */
&Scoped-define SETTING-FILTER RECT-1 fiSettingName cbCategory 

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

DEFINE VARIABLE cbCategory AS CHARACTER FORMAT "X(256)":U INITIAL "All" 
     LABEL "Category" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "All" 
     DROP-DOWN-LIST
     SIZE 36 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE cbSettingType AS CHARACTER FORMAT "X(256)":U INITIAL "All" 
     LABEL "Setting Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "All","Default","Non-Default" 
     DROP-DOWN-LIST
     SIZE 17 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fisearch AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 95 BY 1.19
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiSettingName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 32.4 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 125.6 BY 1.91.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 180 BY 4.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btSearch AT ROW 1 COL 114.4 WIDGET-ID 20
     fisearch AT ROW 1.33 COL 16 COLON-ALIGNED WIDGET-ID 46
     fiSettingName AT ROW 3.33 COL 10.4 WIDGET-ID 2
     btAdvancedFilter AT ROW 1 COL 122.8 WIDGET-ID 48
     cbCategory AT ROW 3.38 COL 94 COLON-ALIGNED WIDGET-ID 14
     cbSettingType AT ROW 3.38 COL 154 COLON-ALIGNED WIDGET-ID 42
     RECT-1 AT ROW 2.86 COL 9.4 WIDGET-ID 18
     RECT-7 AT ROW 1 COL 1 WIDGET-ID 50
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
         HEIGHT             = 9.19
         WIDTH              = 181.
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

/* SETTINGS FOR COMBO-BOX cbCategory IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR COMBO-BOX cbSettingType IN FRAME F-Main
   NO-DISPLAY                                                           */
ASSIGN 
       cbSettingType:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fiSettingName IN FRAME F-Main
   ALIGN-L 1                                                            */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   1                                                                    */
ASSIGN 
       RECT-7:HIDDEN IN FRAME F-Main           = TRUE.

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
    
    RUN pInit.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSearch s-object
ON CHOOSE OF btSearch IN FRAME F-Main /* Search */
DO:
    RUN new-state ("Search").
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


&Scoped-define SELF-NAME fiSettingName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSettingName s-object
ON RETURN OF fiSettingName IN FRAME F-Main /* Name */
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
        fiSettingName:SENSITIVE    = FALSE
        cbCategory:SENSITIVE       = FALSE
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
        fiSettingName:SENSITIVE    = TRUE
        cbCategory:SENSITIVE       = TRUE
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
    DEFINE OUTPUT PARAMETER opcSettingName  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCategory     AS CHARACTER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:    
    END.

    ASSIGN
        opcGlobalSearch = fiSearch:SCREEN-VALUE
        opcSettingName  = fiSettingName:SCREEN-VALUE
        opcCategory     = IF cbCategory:SCREEN-VALUE EQ ? OR cbCategory:SCREEN-VALUE EQ "All" THEN
                              ""
                          ELSE
                              cbCategory:SCREEN-VALUE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view s-object
PROCEDURE local-view:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY "ENTRY":U TO fisearch IN FRAME {&FRAME-NAME}.

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
            VIEW {&SETTING-FILTER}.
        ELSE
            HIDE {&SETTING-FILTER}.
    END.
    
    ASSIGN
        fiSettingName:SCREEN-VALUE = ""
        cbCategory:SCREEN-VALUE    = "All"
        .    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetCategoryList s-object 
PROCEDURE SetCategoryList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCategoryList AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        cbCategory:LIST-ITEMS   = ipcCategoryList
        cbCategory:SCREEN-VALUE = "All"
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

