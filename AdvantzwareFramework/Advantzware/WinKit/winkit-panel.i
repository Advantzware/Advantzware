/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/
/*------------------------------------------------------------------------
    File        : winkit-panel.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Sat Feb 06 20:15:11 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&GLOBAL-DEFINE ADM-DISPATCH-QUALIFIER winkit

DEFINE VARIABLE lInited AS LOGICAL                                                       NO-UNDO INIT FALSE .
DEFINE VARIABLE oForm   AS Consultingwerk.WindowIntegrationKit.Forms.IEmbeddedWindowForm NO-UNDO .

/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE winkit-enable:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    RUN dispatch IN THIS-PROCEDURE ("enable") .
    
    IF VALID-OBJECT (oForm) THEN 
        Consultingwerk.Util.UltraToolbarsHelper:RefreshTools (oForm:ToolbarsManager) .


END PROCEDURE.

PROCEDURE winkit-view:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    RUN dispatch IN THIS-PROCEDURE ("view") .
    
    IF VALID-OBJECT (oForm) THEN 
        Consultingwerk.Util.UltraToolbarsHelper:RefreshTools (oForm:ToolbarsManager) .

END PROCEDURE.

PROCEDURE winkit-make-ribbon-group:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER poForm AS Consultingwerk.WindowIntegrationKit.Forms.IEmbeddedWindowForm NO-UNDO .
    DEFINE INPUT PARAMETER piPage AS INTEGER                                                       NO-UNDO .
    
    DEFINE VARIABLE cKey        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCaption    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTabKey     AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cTabCaption AS CHARACTER NO-UNDO.

    DEFINE VARIABLE oContextual AS Infragistics.Win.UltraWinToolbars.ContextualTabGroup NO-UNDO . 
    DEFINE VARIABLE oRibbonTab AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO . 
    
    IF lInited THEN 
        RETURN . 
        
    ASSIGN lInited = TRUE 
           oForm   = poForm . 

    ASSIGN cKey = Consultingwerk.Util.ProcedureHelper:ShortDotPName (THIS-PROCEDURE)
           cKey = REPLACE (cKey, ".", "_") 
        
           cCaption = FRAME {&frame-name}:PRIVATE-DATA .

    IF cCaption = "" THEN 
        ASSIGN cCaption = cKey .

    IF piPage = 0 THEN 
        oRibbonTab = poForm:ToolbarsManager:Ribbon:Tabs[0] .
    ELSE DO: 
        ASSIGN cTabKey = "page_" + STRING (piPage) .
        
        /* Create contextual ribbon tab */
        IF poForm:ToolbarsManager:Ribbon:Tabs:Exists (cTabKey) THEN 
            oRibbonTab = poForm:ToolbarsManager:Ribbon:Tabs [cTabKey]. 

        ELSE DO:

            ASSIGN cTabCaption = CAST (poForm, 
                                       Consultingwerk.WindowIntegrationKit.Forms.IEmbeddedWindowTabFolderForm):TabFolder:Tabs [piPage - 1]:Text . 
        
            oRibbonTab = poForm:ToolbarsManager:Ribbon:Tabs:Add (cTabKey) .
            oRibbonTab:Caption = cTabCaption . 
        
            oContextual = poForm:ToolbarsManager:Ribbon:ContextualTabGroups:Add (cTabKey) .
            oContextual:Tabs:Add (oRibbonTab) .
            oContextual:Caption = cTabCaption . 
            
            
        
        END.
    END.
    
    Consultingwerk.Util.UltraToolbarsHelper:BuildRibbonGroupFromFrame (FRAME {&FRAME-NAME}:HANDLE, 
                                                                       poForm:ToolbarsManager,
                                                                       oRibbonTab,
                                                                       cKey,
                                                                       cCaption,
                                                                       FALSE, 
                                                                       FALSE) . 
                                                                       
    IF VALID-OBJECT (oForm) THEN 
        Consultingwerk.Util.UltraToolbarsHelper:RefreshTools (oForm:ToolbarsManager) .
    
    

END PROCEDURE.


