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

DEFINE VARIABLE oPanelRibbonTab AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO .
DEFINE VARIABLE cPanelRibbonGroupKey AS CHARACTER NO-UNDO .

DEFINE VARIABLE hUpdateButton AS HANDLE NO-UNDO.

{methods/defines/lValidateError.i}

/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE winkit-enable:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    RUN dispatch IN THIS-PROCEDURE ("enable") .

    IF VALID-OBJECT (oForm) THEN
        Consultingwerk.Util.UltraToolbarsHelper:RefreshTools (oForm:ToolbarsManager, FALSE, FALSE) .

END PROCEDURE.

PROCEDURE winkit-update-button:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcState AS CHARACTER NO-UNDO.
    
    IF VALID-HANDLE (hUpdateButton) THEN DO:
        CASE ipcState:
            WHEN "update-begin" THEN
            {methods/setButton.i hUpdateButton "Save"} 
            WHEN "update-complete" THEN
            {methods/setButton.i hUpdateButton "Update"} 
        END CASE.
        RUN winkit-view IN THIS-PROCEDURE.
    END.
    
END PROCEDURE.

PROCEDURE winkit-view:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    RUN dispatch IN THIS-PROCEDURE ("view") .

    IF VALID-OBJECT (oForm) THEN
        Consultingwerk.Util.UltraToolbarsHelper:RefreshTools (oForm:ToolbarsManager, FALSE, FALSE) .

END PROCEDURE.

PROCEDURE winkit-make-ribbon-group:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER poForm AS Consultingwerk.WindowIntegrationKit.Forms.IEmbeddedWindowForm NO-UNDO .
    DEFINE INPUT PARAMETER piPage AS INTEGER                                                       NO-UNDO .

    DEFINE VARIABLE cKey          AS CHARACTER                                                     NO-UNDO .
    DEFINE VARIABLE cCaption      AS CHARACTER                                                     NO-UNDO .
    DEFINE VARIABLE cTabKey       AS CHARACTER                                                     NO-UNDO .
    DEFINE VARIABLE cTabCaption   AS CHARACTER                                                     NO-UNDO .

    DEFINE VARIABLE oContextual   AS Infragistics.Win.UltraWinToolbars.ContextualTabGroup          NO-UNDO .
    DEFINE VARIABLE oRibbonTab    AS Infragistics.Win.UltraWinToolbars.RibbonTab                   NO-UNDO .
    DEFINE VARIABLE cPrefix       AS CHARACTER                                                     NO-UNDO .
    DEFINE VARIABLE hContainer    AS HANDLE                                                        NO-UNDO .

    IF NOT VALID-OBJECT (poForm) OR NOT VALID-OBJECT (poForm:ToolbarsManager) THEN
        RETURN .

    IF lInited THEN
        RETURN .

    ASSIGN lInited = TRUE
           oForm   = poForm .

    ASSIGN cKey = Consultingwerk.Util.ProcedureHelper:ShortDotPName (THIS-PROCEDURE)
           cKey = REPLACE (cKey, ".", "_")
           cCaption = FRAME {&FRAME-NAME}:PRIVATE-DATA .

    IF cCaption EQ "" THEN
        ASSIGN cCaption = cKey .
        
    IF cCaption EQ "ADM-PANEL" THEN
        cCaption = "".
        
    IF piPage = 0 THEN
        oRibbonTab = poForm:ToolbarsManager:Ribbon:Tabs[0] .
    ELSE DO:
        ASSIGN cTabKey = "page_" + STRING (piPage) .
        
        /* Create contextual ribbon tab */
        IF poForm:ToolbarsManager:Ribbon:Tabs:Exists (cTabKey) THEN
            oRibbonTab = poForm:ToolbarsManager:Ribbon:Tabs [cTabKey].
        ELSE DO ON ERROR UNDO, THROW:
            IF CAST (poForm,
                     Consultingwerk.WindowIntegrationKit.Forms.IEmbeddedWindowTabFolderForm):TabFolder:Tabs:Count < piPage THEN
            RETURN .
            
            ASSIGN cTabCaption = CAST (poForm,
                                       Consultingwerk.WindowIntegrationKit.Forms.IEmbeddedWindowTabFolderForm):TabFolder:Tabs [piPage - 1]:Text 
            oRibbonTab         = poForm:ToolbarsManager:Ribbon:Tabs:Add (cTabKey) 
            oRibbonTab:Caption = cTabCaption 
            oContextual        = poForm:ToolbarsManager:Ribbon:ContextualTabGroups:Add (cTabKey) .
            
            oContextual:Tabs:Add (oRibbonTab) .
            
            oContextual:Caption = cTabCaption .
            
            CATCH err AS Progress.Lang.Error :
            	MESSAGE "Issue accessing tab-page" SKIP
            	        "piPage" piPage SKIP
            	        "Number of tabs:"	CAST (poForm,
                                       Consultingwerk.WindowIntegrationKit.Forms.IEmbeddedWindowTabFolderForm):TabFolder:Tabs:Count SKIP
                         err:GetMessage(1)
                         err:CallStack
                     VIEW-AS ALERT-BOX .
                     
                 RETURN . 
                                       
            END CATCH.
            
        END.
    END.

    hContainer = poForm:ProcedureHandle .

    ASSIGN cPrefix = SUBSTITUTE ("&1-&2-&3":U,
                                 THIS-PROCEDURE:FILE-NAME,
                                 hContainer:FILE-NAME,
                                 STRING (piPage)) .

    Consultingwerk.Util.UltraToolbarsHelper:BuildRibbonGroupFromFrame (FRAME {&FRAME-NAME}:HANDLE,
                                                                       poForm:ToolbarsManager,
                                                                       oRibbonTab,
                                                                       cKey,
                                                                       cCaption,
                                                                       FALSE,
                                                                       FALSE,
                                                                       FALSE,
                                                                       0,
                                                                       cPrefix,
                                                                       "":U) .

    ASSIGN oPanelRibbonTab = oRibbonTab
           cPanelRibbonGroupKey = cKey .

    IF VALID-OBJECT (oForm) THEN
        Consultingwerk.Util.UltraToolbarsHelper:RefreshTools (oForm:ToolbarsManager, FALSE, FALSE) .

    FRAME {&FRAME-NAME}:HIDDEN = TRUE .
    FRAME {&FRAME-NAME}:MOVE-TO-BOTTOM () .

END PROCEDURE.

&GLOBAL-DEFINE EXCLUDE-set-position .

PROCEDURE set-position :
/* -----------------------------------------------------------
  Purpose:     Moves an object to a specified position.
  Parameters:  ROW and COLUMN
  Notes:
-------------------------------------------------------------*/

    DEFINE INPUT PARAMETER p-row    AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER p-col    AS DECIMAL NO-UNDO.

    DEFINE VARIABLE adm-object-hdl AS HANDLE NO-UNDO.

    IF Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive = FALSE THEN DO:

        ASSIGN adm-object-hdl = FRAME {&frame-name}:HANDLE .

        IF VALID-HANDLE(adm-object-hdl) THEN
        DO:
          /* If this is a Window or a Dialog box which is being positioned,
             then the special value 0 means to center the object in that
             dimension (0,0 means center on the screen - 0 can be used to
             signal this because 0 is an invalid row or column position). */
          &IF "{&ADM-CONTAINER}":U NE "":U &THEN
            DEFINE VARIABLE parent-hdl AS HANDLE NO-UNDO.
            IF adm-object-hdl:TYPE = "WINDOW":U THEN
            DO:
              IF p-row = 0 THEN p-row =
                (SESSION:HEIGHT-CHARS - adm-object-hdl:HEIGHT-CHARS) / 2.
              IF p-col = 0 THEN p-col =
                (SESSION:WIDTH-CHARS - adm-object-hdl:WIDTH-CHARS) / 2.
            END.
            /* A Dialog naturally centers on its parent and positions relative
               to its parent, so we must adjust for that. */
            ELSE IF adm-object-hdl:TYPE = "DIALOG-BOX":U THEN
            DO:
              parent-hdl = adm-object-hdl:PARENT.
              IF p-row = 0 THEN p-row =
                ((SESSION:HEIGHT-CHARS - adm-object-hdl:HEIGHT-CHARS) / 2) -
                  parent-hdl:ROW.
              IF p-col = 0 THEN p-col =
                ((SESSION:WIDTH-CHARS - adm-object-hdl:WIDTH-CHARS) / 2) -
                  parent-hdl:COL.
            END.
            /* If the row or column wound up being between 0 and 1 after the
               calculation, change it, because otherwise Progress will complain. */
            IF p-row GE 0 AND p-row < 1 THEN p-row = 1.
            IF p-col GE 0 AND p-col < 1 THEN p-col = 1.
          &ENDIF
          /* Set object's position */
          ASSIGN adm-object-hdl:ROW    =   p-row
                 adm-object-hdl:COLUMN =   p-col.
        END.

    END.

    IF VALID-OBJECT (oPanelRibbonTab) THEN
        oPanelRibbonTab:Groups[cPanelRibbonGroupKey]:MergeOrder = p-col .

END PROCEDURE.
