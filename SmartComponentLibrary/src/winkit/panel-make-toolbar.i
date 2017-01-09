/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
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
    File        : panel-make-toolbar.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jun 16 15:53:28 CEST 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

  FINALLY:
        DEFINE VARIABLE hContainer     AS HANDLE NO-UNDO .
        DEFINE VARIABLE oForm          AS Consultingwerk.WindowIntegrationKit.Forms.IEmbeddedWindowForm NO-UNDO . 
        DEFINE VARIABLE cContainerType AS CHARACTER NO-UNDO .
        
        RUN get-link-handle IN adm-broker-hdl 
                    (THIS-PROCEDURE, 
                     "CONTAINER-SOURCE":U, 
                     OUTPUT hContainer) .
        
        /* Mike Fechner, Consultingwerk Ltd. 07.07.2010
           Currently the Ribbon is only linked when in a toolbar. */
        RUN get-attribute IN hContainer ("TYPE":U) .
        cContainerType = RETURN-VALUE .

        IF NOT cContainerType MATCHES "*Window*":U THEN 
            RETURN . 
                
        REPEAT:
            IF NOT VALID-HANDLE (hContainer) THEN 
                LEAVE . 
            
            RUN get-attribute IN hContainer ("TYPE":U) .
            cContainerType = RETURN-VALUE .
            
            IF cContainerType MATCHES "*Window*":U THEN 
                LEAVE . 
                
            RUN get-link-handle IN adm-broker-hdl 
                        (hContainer, 
                         "CONTAINER-SOURCE":U, 
                         OUTPUT hContainer) .
        END.
        
        IF VALID-HANDLE (hContainer) THEN DO:
            DEFINE VARIABLE hContainerWindow AS HANDLE NO-UNDO.
            RUN GET-ATTRIBUTE IN hContainer ("ADM-OBJECT-HANDLE":U) .

            ASSIGN hContainerWindow = WIDGET-HANDLE (RETURN-VALUE) . 

            ASSIGN oForm = DYNAMIC-FUNCTION ("getEmbeddedWindowForm":U IN hContainer) 
                NO-ERROR .        

            &IF DEFINED (make-ribbon) NE 0 &THEN
            DEFINE VARIABLE oFrameRibbonTab AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO .    
            DEFINE VARIABLE cRibbonTabText AS CHARACTER NO-UNDO.
            
            IF hContainerWindow:TITLE > "":U THEN . 
            ELSE hContainerWindow:TITLE = hContainer:FILE-NAME .
            
            ASSIGN cRibbonTabText = hContainerWindow:TITLE .
            
            IF INDEX (cRibbonTabText, "[":U) > 0 THEN 
                cRibbonTabText = SUBSTRING (cRibbonTabText, 1, INDEX (cRibbonTabText, "[":U) - 1) . 
        
            IF oForm:ToolbarsManager:Ribbon:Tabs:Exists (cRibbonTabText) THEN DO: 
        
            END.
            ELSE DO:
                oFrameRibbonTab = NEW Infragistics.Win.UltraWinToolbars.RibbonTab (cRibbonTabText) .
                
                oForm:ToolbarsManager:Ribbon:Tabs:Add (oFrameRibbonTab) .
            END.
        
            Consultingwerk.Util.UltraToolbarsHelper:BuildRibbonGroupFromFrame 
                (FRAME {&frame-name}:HANDLE,
                 oForm:ToolbarsManager,
                 oFrameRibbonTab,
                 "Toolbar":U,
                 "Tools":U, 
                 FALSE,
                 FALSE,
                 1.5).            
            &ENDIF
        
            IF VALID-OBJECT (oForm) THEN 
                Consultingwerk.Util.UltraToolbarsHelper:BuildToolbarFromFrame
                    (FRAME {&FRAME-NAME}:HANDLE, oForm:ToolbarsManager) .    
        END.
  END FINALLY.
         