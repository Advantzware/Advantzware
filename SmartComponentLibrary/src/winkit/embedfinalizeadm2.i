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
    File        : embedfinalizeadm2.i
    Purpose     : Include in initializeObject of SmartWindow, after standard behaviour. 
    Code must run after the dyntoolbar has created it's menu-bar.

    Syntax      :

    Description : Finalize embedding of SmartWindows

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue May 12 16:03:07 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */
  
/* Mike Fechner, Consultingwerk Ltd. 08.07.2009
   Conditional compile rule: Will compile in OpenEdge release 10.2A and above,
   This rule will break when release 40.x will be reached. Note PROVERSION
   returns a string, so this is alpha comparison, not numeric */
&IF NOT PROVERSION GE "4" AND PROVERSION GE "10.2A" &THEN
IF VALID-OBJECT(oForm) THEN DO:

    /* procedural hook */
    IF CAN-DO (TARGET-PROCEDURE:INTERNAL-ENTRIES, "finalizeEmbedding":U) THEN 
        RUN finalizeEmbedding IN TARGET-PROCEDURE NO-ERROR . 

    /* Mike Fechner, Consultingwerk Ltd. 22.07.2010
       Load custom toolbar design */
    DEFINE VARIABLE cWinKitTooblarFileName AS CHARACTER NO-UNDO.
    
    ASSIGN FILE-INFORMATION:FILE-NAME    = TARGET-PROCEDURE:FILE-NAME 
           cWinKitTooblarFileName = FILE-INFORMATION:FULL-PATHNAME          
           cWinKitTooblarFileName = SUBSTRING (cWinKitTooblarFileName, 1, R-INDEX (cWinKitTooblarFileName, ".":U) - 1) + "_toolbar.xml":U .

    FILE-INFORMATION:FILE-NAME = cWinKitTooblarFileName . 
    IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN 
        oForm:ToolbarsManager:LoadFromXml (cWinKitTooblarFileName) .        

    IF VALID-HANDLE (hMenuBar) THEN DO:
        
        ASSIGN oForm:MENU-BAR = hMenuBar.
        
        IF TYPE-OF (oForm, Consultingwerk.Framework.IFormWithUltraToolbarsManager) THEN 
            Consultingwerk.Util.UltraToolbarsHelper:BuildUltraToolbarMenu (CAST(oForm, Consultingwerk.Framework.IFormWithUltraToolbarsManager)) .
        
    END.

    /* Mike Fechner, Consultingwerk Ltd. 28.02.2012
       Hook to refine toolbar before the Form is shown */
    IF CAN-DO (THIS-PROCEDURE:INTERNAL-ENTRIES, "refineToolbar":U) THEN 
        RUN refineToolbar . 

    IF oForm:MakeMdiChild = TRUE THEN DO:         
        IF TYPE-OF(Consultingwerk.Framework.FrameworkSettings:MdiContainer,
             Consultingwerk.Framework.IMDIContainer) THEN
            CAST(Consultingwerk.Framework.FrameworkSettings:MdiContainer,
                 Consultingwerk.Framework.IMDIContainer):DeactivateWindowTitleEventHandler ().

        CAST(CAST(oForm, Progress.Lang.Object), System.Windows.Forms.Form):MdiParent = Consultingwerk.Framework.FrameworkSettings:MdiContainer.    
    END.
    ELSE 
        IF VALID-OBJECT(Consultingwerk.Framework.FrameworkSettings:MdiContainer) THEN DO:
        
            IF TYPE-OF (Consultingwerk.Framework.FrameworkSettings:MdiContainer,
                        Consultingwerk.Forms.BaseForm) THEN 
                        
                CAST (Consultingwerk.Framework.FrameworkSettings:MdiContainer,
                      Consultingwerk.Forms.BaseForm):DependentForms:Add (CAST(CAST (oForm, Progress.Lang.Object), Progress.Windows.Form)) .                        
            ELSE                             
                CAST(CAST(oForm, Progress.Lang.Object), System.Windows.Forms.Form):Owner = Consultingwerk.Framework.FrameworkSettings:MdiContainer .  
        END.    
    
    DEFINE VARIABLE hPageSource AS HANDLE NO-UNDO.
    DEFINE VARIABLE hFolderFrame AS HANDLE NO-UNDO.
    
    IF TYPE-OF (oForm, Consultingwerk.WindowIntegrationKit.Forms.IEmbeddedWindowTabFolderForm) THEN DO:
        {get PageSource hPageSource} .        
        
        IF VALID-HANDLE (hPageSource) THEN DO: 
            {get ContainerHandle hFolderFrame hPageSource} .
            
            IF VALID-HANDLE (hFolderFrame) THEN 
                ASSIGN CAST (oForm, Consultingwerk.WindowIntegrationKit.Forms.IEmbeddedWindowTabFolderForm):SplitterDistance =
                    hFolderFrame:Y - oForm:WindowContainerRowOffset .
            
        END.
    END.
    
    CAST(CAST(oForm, Progress.Lang.Object), System.Windows.Forms.Form):Show().
    
    oForm:FinalizeEmbedding() .
    
    /* procedural hook */
    IF CAN-DO (TARGET-PROCEDURE:INTERNAL-ENTRIES, "finalizeEmbeddingEnd":U) THEN 
        RUN finalizeEmbeddingEnd IN TARGET-PROCEDURE NO-ERROR . 
    
    /* Mike Fechner, Consultingwerk Ltd. 31/01/2014
       Hide Ribbon, when there are no Tabs */
    IF oForm:ToolbarsManager:Ribbon:Tabs:Count = 0 AND NOT VALID-OBJECT (oForm:ToolbarsManager:MdiParentManager) THEN 
        oForm:ToolbarsManager:Ribbon:Visible = FALSE . 
    
    
    IF oForm:MakeMdiChild = TRUE AND         
       TYPE-OF(Consultingwerk.Framework.FrameworkSettings:MdiContainer,
         Consultingwerk.Framework.IMDIContainer) THEN DO:
        CAST(Consultingwerk.Framework.FrameworkSettings:MdiContainer,
             Consultingwerk.Framework.IMDIContainer):ActivateWindowTitleEventHandler ().

        CAST(Consultingwerk.Framework.FrameworkSettings:MdiContainer,
             Consultingwerk.Framework.IMDIContainer):MdiChildActivateHandler () .
    END.
END.
&ENDIF
