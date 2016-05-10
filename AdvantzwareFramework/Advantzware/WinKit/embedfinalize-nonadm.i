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
    File        : embedfinalize.i
    Purpose     : 

    Syntax      :

    Description : Finalizes the embedding of an ABL window widget in an Mdi Child Window

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Dec 03 22:02:11 CET 2008
    Notes       : The preprocessor varible WinKitIgnoreCustomizations should 
                  be used to avoid any impact of customer customizations to 
                  the execution of the WinKit utilities.
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

&IF DEFINED (winkitactive) NE 0 &THEN

/* Mike Fechner, Consultingwerk Ltd. 08.12.2008
   If no MdiContainer is present, no oForm object has been created - the 
   window will not be embedded and will continue to run as is.  */
   
&IF DEFINED (WinKitVarsDefined) EQ 0 &THEN
DEFINE VARIABLE hWinKitToolbarRectangle AS HANDLE NO-UNDO.
DEFINE VARIABLE cWinKitToolbarFileName AS CHARACTER NO-UNDO.
&ENDIF
   
IF VALID-OBJECT (oForm) AND NOT oForm:Finalized THEN DO:
    
    /* procedural hook */
    IF CAN-DO (THIS-PROCEDURE:INTERNAL-ENTRIES, "finalizeEmbedding":U) THEN 
    DO ON ERROR UNDO, THROW:
        RUN finalizeEmbedding . 
        
        CATCH err AS Progress.Lang.Error :
        	Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.
    END.
    
    /* Mike Fechner, Consultingwerk Ltd. 22.07.2010
       Load custom toolbar design */    
    ASSIGN FILE-INFO:FILE-NAME    = THIS-PROCEDURE:FILE-NAME 
           cWinKitToolbarFileName = FILE-INFO:FULL-PATHNAME          
           cWinKitToolbarFileName = SUBSTRING (cWinKitToolbarFileName, 1, R-INDEX (cWinKitToolbarFileName, ".":U) - 1) + "_toolbar.xml":U .

    FILE-INFO:FILE-NAME = cWinKitToolbarFileName . 
    IF FILE-INFO:FULL-PATHNAME > "":U THEN 
        oForm:ToolbarsManager:LoadFromXml (cWinKitToolbarFileName) .             
   
    /* Mike Fechner, Consultingwerk Ltd. 11.11.2010
       oForm:MENU-BAR set in embedwindow.i or existing framework code
       for dynamic menus */
    IF VALID-HANDLE (oForm:MENU-BAR) THEN 
        IF TYPE-OF (oForm, Consultingwerk.Framework.IFormWithUltraToolbarsManager) THEN 
            Consultingwerk.Util.UltratoolbarsHelper:BuildUltraToolbarMenu (CAST(oForm, Consultingwerk.Framework.IFormWithUltraToolbarsManager)) .

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
                      Consultingwerk.Forms.BaseForm):DependentForms:Add (oFormControl) .                        
            ELSE                             
                CAST(CAST(oForm, Progress.Lang.Object), System.Windows.Forms.Form):Owner = Consultingwerk.Framework.FrameworkSettings:MdiContainer .  
        END.    




        
          /* Mike Fechner, Consultingwerk Ltd. 11.11.2010
             Set FRAME BGCOLOR to 33, which matches the Form Background color
             of Office 2007 applications, color33=227,239,255 */
          Consultingwerk.Util.WidgetHelper:SetFrameBackgroundColor
                  ({&WINDOW-NAME}:HANDLE, 
                   Consultingwerk.WindowIntegrationKit.Forms.WinKitForms:TabFolderBGColor, 
                   15, 
                   15) .


              
    IF NOT oForm:ShowAsDialog THEN 
        oFormControl:Show().

    
    Consultingwerk.Windows.API.Win32:LockWindowUpdate (oFormControl:Handle:ToInt32()) .
    
    oForm:FinalizeEmbedding() .

    Consultingwerk.Windows.API.Win32:LockWindowUpdate (0) .
    

    /* procedural hook */
    IF CAN-DO (THIS-PROCEDURE:INTERNAL-ENTRIES, "finalizeEmbeddingEnd":U) THEN 
    DO ON ERROR UNDO, THROW:
        RUN finalizeEmbeddingEnd . 
        
        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.
    END .
    

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

&GLOBAL-DEFINE WinKitVarsDefined
