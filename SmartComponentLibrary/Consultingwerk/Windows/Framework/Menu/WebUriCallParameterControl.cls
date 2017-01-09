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
    File        : WebUriCallParameterControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Wed Dec 05 11:56:56 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.SmartUserControl FROM PROPATH .
USING Consultingwerk.Util.*                                FROM PROPATH .  
USING Consultingwerk.Windows.Framework.Menu.*              FROM PROPATH .  
USING Consultingwerk.Windows.Framework.*                   FROM PROPATH . 
USING Progress.Lang.*                                      FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Menu.WebUriCallParameterControl 
    INHERITS CallParameterBaseControl: 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE UriTextEditor AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.

    DEFINE PRIVATE VARIABLE oFormStartTypeValueList AS Infragistics.Win.ValueList NO-UNDO.
    DEFINE PRIVATE VARIABLE UseExternalWebBrowserCheckEditor AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.

    CONSTRUCTOR PUBLIC WebUriCallParameterControl ():
        SUPER ().
        InitializeComponent ().
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END CONSTRUCTOR.

    /**
     * Purpose: Initializes the Visual Design
     * Notes:
     */
    @InitializeComponent.
    METHOD PRIVATE VOID InitializeComponent ():
        
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Menu.WebUriCallParameterControl":U).
        THIS-OBJECT:UseExternalWebBrowserCheckEditor = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:UriTextEditor = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        CAST(THIS-OBJECT:UseExternalWebBrowserCheckEditor, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:UriTextEditor, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* UseExternalWebBrowserCheckEditor */
        /*  */
        resources:ApplyResources(THIS-OBJECT:UseExternalWebBrowserCheckEditor, "UseExternalWebBrowserCheckEditor":U).
        THIS-OBJECT:UseExternalWebBrowserCheckEditor:Name = "UseExternalWebBrowserCheckEditor":U.
        /*  */
        /* UriTextEditor */
        /*  */
        resources:ApplyResources(THIS-OBJECT:UriTextEditor, "UriTextEditor":U).
        THIS-OBJECT:UriTextEditor:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:UriTextEditor:Name = "UriTextEditor":U.
        /*  */
        /* ultraLabel2 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel2, "ultraLabel2":U).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        /*  */
        /* WebUriCallParameterControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:BackColor = System.Drawing.SystemColors:Control.
        THIS-OBJECT:Controls:Add(THIS-OBJECT:UseExternalWebBrowserCheckEditor).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:UriTextEditor).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:Name = "WebUriCallParameterControl":U.
        CAST(THIS-OBJECT:UseExternalWebBrowserCheckEditor, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:UriTextEditor, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Clear all editor fields
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID InternalClearDataFields ():
        
        ASSIGN
            UriTextEditor:Text                       = "":U
            UseExternalWebBrowserCheckEditor:Checked = FALSE 
            .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the data shown in the UI
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID UpdateUiData ():
        
        DEFINE VARIABLE oCallParameter AS WebUriCallParameter NO-UNDO.
    
        IF NOT TYPE-OF (CallParameterObject, WebUriCallParameter) THEN 
            RETURN.
            
        oCallParameter = CAST (CallParameterObject, WebUriCallParameter).
        
        ASSIGN
            UriTextEditor:Text                       = oCallParameter:WebSiteLocation
            UseExternalWebBrowserCheckEditor:Checked = oCallParameter:UseExternalWebBrowser 
            .
    
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the data of the CallParameter object after the user has
                 entered data to the UI
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID WritebackCallParameterToTextProperty ():
        
        DEFINE VARIABLE oCallParameter      AS WebUriCallParameter NO-UNDO .
        DEFINE VARIABLE cFormStartTypeValue AS CHARACTER           NO-UNDO .
        
        /* Marko Rüterbories, Consultingwerk Ltd. 20.09.2013
           If the CallParameterObject is not valid or the datatype does not match the needs
           the instance gets deleted and a new instance of the correct type is created. (SCL-119)
           
           Invalid cast from Consultingwerk.Windows.Framework.WebUriCallParameter to 
           Consultingwerk.SmartFramework.InvokeServiceMethodCallParameter. (12869) */
        IF NOT VALID-OBJECT (CallParameterObject) OR 
           NOT TYPE-OF (CallParameterObject, WebUriCallParameter) THEN DO:  
            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (CallParameterObject).
            CallParameterObject = NEW WebUriCallParameter ().
        END.

        oCallParameter = CAST (CallParameterObject, WebUriCallParameter).

        ASSIGN
            oCallParameter:WebSiteLocation       = UriTextEditor:Text
            oCallParameter:UseExternalWebBrowser = UseExternalWebBrowserCheckEditor:Checked  
            .
        
        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.
    END METHOD .

END CLASS.
