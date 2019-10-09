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
    File        : InvokeStaticMethodCallParameterControl
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Wed Dec 05 18:30:30 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.SmartUserControl FROM PROPATH .
USING Consultingwerk.Util.*                                FROM PROPATH .
USING Consultingwerk.Windows.Framework.Menu.*              FROM PROPATH .
USING Progress.Lang.*                                      FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Menu.InvokeStaticMethodCallParameterControl
    INHERITS CallParameterBaseControl:

    DEFINE PRIVATE VARIABLE ClassNameTextEditor AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE CharacterParameterValueTextEditor AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE MethodNameTextEditor AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel5 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE UseCharacterParameterValueCheckEditor AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.

    CONSTRUCTOR PUBLIC InvokeStaticMethodCallParameterControl ():
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
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Menu.InvokeStaticMethodCallParameterControl":U).
        THIS-OBJECT:UseCharacterParameterValueCheckEditor = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:MethodNameTextEditor = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ClassNameTextEditor = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:CharacterParameterValueTextEditor = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel5 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel3 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        CAST(THIS-OBJECT:UseCharacterParameterValueCheckEditor, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:MethodNameTextEditor, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ClassNameTextEditor, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:CharacterParameterValueTextEditor, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* UseCharacterParameterValueCheckEditor */
        /*  */
        resources:ApplyResources(THIS-OBJECT:UseCharacterParameterValueCheckEditor, "UseCharacterParameterValueCheckEditor":U).
        THIS-OBJECT:UseCharacterParameterValueCheckEditor:Name = "UseCharacterParameterValueCheckEditor":U.
        /*  */
        /* MethodNameTextEditor */
        /*  */
        resources:ApplyResources(THIS-OBJECT:MethodNameTextEditor, "MethodNameTextEditor":U).
        THIS-OBJECT:MethodNameTextEditor:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:MethodNameTextEditor:Name = "MethodNameTextEditor":U.
        /*  */
        /* ClassNameTextEditor */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ClassNameTextEditor, "ClassNameTextEditor":U).
        THIS-OBJECT:ClassNameTextEditor:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ClassNameTextEditor:Name = "ClassNameTextEditor":U.
        /*  */
        /* CharacterParameterValueTextEditor */
        /*  */
        resources:ApplyResources(THIS-OBJECT:CharacterParameterValueTextEditor, "CharacterParameterValueTextEditor":U).
        THIS-OBJECT:CharacterParameterValueTextEditor:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:CharacterParameterValueTextEditor:Name = "CharacterParameterValueTextEditor":U.
        /*  */
        /* ultraLabel5 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel5, "ultraLabel5":U).
        THIS-OBJECT:ultraLabel5:Name = "ultraLabel5":U.
        /*  */
        /* ultraLabel3 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel3, "ultraLabel3":U).
        THIS-OBJECT:ultraLabel3:Name = "ultraLabel3":U.
        /*  */
        /* ultraLabel2 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel2, "ultraLabel2":U).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        /*  */
        /* InvokeStaticMethodCallParameterControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:UseCharacterParameterValueCheckEditor).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:MethodNameTextEditor).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ClassNameTextEditor).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:CharacterParameterValueTextEditor).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel5).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel3).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:Name = "InvokeStaticMethodCallParameterControl":U.
        CAST(THIS-OBJECT:UseCharacterParameterValueCheckEditor, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:MethodNameTextEditor, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ClassNameTextEditor, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:CharacterParameterValueTextEditor, System.ComponentModel.ISupportInitialize):EndInit().
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
            ClassNameTextEditor:Text                      = "":U
            CharacterParameterValueTextEditor:Text        = "":U
            MethodNameTextEditor:Text                     = "":U
            UseCharacterParameterValueCheckEditor:Checked = FALSE
            .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the data shown in the UI
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID UpdateUiData ():
        DEFINE VARIABLE oCallParameter AS Consultingwerk.SmartFramework.InvokeStaticMethodCallParameter NO-UNDO.

        IF NOT TYPE-OF (CallParameterObject, Consultingwerk.SmartFramework.InvokeStaticMethodCallParameter) THEN
            RETURN.

        oCallParameter = CAST (CallParameterObject, Consultingwerk.SmartFramework.InvokeStaticMethodCallParameter).

        ASSIGN
            ClassNameTextEditor:Text                      = oCallParameter:ClassName
            CharacterParameterValueTextEditor:Text        = oCallParameter:CharacterParameterValue
            MethodNameTextEditor:Text                     = oCallParameter:MethodName
            UseCharacterParameterValueCheckEditor:Checked = oCallParameter:UseCharacterParameterValue
            .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the data of the CallParameter object after the user has
                 entered data to the UI
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID WritebackCallParameterToTextProperty ():
        DEFINE VARIABLE oCallParameter AS Consultingwerk.SmartFramework.InvokeStaticMethodCallParameter NO-UNDO.

        /* Marko Rüterbories, Consultingwerk Ltd. 20.09.2013
           If the CallParameterObject is not valid or the datatype does not match the needs
           the instance gets deleted and a new instance of the correct type is created. (SCL-119)

           Invalid cast from Consultingwerk.Windows.Framework.LaunchFormCallParameter to
           Consultingwerk.SmartFramework.InvokeServiceMethodCallParameter. (12869) */
        IF NOT VALID-OBJECT (CallParameterObject) OR
           NOT TYPE-OF (CallParameterObject, Consultingwerk.SmartFramework.InvokeStaticMethodCallParameter) THEN DO:
            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (CallParameterObject).
            CallParameterObject = NEW Consultingwerk.SmartFramework.InvokeStaticMethodCallParameter ().
        END.

        oCallParameter = CAST (CallParameterObject, Consultingwerk.SmartFramework.InvokeStaticMethodCallParameter).

        ASSIGN
            oCallParameter:ClassName                  = ClassNameTextEditor:Text
            oCallParameter:CharacterParameterValue    = CharacterParameterValueTextEditor:Text
            oCallParameter:MethodName                 = MethodNameTextEditor:Text
            oCallParameter:UseCharacterParameterValue = UseCharacterParameterValueCheckEditor:Checked
            .

        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the InvokeStaticMethodCallParameterControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC InvokeStaticMethodCallParameterControl ():

    END DESTRUCTOR .

END CLASS.
