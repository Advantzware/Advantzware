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

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Visual Design                                                                        
        Notes:      
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID InitializeComponent ():
		
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
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
        THIS-OBJECT:UseCharacterParameterValueCheckEditor:Location = NEW System.Drawing.Point(173, 90).
        THIS-OBJECT:UseCharacterParameterValueCheckEditor:Name = "UseCharacterParameterValueCheckEditor":U.
        THIS-OBJECT:UseCharacterParameterValueCheckEditor:Size = NEW System.Drawing.Size(193, 20).
        THIS-OBJECT:UseCharacterParameterValueCheckEditor:TabIndex = 12.
        THIS-OBJECT:UseCharacterParameterValueCheckEditor:Text = "Use Character Parameter Value":U.
        /*  */
        /* MethodNameTextEditor */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:MethodNameTextEditor:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:MethodNameTextEditor:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:MethodNameTextEditor:Location = NEW System.Drawing.Point(173, 63).
        THIS-OBJECT:MethodNameTextEditor:Name = "MethodNameTextEditor":U.
        THIS-OBJECT:MethodNameTextEditor:Size = NEW System.Drawing.Size(193, 21).
        THIS-OBJECT:MethodNameTextEditor:TabIndex = 9.
        /*  */
        /* ClassNameTextEditor */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ClassNameTextEditor:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ClassNameTextEditor:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ClassNameTextEditor:Location = NEW System.Drawing.Point(173, 36).
        THIS-OBJECT:ClassNameTextEditor:Name = "ClassNameTextEditor":U.
        THIS-OBJECT:ClassNameTextEditor:Size = NEW System.Drawing.Size(193, 21).
        THIS-OBJECT:ClassNameTextEditor:TabIndex = 11.
        /*  */
        /* CharacterParameterValueTextEditor */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:CharacterParameterValueTextEditor:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:CharacterParameterValueTextEditor:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:CharacterParameterValueTextEditor:Location = NEW System.Drawing.Point(173, 9).
        THIS-OBJECT:CharacterParameterValueTextEditor:Name = "CharacterParameterValueTextEditor":U.
        THIS-OBJECT:CharacterParameterValueTextEditor:Size = NEW System.Drawing.Size(193, 21).
        THIS-OBJECT:CharacterParameterValueTextEditor:TabIndex = 8.
        /*  */
        /* ultraLabel5 */
        /*  */
        THIS-OBJECT:ultraLabel5:Location = NEW System.Drawing.Point(13, 67).
        THIS-OBJECT:ultraLabel5:Name = "ultraLabel5":U.
        THIS-OBJECT:ultraLabel5:Size = NEW System.Drawing.Size(154, 23).
        THIS-OBJECT:ultraLabel5:TabIndex = 5.
        THIS-OBJECT:ultraLabel5:Text = "Method Name:":U.
        /*  */
        /* ultraLabel3 */
        /*  */
        THIS-OBJECT:ultraLabel3:Location = NEW System.Drawing.Point(13, 40).
        THIS-OBJECT:ultraLabel3:Name = "ultraLabel3":U.
        THIS-OBJECT:ultraLabel3:Size = NEW System.Drawing.Size(154, 23).
        THIS-OBJECT:ultraLabel3:TabIndex = 7.
        THIS-OBJECT:ultraLabel3:Text = "Class Name:":U.
        /*  */
        /* ultraLabel2 */
        /*  */
        THIS-OBJECT:ultraLabel2:Location = NEW System.Drawing.Point(13, 13).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        THIS-OBJECT:ultraLabel2:Size = NEW System.Drawing.Size(154, 23).
        THIS-OBJECT:ultraLabel2:TabIndex = 6.
        THIS-OBJECT:ultraLabel2:Text = "Character Parameter Value:":U.
        /*  */
        /* InvokeStaticMethodCallParameterControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:UseCharacterParameterValueCheckEditor).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:MethodNameTextEditor).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ClassNameTextEditor).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:CharacterParameterValueTextEditor).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel5).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel3).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:Name = "InvokeStaticMethodCallParameterControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(380, 127).
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
