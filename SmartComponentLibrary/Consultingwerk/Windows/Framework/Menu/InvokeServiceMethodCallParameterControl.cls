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
    File        : InvokeServiceMethodCallParameterControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Wed Dec 05 18:33:18 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.SmartUserControl FROM PROPATH .
USING Consultingwerk.Util.*                                FROM PROPATH .  
USING Consultingwerk.SmartComponents.Base.*                FROM PROPATH .
USING Consultingwerk.Windows.Framework.Menu.*              FROM PROPATH .
USING Infragistics.Win.UltraWinEditors.*                   FROM ASSEMBLY .
USING Progress.Lang.*                                      FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Windows.Framework.Menu.InvokeServiceMethodCallParameterControl 
    INHERITS CallParameterBaseControl: 
	
    DEFINE PRIVATE VARIABLE ClassNameTextEditor AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE CharacterParameterValueTextEditor AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE MethodNameTextEditor AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE InterfaceNameTextEditor AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel5 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel4 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE UseCharacterParameterValueCheckEditor AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
	
	{Consultingwerk/Util/TempTables/ttClassNames.i}
	
	CONSTRUCTOR PUBLIC InvokeServiceMethodCallParameterControl ():
        SUPER ().
        InitializeComponent ().
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	    Purpose: Event handler for the EditorButtonClick event of the ClassNameTextEditor
	             Control
	    Notes:
	    @param sender The reference to the object that raised the event
	    @param e The EditorButtonEventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ClassNameTextEditor_EditorButtonClick (sender AS System.Object, 
	                                                           e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):
		
        DEFINE VARIABLE oForm         AS Consultingwerk.SmartComponents.Support.BusinessEntityPickerForm NO-UNDO .
        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult                               NO-UNDO .

        DEFINE VARIABLE oInfo         AS Infragistics.Win.Misc.UltraDesktopAlertWindowInfo               NO-UNDO . 

        IF THIS-OBJECT:InterfaceNameTextEditor:Text > "":U THEN .
        ELSE 
            RETURN . 

        oInfo = Consultingwerk.SmartComponents.Support.DesktopAlertHelper:Show (THIS-OBJECT:FindForm():Text,
                                                                                "Scanning Project Folder(s) for Service Classes."{&TRAN}) .

        Consultingwerk.Util.ClassHelper:GetClassNamesInClassPath (THIS-OBJECT:InterfaceNameTextEditor:Text, 
                                                                  FALSE, 
                                                                  OUTPUT TABLE ttClassNames BY-REFERENCE) .

        oInfo:Close () .

        oForm = NEW Consultingwerk.SmartComponents.Support.BusinessEntityPickerForm () .
        oForm:Text = "Select Service class"{&TRAN} .
        
        oForm:ReceiveBusinessEntityNames (TABLE ttClassNames) .
        
/*        WAIT-FOR oForm:ShowDialog () SET oDialogResult .*/

        /* Mike Fechner, Consultingwerk Ltd. 14.07.2010
           WAIT-FOR Workaround */
        ASSIGN oDialogResult = Consultingwerk.SmartComponents.FormHelper:ShowDialog(oForm) .
        
        IF Progress.Util.EnumHelper:AreEqual(oDialogResult, 
                                             System.Windows.Forms.DialogResult:Ok) THEN 
                                    
            ASSIGN THIS-OBJECT:ClassNameTextEditor:Text = oForm:GetSelectedBusinessEntityName () .

        CATCH err AS Progress.Lang.Error:
        	ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Visual Design                                                                        
        Notes:      
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID InitializeComponent ():
		
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        THIS-OBJECT:CharacterParameterValueTextEditor = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ClassNameTextEditor = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:InterfaceNameTextEditor = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:MethodNameTextEditor = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:UseCharacterParameterValueCheckEditor = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel3 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel4 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel5 = NEW Infragistics.Win.Misc.UltraLabel().
        CAST(THIS-OBJECT:CharacterParameterValueTextEditor, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ClassNameTextEditor, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:InterfaceNameTextEditor, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:MethodNameTextEditor, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:UseCharacterParameterValueCheckEditor, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* CharacterParameterValueTextEditor */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:CharacterParameterValueTextEditor:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:CharacterParameterValueTextEditor:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:CharacterParameterValueTextEditor:Location = NEW System.Drawing.Point(173, 9).
        THIS-OBJECT:CharacterParameterValueTextEditor:Name = "CharacterParameterValueTextEditor":U.
        THIS-OBJECT:CharacterParameterValueTextEditor:Size = NEW System.Drawing.Size(193, 21).
        THIS-OBJECT:CharacterParameterValueTextEditor:TabIndex = 1.
        /*  */
        /* ClassNameTextEditor */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ClassNameTextEditor:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton1:Text = "...":U.
        THIS-OBJECT:ClassNameTextEditor:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:ClassNameTextEditor:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ClassNameTextEditor:Location = NEW System.Drawing.Point(173, 63).
        THIS-OBJECT:ClassNameTextEditor:Name = "ClassNameTextEditor":U.
        THIS-OBJECT:ClassNameTextEditor:Size = NEW System.Drawing.Size(192, 21).
        THIS-OBJECT:ClassNameTextEditor:TabIndex = 5.
        THIS-OBJECT:ClassNameTextEditor:EditorButtonClick:Subscribe(THIS-OBJECT:ClassNameTextEditor_EditorButtonClick).
        THIS-OBJECT:ClassNameTextEditor:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        /*  */
        /* InterfaceNameTextEditor */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:InterfaceNameTextEditor:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:InterfaceNameTextEditor:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:InterfaceNameTextEditor:Location = NEW System.Drawing.Point(173, 36).
        THIS-OBJECT:InterfaceNameTextEditor:Name = "InterfaceNameTextEditor":U.
        THIS-OBJECT:InterfaceNameTextEditor:Size = NEW System.Drawing.Size(192, 21).
        THIS-OBJECT:InterfaceNameTextEditor:TabIndex = 3.
        THIS-OBJECT:InterfaceNameTextEditor:ValueChanged:Subscribe(THIS-OBJECT:InterfaceNameTextEditor_ValueChanged).
        /*  */
        /* MethodNameTextEditor */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:MethodNameTextEditor:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:MethodNameTextEditor:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:MethodNameTextEditor:Location = NEW System.Drawing.Point(173, 90).
        THIS-OBJECT:MethodNameTextEditor:Name = "MethodNameTextEditor":U.
        THIS-OBJECT:MethodNameTextEditor:Size = NEW System.Drawing.Size(193, 21).
        THIS-OBJECT:MethodNameTextEditor:TabIndex = 7.
        /*  */
        /* UseCharacterParameterValueCheckEditor */
        /*  */
        THIS-OBJECT:UseCharacterParameterValueCheckEditor:Location = NEW System.Drawing.Point(173, 117).
        THIS-OBJECT:UseCharacterParameterValueCheckEditor:Name = "UseCharacterParameterValueCheckEditor":U.
        THIS-OBJECT:UseCharacterParameterValueCheckEditor:Size = NEW System.Drawing.Size(192, 20).
        THIS-OBJECT:UseCharacterParameterValueCheckEditor:TabIndex = 8.
        THIS-OBJECT:UseCharacterParameterValueCheckEditor:Text = "Use character parameter value":U.
        /*  */
        /* ultraLabel2 */
        /*  */
        THIS-OBJECT:ultraLabel2:Location = NEW System.Drawing.Point(13, 13).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        THIS-OBJECT:ultraLabel2:Size = NEW System.Drawing.Size(154, 23).
        THIS-OBJECT:ultraLabel2:TabIndex = 0.
        THIS-OBJECT:ultraLabel2:Text = "Character Parameter Value:":U.
        /*  */
        /* ultraLabel3 */
        /*  */
        THIS-OBJECT:ultraLabel3:Location = NEW System.Drawing.Point(13, 40).
        THIS-OBJECT:ultraLabel3:Name = "ultraLabel3":U.
        THIS-OBJECT:ultraLabel3:Size = NEW System.Drawing.Size(154, 23).
        THIS-OBJECT:ultraLabel3:TabIndex = 2.
        THIS-OBJECT:ultraLabel3:Text = "Interface Name:":U.
        /*  */
        /* ultraLabel4 */
        /*  */
        THIS-OBJECT:ultraLabel4:Location = NEW System.Drawing.Point(13, 67).
        THIS-OBJECT:ultraLabel4:Name = "ultraLabel4":U.
        THIS-OBJECT:ultraLabel4:Size = NEW System.Drawing.Size(154, 23).
        THIS-OBJECT:ultraLabel4:TabIndex = 4.
        THIS-OBJECT:ultraLabel4:Text = "Class Name:":U.
        /*  */
        /* ultraLabel5 */
        /*  */
        THIS-OBJECT:ultraLabel5:Location = NEW System.Drawing.Point(13, 94).
        THIS-OBJECT:ultraLabel5:Name = "ultraLabel5":U.
        THIS-OBJECT:ultraLabel5:Size = NEW System.Drawing.Size(154, 23).
        THIS-OBJECT:ultraLabel5:TabIndex = 6.
        THIS-OBJECT:ultraLabel5:Text = "Method Name:":U.
        /*  */
        /* InvokeServiceMethodCallParameterControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:UseCharacterParameterValueCheckEditor).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:MethodNameTextEditor).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:InterfaceNameTextEditor).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ClassNameTextEditor).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:CharacterParameterValueTextEditor).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel5).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel4).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel3).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:Name = "InvokeServiceMethodCallParameterControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(380, 146).
        CAST(THIS-OBJECT:CharacterParameterValueTextEditor, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ClassNameTextEditor, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:InterfaceNameTextEditor, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:MethodNameTextEditor, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:UseCharacterParameterValueCheckEditor, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the KeyDown handler of the Text boxes with an 
                 EditorButton (Lookup)
        Notes:   Maps F3 to EditorButtonClick
        @param sender The reference to the object that raised the event
        @param e The KeyEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID LookupKeyDownHandler (sender AS System.Object, 
                                              e AS System.Windows.Forms.KeyEventArgs):

        DEFINE VARIABLE cControlName AS CHARACTER NO-UNDO.
        
        IF e:Handled = FALSE AND Progress.Util.EnumHelper:AreEqual (e:KeyCode, SmartLookupSettings:ShortcutKey) 
            AND THIS-OBJECT:Enabled THEN DO:

            ASSIGN cControlName = CAST (sender, System.Windows.Forms.Control):Name .

            DYNAMIC-INVOKE (THIS-OBJECT, 
                            SUBSTITUTE ("&1_EditorButtonClick":U, cControlName), 
                            sender, 
                            NEW EditorButtonEventArgs (?, ?)) .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .                
        END CATCH.

    END METHOD .

	/*------------------------------------------------------------------------------
	    Purpose: Event handler for the ValueChanged event of the InterfaceNameTextEditor
	            Control
	    Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event 
	------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID InterfaceNameTextEditor_ValueChanged (sender AS System.Object, 
	                                                          e AS System.EventArgs):
		
		THIS-OBJECT:ClassNameTextEditor:ButtonsRight[0]:Enabled = 
		    (THIS-OBJECT:InterfaceNameTextEditor:Text > "":U) . 

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Clear all editor fields
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID InternalClearDataFields ():
        
        ASSIGN
            ClassNameTextEditor:Text                      = "":U
            CharacterParameterValueTextEditor:Text        = "":U
            MethodNameTextEditor:Text                     = "":U 
            InterfaceNameTextEditor:Text                  = "":U 
            UseCharacterParameterValueCheckEditor:Checked = FALSE 
            .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the data shown in the UI
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID UpdateUiData ():
        DEFINE VARIABLE oCallParameter AS Consultingwerk.SmartFramework.InvokeServiceMethodCallParameter NO-UNDO.
    
        IF NOT TYPE-OF (CallParameterObject, Consultingwerk.SmartFramework.InvokeServiceMethodCallParameter) THEN 
            RETURN.
            
        oCallParameter = CAST (CallParameterObject, Consultingwerk.SmartFramework.InvokeServiceMethodCallParameter).
        
        ASSIGN 
            ClassNameTextEditor:Text                      = oCallParameter:ClassName
            CharacterParameterValueTextEditor:Text        = oCallParameter:CharacterParameterValue
            MethodNameTextEditor:Text                     = oCallParameter:MethodName
            InterfaceNameTextEditor:Text                  = oCallParameter:InterfaceName
            UseCharacterParameterValueCheckEditor:Checked = oCallParameter:UseCharacterParameterValue
            .
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the data of the CallParameter object after the user has
                 entered data to the UI
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID WritebackCallParameterToTextProperty ():
        DEFINE VARIABLE oCallParameter AS Consultingwerk.SmartFramework.InvokeServiceMethodCallParameter NO-UNDO.
        
        /* Marko Rüterbories, Consultingwerk Ltd. 20.09.2013
           If the CallParameterObject is not valid or the datatype does not match the needs
           the instance gets deleted and a new instance of the correct type is created. (SCL-119)
           
           Invalid cast from Consultingwerk.Windows.Framework.LaunchFormCallParameter to 
           Consultingwerk.SmartFramework.InvokeServiceMethodCallParameter. (12869) */
        IF NOT VALID-OBJECT (CallParameterObject) OR 
           NOT TYPE-OF (CallParameterObject, Consultingwerk.SmartFramework.InvokeServiceMethodCallParameter) THEN DO:  
            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (CallParameterObject).
            CallParameterObject = NEW Consultingwerk.SmartFramework.InvokeServiceMethodCallParameter ().
        END.

        oCallParameter = CAST (CallParameterObject, Consultingwerk.SmartFramework.InvokeServiceMethodCallParameter).

        ASSIGN
            oCallParameter:ClassName                  = ClassNameTextEditor:Text
            oCallParameter:CharacterParameterValue    = CharacterParameterValueTextEditor:Text
            oCallParameter:MethodName                 = MethodNameTextEditor:Text
            oCallParameter:InterfaceName              = InterfaceNameTextEditor:Text
            oCallParameter:UseCharacterParameterValue = UseCharacterParameterValueCheckEditor:Checked
            .

        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the InvokeServiceMethodCallParameterControl class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC InvokeServiceMethodCallParameterControl ():
        
    END DESTRUCTOR .

END CLASS.
