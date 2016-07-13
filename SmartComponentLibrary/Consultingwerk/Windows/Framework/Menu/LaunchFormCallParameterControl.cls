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
    File        : LaunchFormCallParameterControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Wed Dec 05 11:56:56 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i} 

USING Consultingwerk.Framework.Collections.*               FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.SmartUserControl FROM PROPATH .
USING Consultingwerk.Util.*                                FROM PROPATH .  
USING Consultingwerk.Windows.Framework.Menu.*              FROM PROPATH .  
USING Consultingwerk.Windows.Framework.*                   FROM PROPATH . 
USING Progress.Lang.*                                      FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Menu.LaunchFormCallParameterControl 
    INHERITS CallParameterBaseControl: 
	
    DEFINE PRIVATE VARIABLE AllowMultipleCheckEditor AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ClassNameTextEditor AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE CharacterParameterValueTextEditor AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE FormStartTypeComboEditor AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel4 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE UseCharacterParameterValueCheckEditor AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.

    DEFINE PRIVATE VARIABLE oFormStartTypeValueList AS Infragistics.Win.ValueList NO-UNDO.

	CONSTRUCTOR PUBLIC LaunchFormCallParameterControl ():
        SUPER ().
        InitializeComponent ().
        
        PopulateComboEditor () .
        
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
        THIS-OBJECT:ClassNameTextEditor = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:CharacterParameterValueTextEditor = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel3 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:AllowMultipleCheckEditor = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:FormStartTypeComboEditor = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:ultraLabel4 = NEW Infragistics.Win.Misc.UltraLabel().
        CAST(THIS-OBJECT:UseCharacterParameterValueCheckEditor, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ClassNameTextEditor, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:CharacterParameterValueTextEditor, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:AllowMultipleCheckEditor, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:FormStartTypeComboEditor, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* UseCharacterParameterValueCheckEditor */
        /*  */
        THIS-OBJECT:UseCharacterParameterValueCheckEditor:Location = NEW System.Drawing.Point(173, 115).
        THIS-OBJECT:UseCharacterParameterValueCheckEditor:Name = "UseCharacterParameterValueCheckEditor":U.
        THIS-OBJECT:UseCharacterParameterValueCheckEditor:Size = NEW System.Drawing.Size(193, 20).
        THIS-OBJECT:UseCharacterParameterValueCheckEditor:TabIndex = 12.
        THIS-OBJECT:UseCharacterParameterValueCheckEditor:Text = "Use Character Parameter Value":U.
        /*  */
        /* ClassNameTextEditor */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ClassNameTextEditor:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ClassNameTextEditor:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ClassNameTextEditor:Location = NEW System.Drawing.Point(173, 36).
        THIS-OBJECT:ClassNameTextEditor:Name = "ClassNameTextEditor":U.
        THIS-OBJECT:ClassNameTextEditor:Size = NEW System.Drawing.Size(193, 21).
        THIS-OBJECT:ClassNameTextEditor:TabIndex = 11.
        /*  */
        /* CharacterParameterValueTextEditor */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:CharacterParameterValueTextEditor:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:CharacterParameterValueTextEditor:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:CharacterParameterValueTextEditor:Location = NEW System.Drawing.Point(173, 9).
        THIS-OBJECT:CharacterParameterValueTextEditor:Name = "CharacterParameterValueTextEditor":U.
        THIS-OBJECT:CharacterParameterValueTextEditor:Size = NEW System.Drawing.Size(193, 21).
        THIS-OBJECT:CharacterParameterValueTextEditor:TabIndex = 8.
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
        /* AllowMultipleCheckEditor */
        /*  */
        THIS-OBJECT:AllowMultipleCheckEditor:Location = NEW System.Drawing.Point(173, 89).
        THIS-OBJECT:AllowMultipleCheckEditor:Name = "AllowMultipleCheckEditor":U.
        THIS-OBJECT:AllowMultipleCheckEditor:Size = NEW System.Drawing.Size(193, 20).
        THIS-OBJECT:AllowMultipleCheckEditor:TabIndex = 12.
        THIS-OBJECT:AllowMultipleCheckEditor:Text = "Allow Multiple":U.
        /*  */
        /* FormStartTypeComboEditor */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:FormStartTypeComboEditor:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:FormStartTypeComboEditor:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:FormStartTypeComboEditor:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:FormStartTypeComboEditor:Location = NEW System.Drawing.Point(173, 60).
        THIS-OBJECT:FormStartTypeComboEditor:Name = "FormStartTypeComboEditor":U.
        THIS-OBJECT:FormStartTypeComboEditor:Size = NEW System.Drawing.Size(193, 21).
        THIS-OBJECT:FormStartTypeComboEditor:TabIndex = 14.
        /*  */
        /* ultraLabel4 */
        /*  */
        THIS-OBJECT:ultraLabel4:Location = NEW System.Drawing.Point(13, 64).
        THIS-OBJECT:ultraLabel4:Name = "ultraLabel4":U.
        THIS-OBJECT:ultraLabel4:Size = NEW System.Drawing.Size(154, 23).
        THIS-OBJECT:ultraLabel4:TabIndex = 13.
        THIS-OBJECT:ultraLabel4:Text = "Form Start Type:":U.
        /*  */
        /* LaunchFormCallParameterControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:BackColor = System.Drawing.SystemColors:Control.
        THIS-OBJECT:Controls:Add(THIS-OBJECT:FormStartTypeComboEditor).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel4).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:AllowMultipleCheckEditor).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:UseCharacterParameterValueCheckEditor).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ClassNameTextEditor).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:CharacterParameterValueTextEditor).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel3).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:Name = "LaunchFormCallParameterControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(380, 146).
        CAST(THIS-OBJECT:UseCharacterParameterValueCheckEditor, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ClassNameTextEditor, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:CharacterParameterValueTextEditor, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:AllowMultipleCheckEditor, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:FormStartTypeComboEditor, System.ComponentModel.ISupportInitialize):EndInit().
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
            ClassNameTextEditor:Text                      = ""
            CharacterParameterValueTextEditor:Text        = ""
            FormStartTypeComboEditor:SelectedIndex        = FormStartTypeComboEditor:ValueList:FindString ("default":U)
            AllowMultipleCheckEditor:Checked              = FALSE 
            UseCharacterParameterValueCheckEditor:Checked = FALSE 
            .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populates the ValueList of the ComboEditor
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID PopulateComboEditor ():
        DEFINE VARIABLE oCharacterList AS CharacterList NO-UNDO.
        
        oCharacterList = NEW CharacterList () . 
        oCharacterList:Add ("Default":U) .
        oCharacterList:Add ("MdiChild":U) .
        oCharacterList:Add ("SeparateForm":U) .
        oCharacterList:Add ("Dialog":U) .

        oFormStartTypeValueList = Consultingwerk.Util.ValueListHelper:FromList (oCharacterList).
        
        FormStartTypeComboEditor:ValueList = oFormStartTypeValueList.
        
        FormStartTypeComboEditor:Value = BOX ("default":U).
        
        FINALLY:
            GarbageCollectorHelper:DeleteObject (oCharacterList).
        END.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the data shown in the UI
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID UpdateUiData ():
        
        DEFINE VARIABLE oCallParameter AS LaunchFormCallParameter NO-UNDO.
    
        IF NOT TYPE-OF (CallParameterObject, LaunchFormCallParameter) THEN 
            RETURN.
            
        oCallParameter = CAST (CallParameterObject, LaunchFormCallParameter).
        
        ASSIGN
            ClassNameTextEditor:Text                      = oCallParameter:ClassName
            CharacterParameterValueTextEditor:Text        = oCallParameter:CharacterParameterValue
            AllowMultipleCheckEditor:Checked              = oCallParameter:AllowMultiple
            UseCharacterParameterValueCheckEditor:Checked = oCallParameter:UseCharacterParameterValue
            .
    
        IF VALID-OBJECT (oCallParameter:FormStartType) THEN 
            FormStartTypeComboEditor:Value = BOX (oCallParameter:FormStartType:ToString()) . 
        ELSE 
            FormStartTypeComboEditor:Value = ? .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the data of the CallParameter object after the user has
                 entered data to the UI
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID WritebackCallParameterToTextProperty ():
        
        DEFINE VARIABLE oCallParameter      AS LaunchFormCallParameter NO-UNDO .
        DEFINE VARIABLE cFormStartTypeValue AS CHARACTER               NO-UNDO .
        
        /* Marko Rüterbories, Consultingwerk Ltd. 20.09.2013
           If the CallParameterObject is not valid or the datatype does not match the needs
           the instance gets deleted and a new instance of the correct type is created. (SCL-119)
           
           Invalid cast from Consultingwerk.Windows.Framework.LaunchFormCallParameter to 
           Consultingwerk.SmartFramework.InvokeServiceMethodCallParameter. (12869) */
        IF NOT VALID-OBJECT (CallParameterObject) OR 
           NOT TYPE-OF (CallParameterObject, LaunchFormCallParameter) THEN DO:  
            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (CallParameterObject).
            CallParameterObject = NEW LaunchFormCallParameter ().
        END.

        oCallParameter = CAST (CallParameterObject, LaunchFormCallParameter).

        ASSIGN
            oCallParameter:ClassName                  = ClassNameTextEditor:Text
            oCallParameter:CharacterParameterValue    = CharacterParameterValueTextEditor:Text
            oCallParameter:AllowMultiple              = AllowMultipleCheckEditor:Checked
            oCallParameter:UseCharacterParameterValue = UseCharacterParameterValueCheckEditor:Checked
            .

        IF VALID-OBJECT (FormStartTypeComboEditor:SelectedItem) THEN DO:
            cFormStartTypeValue = FormStartTypeComboEditor:SelectedItem:DataValue:ToString ().
            
&IF DEFINED (NativeEnums) NE 0 &THEN
            oCallParameter:FormStartType = CAST (Progress.Lang.Enum:ToObject ("Consultingwerk.Windows.Framework.FormStartTypeEnum":U,
                                                                              cFormStartTypeValue), 
                                                 Consultingwerk.Windows.Framework.FormStartTypeEnum) . 
&ELSE            
            oCallParameter:FormStartType = Consultingwerk.Windows.Framework.FormStartTypeEnum:FromString (cFormStartTypeValue).
&ENDIF            
        END.
        
        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the LaunchFormCallParameterControl class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC LaunchFormCallParameterControl ():

    END DESTRUCTOR .

END CLASS.
