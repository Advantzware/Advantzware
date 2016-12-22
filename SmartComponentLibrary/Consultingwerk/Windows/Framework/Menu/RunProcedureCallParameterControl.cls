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
    File        : RunProcedureCallParameterControl
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
USING Consultingwerk.SmartFramework.*                      FROM PROPATH . 
USING Consultingwerk.Windows.Framework.Menu.*              FROM PROPATH .  
USING Consultingwerk.Windows.Framework.*                   FROM PROPATH . 
USING Progress.Lang.*                                      FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Menu.RunProcedureCallParameterControl 
    INHERITS CallParameterBaseControl: 
	
    DEFINE PRIVATE VARIABLE AllowMultipleCheckEditor                    AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE CharacterParameterValueTextEditor           AS Infragistics.Win.UltraWinEditors.UltraTextEditor  NO-UNDO.
    DEFINE PRIVATE VARIABLE components                                  AS System.ComponentModel.IContainer                  NO-UNDO.
    DEFINE PRIVATE VARIABLE ReactivateInternalProcedureParameterTextBox AS Infragistics.Win.UltraWinEditors.UltraTextEditor  NO-UNDO.
    DEFINE PRIVATE VARIABLE InitializeInternalProcedureParameterTextBox AS Infragistics.Win.UltraWinEditors.UltraTextEditor  NO-UNDO.
    DEFINE PRIVATE VARIABLE RunPersistentCheckBox                       AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1                                 AS Infragistics.Win.Misc.UltraLabel                  NO-UNDO.
    DEFINE PRIVATE VARIABLE ReactivateInternalProcedureTextBox          AS Infragistics.Win.UltraWinEditors.UltraTextEditor  NO-UNDO.
    DEFINE PRIVATE VARIABLE ProcedureNameTextEditor                     AS Infragistics.Win.UltraWinEditors.UltraTextEditor  NO-UNDO.
    DEFINE PRIVATE VARIABLE InitializeInternalProcedureTextBox          AS Infragistics.Win.UltraWinEditors.UltraTextEditor  NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3                                 AS Infragistics.Win.Misc.UltraLabel                  NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2                                 AS Infragistics.Win.Misc.UltraLabel                  NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel4                                 AS Infragistics.Win.Misc.UltraLabel                  NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel6                                 AS Infragistics.Win.Misc.UltraLabel                  NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel5                                 AS Infragistics.Win.Misc.UltraLabel                  NO-UNDO.
    DEFINE PRIVATE VARIABLE UseCharacterParameterValueCheckEditor       AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.

    DEFINE PRIVATE VARIABLE oFormStartTypeValueList AS Infragistics.Win.ValueList NO-UNDO.

	CONSTRUCTOR PUBLIC RunProcedureCallParameterControl ():
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
        THIS-OBJECT:ProcedureNameTextEditor = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:CharacterParameterValueTextEditor = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel3 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:AllowMultipleCheckEditor = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:InitializeInternalProcedureTextBox = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ReactivateInternalProcedureTextBox = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel4 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:RunPersistentCheckBox = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:ReactivateInternalProcedureParameterTextBox = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel5 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:InitializeInternalProcedureParameterTextBox = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel6 = NEW Infragistics.Win.Misc.UltraLabel().
        CAST(THIS-OBJECT:UseCharacterParameterValueCheckEditor, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ProcedureNameTextEditor, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:CharacterParameterValueTextEditor, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:AllowMultipleCheckEditor, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:InitializeInternalProcedureTextBox, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ReactivateInternalProcedureTextBox, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:RunPersistentCheckBox, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ReactivateInternalProcedureParameterTextBox, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:InitializeInternalProcedureParameterTextBox, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* UseCharacterParameterValueCheckEditor */
        /*  */
        THIS-OBJECT:UseCharacterParameterValueCheckEditor:Location = NEW System.Drawing.Point(213, 152).
        THIS-OBJECT:UseCharacterParameterValueCheckEditor:Name = "UseCharacterParameterValueCheckEditor":U.
        THIS-OBJECT:UseCharacterParameterValueCheckEditor:Size = NEW System.Drawing.Size(436, 20).
        THIS-OBJECT:UseCharacterParameterValueCheckEditor:TabIndex = 13.
        THIS-OBJECT:UseCharacterParameterValueCheckEditor:Text = "Use Character Parameter Value":U.
        /*  */
        /* ProcedureNameTextEditor */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ProcedureNameTextEditor:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ProcedureNameTextEditor:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ProcedureNameTextEditor:Location = NEW System.Drawing.Point(213, 3).
        THIS-OBJECT:ProcedureNameTextEditor:Name = "ProcedureNameTextEditor":U.
        THIS-OBJECT:ProcedureNameTextEditor:Size = NEW System.Drawing.Size(436, 21).
        THIS-OBJECT:ProcedureNameTextEditor:TabIndex = 1.
        /*  */
        /* CharacterParameterValueTextEditor */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:CharacterParameterValueTextEditor:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:CharacterParameterValueTextEditor:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:CharacterParameterValueTextEditor:Location = NEW System.Drawing.Point(213, 31).
        THIS-OBJECT:CharacterParameterValueTextEditor:Name = "CharacterParameterValueTextEditor":U.
        THIS-OBJECT:CharacterParameterValueTextEditor:Size = NEW System.Drawing.Size(436, 21).
        THIS-OBJECT:CharacterParameterValueTextEditor:TabIndex = 3.
        /*  */
        /* ultraLabel3 */
        /*  */
        THIS-OBJECT:ultraLabel3:Location = NEW System.Drawing.Point(13, 7).
        THIS-OBJECT:ultraLabel3:Name = "ultraLabel3":U.
        THIS-OBJECT:ultraLabel3:Size = NEW System.Drawing.Size(204, 23).
        THIS-OBJECT:ultraLabel3:TabIndex = 0.
        THIS-OBJECT:ultraLabel3:Text = "Procedure Name:":U.
        /*  */
        /* ultraLabel2 */
        /*  */
        THIS-OBJECT:ultraLabel2:Location = NEW System.Drawing.Point(13, 35).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        THIS-OBJECT:ultraLabel2:Size = NEW System.Drawing.Size(204, 23).
        THIS-OBJECT:ultraLabel2:TabIndex = 2.
        THIS-OBJECT:ultraLabel2:Text = "Character Parameter Value:":U.
        /*  */
        /* AllowMultipleCheckEditor */
        /*  */
        THIS-OBJECT:AllowMultipleCheckEditor:Location = NEW System.Drawing.Point(213, 126).
        THIS-OBJECT:AllowMultipleCheckEditor:Name = "AllowMultipleCheckEditor":U.
        THIS-OBJECT:AllowMultipleCheckEditor:Size = NEW System.Drawing.Size(436, 20).
        THIS-OBJECT:AllowMultipleCheckEditor:TabIndex = 12.
        THIS-OBJECT:AllowMultipleCheckEditor:Text = "Allow Multiple":U.
        /*  */
        /* InitializeInternalProcedureTextBox */
        /*  */
        THIS-OBJECT:InitializeInternalProcedureTextBox:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:InitializeInternalProcedureTextBox:Location = NEW System.Drawing.Point(213, 60).
        THIS-OBJECT:InitializeInternalProcedureTextBox:Name = "InitializeInternalProcedureTextBox":U.
        THIS-OBJECT:InitializeInternalProcedureTextBox:Size = NEW System.Drawing.Size(176, 21).
        THIS-OBJECT:InitializeInternalProcedureTextBox:TabIndex = 5.
        /*  */
        /* ultraLabel1 */
        /*  */
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(13, 64).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(204, 23).
        THIS-OBJECT:ultraLabel1:TabIndex = 4.
        THIS-OBJECT:ultraLabel1:Text = "Initialize Internal Procedure:":U.
        /*  */
        /* ReactivateInternalProcedureTextBox */
        /*  */
        THIS-OBJECT:ReactivateInternalProcedureTextBox:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ReactivateInternalProcedureTextBox:Location = NEW System.Drawing.Point(213, 93).
        THIS-OBJECT:ReactivateInternalProcedureTextBox:Name = "ReactivateInternalProcedureTextBox":U.
        THIS-OBJECT:ReactivateInternalProcedureTextBox:Size = NEW System.Drawing.Size(176, 21).
        THIS-OBJECT:ReactivateInternalProcedureTextBox:TabIndex = 9.
        /*  */
        /* ultraLabel4 */
        /*  */
        THIS-OBJECT:ultraLabel4:Location = NEW System.Drawing.Point(13, 97).
        THIS-OBJECT:ultraLabel4:Name = "ultraLabel4":U.
        THIS-OBJECT:ultraLabel4:Size = NEW System.Drawing.Size(204, 23).
        THIS-OBJECT:ultraLabel4:TabIndex = 8.
        THIS-OBJECT:ultraLabel4:Text = "Reactivate Internal Procedure:":U.
        /*  */
        /* RunPersistentCheckBox */
        /*  */
        THIS-OBJECT:RunPersistentCheckBox:Location = NEW System.Drawing.Point(213, 178).
        THIS-OBJECT:RunPersistentCheckBox:Name = "RunPersistentCheckBox":U.
        THIS-OBJECT:RunPersistentCheckBox:Size = NEW System.Drawing.Size(436, 20).
        THIS-OBJECT:RunPersistentCheckBox:TabIndex = 14.
        THIS-OBJECT:RunPersistentCheckBox:Text = "Run Persistent":U.
        /*  */
        /* ReactivateInternalProcedureParameterTextBox */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ReactivateInternalProcedureParameterTextBox:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ReactivateInternalProcedureParameterTextBox:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ReactivateInternalProcedureParameterTextBox:Location = NEW System.Drawing.Point(475, 93).
        THIS-OBJECT:ReactivateInternalProcedureParameterTextBox:Name = "ReactivateInternalProcedureParameterTextBox":U.
        THIS-OBJECT:ReactivateInternalProcedureParameterTextBox:Size = NEW System.Drawing.Size(174, 21).
        THIS-OBJECT:ReactivateInternalProcedureParameterTextBox:TabIndex = 11.
        /*  */
        /* ultraLabel5 */
        /*  */
        THIS-OBJECT:ultraLabel5:Location = NEW System.Drawing.Point(395, 95).
        THIS-OBJECT:ultraLabel5:Name = "ultraLabel5":U.
        THIS-OBJECT:ultraLabel5:Size = NEW System.Drawing.Size(89, 23).
        THIS-OBJECT:ultraLabel5:TabIndex = 10.
        THIS-OBJECT:ultraLabel5:Text = "Parameter:":U.
        /*  */
        /* InitializeInternalProcedureParameterTextBox */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:InitializeInternalProcedureParameterTextBox:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:InitializeInternalProcedureParameterTextBox:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:InitializeInternalProcedureParameterTextBox:Location = NEW System.Drawing.Point(475, 60).
        THIS-OBJECT:InitializeInternalProcedureParameterTextBox:Name = "InitializeInternalProcedureParameterTextBox":U.
        THIS-OBJECT:InitializeInternalProcedureParameterTextBox:Size = NEW System.Drawing.Size(174, 21).
        THIS-OBJECT:InitializeInternalProcedureParameterTextBox:TabIndex = 7.
        /*  */
        /* ultraLabel6 */
        /*  */
        THIS-OBJECT:ultraLabel6:Location = NEW System.Drawing.Point(395, 64).
        THIS-OBJECT:ultraLabel6:Name = "ultraLabel6":U.
        THIS-OBJECT:ultraLabel6:Size = NEW System.Drawing.Size(83, 23).
        THIS-OBJECT:ultraLabel6:TabIndex = 6.
        THIS-OBJECT:ultraLabel6:Text = "Parameter:":U.
        /*  */
        /* RunProcedureCallParameterControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:BackColor = System.Drawing.SystemColors:Control.
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ReactivateInternalProcedureParameterTextBox).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel5).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:InitializeInternalProcedureParameterTextBox).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel6).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:RunPersistentCheckBox).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ReactivateInternalProcedureTextBox).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel4).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:InitializeInternalProcedureTextBox).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:AllowMultipleCheckEditor).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:UseCharacterParameterValueCheckEditor).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ProcedureNameTextEditor).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:CharacterParameterValueTextEditor).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel3).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:Name = "RunProcedureCallParameterControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(663, 210).
        CAST(THIS-OBJECT:UseCharacterParameterValueCheckEditor, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ProcedureNameTextEditor, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:CharacterParameterValueTextEditor, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:AllowMultipleCheckEditor, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:InitializeInternalProcedureTextBox, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ReactivateInternalProcedureTextBox, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:RunPersistentCheckBox, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ReactivateInternalProcedureParameterTextBox, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:InitializeInternalProcedureParameterTextBox, System.ComponentModel.ISupportInitialize):EndInit().
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
            ProcedureNameTextEditor:Text                     = "":U
            CharacterParameterValueTextEditor:Text           = "":U
            InitializeInternalProcedureTextBox:TEXT          = "":U
            InitializeInternalProcedureParameterTextBox:TEXT = "":U
            ReactivateInternalProcedureTextBox:TEXT          = "":U 
            ReactivateInternalProcedureParameterTextBox:TEXT = "":U 
            AllowMultipleCheckEditor:Checked                 = FALSE 
            UseCharacterParameterValueCheckEditor:Checked    = FALSE 
            RunPersistentCheckBox:Checked                    = FALSE 
            .

    END METHOD .


    /*------------------------------------------------------------------------------
        Purpose: Updates the data shown in the UI
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID UpdateUiData ():
        
        DEFINE VARIABLE oCallParameter AS RunProcedureCallParameter NO-UNDO.
    
        IF NOT TYPE-OF (CallParameterObject, RunProcedureCallParameter) THEN 
            RETURN.
            
        oCallParameter = CAST (CallParameterObject, RunProcedureCallParameter).
        
        ASSIGN
            ProcedureNameTextEditor:Text                     = oCallParameter:ProcedureName
            CharacterParameterValueTextEditor:Text           = oCallParameter:CharacterParameterValue
            InitializeInternalProcedureTextBox:Text          = oCallParameter:InitializeInternalProcedure
            InitializeInternalProcedureParameterTextBox:Text = oCallParameter:InitializeInternalProcedureParameter
            ReactivateInternalProcedureTextBox:Text          = oCallParameter:ReactivateInternalProcedure
            ReactivateInternalProcedureParameterTextBox:Text = oCallParameter:ReactivateInternalProcedureParameter
            AllowMultipleCheckEditor:Checked                 = oCallParameter:AllowMultiple
            UseCharacterParameterValueCheckEditor:Checked    = oCallParameter:UseCharacterParameterValue
            RunPersistentCheckBox:Checked                    = oCallParameter:RunPersistent
            .
    
    END METHOD .


    /*------------------------------------------------------------------------------
        Purpose: Updates the data of the CallParameter object after the user has
                 entered data to the UI
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID WritebackCallParameterToTextProperty ():
        
        DEFINE VARIABLE oCallParameter      AS RunProcedureCallParameter NO-UNDO .
        DEFINE VARIABLE cFormStartTypeValue AS CHARACTER                 NO-UNDO .
        
        /* Marko Rüterbories, Consultingwerk Ltd. 20.09.2013
           If the CallParameterObject is not valid or the datatype does not match the needs
           the instance gets deleted and a new instance of the correct type is created. (SCL-119)
           
           Invalid cast from Consultingwerk.Windows.Framework.RunProcedureCallParameter to 
           Consultingwerk.SmartFramework.InvokeServiceMethodCallParameter. (12869) */
        IF NOT VALID-OBJECT (CallParameterObject) OR 
           NOT TYPE-OF (CallParameterObject, RunProcedureCallParameter) THEN DO:  
            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (CallParameterObject).
            CallParameterObject = NEW RunProcedureCallParameter ().
        END.

        oCallParameter = CAST (CallParameterObject, RunProcedureCallParameter).

        ASSIGN
            oCallParameter:ProcedureName                        = ProcedureNameTextEditor:Text                   
            oCallParameter:CharacterParameterValue              = CharacterParameterValueTextEditor:Text        
            oCallParameter:InitializeInternalProcedure          = InitializeInternalProcedureTextBox:Text 
            oCallParameter:InitializeInternalProcedureParameter = InitializeInternalProcedureParameterTextBox:Text 
            oCallParameter:ReactivateInternalProcedure          = ReactivateInternalProcedureTextBox:Text      
            oCallParameter:ReactivateInternalProcedureParameter = ReactivateInternalProcedureParameterTextBox:Text      
            oCallParameter:AllowMultiple                        = AllowMultipleCheckEditor:Checked
            oCallParameter:UseCharacterParameterValue           = UseCharacterParameterValueCheckEditor:Checked
            oCallParameter:RunPersistent                        = RunPersistentCheckBox:Checked    
            .
        
        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.
    END METHOD .

END CLASS.
