/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : CsharpGeneratorWizard
    Purpose     : Wrapper wizard to the xsd.exe utility
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Dec 21 14:50:56 CET 2014
    Notes       : http://msdn.microsoft.com/en-us/library/x6c1kb0s(v=vs.100).aspx
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.Plugins.CsharpGenerator.* FROM PROPATH .  
USING Consultingwerk.BusinessEntityDesigner.Services.*                FROM PROPATH .
USING Consultingwerk.Framework.*                                      FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*                           FROM PROPATH .
USING Consultingwerk.Windows.BaseForms.*                              FROM PROPATH .
USING Consultingwerk.Util.*                                           FROM PROPATH.
USING Infragistics.Win.UltraWinEditors.*                              FROM ASSEMBLY .
USING Progress.Lang.*                                                 FROM PROPATH .
USING System.Windows.Forms.*                                          FROM ASSEMBLY.

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.CsharpGenerator.CsharpGeneratorWizard INHERITS BaseDialogForm: 

	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE folderBrowserDialog1 AS System.Windows.Forms.FolderBrowserDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE linkLabel1 AS System.Windows.Forms.LinkLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE openFileDialog1 AS System.Windows.Forms.OpenFileDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton3 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE tbXsdPath AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbOutputFolder AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbNamespace AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbAdditionalParameters AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE osGenerate AS Infragistics.Win.UltraWinEditors.UltraOptionSet NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton4 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton2 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton1 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel5 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel4 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    
    DEFINE VARIABLE cGenerate AS CHARACTER NO-UNDO.
    
    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY="REFERENCE-ONLY" }
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the DatasetController of the 
                 Business Entity Designer
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY BusinessEntityDatasetController AS BusinessEntityDatasetController NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the CsharpGeneratorWizard class 
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC CsharpGeneratorWizard ():
		
        SUPER().
        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the ButtonOk 
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ButtonOk_Click (sender AS System.Object, 
	                                    e AS System.EventArgs):
		
    	THIS-OBJECT:SetSetting ("CsharpGenerator.OutputFolder":U,        tbOutputFolder:Text) .  	
        THIS-OBJECT:SetSetting ("CsharpGenerator.Generate":U,            UNBOX (osGenerate:Value)) .    
        THIS-OBJECT:SetSetting ("CsharpGenerator.Namespace":U,           tbNamespace:Text) .    
        THIS-OBJECT:SetSetting ("CsharpGenerator.AdditionalParameter":U, tbAdditionalParameters:Text) .    

        /* Option Set Value not available when Dialog is closed */
        ASSIGN cGenerate = UNBOX (osGenerate:Value) . 
    
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the additional parameters  
        Notes:   
        @return The additional parameters  
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CHARACTER GetAdditionalParameter ():
		
		RETURN tbAdditionalParameters:Text .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Binds to the Business Entity Designer Dataset
        Notes:
        @param dsBusinessEntity The dataset to bind to 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetDatasetHandle (DATASET dsBusinessEntity BIND):

        /* Noop */
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns if dataset or classes should be generated 
        Notes:   
        @return dataset/classes  
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CHARACTER GetGenerate ():
		
		RETURN cGenerate .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the namespace 
        Notes:   
        @return The namespace 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CHARACTER GetNamespace ():
		
		RETURN tbNamespace:Text .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the output folder
        Notes:   
        @return The output folder  
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CHARACTER GetOutputFolder ():
		
		RETURN tbOutputFolder:Text .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets a setting for this wizard from either the Business Entity 
                 Model or the Windows Registry 
        Notes:   
        @param pcSetting The key for the setting
        @return The value for the setting
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED CHARACTER GetSetting (pcSetting AS CHARACTER):

        DEFINE VARIABLE oSettingService AS ISettingsService NO-UNDO .
		
		FIND eBusinessEntityProperties WHERE eBusinessEntityProperties.PropertyName = pcSetting
		    NO-ERROR . 
		    
		IF AVAILABLE eBusinessEntityProperties THEN
		    RETURN eBusinessEntityProperties.PropertyValue . 
		
		oSettingService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .
		
		pcSetting = oSettingService:GetSetting ("CsharpGenerator":U, pcSetting) . 

        IF pcSetting > "":U THEN 
            RETURN pcSetting . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the path to xsd.exe 
        Notes:   
        @return The path to xsd.exe
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CHARACTER GetXsdExe ():
		
		RETURN tbXsdPath:Text .

	END METHOD .

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
        DEFINE VARIABLE editorButton2 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton2 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem1 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem1 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem2 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem2 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.BusinessEntityDesigner.Plugins.CsharpGenerator.CsharpGeneratorWizard":U).
        THIS-OBJECT:linkLabel1 = NEW System.Windows.Forms.LinkLabel().
        THIS-OBJECT:tbXsdPath = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:osGenerate = NEW Infragistics.Win.UltraWinEditors.UltraOptionSet().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel3 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel4 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbOutputFolder = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbNamespace = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel5 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbAdditionalParameters = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraButton1 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraButton2 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraButton4 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraButton3 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:folderBrowserDialog1 = NEW System.Windows.Forms.FolderBrowserDialog().
        THIS-OBJECT:openFileDialog1 = NEW System.Windows.Forms.OpenFileDialog().
        THIS-OBJECT:ContentPanel:SuspendLayout().
        CAST(THIS-OBJECT:tbXsdPath, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:osGenerate, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbOutputFolder, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbNamespace, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbAdditionalParameters, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ButtonOk */
        /*  */
        THIS-OBJECT:ButtonOk:Enabled = FALSE.
        THIS-OBJECT:ButtonOk:Location = NEW System.Drawing.Point(403, 166).
        THIS-OBJECT:ButtonOk:TabIndex = 0.
        THIS-OBJECT:ButtonOk:Click:Subscribe(THIS-OBJECT:ButtonOk_Click).
        /*  */
        /* ButtonCancel */
        /*  */
        THIS-OBJECT:ButtonCancel:Location = NEW System.Drawing.Point(484, 166).
        THIS-OBJECT:ButtonCancel:TabIndex = 1.
        /*  */
        /* ContentPanel */
        /*  */
        THIS-OBJECT:ContentPanel:Controls:Add(THIS-OBJECT:ultraButton3).
        THIS-OBJECT:ContentPanel:Controls:Add(THIS-OBJECT:ultraButton4).
        THIS-OBJECT:ContentPanel:Controls:Add(THIS-OBJECT:ultraButton2).
        THIS-OBJECT:ContentPanel:Controls:Add(THIS-OBJECT:ultraButton1).
        THIS-OBJECT:ContentPanel:Controls:Add(THIS-OBJECT:tbAdditionalParameters).
        THIS-OBJECT:ContentPanel:Controls:Add(THIS-OBJECT:ultraLabel5).
        THIS-OBJECT:ContentPanel:Controls:Add(THIS-OBJECT:tbNamespace).
        THIS-OBJECT:ContentPanel:Controls:Add(THIS-OBJECT:tbOutputFolder).
        THIS-OBJECT:ContentPanel:Controls:Add(THIS-OBJECT:ultraLabel4).
        THIS-OBJECT:ContentPanel:Controls:Add(THIS-OBJECT:ultraLabel3).
        THIS-OBJECT:ContentPanel:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:ContentPanel:Controls:Add(THIS-OBJECT:osGenerate).
        THIS-OBJECT:ContentPanel:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:ContentPanel:Controls:Add(THIS-OBJECT:tbXsdPath).
        THIS-OBJECT:ContentPanel:Controls:Add(THIS-OBJECT:linkLabel1).
        THIS-OBJECT:ContentPanel:Size = NEW System.Drawing.Size(571, 160).
        /*  */
        /* linkLabel1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:linkLabel1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:linkLabel1:AutoSize = TRUE.
        THIS-OBJECT:linkLabel1:Location = NEW System.Drawing.Point(150, 138).
        THIS-OBJECT:linkLabel1:Name = "linkLabel1":U.
        THIS-OBJECT:linkLabel1:Size = NEW System.Drawing.Size(93, 17).
        THIS-OBJECT:linkLabel1:TabIndex = 15.
        THIS-OBJECT:linkLabel1:TabStop = TRUE.
        THIS-OBJECT:linkLabel1:Text = "MSDN Reference":U.
        THIS-OBJECT:linkLabel1:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:linkLabel1:LinkClicked:Subscribe(THIS-OBJECT:linkLabel1_LinkClicked).
        /*  */
        /* tbXsdPath */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbXsdPath:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton2:Text = "...":U.
        THIS-OBJECT:tbXsdPath:ButtonsRight:Add(editorButton2).
        THIS-OBJECT:tbXsdPath:Location = NEW System.Drawing.Point(150, 8).
        THIS-OBJECT:tbXsdPath:Name = "tbXsdPath":U.
        THIS-OBJECT:tbXsdPath:Size = NEW System.Drawing.Size(313, 21).
        THIS-OBJECT:tbXsdPath:TabIndex = 1.
        THIS-OBJECT:tbXsdPath:ValueChanged:Subscribe(THIS-OBJECT:tbXsdPath_ValueChanged).
        THIS-OBJECT:tbXsdPath:EditorButtonClick:Subscribe(THIS-OBJECT:tbXsdPath_EditorButtonClick).
        THIS-OBJECT:tbXsdPath:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        /*  */
        /* ultraLabel1 */
        /*  */
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(12, 12).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel1:TabIndex = 0.
        THIS-OBJECT:ultraLabel1:Text = "Path to XSD.EXE:":U.
        /*  */
        /* osGenerate */
        /*  */
        THIS-OBJECT:osGenerate:BorderStyle = Infragistics.Win.UIElementBorderStyle:None.
        valueListItem1:CheckState = System.Windows.Forms.CheckState:Checked.
        valueListItem1:DataValue = "dataset":U.
        valueListItem1:DisplayText = "Dataset":U.
        valueListItem2:DataValue = "classes":U.
        valueListItem2:DisplayText = "Classes":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.ValueListItem EXTENT 2 NO-UNDO.
        arrayvar0[1] = valueListItem1.
        arrayvar0[2] = valueListItem2.
        THIS-OBJECT:osGenerate:Items:AddRange(arrayvar0).
        THIS-OBJECT:osGenerate:Location = NEW System.Drawing.Point(149, 90).
        THIS-OBJECT:osGenerate:Name = "osGenerate":U.
        THIS-OBJECT:osGenerate:Size = NEW System.Drawing.Size(189, 17).
        THIS-OBJECT:osGenerate:TabIndex = 10.
        THIS-OBJECT:osGenerate:ValueChanged:Subscribe(THIS-OBJECT:tbXsdPath_ValueChanged).
        /*  */
        /* ultraLabel2 */
        /*  */
        THIS-OBJECT:ultraLabel2:Location = NEW System.Drawing.Point(12, 90).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        THIS-OBJECT:ultraLabel2:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel2:TabIndex = 9.
        THIS-OBJECT:ultraLabel2:Text = "Generate:":U.
        /*  */
        /* ultraLabel3 */
        /*  */
        THIS-OBJECT:ultraLabel3:Location = NEW System.Drawing.Point(12, 66).
        THIS-OBJECT:ultraLabel3:Name = "ultraLabel3":U.
        THIS-OBJECT:ultraLabel3:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel3:TabIndex = 6.
        THIS-OBJECT:ultraLabel3:Text = "Namespace:":U.
        /*  */
        /* ultraLabel4 */
        /*  */
        THIS-OBJECT:ultraLabel4:Location = NEW System.Drawing.Point(12, 41).
        THIS-OBJECT:ultraLabel4:Name = "ultraLabel4":U.
        THIS-OBJECT:ultraLabel4:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel4:TabIndex = 3.
        THIS-OBJECT:ultraLabel4:Text = "Output Folder:":U.
        /*  */
        /* tbOutputFolder */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbOutputFolder:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton1:Text = "...":U.
        THIS-OBJECT:tbOutputFolder:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:tbOutputFolder:Location = NEW System.Drawing.Point(149, 35).
        THIS-OBJECT:tbOutputFolder:Name = "tbOutputFolder":U.
        THIS-OBJECT:tbOutputFolder:Size = NEW System.Drawing.Size(314, 21).
        THIS-OBJECT:tbOutputFolder:TabIndex = 4.
        THIS-OBJECT:tbOutputFolder:ValueChanged:Subscribe(THIS-OBJECT:tbXsdPath_ValueChanged).
        THIS-OBJECT:tbOutputFolder:EditorButtonClick:Subscribe(THIS-OBJECT:tbOutputFolder_EditorButtonClick).
        THIS-OBJECT:tbOutputFolder:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        /*  */
        /* tbNamespace */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbNamespace:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbNamespace:Location = NEW System.Drawing.Point(149, 62).
        THIS-OBJECT:tbNamespace:Name = "tbNamespace":U.
        THIS-OBJECT:tbNamespace:Size = NEW System.Drawing.Size(314, 21).
        THIS-OBJECT:tbNamespace:TabIndex = 7.
        THIS-OBJECT:tbNamespace:ValueChanged:Subscribe(THIS-OBJECT:tbXsdPath_ValueChanged).
        /*  */
        /* ultraLabel5 */
        /*  */
        THIS-OBJECT:ultraLabel5:Location = NEW System.Drawing.Point(12, 118).
        THIS-OBJECT:ultraLabel5:Name = "ultraLabel5":U.
        THIS-OBJECT:ultraLabel5:Size = NEW System.Drawing.Size(131, 23).
        THIS-OBJECT:ultraLabel5:TabIndex = 12.
        THIS-OBJECT:ultraLabel5:Text = "Additional parameters:":U.
        /*  */
        /* tbAdditionalParameters */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbAdditionalParameters:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbAdditionalParameters:Location = NEW System.Drawing.Point(149, 114).
        THIS-OBJECT:tbAdditionalParameters:Multiline = TRUE.
        THIS-OBJECT:tbAdditionalParameters:Name = "tbAdditionalParameters":U.
        THIS-OBJECT:tbAdditionalParameters:Scrollbars = System.Windows.Forms.ScrollBars:Vertical.
        THIS-OBJECT:tbAdditionalParameters:Size = NEW System.Drawing.Size(314, 21).
        THIS-OBJECT:tbAdditionalParameters:TabIndex = 13.
        /*  */
        /* ultraButton1 */
        /*  */
        DEFINE VARIABLE nestedvar5 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar5 = System.Windows.Forms.AnchorStyles:Top.
        THIS-OBJECT:ultraButton1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar5, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButton1:Location = NEW System.Drawing.Point(469, 8).
        THIS-OBJECT:ultraButton1:Name = "ultraButton1":U.
        THIS-OBJECT:ultraButton1:Size = NEW System.Drawing.Size(90, 23).
        THIS-OBJECT:ultraButton1:TabIndex = 2.
        THIS-OBJECT:ultraButton1:Text = "Make Default":U.
        THIS-OBJECT:ultraButton1:Click:Subscribe(THIS-OBJECT:ultraButton1_Click).
        /*  */
        /* ultraButton2 */
        /*  */
        DEFINE VARIABLE nestedvar6 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar6 = System.Windows.Forms.AnchorStyles:Top.
        THIS-OBJECT:ultraButton2:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar6, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButton2:Location = NEW System.Drawing.Point(469, 35).
        THIS-OBJECT:ultraButton2:Name = "ultraButton2":U.
        THIS-OBJECT:ultraButton2:Size = NEW System.Drawing.Size(90, 23).
        THIS-OBJECT:ultraButton2:TabIndex = 5.
        THIS-OBJECT:ultraButton2:Text = "Make Default":U.
        THIS-OBJECT:ultraButton2:Click:Subscribe(THIS-OBJECT:ultraButton2_Click).
        /*  */
        /* ultraButton4 */
        /*  */
        DEFINE VARIABLE nestedvar7 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar7 = System.Windows.Forms.AnchorStyles:Top.
        THIS-OBJECT:ultraButton4:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar7, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButton4:Location = NEW System.Drawing.Point(469, 113).
        THIS-OBJECT:ultraButton4:Name = "ultraButton4":U.
        THIS-OBJECT:ultraButton4:Size = NEW System.Drawing.Size(90, 23).
        THIS-OBJECT:ultraButton4:TabIndex = 14.
        THIS-OBJECT:ultraButton4:Text = "Make Default":U.
        THIS-OBJECT:ultraButton4:Click:Subscribe(THIS-OBJECT:ultraButton4_Click).
        /*  */
        /* ultraButton3 */
        /*  */
        DEFINE VARIABLE nestedvar8 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar8 = System.Windows.Forms.AnchorStyles:Top.
        THIS-OBJECT:ultraButton3:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar8, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButton3:Location = NEW System.Drawing.Point(469, 87).
        THIS-OBJECT:ultraButton3:Name = "ultraButton3":U.
        THIS-OBJECT:ultraButton3:Size = NEW System.Drawing.Size(90, 23).
        THIS-OBJECT:ultraButton3:TabIndex = 11.
        THIS-OBJECT:ultraButton3:Text = "Make Default":U.
        THIS-OBJECT:ultraButton3:Click:Subscribe(THIS-OBJECT:ultraButton3_Click).
        /*  */
        /* folderBrowserDialog1 */
        /*  */
        THIS-OBJECT:folderBrowserDialog1:Description = "Select output folder":U.
        /*  */
        /* openFileDialog1 */
        /*  */
        THIS-OBJECT:openFileDialog1:DefaultExt = "exe":U.
        THIS-OBJECT:openFileDialog1:Filter = "xsd.exe|xsd.exe":U.
        THIS-OBJECT:openFileDialog1:RestoreDirectory = TRUE.
        /*  */
        /* CsharpGeneratorWizard */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(571, 201).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:MinimumSize = NEW System.Drawing.Size(587, 240).
        THIS-OBJECT:Name = "CsharpGeneratorWizard":U.
        THIS-OBJECT:SizeGripStyle = System.Windows.Forms.SizeGripStyle:Show.
        THIS-OBJECT:Text = "C# Code Generator Wizard":U.
        THIS-OBJECT:ContentPanel:ResumeLayout(FALSE).
        THIS-OBJECT:ContentPanel:PerformLayout().
        CAST(THIS-OBJECT:tbXsdPath, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:osGenerate, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbOutputFolder, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbNamespace, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbAdditionalParameters, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
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
        Purpose: Event handler for the LinkClicked event of the linkLabel1 Control 
        Notes:   Opens the MSDN Reference entry for the xsd.exe utility
                 http://msdn.microsoft.com/en-us/library/x6c1kb0s(v=vs.100).aspx
        @param sender The reference to the object that raised the event
        @param e The LinkLabelLinkClickedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID linkLabel1_LinkClicked (sender AS System.Object, 
	                                            e AS System.Windows.Forms.LinkLabelLinkClickedEventArgs):
		
        Consultingwerk.Windows.API.Win32:ShellExecute ("open":U, 
                                                       "http://msdn.microsoft.com/en-us/library/x6c1kb0s(v=vs.100).aspx":U) .		

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event 
        Notes:   
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):
		
		DEFINE VARIABLE cSetting AS CHARACTER NO-UNDO.
		
		SUPER:OnLoad (e) .

        cSetting = THIS-OBJECT:GetSetting ("CsharpGenerator.XsdPath":U) .
        
        IF cSetting > "":U THEN tbXsdPath:Text = cSetting . 
         
        cSetting = THIS-OBJECT:GetSetting ("CsharpGenerator.OutputFolder":U) .
        
        IF cSetting > "":U THEN tbOutputFolder:Text = cSetting . 
        
        cSetting = THIS-OBJECT:GetSetting ("CsharpGenerator.Generate":U) .
        
        IF cSetting > "":U THEN osGenerate:Value = BOX (cSetting) . 

        cSetting = THIS-OBJECT:GetSetting ("CsharpGenerator.Namespace":U) .
        
        IF cSetting > "":U THEN tbNamespace:Text = cSetting . 
        ELSE DO:
            cSetting = THIS-OBJECT:BusinessEntityDatasetController:BusinessEntityViewerControl:SmartDataSource:GetFieldValues ("BusinessEntityPackage":U) .
            
            IF cSetting > "":U THEN tbNamespace:Text = cSetting .
        END.
        
        cSetting = THIS-OBJECT:GetSetting ("CsharpGenerator.AdditionalParameter":U) .
        
        IF cSetting > "":U THEN tbAdditionalParameters:Value = BOX (cSetting) . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Sets a setting default in the Windows Registry
        Notes:   
        @param pcKey The settings key to store
        @param pcValue The settings value to store
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID SetDefaultSetting (pcKey AS CHARACTER, 
	                                         pcValue AS CHARACTER):
		
        DEFINE VARIABLE oSettingService AS ISettingsService NO-UNDO .
        
        oSettingService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .
        
        oSettingService:StoreSetting ("CsharpGenerator":U, pcKey, pcValue) . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Sets a setting  in the Business Entity Design
        Notes:   
        @param pcKey The settings key to store
        @param pcValue The settings value to store
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID SetSetting (pcKey AS CHARACTER, 
                                   pcValue AS CHARACTER):
		
		FIND eBusinessEntityProperties WHERE eBusinessEntityProperties.PropertyName = pcKey NO-ERROR . 
		
		IF AVAILABLE eBusinessEntityProperties THEN DO:
		    IF eBusinessEntityProperties.PropertyValue = pcValue THEN 
		      RETURN . 
		END . 
		ELSE DO:
		    CREATE eBusinessEntityProperties.
		    ASSIGN eBusinessEntityProperties.PropertyName = pcKey .  
	    END.  
		
		ASSIGN eBusinessEntityProperties.PropertyValue                    = pcValue 
		       THIS-OBJECT:BusinessEntityDatasetController:UnsavedChanges = TRUE . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the tbOutputFolder
        Notes:   
        @param sender The reference to the obejct that raised this event
        @param e The EditorButtonEventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID tbOutputFolder_EditorButtonClick (sender AS System.Object, 
	                                                      e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):
		
		DEFINE VARIABLE oDialogResult AS DialogResult NO-UNDO .
		
		IF tbOutputFolder:Text > "":U THEN 
		    folderBrowserDialog1:SelectedPath = tbOutputFolder:Text . 

        WAIT-FOR folderBrowserDialog1:ShowDialog () SET oDialogResult .
         
        {Consultingwerk/check-dialogresult-ok.i oDialogResult} .
        
        tbOutputFolder:Text = folderBrowserDialog1:SelectedPath . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the tbXsdPath
        Notes:   
        @param sender The reference to the obejct that raised this event
        @param e The EditorButtonEventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID tbXsdPath_EditorButtonClick (sender AS System.Object, 
	                                                 e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):
		
        DEFINE VARIABLE oDialogResult AS DialogResult NO-UNDO .
        
        IF tbXsdPath:Text > "":U THEN 
            openFileDialog1:FileName = tbXsdPath:Text . 

        WAIT-FOR openFileDialog1:ShowDialog () SET oDialogResult . 

        {Consultingwerk/check-dialogresult-ok.i oDialogResult} .
        
        tbXsdPath:Text = openFileDialog1:FileName . 
		

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ValueChanged event of the controls 
        Notes:   Validates input and enables/disables ButtonOk
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID tbXsdPath_ValueChanged (sender AS System.Object, e AS System.EventArgs):
		
		DEFINE VARIABLE lXsdExeValid AS LOGICAL NO-UNDO .
		DEFINE VARIABLE lPathValid   AS LOGICAL NO-UNDO .
		
		ASSIGN lXsdExeValid = tbXsdPath:Text > "":U AND FileHelper:Exists (tbXsdPath:Text) 
		       lPathValid   = tbOutputFolder:Text > "":U AND FileHelper:Exists (tbOutputFolder:Text) . 
		
		ASSIGN ButtonOk:Enabled = lXsdExeValid AND lPathValid AND 
		                          UNBOX (osGenerate:VALUE) <> ? AND 
		                          tbNamespace:Text > "":U .  
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the ultraButton1   
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraButton1_Click (sender AS System.Object, 
	                                        e AS System.EventArgs):
		
		THIS-OBJECT:SetDefaultSetting ("CsharpGenerator.XsdPath":U,
		                               tbXsdPath:Text) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the ultraButton2 
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraButton2_Click (sender AS System.Object, 
	                                        e AS System.EventArgs):

        THIS-OBJECT:SetDefaultSetting ("CsharpGenerator.OutputFolder":U,
                                       tbOutputFolder:Text) .		
		

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the ultraButton3 
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraButton3_Click (sender AS System.Object, e AS System.EventArgs):
		
		IF UNBOX (osGenerate:Value) > "":U THEN 
		
        THIS-OBJECT:SetDefaultSetting ("CsharpGenerator.Generate":U,
                                       UNBOX (osGenerate:Value)) .        

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the ultraButton3 
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraButton4_Click (sender AS System.Object, 
	                                        e AS System.EventArgs):
		
		THIS-OBJECT:SetDefaultSetting ("CsharpGenerator.AdditionalParameter":U,
                                       tbAdditionalParameters:Text) .  

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the CsharpGeneratorWizard class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC CsharpGeneratorWizard ():

    END DESTRUCTOR .

END CLASS.
