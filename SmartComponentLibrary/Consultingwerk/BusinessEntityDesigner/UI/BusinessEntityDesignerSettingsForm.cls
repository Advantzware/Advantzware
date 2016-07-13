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
    File        : BusinessEntityDesignerSettingsForm
    Purpose     : Dialog to main settings of the Business Entity Designer
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Oct 21 23:01:24 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.*         FROM PROPATH .
USING Consultingwerk.Framework.Enum.*                 FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Infragistics.Win.UltraWinEditors.*              FROM ASSEMBLY .
USING Progress.Lang.*                                 FROM PROPATH .
USING System.Windows.Forms.*                          FROM ASSEMBLY .

{Consultingwerk/products.i}

CLASS Consultingwerk.BusinessEntityDesigner.UI.BusinessEntityDesignerSettingsForm 
    INHERITS SmartWindowForm: 
    
    DEFINE PRIVATE VARIABLE btn_use_current_commandline AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE cbReplaceUnderscore AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE cbReplaceHyphens AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE cbSplitArrayFields AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE coKeywordCasing AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE gb_launcher AS Infragistics.Win.Misc.UltraGroupBox NO-UNDO.
    DEFINE PRIVATE VARIABLE openFileDialog1 AS System.Windows.Forms.OpenFileDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_CustomServices AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE to_DistinctTempTableIncludes AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_launch_parameter AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE to_CompileDatasetController AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE folderBrowserDialog1 AS System.Windows.Forms.FolderBrowserDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE gb_designer2 AS Infragistics.Win.Misc.UltraGroupBox NO-UNDO.
    DEFINE PRIVATE VARIABLE gb_generator AS Infragistics.Win.Misc.UltraGroupBox NO-UNDO.
    DEFINE PRIVATE VARIABLE gb_designer AS Infragistics.Win.Misc.UltraGroupBox NO-UNDO.
    DEFINE PRIVATE VARIABLE gb_plugins AS Infragistics.Win.Misc.UltraGroupBox NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_beforesuffix AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_beforeprefix AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_datarelationnamesubstitute AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_fieldnamegenerator AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_IncludeFiles AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_namesuffix AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_nameprefix AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_plugins AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_RCodeFolder_Gui AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_RelationFieldsGenerator AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_SourceFolder_Gui AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_tablenamegenerator AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_templatefolder AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_generator AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE to_GenerateDatasetController AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE to_OpenFilesInIDE AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE to_PromptSave AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE toUseLocalFileHistory AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton2 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton1 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton3 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraExplorerBar1 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBar NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel10 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel11 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel12 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel13 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel14 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel15 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel16 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel19 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel18 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel17 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel24 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel23 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel22 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel21 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel20 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel25 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel26 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel27 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel34 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel33 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel9 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel8 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel7 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel6 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel5 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel4 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraPanel1 AS Infragistics.Win.Misc.UltraPanel NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_RCodeFolder AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tb_SourceFolder AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraStatusBar1 AS Infragistics.Win.UltraWinStatusBar.UltraStatusBar NO-UNDO.

    {Consultingwerk/Util/TempTables/ttClassNames.i &ACCESS="PRIVATE STATIC"}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BusinessEntityDesignerSettingsForm  class                                                            
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BusinessEntityDesignerSettingsForm ():
        
        SUPER().
        InitializeComponent().
        
        ASSIGN 
            THIS-OBJECT:tb_generator:Text                     = BusinessEntityDesignerSettings:BusinessEntityGenerator
            THIS-OBJECT:tb_fieldnamegenerator:Text            = BusinessEntityDesignerSettings:FieldNameGeneratorService
            THIS-OBJECT:tb_RelationFieldsGenerator:Text       = BusinessEntityDesignerSettings:DataRelationFieldsService
            THIS-OBJECT:tb_templatefolder:Text                = BusinessEntityDesignerSettings:TemplateFolder
            THIS-OBJECT:tb_datarelationnamesubstitute:Text    = BusinessEntityDesignerSettings:RelationNameSubstitute
            THIS-OBJECT:tb_nameprefix:Text                    = BusinessEntityDesignerSettings:DefaultTablePrefix
            THIS-OBJECT:tb_namesuffix:Text                    = BusinessEntityDesignerSettings:DefaultTableSuffix
            THIS-OBJECT:tb_beforeprefix:Text                  = BusinessEntityDesignerSettings:DefaultBeforeTablePrefix
            THIS-OBJECT:tb_beforesuffix:Text                  = BusinessEntityDesignerSettings:DefaultBeforeTableSuffix
            THIS-OBJECT:tb_plugins:Text                       = REPLACE (BusinessEntityDesignerSettings:BusinessEntityDesignerPlugins,
                                                                         "|":U, System.Environment:NewLine) 
            THIS-OBJECT:tb_SourceFolder:Text                  = BusinessEntityDesignerSettings:SourceCodeBaseFolder
            THIS-OBJECT:tb_RCodeFolder:Text                   = BusinessEntityDesignerSettings:RCodeBaseFolder
                                                             
            THIS-OBJECT:tb_SourceFolder_Gui:Text              = BusinessEntityDesignerSettings:SourceCodeBaseFolderGui
            THIS-OBJECT:tb_RCodeFolder_Gui:Text               = BusinessEntityDesignerSettings:RCodeBaseFolderGui
                                                             
            THIS-OBJECT:tb_IncludeFiles:Text                  = BusinessEntityDesignerSettings:IncludeFilesBaseFolder
                                                             
            THIS-OBJECT:tb_tablenamegenerator:Text            = BusinessEntityDesignerSettings:TableNameGeneratorService
                                                             
            THIS-OBJECT:to_OpenFilesInIDE:Checked             = BusinessEntityDesignerSettings:OpenFilesInIDE
            THIS-OBJECT:to_CompileDatasetController:Checked   = BusinessEntityDesignerSettings:CompileDatasetController
            THIS-OBJECT:to_GenerateDatasetController:Checked  = BusinessEntityDesignerSettings:GenerateDatasetController
            
            THIS-OBJECT:to_PromptSave:Checked                 = BusinessEntityDesignerSettings:PromptForSaveBeforeGenerate
            THIS-OBJECT:to_DistinctTempTableIncludes:Checked  = BusinessEntityDesignerSettings:DistinctTempTableIncludeFiles
                                                              
            THIS-OBJECT:cbReplaceUnderscore:Checked           = BusinessEntityDesignerSettings:ReplaceUnderScoreInTableName
            THIS-OBJECT:cbReplaceHyphens:Checked              = BusinessEntityDesignerSettings:ReplaceHyphenInTableName
                                                              
            THIS-OBJECT:cbSplitArrayFields:Checked            = BusinessEntityDesignerSettings:SplitArrayFields
                                                              
            THIS-OBJECT:tb_CustomServices:Text                = BusinessEntityDesignerSettings:CustomServices  
                                                              
            THIS-OBJECT:tb_launch_parameter:Text              = BusinessEntityDesignerSettings:LauncherCommandLine
                                                              
            THIS-OBJECT:gb_generator:VISIBLE                  = TRUE
            THIS-OBJECT:gb_designer:VISIBLE                   = FALSE  
            THIS-OBJECT:gb_designer2:VISIBLE                  = FALSE  
            THIS-OBJECT:gb_plugins:VISIBLE                    = FALSE
            THIS-OBJECT:gb_launcher:VISIBLE                   = FALSE
                                                              
            THIS-OBJECT:toUseLocalFileHistory:Checked         = BusinessEntityDesignerSettings:UseLocalFileHistory 
                                                              
            THIS-OBJECT:ultraStatusBar1:Text                  = BusinessEntityDesignerSettings:SettingsFile . 
            .       

        IF BusinessEntityDesignerSettings:KeywordCasing = "UPPER":U OR BusinessEntityDesignerSettings:KeywordCasing = "LOWER":U THEN  
            THIS-OBJECT:coKeywordCasing:Value = BOX (BusinessEntityDesignerSettings:KeywordCasing) .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the btn_use_current_commandline 
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID btn_use_current_commandline_Click (sender AS System.Object, 
	                                                       e AS System.EventArgs):
		
		DEFINE VARIABLE oDialogResultEnum AS DialogResultEnum NO-UNDO . 
		
		oDialogResultEnum = Consultingwerk.Util.MessageFormHelper:AskQuestion 
		                      ("Append -param open=%filename% ?"{&TRAN},
		                       "Business Entity Designer"{&TRAN}) .
		
		IF oDialogResultEnum = DialogResultEnum:DialogResultYes THEN 
		    tb_launch_parameter:Text = System.Environment:CommandLine + " -param open=%filename%":U .
		ELSE   
		    tb_launch_parameter:Text = System.Environment:CommandLine . 

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
        DEFINE VARIABLE ultraExplorerBarGroup1 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarGroup NO-UNDO.
        ultraExplorerBarGroup1 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarGroup().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem1 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem1 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.BusinessEntityDesigner.UI.BusinessEntityDesignerSettingsForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem2 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem2 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem4 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem4 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem3 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem3 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarItem5 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem NO-UNDO.
        ultraExplorerBarItem5 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton("LookupFolder":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton2 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton2 = NEW Infragistics.Win.UltraWinEditors.EditorButton("LookupFolder":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
        appearance9 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
        appearance10 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem1 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem1 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem2 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem2 = NEW Infragistics.Win.ValueListItem().
        THIS-OBJECT:ultraExplorerBar1 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBar().
        THIS-OBJECT:ultraPanel1 = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:gb_generator = NEW Infragistics.Win.Misc.UltraGroupBox().
        THIS-OBJECT:to_GenerateDatasetController = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:to_DistinctTempTableIncludes = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:to_PromptSave = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:to_CompileDatasetController = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:ultraLabel24 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tb_IncludeFiles = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel22 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel20 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tb_SourceFolder_Gui = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel21 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tb_RCodeFolder_Gui = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel11 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tb_SourceFolder = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel16 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tb_RCodeFolder = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:to_OpenFilesInIDE = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tb_generator = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tb_templatefolder = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel23 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:gb_launcher = NEW Infragistics.Win.Misc.UltraGroupBox().
        THIS-OBJECT:toUseLocalFileHistory = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:btn_use_current_commandline = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:tb_launch_parameter = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel33 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel34 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:gb_plugins = NEW Infragistics.Win.Misc.UltraGroupBox().
        THIS-OBJECT:ultraLabel25 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tb_CustomServices = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraButton3 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:tb_plugins = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel15 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:gb_designer = NEW Infragistics.Win.Misc.UltraGroupBox().
        THIS-OBJECT:ultraLabel26 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:cbReplaceUnderscore = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:cbReplaceHyphens = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:ultraLabel14 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tb_RelationFieldsGenerator = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel13 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel12 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tb_tablenamegenerator = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel10 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel9 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel8 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel7 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel6 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel5 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel4 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel3 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tb_beforesuffix = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tb_beforeprefix = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tb_namesuffix = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tb_nameprefix = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tb_datarelationnamesubstitute = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:gb_designer2 = NEW Infragistics.Win.Misc.UltraGroupBox().
        THIS-OBJECT:cbSplitArrayFields = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:ultraLabel17 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel18 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel19 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tb_fieldnamegenerator = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraButton1 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraButton2 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:folderBrowserDialog1 = NEW System.Windows.Forms.FolderBrowserDialog().
        THIS-OBJECT:openFileDialog1 = NEW System.Windows.Forms.OpenFileDialog().
        THIS-OBJECT:ultraStatusBar1 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusBar().
        THIS-OBJECT:ultraLabel27 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:coKeywordCasing = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        CAST(THIS-OBJECT:ultraExplorerBar1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraPanel1:ClientArea:SuspendLayout().
        THIS-OBJECT:ultraPanel1:SuspendLayout().
        CAST(THIS-OBJECT:gb_generator, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:gb_generator:SuspendLayout().
        CAST(THIS-OBJECT:to_GenerateDatasetController, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:to_DistinctTempTableIncludes, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:to_PromptSave, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:to_CompileDatasetController, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tb_IncludeFiles, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tb_SourceFolder_Gui, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tb_RCodeFolder_Gui, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tb_SourceFolder, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tb_RCodeFolder, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:to_OpenFilesInIDE, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tb_generator, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tb_templatefolder, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:gb_launcher, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:gb_launcher:SuspendLayout().
        CAST(THIS-OBJECT:toUseLocalFileHistory, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tb_launch_parameter, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:gb_plugins, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:gb_plugins:SuspendLayout().
        CAST(THIS-OBJECT:tb_CustomServices, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tb_plugins, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:gb_designer, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:gb_designer:SuspendLayout().
        CAST(THIS-OBJECT:cbReplaceUnderscore, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:cbReplaceHyphens, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tb_RelationFieldsGenerator, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tb_tablenamegenerator, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tb_beforesuffix, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tb_beforeprefix, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tb_namesuffix, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tb_nameprefix, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tb_datarelationnamesubstitute, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:gb_designer2, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:gb_designer2:SuspendLayout().
        CAST(THIS-OBJECT:cbSplitArrayFields, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tb_fieldnamegenerator, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraStatusBar1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:coKeywordCasing, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraExplorerBar1 */
        /*  */
        THIS-OBJECT:ultraExplorerBar1:AcceptsFocus = Infragistics.Win.DefaultableBoolean:False.
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraExplorerBar1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        ultraExplorerBarItem1:Key = "generator":U.
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        ultraExplorerBarItem1:Settings:AppearancesLarge:Appearance = appearance1.
        ultraExplorerBarItem1:Text = "Generator":U.
        ultraExplorerBarItem2:Key = "design":U.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        ultraExplorerBarItem2:Settings:AppearancesLarge:Appearance = appearance2.
        ultraExplorerBarItem2:Text = "Design":U.
        ultraExplorerBarItem4:Key = "design2":U.
        appearance3:Image = CAST(resources:GetObject("appearance3.Image":U), System.Object).
        ultraExplorerBarItem4:Settings:AppearancesLarge:Appearance = appearance3.
        ultraExplorerBarItem4:Text = "Design, cont.":U.
        ultraExplorerBarItem3:Key = "plugins":U.
        appearance4:Image = CAST(resources:GetObject("appearance4.Image":U), System.Object).
        ultraExplorerBarItem3:Settings:AppearancesLarge:Appearance = appearance4.
        ultraExplorerBarItem3:Text = "Plugins":U.
        ultraExplorerBarItem5:Key = "launcher":U.
        appearance5:Image = CAST(resources:GetObject("appearance5.Image":U), System.Object).
        ultraExplorerBarItem5:Settings:AppearancesLarge:Appearance = appearance5.
        ultraExplorerBarItem5:Text = "Launcher":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarItem EXTENT 5 NO-UNDO.
        arrayvar0[1] = ultraExplorerBarItem1.
        arrayvar0[2] = ultraExplorerBarItem2.
        arrayvar0[3] = ultraExplorerBarItem4.
        arrayvar0[4] = ultraExplorerBarItem3.
        arrayvar0[5] = ultraExplorerBarItem5.
        ultraExplorerBarGroup1:Items:AddRange(arrayvar0).
        ultraExplorerBarGroup1:Text = "New Group":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarGroup EXTENT 1 NO-UNDO.
        arrayvar1[1] = ultraExplorerBarGroup1.
        THIS-OBJECT:ultraExplorerBar1:Groups:AddRange(arrayvar1).
        THIS-OBJECT:ultraExplorerBar1:GroupSettings:HeaderVisible = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraExplorerBar1:GroupSettings:HotTracking = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:ultraExplorerBar1:GroupSettings:Style = Infragistics.Win.UltraWinExplorerBar.GroupStyle:LargeImagesWithText.
        THIS-OBJECT:ultraExplorerBar1:Location = NEW System.Drawing.Point(12, 12).
        THIS-OBJECT:ultraExplorerBar1:Name = "ultraExplorerBar1":U.
        THIS-OBJECT:ultraExplorerBar1:Size = NEW System.Drawing.Size(123, 376).
        THIS-OBJECT:ultraExplorerBar1:Style = Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarStyle:Listbar.
        THIS-OBJECT:ultraExplorerBar1:TabIndex = 1.
        THIS-OBJECT:ultraExplorerBar1:UseOsThemes = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:ultraExplorerBar1:ViewStyle = Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarViewStyle:Office2007.
        THIS-OBJECT:ultraExplorerBar1:ItemClick:Subscribe(THIS-OBJECT:ultraExplorerBar1_ItemClick).
        /*  */
        /* ultraPanel1 */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraPanel1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        appearance6:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance6:BorderColor = System.Drawing.SystemColors:MenuHighlight.
        THIS-OBJECT:ultraPanel1:Appearance = appearance6.
        THIS-OBJECT:ultraPanel1:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        /*  */
        /* ultraPanel1.ClientArea */
        /*  */
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:gb_generator).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:gb_launcher).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:gb_plugins).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:gb_designer).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:gb_designer2).
        THIS-OBJECT:ultraPanel1:Location = NEW System.Drawing.Point(141, 12).
        THIS-OBJECT:ultraPanel1:Name = "ultraPanel1":U.
        THIS-OBJECT:ultraPanel1:Size = NEW System.Drawing.Size(464, 376).
        THIS-OBJECT:ultraPanel1:TabIndex = 2.
        /*  */
        /* gb_generator */
        /*  */
        THIS-OBJECT:gb_generator:CaptionAlignment = Infragistics.Win.Misc.GroupBoxCaptionAlignment:Near.
        THIS-OBJECT:gb_generator:Controls:Add(THIS-OBJECT:coKeywordCasing).
        THIS-OBJECT:gb_generator:Controls:Add(THIS-OBJECT:ultraLabel27).
        THIS-OBJECT:gb_generator:Controls:Add(THIS-OBJECT:to_GenerateDatasetController).
        THIS-OBJECT:gb_generator:Controls:Add(THIS-OBJECT:to_DistinctTempTableIncludes).
        THIS-OBJECT:gb_generator:Controls:Add(THIS-OBJECT:to_PromptSave).
        THIS-OBJECT:gb_generator:Controls:Add(THIS-OBJECT:to_CompileDatasetController).
        THIS-OBJECT:gb_generator:Controls:Add(THIS-OBJECT:ultraLabel24).
        THIS-OBJECT:gb_generator:Controls:Add(THIS-OBJECT:tb_IncludeFiles).
        THIS-OBJECT:gb_generator:Controls:Add(THIS-OBJECT:ultraLabel22).
        THIS-OBJECT:gb_generator:Controls:Add(THIS-OBJECT:ultraLabel20).
        THIS-OBJECT:gb_generator:Controls:Add(THIS-OBJECT:tb_SourceFolder_Gui).
        THIS-OBJECT:gb_generator:Controls:Add(THIS-OBJECT:ultraLabel21).
        THIS-OBJECT:gb_generator:Controls:Add(THIS-OBJECT:tb_RCodeFolder_Gui).
        THIS-OBJECT:gb_generator:Controls:Add(THIS-OBJECT:ultraLabel11).
        THIS-OBJECT:gb_generator:Controls:Add(THIS-OBJECT:tb_SourceFolder).
        THIS-OBJECT:gb_generator:Controls:Add(THIS-OBJECT:ultraLabel16).
        THIS-OBJECT:gb_generator:Controls:Add(THIS-OBJECT:tb_RCodeFolder).
        THIS-OBJECT:gb_generator:Controls:Add(THIS-OBJECT:to_OpenFilesInIDE).
        THIS-OBJECT:gb_generator:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:gb_generator:Controls:Add(THIS-OBJECT:tb_generator).
        THIS-OBJECT:gb_generator:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:gb_generator:Controls:Add(THIS-OBJECT:tb_templatefolder).
        THIS-OBJECT:gb_generator:Controls:Add(THIS-OBJECT:ultraLabel23).
        THIS-OBJECT:gb_generator:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:gb_generator:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:gb_generator:Name = "gb_generator":U.
        THIS-OBJECT:gb_generator:Size = NEW System.Drawing.Size(462, 374).
        THIS-OBJECT:gb_generator:TabIndex = 1.
        THIS-OBJECT:gb_generator:Text = "Generator Settings":U.
        THIS-OBJECT:gb_generator:ViewStyle = Infragistics.Win.Misc.GroupBoxViewStyle:Office2000.
        /*  */
        /* to_GenerateDatasetController */
        /*  */
        THIS-OBJECT:to_GenerateDatasetController:AutoSize = TRUE.
        THIS-OBJECT:to_GenerateDatasetController:Location = NEW System.Drawing.Point(12, 96).
        THIS-OBJECT:to_GenerateDatasetController:Name = "to_GenerateDatasetController":U.
        THIS-OBJECT:to_GenerateDatasetController:Size = NEW System.Drawing.Size(163, 17).
        THIS-OBJECT:to_GenerateDatasetController:TabIndex = 24.
        THIS-OBJECT:to_GenerateDatasetController:Text = "Generate Dataset Controller":U.
        /*  */
        /* to_DistinctTempTableIncludes */
        /*  */
        THIS-OBJECT:to_DistinctTempTableIncludes:AutoSize = TRUE.
        THIS-OBJECT:to_DistinctTempTableIncludes:Location = NEW System.Drawing.Point(12, 119).
        THIS-OBJECT:to_DistinctTempTableIncludes:Name = "to_DistinctTempTableIncludes":U.
        THIS-OBJECT:to_DistinctTempTableIncludes:Size = NEW System.Drawing.Size(279, 17).
        THIS-OBJECT:to_DistinctTempTableIncludes:TabIndex = 23.
        THIS-OBJECT:to_DistinctTempTableIncludes:Text = "Use Distinct Include Files for Temp-Table Definition":U.
        /*  */
        /* to_PromptSave */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:to_PromptSave:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:to_PromptSave:AutoSize = TRUE.
        THIS-OBJECT:to_PromptSave:Location = NEW System.Drawing.Point(199, 73).
        THIS-OBJECT:to_PromptSave:Name = "to_PromptSave":U.
        THIS-OBJECT:to_PromptSave:Size = NEW System.Drawing.Size(193, 17).
        THIS-OBJECT:to_PromptSave:TabIndex = 22.
        THIS-OBJECT:to_PromptSave:Text = "Prompt for save before generating":U.
        /*  */
        /* to_CompileDatasetController */
        /*  */
        THIS-OBJECT:to_CompileDatasetController:AutoSize = TRUE.
        THIS-OBJECT:to_CompileDatasetController:Location = NEW System.Drawing.Point(199, 96).
        THIS-OBJECT:to_CompileDatasetController:Name = "to_CompileDatasetController":U.
        THIS-OBJECT:to_CompileDatasetController:Size = NEW System.Drawing.Size(157, 17).
        THIS-OBJECT:to_CompileDatasetController:TabIndex = 21.
        THIS-OBJECT:to_CompileDatasetController:Text = "Compile Dataset Controller":U.
        /*  */
        /* ultraLabel24 */
        /*  */
        THIS-OBJECT:ultraLabel24:Location = NEW System.Drawing.Point(9, 338).
        THIS-OBJECT:ultraLabel24:Name = "ultraLabel24":U.
        THIS-OBJECT:ultraLabel24:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel24:TabIndex = 16.
        THIS-OBJECT:ultraLabel24:Text = "Include files folder:":U.
        /*  */
        /* tb_IncludeFiles */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_IncludeFiles:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_IncludeFiles:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tb_IncludeFiles:Location = NEW System.Drawing.Point(115, 334).
        THIS-OBJECT:tb_IncludeFiles:Name = "tb_IncludeFiles":U.
        THIS-OBJECT:tb_IncludeFiles:ShowOverflowIndicator = TRUE.
        THIS-OBJECT:tb_IncludeFiles:Size = NEW System.Drawing.Size(341, 21).
        THIS-OBJECT:tb_IncludeFiles:TabIndex = 15.
        THIS-OBJECT:tb_IncludeFiles:Text = ".":U.
        /*  */
        /* ultraLabel22 */
        /*  */
        THIS-OBJECT:ultraLabel22:Location = NEW System.Drawing.Point(9, 142).
        THIS-OBJECT:ultraLabel22:Name = "ultraLabel22":U.
        THIS-OBJECT:ultraLabel22:Size = NEW System.Drawing.Size(141, 23).
        THIS-OBJECT:ultraLabel22:TabIndex = 13.
        THIS-OBJECT:ultraLabel22:Text = "Business Logic Objects:":U.
        /*  */
        /* ultraLabel20 */
        /*  */
        THIS-OBJECT:ultraLabel20:Location = NEW System.Drawing.Point(8, 293).
        THIS-OBJECT:ultraLabel20:Name = "ultraLabel20":U.
        THIS-OBJECT:ultraLabel20:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel20:TabIndex = 12.
        THIS-OBJECT:ultraLabel20:Text = "R-Code folder:":U.
        /*  */
        /* tb_SourceFolder_Gui */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_SourceFolder_Gui:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_SourceFolder_Gui:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tb_SourceFolder_Gui:Location = NEW System.Drawing.Point(114, 262).
        THIS-OBJECT:tb_SourceFolder_Gui:Name = "tb_SourceFolder_Gui":U.
        THIS-OBJECT:tb_SourceFolder_Gui:ShowOverflowIndicator = TRUE.
        THIS-OBJECT:tb_SourceFolder_Gui:Size = NEW System.Drawing.Size(341, 21).
        THIS-OBJECT:tb_SourceFolder_Gui:TabIndex = 9.
        THIS-OBJECT:tb_SourceFolder_Gui:Text = ".":U.
        /*  */
        /* ultraLabel21 */
        /*  */
        THIS-OBJECT:ultraLabel21:Location = NEW System.Drawing.Point(8, 266).
        THIS-OBJECT:ultraLabel21:Name = "ultraLabel21":U.
        THIS-OBJECT:ultraLabel21:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel21:TabIndex = 11.
        THIS-OBJECT:ultraLabel21:Text = "Source folder:":U.
        /*  */
        /* tb_RCodeFolder_Gui */
        /*  */
        DEFINE VARIABLE nestedvar5 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar5 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_RCodeFolder_Gui:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar5, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_RCodeFolder_Gui:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tb_RCodeFolder_Gui:Location = NEW System.Drawing.Point(114, 289).
        THIS-OBJECT:tb_RCodeFolder_Gui:Name = "tb_RCodeFolder_Gui":U.
        THIS-OBJECT:tb_RCodeFolder_Gui:ShowOverflowIndicator = TRUE.
        THIS-OBJECT:tb_RCodeFolder_Gui:Size = NEW System.Drawing.Size(341, 21).
        THIS-OBJECT:tb_RCodeFolder_Gui:TabIndex = 10.
        THIS-OBJECT:tb_RCodeFolder_Gui:Text = ".":U.
        /*  */
        /* ultraLabel11 */
        /*  */
        THIS-OBJECT:ultraLabel11:Location = NEW System.Drawing.Point(8, 198).
        THIS-OBJECT:ultraLabel11:Name = "ultraLabel11":U.
        THIS-OBJECT:ultraLabel11:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel11:TabIndex = 8.
        THIS-OBJECT:ultraLabel11:Text = "R-Code folder:":U.
        /*  */
        /* tb_SourceFolder */
        /*  */
        DEFINE VARIABLE nestedvar6 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar6 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_SourceFolder:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar6, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_SourceFolder:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tb_SourceFolder:Location = NEW System.Drawing.Point(114, 167).
        THIS-OBJECT:tb_SourceFolder:Name = "tb_SourceFolder":U.
        THIS-OBJECT:tb_SourceFolder:ShowOverflowIndicator = TRUE.
        THIS-OBJECT:tb_SourceFolder:Size = NEW System.Drawing.Size(341, 21).
        THIS-OBJECT:tb_SourceFolder:TabIndex = 5.
        THIS-OBJECT:tb_SourceFolder:Text = ".":U.
        /*  */
        /* ultraLabel16 */
        /*  */
        THIS-OBJECT:ultraLabel16:Location = NEW System.Drawing.Point(8, 171).
        THIS-OBJECT:ultraLabel16:Name = "ultraLabel16":U.
        THIS-OBJECT:ultraLabel16:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel16:TabIndex = 7.
        THIS-OBJECT:ultraLabel16:Text = "Source folder:":U.
        /*  */
        /* tb_RCodeFolder */
        /*  */
        DEFINE VARIABLE nestedvar7 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar7 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_RCodeFolder:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar7, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_RCodeFolder:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tb_RCodeFolder:Location = NEW System.Drawing.Point(114, 194).
        THIS-OBJECT:tb_RCodeFolder:Name = "tb_RCodeFolder":U.
        THIS-OBJECT:tb_RCodeFolder:ShowOverflowIndicator = TRUE.
        THIS-OBJECT:tb_RCodeFolder:Size = NEW System.Drawing.Size(341, 21).
        THIS-OBJECT:tb_RCodeFolder:TabIndex = 6.
        THIS-OBJECT:tb_RCodeFolder:Text = ".":U.
        /*  */
        /* to_OpenFilesInIDE */
        /*  */
        DEFINE VARIABLE nestedvar8 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar8 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:to_OpenFilesInIDE:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar8, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:to_OpenFilesInIDE:AutoSize = TRUE.
        THIS-OBJECT:to_OpenFilesInIDE:Location = NEW System.Drawing.Point(12, 73).
        THIS-OBJECT:to_OpenFilesInIDE:Name = "to_OpenFilesInIDE":U.
        THIS-OBJECT:to_OpenFilesInIDE:Size = NEW System.Drawing.Size(160, 17).
        THIS-OBJECT:to_OpenFilesInIDE:TabIndex = 4.
        THIS-OBJECT:to_OpenFilesInIDE:Text = "Open generated files in IDE":U.
        /*  */
        /* ultraLabel2 */
        /*  */
        THIS-OBJECT:ultraLabel2:Location = NEW System.Drawing.Point(9, 50).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        THIS-OBJECT:ultraLabel2:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel2:TabIndex = 3.
        THIS-OBJECT:ultraLabel2:Text = "Template Folder:":U.
        /*  */
        /* tb_generator */
        /*  */
        DEFINE VARIABLE nestedvar9 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar9 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_generator:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar9, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_generator:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tb_generator:Location = NEW System.Drawing.Point(115, 19).
        THIS-OBJECT:tb_generator:Name = "tb_generator":U.
        THIS-OBJECT:tb_generator:ShowOverflowIndicator = TRUE.
        THIS-OBJECT:tb_generator:Size = NEW System.Drawing.Size(341, 21).
        THIS-OBJECT:tb_generator:TabIndex = 0.
        THIS-OBJECT:tb_generator:Text = ".":U.
        /*  */
        /* ultraLabel1 */
        /*  */
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(9, 23).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel1:TabIndex = 2.
        THIS-OBJECT:ultraLabel1:Text = "Generator:":U.
        /*  */
        /* tb_templatefolder */
        /*  */
        DEFINE VARIABLE nestedvar10 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar10 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_templatefolder:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar10, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton1:Key = "LookupFolder":U.
        editorButton1:Text = "...":U.
        THIS-OBJECT:tb_templatefolder:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:tb_templatefolder:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tb_templatefolder:Location = NEW System.Drawing.Point(115, 46).
        THIS-OBJECT:tb_templatefolder:Name = "tb_templatefolder":U.
        THIS-OBJECT:tb_templatefolder:ShowOverflowIndicator = TRUE.
        THIS-OBJECT:tb_templatefolder:Size = NEW System.Drawing.Size(341, 21).
        THIS-OBJECT:tb_templatefolder:TabIndex = 1.
        THIS-OBJECT:tb_templatefolder:Text = "ultraTextEditor2":U.
        THIS-OBJECT:tb_templatefolder:EditorButtonClick:Subscribe(THIS-OBJECT:tb_templatefolder_EditorButtonClick).
        THIS-OBJECT:tb_templatefolder:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        /*  */
        /* ultraLabel23 */
        /*  */
        THIS-OBJECT:ultraLabel23:Location = NEW System.Drawing.Point(9, 241).
        THIS-OBJECT:ultraLabel23:Name = "ultraLabel23":U.
        THIS-OBJECT:ultraLabel23:Size = NEW System.Drawing.Size(141, 23).
        THIS-OBJECT:ultraLabel23:TabIndex = 14.
        THIS-OBJECT:ultraLabel23:Text = "User Interface Objects:":U.
        /*  */
        /* gb_launcher */
        /*  */
        THIS-OBJECT:gb_launcher:CaptionAlignment = Infragistics.Win.Misc.GroupBoxCaptionAlignment:Near.
        THIS-OBJECT:gb_launcher:Controls:Add(THIS-OBJECT:toUseLocalFileHistory).
        THIS-OBJECT:gb_launcher:Controls:Add(THIS-OBJECT:btn_use_current_commandline).
        THIS-OBJECT:gb_launcher:Controls:Add(THIS-OBJECT:tb_launch_parameter).
        THIS-OBJECT:gb_launcher:Controls:Add(THIS-OBJECT:ultraLabel33).
        THIS-OBJECT:gb_launcher:Controls:Add(THIS-OBJECT:ultraLabel34).
        THIS-OBJECT:gb_launcher:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:gb_launcher:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:gb_launcher:Name = "gb_launcher":U.
        THIS-OBJECT:gb_launcher:Size = NEW System.Drawing.Size(462, 374).
        THIS-OBJECT:gb_launcher:TabIndex = 21.
        THIS-OBJECT:gb_launcher:Text = "Launcher Settings":U.
        THIS-OBJECT:gb_launcher:ViewStyle = Infragistics.Win.Misc.GroupBoxViewStyle:Office2000.
        /*  */
        /* toUseLocalFileHistory */
        /*  */
        THIS-OBJECT:toUseLocalFileHistory:Location = NEW System.Drawing.Point(12, 171).
        THIS-OBJECT:toUseLocalFileHistory:Name = "toUseLocalFileHistory":U.
        THIS-OBJECT:toUseLocalFileHistory:Size = NEW System.Drawing.Size(442, 20).
        THIS-OBJECT:toUseLocalFileHistory:TabIndex = 16.
        THIS-OBJECT:toUseLocalFileHistory:Text = "Use local file history for .bedgm files (current history will be lost)":U.
        /*  */
        /* btn_use_current_commandline */
        /*  */
        DEFINE VARIABLE nestedvar11 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar11 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btn_use_current_commandline:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar11, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btn_use_current_commandline:Location = NEW System.Drawing.Point(134, 46).
        THIS-OBJECT:btn_use_current_commandline:Name = "btn_use_current_commandline":U.
        THIS-OBJECT:btn_use_current_commandline:Size = NEW System.Drawing.Size(320, 23).
        THIS-OBJECT:btn_use_current_commandline:TabIndex = 15.
        THIS-OBJECT:btn_use_current_commandline:Text = "Use current command line":U.
        THIS-OBJECT:btn_use_current_commandline:Click:Subscribe(THIS-OBJECT:btn_use_current_commandline_Click).
        /*  */
        /* tb_launch_parameter */
        /*  */
        DEFINE VARIABLE nestedvar12 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar12 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_launch_parameter:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar12, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_launch_parameter:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tb_launch_parameter:Location = NEW System.Drawing.Point(134, 19).
        THIS-OBJECT:tb_launch_parameter:Name = "tb_launch_parameter":U.
        THIS-OBJECT:tb_launch_parameter:ShowOverflowIndicator = TRUE.
        THIS-OBJECT:tb_launch_parameter:Size = NEW System.Drawing.Size(322, 21).
        THIS-OBJECT:tb_launch_parameter:TabIndex = 0.
        THIS-OBJECT:tb_launch_parameter:Text = ".":U.
        /*  */
        /* ultraLabel33 */
        /*  */
        THIS-OBJECT:ultraLabel33:Location = NEW System.Drawing.Point(9, 23).
        THIS-OBJECT:ultraLabel33:Name = "ultraLabel33":U.
        THIS-OBJECT:ultraLabel33:Size = NEW System.Drawing.Size(119, 23).
        THIS-OBJECT:ultraLabel33:TabIndex = 2.
        THIS-OBJECT:ultraLabel33:Text = "Launch Parameters:":U.
        THIS-OBJECT:ultraLabel33:WrapText = FALSE.
        /*  */
        /* ultraLabel34 */
        /*  */
        THIS-OBJECT:ultraLabel34:Location = NEW System.Drawing.Point(11, 78).
        THIS-OBJECT:ultraLabel34:Name = "ultraLabel34":U.
        THIS-OBJECT:ultraLabel34:Size = NEW System.Drawing.Size(411, 115).
        THIS-OBJECT:ultraLabel34:TabIndex = 14.
        THIS-OBJECT:ultraLabel34:Text = resources:GetString("ultraLabel34.Text":U).
        /*  */
        /* gb_plugins */
        /*  */
        THIS-OBJECT:gb_plugins:Controls:Add(THIS-OBJECT:ultraLabel25).
        THIS-OBJECT:gb_plugins:Controls:Add(THIS-OBJECT:tb_CustomServices).
        THIS-OBJECT:gb_plugins:Controls:Add(THIS-OBJECT:ultraButton3).
        THIS-OBJECT:gb_plugins:Controls:Add(THIS-OBJECT:tb_plugins).
        THIS-OBJECT:gb_plugins:Controls:Add(THIS-OBJECT:ultraLabel15).
        THIS-OBJECT:gb_plugins:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:gb_plugins:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:gb_plugins:Name = "gb_plugins":U.
        THIS-OBJECT:gb_plugins:Size = NEW System.Drawing.Size(462, 374).
        THIS-OBJECT:gb_plugins:TabIndex = 19.
        THIS-OBJECT:gb_plugins:Text = "Plugins":U.
        THIS-OBJECT:gb_plugins:ViewStyle = Infragistics.Win.Misc.GroupBoxViewStyle:Office2000.
        /*  */
        /* ultraLabel25 */
        /*  */
        DEFINE VARIABLE nestedvar13 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar13 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:ultraLabel25:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar13, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraLabel25:Location = NEW System.Drawing.Point(8, 349).
        THIS-OBJECT:ultraLabel25:Name = "ultraLabel25":U.
        THIS-OBJECT:ultraLabel25:Size = NEW System.Drawing.Size(121, 19).
        THIS-OBJECT:ultraLabel25:TabIndex = 5.
        THIS-OBJECT:ultraLabel25:Text = "Custom Services .xml:":U.
        /*  */
        /* tb_CustomServices */
        /*  */
        DEFINE VARIABLE nestedvar14 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar14 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Bottom, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_CustomServices:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar14, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton2:Key = "LookupFolder":U.
        editorButton2:Text = "...":U.
        THIS-OBJECT:tb_CustomServices:ButtonsRight:Add(editorButton2).
        THIS-OBJECT:tb_CustomServices:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tb_CustomServices:Location = NEW System.Drawing.Point(135, 345).
        THIS-OBJECT:tb_CustomServices:Name = "tb_CustomServices":U.
        THIS-OBJECT:tb_CustomServices:ShowOverflowIndicator = TRUE.
        THIS-OBJECT:tb_CustomServices:Size = NEW System.Drawing.Size(320, 21).
        THIS-OBJECT:tb_CustomServices:TabIndex = 4.
        THIS-OBJECT:tb_CustomServices:EditorButtonClick:Subscribe(THIS-OBJECT:tb_CustomServices_EditorButtonClick).
        THIS-OBJECT:tb_CustomServices:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        /*  */
        /* ultraButton3 */
        /*  */
        DEFINE VARIABLE nestedvar15 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar15 = System.Windows.Forms.AnchorStyles:Top.
        THIS-OBJECT:ultraButton3:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar15, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        appearance7:Image = CAST(resources:GetObject("appearance7.Image":U), System.Object).
        THIS-OBJECT:ultraButton3:Appearance = appearance7.
        THIS-OBJECT:ultraButton3:Location = NEW System.Drawing.Point(341, 24).
        THIS-OBJECT:ultraButton3:Name = "ultraButton3":U.
        THIS-OBJECT:ultraButton3:Size = NEW System.Drawing.Size(114, 23).
        THIS-OBJECT:ultraButton3:TabIndex = 2.
        THIS-OBJECT:ultraButton3:Text = "Select Plugin":U.
        THIS-OBJECT:ultraButton3:Click:Subscribe(THIS-OBJECT:ultraButton3_Click).
        /*  */
        /* tb_plugins */
        /*  */
        DEFINE VARIABLE nestedvar16 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar16 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_plugins:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar16, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_plugins:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tb_plugins:Location = NEW System.Drawing.Point(9, 52).
        THIS-OBJECT:tb_plugins:Multiline = TRUE.
        THIS-OBJECT:tb_plugins:Name = "tb_plugins":U.
        THIS-OBJECT:tb_plugins:ShowOverflowIndicator = TRUE.
        THIS-OBJECT:tb_plugins:Size = NEW System.Drawing.Size(447, 283).
        THIS-OBJECT:tb_plugins:TabIndex = 1.
        /*  */
        /* ultraLabel15 */
        /*  */
        THIS-OBJECT:ultraLabel15:Location = NEW System.Drawing.Point(9, 21).
        THIS-OBJECT:ultraLabel15:Name = "ultraLabel15":U.
        THIS-OBJECT:ultraLabel15:Size = NEW System.Drawing.Size(413, 29).
        THIS-OBJECT:ultraLabel15:TabIndex = 0.
        THIS-OBJECT:ultraLabel15:Text = "Enter Plugin class-names (one per line)~r~nBegin line with # to deactivate Plugin ":U.
        /*  */
        /* gb_designer */
        /*  */
        THIS-OBJECT:gb_designer:Controls:Add(THIS-OBJECT:ultraLabel26).
        THIS-OBJECT:gb_designer:Controls:Add(THIS-OBJECT:cbReplaceUnderscore).
        THIS-OBJECT:gb_designer:Controls:Add(THIS-OBJECT:cbReplaceHyphens).
        THIS-OBJECT:gb_designer:Controls:Add(THIS-OBJECT:ultraLabel14).
        THIS-OBJECT:gb_designer:Controls:Add(THIS-OBJECT:tb_RelationFieldsGenerator).
        THIS-OBJECT:gb_designer:Controls:Add(THIS-OBJECT:ultraLabel13).
        THIS-OBJECT:gb_designer:Controls:Add(THIS-OBJECT:ultraLabel12).
        THIS-OBJECT:gb_designer:Controls:Add(THIS-OBJECT:tb_tablenamegenerator).
        THIS-OBJECT:gb_designer:Controls:Add(THIS-OBJECT:ultraLabel10).
        THIS-OBJECT:gb_designer:Controls:Add(THIS-OBJECT:ultraLabel9).
        THIS-OBJECT:gb_designer:Controls:Add(THIS-OBJECT:ultraLabel8).
        THIS-OBJECT:gb_designer:Controls:Add(THIS-OBJECT:ultraLabel7).
        THIS-OBJECT:gb_designer:Controls:Add(THIS-OBJECT:ultraLabel6).
        THIS-OBJECT:gb_designer:Controls:Add(THIS-OBJECT:ultraLabel5).
        THIS-OBJECT:gb_designer:Controls:Add(THIS-OBJECT:ultraLabel4).
        THIS-OBJECT:gb_designer:Controls:Add(THIS-OBJECT:ultraLabel3).
        THIS-OBJECT:gb_designer:Controls:Add(THIS-OBJECT:tb_beforesuffix).
        THIS-OBJECT:gb_designer:Controls:Add(THIS-OBJECT:tb_beforeprefix).
        THIS-OBJECT:gb_designer:Controls:Add(THIS-OBJECT:tb_namesuffix).
        THIS-OBJECT:gb_designer:Controls:Add(THIS-OBJECT:tb_nameprefix).
        THIS-OBJECT:gb_designer:Controls:Add(THIS-OBJECT:tb_datarelationnamesubstitute).
        THIS-OBJECT:gb_designer:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:gb_designer:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:gb_designer:Name = "gb_designer":U.
        THIS-OBJECT:gb_designer:Size = NEW System.Drawing.Size(462, 374).
        THIS-OBJECT:gb_designer:TabIndex = 4.
        THIS-OBJECT:gb_designer:Text = "Design Settings":U.
        THIS-OBJECT:gb_designer:ViewStyle = Infragistics.Win.Misc.GroupBoxViewStyle:Office2000.
        /*  */
        /* ultraLabel26 */
        /*  */
        THIS-OBJECT:ultraLabel26:Location = NEW System.Drawing.Point(8, 343).
        THIS-OBJECT:ultraLabel26:Name = "ultraLabel26":U.
        THIS-OBJECT:ultraLabel26:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel26:TabIndex = 21.
        THIS-OBJECT:ultraLabel26:Text = "CamcelCase:":U.
        /*  */
        /* cbReplaceUnderscore */
        /*  */
        THIS-OBJECT:cbReplaceUnderscore:Location = NEW System.Drawing.Point(240, 340).
        THIS-OBJECT:cbReplaceUnderscore:Name = "cbReplaceUnderscore":U.
        THIS-OBJECT:cbReplaceUnderscore:Size = NEW System.Drawing.Size(120, 20).
        THIS-OBJECT:cbReplaceUnderscore:TabIndex = 20.
        THIS-OBJECT:cbReplaceUnderscore:Text = "Underscore":U.
        /*  */
        /* cbReplaceHyphens */
        /*  */
        THIS-OBJECT:cbReplaceHyphens:Location = NEW System.Drawing.Point(115, 340).
        THIS-OBJECT:cbReplaceHyphens:Name = "cbReplaceHyphens":U.
        THIS-OBJECT:cbReplaceHyphens:Size = NEW System.Drawing.Size(120, 20).
        THIS-OBJECT:cbReplaceHyphens:TabIndex = 19.
        THIS-OBJECT:cbReplaceHyphens:Text = "Hyphens ~"-~"":U.
        /*  */
        /* ultraLabel14 */
        /*  */
        THIS-OBJECT:ultraLabel14:Location = NEW System.Drawing.Point(9, 104).
        THIS-OBJECT:ultraLabel14:Name = "ultraLabel14":U.
        THIS-OBJECT:ultraLabel14:Size = NEW System.Drawing.Size(141, 23).
        THIS-OBJECT:ultraLabel14:TabIndex = 18.
        THIS-OBJECT:ultraLabel14:Text = "Relation-Fields Generator:":U.
        /*  */
        /* tb_RelationFieldsGenerator */
        /*  */
        DEFINE VARIABLE nestedvar17 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar17 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_RelationFieldsGenerator:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar17, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_RelationFieldsGenerator:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tb_RelationFieldsGenerator:Location = NEW System.Drawing.Point(156, 100).
        THIS-OBJECT:tb_RelationFieldsGenerator:Name = "tb_RelationFieldsGenerator":U.
        THIS-OBJECT:tb_RelationFieldsGenerator:ShowOverflowIndicator = TRUE.
        THIS-OBJECT:tb_RelationFieldsGenerator:Size = NEW System.Drawing.Size(300, 21).
        THIS-OBJECT:tb_RelationFieldsGenerator:TabIndex = 17.
        THIS-OBJECT:tb_RelationFieldsGenerator:Text = "ultraTextEditor1":U.
        /*  */
        /* ultraLabel13 */
        /*  */
        THIS-OBJECT:ultraLabel13:Location = NEW System.Drawing.Point(115, 317).
        THIS-OBJECT:ultraLabel13:Name = "ultraLabel13":U.
        THIS-OBJECT:ultraLabel13:Size = NEW System.Drawing.Size(307, 23).
        THIS-OBJECT:ultraLabel13:TabIndex = 16.
        THIS-OBJECT:ultraLabel13:Text = "(restart business entity designer after making a change)":U.
        /*  */
        /* ultraLabel12 */
        /*  */
        THIS-OBJECT:ultraLabel12:Location = NEW System.Drawing.Point(9, 296).
        THIS-OBJECT:ultraLabel12:Name = "ultraLabel12":U.
        THIS-OBJECT:ultraLabel12:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel12:TabIndex = 13.
        THIS-OBJECT:ultraLabel12:Text = "Name Generator:":U.
        /*  */
        /* tb_tablenamegenerator */
        /*  */
        DEFINE VARIABLE nestedvar18 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar18 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_tablenamegenerator:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar18, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_tablenamegenerator:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tb_tablenamegenerator:Location = NEW System.Drawing.Point(115, 292).
        THIS-OBJECT:tb_tablenamegenerator:Name = "tb_tablenamegenerator":U.
        THIS-OBJECT:tb_tablenamegenerator:ShowOverflowIndicator = TRUE.
        THIS-OBJECT:tb_tablenamegenerator:Size = NEW System.Drawing.Size(341, 21).
        THIS-OBJECT:tb_tablenamegenerator:TabIndex = 14.
        THIS-OBJECT:tb_tablenamegenerator:Text = "Consultingwerk.BusinessEntityDesigner.Services.TableNameGeneratorService":U.
        /*  */
        /* ultraLabel10 */
        /*  */
        THIS-OBJECT:ultraLabel10:Enabled = FALSE.
        THIS-OBJECT:ultraLabel10:Location = NEW System.Drawing.Point(156, 67).
        THIS-OBJECT:ultraLabel10:Name = "ultraLabel10":U.
        THIS-OBJECT:ultraLabel10:Size = NEW System.Drawing.Size(266, 27).
        THIS-OBJECT:ultraLabel10:TabIndex = 12.
        THIS-OBJECT:ultraLabel10:Text = "&&1 will be substituted with the source table name, &&2 will be substituted with the target table name":U.
        /*  */
        /* ultraLabel9 */
        /*  */
        appearance8:FontData:BoldAsString = "True":U.
        THIS-OBJECT:ultraLabel9:Appearance = appearance8.
        THIS-OBJECT:ultraLabel9:Location = NEW System.Drawing.Point(9, 142).
        THIS-OBJECT:ultraLabel9:Name = "ultraLabel9":U.
        THIS-OBJECT:ultraLabel9:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel9:TabIndex = 11.
        THIS-OBJECT:ultraLabel9:Text = "Temp-Tables":U.
        /*  */
        /* ultraLabel8 */
        /*  */
        THIS-OBJECT:ultraLabel8:Location = NEW System.Drawing.Point(9, 245).
        THIS-OBJECT:ultraLabel8:Name = "ultraLabel8":U.
        THIS-OBJECT:ultraLabel8:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel8:TabIndex = 8.
        THIS-OBJECT:ultraLabel8:Text = "Before-Suffix:":U.
        /*  */
        /* ultraLabel7 */
        /*  */
        THIS-OBJECT:ultraLabel7:Location = NEW System.Drawing.Point(9, 218).
        THIS-OBJECT:ultraLabel7:Name = "ultraLabel7":U.
        THIS-OBJECT:ultraLabel7:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel7:TabIndex = 6.
        THIS-OBJECT:ultraLabel7:Text = "Before-Prefix:":U.
        /*  */
        /* ultraLabel6 */
        /*  */
        THIS-OBJECT:ultraLabel6:Location = NEW System.Drawing.Point(9, 191).
        THIS-OBJECT:ultraLabel6:Name = "ultraLabel6":U.
        THIS-OBJECT:ultraLabel6:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel6:TabIndex = 4.
        THIS-OBJECT:ultraLabel6:Text = "Name-Suffix:":U.
        /*  */
        /* ultraLabel5 */
        /*  */
        THIS-OBJECT:ultraLabel5:Location = NEW System.Drawing.Point(9, 164).
        THIS-OBJECT:ultraLabel5:Name = "ultraLabel5":U.
        THIS-OBJECT:ultraLabel5:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel5:TabIndex = 2.
        THIS-OBJECT:ultraLabel5:Text = "Name-Prefix:":U.
        /*  */
        /* ultraLabel4 */
        /*  */
        THIS-OBJECT:ultraLabel4:Location = NEW System.Drawing.Point(6, 44).
        THIS-OBJECT:ultraLabel4:Name = "ultraLabel4":U.
        THIS-OBJECT:ultraLabel4:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel4:TabIndex = 0.
        THIS-OBJECT:ultraLabel4:Text = "Name Substitute:":U.
        /*  */
        /* ultraLabel3 */
        /*  */
        appearance9:FontData:BoldAsString = "True":U.
        THIS-OBJECT:ultraLabel3:Appearance = appearance9.
        THIS-OBJECT:ultraLabel3:Location = NEW System.Drawing.Point(6, 21).
        THIS-OBJECT:ultraLabel3:Name = "ultraLabel3":U.
        THIS-OBJECT:ultraLabel3:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel3:TabIndex = 5.
        THIS-OBJECT:ultraLabel3:Text = "Data-Relations":U.
        /*  */
        /* tb_beforesuffix */
        /*  */
        DEFINE VARIABLE nestedvar19 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar19 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_beforesuffix:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar19, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_beforesuffix:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tb_beforesuffix:Location = NEW System.Drawing.Point(115, 241).
        THIS-OBJECT:tb_beforesuffix:Name = "tb_beforesuffix":U.
        THIS-OBJECT:tb_beforesuffix:ShowOverflowIndicator = TRUE.
        THIS-OBJECT:tb_beforesuffix:Size = NEW System.Drawing.Size(341, 21).
        THIS-OBJECT:tb_beforesuffix:TabIndex = 9.
        THIS-OBJECT:tb_beforesuffix:Text = "ultraTextEditor5":U.
        /*  */
        /* tb_beforeprefix */
        /*  */
        DEFINE VARIABLE nestedvar20 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar20 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_beforeprefix:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar20, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_beforeprefix:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tb_beforeprefix:Location = NEW System.Drawing.Point(115, 214).
        THIS-OBJECT:tb_beforeprefix:Name = "tb_beforeprefix":U.
        THIS-OBJECT:tb_beforeprefix:ShowOverflowIndicator = TRUE.
        THIS-OBJECT:tb_beforeprefix:Size = NEW System.Drawing.Size(341, 21).
        THIS-OBJECT:tb_beforeprefix:TabIndex = 7.
        THIS-OBJECT:tb_beforeprefix:Text = "ultraTextEditor4":U.
        /*  */
        /* tb_namesuffix */
        /*  */
        DEFINE VARIABLE nestedvar21 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar21 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_namesuffix:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar21, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_namesuffix:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tb_namesuffix:Location = NEW System.Drawing.Point(115, 187).
        THIS-OBJECT:tb_namesuffix:Name = "tb_namesuffix":U.
        THIS-OBJECT:tb_namesuffix:ShowOverflowIndicator = TRUE.
        THIS-OBJECT:tb_namesuffix:Size = NEW System.Drawing.Size(341, 21).
        THIS-OBJECT:tb_namesuffix:TabIndex = 5.
        THIS-OBJECT:tb_namesuffix:Text = "ultraTextEditor3":U.
        /*  */
        /* tb_nameprefix */
        /*  */
        DEFINE VARIABLE nestedvar22 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar22 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_nameprefix:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar22, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_nameprefix:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tb_nameprefix:Location = NEW System.Drawing.Point(115, 160).
        THIS-OBJECT:tb_nameprefix:Name = "tb_nameprefix":U.
        THIS-OBJECT:tb_nameprefix:ShowOverflowIndicator = TRUE.
        THIS-OBJECT:tb_nameprefix:Size = NEW System.Drawing.Size(341, 21).
        THIS-OBJECT:tb_nameprefix:TabIndex = 3.
        THIS-OBJECT:tb_nameprefix:Text = "ultraTextEditor2":U.
        /*  */
        /* tb_datarelationnamesubstitute */
        /*  */
        DEFINE VARIABLE nestedvar23 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar23 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_datarelationnamesubstitute:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar23, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_datarelationnamesubstitute:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tb_datarelationnamesubstitute:Location = NEW System.Drawing.Point(156, 40).
        THIS-OBJECT:tb_datarelationnamesubstitute:Name = "tb_datarelationnamesubstitute":U.
        THIS-OBJECT:tb_datarelationnamesubstitute:ShowOverflowIndicator = TRUE.
        THIS-OBJECT:tb_datarelationnamesubstitute:Size = NEW System.Drawing.Size(300, 21).
        THIS-OBJECT:tb_datarelationnamesubstitute:TabIndex = 1.
        THIS-OBJECT:tb_datarelationnamesubstitute:Text = "ultraTextEditor1":U.
        /*  */
        /* gb_designer2 */
        /*  */
        THIS-OBJECT:gb_designer2:Controls:Add(THIS-OBJECT:cbSplitArrayFields).
        THIS-OBJECT:gb_designer2:Controls:Add(THIS-OBJECT:ultraLabel17).
        THIS-OBJECT:gb_designer2:Controls:Add(THIS-OBJECT:ultraLabel18).
        THIS-OBJECT:gb_designer2:Controls:Add(THIS-OBJECT:ultraLabel19).
        THIS-OBJECT:gb_designer2:Controls:Add(THIS-OBJECT:tb_fieldnamegenerator).
        THIS-OBJECT:gb_designer2:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:gb_designer2:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:gb_designer2:Name = "gb_designer2":U.
        THIS-OBJECT:gb_designer2:Size = NEW System.Drawing.Size(462, 374).
        THIS-OBJECT:gb_designer2:TabIndex = 20.
        THIS-OBJECT:gb_designer2:Text = "Design Settings, cont.":U.
        THIS-OBJECT:gb_designer2:ViewStyle = Infragistics.Win.Misc.GroupBoxViewStyle:Office2000.
        /*  */
        /* cbSplitArrayFields */
        /*  */
        DEFINE VARIABLE nestedvar24 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar24 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:cbSplitArrayFields:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar24, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:cbSplitArrayFields:AutoSize = TRUE.
        THIS-OBJECT:cbSplitArrayFields:Location = NEW System.Drawing.Point(114, 100).
        THIS-OBJECT:cbSplitArrayFields:Name = "cbSplitArrayFields":U.
        THIS-OBJECT:cbSplitArrayFields:Size = NEW System.Drawing.Size(209, 17).
        THIS-OBJECT:cbSplitArrayFields:TabIndex = 17.
        THIS-OBJECT:cbSplitArrayFields:Text = "Split Array Fields into seperate Fields":U.
        /*  */
        /* ultraLabel17 */
        /*  */
        THIS-OBJECT:ultraLabel17:Location = NEW System.Drawing.Point(114, 63).
        THIS-OBJECT:ultraLabel17:Name = "ultraLabel17":U.
        THIS-OBJECT:ultraLabel17:Size = NEW System.Drawing.Size(307, 23).
        THIS-OBJECT:ultraLabel17:TabIndex = 16.
        THIS-OBJECT:ultraLabel17:Text = "(restart business entity designer after making a change)":U.
        /*  */
        /* ultraLabel18 */
        /*  */
        appearance10:FontData:BoldAsString = "True":U.
        THIS-OBJECT:ultraLabel18:Appearance = appearance10.
        THIS-OBJECT:ultraLabel18:Location = NEW System.Drawing.Point(8, 20).
        THIS-OBJECT:ultraLabel18:Name = "ultraLabel18":U.
        THIS-OBJECT:ultraLabel18:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel18:TabIndex = 15.
        THIS-OBJECT:ultraLabel18:Text = "Fields":U.
        /*  */
        /* ultraLabel19 */
        /*  */
        THIS-OBJECT:ultraLabel19:Location = NEW System.Drawing.Point(8, 42).
        THIS-OBJECT:ultraLabel19:Name = "ultraLabel19":U.
        THIS-OBJECT:ultraLabel19:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel19:TabIndex = 13.
        THIS-OBJECT:ultraLabel19:Text = "Name Generator:":U.
        /*  */
        /* tb_fieldnamegenerator */
        /*  */
        DEFINE VARIABLE nestedvar25 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar25 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_fieldnamegenerator:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar25, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tb_fieldnamegenerator:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tb_fieldnamegenerator:Location = NEW System.Drawing.Point(114, 38).
        THIS-OBJECT:tb_fieldnamegenerator:Name = "tb_fieldnamegenerator":U.
        THIS-OBJECT:tb_fieldnamegenerator:ShowOverflowIndicator = TRUE.
        THIS-OBJECT:tb_fieldnamegenerator:Size = NEW System.Drawing.Size(341, 21).
        THIS-OBJECT:tb_fieldnamegenerator:TabIndex = 14.
        THIS-OBJECT:tb_fieldnamegenerator:Text = "ultraTextEditor2":U.
        /*  */
        /* ultraButton1 */
        /*  */
        DEFINE VARIABLE nestedvar26 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar26 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:ultraButton1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar26, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButton1:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:ultraButton1:Location = NEW System.Drawing.Point(449, 394).
        THIS-OBJECT:ultraButton1:Name = "ultraButton1":U.
        THIS-OBJECT:ultraButton1:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:ultraButton1:TabIndex = 3.
        THIS-OBJECT:ultraButton1:Text = "&OK":U.
        THIS-OBJECT:ultraButton1:Click:Subscribe(THIS-OBJECT:ultraButton1_Click).
        /*  */
        /* ultraButton2 */
        /*  */
        DEFINE VARIABLE nestedvar27 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar27 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:ultraButton2:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar27, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButton2:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:ultraButton2:Location = NEW System.Drawing.Point(530, 394).
        THIS-OBJECT:ultraButton2:Name = "ultraButton2":U.
        THIS-OBJECT:ultraButton2:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:ultraButton2:TabIndex = 4.
        THIS-OBJECT:ultraButton2:Text = "&Cancel":U.
        /*  */
        /* folderBrowserDialog1 */
        /*  */
        THIS-OBJECT:folderBrowserDialog1:Description = "Select the folder that contains the template files used by the generator.":U.
        THIS-OBJECT:folderBrowserDialog1:RootFolder = System.Environment+SpecialFolder:MyComputer.
        THIS-OBJECT:folderBrowserDialog1:ShowNewFolderButton = FALSE.
        /*  */
        /* openFileDialog1 */
        /*  */
        THIS-OBJECT:openFileDialog1:DefaultExt = "xml":U.
        THIS-OBJECT:openFileDialog1:Filter = "xml-Files|*.xml|All Files|*.*":U.
        THIS-OBJECT:openFileDialog1:RestoreDirectory = TRUE.
        THIS-OBJECT:openFileDialog1:Title = "Open Custom Services .xml Definition":U.
        /*  */
        /* ultraStatusBar1 */
        /*  */
        THIS-OBJECT:ultraStatusBar1:Location = NEW System.Drawing.Point(0, 423).
        THIS-OBJECT:ultraStatusBar1:Name = "ultraStatusBar1":U.
        THIS-OBJECT:ultraStatusBar1:Size = NEW System.Drawing.Size(617, 23).
        THIS-OBJECT:ultraStatusBar1:TabIndex = 5.
        THIS-OBJECT:ultraStatusBar1:ViewStyle = Infragistics.Win.UltraWinStatusBar.ViewStyle:Office2007.
        THIS-OBJECT:ultraStatusBar1:Click:Subscribe(THIS-OBJECT:ultraStatusBar1_Click).
        /*  */
        /* ultraLabel27 */
        /*  */
        THIS-OBJECT:ultraLabel27:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel27:Location = NEW System.Drawing.Point(297, 120).
        THIS-OBJECT:ultraLabel27:Name = "ultraLabel27":U.
        THIS-OBJECT:ultraLabel27:Size = NEW System.Drawing.Size(90, 14).
        THIS-OBJECT:ultraLabel27:TabIndex = 25.
        THIS-OBJECT:ultraLabel27:Text = "Keyword Casing:":U.
        /*  */
        /* coKeywordCasing */
        /*  */
        DEFINE VARIABLE nestedvar28 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar28 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:coKeywordCasing:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar28, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:coKeywordCasing:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:coKeywordCasing:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        valueListItem1:DataValue = "UPPER":U.
        valueListItem1:DisplayText = "UPPER case":U.
        valueListItem2:DataValue = "LOWER":U.
        valueListItem2:DisplayText = "lower case":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.ValueListItem EXTENT 2 NO-UNDO.
        arrayvar2[1] = valueListItem1.
        arrayvar2[2] = valueListItem2.
        THIS-OBJECT:coKeywordCasing:Items:AddRange(arrayvar2).
        THIS-OBJECT:coKeywordCasing:Location = NEW System.Drawing.Point(388, 116).
        THIS-OBJECT:coKeywordCasing:Name = "coKeywordCasing":U.
        THIS-OBJECT:coKeywordCasing:Size = NEW System.Drawing.Size(66, 21).
        THIS-OBJECT:coKeywordCasing:TabIndex = 26.
        /*  */
        /* BusinessEntityDesignerSettingsForm */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:ultraButton1.
        THIS-OBJECT:CancelButton = THIS-OBJECT:ultraButton2.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(617, 446).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraStatusBar1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButton1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButton2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraPanel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraExplorerBar1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:MaximizeBox = FALSE.
        THIS-OBJECT:MinimizeBox = FALSE.
        THIS-OBJECT:MinimumSize = NEW System.Drawing.Size(606, 445).
        THIS-OBJECT:Name = "BusinessEntityDesignerSettingsForm":U.
        THIS-OBJECT:ShowIcon = FALSE.
        THIS-OBJECT:ShowInTaskbar = FALSE.
        THIS-OBJECT:StartPosition = System.Windows.Forms.FormStartPosition:CenterParent.
        THIS-OBJECT:Text = "BusinessEntity Designer Settings":U.
        THIS-OBJECT:WindowPositionRegistryKey = "BusinessEntityDesignerSettingsForm":U.
        CAST(THIS-OBJECT:ultraExplorerBar1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraPanel1:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:ultraPanel1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:gb_generator, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:gb_generator:ResumeLayout(FALSE).
        THIS-OBJECT:gb_generator:PerformLayout().
        CAST(THIS-OBJECT:to_GenerateDatasetController, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:to_DistinctTempTableIncludes, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:to_PromptSave, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:to_CompileDatasetController, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tb_IncludeFiles, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tb_SourceFolder_Gui, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tb_RCodeFolder_Gui, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tb_SourceFolder, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tb_RCodeFolder, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:to_OpenFilesInIDE, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tb_generator, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tb_templatefolder, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:gb_launcher, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:gb_launcher:ResumeLayout(FALSE).
        THIS-OBJECT:gb_launcher:PerformLayout().
        CAST(THIS-OBJECT:toUseLocalFileHistory, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tb_launch_parameter, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:gb_plugins, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:gb_plugins:ResumeLayout(FALSE).
        THIS-OBJECT:gb_plugins:PerformLayout().
        CAST(THIS-OBJECT:tb_CustomServices, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tb_plugins, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:gb_designer, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:gb_designer:ResumeLayout(FALSE).
        THIS-OBJECT:gb_designer:PerformLayout().
        CAST(THIS-OBJECT:cbReplaceUnderscore, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:cbReplaceHyphens, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tb_RelationFieldsGenerator, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tb_tablenamegenerator, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tb_beforesuffix, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tb_beforeprefix, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tb_namesuffix, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tb_nameprefix, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tb_datarelationnamesubstitute, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:gb_designer2, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:gb_designer2:ResumeLayout(FALSE).
        THIS-OBJECT:gb_designer2:PerformLayout().
        CAST(THIS-OBJECT:cbSplitArrayFields, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tb_fieldnamegenerator, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraStatusBar1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:coKeywordCasing, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

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
        Purpose: Event handler for the EditorButtonClick  event of the tb_CustomServices
                 control
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID tb_CustomServices_EditorButtonClick (sender AS System.Object, 
	                                                         e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):
		
		DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult NO-UNDO . 
        DEFINE VARIABLE cFileName     AS CHARACTER                         NO-UNDO .
		
		IF THIS-OBJECT:tb_CustomServices:Text > "":U THEN DO: 
		    ASSIGN cFileName = Consultingwerk.Util.FileHelper:FindFile (THIS-OBJECT:tb_CustomServices:Text) . 
		    
		    IF cFileName > "":U THEN 
		        openFileDialog1:FileName = cFileName . 
		END.

        WAIT-FOR openFileDialog1:ShowDialog (THIS-OBJECT) SET oDialogResult .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult}

        ASSIGN cFileName = Consultingwerk.Util.FileHelper:RelativeFileName (openFileDialog1:FileName) .
        
        THIS-OBJECT:tb_CustomServices:Text = cFileName . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the ultraButton1                                                                        
        Notes:                                                        
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event                 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraButton1_Click (sender AS System.Object, 
                                            e AS System.EventArgs):
        
        BusinessEntityDesignerSettings:BeginChangeSettings() .

        ASSIGN 
            BusinessEntityDesignerSettings:BusinessEntityGenerator       = THIS-OBJECT:tb_generator:Text
            BusinessEntityDesignerSettings:FieldNameGeneratorService     = THIS-OBJECT:tb_fieldnamegenerator:Text
            BusinessEntityDesignerSettings:DataRelationFieldsService     = THIS-OBJECT:tb_RelationFieldsGenerator:Text
            BusinessEntityDesignerSettings:TemplateFolder                = THIS-OBJECT:tb_templatefolder:Text              
            BusinessEntityDesignerSettings:RelationNameSubstitute        = THIS-OBJECT:tb_datarelationnamesubstitute:Text  
            BusinessEntityDesignerSettings:DefaultTablePrefix            = THIS-OBJECT:tb_nameprefix:Text        
            BusinessEntityDesignerSettings:DefaultTableSuffix            = THIS-OBJECT:tb_namesuffix:Text
            BusinessEntityDesignerSettings:DefaultBeforeTablePrefix      = THIS-OBJECT:tb_beforeprefix:Text
            BusinessEntityDesignerSettings:DefaultBeforeTableSuffix      = THIS-OBJECT:tb_beforesuffix:Text 
            BusinessEntityDesignerSettings:BusinessEntityDesignerPlugins = REPLACE (THIS-OBJECT:tb_plugins:Text,
                                                                                    System.Environment:NewLine, 
                                                                                    "|":U)
            BusinessEntityDesignerSettings:OpenFilesInIDE                = THIS-OBJECT:to_OpenFilesInIDE:Checked 
            BusinessEntityDesignerSettings:SourceCodeBaseFolder          = THIS-OBJECT:tb_SourceFolder:Text
            BusinessEntityDesignerSettings:RCodeBaseFolder               = THIS-OBJECT:tb_RCodeFolder:Text
            
            BusinessEntityDesignerSettings:SourceCodeBaseFolderGui       = THIS-OBJECT:tb_SourceFolder_Gui:Text           
            BusinessEntityDesignerSettings:RCodeBaseFolderGui            = THIS-OBJECT:tb_RCodeFolder_Gui:Text            
            
            BusinessEntityDesignerSettings:IncludeFilesBaseFolder        = THIS-OBJECT:tb_IncludeFiles:Text               
            
            BusinessEntityDesignerSettings:TableNameGeneratorService     = THIS-OBJECT:tb_tablenamegenerator:Text
            BusinessEntityDesignerSettings:CompileDatasetController      = THIS-OBJECT:to_CompileDatasetController:Checked
            BusinessEntityDesignerSettings:GenerateDatasetController     = THIS-OBJECT:to_GenerateDatasetController:Checked  
            BusinessEntityDesignerSettings:PromptForSaveBeforeGenerate   = THIS-OBJECT:to_PromptSave:Checked
            BusinessEntityDesignerSettings:DistinctTempTableIncludeFiles = THIS-OBJECT:to_DistinctTempTableIncludes:Checked
            
            BusinessEntityDesignerSettings:ReplaceUnderScoreInTableName  = THIS-OBJECT:cbReplaceUnderscore:Checked          
            BusinessEntityDesignerSettings:ReplaceHyphenInTableName      = THIS-OBJECT:cbReplaceHyphens:Checked                         
            
            BusinessEntityDesignerSettings:SplitArrayFields              = THIS-OBJECT:cbSplitArrayFields:Checked
            
            BusinessEntityDesignerSettings:CustomServices                = THIS-OBJECT:tb_CustomServices:Text
            
            BusinessEntityDesignerSettings:LauncherCommandLine           = THIS-OBJECT:tb_launch_parameter:Text        
            
            BusinessEntityDesignerSettings:UseLocalFileHistory           = THIS-OBJECT:toUseLocalFileHistory:Checked     
            
            BusinessEntityDesignerSettings:KeywordCasing                 = UNBOX (THIS-OBJECT:coKeywordCasing:Value)
                   
              .
        
        BusinessEntityDesignerSettings:EndChangeSettings() .        

        BusinessEntityDesignerSettings:SaveSettings() . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the ultarButton3 instance
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraButton3_Click (sender AS System.Object,
	                                        e AS System.EventArgs):
		
        DEFINE VARIABLE oForm         AS Consultingwerk.SmartComponents.Support.BusinessEntityPickerForm NO-UNDO .
        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult                               NO-UNDO .
        DEFINE VARIABLE cClassName    AS CHARACTER                                                       NO-UNDO .
        DEFINE VARIABLE oType         AS System.Type                                                     NO-UNDO .
        DEFINE VARIABLE oInstance     AS Progress.Windows.UserControl                                    NO-UNDO .  
        DEFINE VARIABLE oContainer    AS System.Windows.Forms.Control                                    NO-UNDO .

        DEFINE VARIABLE oInfo         AS Infragistics.Win.Misc.UltraDesktopAlertWindowInfo               NO-UNDO . 

        oInfo = Consultingwerk.SmartComponents.Support.DesktopAlertHelper:Show ("Business Entity Designer"{&TRAN},
                                                                                "Searching Plugin Classes."{&TRAN}) .

        Consultingwerk.Util.ClassHelper:GetClassNamesInClassPath ("Consultingwerk.BusinessEntityDesigner.IBusinessEntityDesignerPlugin":U, 
                                                                  FALSE, 
                                                                  OUTPUT TABLE ttClassNames BY-REFERENCE) .

        oInfo:Close () . 

        oForm = NEW Consultingwerk.SmartComponents.Support.BusinessEntityPickerForm () .
        oForm:Text = "Select Plugin"{&TRAN} .
        
        oForm:ReceiveBusinessEntityNames (TABLE ttClassNames) .
        
        ASSIGN oDialogResult = Consultingwerk.SmartComponents.FormHelper:ShowDialog (oForm) .
        
        {Consultingwerk/check-dialogresult-ok.i oDialogResult}
        
        ASSIGN cClassName = oForm:GetSelectedBusinessEntityName () .

        IF cClassName > "":U THEN 
            ASSIGN THIS-OBJECT:tb_plugins:Text = TRIM (TRIM (THIS-OBJECT:tb_plugins:Text, CHR (10)), CHR (13)) + 
                                                 Consultingwerk.Environment:NewLine + 
                                                 cClassName .  

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ItemClick event of the ultraExplorerBar1                                                                        
        Notes:   Controls the view of the settings form                                                              
        @param sender The reference to the object that raised the event       
        @param e The ItemEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraExplorerBar1_ItemClick (sender AS System.Object, 
                                                     e AS Infragistics.Win.UltraWinExplorerBar.ItemEventArgs):
        
        CASE e:Item:Key:
            WHEN "generator":U THEN 
                ASSIGN gb_generator:VISIBLE = TRUE
                       gb_designer:VISIBLE  = FALSE 
                       gb_designer2:VISIBLE = FALSE
                       gb_plugins:VISIBLE   = FALSE 
                       gb_launcher:VISIBLE  = FALSE . 
                       
            WHEN "design":U THEN 
                ASSIGN gb_generator:VISIBLE = FALSE
                       gb_designer:VISIBLE  = TRUE
                       gb_designer2:VISIBLE = FALSE
                       gb_plugins:VISIBLE   = FALSE   
                       gb_launcher:VISIBLE  = FALSE .
                        
            WHEN "design2":U THEN 
                ASSIGN gb_generator:VISIBLE = FALSE
                       gb_designer:VISIBLE  = FALSE
                       gb_designer2:VISIBLE = TRUE
                       gb_plugins:VISIBLE   = FALSE  
                       gb_launcher:VISIBLE  = FALSE . 
                        
            WHEN "plugins":U THEN
                ASSIGN gb_generator:VISIBLE = FALSE
                       gb_designer:VISIBLE  = FALSE 
                       gb_designer2:VISIBLE = FALSE
                       gb_plugins:VISIBLE   = TRUE 
                       gb_launcher:VISIBLE  = FALSE .
                        
            WHEN "launcher":U THEN                        
                ASSIGN gb_generator:VISIBLE = FALSE
                       gb_designer:VISIBLE  = FALSE 
                       gb_designer2:VISIBLE = FALSE
                       gb_plugins:VISIBLE   = FALSE 
                       gb_launcher:VISIBLE  = TRUE . 
        END CASE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the UltraStatusBar
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraStatusBar1_Click (sender AS System.Object, 
	                                           e AS System.EventArgs):
		
		IF BusinessEntityDesignerSettings:SettingsFileLoaded THEN 
		     
		    Consultingwerk.Util.WindowsExplorerHelper:ShowFileInExplorer (BusinessEntityDesignerSettings:SettingsFile) .   

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the ultraTextEditor2
        Notes:                                                                        
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID tb_templatefolder_EditorButtonClick (sender AS System.Object, 
                                                             e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult NO-UNDO .
        
        FILE-INFO:FILE-NAME = Consultingwerk.BusinessEntityDesigner.BusinessEntityDesignerSettings:TemplateFolder .
        
        IF FILE-INFO:FULL-PATHNAME = ? THEN
            FILE-INFO:FILE-NAME = ".":U . 
        
        folderBrowserDialog1:SelectedPath = FILE-INFO:FULL-PATHNAME . 

        WAIT-FOR folderBrowserDialog1:ShowDialog() SET oDialogResult .

        IF Progress.Util.EnumHelper:AreEqual (oDialogResult, System.Windows.Forms.DialogResult:OK) THEN
            ASSIGN Consultingwerk.BusinessEntityDesigner.BusinessEntityDesignerSettings:TemplateFolder = folderBrowserDialog1:SelectedPath 
                   THIS-OBJECT:tb_templatefolder:Text = folderBrowserDialog1:SelectedPath .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the BusinessEntityDesignerSettingsForm  class                                                            
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC BusinessEntityDesignerSettingsForm ():

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
