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
    File        : SmartModuleViewer
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Thu Oct 11 17:32:37 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.SmartViewerControl FROM PROPATH .
USING Consultingwerk.Windows.Framework.Menu.*                FROM PROPATH .  
USING Progress.Lang.*                                        FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Menu.SmartModuleViewer 
    INHERITS SmartViewerControl: 
    
    DEFINE PRIVATE VARIABLE components                                               AS System.ComponentModel.IContainer                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartModule_ModulePackage_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartModule_ModulePackage AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartModule_ModuleDataFolder_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartModule_ModuleDataFolder AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartComboEditor1 AS Consultingwerk.SmartComponents.Implementation.SmartComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraPanel1                                              AS Infragistics.Win.Misc.UltraPanel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE ModuleBindingSource_eSmartModule_ModuleName_Label        AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE ModuleBindingSource_eSmartModule_ModuleName              AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE ModuleBindingSource_eSmartModule_ModuleDescription_Label AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE ModuleBindingSource_eSmartModule_ModuleDescription       AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE ModuleBindingSource                                      AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.

    CONSTRUCTOR PUBLIC SmartModuleViewer ():
        
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
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartModule":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Menu.SmartModuleViewer":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:ModuleBindingSource = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraPanel1 = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartComboEditor1 = NEW Consultingwerk.SmartComponents.Implementation.SmartComboEditor().
        THIS-OBJECT:eSmartModule_ModuleDataFolder_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartModule_ModuleDataFolder = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartModule_ModulePackage_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartModule_ModulePackage = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        CAST(THIS-OBJECT:ModuleBindingSource, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraPanel1:ClientArea:SuspendLayout().
        THIS-OBJECT:ultraPanel1:SuspendLayout().
        CAST(THIS-OBJECT:smartComboEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartModule_ModuleDataFolder, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartModule_ModulePackage, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ModuleBindingSource */
        /*  */
        THIS-OBJECT:ModuleBindingSource:BindTo = "QUERY":U.
        THIS-OBJECT:ModuleBindingSource:EntityName = "Consultingwerk.SmartFramework.Menu.ModuleBusinessEntity":U.
        THIS-OBJECT:ModuleBindingSource:EntityTable = "eSmartModule":U.
        THIS-OBJECT:ModuleBindingSource:MaxDataGuess = 0.
        THIS-OBJECT:ModuleBindingSource:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 7 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("ModuleGuid":U, "ModuleGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("ModuleName":U, "ModuleName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("ModuleDescription":U, "ModuleDescription":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("ProductGuid":U, "ProductGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("ProductCode":U, "Code":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("ModuleDataFolder":U, "Data Folder":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[7] = NEW Progress.Data.ColumnPropDesc("ModulePackage":U, "Package Name":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:ModuleBindingSource:TableSchema = tableDesc1.
        /*  */
        /* ModuleBindingSource_eSmartModule_ModuleName_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName_Label:Appearance = appearance1.
        resources:ApplyResources(THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName_Label, "ModuleBindingSource_eSmartModule_ModuleName_Label":U).
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName_Label:Name = "ModuleBindingSource_eSmartModule_ModuleName_Label":U.
        /*  */
        /* ModuleBindingSource_eSmartModule_ModuleName */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName, "ModuleBindingSource_eSmartModule_ModuleName":U).
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:ModuleBindingSource, "ModuleName":U, TRUE)).
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName:Name = "ModuleBindingSource_eSmartModule_ModuleName":U.
        /*  */
        /* ModuleBindingSource_eSmartModule_ModuleDescription_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription_Label:Appearance = appearance2.
        resources:ApplyResources(THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription_Label, "ModuleBindingSource_eSmartModule_ModuleDescription_Label":U).
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription_Label:Name = "ModuleBindingSource_eSmartModule_ModuleDescription_Label":U.
        /*  */
        /* ModuleBindingSource_eSmartModule_ModuleDescription */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription, "ModuleBindingSource_eSmartModule_ModuleDescription":U).
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:ModuleBindingSource, "ModuleDescription":U, TRUE)).
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription:Name = "ModuleBindingSource_eSmartModule_ModuleDescription":U.
        /*  */
        /* ultraPanel1 */
        /*  */
        /*  */
        /* ultraPanel1.ClientArea */
        /*  */
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:eSmartModule_ModuleDataFolder_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:smartComboEditor1).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:eSmartModule_ModuleDataFolder).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:eSmartModule_ModulePackage_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:eSmartModule_ModulePackage).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription_Label).
        resources:ApplyResources(THIS-OBJECT:ultraPanel1, "ultraPanel1":U).
        THIS-OBJECT:ultraPanel1:Name = "ultraPanel1":U.
        /*  */
        /* ultraLabel1 */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel1:Appearance = appearance3.
        resources:ApplyResources(THIS-OBJECT:ultraLabel1, "ultraLabel1":U).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        /*  */
        /* smartComboEditor1 */
        /*  */
        THIS-OBJECT:smartComboEditor1:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:ModuleBindingSource, "ProductGuid":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:smartComboEditor1, "smartComboEditor1":U).
        THIS-OBJECT:smartComboEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartComboEditor1:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:smartComboEditor1:LimitToList = TRUE.
        THIS-OBJECT:smartComboEditor1:Name = "smartComboEditor1":U.
        /*  */
        /* eSmartModule_ModuleDataFolder_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartModule_ModuleDataFolder_Label:Appearance = appearance4.
        resources:ApplyResources(THIS-OBJECT:eSmartModule_ModuleDataFolder_Label, "eSmartModule_ModuleDataFolder_Label":U).
        THIS-OBJECT:eSmartModule_ModuleDataFolder_Label:Name = "eSmartModule_ModuleDataFolder_Label":U.
        /*  */
        /* eSmartModule_ModuleDataFolder */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartModule_ModuleDataFolder, "eSmartModule_ModuleDataFolder":U).
        THIS-OBJECT:eSmartModule_ModuleDataFolder:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:ModuleBindingSource, "ModuleDataFolder":U, TRUE)).
        THIS-OBJECT:eSmartModule_ModuleDataFolder:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartModule_ModuleDataFolder:Name = "eSmartModule_ModuleDataFolder":U.
        /*  */
        /* eSmartModule_ModulePackage_Label */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartModule_ModulePackage_Label:Appearance = appearance5.
        resources:ApplyResources(THIS-OBJECT:eSmartModule_ModulePackage_Label, "eSmartModule_ModulePackage_Label":U).
        THIS-OBJECT:eSmartModule_ModulePackage_Label:Name = "eSmartModule_ModulePackage_Label":U.
        /*  */
        /* eSmartModule_ModulePackage */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartModule_ModulePackage, "eSmartModule_ModulePackage":U).
        THIS-OBJECT:eSmartModule_ModulePackage:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:ModuleBindingSource, "ModulePackage":U, TRUE)).
        THIS-OBJECT:eSmartModule_ModulePackage:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartModule_ModulePackage:Name = "eSmartModule_ModulePackage":U.
        /*  */
        /* SmartModuleViewer */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraPanel1).
        THIS-OBJECT:Name = "SmartModuleViewer":U.
        CAST(THIS-OBJECT:ModuleBindingSource, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraPanel1:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:ultraPanel1:ClientArea:PerformLayout().
        THIS-OBJECT:ultraPanel1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartComboEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartModule_ModuleDataFolder, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartModule_ModulePackage, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

END CLASS.
