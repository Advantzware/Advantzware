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

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Visual Design                                                                        
        Notes:      
    ------------------------------------------------------------------------------*/
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
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:ModuleBindingSource = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraPanel1 = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:smartComboEditor1 = NEW Consultingwerk.SmartComponents.Implementation.SmartComboEditor().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        CAST(THIS-OBJECT:ModuleBindingSource, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraPanel1:ClientArea:SuspendLayout().
        THIS-OBJECT:ultraPanel1:SuspendLayout().
        CAST(THIS-OBJECT:smartComboEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ModuleBindingSource */
        /*  */
        THIS-OBJECT:ModuleBindingSource:AddFields = FALSE.
        THIS-OBJECT:ModuleBindingSource:BindTo = "QUERY":U.
        THIS-OBJECT:ModuleBindingSource:ClearSchema = FALSE.
        THIS-OBJECT:ModuleBindingSource:EntityName = "Consultingwerk.SmartFramework.Menu.ModuleBusinessEntity":U.
        THIS-OBJECT:ModuleBindingSource:EntityTable = "eSmartModule":U.
        THIS-OBJECT:ModuleBindingSource:ImportSchema = FALSE.
        THIS-OBJECT:ModuleBindingSource:MaxDataGuess = 0.
        THIS-OBJECT:ModuleBindingSource:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 5 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("ModuleGuid":U, "ModuleGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("ModuleName":U, "ModuleName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("ModuleDescription":U, "ModuleDescription":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("ProductGuid":U, "ProductGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("ProductCode":U, "Code":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:ModuleBindingSource:TableSchema = tableDesc1.
        /*  */
        /* ModuleBindingSource_eSmartModule_ModuleName_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName_Label:Appearance = appearance1.
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName_Label:AutoSize = TRUE.
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName_Label:Location = NEW System.Drawing.Point(12, 34).
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName_Label:Name = "ModuleBindingSource_eSmartModule_ModuleName_Label":U.
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName_Label:Size = NEW System.Drawing.Size(38, 14).
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName_Label:TabIndex = 2.
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName_Label:Text = "&Name:":U.
        /*  */
        /* ModuleBindingSource_eSmartModule_ModuleName */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:ModuleBindingSource, "ModuleName":U, TRUE)).
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName:Location = NEW System.Drawing.Point(122, 30).
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName:Name = "ModuleBindingSource_eSmartModule_ModuleName":U.
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName:Size = NEW System.Drawing.Size(337, 21).
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName:TabIndex = 3.
        /*  */
        /* ModuleBindingSource_eSmartModule_ModuleDescription_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription_Label:Appearance = appearance2.
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription_Label:AutoSize = TRUE.
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription_Label:Location = NEW System.Drawing.Point(12, 59).
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription_Label:Name = "ModuleBindingSource_eSmartModule_ModuleDescription_Label":U.
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription_Label:Size = NEW System.Drawing.Size(64, 14).
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription_Label:TabIndex = 4.
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription_Label:Text = "&Description:":U.
        /*  */
        /* ModuleBindingSource_eSmartModule_ModuleDescription */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:ModuleBindingSource, "ModuleDescription":U, TRUE)).
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription:Location = NEW System.Drawing.Point(122, 57).
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription:Multiline = TRUE.
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription:Name = "ModuleBindingSource_eSmartModule_ModuleDescription":U.
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription:Size = NEW System.Drawing.Size(337, 218).
        THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription:TabIndex = 5.
        /*  */
        /* ultraPanel1 */
        /*  */
        /*  */
        /* ultraPanel1.ClientArea */
        /*  */
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:smartComboEditor1).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription_Label).
        THIS-OBJECT:ultraPanel1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraPanel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraPanel1:Name = "ultraPanel1":U.
        THIS-OBJECT:ultraPanel1:Size = NEW System.Drawing.Size(472, 278).
        THIS-OBJECT:ultraPanel1:TabIndex = 5.
        /*  */
        /* smartComboEditor1 */
        /*  */
        THIS-OBJECT:smartComboEditor1:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:ModuleBindingSource, "ProductGuid":U, TRUE)).
        THIS-OBJECT:smartComboEditor1:DisplayFields = "ProductCode":U.
        THIS-OBJECT:smartComboEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartComboEditor1:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:smartComboEditor1:EntityName = "Consultingwerk.SmartFramework.Menu.ProductBusinessEntity":U.
        THIS-OBJECT:smartComboEditor1:EntityTable = "eSmartProduct":U.
        THIS-OBJECT:smartComboEditor1:KeyField = "ProductGuid":U.
        THIS-OBJECT:smartComboEditor1:LimitToList = TRUE.
        THIS-OBJECT:smartComboEditor1:Location = NEW System.Drawing.Point(122, 3).
        THIS-OBJECT:smartComboEditor1:Name = "smartComboEditor1":U.
        THIS-OBJECT:smartComboEditor1:Size = NEW System.Drawing.Size(337, 21).
        THIS-OBJECT:smartComboEditor1:TabIndex = 1.
        /*  */
        /* ultraLabel1 */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel1:Appearance = appearance3.
        THIS-OBJECT:ultraLabel1:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(12, 7).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(46, 14).
        THIS-OBJECT:ultraLabel1:TabIndex = 0.
        THIS-OBJECT:ultraLabel1:Text = "&Product:":U.
        /*  */
        /* SmartModuleViewer */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraPanel1).
        THIS-OBJECT:Name = "SmartModuleViewer":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(472, 278).
        CAST(THIS-OBJECT:ModuleBindingSource, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ModuleBindingSource_eSmartModule_ModuleDescription, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraPanel1:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:ultraPanel1:ClientArea:PerformLayout().
        THIS-OBJECT:ultraPanel1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartComboEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

END CLASS.
