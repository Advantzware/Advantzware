/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SmartValueListViewerControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 08.03.2013 15:47:25
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.System.SmartValueListViewerControl 
    INHERITS SmartViewerControl: 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE smartComboEditor1 AS Consultingwerk.SmartComponents.Implementation.SmartComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbModuleGuid_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbValueListKey_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbValueListKey AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbValueListDescription_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbValueListDescription AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartValueListViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartValueListViewerControl ():
                
        SUPER().
        InitializeComponent().
        
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
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartValueList":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:tbModuleGuid_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbValueListKey_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbValueListKey = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbValueListDescription_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbValueListDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartComboEditor1 = NEW Consultingwerk.SmartComponents.Implementation.SmartComboEditor().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbValueListKey, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbValueListDescription, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartComboEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:AddFields = FALSE.
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:ClearSchema = FALSE.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityJoin = "YES":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.System.ValueListBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartValueList":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:ImportSchema = FALSE.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 4 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("ValueListGuid":U, "GUID":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("ModuleGuid":U, "ModuleGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("ValueListKey":U, "Value List Key":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("ValueListDescription":U, "Description":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* tbModuleGuid_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbModuleGuid_Label:Appearance = appearance1.
        THIS-OBJECT:tbModuleGuid_Label:AutoSize = TRUE.
        THIS-OBJECT:tbModuleGuid_Label:Location = NEW System.Drawing.Point(4, 7).
        THIS-OBJECT:tbModuleGuid_Label:Name = "tbModuleGuid_Label":U.
        THIS-OBJECT:tbModuleGuid_Label:Size = NEW System.Drawing.Size(45, 14).
        THIS-OBJECT:tbModuleGuid_Label:TabIndex = 0.
        THIS-OBJECT:tbModuleGuid_Label:Text = "Module:":U.
        /*  */
        /* tbValueListKey_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbValueListKey_Label:Appearance = appearance2.
        THIS-OBJECT:tbValueListKey_Label:AutoSize = TRUE.
        THIS-OBJECT:tbValueListKey_Label:Location = NEW System.Drawing.Point(4, 33).
        THIS-OBJECT:tbValueListKey_Label:Name = "tbValueListKey_Label":U.
        THIS-OBJECT:tbValueListKey_Label:Size = NEW System.Drawing.Size(80, 14).
        THIS-OBJECT:tbValueListKey_Label:TabIndex = 2.
        THIS-OBJECT:tbValueListKey_Label:Text = "Value List Key:":U.
        /*  */
        /* tbValueListKey */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbValueListKey:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbValueListKey:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ValueListKey":U, TRUE)).
        THIS-OBJECT:tbValueListKey:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbValueListKey:Location = NEW System.Drawing.Point(114, 29).
        THIS-OBJECT:tbValueListKey:Name = "tbValueListKey":U.
        THIS-OBJECT:tbValueListKey:Size = NEW System.Drawing.Size(183, 21).
        THIS-OBJECT:tbValueListKey:TabIndex = 3.
        /*  */
        /* tbValueListDescription_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:tbValueListDescription_Label:Appearance = appearance3.
        THIS-OBJECT:tbValueListDescription_Label:AutoSize = TRUE.
        THIS-OBJECT:tbValueListDescription_Label:Location = NEW System.Drawing.Point(4, 59).
        THIS-OBJECT:tbValueListDescription_Label:Name = "tbValueListDescription_Label":U.
        THIS-OBJECT:tbValueListDescription_Label:Size = NEW System.Drawing.Size(64, 14).
        THIS-OBJECT:tbValueListDescription_Label:TabIndex = 4.
        THIS-OBJECT:tbValueListDescription_Label:Text = "Description:":U.
        /*  */
        /* tbValueListDescription */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbValueListDescription:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbValueListDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ValueListDescription":U, TRUE)).
        THIS-OBJECT:tbValueListDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbValueListDescription:Location = NEW System.Drawing.Point(114, 55).
        THIS-OBJECT:tbValueListDescription:Name = "tbValueListDescription":U.
        THIS-OBJECT:tbValueListDescription:Size = NEW System.Drawing.Size(183, 21).
        THIS-OBJECT:tbValueListDescription:TabIndex = 5.
        /*  */
        /* smartComboEditor1 */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartComboEditor1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartComboEditor1:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ModuleGuid":U, TRUE)).
        THIS-OBJECT:smartComboEditor1:DisplayFields = "ModuleName,ModuleDescription":U.
        THIS-OBJECT:smartComboEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartComboEditor1:DisplaySubstitute = "&1 - &2":U.
        THIS-OBJECT:smartComboEditor1:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:smartComboEditor1:EntityName = "Consultingwerk.SmartFramework.Menu.ModuleBusinessEntity":U.
        THIS-OBJECT:smartComboEditor1:EntityTable = "eSmartModule":U.
        THIS-OBJECT:smartComboEditor1:KeyField = "ModuleGuid":U.
        THIS-OBJECT:smartComboEditor1:LimitToList = TRUE.
        THIS-OBJECT:smartComboEditor1:Location = NEW System.Drawing.Point(114, 3).
        THIS-OBJECT:smartComboEditor1:Name = "smartComboEditor1":U.
        THIS-OBJECT:smartComboEditor1:Size = NEW System.Drawing.Size(183, 21).
        THIS-OBJECT:smartComboEditor1:TabIndex = 1.
        /*  */
        /* SmartValueListViewerControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartComboEditor1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbModuleGuid_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbValueListKey_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbValueListKey).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbValueListDescription_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbValueListDescription).
        THIS-OBJECT:Name = "SmartValueListViewerControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(300, 143).
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbValueListKey, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbValueListDescription, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartComboEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    DESTRUCTOR PUBLIC SmartValueListViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
