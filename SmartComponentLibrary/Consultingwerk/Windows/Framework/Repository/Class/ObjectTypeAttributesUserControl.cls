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
    File        : ObjectTypeAttributesUserControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Oct 20 21:26:43 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.*                          FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*               FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.*    FROM PROPATH .
USING Consultingwerk.Util.*                               FROM PROPATH .
USING Consultingwerk.Windows.Framework.Repository.Class.* FROM PROPATH .  
USING Infragistics.Win.UltraWinGrid.*                     FROM ASSEMBLY .
USING Progress.Lang.*                                     FROM PROPATH .
USING Infragistics.Win.* FROM ASSEMBLY.
USING Consultingwerk.Windows.Util.Forms.* FROM PROPATH.

CLASS Consultingwerk.Windows.Framework.Repository.Class.ObjectTypeAttributesUserControl     
    INHERITS SmartUserControl: 
	
    DEFINE PRIVATE VARIABLE bindingSource1 AS Progress.Data.BindingSource NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE smartTempTableAdapter1 AS Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor1 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartUpdatableBrowser1 AS Consultingwerk.SmartComponents.Implementation.SmartUpdatableBrowser NO-UNDO.
    DEFINE PRIVATE VARIABLE smartPanelController1 AS Consultingwerk.SmartComponents.Implementation.SmartPanelController NO-UNDO.
	
    { Consultingwerk/SmartFramework/Repository/Class/dsDesignAttributeValue.i }
	
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ObjectTypeAttributesUserControl class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC ObjectTypeAttributesUserControl ():
		
		DEFINE VARIABLE oValueList AS ValueList NO-UNDO . 
		
        SUPER().

        InitializeComponent().

        smartTempTableAdapter1:TempTableHandle = TEMP-TABLE eDesignAttributeValue:HANDLE . 
        smartTempTableAdapter1:RetrieveData () . 

        oValueList = NEW ValueList () . 
        oValueList:ValueListItems:Add (BOX (TRUE), "Property":U) .
        oValueList:ValueListItems:Add (BOX (FALSE), "Event":U) .
        
        smartUpdatableBrowser1:DisplayLayout:Bands[0]:Columns["PropertyOrEvent":U]:ValueList = oValueList .
        
        IF VALID-OBJECT (smartTempTableAdapter1:BindingSource) THEN 
            ultraTextEditor1:DataBindings:Add (NEW System.Windows.Forms.Binding ("Value":U,
                                                                                 smartTempTableAdapter1:BindingSource, 
                                                                                 "AttributeDesription":U, 
                                                                                 TRUE)).

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
        tableDesc1 = NEW Progress.Data.TableDesc("eDesignSmartAttributeValue":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Repository.Class.ObjectTypeAttributesUserControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("eDesignSmartAttributeValue":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("AttributeValueGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn2 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn2 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ObjectTypeGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn3 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn3 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ObjectMasterGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn4 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn4 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ContainerObjectMasterGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn5 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn5 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ObjectInstanceGuid":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn6 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn6 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ConstantValue":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn7 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn7 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("AttributeLabel":U, -1, ?, 0, Infragistics.Win.UltraWinGrid.SortIndicator:Ascending, FALSE).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn8 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn8 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("CharacterValue":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn9 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn9 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("IntegerValue":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn10 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn10 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("Int64Value":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn11 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn11 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("DateValue":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn12 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn12 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("DateTimeValue":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn13 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn13 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("DateTimeTzValue":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn14 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn14 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("DecimalValue":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn15 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn15 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("LogicalValue":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn17 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn17 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("AppliesAtRuntime":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn18 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn18 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("IsInherited":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn19 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn19 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("InheritedFrom":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn20 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn20 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TechnicalName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn21 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn21 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("AttributeDesription":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn22 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn22 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("LookupType":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn23 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn23 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("LookupValues":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn24 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn24 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("PropertyType":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn25 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn25 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("RepositoryType":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn26 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn26 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("PropertyOrEvent":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn16 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn16 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("AttributeValue":U, 0).
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
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
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
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
        appearance12 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:bindingSource1 = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
        THIS-OBJECT:smartPanelController1 = NEW Consultingwerk.SmartComponents.Implementation.SmartPanelController().
        THIS-OBJECT:ultraTextEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartUpdatableBrowser1 = NEW Consultingwerk.SmartComponents.Implementation.SmartUpdatableBrowser().
        THIS-OBJECT:smartTempTableAdapter1 = NEW Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter(THIS-OBJECT:components).
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartUpdatableBrowser1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartTempTableAdapter1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* bindingSource1 */
        /*  */
        THIS-OBJECT:bindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:bindingSource1:NoLOBs = FALSE.
        THIS-OBJECT:bindingSource1:Position = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS "Progress.Data.TableDesc[]" NO-UNDO.
        arrayvar0 = NEW "Progress.Data.TableDesc[]"(0).
        tableDesc1:ChildTables = arrayvar0.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Progress.Data.ColumnPropDesc EXTENT 25 NO-UNDO.
        arrayvar1[1] = NEW Progress.Data.ColumnPropDesc("AttributeValueGuid":U, "AttributeValueGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar1[2] = NEW Progress.Data.ColumnPropDesc("ObjectTypeGuid":U, "ObjectTypeGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar1[3] = NEW Progress.Data.ColumnPropDesc("ObjectMasterGuid":U, "ObjectMasterGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar1[4] = NEW Progress.Data.ColumnPropDesc("ContainerObjectMasterGuid":U, "ContainerObjectMasterGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar1[5] = NEW Progress.Data.ColumnPropDesc("ObjectInstanceGuid":U, "ObjectInstanceGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar1[6] = NEW Progress.Data.ColumnPropDesc("ConstantValue":U, "Constant Value":U, Progress.Data.DataType:LOGICAL).
        arrayvar1[7] = NEW Progress.Data.ColumnPropDesc("AttributeLabel":U, "Attribute Label":U, Progress.Data.DataType:CHARACTER).
        arrayvar1[8] = NEW Progress.Data.ColumnPropDesc("CharacterValue":U, "Character Value":U, Progress.Data.DataType:CHARACTER).
        arrayvar1[9] = NEW Progress.Data.ColumnPropDesc("IntegerValue":U, "Integer Value":U, Progress.Data.DataType:INTEGER).
        arrayvar1[10] = NEW Progress.Data.ColumnPropDesc("Int64Value":U, "Int64 value":U, Progress.Data.DataType:INT64).
        arrayvar1[11] = NEW Progress.Data.ColumnPropDesc("DateValue":U, "Date Value":U, Progress.Data.DataType:DATE).
        arrayvar1[12] = NEW Progress.Data.ColumnPropDesc("DateTimeValue":U, "DateTime Value":U, Progress.Data.DataType:DATETIME).
        arrayvar1[13] = NEW Progress.Data.ColumnPropDesc("DateTimeTzValue":U, "DateTime-TZ Value":U, Progress.Data.DataType:DATETIME_TZ).
        arrayvar1[14] = NEW Progress.Data.ColumnPropDesc("DecimalValue":U, "Decimal Value":U, Progress.Data.DataType:DECIMAL).
        arrayvar1[15] = NEW Progress.Data.ColumnPropDesc("LogicalValue":U, "Logical Value":U, Progress.Data.DataType:LOGICAL).
        arrayvar1[16] = NEW Progress.Data.ColumnPropDesc("AppliesAtRuntime":U, "AppliesAtRuntime":U, Progress.Data.DataType:LOGICAL).
        arrayvar1[17] = NEW Progress.Data.ColumnPropDesc("IsInherited":U, "":U, Progress.Data.DataType:LOGICAL).
        arrayvar1[18] = NEW Progress.Data.ColumnPropDesc("InheritedFrom":U, "":U, Progress.Data.DataType:CHARACTER).
        arrayvar1[19] = NEW Progress.Data.ColumnPropDesc("TechnicalName":U, "Technical Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar1[20] = NEW Progress.Data.ColumnPropDesc("AttributeDesription":U, "Description":U, Progress.Data.DataType:CHARACTER).
        arrayvar1[21] = NEW Progress.Data.ColumnPropDesc("LookupType":U, "Lookup Type":U, Progress.Data.DataType:CHARACTER).
        arrayvar1[22] = NEW Progress.Data.ColumnPropDesc("LookupValues":U, "Lookup Values":U, Progress.Data.DataType:CHARACTER).
        arrayvar1[23] = NEW Progress.Data.ColumnPropDesc("PropertyType":U, "Property Type":U, Progress.Data.DataType:CHARACTER).
        arrayvar1[24] = NEW Progress.Data.ColumnPropDesc("RepositoryType":U, "Repository Type":U, Progress.Data.DataType:CHARACTER).
        arrayvar1[25] = NEW Progress.Data.ColumnPropDesc("PropertyOrEvent":U, "PropertyOrEvent":U, Progress.Data.DataType:LOGICAL).
        tableDesc1:Columns = arrayvar1.
        THIS-OBJECT:bindingSource1:TableSchema = tableDesc1.
        /*  */
        /* smartPanelController1 */
        /*  */
        THIS-OBJECT:smartPanelController1:Dock = System.Windows.Forms.DockStyle:Top.
        THIS-OBJECT:smartPanelController1:Location = NEW System.Drawing.Point(5, 5).
        THIS-OBJECT:smartPanelController1:MaximumSize = NEW System.Drawing.Size(5000, 27).
        THIS-OBJECT:smartPanelController1:MinimumSize = NEW System.Drawing.Size(100, 27).
        THIS-OBJECT:smartPanelController1:Name = "smartPanelController1":U.
        THIS-OBJECT:smartPanelController1:Size = NEW System.Drawing.Size(571, 27).
        THIS-OBJECT:smartPanelController1:TabIndex = 0.
        /*  */
        /* ultraTextEditor1 */
        /*  */
        THIS-OBJECT:ultraTextEditor1:AlwaysInEditMode = TRUE.
        THIS-OBJECT:ultraTextEditor1:Dock = System.Windows.Forms.DockStyle:Bottom.
        THIS-OBJECT:ultraTextEditor1:Location = NEW System.Drawing.Point(5, 321).
        THIS-OBJECT:ultraTextEditor1:Multiline = TRUE.
        THIS-OBJECT:ultraTextEditor1:Name = "ultraTextEditor1":U.
        THIS-OBJECT:ultraTextEditor1:ReadOnly = TRUE.
        THIS-OBJECT:ultraTextEditor1:Scrollbars = System.Windows.Forms.ScrollBars:Vertical.
        THIS-OBJECT:ultraTextEditor1:Size = NEW System.Drawing.Size(571, 59).
        THIS-OBJECT:ultraTextEditor1:TabIndex = 1.
        /*  */
        /* smartUpdatableBrowser1 */
        /*  */
        THIS-OBJECT:smartUpdatableBrowser1:DataSource = THIS-OBJECT:bindingSource1.
        THIS-OBJECT:smartUpdatableBrowser1:DisabledColumnsForAdd = resources:GetString("smartUpdatableBrowser1.DisabledColumnsForAdd":U).
        THIS-OBJECT:smartUpdatableBrowser1:DisabledColumnsForUpdate = resources:GetString("smartUpdatableBrowser1.DisabledColumnsForUpdate":U).
        appearance1:BackColor = System.Drawing.SystemColors:Window.
        appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Appearance = appearance1.
        ultraGridColumn1:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn1:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn1:Header:VisiblePosition = 0.
        ultraGridColumn1:Hidden = TRUE.
        ultraGridColumn2:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn2:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn2:Header:VisiblePosition = 1.
        ultraGridColumn2:Hidden = TRUE.
        ultraGridColumn3:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn3:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn3:Header:VisiblePosition = 2.
        ultraGridColumn3:Hidden = TRUE.
        ultraGridColumn4:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn4:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn4:Header:VisiblePosition = 3.
        ultraGridColumn4:Hidden = TRUE.
        ultraGridColumn5:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn5:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn5:Header:VisiblePosition = 4.
        ultraGridColumn5:Hidden = TRUE.
        ultraGridColumn6:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn6:Header:VisiblePosition = 9.
        ultraGridColumn7:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn7:Header:VisiblePosition = 5.
        ultraGridColumn8:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn8:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn8:Header:VisiblePosition = 10.
        ultraGridColumn8:Hidden = TRUE.
        ultraGridColumn9:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn9:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn9:FilterOperatorDefaultValue = Infragistics.Win.UltraWinGrid.FilterOperatorDefaultValue:GreaterThanOrEqualTo.
        ultraGridColumn9:Header:VisiblePosition = 11.
        ultraGridColumn9:Hidden = TRUE.
        ultraGridColumn10:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn10:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn10:FilterOperatorDefaultValue = Infragistics.Win.UltraWinGrid.FilterOperatorDefaultValue:GreaterThanOrEqualTo.
        ultraGridColumn10:Header:VisiblePosition = 12.
        ultraGridColumn10:Hidden = TRUE.
        ultraGridColumn11:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn11:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn11:FilterOperatorDefaultValue = Infragistics.Win.UltraWinGrid.FilterOperatorDefaultValue:GreaterThanOrEqualTo.
        ultraGridColumn11:Header:VisiblePosition = 13.
        ultraGridColumn11:Hidden = TRUE.
        ultraGridColumn12:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn12:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn12:FilterOperatorDefaultValue = Infragistics.Win.UltraWinGrid.FilterOperatorDefaultValue:GreaterThanOrEqualTo.
        ultraGridColumn12:Header:VisiblePosition = 14.
        ultraGridColumn12:Hidden = TRUE.
        ultraGridColumn13:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn13:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn13:FilterOperatorDefaultValue = Infragistics.Win.UltraWinGrid.FilterOperatorDefaultValue:GreaterThanOrEqualTo.
        ultraGridColumn13:Header:VisiblePosition = 15.
        ultraGridColumn13:Hidden = TRUE.
        ultraGridColumn14:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn14:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn14:FilterOperatorDefaultValue = Infragistics.Win.UltraWinGrid.FilterOperatorDefaultValue:GreaterThanOrEqualTo.
        ultraGridColumn14:Header:VisiblePosition = 16.
        ultraGridColumn14:Hidden = TRUE.
        ultraGridColumn15:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn15:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn15:Header:VisiblePosition = 17.
        ultraGridColumn15:Hidden = TRUE.
        ultraGridColumn17:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn17:Header:Caption = "Applies At Runtime":U.
        ultraGridColumn17:Header:VisiblePosition = 18.
        ultraGridColumn18:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn18:Header:Caption = "Inherited":U.
        ultraGridColumn18:Header:VisiblePosition = 7.
        ultraGridColumn19:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn19:Header:Caption = "Inherited From":U.
        ultraGridColumn19:Header:VisiblePosition = 8.
        ultraGridColumn20:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn20:Header:VisiblePosition = 19.
        ultraGridColumn21:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn21:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn21:Header:VisiblePosition = 20.
        ultraGridColumn21:Hidden = TRUE.
        ultraGridColumn22:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn22:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn22:Header:VisiblePosition = 21.
        ultraGridColumn22:Hidden = TRUE.
        ultraGridColumn23:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn23:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True.
        ultraGridColumn23:Header:VisiblePosition = 22.
        ultraGridColumn23:Hidden = TRUE.
        ultraGridColumn24:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn24:Header:VisiblePosition = 23.
        ultraGridColumn25:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn25:Header:VisiblePosition = 24.
        ultraGridColumn26:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn26:Header:Caption = "Property Or Event":U.
        ultraGridColumn26:Header:VisiblePosition = 25.
        ultraGridColumn16:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn16:Header:Caption = "Attribute Value":U.
        ultraGridColumn16:Header:VisiblePosition = 6.
        ultraGridColumn16:Width = 211.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS System.Object EXTENT 26 NO-UNDO.
        arrayvar2[1] = ultraGridColumn1.
        arrayvar2[2] = ultraGridColumn2.
        arrayvar2[3] = ultraGridColumn3.
        arrayvar2[4] = ultraGridColumn4.
        arrayvar2[5] = ultraGridColumn5.
        arrayvar2[6] = ultraGridColumn6.
        arrayvar2[7] = ultraGridColumn7.
        arrayvar2[8] = ultraGridColumn8.
        arrayvar2[9] = ultraGridColumn9.
        arrayvar2[10] = ultraGridColumn10.
        arrayvar2[11] = ultraGridColumn11.
        arrayvar2[12] = ultraGridColumn12.
        arrayvar2[13] = ultraGridColumn13.
        arrayvar2[14] = ultraGridColumn14.
        arrayvar2[15] = ultraGridColumn15.
        arrayvar2[16] = ultraGridColumn17.
        arrayvar2[17] = ultraGridColumn18.
        arrayvar2[18] = ultraGridColumn19.
        arrayvar2[19] = ultraGridColumn20.
        arrayvar2[20] = ultraGridColumn21.
        arrayvar2[21] = ultraGridColumn22.
        arrayvar2[22] = ultraGridColumn23.
        arrayvar2[23] = ultraGridColumn24.
        arrayvar2[24] = ultraGridColumn25.
        arrayvar2[25] = ultraGridColumn26.
        arrayvar2[26] = ultraGridColumn16.
        ultraGridBand1:Columns:AddRange(arrayvar2).
        ultraGridBand1:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        ultraGridBand1:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:BandsSerializer:Add(ultraGridBand1).
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance2:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:Appearance = appearance2.
        appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance4:BackColor2 = System.Drawing.SystemColors:Control.
        appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:MaxRowScrollRegions = 1.
        appearance5:BackColor = System.Drawing.SystemColors:Window.
        appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:ActiveCellAppearance = appearance5.
        appearance6:BackColor = System.Drawing.SystemColors:Highlight.
        appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:ActiveRowAppearance = appearance6.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance7:BackColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CardAreaAppearance = appearance7.
        appearance8:BorderColor = System.Drawing.Color:Silver.
        appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CellAppearance = appearance8.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CellPadding = 0.
        appearance9:BackColor = System.Drawing.SystemColors:Control.
        appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance9:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:GroupByRowAppearance = appearance9.
        appearance10:TextHAlignAsString = "Left":U.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:HeaderAppearance = appearance10.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortMulti.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance11:BackColor = System.Drawing.SystemColors:Window.
        appearance11:BorderColor = System.Drawing.Color:Silver.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:RowAppearance = appearance11.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
        THIS-OBJECT:smartUpdatableBrowser1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:smartUpdatableBrowser1:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:smartUpdatableBrowser1:LinkDataSource = THIS-OBJECT:smartTempTableAdapter1.
        THIS-OBJECT:smartUpdatableBrowser1:LinkTableIOSource = THIS-OBJECT:smartPanelController1.
        THIS-OBJECT:smartUpdatableBrowser1:Location = NEW System.Drawing.Point(5, 32).
        THIS-OBJECT:smartUpdatableBrowser1:Name = "smartUpdatableBrowser1":U.
        THIS-OBJECT:smartUpdatableBrowser1:PromptForSaveValuesOnRowDeactivate = FALSE.
        THIS-OBJECT:smartUpdatableBrowser1:SaveChangesOnLeave = TRUE.
        THIS-OBJECT:smartUpdatableBrowser1:SaveMode = TRUE.
        THIS-OBJECT:smartUpdatableBrowser1:SettingsKey = "e636bd67-986c-53be-0214-04d0acf49331":U.
        THIS-OBJECT:smartUpdatableBrowser1:SimpleKeyboardNavigationDuringUpdate = TRUE.
        THIS-OBJECT:smartUpdatableBrowser1:SimpleKeyboardNavigationDuringUpdateForLeftRight = FALSE.
        THIS-OBJECT:smartUpdatableBrowser1:Size = NEW System.Drawing.Size(571, 289).
        THIS-OBJECT:smartUpdatableBrowser1:TabIndex = 2.
        THIS-OBJECT:smartUpdatableBrowser1:Text = "smartUpdatableBrowser1":U.
        THIS-OBJECT:smartUpdatableBrowser1:InitializeRow:Subscribe(THIS-OBJECT:smartUpdatableBrowser1_InitializeRow).
        THIS-OBJECT:smartUpdatableBrowser1:CellChange:Subscribe(THIS-OBJECT:smartUpdatableBrowser1_CellChange).
        THIS-OBJECT:smartUpdatableBrowser1:ClickCellButton:Subscribe(THIS-OBJECT:smartUpdatableBrowser1_ClickCellButton).
        /*  */
        /* smartTempTableAdapter1 */
        /*  */
        THIS-OBJECT:smartTempTableAdapter1:BindingSourceExcludeFields = "RawValue":U.
        THIS-OBJECT:smartTempTableAdapter1:LinkDataSource = ?.
        THIS-OBJECT:smartTempTableAdapter1:LinkNavigationSource = THIS-OBJECT:smartPanelController1.
        THIS-OBJECT:smartTempTableAdapter1:BeforeDeleteRecord:Subscribe(THIS-OBJECT:smartTempTableAdapter1_BeforeDeleteRecord).
        THIS-OBJECT:smartTempTableAdapter1:AfterRetrieveData:Subscribe(THIS-OBJECT:smartTempTableAdapter1_AfterRetrieveData).
        /*  */
        /* ObjectTypeAttributesUserControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartUpdatableBrowser1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTextEditor1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartPanelController1).
        THIS-OBJECT:Name = "ObjectTypeAttributesUserControl":U.
        THIS-OBJECT:Padding = NEW System.Windows.Forms.Padding(5).
        THIS-OBJECT:Size = NEW System.Drawing.Size(581, 385).
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartUpdatableBrowser1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartTempTableAdapter1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Attribute Value cell in the given UltraGridRow
        Notes:   
        @param poRow The UltraGridRow to initialize
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID InitializeRow (poRow AS UltraGridRow):
		
        DEFINE VARIABLE cDataType     AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE oCell         AS UltraGridCell NO-UNDO . 
        DEFINE VARIABLE cLookupType   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cLookupValues AS CHARACTER NO-UNDO.

        IF NOT poRow:IsDataRow THEN 
            RETURN .
        
        oCell = poRow:Cells ["AttributeValue":U] .
        oCell:Style = ColumnStyle:Default . 
        
        ASSIGN cDataType = UNBOX (poRow:Cells ["RepositoryType":U]:Value) . 
        
        CASE cDataType:
        
            WHEN "CHARACTER":U THEN 
                ASSIGN oCell:Value = poRow:Cells["CharacterValue":U]:Value 
                       oCell:Style = ColumnStyle:Default .
            WHEN "INTEGER":U THEN 
                ASSIGN oCell:Value = poRow:Cells["IntegerValue":U]:Value 
                       oCell:Style = ColumnStyle:IntegerWithSpin .
            WHEN "INT64":U THEN 
                ASSIGN oCell:Value = poRow:Cells["Int64Value":U]:Value 
                       oCell:Style = ColumnStyle:IntegerWithSpin .
            WHEN "DATE":U THEN 
                ASSIGN oCell:Value = poRow:Cells["DateValue":U]:Value 
                       oCell:Style = ColumnStyle:Date .
            WHEN "DATETIME":U THEN 
                ASSIGN oCell:Value = poRow:Cells["DateTimeValue":U]:Value 
                       oCell:Style = ColumnStyle:DateTime .                
            WHEN "DATETIME-TZ":U THEN 
                ASSIGN oCell:Value = poRow:Cells["DateTimeTzValue":U]:Value 
                       oCell:Style = ColumnStyle:DateTime .                
            WHEN "DECIMAL":U THEN 
                ASSIGN oCell:Value = poRow:Cells["DecimalValue":U]:Value 
                       oCell:Style = ColumnStyle:DoubleWithSpin .                    
            WHEN "LOGICAL":U THEN 
                ASSIGN oCell:Value = poRow:Cells["LogicalValue":U]:Value 
                       oCell:Style = ColumnStyle:CheckBox .           
        END CASE .

        ASSIGN cLookupType   = UNBOX (poRow:Cells["LookupType":U]:Value) 
               cLookupValues = UNBOX (poRow:Cells["LookupValues":U]:Value) . 
         
        CASE cLookupType:
            WHEN "LIST":U THEN 
                ASSIGN oCell:ValueList  = ValueListHelper:FromListItemPairs (cLookupValues, CHR(3)) 
                       oCell:Style      = ColumnStyle:DropDownList
                       oCell:Activation = Infragistics.Win.UltraWinGrid.Activation:AllowEdit .
            WHEN "DIALOG":U THEN 
                ASSIGN oCell:Style                  = ColumnStyle:EditButton 
                       oCell:ButtonAppearance:Image = ImageHelper:Load ("Consultingwerk/Windows/Images/folder_16.png":U)
                       oCell:Activation             = Activation:AllowEdit .
            WHEN "DIALOG-R":U THEN 
                ASSIGN oCell:Style                  = ColumnStyle:EditButton 
                       oCell:ButtonAppearance:Image = ImageHelper:Load ("Consultingwerk/Windows/Images/folder_16.png":U)
                       oCell:Activation             = Activation:NoEdit .
                       
            OTHERWISE 
                ASSIGN oCell:Activation = Activation:AllowEdit .
        END CASE . 

        /* Read only row - when inherited and constant*/
        IF UNBOX (poRow:Cells["IsInherited":U]:Value) = TRUE AND 
           UNBOX (poRow:Cells["ConstantValue":U]:Value) = TRUE THEN DO: 
           
            ASSIGN poRow:Cells["IsInherited":U]:Activation    = Activation:NoEdit 
                   poRow:Cells["ConstantValue":U]:Activation  = Activation:NoEdit
                   poRow:Cells["AttributeValue":U]:Activation = Activation:NoEdit .
        END.    
        /* Can't change IsInherited (to reset to base class value) on un-inherited
           property */
        ELSE IF UNBOX (poRow:Cells["InheritedFrom":U]:Value) = "":U THEN DO:
            ASSIGN poRow:Cells["IsInherited":U]:Activation    = Activation:NoEdit 
                   poRow:Cells["ConstantValue":U]:Activation  = Activation:AllowEdit 
                   poRow:Cells["AttributeValue":U]:Activation = Activation:AllowEdit .

        END.
        ELSE 
            ASSIGN poRow:Cells["IsInherited":U]:Activation    = Activation:AllowEdit 
                   poRow:Cells["ConstantValue":U]:Activation  = Activation:AllowEdit
                   poRow:Cells["AttributeValue":U]:Activation = Activation:AllowEdit .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retrieves the SmartObejctType Attributes for design time
        Notes:   
        @param pcObjectTypeId the ID of the object type to retrieve
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID ShowObjectTypeAttributes (pcObjectTypeId AS CHARACTER):
		
		FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
		                                               "Consultingwerk.SmartFramework.Repository.Class.AttributeValueBusinessEntity":U,
		                                               "GetObjectTypeDesignAttributes":U,
		                                               INPUT-OUTPUT DATASET dsDesignAttributeValue BY-REFERENCE,
		                                               NEW GetObjectTypeDesignAttributesParameter (pcObjectTypeId)) .
		
        smartTempTableAdapter1:RetrieveData () .
        smartTempTableAdapter1:BindingSource:RefreshAll () . 

        {Consultingwerk/foreach.i Infragistics.Win.UltraWinGrid.UltraGridColumn oColumn in smartUpdatableBrowser1:DisplayLayout:Bands[0]:SortedColumns}
        
            IF oColumn:IsGroupByColumn THEN DO:
                smartUpdatableBrowser1:DisplayLayout:Bands[0]:SortedColumns:RefreshSort (TRUE) .
                LEAVE . 
            END.
        END.
        
        {Consultingwerk/foreach.i UltraGridRow oRow in smartUpdatableBrowser1:Rows}
            THIS-OBJECT:InitializeRow (oRow) .
        END.
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the AfterRetrieveData event of the smartTempTableAdapter1
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID smartTempTableAdapter1_AfterRetrieveData (sender AS System.Object, 
	                                                              e AS System.EventArgs):
		
        {Consultingwerk/foreach.i UltraGridRow oRow in smartUpdatableBrowser1:Rows}
            THIS-OBJECT:InitializeRow (oRow) .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the BeforeDeleteRecord event of the smartTempTableAdpater1 
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The AfterDeleteRecordEventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID smartTempTableAdapter1_BeforeDeleteRecord (sender AS System.Object, 
	                                                               e AS Consultingwerk.SmartComponents.Implementation.BeforeDeleteRecordEventArgs):
		
        IF e:BufferHandle::InheritedFrom > "":U THEN DO:
            
            e:Cancel = TRUE .
            
            MessageFormHelper:ShowMessage ("Cannot delete inherited attribute."{&TRAN},
                                           THIS-OBJECT:FindForm():Text,
                                           MessageFormImages:ImageWarning) .
            
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the CellChange event of the smartUpdatableBrowser1 
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The CellEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID smartUpdatableBrowser1_CellChange (sender AS System.Object, 
	                                                       e AS CellEventArgs):
		
		DEFINE VARIABLE oEditor                                AS EmbeddableEditorBase                  NO-UNDO .
		DEFINE VARIABLE oRow                                   AS UltraGridRow                          NO-UNDO .  
        DEFINE VARIABLE cDataType                              AS CHARACTER                             NO-UNDO .
		DEFINE VARIABLE oGetObjectTypeDesignAttributeParameter AS GetObjectTypeDesignAttributeParameter NO-UNDO . 
		DEFINE VARIABLE hDataset                               AS HANDLE                                NO-UNDO .
		
		CASE e:Cell:Column:Key:
		    WHEN "AttributeValue":U THEN DO:
		
                ASSIGN oRow      = e:Cell:Row 
                	   cDataType = UNBOX (oRow:Cells ["RepositoryType":U]:Value) 
                       oEditor   = e:Cell:EditorResolved .
                
                CASE cDataType:
                
                    WHEN "CHARACTER":U THEN 
                        ASSIGN oRow:Cells ["CharacterValue":U]:Value  = oEditor:Value . 
                    WHEN "INTEGER":U THEN                             
                        ASSIGN oRow:Cells ["IntegerValue":U]:Value    = oEditor:Value .
                    WHEN "INT64":U THEN                               
                        ASSIGN oRow:Cells ["Int64Value":U]:Value      = oEditor:Value .
                    WHEN "DATE":U THEN                                
                        ASSIGN oRow:Cells ["DateValue":U]:Value       = oEditor:Value .
                    WHEN "DATETIME":U THEN                            
                        ASSIGN oRow:Cells ["DateTimeValue":U]:Value   = oEditor:Value .
                    WHEN "DATETIME-TZ":U THEN  
                        ASSIGN oRow:Cells ["DateTimeTzValue":U]:Value = oEditor:Value . 
                    WHEN "DECIMAL":U THEN 
                        ASSIGN oRow:Cells ["DecimalValue":U]:Value    = oEditor:Value . 
                    WHEN "LOGICAL":U THEN 
                        ASSIGN oRow:Cells ["LogicalValue":U]:Value    = oEditor:Value .
                END CASE .
        
                /* When value was changes, it will be stored on the current class, 
                   not on the parent */
                IF e:Cell:IsInEditMode THEN 
                    oRow:Cells["IsInherited":U]:Value = BOX (FALSE) .
            END.
            WHEN "IsInherited":U THEN DO:
                
                IF e:Cell:IsInEditMode THEN DO:
                    
                    ASSIGN oRow = e:Cell:Row .  
                            
                    IF DataTypeHelper:ToLogical (e:Cell:Text) = TRUE THEN DO ON ERROR UNDO, THROW: 
    
                        oGetObjectTypeDesignAttributeParameter = NEW GetObjectTypeDesignAttributeParameter (UNBOX (oRow:Cells["InheritedFrom":U]:Value),
                                                                                                            UNBOX (oRow:Cells["AttributeLabel":U]:Value)) .                     
    
                        FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                                       "Consultingwerk.SmartFramework.Repository.Class.AttributeValueBusinessEntity":U,
                                                                       "GetObjectTypeDesignAttribute":U,
                                                                       INPUT-OUTPUT DATASET-HANDLE hDataset,
                                                                       oGetObjectTypeDesignAttributeParameter) .
    
                        hDataset:GET-BUFFER-HANDLE (1):FIND-FIRST () . 

                        IF hDataset:GET-BUFFER-HANDLE (1):AVAILABLE THEN DO:

                            CASE STRING (UNBOX (oRow:Cells["RepositoryType":U]:Value)):
                                WHEN "CHARACTER":U THEN 
                                    ASSIGN oRow:Cells["CharacterValue":U]:Value  = BOX (hDataset:GET-BUFFER-HANDLE (1)::CharacterValue) .
                                WHEN "INTEGER":U THEN 
                                    ASSIGN oRow:Cells["IntegerValue":U]:Value    = BOX (hDataset:GET-BUFFER-HANDLE (1)::IntegerValue) .
                                WHEN "INT64":U THEN 
                                    ASSIGN oRow:Cells["Int64Value":U]:Value      = BOX (hDataset:GET-BUFFER-HANDLE (1)::Int64Value) .
                                WHEN "DATE":U THEN 
                                    ASSIGN oRow:Cells["DateValue":U]:Value       = BOX (hDataset:GET-BUFFER-HANDLE (1)::DateValue) .
                                WHEN "DATETIME":U THEN 
                                    ASSIGN oRow:Cells["DateTimeValue":U]:Value   = BOX (hDataset:GET-BUFFER-HANDLE (1)::DateTimeValue) .
                                WHEN "DATETIME-TZ":U THEN 
                                    ASSIGN oRow:Cells["DateTimeTzValue":U]:Value = BOX (hDataset:GET-BUFFER-HANDLE (1)::DateTimeTzValue) .
                                WHEN "DECIMAL":U THEN 
                                    ASSIGN oRow:Cells["DecimalValue":U]:Value    = BOX (hDataset:GET-BUFFER-HANDLE (1)::DecimalValue) .
                                WHEN "LOGICAL":U THEN 
                                    ASSIGN oRow:Cells["LogicalValue":U]:Value    = BOX (hDataset:GET-BUFFER-HANDLE (1)::LogicalValue) .
                            END CASE . 
                                    
                            THIS-OBJECT:InitializeRow (oRow) .
                        END.
                        
                        FINALLY:
                            IF VALID-HANDLE (hDataset) THEN 
                                DELETE OBJECT (hDataset) . 		
                        END FINALLY.
                    END .                 
                END .
            END.

        END CASE.

        CATCH err AS Progress.Lang.Error:
        	ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.
            
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ClickCellButton event of the smartUpdatableBrowser1
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The CellEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID smartUpdatableBrowser1_ClickCellButton (sender AS System.Object, 
	                                                            e AS Infragistics.Win.UltraWinGrid.CellEventArgs):
		
		MESSAGE e:Cell:Row:Cells["LookupValues":U]:VAlue
            VIEW-AS ALERT-BOX.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the InitializeRow event of the smartUpdatableBrowser1  
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The InitializeRowEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID smartUpdatableBrowser1_InitializeRow (sender AS System.Object, 
	                                                          e AS InitializeRowEventArgs):
		
		THIS-OBJECT:InitializeRow (e:Row) .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.
		
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the ObjectTypeAttributesUserControl class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC ObjectTypeAttributesUserControl ():

        smartUpdatableBrowser1:SmartDataSource = ? .
        
        GarbageCollectorHelper:DeleteObject (smartTempTableAdapter1) .
                
    END DESTRUCTOR .

END CLASS.
