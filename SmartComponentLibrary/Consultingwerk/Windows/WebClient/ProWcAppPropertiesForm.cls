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
    File        : ProWcAppPropertiesForm
    Purpose     : Displays contents of a .prowcapp file in two grids
    Syntax      : 
    Description : Based on Consultingwerk.Windows.WebClient.ProWcAppParser.ProWcAppParser
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Aug 31 10:52:41 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Forms.*                            FROM PROPATH .
USING Consultingwerk.Windows.WebClient.*                FROM PROPATH .
USING Consultingwerk.Windows.WebClient.ProWcAppParser.* FROM PROPATH .  
USING Progress.Lang.*                                   FROM PROPATH .

CLASS Consultingwerk.Windows.WebClient.ProWcAppPropertiesForm 
    INHERITS BaseForm: 

    DEFINE PRIVATE VARIABLE bindingSource2 AS Progress.Data.BindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE bindingSource1 AS Progress.Data.BindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE button1 AS System.Windows.Forms.Button NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartTempTableAdapter2 AS Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE smartTempTableAdapter1 AS Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE smartDataBrowser2 AS Consultingwerk.SmartComponents.Implementation.SmartDataBrowser NO-UNDO.
    DEFINE PRIVATE VARIABLE smartDataBrowser1 AS Consultingwerk.SmartComponents.Implementation.SmartDataBrowser NO-UNDO.
    DEFINE PRIVATE VARIABLE tableLayoutPanel1 AS System.Windows.Forms.TableLayoutPanel NO-UNDO.
	
    DEFINE VARIABLE oFullWindowDragManager1 AS Consultingwerk.Windows.Util.Forms.FullWindowDragManager NO-UNDO . 
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
	
    { Consultingwerk/Windows/WebClient/ProWcAppParser/dsProWcApp.i }
	
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ProWcAppPropertiesForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC ProWcAppPropertiesForm ():
		
        SUPER().
        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF
 
        oFullWindowDragManager1 = NEW Consultingwerk.Windows.Util.Forms.FullWindowDragManager (THIS-OBJECT) .
 
        smartTempTableAdapter1:TempTableHandle = TEMP-TABLE ttApplicationProperties:HANDLE .
        smartTempTableAdapter2:TempTableHandle = TEMP-TABLE ttVersions:HANDLE .
 
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the button1
        Notes:   
        @param sender The System.Object that raised the event
        @param e The System.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID button1_Click (INPUT sender AS System.Object, INPUT e AS System.EventArgs):
		
		THIS-OBJECT:Close () . 

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
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("ttApplicationProperties":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc2 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc2 = NEW Progress.Data.TableDesc("ttVersions":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("ttApplicationProperties":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn7 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn7 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("Section":U, -1, ?, 0, Infragistics.Win.UltraWinGrid.SortIndicator:Ascending, TRUE).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn8 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn8 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("EntryName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn9 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn9 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("EntryValue":U).
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
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand2 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand2 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("ttVersions":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn10 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn10 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("VersionNumber":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn11 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn11 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("VersionName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn12 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn12 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("EndUserDescription":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
        appearance16 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
        appearance17 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
        appearance18 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
        appearance19 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance20 AS Infragistics.Win.Appearance NO-UNDO.
        appearance20 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance21 AS Infragistics.Win.Appearance NO-UNDO.
        appearance21 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance22 AS Infragistics.Win.Appearance NO-UNDO.
        appearance22 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance23 AS Infragistics.Win.Appearance NO-UNDO.
        appearance23 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance24 AS Infragistics.Win.Appearance NO-UNDO.
        appearance24 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.WebClient.ProWcAppPropertiesForm":U).
        THIS-OBJECT:bindingSource1 = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
        THIS-OBJECT:bindingSource2 = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
        THIS-OBJECT:smartTempTableAdapter1 = NEW Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter(THIS-OBJECT:components).
        THIS-OBJECT:smartTempTableAdapter2 = NEW Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter(THIS-OBJECT:components).
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:tableLayoutPanel1 = NEW System.Windows.Forms.TableLayoutPanel().
        THIS-OBJECT:smartDataBrowser1 = NEW Consultingwerk.SmartComponents.Implementation.SmartDataBrowser().
        THIS-OBJECT:smartDataBrowser2 = NEW Consultingwerk.SmartComponents.Implementation.SmartDataBrowser().
        THIS-OBJECT:button1 = NEW System.Windows.Forms.Button().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:bindingSource2, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartTempTableAdapter1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartTempTableAdapter2, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:panel1:SuspendLayout().
        THIS-OBJECT:tableLayoutPanel1:SuspendLayout().
        CAST(THIS-OBJECT:smartDataBrowser1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartDataBrowser2, System.ComponentModel.ISupportInitialize):BeginInit().
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
        DEFINE VARIABLE arrayvar1 AS Progress.Data.ColumnPropDesc EXTENT 3 NO-UNDO.
        arrayvar1[1] = NEW Progress.Data.ColumnPropDesc("Section":U, "Section":U, Progress.Data.DataType:CHARACTER).
        arrayvar1[2] = NEW Progress.Data.ColumnPropDesc("EntryName":U, "Entry":U, Progress.Data.DataType:CHARACTER).
        arrayvar1[3] = NEW Progress.Data.ColumnPropDesc("EntryValue":U, "Value":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar1.
        THIS-OBJECT:bindingSource1:TableSchema = tableDesc1.
        /*  */
        /* bindingSource2 */
        /*  */
        THIS-OBJECT:bindingSource2:MaxDataGuess = 0.
        THIS-OBJECT:bindingSource2:NoLOBs = FALSE.
        THIS-OBJECT:bindingSource2:Position = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS "Progress.Data.TableDesc[]" NO-UNDO.
        arrayvar2 = NEW "Progress.Data.TableDesc[]"(0).
        tableDesc2:ChildTables = arrayvar2.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Progress.Data.ColumnPropDesc EXTENT 3 NO-UNDO.
        arrayvar3[1] = NEW Progress.Data.ColumnPropDesc("VersionNumber":U, "Sequence":U, Progress.Data.DataType:INTEGER).
        arrayvar3[2] = NEW Progress.Data.ColumnPropDesc("VersionName":U, "Version":U, Progress.Data.DataType:CHARACTER).
        arrayvar3[3] = NEW Progress.Data.ColumnPropDesc("EndUserDescription":U, "End User Description":U, Progress.Data.DataType:CHARACTER).
        tableDesc2:Columns = arrayvar3.
        THIS-OBJECT:bindingSource2:TableSchema = tableDesc2.
        /*  */
        /* smartTempTableAdapter1 */
        /*  */
        THIS-OBJECT:smartTempTableAdapter1:AppServerPartition = "":U.
        THIS-OBJECT:smartTempTableAdapter1:BindingSourceExcludeFields = "":U.
        THIS-OBJECT:smartTempTableAdapter1:BindingSourceIncludeFields = "*":U.
        THIS-OBJECT:smartTempTableAdapter1:LinkDataSource = ?.
        THIS-OBJECT:smartTempTableAdapter1:LinkNavigationSource = ?.
        THIS-OBJECT:smartTempTableAdapter1:SmartDataSourceActive = TRUE.
        /*  */
        /* smartTempTableAdapter2 */
        /*  */
        THIS-OBJECT:smartTempTableAdapter2:AppServerPartition = "":U.
        THIS-OBJECT:smartTempTableAdapter2:BindingSourceExcludeFields = "":U.
        THIS-OBJECT:smartTempTableAdapter2:BindingSourceIncludeFields = "*":U.
        THIS-OBJECT:smartTempTableAdapter2:LinkDataSource = ?.
        THIS-OBJECT:smartTempTableAdapter2:LinkNavigationSource = ?.
        THIS-OBJECT:smartTempTableAdapter2:SmartDataSourceActive = TRUE.
        /*  */
        /* panel1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:tableLayoutPanel1).
        THIS-OBJECT:panel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:panel1:Name = "panel1":U.
        THIS-OBJECT:panel1:Size = NEW System.Drawing.Size(750, 341).
        THIS-OBJECT:panel1:TabIndex = 2.
        /*  */
        /* tableLayoutPanel1 */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tableLayoutPanel1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tableLayoutPanel1:ColumnCount = 2.
        THIS-OBJECT:tableLayoutPanel1:ColumnStyles:Add(NEW System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType:Percent, Progress.Util.CastUtil:ToSingle(50))).
        THIS-OBJECT:tableLayoutPanel1:ColumnStyles:Add(NEW System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType:Percent, Progress.Util.CastUtil:ToSingle(50))).
        THIS-OBJECT:tableLayoutPanel1:Controls:Add(THIS-OBJECT:smartDataBrowser1, 0, 1).
        THIS-OBJECT:tableLayoutPanel1:Controls:Add(THIS-OBJECT:smartDataBrowser2, 1, 1).
        THIS-OBJECT:tableLayoutPanel1:Controls:Add(THIS-OBJECT:ultraLabel1, 0, 0).
        THIS-OBJECT:tableLayoutPanel1:Controls:Add(THIS-OBJECT:ultraLabel2, 1, 0).
        THIS-OBJECT:tableLayoutPanel1:Location = NEW System.Drawing.Point(0, 12).
        THIS-OBJECT:tableLayoutPanel1:Margin = NEW System.Windows.Forms.Padding(12).
        THIS-OBJECT:tableLayoutPanel1:Name = "tableLayoutPanel1":U.
        THIS-OBJECT:tableLayoutPanel1:RowCount = 2.
        THIS-OBJECT:tableLayoutPanel1:RowStyles:Add(NEW System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType:Absolute, Progress.Util.CastUtil:ToSingle(20))).
        THIS-OBJECT:tableLayoutPanel1:RowStyles:Add(NEW System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType:Percent, Progress.Util.CastUtil:ToSingle(100))).
        THIS-OBJECT:tableLayoutPanel1:Size = NEW System.Drawing.Size(750, 317).
        THIS-OBJECT:tableLayoutPanel1:TabIndex = 0.
        /*  */
        /* smartDataBrowser1 */
        /*  */
        THIS-OBJECT:smartDataBrowser1:DataSource = THIS-OBJECT:bindingSource1.
        THIS-OBJECT:smartDataBrowser1:DeselectRowsOnActivate = TRUE.
        appearance1:BackColor = System.Drawing.SystemColors:Window.
        appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Appearance = appearance1.
        ultraGridColumn7:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn7:Header:VisiblePosition = 0.
        ultraGridColumn8:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn8:Header:VisiblePosition = 1.
        ultraGridColumn9:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn9:Header:VisiblePosition = 2.
        ultraGridColumn9:Width = 272.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS System.Object EXTENT 3 NO-UNDO.
        arrayvar4[1] = ultraGridColumn7.
        arrayvar4[2] = ultraGridColumn8.
        arrayvar4[3] = ultraGridColumn9.
        ultraGridBand1:Columns:AddRange(arrayvar4).
        ultraGridBand1:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        ultraGridBand1:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:BandsSerializer:Add(ultraGridBand1).
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance2:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:GroupByBox:Appearance = appearance2.
        appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance4:BackColor2 = System.Drawing.SystemColors:Control.
        appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:MaxRowScrollRegions = 1.
        appearance5:BackColor = System.Drawing.SystemColors:Window.
        appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:ActiveCellAppearance = appearance5.
        appearance6:BackColor = System.Drawing.SystemColors:Highlight.
        appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:ActiveRowAppearance = appearance6.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance7:BackColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:CardAreaAppearance = appearance7.
        appearance8:BorderColor = System.Drawing.Color:Silver.
        appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:CellAppearance = appearance8.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:CellPadding = 0.
        appearance9:BackColor = System.Drawing.SystemColors:Control.
        appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance9:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:GroupByRowAppearance = appearance9.
        appearance10:TextHAlignAsString = "Left":U.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:HeaderAppearance = appearance10.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortMulti.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance11:BackColor = System.Drawing.SystemColors:Window.
        appearance11:BorderColor = System.Drawing.Color:Silver.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:RowAppearance = appearance11.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:smartDataBrowser1:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
        THIS-OBJECT:smartDataBrowser1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:smartDataBrowser1:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:smartDataBrowser1:LinkDataSource = THIS-OBJECT:smartTempTableAdapter1.
        THIS-OBJECT:smartDataBrowser1:Location = NEW System.Drawing.Point(12, 32).
        THIS-OBJECT:smartDataBrowser1:Margin = NEW System.Windows.Forms.Padding(12).
        THIS-OBJECT:smartDataBrowser1:Name = "smartDataBrowser1":U.
        THIS-OBJECT:smartDataBrowser1:SettingsKey = "b577ac22-c729-438a-ff13-5bb7c055b820":U.
        THIS-OBJECT:smartDataBrowser1:Size = NEW System.Drawing.Size(351, 273).
        THIS-OBJECT:smartDataBrowser1:TabIndex = 0.
        THIS-OBJECT:smartDataBrowser1:Text = "smartDataBrowser1":U.
        /*  */
        /* smartDataBrowser2 */
        /*  */
        THIS-OBJECT:smartDataBrowser2:DataSource = THIS-OBJECT:bindingSource2.
        THIS-OBJECT:smartDataBrowser2:DeselectRowsOnActivate = TRUE.
        appearance13:BackColor = System.Drawing.SystemColors:Window.
        appearance13:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Appearance = appearance13.
        ultraGridColumn10:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn10:FilterOperatorDefaultValue = Infragistics.Win.UltraWinGrid.FilterOperatorDefaultValue:GreaterThanOrEqualTo.
        ultraGridColumn10:Header:VisiblePosition = 0.
        ultraGridColumn11:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn11:Header:VisiblePosition = 1.
        ultraGridColumn12:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn12:Header:VisiblePosition = 2.
        ultraGridColumn12:Width = 437.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS System.Object EXTENT 3 NO-UNDO.
        arrayvar5[1] = ultraGridColumn10.
        arrayvar5[2] = ultraGridColumn11.
        arrayvar5[3] = ultraGridColumn12.
        ultraGridBand2:Columns:AddRange(arrayvar5).
        ultraGridBand2:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        ultraGridBand2:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand2:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand2:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:BandsSerializer:Add(ultraGridBand2).
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        appearance14:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance14:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance14:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance14:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:GroupByBox:Appearance = appearance14.
        appearance15:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:GroupByBox:BandLabelAppearance = appearance15.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        appearance16:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance16:BackColor2 = System.Drawing.SystemColors:Control.
        appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance16:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:GroupByBox:PromptAppearance = appearance16.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:MaxRowScrollRegions = 1.
        appearance17:BackColor = System.Drawing.SystemColors:Window.
        appearance17:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:ActiveCellAppearance = appearance17.
        appearance18:BackColor = System.Drawing.SystemColors:Highlight.
        appearance18:ForeColor = System.Drawing.SystemColors:HighlightText.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:ActiveRowAppearance = appearance18.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance19:BackColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:CardAreaAppearance = appearance19.
        appearance20:BorderColor = System.Drawing.Color:Silver.
        appearance20:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:CellAppearance = appearance20.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:CellPadding = 0.
        appearance21:BackColor = System.Drawing.SystemColors:Control.
        appearance21:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance21:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance21:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance21:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:GroupByRowAppearance = appearance21.
        appearance22:TextHAlignAsString = "Left":U.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:HeaderAppearance = appearance22.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortMulti.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance23:BackColor = System.Drawing.SystemColors:Window.
        appearance23:BorderColor = System.Drawing.Color:Silver.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:RowAppearance = appearance23.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        appearance24:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:Override:TemplateAddRowAppearance = appearance24.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:smartDataBrowser2:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
        THIS-OBJECT:smartDataBrowser2:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:smartDataBrowser2:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:smartDataBrowser2:LinkDataSource = THIS-OBJECT:smartTempTableAdapter2.
        THIS-OBJECT:smartDataBrowser2:Location = NEW System.Drawing.Point(387, 32).
        THIS-OBJECT:smartDataBrowser2:Margin = NEW System.Windows.Forms.Padding(12).
        THIS-OBJECT:smartDataBrowser2:Name = "smartDataBrowser2":U.
        THIS-OBJECT:smartDataBrowser2:SettingsKey = "b577ac22-c729-438a-ff13-5cb79854e4ad":U.
        THIS-OBJECT:smartDataBrowser2:Size = NEW System.Drawing.Size(351, 273).
        THIS-OBJECT:smartDataBrowser2:TabIndex = 1.
        THIS-OBJECT:smartDataBrowser2:Text = "smartDataBrowser2":U.
        /*  */
        /* button1 */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:button1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:button1:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:button1:ImeMode = System.Windows.Forms.ImeMode:NoControl.
        THIS-OBJECT:button1:Location = NEW System.Drawing.Point(664, 367).
        THIS-OBJECT:button1:Name = "button1":U.
        THIS-OBJECT:button1:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:button1:TabIndex = 3.
        THIS-OBJECT:button1:Text = "&OK":U.
        THIS-OBJECT:button1:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:button1:UseVisualStyleBackColor = TRUE.
        THIS-OBJECT:button1:Click:Subscribe(THIS-OBJECT:button1_Click).
        /*  */
        /* ultraLabel1 */
        /*  */
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(12, 3).
        THIS-OBJECT:ultraLabel1:Margin = NEW System.Windows.Forms.Padding(12, 3, 3, 3).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(100, 14).
        THIS-OBJECT:ultraLabel1:TabIndex = 2.
        THIS-OBJECT:ultraLabel1:Text = "Properties:":U.
        /*  */
        /* ultraLabel2 */
        /*  */
        THIS-OBJECT:ultraLabel2:Location = NEW System.Drawing.Point(387, 3).
        THIS-OBJECT:ultraLabel2:Margin = NEW System.Windows.Forms.Padding(12, 3, 3, 3).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        THIS-OBJECT:ultraLabel2:Size = NEW System.Drawing.Size(100, 14).
        THIS-OBJECT:ultraLabel2:TabIndex = 3.
        THIS-OBJECT:ultraLabel2:Text = "Version Info:":U.
        /*  */
        /* ProWcAppPropertiesForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(751, 402).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:button1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "ProWcAppPropertiesForm":U.
        THIS-OBJECT:Text = "WebClient Application Properties":U.
        THIS-OBJECT:WindowPositionRegistryKey = "ProWcAppPropertiesForm":U.
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:bindingSource2, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartTempTableAdapter1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartTempTableAdapter2, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        THIS-OBJECT:tableLayoutPanel1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartDataBrowser1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartDataBrowser2, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Loads a prowcapp file from the given location 
        Notes:   
        @param pcFileName The name of the file to load
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID LoadFrom (pcFileName AS CHARACTER):
		
		DEFINE VARIABLE oParser AS ProWcAppParser NO-UNDO .
		
        oParser = NEW ProWcAppParser () . 

        oParser:ParseProWcAppFile (pcFileName, 
                                   OUTPUT DATASET dsProWcApp) .
		
		smartTempTableAdapter1:RetrieveData() .
        smartTempTableAdapter2:RetrieveData() .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the ProWcAppPropertiesForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC ProWcAppPropertiesForm ():

    END DESTRUCTOR .

END CLASS.
