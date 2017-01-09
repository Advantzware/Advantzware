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
    File        : TaskPanelControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jun 04 12:15:57 CEST 2009
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Advantzware.WinKit.Samples.TaskPanel.TaskPanelControl INHERITS UserControl   : 
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

	DEFINE PRIVATE VARIABLE bindingSource1 AS Progress.Data.BindingSource NO-UNDO.
	DEFINE PRIVATE VARIABLE ultraGrid1 AS Infragistics.Win.UltraWinGrid.UltraGrid NO-UNDO.

    {Advantzware\WinKit\Samples\TaskPanel\dsTasks.i}
		
    DEFINE PRIVATE QUERY qQuery FOR ttTasks SCROLLING . 		
		
	CONSTRUCTOR PUBLIC TaskPanelControl (  ):
		
		
        SUPER().
        InitializeComponent().
        
        FillSomeData () .
        
        OPEN QUERY qQuery PRESELECT EACH ttTasks . 
        
        bindingSource1:HANDLE = QUERY qQuery:HANDLE . 
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

	

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/	
	METHOD PUBLIC VOID FillSomeData(  ):
		
		CREATE ttTasks.
		
		FILE-INFO:FILE-NAME = "Advantzware/WinKit/Samples/TaskPanel/Images\task.jpg" .
		
		COPY-LOB FROM FILE FILE-INFO:FULL-PATHNAME TO ttTasks.TaskIcon .
		
		ASSIGN ttTasks.TaskNum = "4"
		       ttTasks.TaskDescription = "Auftrag von Go Fishing Ltd. erledigen"
		       ttTasks.TaskDetails = "Prüfen von Order Nr. 4711" .
		
		RELEASE ttTasks.

		CREATE ttTasks.

        FILE-INFO:FILE-NAME = "Advantzware/WinKit/Samples/TaskPanel/Images\email.jpg" .

        COPY-LOB FROM FILE FILE-INFO:FULL-PATHNAME TO ttTasks.TaskIcon .
		
		ASSIGN ttTasks.TaskNum = "14"
		       ttTasks.TaskDescription = "Email von Paris St Germain bearbeiten"
		       ttTasks.TaskDetails = "Email erhalten am 31.05.2009 von info@stgermain.fr." .
		
		RELEASE ttTasks.

		CREATE ttTasks.

        FILE-INFO:FILE-NAME = "Advantzware/WinKit/Samples/TaskPanel/Images\task.jpg" .

        COPY-LOB FROM FILE FILE-INFO:FULL-PATHNAME TO ttTasks.TaskIcon .
		
		ASSIGN ttTasks.TaskNum = "21"
		       ttTasks.TaskDescription = "Reklamation von Pedal Power Cycles"
		       ttTasks.TaskDetails = "Bitte dringend prüfen!" .
		
		RELEASE ttTasks.

		CREATE ttTasks.

        FILE-INFO:FILE-NAME = "Advantzware/WinKit/Samples/TaskPanel/Images\forward.jpg" .

        COPY-LOB FROM FILE FILE-INFO:FULL-PATHNAME TO ttTasks.TaskIcon .
		
		ASSIGN ttTasks.TaskNum = "33"
		       ttTasks.TaskDescription = "Information an Tennis Agentti weiter leiten"
		       ttTasks.TaskDetails = "Aktuelle Preisliste angefordert" .
		
		RELEASE ttTasks.


	END METHOD.


 	METHOD PRIVATE VOID InitializeComponent(  ):
		
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("ttTasks").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraGridBand1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("ttTasks", -1).
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraGridColumn1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TaskNum").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraGridColumn2 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn2 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TaskIcon").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraGridColumn3 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn3 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TaskDescription").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraGridColumn4 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn4 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("TaskDetails").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
        appearance9 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
        appearance10 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
        appearance12 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance25 AS Infragistics.Win.Appearance NO-UNDO.
        appearance25 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance26 AS Infragistics.Win.Appearance NO-UNDO.
        appearance26 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:bindingSource1 = NEW Progress.Data.BindingSource().
        THIS-OBJECT:ultraGrid1 = NEW Infragistics.Win.UltraWinGrid.UltraGrid().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraGrid1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* bindingSource1 */
        /*  */
        THIS-OBJECT:bindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:bindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 4 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("TaskNum", "TaskNum", Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("TaskIcon", "TaskIcon", Progress.Data.DataType:BLOB).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("TaskDescription", "TaskDescription", Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("TaskDetails", "TaskDetails", Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:bindingSource1:TableSchema = tableDesc1.
        /*  */
        /* ultraGrid1 */
        /*  */
        THIS-OBJECT:ultraGrid1:DataSource = THIS-OBJECT:bindingSource1.
        appearance3:BackColor = System.Drawing.SystemColors:Window.
        appearance3:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Appearance = appearance3.
        THIS-OBJECT:ultraGrid1:DisplayLayout:AutoFitStyle = Infragistics.Win.UltraWinGrid.AutoFitStyle:ExtendLastColumn.
        ultraGridColumn1:Header:VisiblePosition = 1.
        ultraGridColumn1:Hidden = TRUE.
        ultraGridColumn2:Header:VisiblePosition = 0.
        ultraGridColumn2:RowLayoutColumnInfo:LabelPosition = Infragistics.Win.UltraWinGrid.LabelPosition:None.
        ultraGridColumn2:RowLayoutColumnInfo:OriginX = 0.
        ultraGridColumn2:RowLayoutColumnInfo:OriginY = 0.
        ultraGridColumn2:RowLayoutColumnInfo:PreferredCellSize = NEW System.Drawing.Size(0, 32).
        ultraGridColumn2:RowLayoutColumnInfo:SpanX = 2.
        ultraGridColumn2:RowLayoutColumnInfo:SpanY = 4.
        ultraGridColumn2:Style = Infragistics.Win.UltraWinGrid.ColumnStyle:Image.
        ultraGridColumn2:Width = 20.
        appearance4:FontData:BoldAsString = "True".
        ultraGridColumn3:CellAppearance = appearance4.
        ultraGridColumn3:Header:VisiblePosition = 2.
        ultraGridColumn3:RowLayoutColumnInfo:LabelPosition = Infragistics.Win.UltraWinGrid.LabelPosition:None.
        ultraGridColumn3:RowLayoutColumnInfo:OriginX = 2.
        ultraGridColumn3:RowLayoutColumnInfo:OriginY = 0.
        ultraGridColumn3:RowLayoutColumnInfo:SpanX = 2.
        ultraGridColumn3:RowLayoutColumnInfo:SpanY = 2.
        appearance5:ForeColor = System.Drawing.Color:Silver.
        ultraGridColumn4:CellAppearance = appearance5.
        ultraGridColumn4:Header:VisiblePosition = 3.
        ultraGridColumn4:RowLayoutColumnInfo:LabelPosition = Infragistics.Win.UltraWinGrid.LabelPosition:None.
        ultraGridColumn4:RowLayoutColumnInfo:OriginX = 2.
        ultraGridColumn4:RowLayoutColumnInfo:OriginY = 2.
        ultraGridColumn4:RowLayoutColumnInfo:SpanX = 2.
        ultraGridColumn4:RowLayoutColumnInfo:SpanY = 2.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar1 AS System.Object EXTENT 4 NO-UNDO.
        arrayvar1[1] = ultraGridColumn1.
        arrayvar1[2] = ultraGridColumn2.
        arrayvar1[3] = ultraGridColumn3.
        arrayvar1[4] = ultraGridColumn4.
        ultraGridBand1:Columns:AddRange(arrayvar1).
        ultraGridBand1:UseRowLayout = TRUE.
        THIS-OBJECT:ultraGrid1:DisplayLayout:BandsSerializer:Add(ultraGridBand1).
        THIS-OBJECT:ultraGrid1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:ultraGrid1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        appearance6:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance6:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance6:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance6:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:ultraGrid1:DisplayLayout:GroupByBox:Appearance = appearance6.
        appearance7:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:ultraGrid1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance7.
        THIS-OBJECT:ultraGrid1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        appearance8:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance8:BackColor2 = System.Drawing.SystemColors:Control.
        appearance8:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance8:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:ultraGrid1:DisplayLayout:GroupByBox:PromptAppearance = appearance8.
        THIS-OBJECT:ultraGrid1:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:ultraGrid1:DisplayLayout:MaxRowScrollRegions = 1.
        appearance9:BackColor = System.Drawing.SystemColors:Window.
        appearance9:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:ActiveCellAppearance = appearance9.
        appearance10:BackColor = System.Drawing.SystemColors:Highlight.
        appearance10:ForeColor = System.Drawing.SystemColors:HighlightText.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:ActiveRowAppearance = appearance10.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:AllowGroupBy = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance11:BackColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:CardAreaAppearance = appearance11.
        appearance12:BorderColor = System.Drawing.Color:Transparent.
        appearance12:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:CellAppearance = appearance12.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:CellPadding = 0.
        appearance14:BackColor = System.Drawing.SystemColors:Control.
        appearance14:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance14:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance14:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance14:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:GroupByRowAppearance = appearance14.
        appearance15:TextHAlignAsString = "Left".
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:HeaderAppearance = appearance15.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance25:BackColor = System.Drawing.SystemColors:Window.
        appearance25:BorderColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:RowAppearance = appearance25.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:SelectTypeGroupByRow = Infragistics.Win.UltraWinGrid.SelectType:None.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:Extended.
        appearance26:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:TemplateAddRowAppearance = appearance26.
        THIS-OBJECT:ultraGrid1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:ultraGrid1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:ultraGrid1:DisplayLayout:TabNavigation = Infragistics.Win.UltraWinGrid.TabNavigation:NextControlOnLastCell.
        THIS-OBJECT:ultraGrid1:DisplayLayout:ViewStyle = Infragistics.Win.UltraWinGrid.ViewStyle:SingleBand.
        THIS-OBJECT:ultraGrid1:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
        THIS-OBJECT:ultraGrid1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraGrid1:Font = NEW System.Drawing.Font("Microsoft Sans Serif", 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, Progress.Util.CastUtil:ToByte(0)).
        THIS-OBJECT:ultraGrid1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraGrid1:Name = "ultraGrid1".
        THIS-OBJECT:ultraGrid1:Size = NEW System.Drawing.Size(453, 197).
        THIS-OBJECT:ultraGrid1:TabIndex = 0.
        THIS-OBJECT:ultraGrid1:Text = "ultraGrid1".
        THIS-OBJECT:ultraGrid1:DoubleClickRow:SUBSCRIBE(THIS-OBJECT:ultraGrid1_DoubleClickRow).
        /*  */
        /* TaskPanelControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(6, 13).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraGrid1).
        THIS-OBJECT:Name = "TaskPanelControl".
        THIS-OBJECT:Size = NEW System.Drawing.Size(453, 197).
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraGrid1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD.
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ultraGrid1_DoubleClickRow( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.DoubleClickRowEventArgs ):
	
	    DEFINE VARIABLE hWindow AS HANDLE NO-UNDO.
	    DEFINE VARIABLE hSDO    AS HANDLE NO-UNDO.
	
	    RUN src/demo/customersmart.w PERSISTENT SET hWindow .
	    RUN initializeObject IN hWindow . 
	    
	    ASSIGN hSDO = DYNAMIC-FUNCTION ("getSDOHAndle" IN hWindow) .
	    
	    DYNAMIC-FUNCTION('findRowWhere':U IN hSDO,
              INPUT "CustNum",
              INPUT ttTasks.TaskNum,
              INPUT "EQ") .
	
		
		RETURN.

	END METHOD.




 

	DESTRUCTOR PUBLIC TaskPanelControl ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.

END CLASS.