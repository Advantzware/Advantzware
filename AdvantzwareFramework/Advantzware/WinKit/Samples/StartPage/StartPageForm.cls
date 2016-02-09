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
    File        : StartPageForm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Sep 20 16:20:23 CEST 2009
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.* .
USING Advantzware.WinKit.Samples.* .
USING Progress.Lang.*.
USING Progress.Windows.Form.



CLASS Advantzware.WinKit.Samples.StartPage.StartPageForm INHERITS Form   : 
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE tableLayoutPanel1 AS System.Windows.Forms.TableLayoutPanel NO-UNDO.

    DEFINE PRIVATE VARIABLE ultraGauge1 AS Infragistics.Win.UltraWinGauge.UltraGauge NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraGroupBox1 AS Infragistics.Win.Misc.UltraGroupBox NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraGroupBox2 AS Infragistics.Win.Misc.UltraGroupBox NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraGroupBox3 AS Infragistics.Win.Misc.UltraGroupBox NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraGroupBox4 AS Infragistics.Win.Misc.UltraGroupBox NO-UNDO.


    DEFINE PRIVATE VARIABLE ultraDataSource1 AS Infragistics.Win.UltraWinDataSource.UltraDataSource NO-UNDO.

    DEFINE PRIVATE VARIABLE ultraChart1 AS Infragistics.Win.UltraWinChart.UltraChart NO-UNDO.


    DEFINE PRIVATE VARIABLE webBrowser1 AS System.Windows.Forms.WebBrowser NO-UNDO.

    DEFINE PRIVATE VARIABLE ultraExpandableGroupBox1 AS Infragistics.Win.Misc.UltraExpandableGroupBox NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraExpandableGroupBoxPanel1 AS Infragistics.Win.Misc.UltraExpandableGroupBoxPanel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor1 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.

    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.




		
	CONSTRUCTOR PUBLIC StartPageForm (  ):
		
		
        SUPER().
        InitializeComponent().
        InitChartData () .
        
        webBrowser1:Navigate("http://www.progress.com") .
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

	

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  	
    ------------------------------------------------------------------------------*/	
	METHOD PROTECTED VOID InitChartData(  ):		
		
        DEFINE VARIABLE oColumn AS Infragistics.Win.UltraWinDataSource.UltraDataColumn NO-UNDO.
        DEFINE VARIABLE oRow    AS Infragistics.Win.UltraWinDataSource.UltraDataRow    NO-UNDO.
		DEFINE VARIABLE cMonth  AS CHARACTER NO-UNDO INIT 
		    "January,February,March,April,May,June,July,August,September,October,November,December".
		DEFINE VARIABLE i AS INTEGER NO-UNDO.
		
        /* *** */
        ultraDataSource1:Band:Columns:Clear() .
                       
        oColumn = ultraDataSource1:Band:Columns:Add ("Month").
        oColumn:DataType = Progress.Util.TypeHelper:GetType("System.String").
        
        oColumn = ultraDataSource1:Band:Columns:Add (STRING(YEAR(TODAY))).
        oColumn:DataType = Progress.Util.TypeHelper:GetType("System.Decimal").

        oColumn = ultraDataSource1:Band:Columns:Add (STRING(YEAR(TODAY) - 1)).
        oColumn:DataType = Progress.Util.TypeHelper:GetType("System.Decimal").

        
        ultraDataSource1:Rows:Clear() .
        
        DO i = 1 TO 12:
            oRow = ultraDataSource1:Rows:Add() .
            oRow["Month"] = ENTRY(i, cMonth) .
            oRow[STRING(YEAR(TODAY))] = BOX(DECIMAL(RANDOM (10000, 100000))).
            oRow[STRING(YEAR(TODAY) - 1)] = BOX(DECIMAL(RANDOM (10000, 100000))).
        END.    

        ultraChart1:Data:DataBind() .
        		
		RETURN.

	END METHOD.


 	METHOD PRIVATE VOID InitializeComponent(  ):
		
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE resources AS System.Resources.ResXResourceSet NO-UNDO.
        resources = Progress.Util.ResourceHelper:Load("Advantzware\WinKit\Samples\StartPage\StartPageForm.resx", PROPATH).
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE paintElement1 AS Infragistics.UltraChart.Resources.Appearance.PaintElement NO-UNDO.
        paintElement1 = NEW Infragistics.UltraChart.Resources.Appearance.PaintElement().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE gradientEffect1 AS Infragistics.UltraChart.Resources.Appearance.GradientEffect NO-UNDO.
        gradientEffect1 = NEW Infragistics.UltraChart.Resources.Appearance.GradientEffect().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE view3DAppearance1 AS Infragistics.UltraChart.Resources.Appearance.View3DAppearance NO-UNDO.
        view3DAppearance1 = NEW Infragistics.UltraChart.Resources.Appearance.View3DAppearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE radialGauge1 AS Infragistics.UltraGauge.Resources.RadialGauge NO-UNDO.
        radialGauge1 = NEW Infragistics.UltraGauge.Resources.RadialGauge().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE brushElementGroup1 AS Infragistics.UltraGauge.Resources.BrushElementGroup NO-UNDO.
        brushElementGroup1 = NEW Infragistics.UltraGauge.Resources.BrushElementGroup().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE multiStopRadialGradientBrushElement1 AS Infragistics.UltraGauge.Resources.MultiStopRadialGradientBrushElement NO-UNDO.
        multiStopRadialGradientBrushElement1 = NEW Infragistics.UltraGauge.Resources.MultiStopRadialGradientBrushElement().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE colorStop1 AS Infragistics.UltraGauge.Resources.ColorStop NO-UNDO.
        colorStop1 = NEW Infragistics.UltraGauge.Resources.ColorStop().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE colorStop2 AS Infragistics.UltraGauge.Resources.ColorStop NO-UNDO.
        colorStop2 = NEW Infragistics.UltraGauge.Resources.ColorStop().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE colorStop3 AS Infragistics.UltraGauge.Resources.ColorStop NO-UNDO.
        colorStop3 = NEW Infragistics.UltraGauge.Resources.ColorStop().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE solidFillBrushElement1 AS Infragistics.UltraGauge.Resources.SolidFillBrushElement NO-UNDO.
        solidFillBrushElement1 = NEW Infragistics.UltraGauge.Resources.SolidFillBrushElement().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE multiStopRadialGradientBrushElement2 AS Infragistics.UltraGauge.Resources.MultiStopRadialGradientBrushElement NO-UNDO.
        multiStopRadialGradientBrushElement2 = NEW Infragistics.UltraGauge.Resources.MultiStopRadialGradientBrushElement().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE colorStop4 AS Infragistics.UltraGauge.Resources.ColorStop NO-UNDO.
        colorStop4 = NEW Infragistics.UltraGauge.Resources.ColorStop().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE colorStop5 AS Infragistics.UltraGauge.Resources.ColorStop NO-UNDO.
        colorStop5 = NEW Infragistics.UltraGauge.Resources.ColorStop().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE colorStop6 AS Infragistics.UltraGauge.Resources.ColorStop NO-UNDO.
        colorStop6 = NEW Infragistics.UltraGauge.Resources.ColorStop().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE colorStop7 AS Infragistics.UltraGauge.Resources.ColorStop NO-UNDO.
        colorStop7 = NEW Infragistics.UltraGauge.Resources.ColorStop().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE colorStop8 AS Infragistics.UltraGauge.Resources.ColorStop NO-UNDO.
        colorStop8 = NEW Infragistics.UltraGauge.Resources.ColorStop().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE colorStop9 AS Infragistics.UltraGauge.Resources.ColorStop NO-UNDO.
        colorStop9 = NEW Infragistics.UltraGauge.Resources.ColorStop().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE solidFillBrushElement2 AS Infragistics.UltraGauge.Resources.SolidFillBrushElement NO-UNDO.
        solidFillBrushElement2 = NEW Infragistics.UltraGauge.Resources.SolidFillBrushElement().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE solidFillBrushElement3 AS Infragistics.UltraGauge.Resources.SolidFillBrushElement NO-UNDO.
        solidFillBrushElement3 = NEW Infragistics.UltraGauge.Resources.SolidFillBrushElement().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE radialGaugeScale1 AS Infragistics.UltraGauge.Resources.RadialGaugeScale NO-UNDO.
        radialGaugeScale1 = NEW Infragistics.UltraGauge.Resources.RadialGaugeScale().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE numericAxis1 AS Infragistics.UltraGauge.Resources.NumericAxis NO-UNDO.
        numericAxis1 = NEW Infragistics.UltraGauge.Resources.NumericAxis().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE solidFillBrushElement4 AS Infragistics.UltraGauge.Resources.SolidFillBrushElement NO-UNDO.
        solidFillBrushElement4 = NEW Infragistics.UltraGauge.Resources.SolidFillBrushElement().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE solidFillBrushElement5 AS Infragistics.UltraGauge.Resources.SolidFillBrushElement NO-UNDO.
        solidFillBrushElement5 = NEW Infragistics.UltraGauge.Resources.SolidFillBrushElement().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE radialGaugeNeedle1 AS Infragistics.UltraGauge.Resources.RadialGaugeNeedle NO-UNDO.
        radialGaugeNeedle1 = NEW Infragistics.UltraGauge.Resources.RadialGaugeNeedle().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE solidFillBrushElement6 AS Infragistics.UltraGauge.Resources.SolidFillBrushElement NO-UNDO.
        solidFillBrushElement6 = NEW Infragistics.UltraGauge.Resources.SolidFillBrushElement().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE solidFillBrushElement7 AS Infragistics.UltraGauge.Resources.SolidFillBrushElement NO-UNDO.
        solidFillBrushElement7 = NEW Infragistics.UltraGauge.Resources.SolidFillBrushElement().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE simpleGradientBrushElement1 AS Infragistics.UltraGauge.Resources.SimpleGradientBrushElement NO-UNDO.
        simpleGradientBrushElement1 = NEW Infragistics.UltraGauge.Resources.SimpleGradientBrushElement().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE solidFillBrushElement8 AS Infragistics.UltraGauge.Resources.SolidFillBrushElement NO-UNDO.
        solidFillBrushElement8 = NEW Infragistics.UltraGauge.Resources.SolidFillBrushElement().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE solidFillBrushElement9 AS Infragistics.UltraGauge.Resources.SolidFillBrushElement NO-UNDO.
        solidFillBrushElement9 = NEW Infragistics.UltraGauge.Resources.SolidFillBrushElement().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE radialGaugeRange1 AS Infragistics.UltraGauge.Resources.RadialGaugeRange NO-UNDO.
        radialGaugeRange1 = NEW Infragistics.UltraGauge.Resources.RadialGaugeRange().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE solidFillBrushElement10 AS Infragistics.UltraGauge.Resources.SolidFillBrushElement NO-UNDO.
        solidFillBrushElement10 = NEW Infragistics.UltraGauge.Resources.SolidFillBrushElement().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE solidFillBrushElement11 AS Infragistics.UltraGauge.Resources.SolidFillBrushElement NO-UNDO.
        solidFillBrushElement11 = NEW Infragistics.UltraGauge.Resources.SolidFillBrushElement().
        THIS-OBJECT:tableLayoutPanel1 = NEW System.Windows.Forms.TableLayoutPanel().
        THIS-OBJECT:ultraGroupBox2 = NEW Infragistics.Win.Misc.UltraGroupBox().
        THIS-OBJECT:webBrowser1 = NEW System.Windows.Forms.WebBrowser().
        THIS-OBJECT:ultraGroupBox1 = NEW Infragistics.Win.Misc.UltraGroupBox().
        THIS-OBJECT:ultraExpandableGroupBox1 = NEW Infragistics.Win.Misc.UltraExpandableGroupBox().
        THIS-OBJECT:ultraExpandableGroupBoxPanel1 = NEW Infragistics.Win.Misc.UltraExpandableGroupBoxPanel().
        THIS-OBJECT:ultraTextEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel3 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraGroupBox3 = NEW Infragistics.Win.Misc.UltraGroupBox().
        THIS-OBJECT:ultraChart1 = NEW Infragistics.Win.UltraWinChart.UltraChart().
        THIS-OBJECT:ultraDataSource1 = NEW Infragistics.Win.UltraWinDataSource.UltraDataSource(THIS-OBJECT:components).
        THIS-OBJECT:ultraGroupBox4 = NEW Infragistics.Win.Misc.UltraGroupBox().
        THIS-OBJECT:ultraGauge1 = NEW Infragistics.Win.UltraWinGauge.UltraGauge().
        THIS-OBJECT:tableLayoutPanel1:SuspendLayout().
        CAST(THIS-OBJECT:ultraGroupBox2, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraGroupBox2:SuspendLayout().
        CAST(THIS-OBJECT:ultraGroupBox1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraGroupBox1:SuspendLayout().
        CAST(THIS-OBJECT:ultraExpandableGroupBox1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraExpandableGroupBox1:SuspendLayout().
        THIS-OBJECT:ultraExpandableGroupBoxPanel1:SuspendLayout().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraGroupBox3, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraGroupBox3:SuspendLayout().
        CAST(THIS-OBJECT:ultraChart1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraDataSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraGroupBox4, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraGroupBox4:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* tableLayoutPanel1 */
        /*  */
        THIS-OBJECT:tableLayoutPanel1:ColumnCount = 2.
        THIS-OBJECT:tableLayoutPanel1:ColumnStyles:Add(NEW System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType:Percent, 50)).
        THIS-OBJECT:tableLayoutPanel1:ColumnStyles:Add(NEW System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType:Percent, 50)).
        THIS-OBJECT:tableLayoutPanel1:Controls:Add(THIS-OBJECT:ultraGroupBox2, 1, 0).
        THIS-OBJECT:tableLayoutPanel1:Controls:Add(THIS-OBJECT:ultraGroupBox1, 0, 0).
        THIS-OBJECT:tableLayoutPanel1:Controls:Add(THIS-OBJECT:ultraGroupBox3, 0, 1).
        THIS-OBJECT:tableLayoutPanel1:Controls:Add(THIS-OBJECT:ultraGroupBox4, 1, 1).
        THIS-OBJECT:tableLayoutPanel1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:tableLayoutPanel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:tableLayoutPanel1:Name = "tableLayoutPanel1".
        THIS-OBJECT:tableLayoutPanel1:RowCount = 2.
        THIS-OBJECT:tableLayoutPanel1:RowStyles:Add(NEW System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType:Percent, 50)).
        THIS-OBJECT:tableLayoutPanel1:RowStyles:Add(NEW System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType:Percent, 50)).
        THIS-OBJECT:tableLayoutPanel1:Size = NEW System.Drawing.Size(506, 516).
        THIS-OBJECT:tableLayoutPanel1:TabIndex = 0.
        /*  */
        /* ultraGroupBox2 */
        /*  */
        THIS-OBJECT:ultraGroupBox2:BorderStyle = Infragistics.Win.Misc.GroupBoxBorderStyle:RectangularSolid.
        THIS-OBJECT:ultraGroupBox2:Controls:Add(THIS-OBJECT:webBrowser1).
        THIS-OBJECT:ultraGroupBox2:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraGroupBox2:HeaderPosition = Infragistics.Win.Misc.GroupBoxHeaderPosition:TopOutsideBorder.
        THIS-OBJECT:ultraGroupBox2:Location = NEW System.Drawing.Point(256, 3).
        THIS-OBJECT:ultraGroupBox2:Name = "ultraGroupBox2".
        THIS-OBJECT:ultraGroupBox2:Size = NEW System.Drawing.Size(247, 252).
        THIS-OBJECT:ultraGroupBox2:TabIndex = 1.
        THIS-OBJECT:ultraGroupBox2:Text = "Latest News".
        THIS-OBJECT:ultraGroupBox2:ViewStyle = Infragistics.Win.Misc.GroupBoxViewStyle:Office2007.
        /*  */
        /* webBrowser1 */
        /*  */
        THIS-OBJECT:webBrowser1:AllowNavigation = FALSE.
        THIS-OBJECT:webBrowser1:AllowWebBrowserDrop = FALSE.
        THIS-OBJECT:webBrowser1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:webBrowser1:IsWebBrowserContextMenuEnabled = FALSE.
        THIS-OBJECT:webBrowser1:Location = NEW System.Drawing.Point(1, 20).
        THIS-OBJECT:webBrowser1:MinimumSize = NEW System.Drawing.Size(20, 20).
        THIS-OBJECT:webBrowser1:Name = "webBrowser1".
        THIS-OBJECT:webBrowser1:ScriptErrorsSuppressed = TRUE.
        THIS-OBJECT:webBrowser1:Size = NEW System.Drawing.Size(245, 231).
        THIS-OBJECT:webBrowser1:TabIndex = 0.
        THIS-OBJECT:webBrowser1:Url = NEW System.Uri("http://www.progress.com", System.UriKind:Absolute).
        /*  */
        /* ultraGroupBox1 */
        /*  */
        THIS-OBJECT:ultraGroupBox1:BorderStyle = Infragistics.Win.Misc.GroupBoxBorderStyle:RectangularSolid.
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:ultraExpandableGroupBox1).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:ultraLabel3).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:ultraGroupBox1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraGroupBox1:HeaderPosition = Infragistics.Win.Misc.GroupBoxHeaderPosition:TopOutsideBorder.
        THIS-OBJECT:ultraGroupBox1:Location = NEW System.Drawing.Point(3, 3).
        THIS-OBJECT:ultraGroupBox1:Name = "ultraGroupBox1".
        THIS-OBJECT:ultraGroupBox1:Size = NEW System.Drawing.Size(247, 252).
        THIS-OBJECT:ultraGroupBox1:TabIndex = 0.
        THIS-OBJECT:ultraGroupBox1:Text = "My Activities".
        THIS-OBJECT:ultraGroupBox1:ViewStyle = Infragistics.Win.Misc.GroupBoxViewStyle:Office2007.
        /*  */
        /* ultraExpandableGroupBox1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraExpandableGroupBox1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraExpandableGroupBox1:Appearance = appearance1.
        THIS-OBJECT:ultraExpandableGroupBox1:BorderStyle = Infragistics.Win.Misc.GroupBoxBorderStyle:None.
        THIS-OBJECT:ultraExpandableGroupBox1:Controls:Add(THIS-OBJECT:ultraExpandableGroupBoxPanel1).
        THIS-OBJECT:ultraExpandableGroupBox1:Expanded = FALSE.
        THIS-OBJECT:ultraExpandableGroupBox1:ExpandedSize = NEW System.Drawing.Size(236, 50).
        THIS-OBJECT:ultraExpandableGroupBox1:HeaderClickAction = Infragistics.Win.Misc.GroupBoxHeaderClickAction:ToggleExpansion.
        appearance8:FontData:UnderlineAsString = "True".
        appearance8:ForeColor = System.Drawing.Color:Red.
        THIS-OBJECT:ultraExpandableGroupBox1:HeaderHotTrackingAppearance = appearance8.
        THIS-OBJECT:ultraExpandableGroupBox1:Location = NEW System.Drawing.Point(7, 23).
        THIS-OBJECT:ultraExpandableGroupBox1:Name = "ultraExpandableGroupBox1".
        THIS-OBJECT:ultraExpandableGroupBox1:Size = NEW System.Drawing.Size(236, 20).
        THIS-OBJECT:ultraExpandableGroupBox1:TabIndex = 0.
        THIS-OBJECT:ultraExpandableGroupBox1:Text = "Customer maintenance".
        THIS-OBJECT:ultraExpandableGroupBox1:UseHotTracking = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:ultraExpandableGroupBox1:ViewStyle = Infragistics.Win.Misc.GroupBoxViewStyle:Office2007.
        THIS-OBJECT:ultraExpandableGroupBox1:ExpandedStateChanged:SUBSCRIBE(THIS-OBJECT:ultraExpandableGroupBox1_ExpandedStateChanged).
        /*  */
        /* ultraExpandableGroupBoxPanel1 */
        /*  */
        THIS-OBJECT:ultraExpandableGroupBoxPanel1:Controls:Add(THIS-OBJECT:ultraTextEditor1).
        THIS-OBJECT:ultraExpandableGroupBoxPanel1:Location = NEW System.Drawing.Point(-10000, -10000).
        THIS-OBJECT:ultraExpandableGroupBoxPanel1:Name = "ultraExpandableGroupBoxPanel1".
        THIS-OBJECT:ultraExpandableGroupBoxPanel1:Size = NEW System.Drawing.Size(200, 100).
        THIS-OBJECT:ultraExpandableGroupBoxPanel1:TabIndex = 0.
        THIS-OBJECT:ultraExpandableGroupBoxPanel1:Visible = FALSE.
        /*  */
        /* ultraTextEditor1 */
        /*  */
        THIS-OBJECT:ultraTextEditor1:Location = NEW System.Drawing.Point(0, 3).
        THIS-OBJECT:ultraTextEditor1:Name = "ultraTextEditor1".
        THIS-OBJECT:ultraTextEditor1:Size = NEW System.Drawing.Size(230, 21).
        THIS-OBJECT:ultraTextEditor1:TabIndex = 0.
        THIS-OBJECT:ultraTextEditor1:Text = "Enter customer name".
        THIS-OBJECT:ultraTextEditor1:KeyDown:SUBSCRIBE(THIS-OBJECT:ultraTextEditor1_KeyDown).
        /*  */
        /* ultraLabel3 */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraLabel3:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance4:Image = CAST(resources:GetObject("appearance4.Image"), System.Object).
        appearance4:TextVAlignAsString = "Middle".
        THIS-OBJECT:ultraLabel3:Appearance = appearance4.
        appearance7:FontData:UnderlineAsString = "True".
        appearance7:ForeColor = System.Drawing.Color:Red.
        THIS-OBJECT:ultraLabel3:HotTrackAppearance = appearance7.
        THIS-OBJECT:ultraLabel3:Location = NEW System.Drawing.Point(13, 107).
        THIS-OBJECT:ultraLabel3:Name = "ultraLabel3".
        THIS-OBJECT:ultraLabel3:Size = NEW System.Drawing.Size(225, 23).
        THIS-OBJECT:ultraLabel3:TabIndex = 3.
        THIS-OBJECT:ultraLabel3:Text = "Another function".
        THIS-OBJECT:ultraLabel3:UseHotTracking = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:ultraLabel3:Click:SUBSCRIBE(THIS-OBJECT:ultraLabel1_Click).
        /*  */
        /* ultraLabel2 */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraLabel2:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance3:Image = CAST(resources:GetObject("appearance3.Image"), System.Object).
        appearance3:TextVAlignAsString = "Middle".
        THIS-OBJECT:ultraLabel2:Appearance = appearance3.
        appearance6:FontData:UnderlineAsString = "True".
        appearance6:ForeColor = System.Drawing.Color:Red.
        THIS-OBJECT:ultraLabel2:HotTrackAppearance = appearance6.
        THIS-OBJECT:ultraLabel2:Location = NEW System.Drawing.Point(13, 78).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2".
        THIS-OBJECT:ultraLabel2:Size = NEW System.Drawing.Size(225, 23).
        THIS-OBJECT:ultraLabel2:TabIndex = 2.
        THIS-OBJECT:ultraLabel2:Text = "Different customer maintenance".
        THIS-OBJECT:ultraLabel2:UseHotTracking = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:ultraLabel2:Click:SUBSCRIBE(THIS-OBJECT:ultraLabel1_Click).
        /*  */
        /* ultraLabel1 */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraLabel1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image"), System.Object).
        appearance2:TextVAlignAsString = "Middle".
        THIS-OBJECT:ultraLabel1:Appearance = appearance2.
        appearance5:FontData:UnderlineAsString = "True".
        appearance5:ForeColor = System.Drawing.Color:Red.
        THIS-OBJECT:ultraLabel1:HotTrackAppearance = appearance5.
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(13, 49).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1".
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(225, 23).
        THIS-OBJECT:ultraLabel1:TabIndex = 1.
        THIS-OBJECT:ultraLabel1:Text = "Customer maintenance".
        THIS-OBJECT:ultraLabel1:UseHotTracking = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:ultraLabel1:Click:SUBSCRIBE(THIS-OBJECT:ultraLabel1_Click).
        /*  */
        /* ultraGroupBox3 */
        /*  */
        THIS-OBJECT:ultraGroupBox3:BorderStyle = Infragistics.Win.Misc.GroupBoxBorderStyle:RectangularSolid.
        THIS-OBJECT:ultraGroupBox3:Controls:Add(THIS-OBJECT:ultraChart1).
        THIS-OBJECT:ultraGroupBox3:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraGroupBox3:HeaderPosition = Infragistics.Win.Misc.GroupBoxHeaderPosition:TopOutsideBorder.
        THIS-OBJECT:ultraGroupBox3:Location = NEW System.Drawing.Point(3, 261).
        THIS-OBJECT:ultraGroupBox3:Name = "ultraGroupBox3".
        THIS-OBJECT:ultraGroupBox3:Size = NEW System.Drawing.Size(247, 252).
        THIS-OBJECT:ultraGroupBox3:TabIndex = 2.
        THIS-OBJECT:ultraGroupBox3:Text = "Statistics".
        THIS-OBJECT:ultraGroupBox3:ViewStyle = Infragistics.Win.Misc.GroupBoxViewStyle:Office2007.
        /* 
        'UltraChart' properties's serialization: Since 'ChartType' changes the way axes look,
        'ChartType' must be persisted ahead of any Axes change made in design time.
         */
        THIS-OBJECT:ultraChart1:ChartType = Infragistics.UltraChart.Shared.Styles.ChartType:LineChart3D.
        /*  */
        /* ultraChart1 */
        /*  */
        THIS-OBJECT:ultraChart1:Axis:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(255)), System.Convert:ToInt32(System.Convert:ToByte(248)), System.Convert:ToInt32(System.Convert:ToByte(220))).
        paintElement1:ElementType = Infragistics.UltraChart.Shared.Styles.PaintElementType:None.
        paintElement1:Fill = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(255)), System.Convert:ToInt32(System.Convert:ToByte(248)), System.Convert:ToInt32(System.Convert:ToByte(220))).
        THIS-OBJECT:ultraChart1:Axis:PE = paintElement1.
        THIS-OBJECT:ultraChart1:Axis:X:Extent = 91.
        THIS-OBJECT:ultraChart1:Axis:X:Labels:Font = NEW System.Drawing.Font("Verdana", 7).
        THIS-OBJECT:ultraChart1:Axis:X:Labels:FontColor = System.Drawing.Color:DimGray.
        THIS-OBJECT:ultraChart1:Axis:X:Labels:HorizontalAlign = System.Drawing.StringAlignment:Near.
        THIS-OBJECT:ultraChart1:Axis:X:Labels:ItemFormatString = "<ITEM_LABEL>".
        THIS-OBJECT:ultraChart1:Axis:X:Labels:Layout:Behavior = Infragistics.UltraChart.Shared.Styles.AxisLabelLayoutBehaviors:Auto.
        THIS-OBJECT:ultraChart1:Axis:X:Labels:Orientation = Infragistics.UltraChart.Shared.Styles.TextOrientation:VerticalLeftFacing.
        THIS-OBJECT:ultraChart1:Axis:X:Labels:SeriesLabels:Font = NEW System.Drawing.Font("Verdana", 7).
        THIS-OBJECT:ultraChart1:Axis:X:Labels:SeriesLabels:FontColor = System.Drawing.Color:DimGray.
        THIS-OBJECT:ultraChart1:Axis:X:Labels:SeriesLabels:FormatString = "".
        THIS-OBJECT:ultraChart1:Axis:X:Labels:SeriesLabels:HorizontalAlign = System.Drawing.StringAlignment:Near.
        THIS-OBJECT:ultraChart1:Axis:X:Labels:SeriesLabels:Layout:Behavior = Infragistics.UltraChart.Shared.Styles.AxisLabelLayoutBehaviors:Auto.
        THIS-OBJECT:ultraChart1:Axis:X:Labels:SeriesLabels:Orientation = Infragistics.UltraChart.Shared.Styles.TextOrientation:VerticalLeftFacing.
        THIS-OBJECT:ultraChart1:Axis:X:Labels:SeriesLabels:VerticalAlign = System.Drawing.StringAlignment:Center.
        THIS-OBJECT:ultraChart1:Axis:X:Labels:VerticalAlign = System.Drawing.StringAlignment:Center.
        THIS-OBJECT:ultraChart1:Axis:X:LineThickness = 1.
        THIS-OBJECT:ultraChart1:Axis:X:MajorGridLines:AlphaLevel = System.Convert:ToByte(255).
        THIS-OBJECT:ultraChart1:Axis:X:MajorGridLines:Color = System.Drawing.Color:Gainsboro.
        THIS-OBJECT:ultraChart1:Axis:X:MajorGridLines:DrawStyle = Infragistics.UltraChart.Shared.Styles.LineDrawStyle:Dot.
        THIS-OBJECT:ultraChart1:Axis:X:MajorGridLines:Visible = TRUE.
        THIS-OBJECT:ultraChart1:Axis:X:MinorGridLines:AlphaLevel = System.Convert:ToByte(255).
        THIS-OBJECT:ultraChart1:Axis:X:MinorGridLines:Color = System.Drawing.Color:LightGray.
        THIS-OBJECT:ultraChart1:Axis:X:MinorGridLines:DrawStyle = Infragistics.UltraChart.Shared.Styles.LineDrawStyle:Dot.
        THIS-OBJECT:ultraChart1:Axis:X:MinorGridLines:Visible = TRUE.
        THIS-OBJECT:ultraChart1:Axis:X:TickmarkStyle = Infragistics.UltraChart.Shared.Styles.AxisTickStyle:Smart.
        THIS-OBJECT:ultraChart1:Axis:X:Visible = TRUE.
        THIS-OBJECT:ultraChart1:Axis:X2:Labels:Font = NEW System.Drawing.Font("Verdana", 7).
        THIS-OBJECT:ultraChart1:Axis:X2:Labels:FontColor = System.Drawing.Color:Gray.
        THIS-OBJECT:ultraChart1:Axis:X2:Labels:HorizontalAlign = System.Drawing.StringAlignment:Far.
        THIS-OBJECT:ultraChart1:Axis:X2:Labels:ItemFormatString = "<ITEM_LABEL>".
        THIS-OBJECT:ultraChart1:Axis:X2:Labels:Layout:Behavior = Infragistics.UltraChart.Shared.Styles.AxisLabelLayoutBehaviors:Auto.
        THIS-OBJECT:ultraChart1:Axis:X2:Labels:Orientation = Infragistics.UltraChart.Shared.Styles.TextOrientation:VerticalLeftFacing.
        THIS-OBJECT:ultraChart1:Axis:X2:Labels:SeriesLabels:Font = NEW System.Drawing.Font("Verdana", 7).
        THIS-OBJECT:ultraChart1:Axis:X2:Labels:SeriesLabels:FontColor = System.Drawing.Color:Gray.
        THIS-OBJECT:ultraChart1:Axis:X2:Labels:SeriesLabels:FormatString = "".
        THIS-OBJECT:ultraChart1:Axis:X2:Labels:SeriesLabels:HorizontalAlign = System.Drawing.StringAlignment:Far.
        THIS-OBJECT:ultraChart1:Axis:X2:Labels:SeriesLabels:Layout:Behavior = Infragistics.UltraChart.Shared.Styles.AxisLabelLayoutBehaviors:Auto.
        THIS-OBJECT:ultraChart1:Axis:X2:Labels:SeriesLabels:Orientation = Infragistics.UltraChart.Shared.Styles.TextOrientation:VerticalLeftFacing.
        THIS-OBJECT:ultraChart1:Axis:X2:Labels:SeriesLabels:VerticalAlign = System.Drawing.StringAlignment:Center.
        THIS-OBJECT:ultraChart1:Axis:X2:Labels:VerticalAlign = System.Drawing.StringAlignment:Center.
        THIS-OBJECT:ultraChart1:Axis:X2:Labels:Visible = FALSE.
        THIS-OBJECT:ultraChart1:Axis:X2:LineThickness = 1.
        THIS-OBJECT:ultraChart1:Axis:X2:MajorGridLines:AlphaLevel = System.Convert:ToByte(255).
        THIS-OBJECT:ultraChart1:Axis:X2:MajorGridLines:Color = System.Drawing.Color:Gainsboro.
        THIS-OBJECT:ultraChart1:Axis:X2:MajorGridLines:DrawStyle = Infragistics.UltraChart.Shared.Styles.LineDrawStyle:Dot.
        THIS-OBJECT:ultraChart1:Axis:X2:MajorGridLines:Visible = TRUE.
        THIS-OBJECT:ultraChart1:Axis:X2:MinorGridLines:AlphaLevel = System.Convert:ToByte(255).
        THIS-OBJECT:ultraChart1:Axis:X2:MinorGridLines:Color = System.Drawing.Color:LightGray.
        THIS-OBJECT:ultraChart1:Axis:X2:MinorGridLines:DrawStyle = Infragistics.UltraChart.Shared.Styles.LineDrawStyle:Dot.
        THIS-OBJECT:ultraChart1:Axis:X2:MinorGridLines:Visible = FALSE.
        THIS-OBJECT:ultraChart1:Axis:X2:TickmarkStyle = Infragistics.UltraChart.Shared.Styles.AxisTickStyle:Smart.
        THIS-OBJECT:ultraChart1:Axis:X2:Visible = FALSE.
        THIS-OBJECT:ultraChart1:Axis:Y:Labels:Font = NEW System.Drawing.Font("Verdana", 7).
        THIS-OBJECT:ultraChart1:Axis:Y:Labels:FontColor = System.Drawing.Color:DimGray.
        THIS-OBJECT:ultraChart1:Axis:Y:Labels:HorizontalAlign = System.Drawing.StringAlignment:Far.
        THIS-OBJECT:ultraChart1:Axis:Y:Labels:ItemFormatString = "<DATA_VALUE:00.##>".
        THIS-OBJECT:ultraChart1:Axis:Y:Labels:Layout:Behavior = Infragistics.UltraChart.Shared.Styles.AxisLabelLayoutBehaviors:Auto.
        THIS-OBJECT:ultraChart1:Axis:Y:Labels:Orientation = Infragistics.UltraChart.Shared.Styles.TextOrientation:Horizontal.
        THIS-OBJECT:ultraChart1:Axis:Y:Labels:SeriesLabels:Font = NEW System.Drawing.Font("Verdana", 7).
        THIS-OBJECT:ultraChart1:Axis:Y:Labels:SeriesLabels:FontColor = System.Drawing.Color:DimGray.
        THIS-OBJECT:ultraChart1:Axis:Y:Labels:SeriesLabels:FormatString = "".
        THIS-OBJECT:ultraChart1:Axis:Y:Labels:SeriesLabels:HorizontalAlign = System.Drawing.StringAlignment:Far.
        THIS-OBJECT:ultraChart1:Axis:Y:Labels:SeriesLabels:Layout:Behavior = Infragistics.UltraChart.Shared.Styles.AxisLabelLayoutBehaviors:Auto.
        THIS-OBJECT:ultraChart1:Axis:Y:Labels:SeriesLabels:Orientation = Infragistics.UltraChart.Shared.Styles.TextOrientation:Horizontal.
        THIS-OBJECT:ultraChart1:Axis:Y:Labels:SeriesLabels:VerticalAlign = System.Drawing.StringAlignment:Center.
        THIS-OBJECT:ultraChart1:Axis:Y:Labels:VerticalAlign = System.Drawing.StringAlignment:Center.
        THIS-OBJECT:ultraChart1:Axis:Y:LineThickness = 1.
        THIS-OBJECT:ultraChart1:Axis:Y:MajorGridLines:AlphaLevel = System.Convert:ToByte(255).
        THIS-OBJECT:ultraChart1:Axis:Y:MajorGridLines:Color = System.Drawing.Color:Gainsboro.
        THIS-OBJECT:ultraChart1:Axis:Y:MajorGridLines:DrawStyle = Infragistics.UltraChart.Shared.Styles.LineDrawStyle:Dot.
        THIS-OBJECT:ultraChart1:Axis:Y:MajorGridLines:Visible = TRUE.
        THIS-OBJECT:ultraChart1:Axis:Y:MinorGridLines:AlphaLevel = System.Convert:ToByte(255).
        THIS-OBJECT:ultraChart1:Axis:Y:MinorGridLines:Color = System.Drawing.Color:LightGray.
        THIS-OBJECT:ultraChart1:Axis:Y:MinorGridLines:DrawStyle = Infragistics.UltraChart.Shared.Styles.LineDrawStyle:Dot.
        THIS-OBJECT:ultraChart1:Axis:Y:MinorGridLines:Visible = TRUE.
        THIS-OBJECT:ultraChart1:Axis:Y:TickmarkInterval = 10.
        THIS-OBJECT:ultraChart1:Axis:Y:TickmarkStyle = Infragistics.UltraChart.Shared.Styles.AxisTickStyle:Smart.
        THIS-OBJECT:ultraChart1:Axis:Y:Visible = TRUE.
        THIS-OBJECT:ultraChart1:Axis:Y2:Labels:Font = NEW System.Drawing.Font("Verdana", 7).
        THIS-OBJECT:ultraChart1:Axis:Y2:Labels:FontColor = System.Drawing.Color:Gray.
        THIS-OBJECT:ultraChart1:Axis:Y2:Labels:HorizontalAlign = System.Drawing.StringAlignment:Near.
        THIS-OBJECT:ultraChart1:Axis:Y2:Labels:ItemFormatString = "<DATA_VALUE:00.##>".
        THIS-OBJECT:ultraChart1:Axis:Y2:Labels:Layout:Behavior = Infragistics.UltraChart.Shared.Styles.AxisLabelLayoutBehaviors:Auto.
        THIS-OBJECT:ultraChart1:Axis:Y2:Labels:Orientation = Infragistics.UltraChart.Shared.Styles.TextOrientation:Horizontal.
        THIS-OBJECT:ultraChart1:Axis:Y2:Labels:SeriesLabels:Font = NEW System.Drawing.Font("Verdana", 7).
        THIS-OBJECT:ultraChart1:Axis:Y2:Labels:SeriesLabels:FontColor = System.Drawing.Color:Gray.
        THIS-OBJECT:ultraChart1:Axis:Y2:Labels:SeriesLabels:FormatString = "".
        THIS-OBJECT:ultraChart1:Axis:Y2:Labels:SeriesLabels:HorizontalAlign = System.Drawing.StringAlignment:Near.
        THIS-OBJECT:ultraChart1:Axis:Y2:Labels:SeriesLabels:Layout:Behavior = Infragistics.UltraChart.Shared.Styles.AxisLabelLayoutBehaviors:Auto.
        THIS-OBJECT:ultraChart1:Axis:Y2:Labels:SeriesLabels:Orientation = Infragistics.UltraChart.Shared.Styles.TextOrientation:Horizontal.
        THIS-OBJECT:ultraChart1:Axis:Y2:Labels:SeriesLabels:VerticalAlign = System.Drawing.StringAlignment:Center.
        THIS-OBJECT:ultraChart1:Axis:Y2:Labels:VerticalAlign = System.Drawing.StringAlignment:Center.
        THIS-OBJECT:ultraChart1:Axis:Y2:Labels:Visible = FALSE.
        THIS-OBJECT:ultraChart1:Axis:Y2:LineThickness = 1.
        THIS-OBJECT:ultraChart1:Axis:Y2:MajorGridLines:AlphaLevel = System.Convert:ToByte(255).
        THIS-OBJECT:ultraChart1:Axis:Y2:MajorGridLines:Color = System.Drawing.Color:Gainsboro.
        THIS-OBJECT:ultraChart1:Axis:Y2:MajorGridLines:DrawStyle = Infragistics.UltraChart.Shared.Styles.LineDrawStyle:Dot.
        THIS-OBJECT:ultraChart1:Axis:Y2:MajorGridLines:Visible = TRUE.
        THIS-OBJECT:ultraChart1:Axis:Y2:MinorGridLines:AlphaLevel = System.Convert:ToByte(255).
        THIS-OBJECT:ultraChart1:Axis:Y2:MinorGridLines:Color = System.Drawing.Color:LightGray.
        THIS-OBJECT:ultraChart1:Axis:Y2:MinorGridLines:DrawStyle = Infragistics.UltraChart.Shared.Styles.LineDrawStyle:Dot.
        THIS-OBJECT:ultraChart1:Axis:Y2:MinorGridLines:Visible = FALSE.
        THIS-OBJECT:ultraChart1:Axis:Y2:TickmarkInterval = 10.
        THIS-OBJECT:ultraChart1:Axis:Y2:TickmarkStyle = Infragistics.UltraChart.Shared.Styles.AxisTickStyle:Smart.
        THIS-OBJECT:ultraChart1:Axis:Y2:Visible = FALSE.
        THIS-OBJECT:ultraChart1:Axis:Z:Labels:Font = NEW System.Drawing.Font("Verdana", 7).
        THIS-OBJECT:ultraChart1:Axis:Z:Labels:FontColor = System.Drawing.Color:DimGray.
        THIS-OBJECT:ultraChart1:Axis:Z:Labels:HorizontalAlign = System.Drawing.StringAlignment:Far.
        THIS-OBJECT:ultraChart1:Axis:Z:Labels:ItemFormatString = "<ITEM_LABEL>".
        THIS-OBJECT:ultraChart1:Axis:Z:Labels:Layout:Behavior = Infragistics.UltraChart.Shared.Styles.AxisLabelLayoutBehaviors:Auto.
        THIS-OBJECT:ultraChart1:Axis:Z:Labels:Orientation = Infragistics.UltraChart.Shared.Styles.TextOrientation:Horizontal.
        THIS-OBJECT:ultraChart1:Axis:Z:Labels:SeriesLabels:Font = NEW System.Drawing.Font("Verdana", 7).
        THIS-OBJECT:ultraChart1:Axis:Z:Labels:SeriesLabels:FontColor = System.Drawing.Color:DimGray.
        THIS-OBJECT:ultraChart1:Axis:Z:Labels:SeriesLabels:HorizontalAlign = System.Drawing.StringAlignment:Far.
        THIS-OBJECT:ultraChart1:Axis:Z:Labels:SeriesLabels:Layout:Behavior = Infragistics.UltraChart.Shared.Styles.AxisLabelLayoutBehaviors:Auto.
        THIS-OBJECT:ultraChart1:Axis:Z:Labels:SeriesLabels:Orientation = Infragistics.UltraChart.Shared.Styles.TextOrientation:Horizontal.
        THIS-OBJECT:ultraChart1:Axis:Z:Labels:SeriesLabels:VerticalAlign = System.Drawing.StringAlignment:Center.
        THIS-OBJECT:ultraChart1:Axis:Z:Labels:VerticalAlign = System.Drawing.StringAlignment:Center.
        THIS-OBJECT:ultraChart1:Axis:Z:LineThickness = 1.
        THIS-OBJECT:ultraChart1:Axis:Z:MajorGridLines:AlphaLevel = System.Convert:ToByte(255).
        THIS-OBJECT:ultraChart1:Axis:Z:MajorGridLines:Color = System.Drawing.Color:Gainsboro.
        THIS-OBJECT:ultraChart1:Axis:Z:MajorGridLines:DrawStyle = Infragistics.UltraChart.Shared.Styles.LineDrawStyle:Dot.
        THIS-OBJECT:ultraChart1:Axis:Z:MajorGridLines:Visible = TRUE.
        THIS-OBJECT:ultraChart1:Axis:Z:MinorGridLines:AlphaLevel = System.Convert:ToByte(255).
        THIS-OBJECT:ultraChart1:Axis:Z:MinorGridLines:Color = System.Drawing.Color:LightGray.
        THIS-OBJECT:ultraChart1:Axis:Z:MinorGridLines:DrawStyle = Infragistics.UltraChart.Shared.Styles.LineDrawStyle:Dot.
        THIS-OBJECT:ultraChart1:Axis:Z:MinorGridLines:Visible = FALSE.
        THIS-OBJECT:ultraChart1:Axis:Z:TickmarkStyle = Infragistics.UltraChart.Shared.Styles.AxisTickStyle:Smart.
        THIS-OBJECT:ultraChart1:Axis:Z:Visible = TRUE.
        THIS-OBJECT:ultraChart1:Axis:Z2:Labels:Font = NEW System.Drawing.Font("Verdana", 7).
        THIS-OBJECT:ultraChart1:Axis:Z2:Labels:FontColor = System.Drawing.Color:Gray.
        THIS-OBJECT:ultraChart1:Axis:Z2:Labels:HorizontalAlign = System.Drawing.StringAlignment:Near.
        THIS-OBJECT:ultraChart1:Axis:Z2:Labels:ItemFormatString = "<ITEM_LABEL>".
        THIS-OBJECT:ultraChart1:Axis:Z2:Labels:Layout:Behavior = Infragistics.UltraChart.Shared.Styles.AxisLabelLayoutBehaviors:Auto.
        THIS-OBJECT:ultraChart1:Axis:Z2:Labels:Orientation = Infragistics.UltraChart.Shared.Styles.TextOrientation:Horizontal.
        THIS-OBJECT:ultraChart1:Axis:Z2:Labels:SeriesLabels:Font = NEW System.Drawing.Font("Verdana", 7).
        THIS-OBJECT:ultraChart1:Axis:Z2:Labels:SeriesLabels:FontColor = System.Drawing.Color:Gray.
        THIS-OBJECT:ultraChart1:Axis:Z2:Labels:SeriesLabels:HorizontalAlign = System.Drawing.StringAlignment:Near.
        THIS-OBJECT:ultraChart1:Axis:Z2:Labels:SeriesLabels:Layout:Behavior = Infragistics.UltraChart.Shared.Styles.AxisLabelLayoutBehaviors:Auto.
        THIS-OBJECT:ultraChart1:Axis:Z2:Labels:SeriesLabels:Orientation = Infragistics.UltraChart.Shared.Styles.TextOrientation:Horizontal.
        THIS-OBJECT:ultraChart1:Axis:Z2:Labels:SeriesLabels:VerticalAlign = System.Drawing.StringAlignment:Center.
        THIS-OBJECT:ultraChart1:Axis:Z2:Labels:VerticalAlign = System.Drawing.StringAlignment:Center.
        THIS-OBJECT:ultraChart1:Axis:Z2:Labels:Visible = FALSE.
        THIS-OBJECT:ultraChart1:Axis:Z2:LineThickness = 1.
        THIS-OBJECT:ultraChart1:Axis:Z2:MajorGridLines:AlphaLevel = System.Convert:ToByte(255).
        THIS-OBJECT:ultraChart1:Axis:Z2:MajorGridLines:Color = System.Drawing.Color:Gainsboro.
        THIS-OBJECT:ultraChart1:Axis:Z2:MajorGridLines:DrawStyle = Infragistics.UltraChart.Shared.Styles.LineDrawStyle:Dot.
        THIS-OBJECT:ultraChart1:Axis:Z2:MajorGridLines:Visible = TRUE.
        THIS-OBJECT:ultraChart1:Axis:Z2:MinorGridLines:AlphaLevel = System.Convert:ToByte(255).
        THIS-OBJECT:ultraChart1:Axis:Z2:MinorGridLines:Color = System.Drawing.Color:LightGray.
        THIS-OBJECT:ultraChart1:Axis:Z2:MinorGridLines:DrawStyle = Infragistics.UltraChart.Shared.Styles.LineDrawStyle:Dot.
        THIS-OBJECT:ultraChart1:Axis:Z2:MinorGridLines:Visible = FALSE.
        THIS-OBJECT:ultraChart1:Axis:Z2:TickmarkStyle = Infragistics.UltraChart.Shared.Styles.AxisTickStyle:Smart.
        THIS-OBJECT:ultraChart1:Axis:Z2:Visible = FALSE.
        THIS-OBJECT:ultraChart1:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Center.
        THIS-OBJECT:ultraChart1:Border:Color = System.Drawing.Color:Transparent.
        THIS-OBJECT:ultraChart1:ColorModel:AlphaLevel = System.Convert:ToByte(255).
        THIS-OBJECT:ultraChart1:ColorModel:ColorBegin = System.Drawing.Color:Gold.
        THIS-OBJECT:ultraChart1:ColorModel:ColorEnd = System.Drawing.Color:DarkRed.
        THIS-OBJECT:ultraChart1:ColorModel:ModelStyle = Infragistics.UltraChart.Shared.Styles.ColorModels:CustomLinear.
        THIS-OBJECT:ultraChart1:ColorModel:Scaling = Infragistics.UltraChart.Shared.Styles.ColorScaling:Oscillating.
        THIS-OBJECT:ultraChart1:Data:DataMember = "Band 0".
        THIS-OBJECT:ultraChart1:Data:SwapRowsAndColumns = TRUE.
        THIS-OBJECT:ultraChart1:Data:UseRowLabelsColumn = TRUE.
        THIS-OBJECT:ultraChart1:DataMember = "Band 0".
        THIS-OBJECT:ultraChart1:DataSource = THIS-OBJECT:ultraDataSource1.
        THIS-OBJECT:ultraChart1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraChart1:Effects:Effects:Add(gradientEffect1).
        THIS-OBJECT:ultraChart1:Legend:Location = Infragistics.UltraChart.Shared.Styles.LegendLocation:Left.
        THIS-OBJECT:ultraChart1:Legend:SpanPercentage = 10.
        THIS-OBJECT:ultraChart1:Location = NEW System.Drawing.Point(1, 20).
        THIS-OBJECT:ultraChart1:Name = "ultraChart1".
        THIS-OBJECT:ultraChart1:Size = NEW System.Drawing.Size(245, 231).
        THIS-OBJECT:ultraChart1:TabIndex = 1.
        THIS-OBJECT:ultraChart1:TitleTop:Font = NEW System.Drawing.Font("Microsoft Sans Serif", 12, System.Drawing.FontStyle:Bold).
        THIS-OBJECT:ultraChart1:TitleTop:HorizontalAlign = System.Drawing.StringAlignment:Center.
        THIS-OBJECT:ultraChart1:TitleTop:Text = "Revenue chart".
        THIS-OBJECT:ultraChart1:Tooltips:HighlightFillColor = System.Drawing.Color:DimGray.
        THIS-OBJECT:ultraChart1:Tooltips:HighlightOutlineColor = System.Drawing.Color:DarkGray.
        view3DAppearance1:Perspective = 100.
        view3DAppearance1:Scale = 70.
        view3DAppearance1:XRotation = 147.
        view3DAppearance1:YRotation = 28.
        THIS-OBJECT:ultraChart1:Transform3D = view3DAppearance1.
        /*  */
        /* ultraGroupBox4 */
        /*  */
        THIS-OBJECT:ultraGroupBox4:BorderStyle = Infragistics.Win.Misc.GroupBoxBorderStyle:RectangularSolid.
        THIS-OBJECT:ultraGroupBox4:Controls:Add(THIS-OBJECT:ultraGauge1).
        THIS-OBJECT:ultraGroupBox4:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraGroupBox4:HeaderPosition = Infragistics.Win.Misc.GroupBoxHeaderPosition:TopOutsideBorder.
        THIS-OBJECT:ultraGroupBox4:Location = NEW System.Drawing.Point(256, 261).
        THIS-OBJECT:ultraGroupBox4:Name = "ultraGroupBox4".
        THIS-OBJECT:ultraGroupBox4:Size = NEW System.Drawing.Size(247, 252).
        THIS-OBJECT:ultraGroupBox4:TabIndex = 3.
        THIS-OBJECT:ultraGroupBox4:Text = "Performance".
        THIS-OBJECT:ultraGroupBox4:ViewStyle = Infragistics.Win.Misc.GroupBoxViewStyle:Office2007.
        /*  */
        /* ultraGauge1 */
        /*  */
        THIS-OBJECT:ultraGauge1:BackColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:ultraGauge1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraGauge1:ForeColor = System.Drawing.SystemColors:ControlLightLight.
        multiStopRadialGradientBrushElement1:CenterPoint = CAST(resources:GetObject("multiStopRadialGradientBrushElement1.CenterPoint"), System.Drawing.PointF).
        colorStop1:Color = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(161)), System.Convert:ToInt32(System.Convert:ToByte(161)), System.Convert:ToInt32(System.Convert:ToByte(161))).
        colorStop2:Color = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(96)), System.Convert:ToInt32(System.Convert:ToByte(96)), System.Convert:ToInt32(System.Convert:ToByte(96))).
        colorStop2:Stop = 0.4758621.
        colorStop3:Color = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(89)), System.Convert:ToInt32(System.Convert:ToByte(89)), System.Convert:ToInt32(System.Convert:ToByte(89))).
        colorStop3:Stop = 1.
        multiStopRadialGradientBrushElement1:ColorStops:Add(colorStop1).
        multiStopRadialGradientBrushElement1:ColorStops:Add(colorStop2).
        multiStopRadialGradientBrushElement1:ColorStops:Add(colorStop3).
        multiStopRadialGradientBrushElement1:FocusScales = CAST(resources:GetObject("multiStopRadialGradientBrushElement1.FocusScales"), System.Drawing.PointF).
        solidFillBrushElement1:Color = System.Drawing.Color:Gray.
        solidFillBrushElement1:RelativeBounds = NEW System.Drawing.Rectangle(4, 4, 93, 93).
        solidFillBrushElement1:RelativeBoundsMeasure = Infragistics.UltraGauge.Resources.Measure:Percent.
        multiStopRadialGradientBrushElement2:CenterPoint = CAST(resources:GetObject("multiStopRadialGradientBrushElement2.CenterPoint"), System.Drawing.PointF).
        colorStop4:Color = System.Drawing.Color:Transparent.
        colorStop5:Color = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(141)), System.Convert:ToInt32(System.Convert:ToByte(141)), System.Convert:ToInt32(System.Convert:ToByte(141))).
        colorStop5:Stop = 0.062069.
        colorStop6:Color = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(82)), System.Convert:ToInt32(System.Convert:ToByte(82)), System.Convert:ToInt32(System.Convert:ToByte(82))).
        colorStop6:Stop = 0.0758621.
        colorStop7:Color = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(45)), System.Convert:ToInt32(System.Convert:ToByte(45)), System.Convert:ToInt32(System.Convert:ToByte(45))).
        colorStop7:Stop = 0.1448276.
        colorStop8:Color = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(30)), System.Convert:ToInt32(System.Convert:ToByte(30)), System.Convert:ToInt32(System.Convert:ToByte(30))).
        colorStop8:Stop = 0.2448276.
        colorStop9:Color = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(60)), System.Convert:ToInt32(System.Convert:ToByte(60)), System.Convert:ToInt32(System.Convert:ToByte(60))).
        colorStop9:Stop = 1.
        multiStopRadialGradientBrushElement2:ColorStops:Add(colorStop4).
        multiStopRadialGradientBrushElement2:ColorStops:Add(colorStop5).
        multiStopRadialGradientBrushElement2:ColorStops:Add(colorStop6).
        multiStopRadialGradientBrushElement2:ColorStops:Add(colorStop7).
        multiStopRadialGradientBrushElement2:ColorStops:Add(colorStop8).
        multiStopRadialGradientBrushElement2:ColorStops:Add(colorStop9).
        multiStopRadialGradientBrushElement2:FocusScales = CAST(resources:GetObject("multiStopRadialGradientBrushElement2.FocusScales"), System.Drawing.PointF).
        multiStopRadialGradientBrushElement2:RelativeBounds = NEW System.Drawing.Rectangle(4, 4, 93, 93).
        multiStopRadialGradientBrushElement2:RelativeBoundsMeasure = Infragistics.UltraGauge.Resources.Measure:Percent.
        brushElementGroup1:BrushElements:Add(multiStopRadialGradientBrushElement1).
        brushElementGroup1:BrushElements:Add(solidFillBrushElement1).
        brushElementGroup1:BrushElements:Add(multiStopRadialGradientBrushElement2).
        radialGauge1:Dial:BrushElements:Add(brushElementGroup1).
        solidFillBrushElement2:Color = System.Drawing.Color:Silver.
        radialGauge1:Dial:StrokeElement:BrushElements:Add(solidFillBrushElement2).
        radialGauge1:OverDial:BrushElements:Add(solidFillBrushElement3).
        numericAxis1:EndValue = 100.
        radialGaugeScale1:Axes:Add(numericAxis1).
        radialGaugeScale1:EndAngle = 405.
        solidFillBrushElement4:Color = System.Drawing.Color:White.
        radialGaugeScale1:Labels:BrushElements:Add(solidFillBrushElement4).
        radialGaugeScale1:Labels:Extent = 70.
        radialGaugeScale1:Labels:Font = NEW System.Drawing.Font("Arial", 12, System.Drawing.FontStyle:Bold, System.Drawing.GraphicsUnit:Pixel).
        radialGaugeScale1:Labels:Frequency = 20.
        radialGaugeScale1:Labels:Orientation = Infragistics.UltraGauge.Resources.RadialLabelOrientation:Horizontal.
        radialGaugeScale1:Labels:SpanMaximum = 18.
        solidFillBrushElement5:Color = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(189)), System.Convert:ToInt32(System.Convert:ToByte(189)), System.Convert:ToInt32(System.Convert:ToByte(189))).
        radialGaugeScale1:MajorTickmarks:BrushElements:Add(solidFillBrushElement5).
        radialGaugeScale1:MajorTickmarks:EndExtent = 60.
        radialGaugeScale1:MajorTickmarks:EndWidth = 3.
        radialGaugeScale1:MajorTickmarks:Frequency = 10.
        radialGaugeScale1:MajorTickmarks:StartExtent = 50.
        radialGaugeScale1:MajorTickmarks:StartWidth = 3.
        radialGaugeNeedle1:Anchor:BrushElements:Add(solidFillBrushElement6).
        radialGaugeNeedle1:Anchor:Radius = 6.
        radialGaugeNeedle1:Anchor:StrokeElement:BrushElements:Add(solidFillBrushElement7).
        radialGaugeNeedle1:Anchor:StrokeElement:Thickness = 2.
        simpleGradientBrushElement1:EndColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(208)), System.Convert:ToInt32(System.Convert:ToByte(86)), System.Convert:ToInt32(System.Convert:ToByte(51))).
        simpleGradientBrushElement1:GradientStyle = Infragistics.UltraGauge.Resources.Gradient:Rectangular.
        simpleGradientBrushElement1:StartColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(255)), System.Convert:ToInt32(System.Convert:ToByte(128)), System.Convert:ToInt32(System.Convert:ToByte(0))).
        radialGaugeNeedle1:BrushElements:Add(simpleGradientBrushElement1).
        radialGaugeNeedle1:EndExtent = 50.
        radialGaugeNeedle1:EndWidth = 1.
        radialGaugeNeedle1:MidExtent = 40.
        radialGaugeNeedle1:MidWidth = 5.
        radialGaugeNeedle1:StartWidth = 3.
        radialGaugeNeedle1:StrokeElement:Thickness = 0.
        radialGaugeNeedle1:Value = 80.
        radialGaugeNeedle1:WidthMeasure = Infragistics.UltraGauge.Resources.Measure:Percent.
        radialGaugeScale1:Markers:Add(radialGaugeNeedle1).
        solidFillBrushElement8:Color = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(240)), System.Convert:ToInt32(System.Convert:ToByte(240)), System.Convert:ToInt32(System.Convert:ToByte(240))).
        radialGaugeScale1:MinorTickmarks:BrushElements:Add(solidFillBrushElement8).
        radialGaugeScale1:MinorTickmarks:EndExtent = 52.
        radialGaugeScale1:MinorTickmarks:EndWidth = 1.
        radialGaugeScale1:MinorTickmarks:Frequency = 5.
        radialGaugeScale1:MinorTickmarks:StartExtent = 58.
        solidFillBrushElement9:Color = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(135)), System.Convert:ToInt32(System.Convert:ToByte(135)), System.Convert:ToInt32(System.Convert:ToByte(135))).
        radialGaugeScale1:MinorTickmarks:StrokeElement:BrushElements:Add(solidFillBrushElement9).
        solidFillBrushElement10:Color = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(20)), System.Convert:ToInt32(System.Convert:ToByte(255)), System.Convert:ToInt32(System.Convert:ToByte(255)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        radialGaugeRange1:BrushElements:Add(solidFillBrushElement10).
        radialGaugeRange1:EndValue = 100.
        radialGaugeRange1:InnerExtentEnd = 50.
        radialGaugeRange1:InnerExtentStart = 50.
        radialGaugeRange1:OuterExtent = 60.
        radialGaugeRange1:Shadow:BrushElements:Add(solidFillBrushElement11).
        radialGaugeRange1:StartValue = 0.
        radialGaugeScale1:Ranges:Add(radialGaugeRange1).
        radialGaugeScale1:StartAngle = 135.
        radialGauge1:Scales:Add(radialGaugeScale1).
        THIS-OBJECT:ultraGauge1:Gauges:Add(radialGauge1).
        THIS-OBJECT:ultraGauge1:Location = NEW System.Drawing.Point(1, 20).
        THIS-OBJECT:ultraGauge1:Name = "ultraGauge1".
        THIS-OBJECT:ultraGauge1:Size = NEW System.Drawing.Size(245, 231).
        THIS-OBJECT:ultraGauge1:TabIndex = 0.
        /*  */
        /* StartPageForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(506, 516).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tableLayoutPanel1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon"), System.Drawing.Icon).
        THIS-OBJECT:Name = "StartPageForm".
        THIS-OBJECT:Text = "Welcome page".
        THIS-OBJECT:tableLayoutPanel1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraGroupBox2, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraGroupBox2:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraGroupBox1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraGroupBox1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraExpandableGroupBox1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraExpandableGroupBox1:ResumeLayout(FALSE).
        THIS-OBJECT:ultraExpandableGroupBoxPanel1:ResumeLayout(FALSE).
        THIS-OBJECT:ultraExpandableGroupBoxPanel1:PerformLayout().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraGroupBox3, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraGroupBox3:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraChart1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraDataSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraGroupBox4, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraGroupBox4:ResumeLayout(FALSE).
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
	METHOD PRIVATE VOID ultraExpandableGroupBox1_ExpandedStateChanged( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	
	    IF ultraExpandableGroupBox1:Expanded THEN
            ultraTextEditor1:Focus() . 
	    		
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ultraLabel1_Click (INPUT sender AS System.Object, 
	                                       INPUT e AS System.EventArgs ):
		
		DEFINE VARIABLE oContainer AS SampleMDIContainer NO-UNDO . 
		
		IF VALID-OBJECT (FrameworkSettings:MdiContainer) AND 
		   TYPE-OF (FrameworkSettings:MdiContainer, SampleMDIContainer) THEN DO:
		       
            oContainer = CAST (FrameworkSettings:MdiContainer, SampleMDIContainer) .
            
            IF sender = ultraLabel1 THEN 
                oContainer:LaunchWindow ("customer":U) .            
            ELSE IF sender = ultraLabel2 THEN 
                oContainer:LaunchWindow ("customersmart":U) .            
            ELSE IF sender = ultraLabel3 THEN 
                oContainer:LaunchWindow ("wlargesmart":U) .            
                
		END.
		
	END METHOD.


 	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ultraTextEditor1_KeyDown(sender AS System.Object, 
	                                             e AS System.Windows.Forms.KeyEventArgs ):
		
		IF Progress.Util.EnumHelper:AreEqual(e:KeyCode, System.Windows.Forms.Keys:Enter) THEN DO:
    	    DEFINE VARIABLE hWindow AS HANDLE NO-UNDO.
    	    DEFINE VARIABLE hSDO    AS HANDLE NO-UNDO.
    	
    	    RUN src/demo/customersmart.w PERSISTENT SET hWindow .
    	    RUN initializeObject IN hWindow . 
    	    
    	    ASSIGN hSDO = DYNAMIC-FUNCTION ("getSDOHAndle" IN hWindow) .
    	        	    
    	    DYNAMIC-FUNCTION('assignQuerySelection':U IN hSDO,
                  INPUT "Name",
                  INPUT ultraTextEditor1:Text,
                  INPUT "BEGINS") .
                  
            DYNAMIC-FUNCTION ("openQuery":U IN hSDO) .                  
		END. 

	END METHOD.




 

	DESTRUCTOR PUBLIC StartPageForm ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.

END CLASS.