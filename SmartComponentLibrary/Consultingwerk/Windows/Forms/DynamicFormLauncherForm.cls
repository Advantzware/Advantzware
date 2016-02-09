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
    File        : DynamicFormLauncherForm
    Purpose     : Provides an user interface to launch Forms
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jan 08 16:25:47 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Forms.BaseForm  FROM PROPATH .
USING Consultingwerk.Framework.*     FROM PROPATH . 
USING Progress.Lang.*                FROM PROPATH .


CLASS Consultingwerk.Windows.Forms.DynamicFormLauncherForm INHERITS BaseForm: 
    
    DEFINE PRIVATE VARIABLE bindingSource1 AS Progress.Data.BindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE toMdiChild AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraGrid1 AS Infragistics.Win.UltraWinGrid.UltraGrid NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor1 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton2 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton1 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    
    DEFINE VARIABLE lFirstTime AS LOGICAL NO-UNDO INIT TRUE .   
        
    {Consultingwerk/Util/TempTables/ttClassNames.i}

    DEFINE QUERY qry FOR ttClassNames SCROLLING .
        
    /*------------------------------------------------------------------------------
        Purpose: Returns the selected ClassName
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ClassName AS CHARACTER NO-UNDO 
    GET:
        IF AVAILABLE ttClassNames THEN 
            RETURN ttClassNames.ClassName . 
        ELSE 
            RETURN ? . 
    END GET . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the selected ClassName
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MdiChild AS LOGICAL NO-UNDO 
    GET:
        RETURN toMdiChild:Checked .
    END GET . 
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the DynamicFormLauncherForm class
        Notes:                                                                        
    ------------------------------------------------------------------------------*/        
    CONSTRUCTOR PUBLIC DynamicFormLauncherForm ():
        
        SUPER().
        InitializeComponent().

        Consultingwerk.Util.ClassHelper:UseExternalRuntimeForGetClassNames = TRUE . 
        
        Consultingwerk.Util.ClassHelper:GetClassNames ("System.Windows.Forms.Form":U,
                                                       OUTPUT TABLE ttClassNames) .
        
        OPEN QUERY qry PRESELECT EACH ttClassNames . 
        
        bindingSource1:Handle = QUERY qry:HANDLE .
        
        IF NOT VALID-OBJECT (FrameworkSettings:MdiContainer) THEN 
            toMdiChild:Enabled = FALSE . 
        
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
        tableDesc1 = NEW Progress.Data.TableDesc("Table":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("Table":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ClassName":U).
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
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Forms.DynamicFormLauncherForm":U).
        THIS-OBJECT:bindingSource1 = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
        THIS-OBJECT:ultraButton1 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraButton2 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraTextEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:toMdiChild = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:ultraGrid1 = NEW Infragistics.Win.UltraWinGrid.UltraGrid().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:toMdiChild, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraGrid1, System.ComponentModel.ISupportInitialize):BeginInit().
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
        DEFINE VARIABLE arrayvar1 AS Progress.Data.ColumnPropDesc EXTENT 1 NO-UNDO.
        arrayvar1[1] = NEW Progress.Data.ColumnPropDesc("ClassName":U, "Form Name":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar1.
        THIS-OBJECT:bindingSource1:TableSchema = tableDesc1.
        /*  */
        /* ultraButton1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:ultraButton1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButton1:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:ultraButton1:Location = NEW System.Drawing.Point(443, 279).
        THIS-OBJECT:ultraButton1:Name = "ultraButton1":U.
        THIS-OBJECT:ultraButton1:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:ultraButton1:TabIndex = 0.
        THIS-OBJECT:ultraButton1:Text = "&OK":U.
        /*  */
        /* ultraButton2 */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:ultraButton2:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButton2:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:ultraButton2:Location = NEW System.Drawing.Point(524, 279).
        THIS-OBJECT:ultraButton2:Name = "ultraButton2":U.
        THIS-OBJECT:ultraButton2:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:ultraButton2:TabIndex = 1.
        THIS-OBJECT:ultraButton2:Text = "&Cancel":U.
        /*  */
        /* ultraTextEditor1 */
        /*  */
        THIS-OBJECT:ultraTextEditor1:Location = NEW System.Drawing.Point(52, 8).
        THIS-OBJECT:ultraTextEditor1:Name = "ultraTextEditor1":U.
        THIS-OBJECT:ultraTextEditor1:Size = NEW System.Drawing.Size(547, 21).
        THIS-OBJECT:ultraTextEditor1:TabIndex = 2.
        THIS-OBJECT:ultraTextEditor1:TextChanged:Subscribe(THIS-OBJECT:ultraTextEditor1_TextChanged).
        THIS-OBJECT:ultraTextEditor1:KeyDown:Subscribe(THIS-OBJECT:ultraTextEditor1_KeyDown).
        /*  */
        /* ultraLabel1 */
        /*  */
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(12, 12).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel1:TabIndex = 3.
        THIS-OBJECT:ultraLabel1:Text = "Filter:":U.
        /*  */
        /* toMdiChild */
        /*  */
        THIS-OBJECT:toMdiChild:Location = NEW System.Drawing.Point(12, 281).
        THIS-OBJECT:toMdiChild:Name = "toMdiChild":U.
        THIS-OBJECT:toMdiChild:Size = NEW System.Drawing.Size(407, 20).
        THIS-OBJECT:toMdiChild:TabIndex = 4.
        THIS-OBJECT:toMdiChild:Text = "Start Form as MdiChild":U.
        /*  */
        /* ultraGrid1 */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraGrid1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraGrid1:DataSource = THIS-OBJECT:bindingSource1.
        appearance1:BackColor = System.Drawing.SystemColors:Window.
        appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Appearance = appearance1.
        THIS-OBJECT:ultraGrid1:DisplayLayout:AutoFitStyle = Infragistics.Win.UltraWinGrid.AutoFitStyle:ExtendLastColumn.
        ultraGridColumn1:Header:VisiblePosition = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS System.Object EXTENT 1 NO-UNDO.
        arrayvar2[1] = ultraGridColumn1.
        ultraGridBand1:Columns:AddRange(arrayvar2).
        THIS-OBJECT:ultraGrid1:DisplayLayout:BandsSerializer:Add(ultraGridBand1).
        THIS-OBJECT:ultraGrid1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:ultraGrid1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance2:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:ultraGrid1:DisplayLayout:GroupByBox:Appearance = appearance2.
        appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:ultraGrid1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
        THIS-OBJECT:ultraGrid1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance4:BackColor2 = System.Drawing.SystemColors:Control.
        appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:ultraGrid1:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
        THIS-OBJECT:ultraGrid1:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:ultraGrid1:DisplayLayout:MaxRowScrollRegions = 1.
        appearance5:BackColor = System.Drawing.SystemColors:Window.
        appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:ActiveCellAppearance = appearance5.
        appearance6:BackColor = System.Drawing.SystemColors:Highlight.
        appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:ActiveRowAppearance = appearance6.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance7:BackColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:CardAreaAppearance = appearance7.
        appearance8:BorderColor = System.Drawing.Color:Silver.
        appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:CellAppearance = appearance8.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:CellPadding = 0.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:ColumnAutoSizeMode = Infragistics.Win.UltraWinGrid.ColumnAutoSizeMode:AllRowsInBand.
        appearance9:BackColor = System.Drawing.SystemColors:Control.
        appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance9:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:GroupByRowAppearance = appearance9.
        appearance10:TextHAlignAsString = "Left":U.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:HeaderAppearance = appearance10.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance11:BackColor = System.Drawing.SystemColors:Window.
        appearance11:BorderColor = System.Drawing.Color:Silver.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:RowAppearance = appearance11.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:Single.
        appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
        THIS-OBJECT:ultraGrid1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:ultraGrid1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:ultraGrid1:DisplayLayout:TabNavigation = Infragistics.Win.UltraWinGrid.TabNavigation:NextControl.
        THIS-OBJECT:ultraGrid1:DisplayLayout:ViewStyle = Infragistics.Win.UltraWinGrid.ViewStyle:SingleBand.
        THIS-OBJECT:ultraGrid1:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:ultraGrid1:Location = NEW System.Drawing.Point(12, 41).
        THIS-OBJECT:ultraGrid1:Name = "ultraGrid1":U.
        THIS-OBJECT:ultraGrid1:Size = NEW System.Drawing.Size(587, 232).
        THIS-OBJECT:ultraGrid1:TabIndex = 5.
        THIS-OBJECT:ultraGrid1:Text = "ultraGrid1":U.
        THIS-OBJECT:ultraGrid1:DoubleClickRow:Subscribe(THIS-OBJECT:ultraGrid1_DoubleClickRow).
        /*  */
        /* DynamicFormLauncherForm */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:ultraButton1.
        THIS-OBJECT:CancelButton = THIS-OBJECT:ultraButton2.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(611, 316).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraGrid1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:toMdiChild).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTextEditor1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButton2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButton1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:MaximizeBox = FALSE.
        THIS-OBJECT:MinimizeBox = FALSE.
        THIS-OBJECT:MinimumSize = NEW System.Drawing.Size(627, 354).
        THIS-OBJECT:Name = "DynamicFormLauncherForm":U.
        THIS-OBJECT:ShowInTaskbar = FALSE.
        THIS-OBJECT:SizeGripStyle = System.Windows.Forms.SizeGripStyle:Show.
        THIS-OBJECT:Text = "Form Launcher":U.
        THIS-OBJECT:WindowPositionRegistryKey = "DynamicFormLauncherForm":U.
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:toMdiChild, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraGrid1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown event                                                                         
        Notes:                                                                        
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):
        
        SUPER:OnShown (e) .
        
        IF lFirstTime THEN DO:
            ASSIGN lFirstTime = FALSE . 
            
            ultraTextEditor1:Focus () .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the DoubleClickRow event of the ultraGrid1                                                                        
        Notes:                                                               
        @param sender The reference to the object that raised the event
        @param e The DoubleClickRowEventArgs with the data for this event          
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraGrid1_DoubleClickRow (sender AS System.Object, 
                                                   e AS Infragistics.Win.UltraWinGrid.DoubleClickRowEventArgs):
        
        THIS-OBJECT:DialogResult = System.Windows.Forms.DialogResult:OK . 
        
        THIS-OBJECT:Close () .
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the KeyDown event of the ultraTextEditor1                                                                        
        Notes:   Handles the Cursor Down key                                                                
        @param sender The reference to the object that raised the event
        @param e The KeyEventArgs with the data for this event          
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraTextEditor1_KeyDown (sender AS System.Object, 
                                                  e AS System.Windows.Forms.KeyEventArgs ):
        
        IF Progress.Util.EnumHelper:AreEqual (e:KeyCode,
                                              System.Windows.Forms.Keys:Down) THEN
                                               
            THIS-OBJECT:ultraGrid1:Focus () .         
            
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the TextChanged event of the ultraTextEditor1                                                                        
        Notes:                                                             
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event          
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraTextEditor1_TextChanged (sender AS System.Object, 
                                                      e AS System.EventArgs):
        
        DEFINE VARIABLE cFilter AS CHARACTER NO-UNDO.

        ASSIGN cFilter = ultraTextEditor1:Text .

        IF cFilter > "":U THEN 
            OPEN QUERY qry PRESELECT EACH ttClassNames WHERE ttClassNames.ClassName 
                        MATCHES SUBSTITUTE ("*&1*":U, cFilter).
        ELSE
            OPEN QUERY qry PRESELECT EACH ttClassNames . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor of the DynamicFormLauncherForm class
        Notes:                                                                        
    ------------------------------------------------------------------------------*/        
    DESTRUCTOR PUBLIC DynamicFormLauncherForm ():

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.