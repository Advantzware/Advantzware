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
    File        : DataRelationFieldPickerForm
    Purpose     : Provides an user interface to associate fields for a 
                  data-relation
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Nov 25 20:52:04 CET 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.* FROM PROPATH .
USING Consultingwerk.Util.*                 FROM PROPATH . 
USING Consultingwerk.Windows.Util.Forms.*   FROM PROPATH.
USING Progress.Lang.*                       FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.BusinessEntityDesigner.UI.DataRelationFieldPickerForm 
    INHERITS SmartWindowForm: 
    
    DEFINE PRIVATE VARIABLE m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE tableLayoutPanel1 AS System.Windows.Forms.TableLayoutPanel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton2 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton1 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton3 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraListView3 AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraListView2 AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraListView1 AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraToolbarsManager1 AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the buffer handle of the child table eTable buffer                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ChildTableBuffer AS HANDLE NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the handle of the dsBusinessEntity dataset                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Dataset AS HANDLE NO-UNDO 
    GET.
    SET. 
                
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the buffer handle of the parent table eTable buffer                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ParentTableBuffer AS HANDLE NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current relation-fields for the relation                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RelationFields AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DataRelationFieldPickerForm class                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DataRelationFieldPickerForm ():
        
        SUPER().
        InitializeComponent().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Creates a related field pair                                                                       
        Notes:    
        @param pcParentFieldName  The parent table field name
        @param pcChildFieldName The child table field name                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID CreateRelatedFields (pcParentFieldName AS CHARACTER,
                                               pcChildFieldName AS CHARACTER):
        
        DEFINE VARIABLE oParentItem AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO . 
        DEFINE VARIABLE oChildItem  AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO . 
                
        DEFINE VARIABLE oRelationItem AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO .        
    
        IF NOT ultraListView1:Items:Exists (pcParentFieldName) THEN DO:
            
            MessageFormHelper:ShowMessage (SUBSTITUTE ("Relation field &1 in parent table is no longer valid."{&TRAN}, pcParentFieldName),
                                           THIS-OBJECT:Text,
                                           MessageFormImages:ImageWarning) .
            
            RETURN . 
        END.
        
        IF NOT ultraListView2:Items:Exists (pcChildFieldName) THEN DO:
            
            MessageFormHelper:ShowMessage (SUBSTITUTE ("Relation field &1 in child table is no longer valid."{&TRAN}, pcParentFieldName),
                                           THIS-OBJECT:Text,
                                           MessageFormImages:ImageWarning) .
            
            RETURN . 
        END.
                
        oParentItem = ultraListView1:Items [pcParentFieldName] .
        oChildItem  = ultraListView2:Items [pcChildFieldName] .
                
        ultraListView1:Items:Remove(oParentItem) .
        ultraListView2:Items:Remove(oChildItem) .

        oRelationItem = NEW Infragistics.Win.UltraWinListView.UltraListViewItem (SUBSTITUTE ("&1,&2":U, 
                                                                                             pcParentFieldName, 
                                                                                             pcChildFieldName)) .

        oRelationItem:Value = SUBSTITUTE ("&1 -> &2":U, 
                                          pcParentFieldName, 
                                          pcChildFieldName) . 

        ultraListView3:Items:Add (oRelationItem) .

    END METHOD.

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
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Remove":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool1 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool1 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("JoinPoupMenu":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Remove":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool2 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool2 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("LeftTable":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("LeftMapAutomatically":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE popupMenuTool3 AS Infragistics.Win.UltraWinToolbars.PopupMenuTool NO-UNDO.
        popupMenuTool3 = NEW Infragistics.Win.UltraWinToolbars.PopupMenuTool("RightTable":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("RightMapAutomatically":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("LeftMapAutomatically":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
        buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("RightMapAutomatically":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.BusinessEntityDesigner.UI.DataRelationFieldPickerForm":U).
        THIS-OBJECT:ultraTabPageControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:tableLayoutPanel1 = NEW System.Windows.Forms.TableLayoutPanel().
        THIS-OBJECT:ultraListView1 = NEW Infragistics.Win.UltraWinListView.UltraListView().
        THIS-OBJECT:ultraListView2 = NEW Infragistics.Win.UltraWinListView.UltraListView().
        THIS-OBJECT:ultraListView3 = NEW Infragistics.Win.UltraWinListView.UltraListView().
        THIS-OBJECT:ultraButton3 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraButton2 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraButton1 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraTabControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
        THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
        THIS-OBJECT:ultraToolbarsManager1 = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsManager(THIS-OBJECT:components).
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
        THIS-OBJECT:ultraTabPageControl1:SuspendLayout().
        THIS-OBJECT:tableLayoutPanel1:SuspendLayout().
        CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraListView2, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraListView3, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabControl1:SuspendLayout().
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraTabPageControl1 */
        /*  */
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:tableLayoutPanel1).
        THIS-OBJECT:ultraTabPageControl1:Location = NEW System.Drawing.Point(1, 23).
        THIS-OBJECT:ultraTabPageControl1:Name = "ultraTabPageControl1":U.
        THIS-OBJECT:ultraTabPageControl1:Size = NEW System.Drawing.Size(639, 341).
        /*  */
        /* tableLayoutPanel1 */
        /*  */
        THIS-OBJECT:tableLayoutPanel1:BackColor = System.Drawing.SystemColors:ButtonHighlight.
        THIS-OBJECT:tableLayoutPanel1:ColumnCount = 3.
        THIS-OBJECT:tableLayoutPanel1:ColumnStyles:Add(NEW System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType:Percent, Progress.Util.CastUtil:ToSingle(50))).
        THIS-OBJECT:tableLayoutPanel1:ColumnStyles:Add(NEW System.Windows.Forms.ColumnStyle()).
        THIS-OBJECT:tableLayoutPanel1:ColumnStyles:Add(NEW System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType:Percent, Progress.Util.CastUtil:ToSingle(50))).
        THIS-OBJECT:tableLayoutPanel1:Controls:Add(THIS-OBJECT:ultraListView1, 0, 1).
        THIS-OBJECT:tableLayoutPanel1:Controls:Add(THIS-OBJECT:ultraListView2, 2, 1).
        THIS-OBJECT:tableLayoutPanel1:Controls:Add(THIS-OBJECT:ultraListView3, 0, 2).
        THIS-OBJECT:tableLayoutPanel1:Controls:Add(THIS-OBJECT:ultraButton3, 1, 1).
        THIS-OBJECT:tableLayoutPanel1:Controls:Add(THIS-OBJECT:ultraLabel1, 0, 0).
        THIS-OBJECT:tableLayoutPanel1:Controls:Add(THIS-OBJECT:ultraLabel2, 2, 0).
        THIS-OBJECT:tableLayoutPanel1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:tableLayoutPanel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:tableLayoutPanel1:Name = "tableLayoutPanel1":U.
        THIS-OBJECT:tableLayoutPanel1:RowCount = 3.
        THIS-OBJECT:tableLayoutPanel1:RowStyles:Add(NEW System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType:Absolute, Progress.Util.CastUtil:ToSingle(20))).
        THIS-OBJECT:tableLayoutPanel1:RowStyles:Add(NEW System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType:Percent, Progress.Util.CastUtil:ToSingle(66.6666565))).
        THIS-OBJECT:tableLayoutPanel1:RowStyles:Add(NEW System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType:Percent, Progress.Util.CastUtil:ToSingle(33.3333397))).
        THIS-OBJECT:tableLayoutPanel1:Size = NEW System.Drawing.Size(639, 341).
        THIS-OBJECT:tableLayoutPanel1:TabIndex = 0.
        /*  */
        /* ultraListView1 */
        /*  */
        THIS-OBJECT:ultraListView1:AllowDrop = TRUE.
        THIS-OBJECT:ultraToolbarsManager1:SetContextMenuUltra(THIS-OBJECT:ultraListView1, "LeftTable":U).
        THIS-OBJECT:ultraListView1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraListView1:ItemSettings:HideSelection = FALSE.
        THIS-OBJECT:ultraListView1:ItemSettings:SelectionType = Infragistics.Win.UltraWinListView.SelectionType:Single.
        THIS-OBJECT:ultraListView1:Location = NEW System.Drawing.Point(3, 23).
        THIS-OBJECT:ultraListView1:Name = "ultraListView1":U.
        THIS-OBJECT:ultraListView1:Size = NEW System.Drawing.Size(273, 207).
        THIS-OBJECT:ultraListView1:TabIndex = 0.
        THIS-OBJECT:ultraListView1:Text = "ultraListView1":U.
        THIS-OBJECT:ultraListView1:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:List.
        THIS-OBJECT:ultraListView1:ViewSettingsList:ColumnWidth = 200.
        THIS-OBJECT:ultraListView1:DragDrop:Subscribe(THIS-OBJECT:ultraListView1_DragDrop).
        THIS-OBJECT:ultraListView1:DragOver:Subscribe(THIS-OBJECT:ultraListView1_DragOver).
        THIS-OBJECT:ultraListView1:MouseDown:Subscribe(THIS-OBJECT:ultraListView1_MouseDown).
        /*  */
        /* ultraListView2 */
        /*  */
        THIS-OBJECT:ultraListView2:AllowDrop = TRUE.
        THIS-OBJECT:ultraToolbarsManager1:SetContextMenuUltra(THIS-OBJECT:ultraListView2, "RightTable":U).
        THIS-OBJECT:ultraListView2:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraListView2:ItemSettings:HideSelection = FALSE.
        THIS-OBJECT:ultraListView2:ItemSettings:SelectionType = Infragistics.Win.UltraWinListView.SelectionType:Single.
        THIS-OBJECT:ultraListView2:Location = NEW System.Drawing.Point(363, 23).
        THIS-OBJECT:ultraListView2:Name = "ultraListView2":U.
        THIS-OBJECT:ultraListView2:Size = NEW System.Drawing.Size(273, 207).
        THIS-OBJECT:ultraListView2:TabIndex = 1.
        THIS-OBJECT:ultraListView2:Text = "ultraListView2":U.
        THIS-OBJECT:ultraListView2:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:List.
        THIS-OBJECT:ultraListView2:ViewSettingsList:ColumnWidth = 200.
        THIS-OBJECT:ultraListView2:DragDrop:Subscribe(THIS-OBJECT:ultraListView2_DragDrop).
        THIS-OBJECT:ultraListView2:DragOver:Subscribe(THIS-OBJECT:ultraListView2_DragOver).
        THIS-OBJECT:ultraListView2:MouseDown:Subscribe(THIS-OBJECT:ultraListView2_MouseDown).
        /*  */
        /* ultraListView3 */
        /*  */
        THIS-OBJECT:tableLayoutPanel1:SetColumnSpan(THIS-OBJECT:ultraListView3, 3).
        THIS-OBJECT:ultraToolbarsManager1:SetContextMenuUltra(THIS-OBJECT:ultraListView3, "JoinPoupMenu":U).
        THIS-OBJECT:ultraListView3:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraListView3:ItemSettings:DefaultImage = CAST(resources:GetObject("ultraListView3.ItemSettings.DefaultImage":U), System.Drawing.Image).
        THIS-OBJECT:ultraListView3:Location = NEW System.Drawing.Point(3, 236).
        THIS-OBJECT:ultraListView3:Name = "ultraListView3":U.
        THIS-OBJECT:ultraListView3:Size = NEW System.Drawing.Size(633, 102).
        THIS-OBJECT:ultraListView3:TabIndex = 2.
        THIS-OBJECT:ultraListView3:Text = "ultraListView3":U.
        THIS-OBJECT:ultraListView3:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:List.
        THIS-OBJECT:ultraListView3:ViewSettingsList:ColumnWidth = 300.
        THIS-OBJECT:ultraListView3:KeyDown:Subscribe(THIS-OBJECT:ultraListView3_KeyDown).
        /*  */
        /* ultraButton3 */
        /*  */
        THIS-OBJECT:ultraButton3:Anchor = System.Windows.Forms.AnchorStyles:None.
        appearance4:Image = CAST(resources:GetObject("appearance4.Image":U), System.Object).
        THIS-OBJECT:ultraButton3:Appearance = appearance4.
        THIS-OBJECT:ultraButton3:Location = NEW System.Drawing.Point(282, 113).
        THIS-OBJECT:ultraButton3:Name = "ultraButton3":U.
        THIS-OBJECT:ultraButton3:Size = NEW System.Drawing.Size(75, 27).
        THIS-OBJECT:ultraButton3:TabIndex = 3.
        THIS-OBJECT:ultraButton3:Text = "Join":U.
        THIS-OBJECT:ultraButton3:Click:Subscribe(THIS-OBJECT:ultraButton3_Click).
        /*  */
        /* ultraLabel1 */
        /*  */
        THIS-OBJECT:ultraLabel1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(3, 3).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(273, 14).
        THIS-OBJECT:ultraLabel1:TabIndex = 4.
        THIS-OBJECT:ultraLabel1:Text = "ultraLabel1":U.
        /*  */
        /* ultraLabel2 */
        /*  */
        THIS-OBJECT:ultraLabel2:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraLabel2:Location = NEW System.Drawing.Point(363, 3).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        THIS-OBJECT:ultraLabel2:Size = NEW System.Drawing.Size(273, 14).
        THIS-OBJECT:ultraLabel2:TabIndex = 5.
        THIS-OBJECT:ultraLabel2:Text = "ultraLabel2":U.
        /*  */
        /* ultraButton2 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:ultraButton2:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButton2:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:ultraButton2:Location = NEW System.Drawing.Point(580, 385).
        THIS-OBJECT:ultraButton2:Name = "ultraButton2":U.
        THIS-OBJECT:ultraButton2:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:ultraButton2:TabIndex = 4.
        THIS-OBJECT:ultraButton2:Text = "&Cancel":U.
        /*  */
        /* ultraButton1 */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:ultraButton1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButton1:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:ultraButton1:Location = NEW System.Drawing.Point(499, 385).
        THIS-OBJECT:ultraButton1:Name = "ultraButton1":U.
        THIS-OBJECT:ultraButton1:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:ultraButton1:TabIndex = 3.
        THIS-OBJECT:ultraButton1:Text = "&OK":U.
        THIS-OBJECT:ultraButton1:Click:Subscribe(THIS-OBJECT:ultraButton1_Click).
        /*  */
        /* ultraTabControl1 */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTabControl1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl1).
        THIS-OBJECT:ultraTabControl1:Location = NEW System.Drawing.Point(12, 12).
        THIS-OBJECT:ultraTabControl1:Name = "ultraTabControl1":U.
        THIS-OBJECT:ultraTabControl1:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
        THIS-OBJECT:ultraTabControl1:Size = NEW System.Drawing.Size(643, 367).
        THIS-OBJECT:ultraTabControl1:TabIndex = 5.
        ultraTab1:TabPage = THIS-OBJECT:ultraTabPageControl1.
        ultraTab1:Text = "Fields":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 1 NO-UNDO.
        arrayvar0[1] = ultraTab1.
        THIS-OBJECT:ultraTabControl1:Tabs:AddRange(arrayvar0).
        /*  */
        /* ultraTabSharedControlsPage1 */
        /*  */
        THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
        THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1":U.
        THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(639, 341).
        /*  */
        /* ultraToolbarsManager1 */
        /*  */
        THIS-OBJECT:ultraToolbarsManager1:DesignerFlags = 1.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainer = THIS-OBJECT.
        THIS-OBJECT:ultraToolbarsManager1:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Consultingwerk.SmartComponents.Base.SmartWindowForm":U).
        THIS-OBJECT:ultraToolbarsManager1:ShowFullMenusDelay = 500.
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        buttonTool1:SharedPropsInternal:AppearancesSmall:Appearance = appearance1.
        buttonTool1:SharedPropsInternal:Caption = "Remove":U.
        popupMenuTool1:SharedPropsInternal:Caption = "JoinPoupMenu":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar1[1] = buttonTool2.
        popupMenuTool1:Tools:AddRange(arrayvar1).
        popupMenuTool2:SharedPropsInternal:Caption = "LeftTable":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar2[1] = buttonTool5.
        popupMenuTool2:Tools:AddRange(arrayvar2).
        popupMenuTool3:SharedPropsInternal:Caption = "RightTable":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 1 NO-UNDO.
        arrayvar3[1] = buttonTool6.
        popupMenuTool3:Tools:AddRange(arrayvar3).
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        buttonTool3:SharedPropsInternal:AppearancesSmall:Appearance = appearance2.
        buttonTool3:SharedPropsInternal:Caption = "Map automatically":U.
        appearance3:Image = CAST(resources:GetObject("appearance3.Image":U), System.Object).
        buttonTool4:SharedPropsInternal:AppearancesSmall:Appearance = appearance3.
        buttonTool4:SharedPropsInternal:Caption = "Map automatically":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 6 NO-UNDO.
        arrayvar4[1] = buttonTool1.
        arrayvar4[2] = popupMenuTool1.
        arrayvar4[3] = popupMenuTool2.
        arrayvar4[4] = popupMenuTool3.
        arrayvar4[5] = buttonTool3.
        arrayvar4[6] = buttonTool4.
        THIS-OBJECT:ultraToolbarsManager1:Tools:AddRange(arrayvar4).
        THIS-OBJECT:ultraToolbarsManager1:ToolClick:Subscribe(THIS-OBJECT:ultraToolbarsManager1_ToolClick).
        /*  */
        /* m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Left */
        /*  */
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Left:BackColor = System.Drawing.SystemColors:Control.
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 25).
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Left:Name = "m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Left":U.
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(0, 395).
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Right */
        /*  */
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Right:BackColor = System.Drawing.SystemColors:Control.
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(667, 25).
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Right:Name = "m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Right":U.
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(0, 395).
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Top */
        /*  */
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Top:BackColor = System.Drawing.SystemColors:Control.
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Top:Name = "m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Top":U.
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(667, 25).
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Bottom */
        /*  */
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.SystemColors:Control.
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 420).
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Bottom:Name = "m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Bottom":U.
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(667, 0).
        THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:ultraToolbarsManager1.
        /*  */
        /* DataRelationFieldPickerForm */
        /*  */
        THIS-OBJECT:CancelButton = THIS-OBJECT:ultraButton2.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(667, 420).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTabControl1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButton2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButton1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Left).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Right).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Bottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_DataRelationFieldPickerForm_Toolbars_Dock_Area_Top).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:MaximizeBox = FALSE.
        THIS-OBJECT:MinimizeBox = FALSE.
        THIS-OBJECT:Name = "DataRelationFieldPickerForm":U.
        THIS-OBJECT:Text = "DATA-RELATION Editor":U.
        THIS-OBJECT:WindowPositionRegistryKey = "DataRelationFieldPickerForm":U.
        THIS-OBJECT:ultraTabPageControl1:ResumeLayout(FALSE).
        THIS-OBJECT:tableLayoutPanel1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraListView2, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraListView3, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabControl1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraToolbarsManager1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Automatically maps the currently selected entry on the left table
                 to an entry on the right table 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID LeftMapAutomatically ():
		
		DEFINE VARIABLE cKey AS CHARACTER NO-UNDO.

        IF ultraListView1:SelectedItems:Count <> 1 THEN DO:
            Consultingwerk.Util.MessageFormHelper:ShowMessage ("Please select one item before using the auto mapping function!"{&TRAN},
                                                               Consultingwerk.Windows.Util.Forms.MessageFormImages:ImageWarning) . 
                                                                
            RETURN .                                                                 
        END.

        ASSIGN cKey = ultraListView1:SelectedItems[0]:Key .
        
        IF NOT ultraListView2:Items:Exists (cKey) THEN DO:
            Consultingwerk.Util.MessageFormHelper:ShowMessage (SUBSTITUTE ("The field &1 has no field available for auto mapping!"{&TRAN}, cKey),
                                                                Consultingwerk.Windows.Util.Forms.MessageFormImages:ImageWarning) . 
                                                                
            RETURN .                                                                 
        END.
        
        THIS-OBJECT:CreateRelatedFields (cKey, cKey) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event                                                                       
        Notes:                        
        @param e The System.EventArgs with the data for this event                                                
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):
        
        DEFINE VARIABLE hChildBuffer AS HANDLE               NO-UNDO.
        DEFINE VARIABLE hTableBuffer AS HANDLE               NO-UNDO.
        DEFINE VARIABLE hQuery       AS HANDLE               NO-UNDO.
        DEFINE VARIABLE oImage       AS System.Drawing.Image NO-UNDO .
        DEFINE VARIABLE i            AS INTEGER              NO-UNDO.
        
        FILE-INFO:FILE-NAME = "Consultingwerk\BusinessEntityDesigner\UI\Images\table2_selection_column.png":U .
        
        IF FILE-INFO:FULL-PATHNAME > "":U THEN 
            oImage = Consultingwerk.Util.ImageHelper:Load (FILE-INFO:FULL-PATHNAME) .
        
        SUPER:OnLoad (e) . 
        
        ultraLabel1:Text = THIS-OBJECT:ParentTableBuffer::TempTableName . 
        ultraLabel2:Text = THIS-OBJECT:ChildTableBuffer::TempTableName . 
        
        ASSIGN hQuery = Consultingwerk.Util.QueryHelper:CreatePreparedQuery 
              (THIS-OBJECT:Dataset::eField,
               SUBSTITUTE ("FOR EACH eField WHERE BusinessEntityName = '&1' AND TempTableName = '&2'":U,
                           THIS-OBJECT:ChildTableBuffer::BusinessEntityName,
                           THIS-OBJECT:ChildTableBuffer::TempTableName)) .
        
        DO WHILE NOT hQuery:QUERY-OFF-END:
            
            ultraListView2:Items:Add (THIS-OBJECT:DATASET::eField::FieldName,
                                      THIS-OBJECT:DATASET::eField::FieldName):Appearance:Image = oImage .
            
            hQuery:GET-NEXT () . 
        END.
        
        DELETE OBJECT hQuery .

        ASSIGN hQuery = Consultingwerk.Util.QueryHelper:CreatePreparedQuery
              (THIS-OBJECT:Dataset::eField,
               SUBSTITUTE ("FOR EACH eField WHERE BusinessEntityName = '&1' AND TempTableName = '&2'":U,
                           THIS-OBJECT:ParentTableBuffer::BusinessEntityName,
                           THIS-OBJECT:ParentTableBuffer::TempTableName)) .

        DO WHILE NOT hQuery:QUERY-OFF-END:

            ultraListView1:Items:Add (THIS-OBJECT:DATASET::eField::FieldName,
                                      THIS-OBJECT:DATASET::eField::FieldName):Appearance:Image = oImage .

            hQuery:GET-NEXT () .
        END.
        
        DELETE OBJECT hQuery .
        
        DO i = 2 TO NUM-ENTRIES (THIS-OBJECT:RelationFields) BY 2:
            
            CreateRelatedFields (ENTRY (i - 1, THIS-OBJECT:RelationFields),
                                 ENTRY (i, THIS-OBJECT:RelationFields)) .
            
        END.
        
        CATCH err AS Progress.Lang.Error:
        	ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Removes the currently selected field pair                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID RemoveSelectedFieldPair ():

        DEFINE VARIABLE cParentField AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cChildField  AS CHARACTER NO-UNDO.

        DEFINE VARIABLE oImage AS System.Drawing.Image NO-UNDO .
        
        IF NOT VALID-OBJECT (ultraListView3:ActiveItem) THEN 
            UNDO, THROW NEW AppError ("Please select a field pair from the linked fields list first."{&TRAN}, 0) . 

        ASSIGN cParentField = ENTRY (1, ultraListView3:ActiveItem:Key)
               cChildField  = ENTRY (2, ultraListView3:ActiveItem:Key) .
               
        ultraListView3:Items:Remove (ultraListView3:ActiveItem) .        
        
        FILE-INFO:FILE-NAME = "Consultingwerk\BusinessEntityDesigner\UI\Images\table2_selection_column.png":U .
        
        IF FILE-INFO:FULL-PATHNAME > "":U THEN 
            oImage = Consultingwerk.Util.ImageHelper:Load (FILE-INFO:FULL-PATHNAME) .

        ultraListView1:Items:Add (cParentField,
                                  cParentField):Appearance:Image = oImage .
        
        ultraListView2:Items:Add (cChildField,
                                  cChildField):Appearance:Image = oImage .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Automatically maps the currently selected entry on the left table
                 to an entry on the right table 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID RightMapAutomatically ():
		
        DEFINE VARIABLE cKey AS CHARACTER NO-UNDO.

        IF ultraListView2:SelectedItems:Count <> 1 THEN DO:
            Consultingwerk.Util.MessageFormHelper:ShowMessage ("Please select one item before using the auto mapping function!"{&TRAN},
                                                               Consultingwerk.Windows.Util.Forms.MessageFormImages:ImageWarning) . 
                                                                
            RETURN .                                                                 
        END.

        ASSIGN cKey = ultraListView2:SelectedItems[0]:Key .
        
        IF NOT ultraListView1:Items:Exists (cKey) THEN DO:
            Consultingwerk.Util.MessageFormHelper:ShowMessage (SUBSTITUTE ("The field &1 has no field available for auto mapping!"{&TRAN}, cKey),
                                                                Consultingwerk.Windows.Util.Forms.MessageFormImages:ImageWarning) . 
                                                                
            RETURN .                                                                 
        END.
        
        THIS-OBJECT:CreateRelatedFields (cKey, cKey) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the ultraButton1                                                                        
        Notes:                                                        
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event                 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraButton1_Click (sender AS System.Object, e AS System.EventArgs):
        
        
        THIS-OBJECT:RelationFields = "":U .

        {Consultingwerk/foreach.i Infragistics.Win.UltraWinListView.UltraListViewItem oItem in ultraListView3:Items}

            THIS-OBJECT:RelationFields = THIS-OBJECT:RelationFields +
                                         oItem:Key +
                                         ",":U .  
        END.

        THIS-OBJECT:RelationFields = TRIM (THIS-OBJECT:RelationFields, ",":U) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the ultraButton3                                                                        
        Notes:                                                        
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event                 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraButton3_Click (sender AS System.Object, 
                                            e AS System.EventArgs ):
        
        DEFINE VARIABLE cParentField AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cChildField  AS CHARACTER NO-UNDO.

        IF NOT VALID-OBJECT (ultraListView1:ActiveItem) OR NOT VALID-OBJECT (ultraListView2:ActiveItem) THEN 
            UNDO, THROW NEW AppError ("Please select a field from the parent table and a field from the child table first."{&TRAN}, 0) . 
        
        ASSIGN cParentField = UNBOX (ultraListView1:ActiveItem:Value)
               cChildField  = UNBOX (ultraListView2:ActiveItem:Value)
                .
        
        CreateRelatedFields (cParentField, cChildField) .
        
        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the DragDrop event of the ListView control
        Notes:   
        @param sender The reference to the sending Control
        @param e The DragEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraListView1_DragDrop (sender AS System.Object, 
                                                 e AS System.Windows.Forms.DragEventArgs):
        
        DEFINE VARIABLE oListViewItem  AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO .
        DEFINE VARIABLE oListViewItem2 AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO .

        DEFINE VARIABLE oPoint AS System.Drawing.Point NO-UNDO .

        IF NOT e:Data:GetDataPresent (Progress.Util.TypeHelper:GetType ("Infragistics.Win.UltraWinListView.UltraListViewItem":U)) THEN 
            RETURN . 

        oPoint = ultraListView1:PointToClient (NEW System.Drawing.Point (e:X, e:Y)) .

        oListViewItem = ultraListView1:ItemFromPoint (oPoint) .

        oListViewItem2 = CAST (e:Data:GetData (Progress.Util.TypeHelper:GetType ("Infragistics.Win.UltraWinListView.UltraListViewItem":U)), 
                               Infragistics.Win.UltraWinListView.UltraListViewItem) .

        IF VALID-OBJECT (oListViewItem) AND VALID-OBJECT (oListViewItem2) THEN 
            CreateRelatedFields (oListViewItem:Key, oListViewItem2:Key) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the DragOver event of the ListView control
        Notes:   
        @param sender The reference to the sending Control
        @param e The DragEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraListView1_DragOver (sender AS System.Object, 
                                                 e AS System.Windows.Forms.DragEventArgs):
        
        DEFINE VARIABLE oListViewItem  AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO .
        DEFINE VARIABLE oListViewItem2 AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO .

        DEFINE VARIABLE oPoint AS System.Drawing.Point NO-UNDO .

        IF NOT e:Data:GetDataPresent (Progress.Util.TypeHelper:GetType("Infragistics.Win.UltraWinListView.UltraListViewItem":U)) THEN 
            RETURN . 

        oPoint = ultraListView1:PointToClient (NEW System.Drawing.Point (e:X, e:Y)) .

        oListViewItem = ultraListView1:ItemFromPoint (oPoint) .

        oListViewItem2 = CAST (e:Data:GetData (Progress.Util.TypeHelper:GetType("Infragistics.Win.UltraWinListView.UltraListViewItem":U)), 
                               Infragistics.Win.UltraWinListView.UltraListViewItem) .

        IF VALID-OBJECT (oListViewItem) AND oListViewItem2:Control = ultraListView2 THEN DO:
            e:Effect = System.Windows.Forms.DragDropEffects:Link .
        
            RETURN . 
        END.

        e:Effect = System.Windows.Forms.DragDropEffects:None .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the MouseDown event of the ListView control
        Notes:   
        @param sender The reference to the sending Control
        @param e The MouseEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraListView1_MouseDown (sender AS System.Object, e AS System.Windows.Forms.MouseEventArgs):
        
        DEFINE VARIABLE oListViewItem AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO .
        
        oListViewItem = ultraListView1:ItemFromPoint (e:Location) .
        
        IF NOT VALID-OBJECT (oListViewItem) OR NOT VALID-OBJECT (oListViewItem:Value) THEN 
            RETURN . 
            
        IF Progress.Util.EnumHelper:AreEqual (e:Button, 
                                              System.Windows.Forms.MouseButtons:Left) THEN 
            ultraListView1:DoDragDrop (oListViewItem, System.Windows.Forms.DragDropEffects:Link) .
        ELSE DO:
            oListViewItem:Activate() .                                                  
            ultraListView1:SelectedItems:Add (oListViewItem) .
        END.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the DragDrop event of the ListView control
        Notes:   
        @param sender The reference to the sending Control
        @param e The DragEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraListView2_DragDrop (sender AS System.Object,
                                                 e AS System.Windows.Forms.DragEventArgs):
              
        DEFINE VARIABLE oListViewItem  AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO .
        DEFINE VARIABLE oListViewItem2 AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO .

        DEFINE VARIABLE oPoint AS System.Drawing.Point NO-UNDO .

        IF NOT e:Data:GetDataPresent (Progress.Util.TypeHelper:GetType("Infragistics.Win.UltraWinListView.UltraListViewItem":U)) THEN 
            RETURN . 

        oPoint = ultraListView2:PointToClient (NEW System.Drawing.Point (e:X, e:Y)) .

        oListViewItem2 = ultraListView2:ItemFromPoint (oPoint) .

        oListViewItem = CAST (e:Data:GetData (Progress.Util.TypeHelper:GetType("Infragistics.Win.UltraWinListView.UltraListViewItem":U)), 
                              Infragistics.Win.UltraWinListView.UltraListViewItem) .

        IF VALID-OBJECT (oListViewItem) AND VALID-OBJECT (oListViewItem2) THEN 
            CreateRelatedFields (oListViewItem:Key, oListViewItem2:Key) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the DragOver event of the ListView control
        Notes:   
        @param sender The reference to the sending Control
        @param e The DragEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraListView2_DragOver (sender AS System.Object, e AS System.Windows.Forms.DragEventArgs):
        
        DEFINE VARIABLE oListViewItem  AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO .
        DEFINE VARIABLE oListViewItem2 AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO .
        
        DEFINE VARIABLE oPoint AS System.Drawing.Point NO-UNDO . 
        
        IF NOT e:Data:GetDataPresent (Progress.Util.TypeHelper:GetType("Infragistics.Win.UltraWinListView.UltraListViewItem":U)) THEN 
            RETURN . 
        
        oPoint = ultraListView2:PointToClient (NEW System.Drawing.Point (e:X, e:Y)) .
        
        oListViewItem = ultraListView2:ItemFromPoint (oPoint) .

        oListViewItem2 = CAST (e:Data:GetData (Progress.Util.TypeHelper:GetType("Infragistics.Win.UltraWinListView.UltraListViewItem":U)), 
                               Infragistics.Win.UltraWinListView.UltraListViewItem) .

        IF VALID-OBJECT (oListViewItem) AND oListViewItem2:Control = ultraListView1 THEN DO: 
            e:Effect = System.Windows.Forms.DragDropEffects:Link .
        
            RETURN .
        END.

        e:Effect = System.Windows.Forms.DragDropEffects:None .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the MouseDown event of the ListView control
        Notes:   
        @param sender The reference to the sending Control
        @param e The MouseEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraListView2_MouseDown( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.MouseEventArgs ):
        
        DEFINE VARIABLE oListViewItem AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO .
        
        oListViewItem = ultraListView2:ItemFromPoint (e:Location) .
        
        IF NOT VALID-OBJECT (oListViewItem) OR NOT VALID-OBJECT (oListViewItem:Value) THEN 
            RETURN . 
            
        IF Progress.Util.EnumHelper:AreEqual (e:Button, 
                                              System.Windows.Forms.MouseButtons:Left) THEN 
            ultraListView2:DoDragDrop (oListViewItem, System.Windows.Forms.DragDropEffects:Link) .
        ELSE DO:
            oListViewItem:Activate() .                                                
            ultraListView2:SelectedItems:Add (oListViewItem) .        
        END.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the KeyDown event of the ultraListView3 (selected field pairs)
        Notes:   
        @param sender The object that raised this event
        @param e The KeyEventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraListView3_KeyDown (sender AS System.Object,   
                                                e AS System.Windows.Forms.KeyEventArgs):
        
        IF ultraListView3:SelectedItems:Count > 0 AND 
           Progress.Util.EnumHelper:AreEqual (e:KeyCode, 
                                              System.Windows.Forms.Keys:Delete) THEN 
            THIS-OBJECT:RemoveSelectedFieldPair () .                                              

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the ultraToolbarsManager1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The ToolClickEventArgs with the data for this event                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraToolbarsManager1_ToolClick (sender AS System.Object, 
                                                         e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs):
        
        CASE e:Tool:Key:
            WHEN "Remove":U THEN 
                THIS-OBJECT:RemoveSelectedFieldPair ().        
            WHEN "LeftMapAutomatically":U THEN 
                THIS-OBJECT:LeftMapAutomatically () . 
            WHEN "RightMapAutomatically":U THEN 
                THIS-OBJECT:RightMapAutomatically () . 
        END CASE . 

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the DataRelationFieldPickerForm class                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC DataRelationFieldPickerForm ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.