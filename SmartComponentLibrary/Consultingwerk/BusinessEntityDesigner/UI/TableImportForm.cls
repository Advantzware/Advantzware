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
    File        : TableImportForm
    Purpose     : Provides an user interface to select tables to import from
                  ABL source code
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Sep 19 22:55:34 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Collections.*     FROM PROPATH . 
USING Consultingwerk.Forms.*                     FROM PROPATH . 
USING Consultingwerk.BusinessEntityDesigner.UI.* FROM PROPATH .  
USING Progress.Lang.*                            FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.UI.TableImportForm INHERITS BaseForm: 

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraListView1 AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton2 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton1 AS Infragistics.Win.Misc.UltraButton NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Returns the CharacerList of selected tabels
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SelectedTables AS CharacterList NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TableImportForm class                                                                        
        Notes:      
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TableImportForm ():
        
        SUPER().
        InitializeComponent().
        
        THIS-OBJECT:SelectedTables = NEW CharacterList () .
        
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
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.BusinessEntityDesigner.UI.TableImportForm":U).
        THIS-OBJECT:ultraTabPageControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:ultraListView1 = NEW Infragistics.Win.UltraWinListView.UltraListView().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraTabControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
        THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
        THIS-OBJECT:ultraButton1 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraButton2 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraTabPageControl1:SuspendLayout().
        CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabControl1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraTabPageControl1 */
        /*  */
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraListView1).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:ultraTabPageControl1:Location = NEW System.Drawing.Point(1, 23).
        THIS-OBJECT:ultraTabPageControl1:Name = "ultraTabPageControl1":U.
        THIS-OBJECT:ultraTabPageControl1:Size = NEW System.Drawing.Size(527, 261).
        /*  */
        /* ultraListView1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraListView1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraListView1:ItemSettings:HideSelection = FALSE.
        THIS-OBJECT:ultraListView1:ItemSettings:LassoSelectMode = Infragistics.Win.UltraWinListView.LassoSelectMode:LeftMouseButton.
        THIS-OBJECT:ultraListView1:Location = NEW System.Drawing.Point(3, 28).
        THIS-OBJECT:ultraListView1:Name = "ultraListView1":U.
        THIS-OBJECT:ultraListView1:Size = NEW System.Drawing.Size(521, 230).
        THIS-OBJECT:ultraListView1:TabIndex = 1.
        THIS-OBJECT:ultraListView1:Text = "ultraListView1":U.
        THIS-OBJECT:ultraListView1:ItemSelectionChanged:Subscribe(THIS-OBJECT:ultraListView1_ItemSelectionChanged).
        /*  */
        /* ultraLabel1 */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel1:Appearance = appearance1.
        THIS-OBJECT:ultraLabel1:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(3, 8).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(364, 14).
        THIS-OBJECT:ultraLabel1:TabIndex = 0.
        THIS-OBJECT:ultraLabel1:Text = "Select the Tables that you want to import into the business entity model:":U.
        /*  */
        /* ultraTabControl1 */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTabControl1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl1).
        THIS-OBJECT:ultraTabControl1:Location = NEW System.Drawing.Point(12, 12).
        THIS-OBJECT:ultraTabControl1:Name = "ultraTabControl1":U.
        THIS-OBJECT:ultraTabControl1:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
        THIS-OBJECT:ultraTabControl1:Size = NEW System.Drawing.Size(531, 287).
        THIS-OBJECT:ultraTabControl1:TabIndex = 0.
        ultraTab1:TabPage = THIS-OBJECT:ultraTabPageControl1.
        ultraTab1:Text = "Import Tables":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 1 NO-UNDO.
        arrayvar0[1] = ultraTab1.
        THIS-OBJECT:ultraTabControl1:Tabs:AddRange(arrayvar0).
        /*  */
        /* ultraTabSharedControlsPage1 */
        /*  */
        THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
        THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1":U.
        THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(527, 261).
        /*  */
        /* ultraButton1 */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:ultraButton1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButton1:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:ultraButton1:Location = NEW System.Drawing.Point(387, 305).
        THIS-OBJECT:ultraButton1:Name = "ultraButton1":U.
        THIS-OBJECT:ultraButton1:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:ultraButton1:TabIndex = 1.
        THIS-OBJECT:ultraButton1:Text = "&OK":U.
        /*  */
        /* ultraButton2 */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:ultraButton2:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButton2:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:ultraButton2:Location = NEW System.Drawing.Point(468, 305).
        THIS-OBJECT:ultraButton2:Name = "ultraButton2":U.
        THIS-OBJECT:ultraButton2:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:ultraButton2:TabIndex = 2.
        THIS-OBJECT:ultraButton2:Text = "&Cancel":U.
        /*  */
        /* TableImportForm */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:ultraButton1.
        THIS-OBJECT:CancelButton = THIS-OBJECT:ultraButton2.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(555, 340).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButton2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButton1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTabControl1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:MaximizeBox = FALSE.
        THIS-OBJECT:MinimizeBox = FALSE.
        THIS-OBJECT:Name = "TableImportForm":U.
        THIS-OBJECT:SizeGripStyle = System.Windows.Forms.SizeGripStyle:Show.
        THIS-OBJECT:Text = "Import Tables":U.
        THIS-OBJECT:WindowPositionRegistryKey = "TableImportForm":U.
        THIS-OBJECT:ultraTabPageControl1:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl1:PerformLayout().
        CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabControl1:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Sets the Tables for the List View
        Notes:   
        @param poTables A CharacterList with the Tables to import
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetTables (poTables AS CharacterList):
        
        DEFINE VARIABLE oListViewItem AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO . 
        
        THIS-OBJECT:ultraListView1:Items:Clear () .
        
        {Consultingwerk/foreachPrimitiveList.i Character cTable in poTables}
        
            oListViewItem = THIS-OBJECT:ultraListView1:Items:Add (cTable, cTable) .
            oListViewItem:Appearance:Image = Consultingwerk.Util.ImageHelper:Load 
                    ("Consultingwerk/BusinessEntityDesigner/UI/Images/data_table.png":U) .
        
            ultraListView1:SelectedItems:Add (oListViewItem) .
        END.        
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ItemSelectionChanged event of the ultraListView1
                 instance
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The ItemSelectionChangedEventArgs with the data for this event  
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraListView1_ItemSelectionChanged (sender AS System.Object, 
                                                             e AS Infragistics.Win.UltraWinListView.ItemSelectionChangedEventArgs):
        
        ultraButton1:Enabled = ultraListView1:SelectedItems:Count > 0 . 

        THIS-OBJECT:SelectedTables:Clear () .
        
        {Consultingwerk/foreach.i Infragistics.Win.UltraWinListView.UltraListViewItem oItem in ultraListView1:SelectedItems}

            THIS-OBJECT:SelectedTables:Add (oItem:Key) .

        END . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the TableImportForm class                                                                        
        Notes:      
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC TableImportForm ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
