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
    File        : ReorderFieldsForm
    Purpose     : Provides an user interface to reorder fields in a temp-table
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Nov 07 23:40:51 CET 2010
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Consultingwerk.SmartComponents.Base.SmartWindowForm.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.BusinessEntityDesigner.UI.ReorderFieldsForm INHERITS SmartWindowForm: 
    
    DEFINE PRIVATE VARIABLE btnUp AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnDown AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraListView1 AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton2 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton1 AS Infragistics.Win.Misc.UltraButton NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Returns the comma delimited list of fields
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Fields AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ReorderFieldsForm class                                                                        
        Notes:      
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ReorderFieldsForm ():
        
        SUPER().
        InitializeComponent().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the btnDown instance                                                                        
        Notes:                                              
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event                          
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID btnDown_Click (sender AS System.Object, 
                                       e AS System.EventArgs):
        
        DEFINE VARIABLE oItem AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO .
        DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
        
        ASSIGN oItem  = THIS-OBJECT:ultraListView1:SelectedItems[0] 
               iIndex = oItem:Index.

        THIS-OBJECT:ultraListView1:Items:Remove (oItem) .
        THIS-OBJECT:ultraListView1:Items:Insert (iIndex + 1, oItem) . 
        THIS-OBJECT:ultraListView1:SelectedItems:Clear() .
        THIS-OBJECT:ultraListView1:SelectedItems:Add (oItem) .

        oItem:BringIntoView() .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the btnUp instance                                                                        
        Notes:                                              
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event                          
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID btnUp_Click (sender AS System.Object, e AS System.EventArgs ):
        
        DEFINE VARIABLE oItem AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO .
        DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
        
        ASSIGN oItem  = THIS-OBJECT:ultraListView1:SelectedItems[0] 
               iIndex = oItem:Index.

        THIS-OBJECT:ultraListView1:Items:Remove (oItem) .
        THIS-OBJECT:ultraListView1:Items:Insert (iIndex - 1, oItem) . 
        THIS-OBJECT:ultraListView1:SelectedItems:Clear() .
        THIS-OBJECT:ultraListView1:SelectedItems:Add (oItem) .

        oItem:BringIntoView() .

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
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.BusinessEntityDesigner.UI.ReorderFieldsForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        THIS-OBJECT:ultraTabPageControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:ultraListView1 = NEW Infragistics.Win.UltraWinListView.UltraListView().
        THIS-OBJECT:btnDown = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:btnUp = NEW Infragistics.Win.Misc.UltraButton().
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
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:btnDown).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:btnUp).
        THIS-OBJECT:ultraTabPageControl1:Location = NEW System.Drawing.Point(1, 23).
        THIS-OBJECT:ultraTabPageControl1:Name = "ultraTabPageControl1":U.
        THIS-OBJECT:ultraTabPageControl1:Size = NEW System.Drawing.Size(425, 261).
        /*  */
        /* ultraListView1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraListView1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraListView1:ItemSettings:AllowEdit = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraListView1:ItemSettings:SelectionType = Infragistics.Win.UltraWinListView.SelectionType:Single.
        THIS-OBJECT:ultraListView1:Location = NEW System.Drawing.Point(3, 3).
        THIS-OBJECT:ultraListView1:Name = "ultraListView1":U.
        THIS-OBJECT:ultraListView1:ShowGroups = FALSE.
        THIS-OBJECT:ultraListView1:Size = NEW System.Drawing.Size(338, 255).
        THIS-OBJECT:ultraListView1:TabIndex = 2.
        THIS-OBJECT:ultraListView1:Text = "ultraListView1":U.
        THIS-OBJECT:ultraListView1:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:List.
        THIS-OBJECT:ultraListView1:ViewSettingsList:ImageSize = NEW System.Drawing.Size(0, 0).
        THIS-OBJECT:ultraListView1:ViewSettingsList:MultiColumn = FALSE.
        THIS-OBJECT:ultraListView1:ItemSelectionChanged:Subscribe(THIS-OBJECT:ultraListView1_ItemSelectionChanged).
        /*  */
        /* btnDown */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = System.Windows.Forms.AnchorStyles:Top.
        THIS-OBJECT:btnDown:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        THIS-OBJECT:btnDown:Appearance = appearance1.
        THIS-OBJECT:btnDown:Enabled = FALSE.
        THIS-OBJECT:btnDown:Location = NEW System.Drawing.Point(347, 112).
        THIS-OBJECT:btnDown:Name = "btnDown":U.
        THIS-OBJECT:btnDown:Size = NEW System.Drawing.Size(75, 30).
        THIS-OBJECT:btnDown:TabIndex = 1.
        THIS-OBJECT:btnDown:Text = "&Down":U.
        THIS-OBJECT:btnDown:Click:Subscribe(THIS-OBJECT:btnDown_Click).
        /*  */
        /* btnUp */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = System.Windows.Forms.AnchorStyles:Top.
        THIS-OBJECT:btnUp:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        THIS-OBJECT:btnUp:Appearance = appearance2.
        THIS-OBJECT:btnUp:Enabled = FALSE.
        THIS-OBJECT:btnUp:Location = NEW System.Drawing.Point(347, 76).
        THIS-OBJECT:btnUp:Name = "btnUp":U.
        THIS-OBJECT:btnUp:Size = NEW System.Drawing.Size(75, 30).
        THIS-OBJECT:btnUp:TabIndex = 0.
        THIS-OBJECT:btnUp:Text = "&Up":U.
        THIS-OBJECT:btnUp:Click:Subscribe(THIS-OBJECT:btnUp_Click).
        /*  */
        /* ultraTabControl1 */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTabControl1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl1).
        THIS-OBJECT:ultraTabControl1:Location = NEW System.Drawing.Point(12, 12).
        THIS-OBJECT:ultraTabControl1:Name = "ultraTabControl1":U.
        THIS-OBJECT:ultraTabControl1:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
        THIS-OBJECT:ultraTabControl1:Size = NEW System.Drawing.Size(429, 287).
        THIS-OBJECT:ultraTabControl1:TabIndex = 0.
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
        THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(425, 261).
        /*  */
        /* ultraButton1 */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:ultraButton1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButton1:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:ultraButton1:Location = NEW System.Drawing.Point(285, 305).
        THIS-OBJECT:ultraButton1:Name = "ultraButton1":U.
        THIS-OBJECT:ultraButton1:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:ultraButton1:TabIndex = 1.
        THIS-OBJECT:ultraButton1:Text = "&OK":U.
        THIS-OBJECT:ultraButton1:Click:Subscribe(THIS-OBJECT:ultraButton1_Click).
        /*  */
        /* ultraButton2 */
        /*  */
        DEFINE VARIABLE nestedvar5 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar5 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:ultraButton2:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar5, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButton2:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:ultraButton2:Location = NEW System.Drawing.Point(366, 305).
        THIS-OBJECT:ultraButton2:Name = "ultraButton2":U.
        THIS-OBJECT:ultraButton2:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:ultraButton2:TabIndex = 2.
        THIS-OBJECT:ultraButton2:Text = "&Cancel":U.
        /*  */
        /* ReorderFieldsForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(453, 340).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButton2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButton1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTabControl1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "ReorderFieldsForm":U.
        THIS-OBJECT:Text = "Reorder Fields":U.
        THIS-OBJECT:WindowPositionRegistryKey = "ReorderFieldsForm":U.
        THIS-OBJECT:ultraTabPageControl1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabControl1:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event                                                                       
        Notes:                        
        @param e The System.EventArgs with the data for this event                                                
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):
        
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        
        SUPER:OnLoad (e) . 
        
        THIS-OBJECT:ultraListView1:Items:Clear () .
        
        DO i = 1 TO NUM-ENTRIES (THIS-OBJECT:Fields):
            
            THIS-OBJECT:ultraListView1:Items:Add (ENTRY (i, THIS-OBJECT:Fields),
                                                  ENTRY (i, THIS-OBJECT:Fields))  .
            
        END. 
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the ultraButton1 instance                                                                        
        Notes:                                              
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event                          
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraButton1_Click (sender AS System.Object, 
                                            e AS System.EventArgs):
        
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        
        THIS-OBJECT:Fields = "":U .
        
        DO i = 0 TO THIS-OBJECT:ultraListView1:Items:Count - 1:
            
            ASSIGN THIS-OBJECT:Fields = THIS-OBJECT:Fields + THIS-OBJECT:ultraListView1:Items[i]:Key + ",":U .
            
        END.

        ASSIGN THIS-OBJECT:Fields = TRIM (THIS-OBJECT:Fields, ",":U) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ItemSelectionChanged event of the ultraListView1 instance                                                                        
        Notes:                                              
        @param sender The reference to the object that raised the event
        @param e The ItemSelectionChangedEventArgs with the data for this event                          
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraListView1_ItemSelectionChanged 
                    (sender AS System.Object, 
                     e AS Infragistics.Win.UltraWinListView.ItemSelectionChangedEventArgs):
                    
        IF e:SelectedItems:Count = 0 THEN DO:
            ASSIGN btnUp:Enabled = FALSE 
                   btnDown:Enabled = FALSE .
                   
            RETURN .       
        END.        
        
        IF e:SelectedItems[0]:Index > 0 THEN 
            btnUp:Enabled = TRUE .
        ELSE 
            btnUp:Enabled = FALSE . 
            
        IF e:SelectedItems[0]:Index < THIS-OBJECT:ultraListView1:Items:Count - 1 THEN 
            btnDown:Enabled = TRUE .
        ELSE 
            btnDown:Enabled = FALSE . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the ReorderFieldsForm class                                                                        
        Notes:      
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC ReorderFieldsForm ():

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
