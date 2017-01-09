/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : DatabaseLoginPreferencesForm
    Purpose     : Allows to remove opt-out from DB Login
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Sep 20 09:29:56 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.Forms.*                 FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH .
USING Consultingwerk.Windows.OpenEdgeLogin.* FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.BusinessEntityDesigner.UI.DatabaseLoginPreferencesForm INHERITS BaseForm:

    DEFINE PRIVATE VARIABLE btnRemove AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE btnClose AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraListView1 AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.

    DEFINE VARIABLE oOptOutService AS IDatabaseLoginPreferences                              NO-UNDO .


    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DatabaseLoginPreferencesForm class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DatabaseLoginPreferencesForm ():

        SUPER().

        InitializeComponent().

        ASSIGN btnRemove:Enabled = FALSE .

        ultraListView1:Items:Clear () .

        oOptOutService = {Consultingwerk/get-service.i Consultingwerk.Windows.OpenEdgeLogin.IDatabaseLoginPreferences
                                                       "NEW Consultingwerk.Windows.OpenEdgeLogin.DatabaseLoginPreferences()"} .

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the btnClose
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID btnClose_Click (sender AS System.Object,
                                        e AS System.EventArgs):

        THIS-OBJECT:Close () .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the btnClose
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID btnRemove_Click (sender AS System.Object, e AS System.EventArgs):

        DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.

        {Consultingwerk/foreach.i Infragistics.Win.UltraWinListView.UltraListViewItem oItem in ultraListView1:SelectedItems}

            cValue = UNBOX (oItem:Value) .

            oOptOutService:OptIn (cValue) .
        END.

        THIS-OBJECT:RefreshListItems() .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /**
     * Purpose: Initializes the Visual Design
     * Notes:
     */
    @InitializeComponent.
    METHOD PRIVATE VOID InitializeComponent ():

        /* NOTE: The following method is automatically generated.

        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.

        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraListViewItem1 AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO.
        ultraListViewItem1 = NEW Infragistics.Win.UltraWinListView.UltraListViewItem("-db lorem ipsum":U, ?, ?).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraListViewItem2 AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO.
        ultraListViewItem2 = NEW Infragistics.Win.UltraWinListView.UltraListViewItem("-db lorem ipsum lorem ipsum":U, ?, ?).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraListViewItem3 AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO.
        ultraListViewItem3 = NEW Infragistics.Win.UltraWinListView.UltraListViewItem("-db lorem ipsum":U, ?, ?).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.BusinessEntityDesigner.UI.DatabaseLoginPreferencesForm":U).
        THIS-OBJECT:btnClose = NEW System.Windows.Forms.Button().
        THIS-OBJECT:btnRemove = NEW System.Windows.Forms.Button().
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraListView1 = NEW Infragistics.Win.UltraWinListView.UltraListView().
        THIS-OBJECT:panel1:SuspendLayout().
        CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* btnClose */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnClose:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnClose:Location = NEW System.Drawing.Point(384, 294).
        THIS-OBJECT:btnClose:Name = "btnClose":U.
        THIS-OBJECT:btnClose:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnClose:TabIndex = 0.
        THIS-OBJECT:btnClose:Text = "&Close":U.
        THIS-OBJECT:btnClose:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:btnClose:UseVisualStyleBackColor = TRUE.
        THIS-OBJECT:btnClose:Click:Subscribe(THIS-OBJECT:btnClose_Click).
        /*  */
        /* btnRemove */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnRemove:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnRemove:Location = NEW System.Drawing.Point(12, 294).
        THIS-OBJECT:btnRemove:Name = "btnRemove":U.
        THIS-OBJECT:btnRemove:Size = NEW System.Drawing.Size(184, 23).
        THIS-OBJECT:btnRemove:TabIndex = 1.
        THIS-OBJECT:btnRemove:Text = "Remove Selected Entries":U.
        THIS-OBJECT:btnRemove:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:btnRemove:UseVisualStyleBackColor = TRUE.
        THIS-OBJECT:btnRemove:Click:Subscribe(THIS-OBJECT:btnRemove_Click).
        /*  */
        /* panel1 */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraListView1).
        THIS-OBJECT:panel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:panel1:Name = "panel1":U.
        THIS-OBJECT:panel1:Size = NEW System.Drawing.Size(473, 288).
        THIS-OBJECT:panel1:TabIndex = 2.
        /*  */
        /* ultraLabel1 */
        /*  */
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(12, 12).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(447, 37).
        THIS-OBJECT:ultraLabel1:TabIndex = 1.
        THIS-OBJECT:ultraLabel1:Text = "The following databases will not prompt for user-id and password when launching the tools. Remove entries to re-enable login.":U.
        /*  */
        /* ultraListView1 */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraListView1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinListView.UltraListViewItem EXTENT 3 NO-UNDO.
        arrayvar0[1] = ultraListViewItem1.
        arrayvar0[2] = ultraListViewItem2.
        arrayvar0[3] = ultraListViewItem3.
        THIS-OBJECT:ultraListView1:Items:AddRange(arrayvar0).
        THIS-OBJECT:ultraListView1:ItemSettings:AllowEdit = Infragistics.Win.DefaultableBoolean:False.
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        THIS-OBJECT:ultraListView1:ItemSettings:Appearance = appearance1.
        THIS-OBJECT:ultraListView1:Location = NEW System.Drawing.Point(12, 55).
        THIS-OBJECT:ultraListView1:MainColumn:Width = 500.
        THIS-OBJECT:ultraListView1:Name = "ultraListView1":U.
        THIS-OBJECT:ultraListView1:Size = NEW System.Drawing.Size(447, 219).
        THIS-OBJECT:ultraListView1:TabIndex = 0.
        THIS-OBJECT:ultraListView1:Text = "ultraListView1":U.
        THIS-OBJECT:ultraListView1:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:Details.
        THIS-OBJECT:ultraListView1:ItemSelectionChanged:Subscribe(THIS-OBJECT:ultraListView1_ItemSelectionChanged).
        /*  */
        /* DatabaseLoginPreferencesForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(471, 329).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnRemove).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnClose).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "DatabaseLoginPreferencesForm":U.
        THIS-OBJECT:Text = "Database Login Preferences":U.
        THIS-OBJECT:WindowPositionRegistryKey = "DatabaseLoginPreferencesForm":U.
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
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

        THIS-OBJECT:RefreshListItems () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Refreshes the List Items
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID RefreshListItems ():

        DEFINE VARIABLE oList AS Consultingwerk.Framework.Collections.CharacterList NO-UNDO .

        ultraListView1:Items:Clear () .

        oList = oOptOutService:GetAllOptOutedDbParam() .

        {Consultingwerk/foreachPrimitiveList.i Character cDbParam in oList}

            ultraListView1:Items:Add (GUID, BOX (cDbParam)) .

        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ItemSelectionChanged event of the ultraListView1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The ItemSelectionChangedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraListView1_ItemSelectionChanged (sender AS System.Object,
                                                             e AS Infragistics.Win.UltraWinListView.ItemSelectionChangedEventArgs):

        IF ultraListView1:SelectedItems:Count > 0 THEN
            btnRemove:Enabled = TRUE .
        ELSE
            btnRemove:Enabled = FALSE .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the DatabaseLoginPreferencesForm class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC DatabaseLoginPreferencesForm ( ):

    END DESTRUCTOR.

END CLASS.
