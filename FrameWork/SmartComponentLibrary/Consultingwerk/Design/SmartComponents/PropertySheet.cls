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
    File        : PropertySheet
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 15 22:59:25 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Design.Controls.* FROM PROPATH .
USING Progress.Lang.*                  FROM PROPATH .
USING Progress.Windows.UserControl     FROM ASSEMBLY  .

CLASS Consultingwerk.Design.SmartComponents.PropertySheet INHERITS UserControl:

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE lbLookup AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE lbTabs AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbLookupName AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE tbMultiline AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbTabs AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbText AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.


    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the SmartViewerDesignerControl
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Designer AS Consultingwerk.Design.SmartComponents.Viewer.SmartViewerDesignerControl NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the selected control
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SelectedControl AS System.Windows.Forms.Control NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the SelectionService
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SelectionService AS System.ComponentModel.Design.ISelectionService NO-UNDO
    GET.
    SET (INPUT arg AS System.ComponentModel.Design.ISelectionService):

        IF VALID-OBJECT (THIS-OBJECT:SelectionService) THEN
            THIS-OBJECT:SelectionService:SelectionChanged:Unsubscribe (SelectionChangedHandler) NO-ERROR .

        THIS-OBJECT:SelectionService = arg .

        IF VALID-OBJECT (arg) THEN
            arg:SelectionChanged:Subscribe (SelectionChangedHandler) .

    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertySheet class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC PropertySheet ():

        SUPER().

        InitializeComponent().

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

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
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbText = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbMultiline = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:tbTabs = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:lbTabs = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbLookupName = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:lbLookup = NEW Infragistics.Win.Misc.UltraLabel().
        CAST(THIS-OBJECT:tbText, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbMultiline, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbTabs, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbLookupName, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraLabel1 */
        /*  */
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(3, 7).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(72, 23).
        THIS-OBJECT:ultraLabel1:TabIndex = 0.
        THIS-OBJECT:ultraLabel1:Text = "Text:":U.
        /*  */
        /* tbText */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbText:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbText:Location = NEW System.Drawing.Point(81, 3).
        THIS-OBJECT:tbText:Name = "tbText":U.
        THIS-OBJECT:tbText:Size = NEW System.Drawing.Size(336, 21).
        THIS-OBJECT:tbText:TabIndex = 1.
        THIS-OBJECT:tbText:TextChanged:Subscribe(THIS-OBJECT:tbText_TextChanged).
        /*  */
        /* tbMultiline */
        /*  */
        THIS-OBJECT:tbMultiline:Location = NEW System.Drawing.Point(81, 30).
        THIS-OBJECT:tbMultiline:Name = "tbMultiline":U.
        THIS-OBJECT:tbMultiline:Size = NEW System.Drawing.Size(142, 20).
        THIS-OBJECT:tbMultiline:TabIndex = 2.
        THIS-OBJECT:tbMultiline:Text = "Multiline":U.
        THIS-OBJECT:tbMultiline:CheckedChanged:Subscribe(THIS-OBJECT:tbMultiline_CheckedChanged).
        /*  */
        /* tbTabs */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbTabs:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbTabs:Location = NEW System.Drawing.Point(81, 3).
        THIS-OBJECT:tbTabs:Multiline = TRUE.
        THIS-OBJECT:tbTabs:Name = "tbTabs":U.
        THIS-OBJECT:tbTabs:Size = NEW System.Drawing.Size(336, 113).
        THIS-OBJECT:tbTabs:TabIndex = 3.
        THIS-OBJECT:tbTabs:Visible = FALSE.
        THIS-OBJECT:tbTabs:TextChanged:Subscribe(THIS-OBJECT:tbTabs_TextChanged).
        /*  */
        /* lbTabs */
        /*  */
        THIS-OBJECT:lbTabs:Location = NEW System.Drawing.Point(3, 7).
        THIS-OBJECT:lbTabs:Name = "lbTabs":U.
        THIS-OBJECT:lbTabs:Size = NEW System.Drawing.Size(72, 52).
        THIS-OBJECT:lbTabs:TabIndex = 4.
        THIS-OBJECT:lbTabs:Text = "Tabs (delimit with ~"|~"):":U.
        /*  */
        /* tbLookupName */
        /*  */
        THIS-OBJECT:tbLookupName:AddLookupButton = FALSE.
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbLookupName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton1:Key = "ShowLookupDialog":U.
        editorButton1:Text = "...":U.
        THIS-OBJECT:tbLookupName:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:tbLookupName:Location = NEW System.Drawing.Point(81, 56).
        THIS-OBJECT:tbLookupName:LookupBrowserExcludeFields = "":U.
        THIS-OBJECT:tbLookupName:LookupBrowserFields = "object_filename,object_description,product_module_code,object_type_code":U.
        THIS-OBJECT:tbLookupName:LookupControls = "":U.
        THIS-OBJECT:tbLookupName:LookupDialogClassName = "":U.
        THIS-OBJECT:tbLookupName:LookupDialogFilterFields = "object_filename,product_module_code":U.
        THIS-OBJECT:tbLookupName:LookupDialogFilterOperators = "BEGINS,BEGINS":U.
        THIS-OBJECT:tbLookupName:LookupDialogQuerySort = "":U.
        THIS-OBJECT:tbLookupName:LookupDialogQueryString = "FOR EACH eSmartObject WHERE eSmartObject.object_type_code EQ ~"SmartBusinessEntityLookup~"":U.
        THIS-OBJECT:tbLookupName:LookupDialogTitle = "Select Lookup Control":U.
        THIS-OBJECT:tbLookupName:LookupEntityName = "Consultingwerk.Framework.RepositoryServices.Dynamics.Entities.SmartObjectBusinessEntity":U.
        THIS-OBJECT:tbLookupName:LookupEntityTable = "eSmartObject":U.
        THIS-OBJECT:tbLookupName:LookupEntityView = "":U.
        THIS-OBJECT:tbLookupName:LookupFields = "":U.
        THIS-OBJECT:tbLookupName:LookupKeyField = "object_filename":U.
        THIS-OBJECT:tbLookupName:LookupKeyValue = "":U.
        THIS-OBJECT:tbLookupName:LookupKeyValueColumn = "":U.
        THIS-OBJECT:tbLookupName:LookupQuerySort = "":U.
        THIS-OBJECT:tbLookupName:LookupQueryString = "FOR EACH eSmartObject WHERE eSmartObject.object_type_code EQ ~"SmartBusinessEntityLookup~" AND eSmartObject.object_filename BEGINS ~"&1~"":U.
        THIS-OBJECT:tbLookupName:LookupTimerDelay = 750.
        THIS-OBJECT:tbLookupName:Name = "tbLookupName":U.
        THIS-OBJECT:tbLookupName:Size = NEW System.Drawing.Size(336, 21).
        THIS-OBJECT:tbLookupName:TabIndex = 5.
        THIS-OBJECT:tbLookupName:LookupComplete:Subscribe(THIS-OBJECT:tbLookupName_LookupComplete).
        THIS-OBJECT:tbLookupName:ShowLookupDialog:Subscribe(THIS-OBJECT:tbLookupName_ShowLookupDialog).
        THIS-OBJECT:tbLookupName:TextChanged:Subscribe(THIS-OBJECT:tbLookupName_TextChanged).
        /*  */
        /* lbLookup */
        /*  */
        THIS-OBJECT:lbLookup:Location = NEW System.Drawing.Point(3, 61).
        THIS-OBJECT:lbLookup:Name = "lbLookup":U.
        THIS-OBJECT:lbLookup:Size = NEW System.Drawing.Size(72, 23).
        THIS-OBJECT:lbLookup:TabIndex = 6.
        THIS-OBJECT:lbLookup:Text = "Use Lookup:":U.
        /*  */
        /* PropertySheet */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:lbLookup).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbLookupName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:lbTabs).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbMultiline).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbText).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbTabs).
        THIS-OBJECT:Name = "PropertySheet":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(420, 119).
        CAST(THIS-OBJECT:tbText, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbMultiline, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbTabs, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbLookupName, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Reads the Tabs of a TabControl
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ReadCurrentTabs ():

        DEFINE VARIABLE oTab  AS System.Windows.Forms.TabControl NO-UNDO .
        DEFINE VARIABLE cTabs AS CHARACTER                       NO-UNDO.

        IF NOT TYPE-OF (THIS-OBJECT:SelectedControl,
                        System.Windows.Forms.TabControl) THEN
            RETURN .

        oTab = CAST (THIS-OBJECT:SelectedControl,
                     System.Windows.Forms.TabControl) .

        {Consultingwerk/foreach.i System.Windows.Forms.TabPage oTabPage in oTab:TabPages}

            ASSIGN cTabs = cTabs + oTabPage:Text + "|":U .

        END.

        THIS-OBJECT:tbTabs:Text = TRIM (cTabs, "|":U) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the SelectionChanged event of the SelectionService
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SelectionChangedHandler (sender AS System.Object,
                                                   e AS System.EventArgs):

        DEFINE VARIABLE oSelection  AS System.Collections.ICollection NO-UNDO .
        DEFINE VARIABLE oEnumerator AS System.Collections.IEnumerator NO-UNDO .

        oSelection = THIS-OBJECT:SelectionService:GetSelectedComponents() .

        oEnumerator = oSelection:GetEnumerator() .
        oEnumerator:Reset().

        IF oEnumerator:MoveNext() AND TYPE-OF (oEnumerator:Current,
                                               System.Windows.Forms.Control) THEN DO:

            THIS-OBJECT:SelectedControl = CAST (oEnumerator:Current,
                                                System.Windows.Forms.Control) .

            THIS-OBJECT:tbText:Text = THIS-OBJECT:SelectedControl:Text .

            IF THIS-OBJECT:Designer:SubstituteDesignerControls:ContainsKey (THIS-OBJECT:SelectedControl) THEN
                THIS-OBJECT:tbLookupName:Text = THIS-OBJECT:Designer:SubstituteDesignerControls [THIS-OBJECT:SelectedControl] .
            ELSE
                THIS-OBJECT:tbLookupName:Text = "":U .

            IF TYPE-OF (oEnumerator:Current,
                        System.Windows.Forms.TextBox) THEN DO:

                tbMultiline:Visible = TRUE .
                tbMultiline:Checked  = CAST (oEnumerator:Current,
                                             System.Windows.Forms.TextBox):Multiline .
                tbLookupName:Visible = TRUE .
                lbLookup:Visible = TRUE .
            END.
            ELSE DO:
                tbMultiline:Visible = FALSE .
                tbLookupName:Visible = FALSE .
                lbLookup:Visible = FALSE .

            END.

            IF TYPE-OF (oEnumerator:Current,
                        System.Windows.Forms.TabControl) THEN DO:

                ASSIGN THIS-OBJECT:tbTabs:Visible = TRUE
                       THIS-OBJECT:lbTabs:Visible = TRUE .

                THIS-OBJECT:tbTabs:BringToFront() .
                THIS-OBJECT:ReadCurrentTabs () .
            END.
            ELSE
                ASSIGN THIS-OBJECT:tbTabs:Visible = FALSE
                       THIS-OBJECT:lbTabs:Visible = FALSE .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the LookupComplete event of the tbLookupName
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID tbLookupName_LookupComplete (sender AS System.Object,
                                                     e AS System.EventArgs):

        Consultingwerk.Framework.FrameworkSettings:AllowDesignFeatures = FALSE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ShowLookupDialog event of the tbLookupName
        Notes:
        @param sender The reference to the object that raised the event
        @param e The CancelEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID tbLookupName_ShowLookupDialog (sender AS System.Object,
                                                       e AS System.ComponentModel.CancelEventArgs):

        Consultingwerk.Framework.FrameworkSettings:AllowDesignFeatures = TRUE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the TextChanged event of the tbLookupName
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID tbLookupName_TextChanged (sender AS System.Object,
                                                  e AS System.EventArgs):

        IF NOT VALID-OBJECT (THIS-OBJECT:SelectedControl) THEN
            NEXT .

        IF THIS-OBJECT:Designer:SubstituteDesignerControls:ContainsKey (THIS-OBJECT:SelectedControl) THEN
            THIS-OBJECT:Designer:SubstituteDesignerControls:Remove(THIS-OBJECT:SelectedControl) .

        IF THIS-OBJECT:tbLookupName:Text > "":U THEN
            THIS-OBJECT:Designer:SubstituteDesignerControls:Add (THIS-OBJECT:SelectedControl, THIS-OBJECT:tbLookupName:Text) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the CheckedChanged event of the tbLookupName
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID tbMultiline_CheckedChanged (sender AS System.Object,
                                                    e AS System.EventArgs):

        IF VALID-OBJECT (THIS-OBJECT:SelectedControl) AND TYPE-OF (THIS-OBJECT:SelectedControl, System.Windows.Forms.TextBox) THEN
            CAST (THIS-OBJECT:SelectedControl, System.Windows.Forms.TextBox):Multiline = tbMultiline:Checked .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the TextChanged event of the tbTabs
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID tbTabs_TextChanged (sender AS System.Object,
                                            e AS System.EventArgs):

        IF THIS-OBJECT:tbTabs:Focused THEN
            THIS-OBJECT:WriteTabs () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the TextChanged event of the tbText
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID tbText_TextChanged (sender AS System.Object,
                                            e AS System.EventArgs):

        IF VALID-OBJECT (THIS-OBJECT:SelectedControl) THEN
            THIS-OBJECT:SelectedControl:Text = THIS-OBJECT:tbText:Text .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Writes the Tab labels back to the Control
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID WriteTabs ():

        DEFINE VARIABLE oTab  AS System.Windows.Forms.TabControl NO-UNDO .

        DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        IF NOT TYPE-OF (THIS-OBJECT:SelectedControl,
                        System.Windows.Forms.TabControl) THEN
            RETURN .

        oTab = CAST (THIS-OBJECT:SelectedControl,
                     System.Windows.Forms.TabControl) .

        ASSIGN iCount = oTab:TabPages:Count .

        DO i = iCount + 1 TO NUM-ENTRIES (THIS-OBJECT:tbTabs:Text, "|":U):
            oTab:TabPages:Add ("":U) .
        END.

        DO WHILE oTab:TabPages:Count > NUM-ENTRIES (THIS-OBJECT:tbTabs:Text, "|":U):
            oTab:TabPages:RemoveAt (oTab:TabPages:Count - 1) .
        END.

        DO i = 1 TO NUM-ENTRIES (THIS-OBJECT:tbTabs:Text, "|":U):

            oTab:TabPages[i - 1]:Text = ENTRY (i, THIS-OBJECT:tbTabs:Text, "|":U) .

        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor of the PropertySheet class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC PropertySheet ():

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
