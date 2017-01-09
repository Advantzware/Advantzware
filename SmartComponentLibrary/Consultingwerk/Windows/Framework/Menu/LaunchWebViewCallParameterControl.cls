/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : LaunchWebViewCallParameterControl
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Oct 31 18:56:56 CET 2015
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.*                           FROM PROPATH .
USING Consultingwerk.OERA.BusinessEntityDescriptor.*       FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*                FROM PROPATH .
USING Consultingwerk.Util.*                                FROM PROPATH .
USING Consultingwerk.Web2.SmartFramework.Menu.*            FROM PROPATH .
USING Consultingwerk.Windows.Framework.*                   FROM PROPATH .
USING Consultingwerk.Windows.Framework.Menu.*              FROM PROPATH .
USING Infragistics.Win.UltraWinEditors.*                   FROM ASSEMBLY .
USING Progress.Lang.*                                      FROM PROPATH .
USING System.Windows.Forms.*                               FROM ASSEMBLY .

CLASS Consultingwerk.Windows.Framework.Menu.LaunchWebViewCallParameterControl
    INHERITS CallParameterBaseControl:

    DEFINE PRIVATE VARIABLE coEntityView AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE coEntityTable AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE groupBox1 AS System.Windows.Forms.GroupBox NO-UNDO.
    DEFINE PRIVATE VARIABLE openFileDialog1 AS System.Windows.Forms.OpenFileDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE tbViewTitle AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbParameter AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbEntityName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE coViewUri AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbViewTables AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel4 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel6 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel5 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    DEFINE PRIVATE VARIABLE ultraLabel7 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the LaunchWebViewCallParameterControl class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LaunchWebViewCallParameterControl ():
        SUPER ().
        InitializeComponent ().

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END CONSTRUCTOR.

    /**
     * Purpose: Event handler for the EditorButtonClick event of the coViewUri
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The EditorButtonEventArgs with the data for this event
     */
    @VisualDesigner.
    METHOD PRIVATE VOID coViewUri_EditorButtonClick (sender AS System.Object,
                                                     e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        DEFINE VARIABLE oDialogResult AS DialogResult NO-UNDO .

        WAIT-FOR openFileDialog1:ShowDialog () SET oDialogResult .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult} .

        coViewUri:Text = REPLACE (FileHelper:RelativeFileName (openFileDialog1:FileName),
                                  "~\":U,
                                  "/":U) .

    END METHOD .

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
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Menu.LaunchWebViewCallParameterControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton2 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton2 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem1 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem1 = NEW Infragistics.Win.ValueListItem().
        THIS-OBJECT:ultraLabel3 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel4 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbEntityName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbViewTitle = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel6 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel5 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:coEntityTable = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:coEntityView = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:tbParameter = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:coViewUri = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:groupBox1 = NEW System.Windows.Forms.GroupBox().
        THIS-OBJECT:ultraLabel7 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbViewTables = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:openFileDialog1 = NEW System.Windows.Forms.OpenFileDialog().
        CAST(THIS-OBJECT:tbEntityName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbViewTitle, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:coEntityTable, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:coEntityView, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbParameter, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:coViewUri, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:groupBox1:SuspendLayout().
        CAST(THIS-OBJECT:tbViewTables, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraLabel3 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel3, "ultraLabel3":U).
        THIS-OBJECT:ultraLabel3:Name = "ultraLabel3":U.
        /*  */
        /* ultraLabel2 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel2, "ultraLabel2":U).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        /*  */
        /* ultraLabel4 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel4, "ultraLabel4":U).
        THIS-OBJECT:ultraLabel4:Name = "ultraLabel4":U.
        /*  */
        /* tbEntityName */
        /*  */
        resources:ApplyResources(THIS-OBJECT:tbEntityName, "tbEntityName":U).
        resources:ApplyResources(editorButton1, "editorButton1":U).
        THIS-OBJECT:tbEntityName:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:tbEntityName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbEntityName:Name = "tbEntityName":U.
        THIS-OBJECT:tbEntityName:EditorButtonClick:Subscribe(THIS-OBJECT:tbEntityName_EditorButtonClick).
        THIS-OBJECT:tbEntityName:KeyDown:Subscribe(THIS-OBJECT:tbEntityName_KeyDown).
        THIS-OBJECT:tbEntityName:Leave:Subscribe(THIS-OBJECT:tbEntityName_Leave).
        /*  */
        /* tbViewTitle */
        /*  */
        resources:ApplyResources(THIS-OBJECT:tbViewTitle, "tbViewTitle":U).
        THIS-OBJECT:tbViewTitle:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbViewTitle:Name = "tbViewTitle":U.
        /*  */
        /* ultraLabel6 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel6, "ultraLabel6":U).
        THIS-OBJECT:ultraLabel6:Name = "ultraLabel6":U.
        /*  */
        /* ultraLabel5 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel5, "ultraLabel5":U).
        THIS-OBJECT:ultraLabel5:Name = "ultraLabel5":U.
        /*  */
        /* ultraLabel1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel1, "ultraLabel1":U).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        /*  */
        /* coEntityTable */
        /*  */
        resources:ApplyResources(THIS-OBJECT:coEntityTable, "coEntityTable":U).
        THIS-OBJECT:coEntityTable:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:coEntityTable:Name = "coEntityTable":U.
        /*  */
        /* coEntityView */
        /*  */
        resources:ApplyResources(THIS-OBJECT:coEntityView, "coEntityView":U).
        THIS-OBJECT:coEntityView:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:coEntityView:Name = "coEntityView":U.
        /*  */
        /* tbParameter */
        /*  */
        resources:ApplyResources(THIS-OBJECT:tbParameter, "tbParameter":U).
        THIS-OBJECT:tbParameter:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbParameter:Name = "tbParameter":U.
        /*  */
        /* coViewUri */
        /*  */
        resources:ApplyResources(THIS-OBJECT:coViewUri, "coViewUri":U).
        resources:ApplyResources(editorButton2, "editorButton2":U).
        THIS-OBJECT:coViewUri:ButtonsRight:Add(editorButton2).
        THIS-OBJECT:coViewUri:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        valueListItem1:DataValue = "Consultingwerk/Web2/entityview.html":U.
        resources:ApplyResources(valueListItem1, "valueListItem1":U).
        valueListItem1:ForceApplyResources = "":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.ValueListItem EXTENT 1 NO-UNDO.
        arrayvar0[1] = valueListItem1.
        THIS-OBJECT:coViewUri:Items:AddRange(arrayvar0).
        THIS-OBJECT:coViewUri:Name = "coViewUri":U.
        THIS-OBJECT:coViewUri:EditorButtonClick:Subscribe(THIS-OBJECT:coViewUri_EditorButtonClick).
        /*  */
        /* groupBox1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:groupBox1, "groupBox1":U).
        THIS-OBJECT:groupBox1:Controls:Add(THIS-OBJECT:ultraLabel7).
        THIS-OBJECT:groupBox1:Controls:Add(THIS-OBJECT:tbViewTables).
        THIS-OBJECT:groupBox1:Controls:Add(THIS-OBJECT:ultraLabel6).
        THIS-OBJECT:groupBox1:Controls:Add(THIS-OBJECT:ultraLabel4).
        THIS-OBJECT:groupBox1:Controls:Add(THIS-OBJECT:ultraLabel5).
        THIS-OBJECT:groupBox1:Controls:Add(THIS-OBJECT:tbViewTitle).
        THIS-OBJECT:groupBox1:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:groupBox1:Controls:Add(THIS-OBJECT:tbEntityName).
        THIS-OBJECT:groupBox1:Controls:Add(THIS-OBJECT:coEntityTable).
        THIS-OBJECT:groupBox1:Controls:Add(THIS-OBJECT:coEntityView).
        THIS-OBJECT:groupBox1:Name = "groupBox1":U.
        THIS-OBJECT:groupBox1:TabStop = FALSE.
        THIS-OBJECT:groupBox1:UseCompatibleTextRendering = TRUE.
        /*  */
        /* ultraLabel7 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel7, "ultraLabel7":U).
        THIS-OBJECT:ultraLabel7:Name = "ultraLabel7":U.
        /*  */
        /* tbViewTables */
        /*  */
        resources:ApplyResources(THIS-OBJECT:tbViewTables, "tbViewTables":U).
        THIS-OBJECT:tbViewTables:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbViewTables:Name = "tbViewTables":U.
        /*  */
        /* openFileDialog1 */
        /*  */
        THIS-OBJECT:openFileDialog1:DefaultExt = "html":U.
        resources:ApplyResources(THIS-OBJECT:openFileDialog1, "openFileDialog1":U).
        THIS-OBJECT:openFileDialog1:RestoreDirectory = TRUE.
        /*  */
        /* LaunchWebViewCallParameterControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:BackColor = System.Drawing.SystemColors:Control.
        THIS-OBJECT:Controls:Add(THIS-OBJECT:groupBox1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:coViewUri).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbParameter).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel3).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:Name = "LaunchWebViewCallParameterControl":U.
        CAST(THIS-OBJECT:tbEntityName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbViewTitle, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:coEntityTable, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:coEntityView, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbParameter, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:coViewUri, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:groupBox1:ResumeLayout(FALSE).
        THIS-OBJECT:groupBox1:PerformLayout().
        CAST(THIS-OBJECT:tbViewTables, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Clear all editor fields
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID InternalClearDataFields ():

        ASSIGN
            coViewUri:Text     = "":U
            tbParameter:Text   = "":U
            tbEntityName:Text  = "":U
            coEntityTable:Text = "":U
            coEntityView:Text  = "":U
            tbViewTables:Text  = "":U
            tbViewTitle:Text   = "":U
            .

        coEntityTable:ValueList:ValueListItems:Clear () .
        coEntityView:ValueList:ValueListItems:Clear () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Open a file dialog to select a BusinessEntity
        Notes:
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID tbEntityName_EditorButtonClick (sender AS System.Object,
                                                        e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        DEFINE VARIABLE oOpenFileDialog AS System.Windows.Forms.OpenFileDialog NO-UNDO .
        DEFINE VARIABLE oDialogResult   AS System.Windows.Forms.DialogResult   NO-UNDO .
        DEFINE VARIABLE oClass          AS Progress.Lang.Class                 NO-UNDO .

        oOpenFileDialog = NEW System.Windows.Forms.OpenFileDialog () .
        oOpenFileDialog:AddExtension = TRUE .
        oOpenFileDialog:CheckFileExists = TRUE .
        oOpenFileDialog:CheckPathExists = TRUE .
        oOpenFileDialog:DefaultExt = "cls":U .
        oOpenFileDialog:DereferenceLinks = TRUE .
        oOpenFileDialog:Filter = "Business Entity source files (*.cls)|*.cls|All files (*.*)|*.*"{&TRAN} .
        oOpenFileDialog:Multiselect = FALSE .
        oOpenFileDialog:RestoreDirectory = TRUE .
        oOpenFileDialog:Title = "Select a Business Entity"{&TRAN} .

        WAIT-FOR oOpenFileDialog:ShowDialog () SET oDialogResult .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult} .

        oClass = ClassHelper:FileNameToClass (oOpenFileDialog:FileName) .

        IF ClassHelper:FileNameToClass (oOpenFileDialog:FileName) = ? THEN
            RETURN.

        ASSIGN
            tbEntityName:Text = oClass:TypeName .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the KeyDown handler of the Text boxes with an
                 EditorButton (Lookup)
        Notes:   Maps F3 to EditorButtonClick
        @param sender The reference to the object that raised the event
        @param e The KeyEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID tbEntityName_KeyDown (sender AS System.Object,
                                              e AS System.Windows.Forms.KeyEventArgs):

        DEFINE VARIABLE cControlName AS CHARACTER NO-UNDO.

        IF e:Handled = FALSE AND Progress.Util.EnumHelper:AreEqual (e:KeyCode, SmartLookupSettings:ShortcutKey)
            AND THIS-OBJECT:Enabled THEN DO:

            ASSIGN cControlName = CAST (sender, System.Windows.Forms.Control):Name .

            DYNAMIC-INVOKE (THIS-OBJECT,
                            SUBSTITUTE ("&1_EditorButtonClick":U, cControlName),
                            sender,
                            NEW EditorButtonEventArgs (?, ?)) .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Leave event of the tbEntityName
        Notes:
        @param sender The reference to te object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID tbEntityName_Leave (sender AS System.Object,
                                            e AS System.EventArgs):

        DEFINE VARIABLE oParameter AS GetBusinessEntityDescriptorParameter NO-UNDO .
        DEFINE VARIABLE hDataset   AS HANDLE                               NO-UNDO .
        DEFINE VARIABLE hBuffer    AS HANDLE                               NO-UNDO .
        DEFINE VARIABLE i          AS INTEGER                              NO-UNDO .

        coEntityView:ValueList:ValueListItems:Clear () .
        coEntityTable:ValueList:ValueListItems:Clear () .

        IF tbEntityName:Text > "":U THEN DO:

            oParameter = NEW GetBusinessEntityDescriptorParameter () .
            oParameter:ReturnDataset = TRUE .

            FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                           tbEntityName:Text,
                                                           "GetBusinessEntityDescriptor":U,
                                                           INPUT-OUTPUT DATASET-HANDLE hDataset,
                                                           oParameter) .

            IF VALID-OBJECT (oParameter:Descriptor) THEN DO:
                {Consultingwerk/foreachABL.i BusinessEntityViewDescriptor oView in oParameter:Descriptor:Views}
                    coEntityView:ValueList:ValueListItems:Add (oView:Name) .
                END.
            END.

            DO i = 1 TO hDataset:NUM-BUFFERS:
                ASSIGN hBuffer = hDataset:GET-BUFFER-HANDLE (i) .
                coEntityTable:ValueList:ValueListItems:Add (hBuffer:NAME) .
            END.

            coEntityView:DropDown () .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

        FINALLY:
            IF VALID-HANDLE (hDataset) THEN
                DELETE OBJECT hDataset .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the data shown in the UI
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID UpdateUiData ():

        DEFINE VARIABLE oCallParameter AS LaunchWebViewCallParameter NO-UNDO.

        IF NOT TYPE-OF (CallParameterObject, LaunchWebViewCallParameter) THEN
            RETURN.

        oCallParameter = CAST (CallParameterObject, LaunchWebViewCallParameter).

        ASSIGN
            coViewUri:Text     = oCallParameter:ViewUri
            tbParameter:Text   = oCallParameter:Parameter
            tbEntityName:Text  = oCallParameter:EntityName
            coEntityTable:Text = oCallParameter:EntityTable
            coEntityView:Text  = oCallParameter:EntityView
            tbViewTitle:Text   = oCallParameter:ViewTitle
            tbViewTables:Text  = oCallParameter:EntityViewTables
            .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the data of the CallParameter object after the user has
                 entered data to the UI
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID WritebackCallParameterToTextProperty ():

        DEFINE VARIABLE oCallParameter      AS LaunchWebViewCallParameter NO-UNDO .
        DEFINE VARIABLE cFormStartTypeValue AS CHARACTER                  NO-UNDO .

        /* Marko Rüterbories, Consultingwerk Ltd. 20.09.2013
           If the CallParameterObject is not valid or the datatype does not match the needs
           the instance gets deleted and a new instance of the correct type is created. (SCL-119)

           Invalid cast from Consultingwerk.Windows.Framework.LaunchWebViewCallParameter to
           Consultingwerk.SmartFramework.InvokeServiceMethodCallParameter. (12869) */
        IF NOT VALID-OBJECT (CallParameterObject) OR
           NOT TYPE-OF (CallParameterObject, LaunchWebViewCallParameter) THEN DO:
            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (CallParameterObject).
            CallParameterObject = NEW LaunchWebViewCallParameter ().
        END.

        oCallParameter = CAST (CallParameterObject, LaunchWebViewCallParameter).

        ASSIGN
            oCallParameter:ViewUri          = coViewUri:Text
            oCallParameter:Parameter        = tbParameter:Text
            oCallParameter:EntityName       = tbEntityName:Text
            oCallParameter:EntityTable      = coEntityTable:Text
            oCallParameter:EntityView       = coEntityView:Text
            oCallParameter:ViewTitle        = tbViewTitle:Text
            oCallParameter:EntityViewTables = tbViewTables:Text
            .

        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the LaunchWebViewCallParameterControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC LaunchWebViewCallParameterControl ():

    END DESTRUCTOR .

END CLASS.
