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
    File        : SmartObjectInstanceViewerControl
    Purpose     : Viewer Control for Object Instances
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 30.11.2016 21:36:02
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                      FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*     FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Infragistics.Win.*                              FROM ASSEMBLY .
USING Infragistics.Win.UltraWinEditors.*              FROM ASSEMBLY .
USING Infragistics.Win.UltraWinGrid.*                 FROM ASSEMBLY .
USING Progress.Lang.*                                 FROM PROPATH .
USING System.ComponentModel.*                         FROM ASSEMBLY .

CLASS Consultingwerk.Windows.Framework.Repository.Object.SmartObjectInstanceViewerControl
    INHERITS SmartViewerControl:

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectInstance_ObjectSequence_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectInstance_ObjectSequence AS Infragistics.Win.UltraWinEditors.UltraNumericEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectInstance_ObjectMasterGuid_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectInstance_ObjectMasterGuid AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectInstance_InstanceName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectInstance_InstanceName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectInstance_InstanceDescription_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectInstance_InstanceDescription AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectInstance_PageGuid_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectInstance_PageGuid AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectInstance_ParentInstanceGuid_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectInstance_ParentInstanceGuid AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectInstance_LayoutPosition_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartObjectInstance_LayoutPosition AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.

    /**
     * Purpose: Gets and sets the reference to the Form's Combo Editor used
     *          for filtering the object instances by page
     * Notes:
     */
    DEFINE PUBLIC PROPERTY FilterComboEditor AS UltraComboEditor NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartObjectInstanceViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartObjectInstanceViewerControl ():

        SUPER().
        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        THIS-OBJECT:SetControlEnabled (eSmartObjectInstance_ObjectMasterGuid, ControlEnabledEnum:Add) .

    END CONSTRUCTOR.

    /**
     * Purpose: Adds a new eSmartObjectInstance record
     * Notes:
     */
    METHOD PUBLIC OVERRIDE VOID AddRecord():

        DEFINE VARIABLE hDataset   AS HANDLE  NO-UNDO .
        DEFINE VARIABLE hBuffer    AS HANDLE  NO-UNDO .
        DEFINE VARIABLE iSequence  AS INTEGER NO-UNDO INIT 1 .
        DEFINE VARIABLE lFindFirst AS LOGICAL NO-UNDO .

        SUPER:AddRecord ().

        IF VALID-OBJECT (THIS-OBJECT:FilterComboEditor) THEN DO:

            IF THIS-OBJECT:FilterComboEditor:SelectedIndex > 0 THEN
                ASSIGN eSmartObjectInstance_PageGuid:Value = THIS-OBJECT:FilterComboEditor:Value .

            ASSIGN hDataset = CAST (THIS-OBJECT:SmartDataSource, SmartDatasetAdapter):DataSet .

            IF VALID-HANDLE (hDataset) THEN DO:

                CREATE BUFFER hBuffer FOR TABLE hDataset:GET-BUFFER-HANDLE ("eSmartObjectInstance":U) .

                objectSequenceLoop:
                REPEAT:
                    ASSIGN lFindFirst = hBuffer:FIND-FIRST (SUBSTITUTE ("where PageGuid = &1 and ObjectSequence = &2":U,
                                                                        QUOTER (UNBOX (eSmartObjectInstance_PageGuid:Value)),
                                                                        QUOTER (iSequence))) NO-ERROR .

                    IF lFindFirst = FALSE THEN DO:
                        ASSIGN eSmartObjectInstance_ObjectSequence:Value = BOX (iSequence) .
                        LEAVE objectSequenceLoop .
                    END.

                    iSequence = iSequence + 1 .

                    FINALLY:
                        ErrorHelper:ResetErrorStatus() .
                    END FINALLY.
                END.
            END.
        END.

        FINALLY:
            GarbageCollectorHelper:DeleteObject(hBuffer) .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Updates the value list of the Pages Combo Editor
     * Notes:
     */
    METHOD PUBLIC VOID UpdatePagesComboEditor():

        DEFINE VARIABLE hDataset     AS HANDLE            NO-UNDO .
        DEFINE VARIABLE hBuffer      AS HANDLE            NO-UNDO .
        DEFINE VARIABLE hQuery       AS HANDLE            NO-UNDO .
        DEFINE VARIABLE oValueList1  AS ValueList         NO-UNDO .
        DEFINE VARIABLE oValueList2  AS ValueList         NO-UNDO .
        DEFINE VARIABLE oValueList3  AS ValueList         NO-UNDO .
        DEFINE VARIABLE oDataBrowser AS ISmartDataBrowser NO-UNDO .
        DEFINE VARIABLE oGrid        AS UltraGrid         NO-UNDO .

        ASSIGN hDataset = CAST (THIS-OBJECT:SmartDataSource, SmartDatasetAdapter):DataSet .

        DatasetAssert:HasBuffer(hDataset, "eSmartPage":U) .

        CREATE BUFFER hBuffer FOR TABLE hDataset:GET-BUFFER-HANDLE ("eSmartPage":U) .

        ASSIGN hQuery      = QueryHelper:CreatePreparedQuery(hBuffer, "for each eSmartPage by eSmartPage.PageSequence":U)

               oValueList1 = NEW ValueList ()
               oValueList2 = NEW ValueList ().

        oValueList1:ValueListItems:Add (BOX ("":U), "Container":U) .
        oValueList2:ValueListItems:Add (BOX ("":U), "Container":U) .

        IF VALID-OBJECT (THIS-OBJECT:FilterComboEditor) THEN DO:
            ASSIGN oValueList3 = NEW ValueList ().
            oValueList3:ValueListItems:Add (BOX (-1), "All Pages":U) .
            oValueList3:ValueListItems:Add (BOX ("":U), "Container":U) .
        END.

        DO WHILE NOT hQuery:QUERY-OFF-END:
            oValueList1:ValueListItems:Add (BOX (hBuffer::PageGuid), SUBSTITUTE ("(&1) &2":U,
                                                                                 hBuffer::PageSequence,
                                                                                 hBuffer::PageLabel)) .
            oValueList2:ValueListItems:Add (BOX (hBuffer::PageGuid), SUBSTITUTE ("(&1) &2":U,
                                                                                 hBuffer::PageSequence,
                                                                                 hBuffer::PageLabel)) .

            IF VALID-OBJECT (oValueList3) THEN
                oValueList3:ValueListItems:Add (BOX (hBuffer::PageGuid), SUBSTITUTE ("(&1) &2":U,
                                                                                     hBuffer::PageSequence,
                                                                                     hBuffer::PageLabel)) .

            hQuery:GET-NEXT () .
        END.

        eSmartObjectInstance_PageGuid:ValueList = oValueList1 .

        IF VALID-OBJECT (THIS-OBJECT:FilterComboEditor) THEN DO:
            THIS-OBJECT:FilterComboEditor:ValueList = oValueList3 .

            IF THIS-OBJECT:FilterComboEditor:SelectedIndex < 0 THEN
                THIS-OBJECT:FilterComboEditor:SelectedIndex = 0 .
        END.

        oDataBrowser = CAST (THIS-OBJECT:SmartDataSource, SmartDatasetAdapter):FindLinkedSmartDataBrowser() .

        IF VALID-OBJECT (oDataBrowser) THEN DO:
            oGrid = CAST (CAST (oDataBrowser, Progress.Lang.Object), UltraGrid) .

            IF oGrid:DisplayLayout:Bands[0]:Columns:Exists ("PageGuid":U) THEN
                oGrid:DisplayLayout:Bands[0]:Columns["PageGuid":U]:ValueList = oValueList2 .
        END.

        IF THIS-OBJECT:SmartDataSource:Available THEN
            eSmartObjectInstance_PageGuid:Value = THIS-OBJECT:SmartDataSource:GetFieldValues ("PageGuid":U) .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hQuery) .
            GarbageCollectorHelper:DeleteObject (hBuffer) .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Event handler for the Leave event of the eSmartObjectInstance_ObjectMasterGuid
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PRIVATE VOID eSmartObjectInstance_ObjectMasterGuid_Leave (sender AS System.Object,
                                                                     e AS System.EventArgs):

        IF eSmartObjectInstance_ObjectMasterGuid:Enabled AND
           eSmartObjectInstance_ObjectMasterGuid:Text > "":U AND
           eSmartObjectInstance_InstanceName:Text = "":U AND
           eSmartObjectInstance_InstanceName:Enabled THEN

            eSmartObjectInstance_InstanceName:Text = ListHelper:LastEntry (eSmartObjectInstance_ObjectMasterGuid:Text, ".":U) .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

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
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartObjectInstance":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Repository.Object.SmartObjectInstanceViewerControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
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
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:eSmartObjectInstance_ObjectSequence_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartObjectInstance_ObjectSequence = NEW Infragistics.Win.UltraWinEditors.UltraNumericEditor().
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:eSmartObjectInstance_InstanceName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartObjectInstance_InstanceName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartObjectInstance_InstanceDescription_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartObjectInstance_InstanceDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartObjectInstance_PageGuid_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartObjectInstance_PageGuid = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:eSmartObjectInstance_ParentInstanceGuid_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartObjectInstance_ParentInstanceGuid = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartObjectInstance_LayoutPosition_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartObjectInstance_LayoutPosition = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartObjectInstance_ObjectSequence, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartObjectInstance_InstanceName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartObjectInstance_InstanceDescription, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartObjectInstance_PageGuid, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartObjectInstance_ParentInstanceGuid, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartObjectInstance_LayoutPosition, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartObjectInstance":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 10 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("ObjectInstanceGuid":U, "ObjectInstanceGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("ContainerObjectMasterGuid":U, "ContainerObjectMasterGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("ObjectMasterGuid":U, "ObjectMasterGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("ObjectMasterName":U, "Object Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("ParentInstanceGuid":U, "Parent Instance":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("LayoutPosition":U, "Layout Position":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[7] = NEW Progress.Data.ColumnPropDesc("InstanceName":U, "Instance Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[8] = NEW Progress.Data.ColumnPropDesc("InstanceDescription":U, "Description":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[9] = NEW Progress.Data.ColumnPropDesc("PageGuid":U, "Page":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[10] = NEW Progress.Data.ColumnPropDesc("ObjectSequence":U, "Object Sequence":U, Progress.Data.DataType:INTEGER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* eSmartObjectInstance_ObjectSequence_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartObjectInstance_ObjectSequence_Label:Appearance = appearance1.
        resources:ApplyResources(THIS-OBJECT:eSmartObjectInstance_ObjectSequence_Label, "eSmartObjectInstance_ObjectSequence_Label":U).
        THIS-OBJECT:eSmartObjectInstance_ObjectSequence_Label:Name = "eSmartObjectInstance_ObjectSequence_Label":U.
        /*  */
        /* eSmartObjectInstance_ObjectSequence */
        /*  */
        THIS-OBJECT:eSmartObjectInstance_ObjectSequence:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ObjectSequence":U, TRUE)).
        THIS-OBJECT:eSmartObjectInstance_ObjectSequence:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:eSmartObjectInstance_ObjectSequence, "eSmartObjectInstance_ObjectSequence":U).
        THIS-OBJECT:eSmartObjectInstance_ObjectSequence:Name = "eSmartObjectInstance_ObjectSequence":U.
        /*  */
        /* eSmartObjectInstance_ObjectMasterGuid_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid_Label:Appearance = appearance2.
        resources:ApplyResources(THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid_Label, "eSmartObjectInstance_ObjectMasterGuid_Label":U).
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid_Label:Name = "eSmartObjectInstance_ObjectMasterGuid_Label":U.
        /*  */
        /* eSmartObjectInstance_ObjectMasterGuid */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid, "eSmartObjectInstance_ObjectMasterGuid":U).
        editorButton1:Key = "ShowLookupDialog":U.
        resources:ApplyResources(editorButton1, "editorButton1":U).
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:DataBindings:Add(NEW System.Windows.Forms.Binding("LookupKeyValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ObjectMasterGuid":U, TRUE)).
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ObjectMasterName":U, TRUE)).
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:LookupAdditionalFields = "":U.
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:LookupBrowserExcludeFields = "":U.
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:LookupBrowserFields = "ObjectName,ObjectDescription,ModuleName,ObjectTypeName":U.
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:LookupCascade = FALSE.
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:LookupControls = "":U.
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:LookupDialogClassName = "":U.
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:LookupDialogExpandLastColumn = TRUE.
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:LookupDialogFilterFields = "ObjectName,ModuleName,ObjectTypeName":U.
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:LookupDialogFilterOperators = "BEGINSMATCHES,BEGINSMATCHES,BEGINSMATCHES":U.
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:LookupDialogOpenQuery = TRUE.
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:LookupDialogQuerySort = "BY eSmartObjectMaster.ObjectName":U.
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:LookupDialogQueryString = "":U.
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:LookupEntityName = "Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity":U.
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:LookupEntityTable = "eSmartObjectMaster":U.
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:LookupEntityView = "":U.
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:LookupFields = "":U.
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:LookupKeyField = "ObjectName":U.
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:LookupKeyValue = "":U.
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:LookupKeyValueColumn = "ObjectMasterGuid":U.
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:LookupQuerySort = "":U.
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:LookupQueryString = "FOR EACH eSmartObjectMaster WHERE eSmartObjectMaster.ObjectName BEGINS ~"&1~"":U.
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:LookupTimerDelay = 750.
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:LookupTimerDisabled = FALSE.
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:Name = "eSmartObjectInstance_ObjectMasterGuid":U.
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:OwningGrid = ?.
        THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid:Leave:Subscribe(THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid_Leave).
        /*  */
        /* eSmartObjectInstance_InstanceName_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartObjectInstance_InstanceName_Label:Appearance = appearance3.
        resources:ApplyResources(THIS-OBJECT:eSmartObjectInstance_InstanceName_Label, "eSmartObjectInstance_InstanceName_Label":U).
        THIS-OBJECT:eSmartObjectInstance_InstanceName_Label:Name = "eSmartObjectInstance_InstanceName_Label":U.
        /*  */
        /* eSmartObjectInstance_InstanceName */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartObjectInstance_InstanceName, "eSmartObjectInstance_InstanceName":U).
        THIS-OBJECT:eSmartObjectInstance_InstanceName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "InstanceName":U, TRUE)).
        THIS-OBJECT:eSmartObjectInstance_InstanceName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartObjectInstance_InstanceName:Name = "eSmartObjectInstance_InstanceName":U.
        /*  */
        /* eSmartObjectInstance_InstanceDescription_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartObjectInstance_InstanceDescription_Label:Appearance = appearance4.
        resources:ApplyResources(THIS-OBJECT:eSmartObjectInstance_InstanceDescription_Label, "eSmartObjectInstance_InstanceDescription_Label":U).
        THIS-OBJECT:eSmartObjectInstance_InstanceDescription_Label:Name = "eSmartObjectInstance_InstanceDescription_Label":U.
        /*  */
        /* eSmartObjectInstance_InstanceDescription */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartObjectInstance_InstanceDescription, "eSmartObjectInstance_InstanceDescription":U).
        THIS-OBJECT:eSmartObjectInstance_InstanceDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "InstanceDescription":U, TRUE)).
        THIS-OBJECT:eSmartObjectInstance_InstanceDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartObjectInstance_InstanceDescription:Name = "eSmartObjectInstance_InstanceDescription":U.
        /*  */
        /* eSmartObjectInstance_PageGuid_Label */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartObjectInstance_PageGuid_Label:Appearance = appearance5.
        resources:ApplyResources(THIS-OBJECT:eSmartObjectInstance_PageGuid_Label, "eSmartObjectInstance_PageGuid_Label":U).
        THIS-OBJECT:eSmartObjectInstance_PageGuid_Label:Name = "eSmartObjectInstance_PageGuid_Label":U.
        /*  */
        /* eSmartObjectInstance_PageGuid */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartObjectInstance_PageGuid, "eSmartObjectInstance_PageGuid":U).
        THIS-OBJECT:eSmartObjectInstance_PageGuid:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "PageGuid":U, TRUE)).
        THIS-OBJECT:eSmartObjectInstance_PageGuid:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartObjectInstance_PageGuid:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:eSmartObjectInstance_PageGuid:Name = "eSmartObjectInstance_PageGuid":U.
        /*  */
        /* eSmartObjectInstance_ParentInstanceGuid_Label */
        /*  */
        appearance6:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartObjectInstance_ParentInstanceGuid_Label:Appearance = appearance6.
        resources:ApplyResources(THIS-OBJECT:eSmartObjectInstance_ParentInstanceGuid_Label, "eSmartObjectInstance_ParentInstanceGuid_Label":U).
        THIS-OBJECT:eSmartObjectInstance_ParentInstanceGuid_Label:Name = "eSmartObjectInstance_ParentInstanceGuid_Label":U.
        /*  */
        /* eSmartObjectInstance_ParentInstanceGuid */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartObjectInstance_ParentInstanceGuid, "eSmartObjectInstance_ParentInstanceGuid":U).
        THIS-OBJECT:eSmartObjectInstance_ParentInstanceGuid:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ParentInstanceGuid":U, TRUE)).
        THIS-OBJECT:eSmartObjectInstance_ParentInstanceGuid:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartObjectInstance_ParentInstanceGuid:Name = "eSmartObjectInstance_ParentInstanceGuid":U.
        /*  */
        /* eSmartObjectInstance_LayoutPosition_Label */
        /*  */
        appearance7:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartObjectInstance_LayoutPosition_Label:Appearance = appearance7.
        resources:ApplyResources(THIS-OBJECT:eSmartObjectInstance_LayoutPosition_Label, "eSmartObjectInstance_LayoutPosition_Label":U).
        THIS-OBJECT:eSmartObjectInstance_LayoutPosition_Label:Name = "eSmartObjectInstance_LayoutPosition_Label":U.
        /*  */
        /* eSmartObjectInstance_LayoutPosition */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartObjectInstance_LayoutPosition, "eSmartObjectInstance_LayoutPosition":U).
        THIS-OBJECT:eSmartObjectInstance_LayoutPosition:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "LayoutPosition":U, TRUE)).
        THIS-OBJECT:eSmartObjectInstance_LayoutPosition:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartObjectInstance_LayoutPosition:Name = "eSmartObjectInstance_LayoutPosition":U.
        /*  */
        /* SmartObjectInstanceViewerControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectInstance_ObjectSequence_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectInstance_ObjectSequence).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectInstance_InstanceName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectInstance_InstanceName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectInstance_InstanceDescription_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectInstance_InstanceDescription).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectInstance_PageGuid_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectInstance_PageGuid).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectInstance_ParentInstanceGuid_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectInstance_ParentInstanceGuid).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectInstance_LayoutPosition_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartObjectInstance_LayoutPosition).
        THIS-OBJECT:FocusControlOnAdd = THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid.
        THIS-OBJECT:FocusControlOnUpdate = THIS-OBJECT:eSmartObjectInstance_ObjectSequence.
        THIS-OBJECT:Name = "SmartObjectInstanceViewerControl":U.
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartObjectInstance_ObjectSequence, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartObjectInstance_InstanceName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartObjectInstance_InstanceDescription, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartObjectInstance_PageGuid, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartObjectInstance_ParentInstanceGuid, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartObjectInstance_LayoutPosition, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /**
     * Purpose: Raises the OnBeforeSaveChanges event
     * Notes:
     *
     * @param e The CancelEventArgs with the data for this event
     */
    METHOD PROTECTED OVERRIDE VOID OnBeforeSaveChanges (e AS CancelEventArgs):

        SUPER:OnBeforeSaveChanges(e).

        IF eSmartObjectInstance_ObjectMasterGuid:Focused THEN
            THIS-OBJECT:eSmartObjectInstance_ObjectMasterGuid_Leave (eSmartObjectInstance_ObjectMasterGuid,
                                                                     System.EventArgs:Empty) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartObjectInstanceViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartObjectInstanceViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
