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
    File        : SmartEntityFieldMappingViewerControl
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 05.11.2016 09:05:30
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH.
USING Consultingwerk.Framework.*                      FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Repository.Field.SmartEntityFieldMappingViewerControl
    INHERITS SmartViewerControl:

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartEntityFieldMapping_EntityName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartEntityFieldMapping_EntityName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartEntityFieldMapping_EntityTable_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartEntityFieldMapping_EntityTable AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartEntityFieldMapping_FieldName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartEntityFieldMapping_FieldName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartEntityFieldMapping_Environments_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartEntityFieldMapping_Environments AS Consultingwerk.Windows.Controls.MultilpleEntriesPickerEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartEntityFieldMapping_FieldObjectName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartEntityFieldMapping_FieldObjectName AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.

    DEFINE VARIABLE cObjectTypes AS CHARACTER NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartEntityFieldMappingViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartEntityFieldMappingViewerControl ():

        DEFINE VARIABLE oConfigurationProvider AS IConfigurationProvider NO-UNDO .

        SUPER().
        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        oConfigurationProvider = {Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider} .

        IF VALID-OBJECT (oConfigurationProvider) THEN
            cObjectTypes = oConfigurationProvider:GetValue("RepositoryFieldTypes":U, "SmartLookup,MetaLookup":U) .
        ELSE
            cObjectTypes = "SmartLookup,MetaLookup":U .

    END CONSTRUCTOR.


    /**
     * Purpose: Event handler for the NewFetchDataRequest event of the object name lookups
     *          Business Entity Adapter
     * Notes:   Adds the object types to the lookup using a named query
     * @param sender The reference to the object that raised the event
     * @param e The NewFetchDataRequestEventArgs with the data for this event
     */
    METHOD PRIVATE VOID NewFetchDataRequestHandler (sender AS Progress.Lang.Object, e AS NewFetchDataRequestEventArgs):

        DEFINE VARIABLE oQuery AS NamedQueryParameter NO-UNDO .

        oQuery = NEW NamedQueryParameter ("ObjectTypes":U) .
        oQuery:Parameters:Add ("ObjectTypes":U, cObjectTypes) .

        e:FetchDataRequest:NamedQuery = oQuery .


    END METHOD.

    /**
     * Purpose: Event handler for the EditorButtonClick of the eSmartEntityFieldMapping_EntityName
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The EditorButtonEventArgs with the data for this event
     */
    @VisualDesigner.
    METHOD PRIVATE VOID eSmartEntityFieldMapping_EntityName_EditorButtonClick (sender AS System.Object,
                                                                               e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        DEFINE VARIABLE cEntityName AS CHARACTER NO-UNDO.

        Consultingwerk.SmartComponents.Support.BusinessEntityDesignerSupport:SelectBusinessEntityName (OUTPUT cEntityName).

        IF cEntityName > "":U THEN
            ASSIGN eSmartEntityFieldMapping_EntityName:Text = cEntityName .

    END METHOD .

    /**
     * Purpose: Event handler for the EditorButtonClick of the eSmartEntityFieldMapping_EntityTable
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The EditorButtonEventArgs with the data for this event
     */
    @VisualDesigner.
    METHOD PRIVATE VOID eSmartEntityFieldMapping_EntityTable_EditorButtonClick (sender AS System.Object,
                                                                                e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        DEFINE VARIABLE cTables      AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cEntityJoin  AS CHARACTER NO-UNDO .
        DEFINE VARIABLE hDataHandle  AS HANDLE    NO-UNDO .

        ASSIGN cTables = eSmartEntityFieldMapping_EntityTable:Text .

        FrameworkSettings:ServiceAdapter:FetchDataset ("":U,
                                                      eSmartEntityFieldMapping_EntityName:Text,
                                                      OUTPUT DATASET-HANDLE hDataHandle BY-REFERENCE) .

        ASSIGN cEntityJoin = TRIM (FILL ("yes,":U, NUM-ENTRIES (cTables)), ",":U) .

        RUN Consultingwerk/SmartComponents/Support/SmartBusinessEntityAdapterDesigner_ShowBusinessEntityTblPic.p
                (hDataHandle,
                 eSmartEntityFieldMapping_EntityTable:Text,
                 OUTPUT cTables,
                 INPUT-OUTPUT cEntityJoin) .

        IF cTables > "":U THEN
            ASSIGN eSmartEntityFieldMapping_EntityTable:Text = cTables .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hDataHandle) .
        END FINALLY.

    END METHOD .

    /**
     * Purpose:
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The AfterInitializeBusinessEntityAdapterEventArgs with the data for this event
     */
    @VisualDesigner.
    METHOD PRIVATE VOID eSmartEntityFieldMapping_FieldObjectName_AfterInitializeBusinessEntityAdapter (sender AS System.Object,
                                                                                                       e AS AfterInitializeBusinessEntityAdapterEventArgs):

        e:BusinessEntityAdapter:NewFetchDataRequest:Subscribe (NewFetchDataRequestHandler) .

    END METHOD .

    /**
     * Purpose: Event handler for the EditorButtonClick of the eSmartEntityFieldMapping_FieldObjectName
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The EditorButtonEventArgs with the data for this event
     */
    @VisualDesigner.
    METHOD PRIVATE VOID eSmartEntityFieldMapping_FieldObjectName_EditorButtonClick (sender AS System.Object,
                                                                                    e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        RETURN.

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
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartEntityFieldMapping":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Repository.Field.SmartEntityFieldMappingViewerControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton2 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton2 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
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
        DEFINE VARIABLE editorButton3 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton3 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:eSmartEntityFieldMapping_EntityName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartEntityFieldMapping_EntityName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartEntityFieldMapping_EntityTable_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartEntityFieldMapping_EntityTable = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartEntityFieldMapping_FieldName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartEntityFieldMapping_FieldName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartEntityFieldMapping_Environments_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartEntityFieldMapping_Environments = NEW Consultingwerk.Windows.Controls.MultilpleEntriesPickerEditor().
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartEntityFieldMapping_EntityName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartEntityFieldMapping_EntityTable, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartEntityFieldMapping_FieldName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartEntityFieldMapping_Environments, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.Repository.Field.EntityFieldMappingBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartEntityFieldMapping":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 6 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("EntityFieldMappingGuid":U, "EntityFieldMappingGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("EntityName":U, "Entity Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("EntityTable":U, "Table Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("FieldName":U, "Field Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("Environments":U, "Environments":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("FieldObjectName":U, "Object Name":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* eSmartEntityFieldMapping_EntityName_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartEntityFieldMapping_EntityName_Label:Appearance = appearance1.
        resources:ApplyResources(THIS-OBJECT:eSmartEntityFieldMapping_EntityName_Label, "eSmartEntityFieldMapping_EntityName_Label":U).
        THIS-OBJECT:eSmartEntityFieldMapping_EntityName_Label:Name = "eSmartEntityFieldMapping_EntityName_Label":U.
        /*  */
        /* eSmartEntityFieldMapping_EntityName */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartEntityFieldMapping_EntityName, "eSmartEntityFieldMapping_EntityName":U).
        resources:ApplyResources(editorButton1, "editorButton1":U).
        THIS-OBJECT:eSmartEntityFieldMapping_EntityName:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:eSmartEntityFieldMapping_EntityName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "EntityName":U, TRUE)).
        THIS-OBJECT:eSmartEntityFieldMapping_EntityName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartEntityFieldMapping_EntityName:Name = "eSmartEntityFieldMapping_EntityName":U.
        THIS-OBJECT:eSmartEntityFieldMapping_EntityName:EditorButtonClick:Subscribe(THIS-OBJECT:eSmartEntityFieldMapping_EntityName_EditorButtonClick).
        /*  */
        /* eSmartEntityFieldMapping_EntityTable_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartEntityFieldMapping_EntityTable_Label:Appearance = appearance2.
        resources:ApplyResources(THIS-OBJECT:eSmartEntityFieldMapping_EntityTable_Label, "eSmartEntityFieldMapping_EntityTable_Label":U).
        THIS-OBJECT:eSmartEntityFieldMapping_EntityTable_Label:Name = "eSmartEntityFieldMapping_EntityTable_Label":U.
        /*  */
        /* eSmartEntityFieldMapping_EntityTable */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartEntityFieldMapping_EntityTable, "eSmartEntityFieldMapping_EntityTable":U).
        resources:ApplyResources(editorButton2, "editorButton2":U).
        THIS-OBJECT:eSmartEntityFieldMapping_EntityTable:ButtonsRight:Add(editorButton2).
        THIS-OBJECT:eSmartEntityFieldMapping_EntityTable:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "EntityTable":U, TRUE)).
        THIS-OBJECT:eSmartEntityFieldMapping_EntityTable:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartEntityFieldMapping_EntityTable:Name = "eSmartEntityFieldMapping_EntityTable":U.
        THIS-OBJECT:eSmartEntityFieldMapping_EntityTable:EditorButtonClick:Subscribe(THIS-OBJECT:eSmartEntityFieldMapping_EntityTable_EditorButtonClick).
        /*  */
        /* eSmartEntityFieldMapping_FieldName_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldName_Label:Appearance = appearance3.
        resources:ApplyResources(THIS-OBJECT:eSmartEntityFieldMapping_FieldName_Label, "eSmartEntityFieldMapping_FieldName_Label":U).
        THIS-OBJECT:eSmartEntityFieldMapping_FieldName_Label:Name = "eSmartEntityFieldMapping_FieldName_Label":U.
        /*  */
        /* eSmartEntityFieldMapping_FieldName */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartEntityFieldMapping_FieldName, "eSmartEntityFieldMapping_FieldName":U).
        THIS-OBJECT:eSmartEntityFieldMapping_FieldName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "FieldName":U, TRUE)).
        THIS-OBJECT:eSmartEntityFieldMapping_FieldName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldName:Name = "eSmartEntityFieldMapping_FieldName":U.
        /*  */
        /* eSmartEntityFieldMapping_Environments_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartEntityFieldMapping_Environments_Label:Appearance = appearance4.
        resources:ApplyResources(THIS-OBJECT:eSmartEntityFieldMapping_Environments_Label, "eSmartEntityFieldMapping_Environments_Label":U).
        THIS-OBJECT:eSmartEntityFieldMapping_Environments_Label:Name = "eSmartEntityFieldMapping_Environments_Label":U.
        /*  */
        /* eSmartEntityFieldMapping_Environments */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartEntityFieldMapping_Environments, "eSmartEntityFieldMapping_Environments":U).
        THIS-OBJECT:eSmartEntityFieldMapping_Environments:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "Environments":U, TRUE)).
        THIS-OBJECT:eSmartEntityFieldMapping_Environments:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartEntityFieldMapping_Environments:Name = "eSmartEntityFieldMapping_Environments":U.
        /*  */
        /* eSmartEntityFieldMapping_FieldObjectName_Label */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName_Label:Appearance = appearance5.
        resources:ApplyResources(THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName_Label, "eSmartEntityFieldMapping_FieldObjectName_Label":U).
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName_Label:Name = "eSmartEntityFieldMapping_FieldObjectName_Label":U.
        /*  */
        /* eSmartEntityFieldMapping_FieldObjectName */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName, "eSmartEntityFieldMapping_FieldObjectName":U).
        editorButton3:Key = "ShowLookupDialog":U.
        resources:ApplyResources(editorButton3, "editorButton3":U).
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:ButtonsRight:Add(editorButton3).
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "FieldObjectName":U, TRUE)).
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:LookupAdditionalFields = "":U.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:LookupBrowserExcludeFields = "":U.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:LookupBrowserFields = "ObjectName,ModuleName,ObjectTypeName":U.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:LookupCascade = FALSE.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:LookupControls = "":U.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:LookupDialogClassName = "":U.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:LookupDialogExpandLastColumn = TRUE.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:LookupDialogFilterFields = "ObjectName":U.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:LookupDialogFilterOperators = "BEGINSMATCHES":U.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:LookupDialogOpenQuery = TRUE.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:LookupDialogQuerySort = "":U.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:LookupDialogQueryString = "":U.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:LookupEntityName = "Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity":U.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:LookupEntityTable = "eSmartObjectMaster":U.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:LookupEntityView = "":U.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:LookupFields = "":U.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:LookupKeyField = "ObjectName":U.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:LookupKeyValue = "":U.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:LookupKeyValueColumn = "":U.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:LookupQuerySort = "":U.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:LookupQueryString = "FOR EACH eSmartObjectMaster WHERE eSmartObjectMaster.ObjectName BEGINS ~"&1~"":U.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:LookupTimerDelay = 750.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:LookupTimerDisabled = FALSE.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:Name = "eSmartEntityFieldMapping_FieldObjectName":U.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:OwningGrid = ?.
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:AfterInitializeBusinessEntityAdapter:Subscribe(THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName_AfterInitializeBusinessEntityAdapter).
        THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName:EditorButtonClick:Subscribe(THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName_EditorButtonClick).
        /*  */
        /* SmartEntityFieldMappingViewerControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartEntityFieldMapping_EntityName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartEntityFieldMapping_EntityName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartEntityFieldMapping_EntityTable_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartEntityFieldMapping_EntityTable).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartEntityFieldMapping_FieldName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartEntityFieldMapping_FieldName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartEntityFieldMapping_Environments_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartEntityFieldMapping_Environments).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName).
        THIS-OBJECT:Name = "SmartEntityFieldMappingViewerControl":U.
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartEntityFieldMapping_EntityName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartEntityFieldMapping_EntityTable, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartEntityFieldMapping_FieldName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartEntityFieldMapping_Environments, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartEntityFieldMapping_FieldObjectName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartEntityFieldMappingViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartEntityFieldMappingViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
