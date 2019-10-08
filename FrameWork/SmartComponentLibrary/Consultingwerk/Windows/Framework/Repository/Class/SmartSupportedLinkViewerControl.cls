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
    File        : SmartSupportedLinkViewerControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 03.05.2016 23:41:30
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Repository.Class.SmartSupportedLinkViewerControl 
    INHERITS SmartViewerControl: 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSupportedLink_LinkTypeGuid_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSupportedLink_LinkSource AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSupportedLink_LinkTarget AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSupportedLink_DeactivateOnHide AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityLookup1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartSupportedLinkViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartSupportedLinkViewerControl ():
                
        SUPER().
        InitializeComponent().
        
        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF
                
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
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartSupportedLink":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Repository.Class.SmartSupportedLinkViewerControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:eSmartSupportedLink_LinkTypeGuid_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSupportedLink_LinkSource = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartSupportedLink_LinkTarget = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartSupportedLink_DeactivateOnHide = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:smartBusinessEntityLookup1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSupportedLink_LinkSource, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSupportedLink_LinkTarget, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSupportedLink_DeactivateOnHide, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.Repository.Class.SupportedLinkBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartSupportedLink":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 7 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("SupportedLinkGuid":U, "SupportedLinkGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("ObjectTypeGuid":U, "ObjectTypeGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("LinkTypeGuid":U, "LinkTypeGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("LinkSource":U, "Supported as source":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("LinkTarget":U, "Supported as target":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("DeactivateOnHide":U, "DeactivateLinkOnHide":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[7] = NEW Progress.Data.ColumnPropDesc("LinkName":U, "Link Name":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* eSmartSupportedLink_LinkTypeGuid_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartSupportedLink_LinkTypeGuid_Label:Appearance = appearance1.
        resources:ApplyResources(THIS-OBJECT:eSmartSupportedLink_LinkTypeGuid_Label, "eSmartSupportedLink_LinkTypeGuid_Label":U).
        THIS-OBJECT:eSmartSupportedLink_LinkTypeGuid_Label:Name = "eSmartSupportedLink_LinkTypeGuid_Label":U.
        /*  */
        /* eSmartSupportedLink_LinkSource */
        /*  */
        THIS-OBJECT:eSmartSupportedLink_LinkSource:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "LinkSource":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartSupportedLink_LinkSource, "eSmartSupportedLink_LinkSource":U).
        THIS-OBJECT:eSmartSupportedLink_LinkSource:Name = "eSmartSupportedLink_LinkSource":U.
        THIS-OBJECT:eSmartSupportedLink_LinkSource:UseMnemonics = TRUE.
        /*  */
        /* eSmartSupportedLink_LinkTarget */
        /*  */
        THIS-OBJECT:eSmartSupportedLink_LinkTarget:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "LinkTarget":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartSupportedLink_LinkTarget, "eSmartSupportedLink_LinkTarget":U).
        THIS-OBJECT:eSmartSupportedLink_LinkTarget:Name = "eSmartSupportedLink_LinkTarget":U.
        THIS-OBJECT:eSmartSupportedLink_LinkTarget:UseMnemonics = TRUE.
        /*  */
        /* eSmartSupportedLink_DeactivateOnHide */
        /*  */
        THIS-OBJECT:eSmartSupportedLink_DeactivateOnHide:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DeactivateOnHide":U, TRUE)).
        resources:ApplyResources(THIS-OBJECT:eSmartSupportedLink_DeactivateOnHide, "eSmartSupportedLink_DeactivateOnHide":U).
        THIS-OBJECT:eSmartSupportedLink_DeactivateOnHide:Name = "eSmartSupportedLink_DeactivateOnHide":U.
        THIS-OBJECT:eSmartSupportedLink_DeactivateOnHide:UseMnemonics = TRUE.
        /*  */
        /* smartBusinessEntityLookup1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityLookup1, "smartBusinessEntityLookup1":U).
        editorButton1:Key = "ShowLookupDialog":U.
        resources:ApplyResources(editorButton1, "editorButton1":U).
        THIS-OBJECT:smartBusinessEntityLookup1:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:smartBusinessEntityLookup1:DataBindings:Add(NEW System.Windows.Forms.Binding("LookupKeyValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "LinkTypeGuid":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup1:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "LinkName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupBrowserFields = "LinkName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogFilterFields = "LinkName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogFilterOperators = "BEGINSMATCHES":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityName = "Consultingwerk.SmartFramework.Repository.Class.LinkTypeBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityTable = "eSmartLinkType":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyField = "LinkName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyValueColumn = "LinkTypeGuid":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupQueryString = "FOR EACH eSmartLinkType WHERE eSmartLinkType.LinkName BEGINS ~"&1~"":U.
        THIS-OBJECT:smartBusinessEntityLookup1:Name = "smartBusinessEntityLookup1":U.
        THIS-OBJECT:smartBusinessEntityLookup1:OwningGrid = ?.
        /*  */
        /* SmartSupportedLinkViewerControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityLookup1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSupportedLink_LinkTypeGuid_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSupportedLink_LinkSource).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSupportedLink_LinkTarget).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartSupportedLink_DeactivateOnHide).
        THIS-OBJECT:Name = "SmartSupportedLinkViewerControl":U.
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSupportedLink_LinkSource, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSupportedLink_LinkTarget, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSupportedLink_DeactivateOnHide, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.



    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartSupportedLinkViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartSupportedLinkViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
