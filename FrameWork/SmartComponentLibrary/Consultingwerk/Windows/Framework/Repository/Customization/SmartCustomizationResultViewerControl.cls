/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SmartCustomizationResultViewerControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : 09.03.2016 10:54:26
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Repository.Customization.SmartCustomizationResultViewerControl 
    INHERITS SmartViewerControl: 
    
    DEFINE PRIVATE VARIABLE components                                                     AS System.ComponentModel.IContainer                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1                              AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartCustomizationResult_CustomizationResultCode_Label        AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartCustomizationResult_CustomizationResultCode              AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartCustomizationResult_CustomizationResultDescription_Label AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartCustomizationResult_CustomizationResultDescription       AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartCustomizationResult_CustomizationTypeCode_Label          AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityLookup1                                     AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup        NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartCustomizationResultViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartCustomizationResultViewerControl ():
                
        SUPER ().
        InitializeComponent ().
        
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
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartCustomizationResult":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Repository.Customization.SmartCustomizationResultViewerControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:eSmartCustomizationResult_CustomizationResultCode_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartCustomizationResult_CustomizationResultCode = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartCustomizationResult_CustomizationResultDescription_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartCustomizationResult_CustomizationResultDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartCustomizationResult_CustomizationTypeCode_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityLookup1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartCustomizationResult_CustomizationResultCode, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartCustomizationResult_CustomizationResultDescription, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.Repository.Customization.CustomizationBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartCustomizationResult":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 5 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("CustomizationResultGuid":U, "Customization Result":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("CustomizationResultCode":U, "Result Code":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("CustomizationResultDescription":U, "Description":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("CustomizationTypeGuid":U, "CustomizationTypeGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("CustomizationTypeCode":U, "Customization Type":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* eSmartCustomizationResult_CustomizationResultCode_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartCustomizationResult_CustomizationResultCode_Label:Appearance = appearance1.
        resources:ApplyResources(THIS-OBJECT:eSmartCustomizationResult_CustomizationResultCode_Label, "eSmartCustomizationResult_CustomizationResultCode_Label":U).
        THIS-OBJECT:eSmartCustomizationResult_CustomizationResultCode_Label:Name = "eSmartCustomizationResult_CustomizationResultCode_Label":U.
        /*  */
        /* eSmartCustomizationResult_CustomizationResultCode */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartCustomizationResult_CustomizationResultCode, "eSmartCustomizationResult_CustomizationResultCode":U).
        THIS-OBJECT:eSmartCustomizationResult_CustomizationResultCode:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "CustomizationResultCode":U, TRUE)).
        THIS-OBJECT:eSmartCustomizationResult_CustomizationResultCode:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartCustomizationResult_CustomizationResultCode:Name = "eSmartCustomizationResult_CustomizationResultCode":U.
        /*  */
        /* eSmartCustomizationResult_CustomizationResultDescription_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartCustomizationResult_CustomizationResultDescription_Label:Appearance = appearance2.
        resources:ApplyResources(THIS-OBJECT:eSmartCustomizationResult_CustomizationResultDescription_Label, "eSmartCustomizationResult_CustomizationResultDescription_Label":U).
        THIS-OBJECT:eSmartCustomizationResult_CustomizationResultDescription_Label:Name = "eSmartCustomizationResult_CustomizationResultDescription_Label":U.
        /*  */
        /* eSmartCustomizationResult_CustomizationResultDescription */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartCustomizationResult_CustomizationResultDescription, "eSmartCustomizationResult_CustomizationResultDescription":U).
        THIS-OBJECT:eSmartCustomizationResult_CustomizationResultDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "CustomizationResultDescription":U, TRUE)).
        THIS-OBJECT:eSmartCustomizationResult_CustomizationResultDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartCustomizationResult_CustomizationResultDescription:Name = "eSmartCustomizationResult_CustomizationResultDescription":U.
        /*  */
        /* eSmartCustomizationResult_CustomizationTypeCode_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartCustomizationResult_CustomizationTypeCode_Label:Appearance = appearance3.
        resources:ApplyResources(THIS-OBJECT:eSmartCustomizationResult_CustomizationTypeCode_Label, "eSmartCustomizationResult_CustomizationTypeCode_Label":U).
        THIS-OBJECT:eSmartCustomizationResult_CustomizationTypeCode_Label:Name = "eSmartCustomizationResult_CustomizationTypeCode_Label":U.
        /*  */
        /* smartBusinessEntityLookup1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityLookup1, "smartBusinessEntityLookup1":U).
        editorButton1:Key = "ShowLookupDialog":U.
        resources:ApplyResources(editorButton1, "editorButton1":U).
        THIS-OBJECT:smartBusinessEntityLookup1:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:smartBusinessEntityLookup1:DataBindings:Add(NEW System.Windows.Forms.Binding("LookupKeyValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "CustomizationTypeGuid":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup1:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "CustomizationTypeCode":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupBrowserFields = "CustomizationTypeCode,CustomizationTypeDescription,ServiceTypeName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogFilterFields = "CustomizationTypeCode,ServiceTypeName,ServiceMethodName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogFilterOperators = "BEGINSMATCHES,BEGINSMATCHES,BEGINSMATCHES":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityName = "Consultingwerk.SmartFramework.Repository.Customization.CustomizationTypeBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityTable = "eSmartCustomizationType":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyField = "CustomizationTypeCode":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyValueColumn = "CustomizationTypeGuid":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupQueryString = "FOR EACH eSmartCustomizationType WHERE eSmartCustomizationType.CustomizationTypeCode BEGINS ~"&1~"":U.
        THIS-OBJECT:smartBusinessEntityLookup1:Name = "smartBusinessEntityLookup1":U.
        THIS-OBJECT:smartBusinessEntityLookup1:OwningGrid = ?.
        /*  */
        /* SmartCustomizationResultViewerControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:smartBusinessEntityLookup1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartCustomizationResult_CustomizationResultCode_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartCustomizationResult_CustomizationResultCode).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartCustomizationResult_CustomizationResultDescription_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartCustomizationResult_CustomizationResultDescription).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartCustomizationResult_CustomizationTypeCode_Label).
        THIS-OBJECT:Name = "SmartCustomizationResultViewerControl":U.
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartCustomizationResult_CustomizationResultCode, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartCustomizationResult_CustomizationResultDescription, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartCustomizationResultViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartCustomizationResultViewerControl ():

        IF VALID-OBJECT (components) THEN DO:
            CAST (components, System.IDisposable):Dispose ().
        END.

    END DESTRUCTOR.

END CLASS.
