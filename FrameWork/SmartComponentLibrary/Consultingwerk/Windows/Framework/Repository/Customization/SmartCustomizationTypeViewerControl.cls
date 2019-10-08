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
    File        : SmartCustomizationTypeViewerControl
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : 21.05.2014 11:04:23
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Assertion.*                                  FROM PROPATH.
USING Consultingwerk.Framework.*                                  FROM PROPATH.
USING Consultingwerk.SmartComponents.Base.*                       FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*                       FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.*             FROM PROPATH .
USING Consultingwerk.SmartComponents.Support.*                    FROM PROPATH .
USING Consultingwerk.Util.*                                       FROM PROPATH .
USING Consultingwerk.Windows.Framework.Repository.Customization.* FROM PROPATH .
USING Infragistics.Win.*                                          FROM ASSEMBLY .
USING Infragistics.Win.Misc.*                                     FROM ASSEMBLY .
USING Progress.Lang.*                                             FROM PROPATH .
USING Progress.Reflect.*                                          FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Repository.Customization.SmartCustomizationTypeViewerControl
    INHERITS SmartViewerControl:

    DEFINE PRIVATE VARIABLE components                                                 AS System.ComponentModel.IContainer                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1                          AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartCustomizationType_CustomizationTypeCode_Label        AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartCustomizationType_CustomizationTypeCode              AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartCustomizationType_CustomizationTypeDescription_Label AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartCustomizationType_CustomizationTypeDescription       AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartCustomizationType_ServiceTypeName_Label              AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartCustomizationType_ServiceTypeName                    AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartCustomizationType_ServiceMethodName_Label            AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraComboEditor1                                          AS Infragistics.Win.UltraWinEditors.UltraComboEditor                              NO-UNDO.

    DEFINE VARIABLE oMethodValueList AS ValueList            NO-UNDO.

    {Consultingwerk/Util/TempTables/ttClassNames.i}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartCustomizationTypeViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartCustomizationTypeViewerControl ():

        SUPER ().
        InitializeComponent ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Display Dialog to select a Class or Interface from the project as the
                 ServiceType
        Notes:
        @param sender The control invoking the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID eSmartCustomizationType_ServiceTypeName_EditorButtonClick (sender AS System.Object,
                                                                                   e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        DEFINE VARIABLE oForm         AS BusinessEntityPickerForm          NO-UNDO .
        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult NO-UNDO .
        DEFINE VARIABLE oInfo         AS UltraDesktopAlertWindowInfo       NO-UNDO .

        oInfo = Consultingwerk.SmartComponents.Support.DesktopAlertHelper:Show ("SmartFramework"{&TRAN},
                                                                                "Scanning Project Folder(s) for Service Types."{&TRAN}) .

        Consultingwerk.Util.ClassHelper:GetClassNamesInClassPath ("Progress.Lang.Object"{&TRAN},
                                                                  TRUE,
                                                                  OUTPUT TABLE ttClassNames BY-REFERENCE) .

        oInfo:Close () .

        oForm = NEW BusinessEntityPickerForm () .
        oForm:Text = "Select Service Type Name"{&TRAN} .

        oForm:ReceiveBusinessEntityNames (TABLE ttClassNames) .

        ASSIGN oDialogResult = Consultingwerk.SmartComponents.FormHelper:ShowDialog(oForm) .

        IF Progress.Util.EnumHelper:AreEqual(oDialogResult,
                                             System.Windows.Forms.DialogResult:Ok) THEN

            ASSIGN THIS-OBJECT:eSmartCustomizationType_ServiceTypeName:Text = oForm:GetSelectedBusinessEntityName () .

        CATCH ple AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (ple).
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a Progress.Lang.Class object identified by ClassName
        Notes:
        @param pcClassName Name of the requested Service Type
        @return The Progress.Lang.Class object
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE Progress.Lang.Class GetClassOfType (pcClassName AS CHARACTER):

        DEFINE VARIABLE oClass AS Progress.Lang.Class NO-UNDO.

        IF pcClassName = "":U THEN
            UNDO, THROW NEW AppError ("Please provide a Service Type Name!"{&TRAN}, 0).

        oClass = Progress.Lang.Class:GetClass (pcClassName) NO-ERROR.

        IF NOT VALID-OBJECT (oClass) THEN
            UNDO, THROW NEW AppError ("Please provide a valid Service Type Name!"{&TRAN}, 0).

        RETURN oClass.

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
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartCustomizationType":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Repository.Customization.SmartCustomizationTypeViewerControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton("LookupServices":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraComboEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartCustomizationType_ServiceTypeName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.Repository.Customization.CustomizationTypeBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartCustomizationType":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 5 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("CustomizationTypeGuid":U, "CustomizationTypeGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("CustomizationTypeCode":U, "Customization Type":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("CustomizationTypeDescription":U, "Description":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("ServiceTypeName":U, "Service Type Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("ServiceMethodName":U, "Method Name":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* eSmartCustomizationType_CustomizationTypeCode_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode_Label:Appearance = appearance1.
        resources:ApplyResources(THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode_Label, "eSmartCustomizationType_CustomizationTypeCode_Label":U).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode_Label:Name = "eSmartCustomizationType_CustomizationTypeCode_Label":U.
        /*  */
        /* eSmartCustomizationType_CustomizationTypeCode */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode, "eSmartCustomizationType_CustomizationTypeCode":U).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "CustomizationTypeCode":U, TRUE)).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode:Name = "eSmartCustomizationType_CustomizationTypeCode":U.
        /*  */
        /* eSmartCustomizationType_CustomizationTypeDescription_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription_Label:Appearance = appearance2.
        resources:ApplyResources(THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription_Label, "eSmartCustomizationType_CustomizationTypeDescription_Label":U).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription_Label:Name = "eSmartCustomizationType_CustomizationTypeDescription_Label":U.
        /*  */
        /* eSmartCustomizationType_CustomizationTypeDescription */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription, "eSmartCustomizationType_CustomizationTypeDescription":U).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "CustomizationTypeDescription":U, TRUE)).
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription:Name = "eSmartCustomizationType_CustomizationTypeDescription":U.
        /*  */
        /* eSmartCustomizationType_ServiceTypeName_Label */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartCustomizationType_ServiceTypeName_Label, "eSmartCustomizationType_ServiceTypeName_Label":U).
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName_Label:Appearance = appearance3.
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName_Label:Name = "eSmartCustomizationType_ServiceTypeName_Label":U.
        /*  */
        /* eSmartCustomizationType_ServiceTypeName */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartCustomizationType_ServiceTypeName, "eSmartCustomizationType_ServiceTypeName":U).
        editorButton1:Key = "LookupServices":U.
        resources:ApplyResources(editorButton1, "editorButton1":U).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ServiceTypeName":U, TRUE)).
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName:Name = "eSmartCustomizationType_ServiceTypeName":U.
        THIS-OBJECT:eSmartCustomizationType_ServiceTypeName:EditorButtonClick:Subscribe(THIS-OBJECT:eSmartCustomizationType_ServiceTypeName_EditorButtonClick).
        /*  */
        /* eSmartCustomizationType_ServiceMethodName_Label */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label, "eSmartCustomizationType_ServiceMethodName_Label":U).
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label:Appearance = appearance4.
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label:Name = "eSmartCustomizationType_ServiceMethodName_Label":U.
        /*  */
        /* ultraComboEditor1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraComboEditor1, "ultraComboEditor1":U).
        THIS-OBJECT:ultraComboEditor1:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ServiceMethodName":U, TRUE)).
        THIS-OBJECT:ultraComboEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraComboEditor1:Name = "ultraComboEditor1":U.
        THIS-OBJECT:ultraComboEditor1:BeforeDropDown:Subscribe(THIS-OBJECT:ultraComboEditor1_BeforeDropDown).
        /*  */
        /* SmartCustomizationTypeViewerControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraComboEditor1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartCustomizationType_ServiceTypeName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartCustomizationType_ServiceTypeName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label).
        THIS-OBJECT:Name = "SmartCustomizationTypeViewerControl":U.
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartCustomizationType_CustomizationTypeCode, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartCustomizationType_CustomizationTypeDescription, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartCustomizationType_ServiceTypeName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Instantiates the Service and run the defined method of the currently
                 selected CustomizationType record. The Result gets displayed.
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PerformTestService ():

        DEFINE VARIABLE cClassName  AS CHARACTER            NO-UNDO.
        DEFINE VARIABLE cMethodName AS CHARACTER            NO-UNDO.
        DEFINE VARIABLE cResult     AS CHARACTER            NO-UNDO.
        DEFINE VARIABLE oService    AS Progress.Lang.Object NO-UNDO.
        DEFINE VARIABLE lLocalNew   AS LOGICAL              NO-UNDO.

        ASSIGN
            cClassName  = TRIM (THIS-OBJECT:eSmartCustomizationType_ServiceTypeName:Text)
            cMethodName = TRIM (THIS-OBJECT:ultraComboEditor1:Text)
            .

        IF cMethodName = "":U THEN
            UNDO, THROW NEW AppError ("Please provide a Method Name!"{&TRAN}, 0).

        oService = FrameworkSettings:ServiceContainer:GetService (THIS-OBJECT:GetClassOfType (cClassName)) .

        IF NOT VALID-OBJECT (oService) THEN DO:
            lLocalNew = TRUE.
            oService  = DYNAMIC-NEW (cClassName) () NO-ERROR.
        END.

        IF NOT VALID-OBJECT (oService) THEN
            UNDO, THROW NEW AppError (SUBSTITUTE ("Unable to get or create Service (&1)! &2Please provide a valid Service Type Name!"{&TRAN},
                                                  cClassName, Consultingwerk.Environment:NewLine), 0).

        DO ON ERROR UNDO, THROW:
            cResult = DYNAMIC-INVOKE (oService, cMethodName).

            MessageFormHelper:ShowMessage (SUBSTITUTE ("&1:&2 returned: &3"{&TRAN}, cClassName, cMethodName, cResult),
                                           "Service Method Test Result"{&TRAN}).

            CATCH ple AS Progress.Lang.Error :
                ErrorHelper:ShowErrorMessage (ple).
            END CATCH.
        END.

        FINALLY:
            IF lLocalNew THEN
                GarbageCollectorHelper:DeleteObject (oService).
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populates the MethodCombobox
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PopulateMethodCombobox ():

&IF DEFINED (AblReflection) NE 0 &THEN
        DEFINE VARIABLE oClass       AS Progress.Lang.Class NO-UNDO.
        DEFINE VARIABLE cMethodNames AS CHARACTER           NO-UNDO.

        IF VALID-OBJECT (oMethodValueList) THEN DO:

            THIS-OBJECT:ultraComboEditor1:ValueList = ?.
            GarbageCollectorHelper:DeleteObject (oMethodValueList).

        END.

        oClass = THIS-OBJECT:GetClassOfType (THIS-OBJECT:eSmartCustomizationType_ServiceTypeName:Text).

        IF NOT VALID-OBJECT (oClass) THEN
            UNDO, THROW NEW AppError (SUBSTITUTE ("Unable to get or create Service (&1)! &2Please select a valid Service Type Name before selecting the Method!"{&TRAN},
                                                  THIS-OBJECT:eSmartCustomizationType_ServiceTypeName:Text,
                                                  Consultingwerk.Environment:NewLine), 0).

        cMethodNames = AblReflectionHelper:GetMethodNamesByParameterCount (oClass,
                                                                           0,
                                                                           Progress.Reflect.Flags:Public OR Progress.Reflect.Flags:Instance).

        oMethodValueList = ValueListHelper:FromList(cMethodNames).

        THIS-OBJECT:ultraComboEditor1:ValueList = oMethodValueList.

        THIS-OBJECT:ultraComboEditor1:SelectedIndex = THIS-OBJECT:ultraComboEditor1:FindString
                                (THIS-OBJECT:SmartDataSource:GetFieldValues ("ServiceMethodName":U)).

&ENDIF

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populate Method Combobox before displaying the DropDown list
        Notes:
        @param sender The control invoking the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraComboEditor1_BeforeDropDown (sender AS System.Object,
                                                          e AS System.ComponentModel.CancelEventArgs):

        PopulateMethodCombobox ().

        CATCH ple AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (ple).
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartCustomizationTypeViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartCustomizationTypeViewerControl ():

        IF VALID-OBJECT (components) THEN DO:
            CAST (components, System.IDisposable):Dispose ().
        END.

    END DESTRUCTOR.

END CLASS.
