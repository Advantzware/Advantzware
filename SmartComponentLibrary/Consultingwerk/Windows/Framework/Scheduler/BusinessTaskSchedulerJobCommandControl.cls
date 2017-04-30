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
    File        : BusinessTaskSchedulerJobCommandControl
    Purpose     : Embedded component to maintain the Business Task Job Command
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Thu May 26 12:54:18 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.*                   FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.*    FROM PROPATH .
USING Consultingwerk.SmartComponents.Support.*     FROM PROPATH .
USING Consultingwerk.Util.*                        FROM PROPATH .
USING Consultingwerk.Windows.Framework.Scheduler.* FROM PROPATH .
USING Infragistics.Win.*                           FROM ASSEMBLY.
USING Infragistics.Win.Misc.*                      FROM ASSEMBLY.
USING Progress.Lang.*                              FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Scheduler.BusinessTaskSchedulerJobCommandControl
    INHERITS JobCommandBaseControl:

    {Consultingwerk/Util/TempTables/ttClassNames.i}

    DEFINE PRIVATE VARIABLE eSmartCustomizationType_ServiceMethodName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraComboEditor1 AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor1 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    DEFINE VARIABLE oMethodValueList AS ValueList            NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for BusinessTaskSchedulerJobCommandControl
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BusinessTaskSchedulerJobCommandControl ():

        SUPER ().

        InitializeComponent ().

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
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Scheduler.BusinessTaskSchedulerJobCommandControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton("LookupServices":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraComboEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:ultraTextEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraLabel1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel1, "ultraLabel1":U).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        /*  */
        /* ultraComboEditor1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraComboEditor1, "ultraComboEditor1":U).
        THIS-OBJECT:ultraComboEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraComboEditor1:Name = "ultraComboEditor1":U.
        THIS-OBJECT:ultraComboEditor1:BeforeDropDown:Subscribe(THIS-OBJECT:OnBeforeDropDownHandler).
        /*  */
        /* ultraTextEditor1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTextEditor1, "ultraTextEditor1":U).
        editorButton1:Key = "LookupServices":U.
        resources:ApplyResources(editorButton1, "editorButton1":U).
        THIS-OBJECT:ultraTextEditor1:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:ultraTextEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraTextEditor1:Name = "ultraTextEditor1":U.
        THIS-OBJECT:ultraTextEditor1:EditorButtonClick:Subscribe(THIS-OBJECT:OnEditorButtonClickHandler).
        /*  */
        /* eSmartCustomizationType_ServiceMethodName_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label:Appearance = appearance1.
        resources:ApplyResources(THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label, "eSmartCustomizationType_ServiceMethodName_Label":U).
        THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label:Name = "eSmartCustomizationType_ServiceMethodName_Label":U.
        /*  */
        /* BusinessTaskSchedulerJobCommandControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraComboEditor1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTextEditor1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartCustomizationType_ServiceMethodName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:Name = "BusinessTaskSchedulerJobCommandControl":U.
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Clear all editor fields
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID InternalClearDataFields ():

        ASSIGN
            THIS-OBJECT:ultraTextEditor1:Text = "":U
            THIS-OBJECT:ultraComboEditor1:Text = "":U
            .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populate Method Combobox before displaying the DropDown list
        Notes:
        @param sender The control invoking the event
        @param e The CancelEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID OnBeforeDropDownHandler (INPUT sender AS System.Object, INPUT e AS System.ComponentModel.CancelEventArgs):

        PopulateMethodCombobox ().

        CATCH ple AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (ple).
        END CATCH.

    END METHOD .

  /*------------------------------------------------------------------------------
        Purpose: Display Dialog to select a Class or Interface from the project as the
                 ServiceType
        Notes:
        @param sender The control invoking the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID OnEditorButtonClickHandler (sender AS System.Object,
                                                    e      AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        DEFINE VARIABLE oForm         AS BusinessEntityPickerForm          NO-UNDO .
        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult NO-UNDO .
        DEFINE VARIABLE oInfo         AS UltraDesktopAlertWindowInfo       NO-UNDO .
        DEFINE VARIABLE hServer       AS HANDLE                            NO-UNDO .

        oInfo = Consultingwerk.SmartComponents.Support.DesktopAlertHelper:Show ("SmartFramework"{&TRAN},
                                                                                "Scanning Project Folder(s) for Business Tasks."{&TRAN}) .

        ASSIGN hServer = FrameworkSettings:AppServerServiceManager:ConnectService ("":U) .

        RUN Consultingwerk/Util/Support/getclassnamesonserver.p ON hServer
                                                                 ("Consultingwerk.OERA.IBusinessService":U,
                                                                  FALSE,
                                                                  OUTPUT TABLE ttClassNames) .
        oInfo:Close () .

        oForm = NEW BusinessEntityPickerForm () .
        oForm:Text = "Select Business Task"{&TRAN} .

        oForm:ReceiveBusinessEntityNames (TABLE ttClassNames) .

        ASSIGN oDialogResult = Consultingwerk.SmartComponents.FormHelper:ShowDialog(oForm) .

        IF Progress.Util.EnumHelper:AreEqual(oDialogResult,
                                             System.Windows.Forms.DialogResult:Ok) THEN

            ASSIGN THIS-OBJECT:ultraTextEditor1:Text = oForm:GetSelectedBusinessEntityName () .

        CATCH ple AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (ple).
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populates the MethodCombobox
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PopulateMethodCombobox ():

&IF DEFINED (AblReflection) NE 0 &THEN
        DEFINE VARIABLE cClassName   AS CHARACTER                       NO-UNDO .
        DEFINE VARIABLE oParameter   AS GetBusinessTaskMethodsParameter NO-UNDO .

        IF VALID-OBJECT (oMethodValueList) THEN DO:
            THIS-OBJECT:ultraComboEditor1:ValueList = ?.
            GarbageCollectorHelper:DeleteObject (oMethodValueList).
        END.

        cClassName = THIS-OBJECT:ultraTextEditor1:Text.

        oParameter = NEW GetBusinessTaskMethodsParameter (cClassName) .

        FrameworkSettings:ServiceAdapter:InvokeTask("":U,
                                                    "Consultingwerk.SmartFramework.Scheduler.SchedulerMaintenanceBusinessTask":U,
                                                    "GetBusinessTaskMethods":U,
                                                    oParameter) .

        oMethodValueList = ValueListHelper:FromList (oParameter:MethodNames).

        THIS-OBJECT:ultraComboEditor1:ValueList = oMethodValueList.

&ENDIF

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the data shown in the UI
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID UpdateUiData ():

        DEFINE VARIABLE oSchedulerJobCommand AS BusinessTaskSchedulerJobCommand NO-UNDO.

        IF NOT TYPE-OF (THIS-OBJECT:CallParameterObject, BusinessTaskSchedulerJobCommand) THEN
            RETURN.

        oSchedulerJobCommand = CAST (THIS-OBJECT:CallParameterObject, BusinessTaskSchedulerJobCommand).

        ASSIGN
            THIS-OBJECT:ultraTextEditor1:Text = oSchedulerJobCommand:BusinessTaskClassName
            THIS-OBJECT:ultraComboEditor1:Text = oSchedulerJobCommand:BusinessTaskMethodName
            .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the data of the CallParameter object after the user has
                 entered data to the UI
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID WritebackCallParameterToTextProperty ():

        DEFINE VARIABLE oSchedulerJobCommand AS BusinessTaskSchedulerJobCommand NO-UNDO.

        IF NOT VALID-OBJECT (THIS-OBJECT:CallParameterObject) OR
           NOT TYPE-OF (THIS-OBJECT:CallParameterObject, BusinessTaskSchedulerJobCommand) THEN DO:

            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (THIS-OBJECT:CallParameterObject).

            THIS-OBJECT:CallParameterObject = NEW BusinessTaskSchedulerJobCommand ().

        END.

        oSchedulerJobCommand = CAST (THIS-OBJECT:CallParameterObject, BusinessTaskSchedulerJobCommand).

        ASSIGN
            oSchedulerJobCommand:BusinessTaskClassName  = THIS-OBJECT:ultraTextEditor1:Text
            oSchedulerJobCommand:BusinessTaskMethodName = THIS-OBJECT:ultraComboEditor1:Text
            .

        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.

    END METHOD .

END CLASS.
