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
    File        : SmartFunctionViewer
    Purpose     : Maintenance Viewer for SmartFunction
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Thu Oct 11 18:34:49 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*                  FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*                  FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.*        FROM PROPATH .
USING Consultingwerk.Util.*                                  FROM PROPATH .
USING Consultingwerk.Windows.Framework.Menu.*                FROM PROPATH .
USING Progress.Lang.*                                        FROM PROPATH .
USING Infragistics.Win.UltraWinEditors.*                     FROM ASSEMBLY .
USING System.Windows.Forms.*                                 FROM ASSEMBLY .

CLASS Consultingwerk.Windows.Framework.Menu.SmartFunctionViewer
    INHERITS SmartViewerControl:

    DEFINE PRIVATE VARIABLE components                                                     AS System.ComponentModel.IContainer                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartFunction_ExternalId_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartFunction_ExternalId AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ModuleLookup AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup NO-UNDO.
    DEFINE PRIVATE VARIABLE FunctionBindingSource_eSmartFunction_FunctionModuleGuid_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE openFileDialog1                                                AS System.Windows.Forms.OpenFileDialog                                            NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraPanel1                                                    AS Infragistics.Win.Misc.UltraPanel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE FunctionBindingSource_eSmartFunction_FunctionSmallImage_Label  AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE FunctionBindingSource_eSmartFunction_FunctionSmallImage        AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE FunctionBindingSource_eSmartFunction_FunctionName_Label        AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE FunctionBindingSource_eSmartFunction_FunctionName              AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE FunctionBindingSource_eSmartFunction_FunctionLargeImage_Label  AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE FunctionBindingSource_eSmartFunction_FunctionLargeImage        AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE FunctionBindingSource_eSmartFunction_FunctionDescription_Label AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE FunctionBindingSource_eSmartFunction_FunctionDescription       AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE FunctionBindingSource                                          AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.

    DEFINE PRIVATE VARIABLE oCurrentCallParameterControl AS CallParameterBaseControl NO-UNDO .
    DEFINE PRIVATE VARIABLE lAddingRecord                AS LOGICAL                  NO-UNDO .
    DEFINE PRIVATE VARIABLE lCurrentChangedSubscribed    AS LOGICAL                  NO-UNDO INIT FALSE .

    /* Marko Rüterbories, Consultingwerk Ltd. 09.12.2013
       This variable is used to store the CallParameter during a "copy record" operation */
    DEFINE PRIVATE VARIABLE lcCallParameterCopy AS LONGCHAR NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraSplitter1 AS Infragistics.Win.Misc.UltraSplitter NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraPanel2 AS Infragistics.Win.Misc.UltraPanel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Construtor for the SmartFunctionViewer class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartFunctionViewer ():

        SUPER ().
        InitializeComponent ().

        THIS-OBJECT:SetControlEnabled (THIS-OBJECT:ultraSplitter1, Consultingwerk.SmartComponents.Enum.ControlEnabledEnum:Always) .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Initialize a Call Parameter UserControl on the Viewer.
        Notes:
        @param poCallParameterControl The UserControl to arrange on the Viewer
        @param pcLabel The label to display in the header
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID InitializeCallParameterControl (poCallParameterControl AS System.Windows.Forms.UserControl,
                                                        pcLabel AS CHARACTER):

        IF NOT VALID-OBJECT (poCallParameterControl) THEN
            UNDO, THROW NEW AppError ("No valid UserControl provided!":U, 0).

        IF NOT TYPE-OF (poCallParameterControl, CallParameterBaseControl) THEN
            UNDO, THROW NEW AppError ("No valid CallParameterBaseControl given to arrange!":U).

        IF poCallParameterControl = oCurrentCallParameterControl THEN
            RETURN.

        Consultingwerk.Windows.API.Win32:LockWindowUpdate (THIS-OBJECT:ultraPanel2:ClientArea) .

        THIS-OBJECT:ultraLabel1:Text = pcLabel .

        THIS-OBJECT:ultraPanel2:ClientArea:SuspendLayout ().

        IF VALID-OBJECT (oCurrentCallParameterControl) THEN DO:
            THIS-OBJECT:RemoveControlEnabled (oCurrentCallParameterControl) .

            oCurrentCallParameterControl:TextChanged:Unsubscribe (TextChangedEventHandler) .

            oCurrentCallParameterControl:DataBindings:Clear ().

            THIS-OBJECT:ultraPanel2:ClientArea:Controls:Remove (oCurrentCallParameterControl).

            GarbageCollectorHelper:DeleteObject (oCurrentCallParameterControl) .

            oCurrentCallParameterControl = ? .
        END.

        THIS-OBJECT:ultraPanel2:ClientArea:Controls:Add (poCallParameterControl).

        poCallParameterControl:BringToFront() .

        poCallParameterControl:Dock = System.Windows.Forms.DockStyle:Fill.

        oCurrentCallParameterControl = CAST (poCallParameterControl, CallParameterBaseControl).

        /* Mike Fechner, Consultingwerk Ltd. 04.01.2013
           Let the SmartViewerControl class enable/disable the user control */
        THIS-OBJECT:SetControlEnabled (poCallParameterControl,
                                       ControlEnabledEnum:Update) .

        oCurrentCallParameterControl:TextChanged:Subscribe (TextChangedEventHandler) .

        THIS-OBJECT:SetDataBindingForCallParameterControl () .

        /* Mike Fechner, Consultingwerk Ltd. 04.01.2013
           The User Control should not be enabeld, when we are not updating data */
        poCallParameterControl:Enabled = (THIS-OBJECT:SmartTableIOState = TableIOStateEnum:FieldsEnabled
                                      OR  THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData) .

        FINALLY:
            THIS-OBJECT:ultraPanel2:ClientArea:ResumeLayout ().

            Consultingwerk.Windows.API.Win32:LockWindowUpdate (0) .
        END FINALLY.
    END METHOD .

    /*------------------------------------------------------------------------------
       Purpose: This Method attaches a new BindingSource given as the Parameter
                poBindingSource to the Objects of the current Class. The new
                BindingSource may have been created by a DataAdapter or reused from a
                SmartBrowserControl
       Notes:   Enforced by Interface ISmartDataTarget.
                It is called by the SmartDataAdapter Class when creating a new
                BindingSource or when a new SmartDataSource is registered to the
                Class implementing the Interface ISmartDataTarget.
                This method is also called from the SmartViewerControl destructor (while
                a form is closed) to de-attach from the Binding Source. In this case
                the poBindingSource reference is passed in as the unknown value.
       @param poBindingSource The BindingSource to be attached to the Controls contained in the Viewer
   ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE VOID AttachBindingSource (poBindingSource AS Progress.Data.BindingSource):

        SUPER:AttachBindingSource (poBindingSource) .

        IF VALID-OBJECT (THIS-OBJECT:SmartDataSource) AND NOT lCurrentChangedSubscribed AND TYPE-OF (THIS-OBJECT:SmartDataSource, SmartDataAdapter) THEN DO:
            ASSIGN lCurrentChangedSubscribed = TRUE .
            CAST (THIS-OBJECT:SmartDataSource, SmartDataAdapter):CurrentChanged:Subscribe (CurrentChangedHandler) .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
       Purpose: Event handler for the CurrentChanged event of the SmartDataSource
       Notes:   Routine to build the UI depending on the Data provided in the
                 FunctionCallParameter field from the Database.
       @param sender The reference to the object that raised the event
       @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID CurrentChangedHandler (sender AS System.Object,
                                               e AS System.EventArgs):

        DEFINE VARIABLE oCallParameterControlFactory AS ICallParameterControlFactory NO-UNDO.
        DEFINE VARIABLE lcCallParameter              AS LONGCHAR                     NO-UNDO.
        DEFINE VARIABLE oCallParameter               AS Consultingwerk.ISerializable NO-UNDO.
        DEFINE VARIABLE hQuery                       AS HANDLE                       NO-UNDO.
        DEFINE VARIABLE hBuffer                      AS HANDLE                       NO-UNDO.
        DEFINE VARIABLE hField                       AS HANDLE                       NO-UNDO.

        IF NOT lAddingRecord AND
           VALID-OBJECT (THIS-OBJECT:SmartDataSource) AND
           VALID-OBJECT (THIS-OBJECT:SmartDataSource:BindingSource) AND
           VALID-HANDLE (THIS-OBJECT:SmartDataSource:BindingSource:Handle) THEN DO:

            hQuery  = SmartDataSource:BindingSource:Handle.
            hBuffer = hQuery:GET-BUFFER-HANDLE ("eSmartFunction":U).
            hField  = hBuffer:BUFFER-FIELD ("FunctionCallParameter":U) NO-ERROR.

            IF hBuffer:AVAILABLE AND
               VALID-HANDLE (hField) THEN DO:

                COPY-LOB FROM hField:BUFFER-VALUE ()
                         TO lcCallParameter
                         CONVERT TARGET CODEPAGE "utf-8":U .

                IF LENGTH (lcCallParameter) > 0 THEN DO:
                    oCallParameter = Consultingwerk.Serializable:DeserializeInstance (lcCallParameter).

                    oCallParameterControlFactory = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.Menu.ICallParameterControlFactory
                                                                                 "NEW CallParameterControlFactory ()"}.

                    THIS-OBJECT:InitializeCallParameterControl (oCallParameterControlFactory:GetCallParameterControl (oCallParameter:GetClass()),
                                                                oCallParameter:GetClass():TypeName).
                END.
                ELSE DO:
                    IF VALID-OBJECT (oCurrentCallParameterControl) THEN DO:
                        oCurrentCallParameterControl:DataBindings:Clear ().

                        THIS-OBJECT:ultraPanel2:ClientArea:Controls:Remove (oCurrentCallParameterControl).

                        GarbageCollectorHelper:DeleteObject (oCurrentCallParameterControl) .
                    END.

                    oCurrentCallParameterControl = ? .
                END.
            END.
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

        FINALLY:
            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (oCallParameter).
        END FINALLY.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the AfterAddRecord event
        Notes:   Resets the lAddingRecord property
        @param e An EventArgs that contains no event data
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnAfterAddRecord (e AS System.EventArgs):

        SUPER:OnAfterAddRecord (e).

        FINALLY:
            /* Marko Rüterbories, Consultingwerk Ltd. 18.09.2013
               Set lAddingRecord for local use and AddingRecord in the current CallParameterControl
               to disable not needed data synchronization in the control. (SCL-119)

               Invalid cast from Consultingwerk.Windows.Framework.LaunchFormCallParameter to
               Consultingwerk.SmartFramework.InvokeServiceMethodCallParameter. (12869)*/
            lAddingRecord = FALSE.
            oCurrentCallParameterControl:AddingRecord = FALSE.
        END FINALLY.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: After copying a record the correct CallParameter UserControl has
                 to be shown and the current value is restored by setting the Text
                 property of the CallParameterControl to the stored CallParameter
                 value.
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnAfterCopyRecord (e AS System.EventArgs):

        SUPER:OnAfterCopyRecord (e).

        THIS-OBJECT:CurrentChangedHandler (THIS-OBJECT, e).

        oCurrentCallParameterControl:Text = lcCallParameterCopy.

        FINALLY:
            lcCallParameterCopy = ?.
        END FINALLY.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the OnBeforeAddRecord event
        Notes:   Displays a dialog to let the user choose a type of CallParameter to
                 add. When the user cancels the Dialog, no Record will be created
        @param e A CancelableEventArgs that contains the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnBeforeAddRecord (e AS System.ComponentModel.CancelEventArgs):

        DEFINE VARIABLE oDialogForm                  AS ChooseFunctionCallTypeDialog      NO-UNDO.
        DEFINE VARIABLE oDialogResult                AS System.Windows.Forms.DialogResult NO-UNDO.
        DEFINE VARIABLE oCallParameterControlFactory AS ICallParameterControlFactory      NO-UNDO.

        /* Marko Rüterbories, Consultingwerk Ltd. 18.09.2013
           Set lAddingRecord for local use and AddingRecord in the current CallParameterControl
           to disable not needed data synchronization in the control. (SCL-119)

           Invalid cast from Consultingwerk.Windows.Framework.LaunchFormCallParameter to
           Consultingwerk.SmartFramework.InvokeServiceMethodCallParameter. (12869)*/
        lAddingRecord = TRUE.

        IF VALID-OBJECT (oCurrentCallParameterControl) THEN DO:
            oCurrentCallParameterControl:AddingRecord = TRUE.

            oCurrentCallParameterControl:DataBindings:Clear ().

            oCurrentCallParameterControl:ClearDataFields ().
        END.

        oDialogForm = NEW ChooseFunctionCallTypeDialog ().

        WAIT-FOR oDialogForm:ShowDialog () SET oDialogResult.

        IF oDialogResult:Equals (System.Windows.Forms.DialogResult:OK) THEN DO:
            oCallParameterControlFactory = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.Menu.ICallParameterControlFactory
                                                                             "NEW CallParameterControlFactory ()"}.

            InitializeCallParameterControl (oCallParameterControlFactory:GetCallParameterControl (Progress.Lang.Class:GetClass (oDialogForm:SelectedCallTypeValue)),
                                            oDialogForm:SelectedCallTypeValue) .
        END.
        ELSE
            e:Cancel = TRUE.

        SUPER:OnBeforeAddRecord (e) .

        FINALLY:
            IF VALID-OBJECT (oDialogForm) THEN
                DELETE OBJECT oDialogForm.
        END FINALLY.
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
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartFunction":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Menu.SmartFunctionViewer":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton("SelectSmallImage":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton2 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton2 = NEW Infragistics.Win.UltraWinEditors.EditorButton("SelectLargeImage":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton3 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton3 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:FunctionBindingSource = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionDescription_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionSmallImage_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionSmallImage = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionLargeImage_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionLargeImage = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraPanel1 = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:ModuleLookup = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionModuleGuid_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartFunction_ExternalId_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartFunction_ExternalId = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:openFileDialog1 = NEW System.Windows.Forms.OpenFileDialog().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraPanel2 = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:ultraSplitter1 = NEW Infragistics.Win.Misc.UltraSplitter().
        CAST(THIS-OBJECT:FunctionBindingSource, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionDescription, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionSmallImage, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionLargeImage, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraPanel1:ClientArea:SuspendLayout().
        THIS-OBJECT:ultraPanel1:SuspendLayout().
        CAST(THIS-OBJECT:ModuleLookup, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartFunction_ExternalId, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraPanel2:ClientArea:SuspendLayout().
        THIS-OBJECT:ultraPanel2:SuspendLayout().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* FunctionBindingSource */
        /*  */
        THIS-OBJECT:FunctionBindingSource:BindTo = "QUERY":U.
        THIS-OBJECT:FunctionBindingSource:EntityName = "Consultingwerk.SmartFramework.Menu.MenuBusinessEntity":U.
        THIS-OBJECT:FunctionBindingSource:EntityTable = "eSmartFunction":U.
        THIS-OBJECT:FunctionBindingSource:MaxDataGuess = 0.
        THIS-OBJECT:FunctionBindingSource:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 9 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("FunctionGuid":U, "FunctionGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("FunctionName":U, "FunctionName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("FunctionDescription":U, "FunctionDescription":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("FunctionSmallImage":U, "FunctionSmallImage":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("FunctionLargeImage":U, "FunctionLargeImage":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("FunctionCallParameter":U, "FunctionCallParameter":U, Progress.Data.DataType:CLOB).
        arrayvar0[7] = NEW Progress.Data.ColumnPropDesc("FunctionModuleGuid":U, "FunctionModuleGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[8] = NEW Progress.Data.ColumnPropDesc("ModuleName":U, "ModuleName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[9] = NEW Progress.Data.ColumnPropDesc("ExternalId":U, "External ID":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:FunctionBindingSource:TableSchema = tableDesc1.
        /*  */
        /* FunctionBindingSource_eSmartFunction_FunctionName_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionName_Label:Appearance = appearance1.
        resources:ApplyResources(THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionName_Label, "FunctionBindingSource_eSmartFunction_FunctionName_Label":U).
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionName_Label:Name = "FunctionBindingSource_eSmartFunction_FunctionName_Label":U.
        /*  */
        /* FunctionBindingSource_eSmartFunction_FunctionName */
        /*  */
        resources:ApplyResources(THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionName, "FunctionBindingSource_eSmartFunction_FunctionName":U).
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionName:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:FunctionBindingSource, "FunctionName":U, TRUE)).
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionName:Name = "FunctionBindingSource_eSmartFunction_FunctionName":U.
        /*  */
        /* FunctionBindingSource_eSmartFunction_FunctionDescription_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionDescription_Label:Appearance = appearance2.
        resources:ApplyResources(THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionDescription_Label, "FunctionBindingSource_eSmartFunction_FunctionDescription_Label":U).
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionDescription_Label:Name = "FunctionBindingSource_eSmartFunction_FunctionDescription_Label":U.
        /*  */
        /* FunctionBindingSource_eSmartFunction_FunctionDescription */
        /*  */
        resources:ApplyResources(THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionDescription, "FunctionBindingSource_eSmartFunction_FunctionDescription":U).
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:FunctionBindingSource, "FunctionDescription":U, TRUE)).
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionDescription:Name = "FunctionBindingSource_eSmartFunction_FunctionDescription":U.
        /*  */
        /* FunctionBindingSource_eSmartFunction_FunctionSmallImage_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionSmallImage_Label:Appearance = appearance3.
        resources:ApplyResources(THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionSmallImage_Label, "FunctionBindingSource_eSmartFunction_FunctionSmallImage_Label":U).
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionSmallImage_Label:Name = "FunctionBindingSource_eSmartFunction_FunctionSmallImage_Label":U.
        /*  */
        /* FunctionBindingSource_eSmartFunction_FunctionSmallImage */
        /*  */
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionSmallImage:AllowDrop = TRUE.
        resources:ApplyResources(THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionSmallImage, "FunctionBindingSource_eSmartFunction_FunctionSmallImage":U).
        editorButton1:Key = "SelectSmallImage":U.
        resources:ApplyResources(editorButton1, "editorButton1":U).
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionSmallImage:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionSmallImage:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:FunctionBindingSource, "FunctionSmallImage":U, TRUE)).
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionSmallImage:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionSmallImage:Name = "FunctionBindingSource_eSmartFunction_FunctionSmallImage":U.
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionSmallImage:EditorButtonClick:Subscribe(THIS-OBJECT:ImageSelection_EditorButtonClick).
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionSmallImage:DragDrop:Subscribe(THIS-OBJECT:ImageSelection_DragDrop).
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionSmallImage:DragEnter:Subscribe(THIS-OBJECT:ImageSelection_DragEnter).
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionSmallImage:KeyDown:Subscribe(THIS-OBJECT:ImageSelection_KeyDownHandler).
        /*  */
        /* FunctionBindingSource_eSmartFunction_FunctionLargeImage_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionLargeImage_Label:Appearance = appearance4.
        resources:ApplyResources(THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionLargeImage_Label, "FunctionBindingSource_eSmartFunction_FunctionLargeImage_Label":U).
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionLargeImage_Label:Name = "FunctionBindingSource_eSmartFunction_FunctionLargeImage_Label":U.
        /*  */
        /* FunctionBindingSource_eSmartFunction_FunctionLargeImage */
        /*  */
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionLargeImage:AllowDrop = TRUE.
        resources:ApplyResources(THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionLargeImage, "FunctionBindingSource_eSmartFunction_FunctionLargeImage":U).
        editorButton2:Key = "SelectLargeImage":U.
        resources:ApplyResources(editorButton2, "editorButton2":U).
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionLargeImage:ButtonsRight:Add(editorButton2).
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionLargeImage:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:FunctionBindingSource, "FunctionLargeImage":U, TRUE)).
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionLargeImage:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionLargeImage:Name = "FunctionBindingSource_eSmartFunction_FunctionLargeImage":U.
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionLargeImage:EditorButtonClick:Subscribe(THIS-OBJECT:ImageSelection_EditorButtonClick).
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionLargeImage:DragDrop:Subscribe(THIS-OBJECT:ImageSelection_DragDrop).
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionLargeImage:DragEnter:Subscribe(THIS-OBJECT:ImageSelection_DragEnter).
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionLargeImage:KeyDown:Subscribe(THIS-OBJECT:ImageSelection_KeyDownHandler).
        /*  */
        /* ultraPanel1 */
        /*  */
        /*  */
        /* ultraPanel1.ClientArea */
        /*  */
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ModuleLookup).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionName).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionModuleGuid_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionName_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionLargeImage).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionLargeImage_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionDescription_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionSmallImage).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionDescription).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionSmallImage_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:eSmartFunction_ExternalId_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:eSmartFunction_ExternalId).
        resources:ApplyResources(THIS-OBJECT:ultraPanel1, "ultraPanel1":U).
        THIS-OBJECT:ultraPanel1:Name = "ultraPanel1":U.
        /*  */
        /* ModuleLookup */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ModuleLookup, "ModuleLookup":U).
        editorButton3:Key = "ShowLookupDialog":U.
        resources:ApplyResources(editorButton3, "editorButton3":U).
        THIS-OBJECT:ModuleLookup:ButtonsRight:Add(editorButton3).
        THIS-OBJECT:ModuleLookup:DataBindings:Add(NEW System.Windows.Forms.Binding("LookupKeyValue":U, THIS-OBJECT:FunctionBindingSource, "FunctionModuleGuid":U, TRUE)).
        THIS-OBJECT:ModuleLookup:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:FunctionBindingSource, "ModuleName":U, TRUE)).
        THIS-OBJECT:ModuleLookup:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ModuleLookup:LookupAdditionalFields = "":U.
        THIS-OBJECT:ModuleLookup:LookupBrowserExcludeFields = "":U.
        THIS-OBJECT:ModuleLookup:LookupBrowserFields = "ModuleName":U.
        THIS-OBJECT:ModuleLookup:LookupCascade = FALSE.
        THIS-OBJECT:ModuleLookup:LookupControls = "":U.
        THIS-OBJECT:ModuleLookup:LookupDialogClassName = "":U.
        THIS-OBJECT:ModuleLookup:LookupDialogFilterFields = "ModuleName":U.
        THIS-OBJECT:ModuleLookup:LookupDialogFilterOperators = "BEGINS":U.
        THIS-OBJECT:ModuleLookup:LookupDialogOpenQuery = TRUE.
        THIS-OBJECT:ModuleLookup:LookupDialogQuerySort = "":U.
        THIS-OBJECT:ModuleLookup:LookupDialogQueryString = "FOR EACH eSmartModule":U.
        THIS-OBJECT:ModuleLookup:LookupEntityName = "Consultingwerk.SmartFramework.Menu.ModuleBusinessEntity":U.
        THIS-OBJECT:ModuleLookup:LookupEntityTable = "eSmartModule":U.
        THIS-OBJECT:ModuleLookup:LookupEntityView = "":U.
        THIS-OBJECT:ModuleLookup:LookupFields = "":U.
        THIS-OBJECT:ModuleLookup:LookupKeyField = "ModuleName":U.
        THIS-OBJECT:ModuleLookup:LookupKeyValue = "":U.
        THIS-OBJECT:ModuleLookup:LookupKeyValueColumn = "ModuleGuid":U.
        THIS-OBJECT:ModuleLookup:LookupQuerySort = "":U.
        THIS-OBJECT:ModuleLookup:LookupQueryString = "FOR EACH eSmartModule WHERE eSmartModule.ModuleName BEGINS '&1' BY eSmartModule.ModuleName":U.
        THIS-OBJECT:ModuleLookup:LookupTimerDelay = 750.
        THIS-OBJECT:ModuleLookup:LookupTimerDisabled = FALSE.
        THIS-OBJECT:ModuleLookup:Name = "ModuleLookup":U.
        THIS-OBJECT:ModuleLookup:OwningGrid = ?.
        /*  */
        /* FunctionBindingSource_eSmartFunction_FunctionModuleGuid_Label */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionModuleGuid_Label:Appearance = appearance5.
        resources:ApplyResources(THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionModuleGuid_Label, "FunctionBindingSource_eSmartFunction_FunctionModuleGuid_Label":U).
        THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionModuleGuid_Label:Name = "FunctionBindingSource_eSmartFunction_FunctionModuleGuid_Label":U.
        /*  */
        /* eSmartFunction_ExternalId_Label */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartFunction_ExternalId_Label, "eSmartFunction_ExternalId_Label":U).
        appearance6:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartFunction_ExternalId_Label:Appearance = appearance6.
        THIS-OBJECT:eSmartFunction_ExternalId_Label:Name = "eSmartFunction_ExternalId_Label":U.
        /*  */
        /* eSmartFunction_ExternalId */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartFunction_ExternalId, "eSmartFunction_ExternalId":U).
        THIS-OBJECT:eSmartFunction_ExternalId:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:FunctionBindingSource, "ExternalId":U, TRUE)).
        THIS-OBJECT:eSmartFunction_ExternalId:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartFunction_ExternalId:Name = "eSmartFunction_ExternalId":U.
        /*  */
        /* ultraLabel1 */
        /*  */
        appearance7:BackColor = System.Drawing.SystemColors:Highlight.
        appearance7:ForeColor = System.Drawing.SystemColors:HighlightText.
        resources:ApplyResources(appearance7, "appearance7":U).
        THIS-OBJECT:ultraLabel1:Appearance = appearance7.
        resources:ApplyResources(THIS-OBJECT:ultraLabel1, "ultraLabel1":U).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Padding = NEW System.Drawing.Size(10, 0).
        /*  */
        /* ultraPanel2 */
        /*  */
        /*  */
        /* ultraPanel2.ClientArea */
        /*  */
        THIS-OBJECT:ultraPanel2:ClientArea:Controls:Add(THIS-OBJECT:ultraLabel1).
        resources:ApplyResources(THIS-OBJECT:ultraPanel2, "ultraPanel2":U).
        THIS-OBJECT:ultraPanel2:Name = "ultraPanel2":U.
        /*  */
        /* ultraSplitter1 */
        /*  */
        THIS-OBJECT:ultraSplitter1:BackColor = System.Drawing.SystemColors:Control.
        resources:ApplyResources(THIS-OBJECT:ultraSplitter1, "ultraSplitter1":U).
        THIS-OBJECT:ultraSplitter1:Name = "ultraSplitter1":U.
        THIS-OBJECT:ultraSplitter1:RestoreExtent = 178.
        /*  */
        /* SmartFunctionViewer */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraSplitter1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraPanel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraPanel2).
        THIS-OBJECT:Name = "SmartFunctionViewer":U.
        CAST(THIS-OBJECT:FunctionBindingSource, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionDescription, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionSmallImage, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:FunctionBindingSource_eSmartFunction_FunctionLargeImage, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraPanel1:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:ultraPanel1:ClientArea:PerformLayout().
        THIS-OBJECT:ultraPanel1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ModuleLookup, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartFunction_ExternalId, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraPanel2:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:ultraPanel2:ResumeLayout(FALSE).
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Displays the OpenFileDialog for the ImageEditors to select a path
        Notes:
        @param sender The source of the event
        @param e An EventArgs that contains the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ImageSelection_KeyDownHandler (sender AS System.Object,
                                                       e AS KeyEventArgs):

        IF e:Handled = FALSE AND Progress.Util.EnumHelper:AreEqual (e:KeyCode, SmartLookupSettings:ShortcutKey) THEN

            THIS-OBJECT:ImageSelection_EditorButtonClick (sender, NEW EditorButtonEventArgs (?, ?)) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Displays the OpenFileDialog for the ImageEditors to select a path
        Notes:
        @param sender The source of the event
        @param e An EventArgs that contains the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ImageSelection_EditorButtonClick (sender AS System.Object,
                                                          e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        DEFINE VARIABLE oDialogResult      AS System.Windows.Forms.DialogResult NO-UNDO.
        DEFINE VARIABLE cShortenedFileName AS CHARACTER                         NO-UNDO.

        WAIT-FOR openFileDialog1:ShowDialog () SET oDialogResult.

        IF NOT oDialogResult:Equals (System.Windows.Forms.DialogResult:OK) THEN
            RETURN.

        ASSIGN cShortenedFileName = Consultingwerk.Util.FileHelper:RelativeFileName (openFileDialog1:FileName).

        CAST (sender, System.Windows.Forms.Control):Text = cShortenedFileName.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Assignes the filename of the droped file to the ImageEditors
        Notes:
        @param sender The source of the event
        @param e An EventArgs that contains the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ImageSelection_DragDrop (sender AS System.Object,
                                                 e      AS System.Windows.Forms.DragEventArgs):
        DEFINE VARIABLE cFileNames AS CHARACTER NO-UNDO EXTENT.

        IF NOT Consultingwerk.Util.DragAndDropHelper:IsFileDrop (e:Data) THEN
            RETURN.

        cFileNames = Consultingwerk.Util.DragAndDropHelper:GetDropFileNames (e:Data).

        IF EXTENT (cFileNames) > 0 THEN
            CAST (sender, System.Windows.Forms.Control):Text = Consultingwerk.Util.FileHelper:RelativeFileName (cFileNames[1]).

        THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .

        /* Tell the SmartDataSource that the user has started to update data. */
        IF VALID-OBJECT (THIS-OBJECT:SmartDataSource) THEN
            THIS-OBJECT:SmartDataSource:BeginUpdateState (THIS-OBJECT).

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Allowes the FileDrop to the editor control if the events data
                 contains exact one file.
        Notes:
        @param sender The source of the event
        @param e An EventArgs that contains the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ImageSelection_DragEnter (sender AS System.Object,
                                                  e      AS System.Windows.Forms.DragEventArgs):
        DEFINE VARIABLE cFileNames AS CHARACTER NO-UNDO EXTENT.

        IF Consultingwerk.Util.DragAndDropHelper:IsFileDrop (e:Data) THEN DO:
            cFileNames = Consultingwerk.Util.DragAndDropHelper:GetDropFileNames (e:Data).

            IF EXTENT (cFileNames) = 1 THEN
                e:Effect = System.Windows.Forms.DragDropEffects:Copy.
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Override of the RestoreValuesForCopy to additionaly restore the
                 FunctionCallParameter value.
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID RestoreValuesForCopy ():

        DEFINE VARIABLE hQuery                       AS HANDLE                       NO-UNDO.
        DEFINE VARIABLE hBuffer                      AS HANDLE                       NO-UNDO.
        DEFINE VARIABLE hField                       AS HANDLE                       NO-UNDO.

        SUPER:RestoreValuesForCopy().

        IF NOT lAddingRecord AND
           VALID-OBJECT (THIS-OBJECT:SmartDataSource) AND
           VALID-OBJECT (THIS-OBJECT:SmartDataSource:BindingSource) AND
           VALID-HANDLE (THIS-OBJECT:SmartDataSource:BindingSource:Handle) THEN DO:

            hQuery  = SmartDataSource:BindingSource:Handle.
            hBuffer = hQuery:GET-BUFFER-HANDLE ("eSmartFunction":U).
            hField  = hBuffer:BUFFER-FIELD ("FunctionCallParameter":U) NO-ERROR.

            IF hBuffer:AVAILABLE AND
               VALID-HANDLE (hField) THEN
                COPY-LOB FROM lcCallParameterCopy
                         TO hField:BUFFER-VALUE ()
                         CONVERT TARGET CODEPAGE "utf-8":U .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Sets the Databinding for the CallParameterControl
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SetDataBindingForCallParameterControl ():

        oCurrentCallParameterControl:DataBindings:Add ("Text":U, SmartDataSource:BindingSource, "FunctionCallParameter":U) .

        oCurrentCallParameterControl:DataBindings["Text":U]:DataSourceUpdateMode = DataSourceUpdateMode:OnPropertyChanged .

    END METHOD .

    /*------------------------------------------------------------------------------
       Purpose: Override of the StoreValuesForCopy method to additionaly store the
                value of the FunctionCallParameter.
       Notes:
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID StoreValuesForCopy ():

        DEFINE VARIABLE hQuery                       AS HANDLE                       NO-UNDO.
        DEFINE VARIABLE hBuffer                      AS HANDLE                       NO-UNDO.
        DEFINE VARIABLE hField                       AS HANDLE                       NO-UNDO.

        SUPER:StoreValuesForCopy().

        IF NOT lAddingRecord AND
           VALID-OBJECT (THIS-OBJECT:SmartDataSource) AND
           VALID-OBJECT (THIS-OBJECT:SmartDataSource:BindingSource) AND
           VALID-HANDLE (THIS-OBJECT:SmartDataSource:BindingSource:Handle) THEN DO:

            hQuery  = SmartDataSource:BindingSource:Handle.
            hBuffer = hQuery:GET-BUFFER-HANDLE ("eSmartFunction":U).
            hField  = hBuffer:BUFFER-FIELD ("FunctionCallParameter":U) NO-ERROR.

            IF hBuffer:AVAILABLE AND
               VALID-HANDLE (hField) THEN
                COPY-LOB FROM hField:BUFFER-VALUE ()
                         TO lcCallParameterCopy
                         CONVERT TARGET CODEPAGE "utf-8":U .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event Handler for the TextChanged event of the contained Controls
        Notes:   Overrides the TextChanged handler in the SmartViewerControl
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE VOID TextChangedEventHandler (sender AS System.Object,
                                                         e AS System.EventArgs):

        IF sender <> oCurrentcallParameterControl THEN
            SUPER:TextChangedEventHandler (sender, e) .
        ELSE DO:
            IF THIS-OBJECT:SmartTableIOState = TableIOStateEnum:FieldsEnabled THEN

                THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartFunctionViewer class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartFunctionViewer ():

        IF VALID-OBJECT (oCurrentCallParameterControl) THEN DO:
            oCurrentCallParameterControl:DataBindings:Clear ().

            GarbageCollectorHelper:DeleteObject (oCurrentCallParameterControl) .

            oCurrentCallParameterControl = ? .
        END.

    END DESTRUCTOR .

END CLASS.
