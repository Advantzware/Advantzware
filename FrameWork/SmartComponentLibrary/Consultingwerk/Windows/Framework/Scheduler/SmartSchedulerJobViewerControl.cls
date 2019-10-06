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
    File        : SmartSchedulerJobViewerControl
    Purpose     : Viewer Control to maintain the Scheduler Job Details
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : 25.05.2016 19:58:48
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Consultingwerk.Windows.Framework.Scheduler.*    FROM PROPATH .
USING Infragistics.Win.UltraWinEditors.*              FROM ASSEMBLY .
USING Progress.Lang.*                                 FROM ASSEMBLY .
USING System.Windows.Forms.*                          FROM ASSEMBLY .

CLASS Consultingwerk.Windows.Framework.Scheduler.SmartSchedulerJobViewerControl
    INHERITS SmartViewerControl:

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJob_SchedulerJobName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJob_SchedulerJobName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJob_Description_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJob_Description AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJob_MaxRuntime_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartSchedulerJob_MaxRuntime AS Infragistics.Win.UltraWinEditors.UltraNumericEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraPanel1 AS Infragistics.Win.Misc.UltraPanel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE openFileDialog1 AS System.Windows.Forms.OpenFileDialog NO-UNDO.

    DEFINE PRIVATE VARIABLE oCurrentJobCommandControl AS JobCommandBaseControl NO-UNDO .
    DEFINE PRIVATE VARIABLE lAddingRecord             AS LOGICAL               NO-UNDO .
    DEFINE PRIVATE VARIABLE lCurrentChangedSubscribed AS LOGICAL               NO-UNDO INIT FALSE .

    /* Marko Rüterbories, Consultingwerk Ltd. 09.12.2013
       This variable is used to store the JobCommandControl during a "copy record" operation */
    DEFINE PRIVATE VARIABLE lcJobCommandCopy AS LONGCHAR NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraSplitter1 AS Infragistics.Win.Misc.UltraSplitter NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraPanel2 AS Infragistics.Win.Misc.UltraPanel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartSchedulerJobViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartSchedulerJobViewerControl ():

        SUPER().

        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        THIS-OBJECT:SetControlEnabled (THIS-OBJECT:ultraSplitter1, Consultingwerk.SmartComponents.Enum.ControlEnabledEnum:Always) .

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
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartSchedulerJob":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Scheduler.SmartSchedulerJobViewerControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:eSmartSchedulerJob_SchedulerJobName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSchedulerJob_SchedulerJobName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartSchedulerJob_Description_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSchedulerJob_Description = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartSchedulerJob_MaxRuntime_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartSchedulerJob_MaxRuntime = NEW Infragistics.Win.UltraWinEditors.UltraNumericEditor().
        THIS-OBJECT:openFileDialog1 = NEW System.Windows.Forms.OpenFileDialog().
        THIS-OBJECT:ultraPanel1 = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraSplitter1 = NEW Infragistics.Win.Misc.UltraSplitter().
        THIS-OBJECT:ultraPanel2 = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJob_SchedulerJobName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJob_Description, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartSchedulerJob_MaxRuntime, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraPanel1:ClientArea:SuspendLayout().
        THIS-OBJECT:ultraPanel1:SuspendLayout().
        THIS-OBJECT:ultraPanel2:ClientArea:SuspendLayout().
        THIS-OBJECT:ultraPanel2:SuspendLayout().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.Scheduler.SchedulerJobBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartSchedulerJob":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 5 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("SchedulerJobGuid":U, "Job Guid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("SchedulerJobName":U, "Job Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("Description":U, "Description":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("MaxRuntime":U, "Max Runtime":U, Progress.Data.DataType:INTEGER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("JobCommand":U, "Job Command":U, Progress.Data.DataType:CLOB).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* eSmartSchedulerJob_SchedulerJobName_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartSchedulerJob_SchedulerJobName_Label:Appearance = appearance1.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJob_SchedulerJobName_Label, "eSmartSchedulerJob_SchedulerJobName_Label":U).
        THIS-OBJECT:eSmartSchedulerJob_SchedulerJobName_Label:Name = "eSmartSchedulerJob_SchedulerJobName_Label":U.
        /*  */
        /* eSmartSchedulerJob_SchedulerJobName */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJob_SchedulerJobName, "eSmartSchedulerJob_SchedulerJobName":U).
        THIS-OBJECT:eSmartSchedulerJob_SchedulerJobName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "SchedulerJobName":U, TRUE)).
        THIS-OBJECT:eSmartSchedulerJob_SchedulerJobName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartSchedulerJob_SchedulerJobName:Name = "eSmartSchedulerJob_SchedulerJobName":U.
        /*  */
        /* eSmartSchedulerJob_Description_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartSchedulerJob_Description_Label:Appearance = appearance2.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJob_Description_Label, "eSmartSchedulerJob_Description_Label":U).
        THIS-OBJECT:eSmartSchedulerJob_Description_Label:Name = "eSmartSchedulerJob_Description_Label":U.
        /*  */
        /* eSmartSchedulerJob_Description */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJob_Description, "eSmartSchedulerJob_Description":U).
        THIS-OBJECT:eSmartSchedulerJob_Description:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "Description":U, TRUE)).
        THIS-OBJECT:eSmartSchedulerJob_Description:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartSchedulerJob_Description:Name = "eSmartSchedulerJob_Description":U.
        /*  */
        /* eSmartSchedulerJob_MaxRuntime_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartSchedulerJob_MaxRuntime_Label:Appearance = appearance3.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJob_MaxRuntime_Label, "eSmartSchedulerJob_MaxRuntime_Label":U).
        THIS-OBJECT:eSmartSchedulerJob_MaxRuntime_Label:Name = "eSmartSchedulerJob_MaxRuntime_Label":U.
        /*  */
        /* eSmartSchedulerJob_MaxRuntime */
        /*  */
        THIS-OBJECT:eSmartSchedulerJob_MaxRuntime:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "MaxRuntime":U, TRUE)).
        THIS-OBJECT:eSmartSchedulerJob_MaxRuntime:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:eSmartSchedulerJob_MaxRuntime, "eSmartSchedulerJob_MaxRuntime":U).
        THIS-OBJECT:eSmartSchedulerJob_MaxRuntime:Name = "eSmartSchedulerJob_MaxRuntime":U.
        /*  */
        /* ultraPanel1 */
        /*  */
        /*  */
        /* ultraPanel1.ClientArea */
        /*  */
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:eSmartSchedulerJob_SchedulerJobName_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:eSmartSchedulerJob_SchedulerJobName).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:eSmartSchedulerJob_MaxRuntime).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:eSmartSchedulerJob_Description_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:eSmartSchedulerJob_MaxRuntime_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:eSmartSchedulerJob_Description).
        resources:ApplyResources(THIS-OBJECT:ultraPanel1, "ultraPanel1":U).
        THIS-OBJECT:ultraPanel1:Name = "ultraPanel1":U.
        /*  */
        /* ultraLabel2 */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel2:Appearance = appearance4.
        resources:ApplyResources(THIS-OBJECT:ultraLabel2, "ultraLabel2":U).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        /*  */
        /* ultraSplitter1 */
        /*  */
        THIS-OBJECT:ultraSplitter1:BackColor = System.Drawing.SystemColors:Control.
        resources:ApplyResources(THIS-OBJECT:ultraSplitter1, "ultraSplitter1":U).
        THIS-OBJECT:ultraSplitter1:Name = "ultraSplitter1":U.
        THIS-OBJECT:ultraSplitter1:RestoreExtent = 125.
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
        /* ultraLabel1 */
        /*  */
        appearance5:BackColor = System.Drawing.SystemColors:Highlight.
        appearance5:ForeColor = System.Drawing.SystemColors:HighlightText.
        resources:ApplyResources(appearance5, "appearance5":U).
        THIS-OBJECT:ultraLabel1:Appearance = appearance5.
        resources:ApplyResources(THIS-OBJECT:ultraLabel1, "ultraLabel1":U).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        /*  */
        /* SmartSchedulerJobViewerControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraPanel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraSplitter1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraPanel2).
        THIS-OBJECT:FocusControlOnAdd = THIS-OBJECT:eSmartSchedulerJob_SchedulerJobName.
        THIS-OBJECT:FocusControlOnUpdate = THIS-OBJECT:eSmartSchedulerJob_SchedulerJobName.
        THIS-OBJECT:Name = "SmartSchedulerJobViewerControl":U.
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJob_SchedulerJobName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJob_Description, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartSchedulerJob_MaxRuntime, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraPanel1:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:ultraPanel1:ClientArea:PerformLayout().
        THIS-OBJECT:ultraPanel1:ResumeLayout(FALSE).
        THIS-OBJECT:ultraPanel2:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:ultraPanel2:ResumeLayout(FALSE).
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initialize a Call Parameter UserControl on the Viewer.
        Notes:
        @param poJobCommandControl The UserControl to arrange on the Viewer
        @param pcLabel The label to display in the header
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID InitializeJobCommandControl (poJobCommandControl AS System.Windows.Forms.UserControl,
                                                     pcLabel AS CHARACTER):

        IF NOT VALID-OBJECT (poJobCommandControl) THEN
            UNDO, THROW NEW AppError ("No valid UserControl provided!":U, 0).

        IF NOT TYPE-OF (poJobCommandControl, JobCommandBaseControl) THEN
            UNDO, THROW NEW AppError ("No valid CallParameterBaseControl given to arrange!":U).

        IF poJobCommandControl = oCurrentJobCommandControl THEN
            RETURN.

        Consultingwerk.Windows.API.Win32:LockWindowUpdate (THIS-OBJECT:ultraPanel2:ClientArea) .

        THIS-OBJECT:ultraLabel1:Text = pcLabel .

        THIS-OBJECT:ultraPanel2:ClientArea:SuspendLayout ().

        IF VALID-OBJECT (oCurrentJobCommandControl) THEN DO:
            THIS-OBJECT:RemoveControlEnabled (oCurrentJobCommandControl) .

            oCurrentJobCommandControl:TextChanged:Unsubscribe (TextChangedEventHandler) .

            oCurrentJobCommandControl:DataBindings:Clear ().

            THIS-OBJECT:ultraPanel2:ClientArea:Controls:Remove (oCurrentJobCommandControl).

            GarbageCollectorHelper:DeleteObject (oCurrentJobCommandControl) .

            oCurrentJobCommandControl = ? .
        END.

        THIS-OBJECT:ultraPanel2:ClientArea:Controls:Add (poJobCommandControl).

        poJobCommandControl:BringToFront () .

        poJobCommandControl:Dock = System.Windows.Forms.DockStyle:Fill.

        oCurrentJobCommandControl = CAST (poJobCommandControl, JobCommandBaseControl).

        /* Mike Fechner, Consultingwerk Ltd. 04.01.2013
           Let the SmartViewerControl class enable/disable the user control */
        THIS-OBJECT:SetControlEnabled (poJobCommandControl,
                                       ControlEnabledEnum:Update) .

        oCurrentJobCommandControl:TextChanged:Subscribe (TextChangedEventHandler) .

        THIS-OBJECT:SetDataBindingForJobCommandControl () .

        /* Mike Fechner, Consultingwerk Ltd. 04.01.2013
           The User Control should not be enabeld, when we are not updating data */
        poJobCommandControl:Enabled = (THIS-OBJECT:SmartTableIOState = TableIOStateEnum:FieldsEnabled
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

        DEFINE VARIABLE oJobCommandControlFactory AS IJobCommandControlFactory NO-UNDO.
        DEFINE VARIABLE lcJobCommand              AS LONGCHAR                     NO-UNDO.
        DEFINE VARIABLE oJobCommand               AS Consultingwerk.ISerializable NO-UNDO.
        DEFINE VARIABLE hQuery                    AS HANDLE                       NO-UNDO.
        DEFINE VARIABLE hBuffer                   AS HANDLE                       NO-UNDO.
        DEFINE VARIABLE hField                    AS HANDLE                       NO-UNDO.

        IF NOT lAddingRecord AND
           VALID-OBJECT (THIS-OBJECT:SmartDataSource) AND
           VALID-OBJECT (THIS-OBJECT:SmartDataSource:BindingSource) AND
           VALID-HANDLE (THIS-OBJECT:SmartDataSource:BindingSource:Handle) THEN DO:

            hQuery  = SmartDataSource:BindingSource:Handle.
            hBuffer = hQuery:GET-BUFFER-HANDLE ("eSmartSchedulerJob":U).
            hField  = hBuffer:BUFFER-FIELD ("JobCommand":U) NO-ERROR.

            IF hBuffer:AVAILABLE AND
               VALID-HANDLE (hField) THEN DO:

                COPY-LOB FROM hField:BUFFER-VALUE ()
                         TO lcJobCommand
                         CONVERT TARGET CODEPAGE "utf-8":U .

                IF LENGTH (lcJobCommand) > 0 THEN DO:
                    oJobCommand = Consultingwerk.Serializable:DeserializeInstance (lcJobCommand).

                    oJobCommandControlFactory = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.Scheduler.IJobCommandControlFactory
                                                                              "NEW JobCommandControlFactory ()"}.

                    THIS-OBJECT:InitializeJobCommandControl (oJobCommandControlFactory:GetJobCommandControl(oJobCommand:GetClass()),
                                                             oJobCommand:GetClass():TypeName).
                END.
                ELSE DO:
                    IF VALID-OBJECT (oCurrentJobCommandControl) THEN DO:
                        oCurrentJobCommandControl:DataBindings:Clear ().

                        THIS-OBJECT:ultraPanel2:ClientArea:Controls:Remove (oCurrentJobCommandControl).

                        GarbageCollectorHelper:DeleteObject (oCurrentJobCommandControl) .
                    END.

                    oCurrentJobCommandControl = ? .
                END.
            END.
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

        FINALLY:
            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (oJobCommand).
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
            oCurrentJobCommandControl:AddingRecord = FALSE.
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

        oCurrentJobCommandControl:Text = lcJobCommandCopy.

        FINALLY:
            lcJobCommandCopy = ?.
        END FINALLY.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the OnBeforeAddRecord event
        Notes:   Displays a dialog to let the user choose a type of CallParameter to
                 add. When the user cancels the Dialog, no Record will be created
        @param e A CancelableEventArgs that contains the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnBeforeAddRecord (e AS System.ComponentModel.CancelEventArgs):

        DEFINE VARIABLE oDialogForm               AS ChooseJobCommandTypeDialog        NO-UNDO.
        DEFINE VARIABLE oDialogResult             AS System.Windows.Forms.DialogResult NO-UNDO.
        DEFINE VARIABLE oJobCommandControlFactory AS IJobCommandControlFactory         NO-UNDO.

        lAddingRecord = TRUE.

        IF VALID-OBJECT (oCurrentJobCommandControl) THEN DO:
            oCurrentJobCommandControl:AddingRecord = TRUE.

            oCurrentJobCommandControl:DataBindings:Clear ().

            oCurrentJobCommandControl:ClearDataFields ().
        END.

        oDialogForm = NEW ChooseJobCommandTypeDialog ().

        WAIT-FOR oDialogForm:ShowDialog () SET oDialogResult.

        IF oDialogResult:Equals (System.Windows.Forms.DialogResult:OK) THEN DO:
            oJobCommandControlFactory = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.Scheduler.IJobCommandControlFactory
                                                                      "NEW JobCommandControlFactory ()"}.

            InitializeJobCommandControl (oJobCommandControlFactory:GetJobCommandControl (Progress.Lang.Class:GetClass (oDialogForm:SelectedJobCommandTypeValue)),
                                         oDialogForm:SelectedJobCommandTypeValue) .
        END.
        ELSE
            e:Cancel = TRUE.

        SUPER:OnBeforeAddRecord (e) .

        FINALLY:
            IF VALID-OBJECT (oDialogForm) THEN
                DELETE OBJECT oDialogForm.
        END FINALLY.

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
            hBuffer = hQuery:GET-BUFFER-HANDLE ("eSmartSchedulerJob":U).
            hField  = hBuffer:BUFFER-FIELD ("JobCommand":U) NO-ERROR.

            IF hBuffer:AVAILABLE AND
               VALID-HANDLE (hField) THEN
                COPY-LOB FROM lcJobCommandCopy
                         TO hField:BUFFER-VALUE ()
                         CONVERT TARGET CODEPAGE "utf-8":U .
        END.

    END METHOD .


    /*------------------------------------------------------------------------------
        Purpose: Sets the Databinding for the CallParameterControl
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SetDataBindingForJobCommandControl ():

        oCurrentJobCommandControl:DataBindings:Add ("Text":U, SmartDataSource:BindingSource, "JobCommand":U) .

        oCurrentJobCommandControl:DataBindings["Text":U]:DataSourceUpdateMode = DataSourceUpdateMode:OnPropertyChanged .

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
            hBuffer = hQuery:GET-BUFFER-HANDLE ("eSmartSchedulerJob":U).
            hField  = hBuffer:BUFFER-FIELD ("JobCommand":U) NO-ERROR.

            IF hBuffer:AVAILABLE AND
               VALID-HANDLE (hField) THEN
                COPY-LOB FROM hField:BUFFER-VALUE ()
                         TO lcJobCommandCopy
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

        IF sender <> oCurrentJobCommandControl THEN
            SUPER:TextChangedEventHandler (sender, e) .
        ELSE DO:
            IF THIS-OBJECT:SmartTableIOState = TableIOStateEnum:FieldsEnabled THEN

                THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartSchedulerJobViewerControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartSchedulerJobViewerControl ( ):

        IF VALID-OBJECT (oCurrentJobCommandControl) THEN DO:
            oCurrentJobCommandControl:DataBindings:Clear ().

            GarbageCollectorHelper:DeleteObject (oCurrentJobCommandControl) .

            oCurrentJobCommandControl = ? .
        END.

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
