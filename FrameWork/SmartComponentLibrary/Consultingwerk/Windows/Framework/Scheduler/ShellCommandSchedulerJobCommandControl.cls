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
    File        : ShellCommandSchedulerJobCommandControl
    Purpose     : Specialized Job Command Control to maintain Scheduler Shell Command Jobs
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Thu May 26 12:53:59 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Windows.Framework.Scheduler.* FROM PROPATH .
USING Progress.Lang.*                              FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.*    FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Scheduler.ShellCommandSchedulerJobCommandControl
    INHERITS JobCommandBaseControl:

    DEFINE PRIVATE VARIABLE ultraTextEditor2 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor1 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for ShellCommandSchedulerJobCommandControl
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ShellCommandSchedulerJobCommandControl ():

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
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Scheduler.ShellCommandSchedulerJobCommandControl":U).
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraTextEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraTextEditor2 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTextEditor2, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraLabel1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel1, "ultraLabel1":U).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        /*  */
        /* ultraLabel2 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel2, "ultraLabel2":U).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        /*  */
        /* ultraTextEditor1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTextEditor1, "ultraTextEditor1":U).
        THIS-OBJECT:ultraTextEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraTextEditor1:Name = "ultraTextEditor1":U.
        /*  */
        /* ultraTextEditor2 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraTextEditor2, "ultraTextEditor2":U).
        THIS-OBJECT:ultraTextEditor2:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraTextEditor2:Name = "ultraTextEditor2":U.
        /*  */
        /* ShellCommandSchedulerJobCommandControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTextEditor2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTextEditor1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:Name = "ShellCommandSchedulerJobCommandControl":U.
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTextEditor2, System.ComponentModel.ISupportInitialize):EndInit().
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
            THIS-OBJECT:ultraTextEditor2:Text = "":U
            .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the data shown in the UI
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID UpdateUiData ():

        DEFINE VARIABLE oSchedulerJobCommand AS ShellCommandSchedulerJobCommand NO-UNDO.

        IF NOT TYPE-OF (THIS-OBJECT:CallParameterObject, ShellCommandSchedulerJobCommand) THEN
            RETURN.

        oSchedulerJobCommand = CAST (THIS-OBJECT:CallParameterObject, ShellCommandSchedulerJobCommand).

        ASSIGN
            THIS-OBJECT:ultraTextEditor1:Text = oSchedulerJobCommand:ShellCommand
            THIS-OBJECT:ultraTextEditor2:Text = oSchedulerJobCommand:ShellParameter
            .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the data of the CallParameter object after the user has
                 entered data to the UI
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID WritebackCallParameterToTextProperty ():

        DEFINE VARIABLE oSchedulerJobCommand AS ShellCommandSchedulerJobCommand NO-UNDO.

        IF NOT VALID-OBJECT (THIS-OBJECT:CallParameterObject) OR
           NOT TYPE-OF (THIS-OBJECT:CallParameterObject, ShellCommandSchedulerJobCommand) THEN DO:

            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (THIS-OBJECT:CallParameterObject).

            THIS-OBJECT:CallParameterObject = NEW ShellCommandSchedulerJobCommand ().

        END.

        oSchedulerJobCommand = CAST (THIS-OBJECT:CallParameterObject, ShellCommandSchedulerJobCommand).

        ASSIGN
            oSchedulerJobCommand:ShellCommand   = THIS-OBJECT:ultraTextEditor1:Text
            oSchedulerJobCommand:ShellParameter = THIS-OBJECT:ultraTextEditor2:Text
            .

        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.

    END METHOD .

END CLASS.
