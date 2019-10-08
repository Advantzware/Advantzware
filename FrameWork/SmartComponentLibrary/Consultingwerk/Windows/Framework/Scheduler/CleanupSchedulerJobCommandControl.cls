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
    File        : CleanupSchedulerJobCommandControl
    Purpose     : Embedded component to maintain the Cleanup Job Command
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

CLASS Consultingwerk.Windows.Framework.Scheduler.CleanupSchedulerJobCommandControl
    INHERITS JobCommandBaseControl:

    {Consultingwerk/Util/TempTables/ttClassNames.i}

    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraGroupBox1 AS Infragistics.Win.Misc.UltraGroupBox NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel9 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel8 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel7 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel6 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel5 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE uneRunningDays AS Infragistics.Win.UltraWinEditors.UltraNumericEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE uneFinishedDays AS Infragistics.Win.UltraWinEditors.UltraNumericEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE uneFailedDays AS Infragistics.Win.UltraWinEditors.UltraNumericEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE uneCancelledDays AS Infragistics.Win.UltraWinEditors.UltraNumericEditor NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for CleanupSchedulerJobCommandControl
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CleanupSchedulerJobCommandControl ():

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
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Scheduler.CleanupSchedulerJobCommandControl":U).
        THIS-OBJECT:ultraGroupBox1 = NEW Infragistics.Win.Misc.UltraGroupBox().
        THIS-OBJECT:uneRunningDays = NEW Infragistics.Win.UltraWinEditors.UltraNumericEditor().
        THIS-OBJECT:ultraLabel8 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel9 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:uneCancelledDays = NEW Infragistics.Win.UltraWinEditors.UltraNumericEditor().
        THIS-OBJECT:ultraLabel6 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel7 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:uneFinishedDays = NEW Infragistics.Win.UltraWinEditors.UltraNumericEditor().
        THIS-OBJECT:ultraLabel3 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel5 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:uneFailedDays = NEW Infragistics.Win.UltraWinEditors.UltraNumericEditor().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        CAST(THIS-OBJECT:ultraGroupBox1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraGroupBox1:SuspendLayout().
        CAST(THIS-OBJECT:uneRunningDays, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:uneCancelledDays, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:uneFinishedDays, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:uneFailedDays, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraGroupBox1 */
        /*  */
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:uneRunningDays).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:ultraLabel8).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:ultraLabel9).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:uneCancelledDays).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:ultraLabel6).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:ultraLabel7).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:uneFinishedDays).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:ultraLabel3).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:ultraLabel5).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:uneFailedDays).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:ultraLabel1).
        resources:ApplyResources(THIS-OBJECT:ultraGroupBox1, "ultraGroupBox1":U).
        THIS-OBJECT:ultraGroupBox1:Name = "ultraGroupBox1":U.
        /*  */
        /* uneRunningDays */
        /*  */
        resources:ApplyResources(THIS-OBJECT:uneRunningDays, "uneRunningDays":U).
        THIS-OBJECT:uneRunningDays:Name = "uneRunningDays":U.
        THIS-OBJECT:uneRunningDays:PromptChar = ' ':U.
        /*  */
        /* ultraLabel8 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel8, "ultraLabel8":U).
        THIS-OBJECT:ultraLabel8:Name = "ultraLabel8":U.
        /*  */
        /* ultraLabel9 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel9, "ultraLabel9":U).
        THIS-OBJECT:ultraLabel9:Name = "ultraLabel9":U.
        /*  */
        /* uneCancelledDays */
        /*  */
        resources:ApplyResources(THIS-OBJECT:uneCancelledDays, "uneCancelledDays":U).
        THIS-OBJECT:uneCancelledDays:Name = "uneCancelledDays":U.
        THIS-OBJECT:uneCancelledDays:PromptChar = ' ':U.
        /*  */
        /* ultraLabel6 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel6, "ultraLabel6":U).
        THIS-OBJECT:ultraLabel6:Name = "ultraLabel6":U.
        /*  */
        /* ultraLabel7 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel7, "ultraLabel7":U).
        THIS-OBJECT:ultraLabel7:Name = "ultraLabel7":U.
        /*  */
        /* uneFinishedDays */
        /*  */
        resources:ApplyResources(THIS-OBJECT:uneFinishedDays, "uneFinishedDays":U).
        THIS-OBJECT:uneFinishedDays:Name = "uneFinishedDays":U.
        THIS-OBJECT:uneFinishedDays:PromptChar = ' ':U.
        /*  */
        /* ultraLabel3 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel3, "ultraLabel3":U).
        THIS-OBJECT:ultraLabel3:Name = "ultraLabel3":U.
        /*  */
        /* ultraLabel5 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel5, "ultraLabel5":U).
        THIS-OBJECT:ultraLabel5:Name = "ultraLabel5":U.
        /*  */
        /* uneFailedDays */
        /*  */
        resources:ApplyResources(THIS-OBJECT:uneFailedDays, "uneFailedDays":U).
        THIS-OBJECT:uneFailedDays:Name = "uneFailedDays":U.
        THIS-OBJECT:uneFailedDays:PromptChar = ' ':U.
        /*  */
        /* ultraLabel2 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel2, "ultraLabel2":U).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        /*  */
        /* ultraLabel1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel1, "ultraLabel1":U).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        /*  */
        /* CleanupSchedulerJobCommandControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraGroupBox1).
        THIS-OBJECT:Name = "CleanupSchedulerJobCommandControl":U.
        CAST(THIS-OBJECT:ultraGroupBox1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraGroupBox1:ResumeLayout(FALSE).
        THIS-OBJECT:ultraGroupBox1:PerformLayout().
        CAST(THIS-OBJECT:uneRunningDays, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:uneCancelledDays, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:uneFinishedDays, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:uneFailedDays, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
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
            THIS-OBJECT:uneCancelledDays:Text = "0":U
            THIS-OBJECT:uneFailedDays:Text    = "0":U
            THIS-OBJECT:uneFinishedDays:Text  = "0":U
            THIS-OBJECT:uneRunningDays:Text   = "0":U
            .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the data shown in the UI
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID UpdateUiData ():

        DEFINE VARIABLE oCleanupJobCommand AS CleanupSchedulerJobCommand NO-UNDO.

        IF NOT TYPE-OF (THIS-OBJECT:CallParameterObject, CleanupSchedulerJobCommand) THEN
            RETURN.

        oCleanupJobCommand = CAST (THIS-OBJECT:CallParameterObject, CleanupSchedulerJobCommand).

        ASSIGN
            THIS-OBJECT:uneCancelledDays:Text = STRING (oCleanupJobCommand:DeleteCancelledFrom)
            THIS-OBJECT:uneFailedDays:Text    = STRING (oCleanupJobCommand:DeleteFailedFrom)
            THIS-OBJECT:uneFinishedDays:Text  = STRING (oCleanupJobCommand:DeleteFinishedFrom)
            THIS-OBJECT:uneRunningDays:Text   = STRING (oCleanupJobCommand:DeleteRunningFrom)
            .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the data of the CallParameter object after the user has
                 entered data to the UI
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID WritebackCallParameterToTextProperty ():

        DEFINE VARIABLE oCleanupJobCommand AS CleanupSchedulerJobCommand NO-UNDO.

        IF NOT VALID-OBJECT (THIS-OBJECT:CallParameterObject) OR
           NOT TYPE-OF (THIS-OBJECT:CallParameterObject, CleanupSchedulerJobCommand) THEN DO:

            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (THIS-OBJECT:CallParameterObject).

            THIS-OBJECT:CallParameterObject = NEW CleanupSchedulerJobCommand ().

        END.

        oCleanupJobCommand = CAST (THIS-OBJECT:CallParameterObject, CleanupSchedulerJobCommand).

        ASSIGN
            oCleanupJobCommand:DeleteCancelledFrom = INTEGER (THIS-OBJECT:uneCancelledDays:Text)
            oCleanupJobCommand:DeleteFailedFrom    = INTEGER (THIS-OBJECT:uneFailedDays:Text)
            oCleanupJobCommand:DeleteFinishedFrom  = INTEGER (THIS-OBJECT:uneFinishedDays:Text)
            oCleanupJobCommand:DeleteRunningFrom   = INTEGER (THIS-OBJECT:uneRunningDays:Text)
            .

        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.

    END METHOD .

END CLASS.
