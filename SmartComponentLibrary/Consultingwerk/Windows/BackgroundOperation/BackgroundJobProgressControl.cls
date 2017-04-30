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
    File        : BackgroundJobProgressControl
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Nov 04 01:16:00 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Windows.UserControl.

USING Consultingwerk.Windows.BackgroundOperation.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.Windows.BackgroundOperation.BackgroundJobProgressControl INHERITS UserControl:

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton1 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE lbJobName AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Gets the reference to the Background Job visualized by this Control
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BackgroundJob AS BackgroundJob NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BackgroundJobProgressControl class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BackgroundJobProgressControl ():


        SUPER().
        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
        &ENDIF

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
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.BackgroundOperation.BackgroundJobProgressControl":U).
        THIS-OBJECT:ultraButton1 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:lbJobName = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraButton1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = System.Windows.Forms.AnchorStyles:Top.
        THIS-OBJECT:ultraButton1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        THIS-OBJECT:ultraButton1:Appearance = appearance1.
        THIS-OBJECT:ultraButton1:AutoSize = TRUE.
        THIS-OBJECT:ultraButton1:Location = NEW System.Drawing.Point(354, 3).
        THIS-OBJECT:ultraButton1:Name = "ultraButton1":U.
        THIS-OBJECT:ultraButton1:Size = NEW System.Drawing.Size(26, 26).
        THIS-OBJECT:ultraButton1:TabIndex = 0.
        THIS-OBJECT:ultraButton1:Click:Subscribe(THIS-OBJECT:ultraButton1_Click).
        /*  */
        /* lbJobName */
        /*  */
        THIS-OBJECT:lbJobName:Location = NEW System.Drawing.Point(3, 5).
        THIS-OBJECT:lbJobName:Name = "lbJobName":U.
        THIS-OBJECT:lbJobName:Size = NEW System.Drawing.Size(345, 23).
        THIS-OBJECT:lbJobName:TabIndex = 1.
        THIS-OBJECT:lbJobName:Text = "Jobname":U.
        /*  */
        /* BackgroundJobProgressControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButton1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:lbJobName).
        THIS-OBJECT:Name = "BackgroundJobProgressControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(383, 31).
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Job Monitor for the given job
        Notes:
        @param poJob The Background Job
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InitializeJobMonitor (poJob AS BackgroundJob):

        DEFINE VARIABLE oMonitor AS BackgroundJobMonitorControl NO-UNDO .

        oMonitor = {Consultingwerk/get-service.i Consultingwerk.Windows.BackgroundOperation.BackgroundJobMonitorControl} .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oMonitor """The Job Monitor Control"":U"} .

        THIS-OBJECT:BackgroundJob = poJob .

        IF poJob:JobTitle > "":U THEN
            THIS-OBJECT:lbJobName:Text = poJob:JobTitle .
        ELSE
            THIS-OBJECT:lbJobName:Text = poJob:ProcedureName .

        poJob:ProcessFinished:Subscribe (JobFinishedHandler) .

        oMonitor:AddJobProgressControl (THIS-OBJECT) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ProcessFinished event of the Job
        Notes:
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID JobFinishedHandler (sender AS Progress.Lang.Object,
                                              e AS Consultingwerk.EventArgs):

        THIS-OBJECT:Parent:Controls:Remove (THIS-OBJECT) .

        THIS-OBJECT:BackgroundJob:ProcessFinished:Unsubscribe (JobFinishedHandler) .

        DELETE OBJECT THIS-OBJECT .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the ultraButton1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraButton1_Click (INPUT sender AS System.Object, INPUT e AS System.EventArgs):

        IF VALID-OBJECT (THIS-OBJECT:BackgroundJob) THEN
            THIS-OBJECT:BackgroundJob:Stop () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the BackgroundJobProgressControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC BackgroundJobProgressControl ():

    END DESTRUCTOR .

END CLASS.
