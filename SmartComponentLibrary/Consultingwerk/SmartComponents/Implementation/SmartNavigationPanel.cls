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
    File        : SmartNavigationPanel
    Purpose     : A simple navigation panel in Windows Vista / Windows 7
                  Explorer style
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Mar 10 18:49:25 CET 2010
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*                                    FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*                                    FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.ISmartNavigationSource         FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*                              FROM PROPATH .
USING Consultingwerk.SmartComponents.Resources.*                               FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.Design.IDesignNavigationSource FROM ASSEMBLY .
USING Progress.Lang.*                                                          FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Implementation.SmartNavigationPanel
    INHERITS SmartUserControl
    IMPLEMENTS ISmartNavigationSource, IDoNotCheckOkToClose,
               Consultingwerk.SmartComponents.Interfaces.Design.IDesignNavigationSource:

    DEFINE PRIVATE VARIABLE BtnPrev AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE BtnNext AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE BtnLast AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE BtnFirst AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE VARIABLE oSmartNavigationTarget AS ISmartNavigationTarget NO-UNDO .

    /* Mike Fechner, Consultingwerk Ltd. 03.09.2013
       Static only on 10.2B to workaround too-many-temp-table issue
       Not static on 11.x to avoid issues with the combination of static
       and hybrids on 11.2 and 11.3, OE defect OE00240707
       http://knowledgebase.progress.com/articles/Article/000042229?popup=true */
    DEFINE PRIVATE
    &IF PROVERSION BEGINS "10.2":U &THEN
    STATIC
    &ENDIF TEMP-TABLE ttSmartNavigationTarget NO-UNDO
        FIELD RecordOwner             AS Progress.Lang.Object
        FIELD SmartNavigationTarget   AS Progress.Lang.Object
        FIELD SmartNavigationPosition AS CHARACTER
        INDEX RecordOwner IS UNIQUE RecordOwner SmartNavigationTarget.
        .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartToolbarController class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartNavigationPanel ():

        SUPER().
        InitializeComponent().

        DisableTools ("NavigationFirst,NavigationPrev,NavigationNext,NavigationLast":U) .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

     /*------------------------------------------------------------------------------
        Purpose: Activate the given SmartNavigationTarget if it is registered with
                 the SmartNavigationSource.
        Notes:   This Method has to be called if one Toolbar handels two or more
                 SmartDataSources at a time.
                 Enforced by Interface ISmartNavigationSource.
        @param poSmartNavigationTarget The ISmartNavigationTarget instance to activate
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ActivateSmartNavigationTarget (poSmartNavigationTarget AS ISmartNavigationTarget).

        IF poSmartNavigationTarget = ? THEN DO:
            oSmartNavigationTarget = ? .

            THIS-OBJECT:UpdateSmartNavigationTargetPosition (?, "":U).

            RETURN .
        END.

        FIND ttSmartNavigationTarget WHERE ttSmartNavigationTarget.RecordOwner           = THIS-OBJECT
                                       AND ttSmartNavigationTarget.SmartNavigationTarget = poSmartNavigationTarget NO-LOCK NO-ERROR.

        IF NOT AVAILABLE ttSmartNavigationTarget THEN
            UNDO, THROW NEW AppError ("Invalid SmartNavigationTarget!":U, 0).

        oSmartNavigationTarget = poSmartNavigationTarget .

        THIS-OBJECT:UpdateSmartNavigationTargetPosition (poSmartNavigationTarget,
                                                         ttSmartNavigationTarget.SmartNavigationPosition).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of BtnFirst
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BtnFirst_Click (sender AS System.Object,
                                        e AS System.EventArgs):

        IF VALID-OBJECT (oSmartNavigationTarget) THEN
          oSmartNavigationTarget:FetchFirst () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of BtnLast
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BtnLast_Click (sender AS System.Object,
                                       e AS System.EventArgs):

        IF VALID-OBJECT (oSmartNavigationTarget) THEN
            oSmartNavigationTarget:FetchLast () .

    END METHOD.


     /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of BtnNext
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BtnNext_Click (sender AS System.Object,
                                       e AS System.EventArgs):

        IF VALID-OBJECT (oSmartNavigationTarget) THEN
            oSmartNavigationTarget:FetchNext () .

    END METHOD.


     /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of BtnPrev
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BtnPrev_Click (sender AS System.Object,
                                       e AS System.EventArgs):

        IF VALID-OBJECT (oSmartNavigationTarget) THEN
            oSmartNavigationTarget:FetchPrev () .

    END METHOD.


    /*------------------------------------------------------------------------------
        Purpose: Deregisters a SmartNavigationTarget from the SmartNavigationSource.
        Notes:
        @param poSmartNavigationTarget The ISmartNavigationTarget to deregister
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeregisterSmartNavigationTarget (poSmartNavigationTarget AS ISmartNavigationTarget):

        FIND ttSmartNavigationTarget
            WHERE ttSmartNavigationTarget.RecordOwner           = THIS-OBJECT
              AND ttSmartNavigationTarget.SmartNavigationTarget = poSmartNavigationTarget
                    NO-ERROR .

        IF AVAILABLE ttSmartNavigationTarget THEN
          DELETE ttSmartNavigationTarget .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Disables the tool with the given Key when it exists in this instance.
                 Invoked by DisableTools.
        Notes:
        @param  pcToolKey Key of the Tool ButtonTool to be disabled
        @return True if the Tool exists, False if the Tool does not Exist
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL DisableTool (pcToolKey AS CHARACTER):

        CASE pcToolKey:
            WHEN "NavigationFirst":U
                THEN BtnFirst:Enabled = FALSE .
            WHEN "NavigationPrev":U
                THEN BtnPrev:Enabled = FALSE .
            WHEN "NavigationNext":U
                THEN BtnNext:Enabled = FALSE .
            WHEN "NavigationLast":U
                THEN BtnLast:Enabled = FALSE .
            OTHERWISE
                RETURN FALSE .

        END CASE .

        RETURN TRUE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Disables a list of tools when they exist in this instance.
                 This Method is called from within the Toolbar depending on what
                 action has been performed and what actions are possible with the
                 current SmartTableIOTarget and SmartNavigationTarget.
        Notes:
        @param pcToolKeys Comma delimeted list of ButtonTool Keys to be disabled
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DisableTools (pcToolKeys AS CHARACTER):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DO i = 1 TO NUM-ENTRIES(pcToolKeys):
            DisableTool (ENTRY(i, pcToolKeys)) .
        END.

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Enables the tool with the given Key when it exists in this instance.
        Notes:   Invoked by EnableTools.
        @param  pcToolKey Key of the Tool ButtonTool to be enabled
        @return True if the Tool exists, False if the Tool does not Exist
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL EnableTool (pcToolKey AS CHARACTER):

        CASE pcToolKey:
            WHEN "NavigationFirst":U
                THEN BtnFirst:Enabled = TRUE .
            WHEN "NavigationPrev":U
                THEN BtnPrev:Enabled = TRUE .
            WHEN "NavigationNext":U
                THEN BtnNext:Enabled = TRUE .
            WHEN "NavigationLast":U
                THEN BtnLast:Enabled = TRUE .
            OTHERWISE
                RETURN FALSE .

        END CASE .

        RETURN TRUE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Enables a list of tools when they exist in this instance.
        Notes:   This Method is called from within the Toolbar depending on what
                 action has been performed and what actions are possible with the
                 current SmartTableIOTarget and SmartNavigationTarget.
        @param pcToolKeys Comma delimeted list of ButtonTool Keys to be disabled
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EnableTools (pcToolKeys AS CHARACTER):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DO i = 1 TO NUM-ENTRIES(pcToolKeys):
            EnableTool (ENTRY(i, pcToolKeys)) .
        END.

    END METHOD.

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
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.SmartComponents.Implementation.SmartNavigationPanel":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:BtnFirst = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:BtnPrev = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:BtnNext = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:BtnLast = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* BtnFirst */
        /*  */
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        appearance1:ImageHAlign = Infragistics.Win.HAlign:Center.
        THIS-OBJECT:BtnFirst:Appearance = appearance1.
        THIS-OBJECT:BtnFirst:ButtonStyle = Infragistics.Win.UIElementButtonStyle:WindowsVistaToolbarButton.
        THIS-OBJECT:BtnFirst:ImageSize = NEW System.Drawing.Size(32, 32).
        THIS-OBJECT:BtnFirst:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:BtnFirst:Margin = NEW System.Windows.Forms.Padding(0).
        THIS-OBJECT:BtnFirst:Name = "BtnFirst":U.
        THIS-OBJECT:BtnFirst:ShapeImage = CAST(resources:GetObject("BtnFirst.ShapeImage":U), System.Drawing.Image).
        THIS-OBJECT:BtnFirst:ShowFocusRect = FALSE.
        THIS-OBJECT:BtnFirst:ShowOutline = FALSE.
        THIS-OBJECT:BtnFirst:Size = NEW System.Drawing.Size(32, 32).
        THIS-OBJECT:BtnFirst:TabIndex = 0.
        THIS-OBJECT:BtnFirst:UseAppStyling = FALSE.
        THIS-OBJECT:BtnFirst:UseFlatMode = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:BtnFirst:UseOsThemes = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:BtnFirst:WrapText = FALSE.
        THIS-OBJECT:BtnFirst:Click:Subscribe(THIS-OBJECT:BtnFirst_Click).
        /*  */
        /* BtnPrev */
        /*  */
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        appearance2:ImageHAlign = Infragistics.Win.HAlign:Center.
        THIS-OBJECT:BtnPrev:Appearance = appearance2.
        THIS-OBJECT:BtnPrev:ButtonStyle = Infragistics.Win.UIElementButtonStyle:WindowsVistaToolbarButton.
        THIS-OBJECT:BtnPrev:ImageSize = NEW System.Drawing.Size(32, 32).
        THIS-OBJECT:BtnPrev:Location = NEW System.Drawing.Point(32, 0).
        THIS-OBJECT:BtnPrev:Margin = NEW System.Windows.Forms.Padding(0).
        THIS-OBJECT:BtnPrev:Name = "BtnPrev":U.
        THIS-OBJECT:BtnPrev:ShapeImage = CAST(resources:GetObject("BtnPrev.ShapeImage":U), System.Drawing.Image).
        THIS-OBJECT:BtnPrev:ShowFocusRect = FALSE.
        THIS-OBJECT:BtnPrev:ShowOutline = FALSE.
        THIS-OBJECT:BtnPrev:Size = NEW System.Drawing.Size(32, 32).
        THIS-OBJECT:BtnPrev:TabIndex = 1.
        THIS-OBJECT:BtnPrev:UseAppStyling = FALSE.
        THIS-OBJECT:BtnPrev:UseFlatMode = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:BtnPrev:UseOsThemes = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:BtnPrev:WrapText = FALSE.
        THIS-OBJECT:BtnPrev:Click:Subscribe(THIS-OBJECT:BtnPrev_Click).
        /*  */
        /* BtnNext */
        /*  */
        appearance3:Image = CAST(resources:GetObject("appearance3.Image":U), System.Object).
        appearance3:ImageHAlign = Infragistics.Win.HAlign:Center.
        THIS-OBJECT:BtnNext:Appearance = appearance3.
        THIS-OBJECT:BtnNext:ButtonStyle = Infragistics.Win.UIElementButtonStyle:WindowsVistaToolbarButton.
        THIS-OBJECT:BtnNext:ImageSize = NEW System.Drawing.Size(32, 32).
        THIS-OBJECT:BtnNext:Location = NEW System.Drawing.Point(64, 0).
        THIS-OBJECT:BtnNext:Margin = NEW System.Windows.Forms.Padding(0).
        THIS-OBJECT:BtnNext:Name = "BtnNext":U.
        THIS-OBJECT:BtnNext:ShapeImage = CAST(resources:GetObject("BtnNext.ShapeImage":U), System.Drawing.Image).
        THIS-OBJECT:BtnNext:ShowFocusRect = FALSE.
        THIS-OBJECT:BtnNext:ShowOutline = FALSE.
        THIS-OBJECT:BtnNext:Size = NEW System.Drawing.Size(32, 32).
        THIS-OBJECT:BtnNext:TabIndex = 2.
        THIS-OBJECT:BtnNext:UseAppStyling = FALSE.
        THIS-OBJECT:BtnNext:UseFlatMode = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:BtnNext:UseOsThemes = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:BtnNext:WrapText = FALSE.
        THIS-OBJECT:BtnNext:Click:Subscribe(THIS-OBJECT:BtnNext_Click).
        /*  */
        /* BtnLast */
        /*  */
        appearance4:Image = CAST(resources:GetObject("appearance4.Image":U), System.Object).
        appearance4:ImageHAlign = Infragistics.Win.HAlign:Center.
        THIS-OBJECT:BtnLast:Appearance = appearance4.
        THIS-OBJECT:BtnLast:ButtonStyle = Infragistics.Win.UIElementButtonStyle:WindowsVistaToolbarButton.
        THIS-OBJECT:BtnLast:ImageSize = NEW System.Drawing.Size(32, 32).
        THIS-OBJECT:BtnLast:Location = NEW System.Drawing.Point(96, 0).
        THIS-OBJECT:BtnLast:Margin = NEW System.Windows.Forms.Padding(0).
        THIS-OBJECT:BtnLast:Name = "BtnLast":U.
        THIS-OBJECT:BtnLast:ShapeImage = CAST(resources:GetObject("BtnLast.ShapeImage":U), System.Drawing.Image).
        THIS-OBJECT:BtnLast:ShowFocusRect = FALSE.
        THIS-OBJECT:BtnLast:ShowOutline = FALSE.
        THIS-OBJECT:BtnLast:Size = NEW System.Drawing.Size(32, 32).
        THIS-OBJECT:BtnLast:TabIndex = 3.
        THIS-OBJECT:BtnLast:UseAppStyling = FALSE.
        THIS-OBJECT:BtnLast:UseFlatMode = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:BtnLast:UseOsThemes = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:BtnLast:WrapText = FALSE.
        THIS-OBJECT:BtnLast:Click:Subscribe(THIS-OBJECT:BtnLast_Click).
        /*  */
        /* SmartNavigationPanel */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:AutoSize = TRUE.
        THIS-OBJECT:AutoSizeMode = System.Windows.Forms.AutoSizeMode:GrowAndShrink.
        THIS-OBJECT:BackColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:Controls:Add(THIS-OBJECT:BtnLast).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:BtnNext).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:BtnPrev).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:BtnFirst).
        THIS-OBJECT:Margin = NEW System.Windows.Forms.Padding(0).
        THIS-OBJECT:Name = "SmartNavigationPanel":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(128, 32).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

      /*------------------------------------------------------------------------------
        Purpose: Registers a SmartNavigationTarget with the SmartNavigationSource.
        Notes:   This Method is invoked by the SmartDataAdapter in the setter of
                 the Property SmartNavigationSource.
                 Enforced by the Interface ISmartNavigationSource.
        @param poSmartNavigationTarget The ISmartNavigationTarget to register
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RegisterSmartNavigationTarget (poSmartNavigationTarget AS ISmartNavigationTarget):

        CREATE ttSmartNavigationTarget.

        ASSIGN
            ttSmartNavigationTarget.RecordOwner           = THIS-OBJECT
            ttSmartNavigationTarget.SmartNavigationTarget = poSmartNavigationTarget
            .

        RELEASE ttSmartNavigationTarget.

        IF NOT VALID-OBJECT(oSmartNavigationTarget) THEN
            ActivateSmartNavigationTarget(poSmartNavigationTarget).

        /* Update the tooblar buttons when the link is added */
        UpdateSmartNavigationTargetPosition (poSmartNavigationTarget, poSmartNavigationTarget:RecordPosition) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the SmartNavigationTargetPositionChanged event of the SmartNavigationTarget
        Notes:
        @param sender The reference to the object that raised the evnet
        @param e The SmartNavigationTargetPositionChangedEventArgs instance with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID UpdateSmartNavigationTargetPosition (sender AS System.Object,
                                                            e AS SmartNavigationTargetPositionChangedEventArgs):

        DEFINE VARIABLE oSmartNavigationTarget AS ISmartNavigationTarget NO-UNDO .

        IF VALID-OBJECT (sender) AND TYPE-OF (sender, ISmartNavigationTarget) THEN
            ASSIGN oSmartNavigationTarget = CAST (sender, ISmartNavigationTarget) .

        UpdateSmartNavigationTargetPosition(oSmartNavigationTarget, e:SmartNavigationTargetPosition) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Handles the update of a Position from the SmartNavigationTarget.
                 This Method is called from the SmartDataAdapter derived Classes
                 if the PositionChangedHandler is executed or from inside the
                 ISmartNavigationSource implementing class when the Method
                 UpdateSmartNavigationTargetState or RegisterSmartNavigationTarget
                 are executed.
        Notes:   Enforced by Interface ISmartNavigationSource.
        @param poSmartNavigationTarget The reference to the current SmartNavigationTarget
        @param pcPosition The RecordPositionEnum member indicating the new navigation target position
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID UpdateSmartNavigationTargetPosition (poSmartNavigationTarget AS ISmartNavigationTarget,
                                                               pcPosition AS CHARACTER):

        DEFINE VARIABLE cState AS CHARACTER NO-UNDO.

        THIS-OBJECT:SuspendLayout () .

        IF poSmartNavigationTarget = ? THEN DO:
            /* Mike Fechner, Consultingwerk Ltd. 05.02.2010
               When there is no active Navigation Target, set all buttons to disabled
               and (this is new) leave this routine. The remaining code is not required. */
            DisableTools ("NavigationFirst,NavigationPrev,NavigationNext,NavigationLast":U) .
            RETURN .
        END.

        /* Mike Fechner, Consultingwerk Ltd. 28.01.2010
           SmartNavigationTargetState goes over SmartNavigationTargetPosition */
        ASSIGN cState = poSmartNavigationTarget:SmartDataSourceState .

        IF cState = DataSourceStateEnum:Navigatable THEN DO:
            IF poSmartNavigationTarget = oSmartNavigationTarget THEN DO:
                CASE pcPosition:
                    WHEN RecordPositionEnum:FirstRecord THEN DO:
                        EnableTools ("NavigationNext,NavigationLast":U) .
                        DisableTools ("NavigationFirst,NavigationPrev":U) .
                    END.
                    WHEN RecordPositionEnum:LastRecord THEN DO:
                        EnableTools ("NavigationFirst,NavigationPrev":U) .
                        DisableTools ("NavigationNext,NavigationLast":U) .
                    END.
                    WHEN RecordPositionEnum:NotFirstOrLast THEN DO:
                        EnableTools ("NavigationFirst,NavigationPrev,NavigationNext,NavigationLast":U) .
                    END.
                    OTHERWISE
                        DisableTools ("NavigationFirst,NavigationPrev,NavigationNext,NavigationLast":U) .
                END CASE.
            END.
        END.
        ELSE
            /* Mike Fechner, Consultingwerk Ltd. 28.01.2010
               Currently in Update mode etc. */
            DisableTools ("NavigationFirst,NavigationPrev,NavigationNext,NavigationLast":U) .

        FIND FIRST ttSmartNavigationTarget WHERE ttSmartNavigationTarget.RecordOwner           = THIS-OBJECT
                                           AND   ttSmartNavigationTarget.SmartNavigationTarget = poSmartNavigationTarget
            NO-LOCK NO-ERROR .

        IF AVAILABLE(ttSmartNavigationTarget) THEN DO:
            ASSIGN ttSmartNavigationTarget.SmartNavigationPosition = pcPosition.

            RELEASE ttSmartNavigationTarget.
        END.

        FINALLY:
            THIS-OBJECT:ResumeLayout () .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the SmartNavigationTargetStateChanged event
        Notes:
        @param sender The reference to the object that raised the event
        @param e The SmartNavigationTargetStateChangedEventArgs instance with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID UpdateSmartNavigationTargetState (sender AS System.Object,
                                                         e AS SmartNavigationTargetStateChangedEventArgs ):

        DEFINE VARIABLE oSmartNavigationTarget AS ISmartNavigationTarget NO-UNDO .

        IF VALID-OBJECT (sender) AND TYPE-OF (sender, ISmartNavigationTarget) THEN
            ASSIGN oSmartNavigationTarget = CAST (sender, ISmartNavigationTarget) .

        UpdateSmartNavigationTargetState (oSmartNavigationTarget, e:SmartNavigationTargetState) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Handles the update of a State from the SmartNavigationTarget.
                 This Method is called from the SmartDataAdapter derived Classes
                 if the Property setter of SmartDataSourceState is executed.
        Notes:   Enforced by Interface ISmartNavigationSource.
        @param poSmartNavigationTarget The reference to the current SmartNavigationTarget
        @param pcState The RecordPositionEnum member indicating the new navigation target state, Updating or Navigatable
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID UpdateSmartNavigationTargetState (poSmartNavigationTarget AS ISmartNavigationTarget,
                                                            pcState AS CHARACTER):

        DEFINE VARIABLE cRecordPosition AS CHARACTER NO-UNDO.

        THIS-OBJECT:SuspendLayout () .

        CASE pcState:
            WHEN DataSourceStateEnum:Navigatable THEN DO:
                IF VALID-OBJECT(poSmartNavigationTarget) THEN DO:
                    ASSIGN cRecordPosition = poSmartNavigationTarget:RecordPosition .

                    UpdateSmartNavigationTargetPosition(poSmartNavigationTarget, cRecordPosition) .
                END.
            END .
            WHEN DataSourceStateEnum:Updating THEN DO:
                DisableTools ("NavigationFirst,NavigationPrev,NavigationNext,NavigationLast":U) .
            END .
        END CASE.

        FINALLY:
            THIS-OBJECT:ResumeLayout () .
        END FINALLY.

    END METHOD.

    DESTRUCTOR PUBLIC SmartNavigationPanel ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.