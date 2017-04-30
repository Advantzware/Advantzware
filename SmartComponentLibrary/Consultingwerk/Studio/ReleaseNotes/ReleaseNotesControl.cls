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
    File        : ReleaseNotesControl
    Purpose     : Displays release notes within Consultingwerk Studio applications
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Oct 03 16:22:43 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                             FROM PROPATH .
USING Consultingwerk.Studio.ReleaseNotes.*         FROM PROPATH .
USING Consultingwerk.Util.*                        FROM PROPATH .
USING Consultingwerk.Windows.BackgroundOperation.* FROM PROPATH.
USING Consultingwerk.Windows.API.*                 FROM PROPATH .
USING Infragistics.Win.*                           FROM ASSEMBLY .
USING Infragistics.Win.Misc.*                      FROM ASSEMBLY .
USING Infragistics.Win.FormattedLinkLabel.*        FROM ASSEMBLY .
USING Infragistics.Win.UltraWinDock.*              FROM ASSEMBLY .
USING Progress.Lang.*                              FROM PROPATH .
USING Progress.Windows.*                           FROM ASSEMBLY .
USING System.Windows.Forms.*                       FROM ASSEMBLY .

CLASS Consultingwerk.Studio.ReleaseNotes.ReleaseNotesControl INHERITS UserControl:

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE timer1 AS System.Windows.Forms.Timer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraFlowLayoutManager1 AS Infragistics.Win.Misc.UltraFlowLayoutManager NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraGridBagLayoutPanel1 AS Infragistics.Win.Misc.UltraGridBagLayoutPanel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraGroupBox1 AS Infragistics.Win.Misc.UltraGroupBox NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraPanel1 AS Infragistics.Win.Misc.UltraPanel NO-UNDO.

    DEFINE VARIABLE oJob AS BackgroundJob NO-UNDO .
    DEFINE PRIVATE VARIABLE ultraPanel2 AS Infragistics.Win.Misc.UltraPanel NO-UNDO.

    {Consultingwerk/Studio/ReleaseNotes/ttFeed.i}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ReleaseNotesControl class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ReleaseNotesControl ():

        SUPER().
        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Creates Controls for a single feed entry
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AddControls ():

        DEFINE VARIABLE oTitle        AS UltraLabel              NO-UNDO .
        DEFINE VARIABLE oAge          AS UltraLabel              NO-UNDO .
        DEFINE VARIABLE oCategories   AS UltraLabel              NO-UNDO .
        DEFINE VARIABLE oLink         AS UltraFormattedLinkLabel NO-UNDO .
        DEFINE VARIABLE oSpace        AS UltraLabel              NO-UNDO .

        DEFINE VARIABLE cUrl          AS CHARACTER               NO-UNDO .
        DEFINE VARIABLE cDate         AS CHARACTER               NO-UNDO .
        DEFINE VARIABLE dtReleaseDate AS DATE                    NO-UNDO .

        ASSIGN cUrl = REPLACE (ttFeed.EntryLink,
                               "http://jira.consultingwerkcloud.com:8090":U,
                               "http://confluence.consultingwerkcloud.com":U)

        oTitle = NEW UltraLabel () .
        oTitle:Text = ttFeed.EntryTitle .
        oTitle:AutoSize = TRUE .
        oTitle:StyleSetName = "Start Page Title":U .

        ultraPanel2:ClientArea:Controls:Add (oTitle) .

        ASSIGN cDate         = ENTRY (1, ttFeed.Published, "T":U)
               dtReleaseDate = DATE (INTEGER (ENTRY (2, cDate, "-":U)),
                                     INTEGER (ENTRY (3, cDate, "-":U)),
                                     INTEGER (ENTRY (1, cDate, "-":U))) .

        oAge = NEW UltraLabel () .
        oAge:AutoSize = TRUE .
        oAge:StyleSetName = "Start Page Categories":U .

        IF TODAY - dtReleaseDate > 1 THEN
            ASSIGN oAge:Text = SUBSTITUTE ("Released &1 days ago.":U, (TODAY - dtReleaseDate)) .
        ELSE IF TODAY - dtReleaseDate = 1 THEN
            ASSIGN oAge:Text = "Released yesterday.":U .
        ELSE
            ASSIGN oAge:Text = "Released today.":U .

        ultraPanel2:ClientArea:Controls:Add (oAge) .

        oCategories = NEW UltraLabel () .
        oCategories:Text = REPLACE (ttFeed.Categories, ",":U, ", ":U) .
        oCategories:AutoSize = TRUE .
        oCategories:StyleSetName = "Start Page Categories":U .

        ultraPanel2:ClientArea:Controls:Add (oCategories) .

        oLink = NEW UltraFormattedLinkLabel () .
        oLink:Value = SUBSTITUTE ("<a href=~"&1~">&1</a>":U, cUrl) .
        oLink:AutoSize = TRUE .
        oLink:StyleSetName = "Start Page":U .
        oLink:Tag = BOX (cUrl) .

        ultraPanel2:ClientArea:Controls:Add (oLink) .

        oLink:Click:Subscribe (LinkClickHandler) .

        oSpace = NEW UltraLabel () .
        oSpace:Text = " ":U .
        ultraPanel2:ClientArea:Controls:Add (oSpace) .

    END METHOD .

    /**
     * Purpose: Displays the error text, from the file generated by the
     *          background job
     * Notes:
     * @param pcFileName The file name with the error text to display
     */
    METHOD PROTECTED VOID DisplayErrorText (pcFileName AS CHARACTER):

        DEFINE VARIABLE oBrowser AS WebBrowser NO-UNDO .

        oBrowser = NEW WebBrowser() .
        oBrowser:Dock = DockStyle:Fill .
        oBrowser:AllowWebBrowserDrop = FALSE .
        oBrowser:IsWebBrowserContextMenuEnabled = FALSE .
        oBrowser:ScriptErrorsSuppressed = TRUE .

        ultraPanel2:ClientArea:Controls:Add (oBrowser) .

        oBrowser:Navigate(pcFileName) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Docks the Control in a Tab Group relatively to another .NET Control
        Notes:
        @param poDockManager The reference to the UltraDockManager
        @param poControl The reference to the control that we should dock the Release Notes Control next to it
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DockControl (poDockManager AS UltraDockManager,
                                    poControl AS System.Windows.Forms.Control):

        DEFINE VARIABLE oControls AS "System.Windows.Forms.Control[]" NO-UNDO .
        DEFINE VARIABLE oPane     AS DockableControlPane              NO-UNDO .

        /* Mike Fechner, Consultingwerk Ltd. 26.03.2015
           Fix settings for new Dockable pane */
        oPane = poDockManager:PaneFromControl (THIS-OBJECT) .

        IF NOT VALID-OBJECT (oPane) THEN DO:
            oControls = {Consultingwerk/new-array.i System.Windows.Forms.Control 1} .
            oControls:SetValue (THIS-OBJECT, 0) .

            poDockManager:DockControls (oControls,
                                        DockedLocation:DockedLeft,
                                        ChildPaneStyle:TabGroup) .

            oPane = poDockManager:PaneFromControl (THIS-OBJECT) .

            IF VALID-OBJECT (oPane) THEN DO:
                oPane:Text = "Start Page"{&TRAN} .
                oPane:Settings:Appearance:Image = ImageHelper:Load
                        ("Consultingwerk/Studio/ReleaseNotes/newspaper_16.png":U) .
            END.
        END.
        ELSE DO:
                oPane:Text = "Start Page"{&TRAN} .
                oPane:Settings:Appearance:Image = ImageHelper:Load
                        ("Consultingwerk/Studio/ReleaseNotes/newspaper_16.png":U) .
        END.

        oPane:Settings:AllowPin = DefaultableBoolean:FALSE.
        poDockManager:PaneFromControl (poControl):Parent:ChildPaneStyle = ChildPaneStyle:TabGroup .
        oPane:RepositionToGroup (poDockManager:PaneFromControl (poControl):Parent) .
        oPane:Activate() .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Visual Design
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID InitializeComponent ():

        /* NOTE: The following method is automatically generated.

        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.

        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE gridBagConstraint1 AS Infragistics.Win.Layout.GridBagConstraint NO-UNDO.
        gridBagConstraint1 = NEW Infragistics.Win.Layout.GridBagConstraint().
        THIS-OBJECT:ultraGroupBox1 = NEW Infragistics.Win.Misc.UltraGroupBox().
        THIS-OBJECT:ultraGridBagLayoutPanel1 = NEW Infragistics.Win.Misc.UltraGridBagLayoutPanel().
        THIS-OBJECT:ultraPanel1 = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:timer1 = NEW System.Windows.Forms.Timer(THIS-OBJECT:components).
        THIS-OBJECT:ultraFlowLayoutManager1 = NEW Infragistics.Win.Misc.UltraFlowLayoutManager(THIS-OBJECT:components).
        THIS-OBJECT:ultraPanel2 = NEW Infragistics.Win.Misc.UltraPanel().
        CAST(THIS-OBJECT:ultraGroupBox1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraGroupBox1:SuspendLayout().
        CAST(THIS-OBJECT:ultraGridBagLayoutPanel1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraGridBagLayoutPanel1:SuspendLayout().
        THIS-OBJECT:ultraPanel1:ClientArea:SuspendLayout().
        THIS-OBJECT:ultraPanel1:SuspendLayout().
        CAST(THIS-OBJECT:ultraFlowLayoutManager1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraPanel2:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraGroupBox1 */
        /*  */
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:ultraPanel2).
        gridBagConstraint1:Fill = Infragistics.Win.Layout.FillType:Both.
        gridBagConstraint1:OriginX = 0.
        gridBagConstraint1:OriginY = 0.
        gridBagConstraint1:WeightX = Progress.Util.CastUtil:ToSingle(2000).
        gridBagConstraint1:WeightY = Progress.Util.CastUtil:ToSingle(2000).
        THIS-OBJECT:ultraGridBagLayoutPanel1:SetGridBagConstraint(THIS-OBJECT:ultraGroupBox1, gridBagConstraint1).
        THIS-OBJECT:ultraGroupBox1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraGroupBox1:Name = "ultraGroupBox1":U.
        THIS-OBJECT:ultraGridBagLayoutPanel1:SetPreferredSize(THIS-OBJECT:ultraGroupBox1, NEW System.Drawing.Size(200, 110)).
        THIS-OBJECT:ultraGroupBox1:Size = NEW System.Drawing.Size(465, 391).
        THIS-OBJECT:ultraGroupBox1:StyleSetName = "Start Page":U.
        THIS-OBJECT:ultraGroupBox1:TabIndex = 1.
        THIS-OBJECT:ultraGroupBox1:Text = "Release Notes":U.
        /*  */
        /* ultraGridBagLayoutPanel1 */
        /*  */
        THIS-OBJECT:ultraGridBagLayoutPanel1:Controls:Add(THIS-OBJECT:ultraGroupBox1).
        THIS-OBJECT:ultraGridBagLayoutPanel1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraGridBagLayoutPanel1:ExpandToFitHeight = TRUE.
        THIS-OBJECT:ultraGridBagLayoutPanel1:ExpandToFitWidth = TRUE.
        THIS-OBJECT:ultraGridBagLayoutPanel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraGridBagLayoutPanel1:Name = "ultraGridBagLayoutPanel1":U.
        THIS-OBJECT:ultraGridBagLayoutPanel1:Size = NEW System.Drawing.Size(465, 391).
        THIS-OBJECT:ultraGridBagLayoutPanel1:TabIndex = 2.
        /*  */
        /* ultraPanel1 */
        /*  */
        /*  */
        /* ultraPanel1.ClientArea */
        /*  */
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ultraGridBagLayoutPanel1).
        THIS-OBJECT:ultraPanel1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraPanel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraPanel1:Name = "ultraPanel1":U.
        THIS-OBJECT:ultraPanel1:Size = NEW System.Drawing.Size(465, 391).
        THIS-OBJECT:ultraPanel1:StyleSetName = "Start Page":U.
        THIS-OBJECT:ultraPanel1:TabIndex = 3.
        /*  */
        /* timer1 */
        /*  */
        THIS-OBJECT:timer1:Interval = 1000.
        THIS-OBJECT:timer1:Tick:Subscribe(THIS-OBJECT:timer1_Tick).
        /*  */
        /* ultraFlowLayoutManager1 */
        /*  */
        THIS-OBJECT:ultraFlowLayoutManager1:ContainerControl = THIS-OBJECT:ultraPanel2:ClientArea.
        THIS-OBJECT:ultraFlowLayoutManager1:HorizontalAlignment = Infragistics.Win.Layout.DefaultableFlowLayoutAlignment:Near.
        THIS-OBJECT:ultraFlowLayoutManager1:Orientation = System.Windows.Forms.Orientation:Vertical.
        THIS-OBJECT:ultraFlowLayoutManager1:VerticalAlignment = Infragistics.Win.Layout.DefaultableFlowLayoutAlignment:Near.
        THIS-OBJECT:ultraFlowLayoutManager1:WrapItems = FALSE.
        /*  */
        /* ultraPanel2 */
        /*  */
        THIS-OBJECT:ultraPanel2:AutoScroll = TRUE.
        THIS-OBJECT:ultraPanel2:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraPanel2:Location = NEW System.Drawing.Point(3, 16).
        THIS-OBJECT:ultraPanel2:Name = "ultraPanel2":U.
        THIS-OBJECT:ultraPanel2:Size = NEW System.Drawing.Size(459, 372).
        THIS-OBJECT:ultraPanel2:StyleSetName = "Start Page":U.
        THIS-OBJECT:ultraPanel2:TabIndex = 0.
        /*  */
        /* ReleaseNotesControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraPanel1).
        THIS-OBJECT:Name = "ReleaseNotesControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(465, 391).
        CAST(THIS-OBJECT:ultraGroupBox1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraGroupBox1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraGridBagLayoutPanel1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraGridBagLayoutPanel1:ResumeLayout(FALSE).
        THIS-OBJECT:ultraPanel1:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:ultraPanel1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraFlowLayoutManager1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraPanel2:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Then handler for the LinkClickHandler of a hyperlink
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID LinkClickHandler (sender AS System.Object,
                                            e AS System.EventArgs):

        Win32:ShellExecute ("open":U, UNBOX (CAST (sender, System.Windows.Forms.Control):Tag)) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):

        SUPER:OnLoad (e) .

        timer1:Enabled = TRUE .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ProcessFinished event of the BackgroundJob
        Notes:
        @param sender The reference to the object that raised the event
        @param e The Consultingwerk.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ProcessFinishedHandler (sender AS Progress.Lang.Object,
                                                  e AS Consultingwerk.EventArgs):

        DEFINE VARIABLE oParser   AS ReleaseNotesParser NO-UNDO .
        DEFINE VARIABLE cFileName AS CHARACTER          NO-UNDO .

        ASSIGN cFileName = oJob:OutputData
               cFileName = TRIM (cFileName).

        IF NOT FileHelper:Exists (cFileName) THEN
            RETURN .

        oParser = NEW ReleaseNotesParser () .
        oParser:ParseAtomFeed (cFileName, OUTPUT TABLE ttFeed) .

        IF BufferHelper:NumRecords (BUFFER ttFeed:HANDLE) = 0 THEN
            THIS-OBJECT:DisplayErrorText (cFileName) .
        ELSE
            FOR EACH ttFeed BY ttFeed.EntryTitle DESCENDING ON ERROR UNDO, THROW:

                THIS-OBJECT:AddControls () .

            END.

        CATCH err AS Progress.Lang.Error:
            THIS-OBJECT:DisplayErrorText (cFileName) .
        END CATCH.

        FINALLY:
            OS-DELETE VALUE (cFileName) .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Tick event of the timer1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID timer1_Tick (sender AS System.Object,
                                     e AS System.EventArgs):

        DEFINE VARIABLE cIniFileName AS CHARACTER NO-UNDO.

        timer1:Enabled = FALSE .

        oJob = NEW BackgroundJob (FileHelper:FindFile ("Consultingwerk/Studio/ReleaseNotes/download-rss.p":U)) .

        ASSIGN cIniFileName = StartupParameterHelper:GetStartupParameterValue ("-ininame":U) .

        IF cIniFileName > "":U THEN
           oJob:Parameter = SUBSTITUTE ("-basekey ini -ininame ~"&1~"":U , FileHelper:FindFile(cIniFileName)) .

        oJob:AddToJobMonitor = FALSE .
        oJob:SendQuit = FALSE .
        oJob:UiEnvironment = UiEnvironmentEnum:Gui .
        oJob:ProcessFinished:Subscribe (ProcessFinishedHandler) .

        oJob:Start (THIS-OBJECT) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the ReleaseNotesControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC ReleaseNotesControl ():

    END DESTRUCTOR .

END CLASS.
