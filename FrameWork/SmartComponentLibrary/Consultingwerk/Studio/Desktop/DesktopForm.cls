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
    File        : DesktopForm
    Purpose     : Desktop form to launch various SmartComponent Library
                  tools
    Syntax      :
    Description : Loads items from Consultingwerk/Studio/Desktop/items.xml
                  or a file name referenced using the -icfparam parameter
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun May 17 15:05:14 CEST 2015
    Notes       : DesktopForm is based on the UltraCarousel Control which
                  is available from OpenEdge 11.5 on only
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                        FROM PROPATH .
USING Consultingwerk.Forms.*                  FROM PROPATH .
USING Consultingwerk.Studio.Desktop.*         FROM PROPATH .
USING Consultingwerk.Util.*                   FROM PROPATH .
USING Consultingwerk.Windows.API.*            FROM PROPATH .
USING Progress.Lang.*                         FROM PROPATH .
USING System.Diagnostics.*                    FROM ASSEMBLY.
&IF NOT PROVERSION BEGINS "10" AND
    NOT PROVERSION BEGINS "11.0" AND
    NOT PROVERSION BEGINS "11.1" AND
    NOT PROVERSION BEGINS "11.2" AND
    NOT PROVERSION BEGINS "11.3" AND
    NOT PROVERSION BEGINS "11.4" &THEN
USING Infragistics.Win.UIActionNotification.* FROM ASSEMBLY .
USING Infragistics.Win.UltraWinCarousel.*     FROM ASSEMBLY .

&ENDIF

CLASS Consultingwerk.Studio.Desktop.DesktopForm INHERITS BaseForm:

&IF NOT PROVERSION BEGINS "10" AND
    NOT PROVERSION BEGINS "11.0" AND
    NOT PROVERSION BEGINS "11.1" AND
    NOT PROVERSION BEGINS "11.2" AND
    NOT PROVERSION BEGINS "11.3" AND
    NOT PROVERSION BEGINS "11.4" &THEN

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE imageList1 AS System.Windows.Forms.ImageList NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraCarousel1 AS Infragistics.Win.UltraWinCarousel.UltraCarousel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraStatusBar1 AS Infragistics.Win.UltraWinStatusBar.UltraStatusBar NO-UNDO.

    {Consultingwerk/Studio/Desktop/ttDesktopItems.i}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DesktopForm class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DesktopForm ():

        SUPER().

        InitializeComponent().

        THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).

        THIS-OBJECT:ultraStatusBar1:Panels["proversion":U]:Text       = SessionHelper:FullVersionString() .
        THIS-OBJECT:ultraStatusBar1:Panels["frameworkversion":U]:Text = (NEW FrameworkVersion()):GetVersionString ().
        THIS-OBJECT:ultraStatusBar1:Panels["bitness":U]:Text          = SUBSTITUTE ("&1 bit":U, SessionHelper:ProcessBitness()) .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Creates the items in the UltraCarousel
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID CreateCarouselItems ():

        DEFINE VARIABLE oItem AS CarouselItem NO-UNDO .

        IF SESSION:ICFPARAMETER > "":U THEN DO:
            FILE-INFORMATION:FILE-NAME = SESSION:ICFPARAMETER .

            IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
                TEMP-TABLE ttDesktopItems:READ-XML ("file":U, FILE-INFORMATION:FULL-PATHNAME, ?, ?, ?) .
            ELSE DO:
                FILE-INFORMATION:FILE-NAME = "Consultingwerk/Studio/Desktop/items.xml":U .

                TEMP-TABLE ttDesktopItems:READ-XML ("file":U, FILE-INFORMATION:FULL-PATHNAME, ?, ?, ?) .
            END.
        END.
        ELSE DO:
            FILE-INFORMATION:FILE-NAME = "Consultingwerk/Studio/Desktop/items.xml":U .

            TEMP-TABLE ttDesktopItems:READ-XML ("file":U, FILE-INFORMATION:FULL-PATHNAME, ?, ?, ?) .
        END.

        THIS-OBJECT:ultraCarousel1:Items:Clear () .

        FOR EACH ttDesktopItems ON ERROR UNDO, THROW:
            oItem = NEW CarouselItem (STRING (ROWID (ttDesktopItems))) .
            oItem:Text = ttDesktopItems.ItemText .
            oItem:Settings:Appearance:Image = ImageHelper:Load (ttDesktopItems.ItemImage) .
            THIS-OBJECT:ultraCarousel1:Items:Add (oItem) .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Displays the text of the current item (slot 2) in the status bar
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DisplayCurrentItemText ():

        DEFINE VARIABLE oItem AS CarouselItem NO-UNDO .

        oItem = ultraCarousel1:GetItemFromItemSlot (2) .

        IF VALID-OBJECT (oItem) THEN
           ultraStatusBar1:Panels ["activeitem":U]:Text = oItem:Text .

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
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE carouselItem1 AS Infragistics.Win.UltraWinCarousel.CarouselItem NO-UNDO.
        carouselItem1 = NEW Infragistics.Win.UltraWinCarousel.CarouselItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE carouselItem2 AS Infragistics.Win.UltraWinCarousel.CarouselItem NO-UNDO.
        carouselItem2 = NEW Infragistics.Win.UltraWinCarousel.CarouselItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE carouselItem3 AS Infragistics.Win.UltraWinCarousel.CarouselItem NO-UNDO.
        carouselItem3 = NEW Infragistics.Win.UltraWinCarousel.CarouselItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE carouselItem4 AS Infragistics.Win.UltraWinCarousel.CarouselItem NO-UNDO.
        carouselItem4 = NEW Infragistics.Win.UltraWinCarousel.CarouselItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE carouselItem5 AS Infragistics.Win.UltraWinCarousel.CarouselItem NO-UNDO.
        carouselItem5 = NEW Infragistics.Win.UltraWinCarousel.CarouselItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE carouselItem6 AS Infragistics.Win.UltraWinCarousel.CarouselItem NO-UNDO.
        carouselItem6 = NEW Infragistics.Win.UltraWinCarousel.CarouselItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE carouselItem7 AS Infragistics.Win.UltraWinCarousel.CarouselItem NO-UNDO.
        carouselItem7 = NEW Infragistics.Win.UltraWinCarousel.CarouselItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Studio.Desktop.DesktopForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS System.Drawing.PointF EXTENT 10 NO-UNDO.
        arrayvar0[1] = CAST(resources:GetObject("ultraCarousel1.Path":U), System.Drawing.PointF).
        arrayvar0[2] = CAST(resources:GetObject("ultraCarousel1.Path1":U), System.Drawing.PointF).
        arrayvar0[3] = CAST(resources:GetObject("ultraCarousel1.Path2":U), System.Drawing.PointF).
        arrayvar0[4] = CAST(resources:GetObject("ultraCarousel1.Path3":U), System.Drawing.PointF).
        arrayvar0[5] = CAST(resources:GetObject("ultraCarousel1.Path4":U), System.Drawing.PointF).
        arrayvar0[6] = CAST(resources:GetObject("ultraCarousel1.Path5":U), System.Drawing.PointF).
        arrayvar0[7] = CAST(resources:GetObject("ultraCarousel1.Path6":U), System.Drawing.PointF).
        arrayvar0[8] = CAST(resources:GetObject("ultraCarousel1.Path7":U), System.Drawing.PointF).
        arrayvar0[9] = CAST(resources:GetObject("ultraCarousel1.Path8":U), System.Drawing.PointF).
        arrayvar0[10] = CAST(resources:GetObject("ultraCarousel1.Path9":U), System.Drawing.PointF).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U, Type="System.Byte":U).
        DEFINE VARIABLE arrayvar1 AS INTEGER EXTENT 10 NO-UNDO.
        arrayvar1[1] = System.Convert:ToByte(0).
        arrayvar1[2] = System.Convert:ToByte(3).
        arrayvar1[3] = System.Convert:ToByte(3).
        arrayvar1[4] = System.Convert:ToByte(3).
        arrayvar1[5] = System.Convert:ToByte(3).
        arrayvar1[6] = System.Convert:ToByte(3).
        arrayvar1[7] = System.Convert:ToByte(3).
        arrayvar1[8] = System.Convert:ToByte(3).
        arrayvar1[9] = System.Convert:ToByte(3).
        arrayvar1[10] = System.Convert:ToByte(3).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE carouselPath1 AS Infragistics.Win.UltraWinCarousel.CarouselPath NO-UNDO.
        carouselPath1 = NEW Infragistics.Win.UltraWinCarousel.CarouselPath("EllipseBottom":U, arrayvar0, arrayvar1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraStatusPanel1 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel NO-UNDO.
        ultraStatusPanel1 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusPanel().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraStatusPanel2 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel NO-UNDO.
        ultraStatusPanel2 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusPanel().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraStatusPanel3 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel NO-UNDO.
        ultraStatusPanel3 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusPanel().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraStatusPanel4 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel NO-UNDO.
        ultraStatusPanel4 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusPanel().
        THIS-OBJECT:ultraCarousel1 = NEW Infragistics.Win.UltraWinCarousel.UltraCarousel().
        THIS-OBJECT:imageList1 = NEW System.Windows.Forms.ImageList(THIS-OBJECT:components).
        THIS-OBJECT:ultraStatusBar1 = NEW Infragistics.Win.UltraWinStatusBar.UltraStatusBar().
        CAST(THIS-OBJECT:ultraCarousel1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraStatusBar1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraCarousel1 */
        /*  */
        appearance1:BackColor = System.Drawing.Color:Navy.
        THIS-OBJECT:ultraCarousel1:Appearance = appearance1.
        THIS-OBJECT:ultraCarousel1:Dock = System.Windows.Forms.DockStyle:Fill.
        carouselItem1:Text = "00000":U.
        carouselItem2:Text = "1111":U.
        carouselItem3:Text = "2222":U.
        carouselItem4:Text = "333":U.
        carouselItem5:Text = "4444":U.
        carouselItem6:Text = "5555":U.
        carouselItem7:Text = "666":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinCarousel.CarouselItem EXTENT 7 NO-UNDO.
        arrayvar2[1] = carouselItem1.
        arrayvar2[2] = carouselItem2.
        arrayvar2[3] = carouselItem3.
        arrayvar2[4] = carouselItem4.
        arrayvar2[5] = carouselItem5.
        arrayvar2[6] = carouselItem6.
        arrayvar2[7] = carouselItem7.
        THIS-OBJECT:ultraCarousel1:Items:AddRange(arrayvar2).
        appearance2:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(192)), System.Convert:ToInt32(System.Convert:ToByte(192)), System.Convert:ToInt32(System.Convert:ToByte(255))).
        appearance2:TextHAlignAsString = "Center":U.
        appearance2:TextVAlignAsString = "Bottom":U.
        THIS-OBJECT:ultraCarousel1:ItemSettings:Appearance = appearance2.
        THIS-OBJECT:ultraCarousel1:ItemSettings:ImageSize = NEW System.Drawing.Size(192, 192).
        THIS-OBJECT:ultraCarousel1:ItemSize = NEW System.Drawing.Size(224, 224).
        THIS-OBJECT:ultraCarousel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraCarousel1:Name = "ultraCarousel1":U.
        carouselPath1:Name = "EllipseBottom":U.
        carouselPath1:OpacityStops:Add(NEW Infragistics.Win.UltraWinCarousel.PathOpacityStop(Progress.Util.CastUtil:ToSingle(0), Progress.Util.CastUtil:ToSingle(0.5))).
        carouselPath1:OpacityStops:Add(NEW Infragistics.Win.UltraWinCarousel.PathOpacityStop(Progress.Util.CastUtil:ToSingle(0.5), Progress.Util.CastUtil:ToSingle(1))).
        carouselPath1:OpacityStops:Add(NEW Infragistics.Win.UltraWinCarousel.PathOpacityStop(Progress.Util.CastUtil:ToSingle(1), Progress.Util.CastUtil:ToSingle(0.5))).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS System.Drawing.PointF EXTENT 10 NO-UNDO.
        arrayvar3[1] = CAST(resources:GetObject("resource.Points":U), System.Drawing.PointF).
        arrayvar3[2] = CAST(resources:GetObject("resource.Points1":U), System.Drawing.PointF).
        arrayvar3[3] = CAST(resources:GetObject("resource.Points2":U), System.Drawing.PointF).
        arrayvar3[4] = CAST(resources:GetObject("resource.Points3":U), System.Drawing.PointF).
        arrayvar3[5] = CAST(resources:GetObject("resource.Points4":U), System.Drawing.PointF).
        arrayvar3[6] = CAST(resources:GetObject("resource.Points5":U), System.Drawing.PointF).
        arrayvar3[7] = CAST(resources:GetObject("resource.Points6":U), System.Drawing.PointF).
        arrayvar3[8] = CAST(resources:GetObject("resource.Points7":U), System.Drawing.PointF).
        arrayvar3[9] = CAST(resources:GetObject("resource.Points8":U), System.Drawing.PointF).
        arrayvar3[10] = CAST(resources:GetObject("resource.Points9":U), System.Drawing.PointF).
        carouselPath1:PathData:Points = arrayvar3.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U, Type="System.Byte":U).
        DEFINE VARIABLE arrayvar4 AS INTEGER EXTENT 10 NO-UNDO.
        arrayvar4[1] = System.Convert:ToByte(0).
        arrayvar4[2] = System.Convert:ToByte(3).
        arrayvar4[3] = System.Convert:ToByte(3).
        arrayvar4[4] = System.Convert:ToByte(3).
        arrayvar4[5] = System.Convert:ToByte(3).
        arrayvar4[6] = System.Convert:ToByte(3).
        arrayvar4[7] = System.Convert:ToByte(3).
        arrayvar4[8] = System.Convert:ToByte(3).
        arrayvar4[9] = System.Convert:ToByte(3).
        arrayvar4[10] = System.Convert:ToByte(3).
        carouselPath1:PathData:Types = arrayvar4.
        carouselPath1:ScalingStops:Add(NEW Infragistics.Win.UltraWinCarousel.PathScalingStop(Progress.Util.CastUtil:ToSingle(0), Progress.Util.CastUtil:ToSingle(0.5))).
        carouselPath1:ScalingStops:Add(NEW Infragistics.Win.UltraWinCarousel.PathScalingStop(Progress.Util.CastUtil:ToSingle(0.5), Progress.Util.CastUtil:ToSingle(1))).
        carouselPath1:ScalingStops:Add(NEW Infragistics.Win.UltraWinCarousel.PathScalingStop(Progress.Util.CastUtil:ToSingle(1), Progress.Util.CastUtil:ToSingle(0.5))).
        THIS-OBJECT:ultraCarousel1:Path = carouselPath1.
        THIS-OBJECT:ultraCarousel1:ShowPath = FALSE.
        THIS-OBJECT:ultraCarousel1:Size = NEW System.Drawing.Size(629, 288).
        THIS-OBJECT:ultraCarousel1:TabIndex = 0.
        THIS-OBJECT:ultraCarousel1:Text = "ultraCarousel1":U.
        THIS-OBJECT:ultraCarousel1:AnimationCompleted:Subscribe(THIS-OBJECT:ultraCarousel1_AnimationCompleted).
        THIS-OBJECT:ultraCarousel1:ItemClick:Subscribe(THIS-OBJECT:ultraCarousel1_ItemClick).
        THIS-OBJECT:ultraCarousel1:DoubleClick:Subscribe(THIS-OBJECT:ultraCarousel1_DoubleClick).
        /*  */
        /* imageList1 */
        /*  */
        THIS-OBJECT:imageList1:ColorDepth = System.Windows.Forms.ColorDepth:Depth8Bit.
        THIS-OBJECT:imageList1:ImageSize = NEW System.Drawing.Size(16, 16).
        THIS-OBJECT:imageList1:TransparentColor = System.Drawing.Color:Transparent.
        /*  */
        /* ultraStatusBar1 */
        /*  */
        THIS-OBJECT:ultraStatusBar1:Location = NEW System.Drawing.Point(0, 288).
        THIS-OBJECT:ultraStatusBar1:Name = "ultraStatusBar1":U.
        ultraStatusPanel1:Key = "activeitem":U.
        ultraStatusPanel1:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Spring.
        ultraStatusPanel1:WrapText = Infragistics.Win.DefaultableBoolean:False.
        ultraStatusPanel2:Key = "frameworkversion":U.
        ultraStatusPanel2:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Automatic.
        ultraStatusPanel2:WrapText = Infragistics.Win.DefaultableBoolean:False.
        ultraStatusPanel3:Key = "proversion":U.
        ultraStatusPanel3:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Automatic.
        ultraStatusPanel3:WrapText = Infragistics.Win.DefaultableBoolean:False.
        ultraStatusPanel4:Key = "bitness":U.
        ultraStatusPanel4:SizingMode = Infragistics.Win.UltraWinStatusBar.PanelSizingMode:Automatic.
        ultraStatusPanel4:WrapText = Infragistics.Win.DefaultableBoolean:False.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinStatusBar.UltraStatusPanel EXTENT 4 NO-UNDO.
        arrayvar5[1] = ultraStatusPanel1.
        arrayvar5[2] = ultraStatusPanel2.
        arrayvar5[3] = ultraStatusPanel3.
        arrayvar5[4] = ultraStatusPanel4.
        THIS-OBJECT:ultraStatusBar1:Panels:AddRange(arrayvar5).
        THIS-OBJECT:ultraStatusBar1:Size = NEW System.Drawing.Size(629, 23).
        THIS-OBJECT:ultraStatusBar1:TabIndex = 1.
        THIS-OBJECT:ultraStatusBar1:Text = "ultraStatusBar1":U.
        THIS-OBJECT:ultraStatusBar1:ViewStyle = Infragistics.Win.UltraWinStatusBar.ViewStyle:Office2007.
        /*  */
        /* DesktopForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(629, 311).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraCarousel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraStatusBar1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:MaximizeBox = FALSE.
        THIS-OBJECT:MaximumSize = NEW System.Drawing.Size(1024, 350).
        THIS-OBJECT:Name = "DesktopForm":U.
        THIS-OBJECT:Text = "SmartComponent Library Desktop":U.
        THIS-OBJECT:WindowPositionRegistryKey = "DesktopForm":U.
        CAST(THIS-OBJECT:ultraCarousel1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraStatusBar1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Launches the given special action
        Notes:
        @param pcAction The action to launch
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID LaunchSpecialAction (pcAction AS CHARACTER):

        DEFINE VARIABLE cPath      AS CHARACTER                  NO-UNDO.
        DEFINE VARIABLE oProcess   AS System.Diagnostics.Process NO-UNDO .
        DEFINE VARIABLE oStartInfo AS ProcessStartInfo           NO-UNDO .

        IF pcAction BEGINS "explorer":U THEN DO:
            IF INDEX (pcAction, ":":U) > 0 THEN
                ASSIGN cPath = SUBSTRING (pcAction, INDEX (pcAction, ":":U) + 1) .
            ELSE cPath = ".":U  .

            FILE-INFORMATION:FILE-NAME = cPath .

            IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
                WindowsExplorerHelper:ShowFolder (FILE-INFORMATION:FULL-PATHNAME) .
        END.
        ELSE IF pcAction BEGINS "proenv":U THEN DO:

            oStartInfo = NEW ProcessStartInfo () .
            oStartInfo:FileName = "cmd.exe":U .
            oStartInfo:Arguments = SUBSTITUTE ("/K &1~\bin~\proenv.bat":U,
                                               SessionHelper:DLC) .

            IF INDEX (pcAction, ":":U) > 0 THEN
                ASSIGN cPath = SUBSTRING (pcAction, INDEX (pcAction, ":":U) + 1) .
            ELSE cPath = ".":U  .

            FILE-INFORMATION:FILE-NAME = cPath .

            IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
                oStartInfo:WorkingDirectory = FILE-INFORMATION:FULL-PATHNAME .

            oProcess = System.Diagnostics.Process:Start (oStartInfo) .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event
        Notes:
        @param e The System.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):

        SUPER:OnLoad (e) .

        THIS-OBJECT:CreateCarouselItems () .

        THIS-OBJECT:Text = SUBSTITUTE ("&1 - &2":U,
                                       THIS-OBJECT:Text,
                                       SessionHelper:CurrentDirectory()) .

        CATCH err AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):

        SUPER:OnShown (e) .

        THIS-OBJECT:DisplayCurrentItemText() .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Launches a new AVM with the given startup parameters
        Notes:
        @param pcProcedureName The name of the startup procedure
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID RunProcedure (pcProcedureName AS CHARACTER):

        DEFINE VARIABLE oStartInfo AS System.Diagnostics.ProcessStartInfo NO-UNDO .

        oStartInfo                  = NEW System.Diagnostics.ProcessStartInfo () .
        oStartInfo:FileName         = Consultingwerk.Util.SessionHelper:ExecutablePath() .
        oStartInfo:Arguments        = Consultingwerk.Util.SessionHelper:StartupParameters() +
                                       SUBSTITUTE (" -p ~"&1~"":U, pcProcedureName).
        oStartInfo:WorkingDirectory = Consultingwerk.Util.SessionHelper:CurrentDirectory() .

        System.Diagnostics.Process:Start (oStartInfo) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the AnimationCompleted event of the ultraCarousel1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The AnimationCompletedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraCarousel1_AnimationCompleted (sender AS System.Object,
                                                           e AS AnimationCompletedEventArgs):

        THIS-OBJECT:DisplayCurrentItemText() .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the DoubleClick event of the ultraCarousel1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraCarousel1_DoubleClick (INPUT sender AS System.Object, INPUT e AS System.EventArgs):

        FIND ttDesktopItems WHERE ROWID (ttDesktopItems) = TO-ROWID (THIS-OBJECT:ultraCarousel1:ActiveItem:Key) NO-ERROR .

        IF NOT AVAILABLE ttDesktopItems THEN DO:
            ErrorHelper:ResetErrorStatus() .
            RETURN .
        END.

        IF ttDesktopItems.RunProcedure > "":U THEN
            THIS-OBJECT:RunProcedure (ttDesktopItems.RunProcedure) .
         IF ttDesktopItems.OpenWebSite > "":U THEN
             Win32:ShellExecute ("open":U, ttDesktopItems.OpenWebSite) .
        ELSE IF ttDesktopItems.SpecialAction > "":U THEN
            THIS-OBJECT:LaunchSpecialAction (ttDesktopItems.SpecialAction) .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ItemClick event of the ultraCarousel1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The CarouselItemClickEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraCarousel1_ItemClick (sender AS System.Object,
                                                  e AS CarouselItemClickEventArgs):

        e:ScrollItemToForefront = TRUE .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the DesktopForm class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC DesktopForm ():

    END DESTRUCTOR .
&ENDIF

END CLASS.
