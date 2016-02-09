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
    File        : DatePickerControl
    Purpose     : Control that can be used as a PopupEditor or DATE FILL-IN's
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Oct 05 15:36:05 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW . 

USING Progress.Lang.*.
USING Progress.Windows.UserControl.

CLASS Consultingwerk.WindowIntegrationKit.Controls.DatePickerControl 
    INHERITS UserControl: 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE TodayButton AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton1 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraCalendarInfo1 AS Infragistics.Win.UltraWinSchedule.UltraCalendarInfo NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraPanel1 AS Infragistics.Win.Misc.UltraPanel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraMonthViewMulti1 AS Infragistics.Win.UltraWinSchedule.UltraMonthViewMulti NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraCalendarLook1 AS Infragistics.Win.UltraWinSchedule.UltraCalendarLook NO-UNDO.

    DEFINE VARIABLE lSettingDate AS LOGICAL NO-UNDO.    
        
    /*------------------------------------------------------------------------------
        Purpose: Raised when the Date is changed                                                                        
        Notes:      
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT DateChanged DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the associated DATE                                                                         
        Notes:      
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Date AS DATE NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the associated WIDGET-HANDLE                                                                        
        Notes:      
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY WidgetHandle AS HANDLE NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DatePickerControl class                                                                        
        Notes:      
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DatePickerControl ():
        
        SUPER().
        InitializeComponent().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

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
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:ultraMonthViewMulti1 = NEW Infragistics.Win.UltraWinSchedule.UltraMonthViewMulti().
        THIS-OBJECT:ultraCalendarInfo1 = NEW Infragistics.Win.UltraWinSchedule.UltraCalendarInfo(THIS-OBJECT:components).
        THIS-OBJECT:ultraCalendarLook1 = NEW Infragistics.Win.UltraWinSchedule.UltraCalendarLook(THIS-OBJECT:components).
        THIS-OBJECT:ultraPanel1 = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:ultraButton1 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:TodayButton = NEW Infragistics.Win.Misc.UltraButton().
        CAST(THIS-OBJECT:ultraMonthViewMulti1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraPanel1:ClientArea:SuspendLayout().
        THIS-OBJECT:ultraPanel1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraMonthViewMulti1 */
        /*  */
        THIS-OBJECT:ultraMonthViewMulti1:AllowMonthSelection = FALSE.
        THIS-OBJECT:ultraMonthViewMulti1:AllowWeekSelection = FALSE.
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraMonthViewMulti1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraMonthViewMulti1:BackColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:ultraMonthViewMulti1:CalendarInfo = THIS-OBJECT:ultraCalendarInfo1.
        THIS-OBJECT:ultraMonthViewMulti1:CalendarLook = THIS-OBJECT:ultraCalendarLook1.
        THIS-OBJECT:ultraMonthViewMulti1:DayOfWeekCaptionStyle = Infragistics.Win.UltraWinSchedule.DayOfWeekCaptionStyle:ShortDescription.
        THIS-OBJECT:ultraMonthViewMulti1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraMonthViewMulti1:Name = "ultraMonthViewMulti1":U.
        THIS-OBJECT:ultraMonthViewMulti1:ResizeMode = Infragistics.Win.UltraWinSchedule.ResizeMode:UseMonthDimensions.
        THIS-OBJECT:ultraMonthViewMulti1:Size = NEW System.Drawing.Size(227, 154).
        THIS-OBJECT:ultraMonthViewMulti1:TabIndex = 0.
        THIS-OBJECT:ultraMonthViewMulti1:WeekNumbersVisible = TRUE.
        /*  */
        /* ultraCalendarInfo1 */
        /*  */
        THIS-OBJECT:ultraCalendarInfo1:DataBindingsForAppointments:BindingContextControl = THIS-OBJECT.
        THIS-OBJECT:ultraCalendarInfo1:DataBindingsForOwners:BindingContextControl = THIS-OBJECT.
        THIS-OBJECT:ultraCalendarInfo1:AfterSelectedDateRangeChange:Subscribe(THIS-OBJECT:ultraCalendarInfo1_AfterSelectedDateRangeChange).
        /*  */
        /* ultraCalendarLook1 */
        /*  */
        appearance2:BackColor = System.Drawing.SystemColors:GradientActiveCaption.
        appearance2:FontData:BoldAsString = "True":U.
        THIS-OBJECT:ultraCalendarLook1:TodayAppearance = appearance2.
        THIS-OBJECT:ultraCalendarLook1:ViewStyle = Infragistics.Win.UltraWinSchedule.ViewStyle:Office2007.
        /*  */
        /* ultraPanel1 */
        /*  */
        appearance1:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:ultraPanel1:Appearance = appearance1.
        /*  */
        /* ultraPanel1.ClientArea */
        /*  */
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ultraButton1).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:TodayButton).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ultraMonthViewMulti1).
        THIS-OBJECT:ultraPanel1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraPanel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraPanel1:Name = "ultraPanel1":U.
        THIS-OBJECT:ultraPanel1:Size = NEW System.Drawing.Size(227, 188).
        THIS-OBJECT:ultraPanel1:TabIndex = 1.
        /*  */
        /* ultraButton1 */
        /*  */
        THIS-OBJECT:ultraButton1:ButtonStyle = Infragistics.Win.UIElementButtonStyle:WindowsVistaButton.
        THIS-OBJECT:ultraButton1:Location = NEW System.Drawing.Point(117, 160).
        THIS-OBJECT:ultraButton1:Name = "ultraButton1":U.
        THIS-OBJECT:ultraButton1:Size = NEW System.Drawing.Size(94, 25).
        THIS-OBJECT:ultraButton1:TabIndex = 2.
        THIS-OBJECT:ultraButton1:Text = "No Date":U.
        THIS-OBJECT:ultraButton1:Click:Subscribe(THIS-OBJECT:NoDateButton_Click).
        /*  */
        /* TodayButton */
        /*  */
        THIS-OBJECT:TodayButton:ButtonStyle = Infragistics.Win.UIElementButtonStyle:WindowsVistaButton.
        THIS-OBJECT:TodayButton:Location = NEW System.Drawing.Point(16, 160).
        THIS-OBJECT:TodayButton:Name = "TodayButton":U.
        THIS-OBJECT:TodayButton:Size = NEW System.Drawing.Size(94, 25).
        THIS-OBJECT:TodayButton:TabIndex = 1.
        THIS-OBJECT:TodayButton:Text = "Today":U.
        THIS-OBJECT:TodayButton:Click:Subscribe(THIS-OBJECT:TodayButton_Click).
        /*  */
        /* DatePickerControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(6, 13).
        THIS-OBJECT:BorderStyle = System.Windows.Forms.BorderStyle:FixedSingle.
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraPanel1).
        THIS-OBJECT:Name = "DatePickerControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(227, 188).
        CAST(THIS-OBJECT:ultraMonthViewMulti1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraPanel1:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:ultraPanel1:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.


    /*------------------------------------------------------------------------------
        Purpose: Raises the DataChanged event                                                                       
        Notes:                               
        @param e The System.EventArgs with the data for this event                                         
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnDateChanged (e AS System.EventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            e = System.EventArgs:EMPTY .
            
        THIS-OBJECT:DateChanged:Publish (THIS-OBJECT, e) .

        CATCH err AS Progress.Lang.Error :
            /* Ignore errors */    
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event                                                                       
        Notes:
        @param e The System.EventArgs with the data for this event                                         
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):
        
        SUPER:OnLoad (e) .
        
        IF THIS-OBJECT:DATE <> ? THEN DO:
            ultraCalendarInfo1:SelectedDateRanges:Clear () .
            ultraCalendarInfo1:SelectedDateRanges:Add (THIS-OBJECT:Date) .
            
            /* Mike Fechner, Consultingwerk Ltd. 02.08.2011
               The method ActivateDay was introduced in Infragistics
               2009.2, workaround with 2009.1:
               ultraCalendarInfo1:ActiveDay = ultraCalendarInfo1:GetDay (THIS-OBJECT:Date, TRUE) . */
            ultraCalendarInfo1:ActivateDay (THIS-OBJECT:Date) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the TodayButton                                                                       
        Notes:                                                  
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event                      
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID TodayButton_Click (sender AS System.Object, 
                                           e AS System.EventArgs):
        
        THIS-OBJECT:DATE = TODAY . 
        OnDateChanged (System.EventArgs:Empty) .

    END METHOD.


    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the NoDateButton                                                                       
        Notes:                                                  
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event                      
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID NoDateButton_Click (sender AS System.Object, 
                                            e AS System.EventArgs):
        
      THIS-OBJECT:DATE = ? . 
      OnDateChanged (System.EventArgs:Empty) .


    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the AfterSelectedDateRangeChange event of the ultraCalendarInfo1                                                                       
        Notes:                                                  
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event                      
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraCalendarInfo1_AfterSelectedDateRangeChange (sender AS System.Object, 
                                                                         e AS System.EventArgs):
        
        IF lSettingDate THEN 
            RETURN . 
        
        IF ultraCalendarInfo1:SelectedDateRanges:Count > 0 THEN 
            THIS-OBJECT:DATE = ultraCalendarInfo1:SelectedDateRanges[0]:FirstDay:Date .
            
        OnDateChanged (System.EventArgs:Empty) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor of the DataPickerControl class                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC DatePickerControl ():

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
