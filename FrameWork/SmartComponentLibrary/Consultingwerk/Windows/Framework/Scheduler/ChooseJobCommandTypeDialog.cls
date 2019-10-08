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
    File        : ChooseJobCommandTypeDialog
    Purpose     : Dialog to select a Scheduler's Job Type
    Syntax      : 
    Description : 
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Thu May 26 14:00:39 CEST 2016
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Forms.*                       FROM PROPATH .
USING Consultingwerk.Windows.Framework.Scheduler.* FROM PROPATH .  
USING Progress.Lang.*                              FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Scheduler.ChooseJobCommandTypeDialog    
    INHERITS BaseForm: 
    
    DEFINE PRIVATE VARIABLE components        AS System.ComponentModel.IContainer                  NO-UNDO.
    DEFINE PRIVATE VARIABLE JobTypeCombo     AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE pictureBox1 AS System.Windows.Forms.PictureBox NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButtonOK     AS Infragistics.Win.Misc.UltraButton                 NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButtonCancel AS Infragistics.Win.Misc.UltraButton                 NO-UNDO.

    DEFINE VARIABLE oValueList AS Infragistics.Win.ValueList NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButtonRefreshList AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Represents the Selected CallType
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SelectedJobCommandTypeValue AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ChooseFunctionCallTypeDialog class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ChooseJobCommandTypeDialog ():
        SUPER ().
        
        InitializeComponent ().
        
        THIS-OBJECT:StoreFormPosition = FALSE . 
        THIS-OBJECT:MinimumSize = THIS-OBJECT:Size .
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Assignes the selected Value to the SelectedCallTypeValue property
        Notes:   
        @param sender The publisher of the ValueChanged event
        @param e An EventArgs instance which has no event data
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID JobTypeCombo_ValueChanged (sender AS System.Object, 
                                                    e      AS System.EventArgs):
        
        SelectedJobCommandTypeValue = UNBOX (JobTypeCombo:SelectedItem:DataValue).

        ultraButtonOK:ENABLED = TRUE . 

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
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Scheduler.ChooseJobCommandTypeDialog":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:JobTypeCombo = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:ultraButtonCancel = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraButtonOK = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:pictureBox1 = NEW System.Windows.Forms.PictureBox().
        THIS-OBJECT:ultraButtonRefreshList = NEW Infragistics.Win.Misc.UltraButton().
        CAST(THIS-OBJECT:JobTypeCombo, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* JobTypeCombo */
        /*  */
        resources:ApplyResources(THIS-OBJECT:JobTypeCombo, "JobTypeCombo":U).
        THIS-OBJECT:JobTypeCombo:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:JobTypeCombo:Name = "JobTypeCombo":U.
        THIS-OBJECT:JobTypeCombo:ValueChanged:Subscribe(THIS-OBJECT:JobTypeCombo_ValueChanged).
        /*  */
        /* ultraButtonCancel */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraButtonCancel, "ultraButtonCancel":U).
        THIS-OBJECT:ultraButtonCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:ultraButtonCancel:Name = "ultraButtonCancel":U.
        /*  */
        /* ultraButtonOK */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraButtonOK, "ultraButtonOK":U).
        THIS-OBJECT:ultraButtonOK:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:ultraButtonOK:Name = "ultraButtonOK":U.
        /*  */
        /* ultraLabel1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraLabel1, "ultraLabel1":U).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        /*  */
        /* pictureBox1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:pictureBox1, "pictureBox1":U).
        THIS-OBJECT:pictureBox1:Name = "pictureBox1":U.
        THIS-OBJECT:pictureBox1:TabStop = FALSE.
        /*  */
        /* ultraButtonRefreshList */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraButtonRefreshList, "ultraButtonRefreshList":U).
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        THIS-OBJECT:ultraButtonRefreshList:Appearance = appearance1.
        THIS-OBJECT:ultraButtonRefreshList:ImageSize = NEW System.Drawing.Size(13, 13).
        THIS-OBJECT:ultraButtonRefreshList:Name = "ultraButtonRefreshList":U.
        THIS-OBJECT:ultraButtonRefreshList:Click:Subscribe(THIS-OBJECT:ultraButtonRefreshList_Click).
        /*  */
        /* ChooseJobCommandTypeDialog */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:ultraButtonOK.
        THIS-OBJECT:CancelButton = THIS-OBJECT:ultraButtonCancel.
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButtonRefreshList).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:pictureBox1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButtonOK).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButtonCancel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:JobTypeCombo).
        THIS-OBJECT:MaximizeBox = FALSE.
        THIS-OBJECT:MinimizeBox = FALSE.
        THIS-OBJECT:Name = "ChooseJobCommandTypeDialog":U.
        THIS-OBJECT:ShowIcon = FALSE.
        THIS-OBJECT:ShowInTaskbar = FALSE.
        CAST(THIS-OBJECT:JobTypeCombo, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Execute logic after the form has been shown
        Notes:   
        @param e An EventArgs instance which has no event data
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnShown (INPUT e AS System.EventArgs):
        
        SUPER:OnShown(INPUT e).

        PROCESS EVENTS.
        
        THIS-OBJECT:PopulateJobTypeCombo ().

        /* Marko Rüterbories, Consultingwerk Ltd. 04.12.2012
           Hier wird die Selektion noch nicht am UI angezeigt.
           Warum: ?? */
        IF JobTypeCombo:Items:Count > 0 THEN DO: 
            JobTypeCombo:SelectedIndex = 0. 

            JobTypeCombo:DropDown ().
        END.
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populate the JobTypeCombo with all available CallTypes
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID PopulateJobTypeCombo ():
        
        DEFINE VARIABLE oJobCommandCacheService AS IJobCommandCacheService NO-UNDO .
        
        oJobCommandCacheService = {Consultingwerk/get-service.i IJobCommandCacheService
                                                                "NEW JobCommandCacheService ()"}. 

        JobTypeCombo:ValueList = oJobCommandCacheService:JobCommands.
        
        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Refresh the FunctionCall list
        Notes:   
        @param sender The publisher of the ValueChanged event
        @param e An EventArgs instance which has no event data
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraButtonRefreshList_Click (sender AS System.Object, 
                                                      e AS System.EventArgs):
        
        DEFINE VARIABLE oJobCommandCacheService AS IJobCommandCacheService NO-UNDO .
        
        oJobCommandCacheService = {Consultingwerk/get-service.i IJobCommandCacheService
                                                                "NEW JobCommandCacheService ()"}. 
        oJobCommandCacheService:InitializeService ().
        
        JobTypeCombo:ValueList = oJobCommandCacheService:JobCommands.
        
        IF JobTypeCombo:Items:Count > 0 THEN DO: 
            JobTypeCombo:SelectedIndex = 0. 

            JobTypeCombo:DropDown ().
        END.

        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the ChooseFunctionCallTypeDialog class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC ChooseJobCommandTypeDialog ():

    END DESTRUCTOR .

END CLASS.
