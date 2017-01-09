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
    File        : SmartGroupViewer
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Fri Oct 12 18:09:27 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.SmartViewerControl FROM PROPATH .
USING Consultingwerk.Windows.Framework.Authorization.*       FROM PROPATH .  
USING Progress.Lang.*                                        FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Authorization.SmartGroupViewer 
    INHERITS SmartViewerControl: 
    
    DEFINE PRIVATE VARIABLE components                                                           AS System.ComponentModel.IContainer                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eSmartGroup_LoginCompanyGuid_Label AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityLookup1                                           AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup        NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityLookup2                                           AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup        NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eSmartGroup_ParentGroupGuid_Label  AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eSmartGroup_GroupName_Label        AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eSmartGroup_GroupName              AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1                                    AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraPanel2                                                          AS Infragistics.Win.Misc.UltraPanel                                               NO-UNDO.

    CONSTRUCTOR PUBLIC SmartGroupViewer ():
        
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
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartGroup":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Authorization.SmartGroupViewer":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ClearValue":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton2 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton2 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton3 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton3 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ClearValue":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton4 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton4 = NEW Infragistics.Win.UltraWinEditors.EditorButton("ShowLookupDialog":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartGroup_GroupName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartGroup_GroupName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartGroup_ParentGroupGuid_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraPanel2 = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartGroup_LoginCompanyGuid_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityLookup2 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        THIS-OBJECT:smartBusinessEntityLookup1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartGroup_GroupName, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraPanel2:ClientArea:SuspendLayout().
        THIS-OBJECT:ultraPanel2:SuspendLayout().
        CAST(THIS-OBJECT:smartBusinessEntityLookup2, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.Authorization.GroupBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartGroup":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 6 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("GroupGuid":U, "GroupGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("GroupName":U, "GroupName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("ParentGroupGuid":U, "ParentGroupGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("LoginCompanyGuid":U, "LoginCompanyGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("ParentGroupName":U, "Parent Group Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("LoginCompanyName":U, "LoginCompanyName":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartGroup_GroupName_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartGroup_GroupName_Label:Appearance = appearance1.
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartGroup_GroupName_Label, "smartBusinessEntityBindingSource1_eSmartGroup_GroupName_Label":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartGroup_GroupName_Label:Name = "smartBusinessEntityBindingSource1_eSmartGroup_GroupName_Label":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartGroup_GroupName */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartGroup_GroupName, "smartBusinessEntityBindingSource1_eSmartGroup_GroupName":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartGroup_GroupName:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "GroupName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartGroup_GroupName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartGroup_GroupName:Name = "smartBusinessEntityBindingSource1_eSmartGroup_GroupName":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartGroup_ParentGroupGuid_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartGroup_ParentGroupGuid_Label:Appearance = appearance2.
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartGroup_ParentGroupGuid_Label, "smartBusinessEntityBindingSource1_eSmartGroup_ParentGroupGuid_Label":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartGroup_ParentGroupGuid_Label:Name = "smartBusinessEntityBindingSource1_eSmartGroup_ParentGroupGuid_Label":U.
        /*  */
        /* ultraPanel2 */
        /*  */
        /*  */
        /* ultraPanel2.ClientArea */
        /*  */
        THIS-OBJECT:ultraPanel2:ClientArea:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartGroup_GroupName).
        THIS-OBJECT:ultraPanel2:ClientArea:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartGroup_LoginCompanyGuid_Label).
        THIS-OBJECT:ultraPanel2:ClientArea:Controls:Add(THIS-OBJECT:smartBusinessEntityLookup2).
        THIS-OBJECT:ultraPanel2:ClientArea:Controls:Add(THIS-OBJECT:smartBusinessEntityLookup1).
        THIS-OBJECT:ultraPanel2:ClientArea:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartGroup_ParentGroupGuid_Label).
        THIS-OBJECT:ultraPanel2:ClientArea:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartGroup_GroupName_Label).
        resources:ApplyResources(THIS-OBJECT:ultraPanel2, "ultraPanel2":U).
        THIS-OBJECT:ultraPanel2:Name = "ultraPanel2":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eSmartGroup_LoginCompanyGuid_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartGroup_LoginCompanyGuid_Label:Appearance = appearance3.
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartGroup_LoginCompanyGuid_Label, "smartBusinessEntityBindingSource1_eSmartGroup_LoginCompanyGuid_Label":U).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartGroup_LoginCompanyGuid_Label:Name = "smartBusinessEntityBindingSource1_eSmartGroup_LoginCompanyGuid_Label":U.
        /*  */
        /* smartBusinessEntityLookup2 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityLookup2, "smartBusinessEntityLookup2":U).
        appearance4:Image = CAST(resources:GetObject("appearance4.Image":U), System.Object).
        appearance4:ImageHAlign = Infragistics.Win.HAlign:Center.
        editorButton1:Appearance = appearance4.
        editorButton1:Key = "ClearValue":U.
        THIS-OBJECT:smartBusinessEntityLookup2:ButtonsLeft:Add(editorButton1).
        editorButton2:Key = "ShowLookupDialog":U.
        resources:ApplyResources(editorButton2, "editorButton2":U).
        THIS-OBJECT:smartBusinessEntityLookup2:ButtonsRight:Add(editorButton2).
        THIS-OBJECT:smartBusinessEntityLookup2:DataBindings:Add(NEW System.Windows.Forms.Binding("LookupKeyValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "LoginCompanyGuid":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup2:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "LoginCompanyName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup2:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupAdditionalFields = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupBrowserExcludeFields = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupBrowserFields = "LoginCompanyName":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupCascade = FALSE.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupControls = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogClassName = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogFilterFields = "LoginCompanyName":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogFilterOperators = "BEGINS":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogOpenQuery = TRUE.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogQuerySort = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupDialogQueryString = "FOR EACH eSmartLoginCompany":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupEntityName = "Consultingwerk.SmartFramework.System.LoginCompanyBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupEntityTable = "eSmartLoginCompany":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupEntityView = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupFields = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupKeyField = "LoginCompanyName":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupKeyValue = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupKeyValueColumn = "LoginCompanyGuid":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupQuerySort = "":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupQueryString = "FOR EACH eSmartLoginCompany WHERE eSmartLoginCompany.LoginCompanyName BEGINS ~"&1~"":U.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupTimerDelay = 750.
        THIS-OBJECT:smartBusinessEntityLookup2:LookupTimerDisabled = FALSE.
        THIS-OBJECT:smartBusinessEntityLookup2:Name = "smartBusinessEntityLookup2":U.
        THIS-OBJECT:smartBusinessEntityLookup2:OwningGrid = ?.
        THIS-OBJECT:smartBusinessEntityLookup2:EditorButtonClick:Subscribe(THIS-OBJECT:LoginCompanyLookup_EditorButtonClick).
        THIS-OBJECT:smartBusinessEntityLookup2:TextChanged:Subscribe(THIS-OBJECT:LoginCompanyLookup_TextChanged).
        /*  */
        /* smartBusinessEntityLookup1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:smartBusinessEntityLookup1, "smartBusinessEntityLookup1":U).
        appearance5:Image = CAST(resources:GetObject("appearance5.Image":U), System.Object).
        appearance5:ImageHAlign = Infragistics.Win.HAlign:Center.
        editorButton3:Appearance = appearance5.
        editorButton3:Key = "ClearValue":U.
        THIS-OBJECT:smartBusinessEntityLookup1:ButtonsLeft:Add(editorButton3).
        editorButton4:Key = "ShowLookupDialog":U.
        resources:ApplyResources(editorButton4, "editorButton4":U).
        THIS-OBJECT:smartBusinessEntityLookup1:ButtonsRight:Add(editorButton4).
        THIS-OBJECT:smartBusinessEntityLookup1:DataBindings:Add(NEW System.Windows.Forms.Binding("LookupKeyValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ParentGroupGuid":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup1:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ParentGroupName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityLookup1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupAdditionalFields = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupBrowserExcludeFields = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupBrowserFields = "GroupName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupCascade = FALSE.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupControls = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogClassName = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogFilterFields = "GroupName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogFilterOperators = "BEGINS":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogOpenQuery = TRUE.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogQuerySort = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupDialogQueryString = "FOR EACH eSmartGroup":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityName = "Consultingwerk.SmartFramework.Authorization.GroupBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityTable = "eSmartGroup":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupEntityView = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupFields = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyField = "GroupName":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyValue = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupKeyValueColumn = "GroupGuid":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupQuerySort = "":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupQueryString = "FOR EACH eSmartGroup WHERE eSmartGroup.GroupName BEGINS ~"&1~"":U.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupTimerDelay = 750.
        THIS-OBJECT:smartBusinessEntityLookup1:LookupTimerDisabled = FALSE.
        THIS-OBJECT:smartBusinessEntityLookup1:Name = "smartBusinessEntityLookup1":U.
        THIS-OBJECT:smartBusinessEntityLookup1:OwningGrid = ?.
        THIS-OBJECT:smartBusinessEntityLookup1:EditorButtonClick:Subscribe(THIS-OBJECT:smartBusinessEntityLookup1_EditorButtonClick).
        THIS-OBJECT:smartBusinessEntityLookup1:TextChanged:Subscribe(THIS-OBJECT:smartBusinessEntityLookup1_TextChanged).
        /*  */
        /* SmartGroupViewer */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraPanel2).
        THIS-OBJECT:Name = "SmartGroupViewer":U.
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eSmartGroup_GroupName, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraPanel2:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:ultraPanel2:ClientArea:PerformLayout().
        THIS-OBJECT:ultraPanel2:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:smartBusinessEntityLookup2, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityLookup1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: 
        Notes:   
        @param sender Reference to the Object which is calling the method
        @param e An EventArgs that contains the Event specific data
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID LoginCompanyLookup_EditorButtonClick (sender AS System.Object, 
                                                              e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):
        CASE e:Button:Key:
            WHEN "ClearValue":U THEN DO:
                smartBusinessEntityLookup2:Text           = "":U.
                smartBusinessEntityLookup2:LookupKeyValue = "":U.
            END.
        END CASE.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Set LookupKeyValue to be able to erase the selected LoginCompany
        Notes:   
        @param sender Reference to the Object which is calling the method
        @param e An EventArgs that contains no Event data
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID LoginCompanyLookup_TextChanged (sender AS System.Object, 
                                                        e      AS System.EventArgs):
        
        IF smartBusinessEntityLookup2:Text = "":U THEN 
            smartBusinessEntityLookup2:LookupKeyValue = "":U.
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: 
        Notes:   
        @param sender Reference to the Object which is calling the method
        @param e An EventArgs that contains the Event specific data
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartBusinessEntityLookup1_EditorButtonClick (sender AS System.Object, 
                                                                      e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):
        
        CASE e:Button:Key:
            WHEN "ClearValue":U THEN DO:
                smartBusinessEntityLookup1:Text           = "":U.
                smartBusinessEntityLookup1:LookupKeyValue = "":U.
            END.
        END CASE.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Set LookupKeyValue to be able to erase the selected LoginCompany
        Notes:   
        @param sender Reference to the Object which is calling the method
        @param e An EventArgs that contains no Event data
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID smartBusinessEntityLookup1_TextChanged (sender AS System.Object, 
                                                                e AS System.EventArgs):
        
        IF smartBusinessEntityLookup1:Text = "":U THEN 
            smartBusinessEntityLookup1:LookupKeyValue = "":U.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartGroupViewer class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartGroupViewer ():

    END DESTRUCTOR .

END CLASS.
