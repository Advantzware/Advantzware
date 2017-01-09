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
    File        : SmartLoginCompanyViewer
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Wed Oct 10 20:34:52 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.SmartViewerControl FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*                  FROM PROPATH .
USING Consultingwerk.Windows.Framework.System.*              FROM PROPATH .  
USING Progress.Lang.*                                        FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.System.SmartLoginCompanyViewer 
    INHERITS SmartViewerControl: 
    
    DEFINE PRIVATE VARIABLE components                                                AS System.ComponentModel.IContainer                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyTenantDomain_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyTenantDomain AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyShort_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyShort AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceInteger_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceInteger AS Infragistics.Win.UltraWinEditors.UltraNumericEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceDecimal_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceDecimal AS Infragistics.Win.UltraWinEditors.UltraNumericEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceChar_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceChar AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageSmall_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageSmall AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageLarge_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageLarge AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE LoginCompanyBindingSource_eSmartLoginCompany_Name_Label   AS Infragistics.Win.Misc.UltraLabel                                               NO-UNDO.
    DEFINE PRIVATE VARIABLE LoginCompanyBindingSource_eSmartLoginCompany_Name         AS Infragistics.Win.UltraWinEditors.UltraTextEditor                               NO-UNDO.
    DEFINE PRIVATE VARIABLE LoginCompanyBindingSource                                 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraGroupBox1 AS Infragistics.Win.Misc.UltraGroupBox NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraPanel1                                               AS Infragistics.Win.Misc.UltraPanel                                               NO-UNDO.

    CONSTRUCTOR PUBLIC SmartLoginCompanyViewer ():
        SUPER ().
        InitializeComponent ().

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Assignes the filename of the droped file to the ImageEditors
        Notes:   
        @param sender The source of the event
        @param e An EventArgs that contains the data for the event 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ImageSelection_DragDrop (sender AS System.Object, 
                                                 e      AS System.Windows.Forms.DragEventArgs):
        DEFINE VARIABLE cFileNames AS CHARACTER NO-UNDO EXTENT.
        
        IF NOT Consultingwerk.Util.DragAndDropHelper:IsFileDrop (e:Data) THEN 
            RETURN. 
        
        cFileNames = Consultingwerk.Util.DragAndDropHelper:GetDropFileNames (e:Data).
        
        IF EXTENT (cFileNames) > 0 THEN 
            CAST (sender, System.Windows.Forms.Control):Text = Consultingwerk.Util.FileHelper:RelativeFileName (cFileNames[1]).

        THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData . 

        /* Tell the SmartDataSource that the user has started to update data. */
        IF VALID-OBJECT (THIS-OBJECT:SmartDataSource) THEN 
            THIS-OBJECT:SmartDataSource:BeginUpdateState (THIS-OBJECT).
                
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Allowes the FileDrop to the editor control if the events data
                 contains exact one file.
        Notes:   
        @param sender The source of the event
        @param e An EventArgs that contains the data for the event 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ImageSelection_DragEnter (sender AS System.Object, 
                                                  e      AS System.Windows.Forms.DragEventArgs):
        DEFINE VARIABLE cFileNames AS CHARACTER NO-UNDO EXTENT.
        
        IF Consultingwerk.Util.DragAndDropHelper:IsFileDrop (e:Data) THEN DO: 
            cFileNames = Consultingwerk.Util.DragAndDropHelper:GetDropFileNames (e:Data).

            IF EXTENT (cFileNames) = 1 THEN 
                e:Effect = System.Windows.Forms.DragDropEffects:Copy.
        END.
        
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
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.System.SmartLoginCompanyViewer":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartLoginCompany":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
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
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:ultraPanel1 = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:ultraGroupBox1 = NEW Infragistics.Win.Misc.UltraGroupBox().
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceChar = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:LoginCompanyBindingSource = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceInteger = NEW Infragistics.Win.UltraWinEditors.UltraNumericEditor().
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceInteger_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceDecimal = NEW Infragistics.Win.UltraWinEditors.UltraNumericEditor().
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceDecimal_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceChar_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_Name = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_Name_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyShort_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyShort = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageLarge_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageLarge = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageSmall_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageSmall = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyTenantDomain_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyTenantDomain = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraPanel1:ClientArea:SuspendLayout().
        THIS-OBJECT:ultraPanel1:SuspendLayout().
        CAST(THIS-OBJECT:ultraGroupBox1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraGroupBox1:SuspendLayout().
        CAST(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceChar, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:LoginCompanyBindingSource, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceInteger, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceDecimal, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_Name, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyShort, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageLarge, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageSmall, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyTenantDomain, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraPanel1 */
        /*  */
        /*  */
        /* ultraPanel1.ClientArea */
        /*  */
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ultraGroupBox1).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_Name).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_Name_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyShort_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyShort).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageLarge_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageLarge).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageSmall_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageSmall).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyTenantDomain_Label).
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyTenantDomain).
        resources:ApplyResources(THIS-OBJECT:ultraPanel1, "ultraPanel1":U).
        THIS-OBJECT:ultraPanel1:Name = "ultraPanel1":U.
        /*  */
        /* ultraGroupBox1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraGroupBox1, "ultraGroupBox1":U).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceChar).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceInteger).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceInteger_Label).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceDecimal).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceDecimal_Label).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceChar_Label).
        THIS-OBJECT:ultraGroupBox1:Name = "ultraGroupBox1":U.
        /*  */
        /* LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceChar */
        /*  */
        resources:ApplyResources(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceChar, "LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceChar":U).
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceChar:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:LoginCompanyBindingSource, "LoginCompanyReferenceChar":U, TRUE)).
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceChar:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceChar:Name = "LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceChar":U.
        /*  */
        /* LoginCompanyBindingSource */
        /*  */
        THIS-OBJECT:LoginCompanyBindingSource:BindTo = "QUERY":U.
        THIS-OBJECT:LoginCompanyBindingSource:EntityName = "Consultingwerk.SmartFramework.System.LoginCompanyBusinessEntity":U.
        THIS-OBJECT:LoginCompanyBindingSource:EntityTable = "eSmartLoginCompany":U.
        THIS-OBJECT:LoginCompanyBindingSource:MaxDataGuess = 0.
        THIS-OBJECT:LoginCompanyBindingSource:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 9 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("LoginCompanyGuid":U, "LoginCompanyGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("LoginCompanyName":U, "LoginCompanyName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("LoginCompanyShort":U, "Short Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("LoginCompanyImageLarge":U, "Large Image":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("LoginCompanyImageSmall":U, "Small Image":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("LoginCompanyReferenceChar":U, "Foreign Reference Character":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[7] = NEW Progress.Data.ColumnPropDesc("LoginCompanyReferenceDecimal":U, "Foreign Reference Decimal":U, Progress.Data.DataType:DECIMAL).
        arrayvar0[8] = NEW Progress.Data.ColumnPropDesc("LoginCompanyReferenceInteger":U, "Foreign Reference Integer":U, Progress.Data.DataType:INTEGER).
        arrayvar0[9] = NEW Progress.Data.ColumnPropDesc("LoginCompanyTenantDomain":U, "Multi-Tenancy Domain":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:LoginCompanyBindingSource:TableSchema = tableDesc1.
        /*  */
        /* LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceInteger */
        /*  */
        resources:ApplyResources(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceInteger, "LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceInteger":U).
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceInteger:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:LoginCompanyBindingSource, "LoginCompanyReferenceInteger":U, TRUE)).
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceInteger:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceInteger:Name = "LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceInteger":U.
        /*  */
        /* LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceInteger_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceInteger_Label:Appearance = appearance1.
        resources:ApplyResources(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceInteger_Label, "LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceInteger_Label":U).
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceInteger_Label:Name = "LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceInteger_Label":U.
        /*  */
        /* LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceDecimal */
        /*  */
        resources:ApplyResources(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceDecimal, "LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceDecimal":U).
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceDecimal:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:LoginCompanyBindingSource, "LoginCompanyReferenceDecimal":U, TRUE)).
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceDecimal:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceDecimal:Name = "LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceDecimal":U.
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceDecimal:NumericType = Infragistics.Win.UltraWinEditors.NumericType:Double.
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceDecimal:TabNavigation = Infragistics.Win.UltraWinMaskedEdit.MaskedEditTabNavigation:NextControl.
        /*  */
        /* LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceDecimal_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceDecimal_Label:Appearance = appearance2.
        resources:ApplyResources(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceDecimal_Label, "LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceDecimal_Label":U).
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceDecimal_Label:Name = "LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceDecimal_Label":U.
        /*  */
        /* LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceChar_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceChar_Label:Appearance = appearance3.
        resources:ApplyResources(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceChar_Label, "LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceChar_Label":U).
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceChar_Label:Name = "LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceChar_Label":U.
        /*  */
        /* LoginCompanyBindingSource_eSmartLoginCompany_Name */
        /*  */
        resources:ApplyResources(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_Name, "LoginCompanyBindingSource_eSmartLoginCompany_Name":U).
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_Name:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:LoginCompanyBindingSource, "LoginCompanyName":U, TRUE)).
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_Name:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_Name:Name = "LoginCompanyBindingSource_eSmartLoginCompany_Name":U.
        /*  */
        /* LoginCompanyBindingSource_eSmartLoginCompany_Name_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_Name_Label:Appearance = appearance4.
        resources:ApplyResources(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_Name_Label, "LoginCompanyBindingSource_eSmartLoginCompany_Name_Label":U).
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_Name_Label:Name = "LoginCompanyBindingSource_eSmartLoginCompany_Name_Label":U.
        /*  */
        /* LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyShort_Label */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyShort_Label:Appearance = appearance5.
        resources:ApplyResources(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyShort_Label, "LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyShort_Label":U).
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyShort_Label:Name = "LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyShort_Label":U.
        /*  */
        /* LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyShort */
        /*  */
        resources:ApplyResources(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyShort, "LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyShort":U).
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyShort:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:LoginCompanyBindingSource, "LoginCompanyShort":U, TRUE)).
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyShort:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyShort:Name = "LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyShort":U.
        /*  */
        /* LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageLarge_Label */
        /*  */
        appearance6:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageLarge_Label:Appearance = appearance6.
        resources:ApplyResources(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageLarge_Label, "LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageLarge_Label":U).
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageLarge_Label:Name = "LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageLarge_Label":U.
        /*  */
        /* LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageLarge */
        /*  */
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageLarge:AllowDrop = TRUE.
        resources:ApplyResources(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageLarge, "LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageLarge":U).
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageLarge:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:LoginCompanyBindingSource, "LoginCompanyImageLarge":U, TRUE)).
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageLarge:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageLarge:Name = "LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageLarge":U.
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageLarge:DragDrop:Subscribe(THIS-OBJECT:ImageSelection_DragDrop).
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageLarge:DragEnter:Subscribe(THIS-OBJECT:ImageSelection_DragEnter).
        /*  */
        /* LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageSmall_Label */
        /*  */
        appearance7:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageSmall_Label:Appearance = appearance7.
        resources:ApplyResources(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageSmall_Label, "LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageSmall_Label":U).
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageSmall_Label:Name = "LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageSmall_Label":U.
        /*  */
        /* LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageSmall */
        /*  */
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageSmall:AllowDrop = TRUE.
        resources:ApplyResources(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageSmall, "LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageSmall":U).
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageSmall:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:LoginCompanyBindingSource, "LoginCompanyImageSmall":U, TRUE)).
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageSmall:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageSmall:Name = "LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageSmall":U.
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageSmall:DragDrop:Subscribe(THIS-OBJECT:ImageSelection_DragDrop).
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageSmall:DragEnter:Subscribe(THIS-OBJECT:ImageSelection_DragEnter).
        /*  */
        /* LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyTenantDomain_Label */
        /*  */
        appearance8:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyTenantDomain_Label:Appearance = appearance8.
        resources:ApplyResources(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyTenantDomain_Label, "LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyTenantDomain_Label":U).
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyTenantDomain_Label:Name = "LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyTenantDomain_Label":U.
        /*  */
        /* LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyTenantDomain */
        /*  */
        resources:ApplyResources(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyTenantDomain, "LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyTenantDomain":U).
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyTenantDomain:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:LoginCompanyBindingSource, "LoginCompanyTenantDomain":U, TRUE)).
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyTenantDomain:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyTenantDomain:Name = "LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyTenantDomain":U.
        /*  */
        /* SmartLoginCompanyViewer */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraPanel1).
        THIS-OBJECT:Name = "SmartLoginCompanyViewer":U.
        THIS-OBJECT:ultraPanel1:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:ultraPanel1:ClientArea:PerformLayout().
        THIS-OBJECT:ultraPanel1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraGroupBox1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraGroupBox1:ResumeLayout(FALSE).
        THIS-OBJECT:ultraGroupBox1:PerformLayout().
        CAST(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceChar, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:LoginCompanyBindingSource, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceInteger, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyReferenceDecimal, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_Name, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyShort, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageLarge, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyImageSmall, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:LoginCompanyBindingSource_eSmartLoginCompany_LoginCompanyTenantDomain, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartLoginCompanyViewer class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartLoginCompanyViewer ():

    END DESTRUCTOR .

END CLASS.
