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
    File        : SmartKeyFieldAssignmentTypeViewerControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 03.10.2015 10:28:10
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.System.SmartKeyFieldAssignmentTypeViewerControl 
    INHERITS SmartViewerControl: 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartKeyFieldAssignmentType_KeyFieldAssignmentKey_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartKeyFieldAssignmentType_KeyFieldAssignmentKey AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartKeyFieldAssignmentType_KeyFieldAssignmentDescription_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartKeyFieldAssignmentType_KeyFieldAssignmentDescription AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartKeyFieldAssignmentTypeViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartKeyFieldAssignmentTypeViewerControl ():
                
        SUPER().
        InitializeComponent().
        
        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF
                
        THIS-OBJECT:SetControlEnabled (eSmartKeyFieldAssignmentType_KeyFieldAssignmentKey, 
                                       ControlEnabledEnum:Add) .                
                
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
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartKeyFieldAssignmentType":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.System.SmartKeyFieldAssignmentTypeViewerControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:eSmartKeyFieldAssignmentType_KeyFieldAssignmentKey_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartKeyFieldAssignmentType_KeyFieldAssignmentKey = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartKeyFieldAssignmentType_KeyFieldAssignmentDescription_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartKeyFieldAssignmentType_KeyFieldAssignmentDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartKeyFieldAssignmentType_KeyFieldAssignmentKey, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartKeyFieldAssignmentType_KeyFieldAssignmentDescription, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.System.KeyFieldAssignmentTypeBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartKeyFieldAssignmentType":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 3 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("KeyFieldAssignmentGUID":U, "KeyFieldAssignmentGUID":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("KeyFieldAssignmentKey":U, "KeyFieldAssignmentKey":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("KeyFieldAssignmentDescription":U, "KeyFieldAssignmentDescription":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* eSmartKeyFieldAssignmentType_KeyFieldAssignmentKey_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartKeyFieldAssignmentType_KeyFieldAssignmentKey_Label:Appearance = appearance1.
        resources:ApplyResources(THIS-OBJECT:eSmartKeyFieldAssignmentType_KeyFieldAssignmentKey_Label, "eSmartKeyFieldAssignmentType_KeyFieldAssignmentKey_Label":U).
        THIS-OBJECT:eSmartKeyFieldAssignmentType_KeyFieldAssignmentKey_Label:Name = "eSmartKeyFieldAssignmentType_KeyFieldAssignmentKey_Label":U.
        /*  */
        /* eSmartKeyFieldAssignmentType_KeyFieldAssignmentKey */
        /*  */
        THIS-OBJECT:eSmartKeyFieldAssignmentType_KeyFieldAssignmentKey:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "KeyFieldAssignmentKey":U, TRUE)).
        THIS-OBJECT:eSmartKeyFieldAssignmentType_KeyFieldAssignmentKey:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:eSmartKeyFieldAssignmentType_KeyFieldAssignmentKey, "eSmartKeyFieldAssignmentType_KeyFieldAssignmentKey":U).
        THIS-OBJECT:eSmartKeyFieldAssignmentType_KeyFieldAssignmentKey:Name = "eSmartKeyFieldAssignmentType_KeyFieldAssignmentKey":U.
        /*  */
        /* eSmartKeyFieldAssignmentType_KeyFieldAssignmentDescription_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartKeyFieldAssignmentType_KeyFieldAssignmentDescription_Label:Appearance = appearance2.
        resources:ApplyResources(THIS-OBJECT:eSmartKeyFieldAssignmentType_KeyFieldAssignmentDescription_Label, "eSmartKeyFieldAssignmentType_KeyFieldAssignmentDescription_Label":U).
        THIS-OBJECT:eSmartKeyFieldAssignmentType_KeyFieldAssignmentDescription_Label:Name = "eSmartKeyFieldAssignmentType_KeyFieldAssignmentDescription_Label":U.
        /*  */
        /* eSmartKeyFieldAssignmentType_KeyFieldAssignmentDescription */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartKeyFieldAssignmentType_KeyFieldAssignmentDescription, "eSmartKeyFieldAssignmentType_KeyFieldAssignmentDescription":U).
        THIS-OBJECT:eSmartKeyFieldAssignmentType_KeyFieldAssignmentDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "KeyFieldAssignmentDescription":U, TRUE)).
        THIS-OBJECT:eSmartKeyFieldAssignmentType_KeyFieldAssignmentDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartKeyFieldAssignmentType_KeyFieldAssignmentDescription:Name = "eSmartKeyFieldAssignmentType_KeyFieldAssignmentDescription":U.
        /*  */
        /* SmartKeyFieldAssignmentTypeViewerControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartKeyFieldAssignmentType_KeyFieldAssignmentKey_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartKeyFieldAssignmentType_KeyFieldAssignmentKey).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartKeyFieldAssignmentType_KeyFieldAssignmentDescription_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartKeyFieldAssignmentType_KeyFieldAssignmentDescription).
        THIS-OBJECT:Name = "SmartKeyFieldAssignmentTypeViewerControl":U.
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartKeyFieldAssignmentType_KeyFieldAssignmentKey, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartKeyFieldAssignmentType_KeyFieldAssignmentDescription, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.



    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartKeyFieldAssignmentTypeViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartKeyFieldAssignmentTypeViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
