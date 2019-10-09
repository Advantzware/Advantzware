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
    File        : SmartMenuStyleViewerControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 09.08.2013 08:56:55
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Menu.SmartMenuStyleViewerControl 
    INHERITS SmartViewerControl: 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartMenuStyle_MenuStyleCode_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartMenuStyle_MenuStyleCode AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartMenuStyle_MenuStyleDescription_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartMenuStyle_MenuStyleDescription AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartMenuStyleViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartMenuStyleViewerControl ():
                
        SUPER().
        InitializeComponent().
        
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
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartMenuStyle":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Menu.SmartMenuStyleViewerControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:eSmartMenuStyle_MenuStyleCode_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartMenuStyle_MenuStyleCode = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartMenuStyle_MenuStyleDescription_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartMenuStyle_MenuStyleDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartMenuStyle_MenuStyleCode, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartMenuStyle_MenuStyleDescription, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityJoin = "YES":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.Menu.MenuStyleBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartMenuStyle":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 3 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("MenuStyleGuid":U, "MenuStyleGuid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("MenuStyleCode":U, "Menu Style Code":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("MenuStyleDescription":U, "Description":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* eSmartMenuStyle_MenuStyleCode_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartMenuStyle_MenuStyleCode_Label:Appearance = appearance1.
        resources:ApplyResources(THIS-OBJECT:eSmartMenuStyle_MenuStyleCode_Label, "eSmartMenuStyle_MenuStyleCode_Label":U).
        THIS-OBJECT:eSmartMenuStyle_MenuStyleCode_Label:Name = "eSmartMenuStyle_MenuStyleCode_Label":U.
        /*  */
        /* eSmartMenuStyle_MenuStyleCode */
        /*  */
        THIS-OBJECT:eSmartMenuStyle_MenuStyleCode:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "MenuStyleCode":U, TRUE)).
        THIS-OBJECT:eSmartMenuStyle_MenuStyleCode:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:eSmartMenuStyle_MenuStyleCode, "eSmartMenuStyle_MenuStyleCode":U).
        THIS-OBJECT:eSmartMenuStyle_MenuStyleCode:Name = "eSmartMenuStyle_MenuStyleCode":U.
        /*  */
        /* eSmartMenuStyle_MenuStyleDescription_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartMenuStyle_MenuStyleDescription_Label:Appearance = appearance2.
        resources:ApplyResources(THIS-OBJECT:eSmartMenuStyle_MenuStyleDescription_Label, "eSmartMenuStyle_MenuStyleDescription_Label":U).
        THIS-OBJECT:eSmartMenuStyle_MenuStyleDescription_Label:Name = "eSmartMenuStyle_MenuStyleDescription_Label":U.
        /*  */
        /* eSmartMenuStyle_MenuStyleDescription */
        /*  */
        resources:ApplyResources(THIS-OBJECT:eSmartMenuStyle_MenuStyleDescription, "eSmartMenuStyle_MenuStyleDescription":U).
        THIS-OBJECT:eSmartMenuStyle_MenuStyleDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "MenuStyleDescription":U, TRUE)).
        THIS-OBJECT:eSmartMenuStyle_MenuStyleDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartMenuStyle_MenuStyleDescription:Name = "eSmartMenuStyle_MenuStyleDescription":U.
        /*  */
        /* SmartMenuStyleViewerControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartMenuStyle_MenuStyleCode_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartMenuStyle_MenuStyleCode).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartMenuStyle_MenuStyleDescription_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartMenuStyle_MenuStyleDescription).
        THIS-OBJECT:Name = "SmartMenuStyleViewerControl":U.
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartMenuStyle_MenuStyleCode, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartMenuStyle_MenuStyleDescription, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartMenuStyleViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartMenuStyleViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
