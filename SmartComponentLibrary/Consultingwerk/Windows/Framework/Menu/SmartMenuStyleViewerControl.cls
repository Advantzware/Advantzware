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
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartMenuStyle":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
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
        THIS-OBJECT:smartBusinessEntityBindingSource1:AddFields = FALSE.
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:ClearSchema = FALSE.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityJoin = "YES":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.Menu.MenuStyleBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartMenuStyle":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:ImportSchema = FALSE.
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
        THIS-OBJECT:eSmartMenuStyle_MenuStyleCode_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartMenuStyle_MenuStyleCode_Label:Location = NEW System.Drawing.Point(20, 24).
        THIS-OBJECT:eSmartMenuStyle_MenuStyleCode_Label:Name = "eSmartMenuStyle_MenuStyleCode_Label":U.
        THIS-OBJECT:eSmartMenuStyle_MenuStyleCode_Label:Size = NEW System.Drawing.Size(94, 14).
        THIS-OBJECT:eSmartMenuStyle_MenuStyleCode_Label:TabIndex = 1.
        THIS-OBJECT:eSmartMenuStyle_MenuStyleCode_Label:Text = "Menu Style Code:":U.
        /*  */
        /* eSmartMenuStyle_MenuStyleCode */
        /*  */
        THIS-OBJECT:eSmartMenuStyle_MenuStyleCode:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "MenuStyleCode":U, TRUE)).
        THIS-OBJECT:eSmartMenuStyle_MenuStyleCode:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartMenuStyle_MenuStyleCode:Location = NEW System.Drawing.Point(130, 20).
        THIS-OBJECT:eSmartMenuStyle_MenuStyleCode:Name = "eSmartMenuStyle_MenuStyleCode":U.
        THIS-OBJECT:eSmartMenuStyle_MenuStyleCode:Size = NEW System.Drawing.Size(150, 21).
        THIS-OBJECT:eSmartMenuStyle_MenuStyleCode:TabIndex = 2.
        /*  */
        /* eSmartMenuStyle_MenuStyleDescription_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartMenuStyle_MenuStyleDescription_Label:Appearance = appearance2.
        THIS-OBJECT:eSmartMenuStyle_MenuStyleDescription_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartMenuStyle_MenuStyleDescription_Label:Location = NEW System.Drawing.Point(20, 50).
        THIS-OBJECT:eSmartMenuStyle_MenuStyleDescription_Label:Name = "eSmartMenuStyle_MenuStyleDescription_Label":U.
        THIS-OBJECT:eSmartMenuStyle_MenuStyleDescription_Label:Size = NEW System.Drawing.Size(64, 14).
        THIS-OBJECT:eSmartMenuStyle_MenuStyleDescription_Label:TabIndex = 3.
        THIS-OBJECT:eSmartMenuStyle_MenuStyleDescription_Label:Text = "Description:":U.
        /*  */
        /* eSmartMenuStyle_MenuStyleDescription */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartMenuStyle_MenuStyleDescription:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartMenuStyle_MenuStyleDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "MenuStyleDescription":U, TRUE)).
        THIS-OBJECT:eSmartMenuStyle_MenuStyleDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartMenuStyle_MenuStyleDescription:Location = NEW System.Drawing.Point(130, 46).
        THIS-OBJECT:eSmartMenuStyle_MenuStyleDescription:Name = "eSmartMenuStyle_MenuStyleDescription":U.
        THIS-OBJECT:eSmartMenuStyle_MenuStyleDescription:Size = NEW System.Drawing.Size(167, 21).
        THIS-OBJECT:eSmartMenuStyle_MenuStyleDescription:TabIndex = 4.
        /*  */
        /* SmartMenuStyleViewerControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
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
