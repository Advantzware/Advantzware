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
    File        : SmartClassTypeViewerControl
    Purpose     : Viewer for SmartClassType
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 04.09.2015 23:46:45
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Repository.Class.SmartClassTypeViewerControl 
    INHERITS SmartViewerControl: 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartClassType_ClassTypeName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartClassType_ClassTypeName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartClassType_ClassTypeDescription_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartClassType_ClassTypeDescription AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartClassType_AvmType AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartClassTypeViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartClassTypeViewerControl ():
                
        SUPER().
        InitializeComponent().
        
        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF
                
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
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartClassType":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:eSmartClassType_ClassTypeName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartClassType_ClassTypeName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartClassType_ClassTypeDescription_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartClassType_ClassTypeDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartClassType_AvmType = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartClassType_ClassTypeName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartClassType_ClassTypeDescription, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartClassType_AvmType, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.Repository.Class.ClassTypeBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartClassType":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 4 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("ClassTypeGuid":U, "Class Type":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("ClassTypeName":U, "Class Type Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("ClassTypeDescription":U, "Description":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("AvmType":U, "AVM Type":U, Progress.Data.DataType:LOGICAL).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* eSmartClassType_ClassTypeName_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartClassType_ClassTypeName_Label:Appearance = appearance1.
        THIS-OBJECT:eSmartClassType_ClassTypeName_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartClassType_ClassTypeName_Label:Location = NEW System.Drawing.Point(20, 24).
        THIS-OBJECT:eSmartClassType_ClassTypeName_Label:Name = "eSmartClassType_ClassTypeName_Label":U.
        THIS-OBJECT:eSmartClassType_ClassTypeName_Label:Size = NEW System.Drawing.Size(97, 14).
        THIS-OBJECT:eSmartClassType_ClassTypeName_Label:TabIndex = 1.
        THIS-OBJECT:eSmartClassType_ClassTypeName_Label:Text = "Class Type Name:":U.
        /*  */
        /* eSmartClassType_ClassTypeName */
        /*  */
        THIS-OBJECT:eSmartClassType_ClassTypeName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ClassTypeName":U, TRUE)).
        THIS-OBJECT:eSmartClassType_ClassTypeName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartClassType_ClassTypeName:Location = NEW System.Drawing.Point(130, 20).
        THIS-OBJECT:eSmartClassType_ClassTypeName:Name = "eSmartClassType_ClassTypeName":U.
        THIS-OBJECT:eSmartClassType_ClassTypeName:Size = NEW System.Drawing.Size(150, 21).
        THIS-OBJECT:eSmartClassType_ClassTypeName:TabIndex = 2.
        /*  */
        /* eSmartClassType_ClassTypeDescription_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartClassType_ClassTypeDescription_Label:Appearance = appearance2.
        THIS-OBJECT:eSmartClassType_ClassTypeDescription_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartClassType_ClassTypeDescription_Label:Location = NEW System.Drawing.Point(20, 50).
        THIS-OBJECT:eSmartClassType_ClassTypeDescription_Label:Name = "eSmartClassType_ClassTypeDescription_Label":U.
        THIS-OBJECT:eSmartClassType_ClassTypeDescription_Label:Size = NEW System.Drawing.Size(64, 14).
        THIS-OBJECT:eSmartClassType_ClassTypeDescription_Label:TabIndex = 3.
        THIS-OBJECT:eSmartClassType_ClassTypeDescription_Label:Text = "Description:":U.
        /*  */
        /* eSmartClassType_ClassTypeDescription */
        /*  */
        THIS-OBJECT:eSmartClassType_ClassTypeDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "ClassTypeDescription":U, TRUE)).
        THIS-OBJECT:eSmartClassType_ClassTypeDescription:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartClassType_ClassTypeDescription:Location = NEW System.Drawing.Point(130, 46).
        THIS-OBJECT:eSmartClassType_ClassTypeDescription:Name = "eSmartClassType_ClassTypeDescription":U.
        THIS-OBJECT:eSmartClassType_ClassTypeDescription:Size = NEW System.Drawing.Size(150, 21).
        THIS-OBJECT:eSmartClassType_ClassTypeDescription:TabIndex = 4.
        /*  */
        /* eSmartClassType_AvmType */
        /*  */
        THIS-OBJECT:eSmartClassType_AvmType:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "AvmType":U, TRUE)).
        THIS-OBJECT:eSmartClassType_AvmType:Location = NEW System.Drawing.Point(130, 72).
        THIS-OBJECT:eSmartClassType_AvmType:Name = "eSmartClassType_AvmType":U.
        THIS-OBJECT:eSmartClassType_AvmType:Size = NEW System.Drawing.Size(177, 20).
        THIS-OBJECT:eSmartClassType_AvmType:TabIndex = 6.
        THIS-OBJECT:eSmartClassType_AvmType:Text = "AVM Type (accessible in ABL)":U.
        THIS-OBJECT:eSmartClassType_AvmType:UseMnemonics = TRUE.
        /*  */
        /* SmartClassTypeViewerControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartClassType_ClassTypeName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartClassType_ClassTypeName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartClassType_ClassTypeDescription_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartClassType_ClassTypeDescription).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartClassType_AvmType).
        THIS-OBJECT:Name = "SmartClassTypeViewerControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(321, 300).
        THIS-OBJECT:SmartTableIOState = "NoDataSource":U.
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartClassType_ClassTypeName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartClassType_ClassTypeDescription, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartClassType_AvmType, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.



    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartClassTypeViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartClassTypeViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
