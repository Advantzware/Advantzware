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
    File        : SmartPropertyGridControl
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed May 25 18:01:09 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.*    FROM PROPATH .
USING Progress.Windows.* FROM ASSEMBLY .

CLASS Consultingwerk.Windows.Framework.Repository.Object.SmartPropertyGridControl INHERITS UserControl:

    DEFINE PRIVATE VARIABLE btnSelectMaster AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE lblSelectedObject AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE radPropertyGrid1 AS Telerik.WinControls.UI.RadPropertyGrid NO-UNDO.


    CONSTRUCTOR PUBLIC SmartPropertyGridControl (  ):


        SUPER().
        InitializeComponent().
        THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
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
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Repository.Object.SmartPropertyGridControl":U).
        THIS-OBJECT:radPropertyGrid1 = NEW Telerik.WinControls.UI.RadPropertyGrid().
        THIS-OBJECT:btnSelectMaster = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:lblSelectedObject = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        CAST(THIS-OBJECT:radPropertyGrid1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:lblSelectedObject, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* radPropertyGrid1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:radPropertyGrid1, "radPropertyGrid1":U).
        THIS-OBJECT:radPropertyGrid1:Name = "radPropertyGrid1":U.
        THIS-OBJECT:radPropertyGrid1:PropertySort = System.Windows.Forms.PropertySort:CategorizedAlphabetical.
        THIS-OBJECT:radPropertyGrid1:SortOrder = System.Windows.Forms.SortOrder:Ascending.
        THIS-OBJECT:radPropertyGrid1:ToolbarVisible = TRUE.
        /*  */
        /* btnSelectMaster */
        /*  */
        resources:ApplyResources(THIS-OBJECT:btnSelectMaster, "btnSelectMaster":U).
        THIS-OBJECT:btnSelectMaster:Name = "btnSelectMaster":U.
        /*  */
        /* lblSelectedObject */
        /*  */
        THIS-OBJECT:lblSelectedObject:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        resources:ApplyResources(THIS-OBJECT:lblSelectedObject, "lblSelectedObject":U).
        THIS-OBJECT:lblSelectedObject:Name = "lblSelectedObject":U.
        THIS-OBJECT:lblSelectedObject:ReadOnly = TRUE.
        /*  */
        /* SmartPropertyGridControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:radPropertyGrid1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:lblSelectedObject).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnSelectMaster).
        THIS-OBJECT:Name = "SmartPropertyGridControl":U.
        CAST(THIS-OBJECT:radPropertyGrid1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:lblSelectedObject, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    DESTRUCTOR PUBLIC SmartPropertyGridControl ( ):

    END DESTRUCTOR.

END CLASS.