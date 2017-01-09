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
    File        : FieldValidationPropertiesControl
    Purpose     : User Control with field validation settings
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon May 18 21:57:39 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                   FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.UI.*         FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Validation.* FROM PROPATH .
USING Consultingwerk.Framework.TypeDescriptor.*          FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*              FROM PROPATH .
USING Consultingwerk.Util.*                              FROM PROPATH .
USING Consultingwerk.Windows.Controls.*                  FROM PROPATH .
USING Progress.Lang.*                                    FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.UI.FieldValidationPropertiesControl
    INHERITS SmartUserControl:

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton1 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton2 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraComboEditor1 AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE propertySheetControl1 AS Consultingwerk.Windows.Controls.PropertySheetControl NO-UNDO.

    DEFINE VARIABLE oObject  AS Progress.Lang.Object NO-UNDO .
    DEFINE VARIABLE lSaving  AS LOGICAL              NO-UNDO INIT FALSE .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the value of this control has changed
        Notes:
        @param sender The object that raised the ValueChanged event
        @param e The Consultingwerk.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT ValueChanged SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                     e AS Consultingwerk.EventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FieldValidationPropertiesControl class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC FieldValidationPropertiesControl ():

        SUPER().

        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        THIS-OBJECT:TextChanged:Subscribe (TextChangedHandler) .
        THIS-OBJECT:propertySheetControl1:PropertyChanged:Subscribe (PropertyChangedHandler) .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Assigns the serialization of the current validation settings instance
                 to the Text property of the user control
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Assign ():

        ASSIGN lSaving = TRUE .

        IF NOT VALID-OBJECT (oObject) THEN
            ASSIGN THIS-OBJECT:Text = "":U .
        ELSE DO:
            propertySheetControl1:Assign () .

            ASSIGN THIS-OBJECT:Text = CAST (oObject, ISerializable):Serialize () .
        END.

        FINALLY:
            ASSIGN lSaving = FALSE .
        END FINALLY.

    END METHOD.

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
        THIS-OBJECT:ultraComboEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:propertySheetControl1 = NEW Consultingwerk.Windows.Controls.PropertySheetControl().
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:ultraButton1 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraButton2 = NEW Infragistics.Win.Misc.UltraButton().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:panel1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraComboEditor1 */
        /*  */
        THIS-OBJECT:ultraComboEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:ultraComboEditor1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraComboEditor1:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        THIS-OBJECT:ultraComboEditor1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraComboEditor1:Name = "ultraComboEditor1":U.
        THIS-OBJECT:ultraComboEditor1:Size = NEW System.Drawing.Size(428, 21).
        THIS-OBJECT:ultraComboEditor1:TabIndex = 0.
        /*  */
        /* propertySheetControl1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:propertySheetControl1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:propertySheetControl1:AutoScroll = TRUE.
        THIS-OBJECT:propertySheetControl1:AutoSizeMode = System.Windows.Forms.AutoSizeMode:GrowAndShrink.
        THIS-OBJECT:propertySheetControl1:Location = NEW System.Drawing.Point(0, 21).
        THIS-OBJECT:propertySheetControl1:MinimumSize = NEW System.Drawing.Size(200, 50).
        THIS-OBJECT:propertySheetControl1:Name = "propertySheetControl1":U.
        THIS-OBJECT:propertySheetControl1:Size = NEW System.Drawing.Size(500, 397).
        THIS-OBJECT:propertySheetControl1:TabIndex = 1.
        /*  */
        /* panel1 */
        /*  */
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraComboEditor1).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraButton1).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraButton2).
        THIS-OBJECT:panel1:Dock = System.Windows.Forms.DockStyle:Top.
        THIS-OBJECT:panel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:panel1:Margin = NEW System.Windows.Forms.Padding(5).
        THIS-OBJECT:panel1:Name = "panel1":U.
        THIS-OBJECT:panel1:Size = NEW System.Drawing.Size(500, 21).
        THIS-OBJECT:panel1:TabIndex = 2.
        /*  */
        /* ultraButton1 */
        /*  */
        THIS-OBJECT:ultraButton1:AutoSize = TRUE.
        THIS-OBJECT:ultraButton1:Dock = System.Windows.Forms.DockStyle:Right.
        THIS-OBJECT:ultraButton1:Location = NEW System.Drawing.Point(428, 0).
        THIS-OBJECT:ultraButton1:Name = "ultraButton1":U.
        THIS-OBJECT:ultraButton1:Size = NEW System.Drawing.Size(31, 21).
        THIS-OBJECT:ultraButton1:TabIndex = 3.
        THIS-OBJECT:ultraButton1:Text = "Set":U.
        THIS-OBJECT:ultraButton1:Click:Subscribe(THIS-OBJECT:ultraButton1_Click).
        /*  */
        /* ultraButton2 */
        /*  */
        THIS-OBJECT:ultraButton2:AutoSize = TRUE.
        THIS-OBJECT:ultraButton2:Dock = System.Windows.Forms.DockStyle:Right.
        THIS-OBJECT:ultraButton2:Location = NEW System.Drawing.Point(459, 0).
        THIS-OBJECT:ultraButton2:Name = "ultraButton2":U.
        THIS-OBJECT:ultraButton2:Size = NEW System.Drawing.Size(41, 21).
        THIS-OBJECT:ultraButton2:TabIndex = 3.
        THIS-OBJECT:ultraButton2:Text = "Clear":U.
        THIS-OBJECT:ultraButton2:Click:Subscribe(THIS-OBJECT:ultraButton2_Click).
        /*  */
        /* FieldValidationPropertiesControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:propertySheetControl1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:Name = "FieldValidationPropertiesControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(500, 418).
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        THIS-OBJECT:panel1:PerformLayout().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):

        DEFINE VARIABLE oValidationProvider AS IFieldValidationTypeProvider NO-UNDO .

        SUPER:OnLoad (e) .

        oValidationProvider = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.Validation.IFieldValidationTypeProvider
                                                            "NEW Consultingwerk.BusinessEntityDesigner.Validation.FieldValidationTypeProvider ()"} .

        ultraComboEditor1:ValueList = ValueListHelper:FromList (oValidationProvider:GetFieldValidationTypes()) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the PropertyChanged event of the property grid control
        Notes:
        @param sender The reference to the object that raised the event
        @param e The PropertyChangedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID PropertyChangedHandler (sender AS Progress.Lang.Object,
                                                  e AS PropertyChangedEventArgs):

        THIS-OBJECT:OnValueChanged (Consultingwerk.EventArgs:Empty) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the ValueChanged event
        Notes:
        @param e The Consultingwerk.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnValueChanged (e AS Consultingwerk.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = Consultingwerk.EventArgs:Empty .

        THIS-OBJECT:ValueChanged:Publish (THIS-OBJECT, e) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the TextChanged event of the user control
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID TextChangedHandler (sender AS System.Object,
                                            e AS System.EventArgs):

        DEFINE VARIABLE lcText AS LONGCHAR NO-UNDO .

        IF lSaving THEN
            RETURN .

        IF THIS-OBJECT:Text > "":U THEN DO:
            FIX-CODEPAGE (lcText) = "utf-8":U .

            lcText = THIS-OBJECT:Text .

            oObject = Serializable:DeserializeInstance (lcText) .

            THIS-OBJECT:propertySheetControl1:ObjectReference = CAST (oObject, IHasTypeDescriptor) . .
        END.
        ELSE DO:
            THIS-OBJECT:propertySheetControl1:Clear () .
            GarbageCollectorHelper:DeleteObject (oObject) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the ultraButton1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraButton1_Click (sender AS System.Object, e AS System.EventArgs):

        DEFINE VARIABLE cType      AS CHARACTER            NO-UNDO .
        DEFINE VARIABLE oNewObject AS Progress.Lang.Object NO-UNDO .

        ASSIGN cType = UNBOX (ultraComboEditor1:Value) .

        IF CharacterType:IsNullOrEmpty (cType) THEN
            RETURN .

        oNewObject = DYNAMIC-NEW (cType) () .

        oObject = oNewObject .

        propertySheetControl1:ObjectReference = CAST (oObject, IHasTypeDescriptor) .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the ultraButton1
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraButton2_Click (sender AS System.Object, e AS System.EventArgs ):

        oObject = ? .
        propertySheetControl1:ObjectReference = ? .

        THIS-OBJECT:OnValueChanged (Consultingwerk.EventArgs:Empty) .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the FieldValidationPropertiesControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC FieldValidationPropertiesControl ():

    END DESTRUCTOR.

END CLASS.
