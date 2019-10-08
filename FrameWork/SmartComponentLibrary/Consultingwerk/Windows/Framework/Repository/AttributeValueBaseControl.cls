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
    File        : AttributeValueBaseControl
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Nov 29 07:37:15 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.* FROM PROPATH.
USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Windows.Framework.Repository.*   FROM PROPATH .
USING Infragistics.Win.UltraWinGrid.*                 FROM ASSEMBLY .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Repository.AttributeValueBaseControl
    INHERITS SmartUserControl:

    DEFINE VARIABLE smartUpdatableBrowser AS SmartUpdatableBrowser NO-UNDO .

    /**
     * Purpose: Raised when the control has changes that need to be saved
     * Notes:
     */
    DEFINE PUBLIC EVENT HasChangesChanged DELEGATE System.EventHandler .

    /**
     * Purpose: Returns if the object has changes that need to be saved
     * Notes:
     */
    DEFINE PUBLIC PROPERTY HasChanges AS LOGICAL NO-UNDO
    GET.
    PROTECTED SET (arg AS LOGICAL):

        IF arg <> THIS-OBJECT:HasChanges THEN DO:
            THIS-OBJECT:HasChanges = arg .
            THIS-OBJECT:OnHasChangesChanged(System.EventArgs:Empty) .
        END.

    END SET.

    /**
     * Purpose: Constructor for the AttributeValueBaseControl class
     * Notes:
     */
    CONSTRUCTOR PUBLIC AttributeValueBaseControl ():
        SUPER ().

    END CONSTRUCTOR.

    /**
     * Purpose: Initializes the Attribute Value cell in the given UltraGridRow
     * Notes:
     * @param poRow The UltraGridRow to initialize
     */
    METHOD PROTECTED VOID InitializeRow (poRow AS UltraGridRow):

        DEFINE VARIABLE cDataType     AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE oCell         AS UltraGridCell NO-UNDO .
        DEFINE VARIABLE cLookupType   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cLookupValues AS CHARACTER NO-UNDO.

        IF NOT poRow:IsDataRow THEN
            RETURN .

        oCell = poRow:Cells ["AttributeValue":U] .
        oCell:Style = ColumnStyle:Default .

        ASSIGN cDataType = UNBOX (poRow:Cells ["RepositoryType":U]:Value) .

        CASE cDataType:

            WHEN "CHARACTER":U THEN
                ASSIGN oCell:Value = poRow:Cells["CharacterValue":U]:Value
                       oCell:Style = ColumnStyle:Default .
            WHEN "INTEGER":U THEN
                ASSIGN oCell:Value = poRow:Cells["IntegerValue":U]:Value
                       oCell:Style = ColumnStyle:IntegerWithSpin .
            WHEN "INT64":U THEN
                ASSIGN oCell:Value = poRow:Cells["Int64Value":U]:Value
                       oCell:Style = ColumnStyle:IntegerWithSpin .
            WHEN "DATE":U THEN
                ASSIGN oCell:Value = poRow:Cells["DateValue":U]:Value
                       oCell:Style = ColumnStyle:Date .
            WHEN "DATETIME":U THEN
                ASSIGN oCell:Value = poRow:Cells["DateTimeValue":U]:Value
                       oCell:Style = ColumnStyle:DateTime .
            WHEN "DATETIME-TZ":U THEN
                ASSIGN oCell:Value = poRow:Cells["DateTimeTzValue":U]:Value
                       oCell:Style = ColumnStyle:DateTime .
            WHEN "DECIMAL":U THEN
                ASSIGN oCell:Value = poRow:Cells["DecimalValue":U]:Value
                       oCell:Style = ColumnStyle:DoubleWithSpin .
            WHEN "LOGICAL":U THEN
                ASSIGN oCell:Value = poRow:Cells["LogicalValue":U]:Value
                       oCell:Style = ColumnStyle:CheckBox .
            WHEN "HANDLE":U THEN
                ASSIGN oCell:Value      = ?
                       oCell:Style      = ColumnStyle:Default
                       oCell:Activation = Activation:Disabled.

        END CASE .

        ASSIGN cLookupType   = UNBOX (poRow:Cells["LookupType":U]:Value)
               cLookupValues = UNBOX (poRow:Cells["LookupValues":U]:Value) .

        CASE cLookupType:
            WHEN "LIST":U THEN
                ASSIGN oCell:ValueList  = ValueListHelper:FromListItemPairs (cLookupValues, CHR(3))
                       oCell:Style      = ColumnStyle:DropDownList .
            WHEN "DIALOG":U OR WHEN "DIALOG-R":U THEN
                ASSIGN oCell:Style                  = ColumnStyle:EditButton
                       oCell:ButtonAppearance:Image = ImageHelper:Load ("Consultingwerk/Windows/Images/folder_16.png":U) .

            OTHERWISE
                IF cDataType <> "HANDLE":U THEN
                    ASSIGN oCell:Activation = Activation:AllowEdit .
        END CASE .

        /* Read only row - when inherited and constant*/
        IF UNBOX (poRow:Cells["IsInherited":U]:Value) = TRUE AND
           UNBOX (poRow:Cells["ConstantValue":U]:Value) = TRUE THEN DO:

            ASSIGN poRow:Cells["IsInherited":U]:Activation    = Activation:NoEdit
                   poRow:Cells["ConstantValue":U]:Activation  = Activation:NoEdit
                   poRow:Cells["AttributeValue":U]:Activation = Activation:NoEdit .
        END.
        /* Can't change IsInherited (to reset to base class value) on un-inherited
           property */
        ELSE IF UNBOX (poRow:Cells["InheritedFrom":U]:Value) = "":U THEN DO:
            ASSIGN poRow:Cells["IsInherited":U]:Activation    = Activation:NoEdit
                   poRow:Cells["ConstantValue":U]:Activation  = Activation:AllowEdit
                   poRow:Cells["AttributeValue":U]:Activation = Activation:AllowEdit .

        END.
        ELSE
            ASSIGN poRow:Cells["IsInherited":U]:Activation    = Activation:AllowEdit
                   poRow:Cells["ConstantValue":U]:Activation  = Activation:AllowEdit
                   poRow:Cells["AttributeValue":U]:Activation = Activation:AllowEdit .

    END METHOD .

    /**
     * Purpose: Raises the HasChangesChanged
     * Notes:
     * @param e The System.EventArgs with the data for the event
     */
    METHOD PROTECTED VOID OnHasChangesChanged (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:HasChangesChanged:Publish (THIS-OBJECT, e) .

        IF THIS-OBJECT:HasChanges THEN DO:
            smartUpdatableBrowser:Focus () .
            smartUpdatableBrowser:PerformAction(UltraGridAction:EnterEditMode, FALSE, FALSE).
        END.

    END METHOD .

    /**
     * Purpose: Assigns the reference to the SmartUpdatableBrowser the base control
     *          is operating with
     * Notes:
     * @param poBrowser The SmartUpdatableBrowser instance
     */
    METHOD PROTECTED VOID SetUpdatabelBrowser (poBrowser AS SmartUpdatableBrowser):

        ASSIGN smartUpdatableBrowser = poBrowser .

    END METHOD.

END CLASS.
