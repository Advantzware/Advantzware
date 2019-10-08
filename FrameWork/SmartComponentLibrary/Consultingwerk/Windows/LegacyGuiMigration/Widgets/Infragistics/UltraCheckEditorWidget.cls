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
    File        : UltraCheckEditorWidget
    Purpose     : Represents a toggle-box
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Sep 05 09:22:12 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Windows.LegacyGuiMigration.Widgets.*              FROM PROPATH .
USING Consultingwerk.Windows.LegacyGuiMigration.Widgets.Exceptions.*   FROM PROPATH .
USING Consultingwerk.Windows.LegacyGuiMigration.Widgets.Infragistics.* FROM PROPATH .
USING Infragistics.Win.UltraWinEditors.*                               FROM ASSEMBLY .
USING Progress.Lang.*                                                  FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Windows.LegacyGuiMigration.Widgets.Infragistics.UltraCheckEditorWidget
    INHERITS InfragisticsControlWidget
    IMPLEMENTS ILogicalInputValueWidget:

    DEFINE VARIABLE oControl AS UltraCheckEditor NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: The display state for a toggle box or a toggle-box menu item
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CHECKED AS LOGICAL NO-UNDO
    GET:
        IF oControl:CheckState = System.Windows.Forms.CheckState:Checked THEN
            RETURN TRUE.
        ELSE
            RETURN FALSE.
    END.
    SET(arg AS LOGICAL):
        IF arg THEN
            ASSIGN oControl:CheckState = System.Windows.Forms.CheckState:Checked .
        ELSE
            ASSIGN oControl:CheckState = System.Windows.Forms.CheckState:Unchecked .
    END.

    /*------------------------------------------------------------------------------
        Purpose: A character value that represents the data type of the field
                 associated with the widget
        Notes:   For example, the DATA-TYPE attribute of a slider widget always
                 returns the value "INTEGER" because slider widgets can only
                 represent integer values
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC OVERRIDE PROPERTY DATA-TYPE AS CHARACTER NO-UNDO INIT "LOGICAL":U
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Used for data-representation widgets, such as field-level widgets
                 that represent variables or database fields
        Notes:   The value for the INPUT-VALUE attribute is the unformatted
                 SCREEN-VALUE of a widget
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY INPUT-VALUE AS LOGICAL NO-UNDO
    GET:
        IF oControl:Checked THEN
            RETURN TRUE .
        ELSE
            RETURN FALSE  .
    END GET .
    SET (arg AS LOGICAL):
        IF arg THEN
            oControl:Checked = TRUE .
        ELSE
            oControl:Checked = FALSE .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: The type of a handle
        Notes:   TOGGLE-BOX
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC OVERRIDE PROPERTY TYPE AS CHARACTER NO-UNDO INIT "TOGGLE-BOX":U
    GET .
    PROTECTED SET .

    /*------------------------------------------------------------------------------
       Purpose: Constructor for the UltraCheckEditorWidget class
       Notes:
       @param poEditor The reference to the UltraCheckEditor this instance should represent
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UltraCheckEditorWidget (poEditor AS UltraCheckEditor):
        SUPER (poEditor).

        ASSIGN oControl           = poEditor
               THIS-OBJECT:FORMAT = "yes/no":U .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: GET method for the SCREEN-VALUE property
        Notes:
        @return Character value indicating the SCREEN-VALUE Value
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE CHARACTER GetSCREEN-VALUE ():

        IF oControl:Checked THEN
            RETURN ENTRY(1, THIS-OBJECT:FORMAT, "/":U) .
        ELSE
            RETURN ENTRY(2, THIS-OBJECT:FORMAT, "/":U) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: SET method for the FORMAT property
        Notes:
        @param pcValue The new value for the the FORMAT property
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID SetFORMAT (pcValue AS CHARACTER):

        IF NUM-ENTRIES (pcValue, "/":U) <> 2 THEN
            UNDO, THROW NEW UnsupportedWidgetAttributeValueException
                                ("The FORMAT of a TOGGLE-BOX must provide a TRUE and FALSE representation (yes/no)"{&TRAN},
                                 0,
                                 oControl,
                                 "FORMAT":U) .

        SUPER:SetFORMAT (pcValue) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: SET method for the SCREEN-VALUE property
        Notes:
        @param pcValue The new value for the the SCREEN-VALUE property
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID SetSCREEN-VALUE (pcValue AS CHARACTER):

        DEFINE VARIABLE lCheck AS LOGICAL    NO-UNDO.

        lCheck = (pcValue = ENTRY(1, THIS-OBJECT:Format, "/":U)) NO-ERROR .

        IF lCheck THEN
            ASSIGN oControl:Checked = TRUE
                   oControl:CheckState = System.Windows.Forms.CheckState:Checked.
        ELSE
            ASSIGN oControl:Checked = FALSE
                   oControl:CheckState = System.Windows.Forms.CheckState:Unchecked.

    END METHOD.

END CLASS.
