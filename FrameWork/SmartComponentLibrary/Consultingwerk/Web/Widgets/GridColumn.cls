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
    File        : GridColumn
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Apr 04 23:57:09 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Web.*              FROM PROPATH .
USING Consultingwerk.Web.Widgets.*      FROM PROPATH .
USING Consultingwerk.Web.Widgets.Grid.* FROM PROPATH .
USING Progress.Lang.*                   FROM PROPATH .

CLASS Consultingwerk.Web.Widgets.GridColumn
    INHERITS Component:

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the handle of the Buffer Field this GridColumn is bound to
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BufferField AS HANDLE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the column label
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ColumnLabel AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the sorting type
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ColumnSorting AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the column type
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ColumnType AS CHARACTER NO-UNDO INIT "ro":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the data type
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DataType AS CHARACTER NO-UNDO INIT "CHARACTER":U
    GET.
    SET (pcValue AS CHARACTER):

        CASE pcValue:
            WHEN "INTEGER":U THEN
                ASSIGN THIS-OBJECT:ColumnSorting = "int":U.
            OTHERWISE
                ASSIGN THIS-OBJECT:ColumnSorting = "str":U.
        END CASE .

    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the column should be rendered with the filter
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Filterable AS LOGICAL INITIAL TRUE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets the List of Filters for this GridColumn
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Filters AS Consultingwerk.Web.Widgets.Grid.ListGridFilter NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the column format
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Format AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the column Name
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Name AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Character that will be used as the screen value
                 when no record is available
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY NotAvailableValue AS CHARACTER INITIAL "?":U NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current screen value of the GridColumn
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ScreenValue AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the column should be rendered as sortable
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Sortable AS LOGICAL INITIAL TRUE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current left or right Alignment
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ColumnAlign AS CHARACTER NO-UNDO INIT "left":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the width of the GridColumn
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Width AS CHARACTER NO-UNDO INIT "150":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the GridColumn class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GridColumn ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the GridColumn class
        Notes:
        @param phBufferField The handle of the buffer field to bind to
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GridColumn (phBufferField AS HANDLE):
        SUPER ().

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phBufferField ""BUFFER-FIELD"":U}.

        ASSIGN THIS-OBJECT:BufferField = phBufferField
               THIS-OBJECT:DataType    = phBufferField:DATA-TYPE
               THIS-OBJECT:Format      = phBufferField:FORMAT
               THIS-OBJECT:ColumnLabel = phBufferField:COLUMN-LABEL
               THIS-OBJECT:Name        = phBufferField:NAME
               .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Outputs a cell for this column
        Notes:
        @return The output value for the cell
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR OutputCell ():

        RETURN TRIM (THIS-OBJECT:ScreenValue).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Performs the DataBinding, that is populating the ScreenValue with the
                 current value of the BufferField
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PerformBinding ():

        IF NOT VALID-HANDLE (THIS-OBJECT:BufferField) THEN
            RETURN .

        IF THIS-OBJECT:BufferField:BUFFER-HANDLE:AVAILABLE THEN DO:
            IF THIS-OBJECT:Format > "":U THEN
                THIS-OBJECT:ScreenValue = STRING (THIS-OBJECT:BufferField:BUFFER-VALUE,
                                                  THIS-OBJECT:Format) .
            ELSE
                THIS-OBJECT:ScreenValue = STRING (THIS-OBJECT:BufferField:BUFFER-VALUE) .
        END.
        ELSE
            THIS-OBJECT:ScreenValue = THIS-OBJECT:NotAvailableValue .

    END METHOD.

END CLASS.
