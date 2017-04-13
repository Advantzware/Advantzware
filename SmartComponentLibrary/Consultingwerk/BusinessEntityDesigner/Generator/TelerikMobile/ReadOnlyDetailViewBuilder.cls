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
    File        : ReadOnlyDetailViewBuilder
    Purpose     : Builder for an read-only detail view
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Oct 17 16:44:00 CET 2015
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.Generator.TelerikMobile.* FROM PROPATH .
USING Progress.Lang.*                                                 FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.TelerikMobile.ReadOnlyDetailViewBuilder
    INHERITS BaseDetailViewBuilder:

    /*------------------------------------------------------------------------------
        Purpose: Returns the source code for the the View Columns
        Notes:
        @return The html5 string describing the view columns
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC LONGCHAR GetViewColumns ():

        DEFINE VARIABLE lcView AS LONGCHAR NO-UNDO .

        {Consultingwerk/foreachABL.i Column oColumn in THIS-OBJECT:Columns}

            ASSIGN lcView = lcView + SUBSTITUTE ('            <li>~n':U +
                                                 '                <label>&2:</label>~n':U +
                                                 '                <span data-bind="text: &1" style="float: right"></span>~n':U +
                                                 '            </li>~n':U,
                                                 oColumn:ColumnName,
                                                 oColumn:ColumnLabel) .
        END.

        RETURN lcView .

    END METHOD .

END CLASS.
