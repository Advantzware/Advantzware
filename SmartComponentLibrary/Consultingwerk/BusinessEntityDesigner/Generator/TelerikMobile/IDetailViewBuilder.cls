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
    File        : IDetailViewBuilder
    Purpose     : Interface for Builders of Telerik Mobile Detail View's
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon May 04 10:31:23 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.BusinessEntityDesigner.Generator.TelerikMobile.* FROM PROPATH .
USING Progress.Lang.*                                                 FROM PROPATH .

INTERFACE Consultingwerk.BusinessEntityDesigner.Generator.TelerikMobile.IDetailViewBuilder:

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the ID of the list view
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ListViewID AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and set the name of the view template
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TemplateFileName AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and set the ID of the View
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ViewID AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the referenced ViewModel
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ViewModel AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the title of the view
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ViewTitle AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Adds a Columnn to the Grid definition
        Notes:
        @param pcColumnName The name of the column to add
        @param pcColumnDataType The ABL data type for the colum
        @param pcColumnLabel The label for the column
        @return The new Column instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Column AddColumn (pcColumnName AS CHARACTER,
                                    pcColumnDataType AS CHARACTER,
                                    pcColumnLabel AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns the source code for the the View Columns
        Notes:
        @return The html5 string describing the view columns
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR GetViewColumns ().

END INTERFACE.
