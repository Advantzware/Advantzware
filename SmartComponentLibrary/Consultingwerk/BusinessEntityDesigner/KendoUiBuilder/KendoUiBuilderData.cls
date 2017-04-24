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
    File        : KendoUiBuilderData
    Purpose     : The Parameter data for the KendoUiBuilderViewGenerator
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Oct 03 11:50:27 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.Generator.Viewer.* FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.*   FROM PROPATH .
USING Progress.Lang.*                                          FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.KendoUiBuilderData:

    /**
     * Purpose: Gets and sets the App location folder Name
     * Notes:
     */
    DEFINE PUBLIC PROPERTY AppLocation AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the Data Provider Name
     * Notes:
     */
    DEFINE PUBLIC PROPERTY DataProviderName AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the Name of the Data Source
     * Notes:
     */
    DEFINE PUBLIC PROPERTY DataSourceName AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the Entity Join
     * Notes:
     */
    DEFINE PUBLIC PROPERTY EntityJoin AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the Business Entity Name
     * Notes:
     */
    DEFINE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the Entity Table
     * Notes:
     */
    DEFINE PUBLIC PROPERTY EntityTable AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the Entity View Tables
     * Notes:
     */
    DEFINE PUBLIC PROPERTY EntityView AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the URI of the Kendo UI Builder
     * Notes:
     */
    DEFINE PUBLIC PROPERTY KendoUiBuilderUri AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the Module Name
     * Notes:
     */
    DEFINE PUBLIC PROPERTY ModuleName AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the View Name
     * Notes:
     */
    DEFINE PUBLIC PROPERTY ViewName AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the View Type
     * Notes:
     */
    DEFINE PUBLIC PROPERTY ViewType AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the list of Viewer Field Specs
     * Notes:
     */
    DEFINE PUBLIC PROPERTY ViewerFieldSpecs AS ListViewerFieldSpec NO-UNDO
    GET.
    SET.

END CLASS.
