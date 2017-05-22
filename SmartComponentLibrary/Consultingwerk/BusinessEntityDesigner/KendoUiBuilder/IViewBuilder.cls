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
    File        : IViewBuilder
    Purpose     : Interface for classes that support creating views
    Syntax      :
    Description : Used by the ViewGenerator:CreateView method
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Oct 03 07:55:59 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.Collections.*                   FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.Viewer.* FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.*   FROM PROPATH .
USING Consultingwerk.Web.KendoUiBuilder.Module.*               FROM PROPATH .
USING Progress.Lang.*                                          FROM PROPATH .

INTERFACE Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.IViewBuilder:

    {Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i}

    /**
     * Purpose: Creates a view with the provided specifications to the module
     * Notes:   Launches the specific view generator instance
     * @param poModule The module to add the new view to
     * @param dsBusinessEntity The dataset with the Business Entity Model
     * @param pcTables The entity table and view tables
     * @param pcViewType The type of the view to create
     * @param pcViewName The name of the view to create
     * @param pcDataProvider The name of the data provider
     * @param pcDataSource The name of the data source
     * @param poGridColumns The list with the column names to use for the grid
     * @param poViewerFields The list with the viewer field specs
     */
    METHOD PUBLIC VOID CreateView (poModule       AS Module,
                                   DATASET dsBusinessEntity,
                                   pcTables       AS CHARACTER,
                                   pcViewType     AS CHARACTER,
                                   pcViewName     AS CHARACTER,
                                   pcDataProvider AS CHARACTER,
                                   pcDataSource   AS CHARACTER,
                                   poGridColumns  AS CharacterList,
                                   poViewerFields AS ListViewerFieldSpec) .

END INTERFACE.
