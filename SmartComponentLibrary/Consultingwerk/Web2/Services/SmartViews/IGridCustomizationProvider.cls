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
    File        : IGridCustomizationProvider
    Purpose     : Interface for classes providing customization to Kendo UI
                  Grids
    Syntax      :
    Description : Invoked by the IGridLayoutProvider service
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 08 13:41:24 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.Web.Kendo.Grid.* FROM PROPATH .
USING Progress.Json.ObjectModel.*     FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .

INTERFACE Consultingwerk.Web2.Services.SmartViews.IGridCustomizationProvider:

    /**
     * Purpose: Customizes the resulting JSON defition
     * Notes:
     * @param pcEntityName The name of the Business Entity to return a view spec form
     * @param pcViewName The name of the view
     * @param poJsonObject The JSON Object with the Kendo UI definition to customize
     */
    METHOD PUBLIC VOID CustomizeGridPostSerialization (pcEntityName AS CHARACTER,
                                                       pcViewName AS CHARACTER,
                                                       poJsonObject AS JsonObject).

    /**
     * Purpose: Customizes the ABL KendoGrid definition
     * Notes:
     * @param pcEntityName The name of the Business Entity to return a view spec form
     * @param pcViewName The name of the view
     * @param poKendoGrid The KendoGrid structure to customize
     */
    METHOD PUBLIC VOID CustomizeGridPreSerialization (pcEntityName AS CHARACTER,
                                                      pcViewName AS CHARACTER,
                                                      poKendoGrid AS KendoGrid).

END INTERFACE.
