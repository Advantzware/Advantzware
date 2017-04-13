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
    File        : IGridLayoutProvider
    Purpose     : Interface for services returning Kendo UI Grid definitions
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 08 13:16:18 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.Web2.Services.SmartViews.* FROM PROPATH .
USING Progress.Json.ObjectModel.*               FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

INTERFACE Consultingwerk.Web2.Services.SmartViews.IGridLayoutProvider:

    /**
     * Purpose: Returns the Json Object describing the Grid
     * Notes:
     * @param pcCustomViewName The name of the view
     * @return The JSON Object describing the Grid
     */
    METHOD PUBLIC JsonObject GetFragment (pcCustomViewName AS CHARACTER) .

    /**
     * Purpose: Returns the Json Object describing the Grid
     * Notes:
     * @param pcEntityName The name of the Business Entity to return a view spec form
     * @param pcViewName The name of the view
     * @return The JSON Object describing the Grid
     */
    METHOD PUBLIC JsonObject GetFragment (pcEntityName AS CHARACTER,
                                          pcViewName AS CHARACTER) .

    /**
     * Purpose: Registers an IGridCustomizationProvider
     * Notes:   Allows to customize a grid identified by the Business Entity Name and View Name
     * @param pcEntityNamePattern The name pattern (MATCHES) of Business Entities to customize
     * @param pcViewPattern The name pattern (MATCHES) of Views to customizat
     * @param poCustomizationProvider The customization provider instance
     * @return The IGridCustomizationProvider instance for fluent coding style
     */
    METHOD PUBLIC IGridCustomizationProvider RegisterCustomizationProvider (pcEntityNamePattern AS CHARACTER,
                                                                            pcViewPattern AS CHARACTER,
                                                                            poCustomizationProvider AS IGridCustomizationProvider) .

    /**
     * Purpose: Registers an IGridCustomProvider
     * Notes:   Allows to return a custom grid layout to the browser independently
     *          of a Business Entity Name nad View Name
     * @param pcCustomViewPattern The name pattern (MATCHES) of Views to customize
     * @param poCustomProvider The customization provider instance
     * @return The IGridCustomProvider instance for fluent coding style
     */
    METHOD PUBLIC IGridCustomProvider RegisterGridCustomProvider (pcCustomViewPattern AS CHARACTER,
                                                                  poCustomProvider AS IGridCustomProvider) .

END INTERFACE.
