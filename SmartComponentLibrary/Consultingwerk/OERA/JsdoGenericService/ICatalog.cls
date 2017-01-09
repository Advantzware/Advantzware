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
    File        : ICatalog
    Purpose     : Interface for classes that can generate the JSON/JSDO
                  Catalog file dynamically
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue May 19 11:53:54 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.*                    FROM PROPATH .
USING Consultingwerk.OERA.JsdoGenericService.* FROM PROPATH .
USING Progress.Json.ObjectModel.*              FROM PROPATH .
USING Progress.Lang.*                          FROM PROPATH .

INTERFACE Consultingwerk.OERA.JsdoGenericService.ICatalog:

    /**
     * Purpose: Returns the Catalog for a given Business Entity
     * Notes:   Defaults to the count method type of the .restapplicationsettings file
     * @param pcEntityName The name of the Business Entity
     * @param poAddress The RestServiceAddress to include in the catalog output
     * @return The JSON Object describing the Catalog for the Business Entity
     */
    METHOD PUBLIC JsonObject GetCatalogForBusinessEntity (pcEntityName AS CHARACTER,
                                                          poAddress AS RestServiceAddress) .

    /**
     * Purpose: Returns the Catalog for a given Business Entity
     * Notes:
     * @param pcEntityName The name of the Business Entity
     * @param poAddress The RestServiceAddress to include in the catalog output
     * @param pcCountMethodType The type (count/invoke) for the count method
     * @return The JSON Object describing the Catalog for the Business Entity
     */
    METHOD PUBLIC JsonObject GetCatalogForBusinessEntity (pcEntityName AS CHARACTER,
                                                          poAddress AS RestServiceAddress,
                                                          pcCountMethodType AS CHARACTER) .

    /**
     * Purpose: Returns the Catalog for a given Business Entity
     * Notes:
     * @param pcEntityName The name of the Business Entity
     * @param poAddress The RestServiceAddress to include in the catalog output
     * @param pcJsonCatalog OUTPUT The JSON Catalog for the Business Entity
     */
    METHOD PUBLIC VOID GetCatalogForBusinessEntity (pcEntityName AS CHARACTER,
                                                    poAddress AS RestServiceAddress,
                                                    OUTPUT pcJsonCatalog AS LONGCHAR) .

    /**
     * Purpose: Returns the Catalog for a given Business Task
     * Notes:
     * @param poBusinessTask The name of the Business Task
     * @param poAddress The RestServiceAddress to include in the catalog output
     * @return The JSON Object describing the Catalog for the Business Task
     */
    METHOD PUBLIC JsonObject GetCatalogForBusinessTask (poBusinessTask AS IBusinessTaskCatalogData,
                                                        poAddress AS RestServiceAddress) .

END INTERFACE.
