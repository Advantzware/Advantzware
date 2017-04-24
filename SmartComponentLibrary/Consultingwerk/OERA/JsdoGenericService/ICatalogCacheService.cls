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
    File        : ICatalogCacheService
    Purpose     : Interface for services providing cache for Business Entity
                  and Business Task Catalog's
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jan 13 01:00:07 CET 2017
    Notes       : This is an optional service. When loaded, the CatalogWebHandler
                  will query the cache first for a Catalog JSON object
                  The "JsonCatalogCacheMaxAge" setting in the .restapplicationsetting
                  configuration file controls the maximum age of the cache
                  in seconds
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.JsdoGenericService.* FROM PROPATH .
USING Progress.Json.ObjectModel.*              FROM PROPATH .
USING Progress.Lang.*                          FROM PROPATH .

INTERFACE Consultingwerk.OERA.JsdoGenericService.ICatalogCacheService:

    /**
     * Purpose: Returns the Cached Catalog Data
     * Notes:   Returns ? when no cached catalog is present
     * @param pcBusinessServiceName The name of the cached business service
     * @param pcParameters Additional parameters for the Catalog (e.g. count=invoke)
     * @return The JsonObject with the cached catalog data
     */
    METHOD PUBLIC JsonObject GetCachedCatalog (pcBusinessServiceName AS CHARACTER,
                                               pcParameters AS CHARACTER).

    /**
     * Purpose: Stores the catalog data in the cache
     * Notes:
     * @param pcBusinessServiceName The name of the cached business service
     * @param pcParameters Additional parameters for the Catalog (e.g. count=invoke)
     * @param poJsonObject The JsonObject with the cached catalog data
     */
    METHOD PUBLIC VOID StoreCatalogInCache (pcBusinessServiceName AS CHARACTER,
                                            pcParameters AS CHARACTER,
                                            poJsonObject AS JsonObject) .

END INTERFACE.
