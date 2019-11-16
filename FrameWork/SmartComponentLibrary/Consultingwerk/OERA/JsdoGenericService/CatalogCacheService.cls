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
    File        : CatalogCacheService
    Purpose     : Services providing cache for Business Entity
                  and Business Task Catalog's
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jan 17 09:27:05 CET 2017
    Notes       : This is an optional service. When loaded, the CatalogWebHandler
                  will query the cache first for a Catalog JSON object
                  The "JsonCatalogCacheMaxAge" setting in the .restapplicationsetting
                  configuration file controls the maximum age of the cache
                  in seconds
                  Supports the CatalogCacheService custom log entry type
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*               FROM PROPATH .
USING Consultingwerk.OERA.JsdoGenericService.* FROM PROPATH .
USING Consultingwerk.Util.*                    FROM PROPATH .
USING Progress.Json.ObjectModel.*              FROM PROPATH .
USING Progress.Lang.*                          FROM PROPATH .

CLASS Consultingwerk.OERA.JsdoGenericService.CatalogCacheService
    IMPLEMENTS ICatalogCacheService:

    DEFINE VARIABLE iMaxCacheAge AS INTEGER     NO-UNDO INITIAL 0 .
    DEFINE VARIABLE lLogging     AS LOGICAL     NO-UNDO INIT FALSE .

    DEFINE TEMP-TABLE ttCache NO-UNDO
        FIELD BusinessServiceName AS CHARACTER
        FIELD Parameters          AS CHARACTER
        FIELD CacheTimeStamp      AS DATETIME-TZ
        FIELD CatalogData         AS Progress.Lang.Object
        INDEX BusinessServiceName IS UNIQUE BusinessServiceName Parameters .

    /**
     * Purpose: Constructor for the CatalogCacheService class
     * Notes:
     */
    CONSTRUCTOR PUBLIC CatalogCacheService ():

        DEFINE VARIABLE oConfigurationProvider AS IConfigurationProvider NO-UNDO .

        oConfigurationProvider = {Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider} .

        IF VALID-OBJECT (oConfigurationProvider) THEN DO:

            iMaxCacheAge = DataTypeHelper:ToInteger (oConfigurationProvider:GetValue ("JsonCatalogCacheMaxAge":U)) .

            IF iMaxCacheAge = ? THEN
                ASSIGN iMaxCacheAge = 0 .
        END.

        ASSIGN lLogging = LogManager:IsCustomLogEntryTypeActive ("CatalogCacheService":U) .

    END CONSTRUCTOR.

    /**
     * Purpose: Returns the Cached Catalog Data
     * Notes:   Returns ? when no cached catalog is present
     * @param pcBusinessServiceName The name of the cached business service
     * @param pcParameters Additional parameters for the Catalog (e.g. count=invoke)
     * @return The JsonObject with the cached catalog data
     */
    METHOD PUBLIC JsonObject GetCachedCatalog (pcBusinessServiceName AS CHARACTER,
                                               pcParameters AS CHARACTER):

        DEFINE VARIABLE iAge AS INTEGER NO-UNDO.

        IF NOT CAN-FIND (ttCache WHERE ttCache.BusinessServiceName = pcBusinessServiceName
                                   AND ttCache.Parameters          = pcParameters) THEN DO:
            IF lLogging THEN
                LogManager:WriteFormattedMessage ("[CatalogCacheService] Catalog for Business Service &1&2 not found in cache.":U,
                                                  pcBusinessServiceName,
                                                  RIGHT-TRIM (" ":U + pcParameters),
                                                  NEW MessageSubSystem ("CatalogCacheService":U)) .
            RETURN ? .
        END .

        FIND ttCache WHERE ttCache.BusinessServiceName = pcBusinessServiceName
                       AND ttCache.Parameters          = pcParameters .

        /* Check cache age */
        IF iMaxCacheAge > 0 THEN DO:

            ASSIGN iAge = INTERVAL (NOW, ttCache.CacheTimeStamp, "seconds":U) .

            IF iAge > iMaxCacheAge THEN DO:
                IF lLogging THEN
                    LogManager:WriteFormattedMessage ("[CatalogCacheService] Removing outdated Catalog cache for Business Service &1&2 from cache (cache age: &3 seconds, max age: &4 seconds).":U,
                                                      pcBusinessServiceName,
                                                      RIGHT-TRIM (" ":U + pcParameters),
                                                      STRING (iAge),
                                                      STRING (iMaxCacheAge),
                                                      NEW MessageSubSystem ("CatalogCacheService":U)) .
                DELETE ttCache .
                RETURN ? .
            END.
        END.

        IF lLogging THEN
            LogManager:WriteFormattedMessage ("[CatalogCacheService] Returning cached Catalog for Business Service &1&2":U,
                                              pcBusinessServiceName,
                                              RIGHT-TRIM (" ":U + pcParameters),
                                              NEW MessageSubSystem ("CatalogCacheService":U)) .

        RETURN CAST (ttCache.CatalogData, JsonObject) .

    END METHOD .

    /**
     * Purpose: Stores the catalog data in the cache
     * Notes:
     * @param pcBusinessServiceName The name of the cached business service
     * @param pcParameters Additional parameters for the Catalog (e.g. count=invoke)
     * @param poJsonObject The JsonObject with the cached catalog data
     */
    METHOD PUBLIC VOID StoreCatalogInCache (pcBusinessServiceName AS CHARACTER,
                                            pcParameters AS CHARACTER,
                                            poJsonObject AS JsonObject):

        IF NOT CAN-FIND (ttCache WHERE ttCache.BusinessServiceName = pcBusinessServiceName
                                   AND ttCache.Parameters          = pcParameters) THEN DO:
            IF lLogging THEN
                LogManager:WriteFormattedMessage ("[CatalogCacheService] Storing Catalog for Business Service &1&2 in cache.":U,
                                                  pcBusinessServiceName,
                                                  RIGHT-TRIM (" ":U + pcParameters),
                                                  NEW MessageSubSystem ("CatalogCacheService":U)) .
            CREATE ttCache.
            ASSIGN ttCache.BusinessServiceName = pcBusinessServiceName
                   ttCache.Parameters          = pcParameters
                   ttCache.CacheTimeStamp      = NOW
                   ttCache.CatalogData         = poJsonObject .

            RETURN .
        END .

        FIND ttCache WHERE ttCache.BusinessServiceName = pcBusinessServiceName
                       AND ttCache.Parameters          = pcParameters .

        IF lLogging THEN
            LogManager:WriteFormattedMessage ("[CatalogCacheService] Updating Catalog for Business Service &1&2 in cache.":U,
                                              pcBusinessServiceName,
                                              RIGHT-TRIM (" ":U + pcParameters),
                                              NEW MessageSubSystem ("CatalogCacheService":U)) .

        ASSIGN ttCache.CacheTimeStamp      = NOW
               ttCache.CatalogData         = poJsonObject .

    END METHOD .

END CLASS.
