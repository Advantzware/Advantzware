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
    File        : CatalogWebHandler
    Purpose     : WebHandler for Catalog requests
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Fri May 27 16:33:19 CEST 2016
    Notes       : Compilation requires %DLC%/{gui|tty}/netlib/OpenEdge.Net.pl
                  in propath and PacificWebSpeed setting in products.i
  ----------------------------------------------------------------------*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                          FROM PROPATH .
USING Consultingwerk.Exceptions.*                         FROM PROPATH .
USING Consultingwerk.Framework.*                          FROM PROPATH .
USING Consultingwerk.Framework.Collections.*              FROM PROPATH .
USING Consultingwerk.Framework.Session.*                  FROM PROPATH .
USING Consultingwerk.OERA.*                               FROM PROPATH .
USING Consultingwerk.OERA.Exceptions.*                    FROM PROPATH .
USING Consultingwerk.OERA.JsdoGenericService.*            FROM PROPATH .
USING Consultingwerk.OERA.JsdoGenericService.WebHandler.* FROM PROPATH .
USING Consultingwerk.Util.*                               FROM PROPATH .
USING OpenEdge.Net.HTTP.*                                 FROM PROPATH .
USING OpenEdge.Web.*                                      FROM PROPATH .
USING Progress.Json.ObjectModel.*                         FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.OERA.JsdoGenericService.WebHandler.CatalogWebHandler
    INHERITS SmartWebHandler:

&IF DEFINED (PacificWebSpeed) NE 0 &THEN

    DEFINE VARIABLE oCache AS ICatalogCacheService NO-UNDO .

    /**
     * Purpose: Constructor for the CatalogWebHandler class
     * Notes:
     */
    CONSTRUCTOR PUBLIC CatalogWebHandler ():

        // Short type name is o.k. here, as PacificWebSpeed is 11.6 +
        ASSIGN oCache = {Consultingwerk/get-service.i ICatalogCacheService} .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the RestServiceAddress from the restapplicationsettings file
        Notes:
        @param pcEntityName The BusinessEntity name of the requested catalog
        @return The RestServiceAddress
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC RestServiceAddress GetRestServiceAddress (pcEntityName AS CHARACTER):

        DEFINE VARIABLE cFileName        AS CHARACTER                   NO-UNDO.
        DEFINE VARIABLE oConfig          AS IConfigurationProvider      NO-UNDO.
        DEFINE VARIABLE oAddressProvider AS IRestServiceAddressProvider NO-UNDO.

        FileAssert:Exists (".restapplicationsettings":U) .

        ASSIGN cFileName = FileHelper:FindFile (".restapplicationsettings":U) .

        ASSIGN oConfig = {Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider
                                                       "NEW ConfigurationProvider (cFileName)"} .


        oAddressProvider = {Consultingwerk/get-service.i Consultingwerk.OERA.JsdoGenericService.IRestServiceAddressProvider
                                                         "NEW Consultingwerk.OERA.JsdoGenericService.RestServiceAddressProvider()"} .

        RETURN oAddressProvider:GetRestServiceAddress (pcEntityName,
                                                       oConfig:GetValue ("RestServiceName":U),
                                                       oConfig:GetValue ("RestServiceAddress":U)).
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Default handler for the HTTP GET method. The request being
                 serviced and an optional status code is returned. A zero or
                 null value means this method will deal with all errors.
        Notes:
        @param poRequest The IWebRequest instance representing the call
        @return StatusCode of the response sent to the client
    ------------------------------------------------------------------------------*/
     METHOD OVERRIDE PROTECTED INTEGER HandleGet (poRequest AS IWebRequest):

        DEFINE VARIABLE oResponse        AS IHttpResponse       NO-UNDO .
        DEFINE VARIABLE cEntityName      AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE oCatalog         AS ICatalog            NO-UNDO .
        DEFINE VARIABLE oService         AS IBusinessService    NO-UNDO .
        DEFINE VARIABLE cCountMethodType AS CHARACTER           NO-UNDO INIT ? .
        DEFINE VARIABLE oParamDictionary AS CharacterDictionary NO-UNDO .
        DEFINE VARIABLE oCached          AS JsonObject          NO-UNDO .
        DEFINE VARIABLE cCacheParameter  AS CHARACTER           NO-UNDO .

        ServiceInterface:Activate () .

        ASSIGN
            oResponse             = NEW WebResponse ()
            /* HTTP messages require a content type */
            oResponse:ContentType = 'application/json':U
            .

        DO ON ERROR UNDO, THROW:
            ASSIGN cEntityName      = poRequest:GetPathParameter ("EntityName":U)
                   oParamDictionary = ListHelper:AlternatingListToDictionary (STRING (poRequest:GetContextValue ("QUERY_STRING":U)),
                                                                           "&":U, "=":U) .

            IF oParamDictionary:ContainsKey("count":U) THEN
                ASSIGN cCountMethodType = oParamDictionary:GetValue ("count":U)
                       cCacheParameter  = IF cCountMethodType > "":U THEN
                                              SUBSTITUTE ("count=&1":U, cCountMethodType)
                                          ELSE
                                              "":U.

            ASSIGN cCacheParameter = TRIM (SUBSTITUTE ("&1 &2":U,
                                                       cCacheParameter,
                                                       SessionManager:LanguageIsoCode)) .

            IF VALID-OBJECT (oCache) THEN DO:
                oCached = oCache:GetCachedCatalog (cEntityName, cCacheParameter) .

                IF VALID-OBJECT (oCached) THEN
                    ASSIGN oResponse:Entity = oCached .
            END.

            IF NOT VALID-OBJECT (oCached) THEN DO:
                ASSIGN
                    oService = ServiceManager:GetBusinessService (cEntityName) .
                    oCatalog = {Consultingwerk/get-service.i Consultingwerk.OERA.JsdoGenericService.ICatalog
                                                             "NEW CatalogV14 ()"} .

                IF VALID-OBJECT (oService) THEN DO:

                    IF TYPE-OF (oService, IBusinessTaskCatalogInfo) THEN
                        oResponse:Entity = oCatalog:GetCatalogForBusinessTask (CAST (oService, IBusinessTaskCatalogInfo):GetCatalogData(),
                                                                               CatalogWebhandler:GetRestServiceAddress (cEntityName)) .
                    ELSE IF TYPE-OF (oService, IBusinessEntity) THEN
                        oResponse:Entity = oCatalog:GetCatalogForBusinessEntity (cEntityName,
                                                                                 CatalogWebhandler:GetRestServiceAddress (cEntityName),
                                                                                 cCountMethodType) .
                    ELSE
                        UNDO, THROW NEW CatalogException ("Business Service must be IBusinessEntity or IBusinessTaskCatalogInfo to generate Catalog.":U,
                                                          0,
                                                          cEntityName) .

                    IF VALID-OBJECT (oCache) THEN
                        oCache:StoreCatalogInCache (cEntityName,
                                                    cCacheParameter,
                                                    CAST (oResponse:Entity, JsonObject)) .
                END .
                ELSE
                    UNDO, THROW NEW CatalogException ("Invalid Business Service instance.":U,
                                                      0,
                                                      cEntityName) .
            END.

            oResponse:StatusCode = INTEGER (StatusCodeEnum:OK).

            THIS-OBJECT:WriteResponse (oResponse).

            CATCH ple AS Progress.Lang.Error:
                THIS-OBJECT:HandleError (ple,
                                         oResponse).

                RETURN 500 .
            END CATCH.
        END.

        /* A response of 0 means that this handler will build the entire response;
           a non-zero value is mapped to a static handler in the webapp's /static/error folder.
           The mappings are maintained in the webapps's WEB-INF/web.xml
           A predefined set of HTTP status codes is provided in the OpenEdge.Net.HTTP.StatusCodeEnum
           enumeration */
        RETURN 0.

        CATCH ple AS Progress.Lang.Error :
            THIS-OBJECT:HandleError (ple,
                                     oResponse).

            RETURN 0 .
        END CATCH.

        FINALLY:
            ServiceInterface:Deactivate () .
        END FINALLY.

     END METHOD.
&ENDIF

END CLASS.