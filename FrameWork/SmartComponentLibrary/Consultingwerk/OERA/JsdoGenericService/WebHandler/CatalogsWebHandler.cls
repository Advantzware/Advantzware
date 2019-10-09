/*********************************************************************
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
    File        : BusinessServicesWebHandler
    Purpose     : WebHandler for BusinessServices requests
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Tue Nov 29 22:36:19 CEST 2016
    Notes       : Compilation requires %DLC%/{gui|tty}/netlib/OpenEdge.Net.pl
                  in propath and PacificWebSpeed setting in products.i
  ----------------------------------------------------------------------*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                    FROM PROPATH.
USING Consultingwerk.Assertion.*                          FROM PROPATH.
USING Consultingwerk.Framework.*                          FROM PROPATH.
USING Consultingwerk.Framework.Collections.*              FROM PROPATH .
USING Consultingwerk.Framework.Session.*                  FROM PROPATH .
USING Consultingwerk.OERA.*                               FROM PROPATH.
USING Consultingwerk.OERA.JsdoGenericService.*            FROM PROPATH.
USING Consultingwerk.OERA.JsdoGenericService.WebHandler.* FROM PROPATH.
USING Consultingwerk.Util.*                               FROM PROPATH.
USING OpenEdge.Lang.*                                     FROM PROPATH.
USING OpenEdge.Net.HTTP.*                                 FROM PROPATH.
USING OpenEdge.Web.*                                      FROM PROPATH.
USING Progress.Json.ObjectModel.*                         FROM PROPATH.
USING Progress.Lang.*                                     FROM PROPATH.

{Consultingwerk/products.i}

CLASS Consultingwerk.OERA.JsdoGenericService.WebHandler.CatalogsWebHandler
    INHERITS ListBusinessServicesWebHandler:

&IF DEFINED (PacificWebSpeed) NE 0 &THEN

    DEFINE VARIABLE oCache AS ICatalogCacheService NO-UNDO .

    /**
     * Purpose: Constructor for the CatalogsWebHandler class
     * Notes:
     */
    CONSTRUCTOR PUBLIC CatalogsWebHandler ():

        // Short type name is o.k. here, as PacificWebSpeed is 11.6 +
        ASSIGN oCache = {Consultingwerk/get-service.i ICatalogCacheService} .

    END CONSTRUCTOR.

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
        DEFINE VARIABLE cPackageName     AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE oCatalog         AS ICatalog            NO-UNDO .
        DEFINE VARIABLE oService         AS IBusinessService    NO-UNDO .
        DEFINE VARIABLE cCountMethodType AS CHARACTER           NO-UNDO INIT ? .
        DEFINE VARIABLE oParamDictionary AS CharacterDictionary NO-UNDO .
        DEFINE VARIABLE oJsonCatalog     AS JsonObject          NO-UNDO .
        DEFINE VARIABLE oJsonReturn      AS JsonObject          NO-UNDO .
        DEFINE VARIABLE oResources       AS JsonArray           NO-UNDO .
        DEFINE VARIABLE oCached          AS JsonObject          NO-UNDO .
        DEFINE VARIABLE oPackageCache    AS JsonObject          NO-UNDO .
        DEFINE VARIABLE cCacheParameter  AS CHARACTER           NO-UNDO .

        ServiceInterface:Activate () .

        ASSIGN
            oResponse             = NEW WebResponse ()
            /* HTTP messages require a content type */
            oResponse:ContentType = 'application/json':U
            .

        DO ON ERROR UNDO, THROW:
            ASSIGN cPackageName  = poRequest:GetPathParameter ("PackageName":U)
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

            IF VALID-OBJECT (oCache) THEN
                oPackageCache = oCache:GetCachedCatalog ("Package:":U + cPackageName, cCacheParameter) .

            IF VALID-OBJECT (oPackageCache) THEN
                oResponse:Entity = oPackageCache .
            ELSE DO:
                oCatalog = {Consultingwerk/get-service.i Consultingwerk.OERA.JsdoGenericService.ICatalog
                                                         "NEW CatalogV14 ()"} .

                THIS-OBJECT:GatherClassInformation (cPackageName).

                entryLoop:
                FOR EACH ttTableEntry ON ERROR UNDO, THROW:

                    IF VALID-OBJECT (oCache) THEN
                        oCached = oCache:GetCachedCatalog (ttTableEntry.BEName, cCacheParameter) .
                    ELSE
                        oCached = ? .

                    IF NOT VALID-OBJECT (oCached) THEN DO:
                        ASSIGN oService = ServiceManager:GetBusinessService (ttTableEntry.BEName) .

                        IF VALID-OBJECT (oService) THEN DO:

                            IF TYPE-OF (oService, IBusinessTaskCatalogInfo) THEN
                                oJsonCatalog = oCatalog:GetCatalogForBusinessTask (CAST (oService, IBusinessTaskCatalogInfo):GetCatalogData(),
                                                                                   CatalogWebHandler:GetRestServiceAddress (cPackageName)) .
                            ELSE IF TYPE-OF (oService, IBusinessEntity) THEN
                                oJsonCatalog = oCatalog:GetCatalogForBusinessEntity (ttTableEntry.BEName,
                                                                                     CatalogWebHandler:GetRestServiceAddress (cPackageName),
                                                                                     cCountMethodType) .
                            ELSE
                                NEXT entryLoop .
                        END .
                        ELSE
                            NEXT entryLoop .

                        IF VALID-OBJECT (oCache) THEN
                            oCache:StoreCatalogInCache (ttTableEntry.BEName,
                                                        cCacheParameter,
                                                        oJsonCatalog) .
                    END.
                    ELSE
                        oJsonCatalog = oCached .

                    IF NOT VALID-OBJECT (oJsonReturn) THEN
                        ASSIGN oJsonReturn = oJsonCatalog
                               oResources  = oJsonCatalog:GetJsonArray("services":U):GetJsonObject(1):GetJsonArray("resources":U) .
                    ELSE
                        oResources:Add (oJsonCatalog:GetJsonArray("services":U):GetJsonObject(1):GetJsonArray("resources":U):GetJsonObject(1)) .

                END.

                IF VALID-OBJECT (oCache) THEN
                    oCache:StoreCatalogInCache ("Package:":U + cPackageName,
                                                cCacheParameter,
                                                oJsonReturn) .

                oResponse:Entity = oJsonReturn .
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
            EMPTY TEMP-TABLE ttTableEntry .

            ServiceInterface:Deactivate () .
        END FINALLY.

     END METHOD.
&ENDIF

END CLASS.