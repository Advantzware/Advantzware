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
    File        : ResourceWebHandler
    Purpose     : WebHandler for CRUD operations
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Mon May 30 12:57:29 CEST 2016
    Notes       : Compilation requires %DLC%/{gui|tty}/netlib/OpenEdge.Net.pl
                  in propath and PacificWebSpeed setting in products.i
  ----------------------------------------------------------------------*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                          FROM PROPATH.
USING Consultingwerk.Framework.Collections.*              FROM PROPATH.
USING Consultingwerk.OERA.*                               FROM PROPATH.
USING Consultingwerk.OERA.JsdoGenericService.*            FROM PROPATH.
USING Consultingwerk.OERA.JsdoGenericService.WebHandler.* FROM PROPATH.
USING Consultingwerk.Util.*                               FROM PROPATH.
USING OpenEdge.Net.HTTP.*                                 FROM PROPATH.
USING OpenEdge.Web.*                                      FROM PROPATH.
USING Progress.Json.ObjectModel.*                         FROM PROPATH.

{Consultingwerk/products.i}

CLASS Consultingwerk.OERA.JsdoGenericService.WebHandler.ResourceWebHandler
    INHERITS SmartWebHandler:

&IF DEFINED (PacificWebSpeed) NE 0 &THEN

    /*------------------------------------------------------------------------------
        Purpose: Default handler for the HTTP DELETE method. The request being
                 serviced and an optional status code is returned. A zero or
                 null value means this method will deal with all errors.
        Notes:
        @param poRequest The IWebRequest instance representing the call
        @return StatusCode of the response sent to the client
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED INTEGER HandleDelete (poRequest AS IWebRequest):

        DEFINE VARIABLE oResponse   AS IHttpResponse NO-UNDO.

        DEFINE VARIABLE oService    AS Service       NO-UNDO.
        DEFINE VARIABLE cEntityName AS CHARACTER     NO-UNDO.

        ASSIGN
            oResponse             = NEW WebResponse ()
            /* HTTP messages require a content type */
            oResponse:ContentType = 'application/json':U
            .

        DO ON ERROR UNDO, THROW:
            ASSIGN
                cEntityName      = poRequest:GetPathParameter ("EntityName":U)
                .

            ObjectAssert:TypeOf(poRequest:Entity,
                                "Progress.Json.ObjectModel.JsonObject":U,
                                "Request Body"{&TRAN}).

            oService = {Consultingwerk/get-service.i Consultingwerk.OERA.JsdoGenericService.Service
                                                     "NEW Consultingwerk.OERA.JsdoGenericService.Service ()"}.

            oService:WebRequest = poRequest .

            oResponse:Entity = oService:ProcessUpdate (cEntityName, CAST (poRequest:Entity, JsonObject)) .

            oResponse:StatusCode = INTEGER (StatusCodeEnum:OK).

            THIS-OBJECT:WriteResponse(oResponse).

            CATCH ple AS Progress.Lang.Error:
                THIS-OBJECT:HandleError (ple,
                                         oResponse).

                RETURN 500 .
            END CATCH.

            FINALLY:
                IF VALID-OBJECT (oService) THEN
                    ASSIGN oService:WebRequest = ? .
            END FINALLY.
        END.

        /* A response of 0 means that this handler will build the entire response;
           a non-zero value is mapped to a static handler in the webapp's /static/error folder.
           The mappings are maintained in the webapps's WEB-INF/web.xml
           A predefined set of HTTP status codes is provided in the OpenEdge.Net.HTTP.StatusCodeEnum
           enumeration */
        RETURN 0.

        CATCH ple AS Progress.Lang.Error:
            THIS-OBJECT:HandleError (ple,
                                     oResponse).

            RETURN 0 .
        END CATCH.

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

        DEFINE VARIABLE oResponse        AS IHttpResponse       NO-UNDO.

        DEFINE VARIABLE oParamDictionary AS CharacterDictionary NO-UNDO.
        DEFINE VARIABLE oService         AS Service             NO-UNDO.
        DEFINE VARIABLE cEntityName      AS CHARACTER           NO-UNDO.
        DEFINE VARIABLE cFilter          AS CHARACTER           NO-UNDO.
        DEFINE VARIABLE cNumRecords      AS CHARACTER           NO-UNDO INITIAL ? .
        DEFINE VARIABLE cStopAfter       AS CHARACTER           NO-UNDO INITIAL ? .
        DEFINE VARIABLE hDataset         AS HANDLE              NO-UNDO.

        ASSIGN
            oResponse             = NEW WebResponse ()
            /* HTTP messages require a content type */
            oResponse:ContentType = 'application/json':U
            .

        DO ON ERROR UNDO, THROW:
            ASSIGN
                cEntityName      = poRequest:GetPathParameter ("EntityName":U)
                oParamDictionary = ListHelper:AlternatingListToDictionary (STRING (poRequest:GetContextValue ("QUERY_STRING":U)),
                                                                           "&":U, "=":U)
                .

            IF oParamDictionary:ContainsKey ("filter":U) THEN
                cFilter = WebHelper:DecodeURL (oParamDictionary:GetValue ("filter":U)).

            IF oParamDictionary:ContainsKey ("numRecords":U) THEN
                cNumRecords = WebHelper:DecodeURL (oParamDictionary:GetValue ("numRecords":U)).

            IF oParamDictionary:ContainsKey ("stopAfter":U) THEN
                cStopAfter = WebHelper:DecodeURL (oParamDictionary:GetValue ("stopAfter":U)).

            oService = {Consultingwerk/get-service.i Consultingwerk.OERA.JsdoGenericService.Service
                                                     "NEW Consultingwerk.OERA.JsdoGenericService.Service ()"}.

            oService:WebRequest = poRequest .

            oService:GetData (cEntityName, cFilter, cNumRecords, cStopAfter, OUTPUT DATASET-HANDLE hDataset).

            oResponse:Entity = JsonHelper:FromDataset(hDataset).

            oResponse:StatusCode = INTEGER (StatusCodeEnum:OK).

            THIS-OBJECT:WriteResponse(oResponse).

            CATCH ple AS Progress.Lang.Error:
                THIS-OBJECT:HandleError (ple,
                                         oResponse).
            END CATCH.

            FINALLY:
                IF VALID-OBJECT (oService) THEN
                    ASSIGN oService:WebRequest = ? .
            END FINALLY.
        END.


        /* A response of 0 means that this handler will build the entire response;
           a non-zero value is mapped to a static handler in the webapp's /static/error folder.
           The mappings are maintained in the webapps's WEB-INF/web.xml
           A predefined set of HTTP status codes is provided in the OpenEdge.Net.HTTP.StatusCodeEnum
           enumeration */
        RETURN 0.

        CATCH ple AS Progress.Lang.Error:
            THIS-OBJECT:HandleError (ple,
                                     oResponse).
            RETURN 0 .
        END CATCH.

        FINALLY:
            IF VALID-HANDLE (hDataset) THEN
                DELETE OBJECT hDataset .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Default handler for the HTTP POST method. The request being
                 serviced and an optional status code is returned. A zero or
                 null value means this method will deal with all errors.
        Notes:
        @param poRequest The IWebRequest instance representing the call
        @return StatusCode of the response sent to the client
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED INTEGER HandlePost (poRequest AS IWebRequest):

        DEFINE VARIABLE oResponse   AS IHttpResponse NO-UNDO.

        DEFINE VARIABLE oService    AS Service       NO-UNDO.
        DEFINE VARIABLE cEntityName AS CHARACTER     NO-UNDO.

        ASSIGN
            oResponse             = NEW WebResponse ()
            /* HTTP messages require a content type */
            oResponse:ContentType = 'application/json':U
            .

        DO ON ERROR UNDO, THROW:
            ASSIGN
                cEntityName      = poRequest:GetPathParameter ("EntityName":U)
                .

            ObjectAssert:TypeOf(poRequest:Entity,
                                "Progress.Json.ObjectModel.JsonObject":U,
                                "Request Body"{&TRAN}).

            oService = {Consultingwerk/get-service.i Consultingwerk.OERA.JsdoGenericService.Service
                                                     "NEW Consultingwerk.OERA.JsdoGenericService.Service ()"}.

            oService:WebRequest = poRequest .

            oResponse:Entity = oService:ProcessUpdate (cEntityName, CAST (poRequest:Entity, JsonObject)) .

            oResponse:StatusCode = INTEGER (StatusCodeEnum:OK).

            THIS-OBJECT:WriteResponse(oResponse).

            CATCH ple AS Progress.Lang.Error:
                THIS-OBJECT:HandleError (ple,
                                         oResponse).
            END CATCH.

            FINALLY:
                IF VALID-OBJECT (oService) THEN
                    ASSIGN oService:WebRequest = ? .
            END FINALLY.
        END.


        /* A response of 0 means that this handler will build the entire response;
           a non-zero value is mapped to a static handler in the webapp's /static/error folder.
           The mappings are maintained in the webapps's WEB-INF/web.xml
           A predefined set of HTTP status codes is provided in the OpenEdge.Net.HTTP.StatusCodeEnum
           enumeration */
        RETURN 0.

        CATCH ple AS Progress.Lang.Error:
            THIS-OBJECT:HandleError (ple,
                                     oResponse).

            RETURN 0 .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Default handler for the HTTP PUT method. The request being
                 serviced and an optional status code is returned. A zero or
                 null value means this method will deal with all errors.
        Notes:
        @param poRequest The IWebRequest instance representing the call
        @return StatusCode of the response sent to the client
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED INTEGER HandlePut (poRequest AS IWebRequest):

        DEFINE VARIABLE oResponse   AS IHttpResponse NO-UNDO.

        DEFINE VARIABLE oService    AS Service       NO-UNDO.
        DEFINE VARIABLE cEntityName AS CHARACTER     NO-UNDO.

        ASSIGN
            oResponse             = NEW WebResponse ()
            /* HTTP messages require a content type */
            oResponse:ContentType = 'application/json':U
            .

        DO ON ERROR UNDO, THROW:
            ASSIGN
                cEntityName      = poRequest:GetPathParameter ("EntityName":U)
                .

            ObjectAssert:TypeOf(poRequest:Entity,
                                "Progress.Json.ObjectModel.JsonObject":U,
                                "Request Body"{&TRAN}).

            oService = {Consultingwerk/get-service.i Consultingwerk.OERA.JsdoGenericService.Service
                                                     "NEW Consultingwerk.OERA.JsdoGenericService.Service ()"}.

            oService:WebRequest = poRequest .

            oResponse:Entity = oService:ProcessUpdate (cEntityName, CAST (poRequest:Entity, JsonObject)) .

            oResponse:StatusCode = INTEGER (StatusCodeEnum:OK).

            THIS-OBJECT:WriteResponse(oResponse).

            CATCH ple AS Progress.Lang.Error:
                THIS-OBJECT:HandleError (ple,
                                         oResponse).

                RETURN 500 .
            END CATCH.

            FINALLY:
                IF VALID-OBJECT (oService) THEN
                    ASSIGN oService:WebRequest = ? .
            END FINALLY.
        END.


        /* A response of 0 means that this handler will build the entire response;
           a non-zero value is mapped to a static handler in the webapp's /static/error folder.
           The mappings are maintained in the webapps's WEB-INF/web.xml
           A predefined set of HTTP status codes is provided in the OpenEdge.Net.HTTP.StatusCodeEnum
           enumeration */
        RETURN 0.

        CATCH ple AS Progress.Lang.Error:
            THIS-OBJECT:HandleError (ple,
                                     oResponse).

            RETURN 0 .
        END CATCH.

    END METHOD.
&ENDIF

END CLASS.