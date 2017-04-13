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
    File        : ResourceSubmitWebHandler
    Purpose     : WebHandler for CRUD operations
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jun 19 12:59:29 CEST 2016
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

CLASS Consultingwerk.OERA.JsdoGenericService.WebHandler.ResourceSubmitWebHandler
    INHERITS SmartWebHandler:

&IF DEFINED (PacificWebSpeed) NE 0 &THEN

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

            ObjectAssert:TypeOf (poRequest:Entity,
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

            RETURN 500 .
        END CATCH.

    END METHOD.
&ENDIF

END CLASS.