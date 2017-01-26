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
    File        : SmartValueListWebHandler
    Purpose     : Web Handler which returns the entries of a SmartValueList
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jan 21 21:51:04 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.* FROM PROPATH.
USING Consultingwerk.Util.*                               FROM PROPATH .
USING Consultingwerk.Framework.*                          FROM PROPATH .
USING Consultingwerk.OERA.*                               FROM PROPATH .
USING Consultingwerk.OERA.JsdoGenericService.WebHandler.* FROM PROPATH .
USING Consultingwerk.Web2.WebHandler.*                    FROM PROPATH .
USING OpenEdge.Net.HTTP.*                                 FROM PROPATH .
USING Progress.Json.ObjectModel.*                         FROM PROPATH .
USING Progress.Lang.*                                     FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Web2.WebHandler.SmartValueListWebHandler
    INHERITS SmartWebHandler:

&IF DEFINED (PacificWebSpeed) NE 0 &THEN

    {Consultingwerk/SmartFramework/System/dsValueList.i}

    /**
     * Purpose: Default handler for the HTTP GET method. The request being
     *          serviced and an optional status code is returned. A zero or
     *          null value means this method will deal with all errors.
     * Notes:
     * @param poRequest The IWebRequest instance representing the call
     * @return StatusCode of the response sent to the client
     */
     METHOD OVERRIDE PROTECTED INTEGER HandleGet (poRequest AS OpenEdge.Web.IWebRequest):

        DEFINE VARIABLE oRequest      AS IFetchDataRequest               NO-UNDO .
        DEFINE VARIABLE oResponse     AS OpenEdge.Net.HTTP.IHttpResponse NO-UNDO .
        DEFINE VARIABLE cValueListKey AS CHARACTER                       NO-UNDO .
        DEFINE VARIABLE oJson         AS JsonObject                      NO-UNDO .

        ASSIGN
            oResponse             = NEW OpenEdge.Web.WebResponse ()
            /* HTTP messages require a content type */
            oResponse:ContentType = 'application/json':U

            cValueListKey         = poRequest:GetPathParameter ("ValueList":U) .
            .

        DO ON ERROR UNDO, THROW:

            ServiceInterface:Activate () .

            oRequest = NEW FetchDataRequest ("*":U,
                                             SUBSTITUTE ("FOR EACH eSmartValueList WHERE eSmartValueList.ValueListKey = &1":U,
                                                         QUOTER (cValueListKey)),
                                             1) .

            FrameworkSettings:ServiceAdapter:RetrieveData ("":U,
                                                           "Consultingwerk.SmartFramework.System.ValueListBusinessEntity":U,
                                                           oRequest,
                                                           OUTPUT DATASET dsValueList) .

            IF NOT CAN-FIND (FIRST eSmartValueList) OR NOT CAN-FIND (FIRST eSmartValueListEntry) THEN
                UNDO, THROW NEW InvalidParameterValueException ("ValueList":U, cValueListKey, THIS-OBJECT:GetClass():TypeName) .

            BUFFER eSmartValueListEntry:BUFFER-FIELD ("ValueListEntryGuid":U):SERIALIZE-HIDDEN = TRUE .
            BUFFER eSmartValueListEntry:BUFFER-FIELD ("ValueListGuid":U):SERIALIZE-HIDDEN = TRUE .

            oJson = JsonHelper:FromDataset (DATASET dsValueList:HANDLE) .

            oResponse:Entity = oJson:GetJsonObject ("dsValueList":U):GetJsonArray ("eSmartValueListEntry":U) .

            oResponse:StatusCode = INTEGER (StatusCodeEnum:OK).

            THIS-OBJECT:WriteResponse (oResponse).
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

            RETURN 500 .
        END CATCH.

        FINALLY:
            DATASET dsValueList:EMPTY-DATASET () .

            ServiceInterface:Deactivate () .
        END FINALLY.

    END METHOD .
&ENDIF

END CLASS.
