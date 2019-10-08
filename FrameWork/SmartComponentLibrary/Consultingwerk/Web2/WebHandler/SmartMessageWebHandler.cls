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
    File        : SmartMessageWebHandler
    Purpose     : Pacific WebSpeed WebHandler for access to Messages
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jun 17 11:19:11 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.*                               FROM PROPATH .
USING Consultingwerk.OERA.JsdoGenericService.WebHandler.* FROM PROPATH .
USING Consultingwerk.Util.*                               FROM PROPATH .
USING Consultingwerk.Web2.Services.SmartMessage.*         FROM PROPATH .
USING Consultingwerk.Web2.WebHandler.*                    FROM PROPATH .
USING OpenEdge.Net.HTTP.*                                 FROM PROPATH .
USING OpenEdge.Web.*                                      FROM PROPATH .
USING Progress.Lang.*                                     FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Web2.WebHandler.SmartMessageWebHandler
    INHERITS SmartWebHandler:

&IF DEFINED (PacificWebSpeed) NE 0 &THEN

    /*------------------------------------------------------------------------------
        Purpose: Default handler for the HTTP GET method. The request being
                 serviced and an optional status code is returned. A zero or
                 null value means this method will deal with all errors.
        Notes:
        @param poRequest The IWebRequest instance representing the call
        @return StatusCode of the response sent to the client
    ------------------------------------------------------------------------------*/
     METHOD OVERRIDE PROTECTED INTEGER HandleGet (poRequest AS OpenEdge.Web.IWebRequest):

        DEFINE VARIABLE oMessageService  AS SmartMessageInterface NO-UNDO .
        DEFINE VARIABLE cMessageGroup    AS CHARACTER             NO-UNDO .
        DEFINE VARIABLE cMessageNumber   AS CHARACTER             NO-UNDO .
        DEFINE VARIABLE iMessageNumber   AS INTEGER               NO-UNDO .
        DEFINE VARIABLE oResponse        AS IHttpResponse         NO-UNDO .

        ASSIGN
            oResponse             = NEW WebResponse ()
            /* HTTP messages require a content type */
            oResponse:ContentType = 'application/json':U
            .

        DO ON ERROR UNDO, THROW:

            ServiceInterface:Activate () .

            ASSIGN cMessageGroup  = poRequest:GetPathParameter ("MessageGroup":U)
                   cMessageNumber = poRequest:GetPathParameter ("MessageNumber":U)
                   iMessageNumber = DataTypeHelper:ToInteger (cMessageNumber).

            oMessageService = {Consultingwerk/get-service.i SmartMessageInterface "NEW SmartMessageInterface ()"} .

            oResponse:Entity = oMessageService:GetSmartMessage(cMessageGroup, iMessageNumber) .

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

            RETURN 500 .
        END CATCH.

        FINALLY:
            ServiceInterface:Deactivate () .
        END FINALLY.

     END METHOD.
&ENDIF

END CLASS.
