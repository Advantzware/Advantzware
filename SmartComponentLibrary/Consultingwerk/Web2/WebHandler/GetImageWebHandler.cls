/**********************************************************************
 * Copyright (C) 2006-2017 by Consultingwerk Ltd. ("CW") -            *
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
    File        : GetImageWebHandler
    Purpose     : General purpose GetImage Web Handler.
    Syntax      : Using Path-Parameter FileName to FileName10 to locate
                  image in the PROPATH.
                  Authorization for retrieving individual images can be
                  injected using an IGetImageAuthorizationProvider service
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jan 17 12:01:54 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                          FROM PROPATH .
USING Consultingwerk.Exceptions.*                         FROM PROPATH .
USING Consultingwerk.Framework.Authorization.Exceptions.* FROM PROPATH .
USING Consultingwerk.OERA.*                               FROM PROPATH .
USING Consultingwerk.OERA.JsdoGenericService.WebHandler.* FROM PROPATH .
USING Consultingwerk.Util.*                               FROM PROPATH .
USING Consultingwerk.Web2.WebHandler.*                    FROM PROPATH .
USING Progress.Lang.*                                     FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Web2.WebHandler.GetImageWebHandler
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

        DEFINE VARIABLE oAuthorizationProvider AS IGetImageAuthorizationProvider  NO-UNDO .
        DEFINE VARIABLE oResponse              AS OpenEdge.Net.HTTP.IHttpResponse NO-UNDO .
        DEFINE VARIABLE cFileName              AS CHARACTER                       NO-UNDO .
        DEFINE VARIABLE oMemPtr                AS MEMPTR                          NO-UNDO .
        DEFINE VARIABLE oPicture               AS OpenEdge.Core.Memptr            NO-UNDO .
        DEFINE VARIABLE oEntity                AS OpenEdge.Net.MultipartEntity    NO-UNDO .
        DEFINE VARIABLE oPart                  AS OpenEdge.Net.MessagePart        NO-UNDO .
        DEFINE VARIABLE cContentType           AS CHARACTER                       NO-UNDO .
        DEFINE VARIABLE cExtension             AS CHARACTER                       NO-UNDO .

        ASSIGN
            oResponse             = NEW OpenEdge.Web.WebResponse ()
            /* HTTP messages require a content type */
            oResponse:ContentType = 'application/json':U
            .

        oAuthorizationProvider = {Consultingwerk/get-service.i IGetImageAuthorizationProvider} .

        ASSIGN cFileName  = SUBSTRING (poRequest:PathInfo, 8, -1, "CHARACTER":U)     // Trim "/Image/" from path info
               cExtension = FileHelper:FileExtension (cFileName).

        IF cFileName BEGINS "/":U OR cFileName BEGINS "~\":U OR
           (LENGTH (cFileName, "CHARACTER":U) > 1 AND SUBSTRING (cFileName, 2, 1, "CHARACTER":U) = ":":U) THEN

            UNDO, THROW NEW InvalidParameterValueException ("FileName":U,
                                                            "Consultingwerk.Web2.WebHandler.GetImageWebHandler":U,
                                                            "Absolut path name is not supported."{&TRAN},
                                                            0) .

        CASE cExtension:
            WHEN "png":U THEN
                ASSIGN cContentType  = "image/png":U .
            WHEN "gif":U THEN
                ASSIGN cContentType  = "image/gif":U .
            WHEN "jpg":U OR WHEN "jpeg":U THEN
                ASSIGN cContentType  = "image/jpeg":U .
            OTHERWISE
                UNDO, THROW NEW InvalidParameterValueException ("FileName":U,
                                                                "Consultingwerk.Web2.WebHandler.GetImageWebHandler":U,
                                                                SUBSTITUTE ("Unsupported file extension &1."{&TRAN}, cExtension),
                                                                0) .
        END CASE .

        IF VALID-OBJECT (oAuthorizationProvider) THEN DO:
            ServiceInterface:Activate () .

            IF oAuthorizationProvider:IsGetImageRestricted (cFileName) = TRUE THEN
                UNDO, THROW NEW NotAuthorizedException ("You are not authorized to retrieve this image file."{&TRAN},
                                                        0) .
        END.

        FileAssert:Exists (cFileName) .

        FILE-INFORMATION:FILE-NAME = cFileName.

        COPY-LOB FILE FILE-INFORMATION:FULL-PATHNAME TO oMemptr.

        oPicture = NEW OpenEdge.Core.Memptr (oMemptr).

        IF poRequest:HasHeader('Accept':U) AND
           poRequest:GetHeader('Accept':U):VALUE EQ 'multipart/form-data':U THEN
        DO:
            ASSIGN
                oEntity = NEW OpenEdge.Net.MultipartEntity ()
                oEntity:Boundary = GUID

                oResponse:Entity      = oEntity
                oResponse:ContentType = 'multipart/form-data':U

                oPart = NEW OpenEdge.Net.MessagePart (cContentType, oPicture)
                .

            oEntity:AddPart(oPart).
        END.
        ELSE DO:
            ASSIGN
                oResponse:Entity      = oPicture
                oResponse:ContentType = cContentType
                .
        END.

        oResponse:StatusCode = INTEGER (OpenEdge.Net.HTTP.StatusCodeEnum:OK).

        THIS-OBJECT:WriteResponse (oResponse).

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
            SET-SIZE (oMemptr) = 0 .

            IF VALID-OBJECT (oAuthorizationProvider) THEN
                ServiceInterface:Deactivate () .
        END FINALLY.

     END METHOD.
&ENDIF

END CLASS.
