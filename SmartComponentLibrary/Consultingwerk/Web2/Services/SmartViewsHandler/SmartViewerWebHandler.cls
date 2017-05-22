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
    File        : SmartViewerWebHandler
    Purpose     : Pacific WebSpeed WebHandler for returning viewer definition
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jun 17 14:13:48 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                          FROM PROPATH .
USING Consultingwerk.Framework.*                          FROM PROPATH .
USING Consultingwerk.Framework.Collections.*              FROM PROPATH .
USING Consultingwerk.OERA.*                               FROM PROPATH .
USING Consultingwerk.OERA.JsdoGenericService.WebHandler.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.*          FROM PROPATH .
USING Consultingwerk.Util.*                               FROM PROPATH .
USING Consultingwerk.Web2.Services.Rendering.*            FROM PROPATH .
USING Consultingwerk.Web2.Services.SmartMenu.*            FROM PROPATH .
USING Consultingwerk.Web2.Services.SmartViews.*           FROM PROPATH .
USING Consultingwerk.Web2.WebHandler.*                    FROM PROPATH .
USING OpenEdge.Net.HTTP.*                                 FROM PROPATH .
USING OpenEdge.Web.*                                      FROM PROPATH .
USING Progress.Lang.*                                     FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Web2.Services.SmartViewsHandler.SmartViewerWebHandler
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

        DEFINE VARIABLE oViewerService    AS Viewer                  NO-UNDO .
        DEFINE VARIABLE cObjectName       AS CHARACTER               NO-UNDO .
        DEFINE VARIABLE cEntityName       AS CHARACTER               NO-UNDO .
        DEFINE VARIABLE cViewName         AS CHARACTER               NO-UNDO .
        DEFINE VARIABLE cScope            AS CHARACTER               NO-UNDO .
        DEFINE VARIABLE oResponse         AS IHttpResponse           NO-UNDO .
        DEFINE VARIABLE lcHtml            AS LONGCHAR                NO-UNDO .
        DEFINE VARIABLE oParamDictionary  AS CharacterDictionary     NO-UNDO .
        DEFINE VARIABLE oBindingStyle     AS ViewerBindingStyleEnum  NO-UNDO .

        ASSIGN
            oResponse             = NEW WebResponse ()
            /* HTTP messages require a content type */
            oResponse:ContentType = 'text/html':U
            oBindingStyle         = ViewerBindingStyleEnum:AngularJS
            .

        DO ON ERROR UNDO, THROW:
            ServiceInterface:Activate () .

            ASSIGN cObjectName = poRequest:GetPathParameter ("ObjectName":U)
                   cEntityName = poRequest:GetPathParameter ("EntityName":U)
                   cViewName   = poRequest:GetPathParameter ("ViewName":U) .

                oParamDictionary = ListHelper:AlternatingListToDictionary (STRING (poRequest:GetContextValue ("QUERY_STRING":U)),
                                                                           "&":U, "=":U)
                .

            IF oParamDictionary:ContainsKey ("scope":U) THEN
                cScope = WebHelper:DecodeURL (oParamDictionary:GetValue ("scope":U)).

            IF oParamDictionary:ContainsKey ("bindings":U) THEN
                oBindingStyle = CAST (EnumHelperExt:FromString (oParamDictionary:GetValue ("bindings":U),
                                                                Progress.Lang.Class:GetClass ("Consultingwerk.Web2.Services.SmartViews.ViewerBindingStyleEnum":U)),
                                      Consultingwerk.Web2.Services.SmartViews.ViewerBindingStyleEnum) .

            IF cObjectName > "":U THEN
                lcHtml = THIS-OBJECT:RenderDynamicViewer (cObjectName, cScope, oBindingStyle) .
            ELSE DO:
                oViewerService = {Consultingwerk/get-service.i Viewer "NEW Viewer ()"} .
                oViewerService:GetFragment (cEntityName, cViewName, cScope, oBindingStyle, OUTPUT lcHtml) .
            END.

            oResponse:Entity = NEW OpenEdge.Core.String (lcHtml) .

            oResponse:StatusCode = INTEGER (StatusCodeEnum:OK).

            THIS-OBJECT:WriteResponse (oResponse).

            CATCH ple AS Progress.Lang.Error:
                THIS-OBJECT:HandleError (ple,
                                         oResponse).
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

    /**
     * Purpose: Renders the viewer template
     * Notes:
     * @param pcObjectName The repository object name of render
     * @param pcScope The binding scope
     * @param poBindings The ViewerBindingStyleEnum describing the style of the html template
     * @return The Angular viewer template
     */
    METHOD PUBLIC LONGCHAR RenderDynamicViewer (pcObjectName AS CHARACTER,
                                                pcScope      AS CHARACTER,
                                                poBindings   AS ViewerBindingStyleEnum):

        DEFINE VARIABLE oRepository     AS ISmartRepositoryService        NO-UNDO .
        DEFINE VARIABLE oParameter      AS FetchRepositoryObjectParameter NO-UNDO .
        DEFINE VARIABLE hRepositoryData AS HANDLE                         NO-UNDO .
        DEFINE VARIABLE hAttributes     AS HANDLE                         NO-UNDO .
        DEFINE VARIABLE oService        AS IWebViewerRenderingService     NO-UNDO .
        DEFINE VARIABLE cServiceType    AS CHARACTER                      NO-UNDO .

        oRepository = {Consultingwerk/get-service.i ISmartRepositoryService "NEW SmartRepositoryService ()"} .

        oParameter = NEW FetchRepositoryObjectParameter (pcObjectName, FALSE) .

        hRepositoryData = oRepository:FetchRepositoryObject (oParameter) .

        hAttributes = oRepository:GetAttributeBuffer (?, hRepositoryData) .

        ASSIGN cServiceType = hAttributes::WebRenderingService .

        oService = CAST (FrameworkSettings:ServiceContainer:GetService (Progress.Lang.Class:GetClass (cServiceType)),
                         IWebViewerRenderingService) .

        IF NOT VALID-OBJECT (oService) THEN DO:
            oService = DYNAMIC-NEW (cServiceType) () .
            FrameworkSettings:ServiceContainer:AddService (oService:GetClass(), oService) .
        END.

        RETURN oService:RenderDynamicViewer (hRepositoryData, pcScope, poBindings) .

        FINALLY:
            GarbageCollectorHelper:DeleteObject(hAttributes) .
            GarbageCollectorHelper:DeleteObject(hRepositoryData) .
        END FINALLY.

    END METHOD .

&ENDIF

END CLASS.
