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

CLASS Consultingwerk.OERA.JsdoGenericService.WebHandler.BusinessServicesWebHandler
    INHERITS ListBusinessServicesWebHandler:

&IF DEFINED (PacificWebSpeed) NE 0 &THEN

    { Consultingwerk/Util/TempTables/ttClassNames.i}

    /**
     * Purpose: Generate html page displaying all BusinessServices found in Package
     * Notes:
     * @param poResponse The Response to the client requesting the information
     * @param pcPackageName Base package as requested to be used to build href
     */
    METHOD PRIVATE VOID GenerateHtml (poResponse AS IHttpResponse,
                                      pcPackageName AS CHARACTER):

        DEFINE VARIABLE oString AS OpenEdge.Core.String NO-UNDO .
        /* HTTP messages require a content type */
        poResponse:ContentType = 'text/html':U.

        oString = NEW OpenEdge.Core.String ().
        oString:Append ('<html>':U).
        oString:Append ('<head>':U).
        oString:Append (SUBSTITUTE ('<title>List of Business Services in &1</title>':U, pcPackageName)) .
//      Production URI form rawgit - update commit hash in URI to retrieve updated version
        oString:Append ('<link rel="StyleSheet" type="text/css" href="https://cdn.rawgit.com/consultingwerk/SmartComponentLibraryWebResources/fc11cd26f465858d52a05b14cdb9e07e8da572e7/catalogs.css" >':U).
//      Development URI form rawgit
//        oString:Append ('<link rel="StyleSheet" type="text/css" href="https://rawgit.com/consultingwerk/SmartComponentLibraryWebResources/master/catalogs.css" >':U).
        oString:Append ('</head>':U).
        oString:Append ('<body>':U).
        oString:Append ('<div id="header">':U).
        oString:Append ('<h1>List of Business Services</h1>':U).
        oString:Append ('<img src="https://cdn.rawgit.com/consultingwerk/SmartComponentLibraryWebResources/master/img/SmartComponentLibary_logo.png" alt="SmartComponent Library" align="right" >':U).
        oString:Append ('</div>':U).
        oString:Append ('<br>':U).
        oString:Append ('<div>':U).
        oString:Append (SUBSTITUTE ('<a target="_blank" href="../xml/&1">&2</a>':U, pcPackageName, "&lt;xml&gt;":U)).
        oString:Append ('&nbsp;&nbsp;':U).
        oString:Append (SUBSTITUTE ('<a target="_blank" href="../json/&1">~{json}</a>':U, pcPackageName)).
        oString:Append ('&nbsp;&nbsp;':U).
        oString:Append (SUBSTITUTE ('<a target="_blank" href="../../Catalogs/&1">~{catalogs}</a>':U, pcPackageName)).
        oString:Append ('</div>':U).
        oString:Append ('<br>':U).
        oString:Append ('<div>':U).
        oString:Append ('<table class="hovertable">':U).
        oString:Append ('<tr>':U).
        oString:Append ('<th>Business Entity</th>':U).
        oString:Append ('<th>Catalog</th>':U).
        oString:Append ('<th>Resource</th>':U).
        oString:Append ('<th>PNG</th>':U).
        oString:Append ('</tr>':U).

        FOR EACH ttTableEntry ON ERROR UNDO, THROW:
            oString:Append ('<tr>':U).
            oString:Append (SUBSTITUTE ('<td>&1</td>':U, SUBSTITUTE ('<a target="_blank" href="&2">&1</a>':U,
                                                                     ttTableEntry.BEName,
                                                                     ttTableEntry.CatalogURL))).
            oString:Append (SUBSTITUTE ('<td>&1</td>':U, GetDetailLink ("Catalog":U, ttTableEntry.CatalogURL))).
            oString:Append (SUBSTITUTE ('<td>&1</td>':U, GetDetailLink ("Resource":U, ttTableEntry.ResourceURL))).
            oString:Append (SUBSTITUTE ('<td>&1</td>':U, GetDetailLink ("png":U, ttTableEntry.PNGURL))).
            oString:Append ('</tr>':U).
        END.

        oString:Append ('</table>':U).
        oString:Append ('</div>':U).
        oString:Append ('</body>':U).
        oString:Append ('</html>':U).

        poResponse:Entity = oString.

    END METHOD.

    /**
     * Purpose: Generates a list of all BusinessServices found in Package as json
     * Notes:
     * @param poResponse The Response to the client requesting the information
     */
    METHOD PRIVATE VOID GenerateJson (poResponse AS IHttpResponse):

        DEFINE VARIABLE oJsonArray AS JsonArray NO-UNDO.
        DEFINE VARIABLE iCount     AS INTEGER   NO-UNDO.

        FOR EACH ttTableEntry ON ERROR UNDO, THROW :
            iCount = iCount + 1.
        END.

        oJsonArray = NEW JsonArray (iCount).

        iCount = 0.
        FOR EACH ttTableEntry ON ERROR UNDO, THROW:
            iCount = iCount + 1.
            oJsonArray:Set (iCount, ttTableEntry.BEName).
        END.

        poResponse:ContentType = 'application/json':U.
        poResponse:Entity = oJsonArray.

    END METHOD.

    /**
     * Purpose: Returns an image of the BusinessEntity layout identified by Entity Name
     * Notes:
     * @param poRequest The Request send by the client
     * @param poResponse The Response to the client requesting the information
     * @param pcBusinessEntityName The BusinessEntity Name of the image to be displayed
     */
    METHOD PRIVATE VOID GeneratePng (poRequest AS IWebRequest,
                                     poResponse AS IHttpResponse,
                                     pcBusinessEntityName AS CHARACTER):

        DEFINE VARIABLE oMemptr   AS MEMPTR                       NO-UNDO.
        DEFINE VARIABLE cFilename AS CHARACTER                    NO-UNDO.
        DEFINE VARIABLE oPicture  AS OpenEdge.Core.Memptr         NO-UNDO.
        DEFINE VARIABLE oEntity   AS OpenEdge.Net.MultipartEntity NO-UNDO.
        DEFINE VARIABLE oPart     AS OpenEdge.Net.MessagePart     NO-UNDO.

        IF NUM-ENTRIES (pcBusinessEntityName, ".":U) > 1 THEN
            IF ENTRY (NUM-ENTRIES (pcBusinessEntityName, ".":U), pcBusinessEntityName, ".":U) = "png":U THEN
                ENTRY (NUM-ENTRIES (pcBusinessEntityName, ".":U), pcBusinessEntityName, ".":U) = "":U.

        cFileName = ClassHelper:ClassNameToFile (TRIM (pcBusinessEntityName, ".":U)).
        cFilename = FileHelper:ReplaceFileExtension(cFilename, "png":U).

        FILE-INFORMATION:FILE-NAME = cFileName.

        IF FILE-INFORMATION:FULL-PATHNAME = ? THEN
            UNDO, THROW NEW AppError (SUBSTITUTE ("No Image found for BusinessEntity '&1'!":U, TRIM (pcBusinessEntityName, ".":U)), 0).

        COPY-LOB FILE FILE-INFORMATION:FULL-PATHNAME TO oMemptr.
        oPicture = NEW OpenEdge.Core.Memptr (oMemptr).

        IF poRequest:HasHeader('Accept':U) AND
           poRequest:GetHeader('Accept':U):VALUE EQ 'multipart/form-data':U THEN
        DO:
            ASSIGN
                oEntity = NEW OpenEdge.Net.MultipartEntity ()
                oEntity:Boundary = GUID

                poResponse:Entity      = oEntity
                poResponse:ContentType = 'multipart/form-data':U

                oPart = NEW OpenEdge.Net.MessagePart ('image/png':U, oPicture)
                .

            oEntity:AddPart(oPart).
        END.
        ELSE DO:
            ASSIGN
                poResponse:Entity      = oPicture
                poResponse:ContentType = 'image/png':U
                .

        END.

        FINALLY:
            SET-SIZE (oMemptr) = 0 .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Generates a list of all BusinessServices found in Package as xml
     * Notes:
     * @param poResponse The Response to the client requesting the information
     */
    METHOD PRIVATE VOID GenerateXml (poResponse AS IHttpResponse):

        DEFINE VARIABLE hXDocument AS HANDLE NO-UNDO.
        DEFINE VARIABLE oWidgetHandle AS OpenEdge.Core.WidgetHandle NO-UNDO.

        EMPTY TEMP-TABLE ttClassNames .

        FOR EACH ttTableEntry ON ERROR UNDO, THROW:
            CREATE ttClassNames.
            ASSIGN ttClassNames.ClassName = ttTableEntry.BEName.
        END.

        CREATE X-DOCUMENT hXDocument.

        BUFFER ttClassNames:XML-NODE-NAME = "BusinessService":U.
        BUFFER ttClassNames:BUFFER-FIELD ("ClassName":U):XML-NODE-TYPE = "ATTRIBUTE":U.

        TEMP-TABLE ttClassNames:WRITE-XML ("HANDLE":U, hXDocument, TRUE, Codepages:UTF-8, ?).

        oWidgetHandle = NEW OpenEdge.Core.WidgetHandle (hXDocument).

        poResponse:ContentType = "text/xml":U.
        poResponse:Entity = oWidgetHandle.

    END METHOD.

    /**
     * Purpose: Return a html link string
     * Notes:
     * @param pcType Type of resource to be linked to, determins the Image and alt text
     * @param pcLinkTarget The URL to navigate to
     * @return The html link with associated image
     */
    METHOD PRIVATE CHARACTER GetDetailLink (pcType AS CHARACTER,
                                            pcLinkTarget AS CHARACTER):

        DEFINE VARIABLE cTypeList   AS CHARACTER NO-UNDO INITIAL "Catalog,Resource,png":U.
        DEFINE VARIABLE cDetailLink AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cImage      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cImageURL   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cAltText    AS CHARACTER NO-UNDO.

        ListAssert:EntryIsInList(pcType, cTypeList).

        IF pcLinkTarget = "":U THEN
            RETURN cDetailLink.

        CASE pcType :
            WHEN "Catalog":U THEN
                ASSIGN
                    cImage   = "catalog_16.png":U
                    cAltText = "Catalog":U
                    .
            WHEN "Resource":U THEN
                ASSIGN
                    cImage   = "resource_16.png":U
                    cAltText = "Resource":U
                    .
            WHEN "png":U THEN
                ASSIGN
                    cImage   = "png_16.png":U
                    cAltText = "png":U
                    .
        END CASE.

        cImageURL = SUBSTITUTE ('https://cdn.rawgit.com/consultingwerk/SmartComponentLibraryWebResources/master/img/&1':U,
                                cImage).

        cDetailLink = SUBSTITUTE ('<a target="_blank" href="&1"><img src="&2" alt="&3" style="border:0;"></a>':U,
                                  pcLinkTarget,
                                  cImageURL,
                                  cAltText).

        RETURN cDetailLink.

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

        DEFINE VARIABLE oResponse      AS IHttpResponse       NO-UNDO .
        DEFINE VARIABLE cOutputFormat  AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cPackageName   AS CHARACTER           NO-UNDO .

        DEFINE VARIABLE cOutputFormatOptions AS CHARACTER NO-UNDO INITIAL "html,png,xml,json":U.

        ServiceInterface:Activate () .

        ASSIGN
            oResponse = NEW WebResponse ()
            .

        DO ON ERROR UNDO, THROW:
            ASSIGN
                cOutputFormat = poRequest:GetPathParameter ("OutputFormat":U)
                cPackageName  = poRequest:GetPathParameter ("PackageName":U)
                .

            ListAssert:EntryIsInList(cOutputFormat, cOutputFormatOptions).

            IF NOT cOutputFormat = "png":U THEN
                THIS-OBJECT:GatherClassInformation (cPackageName).

            CASE cOutputFormat :
                WHEN "html":U THEN
                    THIS-OBJECT:GenerateHtml (oResponse, cPackageName).
                WHEN "png":U THEN
                    THIS-OBJECT:GeneratePng (poRequest, oResponse, cPackageName).
                WHEN "xml":U THEN
                    THIS-OBJECT:GenerateXml (oResponse).
                WHEN "json":U THEN
                    THIS-OBJECT:GenerateJson (oResponse).
            END CASE.

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