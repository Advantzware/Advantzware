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
    File        : Web2FormGenerator
    Purpose     : Generator for Web2 Forms
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Nov 17 19:56:50 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.* FROM PROPATH.
USING Consultingwerk.*                                           FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.Web2View.* FROM PROPATH .
USING Progress.Lang.*                                            FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.Web2View.Web2FormGenerator:

    /**
     * Purpose: Generates the Web2 Form
     * Notes:
     * @param pcFileName
     * @param pcTemplateName
     * @param pcControllerName
     * @param pcHeading
     * @param pcGridLayout
     * @param pcEntityName
     * @param pcEntityTable
     * @param pcEntityView
     * @param pcDataSourceName
     * @param pcGridName
     * @param pcToolbarName
     * @param pcViewerName
     * @param pcViewerLayout
     * @param pcViewerTemplate
     */
    METHOD PUBLIC VOID Generate (pcFileName AS CHARACTER,
                                 pcTemplateName AS CHARACTER,
                                 pcControllerName AS CHARACTER,
                                 pcHeading AS CHARACTER,
                                 pcGridLayout AS CHARACTER,
                                 pcEntityName AS CHARACTER,
                                 pcEntityTable AS CHARACTER,
                                 pcEntityView AS CHARACTER,
                                 pcDataSourceName AS CHARACTER,
                                 pcGridName AS CHARACTER,
                                 pcToolbarName AS CHARACTER,
                                 pcViewerName AS CHARACTER,
                                 pcViewerLayout AS CHARACTER,
                                 pcViewerTemplate AS CHARACTER):

        DEFINE VARIABLE lcCode AS LONGCHAR NO-UNDO.

        IF CharacterType:IsNullOrEmpty (pcTemplateName) THEN
            ASSIGN pcTemplateName = "Web/Views/web2form.template":U .

        COPY-LOB FROM FILE TemplateHelper:Find (pcTemplateName) TO lcCode .

        ASSIGN pcGridLayout = SUBSTITUTE ("&1/&2":U, pcEntityName, pcGridLayout) .

        IF pcViewerLayout > "":U THEN
            ASSIGN pcViewerLayout   = SUBSTITUTE ("&1/&2":U, pcEntityName, pcViewerLayout)
                   pcViewerTemplate = "":U.
        ELSE
            ASSIGN pcViewerLayout = "":U .

        ASSIGN lcCode = REPLACE (lcCode, "@ControllerName@":U, pcControllerName)
               lcCode = REPLACE (lcCode, "@Heading@":U       , pcHeading)
               lcCode = REPLACE (lcCode, "@GridLayout@":U    , pcGridLayout)
               lcCode = REPLACE (lcCode, "@EntityName@":U    , pcEntityName)
               lcCode = REPLACE (lcCode, "@EntityTable@":U   , pcEntityTable)
               lcCode = REPLACE (lcCode, "@EntityView@":U    , pcEntityView)
               lcCode = REPLACE (lcCode, "@DataSourceName@":U, pcDataSourceName)
               lcCode = REPLACE (lcCode, "@GridName@":U      , pcGridName)
               lcCode = REPLACE (lcCode, "@ToolbarName@":U   , pcToolbarName)
               lcCode = REPLACE (lcCode, "@ViewerName@":U    , pcViewerName)
               lcCode = REPLACE (lcCode, "@ViewerLayout@":U  , pcViewerLayout)
               lcCode = REPLACE (lcCode, "@ViewerTemplate@":U, pcViewerTemplate) .

        COPY-LOB FROM lcCode TO FILE pcFileName .

    END METHOD.

END CLASS.
