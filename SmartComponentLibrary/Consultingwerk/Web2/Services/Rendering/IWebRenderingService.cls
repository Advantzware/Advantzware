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
    File        : IWebRenderingService
    Purpose     : Interface for Web Rendering services
    Syntax      :
    Description :
    Author(s)   : Mike FEchner / Consultingwerk Ltd.
    Created     : Wed Nov 16 19:48:34 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.Web2.Services.Rendering.*  FROM PROPATH .
USING Consultingwerk.Web2.Services.SmartViews.* FROM PROPATH.
USING Progress.Lang.*                           FROM PROPATH .

INTERFACE Consultingwerk.Web2.Services.Rendering.IWebRenderingService:

    /**
     * Purpose: Renders a repository object as html
     * Notes:
     * @param pcObjectName The name of the repository field
     * @param pcScope The scope to bind the field to (for additional properties)
     * @param pcFieldModel The model to bind the field to
     * @param pcFieldId  The Id of the field (JQuery)
     * @param plMandatory Logical value indicating if the field is mandatory
     * @param plReadOnly Logical value indicating if the field is mandatory
     * @param phAttributes The record buffer with the object attributes
     * @param phDataset The handle of the Business Entity Dataset
     * @param pcTables The list of tables (entity table + entity view)
     * @param poBindingStyle The style of data bindings (AngularJS or Angular2)
     * @return The longchar representation (hmtl code) of the field
     */
    METHOD PUBLIC LONGCHAR RenderHtml (pcObjectName AS CHARACTER,
                                       pcScope AS CHARACTER,
                                       pcFieldModel AS CHARACTER,
                                       pcFieldId AS CHARACTER,
                                       plMandatory AS LOGICAL,
                                       plReadOnly AS LOGICAL,
                                       phAttributes AS HANDLE,
                                       phDataset AS HANDLE,
                                       pcTables AS CHARACTER,
                                       poBindingStyle AS ViewerBindingStyleEnum) .

END INTERFACE.
