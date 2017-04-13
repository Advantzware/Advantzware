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
    File        : IWebViewerRenderingService
    Purpose     : Interface for services rendering repository objects
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jan 02 13:49:10 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.Web2.Services.Rendering.*  FROM PROPATH .
USING Consultingwerk.Web2.Services.SmartViews.* FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

INTERFACE Consultingwerk.Web2.Services.Rendering.IWebViewerRenderingService:

    /**
     * Purpose: Renders the viewer template
     * Notes:
     * @param pcObjectName The repository object name of render
     * @param pcScope The binding scope
     * @param poBindings The ViewerBindingStyleEnum describing the style of the html template
     * @return The Angular viewer template
     */
    METHOD PUBLIC LONGCHAR RenderDynamicViewer (pcObjectName AS CHARACTER,
                                                pcScope AS CHARACTER,
                                                poBindings AS ViewerBindingStyleEnum).

    /**
     * Purpose: Renders the viewer template
     * Notes:
     * @param phRepositoryData The repository data to render (ProDataset)
     * @param pcScope The binding scope
     * @param poBindings The ViewerBindingStyleEnum describing the style of the html template
     * @return The Angular viewer template
     */
    METHOD PUBLIC LONGCHAR RenderDynamicViewer (phRepositoryData AS HANDLE,
                                                pcScope AS CHARACTER,
                                                poBindings AS ViewerBindingStyleEnum).

END INTERFACE.
