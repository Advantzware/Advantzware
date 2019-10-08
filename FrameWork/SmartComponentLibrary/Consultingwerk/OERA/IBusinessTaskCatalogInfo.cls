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
    File        : IBusinessTaskCatalogInfo
    Purpose     : Interface for Business Tasks that should support REST/JSDO
                  catalog generation through the generic service
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Aug 08 11:17:07 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

INTERFACE Consultingwerk.OERA.IBusinessTaskCatalogInfo:

    /**
     * Purpose: Returns the REST/JSDO catalog data to the caller
     * Notes:
     * @return The IBusinessTaskCatalogData instance describing the methods of the Business Task
     */
    METHOD PUBLIC IBusinessTaskCatalogData GetCatalogData ().

END INTERFACE.
