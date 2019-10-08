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
    File        : IGenericServiceCustomization
    Purpose     : Interface for Services which allow the customization of the
                  JSDO Generic Service (Web Handler)
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Nov 23 16:29:55 CET 2016
    Notes       : Only valid from 11.6 on
  ----------------------------------------------------------------------*/

USING Consultingwerk.*                                    FROM PROPATH .
USING Consultingwerk.OERA.*                               FROM PROPATH .
USING Consultingwerk.OERA.JsdoGenericService.WebHandler.* FROM PROPATH .
USING Progress.Lang.*                                     FROM PROPATH .

{Consultingwerk/products.i}

INTERFACE Consultingwerk.OERA.JsdoGenericService.IGenericServiceCustomization:

&IF DEFINED (PacificWebSpeed) NE 0 &THEN

    /**
     * Purpose: Allows customization of the FetchDataRequest parameter
     *          passed to the Business Entities Count request
     * Notes:
     * @param pcEntityName The name of the Business Entity
     * @param poWebRequest The IWebReqeust instance of the current request
     * @param poFetchDataRequest The reference to the FetchDataRequest instance that will be passed to the Business Entity:Count method
     */
    METHOD PUBLIC VOID CustomizeCountParameter (pcEntityName AS CHARACTER,
                                                poWebRequest AS OpenEdge.Web.IWebRequest,
                                                poFetchDataRequest AS IFetchDataRequest).

    /**
     * Purpose: Allows customization of the FetchDataRequest parameter
     *          passed to the Business Entities GetData request
     * Notes:
     * @param pcEntityName The name of the Business Entity
     * @param poWebRequest The IWebReqeust instance of the current request
     * @param poFetchDataRequest The reference to the FetchDataRequest instance that will be passed to the Business Entity:FetchData method
     */
    METHOD PUBLIC VOID CustomizeFetchDataParameter (pcEntityName AS CHARACTER,
                                                    poWebRequest AS OpenEdge.Web.IWebRequest,
                                                    poFetchDataRequest AS IFetchDataRequest).

    /**
     * Purpose: Allows customization of the optional parameter object for Save Changes
     *          requests
     * Notes:
     * @param pcEntityName The name of the Business Entity
     * @param poWebRequest The IWebReqeust instance of the current request
     * @return The parameter object instance to pass to the ServiceInterface:SaveChanges method
     */
    METHOD PUBLIC Progress.Lang.Object CustomizeSaveChangesParameter (pcEntityName AS CHARACTER,
                                                                      poWebRequest AS OpenEdge.Web.IWebRequest).

    /**
     * Purpose: Allows customization of the (optional) parameter object for InvokeMethod and
     *          InvokeTask
     * Notes:
     * @param pcBusinessServiceName The name of the Business Entity or BusinessTask
     * @param pcMethodName The name of the invoked method
     * @param poWebRequest The IWebReqeust instance of the current request
     * @param poParameter INPUT-OUTPUT The parameter object instance to pass to the ServiceInterface:InvokeMethod / InvokeTask method
     */
    METHOD PUBLIC VOID CustomizeInvokeMethodParameter (pcBusinessServiceName AS CHARACTER,
                                                       pcMethodName AS CHARACTER,
                                                       poWebRequest AS OpenEdge.Web.IWebRequest,
                                                       INPUT-OUTPUT poParameter AS JsonSerializable).
&ENDIF

END INTERFACE.
