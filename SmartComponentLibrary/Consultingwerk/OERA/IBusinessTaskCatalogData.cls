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
    File        : IBusinessTaskCatalogData
    Purpose     : Describes the REST/JSDO Interface to a Business Task
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Aug 08 11:19:12 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

INTERFACE Consultingwerk.OERA.IBusinessTaskCatalogData:

    /**
     * Purpose: Returns the name of the Business Task
     * Notes:
     */
    DEFINE PUBLIC PROPERTY BusinessTaskName AS CHARACTER NO-UNDO
    GET.

    /**
     * Purpose: Returns the List of IBusinessTaskMethodData instances
     *          for the Business Task
     * Notes:
     */
    DEFINE PUBLIC PROPERTY Methods AS ListBusinessTaskMethodData NO-UNDO
    GET.

    /**
     * Purpose: Returns the IBusinessTaskMethodData instance for the given
     *          Method Name
     * Notes:   Throwns an error when the Method name is not known
     * @param pcMethodName The name of the method to return
     * @return The IBusinessTaskMethodData instance
     */
    METHOD PUBLIC IBusinessTaskMethodData GetMethod (pcMethodName AS CHARACTER) .

END INTERFACE.
