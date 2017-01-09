/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : RestServiceAddress
    Purpose     : Service Address parameters for JSDO Catalog / Business
                  Entity Resource
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue May 19 13:49:34 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.JsdoGenericService.* FROM PROPATH .
USING Progress.Lang.*                          FROM PROPATH .

CLASS Consultingwerk.OERA.JsdoGenericService.RestServiceAddress:

    /**
     * Purpose: Get and sets the REST service Address for the resource
     * Notes:
     */
    DEFINE PUBLIC PROPERTY Address AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sers the REST service of the resource
     * Notes:
     */
    DEFINE PUBLIC PROPERTY Name AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Constructor for the RestServiceAddress class
     * Notes:
     * @param pcName The value for the service name
     * @param pcAddress The value for the service address
     */
    CONSTRUCTOR PUBLIC RestServiceAddress (pcName AS CHARACTER,
                                           pcAddress AS CHARACTER):
        SUPER ().

        ASSIGN THIS-OBJECT:Name    = pcName
               THIS-OBJECT:Address = pcAddress .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the RestServiceAddress class
     * Notes:
     */
    CONSTRUCTOR PUBLIC RestServiceAddress ():
        SUPER ().

    END CONSTRUCTOR.

END CLASS.
