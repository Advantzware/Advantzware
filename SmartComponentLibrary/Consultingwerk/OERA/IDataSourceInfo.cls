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
    File        : IDataSourceInfo
    Purpose     : Interface for general purpose Datasource (Business Entity)
                  view parameter
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jan 10 19:46:54 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.OERA.IDataSourceInfo:

    /**
     * Purpose: Returns the name of the Business Entity
     * Notes:
     */
    DEFINE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO
    GET.

    /**
     * Purpose: Returns the name of the Business Entity Primry Table
     * Notes:
     */
    DEFINE PUBLIC PROPERTY EntityTable AS CHARACTER NO-UNDO
    GET.

    /**
     * Purpose: Returns the names of the additional Business Entity Tables
     * Notes:   Comma delimited list
     */
    DEFINE PUBLIC PROPERTY EntityView AS CHARACTER NO-UNDO
    GET.

    /**
     * Purpose: Returns if the Entity View tables should be joined to the Entity Table
     * Notes:   Comma delimited list
     */
    DEFINE PUBLIC PROPERTY EntityJoin AS CHARACTER NO-UNDO
    GET.

END INTERFACE.
