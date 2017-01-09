/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ISortableDataSource
    Purpose     : Indicates is a SmartDataAdapter class supports sorting
                  by setting the QuerySort property
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jul 24 12:26:48 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.* FROM ASSEMBLY .

INTERFACE Consultingwerk.SmartComponents.Interfaces.ISortableDataSource:

    /**
     * Purpose: Controls the sort-order used at the client
     * Notes:
     */
    DEFINE PUBLIC PROPERTY QuerySort AS CHARACTER NO-UNDO
    GET.
    SET.

END INTERFACE.