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
    File        : IModelFilterTarget
    Purpose     : Abstracts the access from the ModelFilter classes to the
                  TableModel for invoking the Filter
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jun 17 23:47:23 CEST 2013
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.*                  FROM PROPATH .
USING Consultingwerk.OERA.*             FROM PROPATH .
USING Consultingwerk.OERA.ModelFilter.* FROM PROPATH .
USING Progress.Lang.*                   FROM PROPATH .

INTERFACE Consultingwerk.OERA.ModelFilter.IModelFilterTarget:

    /**
     * Purpose: Retrieves data using the QueryExpression
     * Notes:
     * @param poQueryExpression The QueryExpression to retrieve data data for
     * @return The reference to the TableModel itself
     */
    METHOD PUBLIC TableModel AddQueryExpression (poQueryExpression AS IQueryExpression) .

END INTERFACE.
