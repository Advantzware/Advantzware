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
    File        : ListDataSource
    Purpose     : List of DataSource objects
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 22 16:36:25 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Base.*                  FROM PROPATH .
USING Consultingwerk.Web.KendoUiBuilder.DataProvider.* FROM PROPATH .
USING Progress.Lang.*                                  FROM PROPATH .

CLASS Consultingwerk.Web.KendoUiBuilder.DataProvider.ListDataSource
    INHERITS GenericList:

    {Consultingwerk/Framework/Base/GenericList.i DataSource}

    /**
     * Purpose: Adds a data source to the list
     * Notes:
     * @param pcName The name of the data source to add
     * @return The new data source
     */
    METHOD PUBLIC DataSource Add (pcName AS CHARACTER):

        RETURN THIS-OBJECT:Add (NEW DataSource (pcName)) .

    END METHOD.

END CLASS.
