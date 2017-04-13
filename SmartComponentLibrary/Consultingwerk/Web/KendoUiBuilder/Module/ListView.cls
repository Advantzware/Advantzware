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
    File        : ListView
    Purpose     : List of View objects
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 22 16:36:25 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Base.*            FROM PROPATH .
USING Consultingwerk.Web.KendoUiBuilder.Module.* FROM PROPATH .
USING Progress.Lang.*                            FROM PROPATH .

CLASS Consultingwerk.Web.KendoUiBuilder.Module.ListView
    INHERITS GenericList:

    {Consultingwerk/Framework/Base/GenericList.i View}

    /**
     * Purpose: Adds a data source to the list
     * Notes:
     * @param pcName The name of the data source to add
     * @param pcViewType The type of the View
     * @return The new data source
     */
    METHOD PUBLIC View Add (pcName AS CHARACTER,
                            pcViewType AS CHARACTER):

        RETURN THIS-OBJECT:Add (NEW View (pcName, pcViewType)) .

    END METHOD.

END CLASS.
