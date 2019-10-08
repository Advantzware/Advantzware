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
    File        : ListRelation
    Purpose     : List of Relation objects
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

CLASS Consultingwerk.Web.KendoUiBuilder.DataProvider.ListRelation
    INHERITS GenericList:

    {Consultingwerk/Framework/Base/GenericList.i Relation}

    /**
     * Purpose: Adds a relation to the list
     * Notes:
     * @param pcRelationName The name of the relation to add
     * @return The new relation
     */
    METHOD PUBLIC Relation Add (pcRelationName AS CHARACTER):

        RETURN THIS-OBJECT:Add (NEW Relation (pcRelationName)) .

    END METHOD.

    /**
     * Purpose: Adds a relation to the list
     * Notes:
     * @param pcRelationName The Name of the Relation
     * @param pcParentName The name of the parent table
     * @param pcChildName The name of the child table
     * @return The new relation
     */
    METHOD PUBLIC Relation Add (pcRelationName AS CHARACTER,
                                pcParentName AS CHARACTER,
                                pcChildName AS CHARACTER):

        RETURN THIS-OBJECT:Add (NEW Relation (pcRelationName, pcParentName, pcChildName)) .

    END METHOD.

END CLASS.
