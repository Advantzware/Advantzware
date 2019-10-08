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
    File        : ListRelationFieldPair
    Purpose     : List of Relation Field Pair objects
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

CLASS Consultingwerk.Web.KendoUiBuilder.DataProvider.ListRelationFieldPair
    INHERITS GenericList:

    {Consultingwerk/Framework/Base/GenericList.i RelationFieldPair}

    /**
     * Purpose: Adds a relation to the list
     * Notes:
     * @param pcParentFieldName The Name of the parent field
     * @param pcChildFieldName The Name of the child field
     * @return The new relation
     */
    METHOD PUBLIC RelationFieldPair Add (pcParentFieldName AS CHARACTER,
                                         pcChildFieldName AS CHARACTER):

        RETURN THIS-OBJECT:Add (NEW RelationFieldPair (pcParentFieldName, pcChildFieldName)) .

    END METHOD.

END CLASS.
