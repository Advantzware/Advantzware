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
    File        : ListField
    Purpose     : List of Field objects
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

CLASS Consultingwerk.Web.KendoUiBuilder.DataProvider.ListField
    INHERITS GenericList:

    {Consultingwerk/Framework/Base/GenericList.i Field}

    /**
     * Purpose: Adds a field to the list
     * Notes:
     * @param pcName The name of the field to add
     * @return The new field
     */
    METHOD PUBLIC Field Add (pcName AS CHARACTER):

        RETURN THIS-OBJECT:Add (NEW Field (pcName)) .

    END METHOD.

    /**
     * Purpose: Adds a field to the list
     * Notes:
     * @param pcName The Name of the Field
     * @param pcLabel The Label of the Field
     * @param pcEditorType The editor type of the field
     * @param pcSemanticType The semantic type of the field
     * @param pcFormat The field format
     * @return The new field
     */
    METHOD PUBLIC Field Add (pcName AS CHARACTER,
                             pcLabel AS CHARACTER,
                             pcEditorType AS CHARACTER,
                             pcSemanticType AS CHARACTER,
                             pcFormat AS CHARACTER):

        RETURN THIS-OBJECT:Add (NEW Field (pcName, pcLabel, pcEditorType, pcSemanticType, pcFormat)) .

    END METHOD.

END CLASS.
