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
    File        : ISemanticTypesProvider
    Purpose     : Interface for services that provide semantic types to
                  the Business Entity Designer
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Aug 02 00:40:11 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.* FROM PROPATH .
USING Progress.Lang.*  FROM PROPATH .

INTERFACE Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.ISemanticTypesProvider:

    /**
     * Purpose: Returns the Default Editor for the given Semantic Type
     * Notes:
     * @param pcSemanticType The semantic type to return the default editor for
     * @return The editor type for the semantic type
     */
    METHOD PUBLIC CHARACTER DefaultEditorForSemanticType (pcSemanticType AS CHARACTER).

    /**
     * Purpose: Returns the default Semantic Type for the given ABL data type
     * Notes:
     * @param pcDataType The ABL data type to return the semantic type for
     * @return The semantic type
     */
    METHOD PUBLIC CHARACTER DefaultSemanticTypeForDataType (pcDataType AS CHARACTER).

    /**
     * Purpose: Returns the list of Semantic Types to the caller
     * Notes:
     * @param pcDataType The Data Type of the Field
     * @return The List of NameValuePair's with the available semantic Types
     */
    METHOD PUBLIC ListNameValuePair GetSemanticTypes (pcDataType AS CHARACTER).

END INTERFACE.
