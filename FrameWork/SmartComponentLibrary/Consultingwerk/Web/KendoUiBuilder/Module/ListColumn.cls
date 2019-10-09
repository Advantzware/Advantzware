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
    File        : ListColumn
    Purpose     : List of Column objects
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

CLASS Consultingwerk.Web.KendoUiBuilder.Module.ListColumn
    INHERITS GenericList:

    {Consultingwerk/Framework/Base/GenericList.i Column}

    /**
     * Purpose: Adds a Column to the list
     * Notes:
     * @param pcField The Name of the Field
     * @return The new Column
     */
    METHOD PUBLIC Column Add (pcField AS CHARACTER):

        RETURN THIS-OBJECT:Add (NEW Column (pcField)) .

    END METHOD.

    /**
     * Purpose: Adds a Column to the list
     * Notes:
     * @param pcField The Name of the Field
     * @param pcTitle The Label of the Field
     * @return The new Column
     */
    METHOD PUBLIC Column Add (pcField AS CHARACTER,
                              pcTitle AS CHARACTER):

        RETURN THIS-OBJECT:Add (NEW Column (pcField, pcTitle)) .

    END METHOD.

    /**
     * Purpose: Adds a Column to the list
     * Notes:
     * @param pcField The Name of the Field
     * @param pcTitle The Label of the Field
     * @param pcFormat The Format of the field
     * @param plEncoded Is the field Encoded
     * @param piWidth The Width of the Field
     * @return The new Column
     */
    METHOD PUBLIC Column Add (pcField AS CHARACTER,
                              pcTitle AS CHARACTER,
                              pcFormat AS CHARACTER,
                              plEncoded AS LOGICAL,
                              piWidth AS INTEGER):

        RETURN THIS-OBJECT:Add (NEW Column (pcField, pcTitle, pcFormat, plEncoded, piWidth)) .

    END METHOD.

END CLASS.
