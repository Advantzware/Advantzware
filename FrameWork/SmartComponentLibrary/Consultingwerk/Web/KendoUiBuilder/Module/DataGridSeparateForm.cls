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
    File        : DataGridSeparateForm
    Purpose     : Represents the meta data for the Data Grid with a Separate
                  Form
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Oct 02 00:19:14 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Web.KendoUiBuilder.*        FROM PROPATH .
USING Consultingwerk.Web.KendoUiBuilder.Module.* FROM PROPATH .
USING Progress.Lang.*                            FROM PROPATH .

CLASS Consultingwerk.Web.KendoUiBuilder.Module.DataGridSeparateForm
    INHERITS DataGridForm
    IMPLEMENTS IViewChild:

    /**
     * Purpose: Constructor for the DataGridForm class
     * Notes:
     * @param pcName The name of the View
     */
    CONSTRUCTOR PUBLIC DataGridSeparateForm (pcName AS CHARACTER):

        THIS-OBJECT () .

        THIS-OBJECT:name = pcName .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the DataGridForm class
     * Notes:
     */
    CONSTRUCTOR PUBLIC DataGridSeparateForm ():
        SUPER ().

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

        THIS-OBJECT:type = "data-grid-separate-form":U .

    END CONSTRUCTOR.

END CLASS.
