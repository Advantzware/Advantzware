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
    File        : CustomDataGridSections
    Purpose     : Represents the meta data for view events
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Oct 01 23:23:29 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Web.KendoUiBuilder.*        FROM PROPATH .
USING Consultingwerk.Web.KendoUiBuilder.Module.* FROM PROPATH .
USING Progress.Lang.*                            FROM PROPATH .

CLASS Consultingwerk.Web.KendoUiBuilder.Module.CustomDataGridSections
    INHERITS SimpleMetaDataObject:

    {Consultingwerk/JsonSerializableProperty11.i top CHARACTER "INIT 'topSection.html':U"} .
    {Consultingwerk/JsonSerializableProperty11.i middle CHARACTER "INIT 'middleSection.html':U"} .
    {Consultingwerk/JsonSerializableProperty11.i bottom CHARACTER "INIT 'bottomSection.html':U"} .

    /**
     * Purpose: Constructor for the CustomDataGridSections class
     * Notes:
     */
    CONSTRUCTOR PUBLIC CustomDataGridSections ():

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .
        THIS-OBJECT:SerializeNullValues = FALSE .

    END CONSTRUCTOR.

END CLASS.
