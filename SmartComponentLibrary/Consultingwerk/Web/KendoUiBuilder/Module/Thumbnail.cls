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
    File        : Thumbnail
    Purpose     :
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

CLASS Consultingwerk.Web.KendoUiBuilder.Module.Thumbnail
    INHERITS SimpleMetaDataObject:

    {Consultingwerk/JsonSerializableProperty11.i icon CHARACTER "INIT 'fa-cogs':U"} .
    {Consultingwerk/JsonSerializableProperty11.i color CHARACTER "INIT '#ffffff':U"} .
    {Consultingwerk/JsonSerializableProperty11.i background CHARACTER "INIT '#7092be':U"} .

    /**
     * Purpose: Constructor for the Thumbnail class
     * Notes:
     */
    CONSTRUCTOR PUBLIC Thumbnail ():

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.

END CLASS.
