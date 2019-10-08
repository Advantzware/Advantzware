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
    File        : Module
    Purpose     : Represents the meta data of a Kendo UI Builder Module
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Sep 21 17:56:37 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Web.KendoUiBuilder.*              FROM PROPATH .
USING Consultingwerk.Web.KendoUiBuilder.Module.*       FROM PROPATH .
USING Progress.Lang.*                                  FROM PROPATH .

CLASS Consultingwerk.Web.KendoUiBuilder.Module.Module
    INHERITS MetaDataObject:

    {Consultingwerk/JsonSerializableProperty11.i description CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i thumbnail Consultingwerk.Web.KendoUiBuilder.Module.Thumbnail} .
    {Consultingwerk/JsonSerializableProperty11.i moduleType CHARACTER "INIT 'default':U"} .
    {Consultingwerk/JsonSerializableProperty11.i children Consultingwerk.Web.KendoUiBuilder.Module.ListView} .

    /**
     * Purpose: Constructor for the Module class
     * Notes:
     * @param pcName The name of the Module
     */
    CONSTRUCTOR PUBLIC Module (pcName AS CHARACTER):

        THIS-OBJECT () .

        THIS-OBJECT:name = pcName .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the Module class
     * Notes:
     */
    CONSTRUCTOR PUBLIC Module ():
        SUPER ("module":U).

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

        THIS-OBJECT:thumbnail = NEW Thumbnail () .
        THIS-OBJECT:children = NEW ListView () .

    END CONSTRUCTOR.

END CLASS.
