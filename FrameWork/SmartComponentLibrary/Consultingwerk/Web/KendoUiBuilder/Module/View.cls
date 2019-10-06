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
    File        : View
    Purpose     : Represents the meta data of a Kendo UI Builder View
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

CLASS Consultingwerk.Web.KendoUiBuilder.Module.View
    INHERITS MetaDataObject:

    {Consultingwerk/JsonSerializableProperty11.i viewType CHARACTER}
    {Consultingwerk/JsonSerializableProperty11.i extension CHARACTER "INIT 'view-factory.js':U"}
    {Consultingwerk/JsonSerializableProperty11.i events Consultingwerk.Web.KendoUiBuilder.Module.ViewEvents}
    {Consultingwerk/JsonSerializableProperty11.i children Consultingwerk.Web.KendoUiBuilder.Module.ListViewChildren}
    {Consultingwerk/JsonSerializableProperty11.i readonly LOGICAL "INIT 'false'"}

    /**
     * Purpose: Constructor for the View class
     * Notes:
     * @param pcName The name of the View
     * @param pcViewType The type of the View
     */
    CONSTRUCTOR PUBLIC View (pcName AS CHARACTER,
                             pcViewType AS CHARACTER):

        THIS-OBJECT () .

        THIS-OBJECT:name = pcName .
        THIS-OBJECT:viewType = pcViewType .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the View class
     * Notes:
     */
    CONSTRUCTOR PUBLIC VIEW ():
        SUPER ("view":U).

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

        THIS-OBJECT:events = NEW ViewEvents () .
        THIS-OBJECT:children = NEW ListViewChildren () .

    END CONSTRUCTOR.

END CLASS.
