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
    File        : Field
    Purpose     : Represents the meta data of a Kendo UI Builder Grid Column
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Sep 21 17:56:37 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Web.KendoUiBuilder.*        FROM PROPATH .
USING Consultingwerk.Web.KendoUiBuilder.Module.* FROM PROPATH .
USING Progress.Lang.*                            FROM PROPATH .

CLASS Consultingwerk.Web.KendoUiBuilder.Module.Column
    INHERITS SimpleMetaDataObject:

    {Consultingwerk/JsonSerializableProperty11.i field CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i title CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i format CHARACTER} .
    {Consultingwerk/JsonSerializableProperty11.i encoded LOGICAL "INIT ?"} .
    {Consultingwerk/JsonSerializableProperty11.i width INTEGER "INIT ?"} .

    /**
     * Purpose: Constructor for the Field class
     * Notes:
     * @param pcField The Name of the Field
     * @param pcTitle The Label of the Field
     * @param pcFormat The Format of the field
     * @param plEncoded Is the field Encoded
     * @param piWidth The Width of the Field
     */
    CONSTRUCTOR PUBLIC Column (pcField AS CHARACTER,
                               pcTitle AS CHARACTER,
                               pcFormat AS CHARACTER,
                               plEncoded AS LOGICAL,
                               piWidth AS INTEGER):

        THIS-OBJECT () .

        ASSIGN THIS-OBJECT:field   = pcField
               THIS-OBJECT:title   = pcTitle
               THIS-OBJECT:format  = pcFormat
               THIS-OBJECT:encoded = plEncoded
               THIS-OBJECT:width   = piWidth .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the Field class
     * Notes:
     * @param pcField The Name of the Field
     * @param pcTitle The Label of the Field
     */
    CONSTRUCTOR PUBLIC Column (pcField AS CHARACTER,
                               pcTitle AS CHARACTER):

        THIS-OBJECT () .

        ASSIGN THIS-OBJECT:field = pcField
               THIS-OBJECT:title = pcTitle .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the Field class
     * Notes:
     * @param pcField The Name of the Field
     */
    CONSTRUCTOR PUBLIC Column (pcField AS CHARACTER):

        THIS-OBJECT () .

        THIS-OBJECT:field = pcField .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the Field class
     * Notes:
     */
    CONSTRUCTOR PUBLIC Column ():
        SUPER ().

        THIS-OBJECT:format = "~{0}":U .

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .
        THIS-OBJECT:SerializeNullValues = FALSE .

    END CONSTRUCTOR.

END CLASS.
