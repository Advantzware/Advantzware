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
    File        : DataGridEvents
    Purpose     : Represents the meta data for data grid events
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

CLASS Consultingwerk.Web.KendoUiBuilder.Module.DataGridEvents
    INHERITS SimpleMetaDataObject:

    {Consultingwerk/JsonSerializableProperty11.i onRowSelect CHARACTER "INIT 'onRowSelect':U"} .

    /**
     * Purpose: Constructor for the DataGridEvents class
     * Notes:
     */
    CONSTRUCTOR PUBLIC DataGridEvents ():

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .
        THIS-OBJECT:SerializeNullValues = FALSE .

    END CONSTRUCTOR.

END CLASS.
