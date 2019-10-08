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
    File        : FilterParameter
    Purpose     : JSON serializable class that can be used to deserialize
                  the complex filter parameter used by Rollbase, DataDirect
                  OpenAccess SDK for OpenEdge etc.
    Syntax      :
    Description : Passed in as a character for the "filter" parameter of
                  the Business Entity classes/OpenEdge Service objects
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Oct 28 17:15:27 CET 2014
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                 FROM PROPATH .
USING Consultingwerk.OERA.OpenAccess.* FROM PROPATH .
USING Progress.Lang.*                  FROM PROPATH .

CLASS Consultingwerk.OERA.FilterParameter
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    :

    {Consultingwerk/JsonSerializableProperty.i top INTEGER} .
    {Consultingwerk/JsonSerializableProperty.i skip INTEGER} .
    {Consultingwerk/JsonSerializableProperty.i ablFilter CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i tableRef CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i viewTables CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i id CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i orderBy CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FilterParameter class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC FilterParameter ():
        SUPER ().

        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

        THIS-OBJECT:UseSerializedTypeInformation = FALSE .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FilterParameter class
        Notes:
        @param pcFilter The filter parameter as passed to the read method of an OpenEdge service object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC FilterParameter (pcFilter AS CHARACTER):
        THIS-OBJECT ().

        DEFINE VARIABLE lcFilter AS LONGCHAR NO-UNDO .

        FIX-CODEPAGE (lcFilter) = "utf-8":U .

        ASSIGN lcFilter = pcFilter .

        THIS-OBJECT:Deserialize (lcFilter) .

    END CONSTRUCTOR.

END CLASS.
