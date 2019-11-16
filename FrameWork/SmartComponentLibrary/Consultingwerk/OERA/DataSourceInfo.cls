/**********************************************************************
 * Copyright (C) 2006-2017 by Consultingwerk Ltd. ("CW") -            *
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
    File        : DataSourceInfo
    Purpose     : General purpose Datasource (Business Entity) view parameter
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jan 10 19:54:33 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*      FROM PROPATH .
USING Consultingwerk.OERA.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

CLASS Consultingwerk.OERA.DataSourceInfo
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    IMPLEMENTS IDataSourceInfo
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i EntityName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i EntityTable CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i EntityView CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i EntityJoin CHARACTER} .

    /**
     * Purpose: Constructor for the DataSourceInfo class
     * Notes:
     */
    CONSTRUCTOR PUBLIC DataSourceInfo ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the DataSourceInfo class
     * Notes:
     * @param pcEntityName The name of the Business Entity
     * @param pcEntityTable The name of the primary entity table
     */
    CONSTRUCTOR PUBLIC DataSourceInfo (pcEntityName AS CHARACTER,
                                       pcEntityTable AS CHARACTER):

        THIS-OBJECT () .

        ASSIGN THIS-OBJECT:EntityName  = pcEntityName
               THIS-OBJECT:EntityTable = pcEntityTable .

    END CONSTRUCTOR .

    /**
     * Purpose: Constructor for the DataSourceInfo class
     * Notes:
     * @param pcEntityName The name of the Business Entity
     * @param pcEntityTable The name of the primary entity table
     * @param pcEntityView The name of the additinal entity tables
     */
    CONSTRUCTOR PUBLIC DataSourceInfo (pcEntityName AS CHARACTER,
                                       pcEntityTable AS CHARACTER,
                                       pcEntityView AS CHARACTER):

        THIS-OBJECT () .

        ASSIGN THIS-OBJECT:EntityName  = pcEntityName
               THIS-OBJECT:EntityTable = pcEntityTable
               THIS-OBJECT:EntityView  = pcEntityView .

        IF pcEntityView > "":U THEN
            ASSIGN THIS-OBJECT:EntityJoin = TRIM (FILL ("yes,":U, NUM-ENTRIES (pcEntityTable)), ",":U) .

    END CONSTRUCTOR .

    /**
     * Purpose: Constructor for the DataSourceInfo class
     * Notes:
     * @param pcEntityName The name of the Business Entity
     * @param pcEntityTable The name of the primary entity table
     * @param pcEntityView The name of the additinal entity tables
     * @param pcEntityJoin Comma delimited list of YES/NO describing if the Entity View tables will be joined with the entity table
     */
    CONSTRUCTOR PUBLIC DataSourceInfo (pcEntityName AS CHARACTER,
                                       pcEntityTable AS CHARACTER,
                                       pcEntityView AS CHARACTER,
                                       pcEntityJoin AS CHARACTER):

        THIS-OBJECT () .

        ASSIGN THIS-OBJECT:EntityName  = pcEntityName
               THIS-OBJECT:EntityTable = pcEntityTable
               THIS-OBJECT:EntityView  = pcEntityView
               THIS-OBJECT:EntityJoin  = pcEntityJoin .

    END CONSTRUCTOR .

END CLASS.
