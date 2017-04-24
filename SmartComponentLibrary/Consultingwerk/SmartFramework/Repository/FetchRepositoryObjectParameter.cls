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
    File        : FetchRepositoryObjectParameter
    Purpose     : Parameter object for the FetchRepositoryObject method of
                  the ISmartRepositoryService service
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Dec 29 22:34:24 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                           FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.* FROM PROPATH .
USING Progress.Lang.*                            FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Repository.FetchRepositoryObjectParameter
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i ContainerName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i FetchChildDetails LOGICAL "INITIAL TRUE"} .

    /**
     * Purpose: Constructor for the FetchRepositoryObjectParameter class
     * Notes:
     */
    CONSTRUCTOR PUBLIC FetchRepositoryObjectParameter ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the FetchRepositoryObjectParameter class
     * Notes:
     * @param pcContainerName The name of the container object to retrieve from the repository
     */
    CONSTRUCTOR PUBLIC FetchRepositoryObjectParameter (pcContainerName AS CHARACTER):

        THIS-OBJECT () .

        ASSIGN THIS-OBJECT:ContainerName = pcContainerName .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the FetchRepositoryObjectParameter class
     * Notes:
     * @param pcContainerName The name of the container object to retrieve from the repository
     * @param plFetchChildDetails Logical value indicating if the details about the contained objects should also be retrieved
     */
    CONSTRUCTOR PUBLIC FetchRepositoryObjectParameter (pcContainerName AS CHARACTER,
                                                       plFetchChildDetails AS LOGICAL):

        THIS-OBJECT () .

        ASSIGN THIS-OBJECT:ContainerName     = pcContainerName
               THIS-OBJECT:FetchChildDetails = plFetchChildDetails.

    END CONSTRUCTOR.

END CLASS.
