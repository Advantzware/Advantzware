/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ListInvokableMethodDescriptor
    Purpose     : Dictionary of IInvokableMethodDescriptor
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jul 08 16:07:39 CEST 2015
    Notes       : http://confluence.consultingwerkcloud.com/wiki/display/SCL/Business+Entity+Descriptor
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Base.*                FROM PROPATH .
USING Consultingwerk.OERA.BusinessEntityDescriptor.* FROM PROPATH .
USING Progress.Lang.*                                FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.OERA.BusinessEntityDescriptor.ListInvokableMethodDescriptor
    INHERITS GenericList
    {&SERIALIZABLE}:

    {Consultingwerk/Framework/Base/GenericList.i IInvokableMethodDescriptor}

    /**
     * Purpose: Adds an InvokableMethodDescriptor instance to the list
     * Notes:
     * @param pcName The name of the invokable method
     * @param pcParameterClassName The name of the parameter class
     * @param plDatasetInput Receive dataset from client
     * @param plDatasetOutput Return dataset to client
     * @return The new IInvokableMethodDescriptor instance
     */
    METHOD PUBLIC IInvokableMethodDescriptor Add (pcName AS CHARACTER,
                                                  pcParameterClassName AS CHARACTER,
                                                  plDatasetInput AS LOGICAL,
                                                  plDatasetOutput AS LOGICAL):

        RETURN THIS-OBJECT:Add (NEW InvokableMethodDescriptor (pcName,
                                                               pcParameterClassName,
                                                               plDatasetInput,
                                                               plDatasetOutput)) .

    END METHOD .

    /**
     * Purpose: Returns true when the List contains an Item with the given Name
     * Notes:
     * @param pcName The name to return
     * @return Logical value indicating if the List contains an Item with the given Name
     */
    METHOD PUBLIC LOGICAL ContainsName (pcName AS CHARACTER):

        {Consultingwerk/linqABL.i IInvokableMethodDescriptor oItem in THIS-OBJECT
                    WHERE Name = pcName}

            RETURN TRUE .
        END.

        RETURN FALSE .

    END METHOD .

    /**
     * Purpose: Returns the Item from the List with the given Name
     * Notes:
     * @param pcName The name to return
     * @return The Item from the List with the given Name
     */
    METHOD PUBLIC IInvokableMethodDescriptor GetItem (pcName AS CHARACTER):

        {Consultingwerk/linqABL.i IInvokableMethodDescriptor oItem in THIS-OBJECT
                    WHERE Name = pcName}

            RETURN oItem .
        END.

        RETURN ? .

    END METHOD .

END CLASS.
