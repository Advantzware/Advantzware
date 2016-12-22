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
    File        : ArrayHolder.i
    Purpose     : Core implementation of the ArrayHolder methods

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed Aug 05 01:24:12 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*            FROM PROPATH .
USING Consultingwerk.Assertion.*  FROM PROPATH .
USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.{&ClassNamePrefix}ArrayHolder 
&IF '{&FrameworkSerializationType}' EQ 'JSON' &THEN
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i Value "{&DataType} EXTENT"} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ArrayHolder class
        Notes:
        @param pValue The initial value
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC {&ClassNamePrefix}ArrayHolder (pValue AS {&DataType} EXTENT):
        THIS-OBJECT ().

        THIS-OBJECT:Value = pValue . 

    END CONSTRUCTOR. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ArrayHolder class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC {&ClassNamePrefix}ArrayHolder ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR. 

    /*------------------------------------------------------------------------------
        Purpose: Removes the last element from an array and returns that element 
        Notes:   
        @return The removed element from the array
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC {&DataType} Pop ():
        
        DEFINE VARIABLE xNewArray AS {&DataType} EXTENT NO-UNDO .
        DEFINE VARIABLE xElement  AS {&DataType}        NO-UNDO .
        DEFINE VARIABLE i         AS INTEGER            NO-UNDO .

        IF EXTENT (THIS-OBJECT:Value) >= 1 THEN .
        ELSE 
            UNDO, THROW NEW InvalidValueException (STRING (EXTENT (THIS-OBJECT:Value)), "extent size of array"{&TRAN}) .

        ASSIGN xElement = THIS-OBJECT:Value[EXTENT (THIS-OBJECT:Value)] .

        IF EXTENT (THIS-OBJECT:Value) > 1 THEN DO:

            EXTENT (xNewArray) = EXTENT (THIS-OBJECT:Value) - 1 .

            DO i = 1 TO EXTENT (xNewArray):
                xNewArray[i] = THIS-OBJECT:Value[i] .
            END.
        END.

        THIS-OBJECT:Replace (xNewArray) .

        RETURN xElement .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Adds one element to the end of an array and returns the new length 
                 of the array. 
        Notes:   
        @param pNewElement The new element to add to the array
        @return The new length of the array
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC INTEGER Push (pNewElement AS {&DataType}):
        
        DEFINE VARIABLE xNewArray AS {&DataType} EXTENT NO-UNDO .
        DEFINE VARIABLE i         AS INTEGER            NO-UNDO .

        IF EXTENT (THIS-OBJECT:Value) >= 1 THEN 
            EXTENT (xNewArray) = EXTENT (THIS-OBJECT:Value) + 1 .
        ELSE 
            EXTENT (xNewArray) = 1 .

        DO i = 1 TO EXTENT (xNewArray) - 1:
            xNewArray[i] = THIS-OBJECT:Value[i] .
        END.

        xNewArray[EXTENT (xNewArray)] = pNewElement .

        THIS-OBJECT:Replace (xNewArray) .

        RETURN EXTENT (xNewArray) .

    END METHOD .    
        
    /*------------------------------------------------------------------------------
        Purpose: Replaces the array
        Notes:   Avoids the need to re-initialize the array to extent ? when the array 
                 size changes. Assigning Value directly is only possible when the 
                 array size remains 
        @param pNewValue The new array
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Replace (pNewValue AS {&DataType} EXTENT):
        
        EXTENT (THIS-OBJECT:Value) = ? . 
        
        ASSIGN THIS-OBJECT:Value = pNewValue . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Removes the first element from an array and returns that element. 
                 This method changes the length of the array. 
        Notes:   
        @return The removed element from the array
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC {&DataType} Shift ():
        
        DEFINE VARIABLE xNewArray AS {&DataType} EXTENT NO-UNDO .
        DEFINE VARIABLE xElement  AS {&DataType}        NO-UNDO .
        DEFINE VARIABLE i         AS INTEGER            NO-UNDO .

        IF EXTENT (THIS-OBJECT:Value) >= 1 THEN .
        ELSE 
            UNDO, THROW NEW InvalidValueException (STRING (EXTENT (THIS-OBJECT:Value)), "extent size of array"{&TRAN}) .

        ASSIGN xElement = THIS-OBJECT:Value[1] .

        IF EXTENT (THIS-OBJECT:Value) > 1 THEN DO:

            EXTENT (xNewArray) = EXTENT (THIS-OBJECT:Value) - 1 .

            DO i = 1 TO EXTENT (xNewArray):
                xNewArray[i] = THIS-OBJECT:Value[i + 1] .
            END.
        END.

        THIS-OBJECT:Replace (xNewArray) .

        RETURN xElement .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Adds one element to the beginning of an array and returns the new 
                 length of the array. 
        Notes:   
        @param pNewElement The new element to add to the array
        @return The new length of the array
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC INTEGER Unshift (pNewElement AS {&DataType}):
        
        DEFINE VARIABLE xNewArray AS {&DataType} EXTENT NO-UNDO .
        DEFINE VARIABLE i         AS INTEGER            NO-UNDO .

        IF EXTENT (THIS-OBJECT:Value) >= 1 THEN 
            EXTENT (xNewArray) = EXTENT (THIS-OBJECT:Value) + 1 .
        ELSE 
            EXTENT (xNewArray) = 1 .

        DO i = 1 TO EXTENT (THIS-OBJECT:Value):
            xNewArray[i + 1] = THIS-OBJECT:Value[i] .
        END.

        xNewArray[1] = pNewElement .

        THIS-OBJECT:Replace (xNewArray) .

        RETURN EXTENT (xNewArray) .

    END METHOD .

END CLASS.
