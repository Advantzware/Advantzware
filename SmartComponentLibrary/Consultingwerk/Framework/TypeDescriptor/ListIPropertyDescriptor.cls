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
    File        : ListIPropertyDescriptor
    Purpose     : Generic List of IPropertyDescriptor instances
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Apr 29 07:00:36 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Base.*           FROM PROPATH .
USING Consultingwerk.Framework.TypeDescriptor.* FROM PROPATH .  
USING Progress.Lang.*                           FROM PROPATH .

CLASS Consultingwerk.Framework.TypeDescriptor.ListIPropertyDescriptor 
    INHERITS GenericList: 

    {Consultingwerk/Framework/Base/GenericList.i IPropertyDescriptor}

    /*------------------------------------------------------------------------------
        Purpose: Adds a new PropertyDescriptor to the List
        Notes:
        @param pcDefiningTypeName The name of the class that defines this property
        @param pcPropertyName The name for the property
        @return The new PropertyDescriptor instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC PropertyDescriptor Add (pcDefiningTypeName AS CHARACTER,
                                          pcPropertyName AS CHARACTER):
                                              
        RETURN CAST (THIS-OBJECT:Add (NEW PropertyDescriptor (pcDefiningTypeName,
                                                              pcPropertyName)),
                     PropertyDescriptor) .

    END METHOD. 

    /*------------------------------------------------------------------------------
        Purpose: Adds a new PropertyDescriptor to the List
        Notes:
        @param pcDefiningTypeName The name of the class that defines this property
        @param pcPropertyName The name for the property
        @param pcPropertyLabel The label for the property
        @return The new PropertyDescriptor instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC PropertyDescriptor Add (pcDefiningTypeName AS CHARACTER,
                                          pcPropertyName AS CHARACTER,
                                          pcPropertyLabel AS CHARACTER):

        RETURN CAST (THIS-OBJECT:Add (NEW PropertyDescriptor (pcDefiningTypeName,
                                                              pcPropertyName,
                                                              pcPropertyLabel)),
                     PropertyDescriptor) .

    END METHOD. 

    /*------------------------------------------------------------------------------
        Purpose: Adds a new PropertyDescriptor to the List
        Notes:
        @param pcDefiningTypeName The name of the class that defines this property
        @param pcPropertyName The name for the property
        @param plVisible If the property should be visible
        @param plEditable If the property should be editable
        @return The new PropertyDescriptor instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC PropertyDescriptor Add (pcDefiningTypeName AS CHARACTER,
                                          pcPropertyName AS CHARACTER,
                                          plVisible AS LOGICAL, 
                                          plEditable AS LOGICAL):

        RETURN CAST (THIS-OBJECT:Add (NEW PropertyDescriptor (pcDefiningTypeName,
                                                              pcPropertyName,
                                                              plVisible, 
                                                              plEditable)),
                     PropertyDescriptor) .

    END METHOD. 

    /*------------------------------------------------------------------------------
        Purpose: Adds a new PropertyDescriptor to the List
        Notes:
        @param pcDefiningTypeName The name of the class that defines this property
        @param pcPropertyName The name for the property
        @param pcPropertyLabel The label for the property
        @param plVisible If the property should be visible
        @param plEditable If the property should be editable
        @return The new PropertyDescriptor instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC PropertyDescriptor Add (pcDefiningTypeName AS CHARACTER,
                                          pcPropertyName AS CHARACTER,
                                          pcPropertyLabel AS CHARACTER,
                                          plVisible AS LOGICAL, 
                                          plEditable AS LOGICAL):

        RETURN CAST (THIS-OBJECT:Add (NEW PropertyDescriptor (pcDefiningTypeName,
                                                              pcPropertyName,
                                                              pcPropertyLabel,
                                                              plVisible, 
                                                              plEditable)),
                     PropertyDescriptor) .

    END METHOD. 

    /*------------------------------------------------------------------------------
        Purpose: Adds a new PropertyDescriptor to the List
        Notes:
        @param pcDefiningTypeName The name of the class that defines this property
        @param pcPropertyName The name for the property
        @param poEditorType The type of editor to use for the property
        @return The new PropertyDescriptor instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC PropertyDescriptor Add (pcDefiningTypeName AS CHARACTER,
                                          pcPropertyName AS CHARACTER,
                                          poEditorType AS EditorTypeEnum):

        RETURN CAST (THIS-OBJECT:Add (NEW PropertyDescriptor (pcDefiningTypeName,
                                                              pcPropertyName,
                                                              poEditorType)),
                     PropertyDescriptor) .

    END METHOD. 

    /*------------------------------------------------------------------------------
        Purpose: Adds a new PropertyDescriptor to the List
        Notes:
        @param pcDefiningTypeName The name of the class that defines this property
        @param pcPropertyName The name for the property
        @param pcPropertyLabel The label for the property
        @param poEditorType The type of editor to use for the property
        @return The new PropertyDescriptor instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC PropertyDescriptor Add (pcDefiningTypeName AS CHARACTER,
                                          pcPropertyName AS CHARACTER,
                                          pcPropertyLabel AS CHARACTER,
                                          poEditorType AS EditorTypeEnum):

        RETURN CAST (THIS-OBJECT:Add (NEW PropertyDescriptor (pcDefiningTypeName,
                                                              pcPropertyName,
                                                              pcPropertyLabel,
                                                              poEditorType)),
                     PropertyDescriptor) .

    END METHOD. 

    /*------------------------------------------------------------------------------
        Purpose: Adds a new PropertyDescriptor to the List
        Notes:
        @param pcDefiningTypeName The name of the class that defines this property
        @param pcPropertyName The name for the property
        @param pcPropertyLabel The label for the property
        @param plVisible If the property should be visible
        @param plEditable If the property should be editable
        @param poEditorType The type of editor to use for the property
        @return The new PropertyDescriptor instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC PropertyDescriptor Add (pcDefiningTypeName AS CHARACTER,
                                          pcPropertyName AS CHARACTER,
                                          pcPropertyLabel AS CHARACTER,
                                          plVisible AS LOGICAL, 
                                          plEditable AS LOGICAL,
                                          poEditorType AS EditorTypeEnum):

        RETURN CAST (THIS-OBJECT:Add (NEW PropertyDescriptor (pcDefiningTypeName,
                                                              pcPropertyName,
                                                              pcPropertyLabel,
                                                              plVisible, 
                                                              plEditable,
                                                              poEditorType)),
                     PropertyDescriptor) .

    END METHOD. 

    /*------------------------------------------------------------------------------
        Purpose: Adds a new PropertyDescriptor to the List
        Notes:
        @param pcDefiningTypeName The name of the class that defines this property
        @param pcPropertyName The name for the property
        @param pcPropertyLabel The label for the property
        @param pcListItemPairs The list item pairs (comma delimited) to use for this property
        @return The new PropertyDescriptor instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC PropertyDescriptor Add (pcDefiningTypeName AS CHARACTER,
                                          pcPropertyName AS CHARACTER,
                                          pcPropertyLabel AS CHARACTER,
                                          pcListItemPairs AS CHARACTER):

        RETURN CAST (THIS-OBJECT:Add (NEW PropertyDescriptor (pcDefiningTypeName,
                                                              pcPropertyName,
                                                              pcPropertyLabel,
                                                              pcListItemPairs)),
                     PropertyDescriptor) .

    END METHOD. 

    /*------------------------------------------------------------------------------
        Purpose: Adds a new PropertyDescriptor to the List
        Notes:
        @param pcDefiningTypeName The name of the class that defines this property
        @param pcPropertyName The name for the property
        @param pcPropertyLabel The label for the property
        @param plVisible If the property should be visible
        @param plEditable If the property should be editable
        @param pcListItemPairs The list item pairs (comma delimited) to use for this property
        @return The new PropertyDescriptor instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC PropertyDescriptor Add (pcDefiningTypeName AS CHARACTER,
                                          pcPropertyName AS CHARACTER,
                                          pcPropertyLabel AS CHARACTER,
                                          plVisible AS LOGICAL, 
                                          plEditable AS LOGICAL,
                                          pcListItemPairs AS CHARACTER):

        RETURN CAST (THIS-OBJECT:Add (NEW PropertyDescriptor (pcDefiningTypeName,
                                                              pcPropertyName,
                                                              pcPropertyLabel,
                                                              plVisible, 
                                                              plEditable,
                                                              pcListItemPairs)),
                     PropertyDescriptor) .

    END METHOD. 

    /*------------------------------------------------------------------------------
        Purpose: Adds a new PropertyDescriptor to the List
        Notes:
        @param pcDefiningTypeName The name of the class that defines this property
        @param pcPropertyName The name for the property
        @param poEditorType The type of editor to use for the property
        @param pcListItemPairs The list item pairs (comma delimited) to use for this property
        @return The new PropertyDescriptor instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC PropertyDescriptor Add (pcDefiningTypeName AS CHARACTER,
                                          pcPropertyName AS CHARACTER,
                                          poEditorType AS EditorTypeEnum,
                                          pcListItemPairs AS CHARACTER):

        RETURN CAST (THIS-OBJECT:Add (NEW PropertyDescriptor (pcDefiningTypeName,
                                                              pcPropertyName,
                                                              poEditorType,
                                                              pcListItemPairs)),
                     PropertyDescriptor) .

    END METHOD. 

    /*------------------------------------------------------------------------------
        Purpose: Adds a new PropertyDescriptor to the List
        Notes:
        @param pcDefiningTypeName The name of the class that defines this property
        @param pcPropertyName The name for the property
        @param pcPropertyLabel The label for the property
        @param poEditorType The type of editor to use for the property
        @param pcListItemPairs The list item pairs (comma delimited) to use for this property
        @return The new PropertyDescriptor instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC PropertyDescriptor Add (pcDefiningTypeName AS CHARACTER,
                                          pcPropertyName AS CHARACTER,
                                          pcPropertyLabel AS CHARACTER,
                                          poEditorType AS EditorTypeEnum,
                                          pcListItemPairs AS CHARACTER):

        RETURN CAST (THIS-OBJECT:Add (NEW PropertyDescriptor (pcDefiningTypeName,
                                                              pcPropertyName,
                                                              pcPropertyLabel,
                                                              poEditorType,
                                                              pcListItemPairs)),
                     PropertyDescriptor) .

    END METHOD. 

    /*------------------------------------------------------------------------------
        Purpose: Adds a new PropertyDescriptor to the List
        Notes:
        @param pcDefiningTypeName The name of the class that defines this property
        @param pcPropertyName The name for the property
        @param pcPropertyLabel The label for the property
        @param plVisible If the property should be visible
        @param plEditable If the property should be editable
        @param poEditorType The type of editor to use for the property
        @param pcListItemPairs The list item pairs (comma delimited) to use for this property
        @return The new PropertyDescriptor instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC PropertyDescriptor Add (pcDefiningTypeName AS CHARACTER,
                                          pcPropertyName AS CHARACTER,
                                          pcPropertyLabel AS CHARACTER,
                                          plVisible AS LOGICAL, 
                                          plEditable AS LOGICAL,
                                          poEditorType AS EditorTypeEnum,
                                          pcListItemPairs AS CHARACTER):

        RETURN CAST (THIS-OBJECT:Add (NEW PropertyDescriptor (pcDefiningTypeName,
                                                              pcPropertyName,
                                                              pcPropertyLabel,
                                                              plVisible, 
                                                              plEditable,
                                                              poEditorType,
                                                              pcListItemPairs)),
                     PropertyDescriptor) .

    END METHOD. 

END CLASS.
