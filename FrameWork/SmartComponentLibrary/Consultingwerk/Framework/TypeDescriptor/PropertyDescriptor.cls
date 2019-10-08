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
    File        : PropertyDescriptor
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Apr 29 07:26:24 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                          FROM PROPATH .
USING Consultingwerk.Framework.TypeDescriptor.* FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

&GLOBAL-DEFINE FrameworkSerializationType XML

CLASS Consultingwerk.Framework.TypeDescriptor.PropertyDescriptor IMPLEMENTS IPropertyDescriptor
    INHERITS XmlSerializable:

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the type that defines the property
        Notes:   Returns the name of a base class when a property is inherited
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i DefiningTypeName CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the property should be editable in a property sheed
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i Editable LOGICAL "INITIAL TRUE"} .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the type of editor control to use for the property visualization
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i EditorType Consultingwerk.Framework.TypeDescriptor.EditorTypeEnum} .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets a comma delimited list of name/value pairs for the property
        Notes:   Applies to EditorType = EditorTypeEnum:List
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i ListItemPairs CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the label for the property
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i PropertyLabel CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the property
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i PropertyName CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the property should be visible in a property sheet
        Notes:
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i Visible LOGICAL "INITIAL TRUE"} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertyDescriptor class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC PropertyDescriptor ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

        ASSIGN THIS-OBJECT:EditorType = EditorTypeEnum:Text
               THIS-OBJECT:Editable   = TRUE
               THIS-OBJECT:Visible    = TRUE .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertyDescriptor class
        Notes:
        @param pcDefiningTypeName The name of the class that defines this property
        @param pcPropertyName The name for the property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC PropertyDescriptor (pcDefiningTypeName AS CHARACTER,
                                           pcPropertyName AS CHARACTER):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:DefiningTypeName = pcDefiningTypeName
               THIS-OBJECT:PropertyName     = pcPropertyName .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertyDescriptor class
        Notes:
        @param pcDefiningTypeName The name of the class that defines this property
        @param pcPropertyName The name for the property
        @param pcPropertyLabel The label for the property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC PropertyDescriptor (pcDefiningTypeName AS CHARACTER,
                                           pcPropertyName AS CHARACTER,
                                           pcPropertyLabel AS CHARACTER):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:DefiningTypeName = pcDefiningTypeName
               THIS-OBJECT:PropertyName     = pcPropertyName
               THIS-OBJECT:PropertyLabel    = pcPropertyLabel .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertyDescriptor class
        Notes:
        @param pcDefiningTypeName The name of the class that defines this property
        @param pcPropertyName The name for the property
        @param plVisible If the property should be visible
        @param plEditable If the property should be editable
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC PropertyDescriptor (pcDefiningTypeName AS CHARACTER,
                                           pcPropertyName AS CHARACTER,
                                           plVisible AS LOGICAL,
                                           plEditable AS LOGICAL):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:DefiningTypeName = pcDefiningTypeName
               THIS-OBJECT:PropertyName     = pcPropertyName
               THIS-OBJECT:Editable         = plEditable
               THIS-OBJECT:Visible          = plVisible .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertyDescriptor class
        Notes:
        @param pcDefiningTypeName The name of the class that defines this property
        @param pcPropertyName The name for the property
        @param pcPropertyLabel The label for the property
        @param plVisible If the property should be visible
        @param plEditable If the property should be editable
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC PropertyDescriptor (pcDefiningTypeName AS CHARACTER,
                                           pcPropertyName AS CHARACTER,
                                           pcPropertyLabel AS CHARACTER,
                                           plVisible AS LOGICAL,
                                           plEditable AS LOGICAL):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:DefiningTypeName = pcDefiningTypeName
               THIS-OBJECT:PropertyName     = pcPropertyName
               THIS-OBJECT:PropertyLabel    = pcPropertyLabel
               THIS-OBJECT:Editable         = plEditable
               THIS-OBJECT:Visible          = plVisible .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertyDescriptor class
        Notes:
        @param pcDefiningTypeName The name of the class that defines this property
        @param pcPropertyName The name for the property
        @param poEditorType The type of editor to use for the property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC PropertyDescriptor (pcDefiningTypeName AS CHARACTER,
                                           pcPropertyName AS CHARACTER,
                                           poEditorType AS EditorTypeEnum):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:DefiningTypeName = pcDefiningTypeName
               THIS-OBJECT:PropertyName     = pcPropertyName
               THIS-OBJECT:EditorType       = poEditorType .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertyDescriptor class
        Notes:
        @param pcDefiningTypeName The name of the class that defines this property
        @param pcPropertyName The name for the property
        @param pcPropertyLabel The label for the property
        @param poEditorType The type of editor to use for the property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC PropertyDescriptor (pcDefiningTypeName AS CHARACTER,
                                           pcPropertyName AS CHARACTER,
                                           pcPropertyLabel AS CHARACTER,
                                           poEditorType AS EditorTypeEnum):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:DefiningTypeName = pcDefiningTypeName
               THIS-OBJECT:PropertyName     = pcPropertyName
               THIS-OBJECT:PropertyLabel    = pcPropertyLabel
               THIS-OBJECT:EditorType       = poEditorType.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertyDescriptor class
        Notes:
        @param pcDefiningTypeName The name of the class that defines this property
        @param pcPropertyName The name for the property
        @param pcPropertyLabel The label for the property
        @param plVisible If the property should be visible
        @param plEditable If the property should be editable
        @param poEditorType The type of editor to use for the property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC PropertyDescriptor (pcDefiningTypeName AS CHARACTER,
                                           pcPropertyName AS CHARACTER,
                                           pcPropertyLabel AS CHARACTER,
                                           plVisible AS LOGICAL,
                                           plEditable AS LOGICAL,
                                           poEditorType AS EditorTypeEnum):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:DefiningTypeName = pcDefiningTypeName
               THIS-OBJECT:PropertyName     = pcPropertyName
               THIS-OBJECT:PropertyLabel    = pcPropertyLabel
               THIS-OBJECT:Editable         = plEditable
               THIS-OBJECT:Visible          = plVisible
               THIS-OBJECT:EditorType       = poEditorType.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertyDescriptor class
        Notes:
        @param pcDefiningTypeName The name of the class that defines this property
        @param pcPropertyName The name for the property
        @param pcPropertyLabel The label for the property
        @param pcListItemPairs The list item pairs (comma delimited) to use for this property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC PropertyDescriptor (pcDefiningTypeName AS CHARACTER,
                                           pcPropertyName AS CHARACTER,
                                           pcPropertyLabel AS CHARACTER,
                                           pcListItemPairs AS CHARACTER):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:DefiningTypeName = pcDefiningTypeName
               THIS-OBJECT:PropertyName     = pcPropertyName
               THIS-OBJECT:PropertyLabel    = pcPropertyLabel
               THIS-OBJECT:ListItemPairs    = pcListItemPairs .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertyDescriptor class
        Notes:
        @param pcDefiningTypeName The name of the class that defines this property
        @param pcPropertyName The name for the property
        @param pcPropertyLabel The label for the property
        @param plVisible If the property should be visible
        @param plEditable If the property should be editable
        @param pcListItemPairs The list item pairs (comma delimited) to use for this property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC PropertyDescriptor (pcDefiningTypeName AS CHARACTER,
                                           pcPropertyName AS CHARACTER,
                                           pcPropertyLabel AS CHARACTER,
                                           plVisible AS LOGICAL,
                                           plEditable AS LOGICAL,
                                           pcListItemPairs AS CHARACTER):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:DefiningTypeName = pcDefiningTypeName
               THIS-OBJECT:PropertyName     = pcPropertyName
               THIS-OBJECT:PropertyLabel    = pcPropertyLabel
               THIS-OBJECT:Editable         = plEditable
               THIS-OBJECT:Visible          = plVisible
               THIS-OBJECT:ListItemPairs    = pcListItemPairs .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertyDescriptor class
        Notes:
        @param pcDefiningTypeName The name of the class that defines this property
        @param pcPropertyName The name for the property
        @param poEditorType The type of editor to use for the property
        @param pcListItemPairs The list item pairs (comma delimited) to use for this property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC PropertyDescriptor (pcDefiningTypeName AS CHARACTER,
                                           pcPropertyName AS CHARACTER,
                                           poEditorType AS EditorTypeEnum,
                                           pcListItemPairs AS CHARACTER):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:DefiningTypeName = pcDefiningTypeName
               THIS-OBJECT:PropertyName     = pcPropertyName
               THIS-OBJECT:EditorType       = poEditorType
               THIS-OBJECT:ListItemPairs    = pcListItemPairs .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertyDescriptor class
        Notes:
        @param pcDefiningTypeName The name of the class that defines this property
        @param pcPropertyName The name for the property
        @param pcPropertyLabel The label for the property
        @param poEditorType The type of editor to use for the property
        @param pcListItemPairs The list item pairs (comma delimited) to use for this property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC PropertyDescriptor (pcDefiningTypeName AS CHARACTER,
                                           pcPropertyName AS CHARACTER,
                                           pcPropertyLabel AS CHARACTER,
                                           poEditorType AS EditorTypeEnum,
                                           pcListItemPairs AS CHARACTER):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:DefiningTypeName = pcDefiningTypeName
               THIS-OBJECT:PropertyName     = pcPropertyName
               THIS-OBJECT:PropertyLabel    = pcPropertyLabel
               THIS-OBJECT:EditorType       = poEditorType
               THIS-OBJECT:ListItemPairs    = pcListItemPairs.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertyDescriptor class
        Notes:
        @param pcDefiningTypeName The name of the class that defines this property
        @param pcPropertyName The name for the property
        @param pcPropertyLabel The label for the property
        @param plVisible If the property should be visible
        @param plEditable If the property should be editable
        @param poEditorType The type of editor to use for the property
        @param pcListItemPairs The list item pairs (comma delimited) to use for this property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC PropertyDescriptor (pcDefiningTypeName AS CHARACTER,
                                           pcPropertyName AS CHARACTER,
                                           pcPropertyLabel AS CHARACTER,
                                           plVisible AS LOGICAL,
                                           plEditable AS LOGICAL,
                                           poEditorType AS EditorTypeEnum,
                                           pcListItemPairs AS CHARACTER):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:DefiningTypeName = pcDefiningTypeName
               THIS-OBJECT:PropertyName     = pcPropertyName
               THIS-OBJECT:PropertyLabel    = pcPropertyLabel
               THIS-OBJECT:Editable         = plEditable
               THIS-OBJECT:Visible          = plVisible
               THIS-OBJECT:EditorType       = poEditorType
               THIS-OBJECT:ListItemPairs    = pcListItemPairs.

    END CONSTRUCTOR.

END CLASS.
