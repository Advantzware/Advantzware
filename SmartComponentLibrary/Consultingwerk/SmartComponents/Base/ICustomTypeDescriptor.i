/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ICustomTypeDescriptor.i
    Purpose     :

    Syntax      : Include file to class files implementing
                  ICustomTypeDescriptor Interface

    Description : See MSDN for details
                  http://msdn.microsoft.com/en-us/library/system.componentmodel.icustomtypedescriptor.aspx

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Apr 22 22:06:52 CEST 2010
    Notes       : The majority of methods of the ICustomTypeDescriptor
                  interface are implemented by invoking the default
                  behaviour of the .NET TypeDescriptor (static) class
                  http://msdn.microsoft.com/en-us/library/system.componentmodel.typedescriptor.aspx
  ----------------------------------------------------------------------*/

    DEFINE PROTECTED VARIABLE oCustomTypeDescriptorAttributes AS AttributeCollection                                               NO-UNDO .
    DEFINE PROTECTED VARIABLE oCustomTypeDescriptorProperties AS PropertyDescriptorCollection                                      NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Property that is used to mark properties (comma-delimited list) that
                 should be Data-Bindable
        Notes:   C# Annotation: [Bindable(true)]
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY BindableProperties AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Property that is used to mark properties (comma-delimited list) that
                 should receive a Category attribute
        Notes:   C# Annotation: [Category("...")]
                 This is a comma delimited list
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY CategorizedProperties AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Categories of the Property in the CategorizedProperty list
        Notes:   C# Annotation: [Category("...")]
                 This is a comma delimited list
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY CategoryProperties AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Property that is used to mark properties (comma-delimited list) that
                 should receive a Description attribute
        Notes:   C# Annotation: [Description("...")]
                 This is a comma delimited list
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY DescribedProperties AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Description of the Property in the DescribedProperties list
        Notes:   C# Annotation: [Description("...")]
                 This is a CHR(1) delimited list
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY DescriptionProperties AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Property that is used to mark properties (comma-delimited list) that
                 should receive a DesignerSerializationVisibility Attribute
        Notes:   C# Annotation: [DesignerSerializationVisibility(...)]
                 This is a comma delimited list
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY DesignerVisibilityProperties AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: The CHARACTER representation of the DesignerSerializationVisibiltiy
                 attribute (Content/Visible/Hidden), see .NET Enumeration
                 System.ComponentModel.DesignerSerializationVisibility
        Notes:   C# Annotation: [DesignerSerializationVisibility(...)]
                 This is a comma delimited list
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY DesignerVisibilityAttributes AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns a collection of custom attributes for this instance of a
                 component.
        Notes:
        @return The System.ComponentModel.AttributeCollection describing the types attributes
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC AttributeCollection GetAttributes ():

        IF VALID-OBJECT (oCustomTypeDescriptorAttributes) THEN
            RETURN oCustomTypeDescriptorAttributes .

        ASSIGN oCustomTypeDescriptorAttributes = TypeDescriptor:GetAttributes (THIS-OBJECT, TRUE) .

        RETURN oCustomTypeDescriptorAttributes .

        CATCH err AS Progress.Lang.Error:
            /* ignore */
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the class name of this instance of a component.
        Notes:
        @return The class name of the Component
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetClassName ():

        RETURN TypeDescriptor:GetClassName (THIS-OBJECT, TRUE) .

        CATCH err AS Progress.Lang.Error:
            /* ignore */
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of this instance of a component.
        Notes:
        @return The Component Name of this instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetComponentName ():

        RETURN TypeDescriptor:GetComponentName (THIS-OBJECT, TRUE) .

        CATCH err AS Progress.Lang.Error:
            /* ignore */
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a type converter for this instance of a component.
        Notes:
        @return The System.ComponentModel.TypeConverter for this instance of a component.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC TypeConverter GetConverter ():

        RETURN TypeDescriptor:GetConverter (THIS-OBJECT, TRUE) .

        CATCH err AS Progress.Lang.Error:
            /* ignore */
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the default event for this instance of a component.
        Notes:
        @return The System.ComponentModel.EventDescriptor for the default event of this component
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC EventDescriptor GetDefaultEvent ():

        RETURN TypeDescriptor:GetDefaultEvent (THIS-OBJECT, TRUE) .

        CATCH err AS Progress.Lang.Error:
            /* ignore */
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the default property for this instance of a component.
        Notes:
        @return The System.ComponentModel.PropertyDescriptor for the default property of the component
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC PropertyDescriptor GetDefaultProperty ():

        RETURN TypeDescriptor:GetDefaultProperty (THIS-OBJECT, TRUE) .

        CATCH err AS Progress.Lang.Error:
            /* ignore */
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns an editor of the specified type for this instance of a
                 component.
        Notes:
        @param editorBaseType A System.Type that represents the editor for this object.
        @return The reference to the Editor of the type of the component instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC System.Object GetEditor (editorBaseType AS System.Type):

        RETURN TypeDescriptor:GetEditor (THIS-OBJECT, editorBaseType, TRUE) .

        CATCH err AS Progress.Lang.Error:
            /* ignore */
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the events for this instance of a component.
        Notes:
        @return The System.ComponentModel.EventDescriptorCollection describing the events of the component
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC EventDescriptorCollection GetEvents ():

        RETURN TypeDescriptor:GetEvents (THIS-OBJECT, TRUE) .

        CATCH err AS Progress.Lang.Error:
            /* ignore */
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the events for this instance of a component using the
                 specified attribute array as a filter.
        Notes:
        @param attributes A System.Attribute[] that is used as a filter.
        @return The System.ComponentModel.EventDescriptorCollection describing the events of the component
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC EventDescriptorCollection GetEvents (attributes AS "System.Attribute[]":U):

        RETURN TypeDescriptor:GetEvents (THIS-OBJECT, attributes, TRUE) .

        CATCH err AS Progress.Lang.Error:
            /* ignore */
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the properties for this instance of a component.
        Notes:
        @return A System.ComponentModel.PropertyDescriptorCollection that represents the properties for this component instance.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC PropertyDescriptorCollection GetProperties ():

        DEFINE VARIABLE oProperties          AS PropertyDescriptorCollection                            NO-UNDO .
        DEFINE VARIABLE oNewProperties       AS PropertyDescriptor                               EXTENT NO-UNDO .
        DEFINE VARIABLE oReturn              AS PropertyDescriptor                               EXTENT NO-UNDO .
        DEFINE VARIABLE oNewProperty         AS PropertyDescriptor                                      NO-UNDO .
        DEFINE VARIABLE i                    AS INTEGER                                                 NO-UNDO .
        DEFINE VARIABLE iCount               AS INTEGER                                                 NO-UNDO .
        DEFINE VARIABLE oClass               AS Progress.Lang.Class                                     NO-UNDO .
        DEFINE VARIABLE oAttributes          AS System.Attribute                               EXTENT 4 NO-UNDO .
        DEFINE VARIABLE oListAttributes      AS "System.Collections.Generic.List<System.Attribute>":U   NO-UNDO .

        IF NOT THIS-OBJECT:DesignTime THEN
            RETURN TypeDescriptor:GetProperties (THIS-OBJECT, TRUE) .

        IF VALID-OBJECT (oCustomTypeDescriptorProperties) THEN DO:
            /* Mike Fechner, Consultingwerk Ltd. 01.12.2010
               For SmartViewerControl, make sure that the cached properties contain the Link... properties */
            oClass = Progress.Lang.Class:GetClass ("Consultingwerk.SmartComponents.Base.SmartViewerControl":U) .

            IF THIS-OBJECT:GetClass():IsA (oClass) THEN DO:
/*            IF TYPE-OF (THIS-OBJECT, Consultingwerk.SmartComponents.Base.SmartViewerControl) THEN DO:*/
                IF VALID-OBJECT (oCustomTypeDescriptorProperties:Find ("LinkDataSource":U, TRUE)) THEN
                    RETURN oCustomTypeDescriptorProperties .
            END.
            ELSE
                RETURN oCustomTypeDescriptorProperties .
        END.

        oProperties = TypeDescriptor:GetProperties (THIS-OBJECT, TRUE) .

        /* Mike Fechner, Consultingwerk Ltd. 16.05.2010
           Create Designer Verbs */
        THIS-OBJECT:CreateVerbs () .

        ASSIGN iCount                  = oProperties:Count
               EXTENT (oNewProperties) = iCount .

        { Consultingwerk/SmartComponents/Base/PropertyAttributes.i }

        ASSIGN oCustomTypeDescriptorProperties = NEW PropertyDescriptorCollection (oNewProperties) .
        RETURN oCustomTypeDescriptorProperties .

        CATCH err AS Progress.Lang.Error:
            /* ignore */
        END CATCH.

        &IF DEFINED (DEBUG) NE 0 AND DEFINED (DebugCustomTypeDescriptor) NE 0 &THEN
        FINALLY:
            IF VALID-OBJECT (oCustomTypeDescriptorProperties) THEN DO:
                {Consultingwerk/SmartComponents/Support/CustomTypeDescriptorLogger.i oCustomTypeDescriptorProperties} .
            END.
        END FINALLY.
        &ENDIF

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the properties for this instance of a component using the
                 attribute array as a filter.
        Notes:
        @param attributes A System.Attribute[] that is used as a filter.
        @return A System.ComponentModel.PropertyDescriptorCollection that represents the properties for this component instance.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC PropertyDescriptorCollection GetProperties (attributes AS "System.Attribute[]":U):

        IF NOT THIS-OBJECT:DesignTime THEN
            RETURN TypeDescriptor:GetProperties (THIS-OBJECT, attributes, TRUE) .

        RETURN THIS-OBJECT:GetProperties () .

        CATCH err AS Progress.Lang.Error:
            /* ignore */
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns an object that contains the property described by the
                 specified property descriptor.
        Notes:
        @param pd A System.ComponentModel.PropertyDescriptor that represents the property whose owner is to be found.
        @return An System.Object that represents the owner of the specified property.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC System.Object GetPropertyOwner (pd AS PropertyDescriptor):

        RETURN THIS-OBJECT .

        CATCH err AS Progress.Lang.Error:
            /* ignore */
        END CATCH.

    END METHOD .
