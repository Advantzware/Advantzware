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
    File        : PropertiesSmartBindingSource
    Purpose     : Provides the Property Descriptor for the SmartBindingSource class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Feb 22 15:33:55 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.TypeDescriptor.*            FROM PROPATH .  
USING Consultingwerk.SmartComponents.TypeDescriptor.Components.* FROM PROPATH .  
USING Progress.Lang.*                                            FROM PROPATH .
USING System.ComponentModel.* FROM ASSEMBLY.
USING System.ComponentModel.Design.Serialization.* FROM ASSEMBLY.

CLASS Consultingwerk.SmartComponents.TypeDescriptor.Components.PropertiesSmartBindingSource
    INHERITS Consultingwerk.SmartComponents.TypeDescriptor.PropertyDescriptorCollection: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertiesSmartBindingSource class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC PropertiesSmartBindingSource ():
        
        DEFINE VARIABLE oProperty  AS IPropertyDescriptor NO-UNDO . 
        
        SUPER ().

        THIS-OBJECT:TypeName = "Consultingwerk.SmartComponents.Implementation.SmartBindingSource":U . 

        oProperty = THIS-OBJECT:Add ("AddFields":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBindingSource":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Depricated Visual Designer Command")) .
        oProperty:Attributes:Add (NEW DesignerSerializationVisibilityAttribute (DesignerSerializationVisibility:Hidden)) .
        oProperty:Attributes:Add (NEW DefaultValueAttribute (FALSE)) .
        oProperty:Attributes:Add (NEW BrowsableAttribute (FALSE)) .


        oProperty = THIS-OBJECT:Add ("ClearSchema":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBindingSource":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Depricated Visual Designer Command")) .
        oProperty:Attributes:Add (NEW DesignerSerializationVisibilityAttribute (DesignerSerializationVisibility:Hidden)) .
        oProperty:Attributes:Add (NEW DefaultValueAttribute (FALSE)) .
        oProperty:Attributes:Add (NEW BrowsableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("ImportSchema":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBindingSource":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Depricated Visual Designer Command")) .
        oProperty:Attributes:Add (NEW DesignerSerializationVisibilityAttribute (DesignerSerializationVisibility:Hidden)) .
        oProperty:Attributes:Add (NEW DefaultValueAttribute (FALSE)) .
        oProperty:Attributes:Add (NEW BrowsableAttribute (FALSE)) .

    END CONSTRUCTOR.

END CLASS.
