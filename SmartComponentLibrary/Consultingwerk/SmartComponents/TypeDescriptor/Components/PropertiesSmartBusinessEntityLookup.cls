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
    File        : PropertiesSmartBusinessEntityLookup
    Purpose     : Provides the Property Descriptor for the SmartBusinessEntityLookup class
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
USING System.ComponentModel.*                                    FROM ASSEMBLY .
USING System.ComponentModel.Design.Serialization.*               FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.TypeDescriptor.Components.PropertiesSmartBusinessEntityLookup
    INHERITS Consultingwerk.SmartComponents.TypeDescriptor.PropertyDescriptorCollection: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertiesSmartBusinessEntityLookup class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC PropertiesSmartBusinessEntityLookup ():
	    
	    DEFINE VARIABLE oProperty AS IPropertyDescriptor NO-UNDO . 
		
		SUPER ().

        THIS-OBJECT:TypeName = "Consultingwerk.SmartComponents.Base.SmartBusinessEntityLookup":U . 

        oProperty = THIS-OBJECT:Add ("AdapterType":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.BrowsableAttribute (FALSE)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DesignerSerializationVisibilityAttribute (DesignerSerializationVisibility:Hidden)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("AppServerPartition":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityLookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Gets/Sets the name of the AppServer Partition to be used by this SmartBusinessEntityLookup instance":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("AdditionalLookupFieldValues":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityLookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Returns the reference to the dictionary that contains the values for the LookupAdditionalFields":U)) .

	END CONSTRUCTOR.

END CLASS.
