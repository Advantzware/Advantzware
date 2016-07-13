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
    File        : PropertiesSmartUpdatableBrowser
    Purpose     : Provides the Property Descriptor for the SmartUpdatableBrowser class
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

CLASS Consultingwerk.SmartComponents.TypeDescriptor.Components.PropertiesSmartUpdatableBrowser
    INHERITS Consultingwerk.SmartComponents.TypeDescriptor.PropertyDescriptorCollection: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertiesSmartUpdatableBrowser class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC PropertiesSmartUpdatableBrowser ():
	    
	    DEFINE VARIABLE oProperty AS IPropertyDescriptor NO-UNDO . 
		
		SUPER ().

        THIS-OBJECT:TypeName = "Consultingwerk.SmartComponents.Implementation.SmartUpdatableBrowser":U . 

        oProperty = THIS-OBJECT:Add ("AddingRecord":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.BrowsableAttribute (FALSE)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DesignerSerializationVisibilityAttribute (DesignerSerializationVisibility:Hidden)) .

        oProperty = THIS-OBJECT:Add ("CopyingRecord":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.BrowsableAttribute (FALSE)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DesignerSerializationVisibilityAttribute (DesignerSerializationVisibility:Hidden)) .

        oProperty = THIS-OBJECT:Add ("SmartTableIOState":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.BrowsableAttribute (FALSE)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DesignerSerializationVisibilityAttribute (DesignerSerializationVisibility:Hidden)) .

        oProperty = THIS-OBJECT:Add ("DisabledColumnsForAdd":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartUpdatableBrowser":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("DisabledColumnsForUpdate":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartUpdatableBrowser":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

	END CONSTRUCTOR.

END CLASS.
