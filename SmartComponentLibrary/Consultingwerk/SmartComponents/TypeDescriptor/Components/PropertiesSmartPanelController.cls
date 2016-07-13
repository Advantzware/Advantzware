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
    File        : PropertiesSmartPanelController
    Purpose     : Provides the Property Descriptor for the SmartPanelController class
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

CLASS Consultingwerk.SmartComponents.TypeDescriptor.Components.PropertiesSmartPanelController
    INHERITS Consultingwerk.SmartComponents.TypeDescriptor.PropertyDescriptorCollection: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertiesSmartPanelController class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC PropertiesSmartPanelController ():
	    
	    DEFINE VARIABLE oProperty AS IPropertyDescriptor NO-UNDO . 
		
		SUPER ().

        THIS-OBJECT:TypeName = "Consultingwerk.SmartComponents.Implementation.SmartPanelController":U . 

        oProperty = THIS-OBJECT:Add ("SmartPanelDesigner":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.BrowsableAttribute (FALSE)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DesignerSerializationVisibilityAttribute (DesignerSerializationVisibility:Hidden)) .

	END CONSTRUCTOR.

END CLASS.
