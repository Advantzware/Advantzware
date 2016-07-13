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
    File        : PropertiesSmartDatasetChildAdapter
    Purpose     : Provides the Property Descriptor for the SmartDatasetAdapter class
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

CLASS Consultingwerk.SmartComponents.TypeDescriptor.Components.PropertiesSmartDatasetChildAdapter
    INHERITS Consultingwerk.SmartComponents.TypeDescriptor.PropertyDescriptorCollection: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertiesSmartDatasetAdapter class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC PropertiesSmartDatasetChildAdapter ():
	    
	    DEFINE VARIABLE oProperty  AS IPropertyDescriptor NO-UNDO . 
		SUPER ().

        THIS-OBJECT:TypeName = "Consultingwerk.SmartComponents.Implementation.SmartDatasetChildAdapter":U . 

        oProperty = THIS-OBJECT:Add ("AppServerPartition":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDatasetChildAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("BindingSourceExcludeFields":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDatasetChildAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("BindingSourceIncludeFields":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDatasetChildAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("BindTo":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.BrowsableAttribute (FALSE)) .
        oProperty:Attributes:Add (System.ComponentModel.DesignerSerializationVisibilityAttribute:Hidden).

        oProperty = THIS-OBJECT:Add ("EntityName":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDatasetChildAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .
        
        oProperty = THIS-OBJECT:Add ("EntityTable":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDatasetChildAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("EntityView":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDatasetChildAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("ForeignFields":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDatasetChildAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("LinkGroupCreateSource":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDatasetChildAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Gets and sets the SmartGroupCreateSource")) .

        oProperty = THIS-OBJECT:Add ("NonUpdatableColumns":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDatasetChildAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("QuerySort":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDatasetChildAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("QueryString":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDatasetChildAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX ("":U))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("This is a base query string.")) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .
        
	END CONSTRUCTOR.

END CLASS.
