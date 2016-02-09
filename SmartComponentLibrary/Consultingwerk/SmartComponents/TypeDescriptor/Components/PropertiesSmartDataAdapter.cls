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
    File        : PropertiesSmartDataAdapter
    Purpose     : Provides the Property Descriptor for the SmartDataAdapter class
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

CLASS Consultingwerk.SmartComponents.TypeDescriptor.Components.PropertiesSmartDataAdapter
    INHERITS Consultingwerk.SmartComponents.TypeDescriptor.PropertyDescriptorCollection: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertiesSmartDataAdapter class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC PropertiesSmartDataAdapter ():
	    
	    DEFINE VARIABLE oProperty  AS IPropertyDescriptor NO-UNDO . 
		SUPER ().

        THIS-OBJECT:TypeName = "Consultingwerk.SmartComponents.Base.SmartDataAdapter":U . 

        oProperty = THIS-OBJECT:Add ("AppServerPartition":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDataAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX ("":U))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Gets/Sets the name of the AppServer Partition to be used by this SmartDataAdapter instance")) .

        oProperty = THIS-OBJECT:Add ("BindingSource":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.BrowsableAttribute (FALSE)) .
        oProperty:Attributes:Add (System.ComponentModel.DesignerSerializationVisibilityAttribute:Hidden).

        oProperty = THIS-OBJECT:Add ("BindingSourceExcludeFields":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDataAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX ("":U))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Get/Set the Exclude Fields for the BindingSource created by the CreateBindingSource method.")) . 

        oProperty = THIS-OBJECT:Add ("BindingSourceIncludeFields":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDataAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX ("*":U))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Get/Set the Include Fields for the BindingSource created by the CreateBindingSource method.")) . 

        oProperty = THIS-OBJECT:Add ("DataSourceBindingSource":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.BrowsableAttribute (FALSE)) .
        oProperty:Attributes:Add (System.ComponentModel.DesignerSerializationVisibilityAttribute:Hidden).

        oProperty = THIS-OBJECT:Add ("Initializing":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.BrowsableAttribute (FALSE)) .
        oProperty:Attributes:Add (System.ComponentModel.DesignerSerializationVisibilityAttribute:Hidden).
        
        oProperty = THIS-OBJECT:Add ("LinkDataSource":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDataAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Get/Set the SmartDataSource of the current Class. The SmartDataSource can be set to any class implementing the Interface ISmartDataSource.")) . 

        oProperty = THIS-OBJECT:Add ("LinkNavigationSource":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDataAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Get and Set the current SmartNavigationSource of the SmartDataAdapter.")) . 

        oProperty = THIS-OBJECT:Add ("RecordPosition":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.BrowsableAttribute (FALSE)) .
        oProperty:Attributes:Add (System.ComponentModel.DesignerSerializationVisibilityAttribute:Hidden).

        oProperty = THIS-OBJECT:Add ("SmartDataSourceActive":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDataAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX (TRUE))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Get/Set if this instance of the SmartDataAdapter is subscribed to the PositionChanged event of the SmartDataSource's BindingSource.")) . 

        oProperty = THIS-OBJECT:Add ("SmartDataSourceState":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.BrowsableAttribute (FALSE)) . 
        oProperty:Attributes:Add (System.ComponentModel.DesignerSerializationVisibilityAttribute:Hidden).

	END CONSTRUCTOR.

END CLASS.
