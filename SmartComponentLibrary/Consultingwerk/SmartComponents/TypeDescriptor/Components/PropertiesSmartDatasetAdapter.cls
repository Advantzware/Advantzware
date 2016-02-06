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
    File        : PropertiesSmartDatasetAdapter
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

CLASS Consultingwerk.SmartComponents.TypeDescriptor.Components.PropertiesSmartDatasetAdapter
    INHERITS Consultingwerk.SmartComponents.TypeDescriptor.PropertyDescriptorCollection: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertiesSmartDatasetAdapter class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC PropertiesSmartDatasetAdapter ():
	    
	    DEFINE VARIABLE oProperty  AS IPropertyDescriptor NO-UNDO . 
		SUPER ().

        THIS-OBJECT:TypeName = "Consultingwerk.SmartComponents.Base.SmartDatasetAdapter":U . 

        oProperty = THIS-OBJECT:Add ("BindTo":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDatasetAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX ("QUERY":U))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Binding the BusinessEntityBindingSource to a Query or a Dataset.")) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("CreatingRecord":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.BrowsableAttribute (FALSE)) .
        oProperty:Attributes:Add (System.ComponentModel.DesignerSerializationVisibilityAttribute:Hidden).

        oProperty = THIS-OBJECT:Add ("Dataset":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.BrowsableAttribute (FALSE)) .
        oProperty:Attributes:Add (System.ComponentModel.DesignerSerializationVisibilityAttribute:Hidden).

        oProperty = THIS-OBJECT:Add ("EntityJoin":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDatasetAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX ("":U))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("A List in the same order as EntityView which defines Join (YES or NO).")) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("EntityName":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDatasetAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX ("":U))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Name of the BusinessEntity.")) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("EntityTable":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDatasetAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX ("":U))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("RootTable for this Instance of the BusinessEntity.")) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("EntityView":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDatasetAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX ("":U))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("List of Tables to read with the EntityTable.")) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("ForeignFields":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDatasetAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX ("":U))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("ForeignFields related to the Parent SmartDataSource.")) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("NonUpdatableColumns":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDatasetAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX ("":U))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Get's/set's Columns that will not get updated by UpdateRow (TRUE).")) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("QueryHandle":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.BrowsableAttribute (FALSE)) .
        oProperty:Attributes:Add (System.ComponentModel.DesignerSerializationVisibilityAttribute:Hidden).

        oProperty = THIS-OBJECT:Add ("QuerySort":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDatasetAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX ("":U))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Controls the sort-order used at the client.")) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("QueryString":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDatasetAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX ("":U))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("This is a base query string.")) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("SelectTables":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.BrowsableAttribute (FALSE)) .
        oProperty:Attributes:Add (System.ComponentModel.DesignerSerializationVisibilityAttribute:Hidden).
        
        oProperty = THIS-OBJECT:Add ("ThrowErrorsFromRetrieveData":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDatasetAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX (FALSE))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Gets/Sets if errors from RetrieveData (in childs of this abstract base class) should be THROWN or Shown from the DataAdapter class (Using ErrorHelper)  ")) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

	END CONSTRUCTOR.

END CLASS.
