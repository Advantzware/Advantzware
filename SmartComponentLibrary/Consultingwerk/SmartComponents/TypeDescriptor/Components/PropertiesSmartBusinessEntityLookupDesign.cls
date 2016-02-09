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
    File        : PropertiesSmartBusinessEntityLookupDesign
    Purpose     : Provides the Property Descriptor for the SmartBusinessEntityLookupDesign class
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

CLASS Consultingwerk.SmartComponents.TypeDescriptor.Components.PropertiesSmartBusinessEntityLookupDesign
    INHERITS Consultingwerk.SmartComponents.TypeDescriptor.PropertyDescriptorCollection: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertiesSmartBusinessEntityLookupDesign class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC PropertiesSmartBusinessEntityLookupDesign ():
	    
	    DEFINE VARIABLE oProperty AS IPropertyDescriptor NO-UNDO . 
		
		SUPER ().

        THIS-OBJECT:TypeName = "Consultingwerk.SmartComponents.Base.SmartBusinessEntityLookupDesign":U . 

        oProperty = THIS-OBJECT:Add ("LookupDialogFilterFields":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityLookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Fields that allow to Filter on the Lookup Browser Dialog, Comma-Delimited, first entry is default":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("LookupDialogFilterOperators":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityLookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Operators for the LookupDialogFilterFields, Comma-Delimited, first entry is default":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("LookupKeyValue":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.BindableAttribute (TRUE)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityLookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("The current KeyValue of the Lookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("LookupDialogOpenQuery":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityLookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (TRUE)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Option to open the Lookup Dialog Query when the dialog is opened":U)) .

        oProperty = THIS-OBJECT:Add ("LookupDialogQueryString":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityLookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("QueryString used when opening the Lookup Browser Dialog":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("LookupEntityName":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityLookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("The name of the BusinessEntity for the Lookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("LookupEntityTable":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityLookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("RootTable for this Instance of the SmartBusinessEntityLookup.":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("LookupEntityView":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityLookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("List of additional Table names to read with the EntityTable. Tables will be joined with the EntityTable.":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("LookupQueryString":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityLookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("QueryString used in PerformLookup method":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("LookupKeyValueColumn":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityLookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("The name of the Data Source column which is used for retrieving the LookupKeyValue property.":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("LookupDialogClassName":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityLookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Gets and Sets the Class Name of the Lookup Dialog Form (of type IBusinessEntityLookupDialogForm)":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("LookupQuerySort":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityLookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("QuerySort used in PerformLookup method":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("LookupDialogQuerySort":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityLookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("QuerySort used when opening the Lookup Browser Dialog.":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("LookupCascade":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityLookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Cascade the Perform Lookup to linked Child Lookups.":U)) .

        oProperty = THIS-OBJECT:Add ("LookupAdditionalFields":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityLookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("The comma delimited list of additional fields that will be retrieved by the lookup operations.":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

	END CONSTRUCTOR.

END CLASS.
