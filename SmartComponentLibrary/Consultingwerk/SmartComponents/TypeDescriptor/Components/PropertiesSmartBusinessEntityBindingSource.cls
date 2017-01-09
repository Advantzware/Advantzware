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
    File        : PropertiesSmartBusinessEntityBindingSource
    Purpose     : Provides the Property Descriptor for the SmartBusinessEntityBindingSource class
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

CLASS Consultingwerk.SmartComponents.TypeDescriptor.Components.PropertiesSmartBusinessEntityBindingSource
    INHERITS Consultingwerk.SmartComponents.TypeDescriptor.PropertyDescriptorCollection: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertiesSmartDatasetAdapter class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC PropertiesSmartBusinessEntityBindingSource ():
        
        DEFINE VARIABLE oProperty  AS IPropertyDescriptor NO-UNDO . 
        
        SUPER ().

        THIS-OBJECT:TypeName = "Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource":U . 

        oProperty = THIS-OBJECT:Add ("BindTo":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityBindingSource":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Binding the BusinessEntityBindingSource to a Query or a Dataset.")) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute ("QUERY":U)) .

        oProperty = THIS-OBJECT:Add ("EntityJoin":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityBindingSource":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX ("":U))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("A List in the same order as EntityView which defines Join (YES or NO).")) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("EntityName":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityBindingSource":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX ("":U))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Name of the BusinessEntity.")) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("EntityTable":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityBindingSource":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX ("":U))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("RootTable for this Instance of the BusinessEntity.")) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("EntityView":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityBindingSource":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX ("":U))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("List of Tables to read with the EntityTable.")) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

    END CONSTRUCTOR.

END CLASS.
