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
    File        : PropertiesSmartLookup
    Purpose     : Provides the Property Descriptor for the SmartLookup class
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

CLASS Consultingwerk.SmartComponents.TypeDescriptor.Components.PropertiesSmartLookup
    INHERITS Consultingwerk.SmartComponents.TypeDescriptor.PropertyDescriptorCollection: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertiesSmartLookup class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC PropertiesSmartLookup ():
        
        DEFINE VARIABLE oProperty AS IPropertyDescriptor NO-UNDO . 
        
        SUPER ().

        THIS-OBJECT:TypeName = "Consultingwerk.SmartComponents.Base.SmartLookup":U . 

        oProperty = THIS-OBJECT:Add ("AddLookupButton":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartLookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Depricated Visual Designer Command":U)) .
        oProperty:Attributes:Add (NEW DesignerSerializationVisibilityAttribute (DesignerSerializationVisibility:Hidden)) .
        oProperty:Attributes:Add (NEW DefaultValueAttribute (FALSE)) .
        oProperty:Attributes:Add (NEW BrowsableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("LookupTimer":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartLookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("The reference to the Timer component used by the Lookup":U)) .
        oProperty:Attributes:Add (NEW DesignerSerializationVisibilityAttribute (DesignerSerializationVisibility:Hidden)) .
        oProperty:Attributes:Add (NEW BrowsableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("LookupBrowserExcludeFields":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartLookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Columns (comma delimited list of Column Names) to be excluded from the LookupBrowser.":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("LookupBrowserFields":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartLookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Columns (comma delimited list of Column Names) to be shown in the LookupBrowser.":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("LookupControls":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartLookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("A comma seperated list of Control names. The value of the corresponding Columns gets filled into those Controls.":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("LookupDialogTitle":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartLookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Dialog title for the LookupDialogForm.":U)) .

        oProperty = THIS-OBJECT:Add ("LookupFields":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartLookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("A comma seperated list of Column names. The value of those fields  gets filled into the corresponding Controls of the Property LookupControls.":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("LookupKeyField":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartLookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("The name of the field which is returned by the SmartLookup.":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("LookupTimerDelay":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartLookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("The number of msecs after a key has been pressed in a SmartLookup until the entered value is looked up. This allows the user to enter multiple keys before the value is looked up (in a data source).":U)) .

        oProperty = THIS-OBJECT:Add ("LookupTimerDisabled":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartLookup":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Allows to completely disable the lookup timer functioanlity.":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX (FALSE))) .

    END CONSTRUCTOR.

END CLASS.
