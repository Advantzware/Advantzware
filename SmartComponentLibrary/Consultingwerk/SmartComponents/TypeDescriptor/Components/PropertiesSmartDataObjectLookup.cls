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
    File        : PropertiesSmartDataObjectLookup
    Purpose     : Provides the Property Descriptor for the SmartDataObjectLookup class
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

CLASS Consultingwerk.SmartComponents.TypeDescriptor.Components.PropertiesSmartDataObjectLookup
    INHERITS Consultingwerk.SmartComponents.TypeDescriptor.PropertyDescriptorCollection: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertiesSmartDataObjectLookup class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC PropertiesSmartDataObjectLookup ():
	    
	    DEFINE VARIABLE oProperty AS IPropertyDescriptor NO-UNDO . 
		
		SUPER ().

        THIS-OBJECT:TypeName = "Consultingwerk.SmartComponents.Implementation.SmartDataObjectLookup":U . 

        oProperty = THIS-OBJECT:Add ("OpenSmartDataObject":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.BrowsableAttribute (FALSE)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DesignerSerializationVisibilityAttribute (DesignerSerializationVisibility:Hidden)) .

        oProperty = THIS-OBJECT:Add ("SmartDataSourceState":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.BrowsableAttribute (FALSE)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DesignerSerializationVisibilityAttribute (DesignerSerializationVisibility:Hidden)) .

        oProperty = THIS-OBJECT:Add ("Initializing":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.BrowsableAttribute (FALSE)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DesignerSerializationVisibilityAttribute (DesignerSerializationVisibility:Hidden)) .

        oProperty = THIS-OBJECT:Add ("AddingRecord":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.BrowsableAttribute (FALSE)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DesignerSerializationVisibilityAttribute (DesignerSerializationVisibility:Hidden)) .

        oProperty = THIS-OBJECT:Add ("AddRowIdent":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.BrowsableAttribute (FALSE)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DesignerSerializationVisibilityAttribute (DesignerSerializationVisibility:Hidden)) .

        oProperty = THIS-OBJECT:Add ("DataHandle":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.BrowsableAttribute (FALSE)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DesignerSerializationVisibilityAttribute (DesignerSerializationVisibility:Hidden)) .

        oProperty = THIS-OBJECT:Add ("RecordPosition":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.BrowsableAttribute (FALSE)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DesignerSerializationVisibilityAttribute (DesignerSerializationVisibility:Hidden)) .

        oProperty = THIS-OBJECT:Add ("RowObject":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.BrowsableAttribute (FALSE)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DesignerSerializationVisibilityAttribute (DesignerSerializationVisibility:Hidden)) .

        oProperty = THIS-OBJECT:Add ("SmartDataObjectHandle":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.BrowsableAttribute (FALSE)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DesignerSerializationVisibilityAttribute (DesignerSerializationVisibility:Hidden)) .

        oProperty = THIS-OBJECT:Add ("SmartDataObjectInitialized":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.BrowsableAttribute (FALSE)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DesignerSerializationVisibilityAttribute (DesignerSerializationVisibility:Hidden)) .

	END CONSTRUCTOR.

END CLASS.
