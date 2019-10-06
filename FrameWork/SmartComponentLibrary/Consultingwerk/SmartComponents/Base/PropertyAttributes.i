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
    File        : PropertyAttributes.i
    Purpose     : Part of ICustomTypeDescriptor.i

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Feb 20 18:31:30 CET 2011
    Notes       :
  ----------------------------------------------------------------------*/

            ASSIGN i = 0 . 

            {Consultingwerk/foreach.i PropertyDescriptor oProperty in oProperties}

                oListAttributes = Consultingwerk.Util.SmartTypeDescriptor:GetClassPropertyAttributes (THIS-OBJECT:GetClass (),
                                                                                                      oProperty:Name) .

                /* Mike Fechner, Consultingwerk Ltd. 23.02.2013
                   Bug 2848, new Custom Property Descriptor implementation */
                IF VALID-OBJECT (oListAttributes) THEN DO:
                    oNewProperty = NEW Consultingwerk.SmartComponents.SerializeCustomAttributesPropertyDescriptor (oProperty, 
                                                                                                                   oListAttributes:ToArray ()) .
                    oNewProperties[i + 1] = oNewProperty .
                END.
                
            /* Mike Fechner, Consultingwerk Ltd. 19.02.2015
               SCL-665: Legacy Custom Type Descriptor deactivated by default now.
                        See products.i for details - customers can reactivate the 
                        Legacy Custom Type Descriptor implementation if needed */
&IF DEFINED (LegacyCustomTypeDescriptorImplementation) NE 0 &THEN                
                ELSE IF CAN-DO (THIS-OBJECT:NonBrowsableProperties, oProperty:Name) THEN DO:
                    oNewProperty = NEW Consultingwerk.SmartComponents.SerializeNotBrowsablePropertyDescriptor (oProperty) .
                    oNewProperties[i + 1] = oNewProperty .
                END.
                ELSE IF CAN-DO (THIS-OBJECT:BindableProperties, oProperty:Name) OR
                        CAN-DO (THIS-OBJECT:CategorizedProperties, oProperty:Name) OR
                        CAN-DO (THIS-OBJECT:DescribedProperties, oProperty:Name) OR
                        CAN-DO (THIS-OBJECT:DesignerVisibilityProperties, oProperty:Name) THEN DO:

                    IF CAN-DO (THIS-OBJECT:BindableProperties, oProperty:Name) THEN
                        oAttributes[1] = System.ComponentModel.BindableAttribute:Yes .
                    ELSE
                        oAttributes[1] = System.ComponentModel.BindableAttribute:No .

                    IF CAN-DO (THIS-OBJECT:CategorizedProperties, oProperty:Name) THEN
                        oAttributes[2] = NEW System.ComponentModel.CategoryAttribute (ENTRY (LOOKUP (oProperty:Name, THIS-OBJECT:CategorizedProperties), THIS-OBJECT:CategoryProperties)) .
                    ELSE
                        oAttributes[2] = ? .

                    IF CAN-DO (THIS-OBJECT:DescribedProperties, oProperty:Name) THEN
                        oAttributes[3] = NEW System.ComponentModel.DescriptionAttribute (ENTRY (LOOKUP (oProperty:Name, THIS-OBJECT:DescribedProperties), THIS-OBJECT:DescriptionProperties, CHR(1))) .
                    ELSE
                        oAttributes[3] = ? .

                    IF CAN-DO (THIS-OBJECT:DesignerVisibilityProperties, oProperty:Name) THEN DO:
                        CASE ENTRY (LOOKUP (oProperty:Name, THIS-OBJECT:DesignerVisibilityProperties), THIS-OBJECT:DesignerVisibilityAttributes):
                            WHEN "Content":U THEN
                                oAttributes[4] = NEW System.ComponentModel.DesignerSerializationVisibilityAttribute (System.ComponentModel.DesignerSerializationVisibility:Content) .
                            WHEN "Hidden":U THEN
                                oAttributes[4] = NEW System.ComponentModel.DesignerSerializationVisibilityAttribute (System.ComponentModel.DesignerSerializationVisibility:Hidden) .
                            WHEN "Visible":U THEN
                                oAttributes[4] = NEW System.ComponentModel.DesignerSerializationVisibilityAttribute (System.ComponentModel.DesignerSerializationVisibility:Visible) .
                        END CASE.
                    END.
                    ELSE
                        oAttributes[4] = ? .

                    oNewProperty = NEW Consultingwerk.SmartComponents.SerializeCustomAttributesPropertyDescriptor (oProperty, oAttributes) .
                    oNewProperties[i + 1] = oNewProperty .
                END.
&ENDIF    
                ELSE
                    oNewProperties[i + 1] = oProperty .
                    
                ASSIGN i = i + 1 . 
            END . 
            