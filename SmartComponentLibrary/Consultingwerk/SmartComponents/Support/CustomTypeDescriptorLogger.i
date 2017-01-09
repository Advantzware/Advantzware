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
    File        : CustomTypeDescriptorLogger.i
    Purpose     : Debugging utilities for the ICustomTypeDescriptor.i
                  implementation
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Feb 12 17:17:54 CET 2013
    Notes       : Need to activate DEBUG and DebugCustomTypeDescriptor Preprocessor
                  in the debug obtions (debug.i)
  ----------------------------------------------------------------------*/

&scoped PropertyName {1}
&scoped nodefine {2}

        &IF "{&nodefine}" NE "nodefine" &THEN
        DEFINE VARIABLE oLogAttribute           AS System.Object                  NO-UNDO .
        DEFINE VARIABLE oLogAttributeEnumerator AS System.Collections.IEnumerator NO-UNDO .
        DEFINE VARIABLE oLogAttributeValue      AS System.Object                  NO-UNDO .
        &ENDIF

        Consultingwerk.Debug:Log ("LogProperties":U, SUBSTITUTE ("Class: &1":U,
                                                                 THIS-OBJECT:GetClass():TypeName)) .

        {Consultingwerk/foreach.i System.ComponentModel.PropertyDescriptor oLogProperty in {&PropertyName} {&nodefine}}

            IF VALID-OBJECT (oLogProperty) THEN DO:

                Consultingwerk.Debug:Log ("LogProperties":U, SUBSTITUTE ("      Property: &1 - &2":U,
                                                                         oLogProperty:Name,
                                                                         oLogProperty:PropertyType:FullName)) .

                IF VALID-OBJECT (oLogProperty:ComponentType) THEN
                    Consultingwerk.Debug:Log ("LogProperties":U, SUBSTITUTE ("        Component Type: &1":U,
                                                                             oLogProperty:ComponentType:FullName)) .


                oLogAttributeEnumerator = oLogProperty:Attributes:GetEnumerator() .
                oLogAttributeEnumerator:Reset () .

                DO WHILE oLogAttributeEnumerator:MoveNext () ON ERROR UNDO, THROW:

                    IF VALID-OBJECT (oLogAttributeEnumerator) THEN DO:
                        oLogAttribute = oLogAttributeEnumerator:Current .

                        IF VALID-OBJECT (oLogAttribute) AND NOT TYPE-OF (oLogAttribute, System.Enum) THEN DO:

                            &IF "{&DebugPropertyAttributes}" NE "" &THEN
                            IF LOOKUP (oLogAttribute:GetType():FullName, "{&DebugPropertyAttributes}") = 0 THEN
                                NEXT .
                            &ENDIF

                            IF Consultingwerk.Util.ReflectionHelper:HasProperty (oLogAttribute, "Value":U) THEN
                                oLogAttributeValue = Consultingwerk.Util.ReflectionHelper:GetPropertyValue(oLogAttribute, "Value":U) .
                            ELSE
                                oLogAttributeValue = ? .

                            IF VALID-OBJECT (oLogAttributeValue) THEN
                                Consultingwerk.Debug:Log ("LogProperties":U, SUBSTITUTE ("          Attribute: &1 (&2)":U,
                                                                                         oLogAttribute:GetType():FullName,
                                                                                         UNBOX (oLogAttributeValue))) .
                            ELSE
                                Consultingwerk.Debug:Log ("LogProperties":U, SUBSTITUTE ("          Attribute: &1":U,
                                                                                         oLogAttribute:GetType():FullName)) .
                        END .
                    END.

                    oLogAttribute = Consultingwerk.Util.ReflectionHelper:GetPropertyAttribute (oLogProperty:ComponentType,
                                                                                               oLogProperty:Name,
                                                                                               Progress.Util.TypeHelper:GetType ("System.ComponentModel.DefaultValueAttribute":U)) .

                    Consultingwerk.Debug:Log ("LogProperties":U, SUBSTITUTE ("          DefaultValue: &1":U,
                                                                             UNBOX (CAST (oLogAttribute, System.ComponentModel.DefaultValueAttribute):Value))) .

                    CATCH loggererror AS Progress.Lang.Error :
                        Consultingwerk.Debug:Log ("Error":U, loggererror:GetMessage (1)) .
                    END CATCH.
                END .
            END.

            CATCH loggererror2 AS Progress.Lang.Error :
                Consultingwerk.Debug:Log ("Error":U, loggererror2:GetMessage (1)) .
            END CATCH.
        END.

        Consultingwerk.Debug:Skip (2) .
