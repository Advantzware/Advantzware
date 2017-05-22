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
    File        : FieldDesignerHelper
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Sep 14 00:16:03 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Util.*         FROM PROPATH .
USING Progress.Lang.*               FROM PROPATH .
USING System.Drawing.*              FROM ASSEMBLY .
USING System.Collections.Generic.*  FROM ASSEMBLY .

CLASS Consultingwerk.Design.DesignerFieldsHelper:

    DEFINE STATIC VARIABLE oListDataTypeImage AS "Dictionary<System.String, Image>":U NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Static constructor of the DesignerFieldsHelper class.
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR STATIC DesignerFieldsHelper ():

        /* Mike Fechner, Consultingwerk Ltd. 10.01.2012
           The following ASSIGN statements does crash the compiler due to the
           combination with :U on the generic type name, see Bug 2775 for deails */
        /*ASSIGN*/ oListDataTypeImage = NEW "Dictionary<System.String, Image>":U () .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE DesignerFieldsHelper ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Control type that should be used for a given datatype
        Notes:
        @param poDataType The Progress.Data.DataType of a BindingSource field
        @return The System.Type of the .NET Control used for rendering the Control
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC System.Type DataTypeDesignTimeControl (poDataType AS Progress.Data.DataType):

        DEFINE VARIABLE cDataType  AS CHARACTER   NO-UNDO .

        ASSIGN cDataType = poDataType:ToString() .

        CASE cDataType:
            WHEN "LOGICAL":U THEN
                RETURN Progress.Util.TypeHelper:GetType ("System.Windows.Forms.CheckBox":U) .

            OTHERWISE RETURN Progress.Util.TypeHelper:GetType ("System.Windows.Forms.TextBox":U) .
        END CASE .


    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the image to represent a data type in the toolbox
        Notes:
        @param poDataType The Progress.Data.DataType of a BindingSource field
        @return The System.Drawing.Image to represent fields of that DataType on the Designer toolbox
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC Image DataTypeImage (poDataType AS Progress.Data.DataType):

        DEFINE VARIABLE cType  AS CHARACTER NO-UNDO .
        DEFINE VARIABLE oImage AS Image     NO-UNDO .

        ASSIGN cType = poDataType:ToString() .

        IF oListDataTypeImage:ContainsKey (cType) THEN
            RETURN oListDataTypeImage[cType] .

        CASE cType:
            WHEN "BLOB":U THEN                 oImage = ImageHelper:Load ("Consultingwerk/Design/Images/painting.png":U) .
            WHEN "CHARACTER":U THEN            oImage = ImageHelper:Load ("Consultingwerk/Design/Images/text.png":U) .
            WHEN "CLOB":U THEN                 oImage = ImageHelper:Load ("Consultingwerk/Design/Images/text_rich.png":U) .
            WHEN "COM_HANDLE":U THEN           oImage = ImageHelper:Load ("Consultingwerk/Design/Images/cubes_blue.png":U) .
            WHEN "DATE":U THEN                 oImage = ImageHelper:Load ("Consultingwerk/Design/Images/calendar.png":U) .
            WHEN "DATETIME":U THEN             oImage = ImageHelper:Load ("Consultingwerk/Design/Images/date_time.png":U) .
            WHEN "DATETIME_TZ":U THEN          oImage = ImageHelper:Load ("Consultingwerk/Design/Images/date_time.png":U) .
            WHEN "DECIMAL":U THEN              oImage = ImageHelper:Load ("Consultingwerk/Design/Images/calculator.png":U) .
            WHEN "HANDLE":U THEN               oImage = ImageHelper:Load ("Consultingwerk/Design/Images/cubes_blue.png":U) .
            WHEN "INT64":U THEN                oImage = ImageHelper:Load ("Consultingwerk/Design/Images/calculator.png":U) .
            WHEN "INTEGER":U THEN              oImage = ImageHelper:Load ("Consultingwerk/Design/Images/calculator.png":U) .
            WHEN "LOGICAL":U THEN              oImage = ImageHelper:Load ("Consultingwerk/Design/Images/checkbox.png":U) .
            WHEN "Progress_Lang_Object":U THEN oImage = ImageHelper:Load ("Consultingwerk/Design/Images/cubes_blue.png":U) .
            WHEN "RECID":U THEN                oImage = ImageHelper:Load ("Consultingwerk/Design/Images/data_table.png":U) .
            WHEN "ROWID":U THEN                oImage = ImageHelper:Load ("Consultingwerk/Design/Images/data_table.png":U) .

            OTHERWISE UNDO, THROW
                NEW AppError (SUBSTITUTE ("Invalid DataType &1."{&TRAN}, cType), 0) .
        END CASE .

        oListDataTypeImage:Add (cType, oImage) .

        RETURN oImage .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Default data bindable property for the Control of the given
                 System.Type
        Notes:
        @param poType The System.Type of a .NET Control
        @return The name of the property to data bind to
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER DefaultBindingPropertyForType (poType AS System.Type):

        CASE poType:
            WHEN Progress.Util.TypeHelper:GetType ("System.Windows.Forms.CheckBox":U) THEN
                RETURN "Checked":U .

            OTHERWISE
               RETURN "Text":U .
        END CASE .

    END METHOD.

END CLASS.