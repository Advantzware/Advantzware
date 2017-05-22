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
    File        : getTablesAndFields.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Feb 28 21:24:03 CET 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{Consultingwerk/SmartComponents/Support/ttTablesAndFields.i}

DEFINE OUTPUT PARAMETER DATASET FOR dsSchema .

/* ***************************  Main Block  *************************** */

FOR EACH dictdb._File WHERE NOT dictdb._File._Hidden NO-LOCK:
    CREATE ttTables.
    ASSIGN ttTables.TableName = dictdb._File._File-Name .  
    
    FOR EACH dictdb._Field OF dictdb._File NO-LOCK:
        CREATE ttFields.
        ASSIGN ttFields.TableName   = dictdb._File._File-Name
               ttFields.ColumnName  = dictdb._Field._Field-Name
               ttFields.ColumnLabel = (IF dictdb._Field._Label > "":U THEN dictdb._Field._Label ELSE dictdb._Field._Col-Label)
               ttFields.ColumnOrder = dictdb._Field._Order
               ttFields.DataType    = dictdb._Field._Data-Type
             .  
    END.
END.    

