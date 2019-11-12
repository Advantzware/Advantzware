
/*------------------------------------------------------------------------
    File        : grantSQLpermissions.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : MYT
    Created     : Sun Oct 20 10:11:26 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
OUTPUT TO VALUE("c:\tmp\GrantAll.sql").

FOR EACH _File WHERE _Tbl-Type = "T":
    PUT UNFORMATTED "GRANT ALL ON PUB." + QUOTER(_File-Name) + " TO PUBLIC;" SKIP.
END.
PUT UNFORMATTED 
    "COMMIT WORK;" SKIP.

OUTPUT CLOSE.