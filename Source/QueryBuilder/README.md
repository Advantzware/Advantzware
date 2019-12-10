# QueryBuilder
Query Builder and Viewer for Progress databases. 

## Install QueryBuilder
- Create a folder 'QueryBuilder' and extract all files in it
- Open a Proenv session in 'QueryBuilder\bin'
- Run the command file 'create_db.cmd' and close the Proenv session

## Run QueryBuilder
Start QueryBuilder by double clicking 'QueryBuilder\QueryBuilder.pf':

![](https://i.imgur.com/z2EpMQN.png)

From here you can add, modify or run queries. 

# Special notes
## User types
The programs rely on the user being one of the following types:
- normal
- admin
- superadmin

Currently, this is implemented in a minimal way since it should be part of the host application to set the user's level. This is implemented in the program 'queryLib.p'. Look for the variable gcUserType and the functions setUserType and getUserType. In the window above, you can see the types at the lower right as a radioset with 3 options. This radioset is there solely for testing purposes, so you can easily switch between user types. 

## Forbidden tables and fields
In the src folder you will find files whose name look like 'SchemaRestrictions-*.txt' with the user type at the place of the asterisk. In this file you can specify the tables and fields the user of that type should not have access to. Even for superAdmins, there may be tables you do not want them to mess with. Specify tables or field names (with table prefix) each on their own line. A typical restrictions file might look similar to this:
```
benefits
employee
family.CoveredOnBenefits
family.BenefitDate
```
To keep users away from some sensitive information. 








