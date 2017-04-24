The Kendo UI JavaScript Libraries are expected to be 
deployed on the same web server that also hosts the 
WebSpeed messenger executable under the following URI:
/KendoUI/js/kendo.all.min.js or /KendoUI/styles/kendo.common.min.css
etc.

Sample Apache httpd.conf entry:

Alias /KendoUI C:\Work\SmartComponents4NET\114\ABL\Consultingwerk\Web\KendoUI

<Directory "C:\Work\SmartComponents4NET\114\ABL\Consultingwerk\Web\KendoUI">
    AllowOverride None
    Options None
    Order allow,deny
    Allow from all
</Directory>