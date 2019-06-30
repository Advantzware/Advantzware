using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Web.Caching;
using ASINET;
using ASIDataNS;
using Progress.Open4GL.Proxy;

[System.ComponentModel.DataObject]
public class dumpload : AppServerConnect.AppServer
{
    public dumpload()
    {
        //
        // TODO: Add constructor logic here
        //
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsDumpLoadDataSet SelectDumpLoad(string prmAct1, string prmAct2, string PrmPath)
    {

        dsDumpLoadDataSet dsDumpLoad = new dsDumpLoadDataSet();
        dsDumpLoad = null;
        AppServerConnect();
        aoObject.dumpload(prmAct1, prmAct2, PrmPath, ref dsDumpLoad);
        AppServerDisconnect();

        return dsDumpLoad;
    }
}
    
