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
using System.Data.SqlClient;

/// <summary>
/// Summary description for Class1
/// </summary>
[System.ComponentModel.DataObject]
public class download : AppServerConnect.AppServer
{
    public download()
    {
        //
        // TODO: Add constructor logic here
        //
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsContactList2DataSet contactlist(string prmAction, string prmUser, string prmComp, string prmCust)
    {

        dsContactList2DataSet dsContactList = new dsContactList2DataSet();
        dsContactList = null;
        AppServerConnect();
        aoObject.Cust_contact(prmAction, prmUser, prmComp, prmCust, ref dsContactList);
        AppServerDisconnect();

        return dsContactList;

    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsContactList3DataSet shiplist(string prmAction, string prmUser, string prmComp, string prmCust)
    {

        dsContactList3DataSet dsShipList = new dsContactList3DataSet();
        dsShipList = null;
        AppServerConnect();
        aoObject.Ship_contact(prmAction, prmUser, prmComp, prmCust, ref dsShipList);
        AppServerDisconnect();

        return dsShipList;

    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsNotesDataSet custnotes(string prmAction, string prmUser, string prmComp, string prmCust)
    {

        dsNotesDataSet dscustnotes = new dsNotesDataSet();
        dscustnotes = null;
        AppServerConnect();
        aoObject.CustNotes(prmAction, prmUser, prmComp, prmCust, ref dscustnotes);
        AppServerDisconnect();

        return dscustnotes;

    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsNotes2DataSet shipnotes(string prmAction, string prmUser, string prmComp)
    {

        dsNotes2DataSet dsshipnotes = new dsNotes2DataSet();
        dsshipnotes = null;
        AppServerConnect();
        aoObject.CustNotes2(prmAction, prmUser, prmComp, ref dsshipnotes);
        AppServerDisconnect();

        return dsshipnotes;

    }
}
