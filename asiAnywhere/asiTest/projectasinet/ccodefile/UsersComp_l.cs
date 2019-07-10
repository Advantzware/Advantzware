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

/// <summary>
/// Summary description for UserComp_l
/// </summary>
[System.ComponentModel.DataObject]
public class UserComp_l : AppServerConnect.AppServer
{
	public UserComp_l()
	{
		//
		// TODO: Add constructor logic here
		//
	}

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsUserComp_lDataSet SelectUserComp_l(string prmAction)
    {
        
        dsUserComp_lDataSet dsUserComp_l = new dsUserComp_lDataSet();
        dsUserComp_l = null;
        AppServerConnect();
        aoObject.UsersComp_l(prmAction, "","", ref dsUserComp_l);
        AppServerDisconnect();
        
        return dsUserComp_l;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsUserComp_lDataSet SelectUserComp_l(string prmAction, string prmItem, string vComp_l)
    {
        
        dsUserComp_lDataSet dsUserComp_l = new dsUserComp_lDataSet();
        dsUserComp_l = null;
        
        AppServerConnect();
        aoObject.UsersComp_l(prmAction, prmItem, vComp_l, ref dsUserComp_l);
        AppServerDisconnect();
        HttpContext.Current.Cache.Insert("LocData", dsUserComp_l, null, DateTime.Now.AddMinutes(10), System.Web.Caching.Cache.NoSlidingExpiration);
        return dsUserComp_l;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Update, true)]
    public dsUserComp_lDataSet UpdateUserComp_l(LocData itData)
    {
        dsUserComp_lDataSet dsUserComp_lChange = new dsUserComp_lDataSet();
        dsUserComp_lDataSet dsUserComp_l = new dsUserComp_lDataSet();
        dsUserComp_l = null;
        dsUserComp_l = (dsUserComp_lDataSet)HttpContext.Current.Cache.Get("LocData");

        //dsUserComp_l.ttUserComp_l[0].loc_default = itData.loc_default;
        dsUserComp_lChange = (dsUserComp_lDataSet)dsUserComp_l.GetChanges();
        AppServerConnect();
        aoObject.UsersComp_l("Update", "","", ref dsUserComp_l);
        AppServerDisconnect();

        return dsUserComp_l;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Delete, true)]
    public dsUserComp_lDataSet DeleteUserComp_l(LocData itData)
    {
        dsUserComp_lDataSet dsUserComp_lChange = new dsUserComp_lDataSet();
        dsUserComp_lDataSet dsUserComp_l = new dsUserComp_lDataSet();
        dsUserComp_l = null;
        dsUserComp_l = (dsUserComp_lDataSet)HttpContext.Current.Cache.Get("LocData");
        dsUserComp_l.ttUserComp_l[0].Delete();
        dsUserComp_lChange = (dsUserComp_lDataSet)dsUserComp_l.GetChanges();
        AppServerConnect();
        aoObject.UsersComp_l("Delete", "","", ref dsUserComp_lChange);
        AppServerDisconnect();

        return dsUserComp_l;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Insert, true)]
    public dsUserComp_lDataSet InsertUserComp_l(LocData itData)
    {
        dsUserComp_lDataSet dsUserComp_l = new dsUserComp_lDataSet();
        dsUserComp_lDataSet.ttUserComp_lRow dsUserComp_lRow;

        //dsUserComp_l = null;
        dsUserComp_lRow = dsUserComp_l.ttUserComp_l.NewttUserComp_lRow();
        dsUserComp_lRow.loc = itData.loc;
        //dsUserComp_lRow.loc_default = itData.loc_default;
        dsUserComp_l.ttUserComp_l.AddttUserComp_lRow(dsUserComp_lRow);
        
        AppServerConnect();
        aoObject.UsersComp_l("Insert", "","", ref dsUserComp_l);
        AppServerDisconnect();

        return dsUserComp_l;
    }
}

public class LocData
{
        
    private string _loc;
    private string _loc_default;

    public string loc
    {
        get { return _loc; }
        set { _loc = value; }
    }

    public string loc_default
    {
        get { return _loc_default; }
        set { _loc_default = value; }
    }
public LocData()
    {
    }
}
