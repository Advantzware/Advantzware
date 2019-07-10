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
/// Summary description for Users
/// </summary>
[System.ComponentModel.DataObject]
public class Users : AppServerConnect.AppServer
{
	public Users()
	{
		//
		// TODO: Add constructor logic here
		//
	}

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsUsersDataSet SelectUsers(string prmAction)
    {
        
        dsUsersDataSet dsUsers = new dsUsersDataSet();
        dsUsers = null;
        AppServerConnect();
        aoObject.Users1(prmAction, "", ref dsUsers);
        AppServerDisconnect();
        
        return dsUsers;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsUsersDataSet SelectUsers(string prmAction, string prmItem)
    {
        
        dsUsersDataSet dsUsers = new dsUsersDataSet();
        dsUsers = null;
        
        AppServerConnect();
        aoObject.Users1(prmAction, prmItem, ref dsUsers);
        AppServerDisconnect();
        HttpContext.Current.Cache.Insert("UsersData", dsUsers, null, DateTime.Now.AddMinutes(10), System.Web.Caching.Cache.NoSlidingExpiration);
        return dsUsers;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Update, true)]
    public dsUsersDataSet UpdateUsers(UsersData itData)
    {
        dsUsersDataSet dsUsersChange = new dsUsersDataSet();
        dsUsersDataSet dsUsers = new dsUsersDataSet();
        dsUsers = null;
        dsUsers = (dsUsersDataSet)HttpContext.Current.Cache.Get("UsersData");
        
        dsUsers.ttUsers[0].user_name = itData.user_name;
        dsUsers.ttUsers[0].track_usage =itData.track_usage;
        dsUsers.ttUsers[0].use_colors = itData.use_colors;
        dsUsers.ttUsers[0].use_fonts = itData.use_fonts;
        dsUsers.ttUsers[0].use_ctrl_keys = itData.use_ctrl_keys;
        dsUsers.ttUsers[0].developer = itData.developer;
        dsUsers.ttUsers[0].image_filename = itData.image_filename;
        //dsUsers.ttUsers[0].Language = itData.Language;
        
        dsUsersChange = (dsUsersDataSet)dsUsers.GetChanges();
        AppServerConnect();
        aoObject.Users1("Update", "", ref dsUsers);
        AppServerDisconnect();

        return dsUsers;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Delete, true)]
    public dsUsersDataSet DeleteUsers(UsersData itData)
    {
        dsUsersDataSet dsUsersChange = new dsUsersDataSet();
        dsUsersDataSet dsUsers = new dsUsersDataSet();
        dsUsers = null;
        dsUsers = (dsUsersDataSet)HttpContext.Current.Cache.Get("UsersData");
        dsUsers.ttUsers[0].Delete();
        dsUsersChange = (dsUsersDataSet)dsUsers.GetChanges();
        AppServerConnect();
        aoObject.Users1("Delete", "", ref dsUsersChange);
        AppServerDisconnect();

        return dsUsers;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Insert, true)]
    public dsUsersDataSet InsertUsers(UsersData itData)
    {
        dsUsersDataSet dsUsers = new dsUsersDataSet();
        dsUsersDataSet.ttUsersRow dsUsersRow;

        //dsUsers = null;
        dsUsersRow = dsUsers.ttUsers.NewttUsersRow();
        dsUsersRow.user_id = itData.user_id;
        dsUsersRow.user_name = itData.user_name;
        dsUsersRow.track_usage =itData.track_usage;
        dsUsersRow.use_colors = itData.use_colors;       
        dsUsersRow.use_fonts = itData.use_fonts;
        dsUsersRow.use_ctrl_keys = itData.use_ctrl_keys;
        dsUsersRow.developer = itData.developer;
        dsUsersRow.image_filename = itData.image_filename;
       // dsUsersRow.Language = itData.Language;
        dsUsers.ttUsers.AddttUsersRow(dsUsersRow);
        
        AppServerConnect();
        aoObject.Users1("Insert", "", ref dsUsers);
        AppServerDisconnect();

        return dsUsers;
    }
}

public class UsersData
{
        
    private string _user_id;
    private string _user_name;
    private bool  _track_usage;
    private bool _use_colors;
    private bool _use_fonts;
    private bool _use_ctrl_keys;
    private bool _developer;
    private string _image_filename;
   // private string _Language;
   
    public string user_id
    {
        get { return _user_id; }
        set { _user_id = value; }
    }
    
    public string user_name
    {
        get { return _user_name; }
        set { _user_name = value; }
    }

    public bool track_usage
    {
        get { return _track_usage; }
        set { _track_usage = value; }
    }



    public bool use_colors
    {
        get { return _use_colors; }
        set { _use_colors = value; }
    }

    public bool use_fonts
    {
        get { return _use_fonts; }
        set { _use_fonts = value; }
    }

    public bool use_ctrl_keys
    {
        get { return _use_ctrl_keys; }
        set { _use_ctrl_keys = value; }
    }

    public bool developer
    {
        get { return _developer; }
        set { _developer = value; }
    }
    
    public string image_filename
    {
        get { return _image_filename; }
        set { _image_filename = value; }
    }
   // public string Language
   // {
     //   get { return _iLanguage; }
     //   set { _Language = value; }
   // }

    
    public UsersData()
    {
    }
}
