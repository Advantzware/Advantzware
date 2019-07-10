 
#region " using "
using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Collections;
using System.IO;
using System.Web.UI.WebControls;
#endregion

public partial class Cmain_menu_FullText : System.Web.UI.Page
{	
	private string sKeyFields;
    private string sFieldName = "";
    
private void Page_Load( object sender,  System.EventArgs e)
{
    //Put user code to initialize the page here
    
    CheckLogin();
    
    lblText.Text = "";
    if ( Request.QueryString["key"] != null ) 
    {
        sKeyFields = Request.QueryString["key"];
        if ( Request.QueryString["field"] != null ) sFieldName = Request.QueryString["field"].Split(';')[0];
    } 
    else 
    {
        lblText.Text = "Invalid key field value" + "<p>";
        return;
    }    
    if (!Page.IsPostBack) 
    {
        BindData();
    }
  }

private void BindData() 
{
    if ( sKeyFields == "" || sFieldName == "" ) return;

    ConnectionStringSettings cts = ConfigurationManager.ConnectionStrings["Project1ConnectionString"];
    SqlDataSource sds = new SqlDataSource();
  
	try 
	{

        sds.ConnectionString = cts.ConnectionString;
        sds.ProviderName = cts.ProviderName;
        sds.DataSourceMode = SqlDataSourceMode.DataReader;
        sds.SelectCommand = "select [" + sFieldName  + "] from [dbo].[main_menu] where 2=2 ";

        if ( func.GetsAdvSecurityMethod("[dbo].[main_menu]") == "1" && (! func.IsAdminUser()))
        {
            string sOwnerIDField = func.GetOwnerIDField("[dbo].[main_menu]");
            if (sOwnerIDField != string.Empty ) 
            {
                sds.SelectCommand += " And [" + sOwnerIDField + "]=@"+func.BuildParameterName(sOwnerIDField);
                sds.SelectParameters.Add(func.BuildParameterName(sOwnerIDField),TypeCode.String, ((UserClass)Session["User"]).UserID);
            }
        }

        foreach(string s in sKeyFields.Split(";".ToCharArray()))
        {
            if(s.Split('=').Length != 2) continue;
            string sKeyFieldName = s.Split('=')[0];
            sds.SelectCommand +=  " And [" + sKeyFieldName + "]=@"+func.BuildParameterName(sKeyFieldName);           
            sds.SelectParameters.Add(func.BuildParameterName(sKeyFieldName), s.Split('=')[1]);
        }
        System.Data.Common.DbDataReader dbDr = (System.Data.Common.DbDataReader)sds.Select(System.Web.UI.DataSourceSelectArguments.Empty);          
        if (dbDr.HasRows && dbDr.Read()) lblText.Text = dbDr[sFieldName].ToString();        
    }
    finally
    {
        sds.Dispose();
    }        
}

private void CheckLogin() 
{
    if ((Page.Session["User"] == null || (((UserClass)Session["User"]).ID == Guid.Empty) || !func.CheckUserPermissions("[dbo].[main_menu]", "s")))
    {
      Response.Write("<p>" + "You don't have permissions to access this table" + "</p>");
      Response.End();
    }
}

}