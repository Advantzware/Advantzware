 
using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;

public partial class changepwd : System.Web.UI.Page
{
    private string  sUserName = "";

    protected void Page_Load(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);

        lblMessage.Text = "";

        sUserName = ((UserClass)(Session["User"])).UserName;
        if (sUserName == "Guest" || sUserName == "") 
        {
            string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
            if (sLoginURL == string.Empty) sLoginURL = "login.aspx";
            Response.Redirect(sLoginURL);
        }

        if (!IsPostBack && !(Page.Request.UrlReferrer == null))
        {

            ViewState["hlBack"] = Page.Request.UrlReferrer.ToString();
        }


    }

    protected void ChangePassword1_ChangingPassword(object sender, LoginCancelEventArgs e)
    {
        e.Cancel = true;

        string  sTable = ConfigurationManager.AppSettings["UserListTable"];
        string  sLogin = ConfigurationManager.AppSettings["FieldUserLogin"];
        string  sLoginType = ConfigurationManager.AppSettings["FieldUserLoginType"];
        string  sPwd = ConfigurationManager.AppSettings["FieldUserPwd"];
        string  sPwdType = ConfigurationManager.AppSettings["FieldUserPwdType"];
        string  sSQLSelect = "SELECT [" + sLogin + "] FROM [" + sTable + "] WHERE [" + sLogin + "] = @Login and [" + sPwd + "] = @Password";

    string sPwdOld = ChangePassword1.CurrentPassword.Trim(); 
    string sPwdNew1 = ChangePassword1.NewPassword.Trim();
        

        ConnectionStringSettings cts = ConfigurationManager.ConnectionStrings["Project1ConnectionString"];
        SqlDataSource sds = new SqlDataSource(cts.ProviderName, cts.ConnectionString, sSQLSelect);
        
        try 
        {            
            sds.DataSourceMode = SqlDataSourceMode.DataReader;
            sds.SelectParameters.Add("Login", (TypeCode)Enum.Parse(typeof(TypeCode), sLoginType), sUserName);
            sds.SelectParameters.Add("Password", (TypeCode)Enum.Parse(typeof(TypeCode), sPwdType), sPwdOld);
            
            System.Data.Common.DbDataReader dbDr = (System.Data.Common.DbDataReader)sds.Select(System.Web.UI.DataSourceSelectArguments.Empty);
            if (dbDr.HasRows)
            {
                sds.UpdateCommand = "update [" + sTable + "] set [" + sPwd + "] = @Password where [" + sLogin + "] = @Login and [" + sPwd + "] = @OldPassword";
                sds.UpdateParameters.Add("Password", (TypeCode)Enum.Parse(typeof(TypeCode), sPwdType), sPwdNew1);
                sds.UpdateParameters.Add("Login", (TypeCode)Enum.Parse(typeof(TypeCode), sLoginType), sUserName);                
        sds.UpdateParameters.Add("OldPassword", (TypeCode)Enum.Parse(typeof(TypeCode), sPwdType), sPwdOld);
                sds.Update();

                lblMessage.Text = "Password was changed" + "<BR><a href=" + ViewState["hlBack"].ToString() + ">Back</a>";
            }
            else
            {
                lblMessage.Text = "Invalid password";
            }

        } 
        catch (Exception ex)
        { 
            lblMessage.Text += "Error description" + ": " + ex.Message + "<p>";
        }
        finally
        {
            sds.Dispose();
        }
    }

    protected void  ChangePassword1_CancelButtonClick(object sender, EventArgs e)
    {
    Response.Redirect(ViewState["hlBack"].ToString());
    }
    
}
