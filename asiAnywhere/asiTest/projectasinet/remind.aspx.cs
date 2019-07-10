 
#region " using "
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
#endregion

public partial class Remind : System.Web.UI.Page
{
 protected void Page_Load( object sender,  System.EventArgs e)
 {
        //Put user code to initialize the page here
    lblMessage.Text = "";

    string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
    if ( sLoginURL != "" ) hlBack.NavigateUrl = sLoginURL;
    btnSubmit.Attributes.Add("onclick","javascript:return UpdateValidators();");

        
}

protected void btnSubmit_Click( object sender,  System.EventArgs e)
{
    string  sTable = ConfigurationManager.AppSettings["UserListTable"];
    string  sLogin = ConfigurationManager.AppSettings["FieldUserLogin"];
    string  sLoginType = ConfigurationManager.AppSettings["FieldUserLoginType"];
    string  sPwd = ConfigurationManager.AppSettings["FieldUserPwd"];

    string  sEmail = ConfigurationManager.AppSettings["FieldUserEmail"];
    string  sEmailType = ConfigurationManager.AppSettings["FieldUserEmailType"];

    string  sSQLSelect = "SELECT ["+sLogin + "],[" + sPwd + "],[" + sEmail + "] FROM [" + sTable + "] " + 
                         "WHERE ["+(rbUserName.Checked?sLogin:sEmail) + "] = @value";
            
    ConnectionStringSettings cts = ConfigurationManager.ConnectionStrings["Project1ConnectionString"];
    SqlDataSource sds = new SqlDataSource(cts.ProviderName, cts.ConnectionString, sSQLSelect);

    if (rbUserName.Checked) sds.SelectParameters.Add("value", (TypeCode)Enum.Parse(typeof(TypeCode), sLoginType), txtUserName.Text);
    else sds.SelectParameters.Add("value", (TypeCode)Enum.Parse(typeof(TypeCode), sEmailType), txtEmail.Text);

    try 
    {            
        sds.DataSourceMode = SqlDataSourceMode.DataReader;

        System.Data.Common.DbDataReader dbDr = (System.Data.Common.DbDataReader)sds.Select(System.Web.UI.DataSourceSelectArguments.Empty);
        if (dbDr.HasRows && dbDr.Read())
        {
    
            string txtLogin = Convert.ToString(dbDr[sLogin]);
            string txtPassword = Convert.ToString(dbDr[sPwd]);
            string sUserEmail = Convert.ToString(dbDr[sEmail]);
            dbDr.Close();
                   
      
            string  sMessage = "You asked to remind your username and password at http://" + Request.ServerVariables["SERVER_NAME"] + Request.ServerVariables["SCRIPT_NAME"] + "\nUser Name: " + txtLogin + "\nPassword: " + txtPassword;             
            func.SendMail(sUserEmail, "Password reminder", sMessage);
            lblMessage.Text = "Mail was sended"; 
        }
        else
        {
            if (rbUserName.Checked) lblMessage.Text = "User" + " " + txtUserName.Text + " " + "is not registered.";
            else lblMessage.Text = "This email don't exist in our database";
            pnlChange.Visible = false;
        }
        dbDr.Dispose();
        
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

//

}

