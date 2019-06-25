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

public partial class Cmain_menu_Edit : System.Web.UI.Page
{

    protected void Page_Load(object sender, EventArgs e)
    {

        UserClass.CheckLogin(Page);

        if (!func.CheckUserPermissions("[dbo].[main_menu]", "se")) 
        {
            Response.Write("<p>" + "You don't have permissions to access this table" + "<a href=\"login.aspx\">&nbsp;" + "Back to login page" + "</a></p>");
            Response.End();
        }  

        lblMessage.Text = "";

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture)) 
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        } 

        if (!Page.IsPostBack) 
        {

            BindData();
        }    

    }
    
    protected void BindData() 
    {
        try
        {
            main_menuDetailsView.DataBind();
        }
        catch (Exception ex)
        {
            lblMessage.Text += "Error description" + ": " + ex.Message + "<p>";
        }
    }
    
    protected void main_menuDetailsView_DataBound(object sender, EventArgs e)
    {

        string sOwnerIDField = func.GetOwnerIDField("[dbo].[main_menu]");
        if (func.GetsAdvSecurityMethod("[dbo].[main_menu]") != "0" && (!func.IsAdminUser()) && sOwnerIDField != string.Empty && main_menuDetailsView.DataItem != null) 
            if (DataBinder.Eval(main_menuDetailsView.DataItem, sOwnerIDField).ToString() != ((UserClass)Session["User"]).UserID)
            {
                main_menuDetailsView.Visible = false;
                lblMessage.Text = "You are not allowed to edit this data<p>";
                return;
            }


    }
    
    protected void main_menuDetailsView_ItemUpdating(object sender, DetailsViewUpdateEventArgs e)
    {         

    }
    
    protected void main_menuSqlDataSource_Selected(object sender, SqlDataSourceStatusEventArgs e)
    {
        if (e.Exception != null)
        {
            lblMessage.Text += "Error description" + ": " + e.Exception.Message + "<p>";
            e.ExceptionHandled = true;
        }
/*#if ( DEBUG )
        lblMessage.Text += "<p>SelectCommand = " + main_menuSqlDataSource.SelectCommand + "<p>";
        foreach (System.Data.Common.DbParameter p in e.Command.Parameters)
            lblMessage.Text += p.ParameterName + "(" + p.DbType.ToString() + ")=" + p.Value.ToString() + "<br>";
#endif*/
    }

    protected void main_menuSqlDataSource_Updated(object sender, SqlDataSourceStatusEventArgs e)
    {
        if (e.Exception == null)
        {
            lblMessage.Text = "<b>" + "Record updated" + "</b><p>";
        }
        else
        {
            lblMessage.Text += "Error description" + ": " + e.Exception.Message + "<p>";
            e.ExceptionHandled = true;
        }
        
/*#if ( DEBUG )
        lblMessage.Text += "<p>UpdateCommand = " + main_menuSqlDataSource.UpdateCommand + "<p>";
        foreach (System.Data.Common.DbParameter p in e.Command.Parameters)
            lblMessage.Text += p.ParameterName + "(" + p.DbType.ToString() + ")=" + p.Value.ToString() + "<br>";
#endif*/
    } 
    
    protected void main_menuSqlDataSource_Updating(object sender, SqlDataSourceCommandEventArgs e)
    {

    }

    protected void CancelButton_Click(object sender, EventArgs e)
    {
        Response.Redirect("main_menu_list.aspx");
    }
}
