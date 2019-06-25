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

public partial class Cmain_menu_Add : System.Web.UI.Page
{

    protected void Page_Load(object sender, EventArgs e)
    {

        UserClass.CheckLogin(Page);


        if (!func.CheckUserPermissions("[dbo].[main_menu]", "a"))
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

        }
    }

    protected void BindData()
    {
        try
        {

        }
        catch (Exception ex)
        {
            lblMessage.Text += "Error description" + ": " + ex.Message + "<p>";
        }
    }

    protected void DetailsView1_DataBound(object sender, EventArgs e)
    {

    }
    protected void main_menuDetailsView_DataBound(object sender, EventArgs e)
    {
        if (main_menuDetailsView.CurrentMode == DetailsViewMode.Insert)
        {
            DropDownList menulist = (DropDownList)main_menuDetailsView.FindControl("DropDownList1");
            TextBox menutext = (TextBox)main_menuDetailsView.FindControl("TextBox1");
            int val = Convert.ToInt32(menulist.SelectedValue);
            menutext.Text = Convert.ToString(val + 1);
        }

        BindData();
    }

    protected void main_menuDetailsView_ItemInserting(object sender, DetailsViewInsertEventArgs e)
    {

    }

    protected void main_menuSqlDataSource_Inserted(object sender, SqlDataSourceStatusEventArgs e)
    {
        if (e.Exception == null)
        {
            lblMessage.Text = "<b>" + "Record inserted" + "</b><p>";
        }
        else
        {
            lblMessage.Text += "Error description" + ": " + e.Exception.Message + "<p>";
            e.ExceptionHandled = true;
        }

/*#if ( DEBUG )
        lblMessage.Text += "<p>InsertCommand = " + main_menuSqlDataSource.InsertCommand + "<p>";
        foreach (System.Data.Common.DbParameter p in e.Command.Parameters)
            lblMessage.Text += p.ParameterName + "(" + p.DbType.ToString() + ")=" + p.Value.ToString() + "<br>";
#endif*/
    }

    protected void main_menuSqlDataSource_Inserting(object sender, SqlDataSourceCommandEventArgs e)
    {

    }

    protected void CancelButton_Click(object sender, EventArgs e)
    {
        Response.Redirect("main_menu_list.aspx");
    }
}
