
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
#endregion

public partial class Cindustry_sic_listview : System.Web.UI.Page
{

    
    protected void Page_Load(object sender, System.EventArgs e)
    {
        
        if (Session["industry_sic_list_code"] == null)
        {
            newaddbutton.Visible = true;
        }
        else
        {
            newaddbutton.Visible = false;
        }

        //UserClass.CheckLogin(Page);

        if (!func.CheckUserPermissions("[dbo].[industry_sic]", "SA"))
        {
            Response.Write("<p>" + "You don't have permissions to access this table" + "<a href=\"login.aspx\">&nbsp;" + "Back to login page" + "</a></p>");
            Response.End();
        }
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "industry_sic_list.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            lblComp.Text = PrmComp;
            if (aUsers == "external")
            {
                //txt_customer.Visible = false;
                //CustomerLook.Visible = false;
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
        



        

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

        if (!Page.IsPostBack)
        {
            
            if (Session["User"] != null)
            {
                //UserClass UserLogin = (UserClass)Session["User"];
                lblUser.Text = UserLogin.UserName;

            }

            
        } 

        
        

    }

       protected void dbGrid_industry_sic_RowDataBound(object sender, GridViewRowEventArgs e)
    {
        if (e.Row.RowType == DataControlRowType.DataRow)
        {
            DataRowView rowData;
            rowData = (DataRowView)e.Row.DataItem;

        }

    }

    

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }
    protected void hlkBackToMenu_Click(object sender, EventArgs e)
    {
        string sMenuURL = ConfigurationManager.AppSettings["MenuFile"];
        if (sMenuURL == String.Empty)
        {
            Response.Write("<script language=javascript>alert('Menu page isn't set');</script>");
            return;
        }

        //ClearSession();
        Response.Redirect(sMenuURL);
    }
     


    
    protected void industry_sicSqlDataSource_Selected(object sender, SqlDataSourceStatusEventArgs e)
    {
        //lblCount.Text = "Details found" + ": " + e.AffectedRows.ToString();
    }

   
   
    
    protected void Sqldatasource1_Inserted(object sender, SqlDataSourceStatusEventArgs e)
    {
        if (e.Exception == null)
        {
            //lblMessage.Text = "<b>" + "Record updated" + "</b><p>";
        }
        else
        {
            lblMessage.Text += "Error description" + ": " + e.Exception.Message + "<p>";
            e.ExceptionHandled = true;
        }
    }
    protected void Sqldatasource1_Deleted(object sender, SqlDataSourceStatusEventArgs e)
    {
        if (e.Exception == null)
        {
            //lblMessage.Text = "<b>" + "Record updated" + "</b><p>";
        }
        else
        {
            lblMessage.Text += "Error description" + ": " + e.Exception.Message + "<p>";
            e.ExceptionHandled = true;
        }
        
    }
    protected void lnk_listview_industry_Click(object sender, EventArgs e)
    {

        Response.Redirect("industry_sic_list.aspx");

    }

    protected void lnk_listview_sic_Click(object sender, EventArgs e)
    {
        Response.Redirect("industry_sic_listview.aspx");
    }
    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            TextBox code = (TextBox)FormView1.FindControl("codeTextBox");
            code.Focus();
        }
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            TextBox desc = (TextBox)FormView1.FindControl("descriptionTextBox");
            desc.Focus();
        }
    }

    protected void newaddbutton_Click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);
        newaddbutton.Visible = false;
    }
    protected void DeleteButton_Click(object sender, EventArgs e)
    {
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            SqlCommand cmd = new SqlCommand("delete from industry_sic where industry_sic_code='" + Session["industry_sic_list_code"] + "'", conn);
            cmd.ExecuteNonQuery();
            conn.Close();
        }
        catch
        {
            return;
        }
        finally
        {
            conn.Close();
        }
        Response.Redirect("industry_sic_list.aspx");
    }
    protected void InsertButton_Click(object sender, EventArgs e)
    {
        Response.Write("<script> window.location.href='industry_sic_list.aspx' </script>");
    }
}
