
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

public partial class Cstatus_viewlist : System.Web.UI.Page
{

    

    protected void Page_Load(object sender, System.EventArgs e)
    {
       
        if (Session["status_list_code"] == null)
        {
            AddNewButton.Visible = true;
        }
        else
        {
            AddNewButton.Visible = false;
        }

        //UserClass.CheckLogin(Page);
        //Response.Write(Session["status_list_code"]);
        if (!func.CheckUserPermissions("[dbo].[status]", "SA"))
        {
            Response.Write("<p>" + "You don't have permissions to access this table" + "<a href=\"login.aspx\">&nbsp;" + "Back to login page" + "</a></p>");
            Response.End();
        }
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "status_viewlist.aspx";
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



        lblMessage.Text = "";
       

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

            
            if (func.CheckUserPermissions("[dbo].[status]", "s"))
            {
                
            }
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

        
        Response.Redirect(sMenuURL);
    }

    protected void Sqldatasource1_Inserted(object sender, SqlDataSourceStatusEventArgs e)
    {
        if (e.Exception == null)
        {
            //lblMessage.Text = "<b>" + "Record inserted" + "</b><p>";
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
            //lblMessage.Text = "<b>" + "Record inserted" + "</b><p>";
        }
        else
        {
            lblMessage.Text += "Error description" + ": " + e.Exception.Message + "<p>";
            e.ExceptionHandled = true;
        }
        
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
            TextBox code = (TextBox)FormView1.FindControl("descriptionTextBox");
            code.Focus();
        }
    }
    protected void lnk_status_list_Click(object sender, EventArgs e)
    {
        Response.Redirect("status_list.aspx");
    }
    protected void lnk_status_view_Click(object sender, EventArgs e)
    {
        Response.Redirect("status_viewlist.aspx");
    }
    protected void AddNewButton_Click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);
        AddNewButton.Visible = false;
    }
    protected void DeleteButton_Click(object sender, EventArgs e)
    {
        try
        {
            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            conn.Open();
            SqlCommand cmd = new SqlCommand("delete from [dbo].[status] where status_code='" + Session["status_list_code"] + "'",conn);
            cmd.ExecuteNonQuery();
            conn.Close();
        }
        catch
        {
            return;
        }
        Response.Redirect("status_list.aspx");
    }
    protected void InsertButton_Click(object sender, EventArgs e)
    {
    //    TextBox code = (TextBox)FormView1.FindControl("codeTextBox");
    //    TextBox description = (TextBox)FormView1.FindControl("descriptionTextBox");
    //    Session["status_list_code"] = code.Text;
    //    Response.Write(Session["status_list_code"]);
    //    //InsertCommand = "insert into [dbo].[status] ([status_code], [description]) values (@status_code, @description) ";
    //    try
    //    {
    //        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
    //        conn.Open();
    //        SqlCommand cmd = new SqlCommand("insert into [dbo].[status] (status_code, description) values ('" + Session["status_list_code"] + "','" + description.Text + "'", conn);
    //        cmd.ExecuteNonQuery();
    //        conn.Close();
    //    }
    //    catch
    //    {
    //        return;
    //    }
        
        Response.Write("<script>window.location.href='status_list.aspx'</script>");
    }
    
}


