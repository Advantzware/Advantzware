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
using System.Data.SqlClient;
using System.Text;
using System.Data.OleDb;
using System.IO;
using System.Data.Common;

public partial class email_codes : System.Web.UI.Page
{  
    protected void Page_PreRender(object sender, EventArgs e)
    {
        //Response.Write(ddl_sub_program.SelectedValue);
    }
    protected void Page_Load(object sender, EventArgs e)
    {        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "email_codes.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
            lblUser.Text = UserLogin.UserName;
            labelcompany.Text = PrmComp;
            if (aUsers == "external")
            {
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");
            }
        }

       
        if (!Page.IsPostBack)
        {            
            
        }
        
    }
    
    protected void form_view_databound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {
            try
            {
                add_button.Visible = true;
                Button1.Visible = true;
                delete_button.Visible = true;              
            }
            catch { }
        }
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            add_button.Visible = false;
            Button1.Visible = false;
            delete_button.Visible = false;
        }
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            add_button.Visible = false;
            Button1.Visible = false;
            delete_button.Visible = false;
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
    

    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        UErrorLabel.Visible = false;
    }
    protected void GridView1_RowCreated(object sender, GridViewRowEventArgs e)
    {
       
    }
    protected void GridView1_PageIndexChanging(object sender, GridViewPageEventArgs e)
    {
           
    }
           

    protected void Update_Button_Click(object sender, EventArgs e)
    {
        Label program_name = (Label)FormView1.FindControl("program_nameLabel");
        TextBox email_from = (TextBox)FormView1.FindControl("email_fromTextBox");
        TextBox subject = (TextBox)FormView1.FindControl("subjectTextBox");

        UserClass UserLogin = (UserClass)Session["User"];
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        conn.Open();
        SqlCommand cmd = new SqlCommand("update email_alerts set email_from = '" + email_from.Text.Trim() + "', subject = '" + subject.Text.Trim() + "'" + " where program_name = '" + program_name.Text.Trim() + "' ", conn);
        cmd.ExecuteNonQuery();
        conn.Close();

        grid_view1.DataBind();

    }

    protected void Insert_Button_Click(object sender, EventArgs e)
    {       
        TextBox program_name = (TextBox)FormView1.FindControl("program_nameTextBox");
        TextBox email_from = (TextBox)FormView1.FindControl("email_fromTextBox");
        TextBox subject = (TextBox)FormView1.FindControl("subjectTextBox");

        UserClass UserLogin = (UserClass)Session["User"];

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            string cmd = "select program_name from email_alerts where program_name = '" + program_name.Text.Trim() + "' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);
   
            if (ds.Tables[0].Rows.Count > 0)
            {
                UErrorLabel.Text = "Program Name already exists!";
                FormView1.ChangeMode(FormViewMode.Insert);
                return;
            }

            SqlCommand cmd_insert = new SqlCommand("insert into email_alerts (program_name, email_from, subject) values ('" + program_name.Text.Trim() + "','" + email_from.Text.Trim() + "','" + subject.Text.Trim() + "')", conn);
            cmd_insert.ExecuteNonQuery();
            conn.Close();

            //FormView1.ChangeMode(FormViewMode.ReadOnly);
            UErrorLabel.Text = "";
            grid_view1.DataBind();
        }
        catch { }
    }        
   

    protected void grid_view1_PreRender(object sender, EventArgs e)
    {
     
    }


    protected void add_button_click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);
    }


    protected void update_button_click1(object sender, EventArgs e)
    {
        if (grid_view1.SelectedIndex < 0)
        {
            UErrorLabel.Text = "Please First select any record!";
            return;
        }
        FormView1.ChangeMode(FormViewMode.Edit);
    }

    protected void delete_button_click1(object sender, EventArgs e)
    {
        try
        {
            if (grid_view1.SelectedIndex < 0)
            {
                UErrorLabel.Text = "Please First select any record!";
                return;
            }

            Label program_name = (Label)FormView1.FindControl("program_nameLabel");

            UserClass UserLogin = (UserClass)Session["User"];
            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            conn.Open();
            SqlCommand cmd = new SqlCommand("delete from email_alerts where program_name = '" + program_name.Text.Trim() + "' ", conn);
            cmd.ExecuteNonQuery();
            conn.Close();

            grid_view1.DataBind();
        }
        catch { }
    }
}
