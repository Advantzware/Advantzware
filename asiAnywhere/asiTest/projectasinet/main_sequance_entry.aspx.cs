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

public partial class main_show_hide_ord_entry : System.Web.UI.Page
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
            string vPage = "main_sequance_entry.aspx";
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

        

        BuildDataSource();
        if (!Page.IsPostBack)
        {            
            //BuildDataSource2();
            
            
            //try
            //{
                ddl_main_program.SelectedIndex = 0;
                ddl_sub_program.SelectedIndex = 0;                
            //}
            //catch { }
        } 
       
        if (Session["main_ord_entry_seqnum_2_index"] != null)
        {
            try
            {
                Session["main_ord_entry_seqnum_1"] = ((Label)grid_view1.SelectedRow.FindControl("seqLabel")).Text;
                Session["main_ord_entry_main_program_1"] = ((Label)grid_view1.SelectedRow.FindControl("main_programLabel")).Text;
                Session["main_ord_entry_sub_program_1"] = ((Label)grid_view1.SelectedRow.FindControl("sub_programLabel")).Text;
            }
            catch { }
        }
    }
    private void BuildDataSource2()
    {
        UserClass UserLogin = (UserClass)Session["User"];
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        { 
            conn.Open();            
            string cmd = "select column_seq_user.col_name , column_seq_user.seq_no , column_seq_user.col_val, column_seq_user.main_program, column_seq_user.sub_program   from column_seq_user , column_maintenance where column_seq_user.user_name = '" + UserLogin.UserName + "' and column_maintenance.sub_program = '" + "order_inquiry.aspx" + "' and column_seq_user.main_program = '" + "order_inquiry.aspx" + "' and column_seq_user.sub_program = '" + "order_inquiry.aspx" + "' and column_seq_user.col_val = column_maintenance.col_val  order by seq_no  ";           
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);
            
            grid_view1.DataSource = ds;
            grid_view1.DataBind();
            conn.Close();
        }
        catch
        { }
        finally
        {
            conn.Close();
        }
    }
    private void BuildDataSource()
    {        
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();          

            string cmd = "select column_maintenance.col_name , column_maintenance.seq_no , column_maintenance.col_val, column_maintenance.main_program, column_maintenance.sub_program  from column_maintenance where  column_maintenance.sub_program = '" + ddl_sub_program.SelectedValue + "' order by seq_no  ";            
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);                                 
            grid_view1.DataSource = ds;
            grid_view1.DataBind();
            conn.Close();
        }
        catch { }
        finally
        {
            conn.Close();
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
                Session["ord_entry_main_prgrm_value"] = ((Label)FormView1.FindControl("main_programLabel")).Text;
                Session["ord_entry_sub_prgrm_value"] = ((Label)FormView1.FindControl("sub_programLabel")).Text;              
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

    
    protected void ddl_main_program_selectedindexchanged(object sender, EventArgs e)
    {        
        BuildDataSource();
        //if (grid_view1.SelectedIndex < 0)
        //{
        //    UErrorLabel.Text = "Please First select any record!";
        //    return;
        //}
    }
    protected void ddl_sub_program_selectedindexchanged(object sender, EventArgs e)
    {
        
    }

    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        UErrorLabel.Visible = false;

        Session["main_ord_entry_seqnum_1"] = ((Label)grid_view1.SelectedRow.FindControl("seqLabel")).Text;
        Session["main_ord_entry_main_program_1"] = ((Label)grid_view1.SelectedRow.FindControl("main_programLabel")).Text;
        Session["main_ord_entry_sub_program_1"] = ((Label)grid_view1.SelectedRow.FindControl("sub_programLabel")).Text;
        Session["main_ord_entry_seqnum_2_index"] = grid_view1.SelectedIndex;
        
    }
    protected void GridView1_RowCreated(object sender, GridViewRowEventArgs e)
    {
       
    }
    protected void GridView1_PageIndexChanging(object sender, GridViewPageEventArgs e)
    {
        try
        {
            grid_view1.PageIndex = e.NewPageIndex;
            Session["main_ord_entry_seqnum_2_index"] = e.NewPageIndex;
        }
        catch { }        
    }
           

    protected void Update_Button_Click(object sender, EventArgs e)
    {
        Label main_program = (Label)FormView1.FindControl("main_programLabel");
        Label sub_program = (Label)FormView1.FindControl("sub_programLabel");
        Label seq_no = (Label)FormView1.FindControl("seq_noLabel");
        TextBox col_name = (TextBox)FormView1.FindControl("col_nameTextBox");
        TextBox col_val = (TextBox)FormView1.FindControl("col_valTextBox");

        UserClass UserLogin = (UserClass)Session["User"];
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        conn.Open();
        SqlCommand cmd = new SqlCommand("update column_maintenance set col_name = '" + col_name.Text.Trim() + "', col_val = '" + col_val.Text.Trim() + "'" + " where main_program = '" + main_program.Text.Trim() + "' and sub_program = '" + sub_program.Text.Trim() + "' and seq_no = '" + seq_no.Text.Trim() + "' ", conn);
        cmd.ExecuteNonQuery();
        conn.Close();

    }

    protected void Insert_Button_Click(object sender, EventArgs e)
    {
        Int32 seq_val = 0;     
        TextBox col_name = (TextBox)FormView1.FindControl("col_nameTextBox");
        TextBox col_val = (TextBox)FormView1.FindControl("col_valTextBox");

        UserClass UserLogin = (UserClass)Session["User"];
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        conn.Open();

        

        //string cmd = "select max(seq_no) as seqnum from column_maintenance where main_program = '" + Session["ord_entry_main_prgrm_value"] + "' and sub_program = '" + Session["ord_entry_sub_prgrm_value"] + "' ";
        //SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
        //DataSet ds = new DataSet();
        //da.Fill(ds); 
        //seq_val = Convert.ToInt32((ds.Tables[0].Rows[0][0])) + 1;
        
        string cmd = "select max(seq_no) as seqnum from column_maintenance where main_program = '" + ddl_main_program.SelectedValue + "' and sub_program = '" + ddl_sub_program.SelectedValue + "' ";
        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
        DataSet ds = new DataSet();
        da.Fill(ds);
        try
        {
            seq_val = Convert.ToInt32((ds.Tables[0].Rows[0][0])) + 1;
        }
        catch { seq_val = 1; }

        SqlCommand cmd_insert = new SqlCommand("insert into column_maintenance (main_program, sub_program, col_name, col_val, seq_no, display) values ('" + ddl_main_program.SelectedValue + "','" + ddl_sub_program.SelectedValue + "','" + col_name.Text.Trim() + "','" + col_val.Text.Trim() + "','" + seq_val + "', '0')", conn);
        cmd_insert.ExecuteNonQuery();
        
        conn.Close();
    }        
   

    protected void grid_view1_PreRender(object sender, EventArgs e)
    {
        BuildDataSource();
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
        if (grid_view1.SelectedIndex < 0)
        {
            UErrorLabel.Text = "Please First select any record!";
            return;
        }

        Label main_program = (Label)FormView1.FindControl("main_programLabel");
        Label sub_program = (Label)FormView1.FindControl("sub_programLabel");
        Label seq_no = (Label)FormView1.FindControl("seq_noLabel");                     

        UserClass UserLogin = (UserClass)Session["User"];
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        conn.Open();
        SqlCommand cmd = new SqlCommand("delete from column_maintenance where main_program = '" + main_program.Text.Trim() + "' and sub_program = '" + sub_program.Text.Trim() + "' and seq_no = '" + seq_no.Text.Trim() + "' ", conn);
        cmd.ExecuteNonQuery();
        conn.Close();                
    }
}
