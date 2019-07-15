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

public partial class button_maintain : System.Web.UI.Page
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
            string vPage = "button_maintain.aspx";
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
           
                ddl_main_program.SelectedIndex = 0;
             
        } 
       
        if (Session["main_ord_entry_seqnum_2_index"] != null)
        {
            try
            {
                //Session["main_ord_entry_seqnum_1"] = ((Label)grid_view1.SelectedRow.FindControl("seqLabel")).Text;
                //Session["main_ord_entry_main_program_1"] = ((Label)grid_view1.SelectedRow.FindControl("main_programLabel")).Text;
                //Session["main_ord_entry_sub_program_1"] = ((Label)grid_view1.SelectedRow.FindControl("sub_programLabel")).Text;
            }
            catch { }
        }
    }
    
    //private void BuildDataSource()
    //{        
    //    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
    //    try
    //    {
    //        conn.Open();          

    //        string cmd = "select *  from button_maintain where  button_maintain.button_id = '" + ddl_main_program.SelectedValue + "' order by button_id  ";            
    //        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
    //        DataSet ds = new DataSet();
    //        da.Fill(ds);                                 
    //        //grid_view1.DataSource = ds;
    //        //grid_view1.DataBind();
    //        //FormView1.DataSource = ds;
    //        //FormView1.DataBind();

    //        conn.Close();
    //    }
    //    catch { }
    //    finally
    //    {
    //        conn.Close();
    //    }
    //}
    protected void form_view_databound(object sender, EventArgs e)
    {    
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {            
            try
            {   
                Label chk1 = (Label)FormView1.FindControl("chkLabel1");
                Label chk2 = (Label)FormView1.FindControl("chkLabel2");
                Label chk3 = (Label)FormView1.FindControl("chkLabel3");
                Label chk4 = (Label)FormView1.FindControl("chkLabel4");
                Label chk5 = (Label)FormView1.FindControl("chkLabel5");
                Label chk6 = (Label)FormView1.FindControl("chkLabel6");
                Label chk7 = (Label)FormView1.FindControl("chkLabel7");
                Label chk8 = (Label)FormView1.FindControl("chkLabel8");
                Label chk9 = (Label)FormView1.FindControl("chkLabel9");
                Label chk10 = (Label)FormView1.FindControl("chkLabel10");
                Label chk11 = (Label)FormView1.FindControl("chkLabel11");
                Label chk12 = (Label)FormView1.FindControl("chkLabel12");
                Label chk13 = (Label)FormView1.FindControl("chkLabel13");
                Label chk14 = (Label)FormView1.FindControl("chkLabel14");
                Label chk15 = (Label)FormView1.FindControl("chkLabel15");

                CheckBox chktx1 = (CheckBox)FormView1.FindControl("chk1TextBox");
                CheckBox chktx2 = (CheckBox)FormView1.FindControl("chk2TextBox");
                CheckBox chktx3 = (CheckBox)FormView1.FindControl("chk3TextBox");
                CheckBox chktx4 = (CheckBox)FormView1.FindControl("chk4TextBox");
                CheckBox chktx5 = (CheckBox)FormView1.FindControl("chk5TextBox");
                CheckBox chktx6 = (CheckBox)FormView1.FindControl("chk6TextBox");
                CheckBox chktx7 = (CheckBox)FormView1.FindControl("chk7TextBox");
                CheckBox chktx8 = (CheckBox)FormView1.FindControl("chk8TextBox");
                CheckBox chktx9 = (CheckBox)FormView1.FindControl("chk9TextBox");
                CheckBox chktx10 = (CheckBox)FormView1.FindControl("chk10TextBox");
                CheckBox chktx11 = (CheckBox)FormView1.FindControl("chk11TextBox");
                CheckBox chktx12 = (CheckBox)FormView1.FindControl("chk12TextBox");
                CheckBox chktx13 = (CheckBox)FormView1.FindControl("chk13TextBox");
                CheckBox chktx14 = (CheckBox)FormView1.FindControl("chk14TextBox");
                CheckBox chktx15 = (CheckBox)FormView1.FindControl("chk15TextBox");
                

                if (chk1.Text == "False") {
                    chktx1.Checked = true;
                    }
                if (chk2.Text == "False")
                {
                    chktx2.Checked = true;
                }
                if (chk3.Text == "False")
                {
                    chktx3.Checked = true;
                }
                if (chk4.Text == "False")
                {
                    chktx4.Checked = true;
                }
                if (chk5.Text == "False")
                {
                    chktx5.Checked = true;
                }
                if (chk6.Text == "False")
                {
                    chktx6.Checked = true;
                }
                if (chk7.Text == "False")
                {
                    chktx7.Checked = true;
                }
                if (chk8.Text == "False")
                {
                    chktx8.Checked = true;
                }
                if (chk9.Text == "False")
                {
                    chktx9.Checked = true;
                }
                if (chk10.Text == "False")
                {
                    chktx10.Checked = true;
                }
                if (chk11.Text == "False")
                {
                    chktx11.Checked = true;
                }
                if (chk12.Text == "False")
                {
                    chktx12.Checked = true;
                }
                if (chk13.Text == "False")
                {
                    chktx13.Checked = true;
                }
                if (chk14.Text == "False")
                {
                    chktx14.Checked = true;
                }
                if (chk15.Text == "False")
                {
                    chktx15.Checked = true;
                }

                Label buton6 = (Label)FormView1.FindControl("btn6TextBox");
                Label buton7 = (Label)FormView1.FindControl("btn7TextBox");
                Label buton8 = (Label)FormView1.FindControl("btn8TextBox");
                Label buton9 = (Label)FormView1.FindControl("btn9TextBox");
                Label buton10 = (Label)FormView1.FindControl("btn10TextBox");
                Label buton11 = (Label)FormView1.FindControl("btn11TextBox");
                Label buton12 = (Label)FormView1.FindControl("btn12TextBox");
                Label buton13 = (Label)FormView1.FindControl("btn13TextBox");
                Label buton14 = (Label)FormView1.FindControl("btn14TextBox");
                Label buton15 = (Label)FormView1.FindControl("btn15TextBox");
                Label Label6 = (Label)FormView1.FindControl("Label6");
                Label Label7 = (Label)FormView1.FindControl("Label7");
                Label Label8 = (Label)FormView1.FindControl("Label8");
                Label Label9 = (Label)FormView1.FindControl("Label9");
                Label Label10 = (Label)FormView1.FindControl("Label10");
                Label Label11 = (Label)FormView1.FindControl("Label11");
                Label Label12 = (Label)FormView1.FindControl("Label12");
                Label Label13 = (Label)FormView1.FindControl("Label13");
                Label Label14 = (Label)FormView1.FindControl("Label14");
                Label Label15 = (Label)FormView1.FindControl("Label15");

                if (buton6.Text == "")
                {
                    Label6.Visible = false;
                    chktx6.Visible = false;
                    buton6.Visible = false;
                }
                if (buton7.Text == "")
                {
                    Label7.Visible = false;
                    chktx7.Visible = false;
                    buton7.Visible = false;
                }
                if (buton8.Text == "")
                {
                    Label8.Visible = false;
                    chktx8.Visible = false;
                    buton8.Visible = false;
                }
                if (buton9.Text == "")
                {
                    Label9.Visible = false;
                    chktx9.Visible = false;
                    buton9.Visible = false;
                }
                if (buton10.Text == "")
                {
                    Label10.Visible = false;
                    chktx10.Visible = false;
                    buton10.Visible = false;
                }
                if (buton11.Text == "")
                {
                    Label11.Visible = false;
                    chktx11.Visible = false;
                    buton11.Visible = false;
                }
                if (buton12.Text == "")
                {
                    Label12.Visible = false;
                    chktx12.Visible = false;
                    buton12.Visible = false;
                }
                if (buton13.Text == "")
                {
                    Label13.Visible = false;
                    chktx13.Visible = false;
                    buton13.Visible = false;
                }
                if (buton14.Text == "")
                {
                    Label14.Visible = false;
                    chktx14.Visible = false;
                    buton14.Visible = false;
                }
                if (buton15.Text == "")
                {
                    Label15.Visible = false;
                    chktx15.Visible = false;
                    buton15.Visible = false;
                }
                
               
            }
            catch { }
        }
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            try
            {
                Label chk1 = (Label)FormView1.FindControl("chkLabel1");
                Label chk2 = (Label)FormView1.FindControl("chkLabel2");
                Label chk3 = (Label)FormView1.FindControl("chkLabel3");
                Label chk4 = (Label)FormView1.FindControl("chkLabel4");
                Label chk5 = (Label)FormView1.FindControl("chkLabel5");
                Label chk6 = (Label)FormView1.FindControl("chkLabel6");
                Label chk7 = (Label)FormView1.FindControl("chkLabel7");
                Label chk8 = (Label)FormView1.FindControl("chkLabel8");
                Label chk9 = (Label)FormView1.FindControl("chkLabel9");
                Label chk10 = (Label)FormView1.FindControl("chkLabel10");
                Label chk11 = (Label)FormView1.FindControl("chkLabel11");
                Label chk12 = (Label)FormView1.FindControl("chkLabel12");
                Label chk13 = (Label)FormView1.FindControl("chkLabel13");
                Label chk14 = (Label)FormView1.FindControl("chkLabel14");
                Label chk15 = (Label)FormView1.FindControl("chkLabel15");

                CheckBox chktx1 = (CheckBox)FormView1.FindControl("chk1TextBox");
                CheckBox chktx2 = (CheckBox)FormView1.FindControl("chk2TextBox");
                CheckBox chktx3 = (CheckBox)FormView1.FindControl("chk3TextBox");
                CheckBox chktx4 = (CheckBox)FormView1.FindControl("chk4TextBox");
                CheckBox chktx5 = (CheckBox)FormView1.FindControl("chk5TextBox");
                CheckBox chktx6 = (CheckBox)FormView1.FindControl("chk6TextBox");
                CheckBox chktx7 = (CheckBox)FormView1.FindControl("chk7TextBox");
                CheckBox chktx8 = (CheckBox)FormView1.FindControl("chk8TextBox");
                CheckBox chktx9 = (CheckBox)FormView1.FindControl("chk9TextBox");
                CheckBox chktx10 = (CheckBox)FormView1.FindControl("chk10TextBox");
                CheckBox chktx11 = (CheckBox)FormView1.FindControl("chk11TextBox");
                CheckBox chktx12 = (CheckBox)FormView1.FindControl("chk12TextBox");
                CheckBox chktx13 = (CheckBox)FormView1.FindControl("chk13TextBox");
                CheckBox chktx14 = (CheckBox)FormView1.FindControl("chk14TextBox");
                CheckBox chktx15 = (CheckBox)FormView1.FindControl("chk15TextBox");

                TextBox ddl6 = (TextBox)FormView1.FindControl("ddl_user6");
                TextBox ddl7 = (TextBox)FormView1.FindControl("ddl_user7");
                TextBox ddl8 = (TextBox)FormView1.FindControl("ddl_user8");
                TextBox ddl9 = (TextBox)FormView1.FindControl("ddl_user9");
                TextBox ddl10 = (TextBox)FormView1.FindControl("ddl_user10");
                TextBox ddl11 = (TextBox)FormView1.FindControl("ddl_user11");
                TextBox ddl12 = (TextBox)FormView1.FindControl("ddl_user12");
                TextBox ddl13 = (TextBox)FormView1.FindControl("ddl_user13");
                TextBox ddl14 = (TextBox)FormView1.FindControl("ddl_user14");
                TextBox ddl15 = (TextBox)FormView1.FindControl("ddl_user15");
                Label buton6 = (Label)FormView1.FindControl("btn6TextBox");
                Label buton7 = (Label)FormView1.FindControl("btn7TextBox");
                Label buton8 = (Label)FormView1.FindControl("btn8TextBox");
                Label buton9 = (Label)FormView1.FindControl("btn9TextBox");
                Label buton10 = (Label)FormView1.FindControl("btn10TextBox");
                Label buton11 = (Label)FormView1.FindControl("btn11TextBox");
                Label buton12 = (Label)FormView1.FindControl("btn12TextBox");
                Label buton13 = (Label)FormView1.FindControl("btn13TextBox");
                Label buton14 = (Label)FormView1.FindControl("btn14TextBox");
                Label buton15 = (Label)FormView1.FindControl("btn15TextBox");
                Image img6 = (Image)FormView1.FindControl("Image5");
                Image img7 = (Image)FormView1.FindControl("Image7");
                Image img8 = (Image)FormView1.FindControl("Image8");
                Image img9 = (Image)FormView1.FindControl("Image9");
                Image img10 = (Image)FormView1.FindControl("Image10");
                Image img11 = (Image)FormView1.FindControl("Image11");
                Image img12 = (Image)FormView1.FindControl("Image12");
                Image img13 = (Image)FormView1.FindControl("Image13");
                Image img14 = (Image)FormView1.FindControl("Image14");
                Image img15 = (Image)FormView1.FindControl("Image15");



                if (chk1.Text == "False")
                {
                    chktx1.Checked = true;
                }
                if (chk2.Text == "False")
                {
                    chktx2.Checked = true;
                }
                if (chk3.Text == "False")
                {
                    chktx3.Checked = true;
                }
                if (chk4.Text == "False")
                {
                    chktx4.Checked = true;
                }
                if (chk5.Text == "False")
                {
                    chktx5.Checked = true;
                }
                if (chk6.Text == "False")
                {
                    chktx6.Checked = true;
                }
                if (chk7.Text == "False")
                {
                    chktx7.Checked = true;
                }
                if (chk8.Text == "False")
                {
                    chktx8.Checked = true;
                }
                if (chk9.Text == "False")
                {
                    chktx9.Checked = true;
                }
                if (chk10.Text == "False")
                {
                    chktx10.Checked = true;
                }
                if (chk11.Text == "False")
                {
                    chktx11.Checked = true;
                }
                if (chk12.Text == "False")
                {
                    chktx12.Checked = true;
                }
                if (chk13.Text == "False")
                {
                    chktx13.Checked = true;
                }
                if (chk14.Text == "False")
                {
                    chktx14.Checked = true;
                }
                if (chk15.Text == "False")
                {
                    chktx15.Checked = true;
                }

                if (buton6.Text == "")
                {
                    ddl6.Visible = false;
                    chktx6.Visible = false;
                    buton6.Visible = false;
                    img6.Visible = false;
                }
                if (buton7.Text == "")
                {
                    ddl7.Visible = false;
                    chktx7.Visible = false;
                    buton7.Visible = false;
                    img7.Visible = false;
                }
                if (buton8.Text == "")
                {
                    ddl8.Visible = false;
                    chktx8.Visible = false;
                    buton8.Visible = false;
                    img8.Visible = false;
                }
                if (buton9.Text == "")
                {
                    ddl9.Visible = false;
                    chktx9.Visible = false;
                    buton9.Visible = false;
                    img9.Visible = false;
                }
                if (buton10.Text == "")
                {
                    ddl10.Visible = false;
                    chktx10.Visible = false;
                    buton10.Visible = false;
                    img10.Visible = false;
                }
                if (buton11.Text == "")
                {
                    ddl11.Visible = false;
                    chktx11.Visible = false;
                    buton11.Visible = false;
                    img11.Visible = false;
                }
                if (buton12.Text == "")
                {
                    ddl12.Visible = false;
                    chktx12.Visible = false;
                    buton12.Visible = false;
                    img12.Visible = false;
                }
                if (buton13.Text == "")
                {
                    ddl13.Visible = false;
                    chktx13.Visible = false;
                    buton13.Visible = false;
                    img13.Visible = false;
                }
                if (buton14.Text == "")
                {
                    ddl14.Visible = false;
                    chktx14.Visible = false;
                    buton14.Visible = false;
                    img14.Visible = false;
                }
                if (buton15.Text == "")
                {
                    ddl15.Visible = false;
                    chktx15.Visible = false;
                    buton15.Visible = false;
                    img15.Visible = false;
                }

               
            }
            catch { }
        }
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
           
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
      
        //if (grid_view1.SelectedIndex < 0)
        //{
        //    UErrorLabel.Text = "Please First select any record!";
        //    return;
        //}
    }
    protected void ddl_sub_program_selectedindexchanged(object sender, EventArgs e)
    {
        
    }




    protected void update_button_click1(object sender, EventArgs e)
    {
        CheckBox chktx1 = (CheckBox)FormView1.FindControl("chk1TextBox");
        CheckBox chktx2 = (CheckBox)FormView1.FindControl("chk2TextBox");
        CheckBox chktx3 = (CheckBox)FormView1.FindControl("chk3TextBox");
        CheckBox chktx4 = (CheckBox)FormView1.FindControl("chk4TextBox");
        CheckBox chktx5 = (CheckBox)FormView1.FindControl("chk5TextBox");
        CheckBox chktx6 = (CheckBox)FormView1.FindControl("chk6TextBox");
        CheckBox chktx7 = (CheckBox)FormView1.FindControl("chk7TextBox");
        CheckBox chktx8 = (CheckBox)FormView1.FindControl("chk8TextBox");
        CheckBox chktx9 = (CheckBox)FormView1.FindControl("chk9TextBox");
        CheckBox chktx10 = (CheckBox)FormView1.FindControl("chk10TextBox");
        CheckBox chktx11 = (CheckBox)FormView1.FindControl("chk11TextBox");
        CheckBox chktx12 = (CheckBox)FormView1.FindControl("chk12TextBox");
        CheckBox chktx13 = (CheckBox)FormView1.FindControl("chk13TextBox");
        CheckBox chktx14 = (CheckBox)FormView1.FindControl("chk14TextBox");
        CheckBox chktx15 = (CheckBox)FormView1.FindControl("chk15TextBox");

        TextBox ddl1 = (TextBox)FormView1.FindControl("ddl_user1");
        TextBox ddl2 = (TextBox)FormView1.FindControl("ddl_user2");
        TextBox ddl3 = (TextBox)FormView1.FindControl("ddl_user3");
        TextBox ddl4 = (TextBox)FormView1.FindControl("ddl_user4");
        TextBox ddl5 = (TextBox)FormView1.FindControl("ddl_user5");
        TextBox ddl6 = (TextBox)FormView1.FindControl("ddl_user6");
        TextBox ddl7 = (TextBox)FormView1.FindControl("ddl_user7");
        TextBox ddl8 = (TextBox)FormView1.FindControl("ddl_user8");
        TextBox ddl9 = (TextBox)FormView1.FindControl("ddl_user9");
        TextBox ddl10 = (TextBox)FormView1.FindControl("ddl_user10");
        TextBox ddl11 = (TextBox)FormView1.FindControl("ddl_user11");
        TextBox ddl12 = (TextBox)FormView1.FindControl("ddl_user12");
        TextBox ddl13 = (TextBox)FormView1.FindControl("ddl_user13");
        TextBox ddl14 = (TextBox)FormView1.FindControl("ddl_user14");
        TextBox ddl15 = (TextBox)FormView1.FindControl("ddl_user15");

        if (chktx1.Checked == true)
            HiddenField1.Value = "False";
        else
            HiddenField1.Value = "True";
        if (chktx2.Checked == true)
            HiddenField2.Value = "False";
        else
            HiddenField2.Value = "True";
        if (chktx3.Checked == true)
            HiddenField3.Value = "False";
        else
            HiddenField3.Value = "True";
        if (chktx4.Checked == true)
            HiddenField4.Value = "False";
        else
            HiddenField4.Value = "True";
        if (chktx5.Checked == true)
            HiddenField5.Value = "False";
        else
            HiddenField5.Value = "True";
        if (chktx6.Checked == true)
            HiddenField6.Value = "False";
        else
            HiddenField6.Value = "True";
        if (chktx7.Checked == true)
            HiddenField7.Value = "False";
        else
            HiddenField7.Value = "True";
        if (chktx8.Checked == true)
            HiddenField8.Value = "False";
        else
            HiddenField8.Value = "True";
        if (chktx9.Checked == true)
            HiddenField9.Value = "False";
        else
            HiddenField9.Value = "True";
        if (chktx10.Checked == true)
            HiddenField10.Value = "False";
        else
            HiddenField10.Value = "True";
        if (chktx11.Checked == true)
            HiddenField11.Value = "False";
        else
            HiddenField11.Value = "True";
        if (chktx12.Checked == true)
            HiddenField12.Value = "False";
        else
            HiddenField12.Value = "True";
        if (chktx13.Checked == true)
            HiddenField13.Value = "False";
        else
            HiddenField13.Value = "True";
        if (chktx14.Checked == true)
            HiddenField14.Value = "False";
        else
            HiddenField14.Value = "True";
        if (chktx15.Checked == true)
            HiddenField15.Value = "False";
        else
            HiddenField15.Value = "True";

        UserClass UserLogin = (UserClass)Session["User"];
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        conn.Open();
        SqlCommand cmd = new SqlCommand("update button_maintain set chk1 = '" + Convert.ToString(HiddenField1.Value) + "',chk2 = '" + Convert.ToString(HiddenField2.Value) + "',chk3 = '" + Convert.ToString(HiddenField3.Value) + "',chk4 = '" + Convert.ToString(HiddenField4.Value) + "',chk5 = '" + Convert.ToString(HiddenField5.Value) + "',chk6 = '" + Convert.ToString(HiddenField6.Value) + "',chk7 = '" + Convert.ToString(HiddenField7.Value) + "',chk8 = '" + Convert.ToString(HiddenField8.Value) + "',chk9 = '" + Convert.ToString(HiddenField9.Value) + "',chk10 = '" + Convert.ToString(HiddenField10.Value) + "',chk11 = '" + Convert.ToString(HiddenField11.Value) + "',chk12 = '" + Convert.ToString(HiddenField12.Value) + "',chk13 = '" + Convert.ToString(HiddenField13.Value) + "',chk14 = '" + Convert.ToString(HiddenField14.Value) + "',chk15 = '" + Convert.ToString(HiddenField15.Value) + "',user1 = '" + ddl1.Text.Trim() + "',user2 = '" + ddl2.Text.Trim() + "',user3 = '" + ddl3.Text.Trim() + "',user4 = '" + ddl4.Text.Trim() + "',user5 = '" + ddl5.Text.Trim() + "',user6 = '" + ddl6.Text.Trim() + "',user7 = '" + ddl7.Text.Trim() + "',user8 = '" + ddl8.Text.Trim() + "',user9 = '" + ddl9.Text.Trim() + "',user10 = '" + ddl10.Text.Trim() + "',user11 = '" + ddl11.Text.Trim() + "',user12 = '" + ddl12.Text.Trim() + "',user13 = '" + ddl13.Text.Trim() + "',user14 = '" + ddl14.Text.Trim() + "',user15 = '" + ddl15.Text.Trim() + "' where button_id = '" + ddl_main_program.SelectedValue + "' ", conn);
        cmd.ExecuteNonQuery();
        conn.Close();

        

    }

          
   

   
}
