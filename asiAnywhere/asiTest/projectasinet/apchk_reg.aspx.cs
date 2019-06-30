
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
using System.IO;
using System.Web.UI;
#endregion

public partial class apchk_reg : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "apchk_reg.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();

            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = labelcompany.Text;
           
            if (aUsers == "external")
            {
            }
        }
        if (!Page.IsPostBack)
        {
            OutPutFile.Visible = false;
            HyperLink1.Visible = false;

            try
            {
                string UserId = UserLogin.UserName;
                string aDefaultCust = null;
                string aComp = null;

               // func1 user = new func1();
                //user.CheckUserCustomer(aComp, UserId, ref  aDefaultCust);
                
            }
            catch { }

            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'apchk_reg.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    begdateTextBox.Text = dr["field1"].ToString();
                    enddateTextBox.Text = dr["field2"].ToString();
                    begvendTextBox.Text = dr["field3"].ToString();
                    endvendTextBox.Text = dr["field4"].ToString();
                    begchkTextBox.Text = dr["field5"].ToString();
                    endchkTextBox.Text = dr["field6"].ToString();
                    begbankTextBox.Text = dr["field7"].ToString();
                    endbankTextBox.Text = dr["field8"].ToString();
                   

                    if (dr["chk_field1"].ToString() == "True")
                        CheckBox1.Checked = true;
                    else
                        CheckBox1.Checked = false;
                    if (dr["chk_field2"].ToString() == "True")
                        CheckBox2.Checked = true;
                    else
                        CheckBox2.Checked = false;

                                    
                                       
                }

                conn.Close();
            }
            catch
            {
                conn.Close();
            }
            finally
            {
                conn.Close();
            }

            if (endvendTextBox.Text == "")
                endvendTextBox.Text = "zzzzzzzz";
            if (endchkTextBox.Text == "")
                endchkTextBox.Text = "999999999";
            if (endbankTextBox.Text == "")
                endbankTextBox.Text = "zzzzzzzz";
            
           
            if (Session["User"] != null)
            {
                lblUser.Text = UserLogin.UserName;
            }
            RadioButtonList3.SelectedIndex = 0;
        }

            
            
                    
                    if (Session["User"] != null)
                    {
                        lblUser.Text = UserLogin.UserName;
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

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }

    protected void submitbutton_click(object sender, EventArgs e)
    {


        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmapchk"].DefaultValue = "apchk";
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList3.SelectedValue;
        ObjectDataSource1.SelectParameters["prmbegvend"].DefaultValue =  begvendTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbegdt"].DefaultValue = begdateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbegchk"].DefaultValue = begchkTextBox.Text.Trim(); 
        ObjectDataSource1.SelectParameters["prmbegbnk"].DefaultValue =  begbankTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmendvend"].DefaultValue = endvendTextBox.Text.Trim();  
        ObjectDataSource1.SelectParameters["prmenddt"].DefaultValue =  enddateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmendchk"].DefaultValue = endchkTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmendbnk"].DefaultValue = endbankTextBox.Text.Trim(); 

        ObjectDataSource1.SelectParameters["prmglact"].DefaultValue = Convert.ToString(CheckBox1.Checked);
        ObjectDataSource1.SelectParameters["prmrunpst"].DefaultValue = Convert.ToString(CheckBox2.Checked);



        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'apchk_reg.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);
            
            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6, field7, field8, chk_field1,  chk_field2) values ('" + UserLogin.UserName + "','apchk_reg.aspx' , '" + begdateTextBox.Text.Trim() + "', '" + enddateTextBox.Text.Trim() + "','" + begvendTextBox.Text.Trim() + "','" + endvendTextBox.Text.Trim() + "','" + begchkTextBox.Text.Trim() + "','" + endchkTextBox.Text.Trim() + "','" + begbankTextBox.Text.Trim() + "','" + endbankTextBox.Text.Trim() + "','" + Convert.ToString(CheckBox1.Checked) + "','" + Convert.ToString(CheckBox2.Checked) + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + begdateTextBox.Text.Trim() + "', field2 = '" + enddateTextBox.Text.Trim() + "', field3 = '" + begvendTextBox.Text.Trim() + "', field4 = '" + endvendTextBox.Text.Trim() + "', field5 = '" + begchkTextBox.Text.Trim() + "', field6 = '" + endchkTextBox.Text.Trim() + "', field7 = '" + begbankTextBox.Text.Trim() + "', field8 = '" + endbankTextBox.Text.Trim() + "', chk_field1 = '" + Convert.ToString(CheckBox1.Checked) + "', chk_field2 = '" + Convert.ToString(CheckBox2.Checked) + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'apchk_reg.aspx' ", conn);
                cmd_update.ExecuteNonQuery();
            }
            conn.Close();
        }
        catch (Exception ex)
        {
            Label1.Text = "Error :" + ex.Message + "<p>";
            conn.Close();
        }
        finally
        {
            conn.Close();
        }


        try
        {

            OutPutFile.Visible = true;
            HyperLink1.Visible = true;
            Label vpath = (Label)FormView1.FindControl("apchkLabel");
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;

            if (vpath.Text == "")
            {
                OutPutFile.Text = "No CSV Exists";
                Response.Write("<script>window.location.href='apchk_reg.aspx'</script>");
            }
        }
        catch { }


    }

       

}
