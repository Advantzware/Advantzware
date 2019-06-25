
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

public partial class topprintglact_report : System.Web.UI.Page
{



    protected void Page_Load(object sender, System.EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "topprintglact_report.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            Session["print_ack_order"] = PrmComp;
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        if (!Page.IsPostBack)
        {

            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'topprintglact_report.aspx'  ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);
                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    begact_TextBox.Text = dr["field1"].ToString();
                    begdscr_TextBox.Text = dr["field2"].ToString();
                    endact_TextBox.Text = dr["field3"].ToString();
                    enddscr_TextBox.Text = dr["field4"].ToString();

                    if (dr["rd_field1"].ToString() == "1")
                        RadioButtonList2.SelectedIndex = 0;
                    if (dr["rd_field1"].ToString() == "2")
                        RadioButtonList2.SelectedIndex = 1;
                    if (dr["rd_field1"].ToString() == "3")
                        RadioButtonList2.SelectedIndex = 2;


                    if (dr["chk_field1"].ToString() == "True")
                        chk_shipto.Checked = true;
                    else
                        chk_shipto.Checked = false;
                    
                    if (dr["chk_field4"].ToString() == "True")
                        chk_note.Checked = true;
                    else
                        chk_note.Checked = false;

                    if (dr["chk_field5"].ToString() == "True")
                        chk_parameter.Checked = true;
                    else
                        chk_parameter.Checked = false;
                    if (dr["chk_field6"].ToString() == "True")
                        chk_misc.Checked = true;
                    else
                        chk_misc.Checked = false;

                    if (dr["chk_field7"].ToString() == "True")
                        chk_add.Checked = true;
                    else
                        chk_add.Checked = false;
                    if (dr["chk_field8"].ToString() == "True")
                        chk_phone.Checked = true;
                    else
                        chk_phone.Checked = false;

                    
                }
            }
            catch
            {
                conn.Close();
            }
            finally
            {
                conn.Close();
            }

               RadioButtonList1.SelectedIndex = 0;
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
    protected void SubmitButton_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        
               
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "actrpt";
        ObjectDataSource1.SelectParameters["prmbeact"].DefaultValue = begact_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbegactdr"].DefaultValue = begdscr_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmendact"].DefaultValue = endact_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmendactdr"].DefaultValue = enddscr_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmshwhist"].DefaultValue = Convert.ToString(chk_shipto.Checked);        
        ObjectDataSource1.SelectParameters["prmListOrder"].DefaultValue = RadioButtonList1.SelectedValue;
        ObjectDataSource1.SelectParameters["prmshwnote"].DefaultValue = Convert.ToString(chk_note.Checked);
        ObjectDataSource1.SelectParameters["prmshwselc"].DefaultValue = Convert.ToString(chk_parameter.Checked);
        ObjectDataSource1.SelectParameters["prmshwmisc"].DefaultValue = Convert.ToString(chk_misc.Checked);
        ObjectDataSource1.SelectParameters["prmshwadd"].DefaultValue = Convert.ToString(chk_add.Checked);
        ObjectDataSource1.SelectParameters["prmshwphn"].DefaultValue = Convert.ToString(chk_phone.Checked);
        ObjectDataSource1.SelectParameters["prmReckey"].DefaultValue = RadioButtonList2.SelectedValue;




        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'topprintglact_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name, field1, field2, field3, field4,rd_field1, chk_field1, chk_field2,chk_field5,chk_field6,chk_field7,chk_field8) values ('" + UserLogin.UserName + "','topprintglact_report.aspx','" + begact_TextBox.Text.Trim() + "','" + begdscr_TextBox.Text.Trim() + "','" + endact_TextBox.Text.Trim() + "','" + enddscr_TextBox.Text.Trim() + "','" + RadioButtonList2.SelectedValue + "','" + Convert.ToString(chk_shipto.Checked) + "','" + Convert.ToString(chk_note.Checked) + "','" + Convert.ToString(chk_parameter.Checked) + "','" + Convert.ToString(chk_misc.Checked) + "','" + Convert.ToString(chk_add.Checked) + "','" + Convert.ToString(chk_phone.Checked) + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + begact_TextBox.Text.Trim() + "', field2 = '" + begdscr_TextBox.Text.Trim() + "', field3 = '" + endact_TextBox.Text.Trim() + "', field4 = '" + enddscr_TextBox.Text.Trim() + "', rd_field1 = '" + RadioButtonList2.SelectedValue + "', chk_field1 = '" + Convert.ToString(chk_shipto.Checked) + "', chk_field4 = '" + Convert.ToString(chk_note.Checked) + "', chk_field5 = '" + Convert.ToString(chk_parameter.Checked) + "',chk_field6 = '" + Convert.ToString(chk_misc.Checked) + "',chk_field7 = '" + Convert.ToString(chk_add.Checked) + "',chk_field8 = '" + Convert.ToString(chk_phone.Checked) + "' where user_name = '" + UserLogin.UserName + "' and prog_name ='topprintglact_report.aspx' ", conn);
                cmd_update.ExecuteNonQuery();
            }

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
            Label vpath = (Label)FormView1.FindControl("actprtLabel");
        
        if (vpath.Text != "")
        {
            string path = vpath.Text;
            string path2 = @"/pdfs/" + path;
            Session["open_order_ack_rep"] = path2;           
            if (path2 != "")
            {
                if (!Request.Browser.Browser.Contains("Safari"))
                    Response.Write("<script>window.open('print_open_order_ack_rep.aspx'); target='_blank'</script>");
                else
                    Response.Redirect("topprintglact_report.aspx");
            }
        }
        else
        {
            Label1.Text = "No Pdf Exists";
        }
    }
    catch
    {
        Label1.Text = "No Pdf Exists";
    }
    if (Label1.Text == "")
    {
        Response.Write("<script>window.location.href='topprintglact_report.aspx'</script>");
    }
    }
    protected void FormView1_PreRender(object sender, EventArgs e)
    {
        
    }
}
